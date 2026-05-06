# analytics.R
# MortalITeach — core analytics pipeline
#
# Functions:
#   validate_cohort()   — hard failures on bad data
#   .load_nhanes()      — download and clean NHANES data
#   load_cohort()       — public wrapper
#   build_exposure()    — expand cohort to person-year rows
#   attach_basis()      — join VBT mortality rates to exposure
#   run_study()         — compute A/E, return S3 object
#
# Expected basis:
#   Constant force assumption: mx = -log(1 - qx)
#   At mortality rates in this cohort (<4%), difference
#   between constant force, UDD, and Balducci < 0.2%.

# validate cohort

validate_cohort <- function(df) {
  
  required <- c("seqn", "age_at_exam", "sex",
                "vital_status", "follow_up_years", "smoker")
  
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  for (col in required) {
    na_rows <- which(is.na(df[[col]]))
    if (length(na_rows) > 0) {
      stop("NAs found in column '", col, "' at rows: ",
           paste(head(na_rows, 5), collapse = ", "))
    }
  }
  
  bad_age <- which(df$age_at_exam < 18 | df$age_at_exam > 85)
  if (length(bad_age) > 0) {
    stop("Ages outside 18-85 at rows: ",
         paste(head(bad_age, 5), collapse = ", "))
  }
  
  bad_fu <- which(df$follow_up_years <= 0)
  if (length(bad_fu) > 0) {
    stop("Non-positive follow_up_years at rows: ",
         paste(head(bad_fu, 5), collapse = ", "))
  }
  
  bad_vs <- which(!df$vital_status %in% c(0L, 1L))
  if (length(bad_vs) > 0) {
    stop("vital_status must be 0 or 1. Bad rows: ",
         paste(head(bad_vs, 5), collapse = ", "))
  }
  
  bad_sex <- which(!df$sex %in% c("Male", "Female"))
  if (length(bad_sex) > 0) {
    stop("sex must be 'Male' or 'Female'. Bad rows: ",
         paste(head(bad_sex, 5), collapse = ", "))
  }
  
  bad_smoker <- which(!df$smoker %in% c("smoker", "non_smoker"))
  if (length(bad_smoker) > 0) {
    stop("smoker must be 'smoker' or 'non_smoker'. Bad rows: ",
         paste(head(bad_smoker, 5), collapse = ", "))
  }
  
  invisible(df)
}

# load nhanes
# Downloads three continuous NHANES waves (1999-2004) and
# joins survey demographics + smoking to mortality linkage.
# Returns one row per eligible adult with known smoking status.

.load_nhanes <- function() {
  
  base_mort <- paste0(
    "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/",
    "datalinkage/linked_mortality/"
  )
  
  read_mort <- function(filename) {
    readr::read_fwf(
      paste0(base_mort, filename),
      col_types = "iiiiiiii",
      readr::fwf_cols(
        seqn         = c(1,  6),
        eligstat     = c(15, 15),
        mortstat     = c(16, 16),
        ucod_leading = c(17, 19),
        diabetes     = c(20, 20),
        hyperten     = c(21, 21),
        permth_int   = c(43, 45),
        permth_exm   = c(46, 48)
      ),
      na = c("", ".")
    )
  }
  
  load_wave <- function(demo_code, smq_code, mort_file) {
    
    demo <- nhanesA::nhanes(demo_code)
    smq  <- nhanesA::nhanes(smq_code)
    mort <- read_mort(mort_file)
    
    demo <- demo[, c("SEQN", "RIDAGEYR", "RIAGENDR", "RIDSTATR")]
    smq  <- smq[,  c("SEQN", "SMQ020")]
    mort <- mort[, c("seqn", "eligstat", "mortstat", "permth_exm")]
    
    names(demo) <- c("seqn", "age_at_exam", "sex", "ridstatr")
    names(smq)  <- c("seqn", "smoker")
    
    df <- merge(demo, smq,  by = "seqn", all.x = TRUE)
    df <- merge(df,   mort, by = "seqn", all.x = TRUE)
    
    # keep only examined participants
    df <- df[
      df$ridstatr == "Both Interviewed and MEC examined", ]
    
    # keep only eligible for mortality follow-up
    df <- df[!is.na(df$eligstat) & df$eligstat == 1, ]
    
    # drop unknown vital status
    df <- df[!is.na(df$mortstat), ]
    
    # adults only
    df <- df[df$age_at_exam >= 18 & df$age_at_exam <= 85, ]
    
    # known smoking status only — drop refused and don't know
    df <- df[
      !is.na(df$smoker) & df$smoker %in% c("Yes", "No"), ]
    
    # recode to internal values
    df$sex    <- as.character(df$sex)
    df$smoker <- ifelse(df$smoker == "Yes",
                        "smoker", "non_smoker")
    
    # follow-up in years
    df$follow_up_years <- df$permth_exm / 12
    df <- df[df$follow_up_years > 0, ]
    
    data.frame(
      seqn            = as.integer(df$seqn),
      age_at_exam     = as.integer(df$age_at_exam),
      sex             = df$sex,
      smoker          = df$smoker,
      vital_status    = as.integer(df$mortstat),
      follow_up_years = df$follow_up_years,
      stringsAsFactors = FALSE
    )
  }
  
  wave1 <- load_wave(
    "DEMO",   "SMQ",
    "NHANES_1999_2000_MORT_2019_PUBLIC.dat")
  
  wave2 <- load_wave(
    "DEMO_B", "SMQ_B",
    "NHANES_2001_2002_MORT_2019_PUBLIC.dat")
  
  wave3 <- load_wave(
    "DEMO_C", "SMQ_C",
    "NHANES_2003_2004_MORT_2019_PUBLIC.dat")
  
  rbind(wave1, wave2, wave3)
}

# load cohort
# Public wrapper. Downloads NHANES and validates result.

load_cohort <- function() {
  cohort <- .load_nhanes()
  validate_cohort(cohort)
  cohort
}