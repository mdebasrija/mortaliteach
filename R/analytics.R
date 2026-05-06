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


library(data.table)
# validate cohort

validate_cohort <- function(df) {
  
  required <- c("seqn", "age_at_exam", "sex",
                "vital_status", "follow_up_years", "smoker", "bmi")
  
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
  
  bad_bmi <- which(is.na(df$bmi) | 
                     df$bmi < 10 | df$bmi > 80)
  if (length(bad_bmi) > 0) {
    stop("Impossible BMI values at rows: ",
         paste(head(bad_bmi, 5), collapse = ", "))
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
  
  load_wave <- function(demo_code, smq_code, 
                        bmx_code, mort_file) {
    
    demo <- nhanesA::nhanes(demo_code)
    smq  <- nhanesA::nhanes(smq_code)
    bmx  <- nhanesA::nhanes(bmx_code)
    mort <- read_mort(mort_file)
    
    demo <- demo[, c("SEQN", "RIDAGEYR", 
                     "RIAGENDR", "RIDSTATR")]
    smq  <- smq[,  c("SEQN", "SMQ020")]
    bmx  <- bmx[,  c("SEQN", "BMXBMI")]
    mort <- mort[, c("seqn", "eligstat", 
                     "mortstat", "permth_exm")]
    
    names(demo) <- c("seqn", "age_at_exam", 
                     "sex", "ridstatr")
    names(smq)  <- c("seqn", "smoker")
    names(bmx)  <- c("seqn", "bmi")
    
    df <- merge(demo, smq,  by = "seqn", all.x = TRUE)
    df <- merge(df,   bmx,  by = "seqn", all.x = TRUE)
    df <- merge(df,   mort, by = "seqn", all.x = TRUE)
    
    df <- df[
      df$ridstatr == "Both Interviewed and MEC examined", ]
    df <- df[!is.na(df$eligstat) & df$eligstat == 1, ]
    df <- df[!is.na(df$mortstat), ]
    df <- df[df$age_at_exam >= 18 & 
               df$age_at_exam <= 85, ]
    df <- df[
      !is.na(df$smoker) & 
        df$smoker %in% c("Yes", "No"), ]
    
    # drop missing BMI — 1.5% in wave 1, similar in others
    df <- df[!is.na(df$bmi), ]
    
    df$sex    <- as.character(df$sex)
    df$smoker <- ifelse(df$smoker == "Yes",
                        "smoker", "non_smoker")
    
    df$follow_up_years <- df$permth_exm / 12
    df <- df[df$follow_up_years > 0, ]
    
    data.frame(
      seqn            = as.integer(df$seqn),
      age_at_exam     = as.integer(df$age_at_exam),
      sex             = df$sex,
      smoker          = df$smoker,
      bmi             = df$bmi,
      vital_status    = as.integer(df$mortstat),
      follow_up_years = df$follow_up_years,
      stringsAsFactors = FALSE
    )
  }
  
  wave1 <- load_wave(
    "DEMO",   "SMQ",   "BMX",
    "NHANES_1999_2000_MORT_2019_PUBLIC.dat")
  
  wave2 <- load_wave(
    "DEMO_B", "SMQ_B", "BMX_B",
    "NHANES_2001_2002_MORT_2019_PUBLIC.dat")
  
  wave3 <- load_wave(
    "DEMO_C", "SMQ_C", "BMX_C",
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


# build exposure table
# Expands cohort to one row per integer age interval.
#
# Testing performance here
#
# Possibility 1 — base R for loop
#   Simple, readable. Allocates a new data frame per person
#   inside the loop. On 14,170 participants: ~22 seconds.
#   profvis showed 94% of time in allocation routines,
#   61 GC cycles. Root cause: growing a list by appending
#   one data frame per person triggers repeated copying.
#
# Possibility 2 — dplyr rowwise()
#   Replaced loop with rowwise() + unnest(). Cleaner syntax.
#   Still ~7 seconds. Same root cause — rowwise() allocates
#   per-row intermediates. dplyr did not fix the bottleneck,
#   it just hid it.
#
# Possibility 3 — vectorised data.table (current)
#   Key insight: compute all intervals for all participants
#   simultaneously using rep() and vectorised arithmetic.
#   No per-person allocation. 61ms on 14,170 participants.
#   367x faster than iteration 1.

build_exposure <- function(cohort) {
  
  dt <- data.table::as.data.table(cohort)
  
  dt[, n_full := floor(follow_up_years)]
  dt[, frac   := follow_up_years - n_full]
  dt[, n_intervals := n_full + (frac > 0)]
  
  exp_dt <- dt[rep(seq_len(nrow(dt)), dt$n_intervals)]
  
  exp_dt[, interval := sequence(dt$n_intervals)]
  exp_dt[, age := age_at_exam + interval - 1L]
  
  exp_dt[, exposure_yrs := data.table::fifelse(
    interval == n_intervals & frac > 0,
    frac,
    1.0
  )]
  
  exp_dt[, died := 0L]
  exp_dt[vital_status == 1L & interval == n_intervals,
         died := 1L]
  
  result <- exp_dt[, .(
    seqn,
    age,
    sex,
    smoker,
    exposure_yrs,
    died
  )]
  
  as.data.frame(result)
}


# profiling versions

.build_exposure_loop <- function(cohort) {
  rows <- vector("list", nrow(cohort))
  for (i in seq_len(nrow(cohort))) {
    p      <- cohort[i, ]
    n_full <- floor(p$follow_up_years)
    frac   <- p$follow_up_years - n_full
    
    if (frac > 0) {
      ages    <- p$age_at_exam + seq(0, n_full)
      exp_yrs <- c(rep(1, n_full), frac)
    } else {
      ages    <- p$age_at_exam + seq(0, n_full - 1L)
      exp_yrs <- rep(1, n_full)
    }
    
    n    <- length(ages)
    died <- integer(n)
    if (p$vital_status == 1L) died[n] <- 1L
    
    rows[[i]] <- data.frame(
      seqn         = p$seqn,
      age          = ages,
      sex          = p$sex,
      smoker       = p$smoker,
      exposure_yrs = exp_yrs,
      died         = died
    )
  }
  do.call(rbind, rows)
}


.build_exposure_dplyr <- function(cohort) {
  cohort %>%
    dplyr::rowwise() %>%
    dplyr::mutate(intervals = list({
      n_full  <- floor(follow_up_years)
      frac    <- follow_up_years - n_full
      
      if (frac > 0) {
        ages    <- age_at_exam + seq(0, n_full)
        exp_yrs <- c(rep(1, n_full), frac)
      } else {
        ages    <- age_at_exam + seq(0, n_full - 1L)
        exp_yrs <- rep(1, n_full)
      }
      
      n    <- length(ages)
      died <- integer(n)
      if (vital_status == 1L) died[n] <- 1L
      
      data.frame(
        age          = ages,
        exposure_yrs = exp_yrs,
        died         = died
      )
    })) %>%
    tidyr::unnest(intervals) %>%
    dplyr::select(seqn, age, sex, smoker,
                  exposure_yrs, died)
}


# attach basis
# Joins VBT 2015 central death rates to exposure frame.
# Adds two columns:
#   ref_rate    — mx from VBT 2015 for this age/sex/smoker
#   expected    — exposure_yrs * ref_rate
#
# Hard stop if any ref_rate is NA after join —
# silent NA would corrupt every downstream A/E ratio.

attach_basis <- function(exposure_df,
                         basis_path = "data/vbt_2015.csv") {
  
  basis <- read.csv(basis_path, stringsAsFactors = FALSE)
  
  basis_dt <- data.table::as.data.table(
    basis[, c("age", "sex", "smoker", "mx")]
  )
  data.table::setnames(basis_dt, "mx", "ref_rate")
  data.table::setkeyv(basis_dt, c("age", "sex", "smoker"))
  
  exp_dt <- data.table::as.data.table(exposure_df)
  data.table::setkeyv(exp_dt, c("age", "sex", "smoker"))
  
  result <- basis_dt[exp_dt]
  
  na_count <- sum(is.na(result$ref_rate))
  if (na_count > 0) {
    stop(
      "attach_basis(): ", na_count,
      " rows have NA ref_rate after join. ",
      "Check age/sex/smoker values are in VBT table."
    )
  }
  
  result[, expected := exposure_yrs * ref_rate]
  
  as.data.frame(result)
}


# run study
# Aggregates exposure frame to A/E by group.
# Returns S3 object of class experience_study.
#
# Byar CI is the standard actuarial confidence interval
# for Poisson-distributed death counts.
# reliable flag: TRUE when A >= 5 (minimum for credible CI)

.byar_ci <- function(A, E, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  
  ci_lo <- ifelse(
    A == 0,
    0,
    (A / E) * (1 - 1/(9*A) - z/sqrt(A))^3
  )
  
  ci_hi <- ifelse(
    A == 0,
    -log(1 - conf) / E,
    (A / E) * (1 - 1/(9*(A+1)) + z/sqrt(A+1))^3
  )
  
  list(ci_lo = ci_lo, ci_hi = ci_hi)
}

run_study <- function(exp_basis,
                      group_by = c("sex", "smoker")) {
  
  dt <- data.table::as.data.table(exp_basis)
  
  result <- dt[,
               .(
                 A            = sum(died),
                 E            = sum(expected),
                 exposure_yrs = sum(exposure_yrs)
               ),
               by = group_by
  ]
  
  result[, ae := A / E]
  
  ci <- .byar_ci(result$A, result$E)
  result[, ci_lo    := ci$ci_lo]
  result[, ci_hi    := ci$ci_hi]
  result[, reliable := A >= 5L]
  
  result[, ae    := round(ae,    3)]
  result[, ci_lo := round(ci_lo, 3)]
  result[, ci_hi := round(ci_hi, 3)]
  
  structure(
    list(
      results  = as.data.frame(result),
      group_by = group_by,
      A_total  = sum(result$A),
      E_total  = sum(result$E),
      ae_total = round(sum(result$A) / sum(result$E), 3)
    ),
    class = "experience_study"
  )
}

# S3 methods

print.experience_study <- function(x, ...) {
  cat("Experience study\n")
  cat("Grouped by:", paste(x$group_by, collapse = ", "), "\n")
  cat("Overall A/E:", x$ae_total,
      "(A =", x$A_total, "E =", round(x$E_total, 1), ")\n\n")
  print(x$results)
  invisible(x)
}

summary.experience_study <- function(object, ...) {
  cat("Overall A/E:", object$ae_total, "\n")
  cat("Total actual deaths:  ", object$A_total, "\n")
  cat("Total expected deaths:", round(object$E_total, 1), "\n")
  cat("Reliable groups:",
      sum(object$results$reliable), "/",
      nrow(object$results), "\n")
  invisible(object)
}


# assign risk class
# Assigns underwriting risk class to each cohort participant.
# Rules derived from decision tree in Iteration 0:
#
#   Non-smoker, age < 43           → Preferred Elite
#   Non-smoker, age >= 43          → Preferred
#   Smoker, BMI < 30               → Substandard
#   Smoker, BMI >= 30              → Substandard High
#
# Multipliers relative to Preferred Elite (1.0x):
#   Preferred:        1.25x
#   Substandard:      2.33x
#   Substandard High: 3.25x

assign_risk_class <- function(cohort) {
  
  multipliers <- c(
    "Preferred Elite" = 1.00,
    "Preferred"       = 1.25,
    "Substandard"     = 2.33,
    "Substandard High"= 3.25
  )
  
  cohort$risk_class <- with(cohort, ifelse(
    smoker == "non_smoker" & age_at_exam < 43,
    "Preferred Elite",
    ifelse(
      smoker == "non_smoker" & age_at_exam >= 43,
      "Preferred",
      ifelse(
        smoker == "smoker" & bmi < 30,
        "Substandard",
        "Substandard High"
      )
    )
  ))
  
  cohort$multiplier <- multipliers[cohort$risk_class]
  
  cohort
}