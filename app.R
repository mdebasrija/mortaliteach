# app.R
# MortaliTeach — entry point
# Plain shiny::runApp() — no golem package yet

library(shiny)
library(plotly)
library(rpart)
library(bslib)

source("R/analytics.R")
source("R/plots_act1.R")
source("R/plots_act2.R")
source("R/plots_act3.R")
source("R/mod_act1.R")
source("R/mod_act2.R")
source("R/mod_act3.R")

# ── data preparation ──────────────────────────────────────
insurance <- read.csv("data/insurance.csv",
                      stringsAsFactors = FALSE)

tree <- rpart(
  charges ~ age + bmi + smoker + sex +
    children + region,
  data   = insurance,
  method = "anova"
)

glm_model1 <- glm(
  charges ~ age + bmi + smoker + sex +
    children + region,
  data   = insurance,
  family = Gamma(link = "log")
)

# load pre-computed NHANES data
if (file.exists("data/cohort.rds")) {
  cohort    <- readRDS("data/cohort.rds")
  exp_basis <- readRDS("data/exp_basis.rds")
  study_rc  <- readRDS("data/study_rc.rds")
  repricing <- readRDS("data/repricing.rds")
} else {
  cohort    <- load_cohort()
  cohort    <- assign_risk_class(cohort)
  cohort    <- assign_financials(cohort)
  cohort    <- reinsurance_split(cohort)
  exposure  <- build_exposure(cohort)
  exp_basis <- attach_basis(exposure, cohort)
  study_rc  <- run_study(exp_basis,
                         group_by = "risk_class")
  repricing <- compute_repricing(study_rc, cohort)
}

personas <- data.frame(
  name           = c("Elena", "Damon",
                     "Caroline", "Niklaus"),
  age            = c(38, 59, 42, 47),
  sex            = c("Female", "Male",
                     "Female", "Male"),
  smoker         = c("No", "No", "Yes", "Yes"),
  bmi            = c(37.73, 24.70, 26.60, 36.19),
  risk_class     = c("Preferred Elite", "Preferred",
                     "Substandard", "Substandard High"),
  health_charges = c(5398, 12324, 21349, 41676),
  mx             = c(0.000120, 0.003787,
                     0.000450, 0.003235),
  multiplier     = c(1.00, 1.25, 2.33, 3.25),
  face_amount    = c(8695652, 219130, 992908, 98716),
  stringsAsFactors = FALSE
)

cohort    <- load_cohort()
cohort    <- assign_risk_class(cohort)
cohort    <- assign_financials(cohort)
cohort    <- reinsurance_split(cohort)
exposure  <- build_exposure(cohort)
exp_basis <- attach_basis(exposure, cohort)
study_rc  <- run_study(exp_basis,
                       group_by = "risk_class")
repricing <- compute_repricing(study_rc, cohort)

# ui
ui <- page_navbar(
  title = "MortaliTeach",
  theme = bs_theme(
    bootswatch = "flatly",
    base_font  = font_google("Inter")
  ),
  
  tags$head(
    tags$link(rel  = "stylesheet",
              href = "styles.css"),
    tags$script(src = "scroll.js")
  ),
  
  nav_panel(
    "Act 1 — Risk classes",
    act1_ui("act1")
  ),
  
  nav_panel(
    "Act 2 — Insurance pool",
    act2_ui("act2")
  ),
  
  nav_panel(
    "Act 3 — Experience study",
    act3_ui("act3")
  )
)

# server
server <- function(input, output, session) {
  
  act1_server("act1",
              insurance = insurance,
              tree      = tree,
              glm_model = glm_model1,
              personas  = personas)
  
  act2_server("act2",
              cohort   = cohort,
              personas = personas)
  
  act3_server("act3",
              study_rc  = study_rc,
              exp_basis = exp_basis,
              cohort    = cohort,
              repricing = repricing)
}

shinyApp(ui, server)