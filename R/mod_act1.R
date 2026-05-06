# mod_act1.R
# Shiny module for Act 1 — What drives premiums?
# Scrollytelling layout:
#   Left panel:  narrative text sections
#   Right panel: plot updates as sections scroll into view

act1_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "scroll-container",
      
      # left panel — narrative
      shiny::div(
        class = "scroll-narrative",
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "1",
          shiny::h2("What drives insurance premiums?"),
          shiny::p(
            "1,338 real US health insurance records.",
            "Each dot is a person. The x-axis is their age.",
            "The y-axis is what their insurer actually billed."
          ),
          shiny::p(
            "Three bands are visible. All three rise with age.",
            "But something else is creating the separation.",
            "What is it?"
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "2",
          shiny::h2("Smoking explains the top band"),
          shiny::p(
            "Colour by smoking status.",
            "The top band resolves immediately — it is",
            "entirely smokers."
          ),
          shiny::p(
            "Mean charges: smokers $32,050 vs",
            "non-smokers $8,434.",
            "Smokers cost 3.8 times more."
          ),
          shiny::p(
            "But two bands remain within non-smokers.",
            "Something else is at work."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "3",
          shiny::h2("Four groups, not three bands"),
          shiny::p(
            "Add obesity (BMI >= 30) as shape.",
            "Four groups are now visible."
          ),
          shiny::tags$ul(
            shiny::tags$li(
              "Smoker + obese: $41,558 mean"),
            shiny::tags$li(
              "Smoker + non-obese: $21,363"),
            shiny::tags$li(
              "Non-smoker + obese: $8,843"),
            shiny::tags$li(
              "Non-smoker + non-obese: $7,977")
          ),
          shiny::p(
            "Smoking dominates obesity.",
            "A thin smoker costs more than an obese",
            "non-smoker."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "4",
          shiny::h2("The decision tree finds these groups"),
          shiny::p(
            "A decision tree fitted to all six variables",
            "finds the same groups without being told",
            "what to look for."
          ),
          shiny::p(
            "Variable importance shows smoking explains",
            "62% of the variance. Age adds 14%. BMI adds 6%.",
            "Sex, region, and children — not used."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "5",
          shiny::h2("The GLM confirms and corrects"),
          shiny::p(
            "A Gamma GLM with log link estimates the",
            "marginal effect of each variable holding",
            "everything else constant."
          ),
          shiny::p(
            "The smoking multiplier is 4.48x — higher",
            "than the raw 3.8x because the GLM removes",
            "confounding with age and BMI."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "6",
          shiny::h2("Four people. Four risk classes."),
          shiny::p(
            "These are real rows from the dataset.",
            "Selected as typical representatives of",
            "each risk class."
          ),
          shiny::p(
            "Each person pays the same $1,200 annual",
            "premium. But their mortality rates —",
            "and therefore their cover — are very different."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "7",
          shiny::h2("Same biology, different outcome"),
          shiny::p(
            "The smoker/non-smoker health charges ratio",
            "is 3.8x. The smoker/non-smoker mortality",
            "ratio from VBT 2015 is 3.75x."
          ),
          shiny::p(
            "The same variables that predict health costs",
            "predict mortality. Different outcome,",
            "same underlying biology."
          ),
          shiny::p(
            "This is the bridge from health insurance",
            "to life insurance pricing."
          )
        )
      ),
      
      # right panel — plot
      shiny::div(
        class = "scroll-plot",
        plotly::plotlyOutput(ns("plot"),
                             height = "500px")
      )
    )
  )
}

act1_server <- function(id, insurance, tree,
                        glm_model, personas) {
  shiny::moduleServer(id, function(input, output, session) {
    
    current_step <- shiny::reactiveVal(1)
    
    shiny::observe({
      shiny::req(input$scroll_step)
      current_step(as.integer(input$scroll_step))
    })
    
    output$plot <- plotly::renderPlotly({
      step <- current_step()
      
      if (step == 1) {
        plot_scatter_base(insurance)
      } else if (step == 2) {
        plot_scatter_smoker(insurance)
      } else if (step == 3) {
        plot_scatter_four_groups(insurance)
      } else if (step == 4) {
        plot_var_importance(tree)
      } else if (step == 5) {
        plot_glm_coefs(glm_model)
      } else if (step == 6 || step == 7) {
        plot_persona_table(personas)
      }
    })
  })
}