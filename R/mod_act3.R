# mod_act3.R
# Shiny module for Act 3 — Did we price it right?

act3_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "scroll-container",
      
      shiny::div(
        class = "scroll-narrative",
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "1",
          shiny::h2("Did we price it right?"),
          shiny::p(
            "We priced 13,675 policies using VBT 2015",
            "mortality rates. 4,087 participants died",
            "during follow-up."
          ),
          shiny::p(
            "The experience study asks: how many deaths",
            "did we expect? How many actually happened?",
            "The ratio is A/E."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "2",
          shiny::h2("Overall A/E: 1.192"),
          shiny::p(
            "Actual mortality was 19% above expected.",
            "The VBT rates understated the mortality",
            "of this population."
          ),
          shiny::p(
            "This is the select population effect:",
            "VBT rates are calibrated on insured lives",
            "who passed underwriting. NHANES is general",
            "population — higher mortality on average."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "3",
          shiny::h2("Preferred Elite: the surprise"),
          shiny::p(
            "The lowest-risk group has the worst A/E.",
            "Preferred Elite A/E = 2.834."
          ),
          shiny::p(
            "The CI is 1.467 to 4.824 —",
            "entirely above 1.0.",
            "This is statistically significant."
          ),
          shiny::p(
            "Smoker groups cannot reject A/E = 1.0.",
            "VBT smoker rates, calibrated on a more",
            "general population, are adequate here."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "4",
          shiny::h2("Count A/E vs amount A/E"),
          shiny::p(
            "Count A/E measures deaths.",
            "Amount A/E weights each death by the",
            "net amount at risk — the financial exposure."
          ),
          shiny::p(
            "Where they diverge tells you whether",
            "the expensive deaths are concentrated",
            "in one group."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "5",
          shiny::h2("Repricing the treaty"),
          shiny::p(
            "New multipliers are credibility-weighted.",
            "A group with few deaths gets a small",
            "weight — its A/E may be noise."
          ),
          shiny::p(
            "Preferred Elite has only 100 deaths:",
            "9.2% credibility. Despite A/E of 2.834,",
            "the new multiplier is only 1.17x."
          ),
          shiny::p(
            "Preferred has 1,470 deaths: full credibility.",
            "Its 1.238 A/E fully drives repricing to 1.55x."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "6",
          shiny::h2("The reinsurer's position"),
          shiny::p(
            "For every risk class, compare treaty premium",
            "collected against ceded claims paid."
          ),
          shiny::p(
            "This is what the reinsurer presents at",
            "treaty renewal. The groups where claims",
            "exceed premium are the renegotiation targets."
          )
        )
      ),
      
      shiny::div(
        class = "scroll-plot",
        plotly::plotlyOutput(ns("plot"),
                             height = "500px")
      )
    )
  )
}

act3_server <- function(id, study_rc, exp_basis,
                        cohort, repricing) {
  shiny::moduleServer(id, function(input, output, session) {
    
    current_step <- shiny::reactiveVal(1)
    
    shiny::observe({
      shiny::req(input$scroll_step)
      current_step(as.integer(input$scroll_step))
    })
    
    output$plot <- plotly::renderPlotly({
      step <- current_step()
      
      if (step == 1 || step == 2) {
        plot_ae_overall(study_rc)
      } else if (step == 3) {
        plot_ae_by_class(study_rc)
      } else if (step == 4) {
        plot_count_vs_amount_ae(exp_basis, study_rc)
      } else if (step == 5) {
        plot_repricing(repricing)
      } else if (step == 6) {
        plot_treaty_cashflow(exp_basis, cohort)
      }
    })
  })
}