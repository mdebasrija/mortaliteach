# mod_act2.R
# Shiny module for Act 2 — How does insurance work?

act2_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "scroll-container",
      
      shiny::div(
        class = "scroll-narrative",
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "1",
          shiny::h2("How is a premium calculated?"),
          shiny::p(
            "The annual premium is not arbitrary.",
            "It is derived from the probability of dying",
            "times the amount of cover."
          ),
          shiny::p(
            "annual_premium = mx × face_amount",
            "× risk_multiplier × 1.15"
          ),
          shiny::p(
            "The 1.15 covers administration and profit.",
            "Everything else is mortality."
          ),
          shiny::tags$p(
            shiny::tags$strong("Select a persona:"),
          ),
          shiny::selectInput(
            ns("persona"),
            label   = NULL,
            choices = c("Elena", "Damon",
                        "Caroline", "Niklaus")
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "2",
          shiny::h2("Same budget, different cover"),
          shiny::p(
            "Every person has a $1,200 annual budget.",
            "How much cover does that buy?"
          ),
          shiny::p(
            "face_amount = 1200 / (mx × multiplier × 1.15)"
          ),
          shiny::p(
            "Elena's mortality rate is 0.00012.",
            "Her $1,200 buys over $8 million of cover.",
            "Niklaus's rate is 0.00323 — his $1,200 buys",
            "only $99,000."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "3",
          shiny::h2("The pool — 13,675 people"),
          shiny::p(
            "Scale from one person to the full cohort.",
            "Total net amount at risk: $32.3 billion.",
            "Total annual premium: $16.4 million."
          ),
          shiny::p(
            "Preferred Elite holds most of the NAR",
            "despite being only 24.5% of the pool.",
            "Their low mortality lets them buy",
            "enormous cover."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "4",
          shiny::h2("Why reinsurance exists"),
          shiny::p(
            "One Preferred Elite death produces a claim",
            "of $8 million average.",
            "The insurer collected $1,200 that year."
          ),
          shiny::p(
            "It would take 6,900 years of premium",
            "from that one policy to cover one claim.",
            "No insurer can hold this risk alone."
          ),
          shiny::p(
            "The reinsurer absorbs NAR above $500,000",
            "per policy. The insurer pays a treaty",
            "premium for this protection."
          )
        ),
        
        shiny::div(
          class       = "scroll-section",
          `data-step` = "5",
          shiny::h2("The treaty split"),
          shiny::p(
            "Insurer retains: $4.0 billion (12.4%)",
            "Reinsurer holds: $28.3 billion (87.6%)"
          ),
          shiny::p(
            "Preferred Elite is almost entirely ceded.",
            "Preferred is mostly retained —",
            "most policies are below the $500K limit."
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

act2_server <- function(id, cohort, personas) {
  shiny::moduleServer(id, function(input, output, session) {
    
    current_step <- shiny::reactiveVal(1)
    
    shiny::observe({
      shiny::req(input$scroll_step)
      current_step(as.integer(input$scroll_step))
    })
    
    selected_persona <- shiny::reactive({
      personas[personas$name == input$persona, ]
    })
    
    output$plot <- plotly::renderPlotly({
      step <- current_step()
      
      if (step == 1) {
        plot_premium_formula(selected_persona())
      } else if (step == 2) {
        plot_face_amount_comparison(cohort)
      } else if (step == 3) {
        plot_pool_nar(cohort)
      } else if (step == 4) {
        plot_single_death_impact(cohort)
      } else if (step == 5) {
        plot_pool_nar(cohort)
      }
    })
  })
}