# plots_act2.R
# Pure plot functions for Act 2 — How does insurance work?
# All functions return plotly objects.
# No Shiny dependencies — testable at console.

# ── plot_premium_formula ──────────────────────────────────
# Shows premium formula components for one persona.
# Bar chart: mx × multiplier × loading × face_amount

plot_premium_formula <- function(persona_row) {
  
  components <- data.frame(
    component = c("Base mortality (mx)",
                  "Risk multiplier",
                  "Expense loading",
                  "Annual premium"),
    value     = c(
      persona_row$mx,
      persona_row$mx * persona_row$multiplier,
      persona_row$mx * persona_row$multiplier * 1.15,
      1200 / persona_row$face_amount
    ),
    stringsAsFactors = FALSE
  )
  
  plot_ly(
    data = components,
    x    = ~component,
    y    = ~round(value, 6),
    type = "bar",
    marker = list(color = "#378ADD"),
    text   = ~paste0(component, ": ",
                     round(value, 6)),
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Rate per dollar of cover"),
      showlegend = FALSE
    )
}

# ── plot_face_amount_comparison ───────────────────────────
# Bar chart showing face amount by risk class.
# Same $1,200 budget — different cover.

plot_face_amount_comparison <- function(cohort) {
  
  avg_fa <- aggregate(
    face_amount ~ risk_class,
    data = cohort,
    FUN  = mean
  )
  
  avg_fa$risk_class <- factor(
    avg_fa$risk_class,
    levels = c("Preferred Elite", "Preferred",
               "Substandard", "Substandard High")
  )
  
  colours <- c(
    "Preferred Elite" = "#1D9E75",
    "Preferred"       = "#378ADD",
    "Substandard"     = "#EF9F27",
    "Substandard High"= "#D85A30"
  )
  
  plot_ly(
    data   = avg_fa,
    x      = ~risk_class,
    y      = ~round(face_amount),
    type   = "bar",
    marker = list(
      color = colours[avg_fa$risk_class]
    ),
    text   = ~paste0(
      risk_class,
      "<br>Mean face amount: $",
      format(round(face_amount), big.mark = ",")
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(title = "Risk class"),
      yaxis = list(
        title      = "Mean face amount ($)",
        tickformat = "$,.0f"
      ),
      showlegend = FALSE
    )
}

# ── plot_pool_nar ─────────────────────────────────────────
# Stacked bar showing total NAR by risk class.
# Retained vs ceded split.

plot_pool_nar <- function(cohort) {
  
  nar_summary <- aggregate(
    cbind(retained_nar, ceded_nar) ~ risk_class,
    data = cohort,
    FUN  = sum
  )
  
  nar_summary$risk_class <- factor(
    nar_summary$risk_class,
    levels = c("Preferred Elite", "Preferred",
               "Substandard", "Substandard High")
  )
  
  plot_ly(nar_summary,
          x    = ~risk_class,
          y    = ~round(retained_nar / 1e9, 2),
          type = "bar",
          name = "Retained",
          marker = list(color = "#378ADD"),
          text   = ~paste0(
            "Retained: $",
            round(retained_nar / 1e9, 2), "B"
          ),
          hoverinfo = "text") %>%
    add_trace(
      y      = ~round(ceded_nar / 1e9, 2),
      name   = "Ceded",
      marker = list(color = "#D85A30"),
      text   = ~paste0(
        "Ceded: $",
        round(ceded_nar / 1e9, 2), "B"
      ),
      hoverinfo = "text"
    ) %>%
    layout(
      barmode = "stack",
      xaxis   = list(title = "Risk class"),
      yaxis   = list(title = "Total NAR ($ billions)")
    )
}

# ── plot_single_death_impact ──────────────────────────────
# Shows claim size vs annual premium for each risk class.
# Makes the reinsurance case visually.

plot_single_death_impact <- function(cohort) {
  
  impact <- aggregate(
    cbind(face_amount, annual_premium) ~ risk_class,
    data = cohort,
    FUN  = mean
  )
  
  impact$risk_class <- factor(
    impact$risk_class,
    levels = c("Preferred Elite", "Preferred",
               "Substandard", "Substandard High")
  )
  
  impact$years_of_premium <- round(
    impact$face_amount / impact$annual_premium
  )
  
  plot_ly(
    data      = impact,
    x         = ~risk_class,
    y         = ~years_of_premium,
    type      = "bar",
    marker    = list(
      color   = c("#1D9E75","#378ADD",
                  "#EF9F27","#D85A30")
    ),
    text      = ~paste0(
      risk_class,
      "<br>Mean face: $",
      format(round(face_amount), big.mark = ","),
      "<br>Years of premium to cover one claim: ",
      format(years_of_premium, big.mark = ",")
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(title = "Risk class"),
      yaxis = list(
        title = "Years of premium to cover one claim"
      ),
      showlegend = FALSE
    )
}