# plots_act3.R
# Pure plot functions for Act 3 — Did we price it right?
# All functions return plotly objects.
# No Shiny dependencies — testable at console.

# ── plot_ae_overall ───────────────────────────────────────
# Single number display — overall A/E with context.

plot_ae_overall <- function(study) {
  
  ae    <- study$ae_total
  A     <- study$A_total
  E     <- round(study$E_total, 1)
  
  colour <- ifelse(ae > 1.1, "#D85A30",
                   ifelse(ae < 0.9, "#1D9E75", "#EF9F27"))
  
  plot_ly(
    type = "indicator",
    mode = "gauge+number+delta",
    value = ae,
    delta = list(
      reference  = 1.0,
      increasing = list(color = "#D85A30"),
      decreasing = list(color = "#1D9E75")
    ),
    gauge = list(
      axis      = list(range = list(0, 3)),
      bar       = list(color = colour),
      steps     = list(
        list(range = c(0, 0.9),
             color = "#E1F5EE"),
        list(range = c(0.9, 1.1),
             color = "#FAEEDA"),
        list(range = c(1.1, 3),
             color = "#FAECE7")
      ),
      threshold = list(
        line  = list(color = "black", width = 2),
        value = 1.0
      )
    ),
    title = list(
      text = paste0("Overall A/E<br>",
                    "<span style='font-size:12px'>",
                    "A = ", format(A, big.mark = ","),
                    "  E = ",
                    format(E, big.mark = ","),
                    "</span>")
    )
  )
}

# ── plot_ae_by_class ──────────────────────────────────────
# Forest plot — A/E with Byar CIs by risk class.

plot_ae_by_class <- function(study) {
  
  df <- study$results
  df$risk_class <- factor(
    df$risk_class,
    levels = rev(c("Preferred Elite", "Preferred",
                   "Substandard", "Substandard High"))
  )
  
  colours <- c(
    "Preferred Elite" = "#1D9E75",
    "Preferred"       = "#378ADD",
    "Substandard"     = "#EF9F27",
    "Substandard High"= "#D85A30"
  )
  
  df$text <- paste0(
    df$risk_class,
    "<br>A/E: ", df$ae,
    "<br>95% CI: ", df$ci_lo, " – ", df$ci_hi,
    "<br>A = ", df$A,
    "  E = ", round(df$E, 1),
    "<br>Reliable: ", df$reliable
  )
  
  plot_ly(df,
          x         = ~ae,
          y         = ~risk_class,
          type      = "scatter",
          mode      = "markers",
          marker    = list(
            size  = 12,
            color = colours[as.character(df$risk_class)]
          ),
          error_x   = list(
            type       = "data",
            symmetric  = FALSE,
            array      = ~round(ci_hi - ae, 3),
            arrayminus = ~round(ae - ci_lo, 3),
            color      = colours[as.character(df$risk_class)]
          ),
          text      = ~text,
          hoverinfo = "text") %>%
    layout(
      xaxis = list(
        title    = "A/E ratio",
        zeroline = FALSE
      ),
      yaxis     = list(title = ""),
      shapes    = list(
        list(
          type    = "line",
          x0      = 1, x1 = 1,
          y0      = 0, y1 = 1,
          yref    = "paper",
          line    = list(
            color = "black",
            width = 1,
            dash  = "dash"
          )
        )
      ),
      showlegend = FALSE
    )
}

# ── plot_count_vs_amount_ae ───────────────────────────────
# Side by side count A/E vs amount A/E by risk class.

plot_count_vs_amount_ae <- function(exp_basis, study) {
  
  count_ae <- study$results[,
                            c("risk_class", "ae")]
  names(count_ae)[2] <- "count_ae"
  
  amount_ae <- aggregate(
    cbind(died_amount = died * nar,
          exp_amount  = expected * nar) ~
      risk_class,
    data = exp_basis,
    FUN  = sum
  )
  amount_ae$amount_ae <- round(
    amount_ae$died_amount / amount_ae$exp_amount, 3
  )
  
  df <- merge(count_ae, amount_ae,
              by = "risk_class")
  
  df$risk_class <- factor(
    df$risk_class,
    levels = c("Preferred Elite", "Preferred",
               "Substandard", "Substandard High")
  )
  
  plot_ly(df,
          x         = ~risk_class,
          y         = ~count_ae,
          type      = "bar",
          name      = "Count A/E",
          marker    = list(color = "#378ADD"),
          text      = ~paste0("Count A/E: ", count_ae),
          hoverinfo = "text") %>%
    add_trace(
      y         = ~amount_ae,
      name      = "Amount A/E",
      marker    = list(color = "#D85A30"),
      text      = ~paste0("Amount A/E: ", amount_ae),
      hoverinfo = "text"
    ) %>%
    layout(
      barmode = "group",
      xaxis   = list(title = "Risk class"),
      yaxis   = list(title = "A/E ratio"),
      shapes  = list(
        list(
          type = "line",
          x0   = -0.5, x1 = 3.5,
          y0   = 1.0,  y1 = 1.0,
          line = list(
            color = "black",
            width = 1,
            dash  = "dash"
          )
        )
      )
    )
}

# ── plot_repricing ────────────────────────────────────────
# Old vs new multipliers with credibility shown.

plot_repricing <- function(repricing) {
  
  repricing$risk_class <- factor(
    repricing$risk_class,
    levels = c("Preferred Elite", "Preferred",
               "Substandard", "Substandard High")
  )
  
  repricing$text <- paste0(
    repricing$risk_class,
    "<br>Old multiplier: ", repricing$multiplier, "x",
    "<br>New multiplier: ", repricing$new_multiplier, "x",
    "<br>Change: ", repricing$premium_change_pct, "%",
    "<br>Credibility: ",
    round(repricing$credibility * 100, 1), "%"
  )
  
  plot_ly(repricing,
          x         = ~risk_class,
          y         = ~multiplier,
          type      = "bar",
          name      = "Current multiplier",
          marker    = list(color = "#B5D4F4"),
          text      = ~text,
          hoverinfo = "text") %>%
    add_trace(
      y    = ~new_multiplier,
      name = "Proposed multiplier",
      marker = list(color = "#378ADD"),
      text   = ~text,
      hoverinfo = "text"
    ) %>%
    layout(
      barmode = "group",
      xaxis   = list(title = "Risk class"),
      yaxis   = list(title = "Multiplier")
    )
}

# ── plot_treaty_cashflow ──────────────────────────────────
# Compares premium collected vs claims paid for reinsurer.

plot_treaty_cashflow <- function(exp_basis, cohort) {
  
  treaty <- aggregate(
    cbind(ceded_premium, ceded_nar) ~ risk_class,
    data = cohort,
    FUN  = sum
  )
  
  claims <- aggregate(
    cbind(ceded_claim = died * ceded_nar) ~ risk_class,
    data = merge(
      exp_basis[exp_basis$died == 1,
                c("seqn", "died")],
      cohort[, c("seqn", "risk_class",
                 "ceded_nar")],
      by = "seqn"
    ),
    FUN = sum
  )
  
  df <- merge(treaty, claims,
              by = "risk_class", all.x = TRUE)
  df$ceded_claim[is.na(df$ceded_claim)] <- 0
  
  df$risk_class <- factor(
    df$risk_class,
    levels = c("Preferred Elite", "Preferred",
               "Substandard", "Substandard High")
  )
  
  df$text_prem <- paste0(
    df$risk_class,
    "<br>Treaty premium: $",
    format(round(df$ceded_premium),
           big.mark = ",")
  )
  
  df$text_claim <- paste0(
    df$risk_class,
    "<br>Ceded claims: $",
    format(round(df$ceded_claim),
           big.mark = ",")
  )
  
  plot_ly(df,
          x         = ~risk_class,
          y         = ~round(ceded_premium / 1e6, 2),
          type      = "bar",
          name      = "Treaty premium",
          marker    = list(color = "#1D9E75"),
          text      = ~text_prem,
          hoverinfo = "text") %>%
    add_trace(
      y         = ~round(ceded_claim / 1e6, 2),
      name      = "Ceded claims",
      marker    = list(color = "#D85A30"),
      text      = ~text_claim,
      hoverinfo = "text"
    ) %>%
    layout(
      barmode = "group",
      xaxis   = list(title = "Risk class"),
      yaxis   = list(title = "Amount ($ millions)")
    )
}