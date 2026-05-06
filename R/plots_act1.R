# plots_act1.R
# Pure plot functions for Act 1 — What drives premiums?
# All functions return plotly objects.
# No Shiny dependencies — testable at console.

library(plotly)
library(ggplot2)

# ── plot_scatter_base ─────────────────────────────────────
# Opening scatter: age vs charges, no colouring.
# Shows three bands without explanation.

plot_scatter_base <- function(insurance) {
  p <- ggplot(insurance,
              aes(x = age, y = charges,
                  text = paste0(
                    "Age: ", age,
                    "<br>Charges: $",
                    format(round(charges), big.mark = ",")
                  ))) +
    geom_point(alpha = 0.4, colour = "#378ADD",
               size  = 1.5) +
    scale_y_continuous(
      labels = scales::dollar_format()) +
    labs(x = "Age", y = "Annual charges") +
    theme_minimal(base_size = 13)
  
  ggplotly(p, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

# ── plot_scatter_smoker ───────────────────────────────────
# Coloured by smoker status.
# Shows the top band resolving to smokers.

plot_scatter_smoker <- function(insurance) {
  p <- ggplot(insurance,
              aes(x = age, y = charges,
                  colour = smoker,
                  text   = paste0(
                    "Age: ", age,
                    "<br>Charges: $",
                    format(round(charges), big.mark = ","),
                    "<br>Smoker: ", smoker
                  ))) +
    geom_point(alpha = 0.4, size = 1.5) +
    scale_colour_manual(
      values = c("yes" = "#D85A30",
                 "no"  = "#378ADD")) +
    scale_y_continuous(
      labels = scales::dollar_format()) +
    labs(x = "Age", y = "Annual charges",
         colour = "Smoker") +
    theme_minimal(base_size = 13)
  
  ggplotly(p, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

# ── plot_scatter_four_groups ──────────────────────────────
# Coloured by smoker, shaped by obesity.
# Shows four groups.

plot_scatter_four_groups <- function(insurance) {
  insurance$obese <- ifelse(
    insurance$bmi >= 30, "Obese", "Non-obese")
  
  p <- ggplot(insurance,
              aes(x      = age,
                  y      = charges,
                  colour = smoker,
                  shape  = obese,
                  text   = paste0(
                    "Age: ", age,
                    "<br>Charges: $",
                    format(round(charges), big.mark = ","),
                    "<br>Smoker: ", smoker,
                    "<br>BMI: ", round(bmi, 1)
                  ))) +
    geom_point(alpha = 0.4, size = 1.5) +
    scale_colour_manual(
      values = c("yes" = "#D85A30",
                 "no"  = "#378ADD")) +
    scale_y_continuous(
      labels = scales::dollar_format()) +
    labs(x = "Age", y = "Annual charges",
         colour = "Smoker", shape = "BMI") +
    theme_minimal(base_size = 13)
  
  ggplotly(p, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

# ── plot_var_importance ───────────────────────────────────
# Variable importance bar chart from decision tree.

plot_var_importance <- function(tree) {
  imp <- as.data.frame(tree$variable.importance)
  imp$variable <- rownames(imp)
  names(imp)[1] <- "importance"
  imp$importance <- imp$importance / 
    max(imp$importance) * 100
  
  p <- ggplot(imp,
              aes(x    = reorder(variable, importance),
                  y    = importance,
                  text = paste0(variable, ": ",
                                round(importance, 1)))) +
    geom_col(fill = "#378ADD", alpha = 0.8) +
    coord_flip() +
    labs(x = NULL,
         y = "Relative importance (%)") +
    theme_minimal(base_size = 13)
  
  ggplotly(p, tooltip = "text")
}

# ── plot_glm_coefs ────────────────────────────────────────
# GLM exponentiated coefficients as a dot plot.

plot_glm_coefs <- function(glm_model) {
  coefs <- exp(coef(glm_model))
  ci    <- exp(confint(glm_model))
  
  df <- data.frame(
    variable = names(coefs),
    estimate = coefs,
    ci_lo    = ci[, 1],
    ci_hi    = ci[, 2],
    stringsAsFactors = FALSE
  )
  
  df <- df[df$variable != "(Intercept)", ]
  
  df$text <- paste0(
    df$variable,
    "<br>Multiplier: ", round(df$estimate, 3),
    "<br>95% CI: ", round(df$ci_lo, 3),
    " – ", round(df$ci_hi, 3)
  )
  
  p <- ggplot(df,
              aes(x    = reorder(variable, estimate),
                  y    = estimate,
                  text = text)) +
    geom_point(size = 3, colour = "#378ADD") +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                  width = 0.2,
                  colour = "#378ADD",
                  alpha  = 0.6) +
    geom_hline(yintercept = 1,
               linetype   = "dashed",
               colour     = "#888780") +
    coord_flip() +
    labs(x = NULL,
         y = "Charge multiplier (exp coefficient)") +
    theme_minimal(base_size = 13)
  
  ggplotly(p, tooltip = "text")
}

# ── plot_persona_table ────────────────────────────────────
# Persona summary as an interactive plotly table.

plot_persona_table <- function(personas) {
  plot_ly(
    type       = "table",
    header     = list(
      values   = list(
        "Name", "Age", "Sex", "Smoker",
        "BMI", "Risk class",
        "Health charges", "mx"
      ),
      fill     = list(color = "#378ADD"),
      font     = list(color = "white", size = 13),
      align    = "left"
    ),
    cells      = list(
      values   = list(
        personas$name,
        personas$age,
        personas$sex,
        personas$smoker,
        round(personas$bmi, 1),
        personas$risk_class,
        paste0("$", format(round(personas$health_charges),
                           big.mark = ",")),
        format(personas$mx, scientific = TRUE,
               digits = 3)
      ),
      fill     = list(
        color  = c("#F1EFE8", "white")
      ),
      font     = list(size = 12),
      align    = "left"
    )
  )
}