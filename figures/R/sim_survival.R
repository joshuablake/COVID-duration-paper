# =============================================================================
# sim_survival.R
#
# Paper: Blake et al. "Estimating the duration of RT-PCR positivity for
#        SARS-CoV-2 from doubly interval censored data with undetected
#        infections". Biometrics.
#
# Produces: Figure 3 of the main manuscript -- the results of the simulation
#           study (Section 5). Each panel plots the posterior survival curve
#           (median and 95% credible band) recovered by the model against the
#           ground-truth curve used to simulate the data, for a different
#           combination of assumed test sensitivity and hazard prior. Panels
#           (A)-(B) vary the hazard prior under constant sensitivity; panels
#           (C)-(E) and (F)-(H) show the misspecified and time-varying
#           sensitivity scenarios respectively.
#
# Inputs:
#   data/all_posteriors.rds  Posterior draws from fitting the model to each
#                            simulated dataset. Produced by the simulation
#                            repository, see README.md; the model itself is
#                            defined in the cisDurationModel R package.
#   data/input_curves.rds    The duration distributions used as ground truth in
#                            the simulation (and as the informative prior).
#
# Output:   figures/output/sim-results.pdf
#
# Run from the repository root with:
#   Rscript figures/R/sim_survival.R
# =============================================================================

suppressMessages(library(dplyr))
library(ggplot2)
library(patchwork)
library(purrr)
library(tidybayes)
library(tidyr)
source(here::here("figures/R/utils.R"))

base_plot = function(df, colour_curves_by, colour_key = NULL, facet_suffix = "", start_letter = 1) {
    p = df |>
        mutate(
            sensitivity.model = factor(sensitivity.model),
            facet_label = latex2exp::TeX(glue::glue(
                "({LETTERS[dense_rank(sensitivity.model) + start_letter - 1]})\\ $p_{{sens}}{facet_suffix} = {sensitivity.model}"
            ), output = "character")
        ) |>
        ggplot() +
        ggdist::geom_lineribbon(
            aes(
                time, S, ymin = S.lower, ymax = S.upper,
                fill = {{ colour_curves_by }}, colour = {{ colour_curves_by }}
            ),
            linewidth = 1, alpha = 0.3
        ) +
        facet_wrap(~facet_label, labeller = label_parsed) +
        geom_line(aes(time, S), data = truth, alpha = 0.5) +
        theme_survival_time_series() +
        # Left align the strip so each panel's letter sits in its upper-left
        # corner, as the Biometrics figure guidelines ask. The letter cannot go
        # inside the panel because every curve starts at S = 1 in that corner.
        theme(strip.text = element_text(hjust = 0))

    if (!is.null(colour_key)) {
        p = p +
            labs(
                colour = colour_key,
                fill = colour_key
            )
    }
    return(p)
}

tbl_posteriors = readRDS(here::here("data/all_posteriors.rds")) |>
    filter(
        survival_prior %in% c(
            "ATACCC",
            "vague"
        ),
    ) |>
    mutate(
        survival_prior = case_match(
            survival_prior,
            "ATACCC" ~ "Strong",
            "vague" ~ "Weak"
        ),
    )
truth = readRDS(here::here("data/input_curves.rds")) |>
  filter(source == "Combined")

p_constant_sensitivity = tbl_posteriors |>
    filter(sensitivity.simulation == sensitivity.model, sensitivity.simulation < 1) |>
    base_plot(survival_prior) +
    theme(legend.position = "bottom")


p_misspecified_sensitivity = tbl_posteriors |>
    filter(sensitivity.simulation == 0.8, survival_prior == "Strong") |>
    base_plot(
        sensitivity.model,
        colour_key = expression(p[sens]^`(i)`),
        facet_suffix = "^{(i)}",
        start_letter = 3
    ) +
    theme(legend.position = "none")

p_variable_sensitivity = tbl_posteriors |>
    filter(is.na(sensitivity.simulation), survival_prior == "Strong") |>
    base_plot(
        sensitivity.model,
        colour_key = expression(p[sens]^`(i)`),
        facet_suffix = "^{(i)}",
        start_letter = 6
    ) +
    theme(legend.position = "none")

save_figure(
    "sim-results",
    p_constant_sensitivity / p_misspecified_sensitivity / p_variable_sensitivity,
    width = 15,
    height = 20
)