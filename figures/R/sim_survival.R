suppressMessages(library(dplyr))
library(ggplot2)
library(patchwork)
library(purrr)
library(tidybayes)
library(tidyr)
source(here::here("figures/R/utils.R"))

base_plot = function(df, colour_curves_by, colour_key = NULL, facet_suffix = "") {
    p = df |>
        mutate(
            sensitivity.model = factor(sensitivity.model),
            facet_label = latex2exp::TeX(glue::glue(
                "({LETTERS[dense_rank(sensitivity.model)]})\\ $p_{{sens}}{facet_suffix} = {sensitivity.model}"
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
        theme_survival_time_series()

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
    theme(legend.position = "right")
ggsave(
    filename = here::here("figures/output/sim-constant-sensitivity.pdf"),
    plot = p_constant_sensitivity,
    width = 15,
    height = 6,
    units = "cm",
    dpi = 300
)

p_misspecified_sensitivity = tbl_posteriors |>
    filter(sensitivity.simulation == 0.8, survival_prior == "Strong") |>
    base_plot(sensitivity.model, colour_key = expression(p[sens]^`(i)`), facet_suffix = "^{(i)}") +
    theme(legend.position = "none")

p_variable_sensitivity = tbl_posteriors |>
    filter(is.na(sensitivity.simulation), survival_prior == "Strong") |>
    base_plot(sensitivity.model, colour_key = expression(p[sens]^`(i)`), facet_suffix = "^{(i)}")

ggsave(
    filename = here::here("figures/output/sim-sensitivity.pdf"),
    plot = p_misspecified_sensitivity / p_variable_sensitivity,
    width = 15,
    height = 13,
    units = "cm",
    dpi = 300
)