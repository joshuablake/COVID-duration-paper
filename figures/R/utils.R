# =============================================================================
# utils.R
#
# Paper: Blake et al. "Estimating the duration of RT-PCR positivity for
#        SARS-CoV-2 from doubly interval censored data with undetected
#        infections". Biometrics.
#
# Shared helpers sourced by the other scripts in figures/R. Defines the common
# ggplot2 theming so that every figure uses the same look, the standard axes
# for survival-curve plots (breaks every 14 days on t, every 0.1 on S), and the
# logit/expit transformations used when working with discrete hazards.
#
# Not run directly; sourced via source(here::here("figures/R/utils.R")).
# =============================================================================

standard_plot_theming = function() {
    rlang::list2(
        theme_minimal(),
    )
}

theme_survival_time_series = function() {
    rlang::list2(
        standard_plot_theming(),
        scale_x_continuous(breaks = 0:100*14, minor_breaks = 0:100*2),
        scale_y_continuous(breaks = 0:10/10, minor_breaks = 0:20/20),
        labs(
            x = "t",
            fill = "Hazard prior",
            colour = "Hazard prior"
        ),
        theme(legend.position = "bottom"),
        coord_cartesian(xlim = c(0, 100))
    )
}

logit = function(x) log(x) - log(1 - x)
expit = function(x) 1 / (1 + exp(-x))