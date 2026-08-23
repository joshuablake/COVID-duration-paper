# =============================================================================
# CIS_ntot.R
#
# Paper: Blake et al. "Estimating the duration of RT-PCR positivity for
#        SARS-CoV-2 from doubly interval censored data with undetected
#        infections". Biometrics.
#
# Produces: Web Figure 2 of the Supplementary Materials -- how the posterior
#           estimate of n_tot (the total number of infection episodes,
#           including those never detected) changes with the overdispersion
#           parameter r of its negative binomial prior. Referenced from
#           Section 6 of the main manuscript.
#
# Inputs:
#   data/STATS18744/means.rds  Posterior means/intervals released from the ONS
#                              Secure Research Service; see README.md.
#
# Output:   figures/output/CIS_ntot.pdf
#
# Run from the repository root with:
#   Rscript figures/R/CIS_ntot.R
# =============================================================================

suppressMessages(library(dplyr))
library(ggplot2)
library(patchwork)
library(purrr)
library(tidybayes)
library(tidyr)
source(here::here("figures/R/utils.R"))
set.seed(10)

#############################################################################################
### LOAD THE ESTIMATES TO PLOT
#############################################################################################
posterior = readRDS(here::here("data/STATS18744/means.rds"))

#############################################################################################
### CREATE FIGURE
#############################################################################################
fig = posterior |>
    filter(sensitivity == 0.8, survival_prior == "Informative", missed_model == "total") |>
    ggplot() +
    stat_pointinterval(
        aes(r, n_tot),
        .width = 0.95
    ) +
    standard_plot_theming() +
    scale_x_log10() +
    labs(y = expression(n[tot]))
ggsave(
    filename = here::here("figures/output/CIS_ntot.pdf"),
    plot = fig,
    width = 11,
    height = 7,
    units = "cm",
    dpi = 300
)

posterior |>
    filter(sensitivity == 0.8, survival_prior == "Informative", missed_model == "total", r == 22047) |>
    mean_qi(mean_surv)

