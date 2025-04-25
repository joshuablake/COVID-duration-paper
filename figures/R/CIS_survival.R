suppressMessages(library(dplyr))
library(ggplot2)
library(patchwork)
library(purrr)
library(tidybayes)
library(tidyr)
source(here::here("figures/R/utils.R"))

#############################################################################################
### LOAD THE ESTIMATES TO PLOT
#############################################################################################

posterior_draws = readRDS(here::here("data/STATS18744/draws.rds"))
posterior_draws = bind_rows(
    posterior_draws,
    readRDS(here::here("data/STATS17701/draws.rds")) |>
        filter(!name %in% posterior_draws$name)
)
median_survival = posterior_draws |>
    summarise(
        median = min(time[S <= 0.5]),
        .by = c(
            .draw,
            survival_prior,
            sensitivity,
            missed_model,
            r
        )
    )

ataccc_duration = atacccDurationEstimates::ataccc_posterior_samples() |>
    group_by(time) |>
    median_qi()
ataccc_medians = atacccDurationEstimates::ataccc_posterior_samples() |>
    group_by(.draw) |>
    summarise(median = min(time[S <= 0.5]))

check_counts = posterior_draws |>
    filter(time == 1, .draw == 1, missed_model == "total") |>
    count(
        survival_prior,
        sensitivity,
        missed_model,
        r
    )
stopifnot(check_counts$n == 1)
rm(check_counts)

#############################################################################################
### FIGURE ASSUMING NO FALSE NEGATIVES
#############################################################################################

# p_perfect = posterior_draws |>
#     filter(sensitivity == 1, survival_prior == "Informative", r == 22047) |>
#     ggplot() +
#     stat_lineribbon(
#         aes(time, S, fill = "CIS-based", colour = "CIS-based"),
#         alpha = 0.5,
#         .width = 0.95
#     ) +
#     geom_lineribbon(
#         aes(time, S, ymin = S.lower, ymax = S.upper, fill = "ATACCC-based", colour = "ATACCC-based"),
#         data = ataccc_duration,
#         alpha = 0.5
#     ) +
#     theme_survival_time_series() +
#     labs(
#         fill = "",
#         colour = ""
#     ) +
#     coord_cartesian(xlim = c(0, 45))
# ggsave(
#     filename = here::here("figures/output/CIS_perfect.pdf"),
#     plot = p_perfect,
#     width = 15,
#     height = 9,
#     units = "cm",
#     dpi = 300
# )

#############################################################################################
### FIGURE WITH FINAL RESULTS
#############################################################################################

final_curve = posterior_draws |>
    filter(sensitivity == 0.8, survival_prior == "Informative", r == 22047, missed_model == "total")
check_counts = final_curve |>
    count(.draw, time)
stopifnot(check_counts$n == 1)
rm(check_counts)

final_curve |>
    filter(time %in% c(12, 26, 88)) |>
    group_by(time) |>
    median_qi(S) |>
    print()

ataccc_duration |>
    filter(time %in% c(12, 26, 88)) |>
    select(time, starts_with("S")) |>
    print()

p_final = final_curve |>
    ggplot() +
    stat_lineribbon(
        aes(time, S, fill = "CIS-based", colour = "CIS-based"),
        alpha = 0.5,
        .width = 0.95
    ) +
    geom_lineribbon(
        aes(time, S, ymin = S.lower, ymax = S.upper, fill = "ATACCC-based", colour = "ATACCC-based"),
        data = ataccc_duration,
        alpha = 0.5
    ) +
    theme_survival_time_series() +
    labs(
        fill = "",
        colour = ""
    ) +
    theme(legend.position = "right")
ggsave(
    filename = here::here("figures/output/CIS_final.pdf"),
    plot = p_final,
    width = 15,
    height = 6,
    units = "cm",
    dpi = 300
)

#############################################################################################
### SENSITIVITY ANALYSIS FIGURE
#############################################################################################

update_stat_defaults("pointinterval", list(size = 0.5, .width = 0.95))
colour_scheme_intervals = c(
    "ATACCC-based" = "orange",
    "Final" = "purple",
    "Other" = "dark green"
)

ggplot_median_base = function(..., ataccc_x_loc = 0.1) {
    ggplot(...) +
        stat_pointinterval(
            aes(ataccc_x_loc, median, colour = "ATACCC-based"),
            data = ataccc_medians
        ) +
        standard_plot_theming() +
        theme(legend.position = "bottom") +
        labs(colour = "", y = "Median survival time") +
        coord_cartesian(ylim = c(0, 25)) +
        scale_colour_manual(values = colour_scheme_intervals)
}
ggplot_day50_base = function(...) {
    ggplot(...) +
        standard_plot_theming() +
        coord_cartesian(ylim = c(0, 0.12)) +
        theme(legend.position = "bottom") +
        labs(y = "Day 50 survival", colour = "") +
        scale_colour_manual(values = colour_scheme_intervals)
}

change_colour_scheme_one_level = function(p, level_to_colour, custom_colour, base_scheme = scales::brewer_pal(palette = "Greens")) {
    p = as.factor(p)
    colour_scheme = base_scheme(nlevels(p))
    names(colour_scheme) = levels(p)
    colour_scheme[level_to_colour] = custom_colour
    return(colour_scheme)
}

scale_colour_one_level = function(...) {
    colour_scheme = change_colour_scheme_one_level(...)
    return(scale_colour_manual(values = colour_scheme))
}

scale_fill_one_level = function(...) {
    colour_scheme = change_colour_scheme_one_level(...)
    return(scale_fill_manual(values = colour_scheme))
}

r_medians = median_survival |>
    filter(sensitivity == 0.8, survival_prior == "Informative", missed_model == "total") |>
    ggplot_median_base() +
    stat_pointinterval(aes(r, median, colour = ifelse(r == 22047, "Final", "Other"))) +
    scale_x_log10(limits = c(0.09, NA)) +
    labs(x = "r")
r_day_50 = posterior_draws |>
    filter(sensitivity == 0.8, survival_prior == "Informative", time == 50, missed_model == "total") |>
    ggplot_day50_base() +
    stat_pointinterval(aes(r, S, colour = ifelse(r == 22047, "Final", "Other"))) +
    scale_x_log10() +
    labs(x = "r")
 r_curves = posterior_draws |>
    filter(sensitivity == 0.8, survival_prior == "Informative", missed_model == "total") |>
    mutate(r = as.factor(r)) |>
    ggplot() +
    stat_lineribbon(aes(time, S, fill = r, colour = r), alpha = 0.5, .width = 0.95) +
    theme_survival_time_series() +
    labs(colour = "r", fill = "r") +
    scale_fill_one_level(posterior_draws$r, "22047", "purple") +
    scale_colour_one_level(posterior_draws$r, "22047", "purple")

sens_draws = posterior_draws |>
    filter(r == 22047, survival_prior == "Informative", missed_model == "total") |>
    mutate(sensitivity = as.factor(sensitivity))
sens_medians = median_survival |>
    filter(r == 22047, survival_prior == "Informative", missed_model == "total") |>
    ggplot_median_base(ataccc_x_loc = 0.5) +
    stat_pointinterval(aes(sensitivity, median, colour = ifelse(sensitivity == 0.8, "Final", "Other"))) +
    labs(x = expression(p[sens])) +
    xlim(0.49, 1)
sens_day_50 = sens_draws |>
    filter(time == 50) |>
    ggplot_day50_base() +
    stat_pointinterval(aes(sensitivity, S, colour = ifelse(sensitivity == 0.8, "Final", "Other"))) +
    labs(x = expression(p[sens]))
sens_curves = sens_draws |>
    ggplot() +
    stat_lineribbon(aes(time, S, fill = sensitivity, colour = sensitivity), alpha = 0.5, .width = 0.95) +
    theme_survival_time_series() +
    labs(colour = expression(p[sens]), fill = expression(p[sens])) +
    scale_fill_one_level(posterior_draws$sensitivity, "0.8", "purple") +
    scale_colour_one_level(posterior_draws$sensitivity, "0.8", "purple")
ggsave(
    filename = here::here("figures/output/CIS_vary.pdf"),
    plot = (r_medians + r_day_50 + r_curves) / (sens_medians + sens_day_50 + sens_curves) +
        plot_annotation(tag_levels = "A") &
        theme(
            text = element_text(size = 8),
            legend.key.size = unit(0.3, "cm")
        ),
    width = 18,
    height = 20,
    units = "cm",
    dpi = 300
)

posterior_draws |>
    filter(sensitivity == 0.8, survival_prior == "Informative", time == 50, missed_model == "total") |>
    filter(r == 22047) |>
    mean_qi(S)
