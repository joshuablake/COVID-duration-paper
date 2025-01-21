library(atacccDurationEstimates)
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(tidybayes)
library(tidyr)
source(here::here("figures/R/utils.R"))

dist_levels = c(
  "Beta(0.1, 1.9)",
  "ATACCC"
)

sample_priors = function(alpha, beta, n_samples = 1000) {
  stopifnot(length(alpha) == length(beta))
  tibble(
    time = 1:length(alpha),
    alpha = alpha,
    beta = beta
  ) %>%
    mutate(
      lambda = map2(alpha, beta, ~rbeta(n_samples, .x, .y)),
      .draw = list(1:n_samples),
    ) %>%
    unnest(c(.draw, lambda)) |>
    group_by(.draw) |>
    reframe(
      time = c(time, max(time) + 1),
      S = c(1, cumprod(1 - lambda)),
      lambda = c(lambda, 0)
    )
}

sample_constant_prior = function(alpha, beta, prior_len, n_samples = 1000) {
  stopifnot(length(alpha) == 1)
  stopifnot(length(beta) == 1)
  sample_priors(rep(alpha, prior_len), rep(beta, prior_len), n_samples)
}

beta_plot_dens = function(alpha, beta) {
  reduce2(
    alpha,
    beta,
    function(p, a, b) p + stat_function(aes(colour = glue::glue("Beta({a}, {b})")),
                                        fun = dbeta, args = list(a, b)),
    .init = ggplot(NULL) + labs(colour = "Dist", x = "Value", y = "Density")
  ) +
  standard_plot_theming()
}

beta_plot_surv = function(alpha, beta, prior_len = 30) {
  priors = purrr::map2(
    alpha, beta,
    ~sample_constant_prior(.x, .y, prior_len) |>
      mutate(dist = glue::glue("Beta({.x}, {.y})"))
  ) |>
  bind_rows()
  priors |>
    group_by(dist, time) |>
    mean_qi(S, .simple.names = FALSE) |>
    mutate(dist = factor(dist, levels = dist_levels)) |>
    ggplot(aes(time, S, ymin = S.lower, ymax = S.upper, group = dist)) +
    geom_line(aes(colour = dist)) +
    geom_ribbon(aes(fill = dist), alpha = 0.5) +
    labs(
      x = "Day",
      y = "Survival",
      colour = "Distribution",
      fill = "Distribution"
    ) +
    scale_x_continuous(breaks = seq(0, 100) * 14, minor_breaks = seq(0, 100) * 2) +
    coord_cartesian(c(0, prior_len)) +
    standard_plot_theming()
}

p_vague_surv = beta_plot_surv(0.1, 1.9, prior_len = 30) +
  theme(legend.position = "none")

k_t = 1000 * expit(-0.4 * ((1:40) - 20))

tbl_weak = sample_constant_prior(0.1, 1.9, 40) |>
    mutate(prior = "Weak")

tbl_strong = 
    ## generate h_t from the multinomral approximation
    mvtnorm::rmvnorm(
        20e3,
        mean = ataccc_logit_hazard_mean("hakki"),
        sigma = ataccc_logit_hazard_covar("hakki")
    ) |>
    # convert to a tibble
    as_tibble() |>
    tibble::rowid_to_column(".draw") |>
    pivot_longer(!.draw, names_to = "time", values_to = "h") |>
    ## add Beta process noise
    mutate(
        h = expit(h),
        time = as.integer(time),
        k = k_t[time],
        lambda = rbeta(
            n(),
            h * k + 0.1,
            (1 - h) * k + 1.9
        )
    ) |>
    ## calculate survival
    group_by(.draw) |>
    arrange(time, .by_group = TRUE) |>
    mutate(
        S = c(1, cumprod(1 - lambda)[-n()]),
        F = c(lead(1 - S)[-(n())], 1),
        f = diff(c(0, F))
    ) |>
    ungroup() |>
    mutate(prior = "Strong")

prior_predictive_plot = bind_rows(
    tbl_weak,
    tbl_strong
) |>
    ggplot(aes(time, S, colour = prior, fill = prior)) +
    stat_lineribbon(alpha = 0.4, .width = 0.95) +
    standard_plot_theming() +
    labs(
        x = "Day",
        y = "S",
        colour = "",
        fill = ""
    )

ggsave(
  filename = here::here("figures", "output", "prior_predictive_survival.pdf"),
  plot = prior_predictive_plot,
  device = cairo_pdf,
  width = 15,
  height = 6,
  units = "cm",
  dpi = 300
)
