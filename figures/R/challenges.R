library(dplyr)
library(ggplot2)
library(latex2exp)
library(patchwork)
library(tibble)
library(tidyr)

plot_testing_schedule = function(x, break_modifier = 0) {
  scale_colours = c(
      test = "black",
      positive = "red",
      negative = "seagreen",
      infected = "blue",
      recovered = "purple"
  )
  scale_colours = scale_colours[unique(x$type)]
  scale_shapes = c(
    test = 4,
    positive = 3,
    negative = 4,
    infected = 20,
    recovered = 20
  )
  scale_shapes = scale_shapes[unique(x$type)]
  
  inf_boxes = x %>% 
    filter(type %in% c("infected", "recovered")) %>% 
    pivot_wider(id_cols = individual, names_from = type, values_from = time)
  
  x_breaks = c((0:3) * 7, (1:5) * 28) - break_modifier
  
  p = x %>% 
    ggplot(aes(time, individual)) +
    geom_point(aes(shape = type, colour = type), size = 1.5, stroke = 2) +
    geom_hline(aes(yintercept = individual), colour = "grey") +
    scale_y_discrete() +
    scale_x_continuous(breaks = NULL, minor_breaks = x_breaks,
      limits = c(-3, 84)) +
    scale_shape_manual(values = scale_shapes) +
    scale_colour_manual(values = scale_colours) +
    xlab("Time") +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      panel.grid.major = element_line(colour = "black"),
      legend.text = element_text(size = 10),
      legend.margin = margin(t = -10)
    )
  
  if (nrow(inf_boxes) > 0) {
    p = p +
      geom_rect(aes(xmin = infected, xmax = recovered,
                  ymin = individual - 0.5, ymax = individual + 0.5,
                  x = NULL),
              data = inf_boxes, fill = "red", alpha = 0.1)
  }
  
  return(p)
}

p_censor = tibble(
  time = c((0:3) * 7, (1:3) * 28),
) %>% 
  mutate(
    type = if_else(time >= 10 & time <= 30,
                   "positive", "negative")
  ) %>% 
  bind_rows(tribble(
    ~time, ~type,
    10, "infected",
    40, "recovered",
  )) %>% 
  mutate(individual = 1) %>% 
  plot_testing_schedule() +
  annotate("rect", xmin = 8, xmax = 14, ymin = 0.5, ymax = 1.5, alpha = 0.3, fill = "grey") +
  annotate("text", x = 8, y = -0.5, label = TeX("$l_j^{(b)}$")) +
  annotate("text", x = 14, y = -0.5, label = TeX("$r_j^{(b)}$")) +
  annotate("rect", xmin = 28, xmax = 55, ymin = 0.5, ymax = 1.5, alpha = 0.3, fill = "grey") +
  annotate("text", x = 28, y = -0.5, label = TeX("$l_j^{(e)}$")) +
  annotate("text", x = 55, y = -0.5, label = TeX("$r_j^{(e)}$"))

p_truncation = expand_grid(
  time = c((0:3) * 7, (1:3) * 28),
  individual = 1:2,
  type = "negative",
) %>% 
  bind_rows(tribble(
    ~individual, ~time, ~type,
    1, 33, "infected",
    1, 43, "recovered",
  )) %>% 
  plot_testing_schedule() +
  labs(x = "") +
  theme(legend.position = "none") +
  # no x-axis labels
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

# p_missed = tibble(
#   time = c((0:3) * 7, (1:3) * 28),
# ) %>% 
#   mutate(
#     type = if_else(time <= 7, "positive", "negative")
#   ) %>% 
#   bind_rows(tribble(
#     ~time, ~type,
#     -3, "infected",
#     8, "recovered",
#   )) %>% 
#   mutate(individual = 1) %>% 
#   plot_testing_schedule() +
#   labs(x = "") +
#   theme(legend.position = "none") +
#   # no x-axis labels
#   theme(axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.ticks.x = element_blank())

final_plot = p_truncation / p_censor +
  plot_annotation(tag_levels = 'A') +
  plot_layout(heights = c(1.6, 1, 1.3)) &
  theme(plot.margin = margin(0))#, plot.tag.position = c(0.01, 0.7))

ggsave(
  filename = "figures/output/challenges.pdf",
  plot = final_plot,
  width = 6,
  height = 2,
  dpi = 300,
  unit = "in"
)
