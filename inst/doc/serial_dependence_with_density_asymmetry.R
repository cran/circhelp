## ----setup, include = FALSE---------------------------------------------------
library(patchwork)
library(circhelp)
library(data.table)
library(ggplot2)
library(ggridges)

fig_width <- 3.6
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.width = fig_width, fig.height = fig_width, dpi = 320, res = 320,
  out.width = "40%", out.height = "40%"
)

default_font_size <- 14
default_line_size <- 1
default_font <- "sans"

default_theme <- function(default_font_size, default_line_size) {
  theme_light(base_size = default_font_size, base_family = default_font) +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_line(linewidth = default_line_size / 2, colour = "gray"),
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = rel(1)),
      strip.text = element_text(size = rel(1), color = "black", lineheight = rel(0.5)),
      axis.text = element_text(size = default_font_size * 0.75),
      axis.title = element_text(size = rel(1)),
      panel.border = element_blank(),
      strip.background = element_blank(),
      legend.position = "right",
      plot.title = element_text(size = rel(1), hjust = 0.5, lineheight = rel(1)),
      text = element_text(size = default_font_size),
      legend.text = element_text(size = rel(1))
    )
}

theme_set(default_theme(default_font_size, default_line_size))
update_geom_defaults("line", list(linewidth = default_line_size))
update_geom_defaults("pointrange", list(size = default_line_size))
update_geom_defaults("vline", list(linewidth = default_line_size / 2, color = "#AFABAB"))
update_geom_defaults("hline", list(linewidth = default_line_size / 2, color = "#AFABAB"))

default_colors <- c("#3498db", "#009f06", "#AA2255", "#FF7F00")

options(ggplot2.discrete.colour = default_colors)
options(ggplot2.discrete.fill = default_colors)

## ----load_data----------------------------------------------------------------
data <- Pascucci_et_al_2019_data
data[, err := angle_diff_180(reported, orientation)] # response errors
data[, prev_ori := shift(orientation), by = observer] # orientation on previous trial
data[, diff_in_ori := angle_diff_180(prev_ori, orientation)] # shift in orientations between trials
data[, abs_diff_in_ori := abs(diff_in_ori)] # absolute shift, that is, dissimilarity between the current and the previous target

## ----remove_cardinal_biases---------------------------------------------------
data[, c("err_corrected", "is_outlier") := remove_cardinal_biases(err, orientation)[, c("be_c", "is_outlier")], by = observer]

data[, err_rel_to_prev_targ := ifelse(diff_in_ori < 0, -err_corrected, err_corrected)] # bias towards the previous target

# subset the data to remove outliers and trials with no responses / no previous responses
data <- data[!is.na(err_rel_to_prev_targ) & is_outlier == FALSE ]

## ----density_example_single_observer, fig.width=fig_width*2, fig.asp=0.7, out.width='66%'----

err_dens <- density_asymmetry(data[observer == 1],
  circ_space = 180, weights_sd = 10,
  xvar = "abs_diff_in_ori", yvar = "err_rel_to_prev_targ", return_full_density = TRUE
)

ggplot(err_dens[dist %in% c(1, seq(0, 90, 10))], 
       aes(x = x, y = y, color = dist, height = dist / 2000)) +
  geom_ridgeline(aes(group = dist), fill = NA, stat = "identity") +
  labs(x = "Bias towards the previous target, °", y = "Probability density", color = "Dissimilarity, °") +
  scale_color_viridis_c(end = 0.75) +
  geom_vline(linetype = 2, xintercept = 0) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  guides(color = guide_colourbar())

## -----------------------------------------------------------------------------
err_dens <- density_asymmetry(data,
  circ_space = 180, weights_sd = 10,
  xvar = "abs_diff_in_ori", yvar = "err_rel_to_prev_targ", by = c("observer")
)

## -----------------------------------------------------------------------------
mean_err <- copy(data[!is.na(err_rel_to_prev_targ) & is_outlier == FALSE & !is.na(abs_diff_in_ori)])
mean_err[, abs_diff_in_ori_bin := cut(abs_diff_in_ori,
  breaks = seq(0, 90, 10), include.lowest = TRUE, right = FALSE,
  labels = seq(5, 85, 10)
)]
mean_err <- mean_err[, .(
  center = mean(err_rel_to_prev_targ),
  sd = sd(err_rel_to_prev_targ),
  n = .N
), by = .(abs_diff_in_ori_bin)]
mean_err[, abs_diff_in_ori_bin := as.numeric(as.character(abs_diff_in_ori_bin))]
mean_err[, se := sd / sqrt(n)]
mean_err[, ci95 := qt(0.975, pmax(n - 1, 1)) * se]
mean_err[, `:=`(ymin = center - ci95, ymax = center + ci95)]

## ----density, fig.width=fig_width*2, out.width='66%'--------------------------
asym_summary <- err_dens[, .(
  y = mean(delta * 100),
  se = sd(delta * 100) / sqrt(.N),
  df = pmax(.N - 1, 1)
), by = .(dist)]
asym_summary[, `:=`(
  ymin = y - qt(0.975, df) * se,
  ymax = y + qt(0.975, df) * se
)]

p1 <- ggplot(asym_summary, aes(x = dist, y = y, ymin = ymin, ymax = ymax)) +
  geom_line() +
  geom_ribbon(alpha = 0.1) +
  labs(y = "Asymmetry in error probability, %")

p2 <- ggplot(
  mean_err,
  aes(x = abs_diff_in_ori_bin, y = center, ymin = ymin, ymax = ymax)
) +
  geom_line() +
  geom_pointrange() +
  labs(y = "Mean error, °")

(p1 + p2) &
  labs(x = "Absolute orientation difference, °") &
  geom_hline(yintercept = 0, linetype = 2) &
  scale_x_continuous(breaks = seq(0, 90, 30)) &
  coord_cartesian(xlim = c(0, 90))

