## ----include = FALSE----------------------------------------------------------
library(patchwork)
library(circhelp)
library(data.table)
library(ggplot2)

fig_width <- 3.6
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.width = fig_width, fig.height = fig_width, 
  dpi = 320, res = 320,
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

## ----setup--------------------------------------------------------------------
# load the data
# data <- fread('https://zenodo.org/record/2544946/files/Experiment2_rawdata.csv?download=1')
data <- Pascucci_et_al_2019_data
data[, err := angle_diff_180(reported, orientation)] # response errors
data[, prev_ori := shift(orientation), by = observer] # orientation on previous trial
data[, diff_in_ori := angle_diff_180(prev_ori, orientation)] # shift in orientations between trials

## ----serial_dependence--------------------------------------------------------
ggplot(pad_circ(data, "diff_in_ori"), aes(x = diff_in_ori, y = err)) +
  geom_line(aes(group = observer), stat = "smooth", size = 0.4, color = "black", alpha = 0.2, method = "loess") +
  geom_smooth(se = T, method = "loess") +
  coord_cartesian(xlim = c(-90, 90)) +
  scale_x_continuous(breaks = seq(-90, 90, 90)) +
  labs(y = "Error, °", x = "Orientation difference, °")

## ----card_biases_ex-----------------------------------------------------------
ggplot(data[observer == 4, ], aes(x = angle_diff_180(orientation, 0), y = err)) +
  geom_point() +
  coord_cartesian(xlim = c(-90, 90)) +
  scale_x_continuous(breaks = seq(-90, 90, 90)) +
  labs(y = "Error, °", x = "Orientation, °")

## ----correct_biases_ex, fig.width = fig_width*3, out.width='100%'-------------
ex_subj_data <- data[observer == 4, ]
res <- remove_cardinal_biases(ex_subj_data$err, ex_subj_data$orientation, plots = "show")

## -----------------------------------------------------------------------------
data[, c("err_corrected", "is_outlier") := remove_cardinal_biases(err, orientation)[, c("be_c", "is_outlier")], by = observer]

## -----------------------------------------------------------------------------
data[, err_mean_corrected := angle_diff_180(err, circ_mean_180(err)), by = observer]

## ----raw_vs_corrected, fig.width = fig_width*3, out.width='100%'--------------
datam <- melt(data[!is.na(diff_in_ori)], id.vars = c("diff_in_ori", "observer", "is_outlier"), measure.vars = c("err", "err_corrected", "err_mean_corrected"))
datam[, variablef := factor(variable, levels = c("err", "err_mean_corrected", "err_corrected"), labels = c("Raw error", "Mean-corrected", "Mean and cardinal bias removed"))]
datam[, err_rel_to_prev_targ := ifelse(diff_in_ori < 0, -value, value)]

ggplot(pad_circ(datam[is_outlier == F], "diff_in_ori"), aes(x = diff_in_ori, y = value)) +
  geom_line(aes(group = observer), stat = "smooth", size = 0.4, color = "black", alpha = 0.2, method = "loess") +
  geom_smooth(se = TRUE, method = "loess") +
  facet_grid(~variablef) +
  coord_cartesian(xlim = c(-90, 90), ylim = c(-3, 3)) +
  scale_x_continuous(breaks = seq(-90, 90, 90)) +
  labs(y = "Error, °", x = "Orientation difference, °")

## ----serdep_collapsed, fig.width = fig_width*3, out.width='100%'--------------
ggplot(datam[is_outlier == F], aes(
  x = abs(diff_in_ori),
  y = err_rel_to_prev_targ,
  color = variablef
)) +
  geom_hline(linetype = 2, yintercept = 0) +
  geom_smooth(se = TRUE, method = "loess") +
  facet_grid(~variablef) +
  coord_cartesian(xlim = c(0, 90)) +
  theme(legend.position = "none") +
  labs(
    color = NULL, y = "Bias towards previous targets",
    x = "Absolute orientation difference, °"
  ) +
  scale_x_continuous(breaks = seq(0, 90, 30))

