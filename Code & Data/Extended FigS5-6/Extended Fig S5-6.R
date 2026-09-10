library(readxl)
library(ggplot2)
library(dplyr)
library(ggh4x)
library(patchwork)
library(scales)

input_dir <- "path/to/input/"
output_dir <- "path/to/output/"

file_paths <- list(
  S5 = list(
    a = paste0(input_dir, "Extended FigS5a(0.25-0.99).xlsx"),
    b = paste0(input_dir, "Extended FigS5b(0.25-0.99).xlsx")
  ),
  S6 = list(
    a = paste0(input_dir, "Extended FigS6a(0.25-0.99).xlsx"),
    b = paste0(input_dir, "Extended FigS6b(0.25-0.99).xlsx")
  )
)

cat_levels <- c(
  "Rigid",
  "Flexible",
  "Debris",
  "Burned",
  "Collected",
  "Uncollected"
)

cat_color <- c(
  Rigid = "#8FB4DC",
  Flexible = "#8FB4DC",
  Debris = "#5AB883",
  Burned = "#5AB883",
  Collected = "#F39800",
  Uncollected = "#F39800"
)

cat_fill <- c(
  Rigid = "#BEE3F8",
  Flexible = "#BEE3F8",
  Debris = "#CAE5D4",
  Burned = "#CAE5D4",
  Collected = "#F1DAA4",
  Uncollected = "#F1DAA4"
)

cat_bar <- c(
  Rigid = "grey30",
  Flexible = "grey30",
  Debris = "#5AB883",
  Burned = "#5AB883",
  Collected = "#F39800",
  Uncollected = "#F39800"
)

lw_axis <- 0.5 * 0.3528

read_panel_data <- function(file_path, panel_label) {
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  d <- read_excel(file_path)
  
  d$Panel <- panel_label
  
  d <- d %>%
    rename(
      Slope1 = Beta2,
      L1 = CI95_low,
      U1 = CI95_high,
      UE1 = OLS
    )
  
  d
}

make_col <- function(d, tag, show_ylab = TRUE) {
  
  present <- cat_levels[cat_levels %in% unique(d$Category)]
  d$Category <- factor(d$Category, levels = present)
  
  d$pt_color <- cat_color[as.character(d$Category)]
  d$pt_fill <- cat_fill[as.character(d$Category)]
  d$bar_color <- cat_bar[as.character(d$Category)]
  
  label_df <- distinct(d, Category)
  
  ggplot(d, aes(x = Tau)) +
    geom_point(
      aes(y = Slope1, color = pt_color, fill = pt_fill),
      shape = 21,
      size = 2,
      stroke = 0.4
    ) +
    geom_errorbar(
      aes(ymin = L1, ymax = U1, color = bar_color),
      width = 0,
      linewidth = 0.4
    ) +
    geom_hline(
      aes(yintercept = UE1),
      linetype = "dashed",
      linewidth = 0.5,
      color = "black"
    ) +
    scale_color_identity() +
    scale_fill_identity() +
    geom_text(
      data = label_df,
      aes(x = Inf, y = -Inf, label = Category),
      hjust = 1.1,
      vjust = -0.8,
      size = 10 / .pt,
      inherit.aes = FALSE
    ) +
    facet_wrap2(
      ~ Category,
      ncol = 1,
      scales = "free_y",
      axes = "all"
    ) +
    scale_x_continuous(
      limits = c(0.01, 1),
      breaks = c(0.25, 0.5, 0.75, 0.99),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 3),
      expand = expansion(mult = c(0.10, 0.10))
    ) +
    labs(
      x = "Quantile levels",
      y = if (show_ylab) "Scaling exponents" else NULL,
      tag = tag
    ) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 12, color = "black"),
      axis.line = element_line(linewidth = lw_axis, color = "black"),
      axis.ticks = element_line(linewidth = lw_axis, color = "black"),
      strip.text = element_blank(),
      strip.background = element_blank(),
      panel.spacing = unit(2, "pt"),
      plot.tag = element_text(size = 12, face = "bold"),
      plot.margin = margin(2, 4, 2, 2)
    )
}

for (fig_name in names(file_paths)) {
  
  fa <- read_panel_data(file_paths[[fig_name]]$a, "a")
  fb <- read_panel_data(file_paths[[fig_name]]$b, "b")
  
  if (is.null(fa) || is.null(fb)) {
    next
  }
  
  pa <- make_col(fa, "a", show_ylab = TRUE)
  pb <- make_col(fb, "b", show_ylab = TRUE)
  
  p <- pa | pb
  
  n_rows <- max(
    length(cat_levels[cat_levels %in% unique(fa$Category)]),
    length(cat_levels[cat_levels %in% unique(fb$Category)])
  )
  
  panel_w <- 53
  panel_h <- 32
  margin_w <- 28
  margin_h <- 14
  
  out_w <- 2 * panel_w + margin_w
  out_h <- n_rows * panel_h + margin_h
  
  output_pdf <- paste0(
    output_dir,
    "Extended_Fig",
    fig_name,
    "_combined.pdf"
  )
  
  ggsave(
    output_pdf,
    plot = p,
    width = out_w,
    height = out_h,
    units = "mm",
    device = cairo_pdf
  )
  
  print(p)
  
  cat("Saved:", output_pdf, "\n")
}