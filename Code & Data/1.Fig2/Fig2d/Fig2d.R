# Load packages
library(readxl)
library(ggplot2)
library(dplyr)
library(ggh4x)
library(scales)

# Read data
df <- read_excel("path/to/input/Fig2d.xlsx")

df <- df %>%
  rename(
    Slope1 = Beta2,
    L1 = CI95_low,
    U1 = CI95_high,
    UE1 = OLS
  )

# Category order
cat_levels <- c(
  "Rigid", "Debris", "Collected",
  "Flexible", "Burned", "Uncollected"
)

# Colors
color_map <- data.frame(
  Category = cat_levels,
  pt_color = c(
    "#8FB4DC", "#5AB883", "#FFDB82",
    "#8FB4DC", "#5AB883", "#FFDB82"
  ),
  pt_fill = c(
    "#BEE3F8", "#CAE5D4", "#F1DAA4",
    "#BEE3F8", "#CAE5D4", "#F1DAA4"
  ),
  bar_color = c(
    "grey30", "#5AB883", "#F39800",
    "grey30", "#5AB883", "#F39800"
  ),
  stringsAsFactors = FALSE
)

df <- left_join(df, color_map, by = "Category")
df$Category <- factor(df$Category, levels = cat_levels)

label_df <- df %>%
  distinct(Category)

label_df$Category <- factor(label_df$Category, levels = cat_levels)

lw_axis <- 0.5 * 0.3528

# Plot
p <- ggplot(df, aes(x = Tau)) +
  geom_point(
    aes(y = Slope1, color = pt_color, fill = pt_fill),
    shape = 21,
    size = 3,
    stroke = 0.5
  ) +
  geom_errorbar(
    aes(ymin = L1, ymax = U1, color = bar_color),
    width = 0,
    linewidth = 0.5
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
    size = 9 / .pt,
    inherit.aes = FALSE
  ) +
  facet_wrap2(
    ~ Category,
    ncol = 3,
    dir = "h",
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
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    x = "Quantile levels",
    y = "Scaling exponents"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.line = element_line(linewidth = lw_axis, color = "black"),
    axis.ticks = element_line(linewidth = lw_axis, color = "black"),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(6, "pt"),
    legend.position = "none"
  )

# Save figure
ggsave(
  "path/to/output/Fig2d.pdf",
  plot = p,
  width = 210,
  height = 93,
  units = "mm",
  device = cairo_pdf
)

print(p)