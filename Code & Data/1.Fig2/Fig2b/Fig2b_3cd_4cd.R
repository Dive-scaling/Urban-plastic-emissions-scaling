# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(readxl)
library(egg)
library(grid)

# 1. File paths and settings

file_paths <- list(
  Type   = "path/to/input/1.Type.xlsx",
  Style  = "path/to/input/2.Style.xlsx",
  Source = "path/to/input/3.Source.xlsx"
)

out_pdf <- "path/to/output/Fig2b.pdf"


# Y-axis labels
y_labels <- list(
  Type   = c(Slope1 = "Rigid", Slope2 = "Flexible"),
  Style  = c(Slope1 = "Debris", Slope2 = "Burned"),
  Source = c(Slope1 = "Collected", Slope2 = "Uncollected")
)


# Colors
color_palette <- c(
  "Superlinear (>1)" = "#F07204",
  "Linear (=1)"      = "#FAB800",
  "Sublinear (<1)"   = "#86A851"
)

# 2. Slope label classification

classify_slope <- function(x) {
  
  x <- trimws(as.character(x))
  
  case_when(
    is.na(x) | x == ""          ~ NA_character_,
    tolower(x) == "superlinear" ~ "Superlinear (>1)",
    tolower(x) == "linear"      ~ "Linear (=1)",
    tolower(x) == "sublinear"   ~ "Sublinear (<1)",
    TRUE                        ~ NA_character_
  )
}

# 3. Read and count data

process_data <- function(file_path, file_name) {
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  df <- as.data.frame(read_excel(file_path))
  
  df_long <- df %>%
    select(starts_with("Slope")) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Slope_Type",
      values_to = "Value"
    ) %>%
    mutate(
      Category = classify_slope(Value),
      Y_Label  = y_labels[[file_name]][Slope_Type]
    ) %>%
    filter(!is.na(Category))
  
  df_count <- df_long %>%
    group_by(Y_Label, Category) %>%
    summarise(Count = n(), .groups = "drop")
  
  all_categories <- expand.grid(
    Y_Label = unique(df_count$Y_Label),
    Category = names(color_palette),
    stringsAsFactors = FALSE
  )
  
  df_final <- left_join(
    all_categories,
    df_count,
    by = c("Y_Label", "Category")
  ) %>%
    mutate(
      Count = replace_na(Count, 0),
      Label = ifelse(Count > 0, as.character(Count), "")
    )
  
  df_final
}

# 4. Merge data

all_data <- bind_rows(
  process_data(file_paths$Type, "Type"),
  process_data(file_paths$Style, "Style"),
  process_data(file_paths$Source, "Source")
)

all_data <- all_data[, c(
  "Y_Label",
  "Category",
  "Count",
  "Label"
)]

all_data$Y_Label <- factor(
  all_data$Y_Label,
  levels = rev(c(
    "Rigid",
    "Flexible",
    "Debris",
    "Burned",
    "Collected",
    "Uncollected"
  ))
)

all_data$Category <- factor(
  all_data$Category,
  levels = c(
    "Superlinear (>1)",
    "Linear (=1)",
    "Sublinear (<1)"
  )
)

# 5. Plot

nature_theme <- theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

p_final <- ggplot(
  all_data,
  aes(x = Y_Label, y = Count, fill = Category)
) +
  geom_bar(
    stat = "identity",
    position = "stack",
    width = 0.7,
    color = "white",
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = Label),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    color = "black"
  ) +
  scale_fill_manual(values = color_palette) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 99),
    breaks = c(1, 50, 99),
    labels = c("1", "50", "99"),
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Total Count",
    fill = "Scaling Regime"
  ) +
  nature_theme

print(p_final)

# 6. Save PDF

p_fixed <- set_panel_size(
  p_final,
  width  = unit(85, "mm"),
  height = unit(45, "mm")
)

cairo_pdf(
  filename = out_pdf,
  width  = 210 / 25.4,
  height = 297 / 25.4,
  family = "sans"
)

grid.newpage()

pushViewport(
  viewport(
    x = 0.5,
    y = 0.5,
    just = c("center", "center")
  )
)

grid.draw(p_fixed)

popViewport()

dev.off()

cat("Saved:", out_pdf, "\n")