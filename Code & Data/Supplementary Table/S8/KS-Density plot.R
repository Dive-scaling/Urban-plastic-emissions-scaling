# 1. Load packages
required_packages <- c("readxl", "writexl", "e1071", "ggplot2", "gridExtra")

new_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]

if (length(new_packages)) install.packages(new_packages)

library(readxl)
library(writexl)
library(e1071)
library(ggplot2)
library(gridExtra)

# 2. File paths

paths <- c(
  "Global" = "path/to/Global.xlsx",
  "USA"    = "path/to/USA.xlsx",
  "EU"     = "path/to/EU.xlsx",
  "CHN"    = "path/to/CHN.xlsx",
  "IND"    = "path/to/IND.xlsx"
)

output_summary_path <- "path/to/output/Summary_Distribution_Statistics_AllData.xlsx"

all_stats_list <- list()

row_colors <- c("#BEE3F8", "#5AB883", "#FFDB82")



# 3. Plot function

plot_density <- function(data, variable, stats_row,
                         fill_color = "#77C034",
                         theoretical_color = "black") {
  
  annot_text <- paste0(
    stats_row$Skew_Label, "\n",
    "KS-D value = ", sprintf("%.4f", stats_row$KS_Statistic_D), "\n",
    stats_row$KS_p_label
  )
  
  p <- ggplot(data, aes(x = .data[[variable]])) +
    labs(
      title = paste(stats_row$Region, "-", variable),
      x = paste0("log10(", gsub("_log", "", variable), ")"),
      y = "Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.ticks.length = unit(0.15, "cm"),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold")
    )
  
  if (stats_row$Skew_Label != "No variation (SD=0)") {
    
    p <- p +
      geom_density(
        color = fill_color,
        fill = fill_color,
        alpha = 0.4,
        linewidth = 0.8
      ) +
      stat_function(
        fun = dnorm,
        args = list(mean = stats_row$Mean, sd = stats_row$SD),
        color = theoretical_color,
        linetype = "dashed",
        linewidth = 0.8
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = annot_text,
        hjust = 1.05,
        vjust = 1.15,
        size = 3,
        fontface = "italic"
      )
    
  } else {
    
    p <- p +
      annotate(
        "text",
        x = mean(data[[variable]], na.rm = TRUE),
        y = 0.5,
        label = "Data constant (SD = 0)",
        size = 3,
        color = "red"
      )
  }
  
  p
}



# 4. Loop through regions

for (region_name in names(paths)) {
  
  file_path <- paths[[region_name]]
  
  if (!file.exists(file_path)) {
    warning(paste("File not found, skipped:", file_path))
    next
  }
  
  data <- read_excel(file_path, sheet = 1)
  
  for (col in paste0("PLE", 1:6)) {
    if (col %in% colnames(data)) {
      data[[col]] <- as.numeric(as.character(data[[col]]))
    }
  }
  
  data <- na.omit(data)
  
  log_vars <- paste0("PLE", 1:6, "_log")
  
  for (i in 1:6) {
    orig_col <- paste0("PLE", i)
    log_col  <- paste0("PLE", i, "_log")
    data[[log_col]] <- log10(data[[orig_col]] + 1)
  }
  
  data <- data[complete.cases(data[, log_vars]), ]
  
  region_stats_list <- list()
  
  for (var in log_vars) {
    
    x <- data[[var]]
    
    mean_val <- mean(x, na.rm = TRUE)
    med_val  <- median(x, na.rm = TRUE)
    sd_val   <- sd(x, na.rm = TRUE)
    
    is_constant <- is.na(sd_val) || sd_val == 0
    
    if (is_constant) {
      
      skew_val   <- NA
      skew_label <- "No variation (SD=0)"
      ks_stat    <- NA
      p_exact    <- NA
      p_label    <- "p = NA"
      
    } else {
      
      skew_val <- skewness(x, na.rm = TRUE)
      
      skew_label <- if (is.na(skew_val)) {
        "Skewness unknown"
      } else if (skew_val > 0) {
        "Positive skewness"
      } else {
        "Negative skewness"
      }
      
      ks_res <- suppressWarnings(
        ks.test(
          x,
          "pnorm",
          mean = mean_val,
          sd = sd_val,
          alternative = "two.sided"
        )
      )
      
      ks_stat <- unname(ks_res$statistic)
      p_exact <- ks_res$p.value
      
      p_label <- if (is.na(p_exact)) {
        "p = NA"
      } else if (p_exact < 2.2e-16) {
        "p < 2.2e-16"
      } else if (p_exact < 0.001) {
        sprintf("p = %.2e", p_exact)
      } else {
        sprintf("p = %.3f", p_exact)
      }
    }
    
    region_stats_list[[var]] <- data.frame(
      Region = region_name,
      Variable = var,
      Mean = mean_val,
      Median = med_val,
      SD = sd_val,
      Skewness = skew_val,
      Skew_Label = skew_label,
      KS_Statistic_D = ks_stat,
      KS_p_value = p_exact,
      KS_p_label = p_label,
      Test_Type = "Two-sided Kolmogorov-Smirnov test"
    )
  }
  
  region_df <- do.call(rbind, region_stats_list)
  
  region_df <- region_df[, c(
    "Region",
    "Variable",
    "Mean",
    "Median",
    "SD",
    "Skewness",
    "Skew_Label",
    "KS_Statistic_D",
    "KS_p_value",
    "KS_p_label",
    "Test_Type"
  )]
  
  all_stats_list[[region_name]] <- region_df
  
  plots <- lapply(1:6, function(i) {
    
    var_name  <- paste0("PLE", i, "_log")
    row_index <- ceiling(i / 2)
    
    stats_row <- region_df[region_df$Variable == var_name, ]
    
    plot_density(
      data = data,
      variable = var_name,
      stats_row = stats_row,
      fill_color = row_colors[row_index]
    )
  })
  
  cat("Plotting density figures for:", region_name, "\n")
  
  grid.arrange(
    grobs = plots,
    nrow = 3,
    ncol = 2
  )
}



# 5. Export summary table

final_summary_df <- do.call(rbind, all_stats_list)
rownames(final_summary_df) <- NULL

final_summary_df <- final_summary_df[, c(
  "Region",
  "Variable",
  "Mean",
  "Median",
  "SD",
  "Skewness",
  "Skew_Label",
  "KS_Statistic_D",
  "KS_p_value",
  "KS_p_label",
  "Test_Type"
)]

write_xlsx(final_summary_df, output_summary_path)

cat("Done. Results exported to:\n", output_summary_path, "\n")