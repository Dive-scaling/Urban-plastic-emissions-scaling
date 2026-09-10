library(readxl)
library(quantreg)
library(ggplot2)
library(gridExtra)
library(ggpointdensity)
library(ggrastr)

point_alpha <- 0.4
point_size <- 1.5
text_size <- 2.4
legend_text_size <- 8
axis_label_size <- 9.5
tau_fixed <- 0.99

y_axis_min <- -0.8
y_axis_max <- 5.2

x_axis_min <- 3.8
x_axis_max <- 7.8

dpi_raster <- 300
population_threshold <- 20000

paths <- c(
  "Global" = "path/to/Global.xlsx",
  "USA"    = "path/to/USA.xlsx",
  "EU"     = "path/to/EU.xlsx",
  "CHN"    = "path/to/CHN.xlsx",
  "IND"    = "path/to/IND.xlsx"
)

output_dir <- "path/to/output"

y_label_map <- c(
  "PLE1" = "Log Rigid",
  "PLE2" = "Log Flexible",
  "PLE3" = "Log Debris",
  "PLE4" = "Log Burned",
  "PLE5" = "Log Collected",
  "PLE6" = "Log Uncollected"
)

color_palette <- list(
  row1 = list(low = "#FFFFFF", high = "#BEE3F8"),
  row2 = list(low = "#FFFFFF", high = "#5AB883"),
  row3 = list(low = "#FFFFFF", high = "#FFDB82")
)

calc_pinball <- function(y_true, y_pred, tau) {
  residuals <- y_true - y_pred
  mean(residuals * (tau - (residuals < 0)))
}

make_empty_plot <- function(plot_label, y_variable) {
  ggplot() +
    labs(
      title = plot_label,
      x = "log Urban Population",
      y = y_label_map[y_variable]
    ) +
    scale_y_continuous(
      limits = c(y_axis_min, y_axis_max),
      breaks = seq(0, 4, by = 2)
    ) +
    scale_x_continuous(
      limits = c(x_axis_min, x_axis_max),
      breaks = c(4, 5, 6, 7)
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.title.x = element_text(size = axis_label_size),
      axis.title.y = element_text(size = axis_label_size),
      axis.ticks.length = unit(0.15, "cm"),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      axis.line = element_line(color = "black", linewidth = 0.3),
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0, face = "bold", size = 11),
      legend.position = "none"
    )
}

plot_for_variable <- function(data, y_variable, tau, color_set, plot_label) {
  
  data$POP_clean <- suppressWarnings(as.numeric(as.character(data$POP)))
  data$y_clean <- suppressWarnings(as.numeric(as.character(data[[y_variable]])))
  
  sub_data <- data[
    !is.na(data$POP_clean) &
      data$POP_clean >= population_threshold &
      !is.na(data$y_clean) &
      data$y_clean > 0,
  ]
  
  if (nrow(sub_data) < 5 || length(unique(sub_data$y_clean)) <= 1) {
    return(make_empty_plot(plot_label, y_variable))
  }
  
  sub_data$y_log <- log10(sub_data$y_clean)
  sub_data$x_log <- log10(sub_data$POP_clean)
  
  n <- nrow(sub_data)
  
  if (
    n < 5 ||
    length(unique(sub_data$y_log)) <= 1 ||
    length(unique(sub_data$x_log)) <= 1
  ) {
    return(make_empty_plot(plot_label, y_variable))
  }
  
  fit_null <- lm(y_log ~ 1, data = sub_data)
  null_preds <- predict(fit_null, newdata = sub_data)
  null_pinball <- calc_pinball(sub_data$y_log, null_preds, tau = tau)
  
  if (is.na(null_pinball) || null_pinball == 0) {
    return(make_empty_plot(plot_label, y_variable))
  }
  
  fit_ols <- lm(y_log ~ x_log, data = sub_data)
  ols_preds <- predict(fit_ols, newdata = sub_data)
  ols_pinball <- calc_pinball(sub_data$y_log, ols_preds, tau = tau)
  ols_Ptau <- 1 - (ols_pinball / null_pinball)
  
  ols_intercept <- coef(fit_ols)[1]
  ols_slope <- coef(fit_ols)[2]
  
  ols_sum <- summary(fit_ols)
  t_val_ols <- ols_sum$coefficients["x_log", "t value"]
  ols_p <- ols_sum$coefficients["x_log", "Pr(>|t|)"]
  ci_ols <- confint(fit_ols, "x_log", level = 0.95)
  
  ols_p_str <- if (is.na(ols_p)) {
    "P = NA"
  } else if (ols_p < 2.2e-16) {
    "P < 2.2 \u00d7 10\u207b\u00b9\u2076"
  } else {
    sprintf("P = %.2e", ols_p)
  }
  
  ols_stat_str <- sprintf(
    "t = %.2f, 95%% CI [%.2f, %.2f], %s",
    t_val_ols,
    ci_ols[1],
    ci_ols[2],
    ols_p_str
  )
  
  fit_qr <- rq(y_log ~ x_log, tau = tau, data = sub_data)
  preds_qr <- predict(fit_qr, newdata = sub_data)
  qr_pinball <- calc_pinball(sub_data$y_log, preds_qr, tau = tau)
  qr_Ptau <- 1 - (qr_pinball / null_pinball)
  
  qr_intercept <- coef(fit_qr)[1]
  qr_slope <- coef(fit_qr)[2]
  
  qr_stat_str <- tryCatch({
    qr_rank_sum <- summary(fit_qr, se = "rank", alpha = 0.05)
    
    ci_lower <- qr_rank_sum$coefficients["x_log", "lower bd"]
    ci_upper <- qr_rank_sum$coefficients["x_log", "upper bd"]
    qr_p <- qr_rank_sum$coefficients["x_log", "p-value"]
    
    p_str <- if (is.na(qr_p)) {
      "P = NA"
    } else if (qr_p < 2.2e-16) {
      "P < 2.2 \u00d7 10\u207b\u00b9\u2076"
    } else {
      sprintf("P = %.2e", qr_p)
    }
    
    sprintf(
      "95%% CI [%.2f, %.2f], %s",
      ci_lower,
      ci_upper,
      p_str
    )
  }, error = function(e) {
    qr_nid_sum <- summary(fit_qr, se = "nid")
    
    slope_val <- qr_nid_sum$coefficients["x_log", "Value"]
    se_val <- qr_nid_sum$coefficients["x_log", "Std. Error"]
    qr_p <- qr_nid_sum$coefficients["x_log", "Pr(>|t|)"]
    
    ci_lower <- slope_val - 1.96 * se_val
    ci_upper <- slope_val + 1.96 * se_val
    
    p_str <- if (is.na(qr_p)) {
      "P = NA"
    } else if (qr_p < 2.2e-16) {
      "P < 2.2 \u00d7 10\u207b\u00b9\u2076"
    } else {
      sprintf("P = %.2e", qr_p)
    }
    
    sprintf(
      "95%% CI [%.2f, %.2f], %s",
      ci_lower,
      ci_upper,
      p_str
    )
  })
  
  ggplot(sub_data, aes(x = x_log, y = y_log)) +
    rasterise(
      geom_pointdensity(
        size = point_size,
        alpha = point_alpha
      ),
      dpi = dpi_raster
    ) +
    scale_color_gradient(
      low = color_set$low,
      high = color_set$high,
      name = ""
    ) +
    scale_y_continuous(
      limits = c(y_axis_min, y_axis_max),
      breaks = seq(0, 4, by = 2)
    ) +
    scale_x_continuous(
      limits = c(x_axis_min, x_axis_max),
      breaks = c(4, 5, 6, 7)
    ) +
    geom_abline(
      intercept = ols_intercept,
      slope = ols_slope,
      linetype = "dashed",
      color = "#2C3E50",
      linewidth = 0.5
    ) +
    geom_abline(
      intercept = qr_intercept,
      slope = qr_slope,
      color = "#E74C3C",
      linewidth = 0.5
    ) +
    annotate(
      "text",
      x = x_axis_min + 0.1,
      y = y_axis_max - 0.12,
      label = paste0(
        "E_Q = ",
        round(qr_intercept, 2),
        " + ",
        round(qr_slope, 2),
        " * P\n",
        "P_\u03c4 = ",
        round(qr_Ptau, 2),
        "\n",
        qr_stat_str
      ),
      color = "#E74C3C",
      size = text_size,
      hjust = 0,
      vjust = 1
    ) +
    annotate(
      "text",
      x = x_axis_max - 0.1,
      y = y_axis_min + (y_axis_max - y_axis_min) * 0.02,
      label = paste0(
        "E_o = ",
        round(ols_intercept, 2),
        " + ",
        round(ols_slope, 2),
        " * P\n",
        "P_o = ",
        round(ols_Ptau, 2),
        "\n",
        "n = ",
        format(n, big.mark = ","),
        "\n",
        ols_stat_str
      ),
      color = "#2C3E50",
      size = text_size,
      hjust = 1,
      vjust = 0
    ) +
    labs(
      title = plot_label,
      x = "log Urban Population",
      y = y_label_map[y_variable]
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.title.x = element_text(size = axis_label_size),
      axis.title.y = element_text(size = axis_label_size),
      axis.ticks.length = unit(0.15, "cm"),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      axis.line = element_line(color = "black", linewidth = 0.3),
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0, face = "bold", size = 11),
      legend.position = "bottom",
      legend.key.width = unit(0.8, "cm"),
      legend.key.height = unit(0.2, "cm"),
      legend.text = element_text(size = legend_text_size)
    )
}

for (region_name in names(paths)) {
  
  file_path <- paths[[region_name]]
  
  if (!file.exists(file_path)) {
    warning(paste("File not found, skipped:", file_path))
    next
  }
  
  cat("Generating plots for region:", region_name, "...\n")
  
  data_raw <- suppressWarnings(read_excel(file_path, sheet = 1))
  
  plot_labels <- c("a", "b", "c", "d", "e", "f")
  y_variables <- paste0("PLE", 1:6)
  
  plots_ordered <- lapply(1:6, function(i) {
    
    y_var <- y_variables[i]
    row_index <- ceiling(i / 2)
    color_set <- color_palette[[paste0("row", row_index)]]
    
    cat(
      "  Subplot",
      plot_labels[i],
      "=",
      y_var,
      ", color row",
      row_index,
      "\n"
    )
    
    plot_for_variable(
      data = data_raw,
      y_variable = y_var,
      tau = tau_fixed,
      color_set = color_set,
      plot_label = plot_labels[i]
    )
  })
  
  combined_plot <- arrangeGrob(
    grobs = plots_ordered,
    layout_matrix = rbind(
      c(1, 2),
      c(3, 4),
      c(5, 6)
    ),
    top = NULL
  )
  
  output_pdf_path <- paste0(
    output_dir,
    "/Scaling_",
    region_name,
    "_A4.pdf"
  )
  
  ggsave(
    filename = output_pdf_path,
    plot = combined_plot,
    width = 8.27,
    height = 11.69,
    units = "in",
    dpi = 700,
    device = cairo_pdf
  )
  
  cat("Exported:", output_pdf_path, "\n\n")
}

cat("\nAll regions completed.\n")
cat("Layout:\n")
cat("Row 1: a (Log Rigid)     b (Log Flexible)\n")
cat("Row 2: c (Log Debris)    d (Log Burned)\n")
cat("Row 3: e (Log Collected) f (Log Uncollected)\n")