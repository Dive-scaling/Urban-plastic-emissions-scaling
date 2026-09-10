library(quantreg)
library(openxlsx)

# Set working directory
dir <- "Your path/"

# Read data
data <- read.table(paste0(dir, "Global.txt"), header = TRUE) # USA.txt; EU.txt; CHN.txt; IND.txt;
data <- na.omit(data)

# Set population threshold
population_threshold <- 20000

# Keep cities with population greater than or equal to the threshold
data <- data[data$POP >= population_threshold, ]

# Log10-transform population
data$POP <- log10(data$POP)

# Quantile levels
taus <- seq(0.01, 0.99, by = 0.01)

# Response variables
variables <- paste0("PLE", 1:6)

# Output directory
out_dir <- paste0(dir, "2/")

# Run quantile regression for each response variable
for (var in variables) {
  
  # Check whether the variable exists
  if (!var %in% colnames(data)) {
    warning(paste("Variable", var, "not found in the dataset. Skipping..."))
    next
  }
  
  # Check whether the variable has enough positive values
  if (all(data[[var]] == 0) || sum(data[[var]] > 0) < 2) {
    warning(paste("Variable", var, "contains insufficient or all-zero data. Skipping..."))
    next
  }
  
  # Keep positive observations for the current response variable
  data_filtered <- data[data[[var]] > 0, ]
  
  # Log10-transform the response variable
  data_filtered[[var]] <- log10(data_filtered[[var]])
  
  # Model formula
  fml <- as.formula(paste(var, "~ POP"))
  
  # Quantile regression across all quantile levels
  fit_qr <- rq(fml, tau = taus, data = data_filtered)
  
  # Null quantile regression model
  fit_null <- rq(as.formula(paste(var, "~ 1")), tau = taus, data = data_filtered)
  
  # Ordinary least squares regression
  fit_ols <- lm(fml, data = data_filtered)
  
  # Extract quantile regression coefficients
  qr_coef <- t(coef(fit_qr))
  qr_intercept <- qr_coef[, 1]
  qr_slope <- qr_coef[, 2]
  
  # Extract OLS slope
  ols_slope <- coef(fit_ols)["POP"]
  
  # Pseudo R-squared for quantile regression
  R1 <- 1 - fit_qr$rho / fit_null$rho
  R2 <- 1 - (1 - R1)^2
  
  # Standard errors and P values using bootstrap method
  summary_boot <- summary(fit_qr, se = "boot", R = 200) #For Supplementary Table S6-7: R = 200 replace to 1000
  
  se_boot <- sapply(summary_boot, function(x) {
    x$coefficients["POP", "Std. Error"]
  })
  
  p_boot <- sapply(summary_boot, function(x) {
    x$coefficients["POP", "Pr(>|t|)"]
  })
  
  # Standard errors and P values using nid method
  summary_nid <- summary(fit_qr, se = "nid")
  
  se_nid <- sapply(summary_nid, function(x) {
    x$coefficients["POP", "Std. Error"]
  })
  
  p_nid <- sapply(summary_nid, function(x) {
    x$coefficients["POP", "Pr(>|t|)"]
  })
  
  # Standard errors and P values using kernel method
  summary_ker <- summary(fit_qr, se = "ker")
  
  se_ker <- sapply(summary_ker, function(x) {
    x$coefficients["POP", "Std. Error"]
  })
  
  p_ker <- sapply(summary_ker, function(x) {
    x$coefficients["POP", "Pr(>|t|)"]
  })
  
  # Confidence intervals using rank method
  summary_rank <- summary(fit_qr, se = "rank")
  
  lower_rank <- sapply(summary_rank, function(x) {
    x$coefficients["POP", "lower bd"]
  })
  
  upper_rank <- sapply(summary_rank, function(x) {
    x$coefficients["POP", "upper bd"]
  })
  
  # P value for OLS slope
  ols_sum <- summary(fit_ols)
  p_ols <- ols_sum$coefficients["POP", "Pr(>|t|)"]
  
  # Combine results
  results <- data.frame(
    Tau = taus,
    `QR-Intercept` = qr_intercept,
    `QR-Slope (β2)` = qr_slope,
    `OLS-Slope(β1)` = rep(ols_slope, length(taus)),
    `Pτ` = p_nid,
    Po = rep(p_ols, length(taus)),
    R_squared = R2,
    SE_boot = se_boot,
    P_boot = p_boot,
    SE_nid = se_nid,
    P_nid = p_nid,
    SE_ker = se_ker,
    P_ker = p_ker,
    L_rank = lower_rank,
    U_rank = upper_rank,
    check.names = FALSE
  )
  
  # Save results
  output_file <- paste0(out_dir, "quantile_regression_results-", var, ".xlsx")
  
  write.xlsx(results, file = output_file, rowNames = FALSE)
  
  cat("Results for", var, "saved to", output_file, "\n")
}