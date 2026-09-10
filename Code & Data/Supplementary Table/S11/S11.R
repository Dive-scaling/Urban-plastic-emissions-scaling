
# 0. Environment preparation and packages

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  quantreg,
  openxlsx,
  dplyr,
  tidyr,
  boot,
  foreach,
  doParallel
)


# 1. Basic settings and data loading

base_path <- "Your path/All"
setwd(base_path)

# Read data
data <- read.xlsx("All.xlsx", sheet = 1)

# Convert population to numeric
data$POP <- as.numeric(data$POP)

# Plastic-related response variables
plastic_vars <- c("PRE", "PFE", "PDE", "PBE", "PCE", "PUE")

# Quantile levels
tau_all <- seq(0.01, 0.99, by = 0.01)
target_taus <- tau_all

# Population thresholds
pop_thresholds <- c(20000, 50000)


# 2. Main quantile regression function

run_reg <- function(df, group_name, pop_min, target_tau) {
  
  # Filter cities by population threshold
  df_filtered <- df[df$POP >= pop_min & !is.na(df$POP), ]
  
  if (nrow(df_filtered) < 30) {
    return(NULL)
  }
  
  res_list <- list()
  
  for (var in plastic_vars) {
    
    # Keep valid positive observations for the current plastic variable
    df_p <- df_filtered[!is.na(df_filtered[[var]]) & df_filtered[[var]] > 10, ]
    
    if (nrow(df_p) < 30) {
      next
    }
    
    # Log10-transform population and response variable
    df_p$log10_POP <- log10(df_p$POP)
    df_p[[var]] <- log10(df_p[[var]])
    
    # Keep finite values only
    df_p <- df_p[
      is.finite(df_p$log10_POP) &
        is.finite(df_p[[var]]),
    ]
    
    if (nrow(df_p) < 30) {
      next
    }
    
    # Model formula
    fml <- as.formula(paste(var, "~ log10_POP"))
    
    tryCatch({
      
      # Quantile regression
      fit_qr <- rq(
        fml,
        tau = target_tau,
        data = df_p
      )
      
      # Null quantile regression model
      fit_null <- rq(
        as.formula(paste(var, "~ 1")),
        tau = target_tau,
        data = df_p
      )
      
      # Ordinary least squares regression
      fit_ols <- lm(
        fml,
        data = df_p
      )
      
      # Predicted values from quantile regression
      pred <- predict(fit_qr)
      
      # Extract QR coefficients
      if (is.matrix(coef(fit_qr))) {
        qr_intercept <- coef(fit_qr)["(Intercept)", ]
        qr_slope <- coef(fit_qr)["log10_POP", ]
        taus <- as.numeric(colnames(coef(fit_qr)))
      } else {
        qr_intercept <- coef(fit_qr)["(Intercept)"]
        qr_slope <- coef(fit_qr)["log10_POP"]
        taus <- target_tau
      }
      
      # Extract OLS slope and P value
      ols_sum <- summary(fit_ols)
      ols_slope <- coef(fit_ols)["log10_POP"]
      p_ols <- ols_sum$coefficients["log10_POP", "Pr(>|t|)"]
      
      # Pseudo R-squared for quantile regression
      R1 <- 1 - fit_qr$rho / fit_null$rho
      R2 <- 1 - (1 - R1)^2
      
      # Pinball loss
      if (is.matrix(pred)) {
        pinball <- sapply(1:ncol(pred), function(i) {
          mean(
            (df_p[[var]] - pred[, i]) *
              (taus[i] - (df_p[[var]] - pred[, i] < 0))
          )
        })
      } else {
        pinball <- mean(
          (df_p[[var]] - pred) *
            (taus - (df_p[[var]] - pred < 0))
        )
      }
      
      # Standard errors and P values using bootstrap method
      summary_boot <- summary(
        fit_qr,
        se = "boot",
        R = 200
      )
      
      se_boot <- sapply(summary_boot, function(x) {
        x$coefficients["log10_POP", "Std. Error"]
      })
      
      p_boot <- sapply(summary_boot, function(x) {
        x$coefficients["log10_POP", "Pr(>|t|)"]
      })
      
      # Standard errors and P values using nid method
      summary_nid <- summary(
        fit_qr,
        se = "nid"
      )
      
      se_nid <- sapply(summary_nid, function(x) {
        x$coefficients["log10_POP", "Std. Error"]
      })
      
      p_nid <- sapply(summary_nid, function(x) {
        x$coefficients["log10_POP", "Pr(>|t|)"]
      })
      
      # Standard errors and P values using kernel method
      summary_ker <- summary(
        fit_qr,
        se = "ker"
      )
      
      se_ker <- sapply(summary_ker, function(x) {
        x$coefficients["log10_POP", "Std. Error"]
      })
      
      p_ker <- sapply(summary_ker, function(x) {
        x$coefficients["log10_POP", "Pr(>|t|)"]
      })
      
      # Confidence intervals using rank method
      summary_rank <- summary(
        fit_qr,
        se = "rank"
      )
      
      lower_rank <- sapply(summary_rank, function(x) {
        x$coefficients["log10_POP", "lower bd"]
      })
      
      upper_rank <- sapply(summary_rank, function(x) {
        x$coefficients["log10_POP", "upper bd"]
      })
      
      # Combine results
      res_list[[var]] <- data.frame(
        Group = group_name,
        Plastic = var,
        POP_Cutoff = paste0("POP_", pop_min),
        Sample_Size = nrow(df_p),
        Tau = taus,
        `QR-Intercept` = round(qr_intercept, 6),
        `QR-Slope (β2)` = round(qr_slope, 6),
        `OLS-Slope(β1)` = round(rep(ols_slope, length(taus)), 6),
        `Pτ` = p_nid,
        Po = rep(p_ols, length(taus)),
        R_squared = round(R2, 6),
        SE_boot = se_boot,
        P_boot = p_boot,
        SE_nid = se_nid,
        P_nid = p_nid,
        SE_ker = se_ker,
        P_ker = p_ker,
        L_rank = lower_rank,
        U_rank = upper_rank,
        Pinball_QR = round(pinball, 6),
        check.names = FALSE
      )
      
    }, error = function(e) {
      
      warning(
        paste(
          "Regression failed:",
          group_name,
          var,
          paste0("POP_", pop_min),
          "Error:",
          e$message
        )
      )
      
      return(NULL)
    })
  }
  
  return(bind_rows(res_list))
}


# 3. Group definitions

group_definitions <- list(
  Global = data,
  CHN = data[data$ISO3 == "CHN", ],
  USA = data[data$ISO3 == "USA", ],
  IND = data[data$ISO3 == "IND", ],
  EU_HIC = data[
    grepl("EU", data$OECD_region) &
      data$Income_cat == "HIC",
  ]
)


# 4. CV stability function

calc_cv_stability <- function(df_all) {
  
  cv_results <- df_all %>%
    group_by(Group, Plastic, Tau) %>%
    summarise(
      Slope_Mean = mean(`QR-Slope (β2)`, na.rm = TRUE),
      Slope_SD = sd(`QR-Slope (β2)`, na.rm = TRUE),
      Slope_CV = (Slope_SD / abs(Slope_Mean)) * 100,
      Stability_CV = ifelse(Slope_CV < 10, "Stable", "Fluctuating"),
      .groups = "drop"
    )
  
  return(cv_results)
}


# 5. Run regressions and calculate CV

cat("=== Analysis: quantile regression and CV stability ===\n")
cat(sprintf("Total quantiles to process: %d (0.01 to 0.99)\n", length(target_taus)))

# Start parallel backend
n_cores <- max(1, parallel::detectCores() - 1)

cat(sprintf("Using %d CPU cores for parallel backend...\n", n_cores))

cl <- makeCluster(n_cores)
registerDoParallel(cl)

clusterExport(
  cl,
  c(
    "run_reg",
    "plastic_vars",
    "pop_thresholds",
    "group_definitions",
    "target_taus"
  )
)

clusterEvalQ(cl, {
  library(quantreg)
  library(dplyr)
})

# Run regressions
cat("Running quantile regressions...\n")

res_all <- list()

for (p in pop_thresholds) {
  
  for (g in names(group_definitions)) {
    
    cat("Processing:", g, "with POP cutoff", p, "\n")
    
    res_tmp <- run_reg(
      group_definitions[[g]],
      g,
      p,
      target_taus
    )
    
    if (!is.null(res_tmp)) {
      res_all[[paste(g, p, sep = "_")]] <- res_tmp
    }
  }
}

df_all <- bind_rows(res_all)

# Stop parallel backend
stopCluster(cl)

# Calculate CV stability
cat("Calculating CV stability...\n")

cv_results <- calc_cv_stability(df_all)


# 6. Save results

cat("Saving results...\n")

wb1 <- createWorkbook()

addWorksheet(wb1, "QR_Results")
writeData(wb1, "QR_Results", df_all)

addWorksheet(wb1, "CV_Stability")
writeData(wb1, "CV_Stability", cv_results)

saveWorkbook(
  wb1,
  "Phase_CV_Results-0910.xlsx",
  overwrite = TRUE
)

cat("\nAnalysis completed.\n")
cat("Results were saved to: Phase_CV_Results-0910.xlsx\n")