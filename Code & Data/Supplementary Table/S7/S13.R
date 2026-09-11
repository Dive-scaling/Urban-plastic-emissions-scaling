
# Quantile regression, benchmark tests, and cross-quantile comparisons
# All regions are processed in one run
library(quantreg)
library(openxlsx)
library(parallel)
library(pbapply)

# 1. Basic settings

# Base directory
base_dir <- "Your path"

# Input files for all regions
regions <- list(
  Global = file.path(base_dir, "Global.txt"),
  USA    = file.path(base_dir, "USA.txt"),
  EU     = file.path(base_dir, "EU.txt"),
  China  = file.path(base_dir, "CHN.txt"),
  India  = file.path(base_dir, "IND.txt")
)

# Output file
output_file <- file.path(
  base_dir,
  "Quantile_regression_and_cross_quantile_tests_ALLREGIONS.xlsx"
)

# PLE variables and plastic categories
cat_map <- c(
  PLE1 = "Rigid",
  PLE2 = "Flexible",
  PLE3 = "Debris",
  PLE4 = "Burned",
  PLE5 = "Collected",
  PLE6 = "Uncollected"
)

# Parameters
population_threshold <- 20000

taus <- seq(0.01, 0.99, by = 0.01)

key_taus <- c(0.25, 0.50, 0.75, 0.99)

benchmark <- 1

n_boot <- 1000

rq_method <- "fn"

alpha <- 0.05

pboptions(type = "timer")

# 2. Helper functions

pick <- function(tb, nm) {
  
  if (!is.null(tb) && nm %in% names(tb)) {
    as.numeric(tb[[nm]])[1]
  } else {
    NA_real_
  }
}

safe_run <- function(fun) {
  
  res <- tryCatch({
    
    av <- fun()
    
    tb <- av$table
    
    p <- pick(tb, "pvalue")
    
    data.frame(
      Tn = pick(tb, "Tn"),
      P = p,
      stringsAsFactors = FALSE
    )
    
  }, error = function(e) {
    
    data.frame(
      Tn = NA_real_,
      P = NA_real_,
      stringsAsFactors = FALSE
    )
  })
  
  return(res)
}

prepare_region_data <- function(data_file) {
  
  data <- read.table(data_file, header = TRUE)
  
  data <- na.omit(data)
  
  data <- data[data$POP >= population_threshold, ]
  
  data$POP <- log10(data$POP)
  
  vars <- names(cat_map)
  
  dfs <- list()
  
  valid_vars <- character(0)
  
  for (var in vars) {
    
    if (!var %in% colnames(data)) {
      warning(paste(var, "not found. Skipping..."))
      next
    }
    
    if (all(data[[var]] == 0) || sum(data[[var]] > 0) < 2) {
      warning(paste(var, "insufficient data. Skipping..."))
      next
    }
    
    df <- data[data[[var]] > 0, ]
    
    df[[var]] <- log10(df[[var]])
    
    df$.y_offset <- df[[var]] - benchmark * df$POP
    
    dfs[[var]] <- df
    
    valid_vars <- c(valid_vars, var)
  }
  
  return(
    list(
      dfs = dfs,
      valid_vars = valid_vars
    )
  )
}

# 3. Worker functions

worker_main <- function(task) {
  
  region_name <- task$region
  
  var <- task$var
  
  idx <- task$idx
  
  df <- all_dfs[[region_name]][[var]]
  
  ti <- taus[idx]
  
  fm <- as.formula(paste(var, "~ POP"))
  
  fit <- rq(
    fm,
    tau = ti,
    data = df,
    method = rq_method
  )
  
  slope <- as.numeric(coef(fit)["POP"])
  
  f_full <- rq(
    .y_offset ~ POP,
    tau = ti,
    data = df,
    method = rq_method
  )
  
  f_red <- rq(
    .y_offset ~ 1,
    tau = ti,
    data = df,
    method = rq_method
  )
  
  p_nid <- safe_run(function() {
    anova(f_full, f_red, se = "nid")
  })$P
  
  p_ker <- safe_run(function() {
    anova(f_full, f_red, se = "ker")
  })$P
  
  set.seed(
    1000 +
      match(var, names(cat_map)) * 1000 +
      idx +
      match(region_name, names(regions)) * 10000
  )
  
  p_boot <- safe_run(function() {
    anova(f_full, f_red, se = "boot", R = n_boot)
  })$P
  
  ci95 <- tryCatch(
    as.numeric(
      suppressWarnings(
        summary(
          fit,
          se = "rank",
          alpha = 0.05
        )$coefficients["POP", c("lower bd", "upper bd")]
      )
    ),
    error = function(e) c(NA_real_, NA_real_)
  )
  
  scaling_label <- ifelse(
    p_boot >= alpha,
    "Consistent with linear scaling",
    ifelse(
      slope > benchmark,
      "Superlinear",
      "Sublinear"
    )
  )
  
  data.frame(
    Region = region_name,
    Category = cat_map[[var]],
    Tau = ti,
    `QR-Slope (β2)` = slope,
    CI95_low = ci95[1],
    CI95_high = ci95[2],
    Scaling_label = scaling_label,
    P_boot = p_boot,
    P_nid = p_nid,
    P_ker = p_ker,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

worker_pair <- function(task) {
  
  region_name <- task$region
  
  var <- task$var
  
  t_lo <- task$t_lo
  
  t_hi <- task$t_hi
  
  df <- all_dfs[[region_name]][[var]]
  
  fm <- as.formula(paste(var, "~ POP"))
  
  f_lo <- rq(
    fm,
    tau = t_lo,
    data = df,
    method = rq_method
  )
  
  f_hi <- rq(
    fm,
    tau = t_hi,
    data = df,
    method = rq_method
  )
  
  slope_low <- as.numeric(coef(f_lo)["POP"])
  
  slope_high <- as.numeric(coef(f_hi)["POP"])
  
  diff <- slope_high - slope_low
  
  a_nid <- safe_run(function() {
    anova(f_lo, f_hi, joint = TRUE, se = "nid")
  })
  
  a_ker <- safe_run(function() {
    anova(f_lo, f_hi, joint = TRUE, se = "ker")
  })
  
  data.frame(
    Region = region_name,
    var = var,
    Category = cat_map[[var]],
    Tau_low = t_lo,
    Tau_high = t_hi,
    Slope_low = slope_low,
    Slope_high = slope_high,
    Diff = diff,
    P_nid_raw = a_nid$P,
    P_ker_raw = a_ker$P,
    stringsAsFactors = FALSE
  )
}

worker_joint <- function(task) {
  
  region_name <- task$region
  
  var <- task$var
  
  df <- all_dfs[[region_name]][[var]]
  
  fm <- as.formula(paste(var, "~ POP"))
  
  fit_key <- rq(
    fm,
    tau = key_taus,
    data = df,
    method = rq_method
  )
  
  fits <- lapply(key_taus, function(tt) {
    rq(
      fm,
      tau = tt,
      data = df,
      method = rq_method
    )
  })
  
  a_nid <- safe_run(function() {
    anova(fit_key, joint = TRUE, se = "nid")
  })
  
  if (is.na(a_nid$P)) {
    a_nid <- safe_run(function() {
      do.call(
        anova,
        c(fits, list(joint = TRUE, se = "nid"))
      )
    })
  }
  
  a_ker <- safe_run(function() {
    anova(fit_key, joint = TRUE, se = "ker")
  })
  
  if (is.na(a_ker$P)) {
    a_ker <- safe_run(function() {
      do.call(
        anova,
        c(fits, list(joint = TRUE, se = "ker"))
      )
    })
  }
  
  data.frame(
    Region = region_name,
    var = var,
    Category = cat_map[[var]],
    Test = "Joint equality across 0.25/0.50/0.75/0.99",
    P_nid = a_nid$P,
    P_ker = a_ker$P,
    stringsAsFactors = FALSE
  )
}

worker_boot <- function(task) {
  
  region_name <- task$region
  
  var <- task$var
  
  nb <- task$nb
  
  seed <- task$seed
  
  df <- all_dfs[[region_name]][[var]]
  
  y <- df[[var]]
  
  X <- cbind(1, df$POP)
  
  n <- length(y)
  
  set.seed(seed)
  
  out <- matrix(
    NA_real_,
    nb,
    length(key_taus)
  )
  
  for (b in seq_len(nb)) {
    
    idx <- sample.int(n, n, replace = TRUE)
    
    yb <- y[idx]
    
    Xb <- X[idx, , drop = FALSE]
    
    for (j in seq_along(key_taus)) {
      
      cf <- tryCatch(
        rq.fit.fnb(
          Xb,
          yb,
          tau = key_taus[j]
        )$coefficients,
        error = function(e) c(NA_real_, NA_real_)
      )
      
      out[b, j] <- cf[2]
    }
  }
  
  list(
    region = region_name,
    var = var,
    mat = out
  )
}

# 4. Prepare all regional data

all_dfs <- list()

all_valid_vars <- list()

for (rg in names(regions)) {
  
  cat("Reading and preparing:", rg, "\n")
  
  prep <- prepare_region_data(regions[[rg]])
  
  all_dfs[[rg]] <- prep$dfs
  
  all_valid_vars[[rg]] <- prep$valid_vars
}

# 5. Build all tasks

tasks_main <- list()

for (rg in names(regions)) {
  
  for (var in all_valid_vars[[rg]]) {
    
    for (idx in seq_along(taus)) {
      
      tasks_main[[length(tasks_main) + 1]] <- list(
        region = rg,
        var = var,
        idx = idx
      )
    }
  }
}

pair_idx <- combn(key_taus, 2)

tasks_pair <- list()

for (rg in names(regions)) {
  
  for (var in all_valid_vars[[rg]]) {
    
    for (j in seq_len(ncol(pair_idx))) {
      
      tasks_pair[[length(tasks_pair) + 1]] <- list(
        region = rg,
        var = var,
        t_lo = pair_idx[1, j],
        t_hi = pair_idx[2, j]
      )
    }
  }
}

tasks_joint <- list()

for (rg in names(regions)) {
  
  for (var in all_valid_vars[[rg]]) {
    
    tasks_joint[[length(tasks_joint) + 1]] <- list(
      region = rg,
      var = var
    )
  }
}

n_chunks <- 20

nb_each <- ceiling(n_boot / n_chunks)

tasks_boot <- list()

for (rg in names(regions)) {
  
  for (var in all_valid_vars[[rg]]) {
    
    for (k in seq_len(n_chunks)) {
      
      tasks_boot[[length(tasks_boot) + 1]] <- list(
        region = rg,
        var = var,
        nb = nb_each,
        seed = 10000 +
          97 * match(var, names(cat_map)) +
          1000 * match(rg, names(regions)) +
          k
      )
    }
  }
}

# 6. Parallel computing

n_cores <- max(1, detectCores() - 1)

cat("Using", n_cores, "cores.\n")

cl <- makeCluster(n_cores)

clusterEvalQ(cl, {
  library(quantreg)
})

clusterExport(
  cl,
  c(
    "all_dfs",
    "all_valid_vars",
    "regions",
    "taus",
    "key_taus",
    "n_boot",
    "benchmark",
    "rq_method",
    "cat_map",
    "alpha",
    "safe_run",
    "pick"
  )
)

cat("\n[1/4] Full quantile regressions and benchmark tests\n")

res_main <- pblapply(
  tasks_main,
  worker_main,
  cl = cl
)

cat("\n[2/4] Pairwise cross-quantile tests\n")

res_pair <- pblapply(
  tasks_pair,
  worker_pair,
  cl = cl
)

cat("\n[3/4] Joint cross-quantile tests\n")

res_joint <- pblapply(
  tasks_joint,
  worker_joint,
  cl = cl
)

cat("\n[4/4] Bootstrap slopes for key quantiles\n")

res_boot <- pblapply(
  tasks_boot,
  worker_boot,
  cl = cl
)

stopCluster(cl)

# 7. Combine results

full_df <- do.call(rbind, res_main)

pair_df <- do.call(rbind, res_pair)

joint_df <- do.call(rbind, res_joint)

# 8. Bootstrap-based cross-quantile P values

BOOT <- list()

for (rg in names(regions)) {
  
  for (var in all_valid_vars[[rg]]) {
    
    key <- paste(rg, var, sep = "_")
    
    BOOT[[key]] <- do.call(
      rbind,
      lapply(
        res_boot[
          sapply(res_boot, function(x) {
            x$region == rg && x$var == var
          })
        ],
        `[[`,
        "mat"
      )
    )
  }
}

pair_df$P_boot_raw <- NA_real_

for (rg in names(regions)) {
  
  for (var in all_valid_vars[[rg]]) {
    
    key <- paste(rg, var, sep = "_")
    
    M <- BOOT[[key]]
    
    M <- M[complete.cases(M), , drop = FALSE]
    
    cn <- cat_map[[var]]
    
    for (j in seq_len(ncol(pair_idx))) {
      
      tl <- pair_idx[1, j]
      
      th <- pair_idx[2, j]
      
      jl <- which(key_taus == tl)
      
      jh <- which(key_taus == th)
      
      sel <- pair_df$Region == rg &
        pair_df$Category == cn &
        pair_df$Tau_low == tl &
        pair_df$Tau_high == th
      
      se_d <- sd(M[, jh] - M[, jl])
      
      pair_df$P_boot_raw[sel] <- 2 * pnorm(
        -abs(pair_df$Diff[sel] / se_d)
      )
    }
  }
}

joint_df$P_boot <- NA_real_

for (rg in names(regions)) {
  
  for (var in all_valid_vars[[rg]]) {
    
    key <- paste(rg, var, sep = "_")
    
    M <- BOOT[[key]]
    
    M <- M[complete.cases(M), , drop = FALSE]
    
    cn <- cat_map[[var]]
    
    V <- cov(M)
    
    Rm <- cbind(
      -1,
      diag(length(key_taus) - 1)
    )
    
    sl <- full_df[
      full_df$Region == rg &
        full_df$Category == cn &
        full_df$Tau %in% key_taus,
      c("Tau", "QR-Slope (β2)")
    ]
    
    sl <- sl[order(sl$Tau), "QR-Slope (β2)"]
    
    d <- Rm %*% as.numeric(sl)
    
    W <- tryCatch(
      as.numeric(
        t(d) %*%
          solve(Rm %*% V %*% t(Rm)) %*%
          d
      ),
      error = function(e) NA_real_
    )
    
    joint_df$P_boot[
      joint_df$Region == rg &
        joint_df$Category == cn
    ] <- pchisq(
      W,
      df = nrow(Rm),
      lower.tail = FALSE
    )
  }
}

# 9. Holm adjustment for pairwise tests

pair_df$family <- paste(
  pair_df$Region,
  pair_df$Category,
  sep = "_"
)

pair_df$P_nid <- ave(
  pair_df$P_nid_raw,
  pair_df$family,
  FUN = function(x) p.adjust(x, method = "holm")
)

pair_df$P_ker <- ave(
  pair_df$P_ker_raw,
  pair_df$family,
  FUN = function(x) p.adjust(x, method = "holm")
)

pair_df$P_boot <- ave(
  pair_df$P_boot_raw,
  pair_df$family,
  FUN = function(x) p.adjust(x, method = "holm")
)

# 10. Key tables and columns

Full_quantile_results <- full_df[, c(
  "Region",
  "Category",
  "Tau",
  "QR-Slope (β2)",
  "CI95_low",
  "CI95_high",
  "Scaling_label",
  "P_boot",
  "P_nid",
  "P_ker"
)]

names(Full_quantile_results)[
  names(Full_quantile_results) == "Tau"
] <- "τ"

Full_quantile_results$.region_ord <- match(
  Full_quantile_results$Region,
  names(regions)
)

Full_quantile_results$.cat_ord <- match(
  Full_quantile_results$Category,
  cat_map
)

Full_quantile_results <- Full_quantile_results[
  order(
    Full_quantile_results$.region_ord,
    Full_quantile_results$.cat_ord,
    Full_quantile_results$τ
  ),
]

Full_quantile_results$.region_ord <- NULL

Full_quantile_results$.cat_ord <- NULL

Pairwise_quantile_eq <- pair_df[, c(
  "Region",
  "Category",
  "Tau_low",
  "Tau_high",
  "Slope_low",
  "Slope_high",
  "Diff",
  "P_nid",
  "P_ker",
  "P_boot"
)]

names(Pairwise_quantile_eq)[
  names(Pairwise_quantile_eq) == "Tau_low"
] <- "τ_low"

names(Pairwise_quantile_eq)[
  names(Pairwise_quantile_eq) == "Tau_high"
] <- "τ_high"

Pairwise_quantile_eq$.region_ord <- match(
  Pairwise_quantile_eq$Region,
  names(regions)
)

Pairwise_quantile_eq$.cat_ord <- match(
  Pairwise_quantile_eq$Category,
  cat_map
)

Pairwise_quantile_eq <- Pairwise_quantile_eq[
  order(
    Pairwise_quantile_eq$.region_ord,
    Pairwise_quantile_eq$.cat_ord,
    Pairwise_quantile_eq$τ_low,
    Pairwise_quantile_eq$τ_high
  ),
]

Pairwise_quantile_eq$.region_ord <- NULL

Pairwise_quantile_eq$.cat_ord <- NULL

Joint_quantile_eq <- joint_df[, c(
  "Region",
  "Category",
  "Test",
  "P_nid",
  "P_ker",
  "P_boot"
)]

Joint_quantile_eq$.region_ord <- match(
  Joint_quantile_eq$Region,
  names(regions)
)

Joint_quantile_eq$.cat_ord <- match(
  Joint_quantile_eq$Category,
  cat_map
)

Joint_quantile_eq <- Joint_quantile_eq[
  order(
    Joint_quantile_eq$.region_ord,
    Joint_quantile_eq$.cat_ord
  ),
]

Joint_quantile_eq$.region_ord <- NULL

Joint_quantile_eq$.cat_ord <- NULL

# 11. Save results

wb <- createWorkbook()

addWorksheet(wb, "Full_quantile_results")
addWorksheet(wb, "Pairwise_quantile_eq")
addWorksheet(wb, "Joint_quantile_eq")

writeData(
  wb,
  "Full_quantile_results",
  Full_quantile_results
)

writeData(
  wb,
  "Pairwise_quantile_eq",
  Pairwise_quantile_eq
)

writeData(
  wb,
  "Joint_quantile_eq",
  Joint_quantile_eq
)

saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)

cat("\n========== Done. Saved to", output_file, "==========\n")