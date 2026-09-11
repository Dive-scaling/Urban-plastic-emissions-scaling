# ==============================================================================
# Population and plastic emission summary
# ==============================================================================

library(openxlsx)
library(dplyr)

# ==============================================================================
# Basic settings and data loading
# ==============================================================================

# Set working directory
base_path <- "Your path/All"
setwd(base_path)

# Read data
data <- read.xlsx("All.xlsx", sheet = 1)

# Keep required columns
core_cols <- c(
  "ISO3",
  "Income_cat",
  "OECD_region",
  "POP",
  "RPE",
  "PFE",
  "PDE",
  "PBE",
  "PCE",
  "PUE"
)

data <- data[, core_cols]

# Remove missing values
data <- na.omit(data)

# Convert population to numeric
data$POP <- as.numeric(data$POP)

# Keep records with valid population values
data <- data[!is.na(data$POP), ]

# Population threshold
pop_threshold <- 20000

# Plastic emission variables
plastic_vars <- c("RPE", "PFE", "PDE", "PBE", "PCE", "PUE")

# ==============================================================================
# Summary statistics function
# ==============================================================================

full_statistics <- function(df, threshold, plastics) {
  
  # Keep all cities with positive population
  df_all_pos <- df[df$POP > 0, ]
  
  # Keep cities with population greater than or equal to the threshold
  df_20k <- df[df$POP >= threshold, ]
  
  # Population statistics
  pop_stats <- data.frame(
    Type = "Population",
    Item = c(
      "Total_Cities_POP_ge20k",
      "Total_POP_ge20k",
      "All_Cities_POP_gt0",
      "All_POP_gt0",
      "City_Ratio_ge20k",
      "POP_Ratio_ge20k"
    ),
    Value = c(
      nrow(df_20k),
      sum(df_20k$POP),
      nrow(df_all_pos),
      sum(df_all_pos$POP),
      nrow(df_20k) / nrow(df_all_pos),
      sum(df_20k$POP) / sum(df_all_pos$POP)
    )
  )
  
  # Plastic emission statistics
  plastic_stats <- list()
  
  for (p in plastics) {
    
    total_p <- sum(df_all_pos[[p]], na.rm = TRUE)
    geo20k_p <- sum(df_20k[[p]], na.rm = TRUE)
    ratio_p <- geo20k_p / total_p
    
    plastic_stats[[p]] <- data.frame(
      Type = p,
      Item = c(
        "Total_Emission_ALL",
        "Emission_ge20k",
        "Emission_Ratio_ge20k"
      ),
      Value = c(
        total_p,
        geo20k_p,
        ratio_p
      )
    )
  }
  
  plastic_all <- bind_rows(plastic_stats)
  
  final_stat <- bind_rows(
    pop_stats,
    plastic_all
  )
  
  return(final_stat)
}

# ==============================================================================
# Run summary statistics
# ==============================================================================

summary_result <- full_statistics(
  data,
  pop_threshold,
  plastic_vars
)

# ==============================================================================
# Save results
# ==============================================================================

write.csv(
  summary_result,
  "01_Full_Pop_Plastic_Summary.csv",
  row.names = FALSE
)

write.xlsx(
  summary_result,
  "01_Full_Pop_Plastic_Summary.xlsx",
  rowNames = FALSE
)

cat("Population and plastic emission summary completed.\n")
cat("Results were saved to:\n")
cat("01_Full_Pop_Plastic_Summary.csv\n")
cat("01_Full_Pop_Plastic_Summary.xlsx\n")