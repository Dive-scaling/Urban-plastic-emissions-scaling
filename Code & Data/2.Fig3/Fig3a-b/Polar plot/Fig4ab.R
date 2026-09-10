# Load required packages
library(plotly)
library(htmlwidgets)

# Create input data
data <- data.frame(
  category = c("PRE", "PFE", "PDE", "PBE", "PCE", "PUE"),
  value = c(0.98, 1.00, 1.06, 0.93, 1.04, 1.00),
  baseline = c(0.98, 1.04, 0.99, 0.77, 0.99, 0.74)
)

# Define legend labels
legend_labels <- c(
  "(A) Sublinear (Decrease Return)",
  "(B) Linear (Pause Return)",
  "(C) Superlinear (Increase Return)"
)

# Function for adding a polar bar-style subplot
add_pb_subplot <- function(data,
                           column,
                           fig,
                           max_val = 1.2,
                           min_val = 0.6,
                           interval = 0.2,
                           show_safety_grid = TRUE,
                           show_category_labels = TRUE) {
  
  # Number of sectors
  n <- nrow(data)
  
  # Calculate the starting angle and middle angle of each sector
  begin_thetas <- seq(0, 360, length.out = n + 1)[-(n + 1)]
  mid_thetas <- begin_thetas + 360 / (2 * n)
  
  # ------------------------------------------------------------
  # Add a light grey background disk
  # ------------------------------------------------------------
  bg_theta <- seq(0, 360, length.out = 361)
  
  fig <- fig %>%
    add_trace(
      type = "scatterpolar",
      r = rep(max_val, length(bg_theta)),
      theta = bg_theta,
      mode = "lines",
      fill = "toself",
      fillcolor = "rgba(235, 235, 235, 0.65)",
      line = list(
        color = "rgba(220, 220, 220, 0.8)",
        width = 0.3
      ),
      hoverinfo = "skip",
      showlegend = FALSE
    )
  
  # ------------------------------------------------------------
  # Add colored sector areas
  # ------------------------------------------------------------
  for (i in 1:n) {
    
    # Extract the value for the current sector
    value <- data[[column]][i]
    
    # Assign color according to the value
    sector_color <- ifelse(
      value < 1,
      "#86A851",                 # Sublinear: green
      ifelse(
        value == 1,
        "#FAB800",               # Consistent with Linear Scaling: yellow
        "#F07204"                # Superlinear: orange
      )
    )
    
    # Define the angular span of the current sector
    sector_theta_outer <- seq(
      begin_thetas[i],
      begin_thetas[i] + 360 / n,
      length.out = 120
    )
    
    # Build a closed ring-sector polygon using outer and inner arcs
    sector_theta <- c(
      sector_theta_outer,
      rev(sector_theta_outer)
    )
    
    sector_r <- c(
      rep(value, length(sector_theta_outer)),
      rep(min_val, length(sector_theta_outer))
    )
    
    # Add the sector polygon
    fig <- fig %>%
      add_trace(
        type = "scatterpolar",
        r = sector_r,
        theta = sector_theta,
        mode = "lines",
        fill = "toself",
        fillcolor = sector_color,
        line = list(
          color = sector_color,
          width = 0.3
        ),
        text = paste0(
          data$category[i],
          "<br>Value: ",
          round(value, 2),
          "<br>Baseline: ",
          round(data$baseline[i], 2)
        ),
        hoverinfo = "text",
        showlegend = FALSE
      )
  }
  
  # ------------------------------------------------------------
  # Add the reference circle at r = 1
  # ------------------------------------------------------------
  fig <- fig %>%
    add_trace(
      type = "scatterpolar",
      r = rep(1, length(bg_theta)),
      theta = bg_theta,
      mode = "lines",
      line = list(
        color = "rgba(120, 120, 120, 0.9)",
        dash = "dot",
        width = 1
      ),
      hoverinfo = "skip",
      showlegend = FALSE
    )
  
  # ------------------------------------------------------------
  # Add baseline lines for each sector
  # ------------------------------------------------------------
  for (i in 1:n) {
    
    baseline_theta <- seq(
      begin_thetas[i],
      begin_thetas[i] + 360 / n,
      length.out = 120
    )
    
    baseline_r <- rep(data$baseline[i], length(baseline_theta))
    
    fig <- fig %>%
      add_trace(
        type = "scatterpolar",
        r = baseline_r,
        theta = baseline_theta,
        mode = "lines",
        line = list(
          color = "black",
          dash = "dot",
          width = 1
        ),
        hoverinfo = "skip",
        showlegend = FALSE
      )
  }
  
  # ------------------------------------------------------------
  # Add additional radial grid circles
  # ------------------------------------------------------------
  if (show_safety_grid) {
    
    radial_grid <- seq(min_val, max_val, by = interval)
    
    for (r_val in radial_grid) {
      
      fig <- fig %>%
        add_trace(
          type = "scatterpolar",
          r = rep(r_val, length(bg_theta)),
          theta = bg_theta,
          mode = "lines",
          line = list(
            color = "#E3E4E6",
            width = 0.35
          ),
          hoverinfo = "skip",
          showlegend = FALSE
        )
    }
  }
  
  # ------------------------------------------------------------
  # Add thick radial boundary lines between sectors
  # ------------------------------------------------------------
  for (i in seq_along(begin_thetas)) {
    
    theta_val <- begin_thetas[i]
    
    # Split each radial boundary line into small segments
    # to create a gradually thicker line from center to outside
    r_segments <- seq(min_val, max_val + 0.08, length.out = 32)
    line_widths <- seq(1.0, 11.5, length.out = length(r_segments))
    
    for (j in 2:length(r_segments)) {
      
      fig <- fig %>%
        add_trace(
          type = "scatterpolar",
          r = c(r_segments[j - 1], r_segments[j]),
          theta = c(theta_val, theta_val),
          mode = "lines",
          line = list(
            color = "black",
            width = line_widths[j]
          ),
          hoverinfo = "skip",
          showlegend = FALSE
        )
    }
  }
  
  # ------------------------------------------------------------
  # Add category labels around the outer circle
  # ------------------------------------------------------------
  if (show_category_labels) {
    
    label_r <- rep(max_val + 0.08, n)
    
    fig <- fig %>%
      add_trace(
        type = "scatterpolar",
        r = label_r,
        theta = mid_thetas,
        mode = "text",
        text = data$category,
        textfont = list(
          size = 14,
          color = "black",
          family = "Arial"
        ),
        textposition = "middle center",
        hoverinfo = "skip",
        showlegend = FALSE
      )
  }
  
  # Return the updated figure
  return(fig)
}

# Initialize plotly object
fig <- plot_ly()

# Add the polar sector plot
fig <- add_pb_subplot(
  data = data,
  column = "value",
  fig = fig,
  max_val = 1.2,
  min_val = 0.6,
  interval = 0.2,
  show_safety_grid = TRUE,
  show_category_labels = TRUE
)

# ------------------------------------------------------------
# Update figure layout
# ------------------------------------------------------------
fig <- fig %>%
  layout(
    polar = list(
      domain = list(
        x = c(0, 1),
        y = c(0, 1)
      ),
      angularaxis = list(
        rotation = 180,
        direction = "clockwise",
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        ticks = ""
      ),
      radialaxis = list(
        range = c(0.6, 1.32),
        tickvals = seq(0.6, 1.2, by = 0.2),
        ticktext = c("0.6", "0.8", "1", "1.2"),
        angle = 0,
        visible = TRUE,
        showline = FALSE,
        tickfont = list(
          size = 12,
          color = "black"
        ),
        gridcolor = "#E3E4E6",
        gridwidth = 0.7
      )
    ),
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    showlegend = FALSE,
    margin = list(
      l = 30,
      r = 30,
      t = 30,
      b = 30
    )
  ) %>%
  config(
    displayModeBar = FALSE,
    responsive = TRUE
  )

# Display the figure
fig

# Save the figure as a self-contained HTML file
# saveWidget(
#   fig,
#   "D:/your parth/.../Global.html",
#   selfcontained = TRUE
# )