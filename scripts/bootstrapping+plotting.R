library(readxl)
library(dplyr)
library(readr)     # parse_number()
library(stringr)
library(boot)      # bootstrap
library(ggplot2)   # plotting (optional)
library(janitor)
library(tidyr)
library(forcats)
library(purrr)
library(patchwork)


# 1. Load and clean data
xls_file  <- "data/adaptation_data_subset.xlsx"
xls_sheet <- "Sheet1"

raw <- read_excel(xls_file, sheet = xls_sheet) %>%
  clean_names()  # ensures snake_case column names

# --- 2. Prepare data for all three crops ---
# Filter for Maize, Rice, and Wheat at the same time
filtered_data_all <- raw %>%
  filter(crop %in% c("Maize", "Rice", "Wheat"), adaptation_type != "Yield Potential") %>%
  mutate(
    dt = as.numeric(local_delta_t_from_2005),
    dp = as.numeric(climate_impacts_percent),
    adapt_group = case_when(
      adaptation_type == "No" ~ "No adaptation",
      adaptation_type %in% c("Cultivar", "Cultivar - Long duration", "Cultivar - Short duration") ~ "Variety",
      adaptation_type %in% c("High Fertilizer", "Medium Fertilizer", "Low Fertiliser", "Fertilizer") ~ "Fertilizer",
      adaptation_type == "Combined" ~ "Bundled",
      TRUE ~ adaptation_type
    ),
    adapt_group = factor(adapt_group),
    crop = factor(crop),
    ref_no = factor(ref_no)
  ) %>%
  filter(!adapt_group %in% c("Mulch", "Others")) %>%
  filter(!is.na(dt), !is.na(dp), !is.na(adapt_group))  %>%
  filter(!(crop == "Maize" & adapt_group == "Bundled"))

filtered_data <- filtered_data_all %>%
  group_by(crop, adapt_group) %>%
  mutate(
    q1 = quantile(dp, 0.25, na.rm = TRUE),
    q3 = quantile(dp, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower_bound = q1 - 1.5 * iqr,
    upper_bound = q3 + 1.5 * iqr
  ) %>%
  filter(dp >= lower_bound & dp <= upper_bound) %>%
  ungroup()

cat("✅ Adaptation groups used:\\n")
print(levels(droplevels(filtered_data$adapt_group)))

# --- 3. Bootstrap prediction ---
nested_data <- filtered_data %>%
  group_by(crop) %>%
  nest()

run_bootstrap_for_crop <- function(df_crop) {
  # Precompute the dt range for the control group (No adaptation)
  control_range <- df_crop %>%
    filter(adapt_group == "No adaptation") %>%
    summarise(min_dt = min(dt, na.rm = TRUE), max_dt = max(dt, na.rm = TRUE))
  
  run_bootstrap_for_group <- function(df_group) {
    if (nrow(df_group) < 10 || n_distinct(df_group$dt) < 3) return(NULL)
    
    model_formula <- dp ~ poly(dt, 2, raw = TRUE)
    boot_func <- function(d, i) {
      d2 <- d[i, ]
      if (n_distinct(d2$dt) < 3) return(rep(NA, 3))
      fit <- lm(model_formula, data = d2)
      return(coef(fit))
    }
    
    set.seed(42)
    boot_out <- boot(df_group, boot_func, R = 500, strata = df_group$ref_no)
    if (is.null(boot_out) || all(is.na(boot_out$t))) return(NULL)
    
    # Get dt range for this group
    group_range <- df_group %>%
      summarise(min_dt = min(dt, na.rm = TRUE), max_dt = max(dt, na.rm = TRUE))
    
    # Clip to overlap between control and group dt ranges
    pred_min <- max(control_range$min_dt, group_range$min_dt)
    pred_max <- min(control_range$max_dt, group_range$max_dt)
    grid_df <- data.frame(dt = seq(pred_min, pred_max, length.out = 100))
    
    X <- model.matrix(~ poly(dt, 2, raw = TRUE), data = grid_df)
    pred_mat <- X %*% t(boot_out$t)
    
    valid_boots <- apply(pred_mat, 2, function(col) all(col > -200 & col < 200))
    pred_mat <- pred_mat[, valid_boots, drop = FALSE]
    
    grid_df %>%
      mutate(
        fit = rowMeans(pred_mat, na.rm = TRUE),
        lo95 = apply(pred_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
        hi95 = apply(pred_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
      ) %>%
      mutate(ribbon_width = hi95 - lo95) %>%
      filter(ribbon_width <= 70) %>%
      select(-ribbon_width)
  }
  
  df_crop %>%
    group_by(adapt_group) %>%
    nest() %>%
    mutate(predictions = map(data, run_bootstrap_for_group)) %>%
    select(adapt_group, predictions) %>%
    unnest(cols = c(predictions))
}

# Run for all crops
prediction_results <- nested_data %>%
  mutate(predictions = map(data, run_bootstrap_for_crop)) %>%
  select(crop, predictions) %>%
  unnest(cols = c(predictions))

# --- 4. Plotting ---

adaptation_palette <- c(
  "No adaptation" = "#E69F00",  # orange
  "Fertilizer"    = "#56B4E9",  # sky blue
  "Variety"       = "#009E73",  # bluish green
  "Bundled"       = "#D55E00",  # vermillion (only appears in wheat/rice)
  "Irrigation"    = "#CC79A7",  # reddish purple
  "Planting date" = "#F0E442"   # yellow
)


create_crop_plot <- function(crop_name) {
  # Get control range for this crop
  control_range <- filtered_data %>%
    filter(crop == crop_name, adapt_group == "No adaptation") %>%
    summarise(min_dt = min(dt, na.rm = TRUE), max_dt = max(dt, na.rm = TRUE))
  
  # Only show data points within control ΔT range
  plot_data_points <- filtered_data %>%
    filter(crop == crop_name, dt >= control_range$min_dt, dt <= control_range$max_dt) %>%
    droplevels()
  
  plot_data_lines <- prediction_results %>%
    filter(crop == crop_name) %>%
    droplevels()
  
  used_adapt_levels <- unique(plot_data_lines$adapt_group)
  
  ggplot() +
    geom_jitter(data = plot_data_points, aes(x = dt, y = dp, color = adapt_group), width = 0.1, alpha = 0.4)+
    geom_ribbon(data = plot_data_lines, aes(x = dt, ymin = lo95, ymax = hi95, fill = adapt_group), alpha = 0.2) +
    geom_line(data = plot_data_lines, aes(x = dt, y = fit, color = adapt_group), linewidth = 1) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6, linetype = "dotted") +
    coord_cartesian(xlim = c(0.5, 5.5), ylim = c(-100, 100), expand = FALSE) +
    scale_color_manual(values = adaptation_palette, limits = used_adapt_levels, drop = TRUE) +
    scale_fill_manual(values = adaptation_palette, limits = used_adapt_levels, drop = TRUE) +
    guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
    labs(
      title = crop_name, # Set title to crop name
      x = if(crop_name == "Wheat") "ΔT from 2005 (°C)" else "", # X-axis label only on middle plot
      y = if(crop_name == "Maize") "Yield change (%)" else "" # Y-axis label only on first plot
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = c(0.99, 0.99),
      legend.justification = c("right", "top"),
      legend.title = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(0.5, "cm"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      axis.line = element_line(color = "black")
    )
}

successful_crops <- unique(prediction_results$crop)
plot_list <- map(successful_crops, create_crop_plot)
final_plot <- wrap_plots(plot_list, nrow = 1)
print(final_plot)

