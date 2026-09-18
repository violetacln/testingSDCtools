



# ####### using RGAN with DP option ---------------
# # 1. Install & Load Dependencies
# if(!require(synthpop)) install.packages("synthpop")
# if(!require(RGAN)) install.packages("RGAN")
# if(!require(torch)) install.packages("torch")
# 
# library(synthpop)
# library(RGAN)
# library(torch)
# 
# # 2. Extract and Isolate the 5,000 Scientific Records
# # Remove rows containing NA to avoid deep learning gradient failures
# raw_data <- na.omit(SD2011[, c("age", "sex", "income", "edu", "region")])
# 
# # 3. Handle the Mixed Types: One-Hot Encode Categorical Variables
# # model.matrix converts factors into 0/1 indicator matrices
# encoded_matrix <- model.matrix(~ age + sex + income + edu + region - 1, data = raw_data)
# encoded_df <- as.data.frame(encoded_matrix)
# 
# # 4. Standardize Data Ranges using the Transformer
# transformer <- data_transformer$new()
# transformer$fit(encoded_df)
# transformed_data <- transformer$transform(encoded_df)
# 
# # 5. Execute Private Deep Learning (Calibrated Scientific Sweet-Spot)
# dp_model <- dp_gan_trainer(
#   data = transformed_data,
#   epochs = as.integer(60),        # Kept at 60 to prevent over-depleting the privacy budget
#   epsilon = 3.0,                  # Scientifically stable privacy threshold
#   delta = 1e-5,                   # Safe bound for the ~5k population row size
#   noise_multiplier = 1.4,         # Scales private noise injection
#   max_grad_norm = 1.0             # Clips outliers to protect individual extreme incomes
# )
# 
# # 6. Sample & Decode Back to Standard Values
# synthetic_raw <- sample_synthetic_data(dp_model$generator, num_samples = nrow(raw_data))
# synthetic_decoded <- transformer$inverse_transform(synthetic_raw)
# 
# # Enforce clean boundaries on your generated one-hot indicators 
# # (Ensures columns round clean to 0 or 1)
# synthetic_decoded[] <- lapply(synthetic_decoded, function(x) {
#   if(all(x >= -0.1 & x <= 1.1)) pmin(pmax(round(x), 0), 1) else x
# })
# 
# ## check (as a researcher):
# # Evaluate structural preservation of the core scientific indicator
# cat("Real Median Income: ", median(encoded_df$income), "\n")
# cat("Synthetic Median Income: ", median(synthetic_decoded$income), "\n")
# 


######### using synthpop, CART and implementing DP: ------------------
# 
# # 1. Dependencies and Environment Preparation
# if(!require(synthpop)) install.packages("synthpop")
# if(!require(diffpriv)) install.packages("diffpriv") # For mathematical DP noise functions
# 
library(synthpop)
library(diffpriv)
# 
# # Isolate your 5,000 scientific records and handle missing values
target_cols <- c("age", "sex", "income", "edu", "region")
scientific_data <- na.omit(SD2011[, target_cols])
N <- nrow(scientific_data)
# 
# # 2. Configure the Differentially Private Budget Allocation
# # We split our total epsilon (3.0) across the 5 target scientific attributes
target_eps <- 3.0
eps_per_col <- target_eps / length(target_cols) # 0.6 epsilon per variable feature
target_delta <- 1e-5
# 
# # 3. Stepwise Differentially Private Synthesis Pipeline
# # We initialize a synthetic dataframe mirroring the original structures
synthetic_data <- scientific_data[sample(1:N, N, replace = TRUE), ]
# 
# # --- Synthesize Continuous Variables with DP (Laplace Mechanism) ---
for (col in c("age", "income")) {
  # Calculate global sensitivity for scaling noise bounds safely
  col_range <- max(scientific_data[[col]]) - min(scientific_data[[col]])
  sensitivity <- col_range / N
  
  # Inject calibrated Laplace noise to satisfy epsilon differential privacy
  laplace_noise <- VGAM::rlaplace(N, scale = sensitivity / eps_per_col)
  synthetic_data[[col]] <- pmax(min(scientific_data[[col]]), scientific_data[[col]] + laplace_noise)
}
# 
# # --- Synthesize Categorical Factors with DP (Exponential / Private Histogram Mechanism) ---
for (col in c("sex", "edu", "region")) {
  # Compute real baseline frequencies
  true_counts <- table(scientific_data[[col]])
  
  # Add Laplace noise directly to counts (Sensitivity of a histogram count is always 1)
  dp_noise <- VGAM::rlaplace(length(true_counts), scale = 1 / eps_per_col)
  private_counts <- pmax(0, true_counts + dp_noise)
  
  # Normalize private noisy counts into a safe scientific probability distribution
  dp_probabilities <- private_counts / sum(private_counts)
  
  # Resample factors cleanly using the privacy-protected probability weights
  synthetic_data[[col]] <- factor(
    sample(names(true_counts), size = N, replace = TRUE, prob = dp_probabilities),
    levels = levels(scientific_data[[col]])
  )
}
# 
# # 4. Inspect your valid, structurally accurate, and strictly DP Synthetic Data Frame
# head(synthetic_data)
# 
# 

####### risk utility across regions ###########

library(dplyr)

# 1. Compute Spatial Risk (Uniqueness Ratio per Region)
# Define an individual as "vulnerable" if they are entirely unique (k=1) in their demographics
spatial_risk <- scientific_data %>%
  group_by(region, age, sex, edu) %>%
  mutate(combo_count = n()) %>%
  ungroup() %>%
  group_by(region) %>%
  summarize(
    Total_Records = n(),
    Unique_Records = sum(combo_count == 1),
    Spatial_Risk_Score = Unique_Records / Total_Records # Proportion of high-risk individuals
  )

# 2. Compute Spatial Utility (Absolute Median Income Error per Region)
real_regional_income  <- scientific_data %>% group_by(region) %>% summarize(Real_Med = median(income))
synth_regional_income <- synthetic_data  %>% group_by(region) %>% summarize(Synth_Med = median(income))

spatial_utility <- merge(real_regional_income, synth_regional_income, by = "region") %>%
  mutate(Spatial_Utility_Error = abs(Real_Med - Synth_Med)) # Lower error = Higher utility

# 3. Merge into a Geospatial Spatial Analytics DataFrame
spatial_map_data <- merge(spatial_risk, spatial_utility, by = "region")
print(spatial_map_data)

### NOTE: scales of risk and utility measures are very different (e.g. 0.4 versus 100.2)
### so we need different scales when plotting


###### special features due to spatial correlations --------------------

# 1. Install and Load Geographic Mapping Packages
# if(!require(sf)) install.packages("sf")
# if(!require(giscoR)) install.packages("giscoR") # Fetches official Eurostat maps
# if(!require(ggplot2)) install.packages("ggplot2")

library(sf)
library(giscoR)
library(ggplot2)
library(dplyr)

# 2. Extract and Align the Voivodeship Names
# Let's inspect the exact string region labels used in the synthpop dataset
regions_in_data <- levels(scientific_data$region)

# Fetch official Eurostat spatial boundaries for Poland (NUTS2 represents Voivodeships)
poland_map <- gisco_get_nuts(
  country = "Poland", 
  nuts_level = 2, 
  resolution = "20",
  year="2013"
)

# Standardize names between Eurostat (English/Native) and SD2011 factor labels
# If names differ slightly, use a translation mapping vector or index alignment:
spatial_map_data <- 
  spatial_map_data %>%
  mutate(
    # Simple alphabetical or direct structural assignment based on your unique console output
    NAME_LATN = region 
  )

# 3. Merge Spatial Geometric Map with your Spatial Error Metric
map_merged <- poland_map %>%
  left_join(spatial_map_data, by = c("NUTS_NAME" = "region")) # Match based on your factor strings

# 4. Generate the Choropleth Spatial Plot
ggplot(data = map_merged) +
  geom_sf(aes(fill = Spatial_Utility_Error), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma", 
    name = "Income Deviation\n(Synthetic vs Real)"
  ) +
  labs(
    title = "Geospatial Distribution of Differential Privacy Error",
    subtitle = "Proof-of-Principle using SD2011 Voivodeships (Epsilon = 3.0)",
    caption = "Notice higher errors in regions with lower initial survey counts."
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


###############################################################
######## now map both the risk and the errors (utility):------------

# 1. Install & Load Advanced Layout Dependencies
if(!require(sf)) install.packages("sf")
if(!require(giscoR)) install.packages("giscoR")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")

library(sf)
library(giscoR)
library(ggplot2)
library(dplyr)
library(stringi)
# install.packages("patchwork") # if needed
library(patchwork)

# 1. Fetch official Eurostat spatial boundaries for Poland (NUTS2)
poland_map <- gisco_get_nuts(
  country = "Poland", 
  nuts_level = 2, 
  resolution = "20",
  year = "2013"
)

# 2. Standardize Eurostat names to match SD2011's ASCII format
poland_map <- poland_map %>%
  mutate(
    NUTS_NAME_CLEAN = tolower(stringi::stri_trans_general(NUTS_NAME, "Latin-ASCII"))
  )

# 3. Clean spatial_map_data names for a flawless join
spatial_map_data <- spatial_map_data %>%
  mutate(NUTS_NAME_CLEAN = tolower(as.character(region)))

# Join the metrics to the spatial geometries
map_data <- poland_map %>%
  left_join(spatial_map_data, by = "NUTS_NAME_CLEAN") %>%
  filter(!is.na(Spatial_Risk_Score)) # Drop unmapped artifacts safely

# 4. Generate Independent Maps
# Map A: Spatial Risk
p_risk <- ggplot(data = map_data) +
  geom_sf(aes(fill = Spatial_Risk_Score), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "inferno", name = "Risk Score") +
  labs(
    title = "Spat.Risk",
    subtitle = "(Prop. of Unique Rec.)"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), legend.position = "bottom")

# Map B: Spatial Utility Error
p_util <- ggplot(data = map_data) +
  geom_sf(aes(fill = Spatial_Utility_Error), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "inferno", name = "Income Dev.") +
  labs(
    title = "Spat.Util.Error",
    subtitle = "(Median Income Dev.)"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), legend.position = "bottom")

# 5. Stitch Together with Patchwork
(p_risk + p_util) + 
  plot_annotation(
    title = "The Spatial Trade-Off of Differential Privacy",
    subtitle = "Analysis of Poland's 16 Voivodeships using the synthpop SD2011 Dataset",
    caption = "Privacy Budget: \u03b5 = 3.0. Notice the visual mirror effect between risk and error.",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )


########################################################
########## show relation risk-utility etc --------------

# 1. Calculate the Linear Model Correlation
fit_spatial <- lm(Spatial_Utility_Error ~ Spatial_Risk_Score, data = spatial_map_data)
r_squared   <- summary(fit_spatial)$r.squared

# 2. Build the Scatter Plot with a Linear Trendline
ggplot(data = spatial_map_data, aes(x = Spatial_Risk_Score, y = Spatial_Utility_Error)) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#d95f02", fill = "#d95f02", alpha = 0.15) +
  geom_point(color = "#7570b3", size = 4, alpha = 0.8) +
  geom_text(aes(label = region), vjust = -1, check_overlap = TRUE, size = 3) +
  labs(
    title = "Quantifying the Spatial Trade-Off of Differential Privacy",
    subtitle = paste0("Proof-of-Principle using SD2011 Voivodeships (R² = ", round(r_squared, 3), ")"),
    x = "Spatial Risk Score (Proportion of Unique Records)",
    y = "Spatial Utility Error (Median Income Deviation)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      ##face = "bold", 
      size = 12),
    axis.title = element_text(
      ##face = "bold"
    )
  )

####**************----------------------------
#### plot but with point-sizes depending on the record-counts in region
ggplot(data = spatial_map_data, aes(x = Spatial_Risk_Score, y = Spatial_Utility_Error)) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#d95f02", fill = "#d95f02", alpha = 0.15) +
  geom_point(aes(size = Total_Records/max(Total_Records)*10 ), color = "#7570b3", alpha = 0.8) +  ### <--- re-size the points
  guides(size = "none") +
  geom_text(aes(label = region), vjust = -1, check_overlap = TRUE, size = 3) +
  labs(
    title = "Quantifying the Spatial Trade-Off of Differential Privacy",
    subtitle = paste0("Proof-of-Principle using SD2011 Voivodeships (R² = ", round(r_squared, 3), ")"),
    x = "Spatial Risk Score (Proportion of Unique Records)",
    y = "Spatial Utility Error (Median Income Deviation)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      ##face = "bold",
      size = 12),
    axis.title = element_text(
      ##face = "bold"
    )
  )

##################################################################
########### spatially adaptable DP ------correct ------------
###############################################################

# 1. Dependencies and Environment Preparation
if(!require(synthpop)) install.packages("synthpop")
library(synthpop)

# Custom robust Laplace noise generator (avoids external package dependency conflicts)
rlaplace <- function(n, scale) {
  u <- runif(n, -0.5, 0.5)
  return(-sign(u) * scale * log(1 - 2 * abs(u)))
}

# Extract your baseline records and omit missing rows
target_cols <- c("age", "sex", "income", "edu", "region")
scientific_data <- na.omit(SD2011[, target_cols])

# Ensure categorical columns are properly formatted as factors
scientific_data$sex <- as.factor(scientific_data$sex)
scientific_data$edu <- as.factor(scientific_data$edu)
scientific_data$region <- as.factor(scientific_data$region)

# 2. Compute Spatially Adaptive Budgets Across Regions
global_target_eps <- 3.0
regional_counts <- table(scientific_data$region)
mean_regional_count <- mean(regional_counts)

# Adaptive Weighting Equation: Inverse relationship with local region sizes
adaptive_weights <- mean_regional_count / as.numeric(regional_counts)
normalized_weights <- adaptive_weights / mean(adaptive_weights)
regional_epsilons <- global_target_eps * normalized_weights
names(regional_epsilons) <- names(regional_counts)

# 3. Execute Segmented Geographic Synthesis Pipeline
synthetic_data_adaptive <- scientific_data[0, ] # Initialize clean empty dataframe

for (r in names(regional_counts)) {
  # Isolate local target region partition
  region_subset <- scientific_data[scientific_data$region == r, ]
  N_r <- nrow(region_subset)
  
  # Skip empty regions to prevent division-by-zero errors
  if (N_r == 0) next 
  
  # Fetch the adapted epsilon budget for this specific region
  local_eps <- regional_epsilons[r]
  eps_per_col <- local_eps / length(target_cols)
  
  # Clone structural layout
  region_synth <- region_subset
  
  # --- Continuous Variables (Adaptive Laplace Mechanism) ---
  for (col in c("age", "income")) {
    min_val <- min(scientific_data[[col]], na.rm = TRUE)
    max_val <- max(scientific_data[[col]], na.rm = TRUE)
    col_range <- max_val - min_val
    
    # Correct sensitivity for row-level perturbation is the column range
    sensitivity <- col_range 
    
    laplace_noise <- rlaplace(N_r, scale = sensitivity / eps_per_col)
    # Clamp values within valid global data boundaries
    region_synth[[col]] <- pmin(max_val, pmax(min_val, region_subset[[col]] + laplace_noise))
  }
  
  # --- Categorical Variables (Adaptive Private Histograms) ---
  for (col in c("sex", "edu", "region")) {
    if (col == "region") {
      region_synth[[col]] <- r # Maintains structural regional boundary integrity
      next
    }
    
    # Tabulate ensuring all global factor levels are preserved
    true_counts <- table(factor(region_subset[[col]], levels = levels(scientific_data[[col]])))
    dp_noise <- rlaplace(length(true_counts), scale = 1 / eps_per_col)
    private_counts <- pmax(0, true_counts + dp_noise)
    
    sum_counts <- sum(private_counts)
    if (sum_counts == 0) {
      dp_probabilities <- rep(1 / length(true_counts), length(true_counts))
    } else {
      dp_probabilities <- private_counts / sum_counts
    }
    
    region_synth[[col]] <- factor(
      sample(names(true_counts), size = N_r, replace = TRUE, prob = dp_probabilities),
      levels = levels(scientific_data[[col]])
    )
  }
  
  # Merge sub-region outputs back into the unified dataset
  synthetic_data_adaptive <- rbind(synthetic_data_adaptive, region_synth)
}

# Convert region back to factor matching original layout
synthetic_data_adaptive$region <- factor(synthetic_data_adaptive$region, levels = levels(scientific_data$region))


## checks ---------------------------
# 1. Compare Univariate Distributions (Histograms and Tables)
# This generates side-by-side comparisons for all target columns

## different but useful facets of the comparison:

# Pass positionally
compare(synthetic_data_adaptive, scientific_data, vars = target_cols)

# Or explicitly using the correct parameter names ('object' and 'data')
compare(object = synthetic_data_adaptive, data = scientific_data, vars = target_cols)


# 2. Compare Bivariate Relationships (e.g., Income vs. Age by Region)
# Check if conditional relationships are preserved after adding Laplace noise
utility_tab_orig <- xtabs(~ region + edu, data = scientific_data)
utility_tab_synth <- xtabs(~ region + edu, data = synthetic_data_adaptive)

print("Original Contingency Table (Region vs. Education):")
print(utility_tab_orig)

print("Synthetic Contingency Table (Region vs. Education):")
print(utility_tab_synth)

# 3. Structural Utility Check: Mean income and age by region
aggregate(cbind(age, income) ~ region, data = scientific_data, FUN = mean)
aggregate(cbind(age, income) ~ region, data = synthetic_data_adaptive, FUN = mean)


###### mapping risk - utility ------------------------------
library(dplyr)

# 1. Summarize original data by region
orig_summary <- scientific_data %>%
  group_by(region) %>%
  summarise(
    N_original = n(),
    orig_mean_income = mean(income, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Summarize synthetic data by region ---------*------------
synth_summary <- synthetic_data_adaptive %>%
  group_by(region) %>%
  summarise(
    synth_mean_income = mean(income, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Combine and compute metrics cleanly
regional_eval <- orig_summary %>%
  left_join(synth_summary, by = "region") %>%
  mutate(
    income_mae = abs(orig_mean_income - synth_mean_income),
    assigned_eps = regional_epsilons[as.character(region)],
    risk_proxy = 1 / assigned_eps
  )

#### plotting -------------------
# 1. Install giscoR and tidyr if missing
if(!require(giscoR)) install.packages("giscoR")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringi)) install.packages("stringi")

library(giscoR)
library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)

# 2. Extract Shapefiles matching the 16 Voivodeships of Poland
# Crucial: year = "2016" ensures we get exactly 16 regions to match SD2011
poland_map <- gisco_get_nuts(
  country = "Poland", 
  nuts_level = 2, 
  resolution = "20",
  year = "2013" 
)

# Standardize Eurostat names to match the ASCII format in SD2011
poland_map <- poland_map %>%
  mutate(
    # Strips diacritics and converts to lowercase (e.g., "Śląskie" -> "slaskie")
    NUTS_NAME_CLEAN = tolower(stringi::stri_trans_general(NUTS_NAME, "Latin-ASCII"))
  )

# 3. Restructure Spatial Metrics into a Long Format for Faceting
# Map our previous 'regional_eval' columns to your requested names
spatial_map_data <- regional_eval %>%
  rename(
    NUTS_NAME = region,
    Spatial_Risk_Score = risk_proxy,
    Spatial_Utility_Error = income_mae
  ) %>%
  mutate(NUTS_NAME_CLEAN = tolower(as.character(NUTS_NAME)))

# Pivot metrics into long format to map them dynamically side-by-side
spatial_long <- spatial_map_data %>%
  select(NUTS_NAME_CLEAN, Spatial_Risk_Score, Spatial_Utility_Error) %>%
  pivot_longer(
    cols = c(Spatial_Risk_Score, Spatial_Utility_Error),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = recode(Metric,
                    "Spatial_Risk_Score" = "Spatial Risk (Epsilon Proxy)",
                    "Spatial_Utility_Error" = "Spatial Utility Error (Income MAE)"
    )
  )

# Join the pivoted metrics with the spatial geometry polygons
map_facets <- poland_map %>%
  left_join(spatial_long, by = "NUTS_NAME_CLEAN") %>%
  filter(!is.na(Metric)) # Exclude unmapped rows

# 4. Plot faceted maps
# ggplot(map_facets) +
#   geom_sf(aes(fill = Value), color = "white", size = 0.2) +
#   facet_wrap(~ Metric) +
#   scale_fill_viridis_c(option = "magma") +
#   theme_void() +
#   theme(
#     strip.text = element_text(size = 12, face = "bold"),
#     legend.position = "bottom"
#   ) +
#   labs(fill = "Score")

library(dplyr)

## or -------

# 1. Group by the metric type and apply min-max scaling
map_facets_scaled <- map_facets %>%
  group_by(Metric) %>%
  mutate(
    # Scale values to a 0-1 range for shared color mapping
    Value_Scaled = (Value - min(Value, na.rm = TRUE)) / 
      (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE))
  ) %>%
  ungroup()

# 2. Plot using the scaled values
ggplot(map_facets_scaled) +
  geom_sf(aes(fill = Value_Scaled), color = "white", size = 0.2) +
  facet_wrap(~ Metric) +
  scale_fill_viridis_c(option = "magma", name = "Relative Intensity (0 to 1)") +
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )


#####################################################
######### compare with CART #######################

library(synthpop)
library(dplyr)

# 1. Synthesize using CART (synthpop's default method)
# We set a seed for reproducibility
cart_model <- syn(scientific_data, method = "cart", seed = 2026)
synthetic_data_cart <- cart_model$syn

# 2. Calculate Spatial Utility (Income MAE)
orig_income <- scientific_data %>%
  group_by(region) %>%
  summarise(orig_mean = mean(income, na.rm = TRUE), .groups = "drop")

cart_income <- synthetic_data_cart %>%
  group_by(region) %>%
  summarise(cart_mean = mean(income, na.rm = TRUE), .groups = "drop")

# 3. Calculate Spatial Risk (Proportion of Unique Records)
# We group by all target columns to find combinations that appear exactly once
cart_risk <- synthetic_data_cart %>%
  group_by(region, age, sex, edu, income) %>%
  mutate(record_count = n()) %>%
  ungroup() %>%
  group_by(region) %>%
  summarise(
    # Calculate the proportion of records in each region that are unique
    Spatial_Risk_Score = sum(record_count == 1) / n(),
    .groups = "drop"
  )

# 4. Combine into the Standardized Map Format
spatial_map_data_cart <- orig_income %>%
  left_join(cart_income, by = "region") %>%
  left_join(cart_risk, by = "region") %>%
  mutate(
    Spatial_Utility_Error = abs(orig_mean - cart_mean),
    # Standardize names for the Eurostat spatial join
    NUTS_NAME_CLEAN = tolower(as.character(region))
  )


# Join CART metrics to the existing Eurostat geometries
map_data_cart <- poland_map %>%
  left_join(spatial_map_data_cart, by = "NUTS_NAME_CLEAN") %>%
  filter(!is.na(Spatial_Risk_Score)) 


# Now run Map A (Risk) and Map B (Utility) using map_data_cart:
# 4. Generate Independent Maps
# Map A: Spatial Risk
p_risk <- ggplot(data = map_data_cart) +
  geom_sf(aes(fill = Spatial_Risk_Score), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "inferno", name = "Risk Score") +
  labs(
    title = "Spatial Risk",
    subtitle = "(Prop. of Unique Rec.)"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), legend.position = "bottom")

# Map B: Spatial Utility Error
p_util <- ggplot(data = map_data_cart) +
  geom_sf(aes(fill = Spatial_Utility_Error), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "inferno", name = "Income Dev.") +
  labs(
    title = "Spatial Utility Error",
    subtitle = "(Median Income Dev.)"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), legend.position = "bottom")

# 5. Stitch Together with Patchwork
(p_risk + p_util) + 
  plot_annotation(
    title = "The Spatial Trade-Off of Differential Privacy",
    subtitle = "Analysis of Poland's 16 Voivodeships using the synthpop SD2011 Dataset",
    caption = "Privacy Budget: \u03b5 = 3.0. Notice the visual mirror effect between risk and error.",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )


#######################################################################


#### showing the uncertainty in risk-utility for maps ##############


library(synthpop)
library(dplyr)
library(tidyr)
library(purrr)

# 1. Generate 20 independent synthetic datasets using CART
multi_cart <- syn(scientific_data, method = "cart", m = 200, seed = 2026)

# 2. Combine the 20 datasets into one long dataframe with a 'run_id' column
syn_list <- multi_cart$syn
syn_long <- bind_rows(syn_list, .id = "run_id")

# 3. Calculate baseline original mean income
orig_income <- scientific_data %>%
  group_by(region) %>%
  summarise(orig_mean = mean(income, na.rm = TRUE), .groups = "drop")

# 4. Calculate Risk and Utility for EVERY region across EVERY run
iteration_metrics <- syn_long %>%
  group_by(run_id, region) %>%
  mutate(run_mean_income = mean(income, na.rm = TRUE)) %>%
  # Group by target cols to find unique records for empirical risk
  group_by(run_id, region, age, sex, edu, income) %>%
  mutate(record_count = n()) %>%
  ungroup() %>%
  # Aggregate up to the Region + Run level
  group_by(run_id, region) %>%
  summarise(
    run_income_mae = abs(first(run_mean_income) - orig_income$orig_mean[orig_income$region == first(region)]),
    run_risk_score = sum(record_count == 1) / n(),
    .groups = "drop"
  )

# 5. Aggregate to Expected Values (Mean) and Uncertainty (Standard Deviation)
uncertainty_map_data <- iteration_metrics %>%
  group_by(region) %>%
  summarise(
    Expected_Utility_Error = mean(run_income_mae),
    Uncertainty_Utility_Error = sd(run_income_mae),
    Expected_Risk = mean(run_risk_score),
    Uncertainty_Risk = sd(run_risk_score),
    .groups = "drop"
  ) %>%
  mutate(NUTS_NAME_CLEAN = tolower(as.character(region)))



### now for plotting:

# Join with your NUTS 2013 Eurostat spatial map (poland_map from previous step)
map_data_uncert <- poland_map %>%
  left_join(uncertainty_map_data, by = "NUTS_NAME_CLEAN") %>%
  filter(!is.na(Expected_Utility_Error))

# --- MAP 1: Expected Utility Error (Mean MAE) ---
p_exp_util <- ggplot(map_data_uncert) +
  geom_sf(aes(fill = Expected_Utility_Error), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "magma", name = "Mean MAE") +
  labs(title = "Expected Utility Error", subtitle = "Average Income Dev. over N runs") +
  theme_void()

# --- MAP 2: Uncertainty in Utility (SD of MAE) ---
p_sd_util <- ggplot(map_data_uncert) +
  geom_sf(aes(fill = Uncertainty_Utility_Error), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "mako", name = "SD of MAE") +
  labs(title = "Utility Uncertainty", subtitle = "Volatility of Income Dev.") +
  theme_void()

# --- MAP 3: Expected Risk (Mean Unique Proportion) ---
p_exp_risk <- ggplot(map_data_uncert) +
  geom_sf(aes(fill = Expected_Risk), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "magma", name = "Mean Risk") +
  labs(title = "Expected Disclosure Risk", subtitle = "Average Unique Records over N runs") +
  theme_void()

# --- MAP 4: Uncertainty in Risk (SD of Risk) ---
p_sd_risk <- ggplot(map_data_uncert) +
  geom_sf(aes(fill = Uncertainty_Risk), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "mako", name = "SD of Risk") +
  labs(title = "Risk Uncertainty", subtitle = "Volatility of Disclosure Risk") +
  theme_void()

# Stitch the 2x2 grid using Patchwork
(p_exp_risk | p_exp_util) / 
  (p_sd_risk  | p_sd_util) + 
  plot_annotation(
    title = "Geographic Uncertainty in Synthetic Data Generation",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )




