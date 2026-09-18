
library(bnlearn)
library(synthpop)
library(mlr3mbo)
library(bbotk)
library(paradox)
library(data.table)
library(ggplot2)


####-----------------------------------------------------------------------------
#### using one method (BNlearn), tuning the main parameter (penalty) and then
#### investigating the effect on meaningful, for GEOSPATIAL context, risk-utility measures 


# ==============================================================================
# 1. DEFINE GEOSPATIAL EVALUATION FUNCTION
# ==============================================================================
evaluate_geo_synth <- function(xdt) {
  
  data(SD2011, package = "synthpop")
  # Include 'region' for spatial tracking
  local_orig <- na.omit(SD2011[, c("age", "sex", "income", "edu", "region")])
  local_keys <- c("age", "sex", "edu", "region")
  
  # Tracking 3 Objectives: Global pMSE, Global repU, and Regional Error
  results <- data.table(pMSE = numeric(nrow(xdt)), repU = numeric(nrow(xdt)), 
                        max_reg_mape = numeric(nrow(xdt)))
  
  for (i in seq_len(nrow(xdt))) {
    curr_k <- xdt$penalty_k[i]
    
    eval_metrics <- tryCatch({
      
      # -- A. Train & Generate --
      dag <- hc(local_orig, score = "bic-cg", k = curr_k)
      fitted_bn <- bn.fit(dag, local_orig)
      synth_df <- rbn(fitted_bn, n = nrow(local_orig))
      synth_df$region <- factor(synth_df$region, levels = levels(local_orig$region))
      
      # -- B. SPATIAL METRIC: Worst-Case Regional MAPE --
      orig_counts <- table(local_orig$region)
      synth_counts <- table(synth_df$region)
      
      # Calculate absolute percentage error per region, extract the worst one
      orig_safe <- pmax(orig_counts, 1) # Prevent division by zero
      regional_mapes <- abs(as.numeric(orig_counts - synth_counts)) / as.numeric(orig_safe)
      max_mape <- max(regional_mapes, na.rm = TRUE)
      
      # -- C. STANDARD EVALUATION --
      mock_synds <- list(syn = synth_df, m = 1, method = rep("bnlearn", ncol(local_orig)), names = names(local_orig))
      class(mock_synds) <- "synds"
      
      util <- utility.gen(synth_df, local_orig, print.stats = "all", method = "logit")
      disc <- disclosure(mock_synds, local_orig, keys = local_keys, target = "income")
      
      list(pMSE = util$pMSE, repU = as.numeric(disc$ident[, "repU"]), max_reg_mape = max_mape)
      
    }, error = function(e) list(pMSE = 1.0, repU = 1.0, max_reg_mape = 1.0))
    
    results$pMSE[i] <- eval_metrics$pMSE
    results$repU[i] <- eval_metrics$repU
    results$max_reg_mape[i] <- eval_metrics$max_reg_mape
  }
  return(results)
}

# ==============================================================================
# 2. RUN MLR3MBO OPTIMIZER
# ==============================================================================
# We search just one parameter (penalty_k) to keep the script compact
domain <- ps(penalty_k = p_dbl(lower = 1, upper = 40))

# The codomain now strictly expects 3 objectives to be minimized
codomain <- ps(pMSE = p_dbl(tags = "minimize"), 
               repU = p_dbl(tags = "minimize"), 
               max_reg_mape = p_dbl(tags = "minimize"))

objective <- ObjectiveRFunDt$new(fun = evaluate_geo_synth, domain = domain, codomain = codomain)
instance <- OptimInstanceBatchMultiCrit$new(objective = objective, terminator = trm("evals", n_evals = 15))

optimizer <- opt("mbo")
cat("\nMapping spatial landscape...\n")
optimizer$optimize(instance)

# ==============================================================================
# 3. VISUALIZE THE LANDSCAPE (Global vs. Local Trade-off)
# ==============================================================================
# Extract the raw archive (not just the Pareto front) to see all tested params
archive_dt <- as.data.table(instance$archive$data)
archive_dt <- archive_dt[pMSE < 1.0] # Filter out crashed runs if any

# Plot Global Utility vs Regional Distortion, mapped to the Hyperparameter
ggplot(archive_dt, aes(x = pMSE, y = max_reg_mape, color = penalty_k)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Spatial vs. Global Utility Landscape",
    subtitle = "How Network Regularization (Penalty K) affects Regional Distortion",
    x = "Global Tabular Error (pMSE) -> Lower is better",
    y = "Worst-Case Regional Error (MAPE) -> Lower is better",
    color = "Penalty K"
  ) +
  theme(legend.position = "right")

