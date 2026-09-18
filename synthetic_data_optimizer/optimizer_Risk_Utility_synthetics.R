


###### comparing different methods and different variants of those methods 
###### ( defined by their parameters)
###### for synthetic data generation

###### MAIN TYPES of METHOS:
### m1--  sequential trees (see CART or random forest) / use: synthpop
### m2-- joint probabilities (see 
  ### Bayesian mixture models / use: synMicrodata or 
  ### Bayesian Networks / use: bnlearn)
### m3-- deeep learning, i.e. adversarial NN / use: RGAN



#### do not run all file in one go: do method by method !!! --------------
#### (some, like GANs take longer time) ----------------------------------



######## m1. --------------------synthpop / CART---------------------------

# Load required libraries
library(synthpop)
library(mlr3mbo)
library(mlr3learners)
library(bbotk)
library(paradox)
library(data.table)

data(SD2011, package = "synthpop")
orig_data <- SD2011[, c("age", "sex", "income", "marital", "edu")]

# 1. Initialize a Global Tracker for your Paper
# We will use super-assignment (<<-) inside the function to append to this
benchmarking_tracker <<- data.table()

# 2. Define Quasi-Identifiers and the Target Variable for Disclosure Risk
keys <- c("age", "sex", "marital", "edu")
target_var <- "income"

# 3. Define the Evaluation Function
evaluate_synth <- function(xdt) {
  results <- data.table(pMSE = numeric(nrow(xdt)), 
                        repU = numeric(nrow(xdt)), 
                        DiSCO = numeric(nrow(xdt)))
  
  for (i in seq_len(nrow(xdt))) {
    current_minb <- xdt$minbucket[i]
    current_cp <- xdt$cp[i]
    
    syn_args <- list(
      data = orig_data, method = "cart",
      cart.minbucket = current_minb, cart.cp = current_cp, seed = 123
    )
    
    # Suppress generator output
    capture.output({ synth_out <- do.call(syn, syn_args) })
    
    # -- CALCULATE ALL UTILITY MEASURES --
     util_gen <- utility.gen(synth_out$syn, orig_data, print.stats = "all", method = "logit")
    
    # -- CALCULATE ALL RISK MEASURES --
     capture.output({
       disc <- disclosure(synth_out, orig_data, keys = keys, target = target_var)
     })
    
     # Extract risk metrics
     current_repU <- as.numeric(disc$ident[, "repU"])
     current_DiSCO <- as.numeric(disc$attrib[, "DiSCO"])
    
     # -- LOG EVERYTHING TO THE BENCHMARK TRACKER --
     new_row <- data.table(
       minbucket = current_minb,
       cp = current_cp,
       pMSE = util_gen$pMSE,
       SPECKS = util_gen$SPECKS,
       PO50 = util_gen$PO50,
       repU = current_repU,
       UiO = as.numeric(disc$ident[, "UiO"]),
       UiS = as.numeric(disc$ident[, "UiS"]),
       UiOiS = as.numeric(disc$ident[, "UiOiS"]),
    
       # change "DiO" to "Dorig" to match synthpop's output column name
       Dorig = as.numeric(disc$attrib[, "Dorig"]),
    
       DiSCO = current_DiSCO
     )
     benchmarking_tracker <<- rbindlist(list(benchmarking_tracker, new_row), fill = TRUE)


# -- RETURN ONLY THE 3 CORE OBJECTIVES TO THE OPTIMIZER --
     results$pMSE[i] <- util_gen$pMSE
     results$repU[i] <- current_repU
     results$DiSCO[i] <- current_DiSCO

     
# -- CALCULATE PROPENSITY UTILITY --
    util_gen <- utility.gen(synth_out$syn, orig_data, print.stats = "all", method = "logit")
   
     
# --  CALCULATE TABULAR UTILITY --
    # Suppress output; ngroups = 5 bins continuous variables automatically
    capture.output({
      util_tab <- utility.tab(synth_out$syn, orig_data, vars = names(orig_data), ngroups = 5)
    })
    
    
    
  }
  
  return(results)
}


# 4. Define Domain and Codomain (3 Objectives now)
domain <- ps(
  minbucket = p_int(lower = 5, upper = 50),
  cp = p_dbl(lower = 0.0001, upper = 0.05)
)

codomain <- ps(
  pMSE = p_dbl(tags = "minimize"),
  repU = p_dbl(tags = "minimize"),
  DiSCO = p_dbl(tags = "minimize")  # Added Attribute Risk
)


objective <- ObjectiveRFunDt$new(fun = evaluate_synth, domain = domain, codomain = codomain)

instance <- OptimInstanceBatchMultiCrit$new(
  objective = objective,
  terminator = trm("evals", n_evals = 30) 
)


optimizer <- opt("mbo")
optimizer$optimize(instance)


#### Visualization of the 3D-Pareto front ###----------------

library(plotly)
library(data.table)

# 1. Extract all evaluated points from the archive
all_evals <- as.data.table(instance$archive$data)

# 2. Extract the Pareto optimal points
pareto_front <- instance$archive$best()

# 3. Create the interactive 3D plot
fig <- plot_ly() %>%
  
  # Trace 1: Plot all evaluated configurations in the background (Grey dots)
  add_trace(
    data = all_evals, 
    x = ~repU, 
    y = ~DiSCO, 
    z = ~pMSE,
    type = "scatter3d", 
    mode = "markers",
    marker = list(color = "grey", size = 4, opacity = 0.4),
    name = "All Evaluations",
    # Add hover text to show the hyperparameters for each dot
    text = ~paste("minbucket:", minbucket, "<br>cp:", round(cp, 4)),
    hoverinfo = "text+x+y+z"
  ) %>%
  
  # Trace 2: Highlight the optimal Pareto points (Orange diamonds)
  add_trace(
    data = pareto_front, 
    x = ~repU, 
    y = ~DiSCO, 
    z = ~pMSE,
    type = "scatter3d", 
    mode = "markers",
    marker = list(color = "#D55E00", size = 8, symbol = "diamond", opacity = 1),
    name = "Pareto Optimal",
    text = ~paste("minbucket:", minbucket, "<br>cp:", round(cp, 4)),
    hoverinfo = "text+x+y+z"
  ) %>%
  
  # Format the layout and axes
  layout(
    title = "3D Pareto Front: Risk-Utility Trade-offs",
    scene = list(
      xaxis = list(title = "Identity Risk (repU)", autorange = "reversed"),
      yaxis = list(title = "Attribute Risk (DiSCO)", autorange = "reversed"),
      zaxis = list(title = "Utility Error (pMSE)", autorange = "reversed"),
      # Set the camera angle for a good default viewing position
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2))
    ),
    legend = list(x = 0.8, y = 0.9)
  )

# Display the interactive plot
fig

# How to Use and Interpret the Plot:
##  Interact with it: You can click and drag to rotate the 3D space, scroll to zoom in, and hover your mouse over any point to see the exact minbucket and cp values that generated it.

# The "Ideal" Corner: Because we set autorange = "reversed" for the axes, the plot is oriented so that the "best" theoretical value (0 on all three axes) is visually situated at the bottom-front corner.

# The Pareto Surface: The orange diamonds form a curved boundary. When you rotate the plot, you'll see that none of the grey dots ever cross "in front" of or "below" the orange diamonds towards the 0,0,0 point.

# Making a Decision: If you find a point on the orange surface that has an acceptable level of repU and DiSCO for your paper's specific security threshold, you can hover over it to immediately get the minbucket and cp parameters you need to generate that dataset.

## Additionally, remember you also have that benchmarking_tracker data table in your environment. 
## You can filter it using those specific hyperparameters to pull out all the other tracked metrics (UiO, VW, SPECKS, etc.) to report in your final analysis!



####### m2.a----------------synMicro: Bayesian mixture model------------------


library(synthpop)
library(mlr3mbo)
library(bbotk)
library(paradox)
library(data.table)
library(synMicrodata)

# 1. Define the Evaluation Function with explicit namespacing
evaluate_synmicro_synth <- function(xdt) {
  
  # --- DATA PREP ---
  data(SD2011, package = "synthpop")
  local_orig <- SD2011[, c("age", "sex", "income", "marital", "edu")]
  local_orig <- na.omit(local_orig)
  
  local_keys <- c("age", "sex", "marital", "edu")
  local_target <- "income"
  
  # Split continuous (Y) and categorical factors (X)
  Y_df <- local_orig[, c("age", "income"), drop = FALSE]
  Y_df$age <- as.numeric(Y_df$age)
  Y_df$income <- as.numeric(Y_df$income)
  
  X_df <- local_orig[, c("sex", "marital", "edu"), drop = FALSE]
  for (col in names(X_df)) {
    if (!is.factor(X_df[[col]])) X_df[[col]] <- as.factor(X_df[[col]])
  }
  
  # Explicitly call namespace for synMicrodata functions
  dat_obj <- synMicrodata::readData(Y_input = Y_df, X_input = X_df)
  
  results <- data.table(pMSE = numeric(nrow(xdt)), 
                        repU = numeric(nrow(xdt)), 
                        DiSCO = numeric(nrow(xdt)))
  
  for (i in seq_len(nrow(xdt))) {
    curr_max_r  <- xdt$max_r[i]
    curr_burnin <- xdt$n_burnin[i]
    
    eval_metrics <- tryCatch({
      
      # -- A. CREATE MIXTURE MODEL OBJECT --
      mod_obj <- synMicrodata::createModel(dat_obj, max_R_S_K = c(curr_max_r, 30, 15))
      
      # -- B. GENERATE SYNTHETIC DATA VIA MCMC --
      res_obj <- synMicrodata::multipleSyn(
        data_obj = dat_obj, 
        model_obj = mod_obj, 
        n_burnin = curr_burnin, 
        m = 1, 
        interval_btw_Syn = 10, 
        show_iter = FALSE
      )
      
      synth_df <- res_obj$synt_data[[1]]
      synth_df <- synth_df[, names(local_orig)]
      
      # Align factor levels back to original data definitions
      for (col in names(X_df)) {
        synth_df[[col]] <- factor(synth_df[[col]], levels = levels(local_orig[[col]]))
      }
      
      # -- C. TRICK SYNTHPOP & EVALUATE --
      mock_synds <- list(
        syn = synth_df, m = 1,
        method = rep("synMicrodata", ncol(local_orig)), names = names(local_orig)
      )
      class(mock_synds) <- "synds"
      
      util_gen <- utility.gen(synth_df, local_orig, print.stats = "all", method = "logit")
      
      capture.output({
        disc <- disclosure(mock_synds, local_orig, keys = local_keys, target = local_target)
      })
      
      list(
        pMSE = util_gen$pMSE,
        repU = as.numeric(disc$ident[, "repU"]),
        DiSCO = as.numeric(disc$attrib[, "DiSCO"]),
        status = "success"
      )
      
    }, error = function(e) {
      list(pMSE = 1.0, repU = 1.0, DiSCO = 1.0, status = paste("failed:", e$message))
    })
    
    # -- D. LOG RESULTS --
    new_row <- data.table(
      max_r = curr_max_r, n_burnin = curr_burnin,
      pMSE = eval_metrics$pMSE, repU = eval_metrics$repU,     
      DiSCO = eval_metrics$DiSCO, run_status = eval_metrics$status
    )
    try(benchmarking_tracker <<- rbindlist(list(benchmarking_tracker, new_row), fill = TRUE), silent = TRUE)
    
    results$pMSE[i] <- eval_metrics$pMSE
    results$repU[i] <- eval_metrics$repU
    results$DiSCO[i] <- eval_metrics$DiSCO
  }
  return(results)
}

# 2. Define the Domain (Search Space)
domain_syn <- ps(
  max_r    = p_int(lower = 10, upper = 40),
  n_burnin = p_int(lower = 100, upper = 800)
)
codomain <- ps(pMSE = p_dbl(tags = "minimize"), repU = p_dbl(tags = "minimize"), DiSCO = p_dbl(tags = "minimize"))

# 3. Create Objects and Run Optimizer
objective_syn <- ObjectiveRFunDt$new(fun = evaluate_synmicro_synth, domain = domain_syn, codomain = codomain)
instance_syn <- OptimInstanceBatchMultiCrit$new(objective = objective_syn, terminator = trm("evals", n_evals = 15))

optimizer <- opt("mbo")
cat("\nStarting synMicrodata Optimization...\n")
optimizer$optimize(instance_syn)

# 4. Extract Pareto Front
pareto_syn <- as.data.table(instance_syn$archive$best())

print(pareto_syn[, c("max_r", "n_burnin", "pMSE", "repU", "DiSCO")])


################# m2.b ---bnlearn: Bayesian networks ---------


library(bnlearn)
library(synthpop)
library(mlr3mbo)
library(bbotk)
library(paradox)
library(data.table)

# 1. Define the Evaluation Function
evaluate_bnlearn_synth <- function(xdt) {
  
  # --- DATA PREP ---
  # bnlearn handles factors and numerics natively, no label encoding required!
  data(SD2011, package = "synthpop")
  local_orig <- SD2011[, c("age", "sex", "income", "marital", "edu")]
  local_orig <- na.omit(local_orig) # bnlearn requires complete data
  
  local_keys <- c("age", "sex", "marital", "edu")
  local_target <- "income"
  
  results <- data.table(pMSE = numeric(nrow(xdt)), 
                        repU = numeric(nrow(xdt)), 
                        DiSCO = numeric(nrow(xdt)))
  
  for (i in seq_len(nrow(xdt))) {
    curr_k       <- xdt$penalty_k[i]
    curr_restart <- xdt$restarts[i]
    
    eval_metrics <- tryCatch({
      
      # -- A. LEARN BAYESIAN NETWORK STRUCTURE --
      # score = "bic-cg" automatically builds a Conditional Gaussian network 
      # handling both continuous and discrete demographic data.
      dag <- hc(local_orig, score = "bic-cg", k = curr_k, restart = curr_restart)
      
      # -- B. LEARN PARAMETERS --
      # Calculates the conditional probabilities and regressions for the DAG
      fitted_bn <- bn.fit(dag, local_orig)
      
      # -- C. GENERATE SYNTHETIC DATA --
      synth_df <- rbn(fitted_bn, n = nrow(local_orig))
      
      # -- D. TRICK SYNTHPOP & EVALUATE --
      mock_synds <- list(
        syn = synth_df, m = 1,
        method = rep("bnlearn", ncol(local_orig)), names = names(local_orig)
      )
      class(mock_synds) <- "synds"
      
      util_gen <- utility.gen(synth_df, local_orig, print.stats = "all", method = "logit")
      
      capture.output({
        disc <- disclosure(mock_synds, local_orig, keys = local_keys, target = local_target)
      })
      
      list(
        pMSE = util_gen$pMSE,
        repU = as.numeric(disc$ident[, "repU"]),
        DiSCO = as.numeric(disc$attrib[, "DiSCO"]),
        status = "success"
      )
      
    }, error = function(e) {
      list(pMSE = 1.0, repU = 1.0, DiSCO = 1.0, status = paste("failed:", e$message))
    })
    
    # -- E. LOG RESULTS --
    new_row <- data.table(
      penalty_k = curr_k, restarts = curr_restart,
      pMSE = eval_metrics$pMSE, repU = eval_metrics$repU,     
      DiSCO = eval_metrics$DiSCO, run_status = eval_metrics$status
    )
    try(benchmarking_tracker <<- rbindlist(list(benchmarking_tracker, new_row), fill = TRUE), silent = TRUE)
    
    results$pMSE[i] <- eval_metrics$pMSE
    results$repU[i] <- eval_metrics$repU
    results$DiSCO[i] <- eval_metrics$DiSCO
  }
  return(results)
}

# 2. Define the Domain (Search Space)
domain_bn <- ps(
  penalty_k = p_dbl(lower = 1, upper = 50),
  restarts  = p_int(lower = 0, upper = 10)
)
codomain <- ps(pMSE = p_dbl(tags = "minimize"), repU = p_dbl(tags = "minimize"), DiSCO = p_dbl(tags = "minimize"))

# 3. Create Objects and Run Optimizer
objective_bn <- ObjectiveRFunDt$new(fun = evaluate_bnlearn_synth, domain = domain_bn, codomain = codomain)
instance_bn <- OptimInstanceBatchMultiCrit$new(objective = objective_bn, terminator = trm("evals", n_evals = 30))

optimizer <- opt("mbo")
cat("\nStarting bnlearn Optimization...\n")
optimizer$optimize(instance_bn)

# 4. Print Results
cat("\nOptimization Complete. Bayesian Network Pareto Front:\n")
print(instance_bn$archive$best()[, c("penalty_k", "restarts", "pMSE", "repU", "DiSCO")])


### ... plot dags (if wanted)...



##################### comparing these first 3 methods ##########################

#In multi-objective optimization, the "gold standard" for this is 
#the Hypervolume Indicator (or S-metric). 
#It measures the total volume of the 3D space that is "dominated" by your 
#Pareto front, measured against a "worst-case" reference point. 
#The algorithm with the larger hypervolume strictly provides 
#a better set of privacy-utility trade-offs.

## We can calculate this easily using the emoa 
## (Evolutionary Multi-Objective Optimization Algorithms) package in R:

library(plotly)
library(data.table)
library(emoa)

# 1. Extract Pareto fronts from all three optimization instances
pareto_cart <- as.data.table(instance$archive$best())
pareto_bn   <- as.data.table(instance_bn$archive$best())
pareto_syn  <- as.data.table(instance_syn$archive$best())

# 2. Format into matrices for the emoa hypervolume calculator
mat_cart <- t(as.matrix(pareto_cart[, .(repU, DiSCO, pMSE)]))
mat_bn   <- t(as.matrix(pareto_bn[, .(repU, DiSCO, pMSE)]))
mat_syn  <- t(as.matrix(pareto_syn[, .(repU, DiSCO, pMSE)]))

# 3. Compute a unified global reference point across all three spaces
combined_fronts <- rbind(
  pareto_cart[, .(repU, DiSCO, pMSE)], 
  pareto_bn[, .(repU, DiSCO, pMSE)],
  pareto_syn[, .(repU, DiSCO, pMSE)]
)

true_ref_point <- c(
  max(combined_fronts$repU) + 1,
  max(combined_fronts$DiSCO) + 1,
  max(combined_fronts$pMSE) + 0.05
)

# 4. Calculate Dominated Hypervolumes
hv_cart <- dominated_hypervolume(mat_cart, ref = true_ref_point)
hv_bn   <- dominated_hypervolume(mat_bn, ref = true_ref_point)
hv_syn  <- dominated_hypervolume(mat_syn, ref = true_ref_point)

cat(sprintf("CART Hypervolume (synthpop):       %.4f\n", hv_cart))
cat(sprintf("Bayesian Network Hypervol (bnlearn): %.4f\n", hv_bn))
cat(sprintf("Bayesian Mixture Hypervol (synMicro): %.4f\n", hv_syn))

# 5. Determine the Three-Way Winner
volumes <- c(CART = hv_cart, `Bayesian Network` = hv_bn, `synMicrodata` = hv_syn)
winner <- names(which.max(volumes))
cat(sprintf("\nWinner by Hypervolume Indicator: %s\n", winner))

# 6. Comprehensive 3D Interactive Comparison Plot
fig <- plot_ly() %>%
  
  # CART (synthpop)
  add_trace(
    data = pareto_cart, x = ~repU, y = ~DiSCO, z = ~pMSE,
    type = "scatter3d", mode = "markers",
    marker = list(color = "#0072B2", size = 8, symbol = "circle"),
    name = "CART (synthpop)",
    text = ~paste("<b>CART</b><br>minbucket:", minbucket),
    hoverinfo = "text+x+y+z"
  ) %>%
  
  # Bayesian Network (bnlearn)
  add_trace(
    data = pareto_bn, x = ~repU, y = ~DiSCO, z = ~pMSE,
    type = "scatter3d", mode = "markers",
    marker = list(color = "#CC79A7", size = 8, symbol = "diamond"),
    name = "Bayesian Network (bnlearn)",
    text = ~paste("<b>BN</b><br>Penalty k:", round(penalty_k, 2)),
    hoverinfo = "text+x+y+z"
  ) %>%
  
  # Bayesian Mixture (synMicrodata)
  add_trace(
    data = pareto_syn, x = ~repU, y = ~DiSCO, z = ~pMSE,
    type = "scatter3d", mode = "markers",
    marker = list(color = "#009E73", size = 8, symbol = "square"),
    name = "Bayesian Mixture (synMicrodata)",
    text = ~paste("<b>synMicrodata</b><br>max_r:", max_r, "<br>burnin:", n_burnin),
    hoverinfo = "text+x+y+z"
  ) %>%
  
  layout(
    title = "Three-Way Benchmarking: CART vs. Bayesian Network vs. Bayesian Mixture",
    scene = list(
      xaxis = list(title = "Identity Risk (repU)", autorange = "reversed"),
      yaxis = list(title = "Attribute Risk (DiSCO)", autorange = "reversed"),
      zaxis = list(title = "Utility Error (pMSE)", autorange = "reversed"),
      camera = list(eye = list(x = 1.6, y = 1.6, z = 1.2))
    ),
    legend = list(x = 0.7, y = 0.9)
  )

fig


####### hypervolumes  #######

cat(sprintf("CART Hypervolume (synthpop):       %.4f\n", hv_cart))
cat(sprintf("Bayesian Network Hypervol (bnlearn): %.4f\n", hv_bn))
cat(sprintf("Bayesian Mixture Hypervol (synMicro): %.4f\n", hv_syn))

volumes <- c(CART = hv_cart, Bayesian_Network = hv_bn, synMicrodata = hv_syn)
winner <- names(which.max(volumes))
cat(sprintf("\nWinner by Hypervolume Indicator: %s\n", winner))

## CART          Bayesian_Network     synMicrodata 
## 0.1194312        0.3231553        0.3689790 

#################



######### m3. --------------- GANs with RGAN-package ---------------

##### it takes more time to run:
## multiple GAN synthetics and Risk-Utility evaluations

# 1. Define the Evaluation Function (which automatically handles factors)
evaluate_rgan_synth <- function(xdt) {
  
  # --- DATA PREP & LABEL ENCODING ---
  data(SD2011, package = "synthpop")
  local_orig <- SD2011[, c("age", "sex", "income", "marital", "edu")]
  local_orig <- na.omit(local_orig)
  
  local_keys <- c("age", "sex", "marital", "edu")
  local_target <- "income"
  
  # Identify factors and convert to numeric integers for the GAN
  is_factor_col <- sapply(local_orig, is.factor)
  factor_levels <- lapply(local_orig[, is_factor_col, drop = FALSE], levels)
  
  local_orig_numeric <- local_orig
  for (col in names(local_orig_numeric)) {
    if (is_factor_col[col]) {
      local_orig_numeric[[col]] <- as.numeric(local_orig_numeric[[col]])
    }
  }
  
  # Fit transformer on the purely numeric data
  local_transformer <- data_transformer$new()
  local_transformer$fit(local_orig_numeric)
  local_transformed <- local_transformer$transform(local_orig_numeric)
  # -------------
  
  results <- data.table(pMSE = numeric(nrow(xdt)), 
                        repU = numeric(nrow(xdt)), 
                        DiSCO = numeric(nrow(xdt)))
  
  for (i in seq_len(nrow(xdt))) {
    curr_epochs <- xdt$epochs[i]
    curr_batch  <- xdt$batch_size[i]
    
    eval_metrics <- tryCatch({
      
      # -- TRAIN GAN --
      # We define a specific latent dimension variable
      latent_dim <- 10 
      
      capture.output({
        gan_model <- gan_trainer(
          data = local_transformed, 
          epochs = curr_epochs, 
          batch_size = curr_batch,
          noise_dim = latent_dim  # Tell the GAN to expect 10 columns
        )
      })
      
      # -- GENERATE & DECODE --
      # We generate a noise vector with exactly 10 columns to match
      noise_vector <- torch_randn(c(nrow(local_orig), latent_dim))
      synth_raw <- expert_sample_synthetic_data(gan_model$generator, noise_vector)
      synth_df <- as.data.frame(local_transformer$inverse_transform(as.matrix(synth_raw)))
      names(synth_df) <- names(local_orig)
      
      # Decode numeric outputs back to text categories
      for (col in names(synth_df)) {
        if (is_factor_col[col]) {
          rounded <- round(synth_df[[col]])
          rounded <- pmax(1, pmin(rounded, length(factor_levels[[col]])))
          synth_df[[col]] <- factor(rounded, levels = 1:length(factor_levels[[col]]), labels = factor_levels[[col]])
        }
      }
      
      # -- EVALUATE --
      mock_synds <- list(syn = synth_df, m = 1, method = rep("RGAN", ncol(local_orig)), names = names(local_orig))
      class(mock_synds) <- "synds"
      
      util_gen <- utility.gen(synth_df, local_orig, print.stats = "all", method = "logit")
      capture.output({ disc <- disclosure(mock_synds, local_orig, keys = local_keys, target = local_target) })
      
      list(pMSE = util_gen$pMSE, repU = as.numeric(disc$ident[, "repU"]), DiSCO = as.numeric(disc$attrib[, "DiSCO"]), status = "success")
      
    }, error = function(e) {
      message(sprintf("\n[!] Run failed (Epochs: %d): %s", curr_epochs, e$message))
      list(pMSE = 1.0, repU = 1.0, DiSCO = 1.0, status = paste("failed:", e$message))
    })
    
    # -- LOGGING --
    new_row <- data.table(epochs = curr_epochs, batch_size = curr_batch, pMSE = eval_metrics$pMSE, repU = eval_metrics$repU, DiSCO = eval_metrics$DiSCO, run_status = eval_metrics$status)
    try(benchmarking_tracker <<- rbindlist(list(benchmarking_tracker, new_row), fill = TRUE), silent = TRUE)
    
    results$pMSE[i] <- eval_metrics$pMSE
    results$repU[i] <- eval_metrics$repU
    results$DiSCO[i] <- eval_metrics$DiSCO
  }
  return(results)
}

# 2. Define Domain and Codomain
domain_rgan <- ps(
  epochs     = p_int(lower = 50, upper = 150),
  batch_size = p_int(lower = 64, upper = 256)
)
codomain <- ps(pMSE = p_dbl(tags = "minimize"), repU = p_dbl(tags = "minimize"), DiSCO = p_dbl(tags = "minimize"))


# 3. Create bbotk Objects
# 1. Update the objects with the new function
objective_rgan <- ObjectiveRFunDt$new(fun = evaluate_rgan_synth, domain = domain_rgan, codomain = codomain)
instance_rgan <- OptimInstanceBatchMultiCrit$new(objective = objective_rgan, terminator = trm("evals", n_evals = 10))

# 2. Clear the tracker so we don't mix old penalty scores with the new run
benchmarking_tracker <<- data.table()

# 3. Optimize
cat("\nStarting RGAN Optimization Proof of Principle...\n")
optimizer$optimize(instance_rgan)


###### compare with the CART ( or other "best" method) - Pareto results: ##############

library(plotly)
library(data.table)

# 1. Extract the optimal Pareto points for both models
# (Rename 'instance' to whatever you saved your CART optimization under)
pareto_cart <- as.data.table(instance$archive$best())
pareto_rgan <- as.data.table(instance_rgan$archive$best())

# 2. Create the combined interactive 3D plot
fig_rgan <- plot_ly() %>%
  
  # Trace 1: CART Pareto Front (Blue Circles)
  add_trace(
    data = pareto_cart, 
    x = ~repU, 
    y = ~DiSCO, 
    z = ~pMSE,
    type = "scatter3d", 
    mode = "markers",
    marker = list(color = "#0072B2", size = 8, symbol = "circle", opacity = 0.8),
    name = "CART (synthpop)",
    text = ~paste("<b>CART</b><br>minbucket:", minbucket, "<br>cp:", round(cp, 4)),
    hoverinfo = "text+x+y+z"
  ) %>%
  
  # Trace 2: RGAN Pareto Front (Orange Diamonds)
  add_trace(
    data = pareto_rgan, 
    x = ~repU, 
    y = ~DiSCO, 
    z = ~pMSE,
    type = "scatter3d", 
    mode = "markers",
    marker = list(color = "#D55E00", size = 8, symbol = "diamond", opacity = 0.8),
    name = "RGAN (torch)",
    text = ~paste("<b>RGAN</b><br>epochs:", epochs, "<br>batch_size:", batch_size),
    hoverinfo = "text+x+y+z"
  ) %>%
  
  # 3. Format Layout and Axes
  layout(
    title = "Model Comparison: CART vs RGAN Pareto Fronts",
    scene = list(
      # Reversed axes so the "ideal" 0,0,0 state is at the front
      xaxis = list(title = "Identity Risk (repU)", autorange = "reversed"),
      yaxis = list(title = "Attribute Risk (DiSCO)", autorange = "reversed"),
      zaxis = list(title = "Utility Error (pMSE)", autorange = "reversed"),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2))
    ),
    legend = list(x = 0.8, y = 0.9)
  )

# Display the plot
fig_rgan

## NOTE: even without errors, the RGAN could give only one point,
## due to strict dominance issue. 


###############################################################
########## checking errors and how things performed: #########
# 1. See a quick tally of successes vs. failures
# cat("--- RUN STATUS SUMMARY ---\n")
print(table(benchmarking_tracker$run_status))
# 
# # 2. Filter the table to isolate only the failed runs
failed_runs <- benchmarking_tracker[run_status != "success", 
                                    .(epochs, batch_size, run_status)]
# 
# # 3. Print the diagnostics
if (nrow(failed_runs) > 0) {
  cat("\n--- UNIQUE ERROR MESSAGES ---\n")
  # View the distinct reasons for failure
  print(unique(failed_runs$run_status))
  
  cat("\n--- HYPERPARAMETERS THAT TRIGGERED FAILURES ---\n")
  # View which parameter combinations caused the crash
  print(failed_runs)
} else {
  cat("\nSuccess! All RGAN runs completed without any errors.\n")
}


unique(benchmarking_tracker$run_status)

# 1. Did they succeed or fail?
cat("Status counts:\n")
print(table(benchmarking_tracker$run_status))

# 2. If they failed, what is the NEW error message?
cat("\nUnique run statuses:\n")
print(unique(benchmarking_tracker$run_status))

# Look at the scores of the 8 successful runs
print(benchmarking_tracker[run_status == "success", .(epochs, batch_size, pMSE, repU)])

print(benchmarking_tracker[run_status == "success", .(epochs, batch_size, pMSE, repU, DiSCO)])


################# end GANs ########################

