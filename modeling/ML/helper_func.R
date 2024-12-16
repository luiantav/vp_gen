# Functions used in model fitting and model fit checks 


#generates a schedule for simulations 
generate_seq <- function(nTrial,nOutc, nPerc, oulevels, NLtrials,NGtrials, xs, nSamples) {
  
  trial_sequence = NA
  reward = NA
  pu_condition = NA
  ou_condition = NA
  current_n = NA
  
  trial_sequence = array(NA, dim = c(nTrial, nOutc, nPerc))
  reward = array(NA, dim = c(nTrial, nOutc, nPerc))
  pu_condition = array(NA, dim = c(nTrial, nOutc, nPerc))
  ou_condition = array(NA, dim = c(nTrial, nOutc, nPerc))
  current_n = rep(1:nTrial,6)
  
  for (pu in 1:2){
    for (ou in 1:length(oulevels)) {
      reward[,ou,pu] <- c(sample(c(rep(1,oulevels[ou]*NLtrials), rep(0, (1-oulevels[ou])*NLtrials))), rep(0,NGtrials))
      trial_sequence[,ou,pu] <- c(rep(mean, NLtrials), sample(rep(xs,times=nSamples)))
      pu_condition[,ou,pu] <- c(rep(pu, nTrial))
      ou_condition[,ou,pu] <- c(rep(ou, nTrial))
    }
  }
  
  #Reshape to single vector
  reward <- c(rbind(reward[, , 1], reward[, , 2]))
  trial_sequence <- c(rbind(trial_sequence[, , 1], trial_sequence[, , 2]))
  pu_vec <- c(rbind(pu_condition[, , 1], pu_condition[, , 2]))
  ou_vec <- c(rbind(ou_condition[, , 1], ou_condition[, , 2]))
  task <- c(rep(task, nPerc*nOutc))
  condition <- c(rep(0, 60), rep(1, 60), rep(2, 60),rep(3, 60), rep(4, 60), rep(5, 60))
  current_n = rep(1:nTrial,6)
  
  return(sequ = list(reward = reward, trial_sequence = trial_sequence, pu_vec = pu_vec,ou_vec = ou_vec, task = task, condition=condition, current_n = current_n))
}



# Likelihood 
loglik_fit_gen <- function(x, data, conf) {
  
  for (p in 1:length(conf$params2estimate)) {
    conf$params[conf$params2estimate[p]] = x[p]
  }
  
  params <- conf$params
  if (conf$model_name == 'perc_est'){
    out2 = rw_perc_est_gen(data, params, conf, pepr)
  } else if (conf$model_name == 'value_alt_offset'){
    out2 = rw_value_alt_offset_gen(data, params, conf, pepr)
  } 

  #get likelihood for relevant trials
  for (x in 1:length(out2$stimulus)) {
      sd = 0.2
      x1 = pnorm(out2$response[x]+0.01, out2$y[x], sd)
      x2 = pnorm(out2$response[x]-0.01, out2$y[x], sd)
      out2$logll[x] = log(x1-x2)
  }  

  # sum likelihood learning 
  negloglik_sum = -sum(out2$logll)
  
  return(negloglik_sum)
}


# Extract parameters from fitting dfs for further checks (used in mode_fit_checks.Rmd)
get_parameters2 <- function(df) {
  all_combi <- c("AICc_perc_estR", "AICc_value_alt_offsetR")
  
  # Helper function to update parameters in the dataframe
  update_columns <- function(prefix, indices, sub_df, cols) {
    for (i in seq_along(cols)) {
      col_name <- cols[i]
      param_name <- paste0(prefix, i)
      df[[param_name]][df$sub == sub_df$sub[1]] <- sub_df[[col_name]][indices[i]]
    }
  }
  
  # Helper function to handle a single combination
  handle_combination <- function(dftemp, ic) {
    relevant_cols <- grep(ic, names(dftemp), value = TRUE)
    df2 <- dftemp[, relevant_cols, drop = FALSE]
    
    # Compute metrics
    min_values <- apply(df2, 2, min, na.rm = TRUE)
    sum_metric <- sum(min_values, na.rm = TRUE)
    min_indices <- apply(df2, 2, which.min)
    
    if (ic == "AICc_value_alt_offsetR") {
      update_columns("LDA_value_alt_offset", min_indices, dftemp, grep("lambda_value_alt_offset", names(dftemp), value = TRUE))
      update_columns("OM_value_alt_offset", min_indices, dftemp, grep("omega_value_alt_offset", names(dftemp), value = TRUE))
      update_columns("OFF_value_alt_offset", min_indices, dftemp, grep("offset_value_alt_offset", names(dftemp), value = TRUE))
    }
  }
  
  # Process each subject and combination
  for (sub in unique(df$sub)) {
    dftemp <- subset(df, sub == sub)
    for (ic in all_combi) {
      handle_combination(dftemp, ic)
    }
  }
  
  return(df)
}



#helper function for model fit checks 
index_of_min <- function(x) {
  return(which.min(x))
}