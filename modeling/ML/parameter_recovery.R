########## Simulates data, fit models and save data for parameter recovery ##########

required_packages = c("here", "nloptr", "tibble", "optparse", "truncnorm")
invisible(lapply(required_packages, require, character.only = TRUE))
#renv::restore()
here::i_am("flag_project_root.R")

#### Source everything
source_path = file.path(here::here('modeling/ML/Models'), fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$", full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))
source(here::here('modeling', 'ML', "helper_func.R"))
source(here::here('modeling', 'ML', "Models", "all_models.R"))


#### Basic config.
iter = 30 #10

nOutc = 3
nPerc = 2
oulevels = c(0.25, 0.5, 0.75)
pu_ideal = c(0.4, 0.2)
nSamples =4
mean = 0
xmin = -4
xmax = 4
NLtrials =24
NGtrials= 36
cspos = 5
nStim = 9

xs = seq(xmin, xmax, by=((xmax - (xmin))/(nStim - 1))) #stim -4:4
s = c(1:9) #stimulus index;
nTrial = NLtrials + NGtrials
n = c(1:nTrial)
task = c(rep(0, NLtrials), rep(1, NGtrials))

om = 1 #[0,1] pick rule used 

###### POSSIBLE VALUES FOR INPUT PARAMETERS ############

# Get parameters from fitting and find quantiles 
#lambda
fittedparameters <- read.csv(paste0(here::here(), "/outputs/fitting/fittedparameters_valuemodel.csv"),header = TRUE)
quantiles_l_lin <- quantile(fittedparameters$lambda[fittedparameters$omega < 0.5], probs = c(0.05, 0.95))
quantiles_l_gauss <- quantile(fittedparameters$lambda[fittedparameters$omega >= 0.5], probs = c(0.05, 0.95))
lambda_lin <- seq(quantiles_l_lin[1], quantiles_l_lin[2], by=0.1)
lambda_gauss <- seq(quantiles_l_gauss[1], quantiles_l_gauss[2], by=0.1)

#offset
quantiles_o_lin <- quantile(fittedparameters$offset[fittedparameters$omega < 0.5], probs = c(0.05, 0.95))
quantiles_o_gauss <- quantile(fittedparameters$offset[fittedparameters$omega >= 0.5], probs = c(0.05, 0.95))
offset_lin <- seq(quantiles_o_lin[1], quantiles_o_lin[2], by=0.01)
offset_gauss <- seq(quantiles_o_gauss[1], quantiles_o_gauss[2], by=0.01)

#rho
fittedparameters <- read.csv(paste0(here::here(), "/outputs/fitting/fittedparameters_perceptualmodel.csv"),header = TRUE)
quantiles_r <- quantile(fittedparameters$rho, probs = c(0.05, 0.95))
rho <- seq(quantiles_r[1], quantiles_r[2], by=0.01)


# Set up for saving 
df_template <- data.frame(matrix(,nrow=iter))
rownames(df_template) <- seq(1:iter)
df <- data.frame()



#Prep cluster 
run_on_cluster = 0
if (run_on_cluster == 1) {
  # Create options to pass to script
  option_list = list(
    optparse::make_option(c('-s', '--subject'), type='character', default = NULL, help = 'sub', metavar = 'subject'))
  
  # provide options in list to be callable by script
  opt_parser = optparse::OptionParser(option_list = option_list)
  opt = optparse::parse_args(opt_parser)
  subject <- opt$subject
  
}else{
  subject = c("1","2","3","4")
}


for (sub in c(subject)) {

  #generate trial sequence for "participant"
  sequ = generate_seq(nTrial,nOutc, nPerc, oulevels, NLtrials,NGtrials, xs, nSamples)
  
  #simulate accuracies i.e. pu levels x ppt. 
  pulevels = c(rnorm(n = 1, pu_ideal[1], 0.05), rnorm(n = 1, pu_ideal[2], 0.05))
  
  #Generate pepr (pre-computed probabilities for perceptual mixing based on real/sim accuracies)
  #pepr = generate_pepr(nPerc,nStim, pulevels)
  
  df_sim = data.frame(task = sequ$task, response = c(rep(NA,360)), stimulus = sequ$trial_sequence, mix = c(rep(NA,360)), rr = sequ$ou_vec, pu = sequ$pu_vec, sub = c(rep(sub,360)), 
                      lambda = c(rep(NA,360)), omega = c(rep(NA,360)), alpha = c(rep(NA,360)), alpha_extinction = c(rep(NA,360)),rho = c(rep(NA,360)), n = sequ$current_n, reward = sequ$reward, logll = c(rep(NA,360)), 
                      condition = sequ$condition, subject = sub, total_n = sequ$current_n)
  
  df_temp <- df_template
  df_temp$sub <- sub
  
  
  # Define models to simulate
  models = c('value_alt_offset','perc_est')
  
  for (gm in models){

    for (cond in c(0:5)){ #loop over conditions
      df_sub = df_sim[df_sim$subject %in% sub & df_sim$condition %in% cond,] #get subject data
      df_sub = df_sub[df_sub$task %in% 1,] # get generalisation trials
      df_sub$total_n = c(1: nrow(df_sub))
      df_sub$current_n = c(1: nrow(df_sub))
      rrr = unique(df_sub$rr)
      ppp = unique(df_sub$pu)

      #simulate learned values
      df_learn_info = oulevels[unique(df_sub$rr)]
      mod_gen = gm
      conf = list('model_name'= mod_gen,'nStim' =nStim, 'cspos' = 5,'get_loglik' = 0,'nStimNC'= nStim*2)
      
      ## pick parameters for generating data
      omega_pick = sample(c(0,1),1)
        if (mod_gen == "perc_est"){
            conf$params2estimate <- c("rho")
            params = list('rho' = sample(rho,1))
          
        } else if(mod_gen == "value_alt_offset"){
            conf$params2estimate <- c("omega","lambda","offset")
            if(omega_pick == 1){ params = list('omega'=omega_pick, 'lambda'=sample(lambda_gauss,1),'offset'= sample(offset_gauss ,1))
            } else{ params = list('omega'=omega_pick, 'lambda'=sample(lambda_lin,1),'offset'=sample(offset_lin ,1))
          }
        } 
        
      
      data = list('nSubj' =length(unique(df_sim$sub)), 'sub' = sub, 'reward' = df_sub$reward, 'trial_sequence' = df_sub$stimulus, 'pu_vec' = df_sub$pu, 'ou_vec' = df_sub$ou, 'current_n' = df_sub$n, 'pepr' = NA, 'xs' = xs,
                  'task' = sequ$task, response = df_sub$response, 'total_n' = df_sub$n, 'learn_info' = df_learn_info)
      
      
      # Simulate data with defined models 
      if(conf$model_name == 'perc_est'){
        gen_sim = rw_perc_est_gen(data, params, conf, pepr)
      } else if(conf$model_name == 'value_alt_offset'){
        gen_sim = rw_value_alt_offset_gen(data, params, conf, pepr)
      } 
      
      generated_data <- gen_sim
      generated_data$params <- params
      
      # Prepare for saving 
      if (mod_gen == "perc_est"){
        df_temp[[paste(c("rho_", mod_gen, "_inR", rrr, "P", ppp), collapse = "")]] <- params$rho
      } else if (mod_gen == "value_alt_offset"){
        df_temp[[paste(c("omega_", mod_gen, "_inR", rrr, "P", ppp), collapse = "")]] <- params$omega
        df_temp[[paste(c("lambda_", mod_gen, "_inR", rrr, "P", ppp), collapse = "")]] <- params$lambda
        df_temp[[paste(c("offset_", mod_gen, "_inR", rrr, "P", ppp), collapse = "")]] <- params$offset
      } 
      
      
      # Fit models to simulated data to recovery parameters
      df_sub$current_n = c(1: nrow(df_sub))
      data = list('nSubj' =length(unique(df_sim$sub)), 'sub' = sub, 'reward' = df_sub$reward, 'trial_sequence' = df_sub$stimulus, 'pu_vec' = df_sub$pu, 'ou_vec' = df_sub$rr, 
                  'current_n' = df_sub$n, 'pepr' = NA, 'xs' = xs,
                  'task' = sequ$task, response = gen_sim$y, 'total_n' = df_sub$n, 'condition' = df_sub$condition, 'learn_info' = df_learn_info)
      
      mod_gen = gm
      conf = list('model_name'= mod_gen,'nStim' =nStim, 'cspos' = 5,'get_loglik' = 1,'nStimNC'= nStim*2)
      
      if(mod_gen == "perc_est"){
        conf$params2estimate <- c("rho")
        conf$params = list('rho'=NA)
      } else if(mod_gen == "value_alt_offset"){
        conf$params2estimate <- c("omega","lambda","offset")
        conf$params = list('omega'=NA,'lambda'=NA,'offset'=NA)
      } 
      
      
      for (it in seq(iter)) {
        conf$start_vals <- NA
        opts1 = list('algorithm'='NLOPT_GN_DIRECT_L', 'xtol_rel'=1.0e-4, 'maxeval'= 5000) #5000

        if(mod_gen == "perc_est"){lb = c(0); ub = c(1); conf$start_vals <- c(0.3)
        } else if(mod_gen == "value_alt_offset") {lb = c(0,0,-1); ub = c(1,10,1); conf$start_vals <- c(0.5,2,0)
        } 
        
        fit_stat2 = nloptr::nloptr(x0=conf$start_vals,
                                   # Minimize neg LL
                                   eval_f=loglik_fit_gen,
                                   # Lower bound of parameters (e.g. c(0,0,1))
                                   lb= lb, #c(0, 0, 0),
                                   # Upper bound of parameters (e.g. c(1,1,10))
                                   ub= ub, #c(1,15,1),
                                   # Minimizer options
                                   opts=opts1,
                                   # Inputs to LL function
                                   data=data, 
                                   conf=conf)
        
        
        # Prepare data for saving 
        if(mod_gen == "perc_est"){
          param_names <- c("rho") 
        }else if(mod_gen == "value_alt_offset"){
          param_names <- c("omega", "lambda","offset") 
        }
        
        params<- fit_stat2$solution
        names(params) <- param_names
        params <- as.list(params)
        
        # Calculate fit measures
        fit_stat2$ll <- loglik_fit_gen(fit_stat2$solution, data, conf)
        fit_stat2$AIC <- 2*length(conf$params2estimate) + 2*fit_stat2$ll
        k = length(conf$params2estimate)
        n = length(data$trial_sequence)
        fit_stat2$AICc <- fit_stat2$AIC + (2*(k^2) + 2*k) / (n - k - 1)
        fit_stat2$BIC <- 2*fit_stat2$ll + log(length(data$trial_sequence))*length(conf$params2estimate)
        
        df_temp$iter[it] = it
        if(mod_gen == "perc_est"){
          df_temp[it, paste(c("rho_", mod_gen, "_outR", rrr, "P", ppp), collapse = "")] <- params$rho
        } else if (mod_gen == "value_alt_offset"){
          df_temp[it, paste(c("offset_", mod_gen, "_outR", rrr, "P", ppp), collapse = "")] <- params$offset
          df_temp[it, paste(c("omega_", mod_gen, "_outR", rrr, "P", ppp), collapse = "")] <- params$omega
          df_temp[it, paste(c("lambda_", mod_gen, "_outR", rrr, "P", ppp), collapse = "")] <- params$lambda
        }
        
        df_temp[it,paste(c("ll_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fit_stat2$ll
        df_temp[it,paste(c("AIC_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fit_stat2$AIC
        df_temp[it,paste(c("AICc_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fit_stat2$AICc
        df_temp[it,paste(c("BIC_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fit_stat2$BIC
      }
      
    }
  }
  df<- rbind(df, df_temp)
  #write.csv(df, here::here('outputs','param_recovery', 'JOINTRULES', paste0("simfits_", toString(sub),"full_set.csv")))
}
#write.csv(df, here::here('outputs','param_recovery','JOINTRULES', paste0("simfits_", toString(sub),"full_set.csv")))

