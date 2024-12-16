# Script to fit models to data 

required_packages = c("here", "nloptr", "tibble", "optparse")
invisible(lapply(required_packages, require, character.only = TRUE))
#renv::restore()
here::i_am("flag_project_root.R")

##Source everything
source_path = file.path(here::here('phd_models/ML/Models'), fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$", full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))
source(here::here('modeling', 'ML', "helper_func.R"))
source(here::here('modeling', 'ML', "Models", "all_models.R"))


#### Basic config.
nSubj = 4 # n of ratings x stimulus 
iter = 30 # iterations 
nStim = 9 # n of stimuli 
nPerc = 2 # n PU levels 
pulevels = c(0.4, 0.2)
xmin = -4
xmax = 4

xs = seq(xmin, xmax, by=((xmax - (xmin))/(nStim - 1))) #stimulus space -4:4
task = c(rep(0, 24), rep(1, 36)) # trial x task 
df_template <- data.frame(matrix(,nrow=iter))
df <- data.frame()

# Read in data
df_exp<- read.csv(paste0(here::here(), "/modeling/ML/Data/df_forfitting.csv" ),header = TRUE)

# To run on cluster ... 
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
  subject = c("gnd1v5zo5x7qjad", "brsvshywlogvxcq") # example subjects 
}


for (sub in c(subject)) {

df_temp <- df_template
df_temp$sub <- sub

# Define models to fit 
models = c('perc_est','value_alt_offset')

for (gm in models){ 
for (cond in c(0:5)){ #task conditions

#get subject & generalisation data
dfs = df_exp[df_exp$subject %in% sub & df_exp$condition %in% cond,] 
df_sub = dfs[dfs$task %in% 1,]
df_sub$total_n = c(1: nrow(df_sub))
rrr = unique(df_sub$rr) # rr condition 
ppp = unique(df_sub$pu) # discriminability condition 

#get last learning rating and use as starting values for gen
df_learn_info = dfs[dfs$task %in% 0 & dfs$n_total == 24, ]
df_learn_info <- df_learn_info$response

data = list('nSubj' =length(unique(df_exp$sub)), 'sub' = sub, 'reward' = df_sub$reward, 'trial_sequence' = df_sub$stimulus, 'pu_vec' = df_sub$pu, 'ou_vec' = df_sub$ou, 
            'current_n' = df_sub$n_total, 'pepr' = NA, 'xs' = xs,
            'task' = task, response = df_sub$response, 'total_n' = df_sub$total_n, 'condition' = df_sub$condition, 'learn_info' = df_learn_info)

# Prepare for fitting 
mod_gen = gm
print(mod_gen)
conf = list('model_name'= mod_gen,'nStim' =nStim, 'cspos' = 5,'get_loglik' = 1, 'nStimNC'= nStim*2)

if(mod_gen == "perc_est"){
  conf$params2estimate <- c("rho")
  conf$params = list('rho' = NA)
} else if(mod_gen == "value_alt_offset"){
  conf$params2estimate <- c("omega","lambda","offset")
  conf$params = list('omega'=NA,'lambda'=NA, 'offset'=NA)
} 

# Fit to data 
for (it in seq(iter)) {
  
  conf$start_vals <- NA 
  opts1 = list('algorithm'='NLOPT_GN_DIRECT_L', 'xtol_rel'=1.0e-4, 'maxeval'= 5000) #check for alternatives
  
  if(mod_gen == "perc_est"){lb = c(0); ub = c(1); conf$start_vals <- c(0.3)
  } else if(mod_gen == "value_alt_offset") {lb = c(0,0,-1); ub = c(1,10,1); conf$start_vals <- c(0.5,2,0.01)
  } 
  
  fit_stat2 = nloptr::nloptr(x0=conf$start_vals,
                            # Minimize neg LL
                            eval_f=loglik_fit_gen,
                            # Lower bound of parameters (e.g. c(0,0,1)) #
                            #lb=c(0,0,0),
                            lb = lb,
                            # Upper bound of parameters (e.g. c(1,1,10)) 
                            #ub=c(1,15,1),
                            ub = ub,
                            # Minimizer options
                            opts=opts1,
                            # Inputs to LL function
                            data=data, 
                            conf=conf)
  
  
  # Prepare parameters for saving 
  if(mod_gen == "perc_est"){
    param_names <- c("rho") 
  }else if(mod_gen == "value_alt_offset"){
    param_names <- c("omega", "lambda", "offset") 
  }else if(mod_gen == "full_alt_offset_gen"){
    param_names <- c("omega","lambda", "rho","offset") 
  } else if(mod_gen == "ns2"){
    param_names <- c("vsd","basesd") 
  }

  params<- fit_stat2$solution
  names(params) <- param_names
  params <- as.list(params)
  
  # Get measures of fit 
  fit_stat2$ll <- loglik_fit_gen(fit_stat2$solution, data, conf)
  fit_stat2$AIC <- 2*length(conf$params2estimate) + 2*fit_stat2$ll
  k = length(conf$params2estimate)
  n = length(data$trial_sequence)
  fit_stat2$AICc <- fit_stat2$AIC + (2*(k^2) + 2*k) / (n - k - 1)
  fit_stat2$BIC <- 2*fit_stat2$ll + log(length(data$trial_sequence))*length(conf$params2estimate)
  
  # Save
  df_temp$iter[it] = it
  if(mod_gen == "perc_est"){
    df_temp[it, paste(c("rho_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$rho
  } else if (mod_gen == "value_alt_offset"){
    df_temp[it, paste(c("offset_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$offset
    df_temp[it, paste(c("omega_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$omega
    df_temp[it, paste(c("lambda_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$lambda
  } 
  
  df_temp[it,paste(c("ll_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fit_stat2$ll
  df_temp[it,paste(c("AIC_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fit_stat2$AIC
  df_temp[it,paste(c("AICc_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fit_stat2$AICc
  df_temp[it,paste(c("BIC_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fit_stat2$BIC
  
}
}
}
df<- rbind(df, df_temp)
#write.csv(df, here::here('outputs', 'fitting', 'ITER30', paste0("fits_", toString(sub),"full_set.csv")))
}
#write.csv(df, here::here('outputs', 'fitting', 'ITER30', paste0("fits_", toString(sub),"full_set.csv")))
