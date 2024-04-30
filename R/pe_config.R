# Function to specify selectivity, natural mortality, NAA
pe_config <-  function(basic_info,
                       sel_re_opt  = list(fleet_pars = c(5,1), index_pars = c(2,1)),
                       M_re_opt    = list(M_model = "constant", M_mean = 0.2),
                       NAA_re_opt  = list(NAA_re_sigma = "rec+1", sigma_vals = 0.2, 
                                          NAA_re_cor = "iid", N1_model_opt = 2, rec_ratio = 2)
) {
  
  n_stocks  = basic_info$n_stocks
  n_regions = basic_info$n_regions
  n_fleets  = basic_info$n_fleets
  n_indices = basic_info$n_indices
  n_ages    = basic_info$n_ages
  
  # selectivity configuration 
  # Only logistic curve is included, no age specific selectivity yet.
  # No random effects on selectivity yet.
  fleet_pars = sel_re_opt$fleet_pars
  index_pars = sel_re_opt$index_pars
  sel <- list(model=rep("logistic",n_fleets+n_indices),initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),fix_pars=rep(list(NULL),n_fleets+n_indices))
  
  # natural mortality configuration
  M_model = M_re_opt$M_model
  M_mean    = M_re_opt$M_mean
  M <- list(model=M_model,initial_means=array(M_mean, dim = c(n_stocks,n_regions,n_ages)))
  
  # NAA configuration
  # Note option "iid-re" and "ar1-re" hasn't been test yet
  NAA_re_sigma      = NAA_re_opt$NAA_re_sigma
  #NAA_re_sigma_vals = NAA_re_opt$sigma_vals
  NAA_re_cor        = NAA_re_opt$NAA_re_cor
  N1_model_opt      = NAA_re_opt$N1_model_opt
  #rec_ratio         = NAA_re_opt$rec_ratio
  option = c("age-specific-fe", "equilibrium","iid-re", "ar1-re")[N1_model_opt]
  NAA_re <- list(N1_model=rep(option,n_stocks),sigma=rep(NAA_re_sigma,n_stocks),cor=rep(NAA_re_cor,n_stocks))
  
  # Initial Mean rec for different stocks
  # if (basic_info$recruit_model == 2) {
  #   NAA_re$recruit_pars = matrix(0, n_stocks, 2)
  #   NAA_re$recruit_pars[,1] = exp(10)
  #   if (rec_ratio > 1) NAA_re$recruit_pars[1,1] = exp(10)*rec_ratio
  # }
  
  # for (s in 1:n_stocks){
  #   NAA_re$sigma_vals[[s]] = NAA_re_sigma_vals
  # }
  # NAA_re$sigma_vals must be a list with length = number of stocks
  
  return(list(sel = sel,M = M, NAA_re = NAA_re))
}
  
# {$N1_model}{Integer vector (n_stocks) determining which way to model the initial numbers at age:
# {"age-specific-fe"}{(default) age- and region-specific fixed effects parameters}
# {"equilibrium"}{2 fixed effects parameters: an initial recruitment and an instantaneous fishing mortality rate to generate an equilibruim abundance at age.}
# {"iid-re"}{(default) age- and region-specific iid random effects parameters. 2 parameters: mean and sd for log NAA}
# {"ar1-re"}{(default) age- and region-specific random effects parameters. 3 parameters: mean and sd, and cor for log NAA}