# Function to generate basic biological and fishery information for MSE
generate_basic_info <- function(n_stocks = 2, 
                                n_regions = 2, 
                                n_indices = 2, 
                                n_fleets = 2, 
                                n_seasons = 4, 
                                base.years = 1973:2022,
                                n_feedback_years = 0,
                                life_history = "medium",
                                n_ages = 12, 
                                recruit_model = 2, 
                                q = 0.2, 
                                F.config = 2,
                                F.year1 = 0.3958467,
                                Fhist = "Fmsy-H-L",
                                Fbar_ages = (n_ages-2):n_ages, 
                                catch_cv = 0.1, 
                                catch_Neff = 200, 
                                index_cv = 0.2, 
                                index_Neff = 100,
                                fracyr_indices = 0.5, 
                                fracyr_spawn = 0.5,
                                bias.correct.process = FALSE,
                                bias.correct.observation = FALSE,
                                bias.correct.BRPs = FALSE
                                #XSPR.R.opt = 4
                                ) {
  
  check_dimensions <- function(...){
    length(unique(c(...))) == 1
  }
  
  if (!check_dimensions(n_stocks,n_regions,n_indices,n_fleets)) cat("\nn_stocks, n_regions, n_fleets, n_indices are not the same!")
  
  basic_info = list()
  
  basic_info$bias_correct_process = bias.correct.process
  basic_info$bias_correct_observation = bias.correct.observation
  basic_info$bias_correct_BRPs = bias.correct.BRPs
  #basic_info$XSPR_R_opt = XSPR.R.opt
  basic_info$mig_type = 0 # 0:mortality and movement separate,  1:mortality and movement simultaneous (no way to calculate brps!)
  
  basic_info$years = as.integer(base.years[1] - 1 + 1:(length(base.years) + n_feedback_years))
  basic_info$ages = as.integer(1:n_ages)
  basic_info$n_ages = as.integer(length(basic_info$ages))
  
  na = n_ages
  ny = length(basic_info$years)
  
  basic_info$recruit_model = recruit_model
  
  basic_info$n_stocks  = as.integer(n_stocks)
  basic_info$n_regions = as.integer(n_regions)
  basic_info$n_indices = as.integer(n_indices)
  basic_info$n_fleets  = as.integer(n_fleets)
  basic_info$n_seasons = as.integer(n_seasons)
  
  basic_info$fracyr_seasons = rep(1/n_seasons,n_seasons) # Can be others/ User can define this I think will be more appropreate
  
  basic_info$fracyr_SSB <- matrix(rep(0, ny), ny, n_stocks) # Do you define ny yet? maybe ny = length(basic_info$years) # Here 0 means the fish is recruited at the beigining of the year
  basic_info$fracyr_spawn = rep(fracyr_spawn,n_stocks)
  basic_info$spawn_regions = rep(1:n_stocks)
  
  basic_info$fracyr_indices = matrix(NA, length(basic_info$years), n_indices)
  for (s in 1:n_stocks) basic_info$fracyr_indices[,s] = fracyr_indices # temporarily assume 1 survey in each region/stock and same survey time
  
  basic_info$q <- rep(q, n_indices)
  
  # MAA
  maturity <- Generate_Maturity(life_history = life_history, na)
  maturity <- t(matrix(maturity, na, ny))
  basic_info$maturity <- array(NA, dim = c(n_stocks, ny, na))
  for (i in 1:n_stocks) basic_info$maturity[i,,] <- maturity
  
  # WAA
  W <- Generate_WAA(life_history = life_history, na)
  basic_info$waa <- array(NA,dim = c(n_fleets + n_regions + n_indices + n_stocks, ny, na))
  nwaa <- n_fleets + n_regions + n_indices + n_stocks
  basic_info$waa <- array(NA, dim = c(nwaa, ny, na))
  for(i in 1:nwaa) basic_info$waa[i,,] <- t(matrix(W, na, ny))
  
  # It seesms if you don't define the waa_pointer_totcatch,waa_pointer_indices,waa_pointer_ssb,waa_pointer_M. Then the first element will be used
  basic_info$waa_pointer_fleets   <- 1:n_fleets
  basic_info$waa_pointer_totcatch <- (n_fleets+1):(n_fleets+n_regions)
  basic_info$waa_pointer_indices  <- (n_fleets + n_regions + 1):(n_fleets + n_regions + n_indices)
  basic_info$waa_pointer_ssb      <- (n_fleets + n_regions + n_indices + 1):(n_fleets + n_regions + n_indices + n_stocks)
  basic_info$waa_pointer_M        <- basic_info$waa_pointer_ssb # Not sure what this means
  
  basic_info$Fbar_ages = Fbar_ages
  
  # Catch_info
  
  basic_info$agg_catch = basic_info$agg_catch_sigma = basic_info$catch_Neff = matrix(NA, ny, n_fleets)
  basic_info$use_catch_paa = basic_info$use_agg_catch = matrix(NA, ny, n_fleets)
  basic_info$selblock_pointer_fleets = matrix(NA, ny, n_fleets)
  
  basic_info$catch_paa = array(NA, dim = c(n_fleets, ny, na))
  basic_info$agg_catch[] = 1 # not sure if we can delete this, worth a try
  basic_info$catch_paa[] = 1/na # User can define OR maybe can delete?
  basic_info$catch_cv = catch_cv
  basic_info$agg_catch_sigma[] = sqrt((log(catch_cv^2 + 1)))
  basic_info$catch_Neff[] = catch_Neff
  basic_info$use_catch_paa[] = 1 # keep this first?
  basic_info$use_agg_catch[] = 1 # keep this first?
  basic_info$selblock_pointer_fleets = t(matrix(1:n_fleets, n_fleets, ny))
  
  # Indices_info
  basic_info$agg_indices = basic_info$agg_index_sigma = basic_info$index_Neff = matrix(NA, length(basic_info$years), n_indices)
  basic_info$index_paa = array(NA, dim = c(n_indices, ny, na))
  basic_info$use_indices = basic_info$use_index_paa = matrix(NA, ny, n_indices)
  basic_info$units_indices = basic_info$units_index_paa = rep(NA,n_indices)
  basic_info$index_seasons = rep(NA,n_indices)
  #basic_info$index_regions = 1:n_indices # e.g. if 2 surveys for each region, then should be "1,1,2,2"

  if(n_regions == 1) basic_info$index_regions = rep(1,n_indices)
  if(n_regions > 1) {
    basic_info$index_regions = NULL
    for (r in 1:n_regions) {
      basic_info$index_regions = c(basic_info$index_regions,rep(r,n_indices/n_regions))
    }
  }
  
  #basic_info$fleet_regions = 1:n_fleets
  if(n_regions == 1) basic_info$fleet_regions = rep(1,n_fleets)
  if(n_regions > 1) {
    basic_info$fleet_regions = NULL
    for (r in 1:n_regions) {
      basic_info$fleet_regions = c(basic_info$fleet_regions,rep(r,n_fleets/n_regions))
    }
  }
  
  basic_info$index_cv = index_cv
  
  basic_info$agg_indices[] = 1
  basic_info$agg_index_sigma[] = sqrt(log(index_cv^2 + 1)) # have to double check that # Source code can be wrong
  basic_info$index_Neff[] = index_Neff
  basic_info$index_paa[] = 1/na
  basic_info$use_indices[] = 1
  basic_info$use_index_paa[] = 1
  basic_info$selblock_pointer_indices = t(matrix(n_fleets + 1:n_indices, n_indices, ny))
  basic_info$units_indices = rep(2,n_indices)
  basic_info$units_index_paa = rep(2,n_indices)
  
  # Important about survey
  for(s in 1:n_indices) {
    #for(j in 1:n_indices) { # assume only 1 index for each region now
    int_starts = cumsum(c(0,basic_info$fracyr_seasons))
    ind <- max(which(int_starts <= fracyr_indices))
    basic_info$index_seasons[s] <- ind
    basic_info$fracyr_indices[,s] = fracyr_indices - int_starts[ind] 
  }
  
  # F optional
  nby <- length(base.years)
  mid <- floor(nby/2)
  
  if (F.config == 1){
    if(Fhist == "updown") basic_info$F <- matrix(c(seq(0,0.2,length.out = mid),seq(0.2,0,length.out=nby-mid)),nby, n_fleets)
    if(Fhist == "downup") basic_info$F <- matrix(c(seq(0.2,0,length.out = mid),seq(0,0.2,length.out=nby-mid)),nby, n_fleets)
    if(Fhist == "constant") basic_info$F <- matrix(0, nby, n_fleets)
    basic_info$F[1,] = F.year1
    if(n_feedback_years>0) basic_info$F <- rbind(basic_info$F, basic_info$F[rep(nby, n_feedback_years),, drop = F]) #same F as terminal year for feedback period
  }
  
  if (F.config == 2){
    if(Fhist == "updown") basic_info$F <- matrix(F.year1 + c(seq(0,0.2,length.out = mid),seq(0.2,0,length.out=nby-mid)),nby, n_fleets)
    if(Fhist == "downup") basic_info$F <- matrix(F.year1 + c(seq(0.2,0,length.out = mid),seq(0,0.2,length.out=nby-mid)),nby, n_fleets)
    if(Fhist == "constant") basic_info$F <- matrix(F.year1, nby, n_fleets)
    if(Fhist == "Fmsy-H-L") {
      #Fmsy <- get_FxSPR_BRPS_fn(basic_info)
      Fmsy <- F.year1
      year_change <- floor(nby * 0.5)
      max_mult = 2.5; min_mult = 1
      basic_info$F <- matrix(Fmsy*min_mult,nby, n_fleets)
      basic_info$F[1:year_change,] = Fmsy*max_mult
    }
    if(n_feedback_years>0) basic_info$F <- rbind(basic_info$F, basic_info$F[rep(nby, n_feedback_years),, drop = F]) #same F as terminal year for feedback period
  }
    
  basic_info$F_config = F.config
  
  return(basic_info)
}

get_FxSPR_BRPS_fn <- function(basic_info, percent){
  if(missing(percent)) percent <- 40
  
  n_ages<- basic_info$n_ages
  n_stocks <- basic_info$n_stocks
  if (n_stocks > 2) {
    mat.age <- apply(basic_info$maturity[,1,],n_stocks,mean)
    ssb.waa <- apply(basic_info$waa[,1,],n_stocks,mean)
  } else {
    mat.age <- basic_info$maturity[,1,]
    ssb.waa <- basic_info$waa[,1,]
  }
  M.age <- rep(0.2, basic_info$n_ages)
  seltot <- set_sel(a50=5,k=1,ages=1:basic_info$n_ages)
  seltot <- seltot/max(seltot)
  spawn.time <- basic_info$fracyr_spawn[1]
  spr0 = get_SPR(F=0, M=M.age, sel=seltot, mat=mat.age, waassb=ssb.waa, fracyrssb = spawn.time)
  F.start <- 0.11  # starting guess for optimization routine to find F_SPR%
  spr.f <- function(F.start) {
    spr = get_SPR(F=F.start, M=M.age, sel=seltot, mat=mat.age, waassb=ssb.waa, fracyrssb = spawn.time)
    abs(100*spr/spr0 - percent)
  }
  opt <- nlminb(start=F.start, objective=spr.f, lower=0, upper=10)
  Fxspr <- opt$par
  return(Fxspr)
}

set_sel <- function(a50,k,ages){
  selAA <- 1/(1+exp(-(ages-a50)/k))
  return(selAA)
}
