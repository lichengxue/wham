library(wham)
year_start  <- 2003  # starting year in the burn-in period
year_end    <- 2022  # end year in the burn-in period
MSE_years   <- 15     # number of years in the feedback loop

basic_info <- generate_basic_info(n_stocks   = 2, 
                                  n_regions  = 2, 
                                  n_indices  = 2, 
                                  n_fleets   = 2, 
                                  n_seasons  = 4, 
                                  base.years = year_start:year_end, 
                                  n_feedback_years = MSE_years, 
                                  life_history  = "medium", 
                                  n_ages        = 12, 
                                  Fbar_ages     = 12, 
                                  recruit_model = 2, 
                                  q = 0.2, 
                                  F_info     = list(F.year1 = 0.2, Fhist = "constant"), 
                                  catch_info = list(catch_cv = 0.1, catch_Neff = 200), 
                                  index_info = list(index_cv = 0.2, index_Neff = 100, fracyr_indices = 0.5), 
                                  fracyr_spawn = 0.5, 
                                  bias.correct.process     = FALSE, 
                                  bias.correct.observation = FALSE, 
                                  bias.correct.BRPs        = FALSE, 
                                  mig_type = 0) 

basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2)
# Note: default is move = 0.3 (constant) for stock1 and 0.1 (constant) for the other stocks
move <- generate_move(basic_info = basic_info, move.type = 2, move.rate = 0.3, move.re = "constant")

### 4. Configure selecitvity and natural mortality
n_stocks  <- as.integer(basic_info['n_stocks'])
n_regions <- as.integer(basic_info['n_regions'])
n_fleets  <- as.integer(basic_info['n_fleets'])
n_indices <- as.integer(basic_info['n_indices'])
n_ages    <- as.integer(basic_info['n_ages'])

# Selectivity Configuration
fleet_pars <- c(5,1)
index_pars <- c(2,1)
sel <- list(model=rep("logistic",n_fleets+n_indices),
            initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
            fix_pars=rep(list(NULL),n_fleets+n_indices))

# M Configuration
# M <- list(model="constant") # Default is M = 0.2
M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

sigma        <- "rec+1"
re_cor       <- "iid"
ini.opt      <- "equilibrium" # option   <- c("age-specific-fe", "equilibrium")
Rec_sig      <- 0.2 # (sigma for recruitment)
NAA_sig      <- 0.2 # (sigma for NAA)

# Set initial NAA for each stock
log_N1    <- rep(10,n_stocks)
log_N1[1] <- log(exp(10)*2) # Create difference between stocks
N1_pars   <- generate_ini_N1(log_N1,basic_info,ini.opt)

# Set mean recruitment para. for each stock
mean_rec_par <- list()
for (i in 1:n_stocks) mean_rec_par[[i]] <- exp(log_N1[i])

NAA_re <- list(N1_model=rep(ini.opt,n_stocks),
               sigma=rep(sigma,n_stocks),
               cor=rep(re_cor,n_stocks),
               recruit_model = 2,  # rec random around the mean
               recruit_pars = mean_rec_par, 
               sigma_vals = rep(list(c(Rec_sig,rep(NAA_sig,n_ages-1))),n_stocks),  # two sigmas when "rec+1"
               N1_pars = N1_pars)

input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)

# Global SPR is calculated based on weights of mean rec par 
input$data$SPR_weight_type <- 1
input$data$SPR_weights     <- c(2/3,1/3)
input$data$do_SPR_BRPs     <- 1

### 7. Generate the operating model

om = fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)
print(exp(om$rep$log_FXSPR_static)) # This is static F40%, now you may replace F_info$F.year1 with this value in the above generate_basic_info function if you want historic fishing mortality to be at F40% 
saveRDS(om,"om.RDS") # save the OM 

### 8. Generate datasets
data <- generate_data(om, seed = 123)

assess.interval <- 3 # Assessment interval
base.years      <- year_start:year_end # Burn-in period
first.year      <- head(base.years,1)
terminal.year   <- tail(base.years,1)
assess.years    <- seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)

mods <- list()

n_stocks = n_regions = n_fleets = n_indices = 1
sel_em <- list(model=rep("logistic",n_fleets+n_indices),
               initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
               fix_pars=rep(list(NULL),n_fleets+n_indices))
NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[1]] = loop_through_fn(om = data, 
                            M_om = M,
                            sel_om = sel, 
                            NAA_re_om = NAA_re, 
                            mean_rec_weights = c(2/3,1/3),
                            move_om = move,
                            M_em = M_em, 
                            sel_em = sel_em, 
                            NAA_re_em = NAA_re_em, 
                            move_em = NULL,
                            em.opt = list(separate.em = TRUE, separate.em.type = 1, do.move = FALSE, est.move = FALSE),
                            assess_years = assess.years, 
                            assess_interval = assess.interval, 
                            base_years = base.years,
                            year.use = 20, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)

n_stocks = n_regions = 1
n_fleets = n_indices = 2
sel_em <- list(model=rep("logistic",n_fleets+n_indices),
               initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
               fix_pars=rep(list(NULL),n_fleets+n_indices))
NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[2]] = loop_through_fn(om = data, 
                            M_om = M,
                            sel_om = sel, 
                            NAA_re_om = NAA_re, 
                            mean_rec_weights = c(2/3,1/3),
                            move_om = move,
                            M_em = M_em, 
                            sel_em = sel_em, 
                            NAA_re_em = NAA_re_em, 
                            move_em = NULL,
                            em.opt = list(separate.em = TRUE, separate.em.type = 2, do.move = FALSE, est.move = FALSE),
                            assess_years = assess.years, 
                            assess_interval = assess.interval, 
                            base_years = base.years,
                            year.use = 20, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)

n_stocks = n_regions = 1
n_fleets = n_indices = 1
sel_em <- list(model=rep("logistic",n_fleets+n_indices),
               initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
               fix_pars=rep(list(NULL),n_fleets+n_indices))
NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[3]] = loop_through_fn(om = data, 
                            M_om = M,
                            sel_om = sel, 
                            NAA_re_om = NAA_re, 
                            mean_rec_weights = c(2/3,1/3),
                            move_om = move,
                            M_em = M_em, 
                            sel_em = sel_em, 
                            NAA_re_em = NAA_re_em, 
                            move_em = NULL,
                            em.opt = list(separate.em = TRUE, separate.em.type = 3, do.move = FALSE, est.move = FALSE),
                            assess_years = assess.years, 
                            assess_interval = assess.interval, 
                            base_years = base.years,
                            year.use = 20, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)

n_stocks = n_regions = 2
n_fleets = n_indices = 2

sel_em <- list(model=rep("logistic",n_fleets+n_indices),
               initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
               fix_pars=rep(list(NULL),n_fleets+n_indices))

NAA_re_em <- list(N1_model=rep("equilibrium",n_stocks),
                  sigma=rep("rec+1",n_stocks),
                  cor=rep("iid",n_stocks))

M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[4]] = loop_through_fn(om = data, 
                            M_om = M,
                            sel_om = sel, 
                            NAA_re_om = NAA_re, 
                            mean_rec_weights = c(2/3,1/3),
                            move_om = move,
                            M_em = M_em, 
                            sel_em = sel_em, 
                            NAA_re_em = NAA_re_em, 
                            move_em = move,
                            em.opt = list(separate.em = FALSE, separate.em.type = 3, do.move = TRUE, est.move = FALSE),
                            assess_years = assess.years, 
                            assess_interval = assess.interval, 
                            base_years = base.years,
                            year.use = 20, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)

n_stocks = n_regions = 2
n_fleets = n_indices = 2

sel_em <- list(model=rep("logistic",n_fleets+n_indices),
               initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
               fix_pars=rep(list(NULL),n_fleets+n_indices))

NAA_re_em <- list(N1_model=rep("equilibrium",n_stocks),
                  sigma=rep("rec+1",n_stocks),
                  cor=rep("iid",n_stocks))

M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[5]] = loop_through_fn(om = data, 
                            M_om = M,
                            sel_om = sel, 
                            NAA_re_om = NAA_re, 
                            mean_rec_weights = c(2/3,1/3),
                            move_om = move,
                            M_em = M_em, 
                            sel_em = sel_em, 
                            NAA_re_em = NAA_re_em, 
                            move_em = NULL,
                            em.opt = list(separate.em = FALSE, separate.em.type = 3, do.move = FALSE, est.move = FALSE),
                            assess_years = assess.years, 
                            assess_interval = assess.interval, 
                            base_years = base.years,
                            year.use = 20, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)

n_stocks = n_regions = 2
n_fleets = n_indices = 2

sel_em <- list(model=rep("logistic",n_fleets+n_indices),
               initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
               fix_pars=rep(list(NULL),n_fleets+n_indices))

NAA_re_em <- list(N1_model=rep("equilibrium",n_stocks),
                  sigma=rep("rec+1",n_stocks),
                  cor=rep("iid",n_stocks))

M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[6]] = loop_through_fn(om = data, 
                            M_om = M,
                            sel_om = sel, 
                            NAA_re_om = NAA_re, 
                            mean_rec_weights = c(2/3,1/3),
                            move_om = move,
                            M_em = M_em, 
                            sel_em = sel_em, 
                            NAA_re_em = NAA_re_em, 
                            move_em = NULL,
                            em.opt = list(separate.em = FALSE, separate.em.type = 3, do.move = FALSE, est.move = FALSE),
                            assess_years = assess.years, 
                            assess_interval = assess.interval, 
                            base_years = base.years,
                            year.use = 20, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)

