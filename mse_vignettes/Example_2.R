# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ---------------------------- Examples of MSE --------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# main.dir = here::here()
main.dir = "where/you/save/your/wham/package"

# install.packages(file.path(main.dir,"wham"), dependencies = TRUE, repos = NULL, type = "source")
# devtools::install_local(file.path(main.dir,"wham"), dependencies = TRUE)

library(wham)
# roxygen2::roxygenize(file.path(main.dir,"wham"))

# Create a folder to save your results
folder.name = "Example_2"
sub.dir <- folder.name
if (file.exists(sub.dir)){
  
} else {
  dir.create(file.path(main.dir,sub.dir))
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ----------------------------- Prepare WHAM input ----------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

year_start  <- 2013  # starting year in the burn-in period
year_end    <- 2022  # end year in the burn-in period
MSE_years   <- 3     # number of years in the feedback loop

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
                                  index_info = list(index_cv = 0.2, index_Neff = 100, fracyr_indices = 0.5), fracyr_spawn = 0.5, 
                                  bias.correct.process     = FALSE, 
                                  bias.correct.observation = FALSE, 
                                  bias.correct.BRPs        = FALSE, 
                                  mig_type = 0) 
# see more details in ?generate_basic_info

# Specify movement type
# Note: default is "bidirectional" movement (e.g. stock 1 move to region 2 and stock 2 move to region 1)
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2)

# Configure movement random effects
# Note: default is move = 0.3 (constant) for stock1 and 0.1 (constant) for the other stocks
move <- generate_move(basic_info = basic_info, move.type = 2, move.rate = 0.3, move.re = "constant")

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

# NAA Configuration
sigma      <- "rec+1"
re_cor     <- "iid"
# option   <- c("age-specific-fe", "equilibrium","iid-re", "ar1-re")
ini.opt    <- "equilibrium"
NAA_re <- list(N1_model=rep(ini.opt,n_stocks),
               sigma=rep(sigma,n_stocks),
               cor=rep(re_cor,n_stocks))

# M Configuration
# M <- list(model="constant") # Default is M = 0.2
M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)

input$par$mean_rec_pars[1,1] <- log(exp(10)*2) # Change mean rec for first stock
input$par$log_N1[1,1,1]      <- log(exp(10)*2) # Change initial N1 for the first stock
input$par$log_NAA_sigma[]    <- log(0.2) # Change the sigma for NAA (Rec+1) to be log(0.5)

# Global SPR is calculated based on weights of mean rec par 
input$data$SPR_weight_type = 1
input$data$SPR_weights     = c(2/3,1/3)
input$data$do_SPR_BRPs     = 1

om = fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)

saveRDS(om,file.path(sub.dir,"om.RDS"))

# Generate datasets
data <- update_om_fn(om, seed = 123)
# nsim = 10
# set.seed(12345) 
# sim_input = list()
# sim_input = lapply(1:nsim, function(x) {
#   input_i = om$input
#   sim = om$simulate(complete=TRUE)
#   input_i$data = sim
#   return(input_i)
# })

# MSE information
assess.interval = 3
base.years      = year_start:year_end # Burn-in period
first.year      = head(base.years,1)
terminal.year   = tail(base.years,1)
assess.years    = seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)
mods <- list()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------------ Separate Assessment Models -------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

n_stocks = n_regions = n_fleets = n_indices = 1
sel <- list(model=rep("logistic",n_fleets+n_indices),
            initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
            fix_pars=rep(list(NULL),n_fleets+n_indices))
NAA_re <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[1]] = loop_through_fn2(om = data, 
                             M_em = M, 
                             sel_em = sel, 
                             NAA_re_em = NAA_re, 
                             move_em = NULL,
                             em.opt = list(separate.em = TRUE, separate.em.type = 1, do.move = FALSE, est.move = FALSE),
                             assess_years = assess.years, 
                             assess_interval = assess.interval, 
                             base_years = base.years,
                             year.use = 10, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                             seed = 123,
                             save.sdrep = TRUE)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------ One Assessment Model with fleets-as-areas ----------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

n_stocks = n_regions = 1
n_fleets = n_indices = 2
sel <- list(model=rep("logistic",n_fleets+n_indices),
            initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
            fix_pars=rep(list(NULL),n_fleets+n_indices))
NAA_re <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[2]] = loop_through_fn2(om = data, 
                             M_em = M, 
                             sel_em = sel, 
                             NAA_re_em = NAA_re, 
                             move_em = NULL,
                             em.opt = list(separate.em = TRUE, separate.em.type = 2, do.move = FALSE, est.move = FALSE),
                             assess_years = assess.years, 
                             assess_interval = assess.interval, 
                             base_years = base.years,
                             year.use = 10, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                             seed = 123,
                             save.sdrep = TRUE)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------ One Assessment Model with aggregate catch ----------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

n_stocks = n_regions = 1
n_fleets = n_indices = 1
sel <- list(model=rep("logistic",n_fleets+n_indices),
            initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
            fix_pars=rep(list(NULL),n_fleets+n_indices))
NAA_re <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[3]] = loop_through_fn2(om = data, 
                             M_em = M, 
                             sel_em = sel, 
                             NAA_re_em = NAA_re, 
                             move_em = NULL,
                             em.opt = list(separate.em = TRUE, separate.em.type = 3, do.move = FALSE, est.move = FALSE),
                             assess_years = assess.years, 
                             assess_interval = assess.interval, 
                             base_years = base.years,
                             year.use = 10, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                             seed = 123,
                             save.sdrep = TRUE)

# -----------------------------------------------------------------------------
# ------------------ Correct assessment model with move fixed -----------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

n_stocks = n_regions = 2
n_fleets = n_indices = 2

sel <- list(model=rep("logistic",n_fleets+n_indices),
            initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
            fix_pars=rep(list(NULL),n_fleets+n_indices))

sigma      <- "rec+1"
re_cor     <- "iid"
ini.opt    <- "equilibrium"
NAA_re <- list(N1_model=rep(ini.opt,n_stocks),
               sigma=rep(sigma,n_stocks),
               cor=rep(re_cor,n_stocks))

M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[4]] = loop_through_fn2(om = data, 
                             M_em = M, 
                             sel_em = sel, 
                             NAA_re_em = NAA_re, 
                             move_em = move,
                             em.opt = list(separate.em = FALSE, separate.em.type = 3, do.move = TRUE, est.move = FALSE),
                             assess_years = assess.years, 
                             assess_interval = assess.interval, 
                             base_years = base.years,
                             year.use = 10, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                             seed = 123,
                             save.sdrep = TRUE)

# -----------------------------------------------------------------------------
# ----------------- Assessment model with no move but Rec+1 RE ----------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

n_stocks = n_regions = 2
n_fleets = n_indices = 2

sel <- list(model=rep("logistic",n_fleets+n_indices),
            initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
            fix_pars=rep(list(NULL),n_fleets+n_indices))

sigma      <- "rec+1"
re_cor     <- "iid"
ini.opt    <- "equilibrium"
NAA_re <- list(N1_model=rep(ini.opt,n_stocks),
               sigma=rep(sigma,n_stocks),
               cor=rep(re_cor,n_stocks))

M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[5]] = loop_through_fn2(om = data, 
                             M_em = M, 
                             sel_em = sel, 
                             NAA_re_em = NAA_re, 
                             move_em = move,
                             em.opt = list(separate.em = FALSE, separate.em.type = 3, do.move = FALSE, est.move = FALSE),
                             assess_years = assess.years, 
                             assess_interval = assess.interval, 
                             base_years = base.years,
                             year.use = 10, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                             seed = 123,
                             save.sdrep = TRUE)

# -----------------------------------------------------------------------------
# -------------- Assessment model with no move but Rec  RE --------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

n_stocks = n_regions = 2
n_fleets = n_indices = 2

sel <- list(model=rep("logistic",n_fleets+n_indices),
            initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
            fix_pars=rep(list(NULL),n_fleets+n_indices))

sigma      <- "rec"
re_cor     <- "iid"
ini.opt    <- "equilibrium"
NAA_re <- list(N1_model=rep(ini.opt,n_stocks),
               sigma=rep(sigma,n_stocks),
               cor=rep(re_cor,n_stocks))

M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mods[[6]] = loop_through_fn2(om = data, 
                             M_em = M, 
                             sel_em = sel, 
                             NAA_re_em = NAA_re, 
                             move_em = move,
                             em.opt = list(separate.em = FALSE, separate.em.type = 3, do.move = FALSE, est.move = FALSE),
                             assess_years = assess.years, 
                             assess_interval = assess.interval, 
                             base_years = base.years,
                             year.use = 10, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                             seed = 123,
                             save.sdrep = TRUE)

# -----------------------------------------------------------------------------
# -------------------------------- Plot MSE output ----------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

m <- list()
m[[1]] <- list(mods[[1]],mods[[2]],mods[[3]],mods[[4]],mods[[5]],mods[[6]])

plot_mse_output(m, main.dir = main.dir) # plots should now be saved in main.dir
