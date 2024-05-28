# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------ Hockey-stick Harvest Control Rule  -----------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

main.dir = here::here()
# main.dir = "where/you/save/your/wham/package"

# install.packages(file.path(main.dir,"wham"), dependencies = TRUE, repos = NULL, type = "source")
# devtools::install_local(file.path(main.dir,"wham"), dependencies = TRUE)

library(wham)

# Create a folder to save your results
folder.name = "Example_5"
sub.dir <- folder.name
if (file.exists(sub.dir)){
  setwd(file.path(main.dir,sub.dir))
} else {
  dir.create(file.path(main.dir,sub.dir))
  setwd(file.path(main.dir,sub.dir))
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
                                  F_info     = list(F.year1 = 0.4, Fhist = "Fmsy-H-L", Fmax = 2.5, Fmin = 1, change_time = 0.5), 
                                  catch_info = list(catch_cv = 0.1, catch_Neff = 200), 
                                  index_info = list(index_cv = 0.2, index_Neff = 100, fracyr_indices = 0.5), 
                                  fracyr_spawn = 0.5, 
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
M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

# Configure NAA random effects
sigma        <- "rec+1"
re_cor       <- "iid"
ini.opt      <- "equilibrium" # option   <- c("age-specific-fe", "equilibrium")
Rec_sig      <- 0.2 # (sigma for recruitment)
NAA_sig      <- 0.2 # (sigma for NAA)

# Set initial NAA for each stock
log_N1 = c(log(exp(10)*2), 10) # Create difference between stocks
N1_pars <- generate_ini_N1(log_N1,basic_info,ini.opt)

# Set mean recruitment para. for each stock
mean_rec_par <- list()
for (i in 1:n_stocks) mean_rec_par[[i]] = exp(log_N1[i])

NAA_re <- list(N1_model=rep(ini.opt,n_stocks),
               sigma=rep(sigma,n_stocks),
               cor=rep(re_cor,n_stocks),
               recruit_model = 2,  # rec random around the mean
               recruit_pars = mean_rec_par, 
               sigma_vals = rep(list(c(Rec_sig,rep(NAA_sig,n_ages-1))),n_stocks),  # two sigmas when "rec+1"
               N1_pars = N1_pars)

# recruit_model = 1: estimating annual recruitments as fixed effects or a random walk if NAA_re$sigma specified
# recruit_model = 2: estimating a mean recruitment with annual recruitments as random effects
# recruit_model = 3: Beverton-Holt stock-recruitment with annual recruitments as random effects
# recruit_model = 4: Ricker stock-recruitment with annual recruitments as random effects

# 1. recruit_pars: a list (length = n_stocks) of vectors of initial parameters for recruitment model. 
# If $recruit_model is 3 (B-H) or 4 (Ricker), parameters are "alpha" and "beta".

# 2. sigma_vals: Initial standard deviation values to use for the NAA deviations. Values are not used if recruit_model = 1 
# If sigma="rec": must be a list (length = n_stocks) of single values
# If sigma="rec+1": a list (length = n_stocks) of 2 values must be specified. First is for the first age class (recruits), second is for all other ages.

# Generate wham input 
input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)

# Global SPR is calculated based on weights of mean rec par 
input$data$SPR_weight_type <- 1
input$data$SPR_weights     <- c(2/3,1/3)
input$data$do_SPR_BRPs     <- 1

om = fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)

saveRDS(om,"om.RDS")

# Generate datasets
data <- generate_data(om, seed = 123)

# MSE information
assess.interval = 3
base.years      = year_start:year_end # Burn-in period
first.year      = head(base.years,1)
terminal.year   = tail(base.years,1)
assess.years    = seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------------ Separate Assessment Models -------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

n_stocks = n_regions = n_fleets = n_indices = 1
sel_em <- list(model=rep("logistic",n_fleets+n_indices),
               initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
               fix_pars=rep(list(NULL),n_fleets+n_indices))
NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

# Change to Hockey-stick Harvest Control Rule!
hcr_type = 3
hcr_opts = list(max_percent = 75, min_percent = 0.01, BThresh_up = 0.5, BThresh_low = 0) # use ?loop_through_fn for more details

mod = loop_through_fn(om = data, 
                      M_om = M,
                      sel_om = sel, 
                      NAA_re_om = NAA_re, 
                      mean_rec_weights = NULL,
                      move_om = move,
                      M_em = M_em, 
                      sel_em = sel_em, 
                      NAA_re_em = NAA_re_em, 
                      move_em = NULL,
                      em.opt = list(separate.em = TRUE, separate.em.type = 1, do.move = FALSE, est.move = FALSE),
                      assess_years = assess.years, 
                      assess_interval = assess.interval, 
                      base_years = base.years,
                      year.use = 10, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                      hcr.type = hcr_type, # change hcr to type 3
                      hcr.opts = hcr_opts,
                      seed = 123,
                      save.sdrep = TRUE)

saveRDS(mod,"mod.RDS") # save the result