# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------ Movement treated as random effects -----------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

main.dir = here::here()
# main.dir = "where/you/save/your/wham/package"

# install.packages(file.path(main.dir,"wham"), dependencies = TRUE, repos = NULL, type = "source")
# devtools::install_local(file.path(main.dir,"wham"), dependencies = TRUE)

library(wham)

# Create a folder to save your results
folder.name = "Example_6"
sub.dir <- folder.name
if (file.exists(file.path(main.dir,sub.dir))){
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

year_start  <- 2003  # starting year in the burn-in period
year_end    <- 2022  # end year in the burn-in period
MSE_years   <- 0     # number of years in the feedback loop

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
# see more details in ?generate_basic_info

# Specify movement type
# Note: default is "bidirectional" movement (e.g. stock 1 move to region 2 and stock 2 move to region 1)
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2)

# Configure movement random effects
# Note: default is move = 0.3 (constant) for stock1 and 0.1 (constant) for the other stocks
move <- generate_move(basic_info = basic_info, move.type = 2, move.rate = 0.3, move.re = "iid_y", move.sigma = 0.2)

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
M <- list(model="constant") # Default is M = 0.2
# M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

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

input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)

om = fit_wham(input, do.fit = F, do.brps = F, MakeADFun.silent = TRUE)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ---------------------------------- Self test --------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

sim_fn <- function(om, self.fit = FALSE){
  input <- om$input
  input$data = om$simulate(complete=TRUE)
  if(self.fit) {
    input <- fix_move(input) # fixed movement rate and sigma
    fit <- fit_wham(input, do.osa = FALSE, do.retro = FALSE, MakeADFun.silent = FALSE)
    return(fit)
  } else return(input) 
}
set.seed(12345)
self_sim_fit <- sim_fn(om, self.fit = TRUE)

# Check model convergence
check_convergence(self_sim_fit)

# Create HTML file to view output plots in browser
plot_wham_output(self_sim_fit, out.type = "html")

# Creates a sub directory and saves .png files 
report.dir <- "report"
if (file.exists(report.dir)){
} else {
  dir.create(file.path(main.dir, sub.dir, report.dir))
}
plot_wham_output(self_sim_fit, dir.main = file.path(main.dir, sub.dir, report.dir),out.type = 'png')
