# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -------------------- Examples of self and cross test ------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

main.dir = here::here()
# install.packages(file.path(main.dir,"wham"), repos = NULL, type = "source")

library(wham)
# roxygen2::roxygenize("wham")

# Create a folder to save your results
folder.name = "Example_1"
sub.dir <- folder.name
if (file.exists(sub.dir)){
} else {
  dir.create(file.path(main.dir, sub.dir))
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ----------------------------- Prepare WHAM input ----------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

year_start  <- 2013  # starting year in the burn-in period
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
M <- list(model="constant") # Default is M = 0.2
# M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)

input$par$mean_rec_pars[1,1] <- log(exp(10)*2) # Change mean rec for first stock
input$par$log_N1[1,1,1]      <- log(exp(10)*2) # Change initial N1 for the first stock
input$par$log_NAA_sigma[]    <- log(0.5) # Change the sigma for NAA (Rec+1) to be log(0.5)

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
    fit <- fit_wham(input, do.osa = F, do.retro = F, MakeADFun.silent = T)
    return(fit)
  } else return(input) 
}
set.seed(12345)
self_sim_fit <- sim_fn(om, self.fit = TRUE)

# Create HTML file to view output plots in browser
plot_wham_output(self_sim_fit, out.type = "html")

# Creates a sub directory and saves .png files 
report.dir <- "report"
if (file.exists(report.dir)){
} else {
  dir.create(file.path(main.dir, sub.dir, report.dir))
}
plot_wham_output(self_sim_fit, dir.main = file.path(main.dir, sub.dir, report.dir),out.type = 'png')

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ---------------------------------- Cross test --------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# EM with different NAA configuration
sigma      <- "rec+1" # Before "rec+1"
re_cor     <- "2dar1"
# option   <- c("age-specific-fe", "equilibrium","iid-re", "ar1-re")
ini.opt    <- "equilibrium"
NAA_re <- list(N1_model=rep(ini.opt,n_stocks),
               sigma=rep(sigma,n_stocks),
               cor=rep(re_cor,n_stocks))

input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)
em = fit_wham(input, do.fit = F, do.brps = F, MakeADFun.silent = TRUE)

sim_fn2 <- function(om, em, cross.fit = FALSE){
  input <- em$input
  input$data = om$simulate(complete=TRUE)
  if(cross.fit) {
    fit <- fit_wham(input, do.osa = F, do.retro = F, MakeADFun.silent = T)
    return(fit)
  } else return(input) 
}
set.seed(12345)
cross_sim_fit <- sim_fn2(om, em, cross.fit = TRUE)

# Create HTML file to view output plots in browser
plot_wham_output(cross_sim_fit, out.type = "html")


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ----------------------- Generate replicates for self test -------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

nsim = 5
set.seed(8675309) 
sim_input = list()
sim_input = lapply(1:nsim, function(x) {
  input_i = om$input
  sim = om$simulate(complete=TRUE)
  input_i$data = sim
  return(input_i)
})

# Self test
sim_fits = list()
sim_fits = lapply(1:nsim, function(x){
  cat(paste("model_fit:", x, "start \n"))
  out = fit_wham(sim_input[[x]], do.osa = FALSE, MakeADFun.silent = TRUE, retro.silent = TRUE, save.sdrep = FALSE)
  cat(paste("model_fit:", x, "done \n"))
  return(out)
})

# Summarize results 
conv = sapply(1:n_sim, function(x){
  if (length(sim_fits[[x]]) != 0) {
    if (sim_fits[[x]]$is_sdrep & !sim_fits[[x]]$na_sdrep & !sim_fits[[x]]$hessian) {
      conv = TRUE } else conv = FALSE
  } else conv = FALSE
  return(conv)
})
cat(paste("Convergence rate:", sum(conv)/nsim))

mean_rec_par = lapply(1:n_sim, function(x){
  mean_rec_par_est = sim_fits[[x]]$parList$mean_rec_pars[,1]
  mean_rec_par_true = sim_fits[[x]]$input$par$mean_rec_pars[,1]
  mean_rec_par = cbind(mean_rec_par_est,mean_rec_par_true)
  return(mean_rec_par)
})
print(mean_rec_par)

SSB = lapply(1:n_sim, function(x){
  SSB_est = sim_fits[[x]]$rep$SSB
  SSB_true = sim_fits[[x]]$input$data$SSB
  SSB = cbind(SSB_est, SSB_true)
  return(SSB)
})
print(SSB)




