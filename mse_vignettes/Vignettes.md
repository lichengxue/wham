## Example 1: Simulation-Estimation

#### 1. Load package and create a folder
```r
library(wham)
main.dir = here::here() 
folder.name = "Example_1"
sub.dir <- folder.name
if (file.exists(sub.dir)){
} else {
  dir.create(file.path(main.dir, sub.dir))
}
````

#### 2. Generate basic information 
The operating model is generated based on user-specified biological and fishery information
```r
year_start  <- 2013  # starting year in the burn-in period
year_end    <- 2022  # end year in the burn-in period
MSE_years   <- 0     # number of years in the feedback loop
# Note: no need to include MSE_years in simulation-estimation 

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
````
#### 3. Specify movement type
```r
# Note: default is "bidirectional" movement (e.g. stock 1 move to region 2 and stock 2 move to region 1)
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2)
````
#### 4. Configure movement random effects
```r
# Note: default is move = 0.3 (constant) for stock1 and 0.1 (constant) for the other stocks
move <- generate_move(basic_info = basic_info, move.type = 2, move.rate = 0.3, move.re = "constant")
````

#### 5. Configure selecitvity, numbers-at-age, and natural mortality random effects
```r
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
````
#### 6. Generate wham input 
```r
input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)
````
#### 7. Change mean recruitment para. and variance of NAA random effects (optional)
```r
input$par$mean_rec_pars[1,1] <- log(exp(10)*2) # Change mean rec for first stock
input$par$log_N1[1,1,1]      <- log(exp(10)*2) # Change initial N1 for the first stock
input$par$log_NAA_sigma[]    <- log(0.5) # Change the sigma for NAA (Rec+1) to be log(0.5)
# Note: sigma for recruitment (sigma1) and numbers at older ages (sigma2) are both 0.5 here.
````
#### 8. Generate the operating model
```r
om = fit_wham(input, do.fit = F, do.brps = F, MakeADFun.silent = TRUE)
# Note: do.fit must be FALSE (no modeling fitting yet)
````
#### 9. Self test 
```r
# Create a function to generate data and do self fitting
sim_fn <- function(om, self.fit = FALSE){
  input <- om$input
  input$data = om$simulate(complete=TRUE)
  if(self.fit) {
    fit <- fit_wham(input, do.osa = F, do.retro = F, do.osa = FALSE, do.retro = FALSE, MakeADFun.silent = T)
    return(fit)
  } else return(input) 
}
# Note: turn on do.osa to calculate one-step-ahead residuals
# Note: turn on do.retro to calculate retrospective bias
set.seed(12345)
self_sim_fit <- sim_fn(om, self.fit = TRUE)
check_convergence(self_sim_fit) # check the model convergence
````
#### 10. Create HTML file to view output plots in browser (optional)
```r
plot_wham_output(self_sim_fit, out.type = "html")
````
#### 11. Creates a sub directory and saves .png files (optional)
```r
report.dir <- "report"
if (file.exists(report.dir)){
} else {
  dir.create(file.path(main.dir, sub.dir, report.dir))
}
plot_wham_output(self_sim_fit, dir.main = file.path(main.dir, sub.dir, report.dir),out.type = 'png')
````
#### 12. Cross test
Different Number-at-age configuration is used in the estimation model 
```r
# EM with different NAA configuration
sigma      <- "rec+1" # Before "rec+1"
re_cor     <- "2dar1"
# option   <- c("age-specific-fe", "equilibrium","iid-re", "ar1-re")
ini.opt    <- "equilibrium"
NAA_re <- list(N1_model=rep(ini.opt,n_stocks),
               sigma=rep(sigma,n_stocks),
               cor=rep(re_cor,n_stocks))
# Generate wham input
input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)
# Generate EM
em = fit_wham(input, do.fit = F, do.brps = F, MakeADFun.silent = TRUE)
# Create a function to generate data and do cross fitting
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
# check model convergence
check_convergence(cross_sim_fit)
````
#### 13. Create HTML file to view output plots in browser (optional)
```r
plot_wham_output(cross_sim_fit, out.type = "html")
````
#### 14. Generate replicates for self test (optional)
Generating 100 pseudo data is common when performing a self test.
```r
nsim = 100 
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
conv = sapply(1:nsim, function(x){
  if (length(sim_fits[[x]]) != 0) {
    if (sim_fits[[x]]$is_sdrep & !sim_fits[[x]]$na_sdrep & !sim_fits[[x]]$hessian) {
      conv = TRUE } else conv = FALSE
  } else conv = FALSE
  return(conv)
})
cat(paste("Convergence rate:", sum(conv)/nsim))

mean_rec_par = lapply(1:nsim, function(x){
  mean_rec_par_est = sim_fits[[x]]$parList$mean_rec_pars[,1]
  mean_rec_par_true = sim_fits[[x]]$input$par$mean_rec_pars[,1]
  mean_rec_par = cbind(mean_rec_par_est,mean_rec_par_true)
  return(mean_rec_par)
})
print(mean_rec_par)

SSB = lapply(1:nsim, function(x){
  SSB_est = sim_fits[[x]]$rep$SSB
  SSB_true = sim_fits[[x]]$input$data$SSB
  SSB = cbind(SSB_est, SSB_true)
  return(SSB)
})
print(SSB)
````
## Example 2: Management strategy evaluation
#### 1. Create a folder to save your results
```r
main.dir = here::here()
# install.packages(file.path(main.dir,"wham"), repos = NULL, type = "source")
library(wham)
folder.name = "Example_2"
sub.dir <- folder.name
if (file.exists(sub.dir)){
} else {
  dir.create(file.path(main.dir,sub.dir))
}
````
#### 2. Prepare WHAM input
```r
year_start  <- 2013  # starting year in the burn-in period
year_end    <- 2022  # end year in the burn-in period
MSE_years   <- 3     # number of years in the feedback loop
# Note: MSE_years set to be 3 for simplicity 

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
````
#### 3. Specify movement type
```r
# Note: default is "bidirectional" movement (e.g. stock 1 move to region 2 and stock 2 move to region 1)
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2)
````
#### 4. Configure movement random effects
```r
# Note: default is move = 0.3 (constant) for stock1 and 0.1 (constant) for the other stocks
move <- generate_move(basic_info = basic_info, move.type = 2, move.rate = 0.3, move.re = "constant")
````
#### 5. Configure selecitvity, numbers-at-age, and natural mortality random effects
```r
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
````
#### 6. Generate wham input 
```r
input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)
```
#### 7. Change mean recruitment para. and variance of NAA random effects
```r
input$par$mean_rec_pars[1,1] <- log(exp(10)*2) # Change mean rec for first stock
input$par$log_N1[1,1,1]      <- log(exp(10)*2) # Change initial N1 for the first stock
input$par$log_NAA_sigma[]    <- log(0.2) # Change the sigma for NAA (Rec+1) to be log(0.5)
````
#### 8. Assign weights based on mean recruitment to calculate global SPR-based reference points
```r
# Global SPR is calculated based on weights of mean rec par 
input$data$SPR_weight_type = 1
input$data$SPR_weights     = c(2/3,1/3)
input$data$do_SPR_BRPs     = 1
````
#### 9. Generate the operating model
```r
om = fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)
saveRDS(om,file.path(sub.dir,"om.RDS")) # save the OM 
````
#### 10. Generate datasets
```r
data <- generate_data(om, seed = 123)
# Generate 100 datasets
# nsim = 100
# set.seed(12345) 
# sim_input = list()
# sim_input = lapply(1:nsim, function(x) {
#   input_i = om$input
#   sim = om$simulate(complete=TRUE)
#   input_i$data = sim
#   return(input_i)
# })
````
#### 11. MSE information
```r
assess.interval = 3 # Assessment interval
base.years      = year_start:year_end # Burn-in period
first.year      = head(base.years,1)
terminal.year   = tail(base.years,1)
assess.years    = seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)
# Create a list to save the MSE results
mods <- list()
````
### EM1: Separate Assessment Models with NAA Random Effects
```r
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
                            year.use = 10, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)
````
### EM2: One Assessment Model (Fleets-as-areas) with NAA Random Effects
```r
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
                            year.use = 10, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)
````
### EM3: One Assessment Model (Aggregate Catch) with NAA Random Effects
```r
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
                            year.use = 10, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)
````
### EM4: Correct Assessment Model with NAA Random Effects and Movement Fixed as Known
```r
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
                            year.use = 10, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)
````
### EM5: Assessment Model with NAA Random Effects but No Movement
```r
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
                            year.use = 10, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)
````
### EM6: Assessment Model with Rec Random Effects but no Movement
```r
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
                            year.use = 10, # number of years of data you want to use in the assessment model
                            seed = 123,
                            save.sdrep = FALSE)
````
### Plot MSE output
m <- list()
m[[1]] <- list(mods[[1]],mods[[2]],mods[[3]],mods[[4]],mods[[5]],mods[[6]])
plot_mse_output(m, main.dir = main.dir) # plots should now be saved in main.dir
