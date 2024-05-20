# Background
The “multi-wham-mse” package is used to perform management strategy evaluation (MSE) in the situation where population structure is complex (e.g. multiple stocks in multiple regions) with different movement dynamics (e.g. natal homing). This package is designed specifically for the Woods Hole Assessment Model (WHAM), a state-space age-structured stock assessment model. So far, WHAM can incorporate multiple sources of process errors (treated as random effects) such as deviations in (1) recruitment/numbers-at-age, (2) selectivity, (3) natural mortality, (4) catchability, and (5) movement. WHAM is also capable of including environmental effects on population processes. 
## Download "multi-wham-mse" package 
You can download the “mse” branch of the “wham” package from Github by following the steps below:
#### 1. Open "Command Prompt" in your local computer.
#### 2. Type "cd directory" here you can type a specific path (replace "directory") to save the package (you can also skip this step and the package will be saved in the main directory). 
#### 3. Type "git clone -b mse https://github.com/lichengxue/wham.git".
#### 4. Close the Command and check if the package has been saved in the directory.

## Install "multi-wham-mse" package
For the users who are installing "wham" for the first time:
```r
library_paths <- .libPaths()[1]
install.packages(file.path(library/you/download/package,"wham"), repos = NULL, type = "source", lib=library_paths)
# Remember load the "mse" package using:
library(wham)
````
For the users who has "single-wham" installed before, it's necessary to install the "mse" package in a different directory to avoid one overwriting the other:
```r
devtools::install_github(file.path(library/you/download/package,"wham"), repos = NULL, type = "source", lib="library/you/want/to/save/package/")
# Remember load the "mse" package using:
library(wham, lib.loc = "library/you/want/to/save/package/")
````

## Example 1: Simulation-Estimation

### 1. Load package and create a folder
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

### 2. Generate basic information 
The operating model is generated based on user-specified biological and fishery information. Here it is worthnoting that the longer burn-in/feedback period you set, the longer runtime it may take to generate your operating model.The "generate_basic_info" function is used to create a list of biological and fishery information that can be used for generating a wham input. This function is designed for the users who don't have an ASAP3.dat file (wham is designed to take an ASAP3.dat file as input, if you have an existing ASAP3.dat file, you can skip this step). Users can define the type of fish life history, lifespan, length-at-age, weight-at-age, maturity-at-age. Users can also set fleet information, survey information, and fishing history.
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
### 3. Specify movement type and movement rate
The current version of multi-wham can be only used for the senario of "natal homing", meaning that fish in different regions outside their natal home have to go back during the spawning season. But for the seasons outside their spawning season, they can have probability (i.e. movement rate) to move from their natal home to another region and from other region to their natal home. Users have options to choose different types of movement: (1) unidirectional (only stock1 can move and the rest of stocks can't); (2) all stocks can move (bidirectinal); (3) no movement. Users are allowed to specify the movement rate for each stock. Movement can be constant over years across ages, but can be also specified as changing (treated as random effects) over years (iid_y), over ages (iid_a), over years with an autocorrelation (ar1_y), over years with an autocorrelation (ar1_a).

#### 3a. Specify movement type and movement rate (default: bidirectional)
```r
# Note: default is "bidirectional" movement (e.g. stock 1 move to region 2 and stock 2 move to region 1)
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2)
# Note: default is move = 0.3 (constant) for stock1 and 0.1 (constant) for the other stocks
move <- generate_move(basic_info = basic_info, move.type = 2, move.rate = 0.3, move.re = "constant")
````
#### 3b. Specify movement type and movement rate (unidirectional)
```r
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 1) # unidirectional movement 
move <- generate_move(basic_info = basic_info, move.type = 1, move.re = "constant") # movement rate is constant
# Users can specify movement rate, default is 0.3 for stock1 and 0 for the rest of stocks
````
#### 3c. Specify movement type and movement rate treated as random effects (iid)
```r
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2, move_rate = 0.3) # unidirectional movement 
move <- generate_move(basic_info = basic_info, move.type = 2, move.re = "iid_y") # movement rate is iid across years
````
#### 3d. Specify movement type and movement rate treated as random effects (ar1)
```r
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2, move_rate = 0.3) # unidirectional movement 
move <- generate_move(basic_info = basic_info, move.type = 2, move.re = "ar1_y") # movement rate is ar1 across years
# Users can specify mean and sigma for movement internally
````
### 4. Configure selecitvity, numbers-at-age, and natural mortality random effects
Modeling selectivity, natural mortality, and NAA as time and/or age varying (treated as random effects) is optional. More details can be found in single-wham (https://timjmiller.github.io/wham/). The default for selectivity and natural mortality is constant, but NAA including recruitment (age 1) is varying (iid). Recruitment is assumed to vary around the mean (default), but users are allowed to use "B-H" or "Ricker" stock-recruitment relationship.
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
### 5. Generate wham input 
```r
input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)
````
### 6. Change mean recruitment para. and variance of NAA random effects (optional)
Mean recruitment and sigma for all stocks are assumed the same (mean = exp(10) and sigma = exp(0)). Users are allowed to change the mean and sigma for recruitment for each stock as needed. Note that high sigma (e.g. sigma >= 1) may cause the convergence issue in the feedback loop.
```r
input$par$mean_rec_pars[1,1] <- log(exp(10)*2) # Change mean rec for first stock
input$par$log_N1[1,1,1]      <- log(exp(10)*2) # Change initial N1 for the first stock
input$par$log_NAA_sigma[]    <- log(0.5) # Change the sigma for NAA (Rec+1) to be log(0.5)
# Note: sigma for recruitment (sigma1) and numbers at older ages (sigma2) are both 0.5 here.
````
### 7. Generate the operating model
```r
om = fit_wham(input, do.fit = F, do.brps = F, MakeADFun.silent = TRUE)
# Note: do.fit must be FALSE (no modeling fitting yet)
````
### 8. Self test 
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
### 9. Create HTML file to view output plots in browser (optional)
```r
plot_wham_output(self_sim_fit, out.type = "html")
````
### 10. Creates a sub directory and saves .png files (optional)
```r
report.dir <- "report"
if (file.exists(report.dir)){
} else {
  dir.create(file.path(main.dir, sub.dir, report.dir))
}
plot_wham_output(self_sim_fit, dir.main = file.path(main.dir, sub.dir, report.dir),out.type = 'png')
````
### 11. Cross test
Different numbers-at-age configuration is used in the estimation model 
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
  input$data = om$simulate(complete=TRUE) # Generate one dataset
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
### 12. Create HTML file to view output plots in browser (optional)
```r
plot_wham_output(cross_sim_fit, out.type = "html")
````
### 13. Generate replicates for self test (optional)
Parameters (including random effects parameters) associated with population dynamics are now defined in the operating model. Users can generate pseudo observational data from the operating model with process and observation errors randomly drawn from their corresponding likelihood distribution. Generating 100 pseudo data is common when performing a self test.
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
### 1. Create a folder to save your results
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
### 2. Prepare WHAM input
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
### 3. Specify movement type and movement rate
```r
# Note: default is "bidirectional" movement (e.g. stock 1 move to region 2 and stock 2 move to region 1)
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2)
# Note: default is move = 0.3 (constant) for stock1 and 0.1 (constant) for the other stocks
move <- generate_move(basic_info = basic_info, move.type = 2, move.rate = 0.3, move.re = "constant")
````
### 4. Configure selecitvity, numbers-at-age, and natural mortality random effects
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
### 5. Generate wham input 
```r
input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)
```
### 6. Change mean recruitment para. and variance of NAA random effects
```r
input$par$mean_rec_pars[1,1] <- log(exp(10)*2) # Change mean rec for first stock
input$par$log_N1[1,1,1]      <- log(exp(10)*2) # Change initial N1 for the first stock
input$par$log_NAA_sigma[]    <- log(0.2) # Change the sigma for NAA (Rec+1) to be log(0.5)
````
### 7. Assign weights based on mean recruitment to calculate global SPR-based reference points
The SPR-based biological reference point in multi-wham is a weighted average based on the mean recruitment of each stock. The default is SPR(stock1) and SPR(stock2) are equally weighted. But users should change SPR weights if the mean recruitment for each stock is different. This step is only needed when generating the operating model. In the feedback loop, the weights will be automatically calculated given the mean recruitment estimated from the assessment model.
```r
# Global SPR is calculated based on weights of mean rec par 
input$data$SPR_weight_type = 1
input$data$SPR_weights     = c(2/3,1/3)
input$data$do_SPR_BRPs     = 1
````
### 8. Generate the operating model
```r
om = fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)
saveRDS(om,file.path(sub.dir,"om.RDS")) # save the OM 
````
### 9. Generate datasets
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
### 10. Specify assessment interval and assessment year in the feedback loop
Users can specify the assessment interval for the feedback period. For medium-lived groundfish stock, an assessment interval of 3 years is typically common in the northeast region. It should be noted that the shorter assessment interval, the longer runtime it may take for the whole feedback period.
```r
assess.interval = 3 # Assessment interval
base.years      = year_start:year_end # Burn-in period
first.year      = head(base.years,1)
terminal.year   = tail(base.years,1)
assess.years    = seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)
````
Create a list to save the MSE results
```r
mods <- list()
````
### 11. Performe Management Strategy Evaluation
The code below does a closed-loop simulation with operating model, fitting an estimating model, generating catch advice and incorporating it into the operating model.
#### EM1: Separate assessment models with NAA random effects
Fit separate assessment models for each stock like traditional single-stock assessment
![EM1](https://github.com/lichengxue/wham/blob/mse/mse_vignettes/Vignettes_figs/EM1.png)
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
#### EM2: Assessment model (spatially-implicit fleets-as-areas) with NAA Random Effects
![EM2](https://github.com/lichengxue/wham/blob/mse/mse_vignettes/Vignettes_figs/EM2.png)
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
### EM3: Assessment model (catch and survey aggregated) with NAA Random Effects
![EM3](https://github.com/lichengxue/wham/blob/mse/mse_vignettes/Vignettes_figs/EM3.png)
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
### EM4: Correct assessment model with NAA random effects and movement fixed as known
![EM4](https://github.com/lichengxue/wham/blob/mse/mse_vignettes/Vignettes_figs/EM4.png)
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
### EM5: Assessment model with NAA random effects but no movement
![EM5](https://github.com/lichengxue/wham/blob/mse/mse_vignettes/Vignettes_figs/EM5.png)
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
### EM6: Assessment Model with Rec random effects but no movement
![EM6](https://github.com/lichengxue/wham/blob/mse/mse_vignettes/Vignettes_figs/EM6.png)
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
### Plot MSE Output
```r
m <- list()
m[[1]] <- list(mods[[1]],mods[[2]],mods[[3]],mods[[4]],mods[[5]],mods[[6]])
plot_mse_output(m, main.dir = main.dir) # plots should now be saved in main.dir
````
