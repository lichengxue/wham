# Background
The “multi-wham-mse” package is used to perform management strategy evaluation (MSE) in the situation where population structure is complex (e.g. multiple stocks in multiple regions) with different movement dynamics (e.g. natal homing). This package is designed specifically for the Woods Hole Assessment Model (WHAM), a state-space age-structured stock assessment model. So far, WHAM can incorporate multiple sources of process errors (treated as random effects) such as deviations in (1) recruitment/numbers-at-age, (2) selectivity, (3) natural mortality, (4) catchability, and (5) movement. WHAM is also capable of including environmental effects on population processes. 
## Download "multi-wham-mse" package (for git users)
You can download the “mse” branch of the “wham” package from Github by following the steps below:
  #### 1. Open "Command Prompt" in your local computer.
  #### 2. Type "cd directory" here you can type a specific path (replace "directory") to save the package (you can also skip this step and the package will be saved in the main directory). 
  #### 3. Type "git clone -b mse https://github.com/lichengxue/wham.git".
  #### 4. Close the Command and check if the package has been saved in the directory.
  
  ## Download "multi-wham-mse" package (for non-git users)
  You can download the “mse” branch of the “wham” package from Github by following the steps below:
  #### 1. Open your browser and go to: https://github.com/lichengxue/wham/tree/mse
  #### 2. Select the green "Code" button on the main page. 
  #### 3. Once the dropdown menu appears, select Download ZIP to download the mse package.
  #### 4. Upzip the package, you should see all content are saved in "your_path/wham-mse/wham-mse" (Note: There are two folders with exact same name!)
  #### 5. Create a new folder, rename as "wham", move all the content from your_path/wham-mse/wham-mse to this new folder for the later installation.
  
  ## Install "multi-wham-mse" package
  For the users who are installing "wham" for the first time:
  ```r
# Make sure you have the most updated Rtools (used to compile TMB code) installed before running the install.packages function
install.packages(file.path(library/you/download/package,"wham"), dependencies = TRUE, repos = NULL, type = "source")
# devtools::install_local(file.path(main.dir,"wham"), dependencies = TRUE) # Alternative
# Remember load the "mse" package using:
library(wham)
````
For the users who has "single-wham" installed before, it's necessary to install the "mse" package in a different directory to avoid one overwriting the other:
```r
library_paths <- .libPaths()[1]
new_folder <- "wham_old"
if (file.exists(file.path(library_paths,new_folder))){
} else {
  dir.create(file.path(library_paths,new_folder))
}

file.copy(from = file.path(library_paths,"wham"), to = file.path(library_paths,new_folder), 
          overwrite = TRUE,  recursive = TRUE, copy.mode = TRUE)

# library(wham, lib.loc = file.path(library_paths,new_folder)) # Load the single wham package

# Make sure you have the most updated Rtools (used to compile TMB code) installed before running the install.packages function
install.packages(file.path(library/you/download/package,"wham"), dependencies = TRUE, repos = NULL, type = "source")
# devtools::install_local(file.path(main.dir,"wham"), dependencies = TRUE) # Alternative
# Remember load the "mse" package using:
library(wham)
````

## Example 1: Simulation-Estimation

### 1. Load package and create a folder
```r
library(wham)
main.dir = here::here()
# main.dir = "where/you/save/your/wham/package"

folder.name = "Example_1"
sub.dir <- folder.name
if (file.exists(file.path(main.dir,sub.dir))){
  setwd(file.path(main.dir,sub.dir))
} else {
  dir.create(file.path(main.dir,sub.dir))
  setwd(file.path(main.dir,sub.dir))
}
````

### 2. Generate basic information 
The operating model is generated based on user-specified biological and fishery information. Here it is worthnoting that the longer burn-in/feedback period you set, the longer runtime it may take to generate your operating model.The "generate_basic_info" function is used to create a list of biological and fishery information that can be used for generating a wham input. This function is designed for the users who don't have an ASAP3.dat file (wham is designed to take an ASAP3.dat file as input, if you have an existing ASAP3.dat file, you can skip this step). Users can define the type of fish life history, lifespan, length-at-age, weight-at-age, maturity-at-age. Users can also set fleet information, survey information, and fishing history.
```r
year_start  <- 2003  # starting year in the burn-in period
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
                                  index_info = list(index_cv = 0.2, index_Neff = 100, fracyr_indices = 0.5),
                                  fracyr_spawn = 0.5, 
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
log_N1  <- c(log(exp(10)*2), 10) # Create difference between stocks
N1_pars <- generate_ini_N1(log_N1,basic_info,ini.opt)

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
````
### 5. Generate wham input 
```r
input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)
````

### 6. Generate the operating model
```r
om <- fit_wham(input, do.fit = F, do.brps = F, MakeADFun.silent = TRUE)
# Note: do.fit must be FALSE (no modeling fitting yet)
````
### 7. Self test 
```r
# Create a function to generate data and do self fitting
sim_fn <- function(om, self.fit = FALSE){
  input <- om$input
  input$data <- om$simulate(complete=TRUE)
  # $simulate() is a function to generate datasets given the parameters defined in the OM
  # Note: process error and observation error are both turned on when simulating datasets 
  # To turn them off, use basic_info$simulate_process_error = 0 and basic_info$simulate_observation_error = 0
  if(self.fit) {
    fit <- fit_wham(input, do.osa = FALSE, do.retro = FALSE, MakeADFun.silent = FALSE)
    return(fit)
  } else return(input) 
}
# Note: turn on do.osa to calculate one-step-ahead residuals
# Note: turn on do.retro to calculate retrospective bias
set.seed(12345)
self_sim_fit <- sim_fn(om, self.fit = TRUE)
check_convergence(self_sim_fit) # check the model convergence
````
### 8. Create HTML file to view output plots in browser (optional)
```r
plot_wham_output(self_sim_fit, out.type = "html")
````
### 9. Creates a sub directory and saves .png files (optional)
```r
report.dir <- "report"
if (file.exists(report.dir)){
} else {
  dir.create(file.path(main.dir, sub.dir, report.dir))
}
plot_wham_output(self_sim_fit, dir.main = file.path(main.dir, sub.dir, report.dir),out.type = 'png')
````
### 10. Cross test
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
em <- fit_wham(input, do.fit = F, do.brps = F, MakeADFun.silent = TRUE)
# Create a function to generate data and do cross fitting
sim_fn2 <- function(om, em, cross.fit = FALSE){
  input <- em$input
  input$data <- om$simulate(complete=TRUE)
  if(cross.fit) {
    fit <- fit_wham(input, do.osa = FALSE, do.retro = FALSE, MakeADFun.silent = FALSE)
    return(fit)
  } else return(input) 
}
set.seed(12345)
cross_sim_fit <- sim_fn2(om, em, cross.fit = TRUE)
# check model convergence
check_convergence(cross_sim_fit)
````
### 11. Create HTML file to view output plots in browser (optional)
```r
plot_wham_output(cross_sim_fit, out.type = "html")
````
### 12. Generate replicates for self test (optional)
Parameters (including random effects parameters) associated with population dynamics are now defined in the operating model. Users can generate pseudo observational data from the operating model with process and observation errors randomly drawn from their corresponding likelihood distribution. Generating 100 pseudo data is common when performing a self test.
```r
nsim <- 100 
set.seed(8675309) 
sim_input <- list()
sim_input <- lapply(1:nsim, function(x) {
  input_i <- om$input
  sim <- om$simulate(complete=TRUE)
  input_i$data <- sim
  return(input_i)
})

# Self test
sim_fits <- list()
sim_fits <- lapply(1:nsim, function(x){
  cat(paste("model_fit:", x, "start \n"))
  out <- fit_wham(sim_input[[x]], do.osa = FALSE, MakeADFun.silent = TRUE, retro.silent = TRUE, save.sdrep = FALSE)
  cat(paste("model_fit:", x, "done \n"))
  return(out)
})

# Summarize results 
conv <- sapply(1:nsim, function(x){
  if (length(sim_fits[[x]]) != 0) {
    if (sim_fits[[x]]$is_sdrep & !sim_fits[[x]]$na_sdrep & !sim_fits[[x]]$hessian) {
      conv = TRUE } else conv = FALSE
  } else conv = FALSE
  return(conv)
})
cat(paste("Convergence rate:", sum(conv)/nsim))

mean_rec_par <- lapply(1:nsim, function(x){
  mean_rec_par_est <- sim_fits[[x]]$parList$mean_rec_pars[,1]
  mean_rec_par_true <- sim_fits[[x]]$input$par$mean_rec_pars[,1]
  mean_rec_par <- cbind(mean_rec_par_est,mean_rec_par_true)
  return(mean_rec_par)
})
print(mean_rec_par)

SSB <- lapply(1:nsim, function(x){
  SSB_est <- sim_fits[[x]]$rep$SSB
  SSB_true <- sim_fits[[x]]$input$data$SSB
  SSB <- cbind(SSB_est, SSB_true)
  return(SSB)
})
print(SSB)
````

### 13. Generate replicates for cross test (optional)
```r
nsim <- 100
set.seed(8675309) 
sim_input <- list()
sim_input <- lapply(1:nsim, function(x) {
  input_i <- em$input
  sim <- om$simulate(complete=TRUE)
  input_i$data <- sim
  return(input_i)
})
````
