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

install.packages(file.path(library/you/download/package,"wham"), dependencies = TRUE, repos = NULL, type = "source")
# devtools::install_local(file.path(main.dir,"wham"), dependencies = TRUE) # Alternative
# Remember load the "mse" package using:
library(wham)
````

## Example 2: Management strategy evaluation

### 1. Create a folder to save your results
```r
main.dir = here::here()
# main.dir = "where/you/save/your/wham/package"

# install.packages(file.path(main.dir,"wham"), dependencies = TRUE, repos = NULL, type = "source")
# devtools::install_local(file.path(main.dir,"wham"), dependencies = TRUE)

library(wham)
folder.name = "Example_2"
sub.dir <- folder.name
if (file.exists(sub.dir)){
  setwd(file.path(main.dir,sub.dir))
} else {
  dir.create(file.path(main.dir,sub.dir))
  setwd(file.path(main.dir,sub.dir))
}
````
### 2. Generate basic information 
The operating model is generated based on user-specified biological and fishery information. Here it is worthnoting that the longer burn-in/feedback period you set, the longer runtime it may take to generate your operating model.The "generate_basic_info" function is used to create a list of biological and fishery information that can be used for generating a wham input. This function is designed for the users who don't have an ASAP3.dat file (wham is designed to take an ASAP3.dat file as input, if you have an existing ASAP3.dat file, you can skip this step). Users can define the type of fish life history, lifespan, length-at-age, weight-at-age, maturity-at-age. Users can also set fleet information, survey information, and fishing history.
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
                                  index_info = list(index_cv = 0.2, index_Neff = 100, fracyr_indices = 0.5), 
                                  fracyr_spawn = 0.5, 
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
### 4. Configure selecitvity and natural mortality
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
# M <- list(model="constant") # Default is M = 0.2
M <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))
````

### 5. Configure NAA random effects
```r
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

# recruit_model = 1: estimating annual recruitments as fixed effects or a random walk if NAA_re$sigma specified
# recruit_model = 2: estimating a mean recruitment with annual recruitments as random effects
# recruit_model = 3: Beverton-Holt stock-recruitment with annual recruitments as random effects
# recruit_model = 4: Ricker stock-recruitment with annual recruitments as random effects

# 1. recruit_pars: a list (length = n_stocks) of vectors of initial parameters for recruitment model. 
# If $recruit_model is 3 (B-H) or 4 (Ricker), parameters are "alpha" and "beta".

# 2. sigma_vals: Initial standard deviation values to use for the NAA deviations. Values are not used if recruit_model = 1 
# If sigma="rec": must be a list (length = n_stocks) of single values
# If sigma="rec+1": a list (length = n_stocks) of 2 values must be specified. First is for the first age class (recruits), second is for all other ages.
````

### 5. Generate wham input 
```r
input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)
```

### 6. Assign weights based on mean recruitment to calculate global SPR-based reference points
The SPR-based biological reference point in multi-wham is a weighted average based on the mean recruitment of each stock. The default is SPR(stock1) and SPR(stock2) are equally weighted. But users should change SPR weights if the mean recruitment for each stock is different. This step is only needed when generating the operating model. In the feedback loop, the weights will be automatically calculated given the mean recruitment estimated from the assessment model.
```r
# Global SPR is calculated based on weights of mean rec par 
input$data$SPR_weight_type <- 1
input$data$SPR_weights     <- c(2/3,1/3)
input$data$do_SPR_BRPs     <- 1
````
### 7. Generate the operating model
```r
om = fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)
print(exp(om$rep$log_FXSPR_static)) # This is static F40%, now you may replace F_info$F.year1 with this value in the above generate_basic_info function if you want historic fishing mortality to be at F40% 
saveRDS(om,"om.RDS") # save the OM 
````
### 8. Generate datasets
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
### 9. Specify assessment interval and assessment year in the feedback loop
Users can specify the assessment interval for the feedback period. For medium-lived groundfish stock, an assessment interval of 3 years is typically common in the northeast region. It should be noted that the shorter assessment interval, the longer runtime it may take for the whole feedback period.
```r
assess.interval <- 3 # Assessment interval
base.years      <- year_start:year_end # Burn-in period
first.year      <- head(base.years,1)
terminal.year   <- tail(base.years,1)
assess.years    <- seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)
````
Create a list to save the MSE results
```r
mods <- list()
````
## 10. Perform Management Strategy Evaluation
The code below does a closed-loop simulation with operating model, fitting an estimating model, generating catch advice and incorporating it into the operating model.

The table shown below describes the options of assessment model in the mse package

| Model           |                    Type                 |  Number of models  |    Movement    | Random effects  |    Reference point  |                         Description                             |  
  |:----------------|:----------------------------------------|:-------------------|:---------------|:----------------|:--------------------|:----------------------------------------------------------------|
  | EM1             |  Panmictic                              | Number of regions  | No             | NAA             | No global SPR-based |Separate/independent single stock assessment model               |
  | EM2             |  Spatially implicit (fleets-as-areas)   | One                | No             | NAA             | No global SPR-based |Multiple fleets account for spatial difference in fleet structure|
  | EM3             |  Panmictic (catch aggregated)           | One                | No             | NAA             | No global SPR-based |Fleet aggregated across regions                                  |
  | EM4             |  Spatially explicit                     | Number of regions  | Yes (fixed)    | NAA             | Global SPR-based    |Movement rate is fixed as known                                  |
  | EM5             |  Spatially explicit                     | Number of regions  | No             | NAA             | Global SPR-based    |Movement is not included                                         |
  | EM6             |  Spatially explicit                     | Number of regions  | No             | Rec             | Global SPR-based    |Movement is not included                                         |
  | EM7             |  Spatially explicit                     | Number of regions  | Yes (estimated)| NAA             | Global SPR-based    |Movement rate is estimated with a prior and standard deviation   |
  
  ### EM1: Separate panmictic assessment models with NAA random effects
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


### EM2: One assessment model (spatially-implicit fleets-as-areas) with NAA Random Effects
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


### EM3: One panmictic assessment model (catch and survey aggregated) with NAA Random Effects
The partitioning of a total catch is based on the mean recruitment of each stock.
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


### EM5: Assessment model (spatially explicit) with NAA random effects but no movement
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


### EM6: Assessment Model (spatially explicit) with Rec random effects but no movement
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

### More replicates are ideal for better visualization of the MSE results, so below is the summary of 10 replicates
#### Fig.1 Model performance of SSB, catch, and F 
![Fig.1](https://github.com/lichengxue/wham/blob/mse/mse_vignettes/Report/Performance_last_6years.PNG)
#### Fig.2 Model performance of stock status
![Fig.2](https://github.com/lichengxue/wham/blob/mse/mse_vignettes/Report/Stock_Status_Performance_last_6years.PNG)
#### Fig.3 Model performance in a radar chart
![Fig.3](https://github.com/lichengxue/wham/assets/43245366/001531ab-4f39-481c-b893-3274592b60f3)

#### Fig.4 Performance of different assessment models from a single realization
![Fig.4](https://github.com/lichengxue/wham/blob/mse/mse_vignettes/Report/Performance_Metrics_one_realization.PNG)

#### More Figures are produced by the plot_mse_output() function, check ?plot_mse_output() for more details.

### Check individual assessment model
```r
# check 2 separate assessement model (em1 and em2) from EM1
em1 <- mods[[1]]$em_full[[1]][[1]] # here mods[[1]] means 1st realization, em_full[[1]][[2]] means 1st assessment model from 1st stok assessment in the feedback loop 
check_convergence(em1)
plot_wham_output(em1, out.type = "html")

em2 <- mods[[1]]$em_full[[1]][[2]] # here mods[[1]] means 1st realization, em_full[[1]][[2]] means 2nd assessment model from 1st stok assessment in the feedback loop 
check_convergence(em2)
plot_wham_output(em1, out.type = "html")
````