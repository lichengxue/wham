# Background
The “multiwham-mse” package is used to perform management strategy evaluation (MSE) in the situation where population structure is complex (e.g. multiple stocks in multiple regions) with different movement dynamics (e.g. natal homing). This package is designed specifically for the Woods Hole Assessment Model (WHAM), a state-space age-structured stock assessment model. So far, WHAM can incorporate multiple sources of process errors (treated as random effects) such as deviations in (1) recruitment/numbers-at-age, (2) selectivity, (3) natural mortality, (4) catchability, and (5) movement. WHAM is also capable of including environmental effects on population processes. 
## Installation 
You can install the “lab_test” branch of the “wham” package from Github using:
```r
devtools::install_github("timjmiller/wham", dependencies=TRUE, ref="lab_test")
````
## Perform MSE 
### 1. Load the MSE functions 
For the users who are installing "wham" for the first time, please skip this step. For the users who has "single-wham" installed before, it's necessary to install the "multi-wham" package in a different directory to avoid one overwriting the other. Here is what you can do:
```r
folder.name = "multi_wham"
sub.dir <- folder.name 
if (file.exists(sub.dir)){
} else {
  dir.create(file.path("library/you/save/Rpackages", sub.dir))
}
devtools::install_github("timjmiller/wham", dependencies=TRUE, ref="lab_test", lib="library/you/save/Rpackages/multi_wham")
# Remember load the "multi-wham" package using:
library(wham, lib.loc = "library/you/save/Rpackages/multi_wham")
````
As these MSE functions have not yet been turned into a package, you can directly source the R scripts from Github using:
```r
library(wham)
library(kableExtra)
main.dir = "folder/where/you/save/these/functions"
setwd(main.dir)
# Load all functions needed
function_files <- list.files(main.dir,pattern = "\\.R$",full.names = TRUE)
for (file in function_files) {
  source(file)
}
# Create a folder to save your MSE results
folder.name = "Scenario1"
sub.dir <- folder.name 
if (file.exists(sub.dir)){
} else {
  dir.create(file.path(main.dir, sub.dir))
}
````

### 2. Set burn-in period and feedback period
It is worthnoting that the longer burn-in/feedback period you set, the longer runtime it may take to generate your operating model.  
```r
year_start  = 1993  # starting year in the burn-in period
year_end    = 2022  # end year in the burn-in period 
MSE_years   = 3     # number of years in the feedback loop
````
### 3. Generate a list of basic information 
The "generate_basic_info" function is used to create a list of biological and fishery information that can be used for generating a wham input. This function is designed for the users who don't have an ASAP3.dat file (wham is designed to take an ASAP3.dat file as input, if you have an existing ASAP3.dat file, you can skip this step). Users have options to choose the type of fish life history (short-, medium-, or long-lived) and corresponding lifespan, and information about length-at-age, weight-at-age, maturity-at-age will be automatically generated based on empirical data. Users can also mannually specify all of these information. Users have options to set fishing history to be (1) constant (e.g. at F40% SPR); (2) "updown": go up until the midpoint then go down; (3) "downup": go down until the midpoint then go up; (4) "Fmsy-H-L": 2.5xF40% until the midpoint then F40%. 
```r
# Generate a list of basic information that will be used to create a model object later
basic_info <- generate_basic_info (n_stocks = 2,                       # number of stocks
                                   n_regions = 2,                      # number of regions 
                                   n_indices = 2,                      # number of indices
                                   n_fleets = 2,                       # number of fleets
                                   n_seasons = 4,                      # number of seasons
                                   base.years = year_start: year_end,  # burn-in period
                                   n_feedback_years = MSE_years,       # feedback period
                                   life_history = "medium",            # species life history: short-, medium-, long-lived (can be also specified). 
                                   n_ages = 12,                        # number of ages
                                   recruit_model = 2,                  # recruitment models (2: random around the mean; 3: B-H; 4: Ricker)
                                   q = 0.2,                            # catchability 
                                   F.config = 2,                       # 2: year-specific F (default); 1: F deviations
                                   F.year1 = 0.3958467,                # F in the first year (default: F40%)
                                   Fhist = "Fmsy-H-L",                 # Fmsy-H-L: 2.5xF40% in the 1st half of burn-in period and F40% in the 2nd half 
                                   Fbar_ages = 10:12,                  # Average F over some older ages
                                   catch_cv = 0.1,                     # CV for catch 
                                   catch_Neff = 200,                   # Effective sample size for catch
                                   index_cv = 0.2,                     # CV for index
                                   index_Neff = 100,                   # Effective sample size for index
                                   fracyr_indices = 0.5,               # Fraction of the year when survey is conducted
                                   fracyr_spawn = 0.5,                 # Fraction of the year spawning occurs
                                   bias.correct.process = FALSE,       # T/F process error is bias corrected
                                   bias.correct.observation = FALSE,   # T/F observation error is bias corrected
                                   bias.correct.BRPs = FALSE           # T/F brp is bias corrected
                                   )

````
### 4. Specify movement type and movement rate
The current version of multi-wham can be only used for the senario of "natal homing", meaning that fish in different regions outside their natal home have to go back during the spawning season. But for the seasons outside their spawning season, they can have probability (i.e. movement rate) to move from their natal home to another region and from other region to their natal home. Users have options to choose different types of movement: (1) unidirectional (only stock1 can move and the rest of stocks can't); (2) all stocks can move (bidirectinal); (3) no movement. Users are also allowed to specify the movement rate for each stock. Movement can be constant over years across ages, but can be also specified as changing (treated as random effects) over years (iid_y), over ages (iid_a), over years with an autocorrelation (ar1_y), over years with an autocorrelation (ar1_a). 
#### 4a. Specify movement type and movement rate (default:bidirectional)
```r
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2) # bidirectional movement 
move <- generate_move(basic_info = basic_info, move.type = 2, move.re = "constant", move_rate = 0.3) # movement rate is constant
# Users can specify movement rate, default is 0.3 for stock1 and 0.1 for the rest of stocks 
````
```r
# Spawntime is a vector and first element is used 
# All stocks 'can' move (default) 
# Movement rate for stock 1 is 0.3
# Movement rate for the other stocks is 0.1
# Movement is assumed at a constant rate without random effects
````
#### 4b. Specify movement type and movement rate (unidirectional)
```r
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 1, move_rate = 0.3) # unidirectional movement 
move <- generate_move(basic_info = basic_info, move.type = 1, move.re = "constant") # movement rate is constant
# Users can specify movement rate, default is 0.3 for stock1 and 0 for the rest of stocks 
````
#### 4c. Specify movement type and movement rate treated as random effects (iid)
```r
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2, move_rate = 0.3) # unidirectional movement 
move <- generate_move(basic_info = basic_info, move.type = 2, move.re = "iid_y") # movement rate is iid across years
# Users can specify mean and sigma for movement internally
````
#### 4d. Specify movement type and movement rate treated as random effects (ar1)
```r
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = 2, move_rate = 0.3) # unidirectional movement 
move <- generate_move(basic_info = basic_info, move.type = 2, move.re = "ar1_y") # movement rate is ar1 across years
# Users can specify mean, sigma, and ar1 coefficient for movement internally
````
### 5. Specify the options for modeling selectivity, natural mortality, and numbers-at-age
Modeling selectivity, natural mortality, and NAA as time and/or age varying (treated as random effects) is optional. More details can be found in single-wham (https://timjmiller.github.io/wham/). The default for selectivity and natural mortality is constant, but NAA including recruitment (age 1) is varying (iid). Recruitment is assumed to vary around the mean (default), but users are allowed to use "B-H" or "Ricker" stock-recruitment relationship.     
```r
# Selectivity, M, and NAA can be treated as random effects
# Default is NAA treated as random effects (iid) and sel and M are constant
sel    <- pe_config(basic_info = basic_info)$sel
M      <- pe_config(basic_info = basic_info)$M
NAA_re <- pe_config(basic_info = basic_info)$NAA_re
````
### 6. Use the basic_info to create a wham input 
```r
input <- prepare_wham_input(basic_info = basic_info, selectivity = sel, M = M, NAA_re = NAA_re, move = move)
````
### 7. Set mean and sigma for recruitment externally
Mean recruitment and sigma for all stocks are assumed the same (mean = exp(10) and sigma = exp(0)). Users are allowed to change the mean and sigma for recruitment for each stock as needed. Note that high sigma (e.g. sigma >= 1) may cause the convergence issue in the feedback loop.
```r
input$par$mean_rec_pars[1,1] = log(exp(10)*2)
input$par$log_N1[1,1,1] = log(exp(10)*2)
input$par$log_NAA_sigma[] = log(0.2)
# Note: default for log(mean rec) is 10 and log(sigma) is 0 for all stocks
# here I change the mean rec for stock1 to be twice as high as that of the other stocks
# here I change the sigma for all numbers-at-age including rec to be low (log(0.2))
````
###  8. Set weights (based on recruitment) for calculating brps 
The SPR-based biological reference point in multi-wham is a weighted average based on the mean recruitment of each stock. The default is SPR(stock1) and SPR(stock2) are equally weighted. But users should change SPR weights if the mean recruitment for each stock is different. This step is only needed when users want to calculate and use F40% in the burn-in period. In the feedback loop, the weights will be automatically calculated given the mean recruitment estimated from the assessment model.
```r
input$data$SPR_weight_type = 1
input$data$SPR_weights     = c(2/3,1/3)
input$data$do_SPR_BRPs     = 1
````
### 9. Generate an operating model

```r
om = fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)
# Users can use stock-specific F40% calculated here to set fishing history in the burn-in period.
# check F40% using om$rep$log_FXSPR_static and replace F.year1 with F40% in the generate_basic_info function.
# Sorry this has to be done mannually.
saveRDS(om,file.path(sub.dir,"om.RDS"))

````
### 10. Simulate pseudo data
Parameters (including random effects parameters) associated with population dynamics are now defined in the operating model. Users can generate pseudo observational data from the operating model with process and observation errors randomly drawn from their corresponding likelihood distribution. Here is an example using the operating model to generate one psedu data
```r
data <- update_om_fn(om, seed = 123)
````
It is recommended to generate at least 50 pseudo data for the simulation study, so here is an example that simulate data using parallel computing.
```r
do.parallel = TRUE
n.rep = 60 # It depends how many cores/workers
# Option for parallel computation
if (do.parallel) {
  library(doParallel)
  myCluster <- makeCluster(n.rep) # type of cluster
  print(paste0("Number of Cores: ",detectCores()))
  print(paste0("Cores Registered: ",myCluster))
  registerDoParallel(myCluster)
}
if(do.parallel){
  foreach (i = 1:60) %dopar% {
    library(wham)
    set.seed(i+12345)
    om = readRDS(file.path(sub.dir,"om.RDS"))
    obs_names = c("agg_indices","agg_catch","catch_paa","index_paa", "Ecov_obs", "obsvec")
    om_sim = om$simulate(complete=TRUE) # resimulate the population and observations
    om$input$par[om$input$random] = om_sim[om$input$random]
    om$input$data[obs_names] = om_sim[obs_names] #update any simulated data
    om <- fit_wham(om$input, do.fit = F, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
    saveRDS(om, file.path(sub.dir,paste0("sim_input_",i,".RDS")))
  }
}
data = list()
for (i in 1:60) {
  data[[i]]<-readRDS(file.path(sub.dir,paste0("sim_input_",i,".RDS")))
}
saveRDS(data, file.path(sub.dir,"sim_input.RDS"))
stopCluster(myCluster) # don't forget to turn the parallel computing off
````
### 11. Specify assessment interval and assessment year in the feedback loop
Users can specify the assessment interval for the feedback period. For medium-lived groundfish stock, an assessment interval of 3 years is typically common in the northeast region. It should be noted that the shorter assessment interval, the longer runtime it may take for the whole feedback period.
```r
assess.interval = 3
base.years      = year_start:year_end # Burn-in period
first.year      = head(base.years,1)
terminal.year   = tail(base.years,1)
assess.years    = seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)
````
### 12. Closed-loop simulation
The code below does a closed-loop simulation with operating model, fitting an estimating model, generating catch advice and incorporating it into the operating model.
You can run MSE for one realization using:
```r
looped_res = loop_through_fn(om = data, 
                             M_em = M, 
                             sel_em = sel, 
                             NAA_re_em = NAA_re, 
                             move_em = NULL,
                             em.opt = list(separate.em = TRUE, separate.em.type = 1, do.move = FALSE, est.move = FALSE),
                             assess_years = assess.years, 
                             assess_interval = assess.interval, 
                             base_years = base.years,
                             year.use = 30, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                             seed = 123)
saveRDS(looped_res,file.path(sub.dir,"Mod1_%03d.RDS"))
````

You can run MSE for multiple realizations in parallel using:
```r
# Use separate models (n models estimated independently) with no movement but NAA treated as RE
do.mod1 = TRUE
do.parallel = TRUE

n.rep = 60 # It depends how many cores/workers
# Option for parallel computation
if (do.parallel) {
  library(doParallel)
  myCluster <- makeCluster(n.rep) # type of cluster
  print(paste0("Number of Cores: ",detectCores()))
  print(paste0("Cores Registered: ",myCluster))
  registerDoParallel(myCluster)
}

# Use separate individual assessment models with no movement but NAA treated as RE
if (do.mod1) {
  if(do.parallel){
    foreach (i = 1:60) %dopar% {
      library(wham)
      function_files <- list.files(main.dir,pattern = "\\.R$",full.names = TRUE)
      for (file in function_files) {
        source(file)
      }
      stock_om = data[[i]]
      looped_res = loop_through_fn(om = stock_om, 
                                   M_em = M, 
                                   sel_em = sel, 
                                   NAA_re_em = NAA_re, 
                                   move_em = NULL,
                                   em.opt = list(separate.em = TRUE, separate.em.type = 1, do.move = FALSE, est.move = FALSE),
                                   assess_years = assess.years, 
                                   assess_interval = assess.interval, 
                                   base_years = base.years,
                                   year.use = 30, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                                   seed = i + 12345)
      saveRDS(looped_res,file.path(sub.dir,sprintf("Mod1_%03d.RDS",i)))
    }
  }
}
stopCluster(myCluster) # don't forget to turn the parallel computing off
````
### 13. Functions in the wrapper "loop_through_fn"
The "loop_through_fn" is a wrapper functions that consists of: 

(1) "make_em_input": specify the EM configrations and update observations (generated from the OM) in the EM

(2) "fit_wham": fit the EM (retrospective bias, OSA residuals are optional)

(3) "advice_fn": use brp calculated from the EM to set HRC (e.g. 75% of F40%SPR) and generate catch advice (e.g. project catch for the next x years)

(4) "update_om_fn": update F in the OM given the catch advice and update the population and observations

(5) The EM uses data as it becomes available as time progresses through the feedback period until the end of feedback period


### 14. Assessment model options

#### 1. Fit models for different stocks separately in single-stock version of wham

EM1: Use separate individual assessment models (no globle SPR) with NAA treated as random effects (no movement)
```r
looped_res = loop_through_fn(om = stock_om, 
                             M_em = M, 
                             sel_em = sel, 
                             NAA_re_em = NAA_re, 
                             move_em = NULL,
                             em.opt = list(separate.em = TRUE, separate.em.type = 1, do.move = FALSE, est.move = FALSE),
                             assess_years = assess.years, 
                             assess_interval = assess.interval, 
                             base_years = base.years,
                             year.use = 30, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                             seed = i + 12345)
````
![EM1](https://github.com/lichengxue/multiwham_mse/assets/43245366/bf07901c-e012-43dc-987a-18cb24ede166)

EM2: Use one model with fleets-as-areas and with NAA treated as random effects (no movement)
```r
looped_res = loop_through_fn(om = stock_om, 
                                   M_em = M, 
                                   sel_em = sel, 
                                   NAA_re_em = NAA_re, 
                                   move_em = NULL,
                                   em.opt = list(separate.em = TRUE, separate.em.type = 2, do.move = FALSE, est.move = FALSE),
                                   assess_years = assess.years, 
                                   assess_interval = assess.interval, 
                                   base_years = base.years,
                                   year.use = 30,
                                   seed = i + 12345)
````
![EM2](https://github.com/lichengxue/multiwham_mse/assets/43245366/061e4ef3-c979-4a4c-94cb-5dc22934a980)

EM3: Use one model with aggregate catch and with NAA treated as random effects (no movement) 
```r
looped_res = loop_through_fn(om = stock_om, 
                                   M_em = M, 
                                   sel_em = sel, 
                                   NAA_re_em = NAA_re, 
                                   move_em = NULL,
                                   em.opt = list(separate.em = TRUE, separate.em.type = 3, do.move = FALSE, est.move = FALSE),
                                   assess_years = assess.years, 
                                   assess_interval = assess.interval, 
                                   base_years = base.years,
                                   year.use = 30,
                                   seed = i + 12345)
````
![EM3](https://github.com/lichengxue/multiwham_mse/assets/43245366/3a1fa6a2-a0df-41e1-bf5d-6c5be45cc4bd)

#### 2. Fit model for different stocks simultaneously in multi-stock version of wham (use global SPR)

EM4: Use one model that consists of multiple stocks with NAA treated as random effects (no movement) 
```r
looped_res = loop_through_fn(om = stock_om, 
                                   M_em = M, 
                                   sel_em = sel, 
                                   NAA_re_em = NAA_re, 
                                   move_em = NULL,
                                   em.opt = list(separate.em = FALSE, separate.em.type = 3, do.move = FALSE, est.move = FALSE),
                                   assess_years = assess.years, 
                                   assess_interval = assess.interval, 
                                   base_years = base.years,
                                   year.use = 30,
                                   seed = i + 12345)
````
![EM4](https://github.com/lichengxue/multiwham_mse/assets/43245366/61a0a09b-cf47-4520-8377-9770b94fee10)

EM5: Use one model that consists of multiple stocks with Rec treated as random effects (no movement) 
```r
# Here we have 2 stocks, both of which have Rec re. Note that the NAA_re is now Rec only! 
NAA_re <- list(N1_model = c("equilibrium","equilibrium"),
                     sigma    = list("rec","rec"),
                     cor      = list("iid","iid"))
looped_res = loop_through_fn(om = stock_om, 
                                   M_em = M, 
                                   sel_em = sel, 
                                   NAA_re_em = NAA_re, 
                                   move_em = NULL,
                                   em.opt = list(separate.em = FALSE, separate.em.type = 3, do.move = FALSE, est.move = FALSE),
                                   assess_years = assess.years, 
                                   assess_interval = assess.interval, 
                                   base_years = base.years,
                                   year.use = 30,
                                   seed = i + 12345)

````
![EM5](https://github.com/lichengxue/multiwham_mse/assets/43245366/7cec044b-1a5a-4a4e-952c-9899890b1932)

EM6: Use one model that consists of multiple stocks with movement rate being fixed as known (correct model)
```r
looped_res = loop_through_fn(om = stock_om, 
                                 M_em = M, 
                                 sel_em = sel, 
                                 NAA_re_em = NAA_re, 
                                 move_em = move,
                                 em.opt = list(separate.em = FALSE, separate.em.type = 3, do.move = TRUE, est.move = FALSE),
                                 assess_years = assess.years, 
                                 assess_interval = assess.interval, 
                                 base_years = base.years,
                                 year.use = 30,
                                 seed = i + 12345)
````
![EM6](https://github.com/lichengxue/multiwham_mse/assets/43245366/18a77edd-1808-4d1c-af89-ac2d613701dc)

EM7: Use one model that consists of multiple stocks with movement rate being estimated using a prior and a sigma
```r
looped_res = loop_through_fn(om = stock_om, 
                                 M_em = M, 
                                 sel_em = sel, 
                                 NAA_re_em = NAA_re, 
                                 move_em = move,
                                 em.opt = list(separate.em = FALSE, separate.em.type = 3, do.move = TRUE, est.move = TRUE),
                                 assess_years = assess.years, 
                                 assess_interval = assess.interval, 
                                 base_years = base.years,
                                 year.use = 30,
                                 seed = i + 12345)
````
![EM7](https://github.com/lichengxue/multiwham_mse/assets/43245366/1acb01a4-5d52-4d2f-abe7-a5cdf6ee9c20)
### 15. Performance metrics
Users can use the plot_mse_output function to generate plots of performance metrics such as SSB, F, catch, etc. 
Users can use the plot_wham_output function to generate the output (e.g. parameter estimates, diagnostic results, estimated management quantities, etc) from each assessment model.
#### 1. Performance metrics from one realization (line plot)
![Performance_mod1](https://github.com/lichengxue/multiwham_mse/assets/43245366/d3b00f4e-1621-468c-b6b9-3eb8936fe563)

#### 2. Performance metrics summarized from one realizations (boxplot, only feedback period included)
![Performance2_mod1](https://github.com/lichengxue/multiwham_mse/assets/43245366/51037b79-9fcf-4f56-8804-c909e9b96bb9)


#### 3. Performance metrics summarized from all realizations (boxplot, only feedback period included)
![Performance_all](https://github.com/lichengxue/multiwham_mse/assets/43245366/9bf27ecf-c31e-4b69-92e1-30ac6fce2d8e)

#### 4. Performance metrics summarized from all realizations (boxplot, only last 10 years of feedback period included)
![Performance_last10](https://github.com/lichengxue/multiwham_mse/assets/43245366/941163e2-d55c-4dd7-ace1-01ea0ca6b07a)


