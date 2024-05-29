## Example 3: Self-weighting likelihoods

### 1. Create a folder to save your results
```r
main.dir = here::here()
# main.dir = "where/you/save/your/wham/package"

# install.packages(file.path(main.dir,"wham"), dependencies = TRUE, repos = NULL, type = "source")
# devtools::install_local(file.path(main.dir,"wham"), dependencies = TRUE)

library(wham)
folder.name = "Example_3"
sub.dir <- folder.name
if (file.exists(sub.dir)){
  setwd(file.path(main.dir,sub.dir))
} else {
  dir.create(file.path(main.dir,sub.dir))
  setwd(file.path(main.dir,sub.dir))
}
````
### 2. Generate basic information 
The operating model is generated based on user-specified biological and fishery information. Here it is worthnoting that the longer burn-in/feedback period you set, the longer runtime it may take to generate your operating model.The "generate_basic_info" function is used to create a list of biological and fishery information that can be used for generating a wham input. 

```r
year_start  <- 2003  # starting year in the burn-in period
year_end    <- 2022  # end year in the burn-in period
MSE_years   <- 3     # number of years in the feedback loop
# Note: MSE_years set to be 3 for simplicity 
````

To avoid the convergence issue, users should set high contrast in fishing, low recruitment variability when using B-H or Ricker S-R relationship.

```r
# Have a high contrast in fishing when using self-weighting likelihoods
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
                                  F_info     = list(F.year1 = 0.2, Fhist = "updown",Fmax = 1, Fmin = 0.2, change_time = 0.5), 
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
### 6. Generate wham input
```r
input <- prepare_wham_input(basic_info = basic_info, 
                            selectivity = sel, 
                            M = M, 
                            NAA_re = NAA_re, 
                            move = move,
                            age_comp = "logistic-normal-miss0")
````

### 7. Assign weights based on mean recruitment to calculate global SPR-based reference points
The SPR-based biological reference point in multi-wham is a weighted average based on the mean recruitment of each stock. The default is SPR(stock1) and SPR(stock2) are equally weighted. But users should change SPR weights if the mean recruitment for each stock is different. This step is only needed when generating the operating model. In the feedback loop, the weights will be automatically calculated given the mean recruitment estimated from the assessment model.
```r
# Global SPR is calculated based on weights of mean rec par 
input$data$SPR_weight_type <- 1
input$data$SPR_weights     <- c(2/3,1/3)
input$data$do_SPR_BRPs     <- 1
````

### 8. Generate the operating model
```r
om = fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE) 
saveRDS(om,"om.RDS") # save the OM 
````

### 9. Self test
```r
sim_fn <- function(om, self.fit = FALSE){
  input <- om$input
  input$data <- om$simulate(complete=TRUE)
  if(self.fit) {
    fit <- fit_wham(input, do.osa = FALSE, do.retro = FALSE, MakeADFun.silent = FALSE)
    return(fit)
  } else return(input)
}
set.seed(12345)
self_sim_fit <- sim_fn(om, self.fit = TRUE)
````

### 10. Generate datasets
```r
data <- generate_data(om, seed = 123)
````
### 11. Specify assessment interval and assessment year in the feedback loop
Users can specify the assessment interval for the feedback period. For medium-lived groundfish stock, an assessment interval of 3 years is typically common in the northeast region. It should be noted that the shorter assessment interval, the longer runtime it may take for the whole feedback period.
```r
assess.interval <- 3 # Assessment interval
base.years      <- year_start:year_end # Burn-in period
first.year      <- head(base.years,1)
terminal.year   <- tail(base.years,1)
assess.years    <- seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)
````
### 12. EM: Separate panmictic assessment models with NAA random effects
Fit separate assessment models for each stock like traditional single-stock assessment
![EM1](https://github.com/lichengxue/wham/blob/mse/mse_vignettes/Vignettes_figs/EM1.png)
```r
n_stocks = n_regions = n_fleets = n_indices = 1
sel_em <- list(model=rep("logistic",n_fleets+n_indices),
               initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
               fix_pars=rep(list(NULL),n_fleets+n_indices))
NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))

mod = loop_through_fn(om = data, 
                      M_om = M,
                      sel_om = sel, 
                      NAA_re_om = NAA_re, 
                      mean_rec_weights = c(2/3,1/3),
                      move_om = move,
                      age_comp_om = "logistic-normal-miss0",
                      M_em = M_em, 
                      sel_em = sel_em, 
                      NAA_re_em = NAA_re_em, 
                      move_em = NULL,
                      em.opt = list(separate.em = TRUE, separate.em.type = 1, do.move = FALSE, est.move = FALSE),
                      age_comp_em = "logistic-normal-miss0",
                      assess_years = assess.years, 
                      assess_interval = assess.interval, 
                      base_years = base.years,
                      year.use = 10, # number of years of data you want to use in the assessment model, default is using the data from all years (runtime can be long)
                      seed = 123,
                      save.sdrep = TRUE)
saveRDS(mod,"mod.RDS") # save the result

# plot SSB time series for stock 1
plot(mod$om$rep$SSB[,1], type = "o")

# plot pred catch time series for region 1
plot(mod$om$rep$pred_catch[,1], type = "o")

# check em1 and em2
em1 <- mod$em_full[[1]][[1]]
check_convergence(em1)
plot_wham_output(em1, out.type = "html")

em2 <- mod$em_full[[1]][[2]]
check_convergence(em2)
plot_wham_output(em2, out.type = "html")
````