library(here)
library(wham) 

# Create a folder
sub.dir = "Example_7"
# dir.create(here("wham", "mse_vignettes", sub.dir))
source("C:/Users/chengxue.li/read_asap3_dat2.R")
source("C:/Users/chengxue.li/wham/R/reduce_input.R")
# Load ASAP3 file
asap3 <- read_asap3_dat(here("wham","mse_vignettes","data",c("NORTH.MT.2021.FINAL_less.DAT","SOUTH.MT.2021.FINAL_less.DAT")))

asap3 <- read_asap3_dat2(here("wham","mse_vignettes","data",c("NORTH.MT.2021.FINAL_less.DAT","SOUTH.MT.2021.FINAL_less.DAT")),add_years = 3)

# Configure selectivity
sel = list(model = rep(c("logistic","age-specific"),c(8,6)))
sel$initial_pars <- c(
  rep(list(c(5,1)),8), #north and south fleets
  list( 
    c(rep(c(0.5,1), c(2,6))), #9 begin north indices
    c(rep(c(0.5,1), c(1,7))), #15
    c(rep(c(0.5,1), c(2,6))), #16 end north indices
    c(rep(0.5,3), rep(1,5)), #17 begin south indices
    c(rep(0.5,2), rep(1,6)), #24
    c(rep(0.5,1), rep(1,7))) #25 end south indices
)
sel$fix_pars <- c(
  rep(list(NULL),8), #north and south fleets
  list(
    3:8, #9 begin north indices
    2:8, #15
    3:8, #16 end north indices
    4:8, #17 begin south indices
    3:8, #24
    2:8) #25 end south indices
)

# Generate basic info based on the ASAP file
basic_info <- list(region_names = c("North", "South"), stock_names = paste0("BSB_", c("North", "South"))) #, NAA_where = array(1, dim = c(2,2,6)))

NAA_re <- list(N1_model = c("equilibrium","equilibrium"))
NAA_re$sigma <- list("rec+1","rec+1")
NAA_re$cor <- list("iid","iid")

# Configure movement pattern and movement rate
seasons = c(rep(1,5),2,rep(1,5))/12
basic_info$fracyr_seasons <- seasons
basic_info$NAA_where <- array(1, dim = c(2,2,8))
basic_info$NAA_where[1,2,1] = 0
basic_info$NAA_where[2,1,] = 0
move = list(stock_move = c(TRUE,FALSE), separable = TRUE) #north moves, south doesn't
move$must_move = array(0,dim = c(2,length(seasons),2))
#if north stock in region 2 (south) must move back to region 1 (north) at the end of interval 5 right before spawning

move$must_move[1,5,2] <- 1 
move$can_move = array(0, dim = c(2,length(seasons),2,2))
move$can_move[1,c(1:4,7:11),,] <- 1 #only north stock can move and in seasons prior to spawning and after spawning
move$can_move[1,5,2,] <- 1 #north stock can (and must) move in last season prior to spawning back to north 
move$mean_vals <- array(0.1, dim = c(2,length(seasons),2,1))

# Generate wham input
input <- prepare_wham_input(asap3, NAA_re = NAA_re, basic_info = basic_info, selectivity = sel, move = move)

# Fix movement rate without estimating it in the model 
temp <- array(as.integer(input$map$trans_mu), dim = dim(input$par$trans_mu))
temp[] <- NA
input$map$trans_mu <- factor(temp)
input$fleet_names = paste0(rep(c("North_", "South_"),each = 2), c("Trawl", "Non-Trawl"))

om <- fit_wham(input, do.retro = F, do.osa = F)
saveRDS(om,"om.RDS")
# Model fitting
# mod <- readRDS("C:\\Users\\chengxue.li\\wham\\mse_vignettes\\Example_7\\mod.RDS")
# check_convergence(mod)

# n.yrs.peel = 3
# mod <- fit_wham(input, do.fit = FALSE, do.retro = F, do.osa = F, do.sdrep = T, do.brps = T)
# mod.1 <- retro(mod, ran = unique(names(mod$env$par[mod$env$random])), n.peels=n.yrs.peel, save.input = T)
# mod <- fit_peel(3, input, do.sdrep = FALSE, n.newton = 3, MakeADFun.silent = FALSE, retro.silent = FALSE, save.input = TRUE)
# mod$input

assess.interval = 3
input <- reduce_input(input, tail(input$years,peel = assess.interval))

# Assessment model configuration
# NAA_re_em <- list(N1_model = c("equilibrium","equilibrium"))
# NAA_re_em$sigma <- list("rec+1","rec+1")
# NAA_re_em$cor <- list("iid","iid")

# temp <- set_NAA(temp, NAA_re = NAA_re_em)
mod <- TMB::MakeADFun(input$data, input$par, DLL="wham", random = input$random, map = input$map, silent = FALSE)

mod$input <- list(data = input$data, par = input$par, map = input$map, random = input$random)

source("C:/Users/chengxue.li/check_which_F_age.R")
mod$years <- input$years
mod$years_full <- input$years_full
mod$ages.lab <- input$ages.lab
mod$model_name <- input$model_name
mod$input <- input
mod$call <- match.call()
mod$rep <- mod$report()
mod <- check_which_F_age(mod)
wham_commit <- packageDescription("wham")$GithubSHA1
mod$wham_commit <- ifelse(is.null(wham_commit), "local install", paste0("Github (timjmiller/wham@", wham_commit, ")")) 
wham_version <- packageDescription("wham")$Version
mod$wham_version <- paste0(wham_version, " / ", mod$wham_commit)

em <- fit_tmb(mod, do.sdrep = TRUE, n.newton = 3, do.check=FALSE)
saveRDS(em,"em.RDS")
em <- readRDS(("em.RDS"))
# em <- fit_wham(input = temp.mod)

conv = as.logical(1-em$opt$convergence)
pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)

if (!conv | !pdHess) warning("Assessment model is not converged!")
if (conv | pdHess) cat("Assessment model is converged!")

pro.yr = assess.interval
proj_opts = list(n.yrs=pro.yr, use.FXSPR=TRUE, percentFXSPR=75, percentSPR=40) 
mod = em
do.brps = TRUE
if(do.brps){
  if(any(mod$input$data$can_move==1) & any(mod$input$data$mig_type == 1)){
    warning("Cannot currently calculate standard errors of biological reference points internally when survival and movement are simultaneous for any stock.")
  } else {
    mod <- check_which_F_age(mod)
    mod$input$data$do_SPR_BRPs <- mod$env$data$do_SPR_BRPs <- 1
    if(any(input$data$recruit_model %in% 3:4)) mod$input$data$do_MSY_BRPs <- mod$env$data$do_MSY_BRPs <- 1
    mod$rep = mod$report() #par values don't matter because function has not been evaluated
    mod <- check_FXSPR(mod)
  }
}

proj.opts = proj_opts; MakeADFun.silent=TRUE
source("C:/Users/chengxue.li/wham/R/verify_version.R")
check.version = TRUE
input2 <- prepare_projection(mod, proj.opts, check.version = check.version)
# proj_mod <- TMB::MakeADFun(input2$data, input2$par, DLL = "wham", random = input2$random, map = input2$map, silent = FALSE)
# proj_mod <- TMB::MakeADFun(input$data, input$par, DLL="wham", random = input$random, map = input$map, silent = FALSE)

proj_mod <- TMB::MakeADFun(input2$data, input2$par, DLL = "wham", random = input2$random, map = input2$map, silent = FALSE)

em_proj = project_wham(mod, proj.opts = proj_opts, MakeADFun.silent=TRUE) #projected version of the em

advice = em_proj$rep$pred_catch[length(em_proj$years) + 1:pro.yr,] 

advice = rowSums(advice)

# Generate om2
asap3 <- read_asap3_dat(here("wham","mse_vignettes","data",c("NORTH.MT.2021.FINAL_less.DAT","SOUTH.MT.2021.FINAL_less.DAT")))
# Generate wham input
input <- prepare_wham_input(asap3, NAA_re = NAA_re, basic_info = basic_info, selectivity = sel, move = move)
# Fix movement rate without estimating it in the model 
temp <- array(as.integer(input$map$trans_mu), dim = dim(input$par$trans_mu))
temp[] <- NA
input$map$trans_mu <- factor(temp)
input$fleet_names = paste0(rep(c("North_", "South_"),each = 2), c("Trawl", "Non-Trawl"))

om2 <- fit_wham(input, do.retro = F, do.osa = F)

proj_opts = list(n.yrs=3, proj.catch = advice)
em_proj = project_wham(om2, proj.opts = proj_opts, MakeADFun.silent=TRUE)

updated_F = log(tail(em_proj$rep$Fbar,3))
cat(paste0("\nFsolve: ", exp(updated_F),"\n"))
year_ind = which(om$years %in% interval.info$years)
om$input$par$F_pars[year_ind,] = updated_F

# Generate wham input
input <- prepare_wham_input(asap3, NAA_re = NAA_re, basic_info = basic_info, selectivity = sel, move = move)

# Fix movement rate without estimating it in the model 
temp <- array(as.integer(input$map$trans_mu), dim = dim(input$par$trans_mu))
temp[] <- NA
input$map$trans_mu <- factor(temp)
input$fleet_names = paste0(rep(c("North_", "South_"),each = 2), c("Trawl", "Non-Trawl"))

em_proj$rep$Fbar












# generate catch advice
advice = advice_fn(em, pro.yr = assess.interval)
em_proj = project_wham2(em, proj.opts = proj_opts, MakeADFun.silent=TRUE) #projected version of the em

check_convergence(em)


for(m in 1:n.mods){
  mods[[m]]$peels <- retro(mods[[m]], ran = unique(names(mods[[m]]$env$par[mods[[m]]$env$random])), n.peels=n.yrs.peel, save.input = T)
  for(p in 3:n.yrs.peel) mods[[m]]$peels[[p]] <- project_wham(mods[[m]]$peels[[p]], proj.opts = list(n.yrs = n.yrs.proj, proj.F=rep(0.001,n.yrs.proj)))
}

mod <- fit_wham(input, do.retro = F, do.osa = F, do.sdrep = T, do.brps = T)
saveRDS(mod, here("wham", "mse_vignettes", sub.dir, "mod.RDS"))
plot_wham_output(mod, out.type = "html", dir.main = here("wham","mse_vignettes", sub.dir))

# wham:::make_html_figs_tables_fn(od = here("wham","results",mod_name))

# Now, do projection with random effects treated 
mod$input$data$agg_catch
mod$rep$NAA_devs[,,8,]

mod$input$random = NULL
mod$fn()
mod$rep$NAA_devs

pro.yr = 3
proj_opts = list(n.yrs=pro.yr, use.FXSPR=TRUE, avg.yrs=tail(mod$years,5),percentFXSPR=75, percentSPR=40) 
mod.proj = project_wham(mod,proj.opts = proj_opts)
mod.proj$rep$NAA_devs[,,8,]

mod.proj$rep
mod.proj$input$data$agg_catch
mod$input$data$agg_catch

tail(mod.proj$rep$pred_catch,pro.yr)


WAA <- mod$input$data$waa
mod$input$data$waa_pointer_fleets
mod$input$data$waa_pointer_ssb
mod$input$data$waa_pointer_M
mod$input$data$waa_pointer_indices

# Write a function to extract WAA, SAA, CAA, 
# n_indices = 2,n_fleets = 2, n_seasons = 11?

# base.years = model$years + mse years,
# n_ages = length(mod$ages.lab)?
# Fbar_ages = max(mod$input$data$Fbar_ages)?
# recruit_model ?
# q = 0.2, this has to change to a fleet-speficic
# F_info (F can be first automatically generated and then be modified to mod$rep$Fbar)
# catch_info 
mod$input$data$catch_paa
mod$input$data$catch_Neff
mod$input$data$agg_catch
mod$input$data$agg_catch_sigma
mod$input$data$agg_indices
mod$input$data$n_indices
mod$input$data$index_Neff
mod$input$data$use_indices
mod$input$data$use_catch_paa
mod$input$data$use_agg_catch
mod$input$data$use_index_paa
mod$input$data$units_index_paa


names(mod$input$data)

# we are going to add another proj.years into all arrays/matrices

# Extract the data from model
data = mod$input$data
# Focus on variable names associated with catch 
catch_names = grep("catch",names(data))
names(data)[catch_names]

# Focus on variable names associated with indice 
indice_names = grep("indice",names(data))
names(data)[indice_names]

# Focus on fracyr
fracyr_names = grep("fracyr",names(data))
names(data)[fracyr_names]

# Focus on pointer 
pointer_names = grep("pointer",names(data))
names(data)[pointer_names]
data[pointer_names]

# Focus on WAA
waa_names = grep("waa",names(data))
names(data)[waa_names]
data[waa_names]

# # Focus on MAA
waa_names = grep("maa",names(data))
names(data)[waa_names]
data[waa_names]

# Fleet and survey regions 
data$fleet_regions
data$index_regions
data$spawn_regions

# Focus on basic information 
n_stocks = n_regions = 2
n_ages = 8
Fbar_ages = 7

# selectivity and Catchability q should come from model fit 
selAA = mod$input$data$selblock_models
selAA = mod$input$data$selblock_models


WAA = data$waa



