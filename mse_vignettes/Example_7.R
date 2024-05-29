library(here)
library(wham) 

# Create a folder
sub.dir = "Example_7"
dir.create(here("wham", "mse_vignettes", sub.dir))

# Load ASAP3 file
data <- read_asap3_dat(here("wham","mse_vignettes","data",c("NORTH.MT.2021.FINAL_less.DAT","SOUTH.MT.2021.FINAL_less.DAT")))

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
input <- prepare_wham_input(data, NAA_re = NAA_re, basic_info = basic_info, selectivity = sel, move = move)

# Fix movement rate without estimating it in the model 
temp <- array(as.integer(input$map$trans_mu), dim = dim(input$par$trans_mu))
temp[] <- NA
input$map$trans_mu <- factor(temp)
input$fleet_names = paste0(rep(c("North_", "South_"),each = 2), c("Trawl", "Non-Trawl"))

# Model fitting
mod <- fit_wham(input, do.retro = F, do.osa = F, do.sdrep = T, do.brps = T)
saveRDS(mod, here("wham", "mse_vignettes", sub.dir, "mod.RDS"))
plot_wham_output(mod, out.type = "html", dir.main = here("wham","mse_vignettes", sub.dir))

# wham:::make_html_figs_tables_fn(od = here("wham","results",mod_name))

# Now, do projection with random effects treated 
mod$input$data$agg_catch
mod$rep$NAA_devs[1,,,]

mod$input$random = NULL
mod$fn()
mod$rep$NAA_devs

pro.yr = 3
proj_opts = list(n.yrs=pro.yr, use.FXSPR=TRUE, avg.yrs=tail(mod$years,5),percentFXSPR=75, percentSPR=40) 
mod.proj = project_wham(mod,proj.opts = proj_opts)
mod$rep$log_FXSPR
