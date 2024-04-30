# Function to calculate F given the catch 
get_F_from_catch <- function(om, year, catch, Finit = 0.1, maxF = 10){ # here year has to be year starting from 1
  
  get_catch = function(log_F, naa, sel, waa, Maa){
    Faa = exp(log_F) * sel_tot
    Zaa = Maa + Faa
    Catch = 0
    for(a  in 1:length(naa)) Catch = Catch + waa[a] * naa[a] * Faa[a] *(1 - exp(-Zaa[a]))/Zaa[a];
    return(Catch)
  }
  
  rep = om$rep
  n_regions = length(om$input$region_names)
  Fsolve = NULL
  for (r in 1:n_regions) {
    naa = colSums(rep$NAA[,r,year,]) # n_stocks x n_regions x n_years x n_ages
    Maa = colMeans(rep$MAA[,r,year,])
    sel_tot <- rep$FAA[r,year,]/max(rep$FAA[r,year,])
    waa = om$input$data$waa[om$input$data$waa_pointer_totcatch, year,][r,]
    
    obj = function(log_F) (catch[r] - get_catch(log_F, naa, sel_tot, waa, Maa))^2
    opt = try(nlminb(log(Finit), obj))
    if(!is.character(opt)) Fsolve.tmp = exp(opt$par)[1] else Fsolve.tmp = maxF
    if(Fsolve.tmp>10) Fsolve.tmp = maxF
    Fsolve[r] <- Fsolve.tmp
  }
  cat(paste0("\nFsolve: ", Fsolve,"\n"))
  return(Fsolve)
}
