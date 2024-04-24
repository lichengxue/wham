# Function to calculate catch advice
advice_fn = function(em, pro.yr = assess.interval){
  #make 5 year projections using F40. Use average SSB/R and YPR inputs over most recent 5 years
  proj_opts = list(n.yrs=pro.yr, use.FXSPR=TRUE, avg.yrs=tail(em$years,5),percentFXSPR=75, percentSPR=40) 
  em_proj = project_wham(em, proj.opts = proj_opts, MakeADFun.silent=TRUE) #projected version of the em
  advice = em_proj$rep$pred_catch[length(em_proj$years) + 1:pro.yr,] 
  return(advice)
}