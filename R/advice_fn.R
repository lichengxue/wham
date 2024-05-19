#' Generate catch advice
#' 
#' a function to generate catch advice for management strategy evaluation. 
#' 
#' @param em Estimation model
#' @param pro.yr Number of years in projection
#' @param type Type of harvest control rule
#'   \itemize{
#'     \item \code{"1"} Annual projected catch based on 75% of F40% (default)
#'     \item \code{"2"} Constant catch based on 75% of F40%
#'     \item \code{"3"} "Hockey stick" catch based on stock status
#'     }
#'     
#' @return a list of catch advice
#'   
#' @export
#'
#' @seealso \code{\link{project_wham}}
#'

advice_fn = function(em, pro.yr = assess.interval, type = 1){
  proj_opts = list(n.yrs=pro.yr, use.FXSPR=TRUE, avg.yrs=tail(em$years,5),percentFXSPR=75, percentSPR=40) 
  em_proj = project_wham(em, proj.opts = proj_opts, MakeADFun.silent=TRUE) #projected version of the em
  if(type == 1) {
    advice = em_proj$rep$pred_catch[length(em_proj$years) + 1:pro.yr,] 
  }
  if(type == 2) {
    advice = em_proj$rep$pred_catch[length(em_proj$years) + 1:pro.yr,] 
    if(nrow(advice) != 1) {
      advice = colMeans(advice) #mean of the projected catch over the next 5 years fishing at F40
    }
    advice = matrix(rep(advice,pro.yr),ncol=length(advice), byrow = T)
  }
  if(type == 3) {
    if(ncol(em$rep$log_SSB_FXSPR) == ncol(em$rep$SSB)+1) {
      print("Global SPR is calculated in the model")
      SSB40 = exp(tail(em$rep$log_SSB_FXSPR,1))
      SSB40 = SSB40[,ncol(SSB40)]
      SSB_t = tail(em$rep$SSB,1)
      SSB_t = sum(SSB_t)
      max_percent = 75
      min_percent = 0.01
      BThresh_up = 0.5
      BThresh_low = 0.1
      if (SSB_t/SSB40 >= BThresh_up) {
        proj_opts = list(n.yrs=pro.yr, use.FXSPR=TRUE, avg.yrs=tail(em$years,5),percentFXSPR=max_percent, percentSPR=40) 
        em_proj = project_wham(em, proj.opts = proj_opts, MakeADFun.silent=TRUE) #projected version of the em
      } 
      if (SSB_t/SSB40 < BThresh_up & SSB_t/SSB40 > BThresh_low) {
        slope = (max_percent-min_percent)/(BThresh_up-BThresh_low)
        percent = slope * (SSB_t/SSB40 - BThresh_low) + min_percent
        proj_opts = list(n.yrs=pro.yr, use.FXSPR=TRUE, avg.yrs=tail(em$years,5),percentFXSPR=percent, percentSPR=40) 
        em_proj = project_wham(em, proj.opts = proj_opts, MakeADFun.silent=TRUE) #projected version of the em
      } 
      if (SSB_t/SSB40 <= BThresh_low) {
        percent = min_percent
        proj_opts = list(n.yrs=pro.yr, use.FXSPR=TRUE, avg.yrs=tail(em$years,5),percentFXSPR=percent, percentSPR=40) 
        em_proj = project_wham(em, proj.opts = proj_opts, MakeADFun.silent=TRUE) #projected version of the em
      } 
    }
  }
  return(advice)
}
