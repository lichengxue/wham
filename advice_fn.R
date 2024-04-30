#' Generate catch advice
#' 
#' a function to generate catch advice for management strategy evaluation. 
#' 
#' @param em estimation model
#' @param pro.yr Number of years in projection
#' 
#' @return a list of catch adive
#'   
#' @export
#'
#' @seealso \code{\link{project_wham}}
#'

advice_fn = function(em, pro.yr = assess.interval){
  proj_opts = list(n.yrs=pro.yr, use.FXSPR=TRUE, avg.yrs=tail(em$years,5),percentFXSPR=75, percentSPR=40) 
  em_proj = project_wham(em, proj.opts = proj_opts, MakeADFun.silent=TRUE) #projected version of the em
  advice = em_proj$rep$pred_catch[length(em_proj$years) + 1:pro.yr,] 
  return(advice)
}
