#' Fit the power surface
#'
#' This function loads the power values with corresponding avg_log2FC and avg_PCT derived from bootstrap sampling
#' and utilizes the \href{https://CRAN.R-project.org/package=scam}{scam} package to fit two dimensional
#' smoothing splines under monotone constraints: 1.positive relationship between power and avg_log2FC;
#' 2.positive relationship between power and avg_PCT. The values of avg_log2FC and avg_PCT can be either
#' from the averages of the bootstrap samples or from the original spatial transcriptomics data.
#'
#' @usage fit_powerest(power,avg_log2FC,avg_PCT,filter_zero=TRUE)
#' @param power The raw power values.
#' @param avg_log2FC The corresponding log2FC values.
#' @param avg_PCT The corresponding PCT values.
#' @param filter_zero Whether the user would like to filter to remove the power values being 0, default=TRUE.
#'
#' @return A 'scam' object is the result of scam function. More information about the content of a 'scam' object can be found
#' at the document of R package \href{https://CRAN.R-project.org/package=scam}{scam}.
#' @export
#'
#' @examples
#' data(result_example)
#'  \donttest{b<-fit_powerest(result_example$power,result_example$avg_logFC,result_example$avg_PCT)}
#' @author Lan Shui \email{lshui@@mdanderson.org}
#'
fit_powerest <- function(power,avg_log2FC,avg_PCT,filter_zero=TRUE)
  # This function relies on the SCAM function which uses smoothing splines to fit the power with monotone constrains.
{
  data<-data.frame(power=power,avg_log2FC=avg_log2FC,avg_PCT=avg_PCT)
  if (filter_zero==TRUE)
  {data<-dplyr::filter(data,power!=0)}

  #we take the absolute value of log2FC
  data$avg_log2FC_abs<-abs(data$avg_log2FC)

  #transfer the power value through logit function
  data$power_zero<-ifelse(data$power==0,1,0)
  data$power_one<-ifelse(data$power==1,1,0)
  data$power[data$power_zero==1]<-0.00001
  data$power[data$power_one==1]<-0.99999
  data$logitpower<-log(data$power/(1-data$power))

  b <- scam::scam(logitpower~s(avg_log2FC_abs, avg_PCT, bs="tedmi"), data=data)
  return(b)
}
