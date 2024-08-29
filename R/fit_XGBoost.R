#' Fit with XGBoost
#'
#' This function estimates the power values based on XGBoost under 3-dimensional monotone constraints upon avg_log2FC, avg_PCT
#' and replicates. This function is recommended when there exist crossings between power surfaces fitted by 'fit_powerest' and
#' used for estimating local power values.
#'
#' @usage fit_XGBoost(power,avg_log2FC,avg_PCT,replicates,filter_zero=TRUE,
#' max.depth=6,eta=0.3,nround=100)
#' @param power The raw power values.
#' @param avg_log2FC The corresponding log2FC values.
#' @param avg_PCT The corresponding PCT values.
#' @param replicates The corresponding replicates number.
#' @param filter_zero Whether the user would like to filter to remove the power values being 0. Default=TRUE.
#' @param max.depth Maximum depth of a tree. Default=6.
#' @param eta control the learning rate: scale the contribution of each tree by a factor of 0 < eta < 1 when it is added to the current approximation. Used to prevent overfitting by making the boosting process more conservative. Default=0.3.
#' @param nround Max number of boosting iterations.
#'
#' @return A object of class 'xgb.Booster'. More information about the content of a 'xgb.Booster' object can be found
#' at the document of R package \href{https://CRAN.R-project.org/package=xgboost}{xgboost}.
#' @export
#'
#' @examples
#' data(power_example)
#' # Fit the local power surface of avg_log2FC_abs between 1 and 2
#' avg_log2FC_abs_1_2<-dplyr::filter(power_example,avg_log2FC_abs>1 & avg_log2FC_abs<2)
#' # Fit the model
#' bst<-fit_XGBoost(power_example$power,avg_log2FC=power_example$avg_log2FC_abs,
#' avg_PCT=power_example$mean_pct,replicates=power_example$sample_size)
#'
#' @author Lan Shui \email{lshui@@mdanderson.org}

fit_XGBoost <- function(power,avg_log2FC,avg_PCT,replicates,filter_zero=TRUE,max.depth=6,eta=0.3,nround=100)
{
  data<-data.frame(power=power,avg_log2FC=avg_log2FC,avg_PCT=avg_PCT,replicates=replicates)
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

  #fit the model
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(data[,c(3:5)]), label=data$logitpower)
  monotonic_constraints <- c(1,1,1)
  bst <- xgboost::xgboost(data = dtrain,
                 max.depth = max.depth,
                 eta = eta,
                 monotone_constraints = monotonic_constraints,
                 nround = nround,
                 objective = "reg:linear",
                 verbose = 1)

  return(bst)
}
