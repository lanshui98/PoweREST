#' Prediction results from XGBoost
#'
#' This function takes the result from 'fit_XGBoost' and make predictions.
#'
#' @usage pred_XGBoost(x,n.grid=30,xlim,ylim,replicates)
#' @param x A object of class 'xgb.Booster'.
#' @param n.grid The grid note number within 'xlim' and 'ylim', default=30.
#' @param xlim The range of the absolute value of avg_log2FC used for prediction.
#' @param ylim The range of the avg_pct used for prediction.
#' @param replicates The replicates number.
#' @importFrom stats predict
#' @return The power estimations from XGBoost.
#' @export
#'
#' @examples
#' data(power_example)
#' # Fit the local power surface of avg_log2FC_abs between 1 and 2
#' avg_log2FC_abs_1_2<-dplyr::filter(power_example,avg_log2FC_abs>1 & avg_log2FC_abs<2)
#' # Fit the model
#' bst<-fit_XGBoost(power_example$power,avg_log2FC=power_example$avg_log2FC_abs,
#' avg_PCT=power_example$mean_pct,replicates=power_example$sample_size)
#' pred<-pred_XGBoost(bst,n.grid=30,xlim=c(0,1.5),ylim=c(0,0.1),replicates=3)
#'
#' @author Lan Shui \email{lshui@@mdanderson.org}

pred_XGBoost <- function(x,n.grid=30,xlim,ylim,replicates)
{
  r1<-range(xlim)
  r2<-range(ylim)

  #matrix to put in the prediction
  x0 = seq(r1[1],r1[2],length=n.grid)
  x1 = seq(r2[1],r2[2],length=n.grid)
  X_scan = matrix(data = NA, nrow = n.grid^2, ncol = 3)
  X_scan[,3] = rep(x0,n.grid)
  X_scan[,1] = rep(x1,rep(n.grid,n.grid))
  X_scan[,2] = rep(replicates,n.grid^2)

  X_plot = dplyr::as_tibble(X_scan)
  colnames(X_plot)<-c('avg_PCT','replicates','avg_log2FC_abs')

  X_plot = xgboost::xgb.DMatrix(as.matrix(X_plot))
  y_plot = predict(x, X_plot)
  y_plot = exp(y_plot)/(1+exp(y_plot))

  data<-dplyr::data_frame(avg_PCT=X_scan[,1],
                  replicates=X_scan[,2],
                  avg_log2FC_abs=X_scan[,3],
                  power_est=y_plot)
  return(data)
}
