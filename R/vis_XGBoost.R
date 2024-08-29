#' Visualization of the power estimations from XGBoost
#'
#' This function takes the result from 'pred_XGboost' and plots 2D/3D views of it,
#'
#' @usage vis_XGBoost(x,view='2D',legend_name='Power',
#' xlab='avg_log2FC_abs',ylab='mean_pct')
#' @param x The result dataframe from 'pred_XGboost'.
#' @param view determines plot 2D/3D view, default='2D'.
#' @param legend_name The name of legend, default='Power'.
#' @param xlab The name of xlab, default='avg_log2FC_abs'.
#' @param ylab The name of ylab, default='mean_pct'.
#' @importFrom ggplot2 ggplot
#'
#' @return A 2D/3D plot of the power results from XGBoost.
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
#' vis_XGBoost(pred,view='2D',legend_name='Power',xlab='avg_log2FC_abs',ylab='mean_pct')
#'
#'
#' @author Lan Shui \email{lshui@@mdanderson.org}
#'
vis_XGBoost <- function(x,view='2D',legend_name='Power',xlab='avg_log2FC_abs',ylab='mean_pct')
{
  if (!view %in% c("2D","3D"))
  {stop("The view name must be one of '2D' or '3D'.")}

  if (!is.character(view))
  {stop("The view name should be a character.")}

  txt <- paste('data<-dplyr::data_frame(',ylab,'=x$avg_PCT,replicates=x$replicates,',
               xlab,'=x$avg_log2FC_abs,',legend_name,'=x$power_est)')
  eval(parse(text=txt))

  txt2 <- paste('mtplot = ggplot2::ggplot(data) +
      ggplot2::geom_point(ggplot2::aes(x = ', xlab,', y = ', ylab,', color = ', legend_name,')) +
      ggplot2::scale_color_continuous()')
  eval(parse(text=txt2))

  if (view=='2D'){
    txt3<-'rayshader::plot_gg(mtplot, width = 3.5, raytrace = FALSE, preview = TRUE)'
    eval(parse(text=txt3))
  }
  else{
    txt4<-'rayshader::plot_gg(mtplot, width = 3.5, multicore = TRUE, windowsize = c(800, 800),
            zoom = 0.85, phi = 45, theta = 30, sunangle = 225)'
    eval(parse(text=txt4))
    rayshader::render_snapshot(clear = TRUE)
  }
}

