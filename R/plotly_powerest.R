#' 3D interactive visualization
#'
#' This function creates 3d interactive plot of the power against other parameters based on 'plot_ly'.
#' @usage plotly_powerest(pred,opacity=0.8,colors='BrBG',fig_title=NULL)
#' @param pred The result from 'pred_powerest'.
#' @param opacity The opacity of the graph, default=0.8.
#' @param colors The color for the graph, default='BrBG'.
#' @param fig_title The title of the graph, default=NULL.
#' @return A 3d interactive plot of the power surface. Users can also plot multiple surfaces together to compare them.
#' @export
#'
#' @examples
#' data(result_example)
#'  \donttest{b<-fit_powerest(result_example$power,result_example$avg_logFC,result_example$avg_PCT)}
#'  \donttest{pred <- pred_powerest(b,xlim= c(0,6),ylim=c(0,1))}
#'  \donttest{plotly_powerest(pred,fig_title='Power estimation result')}
#'
#' @author Lan Shui \email{lshui@@mdanderson.org}

plotly_powerest <- function(pred,opacity=0.8,colors='BrBG',fig_title=NULL){
  fig <- plotly::plot_ly(x = pred$x, y = pred$y, z = pred$z,type = "surface",opacity = opacity,colors=colors)
  fig <- fig %>% plotly::layout(scene=list(xaxis = list(title = "Absolute value of logFC"),
                                   yaxis = list(title = "Mean Percentage"),
                                   zaxis = list(title = "Power")))
  fig <- fig %>% plotly::layout(coloraxis=list(range=c(0,1)),title=list(text=fig_title), legend=list(title="Power"))
  return(fig)
}
