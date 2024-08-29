#' Power value prediction
#'
#' This function provides the prediction from the \href{https://CRAN.R-project.org/package=Seurat}{Seurat}
#' object which could be used for visualization by 'plotly_powerest' and 'vis_powerest' or
#' the power result for your proposal or research. And it is a modified version of the scam library code predict.scam.
#'
#' @usage pred_powerest(x,n.grid=30,xlim=NULL,ylim=NULL)
#' @param x A \href{https://CRAN.R-project.org/package=Seurat}{Seurat} object.
#' @param n.grid The grid note number within 'xlim' and 'ylim', default=30.
#' @param xlim The range of the absolute value of log2FC used for prediction, default=NULL which means the original range.
#' @param ylim The range of the avg_pct used for prediction, default=NULL which means the original range.
#'
#' @return The prediction values of the power.
#' @export
#'
#' @examples
#' data(result_example)
#'  \donttest{b<-fit_powerest(result_example$power,result_example$avg_logFC,result_example$avg_PCT)}
#'  \donttest{pred <- pred_powerest(b,xlim= c(0,6),ylim=c(0,1))}
#'
#' @author Lan Shui \email{lshui@@mdanderson.org} based partly on 'scam' by Natalya Pya

pred_powerest<- function(x,n.grid=30,xlim=NULL,ylim=NULL)

  ## hacked version of vis.scam() from scam package, (c) Natalya Pya 24/2/23
  ## Note: the scam package deals with variables being factors, in our case, we don't have variables as factors
{
  view=NULL
  cond=list()
  v.names  <- names(x$var.summary)

  if (is.null(view)) # get default view if none supplied
  {
    k <- 0;view <- rep("",2)
    for (i in 1:length(v.names)) {
      ok <- TRUE
      if (is.matrix(x$var.summary[[i]])) ok <- FALSE else
        if (is.factor(x$var.summary[[i]])) {
          if (length(levels(x$var.summary[[i]]))<=1) ok <- FALSE
        } else {
          if (length(unique(x$var.summary[[i]]))==1) ok <- FALSE
        }
      if (ok) {
        k <- k + 1;view[k] <- v.names[i]
      }
      if (k==2) break;
    }
    if (k<2) stop("Model does not seem to have enough terms to do anything useful")
  } else {
    if (sum(view%in%v.names)!=2) stop(
      paste(c("view variables must be one of",v.names),collapse=", "))
    for (i in 1:2)
      if  (!inherits(x$var.summary[[view[i]]],c("numeric","factor")))
        stop("Don't know what to do with parametric terms that are not simple numeric or factor variables")
  }

  # Make dataframe....
  if (is.null(xlim))
  {r1<-range(x$var.summary[[view[1]]]);m1<-seq(r1[1],r1[2],length=n.grid)}
  else {r1<-range(xlim);m1<-seq(r1[1],r1[2],length=n.grid)}

  if (is.null(ylim))
  {r2<-range(x$var.summary[[view[2]]]);m2<-seq(r2[1],r2[2],length=n.grid)}
  else {r2<-range(ylim);m2<-seq(r2[1],r2[2],length=n.grid)}

  v1<-rep(m1,n.grid);v2<-rep(m2,rep(n.grid,n.grid))

  newd <- data.frame(matrix(0,n.grid*n.grid,0)) ## creating prediction data frame full of conditioning values
  for (i in 1:length(x$var.summary)) {
    ma <- cond[[v.names[i]]]
    if (is.null(ma)) {
      ma <- x$var.summary[[i]]
      if (is.numeric(ma)) ma <- ma[2] ## extract median
    }
    if (is.matrix(x$var.summary[[i]])) newd[[i]] <- matrix(ma,n.grid*n.grid,ncol(x$var.summary[[i]]),byrow=TRUE)
    else newd[[i]]<-rep(ma,n.grid*n.grid)
  }
  names(newd) <- v.names

  newd[[view[1]]]<-v1
  newd[[view[2]]]<-v2

  fv <- scam::predict.scam(x,newdata=newd,se.fit=TRUE)
  z <- fv$fit

  ###transfer back z, we fit the logit(z) with the parameters and transfer back to ensure the z (power) is between 0 and 1
  z <- exp(z)/(1+exp(z))
  z<-matrix(z,n.grid,n.grid)

  return(list(x=m1,y=m2,z=z))
}

