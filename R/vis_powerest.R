#' Visualization of the power surface
#'
#' This function takes the result from 'pred_powerest' and plots 2D views of it,
#' supply ticktype="detailed" to get proper axis annotation and is a modified version of the 'scam' library code 'vis.scam'.
#' @usage vis_powerest(x,color="heat",contour.col=NULL,
#' se=-1,zlim=NULL,n.grid=30,col=NA,plot.type="persp",
#' nCol=50,...)
#' @param x A \href{https://CRAN.R-project.org/package=scam}{scam} object.
#' @param color The color of the plot which can be one of the "heat", "topo", "cm", "terrain", "gray" or "bw".
#' @param contour.col The color of the contour plot when using plot.type="contour".
#' @param se If less than or equal to zero then only the predicted surface is plotted, but if
#' greater than zero, then 3 surfaces are plotted, one at the predicted values minus se standard errors,
#' one at the predicted values and one at the predicted values plus se standard errors.
#' @param zlim The range of power value the user want to show.
#' @param n.grid The number of grid nodes in each direction used for calculating the plotted surface.
#' @param col The colors for the facets of the plot. If this is NA then if se>0 the facets are transparent,
#' otherwise the color scheme specified in color is used. If col is not NA then it is used as the facet color.
#' @param plot.type One of "contour" or "persp".
#' @param nCol The number of colors to use in color schemes.
#' @param ... Other arguments.
#' @importFrom grDevices cm.colors gray heat.colors terrain.colors topo.colors
#' @importFrom graphics par
#' @return A 2d plot of the power surface. More details can be seen at \href{https://CRAN.R-project.org/package=scam}{scam}.
#' @export
#'
#' @examples
#' data(result_example)
#'  \donttest{b<-fit_powerest(result_example$power,result_example$avg_logFC,result_example$avg_PCT)}
#'  \donttest{pred <- pred_powerest(b,xlim= c(0,6),ylim=c(0,1))}
#'  \donttest{vis_powerest(pred,theta=-30,phi=30,color='heat',ticktype = "detailed",xlim=c(0,6),nticks=5)}
#'
#' @author Lan Shui \email{lshui@@mdanderson.org} based partly on 'scam' by Natalya Pya

vis_powerest <- function(x,color="heat",contour.col=NULL,se=-1,zlim=NULL,n.grid=30,col=NA,plot.type="persp",nCol=50,...)
  ## hacked version of vis.scam() from scam package, (c) Natalya Pya 24/2/23
{
  dnm <- names(list(...))
  view<-NULL
  view[1]<-'avg_log2FC'
  view[2]<-'avg_pct'
  zlab<-"power"

  if (se<=0)
  {
    z<-x$z
    m1<-x$x
    m2<-x$y
    max.z <- max(z,na.rm=TRUE)
    suppressWarnings({
    av<-matrix(c(0.5,0.5,rep(0,n.grid-1)),n.grid,n.grid-1)
    })
    surf.col<-t(av)%*%z%*%av # average over tiles
    if (!is.null(zlim))
    { if (length(zlim)!=2||zlim[1]>=zlim[2]) stop("Something wrong with zlim")
      min.z<-zlim[1]
      max.z<-zlim[2]
    } else
    { min.z<-min(z,na.rm=TRUE)
    max.z<-max(z,na.rm=TRUE)
    }
    surf.col<-surf.col-min.z
    surf.col<-surf.col/(max.z-min.z)
    surf.col<-round(surf.col*nCol)
    con.col <-1
    if (color=="heat") { pal<-heat.colors(nCol);con.col<-3;}
    else if (color=="topo") { pal<-topo.colors(nCol);con.col<-2;}
    else if (color=="cm") { pal<-cm.colors(nCol);con.col<-1;}
    else if (color=="terrain") { pal<-terrain.colors(nCol);con.col<-2;}
    else if (color=="gray"||color=="bw") {pal <- gray(seq(0.1,0.9,length=nCol));con.col<-1}
    else stop("color scheme not recognised")
    if (is.null(contour.col)) contour.col<-con.col   # default colour scheme
    surf.col[surf.col<1]<-1;surf.col[surf.col>nCol]<-nCol # otherwise NA tiles can get e.g. -ve index
    if (is.na(col)) col<-pal[as.array(surf.col)]
    if (plot.type=="contour")
    { stub <- paste(ifelse("xlab" %in% dnm, "" , ",xlab=view[1]"),
                    ifelse("ylab" %in% dnm, "" , ",ylab=view[2]"),
                    ifelse("main" %in% dnm, "" , ",main=zlab"),",...)",sep="")
    if (color!="bw")
    { txt <- paste("image(m1,m2,z,col=pal,zlim=c(min.z,max.z)",stub,sep="") # assemble image() call
    eval(parse(text=txt))
    txt <- paste("contour(m1,m2,z,col=contour.col,zlim=c(min.z,max.z)",
                 ifelse("add" %in% dnm, "" , ",add=TRUE"),",...)" , sep="") # assemble contour() call
    eval(parse(text=txt))
    } else
    { txt <- paste("contour(m1,m2,z,col=1,zlim=c(min.z,max.z)",stub,sep="")  # assemble contour() call
    eval(parse(text=txt))
    }
    } else
    { stub <- paste(ifelse("xlab" %in% dnm, "" , ",xlab=view[1]"),
                    ifelse("ylab" %in% dnm, "" , ",ylab=view[2]"),
                    ifelse("main" %in% dnm, "" , ",zlab=zlab"),",...)",sep="")
    if (color=="bw")
    { op <- par(bg="white")
    txt <- paste("persp(m1,m2,z,col=\"white\",zlim=c(min.z,max.z) ",stub,sep="") # assemble persp() call
    eval(parse(text=txt))
    par(op)
    on.exit(par(op))
    } else
    { txt <- paste("persp(m1,m2,z,col=col,zlim=c(min.z,max.z)",stub,sep="")  # assemble persp() call
    eval(parse(text=txt))
    }
    }
  }
}
