#' Bootstrap resampling and power calculation upon ST data
#'
#' This function performs bootstrap resampling upon a Seurat subject under each condition
#' to resemble the real dataset which allows the exact power calculation, and perform DE analysis.
#' Users can specify the test they would like to perform for the DE analysis in '...' which should not contain
#' min.pct and logfc.threshold or other parameters attempt to pre-filter genes, as we specify min.pct and logfc.threshold as 0s
#' to calculate power for all the genes available. Therefore it may take one night to run if the ST data owns over thousands of genes.
#' To speed up this process, one may want to try function 'PoweREST_subset' where the pre-filter of genes are included in this process.
#' @usage PoweREST(Seurat_obj,cond,replicates=1,spots_num,
#' iteration=100,random_seed=1,pvalue=0.05,...)
#'
#' @param Seurat_obj A \href{https://CRAN.R-project.org/package=Seurat}{Seurat} object.
#' @param cond The name of the variable that indicates different conditions which is also stored in the
#' meta.data of the Seurat_obj and should be in character type.
#' @param replicates The number of sample replicates per group.
#' @param spots_num The number of spots per replicate.
#' @param iteration The number of iterations of the resampling.
#' @param random_seed To set a random seed.
#' @param pvalue The pvalue that will be considered significant.
#' @param ... DE test to use other than the default Wilcoxon test.
#' @importFrom Seurat FindMarkers
#'
#' @return A list of values containing the power, average log2FC and percentage of spots detecting the gene among
#' the resampling data, the replicate value and the spots number per slice specified by the user and corresponding genes' name.
#' @export
#'
#' @author Lan Shui \email{lshui@@mdanderson.org}

PoweREST <- function(Seurat_obj,cond,replicates=1,spots_num,iteration=100,random_seed=1,pvalue=0.05,...){
  dnm <- names(list(...))

  if (!is.character(cond))
  {stop("The condition name should be a character.")}

  if (!cond %in% colnames(Seurat_obj@meta.data))
  {stop("The condition name specified is not detected.")}

  txt<-paste0('if (length(unique(Seurat_obj@meta.data$',cond,'))!=2)
  {stop("The number of condition groups should be 2.")}')
  eval(parse(text=txt))

  set.seed(seed = random_seed)

  n1=iteration
  n2=spots_num*replicates

  txt<-paste0('names<-unique(Seurat_obj@meta.data$',cond,')')
  eval(parse(text=txt))
  name1<-names[1]
  name2<-names[2]

  location1<-NULL
  location2<-NULL
  #the location of the spots from each group
  txt<-paste0('location1<-which(Seurat_obj@meta.data$',cond,'==name1)')
  eval(parse(text=txt))

  txt<-paste0('location2<-which(Seurat_obj@meta.data$',cond,'==name2)')
  eval(parse(text=txt))

  #the spot number of the two groups
  spots_condition1<-length(location1)
  spots_condition2<-length(location2)

  sampling1<-resample::samp.bootstrap(spots_condition1, n1, size=n2)
  sampling2<-resample::samp.bootstrap(spots_condition2, n1, size=n2)

  txt<-paste("Seurat_obj[['PoweREST_Group']]<-0")
  eval(parse(text=txt))

  pvalue<-matrix(data=NA, nrow =dim(Seurat_obj@assays$Spatial@counts)[1] , ncol= 2)
  pvalue<-as.data.frame(pvalue)

  logFC<-matrix(data=NA, nrow =dim(Seurat_obj@assays$Spatial@counts)[1] , ncol= 2)
  logFC<-as.data.frame(logFC)

  PCT<-matrix(data=NA, nrow = dim(Seurat_obj@assays$Spatial@counts)[1], ncol= 2)
  PCT<-as.data.frame(PCT)

  for (i in 1:n1) {
    #transfer to the location of the spots in the Seurat object
    location_group1<-location1[sampling1[,i]]
    location_group2<-location2[sampling2[,i]]

    Seurat_obj[['PoweREST_Group']][[1]][c(location_group1,location_group2)]<-1

    #subset Seurat object
    txt2<-'Subset<-subset(x = Seurat_obj, subset = PoweREST_Group == 1)'
    eval(parse(text=txt2))

    if ('test.use' %in% dnm){
      txt<-paste('x<-Seurat::FindMarkers(Subset, ident.1 = name1, ident.2= name2,min.pct=0,logfc.threshold = 0,verbose=FALSE,...)')
      eval(parse(text=txt))
      }
    else{
      txt3<-'x<-Seurat::FindMarkers(Subset, ident.1 = name1, ident.2= name2,min.pct=0,logfc.threshold = 0,verbose=FALSE)'
      eval(parse(text=txt3))
    }

    #sort the data based on rowname since the output of FindMarkers will automatically be sorted according to pvalue
    x$rowname<-rownames(x)
    x<-x[order(x$rowname),]

    pvalue<-cbind(pvalue,x$p_val_adj)
    colnames(pvalue)[2+i]<-paste0('iteration',i)

    logFC<-cbind(logFC,x$avg_log2FC)
    colnames(logFC)[2+i]<-paste0('iteration',i)

    PCT<-cbind(PCT,1/2*(x$pct.1+x$pct.2))
    colnames(PCT)[2+i]<-paste0('iteration',i)

  }

  power<- apply(pvalue[,-c(1:2)], 1, function(x)  mean(x <= pvalue, na.rm = TRUE))
  avg_logFC<- apply(logFC[,-c(1:2)], 1, function(x) mean(x, na.rm = TRUE))
  avg_PCT<- apply(PCT[,-c(1:2)], 1, function(x) mean(x, na.rm = TRUE))

  result<-list(power=power,avg_logFC=avg_logFC,avg_PCT=avg_PCT,replicates=replicates,spots_num=n2,genes_name=sort(rownames(x)))
  return(result)
}
