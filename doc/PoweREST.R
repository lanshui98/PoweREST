## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
#install.packages("devtools")
#devtools::install_github("lanshui1998/PoweREST")
#----or
#install.packages("PoweREST")
library(PoweREST)

## ----eval=FALSE---------------------------------------------------------------
#  #load ST data in R by Seurat:
#  #here we load the pancreatic cancer data which is available on GitHub page
#  three_areas <- readRDS("your path to/GSE233293_scMC.all.3areas.final")
#  Idents(three_areas)
#  #Levels: Peri Juxta Epi
#  SeuratObject_splitlist<-Seurat::SplitObject(three_areas, split.by = "ident")
#  
#  #split the ST data into three areas
#  for (i in 1:length(SeuratObject_splitlist)) {
#    SeuratObject_splitlist[[i]][['Condition']]<-ifelse(SeuratObject_splitlist[[i]][['Type']]=='LG','LG','HR')
#  }
#  
#  for (i in 1:length(SeuratObject_splitlist)) {
#    Seurat::Idents(SeuratObject_splitlist[[i]])<-"Condition"
#  }
#  
#  # Take Peri area for example for downstream analysis
#  Peri<-SeuratObject_splitlist$Peri

## ----eval=FALSE---------------------------------------------------------------
#  result<-PoweREST(Peri,cond='Condition',replicates=5,spots_num=80,iteration=100)
#  
#  #---For test, try this first
#  #PoweREST(Peri,cond='Condition',replicates=5,spots_num=80,iteration=2)
#  #---To get faster, try this
#  #devtools::install_github('immunogenomics/presto')

## ----eval=FALSE---------------------------------------------------------------
#  # For example, use the Student's t-test
#  result2<-PoweREST(Peri,cond='Condition',replicates=5,spots_num=80,iteration=100,test.use="t")

## ----eval=FALSE---------------------------------------------------------------
#  PoweREST_gene(Peri,cond='Condition',replicates=5,spots_num=80,gene_name='MUC1',pvalue=0.00001)

## ----eval=FALSE---------------------------------------------------------------
#  PoweREST_subset(Peri,cond='Condition',replicates=5,spots_num=80,pvalue=0.05,logfc.threshold = 0.1,min.pct = 0.01)

## ----eval=FALSE---------------------------------------------------------------
#  #Fit the power surface for sample size=5 in each arm
#  b<-fit_powerest(result$power,result$avg_logFC,result$avg_PCT)

## ----eval=FALSE---------------------------------------------------------------
#  pred <- pred_powerest(b,xlim= c(0,6),ylim=c(0,1))
#  vis_powerest(pred,theta=-30,phi=30,color='heat',ticktype = "detailed",xlim=c(0,6),nticks=5)

## ----eval=FALSE---------------------------------------------------------------
#  plotly_powerest(pred,fig_title='Power estimation result')

## ----eval=FALSE---------------------------------------------------------------
#  # Fit the local power surface of avg_log2FC_abs between 1 and 2
#  avg_log2FC_abs_1_2<-dplyr::filter(power,avg_log2FC_abs>1 & avg_log2FC_abs<2)
#  # Fit the model
#  bst<-fit_XGBoost(avg_log2FC_abs_1_2$power,avg_log2FC=avg_log2FC_abs_1_2$avg_log2FC_abs,avg_PCT=avg_log2FC_abs_1_2$mean_pct,replicates=avg_log2FC_abs_1_2$sample_size)
#  # Make predictions
#  pred<-pred_XGBoost(bst,n.grid=30,xlim=c(0,1.5),ylim=c(0,0.1),replicates=3)

## ----eval=FALSE---------------------------------------------------------------
#  #2D version
#  vis_XGBoost(pred,view='2D',legend_name='Power',xlab='avg_log2FC_abs',ylab='mean_pct')
#  #3D version
#  vis_XGBoost(pred,view='3D',legend_name='Power',xlab='avg_log2FC_abs',ylab='mean_pct')

