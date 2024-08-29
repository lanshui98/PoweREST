pkgname <- "PoweREST"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "PoweREST-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('PoweREST')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("fit_XGBoost")
### * fit_XGBoost

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_XGBoost
### Title: Fit with XGBoost
### Aliases: fit_XGBoost

### ** Examples

data(power_example)
# Fit the local power surface of avg_log2FC_abs between 1 and 2
avg_log2FC_abs_1_2<-dplyr::filter(power_example,avg_log2FC_abs>1 & avg_log2FC_abs<2)
# Fit the model
bst<-fit_XGBoost(power_example$power,avg_log2FC=power_example$avg_log2FC_abs,
avg_PCT=power_example$mean_pct,replicates=power_example$sample_size)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_XGBoost", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit_powerest")
### * fit_powerest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_powerest
### Title: Fit the power surface
### Aliases: fit_powerest

### ** Examples

data(result_example)
 ## No test: 
b<-fit_powerest(result_example$power,result_example$avg_logFC,result_example$avg_PCT)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_powerest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotly_powerest")
### * plotly_powerest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotly_powerest
### Title: 3D interactive visualization
### Aliases: plotly_powerest

### ** Examples

data(result_example)
 ## No test: 
b<-fit_powerest(result_example$power,result_example$avg_logFC,result_example$avg_PCT)
## End(No test)
 ## No test: 
pred <- pred_powerest(b,xlim= c(0,6),ylim=c(0,1))
## End(No test)
 ## No test: 
plotly_powerest(pred,fig_title='Power estimation result')
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotly_powerest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pred_XGBoost")
### * pred_XGBoost

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pred_XGBoost
### Title: Prediction results from XGBoost
### Aliases: pred_XGBoost

### ** Examples

data(power_example)
# Fit the local power surface of avg_log2FC_abs between 1 and 2
avg_log2FC_abs_1_2<-dplyr::filter(power_example,avg_log2FC_abs>1 & avg_log2FC_abs<2)
# Fit the model
bst<-fit_XGBoost(power_example$power,avg_log2FC=power_example$avg_log2FC_abs,
avg_PCT=power_example$mean_pct,replicates=power_example$sample_size)
pred<-pred_XGBoost(bst,n.grid=30,xlim=c(0,1.5),ylim=c(0,0.1),replicates=3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pred_XGBoost", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pred_powerest")
### * pred_powerest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pred_powerest
### Title: Power value prediction
### Aliases: pred_powerest

### ** Examples

data(result_example)
 ## No test: 
b<-fit_powerest(result_example$power,result_example$avg_logFC,result_example$avg_PCT)
## End(No test)
 ## No test: 
pred <- pred_powerest(b,xlim= c(0,6),ylim=c(0,1))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pred_powerest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vis_XGBoost")
### * vis_XGBoost

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vis_XGBoost
### Title: Visualization of the power estimations from XGBoost
### Aliases: vis_XGBoost

### ** Examples

data(power_example)
# Fit the local power surface of avg_log2FC_abs between 1 and 2
avg_log2FC_abs_1_2<-dplyr::filter(power_example,avg_log2FC_abs>1 & avg_log2FC_abs<2)
# Fit the model
bst<-fit_XGBoost(power_example$power,avg_log2FC=power_example$avg_log2FC_abs,
avg_PCT=power_example$mean_pct,replicates=power_example$sample_size)
pred<-pred_XGBoost(bst,n.grid=30,xlim=c(0,1.5),ylim=c(0,0.1),replicates=3)
vis_XGBoost(pred,view='2D',legend_name='Power',xlab='avg_log2FC_abs',ylab='mean_pct')





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vis_XGBoost", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vis_powerest")
### * vis_powerest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vis_powerest
### Title: Visualization of the power surface
### Aliases: vis_powerest

### ** Examples

data(result_example)
 ## No test: 
b<-fit_powerest(result_example$power,result_example$avg_logFC,result_example$avg_PCT)
## End(No test)
 ## No test: 
pred <- pred_powerest(b,xlim= c(0,6),ylim=c(0,1))
## End(No test)
 ## No test: 
vis_powerest(pred,theta=-30,phi=30,color='heat',ticktype = "detailed",xlim=c(0,6),nticks=5)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vis_powerest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
