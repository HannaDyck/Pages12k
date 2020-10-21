
# Load packages -----------------------------------------------------------

library(usethis)
library(devtools)
library(roxygen2)
library(testthat)
library(knitr)
library(ggplot2)
library(lipdR)
library(zoo)
library(rmarkdown)
library(plyr)
library(tidyverse)

library(perspackage)

library(psem)
library(PaleoSpec)
library(palmoddata)
library(rbacon)
sessionInfo()


# IMPORT and EXPORT of data ----------------------------------------------------------

path.data <- "/Users/hdyck/Desktop/R projects/Pages12k/data"
path.results <- "/Users/hdyck/Desktop/R projects/Pages12k/results"

# this is the file extracting step, allready done:

# L<-lipdR::readLipd()
# 
# Pages12k_Ts<-lipdR::extractTs(L)
# save(file=paste0(path.data, "/Pages12k_Ts.RData"), list="Pages12k_Ts")

load(file=paste0(path.data, "/Pages12k_Ts.RData"))


# Pages12k_TableOfProxys <- table.proxys(Pages12k_Ts)
# save(file=paste0(path.data, "/Pages12k_TableOfProxys.RData"), list="Pages12k_TableOfProxys")
load(file=paste0(path.data, "/Pages12k_TableOfProxys.RData"))


# Pages12k_TableOfProxys<-rename(Pages12k_TableOfProxys, Pages12k =Freq)
# Pages2k_TableOfProxys<-rename(Pages2k_TableOfProxys, Pages2k =Freq)
# 
# Proxys_2kversus12k<-join(Pages12k_TableOfProxys,Pages2k_TableOfProxys, by ="proxy")
# # wie geht es wirklich, "NA" mach tabelle schwer lesbar: rename(allproxys, .=NA)
# save(file=paste0(path.data, "/Proxys_2kversus12k.RData"), list="Proxys_2kversus12k")
load(file=paste0(path.data, "/Proxys_2kversus12k.RData"))

test<-Pages12k_Ts$paleoData_proxy
inds.proxy<-perspackage::Flatten(lipdR::queryTs(Pages12k_Ts,"paleoData_proxy==alkenone"))


# are there sites without site IDs? Is there a similar variable to the pages2k variable: paleoData_pages2kID?: maybe paleoData_pages12kID
# does not work: sites.proxy<-which(sapply(inds.proxy,function(i) !is.null(Pages12k_Ts[[i]]$paleoData_pages12kID)))
# inds.proxy<-inds.proxy[sites.proxy]

structure<-str(Pages12k_Ts)
Pages12k_Ts[[1]]$paleoData_datum
age<-Pages12k_Ts[[1]]$age

# Here I import the actually timeseries of MXD and proxy values and create zoo objects with them so they are easy to work with.
# hier klappt es nicht so: zser.proxy<-sapply(inds.proxy,function(i) zoo::zoo(Pages12k_Ts[[i]]$paleoData_values,order.by = Pages12k_Ts[[i]]$paleoData_datum))
zser.proxy<-sapply(inds.proxy,function(i) zoo::zoo(Pages12k_Ts[[i]]$paleoData_values,order.by = Pages12k_Ts[[i]]$age))

#????? zoo::index(zser.proxy)

# Here I import the coordinates of the sites above
coords.proxy<-t(sapply(inds.proxy,function(i) c(Pages12k_Ts[[i]]$geo_longitude,Pages12k_Ts[[i]]$geo_latitude)))

plot(c(1:length(zser.proxy[[1]])),zser.proxy[[1]])
plot(c(1:length(zser.proxy[[2]])),zser.proxy[[2]])
plot(c(1:length(zser.proxy[[5]])),zser.proxy[[5]])

# Folgendes funktioniert nur für den Schnitt über die Zeitabschnitte, also bei gleichen Indixwerten der Zeitreihen:



#----------------- Matrix with DIMENSION of  (length of intersection of observation periods) --------------------------------------
#---------------------------------    times  (number of observation sites for a specific proxy)------------------------------------

matrix.merge <-perspackage::merge.cutoff(zser.proxy)

save(file=paste0(path.results, "/TRW.matrix.merge.viafunction.RData"), list="matrix.merge")

load(file="/Users/hdyck/Desktop/R projects/Pages2k-ESDOF/results/TRW.RData")
load(file="/Users/hdyck/Desktop/R projects/Pages2k-ESDOF/data/Pages2k_Ts.RData")
load(file="/Users/hdyck/Desktop/R projects/Pages2k-ESDOF/results/TRWok.RData")

################################################################################################################
# -------------------- manuelle Korrektur für TRW:--------------------------------------------------

index.ok <- (rowMeans(matrix.merge[[1]]) < 5 & !is.na(rowMeans(matrix.merge[[1]])))
matrix.merge.ok <- matrix.merge[[1]][index.ok,]
dim(matrix.merge.ok)
coords.merge.ok <- coords.proxy[index.ok,]
dim(coords.merge.ok)
num.sites.ok <- length(matrix.merge.ok[,1])

save(file=paste0(path.results, "/TRW.matrix.merge.ok.viafunction.RData"), list="matrix.merge.ok")

load(file="/Users/hdyck/Desktop/R projects/Pages2k-ESDOF/results/TRW.matrix.merge.ok.RData")


image(matrix.merge.ok-TRWok)

################################################################################################################
#--------------------- manuelle Korrektur für MXD:--------------------------------------------------------


index.ok <- (rowMeans(matrix.merge) < 1000
             & !is.na(rowMeans(matrix.merge)))
matrix.merge.ok <- matrix.merge[index.ok,]
coords.merge.ok <- coords.proxy[index.ok,]
dim(coords.merge.ok)

image(matrix.merge.ok)

################################################################################################################
############################ VARIANCES ###########################################################################
################################################################################################################

# ---------------- CALCULATING spacial averages at all time points of the series -----------------------
#---------- which gives one smooth time series "spac.mean.proxy". to this time series the variance estimator is applyed too.

latitu <-coords.merge.ok[,2]
lon <-coords.merge.ok[,1]
intval<-c(0.001,3)

plot<-variance.plot(matrix.merge.ok,latitu,intval)

################################################################################################################
#---------------------  CALCULATING  D ---------------------------------------------------------

#   D: the quotient of (the spacial average of the variances)
#                 and (the variance of the spacial averages)

#var(colMeans(matrix.merge.ok))
#mean(apply(matrix.merge.ok, 1, var))

D.merge <-ESDOF(matrix.merge.ok)

num.obs.spaces.merge <- dim(matrix.merge.ok)[1]

surrogate <- matrix(rnorm(num.obs.spaces.merge*matrix.merge[[2]]),num.obs.spaces.merge,matrix.merge[[2]])
#var(colMeans(surrogate))
#mean(apply(surrogate, 1, var))

D.test.merge <- ESDOF(surrogate)

