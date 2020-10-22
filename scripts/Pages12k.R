
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

# list of functions on Github ECUS that I should include to Pages2k script:

# Package PALEOSPEC:
# did work with PAT:
# devtools::install_github("EarthSystemDiagnostics/paleospec",force=TRUE, auth_token= "50a272d0a2a6f8490fef6cad4aa01c7d43a90827")

#
# SpecInterpolate
# MakeEquidistant
# GetVarFromSpectra
# FirstElement
# ConfVar
# ConfRatio

# Package ECUSTOOLS:

# GetDistance
# statistical-functions


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

# warum liefert das eine Liste, in der jeder Index dreimal vorkommt?!?:
inds.proxy<-perspackage::Flatten(lipdR::queryTs(Pages12k_Ts,"paleoData_proxy==alkenone"))
inds.proxy<-lipdR::queryTs(Pages12k_Ts,"paleoData_proxy==alkenone")
inds.proxy<-Flatten(inds.proxy)

# are there sites without site IDs? Is there a similar variable to the pages2k variable: paleoData_pages2kID?: maybe paleoData_pages12kID
# does not work: sites.proxy<-which(sapply(inds.proxy,function(i) !is.null(Pages12k_Ts[[i]]$paleoData_pages12kID)))
# inds.proxy<-inds.proxy[sites.proxy]

# is this the way to gain insight to the dataset?
structure<-str(Pages12k_Ts)

# what is the time index variable? and what does it mean?
Pages12k_Ts[[1]]$paleoData_datum

# example:
age1<-Pages12k_Ts[[1]]$age
age<-Pages12k_Ts[[2]]$age

values1<-Pages12k_Ts[[1]]$paleoData_values
values2<-Pages12k_Ts[[2]]$paleoData_values

# Here I import the actually timeseries of MXD and proxy values and create zoo objects with them so they are easy to work with.
# hier klappt es nicht so: zser.proxy<-sapply(inds.proxy,function(i) zoo::zoo(Pages12k_Ts[[i]]$paleoData_values,order.by = Pages12k_Ts[[i]]$paleoData_datum))
# can't do this sorting of data: zser.proxy<-sapply(inds.proxy,function(i) zoo::zoo(Pages12k_Ts[[i]]$paleoData_values,order.by = Pages12k_Ts[[i]]$age))
# I tought I would do:
# zser.proxy<-sapply(inds.proxy,function(i) zoo::zoo(Pages12k_Ts[[i]]$paleoData_values))
# but instead I do:
zser.proxy<-Pages12k_Ts[inds.proxy]

# Here I import the coordinates of the sites above
coords.proxy<-t(sapply(inds.proxy,function(i) c(Pages12k_Ts[[i]]$geo_longitude,Pages12k_Ts[[i]]$geo_latitude)))

# some examples:
plot(zser.proxy[[1]]$age,zser.proxy[[1]]$paleoData_values)
plot(zser.proxy[[30]]$age,zser.proxy[[30]]$paleoData_values)
plot(zser.proxy[[150]]$age,zser.proxy[[150]]$paleoData_values)
plot(zser.proxy[[230]]$age,zser.proxy[[230]]$paleoData_values)
plot(zser.proxy[[300]]$age,zser.proxy[[300]]$paleoData_values)

###################################################################################################
# make time series equidistant:

length(zser.proxy[[300]]$age)
length(Flatten(zser.proxy[[300]]$age))
class(Flatten(zser.proxy[[300]]$paleoData_values))

age300<-Flatten(zser.proxy[[300]]$age)
value300<-Flatten(zser.proxy[[300]]$paleoData_values)

ts230<-MakeEquidistant(age300,value300 , dt = NULL, time.target = seq(from = age300[1],
                                                       to = age300[length(age300)], by = 50), dt.hres = NULL, bFilter = TRUE,
                k = 5, kf = 1.2, method.interpolation = "linear",
                method.filter = 2)

plot(ts230)

# not funtioning yet:

zser.proxy[[1]]$age[1]
class(zser.proxy[[1]]$age)

# what of the object is actually transferred to the function in sapply?:
TsList<- sapply(c(1:length(zser.proxy)), function(i) MakeEquidistant(zser.proxy[[i]]$age,zser.proxy[[i]]$paleoData_values , 
                                                        dt = NULL, time.target = seq(from = zser.proxy[[i]]$age[1],to = zser.proxy[[i]]$age[length(zser.proxy[[i]]$age)]), 
                                                        dt.hres = NULL, bFilter = TRUE,
                       k = 5, kf = 1.2, method.interpolation = "linear",
                       method.filter = 2))




TsList<- lapply( zser.proxy, function(i) MakeEquidistant(Flatten(zser.proxy[[i]]$age),Flatten(zser.proxy[[i]]$paleoData_values) , dt = NULL, time.target = seq(from = Flatten(zser.proxy[[i]]$age)[1],
                                                                                                                                                               to = Flatten(zser.proxy[[i]]$age)[length(Flatten(zser.proxy[[i]]$age))], by = 50), dt.hres = NULL, bFilter = TRUE,
                                                         k = 5, kf = 1.2, method.interpolation = "linear",
                                                         method.filter = 2))


# this is the original form of the function:
MakeEquidistant(t.x, t.y, dt = NULL, time.target = seq(from = t.x[1],
                                                       to = t.x[length(t.x)], by = dt), dt.hres = NULL, bFilter = TRUE,
                k = 5, kf = 1.2, method.interpolation = "linear",
                method.filter = 2)

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

