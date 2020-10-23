
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
library(multitaper)
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

archiveType<-lapply(Pages12k_Ts, function(i) i$archiveType)

# marine proxys auswählen:
# mar12.proxy<-perspackage::Flatten(lipdR::queryTs(Pages12k_Ts,"archiveType==MarineSediment"))
# mar12.proxy<-Pages12k_Ts[mar12.proxy]
# 
# marine12k_TableOfProxys <- table.proxys(mar12.proxy)
# 
# save(file=paste0(path.data, "/marine12k_TableOfProxys.RData"), list="marine12k_TableOfProxys")
load(file=paste0(path.data, "/marine12k_TableOfProxys.RData"))

marine12k.mgca<-perspackage::Flatten(lipdR::queryTs(mar12.proxy,"paleoData_proxy==Mg/Ca"))
marine12k.mgca<-Pages12k_Ts[marine12k.mgca]

age1<-Flatten(marine12k.mgca[[1]]$age)
value1<-Flatten(marine12k.mgca[[1]]$paleoData_values)

ts1.20<-MakeEquidistant(age1,value1 , dt = NULL, time.target = seq(from = age300[1],
                                                                         to = age300[length(age300)]), dt.hres = NULL, bFilter = TRUE,
                          k = 5, kf = 1.2, method.interpolation = "linear",
                          method.filter = 2)

#### WTF!!!?!??! Was passiert hier: Grafiken vergleichen!
plot(ts1.20)
plot(marine12k.mgca[[1]]$age, marine12k.mgca[[1]]$paleoData_values)



# maybe use:
# archiveType "MarineSediment"
# timeID
# maxYear
# minYear
# nUniqueAges
# nUniqueOtherAges
# otherAgesPerKyr
# agesPerKyr
# ageUnits
# nUniqueGoodAges


inds.proxy<-lipdR::queryTs(Pages12k_Ts,"paleoData_proxy==alkenone")
inds.proxy<-lipdR::queryTs(Pages12k_Ts,"paleoData_proxy==foraminifera")
inds.proxy<-lipdR::queryTs(Pages12k_Ts,"paleoData_proxy==Mg/Ca")
inds.proxy<-lipdR::queryTs(Pages12k_Ts,"paleoData_proxy==planktic foraminifera assemblage")
inds.proxy<-lipdR::queryTs(Pages12k_Ts,"paleoData_proxy==planktic foraminifera calcite")
inds.proxy<-lipdR::queryTs(Pages12k_Ts,"paleoData_proxy==delta18O")
inds.proxy<-lipdR::queryTs(Pages12k_Ts,"paleoData_proxy==d18O")

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
# hier klappt es nicht so: ser.proxy<-sapply(inds.proxy,function(i) zoo::zoo(Pages12k_Ts[[i]]$paleoData_values,order.by = Pages12k_Ts[[i]]$paleoData_datum))
# can't do this sorting of data: ser.proxy<-sapply(inds.proxy,function(i) zoo::zoo(Pages12k_Ts[[i]]$paleoData_values,order.by = Pages12k_Ts[[i]]$age))
# I tought I would do:
# ser.proxy<-sapply(inds.proxy,function(i) zoo::zoo(Pages12k_Ts[[i]]$paleoData_values))
# but instead I do:
ser.proxy<-Pages12k_Ts[inds.proxy]


agesPerKyr<-lapply(ser.proxy, function(i) i$agesPerKyr)

# Here I import the coordinates of the sites above
coords.proxy<-t(sapply(inds.proxy,function(i) c(Pages12k_Ts[[i]]$geo_longitude,Pages12k_Ts[[i]]$geo_latitude)))

# some examples:
plot(ser.proxy[[1]]$age,ser.proxy[[1]]$paleoData_values)
plot(ser.proxy[[30]]$age,ser.proxy[[30]]$paleoData_values)
plot(ser.proxy[[150]]$age,ser.proxy[[150]]$paleoData_values)
plot(ser.proxy[[230]]$age,ser.proxy[[230]]$paleoData_values)
plot(ser.proxy[[300]]$age,ser.proxy[[300]]$paleoData_values)

###################################################################################################
# make time series equidistant:

length(ser.proxy[[300]]$age)
length(Flatten(ser.proxy[[300]]$age))
class(Flatten(ser.proxy[[300]]$paleoData_values))

age300<-Flatten(ser.proxy[[300]]$age)
value300<-Flatten(ser.proxy[[300]]$paleoData_values)

ts230.20<-MakeEquidistant(age300,value300 , dt = NULL, time.target = seq(from = age300[1],
                                                       to = age300[length(age300)]), dt.hres = NULL, bFilter = TRUE,
                k = 5, kf = 1.2, method.interpolation = "linear",
                method.filter = 2)

plot(ts230)

# not functioning yet:

ser.proxy[[1]]$age[1]
class(ser.proxy[[1]]$age)

# what of the object is actually transferred to the function in sapply?:
TsList<- sapply(c(1:length(ser.proxy)), function(i) MakeEquidistant(ser.proxy[[i]]$age,ser.proxy[[i]]$paleoData_values , 
                                                        dt = NULL, time.target = seq(from = ser.proxy[[i]]$age[1],to = ser.proxy[[i]]$age[length(ser.proxy[[i]]$age)]), 
                                                        dt.hres = NULL, bFilter = TRUE,
                       k = 5, kf = 1.2, method.interpolation = "linear",
                       method.filter = 2))




TsList<- lapply( ser.proxy, function(i) MakeEquidistant(Flatten(ser.proxy[[i]]$age),Flatten(ser.proxy[[i]]$paleoData_values) , dt = NULL, time.target = seq(from = Flatten(ser.proxy[[i]]$age)[1],
                                                                                                                                                               to = Flatten(ser.proxy[[i]]$age)[length(Flatten(ser.proxy[[i]]$age))], by = 50), dt.hres = NULL, bFilter = TRUE,
                                                         k = 5, kf = 1.2, method.interpolation = "linear",
                                                         method.filter = 2))


# this is the original form of the function:
MakeEquidistant(t.x, t.y, dt = NULL, time.target = seq(from = t.x[1],
                                                       to = t.x[length(t.x)], by = dt), dt.hres = NULL, bFilter = TRUE,
                k = 5, kf = 1.2, method.interpolation = "linear",
                method.filter = 2)


class(ts230)


# just to control for equidistance:
diff<- numeric(length(ts230))
for (i in 2:length(ts230)){
diff[i-1]<- index(ts230)[i]-index(ts230)[i-1]}

diff<- numeric(length(ts230.20))
for (i in 2:length(ts230.20)){
  diff[i-1]<- index(ts230.20)[i]-index(ts230.20)[i-1]}

  
spec230<-SpecMTM(ts230, k = 3, nw = 2, nFFT = "default",
                 centre = c("Slepian"), dpssIN = NULL, returnZeroFreq = FALSE,
                 Ftest = FALSE, jackknife = FALSE, jkCIProb = 0.95,
                 maxAdaptiveIterations = 100, plot = FALSE, na.action = na.fail,
                 returnInternals = FALSE, detrend = TRUE, bPad = FALSE, ...)


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

