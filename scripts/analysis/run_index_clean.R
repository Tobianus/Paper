# Dredge clean #
#remotes::install_github("DTUAqua/DATRAS/DATRAS")
#remotes::install_github("casperwberg/surveyIndex/surveyIndex")
library(surveyIndex)
library(DATRAS)
library(icesDatras)
#library(rgdal)
library(maps)
library(tidyverse)
library(sf)
library(sp)

wd <- "C:/Users/chris/Desktop/DPPO/ICES-DATRAS/NIS/d.export/"

# Download new data for sandeel
downloadExchange('NSSS', years = 2008:2024)

downloadExchange()
#read

# Bind the two together so we get 2004-2008
d=readExchange(file.path(wd,"/NSSS_Combined_2010_2024.zip")) #new data
# Subset by species
d <- subset(d, Species == "Ammodytes marinus")

# # Now run the 2008:2024
# d2 <- readExchangeDir(file.path(wd, 'index_reconstruction/NSSS'))

# Add dates and wave height to data frame
d[[2]]$date = as.Date(paste(d$Year,d$month,d$Day,sep="-"),format="%Y-%m-%d")
d <- addSpectrum(d, by = 1)

d$lat
d$lon
d$Nage


fil = paste('Version9inclSkagerrak',".shp", sep = "")
path3 = file.path(wd,"shp/")
path3<-file.path(path3,fil)

shape <- st_read(dsn = file.path(wd, 'shp/'), layer = 'Version9inclSkagerrak')  #sandeel area shape files
d <- addSpatialData(d, path3)

ages <- 0:4

d <- DATRAS::addNage(d, ages = ages,model="cra ~ Year*SP_ID*LngtCm")
#d <- DATRAS::addNage(d, ages = 0:4, model = "cra ~ s(LngtCm, k = 4) + Year") 
# s() is a smoothing function (from the mgcv package) that models non-linear relationships.
summary(d$HL$LngtCm)

# grid <- getGrid(d, nLon=40)
# ## set max basis dim for spatial smooths by age, P=positive and Z=zero/absence.
# ## These are set relatively low here to speed up the example
# kvP <- c(50,50)
# kvZ <- kvP / 2;
# mP <- rep("Year+s(lon,lat,k=kvecP[a],bs='ts')+offset(log(HaulDur))",length(ages)  );
# mZ <- rep("Year+s(lon,lat,k=kvecZ[a],bs='ts')+offset(log(HaulDur))",length(ages)  );
#
# sandeel_ns_ibts <- getSurveyIdx(d,ages=ages,myids=grid[[3]],cutOff=0.001,kvecP=kvP,kvecZ=kvZ,
#                                 modelZ=mZ,modelP=mP,mc.cores=1) ## if errors are encountered, debug with mc.cores=1


#### Create a new data  frame for me
d.export <- data.frame(Nage = as.matrix(d$Nage),
                       lat = d$lat,
                       long = d$lon,
                       year = d$Year,
                       stock = d$SP_ID) %>% pivot_longer(1:5, names_to = 'Age', values_to = 'cpue')

d.export$Age[d.export$Age == 'Nage.0'] <- '0'
d.export$Age[d.export$Age == 'Nage.1'] <- '1'
d.export$Age[d.export$Age == 'Nage.2'] <- '2'
d.export$Age[d.export$Age == 'Nage.3'] <- '3'
d.export$Age[d.export$Age == 'Nage.4.'] <- '4'


ggplot(d.export[d.export$Age == 0,], aes(x = long, y = lat, fill = log(cpue+1)))+
  geom_tile(width = .2, height = .2)+facet_wrap(~year)+theme_classic()


setwd("C:/Users/chris/Desktop/DPPO/ICES-DATRAS/NIS/")
save(d.export, file = 'nage_space.Rdata')



load("C:/Users/nsja/Dropbox/DTU/Speciale studerende/Chris/data/pol.RData") #banke polygoner


# Run the index model

#
# myidx = getSurveyIdx(d,ages,myids=ggrid[[3]],predD=dp[[2]],cutOff=0.1,fam=myfam,mc.cores=1,modelZ=mZ,modelP=mP,nBoot=1000)


load('nage_space.Rdata')
area1 <- d.export %>% filter(stock == 'SA 1')
ggplot(area1 %>% filter(Age == 0), aes(x = long, lat, fill = log(cpue)))+geom_tile(width = .3, height = .3)+facet_wrap(~year)+theme_classic()




