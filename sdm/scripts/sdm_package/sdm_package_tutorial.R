#this code has been copied fromt he ENM2020 online course
#but i have adapted it to be used with the terra package because I couldn't
#get the raster::getData() function to work

#---------- Script for ENM 2020 online course:


# In this demonstration, we are going to demonstrate the sdm (and usdm) packagge 
# to fit species distribution models for "Acinonyx jubatus" 
# and predict/project the potential distribution in the current and future times
# and measure the changes in the potential distribution (range shift) due to climate change




# install.packages('sdm')
# or
# devtools::install_github('babaknaimi/sdm')


setwd("C:/Users/Rey/Documents/Ischnura/SDM_tutorials/sdm_pkg")


library(sdm)
#installAll() # only firest time after installing the sdm package
library(dismo)
library(dplyr)
library(tidyr)
library(mapview)
library(lwgeom)

# Acinonyx jubatus
sp <- gbif("Acinonyx","jubatus",download= T)

#none of these methods worked, they would only save part of it or wouldn't read it in right again
# write.table(sp, "data/gbif_acinonyx_jubatus.txt", quote=F, col.names=T, row.names=F, sep="\t")
# write.csv(sp, "data/gbif_acinonyx_jubatus.csv", quote=F, row.names=F)
# sp1<- read.csv("data/gbif_acinonyx_jubatus.csv", header=T,row.names=F, fill=T)
# sp1<- read.table("data/gbif_acinonyx_jubatus.txt", sep="\t", header=T, fill=T)
class(sp)
dim(sp)
table(sp$basisOfRecord)


sp <- sp %>% 
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION","OBSERVATION","PRESERVED_SPECIMEN"))
nrow(sp)


spg <- sp %>% select(lon,lat)
head(spg)
spg$species <- 1
spg <- spg %>% drop_na()
nrow(spg)
#------------
class(spg)
#coordinates(spg) <- c('lon','lat')
spg_vect<- vect(spg)
class(spg)
##################
# download the bioclim data:
library(geodata)
#bio <- raster::getData('worldclim', var='bio', res=10)
#bio<- geodata::worldclim_global( var = "bio", res = 10, path = "data")
bio_files<- list.files("data/climate/wc2.1_10m/", pattern="bio_",full.names=T)
bio<- rast(bio_files)
bio
names(bio)

# Make a colour scale
terr_cls<- terrain.colors(100, rev=T)
#rstudio plotting doesn't like this step so need a new window
dev.new(noRStudioGD = TRUE)
plot(bio[[1]], col=terr_cls)  
points(spg)

#e <- drawExtent()
e<- draw()
#this waits for you to click on two points that diagonally encompass the area you want

# spg_crop <- crop(spg_vect, e)
# terra::writeVector(spg_crop, "data/cropped_presence")
spg_crop<- vect("data/cropped_presence/cropped_presence.shp")


bioc <- crop(bio, e)
plot(bioc[[1]], col=terr_cls)
points(spg_crop,col='red')

#------------
library(usdm)

#Calculates variance inflation factor (VIF) for a set of variables and exclude 
#the highly correlated variables from the set through a stepwise procedure. 
vif(bioc)
#f a variable has a strong linear relationship with at least one other variables, 
#the correlation coefficient would be close to 1, and VIF for that variable would be large.
#A VIF greater than 10 is a signal that the model has a collinearity problem
ex <- terra::extract(bioc,spg_crop)
head(ex)

#calculates VIF for all variables, excludes the one with the highest VIF 
#(if it is greater than the threshold), repeat the procedure until no variables 
#with a VIF greater than th remains.
v <- vifstep(ex) #i think threshold is by default 10

v
#Phisically exclude the collinear variables which are identified 
#using vifcor or vifstep from a set of variables.
bioc <- exclude(bioc, v)
#--------------------
# library(sdm)

#~. means it takes species column and considers the rest as predictors, don't need to individually specify
d <- sdmData(species~., spg_crop, predictors= bioc, bg = list(method='gRandom',n=1000))
d


getmethodNames()


m <- sdm(species~., d, methods=c('glm','brt','rf','fda'), replication=c('sub','boot'),
         test.p=30,n=3, parallelSetting=list(ncore=4,method='parallel'))


m
#m@models$species$rf$`13`@object


gui(m)

#if you have n=5, you will have 5x outputs, if mean=T then you only get one
p1 <- predict(m, bioc,filename='pr_c.img')
#p1<- rast("pr.img")
p1<- rast("pr_c.img")
plot(p1, col=terr_cls)
p1
names(p1)


plot(p1[[c(1,7,13,23)]], col=terr_cls)

#can run using the predictors, which will run the predict() function first and then the ensemble
#en1 <- ensemble(m, bio, filename='en.img',setting=list(method='weighted',stat='tss',opt=2))
#en1 <- ensemble(m, bioc, filename='en.img',setting=list(method='weighted',stat='tss',opt=2), overwrite = T)

#but if you have already run the predict() as above, you can use the outputs 
en1 <- ensemble(m, p1, filename='en.img',setting=list(method='weighted',stat='tss',opt=2), overwrite = T)
en1<- rast("en.img")
names(en1)<- "ensemble_weighted"

plot(en1, col=terr_cls)
##################
biof <- raster::getData('CMIP5', var='bio', res=10, rcp=85, model='CN', year=70)
#geodata::cmip6_world(model = "CN", ssp = "245", time = "2061-2080", var = "bioc", res = 10, path = "data")
geodata::cmip6_world(model = "IPSL-CM6A-LR", ssp = "245", time = "2061-2080", var = "bioc", res = 10, path = "data")
biof<- terra::rast("data/climate/wc2.1_10m/wc2.1_10m_bioc_IPSL-CM6A-LR_ssp245_2061-2080.tif")


biof
biof_c <- crop(biof, e)
plot(biof_c[[1]], col=terr_cls)
names(biof_c) <- names(bio)


en2 <- ensemble(m, biof_c, filename='enf_c.img',setting=list(method='weighted',stat='tss',opt=2))
en2<- rast("enf_c.img")
#--------------
plot(en2, col=terr_cls)
cl <- colorRampPalette(c('#3E49BB','#3498DB','yellow','orange','red','darkred'))
#------
plot(en1, col=cl(200))
plot(en2, col=cl(200))


#proj4string(spg_crop) <- projection(en1)
crs(spg_crop)<- crs(en1)


#library(mapview)
# install.packages("lwgeom")
# library(lwgeom)
mapview(c(en1, en2), col.regions=cl(200)) +spg_crop
mapview(en1,col.regions=cl(200)) + spg_crop


#-----------
ch <- en2 - en1
cl2 <- colorRampPalette(c('red','orange','yellow','gray','green','blue'))
plot(ch,col=cl2(200))
#----
df1 <- as.data.frame(d)
# df <- data.frame(species=df$species,coordinates(d))
df <- data.frame(species=df$species,coords(d))
colnames(df)<-c("species", "lon", "lat")
xy <- as.matrix(df[,c('lon','lat')])
head(xy)
p <- terra::extract(en1,xy)$ensemble_weighted
head(p)
nrow(df)
length(p)
ev <- evaluates(df$species,p)
ev@statistics


th <- ev@threshold_based$threshold[2]

pa1<- rast(en1)
#pa1 <- raster(en1)

pa1[] <- ifelse(en1[] >= th, 1, 0)
cl3 <- colorRampPalette(c('gray','green'))
plot(pa1, col=cl3(10))


# pa2 <- raster(en1)
pa2 <- rast(en2)


pa2[] <- ifelse(en2[] >= th, 1, 0)
plot(pa2, col=cl3(10))


chp <- pa2 - pa1
plot(chp,col=c('red','gray','blue'))


#---------------

#Calculate the response of species to the range of values in each predictor 
#variable based on the fitted models in a sdmModels object.
rcurve(m,id=7:12)


plot(getVarImp(m,method='rf'))
#In niche(bio, d, n = c("bio4", "bio18"), col = cl(200)) :
#It seems that the predictor names specified in n do not exist; Predictors wc2.1_10m_bio_14, and wc2.1_10m_bio_15, are used!


#This function maps the species data (either presence/absence or probability of 
#occurrence/habitat suitability) into a two-dimensional environmental space 
#(i.e., based on two environmental variables) to characterise ecological niche 
#based on the specified environmental variables.
niche(bio,d,n=c('bio4','bio18'),col=cl(200))
niche(bio,d,n=c('wc2.1_10m_bio_9','wc2.1_10m_bio_18'),col=cl(200))

#choose which predictors you want to see the niche "as a function of"
niche(bio,en1,n=c('wc2.1_10m_bio_9','wc2.1_10m_bio_18'),col=cl(200))




