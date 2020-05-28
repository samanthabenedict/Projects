
#Set working directory
setwd()


#########  HUNTER COLLEGE: DEPARTMENT OF MATHEMATICS & STATISTICS 
#########  Spatial Analysis Project
#########  FALL 2019
#########  Point Patern Analysis of Wildfires in NYS
#########  Samantha Benedict


#1642 total records
#1636 intersect with NYS

library(readxl)
library(rgdal) #Brings Spatial Data in R
library(spatstat) # Spatial Statistics
library(lattice) #Graphing
library(maptools)
library(raster)
library(ggplot2)
library(RColorBrewer)
library(broom)

# Load an nys.shp polygon shapefile 
s <- readOGR("nys_shapefiles", "NYS_Project")  # Don't add the .shp extension
nys <- as(s, "owin") 

# Load a wf_2018.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2018_Project")  # Don't add the .shp extension
wf_2018 <- as(s, "ppp")

marks(wf_2018) <- NULL

Window(wf_2018) <- nys


# Load a wf_2017.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2017_Project")  # Don't add the .shp extension
wf_2017 <- as(s, "ppp")

marks(wf_2017) <- NULL

Window(wf_2017) <- nys

# Load a wf_2016.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2016_Project")  # Don't add the .shp extension
wf_2016 <- as(s, "ppp")

marks(wf_2016) <- NULL

Window(wf_2016) <- nys

# Load a wf_2015.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2015_Project")  # Don't add the .shp extension
wf_2015 <- as(s, "ppp")

marks(wf_2015) <- NULL

Window(wf_2015) <- nys

# Load a wf_2014.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2014_Project")  # Don't add the .shp extension
wf_2014 <- as(s, "ppp")

marks(wf_2014) <- NULL

Window(wf_2014) <- nys

# Load a wf_2013.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2013_Project")  # Don't add the .shp extension
wf_2013 <- as(s, "ppp")

marks(wf_2013) <- NULL

Window(wf_2013) <- nys

# Load a wf_2012.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2012_Project")  # Don't add the .shp extension
wf_2012 <- as(s, "ppp")

marks(wf_2012) <- NULL

Window(wf_2012) <- nys

# Load a wf_2011.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2011_Project")  # Don't add the .shp extension
wf_2011 <- as(s, "ppp")

marks(wf_2011) <- NULL

Window(wf_2011) <- nys

# Load a wf_2010.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2010_Project")  # Don't add the .shp extension
wf_2010 <- as(s, "ppp")

marks(wf_2010) <- NULL

Window(wf_2010) <- nys

# Load a wf_2009.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2009_Project")  # Don't add the .shp extension
wf_2009 <- as(s, "ppp")

marks(wf_2009) <- NULL

Window(wf_2009) <- nys

# Load a wf_2008.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2008_Project")  # Don't add the .shp extension
wf_2008 <- as(s, "ppp")

marks(wf_2008) <- NULL

Window(wf_2008) <- nys


# plot 2018
plot(wf_2018, main="2018", cols="dark green", pch=20)
#Quadrat density 2018
Q18 <- quadratcount(wf_2018, nx= 3, ny=3)
plot(wf_2018, pch=20, cols="grey70", main="2018")  # Plot points
plot(Q18, main="2018", add=TRUE)  # Add quadrat grid

# plot 2017
plot(wf_2017, main="2017", cols=rgb(0,0,0,.2), pch=20)
#Quadrat density 2017
Q17 <- quadratcount(wf_2017, nx= 3, ny=3)
plot(wf_2017, pch=20, cols="grey70", main="2017")  # Plot points
plot(Q17, main="2017", add=TRUE)  # Add quadrat grid

# plot 2016
plot(wf_2016, main="2016", cols=rgb(0,0,0,.2), pch=20)
#Quadrat density 2016
Q16 <- quadratcount(wf_2016, nx= 3, ny=3)
plot(wf_2016, pch=20, cols="grey70", main="2016")  # Plot points
plot(Q16, add=TRUE)  # Add quadrat grid


# plot 2015
plot(wf_2015, main="2015", cols=rgb(0,0,0,.2), pch=20)
#Quadrat density 2015
Q15 <- quadratcount(wf_2015, nx= 3, ny=3)
plot(wf_2015, pch=20, cols="grey70", main="2015")  # Plot points
plot(Q15, add=TRUE)  # Add quadrat grid



# plot 2014
plot(wf_2014, main="2014", cols=rgb(0,0,0,.2), pch=20)
#Quadrat density 2014
Q14 <- quadratcount(wf_2014, nx= 3, ny=3)
plot(wf_2014, pch=20, cols="grey70", main="2014")  # Plot points
plot(Q14, add=TRUE)  # Add quadrat grid

# plot 2013
plot(wf_2013, main="2013", cols=rgb(0,0,0,.2), pch=20)
#Quadrat density 2013
Q13<- quadratcount(wf_2013, nx= 3, ny=3)
plot(wf_2013, pch=20, cols="grey70", main="2013")  # Plot points
plot(Q13, add=TRUE)  # Add quadrat grid


# plot 2012
plot(wf_2012, main="2012", cols=rgb(0,0,0,.2), pch=20)
#Quadrat density 2012
Q12 <- quadratcount(wf_2012, nx= 3, ny=3)
plot(wf_2012, pch=20, cols="grey70", main="2012")  # Plot points
plot(Q12, add=TRUE)  # Add quadrat grid

# plot 2011
plot(wf_2011, main="2011", cols="dark green", pch=20)

#Quadrat density 2011
Q11 <- quadratcount(wf_2011, nx= 3, ny=3)
plot(wf_2011, pch=20, cols="grey70", main="2011")  # Plot points
plot(Q11, add=TRUE)  # Add quadrat grid


# plot 2010
plot(wf_2010, main="2010", cols=rgb(0,0,0,.2), pch=20)
#Quadrat density 2010
Q10 <- quadratcount(wf_2010, nx= 3, ny=3)
plot(wf_2010, pch=20, cols="grey70", main="2010")  # Plot points
plot(Q10, add=TRUE)  # Add quadrat grid

# plot 2009
plot(wf_2009, main="2009", cols=rgb(0,0,0,.2), pch=20)
#Quadrat density 2009
Q09 <- quadratcount(wf_2009, nx= 3, ny=3)
plot(wf_2009, pch=20, cols="grey70", main="2009")  # Plot points
plot(Q09, add=TRUE)  # Add quadrat grid

# plot 2008
plot(wf_2008, main="2008", cols=rgb(0,0,0,.2), pch=20)
#Quadrat density 2008
Q08 <- quadratcount(wf_2008, nx= 3, ny=3)
plot(wf_2008, pch=20, cols="grey70", main="2008")  # Plot points
plot(Q08, add=TRUE)


#CHI SQUARED TEST
quadrat.test(wf_2018, nx=3, ny=3)
quadrat.test(wf_2017, nx=3, ny=3)
quadrat.test(wf_2016, nx=3, ny=3)
quadrat.test(wf_2015, nx=3, ny=3)
quadrat.test(wf_2014, nx=3, ny=3)
quadrat.test(wf_2013, nx=3, ny=3)
quadrat.test(wf_2012, nx=3, ny=3)
quadrat.test(wf_2011, nx=3, ny=3)
quadrat.test(wf_2010, nx=3, ny=3)
quadrat.test(wf_2009, nx=3, ny=3)
quadrat.test(wf_2008, nx=3, ny=3)

wf_2011_mi <- rescale(wf_2011, 1000, "mi")
wf_2016_mi <- rescale(wf_2016, 1000, "mi")

#First-order property(Measuring Intensity)
#Kernel Estimation GUASSIAN DEFAULT
#Using bandwith 30 and points
plot(density(wf_2011_mi, sigma=14), main="2011: Kernel Intensity, BW = 14 mi.", las=1)
contour(density(wf_2011_mi, sigma=24),col="white", add=TRUE)
plot(wf_2011_mi, pch=20,cex=0.6, col="white", add=TRUE)  # Add points

#First-order property(Measuring Intensity)
#Kernel Estimation GUASSIAN DEFAULT
#Using bandwith 30 and points
plot(density(wf_2016_mi,sigma = 55),  main="2016: Kernel Intensity, BW = 55 mi.", las=2)
contour(density(wf_2016_mi, sigma=55),col="white", add=TRUE)
plot(wf_2016_mi, pch=20, cex=0.6, col="white", add=TRUE)  # Add points

# Load a wf_2011.shp point feature shapefile
s_11 <- readOGR("wf_shapefiles","wf_2011_Project")  # Don't add the .shp extension
wf_2011 <- as(s_11, "ppp")

# Load a wf_2016.shp point feature shapefile
s_16 <- readOGR("wf_shapefiles","wf_2016_Project")  # Don't add the .shp extension
wf_2016 <- as(s_16, "ppp")

# Load a wf_2018.shp point feature shapefile
s_18 <- readOGR("wf_shapefiles","wf_2018_Project")  # Don't add the .shp extension
wf_2018 <- as(s_18, "ppp")

# Load a prism.img rainfall raster layer (2011)
rf_11<- raster("prism_img/prism_extract_2011_lower_proj1.tif")
rf_11_img  <- as.im(rf_11)
plot(rf_11)
rf_11clip = mask(rf_11,s)
plot(rf_11clip)
plot(wf_2011, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

# Load a prism.img rainfall raster layer (2016)
rf_16<- raster("prism_img/prism_extract_2016_lower_proj1.tif")
plot(rf_16)
rf_16clip = mask(rf_16,s)
plot(rf_16clip)
plot(wf_2016, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

# Load a prism.img rainfall raster layer (2018)
rf_18<- raster("prism_img/prism_extract_2018_lower_proj1.tif")
rf_18_img  <- as.im(rf_18)
rf_18_mi <- rescale(rf_18_img, 1000, "mi")
plot(rf_18)
rf_18clip = mask(rf_18,s)
plot(rf_18clip)
plot(wf_2018, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points


#Quadrat density on a tessellated surface
brk  <- c( -Inf, 4, 6, 8 , Inf)  # Define the breaks
Zcut <- cut(rf_18clip, breaks=brk, labels=1:4)  # Classify the raster
E    <- tess(image=Zcut)  # Create a tesselated surface
plot(E, main="", las=1)

# Load an nys.shp polygon shapefile 
s <- readOGR("nys_shapefiles", "NYS_Project")  # Don't add the .shp extension
nys <- as(s, "owin") 

# Load a wf_2018.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2018_Project")  # Don't add the .shp extension
wf_2018 <- as(s, "ppp")



#Create Wildfire Coordinate Matrix
wf_point = matrix(NA, nrow=nrow(s), ncol=2)
wf_point[, 1] = s$long_ #LONG
wf_point[, 2] = s$lat #LAT
colnames(wf_point) = c("Longitude", "Latitude")

plot(wf_point, pch=20)

#Create random points to test agaisnt
u.x = runif(n=nrow(wf_point), min=bbox(wf_point)[1,1], max=bbox(wf_point)[1,2])
u.y = runif(n=nrow(wf_point), min=bbox(wf_point)[2,1], max=bbox(wf_point)[2,2])
plot(u.x)
plot(u.y)

#Create regular 
r.x = seq(from=min(wf_point[,1]), to=max(wf_point[,1]),length=sqrt(nrow(wf_point)))
r.y = seq(from=min(wf_point[,2]), to=max(wf_point[,2]),length=sqrt(nrow(wf_point)))

r.x= jitter(rep(r.x, length(r.x)), .001)
r.y= jitter(rep(r.y, length(r.y)), .001)

plot(r.x)
plot(r.y)

#plot the points
par(mfrow=c(1,3), mar=c(2,2,2,0))
plot(x=wf_point[,1],y=wf_point[,2],main="2018 Wildfires", xlab="LONG", ylab="LAT", cex=.5, pch=20)
plot(x=u.x, y=u.y, main="CSR", xlab="LONG", ylab="LAT", cex=.5,pch=20)
plot(x=r.x, y=r.y, main="Uniform", xlab="LONG", ylab="LAT", cex=.5,pch=20)


marks(wf_2018) <- NULL

Window(wf_2018) <- nys


plot(wf_2018, main="2018", cols=rgb(0,0,0,.2), pch=20)

#First-order property(Measuring Intensity)
#Quadrat Analysis 2018 
#square meters per quadrat
Q18 <- quadratcount(wf_2018, nx=3 , ny=3)

plot(wf_2018, pch=20, cols="grey70", main="2018")  # Plot points
plot(Q18, add=TRUE)  # Add quadrat grid

#First-order property(Measuring Intensity)
#Test for CSR
#chi-squared test using quadrat counts
qt = quadrat.test(wf_2018, nx=3, ny=3)
qt

#Rescale units into miles rather than meters/ km
wf_2018_mi <- rescale(wf_2018, 1000, "mi")
wf_2016_mi <- rescale(wf_2016, 1000, "mi")
wf_2011_mi <- rescale(wf_2011, 1000, "mi")

marks(wf_2018)
Window(wf_2018)

#First-order property(Measuring Intensity)
#Kernel Estimation GUASSIAN DEFAULT
# Using the default bandwidth
plot(density(wf_2018_mi), main=NULL, las=1)
contour(density(wf_2018_mi), add=TRUE)

#First-order property(Measuring Intensity)
#Kernel Estimation GUASSIAN DEFAULT
#Using bandwith 30 and contour
plot(density(wf_2018_mi,sigma = 30), main="2018: Kernel Intensity Contour Plot, BW = 30 mi.", las=1)
contour(density(wf_2018_mi, sigma=30),col="white", add=TRUE)
plot(wf_2018_mi, pch=20, cex=0.6, col="white", add=TRUE)  # Add points

#First-order property(Measuring Intensity)
#Kernel Estimation GUASSIAN DEFAULT
#Using bandwith 30 and points
plot(density(wf_2018_mi,sigma = 30), main="2018: Kernel Intensity, BW = 30 mi.", las=1)
plot(wf_2018_mi, pch=20, cex=0.6, col="white", add=TRUE)  # Add points

#First-order property(Measuring Intensity)
#Kernel Estimation GUASSIAN DEFAULT
#Using bandwith 10
plot(density(wf_2018,sigma = 10), main=NULL, las=1)
contour(density(wf_2018_mi, sigma=10), add=TRUE)

#First-order property(Measuring Intensity)
# Plot the density
plot(intensity(Q18, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(wf_2018, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points


#First-order property(Measuring Intensity)
# Compute the density for each quadrat (in counts per miles)
Q18   <- quadratcount(wf_2018_mi, nx= 3, ny=3)
Q18.den <- intensity(Q18); Q18.den

#First-order property(Measuring Intensity)
# Plot the density (in counts per miles)
plot(intensity(Q18, image=TRUE, sigma=30), main=NULL, las=1)  # Plot density raster
plot(wf_2018_mi, pch=20, cex=0.6, col="white", add=TRUE)  # Add points

#First-order property(Measuring Intensity)
#Quadrat density on a tessellated surface
brk  <- c( -Inf, 4, 6, 8 , Inf)  # Define the breaks
Zcut <- cut(rainfall_lg_mi, breaks=brk, labels=1:4)  # Classify the raster
E <- tess(image=Zcut)  # Create a tesselated surface
plot(E, main="", las=1)


##############################################################

#Second Order Properites (measuring spatial dependence)
#G Function: Nearest Neighbor Analysis (2018)
r= seq(0,150,by=2)
G18 = envelope(wf_2018_mi, Gest, r=r, nsim=59, rank=2)
plot(G18, main="2018: G Function", xlab="Distance in mi. (r)")

#G Function: Nearest Neighbor Analysis (2016)
r= seq(0,100,by=2)
G16 = envelope(wf_2016_mi, Gest, r=r, nsim=59, rank=2)
plot(G16, main="2016: G Function", xlab="Distance in mi. (r)")

#G Function: Nearest Neighbor Analysis (2011)
r= seq(0,120,by=8)
G11 = envelope(wf_2011_mi, Gest, r=r, nsim=59, rank=2)
plot(G11, main="2011: G Function", xlab="Distance in mi. (r)")


#Second Order Properites (measuring spatial dependence)
#F Function: Nearest Neighbor Analysis (2018)
r= seq(0,100,by=.85)
F18= envelope(wf_2018_mi, Fest, r=r, nsim = 59, rank = 2) 
plot(F18, main="2018: F Function", xlab="Distance in mi. (r)")

#F Function: Nearest Neighbor Analysis (2016)
r= seq(0,100,by=.85)
F16= envelope(wf_2016_mi, Fest, r=r, nsim = 59, rank = 2) 
plot(F16, main="2016: F Function", xlab="Distance in mi. (r)")

#F Function: Nearest Neighbor Analysis (2011)
r= seq(0,100,by=.85)
F11= envelope(wf_2011_mi, Fest, r=r, nsim = 59, rank = 2) 
plot(F11, main="2011: F Function", xlab="Distance in mi. (r)")



## Import excel rainfall data(in. per year)  ##

df_rf_30yr= read_excel("prism_img/prism_point_30yr.xls")
summary(df_rf_30yr$in_)
#Import 2011 average rainfall per quadrat excel
df_rf11= read_excel("rawdata/avg_rf_2008_2018.xlsx")

df_linear = lm(df_rf11$wf_freq~df_rf11$avg_in)
summary(df_linear)
cor(df_rf11$wf_freq, df_rf11$avg_in, method="pearson")

plot(df_rf11$wf_freq ~ df_rf11$avg_in, main="Average Rainfall Vs. Wildfire Frequency From 2008-2018", ylab="Wildfire Freq", xlab="Average Rainfall(in inches)", pch=20, col="dark green")
abline(df_rf11_lm)
cor(df_rf11$wf_freq, df_rf11$avg_in, method="pearson")
df_rf11_lm= lm(df_rf11$wf_freq ~ df_rf11$avg_in)
summary(df_rf11_lm)


#Summary Statistics of 30 yr rainfall(in/year)
par(mfrow=c(1,2))

hist(df_rf_30yr$in_,xlab="Average Rainfall (in inches)",col="light blue", las=1, main=NULL)
boxplot(df_rf_30yr$in_, pch=20, col="light blue", las=1, ylab="Average Rainfall (in inches)")
boxplot.stats(df_rf_30yr$in_)
mean(df_rf_30yr$in_)

#### Define low rainfall <= 43.08 in/year

#Plot a frequency distributon of fires by year
year = c(2008:2018)
freq = c(176,173,164,56,188,145,144,186,197,84,129)
freq_dist = data.frame(year, freq)

plot(freq_dist$freq~freq_dist$year, pch=20, xlab="Year", ylab="No. of Fires", main="Number of Wildfires in NYS: 2008 and 2018")
lines(freq_dist$freq~freq_dist$year)

# Load an nys.shp polygon shapefile 
s <- readOGR("nys_shapefiles", "NYS_Project")  # Don't add the .shp extension
nys <- as(s, "owin") 

marks(rf_ppp) <- NULL

Window(rf_ppp) <- nys

plot(rf_ppp, main="2018", cols=rgb(0,0,0,.2), pch=20)



# Load a wf_2018.shp point feature shapefile
s <- readOGR("wf_shapefiles","wf_2018_Project")  # Don't add the .shp extension
wf_2018 <- as(s, "ppp")

marks(wf_2018) <- NULL

Window(wf_2018) <- nys

par(mfrow=c(1,3))
plot(rf_ppp, main="2018", cols=rgb(0,0,0,.2), pch=20)
plot(wf_2018, main="2018", col="magenta", pch=20)



#First-order property(Measuring Intensity)
#Quadrat Analysis 2018 
#square meters per quadrat
Q18 <- quadratcount(wf_2018, nx=3 , ny=3)

plot(wf_2018, pch=20, cols="grey70", main="2018")  # Plot points
plot(Q18, add=TRUE)  # Add quadrat grid

Window(Q18) <- rf

#First-order property(Measuring Intensity)
#Test for CSR
#chi-squared test using quadrat counts
qt = quadrat.test(wf_2018, nx=3, ny=3)
qt

#Create Rainfall Coordinate Matrix
rf_point = matrix(NA, nrow=nrow(rf), ncol=2)
rf_point[, 1] = rf$coords.x1 #LONG
rf_point[, 2] = rf$coords.x2 #LAT
colnames(rf_point) = c("Longitude", "Latitude")

plot(rf_point, pch=20)


## Working with Covariate Rainfall ##


# Load an nys.shp polygon shapefile 
s <- readOGR("nys_shapefiles", "NYS_Project")  # Don't add the .shp extension
nys <- as(s, "owin") 

# Load a wf_2018.shp point feature shapefile
s_18 <- readOGR("wf_shapefiles","wf_2018_Project")  # Don't add the .shp extension
wf_2018 <- as(s_18, "ppp")

# Load a wf_2011.shp point feature shapefile
s_11 <- readOGR("wf_shapefiles","wf_2011_Project")  # Don't add the .shp extension
wf_2011 <- as(s_11, "ppp")

# Load a wf_2016.shp point feature shapefile
s_16 <- readOGR("wf_shapefiles","wf_2016_Project")  # Don't add the .shp extension
wf_2016 <- as(s_16, "ppp")

marks(wf_2018) <- NULL
Window(wf_2018) <- nys

# Load a prism.img rainfall raster layer (2018)
img <- raster("prism_img/prism_extract_2018_lower_proj1.tif")
rf  <- as.im(img)
hist(rf)
rf.lg = log(rf)
hist(rf.lg)

summary(img)

#rescale
wf_2018_mi <- rescale(wf_2018, 1000, "mi")
wf_2016_mi <- rescale(wf_2016, 1000, "mi")
wf_2011_mi <- rescale(wf_2011, 1000, "mi")
nys.mi <- rescale(nys, 1000, "mi") 

rf_in <- rescale(rf, 1000, "in")
rf_lg_in <- rescale(rf.lg, 1000, "in")

summary(rf_18_mi)

# Compute rho using the ratio method
rho <- rhohat(wf_2018_mi, rf_in, method="ratio")

# Generate rho vs covariate plot
plot(rho, las=1, main=NULL, legendargs=list(cex=.3, xpd=TRUE, inset=c(1.01, 0) ))

pred <- predict(rho)
cl   <- interp.colours(c("lightyellow", "pink" ,"purple"), 100) # Create color scheme
plot(pred, col=cl, las=1, main=NULL)

# Create the Poisson point process model
PPM1 <- ppm(wf_2018_mi ~ rf_in)
# Plot the relationship
plot(effectfun(PPM1, "rf_in", se.fit=TRUE), main=NULL, las=1, legendargs=list(cex=0.5, xpd=TRUE, inset=c(1.01, 0) ))

PPM1

rf_18clip = mask(img,s)
rf_18_img  <- as.im(rf_18clip)
rf_18_mi <- rescale(rf_18_img, 1000, "mi")
rf_18_log = log(rf_18_mi)

#Quadrat density on a tessellated surface
brk  <- c( -Inf, 4, 6, 8 , Inf)  # Define the breaks
Zcut <- cut(rf_18_mi, breaks=brk, labels=1:4)  # Classify the raster
E    <- tess(image=Zcut)  # Create a tesselated surface
plot(E, main="", las=1)

#Next, tally the quadrat counts within each tessellated area then compute 
#their density values (number of points per quadrat area).
wf_2018_mi <- rescale(wf_2018, 1000, "mi")
Q_18   <- quadratcount(wf_2018_mi, tess = E)  # Tally counts
Q.d_18 <- intensity(Q_18)  # Compute density
Q.d_18

#Plot the density values across each tessellated region.
plot(intensity(Q_18, image=TRUE), las=1, main=NULL)
plot(wf_2018_mi, pch=20, cex=0.6,cols="light grey", add=TRUE)


ANN18 <- apply(nndist(wf_2018_mi, k=1:150),2,FUN=mean)
plot(ANN18 ~ eval(1:150), type="b", las=1, pch=20,ylab="", xlab="Neighbor Order Number", main="Average Nearest Neighbor Dist. Between WF:2018 ")

ANN16 <- apply(nndist(wf_2016_mi, k=1:200),2,FUN=mean)
plot(ANN16 ~ eval(1:200), type="b", las=1, pch=20,ylab="", xlab="Neighbor Order Number", main="Average Nearest Neighbor Dist. Between WF:2016 ")

ANN11 <- apply(nndist(wf_2011_mi, k=1:80),2,FUN=mean)
plot(ANN11 ~ eval(1:80), type="b", las=1, pch=20,ylab="", xlab="Neighbor Order Number", main="Average Nearest Neighbor Dist. Between WF:2011 ")

ann.p <- mean(nndist(wf_2018_mi, k=1))
ann.p

n     <- 599L               # Number of simulations
ann.r <- vector(length = n) # Create an empty object to be used to store simulated ANN values
for (i in 1:n){
  rand.p   <- rpoint(n=wf_2018_mi$n, win=nys.mi)  # Generate random point locations
  ann.r[i] <- mean(nndist(rand.p, k=1))  # Tally the ANN values
}

plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

n     <- 599L               # Number of simulations
ann.r <- vector(length = n) # Create an empty object to be used to store simulated ANN values
for (i in 1:n){
  rand.p   <- rpoint(n=wf_2018_mi$n, f=rf_in)  # Generate random point locations
  ann.r[i] <- mean(nndist(rand.p, k=1))  # Tally the ANN values
}
Window(rand.p) <- nys.mi  # Replace raster mask with ma.km window
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

N.greater <- sum(ann.r > ann.p)
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1)
p