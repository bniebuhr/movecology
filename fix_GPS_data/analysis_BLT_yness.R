#########################################################################
# 
# FIXING GPS DATA AND PREPARING IT FOR MOVEMENT ANALYSES
#
# Karl Mokross - kmkross at gmail.com
# Bernardo Niebuhr - bernardo_brandaum at yahoo.com.br
#
# Code includes:
# -REMOVING LARGE EMPTY GAPS (30 MINS.)
# -REGULARIZING TRAJECTORIES 
# -FILLING SMALL GAPS (by interpolation)
# -APPLYING A KALMAN FILTER (Smoothing trajectories)
# -APPLYING A GAL FILTER (Correcting extreme wrong moves)
# -OBTAINING MOVEMENT PARAMETERS
# -EXTRACTING DISTANCE FROM EDGE FROM RASTER MAPS
#
# Feel free to modify, improve, and share this code
#
#########################################################################


#######
# Loading libraries
if(!require(ggplot2)) { install.packages("ggplot2", dep = T); library(ggplot2) }
if(!require(adehabitatLT)) { install.packages("adehabitatLT", dep = T); library(adehabitatLT) }
if(!require(zoo)) { install.packages("zoo", dep = T); library(zoo) }
if(!require(MARSS)) { install.packages("MARSS", dep = T); library(MARSS) }
if(!require(rgdal)) { install.packages("rgdal", dep = T); library(rgdal) }
if(!require(raster)) { install.packages("raster", dep = T); library(raster) }

# Loading data
setwd("/home/leecb/Dropbox/edges_MLP/New_code")

dat_aux <- read.table("new_table_BLT.csv", sep="\t", header=T)
head(dat_aux)
str(dat_aux)

#######
# Calculating movement variables and plotting bursts

# removing long sequences of coordinates and behavior data == NA
dat <- dat_aux[!(is.na(dat_aux$COOR_LAT_ML1) & is.na(dat_aux$COOR_LONG_ML1) & 
                       is.na(dat_aux$BHV_ML1) & is.na(dat_aux$FORAGING)),]

# Karl, eu nao sei qual eh esse time zone AMT, mas eh um que eu encontrei que
# nao eh sensivel ao horario de verao, o que eh importante pra gente, pra nao 
# gerar confusao... se tiver alguma outra sugestao (GMT?), fique a vontade
# [e depois pode deletar essas linhas]
dat$date <- paste(dat$DATE, dat$TIME_EE)
dat$date <- as.POSIXct(dat$date, format="%d/%m/%Y %Hh%M", tz = "Brazil/West")

# we have to do this not to lose data as factors later.
# is there a fancy way to do this?
for (i in c(2:3, 6:11))
  dat[,i] <- as.character(dat[,i])

# Make a two-column matrix, col1 = long, col2 = lat
xy <- cbind(dat$COOR_LONG_ML1, dat$COOR_LAT_ML1)
# Convert it to UTM coordinates, zone 22S, datum WGS84 (unit = meters)
xy2 <- project(xy, "+proj=utm +datum=WGS84 +zone=22 +south +ellps=WGS84 +towgs84=0,0,0")

bursts_aux <- dat$DATE # initial bursts = DATE
# transforming data into ltraj paths
Ldat <- as.ltraj(xy = xy2, date = dat$date, id = 1, burst=bursts_aux, 
                 typeII=T, infolocs = dat[,c(1:11)])

# diagnostic
plotltr(Ldat[3], "dt/3600/24")
Ldat[[3]]
plot(Ldat[3])

#==================================
# function to cut bursts with gaps larger than 30 mins
# the warning message is because there are isolated relocations 
# within bursts that are not enough to make for a trajectory 
# (needs at least 3 relocation points)
foo <- function(dt) {
  return(dt> (1800))
}
Ldat2 <- cutltraj(Ldat, "foo(dt)", nextr = TRUE)

#==================================
# Regularizing trajecotries
refda <- strptime("00:00", "%H:%M")
Ldat3 <- setNA(Ldat2, refda, 5, units = "min") #place missing values in gaps (traj not regular yet)
plotltr(Ldat3[1:9], "dt/3600/24")
Ldat3[[1]]
Ldat2[[1]]
Ldat.reg <- sett0(Ldat3, refda, 5, units = "min") #regularizing trajectories for real
plotltr(Ldat.reg[1:9], "dt/3600/24")
plot(Ldat.reg[3])

#==================================
# Interpolations must be done burst by burst (a loop)
# otherwise whack positions are be generated due to 
# possible missing points between bursts 
# (i.e. end of burst is interpolated to beginning of next) 
Ldat.reg.df<-ld(Ldat.reg) #in order to count bursts (gotta figure out a better way)
head(Ldat.reg.df)
Ldat.fix<-NULL
for (i in 1:length(levels(Ldat.reg.df$burst))) {
  Ldat.fill<-transform(Ldat.reg[[i]], 
                       xfill = na.approx(x, date, na.rm=FALSE),
                       yfill = na.approx(y, date, na.rm=FALSE))
  Ldat.fix<-rbind(Ldat.fix,Ldat.fill)
}
nrow(Ldat.fix)

# original coordinates
orig.xy <- Ldat.fix[,c(1,2)]
colnames(orig.xy) <- c("orig.x", "orig.y")

#==================================
# NOTE: figure out a more elegant way to incorporate rec function? 
# Recalculating Ltraj
Ltrj.fixed <- as.ltraj(xy = Ldat.fix[,c("xfill","yfill")], date=Ldat.fix$date, id=1, burst = Ldat.reg.df$burst, typeII = T,
                       infolocs = cbind(Ldat.reg.df[,13:23], orig.xy))
plot(Ltrj.fixed)
Ltrj.fixed.df <- ld(Ltrj.fixed)

#=================================
#Kalman filter
coord = Ltrj.fixed.df[,1:2] #data prep 1 - isolate coords
dt =(t(coord)) #data prep 2- transpose
#filtering
Z.model="identity"
U.model="unequal"
Q.model="diagonal and unequal"
R.model="diagonal and unequal"
control=list(maxit=17500)
kemx = MARSS(dt, model=list(Z = Z.model,Q = matrix(c(20,0,0,20),2,2),
                            R = matrix(c(2,0,0,7),2,2), U = U.model))

#preparing filtered coordinates for visualization 
#and for adding them to dataframe with movmnt parameters
Ltrj.fixed.df$kfilt.x = kemx$states[1,]
Ltrj.fixed.df$kfilt.y = kemx$states[2,]

#==============pa===================
#visualizing data - compare filtered relocations to raw ones
#Suggestion: include codeline for viz individual bursts
op <- par(mai = c(0,0,0,0),mfrow=c(1,1))
#windows()
plot(Ltrj.fixed.df$kfilt.x, Ltrj.fixed.df$kfilt.y)
lines(Ltrj.fixed.df$kfilt.x, Ltrj.fixed.df$kfilt.y,col="red", lwd=1)
lines(Ltrj.fixed.df$x, Ltrj.fixed.df$y, col="blue", lwd=1)
par(op)

#=================================
# GAL (Grab Absurd Location) Filter
# Applying and visualizing, comparing with original data and Kalman Filter

# This is also not elegant, but it works

# when errototal is smaller, more extreme (absurd) locations are relocated 
# or: when errototal is greater, the filter is more sensible to correct 
# not so fast go-and-comeback moves, and the trajectory is smoother; otherwise,
# it corrects only very extreme moves
errototal <- 1/5

pdf("filters.pdf")
#cont <- 1
gfilt.x <- c()
gfilt.y <- c()
for(i in levels(Ltrj.fixed.df$burst))
{
  
  brst <- subset(Ltrj.fixed.df, burst == i)
  coord = brst[,1:2]
  
  for(j in 1:(nrow(brst)-2))
  {
    d <- sqrt((coord[j,1] - coord[(j+2),1])**2 + (coord[j,2] - coord[(j+2),2])**2)
    d1 <- brst$dist[j]
    d2 <- brst$dist[(j+1)]
    p.straight <- d/(d1+d2)
    
    if(!(is.na(d) | is.na(d1+d2)) & ((d1+d2) > 0 & d > 0))
    {
      #print(p.straight)
      if(p.straight < errototal)
      {
        coord[(j+1),1] <- coord[j,1] + (coord[(j+2),1] - coord[j,1])/2
        coord[(j+1),2] <- coord[j,2] + (coord[(j+2),2] - coord[j,2])/2 
      }
      #print(cont)
      #cont <- cont + 1
    }
    
  }
  
  
  ##### visualizing correction
  
  #plotando resultados vs. originais
  pred.lon = coord[,1]
  pred.lat = coord[,2]
  #par(mai = c(0,0,0,0),mfrow=c(1,1))
  #windows()
  xmax <- max(brst$x, brst$kfilt.x, na.rm = T)
  xmin <- min(brst$x, brst$kfilt.x, na.rm = T)
  ymax <- max(brst$y, brst$kfilt.y, na.rm = T)
  ymin <- min(brst$y, brst$kfilt.y, na.rm = T)
  
  plot(pred.lon, pred.lat, type="n", main = i, ylab = "y", xlab = "x",
       xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  lines(brst$x, brst$y, col="black", lwd=1)
  points(brst$x, brst$y, col="black", lwd=1) 
  lines(pred.lon, pred.lat, col="red", lwd=2)
  points(pred.lon, pred.lat, col="red", lwd=2)
  lines(brst$kfilt.x, brst$kfilt.y, col="blue", lwd=2)
  points(brst$kfilt.x, brst$kfilt.y, col="blue", lwd=2)
  
  legend("topleft", legend = c("original", "Kalman", "GAL"), lwd = rep(1.5, 3), col=c(1,4,2))
  
  gfilt.x = c(gfilt.x, coord[,1])
  gfilt.y = c(gfilt.y, coord[,2])
  
}
dev.off()
Ltrj.fixed.df$gfilt.x <- gfilt.x
Ltrj.fixed.df$gfilt.y <- gfilt.y

# This is to compare both approaches. 
# Here I am going to maintain only GAL Filter (more close to real data, relocating only
# most extreme moves), but we could apply first GAL Filter and then Kalman Filter.

#===================================
#Recalculating ltraj after filter
Ltrj.fixed.df$interp.notfilt.x <- Ltrj.fixed.df$x
Ltrj.fixed.df$interp.notfilt.y <- Ltrj.fixed.df$y
Ltrj.fixed.df$x <- Ltrj.fixed.df$gfilt.x
Ltrj.fixed.df$y <- Ltrj.fixed.df$gfilt.y

Ltrj.filt <- rec(dl(Ltrj.fixed.df))

plot(Ltrj.filt)

#pass as dataframe
Ltrj.filt.df <- ld(Ltrj.filt)
Ltrj.filt.df <- Ltrj.filt.df[(order(Ltrj.filt.df$date)),] # organizing by date

#===================================
# Extracting landscape data to points

# Here we extract distance from fixes to forest edges from raster maps with this information
# but any other raster maps could be used as landscape variables

# dir where maps are located
setwd("../maps")

# Loading raster maps
edge.dist <- raster("area_estudo_SM_1_wgs_latlon_to_wgs_UTM22S_polygon_to_raster_EDGES_dist.tif")
#plot(edge.dist)
edge.dist.north <- raster("area_estudo_SM_1_wgs_latlon_to_wgs_UTM22S_polygon_to_raster_EDGES_norte_dist.tif")
#plot(edge.dist.north)
edge.dist.west <- raster("area_estudo_SM_1_wgs_latlon_to_wgs_UTM22S_polygon_to_raster_EDGES_oeste_dist.tif")
#plot(edge.dist.west)

# Extracting distances to points
dist <- extract(edge.dist, Ltrj.filt.df[,c("x","y")])
dist.north <- extract(edge.dist.north, Ltrj.filt.df[,c("x","y")])
dist.west <- extract(edge.dist.west, Ltrj.filt.df[,c("x","y")])

#write relevant data (gal filter + parameters, field data + behavior data + edge data, original coordinates, kalman filtered coordinates, GAL filtered coordinates, pre-filter interpolated coordinates)
master.sheet.yness <- cbind(Ltrj.filt.df[1:23], dist, dist.north, dist.west, Ltrj.filt.df[24:31])

# code directory
setwd("../New_code/")
write.table(master.sheet.yness,"Yness_mastersheet.csv", sep="\t", row.names = F)
###THE END#####