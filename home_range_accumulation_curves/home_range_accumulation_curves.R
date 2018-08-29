#---------------------------------------------------
#
# Creating home range accumulation curves 
# for animal movement data
#
# Until now, only using MCP
#
# This script is greatly based on previous code by
# Carlos A. Zucco and Luiz Gustavo Oliveira-Santos
# (Thanks to them!)
#
# Bernardo Niebuhr <bernardo_brandaum@yahoo.com.br>
# Aug. 2018
#---------------------------------------------------

# Load packages 
if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load('move', 'adehabitatHR')

# Download data
# Here we are going to get movement data of
# Turdus birds in Brazil from MoveBank

# First you have to login on MoveBank with you 
# MoveBank username and password
# (I am ommitting that from here)
movebank_login <- movebankLogin(username = "YourUserName", 
                                password = "YourPassword")

# Now we search for studies with Turdus
studies <- searchMovebankStudies(x = "Turdus", login = movebank_login)
# We're going to download data from the second one, in Southeastern Brazil
studies[2]

# Now, first we have to login MoveBank through the Browser, 
# search for this study, and accept the license terms for 
# downloading it there. Then we can download it from here

# Now we can download it here directly
turdus <- getMovebankData(study=studies[2], login=movebank_login)
turdus
plot(turdus, pch = 20)

# Transform from LAT-LON to UTM Zone 22S (so that home range units make sense)
turdus.utm <- spTransform(turdus, CRS('+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))
ids <- unique(turdus.utm$tag_id)

### MCP 100%
### Parameters to control
MCP_percentage <- 95
cumHRmcp <- list() ## List with cumulative sample size for all individuals
for (i in 1:length(ids)) {  ## loop for individuals
  temp <- turdus.utm[which(turdus.utm$tag_id == ids[i]),] # select an individual
  # Here we do not use the points as they are, but randomize their order using 'sample'
  temp <- SpatialPoints(coordinates(temp)[sample(length(temp)),], CRS(proj4string(turdus.utm)))
  cumulative <- vector()
  for(k in 5:length(temp)){  ##loop for sample size from 5 locations to all locations
    cumulative[k] <- mcp.area(temp[1:k,], percent = MCP_percentage, plotit = F)
  }  
  cumHRmcp[[i]] <- data.frame(hr = unlist(cumulative), ssize = 5:length(temp))
}

names(cumHRmcp) <- ids
cumHRmcp

# Plotting
par(mar = c(5, 4, 4, 2) + 0.1)
# Seeing cummulative MCP area plots
for(i in 1:length(ids)) { ## plot all curves with 2 seconds interval
  plot(cumHRmcp[[i]]$hr ~ cumHRmcp[[i]]$ss, cex=0.5, pch=16, main=ids[i],
       xlab="Number of locations",ylab="MCP 100% area (ha)" )
  points(cumHRmcp[[i]]$hr ~ cumHRmcp[[i]]$ss, type="l", lwd=0.7, lty=2)
  Sys.sleep(1)
}

#------------------
# Now we're going to repeat that several times per individual

iterations <- 20

### MCP 100%
### Parameters to control
MCP_percentage <- 95
cumHRmcp <- list() ## List with cumulative sample size for all individuals
for (i in 1:length(ids)) {  ## loop for individuals
  print(i)
  cumHRmcp[[i]] <- list()
  for(j in 1:iterations) { # Loop for iterations
    temp <- turdus.utm[which(turdus.utm$tag_id == ids[i]),]
    # Here we do not use the points as they are, but randomize their order
    temp <- SpatialPoints(coordinates(temp)[sample(length(temp)),], CRS(proj4string(turdus.utm)))
    cumulative <- vector()
    for(k in 5:length(temp)){  ##loop for sample size from 5 locations to all locations
      cumulative[k] <- mcp.area(temp[1:k,], percent = MCP_percentage, plotit = F)
    }  
    cumHRmcp[[i]][[j]] <- data.frame(hr = unlist(cumulative), ssize = 5:length(temp))
  }
}

names(cumHRmcp) <- ids
cumHRmcp

# Plotting
par(mar = c(5, 4, 4, 2) + 0.1)
# Seeing cummulative MCP area plots
for(i in 1:length(ids)) { ## plot all curves with 2 seconds interval
  cum.df <- as.data.frame(cumHRmcp[[i]])
  cumHR.df.mult <- cum.df[,c(2,seq(1, ncol(cum.df), 2))]
  
  cumHR.df.mult2 <- data.frame(npoins = rep(cumHR.df.mult[,1], iterations), HR = unlist(c(cumHR.df.mult[,2:ncol(cumHR.df.mult)])))
  plot(cumHR.df.mult2$npoins, cumHR.df.mult2$HR, cex=0.5, pch=16, col = 'grey', main=ids[i],
       xlab="Number of locations",ylab="MCP 100% area (ha)" )
  points(unique(cumHR.df.mult2$npoins), apply(cumHR.df.mult[,2:ncol(cumHR.df.mult)], MARGIN = 1, FUN = median), type="l", lwd=3, lty=1)
  
  Sys.sleep(1)
}

# Another way of plotting, with boxplot
par(mar = c(5, 4, 4, 2) + 0.1)
# Seeing cummulative MCP area plots
for(i in 1:length(ids)) { ## plot all curves with 2 seconds interval
  cum.df <- as.data.frame(cumHRmcp[[i]])
  cumHR.df.mult <- cum.df[,c(2,seq(1, ncol(cum.df), 2))]
  
  cumHR.df.mult2 <- data.frame(npoins = rep(cumHR.df.mult[,1], iterations), HR = unlist(c(cumHR.df.mult[,2:ncol(cumHR.df.mult)])))
  boxplot(cumHR.df.mult2$HR ~ as.factor(cumHR.df.mult2$npoins), 
          main = '', xlab="Number of locations",ylab="MCP 100% area (ha)")
  Sys.sleep(1)
}
