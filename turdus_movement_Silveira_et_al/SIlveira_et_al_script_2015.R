########################################################
#
# Movement analysis for thrushes in
# fragmented landscapes
#
# Natalia Stefanini - nat.stefanini at gmail.com
# Bernardo Niebuhr - bernardo_brandaum at yahoo.com.br
# Dec. 2014
# No rights reserved - feel free to modify and share  
#
#########################################################

# bla bla bla

###################
# Loading packages
if(!require(adehabitatLT)) install.packages("adehabitatLT", dep=T); library(adehabitatLT)
if(!require(bbmle)) install.packages("bbmle", dep=T); library(bbmle)
if(!require(circular)) install.packages("circular", dep=T); library(circular)
if(!require(vioplot)) install.packages("vioplot", dep=T); library(vioplot)
if(!require(devtools)) install.packages("devtools", dep=T); library(devtools)
require(digest)

###################
# Loading and organizing data
rm(list=ls())

# Changing working directory
#setwd("C:/Users/ber/Documents/Documentos_ber/Atividades 2014/analises/Nat_movimento_sabias/Manuscrito_movement/Analises_finais")
#setwd("C:/Users/Nat/Documents/paper_movimento")
#setwd("/media/windows/Users/ber/Documents/Documentos_ber/Atividades 2014/analises/dados_Nat_sabias/movement/")
#setwd("~/Documentos_ber/Atividades 2014/analises/dados_Nat_sabias/movement")
setwd("G:/Users/ber/Documents/Documentos_ber/Atividades 2014/analises/Nat_movimento_sabias/Manuscrito_movement/Analises_finais")
source("lik.int_source_code.R")
source("vioplot3_source_code.R")

# Reading data
data <- read.table("dados_final.csv", sep=",",dec=".", header=TRUE)

head(data)
str(data)
names(data)=c("nome", "sexo", "data","hora", "especie", "xestimate", "yestimate",
               "id_poligono", "classe", "classe_bin", "peso", "sitio", "dist_borda")

data$prox_borda <- ifelse(data$dist_borda > 0, data$dist_borda, -data$dist_borda)

# Transforming data into ltraj class
data$data<-as.character(data$data)
data$hora<-as.character(data$hora)

da <- paste(data$data, data$hora)
da

bursts <- paste(data$nome, data$data, sep="_")

da1 <- as.POSIXct(strptime(da, format="%Y-%m-%d %H:%M"))

path <- as.ltraj(xy = data[,c("xestimate", "yestimate")], date = da1, id = data$nome,
                 burst=bursts, typeII=T, infolocs=data[,c("sexo", "especie", "id_poligono",
                                                          "classe", "classe_bin",
                                                          "peso", "sitio", "dist_borda",
                                                          "prox_borda")])

path
plot(path)
# Transforming the paths into data.frame class
path.df <- ld(path)
str(path.df)

# 3 classes
path.df$class3 <- as.character(path.df$classe_bin)
path.df$class3[path.df$classe == "res"] <- "URB"
path.df$class3 <- as.factor(path.df$class3)

# Deleting fixes with dt > 2h = 7200s
path.df <- path.df[!is.na(path.df$dist),]
path.df <- path.df[path.df$dt < 7200,]

# Separating dependent variables
dist <- path.df$dist
time <- path.df$dt[!is.na(path.df$dt)]

# Average speed
velo <- dist/time*60
plot(velo, path.df$dt,cex=1)

# Turning angles
angle <- path.df$rel.angle[!is.na(path.df$rel.angle)]

# Separating independent variables
# for Y = Mean speed
class.bin.v = path.df$classe_bin
species.v = as.factor(path.df$especie)
site.v = as.factor(path.df$sitio)
sex.v = as.factor(path.df$sexo)
class3.v = path.df$class3
distedge.v = path.df$dist_borda
proxedge.v = path.df$prox_borda

# for Y = Turning angles
class.bin.a = path.df$classe_bin[!is.na(path.df$rel.angle)]
species.a = as.factor(path.df$especie[!is.na(path.df$rel.angle)])
site.a = as.factor(path.df$sitio[!is.na(path.df$rel.angle)])
sex.a = as.factor(path.df$sexo[!is.na(path.df$rel.angle)])
class3.a = path.df$class3[!is.na(path.df$rel.angle)]
distedge.a = path.df$dist_borda[!is.na(path.df$rel.angle)]
proxedge.a = path.df$prox_borda[!is.na(path.df$rel.angle)]

##############
# Visual data exploration

plot(velo ~ class.bin.v)

par(mfrow=c(1,3))

plot(velo ~ class3.v, xlab="Three cover classes", ylab="Average speed (m/min)", 
    names=c("Forest","Other", "Urban Areas"),cex= 1,cex.lab=1, cex.axis=1,boxwex=0.6)

plot(velo ~ sex.v,xlab="Sex", ylab="Average speed (m/min)", 
    names=c("Male", "Female"),cex.lab=1,cex=1, cex.axis=1,boxwex=0.6)

plot(velo ~ species.v,xlab="Species", ylab="Average speed (m/min)", 
    names=c("T. leucomelas","T. rufiventris"),cex=1,cex.lab=1, cex.axis=1,boxwex=0.6)


plot(density(velo))

plot(density(velo[class.bin.v == "MATA"]), col = 2)
lines(density(velo[class.bin.v != "MATA"]), col = 1)

plot(density(velo[site.v == 1]), col = 2)
lines(density(velo[site.v == 2]), col = 1)
lines(density(velo[site.v == 3]), col = 3)

plot(density(velo[sex.v == 1]), col = 2)
lines(density(velo[sex.v == 2]), col = 1)

plot(density(velo[species.v == 1]), col = 2)
lines(density(velo[species.v == 2]), col = 1)

hist(distedge.v, breaks = 30)
hist(distedge.v[class3.v == "URB"], breaks = 30)

plot(angle ~ class.bin.a)
plot(angle ~ class3.a)
plot(angle ~ sex.a)
plot(angle ~ species.a)

plot(density(angle))
hist(angle)

plot(density(angle[class.bin.a == "MATA"]), col = 2)
lines(density(angle[class.bin.a != "MATA"]), col = 1)

##############
# Species
# 1 = T. leucomelas
# 2 = T. rufiventris

##############
# Sex
# 1 = male
# 2 = female

##############
# Class
# 1 = forest
# 2 = matrix


##### MODELS ######

# Y = mean speed or turning angles

#####
# For Y = Mean speed

### Defining models
# Exponential models

LLexp <- function(lambda){
  -sum(dexp(velo, rate=lambda, log=T))
}

# Univariate models
LLexp1 <- function(a, b){
  lambda = c(a, b)[class.bin.v]
  -sum(dexp(velo, rate=lambda, log=T))
}

LLexp1.5 <- function(a, b, c){
  lambda = c(a, b, c)[class3.v]
  -sum(dexp(velo, rate=lambda, log=T))
}

LLexp2 <- function(a, b){
  lambda = c(a, b)[species.v]
  -sum(dexp(velo, rate=lambda, log=T))
}

LLexp3 <- function(a, b){
  lambda = c(a, b)[sex.v]
  -sum(dexp(velo, rate=lambda, log=T))
}


LLexp4 <- function(a, b){
  lambda = exp(a + b*distedge.v)
  -sum(dexp(velo, rate=lambda, log=T))
}

LLexp4.1 <- function(a, b){
  lambda = exp(a + b*proxedge.v)
  -sum(dexp(velo, rate=lambda, log=T))
}

# Multivariate models
LLexp5 <- function(a, b, c){
  lambda = c(a, b)[sex.v] + c(0, c)[class.bin.v]
  -sum(dexp(velo, rate=lambda, log=T))
}

LLexp5.5 <- function (a,b,c,d){
  lambda = c(a, b)[sex.v] + c(0, c, d)[class3.v]
  -sum(dexp(velo, rate=lambda, log=T))
}

LLexp6 <- function(a, b, c){
  lambda = c(a, b)[species.v] + c(0, c)[class.bin.v]
  -sum(dexp(velo, rate=lambda, log=T))
}

LLexp6.5 <- function (a,b,c,d){
  lambda = c(a, b)[species.v] + c(0, c, d)[class3.v]
  -sum(dexp(velo, rate=lambda, log=T))
}


LLexp8 <- function(a, b, c, d) {
  lambda = exp(c(a, b)[class.bin.v] + c(c, d)[class.bin.v]*proxedge.v)
  -sum(dexp(velo, rate=lambda, log=T))
}

LLexp8.5 <- function(a, b, c, d, e, f) {
  lambda = exp(c(a, b, c)[class3.v] + c(d, e, f)[class3.v]*proxedge.v)
  -sum(dexp(velo, rate=lambda, log=T))
}


### Fitting models
mexp <- mle2(LLexp, start=list(lambda=1/mean(velo)))

mexp1 <- mle2(LLexp1, start=list(a = 1/mean(velo), b = 1/mean(velo)))
#confint(mexp1)

mexp1.5 <- mle2(LLexp1.5, start=list(a = 1/mean(velo), b = 1/mean(velo), c = 1/mean(velo)))

mexp2 <- mle2(LLexp2, start=list(a = 1/mean(velo), b = 1/mean(velo)))
#confint(mexp2)

mexp3 <- mle2(LLexp3, start=list(a = 1/mean(velo), b = 1/mean(velo)))
#confint(mexp3)

a.tent = log(coef(glm(velo ~ proxedge.v, family = "Gamma"))[1])
b.tent = coef(glm(velo ~ proxedge.v, family = "Gamma"))[2]
mexp4.1 <- mle2(LLexp4.1, start=list(a = a.tent, b = b.tent))

mexp5 <- mle2(LLexp5, start=list(a = 1/mean(velo), b = 1/mean(velo), c = 1/mean(velo)))
#confint(mexp5)

mexp5.5 <- mle2(LLexp5.5, start=list(a = 1/mean(velo), b = 1/mean(velo), c = 1/mean(velo), d = 1/mean(velo)))
#confint(mexp5.5)

mexp6 <- mle2(LLexp6, start=list(a = 1/mean(velo), b = 1/mean(velo), c = 1/mean(velo)))
#confint(mexp6)

mexp6.5 <- mle2(LLexp6.5, start=list(a = 1/mean(velo), b = 1/mean(velo), c = 1/mean(velo), d = 1/mean(velo)))
#confint(mexp6.5)

# different intercepts
a.tent = log(coef(glm(velo ~ proxedge.v, family = "Gamma"))[1])
b.tent = coef(glm(velo ~ proxedge.v, family = "Gamma"))[2]
mexp8 <- mle2(LLexp8, start=list(a = a.tent, b = a.tent, c = b.tent, d = b.tent))

a.tent = log(coef(glm(velo ~ proxedge.v, family = "Gamma"))[1])
b.tent = coef(glm(velo ~ proxedge.v, family = "Gamma"))[2]
mexp8.5 <- mle2(LLexp8.5, start=list(a = a.tent, b = a.tent, c = a.tent, d = b.tent, e = b.tent, f = b.tent))




### Comparing models
(comp <- AICctab(mexp, mexp1, mexp2, mexp3, mexp4, mexp5,mexp5.5, mexp6,mexp6.5, mexp7, 
                 mexp1.5, mexp7.5, mexp4, mexp8, mexp8.5,
                 base=T, weights=T, nobs = length(velo)))

### Plotting models and data
# op <- par(mfrow=c(1,2))
# hist(velo, breaks=50, prob=T, xlab="Average speed",
#      ylab="Probability density", main="")
# curve(dexp(x, rate=coef(mexp)), col=4, add=T) # M0 - no effect
# 
# hist(velo[class.bin.v == "N_MATA"], breaks=50, prob=T, xlab="Average speed",
#      ylab="Probability density", main="", border="red")
# lines(density(velo[class.bin.v == "N_MATA"]))
# hist(velo[class.bin.v == "MATA"], breaks=50, prob=T, lty=2, add=T)
# lines(density(velo[class.bin.v == "MATA"]))
# curve(dexp(x, rate=coef(mexp)), col=4, add=T) # M0 - no effect
# curve(dexp(x, rate=coef(mexp1)[1]+coef(mexp1)[2]), col=2, add=T) # M6 - best model
# curve(dexp(x, rate=coef(mexp1)[1]), col=1, lty=2, add=T) # M6 - best model
# legend("topright", legend = c("matrix", "forest", "no effect model"), col = c(2,1,4), 
#        lty = c(1,2,1), cex=0.7)
# par(op)

coeff <- coef(mexp8.5)
matmax <- max(proxedge.v[class.bin.v != "MATA"])
habmax <- max(proxedge.v[class.bin.v == "MATA"])

# all classes together, different intercepts
plot(0, 0, type = "n", xlim = c(-matmax, habmax), ylim = c(0, max(velo)), xlab = "Distance from Edge (meters)", family="serif",
     ylab = "Average Speed (m/min)", family="serif", cex.lab=1.2, cex.axis=1.2)
abline(v = 0, col = "dark green", lty = 2, lwd=2)
curve(1/(exp(coeff[1] + coeff[4]*x)), 0, habmax, col = "chartreuse3",lwd=2, add=T)
curve(1/(exp(coeff[3] - coeff[6]*x)), -matmax, 0, col = "red", lwd=2, add = T)
curve(1/(exp(coeff[2] - coeff[5]*x)), -matmax, 0, col = "blue",lwd=2,  add = T)



###
# Another possibility of model is "bounded exponential models", i.e, an exponential model
# which is truncated by a minimum and maximum value. The function is found below, but we
# are not going to try that, it is probably going to give a similar result (without big 
# differences in distribution behavior)

# Bounded exponential models
dbexp <- function(x, rate, a = min(velo), b = max(velo)+10, log = F){
  cc <- integrate(dexp, lower = min(velo), upper = max(velo)+10, rate)$value
  if(log) ifelse(x < a | x > b, 0, dexp(x, rate, log = T)/cc)
  else ifelse(x < a | x > b, 0, dexp(x, rate, log = F)/cc)
}


###
# Weibull distribution models

LLwei <- function(forma, escala) {
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}

# Univariate models
LLwei1 <- function(a, b, escala) {
   forma = exp(c(a, b)[class.bin.v])
   -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}

LLwei1.5 <- function(a, b, c, escala) {
  forma = exp(c(a, b, c)[class3.v])
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}

LLwei2 <- function(a, b, escala) {
  forma = exp(c(a, b)[species.v])
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}

LLwei3 <- function(a, b, escala) {
  forma = exp(c(a, b)[sex.v])
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}


LLwei4 <- function(a, b, escala) {
  forma = exp(a + b*distedge.v)
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}



# Multivariate models
LLwei5 <- function(a, b, c, escala) {
  forma = exp(c(a, b)[sex.v] + c(0, c)[class.bin.v])
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}

LLwei5.5 <- function(a, b, c, d, escala) {
  forma = exp(c(a, b)[sex.v] + c(0, c, d)[class3.v])
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}

LLwei6 <- function(a, b, c, escala) {
  forma = exp(c(a, b)[species.v] + c(0, c)[class.bin.v])
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}

LLwei6.5 <- function(a, b, c, d, escala) {
  forma = exp(c(a, b)[species.v] + c(0, c, d)[class3.v])
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}


LLwei8 <- function(a, b, c, d, escala) {
  forma = exp(c(a, b)[class.bin.v] + c(c, d)[class.bin.v]*proxedge.v)
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}

LLwei8.5 <- function(a, b, c, d, e, f, escala) {
  forma = exp(c(a, b, c)[class3.v] + c(d, e, f)[class3.v]*proxedge.v)
  -sum(dweibull(velo, shape=forma, scale=escala, log=T))
}



### Fitting models to data
mwei <- mle2(LLwei, start=list(forma = mean(velo)^2/var(velo), escala = var(velo)/mean(velo)))
esc <- coef(mwei)[2]

mwei1 <- mle2(LLwei1, start=list(a = 1, b = 1, escala = esc))

mwei1.5 <- mle2(LLwei1.5, start=list(a = 1, b = 1, c = 1, escala = esc))

mwei2 <- mle2(LLwei2, start=list(a = 1, b = 1, escala = esc))

mwei3 <- mle2(LLwei3, start=list(a = 1, b = 1, escala = esc))

mwei4 <- mle2(LLwei4.1, start=list(a = 0, b = 0, escala = esc))

mwei5 <- mle2(LLwei5, start=list(a = 0, b = 0, c = 0, escala = esc))

mwei5.5 <- mle2(LLwei5.5, start=list(a = 0, b = 0, c = 0, d = 0, escala = esc))

mwei6 <- mle2(LLwei6, start=list(a = 0, b = 0, c = 0, escala = esc))

mwei6.5 <- mle2(LLwei6.5, start=list(a = 0, b = 0, c = 0, d = 0, escala = esc))

mwei8 <- mle2(LLwei8, start=list(a = 0, b = 0, c = 0, d = 0, escala = esc))

mwei8.5 <- mle2(LLwei8.5, start=list(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, escala = esc))



### Comparing models
(comp <- AICctab(mwei, mwei1, mwei2, mwei3, mwei4, mwei5, mwei5.5, mwei6, mwei6.5, mwei7,
                 mwei1.5, mwei4, mwei8, mwei8.5,
                 base=T, weights=T, nobs=length(velo)))
# 
(comp <- AICctab(mexp, mexp1, mexp2, mexp3, mexp4, mexp5, mexp6,mexp6.5, mexp5.5,
                 mexp1.5, mexp8, mexp8.5, mwei, mwei1, mwei2, mwei3, mwei4, mwei5, mwei6,
                 mwei1.5, mwei8, mwei8.5, mwei6.5, mwei5.5,
                 base=T, weights=T, nobs=length(velo)))

###
# Levy distribution models (maybe we should truncate these models later, it more realistic!)

# Power-law distribution
dpowlaw <- function(x, alfa, xmin, log=FALSE){
  c <- (alfa-1)*xmin^(alfa-1)
  if(log) ifelse(x < xmin, 0, log(c*x^(-alfa)))
  else ifelse(x < xmin, 0, c*x^(-alfa))
}

integrate(dpowlaw, -Inf, Inf, alfa=2, xmin=1)
curve(dpowlaw(x, alfa=2.5, xmin=10), from=0, to=100, log="")
curve(dpowlaw(x, alfa=2.5, xmin=1), from=1, to=100, log="xy")

LLlevy <- function(mu, xmin){
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}

# Univariate models
LLlevy1 <- function(a, b, xmin){
  mu = c(a, b)[class.bin.v]
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}

LLlevy1.5 <- function(a, b, c, xmin){
  mu = c(a, b, c)[class3.v]
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}

LLlevy2 <- function(a, b, xmin){
  mu =  c(a, b)[species.v]
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}

LLlevy3 <- function(a, b, xmin){
  mu = c(a, b)[sex.v]
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}


LLlevy4 <- function(a, b, xmin){
  mu = exp(a + b*distedge.v) + 1
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}


LLlevy5 <- function(a, b, c, xmin){
  mu = c(a, b)[sex.v] + c(0, c)[class.bin.v]
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}

LLlevy5.5 <- function(a, b, c, d, xmin){
  mu = c(a, b)[sex.v] + c(0, c, d)[class3.v]
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}

LLlevy6 <- function(a, b, c, xmin){
  mu = c(a, b)[species.v] + c(0, c)[class.bin.v]
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}
LLlevy6.5 <- function(a, b, c, d, xmin){
  mu = c(a, b)[species.v] + c(0, c, d)[class3.v]
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}

LLlevy8 <- function(a, b, c, d, xmin){
  mu = exp(c(a, b)[class.bin.v] + c(c, d)[class.bin.v]*proxedge.v) + 1
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}

LLlevy8.5 <- function(a, b, c, d, e, f, xmin){
  mu = exp(c(a, b, c)[class3.v] + c(d, e, f)[class3.v]*proxedge.v) + 1
  -sum(dpowlaw(velo, alfa=mu, xmin=xmin, log=T))
}


### Fitting models to data
mlevy <- mle2(LLlevy, start=list(mu=2), fixed=list(xmin=min(velo)), 
              method = "L-BFGS-B", lower=c(mu = 1.1), upper=c(mu = 4))

mlevy1 <- mle2(LLlevy1, start=list(a = 2, b = 2), fixed=list(xmin=min(velo)),
               method = "L-BFGS-B", lower=c(a = 1.1, b = 1.1), upper=c(a = 4, b = 4))

mlevy1.5 <- mle2(LLlevy1.5, start=list(a = 2, b = 2, c = 2), fixed=list(xmin=min(velo)),
               method = "L-BFGS-B", lower=c(a = 1.1, b = 1.1, c = 1.1), upper=c(a = 4, b = 4, c = 4))

mlevy2 <- mle2(LLlevy2, start=list(a = 2, b = 2), fixed=list(xmin=min(velo)),
               method = "L-BFGS-B", lower=c(a = 1.1, b = 1.1), upper=c(a = 4, b = 4))

mlevy3 <- mle2(LLlevy3, start=list(a = 2, b = 2), fixed=list(xmin=min(velo)),
               method = "L-BFGS-B", lower=c(a = 1.1, b = 1.1), upper=c(a = 4, b = 4))

mlevy4 <- mle2(LLlevy4.1, start=list(a = 0, b = 0), fixed=list(xmin=min(velo)), trace = T,
                 method = "L-BFGS-B", lower=c(a = -Inf, b = -5), upper=c(a = Inf, b = 5))

mlevy5 <- mle2(LLlevy5, start=list(a = 2, b = 2, c = 0.1), fixed=list(xmin=min(velo)))
#confint(mlevy5)

mlevy5.5 <- mle2(LLlevy5.5, start=list(a = 2, b = 2, c = 0.1, d = 0.1), fixed=list(xmin=min(velo)))
#confint(mlevy5.5)

mlevy6 <- mle2(LLlevy6, start=list(a = 2, b = 2, c = 0.1), fixed=list(xmin=min(velo)))
#confint(mlevy6)

mlevy6.5 <- mle2(LLlevy6.5, start=list(a = 2, b = 2, c = 0.1, d = 0.1), fixed=list(xmin=min(velo)))
#confint(mlevy6.5)

mlevy8 <- mle2(LLlevy8, start=list(a = 0, b = 0, c = 0, d = 0), fixed=list(xmin=min(velo)),
               method = "L-BFGS-B", lower=c(a = -Inf, b = -Inf, c = -5, d = -5),
               upper=c(a = Inf, b = Inf, c = Inf, d = Inf))

mlevy8.5 <- mle2(LLlevy8.5, start=list(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0), 
                 fixed=list(xmin=min(velo)), method = "L-BFGS-B", 
                 lower=c(a = -Inf, b = -Inf, c = -Inf, d = -5, e = -5, f = -5),
                 upper=c(a = Inf, b = Inf, c = Inf, d = Inf, e = Inf, f = Inf))

### Comparing models
(comp <- AICctab(mlevy, mlevy1, mlevy2, mlevy3, mlevy4, mlevy5,mlevy5.5, mlevy6, mlevy6.5, 
                 mlevy1.5, mlevy8, mlevy8.5, 
                 nobs=length(velo), base=T, weights=T))

### Comparing all models
(comp <- AICctab(mexp, mexp1, mexp2, mexp3, mexp5, mexp5.5, mexp6, mexp6.5,                 
                 mexp1.5, mexp4, mexp8, mexp8.5,                
                 mwei, mwei1, mwei2, mwei3,mwei5,mwei5.5, mwei6, mwei6.5,                 
                 mwei1.5,mwei4, mwei8, mwei8.5,
                 mlevy, mlevy1, mlevy2, mlevy3,mlevy4, mlevy5, mlevy5.5, mlevy6, mlevy6.5,
                 mlevy1.5, mlevy8, mlevy8.5, base=T, weights=T, nobs=length(velo)))


####
# Combination of exponential models
dmix.exp <- function(x, rate1, rate2, a, log=FALSE){
  z <- a*dexp(x, rate=rate1) + (1-a)*dexp(x, rate=rate2)
  if(log) log(z)
  else z
}

LLmexp <- function(rate1, rate2, a) {
  -sum(dmix.exp(velo, rate1 = rate1, rate2 = rate2, a = a, log = T))
}

mmixexp <- mle2(LLmexp, start = list(rate1 = 0.1, rate2 = 0.1, a = 0.7))

# Combination of exponential models
dmix.wei <- function(x, forma1, forma2, escala1, escala2, a, log=FALSE){
  z <- a*dweibull(x, shape = forma1, scale = escala1) + 
    (1-a)*dweibull(x, shape = forma2, scale = escala2)
  if(log) log(z)
  else z
}

LLmwei <- function(forma1, forma2, escala1, escala2, a) {
  -sum(dmix.wei(velo, forma1=forma1, forma2=forma2, escala1=escala1, escala2=escala2,
                a = a, log = T))
}

mmixwei <- mle2(LLmwei, start = list(forma1 = 0.1, forma2 = 0.1, escala1 = esc-1, 
                                     escala2 = esc+1, a = 0.7))

# BEST MODEL = Model Exponential 9/8.5
#   velo ~ Exp(lambda)
#   lambda varia com class3 + proxedge (model 8.5)
#   lambda varia com sitio + proxedge (model 8.5)

# Likelihood intervals for each parameter
mexp.prof = profile(mexp8.5)
par(mfrow=c(3,2))
lik.interval <- plotprofmle2(mexp.prof)
par(mfrow=c(1,1))
lik.interval # all values in these intervals are equally plausible

# Coefficients of the model

coefs <- coef(mexp7); # maximum likelihood estimate MLE
# Coefficients of the exponential distribution, for, respectively:
# - site 1, forest; site 1, matrix
# - site 2, forest; site 2, matrix
# - site 3, forest; site 3, matrix
(coefs2 <- c(coefs[1], (coefs[1]+coefs[4]), 
            coefs[2], (coefs[2]+coefs[4]),
            coefs[3], (coefs[3]+coefs[4])))
# Expected value in each case, in the same order:
(means <- 1/coefs2)

### Ploting
plot(x=NULL, y=NULL,
     xlim = c(0.5, 3.5), ylim=c(min(velo), max(velo)),
     type="n", ann=FALSE, axes=F)
axis(1, at=c(1, 2, 3),  labels=c("1", "2", "3"))
axis(2)

cont <- 1
for (i in levels(site.v)) {
  for (j in levels(class.bin.v)){
    vioplot3(velo[which(site.v == i & class.bin.v == j)],
             at = as.numeric(i),
             side = ifelse(j == "MATA", "left", "right"),
             col = ifelse(j == "MATA", "green", "yellow"),
             lambda = means[cont],
             add = T)
    points(rep(as.numeric(i) + ifelse(j == "MATA", -0.015, +0.015), 
               length(velo[which(site.v == i & class.bin.v == j)])), 
           velo[which(site.v == i & class.bin.v == j)], pch = 45, cex=1)
    cont <- cont+1
  }
}
legend("topleft", legend = c("forest", "matrix"), fill = c("green", "yellow"), box.lwd = 0)
title("", xlab="Site", ylab="Average speed (m/min)")

### Ploting in another fashion
op <- par(mfrow=c(3,2))
titles <- c("Site 1\nForest", "Site 1\nMatrix", "Site 2\nForest", "Site 2\nMatrix",
            "Site 3\nForest", "Site 3\nMatrix")
cont <- 1
for (i in levels(site.v)) {
  for (j in levels(class.bin.v)){
    hist(velo[which(site.v == i & class.bin.v == j)], breaks = seq(0, 170, 2), 
         prob = T, main = titles[cont], 
         ylab = ifelse((cont == 1 | cont == 4), "Probability density", ""),
         xlab = ifelse((cont >= 4), "Av. speed (m/min)", "")) 
    # best curve     
    curve(dexp(x, coefs2[cont]), from = 0, to = 170, add = T, col="red", ylim=c(0, 0.5))
    # expected value for average speed
    abline(v = means[cont], col="darkgrey", lwd=2)
    cont <- cont+1
  }
}
par(op)

#######################
# For Y = Turning angles

# Wrapped Cauchy distribution

# Defining the function with log
dwcauchy <- function(theta, mu, rho, log=FALSE) {
  if(log) log(dwrpcauchy(theta, mu=mu, rho=rho))
  else dwrpcauchy(theta, mu=mu, rho=rho)
}
curve(dwcauchy(x, 0, 0.5), -pi, pi)
curve(dwcauchy(x, 0, 0.00001), -pi, pi, add=T)
curve(dwcauchy(x, pi, 0.2), -pi, pi, add=T)
integrate(dwcauchy, lower = -pi, upper = pi, mu = 0, rho = 0.5)

LLcauchy <- function(mu, rho) {
  -sum(dwcauchy(angle, mu, rho, log=T))
}

# Univariate models
LLcauchy1 <- function(a, b, rho) {
   mu =  c(a, b)[class.bin.a]
   -sum(dwcauchy(angle, mu, rho=rho, log=T))
}

LLcauchy1.5 <- function(a, b, c, rho) {
  mu =  c(a, b, c)[class3.a]
  -sum(dwcauchy(angle, mu, rho=rho, log=T))
}

LLcauchy2 <- function(a, b, rho) {
  mu = c(a, b)[sex.a]
  -sum(dwcauchy(angle, mu, rho=rho, log=T))
}

LLcauchy3 <- function(a, b, rho) {
  mu = c(a, b)[species.a]
  -sum(dwcauchy(angle, mu, rho=rho, log=T))
}


LLcauchy4<- function(a, b, rho) {
  mu = a + b*distedge.a
  -sum(dwcauchy(angle, mu, rho=rho, log=T))
}


# Multivariate models
LLcauchy5 <- function(a, b, c, rho) {
  mu = c(a, b)[sex.a] + c(0, c)[class.bin.a]
  -sum(dwcauchy(angle, mu, rho=rho, log=T))
}

LLcauchy5.5 <- function(a, b, c, d, rho) {
  mu = c(a, b)[sex.a] + c(0, c, d)[class3.a]
  -sum(dwcauchy(angle, mu, rho=rho, log=T))
}


LLcauchy6 <- function(a, b,  c, rho) {
  mu = c(a, b)[species.a] + c(0, c)[class.bin.a]
  -sum(dwcauchy(angle, mu, rho=rho, log=T))
}

LLcauchy6.5 <- function(a, b,  c, d, rho) {
  mu = c(a, b)[species.a] + c(0, c, d)[class3.a]
  -sum(dwcauchy(angle, mu, rho=rho, log=T))
}

LLcauchy8 <- function(a, b, c, d, rho) {
  mu =  c(a, b)[class.bin.a] + c(c, d)[class.bin.a]*proxedge.a
  -sum(dwcauchy(angle, mu, rho=rho, log=T))
}

LLcauchy8.5 <- function(a, b, c, d, e, f, rho) {
  mu =  c(a, b, c)[class3.a] + c(d, e, f)[class3.a]*proxedge.a
  -sum(dwcauchy(angle, mu, rho=rho, log=T))
}


### Fitting models to data
mcauchy <- mle2(LLcauchy, start=list(mu = pi, rho = 0.5), 
                method = "L-BFGS-B", lower=c(mu = -2*pi, rho = 0), 
                upper=c(mu = 2*pi, rho = 1))

mcauchy1 <- mle2(LLcauchy1, start=list(a = pi, b = pi, rho = 0.5), 
                 method = "L-BFGS-B", lower=c(a = -2*pi, b = -2*pi, rho = 0), 
                 upper=c(a = 2*pi, b = 2*pi, rho = 1))

mcauchy1.5 <- mle2(LLcauchy1.5, start=list(a = pi, b = pi, c = pi, rho = 0.5), 
                   method = "L-BFGS-B", lower=c(a = -2*pi, b = -2*pi, c = -2*pi, rho = 0), 
                   upper=c(a = 2*pi, b = 2*pi, c = 2*pi, rho = 1))

mcauchy2 <- mle2(LLcauchy2, start=list(a = pi, b = pi, rho = 0.5), 
                 method = "L-BFGS-B", lower=c(a = -2*pi, b = -2*pi, rho = 0), 
                 upper=c(a = 2*pi, b = 2*pi, rho = 1))

mcauchy3 <- mle2(LLcauchy3, start=list(a = pi, b = pi, rho = 0.5), 
                 method = "L-BFGS-B", lower=c(a = -2*pi, b = -2*pi, rho = 0.01), 
                 upper=c(a = 2*pi, b = 2*pi, rho = 0.99))

mcauchy4 <- mle2(LLcauchy4.1, start=list(a = pi, b = 0, rho = 0.5), 
                 method = "L-BFGS-B", lower=c(a = -2*pi, b = -0.1, rho = 0.01), 
                 upper=c(a = 2*pi, b = 0.1, rho = 0.99))

mcauchy5 <- mle2(LLcauchy5, start=list(a = pi, b = pi, c = 0, rho = 0.5), 
                 method = "L-BFGS-B", lower=c(a = -2*pi, b = -2*pi, c = -pi, rho = 0.01), 
                 upper=c(a = 2*pi, b = 2*pi, c = pi, rho = 0.99))

mcauchy5.5 <- mle2(LLcauchy5.5, start=list(a = pi, b = pi, c = pi, d = 0, rho = 0.5), 
                 method = "L-BFGS-B", lower=c(a = -2*pi, b = -2*pi, c = -2*pi, d = -pi, rho = 0.01), 
                 upper=c(a = 2*pi, b =2* pi, c = 2*pi, c = pi, rho = 0.99))

mcauchy6 <- mle2(LLcauchy6, start=list(a = pi, b = pi, c = 0, rho = 0.5), 
                 method = "L-BFGS-B", lower=c(a = -2*pi, b = -2*pi, c = -pi, rho = 0.01), 
                 upper=c(a = 2*pi, b = 2*pi, c = pi, rho = 0.99))

mcauchy6.5 <- mle2(LLcauchy6.5, start=list(a = pi, b = pi, c = pi, d = 0, rho = 0.5), 
                  method = "L-BFGS-B", lower=c(a = -2*pi, b = -2*pi, c = -2*pi, d = -pi, rho = 0.01), 
                 upper=c(a = 2*pi, b = 2*pi, c = 2*pi, d = pi, rho = 0.99))

mcauchy8 <- mle2(LLcauchy8, start=list(a = pi, b = pi, c = 0, d = 0, rho = 0.5), 
                   method = "L-BFGS-B", 
                   lower=c(a = -2*pi, b = -2*pi, c = -0.1, d = -0.1, rho = 0.01), 
                   upper=c(a = 2*pi, b = 2*pi, c = 0.1, d = 0.1, rho = 0.99))

mcauchy8.5 <- mle2(LLcauchy8.5, start=list(a = pi, b = pi, c = pi, d = 0, e = 0, f = 0, rho = 0.5), 
                 method = "L-BFGS-B", 
                 lower=c(a = -2*pi, b = -2*pi, c = -2*pi, d = -0.1, e = -0.1, f = -0.1, rho = 0.01), 
                 upper=c(a = 2*pi, b = 2*pi, c = 2*pi, d = 0.1, e = 0.1, f = 0.1, rho = 0.99))

mcauchy9 <- mle2(LLcauchy9, start=list(a = pi, b = pi, c = pi, d = 0, e = 0, f = 0, rho = 0.5), 
                   method = "L-BFGS-B", 
                   lower=c(a = -2*pi, b = -2*pi, c = -2*pi, d = -0.1, e = -0.1, f = -0.1, rho = 0.01), 
                   upper=c(a = 2*pi, b = 2*pi, c = 2*pi, d = 0.1, e = 0.1, f = 0.1, rho = 0.99))

(comp <- AICctab(mcauchy, mcauchy1, mcauchy2, mcauchy3, mcauchy5, mcauchy5.5, mcauchy6, mcauchy6.5,
                 mcauchy1.5, mcauchy4, mcauchy8, mcauchy8.5, 
                 base=T, weights=T, nobs=length(velo)))

# BEST MODEL = no effect model!
# Plotting:
hist(angle, breaks = seq(-pi, pi, length=36), probability = T, 
     xlab="Turning angles (rad)", ylab="Probability density", main="")
curve(dwcauchy(x, coef(mcauchy)[1], coef(mcauchy)[2]), -pi, pi, col=2, add=T)

#circular plot
angle.c = circular(angle, type="angle", units="radians")

rose.diag(angle.c, bins=36, ticks=F, shrink=0.6,xlim=c(-2,2),ylim=c(-1,1),
          axes=T, prop=2.8, zero=pi/2)

# we could also do:
m0 <- mle.wrappedcauchy(circular(angle)) 

# Plotting likelihodd intervals
confint(mcauchy)
mcauchy.prof = profile(mcauchy)
par(mfrow=c(1,2))
plotprofmle2(mcauchy.prof)
par(mfrow=c(1,1))

#########

# Fazendo figuras para o material suplementar

# Distribuição Exponencial

pdf("distributions.pdf")

curve(dexp(x, rate = 0.1), 0, 100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density")
curve(dexp(x, rate = 0.05), 0, 100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 2, add=T)
legend(60, 0.1, legend = c(expression(paste(lambda, " = 0.1")), expression(paste(lambda, " = 0.05"))), 
       col = c(1,2), lwd = 2, bty = "n")

# Distribuicao Weibull

# k = shape
# lambda = scale
#pdf("weibull.pdf")
#par(mfrow=c(1,2))
curve(dweibull(x, shape = 1.5, scale = 20), 0, 100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density")
curve(dweibull(x, shape = 1.5, scale = 30), 0, 100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 2, add = T)
curve(dweibull(x, shape = 1.5, scale = 50), 0, 100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 4, add = T)
legend(60, 0.036, legend = c(expression(paste(lambda, " = 20")), expression(paste(lambda, " = 30")), expression(paste(lambda, " = 50"))), 
       col = c(1,2,4), lwd = 2, bty = "n")
text(75, 0.037, "k = 1.5")

curve(dweibull(x, shape = 0.5, scale = 30), 0, 100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density")
curve(dweibull(x, shape = 1, scale = 30), 0, 100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 2, add = T)
curve(dweibull(x, shape = 2, scale = 30), 0, 100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 4, add = T)
curve(dweibull(x, shape = 5, scale = 30), 0, 100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 3, add = T)
legend(60, 0.073, legend = c("k = 0.5", "k = 1", "k = 2", "k = 5"), col = c(1,2,4,3), lwd = 2, bty = "n")
text(75, 0.075, expression(paste(lambda, " = 30")))
par(mfrow=c(1,1))
#dev.off()

# Distribuicao Levy

dpowlaw <- function(x, alfa, xmin, log=FALSE){
  c <- (alfa-1)*xmin^(alfa-1)
  if(log) ifelse(x < xmin, 0, log(c*x^(-alfa)))
  else ifelse(x < xmin, 0, c*x^(-alfa))
}

curve(dpowlaw(x, alfa=3, xmin=1), from=1, to=100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density")
curve(dpowlaw(x, alfa=2, xmin=1), from=1, to=100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 2, add = T)
curve(dpowlaw(x, alfa=1.1, xmin=1), from=1, to=100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 4, add = T)
legend("topright", legend = c(expression(paste(mu, " = 3")), expression(paste(mu, " = 2")), expression(paste(mu, " = 1.1  "))), col = c(1,2,4), lwd = 2, bty = "n")

curve(dpowlaw(x, alfa=3, xmin=1), from=1, to=100, log="xy", lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density")
curve(dpowlaw(x, alfa=2, xmin=1), from=1, to=100, log="xy", lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 2, add = T)
curve(dpowlaw(x, alfa=1.1, xmin=1), from=1, to=100, log="xy", lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 4, add = T)
legend("topright", legend = c(expression(paste(mu, " = 3")), expression(paste(mu, " = 2")), expression(paste(mu, " = 1.1  "))), col = c(1,2,4), lwd = 2, bty = "n")

# Distribuicao Levy truncada

dtpowlaw <- function(x, alfa, xmin, xmax, log=FALSE){
  c <- (alfa-1)*xmin^(alfa-1)/(1 - (xmin/xmax)^(alfa-1))
  if(log) ifelse(x < xmin | x > xmax, 0, log(c*x^(-alfa)))
  else ifelse(x < xmin | x > xmax, 0, c*x^(-alfa))
}

curve(dtpowlaw(x, alfa=3, xmin=1, xmax = 60), from=1, to=100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density")
curve(dtpowlaw(x, alfa=2, xmin=1, xmax = 60), from=1, to=100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 2, add = T)
curve(dtpowlaw(x, alfa=1.1, xmin=1, xmax = 60), from=1, to=100, lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 4, add = T)
legend("topright", legend = c(expression(paste(mu, " = 3")), expression(paste(mu, " = 2")), expression(paste(mu, " = 1.1  "))), col = c(1,2,4), lwd = 2, bty = "n")

curve(dtpowlaw(x, alfa=3, xmin=1, xmax = 60), from=1, to=100, log="xy", lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density")
curve(dtpowlaw(x, alfa=2, xmin=1, xmax = 60), from=1, to=100, log="xy", lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 2, add = T)
curve(dtpowlaw(x, alfa=1.1, xmin=1, xmax = 60), from=1, to=100, log="xy", lwd = 2, xlab = "Speed (m/min)", ylab = "Probability density", col = 4, add = T)
legend("topright", legend = c(expression(paste(mu, " = 3")), expression(paste(mu, " = 2")), expression(paste(mu, " = 1.1  "))), col = c(1,2,4), lwd = 2, bty = "n")

library(CircStats)
curve(dwrpcauchy(x, 0, 0.5), -pi, pi, lwd = 2, xlab = "Turning angle (rad)", ylab = "Probability density")
curve(dwrpcauchy(x, -pi/2, 0.5), -pi, pi, lwd = 2, col = 2, add = T)
curve(dwrpcauchy(x, 0, 0.2), -pi, pi, lwd = 2, lty = 2, col = 1, add = T)
curve(dwrpcauchy(x, -pi/2, 0.2), -pi, pi, lwd = 2, lty = 2, col = 2, add = T)
legend("topright", legend = c(expression(paste(mu, " = 0, ", rho, " = 0.5 ")), expression(paste(mu, " = 0, ", rho, " = 0.2  ")), expression(paste(mu, " = -", pi, "/2 , ", rho, " = 0.5 ")), expression(paste(mu, " = -", pi, "/2 , ", rho, " = 0.2 "))), 
       col = c(1,1,2,2), lty=c(1,2,1,2), lwd = 2, bty = "n", cex = 1)

dev.off()