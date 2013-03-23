source("geom.R")
source("SlopeStability.R")
source("material.R")
source("topLayer.R")
source("cirIntercept.R")
source("F.R")
source("oms.R")

library(moments)
library(parallel)

geo = geom(S1 = list(x = c(0,150,150,110,50,0), y = c(0,0,50,50,20,20)))
materials = material(cohesion_eff = c(10,11,9,10,10,11,12,7,10,9)*1000, FrictionAngle_eff = c(20,19,22,17,20,21,20,21,19), UnitWeight = c(20,20.5,19,19.5,18.5,21,21,20,20.5)*1000)



temp = SlopeStability(geom = geo, materials = materials, xrange = c(69,71), yrange = c(69,71), Rrange = c(59,61), N=10)

if(FALSE)
{

tl = topLayer(geo)
method = "oms"
startX = 70
startY = 70
startR = 60
n= 10
#Optimize over F

fae = (materials["FrictionAngle_eff","MEAN"]*pi)/180
ce = materials["cohesion_eff","MEAN"]
g = materials["UnitWeight","MEAN"]
pp = 0
temp = optim(par = c(startX, startY, startR), TL = tl, fn = F, FAEr = fae, cE = ce, u = pp, gam = g, met = method, n = n, method = "L-BFGS-B", upper = c(75,75,65), lower = c(65,65,55))
}
