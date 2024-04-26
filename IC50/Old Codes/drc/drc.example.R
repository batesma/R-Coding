library(drc)
#~ Data from 'drc' data set
lettuce
#~ Simple model without hormesis (see drc.pdf)
m0<-drm(weight~conc, data = lettuce, fct = LL.3())
summary(m0)
modelFit(m0)
plot(m0)
#~ ED50
ED(m0,50)

#~ Looking for another good models (see drc.pdf)
mselect(m0, fctList=list(CRS.6(),CRS.5a(),CRS.5b(),CRS.5c(),CRS.4a(),CRS.4b(),CRS.4c(),CRS.6(fixed=c(NA,0,NA,NA,NA,NA)),BC.5(),BC.4(),LL.5(),LL.4(),LL.2()),icfct=AIC)
mselect(m0, fctList=list(CRS.6(),CRS.5a(),CRS.5b(),CRS.5c(),CRS.4a(),CRS.4b(),CRS.4c(),CRS.6(fixed=c(NA,0,NA,NA,NA,NA)),BC.5(),BC.4(),LL.5(),LL.4(),LL.2()),icfct=BIC)

#~ BC.4 looks good, although there are not enough points (not all coefficients significantly differ from 0)
m1 <-  drm(weight~conc, data = lettuce, fct = BC.4())
summary(m1)
modelFit(m1)
anova(m0,m1)
plot(m1,add=TRUE,col="red")
#~ ED50
ED(m1,50)
#~ Maximum response and concentration
MAX(m1)