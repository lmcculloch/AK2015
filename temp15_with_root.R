setwd('/Users/lmccullo/Google Drive/LM_root_paper')
temp <- read.csv("agg_temp_15cm.csv")
ak<- read.csv("ViPER_Root_Agg.csv")

dat <- merge(temp,ak, by.x= "Site", incomparables = NA)

#this code uses soil temperature aggregated to 15 cm#

## Temp data normality ##
hist(temp$temp)
hist(temp$tdd)
hist(temp$thaw)
hist(temp$temp.na)

##################################
##Temperature v. root parameters##
##################################
##################################
##Temperature v. root parameters##
##################################
##################################
##Temperature v. root parameters##
##################################

plot(dat$temp,dat$FineLive, col=dat$Boreal.Tundra, na.omit = "TRUE" ) 
plot(dat$temp,dat$FineDead, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,(dat$FineDead/dat$FineLive), col=dat$Boreal.Tundra, na.omit = "TRUE" )
#Somewhat interesting relationship here ^#
plot(dat$temp,dat$CoarseLive, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,dat$CoarseDead, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,(dat$FineLive+dat$FineDead), col=dat$Boreal.Tundra, na.omit = "TRUE")

plot(dat$temp,(dat$FineDead/dat$vol), col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,(dat$FineLive/dat$vol), col=dat$Boreal.Tundra, na.omit = "TRUE" )

plot(dat$temp,dat$frl.car, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,dat$frd.car, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,dat$crl.car, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,dat$crd.car, col=dat$Boreal.Tundra, na.omit = "TRUE" )

plot(dat$temp,dat$deadc.n, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,dat$livec.n, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,dat$soilc.n, col=dat$Boreal.Tundra, na.omit = "TRUE" )

plot(dat$temp,dat$live.n.per, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,dat$live.c.per, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,dat$live.p, col=dat$Boreal.Tundra, na.omit = "TRUE" )

plot(dat$temp,dat$dead.n.per, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,dat$dead.c.per, col=dat$Boreal.Tundra, na.omit = "TRUE" )
plot(dat$temp,dat$dead.p, col=dat$Boreal.Tundra, na.omit = "TRUE" )

##################################
##TDD v. root parameters##
##################################
##################################
##TDD v. root parameters##
##################################
##################################
##TDD v. root parameters##
##################################

plot(dat$tdd,dat$FineLive, col=dat$Boreal.Tundra) 
plot(dat$tdd,dat$FineDead, col=dat$Boreal.Tundra)
plot(dat$tdd,(dat$FineDead/dat$FineLive), col=dat$Boreal.Tundra)
plot(dat$tdd,dat$CoarseLive, col=dat$Boreal.Tundra)
plot(dat$tdd,dat$CoarseDead, col=dat$Boreal.Tundra)
plot(dat$tdd,((dat$FineLive)+(dat$FineDead)), col=dat$Boreal.Tundra)

plot(dat$tdd,(dat$FineDead/dat$vol), col=dat$Boreal.Tundra)
plot(dat$tdd,(dat$FineLive/dat$vol), col=dat$Boreal.Tundra)

plot(dat$tdd,dat$frl.car, col=dat$Boreal.Tundra)
plot(dat$tdd,dat$frd.car, col=dat$Boreal.Tundra)
plot(dat$tdd,dat$crl.car, col=dat$Boreal.Tundra)
plot(dat$tdd,dat$crd.car, col=dat$Boreal.Tundra)

plot(dat$tdd,dat$deadc.n, col=dat$Boreal.Tundra)
plot(dat$tdd,dat$livec.n, col=dat$Boreal.Tundra)
plot(dat$tdd,dat$soilc.n, col=dat$Boreal.Tundra)

plot(dat$tdd,dat$live.n.per, col=dat$Boreal.Tundra)
plot(dat$tdd,dat$live.c.per, col=dat$Boreal.Tundra)
plot(dat$tdd,dat$live.p, col=dat$Boreal.Tundra)

plot(dat$tdd,dat$dead.n.per, col=dat$Boreal.Tundra)
plot(dat$tdd,dat$dead.c.per, col=dat$Boreal.Tundra)
plot(dat$tdd,dat$dead.p, col=dat$Boreal.Tundra)

##################################
##THAW v. root parameters##
##################################
##################################
##THAW v. root parameters##
##################################
##################################
##THAW v. root parameters##
##################################

##seems to be an outlier here with a 0 thaw value, site = 9##

plot(dat$thaw,dat$FineLive, col=dat$Boreal.Tundra) 
plot(dat$thaw,dat$FineDead, col=dat$Boreal.Tundra)
plot(dat$thaw,(dat$FineDead/dat$FineLive), col=dat$Boreal.Tundra)
plot(dat$thaw,dat$CoarseLive, col=dat$Boreal.Tundra)
plot(dat$thaw,dat$CoarseDead, col=dat$Boreal.Tundra)
plot(dat$thaw,(dat$FineLive+dat$FineDead), col=dat$Boreal.Tundra)


plot(dat$thaw,(dat$FineDead/dat$vol), col=dat$Boreal.Tundra)
plot(dat$thaw,(dat$FineLive/dat$vol), col=dat$Boreal.Tundra)

plot(dat$thaw,dat$frl.car, col=dat$Boreal.Tundra)
plot(dat$thaw,dat$frd.car, col=dat$Boreal.Tundra)
plot(dat$thaw,dat$crl.car, col=dat$Boreal.Tundra)
plot(dat$thaw,dat$crd.car, col=dat$Boreal.Tundra)

plot(dat$thaw,dat$deadc.n, col=dat$Boreal.Tundra)
plot(dat$thaw,dat$livec.n, col=dat$Boreal.Tundra)
plot(dat$thaw,dat$soilc.n, col=dat$Boreal.Tundra)

plot(dat$thaw,dat$live.n.per, col=dat$Boreal.Tundra)
plot(dat$thaw,dat$live.c.per, col=dat$Boreal.Tundra)
plot(dat$thaw,dat$live.p, col=dat$Boreal.Tundra)

plot(dat$thaw,dat$dead.n.per, col=dat$Boreal.Tundra)
plot(dat$thaw,dat$dead.c.per, col=dat$Boreal.Tundra)
plot(dat$thaw,dat$dead.p, col=dat$Boreal.Tundra)

