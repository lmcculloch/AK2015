#Code for statistical analysis and figures of fine root biomass 
#between and within Alaskan tundra and boreal forest sites
#Data were collected in 2015 as a part of the ViPER project

#libraries ####
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lme4)

#WD and datasheets####
setwd('/Users/lmccullo/Google Drive/Alaska2015/AK_data')
ak<- read.csv("ViPER_roots_2015_LM.csv")
akk<- read.csv("ViPER_roots_2015_LM.1.csv")
tt<- read.csv("agg_temp_10cm.csv")
colnames(tt)[1] <- "Site"
#join tt to ak
ak2 <- join(ak,tt, by="Site",type="left")
ak2$temp[ak2$Site==16]= NA

#Adding other variables #####
ak2$live.dens <- (ak2$FineLive/(10*ak2$w*ak2$l))*10
ak2$dead.dens <- (ak2$FineDead/(10*ak2$w*ak2$l))*10
ak2$fine.dead.live <- ak2$dead.dens/ak2$live.dens
droplevels(ak2$Boreal.Tundra)
ak2$Boreal.Tundra <- factor(ak2$Boreal.Tundra, exclude = "Tundra", labels = c("Boreal", "Tundra"))
ak$fine.dead.live <- ak$FineDead/ak$FineLive
akk$dead.dens = (akk$FineDead/(10*akk$W*akk$L))*10
akk$live.dens = (akk$FineLive/(10*akk$W*akk$L))*10
akk$Fine.Dead.Live = akk$dead.dens/akk$live.dens

#Linear models with OLD (site average)####

m1 = lm(fine.dead.live ~ OLDsite, data=ak2)
summary(m1)
plot(fine.dead.live ~ OLDsite, data=ak2)

m2 = lm(live.dens ~ OLDsite, data=ak2)
summary(m2)
plot(live.dens ~ OLDsite, data=ak2)

m3 = lm(dead.dens ~ OLDsite, data=ak2)
summary(m3)
plot(dead.dens ~ OLDsite, data=ak2)

#Linear models with TD (site average)####

m1 = lm(fine.dead.live ~ TDsite, data=ak2)
summary(m1)
plot(fine.dead.live ~ TDsite, data=ak2)

m2 = lm(live.dens ~ TDsite, data=ak2)
summary(m2)
plot(live.dens ~ OLDsite, data=ak2)

m3 = lm(dead.dens ~ TDsite, data=ak2)
summary(m3)
plot(dead.dens ~ TDsite, data=ak2)

#########################################
#Coefficient of Variation analyses ######
#########################################

cv = (sd(ak2$fine.dead.live)/mean(ak2$fine.dead.live))*100
cv.b = (sd(ak2[ak2$Boreal.Tundra == "Boreal",]$fine.dead.live)/mean(ak2[ak2$Boreal.Tundra == "Boreal",]$fine.dead.live))*100
cv.t= (sd(ak2[ak2$Boreal.Tundra == "Tundra",]$fine.dead.live)/mean(ak2[ak2$Boreal.Tundra == "Tundra",]$fine.dead.live))*100

cv.l = (sd(akk$live.dens)/mean(akk$live.dens))*100
cv.b.l = (sd(ak2[ak2$Boreal.Tundra == "Boreal",]$live.dens)/mean(ak2[ak2$Boreal.Tundra == "Boreal",]$live.dens))*100
cv.t.l= (sd(ak2[ak2$Boreal.Tundra == "Tundra",]$live.dens)/mean(ak2[ak2$Boreal.Tundra == "Tundra",]$live.dens))*100

cv = (sd(akk$dead.dens)/mean(akk$dead.dens))*100
cv.b = (sd(ak2[ak2$Boreal.Tundra == "Boreal",]$dead.dens)/mean(ak2[ak2$Boreal.Tundra == "Boreal",]$dead.dens))*100
cv.t= (sd(ak2[ak2$Boreal.Tundra == "Tundra",]$dead.dens)/mean(ak2[ak2$Boreal.Tundra == "Tundra",]$dead.dens))*100

#########################################
#Nested T-test analyses #################
#########################################

akk = akk %>%
  filter(!is.na(dead.dens))
# random intercept
data.nest.lme <- lme(dead.dens ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme)


#live density
# random intercept
data.nest.lme <- lme(live.dens ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme)

#dead to live ratio
# random intercept
data.nest.lme <- lme(Fine.Dead.Live ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme)

#live C:N
# random intercept
data.nest.lme <- lme(livec.n ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme)

#dead C:N
# random intercept
data.nest.lme <- lme(deadc.n ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme)

#dead n %

# random intercept
data.nest.lme <- lme(dead.n.per ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme)

#live n %

# random intercept
data.nest.lme <- lme(live.n.per ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme)

#dead c %

# random intercept
data.nest.lme <- lme(dead.c.per ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme1)

#live c %

# random intercept
data.nest.lme <- lme(live.c.per ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme)

#dead p 

# random intercept
data.nest.lme <- lme(dead.p ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme)

#live p

# random intercept
data.nest.lme <- lme(live.p ~ Boreal.Tundra, random = ~1 | Site, akk, method = "REML")
summary(data.nest.lme)

#########################################
#Multivariate linear models analyses ####
#########################################

#Dead:live FRB

m1 <- lm(ak2$fine.dead.live~ak2$soilc.n+ak2$m.cov+ak2$temp+ak2$org.moisture+ak2$Boreal.Tundra+ak2$t-old)
summary(m1)

#Live FRB

m1 <- lm(ak2$live.dens~ak2$soilc.n+ak2$m.cov+ak2$temp+ak2$org.moisture+ak2$Boreal.Tundra+ak2$t-old)
summary(m1)

#Dead FRB

m1 <- lm(ak2$dead.dens~ak2$soilc.n+ak2$m.cov+ak2$temp+ak2$org.moisture+ak2$Boreal.Tundra+ak2$t-old)
summary(m1)

##############################################
##Above to belowground biomass comparisons####
##############################################

ak.t = ak2 %>%
  filter(Boreal.Tundra == "Tundra")


ak.t$blw.abv.ld <- ((ak.t$live.dens)+(ak.t$dead.dens)+
                      ((ak.t$CoarseLive/(ak.t$l*ak.t$w*10)*10))+
                      ((ak.t$CoarseDead/(ak.t$l*ak.t$w*10)*10)))/(ak.t$US.bio/1000)

ak.t$blw.abv.l = ((ak.t$live.dens)+
                    ((ak.t$CoarseLive/(ak.t$l*ak.t$w*10)*10)))/(ak.t$US.bio/1000)


##############################################
################# FIGURES ####################
##############################################

#Figure 2 ####
#Live and dead FRB - Boreal versus tundra
a <- ggplot(ak2, aes(x=Boreal.Tundra, y=live.dens,fill = Boreal.Tundra))+
  geom_boxplot()+
  scale_y_continuous(expand= c(0,0),limits = c(0,0.75))+
  scale_x_discrete(labels = c('Boreal', "Tundra"))+
  scale_fill_manual(values=c("gray75","gray40"),
                    labels=c("Boreal", "Tundra"))+
  xlab ("")+
  ylab (bquote("Live FRB (kg "*"m"
               ^-2* " of soil)"))+
  theme_pubr()+ 
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.position= "none")
  #geom_dotplot(binaxis='y', method = "histodot", stackdir='center', dotsize=3, binwidth = .005, fill = "black")

b <- ggplot(ak2, aes(x=Boreal.Tundra, y=dead.dens, fill = Boreal.Tundra))+
  geom_boxplot()+
  scale_y_continuous(expand= c(0,0),limits = c(0,0.75))+
  scale_x_discrete(labels = c('Boreal', "Tundra"))+
  scale_fill_manual(values=c("gray75","gray40"),
                    labels=c("Boreal", "Tundra"))+
  xlab ("") +
  ylab (bquote("Dead FRB (kg "*"m"
               ^-2* " of soil)"))+
  theme_pubr()+ 
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.position= "none")
  #geom_dotplot(binaxis='y', method = "histodot", stackdir='center', dotsize=3, binwidth = .005, fill = "black")



ggarrange(a, b, 
          labels = c("a", "b"),
          ncol = 2, nrow = 1, 
          heights = c(1,1),
          align = "v")

#Figure 3####

#Coefficient of Variation ###

a = lm(ak2$dead.n.per~ak2$live.n.per)
summary(a)
plot(ak2$dead.n.per,ak2$live.n.per)

CV.live<- akk %>%
  group_by(Boreal.Tundra, Site) %>%
  summarise(
    n=n(),
    mean=mean(live.dens),
    sd=sd(live.dens),
    cv.l = (sd(live.dens)/mean(live.dens))*100
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  mutate( mean2 = mean(cv.l)) %>%
  mutate( sd2 = sd(cv.l))

CV.dead<- akk %>%
  group_by(Boreal.Tundra, Site) %>%
  summarise( 
    n=n(),
    mean=mean(dead.dens),
    sd=sd(dead.dens),
    cv.l = (sd(dead.dens)/mean(dead.dens))*100
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  mutate( mean2 = mean(cv.l)) %>%
  mutate( sd2 = sd(cv.l))

l = lm(ak2$livec.n~ak2$s.cov)

dead = ggplot(CV.dead, aes(x=Boreal.Tundra, y=cv.l, fill = Boreal.Tundra))+
  geom_boxplot()+
  xlab ("") +
  ylab (bquote("Dead FRB coefficient of varaition (%) "))+
  scale_y_continuous(expand= c(0,0),limits = c(0,150))+
  scale_x_discrete(labels = c('Boreal', "Tundra"))+
  scale_fill_manual(values=c("gray75","gray40"),
                    labels=c("Boreal", "Tundra"))+
  theme_pubr()+ 
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.position= "none")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=3, binwidth = 1, fill = "black")

live = ggplot(CV.live, aes(x=Boreal.Tundra, y=cv.l, fill = Boreal.Tundra))+
  geom_boxplot()+
  xlab ("") +
  ylab (bquote("Live FRB coefficient of varaition (%) "))+
  scale_x_discrete(labels = c('Boreal', "Tundra"))+
  scale_y_continuous(expand= c(0,0),limits = c(0,150))+
  scale_fill_manual(values=c("gray75","gray40"),
                    labels=c("Boreal", "Tundra"))+
  theme_pubr()+ 
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.position= "none")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=3, binwidth = 1, fill = "black")


ggarrange(dead, live, 
          labels = c("a", "b"),
          ncol = 2, nrow = 1, 
          heights = c(1,1),
          align = "v",
          common.legend = TRUE, 
          legend = "none")

#Figure 4####
#Multivariate figure 

a <- ggplot (data=ak2, aes(x=soilc.n, y=fine.dead.live)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab ("Soil C:N")+
  ylab ("Dead:Live FRB")+
  theme_pubr() +
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")

b <- ggplot (data=ak2, aes(x=org.moisture, y=fine.dead.live)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab (bquote("Organic soil moisture ("*"cm"
               ^3* " water" *" cm"^-3* " soil)"))+
  ylab ("")+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")

c <- ggplot (data=ak2, aes(x=temp, y=fine.dead.live)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab (expression("Temperature ("*degree*"C)"))+
  ylab ("")+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")

d <- ggplot (data=ak2, aes(x=m.cov, y=fine.dead.live)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab ("Moss Cover (%)")+
  ylab ("")+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")


al <- ggplot (data=ak2, aes(x=soilc.n, y=live.dens)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab ("")+
  ylab (bquote("Live FRB (kg "*"m"
               ^-2* "of soil)"))+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")

bl <- ggplot (data=ak2, aes(x=org.moisture, y=live.dens)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab ("")+
  ylab ("")+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")

cl <- ggplot (data=ak2, aes(x=temp, y=live.dens)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab ("")+
  ylab ("")+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")

dl <- ggplot (data=ak2, aes(x=m.cov, y=live.dens)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab ("")+
  ylab ("")+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")


ad <- ggplot (data=ak2, aes(x=soilc.n, y=dead.dens)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab ("")+
  ylab (bquote("Dead FRB (kg "*"m"
               ^-2* "of soil)"))+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")

bd <- ggplot (data=ak2, aes(x=org.moisture, y=dead.dens)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab ("")+
  ylab ("")+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")

cd <- ggplot (data=ak2, aes(x=temp, y=dead.dens)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab ("")+
  ylab ("")+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")

dd <- ggplot (data=ak2, aes(x=m.cov, y=dead.dens)) +
  geom_point(aes(shape = Boreal.Tundra), size = 4)+
  scale_shape_manual(values = c("Boreal"= "triangle", "Tundra" = "circle"))+
  geom_smooth(method="lm", se = FALSE, color = "black")+
  xlab ("")+
  ylab ("")+
  theme_pubr()+
  theme(axis.title=element_text(size=25),
        axis.text = element_text(size=25),
        legend.position= "none")


ggarrange(ad, bd, cd,dd,al, bl, cl, dl,a,b, c, d,  
          labels = c("a", "b", "c", 'd', "e", "f", "g", "h", "i", "j", "k", "l"),
          ncol = 4, nrow = 3, 
          font.label = list(size = 20),
          align = "v",
          heights = c(1,1))

ggsave("AK_figure3_.pdf", width=20, height=16, units = ("in"))