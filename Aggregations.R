setwd('/Users/lmccullo/Google Drive/Alaska2015')
agg<- read.csv("toaggcnp.csv")
temp<-read.csv("borehole_temp_data_out.csv")
agglnper <- aggregate(list(agg$live.n.per,agg$dead.n.per,agg$live.c.per,
                           agg$dead.c.per, agg$live.p, agg$dead.p), 
                      by=list(agg$Site),names=c(),
                     FUN=mean )
names(agglnper)<-c("site","live.n.per","dead.n.per","live.c.per","dead.c.per",
                   "live.p","dead.p")

write.csv(agglnper,"toaggcnp_complete.csv")

# step 1: drop everything below cutoff
cutoff <- 0.10
temp.cutoff <- temp[temp$depth<cutoff,]

temp.aggregate10 <- aggregate(list(temp.cutoff$temp, temp.cutoff$tdd,
                                   temp.cutoff$thaw, temp.cutoff$temp.na),
                              by=list(temp.cutoff$site),
                            FUN="mean", na.rm=T)
names(temp.aggregate10)<-c("site","temp","tdd","thaw","temp.na")

write.csv(temp.aggregate10,"agg_temp_10cm.csv")

cutoff15 <- 0.15
temp.cutoff15 <- temp[temp$depth<cutoff15,]

temp.aggregate15 <-aggregate(list(temp.cutoff15$temp, temp.cutoff15$tdd,
                             temp.cutoff15$thaw, temp.cutoff15$temp.na),
                             by=list(temp.cutoff15$site),
                             FUN="mean", na.rm=T)
names(temp.aggregate15)<-c("site","temp","tdd","thaw","temp.na")

write.csv(temp.aggregate15,"agg_temp_15cm.csv")

