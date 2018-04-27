library(reshape2)
library(dplyr)


#Statistics For Figure #7
data <- read.csv ("GTEx_skin_for_estee.csv", head=TRUE)
gene <- c("SUBJID", "SEX", "AGE", "RACE", "Tissue","TAS2R1",  "TAS2R10",  "TAS2R13",  "TAS2R14",  "TAS2R16",  "TAS2R19",  "TAS2R20",  "TAS2R3",  "TAS2R30",  "TAS2R31",  "TAS2R38",  "TAS2R39",  "TAS2R4",  "TAS2R40",  "TAS2R41",  "TAS2R42",  "TAS2R43",  "TAS2R46",  "TAS2R5",  "TAS2R50",  "TAS2R60",  "TAS2R7",  "TAS2R8",  "TAS2R9")

data <- data[gene]
dat1 <- subset(data, Tissue=="Skin - Not Sun Exposed (Suprapubic)")
dat2 <-  subset(data, Tissue=="Skin - Sun Exposed (Lower leg)")


##Sex effect on Skin - Not Sun Exposed (Suprapubic)

names<- names(dat1)
Sex_test_dat1<-file("Sex_test_notsun_result.csv", "w")
cat("Gene", "statistic", "parameter", "method", "p.value", "data.name", file=Sex_test_dat1, "\n" ,sep="," )
for(i in 6:29){
  dfsex <- kruskal.test(dat1[,i] ~ SEX, data = dat1) 
  cat(names[i], dfsex$statistic, dfsex$parameter, dfsex$method, dfsex$p.value, dfsex$data.name, file=Sex_test_dat1, "\n", sep="," )
}
closeAllConnections()


meltd<-melt(dat1, id=c("SUBJID","SEX","AGE","RACE", "Tissue")) 
table <- ddply(meltd, .(SEX, variable), summarise, mean(value),median(value),sd(value))
write.table(table, file="Table_notSun_exposed_for_Figure7.csv", sep=",", row.names=F)




##Sex effect on Skin - Sun Exposed (Lower leg)

names<- names(dat2)
Sex_test_dat2<-file("Sex_test_sun_result.csv", "w")
cat("Gene", "statistic", "parameter", "method", "p.value", "data.name", file=Sex_test_dat2, "\n" ,sep="," )
for(i in 6:29){
  dfsex <- kruskal.test(dat2[,i] ~ SEX, data = dat2) 
  cat(names[i], dfsex$statistic, dfsex$parameter, dfsex$method, dfsex$p.value, dfsex$data.name, file=Sex_test_dat2, "\n", sep="," )
}
closeAllConnections()

meltd<-melt(dat2, id=c("SUBJID","SEX","AGE","RACE", "Tissue")) 
table <- ddply(meltd, .(SEX, variable), summarise, mean(value),median(value),sd(value))
write.table(table, file="Table_Sun_exposed_for_Figure7.csv", sep=",", row.names=F)




#statis for Figure#6
##check factor sun_exposed/notsun_exposed, only paired samples N=299

dat1a <- dat1[dat1$SUBJID %in% dat2$SUBJID,]
dat2a <- dat2[dat2$SUBJID %in% dat1$SUBJID,]
datac <- rbind(dat1a, dat2a)

names<- names(datac)
sun_test_datac<-file("SUN_vs_notSun_test_result_pairedsamples.csv", "w")
cat("Gene", "statistic", "parameter", "method", "p.value", "data.name", file=sun_test_datac, "\n" ,sep="," )
for(i in 6:29){
  df <- kruskal.test(datac[,i] ~ Tissue, data = datac) 
  cat(names[i], df$statistic, df$parameter, df$method, df$p.value, df$data.name, file=sun_test_datac, "\n", sep="," )
}

closeAllConnections()


meltd<-melt(datac, id=c("SUBJID","SEX","AGE","RACE", "Tissue")) 
table <- ddply(meltd, .(Tissue, variable), summarise, mean(value),median(value),sd(value))
write.table(table, file="Table_for_Figure6.csv", sep=",", row.names=F)

