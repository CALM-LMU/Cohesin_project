library("ggplot2")
library("beeswarm")

n <- c('1', '2', '3','4','5')

# Open and append to new dataframe all intensity measurements for single cells 
# Calculate the sum intensity for each cell
# Control
kontrolle<-data.frame()
for (x in n){
  a <- 'D:/work/Cohesin_Paper/DAPI_Content/HCT_Kontrolle_Chr4_12_19_00'
  b <- x
  path <- paste(a,b, sep="")
  csvs <- list.files(path=path, pattern="*.csv", full.names=TRUE, recursive=FALSE)
  
  matrix <- data.frame()
  for (i in seq_along(csvs)){
    f <- read.delim(csvs[i], header=T, sep=",")
    sumint <-sum(f$RawIntDen)
    sumarea<-sum(f$Area)
    cellNr<-paste('Cell_', strsplit(csvs[i], 'Results')[[1]][2], sep='')
    series<-paste('Series', x, sep='')
    df<-cbind(sumint,sumarea, cellNr, series)
    df<-data.frame(df)
    matrix<-rbind(matrix, df)
  }
  kontrolle <- rbind(kontrolle, matrix)
}

# Auxin treatment
auxin<-data.frame()
for (x in n){
  a <- 'D:/work/Cohesin_Paper/DAPI_Content/HCT_24hAuxin_Chr4_12_19_00'
  b <- x
  path <- paste(a,b, sep="")
  csvs <- list.files(path=path, pattern="*.csv", full.names=TRUE, recursive=FALSE)
  
  matrix <- data.frame()
  for (i in seq_along(csvs)){
    f <- read.delim(csvs[i], header=T, sep=",")
    sumint <-sum(f$RawIntDen)
    sumarea<-sum(f$Area)
    cellNr<-paste('Cell_', strsplit(csvs[i], 'Results')[[1]][2], sep='')
    series<-paste('Series', x, sep='')
    df<-cbind(sumint,sumarea, cellNr, series)
    df<-data.frame(df)
    matrix<-rbind(matrix, df)
  }
  auxin <- rbind(auxin, matrix)
}

kontrolle[5]<-"Kontrolle"
colnames(kontrolle)[5]<-'Condition'
auxin[5]<-"Treat_Auxin"
colnames(auxin)[5]<-'Condition'

# Combine and save all measurements
all <- rbind(kontrolle, auxin)
all[1] = as.numeric(as.character(all$sumint))
all[2] = as.numeric(as.character(all$sumarea))
write.csv(all, 'D:/work/Resaved_Colors_Multil/Measurements.csv')
write.csv(auxin, 'D:/work/Resaved_Colors_Multil/Measurements_Auxin.csv')
write.csv(kontrolle, 'D:/work/Resaved_Colors_Multil/Measurements_Kontrolle.csv')

# Test plots
ggplot(data=all[all$Condition=="Auxin",], aes(x = Condition, y=sumint))+
  geom_jitter(data=all[all$Condition=="Auxin",], aes(x = Condition, y=sumint),width=0.1)+
  geom_jitter(data=all[all$Condition=="Kontrolle",], aes(x = Condition, y=sumint), width=0.1)+
  labs(y='Integrated Intensity')+
  theme_minimal()

beeswarm(sumint ~ Condition, data = all, 
         log = FALSE, pch = 16, col = rainbow(8), method = "hex", ylim = c(1000000,100000000),
         main = 'beeswarm')
bxplot(sumint ~ Condition, data = all, add = TRUE, width=0.2)

beeswarm(sumint ~ Condition, data = all, 
         log = FALSE, pch = 16, col = rainbow(8), ylim = c(1000000,200000000),
         main = 'beeswarm')
bxplot(sumint ~ Condition, data = all, add = TRUE, width=0.2)

median(all[all$Condition=="Kontrolle",]$sumint)
median(all[all$Condition=="Treat_Auxin",]$sumint)

median(all[all$Condition=="Treat_Auxin",]$sumint)/median(all[all$Condition=="Kontrolle" & all$sumint <20000000,]$sumint)
