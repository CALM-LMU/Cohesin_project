library("ggplot2")

n <- c('01', '02', '03','04','05','06','07', '08', '09', '10', '11', '12', '13', '14')
background_value = 500

# Open and append to new dataframe all intensity measurements for single cells
# Median and Mode normalization for single traces after background subtraction (modal grey value of 500)
series<-data.frame()
for (x in n){
  a <- 'D:/work/Cohesin_Paper/Auxin_treatment_Timeseries/20180919_Timelapse_2-well_001.nd2 - 20180919_Timelapse_2-well_001.nd2 (series '
  b <- x
  c <- ').tif_SiR'
  path <- paste(a,b,c, sep="")
  
  tab <- data.frame()
  csvs <- list.files(path=path, pattern="*.csv", full.names=TRUE, recursive=FALSE)
  for (file in csvs){
    f <- read.delim(file, header=T, sep=",")
    
    cellname <- strsplit(file, 'Results')
    f[10]<-cellname[[1]][2]
    colnames(f)[10]<-'CellNr'
    seriesname<-paste('series', x, sep="")
    f[11]<-seriesname
    colnames(f)[11]<-'Series'
    f[12]<-'NA'
    f[13]<-'NA'
    colnames(f)[12]<-'Mode_SubBackg'
    colnames(f)[13]<-'NormMode'
    for (i in 1:length(f$Mode)){
      f$Mode_SubBackg[i]<-as.numeric(f$Mode[i]-background_value)
      f$NormMode[i]<-as.numeric(f$Mode_SubBackg[i])/as.numeric(f$Mode_SubBackg[1])
    }
    f[14]<-'NA'
    f[15]<-'NA'
    colnames(f)[14]<-'Median_SubBackg'
    colnames(f)[15]<-'NormMedian'
    for (i in 1:length(f$Median)){
      f$Median_SubBackg[i]<-as.numeric(f$Median[i]-background_value)
      f$NormMedian[i]<-as.numeric(f$Median_SubBackg[i])/as.numeric(f$Median_SubBackg[1])
    }
    tab <- rbind(tab, f)
  }
  series <- rbind(series, tab)
}

# write.csv(series, 'D:/work/Cohesin_Paper/Auxin_treatment_Timeseries/Measurements.csv')

# quick and dirty extraction of series 1-7
series1to7<-data.frame()
series1to7<- rbind(series[series$Series=='series01',],
series[series$Series=='series02',],
series[series$Series=='series03',],
series[series$Series=='series04',],
series[series$Series=='series05',],
series[series$Series=='series06',],
series[series$Series=='series07',])
write.csv(series1to7, 'D:/work/Cohesin_Paper/Auxin_treatment_Timeseries/Measurements_series1to7.csv')

# quick and dirty extraction of series8-14
series8to14<-data.frame()
series8to14<- rbind(series[series$Series=='series08',],
series[series$Series=='series09',],
series[series$Series=='series10',],
series[series$Series=='series11',],
series[series$Series=='series12',],
series[series$Series=='series13',],
series[series$Series=='series14',])
write.csv(series8to14, 'D:/work/Cohesin_Paper/Auxin_treatment_Timeseries/Measurements_series8to14.csv')


# Test plots
ggplot(data=series1to7[series1to7$X==1,], aes(x=Median, y=..count..))+
  geom_histogram(bins=60)+
  xlim(500,800)+
  ylim(0,10)

ggplot(data=series8to14[series8to14$X==1,], aes(x=Median, y=..count..))+
  geom_histogram(bins=60)+
  xlim(500,800)+
  ylim(0,10)


ggplot(data=series1to7, aes(x=X, y=..count..))+
  geom_histogram(bins = 84)+
  ylim(0,100)
ggplot(data=series8to14, aes(x=X, y=..count..))+
  geom_histogram(bins = 84)+
  ylim(0,100)


