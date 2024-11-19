# step 0: startup --------------------------------------------------------------

rm(list = ls())

# replace for your directory
setwd("D:/Temp/pond_exp_osf")

source("./R/methods_packages.R")

corr<-fread("./data_raw/ponds_sonde_corr_factors.txt")
sonde_key = fread( "./data_raw/ponds_sonde_key.txt"  ,header=T)

# step 1: merge all raw data files into a single big one -------------------
## takes a while to run

path = "./data_raw/sonde_csv_files"
filenames <- list.files(path=path, pattern=".txt")
files<-data.table(NULL)

system.time(for(i in filenames){
  filepath <- file.path(path,paste(i,sep=""))
  dummy1<-fread(filepath,
                colClasses = "character" ,
                sep = "\t",
                header = FALSE,
                fill=TRUE,
                encoding ="UTF-8")
  rownumb<-dummy1[V1 %like% "(MM/DD/YYYY)", which=TRUE]+1
  colname<-dummy1[V1 %like% "(MM/DD/YYYY)"]
  colname<-gsub(" ", "",(gsub("[[:punct:]]", "",c(lapply(colname,as.character)))))
  colname<-unlist(strsplit(colname,"\t"))
  dummy2<-dummy1[rownumb:.N]
  dummy3<-dummy2[,tstrsplit(V1, "\t", fixed=TRUE)]
  setnames(dummy3,colname)
  dummy3[,(colname[c(3,5:length(dummy3))]):=lapply(.SD, as.numeric),.SDcols=colname[c(3,5:length(dummy3))]]
  dummy3$Sonde<-substr(i, 1,7)
  dummy3$Source_file<-i
  files<-rbindlist(list(files, dummy3), fill=TRUE)
})

all <- files

# replace "µ" with "" in colnames
setnames(all, gsub("µ", "",  names(all)))

# save intermediate stage 1 (FULL RAW DATA, NO REMOVAL)
fwrite(all, "./data/intermediate_processing_steps/ponds_sonde_data_intermediate_1.txt", sep="\t")


# step 2: add exp. design, phases and format time ---------------------------

all <- fread("./data/intermediate_processing_steps/ponds_sonde_data_intermediate_1.txt", sep="\t")

## disturbance timepoints
perturbations<-c("2016-08-12", "2016-08-26", "2016-09-09", "2016-09-23", "2016-10-10", "2017-10-10")
perturbation_seq <- (as.numeric(ymd_hm(paste(perturbations,"10:00")))-1466812800)/86400 

# format date & time
all$TimeHHMMSS<-substr(all$TimeHHMMSS,1,5) #gsub("\\D+", "", cleaning$TimeHHMMSS)
all$DateMMDDYYYY<-mdy(all$DateMMDDYYYY)
all$Date_time<-ymd_hm(paste(all$DateMMDDYYYY, all$TimeHHMMSS))
setnames(all,1:2,c("Date","Time"))

# cut off some dates (incomplete days before and after maintenance)
all <- subset(all, Date>"2016-06-24")
all<-all[!(Date > "2017-02-16" & Date < "2017-03-23") &
                     !(Date > "2017-09-13" & Date < "2017-10-03") &
                     !(Date > "2018-02-19"),]


# add fractions of day since start
all = all[order(Date_time, Sonde)]
all[,Time_seq:=round(((as.numeric(Date_time))-
                        (as.numeric(Date_time)[1]))/86400,7)] #1466812800

# mark phases
all$Phase<-ifelse(all$Date<=ymd("2017-02-16"), 1,
                       ifelse(all$Date>=ymd("2017-03-23") & all$Date<=ymd("2017-09-13"),2,3))

## mark subphases (resolves biweeklydisturbance regimes)
all$Subphase<-
  ifelse(all$Date<ymd("2016-07-18"), 1,
          ifelse(all$Date<ymd("2016-08-12"), 2,
                   ifelse(all$Date<ymd("2016-08-26"), 3,
                          ifelse(all$Date<ymd("2016-09-09"), 4,
                                 ifelse(all$Date<ymd("2016-09-23"), 5,
                                        ifelse(all$Date<ymd("2016-10-10"), 6,
                                               ifelse(all$Date<=ymd("2016-10-24"), 7,
                                                      ifelse(all$Date<=ymd("2017-02-16"), 8,
                                                            ifelse(all$Date<=ymd("2017-09-13"), 9,
                                                                   ifelse(all$Date<ymd("2017-10-10"), 10,
                                                                          ifelse(all$Date<ymd("2017-10-24"), 11, 12)))))))))))

# merge with key
all<-merge(all,sonde_key, all=F)

# reorder and revalue
setorderv(all, c("Block", "Pond", "Sonde", "Date_time"), c(1, 1, 1, 1))

# select columns to keep
all<-all[,c("Pond","Sonde", "Treatment", "Replicate","Phase", "Subphase", "Time_seq",
            "ChlorophyllRFU", "BGAPCRFU","ODOsat", "ODOmgL", "pH", "fDOMRFU", "SpCondScm", "TempC")]

# rename
setnames(all, c("Pond", "Sonde", "Treatment", "Replicate", "Phase", "Subphase", "Time_seq",
                "Chlorophyll_RFU", "BGAPC_RFU", "ODO_sat", "ODO_mgL", "pH", "fDOM_RFU", "SpCond_uScm", "Temp_C"))



# debug: rm duplicate value
# treatment MD, pond 2C, phase 1, timeseq 102.4583
all = unique(all, by=c("Pond", "Time_seq"))

fwrite(all, "./data/intermediate_processing_steps/ponds_sonde_data_intermediate_2.txt", sep="\t")


# step 3.1: remove outliers -------------------------------------------------
## this load the outlier detection function and saves the plots indicating outliers
## the different parameters use slightly different settings for window size and cutoff threshold
## because of different patterns of variability

all <- fread("./data/intermediate_processing_steps/ponds_sonde_data_intermediate_2.txt", sep="\t")

save_path = "./plots/outliers/"

for(i in  c("Chlorophyll_RFU", "BGAPC_RFU", "ODO_sat", "ODO_mgL")){
  all[,  (i) := lapply(.SD[,..i],
                        outlier_removal,
                        window=48,
                        threshold=10,
                        plot=T,
                        return="removed",
                        save=T,
                        path=save_path,
                        name=paste0(i,"_",Pond,"_", Phase)),
      by = c("Pond", "Phase")]
}


for(i in c("fDOM_RFU")){
  all[,  (i) := lapply(.SD[,..i],
                       outlier_removal,
                       window=96,
                       threshold=10,
                       plot=F,
                       return="removed",
                       save=T,
                       path=save_path,
                       name=paste0(i,"_",Pond,"_", Phase)),
      by = c("Pond", "Phase")]
}


for(i in c("Temp_C", "SpCond_uScm")){
  all[,  (i) := lapply(.SD[,..i],
                       outlier_removal,
                       window=96,
                       threshold=20,
                       plot=F,
                       return="removed",
                       save=T,
                       path=save_path,
                       name=paste0(i,"_",Pond,"_", Phase)),
      by = c("Pond", "Phase")]
}


# save
fwrite(all, "./data/intermediate_processing_steps/ponds_sonde_data_intermediate_3.1.txt", sep="\t")


# step 3.2: remove anomalies ----------------------------------------------
## we inspected the data for anomalies and remove them here - for details see 
## Russo, S., M. L�rig, W. Hao, B. Matthews, and K. Villez. 2020. 
## Active learning for anomaly detection in environmental data. 
## Environmental Modelling & Software 134:104869.

all <- fread("./data/intermediate_processing_steps/ponds_sonde_data_intermediate_3.1.txt", sep="\t")

## SpCond_uScm
# 1
all[(Time_seq >= 12 & Time_seq <= 17) & Pond=="4C" , SpCond_uScm := NA]
all[(Time_seq >= 20 & Time_seq <= 20) & Pond=="3A" , SpCond_uScm := NA]

# 2
all[(Time_seq >= 25 & Time_seq <= 29) & Pond=="5C" , SpCond_uScm := NA]

#3
all[(Time_seq >= 52 & Time_seq <= 54) & Pond=="1C" , SpCond_uScm := NA]
all[(Time_seq >= 51 & Time_seq <= 55) & Pond=="2A" , SpCond_uScm := NA]
all[(Time_seq >= 50 & Time_seq <= 57) & Pond=="3A" , SpCond_uScm := NA]
all[(Time_seq >= 52 & Time_seq <= 53) & Pond=="3D" , SpCond_uScm := NA]
all[(Time_seq >= 50 & Time_seq <= 57) & Pond=="4B" , SpCond_uScm := NA]
all[(Time_seq >= 52 & Time_seq <= 56) & Pond=="4D" , SpCond_uScm := NA]

#4
all[(Time_seq >= 64 & Time_seq <= 70) & Pond=="1B" , SpCond_uScm := NA]
all[(Time_seq >= 70 & Time_seq <= 72) & Pond=="2B" , SpCond_uScm := NA]
all[(Time_seq >= 64 & Time_seq <= 71) & Pond=="2C" , SpCond_uScm := NA]
all[(Time_seq >= 64 & Time_seq <= 73) & Pond=="2D" , SpCond_uScm := NA]
all[(Time_seq >= 62 & Time_seq <= 72) & Pond=="4B" , SpCond_uScm := NA]
all[(Time_seq >= 66 & Time_seq <= 68) & Pond=="4C" , SpCond_uScm := NA]
all[(Time_seq >= 63 & Time_seq <= 71) & Pond=="4D" , SpCond_uScm := NA]
all[(Time_seq >= 63 & Time_seq <= 65) & Pond=="5C" , SpCond_uScm := NA]

#5
all[(Time_seq >= 80 & Time_seq <= 84) & Pond=="2C" , SpCond_uScm := NA]
all[(Time_seq >= 79 & Time_seq <= 80) & Pond=="2D" , SpCond_uScm := NA]
all[(Time_seq >= 79 & Time_seq <= 86) & Pond=="5B" , SpCond_uScm := NA]
all[(Time_seq >= 80 & Time_seq <= 85) & Pond=="5C" , SpCond_uScm := NA]
all[(Time_seq >= 88 & Time_seq <= 89) & Pond=="5C" , SpCond_uScm := NA]

#5
all[(Time_seq >= 96 & Time_seq <= 103) & Pond=="1C" , SpCond_uScm := NA]
all[(Time_seq >= 98 & Time_seq <= 101) & Pond=="3A" , SpCond_uScm := NA]
all[(Time_seq >= 93 & Time_seq <= 97) & Pond=="5B" , SpCond_uScm := NA]

# ODO_sat
all[(Time_seq >= 128 & Time_seq <= 128) & Pond=="5C" , ODO_sat := NA]
all[(Time_seq >= 214 & Time_seq <= 216) & Pond=="5A" , ODO_sat := NA]

# fDOM_RFU
all[(Time_seq >= 31 & Time_seq <= 36) & Pond=="1C" , fDOM_RFU := NA]
all[(Time_seq >= 173 & Time_seq <= 186) & Pond=="5A" , fDOM_RFU := NA]
all[(Time_seq >= 382 & Time_seq <= 397) & Pond=="1D" , fDOM_RFU := NA]
all[(Time_seq >= 382 & Time_seq <= 397) & Pond=="5C" , fDOM_RFU := NA]
all[(Time_seq >= 478 & Time_seq <= 479) & Pond=="2B" , fDOM_RFU := NA]
all[(Time_seq >= 480 & Time_seq <= 480) & Pond=="2B" , fDOM_RFU := NA]
all[(Time_seq >= 482 & Time_seq <= 482) & Pond=="2B" , fDOM_RFU := NA]
all[(Time_seq >= 485 & Time_seq <= 485) & Pond=="2B" , fDOM_RFU := NA]


## pH
all[all$Time_seq > 112 & all$Time_seq < 271, pH := NA]


fwrite(all, "./data/intermediate_processing_steps/ponds_sonde_data_intermediate_3.2.txt", sep="\t")


# step 4: add light and precipitation data --------------------------------
## 


all <- fread("./data/intermediate_processing_steps/ponds_sonde_data_intermediate_3.2.txt", sep="\t")

# light manual
light.manual <- fread("./data_raw/abiotic_meteo/ponds_light_manual.txt", sep="\t", header=T)
light.manual$Time_seq <- (as.numeric(dmy_hm(light.manual$Date_time))/86400)-16977
light.manual<-approx(light.manual$Time_seq, light.manual$PAR, xout=unique(all$Time_seq), method="linear")
light.manual<-setDT(light.manual)
colnames(light.manual)<-c("Time_seq","PAR")

# light meteo
light.meteo<-fread("./data_raw/abiotic_meteo/order_62804_data.txt", header=T, sep=";")
light.meteo$Time_seq <- (as.numeric(ymd_hm(light.meteo$time))/86400)-16977
light.meteo<-approx(light.meteo$Time_seq, light.meteo$gre000z0, xout=unique(all$Time_seq), method="linear")
light.meteo<-setDT(light.meteo)
colnames(light.meteo)<-c("Time_seq","GR")

# temp meteo
temp.meteo<-fread("./data_raw/abiotic_meteo/order_64353_data.txt", header=T, sep=";")
temp.meteo$Time_seq <- (as.numeric(ymd_hm(temp.meteo$time))/86400)-16977
temp.meteo<-approx(temp.meteo$Time_seq, temp.meteo$tre200s0, xout=unique(all$Time_seq), method="linear")
temp.meteo<-setDT(temp.meteo)
colnames(temp.meteo)<-c("Time_seq","Temp_air")

# rain meteo
rain.meteo<-fread("./data_raw/abiotic_meteo/order_62802_data.txt", header=T, sep=";")
rain.meteo$Time_seq <- (as.numeric(ymd_hm(rain.meteo$time))/86400)-16977
rain.meteo<-approx(rain.meteo$Time_seq, rain.meteo$rre150z0, xout=unique(all$Time_seq), method="linear")
rain.meteo<-setDT(rain.meteo)
colnames(rain.meteo)<-c("Time_seq","Rain")

# wind meteo
wind.meteo<-fread("./data_raw/abiotic_meteo/order_67461_data.txt", header=T, sep=";")
wind.meteo$Time_seq <- (as.numeric(ymd_hm(wind.meteo$time))/86400)-16977
wind.meteo<-approx(wind.meteo$Time_seq, wind.meteo$fkl010z0, xout=unique(all$Time_seq), method="linear")
wind.meteo<-setDT(wind.meteo)
colnames(wind.meteo)<-c("Time_seq","Wind")

# combine
abiotic<-cbind( temp.meteo, light.manual[,"PAR"], light.meteo[,"GR"], rain.meteo[,"Rain"], wind.meteo[,"Wind"])
abiotic = abiotic[order(Time_seq)]

# do cross correlation of non-missing data to find lag, remove lag, and create linear model for replacement in PAR1
sub<-subset(abiotic, (Time_seq<140 | Time_seq>180) & (Time_seq<297 | Time_seq>352))
ccf(sub$PAR, sub$GR,na.action = na.pass)
sub<-sub %>%
  mutate(GR_lagged=lag(GR, n=6))
mod<-lm(PAR~GR_lagged, data=sub)

## fill missing periods
abiotic[(Time_seq>140 & Time_seq<180) | (Time_seq>297 & Time_seq<352),PAR:=(GR*coef(mod)[[2]])]
abiotic[,c("PAR", "GR", "Rain", "Wind", "Temp_air") := round(.SD,1), .SDcols=c("PAR", "GR", "Rain", "Wind", "Temp_air")]

# remove noisy signal at night
abiotic$PAR <- ifelse(abiotic$PAR<20,0, abiotic$PAR)

## merge all
all<-merge(all, abiotic, all.x=T, by="Time_seq")

fwrite(abiotic, "./data/ponds_abiotic_data.txt", sep="\t")


# step 5: intercept correction --------------------------------------------
## this uses the results form a cross correlation survey to adjust the 
## sondes for "off factory differences, i.e. remove any intercept variation
## among the data


# adjust intercept with cross correlation results 
for(i in unique(corr$Sonde)){
  sub.corr<-subset(corr, Sonde==i)
  for(j in unique(sub.corr$Parameter)){
    intercept = as.numeric(sub.corr[Parameter==j, "intercept"])
    slope = 1
    corr_val <- round((all[Sonde==i,..j]/slope)-intercept,2)
    all[Sonde==i,(j):=corr_val]
  }
}

# set all bigger than zero
all$Chlorophyll_RFU = all$Chlorophyll_RFU + 1
all[, BGAPC_RFU:=if(min(BGAPC_RFU, na.rm = T)<=0.1)
  {BGAPC_RFU+abs(min(BGAPC_RFU, na.rm=T))+0.1}
  else {BGAPC_RFU}, by="Pond"]
all$fDOM_RFU = all$fDOM_RFU + 1.5

# step 6: save ------------------------------------------------------------

## order and remove rownames
all = all[order(Pond, Time_seq), ]
all[,"Sonde"] = NULL
rownames(all)<-NULL

fwrite(all, "./data/ponds_sonde_data_all.txt")


# step 7 (optional): check completeness -----------------------------------

all <- fread("./data/intermediate_processing_steps/ponds_sonde_data_intermediate_3.2.txt", sep="\t")
all$Date_time <- as.POSIXct((all$Time_seq*86400), origin="2016-06-25 00:00:00", tz="UTC")
all$Date <-date(all$Date_time)

# N per date
all[, N := uniqueN(Date), by = Sonde] #
complete <- setDT(data.frame(unclass(table(all$Date, all$Sonde))), keep.rownames=T)
colnames(complete)[1]<-"Date"
fwrite(complete, "./data/sonde_data_completeness.txt", sep="\t")


# step 8: calculate daily averages --------------------------------------------------

all<-fread("./data/ponds_sonde_data_all.txt")
all[,Time_seq:=as.integer(Time_seq)]

parameters = c("Chlorophyll_RFU","BGAPC_RFU", "ODO_sat", "ODO_mgL", "pH", "fDOM_RFU", "SpCond_uScm",
               "Temp_C", "Temp_air", "PAR", "GR", "Wind")
groups = c("Time_seq", "Pond", "Replicate","Treatment", "Phase", "Subphase")

all.summ = all[,lapply(.SD, m_r, 2), by=groups, .SDcols = parameters]
all.summ$Rain = all[, sum(Rain), by=groups]$V1

fwrite(all.summ, "./data/ponds_sonde_data_daily_avg.txt", sep=",")

table(all.summ$Pond, all.summ$Treatment)


# step 9: hourly averages -------------------------------------------------

all<-fread("./data/ponds_sonde_data_all.txt")

parameters = c("Chlorophyll_RFU","BGAPC_RFU", "ODO_sat", "ODO_mgL", "pH", "fDOM_RFU", "SpCond_uScm",
               "Temp_C", "Temp_air", "PAR", "GR", "Wind")

all$Date_time <- as.POSIXct((all$Time_seq*86400), origin="2016-06-25 00:00:00", tz="UTC")

all$Time_seq = as.integer(all$Time_seq)
all$Hour <- as.numeric(substr(as.character(all$Date_time),12,13))

groups = c("Time_seq", "Hour","Pond","Replicate", "Treatment", "Phase", "Subphase")

all.summ = all[,lapply(.SD, m_r, 2), by=groups, .SDcols = parameters]
all.summ$Rain = all[, sum(Rain), by=groups]$V1

fwrite(all.summ, "./data/ponds_sonde_data_hourly_avg.txt", sep=",")


# step 10 (optional): control plots --------------------------------------------------

all<-fread("./data/ponds_sonde_data_all.txt")

all$Date_time <- as.POSIXct((all$Time_seq*86400), origin="2016-06-25 00:00:00", tz="UTC")

sub = subset(all, Pond=="1A")
sub[,Subphase:=factor(Subphase)]

ggplot(sub) + theme_light() +
  geom_line(aes(y=Chlorophyll_RFU, x=Date_time, group=Subphase, colour=Subphase)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  scale_colour_manual(values=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
                               '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')) +
  geom_vline(xintercept = ymd_hms(paste(perturbations, "00:00:00"))) +
  theme(legend.position = "bottom")


table(all$Pond, all$Treatment)
