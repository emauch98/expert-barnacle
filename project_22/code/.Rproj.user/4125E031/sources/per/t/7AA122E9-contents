# Step 0: set up R-script ------------------------------------------------------
rm(list= ls())

source("../../Literature/Moritz_Luehrig_paper_stuff/methods_packages.R")
library(GGally)

sonde_key = fread( "../data/ponds_sonde_key.txt", header=T)
# Treatments:
# S: fish from single population (L26)
# D: fish from 2 populations (double, L26+ERL33)
# NF: no fish

# step 1: merge all raw data files into a single big one -----------------------

path = "../data/raw_data_sondes/Raw_sonde_2022_preprocessed"
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
  setnames(dummy2,colname)
  dummy2[,(colname[c(3,5:length(dummy2))]):=lapply(.SD, as.numeric),.SDcols=colname[c(3,5:length(dummy2))]]
  dummy2$Sonde<-substr(i, 1,7)
  dummy2$Source_file<-i
  files<-rbindlist(list(files, dummy2), fill=TRUE)
})

all <- files

# save intermediate stage 1 (FULL RAW DATA, NO REMOVAL)
fwrite(all, "../data/data_sondes_IntermProcessingSteps/ponds_sonde_data_intermediate_1.txt", sep="\t")

# step 2: add exp. design, phases and format time ------------------------------

all <- fread("../data/data_sondes_IntermProcessingSteps/ponds_sonde_data_intermediate_1.txt", sep="\t")

# format date & time
all$TimeHHMMSS<-substr(all$TimeHHMMSS,1,5) #gsub("\\D+", "", cleaning$TimeHHMMSS)
all$DateMMDDYYYY<-mdy(all$DateMMDDYYYY)
all$Date_time<-ymd_hm(paste(all$DateMMDDYYYY, all$TimeHHMMSS))
setnames(all,1:2,c("Date","Time"))

# add fractions of day since start
# all = all[order(Date_time, Sonde)]
all[,Time_seq:=round(((as.numeric(Date_time))-
                        (as.numeric(Date_time)[1]))/86400,7)]

# merge with key
all<-merge(all,sonde_key, by = "Sonde")

# reorder and revalue
setorderv(all, c("Pond", "Sonde", "Date_time"), c(1, 1, 1))

# select columns to keep
all<-all[,c("Pond","Sonde", "Treatment", "Time_seq", "Date_time",
            "ChlorophyllRFU", "ChlorophyllugL", "ConduScm" , "BGAPCRFU", "ODOmgL", "ODOsat", "pH", "fDOMRFU", "SpConduScm", "TempC")]

# rename
setnames(all, c("Pond", "Sonde", "Treatment", "Time_seq", "Date_time",
                "Chlorophyll_RFU", "Chlorophyll_ugL", "Cond_uScm", "BGAPC_RFU", "ODO_mgL", "ODO_sat", "pH", "fDOM_RFU", "SpCond_uScm", "Temp_C"))

# debug: rm duplicate value
all = unique(all, by=c("Pond", "Time_seq"))

fwrite(all, "../data/data_sondes_IntermProcessingSteps/ponds_sonde_data_intermediate_2.txt", sep="\t")

# step 3.1: remove outliers ----------------------------------------------------
## this loads the outlier detection function and saves the plots indicating outliers
## the different parameters use slightly different settings for window size and cutoff threshold
## because of different patterns of variability

all <- fread("../data/data_sondes_IntermProcessingSteps/ponds_sonde_data_intermediate_2.txt", sep="\t")

save_path = "../outliers_sondes/"

for(i in  c("Chlorophyll_RFU", "Chlorophyll_ugL", "BGAPC_RFU", "ODO_mgL")){
  # Deal with B3P0, which does not have ODO_mgL measurements
  all[,  (i) := if (i != "ODO_mgL" || any(!is.na(.SD[, ..i]))) {
    # Apply outlier removal function if condition is met
    lapply(.SD[, ..i],
           outlier_removal,
           window=48,
           threshold=10,
           plot=T,
           return="removed",
           save=T,
           path=save_path,
           name=paste0(i, "_", Pond))
  } else {
    .SD[, ..i]  # Skip processing for ODO_mgL in B3P0
  },
  by = c("Pond")]
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
                       name=paste0(i,"_",Pond)),
      by = c("Pond")]
}

for(i in c("Temp_C", "SpCond_uScm", "Cond_uScm")){
  all[,  (i) := lapply(.SD[,..i],
                       outlier_removal,
                       window=96,
                       threshold=20,
                       plot=F,
                       return="removed",
                       save=T,
                       path=save_path,
                       name=paste0(i,"_",Pond)),
      by = c("Pond")]
}

# add fractions of day since start
# all = all[order(Date_time, Sonde)]
all[,Time_seq:=round(((as.numeric(Date_time))-
                        (as.numeric(Date_time)[1]))/86400,7)]

fwrite(all, "../data/data_sondes_IntermProcessingSteps/ponds_sonde_data_intermediate_3.1_uncut.txt", sep="\t")

# cut-off "loose ends" on both sides -> all same start and same end of measurements
all <- subset(all, Date_time>"2022-06-23 22:00:00") # Start at 2022-06-23 20:00:00
all <- subset(all, Date_time<"2022-06-29 05:00:00") # End at 2022-06-29 03:00:00

# add fractions of day since start
# all = all[order(Date_time, Sonde)]
all[,Time_seq:=round(((as.numeric(Date_time))-
                        (as.numeric(Date_time)[1]))/86400,7)]

# save
fwrite(all, "../data/data_sondes_IntermProcessingSteps/ponds_sonde_data_intermediate_3.1.txt", sep="\t")

# step 3.2: remove anomalies ---------------------------------------------------
## we inspected the data for anomalies and remove them here - for details see 
## Russo, S., M. L?rig, W. Hao, B. Matthews, and K. Villez. 2020. 
## Active learning for anomaly detection in environmental data. 
## Environmental Modelling & Software 134:104869.

# I didn't do this step, since I would need some time to refresh my python skills, 
# but if you think it is worth, I will manage

# step 4: add light and precipitation data -------------------------------------
# I do not have this data available yet

# step 5: intercept correction -------------------------------------------------
## this uses the results form a cross correlation survey to adjust the 
## sondes for "off factory differences, i.e. remove any intercept variation
## among the data

# I do not have the ponds correlation factors

# step 6: save -----------------------------------------------------------------

## order and remove rownames for cut data
all <- fread("../data/data_sondes_IntermProcessingSteps/ponds_sonde_data_intermediate_3.1.txt", sep="\t")

all = all[order(Pond, Time_seq), ]

rownames(all)<-NULL

fwrite(all, "../data/processed_data_sondes/ponds_sonde_data_all.txt", sep="\t")

## order and remove rownames for uncut data
all2 <- fread("../data/data_sondes_IntermProcessingSteps/ponds_sonde_data_intermediate_3.1_uncut.txt", sep="\t")

all2 = all2[order(Pond, Time_seq), ]

rownames(all2)<-NULL

fwrite(all2, "../data/processed_data_sondes/ponds_sonde_data_all_uncut.txt", sep = "\t")

# step 7 (optional): check completeness ----------------------------------------

all2 <- fread("../data/processed_data_sondes/ponds_sonde_data_all_uncut.txt", sep="\t")

all2$Date <-date(all2$Date_time)

# N per date
all2[, N := uniqueN(Date), by = Pond]
complete <- setDT(data.frame(unclass(table(all2$Date, all2$Pond))), keep.rownames=T)
colnames(complete)[1]<-"Date"
fwrite(complete, "../data/processed_data_sondes/sonde_data_completeness_2022.txt", sep="\t")

# We have huge data-gaps in the measurements of pond ERL152!
# Maybe they got lost during the transfer from the sondes to the field laptop
# in the datafile "~/ZIVI_EAWAG/project_22/data/ponds_sonde_data_all.txt"

# step 8: calculate daily averages ---------------------------------------------
# Does it make sense to calculate daily averages in our case?

all<-fread("../data/processed_data_sondes/ponds_sonde_data_all.txt", sep = "\t")
summary(all)
all[,Time_seq:=as.integer(Time_seq)]

parameters = c("Chlorophyll_RFU", "Cond_uScm", "BGAPC_RFU", "ODO_mgL", "pH",
               "fDOM_RFU", "SpCond_uScm", "Temp_C")
groups = c("Time_seq", "Pond", "Sonde", "Treatment")

all.summ = all[,lapply(.SD, m_r, 2), by=groups, .SDcols = parameters]

fwrite(all.summ, "../data/processed_data_sondes/ponds_sonde_data_daily_avg.txt", sep="\t")

table(all.summ$Pond, all.summ$Treatment)

# step 9: hourly averages ------------------------------------------------------

all<-fread("../data/processed_data_sondes/ponds_sonde_data_all.txt", sep = "\t")

parameters = c("Chlorophyll_RFU", "Cond_uScm", "BGAPC_RFU", "ODO_mgL", "pH",
               "fDOM_RFU", "SpCond_uScm", "Temp_C")

all$Time_seq = as.integer(all$Time_seq)
all$Hour <- as.numeric(substr(as.character(all$Date_time),12,13))

groups = c("Time_seq", "Hour", "Pond", "Treatment")

all.summ = all[,lapply(.SD, m_r, 2), by=groups, .SDcols = parameters]

fwrite(all.summ, "../data/processed_data_sondes/ponds_sonde_data_hourly_avg.txt", sep="\t")

# step 10: 10-minute averages --------------------------------------------------
# I thought that maybe the hour interval is too large for our time series,
# so maybe a 10-minute interval could be more appropriate (or just plot¨
# the absolute values)

all<-fread("../data/processed_data_sondes/ponds_sonde_data_all.txt", sep = "\t")

parameters = c("Chlorophyll_RFU", "Cond_uScm", "BGAPC_RFU",
               "ODO_sat", "ODO_mgL", "pH", "fDOM_RFU", "SpCond_uScm",
               "Temp_C")

all$Date_time <- as.POSIXct((all$Time_seq*86400), origin="2021-06-23 20:00:00", tz="UTC")
# is that the correct code for as.POSIXTct in our case?

all$Time_seq = as.integer(all$Time_seq)
all$Hour <- as.numeric(substr(as.character(all$Date_time),12,13))
all$Ten_min <- as.numeric(substr(as.character(all$Date_time),15,15))


groups = c("Time_seq", "Hour", "Ten_min", "Pond", "Treatment")

all.summ = all[,lapply(.SD, m_r, 2), by=groups, .SDcols = parameters]

fwrite(all.summ, "../data/processed_data_sondes/ponds_sonde_data_ten_min_avg.txt", sep="\t")

# step 11: calculate overall averages of individual ponds ----------------------

all<-fread("../data/processed_data_sondes/ponds_sonde_data_all.txt", sep = "\t")

parameters = c("Chlorophyll_RFU", "Cond_uScm", "BGAPC_RFU", "ODO_mgL", "pH",
               "fDOM_RFU", "SpCond_uScm", "Temp_C")
groups = c("Pond","Treatment","Sonde")

all.summ = all[,lapply(.SD, m_r, 2), by=groups, .SDcols = parameters]

fwrite(all.summ, "../data/processed_data_sondes/ponds_sonde_data_overall_avg.txt", sep="\t")

table(all.summ$Pond, all.summ$Treatment)

# step 12: control plots -------------------------------------------------------

all<-fread("../data/processed_data_sondes/ponds_sonde_data_all.txt", sep = "\t")

all$Date_time <- as.POSIXct((all$Time_seq*86400), origin="2022-06-23 20:00:00", tz="UTC")


# Chloropyll_RFU ---------------------------------------------------------------
p11 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish")

p12 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'ERL122', "ERL152", "ERL85"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'ERL122'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate")) +
  ggtitle("No Fish")

plot_grid(p11, p12, nrow = 2, rel_heights =  c(0.229,0.2,0.2,0.32))

# ------------------------------------------------------------------------------
# ODO mg/L
p3 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish")

p4 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'ERL122', "ERL152", "ERL85"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'ERL122'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate")) +
  ggtitle("No Fish")

plot_grid(p3, p4, nrow = 2, rel_heights =  c(0.229,0.2,0.2,0.32))

# BGAPC RFU
p5 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish")

p6 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=BGAPC_RFU, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'ERL122', "ERL152", "ERL85"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'ERL122'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate")) +
  ggtitle("No Fish")

plot_grid(p5, p6, nrow = 2, rel_heights =  c(0.229,0.2,0.2,0.32))



# Cond uS/cm
p7 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish")

p8 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'ERL122', "ERL152", "ERL85"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'ERL122'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate")) +
  ggtitle("No Fish")

plot_grid(p7, p8, nrow = 2, rel_heights =  c(0.229,0.2,0.2,0.32))

# Speficic Cond uS/cm
p9 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish")

p10 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'ERL122', "ERL152", "ERL85"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'ERL122'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate")) +
  ggtitle("No Fish")

plot_grid(p9, p10, nrow = 2, rel_heights =  c(0.229,0.2,0.2,0.32))

# fDOM RFU
p11 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish")

p12 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'ERL122', "ERL152", "ERL85"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'ERL122'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate")) +
  ggtitle("No Fish")

plot_grid(p11, p12, nrow = 2, rel_heights =  c(0.229,0.2,0.2,0.32))

