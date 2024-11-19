# Step 0: set up R-script ------------------------------------------------------
rm(list= ls())

source("../../Literature/Moritz_Luehrig_paper_stuff/methods_packages.R")
require(GGally)

sonde_key = fread( "../data/key_2023.txt", header=T)

# step 1: merge all raw data files into a single big one -----------------------

path = "../data/raw_data_sondes_preprocessed"
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
  dummy2$Pond<-sub("_Sonde.*", "", i)
  dummy2$Source_file<-i
  files<-rbindlist(list(files, dummy2), fill=TRUE)
})

all <- files

# remove last 3 columns (some variables only measured by few sondes)
all <- all[,!c("Depthm", "Pressurepsia", "VerticalPositionm")]

# step 2: add exp. design, phases and format time ------------------------------

# format date & time
all$TimeHHmmss<-substr(all$TimeHHmmss,1,5) #gsub("\\D+", "", cleaning$TimeHHMMSS)
all$DateMMDDYYYY<-mdy(all$DateMMDDYYYY)
all$Date_time<-ymd_hm(paste(all$DateMMDDYYYY, all$TimeHHmmss))
setnames(all,1:2,c("Date","Time"))
all$Date_time <- as.POSIXct(all$Date_time, tz = "UTC")

# # Check start and end times for equal cutting
# all[,c(min(Date_time), max(Date_time)), by = Pond]

# Cut before and after to obtain the same period for all the ponds
all <- subset(all, Date_time>="2023-06-23 00:00:00" &
                 Date_time<="2023-07-17 04:00:00") # Keep a few hours before and after
# to make sure we have complete days

# add fractions of day since start
# all[,Time_seq:=round(((as.numeric(Date_time))-
#                         (as.numeric(Date_time)[1]))/86400,7)]

# merge with key
all<-merge(all,sonde_key, by = "Pond")

# reorder and revalue
setorderv(all, c("Pond", "Date_time"), c(1, 1))

# select columns to keep
all<-all[,c("Pond", "Sonde", "Treatment", "Date_time",
            "ChlorophyllRFU", "ConduScm", "ODOsat", "ODOmgL", "fDOMRFU", "SpConduScm", "pH", "TempC")]

# rename
setnames(all, c("Pond", "Sonde", "Treatment", "Date_time",
                "Chlorophyll_RFU", "Cond_uScm", "ODO_sat", "ODO_mgL", "fDOM_RFU", "SpCond_uScm", "pH", "Temp_C"))

# debug: rm duplicate value
all = unique(all, by=c("Pond", "Date_time"))

# step 3.1: remove outliers ----------------------------------------------------
## this loads the outlier detection function and saves the plots indicating outliers
## the different parameters use slightly different settings for window size and cutoff threshold
## because of different patterns of variability

save_path = "../outliers_sondes/"

for(i in  c("Chlorophyll_RFU", "ODO_mgL")){
  all[,  (i) := lapply(.SD[,..i],
                       outlier_removal,
                       window=48,
                       threshold=10,
                       plot=T,
                       return="removed",
                       save=T,
                       path=save_path,
                       name=paste0(i,"_",Pond)),
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

# save
fwrite(all, "../data/data_sondes_IntermProcessingSteps/ponds_sonde_data_intermediate_1.txt", sep="\t")

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

## order and remove rownames
all = all[order(Pond, Date_time), ]
rownames(all)<-NULL

fwrite(all, "../data/processed_data_sondes/ponds_sonde_data_all.txt")

# step 7 (optional): check completeness ----------------------------------------

# all <- fread("../data/data_sondes_IntermProcessingSteps/ponds_sonde_data_intermediate_3.1.txt", sep="\t")
# all$Date_time <- as.POSIXct((all$Time_seq*86400), origin="2023-06-23 20:00:00", tz="UTC")

all$Date <-date(all$Date_time)

# N per date
all[, N := uniqueN(Date), by = Pond]
complete <- setDT(data.frame(unclass(table(all$Date, all$Pond))), keep.rownames=T)
colnames(complete)[1]<-"Date"
fwrite(complete, "../data/processed_data_sondes/sonde_data_completeness_2023.txt", sep="\t")
# ERL122 was measured in 15min increments!
# Data is complete otherwise

# step 8: calculate daily averages ---------------------------------------------
# Does it make sense to calculate daily averages in our case?
# 
# all<-fread("../data/processed_data_sondes/ponds_sonde_data_all.txt")
# summary(all)
# all[,Time_seq:=as.integer(Time_seq)]
# 
# parameters = c("Chlorophyll_RFU", "Chlorophyll_ugL", "Cond_uScm", "BGAPC_RFU", "BGAPC_ugL",
#                "ODO_sat", "ODO_mgL", "pH", "fDOM_RFU", "SpCond_uScm",
#                "Temp_C")
# groups = c("Time_seq", "Pond", "Treatment")
# 
# all.summ = all[,lapply(.SD, m_r, 2), by=groups, .SDcols = parameters]
# 
# fwrite(all.summ, "../data/processed_data_sondes/ponds_sonde_data_daily_avg.txt", sep=",")
# 
# table(all.summ$Pond, all.summ$Treatment)

# step 9: hourly averages ------------------------------------------------------
# 
# all<-fread("../data/processed_data_sondes/ponds_sonde_data_all.txt")
# 
# parameters = c("Chlorophyll_RFU", "Cond_uScm", "BGAPC_RFU", "ODO_mgL", "pH",
#                "fDOM_RFU", "SpCond_uScm", "Temp_C")
# 
# all$Time_seq = as.integer(all$Time_seq)
# all$Hour <- as.numeric(substr(as.character(all$Date_time),12,13))
# 
# groups = c("Time_seq", "Hour", "Pond", "Treatment")
# 
# all.summ = all[,lapply(.SD, m_r, 2), by=groups, .SDcols = parameters]
# 
# fwrite(all.summ, "../data/processed_data_sondes/ponds_sonde_data_hourly_avg.txt", sep=",")

# step 10: 10-minute averages --------------------------------------------------
# I thought that maybe the hour interval is too large for our time series,
# so maybe a 10-minute interval could be more appropriate (or just plot¨
# the absolute values)
# 
# all<-fread("../data/processed_data_sondes/ponds_sonde_data_all.txt")
# 
# parameters = c("Chlorophyll_RFU", "Chlorophyll_ugL", "Cond_uScm", "BGAPC_RFU", "BGAPC_ugL",
#                "ODO_sat", "ODO_mgL", "pH", "fDOM_RFU", "SpCond_uScm",
#                "Temp_C")
# 
# all$Date_time <- as.POSIXct((all$Time_seq*86400), origin="2021-09-16 20:00:00", tz="UTC")
# # is that the correct code for as.POSIXTct in our case?
# 
# all$Time_seq = as.integer(all$Time_seq)
# all$Hour <- as.numeric(substr(as.character(all$Date_time),12,13))
# all$Ten_min <- as.numeric(substr(as.character(all$Date_time),15,15))
# 
# 
# groups = c("Time_seq", "Hour", "Ten_min", "Pond", "Treatment")
# 
# all.summ = all[,lapply(.SD, m_r, 2), by=groups, .SDcols = parameters]
# 
# fwrite(all.summ, "../data/processed_data_sondes/ponds_sonde_data_ten_min_avg.txt", sep=",")

# step 11: calculate overall averages of individual ponds ----------------------
# 
# all<-fread("../data/processed_data_sondes/ponds_sonde_data_all.txt")
# 
# parameters = c("Chlorophyll_RFU", "Cond_uScm", "BGAPC_RFU", "ODO_mgL", "pH",
#                "fDOM_RFU", "SpCond_uScm", "Temp_C")
# groups = c("Pond","Treatment")
# 
# all.summ = all[,lapply(.SD, m_r, 2), by=groups, .SDcols = parameters]
# 
# fwrite(all.summ, "../data/processed_data_sondes/ponds_sonde_data_overall_avg.txt", sep=",")
# 
# table(all.summ$Pond, all.summ$Treatment)

# step 12: control plots --------------------------------------------

# all$Date_time <- as.POSIXct((all$Time_seq*86400), origin="2021-09-16 20:00:00", tz="UTC")

all <- fread("../data/processed_data_sondes/ponds_sonde_data_all.txt", sep = ",")

table(all$Pond, all$Treatment)

# Chlorophyll RFU
p1 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish")

p2 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B1P4"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=Chlorophyll_RFU, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'B1P4', "ERL152", "ERL85", "ERL122"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'B1P4'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate", "ERL122"="midnightblue")) +
  ggtitle("Fishless reference")

plot_grid(p1, p2, nrow = 2, rel_heights =  c(0.229,0.2,0.2,0.32))

# ODO mg/L
p5 = ggplot(subset(all, Pond=="B1P1")) + theme_minimal() +
  geom_line(aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish") +
  labs(x = "Date", y = "DO mg/L") +
  theme(
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 20)
  )

p6 = ggplot(subset(all, Pond=="B2P4")) + theme_minimal() +
  geom_line(aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B1P4"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=ODO_mgL, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'B1P4', "ERL152", "ERL85", "ERL122"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'B1P4'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate", "ERL122"="midnightblue")) +
  ggtitle("Fishless reference") +
  labs(x = "Date", y = "DO mg/L") +
  theme(
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 20)
  )

# Save the plot as a .png
png("../../Presentations/control_DO_2023.png", width = 1600, height = 1000)
plot_grid(p5, p6, nrow = 2)
dev.off()

# Cond uS/cm
p7 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish")

p8 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B1P4"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=Cond_uScm, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'B1P4', "ERL152", "ERL85", "ERL122"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'B1P4'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate", "ERL122"="midnightblue")) +
  ggtitle("Fishless reference")

plot_grid(p7, p8, nrow = 2)

# Specific Cond uS/cm
p9 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish")

p10 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B1P4"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=SpCond_uScm, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'B1P4', "ERL152", "ERL85", "ERL122"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'B1P4'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate", "ERL122"="midnightblue")) +
  ggtitle("Fishless reference")

plot_grid(p9, p10, nrow = 2)

# fDOM RFU
p11 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish")

p12 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B1P4"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=fDOM_RFU, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'B1P4', "ERL152", "ERL85", "ERL122"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'B1P4'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate", "ERL122"="midnightblue")) +
  ggtitle("Fishless reference")

plot_grid(p11, p12, nrow = 2)

