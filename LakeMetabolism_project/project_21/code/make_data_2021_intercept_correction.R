################################################################################
# This is a script where the aim is to do an intercept-correction regarding the
# Sondes-data of the Stickeback-experiments of Blakes Group in Greenland 2021.
# The variable of interest is Chlorophyll ug/L measured by the Sondes, which will
# be compared with the actual Chlorophyll content measured chemically at the same specific
# point in time. All the other datapoints of this sonde will then be adjusted according to
# this difference. For example, if the sondes measurement at a given time-point 
# gives 12 ug/L and the chemical analysis gives 11 ug/L, 1 ug/L will be subtracted
# from all the datapoints of this sonde.
# The idea is that the Sondes in the different ponds are then adjusted for factory-differences
# which makes them better comparable.
# Unfortunately, the chemical measurements were made a few days prior to the
# deployment sondes. Therefore, I will take the average
# of the Chlorophyll measurements measured by the deployment sondes at the same day-time
# as the chemical measurements to compare.
################################################################################

# Step 0: set up R-script ------------------------------------------------------

rm(list= ls())

library(lubridate)

setwd("~/ZIVI_EAWAG/project_21")

source("~/ZIVI_EAWAG/project_21/Moritz_Luehrig_paper_stuff/methods_packages.R")

# import the chlorophyll A dataset, containing the Chlorophyll mg/L measurements
# chemically analyzed at a specific point in time
# Danina told me that they took the water samples mostly right after the deployment,
# so I added 10 minutes to the last deployment observation to determine the timepoint
# of the water-sample
chloro <- fread("~/ZIVI_EAWAG/project_21/data/chloro_a_2021.txt", sep = "\t")

# Import the complete dataset
all<-fread("~/ZIVI_EAWAG/project_21/data/ponds_sonde_data_all.txt")

# Step 1: Pond B2P3 ------------------------------------------------------------
# Treatment: Fish

# subset
sub<-subset(all,Pond=="B2P3")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="B2P3"] # 2021-09-12 15:12:15 UTC

# Value:
measured <- chloro$Chla_ugL[chloro$Pond=="B2P3"] # 8.841463

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-time of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 15:12:00", tz = "UTC")
timepoint2 <- as.POSIXct("2021-09-18 15:12:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 15:12:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 15:12:00", tz = "UTC")
timepoint5 <- as.POSIXct("2021-09-21 15:12:00", tz = "UTC")
timepoint6 <- as.POSIXct("2021-09-22 15:12:00", tz = "UTC")
timepoint7 <- as.POSIXct("2021-09-23 15:12:00", tz = "UTC")

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4, timepoint5,
             timepoint6, timepoint7)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  counter = counter + val
}

# Average Chloro ug/L at the same day-time
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg
# -> add 4.685749 on every observation of B2P3

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

B2P3 <- sub

# Step 2: Pond B2P2 ------------------------------------------------------------
# Treatment: Fish

# Subset
sub<-subset(all,Pond=="B2P2")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="B2P2"] # 2021-09-12 16:50:52 UTC

# Value:
measured <- chloro$Chla_ugL[chloro$Pond=="B2P2"] # 11.12805

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-time of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 16:51:00", tz = "UTC")
timepoint2 <- as.POSIXct("2021-09-18 16:51:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 16:51:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 16:51:00", tz = "UTC")
timepoint5 <- as.POSIXct("2021-09-21 16:51:00", tz = "UTC")
timepoint6 <- as.POSIXct("2021-09-22 16:51:00", tz = "UTC")
timepoint7 <- as.POSIXct("2021-09-23 16:51:00", tz = "UTC")

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4, timepoint5,
             timepoint6, timepoint7)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  counter = counter + val
}

# Average Chloro ug/L at the same day-time
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

B2P2 <- sub

# Step 3: Pond B1P1 ------------------------------------------------------------
# Treatment: Fish
# Unfortunately, B1P1 has some major data-gaps (decribed in the Data-processing
# report of 2021). Therefore, not all the reference day-times might be awailable
# to calculate the average from.

# subset
sub<-subset(all,Pond=="B1P1")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="B1P1"]

# Value:
measured <- chloro$Chla_ugL[chloro$Pond=="B1P1"] # 11.46341

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-times of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 16:27:00", tz = "UTC")
timepoint2 <- as.POSIXct("2021-09-18 16:27:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 16:27:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 16:27:00", tz = "UTC")
# timepoint5 <- as.POSIXct("2021-09-21 16:27:00", tz = "UTC") # This Date_time is NA due to data-gaps
# timepoint6 <- as.POSIXct("2021-09-22 16:27:00", tz = "UTC") # This Date_time is NA due to data-gaps
# timepoint7 <- as.POSIXct("2021-09-23 16:27:00", tz = "UTC") # This Date_time is NA due to data-gaps

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  counter = counter + val
}

# Average Chloro ug/L at the same day-time
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

# Rename
B1P1 <- sub

# Step 4: Pond B3P3 ------------------------------------------------------------
# Treatment: Fish

# Subset
sub<-subset(all,Pond=="B3P3")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="B3P3"] # "2021-09-13 12:10:34 UTC"

# Value:
measured <- chloro$Chla_ugL[chloro$Pond=="B3P3"] # 7.560976

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-time of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 12:11:00", tz = "UTC") # Not exactly the same minute due to different intervals
timepoint2 <- as.POSIXct("2021-09-18 12:11:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 12:11:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 12:11:00", tz = "UTC")
timepoint5 <- as.POSIXct("2021-09-21 12:11:00", tz = "UTC")
timepoint6 <- as.POSIXct("2021-09-22 12:11:00", tz = "UTC")
timepoint7 <- as.POSIXct("2021-09-23 12:11:00", tz = "UTC")

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4, timepoint5,
             timepoint6, timepoint7)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  counter = counter + val
}

# Average Chloro ug/L at the same day-time
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg
# -> add 5.513833 on every observation of B3P3

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

B3P3 <- sub

# Step 5: Pond B3P1 ------------------------------------------------------------
# Treatment: Fish

# Subset
sub<-subset(all,Pond=="B3P1")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="B3P1"] # "2021-09-13 16:18:34 UTC"

# Value measured chemically:
measured <- chloro$Chla_ugL[chloro$Pond=="B3P1"] # 12.2561

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-time of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 16:18:00", tz = "UTC")
timepoint2 <- as.POSIXct("2021-09-18 16:18:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 16:18:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 16:18:00", tz = "UTC")
timepoint5 <- as.POSIXct("2021-09-21 16:18:00", tz = "UTC")
timepoint6 <- as.POSIXct("2021-09-22 16:18:00", tz = "UTC")
timepoint7 <- as.POSIXct("2021-09-23 16:18:00", tz = "UTC")

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4, timepoint5,
             timepoint6, timepoint7)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  counter = counter + val
}

# Average Chloro ug/L at the same day-time
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg
# -> add 8.581812 on every observation of B3P3

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

B3P1 <- sub

# Step 6: Pond B3P2 ------------------------------------------------------------
# Treatment: Fish

# Subset
sub<-subset(all,Pond=="B3P2")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="B3P2"] # "2021-09-13 18:23:42 UTC"

# Value measured chemically:
measured <- chloro$Chla_ugL[chloro$Pond=="B3P2"] # 11.03659

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-time of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 18:23:00", tz = "UTC")
timepoint2 <- as.POSIXct("2021-09-18 18:23:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 18:23:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 18:23:00", tz = "UTC")
timepoint5 <- as.POSIXct("2021-09-21 18:23:00", tz = "UTC")
timepoint6 <- as.POSIXct("2021-09-22 18:23:00", tz = "UTC")
timepoint7 <- as.POSIXct("2021-09-23 18:23:00", tz = "UTC")

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4, timepoint5,
             timepoint6, timepoint7)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  counter = counter + val
}

# Average Chloro ug/L at the same day-time
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg
# -> add 7.9223 on every observation of B3P2

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

B3P2 <- sub

# Step 7: Pond B3P0 ------------------------------------------------------------
# Treatment: No Fish

# Subset
sub<-subset(all,Pond=="B3P0")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="B3P0"] # "2021-09-13 17:00:15 UTC"

# Value measured chemically:
measured <- chloro$Chla_ugL[chloro$Pond=="B3P0"] # 2.012195

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-time of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 17:00:00", tz = "UTC")
timepoint2 <- as.POSIXct("2021-09-18 17:00:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 17:00:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 17:00:00", tz = "UTC")
timepoint5 <- as.POSIXct("2021-09-21 17:00:00", tz = "UTC")
timepoint6 <- as.POSIXct("2021-09-22 17:00:00", tz = "UTC")
timepoint7 <- as.POSIXct("2021-09-23 17:00:00", tz = "UTC")

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4, timepoint5,
             timepoint6, timepoint7)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  counter = counter + val
}

# Average Chloro ug/L at the same day-time sonde-measured
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg
# -> add 1.372195 on every observation of B3P0

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

B3P0 <- sub

# Step 8: Pond ERL85 -----------------------------------------------------------
# Treatment: No Fish
# Unfortunately, I couldn't find any chemically measured chlorophyll values in
# 2021, so I can't do an intercept correction for this pond

# Step 9: Pond B2P4 ------------------------------------------------------------
# Treatment: No Fish

# Subset
sub<-subset(all,Pond=="B2P4")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="B2P4"] # "2021-09-15 19:13:09 UTC"

# Value measured chemically:
measured <- chloro$Chla_ugL[chloro$Pond=="B2P4"] # 3.155488

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-time of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 19:13:00", tz = "UTC")
timepoint2 <- as.POSIXct("2021-09-18 19:13:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 19:13:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 19:13:00", tz = "UTC")
timepoint5 <- as.POSIXct("2021-09-21 19:13:00", tz = "UTC")
timepoint6 <- as.POSIXct("2021-09-22 19:13:00", tz = "UTC")
timepoint7 <- as.POSIXct("2021-09-23 19:13:00", tz = "UTC")

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4, timepoint5,
             timepoint6, timepoint7)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  counter = counter + val
}

# Average Chloro ug/L at the same day-time sonde-measured
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg
# -> add 1.706916 on every observation of B2P4

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

B2P4 <- sub

# Step 10: Pond ERL152 ----------------------------------------------------------
# Treatment: No Fish

# Subset
sub<-subset(all,Pond=="ERL152")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="ERL152"] # "2021-09-16 14:41:15 UTC"

# Value measured chemically:
measured <- chloro$Chla_ugL[chloro$Pond=="ERL152"] # 0.7317073

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-time of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 14:42:00", tz = "UTC")
timepoint2 <- as.POSIXct("2021-09-18 14:42:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 14:42:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 14:42:00", tz = "UTC")
timepoint5 <- as.POSIXct("2021-09-21 14:42:00", tz = "UTC")
timepoint6 <- as.POSIXct("2021-09-22 14:42:00", tz = "UTC")
timepoint7 <- as.POSIXct("2021-09-23 14:42:00", tz = "UTC")

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4, timepoint5,
             timepoint6, timepoint7)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  counter = counter + val
}

# Average Chloro ug/L at the same day-time sonde-measured
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg
# -> add 0.2745645 on every observation of ERL152

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times to each observation
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

ERL152 <- sub

# Step 11: Pond ERL122 ----------------------------------------------------------
# Treatment: No Fish

# Subset
sub<-subset(all,Pond=="ERL122")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="ERL122"] # "2021-09-16 19:23:29 UTC"

# Value measured chemically:
measured <- chloro$Chla_ugL[chloro$Pond=="ERL122"] # 1.587398

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-time of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 19:23:00", tz = "UTC")
timepoint2 <- as.POSIXct("2021-09-18 19:24:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 19:24:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 19:24:00", tz = "UTC")
timepoint5 <- as.POSIXct("2021-09-21 19:24:00", tz = "UTC")
timepoint6 <- as.POSIXct("2021-09-22 19:24:00", tz = "UTC")
timepoint7 <- as.POSIXct("2021-09-23 19:24:00", tz = "UTC")

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4, timepoint5,
             timepoint6, timepoint7)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  print(val)
  counter = counter + val
}

# Average Chloro ug/L at the same day-time sonde-measured
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg
# -> subtract 0.1754588 on every observation of ERL122

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times to each observation
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

ERL122 <- sub

# Step 12: Pond B1P4 ------------------------------------------------------------
# Treatment: No Fish

# Subset
sub<-subset(all,Pond=="B1P4")

# Time point of chemically analyzed measurement:
chloro$Date_time[chloro$Pond=="B1P4"] # "2021-09-11 13:34:12 UTC"

# Value measured chemically:
measured <- chloro$Chla_ugL[chloro$Pond=="B1P4"] # 4.283537

# Same timepoint of the sondes-measurement:
# Here, the same timepoint is not given. But I will take the average
# of the same day-time of this sonde as a reference:
timepoint1 <- as.POSIXct("2021-09-17 13:35:00", tz = "UTC")
timepoint2 <- as.POSIXct("2021-09-18 13:35:00", tz = "UTC")
timepoint3 <- as.POSIXct("2021-09-19 13:35:00", tz = "UTC")
timepoint4 <- as.POSIXct("2021-09-20 13:35:00", tz = "UTC")
timepoint5 <- as.POSIXct("2021-09-21 13:35:00", tz = "UTC")
timepoint6 <- as.POSIXct("2021-09-22 13:35:00", tz = "UTC")

timeseq <- c(timepoint1, timepoint2, timepoint3,  timepoint4, timepoint5,
             timepoint6)
counter <- 0

for (i in timeseq){
  val <- sub$Chlorophyll_ugL[sub$Date_time==i]
  print(val)
  counter = counter + val
}

# Average Chloro ug/L at the same day-time sonde-measured
avrg <- counter/length(timeseq)

# Difference chemically measured and sonde-measured
diff <- measured - avrg
# -> add 3.456394 on every observation of B1P4

# Add the difference of chemically measured and the average of measured by sonde
# at at the same day-times to each observation
sub$Chlorophyll_ugL <- sub$Chlorophyll_ugL + diff

B1P4 <- sub

# Step 13: Merge all the intercept-corrected individual ponds to the whole dataframe again ----

# Read in pond that couldn't be corrected
ERL85<-subset(all,Pond=="ERL85")

# Merge
all_corr <- rbind(B2P3, B2P2, B1P1, B3P3, B3P1, B3P2, B3P0, ERL85, B2P4, 
                  ERL152, ERL122, B1P4)

# Save
fwrite(all_corr, "~/ZIVI_EAWAG/project_21/data/ponds_sonde_data_all_chloro_inter_corr.txt",
       sep = "\t")

# Step 14: Control plots -------------------------------------------------------
# We expect the Chlorophyll_ugL measurements of the intercept-corrected dataframe to be
# more closely together (within the same treatment) compared to the non-corrected
# dataframe.

library(cowplot)


# Plot of the non-corrected dataframe (old dataframe) --------------------------

all$Date_time <- as.POSIXct((all$Time_seq*86400), origin="2021-09-16 20:00:00", tz="UTC")
all_corr$Date_time <- as.POSIXct((all_corr$Time_seq*86400), origin="2021-09-16 20:00:00", tz="UTC")

p1 = ggplot(subset(all, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish: uncorrected data")

p2 = ggplot(subset(all, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B3P0"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="B1P4"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL152"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'B1P4', "ERL152", "ERL85", "ERL122"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'B1P4'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate", "ERL122"="midnightblue")) +
  ggtitle("Fishless reference: uncorrected data")

plot_grid(p1, p2, nrow = 2, rel_heights =  c(0.229,0.2,0.2,0.32))

# Plot of the intercept-corrected dataframe ------------------------------------

p3 = ggplot(subset(all_corr, Pond=="B1P1")) + theme_light() +
  geom_line(aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all_corr, Pond=="B2P2"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all_corr, Pond=="B3P3"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all_corr, Pond=="B2P3"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all_corr, Pond=="B3P2"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all_corr, Pond=="B3P1"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B1P1', 'B2P2', 'B3P3', "B2P3", "B3P1", "B3P2"),
                     values=c('B1P1'="gray17", 'B2P2'='dodgerblue3', 'B3P3'='magenta4',
                              "B2P3"="olivedrab4", "B3P1"="chocolate", "B3P2"="midnightblue")) +
  ggtitle("Fish: intercept-corrected data")

p4 = ggplot(subset(all_corr, Pond=="B2P4")) + theme_light() +
  geom_line(aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all_corr, Pond=="B3P0"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all_corr, Pond=="B1P4"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all_corr, Pond=="ERL152"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all_corr, Pond=="ERL85"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  geom_line(data = subset(all_corr, Pond=="ERL122"), aes(y=Chlorophyll_ugL, x=Date_time, color = Pond)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_color_manual(breaks=c('B2P4', 'B3P0', 'B1P4', "ERL152", "ERL85", "ERL122"),
                     values=c('B2P4'="gray17", 'B3P0'='dodgerblue3', 'B1P4'='magenta4',
                              "ERL152"="olivedrab4", "ERL85"="chocolate", "ERL122"="midnightblue")) +
  ggtitle("Fishless reference: intercept-corrected data")

plot_grid(p3, p4, nrow = 2, rel_heights =  c(0.229,0.2,0.2,0.32))

fig <- plot_grid(p1, p2, p3, p4, nrow = 4)

# Save the plot as .png
require(grDevices)
png("~/ZIVI_EAWAG/project_21/results_analysis_chlorophyll_2021/control_plot_intercept_corr.png", width = 1200, height = 1000)
print(fig)
dev.off()









































