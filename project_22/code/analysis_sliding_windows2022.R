# startup --------------------------------------------------------------

rm(list = ls())

# replace for your directory
setwd("~/ZIVI_EAWAG/project_22/code")

source("~/ZIVI_EAWAG/project_22/Moritz_Lührig_paper_stuff/methods_packages.R")

cols = c("Chlorophyll_RFU", "BGAPC_RFU", "ODO_sat", "fDOM_RFU")
sonde_key = fread( "~/ZIVI_EAWAG/project_22/ponds_sonde_key.txt", header=T)

# sliding windows --------------------------------------------------

tenmin<-fread("~/ZIVI_EAWAG/project_22/data/ponds_sonde_data_ten_min_avg.txt")

data = tenmin[order(Pond, Time_seq, Hour, Ten_min), ]

# What window size should I choose here?
winsize = 2 # 2 hours

mean.df = data[, lapply(.SD, rollapplyr, width=winsize, fill=NA, FUN=mean, na.rm=T), 
               by=c("Pond", "Replicate","Treatment"), .SDcols = cols]
mean.df$Time_seq = data$Time_seq
mean.df$Hour = data$Hour
mean.df$Ten_min = data$Ten_min
mean.df[,(cols):=lapply(.SD, round, 2), .SDcols=cols]
mean.df$Variable = "MEAN"
fwrite(mean.df, "~/ZIVI_EAWAG/project_22/data/sliding_ten_min_mean_2h.txt")


cv.df = data[, lapply(.SD, rollapplyr, width=winsize, fill=NA, FUN=CV), 
             by=c("Pond", "Replicate","Treatment"), .SDcols = cols]
cv.df$Time_seq = data$Time_seq
cv.df$Hour = data$Hour
cv.df$Ten_min = data$Ten_min
cv.df[,(cols):=lapply(.SD, round, 2), .SDcols=cols]
cv.df$Variable = "CV"
fwrite(cv.df, "~/ZIVI_EAWAG/project_22/data/sliding_ten_min_cv_2h.txt")

ac.df = data[, lapply(.SD, rollapplyr, width=winsize, fill=NA, FUN=AC), 
             by=c("Pond", "Replicate","Treatment"), .SDcols = cols]
ac.df$Time_seq = data$Time_seq
ac.df$Hour = data$Hour
ac.df$Ten_min = data$Ten_min
ac.df[,(cols):=lapply(.SD, round, 2), .SDcols=cols]
ac.df$Variable = "AC"
fwrite(ac.df, "~/ZIVI_EAWAG/project_22/data/sliding_ten_min_ac_2h.txt")


