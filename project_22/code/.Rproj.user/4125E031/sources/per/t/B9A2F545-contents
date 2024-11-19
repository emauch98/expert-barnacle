################################################################################
# This is the script to process raw data of wind and irradiation
# in Narsarsuaq 2022
# The data stem from the
# QAS_L automated weather station a couple of kilometers away from Narsarsuaq
# It's the closest weather station measuring this parameter, that I could find.
# https://dataverse.geus.dk/dataset.xhtml?persistentId=doi:10.22008/FK2/IW73UU
# The data will be subsequently used to model lake metabolism
# The data are hourly averages of shortwave irradiation and wind speed
################################################################################

# Step 0: set up R-script ------------------------------------------------------
rm(list= ls())

require(data.table) # data wrangling
require(dplyr) # data wrangling

# Import dataset ---------------------------------------------------------------
meta <- fread("../../project_21/data/metadata_metabolism_analysis/QAS_L_hour.csv",
              sep = ",")

# reduce to wanted variables
meta <- meta[,c("time","dsr_cor","wspd_u")]
# downwelling shortwave radiation
# wind speed upper boom
# sensor height upper boom: 2.47 + 0.4 = ca 2.87m

# rename variables
setnames(meta, c("Date_time", "dsr_cor", "wspd_ms"))

# make sure Date_time is UTC timezone
meta$Date_time <- as.POSIXct(meta$Date_time, tz = "UTC")

# choose appropriate range
# 2022-06-24 00:00:00 to 2022-07-05 00:00:00
meta <- meta[meta$Date_time > "2022-06-23 01:00:00" &
               meta$Date_time < "2022-07-05 03:00:00",]

# save
fwrite(meta, "../data/metadata_metabolism_analysis/metadata_2022.txt", sep="\t")
