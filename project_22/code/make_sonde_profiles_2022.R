################################################################################
# This script is meant to import, cut, store, and plot all sonde profiles
# from 2022 in Greenland.
# Please note, that it is important that the .csv files are encoded
# as UTF-8, otherwise the code will not work.
# In excel, one can save a file as UTF-8 .csv.
################################################################################

# Step 0: set up R-script ------------------------------------------------------
rm(list= ls())

# Packages ad functions from Moritz Luehrig's paper
source("../../Literature/Moritz_Luehrig_paper_stuff/methods_packages.R")
require(GGally)
require(gridExtra)
# For images
require(grDevices)

# Step 1: merge all raw data files into a single big one -----------------------
# Please ensure that all files are UTF-8 encoded (you'll get an error message
# that hints to that problem if you try run the following script)

path = "../data/sondes_profiling_raw"
filenames <- list.files(path=path, pattern=".csv")
files<-data.table(NULL)

system.time(for(i in filenames){
  filepath <- file.path(path,paste(i,sep=""))
  dummy1<-fread(filepath,
                colClasses = "character" ,
                sep = ";",
                header = FALSE,
                fill=TRUE,
                encoding ="UTF-8",
                quote = FALSE)
  
  rownumb<-dummy1[V1 %like% "(MM/DD/YYYY)", which=TRUE]+1
  colname<-dummy1[V1 %like% "(MM/DD/YYYY)"]
  colname <- iconv(as.character(colname), from = "UTF-8", to = "UTF-8//IGNORE")
  colname <- gsub("µ", "", colname)  # Remove mu character
  colname <- gsub("°", "", colname)  # Remove degree symbol
  colname<-gsub(" ", "",(gsub("[[:punct:]]", "",c(lapply(colname,as.character)))))
  colname <- make.unique(colname)  # Ensure unique column names
  colname<-unlist(strsplit(colname,"\t"))
  dummy2<-dummy1[rownumb:.N]
  setnames(dummy2,colname)
  dummy2[,(colname[c(3,5:length(dummy2))]):=lapply(.SD, as.numeric),.SDcols=colname[c(3,5:length(dummy2))]]
  dummy2$Pond<-sub(" J.*", "", i) # Extract pond
  dummy2$Source_file<-i # Extract source file
  dummy2$Day <- str_extract(i, "[A-Za-z]+ \\d{1,2}") # Extract day
  files<-rbindlist(list(files, dummy2), fill=TRUE)
})

all <- files

# select columns to keep
all <- all[,c("DateMMDDYYYY", "TimeHHMMSS", "Pond", "Day", "Depthm", "ChlorophyllRFU", "CondScm", "SpCondScm",
              "fDOMRFU", "ODOmgL", "ODOsat", "BGAPCRFU", "pH", "TempC")]

# Step 2: format time, rename columns ------------------------------------------
# format date & time
all$DateMMDDYYYY<-mdy(all$DateMMDDYYYY)
all$Date_time<-ymd_hms(paste(all$DateMMDDYYYY, all$TimeHHMMSS))
setnames(all,1:2,c("Date","Time"))

# exclude "Date" and "Time"
all <- all[,!c("Date", "Time")]

# reorder
all <- all[,c("Date_time", "Pond", "Day", "Depthm", "ChlorophyllRFU", "CondScm", "SpCondScm",
              "fDOMRFU", "ODOmgL", "ODOsat", "BGAPCRFU", "pH", "TempC")]

# rename
setnames(all, c("Date_time", "Pond", "Day", "Depth_m", "Chlorophyll_RFU", "Cond_uScm", "SpCond_uScm",
                "fDOM_RFU", "ODO_mgL", "ODO_sat", "BGAPC_RFU", "pH", "Temp_C"))

# debug: remove duplicate values
all <- unique(all, by=c("Pond", "Date_time"))

# check the variable types
str(all)

# Step 3: cut the profiles -----------------------------------------------------
# Here, I will cut the profile subsets of the individual ponds to the relevant
# range

# The current cutting is as follows:
# First, I individually selected a starting level where I cut, depending
# on the pond. Sometimes, the sondes were initially held at a deeper point
# before starting profiling.
# After cutting away everything below the threshold, I search the maximum depth
# and keep only the sequence starting from the threshold up to the maximum depth
# Then, I order the values by increasing depth, not time (sometimes, the sampling person
# might lift the sonde up a bit by accident).

# Initialize an empty list to store results for each pond
result_list <- list()

# Pick individual thresholds
thresholds <- c("B2P2"=0.75, "B2P3"=0.5, "B2P4"=0.5, "B3P0"=1, "B3P1"=0.5, "B3P2"=0.5,
"B3P3"=0.35, "B3P4"=0.6, "EFL2"=0.35, "ERL1"=0.5, "ERL124"=0.5, "ERL153"=1, "ERL17"=1,
"ERL186"=0.35, "ERL2"=0.425, "ERL20"=0.35, "ERL21"=0.35, "ERL31"=1, "ERL32"=0.35,
"ERL33"=1, "ERL5"=1, "ERL67"=1, "ERL84"=0.5, "ERL87"=0.5, "ERL88"=0.35, "ERL9"=0.35,
"L26"=0.35)

# Loop through each unique pond
for (pond in unique(all$Pond)) {
  # Subset the data for the current pond
  pond_data <- all[Pond == pond]
  
  # Get the unique depth threshold for the current pond
  threshold <- thresholds[pond]
  
  # Filter out Depth_m below the unique threshold for this pond
  pond_data <- pond_data[Depth_m >= threshold]
  
  # Find the maximum depth for the current pond
  max_depth <- max(pond_data$Depth_m, na.rm = TRUE)
  
  # Cut off all values after the maximum depth
  pond_data <- pond_data[seq_len(which.max(pond_data$Depth_m))]  # Retain up to the maximum depth
  
  # Optionally, you could reset the row names
  rownames(pond_data) <- NULL
  
  # Store the result in the list
  result_list[[pond]] <- pond_data
}

# Combine all subsets back into a single data.table if needed
all <- rbindlist(result_list)

# Order the observations by ascending depth
all <- all %>%
  arrange(Pond, Depth_m)

# save
fwrite(all, "../data/sondes_profiling_cut/merged_profiles_cut_2022.txt", sep = "\t")

# Step 4: control plots --------------------------------------------------------
# Here I wanna plot the profiles of different parameters
# x-axis: Parameter; y-axis: Depth

# Temperature
# Get unique pond names
ponds <- unique(all$Pond)

plots_TempC <- lapply(ponds, function(pond_name) {
  dat <- subset(all, Pond == pond_name)
  day <- unique(dat$Day)[1]
  ggplot(data = dat, aes(x = Temp_C, y = Depth_m)) +
    geom_point() +
    geom_path() +
    scale_y_reverse() +
    labs(title = paste("Thermocline", pond_name, ",", day, "2022"))
})

# Save the plot as a .png
png("../results_profiling/profiling_TempC_2022.png", width = 1800, height = 1400)
grid.arrange(grobs = plots_TempC, ncol = 5)
dev.off()

# Chlorophyll_RFU
plots_Chloro <- lapply(ponds, function(pond_name) {
  dat <- subset(all, Pond == pond_name)
  day <- unique(dat$Day)[1]
  ggplot(data = dat, aes(x = Chlorophyll_RFU, y = Depth_m)) +
    geom_point() +
    geom_path() +
    scale_y_reverse() +
    labs(title = paste("Chlorophyll RFU", pond_name, ",", day, "2022"))
})

# Save the plot as a .png
png("../results_profiling/profiling_ChloroRFU_2022.png", width = 1800, height = 1400)
grid.arrange(grobs = plots_Chloro, ncol = 5)
dev.off()

# fDOM RFU
plots_fDOM <- lapply(ponds, function(pond_name) {
  dat <- subset(all, Pond == pond_name)
  day <- unique(dat$Day)[1]
  ggplot(data = dat, aes(x = fDOM_RFU, y = Depth_m)) +
    geom_point() +
    geom_path() +
    scale_y_reverse() +
    labs(title = paste("fDOM RFU", pond_name, ",", day, "2022"))
})

# Save the plot as a .png
png("../results_profiling/profiling_fDOM_2022.png", width = 1800, height = 1400)
grid.arrange(grobs = plots_fDOM, ncol = 5)
dev.off()

# ODO_mgL
plots_ODO <- lapply(ponds, function(pond_name) {
  dat <- subset(all, Pond == pond_name)
  day <- unique(dat$Day)[1]
  
  ggplot(data = dat, aes(x = ODO_mgL, y = Depth_m)) +
    geom_point() +  # Plot the points
    geom_path() +  # Connect the points by row order
    scale_y_reverse() + # Reverse depth scale
    labs(title = paste("Dissolved Oxygen mg/L", pond_name, ",", day, "2022"))
})

# Save the plot as a .png
png("../results_profiling/profiling_ODO_2022.png", width = 1800, height = 1400)
grid.arrange(grobs = plots_ODO, ncol = 5)
dev.off()

# BGA-PC RFU
plots_BGAPC <- lapply(ponds, function(pond_name) {
  dat <- subset(all, Pond == pond_name)
  day <- unique(dat$Day)[1]
  ggplot(data = dat, aes(x = BGAPC_RFU, y = Depth_m)) +
    geom_point() +
    geom_path() +
    scale_y_reverse() +
    labs(title = paste("BGA-PC RFU", pond_name, ",", day, "2022"))
})

# Save the plot as a .png
png("../results_profiling/profiling_BGAPC_2022.png", width = 1600, height = 1400)
grid.arrange(grobs = plots_BGAPC, ncol = 5)
dev.off()
