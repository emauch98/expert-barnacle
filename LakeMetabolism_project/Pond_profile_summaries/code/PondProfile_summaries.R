################################################################################
# This is a Script to calculate some summary statistics for Ponds in Greenland
# sampled each summer.
# The cut profile datasets from 2021-2024 have already been prepared with R scripts
# beforehand.
################################################################################

# Step 0: Setup R-script--------------------------------------------------------
rm(list= ls())

require(data.table) # Advanced data wrangling
require(openxlsx) # write excel files

# Step 1: Import cut profile datasets 2021-2024---------------------------------

# 2021, 2022, 2023, and 2024
prof_21 <- fread("../../project_21/data/sondes_profiling_cut/merged_profiles_cut_2021.txt")
prof_22 <- fread("../../project_22/data/sondes_profiling_cut/merged_profiles_cut_2022.txt")
prof_23 <- fread("../../project_23/data/sondes_profiling_cut/merged_profiles_cut_2023.txt")
prof_24 <- fread("../../project_24/data/sondes_profiling_cut/merged_profiles_cut_2024.txt")

# Step 2: Write a function to calculate Pond-wise means, store into a dataset---

means_profiles <- function(data, data_name) {
  # Select columns from Depth_m (4th column) to the last
  cols <- names(data)[5:ncol(data)]
  
  # extract Year
  year <- as.numeric(paste0("20", sub(".*_", "", data_name)))
  
  # Calculate means grouped by Pond
  result <- data[, lapply(.SD, mean, na.rm = TRUE), 
                 by = .(Pond, Day), .SDcols = cols]
  
  # Column names of result dataset
  setnames(result, cols, paste0("mean_", cols))
  
  # Add the Year column
  result[, Year := year]
  
  return(result)
}

# Apply the function to datasets
result_data_21 <- means_profiles(prof_21, "prof_21")
result_data_22 <- means_profiles(prof_22, "prof_22")
result_data_23 <- means_profiles(prof_23, "prof_23")
result_data_24 <- means_profiles(prof_24, "prof_24")

# Merge
merged_results <- rbindlist(list(result_data_21, result_data_22, result_data_23,
                            result_data_24), fill = TRUE)

# Replace NaN with NA, if present
merged_results[] <- lapply(merged_results, function(x) {
  if (is.numeric(x)) x[is.nan(x)] <- NA
  return(x)
})

# reorder
merged_results <- merged_results[,c("Pond", "Year", "Day", "mean_Chlorophyll_RFU",
                                    "mean_Cond_uScm", "mean_SpCond_uScm",
                                    "mean_fDOM_RFU", "mean_ODO_mgL", "mean_ODO_sat",
                                    "mean_BGAPC_RFU", "mean_pH", "mean_Temp_C")]

# Step 3: Store the result dataset ---------------------------------------------
write.xlsx(merged_results, file = "../result_datasets/ProfileMeansPonds_21_22_23_24.xlsx")
