# Code will analyze a set of surveyed sediment measurements 
# calculate the D50, D84, and Dmax
# Must have the package called 'openxlsx'
## Updated by Kris Taniguchi-Quan, SCCWRP 2018; original script written by Colin Byrne, UC Davis Postdoc, 2018
# Code written for the South Coast Y1 survey data analysis, Survey123 Data App CAGCv4 output table as input to this script

rm(list = ls())


############### ******INPUTS******* ###################
# Location of raw data files
root_dir <- "P:/KrisTaniguchi/FromAbel/CAGC_Final/Cleaned_data/R/InputData/"
file_names <- list.files(path=root_dir) #should only be one output table from field app
file_num <- length(file_names)

############### *******Code******** #####################

# Read in the raw output tables from field app, saved as csv
site.info <- read.csv(paste(root_dir, "CAGC_Merge_Final_01102019.csv", sep="")) #table with general site info
transect.data <- read.csv(paste(root_dir, "Transect_Repeat_Merge_Final_01102019.csv", sep="")) #table with transect data

# Get unique global ID for each site
global.id <- as.character(unique(site.info$station)) #global.id corresponds to station in transect.data

# Sediment Calculation Code ---------------------------------------------------
# Initialize output vectors
site.id <- vector(mode='character', length=length(global.id))
d50 <- vector(mode='numeric', length=length(global.id))
d84 <- vector(mode='numeric', length=length(global.id))
dmax <- vector(mode='numeric', length=length(global.id))


for (i in 1:length(global.id)) {
  
  # Subset transect data for site i (global id corresponds to parentglobalid in transect.data)
  i_transect.data <- transect.data[as.character(transect.data$station)==global.id[i],]
  
  # Subset site general info for site i (row with site info)
  i_site.info <- site.info[i,]
  
  # Identify the Site ID
  site.id[i] <- as.character(i_site.info$station)
  
  # Particle size data
  # Get column index for sediment count start and end
  col.names <- names(i_transect.data) #col header names
  sed.start.col <- grep("tps_lessthan2mm", col.names)
  sed.end.col <- grep("tps_bedrock", col.names)
  sed.subset <- i_transect.data[,sed.start.col:sed.end.col] #subset only the 
  n.size.classes <- 17 #17 total size classes
  
  site_samp <- 0
  # Loop to sum up n number of particles in each size class, then output as list
  for (j in 1:n.size.classes){
    j.size.class <- sed.subset[,j]
    #sum of total particles in j size class
    j.size.class.sum <- sum(j.size.class)
    

    # Assign the correct sediment size
    if (j == 1) {
      samp_size <- 2
    } else if (j == 2) {
      samp_size <- 2.8
    } else if (j == 3) {
      samp_size <- 4
    } else if (j == 4) {
      samp_size <- 5.6
    } else if (j == 5) {
      samp_size <- 8
    } else if (j == 6) {
      samp_size <- 11
    } else if (j == 7) {
      samp_size <- 16
    } else if (j == 8) {
      samp_size <- 22.6
    } else if (j == 9) {
      samp_size <- 32
    } else if (j == 10) {
      samp_size <- 45
    } else if (j == 11) {
      samp_size <- 64
    } else if (j == 12) {
      samp_size <- 90
    } else if (j == 13) {
      samp_size <- 128
    } else if (j == 14) {
      samp_size <- 190
    } else if (j == 15) {
      samp_size <- 1000
    } else if (j == 16) {
      samp_size <- 2000 
    } else if (j == 17) {
      samp_size <- 5000
    } else {samp_size <- NA}
    
    # List the sample size values based on total count for that size
    site_samp0 <- rep(samp_size, length=j.size.class.sum)
    site_samp <- c(site_samp, site_samp0)
    
    # Convert any 0 values to NA
    site_samp[site_samp == 0] <- NA
  }
  
  # Calculate sediment sizes
  d50[i] <- quantile(site_samp, probs=0.50, na.rm=TRUE, type=1)
  d84[i] <- quantile(site_samp, probs=0.84, na.rm=TRUE, type=1)
  dmax[i] <- max(site_samp, na.rm=TRUE)
  
}

sediment_stats <- data.frame(site.id, d50, d84, dmax)
print(sediment_stats)

write.csv(sediment_stats, file="P:/KrisTaniguchi/FromAbel/CAGC_Final/Cleaned_data/R/OutputData/SouthCoast_Y1_field_summary_sediment_20190111.csv", row.names=FALSE)

