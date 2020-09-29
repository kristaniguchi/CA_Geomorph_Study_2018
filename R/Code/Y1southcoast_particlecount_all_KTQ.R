#Extract particle size information for each site

############### ******INPUTS******* ###################
# Location of raw data files
root_dir <- "C:/Users/KristineT/Documents/Git/CA_Geomorph_Study_2018/R/InputData/"
file_names <- list.files(path=root_dir) #should only be one output table from field app
file_num <- length(file_names)

############### *******Code******** #####################

# Read in the raw output tables from field app, saved as csv
site.info <- read.csv(paste(root_dir, "CAGC_Merge_Final_01102019.csv", sep="")) #table with general site info
transect.data <- read.csv(paste(root_dir, "Transect_Repeat_Merge_Final_01102019.csv", sep="")) #table with transect data

# Get unique global ID for each site
global.id <- as.character(unique(site.info$station)) #global.id corresponds to station in transect.data

# Sediment Calculation Code ---------------------------------------------------

# Particle size data
# Get column index for sediment count start and end
col.names <- names(transect.data) #col header names
sed.start.col <- grep("tps_lessthan2mm", col.names)
sed.end.col <- grep("tps_bedrock", col.names)
n.size.classes <- 17 #17 total size classes

# Initialize output dataframe
particle.size.df <- data.frame(matrix(NA, length(global.id), 19))
names(particle.size.df) <- c("site.id", "bio.site", col.names[sed.start.col:sed.end.col])


for (i in 1:length(global.id)) {
  
  # Subset transect data for site i (global id corresponds to parentglobalid in transect.data)
  i_transect.data <- transect.data[as.character(transect.data$station)==global.id[i],]
  
  # Subset site general info for site i (row with site info)
  i_site.info <- site.info[i,]
  
  # Identify the Site ID
  site.id <- as.character(i_site.info$station)
  bio.site <- as.character(i_site.info$bioassessment_site)
  
  # Particle size data
  # Get column index for sediment count start and end
  sed.subset <- i_transect.data[,sed.start.col:sed.end.col] #subset only the 
  n.size.classes <- 17 #17 total size classes
  
  #sum each column to get total particles in each class
  particle.size.df[i,3:19] <-  colSums(sed.subset)
  
  #save site id and bio site in output df
  particle.size.df[i,1] <- site.id
  particle.size.df[i,2] <- bio.site
  
}



#add bioassessment_site name
write.csv(particle.size.df, file="C:/Users/KristineT/Documents/Git/CA_Geomorph_Study_2018/R/OutputData/SouthCoast_Y1_field_particlesizecount_all.csv", row.names=FALSE)

#write just 3 sites need for SD Creek model

