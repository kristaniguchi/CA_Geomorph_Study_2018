# Code will analyze a set of surveyed width and depth measurements
# and calculate attributes bf.w, bf.d, bf.w.d, CV_bf.w, CV_bf.d
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

# Initialize output vectors
site.id <- vector(mode='character', length=length(global.id))
srvy.lngth <- vector(mode='numeric', length=length(global.id))
bf.d <- vector(mode='numeric', length=length(global.id))
bf.w <- vector(mode='numeric', length=length(global.id))
bf.w.d <- vector(mode='numeric', length=length(global.id))
CV_bf.d <- vector(mode='numeric', length=length(global.id))
CV_bf.w <- vector(mode='numeric', length=length(global.id))
split.prop <- vector(mode='numeric', length=length(global.id))


# Loop to calculate bf.w, bf.d, bf.w.d, CV_bf.w, CV_bf.d for each site

for (i in 1:length(global.id)) {

  # Subset transect data for site i (global id corresponds to parentglobalid in transect.data)
  i_transect.data <- transect.data[as.character(transect.data$station)==global.id[i],]
  
  # Subset site general info for site i (row with site info)
  i_site.info <- site.info[i,]
  
  # Identify the Site ID
  site.id[i] <- as.character(i_site.info$station)
  
  # Identify total survey reach length
  srvy.lngth[i] <- i_site.info$reachlen_m
  
  # Collect bankfull depth and width measurements, calc w:d ratio
  d.bf <- as.numeric(as.character(i_transect.data$t_channeldepth))
  w.bf <- as.numeric(as.character(i_transect.data$t_leftthalweg)) + as.numeric(as.character(i_transect.data$t_rightthalweg))
  w.d.bf <- w.bf/d.bf
  
  
  # Which columns are split
  split.cols1 <- which(grepl("SII", i_transect.data$t_channeltype)) #includes splitII to split III
  split.cols2 <- which(grepl("SIV", i_transect.data$t_channeltype)) #includes splitIV
  split.cols <- c(split.cols1, split.cols2)
  # Proportion of cross-sections with split channel
  split.prop[i] <- length(split.cols) / length(d.bf)
  
  # Bankfull width and depth measurements
  for (j in 1:length(i_transect.data$t_channeldepth)) {
    j_transect.data <- i_transect.data[j,]
    # First identify if transect is split (if so, we want to use mean bfd and total sum of split widths)
    if ((j %in% split.cols) == TRUE) {
      # Bankfull Depth (use mean bfd)
      j_bfd.split0 <- as.numeric(c(j_transect.data$t_channeldepth, j_transect.data$t_channeldepth_sii,j_transect.data$t_channeldepth_siii,j_transect.data$t_channeldepth_siv))
      j_bfd.split <- j_bfd.split0[j_bfd.split0>0] #only use the values>0 (some default 0, we don't want to include in mean calc)
      d.bf[j] <- mean(j_bfd.split, na.rm = TRUE) #mean value, remove NA
      
      # Bankfull Width (use sum of widths)
      j_bfw = as.numeric(as.character(j_transect.data$t_leftthalweg)) + as.numeric(as.character(j_transect.data$t_rightthalweg))
      j_bfw.split <- as.numeric(c(j_bfw, j_transect.data$t_bankfullwidth_sii,j_transect.data$t_bankfullwidth_siii,j_transect.data$t_bankfullwidth_siv))
      w.bf[j] <- sum(j_bfw.split, na.rm = TRUE)
      
      # Calculate bankfull width-depth ratio for each transect (use mean of w:d from each split channel)
      w.d.bf[j] <- w.bf[j]/d.bf[j]
      options(warn=2)
      
    } else {
      # If channel is not split calculate the same metrics as above
      d.bf[j] <- as.numeric(d.bf[j])
      w.bf[j] <- as.numeric(w.bf[j])
      w.d.bf[j] <- as.numeric(w.d.bf[j])
    }
  }
  
  # Calculate bankfull metrics by compiling transect data into site data
  bf.d[i] <- mean(na.omit(d.bf))
  bf.w[i] <- mean(na.omit(w.bf))
  bf.w.d[i] <- mean(na.omit(w.d.bf))
  CV_bf.d[i] <- sd(na.omit(d.bf)) / mean(na.omit(d.bf))
  CV_bf.w[i] <- sd(na.omit(w.bf)) / mean(na.omit(w.bf))
  
}
  
# Compile data from each site into one data.frame
att_df <- data.frame(site.id, srvy.lngth, split.prop, bf.d, 
                     bf.w, bf.w.d, CV_bf.d, CV_bf.w)
print(att_df)


write.csv(att_df, file="P:/KrisTaniguchi/FromAbel/CAGC_Final/Cleaned_data/R/OutputData/SouthCoast_Y1_field_summary_channel_20190111.csv", row.names=FALSE)

