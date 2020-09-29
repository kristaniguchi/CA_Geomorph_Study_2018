# Code will analyze a set of surveyed slope measurements (longitudinal)
# calculate the water surface elevation slope (wse) or long. slope of the bed based on thalweg (in dry streams)
# Must have the package called 'openxlsx'
## Updated by Kris Taniguchi-Quan, SCCWRP 2018; original script written by Colin Byrne, UC Davis Postdoc, 2018
# Code written for the South Coast Y1 survey data analysis, Survey123 Data App CAGCv4 output table as input to this script

rm(list = ls())


############### ******INPUTS******* ###################
# Location of raw data files
root_dir <- "P:/KrisTaniguchi/FromAbel/CAGC_Final/Cleaned_data/R/InputData/"
file_names <- list.files(path=root_dir) #should only be one output table from field app
file_num <- length(file_names)

# Any number you want to be the datum for the survey
datum <- 100

############### *******Code******** #####################

# Read in the raw output tables from field app, saved as csv
site.info <- read.csv(paste(root_dir, "CAGC_Merge_Final_01102019.csv", sep="")) #table with general site info including long slope
  site.info[is.na(site.info)] <- "" #convert blank NA values as blank
#transect.data <- read.csv(paste(root_dir, "transect_repeat_1.csv", sep="")) #table with transect data

# Get unique global ID for each site
global.id <- as.character(unique(site.info$station)) #global.id corresponds to station in transect.data


# Slope Calculation Code ---------------------------------------------------

# Initialize output vectors
# hyd.class <- vector(mode='character', length=file_num)
# site_id_split <- vector(mode='character', length=file_num)
site.id <- vector(mode='character', length=length(global.id))
site_slope <- vector(mode='numeric', length=length(global.id))

for (i in 1:length(global.id)) {
  
  # Subset site general info for site i (row with site info including slope)
  i_site.info <- site.info[i,]
  
  # Identify the Site ID
  site.id[i] <- as.character(i_site.info$station)
 
  # Identify the transect spacing
  ts.ind <- grep("trans_spacing_m", names(i_site.info)) #index for transect spacing
  ts <- as.numeric(i_site.info[,ts.ind])
  
  # Identify the starting transect
  start.trans.ind <- grep("longslope_startingT", names(i_site.info))
  st_trans <- as.character(i_site.info[,start.trans.ind]) #t1 or t10 starting?
  st_num <- as.numeric(gsub("t","", st_trans)) #starting transect number
  
  # If no starting transect indicated, slope is NA
  if (is.na(st_num) == TRUE) {
    site_slope[i] <- NA
    print("Starting transect not indicated, slope saved as NA")
    
  } else { 
    
    # Identify distance upstream or downstream of finishing transect
    dist_col.t1 <- grep("t1_distRC_m", names(i_site.info))
    dist_col.t10 <- grep("t10_distRC_m", names(i_site.info))
    if (st_num == 1) {
      dist_updown <- as.numeric(i_site.info[,dist_col.t10])
      t1.10start_yn <- 0
    } else if (st_num == 10) {
      dist_updown <- as.numeric(i_site.info[,dist_col.t1])
      t1.10start_yn <- 0
    }
    
    if (is.na(dist_updown) == TRUE) {
      t1.10start_yn <- 1
      print("Distance upstream or downstream not captured OR T1/10 not end")
    }

    # Identify the columns of interest for Surveyor's level reading ***If level = 0 then omit it!
    slr_cols <- grep("surveylvl_m", names(i_site.info))
    
    # Identify the columns of interest for water depth at thalweg
    td_cols <- grep("waterdepth_m", names(i_site.info))
    
    # Identify the columns of interest for backsight reading
    bs_cols <- grep("backsight_m", names(i_site.info))
    
    # Retrieve data for level reading, water depth, and backsights
    slr.raw <- as.numeric(i_site.info[, slr_cols])
    td.raw <- as.numeric(i_site.info[, td_cols])
    bs.raw <- as.numeric(i_site.info[, bs_cols])
    
    # Determine which transects have level readings
    t.surv <- which(!is.na(slr.raw), arr.ind = TRUE)
    # Determine number of transects in longitudinal survey
    t.num <- length(t.surv)
    
    # Compile distance, level reading, water depth, and backsight in dataframe
    transect <- as.data.frame(t.surv * ts - ts, row.names = as.character(t.surv))
    colnames(transect) <- "distance"
    transect$level <- slr.raw[t.surv]
    transect$depth <- td.raw[t.surv]
    transect$backsight <- bs.raw[t.surv]
    
    # Adjust finishing transect distance
    if (st_num == 1 & t1.10start_yn == 0) {transect$distance[t.num] = 
      transect$distance[t.num] + dist_updown
    } else if (st_num == 10 & t1.10start_yn == 0) {
      transect$distance[2:t.num] = 
        transect$distance[2:t.num] - dist_updown
    } 
    
    # Adjust any dry stream bed depths from NA to 0 (if they exist)
    transect$depth[is.na(transect$depth)==TRUE] <- 0
    
    # Calculate elevation adjustment based on backsights
    transect$bed.z <- 0
    transect$wse <- 0
    # Adjustment for surveys conducted transect 1 to 10
    if (st_num == 1) {
      for (j in 1:t.num) {
        # Calculate bed elevation
        if (j == 1) {
          transect$bed.z[j] <- datum - transect$level[j]
        } else if (j!=1 & is.na(transect$backsight[j-1]) == FALSE) {
          transect$bed.z[j] <-  transect$bed.z[j-1] - 
            (transect$level[j] - transect$backsight[j-1])
        } else {
          transect$bed.z[j] <- transect$bed.z[j-1] - 
            (transect$level[j] - transect$level[j-1])
        }
        # Calculate water surface elevation
        transect$wse[j] <- transect$bed.z[j] + transect$depth[j]
      }
    } else if (st_num == 10) { # Adjustment for surveys conducted transect 10 to 1
      for (j in seq(from = t.num, to = 1, by = -1)) { 
        if (j == max(seq(from = t.num, to = 1, by = -1))) { 
          transect$bed.z[j] <- datum - transect$level[j]
        } else if (j != max(seq(from = t.num)) & 
                   is.na(transect$backsight[j+1]) == FALSE) {
          transect$bed.z[j] <-  transect$bed.z[j+1] - 
            (transect$level[j] - transect$backsight[j+1])
        } else {
          transect$bed.z[j] <- transect$bed.z[j+1] - 
            (transect$level[j] - transect$level[j+1])
        }
        transect$wse[j] <- transect$bed.z[j] + transect$depth[j]
      }
    }
    
    # Use a linear regression model to fit a line to the distance-WSE coordinates
    linear_model <- lm(wse ~ distance, transect)
    site_slope[i] <- coefficients(linear_model)[2] * -1
    # ## Optional print and plots to see slopes of at each file
    # print(site.id[i])
    # print(transect)
    # print("Check if upstream datum is equal to Backsights - Foresights")
    # # Note: Backsights include all in backsight column plus the level reading 
    # # at transect 10. Foresights include all level readings with backsight
    # # measurements and level reading at transect 1
    # print(paste0("Datum: ", as.character(
    #   transect$bed.z[1] - transect$bed.z[t.num])))
    # if (length(transect$level[which(!is.na(transect$backsight),
    #                                 arr.ind = TRUE)])==0) {
    #   print(paste0("Ba-Fo: ", as.character(transect$level[t.num] - 
    #                                          transect$level[1])))
    # } else {
    #   print(paste0("Ba-Fo: ", as.character(
    #     sum(transect$backsight, na.rm = TRUE) + transect$level[1] - 
    #       transect$level[t.num] - 
    #       sum(transect$level[which(!is.na(transect$backsight), 
    #                                arr.ind = TRUE)]))))
    # }
    # 
    # print(paste0("Site slope: ", as.character(site_slope[i])))
    # 
    # # Plot linear model if you wish
     plot(transect$distance, transect$wse)
     abline(linear_model)
    # 
    # readline("Enter to continue.")
  }
}

out.df <- data.frame(site.id, site_slope)
print(out.df)

write.csv(out.df, file="P:/KrisTaniguchi/FromAbel/CAGC_Final/Cleaned_data/R/OutputData/SouthCoast_Y1_field_summary_slope_20190111.csv", row.names=FALSE)
