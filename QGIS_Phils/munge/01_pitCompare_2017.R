# a script to compare new pit scans with newly entered pit data
suppressMessages(library(dplyr))
library(RSQLite)
# open connection to db
local <- dbConnect(SQLite(), "../../local_leyte.sqlite3")

 # import PIT data   This section uses the package RSQLite  -----------------
  infile <- "../../PIT.txt"  # during field season
  # infile <- paste(year,"PIT.txt",sep='') # after field season
  pit <- read.csv(infile, header=FALSE)
  names(pit) <-c("city", "tagid", "date", "time")
  
  # convert date to date
  pit$date <- paste("20", pit$date, sep = "")
  
  # pull pitscan table from db
  exist <- dbReadTable(local, "pitscan")
  
  # join the 2 tables and eliminate duplicate rows
  pit <- rbind(pit, exist)
  pit <- distinct(pit)
  
  # Add new pit scans to db *** note if you want to overwrite or append *****
  dbWriteTable(local, "pitscan", pit, overwrite = T, append = F)
  
  
# Separate out the 6 digit tag numbers from the data ------------------------
pit$scan <- paste(pit$city, pit$tagid, sep = "")

  # Import excel survey data --------------------------------------------
  # This would be a good place to pull data from a database instead of loading the entire excel sheet (it pulls in blank lines 1,048,575 obs)
  library(readxl)
  excelfile <- "../../GPSSurveys2017.xlsx"
  excel <- read_excel(excelfile, sheet = "Clownfish", col_names=TRUE)
  # fileList <- sort(list.files(path='output', pattern = "GPSSurvey.*"), decreasing=TRUE) # lists all extract anem output
  # excelfile <- fileList[1] # pulls out the most recent extractanem output
  # excel <- read.csv(paste("output/",excelfile, sep='')) # using this instead of read_excel because we need the date

  # Find the PIT tags in the excel data -----------------------------------
  # xcel <- excel[,c("TagID1","TagID2","TagID3", "TagID4", "Date", "ObsTime")]
  xcel <- excel[,c("TagID1","TagID2","TagID3", "TagID4", "TagID5", "TagID6", "TagID7", "DiveNum")] # including divenum so we have one row that will always have a value

  # Remove NA from the excel data -----------------------------------------
  xcel <- xcel[!is.na(xcel$DiveNum), ]

# make a list of existing tagids for all of the columns in the datasheet
  xcl <- c(xcel$TagID1[!is.na(xcel$TagID1)], xcel$TagID2[!is.na(xcel$TagID2)], xcel$TagID3[!is.na(xcel$TagID3)], xcel$TagID4[!is.na(xcel$TagID4)], xcel$TagID5[!is.na(xcel$TagID5)], xcel$TagID6[!is.na(xcel$TagID6)], xcel$TagID7[!is.na(xcel$TagID7)])
 
# List any scanned tags that are not in the excel data ------------------
  print(setdiff(as.character(pit$scan), as.character(xcl))) # searches for values in scan that are not in xcl
  # should return numeric(0)
  # scans <- setdiff(as.numeric(pit$scan), as.numeric(xcl))
  
  # List any excel tags that are not in the scanned data ------------------
  print(setdiff(xcl, pit$scan)) #searches for values in xcl that are not in scan
  # should return list()
# mistype <- setdiff(xcl, pit$scan)
  
  
  
