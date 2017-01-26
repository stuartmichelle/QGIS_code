# We "munge" the daily data in order to create the daily maps, pit comparison, and dive summaries.  At the end of the field season, we need one final munge to export the data into a field database and into the sample database.

# Create the project initially
# setwd("~/Documents/Philippines/Surveys_2016/")
# library(ProjectTemplate)
# create.project("QGIS_Phils")

# BEFORE RUNNING THIS, IMPORT GPX AND ENTER DATA FOR THE DAY



setwd("~/Documents/Philippines/Surveys_2016/code/QGIS_Phils/")
library(ProjectTemplate)
load.project()

# The first munge script compares entered PIT tags to scanned PIT tags
#  The result should be
# numeric(0)
# list()
# If it is not, open the GPSSurveys excel file and find the issue by comparing entered data to scanned data

#The second munge trims gpx files
#The third munge concatenates the tracks for extracting anem data
# the Fourth munge extracts anem data
# the fifth munge creates a dive summary file
# the sixth munge extracts collections
