# Run extractAnem first to make input file --------------------------------

  fileList <- sort(list.files(path="../../output", pattern = "GPSSurvey.*"), decreasing=FALSE) # lists all extract anem output
  infile <- paste("../../output/",fileList[2], sep="") # chooses the most recent file in output not for QGIS
  anem <- read.csv(infile, row.names = 1)
  # what is the max column number category for fish (used to be 7, in 2016 was 11)
  m <- 11
 
# Find the largest pair of fish on each anemone
	anem$Rank1 <- NA # column of the largest individual (ID1 through ID6)
	anem$Rank2 <- NA

	k <- which(!is.na(anem$Size1)) # anemones with fish
	for(i in k){
		j <- sort(c(anem$Size1[i],anem$Size2[i],anem$Size3[i],anem$Size4[i],anem$Size5[i],anem$Size6[i], anem$Size7[i], anem$Size8[i], anem$Size9[i], anem$Size10[i], anem$Size11[i]), index.return=T, decreasing=T)$ix
		anem$Rank1[i] <- j[1]
		anem$Rank2[i] <- j[2]
	}


	collections <- data.frame(Size = numeric(0), Col = character(0),  Recap = character(0), Tag = numeric(0), ID = character(0), Date = character(0), Spp = character(0), Notes=character(0), lat = numeric(0), lon = numeric(0), Name = character(0), AnemID = numeric(0), Collector = character(0), DiveNum = numeric(0), TopTwo = logical(0))
	

	names <- names(collections)

	for (i in 1:m){
	  fishcols <- colnames(anem)[grep(i, colnames(anem))]
	  fishcols <- fishcols[1:5] # limit to the first 5 columns (don't need Rank1/2 or Spp2 in list)
	  y <- which(names(anem) == fishcols[1])
	  k <- which(anem[, y] != "")
	  if (length(k) > 0){
	    x <- subset(anem[k, ], select=c(fishcols))
	    x <- cbind(x, subset(anem[k, ], select = c("Date", "Spp", "Notes", "lat", "lon", "Name", "AnemID", "Collector", "DiveNum")))
	    x$TopTwo <- anem$Rank1[k] == i | anem$Rank2[k] == i # is this one of the two largest on the anemone?
	    names(x) <- names
	   collections <- rbind(collections, x)
	    dim(collections)
	  }
	  
	}
	


	collections$Notes <- as.character(collections$Notes)


	# Sort the data
	permut <- order(collections$ID)
	collections <- collections[permut,]
	row.names(collections) <- 1:nrow(collections)

	# Examine the data
	head(collections[,c("Date", "Spp", "lat", "lon")])


	# Write out collection data
	write.csv(collections, file=paste("../../output/Collections2017", Sys.Date(), ".csv", sep=""), row.names=FALSE)

	# send the data to the local db
	# open connection to db
	local <- dbConnect(SQLite(), "../../local_leyte.sqlite3")
	
	#prep fish data 
	fish <- collections[ , c("DiveNum", "Spp", "Size", "ID", "Col", "Recap", "Tag", "Notes", "Collector")]
	fish$fish_table_id <- 1:nrow(fish)
	
	# pull anem tbl from db
	exist <- dbReadTable(local, "clownfish")
	
	# join the 2 tables and eliminate duplicate rows
	fish <- rbind(fish, exist)
	fish <- distinct(fish)
	# Add new info to db *** note if you want to overwrite or append *****
	dbWriteTable(local, "clownfish", fish, overwrite = T, append = F)

