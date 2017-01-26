# This is a script to determine for each dive the number of fish captured, sampled and recatpured and the same numbers at each site, also the number of dives per site.
library(RSQLite)
suppressMessages(library(dplyr))

# open the database
local <- dbConnect(SQLite(), "../../local_leyte.sqlite3")

# dbListTables(local) # show tables


# Fish per dive -----------------------------------------------------------
fish <- dbReadTable(local, "clownfish")
divetot <- fish %>% group_by(DiveNum) %>% summarize(count = n())
names(divetot) <- c("DiveNum", "observed")

# how many tissue samples were collected?  
tissue <- fish %>% filter(!is.na(ID)) %>% group_by(DiveNum) %>% summarize(count=n())
names(tissue) <- c("DiveNum", "tissue")
divetot <- left_join(divetot, tissue, by = "DiveNum")

# how many fish were captured
cap <- fish %>% filter(!is.na(ID) | (!is.na(Tag))) %>% group_by(DiveNum) %>% summarize(count=n())
names(cap) <- c("DiveNum", "cap")
divetot <- left_join(divetot, cap, by = "DiveNum")

# how many fish were recaptures?
recap <- fish %>% filter(Recap == "Y") %>% group_by(DiveNum) %>% summarize(count=n())
names(recap) <- c("DiveNum", "recap")
divetot <- left_join(divetot, recap, by = "DiveNum")


# Fish per site -----------------------------------------------------------

dive <- dbReadTable(local, "diveinfo")
dive <- dive %>% select(DiveNum, Name)

fish <- dbReadTable(local, "clownfish")
fish <- left_join(fish, dive, by = "DiveNum")
sitetot <- fish %>% group_by(Name) %>% summarize(count = n())
names(sitetot) <- c("Name", "observed")

# how many tissue samples were collected?  
tissue <- fish %>% filter(!is.na(ID)) %>% group_by(Name) %>% summarize(count=n())
names(tissue) <- c("Name", "tissue")
sitetot <- left_join(sitetot, tissue, by = "Name")

# how many fish were captured
cap <- fish %>% filter(!is.na(ID) | (!is.na(Tag))) %>% group_by(Name) %>% summarize(count=n())
names(cap) <- c("Name", "cap")
sitetot <- left_join(sitetot, cap, by = "Name")

# how many fish were recaptures?
recap <- fish %>% filter(Recap == "Y") %>% group_by(Name) %>% summarize(count=n())
names(recap) <- c("Name", "recap")
sitetot <- left_join(sitetot, recap, by = "Name")

