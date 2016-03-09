## Access HU Database ##
library(R.cache)
library(RPostgreSQL) 

setCacheRootPath(path="/vagrant")
dbpass <- loadCache(list("pass", "", ""))

# Connect to Database
dbname <- "polconfdb"
dbuser <- "polconfdb"
dbhost <- "moodledb.cms.hu-berlin.de"
dbport <- "5432"

drv <- dbDriver("PostgreSQL") 
con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass) 

dbGetInfo(drv)
summary(con)

tables <- dbListTables(con)

# do tables exist?
for (i in 1:length(tables)) {
  print(dbExistsTable(con, c("config_data",tables[i])))
}

# read tables
for (i in 1:length(tables)) {
  assign(paste(tables[i]), dbReadTable(con, c("config_data",tables[i])))
}

dbDisconnect(con)