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

# list all views
query_views <- dbGetQuery(con,"SELECT viewname from pg_catalog.pg_views")

# extract view names related to 'view_configuration'
view_configurations <- grep("view_configuration",query_views$viewname)
view_configurations <- query_views[view_configurations,]

# read view_configurations
# for sake of convenience, use only substring as name without 'view_configuration_' prefix
for (i in 1:length(view_configurations)) {
  assign(paste(gsub("view_configuration_","", view_configurations[i])), dbReadTable(con, c("config_data",view_configurations[i])))
}

dbDisconnect(con)