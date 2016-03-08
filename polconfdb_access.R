dbname <- "polconfdb"
dbuser <- "polconfdb"
dbpass <- loadCache(list("pass", "", ""))


dbhost <- "moodledb.cms.hu-berlin.de"
dbport <- "5432"

library(RPostgreSQL) 
drv <- dbDriver("PostgreSQL") 
con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass) 

dbGetInfo(drv)
summary(con)

dbListTables(con)

dbExistsTable(con,"config_data.country")
dbExistsTable(con,"lh_election")
dbListFields(con,"config_data.country")

rs <- dbSendQuery(con,"SELECT lhelc_nxt_id 
  from config_data.lh_election")
rs
fetch(rs,n=-1)


data.frame(dbReadTable(con,"SELECT * 
  from config_data.lh_election"))

dbExistsTable(con,"config_data.veto_points")

dbReadTable

dbDisconnect(con)


library(dplyr)
mydb = src_postgres(host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass) 
mydb
tbl(mydb, "cabinet")
tbl(mydb, sql("SELECT * FROM config_data.country"))

