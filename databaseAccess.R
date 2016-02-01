## Access HU Database ##

# Connect to Database
dbname <- "polconfdb"
dbuser <- "polconfdb"
dbpass <-  ""

dbhost <- "moodledb.cms.hu-berlin.de"
dbport <- ""

library(RPostgreSQL) 
drv <- dbDriver("PostgreSQL") 
con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass) 

dbGetInfo(drv)
summary(con)


# dbExistsTable
dbExistsTable(con, c("config_data", "cabinet"))
cabinet <- dbReadTable(con, c("config_data","cabinet"))

dbExistsTable(con, c("config_data", "cabinet_portfolios"))
cabinet_portfolios <- dbReadTable(con, c("config_data","cabinet_portfolios"))

dbExistsTable(con, c("config_data", "cabinet_reshuffle"))
cabinet_reshuffle <- dbReadTable(con, c("config_data","cabinet_reshuffle"))

dbExistsTable(con, c("config_data", "country"))
country <- dbReadTable(con, c("config_data","country"))

dbExistsTable(con, c("config_data", "electoral_alliances"))
electoral_alliances <- dbReadTable(con, c("config_data","electoral_alliances"))

dbExistsTable(con, c("config_data", "lh_election"))
lh_election <- dbReadTable(con, c("config_data","lh_election"))

dbExistsTable(con, c("config_data", "lh_seat_results"))
lh_seat_results <- dbReadTable(con, c("config_data","lh_seat_results"))

dbExistsTable(con, c("config_data", "lh_vote_results"))
lh_vote_results <- dbReadTable(con, c("config_data","lh_vote_results"))

dbExistsTable(con, c("config_data", "lower_house"))
lower_house <- dbReadTable(con, c("config_data","lower_house"))

dbExistsTable(con, c("config_data", "matviews"))
matviews <- dbReadTable(con, c("config_data","matviews"))

dbExistsTable(con, c("config_data", "minister"))
minister <- dbReadTable(con, c("config_data","minister"))

dbExistsTable(con, c("config_data", "mv_configuration_events"))
mv_configuration_events <- dbReadTable(con, c("config_data","mv_configuration_events"))

dbExistsTable(con, c("config_data", "party"))
party <- dbReadTable(con, c("config_data","lh_election"))

dbExistsTable(con, c("config_data", "pres_elec_vres"))
pres_elec_vres <- dbReadTable(con, c("config_data","pres_elec_vres"))

dbExistsTable(con, c("config_data", "presidential_election"))
presidential_election <- dbReadTable(con, c("config_data","presidential_election"))

dbExistsTable(con, c("config_data", "uh_election"))
uh_election <- dbReadTable(con, c("config_data","uh_election"))

dbExistsTable(con, c("config_data", "uh_seat_results"))
uh_seat_results <- dbReadTable(con, c("config_data","uh_seat_results"))

dbExistsTable(con, c("config_data", "upper_house"))
upper_house <- dbReadTable(con, c("config_data","upper_house"))

dbExistsTable(con, c("config_data", "veto_points"))
veto_points <- dbReadTable(con, c("config_data","veto_points"))


dbDisconnect(con)

