# Set-up ---------------------------
# Load packages
library(threadr)
library(dplyr)
library(tidyr)
library(RMySQL)
library(ggplot2)
library(ricardoR)

# Set global options
options(stringsAsFactors = FALSE)

# Set working directory
setwd("C:/Dropbox/ricardo/malta")

# Clear all objects
rm(list = ls(all = TRUE))

# Connect to databases
db <- dbConnect(MySQL(), dbname = "sos_malta_m5", host = "172.31.113.9",
                user = "stuartg", password = "ENrzmw8i")

# db.t <- dbConnect(MySQL(), dbname = "sos_malta_t", host = "172.31.113.9",
#                   user = "stuartg", password = "ENrzmw8i")
# 
# db.uk <- dbConnect(MySQL(), dbname = "sos", host = "172.31.113.9",
#                   user = "stuartg", password = "ENrzmw8i")

# Recommendations: Insert observableProperty and unit table first, then the
# look-up tables.


# # Reset tables ---------------------------
# reset_sos_observation_tables(db)
# 
# dbSendQuery(db, "DELETE FROM series WHERE seriesId IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE series AUTO_INCREMENT = 1")
# 
# dbSendQuery(db, "DELETE FROM samplingPoint WHERE samplingPointId IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE samplingPoint AUTO_INCREMENT = 1")
# 
# dbSendQuery(db, "DELETE FROM featureOfInterest WHERE featureOfInterestId IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE featureOfInterest AUTO_INCREMENT = 1")
# 
# dbSendQuery(db, "DELETE FROM `procedure` WHERE name IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE `procedure` AUTO_INCREMENT = 1")
# 
# dbSendQuery(db, "DELETE FROM observableProperty WHERE observablePropertyId IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE observableProperty AUTO_INCREMENT = 1")
# 
# dbSendQuery(db, "DELETE FROM offering WHERE offeringId IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE offering AUTO_INCREMENT = 1")
# 
# dbSendQuery(db, "DELETE FROM observationConstellation WHERE observationConstellationId IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE observationConstellation AUTO_INCREMENT = 1")


# Observable property table ---------------------------
# Get current table
data.observable.property <- dbGetQuery(db, "SELECT * FROM observableProperty")
# write.csv(data.observable.property, "sos_observable_property_table.csv", 
#           row.names = FALSE)

# Kill table
# dbSendQuery(db, "DELETE FROM observableProperty WHERE observablePropertyId IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE observableProperty AUTO_INCREMENT = 1")

# Insert new table
# insert.observable.property <- read.csv("sos_resources/observable_property_table.csv")
# db_insert(db, "observableProperty", insert.observable.property)


# Unit table ---------------------------
data.units <- dbGetQuery(db, "SELECT * FROM unit")
# write.csv(data.units, "unit_table.csv", row.names = FALSE)

# Kill table
# dbSendQuery(db, "DELETE FROM unit WHERE unitId IS NOT NULL")

# Insert new data
# data.units.new <- read.csv("sos_resources/unit_table.csv")
# db_insert(db, "unit", data.units.new)


# Load look-up tables ---------------------------
# Load data flow d, data used for the identiiers within the sos
data.d <- read.csv("sos_resources/maltas_2013_data_flow_d_from_eionet.csv")

# Load look-up/mapping file
data.lookup <- read.csv("sos_resources/malta_sos_look_up_table.csv", skip = 2)

# Load pollutant look-up table
info.pollutants <- read.csv("../e_reporting/xml_formating/resources/e_reporting_pollutant_codes_look_up.csv")


# Network ---------------------------
insert.network <- data.d %>% 
  distinct(network.inspire.id) %>% 
  select(network.inspire.id, network.name) %>% 
  rename(identifier = network.inspire.id,
         name = network.name) %>% 
  rbind.fill(db_table_names(db, "network"), .)

# Insert
# db_insert(db, "network", insert.network, increment_reset = TRUE)


# Station ---------------------------
# Modify table to include site variable
# dbSendQuery(db, "ALTER TABLE station ADD site varchar(255)")

insert.station <- data.d %>% 
  distinct(station.inspire.id) %>% 
  select(identifier = station.inspire.id, 
         name = station.name) %>% 
  right_join(info.station, c("identifier", "name")) %>% 
  arrange(identifier) %>% 
  rbind.fill(db_table_names(db, "station"), .)
  
# Insert
# db_insert(db, "station", info.station, increment_reset = TRUE)


# Sampling point ---------------------------
insert.sampling.point <- data.lookup %>% 
  filter(sampling.point.identifier != "") %>% 
  mutate(name = str_c(site, variable.name, sep = "_")) %>% 
  select(identifier = sampling.point.identifier,
         station = station.id,
         network = network.id,
         name) %>% 
  mutate(codespace = 1,
         assessmentType = "fixed", 
         description = str_c("sampling_point", name, sep = "_")) %>% 
  rbind.fill(db_table_names(db, "samplingPoint"), .)

# Insert
# db_insert(db, "samplingPoint", insert.sampling.point, increment_reset = TRUE)
# dbReadTable(db, "samplingPoint")

# Feature of interest ---------------------------
insert.foi <- data.lookup %>% 
  filter(feature.of.interest.identifier != "") %>% 
  mutate(name = str_c(site, variable.name, sep = "_")) %>% 
  select(identifier = feature.of.interest.identifier, 
         name) %>% 
  mutate(hibernateDiscriminator = FALSE, 
         featureOfInterestTypeId = 1, 
         description = str_c("sampling_inlet", name, sep = "_")) %>% 
  rbind.fill(db_table_names(db, "featureOfInterest"), .)

# Insert
# db_insert(db, "featureOfInterest", insert.foi, increment_reset = TRUE)
# dbReadTable(db, "featureOfInterest")


# Procedure ---------------------------
insert.procedure <- data.lookup %>% 
  filter(procedure.identifier != "") %>% 
  distinct(procedure.identifier) %>% 
  mutate(name = procedure.identifier) %>% 
  select(identifier = procedure.identifier) %>% 
  mutate(hibernateDiscriminator = FALSE, 
         procedureDescriptionFormatId = 1,
         codespace = 1, 
         deleted = FALSE, 
         disabled = FALSE, 
         referenceFlag = FALSE,
         description = str_c("instrument_configuration", identifier, sep = "_"), 
         name = description) %>% 
  rbind.fill(db_table_names(db, "`procedure`"), .)

# # Insert, no custom function due to reserved word
# dbSendQuery(db, "DELETE FROM `procedure` WHERE name IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE `procedure` AUTO_INCREMENT = 1")
# dbWriteTable(db, "procedure", insert.procedure, append = TRUE, row.names = FALSE)
# dbReadTable(db, "`procedure`")
  

# offering  ---------------------------
insert.offering <- data.lookup %>% 
  select(identifier = offering.identifier) %>% 
  distinct(identifier) %>% 
  filter(identifier != "") %>% 
  mutate(disabled = FALSE, 
         hibernateDiscriminator = FALSE, 
         name = identifier) %>% 
  rbind.fill(db_table_names(db, "offering"), .)

# Insert
# dbSendQuery(db, "DELETE FROM offering WHERE offeringId IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE offering AUTO_INCREMENT = 1")

# db_insert(db, "offering", insert.offering, increment_reset = TRUE)
# dbReadTable(db, "offering")


# observationConstellation  ---------------------------
insert.constellation <- data.lookup %>% 
  select(procedureId = procedure.id,
         observablePropertyId = observed.property.id,
         offeringId = offering.id) %>% 
  filter(offeringId != "") %>% 
  mutate(observationTypeId = 1, 
         deleted = FALSE, 
         hiddenChild = FALSE) %>% 
  rbind.fill(db_table_names(db, "observationConstellation"), .)

# Insert
# dbSendQuery(db, "DELETE FROM observationConstellation WHERE observationConstellationId IS NOT NULL")
# dbSendQuery(db, "ALTER TABLE observationConstellation AUTO_INCREMENT = 1")

# db_insert(db, "observationConstellation", insert.constellation, 
#           increment_reset = TRUE)
# dbReadTable(db, "observationConstellation")


# Series ---------------------------
insert.series <- data.lookup %>% 
  filter(sampling.point.identifier != "") %>% 
  select(featureOfInterestId = feature.of.interest.id, 
         observablePropertyId = observed.property.id, 
         procedureId = procedure.id, 
         samplingPointId = sampling.point.id, 
         unitId = unit.id) %>% 
  mutate(deleted = FALSE, 
         published = TRUE, 
         firstNumericValue = -99, 
         lastNumericValue = -99, 
         firstTimeStamp = ymd("2010-01-01"),
         lastTimeStamp = ymd("2020-01-01")) %>% 
  rbind.fill(db_table_names(db, "series"), .)

# Insert
# db_insert(db, "series", insert.series, increment_reset = TRUE)
# dbReadTable(db, "series")


# Spatial data stuff ---------------------------
# Look at X's database
db.uk <- dbConnect(MySQL(), host = "172.31.113.9", dbname = "sos", 
                   user = "stuartg", password = "ENrzmw8i")

data.foi <- dbGetQuery(db.uk, "SELECT *, AsWKT(geom) 
                               FROM featureOfInterest LIMIT 5")

# Alter feature of interest table
# dbSendQuery(db, "ALTER TABLE featureOfInterest ADD latitude DOUBLE PRECISION")
# dbSendQuery(db, "ALTER TABLE featureOfInterest ADD longitude DOUBLE PRECISION")

# # Update table
# dbSendQuery(db, "UPDATE featureOfInterest 
#                  SET latitude = 35.89002, longitude = 14.43446
#                  WHERE name LIKE 'attard%'")
# 
# dbSendQuery(db, "UPDATE featureOfInterest 
#                  SET latitude = 36.06707, longitude = 14.19715
#                  WHERE name LIKE 'gharb%'")
# 
# dbSendQuery(db, "UPDATE featureOfInterest 
#                  SET latitude = 35.88166, longitude = 14.51167
#                  WHERE name LIKE 'kordin%'")
# 
# dbSendQuery(db, "UPDATE featureOfInterest 
#                  SET latitude = 35.89584, longitude = 14.48999
#                  WHERE name LIKE 'msida%'")
# 
# dbSendQuery(db, "UPDATE featureOfInterest 
#                  SET latitude = 35.85229, longitude = 14.53899
#                  WHERE name LIKE 'zejtun%'")
#
# Now update geom variable
# Purposely reversed x and y to keep consistent with X's database 
# dbSendQuery(db, "UPDATE featureOfInterest SET geom = NULL")
# dbSendQuery(db, "UPDATE featureOfInterest 
#             SET geom = GeomFromText(CONCAT('POINT (', latitude, ' ', longitude, ')'), 4326)")

# Check
# dbGetQuery(db, "SELECT description, AsWKT(geom) AS wkt 
#                 FROM featureOfInterest")

# End of set-up


# Helpers ---------------------------
dbReadTable(db, "network")
dbReadTable(db, "station")
dbReadTable(db, "samplingPoint")
dbReadTable(db, "featureOfInterest")
dbReadTable(db, "`procedure`")
dbReadTable(db, "series")
dbReadTable(db, "unit")
dbReadTable(db, "observableProperty")
dbGetQuery(db, "SELECT * FROM observation LIMIT 5")
dbGetQuery(db, "SELECT * FROM numericValue LIMIT 5")
dbReadTable(db, "observationType")
dbReadTable(db, "offering")
dbGetQuery(db, "SELECT * FROM observationHasOffering LIMIT 5")
dbReadTable(db, "observationConstellation")


# Get site-variable combinations ---------------------------
# Get file list
file.list <- list.files("clean_data/", ".csv$", full.names = TRUE)

# Load data
data.load <- ldply(file.list, read.csv, .progress = "text") %>% 
  mutate(date = parse_date_time(date, c("ymd hms", "ymd")))

data.load.molten <- data.load %>% 
  gather(variable, value, -date, -site, na.rm = TRUE) 

data.load.molten.distinct <- data.load.molten %>% 
  distinct(site, variable) %>% 
  arrange(site, variable) %>% 
  select(-value, -date) %>% 
  mutate(variable = str_replace_all(variable, "\\.", "_"))
  
# # Export
# data.load.molten.distinct %>% 
#   write.csv("malta_site_and_variable_names.csv", row.names = FALSE)


data.current.lookup <- read.csv("sos_resources/malta_sos_look_up_table.csv", skip = 2)

data.lookup <- data.load.molten.distinct %>% 
  left_join(data.current.lookup, by = c("site", "variable"))

# data.lookup %>% 
#   write.csv("test.csv", row.names = FALSE, na = "")

glimpse(data.load)
names(data.load)

data.load %>% 
  ggplot(aes(date, temp, colour = site)) + geom_line() + 
  facet_wrap("site") + ylim(-5, 40)

data.load %>% 
  ggplot(aes(date, rh.3, colour = site)) + geom_line() + 
  facet_wrap("site")

data.load %>% 
  ggplot(aes(date, enc.temp, colour = site)) + geom_line() + 
  facet_wrap("site") + ylim(-5, 40)

data.load %>% 
  ggplot(aes(date, wind.speed, colour = site)) + geom_line() + 
  facet_wrap("site")

data.load %>% 
  ggplot(aes(date, wd, colour = site)) + geom_line() + 
  facet_wrap("site")

data.load %>% 
  ggplot(aes(date, ap, colour = site)) + geom_line() + 
  facet_wrap("site") + ylim(900, 1100)


data.load.molten %>% 
  filter(variable == "nox", site == "mobile_station")



# Alter CO unit ---------------------------
# dbSendQuery(db, "UPDATE observation SET unitId = 27 WHERE seriesId IN (16, 30)")
# dbSendQuery(db, "UPDATE series SET unitId = 27 WHERE seriesId IN (16, 30)")
# 
# dbGetQuery(db, "SELECT * FROM observation WHERE seriesId = 16 LIMIT 100")
# dbGetQuery(db, "SELECT * FROM series WHERE seriesId IN (16, 30)")
