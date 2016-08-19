

library(XML)
library(utils)
library(R.utils)
library(RCurl)
library(curl)
library(threadr)
setwd("C:/SOS_UK_AIR")



library(XML)
library(RCurl)

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"

xData <- getURL(fileURL)
doc <- xmlParse(xData)
doc
xml_data <- xmlToList(doc)

################################################################################

fileURL <- "https://uk-air.defra.gov.uk/data/atom-dls/auto/2016/atom.en.xml"
xData <- getURL(fileURL)
doc <- xmlParse(xData)
doc
xml_data <- xmlToList(doc)


AAA <- as.list(xml_data[["pollutant"]])

BBB <- unlist(xml_data[["data"]][["time-layout"]][
  names(xml_data[["data"]][["time-layout"]]) == "start-valid-time"])

start_time <- unlist(xml_data[["data"]][["time-layout"]][
  names(xml_data[["data"]][["time-layout"]]) == "start-valid-time"])

