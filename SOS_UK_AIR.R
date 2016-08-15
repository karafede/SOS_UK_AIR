
library(XML)
library(utils)
library(R.utils)
library(RCurl)
library(curl)
library(threadr)
setwd("C:/SOS_UK_AIR")


# library(ereportr)


URL <- "https://uk-air.defra.gov.uk/data/sos/service?service=AQD&version=1.0.0&request=GetObservation&temporalFilter=om:phenomenonTime,After,2016-06-27T00:00:00.000Z&observedProperty=http://dd.eionet.europa.eu/vocabulary/aq/pollutant/10000020"
# file <- getURL(URL)
# doc <- xmlParse(link)
# xml_data  <- xmlToList(doc)


##### different approach
# temp_directory <- tempdir()
# work in the current directory
temp_directory  <- getwd()
# file <- "http://cdr.eionet.europa.eu/gb/eu/aqd/b/envve7epq/B_GB_Zones_retro.xml"
file <- "https://uk-air.defra.gov.uk/data/sos/service?service=AQD&version=1.0.0&request=GetObservation&temporalFilter=om:phenomenonTime,After,2016-06-27T00:00:00.000Z&observedProperty=http://dd.eionet.europa.eu/vocabulary/aq/pollutant/10000020"

# Download file if file is url
if (grepl("http://|https://", file, ignore.case = TRUE)) {
  
  # Download file
  if (verbose) message("Downloading...")
  
  # Get file
#  file_download <- file.path(temp_directory, basename(file))
 
 file_out <-  str_sub(URL, start = 1, end = -199) # use this only for long filename
 file_download <- file.path(temp_directory, basename(file_out))
# download.file(file, file_download, quiet = TRUE)
 curl_download(file, file_download)

  
  # Change file to file name of loaded document
  file <- file_download
  temp_file <- stringr::str_replace(file, "^([^.]*).*", "\\1")
  
} else {
  
  # Catch current directory notation
  # Python/system calls seem not to like this
  file <- stringr::str_replace(file, "^./", "")
  
  # Construct temp file for json conversion
  temp_file <- stringr::str_replace(file, "^([^.]*).*", "\\1")
  temp_file <- basename(temp_file)
  temp_file <- file.path(temp_directory, temp_file)
  
}



#######################################################################
#######################################################################

# Function to drive the xml to json conversion
# 
xml_to_json_python <- function(input, output = NA, pretty = FALSE) {
  
  # For script location
  script <- paste("C:/SOS_UK_AIR/xml_json.py")
 # script <- system.file("xml_json.py", package = "ereportr")
  
  # Do the best thing if no output is given
  if (is.na(output)) output <- stringr::str_replace(input, ".xml$|.XML$", ".json")
  
  # Quote the file names so spaces are ok
  # Windows is different, to-do, fix
  if (.Platform$OS.type == "unix") {
    
    input <- stringr::str_c("'", input, "'")
    output <- stringr::str_c("'", output, "'")
    
  }

  # Pretty Python logical switch
  pretty <- ifelse(pretty, "T", "F")
  
  # Build system command
  command <- paste("python", script, "-i", input, "-o", output, "-p", pretty)
  
  # Use command to create JSON with python script
  system(command)
  
}

#######################################################################
#######################################################################

# run the line below in cmd command line
# "python xml_json.py -i C:\Users\fk8\AppData\Local\Temp\Rtmpcr5JVx/B_GB_Zones_retro.xml -o C:\Users\fk8\AppData\Local\Temp\Rtmpcr5JVx/B_GB_Zones_retro -p F"

# "python xml_json.py -i C:\Users\fk8\AppData\Local\Temp\Rtmpcr5JVx/B_GB_Zones_retro.xml -o prova -p F"
  
xml_to_json_python(file, temp_file)  # it creates a greatsaving file!
