#' Function to find "change" information in the header of e-Reporting XML 
#' documents. 
#' 
#' \code{xml_change_infomation} extracts pieces of an XML file's header document
#' so change information can be returned quickly. \code{xml_change_infomation} 
#' is useful to see what an E1a file contains without having to open the large 
#' file completely. 
#' 
#' @param file File name of an e-Reporting XML file. \code{file} can be a 
#' compressed file. 
#' 
#' @param n Number of lines to read in XML file. Default is 150 lines because 
#' the change information is within the header and this is usually adequate. 
#' 
#' @param single Should the text attempted to be filtered so multiple entries 
#' are avoided? Deafult is \code{TRUE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Get change information from UK's 2013 E1a XML file
#' xml_change_infomation("E1a_GB_FixedObservations.xml")
#' 
#' # Get change information from a (compressed) resubmission file
#' xml_change_infomation("2015-11-25_2013_resubmission_E1a_GB_FixedObservations.xml.bz2")
#' 
#' # Attainment information
#' xml_change_infomation("G_GB_Attainment.xml")
#' 
#' # Measurement information
#' xml_change_infomation("GB_D_FixedAssessmentMethods.xml")
#' 
#' }
#' 
#' @export
xml_change_infomation <- function(file, n = 150, single = TRUE) {
  
  # Read n lines of XML file as text
  text <- readLines(file, n = n)
  text <- stringr::str_replace_all(text, "\\s+", " ")
  
  # Get and format strings
  if (grepl(".json$", file, ignore.case = TRUE)) {
    
    # Clean json
    # Header is an attribute
    header_index <- grep("aqd:AQD_ReportingHeader", text)
    header <- text[header_index:header_index + 1]
    header <- stringr::str_replace_all(header, '"|\\,', "")
    header <- stringr::str_replace_all(header, "@gml:id:", "")
    header <- stringr::str_trim(header)
    
    local_id <- grep("base:localId", text, value = TRUE)
    local_id <- str_drop_json_tags(local_id)
    
    version <- grep("base:versionId", text, value = TRUE)
    version <- str_drop_json_tags(version)
    
    change <- grep("aqd:change>|aqd:change\"", text, value = TRUE)
    change <- str_drop_json_tags(change)
    
    change_description <- grep("aqd:changeDescription", text, value = TRUE)
    change_description <-  str_drop_json_tags(change_description)
    
    begin_position <- grep("gml:beginPosition", text, value = TRUE)
    begin_position <- str_drop_json_tags(begin_position)
    
    # Nested here too
    end_position_index <- grep("gml:endPosition", text)
    end_position <- text[end_position_index:end_position_index + 1]
    end_position <- stringr::str_replace_all(end_position, '"|\\}', "")
    end_position <- stringr::str_replace_all(end_position, "@indeterminatePosition:", "")
    end_position <- stringr::str_trim(end_position)
    
  } else {
    
    # For xml, the default
    # Extract pieces from header
    # Header is an attribute
    header <- grep("aqd:AQD_ReportingHeader", text, value = TRUE)
    header <- stringr::str_c(header, collapse = " ")
    header <- stringr::str_split_fixed(header, "gml:id=", n = 2)[, 2]
    header <- stringr::str_split_fixed(header, " ", n = 2)[, 1]
    header <- stringr::str_replace_all(header, '"|\\>', "")
    
    local_id <- grep("base:localId", text, value = TRUE)
    local_id <- str_drop_xml_tags(local_id)
    
    version <- grep("base:versionId", text, value = TRUE)
    version <- str_drop_xml_tags(version)
    
    change <- grep("aqd:change>|aqd:change\"", text, value = TRUE)
    change <- str_drop_xml_tags(change)
    
    change_description <- grep("aqd:changeDescription", text, value = TRUE)
    change_description <-  str_drop_xml_tags(change_description)
    
    begin_position <- grep("gml:beginPosition", text, value = TRUE)
    begin_position <- str_drop_xml_tags(begin_position)
    
    end_position <- grep("gml:endPosition", text, value = TRUE)
    end_position <- str_drop_xml_tags(end_position)
    
  }
  
  # Catches for when variables do not exist
  if (length(header) == 0) header <- NA
  
  if (length(local_id) == 0) local_id <- NA
  
  if (length(version) == 0) version <- NA
  
  if (length(change) == 0) change <- NA
  
  if (length(change_description) == 0) change_description <- NA
  
  if (length(begin_position) == 0) begin_position <- NA
  
  if (length(end_position) == 0) end_position <- NA
  
  # Single row only, the header
  if (single) {
    
    # Create a data frame
    df <- data.frame(header = header[1],
                     local_id = local_id[1], 
                     version = version[1],
                     change = change[1], 
                     change_description = change_description[1], 
                     begin_position = begin_position[1],  
                     end_position = end_position[1])
    
  } else {
    
    # Create a data frame
    df <- data.frame(header,
                     local_id, 
                     version,
                     change, 
                     change_description,
                     begin_position,
                     end_position)
    
  }
  
  # Return
  df
  
}


str_drop_xml_tags <- function(string) {
  string <- stringr::str_replace_all(string, "<.*?>", "")
  string <- stringr::str_trim(string)
  string
}


str_drop_json_tags <- function(string) {
  string <- stringr::str_replace_all(string, '"', "")
  string <- stringr::str_replace(string, ",$", "")
  string <- stringr::str_split_fixed(string, pattern = ":", n = 3)[, 3]
  string <- stringr::str_trim(string)
  string <- stringr::str_replace(string, "\\{|\\}", "")
  string
}

