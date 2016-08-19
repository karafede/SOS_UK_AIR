#' Function to "flatten" e-Reporting XML documents.
#' 
#' \code{flatten_xml} transforms the opaque XML documents used by the European 
#' Environment Agency into tables. \url{http://www.eionet.europa.eu/aqportal/Drep1} 
#' is the repository for these files.
#' 
#' \code{flatten_xml} is not a particularly elegant function. It requires a
#' Python script which converts XML files to JSON. This JSON file is then loaded
#' and parsed by the \strong{jsonlite} package and then transformed into a data 
#' frame. 
#' 
#' For some variables, the 2-dimensional table structure fails to represent the 
#' content in the XML documents. This occurs on occasions because of nested 
#' variables. The "unnesting" of these variables is unreliable because of a 
#' number of reasons. These nested variables are currently extracted, then 
#' collapsed into a value with a "\code{ ; }" separator. The idea of using 
#' \code{ ; } as a separator is that this should be unique and can be used a 
#' delimiter for string splitting after the table has been generated. 
#' 
#' \code{flatten_xml} requires a document to be specified. This is because some
#' XML file contains many documents. An example of this is data flow d could 
#' contain the "network", "station", "sample", "sampling_point", and 
#' "sampling_point_process" documents. 
#' 
#' \code{flatten_xml} does not work for E1a XML documents. Use 
#' \code{\link{scrape_data_array}} for those files. 
#' 
#' \code{flatten_xml} requires Python and the \code{xmltodict} module to be 
#' installed. 
#' 
#' Values of \code{document} can be: 
#' \tabular{ll}{
#' document               \tab data_flow \cr
#' header                 \tab NA \cr
#' zone                   \tab d  \cr
#' assessment_regime      \tab c  \cr
#' network                \tab d  \cr
#' station                \tab d  \cr
#' sample                 \tab d  \cr
#' sampling_point         \tab d  \cr
#' sampling_point_process \tab d  \cr
#' model                  \tab d  \cr
#' model_process          \tab d  \cr
#' model_area             \tab d  \cr
#' model_observation      \tab e1a\cr
#' attainment             \tab g  \cr
#' plan                   \tab h  \cr
#' source_appointment     \tab i  \cr
#' evaluation_scenario    \tab j  \cr
#' measure                \tab k  
#' }
#' 
#' @seealso \url{https://github.com/martinblech/xmltodict}, 
#' \url{http://www.eionet.europa.eu/aqportal/Drep1}
#' 
#' @author Stuart K. Grange
#'
#' @param file XML file name. \code{file} can also be a URL, but not a compressed 
#' XML file. 
#' 
#' @param document Which document unit should be extracted from \code{file}? 
#' For example \code{"sample"}, \code{"plan"}, or \code{"attainment"}.
#' 
#' @param drop Should the unhelpful \code{"nil"} variables be dropped from the 
#' flattened output? 
#' 
#' @param basename Should hyperlinks be trimmed so only their final elements 
#' are returned (their basename)?
#' 
#' @param convert Should \code{type.convert} be applied to the data frame to 
#' attempt to make variables their correct data types? 
#' 
#' @param clean Should the variable names attempted to be cleaned with a look-up 
#' table? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @examples
#' \dontrun{
#' # Flatten UK's sampling point data, within data flow d
#' data_sampling_point <- flatten_xml("GB_D_FixedAssessmentMethods.xml", 
#'                                    "sampling_point")
#' 
#'   
#' # Find the air quality stations in Spain, use a url
#' url <- "http://cdr.eionet.europa.eu/es/eu/aqd/d/envvgejqw/ES_D_Metainfo.xml"
#' data_station <- flatten_xml(url, "station")
#' 
#' 
#' # Air quality plan documents
#' data_h <- flatten_xml("gis_dev_plans_document.xml", "plan")
#'
#' data_i <- flatten_xml("gis_dev_source_appointment_document.xml", 
#'                       "source_appointment")
#'
#' data_j <- flatten_xml("gis_dev_evaluation_scenario_document.xml",
#'                       "evaluation_scenario")
#'
#' data_k <- flatten_xml("gis_dev_measures_document.xml", "measures")
#' 
#' # Export a data frame to a csv file
#' write.csv(data_k, "gis_dev_measures.csv", row.names = FALSE)
#'
#' }
#'    
#' @export
flatten_xml <- function (file, document, drop = FALSE, basename = FALSE, 
                         convert = FALSE, clean = FALSE, verbose = FALSE) {
  
  # Check document
  if (!document %in% get_supported_documents())
    stop("Document is not supported.", call. = FALSE)

  # Get date
  date_flattened <- Sys.time()
  
  # Find directory
  temp_directory <- tempdir()
  
  # Download file if file is url
  if (grepl("http://|https://", file, ignore.case = TRUE)) {
    
    # Download file
    if (verbose) message("Downloading...")
    
    # Get file
    file_download <- file.path(temp_directory, basename(file))
    download.file(file, file_download, quiet = TRUE)
    
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
  
  # Get header information
  version_header <- as.character(xml_change_infomation(file)$version)
  
  # If xml file, convert
  if (grepl(".xml$", file, ignore.case = TRUE)) {

    # Convert xml to json
    if (verbose) message("Converting...")
    
    # Use Python glue script
    xml_to_json_python(file, temp_file)
    
  } else {
    
    # Switch for json files
    temp_file <- file
    
  }
  
  # Parse json file
  if (verbose) message("Parsing...")
  
  # The parser
  json <- jsonlite::fromJSON(temp_file)
  
  # Get a document within json file
  if (verbose) message("Cleaning...")
  
  # Header
  if (document == "header") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$
	   `aqd:AQD_ReportingHeader`
    
  }
  
  # Data flow A, unknown at the moment
  
  # Data flow B
  if (document == "zone") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_Zone`
    
  }
  
  # Data flow C
  if (document == "assessment_regime") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$
	   `aqd:AQD_AssessmentRegime`
    
    # Ireland is strange, content in header and has null elements
    if (is.null(df)) {
      
      df <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`
      
      # When the content has a second null list node
      if (length(df) > 1) df <- df[[1]]
      
    }
    
  }
  
  # Data flow D, many documents within this data flow
  # Measurements
  if (document == "network") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_Network`
    
    # Document within header path
    if (is.null(df)) {
      
      df <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`$`aqd:AQD_Network`
      
    }
    
    # Irish data, an issue with extra nesting which is not named
    if (is.null(df)) {
      
      list_content <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`
      
      # Get logical vector for filtering
      index_names <- lapply(list_content, names) == "aqd:AQD_Network"
      
      # Get data frame
      df <- list_content[index_names]
      
      # Remove final piece of nesting
      df <- data.frame(df)
      
    }
    
  }
  
  if (document == "station") {
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_Station`
    
    # Document within header path
    if (is.null(df)) {
      
      df <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`$`aqd:AQD_Station`
      
    }
    
    # Irish data, an issue with extra nesting which is not named
    if (is.null(df)) {
      
      list_content <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`
      
      # Get logical vector for filtering
      index_names <- lapply(list_content, names) == "aqd:AQD_Station"
      
      # Get data frame
      df <- list_content[index_names]
      
      # Remove final piece of nesting
      df <- data.frame(df)
      
    }
    
  }
  
   if (document == "sample") {
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_Sample`
    
    # Document within header path
    if (is.null(df)) {
      
      df <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`$`aqd:AQD_Sample`
      
    }
    
    # Irish data, an issue with extra nesting which is not named
    if (is.null(df)) {
      
      list_content <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`
      
      # Get logical vector for filtering
      index_names <- lapply(list_content, names) == "aqd:AQD_Sample"
      
      # Get data frame
      df <- list_content[index_names]
      
      # Remove final piece of nesting
      df <- data.frame(df)
      
    }
    
  }
  
  if (document == "sampling_point") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_SamplingPoint`
    
    # Document within header path
    if (is.null(df)) {
      
      df <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`$`aqd:AQD_SamplingPoint`
      
    }
    
    # Irish data, an issue with extra nesting which is not named
    if (is.null(df)) {
      
      list_content <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`
      
      # Get logical vector for filtering
      index_names <- lapply(list_content, names) == "aqd:AQD_SamplingPoint"
      
      # Get data frame
      df <- list_content[index_names]
      
      # Remove final piece of nesting
      df <- data.frame(df)
      
    }
    
  }
  
  if (document == "sampling_point_process") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$
      `aqd:AQD_SamplingPointProcess`
    
    # Document within header path
    if (is.null(df)) {
      
      df <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`$`aqd:AQD_SamplingPointProcess`
      
    }
    
    # Irish data, an issue with extra nesting which is not named
    if (is.null(df)) {
      
      list_content <- json$`gml:FeatureCollection`$`gml:featureMember`$
        `aqd:AQD_ReportingHeader`$`aqd:content`
      
      # Get logical vector for filtering
      index_names <- lapply(list_content, names) == "aqd:AQD_SamplingPointProcess"
      
      # Get data frame
      df <- list_content[index_names]
      
      # Remove final piece of nesting
      df <- data.frame(df)
      
    }
    
  }
  
  # Models
  if (document == "model") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_Model`
    
  }
  
  if (document == "model_process") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_ModelProcess`
    
  }
  
  if (document == "model_area") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_ModelArea`
    
  }

  # Data flow E1a for measurement data requires other functions
  
  # E1a models
  if (document == "model_observation") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`om:OM_Observation`
    
  }
  
  # Data flow F, unknown at the moment
  
  # Data flow G
  if (document == "attainment") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_Attainment`
    
  }
  
  # Data flow H
  if (document == "plan") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_Plan`
    
  }
  
  # Data flow I
  if (document == "source_appointment") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$
	  `aqd:AQD_SourceApportionment`
    
  }
  
  # Data flow J
  if (document == "evaluation_scenario") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$
	  `aqd:AQD_EvaluationScenario`
    
  }
  
  # Data flow K
  if (document == "measure") {
    
    df <- json$`gml:FeatureCollection`$`gml:featureMember`$`aqd:AQD_Measures`
    
  }
  
  # Stop if no document was extracted
  if (is.null(df)) 
    stop("The document was not found in the XML file.", call. = FALSE)
  
  # Remove NA observations
  df <- df[!is.na(df[, 1]), ]
  row.names(df) <- NULL
  
  # Flatten nested data frames
  df <- jsonlite::flatten(df)
  
  # Remove NA observations again if needed after flattening
  if (any(is.na(df[, 1]))) {
    
    df <- df[!is.na(df[, 1]), ]
    row.names(df) <- NULL
    
  }
  
  
  # Clean lists within data frame
  list_index <- sapply(df, is.list)
  
  df[list_index] <- lapply(df[list_index], function (x) 
    extract_nested_elements(x, basename))
  
  # Drop nil variables, pretty old school dropping
  if (drop) {
    
    # Get index
    index <- grep("nil|nil_reason$", names(df), ignore.case = TRUE, value = TRUE)
    
    # Drop
    df <- df[, !names(df) %in% index]
    
  }
  
  # Basenames for hyperlinks
  if (basename) df <- only_basenames(df)
  
  # Attempt to make correct data types
  if (convert) {
    
    # Parse lower-case logicals
    df[] <- lapply(df, function(x) ifelse(x == "false", "FALSE", x))
    df[] <- lapply(df, function(x) ifelse(x == "true", "TRUE", x))
    
    # Type convert
    df[] <- lapply(df, function(x) type.convert(as.character(x), as.is = TRUE))
    
  }
  
  # Clean names
  # Remove special characters
  names(df) <- stringr::str_replace_all(names(df), "@", "")
  names(df) <- stringr::str_replace_all(names(df), "#", "")
  names(df) <- threadr::str_underscore(names(df))
  
  # Use look-up table
  if (clean) df <- clean_xml_names(df, document)
  
  # Add header date
  if (!document %in% c("header", "headers")) df$version_header <- version_header
  
  # Add date of flattening
  df$date_flattened <- date_flattened
  
  # Return
  df
  
}


# Function to drive the xml to json conversion
# 
# No export
xml_to_json_python <- function(input, output = NA, pretty = FALSE) {
  
  # For script location
  script <- system.file("xml_json.py", package = "ereportr")

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


# Pull out elements within lists and return a vector.
# 
# No export
extract_nested_elements <- function(x, basename) {
  
  # Unlist
  x <- lapply(x, unlist)
  
  # Ensure null elements are not dropped
  x[sapply(x, is.null)] <- "NA"
  
  # Strip strings
  if (basename) {
    
    x <- tryCatch({
      
      # Try to use basename function for list
      x <- lapply(x, basename)
      
    }, error = function(e) {
      
      # Pass, give up on the basename function
      
    })
    
  }
  
  # Collapse elements within list's nodes
  x <- lapply(x, function(y) stringr::str_c(y, collapse = " ; "))
  
  # To vector
  x <- unlist(x)

  # Return
  x

}

    
# No export
only_basenames <- function(df) {
  
  # Get index
  # Hyperlinks
  index_hyperlink <- apply(df, 2, function(x) any(grepl("^http:|^https:", x)))
  
  # Semi-colons added for nested vectors
  index_sep <- apply(df, 2, function(x) any(grepl(" ; ", x)))
  
  # Remove nested vector variables from index
  index_all <- cbind(index_hyperlink, index_sep)
  index_all <- cbind(index_all, ifelse(index_all[, 1] & index_all[, 2], FALSE, 
                                       index_all[, 1]))
  
  # Back to vector
  index <- index_all[, 3]
  
  # Strip strings
  df[index] <- lapply(df[index], basename)
  
  # Return
  df
  
}


# Cleaning names; requires a look-up table
clean_xml_names <- function(df, document, dirty = "name_xml", clean = "name_clean") {
  
  # To-do, look at plurals (s at the end of document)
  
  # Read look-up table
  df_lookup <- read.csv(system.file("extdata/xml_names_look_up.csv", 
                                    package = "ereportr"))
  
  # Filter look-up
  df_lookup <- df_lookup[df_lookup$document == document, ]
  
  # Replace names with for-loop?!
  for (i in 1:nrow(df_lookup)) {
    
    # For each observation in look-up table
    names(df) <- ifelse(
      names(df) == df_lookup[, dirty][i], df_lookup[, clean][i], names(df))
    
  }
  
  # Return
  df
  
}


get_supported_documents <- function() {
  # Read look-up table
  read.csv(system.file(
    "extdata/xml_document_types.csv", package = "ereportr"))$document
}
