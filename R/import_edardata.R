############################################
#' @title import_edardata
############################################

#' @name import_edardata
#' @description Function imports EDAR data sets from the West Lothian (See below).
#' @param edardata Character, data of set name (see below)
#' @param simplify.names Logical, if \code{TRUE}, simplifies the column names 
#' (converting to lower case, changes underscores to dots, etc), and replaces 
#' converts from HEATs m[n]_name format so analytes are identified in column 
#' names.   
#' @param ... Currently ignored.
#' @details The function imports data from the 
#' \href{https://www.westlothian.gov.uk/article/45802/Real-Time-Vehicle-Emissions-pilot-project-March-2017}{Edinburgh and West Lothian Real Time Vehicle Emissions Stduy 2017}. 
#' 
# (table hand written - only two sites)
#' \tabular{lll}{
#'  dataset \tab datatype \tab ref\cr
#'
#' edin2017     \tab Edinburgh EDAR 2017 \tab [ref omid's paper?]                                         \cr
#' brox2017     \tab Edinburgh EDAR 2017 \tab [ref omid's paper?]                                         \cr

#' }
#' @returns \code{import_edardata} attempts to match \code{edardata} and, if found, 
#' returns it as a \code{data.frame}.  
#' 

##################################
#doc needs rewriting from here
##################################



#' The function uses base \code{R} code, \code{\link{download.file}} to 
#' temporarily download data sets and \code{\link{readxl::read_xlsx}}  
#' to read data in. The \code{simplify.names=TRUE} argument is used 
#' to make data column names a little r-friendlier. 
#' @note 
#' In-development: code may be subject to significant change.
#' @references 
#' For \code{read_xlsx} package and \code{read_xlsx}: 
#' 
#'  Wickham H, Bryan J (2022). _readxl: Read Excel Files_. 
#'  R package version 1.4.0, <https://CRAN.R-project.org/package=readxl>.
#'
#' For general information about EDAR:
#' 
#' [my ref]?
#' 
#' @seealso
#' \code{\link{readxl::read_xlsx}}.
#' @examples 
#' #to do...
#' 


#kr 2023/01/10
#   draft code assessing pblic data
#   maybe check drew and transport scotland OK with us doing this...
#   v 0.1

################################
#to think about
################################
#think about further ref tidying 
#compare with import_featdata


#notes
########################
#added readxl as import
#usethis::use_package("readxl")

#splatted function
#' @export
import_edardata <- 
  function(edardata = NULL, 
           simplify.names = TRUE,
           ...){
    #initial edardata importer is by reference 
    #does not seem worth effort to make look up table
    #for two datasets...
  
    if(!is.character(edardata)){
      stop("`edardata` missing, please set (see ?import_edardata)")
    }
    edardata <- tolower(edardata)
    
    if(!edardata %in% c("edin2017", "brox2017")){
      stop("`edardata` unknown, please set (see ?import_edardata)")
    }
    
    ref <- NULL
    if(edardata == "edin2017"){
      ref <- "https://www.westlothian.gov.uk/media/18036/2017-03-Anonymised-data-Real-Time-Vehicle-Emissions-pilot-project-Edinburgh/excel/2017-03_Anonymised_data_-__Real_Time_Vehicle_Emissions_pilot_project_Edinburgh_A8209510.xlsx"
    }
    if(edardata == "brox2017"){
      ref <- "https://www.westlothian.gov.uk/media/18084/2017-03-Anonymised-data-Real-Time-Vehicle-Emissions-pilot-project-Broxburn/excel/2017-03_Anonymised_data_-__Real_Time_Vehicle_Emissions_pilot_project_Broxburn.xlsx"
    }
    if(is.null(ref)){
      stop("`edardata` unknown, please set (see ?import_edardata)")
    }
    #download xlsx to read locally
    #did not seem to work when I tried to read this directly 
    utils::download.file(ref, method = "curl", dest = "...xlsx.xlsx", 
                         quiet=TRUE)
    
    #############################
    #as.is equivalent reading data in??
    #cf: used in 
    
    out <- readxl::read_excel("...xlsx.xlsx")
    if(simplify.names){
      names(out) <- tolower(names(out))
      #test for name columns 
      .test <- grep("_name$", names(out))
      #get name of species m2, m3, m4, etc 
      #    (saved as m[n]_name) column 
      #    and replace m2, etc in other names 
      #    with the m2_name 
      if(length(.test)>0){
        for(i in .test){
          .name <- names(out)[i]
          .temp <- tolower(out[[.name]][1])
          .name <- gsub("_name", "", .name)
          names(out) <- gsub(.name, .temp, names(out))
          #print(.temp)
        }
        out <- out[,-.test]
      }
    }
    #remove temp file if made
    if (file.exists("...xlsx.xlsx")) {
      file.remove("...xlsx.xlsx")
    }
    out 
  }


