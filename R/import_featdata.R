############################################
#' @title import_featdata
############################################

#' @name import_featdata
#' @description Function imports data from the Denver FEAT data archive (See below).
#' @param featdata Character, data set name (see below)
#' @param simplify.names Logical, if \code{TRUE}, simplifies the column names of the 
#' found data set by converting them to lower case and changing underscores to dots. 
#' @param ... Currently ignored.
#' @details The function imports data from the 
#' \href{https://digitalcommons.du.edu/feat/}{FEAT data archive}. 
#' 
# (see below about regenerating the table)
#' \tabular{lll}{
#'  dataset \tab datatype \tab ref\cr
#'
#' Tucson 94     \tab light duty vehicles \tab 10.1021/Es950191j                                         \cr
#'  Phnx 98       \tab light duty vehicles \tab 10.1021/es702413b                                         \cr
#'  Phnx 99       \tab light duty vehicles \tab 10.1021/es702413b                                         \cr
#'  AZ 2000       \tab light duty vehicles \tab 10.1021/es702413b                                         \cr
#'  AZ 2002       \tab light duty vehicles \tab 10.1021/es702413b                                         \cr
#'  AZ 2004       \tab light duty vehicles \tab 10.1021/es702413b                                         \cr
#'  AZ 2006       \tab light duty vehicles \tab 10.1021/es702413b                                         \cr
#'  Fresno 08     \tab light duty vehicles \tab 10.1021/acs.est.0c05433                                   \cr
#'  Fresno 21     \tab light duty vehicles \tab 10.1021/acs.est.0c05433                                   \cr
#'  Cal 89        \tab light duty vehicles \tab 10.1126/science.268.5213.991                              \cr
#'  Cal 91        \tab light duty vehicles \tab 10.1126/science.268.5213.991                              \cr
#'  LA710 99      \tab light duty vehicles \tab -                                                         \cr
#'  Lynwd105 2018 \tab light duty vehicles \tab 10.1080/10962247.2019.1611677                             \cr
#'  Lynwd710 2018 \tab light duty vehicles \tab 10.1080/10962247.2019.1611677                             \cr
#'  LA 99         \tab light duty vehicles \tab 10.1021/es026340x                                         \cr
#'  LA 00         \tab light duty vehicles \tab 10.1021/es026340x                                         \cr
#'  LA 01         \tab light duty vehicles \tab 10.1021/es026340x                                         \cr
#'  Smnto 99      \tab light duty vehicles \tab -                                                         \cr
#'  Sanjose 99    \tab light duty vehicles \tab 10.1021/acs.est.0c05433; 10.1021/acs.est.0c05433          \cr
#'  Sanjose 08    \tab light duty vehicles \tab 10.1021/acs.est.0c05433; 10.1021/acs.est.0c05433          \cr
#'  Vannuys 10    \tab light duty vehicles \tab 10.1080/10962247.2012.699015; 10.1080/10962247.2012.699016\cr
#'  Labrea 01     \tab light duty vehicles \tab 10.1080/10962247.2020.1869121                             \cr
#'  Labrea 03     \tab light duty vehicles \tab 10.1021/es702413b                                         \cr
#'  Labrea 05     \tab light duty vehicles \tab 10.1021/acs.est.0c05433                                   \cr
#'  Labrea 08     \tab light duty vehicles \tab 10.1021/es5043518                                         \cr
#'  Labrea 13     \tab light duty vehicles \tab 10.1021/acs.est.5b02392                                   \cr
#'  Labrea 15     \tab light duty vehicles \tab 10.1021/acs.est.6b00717                                   \cr
#'  Labrea 18     \tab light duty vehicles \tab 10.1021/acs.est.0c05433                                   \cr
#'  Labrea 99     \tab light duty vehicles \tab 10.1080/10962247.2019.1611677                             \cr
#'  Peralta 97    \tab heavy duty vehicles \tab 10.1021/Es001533a                                         \cr
#'  Peralta 08    \tab heavy duty vehicles \tab 10.1021/es202392g                                         \cr
#'  Peralta 09    \tab heavy duty vehicles \tab 10.1021/es202392g                                         \cr
#'  Peralta 10    \tab heavy duty vehicles \tab 10.1021/es202392g                                         \cr
#'  Peralta 12    \tab heavy duty vehicles \tab 10.1021/es401487b                                         \cr
#'  Peralta 17    \tab heavy duty vehicles \tab 10.1021/acs.est.8b03994                                   \cr
#'  laport 08     \tab heavy duty vehicles \tab 10.1021/es202392g, 10.1021/es401487b                      \cr
#'  laport 09     \tab heavy duty vehicles \tab 10.1021/es202392g, 10.1021/es401487b                      \cr
#'  laport 10     \tab heavy duty vehicles \tab 10.1021/es202392g, 10.1021/es401487b                      \cr
#'  laport 12     \tab heavy duty vehicles \tab 10.1021/es202392g, 10.1021/es401487b                      \cr
#'  coors 99      \tab heavy duty vehicles \tab 10.1021/Es001533a                                         \cr
#'  coors 05      \tab heavy duty vehicles \tab 10.1021/es060989a                                         \cr
#'  dumont 99     \tab heavy duty vehicles \tab 10.1021/Es001533a                                         \cr
#'  dumont 05     \tab heavy duty vehicles \tab 10.1021/es060989a                                         \cr
#'  uri 98        \tab heavy duty vehicles \tab 10.1021/Es001533a                                         \cr
#'  pohouston 09  \tab heavy duty vehicles \tab -                                                         \cr
#'  sanmarcos 98  \tab heavy duty vehicles \tab 10.1021/Es001533a                                         \cr
#'  utah 20       \tab heavy duty vehicles \tab 10.1021/Es001533a                                         \cr
#'  cottonwood 13 \tab heavy duty vehicles \tab 10.1021/es505534e                                         \cr
#'  cottonwood 15 \tab heavy duty vehicles \tab 10.1021/acs.est.6b06172                                   \cr
#'  cottonwood 17 \tab heavy duty vehicles \tab 10.1021/acs.est.8b00621                                   \cr
#'  pola 13       \tab heavy duty vehicles \tab 10.1021/es505534e                                         \cr
#'  pola 15       \tab heavy duty vehicles \tab 10.1021/acs.est.6b06172                                   \cr
#'  pola 17       \tab heavy duty vehicles \tab 10.1021/acs.est.8b00621                                   
#' }
#' @returns \code{import_featdata} attempts to match \code{featdata} and, if found, 
#' returns it as a \code{data.frame}.  
#' 
#' The function uses base \code{R} code and \code{\link{foreign::read.dbf}} to download, 
#' read and tidy data. The \code{as.is=TRUE} argument is applied when reading data so, 
#' for example, \code{date} and \code{time} columns are imported as \code{character} 
#' vectors rather than \code{factors}.  
#' @note 
#' In-development: code may be subject to significant change; Currently, only some 
#' data sets can be downloaded from the FEAT archive. (See Details above for list of 
#' data sets available using \code{import_featdata}; please check main archive, if you 
#' are after any other FEAT data not listed here.)  
#' @references 
#' For \code{foreign} package and \code{read.dbf}: 
#' 
#' R Core Team (2022). _foreign: Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS',
#' 'Stata', 'Systat', 'Weka', 'dBase', ..._. R package version 0.8-82, 
#' <https://CRAN.R-project.org/package=foreign>.
#'
#' For general information about FEAT:
#' 
#' D.H. Stedman and G.A. Bishop. Measuring the Emissions of Passing Cars,  
#' Acc. Chem. Res., 29:489-495, 1996. 
#' 
#' D.A. Burgard, G.A. Bishop, R.S. Stadtmuller, T.R. Dalton and D.H. Stedman, 
#' Spectroscopy Applied to On-Road Mobile Source Emissions.
#' Appl. Spectrosc., 60:5:135A-148A, 2006.
#'
#' And, for specific campaigns/data sets, please reference using study 
#' reference(s), listed as associated DOI(s) in Details above. 
#' @seealso
#' \code{\link{foreign::read.dbf}}.
#' @examples 
#' #to do...
#' 

#kr 2022/08/30
#   gary confirmed OK to do this...
#   v 0.1

################################
#to think about
################################
#think about further ref tidying 
#   column names handling in dtan_tabular, like bold, like different names 
#   think about what to show and what order?
#   think about tidying dataset enteries, full name space full year...
#   think about extra info, e.g. data type (ldv, hdv, etc) others?
#think about an import_featmeta or get_featmeta function? 
#think about a "all:la" option 
#   will need some thinking
#think about returning doi for datasets when importing 
#   then maybe remove doi from table in documentation
   

#splatted function
#' @export
import_featdata <- 
function(featdata = NULL, 
         simplify.names = TRUE,
         ...){
    #initial featdata importer is by reference 
    #from a look up table
    
    if(!is.character(featdata)){
        stop("`featdata` missing, please set (see ?import_featdata)")
    }
    
    ref <- dtan_featdata_getref(featdata)
    
    ######################
    #got to be a nicer and quicker
    #way to do this
    ######################
    test <- dir()
    temp <- paste(getwd(), "/test.zip", sep="")
    download.file(paste(ref$source, "/", ref$zipfile, ".zip", sep=""),temp, method="libcurl",
                  quiet=TRUE)
    #download.file(test$link,temp, method="libcurl",
    #              quiet=TRUE)
    #add gary thanks measure..?
    unzip(temp)
    out <- foreign::read.dbf(paste(ref$zipfile, ".dbf", sep=""), 
                             as.is=TRUE)
    test2 <- dir()
    test2 <- test2[!test2 %in% test]
    file.remove(test2)
    unlink(temp)
    #might want to think about position of this step
    if(simplify.names){
      names(out) <- gsub("_", ".", tolower(names(out)))
    }
    out
}

#################################
#dtan_featdata_getref

#local function, currently not exporting this

#currently featdata should be known feat dataset 
#featdata = NULL returns full meta file
#featdata = partial match to 1 returns it
#

dtan_featdata_getref <- function(featdata=NULL){
  
  ############################
  #I know this is a little weird 
  #BUT it makes adding new records, 
  #tracking what is what and 
  #spotting mistakes, etc a lot 
  #easier
  ##############################
  
  #if adding new row, do before end stop, same format as others
  #if adding new column, do and one to ncol at end 

    ref <- as.data.frame(matrix(c(
    
    #1
    "Tucson 94",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_az/article/1001/type/native/viewcontent/",
    "10.1021/Es950191j",
    "Tucson_94",
    
    #2
    "Phnx 98", 
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_az/article/1000/type/native/viewcontent/",
    "10.1021/es702413b",
    "Phnx_98",
    
    #3
    "Phnx 99",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_az/article/1000/type/native/viewcontent/",
    "10.1021/es702413b",
    "Phnx_99",
    
    #4
    "AZ 2000",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_az/article/1000/type/native/viewcontent/",
    "10.1021/es702413b",
    "AZ_2000",
    
    #5
    "AZ 2002",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_az/article/1000/type/native/viewcontent/",
    "10.1021/es702413b",
    "AZ_2002",
    
    #6
    "AZ 2004",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_az/article/1000/type/native/viewcontent/",
    "10.1021/es702413b",
    "AZ_2004",
    
    #7
    "AZ 2006",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_az/article/1000/type/native/viewcontent/",
    "10.1021/es702413b",
    "AZ_2006", 
    
    #8
    "Fresno 08",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1000/type/native/viewcontent/",
    "10.1021/acs.est.0c05433",
    "Fresno08",
    
    #9
    "Fresno 21",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1000/type/native/viewcontent/",
    "10.1021/acs.est.0c05433",
    "Fresno21",
    
    #10
    "Cal 89",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1001/type/native/viewcontent/",
    "10.1126/science.268.5213.991",
    "Cal_89",
    
    #11
    "Cal 91",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1001/type/native/viewcontent/",
    "10.1126/science.268.5213.991",
    "Cal_91", 
    
    #12
    "LA710 99",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1002/type/native/viewcontent/",
    "-",
    "LA710_99",
    
    #13
    "Lynwd105 2018",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1004/type/native/viewcontent/",
    "10.1080/10962247.2019.1611677",
    "Lynwd105_2018",
    
    #14
    "Lynwd710 2018",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1004/type/native/viewcontent/", 
    "10.1080/10962247.2019.1611677",
    "Lynwd710_2018",
    
    #15
    "LA 99",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1005/type/native/viewcontent/",
    "10.1021/es026340x",
    "LA_99",
    
    #16
    "LA 00",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1005/type/native/viewcontent/",
    "10.1021/es026340x",
    "LA_00",
    
    #17
    "LA 01",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1005/type/native/viewcontent/",
    "10.1021/es026340x",
    "LA_01", 
    
    #18
    "Smnto 99",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1006/type/native/viewcontent/",
    "-",
    "Smnto_99",
    
    #19
    "Sanjose 99",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1007/type/native/viewcontent/",
    "10.1021/acs.est.0c05433; 10.1021/acs.est.0c05433", 
    "Sanjose99",
    
    #20
    "Sanjose 08",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1007/type/native/viewcontent/",
    "10.1021/acs.est.0c05433; 10.1021/acs.est.0c05433",
    "Sanjose08",
    
    #21
    "Vannuys 10",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1008/type/native/viewcontent/",
    "10.1080/10962247.2012.699015; 10.1080/10962247.2012.699016",
    "Vannuys10",
    
    #22
    "Labrea 01",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1003/type/native/viewcontent/",
    "10.1080/10962247.2020.1869121",
    "Labrea01",
    
    #23
    "Labrea 03",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1003/type/native/viewcontent/",
    "10.1021/es702413b",
    "Labrea03",
    
    #24
    "Labrea 05",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1003/type/native/viewcontent/",
    "10.1021/acs.est.0c05433",
    "Labrea05",
    
    #25
    "Labrea 08",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1003/type/native/viewcontent/",
    "10.1021/es5043518",
    "Labrea08",
    
    #26
    "Labrea 13",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1003/type/native/viewcontent/",
    "10.1021/acs.est.5b02392",
    "Labrea13",
    
    #27
    "Labrea 15",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1003/type/native/viewcontent/",
    "10.1021/acs.est.6b00717",
    "Labrea15",
    
    #28
    "Labrea 18",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1003/type/native/viewcontent/",
    "10.1021/acs.est.0c05433",
    "Labrea18", 
    
    #29
    "Labrea 99",
    "light duty vehicles",
    "https://digitalcommons.du.edu/context/feat_light_us_ca/article/1003/type/native/viewcontent/",
    "10.1080/10962247.2019.1611677",
    "Labrea99",
    
    
    ######################
    #more to do on ldv
    ######################

    #30
    "Peralta 97",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1000/type/native/viewcontent",
    "10.1021/Es001533a",
    "Peralta_1997",
    
    #31
    "Peralta 08",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1000/type/native/viewcontent",
    "10.1021/es202392g",
    "Peralta_2008",

    #32
    "Peralta 09",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1000/type/native/viewcontent",
    "10.1021/es202392g",
    "Peralta_2009",
    
    #33
    "Peralta 10",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1000/type/native/viewcontent",
    "10.1021/es202392g",
    "Peralta_2010",
    
    #34
    "Peralta 12",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1000/type/native/viewcontent",
    "10.1021/es401487b",
    "Peralta_2012",
    
    #35
    "Peralta 17",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1000/type/native/viewcontent",
    "10.1021/acs.est.8b03994",
    "Peralta_2017",
    
    #port of la
    #36
    "laport 08",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1001/type/native/viewcontent",
    "10.1021/es202392g, 10.1021/es401487b",
    "laport_2008",
    
    #37
    "laport 09",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1001/type/native/viewcontent",
    "10.1021/es202392g, 10.1021/es401487b",
    "laport_2009",
    
    #38
    "laport 10",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1001/type/native/viewcontent",
    "10.1021/es202392g, 10.1021/es401487b",
    "laport_2008",
    
    #39
    "laport 12",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1001/type/native/viewcontent",
    "10.1021/es202392g, 10.1021/es401487b",
    "laport_2012",
    
    #coors
    
    #40
    "coors 99",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1002/type/native/viewcontent",
    "10.1021/Es001533a",
    "coors_1999",
    
    #41
    "coors 05",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1002/type/native/viewcontent",
    "10.1021/es060989a",
    "coors_2005",
    
    #dumont
    
    #42
    "dumont 99",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1003/type/native/viewcontent",
    "10.1021/Es001533a",
    "dumont_1999",
    
    #43
    "dumont 05",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1003/type/native/viewcontent",
    "10.1021/es060989a",
    "dumont_2005",
    
    #switzerland
    
    #43
    "uri 98",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1004/type/native/viewcontent",
    "10.1021/Es001533a",
    "uri_1998",
    
    #texas port of houston
    
    #44
    "pohouston 09",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1005/type/native/viewcontent",
    "-",
    "pohouston_2009",
    
    
    #texas san marcos weight station
    
    #45
    "sanmarcos 98",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1006/type/native/viewcontent",
    "10.1021/Es001533a",
    "san_marcos_1998",
    
    #utah perry port
    
    #46
    "utah 20",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyVM/article/1007/type/native/viewcontent",
    "10.1021/Es001533a",
    "utah_2020",
    
    #cottonwood ohms
    
    #47
    "cottonwood 13",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyohms/article/1000/type/native/viewcontent",
    "10.1021/es505534e",
    "cwood13_web_v3",

    #48
    "cottonwood 15",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyohms/article/1000/type/native/viewcontent",
    "10.1021/acs.est.6b06172",
    "cwood15_web_v4",
  
    #49
    "cottonwood 17",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyohms/article/1000/type/native/viewcontent",
    "10.1021/acs.est.8b00621",
    "cwood17_web_v5",
    

    #50
    "pola 13",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyohms/article/1001/type/native/viewcontent",
    "10.1021/es505534e",
    "pola13_web_v2",
    
    
    #51
    "pola 15",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyohms/article/1001/type/native/viewcontent",
    "10.1021/acs.est.6b06172",
    "pola15_web_v6",
    
    #52
    "pola 17",
    "heavy duty vehicles",
    "https://digitalcommons.du.edu/context/feat_heavyohms/article/1001/type/native/viewcontent",
    "10.1021/acs.est.8b00621",
    "pola_2017_web_v4",
    
    
      
    #https://digitalcommons.du.edu/feat/
        
    #to do 
    #ldvs (colorado onwards)
    #non road
    
    
    ##XXXX
    ##following used as end stop
    
    "data set",
    "data type",
    "data source",
    "ref url",
    "zip file"
  ), ncol =5, byrow=TRUE))
  ##remove end stop
  ref <- ref[1:(nrow(ref)-1),]
  names(ref) <- c("dataset", "datatype", "source", "ref", "zipfile")
  
  if(is.null(featdata)){
    return(ref)
  }
  
  ref1 <- gsub("_|[ ]", "", tolower(featdata))
  ref2 <- gsub("_|[ ]", "", tolower(ref$dataset))
  test <- grep(ref1, ref2)
  if(length(test)<1){
    stop("import_featdata(...)\n",
         "`featdata` unknown, please see ?import_featdata",
         call. = TRUE)
  }
  if(length(test)>1){
    stop("import_featdata(...)\n",
         "`featdata` has more than one match, please select from\n",
         paste(ref[test,"dataset"], collapse=",", sep=","),
         call. = FALSE) 
  }
  return(ref[test,])
}


###############################
#dtan_tabular

#local function

#modified from 
#https://rdrr.io/github/ejanalysis/analyze.stuff/src/R/tabular.R
dtan_tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))
  
  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))
  
  cols <- lapply(df, format, ...)
  titles <- paste(paste(names(df), collapse=" \\tab "), "\\cr\n#'", sep="")
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n#'  ")))
  
  cat( paste("#' \\tabular{", paste(col_align, collapse = ""), "}{\n#'  ",
             titles, "\n#' ", contents, "\n#' }\n", sep = "")
  )
}


#test 
#import_featdata("Tucson_94")

#to build table in help
#dtan_tabular(dtan_featdata_getref()[c(-3, -5)]) 
