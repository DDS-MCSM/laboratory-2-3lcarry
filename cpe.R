#******************************************************************************#
#                                                                              #
#                          Lab 2 - CPE Standard                                #
#                                                                              #
#              Arnau Sangra Rocamora - Data Driven Securty                     #
#                                                                              #
#******************************************************************************#

if(!require("xml2")){
  install.packages("xml2")
  library("xml2")
}

if(!require("tidyr")){
  install.packages("tidyr")
  library("tidyr")
}


cpe.file <- "./data/official-cpe-dictionary_v2.3.xml"

DownloadCPEFile <- function()
{
  dir.create("./tmp")
  compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
  cpes_filename <- "./tmp/cpes.zip"
  download.file(compressed_cpes_url, cpes_filename)
  unzip(zipfile = cpes_filename, exdir = "./data")
  unlink("./tmp", recursive = TRUE)
}

GetCPEItems <- function(xmlDocument) {
  cpe.raw <- xml2::xml_find_all(xmlDocument, "//d1:cpe-item")
  cpe.titleEN <- xml_text(xml_find_all(cpe.raw, "./d1:title[@xml:lang='en-US']/text()"))
  cpe.name <- xml_text(xml_find_all(cpe.raw, "./@name"))
  cpe.cpe23 <- xml_text(xml_find_all(cpe.raw, "./cpe-23:cpe23-item/@name"))

  # transform the list to data frame
  df <- data.frame(title = cpe.titleEN,
                   cpe23 = cpe.cpe23,
                   name = cpe.name,
                   stringsAsFactors = F)


  # return data frame
  return(df);
}


CleanCPEs <- function(cpes){
  # data manipulation
  #Clean cpe 23 name to separate properties
  cpeColumns <- c("standard", "cpeversion", "part",
                  "vendor", "product","version",
                  "update", "edition", "language",
                  "sw_edition","target_sw", "target_h",
                  "other")

  cpes <- tidyr::separate(data = cpes, col = cpe23, into = cpeColumns, sep = "(?<=[^\\\\]):", remove = F)
  cpes$standard <- as.factor(cpes$standard)
  cpes$cpeversion <- as.factor(cpes$cpeversion)
  cpes$part <- as.factor(cpes$part)
  cpes$vendor <- as.factor(cpes$vendor)
  cpes$language <- as.factor(cpes$language)
  cpes$target_sw <- as.factor(cpes$target_sw)
  cpes$target_h <- as.factor(cpes$target_h)
  cpes$product <- as.factor(cpes$product)

  return(cpes)
}

ParseCPEData <- function() {
  #Download cpe file if not extis
  if (!file.exists(cpe.file))
  {
    DownloadCPEFile()
  }

  # load cpes as xml file
  cpes <- xml2::read_xml(x = cpe.file)
  # get CPEs
  cpes <- GetCPEItems(cpes)
  # transform, clean, arrange parsed cpes as data frame
  df <- CleanCPEs(cpes)

  # return data frame
  return(df)
}


