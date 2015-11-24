---
Title: California School District Ethnic Diversity
---
# Introduction



# Analysis

## Load needed libraries

These libraries are loaded.


```r
library(RCurl)
library(RJSONIO)
library(ggmap)
library(ggthemes)
```
## Download data files

```r
## Download California API score data file for 2013
download.file(url="http://www3.cde.ca.gov/researchfiles/api/api13gtx.zip",
              destfile="api13gtx.zip")

## Unzip it.
unzip(zipfile="api13gtx.zip")
```
## Load Record Layout file


```r
record_def <- read.csv("record_def.csv", header=FALSE)
colnames(record_def) <- c("index",
                          "colname",
                          "type",
                          "fieldsize",
                          "description")
```




```r
google_map_url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geo_code <- function(address,verbose=FALSE) {
    if(verbose) {
        cat(address,"\n")
    }
    u <- google_map_url(address)
    doc <- getURL(u)
    x <- fromJSON(doc,simplify = FALSE)
    
    if(x$status=="OK") {
        lat <- x$results[[1]]$geometry$location$lat
        lng <- x$results[[1]]$geometry$location$lng
        location_type  <- x$results[[1]]$geometry$location_type
        formatted_address  <- x$results[[1]]$formatted_address
        return(c(lat, lng, location_type, formatted_address))
    } else {
        return(c(NA, NA, NA, NA))
    }
}

download.file(url="http://www3.cde.ca.gov/researchfiles/api/api13gtx.zip",
              destfile="api13gtx.zip")
unzip(zipfile="api13gtx.zip")

record_def <- read.csv("record_def.csv", header=FALSE)
colnames(record_def) <- c("index",
                          "colname",
                          "type",
                          "fieldsize",
                          "description")
```


