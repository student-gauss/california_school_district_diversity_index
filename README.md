---
Title: California School District Ethnic Diversity
---
# Introduction

Hello World.


```r
library(RCurl)
library(RJSONIO)
library(plyr)
library(ggmap)
library(ggthemes)

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

print(head(record_def))
```

```
##   index colname      type fieldsize
## 1     1     CDS Character        14
## 2     2   RTYPE Character         1
## 3     3   STYPE Character         1
## 4     4    SPED Character         1
## 5     5    SIZE Character         1
## 6     6 CHARTER Character         1
##                                                                                                                               description
## 1                                                                                                             County/District/School code
## 2                                                                                              Record Type: D=District, S=School, X=State
## 3 Type: 1=Unified, 2=Elementary District, 3=9-12 High District, 4=7-12 High District, E=Elementary School, M=Middle School, H=High School
## 4                   A= Alternative Schools Accountability Model (ASAM), E=Special Education, and C=Combination ASAM and Special Education
## 5                                                                           S=Small (11-99 Valid API Scores), T=Under 11 Valid API Scores
## 6                                                       Y=Charter, Not Direct Funded, D=Direct Funded Charter, Blank=Not a Charter School
```


