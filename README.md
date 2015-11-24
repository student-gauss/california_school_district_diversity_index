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
```


