---
Title: California School District Ethnic Diversity
---
# Introduction



# Analysis

## Load needed libraries

These libraries are loaded.


```r
library(ggmap)
library(ggthemes)

races <- c("AA", "AI", "AS", "FI", "HI", "PI", "WH", "MR")
```
## Download data files

```r
download_data_file <- function() {
    ## Download 2013 Growth API file
    download.file(url="http://www3.cde.ca.gov/researchfiles/api/api13gtx.zip",
                  destfile="api13gtx.zip")
    ## Unzip it.
    unzip(zipfile="api13gtx.zip")
}
```
## Prepare Record Layout file

The record layout is defined in http://www.cde.ca.gov/ta/ac/ap/reclayout13g.asp. I converted the HTML table to CSV file and saved it as record_def.csv. Most importantly, the file contains the field length which is needed to parse the fixed-width record format.



```r
record_layout <- function() {
    record_def <- read.csv("record_def.csv", header=FALSE)
    colnames(record_def) <- c("index",
                              "colname",
                              "type",
                              "fieldsize",
                              "description")

    ## Set some columns' class.
    numeric_column <-
        c("VALID",
          "API13",
          unlist(lapply(c("NUM", "API13"), function(score) paste(races, score, sep="_") ) ) )
    character_column <- c("SNAME")

    column_classes <- rbind(
        data.frame( colname=numeric_column, class="numeric", stringsAsFactors=FALSE),
        data.frame( colname=character_column, class="character", stringsAsFactors=FALSE))

    record_definition_table <- merge(record_def,
                                     column_classes,
                                     by="colname",
                                     all=TRUE,
                                     sort=FALSE)

    record_definition_table <- record_definition_table[order(record_definition_table$index),]
    record_definition_table[which(is.na(record_definition_table$class)), "class"] <- "character"

    return(record_definition_table)
}
```

## Load Data File

```r
district_data_frame <- function() {
    download_data_file()
    
    record_definition_table <- record_layout()
    
    api13gtx <- read.fwf("api13gtx.txt",
                         width=record_definition_table$fieldsize,
                         col.names=record_definition_table$colname,
                         colClasses=record_definition_table$class, comment.char="")
    
    ## Clean up NA
    colnames_to_use_0_for_na <- c(paste(races, "NUM", sep="_"), "VALID")
    for(colname in colnames_to_use_0_for_na) {
        api13gtx[is.na(api13gtx[,colname]), colname] <- 0
    }
    
    district <- api13gtx[api13gtx$RTYPE == "D",]
    
    return(district)
}
```

## Google Map API


```r
geo_code <- function(address) {
    library(RCurl)
    library(RJSONIO)

    google_map_url <- function(address, return.call = "json", sensor = "false") {
        root <- "http://maps.google.com/maps/api/geocode/"
        u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
        return(URLencode(u))
    }

    u <- google_map_url(address)
    doc <- RCurl::getURL(u)
    x <- RJSONIO::fromJSON(doc,simplify = FALSE)

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

## Search the location


```r
add_geo <- function( district ) {
    geo_data <- sapply(district$DNAME, function (district_name) {
        Sys.sleep(0.5)
        name <- paste(district_name, ", California" )
        geo <- geo_code( name )

        ## print(paste(name, geo[4], as.numeric(geo[1]), as.numeric(geo[2])))
        c(DNAME.clean=geo[4],
          latitude=as.numeric(geo[1]),
          longitude=as.numeric(geo[2]))
    })
    district <- cbind(district, t(geo_data))
    district$latitude <- as.numeric(as.character(district$latitude))
    district$longitude <- as.numeric(as.character(district$longitude))

    return(district)
}
```



## Calculate Shannon Index


```r
add_shannon_index <- function(district) {
    shannon.index <- function(populations) {
        populations <- as.numeric(populations)
        total <- sum(populations)
        ratio <- populations / total
        entropies <- sapply( ratio, function(p) {
            p * ifelse(p > 0, log(p, 2), 0)
        } )
        return(-sum( entropies ))
    }
    
    district$shannon_index <- apply(district, 1, function(record) {
        shannon.index( record[paste(races, "NUM", sep="_")])
    })
    return(district)
}
```

## Plot the map


```r
if( !file.exists("district.RData") ) {
    district <- district_data_frame()
    district <- add_shannon_index(district)
    district <- add_geo(district)
    save(district, file="district.RData")
} else {
    load("district.RData")
}

map <- get_map(location = 'Santa Clara, California', zoom=9)

map_plots <- ggmap(map)
map_plots <- map_plots + geom_point(data=district, aes(x=longitude, y=latitude, color=shannon_index), alpha=1.0, size=3)
map_plots <- map_plots + scale_colour_gradient(name="Shannon Index", low="darkgreen", high="red")
map_plots <- map_plots + theme_map()
map_plots <- map_plots + theme(legend.position="right" )
map_plots
```

```
## Warning: Removed 863 rows containing missing values (geom_point).
```

![plot of chunk gmap](figure/gmap-1.png) 
