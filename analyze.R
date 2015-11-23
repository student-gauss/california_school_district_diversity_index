library(RCurl)
library(RJSONIO)
library(plyr)
library(ggmap)
library(ggthemes)
library(viridis)

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
        Sys.sleep(0.5)
    } else {
        return(c(NA,NA,NA, NA))
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

races <- c("AA", "AI", "AS", "FI", "HI", "PI", "WH", "MR")

numeric_column <- c("VALID",
                    "API13",
                    unlist(lapply(c("NUM", "API13"), function(score) paste(races, score, sep="_") ) )
                    )

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

api13gtx <- read.fwf("api13gtx.txt",
                     width=record_definition_table$fieldsize,
                     col.names=record_definition_table$colname,
                     colClasses=record_definition_table$class, comment.char="")

## Clean up NA
colnames_to_use_0_for_na <- c(paste(races, "NUM", sep="_"), "VALID")
for(colname in colnames_to_use_0_for_na) {
    api13gtx[is.na(api13gtx[,colname]), colname] <- 0
}

school <- api13gtx[api13gtx$RTYPE == "S",]
district <- api13gtx[api13gtx$RTYPE == "D",]
state <- api13gtx[api13gtx$RTYPE == "X",]

shannon.index <- function(populations) {
    populations <- as.numeric(populations)
    total <- sum(populations)
    ratio <- populations / total
    entropies <- sapply( ratio, function(p) {
        p * ifelse(p > 0, log(p), 0)
    } )
    return(-sum( entropies ))
}

district$shannon_index <- apply(district, 1, function(record) {
    shannon.index( record[paste(races, "NUM", sep="_")])
})

school$shannon_index <- apply(school, 1, function(record) {
    shannon.index( record[paste(races, "NUM", sep="_")])
})

district$random <- runif(dim(district)[1])

sub.district <- subset(district, random < 0.3)
geo_data <- sapply(sub.district$DNAME, function (district_name) {
    Sys.sleep(0.5)
    name <- paste(district_name, ", California" )
    geo <- geo_code( name )

    print(paste(name, as.numeric(geo[1]), as.numeric(geo[2])))
    c(DNAME.clean=geo[4],
      latitude=as.numeric(geo[1]),
      longitude=as.numeric(geo[2]))
})

sub.district <- cbind(sub.district, t(geo_data))
sub.district$latitude <- as.numeric(as.character(sub.district$latitude))
sub.district$longitude <- as.numeric(as.character(sub.district$longitude))

map <- get_map(location = 'California', zoom=6)

mapPoints <- ggmap(map)
mapPoints <- mapPoints + geom_point(data=sub.district, aes(x=longitude, y=latitude, color=shannon_index), alpha=0.50, size=5)
mapPoints <- mapPoints + scale_colour_gradient(name="Shannon Index", low="blue", high="red")
mapPoints <- mapPoints + theme_map()
mapPoints <- mapPoints + theme(legend.position="right" )

## api13gtx$AS_API13.clean <- as.numeric(api13gtx$AS_API13)
## api13gtx[,numeric_column] <- as.numeric(unlist(api13gtx[,numeric_column]))