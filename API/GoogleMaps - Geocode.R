require(plyr); require(XML); require(RCurl)

# need to set proxy options if applicable

GoogleGeoCode <- function(address, type = "short", CoOrdsOnly = TRUE){
    # clean url
    address <- gsub(pattern=" ", replacement="%20", x=address)
    link <- paste("http://maps.googleapis.com/maps/api/geocode/xml?address=", address, "&sensor=false", sep="")
    
    # send request
    htmlDocParsed <- htmlParse(getURL(link, .opts = opts))
    result <- data.frame(lon = NA, lat = NA)
    
    # parse result
    if (!CoOrdsOnly) {
        address.type <- xpathSApply(htmlDocParsed, "//address_component//type[1]", xmlValue)
        ifelse (type == "short",
                address.result <- xpathSApply(htmlDocParsed, "//address_component//short_name[1]", xmlValue),
                address.result <- xpathSApply(htmlDocParsed, "//address_component//long_name[1]", xmlValue)
        )
        names(address.result) <- address.type
        
        result <- data.frame(t(address.result))[1, ]
    }
    result$lon <- xpathSApply(htmlDocParsed, "//geometry/location/lng[1]", xmlValue)[1]
    result$lat <- xpathSApply(htmlDocParsed, "//geometry/location/lat[1]", xmlValue)[1]
    
    if (nrow(result) == 0) {result[1, ] <- NA}
    return(result)    
}