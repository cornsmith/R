# Description -------------------------------------------------------------
# Suburb scroring using ABS variables
# Uses ABS raw text files
# Calculates distance against ideal for each of the suburbs
# Can use other geographic breakdowns where available


# Parameters --------------------------------------------------------------
setwd("//fileserver-au-01/Insights/Insights/ABS/2011")
# location of the ABS data files
datalocation <- "//fileserver-au-01/Insights/Insights/ABS/2011/2011 Census BCP State Suburbs for AUST/Seq"

# Retrieve ABS data -------------------------------------------------------
unique.id <- "region_id"
fields.required <- c(5335, 5365, 2562, 2709, 4832, 4852, 4866, 4822, 4866, 21, 24, 27, 3, 4995, 4998, 5001, 5019, 5004, 5007, 5010, 5019)
fields.required <- unique(paste0("B", fields.required))
fields.required <- c(unique.id, fields.required)

datafiles <- list.files(path=datalocation, full.names=TRUE)

# read first data file then merge the rest
dfrequired <- read.csv(datafiles[1])
dfrequired <- dfrequired[ , names(dfrequired) %in% fields.required]

for(i in 2:length(datafiles)){
    dftemp <- read.csv (datafiles[i])
    dftemp <- dftemp[, names(dftemp) %in% fields.required]
    if(is.data.frame(dftemp)){
        dfrequired <- merge(x=dfrequired, y=dftemp, by=unique.id, all=TRUE, sort=FALSE)
    }
}

# Meta-data ---------------------------------------------------------------
# dfmeta <- read.csv("metadata.csv")
# dfmeta <- dfmeta[dfmeta$Sequential %in% names(dfrequired),]

# Index calculation -------------------------------------------------------
dfindex <- data.frame(
    row.names       = dfrequired[,1],
    Broadband       = dfrequired$B5335 / dfrequired$B5365,
    English         = dfrequired$B2562 / dfrequired$B2709,
    Children        = (dfrequired$B4832 + dfrequired$B4852) / dfrequired$B4866,
    NoChildren      = dfrequired$B4822 / dfrequired$B4866,
    Age             = (dfrequired$B21 + dfrequired$B24 + dfrequired$B27) / dfrequired$B3,
    Income150       = (dfrequired$B5004 + dfrequired$B5007 + dfrequired$B5010) / dfrequired$B5019,
    Income80to150   = (dfrequired$B4995 + dfrequired$B4998 + dfrequired$B5001) / dfrequired$B5019
)

# Create the ideal dummy location
dfideal <- data.frame(
    row.names       = "Ideal",
    Broadband       = 1,
    English         = 1,
    Children        = 1,
    NoChildren      = 1,
    Age             = 1,
    Income150       = 1,
    Income80to150   = 1
)

# ideal location at the top
dfindex <- rbind(dfideal, dfindex)

# clean up indices
dfindex$Age[dfindex$Age==Inf] <- NA
dfindex <- na.omit(dfindex)

# standardize
dfzscores <- data.frame(scale(dfindex))

# Calculate distance ------------------------------------------------------
# define ideal variables
demo.gen.var <- c("Broadband", "English", "Age")
demo.1.var <- c(demo.gen.var, "Children","Income150")
demo.2.var <- c(demo.gen.var, "NoChildren","Income150")
demo.3.var <- c(demo.gen.var, "Children","Income80to150")
demo.4.var <- c(demo.gen.var, "NoChildren","Income80to150")

# distance against ideal
demo.1.dist <- as.matrix(dist(dfzscores[names(dfzscores) %in% demo.1.var]))[1,]
demo.2.dist <- as.matrix(dist(dfzscores[names(dfzscores) %in% demo.2.var]))[1,]
demo.3.dist <- as.matrix(dist(dfzscores[names(dfzscores) %in% demo.3.var]))[1,]
demo.4.dist <- as.matrix(dist(dfzscores[names(dfzscores) %in% demo.4.var]))[1,]

distance <- cbind(demo.1.dist, demo.2.dist, demo.3.dist, demo.4.dist)

# remove ideal
distance <- distance[-1,]

# Write output ------------------------------------------------------------
write.csv(dfrequired, "output/raw.csv")
write.csv(dfindex,"output/index.csv")
write.csv(dfzscores,"output/zscore.csv")
write.csv(distance,"output/distance.csv")