require(RODBC)

table_names <- c("INFORMATION_SCHEMA.TABLES", "INFORMATION_SCHEMA.COLUMNS")
server_name <- "SERVERNAME"
username <- "USERNAME"
password <- "PASSWORD"
database <- "DATABASE"
access_db_location <- "C:/access.accdb"

# connect to db ----------------------------------------------------------------------- 
# connection string
dsn_string <- paste0("DRIVER={SQL Server}",
                     ";SERVER=", server_name,
                     ";UID=", username,
                     ";PWD=", password,
                     ";DATABASE=", database)

# open channel
channel <- odbcDriverConnect(dsn_string, readOnlyOptimize=TRUE)

# fetch required
tables_fetched <- lapply(table_names, function(x){
    df <- sqlFetch(channel=channel, sqtable=x)
    return(df)
})
names(tables_fetched) <- table_names

# close channel
odbcClose(channel)

# write ----------------------------------------------------------------------- 
# csv
lapply(table_names, function(x){write.csv(tables_fetched[[x]], paste(x, "csv", sep="."), row.names=FALSE, na="")})

# access db
channel_db <- odbcConnectAccess2007(access_db_location)
lapply(table_names, 
       function(tablename, channel=channel_db){
           vars <- sapply(sapply(tables_fetched[[tablename]], class),"[", 1)
           vars <- vars[vars %in% c("POSIXct", "Date")]
           vars[] <- "datetime"
           if (tablename %in% sqlTables(channel=channel_db)$TABLE_NAME) {sqlDrop(channel=channel_db, sqtable=tablename)}
           sqlSave(channel=channel_db, dat=tables_fetched[[tablename]], tablename=tablename, rownames=FALSE, varTypes=vars)
       }
)
odbcClose(channel_db)

# clean up ----------------------------------------------------------------------- 
odbcCloseAll()