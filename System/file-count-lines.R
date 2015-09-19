# Summarises file lines for each file in directory
file_list <- list.files(path = ".", pattern = "R")
file_summary <- sapply(file_list, function(filename) {
    length(readLines(filename))
})

file_summary