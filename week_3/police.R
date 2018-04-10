library(jsonlite)
file <- fromJSON( txt = "NPA_TMA2_JSON.json" )
doc <- data.frame(matrix(unlist(file[["result"]][["records"]]), nrow = ))
