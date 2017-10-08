setwd(dir = '~/kulbir_R_project/data_cleaning/')
dirtyData <- read.csv('dirty_data.csv', stringsAsFactors = F, header = T)
str(dirtyData)
head(dirtyData)
summary(dirtyData)

# 1. Populate missing values in Area

cell <- read.xlsx("cell.xlsx", sheetName = "Sheet1" )
apply(cell, 1, function(x){
  dirtyData$Area[ as.numeric(x[1]): as.numeric(x[2])-1] <<- x[3]
})

# 2. Remove special characters and  trim white spaces

dirtyData$Street <- trimws(dirtyData$Street)
dirtyData$Street.2 <- trimws(dirtyData$Street.2)

dirtyData$Street <- iconv( dirtyData$Street, to = "ASCII//TRANSLIT")
dirtyData$Street <- gsub("[]['/!#$%()*,.:;<=>@^_`|~.{}]", " ", dirtyData$Street)

dirtyData$Street.2 <- iconv( dirtyData$Street, to = "ASCII//TRANSLIT")
dirtyData$Street.2 <- gsub("[]['/!#$%()*,.:;<=>@^_`|~.{}]", " ", dirtyData$Street)

# 3. Remove duplicates of street in street2
duplicates_Street.2 <- which(dirtyData$Street == dirtyData$Street.2)
dirtyData$Street.2[duplicates_Street.2] <- ""

# 4. Capitalize first letter
library(stringr)
dirtyData$Street <- str_to_title(dirtyData$Street)

# 5. Adding Prefix
streetWithPrefix <- lapply( dirtyData$Street, function(x){
    
  if ( grepl("street", x) == TRUE | grepl("Street", x) == TRUE ) {
    x <- paste0("str.",x)
    return (x)
  }
  else if( grepl("road", x) == TRUE | grepl("Road", x) == TRUE ) {
    x <- paste0("road.",x)
    return (x)
  }
  else if( grepl("lane", x) == TRUE | grepl("Lane", x) == TRUE ) {
    x <- paste0("lane.",x)
    return (x)
  }
  else if( grepl("avenue", x) == TRUE | grepl("Avenue", x) == TRUE ) {
    x <- paste0("ave.",x)
    return (x)
  }
  else if( grepl("park", x) == TRUE | grepl("Park", x) == TRUE ) {
    x <- paste0("prk.",x)
    return (x)
  }
  else if( grepl("station", x) == TRUE | grepl("Station", x) == TRUE ) {
    x <- paste0("sta.",x)
    return (x)
  }
  else{
    return (x)
  }
})

dirtyData$Street <- unlist(streetWithPrefix)

# 6. Remove column Strange.HTML
dirtyData <- dirtyData[ , -c(5)]

write.csv(file = "clean_data.csv", x = dirtyData, row.names = F)