# code won't run unless these are unloaded first

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

detach_package("data.world", TRUE)
detach_package("tidyverse", TRUE)
detach_package("readr", TRUE)
detach_package("plyr", TRUE)
detach_package("leaflet.providers", TRUE)
detach_package("Hmisc", TRUE)
detach_package("colourvalues", TRUE)
detach_package("leafgl", TRUE)
detach_package("reactable", TRUE)
detach_package("bsselectR", TRUE)
detach_package("rmarkdown", TRUE)
detach_package("rjson", TRUE)
detach_package("RJSONIO", TRUE)
detach_package("plotly", TRUE)
detach_package("ggplot2", TRUE)
detach_package("rlist", TRUE)
detach_package("lubridate", TRUE)
detach_package("jsonlite", TRUE)

# add a few packages back in

library(rvest)
library(rjson)
library(httr)
library(stringr)
library(googlesheets4)

#replace the hostname and the path if necessary
#host_url <- "https://public.tableau.com"
#path <- "/views/COVID-19VaccineDashboardPublic/Vaccine"

#body <- read_html(modify_url(host_url, 
#                             path = path, 
#                             query = list(":embed" = "y",":showVizHome" = "no")
#))

url <- "https://public.tableau.com/views/COVID-19VaccineDashboardPublicv2/Vaccine?:embed=y&:showVizHome=no&:host_url=https://public.tableau.com/&:embed_code_version=3&:tabs=no&:toolbar=yes&:animate_transition=yes&:display_static_image=no&:display_spinner=no&:display_overlay=yes&:display_count=yes&:language=en&publish=yes&:loadOrderID=0"

body <- read_html(url)

data <- body %>% 
  html_nodes("textarea#tsConfigContainer") %>% 
  html_text()
json <- fromJSON(data)

url <- modify_url(url, path = paste(json$vizql_root, "/bootstrapSession/sessions/", json$sessionid, sep =""))

resp <- POST(url, body = list(sheet_id = json$sheetId), encode = "form")
data <- content(resp, "text")

extract <- str_match(data, "\\d+;(\\{.*\\})\\d+;(\\{.*\\})")
info <- fromJSON(extract[1,1])
data <- fromJSON(extract[1,3])

worksheets = names(data$secondaryInfo$presModelMap$vizData$presModelHolder$genPresModelMapPresModel$presModelMap)

for(i in 1:length(worksheets)){
  print(paste("[",i,"] ",worksheets[i], sep=""))
}
# selected <-  readline(prompt="select worksheet by index: ");
selected <- 1
worksheet <- worksheets[as.integer(selected)]
# print(paste("you selected :", worksheet, sep=" "))

columnsData <- data$secondaryInfo$presModelMap$vizData$presModelHolder$genPresModelMapPresModel$presModelMap[[worksheet]]$presModelHolder$genVizDataPresModel$paneColumnsData

i <- 1
result <- list();
for(t in columnsData$vizDataColumns){
  if (is.null(t[["fieldCaption"]]) == FALSE) {
    paneIndex <- t$paneIndices
    columnIndex <- t$columnIndices
    if (length(t$paneIndices) > 1){
      paneIndex <- t$paneIndices[1]
    }
    if (length(t$columnIndices) > 1){
      columnIndex <- t$columnIndices[1]
    }
    result[[i]] <- list(
      fieldCaption = t[["fieldCaption"]], 
      valueIndices = columnsData$paneColumnsList[[paneIndex + 1]]$vizPaneColumns[[columnIndex + 1]]$valueIndices,
      aliasIndices = columnsData$paneColumnsList[[paneIndex + 1]]$vizPaneColumns[[columnIndex + 1]]$aliasIndices, 
      dataType = t[["dataType"]],
      stringsAsFactors = FALSE
    )
    i <- i + 1
  }
}
dataFull = data$secondaryInfo$presModelMap$dataDictionary$presModelHolder$genDataDictionaryPresModel$dataSegments[["0"]]$dataColumns

cstring <- list();
for(t in dataFull) {
  if(t$dataType == "cstring"){
    cstring <- t
    break
  }
}
data_index <- 1
name_index <- 1
frameData <-  list()
frameNames <- c()
for(t in dataFull) {
  for(index in result) {
    if (t$dataType == index["dataType"]){
      if (length(index$valueIndices) > 0) {
        j <- 1
        vector <- character(length(index$valueIndices))
        for (it in index$valueIndices){
          vector[j] <- t$dataValues[it+1]
          j <- j + 1
        }
        frameData[[data_index]] <- vector
        frameNames[[name_index]] <- paste(index$fieldCaption, "value", sep="-")
        data_index <- data_index + 1
        name_index <- name_index + 1
      }
      if (length(index$aliasIndices) > 0) {
        j <- 1
        vector <- character(length(index$aliasIndices))
        for (it in index$aliasIndices){
          if (it >= 0){
            vector[j] <- t$dataValues[it+1]
          } else {
            vector[j] <- cstring$dataValues[abs(it)]
          }
          j <- j + 1
        }
        frameData[[data_index]] <- vector
        frameNames[[name_index]] <- paste(index$fieldCaption, "alias", sep="-")
        data_index <- data_index + 1
        name_index <- name_index + 1
      }
    }
  }
}

df <- NULL
lengthList <- c()
for(i in 1:length(frameNames)){
  lengthList[i] <- length(frameData[[i]])
}
max <- max(lengthList)
for(i in 1:length(frameNames)){
  if (length(frameData[[i]]) < max){
    len <- length(frameData[[i]])
    frameData[[i]][(len+1):max]<-""
  }
  df[frameNames[[i]]] <- frameData[i]
}
options(width = 1200)
df <- as.data.frame(df, stringsAsFactors = FALSE)
#print(df)


#______________GET DELIVERED NUMBER_______________

for(i in 1:length(worksheets)){
  print(paste("[",i,"] ",worksheets[i], sep=""))
}
# selected <-  readline(prompt="select worksheet by index: ");
selected <- 4
worksheet <- worksheets[as.integer(selected)]
# print(paste("you selected :", worksheet, sep=" "))

columnsData <- data$secondaryInfo$presModelMap$vizData$presModelHolder$genPresModelMapPresModel$presModelMap[[worksheet]]$presModelHolder$genVizDataPresModel$paneColumnsData

i <- 1
result <- list();
for(t in columnsData$vizDataColumns){
  if (is.null(t[["fieldCaption"]]) == FALSE) {
    paneIndex <- t$paneIndices
    columnIndex <- t$columnIndices
    if (length(t$paneIndices) > 1){
      paneIndex <- t$paneIndices[1]
    }
    if (length(t$columnIndices) > 1){
      columnIndex <- t$columnIndices[1]
    }
    result[[i]] <- list(
      fieldCaption = t[["fieldCaption"]], 
      valueIndices = columnsData$paneColumnsList[[paneIndex + 1]]$vizPaneColumns[[columnIndex + 1]]$valueIndices,
      aliasIndices = columnsData$paneColumnsList[[paneIndex + 1]]$vizPaneColumns[[columnIndex + 1]]$aliasIndices, 
      dataType = t[["dataType"]],
      stringsAsFactors = FALSE
    )
    i <- i + 1
  }
}
dataFull = data$secondaryInfo$presModelMap$dataDictionary$presModelHolder$genDataDictionaryPresModel$dataSegments[["0"]]$dataColumns

cstring <- list();
for(t in dataFull) {
  if(t$dataType == "cstring"){
    cstring <- t
    break
  }
}
data_index <- 1
name_index <- 1
frameData <-  list()
frameNames <- c()
for(t in dataFull) {
  for(index in result) {
    if (t$dataType == index["dataType"]){
      if (length(index$valueIndices) > 0) {
        j <- 1
        vector <- character(length(index$valueIndices))
        for (it in index$valueIndices){
          vector[j] <- t$dataValues[it+1]
          j <- j + 1
        }
        frameData[[data_index]] <- vector
        frameNames[[name_index]] <- paste(index$fieldCaption, "value", sep="-")
        data_index <- data_index + 1
        name_index <- name_index + 1
      }
      if (length(index$aliasIndices) > 0) {
        j <- 1
        vector <- character(length(index$aliasIndices))
        for (it in index$aliasIndices){
          if (it >= 0){
            vector[j] <- t$dataValues[it+1]
          } else {
            vector[j] <- cstring$dataValues[abs(it)]
          }
          j <- j + 1
        }
        frameData[[data_index]] <- vector
        frameNames[[name_index]] <- paste(index$fieldCaption, "alias", sep="-")
        data_index <- data_index + 1
        name_index <- name_index + 1
      }
    }
  }
}

delivered_df <- NULL
lengthList <- c()
for(i in 1:length(frameNames)){
  lengthList[i] <- length(frameData[[i]])
}
max <- max(lengthList)
for(i in 1:length(frameNames)){
  if (length(frameData[[i]]) < max){
    len <- length(frameData[[i]])
    frameData[[i]][(len+1):max]<-""
  }
  delivered_df[frameNames[[i]]] <- frameData[i]
}
options(width = 1200)
delivered_df <- as.data.frame(delivered_df, stringsAsFactors = FALSE)
delivered_number <- delivered_df$SUM.Doses.Delivered..alias
#print(delivered_number)

library(data.world)
library(rgdal)
library(tigris)
library(tidyverse)
library(readr)
library(plyr)
library(stringr)
library(dplyr)
library(leaflet)
library(leaflet.providers)
library(rgdal)
library(htmlwidgets)
library(RCurl)
library(sp)
library(Hmisc)
library(stringr)
library(leafgl)
library(colourvalues)
library(sf)
library(leaflet.extras)
library(tigris)
library(reactable)
library(rmarkdown)
library(htmltools)
library(rjson)
library(RJSONIO)
library(ggplot2)
library(plotly)
library(rlist)
library(bsselectR)
library(lubridate)
library(jsonlite)

#add delivered number to the df
df$statewidedelivered <- NULL
df$statewidedelivered <- delivered_number

df$SUM.Dose.Administered..alias

#clean up headers
df_cleanheaders <- df %>% 
  dplyr::select(statewidedelivered, SUM.Dose.Administered..alias, SUM.COUNT..alias, SUM.Partially.Vaccinated..alias, SUM.Fully.Vaccinated..alias) %>% 
  dplyr::rename(statewideadministered = SUM.Dose.Administered..alias) %>%
  dplyr::rename(partiallyvaxed = SUM.Partially.Vaccinated..alias) %>%
  dplyr::rename(fullyvaxed = SUM.Fully.Vaccinated..alias) %>% 
  dplyr::rename(boostered = SUM.COUNT..alias)

#add in population
df_cleanheaders$population <- NULL
df_cleanheaders$population <- 39952356
df_cleanheaders$population_over16 <- 31822612

#get today's date and format it for a human to read it
today_date = format(Sys.Date(), "%Y-%m-%d")
today_date <- as.Date(today_date,'%Y-%m-%d')
today_Formated = gsub(" 0"," ", format(as.character(today_date, format="%B %d, %Y")))
#today_Formated

#get yesterday's date and format it for a human to read it
yesterday_date = format(Sys.Date()-1, "%Y-%m-%d")
yesterday_date <- as.Date(yesterday_date,'%Y-%m-%d')
yesterday_date_Formated = gsub(" 0"," ", format(as.character(yesterday_date, format="%B %d, %Y")))
#yesterday_date_Formated

#add dates to dataframe
df_cleanheaders$dataasof <- NULL
df_cleanheaders$dataasof <- yesterday_date_Formated

df_cleanheaders$siteupdated <- NULL
df_cleanheaders$siteupdated <- today_Formated

#order the columns how I want to
df_cleanheaders <- df_cleanheaders %>% 
  dplyr::select(dataasof, siteupdated, statewidedelivered, statewideadministered, partiallyvaxed, fullyvaxed, boostered, population, population_over16)
#View(df_cleanheaders)

# google authentication
gs4_auth(path = '~/R/CA-Vaccines/ca-state-vaccine-data-3334f2ba820b.json')

sheet_write(df_cleanheaders, ss = "1cx-bzmUbB1oULMC4NEDRU4iQsezzEu49bjVkqjyoiW0", sheet = "Sheet1")
