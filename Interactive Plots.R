library(tidyverse)
library(htmlwidgets)
library(plotly)
library(ggplot2)

listing <-read.csv('C:/Users/victo/OneDrive/Desktop/MSDA/Data Viz Class/Datasets/Listing.csv', stringsAsFactors=FALSE)
view(listing)

#Converting datatypes to data types (Factors) we want for the analysis

listing$GreenMarkRating <- as.factor(listing$GreenMarkRating)

listing$GreenMarkStatus <- as.factor(listing$GreenMarkStatus)

listing$BuildingType <- as.factor(listing$BuildingType)

#listing$BuildingSize <- as.factor(listing$BuildingSize)  

#listing$GrossFloorArea <-as.numeric(gsub(",", "", listing$GrossFloorArea))

# Checking how many NaNs are in each column 

colSums(is.na(listing))

#Extract Street names and removing all the block numbers 

listing$Street <- as.factor(gsub("(^ *,)|(\\,.*)", "", listing$BuildingAddress))
listing$Street
listing$Street <- as.factor(gsub('[0-9]+', '', listing$Street))   
listing$Street

# Change Intensity is the difference between 2018 and  2017 Energy Use Intensity

listing$ChangeIntensity <- listing$X2018EnergyuseIntensity - listing$X2017EnergyuseIntensity
listing$ChangeIntensity

# Total Energy Use Intensity =  Energy Use Intensity in 2017 + Energy Use Intensity in 2018
listing$TotalIntensity <- listing$X2018EnergyuseIntensity + listing$X2017EnergyuseIntensity
listing$TotalIntensity
#Buildings with Highest Increased Energy Consumption
Bld <- listing%>% filter(!is.na(BuildingName))
Bld1 <- Bld[with(Bld,order(-ChangeIntensity)),]
Bld1 <- Bld1[1:10,]
Bld1

K<- ggplot(Bld1, aes(x = reorder(BuildingName,ChangeIntensity), y = ChangeIntensity)) + 
  geom_bar(stat="identity",fill = "#00abff", color = "#00abff") + 
  labs(title="Buildings with Increased Energy Use Intensity") + 
  xlab("") + 
  ylab("ChangeIntensity") + 
  theme_bw()+
  coord_flip()

ggplotly(K)

p <- plot_ly(x = 1:10, y = 1:10) %>% add_markers()
widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}
widget_file_size(p)
widget_file_size(partial_bundle(p))

#Buildings with Highest Increased Energy Consumption

Bld2 <- Bld[with(Bld,order(ChangeIntensity)),]
Bld2 <- Bld2[1:10,]
Bld2

K1<- ggplot(Bld2, aes(x = reorder(BuildingName,ChangeIntensity), y = ChangeIntensity)) + 
  geom_bar(stat="identity",fill = "#00abff", color = "#00abff") + 
  labs(title="Buildings with Decreased Energy Use Intensity") + 
  xlab("") + 
  ylab("ChangeIntensity") + 
  theme_bw()+
  coord_flip()

ggplotly(K1)
