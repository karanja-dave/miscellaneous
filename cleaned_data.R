##set working directory and clear workspace
rm(list=ls())
setwd("Damma")
load("damma.Rdata")

##load packages
library(readr)
library(dplyr)
library(tidyverse) #set of packages of intrest
library(purrr)
library(jsonlite)
library(ggcorrplot)

##load data
data<-read_csv("damma.csv")

##dealing with null values
data %>% is.na() %>% colSums() %>% as.data.frame()
#drop cols with null counts >0.5 of the proportion
mask_na<-((data %>% is.na() %>% colSums())/nrow(data))>0.5
data<-data[,!mask_na]

#separate numeric and categorical var
num_vars<-data %>% select(where(is.numeric))
cate_vars<-data %>% select(where(is.character))

#extract lat and lon values from MAP_DETAILS
data[c("lat", "lon")]<-data$MAP_DETAILS %>%
  str_replace_all("'", "\"") %>%
  lapply(fromJSON) %>%
  bind_rows() %>%
  select(lat = LATITUDE, lon = LONGITUDE)

#dealing with high and low cardinality
#get count of each character vector
card_counts<-cate_vars %>%
  map_dbl(~length(unique(.)),.id=count) %>% 
  as.data.frame()%>%setNames("count")
#extract only cols with appropriate count
mask_card<-rownames(card_counts)[card_counts$count >= 2 &
                                   card_counts$count <= 4]
#drop cols with low and high card count
data_clean<- data %>%
  select(,AREA,lat,lon,where(is.numeric),all_of(mask_card))

#assigning correct datatypes
data_clean<-data_clean %>% mutate(PRICE=MIN_PRICE,
                      BUILDING_ID=as.character(BUILDING_ID),
                      TRANSACT_TYPE=as.factor(TRANSACT_TYPE),
                      OWNTYPE=as.factor(OWNTYPE),
                      FACING=as.factor(FACING),
                      lat=as.numeric(lat),
                      lon=as.numeric(lon),) %>% 
  #drop leakages (others ~leakages jua mbona nimedrop - Damma ignore hii comment) 
  select(-c(PRICE_PER_UNIT_AREA,CARPET_SQFT,
            SUPERBUILTUP_SQFT,BROKERAGE,AMENITIES,CITY_ID))

data_clean$AREA<-data_clean$AREA%>% 
  gsub("? sq.ft.","",.) %>% as.numeric()

#dealing with multicollinearity
#drop columns with multicollinearity
data_clean<-data_clean %>% select(-c(MIN_AREA_SQFT,MAX_AREA_SQFT,PRICE_SQFT,
                                     MIN_PRICE,MAX_PRICE,BATHROOM_NUM))
num_vars<-data_clean %>% select(where(is.numeric))
#correlation matrix
cor_matrix <- cor(num_vars,use="pairwise.complete.obs")

#heatmap
ggcorrplot(cor_matrix,
           type = "lower",       # lower triangle only
           lab = TRUE,           # show correlation coefficients
           lab_size = 2.5,       # smaller numbers
           tl.cex = 8,           # text label size
           tl.srt = 45,          # rotate x-axis labels
           colors = c("red", "white", "blue")) +
  theme(
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)
  )



#dealing with outliers

#plot histogram
num_vars%>%
  pivot_longer(cols=everything(),
               names_to="variable",
               values_to="value") %>%
  ggplot(aes(x=value)) +
  geom_histogram(bins=30,fill="skyblue",color="black") +
  facet_wrap(~variable,scales="free")+
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
#boxplot
num_vars %>% 
  pivot_longer(cols=everything(),
               names_to="variable",
               values_to="value") %>%
  ggplot(aes(x=variable,y=value)) +
  geom_boxplot(outlier.color="red") +
  facet_wrap(~variable,scales="free") +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Boxplots for Numeric Variables")

#drop null entries
data_clean<-data_clean %>% na.omit() %>% glimpse()


cat("\nYou don’t always need to remove outliers from your data.\nGAM is capable of using various distribution.\nIf your numerical variables have extreme values or heavy tails, you can model them with heavy-tailed distributions rather than the default Gaussian.")

save.image("damma.Rdata")












