#set working directory and clear workspace
rm(list=ls())
setwd("Damma")
load("damma.Rdata")

#load packages
library(readr)
library(dplyr)
library(tidyverse) #set of packages of intrest
library(purrr)
library(jsonlite)
library(fastDummies)
library(ggcorrplot)

#load data
data<-read_csv("damma.csv")


#EDA
glimpse(data)
#dealing with missing values 
data %>% is.na() %>% colSums() %>% as.data.frame()

data.frame(count=colSums(is.na(data)),
           prop=round((colSums(is.na(data)))/nrow(data),4),
           drop=((colSums(is.na(data)))/nrow(data))>0.5)

#drop null counts
mask_na<-((data %>% is.na() %>% colSums())/nrow(data))>0.5
data<-data[,!mask_na]

#separate numeric and categorical var
num_vars<-data %>% select(where(is.numeric))
cate_vars<-data %>% select(where(is.character))

#dealing with high and low cardinality
cate_vars %>%
  map_dbl(~length(unique(.)),.id=count) %>% 
  as.data.frame()%>%setNames("count")


data<-data[-5]

#extract lat and lon values from MAP_DETAILS
data[c("lat", "lon")]<-data$MAP_DETAILS %>%
  str_replace_all("'", "\"") %>%
  lapply(fromJSON) %>%
  bind_rows() %>%
  select(lat = LATITUDE, lon = LONGITUDE)
#extract dealers
data$dealers<-data$CLASS_HEADING %>%gsub("^Dealer: ?","",.)

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

num_vars%>% summary()

#one-hot encoding
data$PROPERTY_TYPE <-data$PROPERTY_TYPE %>%gsub(" ","_",.) %>%gsub("/", "_", .)
data<-dummy_cols(data, 
                 select_columns="PROPERTY_TYPE",
                 remove_first_dummy=T, 
                 remove_selected_columns=T)

#dealing with multicollinearity
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

#saveworkspace
save.image("damma.Rdata")

