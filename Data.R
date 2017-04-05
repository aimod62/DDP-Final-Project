library(dplyr)
library(reshape2)
library(ggplot2)
library("dygraphs")
library(plotly)
library(shiny)
library(maps)

#Extracting and Cleaning the Data. 
#Source:World Bank - Data Bank


#Loading the Raw Data

data1<- read.csv("~/DDP_Finalproject/57b46c40-8cd0-44f0-bb12-b47778bb9861_Data.csv", stringsAsFactors  = FALSE)
View(data1)

#Manipulating the Data 
p_1<- data1 %>% 
  filter(Series.Name == "Adjusted net national income (annual % growth)") %>%
  select(Country.Code, X2011..YR2011., X2012..YR2012., X2013..YR2013.,X2014..YR2014.) %>%
  mutate("2011" = round(as.numeric(X2011..YR2011.), 1),
         "2012" = round(as.numeric(X2012..YR2012.), 1),
         "2013" = round(as.numeric(X2013..YR2013.), 1),
         "2014" = round(as.numeric(X2013..YR2013.), 1))%>%
  select(-starts_with("X")) %>% 
  melt(id = c("Country.Code"))

#Writing a .csv file 
write.csv(p_1, "~/data1.csv")
incData<-read.csv("~/DDP_Finalproject/data1.csv", stringsAsFactors = FALSE)
View(incData)
str(incData)


## Creating Dygraph reactive Object

#Loading the Data

data2 <- read.csv("~/DDP_Finalproject/6d85ede2-232f-4977-93eb-144065932f49_Data.csv", stringsAsFactors = FALSE )
View(data2)
str(data2)

#Manipulating the data
p_2 <-data2[ ,5:21] %>% t() %>% as.data.frame()%>% select(V1) %>% cbind(1997:2013) %>% 
  setNames(.,c("CO2 emissions ","Year" )) 
rownames(p_2) <- c() 
p_3 <- p_2[, c(2,1)]


#Writing a .csv file
write.csv(p_3, "~/data3.csv")
CO2_ELSV<-read.csv("~/DDP_Finalproject/data3.csv", stringsAsFactors = FALSE)
View(CO2_ELSV)
str(CO2_ELSV)


