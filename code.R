library(rvest)
library(tidyverse)

url <- "http://www.vgchartz.com/gamedb/?name=&publisher=&platform=&genre=&minSales=0&results=10000"
html<- read_html(url)
#import and remove duplicated columns eg NA, EU, JPN, ..
rawdata_mat <- matrix(html %>% html_nodes("th , .chart td , .chart center") %>% html_text(),ncol=16,byrow = T)[,-c(8,10,12,14,16)]

data<-rawdata_mat[-c(1,nrow(rawdata_mat)),]

data_df<- data.frame(data)
colnames(data_df)<- rawdata_mat[1,]

data_df$Pos<-as.numeric(as.character(data_df$Pos))
data_df$Year<-as.numeric(as.character(data_df$Year))
data_df$`North America`<-as.numeric(as.character(data_df$`North America`))
data_df$Europe<-as.numeric(as.character(data_df$Europe))
data_df$Japan<-as.numeric(as.character(data_df$Japan))
data_df$`Rest of World`<-as.numeric(as.character(data_df$`Rest of World`))
data_df$Global<-as.numeric(as.character(data_df$Global))
data_df$Year

saveRDS(data_df,"vg.rds")
