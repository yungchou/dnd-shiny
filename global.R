library(shiny)
library(dplyr)
library(ggplot2)
library(dygraphs)
library(xts)
library(anytime)
library(tidyjson)
library(leaflet)
library(reshape2) # for melt
library(gridExtra)

# GPS readings are not implemented at this time.
lng=c(-99,-97,-87,-107,-91)
lat=c(38,32,30,37,41)
deviceID=c('rp1','rp2','rp3','rp4','rp5')

s <- read.csv('./iot_sample_data.csv',header = TRUE)

df <- na.omit(s) %>%
  select( Timestamp, message ) %>%
  mutate( Timestamp = anytime( as.character(Timestamp) ) ) %>%
  arrange( Timestamp ) %>%
  mutate( message = as.character(message) )

df1 <- df$message %>% as.tbl_json %>%
  gather_array %>%
  spread_values(
    msgID = jstring("messageId"),
    devID = jstring("deviceId"),
    temperature = jnumber("temperature"),
    humidity = jnumber("humidity")) %>%
  select(devID, temperature, humidity)

stats <- df1 %>% group_by(devID) %>%
  summarize(meanTemp=mean(temperature),sdTemp=sd(temperature),
            meanHumi=mean(humidity),sdHumi=sd(humidity),
            cor=cor(temperature,humidity))

n=2000

iot_dev  <- df1$devID[1:n]
device <- unique(iot_dev)

iot_time <- df$Timestamp[1:n]

rp1 <- head(df1,n) %>% filter(devID=='rp1') %>% select(-devID)
rp2 <- head(df1,n) %>% filter(devID=='rp2') %>% select(-devID)
rp3 <- head(df1,n) %>% filter(devID=='rp3') %>% select(-devID)

iot_temp <- df1$temperature[1:n]
iot_humi <- df1$humidity[1:n]

ambient <- cbind(iot_temp,iot_humi)
ambient <- xts(ambient, order.by = iot_time)

