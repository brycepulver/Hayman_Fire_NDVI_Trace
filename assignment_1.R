library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T)


#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')


ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')
# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))

##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

## Your code here

full_wide <- spread(full_long,key='data',value='value') %>%
  filter_if(is.numeric,all_vars(!is.na(.))) %>%
  mutate(month = month(DateTime), year = year(DateTime))

summer_only <- filter(full_wide, month %in% c(6,7,8,9))
head(summer_only)

ggplot(full_wide,aes(x=ndvi,y=ndmi,color=site)) + 
  geom_point()  
  
ggplot(summer_only,aes(x=ndvi,y=ndmi,color=site)) + 
  geom_point() 
 

## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 


## Your code here
snow_cover <- filter (full_wide, month %in% c(1:4)) %>%
  group_by(site,year) %>%
  summarize(mean_ndsi=mean(ndsi))
  

veg_growth <- filter (full_wide, month %in% c(5:8)) %>%
  group_by(site,year) %>%
  summarize(mean_ndvi=mean(ndvi))

SC_VG <- merge(snow_cover,veg_growth) 
 

ggplot(SC_VG,aes(x=mean_ndsi,y=mean_ndvi, color = site)) +
  geom_point()

head(SC_VG)
## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

## Your code here
Sc_VG_pre_post <- SC_VG %>% 
          mutate(treatment = cut(year,breaks=c(0,2003,2020),
                         labels=c('pre-burn','post-burn')))%>%
          group_by(year,site,treatment)
  

ggplot(Sc_VG_pre_post,aes(x=mean_ndsi,y=mean_ndvi,color=site)) +
  geom_point() + 
  theme_few() + 
  scale_color_few() + 
  theme(legend.position=c(0.4,0.2)) + 
  facet_wrap(~treatment)

## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
#plots after the fire? 

ndvi_month_pre_post <- full_long %>% 
  mutate(year = year(DateTime),
         month = month(DateTime),
         treatment = cut(year,breaks=c(0,2003,2020),
                         labels=c('pre-burn','post-burn')))%>%
group_by(month,site,treatment,data)%>%
  filter(str_detect(data,"ndvi"))%>%
  summarize(mean_ndvi = mean(value))

ggplot(ndvi_month_pre_post,aes(x=month,y=mean_ndvi,color=treatment)) +
  geom_point() + 
  geom_line() +
  theme_few() + 
  scale_color_few() + 
  theme(legend.position=c(0.6,0.2)) + 
  facet_wrap(~site)

view(ndvi_month_pre_post)
##### Question 5 ####
#What month is the snowiest on average?

ndsi_month_pre_post <- full_long %>% 
  mutate(year = year(DateTime),
         month = month(DateTime),
         treatment = cut(year,breaks=c(0,2003,2020),
                         labels=c('pre-burn','post-burn')))%>%
  group_by(month,site,treatment,data)%>%
  filter(str_detect(data,"ndsi"))%>%
  summarize(mean_ndsi = mean(value))

ggplot(ndsi_month_pre_post,aes(x=month,y=mean_ndsi,color=treatment)) +
  geom_point() + 
  geom_line() +
  theme_few() + 
  scale_color_few() + 
  theme(legend.position=c(0.6,0.2)) + 
  facet_wrap(~site)
