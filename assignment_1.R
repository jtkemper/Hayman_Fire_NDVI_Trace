library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)


# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('hayman_data',full.names=T)
files


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
View(full_long)

###############################################################################################

##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

###transform long data to wide data
full_wide <- full_long %>%
  spread(key = "data", value = "value") %>%
  filter_if(is.numeric, all_vars(!is.na(.))) %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime))

View(full_wide)

##remove winter months
head(full_wide)
summer_only <- filter(full_wide, month %in% c(6,7,8,9))

###plot of ndvi vs ndmi for all data - no summer months removed
full_wide %>%
  ggplot(., aes(x = ndmi, y = ndvi, color = site)) +
  geom_point(size = 1.5) +
  xlab("NDMI") +
  ylab("NDVI") +
  xlim(-0.25, 0.75) +
  ylim(-0.35,0.75)+
  scale_color_fivethirtyeight() + 
  theme_few() +
  theme(legend.position = c(0.15,0.15),
        text = element_text(color = "black"), 
        axis.text = element_text(color = "black"),
        axis.ticks.length = unit(-.2, "cm"),
        axis.text.x = element_text(size = 12, margin = margin(10,10,10,10,"pt")),
        axis.text.y = element_text(size = 12, margin = margin(10,10,10,10,"pt"))) +
  guides(color=guide_legend(title="Site"))

View(full_wide)

###here a plot of the relationship between NDVI and NDSI for burned and unburned sites w/out trend lines
summer_only %>%
  ggplot(., aes(x = ndmi, y = ndvi, color = site)) +
  geom_point(size = 2, shape = 17) +
  xlab("NDMI") +
  ylab("NDSI") +
  scale_color_manual(values = c("red", "forestgreen")) + 
  theme_few() +
  theme(legend.position = c(0.9,0.9),
        text = element_text(color = "black"), 
        axis.text = element_text(color = "black"),
        axis.ticks.length = unit(-.2, "cm"),
        axis.text.x = element_text(size = 12, margin = margin(10,10,10,10,"pt")),
        axis.text.y = element_text(size = 12, margin = margin(10,10,10,10,"pt"))) +
  guides(color=guide_legend(title="Site"))

###here a plot of the relationship between NVDI and NDSI for burned and unburned sites w/ trendlines
summer_only %>%
  ggplot(., aes(x = ndmi, y = ndvi, color = site)) +
  geom_point(size = 2, shape = 17) +
  geom_smooth(method = "lm", se = F) +
  xlab("NDMI") +
  ylab("NDSI") +
  scale_color_manual(values = c("red", "forestgreen")) + 
  theme_few() +
  theme(legend.position = c(0.9,0.9),
        text = element_text(color = "black"), 
        axis.text = element_text(color = "black"),
        axis.ticks.length = unit(-.2, "cm"),
        axis.text.x = element_text(size = 12, margin = margin(10,10,10,10,"pt")),
        axis.text.y = element_text(size = 12, margin = margin(10,10,10,10,"pt"))) +
  guides(color=guide_legend(title="Site"))


###There looks to be a much stronger trend between NDMI and NDVI for the burned sites
### versus the unburned, though both are greatly influenced by outliers. To the naked eye,
### without trendlines, there appears to be a positive correlation between NDMI and NDVI.


## End Code for Question 1 ####
###############################################################################################

#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 


###Average NDSI by the winter months
ndsi_annual <- full_wide %>%
  select("year", "month", "site", "ndsi") %>%
  filter(month %in% c(1,2,3,4))%>%
  group_by(year, site) %>%
  summarise(mean_ndsi = mean(ndsi))
  
View(ndsi_annual)
  
###Average NDVi for the summer months
ndvi_annual <- full_wide %>%
  select("ndvi", "site", "year", "month") %>%
  filter(month %in% c(6:8)) %>%
  group_by(site, year) %>%
  summarise(mean_ndvi = mean(ndvi))
View(ndvi_annual)


###create dataframe with mean annual ndsi and mean annual ndvi
ndvi_ndsi <- inner_join(ndvi_annual, ndsi_annual, by = c("year", "site")) 
View(ndvi_ndsi)


###here a plot of ndvi vs ndsi
ndvi_ndsi %>%
  ggplot(., aes(x = mean_ndsi, y = mean_ndvi)) +
  geom_point(size = 2.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("Mean Annual NDSI") +
  ylab("Mean Annual NDVI") +
  theme_few() +
  theme(legend.position = c(0.9,0.15),
        text = element_text(color = "black"), 
        axis.text = element_text(color = "black"),
        axis.ticks.length = unit(-.2, "cm"),
        axis.text.x = element_text(size = 12, margin = margin(10,10,10,10,"pt")),
        axis.text.y = element_text(size = 12, margin = margin(10,10,10,10,"pt"))) +
  guides(color=guide_legend(title="Site"))

###Examine relationship between ndvi and ndsi with a regression line
ndvi_ndsi.lm <- lm(mean_ndvi ~ mean_ndsi, data = ndvi_ndsi) %>% 
  summary() %>% 
  print


###It appears from the plot and the low R2 (R2 = 0.03) that there is very little correlation 
### between mean anuual NDVI and ean annual NDSI 

## End code for question 2 ##
################################################################################################

###### Question 3 #################
#How is the snow effect from question 2 different between pre- and post-burn
#and burned and unburned? 


###here a plot of ndvi and ndsi for burned and unburned sites
ndvi_ndsi %>%
  ggplot(., aes(x = mean_ndsi, y = mean_ndvi, color = site)) +
  geom_point(size = 3, shape = 17) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Mean Annual NDSI") +
  ylab("Mean Annual NDVI") +
  scale_color_manual(values = c("red", "forestgreen")) + 
  theme_few() +
  theme(legend.position = c(0.9,0.15),
        text = element_text(color = "black"), 
        axis.text = element_text(color = "black"),
        axis.ticks.length = unit(-.2, "cm"),
        axis.text.x = element_text(size = 12, margin = margin(10,10,10,10,"pt")),
        axis.text.y = element_text(size = 12, margin = margin(10,10,10,10,"pt"))) +
  guides(color=guide_legend(title="Site"))

###creating a pre- and post-burn column
ndvi_ndsi_pre_post <- ndvi_ndsi %>%
  ungroup() %>%
  select("year", "mean_ndvi", "mean_ndsi", "site") %>%
  mutate(treatment = cut(year, breaks = c(0, 2003, 2020), labels = c("pre-burn", "post-burn")))
View(ndvi_ndsi_pre_post) 
  
###here a scatter plot of ndvi vs ndsi for pre- and post-burn 
ndvi_ndsi_pre_post %>%
  ggplot(., aes(x = mean_ndsi, y = mean_ndvi, shape = treatment, linetype = treatment)) +
  geom_point(size = 2.5, color = "black") +
  scale_shape_manual(values = c(17, 2)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  xlab("Mean Annual NDSI") +
  ylab("Mean Annual NDVI") +
  theme_few() +
  theme(legend.position = c(0.9,0.15),
        text = element_text(color = "black"), 
        axis.text = element_text(color = "black"),
        axis.ticks.length = unit(-.2, "cm"),
        axis.text.x = element_text(size = 12, margin = margin(10,10,10,10,"pt")),
        axis.text.y = element_text(size = 12, margin = margin(10,10,10,10,"pt"))) +
  guides(color=guide_legend(title="Site")) +
  facet_wrap(~site)
  
###As before, there appears to be very little or no correlation between mean annual NDSI and mean annual NDVi
###for both burned and unburned sites and pre- and post-burn
## End code for question 3 ##

#####################################################################################################

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 


###Calculate maximum ndvi
ndvi_ndsi_max <- full_wide %>%
  select("ndvi", "ndsi", "month", "year", "site") %>%
  mutate(treatmeant = cut(year, breaks = c(0,2003,2020), labels = c("pre-burn", "post-burn"))) %>%
  group_by(month) %>%
  summarise(mean_ndvi = mean(ndvi),
            mean_ndsi = mean(ndsi)) %>%
  slice(which.max(mean_ndvi)) %>%
  print

  ###Greenest month on average: August


###Calculate max greeness month pre- and post-burn
ndvi_ndsi_max_burned <- full_wide %>%
  select("ndvi", "ndsi", "month", "year", "site") %>%
  mutate(treatment = cut(year, breaks = c(0,2003,2020), labels = c("pre-burn", "post-burn"))) %>%
  filter(site == "burned") %>%
  group_by(month, treatment) %>%
  summarise(mean_ndvi = mean(ndvi), 
            mean_ndsi = mean(ndsi)) %>%
  group_by(treatment) %>%
  slice(which.max(mean_ndvi)) %>%
  print
  
  ###The greenest month is August both pre- and post-burn 


###End code question 4

###################################################################################################3
##### Question 5 ####
#What month is the snowiest on average?
ndvi_ndsi_maxsnow <- full_wide %>%
  select("ndvi", "ndsi", "month", "year", "site") %>%
  mutate(treatmeant = cut(year, breaks = c(0,2003,2020), labels = c("pre-burn", "post-burn"))) %>%
  group_by(month) %>%
  summarise(mean_ndvi = mean(ndvi),
            mean_ndsi = mean(ndsi)) %>%
  slice(which.max(mean_ndsi)) %>%
  print

  ###January is the snowiest month on average

###End code question 5######