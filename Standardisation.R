#STANDARDISATION
##Load required packages
library(tidyr)
library(dplyr)
library(readr)
library(reshape2)
library(readxl)

## Read in the health excel file for GW50
s_hd_path_gw50 <- "~/sesam/MSc_project/Thesis_docs/data/Health_GW50.xlsx"
tbl_hd_gw50 <- readxl::read_excel(path = s_hd_path_gw50)
dim(tbl_hd_gw50)
head(tbl_hd_gw50)

Health_GW50 <- tbl_hd_gw50

save(Health_GW50, file = "~/sesam/MSc_project/Thesis_docs/data/Health_GW50.RData")

## Prepare health file
###"Other event" is a description of health events that occurred on the date in column "Other". 
### We remove the description but keep the date of the event to exclude from later analysis 

tbl_hd_gw50 <- Health_GW50 %>%
  select(-Other_event)

### Pivot the data to merge all the event names into a single column "event" and all the dates into a single column "date"
tbl_event_gw50 <- tbl_hd_gw50 %>%
  pivot_longer(!SENSOR_MAC, 
               names_to = "event", 
               values_to = "date",
               values_drop_na = TRUE)
tbl_event_gw50$date <- as.Date(tbl_event_gw50$date)
head(tbl_event_gw50)

save(tbl_event_gw50, file = "~/sesam/MSc_project/Thesis_docs/data/tbl_event_GW50.RData")

### When doing standardisation, we want to use only 'healthy' time periods where no health or reproductive events occurred in order to calculate the mean, baseline behavior of the cow. 
### To ensure we only have data where behavior is likely not affected by an event, we remove 14 days either side of a date where a known event (health or reproductive) has occurred.
### We name the uper and lower oundaries of the event time span as pre_ and post_date
tbl_event_gw50 <- tbl_event_gw50 %>% 
  mutate(pre_date = date - 14,
         post_date = date + 14)
head(tbl_event_gw50)


##Prepare data to include in the standardisation. This is all behavioral data during the "no event" time period.

load("~/sesam/MSc_project/Thesis_docs/data/all_activity_SESAM_COW.RData")
head(all_activity)
nrow(all_activity)


### create an empty data frame called tbl_norm_data
### run through behavior data row by row and determine sensor ID
### here we create a datafram only including the "no event" data and removing the 14days either side of the known events
tbl_norm_data <- NULL
for (idx in 1:nrow(all_activity)){
  current_sensor <- all_activity$SENSOR_MAC[idx]
  vec_event_rows <- which(tbl_event_gw50$SENSOR_MAC == current_sensor)
  if (length(vec_event_rows) == 0) {
    tbl_norm_data <- dplyr::bind_rows(tbl_norm_data, all_activity[idx,])
    next
  }
  # if sensor ID is found in the health event data 
  tbl_event_current_sensor <- tbl_event_gw50[vec_event_rows,]
  # loop over events in tbl_event_current_sensor and check whether current 
  # activity is inside of an event time span
  day_current_activity <- all_activity$DAY[idx]
  b_include_rec <- TRUE
  for (jdx in 1:nrow(tbl_event_current_sensor)){
    if (day_current_activity >  tbl_event_current_sensor$pre_date[jdx] & 
        day_current_activity < tbl_event_current_sensor$post_date[jdx]) {
      b_include_rec <- FALSE
    }
  }
  # check whether we have to include
  if (b_include_rec){
    tbl_norm_data <- dplyr::bind_rows(tbl_norm_data, all_activity[idx,])
  }
}

###save the data only including the data without any reproductive or health events
save(tbl_norm_data,
     file = "~/sesam/MSc_project/Thesis_docs/data/tbl_norm_data.RData",
     verbose=T)

## Now we have the data files we need, we can standardise 
### Find the mean during the 'no event' time period. We then melt all of the behaviours into a single column named "series"
df_mean <- tbl_norm_data %>%
  group_by(SENSOR_MAC) %>%
  summarise(Lying = mean(Lying), Eating = mean(Eating), Standing = mean(Standing), Walking = mean(Walking), Ruminating = mean(Ruminating), Activity = mean(Daily_Activ))

mean_merge <- melt(subset(df_mean,
                          select=c("SENSOR_MAC",
                                   "Lying",
                                   "Eating",
                                   "Standing",
                                   "Walking",
                                   "Ruminating")),
                   id.vars = c("SENSOR_MAC"),
                   variable.name = 'activity')
colnames(mean_merge) <- c("SENSOR_MAC", "series", "mean_minutes")

### Find the standard deviation for all sensors during the 'no event' time period
df_SD <- tbl_norm_data %>%
  group_by(SENSOR_MAC) %>%
  summarise(Lying = sd(Lying), Eating = sd(Eating), Standing = sd(Standing), Walking = sd(Walking), Ruminating = sd(Ruminating), Activity = sd(Daily_Activ))

SD_merge <- melt(subset(df_SD,
                        select=c("SENSOR_MAC",
                                 "Lying",
                                 "Eating",
                                 "Standing",
                                 "Walking",
                                 "Ruminating")),
                 id.vars = c("SENSOR_MAC"),
                 variable.name = 'activity')
colnames(SD_merge) <- c("SENSOR_MAC", "series", "SD_minutes")

### melt mean and SD into the dataset 
load("~/sesam/MSc_project/GW50/update/data/melted_SESAM_COW.RData")
df2_with_mean <- merge(df2, mean_merge, by = c("SENSOR_MAC", "series"))
df2_mean_SD <- merge(df2_with_mean, SD_merge, by = c("SENSOR_MAC", "series"))
df_normalisation <- df2_mean_SD

###add a column for original minutes for behaviour - mean minutes for behaviour 
final_df_normalisation <- df_normalisation %>%
  mutate(new_value = value - as.numeric(mean_minutes))

### standardisation equation: ((value-mean of no event data)/sd of no event data). We multiply by 10 to scale.
standardised_df <- final_df_normalisation %>%
  mutate(standarized_value = (new_value/as.numeric(SD_minutes)*10))

save(standardised_df,
     file = "~/sesam/MSc_project/Thesis_docs/data",
     verbose=T)

## we merge the standardise behavioural data with the health and reproductive event data 
load("~/sesam/MSc_project/GW50/update/data/standardisedGW50.RData")
df_GW50 <- merge(standardised_df, Health_GW50, by = "SENSOR_MAC")
save(df_GW50,
     file="~/sesam/MSc_project/THESIS/27.01.22/df_GW50.RData",
     vebose=T)

