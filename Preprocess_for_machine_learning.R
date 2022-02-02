#Load required libraries 
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(reshape2)

#load required files and prepare health file: 
load("~/sesam/MSc_project/Thesis_docs/data/all_activity_SESAM_COW.RData")
load("~/sesam/MSc_project/Thesis_docs/data/standardisedGW50.RData")
load("~/sesam/MSc_project/Thesis_docs/data/Health_GW50.RData")

tbl_hd_gw50 <- Health_GW50
tbl_hd_gw50 <- tbl_hd_gw50 %>%
  select(-Other_event)

tbl_event_gw50 <- tbl_hd_gw50 %>%
  pivot_longer(!SENSOR_MAC, 
               names_to = "event", 
               values_to = "date",
               values_drop_na = TRUE)
tbl_event_gw50$date <- as.Date(tbl_event_gw50$date)

#Adding a column for events to the behavioural data 
## Create a column for events and set the default to "no_event". We add this column to the standardized behavioural data 
standardised_df$Event <- ("no_event")

##Begin adding the known events to the event column. Here we add all dates with an event to the events column  
### Here we also convert the name "In_heat" to "Oestrus" as we later use in_heat to describe the day before the observed oestrus date 
for (idx in 1:nrow(standardised_df)) {
  sensor <- standardised_df$SENSOR_MAC[idx]
  vec_event_rows <- which(tbl_event_gw50$SENSOR_MAC == sensor)
  day_current_activity <- standardised_df$DAY[idx]
  tbl_event_current_sensor <- tbl_event_gw50[vec_event_rows,]
  tbl_event_current_sensor <- tbl_event_current_sensor %>%
    filter(date == as.Date(day_current_activity))
  standardised_df$Event[idx] <- with((standardised_df[idx,]), ifelse(as.Date(day_current_activity) %in% tbl_event_current_sensor$date, tbl_event_current_sensor$event, Event))
  if(standardised_df$Event[idx] == "In_heat") {
    standardised_df$Event[idx] <- "Oestrus"
  }
}

### We create another event for "In_heat" on the date before the observed heat. This is because we want to use the day before observation for later classification. 
for (idx in 1:nrow(standardised_df)) {
  sensor <- standardised_df$SENSOR_MAC[idx]
  vec_event_rows <- which(tbl_event_gw50$SENSOR_MAC == sensor) #which rows in the health dataset have the same sensor ID
  day_current_activity <- standardised_df$DAY[idx] #what is the date of the row idx
  tbl_event_current_sensor <- tbl_event_gw50[vec_event_rows,] #show the health df only for the select sensor 
  tbl_event_current_sensor <- tbl_event_current_sensor %>%
    filter(event == "In_heat") #only show the rows for "in_heat" 
  tbl_event_current_sensor <- tbl_event_current_sensor %>%
    filter(date >= as.Date(day_current_activity) -1 & date <= as.Date(day_current_activity) +1) #if there is more than one date for in_heat, only show the one that is around the time of the date on the idx row 
  if (nrow(tbl_event_current_sensor) == 0) next
  heat_date <- as.Date(tbl_event_current_sensor$date) - 1
  if (standardised_df$Event[idx] == "no_event") {
    standardised_df$Event[idx] <- with((standardised_df[idx,]), ifelse(as.Date(day_current_activity) == heat_date, "In_heat", standardised_df$Event))
  }
}

### We create another event for "Pre-calving" for the day before calving so that we can use this as the date for classification.
for (idx in 1:nrow(standardised_df)) {
  sensor <- standardised_df$SENSOR_MAC[idx]
  vec_event_rows <- which(tbl_event_gw50$SENSOR_MAC == sensor) #which rows in the health dataset have the same sensor ID
  day_current_activity <- standardised_df$DAY[idx] #what is the date of the row idx
  tbl_event_current_sensor <- tbl_event_gw50[vec_event_rows,] #show the health df only for the select sensor 
  tbl_event_current_sensor <- tbl_event_current_sensor %>%
    filter(event == "Calving") #only show the rows for "calving"
  tbl_event_current_sensor <- tbl_event_current_sensor %>%
    filter(date >= as.Date(day_current_activity) -1 & date <= as.Date(day_current_activity) +1) #if there is more than one date for in_heat, only show the one that is around the time of the date on the idx row 
  if (nrow(tbl_event_current_sensor) == 0) next
  calving_date <- unique(as.Date(tbl_event_current_sensor$date)) - 1
  if (standardised_df$Event[idx] == "no_event") {
    standardised_df$Event[idx] <- with((standardised_df[idx,]), ifelse(as.Date(day_current_activity) == calving_date, "Pre_calving", standardised_df$Event))
  }
}

#save as new data frame 
save(standardised_df, file = "~/sesam/MSc_project/THESIS/Data/standardised_df50.RData")

### remove column we dont require and create the final dataframe that we require fore use in ML classification
df_prep <- standardised_df %>%
  select(-value, -mean_minutes, -SD_minutes, -new_value) %>%
  group_by() %>%
  pivot_wider(names_from = series, values_from = standarized_value)
df_prep50 <<- df_prep

save(df_prep50, file = "~/sesam/MSc_project/THESIS/Data/df_prep50.RData")
