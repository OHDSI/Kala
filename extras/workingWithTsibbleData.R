## TSIBBLE############
######################
# filter by key
data <- tsibbleData %>% 
  dplyr::filter(rateType == 'incidence', firstOccurrenceOnly == TRUE, washoutPeriod == 365)

# filter by index 
data <- tsibbleData %>% 
  dplyr::filter(rateType == 'incidence', firstOccurrenceOnly == TRUE, washoutPeriod == 365)  %>% 
  tsibble::filter_index("2010-01-01" ~ "2019-12-31") 


# change index period to calendar-week, month, quarter, year
data2 <- tsibbleData %>% 
  dplyr::filter(rateType == 'incidence', firstOccurrenceOnly == TRUE, washoutPeriod == 365)  %>% 
  dplyr::select(-rateType, -firstOccurrenceOnly, -washoutPeriod) %>% 
  tsibble::filter_index("2010-01-01" ~ "2019-12-31") %>% 
  tsibble::group_by_key() %>% 
  tsibble::index_by(calendarWeek = ~ tsibble::yearweek(.)) %>% 
  dplyr::summarise(numeratorCount = sum(numeratorCount),
                   denominatorCount = sum(denominatorCount)
  )

# create new measure from existing measures
data2 <- tsibbleData %>% 
  dplyr::filter(rateType == 'incidence', firstOccurrenceOnly == TRUE, washoutPeriod == 365)  %>% 
  dplyr::select(-rateType, -firstOccurrenceOnly, -washoutPeriod) %>% 
  tsibble::filter_index("2010-01-01" ~ "2019-12-31") %>% 
  tsibble::group_by_key() %>% 
  tsibble::index_by(calendarWeek = ~ tsibble::yearweek(.)) %>% 
  dplyr::summarise(numeratorCount = sum(numeratorCount),
                   denominatorCount = sum(denominatorCount),
                   rate = sum(numeratorCount)/sum(denominatorCount)
  )

# get the metadata for tsibble
tsibble::key(tsibbleData)
tsibble::measures(tsibbleData)
tsibble::interval(tsibbleData)

tsibble::key(data)
tsibble::measures(data)
tsibble::interval(data)
tsibble::is_regular(data)



## FEASTS ############
######################

data2 %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(numeratorCount = sum(numeratorCount),
                   denominatorCount = sum(denominatorCount),
                   rate = sum(numeratorCount)/sum(denominatorCount)
  ) %>% 
  feasts::gg_season(y = rate)


# visualize


# seasonally decompose
# forecast
# detect seasonality
# detect break points
# detect change in performance of detection of seasonality and breakpoints by calendar period - power?
# change 


group_by_key() %>%
  index_by(Year_Month = ~ yearmonth(.)) %>%
  summarise(
    Max_Count = max(Count),
    Min_Count = min(Count)
  )