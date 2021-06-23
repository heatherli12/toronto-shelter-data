library(tidyverse)
library(opendatatoronto)
library(lubridate)
library(plotly)
library(ggthemes)

#getting all the resources for the package, i.e. individual data sets
resources <- list_package_resources("8a6eceb2-821b-4961-a29d-758f3087732d")
#identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
#getting data for each year and cleaning it up
#overview of toronto's data
shelter_data_2020_overview <- datastore_resources %>% 
  filter(row_number()==5) %>% 
  get_resource() %>% 
  select(OCCUPANCY_DATE, SHELTER_POSTAL_CODE, OCCUPANCY, CAPACITY) %>% 
  filter(CAPACITY > 0) %>% 
  mutate(SHELTER_POSTAL_CODE = str_sub(SHELTER_POSTAL_CODE, 1, 3)) %>% 
  mutate(PERCENT_OCCUPIED = OCCUPANCY/CAPACITY) %>% 
  mutate(OCCUPANCY_DATE = mdy(OCCUPANCY_DATE)) %>%
  select(OCCUPANCY_DATE, SHELTER_POSTAL_CODE, PERCENT_OCCUPIED) %>% 
  group_by(OCCUPANCY_DATE) %>%
  summarize(AVG_ORATE = mean(PERCENT_OCCUPIED))

#making the overview plot
plot_overview <- ggplot(data = shelter_data_2020_overview,
       mapping = aes(x = OCCUPANCY_DATE,
                     y = AVG_ORATE,
                     text = paste("Date:", OCCUPANCY_DATE, "\n",
                                  "% Occupied:", str_sub(AVG_ORATE, 1, 4))))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "An Overview of Shelter Occupancy in the \nCity of Toronto in 2020",
       caption = "Open Data, City of Toronto 2021")+
  theme_economist() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(plot_overview, "overview.rds")

#making the second data set for the individual postal codes one
postal_code_data <- datastore_resources %>% 
  filter(row_number()==5) %>% 
  get_resource() %>% 
  select(OCCUPANCY_DATE, SHELTER_POSTAL_CODE, OCCUPANCY, CAPACITY) %>% 
  filter(CAPACITY > 0) %>% 
  mutate(SHELTER_POSTAL_CODE = str_sub(SHELTER_POSTAL_CODE, 1, 3)) %>% 
  mutate(PERCENT_OCCUPIED = OCCUPANCY/CAPACITY) %>% 
  mutate(OCCUPANCY_DATE = mdy(OCCUPANCY_DATE)) %>%
  select(OCCUPANCY_DATE, SHELTER_POSTAL_CODE, PERCENT_OCCUPIED) %>% 
  group_by(OCCUPANCY_DATE, SHELTER_POSTAL_CODE) %>%
  summarize(AVG_ORATE = mean(PERCENT_OCCUPIED))

#plot for M1E
M1E <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M1E") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE,
                       text = paste("Date:", OCCUPANCY_DATE, "\n",
                                    "% Occupied:", str_sub(AVG_ORATE, 1, 4))))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "An Overview of Shelter Occupancy in \nPostal Code M1E in 2020",
       caption = "Open Data, City of Toronto 2021")+
  theme_economist() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
M1E
write_rds(M1E, "m1e.rds")

