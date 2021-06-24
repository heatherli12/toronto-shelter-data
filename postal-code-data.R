library(tidyverse)
library(opendatatoronto)
library(lubridate)
library(plotly)
library(ggthemes)

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
       y = "Percent of Shelter Beds Occupied")+
  theme_economist() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M1E, "m1e.rds")