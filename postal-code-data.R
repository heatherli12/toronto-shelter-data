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
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M1E Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M1E, "m1e.rds")

#plot for M1K
M1K <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M1K") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M1K Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M1K, "m1k.rds")

#plot for M1L
M1L <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M1L") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M1L Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M1L, "m1l.rds")

#plot for M1M
M1M <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M1M") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M1M Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M1M, "m1m.rds")

#plot for M1P
M1P <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M1P") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M1P Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M1P, "m1p.rds")

#plot for M2M
M2M <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M2M") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M2M Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M2M, "m2m.rds")

#plot for M2N
M2N <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M2N") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M2N Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M2N, "m2n.rds")

#plot for M3B
M3B <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M3B") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M3B Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M3B, "m3b.rds")

#plot for M3M
M3M <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M3M") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M3M Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M3M, "m3m.rds")

#plot for M4C
M4C <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M4C") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M4C Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M4C, "m4c.rds")

#plot for M4K
M4K <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M4K") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M4K Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M4K, "m4k.rds")

#plot for M4M
M4M <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M4M") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M4M Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M4M, "m4m.rds")

#plot for M4T
M4T <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M4T") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M4T Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M4T, "m4t.rds")

#plot for M4W
M4W <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M4W") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M4W Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M4W, "m4w.rds")

#plot for M4X
M4X <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M4X") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M4X Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M4X, "m4x.rds")

#plot for M4Y
M4Y <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M4Y") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M4Y Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M4Y, "m4y.rds")

#plot for M5A
M5A <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M5A") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M5A Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M5A, "m5a.rds")

#plot for M5B
M5B <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M5B") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M5B Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M5B, "m5b.rds")

#plot for M5C
M5C <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M5C") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M5C Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M5C, "m5c.rds")

#plot for M5R
M5R <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M5R") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M5R Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M5R, "m5r.rds")

#plot for M5S
M5S <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M5S") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M5S Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M5S, "m5s.rds")

#plot for M5T
M5T <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M5T") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M5T Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M5T, "m5t.rds")

#plot for M5V
M5V <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M5V") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M5V Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M5V, "m5v.rds")

#plot for M6A
M6A <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M6A") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M6A Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M6A, "m6a.rds")

#plot for M6C
M6C <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M6C") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M6C Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M6C, "m6c.rds")

#plot for M6E
M6E <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M6E") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M6E Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M6E, "m6e.rds")

#plot for M6G
M6G <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M6G") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M6G Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M6G, "m6g.rds")

#plot for M6H
M6H <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M6H") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M6H Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M6H, "m6h.rds")

#plot for M6J
M6J <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M6J") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M6J Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M6J, "m6j.rds")

#plot for M6P
M6P <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M6P") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M6P Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M6P, "m6p.rds")

#plot for M8Y
M8Y <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M6A") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M8Y Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M8Y, "m8y.rds")

#plot for M9V
M9V <- postal_code_data %>% 
  filter(SHELTER_POSTAL_CODE == "M9V") %>% 
  ggplot(mapping = aes(x = OCCUPANCY_DATE,
                       y = AVG_ORATE))+
  geom_line(aes(group = 1), colour = "steelblue")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = NULL,
       y = "Percent of Shelter Beds Occupied",
       title = "M9V Shelter Data in 2020")+
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "#bebebe"))
write_rds(M9V, "m9v.rds")