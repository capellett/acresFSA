
library(dplyr)
library(ggplot2)
library(acresFSA)

acresFSA %>%
  filter(State == "SOUTH CAROLINA") %>% # & County == "ORANGEBURG") %>%
  group_by(Year, Crop,
           `Irrigation Practice`) %>%
  summarise(Acres=sum(Acres)) %>%
  ggplot(aes(x=Year, y=Acres, color=`Irrigation Practice`)) +
  geom_line() +
  facet_wrap("Crop", scales="free_y")



acresFSA %>%
  filter(State == "SOUTH CAROLINA" &
           County %in% c('MARLBORO', 'WILLIAMSBURG', 'FLORENCE', 'DILLON',
                         'HORRY', 'CHESTERFIELD')
         & `Irrigation Practice`=='Irrigated') %>%
  group_by(Year, County,
           `Irrigation Practice`) %>%
  summarise(Acres=sum(Acres)) %>%
  ggplot(aes(x=Year, y=Acres, color=`Irrigation Practice`)) +
  geom_line() +
  facet_wrap("County", scales="free_y")

Marlboro, Williamsburg, Florence, Dillon, Horry, and Chesterfield
