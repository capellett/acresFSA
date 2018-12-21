## Read in, tidy, and combine FSA data from 2009-2018,
## Design goals:
## 1. Protect the integrity of the data.
## 2. Enhance comparability over time.
## 3. Reduce file size & read speed.



library(readxl)
library(tidyverse)

## A function to track object.size, nrow, & ncol
## Add runtime to log.
## (color by col type? stripe width by col size?)
track_dataset <- function(df, step_name=NA_character_,
                          track_log='data_manipulation_log', step_inc=1 ) {
  track_log_entry <- list(step_name=step_name, step_inc=step_inc,
                          nrow=nrow(df), ncol=ncol(df), object.size=object.size(df)[1])
  if (exists(track_log)) {
    track_log_entry$.data <- get(track_log)
    track_log1 <- do.call(add_row, track_log_entry)
  } else {
    track_log1 <- as.data.frame(track_log_entry, stringsAsFactors=FALSE)
  }
  assign(track_log, track_log1, globalenv())
  print(step_name)
  invisible(df)
  #### Todo: Time each step
}

##### fsa0: The early years, 2009 and 2010
## did not contain information on the irrigation practice.
## Some fields were labelled inconsistently.
## Create 'Irrigation Practice' and 'State County Code' columns for forward compatibility
## Gather the columns for each type of acreage in to 'Type' and 'Acres' columns.
## Remove the last word of each entry in the Type column (the word "Acres")
## Many rows have been created with an Acres value of 0.
## In this case, 0 is conceptually equivalent to missing data.
## "The system of data collection which the FSA has in place did not detect any
## of such acreage in such county." != "FSA knows that there were 0 such acres."
## I think I'm arguing that 0 == NA == NULL in this case,
## and the plots will look cleaner without excess lines.
fsa0 <- bind_rows(
  `2009`=read_xlsx('data-raw\\downloaded excel files\\2009_fsa_acres_detail_final_7.xlsx') %>%
    rename(`State Abbrev`='State Name') %>% track_dataset('2009'),
  `2010`=read_xlsx('data-raw\\downloaded excel files\\2010_fsa_acres_detail_final_5.xlsx') %>%
    rename(`State Code`=State) %>% track_dataset('2010'),
  .id='Year') %>% track_dataset('fsa0') %>%
  mutate(`Irrigation Practice`='T',
         `State County Code`= paste0(`State Code`, `County Code`)) %>%
  gather('Type', 'Acres', `Planted Acres`:`Failed Acres`) %>%
  mutate(Type = gsub("\\s*\\w*$", "", Type)) %>%
  track_dataset('fsa0 tidy') %>%
  filter(Acres != 0 & !is.na(Acres)) %>%
  track_dataset('fsa0 filter 0s') ## 310,076 obs of 14 variables. 35,177,520 bytes



##### fsa1: 2011 transition year
### 2011 data includes irrigation practice,
### but is formatted differently from the subsequent years.
### I deleted some newlines in the column labels of 2011 by hand in excel.
fsa11 <- read_xlsx('data-raw\\downloaded excel files\\2011_fsa_acres_detail_jan2012.xlsx') %>%
  track_dataset('2011')
## 162,334 obs of 27 variables.
## The addition of irrigation data doubles
## the set of columns representing different types of acreage.

## Test input data before making assumptions:
## Do the total columns all add up correctly?
## If so, then I can safely remove some redundant data.
# fsa11b <- fsa11 %>% mutate(
#   PlantT=`Planted Irrigated` + `Planted Nonirrigated`,
#   PreventT = `Prevented Irrigated` + `Prevented Nonirrigated`,
#   FailT = `Failed Irrigated` + `Failed Nonirrigated`,
#   VolT = `Volunteer Irrigated` + `Volunteer Nonirrigated`,
#   NPlantT = `Not Planted Irrigated` + `Not Planted Nonirrigated`,
#   PandFT = `Plant and Fail Irrigated` + `Plant and Fail Nonirrigated`,
#   PandFI = `Planted Irrigated` + `Failed Irrigated`,
#   PandFN = `Planted Nonirrigated` + `Failed Nonirrigated`) %>%
#   mutate(
#     PlantTotalTest = PlantT-`Planted Total`,
#     PreventTotalTest = PreventT-`Prevented Total`,
#     FailTotalTest = FailT-`Failed Total`,
#     VolunteerTotalTest = VolT-`Volunteer Total`,
#     NotPlantedTotalTest = NPlantT-`Not Planted Total`,
#     PlantedFailedTotalTest = PandFT-`Plant and Fail Total`,
#     PandFIrrigatedTest = PandFI-`Plant and Fail Irrigated`,
#     PandFNonTest = PandFN-`Plant and Fail Nonirrigated`)
#
# summary(fsa11b[,36:43])
# # filter(fsa11b, PlantedFailedTotalTest != 0) %>% View()
# all.equal(fsa11b$`Plant and Fail Total`, fsa11b$PandFT)
# ## it checks out.
# rm(fsa11b)

### Remove unneeded columns and tidy up the data.
### Could remove the acreage totals, but I want to plot those values.
### Instead, remove the nonirrigated acreages, and also the plant-and-fail sums.
### Gather the remaining acreage types in to 2 columns: Type & Acres
### Create 'Irrigation Practice' column from the Type column,
### Trim the last word from each Type entry
### Remove 0's and missing data (if any?)
fsa11b <- select(fsa11, # -`Crop Type`,
                -`Planted Nonirrigated`, -`Prevented Nonirrigated`,-`Failed Nonirrigated`,
                -`Volunteer Nonirrigated`, -`Not Planted Nonirrigated`,-`Plant and Fail Nonirrigated`,
                -`Plant and Fail Total`, -`Plant and Fail Irrigated`) %>%
  track_dataset('fsa11 remove nonirrigated') %>%
  gather('Type', 'Acres', `Planted Total`:`Not Planted Irrigated`) %>%
  track_dataset('fsa11 gather type') %>%
  mutate(`Irrigation Practice` = if_else(Type %in% c(
    'Planted Irrigated', 'Prevented Irrigated', 'Failed Irrigated',
    'Volunteer Irrigated', 'Not Planted Irrigated'), 'I', 'T'),
    `State County Code`= paste0(`State Code`, `County Code`),
    Year='2011') %>%
  track_dataset('fsa11 add irrigated practice') %>%
  mutate(Type = gsub("\\s*\\w*$", "", Type)) %>%
  filter(Acres != 0 & !is.na(Acres)) %>%
  track_dataset('fsa11 filter missing data')
## 232618 obs. of 14 variables.

#### Create State_table to help clean-up
## variable spellings of states or abbreviations
State_table <-
  full_join(
    select(fsa11b, `State Code`, `State Name`) %>% unique(),
    select(fsa0, `State Code`, `State Abbrev`) %>% unique()) %>%
  mutate(`State Name`=if_else(`State Abbrev`=='MP', 'MARIANA ISLANDS', `State Name`))

fsa1 <- bind_rows(fsa0, fsa11b) %>%
  select(-`State Name`, -`State Abbrev`) %>%
  left_join(State_table) %>% track_dataset('fsa1')
## 542,694 obs. of 15 variables. 65,605,208 bytes

# all.equal(fsa1[,c('State Code', 'State Name', 'State Abbrev')] %>% unique(),
#           State_table) ## TRUE
## This checks out...

rm(fsa0, fsa11, fsa11b)


#### fsa2: 2012-present: the dataset reaches a steady state.
### The remaining files are more uniform.
select_cols <- function(x) {
  select(x, -`Planted and Failed Acres`) }

read_fsa <- function(x) {
  read_xlsx(x, sheet='county_data') %>%
    select_cols() %>%
    filter(!is.na(County))
}

fsa2 <- bind_rows(
  `2012`=read_fsa('data-raw\\downloaded excel files\\2012_fsa_acres_jan_2013.xlsx') %>%
    rename(`Failed Acres`='Failded Acres') %>% track_dataset('2012'),
  `2013`=read_fsa('data-raw\\downloaded excel files\\2013_fsa_acres_jan_2014.xlsx') %>% track_dataset('2013'),
  `2014`=read_fsa('data-raw\\downloaded excel files\\2014_fsa_acres_jan2014.xlsx') %>% track_dataset('2014'),
  `2015`=read_fsa('data-raw\\downloaded excel files\\2015_fsa_acres_01052016.xlsx') %>% track_dataset('2015'),
  `2016`=read_fsa('data-raw\\downloaded excel files\\2016_fsa_acres_010417.xlsx') %>% track_dataset('2016'),
  `2017`=read_fsa('data-raw\\downloaded excel files\\2017_fsa_acres_010418.xlsx') %>% track_dataset('2017'),
  `2018`=read_fsa('data-raw\\downloaded excel files\\2018_fsa_acres_100118.xlsx') %>% track_dataset('2018'),
  .id='Year')  %>% track_dataset('fsa2') ## 1,237,908 obs of 17 variables. 158,999,384 bytes

## calculate Total acres, make tidy
fsa2b <- fsa2 %>% track_dataset('fsa2012-2018') %>%
  gather('Type', 'Acres', `Planted Acres`:`Not Planted Acres`) %>% ## 6,189,540 obs. of 13 variables. 644,259,288 bytes
  track_dataset('fsa2 gather acreage types') %>%
  group_by(`State Code`, County, `County Code`, `State County Code`, Year,
                  `Crop`, `Crop Type`, `Crop Code`, `Intended Use`, `Irrigation Practice`, `Type`) %>%
  summarise(Acres=sum(Acres, na.rm=TRUE)) %>%
  track_dataset('fsa2 sum acres by county, crop, use, irrigation, and type') %>%
  ungroup() %>%
  track_dataset('fsa2 ungrouped') %>%
  spread(`Irrigation Practice`, Acres) %>%
  track_dataset('fsa2 spread irrigation practice') %>%
    mutate() %>%
  mutate(Type = gsub("\\s*\\w*$", "", Type),
         I= if_else(is.na(I), 0, I),
         N= if_else(is.na(N), 0, N),
         O= if_else(is.na(O), 0, O)) %>%
  mutate(`T`=I+N+O) %>% select(-N) %>%
  track_dataset('fsa2 calculate total, remove unirrigated') %>%
  gather(`Irrigation Practice`, 'Acres', `I`:`T`) %>%
  track_dataset('fsa2 gather irrigation practice') %>%
  filter(Acres != 0 & !is.na(Acres)) %>% ## 1522162
  track_dataset("fsa2 remove 0's and missing data") %>%
  rename(`County Name`=County, `Crop Type Name`='Crop Type', `Crop Name`=Crop)

State_table[55,] <- list("60","AMERICAN SAMOA", "AS")
fsa2b <- inner_join(fsa2b, State_table) %>% track_dataset('fsa2 state join')

fsa <- bind_rows(fsa1, fsa2b) %>% track_dataset('fsa')
saveRDS(fsa, 'data-raw//acresFSA_compilation.rds')
rm(fsa1, fsa2, fsa2b)

write.csv(data_manipulation_log, 'data_compilation_log.csv')

### TODO!!!: Read in farm counts
# read_farm_count <- function(x) {read_xlsx(x, sheet='farm_count')}



