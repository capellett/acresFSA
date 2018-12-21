#############
############# data validation
#############

fsa <- readRDS('data-raw//acresFSA_compilation.rds')
fsa <- mutate(fsa, `County Name` = toupper(`County Name`))

### TODO: use these libraries.
### install.packages('validate', 'errorlocate); library(validate); library(errorlocate)

## Convert any values of 'NULL', 'null', 'NA', or 'na' to NA_character_
unique(nchar(fsa$`State Code`))==2
unique(nchar(fsa$`County Code`)) == 3

## Test that the State County Code is truly the concatenation of state and county codes.
filter(fsa, `State County Code` != paste0(`State Code`, `County Code`)) %>%
  nrow(.) == 0


duped <- function(x) { duplicated(x) | duplicated(x, fromLast=TRUE) }

### Return cases where 1:1 fails
one_to_one_fails <- function(df, id1, id2) {
  unique(df[,c(id1, id2)]) %>%
    .[duped(.[,id1]) | duped(.[,id2]),] }

## Test that State Code, State Name, and State Abbrev are all in agreement
## with reference table (?)
one_to_one_fails(fsa, 'State Code', 'State Name') %>% nrow() == 0
one_to_one_fails(fsa, 'State Code', 'State Abbrev') %>% nrow() == 0
one_to_one_fails(fsa, 'State Abbrev', 'State Name') %>% nrow() == 0
##### It checks out.
fsa <- select(fsa, -`State Code`, -`State Abbrev`, -`County Code`)
fsa <- rename(fsa, State='State Name', County='County Name')

## Test that State+County is 1:1 with 'State County Code'
one_to_one_fails(fsa, c('State', 'County'), 'State County Code') %>% nrow()
#### Should be nrow 0, is nrow 835...
#### Caused by variation in county names

# length(unique(fsa$`State County Code`))
# filter(fsa, Year==2016)$`State County Code` %>%
#   unique() %>% length()
#### Apparently, not all state county codes are in every year...

county_name_recode <-
  select(fsa, 'State County Code', 'County') %>%
  unique() %>%
  group_by(`State County Code`) %>%
  do({
    county_names <- .$`County` %>%
      str_replace_all("\\.", '') %>%
      str_replace_all('&#39;', '')
    county_names <- gsub(",.*","",county_names) %>%
      unique()
    tibble(County= county_names[which.max(nchar(county_names))])
  }) %>% ungroup()

fsa <- select(fsa, -County) %>% inner_join(county_name_recode)

## Test again that 'State'+'County' is 1:1 with 'State County Code'
one_to_one_fails(fsa, c('State', 'County'), 'State County Code') %>% nrow() == 0
#### Then drop State County Code
fsa <- select(fsa, -`State County Code`)


#### Just like for the State_table abd county_name_recode,
###  create an authoritative reference table for:
### Crops & crop types
### that will allow dropping code columns.

## Test that Crop Name is 1:1 with Crop Code
one_to_one_fails(fsa, 'Crop Name', 'Crop Code')
### Should be 0, is 49.

recode_column_with_table <- function(df, col_index, recode_table) {
  df2 <- df
  names(df2)[col_index] <- 'from'
  print(paste0('recoding ', names(df)[col_index]))
  df[col_index] <- left_join(df2, recode_table, by='from') %>%
    mutate(to=if_else(is.na(to), from, to)) %>% .$to
  invisible(df) }

### Recode crop names.
table(fsa[c('Crop Name', 'Year')]) %>% write.csv('data-raw\\Crop Names per Year.csv')
crop_name_recode <- read_xlsx("data-raw\\FSA - Clean up the names.xlsx", sheet = 'crop name recode')
fsa <- recode_column_with_table(fsa, 3, crop_name_recode)

one_to_one_fails(fsa, 'Crop Name', 'Crop Code')
### was 49, now is 22... will come back to this.

filter(fsa, !is.na(`Crop Type`)) %>%
  one_to_one_fails('Crop Type Name', 'Crop Type') %>% View() ## 420 rows.

## Test that Crop Name + Crop Type Name is 1:many with Crop Type
fsa[,c('Crop Name', 'Crop Type Name', 'Crop Type')] %>%
  filter(!is.na(`Crop Type`)) %>% unique() %>%
  .[duped(.[,c('Crop Name', 'Crop Type Name')]),]
## Should be nrow 0. is nrow 0.

### Recode Crop Type Names
table(fsa_[c('Crop Type Name', 'Year')]) %>% write.csv('data-raw\\Crop Type Names per Year.csv')
crop_type_name_recode <- read_xlsx("data-raw\\FSA - Clean up the names.xlsx",
                                   sheet='crop type name recode')
fsa <- recode_column_with_table(fsa, 4, crop_type_name_recode)

### Recode crop names and crop type names together ...
### Make a table with unique crop code, crop name, crop type name, and crop type - by year
crop_classification_review <- fsa %>%
  mutate(`Crop Type Name` = if_else(
    `Crop Type Name`=='NULL', NA_character_, `Crop Type Name`)) %>%
  group_by(Year, `Crop Code`, `Crop Name`, `Crop Type Name`) %>%
  summarise(Acres=sum(Acres, na.rm=T)) %>%
  ungroup() %>%
  spread('Year', 'Acres')

write.csv(crop_classification_review, 'data-raw\\Crop Classification Review.csv')
crop_name_and_type_recode <- read_xlsx("data-raw\\FSA - Clean up the names.xlsx",
                                       sheet='crop name and type recode',
                                       col_types='text') %>%
  mutate(`Crop Code`=str_pad(`Crop Code`, 4, 'left', '0'))

#### Recode Crop Names (again)
crop_name_recode2 <- select(crop_name_and_type_recode,
                            `Crop Code`, `Crop Name 2`) %>%
  unique() %>%
  filter(!is.na(`Crop Name 2`))

fsa <- left_join(fsa, crop_name_recode2) %>%
  mutate(`Crop Name`=if_else(is.na(`Crop Name 2`), `Crop Name`, `Crop Name 2`)) %>%
  select(-`Crop Name 2`)
one_to_one_fails(fsa, 'Crop Name', 'Crop Code')
## from 22 to 16 rows.
## actually, many are doubled because I combined tobacco and cotton crop types.

## Call "turn area"s "idle" with type "turn area".
filter(fsa, `Crop Code`=='0105') %>%
  select(`Crop Name`, `Crop Type Name`) %>%
  unique()
fsa <- mutate(fsa, `Crop Type Name`=if_else(`Crop Name`=='TURN AREAS',
                                            'TURN AREAS', `Crop Type Name`))
fsa <- mutate(fsa, `Crop Name`=if_else(`Crop Code`=='0105',
                                       'IDLE', `Crop Name`))
#### Recode Crop Type Names (again)
crop_type_name_recode2 <- select(crop_name_and_type_recode, -`Crop Name`, -`Crop Name 2`) %>%
  unique() %>%
  filter(!is.na(`Crop Type Name 2`))

fsa <- left_join(fsa, crop_type_name_recode2) %>%
  mutate(`Crop Type Name`=if_else(is.na(`Crop Type Name 2`),
                                  `Crop Type Name`, `Crop Type Name 2`)) %>%
  select(-`Crop Type Name 2`)

# is.missing <- function(x) {is.null(x) | is.na(x)}
#### If Crop Type Name is missing, so is Crop Type. So What good is Crop Type?
# filter(fsa_, is.na(`Crop Type Name`) & !is.na(`Crop Type`))
# filter(fsa_, is.null(`Crop Type Name`) & !is.null(`Crop Type`))
# filter(fsa_, is.missing(`Crop Type Name`) & !is.missing(`Crop Type`))
# unique(fsa_[c('Crop Name', 'Crop Type Name', 'Crop Type')]) %>%
#   filter(!is.na(`Crop Type`) & `Crop Type` != 'NULL') %>%
#   write.csv('Crop Type Review.csv')

fsa <- select(fsa, -`Crop Type`, -`Crop Code`) %>%
  rename(Crop='Crop Name', Variety='Crop Type Name')

fsa <- group_by(fsa, Year, Crop, State, County, Variety, `Irrigation Practice`,
                `Intended Use`, Type) %>%
  summarise(Acres=sum(Acres, na.rm=TRUE)) %>%
  ungroup() ### Why did I loose 800 rows? because there had been inconsistent names...

track_dataset(fsa, 'Tested and removed redundant columns')

filter(fsa, Variety %in% c('NULL', 'null', 'NA', 'na'))
## 72,017 rows.

convert_NAs <- function(x) {
  if_else(x %in% c('NULL', 'null', 'NA', 'na'),
          NA_character_, x) }

fsa <- mutate(fsa, Variety = convert_NAs(Variety))

filter(fsa, Crop %in% c('NULL', 'null', 'NA', 'na')) ## 0 rows
filter(fsa, is.na(Crop)) ## 0 rows

filter(fsa, `Irrigation Practice` %in% c('NULL', 'null', 'NA', 'na')) ## 0 rows
filter(fsa, is.na(`Irrigation Practice`)) ## 0 rows

filter(fsa, `Intended Use` %in% c('NULL', 'null', 'NA', 'na')) ## 0 rows
filter(fsa, is.na(`Intended Use`)) ## 313,237 rows

filter(fsa, Type %in% c('NULL', 'null', 'NA', 'na')) ## 0 rows
filter(fsa, is.na(Type)) ## 0 rows

filter(fsa, is.na(Acres)) ## 0 rows

rm(county_name_recode, crop_classification_review, crop_name_and_type_recode,
   crop_name_recode, crop_name_recode2, crop_type_name_recode,
   crop_type_name_recode2, State_table)

#####
##### Intended Use review
#####
group_by(fsa, Year, `Intended Use`) %>%
  summarise(Acres=sum(Acres, na.rm=TRUE)) %>%
  spread(Year, Acres) %>% write.csv('data-raw\\Intended Use Review.csv')

# filter(fsa, `Intended Use`=='LF') ## sesame leaves?

intended_use_recode <- read_xlsx(
  "data-raw\\FSA - Clean up the names.xlsx",
  sheet = 'intended use recode 1')
fsa <- recode_column_with_table(fsa, 7, intended_use_recode)

### `Intended Use` from 2009 to 2012 included Conservation Practices
## 2009 - 2011 as numerics, 2012 as CP## Description
## from 2013-2018, Conservation Practices appear in the 'Variety' Column

fsa <- mutate(fsa, `Intended Use`= if_else(
  is.na(as.numeric(`Intended Use`)),
  `Intended Use`, paste0('CP', `Intended Use`)))

group_by(fsa, Year, `Intended Use`) %>%
  summarise(Acres=sum(Acres, na.rm=TRUE)) %>%
  spread(Year, Acres) %>% write.csv('data-raw\\Intended Use Review2.csv')

intended_use_recode2 <- read_xlsx(
  "data-raw\\FSA - Clean up the names.xlsx",
  sheet = 'intended use recode 2')
fsa <- recode_column_with_table(fsa, 7, intended_use_recode2)

group_by(fsa, Crop, `Intended Use`) %>%
  summarise(Acres=sum(Acres, na.rm=TRUE)) %>%
  spread(`Intended Use`, Acres) %>% View()

### All of those CP## entries in `Intended Use` have Crop=='CRP'
filter(fsa, Crop != 'CRP')$`Intended Use` %>% unique()
### Yea, pretty much.

# filter(fsa, Crop=='CRP' | is.na(Crop))$Type %>% unique()

filter(fsa, Crop=='CRP' | is.na(Crop)) %>%
  group_by(Crop, Year, Variety, `Intended Use`) %>%
  summarise(Acres=sum(Acres, na.rm=TRUE)) %>%
  spread(Year, Acres) %>% View()

fsa_crp <- filter(fsa, Crop=='CRP') %>%
  mutate(Variety = if_else(is.na(Variety),
                           `Intended Use`, Variety))

group_by(fsa_crp, Crop, Year, Variety, `Intended Use`) %>%
  summarise(Acres=sum(Acres, na.rm=TRUE)) %>%
  spread(Year, Acres) %>% write.csv('CRP Variety Review.csv')

CRP_variety_recode <- read_xlsx(
  "data-raw\\FSA - Clean up the names.xlsx",
  sheet = 'CRP variety recode')

fsa_crp <- recode_column_with_table(fsa_crp, 5, CRP_variety_recode)

#### Some acronyms in the CRP varieties:
### FWP	Farmable Wetland Program
### MPL	Marginal Pasture Land
### EFCRP	Emergency Forest Conservation Reserve Program
### SAFE	State Acres for …
### WL	Wildlife

fsa_crp <- mutate(fsa_crp, `Intended Use` = NA_character_)
select(fsa_crp, Variety, `Intended Use`) %>% unique() %>% View()

fsa <- bind_rows(filter(fsa, Crop != 'CRP'), fsa_crp)

rm(fsa_crp, CRP_variety_recode)

rm(intended_use_recode, intended_use_recode2)

acresFSA <- mutate(fsa,
                   Year= as.numeric(Year),
                   State=as.factor(State),
                   County=as.factor(County),
                   Type=as.factor(Type),
                   `Irrigation Practice`=as.factor(`Irrigation Practice`),
                   Crop=as.factor(Crop),
                   Variety=Variety,
                   `Intended Use`=as.factor(`Intended Use`),
                   Acres=Acres) %>%
  track_dataset("Convert some column types")

acresFSA <- acresFSA %>%
  select(Year, State, County, Type, `Irrigation Practice`, Crop, Variety, `Intended Use`,
         Acres)

usethis::use_data(acresFSA, overwrite=TRUE)
write.csv(data_manipulation_log, 'data_cleaning_log.csv')
