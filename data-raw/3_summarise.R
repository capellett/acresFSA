### summarizing to remove "Variety" and "Intended Use" would significantly reduce
### file size, while still being useful for many analyses

### Type could be summarized
### Drop "Not Planted" and "Prevented",
### Sum "Planted", "Failed", and "Volunteer"?

## Could filter out Crop == 'CRP'
## Or acres < 1 or something like that...

#############
############# fsa_min
#############

# fsa_min <- fsa
# fsa_min[c(1:8)] <- lapply(fsa_min[c(1:8)], as_factor)
# track_dataset(fsa_min, 'fs_min 2.0')
#
# fsa_min %>% # select(-`Crop Type`, -`County Name`, -`Intended Use`) %>%
#   group_by(Year, `State County Code`, `Crop Code`, `Irrigation Practice`, `Type`, `Intended Use`) %>%
#   summarise(Acres=sum(Acres)) %>%
#   ungroup() %>%
#   track_dataset('remove crop type, county name, and KEEP intended use, summarise') %>%
#   saveRDS('fsa_min.rds')
