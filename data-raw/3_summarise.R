### summarizing to remove "Variety" and "Intended Use" would significantly reduce
### file size, while still being useful for many analyses

### Type could be summarized
### Drop "Not Planted" and "Prevented",
### Sum "Planted", "Failed", and "Volunteer"?

## Could filter out Crop == 'CRP'
## Or acres < 1 or something like that...


#### Data type validation
## nchar, regex, range, consistency, 'NA' like values,
## spelling and grammar, uniqueness, table lookup
#### Simple range and constraint validation
#### Code and cross-reference validation
#### Structured validation of complex objects

#### Data validation actions
## enforcement : reject or change ?
## advisory or verification
## log



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




# fsa2 <- ungroup(fsa) %>%
#   mutate(Crop2=recode(
#     Crop,
#     `TOBACCO- CIGAR WRAPPER` = 'TOBACCO, Cigar Wrapper',
#     `TOBACCO CIGAR WRAPPER` = 'TOBACCO, Cigar Wrapper',
#     `TOBACCO- CIGAR WRAPPER ` = 'TOBACCO, Cigar Wrapper',
#     `KAMUT`='KHORASAN',
#     `COTTON- Upland` = 'COTTON, Upland',
#     `COTTON UPLAND` = 'COTTON, Upland',
#     `COTTON- ELS`='COTTON, ELS',
#     `COTTON ELS`='COTTON, ELS',
#     `SORGHUM- DUAL PURPOSE`='SORGHUM, Dual Purpose',
#     `SORGHUM DUAL PURPOSE`='SORGHUM, Dual Purpose',
#     `TURN AREAS`='IDLE',
#     `SPELTZ`='SPELT',
#     `311`='WILLOW SHRUB',
#     `380`='PITAYA/DRAGONFRUIT',
#     `381`='PAWPAW',
#     `421`='NONI',
#     `427`='WOLFBERRY/GOJI',
#     `RICE- Wild` = 'RICE, Wild',
#     `RICE WILD` = 'RICE, Wild',
#     `GARDEN HOME` = 'HOME GARDEN',
#     `GARDEN COMMERCIAL` = 'COMMERCIAL GARDEN', # null
#     `RICE- Sweet` = 'RICE, Sweet',
#     `RICE SWEET` = 'RICE, Sweet',
#     `TREES- Timber` = 'TIMBER',
#     `TREES TIMBER` = 'TIMBER'))
#
# fsa3 <- mutate(
#   fsa2,
#   Crop2=recode(
#     Crop2,
#     `TOBACCO CIGAR WRAPPER` = 'TOBACCO, Cigar Wrapper',
#     `TOBACCO- CIGAR WRAPPER ` = 'TOBACCO, Cigar Wrapper',
#     `COTTON UPLAND` = 'COTTON, Upland',
#     `COTTON ELS`='COTTON, ELS',
#     `SORGHUM DUAL PURPOSE`='SORGHUM, Dual Purpose',
#     `RICE WILD` = 'RICE, Wild',
#     `GARDEN HOME` = 'HOME GARDEN',
#     `GARDEN COMMERCIAL` = 'COMMERCIAL GARDEN',
#     `RICE SWEET` = 'RICE, Sweet',
#     `TREES TIMBER` = 'TIMBER'))
# # GROUND CHERRY # null
#
# problemCropCodes <- fsa3 %>% group_by(`Crop Code`) %>%
#   summarise(Crops = length(unique(Crop2))) %>%
#   filter(Crops > 1)
#
# semi_join(fsa2, problemCropCodes) %>%
#   select(Code=`Crop Code`, Crop2) %>% unique() %>% View()
#
# filter(fsa, !is.null(Crop)) %>%
#   group_by(`Crop Code`) %>%
#   summarise(Crops=length(unique(Crop))) %>%
#   filter(Crops > 1)
