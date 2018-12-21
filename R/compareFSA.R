# ############################
# ## Compare with Census of Agriculture
#
# coa <- IrrigatedCropsByCounty %>%
#   # read.csv('../../Planning_Input/Agriculture/IrrigatedCropsByCounty.csv') %>%
#   filter(AllAcres != 0) %>%
#   mutate(County=as.character(County),
#          Crop=as.character(Commodity),
#          Year=as.numeric(Year)) %>%
#   rename(All=AllAcres, Irrigated=IrrigatedAcres) %>%
#   select(County, Year, Crop, All, Irrigated) %>%
#   gather('type', 'Acres', All:Irrigated)
# # names(coa)[1] <- 'County'
#
# coa_cnty <- IrrigatedAcresByCounty %>%
#   mutate(Year = as.numeric(as.character(Year)),
#          County = toupper(County))
#
# # fsa <- readRDS('data-raw//fsa.rds') %>%
# #   filter(State=='SOUTH CAROLINA' | State=='South Carolina' | State=='S CAROLIN') %>%
# #   mutate(Irrigation = recode(Irrigation,
# #                              N='Nonirrigated',
# #                              I='Irrigated',
# #                              O='Other'),
# #          County=recode(County,
# #                        CHARLSTON='Charleston',
# #                        CHESTRFLD='Chesterfield',
# #                        DARLNGTON='Darlington',
# #                        DORCHESTR='Dorchester',
# #                        GEORGETWN='Georgetown',
# #                        GREENVILL='Greenville',
# #                        ORANGEBRG='Orangeburg',
# #                        SPARTNBRG='Spartanburg',
# #                        WMSBURG='Williamsburg'),
# #          Crop=recode(Crop,
# #                      `COTTON, Upland`='COTTON',
# #                      `COTTON- Upland`='COTTON',
# #                      `COTTON UPLAND`='COTTON',
# #                      `COTTON  UPLAND`='COTTON',
# #                      `GARDEN  HOME`='HOME GARDEN',
# #                      #`GARDEN  HOME` = 'HOME GARDEN',
# #                      `GARDEN  COMMERCIAL` = 'COMMERCIAL GARDEN',
# #                      `FALLOW`='IDLE',
# #                      `POTATOES SWEET`='SWEET POTATOES',
# #                      `TREES TIMBER`='TIMBER',
# #                      `TREES  TIMBER`='TIMBER',
# #                      `TREES- Timber`='TIMBER',
# #                      `SORGHUM-  DUAL PURPOSE`='SORGHUM',
# #                      `SORGHUM FORAGE`='SORGHUM',
# #                      `SORGHUM  DUAL PURPOSE`='SORGHUM',
# #                      `TOBACCO BURLEY`='TOBACCO',
# #                      `TOBACCO FLUE CURED`='TOBACCO',
# #                      `WATERMELON`='WATERMELONS')) %>%
# #   group_by(Year, County, Crop, Irrigation) %>%
# #   summarise(Acres=sum(Acres, na.rm=TRUE)) %>%
# #   ungroup() %>%
# #   mutate(Year=as.numeric(Year))
#
# unique(fsa$Crop)
#
# fsa_sc <- group_by(fsa, Year, Irrigation) %>%
#   summarise(Acres=sum(Acres, na.rm=TRUE)) %>%
#   # filter(Irrigation %in% c('Irrigated', 'Nonirrigated')) %>%
#   ungroup() %>%
#   spread(Irrigation, Acres) %>%
#   rowwise() %>%
#   mutate(All=sum(Irrigated, Nonirrigated, Other, na.rm=TRUE)) %>%
#   select(Year, All, Irrigated, Other)
#
# coa_sc <- group_by(coa_cnty, Year) %>%
#   summarise(All=sum(All, na.rm=T),
#             Irrigated=sum(Irrigated, na.rm=T))
#
# fsa_and_coa_irrigation_sc <- inner_join(fsa_sc, coa_sc, by='Year', suffix=c('_fsa', '_coa'))
#
# fsa_cnty <- group_by(fsa, Year, County, Irrigation) %>%
#   summarise(Acres=sum(Acres, na.rm=T)) %>%
#   ungroup() %>%
#   mutate(Year=as.numeric(Year),
#          County=toupper(County)) %>%
#   # filter(Irrigation %in% c('Irrigated', 'Nonirrigated')) %>%
#   spread(Irrigation, Acres) %>%
#   rowwise() %>%
#   mutate(All=sum(Irrigated, Nonirrigated, Other, na.rm=TRUE)) %>%
#   ungroup() %>%
#   select(Year, County, All, Irrigated, Other)
#
# cropscape_cnty_crop <- readRDS('_input//CountyCrops.rds') %>%
#   rename(Crop=LandUse) %>%
#   filter(!(Crop %in% c('Forest', 'Open Water', 'Developed/Open Space',
#                        'Developed/Low Intensity', 'Developed/Med Intensity',
#                        'Developed/High Intensity', 'Barren', 'Deciduous Forest',
#                        'Evergreen Forest', 'Mixed Forest', 'Shrubland',
#                        'Grassland/Pasture', 'Woody Wetlands', 'Herbaceous Wetlands',
#                        'Wetlands', 'Background', 'Other Hay/Non Alfalfa', 'Fallow/Idle Cropland'))) %>%
#   mutate(County=toupper(County),
#          Crop=toupper(Crop),
#          Source='Cropland Data Layer',
#          type='All') %>%
#   mutate(Crop=recode(Crop,
#                      `DRY BEANS`='BEANS',
#                      `WINTER WHEAT`='WHEAT',
#                      `SPRING WHEAT`='WHEAT',
#                      `SUNFLOWER`='SUNFLOWERS',
#                      `WATERMELON`='WATERMELONS',
#                      `SOD/GRASS SEED`='GRASS',
#                      `HAY & HAYLAGE TOTALS`='GRASS',
#                      `GRASSES & LEGUMES TOTALS`='GRASS',
#                      `DBL CROP BARLEY/CORN`='DOUBLE CROP',
#                      `DBL CROP BARLEY/SORGHUM`='DOUBLE CROP',
#                      `DBL CROP BARLEY/SOYBEANS`='DOUBLE CROP',
#                      `DBL CROP CORN/SOYBEANS`='DOUBLE CROP',
#                      `DBL CROP OATS/CORN`='DOUBLE CROP',
#                      `DBL CROP SOYBEANS/COTTON`='DOUBLE CROP',
#                      `DBL CROP SOYBEANS/OATS`='DOUBLE CROP',
#                      `DBL CROP WINWHT/CORN`='DOUBLE CROP',
#                      `DBL CROP WINWHT/COTTON`='DOUBLE CROP',
#                      `DBL CROP WINWHT/SORGHUM`='DOUBLE CROP',
#                      `DBL CROP WINWHT/SOYBEANS`='DOUBLE CROP'))
#
#
# cropscape_crop <- group_by(cropscape_cnty_crop, Year, Crop, Source, type) %>%
#   summarise(Acres=sum(Acres, na.rm=T)) %>%
#   ungroup()
#
# cropscape_cnty <- group_by(cropscape_cnty_crop, Year, County, Source, type) %>%
#   summarise(Acres=sum(Acres, na.rm=T)) %>%
#   ungroup()
#
# fsa_coa_cropscape_irrigation_by_county <- bind_rows(
#   mutate(fsa_cnty, Source='Farm Service Agency'),
#   mutate(select(coa_cnty, 1:4), Source='Census of Agriculture')) %>%
#   gather('type', 'Acres', All:Other) %>%
#   filter(!is.na(Acres)) %>%
#   bind_rows(cropscape_cnty)
#
# test <- filter(fsa_coa_cropscape_irrigation_by_county, type=='All') %>%
#   group_by(County) %>%
#   summarise(sort=sum(Acres, na.rm=TRUE)) %>%
#   mutate(rank=rank(-sort)) %>%
#   mutate(rank=if_else(rank<10, paste0(0, rank), as.character(rank))) %>%
#   select(County, rank) %>%
#   right_join(fsa_coa_cropscape_irrigation_by_county) %>%
#   mutate(label=paste0(rank, ' ', County))
#
# ggplot(test, aes(x=Year, y=Acres/1000)) +
#   geom_line(aes(group=interaction(type, Source), alpha=.5)) +
#   geom_point(aes(colour=type, shape=Source), size=1) +
#   facet_wrap('label', scales='free_y') +
#   scale_y_continuous(name='Acres (thousands)', labels=scales::comma_format()) +
#   # scale_linetype_manual(values=c('dotted', 'solid', 'dashed')) +
#   scale_x_continuous(breaks=c(1997, 2007, 2017)) +
#   scale_colour_manual(values=c('black', 'blue', 'red')) +
#   theme_bw() +
#   ggtitle(label='Total Cultivated and Irrigated Area per County',
#           subtitle='A comparison of several datasets, with counties ordered by cultivated area') +
#   theme(legend.position=c(1,0), legend.direction='vertical',
#         legend.box='horizontal',
#         legend.justification=c(1,.5),
#         legend.spacing=unit(10, units="points"))
#
# fsa_crop <- group_by(fsa, Year, Crop, Irrigation) %>%
#   summarise(Acres=sum(Acres, na.rm=T)) %>%
#   ungroup() %>%
#   spread(Irrigation, Acres) %>%
#   rowwise() %>%
#   mutate(All=sum(Irrigated, Nonirrigated, Other, na.rm=TRUE)) %>%
#   select(Year, Crop, All, Irrigated, Other) %>%
#   gather('type', 'Acres', All:Other) %>%
#   filter(!is.na(Acres))
#
# # unique(fsa_crop$Crop)
#
# ggplot(fsa_crop, aes(x=Year, y=Acres, linetype=type)) +
#   geom_line() +
#   # geom_point() +
#   theme_bw() +
#   facet_wrap('Crop', scales='free_y')
#
# coa_crop <- group_by(coa, Year, Crop, type) %>%
#   summarise(Acres=sum(Acres, na.rm=T)) %>%
#   ungroup()
#
# ggplot(coa_crop, aes(x=Year, y=Acres, linetype=type)) +
#   geom_line() +
#   theme_bw() +
#   facet_wrap('Crop', scales='free_y')
#
# ggplot(cropscape_crop, aes(x=Year, y=Acres)) +
#   geom_line() +
#   theme_bw() +
#   facet_wrap('Crop', scales='free_y')
#
#
# fsa_and_coa_irrigation_by_crop <- bind_rows(
#   semi_join(fsa_crop, coa_crop, by='Crop') %>%
#     mutate(Source='Farm Service Agency'),
#   semi_join(coa_crop, fsa_crop, by='Crop') %>%
#     mutate(Source='Census of Agriculture') )
#
# fsa_coa_cropscape_irrigation_by_crop <- bind_rows(
#   fsa_and_coa_irrigation_by_crop,
#   semi_join(cropscape_crop, fsa_and_coa_irrigation_by_crop,
#             by='Crop') )
#
# # add_sorting_label <- function(table, label_column, sorting_column) {
# #   group_by(table, label_column) %>%
# #     summarise(sort=sum(sorting_column, na.rm=TRUE))}
#
# # test <- left_join(fsa_and_coa_irrigation_by_crop, 'Crop', 'Acres')
#
# test2 <- filter(fsa_coa_cropscape_irrigation_by_crop, type=='All') %>%
#   # filter(fsa_and_coa_irrigation_by_crop, type=='All') %>%
#   group_by(Crop) %>%
#   summarise(sort=sum(Acres, na.rm=TRUE)) %>%
#   mutate(rank=rank(-sort)) %>%
#   mutate(rank=if_else(rank<10, paste0(0, rank), as.character(rank))) %>%
#   select(Crop, rank) %>%
#   right_join(fsa_coa_cropscape_irrigation_by_crop) %>%
#   mutate(label=paste0(rank, ' ', Crop))
#
# ggplot(test2,
#        aes(x=Year, y=Acres/1000)) +
#   geom_line(aes(group=interaction(type, Source)), alpha=.5) +
#   geom_point(aes(colour=type, shape=Source), size=1) +
#   scale_x_continuous(breaks=c(1997, 2007, 2017)) +
#   scale_y_continuous(
#     name='Acres (thousands)',
#     labels=scales::comma_format()) +
#   scale_linetype_manual(values=c('dotted', 'solid')) +
#   scale_colour_manual(values=c('black', 'blue', 'gray')) +
#   facet_wrap('label', scales='free_y') +
#   theme_bw() +
#   theme(legend.position=c(1,0), legend.direction='vertical',
#         legend.box='vertical',
#         legend.justification=c(1,0),
#         legend.spacing=unit(10, units="points")) +
#   ggtitle(label='Total Cultivated and Irrigated Area in South Carolina per Crop',
#           subtitle='A comparison of several datasets, with crops ordered by cultivated area')
#
#
# ## Manually delineate surveyed counties to create a reference dataset.
# ## Plot the crops over time for each county.
#
# fsa2 <- group_by(fsa, Year, County, Crop) %>%
#   summarise(Acres=sum(Acres, na.rm=TRUE),
#             type='All') %>%
#   ungroup() %>%
#   bind_rows(
#     rename(fsa, type=Irrigation) %>%
#       filter(type=='Irrigated'))
#
#
# fsa_coa_cropscape <- bind_rows(
#   `Farm Service Agency`=fsa2,
#   `Census of Agriculture`=coa,
#   `Cropland Data Layer`=select(cropscape_cnty_crop, -Source),
#   .id='Source')
#
#
# for(county in unique(fsa_coa_cropscape$County)) {
#   filter(fsa_coa_cropscape, County==county) %>%
#     ggplot(aes(x=Year, y=Acres)) +
#     geom_line(aes(group=interaction(type, Source),
#                   colour=type)) +
#     geom_point(aes(colour=Crop, shape=Source)) %>%
#     print()
# }
#
#
#
# filter(fsa_coa_cropscape, County=='ABBEVILLE') %>%
#   ggplot(aes(x=Year, y=Acres)) +
#   # geom_line(aes(group=interaction(type, Source, Crop),
#   #               colour=type)) +
#   geom_point(aes(colour=Crop, shape=Source))
