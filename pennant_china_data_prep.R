# this is a comment

library(tidyverse)
library(sf)
library(rnaturalearth)
library(leaflet)

source('ne_scripts.R')

pennant_annotations = read_csv('/Users/yannryan/Downloads/5x7kygxisii40w (11).csv')

text_vec = c()

for(row in 1:nrow(pennant_annotations)){
  
  text = read_file(paste0("/Users/yannryan/Downloads/pennant_china_scans_v2/", pennant_annotations$FILE[row]))
  
  
  offset = as.numeric(str_extract(pennant_annotations$ANCHOR[row], "[0-9]{1,}"))
  
  text_vec[row] = substr(text, (offset-100), (offset+100))
  
}

pennant_annotations = pennant_annotations %>%separate(COMMENTS, into = c('X1', 'info'), sep = '\\| ')

pennant_full_sf = pennant_annotations %>% 
  filter(!is.na(LAT)) %>% 
  #mutate(context = ifelse(!is.na(TYPE), str_replace(context, QUOTE_TRANSCRIPTION, paste0("<b>", QUOTE_TRANSCRIPTION, "</b>")), context))  %>%
  #mutate(context = paste0("<i>...", context, "...</i>")) %>% 
  sf::st_as_sf(coords = c('LNG', 'LAT')) %>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info)) 

pennant_full_sf = pennant_full_sf %>% st_set_crs(4326)

china_prov = ne_download_e(scale = 10, type = 'admin_1_states_provinces', returnclass = 'sf')

china_prov = china_prov %>% filter(admin == 'China')

all_provinces = pennant_annotations %>% filter(!is.na(TYPE)) %>% 
  #mutate(context = ifelse(!is.na(TYPE), str_replace(context, QUOTE_TRANSCRIPTION, paste0("<b>", QUOTE_TRANSCRIPTION, "</b>")), context))  %>%
  #mutate(context = paste0("<i>...", context, "...</i>")) %>% 
  distinct(VOCAB_LABEL, .keep_all = TRUE) %>% 
  inner_join(china_prov, by = c('VOCAB_LABEL'  = 'name')) %>% 
  sf::st_as_sf() %>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info)) 

all_countries = ne_countries(scale = 10, returnclass = 'sf')

all_countries_sf = pennant_annotations %>% filter(!is.na(TYPE)) %>% 
 # mutate(context = ifelse(!is.na(TYPE), str_replace(context, QUOTE_TRANSCRIPTION, paste0("<b>", QUOTE_TRANSCRIPTION, "</b>")), context))  %>%
  #mutate(context = paste0("<i>...", context, "...</i>"))%>% distinct(VOCAB_LABEL, .keep_all = TRUE) %>% 
  inner_join(all_countries, by = c('VOCAB_LABEL'  = 'name')) %>% sf::st_as_sf() %>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info)) 

all_countries_sf = all_countries_sf %>% st_set_crs(4326)

pennant_full_sf  = pennant_full_sf %>% filter(!UUID %in% c(all_countries_sf$UUID, all_provinces$UUID))

l = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>% setView(50, 30, zoom = 2)%>% 
  addPolygons(data = all_countries_sf, popup = ~info, fillColor = 'lightblue', color = 'black', weight = 2, group = 'Gwledydd')%>% 
  addPolygons(data = all_provinces,  popup = ~info, fillColor = 'forestgreen', color = 'black', weight = 2, group = 'Taleithiau')  %>% 
  addCircleMarkers(data = pennant_full_sf, label = ~VOCAB_LABEL,  popup =~info ,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#d9d9d9', group = 'Lleoliadau (dinasoedd)') %>%
  # Layers control
  addLayersControl(
    overlayGroups = c('Gwledydd', 'Taleithiau', 'Lleoliadau (dinasoedd)'),
    options = layersControlOptions(collapsed = FALSE)
  )
l
save(l, file = 'l')



xavier = pennant_annotations %>% filter(str_detect(TAGS, "journey_francisdexavier01"))

xavier = xavier %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

xavier_sf = xavier %>% 
  filter(!is.na(LAT)) %>% 
  #mutate(context = ifelse(!is.na(TYPE), str_replace(context, QUOTE_TRANSCRIPTION, paste0("<b>", QUOTE_TRANSCRIPTION, "</b>")), context))  %>%
  #mutate(context = paste0("<i>...", context, "...</i>")) %>% 
  #mutate(context = paste0("<b>Place: </b>", VOCAB_LABEL, "<br><br><b>Name in text: </b>", QUOTE_TRANSCRIPTION, "<br><b>Context: </b>", context)) %>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info)) %>%
  sf::st_as_sf(coords = c('LNG', 'LAT'))

xavier_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% 
  addCircleMarkers(data = xavier_sf,  popup =~info ,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#d9d9d9')

save(xavier_leaflet, file = 'xavier_leaflet')

polo = pennant_annotations %>% filter(str_detect(TAGS, "journey_marcopolo"))

polo = polo %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

polo_sf = polo %>% 
  filter(!is.na(LAT)) %>% 
 # mutate(context = ifelse(!is.na(TYPE), str_replace(context, QUOTE_TRANSCRIPTION, paste0("<b>", QUOTE_TRANSCRIPTION, "</b>")), context))  %>%
  #mutate(context = paste0("<i>...", context, "...</i>")) %>% 
  #mutate(context = paste0("<b>Place: </b>", VOCAB_LABEL, "<br><br><b>Name in text: </b>", QUOTE_TRANSCRIPTION, "<br><b>Context: </b>", context)) %>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info)) %>%
  sf::st_as_sf(coords = c('LNG', 'LAT'))

polo_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% 
  addCircleMarkers(data = polo_sf,  popup =~info ,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#d9d9d9')

save(polo_leaflet, file = 'polo_leaflet')


ricci = pennant_annotations %>% filter(str_detect(TAGS, "journey_matteoricci01"))

ricci = ricci %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

ricci_sf = ricci %>% 
  filter(!is.na(LAT)) %>% 
 # mutate(context = ifelse(!is.na(TYPE), str_replace(context, QUOTE_TRANSCRIPTION, paste0("<b>", QUOTE_TRANSCRIPTION, "</b>")), context))  %>%
  #mutate(context = paste0("<i>...", context, "...</i>")) %>% 
  #mutate(context = paste0("<b>Place: </b>", VOCAB_LABEL, "<br><br><b>Name in text: </b>", QUOTE_TRANSCRIPTION, "<br><b>Context: </b>", context)) %>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info)) %>%
  sf::st_as_sf(coords = c('LNG', 'LAT'))

ricci_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% 
  addCircleMarkers(data = ricci_sf,  popup =~info ,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#d9d9d9')

save(ricci_leaflet, file = 'ricci_leaflet')


andrade = pennant_annotations %>% filter(str_detect(TAGS, "journey_andrade01"))

andrade = andrade %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

andrade_sf = andrade %>% 
  filter(!is.na(LAT)) %>% 
 # mutate(context = ifelse(!is.na(TYPE), str_replace(context, QUOTE_TRANSCRIPTION, paste0("<b>", QUOTE_TRANSCRIPTION, "</b>")), context))  %>%
  #mutate(context = paste0("<i>...", context, "...</i>")) %>% 
  #mutate(context = paste0("<b>Place: </b>", VOCAB_LABEL, "<br><br><b>Name in text: </b>", QUOTE_TRANSCRIPTION, "<br><b>Context: </b>", context))  %>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info)) %>%
  sf::st_as_sf(coords = c('LNG', 'LAT'))

andrade_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% 
  addCircleMarkers(data = andrade_sf,  popup =~info ,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#d9d9d9')

save(andrade_leaflet, file = 'andrade_leaflet')




nieuhof = pennant_annotations %>% filter(str_detect(TAGS, "journey_nieuhof"))

nieuhof = nieuhof %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

nieuhof_sf = nieuhof %>% 
  filter(!is.na(LAT)) %>% 
  #mutate(context = ifelse(!is.na(TYPE), str_replace(context, QUOTE_TRANSCRIPTION, paste0("<b>", QUOTE_TRANSCRIPTION, "</b>")), context))  %>%
  #mutate(context = paste0("<i>...", context, "...</i>")) %>% 
 # mutate(context = paste0("<b>Place: </b>", VOCAB_LABEL, "<br><br><b>Name in text: </b>", QUOTE_TRANSCRIPTION, "<br><b>Context: </b>", context))  %>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info))%>%
  sf::st_as_sf(coords = c('LNG', 'LAT'))

nieuhof_leaflet = leaflet()  %>% setView(120, 30, zoom = 5) %>%
  addTiles( 
    group='Historic Map', urlTemplate = 'https://maps.georeferencer.com/georeferences/498f6114-6109-50ff-a476-b6ce9a340c06/2019-10-08T11:46:44.143428Z/map/{z}/{x}/{y}.png?key=mp8Uneu8RsMxOKqs5e9C')   %>% 
  addCircleMarkers(data = nieuhof_sf,  popup =~info ,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = 'blue')

save(nieuhof_leaflet, file = 'nieuhof_leaflet')

pennant_coastal = pennant_annotations %>% filter(str_detect(TAGS, "journey_pennant coastal_01"))

pennant_coastal = pennant_coastal %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

pennant_coastal_sf = pennant_coastal %>% 
  filter(!is.na(LAT)) %>% 
  #mutate(context = ifelse(!is.na(TYPE), str_replace(context, QUOTE_TRANSCRIPTION, paste0("<b>", QUOTE_TRANSCRIPTION, "</b>")), context))  %>%
  #mutate(context = paste0("<i>...", context, "...</i>")) %>% 
  #mutate(context = paste0("<b>Place: </b>", VOCAB_LABEL, "<br><br><b>Name in text: </b>", QUOTE_TRANSCRIPTION, "<br><b>Context: </b>", context))  %>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info))%>%
  sf::st_as_sf(coords = c('LNG', 'LAT'))


china_prov = rnaturalearth::ne_states(returnclass = 'sf')

china_prov = china_prov %>% filter(admin == 'China')

china_prov = china_prov %>% st_transform(4326)

pennant_provinces = pennant_coastal %>% 
  filter(str_detect(TAGS, "province")) %>% 
  distinct(VOCAB_LABEL, .keep_all = TRUE) %>% 
  left_join(china_prov, by = c('VOCAB_LABEL'  = 'name')) %>% 
  filter(!is.na(LAT)) %>% 
  #mutate(context = ifelse(!is.na(TYPE), str_replace(context, QUOTE_TRANSCRIPTION, paste0("<b>", QUOTE_TRANSCRIPTION, "</b>")), context))  %>%
  #mutate(context = paste0("<i>...", context, "...</i>")) %>% 
  #mutate(context = paste0("<b>Place: </b>", VOCAB_LABEL, "<br><br><b>Name in text: </b>", QUOTE_TRANSCRIPTION, "<br><b>Context: </b>", context))  %>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info))%>%
  sf::st_as_sf()

china_rivers2 = st_read('/Users/yannryan/Downloads/dataverse_files (1)/v6_1820_coded_rvr_lin_utf/v6_1820_coded_rvr_lin_utf.shp')

china_rivers2 = china_rivers2 %>% st_transform(4326)

china_rivers = ne_download_e(scale = 10, type = 'rivers_lake_centerlines', returnclass = 'sf', category = 'physical')

china_lakes = ne_download_e(scale = 10, type = 'lakes', returnclass = 'sf', category = 'physical')

china_lakes2 = st_read('/Users/yannryan/Downloads/dataverse_files (1)/v6_1820_lks_pgn_utf/v6_1820_lks_pgn_utf.shp')

china_lakes2 = china_lakes2 %>% st_transform(4326)

pennant_lakes1 = china_lakes2 %>% filter(LKS_ID %in% c(148))

pennant_rivers1 = china_rivers %>% filter(ne_id %in% c('1159128895', '1159123967')) %>% select(name)

pennant_rivers2 = china_rivers2 %>% filter(NAME_PY %in% c('Qiantang Jiang')) %>% select(name = NAME_PY)

all_pennant_rivers = rbind(pennant_rivers1, pennant_rivers2)

pennant_coastal_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% 
  addPolygons(data = pennant_provinces, popup = ~info, fillColor = 'forestgreen', color = 'black', weight = 2) %>% 
  addCircleMarkers(data = pennant_coastal_sf,  popup =~info ,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#d9d9d9') %>% 
  addPolylines(data = all_pennant_rivers) %>% addPolygons(data = pennant_lakes1)

save(pennant_coastal_leaflet, file = 'pennant_coastal_leaflet')