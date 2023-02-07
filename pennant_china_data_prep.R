# this is a comment

library(tidyverse)
library(sf)
library(rnaturalearth)
library(leaflet)

source('ne_scripts.R')

#bed3ec

#2f5d8c

download.file('https://recogito.pelagios.org/document/5x7kygxisii40w/downloads/annotations/csv', 'infile.csv')

pennant_annotations = read_csv('infile.csv') 

pennant_annotations[pennant_annotations$UUID== '7ad3e5d1-7b18-4f91-8649-4b69e70861fd', 7 ] = 'Xiamen (porthladd A-mwy)'
pennant_annotations[pennant_annotations$UUID== '61dda076-7ed5-4998-9ede-cfdc211ffd4d', 7 ] = 'Xiamen (A-mwy)'
pennant_annotations[pennant_annotations$UUID== 'b7e12d37-f8a8-4c63-9ed3-029d998e73f2', 7 ] = 'Xiamen (Ynys A-mwy)'
pennant_annotations[pennant_annotations$UUID== '61dda076-7ed5-4998-9ede-cfdc211ffd4d', 7 ] = 'Xiamen (Ynys A-mwy)'

pennant_annotations[pennant_annotations$UUID== '39fffd71-f147-4725-a637-4f32633158e9', 7 ] = 'Ynys Shangchuan'

pennant_annotations[pennant_annotations$QUOTE_TRANSCRIPTION== 'Xaochew or Tchau-tchoo-soo', 3 ] = 'Xaochew or Tchau-tchoo-foo'

pennant_annotations[pennant_annotations$QUOTE_TRANSCRIPTION== 'Asses ears', 7 ] = 'Ynysoedd Jiapeng Liedao'

pennant_annotations[pennant_annotations$QUOTE_TRANSCRIPTION== 'Ladrone isles', 7 ] = 'Ynysoedd Ladrone'

pennant_annotations[pennant_annotations$QUOTE_TRANSCRIPTION== 'lake Kao-yeou-boo', 3 ] = 'lake Kao-yeou-hoo'


pennant_annotations =  pennant_annotations %>% 
  filter(UUID != '92e6d710-806a-4127-8fd9-feb27e033b3d')


# 
# text_vec = c()
# 
# for(row in 1:nrow(pennant_annotations)){
#   
#   text = read_file(paste0("/Users/yannryan/Downloads/pennant_china_scans_v2/", pennant_annotations$FILE[row]))
#   
#   
#   offset = as.numeric(str_extract(pennant_annotations$ANCHOR[row], "[0-9]{1,}"))
#   
#   text_vec[row] = substr(text, (offset-100), (offset+100))
#   
# }


pennant_translations = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1RYrHV2ytY4iHZDQLofFr9jaEF40tl8gIQBF8XdQc5G0/edit?usp=sharing')

pennant_annotations = pennant_annotations %>% 
  separate(COMMENTS, into = c('X1', 'info'), sep = '\\|') %>% 
  mutate(pages = as.numeric(str_extract(FILE, "[0-9]{3}"))) %>% 
  mutate(pages = pages + 86) 


xavier = pennant_annotations %>% filter(str_detect(TAGS, "journey_francisdexavier01"))

xavier = xavier %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% 
  mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

xavier_sf = xavier %>% 
  filter(!is.na(LAT)) %>% group_by(VOCAB_LABEL, URI) %>% 
  summarise(VOCAB_LABEL = max(VOCAB_LABEL, na.rm = T), 
            LAT = max(LAT, na.rm = T), 
            LNG = max(LNG, na.rm = T), 
            names = paste5(unique(QUOTE_TRANSCRIPTION),collapse = '; ',na.rm=TRUE), 
            page_nos = paste5(unique(pages), collapse = ', ',na.rm=TRUE),
            infos = paste5(unique(info), collapse = ';',na.rm=TRUE)) %>%  
  sf::st_as_sf(coords = c('LNG', 'LAT')) %>% 
  left_join(pennant_translations) %>% 
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL)) %>% 
  mutate(info = ifelse(!is.na(infos), 
                        paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Gwybodaeth: </b>", infos, "<br><b>Rhif(au) tudalen: </b>", page_nos),
                        paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Rhif(au) tudalen: </b>", page_nos))) 

xavier_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% 
  addCircleMarkers(data = xavier_sf,  popup =~info, label = ~VOCAB_LABEL,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#1f78b4')

save(xavier_leaflet, file = 'xavier_leaflet')

polo = pennant_annotations %>% filter(str_detect(TAGS, "journey_marcopolo"))

polo = polo %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

polo_sf = polo%>% 
  filter(!is.na(LAT)) %>% group_by(VOCAB_LABEL, URI) %>% 
  summarise(VOCAB_LABEL = max(VOCAB_LABEL, na.rm = T), 
            LAT = max(LAT, na.rm = T), 
            LNG = max(LNG, na.rm = T), 
            names = paste5(unique(QUOTE_TRANSCRIPTION),collapse = '; ',na.rm=TRUE), 
            page_nos = paste5(unique(pages), collapse = ', ',na.rm=TRUE),
            infos = paste5(unique(info), collapse = ';',na.rm=TRUE)) %>%  
  sf::st_as_sf(coords = c('LNG', 'LAT')) %>% 
  left_join(pennant_translations) %>%  
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL)) %>% 
  mutate(info = ifelse(!is.na(infos), 
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Gwybodaeth: </b>", infos, "<br><b>Rhif(au) tudalen: </b>", page_nos),
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Rhif(au) tudalen: </b>", page_nos))) 

polo_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% 
  addCircleMarkers(data = polo_sf,  popup =~info , label = ~VOCAB_LABEL,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#1f78b4')

save(polo_leaflet, file = 'polo_leaflet')


ricci = pennant_annotations %>% filter(str_detect(TAGS, "journey_matteoricci01"))

ricci = ricci %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

ricci_sf = ricci %>% 
  filter(!is.na(LAT)) %>% group_by(VOCAB_LABEL, URI) %>% 
  summarise(VOCAB_LABEL = max(VOCAB_LABEL, na.rm = T), 
            LAT = max(LAT, na.rm = T), 
            LNG = max(LNG, na.rm = T), 
            names = paste5(unique(QUOTE_TRANSCRIPTION),collapse = '; ',na.rm=TRUE), 
            page_nos = paste5(unique(pages), collapse = ', ',na.rm=TRUE),
            infos = paste5(unique(info), collapse = ';',na.rm=TRUE)) %>%  
  sf::st_as_sf(coords = c('LNG', 'LAT'))%>% 
  left_join(pennant_translations) %>% 
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL)) %>% 
  mutate(info = ifelse(!is.na(infos), 
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Gwybodaeth: </b>", infos, "<br><b>Rhif(au) tudalen: </b>", page_nos),
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Rhif(au) tudalen: </b>", page_nos))) 

ricci_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% 
  addCircleMarkers(data = ricci_sf,  popup =~info , label = ~VOCAB_LABEL,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#1f78b4')

save(ricci_leaflet, file = 'ricci_leaflet')


andrade = pennant_annotations %>% filter(str_detect(TAGS, "journey_andrade01"))

andrade = andrade %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

andrade_sf = andrade %>% 
  filter(!is.na(LAT)) %>% group_by(VOCAB_LABEL, URI) %>% 
  summarise(VOCAB_LABEL = max(VOCAB_LABEL, na.rm = T), 
            LAT = max(LAT, na.rm = T), 
            LNG = max(LNG, na.rm = T), 
            names = paste5(unique(QUOTE_TRANSCRIPTION),collapse = '; ',na.rm=TRUE), 
            page_nos = paste5(unique(pages), collapse = ', ',na.rm=TRUE),
            infos = paste5(unique(info), collapse = ';',na.rm=TRUE)) %>%  
  sf::st_as_sf(coords = c('LNG', 'LAT')) %>% 
  left_join(pennant_translations) %>% 
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL))%>% 
  mutate(info = ifelse(!is.na(infos), 
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Gwybodaeth: </b>", infos, "<br><b>Rhif(au) tudalen: </b>", page_nos),
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Rhif(au) tudalen: </b>", page_nos))) 

andrade_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% 
  addCircleMarkers(data = andrade_sf,  popup =~info , label = ~VOCAB_LABEL,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#1f78b4')

save(andrade_leaflet, file = 'andrade_leaflet')

n = sf::st_read('Koylan_Nieuhof_Pennant.geojson')

n = n %>% filter(name != '') %>% 
  filter(id != 'ce232e50-3f5d-4c29-86f5-29e82cc98966') %>% 
  filter(name != 'a3ac53fb-506a-4183-8c87-815ad8ab02a1') %>% 
  left_join(pennant_annotations, by = c('name' = 'UUID')) %>% 
  left_join(pennant_translations) %>% 
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL)) %>% mutate(VOCAB_LABEL = replace_na(VOCAB_LABEL, "Ddim ar gael"))%>% 
  mutate(info = paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", QUOTE_TRANSCRIPTION, "<br><b>Gwybodaeth: </b>", info, "<br><b>Rhif(au) tudalen: </b>", pages))

n_uncertain = n %>% filter(str_detect(X1, "_ansicr"))


n_certain = n %>% filter(!str_detect(X1, "_ansicr")) 

nieuhof_leaflet = leaflet() %>%
  addTiles( 
    group='Historic Map', urlTemplate = 'https://maps.georeferencer.com/georeferences/498f6114-6109-50ff-a476-b6ce9a340c06/2019-10-08T11:46:44.143428Z/map/{z}/{x}/{y}.png?key=EYmpUMUlpCkwonIVeNOn')  %>% 
  addCircleMarkers(data = n_uncertain, label = ~QUOTE_TRANSCRIPTION, popup = ~info,
                   radius = 7, weight = 1, opacity = 1, fillOpacity = 1, color = 'black', fillColor = '#a6cee3') %>% 
  addCircleMarkers(data = n_certain, label = ~QUOTE_TRANSCRIPTION, popup = ~info,
                   radius = 7, weight = 1, opacity = 1, fillOpacity = 1, color = 'black', fillColor = '#1f78b4')


save(nieuhof_leaflet, file = 'nieuhof_leaflet')


pennant_coastal = pennant_annotations %>% filter(str_detect(TAGS, "journey_pennant coastal_01"))

pennant_coastal = pennant_coastal %>% 
  mutate(extra_coords = str_extract(X1, "([-+]?\\d{1,2}[.]\\d+),\\s*([-+]?\\d{1,3}[.]\\d+)")) %>% 
  separate(extra_coords, into = c('LAT2', 'LNG2'), sep = ',') %>% mutate(LAT2 = as.numeric(LAT2)) %>% 
  mutate(LNG2 = as.numeric(LNG2)) %>% 
  mutate(LAT = coalesce(LAT, LAT2))%>% 
  mutate(LNG = coalesce(LNG, LNG2))

pennant_coastal_sf = pennant_coastal %>% 
  mutate(VOCAB_LABEL = coalesce(VOCAB_LABEL, QUOTE_TRANSCRIPTION)) %>% 
  filter(!is.na(LAT)) %>% group_by(VOCAB_LABEL, URI) %>% 
  summarise(VOCAB_LABEL = max(VOCAB_LABEL, na.rm = T), 
            LAT = max(LAT, na.rm = T), 
            LNG = max(LNG, na.rm = T), 
            names = paste5(unique(QUOTE_TRANSCRIPTION),collapse = '; ',na.rm=TRUE), 
            page_nos = paste5(unique(pages), collapse = ', ',na.rm=TRUE),
            infos = paste5(unique(info), collapse = ';',na.rm=TRUE)) %>%  
  sf::st_as_sf(coords = c('LNG', 'LAT')) %>% 
  left_join(pennant_translations) %>% 
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL)) %>% 
  mutate(info = ifelse(!is.na(infos), 
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Gwybodaeth: </b>", infos, "<br><b>Rhif(au) tudalen: </b>", page_nos),
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Rhif(au) tudalen: </b>", page_nos))) 


china_prov = rnaturalearth::ne_states(returnclass = 'sf')

china_prov = china_prov %>% filter(admin == 'China')

china_prov = china_prov %>% st_transform(4326)

pennant_provinces = pennant_coastal %>% 
  mutate(VOCAB_LABEL = coalesce(VOCAB_LABEL, QUOTE_TRANSCRIPTION)) %>% 
  #filter(str_detect(TAGS, "province")) %>% 
  filter(!is.na(LAT)) %>% group_by(VOCAB_LABEL, URI) %>% 
  summarise(VOCAB_LABEL = max(VOCAB_LABEL, na.rm = T), 
            LAT = max(LAT, na.rm = T), 
            LNG = max(LNG, na.rm = T), 
            names = paste5(unique(QUOTE_TRANSCRIPTION),collapse = '; ',na.rm=TRUE), 
            page_nos = paste5(unique(pages), collapse = ', ',na.rm=TRUE),
            infos = paste5(unique(info), collapse = ';',na.rm=TRUE))  %>% 
  mutate(gn_id = as.numeric(str_extract(URI, '[0-9]{1,}'))) %>% filter(!is.na(gn_id)) %>% 
  left_join(china_prov, by = 'gn_id') %>% 
  left_join(pennant_translations) %>% 
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL)) %>% 
  mutate(info = ifelse(!is.na(infos), 
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Gwybodaeth: </b>", infos, "<br><b>Rhif(au) tudalen: </b>", page_nos),
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Rhif(au) tudalen: </b>", page_nos)))  %>% 
  sf::st_as_sf() %>% filter(!is.na(featurecla))

china_rivers2 = st_read('/Users/yannryan/Downloads/dataverse_files (1)/v6_1820_coded_rvr_lin_utf/v6_1820_coded_rvr_lin_utf.shp')
# 
china_rivers2 = china_rivers2 %>% st_transform(4326)
# 
china_rivers = ne_download_e(scale = 10, type = 'rivers_lake_centerlines', returnclass = 'sf', category = 'physical')
# 
# china_lakes = ne_download_e(scale = 10, type = 'lakes', returnclass = 'sf', category = 'physical')
# 
china_lakes2 = st_read('/Users/yannryan/Downloads/dataverse_files (1)/v6_1820_lks_pgn_utf/v6_1820_lks_pgn_utf.shp')
# 
china_lakes2 = china_lakes2 %>% st_transform(4326)
# 
pennant_lakes1 = china_lakes2 %>% filter(LKS_ID %in% c(148))

pennant_rivers1 = china_rivers %>% filter(ne_id %in% c('1159128895', '1159123967')) %>% select(name)

pennant_rivers2 = china_rivers2 %>% filter(NAME_PY %in% c('Qiantang Jiang')) %>% select(name = NAME_PY)

all_pennant_rivers = rbind(pennant_rivers1, pennant_rivers2)

pennant_coastal_sf = pennant_coastal_sf %>% filter(! URI %in% pennant_provinces$URI)

pennant_coastal_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% 
  addPolygons(data = pennant_provinces, popup = ~info, fillColor = '#faff33', color = 'black', weight = 2) %>% 
  addCircleMarkers(data = pennant_coastal_sf,  popup =~info , label = ~VOCAB_LABEL,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = .9, color = 'black', fillColor = '#1f78b4') %>% 
  addPolylines(data = all_pennant_rivers) %>% addPolygons(data = pennant_lakes1)

save(pennant_coastal_leaflet, file = 'pennant_coastal_leaflet')



china_prov = ne_download_e(scale = 10, type = 'admin_1_states_provinces', returnclass = 'sf')

china_prov = china_prov %>% filter(admin == 'China')

all_provinces = pennant_annotations %>% 
  filter(!is.na(LAT)) %>% group_by(VOCAB_LABEL, URI) %>% 
  summarise(VOCAB_LABEL = max(VOCAB_LABEL, na.rm = T), 
            LAT = max(LAT, na.rm = T), 
            LNG = max(LNG, na.rm = T), 
            names = paste5(unique(QUOTE_TRANSCRIPTION),collapse = '; ',na.rm=TRUE), 
            page_nos = paste5(unique(pages), collapse = ', ',na.rm=TRUE),
            infos = paste5(unique(info), collapse = ';',na.rm=TRUE))%>% 
  mutate(gn_id = as.numeric(str_extract(URI, '[0-9]{1,}'))) %>% filter(!is.na(gn_id)) %>% 
 inner_join(china_prov, by = 'gn_id')  %>% 
  left_join(pennant_translations) %>% 
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL)) %>% 
  mutate(info = ifelse(!is.na(infos), 
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Gwybodaeth: </b>", infos, "<br><b>Rhif(au) tudalen: </b>", page_nos),
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Rhif(au) tudalen: </b>", page_nos)))  %>%  
  sf::st_as_sf() 

all_countries = ne_countries(scale = 10, returnclass = 'sf')

all_countries_sf = pennant_annotations %>% 
  filter(UUID != '30ceb7c8-be80-4216-b7f3-1e8fc6d5b0e6') %>% 
  filter(!is.na(LAT)) %>% group_by(VOCAB_LABEL, URI) %>% 
  summarise(VOCAB_LABEL = max(VOCAB_LABEL, na.rm = T), 
            LAT = max(LAT, na.rm = T), 
            LNG = max(LNG, na.rm = T), 
            names = paste5(unique(QUOTE_TRANSCRIPTION),collapse = '; ',na.rm=TRUE), 
            page_nos = paste5(unique(pages), collapse = ', ',na.rm=TRUE),
            infos = paste5(unique(info), collapse = ';',na.rm=TRUE)) %>% 
  inner_join(all_countries, by = c('VOCAB_LABEL'  = 'name')) %>% 
  left_join(pennant_translations) %>% 
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL)) %>% 
  mutate(info = ifelse(!is.na(infos), 
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Gwybodaeth: </b>", infos, "<br><b>Rhif(au) tudalen: </b>", page_nos),
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Rhif(au) tudalen: </b>", page_nos)))  %>%  
  sf::st_as_sf()

all_countries_sf = all_countries_sf %>% st_set_crs(4326)

journeys = c('journey_francisdexavier01',
             'journey_marcopolo', 
             'journey_matteoricci01', 
             'journey_andrade01',
             'journey_pennant coastal_01')



pennant_full_sf_not_j = pennant_annotations %>% 
  #filter(!VOCAB_LABEL %in% c(andrade_sf$VOCAB_LABEL, pennant_coastal_sf$VOCAB_LABEL, polo_sf$VOCAB_LABEL, ricci_sf$VOCAB_LABEL, xavier_sf$VOCAB_LABEL)) %>% 
  filter(!str_detect(TAGS, paste0(journeys, collapse = "|")) | is.na(TAGS) ) %>% 
  filter(!is.na(LAT)) %>% 
  group_by(VOCAB_LABEL, URI) %>% 
  summarise(VOCAB_LABEL = max(VOCAB_LABEL, na.rm = T), 
            LAT = max(LAT, na.rm = T), 
            LNG = max(LNG, na.rm = T), 
            names = paste5(unique(QUOTE_TRANSCRIPTION),collapse = '; ',na.rm=TRUE), 
            page_nos = paste5(unique(pages), collapse = ', ',na.rm=TRUE),
            infos = paste5(unique(info), collapse = ';',na.rm=TRUE)) %>%  
  sf::st_as_sf(coords = c('LNG', 'LAT')) %>% 
  left_join(pennant_translations) %>% 
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL)) %>% 
  mutate(info = ifelse(!is.na(infos), 
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Gwybodaeth: </b>", infos, "<br><b>Rhif(au) tudalen: </b>", page_nos),
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Rhif(au) tudalen: </b>", page_nos))) 

pennant_full_sf_j = pennant_annotations %>% 
 # filter(VOCAB_LABEL %in% c(andrade_sf$VOCAB_LABEL, pennant_coastal_sf$VOCAB_LABEL, polo_sf$VOCAB_LABEL, ricci_sf$VOCAB_LABEL, xavier_sf$VOCAB_LABEL)) %>% 
  filter(str_detect(TAGS, paste0(journeys, collapse = "|"))) %>% 
  filter(!is.na(LAT)) %>% 
  group_by(VOCAB_LABEL, URI) %>% 
  summarise(VOCAB_LABEL = max(VOCAB_LABEL, na.rm = T), 
            LAT = max(LAT, na.rm = T), 
            LNG = max(LNG, na.rm = T), 
            names = paste5(unique(QUOTE_TRANSCRIPTION),collapse = '; ',na.rm=TRUE), 
            page_nos = paste5(unique(pages), collapse = ', ',na.rm=TRUE),
            infos = paste5(unique(info), collapse = ';',na.rm=TRUE)) %>%  
  sf::st_as_sf(coords = c('LNG', 'LAT')) %>% 
  left_join(pennant_translations) %>% 
  mutate(VOCAB_LABEL = coalesce(TRANSLATION, VOCAB_LABEL)) %>% 
  mutate(info = ifelse(!is.na(infos), 
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Gwybodaeth: </b>", infos, "<br><b>Rhif(au) tudalen: </b>", page_nos),
                       paste0("<b>Lleoliad: </b>", VOCAB_LABEL, "<br><b>Enw yn y testun: </b>", names, "<br><b>Rhif(au) tudalen: </b>", page_nos))) 

pennant_full_sf_j = pennant_full_sf_j %>% 
  filter(!URI %in% c(all_provinces$URI, all_countries_sf$URI))

pennant_full_sf_not_j = pennant_full_sf_not_j %>% 
  filter(!URI %in% c(all_provinces$URI, all_countries_sf$URI))

pennant_full_sf_j = pennant_full_sf_j %>% st_set_crs(4326)
pennant_full_sf_not_j = pennant_full_sf_not_j %>% st_set_crs(4326)
all_countries_sf = all_countries_sf %>% st_set_crs(4326)
all_provinces = all_provinces%>% st_set_crs(4326)

#pennant_full_sf  = pennant_full_sf %>% filter(!UUID %in% c(all_countries_sf$UUID, all_provinces$UUID))

pennant_full_leaflet = leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = 'Map sylfaenol') %>% setView(50, 30, zoom = 2) %>% 
  addPolygons(data = all_countries_sf, popup = ~info, fillColor = '#ffb433', color = 'black', weight = 2, group = 'Gwledydd') %>% 
  addPolygons(data = all_provinces,  popup = ~info, fillColor = '#faff33', color = 'black', weight = 2, group = 'Taleithiau')  %>% 
  addCircleMarkers(data = pennant_full_sf_not_j, label = ~VOCAB_LABEL,  popup =~info ,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = 1, color = 'black', fillColor = '#1f78b4', group = 'Lleoliadau (dinasoedd)') %>% 
  addCircleMarkers(data = pennant_full_sf_j, label = ~VOCAB_LABEL,  popup =~info ,
                   radius = 5, weight = 1, opacity = 1, fillOpacity = 1, color = 'black', fillColor = '#a6cee3', group = 'Lleoliadau (dinasoedd)') %>%
  # Layers control
  addLayersControl(
    overlayGroups = c('Gwledydd', 'Taleithiau', 'Lleoliadau (dinasoedd)', 'Map sylfaenol'),
    options = layersControlOptions(collapsed = FALSE)
  )

save(pennant_full_leaflet, file = 'pennant_full_leaflet')



