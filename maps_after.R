###  Read results from CPT (D.Agudelo script) (results for FEDEGAN --- meeting). 
# https://github.com/jrodriguez88/Madr_semillas
# Author: Esquivel A.
# 2019

### Load libraries
library(cowsay)

library(curl)
library(tidyverse)
library(stringi)
library(lubridate)
library(naniar)
library(data.table)
library(raster)
library(sf)
library(plotly)
library(htmlwidgets)
library(tictoc)


# Comentarios...
cowsay::say(what = 'cattle raising', by = 'cow')
# Ganadería
# 1.	Planeta Rica, Lorica (Córdoba)
# 2.	Santa Rosa de Osos, Caucasia, San Pedro-Matias (Antioquia)
# 3.	Zona Altiplano Cundiboyacense (Zipaquirá, Duitama, Chiquinquira)
# 4.	Departamento de César

path <- 'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/CPT_run/'
departamento <- c( 'Cundinamarca', 'Boyacá', 'Córdoba', 'Cesar', 'Antioquia')

# =----------------------
COL_shp <-  getData('GADM', country='COL', level=1) %>% crop(extent(-81 , -66.7 , 2 , 12.5 )) %>% st_as_sf()

DPTO_shp <- COL_shp %>% filter(NAME_1 %in% departamento) %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

Mun_shp <-getData('GADM', country='COL', level=2) %>% crop(extent(-81 , -66.7 , 2 , 12.5 )) %>% st_as_sf()

Mun__filter <- Mun_shp %>%
  filter(NAME_2 %in% c('Caucasia' , 'Don Matías' , 'San Pedro de los Milagros', 'Santa Rosa de Osos', 
                       'Lorica' ,  'Planeta Rica', 'Chiquinquirá' , 'Duitama', 'Zipaquirá') | 
           NAME_1 == 'Cesar') %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))
# =----------------------


to_read <- tibble(path_t = list.files(path = path, pattern = 'metrics.csv', recursive = TRUE, full.names = TRUE)) %>% 
  mutate(sect = purrr::map(.x = path_t, .f = function(x){
    guardado <- str_remove(x, path) %>% str_remove('/metrics.csv') %>% 
      str_remove('/output') %>% str_split(pattern = '/') %>% 
      unlist()
    sect <- tibble(zone = guardado[2], domain = guardado[3])
  return(sect)})) %>% 
  dplyr::select(sect, path_t) %>% 
  unnest(sect) %>% 
  separate(domain, c('domain', 'ers')) %>% 
  dplyr::select(-ers) %>% 
  mutate(data = purrr::map(.x = path_t, .f = readr::read_csv)) %>% 
  dplyr::select(-path_t)





GI <- to_read %>% 
  unnest() %>% 
  dplyr::select(zone, file,  domain, goodness) %>% 
  unique()

# Temporal...
GI %>% 
  separate(file,   into =  c('ic', 'season'), '_') %>% 
ggplot(aes(x = zone, y = goodness, fill = domain)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_grey() + 
  geom_text(aes(label=goodness), position=position_dodge(width=0.9), vjust=-0.25) + 
  facet_wrap(~season) + 
  theme_bw() + 
  labs(x = '', y = 'Goodness Index', fill = 'Domain')



max_GI <- GI  %>% 
  separate(file,   into =  c('ic', 'season'), '_') %>% 
  dplyr::select(-ic) %>% 
  nest(-zone, -season) %>% 
  mutate(data = purrr::map(.x = data, .f = function(x){arrange(x, desc(goodness)) %>% slice(1)})) %>% 
  unnest(data)
  
# =------ Remaches del pais. 
test <- to_read %>% 
  filter(domain == 'opt') %>% 
  filter(zone %in% c('All', 'AntCord')) %>% 
  unnest() 


id_AC <- test %>% 
  filter(zone == 'AntCord') %>% 
  dplyr::pull(id)


country <- test %>% 
  filter(!(zone =='All' & id %in% id_AC))


country_mod <- country %>% 
  dplyr::select(zone, id, file, latitud, longitud, below, normal, above) %>% 
  mutate(file = str_remove(file, 'Oct_')) %>% 
  separate(col = file, into = c('season', 'ers'), sep = '_') %>%
  dplyr::select(-ers) %>% 
  tidyr::pivot_longer(cols = below:above, names_to = "type", values_to = "prob")


# aun no esta terminado...
ggplot()  +
  geom_sf(data = Mun__filter, color = gray(.5)) + 
   geom_point(data = country_mod, aes(x = longitud, y = latitud, colour = prob)) +
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
  facet_wrap() + 
  theme_bw() +
  labs(title = glue::glue('Estaciones: {nrow(country)}'), x = 'Longitud',
       y = 'Latitud',
       caption = "Fuente: IDEAM") +
  theme( panel.grid.minor = element_blank(),
         strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
         strip.text = element_text(face = "bold"))







