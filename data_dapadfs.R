###  Import IDEAM_data from //dapadfs
# https://github.com/jrodriguez88/Madr_semillas
# Author: Rodriguez-Espinoza J., Esquivel A.
# 2019


### Load libraries

library(curl)
library(tidyverse)
library(stringi)
library(lubridate)
library(naniar)
library(data.table)
library(raster)
library(sf)

load(file = "catalog.rds")


#############################################################
## Filter data by Departamento, Categoria, status
dpto <- c("CESAR", "TOLIMA", "HUILA")#, "CORDOBA", "VALLE DEL CAUCA")

cat <- c("CLIMATOLOGICA PRINCIPAL", "CLIMATOLOGICA ORDINARIA", 
         "AGROMETEOROLOGICA", "SINOPTICA PRINCIPAL", "SINOPTICA SECUNDARIA", "METEOROLOGICA ESPECIAL", "PLUVIOMETRICA" )

status <- c("ACTIVA", "SUSPENDIDA", "FUERA DE SERVICIO")


## Filter IDEAM catalog
filter_ideam <- function (catalog, dpto , cat, status, min_alt = 0, max_alt = 3500) {
  
  catalog %>% 
    mutate(Departamento = toupper(stri_trans_general(Departamento,"Latin-ASCII")),
           Municipio = toupper(stri_trans_general(Municipio,"Latin-ASCII"))) %>%
    filter(Departamento %in% dpto,
           Categoria %in% cat,
           Estado %in% status,
           Altitud < max_alt,
           Altitud > min_alt)
  #               yfunc > min_year)
  
  
}

data_filter <- filter_ideam(catalog, dpto, cat, status= "ACTIVA") 


###############################################
## Scan in data_cluster4
path <- "//dapadfs/data_cluster_4/observed/weather_station/col-ideam/daily-raw"

## Set variables to join
var_selec <- c("prec", "tmax", "tmin", "sbright", "rhum")

## Set IDEAM id station 
ideam_id <- data_filter$id

scan_ciat_db <- function (path, ideam_id, var_selec) {
  
  list_folder <- list.dirs(path, recursive = F)
  pat_finder <- paste0(ideam_id, collapse = "|")
  var_names <- str_extract(list_folder, pattern = "(?<=raw/)(.*\n?)(?=-per)")
  
  
  ## Create a list with file names by var
  list_files_var <- pmap(list(path = list_folder, pattern = pat_finder), list.files)
  names(list_files_var) <- var_names
  
  ## Select station id tha contain all variable c("prec", "tmax", "tmin", "sbright", "rhum") 
  list_id <- map(list_files_var, ~str_extract(.,  "[^_]+")) %>% 
    map(., ~ enframe(., name = NULL)) %>%
    .[var_selec] %>%
    reduce(inner_join, by = "value")
  
  ## List files of select vars
  list_files_select <- pmap(list(
    path = str_subset(list_folder, 
                      paste(var_selec, collapse = "|")),
    pattern = paste0(list_id$value, collapse = "|")),
    list.files)
  
  ## Set list name as path folder
  names(list_files_select) <- str_subset(list_folder, paste(var_selec, collapse = "|"))
  
  return(list_files_select)
  
}

#list_files_select <- scan_ciat_db(path, ideam_id, var_selec)

list_files_select <- map(var_selec, ~scan_ciat_db(path, ideam_id, .x))



## Read data for each id. Convert to Tibble. This function take a list of path files by var, return a tibble
get_station_data <- function(list_files_select) {
  
  read_raw <- function(file) {
    
    data.table::fread(file) %>%
      mutate(Date = lubridate::ymd(Date),
             Value = as.numeric(Value))
    
    
  }
  
  list_files_select %>% 
#    map(., ~enframe(., name = NULL)) %>% bind_rows() %>%
    mutate(file = paste0(path, "/", value),
           data = map(file, read_raw),
           id = as.numeric(str_extract(.$value,  "[^_]+")),
           var = str_extract(file, "(?<=raw/)(.*\n?)(?=-per)")) %>%
    dplyr::select(id, var, path, data)
  
}


#wdata_tb <- get_station_data(list_files_select)

wdata_tb <- list_files_select %>% 
  map(., ~enframe(., name = "path")) %>% 
  bind_rows() %>% unnest(value) %>% 
  split(., .$path) %>%
  map(~get_station_data(.x)) 

##  ----- LOADED DATA --->

## Filter raw data, extract dates and "NA" percent...
ideam_raw <- wdata_tb %>% map( ~.x %>%
  dplyr::select(-path) %>% group_by(var) %>%
  unnest(data) %>% spread(var, Value) %>%
  nest(-id) %>%
  mutate(idate = map(data, ~ min(.x$Date)) %>% do.call("c", .),
         fdate = map(data, ~ max(.x$Date)) %>% do.call("c", .),
         years = time_length(fdate-idate, "years"),
         na_percent = map(data, ~ pct_miss(.x %>% dplyr::select(-Date))) %>% flatten_dbl()) %>%
  filter(years > 20)) %>% 
  set_names(str_extract(names(.), "(?<=raw/)(.*\n?)(?=-per)"))
#  bind_rows(.id = "var")


ws_selected <- ideam_raw %>% bind_rows(.id = "var") %>%
  dplyr::select(-data) %>% 
  left_join(catalog %>% 
              dplyr::select(id, Nombre, Clase, Departamento, Municipio,lat, lon))
# ajam <- raster::getData('GADM' , country='COL', level=1) 



##### MMMMMMAAAAAPPPAAAAAssss


COL_shp <-  getData('GADM', country='COL', level=1) %>% st_as_sf(HM_shp) 
DPTO_shp <- COL_shp %>% filter(NAME_1 %in% c('Tolima', 'Cesar', 'Huila')) %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

Municipios <- tibble(lon = c(-75.1148, -73.6116, -75.6362),
                   lat = c(4.1941, 8.30686, 2.1954),
                   Municipio = c("San Juan", "Aguachica", "Garzon"))

DEM_dpto <- DEM %>% crop(DPTO_shp) %>% mask(DPTO_shp) %>% 
  rasterToPoints() %>% as_tibble() %>% rename(Alt = COL_msk_alt)

unique(ws_selected$var)

pp <- ggplot()  + 
  geom_tile(data = DEM_dpto, aes(x, y, fill = Alt)) + 
  scale_fill_distiller(palette = "Greys") +
  geom_text(data = DPTO_shp, aes(label = NAME_1, x = lon, y = lat), hjust= -1) +
  geom_point(data = ws_selected, aes(x = lon, y = lat   ,
                                     colour = na_percent,
                                     label = id,
                                     label2 = Municipio,
                                     label3 = Nombre,
                                     label4 = Clase)) +
  viridis::scale_colour_viridis(na.value="white",  direction = -1) + 
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
  geom_point(data = point_site, aes(x = lon, y =  lat, shape = Municipio), color = "red") +
#  facet_wrap(~Departamento) +
  theme_bw() +
  labs(title = paste0("Datos meteorologicos disponibles"), 
       x = 'Longitud', 
       y = 'Latitud', 
       colour = '% NA',
       caption = "Fuente: IDEAM") + 
  theme(
    #axis.text.x = element_text(angle = 90),
    #    legend.position="bottom",
    #        legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))


pp %>% plotly::ggplotly()

make_map <- function(ws_selected, var, ){
  
  
  
}

# Tolima 

Tolima <- ws_selected %>% filter(Departamento == 'TOLIMA')
T_shp <- COL_shp %>% filter(NAME_1 %in% c('Tolima'))
DEM <- getData('alt', country='COL', mask=TRUE) 
DEM_tol <- DEM %>% crop(T_shp) %>% mask(T_shp) %>% 
  rasterToPoints() %>% as_tibble() %>% rename(Alt = COL_msk_alt)

pp <- ggplot() + geom_tile(data = DEM_tol, aes(x, y, fill = Alt)) + 
  scale_fill_distiller(palette = "Greys") +
  geom_point(data = Tolima, aes(x = lon, y = lat   ,
                                colour = na_percent,
                                label = Municipio,
                                label2 = Nombre, label3 = id)) + facet_grid(~var) + 
  viridis::scale_colour_viridis(na.value="white",  direction = -1) + 
  geom_sf(data = T_shp, fill = NA, color = gray(.1)) +
  geom_point(data = san_juan, aes(x = lon, y =  lat), colour = 'red') +
  theme_bw() +
  labs(title = "TOLIMA. Estaciones Pluviometricas", 
       x = 'Longitud', 
       y = 'Latitud', 
       colour = '% NA',
       caption = "Fuente: IDEAM") + 
  theme(
    #axis.text.x = element_text(angle = 90),
#    legend.position="bottom",
    #        legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))

a <- pp %>% plotly::ggplotly()

htmlwidgets::saveWidget(a, 'Tets_all_vars.html')





## Search nearest point between two location
coor <- Tolima %>% dplyr::select(lon, lat) # tibble(lat = c(8.839527778, 5.177083333) , lon = c(-75.80188889, -72.54738889))

set1_sp <- SpatialPoints(coor)
set2_sp <- SpatialPoints(san_juan)


set2_sp$similar_set1 <- as.numeric(apply(gDistance(set1_sp, set2_sp, byid=TRUE), 1, which.min))





