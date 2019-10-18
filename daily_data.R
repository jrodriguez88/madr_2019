###  Import IDEAM_data from //dapadfs --- functions. 
# https://github.com/jrodriguez88/Madr_semillas
# Author: Rodriguez-Espinoza J., Esquivel A.
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

load(file = "catalog.rds")
# = -----
# Change this when we have monthly data...
source('data_dapadfs.R') # Por ahora no se debe correr directamente...

# =------------------------------------------------------------------------------------
# =------------------------------------------------------------------------------------
# Maize. 
# =------------------------------------------------------------------------------------
# =------------------------------------------------------------------------------------
# =----------------------------------------------------------
# Results Maiz
# =----------------------------------------------------------
cowsay::say('Maiz - Results - Station detection', by = 'smallcat')

#############################################################
## Filter data by Departamento, Categoria, status
dpto <- c('CESAR', 'TOLIMA', 'HUILA')

cat <- c("CLIMATOLOGICA PRINCIPAL", "CLIMATOLOGICA ORDINARIA", 
         "AGROMETEOROLOGICA", "SINOPTICA PRINCIPAL",
         "SINOPTICA SECUNDARIA", "METEOROLOGICA ESPECIAL", "PLUVIOMETRICA" )

status <- c("ACTIVA", "SUSPENDIDA", "FUERA DE SERVICIO")


## Filter IDEAM catalog
data_filter <- filter_ideam(catalog, dpto, cat, status= "ACTIVA") 

## Scan in data_cluster4
path <- "//dapadfs/data_cluster_4/observed/weather_station/col-ideam/daily-raw"

## Set variables to join
var_selec <- c("prec", "tmax", "tmin", "sbright", "rhum")


## Set IDEAM id station 
ideam_id <- data_filter$id

#list_files_select <- scan_ciat_db(path, ideam_id, var_selec)
list_files_select <- map(var_selec, ~scan_ciat_db(path, ideam_id, .x))

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


ws_selected <- ideam_raw %>% bind_rows(.id = "var") %>%
  dplyr::select(-data) %>% 
  left_join(catalog %>% 
              dplyr::select(id, Nombre, Categoria, Departamento, Municipio,lat, lon))


# Maps for Maize... 
cowsay::say('Maiz - Maps', by = 'smallcat')

# =----------------------------------------------
departamento <- c( 'Cesar', 'Tolima', 'Huila')

# Este objeto por ahora es para maiz... (por eso esta comentado temporalmente)
localidad <- tibble(lon = c(-75.1148, -73.6116, -75.6362),
                    lat = c(4.1941, 8.30686, 2.1954),
                    Municipio = c("San Juan", "Aguachica", "Garzon"), 
                    Dep = c('Tolima', 'Cesar', 'Huila'))
variable <- unique(ws_selected$var)


##Output folder
path <- getwd()
dir.create(paste0(path, "/maize/plots_ideam_20_more"))

tictoc::tic()
plots_by_var <- map(variable, ~make_map(ws_selected, .x, departamento, localidad = localidad))
tictoc::toc() # 14.53 sec. 

tictoc::tic()
walk2(plots_by_var, variable, 
      ~ggsave(filename = paste0(paste0(path, "/maize/plots_ideam_20_more/",.y, ".png")), .x, height = 8, width = 11, units = "in"))
tictoc::toc() # 73.25/60 = 1.22


# =-=...
tictoc::tic()
plots_by_department <- map(departamento, ~make_map_by(ws_selected, variable, .x, localidad = localidad))
tictoc::toc() # 3.14  sec. 


tictoc::tic()
walk2(plots_by_department, departamento, 
      ~ggsave(filename = paste0(paste0(path, "/maize/plots_ideam_20_more/",
                                       .y, ".png")), .x, height = 6, width = 12, units = "in"))
tictoc::toc() #



# =----- ...

new_select <- ws_selected %>%
  mutate(NA_per_cat = case_when(
    var == "prec"  & na_percent <= 20 ~ 'Cumple',
    var == "prec"  & na_percent > 20 ~ 'No cumple',
    var %in% c("tmax", "tmin", "sbright", "rhum")  & na_percent <= 30 ~ 'Cumple',
    var %in% c("tmax", "tmin", "sbright", "rhum")  & na_percent > 30 ~ 'No cumple',
    TRUE ~ var))  %>% 
  # filter(var %in% c("prec", "tmax", "tmin", "sbright")) %>% 
  filter(var %in% c("prec", "tmax", "tmin")) %>% 
  filter(NA_per_cat == 'Cumple')


# All variables at the same time...
id_s <- new_select  %>% group_by(id) %>% summarise(freq = n()) %>% ungroup() %>% filter(freq == max(freq)) %>% pull(id)

all_varS <- new_select %>% filter(id %in% id_s)

# For graph
COL_shp <-  getData('GADM', country='COL', level=1) %>% crop(extent(-81 , -66.7 , 0 , 12.5 )) %>% st_as_sf()
DPTO_shp <- COL_shp %>% filter(NAME_1 %in% departamento) %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

DEM_dpto <- getData('alt', country = 'COL') %>%
  crop(DPTO_shp) %>% mask(DPTO_shp) %>%
  rasterToPoints() %>% as_tibble() %>%
  rename(Alt = COL_msk_alt)


label_var <- as_labeller(c("prec" = "Precipitacion", "sbright" = "Brillo Solar",
                           "tmax" = "Temperatura Maxima", "tmin" = "Temperatura Minima"))

pp <- ggplot()  +
  geom_tile(data = DEM_dpto, aes(x, y, fill = Alt)) +
  scale_fill_distiller(palette = "Greys") +
  geom_text(data = DPTO_shp, aes(label = NAME_1, x = lon, y = lat), hjust= -1) +
  geom_point(data = all_varS, aes(x = lon, y = lat   ,
                                 colour = na_percent,
                                 label = id,
                                 label2 = Municipio,
                                 label3 = Nombre,
                                 label4 = Categoria)) +
  viridis::scale_colour_viridis(na.value="white",  direction = -1) +
  geom_point(data = localidad, aes(lon, lat), colour = 'red')+
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
  #    geom_point(data = localidad, aes(x = lon, y =  lat, shape = Municipio), color = "red") +
  facet_wrap(~var, labeller = label_var) +
  theme_bw() +
  labs(title = glue::glue('Total: {length(id_s)}'), x = 'Longitud',
       y = 'Latitud',
       colour = '% NA',
       caption = "Fuente: IDEAM") +
  theme( #axis.text.x = element_text(angle = 90), legend.position="bottom", legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))

ggsave(pp, filename = 'maize/plots_ideam_20_more/all_var.png', height = 8, width = 11, units = "in")




prueba <- all_varS %>% filter(Departamento == 'CESAR')



localidad 

stations <- prueba %>% 
  dplyr::select(id, lon, lat, Municipio) %>% 
  unique()

test1 <- st_point(c(-73.6, 8.31))

stations %>% 
  mutate(sf_point = purrr::map2(.x = lon, .y = lat, .f = function(x,y){st_point(c(x,y))})) %>% 
  mutate(distance = purrr::map(.x = sf_point, .f = function(x, point){st_distance(x, point) %>% .[,1]}, point = test1)) %>% 
  unnest(distance)



l_mod <- localidad %>%
  rename('Departamento'  = 'Dep') %>% 
  mutate(Departamento = toupper(stri_trans_general(Departamento,"upper"))) %>% 
  nest(-Municipio, -Departamento) %>% 
  rename('coord' = 'data')



sf_dist_mod <- function(data, point_st){
  
  point <- st_point(point_st %>% as.numeric(.))
  
  data_dist <- data %>% 
    mutate(dist = purrr::map(.x = sf_point, .f = function(x, point){st_distance(x, point) %>% .[,1]}, point = point)) %>% 
    unnest(dist) %>% 
    dplyr::select(-sf_point) %>% 
    arrange(dist)
  
  return(data_dist)}


f <- all_varS %>% 
  nest(-Departamento) %>% inner_join(. , l_mod) %>% 
  mutate(data_mod = purrr::map(.x = data, .f = function(x){ x %>% dplyr::select(id, lon, lat, Municipio) %>% 
      unique() %>%  mutate(sf_point = purrr::map2(.x = lon, .y = lat, .f = function(x,y){st_point(c(x,y))}))})) 


station_t <- f %>% 
  mutate(distance = purrr::map2(.x = data_mod, .y = coord, .f = function(x, y){sf_dist_mod(x, y)})) %>% 
  dplyr::select(Departamento,  Municipio, distance) %>% 
  rename(Obj_M = 'Municipio') %>% 
  mutate(min = purrr::map(.x = distance, .f = function(x){slice(x, 1)})) %>% 
  unnest(min)



#  Estaciones mas cercanas?????







shp <-  getData('GADM', country='COL', level=2) %>% crop(extent(-81 , -66.7 , 0 , 12.5 )) %>% st_as_sf()
other_shp <- shp %>% filter(NAME_1 %in% departamento) %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

ggplot()  +
  geom_tile(data = DEM_dpto, aes(x, y, fill = Alt), alpha = 0.7) +
  scale_fill_distiller(palette = "Greys") +
  geom_text(data = DPTO_shp, aes(label = NAME_1, x = lon, y = lat), hjust= -1) +
  geom_point(data = localidad, aes(lon, lat), colour = 'red')+
  geom_point(data = station_t, aes(lon, lat), colour = 'blue')+
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
  geom_sf(data = other_shp,  fill = NA, color = gray(.1)) +
  theme_bw()







  all_p <- station_t %>% dplyr::select(Departamento, distance) %>% 
    filter(Departamento == 'HUILA') %>% unnest()
  
  
  ggplot()  +
    geom_point(data = filter(all_p, Departamento == 'HUILA'), aes(lon, lat), colour = 'orange')+
    geom_point(data = filter(localidad, Dep == 'Huila'), aes(lon, lat), colour = 'red')+
    geom_point(data = filter(station_t, Departamento == 'HUILA') , aes(lon, lat), colour = 'blue')+
    # geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
    geom_sf(data = filter(DPTO_shp, NAME_1 == 'Huila'), fill = NA, color = gray(.1)) +
    geom_sf(data = filter(other_shp, NAME_1 == 'Huila'),  fill = NA, color = gray(.1)) +
  theme_bw()
  
  
  
  
  

# %>% mutate(distance = purrr::map(.x = data_mod, y = point_st, .f = 
             # function(x, y){st_distance(x %>% select(sf_point), y) %>% .[,1]}))






# tictoc::tic()
# plots_html <- map(plots_by_var, ggplotly)
# walk2(plots_html, paste0(path, "/maize/plots_ideam_20_more/", variable, ".html"), 
#       ~saveWidget(.x, file.path(normalizePath(dirname(.y)), basename(.y))))
# tictoc::toc() # 4.487 min. 



