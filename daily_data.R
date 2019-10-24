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
source('QC_daily.R')

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


# =----------------------------------------
# =----- ... Graph cumplimiento. 
# =----------------------------------------

# Creacion de la variable (tiene menos de cierto porcentaje de NA).
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

# Grafico con el total de estaciones que poseen todas las variables (prec, tmax, tmin).
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



# =-------------------------------------------------------
# Nearest station --- Encontrar las estaciones cercanas.
# =-------------------------------------------------------

# =--------------------------------------------------
# Function to calculate spatial distances at x point.
sf_dist_mod <- function(data, point_st){
  
  point <- st_point(point_st %>% as.numeric(.))
  
  data_dist <- data %>% 
    mutate(dist = purrr::map(.x = sf_point, .f = function(x, point){st_distance(x, point) %>% .[,1]}, point = point)) %>% 
    unnest(dist) %>% 
    dplyr::select(-sf_point) %>% 
    arrange(dist)
  
  return(data_dist)}
# =--------------------------------------------------

# modification of the locality object.
l_mod <- localidad %>%
  rename('Departamento'  = 'Dep') %>% 
  mutate(Departamento = toupper(stri_trans_general(Departamento,"upper"))) %>% 
  nest(-Municipio, -Departamento) %>% 
  rename('coord' = 'data')

# modification of the stations to turn them into an sf object.
data_spatial <- all_varS %>% 
  nest(-Departamento) %>% inner_join(. , l_mod) %>% 
  mutate(data_mod = purrr::map(.x = data, .f = function(x){ x %>% dplyr::select(id, lon, lat, Municipio) %>% 
      unique() %>% mutate(sf_point = purrr::map2(.x = lon, .y = lat, .f = function(x,y){st_point(c(x,y))}))})) 

# Calculation of locality-stations distances.
station_t <- data_spatial %>% 
  mutate(distance = purrr::map2(.x = data_mod, .y = coord, .f = function(x, y){sf_dist_mod(x, y)})) %>% 
  dplyr::select(Departamento,  Municipio, distance) %>% 
  rename(Obj_M = 'Municipio') %>% 
  mutate(min = purrr::map(.x = distance, .f = function(x){slice(x, 1)})) %>% 
  unnest(min)


# =-------------------------------------------
#  Graph nearest stations to each locality. 
# =-------------------------------------------

shp <-  getData('GADM', country='COL', level=2) %>% crop(extent(-81 , -66.7 , 0 , 12 )) %>% st_as_sf()
other_shp <- shp %>% filter(NAME_1 %in% departamento) %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

nearest <- ggplot()  +
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp,  aes(fill = NAME_1), color = gray(.3), alpha = 0.5) +
  geom_sf(data = other_shp,  fill = NA, color = gray(.1)) +
  scale_fill_viridis_d() +
  geom_text(data = DPTO_shp, aes(label = NAME_1, x = lon, y = lat), hjust= -1) +
  geom_point(data = localidad, aes(lon, lat), colour = 'red')+
  geom_point(data = station_t, aes(lon, lat), colour = 'blue')+
  theme_bw()+
  theme(legend.position = 'none') +
  labs( y = 'Latitud', x = 'Longitud', caption = "Fuente: IDEAM")

ggsave(nearest, filename = 'maize/plots_ideam_20_more/nearest.png', height = 8, width = 11, units = "in")



# =--------------------------------------------------
# Function graph for each department near stations for each locality. 
by_each_depar_graph <- function(data){
  
  all_p <- data %>% dplyr::select(Departamento, distance) %>% unnest()
  
  dep_st <- ggplot()  +
    geom_point(data = all_p, aes(lon, lat, shape = "a"), colour = 'orange')+
    geom_point(data = data %>% select(data) %>% unnest(data), aes(lon, lat, shape = "b"), colour = 'red')+
    geom_point(data = data , aes(lon, lat, shape = "c"), colour = 'blue')+
    scale_shape_manual(name   = "Estado",
                       values = c(a = 7, b = 8, c = 9),
                       labels = c("Cercana","Objetivo","Selecionada"), 
                       guide  = ggplot2::guide_legend(override.aes = list(colour = c('orange', 'red', 'blue'))))+ 
    geom_sf(data = filter(DPTO_shp, NAME_1 == pull(data, Dep)), fill = NA, color = gray(.3)) +
    geom_sf(data = filter(other_shp, NAME_1 == pull(data, Dep)),  fill = NA, color = gray(.1)) +
    labs( title = pull(data, Dep), y = 'Latitud', x = 'Longitud', caption = "Fuente: IDEAM") + 
    theme_bw()
  
  ggsave(dep_st, filename = glue::glue('maize/plots_ideam_20_more/{pull(data, Dep)}.png'), 
         height = 8, width = 11, units = "in")
}
# =-------------------------------------------------

station_t %>%
  mutate(Dep = c('Huila', 'Tolima', 'Cesar'), 
         id = 1:nrow(.)) %>%
  inner_join(localidad %>% nest(-Dep)) %>% 
  nest(-id) %>% 
  mutate(maps = purrr::map(.x = data, .f = by_each_depar_graph))


# tictoc::tic()
# plots_html <- map(plots_by_var, ggplotly)
# walk2(plots_html, paste0(path, "/maize/plots_ideam_20_more/", variable, ".html"), 
#       ~saveWidget(.x, file.path(normalizePath(dirname(.y)), basename(.y))))
# tictoc::toc() # 4.487 min. 



# =--------------------------------------------------
# Extraction final stations
# =--------------------------------------------------

station_t %>% 
  dplyr::select(-distance) %>% 
  write_csv(path = 'maize/final_st.csv')

id_selected <- dplyr::pull(station_t, id)

# =------------------------------------------------------------------------
# tic("making quality control")
# Basado en el qc de Jeison. 

# Join all variables in one tibble by id and date.
full_all_var <- function(data){
  
  date_ini <- lubridate::as_date('1980-01-01')
  date_final <- Sys.Date()
  
  data_mod <- data %>% 
    group_split(var) %>% 
    purrr::map(unnest) %>% 
    reduce(full_join, by = "Date") %>% 
    filter(Date >= date_ini & Date <= date_final) %>% 
    dplyr::select(-contains('var')) 
  
  return(data_mod)}

# =------------------------------------------------------------------------
# Aqui se contruye las bases de datos con todas las variables disponibles. 
# =------------------------------------------------------------------------
fila_1 <- ideam_raw %>%
  bind_rows(.id = "var") %>% 
  ungroup() %>% 
  dplyr::select(-years, -na_percent, -idate, -fdate) %>% 
  filter(var != 'rhum' ) %>% 
  filter(id %in% id_selected) %>% 
  nest(-id) %>% 
  mutate(rows = purrr::map(.x = data, .f = nrow)) %>% 
  unnest(rows) %>% 
  mutate(data_join = purrr::map(.x = data, .f = full_all_var))


# =---- Aqui se hace el QC...
prueba <- dplyr::select(fila_1, -data) %>% 
  rename(data = 'data_join') %>% 
  make_qc(.)






# =--------------------------------------
# Qc_sbright
# =--------------------------------------
cowsay::say(  what =  'Warning!!!! \nPara correr esta parte primero es necesario hacer el llenado de las temp', by = 'smallcat')
# Preparando los datos para que pasen al cod de jeffer. 


cat_mod <- catalog %>% filter(id %in% id_selected) %>% dplyr::select(id, Nombre, Altitud)


datos <- inner_join(prueba, station_t) %>% 
  inner_join(. , cat_mod) %>% 
  rename(alt = 'Altitud')


datos %>% 
  dplyr::select(-data, -qc_climate, -distance) %>% 
  mutate(Nombre = str_replace_all(Nombre, ' ', '_')) %>% 
  write_csv(path = 'maize/final_st.csv')


dir.create(path = 'maize/original_data')
dir.create(path = 'maize/qc_data')

datos %>% 
  dplyr::select(Nombre, Municipio, Obj_M, data, qc_climate) %>% 
  mutate(Nombre = str_replace_all(Nombre, ' ', '_')) %>% 
  unnest(Nombre) %>%
  mutate(name = glue::glue('{Obj_M}_{Municipio}') %>% str_replace(' ', '_') %>% stri_trans_general( id = "Latin-ASCII") ) %>% 
  dplyr::select(-Municipio, -Obj_M) %>% 
  mutate(save_data = purrr::map2(.x = data, .y = name, .f = function(x, y){
    write_csv(x = x, path = glue::glue('maize/original_data/{y}.csv'))}), 
    save_qc = purrr::map2(.x =  qc_climate , .y = name, .f = function(x, y){
      write_csv(x = x, path = glue::glue('maize/qc_data/qc_{y}.csv'))}))



# Correr radiacion... y guardar los archivos brutos y qc (por si acaso)... 
# Luego guardar los de radiacion...




srad_if <- function(exist, data, lat, lon, alt){
  
  if(exist == TRUE){
    data_mod <- data %>% mutate(srad = get_srad_ideam(., lat, lon, alt)) 
    }else{  data_mod <- data %>%
        mutate(srad = get_srad_ideam(., lat, lon, alt, kRs = 0.165))}
  
return(data_mod)}


tratando <- datos %>% 
  dplyr::select(-distance, -dist, -data, -Obj_M) %>%
  mutate(Nombre = str_replace_all(Nombre, ' ', '_')) %>% 
  unnest(Nombre) %>% 
  mutate(cond = purrr::map(.x = qc_climate, .f = function(x){isTRUE(sum(names(x) == 'sbright') == 1)})) %>% 
  unnest(cond) %>% 
  mutate(data_srad = pmap(.l = list(cond, qc_climate, lat, lon, alt), .f = srad_if))




tratando %>% 
  dplyr::select(id, data_srad) %>% 
  filter(row_number() == 3) %>% 
  unnest()

