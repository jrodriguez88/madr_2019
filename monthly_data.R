# =--------------------------------------------------------------------
# https://github.com/jrodriguez88/Madr_semillas
# Author: Rodriguez-Espinoza J., Esquivel A.
# 2019
# cattle raising --- Script para ganaderia (datos mensuales).
# =--------------------------------------------------------------------

rm(list = ls()) ; gc(reset = TRUE)

# =----------------------------
# Packages
# =----------------------------

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



# Load important components...
load(file = "catalog.rds")

# Por ahora correr asi...
source('data_dapadfs.R') # Por ahora no se debe correr directamente..
source('QC_daily.R')


# =----------------------------------------------------------
# Results Ganadaderia
# =----------------------------------------------------------

# Filter data by Departamento, Categoria, status
dpto <- c( 'CUNDINAMARCA', 'BOYACA', 'CORDOBA', 'CESAR', 'ANTIOQUIA')
 
cat <- c("CLIMATOLOGICA PRINCIPAL", "CLIMATOLOGICA ORDINARIA",
         "AGROMETEOROLOGICA", "SINOPTICA PRINCIPAL",
         "SINOPTICA SECUNDARIA", "METEOROLOGICA ESPECIAL", "PLUVIOMETRICA" )
 
status <- c("ACTIVA", "SUSPENDIDA", "FUERA DE SERVICIO")

# Filter IDEAM catalog
data_filter <- filter_ideam(catalog, dpto, cat, status= "ACTIVA") 
 
# Scan in data_cluster4
path <- "//dapadfs/data_cluster_4/observed/weather_station/col-ideam/daily-raw"
 
# Set variables to join
var_selec <- "prec"
 
# Set IDEAM id station 
ideam_id <- data_filter$id

# list_files_select <- scan_ciat_db(path, ideam_id, var_selec)
list_files_select <- map(var_selec, ~scan_ciat_db(path, ideam_id, .x))

# wdata_tb <- get_station_data(list_files_select)
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


# =-------------------------------------------------------------------------------------
# =-------------------------------------------------------------------------------------

##### MMMMMMAAAAAPPPAAAAAssss
####### Export plots *.png and *.html

departamento <- c( 'Cundinamarca', 'Boyacá', 'Córdoba', 'Cesar', 'Antioquia')
# 
# # Este objeto por ahora es para maiz... (por eso esta comentado temporalmente)
# localidad <- tibble(lon = c(-75.1148, -73.6116, -75.6362),
#                     lat = c(4.1941, 8.30686, 2.1954),
#                     Municipio = c("San Juan", "Aguachica", "Garzon"), 
#                     Dep = c('Tolima', 'Cesar', 'Huila'))

# Localidades... temporales (de interes). 
# N_mun <- tibble(Municipio = c('PLANETA RICA', 'LORICA', 'SANTA ROSA DE OSOS', 'CAUCASIA', 'ZIPAQUIRÁ', 'DUITAMA'), 
#                 Dep = c('CÓRDOBA', 'CÓRDOBA', 'ANTIOQUIA', 'ANTIOQUIA', 'CUNDINAMARCA', 'BOYACÁ'))
# 
# ws_selected %>% filter(Municipio %in% pull(N_mun, Municipio)) %>% 
#   bind_rows(. , filter(ws_selected, Departamento == 'CESAR')) 

variable <- unique(ws_selected$var)

##Output folder
path <- getwd()
dir.create(paste0(path, "/ganaderia/plots_ideam_20_more"))

tictoc::tic()
plots_by_var <- map(variable, ~make_map(ws_selected, .x, departamento, localidad = NA))
tictoc::toc() # 14.53 sec.

tictoc::tic()
plots_by_department <- map(departamento, ~make_map_by(ws_selected, variable, .x))
tictoc::toc() # 3.14  sec.


tictoc::tic()
walk2(plots_by_var, variable,
     ~ggsave(filename = paste0(paste0(path, "/ganaderia/plots_ideam_20_more/",.y, ".png")), .x, height = 8, width = 11, units = "in"))
tictoc::toc() # 73.25/60 = 1.22

tictoc::tic()
walk2(plots_by_department, departamento,
      ~ggsave(filename = paste0(paste0(path, "/ganaderia/plots_ideam_20_more/",
                                       .y, ".png")), .x, height = 8, width = 11, units = "in"))
tictoc::toc() #

tictoc::tic()
plots_html <- map(plots_by_var, ggplotly)
walk2(plots_html, paste0(path, "/ganaderia/plots_ideam_20_more/", variable, ".html"),
      ~saveWidget(.x, file.path(normalizePath(dirname(.y)), basename(.y))))
tictoc::toc() # 4.487 min.



# =------------------------------------------------------------------------------------------------
# =------------------------------------------------------------------------------------------------

# Pruebas nuevas...

# unique(ws_selected$var)

new_select <- ws_selected %>%
  mutate(NA_per_cat = case_when(
    var == "prec"  & na_percent <= 20 ~ 'Cumple',
    var == "prec"  & na_percent > 20 ~ 'No cumple',
    TRUE ~ var))


new_select %>% filter(NA_per_cat == 'Cumple') %>% write_csv(path = 'Use_stations.csv')
new_select %>% write_csv(path = 'ganaderia/stations_20_more.csv')

data_to <- new_select %>%
  group_by(var, Departamento, NA_per_cat) %>%
  summarise(n_St_Na = n())

# Prueba... Si tuviera más variables, saldrian varios paneles... (pero en este caso solo prec). 
bar_by_var <- data_to %>%
  ggplot(aes(x = Departamento, y = n_St_Na, fill = NA_per_cat)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label=n_St_Na), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_viridis_d()+
  facet_grid(~ var, scales = 'free_x') +
  theme_bw() +
  labs( x = 'Departamento', y = '# Estaciones', caption = "Fuente: IDEAM", fill = NULL) +
  theme( #axis.text.x = element_text(angle = 90), legend.position="bottom", legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(bar_by_var, filename = 'ganaderia/var.png', height = 5, width = 10, units = "in")

# =--------------------------------------------------::::::::::::::::
# Nuevos graph solo con los datos con el filtro.
# =--------------------------------------------------::::::::::::::::

##Output folder
dir.create(paste0(path, "/ganaderia/plots_ideam"))

ns <- new_select %>% filter(NA_per_cat == 'Cumple')

tictoc::tic()
plots_by_var <- map(variable, ~make_map(ns, .x, departamento, localidad = NA))
tictoc::toc() # 14.53 sec.

tictoc::tic()
plots_by_department <- map(departamento, ~make_map_by(ns, variable, .x))
tictoc::toc() # 3.14  sec.


tictoc::tic()
walk2(plots_by_var, variable,
      ~ggsave(filename = paste0(paste0(path, "/ganaderia/plots_ideam/",.y, ".png")), .x, height = 8, width = 11, units = "in"))
tictoc::toc() # 73.25/60 = 1.22

tictoc::tic()
walk2(plots_by_department, departamento,
      ~ggsave(filename = paste0(paste0(path, "/ganaderia/plots_ideam/",
                                       .y, ".png")), .x, height = 8, width = 11, units = "in"))
tictoc::toc() #



# =-----------------------


tictoc::tic()
plots_by_var <- map(variable, ~make_map_cat(new_select, .x, departamento, localidad = NA))
tictoc::toc() # 14.53 sec.

tictoc::tic()
walk2(plots_by_var, variable,
      ~ggsave(filename = paste0(paste0(path, "/plots_ideam/cat_",.y, ".png")), .x, height = 8, width = 11, units = "in"))
tictoc::toc() # 73.25/60 = 1.22



tictoc::tic()
plots_by_department <- map(departamento, ~make_map_by_cat(new_select, variable, .x))
tictoc::toc() # 3.14  sec.


tictoc::tic()
walk2(plots_by_department, departamento,
      ~ggsave(filename = paste0(paste0(path, "/plots_ideam/cat_",
                                       .y, ".png")), .x, height = 8, width = 11, units = "in"))
tictoc::toc() #



tictoc::tic()
plots_html <- map(plots_by_var, ggplotly)
walk2(plots_html, paste0(path, "/plots_ideam/", variable, ".html"),
      ~saveWidget(.x, file.path(normalizePath(dirname(.y)), basename(.y))))
tictoc::toc() # 4.487 min.


# =------------------------------------------------------------------------------------------
cowsay::say(what = 'Other graphs', by = 'smallcat')
# =------------------------------------------------------------------------------------------
# Si se tuvieran varias variables, podria ser util. 
# all_var_stations <- ns %>%
#   group_by(id) %>%
#   summarise(freq = n()) %>%
#   ungroup() %>%
#   filter(freq == max(freq)) %>%
#   pull(id)
# 
# all_varS <- ns %>% filter(id %in% all_var_stations)
# 
# 
COL_shp <-  getData('GADM', country='COL', level=1) %>% crop(extent(-81 , -66.7 , 2 , 12.5 )) %>% st_as_sf()
DPTO_shp <- COL_shp %>% filter(NAME_1 %in% departamento) %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))
# 
# DEM_dpto <- getData('alt', country = 'COL') %>%
#   crop(DPTO_shp) %>% mask(DPTO_shp) %>%
#   rasterToPoints() %>% as_tibble() %>%
#   rename(Alt = COL_msk_alt)
# 
# 
# label_var <- as_labeller(c("prec" = "Precipitacion", "rhum" = "Humedad Relativa", "sbright" = "Brillo Solar",
#                            "tmax" = "Temperatura Maxima", "tmin" = "Temperatura Minima"))
# 
# pp <- ggplot()  +
#   geom_tile(data = DEM_dpto, aes(x, y, fill = Alt)) +
#   scale_fill_distiller(palette = "Greys") +
#   geom_text(data = DPTO_shp, aes(label = NAME_1, x = lon, y = lat), hjust= -1) +
#   geom_point(data = all_varS, aes(x = lon, y = lat   ,
#                                  colour = na_percent,
#                                  label = id,
#                                  label2 = Municipio,
#                                  label3 = Nombre,
#                                  label4 = Categoria)) +
#   viridis::scale_colour_viridis(na.value="white",  direction = -1) +
#   geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
#   geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
#   #    geom_point(data = localidad, aes(x = lon, y =  lat, shape = Municipio), color = "red") +
#   facet_wrap(~var, labeller = label_var) +
#   theme_bw() +
#   labs(title = glue::glue('Total: {length(all_var_stations)}'), x = 'Longitud',
#        y = 'Latitud',
#        colour = '% NA',
#        caption = "Fuente: IDEAM") +
#   theme( #axis.text.x = element_text(angle = 90), legend.position="bottom", legend.title = element_blank(),
#     panel.grid.minor = element_blank(),
#     strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
#     strip.text = element_text(face = "bold"))
# 
# ggsave(pp, filename = 'plots_ideam/all_var.png', height = 8, width = 11, units = "in")
# 
# 
# # =--------------------------------------------------
# # Presencia de las 5 variables...
# 
# freq <- all_varS %>%
#   filter(var == 'prec') %>%
#   group_by(Departamento) %>% 
#   summarise(Freq = n() %>% as.factor()) %>% 
#   rename(NAME_1 = 'Departamento') %>% 
#   # mutate( NAME_1 = tolower(stri_trans_general(NAME_1,"upper")))
#   mutate(NAME = toupper(substr(NAME_1, 1, 1)), test = tolower(substr(NAME_1, 2, nchar(NAME_1))), 
#          NAME_1 = glue::glue('{NAME}{test}')) %>% 
#   dplyr::select(-NAME, -test)
# 
# 
# test <- DPTO_shp %>% right_join(. , freq) 
# 
# 
# test_g <- ggplot()  +
#   geom_text(data = test, aes(label = glue::glue('{NAME_1}:{Freq}'), x = lon, y = lat), hjust= -1) +
#   geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
#   geom_sf(data = test, aes(fill =  as.factor(Freq)), color = gray(.1), alpha = 0.5) +
#   geom_point(data = all_varS, aes(x = lon, y = lat   ,
#                                   colour = NA_per_cat,
#                                   label = id,
#                                   label2 = Municipio,
#                                   label3 = Nombre,
#                                   label4 = Categoria), colour = 'red') +
#   theme_bw() +  
#   labs(title = glue::glue('Total: {length(all_var_stations)}'), x = 'Longitud', 
#                      y = 'Latitud', 
#                      colour = NULL, 
#                      caption = "Fuente: IDEAM") + 
#   theme( #axis.text.x = element_text(angle = 90), legend.position="bottom", legend.title = element_blank(),
#     panel.grid.minor = element_blank(),
#     strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
#     strip.text = element_text(face = "bold"), 
#     legend.position = 'none')
# 
# ggsave(test_g, filename = 'plots_ideam/all_var.png', height = 8, width = 11, units = "in")





# =----------------------------------------
# =----- ... QC ...
# =----------------------------------------

# =------------------------------------------------------------------------
# Aqui se contruye las bases de datos con todas las variables disponibles. 
# =------------------------------------------------------------------------
fila_1 <- ideam_raw %>%
  bind_rows(.id = "var") %>% 
  ungroup() %>% 
  dplyr::select(-years, -na_percent, -idate, -fdate, -var) 

# =---- Aqui se hace el QC...

# Monthly... (es la misma cosa, pero tengo que mirar porque no corre)


# Tratando de entender... esto...

# make_qc(filter(fila_1, row_number() == 1) )


# Aqui ya esta paralelizado directo en la funcion...
tictoc::tic()
prueba <- fila_1 %>% make_qc(.)
tictoc::toc() # 12.44 (para 455 estaciones aprox).






# prueba_1 <- prueba %>% 
#   filter(row_number() == 1) %>% 
#   dplyr::select(data) %>% 
#   unnest()



prueba_1 %>% slice(n()) %>% pull(Date)



j <- prueba %>% 
  mutate(NA_data = purrr::map(.x = data, .f = function(x){ dplyr::select(x, prec) %>% naniar::miss_var_summary() }), 
         final_Date = purrr::map(.x = data, .f = function(x){x %>% slice(n()) %>% pull(Date)})) %>% 
  unnest(NA_data)  %>% 
  dplyr::select(-variable) %>% 
  unnest(final_Date) %>% 
  filter(pct_miss <= 20)  %>% 
  mutate(year = lubridate::year(final_Date), month =  lubridate::month(final_Date)) 






j %>% count( year)

# Segundo filtro...
jq <- j %>% 
  filter(year >= 2014) %>% 
  dplyr::select(-data, -n_miss)

# Hacer llenado hasta 2016...
count(jq, year)

# Aqui debemos poner tambien las coordenadas... :S ... 


jq <- data_filter%>% 
  filter(id %in% jq$id) %>% 
  dplyr::select(id, Nombre, Departamento, Municipio, lon, lat, Altitud) %>% 
  nest(-id) %>% 
  rename(local_data = 'data') %>% 
  inner_join(jq)




# =-------------------------------------
library(raster)
library(ncdf4)


ncdf4::nc_open('ganaderia/chirps-v2.0.monthly.nc')
probando_layers <- stack('ganaderia/chirps-v2.0.monthly.nc') %>% raster::crop(. , DPTO_shp) # %>% raster::mask(. , DPTO_shp)

layers_filter <- probando_layers[[1:432]]
names(layers_filter) # (2017-1981)*12 = 432


# ar <- jq %>% filter(row_number() == 2) %>% dplyr::select(local_data) %>% unnest()



spatial_ext <- function(coord_d, spatial_data){
  
  data_ext <- spatial_data %>% 
    raster::extract(., data.frame(x= pull(coord_d, lon),y= pull(coord_d, lat)))  %>% 
    t() %>% 
    as_tibble() %>% 
    set_names('prec_chirps') %>%
    mutate(Date = seq(as.Date("1981-01-01"), as.Date("2016-12-01"), "months")) %>% 
    dplyr::select(Date, prec_chirps)
  
return(data_ext)}


# spatial_ext(ar, layers_filter)

tictoc::tic()
plan(multiprocess)
options(future.globals.maxSize= 891289600)
jq <- jq %>%
  mutate(Chirps_data = furrr::future_map(.x  = local_data, .f = spatial_ext, spatial_data = layers_filter))
gc()
gc(reset = TRUE)
tictoc::toc() # 532.6 = 8.87min




acum_filling_data <- function(qc_data, salellite_data){
  # Llenado de datos y acumulacion 
  # qc_data <- jq %>% filter(row_number() == 1) %>% dplyr::select(qc_climate) %>% unnest
  # salellite_data <- jq %>% filter(row_number() == 1)  %>% dplyr::select(Chirps_data) %>% unnest

  full_NA <- qc_data %>%  
    mutate(year = lubridate::year(Date), month = lubridate::month(Date)) %>% 
    mutate(prueba = ifelse( is.na(prec_qc) == TRUE, 1, 0)) %>% 
    group_by(year, month) %>% 
    summarise( prec = sum(prec_qc, na.rm = TRUE),na = sum(prueba)/n() * 100) %>%
    ungroup() %>%
    mutate(prec = ifelse(na > 10, NA_real_, prec)) %>% 
    dplyr::select(-na)
  
  All <- salellite_data  %>%  
    mutate(year = lubridate::year(Date), month = lubridate::month(Date)) %>% 
    full_join(filter(full_NA, year > 1980) ) 
    
  coef <- lm( prec ~ prec_chirps, data = All)$coefficients %>% as.numeric()
  
  All <- All %>%
    mutate(prec = ifelse(is.na(prec) == TRUE, coef[1] + coef[2] * prec_chirps, prec)) %>% 
    dplyr::select(Date, prec)
    
return(All)}

tictoc::tic()
change_Data <- jq %>% 
  mutate(data_complete = purrr::map2(.x = qc_climate, .y = Chirps_data, .f = acum_filling_data))
tictoc::toc() # 46.86 sec.



# Ya no quedan NA...
#  Aqui ya comenzamos la creacion de los archivos para cpt. 
fuente <- change_Data %>% 
  dplyr::select(id, local_data,  data_complete) %>% 
  unnest(local_data) 



# sitios <- 

Mun_shp <-getData('GADM', country='COL', level=2) %>% crop(extent(-81 , -66.7 , 2 , 12.5 )) %>% st_as_sf()
# DPTO_shp <- COL_shp %>% filter(NAME_1 %in% departamento) %>%
#   mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
#          lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))


# municipio <- Mun_shp %>% filter(NAME_1 %in% departamento) %>% dplyr::select(NAME_1, NAME_2)
Mun__filter <- Mun_shp %>%
  filter(NAME_2 %in% c('Caucasia' , 'Don Matías' , 'San Pedro de los Milagros', 'Santa Rosa de Osos', 
                       'Lorica' ,  'Planeta Rica', 'Chiquinquirá' , 'Duitama', 'Zipaquirá') | 
           NAME_1 == 'Cesar') %>% 
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))




# Antioquia - Caucasia , Don Matías , San Pedro de los Milagros, Santa Rosa de Osos 
# Córdoba - Lorica ,  Planeta Rica 
# Boyacá - Chiquinquirá , Duitama
# Cundinamarca - Zipaquirá
# Cesar - Todas...

pp <- ggplot()  +
  geom_sf(data = Mun__filter, aes(fill = NAME_1), color = gray(.5)) + 
  geom_text(data = DPTO_shp, aes(label = NAME_1, x = lon, y = lat), hjust= -1) +
  geom_point(data = fuente, aes(x = lon, y = lat   , 
                                 label = id,
                                 label2 = Municipio,
                                 label3 = Nombre)) +
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
  theme_bw() +
  labs(title = glue::glue('Total: {nrow(fuente)}'), x = 'Longitud',
       y = 'Latitud',
       colour = '% NA',
       caption = "Fuente: IDEAM") +
  theme( legend.position = 'none',
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))

ggsave(pp, filename = 'ganaderia/mun.png', height = 8, width = 11, units = "in")


# Esta function debe desarrollarse para buscar las estaciones 
# # Antioquia - Caucasia , Don Matías , San Pedro de los Milagros, Santa Rosa de Osos 
# fuente %>% filter(Departamento == 'ANTIOQUIA' ) %>% pull(Municipio)
# 
# # CAUCASIA, 'SANTA ROSA DE OSOS'
# 
# 
# a <- Mun_shp %>%
#   filter(NAME_2 %in% c('Caucasia')) %>% 
#   mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
#          lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))
# 
# point<- st_centroid(a) %>% dplyr::select(lon, lat) 
# 
# point1 <- st_point(x = c(-76.1,5.88))
# point <- st_point(x = c(point$lon, point$lat))
# 
# st_distance(point, point1) %>% as.numeric()
# 
# 
# sf_dist_mod <- function(data, point_st){
#   
#   point <- st_point(point_st %>% as.numeric(.))
#   
#   data_dist <- data %>% 
#     mutate(dist = purrr::map(.x = sf_point, .f = function(x, point){st_distance(x, point) %>% .[,1]}, point = point)) %>% 
#     unnest(dist) %>% 
#     dplyr::select(-sf_point) %>% 
#     arrange(dist)
#   
#   return(data_dist)}






data_for <- fuente %>% filter(row_number() == 1) %>% dplyr::select(data_complete) %>% unnest()
name <- fuente %>% filter(row_number() == 1) %>% pull(id) 




two_place <- fuente %>% 
  filter(Departamento %in% c('ANTIOQUIA', 'CORDOBA') ) %>% 
  dplyr::select(id, data_complete) %>% 
  unnest(data_complete) %>% 
  tidyr::pivot_wider(names_from = id, values_from = prec)

fuente %>% 
  filter(Departamento %in% c('ANTIOQUIA', 'CORDOBA') ) %>% 
  dplyr::select(lon, lat)






#### Save CPT files by stations. 

# This are tje functions to make the final file. 
# CPT_file_s <- function(data, var){
#   
#   # data <-  MSD_SO
#   # var <- 'start_date'
#   
#   CPT_data <- data %>%
#     # dplyr::select(-type) %>%
#     # unnest %>%
#     rename(id = 'station_N') %>%
#     dplyr::select(year, id, !!var) %>%
#     mutate_if(is.numeric, list(~round(., 1))) %>%
#     replace(is.na(.), -999) %>% 
#     spread(key = id, value = !!var) 
#   
#   Lat_Long  <- data %>%
#     # dplyr::select(-id, -data) %>%
#     # unnest %>%
#     rename(x = Lon, y = Lat) %>% 
#     dplyr::select(x, y) %>%
#     unique %>%
#     t()
#   
#   colnames(Lat_Long) <- paste0(1:10)
#   rownames(Lat_Long) <- NULL
#   
#   Lat_Long <- add_column(as_tibble(Lat_Long), year = c('cpt:X', 'cpt:Y'), .before = 1)
#   
#   names(Lat_Long) <- c('', paste0('V',1:10))
#   names(CPT_data) <- c('', paste0('V',1:10))
#   
#   # =-=-=-=-=-=-=-=-=-=-=-=
#   CPT_data <- CPT_data %>%
#     mutate_if(is.factor, as.character) %>%
#     mutate_if(is.character, as.numeric)  %>%
#     rbind(Lat_Long, .)
#   
#   file <- paste0('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/MSD_Index/to_proof_in_CPT/stations/', var, '.txt')
#   
#   sink(file = file)
#   cat('xmlns:cpt=http://iri.columbia.edu/CPT/v10/', sep = '\n')
#   cat('cpt:nfield=1', sep = '\n')
#   cat(glue("cpt:field=days, cpt:nrow=36, cpt:ncol=10, cpt:col=station, cpt:row=T, cpt:units=julian;cpt:missing=-999"), sep = '\n')
#   cat(write.table(CPT_data, sep = '\t', col.names = TRUE, row.names = FALSE, na = "", quote = FALSE))
#   sink()
# }


