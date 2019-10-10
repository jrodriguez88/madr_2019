###  Import IDEAM_data from //dapadfs
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


# =----------------------------------------------------------
# Functions. 
# =----------------------------------------------------------

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
}

# 
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
  
  return(list_files_select)}

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


### Function to make map of weather stations by meteorological variable and department
make_map <- function(ws_data, variable, departamento, localidad) {
  
  ws_data <- ws_data %>% dplyr::filter(var == variable)
  labels <- enframe(variable) %>% 
    mutate(label = case_when(value == "prec" ~ "Precipitacion", 
                             value == "rhum" ~ "Humedad_Relativa",
                             value == "sbright" ~ "Brillo_Solar",
                             value == "tmax" ~ "Temperatura_Maxima",
                             value == "tmin" ~ "Temperatura_Minima",
                             TRUE ~ value)) %>%
    pull(label)
  
  stopifnot(require(sf))
  # 106 mod...
  COL_shp <-  getData('GADM', country='COL', level=1) %>% crop(extent(-81 , -66.7 , 0 , 12.5 )) %>% st_as_sf() 
  DPTO_shp <- COL_shp %>% filter(NAME_1 %in% departamento) %>% 
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))
  
  DEM_dpto <- getData('alt', country = 'COL') %>% 
    crop(DPTO_shp) %>% mask(DPTO_shp) %>% 
    rasterToPoints() %>% as_tibble() %>% 
    rename(Alt = COL_msk_alt)
  
  
  pp <- ggplot()  +
    geom_tile(data = DEM_dpto, aes(x, y, fill = Alt)) + 
    scale_fill_distiller(palette = "Greys") +
    geom_text(data = DPTO_shp, aes(label = NAME_1, x = lon, y = lat), hjust= -1) +
    geom_point(data = ws_data, aes(x = lon, y = lat   ,
                                   colour = na_percent,
                                   label = id,
                                   label2 = Municipio,
                                   label3 = Nombre,
                                   label4 = Categoria)) +
    viridis::scale_colour_viridis(na.value="white",  direction = -1) + 
    geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
    geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
    #    geom_point(data = localidad, aes(x = lon, y =  lat, shape = Municipio), color = "red") +
    #  facet_wrap(~Departamento) +
    theme_bw() +
    labs(title = paste0("Datos disponibles - ", labels), 
         x = 'Longitud', 
         y = 'Latitud', 
         colour = '% NA',
         caption = "Fuente: IDEAM") + 
    theme( #axis.text.x = element_text(angle = 90), legend.position="bottom", legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold"))
  
  if(is.na(localidad)){
    pp <- pp
  } else {
    pp <- pp + geom_point(data = localidad, aes(x = lon, y =  lat, shape = Municipio), color = "red")}
  
  return(pp)}

make_map_by <- function(ws_data, variable, departamento, localidad = NA) {
  
  labels <- enframe(variable) %>% 
    mutate(label = case_when(value == "prec" ~ "Precipitacion", 
                             value == "rhum" ~ "Humedad_Relativa",
                             value == "sbright" ~ "Brillo_Solar",
                             value == "tmax" ~ "Temperatura_Maxima",
                             value == "tmin" ~ "Temperatura_Minima",
                             TRUE ~ value)) %>%
    pull(label)

  label_var <- as_labeller(c("prec" = "Precipitacion", "rhum" = "Humedad Relativa", "sbright" = "Brillo Solar",
                 "tmax" = "Temperatura Maxima", "tmin" = "Temperatura Minima"))
  
  #  var <- unique(ws_data$var)
  
  stopifnot(require(sf))
  
  DPTO_shp <- getData('GADM', country='COL', level=1) %>% st_as_sf()  %>%
    filter(NAME_1 %in% departamento) %>% 
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))
  
  DEM_dpto <- getData('alt', country = 'COL') %>% 
    crop(DPTO_shp) %>% mask(DPTO_shp) %>% 
    rasterToPoints() %>% as_tibble() %>% 
    rename(Alt = COL_msk_alt)
  
  ws_data <- ws_data %>% filter(Departamento == toupper(stri_trans_general(departamento,"upper")))
  
  pp <- ggplot()  +
    geom_tile(data = DEM_dpto, aes(x, y, fill = Alt)) + 
    scale_fill_distiller(palette = "Greys") +
    #    geom_text(data = DPTO_shp, aes(label = NAME_1, x = lon, y = lat), hjust= -1) +
    geom_point(data = ws_data, aes(x = lon, y = lat   ,
                                   colour = na_percent,
                                   label = id,
                                   label2 = Municipio,
                                   label3 = Nombre,
                                   label4 = Categoria)) + 
    facet_wrap(~var, labeller = label_var) + 
    viridis::scale_colour_viridis(na.value="white",  direction = -1) + 
    #    geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
    geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
    #    geom_point(data = localidad, aes(x = lon, y =  lat, shape = Municipio), color = "red") +
    #  facet_wrap(~Departamento) +
    theme_bw() +
    labs(title = paste0("Datos disponibles - ", departamento), 
         x = 'Longitud', y = 'Latitud', colour = '% NA', caption = "Fuente: IDEAM") + 
    theme(
      #axis.text.x = element_text(angle = 90), legend.position="bottom", legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold"))
  
  # if_else(is.na(localidad), pp, pp + geom_point(data = localidad, aes(x = lon, y =  lat, shape = Municipio), color = "red"))
  if(is.na(localidad)){
    pp <- pp
  } else {
    pp <- pp + geom_point(data = localidad, aes(x = lon, y =  lat, shape = Municipio), color = "red")}
  
  return(pp)}




# =----------------------------------------------------------
# Results Ganadaderia
# =----------------------------------------------------------
cowsay::say('Ganadaderia - Results', by = 'smallcat')

#############################################################
## Filter data by Departamento, Categoria, status
dpto <- c( 'CUNDINAMARCA', 'BOYACA', 'CORDOBA', 'CESAR', 'ANTIOQUIA')

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



##### MMMMMMAAAAAPPPAAAAAssss
########################## Export plots *.png and *.html 

departamento <- c( 'Cundinamarca', 'Boyacá', 'Córdoba', 'Cesar', 'Antioquia')

# Este objeto por ahora es para maiz... (por eso esta comentado temporalmente)
# localidad <- tibble(lon = c(-75.1148, -73.6116, -75.6362),
#                     lat = c(4.1941, 8.30686, 2.1954),
#                     Municipio = c("San Juan", "Aguachica", "Garzon"))
variable <- unique(ws_selected$var)


##Output folder
path <- getwd()
dir.create(paste0(path, "/plots_ideam"))

tictoc::tic()
plots_by_var <- map(variable, ~make_map(ws_selected, .x, departamento, localidad = NA))
tictoc::toc() # 14.53 sec. 

tictoc::tic()
plots_by_department <- map(departamento, ~make_map_by(ws_selected, variable, .x))
tictoc::toc() # 3.14  sec. 


tictoc::tic()
walk2(plots_by_var, variable, 
     ~ggsave(filename = paste0(paste0(path, "/plots_ideam/",.y, ".png")), .x, height = 11, width = 8, units = "in"))
tictoc::toc() # 73.25/60 = 1.22

tictoc::tic()
walk2(plots_by_department, departamento, 
      ~ggsave(filename = paste0(paste0(path, "/plots_ideam/",
                                       .y, ".png")), .x, height = 8, width = 11, units = "in"))
tictoc::toc() # 

tictoc::tic()
plots_html <- map(plots_by_var, ggplotly)
walk2(plots_html, paste0(path, "/plots_ideam/", variable, ".html"), 
      ~saveWidget(.x, file.path(normalizePath(dirname(.y)), basename(.y))))
tictoc::toc() # 
