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
library(future)
library(furrr)

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






# =--------------------------------------------------------------------------
# =--------------------------------------------------------------------------
# =--------------------------------------------------------------------------

cowsay::say(what = 'Pruebas en lo referente al llenado de datos', by = 'owl')




datos_mod <- datos  %>% dplyr::select(-data, -dist, -distance, -rows)


# Deberiamos tener los datos diarios para el pais... 
# Otra cosa a tener en cuenta es conocer el nuevo porcentaje de NA... 
# Ademas obtener la nueva fecha final... 
# Tener los datos satelitales de prec y temp. 
# 



# mutate(NA_data = purrr::map(.x = data, .f = function(x){ dplyr::select(x, prec) %>% naniar::miss_var_summary() }), 
#          final_Date = purrr::map(.x = data, .f = function(x){x %>% slice(n()) %>% pull(Date)})) 



#  Haciendo pruebas para todo lo de los NA... entonces 

ajam <- datos_mod %>% 
  dplyr::select(id, Departamento, lon, lat, alt,qc_climate) %>% 
  filter(row_number() == 2) %>% 
  dplyr::select(qc_climate) %>% 
  unnest()





# =-------------------------------------------------- 
# Para descargar NASA-Power...
# Revisar lo que tengo de codigos de diario para obtener los datos de Chirps. 


# Aqui se esta calculando el % de NA despues del control de calidad...
a <- datos_mod %>% 
  dplyr::select( id, Departamento, lon, lat, alt,qc_climate) %>% 
  mutate(NA_preliminar = purrr::map(.x = qc_climate, .f = function(x){x  %>% dplyr::select(-Date) %>% naniar::miss_var_summary()}), 
         Dates = purrr::map(.x = qc_climate, .f = function(x){slice(x, 1, n()) %>% dplyr::pull(Date) %>% tibble(start_Date = .[1], end_Date = .[2]) %>% dplyr::select(-.) %>% unique()})) %>%
  unnest(Dates)




f <- a %>% filter(row_number() == 3) %>% dplyr::select(qc_climate) %>% 
  unnest() 

  ggplot(f, aes(x = Date, y = prec_qc)) + 
  geom_line() + 
  theme_bw()

  ggplot(f, aes(x = Date, y = tmax_qc)) + 
    geom_line() + 
    theme_bw()
  
  ggplot(f, aes(x = Date, y = tmin_qc)) + 
    geom_line() + 
    theme_bw()
  
  
  
  ggplot(f, aes(x = Date, y = sbright)) + 
    geom_line() + 
    theme_bw()
  
  
  
  
# =-----------------------------------------------------------------------------------------
# 1. Hacer el qc para todos los datos del departamento... 
# 2. Filtrar las estaciones con 20% para faltantes
# 3. Pegarle las estaciones de interes...
# 4. Filtrar Tolima. 
# 5. Dejar como periodo de interes 1982 - 2015.
# Ojo!!!! = Mirar el cod de Lizeth antes para verificiar que información se necesita. 
# Guardar los graphs de control de calidad de las estaciones, con periodos de faltantes... 
# =------------------------------------------------------------------------------------------
  
filter(datos, Departamento != 'TOLIMA')
# filter(new_select, Departamento != 'TOLIMA')

id_dep <- data_filter %>% filter(Departamento != 'TOLIMA') %>% pull( id)

fila_2 <- ideam_raw %>%
  bind_rows(.id = "var") %>% 
  ungroup() %>% 
  dplyr::select(-years, -na_percent, -idate, -fdate) %>% 
  filter(var != 'rhum' ) %>% 
  filter(id %in% id_dep) %>%
  nest(-id) %>% 
  mutate(rows = purrr::map(.x = data, .f = nrow)) %>% 
  unnest(rows) %>% 
  mutate(data_join = purrr::map(.x = data, .f = full_all_var))  %>% 
  mutate(ncol = purrr::map(.x = data_join, .f = ncol)) %>% 
  unnest(ncol) %>% 
  filter(ncol > 3)


# Calcular el numero de columnas (por lo menos saber que variables tiene)... 
# fila_2 %>% 
# En fila 2 eliminar aquellos que no tengan tmin 

fila_2 <- fila_2 %>% 
  mutate(var_3 = purrr::map(.x = data_join, .f = function(x){ names(x) %in% c('prec', 'tmax', 'tmin') %>% sum })) %>% 
  unnest(var_3) %>% 
  filter(var_3 == 3)




tictoc::tic()
# =---- Aqui se hace el QC...
prueba2 <- dplyr::select(fila_2, -data, -rows, -ncol) %>% # filter(row_number() == 7) %>% 
  rename(data = 'data_join') %>% 
  make_qc(.)
tictoc::toc() # 7.02 min. 

# Aqui debo poner el departamento... porque necesito hacer el llenado con eso... 
prueba_mod2 <- prueba2 %>% 
  dplyr::select(-var_3, -data) %>% 
  mutate(NA_preliminar = purrr::map(.x = qc_climate, .f = function(x){x  %>% dplyr::select(-Date) %>% naniar::miss_var_summary()}), 
         Dates = purrr::map(.x = qc_climate, .f = function(x){slice(x, 1, n()) %>% dplyr::pull(Date) %>% tibble(start_Date = .[1], end_Date = .[2]) %>% dplyr::select(-.) %>% unique()})) %>%
  unnest(Dates) %>% 
  mutate(Departamento = purrr::map(.x = id, .f = function(x, data){filter(data, id == x) %>% pull(Departamento)}, data = data_filter)) %>% 
  unnest(Departamento)



select_var <- function(x, y, var){
  
  dplyr::select(y, Date, var) %>% 
    setNames(c('Date', glue::glue('s_{x}')))
  
}



# Para prueba...


# QC_by_var <- prueba_mod2 %>% 
#   filter(Departamento == 'HUILA') %>% 
#   dplyr::select(id, qc_climate) %>%
#   mutate(data_tmin = purrr::map2(.x = id, .y = qc_climate, .f = select_var, var = 'tmin_qc'), 
#          data_tmax = purrr::map2(.x = id, .y = qc_climate, .f = select_var, var = 'tmax_qc'), 
#          data_prec = purrr::map2(.x = id, .y = qc_climate, .f = select_var, var = 'prec_qc'))

path_RClimTool <- 'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/maize/para_RClimTool/'

fun_Save <- function(id_st, QC_by_var, var){
  # =------------
  data_para <- QC_by_var %>% 
    dplyr::select(-id) %>%
    group_split(n = row_number()) %>% 
    purrr::map(.f = function(x){select(x, -n)}) %>% 
    purrr::map(unnest) %>% 
    reduce(full_join, by = "Date") %>% 
    filter(Date > '1981-12-31' & Date < '2015-12-31')
  
  id_data <- data_para %>% 
    naniar::miss_var_summary() %>% 
    filter(pct_miss < 35) %>% 
    filter(row_number() != n()) %>% 
    pull(variable)
  
  data_final <- data_para %>% dplyr::select(Date, id_data)
  cond <- sum(isTRUE(names(data_final) %in%  id_st))
  
  if(cond == 0){
    data_total <- bind_cols(data_final, dplyr::select(data_para, id_st))} else{ 
      data_total <-  data_final}
  
  write_csv(x = data_total, path = glue::glue('{path_RClimTool}/Original/{var}_{id_st}.csv'))
}


# =------
prueba_mod2 %>% dplyr::select(Departamento, id, qc_climate) %>% 
  mutate(data_tmin = purrr::map2(.x = id, .y = qc_climate, .f = select_var, var = 'tmin_qc')) %>%
  dplyr::select(-qc_climate) %>% 
  nest(-Departamento) %>% 
  mutate(id_st = paste0('s_', c(21045010, 23215030)))  %>% 
  # filter(row_number() == 1)
  mutate(data_save_min = purrr::map2(.x = id_st, .y = data, .f = fun_Save, var = 'tmin'))

# id_st <- pull(f, id_st)
# var = 'tmin'
# QC_by_var <- dplyr::select(f, data) %>% unnest()


prueba_mod2 %>%
  dplyr::select(Departamento, id, qc_climate) %>%
  mutate(data_tmin = purrr::map2(.x = id, .y = qc_climate, .f = select_var, var = 'tmax_qc')) %>%
  dplyr::select(-qc_climate) %>%
  nest(-Departamento) %>%
  mutate(id_st = paste0('s_', c(21045010, 23215030))) %>%
  mutate(data_save_min = purrr::map2(.x = id_st, .y = data, .f = fun_Save, var = 'tmax'))


prueba_mod2 %>%
  dplyr::select(Departamento, id, qc_climate) %>%
  mutate(data_tmin = purrr::map2(.x = id, .y = qc_climate, .f = select_var, var = 'prec_qc')) %>%
  dplyr::select(-qc_climate) %>%
  nest(-Departamento) %>%
  mutate(id_st = paste0('s_', c(21045010, 23215030))) %>%
  mutate(data_save_min = purrr::map2(.x = id_st, .y = data, .f = fun_Save, var = 'prec'))







# =-------------------------------------------------------------------------------------
cowsay::say('Solo voy a dejar las mismas estaciones en los 3 archivos (Prec-Temp max and min).', 'owl')


path_after <- 'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/Maize/para_RClimTool/Original/'

For_Change <- tibble(names = list.files(path_after) %>% str_remove('.csv') %>% str_remove('_s'), 
       path = list.files(path_after, full.names = TRUE) ) %>% 
  mutate(data =  purrr::map(.x = path, .f = function(x){read.csv(x) %>% as.tibble(.)})) %>% 
  dplyr::select(-path) %>% 
  separate(names, c('var', 'station'), '_') %>% 
  nest(-station) 

# =---------------------------------------------------------------------------------------
# =---------------------------------------------------------------------------------------
cowsay::say('Para que funcione esta parte se debe crear una carrera\nSolo se usa si se desea guardar archivos.')
# Esta parte se corre solo si se necesita correr a menos que se desee guardar algo...
ta <- For_Change %>% dplyr::select(data) %>% filter(row_number() == 2) %>% unnest()
id_grid <- '23215030'

  prec <- ta %>% dplyr::filter(var == 'prec') %>%dplyr::select(data) %>% unnest()
  tmax <- ta %>% dplyr::filter(var == 'tmax') %>% dplyr::select(data) %>% unnest()
  tmin <- ta %>% dplyr::filter(var == 'tmin') %>% dplyr::select(data) %>% unnest()
  
  names_all <- intersect(names(tmax),names(tmin)) %>% intersect(. , names(prec))
  ta <- ta %>% 
    mutate(data_mod = purrr::map(.x = data, .f = function(x, names_select){select(x, names_select)  %>% 
        mutate(day = lubridate::day(Date), month = lubridate::month(Date), year = lubridate::year(Date) ) %>%
        dplyr::select(-Date) %>% 
        dplyr::select(day, month, year, everything())}, names_select = names_all))
  
  ta %>% 
    mutate(save_path = glue::glue('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/para_RClimTool/{id_grid}/{var}_s_{id_grid}.csv'), 
           save = purrr::map2(.x = data_mod, .y = save_path, .f = function(x, y){write_csv(x = x, path = y)}))

# =---------------------------------------------------------------------------------------
# =---------------------------------------------------------------------------------------
  

  # Arreglar el problema de seleccionar desde 
  For_Change %>% dplyr::select(data) %>% filter(row_number() == 1) %>% unnest() %>% 
    filter(row_number() == 3) %>% dplyr::select(data) %>% unnest() %>% pull(Date)
  
  # Desde aqui... entonces 
    
  
  # station_t %>% dplyr::filter(Departamento != 'TOLIMA')
  data_qc <- datos %>% dplyr::filter(Departamento != 'TOLIMA') %>% dplyr::select(-rows, -data, -distance, -dist)
  
  names_stations <- pull(data_qc, id)
  
  # Coordenadas
  
  
  
  # Seleccion de datos de temperatura...
  
  genTmax <- readr::read_csv('maize/para_RClimTool/Datos_faltantes/data_genTmax.csv')
  genTmin <- readr::read_csv('maize/para_RClimTool/Datos_faltantes/data_genTmin.csv')
  
  
  
  # Para Huila... 21045010 
  genTmax %>% dplyr::select(day, month, year, glue::glue('s_21045010')) %>% rename('tmax' = 's_21045010')
  genTmin %>% dplyr::select(day, month, year, glue::glue('s_21045010')) %>% rename('tmin' = 's_21045010')
  

  # Para Cesar 21045010 
  genTmax %>% dplyr::select(day, month, year, glue::glue('s_23215030')) %>% rename('tmax' = 's_23215030')
  genTmin %>% dplyr::select(day, month, year, glue::glue('s_23215030')) %>% rename('tmin' = 's_23215030')
  
  
  
  
  
  # Llenado de datos para prec... 
  library(ncdf4)
  library(raster)
  
  ncdf4::nc_open('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/maize/Data_Chirps.nc')
  Chirps <- raster::stack('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/maize/Data_Chirps.nc')
  Chirps %>% names() %>% length()
  
  
  # Huila ... 
  special_data <- data_qc %>% dplyr::select(lon, lat) %>% filter(row_number() == 1)

  lat <- special_data$lat
  lon <- special_data$lon
    
  options(timeout=180)
    
  json_file <- paste0("https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?&request=execute&identifier=SinglePoint&parameters=PRECTOT&startDate=19820101&endDate=",format(as.Date("2015/12/31"),"%Y%m%d"),"&userCommunity=AG&tempAverage=DAILY&outputList=ASCII&lat=",lat,"&lon=",lon)
  json_data <- rjson::fromJSON(file=json_file)
  
  chirpH <- Chirps %>% 
    raster::extract(., data.frame(x= special_data$lon,y= special_data$lat)) %>% # arreglar aqui 
    t() %>% as_tibble()
  
    
  qc_huila <- data_qc %>% dplyr::select(qc_climate) %>% filter(row_number() == 1) %>%
    unnest() %>% dplyr::select(Date, prec_qc) # %>% 
    # mutate(day = lubridate::day(Date), month = lubridate::month(Date), year = lubridate::year(Date)) %>% 
    # dplyr::select(-Date)
  
  Prec_Huila <- tibble(Date = seq(as.Date("1982/1/1"), as.Date("2015/12/31"), "days")) %>% 
      mutate(NASA_prec = json_data$features[[1]]$properties$parameter$PRECTOT %>% unlist) %>% 
    bind_cols(chirpH) %>% 
    rename( 'Chirps' = 'V1') %>% 
    full_join(. , qc_huila)
  
  
  Huila <-  Prec_Huila %>%
    pivot_longer(cols = c('NASA_prec', 'Chirps', 'prec_qc')) %>%
    ggplot(aes(x = Date, y = value, colour = name)) +
    geom_line() +
    theme_bw() +
    labs(x = NULL, y = 'Precipitación (mm)', colour = NULL)
  
  
  ggsave('maize/para_RClimTool/Huila.png', height = 5, width = 10, units = "in")
  
  
  a_huila <- Prec_Huila %>% drop_na() %>% 
    mutate(day = lubridate::day(Date), 
           month = lubridate::month(Date), 
           year = lubridate::year(Date) )  %>%
    group_by(month) %>%
    yardstick::rmse(prec_qc, Chirps) %>% 
    dplyr::select(month, .estimate) %>%
    rename('Chirps' = '.estimate')
  
  
  
  b_huila <- Prec_Huila %>% drop_na() %>% 
    mutate(day = lubridate::day(Date), 
           month = lubridate::month(Date), 
           year = lubridate::year(Date) )  %>%
    group_by(month) %>%
    yardstick::rmse(prec_qc, NASA_prec) %>% 
    dplyr::select(month, .estimate) %>%
    rename('NASA' = '.estimate')
  
  # En este caso gano NASA
  inner_join(a_huila, b_huila) %>% ungroup() %>%
    mutate(sR = Chirps - NASA) %>% summarise(dif_m = mean(sR))
  
  
  # correction_Huila <- Prec_Huila %>%
  #   drop_na() %>%
  #   filter(Chirps > 0 & NASA_prec > 0) %>%
  #   mutate(div_Chirps = prec_qc/Chirps, div_NASA = prec_qc/NASA_prec) %>%
  #   summarise(mean_div_Chirps = mean(div_Chirps), mean_div_NASA = mean(div_NASA))
  # 
  # Prec_Huila %>%
  #   mutate(NASA_prec = NASA_prec * correction_cesar$mean_div_NASA,
  #          Chirps = Chirps * correction_cesar$mean_div_Chirps) %>%
  # pivot_longer(cols = c('NASA_prec', 'Chirps', 'prec_qc')) %>%
  #   ggplot(aes(x = Date, y = value, colour = name)) +
  #   geom_line() +
  #   theme_bw() +
  #   labs(x = NULL, y = 'Precipitación (mm)', colour = NULL)
  
  
  
  

  tmax_H <- genTmax %>% dplyr::select(day, month, year, glue::glue('s_21045010')) %>% rename('tmax' = 's_21045010')
  tmin_H <- genTmin %>% dplyr::select(day, month, year, glue::glue('s_21045010')) %>% rename('tmin' = 's_21045010')
  
  inner_join(tmax_H, tmin_H) 
  
  
  fill_Huila <- Prec_Huila %>% 
    mutate(prec = ifelse(is.na(prec_qc) == TRUE, NASA_prec, prec_qc)) %>% 
    mutate(day = lubridate::day(Date), 
           month = lubridate::month(Date), 
           year = lubridate::year(Date) )  %>% 
    inner_join(inner_join(tmax_H, tmin_H) ) %>% 
    dplyr::select(Date, tmax, tmin, prec) %>%
    setNames(c('Date', 'tmax_qc', 'tmin_qc', 'prec_qc'))
  
  
  
  Huila_todo <- data_qc %>% filter(row_number() == 1)
  
  Huila_New <- srad_if( exist = FALSE, data = fill_Huila, 
           lat = pull(Huila_todo, lat), lon = pull(Huila_todo, lon), alt = pull(Huila_todo, alt)) %>% 
    setNames(c('Date', 't_max', 't_min', 'prec', 'sol_rad')) %>%
    mutate(day = lubridate::day(Date), 
           month = lubridate::month(Date), 
           year = lubridate::year(Date) )  %>% 
    dplyr::select(-Date) %>% 
    dplyr::select(day, month, year, everything())
    
  
  readr::write_csv(Huila_New, path = 'maize/Huila_Complete.csv')

  
  
  
  
  # =-----------------------------------------------------  
  # =-----------------------------------------------------
  # Cesar ... 
  special_data2 <- data_qc %>% dplyr::select(lon, lat) %>% filter(row_number() == 2)
  
  lat <- special_data2$lat
  lon <- special_data2$lon
  
  options(timeout=180)
  
  json_file2 <- paste0("https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?&request=execute&identifier=SinglePoint&parameters=PRECTOT&startDate=19820101&endDate=",format(as.Date("2015/12/31"),"%Y%m%d"),"&userCommunity=AG&tempAverage=DAILY&outputList=ASCII&lat=",lat,"&lon=",lon)
  json_data2 <- rjson::fromJSON(file=json_file)
  
  chirpC <- Chirps %>% 
    raster::extract(., data.frame(x= special_data2$lon,y= special_data2$lat)) %>% # arreglar aqui 
    t() %>% as_tibble()
  
  
  qc_cesar <- data_qc %>% dplyr::select(qc_climate) %>% filter(row_number() == 2) %>%
    unnest() %>% dplyr::select(Date, prec_qc) # %>% 
  # mutate(day = lubridate::day(Date), month = lubridate::month(Date), year = lubridate::year(Date)) %>% 
  # dplyr::select(-Date)
  
  Prec_Cesar <- tibble(Date = seq(as.Date("1982/1/1"), as.Date("2015/12/31"), "days")) %>% 
    mutate(NASA_prec = json_data2$features[[1]]$properties$parameter$PRECTOT %>% unlist) %>% 
    bind_cols(chirpC) %>% 
    rename( 'Chirps' = 'V1') %>% 
    full_join(. , qc_cesar)
  
  

Cesar <-  Prec_Cesar %>%
    pivot_longer(cols = c('NASA_prec', 'Chirps', 'prec_qc')) %>%
    ggplot(aes(x = Date, y = value, colour = name)) +
    geom_line() +
    theme_bw() +
    labs(x = NULL, y = 'Precipitación (mm)', colour = NULL)


ggsave('maize/para_RClimTool/Cesar.png', height = 5, width = 10, units = "in")

dplyr::select(Prec_Cesar, -Date) %>% drop_na() %>%
    summarise(cor_NASA_Original = cor(NASA_prec, prec_qc), 
              cor_Chirps_Original = cor(Chirps, prec_qc), 
              cor_Chirps_NASA = cor(Chirps, NASA_prec))
  
    
Prec_Cesar %>% drop_na() %>% 
  mutate(day = lubridate::day(Date), 
         month = lubridate::month(Date), 
         year = lubridate::year(Date) ) %>%
  mutate(mean_sa = (NASA_prec + Chirps)/2 ) %>%
  dplyr::group_by(month) %>%
  summarise(cor_mean = cor(mean_sa, prec_qc), 
            kendall = cor(mean_sa, prec_qc, method = 'kendall'),
            cor_NASA_Original = cor(NASA_prec, prec_qc), 
            kendall_NASA_Original = cor(NASA_prec, prec_qc, method = 'kendall'),
            cor_Chirps_Original = cor(Chirps, prec_qc), 
            kendall_Chirps_Original = cor(Chirps, prec_qc, method = 'kendall'), 
            cor_Chirps_NASA = cor(Chirps, NASA_prec), 
            kendall_Chirps_NASA = cor(Chirps, NASA_prec, method = 'kendall')) 



a_cesar <- Prec_Cesar %>% drop_na() %>% 
  mutate(day = lubridate::day(Date), 
         month = lubridate::month(Date), 
         year = lubridate::year(Date) )  %>%
  group_by(month) %>%
  yardstick::rmse(prec_qc, Chirps) %>% 
  dplyr::select(month, .estimate) %>%
  rename('Chirps' = '.estimate')



b_cesar <- Prec_Cesar %>% drop_na() %>% 
  mutate(day = lubridate::day(Date), 
         month = lubridate::month(Date), 
         year = lubridate::year(Date) )  %>%
  group_by(month) %>%
  yardstick::rmse(prec_qc, NASA_prec) %>% 
  dplyr::select(month, .estimate) %>%
  rename('NASA' = '.estimate')


# En este caso gano Chirps 
inner_join(a_cesar, b_cesar)  %>% 
  ungroup() %>%
  mutate(sR = Chirps - NASA) %>% 
  summarise(dif_m = mean(sR))

# Prueba de lo del sesgo... 
# 
# # overlap_cesar <- 
# correction_cesar <- Prec_Cesar %>% 
#   drop_na() %>% 
#   filter(Chirps > 0 & NASA_prec > 0) %>% 
#   mutate(div_Chirps = prec_qc/Chirps, div_NASA = prec_qc/NASA_prec) %>% 
#   summarise(mean_div_Chirps = mean(div_Chirps), mean_div_NASA = mean(div_NASA))
# 
# Prec_Cesar %>% 
#   mutate(NASA_prec = NASA_prec * correction_cesar$mean_div_NASA, 
#          Chirps = Chirps * correction_cesar$mean_div_Chirps) %>%  
# pivot_longer(cols = c('NASA_prec', 'Chirps', 'prec_qc')) %>%
#   ggplot(aes(x = Date, y = value, colour = name)) +
#   geom_line() +
#   theme_bw() +
#   labs(x = NULL, y = 'Precipitación (mm)', colour = NULL)



# Para Cesar 21045010 
tmax_C <- genTmax %>% dplyr::select(day, month, year, glue::glue('s_23215030')) %>% rename('tmax' = 's_23215030')
tmin_C <- genTmin %>% dplyr::select(day, month, year, glue::glue('s_23215030')) %>% rename('tmin' = 's_23215030')

inner_join(tmax_C, tmin_C) 


fill_Cesar <- Prec_Cesar %>% 
  mutate(prec = ifelse(is.na(prec_qc) == TRUE, Chirps, prec_qc)) %>% 
  mutate(day = lubridate::day(Date), 
         month = lubridate::month(Date), 
         year = lubridate::year(Date) )  %>% 
  inner_join(inner_join(tmax_C, tmin_C) ) %>% 
  dplyr::select(Date, tmax, tmin, prec) %>%
  setNames(c('Date', 'tmax_qc', 'tmin_qc', 'prec_qc'))



sbright_C <- data_qc %>% 
  filter(Departamento == 'CESAR') %>% 
  dplyr::select(qc_climate) %>% 
  unnest() %>% 
  dplyr::select(Date, sbright) %>% 
  filter(Date > '1981-12-31' & Date <= '2015-12-31')



fill_Cesar <- full_join(sbright_C, fill_Cesar)



Cesar_todo <- data_qc %>% filter(row_number() == 2)
row_1_QC <- Cesar_todo %>% dplyr::select(qc_climate) %>% 
  unnest() %>% dplyr::select(tmax_qc, tmin_qc) %>% filter(row_number() ==1)

Cesar_New <- srad_if( exist = TRUE, data = fill_Cesar, 
                      lat = pull(Cesar_todo, lat), 
                      lon = pull(Cesar_todo, lon), 
                      alt = pull(Cesar_todo, alt)) %>% 
  dplyr::select(-sbright) %>%
  setNames(c('Date', 't_max', 't_min', 'prec', 'sol_rad')) %>%
  mutate(day = lubridate::day(Date), 
         month = lubridate::month(Date), 
         year = lubridate::year(Date) )  %>% 
  dplyr::select(-Date) %>% 
  dplyr::select(day, month, year, everything()) %>% 
  mutate(t_max = ifelse(row_number() == 1, row_1_QC$tmax_qc , t_max), 
         t_min = ifelse(row_number() == 1, row_1_QC$tmin_qc , t_min))

readr::write_csv(Cesar_New, path = 'maize/Cesar_Complete.csv')



# Creando el archivo de Chirps 
