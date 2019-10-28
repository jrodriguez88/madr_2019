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


# Por ahora correr asi...
source('data_dapadfs.R') # Por ahora no se debe correr directamente..
source('QC_daily.R')

# //dapadfs/data_cluster_4/observed/weather_station/col-ideam/daily-raw/Update_2019_10_07/VARIABLES HM.xlsx
new_catalog <- readxl::read_excel("//dapadfs/data_cluster_4/observed/weather_station/col-ideam/daily-raw/Update_2019_10_07/CNE_OE.xls")


# Con los nuevos path de IDEAM... 

# dpto <- c('Bogotá', 'Cundinamarca', 'Boyacá', 'Córdoba', 'Cesar', 'Antioquia')
dpto <- c('BOGOTA', 'CUNDINAMARCA', 'BOYACA', 'CORDOBA', 'CESAR', 'ANTIOQUIA')

cat <- c("Climática Ordinaria", "Climática Principal", "Pluviométrica")

status <- c("Activa", "Suspendida", "Fuera de servicio")


# Funcion
filter_ideam <- function (catalog, dpto , cat, status, min_alt = 0, max_alt = 3500) {
  
  catalog %>% 
    mutate(Departamento = toupper(stri_trans_general(DEPARTAMENTO,"Latin-ASCII")),
           Municipio = toupper(stri_trans_general(MUNICIPIO,"Latin-ASCII"))) %>%
    filter(Departamento %in% dpto,
           CATEGORIA%in% cat,
           ESTADO%in% status,
           altitud < max_alt,
           altitud > min_alt)
}



# Filter IDEAM catalog
data_filter <- filter_ideam(new_catalog, dpto, cat, status= "Activa") %>% mutate(id = CODIGO)


# Scan in data_cluster4
path <- "//dapadfs/data_cluster_4/observed/weather_station/col-ideam/daily-raw/Update_2019_10_07"


# Set variables to join
var_selec <- 'PTPG_TT_D'

# Set IDEAM id station 
ideam_id <- data_filter$id



# =---------------------------------------------------
read_new_format <- function(x){

    x <- readr::read_delim(x, "|", escape_double = FALSE, trim_ws = TRUE) %>% 
    rename('prec' = 'Valor') %>% 
    separate(Fecha, c("Date", "Hour"), sep = ' ') %>% 
    dplyr::select(-Hour) %>% 
    mutate(Date = lubridate::date(Date))
    
  return(x)}

# =---------------------------------------------------
wdata_tb <- tibble(id = list.files(path, pattern = var_selec) %>% str_remove('PTPG_TT_D@') %>% str_remove('.data'),
                   var = 'prec', 
  path = list.files(path, pattern = var_selec, full.names = 'TRUE') )%>%
  mutate(data = purrr::map(.x = path, .f = read_new_format))




# mutate(idate = map(data, ~ min(.x$Date)) %>% do.call("c", .),
#        fdate = map(data, ~ max(.x$Date)) %>% do.call("c", .),
#        years = time_length(fdate-idate, "years"),
#        na_percent = map(data, ~ pct_miss(.x %>% dplyr::select(-Date))) %>% flatten_dbl()) %>%
#   filter(years > 20)



ideam_raw <-  wdata_tb %>%
  dplyr::select(-path) %>% 
  mutate(data = purrr::map(.x = data, .f = function(x){filter(x, Date >= '1980-01-01')})) %>% 
  mutate(idate = purrr::map(data, ~ min(.x$Date)) %>% do.call("c", .), 
         fdate = map(data, ~ max(.x$Date)) %>% do.call("c", .), 
         years = time_length(fdate-idate, "years"),
         na_percent = map(data, ~ pct_miss(.x %>% dplyr::select(-Date))) %>% flatten_dbl()) %>% 
  filter(years > 20)


# ajam1 <- a %>% filter(id == 12045010) %>% dplyr::select(data) %>% unnest
# 
# ajam2 <- read_delim("//dapadfs/data_cluster_4/observed/weather_station/col-ideam/daily-raw/prec-per-station/12045010_raw_prec.txt",
#                                  "\t", escape_double = FALSE, trim_ws = TRUE) 
# filter(ajam1, Date >= '1980-01-01' & Date <= '2016-02-29') %>% nrow()
# nrow(ajam2)
# 
# t_to <- cbind(filter(ajam1, Date >= '1980-01-01' & Date <= '2016-02-29'), ajam2 %>% dplyr::select(-Date)) 
# 
# t_to %>% as_tibble() %>% ggplot(.) +  geom_line(aes(x = Date, y = prec), colour= 'red') + 
#   geom_line(aes(x = Date, y = Value), colur = 'blue')
# 
# t_to %>% as_tibble() %>% filter(is.na(Value)) %>% pull(prec) %>% unique()

# list.files('//dapadfs/data_cluster_4/observed/weather_station/col-ideam/daily-raw/prec-per-station/', pattern = '12045010', full.names = TRUE)
# write_csv("//dapadfs/data_cluster_4/observed/weather_station/col-ideam/daily-raw/prec-per-station/12045010_raw_prec.txt")





# catalog <- 
data_filter %>%
  dplyr::select(id, nombre, CATEGORIA, DEPARTAMENTO, MUNICIPIO, longitud, latitud, altitud) %>% 
  setNames(c('id', 'Nombre', 'Categoria', 'Departamento', 'Municipio','lat', 'lon', 'altitud') ) %>% 
  filter(id %in% ideam_raw$id)



# ws_selected <- 
ideam_raw %>% 
  # dplyr::select(-data) %>%
  left_join(catalog %>%
              dplyr::select(id, Nombre, Categoria, Departamento, Municipio,lat, lon))





