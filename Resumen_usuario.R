###  Import IDEAM_data from //dapadfs --- functions. 
# https://github.com/jrodriguez88/Madr_semillas
# Author: Rodriguez-Espinoza J., Esquivel A.
# 2019

# =----------------------------------------------
# QC... 
# =---------------------------------------------- 


###  Import IDEAM_data from //dapadfs
# https://github.com/jrodriguez88/Madr_semillas
# Author: Rodriguez-Espinoza J., Esquivel A. , Mesa J.
# 2019


### Load libraries
library(cowsay)

library(curl)  ## working with readr (its necessary to update the las version)
library(tidyverse)
library(stringi)
library(lubridate)
library(naniar)
library(sf)
library(fs)
library(glue)
library(future)
library(furrr)
library(purrr)
library(lubridate)
library(tictoc)
library(stringr)
library(rjson)
library(jsonlite)
library(rio)
library(anytime)
library(sirad)

# =----------------------------------------------
# Quaility control
# =----------------------------------------------


# Tratando de entender... esto...

# Funcion 1 QC_J.  
rep_rows <- function(x){
  
  if(x > 1){ x <- rep(1, x) }else{ x <- NA_real_}
  
  return(x)}


# Funcion 2 QC_J.  
rle_id <- function(x){
  r <- rle(x)
  
  consecutive <- tibble::tibble(rows = r$values, times = r$lengths) %>%
    mutate(seq_consecutive = purrr::map(.x = times, .f = rep_rows)) %>%
    unnest(seq_consecutive) %>%
    dplyr::pull(seq_consecutive) 
  
  return(consecutive)}

# Funcion 3_1 QC_J.  
qc_prec <- function(prec, min = 0, max = 300){
  
  prec %>%
    transmute(value_qc = case_when(value < min ~ NA_real_,
                                   value > max ~ NA_real_,
                                   TRUE ~ value)) %>%
    mutate(consecutive = rle_id(value_qc)) %>%
    mutate(consecutive = if_else(value_qc == 0, value_qc, consecutive)) %>%
    mutate(value_qc = case_when(consecutive == 1 ~ NA_real_,
                                consecutive == NA_real_ ~ value_qc,
                                TRUE ~ value_qc))  %>%
    mutate(value_qc = if_else(value_qc < 0, NA_real_, value_qc)) %>%
    dplyr::select(value_qc)
  
}

# Funcion 3_2 QC_J.  
qc_tmax <- function(tmax, min = 10, max = 50, criterio = 15){
  
  tmax %>%
    mutate(value_qc = case_when(value < min ~ NA_real_,
                                value > max ~ NA_real_,
                                TRUE ~ value),
           value_qc = case_when(abs(lag(value_qc) - value_qc) >= criterio ~ NA_real_,
                                abs(lead(value_qc) - value_qc) >= criterio ~ NA_real_, 
                                TRUE ~ value_qc)) %>%
    mutate(consecutive = rle_id(value_qc)) %>%
    mutate(value_qc = case_when(consecutive == 1 ~ NA_real_,
                                consecutive == NA_real_ ~ value_qc,
                                TRUE ~ value_qc)) %>%
    dplyr::select(value_qc)
}

# Funcion 3_3 QC_J. 
qc_tmin <- function(tmin, min = -10, max = 35, criterio = 15){
  
  tmin %>%
    transmute(value_qc = case_when(value < min ~ NA_real_,
                                   value > max ~ NA_real_,
                                   TRUE ~ value),
              value_qc = case_when(abs(lag(value_qc) - value_qc) >= criterio ~ NA_real_,
                                   abs(lead(value_qc) - value_qc) >= criterio ~ NA_real_, 
                                   TRUE ~ value_qc))  %>%
    mutate(consecutive = rle_id(value_qc)) %>%
    mutate(value_qc = case_when(consecutive == 1 ~ NA_real_,
                                consecutive == NA_real_ ~ value_qc,
                                TRUE ~ value_qc)) %>%
    dplyr::select(value_qc)
}

# Funcion 4 QC_J. 
check_variable <- function(variable, x){
  
  check <- assertthat::has_name(x, variable)  
  if(isFALSE(check)){  variable <- NA_character_ }else{  return(variable) }
}

# Funcion 5 QC_J. 
variable_qc <- function(x, variable){
  
  vars <- purrr::map_chr(.x = variable, .f = check_variable, x) %>%
    na.omit() %>%
    as.character() %>%
    rlang::syms()
  
  vars <- glue::glue('{vars}')
  return(vars)}


# Aqui debo de agregar la variable brillo solar.
# Funcion 6 QC_J. 
qc_climate <- function(x, variable){
  ## cambiar por switch luego, or case_when
  if(variable == 'prec'){
    x <- qc_prec(x)
  }
  if(variable == 'tmax'){
    x <- qc_tmax(x)
  }
  if(variable == 'tmin'){
    x <- qc_tmin(x)
  }
  
  return(x)}

# Funcion 7 QC_J. 
qc_weather_station <- function(x, variable= c("tmax", "tmin", "prec")){
  
  vars <- variable_qc(x, variable)
  
  dates <- x %>%
    dplyr::select(-c(!!vars))
  
  quality_control <- x %>%
    dplyr::select(!!vars) %>%
    gather(variable, value) %>%
    nest(value) %>% 
    mutate(qc_climate = purrr::map2(.x = data, .y = variable, .f = qc_climate)) %>%
    dplyr::select(-data) %>% 
    unnest() %>%
    group_by(variable) %>%
    dplyr::mutate(id = row_number()) %>%
    spread(variable, value_qc) %>%
    dplyr::select(-id) %>%
    rename_all(.funs = list(name = ~glue::glue({'{.}_qc'}))) %>%
    dplyr::bind_cols(dates, .)  
  
  # check if tmax > tmin 
  exist_tmax <- assertthat::has_name(quality_control, "tmax_qc") 
  exist_tmin <- assertthat::has_name(quality_control, "tmin_qc")
  
  if(exist_tmax | exist_tmin){
    
    quality_control <- quality_control %>%
      mutate(tmax_qc = case_when(tmax_qc < tmin_qc ~ NA_real_,
                                 TRUE ~ tmax_qc),
             tmin_qc = case_when(tmin_qc > tmax_qc ~ NA_real_,
                                 TRUE ~ tmin_qc)) }
  
  return(quality_control)}

# Funcion 8 QC_J. 
make_qc <- function(x, date_filter = NULL){
  
  plan(multiprocess)
  options(future.globals.maxSize= 891289600)
  
  x <- x %>%
    mutate(qc_climate = furrr::future_map(.x = data, .f = qc_weather_station)) 
  
  gc()
  gc(reset = T)
  
  return(x)}






# =----------------------------------------------
# Jeferson... radiacion solar...
cowsay::say(what = 'Jeferson... Srad - daily', by = 'smallcat')
# =----------------------------------------------


## Script to convert sunshine hours ( or temperatures) to solar radiation
# https://github.com/jrodriguez88/
# Author: Rodriguez-Espinoza J.
# 2019

# data <- read_csv("test_ideam.csv")

# Grafico exploratorio. 
# data %>% group_by(year = year(Date), month = month(Date)) %>%
#   summarise(shour = mean(sbright, na.rm = T)) %>% ungroup() %>%
#   ggplot(aes(factor(month), shour)) + geom_boxplot() + theme_light()

# data must have "Date" and 'sbright' (or 'tmax' and 'tmin') variable,
## A & B parameters from FAO, 
#Frère M, Popov GF. 1979. Agrometeorological crop monitoring and forecasting. Plant
#Production Protection Paper 17. Rome: Food and Agricultural Organization.
#64 p.
# kRs adjustment coefficient (0.16.. 0.19) -- for interior (kRs = 0.16) and coastal (kRs = 0.19) regions
# Ese ultimo parametro toca machetearlo...para colombia toca poner un kRs mas bajo que 0.145 para interior y 0.17 para costa
# Concluyo eso despues de probar varias localidades, para huila usar 0.145 o 0.15

get_srad_ideam <- function(data, lat, lon, alt, A = 0.29, B = 0.45, kRs = 0.175){
  
  stopifnot(require(sirad))
  
  if(!"sbright" %in% colnames(data))
  {
    data <- mutate(data, sbright = NA_real_)   #Aqui crea la variable brillo por si no existe
  }
  
  step1 <- data %>% 
    mutate(
      extraT = extrat(lubridate::yday(Date), radians(lat))$ExtraTerrestrialSolarRadiationDaily, # Calcula la radiacion extraterrestre
      srad = ap(Date, lat = lat, lon = lon,    # aqui aplica Angstrom-Prescott
                extraT, A, B, sbright),
      srad = if_else(is.na(srad), kRs*sqrt(tmax_qc - tmin_qc)*extraT, srad))  # Aqui aplica Hargreaves
  
  max_srad <- mean(step1$extraT)*0.80     # calcula el maximo teorico de radiacion
  
  step2 <- step1 %>%    ## aca no me acuerdo, como que macheteo con la mediana
    mutate(
      srad = if_else(srad>max_srad|srad<0|is.na(srad),  median(step1$srad, na.rm = T), srad)) %>%
    dplyr::pull(srad)
  
  return(step2)   # retorna radiacion en MJ/m2*dia
  
  
}





# =----------------------------------------------------------------------------
# Functions read...
# =----------------------------------------------------------------------------



summary_mod <- function(g1){
  # g1 <- x %>% unnest()
  tibble( idate = min(g1$Date),
          fdate = max(g1$Date) ,
          years = time_length(fdate-idate, "years"),
          na_prec = ifelse(is.null(g1$prec) == FALSE, sum(is.na(g1$prec)) / nrow(g1) * 100, NA_real_) , 
          na_tmax = ifelse(is.null(g1$tmax) == FALSE, sum(is.na(g1$tmax)) / nrow(g1) * 100, NA_real_), 
          na_tmin = ifelse(is.null(g1$tmin) == FALSE, sum(is.na(g1$tmin)) / nrow(g1) * 100, NA_real_), 
          na_sbright = ifelse(is.null(g1$sbright) == FALSE, sum(is.na(g1$sbright)) / nrow(g1) * 100, NA_real_))
}


# =----------------------------------------------------------------------------
# Lectura de datos...
# =----------------------------------------------------------------------------

cowsay::say('Lectura de datos de estaciones', 'owl')

# Cambiar 
path_raw <- 'maize/Capacitacion/Estaciones/'

catalog_filter <- glue::glue('{path_raw}catalog_filter.csv') %>% readr::read_csv()



# read_data

Data_Complete <- tibble(id = list.files(path_raw) %>% str_remove('.csv') %>% as.numeric(),
       path = list.files(path_raw, full.names = TRUE) )  %>%
  mutate(data = purrr::map(.x = path, .f = readr::read_csv)) %>% 
  dplyr::select(-path)  %>% 
  inner_join(catalog_filter) %>%
  dplyr::select(id, data, Nombre, Departamento, Municipio, Altitud, lon, lat) %>%
  mutate(summary_data = purrr::map(.x = data, .f = summary_mod)) 



statios_20more <- Data_Complete %>% 
  dplyr::select(-data) %>% 
  unnest(summary_data) %>% 
  filter(years > 20)


id_station20 <- statios_20more %>% pull(id)


summary_data <- Data_Complete %>% 
  dplyr::select(id, summary_data) %>% 
  unnest(summary_data) %>% 
  filter(id %in% id_station20)


Data_Complete <- Data_Complete %>% 
  dplyr::select(-summary_data) %>% 
  inner_join(summary_data, .)



# Make_qc 
data_qc_ <- dplyr::select(Data_Complete, id, data) %>% 
  make_qc(.)

# Guardando las estaciones individuales con el QC. 
path_save <- 'maize/Capacitacion/datos_ind/'

purrr::walk2(.x = data_qc_$qc_climate, .y = data_qc_$id, .f = function(x,y, path_save){readr::write_csv(x = x, path = glue::glue('{path_save}{y}_qc.csv'))}, path_save = path_save)


# Ahora por variable... 

path_by_Var <- 'maize/Capacitacion/var/'




