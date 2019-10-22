###  Import IDEAM_data from //dapadfs
# https://github.com/jrodriguez88/Madr_semillas
# Author: Rodriguez-Espinoza J., Esquivel A.
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

# load(file = "catalog.rds")

# =----------------------------------------------
# Quaility control Jeison...
cowsay::say(what = 'Jeison QC - daily', by = 'smallcat')
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
    mutate(qc_climate = furrr::future_map(.x = data, .f = qc_weather_station)) %>%
    mutate(percent_na = furrr::future_map_dbl(.x = qc_climate,
                                              .f = miss_values, vars = c("prec_qc",
                                                                         "tmax_qc",
                                                                         "tmin_qc"),
                                              date_filter)) %>% dplyr::select(-data)
  gc()
  gc(reset = T)
  
  return(x)}


