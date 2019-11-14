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
COL_shp <-  getData('GADM', country='COL', level=1) %>% crop(extent(-78 , -71 , 3.5 , 12.5 )) %>% st_as_sf()

DPTO_shp <- COL_shp %>% filter(NAME_1 %in% departamento) %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

Mun_shp <-getData('GADM', country='COL', level=2) %>% crop(extent(-78 , -71 , 3.5 , 12.5 ))  %>% st_as_sf()

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
p <- GI %>% 
  separate(file,   into =  c('ic', 'season'), '_') %>% 
ggplot(aes(x = zone, y = goodness, fill = domain)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_grey() + 
  geom_text(aes(label=goodness), position=position_dodge(width=0.9), vjust=-0.25) + 
  facet_wrap(~season) + 
  theme_bw() + 
  labs(x = '', y = 'Goodness Index', fill = 'Domain')

ggsave('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/png/bar.png' , height = 8, width = 11, units = "in")

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


all_distance <- readr::read_csv("ganaderia/all_distance.csv") %>% dplyr::select(-NAME_2, -lon, -lat, -dist)


country_mod <- country %>% full_join(all_distance, .) %>% 
  dplyr::select(zone, id, file, Departamento, Municipio, latitud, longitud, below, normal, above) %>% 
  mutate(file = str_remove(file, 'Oct_')) %>% 
  separate(col = file, into = c('season', 'ers'), sep = '_') %>%
  dplyr::select(-ers) %>% 
  tidyr::pivot_longer(cols = below:above, names_to = "type", values_to = "prob") %>% 
  mutate(type =  case_when(type == 'below' ~ 1, type == 'normal' ~ 2, type == 'above' ~ 3)) %>% 
  unique()



# En el caso que se desee el graph completo. 

label_var <- as_labeller(c('1' = 'Below', '2' = 'Normal', '3' = 'Above'))
season_label <- as_labeller(c('Dec-Jan-Feb' = 'DEF' ,  'Mar-Apr-May' = 'MAM'))

# ...
all_data <- ggplot()  +
  geom_sf(data = Mun__filter, color = gray(.5)) + 
   geom_point(data = country_mod, aes(x = longitud, y = latitud, colour = prob, 
                                      label = id, 
                                      label2 = Departamento, 
                                      label3 = Municipio)) +
   scale_color_gradient(low = "snow", high = "red") + 
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
  facet_grid(season ~ type, labeller = labeller(season = season_label, type = label_var)) + 
  theme_bw() +
  labs(title = glue::glue('Estaciones: {nrow(country)}'), x = 'Longitud',
       y = 'Latitud', colour = 'Probability ', caption = "Fuente: IDEAM") +
  theme( panel.grid.minor = element_blank(),
         strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
         strip.text = element_text(face = "bold"))

ggsave('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/png/all.png' , height = 8, width = 11, units = "in")

all_data_html <- ggplotly(all_data)
saveWidget( all_data_html, 'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/graph_int/all.html')



# ...
# "Dec-Jan-Feb" "Mar-Apr-May"
season_mod <- filter(country_mod, season == 'Dec-Jan-Feb')

season_Graph <- ggplot()  +
  geom_sf(data = Mun__filter, color = gray(.5)) + 
  geom_point(data = season_mod, aes(x = longitud, y = latitud, colour = prob, 
                                     label = id, 
                                     label2 = Departamento, 
                                     label3 = Municipio)) +
  scale_color_gradient(low = "snow", high = "red") + 
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
  facet_wrap( ~ type, labeller = labeller(season = season_label, type = label_var)) + 
  theme_bw() +
  labs(title = glue::glue('Estaciones: {nrow(country)}'), x = 'Longitud',
       y = 'Latitud', colour = 'Probability ', caption = "Fuente: IDEAM") +
  theme( panel.grid.minor = element_blank(),
         strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
         strip.text = element_text(face = "bold"))

ggsave('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/png/season_Graph1.png' , height = 8, width = 11, units = "in")

season_Graph_html <- ggplotly(season_Graph)
saveWidget( season_Graph_html, 
            'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/graph_int/season_Graph1.html')



# =.... Ahora todos las categorías 

max_cat <- country_mod %>% 
  dplyr::select(-zone) %>% 
  nest(-id, -season, -Departamento, -Municipio) %>% 
  mutate(data_max = purrr::map(.x = data, .f = function(x){arrange(x, desc(prob)) %>% slice(1)})) %>% 
  dplyr::select(-data) %>% 
  unnest(data_max)





categories <- ggplot()  +
  geom_sf(data = Mun__filter, color = gray(.5)) + 
  geom_point(data = max_cat, aes(x = longitud, y = latitud, colour = prob, 
                                 shape = as.factor(type), 
                                    label = id, 
                                    label2 = Departamento, 
                                    label3 = Municipio)) +
  scale_color_gradient(low = "snow", high = "red") + 
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) +
  facet_wrap( ~ season, labeller = labeller(season = season_label)) +
  theme_bw() +
  labs(title = glue::glue('Estaciones: {nrow(country)}'), x = 'Longitud',
       y = 'Latitud', colour = 'Probability ', caption = "Fuente: IDEAM") +
  theme( panel.grid.minor = element_blank(),
         strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
         strip.text = element_text(face = "bold"))

ggsave('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/png/cat.png' , height = 8, width = 11, units = "in")

categories_html <- ggplotly(categories)
saveWidget( categories_html, 
            'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/graph_int/cat.html')


# =-----------------------------------------------------------------------------------------------
# =-----------------------------------------------------------------------------------------------
cowsay::say('Individual graphs.')
# Para los graphs individuales...


# ...
# "Dec-Jan-Feb" "Mar-Apr-May"
season_mod <- filter(country_mod, season == "Dec-Jan-Feb") %>% mutate(prob = round(prob, 2))

a <- ggplot()  +
  geom_sf(data = Mun__filter, color = gray(.5)) + 
  geom_point(data = filter(season_mod, type == 1), aes(x = longitud, y = latitud, colour = prob, 
                                    label = id, 
                                    label2 = Departamento, 
                                    label3 = Municipio)) +
  scale_color_gradient(low = "snow", high = "#FF0000") + 
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) + 
  theme_bw() +
  labs(title = glue::glue('Below'), x = 'Longitud',
       y = 'Latitud', colour = 'Probability ') +
  theme( panel.grid.minor = element_blank(),legend.position = 'bottom', 
         strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
         strip.text = element_text(face = "bold"))


b <- ggplot()  +
  geom_sf(data = Mun__filter, color = gray(.5)) + 
  geom_point(data = filter(season_mod, type == 2), aes(x = longitud, y = latitud, colour = prob, 
                                                       label = id, 
                                                       label2 = Departamento, 
                                                       label3 = Municipio)) +
  scale_color_gradient(low = "snow", high = "#006633") + 
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) + 
  theme_bw() +
  labs(title = glue::glue('Normal'), x = 'Longitud',
       y = 'Latitud', colour = 'Probability ') +
  theme( panel.grid.minor = element_blank(),legend.position = 'bottom', 
         strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
         strip.text = element_text(face = "bold"))


c <- ggplot()  +
  geom_sf(data = Mun__filter, color = gray(.5)) + 
  geom_point(data = filter(season_mod, type == 3), aes(x = longitud, y = latitud, colour = prob, 
                                                       label = id, 
                                                       label2 = Departamento, 
                                                       label3 = Municipio)) +
  scale_color_gradient(low = "snow", high = "#004C99") + 
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) + 
  theme_bw() +
  labs(title = glue::glue('Above'), x = 'Longitud',
       y = 'Latitud', colour = 'Probability ') +
  theme( panel.grid.minor = element_blank(), legend.position = 'bottom', 
         strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
         strip.text = element_text(face = "bold"))




arrange_g <- gridExtra::grid.arrange(a, b, c, nrow = 1)
ggsave('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/png/DEF.png' , arrange_g , height = 8, width = 11, units = "in")



a_m <- ggplotly(a)
saveWidget( a_m, 
            'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/final_graphs/DEF_below.html')
b_m <- ggplotly(b)
saveWidget( b_m, 
            'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/final_graphs/DEF_normal.html')
c_m <- ggplotly(c)
saveWidget( c_m, 
            'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/final_graphs/DEF_above.html')

# arrange_g_html <- subplot(a_m, b_m, c_m, nrows = 1, shareX = TRUE, shareY = TRUE)


# =---------------------------------------------------------
# =---------------------------------------------------------

prueba_cat <- country_mod %>% 
  dplyr::select(-zone) %>% 
  nest(-id, -season, -Departamento, -Municipio) %>% 
  mutate(data_max = purrr::map(.x = data, .f = function(x){arrange(x, desc(prob)) %>% slice(1) %>% dplyr::select(-prob)}),
         data_mod = purrr::map(.x = data, .f = function(x){x %>% dplyr::select(type, prob) %>% 
             tidyr::pivot_wider(names_from = type, values_from = prob) %>% 
             set_names(c('Below', 'Normal', 'Above'))})) %>% 
  dplyr::select(-data) %>% 
  unnest()


  
cat_f <- ggplot()  +
  geom_sf(data = Mun__filter, color = gray(.5)) + 
  geom_point(data = mutate(prueba_cat, Below = round(Below, 2), Normal = round(Normal, 2), Above = round(Above, 2)  ) , aes(x = longitud, y = latitud, colour = as.factor(type), 
                                                       label = id, 
                                                       label2 = Departamento, 
                                                       label3 = Municipio, 
                                                       label4 = Below, 
                                                       label5 = Normal,
                                                       label6 = Above)) +
  scale_colour_manual(values = c("#FF0000", "#006633", "#004C99"), labels = c('Below', 'Normal', 'Above')) + 
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) + 
  theme_bw() +
  # facet_wrap( ~ season, labeller = labeller(season = season_label))  + 
  facet_wrap( ~ season) + 
  labs(x = 'Longitud',
       y = 'Latitud', colour = 'Probability ') +
  theme( panel.grid.minor = element_blank(),
         strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
         strip.text = element_text(face = "bold"))

ggsave('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/png/cat_f.png' , height = 8, width = 11, units = "in")

cat_f_html <- ggplotly(cat_f)
saveWidget( cat_f_html, 
            'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/final_graphs/cat.html')

# =---------------------------------------------------------
# =---------------------------------------------------------


kendall_mod <- country %>% full_join(all_distance, .) %>%
  dplyr::select(zone, id, file, Departamento, Municipio, latitud, longitud, kendall) %>% 
  mutate(file = str_remove(file, 'Oct_')) %>% 
  separate(col = file, into = c('season', 'ers'), sep = '_') %>%
  dplyr::select(-ers) %>% 
  unique()

table_all <- inner_join(prueba_cat, kendall_mod)
table_all %>% write_csv('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/principal_values.csv')


# =----
AFC <- ggplot()  +
  geom_sf(data = Mun__filter, color = gray(.5)) + 
  geom_point(data = mutate(table_all, kendall =  round(kendall, 2)), aes(x = longitud, y = latitud, colour = kendall, 
                                    label = id, 
                                    label2 = Departamento, 
                                    label3 = Municipio)) +
  #  c("#FF0000", "#006633", "#004C99")
  scale_colour_gradient2(low = "#FF0000", mid = 'snow', high = "#004C99") + 
  geom_sf(data = COL_shp, fill = NA, color = gray(.5)) +
  geom_sf(data = DPTO_shp, fill = NA, color = gray(.1)) + 
  theme_bw() +
  # facet_wrap( ~ season, labeller = labeller(season = season_label))  + 
  facet_wrap( ~ season) + 
  labs(x = 'Longitud',
       y = 'Latitud', colour = 'Probability ') +
  theme( panel.grid.minor = element_blank(),
         strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
         strip.text = element_text(face = "bold"))

ggsave('D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/png/AFC.png' , height = 8, width = 11, units = "in")

AFC_html <- ggplotly(AFC)
saveWidget( AFC_html,
            'D:/OneDrive - CGIAR/Desktop/madr_2019/madr_2019/ganaderia/final_graphs/AFC_html.html')
