

# EXAMEN ------------------------------------------------------------------


##A
# i LIMPIANDO DATA-----------------------------------------------------------------------

url_='https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/'

install.packages("pacman")
library(pacman)
p_load(tidyverse, janitor, jsonlite,leaflet)
data <- fromJSON(url_)
install.packages("dplyr")
library(dplyr)

ds_raw <- data$ListaEESSPrecio
locale()

ds_f <- ds_raw %>% clean_names() %>% type_convert(locale = locale(decimal_mark = ",")) %>% as_tibble() %>% glimpse()

#  ii ----------------------------------------------------------- --------
------------

ds_f %>% count(rotulo) %>% view()

ds_f %>%  distinct(rotulo) %>% view()
no_lowcost <- c('REPSOL','CEPSA','GALP','SHELL','BP','PETRONOR','AVIA','Q8','CAMPSA','BONAREA')


# CREANDO COLUMNA LOW COST ------------------------------------------------

ds_lowcost <- ds_f %>% mutate(low_cost = !rotulo %in% no_lowcost)

ds_lowcost %>% count(low_cost) %>% view()

##LA COLUMNA CLASIFICA LAS ESTACIONES POR LOWCOST = TRUE //// NO LOW COST = FALSE


# PRECIO MEDIO DE TODOS LOS GASOLEOS ------------------------------------------------------------


preciopromedio_idccaa <- ds_lowcost %>% select(idccaa,precio_biodiesel,precio_bioetanol,precio_gas_natural_comprimido,precio_gas_natural_licuado,precio_gases_licuados_del_petroleo,precio_gasoleo_a,precio_gasoleo_b,precio_gasoleo_premium,precio_gasolina_95_e10,precio_gasolina_95_e5,precio_gasolina_95_e5_premium,precio_gasolina_98_e10,precio_gasolina_98_e5,precio_hidrogeno) %>% group_by(idccaa) %>% 
 summarise(precio_biodiesel = mean(precio_biodiesel, na.rm = TRUE),
           precio_bioetanol = mean(precio_bioetanol, na.rm = TRUE),
           precio_gas_natural_comprimido = mean(precio_gas_natural_comprimido, na.rm = TRUE),
           precio_gas_natural_licuado = mean(precio_gas_natural_licuado, na.rm = TRUE),
           precio_gases_licuados_del_petroleo = mean(precio_gases_licuados_del_petroleo, na.rm = TRUE),
           precio_gasoleo_a = mean(precio_gasoleo_a, na.rm = TRUE),
           precio_gasoleo_b = mean(precio_gasoleo_b, na.rm = TRUE),
           precio_gasoleo_premium = mean(precio_gasoleo_premium, na.rm = TRUE),
           precio_gasolina_95_e10 = mean(precio_gasolina_95_e10, na.rm = TRUE),
           precio_gasolina_95_e5 = mean(precio_gasolina_95_e5, na.rm = TRUE),
           precio_gasolina_95_e5_premium = mean(precio_gasolina_95_e5_premium, na.rm = TRUE),
           precio_gasolina_98_e10 = mean(precio_gasolina_98_e10, na.rm = TRUE),
           precio_gasolina_98_e5 = mean(precio_gasolina_98_e5, na.rm = TRUE),
           precio_hidrogeno = mean(precio_hidrogeno, na.rm = TRUE)) %>% view()
  

# iii ------------------------------------------------------------- --------


# CODIGOS PARA SACAR TOP MAS CARAS ----------------------------------------

precio_descendiendo <- ds_lowcost[order(ds_lowcost$precio_gasoleo_a,decreasing = TRUE),] %>% view()
precio_desc_paramapa <- precio_descendiendo %>% select(idccaa,precio_gasoleo_a,low_cost,id_municipio,id_provincia,rotulo,provincia,latitud,longitud_wgs84,margen) %>% view()

# TOP 10 MAS CARAS ------------------------------------------------------------------

top_10 <- precio_desc_paramapa[1:10,] %>% view()


# MAPA TOP 10 -------------------------------------------------------------

top_10 %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84,lat = ~latitud)



# -------------------------------------------- ----------------------------


# CODIGOS PARA SACAR MAS BARATAS ------------------------------------------

precio_ascendiendo <- ds_lowcost[order(ds_lowcost$precio_gasoleo_a),] %>% view()
precio_asc_paramapa <- precio_ascendiendo %>% select(idccaa,precio_gasoleo_a,low_cost,id_municipio,id_provincia,rotulo,provincia,latitud,longitud_wgs84,margen) %>% view()

# TOP 20 ------------------------------------------------------------------

top_20 <- precio_asc_paramapa[1:20,] %>% view()


# MAPA TOP 20 -------------------------------------------------------------

top_20 %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84,lat = ~latitud)


#  -------------------------------------------- ---------------------------

#  B ----------------------------------------------------------- --------


# i-----------------------------------------------------------------------


# Dataset sin las grandes ciudades Madrid, Barcelona, Sevilla, y Valencia --------

Sin_GrandesCiudades <- ds_lowcost %>% filter(id_provincia == '01'| id_provincia == '02'|id_provincia == '03'|id_provincia == '01'| id_provincia == '04'| id_provincia == '05'| id_provincia == '06'| id_provincia == '07'| id_provincia == '09'| id_provincia == '10'| id_provincia == '11'| id_provincia == '12'| id_provincia == '13'| id_provincia == '14'|id_provincia == '15'| id_provincia == '16'| id_provincia == '17'|id_provincia == '18'|id_provincia == '19'|id_provincia == '20'| id_provincia == '21'| id_provincia == '22'| id_provincia == '23'| id_provincia == '24'| id_provincia == '25'| id_provincia == '26'| id_provincia == '27'| id_provincia == '29'|id_provincia == '30'| id_provincia == '31'| id_provincia == '31'| id_provincia == '32'| id_provincia == '33'| id_provincia == '34'| id_provincia == '35'| id_provincia == '36'| id_provincia == '37'| id_provincia == '38'| id_provincia == '39'| id_provincia == '40'| id_provincia == '42'| id_provincia == '43'| id_provincia == '44'| id_provincia == '45'| id_provincia == '47'| id_provincia == '48'| id_provincia == '49'| id_provincia == '50' | id_provincia == '51'| id_provincia == '52')    


# Codigos para precios promedio, minimo,y maximo sin grandes ciudades de Gasoleo A y Gasolina 95 e5 premium --------

prom_sinGrandesCiud <- Sin_GrandesCiudades %>% select(precio_gasoleo_a,precio_gasolina_95_e5_premium, id_provincia) %>% group_by(id_provincia) %>% 
  summarise(gasoleo_a = mean(precio_gasoleo_a, na.rm = TRUE),  gasolina_95= mean(precio_gasolina_95_e5_premium, na.rm = TRUE)) %>% view()

minimo_sinGrandesCiud <- Sin_GrandesCiudades %>% select(precio_gasoleo_a,precio_gasolina_95_e5_premium, id_provincia) %>% group_by(id_provincia) %>% 
  summarise(gasoleo_a = min(precio_gasoleo_a, na.rm = TRUE),  gasolina_95= min(precio_gasolina_95_e5_premium, na.rm = TRUE)) %>% view()

maximo_sinGrandesCiud <- Sin_GrandesCiudades %>% select(precio_gasoleo_a,precio_gasolina_95_e5_premium, id_provincia) %>% group_by(id_provincia) %>% 
  summarise(gasoleo_a = max(precio_gasoleo_a, na.rm = TRUE),  gasolina_95= max(precio_gasolina_95_e5_premium, na.rm = TRUE)) %>% view()


# Tabla que presenta el precio Promedio, Maximo, Y Minimo  de Gasoleo A y Gasolina 95 e5 premium sin grandes ciudades --------

PromMaxMinSinGrandesCiudades <- merge(prom_sinGrandesCiud, merge(minimo_sinGrandesCiud, maximo_sinGrandesCiud, all = TRUE), all = TRUE) %>% view()

# ii--------- -------------------------------------------------------------

# Creando formato Excel para repositorio ---------------------


write_excel_csv(PromMaxMinSinGrandesCiudades,'informe_no_grandes_ciudades_22288002')


# C------- ----------------------------------------------------------------

# i ---------- ------------------------------------------------------------

pobmun <- readxl::read_excel('pobmun21.xlsx', skip = 1) %>% view()

pob_def <- pobmun %>% select(NOMBRE,POB21) %>% clean_names() %>% view()

pobdef_municipio <- pob_def %>% rename(municipio=nombre) %>% view()


# CREANDO COLUMNA "POBLACION" ---------------------------------------------

ds_poblacion <- pobdef_municipio %>% rename(poblacion=pob21) %>% view()


# AÑADIENDO POBLACION AL DATASET ORIGINAL ---------------------------------

ds_poblowcost <- inner_join(ds_lowcost,ds_poblacion, by="municipio") %>% view()


# CON MAS DE 15.000 HABITANTES --------------------------------------------

Gasolineras_Iberica_15000Hab <- ds_poblowcost %>% filter(idccaa != '05' & idccaa != '18' & idccaa != '19' & idccaa != '04' & poblacion>15000) %>% view()

write_excel_csv(Gasolineras_Iberica_15000Hab,'Gasolineras en Peninsula Iberica con mas de 15,000 habitantes')
