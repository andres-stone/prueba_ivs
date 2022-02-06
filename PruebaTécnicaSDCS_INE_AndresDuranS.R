# Prueba técnica IVS
# Postulante: 
# Andrés Durán S. MAE
# duransalgado.andres@gmail.com

#  Seteo inicial  --------------------------------------------------------------
rm(list = ls())
memory.size(max = T)

# Nombre bases de datos
p2 <- "Prueba_SDCS_P2.xlsx"
p5 <- "Prueba_SDCS_P5.xlsx"

# Librerías 
library(tidyverse)
library(ggplot2)
library(readxl)

# Pregunta 2 -------------------------------------------------------------------
# a. Construir el índice laspeyres de ventas base promedio año 2014=100 para 
# las actividades de alquiler y arriendamiento de vehículos automotores.
base_p2 <- read_excel(paste0("inputs/",p2)) %>%
  mutate(ano = as.numeric(format(Fecha, '%Y')),
         mes = as.numeric(format(Fecha, '%m')))

prom2014 <- base_p2 %>%
  group_by(ano) %>%
  summarise(x_2016 = mean(Ventas)) %>%
  filter(ano == 2014)

base_p2 <- base_p2 %>%
  mutate(media2014 = prom2014$x_2016,
         indice_laspeyres = 100*(Ventas/media2014)) %>%
  select(-media2014)

rm(prom2014)

# b. Calcule variación en 12 meses
base_p2 <- base_p2 %>%
  mutate(il_12  = lag(indice_laspeyres,12),
         var_il = 100*((indice_laspeyres - il_12)/il_12)) %>%
  select(-il_12) %>%
  filter(ano >= 2015)

# c. Generar un gráfico con el índice y las variaciones (en un solo gráfico)
plot_b2 <- ggplot() + 
  geom_line(data = base_p2,
            aes(x = Fecha, y = var_il, group = 1
                ,color = "Variación Indice Laspeyres")) + 
  geom_histogram(data = base_p2
                 ,aes(x = Fecha, y = indice_laspeyres/5, fill = "Indice Laspeyres")
                 ,stat = "identity", alpha = .3) + 
  scale_color_manual("", values = c("Variación Indice Laspeyres" = "royalblue3")) + 
  scale_fill_manual("", values = "orangered3") + 
  scale_y_continuous(sec.axis = sec_axis(trans=~.*10, 
                                         name="Valor Índice Laspeyres")) + 
  theme_light() + 
  labs(x = NULL, 
       y = "Porcentaje (%)",
       title = "Alquiler y arriendamiento de vehículos automotores",
       subtitle = "Índice Laspeyres año 2014 = 100");plot_b2

ggsave(path = paste0("output/")
       ,device = "png", plot = plot_b2
       ,filename = paste("Alquiler y arriendamiento de vehículo",".png", sep = "")
       ,scale = 1, dpi = 300, width = 6, height = 5)

# Pregunta 5 -------------------------------------------------------------------
# a. Cargue la base de datos de la hoja 2 denominada "Base_Datos" contenida en el excel 
# "Prueba_SDCS_P5.xlsx". Asignele el nombre de "base_servicios"
base_servicios <- read_excel(paste0("inputs/",p5))

# b. Mediante una función indique el número de observaciones 
# y nombre de las variables de la base recien cargada
num <- base_servicios %>% summarise(n_obs = n_distinct(RUT_FICTICIO)) %>% print()
nom <- names(base_servicios) %>% print()

# c. Cree la variable "VENTAS2020" como la suma de las ventas de enero hasta abril
base_servicios <- base_servicios %>%
  mutate(VENTAS2020 = VENTAS_ENERO2020 + VENTAS_FEBRERO2020 + VENTAS_MARZO2020 + VENTAS_ABRIL2020)

# d.	Genere la base de datos "Actividad" como 
# la suma de las ventas de enero hasta abril por actividad economica
actividad <- base_servicios %>%
  group_by(ACT_ECONOMICA) %>%
  summarise(SUM_2020 = sum(VENTAS2020))

# e.	Mediante la utilizacion de una función creada por usted, 
# agregue una nueva variable denominada "GLOSA_ACT_ECON" que indique, 
# "Transporte y almacenamiento" para las actividades economicas 4923 y 521, 
# "Actividades de alojamiento y de servicio de comidas" para 561 y 562, 
# y "Otras actividades de servicios" para la 96
ACT_ECONOMICA  <- c(4923, 521, 561, 562, 96)
GLOSA_ACT_ECON <- c("Transporte y almacenamiento",
                    "Transporte y almacenamiento",
                    "Actividades de alojamiento y de servicio de comidas",
                    "Actividades de alojamiento y de servicio de comidas",
                    "Otras actividades de servicios")
ACT_GLOSA <- data.frame(ACT_ECONOMICA, GLOSA_ACT_ECON)

actividad <- actividad %>%
  inner_join(ACT_GLOSA, by = c("ACT_ECONOMICA"))

# f. Guarde el desarrollo en formato imagen RData con el 
# nombre de "Desarrollo_Prueba_SDCS_P5.RData".
save.image("output/PruebaTecnicaSDCS_INE_AD.RData")