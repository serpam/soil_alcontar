# Antonio J. Perez-Luque (@ajpelu)
# August 2021

# This file: prepare soil data of parcelas for visualization

# Packages ----------------------------------------------------------------

library(data.table)
library(tidyverse)
library(janitor)  # additional tidy functions
library(sf)
library(here)


# Prepare soil data  for  plot -------------------------------------------------
soil <- readxl::read_excel(
  here::here("data/Resultados_Suelos_2018_2021_v2.xlsx"),
  sheet = 1) %>%
  janitor::clean_names()


# soil_ternary
soil_ternary <- soil %>%
  dplyr::select(nombre = geo_parcela_nombre,
                sand = arena,
                clay = arcilla,
                slit = limo)

ggtern(soil_ternary)


g <- ggtern(data=parcelas,
            aes(x=arena,y=arcilla,z=limo)) +
  geom_point(size=3) + # tamaño de los puntos / point size
  labs(yarrow="Clay (%)", # etiqueta de las fechas / arrow label
       zarrow="Silt (%)",
       xarrow="Sand (%)") +
  theme_showarrows() + # mostrar flechas / display arrows
  scale_colour_gradient(low = "yellow",# escala de color / color scale
                        high = "red") +
  theme_rgbw()





parcelas <- st_read(dsn = here::here("data/spatial/parcelas/GEO_PARCELAS.shp"),
                    quiet = TRUE)
parcelas <- st_transform(parcelas, crs = 4326)
parcelas <- parcelas %>%
  filter(TIPO == "QUEMA") %>%
  mutate(
    treatment_name =
      case_when(
        str_detect(NOMBRE, "AL_NP_") ~	"Autumn Burning / No Browsing",
        str_detect(NOMBRE, "AL_PR_") ~ "Spring Burning / Browsing",
        str_detect(NOMBRE, "AL_P_") ~ "Autumn Burning / Browsing"
      ),
    treatment_code =
      case_when(
        str_detect(NOMBRE, "AL_NP_") ~	"NP",
        str_detect(NOMBRE, "AL_PR_") ~ "PR",
        str_detect(NOMBRE, "AL_P_") ~ "P"
      )
  )

parcelas <- parcelas %>%
  inner_join(s, by = c("NOMBRE"="geo_parcela_nombre")) %>%
  dplyr::select(
    -TRATAMIENT, -TIPO, -nombre_zona
  )

remove(s, soil)







