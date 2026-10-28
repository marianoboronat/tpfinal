# oferta de casas y departamentos para rentar en airbnb en 10 ciudades europeas en dias de semana
# (año 2021)

library(readr)
library(tidyverse)
library(scales)
library(modelr)

archivos <- c(
  "amsterdam" = "data/amsterdam_weekdays.csv",
  "atenas"    = "data/athens_weekdays.csv",
  "barcelona" = "data/barcelona_weekdays.csv",
  "berlin"    = "data/berlin_weekdays.csv",
  "budapest"  = "data/budapest_weekdays.csv",
  "lisboa"    = "data/lisbon_weekdays.csv",
  "londres"   = "data/london_weekdays.csv",
  "paris"     = "data/paris_weekdays.csv",
  "roma"      = "data/rome_weekdays.csv",
  "viena"     = "data/vienna_weekdays.csv"
)

# 2. Leemos y concatenamos, indicando con '.id' que cree una columna llamada "ciudad"
df <- map_df(archivos, read_csv, .id = "ciudad")

# visualizamos el dataset
df |>
  glimpse()


# creamos la columna precio por noche, y lo redondeamos a 1 decimal
df <- df %>%
  mutate(precio_x_noche = realSum / 2,
         person_capacity = factor(person_capacity))

# omitimos las variables innecesarias.
df <- df |>
  select(-room_private,
         -biz,
         -room_type,
         -host_is_superhost,
         -multi,
         -attr_index,
         -rest_index,
         )

# renombramos las variables
df <- df %>%
  rename(precio_dos_noches = realSum,
         cuarto_compartido = room_shared,
         capacidad_personas = person_capacity,
         puntuacion_limpieza = cleanliness_rating ,
         puntuacion = guest_satisfaction_overall,
         dormitorios = bedrooms,
         indice_restaurantes = rest_index_norm,
         indice_atraccion = attr_index_norm,
         )


view(df)

write_csv(df, "airbnb_limpio.csv")

df <- read_csv("airbnb_limpio.csv")

