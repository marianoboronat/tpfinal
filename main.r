library(tidyverse)
library(scales)

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
# df |>
#   glimpse()


# creamos la columna precio por noche, y lo redondeamos a 1 decimal
df <- df %>%
  mutate(precio_x_noche = realSum / 2)|>
  view()

# omitimos las variables innecesarias.
df <- df |>
  select(-room_private,
         -biz,
         -room_type,
         -host_is_superhost,
         -multi,
         -attr_index,
         -rest_index,
         )|>
  view()


# oferta de casas y departamentos para rentar en airbnb en 10 ciudades europeas en dias de semana
# (año 2021)

# proporcion de la cantidad de datos por ciudad
df |> 
  group_by(ciudad) |> 
  summarize(cantidad = n()) |> 
  # ordenamos de mayor a menor
  arrange(desc(cantidad)) |> 
  mutate(proporcion = cantidad / sum(cantidad)) |> 
  ggplot() +
  aes(x = ciudad, y = proporcion, )+
  geom_col(position = "fill") +
  # geom_text(aes(label = percent(proporcion, accuracy = 0.1)),
  #             vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(),
                       expand = expansion(mult = c(0, 0.1)))


# # vemos la distribucion de precios por cantidad de personas permitidas
df |>
  filter(precio_x_noche<=1000) |>
  ggplot()+
  aes(x = factor(person_capacity), y = precio_x_noche, alpha = 0.1)+
  geom_violin()+
  labs(
    title = "distribución de precios por cantidad de personas permitidas",
    x = "capacidad de personas (cantidad)",
    y = "precio"
  )

# # vemos la distribucion de precios por ciudad
df |>
  filter(precio_x_noche<=750) |>
  ggplot()+
  # CONVERTIMOS A VARIABLE CATEGORICA "CIUDAD" 
  aes(x = factor(ciudad), y = precio_x_noche, alpha = 0.1)+
  # geom_violin()+
  geom_boxplot()+
  labs(
    title = "distribución de precios por ciudad.",
    x = "ciudad",
    y = "precio"
  )


# GRAFICAMOS LAS COORDENADAS GEOGRAFICAS
df |> 
  ggplot()+
  aes(x = lng, y= lat, colour = ciudad )+
  geom_point()+
  facet_wrap(~ciudad, scales = "free") +
  labs(title = "Ubicación de los alojamientos por ciudad",
       x = "Longitud", y = "Latitud")

# mod0 = lm(df, precio~)
# summary(mod0)
