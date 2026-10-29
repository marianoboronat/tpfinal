library(readr)
library(tidyverse)
library(scales)
library(modelr)

file <- "airbnb_limpio.csv"

# abrimos el dataset
df <- read_csv(file)


# filtramos por los precios menores de 
df <- df |> 
  filter(precio_x_noche <= 750) 
  
glimpse(df)

# # GRAFICAMOS LAS COORDENADAS GEOGRAFICAS
df |>
  ggplot()+
  aes(x = lng, y= lat, colour = ciudad )+
  geom_point(size = 0.1)+
  facet_wrap(~ciudad, scales = "free") +
  labs(title = "Ubicación de los alojamientos por ciudad",
       x = "Longitud", y = "Latitud")

# proporcion de la cantidad de datos por ciudad
df |> 
  group_by(ciudad) |> 
  summarize(cantidad = n()) |> 
  # ordenamos de mayor a menor
  arrange(desc(cantidad)) |> 
  # agregamos la proporcion
  mutate(proporcion = cantidad / sum(cantidad)*100) |> 
  
  mutate(ciudad = reorder(ciudad, proporcion)) |>
  ggplot(aes(x = "", y = proporcion, fill = ciudad)) +
  geom_col(color = "white") +
  geom_text(aes(label = paste0(round(proporcion, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.5) +
  labs(title = "proporción de datos por ciudad",
       x = NULL, y = "proporción (%)", fill = "ciudad")


# precio
df |> 
  filter(precio_x_noche <= 600) |> 
  ggplot(aes(x = precio_x_noche,) )  + 
  geom_histogram(binwidth =  50, ) +
  # geom_density( color = ciudad) + 
  # facet_wrap(~ ciudad, scales = "free_y") + 
  labs(
    title = "Densidad de alojamientos por precio",
    subtitle = "Comparativa por ciudad",
    x = "Precio",
    y = "Densidad",
    color = "Ciudad",
    fill = "Ciudad"
  )+
  theme_minimal()

# # vemos la distribucion de precios por cantidad de personas permitidas
df |>
  # filter(precio_x_noche<=1000) |>
  ggplot()+
  aes(x = factor(capacidad_personas), y = precio_x_noche, alpha = 0.1)+
  geom_violin()+
  labs(
    title = "distribución de precios por cantidad de personas permitidas",
    x = "capacidad de personas (cantidad)",
    y = "precio (euros)"
  )


# # vemos la distribucion de precios por ciudad
df |>
  ggplot()+
  # CONVERTIMOS A VARIABLE CATEGORICA "CIUDAD" 
  aes(x = factor(ciudad), y = precio_x_noche, alpha = 0.1, colour = factor(ciudad))+
  # geom_violin()+
  geom_boxplot()+
  labs(
    title = "distribución de precios por ciudad.",
    x = "ciudad",
    y = "precio (euros)"
  )
# +geom_jitter(alpha = 0.01)

# quantile(df%precio, )

# GRAFICAMOS LOS PRECIOS CON RESPECTO A LA DISTANCIA DEL CENTRO DE LA CIUDAD
df |>
  ggplot()+
  aes(x = dist, y= precio_x_noche, colour = ciudad )+
  geom_point()+
  facet_wrap(~ciudad, scales = "free") +
  labs(title = "relacion de los precios con respecto a la distancia al centro de la ciudad.",
       x = "distancia al centro de la ciudad (km)", y = "precio (euros)")

# GRAFICAMOS LOS PRECIOS CON RESPECTO A LA DISTANCIA DE LA ESTACION DE METRO MAS CERCANA
df |>
  ggplot()+
  aes(x = metro_dist, y= precio_x_noche, colour = ciudad )+
  geom_point()+
  facet_wrap(~ciudad, scales = "free") +
  labs(title = "relacion de los precios con respecto a la distancia a la parada del metro mas cercano.",
       x = "distancia al metro (km)", y = "precio (euros)")


# verificamos la relacion entre las variables de INDEPENDIENTES.
df |> 
  # filter(ciudad == "paris") |>
  ggplot()+
  aes(x = dist, y= metro_dist, colour =  df$precio_x_noche)+
  geom_point( size = df$precio_x_noche * 0.005) + 
  facet_wrap(~ciudad, scales = "free") +
  labs(title = "relacion distancia ciudad-metro",
       x = "distancia al centro de la ciudad (km)", y = "distancia metro (km)")

mod0 <- lm(data = df , formula = df$metro_dist ~ dist + ciudad)
summary(mod0)




# primer modelo: relacion precio distancia del centro.

# modelamos la relacion
# precio_por_noche = intersepto + pendiente * distancia + ciudad

mod1 <-  lm( data = df, formula =  precio_x_noche~ dist )

summary(mod1)


