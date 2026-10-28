library(readr)
library(tidyverse)
library(scales)
library(modelr)

# proporcion de la cantidad de datos por ciudad
df |> 
  group_by(ciudad) |> 
  summarize(cantidad = n()) |> 
  # ordenamos de mayor a menor
  arrange(desc(cantidad)) |> 
  mutate(proporcion = cantidad / sum(cantidad)*100) |> 
  
  mutate(ciudad = reorder(ciudad, proporcion)) |>
  ggplot(aes(x = "", y = proporcion, fill = ciudad)) +
  geom_col(color = "white") +
  geom_text(aes(label = paste0(round(proporcion, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.5) +
  labs(title = "proporción de datos por ciudad",
       x = NULL, y = "proporción (%)", fill = "ciudad")+
  theme_light()


# # vemos la distribucion de precios por cantidad de personas permitidas
df |>
  filter(precio_x_noche<=1000) |>
  ggplot()+
  aes(x = factor(), y = precio_x_noche, alpha = 0.1)+
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
# +geom_jitter(alpha = 0.01)

# quantile(df%precio, )

# GRAFICAMOS LOS PRECIOS CON RESPECTO A LA DISTANCIA DEL CENTRO DE LA CIUDAD
df |> 
  
  filter(precio_x_noche<=750) |>
  ggplot()+
  aes(x = dist, y= precio_x_noche, colour = ciudad )+
  geom_point()+
  facet_wrap(~ciudad, scales = "free") +
  labs(title = "relacion de los precios con respecto a la distancia al centro de la ciudad.",
       x = "distancia al centro de la ciudad (en km)", y = "precio por noche (euros)")

# GRAFICAMOS LOS PRECIOS CON RESPECTO A LA DISTANCIA DE LA ESTACION DE METRO MAS CERCANA
df |> 
  
  filter(precio_x_noche<=750) |>
  ggplot()+
  aes(x = metro_dist, y= precio_x_noche, colour = ciudad )+
  geom_point()+
  facet_wrap(~ciudad, scales = "free") +
  labs(title = "relacion de los precios con respecto a la distancia a la parada del metro mas cercano.",
       x = "distancia al metro (en km)", y = "precio por noche (euros)")

# # GRAFICAMOS LAS COORDENADAS GEOGRAFICAS
# df |> 
#   ggplot()+
#   aes(x = lng, y= lat, colour = ciudad )+
#   geom_point(size = 0.1)+
#   facet_wrap(~ciudad, scales = "free") +
#   labs(title = "Ubicación de los alojamientos por ciudad",
#        x = "Longitud", y = "Latitud")



# primer modelo: relacion precio distancia del centro.
df <- df |> 
  # menos de 750 euros
  filter(precio_x_noche <=750)

# modelamos la relacion
# precio_por_noche = intersepto + pendiente * distancia + ciudad

mod1 <-  lm( data = df, formula =  precio_x_noche~ dist )

summary(mod1)


