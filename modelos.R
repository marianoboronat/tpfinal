library(readr)
library(tidyverse)
library(scales)
library(modelr)

file <- "airbnb_limpio.csv"

# abrimos el dataset
df <- read_csv(file)


df <- df |> 
  filter(ciudad == "londres",
         precio <= 750) |>
  glimpse()

df |>
  ggplot() + 
  aes(x = dist, y = precio ) + 
  geom_violin(alpha = 0) + 
  geom_boxplot()

df |> 
  ggplot() + 
  aes( x = dist, y = precio) + 
  geom_point(alpha = 0.1)
  

mod1 <- df |> 
  lm(formula = precio ~ dist) |> 
  summary()


