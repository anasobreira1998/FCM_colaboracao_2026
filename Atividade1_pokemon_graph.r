library(tidyverse)

dados <- read.csv("Pokemon_full.csv")
dados

# Relação entre ataque especial e velocidade (por tipo de Pokemon)

### Tipos 
table(dados$type)

### GGplot
ggplot(dados, aes(x = sp.atk, y = speed, color = type)) +
  geom_point(alpha = 0.6)


##### Tipos mais comuns:::

top_types <- dados %>%
  count(type, sort = TRUE) %>%
  slice_head(n = 6)

top_types

###### Filtragem 

filtered_data <- filtered_data %>%
  mutate(type = str_to_title(type))

# ------------ GGplot Core collection

ggplot(filtered_data, aes(x = sp.atk, y = speed, color = type)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  facet_wrap(~ type) +
  labs(
    title = "Relationship Between Special Attack And Speed Among Pokemon",
    subtitle = "Comparison Among The Six Most Frequent Types In The Dataset ",
    x = "Special Attack",
    y = "Speed"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 13, face = "bold"),
    legend.position = "none"
  )
