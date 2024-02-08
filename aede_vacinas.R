# Instalar e carregar pacotes
install.packages(c("geobr", "ggplot2", "dplyr", "spdep", "normtest"))
library(geobr)
library(ggplot2)
library(dplyr)
library(spdep)
library(normtest)

# Leitura do mapa de MG
muni <- read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE) %>%
  rename(ibge7 = 1)

# Leitura dos dados
cobertura_vacinal <- read_excel("cobertura_vacinal.xlsx") %>%
  janitor::clean_names()

poliomelite <- read_excel("poliomelite.xlsx") %>%
  pivot_longer(cols = `2010`:`2022`,
               names_to = "ano",
               values_to = "taxa_vacinacao") %>%
  filter(ano == 2022)

# Juntar bases de dados
df <- left_join(muni, cobertura_vacinal)

# Criar matriz de vizinhança 
mg.rook <- poly2nb(df$geom, queen = FALSE)
rook.listw <- nb2listw(mg.rook, style = "W")

# Calcular índice de Moran local
locm <- localmoran(df$cobertura_vacinal_de_triplice_viral_da_populacao_de_1_ano_de_idade, listw = rook.listw)

#indice
moran.test(x = df$cobertura_vacinal_de_triplice_viral_da_populacao_de_1_ano_de_idade, listw = rook.listw)

# Adicionar resultados ao conjunto de dados original
df$spvacinas <- scale(df$cobertura_vacinal_de_triplice_viral_da_populacao_de_1_ano_de_idade) %>%
  as.vector()
df$lag_spib02 <- lag.listw(rook.listw, df$spvacinas)

# Mapeamento do LISA
breaks <- seq(1, 5, 1)
labels <- c("alto-alto", "baixo-baixo", "alto-baixo", "baixo-alto", "não signif.")
np <- findInterval(df$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "white")

# Ajustar np para ter o mesmo comprimento que o número de observações em df
np <- rep(colors, length.out = nrow(df))

# Converter o conjunto de dados para um objeto sf
sf_df <- st_as_sf(df)

# Adicionar as cores correspondentes à geometria
sf_df$colors <- np

# Plotagem 

ggplot(sf_df) +
  geom_sf(aes(fill = colors)) +
  scale_fill_manual(values = colors, name = "Cluster", labels = labels) +
  theme_void() +
  labs(title = "I de Moran local Vacinação") +
  theme(legend.position = "top")



glimpse(df)


library(tidyverse)






# Criar uma tabela de frequência dos tipos de cluster
cluster_freq <- table(df$quad_sig)

# Renomear os níveis para tornar mais legível
names(cluster_freq) <- labels

# Mostrar a tabela de frequência
cluster_freq





