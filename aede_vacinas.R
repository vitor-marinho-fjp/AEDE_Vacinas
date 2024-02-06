install.packages(c("geobr","ggplot2", "dplyr","spdep","spatialreg", "normtest"), repos = "http://cran.us.r-project.org")

library(spatialreg)
library(geobr)
library(ggplot2)
library(dplyr)
library(readr)
library(spdep)
library(classInt)
library(tseries)
library(lmtest)
library(MASS)
library(normtest)


#Leitura do mapa de MG
muni <- read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE) %>% 
   rename(ibge7=1)


cobertura_vacinal <- read_excel("cobertura_vacinal.xlsx") %>% janitor::clean_names()

library(readxl)
poliomelite <- read_excel("poliomelite.xlsx") %>%
  pivot_longer(cols = `2010`:`2022`,
               names_to = "ano",
               values_to = "taxa_vacinacao") %>% 
  filter(ano==2022)



##juntar bases
df <- left_join(muni, poliomelite)

# Criando matriz de pesos -------------------------------------------

#Queen
queen1 <- poly2nb(df)

summary(queen1)

coords <- st_centroid(st_geometry(df),
                      of_largest_polygon = TRUE) ##salvar coordenadas centroides
plot(coords)


plot(st_geometry(db), border = 'grey')
plot(queen1, coords, add = T, col = 'green')



#  5 Análise Exploratória Espacial ----------------------------------------

# 5.1 I de Moran Global para PIB per capita


library(spdep)

# Criar uma matriz de vizinhança
mg.rook <- poly2nb(df$geom, queen = FALSE)

# Transformar a matriz em lista de pesos espaciais
rook.listw <- nb2listw(mg.rook, style = "W")

# Calcular o índice de Moran
moran_result <- moran.test(df$taxa_vacinacao, listw = rook.listw)

# Visualizar os resultados
print(moran_result)


# Calcular o índice de Moran local
moran_local <- localmoran(df$taxa_vacinacao, listw = rook.listw)

# Visualizar os resultados
print(moran_local)



# Criar mapa --------------------------------------------------------------




library(spdep)
library(dplyr)

# Criar uma matriz de vizinhança
mg.rook <- poly2nb(df$geom, queen = FALSE)

# Transformar a matriz em lista de pesos espaciais
rook.listw <- nb2listw(mg.rook, style = "W")

# Calcular o índice de Moran local
locm <- localmoran(df$taxa_vacinacao, listw = rook.listw)

# Adicionar os resultados ao seu conjunto de dados original
df$spib02 <- scale(df$taxa_vacinacao) %>% as.vector()
df$lag_spib02 <- lag.listw(rook.listw, df$spib02)

df$quad_sig <- NA
df[(df$spib02 >= 0 & df$lag_spib02 >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 1
df[(df$spib02 <= 0 & df$lag_spib02 <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 2
df[(df$spib02 >= 0 & df$lag_spib02 <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 3
df[(df$spib02 <= 0 & df$lag_spib02 >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 4
df[(locm[, 5] >= 0.05), "quad_sig"] <- 5 

# Mapeamento do LISA
breaks <- seq(1, 5, 1)
labels <- c("alto-alto", "baixo-baixo", "alto-baixo", "baixo-alto", "não signif.")

# Encontre o intervalo para cada observação
np <- findInterval(df$quad_sig, breaks)

# Estabelecendo cores para cada tipo de cluster
colors <- c("red", "blue", "lightpink", "skyblue2", "white")

# Plotagem do mapa com cores de acordo com os clusters
plot(df$geom, col = colors[np])
mtext("I de Moran local Vacinação", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")



