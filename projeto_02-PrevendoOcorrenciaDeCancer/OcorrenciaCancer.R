# Configurando diret�rio de trabalho
setwd("E:/Dev/Source/DataScienceAcademy-FormacaoCientistaDeDados/projeto_02-PrevendoOcorrenciaDeCancer")
getwd()

# Sources
source("lib/Tools.R")
#install.packages("class")
library(class)
#install.packages("gmodels")
library(gmodels)
#install.packages("ggplot2")
library(ggplot2)

# Coleta de dados 
data.source <- read.csv("data/bc_data.csv", stringsAsFactors = FALSE)
#View(data.source)


# Pr�-processamento
## Data Exploration - Explora��o dos dados
head(data.source)

data.df <- data.source[-1] # Exluindo a coluna ID

str(data.df)

## Data Cleaning - Tratamento de valores missing, tratamento de outliers
any(is.na(data.df))

## Data Preparation - Identifica��o das vari�veis
### Transforma��o para da vari�vel para tipo fator
table(data.df$diagnosis)
data.df$diagnosis <- factor(data.df$diagnosis, levels = c("B", "M"), labels = c("Benigno", "Maligno"))
str(data.df$diagnosis)

### Verificando a propor��o
round(prop.table(table(data.df$diagnosis)) * 100, digits = 1) 

### Normaliza��o das vari�veis
summary(data.df[c("radius_mean", "area_mean", "smoothness_mean")])
data.df_norm <- as.data.frame(scale(data.df[-1]))
summary(data.df_norm[c("radius_mean", "area_mean", "smoothness_mean")])

## Dividindo os dados em treino e teste - 70:30 ratio
indexes <- sample(1:nrow(data.df_norm), size = 0.8 * nrow(data.df_norm))
train.data <- data.df_norm[indexes,]
test.data <- data.df_norm[-indexes,]


### Criando os labels para os dados de treino e de teste
train.data.labels <- data.df[indexes, 1]
test.data.labels <- data.df[-indexes, 1]


# Cria��o do modelo v1
## Treinando o modelo
?knn
model.v1 = knn(train = train.data,
               test = test.data,
               cl = train.data.labels,
               k = 21)

## Avalia��o do modelo
CrossTable(x = test.data.labels, y = model.v1, prop.chisq = FALSE)

# Cria��o do modelo v2
## Treinando o modelo
model.v2 = knn(train = train.data,
               test = test.data,
               cl = train.data.labels,
               k = 5)

## Avalia��o do modelo
CrossTable(x = test.data.labels, y = model.v2, prop.chisq = FALSE)

# Cria��o do modelo v3
## Treinando o modelo
model.v3 = knn(train = train.data,
               test = test.data,
               cl = train.data.labels,
               k = 11)

## Avalia��o do modelo
CrossTable(x = test.data.labels, y = model.v3, prop.chisq = FALSE)

# Cria��o do modelo v4
## Treinando o modelo
model.v4 = knn(train = train.data,
               test = test.data,
               cl = train.data.labels,
               k = 15)

## Avalia��o do modelo
CrossTable(x = test.data.labels, y = model.v4, prop.chisq = FALSE)

# Cria��o do modelo v5
## Treinando o modelo
model.v5 = knn(train = train.data,
               test = test.data,
               cl = train.data.labels,
               k = 27)

## Avalia��o do modelo
CrossTable(x = test.data.labels, y = model.v5, prop.chisq = FALSE)


# Calculando a taxa de erro
prev = NULL
taxa_erro = NULL

suppressWarnings(
  for(i in 1:20){
    set.seed(101)
    prev = knn(train = train.data, test = test.data, cl = train.data.labels, k = i)
    taxa_erro[i] = mean(data.df$diagnosis != prev)
})

# Obtendo os valores de k e das taxas de erro
k.values <- 1:20
df_erro <- data.frame(taxa_erro, k.values)
df_erro

# � medida que aumentamos k, diminu�mos a taxa de erro do modelo
ggplot(df_erro, aes(x = k.values, y = taxa_erro)) + geom_point()+ geom_line(lty = "dotted", color = 'red')