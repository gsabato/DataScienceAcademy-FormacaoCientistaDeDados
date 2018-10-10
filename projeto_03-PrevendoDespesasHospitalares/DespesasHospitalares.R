# Configurando diretório de trabalho
setwd("E:/Dev/Source/DataScienceAcademy-FormacaoCientistaDeDados/projeto_03-PrevendoDespesasHospitalares")
getwd()

# Sources
source("lib/Tools.R")

# Coleta de dados 
data.source <- read.csv("data/despesas.csv", header = TRUE, sep = ",")
#View(data.source)


# Pré-processamento
## Data Exploration - Exploração dos dados
head(data.source)
str(data.source)

summary(data.source$gastos)

### Histograma
hist(data.source$gastos, main = 'Histograma', xlab = 'Gastos')

### Matriz de Correlação
data.numeric = data.source[c("idade", "bmi", "filhos", "gastos")]
plot.corr.matrix(data.numeric)

### Scatterplot de relacionamento entre as varáveis
plot.scatt.matrix(data.numeric)

## Data Cleaning - Tratamento de valores missing, tratamento de outliers
any(is.na(data.source))

## Dividindo os dados em treino e teste - 80:20 ratio
indexes <- sample(1:nrow(data.source), size = 0.8 * nrow(data.source))
train.data <- data.source[indexes,]
test.data <- data.source[-indexes,]

# Criação do modelo
## Treinando o modelo
model.v1 <- lm(gastos ~ ., data = train.data)
model.v1

## Testando o modelo
model.v1.predictions <- predict(model.v1, test.data, type="response")
class(model.v1.predictions)
head(model.v1.predictions)

## Avaliação do modelo
summary(modelo)


# Otimização
## Revisando as variáveis preditoras
data.source$idade2 <- data.source$idade ^ 2
data.source$bmi30 <- ifelse(data.source$bmi >= 30, 1, 0)
indexes <- sample(1:nrow(data.source), size = 0.8 * nrow(data.source))
train.data <- data.source[indexes,]
test.data <- data.source[-indexes,]

## Treinando o modelo
model.v2 <- lm(gastos ~ idade + idade2 + filhos + bmi + sexo +
                 bmi30 * fumante + regiao, data = train.data)
model.v2

## Testando o modelo
model.v2.predictions <- predict(model.v2, test.data)

## Avaliação do modelo
summary(model.v2)
