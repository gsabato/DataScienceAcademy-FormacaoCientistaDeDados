# Configurando diretório de trabalho
setwd("E:/Dev/Source/DataScienceAcademy-FormacaoCientistaDeDados/projeto_04-AnaliseDeRiscoDeCredito")
getwd()

# Sources
source("lib/Tools.R")

# Coleta de dados 
dataSource <- read.csv("data/credit_dataset.csv", header = TRUE, sep = ",")
View(dataSource)


# Pré-processamento
## Data Exploration - Exploração dos dados
head(dataSource)
str(dataSource)

## Data Preparation - Identificação das variáveis
### Normalização das variáveis
numeric_vars <- c("credit.duration.months", "age", "credit.amount")
credit.ds <- scale.features(dataSource, numeric_vars)

categorical_vars <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation', 
                      'dependents', 'telephone', 'foreign.worker')
credit.ds <- to.factors(dataSource, categorical_vars)

str(credit.ds)

## Data Cleaning - Tratamento de valores missing, tratamento de outliers
any(is.na(credit.ds))

## Feature Engineering - Transformação e criação de variáveis 

rfe.results <- run.feature.selection(feature.vars = credit.ds[,-1], 
                                     class.var = credit.ds[,1])
rfe.results
varImp((rfe.results))

## Dividindo os dados em treino e teste - 70:30 ratio
indexes <- sample(1:nrow(credit.ds), size = 0.7 * nrow(credit.ds))
train.data <- credit.ds[indexes,]
test.data <- credit.ds[-indexes,]

### Separando em variáveis preditoras e target
test.feature.vars <- test.data[,-1]
str(test.feature.vars)
test.class.var <- test.data[,1]
str(test.class.var)


# Criação do modelo
## Treinando o modelo
?glm
model.v1 = glm(formula = "credit.rating ~ .", data = train.data, family = "binomial")
summary(model.v1)

## Testando o modelo
model.v1.predictions <- predict(model.v1, test.data, type="response")
model.v1.predictions <- round(model.v1.predictions)

## Avaliação do modelo
confusionMatrix(table(data = model.v1.predictions, reference = test.class.var), positive = '1')


# Otimização
## Revisando as variáveis preditoras
formula <- "credit.rating ~ ."
formula <- as.formula(formula)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula, data = train.data, method = "glm", trControl = control)
importance <- varImp(model, scale = FALSE)
plot(importance)

## Treinando o modelo
model.v2 = glm(formula = "credit.rating ~ account.balance + credit.purpose + previous.credit.payment.status + savings + credit.duration.months",
               data = train.data, family = "binomial")
summary(model.v2)

## Testando o modelo
model.v2.predictions <- predict(model.v2, test.data, type="response")
model.v2.predictions <- round(model.v2.predictions)

## Avaliação do modelo
confusionMatrix(table(data = model.v2.predictions, reference = test.class.var), positive = '1')


# Apresentação do Resultado
model.best <- model.v1
model.best.prediction.values <- predict(model.best, test.feature.vars, type = "response")
predictions <- prediction(model.best.prediction.values, test.class.var)
par(mfrow = c(1,2))
plot.roc.curve(predictions, title.text = "Curva ROC")
plot.pr.curve(predictions, title.text = "Curva Precision/Recall")