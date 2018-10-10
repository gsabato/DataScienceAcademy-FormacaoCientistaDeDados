#install.packages("caret")
library(caret)
#install.packages("randomForest")
library(randomForest) 
#install.packages("ROCR")
library(ROCR)

## Convertendo as variáveis para o tipo fator (categórica)
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

## Normalização
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

# Função para seleção de variáveis
run.feature.selection <- function(num.iters=20, feature.vars, class.var){
  set.seed(10)
  variable.sizes <- 1:10
  control <- rfeControl(functions = rfFuncs, method = "cv", 
                        verbose = FALSE, returnResamp = "all", 
                        number = num.iters)
  results.rfe <- rfe(x = feature.vars, y = class.var, 
                     sizes = variable.sizes, 
                     rfeControl = control)
  return(results.rfe)
}

# Geração de curvas ROC
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf, col = "black", lty = 1, lwd = 2, 
       main = title.text, cex.main = 0.6, 
       cex.lab = 0.8, xaxs="i", yaxs="i")
  abline(0,1, col = "red")
  auc <- performance(predictions, "auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4, legend = c(paste0("AUC: ",auc)), cex = 0.6, bty = "n", box.col = "white")
  
}

plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf, col = "black", lty = 1, lwd = 2,
       main = title.text, cex.main = 0.6, cex.lab = 0.8, xaxs = "i", yaxs = "i")
}