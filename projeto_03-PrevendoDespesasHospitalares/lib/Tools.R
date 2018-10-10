#install.packages("corrplot")
library(corrplot)
#install.packages("psych")
library(psych)
#install.packages("ROCR")
library(ROCR)

# Plot Matriz de Correlação com Teste de Significância
plot.corr.matrix <- function(M){
  corr <- cor(M)
  p.mat <- cor.mtest(M)
  corrplot(corr, method="color", 
           type="upper", order="hclust", 
           addCoef.col = "black", 
           tl.col="black", tl.srt=45, 
           p.mat = p.mat, sig.level = 0.01, insig = "blank", 
           diag=FALSE 
  )
}

# Calcula o valor-p da matriz 
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# Plot Scatterplot Matrix
plot.scatt.matrix <- function(M) {
  pairs.panels(M)
}