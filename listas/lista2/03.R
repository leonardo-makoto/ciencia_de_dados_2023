# resposta exercício 3
# autor: Leonardo Makoto
# data: 2023/05/09


# pacotes
# install.packages("ISLR")
# install.packages("cowplot")

library(ISLR)
library(tidyverse)
library(data.table)
library(ggplot2)
library(cowplot)
library(class)
db_1 <- Auto

# 3a
# Crie uma vari´avel bin´aria, mpg1, que ´e igual a 1 se mpg for maior
# que sua mediana, e mpg1 igual a zero, se mpg for menor que sua
# mediana. (Use a fun¸c˜ao data.frame () para criar um conjunto de dados
#           contendo mpg1 e as outras vari´aveis do conjunto Auto).
str(db_1)
colnames(db_1)
db_2 <- db_1 %>% 
  mutate(
    mpg1 = case_when(
      mpg > median(mpg) ~ 1,
      mpg < median(mpg) ~ 0
      )
    )
str(db_2)
#3b 
# (b) Fa¸ca gr´aficos para investigar a associa¸c˜ao entre mpg1 e as outras
# variáeis (e.g., draftsman display, boxplots). Divida os dados em conjunto de treinamento e de teste.

plots <- list()


# lista com os nomes das variáveis
var_names <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year", "origin")

# inicialize uma lista vazia para armazenar os plots
plots <- list()

# loop através dos nomes das variáveis
for (i in 1:length(var_names)) {
  # criar o plot usando ggplot
  plot <- ggplot(db_2, aes(x = as.factor(mpg1), y = .data[[var_names[i]]])) +
    geom_boxplot() +
    xlab("mpg1") +
    ylab(var_names[i]) +
    theme_classic()
  
  # adicionar o plot à lista
  plots[[i]] <- plot
}

# exibir os plots
for (i in 1:length(plots)) {
  print(plots[[i]])
}

# exibir todos os gráficos no formato draftsman display
plot_grid(plots[[1]], plots[[2]], plots[[3]],
          plots[[4]], plots[[5]], plots[[6]],
          plots[[7]], plots[[8]],
          ncol = 3, align = "h")

# setando o seed
set.seed(123)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(db_2), replace=TRUE, prob=c(0.7,0.3))
train  <- db_2[sample, ]
test   <- db_2[!sample, ]



# 3c
# Use an´alise discriminante linear de Fisher para prever mpg1 usando
# os preditores que vocˆe acha que sejam mais associadas com ela, usando
# o item (b). Qual a taxa de erros do conjunto teste?


# para prever mpg1 com os preditores, vamos calcular o discriminante para todas 
# combinações dos preditores numéricos: cylinders, displacement, horsepower,
# weight, acceleration, year

# não usamos a variável mpg, nem origin, nem name
# mpg explica mpg1 já que uma é função da outra
# origin não se mostrou muito correlacionada com mpg1 no boxplot
# name é uma variável categórica


library(MASS)

# Lista de variáveis independentes
preditores_possiveis <- names(db_2)[2:7]

# Todas as combinações possíveis de variáveis
preditores_combinacoes <- unlist(
  lapply(
    seq_along(
      preditores_possiveis),
    function(x) combn(preditores_possiveis, x, simplify = FALSE)),
  recursive = FALSE
  )

# Função para ajustar o modelo e calcular o erro de classificação
fit_lda <- function(x) {
  formula <- as.formula(paste("mpg1 ~", paste(x, collapse = "+")))
  lda_fit <- lda(formula, data = train)
  lda_pred <- predict(lda_fit, newdata = test)
  lda_error <- mean(lda_pred$class != test$mpg1)
  return(list(x = x, lda_fit = lda_fit, lda_pred = lda_pred, lda_error = lda_error))
}

# Aplicar a função em todas as combinações possíveis de variáveis
lda_results <- lapply(preditores_combinacoes, fit_lda)

# Selecionar o modelo com o menor erro de classificação
best_lda <- lda_results[[which.min(sapply(lda_results, function(x) x$lda_error))]]

# Imprimir o modelo selecionado e a matriz de confusão
print(best_lda$lda_fit)
matriz_confusao <- table(best_lda$lda_pred$class, test$mpg1)


# calculando taxa de erro
taxa_erro <- sum(matriz_confusao[row(matriz_confusao) != col(matriz_confusao)]) / sum(matriz_confusao)

# imprimindo resultados
cat("Variáveis preditoras selecionadas: ", paste(best_lda$x, collapse = ", "), "\n")
cat("Taxa de erro: ", taxa_erro, "\n")



# 3c
# (d) Use KNN, com v´arios valores de K, e determine a taxa de erros do
# conjunto teste. Qual valor de K ´e melhor nesse caso?


# função para ajustar o modelo e calcular o erro de classificação
fit_knn <- function(x, k) {
  knn_pred <- knn(
    as.data.frame(train[, unlist(x)]), 
    as.data.frame(test[, unlist(x)]), 
                  train$mpg1, k = k
    )
  knn_error <- mean(knn_pred != test$mpg1)
  return(list(x = x, k = k, knn_pred = knn_pred, knn_error = knn_error))
}

# aplicar a função em todas as combinações possíveis de variáveis e valores de k


knn_results <- lapply((preditores_combinacoes), function(x) {
  lapply(seq(1, 11,1), function(k) {
    fit_knn(x, k)

  })
})

# selecionar o modelo com o menor erro de classificação


k_menores_erros <- lapply(knn_results, function(x) which.min(sapply(x, function(y) y$knn_error)))
previsores_menor_erro <- lapply(knn_results, function(x) min(sapply(x, function(y) y$knn_error))) %>% 
  which.min()
k_menor_erro <- k_menores_erros[[previsores_menor_erro]]

best_knn_0 <- min(unlist(lapply(knn_results, function(x) min(sapply(x, function(y) y$knn_error)))))

best_knn <- knn_results[[previsores_menor_erro]][[k_menor_erro]]$knn_error

# imprimir os resultados
cat("Variáveis preditoras selecionadas: ", paste(knn_results[[previsores_menor_erro]][[k_menor_erro]]$x, collapse = ", "), "\n")
cat("Valor de k selecionado: ", k_menor_erro, "\n")
cat("Taxa de erro: ", best_knn, "\n")


# 3e 
# Qual classificador vocˆe julga que ´e melhor?
cat("Taxa de erro do melhor modelo de knn: ", best_knn, "\n", 
    "com o valor de k selecionado: ", k_menor_erro, "\n",
    "e com as variáveis preditoras selecionadas: ", paste(knn_results[[previsores_menor_erro]][[k_menor_erro]]$x, collapse = ", "), "\n") 
cat("Taxa de erro do melhor modelo de an´alise discriminante linear de Fisher: ", 
    taxa_erro, "\n",
    "com as seguintes variáveis preditoras selecionadas: ", paste(best_lda$x, collapse = ", "), "\n")


cat("O melhor modelo nesse caso é o KNN")
