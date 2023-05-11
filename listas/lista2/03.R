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
# gerar gráfico de boxplot para a variável mpg
# 
# plots[[1]] <- ggplot(db_2, aes(x = as.factor(mpg1), y = mpg)) +
#   geom_boxplot() +
#   xlab("mpg1") +
#   ylab("mpg") +
#   theme_classic()
# plots[[1]]
# 
# 
# plots[[2]] <- ggplot(db_2, aes(x = as.factor(mpg1), y = cylinders)) +
#   geom_boxplot() +
#   xlab("mpg1") +
#   ylab("cylinders") +
#   theme_classic()
# plots[[2]]
# 
# plots[[3]] <- ggplot(db_2, aes(x = as.factor(mpg1), y = displacement)) +
#   geom_boxplot() +
#   xlab("mpg1") +
#   ylab("displacement") +
#   theme_classic()
# plots[[3]]
# 
# plots[[4]] <- ggplot(db_2, aes(x = as.factor(mpg1), y = horsepower)) +
#   geom_boxplot() +
#   xlab("mpg1") +
#   ylab("horsepower") +
#   theme_classic()
# plots[[4]]
# 
# plots[[5]] <- ggplot(db_2, aes(x = as.factor(mpg1), y = weight)) +
#   geom_boxplot() +
#   xlab("mpg1") +
#   ylab("weight") +
#   theme_classic()
# plots[[5]]
# 
# plots[[6]] <- ggplot(db_2, aes(x = as.factor(mpg1), y = acceleration)) +
#   geom_boxplot() +
#   xlab("mpg1") +
#   ylab("acceleration") +
#   theme_classic()
# plots[[6]]
# 
# plots[[7]] <- ggplot(db_2, aes(x = as.factor(mpg1), y = year)) +
#   geom_boxplot() +
#   xlab("mpg1") +
#   ylab("year") +
#   theme_classic()
# plots[[7]]
# 
# plots[[8]] <- ggplot(db_2, aes(x = as.factor(mpg1), y = origin)) +
#   geom_boxplot() +
#   xlab("mpg1") +
#   ylab("origin") +
#   theme_classic()
# plots[[8]]


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
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(db_2), replace=TRUE, prob=c(0.7,0.3))
train  <- db_2[sample, ]
test   <- db_2[!sample, ]



# 3c
# Use an´alise discriminante linear de Fisher para prever mpg1 usando
# os preditores que vocˆe acha que sejam mais associadas com ela, usando
# o item (b). Qual a taxa de erros do conjunto teste?





library(MASS)

# Lista de variáveis independentes
vars <- names(db_2)[1:7]

# Todas as combinações possíveis de variáveis
var_combinations <- unlist(lapply(seq_along(vars), function(x) combn(vars, x, simplify = FALSE)), recursive = FALSE)

# Função para ajustar o modelo e calcular o erro de classificação
fit_lda <- function(vars) {
  formula <- as.formula(paste("mpg1 ~", paste(vars, collapse = "+")))
  lda_fit <- lda(formula, data = train)
  lda_pred <- predict(lda_fit, newdata = test)
  lda_error <- mean(lda_pred$class != test$mpg1)
  return(list(vars = vars, lda_fit = lda_fit, lda_pred = lda_pred, lda_error = lda_error))
}

# Aplicar a função em todas as combinações possíveis de variáveis
lda_results <- lapply(var_combinations, fit_lda)

# Selecionar o modelo com o menor erro de classificação
best_lda <- lda_results[[which.min(sapply(lda_results, function(x) x$lda_error))]]

# Imprimir o modelo selecionado e a matriz de confusão
print(best_lda$lda_fit)
table(best_lda$lda_pred$class, test$mpg1)

