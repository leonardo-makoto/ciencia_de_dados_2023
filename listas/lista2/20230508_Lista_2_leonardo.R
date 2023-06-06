# Lista 2 Morettin

# pacotes
library(ISLR2)
library(tidyverse)
library(leaps)

set.seed(9845)

# Questão 1 ----

## a ----
# criando a variável x
x <- rnorm(100)

# criando o termo de erro
e <- rnorm(100)

## b ----
# criando Y
y <- 5 + 3*x - 0.5*x^2 + x^3 + e

## c ----

# criando o modelo simples
mod_simples <- lm(y ~ x)
summary(mod_simples)

# criando o modelo quadrático
mod_quad <- lm(y ~ poly(x,2))
summary(mod_quad)

# criando o modelo cúbico
mod_cubico <- lm(y ~ poly(x,3))
summary(mod_cubico)

# modelo de ordem 4
mod_4 <- lm(y ~ poly(x,4))
summary(mod_4)

# comparando o R2 de cada modelo
summary(mod_simples)$adj.r.squared # 0.8731602
summary(mod_quad)$adj.r.squared # 0.8779173
summary(mod_cubico)$adj.r.squared # 0.9624596
summary(mod_4)$adj.r.squared # 0.9621

# comparando os BICs
BIC(mod_simples) # 413.72
BIC(mod_quad) # 413.48
BIC(mod_cubico) # 299.12
BIC(mod_4) # 303.63

# Com base nas métricas R2 e BIC, o melhor modelo considerando polinômios de
# ordem até 4 é o modelo cúbico,dado que o R2 ajustado é o maior e BIC é o menor.

## d ----
library(glmnet)

# vamos criar a base de x e y
base <- data.frame(x,y)

# Vamos criar a nossa matriz X de explicativas
X <- model.matrix(y ~ poly(x,3), base)[,-1] # removemos o coeficiente pq ele cria automaticamente.

# Ridge
reg_ridge <- cv.glmnet(X,y, alpha = 0)

# vamos ver os coeficientes e o valor de lambda
coef(reg_ridge, s = "lambda.min")
reg_ridge$lambda.min

# Lasso
reg_lasso <- cv.glmnet(X,y, alpha = 1)

# vamos ver os coeficientes e o valor de lambda
coef(reg_lasso, s = "lambda.min")
reg_lasso$lambda.min

# Questão 2 ----

# removendo os dados do exercício anterior
rm(list = ls())

# carrengando o pacote sugerido da questão:
library(astsa)

# carrengando a base de dados do exercício
data(Weekly)

## criando as estatísticas descritivas ----

descritivas <- Weekly %>%
  select(-Direction) %>%
  pivot_longer(cols = 1:ncol(.), names_to = 'variavel') %>%
  group_by(variavel) %>%
  summarise(media = mean(value),
            variancia = var(value),
            desvio_p = sd(value),
            mediana = median(value),
            prim_quartil = quantile(value, probs = 0.25),
            terc_quartil = quantile(value,probs = 0.75),
            minimo = min(value),
            maximo = max(value))

descritivas

## gráficos das estatísticas ----

week_long <- Weekly %>%
  select(-Direction) %>%
  pivot_longer(cols = 1:ncol(.), names_to = 'variavel')

# boxplot

boxplot(value ~ variavel, data = week_long %>%
          filter(variavel != "Year"))

# histograma
ggplot(week_long %>%
         filter(variavel != "Year"), aes(x = value)) + 
  geom_histogram() +
  facet_grid(variavel ~ ., scales = "free")

## b ----


# regressão logística
reg_log_1 <- glm(Direction ~ Lag1, data = Weekly, family = binomial) # 1 para Up e 0 para down

# sumário dos resultados
summary(reg_log_1)

# Resultados mostram que Lag1 não é significativo para explicar a direção
# dos retornos das ações.

## c ----

# regressão logística para 2 lags
reg_log_2 <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial) # 1 para Up e 0 para down

# sumário dos resultados
summary(reg_log_2)

# No caso com 2 Lags, a estatística Lag2 é significativa para explicar a direção
# dos retornos das ações na semana. O coeficiente demonstra que há uma relação positiva entre o percentual
# de retorno das duas semanas anteriores com a semana atual. Mais especificamente,
# Cada percentual a mais de 2 semanas atrás aumenta em 1,06 a chance da direção das ações ser Up
# esta semana.

## d ----

# separando a base de treino
Weekly_train <- Weekly %>%
  filter(Year<=2008)

# separando a base de teste
Weekly_teste <- Weekly %>%
  filter(Year > 2008)

# calibrando a função com base na amostra de treino
fit.log <- glm(Direction ~ Lag2, family = binomial, data = Weekly_train)

# vamos fazer a previsão com a amostra de teste
log.probs <- predict(fit.log, Weekly_teste, type = "response") # response é para retornar as probs, não log.

# vamos supor que se prob > 0.5, classificamos como Up
log.previsao <- rep("Down", 104) # 104 porque há 104 observações na amostra de teste

log.previsao[log.probs > 0.5] <- "Up"

# criando a tabela de confusão
table(log.previsao, Weekly_teste$Direction)

# a taxa de erro de classificação será a soma das classificações erradas sobre total:
mean(log.previsao != Weekly_teste$Direction) # 37,5%

## e ----

# carregando o pacote class para realizar KNN
library(class)

# realizando a previsão

# como os dados precisam ser imputados em matriz, precisarei converter para matrix as variáveis
knn.previsao <- knn(as.matrix(Weekly_train$Lag2), as.matrix(Weekly_teste$Lag2), Weekly_train$Direction, k = 1)

# vamos criar a matriz de confusão novamente
table(knn.previsao, Weekly_teste$Direction)

# calculando a taxa de erro
mean(knn.previsao != Weekly_teste$Direction) # 50%

## f ----

# Considerando a taxa de erro de classificação como métrica de seleção,
# a regressão logística fornece os resultados mais precisos.