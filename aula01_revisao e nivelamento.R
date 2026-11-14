# Aula 01 - Revisão: regressão linear simples e principais conceitos
#
# Prof. Elson Rodrigo de Souza Santos
# E-mail: elson.rodrigo@ufabc.edu.br ou elson129@gmail.com
#
# Universidade Federal do ABC (UFABC)
# Bacharel em Ciências Econômicas
# Econometria I - 2026.3
# Disciplina: Econometria I - 2026.3
# Universidade Federal do ABC (UFABC)
#
###
# Descrição:
# O scrip é o material de suporte para a aula 01 de econometria. Contém a simulação
# de dados para oferta e demanda. Em seguida, apresentação da correlação, tabela 
# de resultados e testes.
#
###
# Nesta primeira abordagem, usamos os recursos nativos do R e do RStudio. Posteriormente,
# à medida que os modelos se tornarem mais complexos, recorreremos aos pacotes.
#
# 1. Simulação de dados
#
# Simulamos 10 mil observações para oferta e demanda, considerando a quantidade
# e preços.

# Definir tamanho da amostra
set.seed(123)
n <- 10000

# Simulação do preço em logaritmo natural 
log_p <- rnorm(n, mean = 3, sd = 0.8)

# Simulação dos erros aleatórios (média zero e variância constante)
u_d <- rnorm(n, mean = 0, sd = 0.5)
u_s <- rnorm(n, mean = 0, sd = 0.5)

# Relações populacionais log-lineares

# Demanda: intercepto = 10, elasticidade-preço = -1.2
log_qd <- 10 - 1.2 * log_p + u_d

# Oferta: intercepto = 2, elasticidade-preço = 0.8
log_qs <- 2 + 0.8 * log_p + u_s

# Organização da base em corte transversal (data frame)
dados <- data.frame(
  id = 1:n,
  log_p = log_p,
  log_qd = log_qd,
  log_qs = log_qs
)

#####
# 2. Correlação

# Coeficiente de correlação de Pearson
cor_demanda <- cor(dados$log_p, dados$log_qd)
cor_oferta <- cor(dados$log_p, dados$log_qs)

cor_demanda
cor_oferta

#  Teste de hipótese para a correlação
cor.test(dados$log_p, dados$log_qd)
cor.test(dados$log_p, dados$log_qs)

# Matriz de correlação entre as três variáveis
cor(dados[, c("log_p", "log_qd", "log_qs")])

# Configurar janela gráfica com 1 linha e 2 colunas
par(mfrow = c(1, 3))

# Distribuição da quantidade demandada (log_qd)
hist(dados$log_qd,
     probability = TRUE,
     main = "Distribuição da Demanda (log_qd)",
     xlab = "Log da Quantidade Demandada",
     ylab = "Densidade",
     col = "lightblue",
     border = "white")
lines(density(dados$log_qd), col = "darkblue", lwd = 2)

# Distribuição da quantidade ofertada (log_qs)
hist(dados$log_qs,
     probability = TRUE,
     main = "Distribuição da Oferta (log_qs)",
     xlab = "Log da Quantidade Ofertada",
     ylab = "Densidade",
     col = "lightgreen",
     border = "white")
lines(density(dados$log_qs), col = "darkgreen", lwd = 2)

# Distribuição preço log
hist(dados$log_p,
     probability = TRUE,
     main = "Distribuição do Preço (log_p)",
     xlab = "Log do Preço",
     ylab = "Densidade",
     col = "moccasin",
     border = "white")
lines(density(dados$log_p), col = "darkorange", lwd = 2)

# Restaurar o padrão de 1 gráfico por janela
par(mfrow = c(1, 1))

# Gráfico correlação preço e quantidade

# Configurar dois gráficos lado a lado (use c(1, 1) para uma janela por gráfico)
par(mfrow = c(1, 2))

# Correlação: Demanda vs. Preço (preço no eixo vertical)
r_demanda <- round(cor(dados$log_qd, dados$log_p), 4)

plot(dados$log_qd, dados$log_p,
     main = "Correlação: Demanda e Preço",
     xlab = "Log da Quantidade Demandada (log_qd)",
     ylab = "Log do Preço (log_p)",
     pch = 16,
     cex = 0.4,
     col = rgb(0, 0, 0.8, 0.15))
abline(lm(log_p ~ log_qd, data = dados), col = "black", lwd = 2)
legend("topright",
       legend = paste("r =", r_demanda),
       bty = "n",
       text.col = "darkblue",
       text.font = 2,
       cex = 1.1)

# Correlação: Oferta vs. Preço (preço no eixo vertical)
r_oferta <- round(cor(dados$log_qs, dados$log_p), 4)

plot(dados$log_qs, dados$log_p,
     main = "Correlação: Oferta e Preço",
     xlab = "Log da Quantidade Ofertada (log_qs)",
     ylab = "Log do Preço (log_p)",
     pch = 16,
     cex = 0.4,
     col = rgb(0, 0.5, 0, 0.15))
abline(lm(log_p ~ log_qs, data = dados), col = "black", lwd = 2)
legend("topleft",
       legend = paste("r =", r_oferta),
       bty = "n",
       text.col = "darkgreen",
       text.font = 2,
       cex = 1.1)

# Restaurar o padrão de 1 gráfico por janela
par(mfrow = c(1, 1))

# 3. Regressão linear simples

# Modelos:
modelo_demanda <- lm(log_qd ~ log_p, data = dados)
modelo_oferta <- lm(log_qs ~ log_p, data = dados)

# Resultados
summary(modelo_demanda)
summary(modelo_oferta)

# 4. Normalidade dos erros

# Extrair os resíduos
res_d <- residuals(modelo_demanda)
res_o <- residuals(modelo_oferta)

# Configurar janela com 1 linha e 2 colunas
par(mfrow = c(1, 2))

# Distribuição dos resíduos da Demanda
hist(res_d,
     probability = TRUE,
     main = "Resíduos: Demanda",
     xlab = "Resíduos",
     ylab = "Densidade",
     col = "lightblue",
     border = "white")
curve(dnorm(x, mean = mean(res_d), sd = sd(res_d)),
      add = TRUE, col = "red", lwd = 2)

# Distribuição dos resíduos da Oferta
hist(res_o,
     probability = TRUE,
     main = "Resíduos: Oferta",
     xlab = "Resíduos",
     ylab = "Densidade",
     col = "lightgreen",
     border = "white")
curve(dnorm(x, mean = mean(res_o), sd = sd(res_o)),
      add = TRUE, col = "red", lwd = 2)

# Restaurar janela padrão
par(mfrow = c(1, 1))

# Gráfico direto de Homoscedasticidade (Resíduos vs. Valores Ajustados)
par(mfrow = c(1, 2))

plot(modelo_demanda, which = 1, main = "Demanda",
     pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.2))

plot(modelo_oferta, which = 1, main = "Oferta",
     pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.2))

par(mfrow = c(1, 1))

# Gráficos completos

# Demanda
par(mfrow = c(2, 2))
plot(modelo_demanda, pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.2))

# oferta
par(mfrow = c(2, 2))
plot(modelo_oferta, pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.2))

# Restaurar janela padrão
par(mfrow = c(1, 1))


###
# FIM
