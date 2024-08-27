#TV : investimento em propaganda
#Rádio: investimento em propaganda na rádio
#Newspaper: investimento em propaganda no jormal impresso
#Sales: milhares de unidaes vendidas de um produto

#Quero usar um modelo de regressão para vendas Sales como y e TV como x

###################
#Pacotes
library(corrplot)
library(ggplot2)
#################

#1)Matriz de correlações
M <- cor(advertising)
corrplot(M, method = 'number')

#2 análise individual para tv
ggplot(data = advertising, aes(x = TV, y = Sales)) + geom_point(size = 3) +
  xlab("Investimento em propaganda na TV") + ylab("Vendas") + theme_classic()

#Bônus: gráfico de perfis
plot(advertising)

#Análise de regressão

#1° - usando todas as variaveis

modelo_1 <- lm(formula = Sales ~., data = advertising)
summary(modelo_1)

#2° - Tirando as variaveis nao significativas

modelo_2 <- lm(formula = Sales ~TV + Radio, data = advertising)
summary(modelo_2)

#Diagnóstico dos resíduos

#1° - Verifiação da média (testar se a média é zero)

t.test(modelo_2$residuals)

#Valor-p = 1. como é maior que 0,05, nao ha evidencias para rejeitar a
#hipotese de media 0

#2° - verificação de normalidade

qqnorm(modelo_2$residuals)
qqline(modelo_2$residuals)

shapiro.test(modelo_2$residuals) #valor-p < 0,05 (há elementos para descartar a hipotese)

hist(modelo_2$residuals)

# por conta de dois outliers, o teste de shapiro rejeita normalidade

#3° - homocedasticidade

plot(modelo_2$fitted.values, modelo_2$residuals)
abline(0,0)

#verificam-se que há indícioss de homocedasticidade.