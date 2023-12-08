################################################################################
######################## CRIANDO E DECOMPONDO A SERIE ##########################
################################################################################

ts_co2 <- ts(medias_inter$media_interpolada , 
             start = c(2015,1),
             end = c(2022,12), 
             frequency = 12)
plot(ts_co2)

# Decomposição pelo modelo ADITIVO

decompa = decompose(ts_co2 , type = "additive")
plot(decompa)

decompa$trend
plot(decompa$trend)

# salvando em um data.frame
adt_decompa_db <- as.data.frame(medias_inter$data)
adt_decompa_db$x <- decompa$x
adt_decompa_db$trend <- decompa$trend
adt_decompa_db$seasonal <- decompa$seasonal
adt_decompa_db$random <- decompa$random


write.csv(adt_decompa_db, file = "data/adt_decompa_db.csv")


# Decomposição pelo modelo MULTIPLICATIVO

decompm=decompose(ts_co2 ,type = "multiplicative")
plot(decompm)

# salvando em um data.frame
mtp_decompa_db <- as.data.frame(medias_inter$data)
mtp_decompa_db$x <-  decompm$x
mtp_decompa_db$trend <-  decompm$trend
mtp_decompa_db$seasonal <-  decompm$seasonal
mtp_decompa_db$random <-  decompm$random

# write.csv(mtp_decompa_db, file = "data/mtp_decompa_db.csv")


################################################################
##################  criando o modelo ets  ######################
################################################################

# dividindo o modelo para treino e teste
# para treino do modelo período de 2015 - 2021

co2_modelo_ets = window(ts_co2 ,start=c(2015,1), end=c(2021,12))

# para teste do modelo e período de 2022
co2_modelo_ets_valida = window(ts_co2 ,start=c(2022,1), end=c(2022,12))

# Criando o modelo ETS com parametros ajustados pelo algorithm(ZZZ)
modelo_ets <- ets(co2_modelo_ets, model = "ZZZ")

summary(modelo_ets)

autoplot(modelo_ets$fitted)



# criando uma base para visualizar os dados fitted 

base_fitted_ets <- medias_inter[1:84,] %>% 
  mutate(fitted_ets = modelo_ets$fitted, 
         num = 1:84)


# visualizando os fitted Values

ggplotly(
  ggplot() + 
    geom_line(aes(x = base_fitted_ets$data , y = base_fitted_ets$media_interpolada , color = "reais" )) +
    geom_line(aes(x = base_fitted_ets$data , y = base_fitted_ets$fitted_ets , color = "fitted_ets")))


# criando uma base para posterior comparação dos valores predicts


forecasts_ets <- forecast.ets(modelo_ets, h = 12)

summary(forecasts_ets)

# realizando a comparação dos dados preditos através do FORECAST

forecast::accuracy(forecasts_ets$mean, # valores preditos pelo forecast
                   medias_inter$media_interpolada[85:96]) # valores reais



# Analisando os resíduos (erros) das previsões
# Condições:
# não podem ser correlacionados; se forem correlacionados ficaram informações
# nos resíduos que deveriam estar no modelo
# devem possui média zero, caso não seja então as previsões são viesadas

autoplot(modelo_ets$residuals)

acf(modelo_ets$residuals)

# Teste de Ljung-box
# H0: os resíduos são iid (modelo não exibe falhas de ajustes)
# H1: os resíduos não são iid (modelo exibe falhas de ajustes)
# não quero rejeitar H0 (quero um pvalor grande)


Box.test(modelo_ets$residuals, lag=1,type=c("Ljung-Box"))

################################################################################
############################### MODELO ARIMA ###################################
################################################################################

# dividindo o modelo para treino e teste
# para treino do modelo período de 2015 - 2021

co2_modelo_arima = window(ts_co2 ,start=c(2015,1), end=c(2021,12))

# para teste do modelo e período de 2022
co2_modelo_arima_valida = window(ts_co2 ,start=c(2022,1), end=c(2022,12))


modelo_arima <- auto.arima(co2_modelo_arima)
summary(modelo_arima)

# criando uma base para visualizar os dados fitted 

base_fitted_arima <- medias_inter[1:84,] %>% 
  mutate(fitted_arima = modelo_arima$fitted, 
         num = 1:84)


# visualizando os fitted Values

ggplotly(
  ggplot() + 
    geom_line(aes(x = base_fitted_arima$data , y = base_fitted_arima$media_interpolada , color = "reais" )) +
    geom_line(aes(x = base_fitted_arima$data , y = base_fitted_arima$fitted_arima , color = "fitted_arima")))


# criando uma base para posterior comparação dos valores predicts

forecasts_arima <-  forecast(modelo_arima)

summary(forecasts_arima)

# realizando a comparação dos dados preditos através do FORECAST

forecast::accuracy(forecasts_arima$mean[1:12], # valores preditos pelo forecast
                   medias_inter$media_interpolada[85:96]) # valores reais

# Analisando os resíduos (erros) das previsões
# Condições:
# não podem ser correlacionados; se forem correlacionados ficaram informações
# nos resíduos que deveriam estar no modelo
# devem possui média zero, caso não seja então as previsões são viesadas

autoplot(modelo_arima$residuals)

acf(modelo_arima$residuals)

# Teste de Ljung-box
# H0: os resíduos são iid (modelo não exibe falhas de ajustes)
# H1: os resíduos não são iid (modelo exibe falhas de ajustes)
# não quero rejeitar H0 (quero um pvalor grande)


Box.test(modelo_arima$residuals, lag=1,type=c("Ljung-Box"))


# plotando os valores preditos pelos 2 modelos ETS e Arima

base_predict <- medias_inter[85:96,] %>%
  mutate(predict_ets = forecasts_ets$mean,
         predict_arima = forecasts_arima$mean[1:12])
  
  
ggplotly(
  ggplot() + 
    geom_line(aes(x = base_predict$data , y = base_predict$media_interpolada , color = "Reais" )) +
    geom_line(aes(x = base_predict$data , y = base_predict$predict_ets , color = "Predict ETS"))+
    geom_line(aes(x = base_predict$data , y = base_predict$predict_arima , color = "Predict Arima"))
    )

