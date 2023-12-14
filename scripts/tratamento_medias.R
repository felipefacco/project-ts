pacotes <- c("readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth","tidyverse", "tsibble", "fable","tsibbledata", "fpp3",
             "urca","dplyr","lubridate","zoo")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

xco2_2015_amazonas <- read.csv("data-raw/xco2_2015_amazonas.csv", sep = "," )
xco2_2016_amazonas <- read.csv("data-raw/xco2_2016_amazonas.csv", sep = "," )
xco2_2017_amazonas <- read.csv("data-raw/xco2_2017_amazonas.csv", sep = "," )
xco2_2018_amazonas <- read.csv("data-raw/xco2_2018_amazonas.csv", sep = "," )
xco2_2019_amazonas <- read.csv("data-raw/xco2_2019_amazonas.csv", sep = "," )
xco2_2020_amazonas <- read.csv("data-raw/xco2_2020_amazonas.csv", sep = "," )
xco2_2021_amazonas <- read.csv("data-raw/xco2_2021_amazonas.csv", sep = "," )
xco2_2022_amazonas <- read.csv("data-raw/xco2_2022_amazonas.csv", sep = "," )

xco2_df <- bind_rows(xco2_2015_amazonas,
                     xco2_2016_amazonas,
                     xco2_2017_amazonas,
                     xco2_2018_amazonas,
                     xco2_2019_amazonas,
                     xco2_2020_amazonas,
                     xco2_2021_amazonas,
                     xco2_2022_amazonas)

xco2_df <- bind_rows(xco2_2015_area_1,
                     xco2_2016_area_1,
                     xco2_2017_area_1,
                     xco2_2018_area_1,
                     xco2_2019_area_1,
                     xco2_2020_area_1,
                     xco2_2021_area_1,
                     xco2_2022_area_1)


summary(xco2_df)

################################################################################
######################## criando a tabela de médias mensais ####################
################################################################################
view(medias)
medias <- xco2_df %>% 
  group_by(data = yearmonth(date)) %>% 
  summarise(media=mean(xco2))

# Adicionando e interpolando Ago 2017 utilizando a função na.approx

medias_inter <- medias %>% 
#  tibble::add_row(data = yearmonth("2015-10"), media = NA,.after = 09) %>% 
  tibble::add_row(data = yearmonth("2017-08"), media = NA,.after = 31) %>% 
  mutate(media_interpolada = na.approx(media))
view(medias_inter)

# Salvando a tabela de medias interpolada

# write.csv(medias_inter, file = "data/medias.csv" , sep = ",")
# write_rds(medias_inter,"data/medias_rds")

################################################################################
########################### visualizando os dados ##############################
################################################################################

medias_inter %>% 
  ggplot(aes(
    x = data,
    y = media_interpolada,
    color = "xco2"
  ))+
  geom_line()+
  theme_bw()

medias_inter %>% 
  pull(media_interpolada) %>% 
  skimr::skim()

medias_inter %>% 
  ggplot(aes(
    y = media_interpolada,
    color = "xco2"
  ))+
  geom_boxplot()+
  theme_bw()





  

