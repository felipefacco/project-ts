### coordenadas extremas Amazonas
# -60.21250000 oeste
# -9.897784 sul
# -56.101444 leste
# 2.212526 norte
library("tidyverse")
library("skimr")

################################################################################
########################### data extraction 2015 ###############################
################################################################################

file_names_2015 <- list.files('../arquivos/OCO2_2015/', pattern = 'nc')

for(i in 1:length(file_names_2015)){
  if(i==1){
    df_2015 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2015/',file_names_2015[i]))
    if (df_2015$ndims == 0){
      
    }else{
      xco2_2015 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_2015,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_2015,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_2015,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_2015,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_2015,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_2015,varid='xco2_quality_flag')
      ) |>
        dplyr::filter(lon < -35 & lat < 5) |> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')
    }
    ncdf4::nc_close(df_2015)
  }else{
    df_a_2015 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2015/',file_names_2015[i]))
    if (df_a_2015$ndims == 0){
    }else{
      xco2_a_2015 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_a_2015,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_a_2015,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_a_2015,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_a_2015,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_a_2015,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_a_2015,varid='xco2_quality_flag')
      )|>
        dplyr::filter(lon < -35 & lat < 5)|> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')}
    ncdf4::nc_close(df_a_2015)
    xco2_2015 <- rbind(xco2_2015,xco2_a_2015)
  }
}

xco2_2015_2 <- xco2_2015 %>% 
  dplyr::filter(xco2_2015$lon > -60.21 & xco2_2015$lon  < -56.10 & xco2_2015$lat > -9.89 & xco2_2015$lat < 2.21)


xco2_date_2015 <- xco2_2015_2 |>
  dplyr::mutate(
    date = as.Date.POSIXct(time))|> 
  dplyr::mutate(mes = as.numeric(lubridate::month(date)) )|>
  dplyr::filter(
    lubridate::year(date)>2014 & lubridate::year(date)<2016
  )


xco2_date_2015 %>% 
  pull(xco2) %>% 
  skim()
  

xco2_date_2015 <- xco2_2015 |>  
  dplyr::filter(xco2_2015$lon > -60.21 & xco2_2015$lon  < -56.10 & xco2_2015$lat > -9.89 & xco2_2015$lat < 2.21) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time))|> 
  dplyr::mutate(mes = as.numeric(lubridate::month(date)) )|>
  dplyr::filter(
    lubridate::year(date)>2014 & lubridate::year(date)<2016
  )

write.csv(xco2_date_2015, file = 'xco2_2015_amazonas.csv')

write_rds(xco2_date_2015,"xco2_2015_amazonas.rds")

################################################################################
########################### data extraction 2016 ###############################
################################################################################

file_names_2016 <- list.files('../arquivos/OCO2_2016/', pattern = 'nc')

for(i in 1:length(file_names_2016)){
  if(i==1){
    df_2016 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2016/',file_names_2016[i]))
    if (df_2016$ndims == 0){
      
    }else{
      xco2_2016 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_2016,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_2016,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_2016,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_2016,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_2016,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_2016,varid='xco2_quality_flag')
      ) |>
        dplyr::filter(lon < -35 & lat < 5) |> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')
    }
    ncdf4::nc_close(df_2016)
  }else{
    df_a_2016 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2016/',file_names_2016[i]))
    if (df_a_2016$ndims == 0){
    }else{
      xco2_a_2016 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_a_2016,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_a_2016,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_a_2016,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_a_2016,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_a_2016,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_a_2016,varid='xco2_quality_flag')
      )|>
        dplyr::filter(lon < -35 & lat < 5)|> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')}
    ncdf4::nc_close(df_a_2016)
    xco2_2016 <- rbind(xco2_2016,xco2_a_2016)
  }
}

xco2_date_2016 <- xco2_2016 |>  
  dplyr::filter(xco2_2016$lon > -60.21 & xco2_2016$lon  < -56.10 & xco2_2016$lat > -9.89 & xco2_2016$lat < 2.21) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time))|> 
  dplyr::mutate(mes = as.numeric(lubridate::month(date)) )|>
  dplyr::filter(
    lubridate::year(date)>2015 & lubridate::year(date)<2017
  )


write.csv(xco2_date_2016, file = "xco2_2016_amazonas.csv" , sep = ",")


write_rds(xco2_date_2016,"xco2_2016_amazonas.rds")


################################################################################
########################### data extraction 2017 ###############################
################################################################################

file_names_2017 <- list.files('../arquivos/OCO2_2017/', pattern = 'nc')

for(i in 1:length(file_names_2017)){
  if(i==1){
    df_2017 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2017/',file_names_2017[i]))
    if (df_2017$ndims == 0){
      
    }else{
      xco2_2017 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_2017,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_2017,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_2017,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_2017,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_2017,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_2017,varid='xco2_quality_flag')
      ) |>
        dplyr::filter(lon < -35 & lat < 5) |> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')
    }
    ncdf4::nc_close(df_2017)
  }else{
    df_a_2017 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2017/',file_names_2017[i]))
    if (df_a_2017$ndims == 0){
    }else{
      xco2_a_2017 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_a_2017,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_a_2017,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_a_2017,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_a_2017,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_a_2017,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_a_2017,varid='xco2_quality_flag')
      )|>
        dplyr::filter(lon < -35 & lat < 5)|> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')}
    ncdf4::nc_close(df_a_2017)
    xco2_2017 <- rbind(xco2_2017,xco2_a_2017)
  }
}

xco2_date_2017 <- xco2_2017 |>  
  dplyr::filter(xco2_2017$lon > -60.21 & xco2_2017$lon  < -56.10 & xco2_2017$lat > -9.89 & xco2_2017$lat < 2.21) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time))|> 
  dplyr::mutate(mes = as.numeric(lubridate::month(date)) )|>
  dplyr::filter(
    lubridate::year(date)>2016 & lubridate::year(date)<2018
  )


write.csv(xco2_date_2017, file = "xco2_2017_amazonas.csv" , sep = ",")


write_rds(xco2_date_2017,"xco2_2017_amazonas.rds")


################################################################################
########################### data extraction 2018 ###############################
################################################################################

file_names_2018 <- list.files('../arquivos/OCO2_2018/', pattern = 'nc')

for(i in 1:length(file_names_2018)){
  if(i==1){
    df_2018 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2018/',file_names_2018[i]))
    if (df_2018$ndims == 0){
      
    }else{
      xco2_2018 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_2018,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_2018,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_2018,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_2018,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_2018,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_2018,varid='xco2_quality_flag')
      ) |>
        dplyr::filter(lon < -35 & lat < 5) |> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')
    }
    ncdf4::nc_close(df_2018)
  }else{
    df_a_2018 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2018/',file_names_2018[i]))
    if (df_a_2018$ndims == 0){
    }else{
      xco2_a_2018 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_a_2018,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_a_2018,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_a_2018,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_a_2018,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_a_2018,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_a_2018,varid='xco2_quality_flag')
      )|>
        dplyr::filter(lon < -35 & lat < 5)|> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')}
    ncdf4::nc_close(df_a_2018)
    xco2_2018 <- rbind(xco2_2018,xco2_a_2018)
  }
}


xco2_date_2018 <- xco2_2018 |>  
  dplyr::filter(xco2_2018$lon > -60.21 & xco2_2018$lon  < -56.10 & xco2_2018$lat > -9.89 & xco2_2018$lat < 2.21) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time))|> 
  dplyr::mutate(mes = as.numeric(lubridate::month(date)) )|>
  dplyr::filter(
    lubridate::year(date)>2017 & lubridate::year(date)<2019
  )


write.csv(xco2_date_2018, file = "xco2_2018_amazonas.csv" , sep = ",")


write_rds(xco2_date_2018,"xco2_2018_amazonas.rds")


################################################################################
########################### data extraction 2019 ###############################
################################################################################

file_names_2019 <- list.files('../arquivos/OCO2_2019/', pattern = 'nc')

for(i in 1:length(file_names_2019)){
  if(i==1){
    df_2019 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2019/',file_names_2019[i]))
    if (df_2019$ndims == 0){
      
    }else{
      xco2_2019 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_2019,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_2019,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_2019,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_2019,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_2019,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_2019,varid='xco2_quality_flag')
      ) |>
        dplyr::filter(lon < -35 & lat < 5) |> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')
    }
    ncdf4::nc_close(df_2019)
  }else{
    df_a_2019 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2019/',file_names_2019[i]))
    if (df_a_2019$ndims == 0){
    }else{
      xco2_a_2019 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_a_2019,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_a_2019,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_a_2019,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_a_2019,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_a_2019,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_a_2019,varid='xco2_quality_flag')
      )|>
        dplyr::filter(lon < -35 & lat < 5)|> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')}
    ncdf4::nc_close(df_a_2019)
    xco2_2019 <- rbind(xco2_2019,xco2_a_2019)
  }
}


xco2_date_2019 <- xco2_2019 |>  
  dplyr::filter(xco2_2019$lon > -60.21 & xco2_2019$lon  < -56.10 & xco2_2019$lat > -9.89 & xco2_2019$lat < 2.21) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time))|> 
  dplyr::mutate(mes = as.numeric(lubridate::month(date)) )|>
  dplyr::filter(
    lubridate::year(date)>2018 & lubridate::year(date)<2020
  )


write.csv(xco2_date_2019, file = "xco2_2019_amazonas.csv" , sep = ",")

write_rds(xco2_date_2019,"xco2_2019_amazonas.rds")


################################################################################
########################### data extraction 2020 ###############################
################################################################################


file_names_2020 <- list.files('../arquivos/OCO2_2020/', pattern = 'nc')

for(i in 1:length(file_names_2020)){
  if(i==1){
    df_2020 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2020/',file_names_2020[i]))
    if (df_2020$ndims == 0){
      
    }else{
      xco2_2020 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_2020,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_2020,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_2020,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_2020,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_2020,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_2020,varid='xco2_quality_flag')
      ) |>
        dplyr::filter(lon < -35 & lat < 5) |> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')
    }
    ncdf4::nc_close(df_2020)
  }else{
    df_a_2020 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2020/',file_names_2020[i]))
    if (df_a_2020$ndims == 0){
    }else{
      xco2_a_2020 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_a_2020,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_a_2020,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_a_2020,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_a_2020,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_a_2020,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_a_2020,varid='xco2_quality_flag')
      )|>
        dplyr::filter(lon < -35 & lat < 5)|> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')}
    ncdf4::nc_close(df_a_2020)
    xco2_2020 <- rbind(xco2_2020,xco2_a_2020)
  }
}

xco2_date_2020 <- xco2_2020 |>  
  dplyr::filter(xco2_2020$lon > -60.21 & xco2_2020$lon  < -56.10 & xco2_2020$lat > -9.89 & xco2_2020$lat < 2.21) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time))|> 
  dplyr::mutate(mes = as.numeric(lubridate::month(date)) )|>
  dplyr::filter(
    lubridate::year(date)>2019 & lubridate::year(date)<2021
  )


write.csv(xco2_date_2020, file = "xco2_2020_amazonas.csv" , sep = ",")

write_rds(xco2_date_2020,"xco2_2020_amazonas.rds")


################################################################################
########################### data extraction 2021 ###############################
################################################################################

file_names_2021 <- list.files('../arquivos/OCO2_2021/', pattern = 'nc')

for(i in 1:length(file_names_2021)){
  if(i==1){
    df_2021 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2021/',file_names_2021[i]))
    if (df_2021$ndims == 0){
      
    }else{
      xco2_2021 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_2021,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_2021,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_2021,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_2021,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_2021,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_2021,varid='xco2_quality_flag')
      ) |>
        dplyr::filter(lon < -35 & lat < 5) |> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')
    }
    ncdf4::nc_close(df_2021)
  }else{
    df_a_2021 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2021/',file_names_2021[i]))
    if (df_a_2021$ndims == 0){
    }else{
      xco2_a_2021 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_a_2021,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_a_2021,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_a_2021,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_a_2021,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_a_2021,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_a_2021,varid='xco2_quality_flag')
      )|>
        dplyr::filter(lon < -35 & lat < 5)|> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')}
    ncdf4::nc_close(df_a_2021)
    xco2_2021 <- rbind(xco2_2021,xco2_a_2021)
  }
}


xco2_date_2021 <- xco2_2021 |>  
  dplyr::filter(xco2_2021$lon > -60.21 & xco2_2021$lon  < -56.10 & xco2_2021$lat > -9.89 & xco2_2021$lat < 2.21) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time))|> 
  dplyr::mutate(mes = as.numeric(lubridate::month(date)) )|>
  dplyr::filter(
    lubridate::year(date)>2020 & lubridate::year(date)<2022
  )


write.csv(xco2_date_2021, file = "xco2_2021_amazonas.csv" , sep = ",")



################################################################################
########################### data extraction 2022 ###############################
################################################################################

file_names_2022 <- list.files('../arquivos/OCO2_2022/', pattern = 'nc')

for(i in 1:length(file_names_2022)){
  if(i==1){
    df_2022 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2022/',file_names_2022[i]))
    if (df_2022$ndims == 0){
      
    }else{
      xco2_2022 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_2022,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_2022,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_2022,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_2022,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_2022,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_2022,varid='xco2_quality_flag')
      ) |>
        dplyr::filter(lon < -35 & lat < 5) |> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')
    }
    ncdf4::nc_close(df_2022)
  }else{
    df_a_2022 <- ncdf4::nc_open(paste0('../arquivos/OCO2_2022/',file_names_2022[i]))
    if (df_a_2022$ndims == 0){
    }else{
      xco2_a_2022 <- data.frame(
        'lon' = ncdf4::ncvar_get(df_a_2022,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_a_2022,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_a_2022,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_a_2022,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_a_2022,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_a_2022,varid='xco2_quality_flag')
      )|>
        dplyr::filter(lon < -35 & lat < 5)|> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')}
    ncdf4::nc_close(df_a_2022)
    xco2_2022 <- rbind(xco2_2022,xco2_a_2022)
  }
}

xco2_date_2022 <- xco2_2022 |>  
  dplyr::filter(xco2_2022$lon > -60.21 & xco2_2022$lon  < -56.10 & xco2_2022$lat > -9.89 & xco2_2022$lat < 2.21) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time))|> 
  dplyr::mutate(mes = as.numeric(lubridate::month(date)) )|>
  dplyr::filter(
    lubridate::year(date)>2021 & lubridate::year(date)<2023
  )

write.csv(xco2_date_2022, file = "xco2_2022_amazonas.csv" , sep = ",")

write_rds(xco2_date_2022,"xco2_2022_amazonas.rds")









