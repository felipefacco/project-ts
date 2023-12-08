

filename <- dir('urls/subset_OCO2_L2_Lite_FP_11r_20230603_175134_.txt', pattern = 'teste_download')
urls <- read.table(paste0('urls/subset_OCO2_L2_Lite_FP_11r_20230603_175134_.txt',filename))
n_urls <- nrow(urls)
n_split <- length(stringr::str_split(urls[1,1],"/",simplify = TRUE))

View(files)
files_nc <- stringr::str_split_fixed(urls[,1],"/",n=Inf)[,n_split]


for (i in 1:n_urls){
  repeat{
    dw <- try(download.file(urls[i,1],
        paste0('arquivos/OCO2_2022/',files_nc[i]),
        method = 'wget',
        extra = c('--user=facco.felipe --password=')
                            
    )
    )
    if (!(inherits(dw,"try-error")))
      break
  }
}


