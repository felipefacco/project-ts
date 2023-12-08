# para fazer o download fiz ano a ano, o link para os anos anteriores esta dentro de urls/old
# o script ficou salvo com a formatação do ultimo ano, para baixar os interiores necessário ajustar

filename <- dir('urls/', 
                pattern = '.txt')
urls <- read.table(paste0('urls/', 
                          filename))
n_urls <- nrow(urls)
n_split <- length(stringr::str_split(urls[1,1],"/",simplify = TRUE))

View(files)
files_nc <- stringr::str_split_fixed(urls[,1],"/",n=Inf)[,n_split]


for (i in 1:n_urls){
  repeat{
    dw <- try(download.file(urls[i,1],
        paste0('arquivos/OCO2_2022/',files_nc[i]), # ajustar pasta para diferentes anos
        method = 'wget',
        extra = c('--user=facco.felipe --password=')
                            
    )
    )
    if (!(inherits(dw,"try-error")))
      break
  }
}


