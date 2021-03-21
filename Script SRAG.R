library(readr)

dados<-NULL
i<-0
{
  start_time<-Sys.time() 
  while ( is.null(dados) ) {
    dados<-tryCatch(
      read_delim(
        paste0(
        "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD-",
        format(Sys.Date()-i,"%d-%m-%Y"),".csv"
        ), 
        ";", escape_double = FALSE, trim_ws = TRUE
        ),
        error = function(e) NULL)
    i<-i+1 } 
  end_time<-Sys.time()
}
end_time-start_timeSys.Date()-i+1

# https://opendatasus.saude.gov.br/dataset/9f76e80f-a2f1-4662-9e37-71084eae23e3/resource/b3321e55-24e9-49ab-8651-29cf5c8f3179/download/dicionario-de-dados-srag-hospitalizado-27.07.2020-final.pdf
# https://opendatasus.saude.gov.br/dataset/bd-srag-2020

library(readr)

INFLUD_08_03_2021 <- readr::read_delim("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD-15-03-2021.csv", ";", escape_double = FALSE, trim_ws = TRUE)

INFLUD_08_03_2021 <- readr::read_delim("C:/Users/Daniel/Downloads/INFLUD-15-03-2021.csv", ";", escape_double = FALSE, trim_ws = TRUE) # ou documento mais recente



colnames(INFLUD_08_03_2021)


# http://cnes2.datasus.gov.br/Mod_Ind_Tipo_Leito.asp?VEstado=33&VMun=330455&VComp=202101S


library(dplyr)
hospitais<-dplyr::distinct(INFLUD_08_03_2021,INFLUD_08_03_2021$CO_UNI_NOT,.keep_all=TRUE,n=n())[,c("ID_UNIDADE","ID_MUNICIP","SG_UF_NOT","CO_MUN_NOT","CO_UNI_NOT")]

library(stringi)
sum(max(stringi::stri_length(hospitais$CO_MUN_NOT))-stringi::stri_length(hospitais$CO_MUN_NOT))
sum(max(stringi::stri_length(hospitais$CO_UNI_NOT))-stringi::stri_length(hospitais$CO_UNI_NOT))


adicionando_zeros_CNES<-max(stringi::stri_length(hospitais$CO_UNI_NOT))-stringi::stri_length(hospitais$CO_UNI_NOT)

hospitais$CNES_ajustado<-NA


i=1
for (i in 1:nrow(hospitais)) {
  hospitais$CNES_ajustado[i] <- stringi::stri_c(c(rep(0,adicionando_zeros_CNES[i]),hospitais$CO_UNI_NOT[i]),collapse = "")
}

hospitais$IDENTIFICADOR_CNES<-NA

i=1
for (i in 1:nrow(hospitais)) {
  hospitais$IDENTIFICADOR_CNES[i] <- as.numeric(stringi::stri_c(data.frame(hospitais$CO_MUN_NOT[i],hospitais$CNES_ajustado[i]),collapse = ""))
}

hospitais$internacoes<-NA

i=1
for (i in 1:nrow(hospitais)) {
  hospitais$internacoes[i] <- length(which(hospitais$CO_UNI_NOT[i]==INFLUD_08_03_2021$CO_UNI_NOT))
}

rm(adicionando_zeros_CNES)


dplyr::arrange(hospitais,desc(internacoes))

View(dplyr::arrange(hospitais,desc(internacoes)))


#http://cnes2.datasus.gov.br/cabecalho_reduzido.asp?VCod_Unidade=2408104013484

require(lubridate)
require(rvest)
require(magrittr)
require(stringr)


i=1


  
 # url <- paste0("http://cnes2.datasus.gov.br/cabecalho_reduzido.asp?VCod_Unidade=",hospitais$IDENTIFICADOR_CNES[i])
  url <- "http://cnes2.datasus.gov.br/cabecalho_reduzido.asp?VCod_Unidade=2408104013484"
  
  pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  pagina %>%  
    html_nodes("table") %>% 
    .[[1]] %>% 
    html_table(fill=T) -> x
  tabela<-x[13:100,1:4]
  require(tidyverse)
  tabela<-tidyr::drop_na(tabela)
  tabela
  









