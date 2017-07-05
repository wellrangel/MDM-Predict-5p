library(RCurl)
library(XML)
library(sqldf)
library(stringi)
library(qdapRegex)


atletasUnique <- read.csv("data/atletasPosicaoDistinct.csv", sep = "," ,stringsAsFactors=FALSE)
atletasJogo <- read.csv("data/bancoatletasORIGINAL.csv", sep = ";" ,stringsAsFactors=FALSE)

atletasJogo$posicao = NA
atletasJogo$altura = ""
atletasJogo$peso = ""



for(i in 1:nrow(atletasJogo)) {
  #for(i in 1:3) {
  
  print(i)
  idJogador = atletasJogo[i,"id_jogador"]
  
  if (is.na(idJogador)==0){
    #browser()
    varsql = paste("SELECT id_jogador, nome_jogador, posicao, altura, peso FROM atletasUnique where id_jogador = ", idJogador, sep="" )
    varsql = paste(varsql, " limit  1", sep="" )
    linha <- sqldf( varsql)
    
    atletasJogo[i,"posicao"] =  linha$posicao
    atletasJogo[i,"peso"] =  linha$peso
    atletasJogo[i,"altura"] =  linha$altura;
    
  }
}

write.csv(atletasJogo, file = "data/bancoatletasORIGINAL.csv")