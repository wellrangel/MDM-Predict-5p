
initH2o<-function()
{
 

  h2o.init()
  
  h2o.init(nthreads = -1)
  h2o.clusterInfo()
  
  options(scipen=20)
  return (h2o);
}
shutDownQuitH2o<-function(h2o)
{
  
h2o.shutdown(prompt=FALSE)

}
readData<-function()
{
  
  #lendo cvs com todos os dados
  totaldados <- read.csv("data/agregadoPorTemporadaSTATS.csv", sep=",", stringsAsFactors=FALSE)
  
  
  totaldados[is.na(totaldados)] <- 0
  totaldados <- totaldados[totaldados$id_jogador > 0,]
  
  #todos jogadores com pelo menos 10 partidas por temporada
  
  totaldados[,c("posicao")] <- as.factor(totaldados[,c("posicao")])
  
  #totaldados$posicao <- as.factor(totaldados$posicao)
  #totaldados$violacao <- as.factor(totaldados$violacao)
  #totaldados$faltas_recebidas <- as.factor(totaldados$faltas_recebidas)
  
  #totaldados$posicaoInt <- as.integer(totaldados$posicao)
  #removendo jogadores de posicao nao definida
  
  #removendo jogadores de posicao nao definida
  totaldados <- totaldados[totaldados$posicao != "NOTFOUND",]
  
  return(totaldados);
  
}

getTreinoData<-function(totaldados)
{
  #separando dados de treino com todas as temporadas exceto as duas ultimas
  treino = totaldados[totaldados$nome_temporada != "T20142015",]
  treino = treino[treino$nome_temporada != "T20152016",]

  return (treino)
}

getValidacaoData<-function(totaldados)
{
  
  #separando dados de validacao com a penultima temporada
  validacao = totaldados[totaldados$nome_temporada == "T20142015",]
  
  return (validacao)
}

getTesteData<-function(totaldados)
{
  
  #separando dados de teste com a ultima temporada
  teste = totaldados[totaldados$nome_temporada == "T20152016",]
  
  
  return (teste)
}



