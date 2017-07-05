library(plyr)
library(party)
library(reshape2)
library(gplots)
library(gtools)
library(plyr)
require(stringi)


source("scripts/functions.R")
source("scripts/grid_RandomForest.R")
#source("scripts/gridGM.R")
source("scripts/grid_naivebayes.R")
source("scripts/gridDL.R")



#nao ler toda hora
if (!file.exists("dadosOriginalCSV")) {
  dadosOriginalCSV <- readData();

}



inputOutputType <- function()
{
  outputType <- ask("Select an option? 1 - Random Forest, 2 - GBM, 3 - Deep Learning, 4 - Naive Baiyes")
  outputType <- as.numeric(outputType)
  return (outputType)
} 

method <- inputOutputType()

cat("You chose", method)

treino <- getTreinoData(dadosOriginalCSV)
validacao <- getValidacaoData(dadosOriginalCSV)
teste <- getTesteData(dadosOriginalCSV)

treino.hex  <- as.h2o(treino) # convertendo df para formato h2o
validacao.hex  <- as.h2o(validacao) 
teste.hex  <- as.h2o(teste) 

x <- setdiff(names(dadosOriginalCSV), c(dadosOriginalCSV,"posicao", "ts_per", "eFG_per","ASTPer", "TurPer", "FTPer", "FG_per", "PirPer", "minutos_partida", "idade", "id_jogador","id_partida", "data_nascimento", "violacao", "faltas_recebidas" ))  #retirando colunas nao necessarias ao processo
y <- "posicao"


if (method == 1){
  #Random Forest  
  print(RF(h2o, treino.hex, validacao.hex, teste.hex,x, y ))
}else if (method == 2){
  # GBM
  print(GBM(treino.hex, validacao.hex, teste.hex,x, y ))
}else if (method == 3){
  # Deep Learning
  print(DL(h2o, treino.hex, validacao.hex, teste.hex,x, y ))
}else if (method == 4){
  #Naive Baiyes
  print(NB(h2o, treino.hex, validacao.hex, teste.hex,x, y ))
}


