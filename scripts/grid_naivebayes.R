#Name of algorithm to use in grid search (naivebayes, randomForest, kmeans, nb, deeplearning, naivebayes, pca).
#0.558140 sem AV
#0.5581395 com AV

library(h2o)
library(ggplot2)
library(data.table)

set.seed(1234);  

source("scripts/functions.R")

h2o.init(nthreads = -1)
options(scipen=20)

#nao ler toda hora
if (!file.exists("dadosOriginalCSV")) {
  dadosOriginalCSV <- readData();
  
}


treino <- getTreinoData(dadosOriginalCSV)
validacao <- getValidacaoData(dadosOriginalCSV)
teste <- getTesteData(dadosOriginalCSV)

treino.hex  <- as.h2o(treino) # convertendo df para formato h2o
validacao.hex  <- as.h2o(validacao) 
teste.hex  <- as.h2o(teste) 

x <- setdiff(names(dadosOriginalCSV), c(dadosOriginalCSV,"posicao",  "minutos_partida", "idade", "pontos_time_casa","pontos_time_fora", "id_jogador","id_partida", "data_nascimento", "violacao", "faltas_recebidas" ))  #retirando colunas nao necessarias ao processo
y <- "posicao"

#"ts_per", "eFG_per","ASTPer", "TurPer", "FTPer", "FG_per", "PirPer",



# specify the list of paramters
hyper_parametersNB <- list(
  laplace = c(0,0.5,1,2,3,4,5),
  seed = 1234
  
)


gridNB <- h2o.grid("naivebayes", grid_id="naivebayes_grid_test", 
                   x=x, 
                   y=y, 
                   training_frame=treino.hex, 
                   validation_frame=validacao.hex,
                   nfolds = 50,
                   hyper_params = hyper_parametersNB)


nb_sorted_grid <- h2o.getGrid(grid_id = "naivebayes_grid_test", sort_by = "accuracy", decreasing = TRUE)

print(nb_sorted_grid)


# Grab the model_id based in AUC
best_nb_model_id <- gridNB@model_ids[[1]]

# The best model
best_nb <- h2o.getModel(best_nb_model_id)

h2o.confusionMatrix(best_nb, valid = TRUE, metrics = 'accuracy')



model = best_nb


pred2 = h2o.predict(object = model, newdata = teste.hex)

dataset_pred = as.data.frame(pred2)

final <- cbind(dataset_pred, teste ) 

perf <- h2o.performance(model, teste.hex)
h2o.confusionMatrix(perf)


# Summary of the best model
summary(pred2, exact_quantiles=TRUE)

mean(pred2$predict==teste.hex$posicao)

#h2o.shutdown(prompt=FALSE)