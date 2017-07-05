library(h2o)
library(ggplot2)
library(data.table)
library(plyr)
library(sqldf)


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

x <- setdiff(names(dadosOriginalCSV), c(dadosOriginalCSV,"posicao",  "ts_per", "eFG_per","ASTPer", "TurPer", "FTPer", "FG_per", "PirPer", "minutos_partida", "pontos_time_casa","pontos_time_fora", "idade", "id_jogador","id_partida", "data_nascimento", "violacao", "faltas_recebidas" ))  #retirando colunas nao necessarias ao processo
y <- "posicao"




max_depth_opts = c(3, 5, 9)
sample_rate_opts = c(0.8, 1.0)
col_sample_rate_opts = c(0.2, 0.5, 1.0)
ntrees_opts = c(10, 200)
learn_rate_opts = c(0.1, 0.01)


# specify the list of paramters

hyper_parametersRF = list(ntrees = ntrees_opts, mtries = 7, max_depth = max_depth_opts)
gridRF <- h2o.grid("randomForest", grid_id="randomForest_grid_test", 
                   x=x, 
                   y=y, 
                   training_frame=treino.hex, 
                   validation_frame=validacao.hex,
                   nfolds=30,
                   seed=1234,
                   hyper_params = hyper_parametersRF)


RF_sorted_grid <- h2o.getGrid(grid_id = "randomForest_grid_test", sort_by = "accuracy", decreasing = TRUE)

print (RF_sorted_grid)
# Grab the model_id based in AUC
best_RF_model_id <- gridRF@model_ids[[1]]

# The best model
best_RF <- h2o.getModel(best_RF_model_id)


h2o.confusionMatrix(best_RF, valid = TRUE, metrics = 'accuracy')


scoring_history <- as.data.frame(best_RF@model$scoring_history)
plot(scoring_history$number_of_trees, scoring_history$training_MSE, type="p") #training mse

## get the actual number of trees
ntrees <- best_RF@model$model_summary$number_of_trees
print(ntrees)



my_varimpRF <- h2o.varimp(best_RF)
barplot(my_varimpRF$scaled_importance,
        names.arg = my_varimpRF$variable,
        space = 1,
        las = 2,
        main = "Variable Importance: RF GRID")

p <- ggplot(data = my_varimpRF, aes(x = variable, y =scaled_importance ))

p <- p + geom_bar(stat = "identity")
p <- p + coord_flip()
p



model = best_RF


pred2 = h2o.predict(object = model, newdata = teste.hex)

dataset_pred = as.data.frame(pred2)

final <- cbind(dataset_pred, teste ) 
erro =  sqldf( "select * from final where predict != posicao")

perf <- h2o.performance(model, teste.hex)
h2o.confusionMatrix(perf)


# Summary of the best model
summary(pred2, exact_quantiles=TRUE)

mean(pred2$predict==teste.hex$posicao)


#h2o.shutdown(prompt=FALSE)