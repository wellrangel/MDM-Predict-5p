#0.7024793
#0.7272727
#0.6744186
#0.7209302 hoje de manha

#0.7209302 sem variaveis avancadas
#0.7325581 sem parametros
#0.7906977 com search
#0.7906977 com hyper parametros
#0.7906977 com 10 a 50 arvores
#0.7906977 com 10 fold
#0.7093023 com variaveis avancadas
#0.744186 100 folds
library(h2o)
library(ggplot2)


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

x <- setdiff(names(dadosOriginalCSV), c(dadosOriginalCSV,"posicao", "minutos_partida", "pontos_time_casa","pontos_time_fora", "idade", "id_jogador","id_partida", "data_nascimento", "violacao", "faltas_recebidas" ))  #retirando colunas nao necessarias ao processo
y <- "posicao"

#"ts_per", "eFG_per","ASTPer", "TurPer", "FTPer", "FG_per", "PirPer", 
  
  
hyper_parametersGM = list( 
  ## restrict the search to the range of max_depth established above
  #max_depth = seq(minDepth,maxDepth,1),         
  
  
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = c(10,50),
  
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                                             
  
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                                         
  
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(treino))-1,1),                                 
  
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  
  ## search a few minimum required relative error improvement thresholds for a split to happen
   min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)

search_criteriaGM = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,         
  
  ## build no more than 100 models
  max_models = 100,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "AUTO",
  stopping_tolerance = 1e-3
)

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_parametersGM,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteriaGM,
  
  ## which algorithm to run
  algorithm = "gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="gbm_grid_test", 
  
  ## standard model parameters
  x=x, 
  y=y, 
  training_frame=treino.hex, 
  validation_frame=validacao.hex,
  nfolds = 20,
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  
  #learn_rate_annealing = 0.99,                                               
  
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  #max_runtime_secs = 3600,                                                 
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4,
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234                                                             
)


## Sort the grid models by AUC
sortedGrid <- h2o.getGrid("gbm_grid_test", sort_by = "accuracy", decreasing = TRUE)    
sortedGrid


glm_sorted_grid <- h2o.getGrid(grid_id = "gbm_grid_test", sort_by = "accuracy", decreasing = TRUE)


# Grab the model_id based in AUC
best_glm_model_id <- grid@model_ids[[1]]

# The best model
best_glm <- h2o.getModel(best_glm_model_id)


scoring_history <- as.data.frame(best_glm@model$scoring_history)
plot(scoring_history$number_of_trees, scoring_history$training_MSE, type="p") #training mse



my_varimpGM <- h2o.varimp(best_glm)
barplot(my_varimpGM$scaled_importance,
        names.arg = my_varimpGM$variable,
        space = 1,
        las = 2,
        main = "Variable Importance: GM GRID")

p <- ggplot(data = my_varimpGM, aes(x = variable, y =scaled_importance ))

p <- p + geom_bar(stat = "identity")
p <- p + coord_flip()
p



model = best_glm


pred2 = h2o.predict(object = model, newdata = teste.hex)


dataset_pred <- as.data.frame(pred2)


sub_reg <- data.frame(id_jogador = teste$id_jogador,  id_partida = teste$id_partida, temporada = teste$nome_temporada, posicao =  dataset_pred$predict)


final <- cbind(dataset_pred, teste ) 
#erro =  sqldf( "select * from final where predict != posicao")

perf <- h2o.performance(model, teste.hex)
h2o.confusionMatrix(perf)


# Summary of the best model
summary(pred2, exact_quantiles=TRUE)
#browser()



mean(pred2$predict==teste.hex$posicao)


h2o.shutdown(prompt=FALSE)
#return (mean(pred2$predict==teste.hex$posicao))

#}