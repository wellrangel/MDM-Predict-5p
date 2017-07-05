#0.7089947  SEM PONTOS DO TIME - CORRETO ESSE CRITERIO
#0.7513228 com parametros
#0.7301587 sem parametros e search

#0.6046512
#0.6860465
#0.6860465
#0.6860465
#0.6744186
#0.6860465
#0.7093023
#0.627907
#0.6976744
#0.7093023
#0.7209302
#0.7674419 agora na aula




#0.744186
#0.755814 com altura nfolds = 100,
#0.709302 com altura nfolds = 10,
#0.7093023 com altura
#0.6976744 sem altura
library(h2o)
library(ggplot2)
library(plyr)

set.seed(1234);


source("scripts/functions.R")

h2o.init(nthreads = -1)
options(scipen=20)

#h2o.set.seed(1234);
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

x <- setdiff(names(dadosOriginalCSV), c(dadosOriginalCSV,"posicao", "minutos_partida", "ts_per", "eFG_per","ASTPer", "TurPer", "FTPer", "FG_per", "PirPer",  "idade","pontos_time_casa","pontos_time_fora", "id_jogador","id_partida", "data_nascimento", "violacao", "faltas_recebidas" ))  #retirando colunas nao necessarias ao processo
y <- "posicao"

#

hyper_parametersDP <- list(
     hidden=list(c(32,32,32),c(64,64)),
     input_dropout_ratio=c(0,0.05),
     rate=c(0.01,0.02)
     
)


#set search criteria
search_criteriaDP <- list(strategy = "RandomDiscrete", max_models=400)


gridDL <- h2o.grid("deeplearning", 
                   grid_id="dl_grid_test", 
                   x=x, 
                   y=y, 
                   training_frame=treino.hex, 
                   validation_frame=validacao.hex, 
                   epochs=10,
                   seed = 1234,
                   variable_importances = T,
                   reproducible = TRUE,
                   
                   #stopping_metric="misclassification",
                   #stopping_tolerance=1e-2,        ## stop when misclassification does not improve by >=1% for 2 scoring events
                   #stopping_rounds=2,
                   #score_validation_samples=10000, ## downsample validation set for faster scoring
                   #score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
                   #adaptive_rate=F,                ## manually tuned learning rate
                   #momentum_start=0.5,             ## manually tuned momentum
                   #momentum_stable=0.9, 
                   #momentum_ramp=1e7, 
                   #l1=1e-5,
                   #l2=1e-5,
                   #activation=c("Rectifier"),
                   #max_w2=10,
                   nfolds = 10,
                   hyper_params = hyper_parametersDP,
                  search_criteria = search_criteriaDP
                   )

dl_sorted_grid <- h2o.getGrid(grid_id = "dl_grid_test", sort_by = "accuracy", decreasing = TRUE)

print(dl_sorted_grid, exact_quantiles=TRUE)


# Grab the model_id based in AUC
best_dl_model_id <- gridDL@model_ids[[1]]

# The best model
best_dl <- h2o.getModel(best_dl_model_id)

print(h2o.performance(best_dl, valid=T))



my_varimpDL <- h2o.varimp(best_dl)
barplot(my_varimpDL$scaled_importance,
        names.arg = my_varimpDL$variable,
        space = 1,
        las = 2,
        main = "Variable Importance: DL GRID")

model = best_dl

# Summary of the best model
pred2 = h2o.predict(object = model, newdata = teste.hex)

dataset_pred <- as.data.frame(pred2)


sub_reg <- data.frame(id_jogador = teste$id_jogador,  id_partida = teste$id_partida, temporada = teste$nome_temporada, posicao =  dataset_pred$predict)


final <- cbind(dataset_pred, teste ) 
#erro =  sqldf( "select * from final where predict != posicao")

perf <- h2o.performance(model, teste.hex)
h2o.confusionMatrix(perf)

mean(pred2$predict==teste.hex$posicao)
#h2o.shutdown(prompt=FALSE)
