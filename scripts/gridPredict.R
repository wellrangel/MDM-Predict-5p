library(h2o)
library(ggplot2)
h2o.init()

h2o.init(nthreads = -1)
h2o.clusterInfo()

options(scipen=20)
#lendo cvs com todos os dados
totaldados <- read.csv("data/agregadoPorTemporadaSTATS.csv", sep=";")
totaldados[,c("posicao")] <- as.factor(totaldados[,c("posicao")])

totaldados$posicao <- as.factor(totaldados$posicao)
totaldados$violacao_jogador <- as.factor(totaldados$violacao_jogador)
totaldados$faltas_recebidas_jogador <- as.factor(totaldados$faltas_recebidas_jogador)

#totaldados$posicaoInt <- as.integer(totaldados$posicao)
#removendo jogadores de posicao nao definida
totaldados <- totaldados[totaldados$posicao != "NOTFOUND",]

#separando dados de treino com todas as temporadas exceto as duas ultimas
treino = totaldados[totaldados$nome_temporada != "T20142015",]
treino = treino[treino$nome_temporada != "T20152016",]

#separando dados de validacao com a penultima temporada
validacao = totaldados[totaldados$nome_temporada == "T20142015",]
#separando dados de teste com a ultima temporada
teste = totaldados[totaldados$nome_temporada == "T20152016",]




x <- setdiff(names(totaldados), c(totaldados, "id_jogador","id_partida", "nome_temporada", "data_nascimento", "eficiencia", "ts_per", "eFG_per", "ASTPer", "FTPer", "violacao", "faltas_recebidas" ))  #retirando colunas nao necessarias ao processo
y <- "posicao"

treino.hex  <- as.h2o(treino) # convertendo df para formato h2o
teste.hex  <- as.h2o(teste) 
validacao.hex  <- as.h2o(validacao) 

#ver resumo dos dados de treino
summary(treino.hex, exact_quantiles=TRUE)

#sumario tipo tabela
h2o.table(teste.hex$posicao)


#hyper_parameters = list(ntrees = ntrees_opts, learn_rate = learn_rate_opts)

ntrees_opts = c(50, 100)
learn_rate_opts = c(0.1, 0.01)
hyper_parametersGM = list(ntrees = ntrees_opts, learn_rate = learn_rate_opts)

gridGM <- h2o.grid("gbm", grid_id="gbm_grid_test", 
                 x=x, 
                 y=y, 
                 training_frame=treino.hex, 
                 validation_frame=validacao.hex, 
                 hyper_params = hyper_parametersGM)

hyper_parametersDP <- list(
  hidden=list(c(32,32,32),c(64,64)),
  input_dropout_ratio=c(0,0.05),
  rate=c(0.01,0.02),
  rate_annealing=c(1e-8,1e-7,1e-6)
)

gridDL <- h2o.grid("deeplearning", 
                grid_id="dl_grid_test", 
                x=x, 
                y=y, 
                training_frame=treino.hex, 
                validation_frame=validacao.hex, 
                epochs=10,
                stopping_metric="misclassification",
                stopping_tolerance=1e-2,        ## stop when misclassification does not improve by >=1% for 2 scoring events
                stopping_rounds=2,
                score_validation_samples=10000, ## downsample validation set for faster scoring
                score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
                adaptive_rate=F,                ## manually tuned learning rate
                momentum_start=0.5,             ## manually tuned momentum
                momentum_stable=0.9, 
                momentum_ramp=1e7, 
                l1=1e-5,
                l2=1e-5,
                activation=c("Rectifier"),
                max_w2=10,                  
                hyper_params = hyper_parametersDP)

glm_sorted_grid <- h2o.getGrid(grid_id = "gbm_grid_test", sort_by = "accuracy", decreasing = TRUE)

dl_sorted_grid <- h2o.getGrid(grid_id = "dl_grid_test", sort_by = "accuracy", decreasing = FALSE)
print(glm_sorted_grid)
print(dl_sorted_grid)

#grid_models <- lapply(grid@model_ids, function(mid) {
#  model = h2o.getModel(mid)
#})


# Grab the model_id based in AUC
best_glm_model_id <- gridGM@model_ids[[1]]

# The best model
best_glm <- h2o.getModel(best_glm_model_id)


my_varimpGM <- h2o.varimp(best_glm)
barplot(my_varimpGM$scaled_importance,
        names.arg = my_varimpGM$variable,
        space = 1,
        las = 2,
        main = "Variable Importance: GM GRID")

model = best_glm

pred2 = h2o.predict(object = model, newdata = teste.hex)

# Summary of the best model
summary(pred2)

mean(pred2$predict==teste.hex$posicao)


