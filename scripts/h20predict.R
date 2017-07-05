#https://mineracaodedados.wordpress.com/2017/01/10/implementacao-de-glm-com-grid-search-no-r-usando-o-h2o-ai-como-backend/
#https://mineracaodedados.wordpress.com/2017/01/15/deep-dive-com-gradient-boosting-machine-com-h2o-r-mais-grid-search/
#https://github.com/h2oai/h2o-3/blob/master/h2o-docs/src/product/tutorials/gbm/gbmTuning.Rmd
#https://github.com/h2oai/h2o-3/blob/master/h2o-docs/src/product/tutorials/GridSearch.md

library(h2o)
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

totaldados$posicaoInt <- as.integer(totaldados$posicao)
#removendo jogadores de posicao nao definida
totaldados <- totaldados[totaldados$posicao != "NOTFOUND",]

#separando dados de treino com todas as temporadas exceto as duas ultimas
treino = totaldados[totaldados$nome_temporada != "T20142015",]
treino = treino[treino$nome_temporada != "T20152016",]

#separando dados de validacao com a penultima temporada
validacao = totaldados[totaldados$nome_temporada == "T20142015",]
#separando dados de teste com a ultima temporada
teste = totaldados[totaldados$nome_temporada == "T20152016",]



X <- setdiff(names(totaldados), c(totaldados, "id_jogador","id_partida", "data_nascimento_jogador", "eficiencia_jogador", "ts_per", "eFG_per", "ASTPer", "FTPer", "violacao_jogador", "faltas_recebidas_jogador" ))  #retirando colunas nao necessarias ao processo
Y <- "posicaoInt"

treino.hex  <- as.h2o(treino) # convertendo df para formato h2o
teste.hex  <- as.h2o(teste) 

#ver resumo dos dados de treino
summary(treino.hex, exact_quantiles=TRUE)

#sumario tipo tabela
h2o.table(treino.hex$posicao)


# Construct a large Cartesian hyper-parameter space
ntrees_opts = c(50)       # early stopping will stop earlier
max_depth_opts = seq(1,20)
min_rows_opts = c(1,5,10,20,50,100)
learn_rate_opts = seq(0.001,0.01,0.001)
sample_rate_opts = seq(0.3,1,0.05)
col_sample_rate_opts = seq(0.3,1,0.05)
col_sample_rate_per_tree_opts = seq(0.3,1,0.05)
#nbins_cats_opts = seq(100,10000,100) # no categorical features
# in this dataset

hyper_params = list( ntrees = ntrees_opts, 
                     max_depth = max_depth_opts, 
                     min_rows = min_rows_opts, 
                     learn_rate = learn_rate_opts,
                     sample_rate = sample_rate_opts,
                     col_sample_rate = col_sample_rate_opts,
                     col_sample_rate_per_tree = col_sample_rate_per_tree_opts
                     #,nbins_cats = nbins_cats_opts
)
alpha_opts = c(0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1)
hyper_params_opt = list(alpha = alpha_opts)

rf_fit2 <- h2o.randomForest(x = X,
                            y = Y,
                            training_frame = treino.hex,
                            model_id = "rf_fit2",
                            
                            ntrees = 50,
                            seed = 1)

h2o.auc(rf_fit2)
treino.glm <- h2o.glm(training_frame=treino.hex
                        ,x=X
                        ,y=Y
                        ,alpha = 0.5
                        ,max_iterations = 300
                        ,beta_epsilon = 0
                        ,lambda = 1e-05
                        ,lambda_search = FALSE
                        ,early_stopping = FALSE
                        ,nfolds = 0
                        ,seed = NULL
                        ,intercept = TRUE
                        ,gradient_epsilon = -1
                        ,remove_collinear_columns = FALSE
                        ,max_runtime_secs = 10000
                        )


glm_grid <- h2o.grid("randomForest"
                     ,grid_id = "glm_grid_1"
                     ,x=X
                     ,y=Y
                     ,training_frame=treino.hex
                     #,hyper_params = hyper_params_opt
                     )

# Grid object with hyperparameters




gbm_grid <- h2o.grid("gbm", 
                     grid_id = "mygrid",
                     x = X, 
                     y = Y, 
                     
                     # faster to use a 80/20 split
                     training_frame = treino.hex,
                     validation_frame = teste.hex,
                     nfolds = 5,
                     
                     # alternatively, use N-fold cross-validation:
                     # training_frame = train,
                     # nfolds = 5,
                     
                     # Gaussian is best for MSE loss, but can try 
                     # other distributions ("laplace", "quantile"):
                     distribution="gaussian",
                     
                     # stop as soon as mse doesn't improve by 
                     # more than 0.1% on the validation set, 
                     # for 2 consecutive scoring events:
                     stopping_rounds = 2,
                     stopping_tolerance = 1e-3,
                     stopping_metric = "MSE",
                     
                     # how often to score (affects early stopping):
                     score_tree_interval = 100, 
                     
                     ## seed to control the sampling of the 
                     ## Cartesian hyper-parameter space:
                     seed = 123456,
                     hyper_params = hyper_params)





sortedGrid <- h2o.getGrid("glm_grid_1", sort_by="mse", decreasing = TRUE)

print(glm_sorted_grid)

# Grab the model_id based in AUC
best_glm_model_id <- glm_grid@model_ids[[1]]

# The best model
best_glm <- h2o.getModel(best_glm_model_id)

# Shutdown the cluster 
#h2o.shutdown()
