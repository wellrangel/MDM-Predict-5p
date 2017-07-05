library(h2o)


h2o.init(nthreads = -1)
h2o.clusterInfo()

options(scipen=20)
#lendo cvs com todos os dados
totaldados <- read.csv("data/agregadoPorTemporadaSTATS.csv", sep=",")
totaldados$X1 = NULL
totaldados$X = NULL

head(totaldados)
totaldados[,c("posicao")] <- as.factor(totaldados[,c("posicao")])

totaldados$posicao <- as.factor(totaldados$posicao)
totaldados$violacao <- as.factor(totaldados$violacao)
totaldados$faltas_recebidas <- as.factor(totaldados$faltas_recebidas)

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




x <- setdiff(names(totaldados), c(totaldados, "idade","id_jogador","id_partida", "data_nascimento", "violacao", "faltas_recebidas" ))  #retirando colunas nao necessarias ao processo
y <- "posicao"

treino.hex  <- as.h2o(treino) # convertendo df para formato h2o
teste.hex  <- as.h2o(teste) 
validacao.hex  <- as.h2o(validacao) 

#ver resumo dos dados de treino
summary(treino.hex, exact_quantiles=TRUE)

#sumario tipo tabela
h2o.table(treino.hex$posicao)



# 2. Random Forest
# H2O's Random Forest (RF) implements a distributed version of the standard 
# Random Forest algorithm and variable importance measures.
# First we will train a basic Random Forest model with default parameters. 
# The Random Forest model will infer the response distribution from the response encoding. 
# A seed is required for reproducibility.
rf_fit1 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = treino.hex,
                            model_id = "rf_fit1",
                            seed = 1)

# Next we will increase the number of trees used in the forest by setting `ntrees = 100`.  
# The default number of trees in an H2O Random Forest is 50, so this RF will be twice as 
# big as the default.  Usually increasing the number of trees in a RF will increase 
# performance as well.  Unlike Gradient Boosting Machines (GBMs), Random Forests are fairly 
# resistant (although not free from) overfitting.
# See the GBM example below for additional guidance on preventing overfitting using H2O's 
# early stopping functionality.
rf_fit2 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = treino.hex,
                            model_id = "rf_fit2",
                            #validation_frame = valid,  #only used if stopping_rounds > 0
                            ntrees = 100,
                            seed = 1)




# Cross-validate performance
# Rather than using held-out test set to evaluate model performance, a user may wish 
# to estimate model performance using cross-validation. Using the RF algorithm 
# (with default model parameters) as an example, we demonstrate how to perform k-fold 
# cross-validation using H2O. No custom code or loops are required, you simply specify 
# the number of desired folds in the nfolds argument.
# Since we are not going to use a test set here, we can use the original (full) dataset, 
# which we called data rather than the subsampled `train` dataset. Note that this will 
# take approximately k (nfolds) times longer than training a single RF model, since it 
# will train k models in the cross-validation process (trained on n(k-1)/k rows), in 
# addition to the final model trained on the full training_frame dataset with n rows.

rf_fit3 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = treino.hex,
                            validation_frame = validacao.hex,
                            model_id = "rf_fit3",
                            seed = 1,
                            ntrees = 50)



h2o.auc(rf_fit3)

my_varimpRF <- h2o.varimp(rf_fit3)
varImpPlot(my_varimpRF)
1- h2o.mse(rf_fit3)



barplot(my_varimpRF$scaled_importance,
        names.arg = my_varimpRF$variable,
        space = 1,
        las = 2,
        main = "Variable Importance: Random Forest")

h2o.varimp(rf_fit3)
#plot(my_varimpRF)

plot(rf_fit1)
plot(rf_fit2)
plot(rf_fit3)

# Let's compare the performance of the two RFs
rf_perf1 <- h2o.performance(model = rf_fit1,
                            newdata = teste.hex)
rf_perf2 <- h2o.performance(model = rf_fit2,
                            newdata = teste.hex)
rf_perf3 <- h2o.performance(model = rf_fit3,
                            newdata = teste.hex)

# Print model performance
rf_perf1
rf_perf2
rf_perf3

h2o.auc(rf_perf3, train = FALSE, valid = FALSE, xval = FALSE)

h2o.mse(rf_perf1, valid = TRUE)
h2o.mse(rf_perf2, valid = TRUE)
h2o.mse(rf_perf3, valid = TRUE)


h2o.table(treino.hex$posicao)
h2o.table(validacao.hex$posicao)
h2o.table(teste.hex$posicao)


finalRf_predictions<-h2o.predict(
  object = rf_fit3
  ,newdata = teste.hex)



h2o.hit_ratio_table(rf_fit3,valid = T)[1,2]             ## validation set accuracy
mean(finalRf_predictions$predict==teste.hex$posicao)  ## test set accuracy

#h2o.shutdown()