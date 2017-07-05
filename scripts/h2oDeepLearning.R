#0.7093023 com altura
#0.6976744 sem altura
#0.686047 com altura sem AV
#0.686047 com altura e AV

library(h2o)
library(ggplot2)
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

x <- setdiff(names(dadosOriginalCSV), c(dadosOriginalCSV,"posicao", "ts_per", "eFG_per","ASTPer", "TurPer", "FTPer", "FG_per", "PirPer", "minutos_partida", "idade", "id_jogador","id_partida", "data_nascimento", "violacao", "faltas_recebidas" ))  #retirando colunas nao necessarias ao processo
y <- "posicao"


dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = treino.hex,
                            validation_frame = validacao.hex,
                            model_id = "dl_fit3",
                            seed = 1234,
                            epochs = 20,
                            #score_interval = 1,           #used for early stopping
                            #stopping_rounds = 3,          #used for early stopping
                            #stopping_metric = "MSE",      #used for early stopping
                            #stopping_tolerance = 0.0005,  #used for early stopping
                            nfolds = 10,
                            variable_importances=T)




dl_fit3
plot(dl_fit3, 
     timestep = "epochs", 
     metric = "rmse")
h2o.mse(dl_fit3)

my_varimpDL <- h2o.varimp(dl_fit3)



barplot(my_varimpDL$scaled_importance,
        names.arg = my_varimpDL$variable,
        space = 1,
        las = 2,
        main = "Variable Importance: Deep Learning")


p <- ggplot(data = my_varimpDL, aes(x = variable, y =scaled_importance ))
p <- p + geom_bar(stat = "identity")
p <- p + coord_flip()
p

h2o.varimp(dl_fit3)
#plot(my_varimpDL)

plot(dl_fit3)


dl_perf3 <- h2o.performance(model = dl_fit3,
                            newdata = teste.hex)

# Print model performance
dl_perf3

h2o.auc(dl_perf3, train = FALSE, valid = FALSE, xval = FALSE)

h2o.mse(dl_perf3, valid = TRUE)


h2o.table(treino.hex$posicao)
h2o.table(validacao.hex$posicao)
h2o.table(teste.hex$posicao)


  finaldl_predictions<-h2o.predict(
  object = dl_fit3
  ,newdata = teste.hex)

h2o.hit_ratio_table(dl_fit3,valid = T)[1,2]             ## validation set accuracy

mean(finaldl_predictions$predict==teste.hex$posicao)

h2o.shutdown(prompt=FALSE)