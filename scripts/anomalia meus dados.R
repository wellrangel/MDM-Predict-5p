library(h2o)
library(data.table)
library(sqldf)
library(plyr)

h2o.init()
h2o.init(nthreads = -1)

options(scipen=20)
totaldados <- read.csv("data/agregadoPorTemporadaSTATS.csv", sep=",", stringsAsFactors=FALSE)


dadospartida <- read.csv("data/bancoatletasORIGINAL.csv", sep=";", stringsAsFactors=FALSE)
#groupPartida =  sqldf( "select distinct(id_jogador) from dadospartida group by id_partida having count(id_partida) > 5")
#totaldados =  join(totaldados, groupPartida, type = "inner")


group =  sqldf( "select id_jogador from totaldados group by id_jogador having count(1) > 3")
totaldados =  join(totaldados, group, type = "inner")

totaldados[is.na(totaldados)] <- 0
totaldados <- totaldados[totaldados$id_jogador > 0,]

#todos jogadores com pelo menos 10 partidas por temporada

totaldados[,c("posicao")] <- as.factor(totaldados[,c("posicao")])

#totaldados$posicao <- as.factor(totaldados$posicao)
#totaldados$violacao <- as.factor(totaldados$violacao)
#totaldados$faltas_recebidas <- as.factor(totaldados$faltas_recebidas)

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



y <- "posicao"
x <- setdiff(names(totaldados), c(totaldados, "altura", "minutos_partida", "idade", "id_jogador","id_partida", "data_nascimento", "violacao", "faltas_recebidas" ))  #retirando colunas nao necessarias ao processo

treino.hex  <- as.h2o(treino)
teste.hex  <- as.h2o(teste)

ae_model = h2o.deeplearning(x = x, training_frame = treino.hex,
                           autoencoder = TRUE,
                           reproducible = T,
                           seed = 1234,
                           hidden = c(6,5,6), epochs = 50)

test_rec_error <- as.data.frame(h2o.anomaly(ae_model, teste.hex)) 

plot.ts(test_rec_error)
plot(sort(test_rec_error$Reconstruction.MSE), main='Reconstruction Error', ylab='Reconstruction.MSE', xlab='Distribution')


anon=teste[test_rec_error>0.081,]
mean(teste[,c("minutos_partida_jogador")])

mean(anon[,c("minutos_partida_jogador")])

test_recon <- predict(ae_model, teste.hex)

summary(ae_model)

errors <- h2o.anomaly(ae_model, teste.hex, per_feature = TRUE)

print(errors)

recon_error <- as.data.frame(errors)

plot.ts(recon_error$reconstr_altura.SE)
plot(sort(recon_error$Reconstruction.MSE), main='Reconstruction Error', ylab='Reconstruction.MSE', xlab='Distribution')


anon=teste[recon_error$reconstr_altura.SE>0.10,]
my_data <- as.matrix(as.data.frame(teste[anomalia,]))


h2o.shutdown(prompt=FALSE)