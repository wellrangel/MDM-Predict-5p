library(sqldf)
atletasJogo <- read.csv("data/bancoatletasORIGINAL.csv", sep = ";" ,stringsAsFactors=FALSE)


#retirando jogadores com jogos por temporada
atletas <- sqldf("SELECT id_jogador, nome_temporada, count(id_partida)  as totalpartida from atletasJogo group by nome_temporada, id_jogador") 
atletas <- atletas[atletas$totalpartida > 5,]

atletas10 <- sqldf("SELECT *  from atletasJogo aj inner join atletas a on aj.id_jogador = a.id_jogador  and aj.nome_temporada = a.nome_temporada") 

#retirando jogadores com pouca produtividade

atletasProdutividade <- atletas10[atletas10$pontos_total_jogador >=3,]
atletasProdutividade <- atletasProdutividade[atletasProdutividade$rebotes_total_jogador >=1,]
#atletasProdutividade <- atletasProdutividade[atletasProdutividade$faltas_cometidas_jogador >=1,]

#retirando jogadores que jogaram poucas temporadas 
atletas <- sqldf("SELECT id_jogador  as totalpartida from atletasJogo group by nome_temporada, id_jogador") 

atletas <- count(atletas)

atletas <-  atletas[atletas$freq > 3,]

atletasIDJ <-atletas$totalpartida

atletasProdutividadeFinal <- subset(atletasProdutividade, id_jogador %in% atletasIDJ)

write.csv(atletasProdutividadeFinal, file = "data/bancoatletasProdutivo.csv")

