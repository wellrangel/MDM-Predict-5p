library(sqldf)
atletasJogo <- read.csv("data/bancoatletasProdutivo.csv", sep = "," ,stringsAsFactors=FALSE)



#atletasJogo$minutos_partida_jogadorAjustado =  substr(atletasJogo$minutos_partida_jogador, 1,  3)
#atletasJogo[regexpr('\\.', atletasJogo$minutos_partida_jogadorAjustado) > 0,]$minutos_partida_jogadorAjustado = as.numeric(atletasJogo[regexpr('\\.', atletasJogo$minutos_partida_jogadorAjustado) > 0,]$minutos_partida_jogadorAjustado)
#atletasJogo[!regexpr('\\.', atletasJogo$minutos_partida_jogadorAjustado) > 0,]$minutos_partida_jogadorAjustado = as.numeric(atletasJogo[!regexpr('\\.', atletasJogo$minutos_partida_jogadorAjustado) > 0,]$minutos_partida_jogadorAjustado)/60
#atletasJogo$minutos_partida_jogadorAjustado =  substr(atletasJogo$minutos_partida_jogadorAjustado, 1,  4)
#atletasJogo[atletasJogo$minutos_partida_jogadorAjustado > 10.0,]$minutos_partida_jogadorAjustado

atletas <- sqldf("SELECT distinct(id_jogador), nome_jogador, id_partida, data_nascimento_jogador ,posicao, altura, nome_temporada, sum(minutos_partida_jogador) as minutos_partida_jogador, pontos_equipe_casa, 
pontos_equipe_fora, sum(pontos_total_jogador) as pontos_total_jogador, sum(pontos_total_jogador_e_pontos_perdidos_jogador) as pontos_total_jogador_e_pontos_perdidos_jogador,
sum(arremessos_2_certos_jogador) as arremessos_2_certos_jogador, sum(arremessos_2_total_jogador) as arremessos_2_total_jogador, sum(arremessos_3_certos_jogador) as arremessos_3_certos_jogador,
sum(arremessos_3_total_jogador) as arremessos_3_total_jogador, sum(arremessos_lance_livre_certos_jogador) as arremessos_lance_livre_certos_jogador, 
sum(arremessos_lance_livre_total_jogador) as arremessos_lance_livre_total_jogador,
sum(rebotes_ofensivos_jogador) as rebotes_ofensivos_jogador, sum(rebotes_defensivos_jogador) as rebotes_defensivos_jogador, sum(rebotes_total_jogador) as rebotes_total_jogador,
sum(assistencias_total_jogador) as assistencias_total_jogador, sum(roubadas_bola_jogador) as roubadas_bola_jogador, sum(tocos_total_jogador) as tocos_total_jogador, 
sum(faltas_cometidas_jogador) as faltas_cometidas_jogador,sum(faltas_recebidas_jogador) as faltas_recebidas_jogador, sum(erros_jogador) as erros_jogador, 
sum(violacao_jogador) as violacao_jogador, sum(enterradas_total_jogador) as enterradas_total_jogador,
sum(enterradas_total_jogador) as enterradas_total_jogador, sum(eficiencia_jogador) as eficiencia_jogador
FROM atletasJogo group by nome_temporada, id_jogador")


write.csv(atletas, file = "data/agregadoPorTemporada.csv")
