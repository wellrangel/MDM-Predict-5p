library(sqldf)
library(lubridate)

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

totalGames <- function(idJogador, temporada) {
  if (!exists("atletasJogoOriginal") ){
    atletasJogoOriginal <- read.csv("data/bancoatletasProdutivo.csv", sep = ";" ,stringsAsFactors=FALSE)
  }
  
  temp = atletasJogoOriginal[atletasJogoOriginal$id_jogador==idJogador,]
  temp = temp[temp$nome_temporada==temporada,]
  #browser()
  return(nrow(temp))
}

atletasJogo <- read.csv("data/agregadoPorTemporada.csv", sep = "," ,stringsAsFactors=FALSE)

#totalGames(22, '2008/2009')

#temp = atletasJogoOriginal[atletasJogoOriginal$id_jogador==22,]
#temp = temp[temp$nome_temporada=='2008/2009',]

#transformando data de nascimento em anos
atletasJogo[atletasJogo$data_nascimento_jogador=="NULL", c("data_nascimento_jogador")] <- 0

atletasJogo$idade = age(as.Date(paste(substr(atletasJogo$data_nascimento_jogador, 7, 10), substr(atletasJogo$data_nascimento_jogador, 4, 5), substr(atletasJogo$data_nascimento_jogador, 1, 2), sep="-" )))


#faz media da idade para casos de NA
atletasJogo[is.na(atletasJogo$idade), c("idade")] <- mean(atletasJogo[,c("idade")], na.rm = TRUE) 

#faz media da altura para casos de NA
atletasJogo[is.na(atletasJogo$altura), c("altura")] <- mean(atletasJogo[,c("altura")], na.rm = TRUE) 

#calculando assistencia/erro
atletasJogo$assistencia_erro = atletasJogo$assistencias_total_jogador/atletasJogo$erros_jogador

#True shooting percentage
temp = (atletasJogo$arremessos_2_total_jogador + atletasJogo$arremessos_2_total_jogador) + (0.44 * atletasJogo$arremessos_lance_livre_total_jogador) 
atletasJogo$ts_per = ( atletasJogo$pontos_total_jogador/ (2*temp) ) * 100


#Effective Field Goal Percentage (eFG%)
atletasJogo$eFG_per =  (( (atletasJogo$arremessos_2_certos_jogador + atletasJogo$arremessos_3_certos_jogador) + (0.5 * atletasJogo$arremessos_3_certos_jogador) ) / (atletasJogo$arremessos_2_total_jogador + atletasJogo$arremessos_3_total_jogador) ) *100

#Assist Turnover (AST)
atletasJogo$ASTPer = (atletasJogo$assistencias_total_jogador/atletasJogo$roubadas_bola_jogador) 


#Assist Turnover (AST)
atletasJogo$TurPer = (100*atletasJogo$erros_jogador)/ ((atletasJogo$arremessos_2_certos_jogador + atletasJogo$arremessos_3_certos_jogador) + (0.44 * atletasJogo$arremessos_lance_livre_total_jogador) +  atletasJogo$erros_jogador)

#Free Throw Percentage (FT%)
atletasJogo$FTPer = (atletasJogo$arremessos_lance_livre_certos_jogador/atletasJogo$arremessos_lance_livre_total_jogador) * 100


#Field Goal Percentage
atletasJogo$FG_per = ( (atletasJogo$arremessos_2_certos_jogador + atletasJogo$arremessos_3_certos_jogador) / (atletasJogo$arremessos_2_total_jogador + atletasJogo$arremessos_3_total_jogador) )* 100


#Performance Index Rating (PIR) --- problema aqui porque nao temos tocos recebidos... aparentemente somente os feitos
atletasJogo$PirPer = (atletasJogo$pontos_total_jogador + atletasJogo$rebotes_total_jogador + atletasJogo$assistencias_total_jogador + atletasJogo$roubadas_bola_jogador +atletasJogo$tocos_total_jogador + atletasJogo$faltas_recebidas_jogador) 
- ( (atletasJogo$arremessos_2_total_jogador - atletasJogo$arremessos_2_certos_jogador) + (atletasJogo$arremessos_3_total_jogador - atletasJogo$arremessos_3_certos_jogador) + 
      ( atletasJogo$arremessos_lance_livre_total_jogador - atletasJogo$arremessos_lance_livre_certos_jogador) +  atletasJogo$erros_jogador + atletasJogo$tocos_total_jogador +  atletasJogo$faltas_cometidas_jogador) 



#ajustando eficiencia por ser percentual, preciso dividir pelo numero de jogos na temporada
for(i in 1:nrow(atletasJogo)) {
  #atletasJogo[i,]$eficiencia_jogador = atletasJogo[i,]$eficiencia_jogador/totalGames(atletasJogo[i,]$id_jogador,atletasJogo[i,]$nome_temporada) 
}




#for(i in 1:nrow(atletasJogo)) {
  #Defensive Rebound Percentage (DRB%)
  #para cada jogo do jogador, contar o ORB total do time adversario
  #pegar os dados do jogos de todos os jogadores da partida sendo o time adversario
  
 # browser()
  #temp = atletasJogoOriginal[atletasJogoOriginal$id_jogador==atletasJogo[i,]$id_jogador,]
 
  
#  nomeTimeJogador <- unique(temp[i,]$nome_equipe_jogador)
  
 # ok = atletasJogo[i,]
  #temp = atletasJogoOriginal[atletasJogoOriginal$nome_temporada==atletasJogo[i,]$nome_temporada,]
#  minutosJogados = atletasJogo[i,]$minutos_partida_jogador
#  timeadv = temp[temp$nome_equipe_fora==nomeTimeJogador,]
#  timejogador = temp[temp$nome_equipe_casa==nomeTimeJogador,]
#  minutosTimeJogado = 240* nrow(timejogador)
  
#  totalTeamDRB = sum(timejogador$rebotes_defensivos_jogador)
#   totalTimeAdvORB = sum(timeadv$rebotes_ofensivos_jogador)
  
#  atletasJogo[i,]$DRB = ((atletasJogo[i,]$rebotes_defensivos_jogador *(minutosTimeJogado/5)) / (minutosJogados *(totalTeamDRB+totalTimeAdvORB)) ) * 100
#}



#team possessions
#pega o time do jogador na temporada




#timeJogador <- unique(temp$nome_equipe_jogador)
# agora pega todos os jogos da temporada desta equipe
#jogosequipe = atletasJogoOriginal[atletasJogoOriginal$nome_temporada=='2008/2009',]
#jogosequipe = jogosequipe[jogosequipe$nome_equipe_casa==timeJogador | jogosequipe$nome_equipe_fora==timeJogador,]


#pega as estatisticas feitas
#jogosequipeFeitas = jogosequipe[jogosequipe$nome_equipe_casa==jogosequipe$nome_equipe_jogador,]

#pega as estatisticas recebidas
#jogosequipeRecebidas = jogosequipe[jogosequipe$nome_equipe_fora==jogosequipe$nome_equipe_jogador,]

#calcula a tem possessions

#teampos <-  jogosequipeFeitas$arremessos_2_certo_jogador + jogosequipeFeitas$arremessos_3_certo_jogador 






#ajusta nome da temporada 
atletasJogo$nome_temporada = paste("T", gsub("/", "", atletasJogo$nome_temporada), sep="")


#,"id_jogador","id_partida","data_nascimento_jogador","posicao","altura","nome_temporada","minutos_partida_jogador","pontos_equipe_casa","pontos_equipe_fora",
#"pontos_total_jogador","pontos_total_jogador_e_pontos_perdidos_jogador","arremessos_2_certos_jogador","arremessos_2_total_jogador","arremessos_3_certos_jogador","arremessos_3_total_jogador","
#arremessos_lance_livre_certos_jogador","arremessos_lance_livre_total_jogador","rebotes_ofensivos_jogador","rebotes_defensivos_jogador","rebotes_total_jogador","assistencias_total_jogador"
#,"roubadas_bola_jogador","tocos_total_jogador","faltas_cometidas_jogador","faltas_recebidas_jogador","erros_jogador","violacao_jogador","enterradas_total_jogador","enterradas_total_jogador.1"
#,"eficiencia_jogador","idade","assistencia_erro","ts_per","eFG_per","ASTPer","TurPer","FTPer","FG_per","PirPer"
#ajustando nomes das colunas
colnames(atletasJogo)[colnames(atletasJogo)=="data_nascimento_jogador"] <- "data_nascimento"
colnames(atletasJogo)[colnames(atletasJogo)=="minutos_partida_jogador"] <- "minutos_partida"
colnames(atletasJogo)[colnames(atletasJogo)=="pontos_equipe_casa"] <- "pontos_time_casa"
colnames(atletasJogo)[colnames(atletasJogo)=="pontos_equipe_fora"] <- "pontos_time_fora"
colnames(atletasJogo)[colnames(atletasJogo)=="pontos_total_jogador"] <- "pontos_total_feitos"
colnames(atletasJogo)[colnames(atletasJogo)=="pontos_total_jogador_e_pontos_perdidos_jogador"] <- "pontos_total"
colnames(atletasJogo)[colnames(atletasJogo)=="arremessos_2_certos_jogador"] <- "arremessos_2_certos"
colnames(atletasJogo)[colnames(atletasJogo)=="arremessos_2_total_jogador"] <- "arremessos_2_total"
colnames(atletasJogo)[colnames(atletasJogo)=="arremessos_3_certos_jogador"] <- "arremessos_3_certos"
colnames(atletasJogo)[colnames(atletasJogo)=="arremessos_3_total_jogador"] <- "arremessos_3_total"
colnames(atletasJogo)[colnames(atletasJogo)=="arremessos_lance_livre_certos_jogador"] <- "arremessos_lance_livre_certos"
colnames(atletasJogo)[colnames(atletasJogo)=="arremessos_lance_livre_total_jogador"] <- "arremessos_lance_livre_total"
colnames(atletasJogo)[colnames(atletasJogo)=="rebotes_ofensivos_jogador"] <- "rebotes_ofensivos"
colnames(atletasJogo)[colnames(atletasJogo)=="rebotes_defensivos_jogador"] <- "rebotes_defensivos"
colnames(atletasJogo)[colnames(atletasJogo)=="rebotes_total_jogador"] <- "rebotes_total"
colnames(atletasJogo)[colnames(atletasJogo)=="roubadas_bola_jogador"] <- "roubadas_bola"
colnames(atletasJogo)[colnames(atletasJogo)=="tocos_total_jogador"] <- "tocos_total"
colnames(atletasJogo)[colnames(atletasJogo)=="faltas_cometidas_jogador"] <- "faltas_cometidas"
colnames(atletasJogo)[colnames(atletasJogo)=="faltas_recebidas_jogador"] <- "faltas_recebidas"
colnames(atletasJogo)[colnames(atletasJogo)=="erros_jogador"] <- "erros"
colnames(atletasJogo)[colnames(atletasJogo)=="violacao_jogador"] <- "violacao"
colnames(atletasJogo)[colnames(atletasJogo)=="enterradas_total_jogador"] <- "enterradas_total"
colnames(atletasJogo)[colnames(atletasJogo)=="eficiencia_jogador"] <- "eficiencia"

atletasJogo <- atletasJogo[atletasJogo$posicao != "",]
atletasJogo$X.1 = NULL
atletasJogo$X1 = NULL
atletasJogo$X = NULL
atletasJogo$enterradas_total_jogador.1 = NULL


write.csv(atletasJogo, file = "data/agregadoPorTemporadaSTATS.csv", row.names=FALSE)
