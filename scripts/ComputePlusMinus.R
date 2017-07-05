library(sqldf)

if (!exists("pbp") ){
  pbp <- read.csv("playbyplayunion.csv", sep = "," ,stringsAsFactors=FALSE)
  partidas <- sqldf("SELECT distinct (id_partida) from pbp")
  
  varsql = paste("SELECT id_jogador, equipe, id_equipe_casa, id_equipe_fora, tipo, placarcasa, placarfora, tempo, temposeg from pbp where (tipo = 'SBS' or tipo ='SBE' or tipo ='FIM') and id_partida=", 3536, sep= "")
  varsql = paste(varsql, " order by temposeg asc", sep= "")
  partida  <- sqldf(varsql)
  
  
}

players <- sqldf("SELECT distinct (id_jogador), equipe, id_equipe_casa, id_equipe_fora, placarcasa, placarfora, tipo from partida")
for(i in 1:nrow(players)) {
  
  pid = players[i,"id_jogador"];
  varsql = paste("SELECT id_jogador, equipe, id_equipe_casa, id_equipe_fora, tipo, placarcasa, placarfora, tempo, temposeg from partida where (tipo = 'SBS' or tipo ='SBE' or tipo ='FIM') and id_jogador=", pid, sep= "")
  partidaPlayer  <- sqldf(varsql)
  bpm = 0;
  for(j in 1:nrow(partidaPlayer)) {
    row = partidaPlayer[j,];
    #browser()
    if (is.na(row$id_equipe_casa)==0 && is.na(row$id_equipe_fora)==0){
      if (row$equipe == row$id_equipe_casa){
        bpm = (row$placarfora - row$placarcasa) + bpm
      }else if (row$equipe ==  row$id_equipe_fora){
        bpm = (row$placarcasa - row$placarfora) + bpm
      }
    }
  }
  
  players[i,"bpm"] = bpm
  
  
  
}
browser()


