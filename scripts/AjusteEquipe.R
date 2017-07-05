equipe <- read.csv("nbbdata/equipe.csv", sep = ";" ,stringsAsFactors=FALSE)

#separa pontos
equipe$pontosEquipe1 = paste(equipe$pontosEquipe1, " F", sep= "")
equipe$pontosAcertosEquipe1 = substr(equipe$pontosEquipe1, 1, regexpr('-', equipe$pontosEquipe1)-1)
equipe$pontosTentativaEquipe1 = substr(equipe$pontosEquipe1, regexpr('-', equipe$pontosEquipe1) +1, regexpr('F', equipe$pontosEquipe1))
equipe$pontosTentativaEquipe1 = substr(equipe$pontosTentativaEquipe1, 1, regexpr(' ', equipe$pontosTentativaEquipe1))
equipe$pontosEquipe1 <- NULL
  
#separa faltas
equipe$faltasEquipe1 = paste(equipe$faltasEquipe1, ",", sep= "")
equipe$faltasEquipe1 = substr(equipe$faltasEquipe1, 1, regexpr(',', equipe$faltasEquipe1)-1)

#separa 2 pontos 
equipe$p2Equipe1 = paste(equipe$p2Equipe1, " F", sep= "")
equipe$pontos2AcertosEquipe1 = substr(equipe$p2Equipe1, 1, regexpr('-', equipe$p2Equipe1)-1)
equipe$pontos2TentativaEquipe1 = substr(equipe$p2Equipe1, regexpr('-', equipe$p2Equipe1) +1, regexpr('F', equipe$p2Equipe1))
equipe$pontos2TentativaEquipe1 = substr(equipe$pontos2TentativaEquipe1, 1, regexpr(' ', equipe$pontos2TentativaEquipe1))
equipe$p2Equipe1 <- NULL

#separa 3 pontos 
equipe$p3Equipe1 = paste(equipe$p3Equipe1, " F", sep= "")
equipe$pontos3AcertosEquipe1 = substr(equipe$p3Equipe1, 1, regexpr('-', equipe$p3Equipe1)-1)
equipe$pontos3TentativaEquipe1 = substr(equipe$p3Equipe1, regexpr('-', equipe$p3Equipe1) +1, regexpr('F', equipe$p3Equipe1))
equipe$pontos3TentativaEquipe1 = substr(equipe$pontos3TentativaEquipe1, 1, regexpr(' ', equipe$pontos3TentativaEquipe1))
equipe$p3Equipe1 <- NULL

#separa ll 
equipe$llEquipe1 = paste(equipe$llEquipe1, " F", sep= "")
equipe$llAcertosEquipe1 = substr(equipe$llEquipe1, 1, regexpr('-', equipe$llEquipe1)-1)
equipe$llTentativaEquipe1 = substr(equipe$llEquipe1, regexpr('-', equipe$llEquipe1) +1, regexpr('F', equipe$llEquipe1))
equipe$llTentativaEquipe1 = substr(equipe$llTentativaEquipe1, 1, regexpr(' ', equipe$llTentativaEquipe1))
equipe$llEquipe1 <- NULL
  
#separa enterradas
equipe$enEquipe1= paste(equipe$enEquipe1, ",", sep= "")
equipe$enEquipe1 = substr(equipe$enEquipe1, 1, regexpr(',', equipe$enEquipe1)-1)

#separa rebote
equipe$rebEquipe1 = paste(equipe$rebEquipe1, " F", sep= "")
equipe$rebOfensivoEquipe1 = substr(equipe$rebEquipe1, 1, regexpr('-', equipe$rebEquipe1)-1)
equipe$rebDefensivoEquipe1 = substr(equipe$rebEquipe1, regexpr('-', equipe$rebEquipe1) +1, regexpr('F', equipe$rebEquipe1) -1)
equipe$rebDefensivoEquipe1 = substr(equipe$rebDefensivoEquipe1, 1, regexpr(' ', equipe$rebDefensivoEquipe1)-1)
equipe$rebDefensivoEquipe1 = gsub(",", "", equipe$rebDefensivoEquipe1)
#equipe$rebEquipe1 <- NULL


#separa assistencia
equipe$assEquipe1= paste(equipe$assEquipe1, ",", sep= "")
equipe$assEquipe1 = substr(equipe$assEquipe1, 1, regexpr(',', equipe$assEquipe1)-1)

#separa br
equipe$brEquipe1= paste(equipe$brEquipe1, ",", sep= "")
equipe$brEquipe1 = substr(equipe$brEquipe1, 1, regexpr(',', equipe$brEquipe1)-1)

#separa to
equipe$toEquipe1= paste(equipe$toEquipe1, ",", sep= "")
equipe$toEquipe1 = substr(equipe$toEquipe1, 1, regexpr(',', equipe$toEquipe1)-1)

#separa fr
equipe$frEquipe1= paste(equipe$frEquipe1, ",", sep= "")
equipe$frEquipe1 = substr(equipe$frEquipe1, 1, regexpr(',', equipe$frEquipe1)-1)

#separa er
equipe$erEquipe1= paste(equipe$erEquipe1, ",", sep= "")
equipe$erEquipe1 = substr(equipe$erEquipe1, 1, regexpr(',', equipe$erEquipe1)-1)

#separa ef
equipe$efEquipe1= paste(equipe$efEquipe1, ",", sep= "")
equipe$efEquipe1 = substr(equipe$efEquipe1, 1, regexpr(',', equipe$efEquipe1)-1)


#separa pontos
equipe$pontosEquipe2 = paste(equipe$pontosEquipe2, " F", sep= "")
equipe$pontosAcertosEquipe2 = substr(equipe$pontosEquipe2, 1, regexpr('-', equipe$pontosEquipe2)-1)
equipe$pontosTentativaEquipe2 = substr(equipe$pontosEquipe2, regexpr('-', equipe$pontosEquipe2) +1, regexpr('F', equipe$pontosEquipe2))
equipe$pontosTentativaEquipe2 = substr(equipe$pontosTentativaEquipe2, 1, regexpr(' ', equipe$pontosTentativaEquipe2))
equipe$pontosEquipe2 <- NULL

#separa faltas
equipe$faltasEquipe2 = paste(equipe$faltasEquipe2, ",", sep= "")
equipe$faltasEquipe2 = substr(equipe$faltasEquipe2, 1, regexpr(',', equipe$faltasEquipe2)-1)

#separa 2 pontos 
equipe$p2Equipe2 = paste(equipe$p2Equipe2, " F", sep= "")
equipe$pontos2AcertosEquipe2 = substr(equipe$p2Equipe2, 1, regexpr('-', equipe$p2Equipe2)-1)
equipe$pontos2TentativaEquipe2 = substr(equipe$p2Equipe2, regexpr('-', equipe$p2Equipe2) +1, regexpr('F', equipe$p2Equipe2))
equipe$pontos2TentativaEquipe2 = substr(equipe$pontos2TentativaEquipe2, 1, regexpr(' ', equipe$pontos2TentativaEquipe2))
equipe$p2Equipe2 <- NULL

#separa 3 pontos 
equipe$p3Equipe2 = paste(equipe$p3Equipe2, " F", sep= "")
equipe$pontos3AcertosEquipe2 = substr(equipe$p3Equipe2, 1, regexpr('-', equipe$p3Equipe2)-1)
equipe$pontos3TentativaEquipe2 = substr(equipe$p3Equipe2, regexpr('-', equipe$p3Equipe2) +1, regexpr('F', equipe$p3Equipe2))
equipe$pontos3TentativaEquipe2 = substr(equipe$pontos3TentativaEquipe2, 1, regexpr(' ', equipe$pontos3TentativaEquipe2))
equipe$p3Equipe2 <- NULL

#separa ll 
equipe$llEquipe2 = paste(equipe$llEquipe2, " F", sep= "")
equipe$llAcertosEquipe2 = substr(equipe$llEquipe2, 1, regexpr('-', equipe$llEquipe2)-1)
equipe$llTentativaEquipe2 = substr(equipe$llEquipe2, regexpr('-', equipe$llEquipe2) +1, regexpr('F', equipe$llEquipe2))
equipe$llTentativaEquipe2 = substr(equipe$llTentativaEquipe2, 1, regexpr(' ', equipe$llTentativaEquipe2))
equipe$llEquipe2 <- NULL

#separa enterradas
equipe$enEquipe2= paste(equipe$enEquipe2, ",", sep= "")
equipe$enEquipe2 = substr(equipe$enEquipe2, 1, regexpr(',', equipe$enEquipe2)-1)

#separa rebote
equipe$rebEquipe2 = paste(equipe$rebEquipe2, " F", sep= "")
equipe$rebOfensivoEquipe2 = substr(equipe$rebEquipe2, 1, regexpr('-', equipe$rebEquipe2)-1)
equipe$rebDefensivoEquipe2 = substr(equipe$rebEquipe2, regexpr('-', equipe$rebEquipe2) +1, regexpr('F', equipe$rebEquipe2) -1)
equipe$rebDefensivoEquipe2 = substr(equipe$rebDefensivoEquipe2, 1, regexpr(' ', equipe$rebDefensivoEquipe2)-1)
equipe$rebDefensivoEquipe2 = gsub(",", "", equipe$rebDefensivoEquipe2)
equipe$rebEquipe2 <- NULL


#separa assistencia
equipe$assEquipe2= paste(equipe$assEquipe2, ",", sep= "")
equipe$assEquipe2 = substr(equipe$assEquipe2, 1, regexpr(',', equipe$assEquipe2)-1)

#separa br
equipe$brEquipe2= paste(equipe$brEquipe2, ",", sep= "")
equipe$brEquipe2 = substr(equipe$brEquipe2, 1, regexpr(',', equipe$brEquipe2)-1)

#separa to
equipe$toEquipe2= paste(equipe$toEquipe2, ",", sep= "")
equipe$toEquipe2 = substr(equipe$toEquipe2, 1, regexpr(',', equipe$toEquipe2)-1)

#separa fr
equipe$frEquipe2= paste(equipe$frEquipe2, ",", sep= "")
equipe$frEquipe2 = substr(equipe$frEquipe2, 1, regexpr(',', equipe$frEquipe2)-1)

#separa er
equipe$erEquipe2= paste(equipe$erEquipe2, ",", sep= "")
equipe$erEquipe2 = substr(equipe$erEquipe2, 1, regexpr(',', equipe$erEquipe2)-1)

#separa ef
equipe$efEquipe2= paste(equipe$efEquipe2, ",", sep= "")
equipe$efEquipe2 = substr(equipe$efEquipe2, 1, regexpr(',', equipe$efEquipe2)-1)


write.csv(equipe, file = "equipeAjustado.csv")

#teste = gsub("\\\\/", "-", "dfd\\/fdfd")
#teste = gsub("\\\\", "-", teste)

#posicaoBarra = regexpr('-', teste)
#pa = substr(teste, 1, posicaoBarra-1)
#pt = substr(teste, posicaoBarra +1, nchar(teste))


