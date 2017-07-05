library(SDMTools)
library(lattice)
library(rjson)
library(ggplot2)
library(grid)
library(gridExtra)
library(png) 
library(jpeg)
library(RCurl)
library(hexbin)
library(plyr)
library(cowplot)


calculaRegiaoQuadra<-function(DFLinha, idJogador, idPartida)
{
  
if (!exists("dataFrameOriginal") ){
dataFrameOriginal <- read.csv("nbbdata/jogadas.csv", sep = ";" ,stringsAsFactors=FALSE)
}

 

dataFrame = dataFrameOriginal[ dataFrameOriginal$idPartida==idPartida, ]
dataFrame = dataFrame[ dataFrame$id_jogador==idJogador, ]
dataFrame[is.na(dataFrame)] <- 0
dataFrame <- dataFrame[dataFrame$posX > 0, ]
dataFrame <- dataFrame[dataFrame$posY > 0, ]

if (nrow(dataFrame) > 0 ){
  #browser()
playName = "giovanoni"
dataFrame["resultado"]<- NA 
dataFrame["resultadoint"]<- NA 


#put zero where cell is not a number
dataFrame[is.na(dataFrame)] <- 0
#get only coords greater than zero


for (i in 1:nrow(dataFrame))
{ 
  # fazendo o mirror dos pontos do ado direito da quadra para o esquerdo
  # considerando quadra de 250 por 466
  if(dataFrame[i,c("posX")]> 233){
    dataFrame[i,c("posX")] <- 466 - dataFrame[i,c("posX")]
    dataFrame[i,c("posY")] <- 250 - dataFrame[i,c("posY")]
  }
  
  
  # agrupando erros e acertos
  if(dataFrame[i,c("tipo")] == "ENT" || dataFrame[i,c("tipo")] == "A2C" || dataFrame[i,c("tipo")] == "A3C" ){
    dataFrame[i,c("resultado")] <- "Made Shot"
    dataFrame[i,c("resultadoint")] <- 1
  }
  
  if(dataFrame[i,c("tipo")] == "A2E" || dataFrame[i,c("tipo")] == "A3E" ){
    dataFrame[i,c("resultado")]<- "Missed Shot"
    dataFrame[i,c("resultadoint")] <- 0
  }
  if(dataFrame[i,c("tipo")] == "ENT"  ){
    dataFrame[i,c("tipo")] <-  "A2C"
  }
  
  if (dataFrame[i,c("tipo")] != "A3C" && dataFrame[i,c("tipo")] != "A3E" && dataFrame[i,c("posX")] > 150){
    dataFrame[i,c("posX")] = 0
    
  }
  
  if (dataFrame[i,c("tipo")] != "A3C" && dataFrame[i,c("tipo")] != "A3E" && dataFrame[i,c("posY")] > 220){
    dataFrame[i,c("posX")] = 0
    
  }
  if ( dataFrame[i,c("posY")] > 250){
    dataFrame[i,c("posX")] = 0
    
  }
  
  
  
}
dataFrame <- dataFrame[dataFrame$posX > 0, ]

#browser()

  
  #get court image from the folder
  courtImg <- "fiba.jpg"
  
  court <- rasterGrob(readJPEG(courtImg),
                      width=unit(1,"npc"), height=unit(1,"npc"))
  
  
  key <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  key <- rbind(key ,c(113, 85),c(113, 165),c(140, 180),c(155, 125),c(140, 70))
  colnames(key) <- cols
  
  topkey <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  topkey <- rbind(topkey ,c(140, 70), c(155, 125), c(140, 180), c(250, 237),c(250, 14))
  colnames(topkey) <- cols
  
  pc <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  pc <- rbind(pc ,c(113, 165),c(113, 85),c(30, 125))
  colnames(pc) <- cols
  
  pcl <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  pcl <- rbind(pcl ,c(30, 125), c(30, 160),c(113, 160))
  colnames(pcl) <- cols
  
  pcr <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  pcr <- rbind(pcr ,c(30, 90), c(30, 125),c(113, 90))
  colnames(pcr) <- cols

  cl <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  cl <- rbind(cl, c(30,223) ,c(30,237), c(113,237), c(113,203), c(70,223))
  colnames(cl) <- cols  

  scl <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  scl <- rbind(scl, c(30, 160),c(30,223), c(70,223), c(113,203), c(113,160))
  colnames(scl) <- cols   
  
  swl <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  swl <- rbind(swl, c(113, 165), c(113,203),c(140, 180) )
  colnames(swl) <- cols   
  
  wl <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  wl <- rbind(wl, c(113,203),c(113,237),c(250, 237), c(140, 180) )
  colnames(wl) <- cols   

  cr <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  cr <- rbind(cr, c(30,14), c(30, 28),  c(70,28), c(113,46),c(113,14))
  colnames(cr) <- cols  
  
  scr <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  scr <- rbind(scr, c(30, 28),c(30,90), c(113,90), c(113,46), c(70,28))
  colnames(scr) <- cols   
  
  swr <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  swr <- rbind(swr, c(113,46),c(113,90),c(139, 70) )
  colnames(swr) <- cols   
  
  wr <- data.frame(posX= numeric(0), posY= integer(0))
  cols <- c("posX","posY")
  wr <- rbind(wr,c(113,15),c(113,46), c(139, 70) ,c(250, 14))
  colnames(wr) <- cols   
  #names(dataFrame)
  
   #df <- dataFrame.binned[,c("posX","posY")]
   #ggplot ( dataFrame , aes (x =posX, y =posY )) +
  #  annotation_custom(court, 14, 250, 12, 237) +
  #  geom_point()+
  #  geom_polygon(data = topkey, alpha = 0.8, fill="#0000FF") +
  #  geom_text(x=200, y=125, label="TK")+
  #  geom_polygon(data = pc, alpha = 0.8, fill="#298A08") +
  #  geom_polygon(data = pcl, alpha = 0.5, fill="#2EFE2E") +
  #  geom_polygon(data = pcr, alpha = 0.5, fill="#2EFE2E") +
  #  geom_polygon(data = key, alpha = 0.8, fill="#FE2E2E") +
  #  geom_polygon(data = cl, alpha = 0.8, fill="#F7FE2E") +
  #  geom_polygon(data = cr, alpha = 0.8, fill="#F7FE2E") +     
  #  geom_polygon(data = scr, alpha = 0.5, fill="#F7FE2E") +     
  #  geom_polygon(data = scl, alpha = 0.5, fill="#F7FE2E") +
  #  geom_polygon(data = swl, alpha = 0.5, fill="#FE2E2E") +
  #  geom_polygon(data = swr, alpha = 0.5, fill="#FE2E2E") +     
  #  geom_polygon(data = wl, alpha = 0.5, fill="#0000FF") +
  #  geom_polygon(data = wr, alpha = 0.5, fill="#0000FF") +
     
  #  scale_colour_gradient("Points/Attempt", low = "yellow", high="red") + 
  #  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  #  geom_rug(alpha = 0.2) +
  #  coord_fixed() +
  #  scale_x_continuous(limits = c(0, 250))+
  #  ggtitle(paste("Shot Chart\n", unique(playName), sep = "")) 
   
   
   # contando os pontos de cada area
   pontosCertos = dataFrame[dataFrame$resultadoint == 1,c("posX","posY")]
   pontosTotal = dataFrame[,c("posX","posY")] 
   
   
   intotopkeyC = pnt.in.poly(pontosCertos,topkey)
   intotopkeyT = pnt.in.poly(pontosTotal,topkey)
   DFLinha$total_certo_topkey <- nrow(intotopkeyC[which(intotopkeyC$pip==1),])
   DFLinha$total_topkey <- nrow(intotopkeyT[which(intotopkeyT$pip==1),])
   
   intokeyC = pnt.in.poly(pontosCertos,key)
   intokeyT = pnt.in.poly(pontosTotal,key)
   DFLinha$total_certo_key <- nrow(intokeyC[which(intokeyC$pip==1),])
   DFLinha$total_key <- nrow(intokeyT[which(intokeyT$pip==1),])
   
   intopcC = pnt.in.poly(pontosCertos,pc)
   intopcT = pnt.in.poly(pontosTotal,pc)
   DFLinha$total_certo_pc <- nrow(intopcC[which(intopcC$pip==1),])
   DFLinha$total_pc <- nrow(intopcT[which(intopcT$pip==1),])
   
   intopcrC = pnt.in.poly(pontosCertos,pcr)
   intopcrT = pnt.in.poly(pontosTotal,pcr)
   DFLinha$total_certo_pcr <- nrow(intopcrC[which(intopcrC$pip==1),])
   DFLinha$total_pcr <- nrow(intopcrT[which(intopcrT$pip==1),])
   
   intopclC = pnt.in.poly(pontosCertos,pcl)
   intopclT = pnt.in.poly(pontosTotal,pcl)
   DFLinha$total_certo_pcl <- nrow(intopclC[which(intopclC$pip==1),])
   DFLinha$total_pcl <- nrow(intopclT[which(intopclT$pip==1),])
   
   intocrC = pnt.in.poly(pontosCertos,cr)
   intocrT = pnt.in.poly(pontosTotal,cr)
   DFLinha$total_certo_cr <- nrow(intocrC[which(intocrC$pip==1),])
   DFLinha$total_cr <- nrow(intocrT[which(intocrT$pip==1),])
   
   intoclC = pnt.in.poly(pontosCertos,cl)
   intoclT = pnt.in.poly(pontosTotal,cl)
   DFLinha$total_certo_cl <- nrow(intoclC[which(intoclC$pip==1),])
   DFLinha$total_cl <- nrow(intoclT[which(intoclT$pip==1),])
   
   intoscrC = pnt.in.poly(pontosCertos,scr)
   intoscrT = pnt.in.poly(pontosTotal,scr)
   DFLinha$total_certo_scr <- nrow(intoscrC[which(intoscrC$pip==1),])
   DFLinha$total_scr <- nrow(intoscrT[which(intoscrT$pip==1),])
   
   intosclC = pnt.in.poly(pontosCertos,scl)
   intosclT = pnt.in.poly(pontosTotal,scl)
   DFLinha$total_certo_scl <- nrow(intosclC[which(intosclC$pip==1),])
   DFLinha$total_scl <- nrow(intosclT[which(intosclT$pip==1),])
   
   intoswlC = pnt.in.poly(pontosCertos,swl)
   intoswlT = pnt.in.poly(pontosTotal,swl)
   DFLinha$total_certo_swl <- nrow(intoswlC[which(intoswlC$pip==1),])
   DFLinha$total_swl <- nrow(intoswlT[which(intoswlT$pip==1),])
   
   intoswrC = pnt.in.poly(pontosCertos,swr)
   intoswrT = pnt.in.poly(pontosTotal,swr)
   DFLinha$total_certo_swr <- nrow(intoswrC[which(intoswrC$pip==1),])
   DFLinha$total_swr <- nrow(intoswrT[which(intoswrT$pip==1),])
   
   intowlC = pnt.in.poly(pontosCertos,wl)
   intowlT = pnt.in.poly(pontosTotal,wl)
   DFLinha$total_certo_wl <- nrow(intowlC[which(intowlC$pip==1),])
   DFLinha$total_wl <- nrow(intowlT[which(intowlT$pip==1),])
   
   intowrC = pnt.in.poly(pontosCertos,wr)
   intowrT = pnt.in.poly(pontosTotal,wr)
   DFLinha$total_certo_wr <- nrow(intowrC[which(intowrC$pip==1),])
   DFLinha$total_wr <- nrow(intowrT[which(intowrT$pip==1),])
   #browser()
   
}else{
  #browser()
  DFLinha$total_certo_topkey <- 0
  DFLinha$total_topkey <- 0
  
  DFLinha$total_certo_key <- 0
  DFLinha$total_key <- 0
  
  DFLinha$total_certo_pc <- 0
  DFLinha$total_pc <- 0
  
  DFLinha$total_certo_pcr <- 0
  DFLinha$total_pcr <- 0
  
  DFLinha$total_certo_pcl <- 0
  DFLinha$total_pcl <- 0
  
  DFLinha$total_certo_cr <- 0
  DFLinha$total_cr <- 0
  
  DFLinha$total_certo_cl <- 0
  DFLinha$total_cl <- 0
  
  DFLinha$total_certo_scr <- 0
  DFLinha$total_scr <- 0
  
  DFLinha$total_certo_scl <- 0
  DFLinha$total_scl <- 0
  
  DFLinha$total_certo_swl <- 0
  DFLinha$total_swl <- 0
  
  DFLinha$total_certo_swr <- 0
  DFLinha$total_swr <- 0
  
  DFLinha$total_certo_wl <- 0
  DFLinha$total_wl <- 0
  
  DFLinha$total_certo_wr <- 0
  DFLinha$total_wr <- 0

}
  
   return (DFLinha);
}

atletasJogo <- read.csv("bancoatletas.csv", sep = "," ,stringsAsFactors=FALSE)
atletasJogoAjustado <- atletasJogo[1, ]

atletasJogoAjustado$total_certo_topkey <- 0
atletasJogoAjustado$total_topkey <- 0

atletasJogoAjustado$total_certo_key <- 0
atletasJogoAjustado$total_key <- 0

atletasJogoAjustado$total_certo_pc <- 0
atletasJogoAjustado$total_pc <- 0

atletasJogoAjustado$total_certo_pcr <- 0
atletasJogoAjustado$total_pcr <- 0

atletasJogoAjustado$total_certo_pcl <- 0
atletasJogoAjustado$total_pcl <- 0

atletasJogoAjustado$total_certo_cr <- 0
atletasJogoAjustado$total_cr <- 0

atletasJogoAjustado$total_certo_cl <- 0
atletasJogoAjustado$total_cl <- 0

atletasJogoAjustado$total_certo_scr <- 0
atletasJogoAjustado$total_scr <- 0

atletasJogoAjustado$total_certo_scl <- 0
atletasJogoAjustado$total_scl <- 0

atletasJogoAjustado$total_certo_swl <- 0
atletasJogoAjustado$total_swl <- 0

atletasJogoAjustado$total_certo_swr <- 0
atletasJogoAjustado$total_swr <- 0

atletasJogoAjustado$total_certo_wl <- 0
atletasJogoAjustado$total_wl <- 0

atletasJogoAjustado$total_certo_wr <- 0
atletasJogoAjustado$total_wr <- 0

atletasJogoAjustado <- atletasJogoAjustado[0, ]

for(i in 1:nrow(atletasJogo)) {
  linha = atletasJogo[i,];
  
  atletasJogoAjustado[i,] = calculaRegiaoQuadra(linha, linha$id_jogador, linha$id_partida)
  #browser()
  print(i)
  
  if (i %% 1000 == 0){
    write.csv(atletasJogoAjustado, file = "bancoatletasAjustado.csv") 
  }
}
  
