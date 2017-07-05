library(RCurl)
library(XML)
library(sqldf)
library(stringi)
library(qdapRegex)

#retentar retirando acentuacao dos jogadores


atletasUnique <- read.csv("atletasPosicaoDistinct.csv", sep = "," ,stringsAsFactors=FALSE)
atletas <- sqldf("SELECT distinct (id_jogador), nome_jogador, posicao, altura, peso FROM atletasUnique")
#wagner-carvalho-frança-mattos
#Manuil-Egidio-Leal-De-Souza
#Eduardo-Magalhaes-Machado
#Rodrigo-de-Camargo-Sant´anna
#Andre-Luiz-Chueri-da-Silva-Barbosa
#Pedro-Cesar-Teruel-Lima
#Ronald-Ricardo-Ramon-Guerrero
#Atila-Sacramento-dos-Santos
#David-Waynne-Jackson-Jr

#atletas <- read.csv("atletasPosicao.csv", sep = "," ,stringsAsFactors=FALSE)
#atletas <- atletas[is.na(atletas$posicao)==1,]
#atletas$posicao = NA
#atletas$altura = ""
#atletas$peso = ""
for(i in 1:nrow(atletas)) {
  #for(i in 1:1) {
  posicao = atletas[i,"posicao"];
  nomeatleta = atletas[i,"nome_jogador"];  
  #navega no site
  if (posicao=="NOTFOUND"){
     #browser()
    nomeatleta = gsub(" ","-",nomeatleta)
    nomeatleta <- stri_trans_general(nomeatleta, "Latin-ASCII")
    
    url = paste("http://lnb.com.br/atletas/", nomeatleta, sep="")
    
    tryCatch({
      
      tabelas <- readHTMLTable(url, stringsAsFactors = F)
      if (length(tabelas) > 0){
        temp <- tabelas[1]
        
        names(temp) <- c("name1")
        atletas[i,"posicao"] = temp$name1$V4[1]
        posicaoBarra = regexpr('/', temp$name1$V4[3])
        if (posicaoBarra >= 0) {
          
          atletas[i,"altura"] =gsub("/", "", trimws(substr(temp$name1$V4[3], 1, posicaoBarra)))
          atletas[i,"peso"] = gsub("/", "", trimws(substr(temp$name1$V4[3], posicaoBarra, nchar(temp$name1$V4[3] ))))
        }else{
          if (regexpr('kg', temp$name1$V4[3]) >= 0) {
            atletas[i,"peso"] = gsub("/", "",trimws(temp$name1$V4[3]))
          }else{
            atletas[i,"altura"] = gsub("/", "",trimws(temp$name1$V4[3]))
          }
        }
        print(i)
        print(url)
        
      }else{
        atletas[i,"posicao"] = "NOTFOUNDCONFIRMED"
        print(i)
        print(url)
        print("Nao encontrado")
        
      }
      if (i%%5==0){
        print("salvando")
        write.csv(atletas, file = "atletasPosicaoDistinct.csv")
        
      }
      
      
    }, error = function() { 
      
    }, finally={
      
    })
    
  }
  #write.table(atletas[i,], file = "atletasPosicaoV1.csv", sep = ",", col.names = F, append=T)
  
  
}
print("fim")
#write.csv(atletas, file = "atletasPosicaoV2.csv")

