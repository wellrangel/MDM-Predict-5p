library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(party)
library(RColorBrewer)
library(MASS)
library(GGally)
library(dplyr)
library(d3heatmap)


#nba = read.csv("nba.csv")
#nba$Position = as.character(nba$Position)
#which(nchar(nba$Position)>3)
#nba = nba[1:342,]
#nba = nba[-c(159,260,324,327),]
#nba$randu = runif(338, 0, 1)
#nba.train = nba[nba$randu < .4,]
#nba.test = nba[nba$randu >= .4,]

totaldados <- read.csv("data/agregadoPorTemporadaSTATS.csv", sep=";")
totaldados[,c("posicao")] <- as.factor(totaldados[,c("posicao")])

totaldados$posicao <- as.factor(totaldados$posicao)
totaldados$violacao_jogador <- as.factor(totaldados$violacao_jogador)
totaldados$faltas_recebidas_jogador <- as.factor(totaldados$faltas_recebidas_jogador)

totaldados$posicaoInt <- as.integer(totaldados$posicao)
#removendo jogadores de posicao nao definida
totaldados <- totaldados[totaldados$posicao != "NOTFOUND",]

#separando dados de treino com todas as temporadas exceto as duas ultimas
treino = totaldados[totaldados$nome_temporada != "T20142015",]
treino = treino[treino$nome_temporada != "T20152016",]

#separando dados de validacao com a penultima temporada
validacao = totaldados[totaldados$nome_temporada == "T20142015",]
#separando dados de teste com a ultima temporada
teste = totaldados[totaldados$nome_temporada == "T20152016",]


#points
ggplot(totaldados, aes(x = totaldados$arremessos_3_certos_jogador)) + geom_density(aes(fill = posicao, alpha = 0.2))
#true shootings score
#ggplot(nba, aes(x = totaldados$a)) + geom_density(aes(fill = posicao, alpha = 0.2))
#Offensive Rebounds
#ggplot(nba, aes(x = TRB)) + geom_density(aes(fill = Position, alpha = 0.2))
#Defensive Rebounds
#ggplot(nba, aes(x = DRB)) + geom_density(aes(fill = Position, alpha = 0.2))
#Steals
#ggplot(nba, aes(x = STL)) + geom_density(aes(fill = Position, alpha = 0.2))



#Decision Tree - Good Fit 
#model2
fit = rpart(posicao ~., data = treino, method="class")
#fancyRpartPlot(fit)
#prediction
nba.test$Prediction <- predict(fit, nba.test, type = "class")
#tabling results
table(nba.test$Position, nba.test$Prediction)
prop.table(table(nba.test$Position,nba.test$Prediction),1)

#Decision Tree - Over Fit 
#model2
fit = rpart(Position ~., data = nba.train[,c(2,6:21)], method="class", control=rpart.control(minsplit=3, cp=0.001))
plot(fit$variable.importance)
#fancyRpartPlot(fit)
#prediction
nba.test$Prediction <- predict(fit, nba.test, type = "class")
#tabling results
table(nba.test$Position, nba.test$Prediction)
prop.table(table(nba.test$Position,nba.test$Prediction),1)

#RF
#model 1
rf.model = randomForest(as.factor(Position) ~ . , data= nba.train[,c(2,6:21)], ntree= 500, mtry = round(sqrt(ncol(nba))))
mean(rf.model$err.rate)
plot(rf.model)
#predicting using test set
nba.test$pred.pos.rf = predict(rf.model, nba.test, type="response")
#tabling resutls 
table(nba.test$Position,nba.test$pred.pos.rf)
#proportion table 

prop.table(table(nba.test$Position,nba.test$pred.pos.rf),1)
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rf.model)
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf.model$err.rate),col=1:6,cex=0.8,fill=1:6)


#table
ave.table = summaryBy(. ~ factor(Position) , data = nba[,c(2,6:21)], FUN = mean)
stargazer(ave.table[,-c(2,13,14,15)], summary = F, font.size = "small", column.sep.width = "1pt", covariate.labels = c("ID", "Position", "PER", "TS", "ORB", "DRB", "TRB", "AST", "BLK", "TOV", "USG", "ORtg", "DRtg","STL"))
stargazer(nba)

#finding best parameters(ntree and mtry)
bestmtry <- tuneRF(nba[,c(6:21)], factor(nba$Position), stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)
plot(rf.model)



