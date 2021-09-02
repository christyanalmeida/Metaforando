Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_241')
library('rvest')
library('stringr')
#library('xlsx')
library('tidyverse')
library('dplyr')
library('tidyr')
library('readxl')
library('class')




#FUNÇÕES
rnorm<-function(planilha, minimo=0, maximo=1) {
  for(i in 1:ncol(planilha)){
    if(is.numeric(planilha[,i])){
      v_min<-min(planilha[,i])
      v_max<-max(planilha[,i])
      for(j in 1:nrow(planilha)){
        planilha[j,i]<-((planilha[j,i]-v_min)/(v_max-v_min))*((maximo-minimo)+minimo)
      }
    }
  }
  return(planilha) # Retornando a planilha normalizada
}

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}


dbParametros = NULL 
dbParametros <- read.csv(paste('C:/Users/CHRISTYAN/Google Drive/MACKENZIE/MESTRADO/TESE/4.DataStorage/2.DW/MetaIndicadores/Parametros.csv',sep = '')
                         ,header=TRUE,sep=';')

dbParametros$WP <- dbParametros$ï..WP
dbParametros <- dbParametros[order(dbParametros$WP),]

for(i in (1:nrow(dbParametros))) {

  #PARAMETRIZAÇÃO
  db = NULL
  PP = 2008
  MF = dbParametros$MF[i]
  WP = dbParametros$WP[i]
  WF = dbParametros$WF[i]
  WFYean = 2018 - WF 

#CARREGANDO ARQUIVOS
for (anos in c(PP:WFYean)){
  
  db = rbind(db,read.csv(paste('C:/Users/CHRISTYAN/Google Drive/MACKENZIE/MESTRADO/TESE/4.DataStorage/2.DW/MetaIndicadores/MetaIndicadores_',anos,'_WP',WP,'_WF',WF,'_MF',MF,'_BOVA.csv',sep = '')
                         ,header=TRUE,sep=','))
  
}

#TRATANDO ARQUIVOS CARREGADOS
db<- db[,2:22]
db <- as.data.frame(db) %>% column_to_rownames("Key")


db[is.na(db)] <- 0

  set.seed(2021)
  
  #EMBRALHAR DADOS
  db<-db[sample(nrow(db)),]
  
  #CRIANDO 10 PASTAS DE TAMANHOS IGUAIS
  folds <- cut(seq(1,nrow(db)),breaks=10,labels=FALSE)
  
  
 
  for(vkFolder in 1:10){
    #SEGMENTANDO OS DADOS EM CADA UMA DAS PASTAS
    testIndexes <- which(folds==vkFolder,arr.ind=TRUE)
    
    db_train <- db[-testIndexes,-20]
    #DATASET TESTE
    db_test <-db[testIndexes,-20]
    #CATEGORIA BASE TREINO
    db_categoria_treino <- db[-testIndexes,20]
    #CATEGORIA BASE TESTE
    db_categoria_test <- db[testIndexes,20]

    
    for(kvalor in c(1,3,5,7,9,11)){
     
      
      pr <- knn(db_train,db_test,cl=db_categoria_treino,k=kvalor)
      
      db_predito <- db_test
      db_predito <- cbind(AtivoAnalisado = rownames(db_predito), db_predito)
      rownames(db_predito) <- 1:nrow(db_predito)
      db_predito$real <- db_categoria_test
      db_predito$predito <- pr
      
      parameter.pathFile <- paste('C:/Users/CHRISTYAN/Google Drive/MACKENZIE/MESTRADO/TESE/4.DataStorage/2.DW/MetaIndicadores/Results/KNN/Result.Metaindicadores_PP',PP,'.',WFYean,'_WP',WP,'_WF',WF,'_K',kvalor,'_MF',MF,'_KFolder',vkFolder,'.csv',sep = '')
      write.csv(db_predito, parameter.pathFile )
      
      }

  }
  
}  
