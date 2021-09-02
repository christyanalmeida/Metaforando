Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_241')
library('rvest')
library('stringr')
#library('xlsx')
library('tidyverse')
library('dplyr')
library('tidyr')
library('readxl')
library('class')
library('plotly')



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

#PARAMETRIZAÇÃO
db = NULL
PerTreino = 0.7
PP = 2008
WP = 10
WF = 10
WFYean = 2018 - WF 

#CARREGANDO ARQUIVOS
for (anos in c(PP:WFYean)){
  
  db = rbind(db,read.csv(paste('C:/Users/christyan.almeida/Google Drive/MACKENZIE/MESTRADO/TESE/4.DataStorage/2.DW/MetaIndicadores/MetaIndicadores_',anos,'_WP',WP,'_WF',WF,'_MF0.5_BOVA.csv',sep = '')
                         ,header=TRUE,sep=','))
  
}

#TRATANDO ARQUIVOS CARREGADOS
db<- db[,2:22]
db <- as.data.frame(db) %>% column_to_rownames("Key")
db[is.na(db)] <- 0

for (vSeed in c(1997:2007)){
  set.seed(vSeed)
  
  #NUMERO DE LINHAS ALEÁTORIAS
  ran <- sample(1:nrow(db), PerTreino * nrow(db)) 
  
  #DATASET TREINO
  db_train <- rnorm(db[ran,-20])
  #DATASET TESTE
  db_test <- rnorm(db[-ran,-20])
  
  #CATEGORIA BASE TREINO
  db_categoria_treino <- db[ran,20]
  #CATEGORIA BASE TESTE
  db_categoria_test <- db[-ran,20]
  
  for(kvalor in c(1,3,5,7,9,11)){
    
    pr <- knn(db_train,db_test,cl=db_categoria_treino,k=kvalor)
    lvs <- levels(pr)
    pr <- factor(pr,
                 levels = rev(lvs))
    db_categoria_test <- factor(db_categoria_test,               
                                levels = rev(lvs))
    
    tab <- table(pr,db_categoria_test)
    accuracy(tab)
    
    db_predito <- db_test
    db_predito$real <- db_categoria_test
    db_predito$predito <- pr
    
    parameter.pathFile <- paste('C:/Users/christyan.almeida/Google Drive/MACKENZIE/MESTRADO/TESE/4.DataStorage/2.DW/MetaIndicadores/Results/Result.Metaindicadores_PP',PP,'.',WFYean,'_WP',WP,'_WF',WF,'_K',kvalor,'_MF0.5_Seed',vSeed,'.csv',sep = '')
    write.csv(db_predito, parameter.pathFile )
  }
}
#fig <- plot_ly(z = ~volcano)
#fig <- fig %>% add_surface()

#fig    
