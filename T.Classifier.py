# -*- coding: utf-8 -*-
"""
TCC
"""
#Bibliotecas
import nltk
import re
import glob
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn import metrics
from sklearn.model_selection import cross_val_predict
import random
nltk.download('stopwords')
random.seed(1997)

#Lista de Stop Words + acentuação
def Preprocessamento_SemStopWords(instancia):
    from nltk.corpus import stopwords
    from string import punctuation

    instancia = re.sub(r"http\S+", "", instancia).lower().replace(',','').replace('?','').replace('”','').replace("“",'').replace('"','').replace(':','').replace(';','').replace('.','').replace(';','').replace('-','').replace('1','').replace('2','').replace('3','').replace('4','').replace('5','').replace('6','').replace('7','').replace('8','').replace('9','').replace('0','').replace('%','')
    baseParametros = pd.read_excel("C:/Users/christyan.almeida/Google Drive/MESTRADO/1.ExternalData/Extract.Parameters.xlsx")
    codCompany  = baseParametros['CodCompany'].values
    NameCompany  = baseParametros['parameter1'].values
    
    grandesfreq = ['é','semestre','trimestre','ano','janeiro','fevereiro','março','abril','maio','junho','julho','agosto','setembro','agosto','novembro','dezembro','segunda','terça','quarta','quinta','sexta','sábado','somingo','-feira',
                   'dólares','reais','dólar','milhões','bilhões','trilhões','sede','maiores','menores','muito','pouco','muitos','poucos','primeiro','segundo','terceiro','quarto','quinto','sexto','setimo','oitavo','nono','décimo',
                   'analista','analistas','banco','brasil','destaque','destaques','acre','alagoas','amapá','amazonas','bahia','ceará','distrito federal','espírito santo','goiás','maranhão','mato grosso','mato grosso do sul','minas gerais','pará','paraíba','paraná','pernambuco','piauí','rio de janeiro','rio grande do norte','rio grande do sul','rondônia','roraima','santa catarina','são paulo','sergipe','tocantins',
                   'ª','º','°','resultados','carteira','zero','um','dois','três','quatro','cinco','seis','sete','oito','nove','dez',
                   'outros','infomoney','lojas','luiza','loja','bolsa','bancos','valor','mercado','até','dias','varejista','varejistas','varejo','magazine','cvc','kroton','bb','b3','tim','mil','turno','ranking','confira',
                   'boeing','estatal','multiplus','marcopolo','cvm','siderúrgicas','smiles','ação','ambev','nesta','cap','cielo','facebook','petroleiros','lista','petroleira','santander','us$','bradesco','entrevista','bnds','bradespar','usiminas','infotrade','camil','carrefour','ceming','corretora','construtoras','xp','duratex','inter','itaú','jbs','br','marfing','klabin','bc','bndes','stf','semana','veja','caixa','credit','bbi','itaúsa',
                   'balanço','balanços','faz','natura','minerva','ac','al','ap','am','ba','ce','df','es','go','ma','mt','ms','mg','pa','pb','pr','pe','pi','rj','rn','rs','ro','rr','sc','sp','se','to',
                   'ainda','btg','pactual','pdv','caps','small','recomendadas','ccr','oi','fibria','ibovespa','notícias','notícia','radar','brf','bi','mi','petrobras','ações','r$','vale','após','destaques','eletrobras','noticias','radar','tem','recomendações','embraer','suzano','itau']
    
    to_remove = ['não']
    new_stopwords = set(stopwords.words('portuguese')).difference(to_remove)
    
    stopwords = set(list(codCompany)+list(NameCompany)+list(new_stopwords)+ list(punctuation)+list(grandesfreq))
    palavras = [i for i in instancia.split() if not i in stopwords]
    return (" ".join(palavras))

#função para criar a raiz das palavras
def Stemming(instancia):
    from nltk.stem.porter import PorterStemmer
    stemmer = PorterStemmer()#nltk.stem.RSLPStemmer()
    palavras=[]
    for w in instancia.split():
        palavras.append(stemmer.stem(w))
    return (" ".join(palavras))






#Carregando base de dados (Total)
caminho =r'C:/Users/christyan.almeida/Google Drive/MESTRADO/4.DataStorage/1.ODS/Trainning'
#definindo a lista de arquivos presentes no caminho
arquivos = glob.glob(caminho + "/DBT.Trainning.csv")
frame = pd.DataFrame()
listaDados = []
for arquivo in arquivos:
    base = pd.read_csv(arquivo,delimiter=';')
    listaDados.append(base)


#criando a base total dos dados classificados
baseTotal = pd.concat(listaDados) 
#numero de linhas da base
nLinhasTotal = baseTotal['class'].count() 
#criando um vetor das linhas da base
vetorLinhas = [*range(nLinhasTotal)] 
#criando uma sequencia aleatoria pra o vetor de linhas
random.shuffle(vetorLinhas) 

#Carregando base de dados (Treinamento)
#quantidad de linhas que serao carregados no treinamento
qtdLinhaTreinamento = int(nLinhasTotal * (80/100))
baseTreinamento = baseTotal.iloc[vetorLinhas[0:qtdLinhaTreinamento]]
#Carregando base de dados (Teste)
baseTeste = baseTotal.iloc[vetorLinhas[qtdLinhaTreinamento:nLinhasTotal]]

#Separando texto da classificacao
#base de treinamento
textos = baseTreinamento['web.title'].values
classe = baseTreinamento['class'].values

textosTratados = []

for frase in textos:
    textosTratados.append(Preprocessamento_SemStopWords(frase))

textos = textosTratados

#base de teste
textosTesteOriginal = baseTeste['web.title'].values
textosTeste = baseTeste['web.title'].values
classeTeste = baseTeste['class'].values

textosTratados = []
for frase in textosTeste:
    textosTratados.append(Preprocessamento_SemStopWords(frase))


textosTeste = textosTratados

#Analisando o modelo seguindo a abordagem Bag of Words e o algoritmo Naive Bayes Multinomial
#MODELO COM UMA UNICA PALVRA
#vetor = CountVectorizer(analyzer="word")
#frequencia = vetor.fit_transform(textos) #armazena a frequencia de todas as palavras
#modelo = MultinomialNB()
#modelo.fit(frequencia,classe)

#MODELO COM GRUPOS DE PALVRAS
vetor = CountVectorizer(ngram_range=(1,4))
frequencia = vetor.fit_transform(textos)
modelo = MultinomialNB()
modelo.fit(frequencia,classe)

#Aplicar modelo na base de testes
frequencia_testes = vetor.transform(textosTeste)
modelo.predict(frequencia_testes)
probabilidade = modelo.predict_proba(frequencia_testes).tolist()
classeNova = modelo.predict(frequencia_testes).tolist()

#Criando e exportando tabela de produção classificada
tabelaFinalProd = pd.DataFrame({'prob':probabilidade,'title' : textosTesteOriginal,'titleT':textosTeste,'class':classeNova}) 

#Salvando Classificação
exportar = pd.ExcelWriter(caminho+'/DBT.Classification.xlsx', engine='xlsxwriter')
tabelaFinalProd.to_excel(exportar, sheet_name='class')
exportar.save()


#resultados = cross_val_predict(modelo, frequencia_testes, classeTeste, cv=10)
#acerto = metrics.accuracy_score(classeTeste,resultados)*100
#sentimento=[1,-1,0]
#print(acerto)
#print (metrics.classification_report(classeTeste,resultados,sentimento))
#print (pd.crosstab(classeTeste, resultados, rownames=['Real'], colnames=['Predito'], margins=True), '')
#print(acerto)
