source("funcoes_interpolacao.R",  encoding = getOption("encoding"))
source("leitura_dados.R",  encoding = getOption("encoding"))
library("dplyr")

dados = leitura_dados()

#REMOVER OUTLIERS GERAL
outliers = boxplot.stats(dados$Total)$out
for(i in 1:length(outliers)){
	dados = dados[which(dados$Total != outliers[i]),]
}

outliers = boxplot.stats(dados$Total)$out
for(i in 1:length(outliers)){
	dados = dados[which(dados$Total != outliers[i]),]
}
#-------------------------------
#REMOVER OUTLIERS POR TEMPERATURA

vetor_temperatura = unique(dados[5])
dados1 = data.frame()
for(i in 1:length(vetor_temperatura[,1])){
	nome = paste(vetor_temperatura[i,1])
	temp = subset(dados, dados[5]==nome)
	outliers = boxplot.stats(temp$Total)$out
	if(length(outliers)>0){
	for(i in 1:length(outliers)){
		temp = temp[which(temp$Total != outliers[i]),]
	}
	}
	dados1 = rbind(dados1,temp)
}
dados = dados1
#-------------------------------
#RETIRADA DE DADOS TESTE

tabela = aggregate(Total ~ temp, data=dados, FUN=mean)
n = aggregate(Total~temp, data=dados, FUN=length)
tabela = merge(tabela,n,by.x = "temp",by.y = "temp")
colnames(tabela) = c("Temperatura","Venda","n")

dados_teste = sample(dados$Temperatura,length(dados[,1])*0.10,replace = FALSE)
dados_teste = data.frame(dados_teste)
colnames(dados_teste) = "Temperatura"
dados_teste = left_join(dados_teste, dados, by = "Temperatura")

validacao = data.frame()

#-------------------------------
#INTERPOLACAO E AJUTE DE CURVAS
#tabela = tabela[8:16,]
for(i in 1:length(dados_teste[,1])){

	ponto = dados_teste[i,1]
	resultado = dados_teste[i,3]

	if(ponto < 16 || ponto > 25){
		resultado_estimado = ajuste_curva(ponto,tabela)
	} else {
		resultado_estimado = interpolacao(ponto,tabela)
	}

	if(resultado_estimado < 0){resultado_estimado = resultado_estimado*-1}

	erro = resultado - resultado_estimado
	if(erro < 0){erro = erro*-1}
	erro_percentual = round((erro / resultado),2)

	print(paste("Erro Absoluto: ",erro," ",erro_percentual))

	v = data.frame(ponto,resultado,resultado_estimado,erro,erro_percentual)
	validacao = rbind(validacao,v)
}
colnames(validacao) = c("Temperatura","Demanda Real","Demanda Estimada","Erro Absoluto","Erro Percentual")
#-------------------------------
#View(validacao[order(validacao$Temperatura),])

a = (validacao[order(validacao[,5]),])
a = a[1:(length(a[,1])),]
View(a)
summary(a)