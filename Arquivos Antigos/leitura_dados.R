#Carregar Pacote
#---------------------------
#library("dplyr")
#---------------------------

leitura_dados = function(){
	dados = read.csv('dataset_venda.csv',sep=';')
	dados = data.frame(dados,stringsAsFactors = FALSE)
	names = c("Dia","Temperatura",1:70,"Total")
	colnames(dados) = names
	dados[,2] = gsub(",",".",dados[,2])

	for(j in 3:length(dados[1,])){
		for(i in 1:length(dados[,1])){
			if(is.na(dados[i,j])==TRUE){
				dados[i,j] = ""
			} 
		}	
	}

	geral = data.frame(dados$Dia,dados$Temperatura,dados$Total)
	colnames(geral) = c("Dia","Temperatura","Total")
	geral$dia.da.semana = format.Date(as.Date(geral[,1]), "%a",tz = "UTC")
	geral$Temperatura = as.numeric(as.character(geral$Temperatura))
	for (i in 1:length(geral[,1])){
		geral$temp[i] = geral[i,2]-(geral[i,2]%%1)
	}

	return(geral)
}