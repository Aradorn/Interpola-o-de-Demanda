interpolacao = function(ponto,tabela){

	lagrange =  function(k,x,xi){
		lagrange_k = 1
		n=length(xi)
		for (i in 1:n){
			if(i!=k){
				lagrange_k = lagrange_k*((x-xi[i])/(xi[k]-xi[i]))
			}
		}
		return(lagrange_k)
	}

	teste = sample(1:length(dados[,1]),1)

	x = ponto
	xi = tabela[,1]
	yi = tabela[,2]
	n = length(xi)
	P=0
	for(k in 1:n){
		P = P+yi[k]*lagrange(k,x,xi)

	}
	print(paste("Resultado do Ponto = ",P))
	return(P)
}

ajuste_curva = function(ponto,tabela){
	m = length(tabela[,1])
	Sxi = sum(tabela[,1])
	Sxi2 = 0
	for(i in 1:length(tabela[,1])){Sxi2 = Sxi2+(tabela[i,1]^2)}
	Sf = sum(tabela[,2])
	Sfxi = 0
	for(i in 1:length(tabela[,1])){Sfxi = Sfxi+(tabela[i,1]*tabela[i,2])}

	alfa2 = (m*Sfxi-Sxi*Sf)/(m*Sxi2-(Sxi^2))
	alfa1 = (Sf-Sxi*alfa2)/5

	y = alfa1+alfa2*ponto

	print(paste("Função Linear = ",alfa1,"+",alfa2,"x"))

	print(paste("Resultado do Ponto = ",y))
	return(y)
}