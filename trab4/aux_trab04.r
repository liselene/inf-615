# Exemplo de leitura do dataset
# A primeira feature/coluna (V1) ?? a classe 
# As colunas V2 ~ V785 s??o os pixels de cada imagem 28x28 em tons de cinza. 
data = read.csv("mnist_trainVal.csv", header=FALSE)

#Inspecionando o n??mero de exemplos por classe
summary(as.factor(data[,1]))



# Fun????o para plotar uma imagem a partir de um ??nico feature vector
# imgArray = fv da imagem, com classe na V1 e os pixels em V2 ~ V785
# os pixels podem estar no intervalo [0.0,1.0] ou [0, 255]
plotImage = function(imgArray){
	# Transforma array em uma matrix 28x28 (ignorando a classe em V1)
	imgMatrix = matrix((imgArray[2:ncol(imgArray)]), nrow=28, ncol=28)

	# Transforma cada elemento em numeric
	im_numbers <- apply(imgMatrix, 2, as.numeric)

	# Girando a imagem apenas p/ o plot
	flippedImg = im_numbers[,28:1]

	image(1:28, 1:28, flippedImg, col=gray((0:255)/255), xlab="", ylab="")
	title(as.numeric(imgArray[1]) - 1)
}

#Ex de uso pegando o primeiro sample
plotImage(data[2,])






##################################
# Para gerar a f??rmula concatenando os nomes
#de cada uma das colunas
##################################
# Seleciona todos os nomes
feats <- names(data)

# Concatena o nome de cada feature, ignorando a primeira
f <- paste(feats[2:length(feats)],collapse=' + ')
f <- paste('V1 ~',f)

# Converte para f??rmula
f <- as.formula(f)
f