#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#
#     Aula 6c: Estimação da área foliar por imagens 
#              (Segmentação usando modelo linear generalizado)
#
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Este script faz a segmentação da mesma forma que feito no script RotinaSeg.R
# Porém vamos usar ootro pacote, o EBImage. Além disso, será usada uma figura
# referência com area conhcecida para pode estimar áreas de imagens.

# Fonte: https://www.youtube.com/watch?v=mEVkdh6_0Pk

  remove(list=ls())

# Carregando imagens ------------------------------------------------------
  library(EBImage) # Parece que ele é um pouco melhor que OpenImageR
  
  
  im <- readImage("images/area_sarnambi_fundo-azul2/imagem2.jpg")
  plot(im)
  ref <- readImage("images/area_sarnambi_fundo-azul2/referencia.jpg")
  plot(ref)
  fundo <- readImage("images/area_sarnambi_fundo-azul2/imagem-fundo.jpg")
  plot(fundo)
  sarnambi <- readImage("images/area_sarnambi_fundo-azul2/sarnambi.jpg")
  plot(sarnambi)


# Transformando valores de cor em matriz (Array bidimensional) -----------------------------------------
# Cada imagem é convertida para uma matriz de pixels (linhas e colunas), cada pixel 
  # tendo 3 valores: R, G e B.
 
  mref <- cbind(c(ref@.Data[,,1]),  # Canal vermelho da imagem de referência
                c(ref@.Data[,,2]),  # Canal verde
                c(ref@.Data[,,3]))  # Canal azul
  
  # Embaralha as linhas (pixels) da matriz da referência
  mref <- mref[sample(1:nrow(mref)),]
  mref <- mref[1:20000,]  # Seleciona apenas os primeiros 20.000 pixels
  colnames(mref) <- c("R","G","B")
  
# Repetindo para o fundo e sarnambi
  mfundo <- cbind(c(fundo@.Data[,,1]),
                  c(fundo@.Data[,,2]),
                  c(fundo@.Data[,,3]))
  mfundo <- mfundo[sample(1:nrow(mfundo)),]
  mfundo <- mfundo[1:20000,]
  colnames(mfundo) <- c("R","G","B")
  
  msarnambi <- cbind(c(sarnambi@.Data[,,1]),
                     c(sarnambi@.Data[,,2]),
                     c(sarnambi@.Data[,,3]))
  msarnambi <- msarnambi[sample(1:nrow(msarnambi)),]
  msarnambi <- msarnambi[1:20000,]
  colnames(msarnambi) <- c("R","G","B")

# Algoritmo para descobrir qual pixel corresponde a fundo e qual a objetos ---------

# Vamos colocar uma matriz sobre a outra
  Mat1 <- rbind(cbind(mfundo, 1), # Fará de preto o fundo
                cbind(msarnambi,0), # Objeto de interesse, será branco
                cbind(mref,0))    # Objeto2 de interesse, fazendo-o branco
  colnames(Mat1)[4] <- "Y"
  Mat1 <- data.frame(Mat1)

# Vamos criar modelo de predição dos objetos a partir do dataframe mat1
  modelo1 <- glm(Y ~ R+G+B,data = Mat1, family = binomial("logit"))

# vamos predizer a imagem em função do modelo1 ---------------------------------
  # Não precisa aleatorizar e vamos usar todos os pixels da imagem
  Mim <- cbind(c(im@.Data[,,1]),
               c(im@.Data[,,2]),
               c(im@.Data[,,3]))
  colnames(Mim) <- c("R","G","B")
  Mim <- data.frame(Mim)
  
  
  # Predizendo para cada um dos 3 objetos sobre a imagem se é valor 1 ou 0
  pred1 <- predict(modelo1,newdata = Mim, type = "response")
  pred1 <- round(pred1,0)
  # pred1 é um vetor?
  is.vector(pred1)
  
  # Converte o vetor em matriz para poder gerar imagem
  MPred1 <- matrix(pred1, ncol = ncol(im@.Data[,,1]))
  im2 <- im
  im2@.Data <- MPred1 # A imagem será substituída pelos valores da matriz de 1 e 0
  
  plot(im2) # A referencia e as sarnambi se destacam do fundo
  # Modelo conseguiu separa o que é fundo do restante
  
# Criar outro modelo para separar a imagem de ref do que é sarnambi ---------------

  Mat2 <- rbind(cbind(msarnambi,0), 
                cbind(mref,1))
  colnames(Mat2) <- c("R","G","B","Y")
  Mat2 <- data.frame(Mat2)
  modelo2 <- glm(Y~R+G+B, family = binomial("logit"), data = Mat2)


# Criando uma terceira matriz para separar a folha do fundo e da ref --------------
  # Vamos criar uma ID para os nossos objetos a fim de extraí-los da imagem im
  ID <- MPred1==0 # faz com que crie uma matriz de valores lógicos TRUE e FALSE
  
  Mobj <- cbind(cbind(im@.Data[,,1][ID]), # Pega os valores de R para folha e Ref
                cbind(im@.Data[,,2][ID]), # Pega os valores de G para folha e Ref
                cbind(im@.Data[,,3][ID]))
  colnames(Mobj) <- c("R","G","B")
  Mobj <- data.frame(Mobj)
  # Observe o quanto a matriz Mobj é menor que a Mim em termos de obs. Isto porque
  # o fundo é muito maior que os objetos como podemos ver:
  plot(im)
  nrow(Mobj)/nrow(Mim)  # Aprox 45.6%
  
  # Predizendo o que é Referencia de concha dentro da matriz de objetos: Mobj
  pred2 <- predict(modelo2,newdata = Mobj,type="response")
  pred2 <- round(pred2,0)
  
  # Cálculo do número de pixel da imagem de referencia:
  NumPixelRef <- sum(pred2==1)
  
  # ?EBImage
  MPred1b <- bwlabel(MPred1==0) # Serve para fazer a seguimentação binária
  Shape <- computeFeatures.shape(MPred1b) # Obtém a área
  ID2 <- Shape[,1]>1200  # Valores acima de 1.000 para excluir ruídos
  summary(ID2) # Observe que pega 30 objetos (29 sarnambi e a ref.)
  
  
  Area <- Shape[ID2,1] # Seleciona linhas dos objetos de interesse e a apenas a col. 1
                       # s.area (area em pixel)

# Área corrigida para cm²
# Objeto de referencia (cartão vermelho): 4.53 cm²
  base = 3.01
  altura = 1.505
  fator_conversao_area = (base*altura)/NumPixelRef
  AreaCor <- Area*fator_conversao_area
  
  # Obtendo as coordenadas dos objetos
  Coord <- computeFeatures.moment(MPred1b) # Pega a coordenada do ponto de massa
  Coord <- Coord[ID2,]  # Pega coordenada só do que nos interessa
  
  plot(im)
  text(Coord[,1],Coord[,2], round(AreaCor,2 ), col = "black", cex=0.8)

  
 # Calculo dos diametros máximos ---------------------------------------------
  
  # Supondo que já tenha MPred1b e a lista de objetos filtrados por área
  Shape <- computeFeatures.shape(MPred1b)
  ID2 <- Shape[,"s.area"] > 1200
  Shape_filt <- Shape[ID2, ]
  
  # Extrair o raio máximo
  max_radius_pixels <- Shape_filt[,"s.radius.max"]
  
  # Coordenadas do centro dos objetos
  Coord <- computeFeatures.moment(MPred1b)
  Coord <- Coord[ID2,]
  
  # Área real da referência (em cm²)
  AreaReal <- 3.01 * 1.505 # = 4.53 cm²
  
  # NumPixelRef já calculado no código anterior (quantidade de pixels da referência)
  # Por exemplo, poderia ser:
  # NumPixelRef <- sum(pred2 == 1)
  
  # Fator de conversão: pixel² -> cm²
  fator_area <- AreaReal / NumPixelRef
  
  # Fator de conversão: pixel -> cm
  fator_linear <- sqrt(fator_area)
  
  # Converter s.radius.max de pixels para cm
  radius_cm <- max_radius_pixels * fator_linear
  diameter_cm <- 2 * radius_cm
  
  # Plotar os diâmetros em cm sobre a imagem
  plot(im)
  text(Coord[,1], Coord[,2], round(diameter_cm, 2), col="blue", cex=0.8)
  
  
  
  # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  
  
  # correlacionando peso e área
  area_peso <- as.data.frame(AreaCor)
  library(openxlsx)
  write.xlsx(x = area_peso, "area_peso_exp.xlsx")
  