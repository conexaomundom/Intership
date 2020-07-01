rm(list = ls()) 

# install.packages("readODS")
# library(readODS)


install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")
install.packages("urca")
install.packages("astsa")
install.packages("gridExtra")

# Ler o arquivo de dados

# local = file.choose()
# local <- tclvalue(tkgetOpenFile(title="Abrir Banco de Dados"))
# dados <- read.table(file = local, header=TRUE, dec=".")

banco <- Precos_Cesta_Basica_Procon_Novo
attach(banco)
names(banco)
# "IE"  "documento"  "razaoSocial" "dtEmissao"  "cdgtin"    "DescrProduto"  "precoMedioDia"
banco

banco$IE <- factor(banco$IE)
ie <- levels(banco$IE) 
length(levels(banco$IE)) # 14
# "160014646" "160659191" "161086772" "161255914" 
# "161279198" "161320902" "161364330" "161519008" 
# "161538452" "161588964" "161590608" "161595685"
# "162190883" "162664192"

banco$documento <- factor(banco$documento)
levels(banco$documento)
length(levels(banco$documento)) # 14
# "00395042000179" "04528825000134" "05272461000137" "08414996000355"
# "08824586000110" "09143892000154" "09148787000108" "13004510025335"
# "18458237000186" "23708372000172" "45543915044120" "47508411032007"
# "47508411114402" "75315333007464"

banco$razaoSocial <- factor(banco$razaoSocial)
levels(banco$razaoSocial)
length(levels(banco$razaoSocial))




# banco[banco$razaoSocial == 1]
banco1 <- banco[which(banco$razaoSocial == 1), ]
igual_diferente <- which(banco1$IE !=160014646)

# separando os alimentos em levels # DescrProduto
banco$DescrProduto <- factor(banco$DescrProduto)
produto <- levels(banco$DescrProduto)


# Datas dtEmissao ---------------------------------------------------------
banco$dtEmissao <- factor(banco$dtEmissao)
datas <- levels(banco$dtEmissao)


# Estabelecimentos, b e prod ----------------------------------------------
# Estabelecimento 1
b1 <- banco[which(banco$IE == ie[1]), ]
prod1 <- levels(b1$DescrProduto)
# Estabelecimento 1
b2 <- banco[which(banco$IE == ie[2]), ]
prod2 <- levels(b2$DescrProduto)
# Estabelecimento 3
b3 <- banco[which(banco$IE == ie[3]), ]
prod3 <- levels(b3$DescrProduto)
# Estabelecimento 4
b4 <- banco[which(banco$IE == ie[4]), ]
prod4 <- levels(b4$DescrProduto)
# Estabelecimento 5
b5 <- banco[which(banco$IE == ie[5]), ]
prod5 <- levels(b5$DescrProduto)
# Estabelecimento 6
b6 <- banco[which(banco$IE == ie[6]), ]
prod6 <- levels(b6$DescrProduto)
# Estabelecimento 7
b7 <- banco[which(banco$IE == ie[7]), ]
prod7 <- levels(b7$DescrProduto)
# Estabelecimento 8
b8 <- banco[which(banco$IE == ie[8]), ]
prod8 <- levels(b8$DescrProduto)
# Estabelecimento 9
b9 <- banco[which(banco$IE == ie[9]), ]
prod9 <- levels(b9$DescrProduto)
# Estabelecimento 10
b10 <- banco[which(banco$IE == ie[10]), ]
prod10 <- levels(b10$DescrProduto)
# Estabelecimento 11
b11 <- banco[which(banco$IE == ie[11]), ]
prod11 <- levels(b11$DescrProduto)
# Estabelecimento 12
b12 <- banco[which(banco$IE == ie[12]), ]
prod12 <- levels(b12$DescrProduto)
# Estabelecimento 13
b13 <- banco[which(banco$IE == ie[13]), ]
prod13 <- levels(b13$DescrProduto)
# Estabelecimento 4
b14 <- banco[which(banco$IE == ie[4]), ]
prod14 <- levels(b14$DescrProduto)


avaliacao <- function(nestab, percents){
  vetor_media <- NULL
  vetor_variancia <- NULL
  variacao <- NULL
  dados <- banco[which(banco$IE == ie[nestab]), ]
  prod <- levels(dados$DescrProduto)
  
  for(i in 1:length(prod)){

    vetor_media[i] <- mean(as.numeric(dados[which(dados$DescrProduto == prod[i]), ]$precoMedioDia))
    vetor_variancia[i] <- var(dados[which(dados$DescrProduto == prod[i]), ]$precoMedioDia)
    #summary()
    variacao[i] <- round(100*(max(as.numeric(dados[which(dados$DescrProduto == prod[i]), ]$precoMedioDia)) - min(as.numeric(dados[which(dados$DescrProduto == prod[i]), ]$precoMedioDia)))/ min(as.numeric(dados[which(dados$DescrProduto == prod[i]), ]$precoMedioDia)),2)
  }
  prod[which(variacao >= percents)]
  

}

for(j in 1:14) print(avaliacao(j, percents = 65))

# Estabelecimentos

###################################################################
# variação maior que 80% foram apenas dois estabelecimentos com 3 produtos.
# estabelecimento 4 e estabelecimento 10.
# CAFE_PILAO_ALMOFADA_250G_TRADICIONAL "LEITE_UHT_PIRACANJUBA_1LT_INTEGRAL_CTAMPA" # "MARGARINA_CSAL_DELICIA_500G"








###########################################################################
###########################################################################
###########################################################################
# Gráficos ----------------------------------------------------------------
library(ggplot2)
require(gridExtra)


###########################################################################
# CAFE_PILAO_ALMOFADA_250G_TRADICIONAL ------------------------------------
item1 <- "CAFE_PILAO_ALMOFADA_250G_TRADICIONAL"

# Algumas estatisticas descritivas sobre o item CAFE_PILAO_ALMOFADA_250G_TRADICIONAL
banco[which(banco$DescrProduto == item1), ]$precoMedioDia
summary(as.numeric(banco[which(banco$DescrProduto == item1), ]$precoMedioDia))
minimo1 <- min(as.numeric(banco[which(banco$DescrProduto == item1), ]$precoMedioDia))
maximo1 <- max(as.numeric(banco[which(banco$DescrProduto == item1), ]$precoMedioDia))
###########################################################################


# estab1 ------------------------------------------------------------------
y1 <- as.numeric(b1[which(b1$DescrProduto == prod1[which(prod1 == item1)]), ]$precoMedioDia)
summary(y1)
data1 <- b1[which(b1$DescrProduto == prod1[which(prod1 == item1)]), ]$dtEmissao

a1 <- ggplot(data = b1[which(b1$DescrProduto == prod1[which(prod1 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item1)]), ])), y = y1), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item1)]), ])), y = rep(mean(y1), length(y1))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab1", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a1

# estab2 ------------------------------------------------------------------
y2 <- as.numeric(b2[which(b2$DescrProduto == prod2[which(prod2 == item1)]), ]$precoMedioDia)
data2 <- b2[which(b2$DescrProduto == prod2[which(prod2 == item1)]), ]$dtEmissao

a2 <- ggplot(data = b2[which(b2$DescrProduto == prod2[which(prod2 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item1)]), ])), y = y2), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item1)]), ])), y = rep(mean(y2), length(y2))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab2", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a2

# grid.arrange(a1, a2, ncol=2)

# estab3 ------------------------------------------------------------------
y3 <- as.numeric(b3[which(b3$DescrProduto == prod3[which(prod3 == item1)]), ]$precoMedioDia)
data3 <- b3[which(b3$DescrProduto == prod3[which(prod3 == item1)]), ]$dtEmissao

a3 <- ggplot(data = b3[which(b3$DescrProduto == prod3[which(prod3 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item1)]), ])), y = y3), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item1)]), ])), y = rep(mean(y3), length(y3))), size = 0.3, col = 2) +
  labs(subtitle="Supermercados Manaíra LTDA", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a3

# estab4 ------------------------------------------------------------------
y4 <- as.numeric(b4[which(b4$DescrProduto == prod4[which(prod4 == item1)]), ]$precoMedioDia)
data4 <- b4[which(b4$DescrProduto == prod4[which(prod4 == item1)]), ]$dtEmissao

a4 <- ggplot(data = b4[which(b4$DescrProduto == prod4[which(prod4 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item1)]), ])), y = y4), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item1)]), ])), y = rep(mean(y4), length(y4))), size = 0.3, col = 2) +
  labs(subtitle="Jacto Comercio de Alimentos LTDA", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a4

pdf("/home/conexaomundo/CAFE_PILAO_ALMOFADA_250G_TRADICIONAL1")
grid.arrange(a1, a2, a3, a4, ncol=2)
dev.off()

# estab5 ------------------------------------------------------------------
y5 <- as.numeric(b5[which(b5$DescrProduto == prod5[which(prod5 == item1)]), ]$precoMedioDia)
data5 <- b5[which(b5$DescrProduto == prod5[which(prod5 == item1)]), ]$dtEmissao

a5 <- ggplot(data = b5[which(b5$DescrProduto == prod5[which(prod5 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b5[which(b5$DescrProduto == prod5[which(prod5 == item1)]), ])), y = y5), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b5[which(b5$DescrProduto == prod5[which(prod5 == item1)]), ])), y = rep(mean(y5), length(y5))), size = 0.3, col = 2) +
  labs(subtitle="Bompreço supermercados LTDA", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a5


# estab7 ------------------------------------------------------------------
y7 <- as.numeric(b7[which(b7$DescrProduto == prod7[which(prod7 == item1)]), ]$precoMedioDia)
data7 <- b7[which(b7$DescrProduto == prod7[which(prod7 == item1)]), ]$dtEmissao

a7 <- ggplot(data = b7[which(b7$DescrProduto == prod7[which(prod7 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item1)]), ])), y = y7), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item1)]), ])), y = rep(mean(y7), length(y7))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Latorre LTDA", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a7

# estab8 ------------------------------------------------------------------
y8 <- as.numeric(b8[which(b8$DescrProduto == prod8[which(prod8 == item1)]), ]$precoMedioDia)
data8 <- b8[which(b8$DescrProduto == prod8[which(prod8 == item1)]), ]$dtEmissao

a8 <- ggplot(data = b8[which(b8$DescrProduto == prod8[which(prod8 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item1)]), ])), y = y8), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item1)]), ])), y = rep(mean(y8), length(y8))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Colibris LTDA-EPP", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a8

# estab9 ------------------------------------------------------------------
y9 <- as.numeric(b9[which(b9$DescrProduto == prod9[which(prod9 == item1)]), ]$precoMedioDia)
data9 <- b9[which(b9$DescrProduto == prod9[which(prod9 == item1)]), ]$dtEmissao

a9 <- ggplot(data = b9[which(b9$DescrProduto == prod9[which(prod9 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item1)]), ])), y = y9), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item1)]), ])), y = rep(mean(y9), length(y9))), size = 0.3, col = 2) +
  labs(subtitle="Comercial de Alimentos E J C LTDA", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a9

pdf("/home/conexaomundo/CAFE_PILAO_ALMOFADA_250G_TRADICIONAL2")
grid.arrange(a5, a7, a8, a9, ncol=2)
dev.off()


# estab10 ------------------------------------------------------------------
y10 <- as.numeric(b10[which(b10$DescrProduto == prod10[which(prod10 == item1)]), ]$precoMedioDia)
data10 <- b10[which(b10$DescrProduto == prod10[which(prod10 == item1)]), ]$dtEmissao

a10 <- ggplot(data = b10[which(b10$DescrProduto == prod10[which(prod10 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item1)]), ])), y = y10), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item1)]), ])), y = rep(mean(y10), length(y10))), size = 0.3, col = 2) +
  labs(subtitle="Carrefour Comercio e Industria LTDA", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a10

# estab12 ------------------------------------------------------------------
y12 <- as.numeric(b12[which(b12$DescrProduto == prod12[which(prod12 == item1)]), ]$precoMedioDia)
data12 <- b12[which(b12$DescrProduto == prod12[which(prod12 == item1)]), ]$dtEmissao

a12 <- ggplot(data = b12[which(b12$DescrProduto == prod12[which(prod12 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item1)]), ])), y = y12), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item1)]), ])), y = rep(mean(y12), length(y12))), size = 0.3, col = 2) +
  labs(subtitle="Atacadão S.A.", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a12

# estab14 ------------------------------------------------------------------
y14 <- as.numeric(b14[which(b14$DescrProduto == prod14[which(prod14 == item1)]), ]$precoMedioDia)
data14 <- b14[which(b14$DescrProduto == prod14[which(prod14 == item1)]), ]$dtEmissao

a14 <- ggplot(data = b14[which(b14$DescrProduto == prod14[which(prod14 == item1)]), ]) + coord_cartesian(ylim = c(minimo1, maximo1)) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item1)]), ])), y = y14), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item1)]), ])), y = rep(mean(y14), length(y14))), size = 0.3, col = 2) +
  labs(subtitle="Comercio Varejista de Alimentos Verde Vale LTDA", 
       y="Preço", 
       x="Dias", 
       title="Café Pilão Tradicional Almofada 200g")

a14

pdf("/home/conexaomundo/CAFE_PILAO_ALMOFADA_250G_TRADICIONAL3")
grid.arrange(a10, a12, a14, ncol=2)
dev.off()










###########################################################################
# LEITE_UHT_PIRACANJUBA_1LT_INTEGRAL_CTAMPA -------------------------------
item2 <- "LEITE_UHT_PIRACANJUBA_1LT_INTEGRAL_CTAMPA"

# Algumas estatisticas descritivas sobre o item LEITE_UHT_PIRACANJUBA_1LT_INTEGRAL_CTAMPA
banco[which(banco$DescrProduto == item2), ]$precoMedioDia
summary(as.numeric(banco[which(banco$DescrProduto == item2), ]$precoMedioDia))
minimo2 <- min(as.numeric(banco[which(banco$DescrProduto == item2), ]$precoMedioDia))
maximo2 <- max(as.numeric(banco[which(banco$DescrProduto == item2), ]$precoMedioDia))
###########################################################################


# estab1 ------------------------------------------------------------------
y1 <- as.numeric(b1[which(b1$DescrProduto == prod1[which(prod1 == item2)]), ]$precoMedioDia)
data1 <- b1[which(b1$DescrProduto == prod1[which(prod1 == item2)]), ]$dtEmissao

a1 <- ggplot(data = b1[which(b1$DescrProduto == prod1[which(prod1 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item2)]), ])), y = y1), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item2)]), ])), y = rep(mean(y1), length(y1))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab1", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a1

# estab2 ------------------------------------------------------------------
y2 <- as.numeric(b2[which(b2$DescrProduto == prod2[which(prod2 == item2)]), ]$precoMedioDia)
data2 <- b2[which(b2$DescrProduto == prod2[which(prod2 == item2)]), ]$dtEmissao

a2 <- ggplot(data = b2[which(b2$DescrProduto == prod2[which(prod2 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item2)]), ])), y = y2), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item2)]), ])), y = rep(mean(y2), length(y2))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab2", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a2


# estab3 ------------------------------------------------------------------
y3 <- as.numeric(b3[which(b3$DescrProduto == prod3[which(prod3 == item2)]), ]$precoMedioDia)
data3 <- b3[which(b3$DescrProduto == prod3[which(prod3 == item2)]), ]$dtEmissao

a3 <- ggplot(data = b3[which(b3$DescrProduto == prod3[which(prod3 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item2)]), ])), y = y3), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item2)]), ])), y = rep(mean(y3), length(y3))), size = 0.3, col = 2) +
  labs(subtitle="Supermercados Manaíra LTDA", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a3

# estab4 ------------------------------------------------------------------
y4 <- as.numeric(b4[which(b4$DescrProduto == prod4[which(prod4 == item2)]), ]$precoMedioDia)
data4 <- b4[which(b4$DescrProduto == prod4[which(prod4 == item2)]), ]$dtEmissao

a4 <- ggplot(data = b4[which(b4$DescrProduto == prod4[which(prod4 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item2)]), ])), y = y4), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item2)]), ])), y = rep(mean(y4), length(y4))), size = 0.3, col = 2) +
  labs(subtitle="Jacto Comercio de Alimentos LTDA", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a4

pdf("/home/conexaomundo/LEITE_UHT_PIRACANJUBA_1LT_INTEGRAL_CTAMPA1")
grid.arrange(a1, a2, a3, a4, ncol=2)
dev.off()



# estab5 ------------------------------------------------------------------
y5 <- as.numeric(b5[which(b5$DescrProduto == prod5[which(prod5 == item2)]), ]$precoMedioDia)
data5 <- b5[which(b5$DescrProduto == prod5[which(prod5 == item2)]), ]$dtEmissao

a5 <- ggplot(data = b5[which(b5$DescrProduto == prod5[which(prod5 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b5[which(b5$DescrProduto == prod5[which(prod5 == item2)]), ])), y = y5), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b5[which(b5$DescrProduto == prod5[which(prod5 == item2)]), ])), y = rep(mean(y5), length(y5))), size = 0.3, col = 2) +
  labs(subtitle="Bompreço supermercados LTDA", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a5


# estab7 ------------------------------------------------------------------
y7 <- as.numeric(b7[which(b7$DescrProduto == prod7[which(prod7 == item2)]), ]$precoMedioDia)
data7 <- b7[which(b7$DescrProduto == prod7[which(prod7 == item2)]), ]$dtEmissao

a7 <- ggplot(data = b7[which(b7$DescrProduto == prod7[which(prod7 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item2)]), ])), y = y7), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item2)]), ])), y = rep(mean(y7), length(y7))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Latorre LTDA", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a7

# estab8 ------------------------------------------------------------------
y8 <- as.numeric(b8[which(b8$DescrProduto == prod8[which(prod8 == item2)]), ]$precoMedioDia)
data8 <- b8[which(b8$DescrProduto == prod8[which(prod8 == item2)]), ]$dtEmissao

a8 <- ggplot(data = b8[which(b8$DescrProduto == prod8[which(prod8 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item2)]), ])), y = y8), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item2)]), ])), y = rep(mean(y8), length(y8))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Colibris LTDA-EPP", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a8

# estab9 ------------------------------------------------------------------
y9 <- as.numeric(b9[which(b9$DescrProduto == prod9[which(prod9 == item2)]), ]$precoMedioDia)
data9 <- b9[which(b9$DescrProduto == prod9[which(prod9 == item2)]), ]$dtEmissao

a9 <- ggplot(data = b9[which(b9$DescrProduto == prod9[which(prod9 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item2)]), ])), y = y9), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item2)]), ])), y = rep(mean(y9), length(y9))), size = 0.3, col = 2) +
  labs(subtitle="Comercial de Alimentos E J C LTDA", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a9

pdf("/home/conexaomundo/LEITE_UHT_PIRACANJUBA_1LT_INTEGRAL_CTAMPA2")
grid.arrange(a5, a7, a8, a9, ncol=2)
dev.off()


# estab10 ------------------------------------------------------------------
y10 <- as.numeric(b10[which(b10$DescrProduto == prod10[which(prod10 == item2)]), ]$precoMedioDia)
data10 <- b10[which(b10$DescrProduto == prod10[which(prod10 == item2)]), ]$dtEmissao

a10 <- ggplot(data = b10[which(b10$DescrProduto == prod10[which(prod10 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item2)]), ])), y = y10), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item2)]), ])), y = rep(mean(y10), length(y10))), size = 0.3, col = 2) +
  labs(subtitle="Carrefour Comercio e Industria LTDA", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a10

# estab11 ------------------------------------------------------------------
y11 <- as.numeric(b11[which(b11$DescrProduto == prod11[which(prod11 == item2)]), ]$precoMedioDia)
data11 <- b11[which(b11$DescrProduto == prod11[which(prod11 == item2)]), ]$dtEmissao

a11 <- ggplot(data = b11[which(b11$DescrProduto == prod11[which(prod11 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item2)]), ])), y = y11), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item2)]), ])), y = rep(mean(y11), length(y11))), size = 0.3, col = 2) +
  labs(subtitle="Rede Menor Preço Supermercado LTDA", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a11

# estab12 ------------------------------------------------------------------
y12 <- as.numeric(b12[which(b12$DescrProduto == prod12[which(prod12 == item2)]), ]$precoMedioDia)
data12 <- b12[which(b12$DescrProduto == prod12[which(prod12 == item2)]), ]$dtEmissao

a12 <- ggplot(data = b12[which(b12$DescrProduto == prod12[which(prod12 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item2)]), ])), y = y12), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item2)]), ])), y = rep(mean(y12), length(y12))), size = 0.3, col = 2) +
  labs(subtitle="Atacadão S.A.", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L", 
       caption = "Source: midwest")

a12


# estab14 ------------------------------------------------------------------
y14 <- as.numeric(b14[which(b14$DescrProduto == prod14[which(prod14 == item2)]), ]$precoMedioDia)
data14 <- b14[which(b14$DescrProduto == prod14[which(prod14 == item2)]), ]$dtEmissao

a14 <- ggplot(data = b14[which(b14$DescrProduto == prod14[which(prod14 == item2)]), ]) + coord_cartesian(ylim = c(minimo2, maximo2)) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item2)]), ])), y = y14), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item2)]), ])), y = rep(mean(y14), length(y14))), size = 0.3, col = 2) +
  labs(subtitle="Comercio Varejista de Alimentos Verde Vale LTDA", 
       y="Preço", 
       x="Dias", 
       title="Leite UHT Integral Piracanjuba C/Tampa 1L")

a14

pdf("/home/conexaomundo/LEITE_UHT_PIRACANJUBA_1LT_INTEGRAL_CTAMPA3")
grid.arrange(a10, a11, a12, a14, ncol=2)
dev.off()





# MARGARINA_CSAL_DELICIA_500G ---------------------------------------------
item3 <- "MARGARINA_CSAL_DELICIA_500G"
# Algumas estatisticas descritivas sobre o item MARGARINA_CSAL_DELICIA_500G
banco[which(banco$DescrProduto == item3), ]$precoMedioDia
summary(as.numeric(banco[which(banco$DescrProduto == item3), ]$precoMedioDia))
minimo3 <- min(as.numeric(banco[which(banco$DescrProduto == item3), ]$precoMedioDia))
maximo3 <- max(as.numeric(banco[which(banco$DescrProduto == item3), ]$precoMedioDia))
###########################################################################


# estab1 ------------------------------------------------------------------
y1 <- as.numeric(b1[which(b1$DescrProduto == prod1[which(prod1 == item3)]), ]$precoMedioDia)
data1 <- b1[which(b1$DescrProduto == prod1[which(prod1 == item3)]), ]$dtEmissao

a1 <- ggplot(data = b1[which(b1$DescrProduto == prod1[which(prod1 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item3)]), ])), y = y1), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item3)]), ])), y = rep(mean(y1), length(y1))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab1", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a1

# estab2 ------------------------------------------------------------------
y2 <- as.numeric(b2[which(b2$DescrProduto == prod2[which(prod2 == item3)]), ]$precoMedioDia)
data2 <- b2[which(b2$DescrProduto == prod2[which(prod2 == item3)]), ]$dtEmissao

a2 <- ggplot(data = b2[which(b2$DescrProduto == prod2[which(prod2 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item3)]), ])), y = y2), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item3)]), ])), y = rep(mean(y2), length(y2))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab2", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a2


# estab3 ------------------------------------------------------------------
y3 <- as.numeric(b3[which(b3$DescrProduto == prod3[which(prod3 == item3)]), ]$precoMedioDia)
data3 <- b3[which(b3$DescrProduto == prod3[which(prod3 == item3)]), ]$dtEmissao

a3 <- ggplot(data = b3[which(b3$DescrProduto == prod3[which(prod3 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item3)]), ])), y = y3), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item3)]), ])), y = rep(mean(y3), length(y3))), size = 0.3, col = 2) +
  labs(subtitle="Supermercados Manaíra LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g", 
       caption = "Source: midwest")

a3

# estab4 ------------------------------------------------------------------
y4 <- as.numeric(b4[which(b4$DescrProduto == prod4[which(prod4 == item3)]), ]$precoMedioDia)
data4 <- b4[which(b4$DescrProduto == prod4[which(prod4 == item3)]), ]$dtEmissao

a4 <- ggplot(data = b4[which(b4$DescrProduto == prod4[which(prod4 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item3)]), ])), y = y4), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item3)]), ])), y = rep(mean(y4), length(y4))), size = 0.3, col = 2) +
  labs(subtitle="Jacto Comercio de Alimentos LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a4

pdf("/home/conexaomundo/MARGARINA_CSAL_DELICIA_500G1")
grid.arrange(a1, a2, a3, a4, ncol=2)
dev.off()



# estab5 ------------------------------------------------------------------
y5 <- as.numeric(b5[which(b5$DescrProduto == prod5[which(prod5 == item3)]), ]$precoMedioDia)
data5 <- b5[which(b5$DescrProduto == prod5[which(prod5 == item3)]), ]$dtEmissao

a5 <- ggplot(data = b5[which(b5$DescrProduto == prod5[which(prod5 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b5[which(b5$DescrProduto == prod5[which(prod5 == item3)]), ])), y = y5), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b5[which(b5$DescrProduto == prod5[which(prod5 == item3)]), ])), y = rep(mean(y5), length(y5))), size = 0.3, col = 2) +
  labs(subtitle="Bompreço supermercados LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a5

# estab6 ------------------------------------------------------------------
y6 <- as.numeric(b6[which(b6$DescrProduto == prod6[which(prod6 == item3)]), ]$precoMedioDia)
data6 <- b6[which(b6$DescrProduto == prod6[which(prod6 == item3)]), ]$dtEmissao

a6 <- ggplot(data = b6[which(b6$DescrProduto == prod6[which(prod6 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item3)]), ])), y = y6), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item3)]), ])), y = rep(mean(y6), length(y6))), size = 0.3, col = 2) +
  labs(subtitle="Destakão Magazine LTDA", 
       y="Preço", 
      x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a6

# estab7 ------------------------------------------------------------------
y7 <- as.numeric(b7[which(b7$DescrProduto == prod7[which(prod7 == item3)]), ]$precoMedioDia)
data7 <- b7[which(b7$DescrProduto == prod7[which(prod7 == item3)]), ]$dtEmissao

a7 <- ggplot(data = b7[which(b7$DescrProduto == prod7[which(prod7 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item3)]), ])), y = y7), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item3)]), ])), y = rep(mean(y7), length(y7))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Latorre LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a7

# estab8 ------------------------------------------------------------------
y8 <- as.numeric(b8[which(b8$DescrProduto == prod8[which(prod8 == item3)]), ]$precoMedioDia)
data8 <- b8[which(b8$DescrProduto == prod8[which(prod8 == item3)]), ]$dtEmissao

a8 <- ggplot(data = b8[which(b8$DescrProduto == prod8[which(prod8 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item3)]), ])), y = y8), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item3)]), ])), y = rep(mean(y8), length(y8))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Colibris LTDA-EPP", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a8

pdf("/home/conexaomundo/MARGARINA_CSAL_DELICIA_500G3")
grid.arrange(a5, a6, a7, a8, ncol=2)
dev.off()




# estab9 ------------------------------------------------------------------
y9 <- as.numeric(b9[which(b9$DescrProduto == prod9[which(prod9 == item3)]), ]$precoMedioDia)
data9 <- b9[which(b9$DescrProduto == prod9[which(prod9 == item3)]), ]$dtEmissao

a9 <- ggplot(data = b9[which(b9$DescrProduto == prod9[which(prod9 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item3)]), ])), y = y9), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item3)]), ])), y = rep(mean(y9), length(y9))), size = 0.3, col = 2) +
  labs(subtitle="Comercial de Alimentos E J C LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a9

# estab10 ------------------------------------------------------------------
y10 <- as.numeric(b10[which(b10$DescrProduto == prod10[which(prod10 == item3)]), ]$precoMedioDia)
data10 <- b10[which(b10$DescrProduto == prod10[which(prod10 == item3)]), ]$dtEmissao

a10 <- ggplot(data = b10[which(b10$DescrProduto == prod10[which(prod10 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item3)]), ])), y = y10), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item3)]), ])), y = rep(mean(y10), length(y10))), size = 0.3, col = 2) +
  labs(subtitle="Carrefour Comercio e Industria LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a10

# estab11 ------------------------------------------------------------------
y11 <- as.numeric(b11[which(b11$DescrProduto == prod11[which(prod11 == item3)]), ]$precoMedioDia)
data11 <- b11[which(b11$DescrProduto == prod11[which(prod11 == item3)]), ]$dtEmissao

a11 <- ggplot(data = b11[which(b11$DescrProduto == prod11[which(prod11 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item3)]), ])), y = y11), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item3)]), ])), y = rep(mean(y11), length(y11))), size = 0.3, col = 2) +
  labs(subtitle="Rede Menor Preço Supermercado LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a11

# estab12 ------------------------------------------------------------------
y12 <- as.numeric(b12[which(b12$DescrProduto == prod12[which(prod12 == item3)]), ]$precoMedioDia)
data12 <- b12[which(b12$DescrProduto == prod12[which(prod12 == item3)]), ]$dtEmissao

a12 <- ggplot(data = b12[which(b12$DescrProduto == prod12[which(prod12 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item3)]), ])), y = y12), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item3)]), ])), y = rep(mean(y12), length(y12))), size = 0.3, col = 2) +
  labs(subtitle="Atacadão S.A.", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a12

pdf("/home/conexaomundo/MARGARINA_CSAL_DELICIA_500G3")
grid.arrange(a9, a10, a11, a12, ncol=2)
dev.off()


# estab13 ------------------------------------------------------------------
y13 <- as.numeric(b13[which(b13$DescrProduto == prod13[which(prod13 == item3)]), ]$precoMedioDia)
data13 <- b13[which(b13$DescrProduto == prod13[which(prod13 == item3)]), ]$dtEmissao

a13 <- ggplot(data = b13[which(b13$DescrProduto == prod13[which(prod13 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b13[which(b13$DescrProduto == prod13[which(prod13 == item3)]), ])), y = y13), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b13[which(b13$DescrProduto == prod13[which(prod13 == item3)]), ])), y = rep(mean(y13), length(y13))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado e Comercio Varejista Classe A LTDA EPP", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a13

# estab14 ------------------------------------------------------------------
y14 <- as.numeric(b14[which(b14$DescrProduto == prod14[which(prod14 == item3)]), ]$precoMedioDia)
data14 <- b14[which(b14$DescrProduto == prod14[which(prod14 == item3)]), ]$dtEmissao

a14 <- ggplot(data = b14[which(b14$DescrProduto == prod14[which(prod14 == item3)]), ]) + coord_cartesian(ylim = c(minimo3, maximo3)) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item3)]), ])), y = y14), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item3)]), ])), y = rep(mean(y14), length(y14))), size = 0.3, col = 2) +
  labs(subtitle="Comercio Varejista de Alimentos Verde Vale LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Delícia 500g")

a14

pdf("/home/conexaomundo/MARGARINA_CSAL_DELICIA_500G4")
grid.arrange(a13, a14, ncol=2)
dev.off()
















# [3] "MARGARINA_CSAL_QUALY_500G_MQUS"  
item4 <- "MARGARINA_CSAL_QUALY_500G_MQUS"
# Algumas estatisticas descritivas sobre o item MARGARINA_CSAL_QUALY_500G_MQUS
banco[which(banco$DescrProduto == item4), ]$precoMedioDia
summary(as.numeric(banco[which(banco$DescrProduto == item4), ]$precoMedioDia))
minimo4 <- min(as.numeric(banco[which(banco$DescrProduto == item4), ]$precoMedioDia))
maximo4 <- max(as.numeric(banco[which(banco$DescrProduto == item4), ]$precoMedioDia))
###########################################################################


# estab1 ------------------------------------------------------------------
y1 <- as.numeric(b1[which(b1$DescrProduto == prod1[which(prod1 == item4)]), ]$precoMedioDia)
data1 <- b1[which(b1$DescrProduto == prod1[which(prod1 == item4)]), ]$dtEmissao

a1 <- ggplot(data = b1[which(b1$DescrProduto == prod1[which(prod1 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item4)]), ])), y = y1), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item4)]), ])), y = rep(mean(y1), length(y1))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab1", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a1

# estab2 ------------------------------------------------------------------
y2 <- as.numeric(b2[which(b2$DescrProduto == prod2[which(prod2 == item4)]), ]$precoMedioDia)
data2 <- b2[which(b2$DescrProduto == prod2[which(prod2 == item4)]), ]$dtEmissao

a2 <- ggplot(data = b2[which(b2$DescrProduto == prod2[which(prod2 == item4)]), ]) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item4)]), ])), y = y2), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item4)]), ])), y = rep(mean(y2), length(y2))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab2", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a2


# estab3 ------------------------------------------------------------------
y3 <- as.numeric(b3[which(b3$DescrProduto == prod3[which(prod3 == item4)]), ]$precoMedioDia)
data3 <- b3[which(b3$DescrProduto == prod3[which(prod3 == item4)]), ]$dtEmissao
# length(data3)/10

# data33 <- c(data3[1], rep(" ", 1), data1[12],  rep(" ", 1), data1[23],  rep(" ", 1), data1[34], rep(" ", 1), data1[45], rep(" ", 1), data1[56],  rep(" ", 1), data1[67], rep(" ", 1), data1[78], rep(" ", 6))

a3 <- ggplot(data = b3[which(b3$DescrProduto == prod3[which(prod3 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item4)]), ])), y = y3), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item4)]), ])), y = rep(mean(y3), length(y3))), size = 0.3, col = 2) +
  labs(subtitle="Supermercados Manaíra LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a3

# estab4 ------------------------------------------------------------------

y4 <- as.numeric(b4[which(b4$DescrProduto == prod4[which(prod4 == item4)]), ]$precoMedioDia)
data4 <- b4[which(b4$DescrProduto == prod4[which(prod4 == item4)]), ]$dtEmissao
# length(data4)/10

# data33 <- c(data3[1], rep(" ", 1), data1[12],  rep(" ", 1), data1[23],  rep(" ", 1), data1[34], rep(" ", 1), data1[45], rep(" ", 1), data1[56],  rep(" ", 1), data1[67], rep(" ", 1), data1[78], rep(" ", 6))

a4 <- ggplot(data = b4[which(b4$DescrProduto == prod4[which(prod4 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item4)]), ])), y = y4), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item4)]), ])), y = rep(mean(y4), length(y4))), size = 0.3, col = 2) +
  labs(subtitle="Jacto Comercio de Alimentos LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a4

pdf("/home/conexaomundo/MARGARINA_CSAL_QUALY_500G_MQUS1")
grid.arrange(a1, a2, a3, a4, ncol=2)
dev.off()



# estab5 ------------------------------------------------------------------
y5 <- as.numeric(b5[which(b5$DescrProduto == prod5[which(prod5 == item4)]), ]$precoMedioDia)
data5 <- b5[which(b5$DescrProduto == prod5[which(prod5 == item4)]), ]$dtEmissao

a5 <- ggplot(data = b5[which(b5$DescrProduto == prod5[which(prod5 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b5[which(b5$DescrProduto == prod5[which(prod5 == item4)]), ])), y = y5), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b5[which(b5$DescrProduto == prod5[which(prod5 == item4)]), ])), y = rep(mean(y5), length(y5))), size = 0.3, col = 2) +
  labs(subtitle="Bompreço supermercados LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a5

# estab6 ------------------------------------------------------------------
y6 <- as.numeric(b6[which(b6$DescrProduto == prod6[which(prod6 == item4)]), ]$precoMedioDia)
data6 <- b6[which(b6$DescrProduto == prod6[which(prod6 == item4)]), ]$dtEmissao

a6 <- ggplot(data = b6[which(b6$DescrProduto == prod6[which(prod6 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item4)]), ])), y = y6), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item4)]), ])), y = rep(mean(y6), length(y6))), size = 0.3, col = 2) +
  labs(subtitle="Destakão Magazine LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a6

# estab7 ------------------------------------------------------------------
y7 <- as.numeric(b7[which(b7$DescrProduto == prod7[which(prod7 == item4)]), ]$precoMedioDia)
data7 <- b7[which(b7$DescrProduto == prod7[which(prod7 == item4)]), ]$dtEmissao

a7 <- ggplot(data = b7[which(b7$DescrProduto == prod7[which(prod7 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item4)]), ])), y = y7), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item4)]), ])), y = rep(mean(y7), length(y7))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Latorre LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a7

# estab8 ------------------------------------------------------------------
y8 <- as.numeric(b8[which(b8$DescrProduto == prod8[which(prod8 == item4)]), ]$precoMedioDia)
data8 <- b8[which(b8$DescrProduto == prod8[which(prod8 == item4)]), ]$dtEmissao

a8 <- ggplot(data = b8[which(b8$DescrProduto == prod8[which(prod8 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item4)]), ])), y = y8), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item4)]), ])), y = rep(mean(y8), length(y8))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Colibris LTDA-EPP", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a8

pdf("/home/conexaomundo/MARGARINA_CSAL_QUALY_500G_MQUS2")
grid.arrange(a5, a6, a7, a8, ncol=2)
dev.off()




# estab9 ------------------------------------------------------------------
y9 <- as.numeric(b9[which(b9$DescrProduto == prod9[which(prod9 == item4)]), ]$precoMedioDia)
data9 <- b9[which(b9$DescrProduto == prod9[which(prod9 == item4)]), ]$dtEmissao

a9 <- ggplot(data = b9[which(b9$DescrProduto == prod9[which(prod9 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item4)]), ])), y = y9), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item4)]), ])), y = rep(mean(y9), length(y9))), size = 0.3, col = 2) +
  labs(subtitle="Comercial de Alimentos E J C LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a9


# estab10 ------------------------------------------------------------------
y10 <- as.numeric(b10[which(b10$DescrProduto == prod10[which(prod10 == item4)]), ]$precoMedioDia)
data10 <- b10[which(b10$DescrProduto == prod10[which(prod10 == item4)]), ]$dtEmissao

a10 <- ggplot(data = b10[which(b10$DescrProduto == prod10[which(prod10 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item4)]), ])), y = y10), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item4)]), ])), y = rep(mean(y10), length(y10))), size = 0.3, col = 2) +
  labs(subtitle="Carrefour Comercio e Industria LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a10

# estab11 ------------------------------------------------------------------
y11 <- as.numeric(b11[which(b11$DescrProduto == prod11[which(prod11 == item4)]), ]$precoMedioDia)
data11 <- b11[which(b11$DescrProduto == prod11[which(prod11 == item4)]), ]$dtEmissao

a11 <- ggplot(data = b11[which(b11$DescrProduto == prod11[which(prod11 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item4)]), ])), y = y11), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item4)]), ])), y = rep(mean(y11), length(y11))), size = 0.3, col = 2) +
  labs(subtitle="Rede Menor Preço Supermercado LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a11

# estab12 ------------------------------------------------------------------
y12 <- as.numeric(b12[which(b12$DescrProduto == prod12[which(prod12 == item4)]), ]$precoMedioDia)
data12 <- b12[which(b12$DescrProduto == prod12[which(prod12 == item4)]), ]$dtEmissao

a12 <- ggplot(data = b12[which(b12$DescrProduto == prod12[which(prod12 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item4)]), ])), y = y12), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item4)]), ])), y = rep(mean(y12), length(y12))), size = 0.3, col = 2) +
  labs(subtitle="Atacadão S.A.", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a12

pdf("/home/conexaomundo/MARGARINA_CSAL_QUALY_500G_MQUS3")
grid.arrange(a9, a10, a11, a12, ncol=2)
dev.off()


# estab13 ------------------------------------------------------------------
y13 <- as.numeric(b13[which(b13$DescrProduto == prod13[which(prod13 == item4)]), ]$precoMedioDia)
data13 <- b13[which(b13$DescrProduto == prod13[which(prod13 == item4)]), ]$dtEmissao

a13 <- ggplot(data = b13[which(b13$DescrProduto == prod13[which(prod13 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b13[which(b13$DescrProduto == prod13[which(prod13 == item4)]), ])), y = y13), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b13[which(b13$DescrProduto == prod13[which(prod13 == item4)]), ])), y = rep(mean(y13), length(y13))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado e Comercio Varejista Classe A LTDA EPP", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a13

# estab14 ------------------------------------------------------------------

y14 <- as.numeric(b14[which(b14$DescrProduto == prod14[which(prod14 == item4)]), ]$precoMedioDia)
data14 <- b14[which(b14$DescrProduto == prod14[which(prod14 == item4)]), ]$dtEmissao

a14 <- ggplot(data = b14[which(b14$DescrProduto == prod14[which(prod14 == item4)]), ]) + coord_cartesian(ylim = c(minimo4, maximo4)) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item4)]), ])), y = y14), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item4)]), ])), y = rep(mean(y14), length(y14))), size = 0.3, col = 2) +
  labs(subtitle="Comercio Varejista de Alimentos Verde Vale LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Qualy 500g Mqus")

a14

pdf("/home/conexaomundo/MARGARINA_CSAL_QUALY_500G_MQUS4")
grid.arrange(a13, a14, ncol=2)
dev.off()













# [4] "MARGARINA_CSAL_VITARELLA_500G"      

item5 <- "MARGARINA_CSAL_VITARELLA_500G"
# Algumas estatisticas descritivas sobre o item MARGARINA_CSAL_VITARELLA_500G
banco[which(banco$DescrProduto == item5), ]$precoMedioDia
summary(as.numeric(banco[which(banco$DescrProduto == item5), ]$precoMedioDia))
minimo5 <- min(as.numeric(banco[which(banco$DescrProduto == item5), ]$precoMedioDia))
maximo5 <- max(as.numeric(banco[which(banco$DescrProduto == item5), ]$precoMedioDia))
###########################################################################




# estab1 ------------------------------------------------------------------
y1 <- as.numeric(b1[which(b1$DescrProduto == prod1[which(prod1 == item5)]), ]$precoMedioDia)
data1 <- b1[which(b1$DescrProduto == prod1[which(prod1 == item5)]), ]$dtEmissao

a1 <- ggplot(data = b1[which(b1$DescrProduto == prod1[which(prod1 == item5)]), ]) + coord_cartesian(ylim = c(minimo5, maximo5)) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item5)]), ])), y = y1), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item5)]), ])), y = rep(mean(y1), length(y1))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab1", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Vitarella 500g")

a1


# estab3 ------------------------------------------------------------------
y3 <- as.numeric(b3[which(b3$DescrProduto == prod3[which(prod3 == item5)]), ]$precoMedioDia)
data3 <- b3[which(b3$DescrProduto == prod3[which(prod3 == item5)]), ]$dtEmissao

a3 <- ggplot(data = b3[which(b3$DescrProduto == prod3[which(prod3 == item5)]), ]) + coord_cartesian(ylim = c(minimo5, maximo5)) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item5)]), ])), y = y3), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item5)]), ])), y = rep(mean(y3), length(y3))), size = 0.3, col = 2) +
  labs(subtitle="Supermercados Manaíra LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Vitarella 500g", 
       caption = "Source: midwest")

a3



# estab6 ------------------------------------------------------------------
y6 <- as.numeric(b6[which(b6$DescrProduto == prod6[which(prod6 == item5)]), ]$precoMedioDia)
data6 <- b6[which(b6$DescrProduto == prod6[which(prod6 == item5)]), ]$dtEmissao

a6 <- ggplot(data = b6[which(b6$DescrProduto == prod6[which(prod6 == item5)]), ]) + coord_cartesian(ylim = c(minimo5, maximo5)) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item5)]), ])), y = y6), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item5)]), ])), y = rep(mean(y6), length(y6))), size = 0.3, col = 2) +
  labs(subtitle="Destakão Magazine LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Vitarella 500g")

a6

# estab10 ------------------------------------------------------------------
y10 <- as.numeric(b10[which(b10$DescrProduto == prod10[which(prod10 == item5)]), ]$precoMedioDia)
data10 <- b10[which(b10$DescrProduto == prod10[which(prod10 == item5)]), ]$dtEmissao


a10 <- ggplot(data = b10[which(b10$DescrProduto == prod10[which(prod10 == item5)]), ]) + coord_cartesian(ylim = c(minimo5, maximo5)) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item5)]), ])), y = y10), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item5)]), ])), y = rep(mean(y10), length(y10))), size = 0.3, col = 2) +
  labs(subtitle="Carrefour Comercio e Industria LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Vitarella 500g")

a10

png("/home/conexaomundo/MARGARINA_CSAL_VITARELLA_500G1")
grid.arrange(a1, a3, a6, a10, ncol=2)
dev.off()




# estab11 ------------------------------------------------------------------
y11 <- as.numeric(b11[which(b11$DescrProduto == prod11[which(prod11 == item5)]), ]$precoMedioDia)
data11 <- b11[which(b11$DescrProduto == prod11[which(prod11 == item5)]), ]$dtEmissao

a11 <- ggplot(data = b11[which(b11$DescrProduto == prod11[which(prod11 == item5)]), ]) + coord_cartesian(ylim = c(minimo5, maximo5)) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item5)]), ])), y = y11), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item5)]), ])), y = rep(mean(y11), length(y11))), size = 0.3, col = 2) +
  labs(subtitle="Rede Menor Preço Supermercado LTDA", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Vitarella 500g")

a11

# estab12 ------------------------------------------------------------------
y12 <- as.numeric(b12[which(b12$DescrProduto == prod12[which(prod12 == item5)]), ]$precoMedioDia)
data12 <- b12[which(b12$DescrProduto == prod12[which(prod12 == item5)]), ]$dtEmissao

a12 <- ggplot(data = b12[which(b12$DescrProduto == prod12[which(prod12 == item5)]), ]) + coord_cartesian(ylim = c(minimo5, maximo5)) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item5)]), ])), y = y12), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item5)]), ])), y = rep(mean(y12), length(y12))), size = 0.3, col = 2) +
  labs(subtitle="Atacadão S.A.", 
       y="Preço", 
       x="Dias", 
       title="Margarina C/Sal Vitarella 500g")

a12

png("/home/conexaomundo/MARGARINA_CSAL_VITARELLA_500G2")
grid.arrange(a11, a12, ncol=2)
dev.off()









   

# [3] "MACARRAO_IMPERADOR_500G_ESPAGUETE"
item6 <- "MACARRAO_IMPERADOR_500G_ESPAGUETE"
# Algumas estatisticas descritivas sobre o item MACARRAO_IMPERADOR_500G_ESPAGUETE
banco[which(banco$DescrProduto == item6), ]$precoMedioDia
summary(as.numeric(banco[which(banco$DescrProduto == item6), ]$precoMedioDia))
minimo6 <- min(as.numeric(banco[which(banco$DescrProduto == item6), ]$precoMedioDia))
maximo6 <- max(as.numeric(banco[which(banco$DescrProduto == item6), ]$precoMedioDia))
###########################################################################




# estab1 ------------------------------------------------------------------
y1 <- as.numeric(b1[which(b1$DescrProduto == prod1[which(prod1 == item6)]), ]$precoMedioDia)
data1 <- b1[which(b1$DescrProduto == prod1[which(prod1 == item6)]), ]$dtEmissao

a1 <- ggplot(data = b1[which(b1$DescrProduto == prod1[which(prod1 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item6)]), ])), y = y1), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item6)]), ])), y = rep(mean(y1), length(y1))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab1", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a1

# estab2 ------------------------------------------------------------------
y2 <- as.numeric(b2[which(b2$DescrProduto == prod2[which(prod2 == item6)]), ]$precoMedioDia)
data2 <- b2[which(b2$DescrProduto == prod2[which(prod2 == item6)]), ]$dtEmissao

a2 <- ggplot(data = b2[which(b2$DescrProduto == prod2[which(prod2 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item6)]), ])), y = y2), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item6)]), ])), y = rep(mean(y2), length(y2))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab2", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a2


# estab3 ------------------------------------------------------------------
y3 <- as.numeric(b3[which(b3$DescrProduto == prod3[which(prod3 == item6)]), ]$precoMedioDia)
data3 <- b3[which(b3$DescrProduto == prod3[which(prod3 == item6)]), ]$dtEmissao

a3 <- ggplot(data = b3[which(b3$DescrProduto == prod3[which(prod3 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item6)]), ])), y = y3), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item6)]), ])), y = rep(mean(y3), length(y3))), size = 0.3, col = 2) +
  labs(subtitle="Supermercados Manaíra LTDA", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a3

# estab4 ------------------------------------------------------------------
y4 <- as.numeric(b4[which(b4$DescrProduto == prod4[which(prod4 == item6)]), ]$precoMedioDia)
data4 <- b4[which(b4$DescrProduto == prod4[which(prod4 == item6)]), ]$dtEmissao

a4 <- ggplot(data = b4[which(b4$DescrProduto == prod4[which(prod4 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item6)]), ])), y = y4), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item6)]), ])), y = rep(mean(y4), length(y4))), size = 0.3, col = 2) +
  labs(subtitle="Jacto Comercio de Alimentos LTDA", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a4

png("/home/conexaomundo/MACARRAO_IMPERADOR_500G_ESPAGUETE1")
grid.arrange(a1, a2, a3, a4, ncol=2)
dev.off()



# estab6 ------------------------------------------------------------------
y6 <- as.numeric(b6[which(b6$DescrProduto == prod6[which(prod6 == item6)]), ]$precoMedioDia)
data6 <- b6[which(b6$DescrProduto == prod6[which(prod6 == item6)]), ]$dtEmissao

a6 <- ggplot(data = b6[which(b6$DescrProduto == prod6[which(prod6 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item6)]), ])), y = y6), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item6)]), ])), y = rep(mean(y6), length(y6))), size = 0.3, col = 2) +
  labs(subtitle="Destakão Magazine LTDA", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a6

# estab7 ------------------------------------------------------------------
y7 <- as.numeric(b7[which(b7$DescrProduto == prod7[which(prod7 == item6)]), ]$precoMedioDia)
data7 <- b7[which(b7$DescrProduto == prod7[which(prod7 == item6)]), ]$dtEmissao

a7 <- ggplot(data = b7[which(b7$DescrProduto == prod7[which(prod7 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item6)]), ])), y = y7), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item6)]), ])), y = rep(mean(y7), length(y7))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Latorre LTDA", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a7

# estab8 ------------------------------------------------------------------
y8 <- as.numeric(b8[which(b8$DescrProduto == prod8[which(prod8 == item6)]), ]$precoMedioDia)
data8 <- b8[which(b8$DescrProduto == prod8[which(prod8 == item6)]), ]$dtEmissao

a8 <- ggplot(data = b8[which(b8$DescrProduto == prod8[which(prod8 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item6)]), ])), y = y8), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item6)]), ])), y = rep(mean(y8), length(y8))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Colibris LTDA-EPP", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a8

# estab9 ------------------------------------------------------------------
y9 <- as.numeric(b9[which(b9$DescrProduto == prod9[which(prod9 == item6)]), ]$precoMedioDia)
data9 <- b9[which(b9$DescrProduto == prod9[which(prod9 == item6)]), ]$dtEmissao

a9 <- ggplot(data = b9[which(b9$DescrProduto == prod9[which(prod9 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item6)]), ])), y = y9), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item6)]), ])), y = rep(mean(y9), length(y9))), size = 0.3, col = 2) +
  labs(subtitle="Comercial de Alimentos E J C LTDA", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a9

png("/home/conexaomundo/MACARRAO_IMPERADOR_500G_ESPAGUETE2")
grid.arrange(a6, a7, a8, a9, ncol=2)
dev.off()



# estab10 ------------------------------------------------------------------
y10 <- as.numeric(b10[which(b10$DescrProduto == prod10[which(prod10 == item6)]), ]$precoMedioDia)
data10 <- b10[which(b10$DescrProduto == prod10[which(prod10 == item6)]), ]$dtEmissao

a10 <- ggplot(data = b10[which(b10$DescrProduto == prod10[which(prod10 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item6)]), ])), y = y10), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item6)]), ])), y = rep(mean(y10), length(y10))), size = 0.3, col = 2) +
  labs(subtitle="Carrefour Comercio e Industria LTDA", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a10

# estab11 ------------------------------------------------------------------
y11 <- as.numeric(b11[which(b11$DescrProduto == prod11[which(prod11 == item6)]), ]$precoMedioDia)
data11 <- b11[which(b11$DescrProduto == prod11[which(prod11 == item6)]), ]$dtEmissao

a11 <- ggplot(data = b11[which(b11$DescrProduto == prod11[which(prod11 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item6)]), ])), y = y11), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item6)]), ])), y = rep(mean(y11), length(y11))), size = 0.3, col = 2) +
  labs(subtitle="Rede Menor Preço Supermercado LTDA", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a11

# estab12 ------------------------------------------------------------------
y12 <- as.numeric(b12[which(b12$DescrProduto == prod12[which(prod12 == item6)]), ]$precoMedioDia)
data12 <- b12[which(b12$DescrProduto == prod12[which(prod12 == item6)]), ]$dtEmissao

a12 <- ggplot(data = b12[which(b12$DescrProduto == prod12[which(prod12 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item6)]), ])), y = y12), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item6)]), ])), y = rep(mean(y12), length(y12))), size = 0.3, col = 2) +
  labs(subtitle="Atacadão S.A.", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a12

# estab14 ------------------------------------------------------------------
y14 <- as.numeric(b14[which(b14$DescrProduto == prod14[which(prod14 == item6)]), ]$precoMedioDia)
data14 <- b14[which(b14$DescrProduto == prod14[which(prod14 == item6)]), ]$dtEmissao

a14 <- ggplot(data = b14[which(b14$DescrProduto == prod14[which(prod14 == item6)]), ]) + coord_cartesian(ylim = c(minimo6, maximo6)) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item6)]), ])), y = y14), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item6)]), ])), y = rep(mean(y14), length(y14))), size = 0.3, col = 2) +
  labs(subtitle="Comercio Varejista de Alimentos Verde Vale LTDA", 
       y="Preço", 
       x="Dias", 
       title="Macarrão Espaguete Imperador 500g")

a14

png("/home/conexaomundo/MACARRAO_IMPERADOR_500G_ESPAGUETE3")
grid.arrange(a10, a11, a12, a14, ncol=2)
dev.off()






# "MANTEIGA_CSAL_ITACOLOMY_PT_200G"  
item7 <- "MANTEIGA_CSAL_ITACOLOMY_PT_200G"
# Algumas estatisticas descritivas sobre o item MACARRAO_IMPERADOR_500G_ESPAGUETE
banco[which(banco$DescrProduto == item7), ]$precoMedioDia
summary(as.numeric(banco[which(banco$DescrProduto == item7), ]$precoMedioDia))
minimo7 <- min(as.numeric(banco[which(banco$DescrProduto == item7), ]$precoMedioDia))
maximo7 <- max(as.numeric(banco[which(banco$DescrProduto == item7), ]$precoMedioDia))
###########################################################################

# estab1 ------------------------------------------------------------------
y1 <- as.numeric(b1[which(b1$DescrProduto == prod1[which(prod1 == item7)]), ]$precoMedioDia)
data1 <- b1[which(b1$DescrProduto == prod1[which(prod1 == item7)]), ]$dtEmissao

a1 <- ggplot(data = b1[which(b1$DescrProduto == prod1[which(prod1 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item7)]), ])), y = y1), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item7)]), ])), y = rep(mean(y1), length(y1))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab1", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a1

# estab2 ------------------------------------------------------------------
y2 <- as.numeric(b2[which(b2$DescrProduto == prod2[which(prod2 == item7)]), ]$precoMedioDia)
data2 <- b2[which(b2$DescrProduto == prod2[which(prod2 == item7)]), ]$dtEmissao
# length(data2)/10

# data22 <- c(data2[1], rep(" ", 1), data1[12],  rep(" ", 1), data1[23],  rep(" ", 1), data1[34], rep(" ", 1), data1[45], rep(" ", 1), data1[56],  rep(" ", 1), data1[67], rep(" ", 1), data1[78], rep(" ", 6))

a2 <- ggplot(data = b2[which(b2$DescrProduto == prod2[which(prod2 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item7)]), ])), y = y2), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item7)]), ])), y = rep(mean(y2), length(y2))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab2", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a2


# estab3 ------------------------------------------------------------------
y3 <- as.numeric(b3[which(b3$DescrProduto == prod3[which(prod3 == item7)]), ]$precoMedioDia)
data3 <- b3[which(b3$DescrProduto == prod3[which(prod3 == item7)]), ]$dtEmissao
# length(data3)/10

# data33 <- c(data3[1], rep(" ", 1), data1[12],  rep(" ", 1), data1[23],  rep(" ", 1), data1[34], rep(" ", 1), data1[45], rep(" ", 1), data1[56],  rep(" ", 1), data1[67], rep(" ", 1), data1[78], rep(" ", 6))

a3 <- ggplot(data = b3[which(b3$DescrProduto == prod3[which(prod3 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item7)]), ])), y = y3), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item7)]), ])), y = rep(mean(y3), length(y3))), size = 0.3, col = 2) +
  labs(subtitle="Supermercados Manaíra LTDA", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a3

# estab4 ------------------------------------------------------------------

y4 <- as.numeric(b4[which(b4$DescrProduto == prod4[which(prod4 == item7)]), ]$precoMedioDia)
data4 <- b4[which(b4$DescrProduto == prod4[which(prod4 == item7)]), ]$dtEmissao
# length(data4)/10

# data33 <- c(data3[1], rep(" ", 1), data1[12],  rep(" ", 1), data1[23],  rep(" ", 1), data1[34], rep(" ", 1), data1[45], rep(" ", 1), data1[56],  rep(" ", 1), data1[67], rep(" ", 1), data1[78], rep(" ", 6))

a4 <- ggplot(data = b4[which(b4$DescrProduto == prod4[which(prod4 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item7)]), ])), y = y4), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b4[which(b4$DescrProduto == prod4[which(prod4 == item7)]), ])), y = rep(mean(y4), length(y4))), size = 0.3, col = 2) +
  labs(subtitle="Jacto Comercio de Alimentos LTDA", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a4

grid.arrange(a1, a2, a3, a4, ncol=2)

# estab5 ------------------------------------------------------------------

y5 <- as.numeric(b5[which(b5$DescrProduto == prod5[which(prod5 == item7)]), ]$precoMedioDia)
data5 <- b5[which(b5$DescrProduto == prod5[which(prod5 == item7)]), ]$dtEmissao
# length(data5)/10


a5 <- ggplot(data = b5[which(b5$DescrProduto == prod5[which(prod5 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b5[which(b5$DescrProduto == prod5[which(prod5 == item7)]), ])), y = y5), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b5[which(b5$DescrProduto == prod5[which(prod5 == item7)]), ])), y = rep(mean(y5), length(y5))), size = 0.3, col = 2) +
  labs(subtitle="Bompreço supermercados LTDA", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a5

# estab6 ------------------------------------------------------------------

y6 <- as.numeric(b6[which(b6$DescrProduto == prod6[which(prod6 == item7)]), ]$precoMedioDia)
data6 <- b6[which(b6$DescrProduto == prod6[which(prod6 == item7)]), ]$dtEmissao
length(data6)/10


a6 <- ggplot(data = b6[which(b6$DescrProduto == prod6[which(prod6 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item7)]), ])), y = y6), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item7)]), ])), y = rep(mean(y6), length(y6))), size = 0.3, col = 2) +
  labs(subtitle="Destakão Magazine LTDA", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a6

# estab7 ------------------------------------------------------------------

y7 <- as.numeric(b7[which(b7$DescrProduto == prod7[which(prod7 == item7)]), ]$precoMedioDia)
data7 <- b7[which(b7$DescrProduto == prod7[which(prod7 == item7)]), ]$dtEmissao
# length(data7)/10


a7 <- ggplot(data = b7[which(b7$DescrProduto == prod7[which(prod7 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item7)]), ])), y = y7), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item7)]), ])), y = rep(mean(y7), length(y7))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Latorre LTDA", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a7

# estab8 ------------------------------------------------------------------

y8 <- as.numeric(b8[which(b8$DescrProduto == prod8[which(prod8 == item7)]), ]$precoMedioDia)
data8 <- b8[which(b8$DescrProduto == prod8[which(prod8 == item7)]), ]$dtEmissao
# length(data8)/10


a8 <- ggplot(data = b8[which(b8$DescrProduto == prod8[which(prod8 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item7)]), ])), y = y8), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item7)]), ])), y = rep(mean(y8), length(y8))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Colibris LTDA-EPP", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a8
grid.arrange(a5, a6, a7, a8, ncol=2)



# estab9 ------------------------------------------------------------------

y9 <- as.numeric(b9[which(b9$DescrProduto == prod9[which(prod9 == item7)]), ]$precoMedioDia)
data9 <- b9[which(b9$DescrProduto == prod9[which(prod9 == item7)]), ]$dtEmissao
# length(data9)/10


a9 <- ggplot(data = b9[which(b9$DescrProduto == prod9[which(prod9 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item7)]), ])), y = y9), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item7)]), ])), y = rep(mean(y9), length(y9))), size = 0.3, col = 2) +
  labs(subtitle="Comercial de Alimentos E J C LTDA", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a9

# grid.arrange(a5, a7, a8, a9, ncol=2)

# estab10 ------------------------------------------------------------------

y10 <- as.numeric(b10[which(b10$DescrProduto == prod10[which(prod10 == item7)]), ]$precoMedioDia)
data10 <- b10[which(b10$DescrProduto == prod10[which(prod10 == item7)]), ]$dtEmissao
# length(data10)/10


a10 <- ggplot(data = b10[which(b10$DescrProduto == prod10[which(prod10 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item7)]), ])), y = y10), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b10[which(b10$DescrProduto == prod10[which(prod10 == item7)]), ])), y = rep(mean(y10), length(y10))), size = 0.3, col = 2) +
  labs(subtitle="Carrefour Comercio e Industria LTDA", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a10

# estab11 ------------------------------------------------------------------

y11 <- as.numeric(b11[which(b11$DescrProduto == prod11[which(prod11 == item7)]), ]$precoMedioDia)
data11 <- b11[which(b11$DescrProduto == prod11[which(prod11 == item7)]), ]$dtEmissao
# length(data11)/10


a11 <- ggplot(data = b11[which(b11$DescrProduto == prod11[which(prod11 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item7)]), ])), y = y11), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item7)]), ])), y = rep(mean(y11), length(y11))), size = 0.3, col = 2) +
  labs(subtitle="Rede Menor Preço Supermercado LTDA", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a11

# estab12 ------------------------------------------------------------------

y12 <- as.numeric(b12[which(b12$DescrProduto == prod12[which(prod12 == item7)]), ]$precoMedioDia)
data12 <- b12[which(b12$DescrProduto == prod12[which(prod12 == item7)]), ]$dtEmissao
# length(data12)/10


a12 <- ggplot(data = b12[which(b12$DescrProduto == prod12[which(prod12 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item7)]), ])), y = y12), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b12[which(b12$DescrProduto == prod12[which(prod12 == item7)]), ])), y = rep(mean(y12), length(y12))), size = 0.3, col = 2) +
  labs(subtitle="Atacadão S.A.", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a12

# estab13 ------------------------------------------------------------------

y13 <- as.numeric(b13[which(b13$DescrProduto == prod13[which(prod13 == item7)]), ]$precoMedioDia)
data13 <- b13[which(b13$DescrProduto == prod13[which(prod13 == item7)]), ]$dtEmissao
length(data13)/10


a13 <- ggplot(data = b13[which(b13$DescrProduto == prod13[which(prod13 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b13[which(b13$DescrProduto == prod13[which(prod13 == item7)]), ])), y = y13), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b13[which(b13$DescrProduto == prod13[which(prod13 == item7)]), ])), y = rep(mean(y13), length(y13))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado e Comercio Varejista Classe A LTDA EPP", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a13

# estab14 ------------------------------------------------------------------

y14 <- as.numeric(b14[which(b14$DescrProduto == prod14[which(prod14 == item7)]), ]$precoMedioDia)
data14 <- b14[which(b14$DescrProduto == prod14[which(prod14 == item7)]), ]$dtEmissao
# length(data14)/10


a14 <- ggplot(data = b14[which(b14$DescrProduto == prod14[which(prod14 == item7)]), ]) + coord_cartesian(ylim = c(minimo7, maximo7)) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item7)]), ])), y = y14), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b14[which(b14$DescrProduto == prod14[which(prod14 == item7)]), ])), y = rep(mean(y14), length(y14))), size = 0.3, col = 2) +
  labs(subtitle="Comercio Varejista de Alimentos Verde Vale LTDA", 
       y="Preço", 
       x="Dias", 
       title="Manteiga C/Sal Itacolomy PT 200g", 
       caption = "Source: midwest")

a14

grid.arrange(a9, a10, a11, a12, a13, a14, ncol=2)




# [1] "FEIJAO_CARIOCA_COMETA_1KG"  

item8 <- "FEIJAO_CARIOCA_COMETA_1KG"


# estab1 ------------------------------------------------------------------
y1 <- as.numeric(b1[which(b1$DescrProduto == prod1[which(prod1 == item8)]), ]$precoMedioDia)
data1 <- b1[which(b1$DescrProduto == prod1[which(prod1 == item8)]), ]$dtEmissao
length(data1)/10

# data11 <- c(data1[1], rep(" ", 1), data1[12],  rep(" ", 1), data1[23],  rep(" ", 1), data1[34], rep(" ", 1), data1[45], rep(" ", 1), data1[56],  rep(" ", 1), data1[67], rep(" ", 1), data1[78], rep(" ", 6))

a1 <- ggplot(data = b1[which(b1$DescrProduto == prod1[which(prod1 == item8)]), ]) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item8)]), ])), y = y1), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b1[which(b1$DescrProduto == prod1[which(prod1 == item8)]), ])), y = rep(mean(y1), length(y1))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab1", 
       y="Preço", 
       x="Dias", 
       title="Feijão Carioca Cometa 1Kg", 
       caption = "Source: midwest")

a1

# estab2 ------------------------------------------------------------------
y2 <- as.numeric(b2[which(b2$DescrProduto == prod2[which(prod2 == item8)]), ]$precoMedioDia)
data2 <- b2[which(b2$DescrProduto == prod2[which(prod2 == item8)]), ]$dtEmissao
# length(data2)/10

# data22 <- c(data2[1], rep(" ", 1), data1[12],  rep(" ", 1), data1[23],  rep(" ", 1), data1[34], rep(" ", 1), data1[45], rep(" ", 1), data1[56],  rep(" ", 1), data1[67], rep(" ", 1), data1[78], rep(" ", 6))

a2 <- ggplot(data = b2[which(b2$DescrProduto == prod2[which(prod2 == item8)]), ]) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item8)]), ])), y = y2), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b2[which(b2$DescrProduto == prod2[which(prod2 == item8)]), ])), y = rep(mean(y2), length(y2))), size = 0.3, col = 2) +
  labs(subtitle="Companhia Brasileirta de Distribuição estab2", 
       y="Preço", 
       x="Dias", 
       title="Feijão Carioca Cometa 1Kg", 
       caption = "Source: midwest")

a2


# estab3 ------------------------------------------------------------------
y3 <- as.numeric(b3[which(b3$DescrProduto == prod3[which(prod3 == item8)]), ]$precoMedioDia)
data3 <- b3[which(b3$DescrProduto == prod3[which(prod3 == item8)]), ]$dtEmissao
# length(data3)/10

# data33 <- c(data3[1], rep(" ", 1), data1[12],  rep(" ", 1), data1[23],  rep(" ", 1), data1[34], rep(" ", 1), data1[45], rep(" ", 1), data1[56],  rep(" ", 1), data1[67], rep(" ", 1), data1[78], rep(" ", 6))

a3 <- ggplot(data = b3[which(b3$DescrProduto == prod3[which(prod3 == item8)]), ]) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item8)]), ])), y = y3), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b3[which(b3$DescrProduto == prod3[which(prod3 == item8)]), ])), y = rep(mean(y3), length(y3))), size = 0.3, col = 2) +
  labs(subtitle="Supermercados Manaíra LTDA", 
       y="Preço", 
       x="Dias", 
       title="Feijão Carioca Cometa 1Kg", 
       caption = "Source: midwest")

a3




# estab6 ------------------------------------------------------------------

y6 <- as.numeric(b6[which(b6$DescrProduto == prod6[which(prod6 == item8)]), ]$precoMedioDia)
data6 <- b6[which(b6$DescrProduto == prod6[which(prod6 == item8)]), ]$dtEmissao
length(data6)/10


a6 <- ggplot(data = b6[which(b6$DescrProduto == prod6[which(prod6 == item8)]), ]) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item8)]), ])), y = y6), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b6[which(b6$DescrProduto == prod6[which(prod6 == item8)]), ])), y = rep(mean(y6), length(y6))), size = 0.3, col = 2) +
  labs(subtitle="Destakão Magazine LTDA", 
       y="Preço", 
       x="Dias", 
       title="Feijão Carioca Cometa 1Kg", 
       caption = "Source: midwest")

a6


grid.arrange(a1, a2, a3, a6, ncol=2)

# estab7 ------------------------------------------------------------------

y7 <- as.numeric(b7[which(b7$DescrProduto == prod7[which(prod7 == item8)]), ]$precoMedioDia)
data7 <- b7[which(b7$DescrProduto == prod7[which(prod7 == item8)]), ]$dtEmissao
# length(data7)/10


a7 <- ggplot(data = b7[which(b7$DescrProduto == prod7[which(prod7 == item8)]), ]) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item8)]), ])), y = y7), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b7[which(b7$DescrProduto == prod7[which(prod7 == item8)]), ])), y = rep(mean(y7), length(y7))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Latorre LTDA", 
       y="Preço", 
       x="Dias", 
       title="Feijão Carioca Cometa 1Kg", 
       caption = "Source: midwest")

a7

# estab8 ------------------------------------------------------------------

y8 <- as.numeric(b8[which(b8$DescrProduto == prod8[which(prod8 == item8)]), ]$precoMedioDia)
data8 <- b8[which(b8$DescrProduto == prod8[which(prod8 == item8)]), ]$dtEmissao
# length(data8)/10


a8 <- ggplot(data = b8[which(b8$DescrProduto == prod8[which(prod8 == item8)]), ]) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item8)]), ])), y = y8), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b8[which(b8$DescrProduto == prod8[which(prod8 == item8)]), ])), y = rep(mean(y8), length(y8))), size = 0.3, col = 2) +
  labs(subtitle="Supermercado Colibris LTDA-EPP", 
       y="Preço", 
       x="Dias", 
       title="Feijão Carioca Cometa 1Kg", 
       caption = "Source: midwest")

a8




# estab9 ------------------------------------------------------------------

y9 <- as.numeric(b9[which(b9$DescrProduto == prod9[which(prod9 == item8)]), ]$precoMedioDia)
data9 <- b9[which(b9$DescrProduto == prod9[which(prod9 == item8)]), ]$dtEmissao
# length(data9)/10


a9 <- ggplot(data = b9[which(b9$DescrProduto == prod9[which(prod9 == item8)]), ]) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item8)]), ])), y = y9), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b9[which(b9$DescrProduto == prod9[which(prod9 == item8)]), ])), y = rep(mean(y9), length(y9))), size = 0.3, col = 2) +
  labs(subtitle="Comercial de Alimentos E J C LTDA", 
       y="Preço", 
       x="Dias", 
       title="Feijão Carioca Cometa 1Kg", 
       caption = "Source: midwest")

a9

# grid.arrange(a5, a7, a8, a9, ncol=2)


# estab11 ------------------------------------------------------------------

y11 <- as.numeric(b11[which(b11$DescrProduto == prod11[which(prod11 == item8)]), ]$precoMedioDia)
data11 <- b11[which(b11$DescrProduto == prod11[which(prod11 == item8)]), ]$dtEmissao
# length(data11)/10


a11 <- ggplot(data = b11[which(b11$DescrProduto == prod11[which(prod11 == item8)]), ]) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item8)]), ])), y = y11), size = 0.25) +
  geom_line(mapping = aes(x = seq(1, nrow(b11[which(b11$DescrProduto == prod11[which(prod11 == item8)]), ])), y = rep(mean(y11), length(y11))), size = 0.3, col = 2) +
  labs(subtitle="Rede Menor Preço Supermercado LTDA", 
       y="Preço", 
       x="Dias", 
       title="Feijão Carioca Cometa 1Kg", 
       caption = "Source: midwest")

a11

grid.arrange(a7, a8, a9, a11, ncol=2)


## UMA TABELA COM ANALISE DESCRITIVA SOBRE O PROFDUTO NOS ESTABELECIMENTOS
# SIMPLES LIMPA, MAS COM INOFRMAÇÕE POSSÍVEIS DE ENTENDIMENTO.




