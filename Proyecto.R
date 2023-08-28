###############################################################################
###############################################################################
###############################################################################
###### PCA ######
data_afidos
summary (data_afidos)
##
##
###### PCA ######
library ("factoextra")
datafc <- data_afidos1 [,6:19]
datafc
datafpca <- prcomp (datafc,scale = TRUE)
datafpca
summary (datafpca)
screeplot (datafpca)
biplot (datafpca, scale = TRUE, cex = 1, col = c("dodgerblue3", "deeppink3"))
fviz_pca_biplot (datafpca)
cargas <- datafpca$rotation 
cargas

###############################################################################
###############################################################################
###### ANOSIM ######
library("vegan")
datafAN <- data_afidos1 [,2:19]
datafAN
mdaf <- vegdist (datafAN, method = "euclidean")
mdaf
anosimaf <-  anosim (mdaf, datafAN$chili, permutations = 999, 
                     distance = "euclidean")
anosimaf

###############################################################################
###############################################################################
###############################################################################
###### Univariados ######
### Correlacion ###
library ("ggplot2")
##
##
## Poblano y Jalapeño 5% ##
datafcp5 <- data_afidos1 [1:5,10]
datafcp5
datafcj5 <- data_afidos1 [16:20,10]
datafcj5
ctpj5 <- cor.test (~datafcp5+datafcj5, data_afidos1, method= "pearson", 
                   conf.level=0.95)
ctpj5
## Poblano y Jalapeño 10% ##
datafcp10 <- data_afidos1 [6:10,10]
datafcp10
datafcj10 <- data_afidos1 [21:25,10]
datafcj10
ctpj10 <- cor.test (~datafcp10+datafcj10, data_afidos1, method= "pearson", 
                   conf.level=0.95)
ctpj10
## Poblano y Jalapeño 15% ##
datafcp15 <- data_afidos1 [11:15,10]
datafcp15
datafcj15 <- data_afidos1 [26:30,10]
datafcj15
ctpj15 <- cor.test (~datafcp15+datafcj15, data_afidos1, method= "pearson", 
                    conf.level=0.95)
ctpj15
##
##
##
## Poblano y Arbol 5% ##
datafcp5 <- data_afidos1 [1:5,10]
datafcp5
datafca5 <- data_afidos1 [31:35,10]
datafca5
ctpa5 <- cor.test (~datafcp5+datafca5, data_afidos1, method= "pearson", 
                   conf.level=0.95)
ctpa5
## Poblano y Arbol 10% ## S
datafcp10 <- data_afidos1 [6:10,10]
datafcp10
datafca10 <- data_afidos1 [36:40,10]
datafca10
ctpa10 <- cor.test (~datafcp10+datafca10, data_afidos1, method= "pearson", 
                   conf.level=0.95)
ctpa10
## Poblano y Arbol 15% ##
datafcp15 <- data_afidos1 [11:15,10]
datafcp15
datafca15 <- data_afidos1 [41:45,10]
datafca15
ctpa15 <- cor.test (~datafcp15+datafca15, data_afidos1, method= "pearson", 
                    conf.level=0.95)
ctpa15
##
##
##
## Jalapeño y Arbol 5% ##
datafcj5 <- data_afidos1 [16:20,10]
datafcj5
datafca5 <- data_afidos1 [31:35,10]
datafca5
ctja5 <- cor.test (~datafcj5+datafca5, data_afidos1, method= "pearson", 
                   conf.level=0.95)
ctja5
## Jalapeño y Arbol 10% ##
datafcj10 <- data_afidos1 [21:25,10]
datafcj10
datafca10 <- data_afidos1 [36:40,10]
datafca10
ctja10 <- cor.test (~datafcj10+datafca10, data_afidos1, method= "pearson", 
                   conf.level=0.95)
ctja10
## Jalapeño y Arbol 15% ##
datafcj15 <- data_afidos1 [26:30,10]
datafcj15
datafca15 <- data_afidos1 [41:45,10]
datafca15
ctja15 <- cor.test (~datafcj15+datafca15, data_afidos1, method= "pearson", 
                    conf.level=0.95)
ctja15


###############################################################################
###############################################################################
### Prueba de T  ###
##
##
## Poblano y Jalapeño 5% ##
dataftp5 <- data_afidos1 [1:5,4:19]
dataftp5
dataftj5 <- data_afidos1 [16:20,4:19]
dataftj5
resultadotpj5 <- t.test (dataftp5$abdomen_l,dataftj5$abdomen_l)
resultadotpj5
## Poblano y Jalapeño 10% ##
dataftp10 <- data_afidos1 [6:10,4:19]
dataftp10
dataftj10 <- data_afidos1 [21:25,4:19]
dataftj10
resultadotpj10 <- t.test (dataftp10$abdomen_l,dataftj10$abdomen_l)
resultadotpj10
## Poblano y Jalapeño 15% ##
dataftp15 <- data_afidos1 [11:15,4:19]
dataftp15
dataftj15 <- data_afidos1 [26:30,4:19]
dataftj15
resultadotpj15 <- t.test (dataftp15$abdomen_l,dataftj15$abdomen_l)
resultadotpj15
##
##
##
## Poblano y Arbol 5% ##
dataftp5 <- data_afidos1 [1:5,4:19]
dataftp5
datafta5 <- data_afidos1 [31:35,4:19]
datafta5
resultadotpa5 <- t.test (dataftp5$abdomen_l,datafta5$abdomen_l)
resultadotpa5
## Poblano y Arbol 10% ## S
dataftp10 <- data_afidos1 [6:10,4:19]
dataftp10
datafta10 <- data_afidos1 [36:40,4:19]
datafta10
resultadotpa10 <- t.test (dataftp10$abdomen_l,datafta10$abdomen_l)
resultadotpa10
## Poblano y Arbol 15% ##
dataftp15 <- data_afidos1 [11:15,4:19]
dataftp15
datafta15 <- data_afidos1 [41:45,4:19]
datafta15
resultadotpa15 <- t.test (dataftp15$abdomen_l,datafta15$abdomen_l)
resultadotpa15
##
##
##
## Jalapeño y Arbol 5% ##
dataftj5 <- data_afidos1 [16:20,4:19]
dataftj5
datafta5 <- data_afidos1 [31:35,4:19]
datafta5
resultadotJa5 <- t.test (dataftj5$abdomen_l,datafta5$abdomen_l)
resultadotJa5
## Jalapeño y Arbol 10% ##
dataftj10 <- data_afidos1 [21:25,4:19]
dataftj10
datafta10 <- data_afidos1 [36:40,4:19]
datafta10
resultadotJa10 <- t.test (dataftj10$abdomen_l,datafta10$abdomen_l)
resultadotJa10
## Jalapeño y Arbol 15% ##
dataftj15 <- data_afidos1 [26:30,4:19]
dataftj15
datafta15 <- data_afidos1 [41:45,4:19]
datafta15
resultadotJa15 <- t.test (dataftj15$abdomen_l,datafta15$abdomen_l)
resultadotJa15


citation()



