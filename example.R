



# ---------------------------------------------
# Functional diversity Analisys
# 20 Oct 2021
# Pablo E. Guti�rrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  
##### Spp Matrix #####
sp.matrix<-read.csv("sp_abundance.csv", row.names=1)
head(sp.matrix)
str(sp.matrix)

#Traits Matrix
tr.matrix<-read.csv("traits.csv", row.names=1)
head(tr.matrix)
str(tr.matrix)

#Treatment Matrix
treatment.matrix<-read.csv("treatment.csv", row.names=1)
head(treatment.matrix) 
str(treatment.matrix)

#convertir los numeros enteros en factores (para que R no entienda las categorias como numeros)
tr.matrix.factor<-as.data.frame(apply(tr.matrix[,], 2, as.factor))
tr.matrix.factor

#Anadir nombres a las filas
rownames(tr.matrix.factor) <- rownames(tr.matrix)

#Crear una matriz de distancias usando la metrica de desemejanza de Gower (u otra)
#(usualmente usamos la distancia euclidea para rasgos continuos)
tr.dist<-gowdis(tr.matrix.factor)
head(tr.dist)



#  Correr los analisis 
FD.output<-dbFD(tr.dist, sp.matrix)
FD.output$FRic
FD.output$FEve
FD.output$FDis
FD.output$FDiv
FD.output$RaoQ


treatment.matrix$FRic<- FD.output$FRic
treatment.matrix$FEve<- FD.output$FEve
treatment.matrix$FDis<- FD.output$FDis
treatment.matrix$FDiv<- FD.output$FDiv
treatment.matrix$RaoQ<- FD.output$RaoQ
treatment.matrix


dummy$abun
dummy$trait

ex1 <- dbFD(dummy$trait, dummy$abun)
ex1$CWM
