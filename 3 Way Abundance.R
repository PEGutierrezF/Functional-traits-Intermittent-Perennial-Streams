

install.packages("FD")
install.packages("ggplot2")

require("FD")
require ("vegan")
library("plyr")
library("ggplot2")
library(grid)


citation("FD")
# setwd ("D:/Curriculum/02_ Articulos/00 In progress/117 Darixa Hernandez/Functional Diversity/ThreeWay/Abundance")

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




########################################
########## Funtional Richness ##########

FD.output<-dbFD(tr.dist, sp.matrix)
FD.output$FRic

treatment.matrix$FRic<-FD.output$FRic
treatment.matrix

# Permanova para FRic

adonis(treatment.matrix$FRic ~ treatment.matrix$Treatment*treatment.matrix$Season*treatment.matrix$Habitat, permutations=999, method = "euclidean") ## generación de permanova con euclideana y un alfa de 0.001

summarize <- ddply(treatment.matrix, .(Treatment, Season,Habitat), summarize, mean = mean(FRic), sd= sd(FRic), se = sd(FRic)/length(FRic)) 
summarize

library(grid)
dodge <- position_dodge(width = 0.9)

g1 <- ggplot(data = summarize, aes(x = interaction(Habitat, Treatment), y = mean, fill = factor(Season))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), position = dodge, width = 0.2)+
  coord_cartesian(ylim = c(0.00002, 0.0004)) + 
  annotate("text", x = 1:4, y = -0.00001,
           label = rep(c("Pool", "Riffle"), 2)) +
  annotate("text", c(1.5, 3.5), y =  -0.00002, label = c("Intermittent", "Perenial"))+
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
(g1 = g1 + scale_fill_grey(start = 0, end = .9))
g1 + guides(fill=guide_legend(title=NULL))

g2 <- ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.draw(g2)


# Boxplot


version.labs <- c(`1`="FRic", `2`="FEve", `3`="FDis")
ggplot(summarize, aes(x=factor(Treatment), y=mean, fill=factor(Season)))+
geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=factor(Version)), alpha=0.9) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) + facet_grid(.~Version, labeller = as_labeller(version.labs)) +
  theme(strip.text.x = element_text(size=9, color="black", face="bold"))

ggplot(summarize, aes(x=factor(Treatment), y=mean) ) + 
  geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=factor(Version)), alpha=0.9) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE, aes(fill=factor(Season))) + facet_grid(.~Version, labeller = as_labeller(version.labs)) +
  theme(strip.text.x = element_text(size=9, color="black", face="bold")) +
  labs(x="Risk distribution per software version and subsystem type", y="mean") + 
  guides(color=guide_legend("Version"))




########################################
########## Funtional evenness ##########

FD.output1<-dbFD(tr.dist, sp.matrix)
FD.output1$FEve

treatment.matrix$FEve<-FD.output1$FEve
treatment.matrix

adonis(treatment.matrix$FEve ~ treatment.matrix$Treatment*treatment.matrix$Season*treatment.matrix$Habitat, permutations=999, method = "euclidean") ## generación de permanova con euclideana y un alfa de 0.001

summarize <- ddply(treatment.matrix, .(Treatment, Season, Habitat), summarize, mean = mean(FEve), sd= sd(FEve), se = sd(FEve)/length(FEve)) 
summarize

ddply(treatment.matrix,~Treatment,summarise,mean=mean(FEve),sd=sd(FEve))
ddply(treatment.matrix,~Season,summarise,mean=mean(FEve),sd=sd(FEve))

library(grid)
dodge <- position_dodge(width = 0.9)

g1 <- ggplot(data = summarize, aes(x = interaction(Habitat, Treatment), y = mean, fill = factor(Season))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), position = dodge, width = 0.2)+
  coord_cartesian(ylim = c(0.008, 0.8)) + # to change the y-scale
  annotate("text", x = 1:4, y = -0.05,    # to changes sub title
           label = rep(c("Pool", "Riffle"), 2)) +
  annotate("text", c(1.5, 3.5), y =  -0.08, label = c("Intermittent", "Perenial"))+ # to change Title
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
(g1 = g1 + scale_fill_grey(start = 0, end = .9))
g1 + guides(fill=guide_legend(title=NULL))

g2 <- ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.draw(g2)


###########################################
########## Functional Diversity  ##########

FD.output<-dbFD(tr.dist, sp.matrix)
FD.output$FDis

treatment.matrix$FDis<-FD.output$FDis
treatment.matrix

###### Permanova ######

adonis(treatment.matrix$FDis ~ treatment.matrix$Treatment*treatment.matrix$Season*treatment.matrix$Habitat, permutations=999, method = "euclidean") ## generación de permanova con euclideana y un alfa de 0.001

summarize <- ddply(treatment.matrix, .(Treatment, Season,Habitat), summarize, mean = mean(FDis), sd= sd(FDis), se = sd(FDis)/length(FDis)) 
summarize

ddply(treatment.matrix,~Treatment,summarise,mean=mean(FDis),sd=sd(FDis))
ddply(treatment.matrix,~Season,summarise,mean=mean(FDis),sd=sd(FDis))

library(grid)
dodge <- position_dodge(width = 0.9)

g1 <- ggplot(data = summarize, aes(x = interaction(Habitat, Treatment), y = mean, fill = factor(Season))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), position = dodge, width = 0.2)+
  coord_cartesian(ylim = c(0.018, 0.4)) + 
  annotate("text", x = 1:4, y = -0.015,
           label = rep(c("Pool", "Riffle"), 2)) +
  annotate("text", c(1.5, 3.5), y =  -0.03, label = c("Intermittent", "Perenial"))+
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
       axis.title.x = element_blank(),
       axis.text.x = element_blank())
(g1 = g1 + scale_fill_grey(start = 0, end = .9))
g1 + guides(fill=guide_legend(title=NULL))

g2 <- ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.draw(g2)


############### Boxplot ##################

FR<-read.csv("FRichness.csv")
head(FR)

p <- ggplot(data =FR, aes(interaction(Habitat, Treatment), y=Mean)) + 
            geom_boxplot(aes(fill=Season))
p2 <- p + facet_wrap(.~Frichness, scales= "free")+ # or as Label
         # theme(strip.text.x = element_text(size=9, color="black", face="bold"))+
  theme(strip.text.x = element_text(face="bold", size=12)) +
  theme(strip.text.y = element_blank(), # remove background
        strip.background.x = element_blank()) +  # remove background
theme(strip.text.x = element_text(face="bold", size=12, colour="white")) # Each title white
p2
p3 <- p2 + labs(x="Stream-Habitat", y = "Functional diversity")
p3
p4 <- p3 + theme(axis.title.x = element_text(color = "black", size = 12, face = "bold"),
            axis.title.y = element_text(color = "black", size = 12, face = "bold"))
p4
p5 <- p4 + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5,hjust=1, color="black")) #subaxis x
p5
p6 <- p5 + theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) # #subaxis y
p6
# p6 <- p5 +   stat_summary(fun.y=mean, geom="point", aes(group=Season), position=position_dodge(.9), 
#                          color="gray", size=3)  # to add mean

p7 <- p6 +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
p8 <- p7 + scale_fill_grey(start = 0, end = .9)
# scale_fill_manual(values = c("grey80", "grey30"))
p8


dat_text <- data.frame(
  label = c("A", "B", "C"),
  Frichness   = c("FDis", "FEve", "FRic"),
  x     = c(4, 4.4, 4),
  y     = c(0.45, 0.8, 0.00053)
)

p9 <- p8 + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label))
p9

p9 + ggsave("Figure 1.TIFF",width=6, height=4,dpi=600)

