cbasico1 <- read.csv("enpecyt_bienal_csv/conjunto_de_datos/tr_cbasico1.csv")
ver <- read.csv("enpecyt_bienal_csv/diccionario_de_datos/diccionario_de_datos_csocio.csv")
dicc_cbasico1 <- read.csv("enpecyt_bienal_csv/diccionario_de_datos/diccionario_de_datos_cbasico1.csv",
                          na.strings = "")

diccionario <- dicc_cbasico1
cambio <- "ENT"
encuesta <- cbasico1

translate <- function(encuesta, diccionario, cambio){

  diccionario <- diccionario[,c(1,5,6)]
  colnames(diccionario) <- c("columna","valor","respuesta")

  seleccion <- list()
  for (i in 1:nrow(diccionario)) {
    if(sum(!is.na(diccionario[i,]))==3){
      seleccion[[i]] <- i
    }
  }
  index <- unlist(seleccion)
  for (i in index){

    if(diccionario[i,1]==cambio){

      a <- which(index==i)
      b <- index[a+1]-1
      aswers <- diccionario[i:b,c(2,3)]
      print(aswers)
    }
  }
  aswers[,1] <- as.numeric(aswers[,1])
  aswers <- na.omit(aswers)
  sleep_2 <- encuesta[!is.na(encuesta[,cambio]), ]
  for(i in 1:nrow(sleep_2)){
    for(j in 1:nrow(aswers)){
      if(sleep_2[i,cambio]==as.numeric(aswers[j,1])){
        sleep_2[i,cambio] <- aswers[j,2]
        next
      } else {
        next
      }
    }
  }
  return(sleep_2)
}


entidad <- translate(cbasico1, dicc_cbasico1, "ENT")
escolaridad <- translate(entidad, dicc_cbasico1, "S3P1")

datos_generales <- read.csv("enpecyt_bienal_csv/conjunto_de_datos/tr_vivhog.csv")
dicc_dg <- read.csv("enpecyt_bienal_csv/diccionario_de_datos/diccionario_de_datos_vivhog.csv", na.strings = "")
definitivos <- translate(datos_generales, dicc_dg, "R_DEF")
definitivos$R_DEF <- as.factor(definitivos$R_DEF)
summary(definitivos)


escolaridad <- escolaridad[which(definitivos$R_DEF == "Entrevista completa"), ]
escolaridad <- escolaridad[!is.na(escolaridad$ENT), ]
escolaridad$ENT <- as.factor(escolaridad$ENT)
escolaridad$S3P1 <- as.factor(escolaridad$S3P1)


a <- table(escolaridad$ENT, escolaridad$S3P1)
plot(a)

library(ggplot2)
library(RColorBrewer)

nb.cols <- length(levels(escolaridad$ENT))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
reorder(LOCATION, socexp)
ggplot(escolaridad, aes(x = ENT, fill = ENT)) +
  geom_bar(color = "black", size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  facet_wrap(~S3P1) + labs(x = "Entidades Federativas", y = "N??mero de personas")


escolaridad$S4P6_2 <- as.numeric(escolaridad$S4P6_2)
nb.cols <- length(levels(escolaridad$S3P1))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
ggplot(escolaridad, aes(x = S3P1, y = S4P6_2, fill = S3P1)) +
  geom_col(size = 0.3, width = 0.70) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Nivel de educaci??n", y = "N??mero de personas") +
  ggtitle("??Cu??ntos art??culos semanales de ciencia y tecnolog??a lee en el peri??dico?")


S4P7_2
ggplot(escolaridad, aes(x = S3P1, y = S4P7_2, fill = S3P1)) +
  geom_col(size = 0.3, width = 0.70) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Nivel de educaci??n", y = "N??mero de personas") +
  ggtitle("??Cu??ntos art??culos semanales de ciencia y tecnolog??a lee en revistas?")


escolaridad <- translate(escolaridad, dicc_cbasico1, "S4P8")
ggplot(escolaridad, aes(x = S3P1, fill = S3P1)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Nivel de educaci??n", y = "N??mero de personas") +
  ggtitle("??Tiene usted acceso, o cuenta con alguna computadora?") +
  facet_wrap(~S4P8)


cbasico2 <- read.csv("enpecyt_bienal_csv/conjunto_de_datos/tr_cbasico2.csv")
dicc_basic2 <- read.csv("enpecyt_bienal_csv/diccionario_de_datos/diccionario_de_datos_cbasico2.csv", na.strings = "")
preguntas <- translate(cbasico2, dicc_basic2, "S4P31_1_1")
preguntas <- translate(preguntas, dicc_basic2, "S4P23_1")
preguntas <- translate(preguntas, dicc_basic2, "S4P22_2")
preguntas <- translate(preguntas, dicc_basic2, "S4P22_3")
preguntas <- translate(preguntas, dicc_basic2, "S4P22_8")
preguntas <- translate(preguntas, dicc_basic2, "S4P23_4")
preguntas <- translate(preguntas, dicc_basic2, "S4P23_10")


preguntas <- preguntas[which(definitivos$R_DEF == "Entrevista completa"), ]
preguntas <- preguntas[!is.na(preguntas$ENT), ]

preguntas$S4P31_1_1 <- as.factor(preguntas$S4P31_1_1)
nb.cols <- length(levels(preguntas$S4P31_1_1))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
ggplot(preguntas, aes(x = S4P31_1_1, fill = S4P31_1_1)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Opini??n", y = "N??mero de personas") +
  ggtitle("Confiamos demasiado en la fe y muy poco en la ciencia")


preguntas$S4P23_1 <- as.factor(preguntas$S4P23_1)
nb.cols <- length(levels(preguntas$S4P23_1))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
ggplot(preguntas, aes(x = S4P23_1, fill = S4P23_1)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Opini??n", y = "N??mero de personas") +
  ggtitle("La investigaci??n cient??fica y tecnol??gica juega un papel fundamental en el desarrollo tecnol??gico")


preguntas$S4P22_2 <- as.factor(preguntas$S4P22_2)
nb.cols <- length(levels(preguntas$S4P22_2))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
ggplot(preguntas, aes(x = S4P22_2, fill = S4P22_2)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Opini??n", y = "N??mero de personas") +
  ggtitle("Gracias a la ciencia y la tecnolog??a habr?? m??s oportunidades para las pr??ximas generaciones")


preguntas$S4P22_3 <- as.factor(preguntas$S4P22_3)
nb.cols <- length(levels(preguntas$S4P22_3))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
ggplot(preguntas, aes(x = S4P22_3, fill = S4P22_3)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Opini??n", y = "N??mero de personas") +
  ggtitle("La ciencia y la tecnolog??a hacen nuestras vidas m??s f??ciles, confortables y con mayores niveles de salud")


preguntas$S4P22_8 <- as.factor(preguntas$S4P22_8)
nb.cols <- length(levels(preguntas$S4P22_8))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
ggplot(preguntas, aes(x = S4P22_8, fill = S4P22_8)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Opini??n", y = "N??mero de personas") +
  ggtitle("La ciencia y la tecnolog??a ayudar??n a erradicar la pobreza y hambruna en el mundo")


preguntas$S4P23_4 <- as.factor(preguntas$S4P23_4)
nb.cols <- length(levels(preguntas$S4P23_4))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
ggplot(preguntas, aes(x = S4P23_4, fill = S4P23_4)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Opini??n", y = "N??mero de personas") +
  ggtitle("S??lo con base en la investigaci??n b??sica, aplicada y el desarrollo tecnol??gico nuestra econom??a podr?? ser m??s competitiva")


preguntas$S4P23_10 <- as.factor(preguntas$S4P23_10)
nb.cols <- length(levels(preguntas$S4P23_10))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
ggplot(preguntas, aes(x = S4P23_10, fill = S4P23_10)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 20)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Opini??n", y = "N??mero de personas") +
  ggtitle("Son mayores los beneficios generados por la investigaci??n cient??fica que los da??os asociados a dicha investigaci??n")

ggplot(escolaridad, aes(x = S3P1, fill = S3P1)) +
  geom_bar() + facet_grid(~ENT) + theme_classic()


cbasico2 <- cbasico2[,-c(1:7)]
cb3 <- cbind(S3P1 = cbasico1$S3P1, ENT = cbasico1$ENT, cbasico2)

cb3 <-translate(cb3, dicc_cbasico1, "S3P1")
cb3 <- translate(cb3, dicc_cbasico1, "ENT")
cb3 <- translate(cb3, dicc_basic2, "S4P31_1_1")
cb3 <- translate(cb3, dicc_basic2, "S4P23_1")
cb3 <- translate(cb3, dicc_basic2, "S4P22_2")
cb3 <- translate(cb3, dicc_basic2, "S4P22_3")
cb3 <- translate(cb3, dicc_basic2, "S4P22_8")
cb3 <- translate(cb3, dicc_basic2, "S4P23_4")
cb3 <- translate(cb3, dicc_basic2, "S4P23_10")

cb3 <- cb3[which(definitivos$R_DEF == "Entrevista completa"), ]
cb3 <- cb3[!is.na(cb3$ENT),]


cb3$S4P31_1_1 <- as.factor(cb3$S4P31_1_1)
nb.cols <- length(levels(cb3$S4P31_1_1))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
ggplot(cb3, aes(x = S4P31_1_1, fill = S4P31_1_1)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 15)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Opini??n", y = "N??mero de personas") +
  ggtitle("Confiamos demasiado en la fe y muy poco en la ciencia") + theme(title = element_text(size = 10)) +
  facet_wrap(~S3P1)

ggplot(cb3, aes(x = S4P23_10, fill = S4P23_10)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 15)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Opini??n", y = "N??mero de personas") +
  ggtitle("Son mayores los beneficios generados por la investigaci??n cient??fica que los da??os asociados a dicha investigaci??n") + theme(title = element_text(size = 10)) +
  facet_wrap(~S3P1)


ggplot(cb3, aes(x = S4P23_1, fill = S4P23_1)) +
  geom_bar(color = "black",size = 0.3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5),
        text = element_text(size = 12)) +
  scale_fill_manual(values = mycolors, guide = FALSE) +
  labs(x = "Opini??n", y = "N??mero de personas") +
  ggtitle("La investigaci??n cient??fica y tecnol??gica juega un papel fundamental en el desarrollo tecnol??gico")+ theme(title = element_text(size = 8))+
  facet_wrap(~ENT)



