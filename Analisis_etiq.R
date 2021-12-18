library(ggplot2)

#Cargamos la base de datos con la información de los envases de alcohol
setwd("C:/Users/norberto.llanes/Desktop/Etiquetado Bebidas Alcoholicas/Analisis R/15dic2021")
etiq <- read.csv("BD_etiq_alcohol-1_2.csv", header = TRUE)
str(etiq)
summary(etiq)

#Eliminamos los registros que contienen  vacios (valores perdidos)
etiq2 <- subset(etiq, h != "NA")
summary(etiq2)

#Recodificamos los distintos tipos de bebidas a categorias generales
table(etiq2$tipo_bebida)

etiq2$tipo_bebida2 <- etiq2$tipo_bebida
etiq2$tipo_bebida2 <- ifelse(etiq2$tipo_bebida == "Ginebra", "Destilado", etiq2$tipo_bebida2)
etiq2$tipo_bebida2 <- ifelse(etiq2$tipo_bebida == "Mezcal", "Destilado", etiq2$tipo_bebida2)
etiq2$tipo_bebida2 <- ifelse(etiq2$tipo_bebida == "Ron", "Destilado", etiq2$tipo_bebida2)
etiq2$tipo_bebida2 <- ifelse(etiq2$tipo_bebida == "Tequila", "Destilado", etiq2$tipo_bebida2)
etiq2$tipo_bebida2 <- ifelse(etiq2$tipo_bebida == "Vodka", "Destilado", etiq2$tipo_bebida2)
etiq2$tipo_bebida2 <- ifelse(etiq2$tipo_bebida == "Whiskey", "Destilado", etiq2$tipo_bebida2)
etiq2$tipo_bebida2 <- ifelse(etiq2$tipo_bebida == "Rompope", "Licor", etiq2$tipo_bebida2)

#Descriptivos de la Tabla 1
table(etiq2$tipo_bebida2)
table(etiq2$material_envase)
table(etiq2$origen)
summary(etiq2$capacidad)

#Calculo de las áreas del envase, de las etiquetas y de los pictogramas
#Area del envase
etiq2$area_envase_cm <- NA
etiq2$area_envase_cm <- ifelse(etiq2$forma_envase == "Cilindro", 
					(2*pi*(etiq2$diametro_base/2)*etiq2$h)/1000, etiq2$area_envase)
etiq2$area_envase_cm <- ifelse(etiq2$forma_envase == "Prisma rectangular", 
					(2*(etiq2$ancho_base + etiq2$largo_base)*etiq2$h)/1000, etiq2$area_envase)
summary(etiq2$area_envase_cm)

#Area de las etiquetas
#Etiqueta frontal
etiq2$EF_area_cm <- ((etiq2$EF1_ancho*etiq2$EF1_largo)+(etiq2$EF2_ancho*etiq2$EF2_largo))/1000

#Etiqueta trasera
etiq2$ET_area_cm <- ((etiq2$ET1_ancho*etiq2$ET1_largo)+(etiq2$ET2_ancho*etiq2$ET2_largo))/1000

#Etiqueta lateral
etiq2$EL_area_cm <- ((etiq2$EL1_ancho*etiq2$EL1_largo)+(etiq2$EL2_ancho*etiq2$EL2_largo))/1000

#Marbete
etiq2$marbete_area_cm <- (2*pi*(etiq2$marbete_diametro/2)*etiq2$marbete_altura)/1000

ef <- summary(etiq2$EF_area_cm)
et <- summary(etiq2$ET_area_cm)
el <- summary(etiq2$EL_area_cm)
marbete <- summary(etiq2$marbete_area_cm)


areas_relativas <- data.frame(EF = (etiq2$EF_area_cm/etiq2$area_envase_cm)*100,
					EL = (etiq2$EL_area_cm/etiq2$area_envase_cm)*100,
					ET = (etiq2$ET_area_cm/etiq2$area_envase_cm)*100,
					Marbete = (etiq2$marbete_area_cm/etiq2$area_envase_cm)*100)

png("Figura_1.png", res = 300, unit = "px", width = 1800, height = 1440)
plot.new()
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
     col = "#ebebeb")
grid(nx = NULL, ny = NULL, col = "white", lty = 1,
     lwd = par("lwd"), equilogs = TRUE)
par(new = TRUE)
boxplot(areas_relativas, xlab = "Localización de la etiqueta",
	  ylab = "%", cex.main = 1, cex.lab = 0.9, cex = 0.7, col = rgb(0, 0, 1, alpha = 0.4),
        main = "Área relativa de las etiquetas de acuerdo a su localización")
dev.off()

