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

summary(etiq2$EF_area_cm)
summary(etiq2$ET_area_cm)
summary(etiq2$EL_area_cm)
summary(etiq2$marbete_area_cm)

#Área de los pictogramas
etiq2$area_pict_total <-  ((pi*(etiq2$P1_altura/2)^2) +
				  (pi*(etiq2$P2_altura/2)^2) +
				  (pi*(etiq2$P3_altura/2)^2) +
				  (pi*(etiq2$P1_altura/2)^2))/1000

areas_relativas <- data.frame(EF = (etiq2$EF_area_cm/etiq2$area_envase_cm)*100,
					EL = (etiq2$EL_area_cm/etiq2$area_envase_cm)*100,
					ET = (etiq2$ET_area_cm/etiq2$area_envase_cm)*100,
					Marbete = (etiq2$marbete_area_cm/etiq2$area_envase_cm)*100)

areas_relativas$Pict_ET <- ifelse(etiq2$posicion_pictograma == 1, (etiq2$area_pict_total/etiq2$area_envase_cm) * 100, NA)
areas_relativas$Pict_EL <- ifelse(etiq2$posicion_pictograma == 2, (etiq2$area_pict_total/etiq2$area_envase_cm) * 100, NA)
areas_relativas$Pict_EF <- ifelse(etiq2$posicion_pictograma == 3, (etiq2$area_pict_total/etiq2$area_envase_cm) * 100, NA)
areas_relativas$Pict_Marbete <- ifelse(etiq2$posicion_pictograma == 4, (etiq2$area_pict_total/etiq2$area_envase_cm) * 100, NA)

areas <- list( "Etiqueta frontal" = areas_relativas$Pict_EF, "Etiqueta Trasera" = areas_relativas$Pict_ET, "Etiqueta Lateral" = areas_relativas$Pict_EL, "Marbete" = areas_relativas$Pict_Marbete)


#Figura 1
#png("Figura_1.png")
plot.new()
par(new = TRUE, mar = c(5,4,1,1))
rect(0, 0, 1, 1,
     #par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
     col = "#ebebeb", border = NA)
grid(nx = NULL, ny = NULL, col = "white", lty = 1,
     lwd = par("lwd"), equilogs = TRUE)
par (new = TRUE, mar = c(5,4,1,1))
boxplot(areas_relativas$EF, areas_relativas$ET, areas_relativas$EL, areas_relativas$Marbete,
	  at = c(1,2,3,4),  names = c("Etiq Frontal", "Etiq Trasera", "Etiq Lateral", "Marbete"),
	  ylab = "Localización", xlab = "Área relativa respecto al contenedor (%)", ylim =c(0,100), yaxp = c(0, 100, 10),cex.main = 1, 
	  cex.lab = 0.9, cex = 0.7, col = rgb(0, 0, 1, alpha = 0.4), horizontal = TRUE)
stripchart(areas, method = "jitter", pch = 10, add = TRUE, col = "red")
legend("topright", inset = 0.02, legend = "Área relativa de los símbolos", col = "red", pch =10, bg = "#FFFFFF", box.lty=0)
#dev.off()

#Análisis de los colores}
#Cuando el color de fondo es transparente (n=26) se toma el color de la etiqueta
etiq2$bg_col <- etiq2$color_fondo
etiq2$bg_col <- ifelse(etiq2$color_fondo == "Transparente", etiq2$color_etiqueta, etiq2$bg_col)


table(etiq2$color_fondo, etiq2$color_etiqueta)color_figura color_lineas


col_142_claro <- (etiq2$bg_col == "Blanco" | etiq2$bg_col == "Crema") & etiq2$color_figura == "Negro" & etiq2$color_lineas == "Rojo"
col_142_oscuro <- etiq2$bg_col == "Negro" & (etiq2$color_figura == "Blanco" | etiq2$color_figura == "Crema") & etiq2$color_lineas == "Rojo"
col_142_transp <- etiq2$bg_col == "Transparente"

