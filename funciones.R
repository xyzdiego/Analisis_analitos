# Funciones para el análisis gráfico de la base de datos de unas plantas
# en relación a los genotipos estudiados dentro de una tesis de 
# maestría.
#
# Este código fue elaborado por Diego Andres Benitez.
#
# La función Selector, sirve como base de las demás funciones. Permite
# la generación de una base de datos ordenada lista para el análisis gráfico
# que realizará EDA_boxplot(), o el análisis de diferencias que realizará
# test_dif().
# La función EDA_boxplot() realiza un Análisis Exploratorio de Datos (EDA)
# del cual se obtiene un boxplot resultado de acuerdo a cada componente
# o analito del estudio de maiz.
# La función IC_teorico() actua teniendo como base la función EDA_boxplot(),
# pero permite al usuario añadir el intervalo de confianza teórico sobre
# el cual debería encontrarse cada analito.
# La función test_dif() actua teniendo como base la función Selector(), con
# esa información permite realizar un análisis de diferencias entre genotipos
# de acuerdo con el analito a trabajar. 

Selector <- function(datos = semilla, i = i, variable = "SEMILLA"){
    datos[i, ] <- sub(",", ".", datos[i, ], fixed = T)
    nombre <<- datos[i, 1]
    datos[i, ] <- as.numeric(datos[i, 1:length(datos[1, ])])
    datos[i, 1] <- nombre
    ordenado <- datos[i, ] %>% gather(key = "LINEA", value = "Valor", 2:19) %>%
        arrange(desc(LINEA))
    ordenado <- ordenado %>% separate(col = LINEA, 
                                      into = c("Tipo", "Subtipo", "Linea"), 
                                      sep = " ") %>%
        unite(col = Tipo, sep = "_", Tipo, Subtipo)
    ordenado$Tipo <- as.factor(ordenado$Tipo)
    ordenado$Valor <- as.numeric(ordenado$Valor)
    return(ordenado)
}

EDA_boxplot <- function(datos = semilla, i = i, variable = "SEMILLA"){
    if(!require(dplyr) & !require(tidyr) & !require(ggplot2)){
        stop("Por favor instale los paquetes dplyr y tidyr")
    }
    ordenado <- Selector(datos, i, variable)
    cla1 <- c("#1F618D", "#A04000", "#2E86C1", "#AED6F1", "#D35400", "#EDBB99")
    g1 <<- ordenado %>% ggplot(aes(Tipo, Valor, fill = Tipo)) + 
        geom_boxplot() + stat_boxplot(geom ='errorbar', width = 0.5, alpha = 0.3) +
        theme_bw() + scale_fill_manual(values = cla1) +
        labs(x = "GENOTIPO",
             y = nombre,
             title = paste(nombre, "DEL", variable, "DE ACUERDO AL GENOTIPO")) +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, color = "gray25",
                                        size =  18, face = "bold"),
              axis.title.x = element_text(color = "gray25"),
              axis.title.y = element_text(color = "gray25"))
}

IC_teorico <- function(datos = semilla, i = 1, variable = "SEMILLA",
                  min = 0, max = 15){
    EDA_boxplot(datos = datos, i = i, variable = variable)
    ICG <- data.frame(minimo = rep(min, 18),
                      maximo = rep(max, 18))
    g1 + geom_errorbar(aes(ymin=ICG$minimo, ymax=ICG$maximo), 
                       width=0.2, size=1.0, alpha=0.05)
}

test_dif <- function(datos = semilla, i = i, variable = "SEMILLA"){
    if(!require(knitr) & !require(car)){
        stop("Por favor instale los paquetes dplyr y tidyr")
    }
    datos <- Selector(datos = semilla, i = i, variable = "SEMILLA")
    Homocedasticidad <- leveneTest(datos$Valor ~ datos$Tipo, center = "median")
    ks_test <- kruskal.test(datos$Valor ~ datos$Tipo)
    pos_hoc <- pairwise.wilcox.test(datos$Valor, datos$Tipo, p.adjust.method = "holm")
    tabla <- kable(pos_hoc$p.value)
    salida <<- list(nombre = nombre,
                    Homocedasticidad = Homocedasticidad, 
                    ks_test = ks_test, 
                    pos_hoc = pos_hoc, 
                    tabla = tabla)
    message('El argumento "salida$" le permitira tener acceso a los elementos del análisis')
}