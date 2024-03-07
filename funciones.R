# Funciones para el an?lisis gr?fico de la base de datos de unas plantas
# en relaci?n a los genotipos estudiados dentro de una tesis de 
# maestr?a.
#
# Este c?digo fue elaborado por Diego Andres Benitez.
#
# La funci?n Selector, sirve como base de las dem?s funciones. Permite
# la generaci?n de una base de datos ordenada lista para el an?lisis gr?fico
# que realizar? EDA_boxplot(), o el an?lisis de diferencias que realizar?
# test_dif().
# La funci?n EDA_boxplot() realiza un An?lisis Exploratorio de Datos (EDA)
# del cual se obtiene un boxplot resultado de acuerdo a cada componente
# o analito del estudio de maiz.
# La funci?n IC_teorico() actua teniendo como base la funci?n EDA_boxplot(),
# pero permite al usuario a?adir el intervalo de confianza te?rico sobre
# el cual deber?a encontrarse cada analito.
# La funci?n test_dif() actua teniendo como base la funci?n Selector(), con
# esa informaci?n permite realizar un an?lisis de diferencias entre genotipos
# de acuerdo con el analito a trabajar. 

Selector <- function(datos = semilla, i = i, variable = "grain"){
    datos[i, ] <- sub(",", ".", datos[i, ], fixed = T)
    nombre <<- datos[i, 1]
    datos[i, ] <- as.numeric(datos[i, 1:length(datos[1, ])])
    datos[i, 1] <- nombre
    ordenado <- datos[i, ] %>% gather(key = "LINEA", value = "Valor", 2:19) %>%
        arrange(desc(LINEA))
    ordenado <- ordenado %>% separate(col = LINEA, 
                                      into = c("Tipo", "Subtipo", "Linea"), 
                                      sep = " ") %>%
        unite(col = Tipo, sep = "", Tipo, Subtipo)
    ordenado$Tipo <- as.factor(ordenado$Tipo)
    ordenado$Valor <- as.numeric(ordenado$Valor)
    return(ordenado)
}

EDA_boxplot <- function(datos = semilla, i = i, variable = "grain"){
    if(!require(dplyr) & !require(tidyr) & !require(ggplot2) &
       !require(stringr) & !require(tokenizers)){
        stop("Por favor instale los paquetes tidyverse y stringr")
    }
    ordenado <- Selector(datos, i, variable)
    cla1 <- c("gray16", "gray32", "gray48", "gray64", "gray80", "gray96")
    g1 <<- ordenado %>% 
        mutate(Tipo = fct_relevel(Tipo, 
                      "HibrydC", "HibrydT", "C1", "C2", "T1", "T2")) %>% 
        ggplot(aes(Tipo, Valor, fill = Tipo)) + 
        stat_boxplot(geom ='errorbar') + geom_boxplot() +
        theme_bw() + scale_fill_manual(values = cla1) +
        labs(x = "Genotype",
             y = str_to_title(nombre) %>% sub("Dw", "DW", .),
             title = sub("(%DW)|(%)", c(""), nombre) %>% 
               str_extract_all("[:alpha:]+") %>% unlist() %>% 
               paste(collapse = " ") %>% 
               str_to_title(),
             caption = "AFSI, 2020") +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, color = "gray25",
                                        size =  14, face = "bold"),
              plot.caption = element_text(hjust = 1, color = "gray25",
                                        size =  8),
              axis.title.x = element_text(color = "gray25"),
              axis.title.y = element_text(color = "gray25"),
              axis.text.x = element_text(angle = 45))
}

IC_teorico <- function(datos = semilla, i = 1, variable = "grain",
                  min = 0, max = 15){
    if(!require(knitr) & !require(tidyr)){
        stop("Por favor instale los paquetes knitr y car")
    }
    EDA_boxplot(datos = datos, i = i, variable = variable)
    ICG <- data.frame(minimo = rep(min, 18),
                      maximo = rep(max, 18))
    grafico <- g1 + geom_hline(yintercept = ICG$minimo, color = "black",
                               size = 1.0, alpha = 0.45) +
        geom_text(x = 3.5, y = ICG$minimo + 0.15, 
                  label = "Minimum range (Literature)", 
                  color = "gray50", size = 3) +
        geom_hline(yintercept = ICG$maximo, color = "black",
                   size = 1.0, alpha = 0.45) +
        geom_text(x = 3.5, y = ICG$maximo - 0.2, 
                  label = "Maximum range (Literature)", 
                  color = "gray50", size = 3)
    Tabla <- Selector(i = i, datos = datos) %>% group_by(Tipo) %>% 
            summarise(Media = round(mean(Valor),2),
                      Desv = round(sd(Valor),3),
                      Mediana = median(Valor),
                      Max_rep = max(Valor),
                      Min_rep = min(Valor),
                      IQR_max = Mediana + IQR(Valor),
                      IQR_min = Mediana - IQR(Valor),
                      Min_lit = min,
                      Max_lit = max) %>%
            mutate(Tipo = fct_relevel(Tipo, 
            "HibrydC", "HibrydT", "C1", "C2", "T1", "T2")) %>% 
            kable()
    mediana <<- list(grafico = grafico,
                    Tabla = Tabla,
                    nombre = nombre)
}

test_dif <- function(datos = semilla, i = i, variable = "grain"){
    if(!require(knitr) & !require(car)){
        stop("Por favor instale los paquetes knitr y car")
    }
    datos <- Selector(datos = semilla, i = i, variable = "grain")
    Homocedasticidad <- leveneTest(datos$Valor ~ datos$Tipo, center = "median")
    kw_test <- kruskal.test(datos$Valor ~ datos$Tipo)
    pos_hoc <- pairwise.wilcox.test(datos$Valor, datos$Tipo, 
                                    p.adjust.method = "bonf")
    tabla <- kable(pos_hoc$p.value)
    salida <<- list(nombre = nombre,
                    Homocedasticidad = Homocedasticidad, 
                    kw_test = kw_test, 
                    pos_hoc = pos_hoc, 
                    tabla = tabla)
    message('El argumento "salida" le permitira tener acceso a los elementos del an?lisis')
}