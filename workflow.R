library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(knitr)
library(stringr)
source("funciones.R")

plantas <- read.csv2("DATA_EQS.csv", encoding = "UTF-8")

y <- c("Componente", rep(c(rep("", 6), rep("Hibryd ", 3)), 2))
w <- c("", rep("T", 9), rep("C", 9))
y <- paste0(y, w)
z <- c("", rep(c(paste(1, LETTERS[1:3]), paste(2, LETTERS[1:3]), 
                 paste(1, LETTERS[1:3])), 2))
componente <- paste(y, z)

semilla <- plantas[13:20, ]
semilla <- semilla[2:8, ]
names(semilla) <- componente

forraje <- plantas[1:11, ]
forraje <- forraje[2:11, ]
names(forraje) <- componente

IC_teorico(i = 2, datos = forraje, min = 5.72, max = 17.26)
IC_teorico(i = 6, min = 9.72, max = 14.26)
mediana$grafico

grain <- data.frame(min = c(7, 5.72, 1.363, 0.49, 0.616, 0, 77.4), 
                    max = c(23, 17.26, 7.83, 5.5, 6.282, 0, 89.7))

myplots <- vector('list', 7)

for (i in c(1:5,7)) {
  message(i)
  myplots[[i]] <- local({
    i <- i
    p1 <- IC_teorico(i=i, min = grain[i,1], max = grain[i,2])
    print(p1)
  })
}

cowplot::plot_grid(myplots[[1]]$grafico,
                   myplots[[2]]$grafico,
                   myplots[[3]]$grafico,
                   myplots[[4]]$grafico,
                   myplots[[5]]$grafico,
                   myplots[[7]]$grafico, nrow = 3,
                   labels = "AUTO")


forage <- data.frame(min = c(55.3, 3.14, 1.04, 12.5, 0.66, 0,0,0,0, 66.9),
                     max = c(87.1, 16.32, 6.755, 42, 13.2, 0,0,0,0, 92.9))

myplots <- vector('list', 10)

for (i in c(1:5,10)) {
  message(i)
  myplots[[i]] <- local({
    i <- i
    p1 <- IC_teorico(datos = forraje, variable = "forage",
                     i=i, min = forage[i,1], max = forage[i,2])
    print(p1)
  })
}

cowplot::plot_grid(myplots[[1]]$grafico,
                   myplots[[2]]$grafico,
                   myplots[[3]]$grafico,
                   myplots[[4]]$grafico,
                   myplots[[5]]$grafico,
                   myplots[[10]]$grafico, nrow = 3,
                   labels = "AUTO")

mediana$Tabla
mediana$grafico
test_dif(i = 2)
salida$tabla
salida$nombre
salida$kw_test

IC_teorico(datos = forraje, i = 3, variable = "FORRAJE")
test_dif(datos = forraje, i = 3, variable = "FORRAJE")
salida$tabla
salida$nombre

for(i in 1:nrow(semilla)){
    semilla[i, ] <- sub(",", ".", semilla[i, ], fixed = T)
    nombre <- semilla[i, 1]
    semilla[i, ] <- as.numeric(semilla[i, 1:length(semilla[1, ])])
    semilla[i, 1] <- nombre
}
semilla

ordenado <- semilla %>% gather(key = "LINEA", value = "Valor", 2:19) %>%
    arrange(desc(LINEA))
ordenado <- ordenado %>% separate(col = LINEA, 
                                  into = c("Tipo", "Subtipo", "Linea"), 
                                  sep = " ") %>%
    unite(col = Tipo, sep = "", Tipo, Subtipo)
ordenado$Tipo <- as.factor(ordenado$Tipo)
ordenado$Valor <- as.numeric(ordenado$Valor)
kruskal.test(ordenado$Valor ~ ordenado$Tipo)
pairwise.wilcox.test(ordenado$Valor, ordenado$Tipo, p.adjust.method = "holm")
cla1 <- c("#1F618D", "#A04000", "#2E86C1", "#AED6F1", "#D35400", "#EDBB99")
p1 <- ordenado %>% 
    mutate(Tipo = fct_relevel(Tipo, 
                              "HibrydC", "HibrydT", "C1", "C2", "T1", "T2")) %>% 
    ggplot(aes(Tipo, Valor, fill = Tipo)) + 
    geom_boxplot() + stat_boxplot(geom ='errorbar', width = 0.5, alpha = 0.3) +
    theme_bw() + scale_fill_manual(values = cla1) +
    labs(x = "GENOTIPO",
         y = "ANALITOS",
         title = paste("ANALITOS", "DE", "GRANO", "DE ACUERDO AL GENOTIPO")) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, color = "gray25",
                                    size =  18, face = "bold"),
          axis.title.x = element_text(color = "gray25"),
          axis.title.y = element_text(color = "gray25"))



for(i in 1:nrow(forraje)){
    forraje[i, ] <- sub(",", ".", forraje[i, ], fixed = T)
    nombre <- semilla[i, 1]
    forraje[i, ] <- as.numeric(forraje[i, 1:length(forraje[1, ])])
    forraje[i, 1] <- nombre
}

ordenado <- forraje %>% gather(key = "LINEA", value = "Valor", 2:19) %>%
    arrange(desc(LINEA))
ordenado <- ordenado %>% separate(col = LINEA, 
                                  into = c("Tipo", "Subtipo", "Linea"), 
                                  sep = " ") %>%
    unite(col = Tipo, sep = "", Tipo, Subtipo)
ordenado$Tipo <- as.factor(ordenado$Tipo)
ordenado$Valor <- as.numeric(ordenado$Valor)
kruskal.test(ordenado$Valor ~ ordenado$Tipo)
pairwise.wilcox.test(ordenado$Valor, ordenado$Tipo, p.adjust.method = "bonf")
cla1 <- c("#1F618D", "#A04000", "#2E86C1", "#AED6F1", "#D35400", "#EDBB99")
p2 <- ordenado %>% 
    mutate(Tipo = fct_relevel(Tipo, 
                              "HibrydC", "HibrydT", "C1", "C2", "T1", "T2")) %>% 
    ggplot(aes(Tipo, Valor, fill = Tipo)) + 
    geom_boxplot() + stat_boxplot(geom ='errorbar', width = 0.5, alpha = 0.3) +
    theme_bw() + scale_fill_manual(values = cla1) +
    labs(x = "GENOTIPO",
         y = "ANALITOS",
         title = paste("ANALITOS", "DE", "FORRAJE", "DE ACUERDO AL GENOTIPO")) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, color = "gray25",
                                    size =  18, face = "bold"),
          axis.title.x = element_text(color = "gray25"),
          axis.title.y = element_text(color = "gray25"))

cowplot::plot_grid(p1, p2, labels = "AUTO")
