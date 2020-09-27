library(tidyr)
library(dplyr)
library(ggplot2)
source("funciones.R")

plantas <- read.csv("DATA_EQS.csv", encoding = "UTF-8")

y <- c("Componente", rep(c(rep("LINEA_", 6), rep("HIBRIDO_", 3)), 2))
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

IC_teorico(i = 4, min = 11, max = 18)
test_dif(i = 4)
salida$tabla
salida$nombre