# Árbol elemental con librería  rpart
# Debe tener instaladas las librerías  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
library("data.table")
library("rpart")
library("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/maxvega/Downloads/MCD/DataMining") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/myGAnalisis.csv")
n <- dim(dataset)[1]

dtrain <- dataset[,ganancia_promedio := ganancia_promedio/1000000]

# genero el modelo,  aquí se construye el árbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "ganancia_promedio ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.5, # esto significa no limitar la complejidad de los splits
        minsplit = 50, # mínima cantidad de registros para que se haga el split
        minbucket = 4, # tamaño mínimo de una hoja
        maxdepth = 6
) # profundidad máxima del árbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# grafico el arbol
pdf(paste0('./pictures/K101_022.pdf'))
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)
dev.off()

