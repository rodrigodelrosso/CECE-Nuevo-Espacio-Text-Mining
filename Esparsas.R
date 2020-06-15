##########################################################
#      ANÁLISIS DE INFORMACIÓN CUALITATIVA USANDO R      #
#	         CONFERENCIAS CECE - UBA                 #
#            FACULTAD DE CIENCIAS ECONÓMICAS             #
#              UNIVERSIDAD DE BUENOS AIRES               #
##########################################################

# Expositores: Martin Masci - Rodrigo Del Rosso
# Moderador  : Diego Parras
# Lunes 15 de Junio de 2020

.rs.restartR()

rm(list = ls())

library('Matrix')

M1 <- matrix(data = 0, 
             nrow = 1000, 
             ncol = 1000)

data.class(M1)

M2 <- Matrix(data = 0, 
             nrow = 1000, 
             ncol = 1000, 
             sparse = T)

data.class(M2)

object.size(M1)
object.size(M2)

format(object.size(M1), 
       unit = "Mb")

format(object.size(M2), 
       unit = "Mb")

M1[500, 500] <- 1

M2[500, 500] <- 1

format(object.size(M1), 
       unit = "Mb")

format(object.size(M2), 
       unit = "Mb")

## NO SIEMPRE CONVIENE UTILIZAR UNA MATRIZ ESPARSA
## EJEMPLO DE MATRIZ CON VALORES DE UNA NORMAL ESTÁNDAR

M3 <- matrix(rnorm(1000000), 
             nrow = 1000, 
             ncol = 1000)

M4 <- Matrix(rnorm(1000000), 
             nrow = 1000, 
             ncol = 1000, 
             sparse = TRUE)

format(object.size(M3), 
       unit = "Mb")

format(object.size(M4), 
       unit = "Mb")

## UNO PUEDE GENERAR MATRICES ESPARSAS INDICANDO LA POSICIÓN 
## Y EL VALOR DE LOS ELEMENTOS NO NULOS

M1 <- sparseMatrix(i = c(2,20), 
                   j = c(3, 5), 
                   x = c(4,8))
class(M1)
print(M1)


M2 <- sparseMatrix(i = c(2,20), 
                   j = c(3,5), 
                   x = c(4,8), 
                   dims = c(25,10))

print(M2)

M2 <- sparseMatrix(i = c(2,20), 
                   j = c(3,5), 
                   x = c(4,8), 
                   dims = c(25000,10000))

print(M2)

format(object.size(M1), 
       units = "Mb")

format(object.size(M2), 
       units = "Mb")

rm(M1,M2)
gc()
