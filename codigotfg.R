# Eliminar environment 

rm(list = ls())

# Importación dataset

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

#dataset cuestionario online
dataset <- read_excel("~/Cuestionario TFG (respuestas).xlsx")
View(dataset)

#RANDOMNESS CHECK, t-test grupo par e impar y grupo hombres
grupo_par <- dataset[dataset[, 3] == "Par", ]
grupo_impar <- dataset[dataset[, 3] == "Impar", ]

t.test(grupo_par[,11], grupo_impar[,11])

grupoparhombres <- na.exclude(grupo_par[, 12])
grupoimparhombres <- na.exclude(grupo_impar[, 12])
grupopar_num<-ifelse(grupoparhombres=="Hombre",1,0)
grupoimpar_num<-ifelse(grupoimparhombres=="Hombre",1,0)

t_test_genero<-t.test(grupopar_num,grupoimpar_num)

print(t_test_genero)



#Manipulation Check (respuestas sí en grupo par e impar)
grupoparmanipulation <- na.exclude(grupo_par[, 15])
grupoimparmanipulation <- na.exclude(grupo_impar[, 15])
grupoparmanipulation_num<-ifelse(grupoparmanipulation=="Sí",1,0)
grupoimparmanipulation_num<-ifelse(grupoimparmanipulation=="Sí",1,0)


t_test_manipulation<-t.test(grupoparmanipulation_num,grupoimparmanipulation_num)

print(t_test_manipulation)



#Sesgo encuadre
#t-test cuestionario online
grupo1encuadre<-na.exclude(dataset[,4])
grupo2encuadre<-na.exclude(dataset[,7])

grupo1encuadre_num<-ifelse(grupo1encuadre=="Sí",1,0)
grupo2encuadre_num<-ifelse(grupo2encuadre=="Sí",1,0)

t_test_1<-t.test(grupo1encuadre_num,grupo2encuadre_num)

print(t_test_1)

#t-test para chatgpt con sesgo encuadre

dataset2 <- read_excel("~/tfgchat.xlsx")
grupo1encuadrechat<-na.exclude(dataset2[,1])
grupo2encuadrechat<-na.exclude(dataset2[,2])

grupo1encuadrechat_num<-ifelse(grupo1encuadrechat=="Si",1,0)
grupo2encuadrechat_num<-ifelse(grupo2encuadrechat=="Si",1,0)

t_test_2<-t.test(grupo1encuadrechat_num,grupo2encuadrechat_num)

print(t_test_2)


#t-test humanos e IA comparar % de opción A en humanos y en IA, para ver si el tamaño del sesgo en caso de existir es significativamente diferente
t_test_encuadreIAhumanosgrupo1<-t.test(grupo1encuadre_num,grupo1encuadrechat_num)
print(t_test_encuadreIAhumanosgrupo1)


#Sesgo aversión a las pérdidas
#t-test cuestionario online
grupo1perdidas<-na.exclude(dataset[,5])
grupo2perdidas<-na.exclude(dataset[,8])

grupo1perdidas_num<-ifelse(grupo1perdidas=="La cartera A ofrece una ganancia segura de 100$. Esto significa que independientemente de las condiciones del mercado, obtendrás un beneficio fijo de 100$.",1,0)
grupo2perdidas_num<-ifelse(grupo2perdidas=="La cartera A ofrece una perdida segura de 100$. Esto significa que independientemente de las condiciones del mercado, incurrirás en una pérdida fija de 100$",1,0)

t_test_3<-t.test(grupo1perdidas_num,grupo2perdidas_num)

print(t_test_3)

#t-test chatgpt 

grupo1perdidaschat<-na.exclude(dataset2[,4])
grupo2perdidaschat<-na.exclude(dataset2[,5])

grupo1perdidaschat_num<-ifelse(grupo1perdidaschat=="A",1,0)
grupo2perdidaschat_num<-ifelse(grupo2perdidaschat=="A",1,0)

t_test_4<-t.test(grupo1perdidaschat_num,grupo2perdidaschat_num)

print(t_test_4)

#t-test humanos e IA comparar % de opción A en humanos y en IA, para ver si el tamaño del sesgo en caso de existir es significativamente diferente
t_test_perdidasIAhumanosgrupo1<-t.test(grupo1perdidas_num,grupo1perdidaschat_num)
print(t_test_perdidasIAhumanosgrupo1)
t_test_perdidasIAhumanosgrupo2<-t.test(grupo2perdidas_num,grupo2perdidaschat_num)
print(t_test_perdidasIAhumanosgrupo2)


#Sesgo de rebaño/herding
#t-test cuestionario online
grupo1herding<-na.exclude(dataset[,6])
grupo2herding<-na.exclude(dataset[,9])

grupo1herding_num<-ifelse(grupo1herding=="Inversión A: está ganando popularidad rápidamente y muchos analistas la están recomendando. Sus características son las siguientes: Rendimiento promedio anual: 5.2%, riesgo: bajo e historial de crecimiento: constante pero no espectacular",1,0)
grupo2herding_num<-ifelse(grupo2herding=="Inversión A: rendimiento promedio anual: 5.2%, riesgo: bajo e historial de crecimiento: constante pero no espectacular",1,0)

t_test_5<-t.test(grupo1herding_num,grupo2herding_num)

print(t_test_5)


#herding chatgpt
grupo1herdingchat<-na.exclude(dataset2[,7])
grupo2herdingchat<-na.exclude(dataset2[,8])

grupo1herdingchat_num<-ifelse(grupo1herdingchat=="A",1,0)
grupo2herdingchat_num<-ifelse(grupo2herdingchat=="A",1,0)

t_test_6<-t.test(grupo1herdingchat_num,grupo2herdingchat_num)

print(t_test_6)


#Filtracion conocimiento avanzado e intermedio (análisis posteriores)
filas_deseadas <- c()

for(i in 1:nrow(dataset)) {
  # Verifica si la columna 2 en la fila i contiene "Intermedio" o "Avanzado"
  if(dataset[i, 2] %in% c("Intermedio", "Avanzado")) {
    # Si es así, agrega el índice de la fila al vector de filas deseadas
    filas_deseadas <- c(filas_deseadas, i)
  }
}

# nuevo dataset filtrado
dataset_filtrado <- dataset[filas_deseadas, ]


# Aplicar el mismo proceso que antes pero con el dataset filtrado
grupo1encuadrefiltrado <- na.exclude(dataset_filtrado[, 4])
grupo2encuadrefiltrado <- na.exclude(dataset_filtrado[, 7])


grupo1encuadrefiltrado_num<-ifelse(grupo1encuadrefiltrado=="Sí",1,0)
grupo2encuadrefiltrado_num<-ifelse(grupo2encuadrefiltrado=="Sí",1,0)

t_test_1filtrado<-t.test(grupo1encuadrefiltrado_num,grupo2encuadrefiltrado_num)

print(t_test_1filtrado)


grupo1perdidasfiltrado <- na.exclude(dataset_filtrado[, 5])
grupo2perdidasfiltrado <- na.exclude(dataset_filtrado[, 8])


grupo1perdidasfiltrado_num<-ifelse(grupo1perdidasfiltrado=="La cartera A ofrece una ganancia segura de 100$. Esto significa que independientemente de las condiciones del mercado, obtendrás un beneficio fijo de 100$.",1,0)
grupo2perdidasfiltrado_num<-ifelse(grupo2perdidasfiltrado=="La cartera A ofrece una perdida segura de 100$. Esto significa que independientemente de las condiciones del mercado, incurrirás en una pérdida fija de 100$",1,0)

t_test_2filtrado<-t.test(grupo1perdidasfiltrado_num,grupo2perdidasfiltrado_num)

print(t_test_2filtrado)


grupo1herdingfiltrado <- na.exclude(dataset_filtrado[, 6])
grupo2herdingfiltrado <- na.exclude(dataset_filtrado[, 9])


grupo1herdingfiltrado_num<-ifelse(grupo1herdingfiltrado=="Inversión A: está ganando popularidad rápidamente y muchos analistas la están recomendando. Sus características son las siguientes: Rendimiento promedio anual: 5.2%, riesgo: bajo e historial de crecimiento: constante pero no espectacular",1,0)
grupo2herdingfiltrado_num<-ifelse(grupo2herdingfiltrado=="Inversión A: rendimiento promedio anual: 5.2%, riesgo: bajo e historial de crecimiento: constante pero no espectacular",1,0)

t_test_3filtrado<-t.test(grupo1herdingfiltrado_num,grupo2herdingfiltrado_num)

print(t_test_3filtrado)

