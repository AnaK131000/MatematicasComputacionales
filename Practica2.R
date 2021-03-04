# list all datasets in the package
library(help = "datasets")

# Base de datos del Titanic
data("Titanic")
head(Titanic)

#DATOS QUE CONTIENE y NO CONTIENE
Titanic<- data.frame(Titanic)
head(Titanic$Passenger)
head(Titanic$Survived)
head(Titanic$Pclass)
head(Titanic$Name)
head(Titanic$Sex)
head(Titanic$Ticket)
######################################################################
#Entender los datos a través de la estadística descriptiva

#Descripcion de variables
names(Titanic)

#clase
class(Titanic)

# Los datos de las tablas
tail(Titanic)

#Numero de veces que cada valor aparece en los datos
summary(Titanic)

#Para conocer el tipo de variables 
str(Titanic)

# Longitud del vector 
length(Titanic)

#Numero de filas
nrow(Titanic)

#Numero de Columnas
ncol(Titanic)

#Media
mean(Titanic)

#Mediana
median(Titanic)

#Desviacion Estandar
sd(Titanic)

#Minimo
min(Titanic)

#Maximo
max(Titanic)


###################################################################
# Understand Your Data Using Data Visualization

#Tabla de frecuencias en consola
table(Titanic)

#Graficas de barras

Titanic<- data.frame(Titanic)

#Por CLASE
clase <- aggregate(Titanic$Freq, by=list(Category=Titanic$Class), FUN=sum)
barplot(clase[,2], width = 1, xlab = "Clases", ylab = "Cantidad", names.arg = clase[,1])

#Por SEXO
sexo <- aggregate(Titanic$Freq, by=list(Category=Titanic$Sex), FUN=sum)
barplot(sexo[,2], width = 1, xlab = "Sexo", ylab = "Cantidad", names.arg = sexo[,1])

#Por EDAD
Age <- aggregate(Titanic$Freq, by=list(Category=Titanic$Age), FUN=sum)
barplot(Age[,2], width = 1, xlab = "Edad", ylab = "Cantidad", names.arg = Age[,1])

#Por cantidad de SOBREVIVIENTES
Survived <- aggregate(Titanic$Freq, by=list(Category=Titanic$Survived), FUN=sum)
barplot(Survived[,2], width = 1, xlab = "Sobrevivientes", ylab = "Cantidad", names.arg = Survived[,1])


#Histograma 
hist(Titanic$Freq,main="Histograma de Frecuencias",xlab="Numero de pasajeros",ylab="Frecuencia",col="purple")


#Diagrama de dispersion
plot(x = Titanic$Class, y = Titanic$Sex,main="Relacion entre Clases y Sexo")
plot(x = Titanic$Class, y = Titanic$Survived,main="Relacion entre Clases y Sobrevivientes")
plot(x = Titanic$Class, y = Titanic$Age,main="Relacion entre Clases y Edad")

#Diagrama de caja 
boxplot(x = Titanic$Freq,main="Distribucion del numero de pasajeros")


     


