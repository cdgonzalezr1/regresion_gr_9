#Instalando paquetes
install.packages("readr")

#Cargando paquetes
library(readr)


#Lectura de datos
real_state_df= read.csv("Train_real_state.csv")
print(head(real_state_df))


#Análisis descriptivo
colnames(real_state_df)

var=real_state_df$SalePrice
summary(var)
plot(density(var))
box=boxplot(var)
box$stats
box$conf
box$out
cat("La variable SalePrice es nuestra variable dependiente, es decir, es la que nos interesa predecir. Presenta una cola a la derecha con un grupo escaso de casas con un precio de venta muy alto. También da la impresión de dividirs een 4 grupos: casas con precios bajos, medios, medio altos y muy altos.El boxplot indica que hay 25 potenciales datos que pueden considerarse como atípicos pero los evaluaremos posteriormente.\n")

cat("Para monitorear la posible relación entre variebles idnependientes y los rpecios de venta sospechosos crearemos una variable que nos permita identificar si una casa tiene o no una venta muy alta.\n")
real_state_df[real_state_df$SalePrice>=min(box$out),'atypicalSalePrice']=TRUE
real_state_df[real_state_df$SalePrice<min(box$out),'atypicalSalePrice']=FALSE


real_state_df$YearBuilt=as.character(real_state_df$YearBuilt)
var=real_state_df$YearBuilt
barplot(table(var))
barplot(table(real_state_df[real_state_df$atypicalSalePrice==TRUE,'YearBuilt']))
cat("La variable YearBuilt es una variable categórica que presenta una distribución de frecuencia del año de construcción de la vivienda. Se observa que hay una distribución ciclica con periodos de auge de las construcciones y periodos de baja frecuencia donde 2007 es el año donde se rpesentó la mayor cantidad de casas construidas. Las casas con precios muy altos fueron consttruidas en general en 2007.\n")

real_state_df$YrSold=as.character(real_state_df$YrSold)
var=real_state_df$YrSold
barplot(table(var))
barplot(table(real_state_df[real_state_df$atypicalSalePrice==TRUE,'YrSold']))
cat("La variable YrSold es una variable categórica que presenta una distribución de frecuencia del año de venta de la vivienda. Es sospechoso que aunque tenemos fechas de construcción desde 1978, en las ventas solo se han registrado casas desde 2007. Del mismo modo las fechas de construcción se registran hasta el 2015 pero las de venta hasta el 2017. Las casas con precios muy altos fueron vendidas en general en 2017 y 2015.\n")

real_state_df$MonthSold=as.character(real_state_df$MonthSold)
var=real_state_df$MonthSold
barplot(table(var))
barplot(table(real_state_df[real_state_df$atypicalSalePrice==TRUE,'MonthSold']))
cat("La variable MonthSold es una variable categórica que presenta una distribución de frecuencia del mes de venta de la vivienda. Se observa que hay una distribución ciclica con periodos de auge de las ventas y periodos de baja frecuencia donde se rpesentó la mayor cantidad de casas vendidas. Las casas con precios muy altos fueron vendidas en general a mediados del año.\n")


pairs(real_state_df)
