# Variables de entrada:

# ‘# - índice que indica la posición de la observación.

# 1- SalePrice: precio de venta en dólares.

# 2- YearBuilt: año de construcción

# 3- YrSold: año de venta

# 4- MonthSold: mes de venta

# 5- Size.sqf: tamaño del apartamento en pies cuadrados

# 6- Floor: piso en el que se ubica la propiedad

# 7- HallwayType: tipo de vestíbulo

# 8- HeatingType: tipo de calefacción.

# 9- AptManageType: forma en la que se administraba la propiedad

# 10- N_Parkinglot.Ground: número de estacionamientos en el suelo.

# 11- N_Parkinglot.Basement: número de estacionamientos en el sótano.

# 12- TimeToBusStop: tiempo hasta una parada de autobús

# 13- TimeToSubway: tiempo hasta una estación del subterráneo

# 14- N_APT: número de apartamentos en el complejo.

# 15- N_manager: número de personas que atienden las instalaciones del apartamento.

# 16- N_elevators: número de ascensores.

# 17- SubwayStation: nombre de la estación más cercana.

# 18- N_FacilitiesNearBy.PublicOffice: número de oficinas públicas cercanas.

# 19- N_FacilitiesNearBy.Hospital: número de hospitales cercanos.

# 20- N_FacilitiesNearBy.Dpartmentstore: número de tiendas departamentales cercanas

# 21- N_FacilitiesNearBy.Mall: número de centros comerciales cercanos.

# 22- N_FacilitiesNearBy.ETC. número de otras facilidades cercanas (p.ej. hoteles)

# 23- N_FacilitiesNearBy.Park. número de parques cercanos.

# 24- N_SchoolNearBy.Elementary: número de escuelas primarias cercanas.

# 25- N_SchoolNearBy.Middle: número de escuelas medias cercanas.

# 26- N_SchoolNearBy.High. número de escuelas secundarias cercanas.

# 27- N_SchoolNearBy.University. número de universidades cercanas.

# 28- N_FacilitiesInApt: número de instalaciones del apartamento (p.ej. piscina, gimnasio)

# 29- N_FacilitiesNearBy.Total: número total de instalaciones cercanas.

# 30- N_SchoolNearBy.Total: número total de escuelas cercanas.


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
real_state_df[real_state_df$SalePrice>=min(box$out),'atypicalSalePrice']='atip'
real_state_df[real_state_df$SalePrice<min(box$out),'atypicalSalePrice']='no_atip'


real_state_df$YearBuilt=as.character(real_state_df$YearBuilt)
var=real_state_df$YearBuilt
barplot(table(var))
barplot(table(real_state_df[real_state_df$atypicalSalePrice=='atip','YearBuilt']))
cat("La variable YearBuilt es una variable categórica que presenta una distribución de frecuencia del año de construcción de la vivienda. Se observa que hay una distribución ciclica con periodos de auge de las construcciones y periodos de baja frecuencia donde 2007 es el año donde se rpesentó la mayor cantidad de casas construidas. Las casas con precios muy altos fueron consttruidas en general en 2007.\n")

real_state_df$YrSold=as.character(real_state_df$YrSold)
var=real_state_df$YrSold
barplot(table(var))
barplot(table(real_state_df[real_state_df$atypicalSalePrice=='atip','YrSold']))
cat("La variable YrSold es una variable categórica que presenta una distribución de frecuencia del año de venta de la vivienda. Es sospechoso que aunque tenemos fechas de construcción desde 1978, en las ventas solo se han registrado casas desde 2007. Del mismo modo las fechas de construcción se registran hasta el 2015 pero las de venta hasta el 2017. Las casas con precios muy altos fueron vendidas en general en 2017 y 2015.\n")

real_state_df$MonthSold=as.character(real_state_df$MonthSold)
var=real_state_df$MonthSold
barplot(table(var))
barplot(table(real_state_df[real_state_df$atypicalSalePrice=='atip','MonthSold']))
cat("La variable MonthSold es una variable categórica que presenta una distribución de frecuencia del mes de venta de la vivienda. Se observa que hay una distribución ciclica con periodos de auge de las ventas y periodos de baja frecuencia donde se rpesentó la mayor cantidad de casas vendidas. Las casas con precios muy altos fueron vendidas en general a mediados del año.\n")

var=real_state_df$Size.sqf
summary(var)
plot(density(var))
box=boxplot(var)
box$stats
box$conf
box$out
real_state_df[real_state_df$Size.sqf>=min(box$out),'atypicalSize.sqf']='atip'
real_state_df[real_state_df$Size.sqf<min(box$out),'atypicalSize.sqf']='no_atip'
plot(real_state_df$Size.sqf,real_state_df$SalePrice,col=as.factor(real_state_df$atypicalSize.sqf),pch=20)
plot(real_state_df$Size.sqf,real_state_df$SalePrice,col=as.factor(real_state_df$atypicalSalePrice),pch=20)
cor(var,real_state_df$SalePrice)
cat("La variable Size.sqf es una variable numérica que representa el tamaño de la vivienda en pies cuadrados. Los apartamentos de mayor tamaño corresponden con los de mayor precio de venta. Parece que las casas con mayor tamaño tienen una caida de precios, aunque la correlación es superior al 70% esta variable pude estarse relacionandod e forma no lineal o tener un sub grupo de casas cuyo precio de venta no crece tan aceleradamente con el tamaño de la vivienda.\n")


var=real_state_df$Floor
summary(var)
plot(density(var))
box=boxplot(var)
box$stats
box$conf
box$out
real_state_df[real_state_df$Floor>=min(box$out),'atypicalFloor']='atip'
real_state_df[real_state_df$Floor<min(box$out),'atypicalFloor']='no_atip'
plot(real_state_df$Floor,real_state_df$SalePrice,col=as.factor(real_state_df$atypicalFloor),pch=20)
plot(real_state_df$Floor,real_state_df$SalePrice,col=as.factor(real_state_df$atypicalSalePrice),pch=20)
cor(var,real_state_df$SalePrice)
cat("La variable Floor es una variable categórica que representa el número de piso en el que se encuentra la vivienda. Son pocas las viviendas que se encuentran en pisos del 30 en adelante pero no consideramos que sean atipicos a primera vista, Pero hay un comportamiento aprticcular en el que los precios de venta pueden tener cualquier valor cuando se encuentra en el piso 20 hacia abajo y que la correlación solo existe cuando hablamos de viviendas ubicadas en el piso 21 en adelante. La correlación es de 34% pero si separamos estos dos sub grupos tendremos que las ubicadas en pisos inferiores al 20 no se relacionean con el precio mientras que las ubicadas en pusos sueriores si tienen una correlacion fuerte.\n")

real_state_df[real_state_df$Floor>=20,'pisosElevados']='pisosElevados'
real_state_df[real_state_df$Floor<20,'pisosElevados']='pisosBajos'


real_state_df$HallwayType=as.character(real_state_df$HallwayType)
var=real_state_df$HallwayType
barplot(table(var))
barplot(table(real_state_df[real_state_df$atypicalSalePrice=='atip','HallwayType']))
cat("La variable HallwayType es una variable categórica que representa el tipo de pasillo de la vivienda. Se observa que hay una distribución ciclica con periodos de auge de los pasillos y periodos de baja frecuencia donde se rpesentó la mayor cantidad de casas con pasillos de madera. Las casas con precios muy altos fueron consttruidas en general en madera.\n")

#Generando nuevas variables independientes


#Selección de variables


#Estructurando y ajustando modelo


#Estimando e interpretando parámetros


#Validando supuestos del modelo


#Bondad de ajuste del modelo