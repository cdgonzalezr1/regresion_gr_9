
#Instalando paquetes
install.packages("readr")
install.packages("corrplot")
install.packages("leaps")
install.packages("lmtest")
install.packages("faraway")
install.packages("MASS")


#Cargando paquetes
library(readr)
library(corrplot)
library(leaps)
library(lmtest)
library(faraway)
library(MASS)


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
cat("La variable YearBuilt es una variable categórica que presenta una distribución de frecuencia del año de construcción de la vivienda. Se observa que hay una distribución ciclica con periodos de auge de las construcciones y periodos de baja frecuencia donde 2007 es el año donde se rpesentó la mayor cantidad de casas construidas. Las casas con precios muy altos fueron construidas en general en 2007.\n")

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
cat("La variable HallwayType es una variable categórica que representa el tipo de vestibulo de la vivienda. Lo más frecuente es el tipo (terraced) y las viviendas de mayor precio se encuentran en esta categoría.\n")

real_state_df$HeatingType=as.character(real_state_df$HeatingType)
var=real_state_df$HeatingType
barplot(table(var))
barplot(table(real_state_df[real_state_df$atypicalSalePrice=='atip','HeatingType']))
cat("La variable HeatingType es una variable categórica que representa el tipo de calefacción de la vivienda. Lo más frecuente es el tipo (central) y las viviendas de mayor precio se encuentran en la categoría individual.\n")


real_state_df$AptManageType=as.character(real_state_df$AptManageType)
var=real_state_df$AptManageType
barplot(table(var))
barplot(table(real_state_df[real_state_df$atypicalSalePrice=='atip','AptManageType']))
cat("La variable AptManageType es una variable categórica que representa el tipo de administración de la vivienda. Lo más frecuente es el tipo (full service) y las viviendas de mayor precio se encuentran en esta categoría.\n")

#Generando nuevas variables independientes
real_state_df$Size_sqf_sqrt=sqrt(real_state_df$Size.sqf)
real_state_df$Size_sqf_ln=log(real_state_df$Size.sqf)
real_state_df$Size_sqf_pot_2=real_state_df$Size.sqf^2
real_state_df$Size_sqf_pot_3=real_state_df$Size.sqf^3
real_state_df$Size_sqf_div=1/real_state_df$Size.sqf

real_state_df$Floor_sqrt=sqrt(real_state_df$Floor)
real_state_df$Floor_ln=log(real_state_df$Floor)
real_state_df$Floor_pot_2=real_state_df$Floor^2
real_state_df$Floor_pot_3=real_state_df$Floor^3
real_state_df$Floor_div=1/real_state_df$Floor

real_state_df$interaction_Size_Floor=real_state_df$Size.sqf*real_state_df$Floor

#Selección de variables
corrplot(as.matrix(cor(real_state_df[,c('Size.sqf.','Floor','Size_sqf_sqrt','Size_sqf_ln','Size_sqf_pot_2','Size_sqf_pot_3','Size_sqf_div','Floor_sqrt','Floor_ln','Floor_pot_2','Floor_pot_3','Floor_div','interaction_Size_Floor')])))

#Estructurando y ajustando modelo
modelo=lm(SalePrice~.,data=real_state_df)
summary(modelo)
anova(modelo)
confint(modelo)
AIC(modelo)
BIC(modelo)
summary(modelo)$adj.r.squared

step.model <- stepAIC(modelo, direction = "backward", trace = TRUE)
summary(step.model)


models <- regsubsets(SalePrice~., data = real_state_df, nvmax = 13, method = "forward")
sumreg<-summary(models)

## determinar el máximo
which.min(sumreg$bic)
plot(sumreg$bic,type = "l",ylab = "BIC")
points(4,sumreg$bic[4],pch=19,col='red')

# Observar las variables seleccionadas
coef(models,4)

#Estimando e interpretando parámetros


#Validando supuestos del modelo

#Media 0
residuos<-modelo$residuals

plot(y=residuos,x=datos$habitantes)

#Varianza constante
residuos<-modelo$residuals
proyectados<-modelo$fitted.values

plot(y=residuos,x=proyectados)
bptest(modelo)

#Normalidad
residuos<-modelo$residuals

par(mfrow=c(1,2))
qqnorm(residuos)
qqline(residuos)
hist(residuos)
residuos<-modelo$residuals

shapiro.test(residuos)

#Independencia
dwtest(modelo)


#No multicolinealidad
vif(modelo)

#Bondad de ajuste del modelo