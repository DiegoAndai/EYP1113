#Cargar Datos#
getwd()
setwd('/Users/diegoandai/Desktop/Tarea1Proba')
Base = read.table('datosTarea.txt', header = T, dec = '.')
head(Base)

#Reporte#

#1. Variables (preguntas 1 y 2)#

Skewness = function(x){
n = length(x)
sum((x-mean(x))^3/n)/sd(x)^3
}

Kurtosis = function(x){
n = length(x)
sum((x - mean(x))^4/n)/sd(x)^4-3
}

# Obtuve esto de http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode, era solo para tener la moda en un formato más ordenado y que funcionara con multi-modal
Moda = function(x) {
	ux = unique(x)
	tab = tabulate(match(x, ux))
	result = ux[tab == max(tab)]
	if (length(result) != length(x)){
		paste(c('[', paste(c(result), collapse = ', '), ']'), collapse = '') #esto es solo para mejor visualización
	} else {
		'None'
	}
}

#La siguiente función agrega a summary medidas descriptivas
SummaryX = function(x){
	super_summ = data.frame(Variable = deparse(substitute(x)))
	super_summ['  Min.'] = min(x)
	super_summ['  Max.'] = max(x)
	super_summ['  Median'] = median(x)
	super_summ['  Mean'] = round(mean(x), digits = 4)
	super_summ['  1st Qu.'] = quantile(x, na.rm=T, 0.25) 
	super_summ['  3rd Qu.'] = quantile(x, na.rm=T, 0.75) 
	super_summ['  Mean'] = round(mean(x), digits = 4)
	super_summ['  Mode'] = Moda(x)	
	super_summ['  Standard Dev.'] = round(sd(x), digits = 4)
	super_summ['  Variance'] = round(var(x), digits = 4)
	super_summ['  Skewness'] = round(Skewness(x), digits = 4)
	super_summ['  Kurtosis'] = round(Kurtosis(x), digits = 4)
	super_summ
}

#1.1 Temperatura Enero#

t_enero = Base$T_Enero
SummaryX(t_enero)

par(mfrow=c(1,2))
boxplot(t_enero, outline = FALSE, cex.axis = 0.5, ylim = c(10, 60), main = "Boxplot temperaturas Enero", col = 'gray', ylab = 'Temperatura [Fahrenheit]')
hist(t_enero, freq = F, ylim = c(0, 0.06), col = "gray", border = "white", main = "Histograma temperaturas Enero", xlab ='Temperatura [Fahrenheit]', ylab = 'Densidad', breaks = 10)
lines(density(t_enero, na.rm=T), col = 'red', lwd = 2)

dev.print(pdf, 'histte.pdf')
dev.print(pdf, 'boxte.pdf')

#1.2 Temperatura Julio#

t_julio = Base$T_Julio
SummaryX(t_julio)

par(mfrow=c(1,2))
boxplot(t_julio, outline = FALSE, cex.axis = 0.5, ylim = c(65, 85), main = "Boxplot temperaturas Julio", col = 'gray', ylab = 'Temperatura [Fahrenheit]')
hist(t_julio, freq = F, ylim = c(0, 0.1), col = "gray", border = "white", main = "Histograma temperaturas Julio", xlab ='Temperatura [Fahrenheit]', ylab = 'Densidad', breaks = 10)
lines(density(t_julio, na.rm=T), col = 'red', lwd = 2)

dev.print(pdf, 'histtj.pdf')
dev.print(pdf, 'boxtj.pdf')

#1.3 Humedad#

humedad = Base$Humedad
SummaryX(humedad)

par(mfrow=c(1,2))
boxplot(humedad, outline = FALSE, cex.axis = 0.5, main = "Boxplot humedad", ylab = 'Humedad [Porcentaje]', col = 'gray')
hist(humedad, freq = F, ylim = c(0, 0.13), col = "gray", border = "white", main = "Histograma humedad", xlab = 'Humedad [Porcentaje]', ylab = 'Densidad', breaks = 20)
lines(density(humedad, na.rm=T), col = 'red', lwd=2)

dev.print(pdf, 'histhu.pdf')
dev.print(pdf, 'boxhu.pdf')

#1.4 Cantidad de lluvia#

cant_lluvia = Base$Cant_Lluvia
SummaryX(cant_lluvia)

par(mfrow=c(1,2))
boxplot(cant_lluvia, outline = FALSE, cex.axis = 0.5, ylim = c(17, 56), main = "Boxplot cantidad de lluvias", ylab = 'Lluvia caida [Pulgadas]', col = 'gray')
hist(cant_lluvia, freq = F, ylim = c(0, 0.05), col = "gray", border = "white", main = "Histograma cantidad de lluvias", breaks = 10, xlab = 'Lluvi caida [Pulgadas]', ylab = 'Densidad')
lines(density(cant_lluvia, na.rm=T), col = 'red', lwd = 2)

dev.print(pdf, 'histll.pdf')
dev.print(pdf, 'boxll.pdf')

#1.5 Mortalidad# 

mortalidad = Base$Mortalidad
SummaryX(mortalidad)
mortalidad

par(mfrow=c(1,2))
boxplot(mortalidad, outline = FALSE, cex.axis = 0.5, main = "Boxplot mortalidad", col = 'gray', ylab = 'Mortalidad')
hist(mortalidad, freq = F, col = "gray", border = "white", main = "Histograma mortalidad", breaks = 20, xlab = 'Mortalidad', ylab = 'Densidad')
lines(density(mortalidad, na.rm=T), col = 'red', lwd = 2)

dev.print(pdf, 'histmo.pdf')
dev.print(pdf, 'boxmo.pdf')

#1.5 Población total# 

pobl_total = Base$Pobl_total
pobl_total
SummaryX(pobl_total)

par(mfrow=c(1,2))
boxplot(pobl_total, outline = FALSE, cex.axis = 0.5, ylim = c(124833, 3300000), main = "Boxplot poblacion total", col = 'gray')
hist(pobl_total, ylim = c(0, 0.0000009), freq = F, col = "gray", border = "white", main = "Histograma de poblacion total", xlab = 'Cantidad de poblacion', ylab = 'Densidad', cex.main = 1, breaks = 20)
lines(density(pobl_total, na.rm=T), col = 'red', lwd = 2)

dev.print(pdf, 'histpt.pdf')
dev.print(pdf, 'boxpt.pdf')

#1.6 Población por hogar#

pobl_hogar = Base$Pobl_hogar
SummaryX(pobl_hogar)

par(mfrow=c(1,2))
boxplot(pobl_hogar, outline = FALSE, cex.axis = 0.5, ylim = c(2.97, 3.55), main = "Boxplot poblacion por hogar", col = 'gray', ylab= 'Promedio de poblacion por hogar')
hist(pobl_hogar, freq = F, ylim = c(0, 3), col = "gray", border = "white", main = "Histograma poblacion por hogar", xlab = 'Promedio de poblacion por hogar', ylab = 'Densidad')
lines(density(pobl_hogar, na.rm=T), col = 'red', lwd = 2)

dev.print(pdf, 'histph.pdf')
dev.print(pdf, 'boxph.pdf')

#1.7 Ingresos Medios#

ingresos_medios = Base$Ingresos_medios
SummaryX(ingresos_medios)

par(mfrow=c(1,2))
boxplot(ingresos_medios, outline = FALSE, cex.axis = 0.5, ylim = c(25000, 43000), main = "Boxplot ingresos medios", col = 'gray', ylab = 'Ingresos medios [Miles de USD]')
hist(ingresos_medios, freq = F, ylim = c(0, 0.0001), col = "gray", border = "white", main = "Histograma de ingresos medios", breaks = 10, xlab = 'Ingresos medios [Miles de USD]', ylab = 'Densidad')
lines(density(ingresos_medios, na.rm=T), col = 'red', lwd = 2)

dev.print(pdf, 'histim.pdf')
dev.print(pdf, 'boxim.pdf')

#1.8 HC#

hc = Base$HC
Base[order(Base[,11]),]
SummaryX(hc)

par(mfrow=c(1,2))
boxplot(hc, outline = FALSE, cex.axis = 0.5, ylim = c(0, 70), col = 'lightgray', main = "Boxplot HC", ylab = 'HC en el aire [ppm]')
hist(hc, freq = F, ylim = c(0, 0.03), col = "gray", border = "white", main = "Histograma HC", xlab = 'HC en el aire [ppm]', ylab = 'Densidad', breaks = 50)
lines(density(hc, na.rm=T), col = 'red', lwd = 2)

dev.print(pdf, 'histhc.pdf')
dev.print(pdf, 'boxhc.pdf')

#para estudiar la zona densa de hc
#hc.lt100 = Base[Base$HC < 100, ]
#hist(hc.lt100$HC, freq = F)
#lines(density(hc.lt100$HC, na.rm = T))


#1.9 Nox#

nox = Base$Nox
SummaryX(nox)

par(mfrow=c(1,2))
boxplot(nox, outline = FALSE, cex.axis = 0.5, ylim = c(0, 42), col = 'lightgray', main = "Boxplot NOx", ylab = 'NOx en el aire [ppm]')
hist(nox, freq = F, ylim = c(0, 0.04), col = "gray", border = "white", main = "Histograma NOx", xlab = 'NOx en el aire [ppm]', ylab = 'Densidad', breaks = 30)
lines(density(nox, na.rm=T), col = 'red', lwd = 2)

dev.print(pdf, 'histnox.pdf')
dev.print(pdf, 'boxnox.pdf')

#1.9 S02#

s02 = Base$S02
SummaryX(s02)

par(mfrow=c(1,2))
boxplot(s02, outline = FALSE, cex.axis = 0.5, ylim = c(0, 150), col = 'lightgray', main = "Boxplot SO2", ylab = 'SO2 en el aire [ppm]')
hist(s02, freq = F, ylim = c(0, 0.02), col = "gray", border = "white", main = "Histograma SO2", xlab = 'SO2 en el aire [ppm]', ylab = 'Densidad', breaks = 10)
lines(density(s02, na.rm=T), col = 'red', lwd = 2)

dev.print(pdf, 'histso2.pdf')
dev.print(pdf, 'boxso2.pdf')

#2. Relación mortalidad y gases (pregunta 3)#

par(mfrow=c(1,3))

plot(s02, mortalidad)
abline(lm(mortalidad~s02), col = 'blue')
lines(lowess(s02, mortalidad), col = 'red')

#2.1. Nox#

par(mfrow=c(1,2), oma=c(0,0,0,0))

plot(nox, mortalidad, pch = 20, xlab = 'Cantidad de NOx [ppm]', ylab = 'Mortalidad')
abline(lm(mortalidad~nox), col = 'blue', lwd = 2)
lines(lowess(nox, mortalidad), col = 'red', lwd =2)

title('Relacion entre mortalidad y presencia de gases NOx')

Base.dense_nox = subset(Base, nox < 40)
nox.dense_nox = Base.dense_nox$Nox
mortalidad.dense_nox = Base.dense_nox$Mortalidad

plot(nox.dense_nox, mortalidad.dense_nox, pch = 20, xlab = 'Cantidad de NOx [ppm]', ylab = 'Mortalidad')
abline(lm(mortalidad.dense_nox~nox.dense_nox), col = 'blue', lwd = 2)
lines(lowess(nox.dense_nox, mortalidad.dense_nox), col = 'red', lwd = 2)

lm(mortalidad.dense_nox~nox.dense_nox)

dev.print(pdf, 'mortnox.pdf')




#2.2. s02#

par(mfrow=c(1,2))

plot(s02, mortalidad,pch=20, xlab = 'Cantidad de SO2 [ppm]', ylab = 'Mortalidad')
abline(lm(mortalidad~s02), col = 'blue', lwd=2)
lines(lowess(s02, mortalidad), col = 'red', lwd=2)
title('Relacion entre mortalidad y presencia de gases SO2')

Base.dense_s02 = subset(Base, s02 < 150)
s02.dense_s02 = Base.dense_s02$S02
mortalidad.dense_s02 = Base.dense_s02$Mortalidad

plot(s02.dense_s02, mortalidad.dense_s02,pch=20, xlab = 'Cantidad de SO2 [ppm]', ylab = 'Mortalidad')
abline(lm(mortalidad.dense_s02~s02.dense_s02), col = 'blue', lwd=2)
lines(lowess(s02.dense_s02, mortalidad.dense_s02), col = 'red', lwd=2)

lm(mortalidad.dense_s02~s02.dense_s02)
lm(mortalidad~s02)


dev.print(pdf, 'mortso2.pdf')

#2.3. HC#

par(mfrow=c(1,2))

plot(hc, mortalidad,pch=20, xlab = 'Cantidad de HC [ppm]', ylab = 'Mortalidad')
abline(lm(mortalidad~hc), col = 'blue', lwd = 2)
lines(lowess(hc, mortalidad), col = 'red', lwd = 2)

Base.dense_hc = subset(Base, hc < 70)
hc.dense_hc = Base.dense_hc$HC
mortalidad.dense_hc = Base.dense_hc$Mortalidad

title('Relacion entre mortalidad y presencia de gases HC')

plot(hc.dense_hc, mortalidad.dense_hc, pch= 20, xlab = 'Cantidad de HC [ppm]', ylab = 'Mortalidad')
abline(lm(mortalidad.dense_hc~hc.dense_hc), col = 'blue', lwd =2)
lines(lowess(hc.dense_hc, mortalidad.dense_hc), col = 'red', lwd =2)

lm(mortalidad.dense_hc~hc.dense_hc)

dev.print(pdf, 'morthc.pdf')

#3. polución en NY y OH (pregunta 4) #

oh = Base[Base$Estado == 'OH', ]
ny = Base[Base$Estado == 'NY', ]

#3.1 so2
par(mfrow=c(1,2))


boxplot(oh$S02, outline = F, ylim=c(0, 70), main = 'Contaminacion SO2 En Ohio', ylab = 'SO2 [ppm]', col = 'gray')
boxplot(ny$S02, outline = F, ylim=c(0, 70), main = 'Contaminacion SO2 En NY', ylab = 'SO2 [ppm]', col = 'gray')

dev.print(pdf, 'ohnyso2.pdf')


#3.2 nox

par(mfrow=c(1,2))


boxplot(oh$Nox, outline = F, ylim=c(0, 23), main = 'Contaminacion NOx En Ohio', ylab = 'NOx [ppm]', col = 'gray')
boxplot(ny$Nox, outline = F, ylim=c(0, 23), main = 'Contaminacion NOx En NY', ylab = 'NOx [ppm]', col = 'gray')

dev.print(pdf, 'ohnynox.pdf')

#3.3 hc

par(mfrow=c(1,2))


boxplot(oh$HC, outline = F, ylim=c(0, 32), main = 'Contaminacion HC En Ohio', ylab = 'HC [ppm]', col = 'gray')
boxplot(ny$HC, outline = F, ylim=c(0, 32), main = 'Contaminacion HC En NY', ylab = 'HC [ppm]', col = 'gray')

dev.print(pdf, 'ohnyhc.pdf')

#4. población por hogar (pregunta 5)#

y = sort(pobl_hogar)
N = length(pobl_hogar)
p = 1:N/(N+1)

par(mfrow=c(1,3))

#4.1. Normal#

x = qnorm(p)
plot(y ~ x, pch = 20, ylim = c(2,4), xlim = c(-2,2), main = 'Distribucion Normal')
recta = lm(y ~ x)$coef
abline(recta, col = "red", lwd = 2)
mu = recta[1]
sigma = recta[2]

#4.2 Log-normal#

x = qnorm(p)
plot(log(y) ~ x, pch = 20, ylim = c(0.9,1.5), xlim = c(-2,3))
recta = lm(log(y) ~ x)$coef
abline(recta, col = "red", lwd = 2)
lambda = recta[1]
zeta = recta[2]

#4.3. Weibull#

x = log(-log(1-p))
plot(y ~ x, pch = 20, ylim = c(3,3.6), xlim = c(-3,3))
recta = lm(y ~ x)$coef
abline(recta, col = "red", lwd = 2)
nu = exp(recta[1])
beta = 1/recta[2]
recta


#4.4. Logistico#

x = log(p/(1-p))
plot(y ~ x, pch = 20, ylim = c(2.5,3.7), xlim = c(-5,5), main = 'Distribucion Logistica')
recta = lm(y ~ x)$coef
abline(recta, col = "red", lwd = 2)
mu1 = recta[1]
sigma1 = recta[2]

#4.5. Log-logistico#

x = log(p/(1-p))
plot(log(y) ~ x, pch = 20, ylim = c(1.05,1.3), xlim = c(-3,5))
recta = lm(log(y) ~ x)$coef
abline(recta, col = "red", lwd = 2)
mu2 = recta[1]
sigma2 = recta[2]

#4.6 Exponencial trasladada#

x = -log(1-p)
plot(y ~ x, pch = 20, ylim = c(2.5, 4), xlim = c(0,4.5), main = 'Distribucion Exponencial trasladada')
recta = lm(y ~ x)$coef
abline(recta, col = "red", lwd = 2)
alpha = recta[1]
lambda = 1/recta[2]

dev.print(pdf, 'modelchoose.pdf')





hist(pobl_hogar, freq = F, xlim= c(2.5,3.6), ylim = c(0,7), main = 'Histograma densidad poblacion hogar y modelos', ylab = 'Densidad', xlab = 'Promedio de poblacion por hogar')
x = seq(0,60,0.01)
lines(density(pobl_hogar),col = "magenta", lwd = 2)
lines(dexp(x-alpha, rate = lambda)~x, lwd = 2, col = "green")
lines(dnorm(x, mean = mu, sd = sigma)~x, lwd = 2, col = "blue")
lines(dlogis(x, location = mu1, scale = sigma1)~x, lwd = 2, col = "red")
grid()
dev.print(pdf, 'modelfit.pdf')




