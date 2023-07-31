rm(list=ls())

dir="~/Documents/Estaciones_rojas/4056/"
setwd(dir)

#Bibliotecas y Funciones auxiliares
#-----------------
library("tseries")
library("xts")
library('dplyr')
library('lubridate')
library(imputeTS)
library(forecast)
library(MASS)

#funci?n para simular 1 trayectoria
trayectoria_sim=function(ST,p0,pp,shape,rate){
  #previo al ajuste quitamos NA
  ##vacios=which(is.na(ST)==T)
  ##ST1=ST[-vacios]
  ST1=ST
  #continuamos con trayectoria
  m=length(ST1)
  sim=numeric(m)
  moneda=runif(m,0,1)
  for(i in 1:m){
    if(moneda[i]<p0){sim[i]=0}else{
      sim[i]=rgamma(1,shape=shape,rate=rate)}}
  return(sim)}

#funci?n que guarda en una lista los datos de precipitaci?n por mes
indicesXmes=function(ST_mes,ST_precip){
  lista=list(NULL)
  for(i in 1:12){
    mes_i=which(ST_mes==i)
    lista[[i]]=ST_precip[mes_i]}
  return(lista)}

#Lista con nombres de meses
Mes=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

#funci?n que elabora el ajuste de distribuci?n gamma a un vector de 
#datos de precipitaci?n x mes
Ajuste=function(m,ST){
  #previo al ajuste quitamos NA
  vacios=which(is.na(ST)==T)
  ST1=ST[-vacios]
  #ST1=ST
  #continuamos con ajuste
  x=min(ST1)
  if(x==0){
    y0=which(ST1==0)
    yp=ST1[-y0]
    p0=length(y0)/m
    pp=1-p0
    ajuste=fitdistr(yp,"gamma")
    forma=ajuste$estimate[1]
    tasa=ajuste$estimate[2]
    return(c(p0,pp,forma,tasa))}else{
      ajuste=fitdistr(ST1,"gamma")
      forma=ajuste$estimate[1]
      tasa=ajuste$estimate[2]
      return(c(0,0,forma,tasa))}
}

sim_gamma=function(mes,shape,rate,pp,p0){
  #funcion de probabilidad que genera una observacion cero o gamma
  x=runif(1,0,1)
  if(x<p0){a=0}else{a=rgamma(1,shape=shape,rate=rate)}
  return(a)}

find_leap = function(x){
  #funcion para encontrar dias de a?os bisiestos
  day(x) == 29 & month(x) == 2}

convertir_fecha=function(x){
  y=as.Date(paste(x,collapse="/"))
  return(y)}
#-----------------


#leemos bases de datos de daymet y de SMN
#__________________________________________________________
daymet4056=read.csv("est_29002.csv",header=T,sep=",")
daymet4056_precip=daymet4056$prcp..mm.day.
SMN4056=read.csv("29002.csv",header=T,sep=",")
colnames(SMN4056)=c("Fecha","Precipitacion")
SMN4056$Fecha=as.Date(SMN4056$Fecha,format=c("%d/%m/%Y"))
SMN4056$Mes=as.numeric(format(SMN4056$Fecha,"%m"))
SMN4056$Dia=as.numeric(format(SMN4056$Fecha,"%d"))

#Creamos la columna a?o
A=as.numeric(format(SMN4056$Fecha,"%Y"))
aaaa=which(A>26)
A1=A
for(i in 1:length(aaaa)){A1[aaaa[i]]=as.numeric(paste(c("19",A[aaaa[i]]),collapse=""))}
aaaa2=which(A<10)
for(i in 1:length(aaaa2)){A1[aaaa2[i]]=as.numeric(paste(c("200",A[aaaa2[i]]),collapse=""))}
aaaa3=which(A<22 & A>9)
for(i in 1:length(aaaa3)){A1[aaaa3[i]]=as.numeric(paste(c("20",A[aaaa3[i]]),collapse=""))}
unique(A1)
SMN4056$A=A1
SMN4056$Precipitacion=as.numeric(SMN4056$Precipitacion)
#__________________________________________________________


#_______Realizamos ajuste de distribuciones gamma y graficamos ajuste___________
#_______________________________________________________________________________

#Generamos vectores de datos x mes
PrecipXmes=indicesXmes(SMN4056$Mes,SMN4056$Precipitacion)
#Generamos ajuste de distribuci?n (gamma o alternativa)
#Gr?ficas que muestren el ajuste
#y trayectoria estimada
parametros=matrix(0,ncol=4,nrow=12)

for(i in 1:12){
  precip=PrecipXmes[[i]]
  n=length(precip)
  ajuste_i=Ajuste(n,precip)#vector de par?metros
  parametros[i,]=ajuste_i#matriz que va guardando par?metros
  p_ceros=ajuste_i[1]
  p_posit=ajuste_i[2]
  forma=ajuste_i[3]
  tasa=ajuste_i[4]
  trayect_estimada=trayectoria_sim(precip,p_ceros,p_posit,forma,tasa)
  y=cbind(trayect_estimada,precip)
  indices=which(is.na(y[,2])==T)
  y1=y[-indices,]
  r1=cor(sort(y1[,1]),sort(y1[,2]))
  r2=round(r1,4)
  #Gr?fica x mes
  x11()
  plot(density(sort(trayect_estimada)),col="blue2",main=paste(c(Mes[i],' corr=',r2),collapse=""),type="p",pch=20)
  points(density(sort(precip)),col="red")
  legend("topright",c("obs","est"),text.col=c("red","blue"),pch=c(1,20),col=c("red","blue2"))
}

colnames(parametros)=c("p0","pp","forma","tasa")
write.csv(parametros,file="parametros_29002.csv")
#______________________________Rellenar NA__________________________
#___________________________________________________________________

#Generamos fechas para comparar con daymet
start.date=as.Date("1980/01/01")
end.date=as.Date("2019/12/31")
time.index=seq(start.date, by=1, end.date)
time.index2=time.index[find_leap(time.index)==F]#quitamos 29-feb
time.index3=as.Date(NULL)
for(i in 1:dim(SMN4056)[1]){
  time.index3=c(time.index3,convertir_fecha(SMN4056[i,c(5,3,4)]))}

filas=dim(daymet4056)[1]

X=matrix(0,ncol=2,nrow=filas)
orden_guardado=NULL
for(i in 1:dim(SMN4056)[1]){
  orden_fecha_daymet=which(time.index2==time.index3[i])
  orden_guardado=c(orden_fecha_daymet,orden_guardado)
  X[orden_fecha_daymet,1]=SMN4056$Precipitacion[i]}
#rellenamos de NA donde no hay datos de la estacion 4056 del SMN
X[-orden_guardado,1]=rep(NA,filas-length(orden_guardado))
#segunda columna son datos de daymet
X[,2]=daymet4056$prcp..mm.day.
#construimos data.frame con las dos bases de datos
X1=data.frame("fecha"=time.index2,"SMN4056_NA"=X[,1],"daymet4056"=X[,2])
X1$D=as.numeric(format(X1[,1],'%d'))
X1$M=as.numeric(format(X1[,1],'%m'))
X1$A=as.numeric(format(X1[,1],'%Y'))

parametros1=data.frame(parametros)

NAs=which(is.na(X1[,2])==T)
nas=length(NAs)
for(i in 1:nas){
  for(j in 1:12){
    if(X1$M[NAs[i]]==j){
      a=parametros1$forma[j]
      b=parametros1$tasa[j]
      c=parametros1$pp[j]
      d=parametros1$p0[j]
      X1$SMN4056[NAs[i]]=sim_gamma(j,a,b,c,d)}}}


write.csv(X1,file="X1_29002.csv")

#______________Gráficas______________________
#_______________________________________________________________________________

NAs=which(is.na(X1$SMN4056_NA)==T)
#graficamos la posici?n de los NA para saber si est?n en "la orilla"
X11()
plot(NAs)

ST_SMN=ts(X1$SMN4056,start=c(1980,1),end=c(2019,365),freq=365)
ST_daymet=ts(X1$daymet4056,start=c(1980,1),end=c(2019,365),freq=365)
x11()
par(mfrow=c(2,1))
plot(ST_SMN)
plot(ST_daymet)

x11()
plot(sort(X1$SMN4056),sort(X1$daymet4056))
cor(sort(X1$SMN4056),sort(X1$daymet4056))

x11()
plot(decompose(ST_SMN))

#Gráfica la series de datos faltantes del SMN con el rellenado Gamma
ST_SMN_NA=ts(X1$SMN4056_NA,start=c(1980,1),end=c(2019,365),freq=365)
ST_SMN=ts(X1$SMN4056,start=c(1980,1),end=c(2019,365),freq=365)
x11()
par(mfrow=c(2,1))
plot(ST_SMN_NA)
plot(ST_SMN)
