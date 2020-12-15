# TAREA-2-FOR-LOOP
# Antes de correr, se corrigieron nombres de columnas para la fusión de datas
# En la siguiente línea, cambiar directorio para ejectar el script en r studio
setwd("C:/Users/Alumno/Documents/TAREA2_FOR_LOOP/TAREA-2-FOR-LOOP") #directorio de trabajo

# respuesta N°1. Cargar CSV y agregar columna "tamanio"

Chi.1<-read.csv ("grandes_chile.csv", sep = ";", row.names = 1)
Chi.1$tamanio = "grande"
Chi.2<-read.csv ("medianas_chile.csv", sep = ";", row.names = 1)
Chi.2$tamanio = "mediana"
Chi.3<-read.csv ("pequena_chile.csv", sep = ";", row.names = 1)
Chi.3$tamanio = "pequena"
Chi.4<-read.csv ("micro_chile.csv", sep = ";", row.names = 1)
Chi.4$tamanio = "micro"
Col.1<-read.csv ("grandes_colombia.csv", sep = ";", row.names = 1)
Col.1$tamanio = "grande"
Col.2<-read.csv ("medianas_colombia.csv", sep = ";", row.names = 1)
Col.2$tamanio = "mediana"
Col.3<-read.csv ("pequena_colombia.csv", sep = ";", row.names = 1)
Col.3$tamanio = "pequena"
Col.4<-read.csv ("micro_colombia.csv", sep = ";", row.names = 1)
Col.4$tamanio = "micro"
Per.1<-read.csv ("grandes_peru.csv", sep = ";", row.names = 1)
Per.1$tamanio = "grande"
Per.2<-read.csv ("medianas_peru.csv", sep = ";", row.names = 1)
Per.2$tamanio = "mediana"
Per.3<-read.csv ("pequena_peru.csv", sep = ";", row.names = 1)
Per.3$tamanio = "pequena"
Per.4<-read.csv ("micro_peru.csv", sep = ";", row.names = 1)
Per.4$tamanio = "micro"


# respuesta N°2. Fusionar los CSV y definir tipos de datos

datosCSV<-rbind(Chi.1, Chi.2, Chi.3, Chi.4, Col.1, Col.2, Col.3, Col.4, Per.1, Per.2, Per.3, Per.4)
respuesta2<-matrix(1:24,12,2)
for (x in 1: ncol(datosCSV)){
  respuesta2[x,1]<-colnames(datosCSV[x])
  respuesta2[x,2]<-typeof(datosCSV[1,x])
}
print(respuesta2)


# respuesta N°3. observaciones Peru versus Chile

obsPeru<-0    # contador de observaciones para Perú
obsChile<-0   # contador de obsevaciones para Chile
ultimo<-nrow(datosCSV)

for (x in 1:ultimo){
  if (datosCSV[x,1]=="peru"){
    obsPeru<-obsPeru+1
  }else if (datosCSV[x,1]=="chile"){
    obsChile<-obsChile+1
  }
}
diferencia<- obsPeru -obsChile
if (diferencia==0){
  respuesta3<-"Peru tiene igual numero de observaciones que Chile"
}else if (diferencia>0){
  respuesta3<-"Peru tiene mas observaciones que Chile"
}else if (diferencia<0){
  respuesta3<-"Peru tiene menos observaciones que Chile"
}
print(respuesta3)


# respuesta N°4. Pais con mas ingresos de explotación en cada año

ingre_Chile<-0        #contador de ingresos para Chile
ingre_Colombia<-0     #contador de ingresos para Colombia
ingre_Peru<-0         #contador de ingresos para Peru
ultimo<-nrow(datosCSV)

for (x in 1:ultimo){
  # rutina para convertir tasa chr a número
  ingresoriginal<-datosCSV[x,2]
  n_datos<-nchar(datosCSV[x,2])
  busca_coma<-0
  for (pos_coma in 1:n_datos){
    if (substr(ingresoriginal,pos_coma,pos_coma)==","){
      busca_coma<-pos_coma
      y<-n_datos
    }
  }
  ingresonumero<-0
  for (entero in 1:n_datos){
    if (entero<busca_coma){
      potencia<-busca_coma - entero -1
    }else{
      potencia<-busca_coma - entero
    }
    if (substr(ingresoriginal,entero,entero)=="0"){
      valor<-0
    }else if (substr(ingresoriginal,entero,entero)=="1"){
      valor<-1
    }else if (substr(ingresoriginal,entero,entero)=="2"){
      valor<-2
    }else if (substr(ingresoriginal,entero,entero)=="3"){
      valor<-3
    }else if (substr(ingresoriginal,entero,entero)=="4"){
      valor<-4
    }else if (substr(ingresoriginal,entero,entero)=="5"){
      valor<-5
    }else if (substr(ingresoriginal,entero,entero)=="6"){
      valor<-6
    }else if (substr(ingresoriginal,entero,entero)=="7"){
      valor<-7
    }else if (substr(ingresoriginal,entero,entero)=="8"){
      valor<-8
    }else if (substr(ingresoriginal,entero,entero)=="9"){
      valor<-9
    }
    ingresonumero<-ingresonumero + valor* (10^potencia)
    if (entero+1==busca_coma){
      entero<-busca_coma
    }
  }
  
  # subrutina para sumar el ingreso al pais
  if (datosCSV[x,1]=="chile"){
    ingre_Chile<-ingre_Chile +ingresonumero
  }else if (datosCSV[x,1]=="colombia"){
    ingre_Colombia<-ingre_Colombia +ingresonumero
  }else if (datosCSV[x,1]=="peru"){
    ingre_Peru<-ingre_Peru +ingresonumero
  }
}
if (ingre_Peru>ingre_Chile & ingre_Peru>ingre_Colombia){
  respuesta4<-"Perú"
}
if (ingre_Chile>ingre_Colombia & ingre_Chile>ingre_Peru){
  respuesta4<-"Chile"
}
if (ingre_Colombia>ingre_Chile & ingre_Colombia>ingre_Peru){
  respuesta4<-"Colombia"
}
respuesta4<-paste("el pais con mas ingresos es",respuesta4,sep = " ")
print(respuesta4)


# respuesta N°5. Nueva variable, donde...
#   caso Chile,     tasa interes x 0,1
#   caso Colombia,  tasa interes / 2
#   caso Peru,      tasa interes + 0,3

datosCSV$nuevatasa=0    # el nombre de la variable es nuevatasa
ultimo<-nrow(datosCSV)
for (x in 1:ultimo){    # rutina para convertir tasa chr a número
  tasaoriginal<-datosCSV[x,11]
  n_datos<-nchar(datosCSV[x,11])
  busca_coma<-0
  for (pos_coma in 1:n_datos){
    if (substr(tasaoriginal,pos_coma,pos_coma)==","){
      busca_coma<-pos_coma
      y<-n_datos
    }
  }
  tasanumerica<-0
  for (entero in 1:n_datos){
    if (entero<busca_coma){
      potencia<-busca_coma - entero -1
    }else{
      potencia<-busca_coma - entero
    }
    if (substr(tasaoriginal,entero,entero)=="0"){
      valor<-0
    }else if (substr(tasaoriginal,entero,entero)=="1"){
      valor<-1
    }else if (substr(tasaoriginal,entero,entero)=="2"){
      valor<-2
    }else if (substr(tasaoriginal,entero,entero)=="3"){
      valor<-3
    }else if (substr(tasaoriginal,entero,entero)=="4"){
      valor<-4
    }else if (substr(tasaoriginal,entero,entero)=="5"){
      valor<-5
    }else if (substr(tasaoriginal,entero,entero)=="6"){
      valor<-6
    }else if (substr(tasaoriginal,entero,entero)=="7"){
      valor<-7
    }else if (substr(tasaoriginal,entero,entero)=="8"){
      valor<-8
    }else if (substr(tasaoriginal,entero,entero)=="9"){
      valor<-9
    }
    tasanumerica<-tasanumerica + valor* (10^potencia)
    if (entero+1==busca_coma){
      entero<-busca_coma
    }
  }
  
  if (datosCSV[x,1]=="chile"){    #tratamiento de la tasa según problema planteado
    datosCSV[x,13]<- tasanumerica*0.1
  }else if (datosCSV[x,1]=="colombia"){
    datosCSV[x,13]<- tasanumerica/2  
  }else if (datosCSV[x,1]=="peru"){
    datosCSV[x,13]<- tasanumerica+0.3
  }
}
print(datosCSV$nuevatasa)


# respuesta N°6. Reemplazar exportaciones redondeadas al primer decimal, donde...
#   si es mayor a 2,1 reemplazar por valor 1
#   si es menor a 2,1 reemplazar por valor 2
#   si es igual a 2,1 reemplazar por valor 3

expo_num<-numeric()
ultimo<-nrow(datosCSV)
for (x in 1:ultimo){    # rutina para convertir tasa chr a número
  exporiginal<-datosCSV[x,5]
  n_datos<-nchar(datosCSV[x,5])
  busca_coma<-0
  for (pos_coma in 1:n_datos){
    if (substr(exporiginal,pos_coma,pos_coma)==","){
      busca_coma<-pos_coma
      y<-n_datos
    }
  }
  exponumerica<-0
  for (entero in 1:n_datos){
    if (entero<busca_coma){
      potencia<-busca_coma - entero -1
    }else{
      potencia<-busca_coma - entero
    }
    if (substr(exporiginal,entero,entero)=="0"){
      valor<-0
    }else if (substr(exporiginal,entero,entero)=="1"){
      valor<-1
    }else if (substr(exporiginal,entero,entero)=="2"){
      valor<-2
    }else if (substr(exporiginal,entero,entero)=="3"){
      valor<-3
    }else if (substr(exporiginal,entero,entero)=="4"){
      valor<-4
    }else if (substr(exporiginal,entero,entero)=="5"){
      valor<-5
    }else if (substr(exporiginal,entero,entero)=="6"){
      valor<-6
    }else if (substr(exporiginal,entero,entero)=="7"){
      valor<-7
    }else if (substr(exporiginal,entero,entero)=="8"){
      valor<-8
    }else if (substr(exporiginal,entero,entero)=="9"){
      valor<-9
    }else if (substr(exporiginal,entero,entero)==","){
      valor<-"no"
    }
    
    if (valor!="no"){   #realizando excepción del caracter ","
      exponumerica<-exponumerica + valor* (10^potencia)
    }
  }
  exponumerica<-round(exponumerica,1) #redondeo al primer decimal
  if (exponumerica>2.1){    #exportación según problema planteado
    datosCSV[x,5]<- 1
  }else if (exponumerica<2.1){
    datosCSV[x,5]<- 2  
  }else if (exponumerica==2.1){
    datosCSV[x,5]<- 3
  }
}
print(datosCSV$exportaciones)

  
# bonus track. Graficar algunas variables, en función de una pregunta
# pregunta: represente los ingresos y gastos de empresas chilenas, agrupados por tipo

baseChile<-datosCSV[datosCSV$pais=="chile",c(2,3,12)]  #extrae datos de Chile
ultimo<-nrow(baseChile)

ingresos<-numeric()   #vector que tiene los ingresos
for (x in 1:ultimo){    # rutina para convertir chr a número
  exporiginal<-baseChile[x,1]
  n_datos<-nchar(baseChile[x,1])
  busca_coma<-0
  for (pos_coma in 1:n_datos){
    if (substr(exporiginal,pos_coma,pos_coma)==","){
      busca_coma<-pos_coma
      y<-n_datos
    }
  }
  exponumerica<-0
  for (entero in 1:n_datos){
    if (entero<busca_coma){
      potencia<-busca_coma - entero -1
    }else{
      potencia<-busca_coma - entero
    }
    if (substr(exporiginal,entero,entero)=="0"){
      valor<-0
    }else if (substr(exporiginal,entero,entero)=="1"){
      valor<-1
    }else if (substr(exporiginal,entero,entero)=="2"){
      valor<-2
    }else if (substr(exporiginal,entero,entero)=="3"){
      valor<-3
    }else if (substr(exporiginal,entero,entero)=="4"){
      valor<-4
    }else if (substr(exporiginal,entero,entero)=="5"){
      valor<-5
    }else if (substr(exporiginal,entero,entero)=="6"){
      valor<-6
    }else if (substr(exporiginal,entero,entero)=="7"){
      valor<-7
    }else if (substr(exporiginal,entero,entero)=="8"){
      valor<-8
    }else if (substr(exporiginal,entero,entero)=="9"){
      valor<-9
    }else if (substr(exporiginal,entero,entero)==","){
      valor<-"no"
    }
    
    if (valor!="no"){   #realizando excepción del caracter ","
      exponumerica<-exponumerica + valor* (10^potencia)
    }
  }
  ingresos[x]<- exponumerica
}

gastos<-numeric()   #vector que tiene los gastos
for (x in 1:ultimo){    # rutina para convertir chr a número
  exporiginal<-baseChile[x,2]
  n_datos<-nchar(baseChile[x,2])
  busca_coma<-0
  for (pos_coma in 1:n_datos){
    if (substr(exporiginal,pos_coma,pos_coma)==","){
      busca_coma<-pos_coma
      y<-n_datos
    }
  }
  exponumerica<-0
  for (entero in 1:n_datos){
    if (entero<busca_coma){
      potencia<-busca_coma - entero -1
    }else{
      potencia<-busca_coma - entero
    }
    if (substr(exporiginal,entero,entero)=="0"){
      valor<-0
    }else if (substr(exporiginal,entero,entero)=="1"){
      valor<-1
    }else if (substr(exporiginal,entero,entero)=="2"){
      valor<-2
    }else if (substr(exporiginal,entero,entero)=="3"){
      valor<-3
    }else if (substr(exporiginal,entero,entero)=="4"){
      valor<-4
    }else if (substr(exporiginal,entero,entero)=="5"){
      valor<-5
    }else if (substr(exporiginal,entero,entero)=="6"){
      valor<-6
    }else if (substr(exporiginal,entero,entero)=="7"){
      valor<-7
    }else if (substr(exporiginal,entero,entero)=="8"){
      valor<-8
    }else if (substr(exporiginal,entero,entero)=="9"){
      valor<-9
    }else if (substr(exporiginal,entero,entero)==","){
      valor<-"no"
    }
    
    if (valor!="no"){   #realizando excepción del caracter ","
      exponumerica<-exponumerica + valor* (10^potencia)
    }
  }
  gastos[x]<- exponumerica
}

porte<-baseChile$tamanio
porte2<-matrix(0,2,4)
colnames(porte2)<-c("grande","mediana","pequeña","micro")
row.names(porte2)<-c("ingresos","gastos")
for (x in 1:ultimo){
  if (porte[x]=="grande"){
    porte2[1,1]<-porte2[1,1]+ingresos[x]
    porte2[2,1]<-porte2[2,1]+gastos[x]
  }else if (porte[x]=="mediana"){
    porte2[1,2]<-porte2[1,2]+ingresos[x]
    porte2[2,2]<-porte2[2,2]+gastos[x]
  }else if (porte[x]=="pequena"){
    porte2[1,3]<-porte2[1,3]+ingresos[x]
    porte2[2,3]<-porte2[2,3]+gastos[x]
  }else if (porte[x]=="micro"){
    porte2[1,4]<-porte2[1,4]+ingresos[x]
    porte2[2,4]<-porte2[2,4]+gastos[x]
  }
}

barplot(porte2, main="Empresas Chilenas", xlab="tipo de empresa", ylab="monto total periodo 2012-2017", col=c("green","purple"),
        legend = rownames(porte2), beside=TRUE, ylim = c(0, 2000))
porte2

 


