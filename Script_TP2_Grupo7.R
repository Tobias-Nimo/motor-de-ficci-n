##############################################################
#          Computación Científica Actuarial (746)            #
#             Facultad de Ciencias Económicas                #
#               Universidad de Buenos Aires                  #
##############################################################

################### Trabajo Práctico N°2 #####################

#### Docente: Rodrigo Del Rosso

#### Colaboradores: Santiago Silva - Auza - Sanchez Gavier - Ponce

#### Deadline: 09/12/2020

#### Integrantes del grupo 7: Tobías Nimo - Franco Natiello - Lautaro Ferroni - Silvina Solange Martin



#_______________________________________________________________________________
# ESTUDIO DE SUPERVIVENCIA CON INFORMACION COMPLETA

# Se confecciona la función "Estudio.Completo".

Estudio.Completo <- function(t, nt){
  
  # t: tiempo
  # n(t): sobrevivientes
  
  n0 <- nt[1] # tamaño poblacion inicial
  
  # estimacion de funciones biometricas ...
  
  St = NULL
  for (i in 1:length(t)) {
    St[i] = nt[i]/n0
  } # S(t)
  
  dt = NULL
  for (i in 1:length(t)) {
    dt[i] = nt[i] - nt[i+1] 
    dt[is.na(dt)] <- 0
  } # d(t,0,1)
  
  pt = NULL
  for (i in 1:length(t)) {
    pt[i] = round(nt[i+1] / nt[i],4) 
    pt[is.na(pt)] <- 0
  } # p(t,1)
  
  qt = NULL
  for (i in 1:length(t)) {
    qt[i] = round(dt[i] / nt[i],4) 
    qt[is.na(qt)] <- 1
  } # q(t,0,1)
  
  
  q.dif = NULL
  for (i in 1:length(t)) {
    q.dif[i] = round(dt[i] / n0,4) 
    q.dif[is.na(q.dif)] <- 1
  } # q(0,t,1)
  
  Mu = NULL
  for (i in 1:length(t)) {
    Mu[i] = round(-log(pt[i]),4) 
  } # Mu(t)
  
  # tabla con los resultados de los estimadores ...
  
  tabla = data.frame(t,nt,St,dt,pt,qt,q.dif,Mu)
  colnames(tabla) <- c("t", "n(t)", "S(t)", "d(t,0,1)", "p(t,1)",
                       "q(t,0,1)", "q(0,t,1)", "Mu(t)")
  
  plot(data.frame(t, St), type = "s", xlab = "t", ylab = "S(t)", ylim = c(0,1),
       col = "red")
  
  return(tabla)
}

# A continuación se muestran 3 aplicaciones prácticas para la función.

# Ejemplo 1:

datos.1 <- read.csv2(file = "Dataset - Ejemplo.1.csv", header = T)
datos.1

t <- datos.1$Tiempo
nt <- datos.1$Sobrevivientes

Estudio.Completo(t,nt)

# Ejemplo 2:

datos.2 <- read.csv2(file = "Dataset - Ejemplo.2.csv", header = T)
datos.2

t <- datos.2$Tiempo
nt <- datos.2$Pacientes

Estudio.Completo(t,nt)

# Ejemplo 3:

datos.3 <- read.csv2(file = "Dataset - Ejemplo.3.csv", header = T)
datos.3 # datos exactos

# Es necesario agrupar los datos, antes de introducirlos en la función.
# Para ello se elabora una rutina que de forma automática transforma una base
# de datos exactos otra con datos agrupados.

time = datos.3$Falleció.por.motivo.de.la.enfermedad.en.X.meses
n0 = max(datos.3$persona)
v = 0:max(time)
counts = table(factor(time))
w = data.frame(as.numeric(counts), sort(unique(time)))

slave <- function(v,n){
  for (i in 1:length(v)) {
    if(v[i] == n) break
  }
  lista=list(valor=1,pos=i)
  return(lista)
}

z = rep(n0,length(v))
cont = 1
for (k in w$sort.unique.time..) {
  if (slave(v,k)$valor==1) z[slave(v,k)$pos]=w$as.numeric.counts.[cont]
  cont=cont+1
}

slave.sum <- function(v,i){
  w = NULL
  for (j in 1:i) {
    w[j] = v[j]
    sum(w)
  }
  return(sum(w))
}

x = z
cont = 1 
for (i in min(sort(unique(time))):(length(z))) {
  N0 = n0
  if (z[i] != n0) {
    N0 = N0 - slave.sum(w$as.numeric.counts.,cont) 
    x[i] = N0
    cont = cont + 1
  } else if (z[i] == n0) x[i]=x[i-1]
} # Listo, ahora se tiene datos parciales !

t <- v # tiempo
nt <- x # sobrevivientes

Estudio.Completo(t,nt)

#_______________________________________________________________________________
# ESTUDIO DE SUPERVIVENCIA CON INFORMACION INCOMPLETA

# A continuación, se presenta el código de la función "Exposicion".

Exposicion <- function(Nacimiento, Fallecimiento, año_estudio, x, exp. = c("programada","exacta + duf","exacta + Mu.cte")){
  
  Y = difftime(año_estudio, Nacimiento, units = "auto") / 365
  Z = Y + 1
  
  # edad fraccionaria de entrada
  r = NULL
  for (i in 1:length(Y)) {
    if(Y[i] < x) r[i] = x
    else r[i] = Y[i]
  }
  
  # edad fraccionaria de salida
  s = NULL
  for (i in 1:length(Z)) {
    if(Z[i] > x+1) s[i] = x+1
    else s[i] = Z[i]
  }
  
  # edad fraccionaria al fallecimiento
  t = NULL
  for (i in 1:length(Y)) {
    t[i] = difftime(Fallecimiento[i], Nacimiento[i]) / 365
  }
  t[is.na(t)] <- 0
  for (i in 1:length(Y)) {
    if (t[i] == 0) t[i] = s[i]
  }
  for (i in 1:length(Y)) {
    if (t[i] > s[i]) t[i] = s[i]
  }
  
  # N° de fallecidos
  v = NULL
  d = 0
  for (i in 1:length(t)) {
    v[i] = difftime(Fallecimiento[i], Nacimiento[i]) / 365
  }
  v[is.na(v)] <- 0
  for (i in 1:length(v)) {
    if (v[i] < x+1 & v[i] != 0) d = d + 1
  }
  
  # estimacion de q(x,0,1) 
  if (exp. == "programada"){  
    q = d/sum(s - r) 
  } else if (exp. == "exacta + duf"){
    m = d/sum(t - r)
    q = m/(1-0.5*m)
  } else if (exp. == "exacta + Mu.cte"){
    mu = d/sum(t - r)
    q = 1 - exp(-mu) 
  } 
  
  tabla <- data.frame(Nacimiento,Fallecimiento,Y,r,Z,s,t)
  tabla$Y = as.numeric(tabla$Y)
  tabla$Z = as.numeric(tabla$Z)
  colnames(tabla) = c("Nacimiento", "Fallecimiento", "Y", "x+r", "Z", "x+s", "x+t")
  
  q = paste("q(",x,", 0 , 1) = ",q)
  lista.exposicion <- list(tabla = tabla, estimacion = q)
  
  return(lista.exposicion)
}

# A continuación se muestran 2 aplicaciones prácticas para la función.

# Ejemplo 4:

datos.4 <- read.csv2(file = "Dataset - Ejemplo.4.csv", header = T)
datos.4

# Primero, es necesario corregir el formato de las fechas.

datos.4$Nacimiento = as.Date(datos.4$Nacimiento, format="%d/%m/%Y")
datos.4$Fallecimiento = as.Date(datos.4$Fallecimiento, format="%d/%m/%Y")
datos.4

Nacimiento <- datos.4$Nacimiento
Fallecimiento <- datos.4$Fallecimiento

# Se desea estimar q(30,0,1)
x = 30

# El periodo de observacion será el año 1999 
año_estudio = as.Date("1999-1-1")
año_estudio

# Se hace uso de la función "Exposicion" para estimar q(30,0,1) mediante los 
# métodos de exposición programada y exacta al riesgo.

# via método de exposición programada:
Metodo_exposicion_programda <- Exposicion(Nacimiento, Fallecimiento,
                                          año_estudio, x, exp. = "programada")
Metodo_exposicion_programda$tabla
Metodo_exposicion_programda$estimacion

# via metodo de exposicion exacta, con supuesto uniforme:
Metodo_exposicion_exacta_duf <- Exposicion(Nacimiento, Fallecimiento,
                                           año_estudio, x, exp. = "exacta + duf")
Metodo_exposicion_exacta_duf$estimacion

# via metodo de exposicion exacta, con supuesto exponencial:
Metodo_exposicion_exacta_exp <- Exposicion(Nacimiento, Fallecimiento, 
                                           año_estudio, x, exp. = "exacta + Mu.cte")
Metodo_exposicion_exacta_exp$estimacion

# Ejemplo 5:

datos.5 <- read.csv2(file = "Dataset - Ejemplo.5.csv", header = T)
datos.5 <- datos.5[,1:3]
head(datos.5)
str(datos.5)

# Primero, es necesario corregir el formato de las fechas.

datos.5$f..de.nacimiento = as.Date(datos.5$f..de.nacimiento, format = "%d/%m/%Y")
datos.5$fecha.muerte = as.Date(datos.5$fecha.muerte, format = "%d/%m/%Y")
View(datos.5)

Nacimiento <- datos.5$f..de.nacimiento
Fallecimiento <- datos.5$fecha.muerte

# Se desea estimar estimar q(78,0,1)
x = 78

# El periodo de observacion será el año 2019 
año_estudio = as.Date("2019-1-1")
año_estudio

# Se hace uso de la función "Exposicion" para estimar q(78,0,1) mediante los 
# métodos de exposición programada y exacta al riesgo.

# via metodo de exposicion programada:
Metodo_exposicion_programda <- Exposicion(Nacimiento, Fallecimiento, 
                                          año_estudio, x, exp. = "programada")
Metodo_exposicion_programda$tabla
Metodo_exposicion_programda$estimacion

# via metodo de exposicion exacta, con supuesto uniforme:
Metodo_exposicion_exacta_duf <- Exposicion(Nacimiento, Fallecimiento, 
                                           año_estudio, x, exp. = "exacta + duf")
Metodo_exposicion_exacta_duf$estimacion

# via metodo de exposicion exacta, con supuesto exponencial:
Metodo_exposicion_exacta_exp <- Exposicion(Nacimiento, Fallecimiento, 
                                           año_estudio, x, exp. = "exacta + Mu.cte")
Metodo_exposicion_exacta_exp$estimacion

#_______________________________________________________________________________
# KAPLAN-MEIR & NELSON-AALEN

# Se presenta el diseño de la función "K.M".

K.M <- function(tiempo,estado){
  
  # funcion 1
  tabla.base.KM <- function(tiempo, estado){
    
    slave.1 <- function(df, eventosi){
      ri = NULL
      for (i in 1:length(eventosi)) {
        if (i == 1){
          ri[i] = length(df$tiempo)
        } else 
          ri[i] = ri[i-1]-eventosi[i-1]
      }
      return(ri)
    }
    
    ti = unique(sort(tiempo))
    di =  tapply(estado, tiempo, sum)
    eventosi = tapply(estado, tiempo, length)
    ri = slave.1(df,eventosi)
    tabla_base = data.frame(ti,di,eventosi,ri)
    colnames(tabla_base) <- c("ti","di","#eventos","ri")
    return(tabla_base)
  }
  
  # funcion 2
  tabla.estimacion.KM <- function(tabla_base){
    
    slave.2 <- function(tabla_base){
      Ti = NULL
      cont = 1
      for (i in 1:nrow(tabla_base)) {
        if (tabla_base$di[i] != 0)
          Ti[cont] = tabla_base$ti[i]
        cont = cont + 1
      }
      Ti = Ti[is.na(Ti)==F]
      Ti = c(0,Ti)
      return(Ti)
    }
    
    slave.3 <- function(tabla_base){
      pos = NULL
      cont = 1
      for (i in 1:nrow(tabla_base)) {
        if (tabla_base$di[i] != 0) pos[cont] = i  
        cont = cont + 1
      }
      pos = pos[is.na(pos)==F]
      return(pos)
    }
    
    slave.4 <- function(Pi){
      St = NULL
      for (i in 1:length(Pi)) {
        St[i]=prod(Pi[1:i])
      }
      return(St)
    }
    
    slave.5 <- function(Ti){
      dift = NULL
      for (i in 1:length(Ti)) {
        if (i == 1) dift[i]=0
        else dift[i]= Ti[i]-Ti[i-1]
      }
      return(dift)
    }
    
    slave.6 <- function(dift,St){
      areas = NULL
      for (i in 1:length(St)) {
        if (i == 1) areas[i] = 0
        else areas[i]= dift[i]*St[i-1]
      }
      return(areas)
    }
    
    slave.7 <- function(St) {
      var1 = NULL
      for (i in 2:length(St)) {
        var1[i] = St[i]^2
      }
      return(var1)
    }
    
    slave.8 <- function(St,di,ri,pos) {
      var2 = NULL
      for (i in 2:length(St)) {
        var2[i] = sum(di[pos][1:i-1]/(ri[pos][1:i-1]*(ri[pos][1:i-1]-di[pos][1:i-1])))
      }
      return(var2)
    }
    
    Ti = slave.2(tabla_base)
    pos = slave.3(tabla_base)
    Pi = c(1,(ri[pos]-di[pos])/ri[pos])
    St = slave.4(Pi)
    dift = slave.5(Ti)
    areas = slave.6(dift,St)
    var = slave.7(St)*slave.8(St,di,ri,pos) 
    tabla_estimacion = data.frame(Ti,Pi,St,dift,areas,var)
    colnames(tabla_estimacion) <- c("ti*","(ri-di)/ri","S(t)","^t","^t*S(t-1)",
                                    "Var[S(t)]")
    return(tabla_estimacion)
  }
  
  # Estimacion
  df <- data.frame(tiempo, estado)
  
  tabla_base.KM = tabla.base.KM(tiempo,estado)
  attach(tabla_base.KM)
  
  tabla_estimacion.KM = tabla.estimacion.KM(tabla_base.KM)
  attach(tabla_estimacion.KM)
  
  plot(data.frame(tabla_estimacion.KM$`ti*`,tabla_estimacion.KM$`S(t)`), type = "s",
       xlab = "ti*", ylab = "S(t)", ylim = c(0,1), col = "red")
  E = paste("E(T) = ",sum(tabla_estimacion.KM$`^t*S(t-1)`)) 
  
  KM.list <- list(tabla_base = tabla_base.KM, tabla_estimacion = tabla_estimacion.KM, Esperanza_vida = E)
  
  return(KM.list)
}

# Se presenta el diseño de la función "N.A".

N.A <- function(tiempo, estado){
  
  # funcion 1
  tabla.base.NA <- function(tiempo, estado){
    
    slave.1 <- function(df, eventosi){
      ri = NULL
      for (i in 1:length(eventosi)) {
        if (i == 1){
          ri[i] = length(df$tiempo)
        } else 
          ri[i] = ri[i-1]-eventosi[i-1]
      }
      return(ri)
    }
    
    ti = unique(sort(tiempo))
    di =  tapply(estado, tiempo, sum)
    eventosi = tapply(estado, tiempo, length)
    ri = slave.1(df,eventosi)
    tabla_base = data.frame(ti,di,eventosi,ri)
    colnames(tabla_base) <- c("ti","di","#eventos","ri")
    return(tabla_base)
  }
  
  # funcion 2
  tabla.estimacion.NA <- function(tabla_base){
    
    slave.2 <- function(tabla_base){
      Ti = NULL
      cont = 1
      for (i in 1:nrow(tabla_base)) {
        if (tabla_base$di[i] != 0)
          Ti[cont] = tabla_base$ti[i]
        cont = cont + 1
      }
      Ti = Ti[is.na(Ti)==F]
      Ti = c(0,Ti)
      return(Ti)
    }
    
    slave.3 <- function(tabla_base){
      pos = NULL
      cont = 1
      for (i in 1:nrow(tabla_base)) {
        if (tabla_base$di[i] != 0) pos[cont] = i  
        cont = cont + 1
      }
      pos = pos[is.na(pos)==F]
      return(pos)
    }
    
    slave.4 <- function(RsobreD){
      Mu = NULL
      for (i in 1:length(RsobreD)) {
        Mu[i] = sum(RsobreD[1:i])
      }
      return(Mu)
    }
    
    slave.5 <- function(Mu){
      St = NULL
      for (i in 1:length(Mu)) {
        St[i] = exp(-Mu[i])
      }
      return(St)
    }
    
    slave.6 <- function(Ti){
      dift = NULL
      for (i in 1:length(Ti)) {
        if (i == 1) dift[i]=0
        else dift[i]= Ti[i]-Ti[i-1]
      }
      return(dift)
    }
    
    slave.7 <- function(dift,St){
      areas = NULL
      for (i in 1:length(St)) {
        if (i == 1) areas[i] = 0
        else areas[i]= dift[i]*St[i-1]
      }
      return(areas)
    }
    
    slave.8 <- function(St,di,ri,pos) {
      var = NULL
      for (i in 2:length(St)) {
        var[i] = sum(di[pos][1:i-1]/(ri[pos][1:i-1]*ri[pos][1:i-1]))
      }
      return(var)
    }
    
    Ti = slave.2(tabla_base)
    pos = slave.3(tabla_base)
    RsobreD = c(0,di[pos]/ri[pos])
    Mu.acum = slave.4(RsobreD)
    St = slave.5(Mu.acum)
    dift = slave.6(Ti)
    areas = slave.7(dift, St)
    var_Mu.acum = slave.8(St,di,ri,pos)
    tabla_estimacion = data.frame(Ti,RsobreD,Mu.acum,St,dift,areas,var_Mu.acum)
    colnames(tabla_estimacion) <- c("ti*","di/ri","Cumulative H-R","S(t)","^t","^t*S(t-1)","Var(Cumulative H-R)")
    return(tabla_estimacion)
  }
  
  #Estimacion
  df <- data.frame(tiempo, estado)
  
  tabla_base.NA = tabla.base.NA(tiempo,estado)
  attach(tabla_base.NA)
  
  tabla_estimacion.NA = tabla.estimacion.NA(tabla_base.NA)
  attach(tabla_estimacion.NA)
  
  plot(data.frame(tabla_estimacion.NA$`ti*`,tabla_estimacion.NA$`S(t)`),type = "s", xlab = "ti*",ylab = "S(t)",ylim=c(0,1),col="red")
  E = paste("E(T) = ",sum(tabla_estimacion.NA$`^t*S(t-1)`))
  
  NA.list <- list(tabla_base = tabla_base.NA, tabla_estimacion = tabla_estimacion.NA, Esperanza_vida = E)
  
  return(NA.list)
} 

# Se procede a mostrar dos aplicaciones prácticas para las funciones.

# Ejemplo 6:

datos.6 = read.csv2("Dataset - Ejemplo.6.csv", header = T)
datos.6

tiempo <- datos.6$Tiempo
estado <- datos.6$Estado

# Estimacion usando la funcion K.M ...
Estimador_K.M <- K.M(tiempo,estado)

Estimador_K.M$tabla_base
Estimador_K.M$tabla_estimacion
Estimador_K.M$Esperanza_vida 

# Estimacion usando la funcion N.A ...
Estimador_N.A <- N.A(tiempo,estado)

Estimador_N.A$tabla_base
Estimador_N.A$tabla_estimacion
Estimador_N.A$Esperanza_vida 

# Ejemplo 7:

datos.7 = read.csv2("Dataset - Ejemplo.7.csv", header = T)
View(datos.7)

tiempo <- datos.7$studytime
estado <- datos.7$died

# Estimacion usando la funcion K.M ...
Estimador_K.M <- K.M(tiempo,estado)

Estimador_K.M$tabla_base
Estimador_K.M$tabla_estimacion
Estimador_K.M$Esperanza_vida 

# Estimacion usando la funcion N.A ...
Estimador_N.A <- N.A(tiempo,estado)

Estimador_N.A$tabla_base
Estimador_N.A$tabla_estimacion
Estimador_N.A$Esperanza_vida 
