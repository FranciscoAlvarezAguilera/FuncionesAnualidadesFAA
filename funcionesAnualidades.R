# F1 Tarea proyecto integrador 1 (primera parte del examen final) 

# FRANCISCO ALVAREZ AGUILERA - 3er Semestre - Matematicas Financieras - LACD

# VENCIDAS  VF #
# 1
VFVenc = function(tasa, numeroPeriodos, anualidadPago){
  ValorFinal = anualidadPago*((1+tasa)^numeroPeriodos-1)/tasa
  
  return(ValorFinal)
}

# 2

pagoVFVenc = function(tasa, numeroPeriodos, valorFinal){
  Anualidad = valorFinal/(((1+tasa)^numeroPeriodos-1)/(tasa))
  
  return(Anualidad)
}

# 3

tasaVFVenc  = function(nper, pago, VF){
  tasa = 1
  
  while (((((tasa+1)^nper-1)/tasa)-(VF/pago)) > (0.00001)) {
    
    if((((tasa+1)^nper-1)/tasa) > (VF/pago)){
      
      tasa = tasa*0.99999
      
    }
    else{
      
      tasa = tasa*1.00001
      
    }
  }
  return(tasa)
}

# 4

periodosVFVenc = function(tasa, pago, valorFinal){
  numPeriodos = log((valorFinal*tasa/pago)+1) / log(1+tasa)
  
  return(numPeriodos)
}
# VENCIDAS VA #
# 5

VAVenc = function(tasa, nper, pago){
  
  VA = pago*((1-(1 + tasa)^-nper)/tasa)
  
  return(VA)
}

# 6

pagoVAVenc = function(tasa, nper, VA){
  pago = VA / ((1-(1 + tasa)^-nper)/tasa)
  
  return(pago)
}

# 7

tasaVAVenc = function(nper, pago, VA){
  
  tasa = 1
  
  while (((VA/pago)-((1-(1+tasa)^-nper)/tasa)) > (0.00001)) {
    
    if((VA/pago) > ((1-(1+tasa)^-nper)/tasa)){
      
      tasa = tasa*0.99999
      
    }
    else{
      
      tasa = tasa*1.00001
      
    }
  }
  return(tasa)
}

# 8

periodosVAVenc = function(tasa, pago, VA){
  nper = -log(1-(VA*tasa/pago)) / log(1+tasa)
  
  return(nper)
}

# ANTICIPADAS VF #

# 9

VFAnt = function(tasa, nper, pago){
  VF = pago*(((1+tasa)^nper-1)/tasa)*(1+tasa)
  
  return(VF)
}

# 10

pagoVFAnt = function(tasa, nper, VF){
  pago = VF / ((((1+tasa)^nper-1)/tasa)*(1+tasa))
  
  return(pago)
}

# 11

tasaVFAnt = function(nper, pago, VF){
  
  tasa = 1
  
  while (((((tasa+1)^nper-1)/tasa)*(1+tasa))-(VF/pago) > (0.00001)) {
    
    if(((((tasa+1)^nper-1)/tasa)*(1+tasa)) > (VF/pago)){
      
      tasa = tasa*0.99999
      
    }
    else{
      
      tasa = tasa*1.00001
      
    }
  }
  return(tasa)
}

# 12

periodosVFAnt = function(tasa, pago, VF){
  nper = log(((VF*tasa)/(pago*(1+tasa)))+1) / log(1+tasa)
  
  return(nper)
}

# ANTICIPADAS VA #

# 13

VAAnt = function(tasa, nper, pago){
  VA = (1+tasa)*pago*(1-(1+tasa)^-nper)/tasa
  
  return(VA)
}

# 14

pagoVAAnt = function(tasa, nper, VA){
  pago = VA / (((1-(1+tasa)^-nper)/tasa)*(1+tasa))
  
  return(pago)
}

# 15

tasaVAAnt = function(nper, pago, VA){
  
  tasa = 1
  
  while (((VA/pago)-((((1-(1+tasa)^-nper)/tasa))*(1+tasa))) > (0.00001)){
    
    if((VA/pago) > (((1-(1+tasa)^-nper)/tasa)*(1+tasa))){
      
      tasa = tasa*0.99999
      
    }
    else{
      
      tasa = tasa*1.00001
      
    }
  }
  return(tasa)
  
}

# 16

periodosVAAnt = function(tasa, pago, VA){
  periodos = -log(((-VA*tasa)/(pago*(1+tasa)))+1)/log(1+tasa)
  
  return(periodos)
}

# DIFERIDAS VA #

# 17

VADif = function(tasa, nper, pago, nperDiferimiento){
  
  # Obtenemos el VA2 que sera el "VF" del VA1:
  
  VA2 = pago*((1-(1 + tasa)^-nper)/tasa)
  
  # Encontramos VA1 normalmente con la formula de i compuesto
  
  VA1 = VA2 / ((1+tasa)^nperDiferimiento)
  
  return(VA1)
  
}

# 18

pagoVADif = function(tasa, nper, nperDiferimiento,VA1){
  
  pago = (VA1*((1+tasa)^nperDiferimiento)) / ((1-(1 + tasa)^-nper)/tasa)
  
  return(pago)
  
  
}








