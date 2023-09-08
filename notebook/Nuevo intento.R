library(stringr)

silogismo<-function(mayor, medio, menor, forma, figura){
  if(str_sub(forma,1,1)=="A" & (figura=="Primera"|figura=="Tercera")){
    primera <- paste("Todo(a)", medio, "es", mayor)
  } else if(str_sub(forma,1,1)=="E" & (figura=="Primera"|figura=="Tercera")){
    primera <- paste("Ningun(a)", medio, "es", mayor)
  } else if(str_sub(forma,1,1)=="I" & (figura=="Primera"|figura=="Tercera")){
    primera <-paste("Algun(a)", medio, "es", mayor)
  } else if(str_sub(forma,1,1)=="O" & (figura=="Primera"|figura=="Tercera")){
    primera <- paste("Algun(a)", medio, "no es", mayor)
  } else if(str_sub(forma,1,1)=="A" & (figura=="Segunda"|figura=="Cuarta")){
    primera <- paste("Todo(a)", mayor, "es", medio)
  } else if(str_sub(forma,1,1)=="E" & (figura=="Segunda"|figura=="Cuarta")){
    primera <- paste("Ningun(a)", mayor, "es", medio)
  } else if(str_sub(forma,1,1)=="I" & (figura=="Segunda"|figura=="Cuarta")){
    primera <-paste("Algun(a)", mayor, "es", medio)
  } else if(str_sub(forma,1,1)=="O" & (figura=="Segunda"|figura=="Cuarta")){
    primera <- paste("Algun(a)", mayor, "no es", medio)
  } else {
    primera <- "Ingrese una letra válida para la primera proposición del silogismo"
  }
  
  if(str_sub(forma,2,2)=="A" & (figura=="Primera"|figura=="Segunda")){
    segunda <- paste("Todo(a)", menor, "es", medio)
  } else if(str_sub(forma,1,1)=="E" & (figura=="Primera"|figura=="Segunda")){
    segunda <- paste("Ningun(a)", menor, "es", medio)
  } else if(str_sub(forma,1,1)=="I" & (figura=="Primera"|figura=="Segunda")){
    segunda <-paste("Algun(a)", menor, "es", medio)
  } else if(str_sub(forma,1,1)=="O" & (figura=="Primera"|figura=="Segunda")){
    segunda <- paste("Algun(a)", menor, "no es", medio)
  } else if(str_sub(forma,1,1)=="A" & (figura=="Tercera"|figura=="Cuarta")){
    segunda <- paste("Todo(a)", medio, "es", menor)
  } else if(str_sub(forma,1,1)=="E" & (figura=="Tercera"|figura=="Cuarta")){
    segunda <- paste("Ningun(a)", medio, "es", menor)
  } else if(str_sub(forma,1,1)=="I" & (figura=="Tercera"|figura=="Cuarta")){
    segunda <-paste("Algun(a)", medio, "es", menor)
  } else if(str_sub(forma,1,1)=="O" & (figura=="Tercera"|figura=="Cuarta")){
    segunda <- paste("Algun(a)", medio, "no es", menor)
  } else {
    segunda <- "Ingrese una letra válida para la segunda proposición del silogismo"
  }
  
  if(str_sub(forma,3,3)=="A"){
    conclusion<-paste("Todo(a)", menor, "es", mayor)
  } else if(str_sub(forma,3,3)=="E"){
    conclusion<-paste("Ningun(a)", menor, "es", mayor)
  } else if(str_sub(forma,3,3)=="I"){
    conclusion<-paste("Algun(a)", menor, "es", mayor)
  } else if(str_sub(forma,3,3)=="O"){
    conclusion<-paste("Algun(a)", menor, "no es", mayor)
  } else {
    conclusion <- "Ingrese una letra válida para la conclusion del silogismo"
  }
  
  print(paste("Primera premisa:", primera))
  print(paste("Segunda premisa:", segunda))
  print(paste("Conclusion:", conclusion))
  
  silo<-c(primera,segunda,conclusion)
}

 
