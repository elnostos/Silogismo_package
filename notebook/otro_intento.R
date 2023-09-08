library(stringr)

#cont<-function(valor){
#  if (valor=="A"){
#    print("O")
#  } else if(valor=="E"){
#    print("I")
#  } else if(valor=="I"){
#    print("E")
#  } else if(valor=="O"){
#    print("A")
#  } else {
#    print("Ingrese un valor valido")
#  }
#}

absurdo<-function(valor, figura){
  axiomas<-c("AAA","EAE","AII","EIO")

  if(str_sub(valor,3,3)=="A"){
    contr<-"O"
  } else if(str_sub(valor,3,3)=="E"){
    contr<-"I"
  } else if(str_sub(valor,3,3)=="I"){
    contr<-"E"
  } else if(str_sub(valor,3,3)=="O"){
    contr<-"A"
  } else {
    print("Ingrese un modo válido")
  }
   
  
  if((valor %in% axiomas)== TRUE & figura==1){
    print("Axioma de la primera figura del silogismo")
  } else if(valor=="EAE" & figura ==2){
      print("Resolver por EAE de la primera figura")
    } else if(valor=="AEE" & figura==2){
      print("Resolver por EAE de la primera figura")
    } else if(valor=="EIO" & figura==2){
      print("Resolver por EIO de la primera figura")
    } else if(valor=="AII" & figura==3){
      print("Resolver por AII de la primera figura")
    } else if(valor=="IAI" & figura==3){
      print("Resolver por AII de la primera figura")
    } else if(valor=="EIO" & figura==3){
      print("Resolver por EIO de la primera figura")
    } else if(valor=="AAI" & figura==3){
      print("Resolver por AII de la primera figura")
    } else if(valor=="EAO" & figura==3){
      print("Resolver por EIO de la primera figura")
    } else if(valor=="AAI" & figura==4){
      print("Resolver por AAA de la primera figura")
    } else if(valor=="EAO" & figura==4){
      print("Resolver por EIO de la primera figura")
    } else if(valor=="AEE" & figura==4){
      print("Resolver por EAE de la primera figura")
    } else if(valor=="IAI" & figura==4){
      print("Resolver por AII de la primera figura")
    } else if(valor=="EIO" & figura==4){
      print("Resolver por EIO de la primera figura")
    } else if((paste(c(str_sub(valor,1,1),contr, "A"), collapse=''))%in% axiomas == TRUE & figura==2){
        print(paste("Resolver por la forma",paste(c(str_sub(valor,1,1),contr, "A"), collapse=''),"de la primera figura del silogismo"))
      } else {
        print("Hola amigo")
      }
} 


figura<-function(sujeto1, predicado1, sujeto2, predicado2, termino_medio){
  if(termino_medio==sujeto1 & termino_medio==predicado2){
    print("Primera figura del silogismo")
  } else if(termino_medio==predicado1 & termino_medio==predicado2){
    print("Segunda figura del silogismo")
  } else if(termino_medio==sujeto1 & termino_medio==sujeto2){
    print("Tercera figura del silogismo")
  } else if(termino_medio==predicado1 & termino_medio==sujeto2){
    print("Cuarta figura del silogismo")
  } else {
    print("Ingrese valores validos")
  }
}





