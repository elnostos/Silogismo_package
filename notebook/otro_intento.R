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


comb_p1<-c("AB","BA")
comb_p2<-c("CA","AC")
comb_fig<-c("AA","AE","AI","AO",
            "EA","EE","EI","EO",
            "IA","IE","II","IO",
            "OA","OE","OI","OO")


formis<-function(AB, CA){
  if(str_sub(AB,1,1)==str_sub(CA,2,2)){
    1
  } else if(str_sub(AB,2,2)==str_sub(CA,2,2)){
    2
  } else if(str_sub(AB,1,1)==str_sub(CA,1,1)){
    3
  } else if(str_sub(AB,2,2)==str_sub(CA,1,1)){
    4
  } else{
    print("Ingrese valores válidos")
  }
}

absurdo<-function(AB, CA, valor){
  
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
   
  if((valor %in% axiomas)== TRUE & formis(AB,CA)==1){
    print("Axioma de la primera figura del silogismo")
    
    } else if((paste(c(str_sub(valor,1,1),contr, "A"), collapse='')) %in% axiomas == TRUE & formis(AB,"CB")==1){
      print(paste("Resolver por la forma",paste(c(str_sub(valor,1,1),contr, "A"), collapse=''),"de la primera figura del silogismo"))
    
    } else if((paste(c(str_sub(valor,1,1),contr, "E"), collapse='')) %in% axiomas == TRUE & formis(AB,"CB")==1){
      print(paste("Resolver por la forma",paste(c(str_sub(valor,1,1),contr, "E"), collapse=''),"de la primera figura del silogismo"))
    
    } else if((paste(c(str_sub(valor,1,1),contr, "I"), collapse='')) %in% axiomas == TRUE & formis(AB,"CB")==1){
      print(paste("Resolver por la forma",paste(c(str_sub(valor,1,1),contr, "I"), collapse=''),"de la primera figura del silogismo"))
    
    } else if((paste(c(str_sub(valor,1,1),contr, "O"), collapse='')) %in% axiomas == TRUE & formis(AB,"CB")==1){
      print(paste("Resolver por la forma",paste(c(str_sub(valor,1,1),contr, "O"), collapse=''),"de la primera figura del silogismo"))
    
    } else if((paste(c(contr, str_sub(valor,2,2), "A"), collapse='')) %in% axiomas == TRUE & formis("CB", CA)==1){
      print(paste("Resolver por la forma",paste(c(contr, str_sub(valor,2,2), "A"), collapse=''),"de la primera figura del silogismo"))
    
    } else if((paste(c(contr, str_sub(valor,2,2), "E"), collapse='')) %in% axiomas == TRUE & formis("CB", CA)==1){
      print(paste("Resolver por la forma",paste(c(contr, str_sub(valor,2,2), "E"), collapse=''),"de la primera figura del silogismo"))
    
    } else if((paste(c(contr, str_sub(valor,2,2), "I"), collapse='')) %in% axiomas == TRUE & formis("CB", CA)==1){
      print(paste("Resolver por la forma",paste(c(contr, str_sub(valor,2,2), "I"), collapse=''),"de la primera figura del silogismo"))
    
    } else if((paste(c(contr, str_sub(valor,2,2), "O"), collapse='')) %in% axiomas == TRUE & formis("CB", CA)==1){
      print(paste("Resolver por la forma",paste(c(contr, str_sub(valor,2,2), "O"), collapse=''),"de la primera figura del silogismo"))
    
    } else {
      print("No puede realizarse reducción al absurdo")
    }
} 









