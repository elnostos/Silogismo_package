#' Primera figura del silogismo
#'
#' Si el modo es válido, devuelve la primera figura del silogismo con el modo indicado.
#' Si el modo es inválido, devuelve una advertencia. 
#' @param mayor El término mayor entre comillas.
#' @param medio El término medio entre comillas.
#' @param menor El término menor entre comillas.
#' @return La primera figura del silogismo según el modo indicado, en caso de ser válido.
#' @examples 
#' silogismo1 <- primer_silogismo("Hombre", "Animal", "Piedra", "BARBARA");
#' silogismo2 <- primer_silogismo("Hombre", "Animal", "Piedra", "EAE");
#' @export


primer_silogismo <- function(mayor, medio, menor, modo) {
  if(modo=="BARBARA"| modo =="AAA") {
    print(paste("P1:", "Todo(a)", medio, "es", mayor, "/", "P2:", "Todo(a)", menor, "es",
                medio, "/", "Conclusion:", "Todo(a)", menor, "es", mayor, sep=" "))
  } else if(modo=="CELARENT"| modo =="EAE") {
    print(paste("P1:", "Ningun(a)", medio, "es", mayor, "/", "P2:", "Todo(a)", menor, "es",
                medio, "/", "Conclusion:", "Ningun(a)", menor, "es", mayor, sep=" "))  
  } else if(modo=="DARII"| modo =="AII") {
    print(paste("P1:", "Todo(a)", medio, "es", mayor, "/", "P2:", "Algun(a)", menor, "es",
                medio, "/", "Conclusion:", "Algun(a)", menor, "es", mayor, sep=" "))
  } else if(modo=="FERIO"| modo =="EIO") {
    print(paste("P1:", "Ningun(a)", medio, "es", mayor, "/", "P2:", "Algun(a)", menor, "es",
                medio, "/", "Conclusion:", "Algun(a)", menor, "no es", mayor, sep=" "))
  } else {
    print("Figura invalida del primer silogismo")
  }
}

#' Segunda figura del silogismo
#'
#' Si el modo es válido, devuelve la segunda figura del silogismo con el modo indicado.
#' Si el modo es inválido, devuelve una advertencia. 
#' @param mayor El término mayor entre comillas.
#' @param medio El término medio entre comillas.
#' @param menor El término menor entre comillas.
#' @return La segunda figura del silogismo según el modo indicado, en caso de ser válido.
#' @examples 
#' silogismo1 <- segundo_silogismo("Hombre", "Animal", "Piedra", "CAMESTRES");
#' silogismo2 <- segundo_silogismo("Hombre", "Animal", "Piedra", "EIO");
#' @export

segundo_silogismo <- function(mayor, medio, menor, modo) {
  if(modo=="CESARE"| modo =="EAE") {
    print(paste("P1:", "Ningun(a)", mayor, "es", medio, "/", "P2:", "Todo(a)", menor, "es",
                medio, "/", "Conclusion:", "Ningun(a)", menor, "es", mayor, sep=" "))
  } else if(modo=="CAMESTRES"| modo =="AEE") {
    print(paste("P1:", "Todo(a)", mayor, "es", medio, "/", "P2:", "Ningun(a)", menor, "es",
                medio, "/", "Conclusion:", "Ningun(a)", menor, "es", mayor, sep=" "))  
  } else if(modo=="FESTINO"| modo =="EIO") {
    print(paste("P1:", "Ningun(a)", mayor, "es", medio, "/", "P2:", "Algun(a)", menor, "es",
                medio, "/", "Conclusion:", "Algun(a)", menor, "no es", mayor, sep=" "))
  } else if(modo=="BAROCO"| modo =="AOO") {
    print(paste("P1:", "Todo(a)", mayor, "es", medio, "/", "P2:", "Algun(a)", menor, "no es",
                medio, "/", "Conclusion:", "Algun(a)", menor, "no es", mayor, sep=" "))
  } else {
    print("Figura invalida del segundo silogismo")
  }
}


#' Tercera figura del silogismo
#'
#' Si el modo es válido, devuelve la tercera figura del silogismo con el modo indicado.
#' Si el modo es inválido, devuelve una advertencia. 
#' @param mayor El término mayor entre comillas.
#' @param medio El término medio entre comillas.
#' @param menor El término menor entre comillas.
#' @return La tercera figura del silogismo según el modo indicado, en caso de ser válido.
#' @examples 
#' silogismo1 <- tercer_silogismo("Hombre", "Animal", "Piedra", "DATISIS");
#' silogismo2 <- tercer_silogismo("Hombre", "Animal", "Piedra", "EAO");
#' @export

tercer_silogismo <- function(mayor, medio, menor, modo) {
  if(modo=="DATISI"| modo =="AII") {
    print(paste("P1:", "Todo(a)", medio, "es", mayor, "/", "P2:", "Algun(a)", medio, "es",
                menor, "/", "Conclusion:", "Algun(a)", menor, "es", mayor, sep=" "))
  } else if(modo=="DISAMIS"| modo =="IAI") {
    print(paste("P1:", "Algun(a)", medio, "es", mayor, "/", "P2:", "Todo(a)", medio, "es",
                menor, "/", "Conclusion:", "Algun(a)", menor, "es", mayor, sep=" "))  
  } else if(modo=="FERISON"| modo =="EIO") {
    print(paste("P1:", "Ningun(a)", medio, "es", mayor, "/", "P2:", "Algun(a)", medio, "es",
                menor, "/", "Conclusion:", "Algun(a)", menor, "no es", mayor, sep=" "))
  } else if(modo=="BOCARDO"| modo =="OAO") {
    print(paste("P1:", "Algun(a)", medio, "no es", mayor, "/", "P2:", "Todo(a)", medio, "es",
                menor, "/", "Conclusion:", "Algun(a)", menor, "no es", mayor, sep=" "))
  } else if(modo=="DARAPTI"| modo =="AAI") {
    print(paste("P1:", "Todo(a)", medio, "es", mayor, "/", "P2:", "Todo(a)", medio, "es",
                menor, "/", "Conclusion:", "Algun(a)", menor, "es", mayor, sep=" "))
  } else if(modo=="FELAPTON"| modo =="EAO") {
    print(paste("P1:", "Ningun(a)", medio, "es", mayor, "/", "P2:", "Todo(a)", medio, "es",
                menor, "/", "Conclusion:", "Algun(a)", menor, "no es", mayor, sep=" "))
  } else {
    print("Figura invalida del tercer silogismo")
  }
}

#' Cuarta figura del silogismo
#'
#' Si el modo es válido, devuelve la cuarta figura del silogismo con el modo indicado.
#' Si el modo es inválido, devuelve una advertencia. 
#' @param mayor El término mayor entre comillas.
#' @param medio El término medio entre comillas.
#' @param menor El término menor entre comillas.
#' @return La cuarta figura del silogismo según el modo indicado, en caso de ser válido.
#' @examples 
#' silogismo1 <- cuarto_silogismo("Hombre", "Animal", "Piedra", "CAMENES");
#' silogismo2 <- cuarto_silogismo("Hombre", "Animal", "Piedra", "EAO");
#' @export


cuarto_silogismo <- function(mayor, medio, menor, modo) {
  if(modo=="BAMALIP"| modo =="AAI") {
    print(paste("P1:", "Todo(a)", mayor, "es", medio, "/", "P2:", "Todo(a)", medio, "es",
                menor, "/", "Conclusion:", "Algun(a)", menor, "es", mayor, sep=" "))
  } else if(modo=="FESAPO"| modo =="EAO") {
    print(paste("P1:", "Ningun(a)", mayor, "es", medio, "/", "P2:", "Todo(a)", medio, "es",
                menor, "/", "Conclusion:", "Algun(a)", menor, "no es", mayor, sep=" "))  
  } else if(modo=="CAMENES"| modo =="AEE") {
    print(paste("P1:", "Todo(a)", mayor, "es", medio, "/", "P2:", "Ningun(a)", medio, "es",
                menor, "/", "Conclusion:", "Ningun(a)", menor, "es", mayor, sep=" "))
  } else if(modo=="DIMATIS"| modo =="IAI") {
    print(paste("P1:", "Algun(a)", mayor, "es", medio, "/", "P2:", "Todo(a)", medio, "es",
                menor, "/", "Conclusion:", "Algun(a)", menor, "es", mayor, sep=" "))
  } else if(modo=="FRESISON"| modo =="EIO") {
    print(paste("P1:", "Ningun(a)", mayor, "es", medio, "/", "P2:", "Algun(a)", medio, "es",
                menor, "/", "Conclusion:", "Algun(a)", menor, "no es", mayor, sep=" "))
  } else {
    print("Figura invalida del cuarto silogismo")
  }
}

#' Silogismo
#'
#' Si el modo es válido, devuelve la figura indicada del silogismo a partir de los términos definidos.
#' Si el modo es inválido, devuelve una advertencia. 
#' @param mayor El término mayor entre comillas.
#' @param medio El término medio entre comillas.
#' @param menor El término menor entre comillas.
#' @param modo El modo del silogismo entre comillas.
#' @param figura La figura del silogismo entre comillas o en valor númerico.
#' @return La figura del silogismo según el modo indicado, en caso de ser válido.
#' @examples 
#' silogismo1 <- Silogismo("Hombre", "Animal", "Piedra", "CAMENES", "Cuarto");
#' silogismo2 <- Silogismo("Hombre", "Animal", "Piedra", "AAA", 1);
#' @export

Silogismo <- function(mayor, medio, menor, modo, figura) {
  if(figura=="Primera" | figura==1){
    primer_silogismo(mayor, medio, menor, modo)
  } else if(figura=="Segunda" | figura==2){
    segundo_silogismo(mayor, medio, menor, modo)
  } else if(figura=="Tercera" | figura==3){
    tercer_silogismo(mayor, medio, menor, modo)
  } else if(figura=="Cuarta" | figura==4){
      cuarto_silogismo(mayor, medio, menor, modo)
  } else {
    print("Ingrese una figura válida")
  }
}