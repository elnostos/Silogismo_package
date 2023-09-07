primer_silogismo <- function(mayor, medio, menor, modo) {
  if(modo=="BARBARA"| modo =="AAA") {
   primera<- paste("P1:", "Todo(a)", medio, "es", mayor)
   segunda<- paste("P2:", "Todo(a)", menor, "es", medio)
   conclusion<- paste("Conclusion:", "Todo(a)", menor, "es", mayor)
   print(primera)
   print(segunda)
   print(conclusion)
   invisible(list(primera=primera, segunda=segunda, conclusion=conclusion))
  } else if(modo=="CELARENT"| modo =="EAE") {
    primera <- paste("P1:", "Ningun(a)", medio, "es", mayor)
    primera <- paste("P2:", "Todo(a)", menor, "es", medio))
    primera <- paste("Conclusion:", "Ningun(a)", menor, "es", mayor)
  } else if(modo=="DARII"| modo =="AII") {
    primera <- paste("P1:", "Todo(a)", medio, "es", mayor)
    primera <- paste("P2:", "Algun(a)", menor, "es", medio)
    primera <- paste("Conclusion:", "Algun(a)", menor, "es", mayor)
  } else if(modo=="FERIO"| modo =="EIO") {
    primera <- paste("P1:", "Ningun(a)", medio, "es", mayor)
    primera <- paste("P2:", "Algun(a)", menor, "es", medio)
    primera <- paste("Conclusion:", "Algun(a)", menor, "no es", mayor)
  } else {
    print("Figura invalida del primer silogismo")
  }
}