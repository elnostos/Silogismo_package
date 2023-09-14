#' Primera figura del silogismo
#'
#'@import stringr
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
    print(paste("P1:", "Todo(a)", medio, "es", mayor)) 
    print(paste("P2:", "Todo(a)", menor, "es", medio) )
    print(paste("Conclusion:", "Todo(a)", menor, "es", mayor))
  } else if(modo=="CELARENT"| modo =="EAE") {
    print(paste("P1:", "Ningun(a)", medio, "es", mayor))
    print(paste("P2:", "Todo(a)", menor, "es", medio))
    print(paste("Conclusion:", "Ningun(a)", menor, "es", mayor))  
  } else if(modo=="DARII"| modo =="AII") {
    print(paste("P1:", "Todo(a)", medio, "es", mayor))
    print(paste("P2:", "Algun(a)", menor, "es", medio))
    print(paste("Conclusion:", "Algun(a)", menor, "es", mayor))
  } else if(modo=="FERIO"| modo =="EIO") {
    print(paste("P1:", "Ningun(a)", medio, "es", mayor))
    print(paste("P2:", "Algun(a)", menor, "es", medio))
    print(paste("Conclusion:", "Algun(a)", menor, "no es", mayor))
  } else {
    print("Forma invalida de la primera figura del silogismo")
  }
}

#' Segunda figura del silogismo
#'
#' @import stringr
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
    print(paste("P1:", "Ningun(a)", mayor, "es", medio)) 
    print(paste("P2:", "Todo(a)", menor, "es", medio)) 
    print(paste("Conclusion:", "Ningun(a)", menor, "es", mayor))
  } else if(modo=="CAMESTRES"| modo =="AEE") {
    print(paste("P1:", "Todo(a)", mayor, "es", medio)) 
    print(paste("P2:", "Ningun(a)", menor, "es", medio)) 
    print(paste("Conclusion:", "Ningun(a)", menor, "es", mayor))  
  } else if(modo=="FESTINO"| modo =="EIO") {
    print(paste("P1:", "Ningun(a)", mayor, "es", medio)) 
    print(paste("P2:", "Algun(a)", menor, "es", medio)) 
    print(paste("Conclusion:", "Algun(a)", menor, "no es", mayor))
  } else if(modo=="BAROCO"| modo =="AOO") {
    print(paste("P1:", "Todo(a)", mayor, "es", medio)) 
    print(paste("P2:", "Algun(a)", menor, "no es", medio)) 
    print(paste("Conclusion:", "Algun(a)", menor, "no es", mayor))
  } else {
    print("Forma invalida de la segunda figura del silogismo")
  }
}

#' Tercera figura del silogismo
#'
#' @import stringr
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
    print(paste("P1:", "Todo(a)", medio, "es", mayor)) 
    print(paste("P2:", "Algun(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Algun(a)", menor, "es", mayor))
  } else if(modo=="DISAMIS"| modo =="IAI") {
    print(paste("P1:", "Algun(a)", medio, "es", mayor)) 
    print(paste("P2:", "Todo(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Algun(a)", menor, "es", mayor))  
  } else if(modo=="FERISON"| modo =="EIO") {
    print(paste("P1:", "Ningun(a)", medio, "es", mayor)) 
    print(paste("P2:", "Algun(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Algun(a)", menor, "no es", mayor))
  } else if(modo=="BOCARDO"| modo =="OAO") {
    print(paste("P1:", "Algun(a)", medio, "no es", mayor)) 
    print(paste("P2:", "Todo(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Algun(a)", menor, "no es", mayor))
  } else if(modo=="DARAPTI"| modo =="AAI") {
    print(paste("P1:", "Todo(a)", medio, "es", mayor)) 
    print(paste("P2:", "Todo(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Algun(a)", menor, "es", mayor))
  } else if(modo=="FELAPTON"| modo =="EAO") {
    print(paste("P1:", "Ningun(a)", medio, "es", mayor)) 
    print(paste("P2:", "Todo(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Algun(a)", menor, "no es", mayor))
  } else {
    print("Forma invalida de la tercera figura del silogismo")
  }
}

#' Cuarta figura del silogismo
#'
#' @import stringr
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
    print(paste("P1:", "Todo(a)", mayor, "es", medio)) 
    print(paste("P2:", "Todo(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Algun(a)", menor, "es", mayor))
  } else if(modo=="FESAPO"| modo =="EAO") {
    print(paste("P1:", "Ningun(a)", mayor, "es", medio)) 
    print(paste("P2:", "Todo(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Algun(a)", menor, "no es", mayor))  
  } else if(modo=="CAMENES"| modo =="AEE") {
    print(paste("P1:", "Todo(a)", mayor, "es", medio)) 
    print(paste("P2:", "Ningun(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Ningun(a)", menor, "es", mayor))
  } else if(modo=="DIMATIS"| modo =="IAI") {
    print(paste("P1:", "Algun(a)", mayor, "es", medio)) 
    print(paste("P2:", "Todo(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Algun(a)", menor, "es", mayor))
  } else if(modo=="FRESISON"| modo =="EIO") {
    print(paste("P1:", "Ningun(a)", mayor, "es", medio)) 
    print(paste("P2:", "Algun(a)", medio, "es",
                menor)) 
    print(paste("Conclusion:", "Algun(a)", menor, "no es", mayor))
  } else {
    print("Forma invalida de la cuarta figura del silogismo")
  }
}

#' Figura Silogismo
#'
#' @import stringr
#' Si el modo es válido, devuelve la figura indicada del silogismo a partir de los términos definidos.
#' Si el modo es inválido, devuelve una advertencia. 
#' @param mayor El término mayor entre comillas.
#' @param medio El término medio entre comillas.
#' @param menor El término menor entre comillas.
#' @param modo El modo del silogismo entre comillas.
#' @param figura La figura del silogismo entre comillas o en valor númerico.
#' @return La figura del silogismo según el modo indicado, en caso de ser válido.
#' @examples 
#' silogismo1 <- figura_silogismo("Hombre", "Animal", "Piedra", "CAMENES", "Cuarto");
#' silogismo2 <- figura_silogismo("Hombre", "Animal", "Piedra", "AAA", 1);
#' @export

figura_silogismo <- function(mayor, medio, menor, modo, figura) {
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

#' Forma axiomática
#'
#' @import stringr
#' Si el modo es válido, devuelve la forma axiomática de la primera figura del silogismo. 
#' @param modo El modo del silogismo entre comillas.
#' @param figura La figura del silogismo entre comillas o en valor númerico.
#' @return La figura axiomática del primer silogismo para el modo y figura especificados, en caso de ser válidos.
#' @examples 
#' forma_primer_silogismo1 <- forma_axiomatica("BAROCO", "Segunda");
#' forma_primer_silogismo2 <- forma_axiomatica("IAI", "Cuarta");
#' @export

forma_axiomatica <- function(modo, figura) {
  if(modo=="CESARE" & (figura!=2)){
    print("CESARE no es una forma de la figura especificada")
  } else  if(modo=="CESARE" | (modo=="EAE" & (figura=="Segunda"|figura==2))){
    print("CELARENT")
  } else if(modo=="CAMESTRES" & (figura!=2)){
    print("CAMESTRES no es una forma de la figura especificada")
  } else  if(modo=="CAMESTRES" | (modo=="AEE" & (figura=="Segunda"|figura==2))){
    print("CELARENT")
  } else if(modo=="FESTINO" & (figura!=2)){
    print("FESTINO no es una forma de la figura especificada")
  } else  if(modo=="FESTINO" | (modo=="EIO" & (figura=="Segunda"|figura==2))){
    print("FERIO")
  } else if(modo=="BAROCO" & (figura!=2)){
    print("BAROCO no es una forma de la figura especificada")
  } else  if(modo=="BAROCO" | (modo=="AOO" & (figura=="Segunda"|figura==2))){
    print("BAROCO debe ser resuelta por reducción al absurdo")
  } else if(modo=="DATISI" & (figura!=3)){
    print("DATISI no es una forma de la figura especificada")
  } else  if(modo=="DATISI" | (modo=="AII" & (figura=="Tercera"|figura==3))){
    print("DARII")
  } else if(modo=="DISAMIS" & (figura!=3)){
    print("DISAMIS no es una forma de la figura especificada")
  } else  if(modo=="DISAMIS" | (modo=="IAI" & (figura=="Tercera"|figura==3))){
    print("DARII")
  } else if(modo=="FERISON" & (figura!=3)){
    print("FERISON no es una forma de la figura especificada")
  } else  if(modo=="FERISON" | (modo=="EIO" & (figura=="Tercera"|figura==3))){
    print("FERIO")
  } else if(modo=="BOCARDO" & (figura!=3)){
    print("BOCARDO no es una forma de la figura especificada")
  } else  if(modo=="BOCARDO" | (modo=="OAO" & (figura=="Tercera"|figura==3))){
    print("BOCARDO debe ser resuelta por reducción al absurdo")
  } else if(modo=="DARAPTI" & (figura!=3)){
    print("DARAPTI no es una forma de la figura especificada")
  } else  if(modo=="DARAPTI" | (modo=="AAI" & (figura=="Tercera"|figura==3))){
    print("DARII")
  } else if(modo=="FELAPTON" & (figura!=3)){
    print("FELAPTON no es una forma de la figura especificada")
  } else  if(modo=="FELAPTON" | (modo=="EAO" & (figura=="Tercera"|figura==3))){
    print("FERIO")
  } else if(modo=="BAMALIP" & (figura!=4)){
    print("BAMALIP no es una forma de la figura especificada")
  } else  if(modo=="BAMALIP" | (modo=="AAI" & (figura=="Cuarta"|figura==4))){
    print("BARBARA")
  } else if(modo=="FESAPO" & (figura!=4)){
    print("FESAPO no es una forma de la figura especificada")
  } else  if(modo=="FESAPO" | (modo=="EAO" & (figura=="Cuarta"|figura==4))){
    print("FERIO")
  } else if(modo=="CAMENES" & (figura!=4)){
    print("CAMENES no es una forma de la figura especificada")
  } else  if(modo=="CAMENES" | (modo=="AEE" & (figura=="Cuarta"|figura==4))){
    print("CELARENT")
  } else if(modo=="DIMATIS" & (figura!=4)){
    print("DIMATIS no es una forma de la figura especificada")
  } else  if(modo=="DIMATIS" | (modo=="IAI" & (figura=="Cuarta"|figura==4))){
    print("DARII")
  } else if(modo=="FRESISON" & (figura!=4)){
    print("FRESISON no es una forma de la figura especificada")
  } else  if(modo=="FRESISON" | (modo=="EIO" & (figura=="Cuarta"|figura==4))){
    print("FERIO")
  } else {
    print("Ingrese un modo o forma válida")
  }
}

#' Forma axiomática para reducción al absurdo
#'
#' @import stringr
#' Si el modo es válido, devuelve la forma axiomática del primer silogismo para resolver por reducción al absurdo. 
#' @param modo El modo del silogismo entre comillas.
#' @param figura La figura del silogismo entre comillas o en valor númerico.
#' @return La forma axiomática del primer silogismo para resolver por reducción al absurdo.
#' @examples 
#' forma_absurdo_silogismo1 <- forma_reduccion_absurdo("BAROCO", "Segunda");
#' forma_absurdo_silogismo2 <- forma_reduccion_absurdo("IAI", "Cuarta");
#' @export

forma_reduccion_absurdo <- function(modo, figura) {
  if(modo=="CESARE" & (figura!=2)){
    print("CESARE no es una forma de la figura especificada")
  } else  if(modo=="CESARE" | (modo=="EAE" & (figura=="Segunda"|figura==2))){
    print("FERIO")
  } else if(modo=="CAMESTRES" & (figura!=2)){
    print("CAMESTRES no es una forma de la figura especificada")
  } else  if(modo=="CAMESTRES" | (modo=="AEE" & (figura=="Segunda"|figura==2))){
    print("DARII")
  } else if(modo=="FESTINO" & (figura!=2)){
    print("FESTINO no es una forma de la figura especificada")
  } else  if(modo=="FESTINO" | (modo=="EIO" & (figura=="Segunda"|figura==2))){
    print("CELARENT")
  } else if(modo=="BAROCO" & (figura!=2)){
    print("BAROCO no es una forma de la figura especificada")
  } else  if(modo=="BAROCO" | (modo=="AOO" & (figura=="Segunda"|figura==2))){
    print("BARBARA")
  } else if(modo=="DATISI" & (figura!=3)){
    print("DATISI no es una forma de la figura especificada")
  } else  if(modo=="DATISI" | (modo=="AII" & (figura=="Tercera"|figura==3))){
    print("FERIO")
  } else if(modo=="DISAMIS" & (figura!=3)){
    print("DISAMIS no es una forma de la figura especificada")
  } else  if(modo=="DISAMIS" | (modo=="IAI" & (figura=="Tercera"|figura==3))){
    print("CELARENT")
  } else if(modo=="FERISON" & (figura!=3)){
    print("FERISON no es una forma de la figura especificada")
  } else  if(modo=="FERISON" | (modo=="EIO" & (figura=="Tercera"|figura==3))){
    print("DARII")
  } else if(modo=="BOCARDO" & (figura!=3)){
    print("BOCARDO no es una forma de la figura especificada")
  } else  if(modo=="BOCARDO" | (modo=="OAO" & (figura=="Tercera"|figura==3))){
    print("BARBARA")
  } else if(modo=="DARAPTI" & (figura!=3)){
    print("DARAPTI no es una forma de la figura especificada")
  } else  if(modo=="DARAPTI" | (modo=="AAI" & (figura=="Tercera"|figura==3))){
    print("CELARENT")
  } else if(modo=="FELAPTON" & (figura!=3)){
    print("FELAPTON no es una forma de la figura especificada")
  } else  if(modo=="FELAPTON" | (modo=="EAO" & (figura=="Tercera"|figura==3))){
    print("BARBARA")
  } else if(modo=="BAMALIP" & (figura!=4)){
    print("BAMALIP no es una forma de la figura especificada")
  } else  if(modo=="BAMALIP" | (modo=="AAI" & (figura=="Cuarta"|figura==4))){
    print("CELARENT")
  } else if(modo=="FESAPO" & (figura!=4)){
    print("FESAPO no es una forma de la figura especificada")
  } else  if(modo=="FESAPO" | (modo=="EAO" & (figura=="Cuarta"|figura==4))){
    print("BARBARA o CELARENT")
  } else if(modo=="CAMENES" & (figura!=4)){
    print("CAMENES no es una forma de la figura especificada")
  } else  if(modo=="CAMENES" | (modo=="AEE" & (figura=="Cuarta"|figura==4))){
    print("DARII")
  } else if(modo=="DIMATIS" & (figura!=4)){
    print("DIMATIS no es una forma de la figura especificada")
  } else  if(modo=="DIMATIS" | (modo=="IAI" & (figura=="Cuarta"|figura==4))){
    print("CELARENT")
  } else if(modo=="FRESISON" & (figura!=4)){
    print("FRESISON no es una forma de la figura especificada")
  } else  if(modo=="FRESISON" | (modo=="EIO" & (figura=="Cuarta"|figura==4))){
    print("CELARENT o DARII")
  } else {
    print("Ingrese un modo o figura valida")
  }
}

#' Regla de conversión
#'
#' @import stringr
#' Devuelve la proposición original y su conversa para los términos y el tipo de proposición específicados. 
#' @param primer_termino Corresponde al sujeto de la proposición original entre comillas.
#' @param segundo_termino Corresponde al predicado de la proposición original entre comillas.
#' @param tipo_proposicion A, E, I, O, según la notación de las proposiciones.
#' @return La proposición original y su conversa.
#' @examples 
#' conversion1 <- conversion("Hombre", "Humano", "O");
#' conversion2 <- conversion("Caballo", "Animal", "Universal Afirmativa");
#' @export

conversion <- function(primer_termino, segundo_termino, tipo_proposicion){
  if(tipo_proposicion=="A"|tipo_proposicion=="Universal afirmativa"){
    print(paste("Original:", "Todo(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Conversion:", "Algun(a)", segundo_termino, "es", primer_termino))
  }else if(tipo_proposicion=="E"|tipo_proposicion=="Universal negativa"){
    print(paste("Original:", "Ningun(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Conversion:", "Ningun(a)", segundo_termino, "es", primer_termino))
  }else if(tipo_proposicion=="I"|tipo_proposicion=="Particular afirmativa"){
    print(paste("Original:", "Algun(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Conversion:", "Algun(a)", segundo_termino, "es", primer_termino))
  }else if(tipo_proposicion=="O"|tipo_proposicion=="Particular negativa"){
    print(paste("Original:", "Algun(a)", primer_termino, "no es", segundo_termino)) 
    print(paste(
                "Conversion:", "No puede realizarse conversion"))
  }else{
    print("Ingrese valores validos")
  }
}

#' Contradicción
#'
#' @import stringr
#' Devuelve la proposición original y su contradicción para los términos y el tipo de proposición específicados. 
#' @param primer_termino Corresponde al sujeto de la proposición original entre comillas.
#' @param segundo_termino Corresponde al predicado de la proposición original entre comillas.
#' @param tipo_proposicion A, E, I, O, según la notación de las proposiciones.
#' @return La proposición original y su contradictoria.
#' @examples 
#' contradiccion1 <- contradiccion("Hombre", "Humano", "O");
#' contradiccion2 <- contradiccion("Caballo", "Animal", "Universal Afirmativa");
#' @export

contradiccion <- function(primer_termino, segundo_termino, tipo_proposicion){
  if(tipo_proposicion=="A"|tipo_proposicion=="Universal afirmativa"){
    print(paste("Original:", "Todo(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Contradictoria:", "Algun(a)", primer_termino, "no es", segundo_termino))
  }else if(tipo_proposicion=="E"|tipo_proposicion=="Universal negativa"){
    print(paste("Original:", "Ningun(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Contradictoria:", "Algun(a)", primer_termino, "es", segundo_termino))
  }else if(tipo_proposicion=="I"|tipo_proposicion=="Particular afirmativa"){
    print(paste("Original:", "Algun(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Contradictoria:", "Ningun(a)", primer_termino, "es", segundo_termino))
  }else if(tipo_proposicion=="O"|tipo_proposicion=="Particular negativa"){
    print(paste("Original:", "Algun(a)", primer_termino, "no es", segundo_termino)) 
    print(paste(
                "Contradictoria:", "Todo(a)", primer_termino, "es", segundo_termino))
  }else{
    print("Ingrese valores validos")
  }
}

#' Subordinación
#'
#' @import stringr
#' Devuelve la proposición original y su subordinada para los términos y el tipo de proposición específicados. 
#' @param primer_termino Corresponde al sujeto de la proposición original entre comillas.
#' @param segundo_termino Corresponde al predicado de la proposición original entre comillas.
#' @param tipo_proposicion A, E, I, O, según la notación de las proposiciones.
#' @return La proposición original y su subordinada.
#' @examples 
#' subordinacion1 <- subordinacion("Hombre", "Humano", "O");
#' subordinacion2 <- subordinacion("Caballo", "Animal", "Universal Afirmativa");
#' @export

subordinacion <- function(primer_termino, segundo_termino, tipo_proposicion){
  if(tipo_proposicion=="A"|tipo_proposicion=="Universal afirmativa"){
    print(paste("Original:", "Todo(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Subordinada:", "Algun(a)", primer_termino, "es", segundo_termino))
  }else if(tipo_proposicion=="E"|tipo_proposicion=="Universal negativa"){
    print(paste("Original:", "Ningun(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Subordinada:", "Algun(a)", primer_termino, "no es", segundo_termino))
  }else if(tipo_proposicion=="I"|tipo_proposicion=="Particular afirmativa"){
    print(paste("Original:", "Algun(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Subordinada:", "Todo(a)", primer_termino, "es", segundo_termino))
  }else if(tipo_proposicion=="O"|tipo_proposicion=="Particular negativa"){
    print(paste("Original:", "Algun(a)", primer_termino, "no es", segundo_termino)) 
    print(paste(
                "Subordinada:", "Ningun(a)", primer_termino, "es", segundo_termino))
  }else{
    print("Ingrese valores validos")
  }
}

#' Oposición
#'
#' @import stringr
#' Devuelve la proposición original y su opuesta para los términos y el tipo de proposición específicados. 
#' @param primer_termino Corresponde al sujeto de la proposición original entre comillas.
#' @param segundo_termino Corresponde al predicado de la proposición original entre comillas.
#' @param tipo_proposicion A, E, I, O, según la notación de las proposiciones.
#' @return La proposición original y su opuesta.
#' @examples 
#' oposicion1 <- oposicion("Hombre", "Humano", "O");
#' oposicion2 <- oposicion("Caballo", "Animal", "Universal Afirmativa");
#' @export

oposicion <- function(primer_termino, segundo_termino, tipo_proposicion){
  if(tipo_proposicion=="A"|tipo_proposicion=="Universal afirmativa"){
    print(paste("Original:", "Todo(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Opuesta:", "Ningun(a)", primer_termino, "es", segundo_termino))
  }else if(tipo_proposicion=="E"|tipo_proposicion=="Universal negativa"){
    print(paste("Original:", "Ningun(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Opuesta:", "Todo(a)", primer_termino, "es", segundo_termino))
  }else if(tipo_proposicion=="I"|tipo_proposicion=="Particular afirmativa"){
    print(paste("Original:", "Algun(a)", primer_termino, "es", segundo_termino)) 
    print(paste(
                "Opuesta:", "Algun(a)", primer_termino, "no es", segundo_termino))
  }else if(tipo_proposicion=="O"|tipo_proposicion=="Particular negativa"){
    print(paste("Original:", "Algun(a)", primer_termino, "no es", segundo_termino)) 
    print(paste(
                "Opuesta:", "Algun(a)", primer_termino, "es", segundo_termino))
  }else{
    print("Ingrese valores validos")
  }
}

#' Cuadro de Boecio o de oposición de los juicios
#'
#' @import stringr
#' Devuelve la proposición original, su opuesta, subordinada y contradictoria para los términos y el tipo de proposición específicados. 
#' @param primer_termino Corresponde al sujeto de la proposición original entre comillas.
#' @param segundo_termino Corresponde al predicado de la proposición original entre comillas.
#' @param tipo_proposicion A, E, I, O, según la notación de las proposiciones.
#' @return La proposición original, su opuesta, subordinada y contradictoria.
#' @examples 
#' Boecio1 <- Boecio("Hombre", "Humano", "O");
#' Boecio2 <- Boecio("Caballo", "Animal", "Universal Afirmativa");
#' @export

Boecio<-function(primer_termino, segundo_termino, tipo_proposicion){
  if(tipo_proposicion=="A"|tipo_proposicion=="Universal afirmativa"){
    subordinacion(primer_termino, segundo_termino, tipo_proposicion)
    print(paste("Opuesta:", "Ningun(a)", primer_termino, "es", segundo_termino))
    print(paste("Contradictoria:", "Algun(a)", primer_termino, "no es", segundo_termino))
  } else if(tipo_proposicion=="E"|tipo_proposicion=="Universal negativa"){
    subordinacion(primer_termino, segundo_termino, tipo_proposicion)
    print(paste("Opuesta:", "Todo(a)", primer_termino, "es", segundo_termino))
    print(paste("Contradictoria:", "Algun(a)", primer_termino, "es", segundo_termino))
  } else if(tipo_proposicion=="I"|tipo_proposicion=="Particular afirmativa"){
    subordinacion(primer_termino, segundo_termino, tipo_proposicion)
    print(paste("Opuesta:", "Algun(a)", primer_termino, "no es", segundo_termino))
    print(paste("Contradictoria:", "Ningun(a)", primer_termino, "es", segundo_termino))
  } else if(tipo_proposicion=="O"|tipo_proposicion=="Particular negativa"){
    subordinacion(primer_termino, segundo_termino, tipo_proposicion)
    print(paste("Opuesta:", "Algun(a)", primer_termino, "es", segundo_termino))
    print(paste("Contradictoria:", "Todo(a)", primer_termino, "es", segundo_termino))
  } else {
    print("Ingrese valores válidos")
  }
}

#' Figura Silogismo
#'
#' @import stringr
#' A partir de la definicion de los sujetos y predicados de las premisas, asi como del termino medio, devuelve la figura del silogismo correspondiente. 
#' @param predicado1 Corresponde al predicado de la primera premisa.
#' @param sujeto1 Corresponde al sujeto de la primera premisa.
#' @param predicado2 Corresponde al predicado de la segunda premisa.
#' @param sujeto2 Corresponde al sujeto de la segunda premisa.
#' @param termino_medio Corresponde al termino medio del silogismo.
#' @examples 
#' figura1 <- numero_figura("Hombre", "Caballo", "Animal", "Caballo", "Caballo");
#' figura2 <- numero_figura("Hombre", "Caballo", "Caballo", "Animal", "Caballo");
#' @export

numero_figura<-function(sujeto1, predicado1, sujeto2, predicado2, termino_medio){
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


#' Reduccion al absurdo
#'
#' @import stringr
#' A partir de la forma de las primera y segunda premisa, asi como la figura del
#' silogismo dice que forma debe tener el silogismo a evaluar en reduccion al absurdo,
#' o si este no es posible resolver por dicho metodo. Todavia no realiza la demostracion
#' completa de la reduccion al absurdo 
#' @param AB Corresponde a la primera premisa.
#' @param CA Corresponde a la segunda premisa.
#' @param valor Corresponde a la forma de la figura.
#' @examples 
#' reductio1 <- absurdo("AB", "CA", "AAA");
#' reductio2 <- absurdo("BA", "CA", "EAE");
#' @export

absurdo<-function(AB, CA, valor){
  
    if(str_sub(AB,1,1)==str_sub(CA,2,2)){
      formis<-1
    } else if(str_sub(AB,2,2)==str_sub(CA,2,2)){
      formis<-2
    } else if(str_sub(AB,1,1)==str_sub(CA,1,1)){
      formis<-3
    } else if(str_sub(AB,2,2)==str_sub(CA,1,1)){
      formis<-4
    } else{
      print("Ingrese valores válidos")
    }
  
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
