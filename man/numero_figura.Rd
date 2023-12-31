% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Silogismo.R
\name{numero_figura}
\alias{numero_figura}
\title{Figura Silogismo}
\usage{
numero_figura(sujeto1, predicado1, sujeto2, predicado2, termino_medio)
}
\arguments{
\item{sujeto1}{Corresponde al sujeto de la primera premisa.}

\item{predicado1}{Corresponde al predicado de la primera premisa.}

\item{sujeto2}{Corresponde al sujeto de la segunda premisa.}

\item{predicado2}{Corresponde al predicado de la segunda premisa.}

\item{termino_medio}{Corresponde al termino medio del silogismo.}
}
\description{
A partir de la definicion de los sujetos y predicados de las premisas, asi como del termino medio, devuelve la figura del silogismo correspondiente.
}
\examples{
figura1 <- numero_figura("Hombre", "Caballo", "Animal", "Caballo", "Caballo");
figura2 <- numero_figura("Hombre", "Caballo", "Caballo", "Animal", "Caballo");
}
