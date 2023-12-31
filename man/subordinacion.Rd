% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Silogismo.R
\name{subordinacion}
\alias{subordinacion}
\title{Subordinación}
\usage{
subordinacion(primer_termino, segundo_termino, tipo_proposicion)
}
\arguments{
\item{primer_termino}{Corresponde al sujeto de la proposición original entre comillas.}

\item{segundo_termino}{Corresponde al predicado de la proposición original entre comillas.}

\item{tipo_proposicion}{A, E, I, O, según la notación de las proposiciones.}
}
\value{
La proposición original y su subordinada.
}
\description{
Devuelve la proposición original y su subordinada para los términos y el tipo de proposición específicados.
}
\examples{
subordinacion1 <- subordinacion("Hombre", "Humano", "O");
subordinacion2 <- subordinacion("Caballo", "Animal", "Universal Afirmativa");
}
