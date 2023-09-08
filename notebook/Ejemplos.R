library(Silogismo)
lsf.str("package:Silogismo")

# Regla de conversión

conversion("Hombre","Caballo","E")
conversion("Pájaro","Volador","I")

# Regla de oposición
oposicion("Hombre","Caballo","E")
oposicion("Pájaro","Volador","I")

# Regla de subordinación
subordinacion("Hombre","Caballo","E")
subordinacion("Pájaro","Volador","I")

# Regla de contradicción
contradiccion("Hombre","Caballo","E")
contradiccion("Hombre","Caballo","A")
contradiccion("Pájaro","Volador","I")
contradiccion("Pájaro","Volador","O")

# Forma axiomática
forma_axiomatica("DATISI", 3)
forma_axiomatica("BAROCO", 2)

# Forma reducción al absurdo
forma_reduccion_absurdo("DATISI", 3)
forma_reduccion_absurdo("BAROCO", 2)

# Expresión silogística
figura_silogismo("Hombre", "Humano", "Piedra", "CAMESTRES", 2)
figura_silogismo("Hombre", "Humano", "Piedra", "AAA", 3)


