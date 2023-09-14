library(stringr)
library(Silogismo)

# Conversión:
conversion("Hombre", "Mortal", "A")
conversion("Caballo", "Hombre", "E")
conversion("Hombre", "Griegos", "I")
conversion("Voladores", "Peces", "O")

# Oposición:
oposicion("Hombre", "Caballo", "E")

# Subordinada:
subordinacion("Mujer", "Sabia", "I")

# Contradicción:
contradiccion("Hombre", "Mortal", "A")

# Cuadro de Boecio:
Boecio("Hombre", "Griego", "I")

# Primera figura:
primer_silogismo("Hombre", "Animal", "Caballo", "EAE")

# Segunda figura:
segundo_silogismo("Hombre", "Animal", "Caballo", "EIO")

# Tercera figura:
tercer_silogismo("Hombre", "Animal", "Caballo", "AAI")

# Cuarta figura:
cuarto_silogismo("Hombre", "Animal", "Caballo", "EAO")

# Figura del silogismo:
figura_silogismo("Hombre", "Animal", "Caballo", "AOO", 2)

# Figura del silogismo:
numero_figura("Hombre", "Caballo", "Hombre", "Animal", "Hombre")

# Forma de la prueba directa:
forma_axiomatica("IAI",4)

# Forma resultante en reducción al absurdo:
forma_reduccion_absurdo("OAO",3)

# ¿El silogismo tiene reducción al absurdo?
absurdo("AB", "AC", "AAI")

absurdo("AB", "AC", "AOI")
