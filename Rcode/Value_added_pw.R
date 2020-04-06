# Script for calculating Value added per woker, based on Olley Pakes disaggregation.

# Industry Productivity ia a weighted average of plant-level productivity, with shares of
# industry output as weight.

# First, the value added variable to use must be constructed using variables asked in the questionnaires,
# the value calculates by INEGI will not be used. Theree will be two differnt definitions for value added:
#   1.- Sales -Delta(Inventory(..In process))- Delta(Inventory(ProductosElaborados))- Intermidiates*
#   2.- ""-Electricity - Fuels -Packaging
#   * Intermidiate= Intermidiate Material**+Products to be resale.
#   ** Intermidiate MAterial= Intermidiate Material-Change in Inventory
# 
# Once computed you only have to divide by the # of employees.It could be " Personal Remunerado", "Personal 
# Ocupado" or " Total de personal dependiente de la razón social".

Materias_primas=+k020a-(Invenntario)
Materias_primas=k020a-(p100c+p200c+p230c) # Mercancias para su reventa, Materias primas y auxiliares,Materiales(Se pueden agregar)



Combustibles= Combustible- Inventario # (k421a ,k431a)Manuf, k411a (Básico)
Combustibles= k411a-p900c # Otros bienes, No hay específico para combustibles.

Intermidiate= Materias_primas+k010

# No tengo las variables de existencias para 2014 que necesito para construir materias primas, combustible e 
# intermedios.

sales=m010a+m030a+m020 #m100a+m310a+m200a
va1=sales-(p330c+p030c)-Intermidiate #sales-(p330c+p340c)-Intermidiate
va2=sales-(p330c+p340c)-Intermidiate-Combustible-k412a-k910a

# bindscatter or lowes Frontiuer 

# 