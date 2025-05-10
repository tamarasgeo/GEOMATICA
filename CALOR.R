# Autor: Tamara Sepúlveda
# Año: 2025
# Licencia: MIT License
# Descripción: Cálculo de temperatura superficial (LST) desde Banda 10 de Landsat y conversión a grados Celsius.

install.packages("terra")
library(terra)

# RUTAS
input_folder <- "_"
output_folder <- "_"

# PARÁMETROS MTL
temp_mult <- 0.00341802
temp_add <- 149.0

# ️BANDA 10
b_st <- rast(file.path(input_folder, "_.TIF"))

# KELVIN A CELSIUS
lst_kelvin <- (b_st * temp_mult) + temp_add
lst_celsius <- lst_kelvin - 273.15

# UTM 19S
lst_utm <- project(lst_celsius, "EPSG:32719")

# GUARDA
writeRaster(lst_utm, file.path(output_folder, "LST.tif"), overwrite = TRUE)
