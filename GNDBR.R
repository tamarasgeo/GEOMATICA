# Autor: Tamara Sepúlveda
# Año: 2025
# Licencia: MIT License
# Descripción: Cálculo de NDBR

#INSTALA LIBRERÍAS
install.packages("terra")
install.packages("dplyr")
install.packages("readr")

#LIBRERÍAS
library(terra)
library(dplyr)
library(readr)

#FUNCIONES
calcular_nbr <- function(b5, b7) {
  (b5 - b7) / (b5 + b7)
}

#FLUJO PRINCIPAL
procesar_dnbr <- function(pre_dir, post_dir, output_dir, nombre_salida) {
  
  #LEE BANDAS
  pre_b5 <- rast(list.files(pre_dir, pattern = "B5.*\\.TIF$", full.names = TRUE))
  pre_b7 <- rast(list.files(pre_dir, pattern = "B7.*\\.TIF$", full.names = TRUE))
  
  post_b5 <- rast(list.files(post_dir, pattern = "B5.*\\.TIF$", full.names = TRUE))
  post_b7 <- rast(list.files(post_dir, pattern = "B7.*\\.TIF$", full.names = TRUE))
  
  #REPROYECTA
  crs_utm <- "EPSG:32719"
  pre_b5 <- project(pre_b5, crs_utm)
  pre_b7 <- project(pre_b7, crs_utm)
  
  post_b5 <- project(post_b5, crs_utm)
  post_b7 <- project(post_b7, crs_utm)
  
  #REMUESTREA
  post_b5 <- resample(post_b5, pre_b5, method = "bilinear")
  post_b7 <- resample(post_b7, pre_b5, method = "bilinear")
  
  #CALCULA NBR PRE Y POST INCENDIO
  nbr_pre <- calcular_nbr(pre_b5, pre_b7)
  nbr_post <- calcular_nbr(post_b5, post_b7)
  
  #CALCULA NBR
  dnbr <- nbr_pre - nbr_post
  
  #CLASIFICA SEVERIDAD (según USGS)
  clases <- classify(dnbr, 
                     rcl = matrix(c(
                       -Inf, -0.1, 0,       # Sin cambio
                       -0.1, 0.1, 1,         # Baja severidad o regeneración
                       0.1, 0.27, 2,         # Baja severidad
                       0.27, 0.44, 3,        # Moderada severidad
                       0.44, 0.66, 4,        # Alta severidad
                       0.66, Inf, 5          # Severidad extrema
                     ), ncol = 3, byrow = TRUE))
  
  #ÁREA POR CLASE
  tam_pixel_ha <- res(dnbr)[1] * res(dnbr)[2] / 10000
  tabla <- freq(clases) %>%
    as.data.frame() %>%
    mutate(Superficie_ha = count * tam_pixel_ha) %>%
    select(Clase = value, Superficie_ha)

  #EVITA LA NOTACIÓN CIENTÍFICA Y SACA BIEN LOS CÁLCULOS EN EL CSV
  tabla <- freq(clases) %>%
    as.data.frame() %>%
    mutate(Superficie_ha = count * tam_pixel_ha) %>%
    mutate(Superficie_ha = format(Superficie_ha, scientific = FALSE, big.mark = ",")) %>%
    select(Clase = value, Superficie_ha)
  
  #GUARDA RESULTADOS
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  writeRaster(dnbr, file.path(output_dir, paste0(nombre_salida, "_dNBR.tif")), overwrite = TRUE)
  writeRaster(clases, file.path(output_dir, paste0(nombre_salida, "_clases_severidad.tif")), overwrite = TRUE)
  write_csv(tabla, file.path(output_dir, paste0(nombre_salida, "_estadisticas.csv")))
  
  message("OK", output_dir)
}

# RUTAS
ruta_pre_incendio <- "C:/GEO/EJEMPLO_INCENDIO/PRE"
ruta_post_incendio <- "C:/GEO/EJEMPLO_INCENDIO/POST"
nombre_base <- "DNBR"

# EJECUTAR
procesar_dnbr(ruta_pre_incendio, ruta_post_incendio, ruta_salida, nombre_base)
