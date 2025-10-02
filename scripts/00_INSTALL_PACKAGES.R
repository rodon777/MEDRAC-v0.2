# Lista completa de librerías usadas en el proyecto
libs <- c(
  "bezier",         # curvas Bezier
  "plotrix",        # gráficos base
  "scatterplot3d",  # gráficos 3D tradicionales
  "tcltk",          # paneles interactivos básicos
  "stats",          # funciones estadísticas básicas (ya viene con R)
  "pracma",          # integración / reemplazo de auc()
  "digest"        	# para funciones de hash si se usan en la GUI
)

# Función para instalar librerías si no están presentes
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Instalando ", pkg, "...")
    install.packages(pkg, dependencies = TRUE)
  } else {
    message(pkg, " ya está instalada.")
  }
}

# Instalar librerías faltantes
lapply(libs, install_if_missing)

# Cargar todas las librerías
lapply(libs, function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))
})
