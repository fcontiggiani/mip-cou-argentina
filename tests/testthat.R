# -----------------------------------------------------------------------------
# Ejecuta todas las pruebas: Rscript tests/testthat.R (desde la raíz del proyecto)
# -----------------------------------------------------------------------------
library(testthat)                                    # Carga el paquete de pruebas
test_dir(file.path("tests", "testthat"),             # Ejecuta las pruebas de la carpeta tests/testthat...
         reporter = "summary")                       # ...con un informe resumido
