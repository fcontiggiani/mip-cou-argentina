# -----------------------------------------------------------------------------
# Carga de las funciones del proyecto para las pruebas
# -----------------------------------------------------------------------------
raiz <- normalizePath(file.path(testthat::test_path(), "..", "..")) # Ubica la carpeta raíz del proyecto
archivos <- sort(list.files(file.path(raiz, "R"), pattern = "\\.R$", full.names = TRUE)) # Lista los scripts de R/
for (f in archivos) source(f, encoding = "UTF-8")    # Carga cada script de funciones
