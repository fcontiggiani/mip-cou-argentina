# =============================================================================
# 02_leer_cou_indec.R
# Descarga, inspección y lectura del COU completo publicado por el INDEC.
#
# Archivo del COU 2018 (107 ramas CIIU x 223 productos CPC), según INDEC (2021):
#   https://www.indec.gob.ar/ftp/cuadros/economia/sh_cou_08_21.xls
# Verificar en el sitio del INDEC si hay versiones posteriores (2019-2020) y
# su nombre de archivo, que cambia con cada publicación.
#
# La disposición de hojas y celdas del libro no está documentada en el
# informe metodológico; por eso la lectura se parametriza con una
# configuración de rangos (ver `config_cou_plantilla()`), que se completa una
# vez con ayuda de `inspeccionar_cou()`.
# =============================================================================

# -----------------------------------------------------------------------------
# Descarga
# -----------------------------------------------------------------------------

#' Descarga un archivo del INDEC
#'
#' @param url URL del libro Excel.
#' @param destino Ruta local de destino.
#' @return Ruta local (invisible).
descargar_cou_indec <- function(                     # Abre la función de descarga
    url = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_cou_08_21.xls", # URL por defecto: COU 2018 completo
    destino = file.path("datos", basename(url))) {   # Destino por defecto: carpeta datos/ con el mismo nombre
  dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE) # Crea la carpeta de destino si no existe
  if (!file.exists(destino)) {                       # Descarga solo si el archivo todavía no está en disco
    utils::download.file(url, destino, mode = "wb", quiet = TRUE) # Descarga en modo binario (necesario para .xls)
  }                                                  # Cierra el condicional de descarga
  invisible(destino)                                 # Devuelve la ruta sin imprimirla
}                                                    # Cierra la función descargar_cou_indec

# -----------------------------------------------------------------------------
# Inspección del libro Excel
# -----------------------------------------------------------------------------

# Convierte un índice de columna en letras de Excel (1 -> A, 28 -> AB)
.col_excel <- function(j) {                          # Abre la función auxiliar de conversión
  s <- character(length(j))                          # Reserva un vector de texto para los resultados
  for (k in seq_along(j)) {                          # Recorre cada índice de columna recibido
    n <- j[k]                                        # Toma el índice numérico de la columna
    out <- ""                                        # Inicia el texto de la letra de columna vacío
    while (n > 0) {                                  # Repite mientras queden "dígitos" en base 26
      r <- (n - 1) %% 26                             # Calcula el resto en base 26 (0 = A, 25 = Z)
      out <- paste0(LETTERS[r + 1], out)             # Antepone la letra correspondiente
      n <- (n - 1) %/% 26                            # Pasa al siguiente "dígito" en base 26
    }                                                # Cierra el ciclo de conversión
    s[k] <- out                                      # Guarda la letra de columna obtenida
  }                                                  # Cierra el ciclo sobre los índices
  s                                                  # Devuelve el vector de letras
}                                                    # Cierra la función .col_excel

#' Inspecciona un libro del COU y ubica rótulos clave
#'
#' Lista las hojas, sus dimensiones y las celdas cuyo texto contiene alguna
#' de las palabras clave. Sirve para completar la configuración de rangos.
#'
#' @param archivo Ruta del .xls/.xlsx.
#' @param claves Vector de expresiones regulares (sin distinguir mayúsculas).
#' @return data.frame con hoja, celda y texto encontrado.
inspeccionar_cou <- function(archivo,                # Abre la función; recibe la ruta del libro
                             claves = c("oferta", "utilizaci", "importaci", "cif", # Palabras clave del cuadro de oferta
                                        "margen", "impuesto", "exportaci",          # ...márgenes, impuestos y exportaciones
                                        "consumo", "formaci", "existencias",        # ...usos finales
                                        "valor agregado", "total")) {               # ...valor agregado y totales
  hojas <- readxl::excel_sheets(archivo)             # Obtiene los nombres de todas las hojas del libro
  res <- list()                                      # Prepara una lista para acumular los hallazgos por hoja
  patron <- paste(claves, collapse = "|")            # Une las palabras clave en una sola expresión regular
  for (h in hojas) {                                 # Recorre cada hoja del libro
    m <- suppressMessages(readxl::read_excel(        # Lee la hoja completa sin mensajes de readxl:
      archivo, sheet = h,                            #   archivo y hoja a leer
      col_names = FALSE,                             #   sin usar la primera fila como encabezado
      col_types = "text",                            #   todas las celdas como texto (para buscar rótulos)
      .name_repair = "minimal"))                     #   sin modificar los nombres de columna
    m <- as.matrix(m)                                # Convierte la tabla leída en matriz de texto
    cat(sprintf("Hoja '%s': %d filas x %d columnas\n", h, nrow(m), ncol(m))) # Informa el tamaño de la hoja
    idx <- which(!is.na(m) &                         # Busca celdas no vacías...
                   grepl(patron, m, ignore.case = TRUE), # ...que contengan alguna palabra clave
                 arr.ind = TRUE)                     # Devuelve las posiciones como pares (fila, columna)
    if (nrow(idx) > 0) {                             # Si hubo coincidencias en la hoja...
      res[[h]] <- data.frame(                        # ...arma una tabla con los hallazgos:
        hoja = h,                                    #   nombre de la hoja
        celda = paste0(.col_excel(idx[, "col"]), idx[, "row"]), # referencia de celda estilo Excel (p. ej., D9)
        texto = substr(m[idx], 1, 60),               #   primeros 60 caracteres del rótulo encontrado
        stringsAsFactors = FALSE)                    #   texto como caracteres, no factores
    }                                                # Cierra el condicional de coincidencias
  }                                                  # Cierra el ciclo sobre las hojas
  out <- do.call(rbind, res)                         # Apila los hallazgos de todas las hojas
  if (!is.null(out)) rownames(out) <- NULL           # Elimina los nombres de fila heredados
  out                                                # Devuelve la tabla de ubicaciones
}                                                    # Cierra la función inspeccionar_cou

# -----------------------------------------------------------------------------
# Configuración de rangos
# -----------------------------------------------------------------------------

#' Plantilla de configuración de rangos para leer el COU
#'
#' Cada bloque se define por hoja y rango de Excel ("D9:DF231"). Los vectores
#' por producto son rangos de una columna; los vectores por rama, de una fila.
#' Los valores "COMPLETAR" deben reemplazarse por los rangos que surjan de
#' `inspeccionar_cou()`.
#'
#' @return Lista de configuración.
config_cou_plantilla <- function() {                 # Abre la función que devuelve la plantilla
  list(                                              # Arma la lista de parámetros de lectura
    archivo            = "datos/sh_cou_08_21.xls",   # Ruta del libro Excel
    unidad             = "millones de pesos corrientes de 2018", # Unidad de medida de los cuadros
    hoja_oferta        = "COMPLETAR",                # Hoja del cuadro de oferta
    productos          = "COMPLETAR",                # Rango de rótulos de productos (una columna), p. ej. "B9:B231"
    ramas              = "COMPLETAR",                # Rango de rótulos de ramas (una fila), p. ej. "D7:DF7"
    prod               = "COMPLETAR",                # Bloque de producción productos x ramas, p. ej. "D9:DF231"
    imp                = "COMPLETAR",                # Columna de importaciones CIF
    aj_cif_fob         = NA,                         # Columna de ajuste CIF/FOB (NA si no existe)
    margenes           = "COMPLETAR",                # Columna de márgenes de distribución
    impuestos          = "COMPLETAR",                # Columna de impuestos netos de subsidios
    hoja_utilizacion   = "COMPLETAR",                # Hoja del cuadro de utilización
    u_int              = "COMPLETAR",                # Bloque de consumo intermedio productos x ramas
    y                  = "COMPLETAR",                # Bloque de demanda final productos x usos finales
    usos_finales       = "COMPLETAR",                # Rótulos de los usos finales (una fila)
    col_export         = "Exportaciones",            # Nombre exacto de la columna de exportaciones
    vab                = "COMPLETAR",                # Fila del VAB a precios básicos por rama
    ajuste_territorial = NA,                         # Bloque de filas de ajuste territorial (NA si no se usa)
    imp_territorial    = 0                           # Compras de residentes en el exterior (importaciones)
  )                                                  # Cierra la lista
}                                                    # Cierra la función config_cou_plantilla

# -----------------------------------------------------------------------------
# Lectura de bloques
# -----------------------------------------------------------------------------

# Lee un bloque numérico; trata NA, "-" y celdas vacías como cero
.leer_bloque <- function(archivo, hoja, rango) {     # Abre la función auxiliar de lectura numérica
  m <- suppressMessages(readxl::read_excel(          # Lee el rango indicado sin mensajes de readxl:
    archivo, sheet = hoja, range = rango,            #   archivo, hoja y rango de celdas
    col_names = FALSE,                               #   sin encabezados
    col_types = "text",                              #   como texto, para limpiar guiones y vacíos
    .name_repair = "minimal"))                       #   sin modificar los nombres de columna
  m <- as.matrix(m)                                  # Convierte la tabla en matriz de texto
  m[is.na(m) | trimws(m) %in% c("-", "--", "")] <- "0" # Reemplaza celdas vacías y guiones por cero
  num <- suppressWarnings(matrix(                    # Convierte el texto a números en una matriz:
    as.numeric(gsub(",", ".", m)),                   #   cambia coma decimal por punto y pasa a numérico
    nrow = nrow(m)))                                 #   conserva la cantidad de filas del bloque
  if (anyNA(num)) stop("Celdas no numéricas en ", hoja, "!", rango) # Detiene si quedó algún texto no convertible
  num                                                # Devuelve la matriz numérica
}                                                    # Cierra la función .leer_bloque

# Lee un rango de rótulos y lo devuelve como vector de texto
.leer_rotulos <- function(archivo, hoja, rango) {    # Abre la función auxiliar de lectura de rótulos
  m <- suppressMessages(readxl::read_excel(          # Lee el rango sin mensajes de readxl:
    archivo, sheet = hoja, range = rango,            #   archivo, hoja y rango de celdas
    col_names = FALSE,                               #   sin encabezados
    col_types = "text",                              #   como texto
    .name_repair = "minimal"))                       #   sin modificar los nombres de columna
  trimws(as.vector(as.matrix(m)))                    # Aplana a vector y quita espacios sobrantes
}                                                    # Cierra la función .leer_rotulos

#' Lee el COU completo a partir de una configuración de rangos
#'
#' @param cfg Lista con la estructura de `config_cou_plantilla()`.
#' @return Objeto de clase "cou".
leer_cou_indec <- function(cfg) {                    # Abre la función de lectura del COU completo
  if (any(unlist(cfg) == "COMPLETAR", na.rm = TRUE)) { # Verifica que no queden campos sin completar
    stop("Complete la configuración con los rangos de `inspeccionar_cou()`.") # Detiene con un mensaje claro
  }                                                  # Cierra el control de configuración
  f <- cfg$archivo                                   # Ruta del libro
  ho <- cfg$hoja_oferta                              # Hoja del cuadro de oferta
  hu <- cfg$hoja_utilizacion                         # Hoja del cuadro de utilización
  productos <- .leer_rotulos(f, ho, cfg$productos)   # Lee los nombres de los productos
  ramas     <- .leer_rotulos(f, ho, cfg$ramas)       # Lee los nombres de las ramas
  usos      <- .leer_rotulos(f, hu, cfg$usos_finales) # Lee los nombres de los usos finales
  np <- length(productos)                            # Cuenta los productos

  prod <- .leer_bloque(f, ho, cfg$prod)              # Lee el bloque de producción (productos x ramas)
  dimnames(prod) <- list(productos, ramas)           # Rotula filas y columnas del bloque de producción
  vec_p <- function(r) {                             # Define un lector de vectores por producto:
    if (is.na(r)) rep(0, np)                         #   si el rango no existe, devuelve ceros
    else as.vector(.leer_bloque(f, ho, r))           #   si existe, lee la columna y la aplana
  }                                                  # Cierra el lector de vectores

  u_int <- .leer_bloque(f, hu, cfg$u_int)            # Lee el consumo intermedio a precios de comprador
  y <- .leer_bloque(f, hu, cfg$y)                    # Lee la demanda final a precios de comprador
  colnames(y) <- usos                                # Rotula las columnas de la demanda final
  vab <- as.vector(.leer_bloque(f, hu, cfg$vab))     # Lee el VAB por rama y lo aplana

  aj_terr <- NULL                                    # Inicia el ajuste territorial como inexistente
  if (!is.na(cfg$ajuste_territorial)) {              # Si la configuración indica un rango de ajuste...
    aj_terr <- .leer_bloque(f, hu, cfg$ajuste_territorial) # ...lee las filas de ajuste
    colnames(aj_terr) <- usos                        # ...y las rotula con los usos finales
  }                                                  # Cierra el condicional del ajuste territorial
  nuevo_cou(prod,                                    # Construye el objeto "cou" con la producción
            imp = vec_p(cfg$imp),                    # ...las importaciones CIF
            aj_cif_fob = vec_p(cfg$aj_cif_fob),      # ...el ajuste CIF/FOB
            margenes = vec_p(cfg$margenes),          # ...los márgenes
            impuestos = vec_p(cfg$impuestos),        # ...los impuestos sobre productos
            u_int = u_int, y = y, vab = vab,         # ...el cuadro de utilización y el VAB
            col_export = cfg$col_export,             # ...la columna de exportaciones
            ajuste_territorial = aj_terr,            # ...el ajuste territorial
            imp_territorial = cfg$imp_territorial,   # ...las importaciones del ajuste territorial
            unidad = cfg$unidad)                     # ...y la unidad de medida
}                                                    # Cierra la función leer_cou_indec

# -----------------------------------------------------------------------------
# Lectura automática de los COU del INDEC (formato 2018-2023)
# -----------------------------------------------------------------------------
# Estructura verificada en los archivos sh_cou_2018_08_21.xls a
# sh_cou_2023_06_26.xls:
#   - Hoja "Mat_Of_pc_AAAA" (oferta) y hoja "Mat_Ut_pc_AAAA" (utilización).
#   - Fila con "Descripción" en la columna 1: códigos de columna (ramas CIIU
#     Rev. 3 desde la columna 3; luego OPB, IMPO, Ajuste CIF/FOB, DI, IP,
#     Mg D, IVA, [CSCD desde 2019], OPC en la oferta; UI, CH, CP, EX,
#     Fbc Fijo, OV, Productos terminados, Trabajos en curso, UF, DEMANDA en
#     la utilización).
#   - Productos CPC 1.1 en las filas siguientes, hasta la fila "Total".
#   - Filas de ajuste territorial y de VAB identificadas por su rótulo.
#   - Unidad: miles de pesos (2018-2022) o millones de pesos (2023), según la
#     fila 2 de cada hoja.

# Convierte texto a número tratando vacíos como cero
.a_num <- function(x) {                              # Abre la función auxiliar de conversión
  x[is.na(x) | trimws(x) %in% c("", "-")] <- "0"     # Reemplaza celdas vacías y guiones por cero
  as.numeric(x)                                      # Convierte el texto a número y lo devuelve
}                                                    # Cierra la función .a_num

# Lee una hoja completa como matriz de texto
.hoja_texto <- function(archivo, hoja) {             # Abre la función; recibe archivo y hoja
  as.matrix(suppressMessages(readxl::read_excel(     # Lee la hoja y la convierte en matriz:
    archivo, sheet = hoja,                           #   archivo y hoja a leer
    col_names = FALSE,                               #   sin encabezados
    col_types = "text",                              #   todas las celdas como texto
    .name_repair = "minimal")))                      #   sin modificar los nombres de columna
}                                                    # Cierra la función .hoja_texto

#' Lee un COU del INDEC ubicando los bloques por sus rótulos
#'
#' @param archivo Ruta del libro .xls del INDEC.
#' @param a_millones Si TRUE, expresa todos los valores en millones de pesos.
#' @return Objeto "cou" con los campos adicionales `anio`, `codigos_ramas`
#'   y `codigos_productos`.
leer_cou_indec_auto <- function(archivo, a_millones = TRUE) { # Abre la función de lectura automática
  hojas <- readxl::excel_sheets(archivo)             # Lista las hojas del libro
  ho <- hojas[grepl("^Mat_Of", hojas)][1]            # Identifica la hoja del cuadro de oferta
  hu <- hojas[grepl("^Mat_Ut", hojas)][1]            # Identifica la hoja del cuadro de utilización
  if (is.na(ho) || is.na(hu)) stop("No se encontraron las hojas Mat_Of / Mat_Ut en ", archivo) # Controla el formato
  mo <- .hoja_texto(archivo, ho)                     # Lee la hoja de oferta como texto
  mu <- .hoja_texto(archivo, hu)                     # Lee la hoja de utilización como texto

  rotulo_unidad <- mo[2, 1]                          # Lee el rótulo de año y unidad (p. ej., "Año 2018 (miles de pesos)")
  anio <- as.integer(regmatches(rotulo_unidad,       # Extrae el año del rótulo...
                                regexpr("[0-9]{4}", rotulo_unidad))) # ...como los primeros cuatro dígitos
  en_miles <- grepl("miles", rotulo_unidad, ignore.case = TRUE) # Detecta si los valores están en miles de pesos
  factor <- if (a_millones && en_miles) 1 / 1000 else 1 # Factor para llevar todo a millones de pesos
  unidad <- if (a_millones || !en_miles) {           # Define el texto de la unidad resultante:
    paste("millones de pesos corrientes de", anio)   #   millones de pesos del año del cuadro
  } else {                                           # Si se conservan los miles...
    paste("miles de pesos corrientes de", anio)      #   miles de pesos del año del cuadro
  }                                                  # Cierra la definición de la unidad

  # --- Cuadro de oferta ---
  hro <- which(mo[, 1] %in% "Descripción")[1]        # Fila de encabezado con los códigos de columna (oferta)
  tro <- which(mo[, 1] %in% "Total")[1]              # Fila del total de productos (oferta)
  filas_o <- (hro + 1):(tro - 1)                     # Filas de productos en la oferta
  cod_o <- mo[hro, ]                                 # Códigos de columna de la oferta
  col_o <- function(cod) {                           # Define un buscador de columnas por código:
    j <- match(cod, cod_o)                           #   ubica la columna con ese código
    if (is.na(j)) stop("Falta la columna '", cod, "' en la oferta de ", archivo) # detiene si no existe
    j                                                #   devuelve el índice de columna
  }                                                  # Cierra el buscador
  ramas_o <- 3:(col_o("OPB") - 1)                    # Columnas de ramas: desde la 3 hasta antes de OPB
  num_o <- function(j) .a_num(mo[filas_o, j]) * factor # Lee una columna numérica de la oferta en la unidad final

  prod <- sapply(ramas_o, num_o)                     # Bloque de producción a precios básicos (productos x ramas)
  imp <- num_o(col_o("IMPO"))                        # Importaciones CIF por producto
  aj <- num_o(col_o("Ajuste CIF/FOB"))               # Ajuste CIF/FOB por producto
  impuestos <- num_o(col_o("DI")) +                  # Impuestos sobre productos: derechos de importación...
    num_o(col_o("IP")) +                             # ...más impuestos netos de subsidios...
    num_o(col_o("IVA"))                              # ...más IVA no deducible
  tipos_mg <- intersect(c("Mg D", "CSCD"), cod_o)    # Tipos de margen presentes (CSCD existe desde 2019)
  margenes <- sapply(tipos_mg, function(cod) num_o(col_o(cod))) # Márgenes por producto y tipo
  margenes <- matrix(margenes, nrow = length(filas_o), # Asegura que los márgenes sean una matriz...
                     dimnames = list(NULL, tipos_mg)) # ...con una columna por tipo
  opc <- num_o(col_o("OPC"))                         # Oferta total a precios de comprador (para control)

  # --- Cuadro de utilización ---
  hru <- which(mu[, 1] %in% "Descripción")[1]        # Fila de encabezado con los códigos de columna (utilización)
  tru <- which(mu[, 1] %in% "Total")[1]              # Fila del total de productos (utilización)
  filas_u <- (hru + 1):(tru - 1)                     # Filas de productos en la utilización
  cod_u <- mu[hru, ]                                 # Códigos de columna de la utilización
  stopifnot(identical(mo[filas_o, 2], mu[filas_u, 2])) # Verifica que los productos coincidan en ambas hojas
  ramas_u <- 3:(match("UI", cod_u) - 1)              # Columnas de ramas: desde la 3 hasta antes de UI
  stopifnot(length(ramas_o) == length(ramas_u),      # Verifica que ambas hojas tengan la misma cantidad de ramas...
            identical(substr(cod_o[ramas_o], 1, 3),  # ...y los mismos códigos a tres dígitos (algunos códigos
                      substr(cod_u[ramas_u], 1, 3))) #    difieren solo en el formato, p. ej., "1512" y "15120")
  num_u <- function(filas, j) .a_num(mu[filas, j]) * factor # Lee celdas numéricas de la utilización

  u_int <- sapply(ramas_u, function(j) num_u(filas_u, j)) # Consumo intermedio a precios de comprador (productos x ramas)
  cod_fin <- c("EX", "CH", "CP", "Fbc Fijo", "OV",   # Códigos de los usos finales a leer...
               "Productos terminados", "Trabajos en curso") # ...incluidas las dos variaciones de existencias
  nom_fin <- c("Exportaciones", "Consumo privado", "Consumo público", "FBCF", # Nombres legibles de los usos finales
               "Objetos valiosos", "Var. existencias (terminados)",            # ...continuación
               "Var. existencias (en curso)")                                  # ...continuación
  j_fin <- match(cod_fin, cod_u)                     # Ubica las columnas de los usos finales
  if (anyNA(j_fin)) stop("Faltan usos finales en ", archivo) # Detiene si falta alguna columna
  y <- sapply(j_fin, function(j) num_u(filas_u, j))  # Demanda final a precios de comprador (productos x usos)
  colnames(y) <- nom_fin                             # Rotula los usos finales

  f_vab <- which(mu[, 1] %in% "Valor Agregado Bruto pb")[1] # Fila del valor agregado bruto
  vab <- num_u(f_vab, ramas_u)                       # VAB a precios básicos por rama

  f_res <- which(mu[, 1] %in% "Compras de residentes en el mercado exterior")[1]     # Fila de compras de residentes afuera
  f_nores <- which(mu[, 1] %in% "Compras de no residentes en el mercado doméstico")[1] # Fila de compras de no residentes
  aj_terr <- rbind(num_u(f_res, j_fin),              # Ajuste territorial: fila 1, residentes en el exterior...
                   num_u(f_nores, j_fin))            # ...fila 2, no residentes en el país
  dimnames(aj_terr) <- list(c("Compras de residentes en el exterior", # Rotula las filas del ajuste
                              "Compras de no residentes en el país"), # ...(fila 2)
                            nom_fin)                 # Rotula las columnas con los usos finales
  f_res_o <- which(mo[, 1] %in% "Compras de residentes en el mercado exterior")[1] # Misma fila en la oferta
  imp_terr <- .a_num(mo[f_res_o, col_o("IMPO")]) * factor # Importaciones por compras de residentes en el exterior

  productos <- mo[filas_o, 1]                        # Nombres de los productos (código y descripción)
  ramas <- mo[hro - 1, ramas_o]                      # Nombres de las ramas (fila anterior al encabezado)
  dimnames(prod) <- list(productos, ramas)           # Rotula el bloque de producción

  cou <- nuevo_cou(prod, imp = imp, aj_cif_fob = aj, # Construye el objeto "cou" con la oferta...
                   margenes = margenes,              # ...los márgenes por tipo...
                   impuestos = impuestos,            # ...los impuestos sobre productos...
                   u_int = u_int, y = y, vab = vab,  # ...la utilización y el VAB...
                   col_export = "Exportaciones",     # ...la columna de exportaciones...
                   ajuste_territorial = aj_terr,     # ...el ajuste territorial...
                   imp_territorial = imp_terr,       # ...las importaciones del ajuste territorial...
                   unidad = unidad)                  # ...y la unidad de medida
  cou$anio <- anio                                   # Agrega el año del cuadro
  cou$codigos_ramas <- cod_o[ramas_o]                # Agrega los códigos CIIU de las ramas
  cou$codigos_productos <- mo[filas_o, 2]            # Agrega los códigos CPC de los productos
  cou$control_opc <- max(abs(rowSums(prod) + imp + aj + rowSums(margenes) + impuestos - opc)) # Control interno de la oferta pc
  cou                                                # Devuelve el COU
}                                                    # Cierra la función leer_cou_indec_auto
