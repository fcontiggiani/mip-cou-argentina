# =============================================================================
# 07_agregacion.R
# Agregación de la MIP de 107 ramas a los 5 grandes sectores del COU reducido
# publicado por el INDEC (2021), para comparar resultados.
#
# Correspondencia (secciones de la CIIU Rev. 3), verificada con el COU 2018:
# el VAB agregado de las 107 ramas reproduce exactamente el VAB de las 5
# ramas del cuadro reducido.
#   R1  A-B  divisiones 01-05  agro y pesca
#   R2  C-F  divisiones 10-45  minería, industria, electricidad, gas, agua y construcción
#   R3  G-I  divisiones 50-64  comercio, hoteles y restaurantes, transporte y comunicaciones
#   R4  J-K  divisiones 65-74  intermediación financiera, inmobiliarias y servicios empresariales
#   R5  L-P  divisiones 75-99  administración pública, enseñanza, salud y otros servicios
# =============================================================================

# -----------------------------------------------------------------------------
# Correspondencia de ramas
# -----------------------------------------------------------------------------

#' Asigna cada rama CIIU Rev. 3 a uno de los 5 grandes sectores del COU reducido
#'
#' @param codigos Códigos de rama tal como aparecen en el COU (p. ej., "011/014",
#'   "1511", "651 2", "73 PR"); se usan sus dos primeros dígitos (división).
#' @return Vector de texto con el nombre del gran sector de cada rama.
grupos_ramas_ciiu <- function(codigos) {             # Abre la función; recibe los códigos de rama
  division <- as.integer(substr(codigos, 1, 2))      # Extrae la división CIIU (dos primeros dígitos)
  if (anyNA(division)) stop("Códigos de rama sin división numérica: ", # Detiene si algún código no es interpretable...
                            paste(codigos[is.na(division)], collapse = ", ")) # ...e informa cuáles
  nombres <- c("R1 Agro y pesca",                    # Nombre del gran sector 1 (igual que en el COU reducido)
               "R2 Minería, industria, energía y construcción",          # Nombre del gran sector 2
               "R3 Comercio, transporte, comunicaciones y alojamiento",  # Nombre del gran sector 3
               "R4 Finanzas, inmobiliarias y servicios empresariales",   # Nombre del gran sector 4
               "R5 Educación, salud, gobierno y servicios personales")   # Nombre del gran sector 5
  idx <- findInterval(division, c(1, 10, 50, 65, 75)) # Ubica cada división en los cortes 01, 10, 50, 65 y 75
  nombres[idx]                                       # Devuelve el nombre del gran sector de cada rama
}                                                    # Cierra la función grupos_ramas_ciiu

# -----------------------------------------------------------------------------
# Agregación de una MIP
# -----------------------------------------------------------------------------

#' Agrega una MIP sumando transacciones de sectores del mismo grupo
#'
#' Se agregan las transacciones (Z, F, insumos primarios y VBP) y luego se
#' recalculan A y L: la agregación correcta es sobre flujos, no sobre
#' coeficientes ni sobre la inversa.
#'
#' @param mip Objeto "mip".
#' @param grupos Vector con el grupo de cada sector de la MIP (mismo orden).
#' @return Objeto "mip" agregado.
agregar_mip <- function(mip, grupos) {               # Abre la función; recibe la MIP y los grupos
  n <- length(mip$x)                                 # Cantidad de sectores de la MIP original
  stopifnot(length(grupos) == n)                     # Verifica que haya un grupo por sector
  niveles <- sort(unique(grupos))                    # Lista ordenada de grupos
  S <- t(sapply(niveles, function(g) as.numeric(grupos == g))) # Matriz de agregación S (grupos x sectores): 1 si pertenece
  rownames(S) <- niveles                             # Rotula las filas de S con los grupos
  Z <- S %*% mip$Z %*% t(S)                          # Transacciones intermedias agregadas: S Z S'
  F <- S %*% mip$F                                   # Demanda final agregada por grupo: S F
  x <- as.vector(S %*% mip$x)                        # VBP agregado por grupo: S x
  names(x) <- niveles                                # Rotula el VBP agregado
  pr_sec <- mip$primarios[, seq_len(n), drop = FALSE] %*% t(S) # Insumos primarios de los sectores, sumados por grupo
  pr_fin <- mip$primarios[, -seq_len(n), drop = FALSE] # Insumos primarios de los usos finales (sin cambios)
  primarios <- cbind(pr_sec, pr_fin)                 # Une ambos bloques de insumos primarios
  colnames(primarios) <- c(niveles, colnames(pr_fin)) # Rotula las columnas de insumos primarios
  dimnames(Z) <- list(niveles, niveles)              # Rotula filas y columnas de Z
  dimnames(F) <- list(niveles, colnames(mip$F))      # Rotula filas y columnas de F
  structure(list(Z = Z, F = F, x = x,                # Arma la MIP agregada con sus cuadrantes...
                 primarios = primarios,              # ...los insumos primarios...
                 ajuste_territorial = mip$ajuste_territorial, # ...el ajuste territorial...
                 modelo = mip$modelo,                # ...el modelo tecnológico...
                 unidad = mip$unidad),               # ...y la unidad de medida
            class = "mip")                           # Asigna la clase "mip"
}                                                    # Cierra la función agregar_mip

# -----------------------------------------------------------------------------
# Nombres de grupos para otros niveles de agregación
# -----------------------------------------------------------------------------

#' Nombre de la división CIIU Rev. 3 de cada rama (dos primeros dígitos)
#'
#' @param codigos Códigos de rama del COU.
#' @return Vector de texto "DD Nombre de la división".
divisiones_ciiu <- function(codigos) {               # Abre la función; recibe los códigos de rama
  nombres <- c(                                      # Tabla de divisiones CIIU Rev. 3 presentes en el COU:
    "01" = "Agricultura, ganadería y caza", "02" = "Silvicultura", "05" = "Pesca",                 # sección A-B
    "10" = "Extracción de carbón", "11" = "Petróleo crudo y gas natural",                         # sección C
    "13" = "Minerales metalíferos", "14" = "Otras minas y canteras",                              # sección C (cont.)
    "15" = "Alimentos y bebidas", "16" = "Tabaco", "17" = "Textiles", "18" = "Prendas de vestir", # sección D
    "19" = "Cuero y calzado", "20" = "Madera", "21" = "Papel", "22" = "Edición e impresión",      # sección D (cont.)
    "23" = "Refinación de petróleo", "24" = "Químicos", "25" = "Caucho y plástico",               # sección D (cont.)
    "26" = "Minerales no metálicos", "27" = "Metales comunes", "28" = "Productos de metal",       # sección D (cont.)
    "29" = "Maquinaria y equipo", "30" = "Maquinaria de oficina", "31" = "Aparatos eléctricos",   # sección D (cont.)
    "32" = "Radio, TV y comunicaciones", "33" = "Instrumentos médicos y de precisión",            # sección D (cont.)
    "34" = "Vehículos automotores", "35" = "Otro equipo de transporte",                           # sección D (cont.)
    "36" = "Muebles e industrias n.c.p.", "37" = "Reciclado y reparación",                        # sección D (cont.)
    "40" = "Electricidad y gas", "41" = "Agua", "45" = "Construcción",                            # secciones E-F
    "50" = "Venta y reparación de vehículos", "51" = "Comercio mayorista",                        # sección G
    "52" = "Comercio minorista", "55" = "Hoteles y restaurantes",                                 # secciones G-H
    "60" = "Transporte terrestre y por tuberías", "61" = "Transporte por agua",                   # sección I
    "62" = "Transporte aéreo", "63" = "Servicios complementarios del transporte",                 # sección I (cont.)
    "64" = "Correo y telecomunicaciones",                                                         # sección I (cont.)
    "65" = "Intermediación financiera", "66" = "Seguros y pensiones",                             # sección J
    "67" = "Auxiliares financieros", "70" = "Inmobiliarias", "71" = "Alquiler de equipo",         # secciones J-K
    "72" = "Informática", "73" = "Investigación y desarrollo", "74" = "Otros servicios empresariales", # sección K
    "75" = "Administración pública", "80" = "Enseñanza", "85" = "Salud y servicios sociales",     # secciones L-N
    "90" = "Saneamiento", "91" = "Asociaciones", "92" = "Esparcimiento y cultura",                # sección O
    "93" = "Otros servicios", "95" = "Servicio doméstico")                                        # secciones O-P
  div <- substr(codigos, 1, 2)                       # Extrae la división (dos primeros caracteres)
  etiqueta <- nombres[div]                           # Busca el nombre de cada división
  etiqueta[is.na(etiqueta)] <- "Sin clasificar"      # Marca las divisiones no previstas en la tabla
  paste(div, etiqueta)                               # Devuelve "DD Nombre"
}                                                    # Cierra la función divisiones_ciiu

#' Sección CPC 1.1 de cada producto (primer dígito)
#'
#' @param codigos Códigos de producto del COU.
#' @return Vector de texto con la sección CPC, con los mismos nombres que el COU reducido.
secciones_cpc <- function(codigos) {                 # Abre la función; recibe los códigos de producto
  nombres <- c(                                      # Secciones de la CPC 1.1 (como en el COU reducido):
    "P0 Agricultura, silvicultura y pesca", "P1 Minerales; electricidad, gas y agua", # secciones 0 y 1
    "P2 Alimentos, bebidas, tabaco y textiles", "P3 Otros bienes transportables",     # secciones 2 y 3
    "P4 Productos metálicos, maquinaria y equipo", "P5 Construcción",                 # secciones 4 y 5
    "P6 Comercio, alojamiento, transporte y comunicaciones",                          # sección 6
    "P7 Servicios financieros e inmobiliarios", "P8 Servicios a empresas y de producción", # secciones 7 y 8
    "P9 Servicios comunitarios, sociales y personales")                               # sección 9
  nombres[as.integer(substr(codigos, 1, 1)) + 1]     # Devuelve el nombre según el primer dígito del código
}                                                    # Cierra la función secciones_cpc

#' Quita de la MIP los sectores sin producción (filas y columnas nulas)
#'
#' En la MIP producto x producto, los productos que solo se importan no tienen
#' producción nacional ni tecnología propia; si se dejaran, sumarían columnas
#' de la identidad a L y alterarían los índices de encadenamiento.
#'
#' @param mip Objeto "mip".
#' @return Objeto "mip" sin los sectores de VBP nulo.
quitar_sectores_nulos <- function(mip) {             # Abre la función; recibe una MIP
  n <- length(mip$x)                                 # Cantidad de sectores
  ok <- mip$x > 0                                    # Sectores con producción positiva
  mip$Z <- mip$Z[ok, ok, drop = FALSE]               # Conserva solo filas y columnas de esos sectores en Z
  mip$F <- mip$F[ok, , drop = FALSE]                 # Conserva solo sus filas en la demanda final
  mip$primarios <- mip$primarios[, c(which(ok), (n + 1):ncol(mip$primarios)), drop = FALSE] # Conserva sus columnas de insumos primarios y los usos finales
  mip$x <- mip$x[ok]                                 # Conserva su VBP
  mip                                                # Devuelve la MIP depurada
}                                                    # Cierra la función quitar_sectores_nulos

# -----------------------------------------------------------------------------
# Reclasificación de las ramas a la CIIU Rev. 4 (correspondencia por sección)
# -----------------------------------------------------------------------------
# El INDEC compila los COU en CIIU Rev. 3. La tabla datos/correspondencia_ciiu3_ciiu4.csv
# asigna cada una de las 107 ramas a su sección principal de la CIIU Rev. 4, con
# la marca "exacta" o "aproximada" (ramas que mezclan actividades de dos
# secciones de la Rev. 4) y una nota. La MIP por sección Rev. 4 es la suma
# exacta de las ramas asignadas; lo aproximado es la asignación de las ramas mixtas.

#' Lee la tabla de correspondencia CIIU Rev. 3 -> Rev. 4
#'
#' @param archivo Ruta del CSV de correspondencia.
#' @return data.frame con codigo_rev3, rama_rev3, seccion_rev4,
#'   nombre_seccion_rev4, gran_sector_rev4, correspondencia y nota.
leer_correspondencia_ciiu4 <- function(archivo = "datos/correspondencia_ciiu3_ciiu4.csv") { # Abre la función de lectura
  corr <- utils::read.csv(archivo, colClasses = "character", # Lee el CSV con todas las columnas como texto...
                          encoding = "UTF-8")        # ...en UTF-8 (acentos)
  stopifnot(!anyDuplicated(corr$codigo_rev3))        # Verifica que cada rama aparezca una sola vez
  corr                                               # Devuelve la tabla de correspondencia
}                                                    # Cierra la función leer_correspondencia_ciiu4

#' Sección CIIU Rev. 4 de cada rama ("C Industrias manufactureras")
#'
#' @param codigos Códigos de rama del COU (CIIU Rev. 3).
#' @param corr Tabla de `leer_correspondencia_ciiu4()`.
#' @return Vector de texto con la letra y el nombre de la sección Rev. 4.
secciones_ciiu4 <- function(codigos, corr = leer_correspondencia_ciiu4()) { # Abre la función; recibe códigos y tabla
  i <- match(codigos, corr$codigo_rev3)              # Ubica cada código en la tabla de correspondencia
  if (anyNA(i)) stop("Ramas sin correspondencia CIIU Rev. 4: ", # Detiene si falta alguna rama...
                     paste(codigos[is.na(i)], collapse = ", ")) # ...e informa cuáles
  paste(corr$seccion_rev4[i], corr$nombre_seccion_rev4[i]) # Devuelve "Letra Nombre de la sección"
}                                                    # Cierra la función secciones_ciiu4

#' Gran sector (6 grupos) según la CIIU Rev. 4 de cada rama
#'
#' G1 agro y pesca (A); G2 industria manufacturera (C); G3 minería, energía,
#' agua y construcción (B, D, E, F); G4 comercio, transporte, alojamiento e
#' información (G-J); G5 finanzas, inmobiliarias y servicios a empresas (K-N);
#' G6 administración pública, enseñanza, salud y otros servicios (O-T).
#'
#' @param codigos Códigos de rama del COU (CIIU Rev. 3).
#' @param corr Tabla de `leer_correspondencia_ciiu4()`.
#' @return Vector de texto con el gran sector Rev. 4.
grandes_sectores_ciiu4 <- function(codigos, corr = leer_correspondencia_ciiu4()) { # Abre la función
  i <- match(codigos, corr$codigo_rev3)              # Ubica cada código en la tabla
  if (anyNA(i)) stop("Ramas sin correspondencia CIIU Rev. 4: ", # Detiene si falta alguna rama...
                     paste(codigos[is.na(i)], collapse = ", ")) # ...e informa cuáles
  corr$gran_sector_rev4[i]                           # Devuelve el gran sector Rev. 4 de cada rama
}                                                    # Cierra la función grandes_sectores_ciiu4

#' Gran sector Rev. 4 asociado a cada sección CPC (para colorear productos)
#'
#' @param seccion_cpc Dígito de la sección CPC (0 a 9), como número o texto.
#' @return Vector de texto con el gran sector Rev. 4 más afín a la sección.
cpc_a_gran_sector_ciiu4 <- function(seccion_cpc) {  # Abre la función; recibe la sección CPC
  nombres <- c(G1 = "G1 Agro y pesca",               # Nombres de los 6 grandes sectores Rev. 4...
               G2 = "G2 Industria manufacturera", # gran sector G2: industria
               G3 = "G3 Minería, energía, agua y construcción", # gran sector G3: minería, energía, agua y construcción
               G4 = "G4 Comercio, transporte, alojamiento e información", # gran sector G4: comercio, transporte, alojamiento e información
               G5 = "G5 Finanzas, inmobiliarias y servicios a empresas", # gran sector G5: finanzas, inmobiliarias y servicios a empresas
               G6 = "G6 Administración pública, enseñanza, salud y otros servicios") # ...(como en la tabla de correspondencia)
  clave <- c("G1", "G3", "G2", "G2", "G2", "G3", "G4", "G5", "G5", "G6") # Sección CPC 0-9 -> gran sector Rev. 4
  unname(nombres[clave[as.integer(seccion_cpc) + 1]]) # Devuelve el nombre del gran sector de cada sección
}                                                    # Cierra la función cpc_a_gran_sector_ciiu4
