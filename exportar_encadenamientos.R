# =============================================================================
# exportar_encadenamientos.R
# Calcula los encadenamientos hacia atrás (BL) y hacia adelante (FL, con
# Leontief y con Ghosh) para varios niveles de agregación de ramas y de
# productos, en cada COU anual del INDEC, y los exporta en JSON y CSV para
# el tablero interactivo.
#
# Niveles:
#   Ramas (MIP rama x rama, modelo D), CIIU Rev. 3: 107 ramas; 58 divisiones; 5 grandes sectores.
#   Ramas, CIIU Rev. 4 (correspondencia por sección): 20 secciones; 6 grandes sectores.
#   Productos (MIP producto x producto, modelo B): productos CPC; 10 secciones CPC.
#   COU 2018 reducido: 5 ramas (modelo D) y 10 productos (modelo B).
#
# Uso: Rscript exportar_encadenamientos.R "G:/Unidades compartidas/FEC-UNRN/Rfiles/MIP/COU"
# Salidas: salidas/encadenamientos_2018_2023.json y .csv, y
#          docs/datos/encadenamientos_2018_2023.js (datos del tablero web)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Carga de funciones y parámetros
# -----------------------------------------------------------------------------
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8") # Carga las funciones
args <- commandArgs(trailingOnly = TRUE)             # Lee los argumentos de la línea de comandos
carpeta_cou <- if (length(args) > 0) args[1] else Sys.getenv("CARPETA_COU", "datos/COU") # Carpeta de los COU
archivos_cou <- sort(list.files(carpeta_cou, pattern = "^sh_cou_.*\\.xlsx?$", full.names = TRUE)) # Libros anuales
if (length(archivos_cou) == 0) stop("No hay archivos sh_cou_*.xls en ", carpeta_cou) # Detiene si no hay archivos
dir.create("salidas", showWarnings = FALSE)          # Crea la carpeta de salidas si no existe
corr4 <- leer_correspondencia_ciiu4()                # Lee la correspondencia de ramas CIIU Rev. 3 -> Rev. 4

# -----------------------------------------------------------------------------
# 1. Función que resume una MIP en una tabla de puntos para el gráfico
# -----------------------------------------------------------------------------
#' @param mip Objeto "mip" (ya agregado si corresponde).
#' @param anio Año del COU.
#' @param nivel Clave del nivel de agregación.
#' @param codigos Código de cada sector (mismo orden que mip$x).
#' @param grupo Gran grupo de cada sector (para el color del punto).
#' @return data.frame con un punto por sector.
puntos_mip <- function(mip, anio, nivel, codigos, grupo) { # Abre la función de resumen
  ok <- mip$x > 0                                    # Marca los sectores con producción positiva
  codigos <- codigos[ok]                             # Conserva los códigos de esos sectores
  grupo <- grupo[ok]                                 # Conserva sus grupos
  mip <- quitar_sectores_nulos(mip)                  # Quita de la MIP los sectores sin producción
  an <- analizar_mip(mip)                            # Calcula A, L, multiplicadores y encadenamientos
  m <- an$multiplicadores                            # Tabla de multiplicadores
  e <- an$encadenamientos                            # Tabla de encadenamientos
  data.frame(                                        # Arma la tabla de puntos:
    anio = anio,                                     #   año
    nivel = nivel,                                   #   nivel de agregación
    codigo = codigos,                                #   código del sector
    sector = names(mip$x),                           #   nombre del sector
    grupo = grupo,                                   #   gran grupo (color)
    vbp = round(mip$x),                              #   valor bruto de producción (millones de pesos corrientes)
    vbp_part = round(100 * mip$x / sum(mip$x), 3),   #   participación en el VBP total (%)
    BL = round(e$BL, 4),                             #   encadenamiento hacia atrás (Rasmussen)
    FL_leontief = round(e$FL_leontief, 4),           #   hacia adelante con filas de L (Hirschman-Rasmussen)
    FL_ghosh = round(e$FL_ghosh, 4),                 #   hacia adelante con filas de G (Ghosh)
    mult_prod = round(m$produccion, 4),              #   multiplicador de producción
    mult_va = round(m$valor_agregado, 4),            #   multiplicador de valor agregado
    mult_imp = round(m$importaciones, 4),            #   multiplicador de importaciones
    stringsAsFactors = FALSE)                        #   texto como caracteres
}                                                    # Cierra la función puntos_mip

# -----------------------------------------------------------------------------
# 2. COU 2018 reducido (referencia)
# -----------------------------------------------------------------------------
cou_red <- cou_indec_2018_reducido()                 # Carga el COU 2018 reducido
mip_red_D <- cou_a_mip(cou_red, "D")                 # MIP rama x rama (5 ramas)
mip_red_B <- cou_a_mip(cou_red, "B")                 # MIP producto x producto (10 productos)
puntos <- list(                                      # Inicia la lista de tablas de puntos con la referencia:
  puntos_mip(mip_red_D, 2018, "red_ramas",           #   5 ramas del COU reducido...
             substr(names(mip_red_D$x), 1, 2),       #   ...código R1 a R5...
             names(mip_red_D$x)),                    #   ...cada rama es su propio grupo
  puntos_mip(mip_red_B, 2018, "red_productos",       #   10 productos del COU reducido...
             substr(names(mip_red_B$x), 1, 2),       #   ...código P0 a P9...
             names(mip_red_B$x)))                    #   ...cada producto es su propio grupo

# -----------------------------------------------------------------------------
# 3. COU completos: siete niveles por año
# -----------------------------------------------------------------------------
for (archivo in archivos_cou) {                      # Recorre cada libro anual
  cou <- leer_cou_indec_auto(archivo)                # Lee el COU por rótulos
  anio <- cou$anio                                   # Año del cuadro
  cat("Procesando", anio, "...\n")                   # Informa el avance

  # Ramas (modelo D)
  mip_D <- suppressWarnings(cou_a_mip(cou, "D"))     # MIP rama x rama de 107 ramas
  cod_r <- cou$codigos_ramas                         # Códigos CIIU de las ramas
  g5 <- grupos_ramas_ciiu(cod_r)                     # Gran sector de cada rama
  div <- divisiones_ciiu(cod_r)                      # División CIIU de cada rama
  puntos[[length(puntos) + 1]] <- puntos_mip(mip_D, anio, "ramas_107", cod_r, g5) # Nivel: 107 ramas
  div_u <- sort(unique(div))                         # Divisiones distintas, ordenadas
  puntos[[length(puntos) + 1]] <- puntos_mip(agregar_mip(mip_D, div), anio, "ramas_div", # Nivel: divisiones CIIU...
                                             substr(div_u, 1, 2),                        # ...código de división...
                                             g5[match(div_u, div)])                      # ...gran sector de cada división
  g5_u <- sort(unique(g5))                           # Grandes sectores distintos, ordenados
  puntos[[length(puntos) + 1]] <- puntos_mip(agregar_mip(mip_D, g5), anio, "ramas_5", # Nivel: 5 grandes sectores...
                                             substr(g5_u, 1, 2), g5_u)                # ...cada uno es su propio grupo

  # Ramas reclasificadas a la CIIU Rev. 4
  sec4 <- secciones_ciiu4(cod_r, corr4)              # Sección CIIU Rev. 4 de cada rama
  gs4 <- grandes_sectores_ciiu4(cod_r, corr4)        # Gran sector Rev. 4 de cada rama
  sec4_u <- sort(unique(sec4))                       # Secciones Rev. 4 distintas, ordenadas
  puntos[[length(puntos) + 1]] <- puntos_mip(agregar_mip(mip_D, sec4), anio, "ramas4_secc", # Nivel: secciones Rev. 4...
                                             substr(sec4_u, 1, 1),                          # ...código: letra de la sección...
                                             gs4[match(sec4_u, sec4)])                      # ...gran sector Rev. 4 de cada sección
  gs4_u <- sort(unique(gs4))                         # Grandes sectores Rev. 4 distintos, ordenados
  puntos[[length(puntos) + 1]] <- puntos_mip(agregar_mip(mip_D, gs4), anio, "ramas4_6", # Nivel: 6 grandes sectores Rev. 4...
                                             substr(gs4_u, 1, 2), gs4_u)                # ...cada uno es su propio grupo

  # Productos (modelo B)
  mip_B <- suppressWarnings(cou_a_mip(cou, "B"))     # MIP producto x producto
  cod_p <- cou$codigos_productos                     # Códigos CPC de los productos
  secc <- secciones_cpc(cod_p)                       # Sección CPC de cada producto
  puntos[[length(puntos) + 1]] <- puntos_mip(mip_B, anio, "productos", cod_p, secc) # Nivel: productos CPC
  secc_u <- sort(unique(secc))                       # Secciones distintas, ordenadas
  puntos[[length(puntos) + 1]] <- puntos_mip(agregar_mip(mip_B, secc), anio, "productos_secc", # Nivel: secciones CPC...
                                             substr(secc_u, 1, 2), secc_u)                     # ...cada una es su propio grupo
}                                                    # Cierra el ciclo sobre los años

tabla <- do.call(rbind, puntos)                      # Une todas las tablas de puntos
rownames(tabla) <- NULL                              # Elimina los nombres de fila

# -----------------------------------------------------------------------------
# 4. Columnas derivadas: tipología según cada enfoque, gran sector e identificador
# -----------------------------------------------------------------------------
tipologia <- function(bl, fl) {                      # Abre la función de tipología de Hirschman
  ifelse(bl >= 1 & fl >= 1, "clave",                 # BL y FL mayores que el promedio: sector clave
         ifelse(bl >= 1, "impulsor",                 # Solo BL alto: impulsor (arrastra a sus proveedores)
                ifelse(fl >= 1, "base",              # Solo FL alto: base (provee a muchos sectores)
                       "independiente")))            # Ambos bajos: independiente
}                                                    # Cierra la función tipologia
tabla$tipo_hirschman <- tipologia(tabla$BL, tabla$FL_leontief) # Tipología con FL de Leontief (Hirschman-Rasmussen)
tabla$tipo_ghosh <- tipologia(tabla$BL, tabla$FL_ghosh)        # Tipología con FL de Ghosh
seccion_a_sector <- c("R1", "R2", "R2", "R2", "R2", "R2", "R3", "R4", "R4", "R5") # Sección CPC 0-9 -> gran sector Rev. 3
nombres_gs <- c(R1 = "Agro y pesca", R2 = "Minería, industria, energía y construcción", # Nombres de los grandes sectores Rev. 3...
                R3 = "Comercio, transporte, comunicaciones y alojamiento", # gran sector R3
                R4 = "Finanzas, inmobiliarias y servicios empresariales",  # gran sector R4
                R5 = "Educación, salud, gobierno y servicios personales") # ...(los mismos del COU reducido)
es_prod <- tabla$nivel %in% c("productos", "productos_secc", "red_productos") # Filas de productos (grupo = sección CPC)
es_rev3 <- tabla$nivel %in% c("ramas_107", "ramas_div", "ramas_5", "red_ramas") # Filas de ramas clasificadas en CIIU Rev. 3
es_rev4 <- tabla$nivel %in% c("ramas4_secc", "ramas4_6") # Filas de ramas clasificadas en CIIU Rev. 4
dig_cpc <- ifelse(es_prod, substr(tabla$grupo, 2, 2), NA) # Dígito de la sección CPC (solo productos)

# Gran sector según la CIIU Rev. 3 (color del tablero con la Rev. 3)
clave3 <- ifelse(es_prod, seccion_a_sector[as.integer(dig_cpc) + 1], # productos: según su sección CPC
                 ifelse(es_rev3, substr(tabla$grupo, 1, 2), NA))     # ramas Rev. 3: R1 a R5; ramas Rev. 4: sin dato
tabla$gran_sector <- ifelse(is.na(clave3), NA, paste(clave3, nombres_gs[clave3])) # Gran sector Rev. 3 de cada fila

# Gran sector según la CIIU Rev. 4 (color del tablero con la Rev. 4)
tabla$gran_sector4 <- NA_character_                  # Inicia la columna sin dato
tabla$gran_sector4[es_rev4] <- tabla$grupo[es_rev4]  # Niveles Rev. 4: el grupo ya es el gran sector Rev. 4
r107 <- tabla$nivel == "ramas_107"                   # Filas de las 107 ramas
tabla$gran_sector4[r107] <- grandes_sectores_ciiu4(tabla$codigo[r107], corr4) # 107 ramas: según la correspondencia
tabla$gran_sector4[es_prod & tabla$nivel != "red_productos"] <- # Productos de los COU completos...
  cpc_a_gran_sector_ciiu4(dig_cpc[es_prod & tabla$nivel != "red_productos"]) # ...según su sección CPC

# Sección Rev. 4 y tipo de correspondencia de cada una de las 107 ramas
tabla$seccion4 <- NA_character_                      # Inicia la columna de sección Rev. 4 sin dato
tabla$seccion4[r107] <- secciones_ciiu4(tabla$codigo[r107], corr4) # Sección Rev. 4 de cada rama
tabla$correspondencia4 <- NA_character_              # Inicia la columna de tipo de correspondencia
tabla$correspondencia4[r107] <- corr4$correspondencia[match(tabla$codigo[r107], corr4$codigo_rev3)] # "exacta" o "aproximada"
sec_nivel <- tabla$nivel == "ramas4_secc"            # Filas del nivel de secciones Rev. 4
tabla$seccion4[sec_nivel] <- tabla$sector[sec_nivel] # En ese nivel, la sección es el propio sector
tabla$id <- paste(tabla$nivel, tabla$anio, tabla$codigo, sep = "|") # Identificador único de cada fila
stopifnot(!anyDuplicated(tabla$id))                  # Verifica que el identificador no se repita

# -----------------------------------------------------------------------------
# 5. Exportación
# -----------------------------------------------------------------------------
utils::write.csv(tabla, "salidas/encadenamientos_2018_2023.csv", # Exporta la tabla en CSV...
                 row.names = FALSE, fileEncoding = "UTF-8")      # ...sin nombres de fila y en UTF-8
jsonlite::write_json(tabla, "salidas/encadenamientos_2018_2023.json", # Exporta la tabla en JSON (una fila por objeto)...
                     dataframe = "rows", auto_unbox = TRUE, digits = NA) # ...sin envolver escalares y sin perder decimales
dir.create(file.path("docs", "datos"), showWarnings = FALSE, recursive = TRUE) # Crea la carpeta de datos del tablero web
writeLines(c("window.DATOS = ",                      # Escribe los datos como script de JavaScript para el tablero...
             jsonlite::toJSON(tabla, dataframe = "rows", auto_unbox = TRUE, digits = NA, na = "null"), # ...en formato JSON...
             ";"),                                   # ...cerrando la asignación
           file.path("docs", "datos", "encadenamientos_2018_2023.js"), useBytes = TRUE) # Ruta del archivo que lee docs/index.html
print(table(tabla$nivel, tabla$anio))                # Muestra cuántos puntos hay por nivel y año
