# =============================================================================
# comparar_cou_anuales.R
# Estima la MIP rama x rama (modelo D) para cada COU anual del INDEC de una
# carpeta, calcula multiplicadores y encadenamientos, y los compara con los
# resultados del COU 2018 reducido (5 ramas).
#
# Uso (desde la carpeta del proyecto):
#   Rscript comparar_cou_anuales.R "G:/Unidades compartidas/FEC-UNRN/Rfiles/MIP/COU"
# Si no se indica la carpeta, se usa la variable de entorno CARPETA_COU o,
# en su defecto, "datos/COU".
#
# Salida: salidas/comparacion_mip_cou_2018_2023.xlsx
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Carga de funciones y parámetros
# -----------------------------------------------------------------------------
archivos_r <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE)) # Lista los scripts de funciones
for (f in archivos_r) source(f, encoding = "UTF-8")  # Carga cada script de funciones
options(width = 160)                                 # Amplía el ancho de impresión en la consola

args <- commandArgs(trailingOnly = TRUE)             # Lee los argumentos de la línea de comandos
carpeta_cou <- if (length(args) > 0) {               # Si se indicó una carpeta como argumento...
  args[1]                                            #   ...se usa esa carpeta
} else {                                             # Si no...
  Sys.getenv("CARPETA_COU", "datos/COU")             #   ...se usa la variable de entorno o la carpeta por defecto
}                                                    # Cierra la elección de la carpeta
archivos_cou <- sort(list.files(carpeta_cou,         # Lista los libros del INDEC en la carpeta...
                                pattern = "^sh_cou_.*\\.xlsx?$", # ...con nombre sh_cou_*.xls o .xlsx...
                                full.names = TRUE))  # ...con su ruta completa
if (length(archivos_cou) == 0) stop("No hay archivos sh_cou_*.xls en ", carpeta_cou) # Detiene si no hay archivos
dir.create("salidas", showWarnings = FALSE)          # Crea la carpeta de salidas si no existe

# Indicadores a comparar: nombre de columna -> rótulo legible
indicadores <- c(                                    # Define los indicadores y sus rótulos:
  produccion     = "Multiplicador de producción",   #   1'L
  valor_agregado = "Multiplicador de valor agregado", # v'L
  importaciones  = "Multiplicador de importaciones", #   m'L
  impuestos      = "Multiplicador de impuestos sobre productos", # t'L
  BL             = "Encadenamiento hacia atrás (BL)", #  Rasmussen, columnas de L
  FL_ghosh       = "Encadenamiento hacia adelante (FL, Ghosh)", # filas de G
  FL_leontief    = "Encadenamiento hacia adelante (FL, Leontief)" # filas de L
)                                                    # Cierra la definición de indicadores

# Une multiplicadores y encadenamientos en una sola tabla por sector
tabla_indicadores <- function(an) {                  # Abre la función; recibe el resultado de analizar_mip()
  merge(an$multiplicadores,                          # Combina la tabla de multiplicadores...
        an$encadenamientos,                          # ...con la de encadenamientos...
        by = "sector", sort = FALSE)                 # ...por el nombre del sector, sin reordenar
}                                                    # Cierra la función tabla_indicadores

# -----------------------------------------------------------------------------
# 1. Referencia: COU 2018 reducido (5 ramas), modelo D
# -----------------------------------------------------------------------------
cou_red <- cou_indec_2018_reducido()                 # Carga el COU 2018 reducido publicado por el INDEC
invisible(capture.output(validar_cou(cou_red)))      # Verifica sus identidades contables (sin imprimir)
mip_red <- cou_a_mip(cou_red, modelo = "D")          # Construye la MIP rama x rama de 5 sectores
invisible(capture.output(validar_mip(mip_red)))      # Verifica la MIP (sin imprimir)
ind_red <- tabla_indicadores(analizar_mip(mip_red))  # Calcula sus multiplicadores y encadenamientos

# -----------------------------------------------------------------------------
# 2. MIP de 107 ramas para cada año y su agregación a 5 grandes sectores
# -----------------------------------------------------------------------------
res107 <- list()                                     # Lista para los indicadores de 107 ramas, por año
res5 <- list()                                       # Lista para los indicadores agregados a 5 sectores, por año
res4 <- list()                                       # Lista para los indicadores por sección CIIU Rev. 4, por año
corr4 <- leer_correspondencia_ciiu4()                # Lee la correspondencia de ramas CIIU Rev. 3 -> Rev. 4
controles <- list()                                  # Lista para los controles de consistencia, por año
for (archivo in archivos_cou) {                      # Recorre cada libro anual del INDEC
  cou <- leer_cou_indec_auto(archivo)                # Lee el COU ubicando los bloques por sus rótulos
  anio <- as.character(cou$anio)                     # Año del cuadro, como texto (clave de las listas)
  cat("Procesando COU", anio, "...\n")               # Informa el avance
  ctrl_cou <- NULL                                   # Reserva la variable de controles del COU
  invisible(capture.output(ctrl_cou <- validar_cou(cou, detener = FALSE))) # Valida el COU sin imprimir
  mip <- suppressWarnings(cou_a_mip(cou, modelo = "D")) # Construye la MIP rama x rama (107 ramas)
  ctrl_mip <- NULL                                   # Reserva la variable de controles de la MIP
  invisible(capture.output(ctrl_mip <- validar_mip(mip, detener = FALSE))) # Valida la MIP sin imprimir
  an <- analizar_mip(mip)                            # Calcula A, L, multiplicadores y encadenamientos

  grupos <- grupos_ramas_ciiu(cou$codigos_ramas)     # Asigna cada rama a su gran sector (5 grupos)
  ind <- tabla_indicadores(an)                       # Une los indicadores de las 107 ramas
  ind$codigo <- cou$codigos_ramas[match(ind$sector, names(mip$x))] # Agrega el código CIIU de cada rama
  ind$gran_sector <- grupos[match(ind$sector, names(mip$x))]       # Agrega el gran sector de cada rama
  res107[[anio]] <- ind                              # Guarda los indicadores de 107 ramas del año

  mip5 <- agregar_mip(mip, grupos)                   # Agrega las transacciones de la MIP a 5 sectores
  res5[[anio]] <- tabla_indicadores(analizar_mip(mip5)) # Calcula los indicadores de la MIP agregada
  mip4 <- agregar_mip(mip, secciones_ciiu4(cou$codigos_ramas, corr4)) # Agrega las transacciones a secciones CIIU Rev. 4
  res4[[anio]] <- tabla_indicadores(analizar_mip(mip4)) # Calcula los indicadores por sección Rev. 4

  s <- mip$pasos$importaciones$s                     # Proporciones importadas de los usos internos
  controles[[anio]] <- data.frame(                   # Arma la fila de controles del año:
    anio = cou$anio,                                 #   año
    productos = nrow(cou$prod),                      #   cantidad de productos CPC
    ramas = ncol(cou$prod),                          #   cantidad de ramas CIIU
    tipos_de_margen = paste(colnames(as.matrix(cou$margenes)), collapse = " + "), # tipos de margen leídos
    PIB_millones = round(attr(ctrl_cou, "pib")[["produccion"]]),  # PIB por la producción (millones de pesos)
    PIB_MIP_millones = round(sum(mip$primarios["Valor agregado bruto pb", ]) +   # PIB según la MIP: VAB...
                               sum(mip$primarios["Impuestos netos sobre productos", ])), # ...+ impuestos
    COU_identidades_ok = all(ctrl_cou$ok),           #   ¿el COU cumple sus identidades?
    MIP_identidades_ok = all(ctrl_mip$ok),           #   ¿la MIP cumple sus identidades y Hawkins-Simon?
    MIP_max_dif_filas = signif(ctrl_mip$max_dif_abs[1], 3), # mayor diferencia en la identidad por filas
    radio_espectral_A = round(ctrl_mip$max_dif_abs[4], 4),  # radio espectral de A
    prop_importada_mediana = round(stats::median(s[s > 0]), 4), # mediana de la proporción importada (productos con importaciones)
    ajuste_CIF_FOB_residentes = round(sum(mip$pasos$importaciones$aj_residentes)), # fletes de residentes (ajuste CIF/FOB)
    stringsAsFactors = FALSE)                        #   texto como caracteres
}                                                    # Cierra el ciclo sobre los años
anios <- names(res107)                               # Años procesados, en orden

# -----------------------------------------------------------------------------
# 3. Tabla comparativa a 5 grandes sectores
# -----------------------------------------------------------------------------
sectores5 <- ind_red$sector                          # Orden de los 5 grandes sectores (como en el COU reducido)
comp5 <- do.call(rbind, lapply(names(indicadores), function(k) { # Para cada indicador, arma un bloque de 5 filas:
  bloque <- data.frame(indicador = indicadores[[k]], #   rótulo del indicador
                       gran_sector = sectores5,      #   gran sector
                       stringsAsFactors = FALSE)     #   texto como caracteres
  bloque[["COU reducido 2018"]] <- ind_red[[k]][match(sectores5, ind_red$sector)] # Valor con el COU reducido
  for (a in anios) {                                 #   Para cada año del COU completo...
    bloque[[paste("COU completo", a)]] <- res5[[a]][[k]][match(sectores5, res5[[a]]$sector)] # ...valor agregado a 5 sectores
  }                                                  #   Cierra el ciclo de años
  bloque[["Dif. completo-reducido 2018"]] <- bloque[["COU completo 2018"]] - bloque[["COU reducido 2018"]] # Sesgo de agregación
  bloque[[paste0("Var. ", anios[length(anios)], "-", anios[1])]] <- # Variación entre el último y el primer año...
    bloque[[paste("COU completo", anios[length(anios)])]] - bloque[[paste("COU completo", anios[1])]] # ...del COU completo
  bloque                                             #   Devuelve el bloque del indicador
}))                                                  # Apila los bloques de todos los indicadores

# Indicadores por sección CIIU Rev. 4 (una fila por sección e indicador, una columna por año)
secciones4 <- res4[[anios[1]]]$sector                # Secciones Rev. 4 (orden alfabético por letra)
comp4 <- do.call(rbind, lapply(names(indicadores), function(k) { # Para cada indicador, arma un bloque:
  bloque <- data.frame(indicador = indicadores[[k]], #   rótulo del indicador
                       seccion_ciiu4 = secciones4,   #   sección CIIU Rev. 4
                       stringsAsFactors = FALSE)     #   texto como caracteres
  for (a in anios) {                                 #   Para cada año...
    bloque[[a]] <- res4[[a]][[k]][match(secciones4, res4[[a]]$sector)] # ...valor de la sección
  }                                                  #   Cierra el ciclo de años
  bloque[[paste0("Var. ", anios[length(anios)], "-", anios[1])]] <- bloque[[anios[length(anios)]]] - bloque[[anios[1]]] # Variación del período
  bloque                                             #   Devuelve el bloque del indicador
}))                                                  # Apila los bloques de todos los indicadores

tipo5 <- data.frame(gran_sector = sectores5,         # Tipología de Hirschman a 5 sectores: gran sector...
                    "COU reducido 2018" = ind_red$tipo[match(sectores5, ind_red$sector)], # ...según el COU reducido
                    check.names = FALSE, stringsAsFactors = FALSE) # Conserva los nombres de columna
for (a in anios) tipo5[[paste("COU completo", a)]] <- res5[[a]]$tipo[match(sectores5, res5[[a]]$sector)] # Agrega cada año

# -----------------------------------------------------------------------------
# 4. Tablas por indicador para las 107 ramas
# -----------------------------------------------------------------------------
base107 <- res107[[anios[1]]][, c("codigo", "sector", "gran_sector")] # Identificación de las ramas (primer año)
tablas107 <- lapply(names(indicadores), function(k) { # Para cada indicador, arma una tabla de 107 ramas:
  t107 <- base107                                    #   parte de la identificación de las ramas
  for (a in anios) {                                 #   para cada año...
    t107[[a]] <- res107[[a]][[k]][match(t107$codigo, res107[[a]]$codigo)] # ...valor de la rama (por código CIIU)
  }                                                  #   cierra el ciclo de años
  t107[[paste0("Var. ", anios[length(anios)], "-", anios[1])]] <- t107[[anios[length(anios)]]] - t107[[anios[1]]] # Variación del período
  t107[["Gran sector: COU reducido 2018"]] <- ind_red[[k]][match(t107$gran_sector, ind_red$sector)] # Referencia: gran sector, COU reducido
  t107[["Gran sector: COU completo 2018"]] <- res5[[anios[1]]][[k]][match(t107$gran_sector, res5[[anios[1]]]$sector)] # Referencia: gran sector, COU completo
  t107                                               #   devuelve la tabla del indicador
})                                                   # Cierra el armado de tablas
names(tablas107) <- names(indicadores)               # Nombra cada tabla con la clave del indicador

tipo107 <- base107                                   # Tipología de Hirschman de las 107 ramas: identificación...
for (a in anios) tipo107[[a]] <- res107[[a]]$tipo[match(tipo107$codigo, res107[[a]]$codigo)] # ...y tipo de cada año

# -----------------------------------------------------------------------------
# 5. Formato largo (todas las cifras en una sola tabla)
# -----------------------------------------------------------------------------
a_largo <- function(df, anio, nivel) {               # Abre la función que pasa una tabla de indicadores a formato largo
  do.call(rbind, lapply(names(indicadores), function(k) { # Para cada indicador...
    data.frame(anio = anio, nivel = nivel,           #   ...año y nivel de agregación...
               codigo = if (is.null(df$codigo)) NA else df$codigo, # ...código CIIU (si existe)...
               sector = df$sector,                   #   ...sector...
               indicador = indicadores[[k]],         #   ...rótulo del indicador...
               valor = df[[k]],                      #   ...y valor
               stringsAsFactors = FALSE)             #   texto como caracteres
  }))                                                # Apila los indicadores
}                                                    # Cierra la función a_largo
largo <- rbind(                                      # Apila todas las cifras en formato largo:
  a_largo(ind_red, 2018, "5 sectores - COU reducido"), # referencia del COU reducido
  do.call(rbind, lapply(anios, function(a) a_largo(res5[[a]], as.integer(a), "5 sectores - COU completo agregado"))), # agregados
  do.call(rbind, lapply(anios, function(a) a_largo(res107[[a]], as.integer(a), "107 ramas - COU completo")))         # 107 ramas
)                                                    # Cierra el formato largo

# -----------------------------------------------------------------------------
# 6. Exportación a Excel
# -----------------------------------------------------------------------------
redondear <- function(df, d = 4) {                   # Abre la función que redondea las columnas numéricas
  num <- vapply(df, is.numeric, logical(1))          # Identifica las columnas numéricas
  df[num] <- lapply(df[num], round, digits = d)      # Redondea esas columnas a d decimales
  df                                                 # Devuelve la tabla redondeada
}                                                    # Cierra la función redondear
hojas <- list(                                       # Arma la lista de hojas del libro de salida:
  "Comparación 5 sectores" = redondear(comp5),       #   comparación de indicadores a 5 sectores
  "Tipología 5 sectores" = tipo5,                    #   tipología de Hirschman a 5 sectores
  "Secciones CIIU Rev4" = redondear(comp4),          #   indicadores por sección CIIU Rev. 4
  "Correspondencia CIIU" = corr4,                    #   correspondencia de ramas Rev. 3 -> Rev. 4
  "107 - Mult producción" = redondear(tablas107$produccion),        # multiplicador de producción por rama
  "107 - Mult VA" = redondear(tablas107$valor_agregado),            # multiplicador de valor agregado por rama
  "107 - Mult importaciones" = redondear(tablas107$importaciones),  # multiplicador de importaciones por rama
  "107 - Mult impuestos" = redondear(tablas107$impuestos),          # multiplicador de impuestos por rama
  "107 - BL" = redondear(tablas107$BL),                             # encadenamiento hacia atrás por rama
  "107 - FL Ghosh" = redondear(tablas107$FL_ghosh),                 # encadenamiento hacia adelante (Ghosh) por rama
  "107 - FL Leontief" = redondear(tablas107$FL_leontief),           # encadenamiento hacia adelante (Leontief) por rama
  "107 - Tipología" = tipo107,                                      # tipología de Hirschman por rama
  "Datos largo" = redondear(largo, 6),               #   todas las cifras en formato largo
  "Controles" = do.call(rbind, controles),           #   controles de consistencia por año
  "Notas" = data.frame(nota = c(                     #   notas metodológicas:
    "Fuente: INDEC, Cuadros de oferta y utilización 2018-2023 (sh_cou_AAAA_*.xls) y COU 2018 reducido (INDEC, 2021). Elaboración propia.", # fuente
    "MIP rama x rama (modelo D de Eurostat: estructura fija de ventas por producto), flujos domésticos a precios básicos, 107 ramas CIIU Rev. 3.", # método
    "Valores en millones de pesos corrientes de cada año; los multiplicadores y encadenamientos son adimensionales.", # unidades
    "Supuestos: tasas uniformes de impuestos (DI + IP + IVA no deducible) y de márgenes (distribución y comisiones por cambio de divisas) por producto; proporcionalidad de importaciones en los usos internos; exportaciones abastecidas primero con producción nacional.", # supuestos
    "Ajuste CIF/FOB: se descuenta primero de las importaciones del mismo servicio; el excedente se registra como fletes de residentes en una columna de la demanda final.", # ajuste CIF/FOB
    "5 sectores: agregación de las transacciones de la MIP de 107 ramas según secciones CIIU Rev. 3 (A-B, C-F, G-I, J-K, L-P); reproduce exactamente el VAB del COU 2018 reducido.", # agregación
    "La diferencia 'completo - reducido' en 2018 mide el sesgo de agregación: aplicar los supuestos sobre 10 grupos de productos en lugar de 223 productos cambia los coeficientes.", # sesgo de agregación
    "CIIU Rev. 4: los COU se compilan en CIIU Rev. 3; las secciones Rev. 4 agregan las ramas según su actividad principal (datos/correspondencia_ciiu3_ciiu4.csv). 8 de las 107 ramas mezclan actividades de dos secciones y se asignan a la principal (marca 'aproximada').", # nota CIIU Rev. 4
    "BL y FL son índices relativos al promedio de cada matriz (promedio = 1): no son comparables entre el nivel de 107 ramas y el de 5 sectores.", # comparabilidad de índices
    "Los cambios entre años reflejan cambios técnicos y de precios relativos (cuadros a precios corrientes)." # interpretación temporal
  ))                                                 #   cierra la hoja de notas
)                                                    # Cierra la lista de hojas
salida <- file.path("salidas", "comparacion_mip_cou_2018_2023.xlsx") # Ruta del libro de salida
writexl::write_xlsx(hojas, salida)                   # Escribe todas las hojas en el libro Excel
cat("\nResultados exportados en", salida, "\n")      # Informa la ruta del archivo generado

# -----------------------------------------------------------------------------
# 7. Resumen en consola
# -----------------------------------------------------------------------------
cat("\n== Controles por año ==\n")                   # Imprime un encabezado
print(do.call(rbind, controles)[, c("anio", "productos", "PIB_millones", "PIB_MIP_millones", # Muestra los controles principales
                                    "COU_identidades_ok", "MIP_identidades_ok", "radio_espectral_A")], # ...columnas a mostrar
      row.names = FALSE)                             # ...sin nombres de fila
cat("\n== Comparación a 5 sectores (multiplicador de valor agregado) ==\n") # Imprime un encabezado
print(redondear(comp5[comp5$indicador == indicadores[["valor_agregado"]], -1], 3), row.names = FALSE) # Muestra el bloque del multiplicador de VA
