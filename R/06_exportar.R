# =============================================================================
# 06_exportar.R
# Exportación de resultados a Excel (writexl) y CSV.
# =============================================================================

# -----------------------------------------------------------------------------
# Función auxiliar
# -----------------------------------------------------------------------------

# Convierte una matriz rotulada en data.frame con los nombres de fila como columna
.a_df <- function(m, primera = "fila") {             # Abre la función; recibe la matriz y el nombre de la 1.ª columna
  df <- data.frame(rownames(m),                      # Primera columna: los nombres de fila de la matriz
                   as.data.frame(m, check.names = FALSE), # Resto de columnas: los valores de la matriz
                   check.names = FALSE,              # Conserva los nombres de columna originales
                   stringsAsFactors = FALSE)         # Texto como caracteres
  names(df)[1] <- primera                            # Nombra la primera columna
  df                                                 # Devuelve el data.frame
}                                                    # Cierra la función .a_df

# -----------------------------------------------------------------------------
# Exportación
# -----------------------------------------------------------------------------

#' Exporta la MIP y su análisis
#'
#' @param mip Objeto "mip".
#' @param an Lista devuelta por `analizar_mip()`.
#' @param dir Carpeta de salida.
#' @param prefijo Prefijo de los archivos.
#' @return Rutas de los archivos generados (invisible).
exportar_mip <- function(mip, an, dir = "salidas", prefijo = "mip") { # Abre la función de exportación
  dir.create(dir, showWarnings = FALSE, recursive = TRUE) # Crea la carpeta de salida si no existe
  hojas <- list(                                     # Arma la lista de tablas (una hoja por tabla):
    MIP = .a_df(tabla_mip(mip)),                     #   la MIP completa con sus cuadrantes
    A_coef_tecnicos = .a_df(round(an$A, 6)),         #   coeficientes directos, redondeados a 6 decimales
    L_inversa_Leontief = .a_df(round(an$L, 6)),      #   inversa de Leontief, redondeada a 6 decimales
    multiplicadores = an$multiplicadores,            #   tabla de multiplicadores
    encadenamientos = an$encadenamientos,            #   tabla de encadenamientos
    notas = data.frame(nota = c(                     #   hoja de notas metodológicas:
      paste("Unidad:", mip$unidad),                  #     unidad de medida
      paste("Modelo tecnológico:", mip$modelo,       #     modelo tecnológico...
            ifelse(mip$modelo == "D",                #     ...con su descripción según corresponda
                   "(rama x rama, estructura fija de ventas por producto)", # descripción del modelo D
                   "(producto x producto, tecnología de rama)")),           # descripción del modelo B
      "Flujos domésticos a precios básicos.",        #     valuación y tipo de flujos
      "Supuestos: tasas de margen e impuestos uniformes por producto; proporcionalidad de importaciones en los usos internos; exportaciones de origen nacional.", # supuestos
      "Fuente de los datos: INDEC, Cuadro de oferta y utilización. Elaboración propia." # fuente
    ))                                               #   cierra la hoja de notas
  )                                                  # Cierra la lista de hojas
  rutas <- c()                                       # Inicia el vector de rutas generadas
  if (requireNamespace("writexl", quietly = TRUE)) { # Si está instalado writexl...
    f <- file.path(dir, paste0(prefijo, ".xlsx"))    # ...define la ruta del libro Excel...
    writexl::write_xlsx(hojas, f)                    # ...escribe todas las hojas en un solo libro...
    rutas <- c(rutas, f)                             # ...y registra la ruta
  }                                                  # Cierra el condicional de Excel
  for (h in names(hojas)) {                          # Recorre cada tabla para exportarla también en CSV
    f <- file.path(dir, paste0(prefijo, "_", h, ".csv")) # Define la ruta del CSV de esa tabla
    utils::write.csv(hojas[[h]], f,                  # Escribe la tabla en CSV...
                     row.names = FALSE,              # ...sin nombres de fila...
                     fileEncoding = "UTF-8")         # ...con codificación UTF-8 (acentos)
    rutas <- c(rutas, f)                             # Registra la ruta
  }                                                  # Cierra el ciclo de CSV
  invisible(rutas)                                   # Devuelve las rutas sin imprimirlas
}                                                    # Cierra la función exportar_mip
