# =============================================================================
# 03_validaciones.R
# Controles de consistencia contable del COU y de la MIP.
# =============================================================================

# -----------------------------------------------------------------------------
# Funciones auxiliares
# -----------------------------------------------------------------------------

# Tolerancia: los cuadros publicados están redondeados a millones; se admite
# una discrepancia absoluta pequeña o relativa de 1e-6 del total.
.tol <- function(total, abs_min = 5) {               # Abre la función de tolerancia
  max(abs_min, 1e-6 * abs(total))                    # Toma el mayor entre 5 unidades y una millonésima del total
}                                                    # Cierra la función .tol

# Arma una fila de la tabla de controles
.fila_control <- function(nombre, dif, total) {      # Abre la función; recibe nombre, diferencias y total de referencia
  data.frame(control = nombre,                       # Nombre del control
             max_dif_abs = max(abs(dif)),            # Mayor diferencia absoluta encontrada
             tolerancia = .tol(total),               # Tolerancia admitida para ese control
             ok = max(abs(dif)) <= .tol(total),      # TRUE si la diferencia está dentro de la tolerancia
             stringsAsFactors = FALSE)               # Texto como caracteres, no factores
}                                                    # Cierra la función .fila_control

# -----------------------------------------------------------------------------
# Validación del COU
# -----------------------------------------------------------------------------

#' Valida las identidades contables de un COU
#'
#' 1. Oferta a precios de comprador = utilización a precios de comprador.
#' 2. Producción de cada rama = consumo intermedio pc + VAB pb.
#' 3. Los márgenes suman cero.
#' 4. PIB por producción (VAB + impuestos sobre productos) = PIB por gasto.
#'
#' @param cou Objeto de clase "cou".
#' @param detener Si TRUE, detiene la ejecución ante un control fallido.
#' @return data.frame con los controles (invisible); además lo imprime.
validar_cou <- function(cou, detener = TRUE) {       # Abre la función de validación del COU
  pb <- rowSums(cou$prod) + cou$imp + cou$aj_cif_fob # Oferta a precios básicos: producción + importaciones + ajuste
  mg <- as.matrix(cou$margenes)                      # Márgenes como matriz (una columna por tipo de margen)
  pc <- pb + rowSums(mg) + cou$impuestos             # Oferta a precios de comprador: suma márgenes e impuestos
  uso <- rowSums(cou$u_int) + rowSums(cou$y)         # Utilización total a precios de comprador, por producto
  g <- colSums(cou$prod)                             # Producción de cada rama (suma por columnas)
  aj_t <- if (is.null(cou$ajuste_territorial)) 0 else sum(cou$ajuste_territorial) # Total neto del ajuste territorial
  pib_prod <- sum(cou$vab) + sum(cou$impuestos)      # PIB por la producción: VAB pb + impuestos sobre productos
  pib_gasto <- sum(cou$y) + aj_t -                   # PIB por el gasto: demanda final + ajuste territorial...
    (sum(cou$imp) + sum(cou$aj_cif_fob) +            # ...menos importaciones (CIF ajustadas a FOB)...
       cou$imp_territorial)                          # ...y compras de residentes en el exterior

  ctrl <- rbind(                                     # Apila los controles en una tabla:
    .fila_control("Oferta pc = utilización pc (por producto)", pc - uso, sum(pc)),   # equilibrio por producto
    .fila_control("Producción = CI pc + VAB (por rama)",                             # equilibrio por rama...
                  g - colSums(cou$u_int) - cou$vab, sum(g)),                         # ...producción menos costos
    .fila_control("Márgenes suman cero (cada tipo)", colSums(mg), sum(abs(mg))),   # márgenes netos nulos por tipo
    .fila_control("PIB producción = PIB gasto", pib_prod - pib_gasto, pib_prod)      # igualdad de los dos enfoques
  )                                                  # Cierra la tabla de controles
  attr(ctrl, "pib") <- c(produccion = pib_prod, gasto = pib_gasto) # Adjunta el PIB por ambos enfoques
  print(ctrl, row.names = FALSE)                     # Muestra la tabla de controles
  cat(sprintf("PIB (enfoque de la producción): %s %s\n", # Imprime el PIB con formato argentino...
              format(round(pib_prod), big.mark = ".", decimal.mark = ","), # ...punto de miles y coma decimal
              cou$unidad))                           # ...seguido de la unidad de medida
  if (detener && !all(ctrl$ok)) {                    # Si se pidió detener y algún control falló...
    stop("El COU no supera los controles de consistencia.") # ...detiene la ejecución
  }                                                  # Cierra el condicional
  invisible(ctrl)                                    # Devuelve la tabla sin volver a imprimirla
}                                                    # Cierra la función validar_cou

# -----------------------------------------------------------------------------
# Validación de la MIP
# -----------------------------------------------------------------------------

#' Valida una MIP simétrica
#'
#' Controla: (i) filas: Z 1 + F 1 = x; (ii) columnas: 1'Z + insumos primarios
#' = x; (iii) no negatividad de Z; (iv) condición de Hawkins-Simon
#' (radio espectral de A menor que 1).
#'
#' @param mip Lista devuelta por `cou_a_mip()`.
#' @param detener Si TRUE, detiene la ejecución ante un control fallido.
#' @return data.frame con los controles (invisible).
validar_mip <- function(mip, detener = TRUE) {       # Abre la función de validación de la MIP
  x <- mip$x                                         # Valor bruto de producción de cada sector
  filas <- rowSums(mip$Z) + rowSums(mip$F) - x       # Diferencia por filas: ventas intermedias + finales - VBP
  cols <- colSums(mip$Z) +                           # Diferencia por columnas: insumos intermedios domésticos...
    colSums(mip$primarios[, seq_along(x), drop = FALSE]) - # ...más insumos primarios (importaciones, impuestos, VAB)...
    x                                                # ...menos VBP
  A <- coeficientes_tecnicos(mip$Z, x)               # Calcula la matriz de coeficientes técnicos
  rho <- max(Mod(eigen(A, only.values = TRUE)$values)) # Radio espectral de A (mayor autovalor en módulo)
  ctrl <- rbind(                                     # Apila los controles en una tabla:
    .fila_control("Filas: Z1 + F1 = x", filas, sum(x)), # identidad por filas
    .fila_control("Columnas: 1'Z + insumos primarios = x", cols, sum(x)), # identidad por columnas
    data.frame(control = "Z no negativa",            # control de no negatividad:
               max_dif_abs = -min(0, min(mip$Z)),    #   magnitud del valor más negativo (0 si no hay)
               tolerancia = .tol(sum(x)),            #   tolerancia de redondeo
               ok = min(mip$Z) >= -.tol(sum(x)),     #   TRUE si no hay negativos significativos
               stringsAsFactors = FALSE),            #   texto como caracteres
    data.frame(control = "Hawkins-Simon: radio espectral de A < 1", # condición de Hawkins-Simon:
               max_dif_abs = rho,                    #   valor del radio espectral
               tolerancia = 1,                       #   umbral teórico
               ok = rho < 1,                         #   TRUE si la inversa de Leontief existe y es no negativa
               stringsAsFactors = FALSE)             #   texto como caracteres
  )                                                  # Cierra la tabla de controles
  print(ctrl, row.names = FALSE)                     # Muestra la tabla de controles
  if (detener && !all(ctrl$ok)) {                    # Si se pidió detener y algún control falló...
    stop("La MIP no supera los controles de consistencia.") # ...detiene la ejecución
  }                                                  # Cierra el condicional
  invisible(ctrl)                                    # Devuelve la tabla sin volver a imprimirla
}                                                    # Cierra la función validar_mip
