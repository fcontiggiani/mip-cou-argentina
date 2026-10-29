# =============================================================================
# test-mip.R
# Pruebas del proyecto. Ejecutar desde la raíz: Rscript tests/testthat.R
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Ejemplo de la cadena del pan (clase 14)
# -----------------------------------------------------------------------------
test_that("ejemplo de la cadena del pan (clase 14): A y L conocidas", { # Abre la prueba del ejemplo didáctico
  Z <- matrix(c(10, 100, 0,                          # Ventas de la agricultura a cada sector
                20,   0, 150,                        # Ventas de los molinos a cada sector
                 0,   0, 0),                         # Las panaderías no venden insumos
              3, byrow = TRUE)                       # Matriz 3 x 3 cargada por filas
  x <- c(110, 170, 200)                              # Valor bruto de producción de cada sector
  A <- coeficientes_tecnicos(Z, x)                   # Calcula los coeficientes técnicos
  L <- inversa_leontief(A)                           # Calcula la inversa de Leontief
  expect_equal(A[1, 2], 100 / 170)                   # El coeficiente trigo/harina debe ser 100/170
  expect_equal(unname(L[, 3]), c(0.55, 0.85, 1),     # La columna del pan debe ser (0,55; 0,85; 1)...
               tolerance = 1e-10)                    # ...con precisión numérica
  expect_equal(unname(colSums(L)),                   # Los multiplicadores de producción deben ser...
               c(1.473333, 1.866667, 2.4),           # ...1,47; 1,87 y 2,40...
               tolerance = 1e-6)                     # ...con 6 decimales de precisión
  expect_equal(unname(as.vector(L %*% c(0, 0, 10))), # El impacto de +10 de pan...
               c(5.5, 8.5, 10), tolerance = 1e-10)   # ...debe ser (5,5; 8,5; 10)
})                                                   # Cierra la prueba

# -----------------------------------------------------------------------------
# 2. Consistencia del COU 2018 reducido
# -----------------------------------------------------------------------------
test_that("el COU 2018 reducido del INDEC es consistente", { # Abre la prueba de los datos del INDEC
  cou <- cou_indec_2018_reducido()                   # Carga el COU 2018 reducido
  ctrl <- capture.output(res <- validar_cou(cou))    # Valida el COU sin mostrar la salida en pantalla
  expect_true(all(res$ok))                           # Todos los controles contables deben aprobarse
  expect_equal(unname(attr(res, "pib")["produccion"]), # El PIB por la producción debe ser...
               14744811,                             # ...el publicado por el INDEC...
               tolerance = 5 / 14744811)             # ...con un margen de redondeo de 5 millones
})                                                   # Cierra la prueba

# -----------------------------------------------------------------------------
# 3. Valuación a precios básicos
# -----------------------------------------------------------------------------
test_that("la valuación a precios básicos reproduce la oferta pb por producto", { # Abre la prueba del paso 1
  cou <- cou_indec_2018_reducido()                   # Carga el COU 2018 reducido
  pb <- a_precios_basicos(cou)                       # Lleva la utilización a precios básicos
  oferta_pb <- rowSums(cou$prod) + cou$imp + cou$aj_cif_fob # Oferta a precios básicos publicada, por producto
  expect_lt(max(abs(rowSums(pb$u_pb) - oferta_pb)), 5) # La utilización pb debe igualar la oferta pb (±5)
  # La reasignación de márgenes conserva el total de cada columna, salvo el
  # redondeo de los márgenes publicados (suman -1 en lugar de 0)
  expect_lt(max(abs(colSums(pb$u_pb) +               # Total por columna a precios básicos...
                      colSums(pb$t_mat) -            # ...más impuestos...
                      colSums(cbind(cou$u_int, cou$y)))), # ...menos total a precios de comprador...
            5)                                       # ...debe ser casi cero
})                                                   # Cierra la prueba

# -----------------------------------------------------------------------------
# 4. Identidades de la MIP en ambos modelos tecnológicos
# -----------------------------------------------------------------------------
for (mod in c("D", "B")) {                           # Repite las pruebas para los modelos D y B
  test_that(paste("MIP modelo", mod, ": identidades, no negatividad y multiplicadores"), { # Abre la prueba del modelo
    cou <- cou_indec_2018_reducido()                 # Carga el COU 2018 reducido
    mip <- cou_a_mip(cou, mod)                       # Construye la MIP con el modelo correspondiente
    invisible(capture.output(ctrl <- validar_mip(mip))) # Valida la MIP sin mostrar la salida
    expect_true(all(ctrl$ok))                        # Todos los controles de la MIP deben aprobarse
    an <- analizar_mip(mip)                          # Calcula A, L, multiplicadores y encadenamientos
    expect_true(all(an$L >= 0))                      # La inversa de Leontief debe ser no negativa
    expect_equal(an$multiplicadores$control_suma_1,  # (v + m + t)'L debe sumar 1...
                 rep(1, length(mip$x)),              # ...en cada sector...
                 tolerance = 1e-5)                   # ...salvo redondeo
    f <- rowSums(mip$F)                              # Demanda final doméstica total por sector
    v <- mip$primarios["Valor agregado bruto pb", seq_along(mip$x)] / mip$x # Coeficientes de valor agregado
    expect_equal(sum(v * as.vector(an$L %*% f)),     # Como L f = x, el VAB generado por la demanda final...
                 sum(cou$vab), tolerance = 1e-8)     # ...debe igualar el VAB publicado
    expect_equal(mean(an$encadenamientos$BL), 1)     # Por construcción, el BL promedio es 1
    expect_equal(mean(an$encadenamientos$FL_ghosh), 1) # Por construcción, el FL de Ghosh promedio es 1
  })                                                 # Cierra la prueba del modelo
}                                                    # Cierra el ciclo sobre los modelos

# -----------------------------------------------------------------------------
# 5. Equivalencia de los modelos sin producción secundaria
# -----------------------------------------------------------------------------
test_that("con producción sin productos secundarios, los modelos B y D coinciden", { # Abre la prueba de equivalencia
  prod <- diag(c(100, 200, 150))                     # Cada rama produce un único producto (V diagonal)
  dimnames(prod) <- list(paste0("P", 1:3), paste0("R", 1:3)) # Rotula productos y ramas
  u_int <- matrix(c(10, 40, 5,                       # Compras intermedias del producto 1
                    20, 30, 30,                      # Compras intermedias del producto 2
                     5, 10, 15),                     # Compras intermedias del producto 3
                  3, byrow = TRUE)                   # Matriz 3 x 3 cargada por filas
  ci <- colSums(u_int)                               # Consumo intermedio de cada rama
  y_tot <- c(100, 200, 150) - rowSums(u_int)         # Demanda final que cierra el balance de cada producto
  y <- cbind(Exportaciones = 0.2 * y_tot,            # Asigna el 20 % de la demanda final a exportaciones...
             Consumo = 0.8 * y_tot)                  # ...y el 80 % a consumo
  cou <- nuevo_cou(prod,                             # Construye un COU sintético con esa producción...
                   imp = rep(0, 3),                  # ...sin importaciones...
                   aj_cif_fob = rep(0, 3),           # ...sin ajuste CIF/FOB...
                   margenes = rep(0, 3),             # ...sin márgenes...
                   impuestos = rep(0, 3),            # ...sin impuestos...
                   u_int = u_int, y = y,             # ...con estos usos intermedios y finales...
                   vab = c(100, 200, 150) - ci,      # ...con el VAB que cierra cada rama...
                   col_export = "Exportaciones")     # ...e indicando la columna de exportaciones
  zb <- cou_a_mip(cou, "B")$Z                        # Matriz Z con el modelo B
  zd <- cou_a_mip(cou, "D")$Z                        # Matriz Z con el modelo D
  expect_equal(unname(zb), unname(zd))               # Ambos modelos deben dar el mismo resultado
  expect_equal(unname(zd), unname(u_int))            # Y ese resultado debe ser el consumo intermedio original
})                                                   # Cierra la prueba

# -----------------------------------------------------------------------------
# 6. Agregación de la MIP y correspondencia de ramas
# -----------------------------------------------------------------------------
test_that("agregar con un grupo por sector deja la MIP intacta", { # Abre la prueba de agregación trivial
  mip <- cou_a_mip(cou_indec_2018_reducido(), "D")   # Construye la MIP de 5 ramas
  mip_ag <- agregar_mip(mip, names(mip$x))           # La agrega usando cada sector como su propio grupo
  expect_equal(unname(mip_ag$Z), unname(mip$Z[order(names(mip$x)), order(names(mip$x))])) # Z no cambia (salvo el orden)
  expect_equal(sum(mip_ag$primarios), sum(mip$primarios)) # Los insumos primarios totales se conservan
})                                                   # Cierra la prueba

test_that("la agregación conserva el VBP y las identidades", { # Abre la prueba de agregación a 2 grupos
  mip <- cou_a_mip(cou_indec_2018_reducido(), "D")   # Construye la MIP de 5 ramas
  mip2 <- agregar_mip(mip, c("bienes", "bienes", "servicios", "servicios", "servicios")) # Agrega a bienes y servicios
  expect_equal(sum(mip2$x), sum(mip$x))              # El VBP total se conserva
  invisible(capture.output(ctrl <- validar_mip(mip2))) # Valida la MIP agregada sin imprimir
  expect_true(all(ctrl$ok))                          # La MIP agregada cumple sus identidades
})                                                   # Cierra la prueba

test_that("la correspondencia CIIU asigna las divisiones a los 5 grandes sectores", { # Abre la prueba de correspondencia
  g <- grupos_ramas_ciiu(c("011/014", "050", "101", "45", "502", "644", "651 2", "74", "75", "950")) # Clasifica códigos de borde
  expect_equal(substr(g, 1, 2), c("R1", "R1", "R2", "R2", "R3", "R3", "R4", "R4", "R5", "R5")) # Verifica cada asignación
})                                                   # Cierra la prueba

# -----------------------------------------------------------------------------
# 7. Márgenes de varios tipos
# -----------------------------------------------------------------------------
test_that("márgenes en matriz de una columna equivalen al vector", { # Abre la prueba de márgenes matriciales
  cou <- cou_indec_2018_reducido()                   # Carga el COU con márgenes en vector
  cou_m <- cou                                       # Copia el COU
  cou_m$margenes <- matrix(cou$margenes, ncol = 1,   # Convierte los márgenes en una matriz de una columna...
                           dimnames = list(names(cou$margenes), "Mg D")) # ...rotulada
  expect_equal(a_precios_basicos(cou_m)$u_pb, a_precios_basicos(cou)$u_pb) # La valuación a precios básicos no cambia
})                                                   # Cierra la prueba

test_that("el ajuste CIF/FOB se descuenta primero de las importaciones", { # Abre la prueba del ajuste CIF/FOB
  cou <- cou_indec_2018_reducido()                   # Carga el COU reducido (ajuste menor que las importaciones)
  imp <- separar_importaciones(cou, a_precios_basicos(cou)$u_pb) # Separa usos domésticos e importados
  expect_equal(sum(imp$aj_residentes), 0)            # Todo el ajuste se descuenta de las importaciones
  expect_lt(max(abs(rowSums(imp$u_dom) - rowSums(cou$prod))), 5) # El uso doméstico iguala la producción nacional
})                                                   # Cierra la prueba

# -----------------------------------------------------------------------------
# 8. Correspondencia CIIU Rev. 3 -> Rev. 4
# -----------------------------------------------------------------------------
test_that("la correspondencia cubre las 107 ramas con secciones Rev. 4 válidas", { # Abre la prueba de la correspondencia
  corr <- leer_correspondencia_ciiu4(file.path(raiz, "datos", "correspondencia_ciiu3_ciiu4.csv")) # Lee la tabla
  expect_equal(nrow(corr), 107)                      # Debe haber una fila por rama del COU
  expect_true(all(corr$seccion_rev4 %in% LETTERS[1:21])) # Las secciones deben ser letras A a U
  expect_true(all(corr$correspondencia %in% c("exacta", "aproximada"))) # Solo dos tipos de correspondencia
  expect_true(all(nzchar(corr$nota[corr$correspondencia == "aproximada"]))) # Toda asignación aproximada lleva nota
  expect_equal(corr$seccion_rev4[corr$codigo_rev3 == "1511"], "C") # Los frigoríficos son industria manufacturera (C)
  expect_equal(substr(grandes_sectores_ciiu4("1511", corr), 1, 2), "G2") # ...y van al gran sector de industria
  expect_equal(substr(grandes_sectores_ciiu4("111", corr), 1, 2), "G3")  # El petróleo va a minería, energía y construcción
})                                                   # Cierra la prueba
