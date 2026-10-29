# =============================================================================
# 01_cou_datos.R
# Estructura de datos de un cuadro de oferta y utilización (COU) y datos
# reales del COU 2018 del INDEC en su versión reducida (10 productos x 5 ramas).
#
# Fuente de los datos: INDEC (2021). Cuentas nacionales. Cuadro de oferta y
#   utilización (COU) 2018. Buenos Aires: INDEC (agosto de 2021).
#   https://www.indec.gob.ar/ftp/cuadros/economia/cou_2018_08_21.pdf
#   Millones de pesos corrientes de 2018. Oferta a precios básicos;
#   utilización a precios de comprador.
# =============================================================================

# -----------------------------------------------------------------------------
# Constructor del objeto "cou"
# -----------------------------------------------------------------------------

#' Construye un objeto COU con validación de dimensiones
#'
#' @param prod Matriz productos x ramas: producción a precios básicos
#'   (bloque de producción del cuadro de oferta, "V transpuesta").
#' @param imp Vector por producto: importaciones CIF.
#' @param aj_cif_fob Vector por producto: ajuste CIF/FOB (negativo en los
#'   servicios de flete y seguro prestados por residentes).
#' @param margenes Vector por producto, o matriz productos x tipos de margen
#'   (p. ej., márgenes de distribución y comisiones por cambio de divisas):
#'   positivos en los bienes, negativos en los productos que prestan el
#'   servicio; cada tipo suma cero.
#' @param impuestos Vector por producto: impuestos netos de subsidios sobre
#'   los productos.
#' @param u_int Matriz productos x ramas: consumo intermedio a precios de
#'   comprador (incluye insumos nacionales e importados).
#' @param y Matriz productos x usos finales a precios de comprador.
#' @param vab Vector por rama: valor agregado bruto a precios básicos.
#' @param col_export Nombre de la columna de `y` con las exportaciones.
#' @param ajuste_territorial Matriz (filas de ajuste x usos finales), opcional.
#'   La PRIMERA fila debe ser "compras de residentes en el exterior"; las
#'   siguientes, otros ajustes (p. ej., compras de no residentes en el país).
#' @param imp_territorial Escalar: importaciones registradas por el ajuste
#'   territorial (compras de residentes en el exterior).
#' @param unidad Texto descriptivo de la unidad de medida.
#' @return Lista de clase "cou".
nuevo_cou <- function(prod, imp, aj_cif_fob, margenes, impuestos,  # Abre la función con los bloques del cuadro de oferta
                      u_int, y, vab, col_export,                     # ...los bloques del cuadro de utilización
                      ajuste_territorial = NULL, imp_territorial = 0,  # ...los ajustes territoriales opcionales
                      unidad = "millones de pesos corrientes") {    # ...y la unidad de medida
  np <- nrow(prod)                                   # Cuenta los productos (filas del bloque de producción)
  nr <- ncol(prod)                                   # Cuenta las ramas (columnas del bloque de producción)
  stopifnot(                                         # Detiene la ejecución si alguna condición de forma no se cumple:
    is.matrix(prod), is.matrix(u_int), is.matrix(y), #   los bloques deben ser matrices
    nrow(u_int) == np, ncol(u_int) == nr,            #   el consumo intermedio debe ser productos x ramas
    nrow(y) == np,                                   #   la demanda final debe tener una fila por producto
    length(imp) == np, length(aj_cif_fob) == np,     #   importaciones y ajuste CIF/FOB: un valor por producto
    NROW(margenes) == np, length(impuestos) == np,   #   márgenes (vector o matriz) e impuestos: una fila por producto
    length(vab) == nr,                               #   valor agregado: un valor por rama
    col_export %in% colnames(y)                      #   la columna de exportaciones debe existir en la demanda final
  )                                                  # Cierra los controles de forma
  productos <- rownames(prod)                        # Toma los nombres de los productos del bloque de producción
  ramas <- colnames(prod)                            # Toma los nombres de las ramas del bloque de producción
  dimnames(u_int) <- list(productos, ramas)          # Rotula el consumo intermedio con productos y ramas
  rownames(y) <- productos                           # Rotula las filas de la demanda final con los productos
  names(imp) <- productos                            # Rotula las importaciones por producto
  names(aj_cif_fob) <- productos                     # Rotula el ajuste CIF/FOB por producto
  if (is.matrix(margenes)) {                         # Si los márgenes vienen por tipo (matriz)...
    rownames(margenes) <- productos                  # ...rotula sus filas con los productos
  } else {                                           # Si vienen en un solo vector...
    names(margenes) <- productos                     # ...rotula cada elemento con su producto
  }                                                  # Cierra el condicional de márgenes
  names(impuestos) <- productos                      # Rotula los impuestos por producto
  names(vab) <- ramas                                # Rotula el valor agregado por rama
  structure(list(                                    # Arma la lista que contiene todos los bloques del COU
    prod = prod, imp = imp, aj_cif_fob = aj_cif_fob, # Guarda producción, importaciones y ajuste CIF/FOB
    margenes = margenes, impuestos = impuestos,      # Guarda márgenes e impuestos sobre productos
    u_int = u_int, y = y, vab = vab,                 # Guarda consumo intermedio, demanda final y VAB
    col_export = col_export,                         # Guarda el nombre de la columna de exportaciones
    ajuste_territorial = ajuste_territorial,         # Guarda los ajustes territoriales (o NULL)
    imp_territorial = imp_territorial,               # Guarda las importaciones del ajuste territorial
    unidad = unidad                                  # Guarda la unidad de medida
  ), class = "cou")                                  # Asigna la clase "cou" a la lista
}                                                    # Cierra la función nuevo_cou

# -----------------------------------------------------------------------------
# Datos reales: COU 2018 del INDEC, versión reducida
# -----------------------------------------------------------------------------

#' COU 2018 del INDEC, versión reducida (10 productos CPC x 5 ramas CIIU)
#'
#' Reproduce los cuadros resumidos publicados en INDEC (2021). Sirve para
#' probar el código con datos reales mientras se configura la lectura del
#' COU completo (107 ramas x 223 productos, archivo sh_cou_08_21.xls).
#'
#' @return Objeto de clase "cou".
cou_indec_2018_reducido <- function() {              # Abre la función que devuelve el COU 2018 reducido
  productos <- c(                                    # Define los nombres de los 10 grupos de productos (CPC):
    "P0 Agricultura, silvicultura y pesca",          #   sección 0 de la CPC
    "P1 Minerales; electricidad, gas y agua",        #   sección 1
    "P2 Alimentos, bebidas, tabaco y textiles",      #   sección 2
    "P3 Otros bienes transportables",                #   sección 3
    "P4 Productos metálicos, maquinaria y equipo",   #   sección 4
    "P5 Construcción",                               #   sección 5
    "P6 Comercio, alojamiento, transporte y comunicaciones", # sección 6 (presta los márgenes de distribución)
    "P7 Servicios financieros e inmobiliarios",      #   sección 7
    "P8 Servicios a empresas y de producción",       #   sección 8
    "P9 Servicios comunitarios, sociales y personales" # sección 9
  )                                                  # Cierra el vector de productos
  ramas <- c(                                        # Define los nombres de las 5 ramas de actividad (CIIU):
    "R1 Agro y pesca",                               #   agricultura, ganadería, silvicultura y pesca
    "R2 Minería, industria, energía y construcción", #   sectores productores de bienes no agropecuarios
    "R3 Comercio, transporte, comunicaciones y alojamiento", # servicios de distribución
    "R4 Finanzas, inmobiliarias y servicios empresariales",  # servicios a empresas
    "R5 Educación, salud, gobierno y servicios personales"   # servicios sociales y gobierno
  )                                                  # Cierra el vector de ramas

  # Cuadro de oferta: producción a precios básicos (productos x ramas)
  prod <- matrix(c(                                  # Crea la matriz de producción, cargada por filas:
    1393550,   45163,    1824,       0,       0,     #   P0: producción de cada rama
          0,  826411,       0,       0,       0,     #   P1
          0, 2434274,    3463,       0,       0,     #   P2
          0, 2138567,     101,       0,       0,     #   P3
          0, 1424087,      90,       0,       0,     #   P4
      23147, 1335936,    4287,   25076,       0,     #   P5
          0,  601676, 4610980,       0,       0,     #   P6
          0,   15388,   40915, 1803649,    6668,     #   P7
      84916,  363050, 1250119, 1183589,   22970,     #   P8
          0,       0,    3585,       0, 4285824      #   P9
  ), nrow = 10, byrow = TRUE,                        # Indica 10 filas y lectura por filas
  dimnames = list(productos, ramas))                 # Rotula filas (productos) y columnas (ramas)

  imp <- c(93849, 114453, 105035, 550694, 960224,    # Importaciones CIF de P0 a P4
           982, 360209, 103623, 115870, 54397)       # Importaciones CIF de P5 a P9
  aj  <- c(0, 0, 0, 0, 0, 0, -75361, -7387, 0, 0)    # Ajuste CIF/FOB (fletes y seguros de residentes en P6 y P7)
  mg  <- c(221143, 24752, 987402, 780886, 617517,    # Márgenes de distribución sobre los bienes P0 a P4
           0, -2631701, 0, 0, 0)                     # P6 registra el margen con signo negativo (lo presta)
  tx  <- c(82721, 29369, 663254, 346641, 270037,     # Impuestos netos de subsidios sobre P0 a P4
           111988, 271788, 140632, 204965, 145291)   # Impuestos netos de subsidios sobre P5 a P9

  # Cuadro de utilización a precios de comprador: consumo intermedio (productos x ramas)
  u_int <- matrix(c(                                 # Crea la matriz de consumo intermedio, cargada por filas:
    146986,  881402,  85385,   2132,   4838,         #   compras de P0 por cada rama
         0,  812308,  14928,   3910,   1966,         #   compras de P1
     92726,  383651, 280539,   8323,  75871,         #   compras de P2
    208134, 1351897, 434673, 117907, 206918,         #   compras de P3
     23284, 1108623, 238455,  41627,  39033,         #   compras de P4
         0,   14274,  54095,  54599,  43103,         #   compras de P5
    166422,  392943, 398974,  79716, 156702,         #   compras de P6
     17186,  154492, 370524, 218979, 189305,         #   compras de P7
    174861,  576300, 736753, 415155, 301878,         #   compras de P8
      2914,   11254,  46247,  45797, 263195          #   compras de P9
  ), nrow = 10, byrow = TRUE)                        # Indica 10 filas y lectura por filas

  usos_finales <- c("Exportaciones", "Consumo privado", "Consumo público", # Nombres de los usos finales (1 a 3)
                    "FBCF", "Variación de existencias", "Objetos valiosos") # Nombres de los usos finales (4 a 6)
  y <- matrix(c(                                     # Crea la matriz de demanda final, cargada por filas:
    323824,  270672,       0,   11953,  111059, 0,   #   destinos finales de P0
    126841,   36071,       0,       0,   -1038, 0,   #   destinos finales de P1
    661900, 2575811,       0,       0,  114606, 0,   #   destinos finales de P2
    276887, 1209745,     447,   20341,  -10059, 0,   #   destinos finales de P3
    321381,  635539,    2000,  875634,  -13621, 0,   #   destinos finales de P4
       661,       0,       0, 1334685,       0, 0,   #   destinos finales de P5
    147430, 1795405,       0,       0,       0, 0,   #   destinos finales de P6
     17894, 1126575,    8534,       0,       0, 0,   #   destinos finales de P7
    178063,  808591,   27751,    6126,       0, 0,   #   destinos finales de P8
     33403, 1794587, 2291699,       0,       0, 0    #   destinos finales de P9
  ), nrow = 10, byrow = TRUE,                        # Indica 10 filas y lectura por filas
  dimnames = list(productos, usos_finales))          # Rotula filas (productos) y columnas (usos finales)

  vab <- c(669102, 3497409, 3254792, 2024167, 3032654) # Valor agregado bruto a precios básicos por rama

  # Ajuste territorial del consumo de los hogares
  aj_terr <- matrix(0, nrow = 2,                     # Crea una matriz de ceros con dos filas de ajuste
                    ncol = length(usos_finales),     # ...y una columna por uso final
                    dimnames = list(c("Compras de residentes en el exterior",  # Rotula la fila 1 (importación de consumo)
                                      "Compras de no residentes en el país"),  # Rotula la fila 2 (exportación de consumo)
                                    usos_finales))   # Rotula las columnas con los usos finales
  aj_terr[1, "Consumo privado"] <- 30627             # Suma al consumo privado lo que los residentes compran en el exterior
  aj_terr[2, "Exportaciones"]   <- 40377             # Suma a las exportaciones lo que los no residentes compran en el país
  aj_terr[2, "Consumo privado"] <- -40377            # Resta ese mismo monto del consumo privado (no es de residentes)

  nuevo_cou(prod, imp, aj, mg, tx, u_int, y, vab,    # Construye el objeto "cou" con todos los bloques
            col_export = "Exportaciones",            # Indica cuál es la columna de exportaciones
            ajuste_territorial = aj_terr,            # Incorpora los ajustes territoriales
            imp_territorial = 30627,                 # Registra las importaciones del ajuste territorial
            unidad = "millones de pesos corrientes de 2018") # Declara la unidad de medida
}                                                    # Cierra la función cou_indec_2018_reducido
