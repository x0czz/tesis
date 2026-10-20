# =====================================================================
# Base limpia: percepción de igualdad de oportunidades (EDUMER, olas 1 y 2)
# Ejecutar desde la raíz del proyecto (carpeta tesis).
# Entrada : input/edumer_students_long.sav
# Salida  : input/datos_igualdad_oportunidades.RData
#           input/diccionario_datos_igualdad_oportunidades.csv
# Formato : largo (una fila por estudiante y ola)
# Códigos 88 (No sabe) y 99 (No responde) recodificados como NA.
# =====================================================================
suppressPackageStartupMessages({ library(dplyr); library(haven) })

d <- read_sav(file.path("input", "edumer_students_long.sav")) |>
  mutate(d3_def = toupper(as.character(as_factor(d3_def)))) |>   # curso de ola 1 como texto (p. ej., "6A")
  zap_labels() |> as.data.frame()
na_ns <- function(x) ifelse(x %in% c(88, 99), NA, x)
lab <- function(x, l) { attr(x, "label") <- l; x }

# --- Escuela: la ola 1 usa códigos distintos a la ola 2; se homologa a 9 escuelas ---
map_w1 <- c(`2`=1, `3`=2, `4`=3, `5`=4, `7`=5, `8`=6, `10`=7, `11`=8, `12`=9)
nombres_esc <- c("Colegio Nuestra Señora del Carmen (Maipú)", "Colegio San Alberto (Estación Central)",
                 "Colegio Santa Isabel de Hungría (La Cisterna)", "Liceo Reina de Dinamarca (Maipú)",
                 "Colegio San Francisco de Sales (Maipú)", "Liceo Miguel Rafael Prado (Independencia)",
                 "Instituto del Puerto (San Antonio)", "Liceo Santa Teresa de Los Andes (Valparaíso)",
                 "Colegio San Alberto Hurtado (Quilicura)")

# --- País de nacimiento: solo se preguntó en ola 1; se asigna a la ola 2 por estudiante ---
nac <- d |> filter(ola == 1) |> transmute(id_estudiante, extranjero_w1 = as.integer(p22_1 == 2))

# --- Curso: solo está registrado en la ola 1 (la ola 2 registra escuela y nivel, no la sección).
#     Se toma el curso de la ola 1 y se asigna a ambas observaciones del estudiante.
#     Es la unidad en que se aleatorizó el experimento. Los 35 estudiantes que solo
#     participan en la ola 2 quedan sin curso (NA).
cur <- d |>
  filter(ola == 1) |>
  transmute(id_estudiante,
            esc_w1   = map_w1[as.character(d2)],
            curso_w1 = paste0("E", esc_w1, "-", d3_def),
            media_w1 = as.integer(nivel_estudiante %in% 3:4))
niveles_curso <- cur |> distinct(esc_w1, curso_w1) |> arrange(esc_w1, curso_w1) |> pull(curso_w1)
cur <- cur |>
  mutate(id_curso = match(curso_w1, niveles_curso),
         estrato  = paste0("E", esc_w1, "-", ifelse(media_w1 == 1, "media", "basica")))
niveles_estrato <- sort(unique(cur$estrato))

db <- d |>
  left_join(nac, by = "id_estudiante") |>
  left_join(cur, by = "id_estudiante") |>
  transmute(
    id_estudiante = lab(id_estudiante, "Identificador del estudiante"),
    ola     = lab(as.integer(ola), "Ola (1 = 2023-2024, 2 = 2024)"),
    ola2    = lab(as.integer(ola == 2), "Segunda ola (1 = sí)"),
    escuela = lab(factor(ifelse(ola == 1, map_w1[as.character(d2)], d2), levels = 1:9, labels = nombres_esc),
                  "Establecimiento (homologado entre olas)"),
    id_escuela = lab(as.integer(ifelse(ola == 1, map_w1[as.character(d2)], d2)),
                     "Identificador del establecimiento (1-9, homologado entre olas)"),
    id_curso = lab(as.integer(id_curso),
                   "Identificador del curso en ola 1 (1-33); se repite en ola 2. NA si solo participa en ola 2"),
    curso    = lab(factor(curso_w1, levels = niveles_curso),
                   "Curso en ola 1 (E<escuela>-<nivel y letra>); se repite en ola 2"),
    id_estrato = lab(as.integer(match(estrato, niveles_estrato)),
                     "Estrato del experimento: escuela x cohorte en ola 1 (1-18)"),
    estrato  = lab(factor(estrato, levels = niveles_estrato),
                   "Estrato del experimento: escuela x cohorte en ola 1"),
    cohorte_media = lab(as.integer(ifelse(ola == 1, nivel_estudiante %in% 3:4, nivel_estudiante == 2)),
                        "Cohorte de enseñanza media (1) vs. básica (0)"),
    nivel_estudiante = lab(factor(ifelse(ola == 1, nivel_estudiante, nivel_estudiante + 4), levels = 1:6,
                                  labels = c("6° básico", "7° básico", "1° medio", "2° medio", "7° básico", "2° medio")),
                           "Nivel del estudiante al momento de la encuesta"),

    # Variable dependiente
    p1_9 = lab(na_ns(p1_9), "En Chile, todas las personas tienen las mismas oportunidades para salir adelante (1-4)"),
    p1_9_ord = lab(factor(na_ns(p1_9), levels = 1:4, ordered = TRUE,
                          labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo")),
                   "Igualdad de oportunidades percibida (ordinal)"),
    p1_9_acuerdo = lab(as.integer(na_ns(p1_9) >= 3), "Igualdad de oportunidades: de acuerdo o muy de acuerdo (1)"),
    p1_10 = lab(na_ns(p1_10), "En Chile, todas las personas obtienen lo que merecen (1-4)"),

    # Meritocracia escolar
    p2_1 = lab(na_ns(p2_1), "En esta escuela, quienes se esfuerzan obtienen buenas notas (1-4)"),
    p2_2 = lab(na_ns(p2_2), "En esta escuela, quienes son inteligentes obtienen buenas notas (1-4)"),
    p2_3 = lab(na_ns(p2_3), "En esta escuela, los/as estudiantes obtienen las notas que merecen (1-4)"),

    # Nivel socioeconómico
    educ_madre = lab(na_ns(p26), "Educación de la madre (1 = básica incompleta ... 6 = posgrado)"),
    educ_padre = lab(na_ns(p27), "Educación del padre (1 = básica incompleta ... 6 = posgrado)"),
    libros = lab(na_ns(p30), "Libros en el hogar (1 = 0-10 ... 6 = más de 500)"),

    # Rendimiento y horizonte educativo
    notas = lab(p4, "Promedio de notas del año anterior (1,0-7,0)"),
    aspiracion = lab(factor(na_ns(p25), levels = 1:3, labels = c("Educación media", "Técnico (IP/CFT)", "Universidad")),
                     "Nivel educacional que espera completar"),
    univ = lab(as.integer(na_ns(p25) == 3), "Espera completar una carrera universitaria (1)"),

    # Experimento (solo ola 1)
    trat = lab(ifelse(ola == 1, as.integer(asignacion == 1), NA_integer_),
               "Asignación al tratamiento: información sobre acceso a la universidad (1) vs. control deportes (0). Solo ola 1"),
    comprende = lab(ifelse(ola == 1, as.integer(ifelse(asignacion == 1, tratamiento == 2, control == 2)), NA_integer_),
                    "Responde correctamente la pregunta de verificación (1). Solo ola 1"),

    # Creencias complementarias y de control
    p1_1 = lab(na_ns(p1_1), "En Chile, las personas son recompensadas por sus esfuerzos (1-4)"),
    p1_2 = lab(na_ns(p1_2), "En Chile, las personas son recompensadas por su inteligencia y habilidad (1-4)"),
    p1_3 = lab(na_ns(p1_3), "En Chile, a quienes tienen padres ricos les va mucho mejor en la vida (1-4)"),
    p1_4 = lab(na_ns(p1_4), "En Chile, quienes tienen buenos contactos les va mejor en la vida (1-4)"),
    p1_5 = lab(na_ns(p1_5), "Quienes más se esfuerzan deberían obtener mayores recompensas (1-4)"),
    p1_6 = lab(na_ns(p1_6), "Quienes poseen más talento deberían obtener mayores recompensas (1-4)"),
    p1_7 = lab(na_ns(p1_7), "Está bien que quienes tienen padres ricos les vaya bien en la vida (1-4)"),
    p1_8 = lab(na_ns(p1_8), "Está bien que quienes tienen buenos contactos les vaya bien en la vida (1-4)"),

    # Sociodemográficas
    genero = lab(factor(p20, levels = 1:3, labels = c("Hombre", "Mujer", "Otro")), "Género"),
    extranjero = lab(extranjero_w1, "Nació fuera de Chile (1). Reportado en ola 1")
  ) |>
  mutate(
    # Índices (promedio de ítems disponibles)
    merit_esc  = lab(rowMeans(cbind(p2_1, p2_2, p2_3)), "Meritocracia escolar percibida: promedio p2_1-p2_3"),
    merit_soc  = lab(rowMeans(cbind(p1_1, p1_2)), "Meritocracia social percibida: promedio p1_1-p1_2"),
    privilegio = lab(rowMeans(cbind(p1_3, p1_4)), "Privilegio percibido: promedio p1_3-p1_4"),
    pref_merit = lab(rowMeans(cbind(p1_5, p1_6)), "Preferencia meritocrática: promedio p1_5-p1_6"),
    pref_nomerit = lab(rowMeans(cbind(p1_7, p1_8)), "Preferencia no meritocrática: promedio p1_7-p1_8"),
    # Indicadores de NSE
    madre_univ = lab(as.integer(educ_madre >= 5), "Madre con estudios universitarios o posgrado (1)"),
    educ_padres_max = lab({x <- suppressWarnings(pmax(educ_madre, educ_padre, na.rm = TRUE)); ifelse(is.infinite(x), NA, x)},
                          "Educación más alta entre madre y padre (1-6)"),
    padres_univ = lab(as.integer(educ_padres_max >= 5), "Al menos un padre con estudios universitarios o posgrado (1)"),
    libros_25 = lab(as.integer(libros >= 3), "Más de 25 libros en el hogar (1)"),
    # Género en dos categorías
    mujer_otro = lab(as.integer(genero != "Hombre"), "Mujer u otro género (1) vs. hombre (0)"),
    genero2    = lab(factor(ifelse(genero == "Hombre", "Hombre", "Mujer u otro"), levels = c("Hombre", "Mujer u otro")),
                     "Género en dos categorías"),
    # Asignación al tratamiento como control en modelos de ambas olas (0 en ola 2)
    trat_ctrl = lab(ifelse(ola == 2, 0L, trat),
                    "Asignación al tratamiento en ola 1 (1); 0 en ola 2. Para usar como control en modelos con ambas olas")
  ) |>
  arrange(id_estudiante, ola)

# --- Especificaciones categóricas ---------------------------------------
# Para cada ítem Likert (1-4): _ord (factor ordenado), _fac (factor sin orden) y
# _acuerdo (1 = de acuerdo o muy de acuerdo; 0 = en desacuerdo o muy en desacuerdo).
lik <- c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo")
items <- c("p1_9", "p1_10", "p1_1", "p1_2", "p1_3", "p1_4", "p1_5", "p1_6", "p1_7", "p1_8", "p2_1", "p2_2", "p2_3")
for (v in items) {
  base <- attr(db[[v]], "label")
  if (!paste0(v, "_ord") %in% names(db))
    db[[paste0(v, "_ord")]] <- lab(factor(db[[v]], levels = 1:4, labels = lik, ordered = TRUE), paste(base, "- ordinal"))
  db[[paste0(v, "_fac")]] <- lab(factor(db[[v]], levels = 1:4, labels = lik), paste(base, "- factor"))
  if (!paste0(v, "_acuerdo") %in% names(db))
    db[[paste0(v, "_acuerdo")]] <- lab(as.integer(db[[v]] >= 3), paste(base, "- de acuerdo (1)"))
}

# Índices: versión binaria (1 = promedio sobre el punto medio, 2,5) y, para meritocracia
# escolar (3 ítems, sin empates en 2,5), versión en categorías redondeando el promedio.
for (v in c("merit_esc", "merit_soc", "privilegio", "pref_merit", "pref_nomerit"))
  db[[paste0(v, "_acuerdo")]] <- lab(as.integer(db[[v]] > 2.5),
                                     paste(attr(db[[v]], "label"), "- de acuerdo en promedio (> 2,5) (1)"))
db$merit_esc_ord <- lab(factor(round(db$merit_esc), levels = 1:4, labels = lik, ordered = TRUE),
                        "Meritocracia escolar percibida: promedio redondeado a la categoría más cercana - ordinal")
db$merit_esc_fac <- lab(factor(round(db$merit_esc), levels = 1:4, labels = lik),
                        "Meritocracia escolar percibida: promedio redondeado a la categoría más cercana - factor")

# Educación de los padres: ordinal y factor (la versión binaria es madre_univ / padres_univ)
educ_lab <- c("Básica incompleta", "Básica completa", "Media completa", "Técnica (IP/CFT)", "Universitaria", "Posgrado")
for (v in c("educ_madre", "educ_padre", "educ_padres_max")) {
  db[[paste0(v, "_ord")]] <- lab(factor(db[[v]], levels = 1:6, labels = educ_lab, ordered = TRUE),
                                 paste(attr(db[[v]], "label"), "- ordinal"))
  db[[paste0(v, "_fac")]] <- lab(factor(db[[v]], levels = 1:6, labels = educ_lab),
                                 paste(attr(db[[v]], "label"), "- factor"))
}
db$padre_univ <- lab(as.integer(db$educ_padre >= 5), "Padre con estudios universitarios o posgrado (1)")

# Libros en el hogar: ordinal y factor (la versión binaria es libros_25)
libros_lab <- c("0-10", "11-25", "26-100", "101-200", "201-500", "Más de 500")
db$libros_ord <- lab(factor(db$libros, levels = 1:6, labels = libros_lab, ordered = TRUE), "Libros en el hogar - ordinal")
db$libros_fac <- lab(factor(db$libros, levels = 1:6, labels = libros_lab), "Libros en el hogar - factor")

# Expectativa educativa: ordinal (aspiracion ya es factor; la binaria es univ)
db$aspiracion_ord <- lab(factor(db$aspiracion, ordered = TRUE), "Nivel educacional que espera completar - ordinal")

# Ordenar: cada variable seguida de sus versiones alternativas
grupos <- list(p1_9 = c("p1_9_ord", "p1_9_fac", "p1_9_acuerdo"),
               genero = c("genero2", "mujer_otro"), trat = "trat_ctrl",
               educ_madre = c("educ_madre_ord", "educ_madre_fac", "madre_univ"),
               educ_padre = c("educ_padre_ord", "educ_padre_fac", "padre_univ"),
               educ_padres_max = c("educ_padres_max_ord", "educ_padres_max_fac", "padres_univ"),
               libros = c("libros_ord", "libros_fac", "libros_25"),
               aspiracion = c("aspiracion_ord", "univ"),
               merit_esc = c("merit_esc_ord", "merit_esc_fac", "merit_esc_acuerdo"))
for (v in setdiff(c(items, "merit_soc", "privilegio", "pref_merit", "pref_nomerit"), names(grupos)))
  grupos[[v]] <- intersect(paste0(v, c("_ord", "_fac", "_acuerdo")), names(db))
orden <- character(0)
for (v in names(db)) {
  if (v %in% unlist(grupos)) next
  orden <- c(orden, v, grupos[[v]])
}
stopifnot(setequal(orden, names(db)))
db <- db[, orden]

# --- Medias por grupo (cluster means) y centrados ------------------------
# Para cada variable x y cada agrupamiento g (est = estudiante, cur = curso de ola 1):
#   x_m_g      media de x en el grupo (cluster mean; promedio de las observaciones disponibles)
#   x_cwc_g    centrado en la media del grupo: x - x_m_g (variación dentro del grupo)
#   x_m_g_gmc  media del grupo centrada en la media general: x_m_g - media general
#   x_m_est_cwc_cur  media del estudiante - media del curso (entre estudiantes del mismo curso)
# y una versión común a ambos agrupamientos:
#   x_gmc      centrado en la media general: x - media general
# La media general se calcula sobre todas las observaciones (estudiante-ola) con dato válido,
# de modo que x_gmc = x_cwc_g + x_m_g_gmc. Las medias de curso usan ambas olas y quedan NA
# para los 35 estudiantes sin curso (solo ola 2).
vars_centrar <- c("merit_esc", "p2_1", "p2_2", "p2_3", "notas", "univ",
                  "educ_madre", "madre_univ", "educ_padres_max", "padres_univ", "libros",
                  "pref_merit", "pref_nomerit", "merit_soc", "privilegio",
                  "mujer_otro", "extranjero", "trat_ctrl")
grupos_c <- c(est = "id_estudiante", cur = "id_curso")
nom_g <- c(est = "estudiante", cur = "curso")
for (v in vars_centrar) {
  x  <- as.numeric(db[[v]])
  gm <- mean(x, na.rm = TRUE)
  et <- sub(" \\(.*$", "", attr(db[[v]], "label"))
  db[[paste0(v, "_gmc")]] <- lab(x - gm, paste0(et, " - centrada en la media general (GMC)"))
  for (g in names(grupos_c)) {
    id <- db[[grupos_c[g]]]
    m  <- ave(x, id, FUN = function(z) if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE))
    m[is.na(id)] <- NA
    db[[paste0(v, "_m_", g)]]     <- lab(m, paste0(et, " - media por ", nom_g[g]))
    db[[paste0(v, "_cwc_", g)]]   <- lab(x - m, paste0(et, " - centrada en la media del ", nom_g[g], " (CWC)"))
    db[[paste0(v, "_m_", g, "_gmc")]] <- lab(m - gm, paste0(et, " - media por ", nom_g[g], " centrada en la media general"))
  }
  # Componente entre estudiantes del mismo curso (para modelos de tres niveles)
  db[[paste0(v, "_m_est_cwc_cur")]] <- lab(db[[paste0(v, "_m_est")]] - db[[paste0(v, "_m_cur")]],
                                           paste0(et, " - media del estudiante centrada en la media del curso"))
}

# --- Chequeos ---------------------------------------------------------
stopifnot(nrow(db) == 1576, !anyNA(db$escuela), !anyNA(db$cohorte_media), sum(db$cohorte_media) > 0,
          !anyNA(db$id_curso[db$ola == 1]),
          all(tapply(db$id_curso, db$id_estudiante, function(x) length(unique(x)) == 1)))
cat("Cursos:", nlevels(db$curso), "| Estratos:", nlevels(db$estrato),
    "| Observaciones sin curso:", sum(is.na(db$id_curso)), "\n")
cat("Observaciones:", nrow(db), "| Estudiantes:", n_distinct(db$id_estudiante),
    "| En ambas olas:", sum(table(db$id_estudiante) == 2), "\n")
print(table(db$escuela, db$ola))

# --- Exportar ---------------------------------------------------------
datos_io <- db
save(datos_io, file = file.path("input", "datos_igualdad_oportunidades.RData"))

# Diccionario de variables
dic <- data.frame(variable = names(db),
                  etiqueta = sapply(db, function(x) ifelse(is.null(attr(x, "label")), "", attr(x, "label"))),
                  tipo = sapply(db, function(x) class(x)[1]),
                  n_validos_ola1 = sapply(db, function(x) sum(!is.na(x[db$ola == 1]))),
                  n_validos_ola2 = sapply(db, function(x) sum(!is.na(x[db$ola == 2]))))
write.csv(dic, file.path("input", "diccionario_datos_igualdad_oportunidades.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(dic[, -1], right = FALSE)
