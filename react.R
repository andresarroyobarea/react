library(readxl)
library(dplyr)
library(table1)
library(arsenal)
library(survival)
library(gtsummary)
library(survminer)

setwd("/home/andres/Escritorio/bioinfo_hematology_unit/react/data")

# 1. DATA PREPROCESSING -----------------------------------------------------------------------------------------------------------------------

# Import data. Dates values with "0000-00-00" and any field equal to "" were codified as NA.
covid_hemato <- read.csv("REVIPRIN_20220725_094416.csv", header = T, sep = ",", check.names = F, stringsAsFactors = T,
                         na.strings =  c("", "0000-00-00", "Desconocido", "DESCONOCIDO", "DESC"), fileEncoding = "iso-8859-3")


# Identify columns with all values missing.
missing_variables <- names(which(colSums(is.na(covid_hemato)) == nrow(covid_hemato)))

#miss_var_table <- as.data.frame(missing_variables)

#colnames(miss_var_table) <- "Variables_sin_ningun_dato"

#write.csv(miss_var_table, "empty_variables.csv", col.names = T, row.names = F)

# Remove variables with all values equal to NA
covid_hemato <- as.data.frame(covid_hemato[, !names(covid_hemato) %in% missing_variables])

# Remove non-informative variables.

# Todos los hospitales de alta son iguales a los hospitales de ficha y de ultima modificación.
table(covid_hemato$id_hospital_alta == covid_hemato$id_hospital_ficha)

#table(covid_hemato$nombre_hospital_alta == covid_hemato$nombre_hospital_ficha)

table(covid_hemato$id_hospital_alta == covid_hemato$ultimamodificacion_id_hospital)
#table(covid_hemato$nombre_hospital_alta == covid_hemato$ultimamodificacion_nombre_hospital)

# All values in "activo" y "verificados" variable were 1, so they were removed.
table(covid_hemato$activo)
table(covid_hemato$verificado)

# Variables names: usuario_alta, id_hospital_alta,
# completo, id_hospital_ficha, nombre_hospital_ficha, ultimamodificacion_usuario,
# ultimamodificacion_nombre_hospital, ultimamodificacion_fecha, activo,
# verificado, lf_medico, lf_email, tph_medico, tph_email.

uninformative_vars <- c("usuario_alta", "id_hospital_alta", "id_hospital_alta",
                        "completo", "id_hospital_ficha", "nombre_hospital_ficha", "ultimamodificacion_usuario",
                        "ultimamodificacion_nombre_hospital", "ultimamodificacion_id_hospital", "activo",
                        "verificado", "lf_medico", "lf_email", "tph_medico", "tph_email")

covid_hemato <- covid_hemato[, !names(covid_hemato) %in% uninformative_vars]

# There are two fields relatives to age ("age" and "dem_fechanacimiento"). Missing ages were calculated with dem_fechanacimiento.
covid_hemato$año_nacimiento <- as.numeric(substr(gsub("\\-*", "", covid_hemato$dem_fechanacimiento), 1, 4))
covid_hemato$edad <- ifelse(is.na(covid_hemato$edad), 2022-covid_hemato$año_nacimiento, covid_hemato$edad)

# Edad grupos
covid_hemato <- covid_hemato %>% mutate(edad_grupo = cut(covid_hemato$edad,
                                                         breaks = c(18, 49, 59, 69, 79, 80, 98),
                                                         labels = c('18-49', '49-59', '59-69', '69-79', ">=80", ">=80")), .after = edad)

# Remove año de nacimiento.
covid_hemato <- covid_hemato %>% select(-año_nacimiento)

# Fechas
# Some date variables will not be used in the project.
uninterested_dates <- c("f_tph", "xt_molnupiravir_fecha", "xt_hidroxicloroquina_fecha", "xt_baricitinib_fecha",
                        "xt_remdesivir_fecha", "xt_azitromicina_fecha", "xt_PAXLOVID_fecha", "xt_tocilizumab_fecha", "xt_corticoides_fecha", "covidi_reinfeccion_remdesivir_fecha",
                        "covidi_reinfeccion_PAXLOVID_fecha", "covidi_reinfeccion_tocilizumab_fecha", "covidi_reinfeccion_corticoides_fecha")

covid_hemato <- covid_hemato[, !names(covid_hemato) %in% uninterested_dates]

# Fechas relevantes en clase "Date"
relevant_dates <- c("f_covid19", "dem_ultimoseguimiento_fecha", "covida_status_fecha", "dem_vac_fecha","covidi_reinfeccion_fecha",
                    "covidi_reinfeccion_vacunacion_fecha", "covidi_reinfeccion_fechaaltaexitus")

covid_hemato[relevant_dates] <- lapply(covid_hemato[relevant_dates], as.Date)


# Identify other factor variables. SE AÑADIÓ LA VARIABLE PATOLOGÍA OTROS OBSERVACIONES,
factor_vars <- c("comor_cardiaca", "comor_pulmonar", "comor_hta", "comor_asma", "comor_renal",
                 "comor_hepatica", "comor_neoplasia", "comor_diabetes", "comor_bmi", "comor_reuma",
                 "comor_otras", "lf_verificado", "covida_verificado", "covidi_verificado", "covidii_verificado", 
                 "covidiii_verificado", "dem_comor_fumador", "dem_comor_ansiedad", "dem_comor_depresion", "dem_vac_dosis", 
                 "covidi_reinfeccion_vacunacion_dosis", "covidi_reinfeccion_tocilizumab_dosis", "tph_verificado", "dem_verificado", "patologia_subtipo", 
                 "xt_molnupiravir_dias", "xt_hidroxicloroquina_dias", "xt_baricitinib_dias", "xt_remdesivir_dias",
                 "xt_azitromicina_dias", "xt_PAXLOVID_dias", "xt_tocilizumab_dosis", "xt_corticoides_dias", "covidi_reinfeccion_remdesivir_dias",
                 "covidi_reinfeccion_PAXLOVID_dias", "covidi_reinfeccion_corticoides_dias")


covid_hemato[factor_vars] <- lapply(covid_hemato[factor_vars], as.factor)


# Añadir columna suma de comorbilidades
covid_hemato <- covid_hemato %>% mutate(comorbilidades_suma = as.numeric(covid_hemato$comor_cardiaca) -1 + as.numeric(covid_hemato$comor_pulmonar)-1 +
                                          as.numeric(covid_hemato$comor_hta)-1 + as.numeric(covid_hemato$comor_asma)-1 + 
                                          as.numeric(covid_hemato$comor_renal)-1 + as.numeric(covid_hemato$comor_hepatica)-1 + 
                                          as.numeric(covid_hemato$comor_neoplasia)-1 + as.numeric(covid_hemato$comor_diabetes)-1 +
                                          as.numeric(covid_hemato$comor_bmi)-1 + as.numeric(covid_hemato$comor_reuma)-1, .after = comorbilidades_new)

# Como factor
covid_hemato <- covid_hemato %>% mutate(comorbilidades_groups = cut(covid_hemato$comorbilidades_suma,
                                                         breaks = c(0, 1, 2, 3, 5),
                                                         labels = c('0', '1', '2', '>=3'), right = F, include.lowest = T), .after = comorbilidades_suma)



# Identify other character variables.
character_vars <- c("covidi_reinfeccion_antineoplasico_ultimo_especificar", "dem_vac_tipo_especificar", "tph_inmunosupresion_farmacos", "lf_esquema", "tratamiento_especificar",
"comor_neoplasia_especificar", "comor_hta_especificar", "identificador")

covid_hemato[character_vars] <- lapply(covid_hemato[character_vars], as.character)

# Remove white spaces in idenficador, patologia_subtipo y patologia_otros.
covid_hemato$identificador <- trimws(covid_hemato$identificador)
covid_hemato$patologia_otros <- trimws(covid_hemato$patologia_otros)
covid_hemato$patologia_subtipo <- trimws(covid_hemato$patologia_subtipo)

# Rest of variables removed.
vars_removed <-as.data.frame(c(uninformative_vars, "dem_fechanacimiento", uninterested_dates))
colnames(vars_removed) <- "Not_relevant_variables"

#write.csv(vars_removed, "not_relevant_vars_removed.csv", sep = ",", row.names = F)

rm(vars_removed, miss_var_table, character_vars, factor_vars, missing_variables, uninformative_vars)


# 2. MISSING VALUES STUDY ---------------------------------------------------------------------------------------------------------

# 2.1 Missing values table 

# En este momento se requirió una tabla con los missing de cada variable.
na_table_hospital <- covid_hemato %>% 
  group_by(nombre_hospital_alta) %>% 
  summarize_all(.funs = funs('NA' = sum(is.na(.))))

# Trasposition
na_table_hospital <- as.data.frame(t(na_table_hospital))

# hospital_name information.
colnames(na_table_hospital) <- na_table_hospital[1, ]
rownames(na_table_hospital)[1] <- "nombre_hospital_alta_NA"
na_table_hospital[1, ] <- rep(0, 10)

# Convert dataframe variables to integer.
na_table_hospital[] <- apply(na_table_hospital[], 2, as.integer)

# Total missing values per variable.
na_table_hospital$Total_missing <- rowSums(na_table_hospital)

# Total missing percetage per variable.
na_table_hospital$Total_missing_per <- round(na_table_hospital$Total_missing/442 * 100, 2)

# Sort in descendant way by total missing values.
na_table_hospital <- na_table_hospital %>% arrange(Total_missing)

write.csv(na_table_hospital, "missing_values_report.csv", col.names = T, row.names = T)

# 2.2 Missing tables by hospital to request data. 

# Generate missing table to H12O.
# Filter columns complete columns (no NA vars) except "identificador".
complete_vars <- names(which(colSums(is.na(covid_hemato)) == 0))[c(1, 4:27)]

missing_table <- covid_hemato %>% select(-all_of(complete_vars))

non_interested_vars <- colnames(missing_table)[96:126]

missing_table <- missing_table %>% select(-all_of(non_interested_vars))

missing_table[is.na(missing_table)] <- "NA"

missing_table <- missing_table %>% filter(nombre_hospital_alta == "H. RAMÓN Y CAJAL")

write.csv(missing_table, "missing_table_H_RAMON_CAJAL.csv", col.names = T, row.names = F)

rm(complete_vars, missing_table, non_interested_vars)

# NA report table with variables to request (covid_hemato has more variables)
# na_table_report <- na_table_report[rownames(na_table_report) %in% colnames(missing_table)[-c(1, 2)], ]

# na_table_report <- na_table_hospital %>% filter(Total_missing > 0)

# rownames(na_table_report) <- substring(rownames(na_table_report), 1, nchar(rownames(na_table_report))-3)

# new_order <- colnames(missing_table)[-c(1,2)]

# na_table_report <- na_table_report[match(new_order, rownames(na_table_report)),]

# write.csv(na_table_report, "missing_report_H12O.csv", sep = ",", col.names = T, row.names = T)





# 3 PREPROCESSING II  ------------------------------------------------------------------------------------------------------------

#### 3.1 Establecer un orden lógico para las variables #### 
var_order <- read.table("orden_variables.csv", sep = ",", header = F)

var_order <- var_order %>% filter(V1 %in% colnames(covid_hemato)) %>% pull()

covid_hemato <- covid_hemato[, var_order]

#### 3.2 Duplicados in V3 #### 
table(duplicated(covid_hemato$identificador))

which(duplicated(covid_hemato$identificador))

duplicated <- covid_hemato %>% filter(identificador %in% c("FGS", "4897450", "3053686", "213945", "1164137")) %>% arrange(identificador)

# write.csv(duplicated, "duplicated_patient_identifiers_V3.csv", sep = ",", row.names = F)

#### 3.3 Pacientes comunes entre V3 y V2 #### 

# Solapamiento V3-V2 basado en identificador de paciente.
V2_data <- read.csv("REVIPRIN_V2.csv", sep = ";", header = T)

V2_data$identificador <- trimws(V2_data$identificador)

duplicated_id_pat_V2_V3 <- intersect(V2_data$identificador, covid_hemato$identificador)


##### 3.3.1 Hospitales con ID de paciente con diferente formato entre V2 y V3 #####

# Solo se evaluó solapamiento por fecha de nacimiento.

# 3.3.1.1 Doce de Octubre.  
fecha_nac_doce_v2 <- as.Date(V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. 12 de OCTUBRE"]) # 2 NA
fecha_nac_doce_v3 <- covid_hemato$dem_fechanacimiento[covid_hemato$nombre_hospital_alta == "H. 12 de OCTUBRE"]

common_fech_nac_doce <- intersect(as.factor(fecha_nac_doce_v2), as.factor(fecha_nac_doce_v3))

# Los pacientes con las 5 fechas de nacimiento comunes se compararon a nivel de historial clínico en V3 y V2. Se sospecha que son pacientes comunes entre
# ambas bases de datos.
V3_simp <- covid_hemato %>% select(id, identificador, nombre_hospital_alta, dem_fechanacimiento, edad, sexo, patologia, patologia_otros, patologia_subtipo)
V2_simp <- V2_data %>% select(id, identificador, nombre_hospital_alta, dem_fechanacimiento, edad, sexo, patologia, patologia_otros, patologia_subtipo)

# 3.3.1.2 H. Infanta Leonor
fecha_nac_il_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. INFANTA LEONOR (Vallecas)"]
fecha_nac_il_v3 <- covid_hemato$dem_fechanacimiento[covid_hemato$nombre_hospital_alta == "H. INFANTA LEONOR (Vallecas)"]

common_fech_nac_il <- intersect(as.factor(fecha_nac_il_v2), as.factor(fecha_nac_il_v3))
common_fech_nac_il

# 3.3.1.3 H. Ramón y Cajal.
fecha_nac_rc_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. RAM\xd3N Y CAJAL"] # 6 NA en V2.
fecha_nac_rc_v3 <- covid_hemato$dem_fechanacimiento[covid_hemato$nombre_hospital_alta == "H. RAMÓN Y CAJAL"]

intersect(as.factor(fecha_nac_rc_v2), as.factor(fecha_nac_rc_v3)) # Empty

#####  3.3.2 Hospitales con ID de paciente con el mismo formato entre V2 y V3 #####
# Se evaluó el solapamiento de pacientes a nivel de ID y de fecha de nacimiento para conseguir una mayor credibilidad en los resultados.

# 3.3.2.1 H. PRINCIPE DE ASTURIAS
# Id paciente
id_PAS_V2 <- V2_data$identificador[V2_data$nombre_hospital_alta == "H. PRINCIPE DE ASTURIAS"]
id_PAS_V2 <- as.numeric(sub(" .*", "", id_PAS_V2))
id_PAS_V2 <- c(id_PAS_V2, 130038)

id_PAS_V3 <- as.numeric(covid_hemato$identificador[covid_hemato$nombre_hospital_alta == "H. PRINCIPE DE ASTURIAS"])

# Estos 9 pacientes se confirmaron como duplicados tras ver sus historias clínicas.
common_pat_PAS <- intersect(id_PAS_V2, id_PAS_V3)
common_pat_PAS

# Fecha nacimiento.
fecha_nac_pas_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. PRINCIPE DE ASTURIAS"] 
fecha_nac_pas_v3 <- covid_hemato$dem_fechanacimiento[covid_hemato$nombre_hospital_alta == "H. PRINCIPE DE ASTURIAS"] # Una fecha tiene formato incorrecto ("0-07-31)

# 9 fechas de nacimiento comunes. Son las de los pacientes detectados por ID.
intersect(as.factor(fecha_nac_pas_v2), as.factor(fecha_nac_pas_v3))

# 3.3.2.2 H. DE MOSTOLES
# ID.
id_mos_V2 <- as.numeric(V2_data$identificador[V2_data$nombre_hospital_alta == "H. DE MOSTOLES"])
id_mos_V2 <- as.numeric(sub(" .*", "", id_mos_V2))
id_mos_V2

id_mos_V3 <- as.numeric(covid_hemato$identificador[covid_hemato$nombre_hospital_alta == "H. DE MOSTOLES"])
id_mos_V3

intersect(id_mos_V2, id_mos_V3) # Empty

# Fecha de nacimiento.
fecha_nac_mos_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. DE MOSTOLES"] # 1 NA ("0000-00-00")
fecha_nac_mos_v3 <- covid_hemato$dem_fechanacimiento[covid_hemato$nombre_hospital_alta == "H. DE MOSTOLES"] 

# Tampoco se observa solapamiento a nivel de fechas de nacimiento.
intersect(as.factor(fecha_nac_mos_v2), as.factor(fecha_nac_mos_v3))

V2_data$dem_fechanacimiento[V2_data$identificador == "JMMH"]
covid_hemato$dem_fechanacimiento[covid_hemato$identificador == "MPV"]

# 3.3.2.3 H. DE GETAFE 
# ID
id_get_V2 <- V2_data$identificador[V2_data$nombre_hospital_alta == "H. de GETAFE"]
id_get_V3 <- covid_hemato$identificador[covid_hemato$nombre_hospital_alta == "H. de GETAFE"]

# Dos identificadores comunes.
common_get <- intersect(id_get_V2, id_get_V3)
common_get
# Fecha de nacimiento
fecha_nac_get_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. de GETAFE"] 
fecha_nac_get_v3 <- covid_hemato$dem_fechanacimiento[covid_hemato$nombre_hospital_alta == "H. de GETAFE"] 

# "1949-05-07" es AMR (común detectado por ID) mientras que "1944-01-21" corresponde a un paciente con historial clinico confuso en cuanto a presencia en 
# V2 (JMMH) Y V3 (MPV).
common_fech_nac_get <- intersect(as.factor(fecha_nac_get_v2), as.factor(fecha_nac_get_v3))

# 3.3.2.4 H. GREGORIO MARAÑON
# ID
# Dada la complejida de los códigos usados por el H.GM en V2 añadieron manualmente.
id_gm_V2 <- c(10224736, 840026475, 2252919, 2062290, 810555060, 870049425, 11051258, 10128148, 1738954, 1880278, 830146363, 2066640, 804339950, 1869601, 11117935, 
              1263220, 1857711, 1529634, 2079839, 10978759, 2398911, 1634472, 2119427, 1846535, 10665358, 10378909, 860010773, 1331231, 1331231, 10066218, 
              10851632, 820454886, 1106879, 1462641, 2093447, 1421579, 10276041, 10642435, 2172626, 10149393, 1041975, 1521305, 11152803, 820428574, 1459622, 
              2387464, 2387464, 1157924, 804592224, 10182797, 870049425, 10040684, 1304645, 870436762, 2122349, 1049067, 10868199, 1841187, 820365337, 11097396,
              10359696)

id_gm_V3 <- as.numeric(covid_hemato$identificador[covid_hemato$nombre_hospital_alta == "H. GREGORIO MARAÑON"])

intersect(id_gm_V2, id_gm_V3) # Empty

# Fecha de nacimiento
fecha_nac_gm_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. GREGORIO MARA\xd1ON"] # 12 NAs
fecha_nac_gm_v3 <- covid_hemato$dem_fechanacimiento[covid_hemato$nombre_hospital_alta == "H. GREGORIO MARAÑON"] 

# No hay comunes.
intersect(as.factor(fecha_nac_gm_v2), as.factor(fecha_nac_gm_v3)) # Empty

# 3.3.2.5 H. SEVERO OCHOA 
# ID
id_so_V2 <- V2_data$identificador[V2_data$nombre_hospital_alta == "H. SEVERO OCHOA"]
id_so_V2

id_so_V3 <- covid_hemato$identificador[covid_hemato$nombre_hospital_alta == "H. SEVERO OCHOA"]
id_so_V3

common_so <- intersect(id_so_V2, id_so_V3)
common_so

# Fecha de nacimiento
fecha_nac_so_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. SEVERO OCHOA"] # 44 NAs de 61 pacientes totales.
fecha_nac_so_v3 <- covid_hemato$dem_fechanacimiento[covid_hemato$nombre_hospital_alta == "H. SEVERO OCHOA"] 

# No hay comunes a nivel de fecha_nac
intersect(as.factor(fecha_nac_so_v2), as.factor(fecha_nac_so_v3)) # Empty

# Se evaluó el historial clínico de los ID comunes y se sospecha 15HUSO y 16HUSO son comunes entre ambas bases de datos. Sin embargo, carecen de 
# f_nac en V2.

# 3.3.2.6 H. INFANTA SOFÍA (Norte)
# ID
id_is_V2 <- as.numeric(V2_data$identificador[V2_data$nombre_hospital_alta == "H. INFANTA SOF\xcdA (Norte)"])
id_is_V3 <-  as.numeric(covid_hemato$identificador[covid_hemato$nombre_hospital_alta == "H. INFANTA SOFÍA (Norte)"])

intersect(id_is_V2, id_is_V3) # Empty

# Fecha de nacimiento
fecha_nac_is_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. INFANTA SOF\xcdA (Norte)"] 
fecha_nac_is_v3 <- covid_hemato$dem_fechanacimiento[covid_hemato$nombre_hospital_alta == "H. INFANTA SOFÍA (Norte)"] 

# No hay comunes a nivel de fecha_nac
intersect(as.factor(fecha_nac_is_v2), as.factor(fecha_nac_is_v3)) # Empty

# 3.3.2.7 H. C. SAN CARLOS
# ID
id_sc_V2 <- as.numeric(V2_data$identificador[V2_data$nombre_hospital_alta == "H. C. SAN CARLOS"])
id_sc_V3 <-  as.numeric(covid_hemato$identificador[covid_hemato$nombre_hospital_alta == "H. C. SAN CARLOS"])

intersect(id_sc_V2, id_sc_V3) # Empty

# Fecha de nacimiento
fecha_nac_sc_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. C. SAN CARLOS"] # 1 NA 
fecha_nac_sc_v3 <- covid_hemato$dem_fechanacimiento[covid_hemato$nombre_hospital_alta == "H. C. SAN CARLOS"] 

# No hay comunes a nivel de fecha_nac
intersect(as.factor(fecha_nac_sc_v2), as.factor(fecha_nac_sc_v3)) # Empty


# Guardar los resultados en listas.
# Lista de IDs en comun entre V2 y V3 por hospital.
ids_V2_V3 <- list(H_PRINC_ASTUR = common_pat_PAS,
                  H_GET = common_get,
                  H_SEV_OCHOA = common_so)

# Lista de fechas de nacimiento comunes entre V2 y V3 por hospital.
f_nac_V2_V3 <- list(H12O = common_fech_nac_doce,
                    H_GET = common_fech_nac_get, 
                    H_INF_LEONOR = common_fech_nac_il)


# Hay 4 ID de paciente comunes entre V2 y V3 que corresponden a distintos hospitales y distintos pacientes, indicando que 
# dos hospitales pueden asignarle un mismo identificador a dos pacientes distintos. Estos códigos fueron: JCG, JGA, ALA y MRL.

rm(list = ls(pattern = "fecha_nac"))
rm(list = ls(pattern = "id")[-c(1,2)])
rm(list = ls(pattern = "simp"))
rm(list = ls(pattern = "common"))
rm(duplicated_id_pat_V2_V3)




#### 3.4 Incluir nuevas variables basadas en fechas #### 

# Variable mes_año_covid: Representa la fecha en que el paciente fue confirmado por covid 19 en año-mes.
covid_hemato <- covid_hemato %>% mutate(mes_año_covid = as.factor(substr(as.character(covid_hemato$f_covid19), 1, 7)) , .after = f_covid19)

# Cambiar las variables con fechas a formato Date
covid_hemato[relevant_dates] <- lapply(covid_hemato[relevant_dates], as.Date)

# Nueva variable para cálculos. Aquellos pacientes sin fecha de último seguimiento se les asigna la fecha de f_covid_status.
# Se eliminar tras los cálculos de fechas. CORREGIR CUANDO LAS FECHAS ESTAN COMPLETAS.
covid_hemato$new_date <- as.Date(ifelse(is.na(as.character(covid_hemato$dem_ultimoseguimiento_fecha)), as.character(covid_hemato$covida_status_fecha), as.character(covid_hemato$dem_ultimoseguimiento_fecha)))

# Diferencia dem_ult_seg y f_covid
covid_hemato <- covid_hemato %>% mutate(diff_ult_seg_fecha_f_covid = as.numeric(difftime(covid_hemato$new_date, covid_hemato$f_covid19, units = "days")), .after = dem_ultimoseguimiento_fecha)

# Diferencia dem_ult_seg y f_covid en meses
covid_hemato <- covid_hemato %>% mutate(diff_ult_seg_fecha_f_covid_months = round(as.numeric(diff_ult_seg_fecha_f_covid/30.4), 1), .after = diff_ult_seg_fecha_f_covid)

# Diferencia fecha covid y fecha de la ULTIMA dosis de la vacuna.
covid_hemato <- covid_hemato %>% mutate(diff_f_covid_dem_vac_f = as.numeric(difftime(covid_hemato$f_covid19, covid_hemato$dem_vac_fecha, units = "days")), .after = diff_ult_seg_fecha_f_covid)

# Remove new_date variable.
covid_hemato <- covid_hemato %>% select(-new_date)



# 5. LISTA DE FRECUENCIAS CON PATOLOGIA, PATOLOGÍA SUBTIPO Y PATOLOGÍA OTROS ------------------------------------------------------------------------------------------------
pat_freq <- covid_hemato %>% 
            group_by(patologia, patologia_subtipo, patologia_otros, .drop=T) %>% 
            tally() %>% group_by(patologia) %>% mutate(freq_patologia = (n / sum(n))*100, freq_total = (n / nrow(covid_hemato))*100) %>%
            arrange(patologia, desc(freq_patologia)) 

# Round percentages
pat_freq$freq_patologia <- round(pat_freq$freq_patologia, 2)
pat_freq$freq_total <- round(pat_freq$freq_total, 2)

#write.csv(pat_freq, "list_frequencies_patology.csv", sep = ",", col.names = T, row.names = F)




# 6. ESTABLECIMIENTO DE NUESTRA COHORTE DE ESTUDIO Y PRIMER DESCRIPTIVO ---------------------------------------------------------------------------------------------------------------

#### 6.1 Patient filtering ####

## 6.1.1 Filter cases without f_covid19 or covid19 information 
covid_hemato <- covid_hemato %>% filter(!is.na(f_covid19) & !is.na(covid19))

# ChecK NA absence in both variables
table(is.na(covid_hemato$f_covid19))
table(is.na(covid_hemato$covid19))

## 6.1.2 Filter cases with "NO" covid19
covid_hemato <- covid_hemato %>% filter(covid19 != "NO")

## 6.1.3 Filter cases with missing values in dem_ultimoseguimiento
covid_hemato <- covid_hemato %>% filter(!is.na(dem_ultimoseguimiento))

## 6.1.4 Filter patients with covid19 date between 2021-03-01 and 2022-07-15
covid_hemato <- covid_hemato %>% filter(f_covid19 >= "2021-03-01" & f_covid19 <= "2022-07-15")

## 6.1.5 Remove patient with identificador 10321770 due date of birth error.
covid_hemato <- covid_hemato %>% filter(identificador != "10321770")

## 6.1.6 Filter diseases not presented in the previous paper.
not_int_patologies <- c("MIELOFIBROSIS PRIMARIA (MFP)", "OTROS", "POLICITEMIA VERA (PV)", "SÍNDROMES HIPEREOSINOFÍLICOS", "TROMBOCITOSIS ESENCIAL (TE)")
covid_hemato <- covid_hemato %>% filter(!(patologia %in% not_int_patologies))

## 6.1.7 Add LMMC patient to LMC group.
covid_hemato$patologia <- as.factor(ifelse(as.character(covid_hemato$patologia == "LEUCEMIA MIELOMONOCÍTICA CRÓNICA (LMMC)"), "LEUCEMIA MIELOIDE CRÓNICA (LMC)", as.character(covid_hemato$patologia)))

covid_hemato$patologia <- droplevels(covid_hemato$patologia)

## 6.1.8 Filter V3 duplicates. Criteria for filtering: The item with the last fecha_ultimo_seguimiento was kept. Id was used to filter due to similar identificador.
dup_v3 <- c(400, 182, 184)
covid_hemato <- covid_hemato %>% filter(!(id %in% dup_v3))

## 6.1.9 Recode gravedad y gravedad_maxima en grupos de 2 y 4 variables.
# covida_gravedad_simplied.
covid_hemato <- covid_hemato %>% mutate(covida_gravedad_simplified = as.factor(ifelse(covida_gravedad == "LEVE" | covida_gravedad == "MODERADO", "BAJA", "ALTA")), .after = covida_gravedad)

# covida_gravedad_maxima_simplied.
covid_hemato <- covid_hemato %>% mutate(covida_gravedad_maxima_simplified = as.factor(ifelse(covida_gravedad_maxima == "LEVE" | covida_gravedad_maxima == "MODERADO", "BAJA", "ALTA")), .after = covida_gravedad_maxima)

## 6.1.10 Codificar nueva variable como vivo o muerto simplemente. INCLUIDO COMO INICIATIVA.
covid_hemato <-  covid_hemato %>% mutate(dem_ultimoseguimiento_simplified = as.factor(ifelse(dem_ultimoseguimiento == "Muerto por COVID-19" | dem_ultimoseguimiento == "Muerto por otras causas", "Muerto", "Vivo")), .after = dem_ultimoseguimiento)

### 6.1.11 Create a variable to divide in 3 periods of time.
# 1º Periodo (01/03/2021-30/11/2021)
# 2º Periodo (01/12/2021-28/02/2022)
# 3º Periodo (01/03/2022-15/07/2022)

covid_hemato <- covid_hemato %>% mutate(f_covid19_period = as.factor(ifelse(covid_hemato$f_covid19 <= "2021-11-30", "Periodo_1",
                                                                  ifelse(covid_hemato$f_covid19 > "2021-11-30" & covid_hemato$f_covid19 <= "2022-02-28", 
                                                                         "Periodo_2", "Periodo_3"))), .after = f_covid19)

rm(var_order, dup_v3, f_nac_V2_V3, na_table_hospital, pat_freq, duplicated, V2_data, not_int_patologies)


#### 6.2 Variables selection and recoding ####

#### 6.2.1 Data was subsetted only for descriptive analysis purporse.
V3_data <- covid_hemato %>% select(f_covid19_period, diff_ult_seg_fecha_f_covid, diff_ult_seg_fecha_f_covid_months, edad, edad_grupo, sexo, comor_cardiaca, comor_pulmonar, comor_hta, 
                                   comor_asma, comor_renal, comor_hepatica, comor_neoplasia, 
                                   comor_diabetes, comor_bmi, comor_reuma, comorbilidades_suma, comorbilidades_groups,
                                   comor_otras, patologia, tph, tratamiento_tipo, manejo, covida_gravedad, covida_gravedad_simplified,
                                   covida_gravedad_maxima, covida_gravedad_maxima_simplified,
                                   dem_ultimoseguimiento, dem_ultimoseguimiento_simplified, dem_vac_sino, dem_vac_dosis, xt_PAXLOVID, xt_remdesivir, xt_corticoides, 
                                   covidi_reinfeccion)

## 6.2.2 Recode Patologias.
V3_data$patologia <- as.factor(sub("\\).*", "", sub(".*\\(", "", V3_data$patologia)))

## 6.2.3 Recode tratamiento_tipo.
V3_data$tratamiento_tipo <- as.factor(gsub(r"{\s*\([^\)]+\)}","",as.character(V3_data$tratamiento_tipo)))

## 6.2.4 Merge thp auto and haplo
V3_data$tph <- recode_factor(V3_data$tph, `CART` = "CART", `NO` = "NO", 
                                  `TPH ALO EMP` = "ALOGENICO", `TPH ALO NO EMP` = "ALOGENICO", 
                                  `TPH HAPLO` = "ALOGENICO", `TPH AUTO` = "AUTOLOGO")



# 6.1.12 Create Lymphoid variable
V3_data <- V3_data %>% mutate(Linfoide_Mieloide = as.factor(ifelse(as.character(patologia) %in% c("NHL", "MM", "LLC", "LH", "LLA"), "Linfoide", "Mieloide")), .before = patologia) 

# 6.1.13 Create Delta_Omicron variable.
V3_data <- V3_data %>% mutate(Delta_Omicron = as.factor(ifelse(as.character(f_covid19_period) %in% c("Periodo_1"), "Delta", "Omicron")), .before = f_covid19_period) 

# 6.1.14 Create two variables to group covid19 vaccination groups:
# dem_vac_dosis_groups1: "Bajo" (0-1) and "Alto" (>=2)
# dem_vac_dosis_groups2: "Bajo" (0-1-2) and "Alto" (>=3)
# Como es un factor los 0 son 1, los 1 son 2 y así sucesivamente. Esto hay que tenerlo en cuenta para la codificación.
V3_data <- V3_data %>% mutate(dem_vac_dosis_groups_1 = as.factor(ifelse(as.numeric(V3_data$dem_vac_dosis) <= 2, "Bajo", "Alto")), .after = dem_vac_dosis) 
V3_data <- V3_data %>% mutate(dem_vac_dosis_groups_2 = as.factor(ifelse(as.numeric(V3_data$dem_vac_dosis) <= 3, "Bajo", "Alto")), .after = dem_vac_dosis_groups_1) 

# Es importante tener en cuenta que los NAs presentes en las siguientes variables no deben mostrarse en el resumen descriptivo ni computarse como parte del total.
table(is.na(V3_data$manejo))
table(is.na(V3_data$dem_vac_sino))
table(is.na(V3_data$covida_gravedad_maxima_simplified))
table(is.na(V3_data$dem_ultimoseguimiento_simplified))

#write.csv(V3_data, "./V3_data.csv", sep = ",", col.names = TRUE)

# 6.1.14 Definir dataset únicamente con pacientes ingresados 
V3_data_ingresados <- V3_data %>% filter(manejo == "Ingreso hospitalario")

# 6.1.15 Reorder factor variables
# Reorder levels for logistic regresion
V3_data$patologia <- factor(V3_data$patologia, levels = c("NHL", "LH", "LLA", "LLC", "LMA", "LMC", "MM", "SMD"))

V3_data$tph <- factor(V3_data$tph, levels = c("NO", "CART", "ALOGENICO", "AUTOLOGO"))

V3_data$tratamiento_tipo <- factor(V3_data$tratamiento_tipo, levels = c("No active therapy", "Conventional QT", "Hypomethylating agents", "IMIDS", "INMUNO-QT", "MoAb", "Molecular targeted",        
                                                                        "Not DETAILLED", "Supportive care"))

V3_data$covida_gravedad_maxima_simplified <- factor(V3_data$covida_gravedad_maxima_simplified, levels = c("BAJA", "ALTA"))

V3_data$dem_vac_dosis_groups_1 <- factor(V3_data$dem_vac_dosis_groups_1, levels = c("Bajo", "Alto"))

V3_data$dem_vac_dosis_groups_2 <- factor(V3_data$dem_vac_dosis_groups_2, levels = c("Bajo", "Alto"))

V3_data$covida_gravedad_maxima <- factor(V3_data$covida_gravedad_maxima, levels = c("LEVE", "MODERADO", "GRAVE", "CRÍTICO"))

V3_data$dem_vac_dosis <- factor(V3_data$dem_vac_dosis, levels = c("1", "0", "2", "3", "4"))



# 7. DESCRIPTIVO CON VARIABLES RELEVANTES Y NUEVOS PORCENTAJES --------------------------------------------------------------------------------------

# Variables outcome: dem_ultimo_seguimiento_simplified, covida_gravedad_maxima_simplified, manejo

# Factores de interes: Linfoide_mieloide, manejo, dem_vac_sino, xt_Paxlovid_

# dem_ultimoseguimiento_simplified

V3_data_ingresados <- V3_data %>% filter(manejo == "Ingreso hospitalario")

V3_P1 <- V3_data_ingresados %>% filter(f_covid19_period == "Periodo_1")
V3_P2 <- V3_data_ingresados %>% filter(f_covid19_period == "Periodo_2")
V3_P3 <- V3_data_ingresados %>% filter(f_covid19_period == "Periodo_3")

table(V3_data$tph)

V1_res <- 
  V3_P1 %>%
  select(dem_ultimoseguimiento_simplified, covida_gravedad_maxima_simplified, Linfoide_Mieloide, dem_vac_sino,
         dem_vac_dosis, dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, xt_PAXLOVID) %>%
  tbl_summary(
    by = covida_gravedad_maxima_simplified,
    percent = "row",
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)),
    type = covida_gravedad_maxima_simplified ~ "categorical"
  )  %>%
  bold_labels()


V2_res <- 
  V3_P2 %>%
  select(dem_ultimoseguimiento_simplified, covida_gravedad_maxima_simplified, Linfoide_Mieloide, dem_vac_sino, 
         dem_vac_dosis, dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, xt_PAXLOVID) %>%
  tbl_summary(
    by = covida_gravedad_maxima_simplified,
    percent = "row",
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)),
    type = covida_gravedad_maxima_simplified ~ "categorical"
  )  %>%
  bold_labels()


V3_res <- 
  V3_P3 %>%
  select(dem_ultimoseguimiento_simplified, covida_gravedad_maxima_simplified, Linfoide_Mieloide, dem_vac_sino,
         dem_vac_dosis, dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, xt_PAXLOVID) %>%
  tbl_summary(
    by = covida_gravedad_maxima_simplified,
    percent = "row",
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)),
    type = covida_gravedad_maxima_simplified ~ "categorical"
  )  %>%
  bold_labels()

V3_total <- 
  V3_data_ingresados %>%
  select(dem_ultimoseguimiento_simplified, covida_gravedad_maxima_simplified, Linfoide_Mieloide, dem_vac_sino,
         dem_vac_dosis, dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, xt_PAXLOVID) %>%
  tbl_summary(
    by = covida_gravedad_maxima_simplified,
    percent = "row",
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)),
    type = covida_gravedad_maxima_simplified ~ "categorical"
  )  %>%
  bold_labels()

tbl_final <-
  tbl_merge(list(V1_res, V2_res, V3_res, V3_total),
            tab_spanner = c("**Periodo 1**", "**Periodo 2**", "**Periodo 3**", "**Total**")) %>%
  as_gt()%>%
  gt::tab_header(title = "Table 4. Datos de gravedad máxima en pacientes ingresados")


# ODDS RATIO TABLES ---------------------------------------------------------------------------------------------------------------------------

# Variables de estudio: 

# Age: Reference 18-49
# Sexo: Referencia Female
# Manejo: Referencia Ambulatorio
# Delta Omicron: Referencia Delta
# Dem_vac_sino: Referencia SI
# Dem_vac_group_1
# Comorbidities
# Number of commorbidities: 0 reference
# Linfoide_Mieloide: Reference Linfoide
# Patologia: Reference NHL. 
# tph: Referencia NO CAMBIAR
# tratamiento_tipo: Referencia No activo CAMBIAR

# GLM of interest variables

edad_grupo_tbl <- glm(covida_gravedad_maxima_simplified ~ edad_grupo, data = V3_data, family = binomial()) %>% 
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

sexo_tbl <- glm(covida_gravedad_maxima_simplified ~ sexo, data = V3_data, family = binomial()) %>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

manejo_tbl <- glm(covida_gravedad_maxima_simplified ~ manejo, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

delta_omicron_tbl <- glm(covida_gravedad_maxima_simplified ~ Delta_Omicron, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

dem_vac_sino_tbl <- glm(covida_gravedad_maxima_simplified ~ dem_vac_sino, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

dem_vac_dosis_tbl <-  glm(covida_gravedad_maxima_simplified ~ dem_vac_dosis, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

dem_vac_dosis_g1_tbl <-  glm(covida_gravedad_maxima_simplified ~ dem_vac_dosis_groups_1, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

dem_vac_dosis_g2_tbl <-  glm(covida_gravedad_maxima_simplified ~ dem_vac_dosis_groups_2, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comor_cardiaca_tbl <- glm(covida_gravedad_maxima_simplified ~ comor_cardiaca, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comor_pulmonar_tbl <- glm(covida_gravedad_maxima_simplified ~ comor_pulmonar, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comor_hepatica_tbl <- glm(covida_gravedad_maxima_simplified ~ comor_hepatica, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comor_hta_tbl<- glm(covida_gravedad_maxima_simplified ~ comor_hta, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comor_asma_tbl <- glm(covida_gravedad_maxima_simplified ~ comor_asma, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comor_renal_tbl <- glm(covida_gravedad_maxima_simplified ~ comor_renal, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comor_neoplasia_tbl <- glm(covida_gravedad_maxima_simplified ~ comor_neoplasia, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comor_diabetes_tbl <- glm(covida_gravedad_maxima_simplified ~ comor_diabetes, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comor_bmi_tbl <- glm(covida_gravedad_maxima_simplified ~ comor_bmi, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comor_reuma_tbl <- glm(covida_gravedad_maxima_simplified ~ comor_reuma, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

comorbilidades_groups_tbl <- glm(covida_gravedad_maxima_simplified ~ comorbilidades_groups, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

linf_mielo_tbl <- glm(covida_gravedad_maxima_simplified ~ Linfoide_Mieloide, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

patologia_tbl <- glm(covida_gravedad_maxima_simplified ~ patologia, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

tto_tipo_tbl <- glm(covida_gravedad_maxima_simplified ~ tratamiento_tipo, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

tph_tbl <- glm(covida_gravedad_maxima_simplified ~ tph, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

paxl_tbl <- glm(covida_gravedad_maxima_simplified ~ xt_PAXLOVID, data = V3_data, family = binomial())%>%
  tbl_regression(., exponentiate = TRUE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% bold_labels() %>% italicize_levels()%>% bold_p(t = .05)

tbl_stack(list(edad_grupo_tbl, sexo_tbl, delta_omicron_tbl, dem_vac_sino_tbl, dem_vac_dosis_tbl,dem_vac_dosis_g1_tbl, dem_vac_dosis_g2_tbl,
               comor_cardiaca_tbl, comor_pulmonar_tbl, comor_hepatica_tbl, comor_hta_tbl, comor_asma_tbl, comor_renal_tbl, 
               comor_diabetes_tbl, comor_neoplasia_tbl, comor_bmi_tbl, comor_reuma_tbl, comorbilidades_groups_tbl,
               linf_mielo_tbl, patologia_tbl, tto_tipo_tbl, tph_tbl, paxl_tbl)) %>% as_gt()%>%
  gt::tab_header(title = "Odds Ratio gravedad en todos los pacientes")

rm(list = ls(pattern = "_tbl"))


# 8. SURVIVAL ANALYSIS -------------------------------------------------------------------------------------------------------------------------------------------------

# ESTE MISMO BLOQUE DE CÓDIGO SE UTILIZÓ PARA EL TOTAL DE PACIENTES Y PARA LOS PACIENTES INGRESADOS (V3_data y V3_data_ingresados)

# N = 323 because patients with negative diff_ult_seg_fecha_f_covid were filtered out.
V3_surv_data <- V3_data %>% filter(diff_ult_seg_fecha_f_covid >= 0 & manejo == "Ingreso hospitalario")

# Specify dead as 1 and alive as 0.
V3_surv_data$dem_ultimoseguimiento_simplified <- as.numeric(ifelse(V3_surv_data$dem_ultimoseguimiento_simplified == "Muerto", 1, 0))

# 8.1 Survival probabilities.
V3_surv_prob <-  list(
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ f_covid19_period, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ Delta_Omicron, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ edad_grupo, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ sexo, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ comorbilidades_groups, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ Linfoide_Mieloide, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ patologia, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ covida_gravedad_maxima, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ covida_gravedad_maxima_simplified, V3_surv_data),
    #survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ manejo, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ dem_vac_sino, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ dem_vac_dosis, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ dem_vac_dosis_groups_1, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ dem_vac_dosis_groups_2, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ tratamiento_tipo, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ tph, V3_surv_data),
    survfit(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ xt_PAXLOVID, V3_surv_data)
  ) %>%
    tbl_survfit(
  times = c(30, 60, 90),
  label_header = "**{time} Days**",
  estimate_fun = style_percent) %>%
  bold_labels() 

V3_surv_prob

# 8.2 Hazard Ratios

V3_surv_data$edad_grupo <- factor(V3_surv_data$edad_grupo, levels = c("49-59", "18-49", "59-69", "69-79", ">=80"))
levels(V3_surv_data$edad_grupo)

hr_f_covid19_p <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ f_covid19_period, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_delta_omicron <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ Delta_Omicron, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_edad_grupo <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ edad_grupo, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_sexo <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ sexo, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_comorbilidades_groups <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ comorbilidades_groups, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_Linfoide_Mieloide <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ Linfoide_Mieloide, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_patologia <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ patologia, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_covida_gravedad_maxima <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ covida_gravedad_maxima, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_covida_gravedad_maxima_simplified <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ covida_gravedad_maxima_simplified, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_dem_vac_sino <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ dem_vac_sino, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_dem_vac_dosis <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ dem_vac_dosis, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_dem_vac_dosis_groups_1 <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ dem_vac_dosis_groups_1, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_dem_vac_dosis_groups_2 <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ dem_vac_dosis_groups_2, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_tto_tipo <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ tratamiento_tipo, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_tph <- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ tph, V3_surv_data) %>% tbl_regression(exponentiate = TRUE)

hr_paxlovid<- coxph(Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified) ~ xt_PAXLOVID, V3_surv_data)%>% tbl_regression(exponentiate = TRUE)


hazard <- tbl_stack(list(hr_f_covid19_p, hr_delta_omicron, hr_edad_grupo, hr_sexo, hr_comorbilidades_groups,
               hr_Linfoide_Mieloide, hr_patologia, hr_covida_gravedad_maxima, hr_covida_gravedad_maxima_simplified,
               hr_dem_vac_sino, hr_dem_vac_dosis, hr_dem_vac_dosis_groups_1, hr_dem_vac_dosis_groups_2,
               hr_tto_tipo, hr_tph, hr_paxlovid)) %>% bold_labels()


surv_hazard <-
  tbl_merge(list(V3_surv_prob, hazard),
            tab_spanner = c("**Survival estimate, % (95% CI)**", "**Hazard ratio (95%-CI)**")) %>% 
  as_gt()%>%
  gt::tab_header(title = "Probabilidad de supervivencia y HR en los pacientes ingresados (N = 120)")



ggsurvplot(
  fit = survfit(Surv(diff_ult_seg_fecha_f_covid, as.numeric(dem_ultimoseguimiento_simplified)) ~ Delta_Omicron + covida_gravedad_maxima_simplified, V3_surv_data), 
  break.time.by = 50,
  xlim = c(0, 500),
  ggtheme = theme_bw(),
  #palette = c("#FF0027", "#060606", "#BEBADA", "#BEBADA"), # "skyblue"),
  xlab = "Time (days)", 
  ylab = "Survival probability (%)",
  title = "OS by Delta_Omicron y covida_grav_max_simp ingresados",
  legend = "right",
  #legend.labs = c("Delta", "Omicron"),
  legend.labs = c("Delta-BAJA", "Delta-ALTA", "Omicron-BAJA", "Omicron-ALTA"),
  lynetype = "solid",
  risk.table = T,
  pval = TRUE,
  pval.method = T)

rm(list = ls(pattern = "hr_"))
rm(list = ls(pattern = "hazard"))
rm(V3_surv_data, V3_surv_prob)

