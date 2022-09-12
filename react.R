############################################## REACT COVID-HEMATO ###############################################################
library(readxl)
library(dplyr)
library(table1)
library(arsenal)
library(survival)
library(gtsummary)
library(survminer)
library(purrr)
library(officer)
library(writexl)
library(stringr)

# 0. FUNCTIONS
# Proporties to print tables as Word.
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)

setwd("/home/andres/Escritorio/bioinfo_hematology_unit/react/data")

# 1. DATA PREPROCESSING -----------------------------------------------------------------------------------------------------------------------

##### 1.1 Import data #####
v3_data <- read.csv("REVIPRIN_20220908_094919.csv", header = T, sep = ";", check.names = F, stringsAsFactors = F,
                         na.strings =  c("", "0000-00-00", "Desconocido", "DESCONOCIDO", "DESC"), fileEncoding = "iso-8859-3")


##### 1.2 Add data recovered from other hospitals ####

# IMPORTANT: This step would be needed if hospital would retrieve us updated information in a separated data frame and not directly in covidhemato online database.
# For now, we only have updated data from HIL hospital, so we substitute this updated data in the original data dowloaded from the database.
# If we recovered data from more hospital, we must combine the dataframes (rbind) and the update the original dataframe.

# Data from HIL hospital updated
data_rec_HIL <- read_excel("/home/andres/Escritorio/bioinfo_hematology_unit/react/data/other_data/data_recovered_HIL.xlsx", trim_ws = TRUE)

# comor_otras variable was updated manually.
data_rec_HIL <- data_rec_HIL %>% mutate(comor_otras = ifelse(is.na(comor_otras_especificar), 0, 1), .before = comor_otras_especificar)

# Substitute the HIL original data .
v3_data[v3_data$nombre_hospital_alta == "H. INFANTA LEONOR (Vallecas)", names(data_rec_HIL)] <- data_rec_HIL

# Convert all character columns to factor.
v3_data[sapply(v3_data, is.character)] <- lapply(v3_data[sapply(v3_data, is.character)], as.factor)

rm(data_rec_HIL)

##### 1.3 Missing variables study #####

###### 1.3.1 Identify variables with all values missing (empty variables) ######

# Name of initial variables
initial_vars <- colnames(v3_data)

# Identify columns with all values missing.
missing_variables <- names(which(colSums(is.na(v3_data)) == nrow(v3_data)))

# Generate a resume of empty variables.
miss_var_table <- as.data.frame(missing_variables)

colnames(miss_var_table) <- "Variables_sin_ningun_dato"

write.table(miss_var_table, "/home/andres/Escritorio/bioinfo_hematology_unit/react/resultados/discarded_variables/empty_variables.csv", 
          col.names = NA, row.names = T)

# Remove variables with all values equal to NA
v3_data <- as.data.frame(v3_data[, !names(v3_data) %in% missing_variables])

vars_after_miss <- colnames(v3_data)

###### 1.3.2 Missing values report by hospital ######
na_table_hospital <- v3_data %>% 
  group_by(nombre_hospital_alta) %>% 
  summarize_all(.funs = funs('NA' = sum(is.na(.)))) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame()

# Format table
colnames(na_table_hospital) <- na_table_hospital[1, ]
rownames(na_table_hospital)[1] <- "nombre_hospital_alta_NA"
na_table_hospital[1, ] <- rep(0, 14)

# Convert dataframe variables to integer.
na_table_hospital[] <- apply(na_table_hospital[], 2, as.integer)

# Total missing values per variable.
na_table_hospital$Total_missing <- rowSums(na_table_hospital)

# Total missing percetage per variable.
na_table_hospital$Total_missing_per <- round(na_table_hospital$Total_missing/nrow(v3_data) * 100, 2)

# Sort in descendant way by total missing values.
na_table_hospital <- na_table_hospital %>% arrange(Total_missing)

write.table(na_table_hospital, "/home/andres/Escritorio/bioinfo_hematology_unit/react/resultados/missing_report/missing_values_report.csv", 
            col.names = NA, row.names = T)

###### 1.3.3 Missing tables with queries by hospital #######

# Filter columns complete columns (no NA vars) except "identificador".
complete_vars <- names(which(colSums(is.na(v3_data)) == 0))[-c(1:4)]

# Select variables from "v3_data". (ESTO PUEDE SER MODIFICADO SI ELEGIMOS SOLO PEDIR QUERIES PARA LAS VARIABLES QUE NOS INTERESAN)
missing_table <- v3_data %>% select(-all_of(complete_vars))

# Remove covid reinfection variables
missing_table <- missing_table %>% 
  select(-all_of(colnames(missing_table)[93:126])) %>% 
  group_by(nombre_hospital_alta) %>%
  group_split()

# Assing names to tables
names(missing_table) <- c("H12O_missings", "H_SAN_CARLOS_missings", "H_GETAFE_missings", "H_LA_PRINCESA_missings", "H_MOSTOLES_missings",
                          "H_DEL_TAJO_missings", "H_F_ALCORCON_missings", "H_GREGORIO_MARAÑON_missings", "H_INFANTA_CRISTINA_missings",
                          "H_INFANTA_LEONOR_missings", "H_INFANTA_SOFIA_missings", "H_PRINCIPE_DE_ASTURIAS_missings", "H_RAMON_CAJAL_missings",
                          "H_SEVERO_OCHOA_missings")

# Save reports into directory.
setwd("/home/andres/Escritorio/bioinfo_hematology_unit/react/data/data_to_request_hospitals/")
lapply(names(missing_table), function(x){write_xlsx(missing_table[[x]], path = paste(x, "xlsx", sep = "."))})

rm(complete_vars, initial_vars, missing_variables, vars_after_miss, missing_table, miss_var_table, na_table_hospital)

##### 1.4 Uninformative variables #####

# Todos los hospitales de alta son iguales a los hospitales de ficha y de ultima modificación.
all(v3_data$id_hospital_alta == v3_data$id_hospital_ficha) == TRUE
all(v3_data$nombre_hospital_alta == v3_data$nombre_hospital_ficha) == TRUE
all(v3_data$id_hospital_alta == v3_data$ultimamodificacion_id_hospital) == TRUE
all(v3_data$nombre_hospital_alta == v3_data$ultimamodificacion_nombre_hospital) == TRUE

# All values in "activo" y "verificados" variable were 1, so they were removed.
all(v3_data$activo) == TRUE
all(v3_data$verificado) == TRUE

# Uninformative variable names
uninformative_vars <- c("usuario_alta", "id_hospital_alta",
                        "completo", "id_hospital_ficha", "nombre_hospital_ficha", "ultimamodificacion_usuario",
                        "ultimamodificacion_nombre_hospital", "ultimamodificacion_id_hospital", "activo",
                        "verificado", "lf_medico", "lf_email", "tph_medico", "tph_email")

# Remove uninformative variables
v3_data <- v3_data[, !names(v3_data) %in% uninformative_vars]

##### 1.5 Date variables #####

# Replace "-00" days in f_covid19 and dem_ultimoseguimiento_fecha by "-15".
v3_data$f_covid19 <- gsub("-00", "-15", v3_data$f_covid19)
v3_data$dem_ultimoseguimiento_fecha <- gsub("-00", "-15", v3_data$dem_ultimoseguimiento_fecha)

# There are two fields relatives to age ("age" and "dem_fechanacimiento"). Missing age values were calculated using dem_fechanacimiento.
v3_data$año_nacimiento <- as.numeric(substr(gsub("\\-*", "", v3_data$dem_fechanacimiento), 1, 4))
v3_data$edad <- ifelse(is.na(v3_data$edad), 2022-v3_data$año_nacimiento, v3_data$edad)

# Remove año_nacimiento because it will not be used.
v3_data <- v3_data %>% select(-año_nacimiento)

# Remove uninformative dates
uninterested_dates <- c("dem_fechanacimiento", "f_tph", "xt_molnupiravir_fecha", "xt_hidroxicloroquina_fecha", "xt_baricitinib_fecha",
                        "xt_remdesivir_fecha", "xt_azitromicina_fecha", "xt_PAXLOVID_fecha", "xt_tocilizumab_fecha", 
                        "xt_corticoides_fecha", "covidi_reinfeccion_remdesivir_fecha", "covidi_reinfeccion_PAXLOVID_fecha", 
                        "covidi_reinfeccion_tocilizumab_fecha", "covidi_reinfeccion_corticoides_fecha")

v3_data <- v3_data[, !names(v3_data) %in% uninterested_dates]

##### 1.6 Recoding variables class #####

# Date variables
relevant_dates <- c("f_covid19", "dem_ultimoseguimiento_fecha", "covida_status_fecha", "dem_vac_fecha","covidi_reinfeccion_fecha",
                    "covidi_reinfeccion_vacunacion_fecha", "covidi_reinfeccion_fechaaltaexitus")

v3_data[relevant_dates] <- lapply(v3_data[relevant_dates], as.Date)

# Factor variables
factor_vars <- c("comor_cardiaca", "comor_pulmonar", "comor_hta", "comor_asma", "comor_renal",
                 "comor_hepatica", "comor_neoplasia", "comor_diabetes", "comor_bmi", "comor_reuma",
                 "comor_otras", "lf_verificado", "covida_verificado", "covidi_verificado", "covidii_verificado", 
                 "covidiii_verificado", "dem_comor_fumador", "dem_comor_ansiedad", "dem_comor_depresion", "dem_vac_dosis", 
                 "covidi_reinfeccion_vacunacion_dosis", "covidi_reinfeccion_tocilizumab_dosis", "tph_verificado", "dem_verificado", "patologia_subtipo", 
                 "xt_molnupiravir_dias", "xt_hidroxicloroquina_dias", "xt_baricitinib_dias", "xt_remdesivir_dias",
                 "xt_azitromicina_dias", "xt_PAXLOVID_dias", "xt_tocilizumab_dosis", "xt_corticoides_dias", "covidi_reinfeccion_remdesivir_dias",
                 "covidi_reinfeccion_PAXLOVID_dias", "covidi_reinfeccion_corticoides_dias")

v3_data[factor_vars] <- lapply(v3_data[factor_vars], as.factor)

# Character variables.
character_vars <- c("covidi_reinfeccion_antineoplasico_ultimo_especificar", "dem_vac_tipo_especificar", "tph_inmunosupresion_farmacos", "lf_esquema", "tratamiento_especificar",
                    "comor_neoplasia_especificar", "comor_hta_especificar", "identificador")

v3_data[character_vars] <- lapply(v3_data[character_vars], as.character)

##### 1.7 Create new variables #####

###### 1.7.1 edad_grupo #######
v3_data <- v3_data %>% mutate(edad_grupo = cut(v3_data$edad,
                                                         breaks = c(18, 49, 59, 69, 79, 80, 98),
                                                         labels = c('18-49', '49-59', '59-69', '69-79', ">=80", ">=80")), .after = edad)

###### 1.7.2 comorbilidades_suma #######
v3_data <- v3_data %>% mutate(comorbilidades_suma = as.numeric(v3_data$comor_cardiaca) -1 + as.numeric(v3_data$comor_pulmonar)-1 +
                                          as.numeric(v3_data$comor_hta)-1 + as.numeric(v3_data$comor_asma)-1 + 
                                          as.numeric(v3_data$comor_renal)-1 + as.numeric(v3_data$comor_hepatica)-1 + 
                                          as.numeric(v3_data$comor_neoplasia)-1 + as.numeric(v3_data$comor_diabetes)-1 +
                                          as.numeric(v3_data$comor_bmi)-1 + as.numeric(v3_data$comor_reuma)-1, .after = comorbilidades_new)

###### 1.7.2 comorbilidades_groups #######
v3_data <- v3_data %>% mutate(comorbilidades_groups = cut(v3_data$comorbilidades_suma,
                                                         breaks = c(0, 1, 2, 3, 5),
                                                         labels = c('0', '1', '2', '>=3'), right = F, include.lowest = T), .after = comorbilidades_suma)

###### 1.7.3 mes_año_covid #######

# Cambiar las variables con fechas a formato Date
v3_data[relevant_dates] <- lapply(v3_data[relevant_dates], as.Date)

# Nueva variable para cálculos. Aquellos pacientes sin fecha de último seguimiento se les asigna la fecha de f_covid_status.
# Se elimina tras los cálculos de fechas. CORREGIR CUANDO LAS FECHAS ESTAN COMPLETAS.
v3_data$new_date <- as.Date(ifelse(is.na(as.character(v3_data$dem_ultimoseguimiento_fecha)), as.character(v3_data$covida_status_fecha), as.character(v3_data$dem_ultimoseguimiento_fecha)))

# Representa la fecha en que el paciente fue confirmado por covid 19 en año-mes.
v3_data <- v3_data %>% mutate(mes_año_covid = as.factor(substr(as.character(v3_data$f_covid19), 1, 7)) , .after = f_covid19)

###### 1.7.4 diff_ult_seg_fecha_f_covid #######
### Diferencia dem_ult_seg y f_covid
v3_data <- v3_data %>% mutate(diff_ult_seg_fecha_f_covid = as.numeric(difftime(v3_data$new_date, v3_data$f_covid19, units = "days")), .after = dem_ultimoseguimiento_fecha)

###### 1.7.5 diff_ult_seg_fecha_f_covid_months #######
# Diferencia dem_ult_seg y f_covid en meses
v3_data <- v3_data %>% mutate(diff_ult_seg_fecha_f_covid_months = round(as.numeric(diff_ult_seg_fecha_f_covid/30.4), 1), .after = diff_ult_seg_fecha_f_covid)

###### 1.7.6 diff_f_covid_dem_vac_f #######
# Diferencia fecha covid y fecha de la ULTIMA dosis de la vacuna.
v3_data <- v3_data %>% mutate(diff_f_covid_dem_vac_f = as.numeric(difftime(v3_data$f_covid19, v3_data$dem_vac_fecha, units = "days")), .after = diff_ult_seg_fecha_f_covid)

# Remove new_date variable.
v3_data <- v3_data %>% select(-new_date)

###### 1.7.7 covida_gravedad_simplified #######
# COVID-19 severity level simplified as "ALTA" o "BAJA".
v3_data <- v3_data %>% mutate(covida_gravedad_simplified = as.factor(ifelse(covida_gravedad == "LEVE" | covida_gravedad == "MODERADO", "BAJA", "ALTA")), .after = covida_gravedad)

###### 1.7.8 covida_gravedad_maxima simplified #######
# Maximum COVID-19 severity level simplified as "ALTA" o "BAJA".
v3_data <- v3_data %>% mutate(covida_gravedad_maxima_simplified = as.factor(ifelse(covida_gravedad_maxima == "LEVE" | covida_gravedad_maxima == "MODERADO", "BAJA", "ALTA")), .after = covida_gravedad_maxima)

###### 1.7.9 dem_ultimoseguimiento_simplified #######
# Status in "dem_ultimoseguimiento" simplified as "Vivo" or "Muerto".
v3_data <-  v3_data %>% mutate(dem_ultimoseguimiento_simplified = as.factor(ifelse(dem_ultimoseguimiento == "Muerto por COVID-19" | dem_ultimoseguimiento == "Muerto por otras causas", "Muerto", "Vivo")), .after = dem_ultimoseguimiento)

###### 1.7.10 f_covid_period #######
# This variable grouped the patientes in 3 periods of time.
# 1º Periodo (01/03/2021-30/11/2021)
# 2º Periodo (01/12/2021-28/02/2022)
# 3º Periodo (01/03/2022-15/07/2022)
v3_data <- v3_data %>% mutate(f_covid19_period = as.factor(ifelse(v3_data$f_covid19 >= "2021-03-01" & v3_data$f_covid19 <= "2021-11-30", "Periodo_1",
                                                                  ifelse(v3_data$f_covid19 > "2021-11-30" & v3_data$f_covid19 <= "2022-02-28", "Periodo_2",
                                                                         ifelse(v3_data$f_covid19 > "2022-02-28" & v3_data$f_covid19 <= "2022-07-15", "Periodo_3", NA)))), .after = f_covid19)


###### 1.7.11 Delta_Omicron  #######
v3_data <- v3_data %>% mutate(Delta_Omicron = as.factor(ifelse(as.character(f_covid19_period) %in% "Periodo_1", "Delta",
                                                                         ifelse(as.character(f_covid19_period) %in% c("Periodo_2", "Periodo_3"), "Omicron", NA))), 
                                                                         .before = f_covid19_period) 


###### 1.7.12 Linfoide_Mieloide  #######
# Lymphoid patologies: NHL, MM, LLC, LH, LLA and LMMC.
# Myeloid patologies: SMD, LMA and LMC.
v3_data <- v3_data %>% mutate(Linfoide_Mieloide = as.factor(ifelse(as.character(patologia) %in% c("LINFOMAS NO HODGKIN (NHL)", "MIELOMA MÚLTIPLE (MM)", 
                                                                                                            "LEUCEMIA LINFOCÍTICA CRÓNICA (LLC)", "LINFOMA DE HODGKIN (LH)",
                                                                                                            "LEUCEMIAS LINFOBLÁSTICAS AGUDAS (LLA)", 
                                                                                                            "LEUCEMIA MIELOMONOCÍTICA CRÓNICA (LMMC)"), "Linfoide",
                                                                             ifelse(as.character(patologia) %in% c("SÍNDROME MIELODISPLÁSICO (SMD)", "LEUCEMIAS MIELOIDES AGUDAS (LMA)",
                                                                                                                   "LEUCEMIA MIELOIDE CRÓNICA (LMC)"), "Mieloide", NA))), .before = patologia) 


###### 1.7.13 Delta_Omicron  #######
# dem_vac_dosis_groups1: "Bajo" (0-1) and "Alto" (>=2)
# dem_vac_dosis_groups2: "Bajo" (0-1-2) and "Alto" (>=3)
# Como es un factor los 0 son 1, los 1 son 2 y así sucesivamente. Esto hay que tenerlo en cuenta para la codificación.
v3_data <- v3_data %>% mutate(dem_vac_dosis_groups_1 = as.factor(ifelse(as.numeric(v3_data$dem_vac_dosis) <= 2, "Bajo", "Alto")), .after = dem_vac_dosis) 
v3_data <- v3_data %>% mutate(dem_vac_dosis_groups_2 = as.factor(ifelse(as.numeric(v3_data$dem_vac_dosis) <= 3, "Bajo", "Alto")), .after = dem_vac_dosis_groups_1) 


# Nuevas variables añadidas.
new_var <- c("edad_grupo", "comorbilidades_suma", "comorbilidades_groups", "mes_año_covid", "diff_ult_seg_fecha_f_covid",
             "diff_ult_seg_fecha_f_covid_months", "diff_f_covid_dem_vac_f", "covida_gravedad_simplified",
             "covida_gravedad_maxima_simplified", "dem_ultimoseguimiento_simplified", "f_covid19_period")


##### 1.8 Other text mining #####

# Remove white spaces in idenficador, patologia_subtipo y patologia_otros.
v3_data$identificador <- trimws(v3_data$identificador)
v3_data$patologia_otros <- trimws(v3_data$patologia_otros)
v3_data$patologia_subtipo <- trimws(v3_data$patologia_subtipo)

## Add LMMC patient to LMC group.
v3_data$patologia <- as.factor(ifelse(as.character(v3_data$patologia == "LEUCEMIA MIELOMONOCÍTICA CRÓNICA (LMMC)"), "LEUCEMIA MIELOIDE CRÓNICA (LMC)", as.character(v3_data$patologia)))
v3_data$patologia <- droplevels(v3_data$patologia)

##### 1.9 Variables removed report #####
vars_removed <- as.data.frame(c(uninformative_vars, "dem_fechanacimiento", uninterested_dates))

colnames(vars_removed) <- "Not_relevant_variables"

write.table(vars_removed, "/home/andres/Escritorio/bioinfo_hematology_unit/react/resultados/discarded_variables/not_relevant_vars_removed.csv", row.names = T, col.names = NA)

rm(vars_removed, character_vars, factor_vars, uninformative_vars, relevant_dates, uninterested_dates, new_var)


##### 1.10 Establecer un orden lógico para las variables ##### 
# ESTA PORCIÓN DE CÓDIGO PODRÍA OBVIARSE UNA VEZ SELECCIONADAS LAS VARIABLES DE INTERES.
setwd("/home/andres/Escritorio/bioinfo_hematology_unit/react/data/")

var_order <- read.table("orden_variables.csv", sep = ",", header = F)

var_order <- var_order %>% filter(V1 %in% colnames(v3_data)) %>% pull()

v3_data <- v3_data[, var_order]

##### 1.11 Duplicated analysis ##### 

###### 1.11.1 V3 duplicated analysis ######

# Identify duplicates in V3 using "identificador" variable.
identificador_dup <- v3_data %>% filter(duplicated(identificador)) %>% select(identificador) %>% pull()

duplicated <- v3_data %>% filter(identificador %in% identificador_dup) %>% arrange(identificador)

# Some duplicates were removed using the last followup date: (THIS PROCEDURE SHOULD BE AUTOMATED)

# - FGS: eliminar id 419

# - 1164137: Eliminar id 300

# - 4897450: Eliminar id 147

# - TPG: ELiminar id 275

# - 2368896: Eliminar id 526

# 69HUSO son distintos pacientes, así que no se elimino

# write.csv(duplicated, "duplicated_patient_identifiers_V3.csv", sep = ",", row.names = F)

####### 1.11.2 Common patients V3 and V2 #### 

# Solapamiento V3-V2 basado en identificador de paciente.
V2_data <- read.csv("REVIPRIN_V2.csv", sep = ";", header = T)

V2_data$identificador <- trimws(V2_data$identificador)

duplicated_id_pat_V2_V3 <- intersect(V2_data$identificador, v3_data$identificador)


########## 1.11.2.1 Hospitals with different "identificador" between V2 and V3 ############

# Solo se evaluó solapamiento por fecha de nacimiento.

# 3.3.1.1 Doce de Octubre.  
fecha_nac_doce_v2 <- as.Date(V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. 12 de OCTUBRE"]) # 2 NA
fecha_nac_doce_v3 <- v3_data$dem_fechanacimiento[v3_data$nombre_hospital_alta == "H. 12 de OCTUBRE"]

common_fech_nac_doce <- intersect(as.factor(fecha_nac_doce_v2), as.factor(fecha_nac_doce_v3))

# Los pacientes con las 5 fechas de nacimiento comunes se compararon a nivel de historial clínico en V3 y V2. Se sospecha que son pacientes comunes entre
# ambas bases de datos.
V3_simp <- v3_data %>% select(id, identificador, nombre_hospital_alta, dem_fechanacimiento, edad, sexo, patologia, patologia_otros, patologia_subtipo)
V2_simp <- V2_data %>% select(id, identificador, nombre_hospital_alta, dem_fechanacimiento, edad, sexo, patologia, patologia_otros, patologia_subtipo)

# 3.3.1.2 H. Infanta Leonor
fecha_nac_il_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. INFANTA LEONOR (Vallecas)"]
fecha_nac_il_v3 <- v3_data$dem_fechanacimiento[v3_data$nombre_hospital_alta == "H. INFANTA LEONOR (Vallecas)"]

common_fech_nac_il <- intersect(as.factor(fecha_nac_il_v2), as.factor(fecha_nac_il_v3))
common_fech_nac_il

# 3.3.1.3 H. Ramón y Cajal.
fecha_nac_rc_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. RAM\xd3N Y CAJAL"] # 6 NA en V2.
fecha_nac_rc_v3 <- v3_data$dem_fechanacimiento[v3_data$nombre_hospital_alta == "H. RAMÓN Y CAJAL"]

intersect(as.factor(fecha_nac_rc_v2), as.factor(fecha_nac_rc_v3)) # Empty

########## 1.11.2.2 Hospitals with the same "identificador" format between V2 and V3  #####
# Se evaluó el solapamiento de pacientes a nivel de ID y de fecha de nacimiento para conseguir una mayor credibilidad en los resultados.

# 3.3.2.1 H. PRINCIPE DE ASTURIAS
# Id paciente
id_PAS_V2 <- V2_data$identificador[V2_data$nombre_hospital_alta == "H. PRINCIPE DE ASTURIAS"]
id_PAS_V2 <- as.numeric(sub(" .*", "", id_PAS_V2))
id_PAS_V2 <- c(id_PAS_V2, 130038)

id_PAS_V3 <- as.numeric(v3_data$identificador[v3_data$nombre_hospital_alta == "H. PRINCIPE DE ASTURIAS"])

# Estos 9 pacientes se confirmaron como duplicados tras ver sus historias clínicas.
common_pat_PAS <- intersect(id_PAS_V2, id_PAS_V3)
common_pat_PAS

# Fecha nacimiento.
fecha_nac_pas_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. PRINCIPE DE ASTURIAS"] 
fecha_nac_pas_v3 <- v3_data$dem_fechanacimiento[v3_data$nombre_hospital_alta == "H. PRINCIPE DE ASTURIAS"] # Una fecha tiene formato incorrecto ("0-07-31)

# 9 fechas de nacimiento comunes. Son las de los pacientes detectados por ID.
intersect(as.factor(fecha_nac_pas_v2), as.factor(fecha_nac_pas_v3))

# 3.3.2.2 H. DE MOSTOLES
# ID.
id_mos_V2 <- as.numeric(V2_data$identificador[V2_data$nombre_hospital_alta == "H. DE MOSTOLES"])
id_mos_V2 <- as.numeric(sub(" .*", "", id_mos_V2))
id_mos_V2

id_mos_V3 <- as.numeric(v3_data$identificador[v3_data$nombre_hospital_alta == "H. DE MOSTOLES"])
id_mos_V3

intersect(id_mos_V2, id_mos_V3) # Empty

# Fecha de nacimiento.
fecha_nac_mos_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. DE MOSTOLES"] # 1 NA ("0000-00-00")
fecha_nac_mos_v3 <- v3_data$dem_fechanacimiento[v3_data$nombre_hospital_alta == "H. DE MOSTOLES"] 

# Tampoco se observa solapamiento a nivel de fechas de nacimiento.
intersect(as.factor(fecha_nac_mos_v2), as.factor(fecha_nac_mos_v3))

V2_data$dem_fechanacimiento[V2_data$identificador == "JMMH"]
v3_data$dem_fechanacimiento[v3_data$identificador == "MPV"]

# 3.3.2.3 H. DE GETAFE 
# ID
id_get_V2 <- V2_data$identificador[V2_data$nombre_hospital_alta == "H. de GETAFE"]
id_get_V3 <- v3_data$identificador[v3_data$nombre_hospital_alta == "H. de GETAFE"]

# Dos identificadores comunes.
common_get <- intersect(id_get_V2, id_get_V3)
common_get
# Fecha de nacimiento
fecha_nac_get_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. de GETAFE"] 
fecha_nac_get_v3 <- v3_data$dem_fechanacimiento[v3_data$nombre_hospital_alta == "H. de GETAFE"] 

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

id_gm_V3 <- as.numeric(v3_data$identificador[v3_data$nombre_hospital_alta == "H. GREGORIO MARAÑON"])

intersect(id_gm_V2, id_gm_V3) # Empty

# Fecha de nacimiento
fecha_nac_gm_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. GREGORIO MARA\xd1ON"] # 12 NAs
fecha_nac_gm_v3 <- v3_data$dem_fechanacimiento[v3_data$nombre_hospital_alta == "H. GREGORIO MARAÑON"] 

# No hay comunes.
intersect(as.factor(fecha_nac_gm_v2), as.factor(fecha_nac_gm_v3)) # Empty

# 3.3.2.5 H. SEVERO OCHOA 
# ID
id_so_V2 <- V2_data$identificador[V2_data$nombre_hospital_alta == "H. SEVERO OCHOA"]
id_so_V2

id_so_V3 <- v3_data$identificador[v3_data$nombre_hospital_alta == "H. SEVERO OCHOA"]
id_so_V3

common_so <- intersect(id_so_V2, id_so_V3)
common_so

# Fecha de nacimiento
fecha_nac_so_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. SEVERO OCHOA"] # 44 NAs de 61 pacientes totales.
fecha_nac_so_v3 <- v3_data$dem_fechanacimiento[v3_data$nombre_hospital_alta == "H. SEVERO OCHOA"] 

# No hay comunes a nivel de fecha_nac
intersect(as.factor(fecha_nac_so_v2), as.factor(fecha_nac_so_v3)) # Empty

# Se evaluó el historial clínico de los ID comunes y se sospecha 15HUSO y 16HUSO son comunes entre ambas bases de datos. Sin embargo, carecen de 
# f_nac en V2.

# 3.3.2.6 H. INFANTA SOFÍA (Norte)
# ID
id_is_V2 <- as.numeric(V2_data$identificador[V2_data$nombre_hospital_alta == "H. INFANTA SOF\xcdA (Norte)"])
id_is_V3 <-  as.numeric(v3_data$identificador[v3_data$nombre_hospital_alta == "H. INFANTA SOFÍA (Norte)"])

intersect(id_is_V2, id_is_V3) # Empty

# Fecha de nacimiento
fecha_nac_is_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. INFANTA SOF\xcdA (Norte)"] 
fecha_nac_is_v3 <- v3_data$dem_fechanacimiento[v3_data$nombre_hospital_alta == "H. INFANTA SOFÍA (Norte)"] 

# No hay comunes a nivel de fecha_nac
intersect(as.factor(fecha_nac_is_v2), as.factor(fecha_nac_is_v3)) # Empty

# 3.3.2.7 H. C. SAN CARLOS
# ID
id_sc_V2 <- as.numeric(V2_data$identificador[V2_data$nombre_hospital_alta == "H. C. SAN CARLOS"])
id_sc_V3 <-  as.numeric(v3_data$identificador[v3_data$nombre_hospital_alta == "H. C. SAN CARLOS"])

intersect(id_sc_V2, id_sc_V3) # Empty

# Fecha de nacimiento
fecha_nac_sc_v2 <- V2_data$dem_fechanacimiento[V2_data$nombre_hospital_alta == "H. C. SAN CARLOS"] # 1 NA 
fecha_nac_sc_v3 <- v3_data$dem_fechanacimiento[v3_data$nombre_hospital_alta == "H. C. SAN CARLOS"] 

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











# 2. PATIENT EXCLUSION-INCLUSION (COHORT STABLISHMENT) ---------------------------------------------------------------------------------------------------------------

## 2.1 Discard patients without f_covid19 or covid19 information 
v3_data <- v3_data %>% filter(!is.na(f_covid19) & !is.na(covid19))

## 2.2 Discard patients with "NO" covid19
v3_data <- v3_data %>% filter(covid19 != "NO")

## 2.3 Discard patients with missing values in dem_ultimoseguimiento
v3_data <- v3_data %>% filter(!is.na(dem_ultimoseguimiento))

## 2.4 Discard patients with covid19 date outside 2021-03-01 and 2022-07-15
v3_data <- v3_data %>% filter(f_covid19 >= "2021-03-01" & f_covid19 <= "2022-07-15")

## 2.5 Discard patient with identificador 10321770 due date of birth error.
v3_data <- v3_data %>% filter(identificador != "10321770")

## 2.6 Discard patients with diseases not included in the previous paper.
# Some patients were classified in the "patologia" variable as "OTROS" but they were Waldestrom disease (a type of NHL). They were recodified to 
# avoid losing them during filtering.
v3_data$patologia <- as.character(v3_data$patologia)
v3_data$patologia[grepl("(?i)waldestrom", v3_data$patologia_otros)] <- "NHL"
v3_data$patologia <- as.factor(v3_data$patologia)

# Patologies to discard
not_int_patologies <- c("MIELOFIBROSIS PRIMARIA (MFP)", "OTROS", "POLICITEMIA VERA (PV)", "SÍNDROMES HIPEREOSINOFÍLICOS", "TROMBOCITOSIS ESENCIAL (TE)")
v3_data <- v3_data %>% filter(!(patologia %in% not_int_patologies))

## 2.7 Filter V3 duplicates. Criteria for filtering: The item with the last fecha_ultimo_seguimiento was kept. Id was used to filter due to similar identificador.
dup_v3 <- c(419, 300, 147, 275, 526, 184, 165, 420, 421)
v3_data <- v3_data %>% filter(!(id %in% dup_v3))


rm(dup_v3, identificador_dup, not_int_patologies, duplicated)

# 3. VARIABLES SELECTION AND RECODING ---------------------------------------------------------------------------------------------------------------

#### 3.1 Variables selection ####
v3_data <- v3_data %>% select(f_covid19_period, diff_ult_seg_fecha_f_covid, edad, edad_grupo, sexo, comor_cardiaca, comor_pulmonar, comor_hta, 
                                   comor_asma, comor_renal, comor_hepatica, comor_neoplasia, 
                                   comor_diabetes, comor_bmi, comor_reuma, comorbilidades_suma, comorbilidades_groups,
                                   comor_otras, patologia, tph, tratamiento_tipo, manejo, covida_gravedad, covida_gravedad_simplified,
                                   covida_gravedad_maxima, covida_gravedad_maxima_simplified,
                                   dem_ultimoseguimiento, dem_ultimoseguimiento_simplified, dem_vac_sino, dem_vac_dosis, dem_vac_dosis_groups_1, 
                                   dem_vac_dosis_groups_2, xt_PAXLOVID)

#### 3.2 Variables recoding ####
# Patologia
v3_data$patologia <- as.factor(sub("\\).*", "", sub(".*\\(", "", v3_data$patologia)))

## tratamiento_tipo
v3_data$tratamiento_tipo <- as.factor(gsub(r"{\s*\([^\)]+\)}", "", as.character(v3_data$tratamiento_tipo)))

## Merge thp auto and haplo in tph
v3_data$tph <- recode_factor(v3_data$tph, `CART` = "CART", `NO` = "NO", 
                                  `TPH ALO EMP` = "ALOGENICO", `TPH ALO NO EMP` = "ALOGENICO", 
                                  `TPH HAPLO` = "ALOGENICO", `TPH AUTO` = "AUTOLOGO")


#### 3.3 Factor levels reordering ####
# Levels were reordered for logistic regression analysis.

# Patología
v3_data$patologia <- factor(v3_data$patologia, levels = c("NHL", "LH", "LLA", "LLC", "LMA", "LMC", "MM", "SMD"))

# tph
v3_data$tph <- factor(v3_data$tph, levels = c("NO", "CART", "ALOGENICO", "AUTOLOGO"))

# tratamiento_tipo
v3_data$tratamiento_tipo <- factor(v3_data$tratamiento_tipo, levels = c("No active therapy", "Conventional QT", "Hypomethylating agents", "IMIDS", "INMUNO-QT", "MoAb", "Molecular targeted",        
                                                                        "Not DETAILLED", "Supportive care"))
# covida_gravedad_maxima_simplified
v3_data$covida_gravedad_maxima_simplified <- factor(v3_data$covida_gravedad_maxima_simplified, levels = c("BAJA", "ALTA"))

# dem_vac_dosis_groups_1
v3_data$dem_vac_dosis_groups_1 <- factor(v3_data$dem_vac_dosis_groups_1, levels = c("Bajo", "Alto"))

# dem_vac_dosis_groups_2
v3_data$dem_vac_dosis_groups_2 <- factor(v3_data$dem_vac_dosis_groups_2, levels = c("Bajo", "Alto"))

# covida_gravedad_maxima
v3_data$covida_gravedad_maxima <- factor(v3_data$covida_gravedad_maxima, levels = c("LEVE", "MODERADO", "GRAVE", "CRÍTICO"))

# dem_vac_dosis
v3_data$dem_vac_dosis <- factor(v3_data$dem_vac_dosis, levels = c("1", "0", "2", "3", "4"))


# Es importante tener en cuenta que los NAs presentes en las siguientes variables no deben mostrarse en el resumen descriptivo ni computarse como parte del total.
table(is.na(v3_data$manejo))
table(is.na(v3_data$dem_vac_sino))
table(is.na(v3_data$covida_gravedad_maxima_simplified))
table(is.na(v3_data$dem_ultimoseguimiento_simplified))


#### 3.4 Subsetting "ingresados" data ####
v3_data_ingresados <- v3_data %>% filter(manejo == "Ingreso hospitalario")


# 4. PATOLOGIES FREQUENCIES ------------------------------------------------------------------------------------------------
pat_freq <- v3_data %>% 
  group_by(patologia, patologia_subtipo, patologia_otros, .drop=T) %>% 
  tally() %>% group_by(patologia) %>% mutate(freq_patologia = (n / sum(n))*100, freq_total = (n / nrow(v3_data))*100) %>%
  arrange(patologia, desc(freq_patologia)) 

# Round percentages
pat_freq$freq_patologia <- round(pat_freq$freq_patologia, 2)
pat_freq$freq_total <- round(pat_freq$freq_total, 2)

rm(pat_freq)


# 5. DESCRIPTIVO CON VARIABLES RELEVANTES Y NUEVOS PORCENTAJES --------------------------------------------------------------------------------------

# Variables outcome: dem_ultimo_seguimiento_simplified, covida_gravedad_maxima_simplified, manejo

# Factores de interes: Linfoide_mieloide, manejo, dem_vac_sino, xt_Paxlovid_

# Dividir data por periodos
V3_P1 <- v3_data_ingresados %>% filter(f_covid19_period == "Periodo_1")
V3_P2 <- v3_data_ingresados %>% filter(f_covid19_period == "Periodo_2")
V3_P3 <- v3_data_ingresados %>% filter(f_covid19_period == "Periodo_3")

# Descriptivo periodo 1
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

# Descriptivo periodo 2
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

# Descriptivo periodo 3
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

# Descriptivo total
V3_total <- 
  v3_data_ingresados %>%
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

# Stack tablas
tbl_final <-
  tbl_merge(list(V1_res, V2_res, V3_res, V3_total),
            tab_spanner = c("**Periodo 1**", "**Periodo 2**", "**Periodo 3**", "**Total**")) %>%
  as_gt()%>%
  gt::tab_header(title = "Table 4. Datos de gravedad máxima en pacientes ingresados")


# 6. ODDS RATIO TABLES ---------------------------------------------------------------------------------------------------------------------------

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

#### 6.1 Clinical summary for all patients ####

# Frequencies table
frequencies <- v3_data %>%
  select(edad_grupo, sexo, f_covid19_period, Delta_Omicron, dem_vac_sino, dem_vac_dosis, covida_gravedad_maxima_simplified,
         dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, comor_cardiaca, comor_pulmonar, comor_hepatica, comor_hta, comor_renal,
         comor_hepatica, comor_neoplasia, comor_diabetes, comor_reuma, comor_bmi, comorbilidades_groups, Linfoide_Mieloide, patologia, 
         tratamiento_tipo, tph, xt_PAXLOVID) %>%
  tbl_summary(by = covida_gravedad_maxima_simplified, missing = "no", digits = everything() ~ 0,
              statistic = list(all_categorical() ~ "{n} ({p})")) %>%
  add_n() %>%
  add_overall()

# Odds ratio table
or <- v3_data %>% 
  select(edad_grupo, sexo, f_covid19_period, Delta_Omicron, dem_vac_sino, dem_vac_dosis, covida_gravedad_maxima_simplified,
         dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, comor_cardiaca, comor_pulmonar, comor_hepatica, comor_hta, comor_renal,
         comor_hepatica, comor_neoplasia, comor_diabetes, comor_reuma, comor_bmi, comorbilidades_groups, Linfoide_Mieloide, patologia, 
         tratamiento_tipo, tph, xt_PAXLOVID) %>%
  tbl_uvregression(
    method = glm,
    y = covida_gravedad_maxima_simplified,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2),
    hide_n = T) %>% bold_labels() %>% italicize_levels() %>% bold_p(t = .05) 

# All patients report.
all_patients_summ <- tbl_merge(list(frequencies, or), tab_spanner = c("**Patients with COVID-19 severity data, N = 345**", "**Odds Ratio (95% CI)**")) %>%
              modify_footnote(everything() ~ NA) %>%
              as_gt() %>%
              gt::tab_header(title = "Odds Ratio gravedad en todos los pacientes")

#### 6.2 Clinical summary for hospitalize patients ####
# Frequencies table
frequencies <- v3_data_ingresados %>%
  select(edad_grupo, sexo, f_covid19_period, Delta_Omicron, dem_vac_sino, dem_vac_dosis, covida_gravedad_maxima_simplified,
         dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, comor_cardiaca, comor_pulmonar, comor_hepatica, comor_hta, comor_renal,
         comor_hepatica, comor_neoplasia, comor_diabetes, comor_reuma, comor_bmi, comorbilidades_groups, Linfoide_Mieloide, patologia, 
         tratamiento_tipo, tph, xt_PAXLOVID) %>%
  tbl_summary(by = covida_gravedad_maxima_simplified, missing = "no", digits = everything() ~ 0,
              statistic = list(all_categorical() ~ "{n} ({p})")) %>%
  add_n() %>%
  add_overall()

# Odds ratio table
or <- v3_data_ingresados %>% 
  select(edad_grupo, sexo, f_covid19_period, Delta_Omicron, dem_vac_sino, dem_vac_dosis, covida_gravedad_maxima_simplified,
         dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, comor_cardiaca, comor_pulmonar, comor_hepatica, comor_hta, comor_renal,
         comor_hepatica, comor_neoplasia, comor_diabetes, comor_reuma, comor_bmi, comorbilidades_groups, Linfoide_Mieloide, patologia, 
         tratamiento_tipo, tph, xt_PAXLOVID) %>%
  tbl_uvregression(
    method = glm,
    y = covida_gravedad_maxima_simplified,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2),
    hide_n = T) %>% bold_labels() %>% italicize_levels() %>% bold_p(t = .05) 

# Hospitalize patients report.
hosp_patients_summ <- tbl_merge(list(frequencies, or), tab_spanner = c("**Patients with COVID-19 severity data, N = 345**", "**Odds Ratio (95% CI)**")) %>%
  modify_footnote(everything() ~ NA) %>%
  #as_gt() %>%
  #gt::tab_header(title = "Odds Ratio gravedad en los pacientes ingresados") %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "report_2.docx", pr_section = sect_properties)

rm(or, frequencies)

# IMPORTANT: IT IS POSSIBLE TO SAVE ALL TABLES IN A WORD DOCUMENT.

# 7. SURVIVAL ANALYSIS -------------------------------------------------------------------------------------------------------------------------------------------------

# ESTE MISMO BLOQUE DE CÓDIGO SE UTILIZÓ PARA EL TOTAL DE PACIENTES Y PARA LOS PACIENTES INGRESADOS (v3_data y v3_data_ingresados)

#### 7.1 Survival report for all patients ####

# N = 333 because patients with negative diff_ult_seg_fecha_f_covid were filtered out.
v3_surv_data <- v3_data %>% filter(diff_ult_seg_fecha_f_covid >= 0)

# Specify dead as 1 and alive as 0.
v3_surv_data$dem_ultimoseguimiento_simplified <- as.numeric(ifelse(v3_surv_data$dem_ultimoseguimiento_simplified == "Muerto", 1, 0))

# Survival probabilities at time 30, 60 and 90 days.
surv_prob <- v3_surv_data %>% 
      select(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified, f_covid19_period, Delta_Omicron, edad_grupo, sexo, comorbilidades_groups,
             Linfoide_Mieloide, patologia, covida_gravedad_maxima, covida_gravedad_maxima_simplified, manejo, dem_vac_sino, dem_vac_dosis,
             dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, tratamiento_tipo, tph, xt_PAXLOVID) %>%
     tbl_survfit(
      .,
      y = Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified),
      times = c(30, 60, 90),
      label_header = "**{time} Days**",
      estimate_fun = style_percent) %>%
      bold_labels() %>% 
      italicize_levels()

# Hazard Ratios
hr <- v3_surv_data %>% 
      select(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified, f_covid19_period, Delta_Omicron, edad_grupo, sexo, comorbilidades_groups,
             Linfoide_Mieloide, patologia, covida_gravedad_maxima, covida_gravedad_maxima_simplified, manejo, dem_vac_sino, dem_vac_dosis,
             dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, tratamiento_tipo, tph, xt_PAXLOVID) %>%
      tbl_uvregression(
        method = coxph,
        y = Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified),
        exponentiate = TRUE,
        pvalue_fun = function(x) style_pvalue(x, digits = 2),
        hide_n = T
      ) %>% 
      bold_labels() %>%
      italicize_levels() %>%
      bold_p(t = .05) 

# Merge both tables
surv_all <- tbl_merge(list(surv_prob, hr), 
                      tab_spanner = c("**Survival estimate, % (95% CI)**", "**Hazard ratio (95%-CI)**")) %>%
  as_gt()%>%
  gt::tab_header(title = "Probabilidad de supervivencia y HR en todos los pacientes (N = 333)")

#### 7.2 Survival report for hospitalized patients ####

# Survival probabilities at time 30, 60 and 90 days.
surv_prob <- V3_surv_data_ingresados %>% 
  select(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified, f_covid19_period, Delta_Omicron, edad_grupo, sexo, comorbilidades_groups,
         Linfoide_Mieloide, patologia, covida_gravedad_maxima, covida_gravedad_maxima_simplified, manejo, dem_vac_sino, dem_vac_dosis,
         dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, tratamiento_tipo, tph, xt_PAXLOVID) %>%
  tbl_survfit(
    .,
    y = Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified),
    times = c(30, 60, 90),
    label_header = "**{time} Days**",
    estimate_fun = style_percent) %>%
  bold_labels() %>% 
  italicize_levels()

# Hazard Ratios
hr <- V3_surv_data_ingresados %>% 
  select(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified, f_covid19_period, Delta_Omicron, edad_grupo, sexo, comorbilidades_groups,
         Linfoide_Mieloide, patologia, covida_gravedad_maxima, covida_gravedad_maxima_simplified, manejo, dem_vac_sino, dem_vac_dosis,
         dem_vac_dosis_groups_1, dem_vac_dosis_groups_2, tratamiento_tipo, tph, xt_PAXLOVID) %>%
  tbl_uvregression(
    method = coxph,
    y = Surv(diff_ult_seg_fecha_f_covid, dem_ultimoseguimiento_simplified),
    exponentiate = TRUE,
    pvalue_fun = function(x) style_pvalue(x, digits = 2),
    hide_n = T
  ) %>% 
  bold_labels() %>%
  italicize_levels() %>%
  bold_p(t = .05) 

# Merge both tables
surv_ingresados <- tbl_merge(list(surv_prob, hr), 
                      tab_spanner = c("**Survival estimate, % (95% CI)**", "**Hazard ratio (95%-CI)**")) %>%
  as_gt()%>%
  gt::tab_header(title = "Probabilidad de supervivencia y HR en pacientes ingresados (N = 120)")

#### 7.3 Survival graphs ####

ggsurvplot(
  fit = survfit(Surv(diff_ult_seg_fecha_f_covid, as.numeric(dem_ultimoseguimiento_simplified)) ~ Delta_Omicron + covida_gravedad_maxima_simplified, v3_surv_data), 
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
rm(v3_surv_data, V3_surv_prob)

