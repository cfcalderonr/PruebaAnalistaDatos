# -------------------------------------------------------------------------
# R version 4.3.1
# 
# R00_ProcesaDatos.R
# Autor: Cristhian Calderon
# Fecha: 29/02/2024
# 
# input: 
#   Conexion con base de datos en SQLite (databaseADRESS.db)
#   tablas municipios, prestadores
# 
# output: 
#   00Resultados_v2.xlsx - Excel con resultados consolidados
# 
# -------------------------------------------------------------------------
rm(list = ls())

# -------------------------------------------------------------------------
# Rutas -------------------------------------------------------------------
# -------------------------------------------------------------------------
setwd("G:/Mi unidad/ADRES/src")
inPath <- file.path("..", "input")
outPath <- file.path("..", "output")

# -------------------------------------------------------------------------
# Librerias ---------------------------------------------------------------
# -------------------------------------------------------------------------

# Conexion base de datos
library(DBI)
library(RSQLite)

# Manejo de datos
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(fuzzyjoin)
library(ggplot2)
library(ggpubr)
library(openxlsx)

# Analisis adicionales
library(randomForest)
library(factoextra)

# -------------------------------------------------------------------------
# Funciones ---------------------------------------------------------------
# -------------------------------------------------------------------------
ajustTxt <- function(string){
  string2 <- string
  string2 <- tolower(string2)
  string2 <- gsub("%|>|&|!|'|#|\\?|\\*", "", string2)
  string2 <- gsub("\\s+", " ", string2)
  string2 <- chartr("áéíóú", "aeiou", string2)
  string2 <- str_trim(string2)
  string2 <- str_to_title(string2)
}

# -------------------------------------------------------------------------
# Datos -------------------------------------------------------------------
# -------------------------------------------------------------------------
mydb <- dbConnect(RSQLite::SQLite(), file.path(inPath, "databaseADRESS.db"))

municipios <- dbGetQuery(mydb, 'SELECT * FROM municipios')
dim(municipios) # 1118    8

prestadores <- dbGetQuery(mydb, 'SELECT depa_nombre, muni_nombre, codigo_habilitacion, nits_nit, razon_social, clpr_nombre, nivel, caracter, fecha_radicacion, fecha_vencimiento, clase_persona, naju_nombre FROM prestadores')
dim(prestadores) # 60946    12

table(prestadores$clpr_nombre)

# -------------------------------------------------------------------------
# Procesamiento -----------------------------------------------------------
# -------------------------------------------------------------------------

# Ajuste formato ----------------------------------------------------------

# Tabla municipio
municipios$Departamento <- ajustTxt(municipios$Departamento)
municipios$Municipio <- ajustTxt(municipios$Municipio)

municipios <- municipios %>% 
  mutate(Superficie = as.numeric(gsub(",", "\\.", Superficie)),
         Poblacion = as.numeric(Poblacion),
         Irural = as.numeric(Irural)) %>% 
  mutate_all(~ifelse(. == "", NA, .))

# Tabla prestadores
prestadores$depa_nombre <- iconv(prestadores$depa_nombre, "latin1", "UTF-8")
prestadores$depa_nombre <- ajustTxt(prestadores$depa_nombre)
prestadores$muni_nombre <- iconv(prestadores$muni_nombre, "latin1", "UTF-8")
prestadores$muni_nombre <- ajustTxt(prestadores$muni_nombre)

prestadores$razon_social <- iconv(prestadores$razon_social, "latin1", "UTF-8")
prestadores$clpr_nombre <- iconv(prestadores$clpr_nombre, "latin1", "UTF-8")
prestadores$naju_nombre <- iconv(prestadores$naju_nombre, "latin1", "UTF-8")

prestadores <- prestadores %>% 
  mutate_all(~ifelse(. == "", NA, .)) %>% 
  mutate(depa_nombre = ifelse(depa_nombre == "Cali", "Valle Del Cauca", depa_nombre),
         depa_nombre = ifelse(depa_nombre == "Buenaventura", "Valle Del Cauca", depa_nombre),
         depa_nombre = ifelse(depa_nombre == "Barranquilla", "Atlantico", depa_nombre),
         depa_nombre = ifelse(depa_nombre == "Santa Marta", "Magdalena", depa_nombre),
         depa_nombre = ifelse(depa_nombre == "Cartagena", "Bolivar", depa_nombre))

# Validaciones base municipios --------------------------------------------

# Municipios duplicados
sum(duplicated(municipios$Depmun))

# Municipios clasificados en departamentos incorrectos
municipios %>% 
  mutate(DepVal = substr(Depmun, 1, 2)) %>% 
  filter(DepVal != Dep)

# Departamentos en regiones diferentes
municipios %>% 
  distinct(Dep, Region) %>% 
  group_by(Dep) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# Validaciones base prestadores -------------------------------------------

# Codigo de habilitacion duplicado duplicados
sum(duplicated(prestadores$codigo_habilitacion))

# Prestadores/Nits duplicados [Hay varias sedes con el mismo nit]
sum(duplicated(prestadores$nits_nit))
# prestadores %>%
#   group_by(nits_nit) %>%
#   mutate(n = n()) %>%
#   filter(n > 1)

# Nits con diferentes razones sociales
prestadores %>% 
  distinct(nits_nit, razon_social) %>% 
  group_by(nits_nit) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)

# Cruce exacto de las dos bases -------------------------------------------

# Prestadores
exc_join <- prestadores %>% 
  inner_join(municipios,
             by = c("depa_nombre" = "Departamento", "muni_nombre" = "Municipio")) %>% 
  mutate(tCruce = "Exacto")

# Cruce por distancia de cadena -------------------------------------------

# Identificacion de los prestadores faltantes
prestadores_falt <- prestadores %>% 
  anti_join(municipios,
            by = c("depa_nombre" = "Departamento", "muni_nombre" = "Municipio"))

# Cruce por distancia de los departamento-municipio
dist_join <- prestadores_falt %>% 
  distinct(depa_nombre, muni_nombre) %>% 
  stringdist_left_join(municipios %>% 
                       rename(depa_nombre = Departamento,
                              muni_nombre = Municipio) %>% 
                       distinct(depa_nombre, muni_nombre), 
                     method = "jaccard", max_dist = 40, distance_col = c("Dist"),
                     by = c("depa_nombre", "muni_nombre")) %>% 
  select(-Dist) %>% 
  mutate(distProm = rowMeans(.[, c("depa_nombre.Dist", "muni_nombre.Dist")])) %>% 
  arrange(depa_nombre.x, muni_nombre.x, distProm) %>% 
  group_by(depa_nombre.x, muni_nombre.x) %>% 
  mutate(idSel = 1:n()) %>% 
  ungroup() %>% 
  filter(idSel %in% 1) %>% 
  select(-idSel, -depa_nombre.Dist, -muni_nombre.Dist, -distProm) %>% 
  rename(depa_nombre = depa_nombre.x, muni_nombre = muni_nombre.x, 
         Departamento = depa_nombre.y, Municipio = muni_nombre.y)

# Cruce de los prestadores faltantes
falt_join <- prestadores_falt %>% 
  left_join(dist_join, by = c("depa_nombre", "muni_nombre")) %>% 
  left_join(municipios, by = c("Departamento", "Municipio")) %>% 
  select(-depa_nombre, -muni_nombre) %>% 
  rename(depa_nombre = Departamento, muni_nombre = Municipio) %>% 
  mutate(tCruce = "Distancia")

# Municipios que no tienen prestadores
muni_no <- municipios %>% 
  anti_join(exc_join %>% 
  select(Departamento = depa_nombre, Municipio = muni_nombre) %>% 
  distinct() %>% 
  rbind(dist_join %>% 
          distinct(Departamento, Municipio)) %>% 
  group_by(Departamento, Municipio) %>% 
  distinct())

# Validaciones
nrow(falt_join) == nrow(prestadores_falt)
(nrow(exc_join)+nrow(falt_join)) == nrow(prestadores)
setdiff(names(falt_join), names(exc_join))

# Base final --------------------------------------------------------------
baseF <- exc_join %>% 
  rbind(falt_join) %>% 
  group_by(nits_nit) %>% 
  mutate(no_sedes = n()) %>% 
  ungroup()

# Descriptivo cantidad de sedes
table(baseF$no_sedes)

colSums(is.na(baseF))[colSums(is.na(baseF)) != 0]

# Limpieza de memoria
rm(exc_join, prestadores_falt, dist_join, falt_join)

# -------------------------------------------------------------------------
# Analisis ----------------------------------------------------------------
# -------------------------------------------------------------------------

# Indicador - Habitantes por prestador ------------------------------------
tab1 <- baseF %>% 
  group_by(Region, depa_nombre, muni_nombre, Depmun) %>% 
  mutate(Prestadores = n()) %>% 
  ungroup() %>% 
  distinct(Region, depa_nombre, muni_nombre, Depmun, Prestadores, Poblacion) %>% 
  rbind(muni_no %>%
          select(Region, depa_nombre = Departamento, muni_nombre = Municipio,
                 Depmun, Poblacion) %>%
          mutate(Prestadores = 0)) %>%
  group_by(Region) %>% 
  summarise(nMun = n_distinct(Depmun),
            PrestadoresR = sum(Prestadores),
            PoblacionR = sum(Poblacion)) %>% 
  ungroup() %>% 
  mutate(IndicadorR = PoblacionR/PrestadoresR) %>% 
  arrange(IndicadorR)

tabP <- baseF %>% 
  group_by(Region, depa_nombre, muni_nombre, Depmun) %>% 
  mutate(Prestadores = n()) %>% 
  ungroup() %>% 
  distinct(Region, depa_nombre, muni_nombre, Depmun, Prestadores, Poblacion) %>% 
  rbind(muni_no %>%
          select(Region, depa_nombre = Departamento, muni_nombre = Municipio,
                 Depmun, Poblacion) %>%
          mutate(Prestadores = 0)) %>%
  group_by(Region, depa_nombre) %>% 
  summarise(PrestadoresD = sum(Prestadores),
            PoblacionD = sum(Poblacion)) %>% 
  ungroup() %>% 
  mutate(IndicadorD = PoblacionD/PrestadoresD) %>% 
  arrange(IndicadorD) %>% 
  left_join(tab1) %>% 
  mutate(Color = "",
         depa_nombre = ifelse(depa_nombre == "La Guajira", "La\nGuajira", depa_nombre),
         depa_nombre = ifelse(depa_nombre == "San Andres", "San\nAndres", depa_nombre),
         depa_nombre = ifelse(depa_nombre == "Norte De Santander", "Norte De\nSantander", depa_nombre),
         depa_nombre = ifelse(depa_nombre == "Valle Del Cauca", "Valle Del\nCauca", depa_nombre)) %>% 
  arrange(Region, IndicadorD)

depOrd <- tabP$depa_nombre
tabP$depa_nombre <- factor(tabP$depa_nombre, levels = depOrd)

geom.text.size = 2.8
theme.size = 10
p1 <- tabP %>% 
  ggplot(aes(x = depa_nombre, y = IndicadorD)) + 
  geom_point() + 
  geom_text(aes(label = round(IndicadorD, 1)), vjust = 1.5, size = geom.text.size) + 
  facet_wrap(Region~., scales = "free_x") + 
  geom_hline(aes(yintercept = IndicadorR, color = Color), tabP) + 
  labs(y="Indicador", col = "Indicador de la región") + 
  theme(legend.position="bottom", text = element_text(size = theme.size),
        legend.title = element_text(size=theme.size-2), 
        legend.text = element_text(size=theme.size-2),
        axis.title.x=element_blank())

# Municipio - Prestadores por cada habitante ------------------------------
tab2 <- baseF %>% 
  group_by(Region, depa_nombre, muni_nombre, Depmun, naju_nombre) %>% 
  summarise(Prestadores = n(),
            Poblacion = unique(Poblacion),
            Irural = unique(Irural),
            Superficie = unique(Superficie)) %>% 
  ungroup() %>% 
  spread(naju_nombre, Prestadores, fill = 0) %>% 
  full_join(baseF %>% 
              group_by(Region, depa_nombre, muni_nombre, Depmun) %>% 
              summarise(Prestadores = n(),
                        Poblacion = unique(Poblacion)) %>% 
              ungroup() %>% 
              mutate(Indicador = Poblacion/Prestadores) %>% 
              select(-Prestadores, -Poblacion))

ggplot(tab2, aes(x=Region, y=Indicador)) +
  geom_boxplot()

# Modelo de regresion lineal ----------------------------------------------
dataM <- tab2 %>% 
  select(-c(Region:Poblacion))
fit <- lm(Indicador ~ ., data = dataM)
tabFit <- summary(fit)$coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(Variable = rowname)
p2 <- ggplot(data.frame(res = fit$residuals), aes(sample = res)) + 
  stat_qq() + xlab("Cuantiles teóricos") + ylab("Cuantiles muestrales")

# Random Forest - Importancia ---------------------------------------------
fit_RF <- randomForest(Indicador ~ ., data = dataM)
tabFitF <- tabFit %>% 
  left_join(fit_RF$importance %>% 
              as.data.frame() %>% 
              rownames_to_column() %>% 
              rename(Variable = rowname) %>% 
              mutate(Importancia = IncNodePurity/sum(IncNodePurity)*100) %>% 
              select(-IncNodePurity)) %>% 
  arrange(desc(Importancia)) %>% 
  mutate(Estimate = round(Estimate, 3),
         `Std. Error` = round(`Std. Error`, 3),
         `t value` = round(`t value`, 3),
         `Pr(>|t|)` = round(`Pr(>|t|)`, 3),
         Importancia = round(Importancia, 1))

# ACP - Cluster -----------------------------------------------------------
dat_cl <- tab2 %>%
  left_join(baseF %>%
              group_by(Region, depa_nombre, muni_nombre, Depmun, clpr_nombre) %>%
              summarise(Prestadores = n(),
                        Poblacion = unique(Poblacion),
                        Irural = unique(Irural),
                        Superficie = unique(Superficie)) %>%
              ungroup() %>%
              spread(clpr_nombre, Prestadores, fill = 0)) %>%
  select(Region:Depmun, Indicador, everything())

acp <- prcomp(dat_cl[, -c(1:5)], scale = TRUE)
p3 <- fviz_pca_ind(acp, 
             addEllipses=TRUE)

dat_cl2 <- dat_cl %>% 
  mutate(cat_tam = cut(dat_cl$Poblacion, 
                       breaks = c(-1, 50000, 100000, 200000, 20000000), 
                       labels = c("(0-50k]", "(50k-100k]", "(100k-200k]", "200k+"))) %>% 
  group_by(cat_tam) %>% 
  mutate(indOutlier = ifelse(Indicador > quantile(Indicador, probs = 0.75)+1.5*IQR(Indicador), 
                             "Atípico", "No atípico")) %>% 
  ungroup()

max2_mun <- dat_cl2 %>% 
  group_by(cat_tam) %>% 
  arrange(cat_tam, desc(Indicador)) %>% 
  mutate(IdLabel = 1:n()) %>% 
  ungroup() %>% 
  filter(IdLabel <= 2)

p4 <- ggplot(dat_cl2, aes(x=cat_tam, y=Indicador)) +
  geom_boxplot() + 
  geom_text(aes(cat_tam, Indicador, hjust = 1.2, label=muni_nombre),
            size = geom.text.size, data = max2_mun) + 
  xlab("Clasificación de municipios por tamaño de población según el DANE") + 
  theme(text = element_text(size = theme.size),
        legend.title = element_text(size=theme.size-2), 
        legend.text = element_text(size=theme.size-2))

tab_dscrp <- dat_cl2 %>% 
  group_by(cat_tam, indOutlier) %>% 
  summarise(no_municipios = n()) %>% 
  left_join(dat_cl2 %>% 
              select(-c(Region:Depmun)) %>% 
              gather(Variable, Valor, -c(indOutlier, cat_tam)) %>% 
              group_by(indOutlier, cat_tam, Variable) %>% 
              summarise(Media = round(mean(Valor), 1)) %>% 
              spread(Variable, Media))

# Salida ------------------------------------------------------------------

# Crea libro
wb <- createWorkbook()

# Escritura de tablas y graficos
addWorksheet(wb, "Ind_RegDep_1")
writeDataTable(wb, "Ind_RegDep_1", tab1)
print(p1)
insertPlot(wb, "Ind_RegDep_1", startCol = ncol(tab1) + 2,
           width = 30, height = 20, fileType = "png", units = "cm")

addWorksheet(wb, "Ind_RegDep_2")
writeDataTable(wb, "Ind_RegDep_2", tabP)

addWorksheet(wb, "Ind_Mun")
writeDataTable(wb, "Ind_Mun", dat_cl2)

addWorksheet(wb, "Mod_lm_rf")
writeDataTable(wb, "Mod_lm_rf", tabFitF)
print(p2)
insertPlot(wb, "Mod_lm_rf", startCol = ncol(tabFitF) + 2,
           width = 12, height = 12, fileType = "png", units = "cm")

addWorksheet(wb, "Cluster")
writeDataTable(wb, "Cluster", tab_dscrp)
print(p3)
insertPlot(wb, "Cluster", startCol = 8, startRow = nrow(tab_dscrp) + 3, 
           width = 12, height = 12, fileType = "png", units = "cm")
print(p4)
insertPlot(wb, "Cluster", startCol = 1, startRow = nrow(tab_dscrp) + 3, 
           width = 14, height = 15, fileType = "png", units = "cm")

# Guarda excel
outFile <- file.path(outPath, "00Resultados_v2.xlsx")
saveWorkbook(wb, file = outFile, overwrite = TRUE)

# Desconecta base
dbDisconnect(mydb)
