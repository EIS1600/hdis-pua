################################################################################
# PREPARING PUA DATA ###########################################################
################################################################################

### FUNCTIONS ##################################################################

# the function generates LLD URIs from internal IDs

generate_id <- function(prefix, number) {
  # Ensure that the number is an integer
  number <- as.integer(number)
  # Ensure the prefix is a character
  prefix <- as.character(prefix)
  # Generate the ID with the specified format
  id <- sprintf("%s%05d", prefix, number)
  return(id)
}

### FUNCTIONS ##################################################################


library(tidyverse)

PUAall <- readRDS("Data/PUA_processed/PUA_allDataTables_asList.rds")

# every table can be accessed with syntax: list$table, like: PUAall$personaje
PUA <- list()
PUA$original <- PUAall
PUA$modificado <- PUAall

saveRDS(PUA, "Data/PUA_processed/PUA_allDataTables_Original_y_Modificado.rds")

View(PUA$original$personaje)

# UPDATE TABLES in PUA$modificado
# - apply a function to all `idType` columns, where Type can be taken from the name of the column
#   - Internal IDs into LLD URIs of pattern > PUA_type_0000ID
# - review each column; remove columns with little amount of data, to simplify analysis

summary(PUA$original$personaje)

test <- PUA$original$personaje %>%
  mutate(column, value = paste0(column_name, column))


summary(PUA$original$personaje$fuente)

toc <- names(PUA$original$fuente)
toc_mod <- paste0("- `", toc, "` :: ")
final <- paste(toc_mod, collapse = "\n")
cat(final)


view(PUA$original$fuente)











# 
# 1. Convert all internal PUA DB IDs into LLD IDs that will be more informative
#    and unique;
#    - applying the same function to columns that begin with `idX` will generate
#      more informative IDs (for linked local data);
#    - PUAIDpersona0000000
#    - PUAIDfamilia0000000
#    - etc.

# X. Aggregate data into the model that I used in my article AAABC
#    - PERSON_ID
#    - CATEGORY
#    - VALUE (all will be CHARACTER, inevitably)
#
#    This structure is easy to work with and subset
# X. Additional tables must include: EXPLANATORY SCHEMES / EXPANSION SCHEMES
#    - places
#    - families
#    - ???
#    - ???
#    - ???




### LIBRARIES ##################################################################

library(tidyverse)

### FUNCTIONS ##################################################################

generate_id <- function(prefix, number) {
  # Ensure that the number is an integer
  number <- as.integer(number)
  # Ensure the prefix is a character
  prefix <- as.character(prefix)
  # Generate the ID with the specified format
  id <- sprintf("%s%05d", prefix, number)
  return(id)
}


### REFORMATTING DATA ##########################################################

# AGGGREGATING data on persons:

personaje <- read_delim("Data/raw_PUA/personaje.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

personajeAcumulativo <- personaje %>%
  #rename_with(~paste0("pers_", .)) %>%
  mutate(PUApersonaje = generate_id("PUApersonaje", idPersonaje), .before = idPersonaje) %>%
  mutate(PUAfamilia = generate_id("PUAfamilia", idFamilia), .before = idFamilia) %>%
  select(-idPersonaje, -idFamilia) %>%
  mutate_all(as.character) %>%
  pivot_longer(!PUApersonaje, names_to = "category", values_to = "value") %>%
  mutate(tablaFuente = "personaje")
  
# LOAD all other tables with PERSONAL data, update their IDs, pivot_longer, and append to main PERSONAJE 
personaje_actividad <- read_delim("Data/raw_PUA/personaje_actividad.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

personaje_actividad <- personaje_actividad %>%
  mutate(PUApersonaje = generate_id("PUApersonaje", idPersonaje), .before = idPersonaje) %>%
  mutate(PUAactividad = generate_id("PUAactividad", idActividad), .before = idActividad) %>%
  select(-idPersonaje, -idActividad) %>%
  mutate_all(as.character) %>%
  pivot_longer(!PUApersonaje, names_to = "category", values_to = "value") %>%
  mutate(tablaFuente = "personaje_actividad")

personajeAcumulativo <- personajeAcumulativo %>%
  add_row(personaje_actividad) %>%
  arrange(PUApersonaje)

personaje_alias <- read_delim("Data/raw_PUA/personaje_alias.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
personaje_caracteristica <- read_delim("Data/raw_PUA/personaje_caracteristica.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
personaje_cargo <- read_delim("Data/raw_PUA/personaje_cargo.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
personaje_disciplina <- read_delim("Data/raw_PUA/personaje_disciplina.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
personaje_fuente <- read_delim("Data/raw_PUA/personaje_fuente.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
personaje_lugar <- read_delim("Data/raw_PUA/personaje_lugar.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
personaje_nisba <- read_delim("Data/raw_PUA/personaje_nisba.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
personaje_obra <- read_delim("Data/raw_PUA/personaje_obra.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
personaje_referencia <- read_delim("Data/raw_PUA/personaje_referencia.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
personaje_relacion <- read_delim("Data/raw_PUA/personaje_relacion.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)




write_delim(personajeAcumulativo, "Data/PUA_processed/PUApersonaje_acumulativo.tsv", delim = "\t")



#### FOR CLASS 5 on DATAFRAMES

library(tidyverse)
library(readr)
PUA_personaje <- read_delim("Data/PUA_processed/PUA_personaje.tsv", 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)

write_delim(PUA_personaje, "Data/Pua_Processed/PUA_personaje_saved.tsv", delim = "\t")


PUA <- readRDS("Data/PUA_processed/PUA_allDataTables_asList.rds")

PUA$personaje

View(PUA$personaje)

View(PUA)

PUA$personaje$nombreA

length(PUA$personaje$nombreA)

mean(PUA$personaje$edad, na.rm = TRUE)

ages <- PUA$personaje %>%
  filter(edad > 0) %>%
  filter(!is.na(edad))

mean(ages$edad)


OpenITI <- mutate(OpenITI, date = as.integer(date))
openITI_after701 <- filter(OpenITI, date >= 701)
openITI_8thCent <- filter(openITI_after701, date <= 800)
openITI_8thCent_pri <- filter(openITI_8thCent, status == "pri")
numberOfBooks <- nrow(openITI_8thCent)

numberOfBooks

septuagenarians <- PUA$personaje
septuagenarians <- filter(septuagenarians, muerte >= 501, muerte <= 600)
septuagenarians <- filter(septuagenarians, edad >= 70, edad <= 79)
septuagenarians <- nrow(septuagenarians)

septuagenarians <- PUA$personaje %>%
  filter(muerte >= 501, muerte <= 600) %>%
  filter(edad >= 70, edad <= 79) %>%
  nrow()

sixthCentAH <- PUA$personaje %>%
  filter(muerte >= 501, muerte <= 600)

cent_600AH <- PUA$personaje %>%
  filter(muerte >= 501, muerte <= 600)


septuagenarians600AH <- PUA$personaje %>%
  filter(muerte >= 501, muerte <= 600) %>%
  filter(edad >= 70, edad <= 79)


temp <- PUA$personaje %>%
  filter(muerte >= 501) %>%
  filter(muerte <  600)


personajeLite <- PUA$personaje %>%
  select(idPersonaje, idFamilia, nombreA, nacimiento, muerte, edad)

personaje_IDs <- PUA$personaje %>% select(starts_with("id"))
personaje_noComments <- PUA$personaje %>% select(!ends_with("_comentario"))

v1 <- c(0,0,0,0,0,0,67,70,90)
v2 <- c(NA,NA,NA,NA,NA,NA,67,70,90) # OR: va <- na_if(v1, 0)

mean(v1) # 25.22222
mean(v2, na.rm = TRUE) # 75.66667


personajeLite <- personajeLite %>%
  mutate(nacimiento = na_if(nacimiento, 0)) %>%
  mutate(muerte = na_if(muerte, 0)) %>%
  mutate(edad = na_if(edad, 0))


personajeLite <- PUA$personaje %>%
  select(idPersonaje, idFamilia, nombreA, nacimiento, muerte, edad) %>%
  mutate(nacimiento = na_if(nacimiento, 0)) %>%
  mutate(muerte = na_if(muerte, 0)) %>%
  mutate(edad = na_if(edad, 0))


personajeLite <- PUA$personaje %>%
  select(idPersonaje, idFamilia, nombreA, nacimiento, muerte, edad) %>%
  mutate(nacimiento = na_if(nacimiento, 0),
         muerte = na_if(muerte, 0),
         edad = na_if(edad, 0))


AH2CEa <- function(AH) {
  CE <- round(AH - AH/33 + 622)
  return(CE)
}

AH2CE <- function(AH) {
  CE <- round(AH - AH/33 + 622)
  AH <- ifelse(AH == 0, 1, AH)
  final <- ifelse(is.na(AH), NA, paste0(AH, "AH/", CE, "CE"))
  return(final)
}


AH2CE <- function(AH) {
  CE <- round(AH - AH/33 + 622)
  AH <- ifelse(AH == 0, 1, AH)
  final <- paste0(AH, "AH/", CE, "CE")
  return(final)
}


personajeLite <- personajeLite %>%
  mutate(muerteCE = ifelse(is.na(muerte), NA, AH2CE(muerte)))


personajeLiteEN <- personajeLite %>%
  rename(idPerson = idPersonaje,
         idFamily = idFamilia,
         nameA = nombreA,
         born = nacimiento,
         died = muerte,
         diedCE = muerteCE,
         age = edad)


oldestInFamilies <- personajeLite %>%
  filter(idFamilia != 0) %>%
  filter(!is.na(edad)) %>%
  arrange(idFamilia, desc(edad))


personajeLite %>% group_by(idFamilia)


OpenITI_booksByAuthors <- OpenITI_robust %>%
  filter(status == "pri") %>%
  group_by(authorUri) %>%
  summarize(booksNumber = n()) %>%
  arrange(desc(booksNumber))

miembrosDeFamilias <- personajeLite %>%
  filter(idFamilia != 0) %>%
  group_by(idFamilia) %>%
  summarize(miembros = n()) %>%
  arrange(desc(miembros))



familiasEdadPromedio <- personajeLite %>%
  filter(idFamilia != 0) %>%
  group_by(idFamilia) %>%
  summarize(edadPromedio = mean(edad, na.rm = TRUE),
            miembros = n()) %>%
  arrange(desc(edadPromedio))


familiasMasAntiguo <- personajeLite %>%
  filter(idFamilia != 0) %>%
  group_by(idFamilia) %>%
  top_n(1, wt = edad) %>%
  arrange(desc(edad)) %>%
  select(-nombreA)

familiaDataLite <- PUA$familia %>%
  select(idFamilia, nombreE)

familiaData


miembrosDeFamilias <- miembrosDeFamilias %>%
  left_join(familiaDataLite, by = c("idFamilia" = "idFamilia"))

miembrosDeFamilias

familiasEdadPromedio <- familiasEdadPromedio %>%
  left_join(familiaDataLite, by = c("idFamilia" = "idFamilia"))

familiasEdadPromedio

familiasMasAntiguo <- familiasMasAntiguo %>%
  left_join(familiaDataLite, by = c("idFamilia" = "idFamilia")) %>%
  select(-nacimiento, -muerte)

familiasMasAntiguo

# places, people, centuries

lugarNombres <- PUA$lugar %>%
  select(idLugar, nombre_castellano)

lugar <- PUA$personaje_lugar %>%
  select(idLugar, idPersonaje, idRelacion) %>%
  left_join(personajeLite, by = c("idPersonaje" = "idPersonaje")) %>%
  select(-nombreA, -nacimiento, -edad, -muerteCE) %>%
  mutate(century = plyr::round_any(muerte, 100, f = ceiling)) %>%
  filter(!is.na(century))

lugarTop10 <- lugar %>%
  group_by(idLugar) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  top_n(10, wt = total)

lugarSummary <- lugar %>%
  group_by(idLugar, century) %>%
  summarize(individuals = n()) %>%
  filter(idLugar %in% lugarTop10$idLugar) %>%
  left_join(lugarNombres) %>%
  arrange(nombre_castellano)

# pivot
lugarSummaryWide <- lugarSummary %>%
  ungroup() %>%
  select(century, individuals, nombre_castellano) %>%
  pivot_wider(names_from = nombre_castellano, values_from = individuals)


# pivot longer
lugarSummaryLong <- lugarSummaryWide %>%
  pivot_longer(!century, names_to = "places", values_to = "individuals") %>%
  arrange(places)
