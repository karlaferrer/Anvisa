library (readxl)
library (janitor)
library(dplyr)


# Organização -------------------------------------------------------------

cases <- read_excel("GLP/BR_VigiLyze.xlsx", sheet = "Cases")
drugs <- read_excel("GLP/BR_VigiLyze.xlsx", sheet = "Drugs")

#separar medicamentos por campos
library(tidyr)
#v <- setNames(rep(NA, 21), paste0("d", 1:21))
#v <- names(v)
#cases2 <- cases |> 
#separate(col=`WHODrug active ingredient variant`, 
#         into = v,
#         sep = "\r\n")
#frequencia por campo
#t1 <- as.data.frame(table(cases2$d1))
#t10 <- as.data.frame(table(cases2$d1))
#t21 <- as.data.frame(table(cases2$d21))

library(dplyr)
library (stringr)
cases2 <- cases |> 
  mutate(
    Semaglutide = case_when(
  str_detect(`WHODrug active ingredient variant`, "Semaglutide") == TRUE ~ 1
  ),
  Dulaglutide = case_when(
    str_detect(`WHODrug active ingredient variant`, "Dulaglutide") == TRUE ~ 1
  ),
  Liraglutide = case_when(
    str_detect(`WHODrug active ingredient variant`, "Liraglutide") == TRUE ~ 1
  ),
  Lixisenatide = case_when(
    str_detect(`WHODrug active ingredient variant`, "Lixisenatide") == TRUE ~ 1
  ),
  Tirzepatide = case_when(
    str_detect(`WHODrug active ingredient variant`, "Tirzepatide") == TRUE ~ 1
  ),
  Insulin = case_when(
    str_detect(`WHODrug active ingredient variant`, "Insulin degludec;Liraglutide") == TRUE ~ 1
  )
  )  
  
#selecionar casos de interesse


#pares unicos medicamento e ID
#tabela pro join de características
unicos <- drugs2 |> 
  select(`UMC report ID`, `WHODrug active ingredient variant`) |> 
  distinct()

#casos com mais de um medicamento reportado
teste <- drugs2 |>              
group_by(`UMC report ID`, `WHODrug active ingredient variant`) |>
  count() |> 
  filter (n>1)

teste2 <- drugs2 |> 
  filter(drugs2$`UMC report ID` %in% teste$`UMC report ID`)

unicosm <- unicos |> 
  group_by(`UMC report ID`) |> 
  count() |> 
  filter (n>1)
             
which (drugs2$`UMC report ID`=="47909629")

cases[cases$`UMC report ID`=="47909629",]
  
# join n= 2627

unicos <- unicos |> 
  left_join(cases, by = (`UMC report ID`), keep = F) 

#clean names no unicos
unicos <- clean_names(unicos)


# Tabela 1 ----------------------------------------------------------------

#proporcao de sexo feminino  
round(prop.table(table(unicos$who_drug_active_ingredient_variant_x, unicos$sex),1)*100,1)


#total de NA para idade
sum(!is.na(unicos$height_cm))

# idade, sexo, IMC por medicamento
t1 <- unicos |> 
  group_by(who_drug_active_ingredient_variant_x) |> 
  summarise(
    med_age = median(age, na.rm = TRUE),
    n_age = sum(!is.na(age)),
    prop_fem = mean(sex == "Female", na.rm = TRUE)*100,
    med_IMC = median(weight_kg / (height_cm/100)^2 , na.rm = TRUE),
    n_weight = sum(!is.na(weight_kg)),
    n_height = sum(!is.na(height_cm)),
    Total = n(),
            )
  
# linha do total 
t2 <- unicos |> 
  #group_by(who_drug_active_ingredient_variant_x) |> 
  summarise(
    med_age = median(age, na.rm = TRUE),
    n_age = sum(!is.na(age)),
    prop_fem = mean(sex == "Female", na.rm = TRUE)*100,
    med_IMC = median(weight_kg / (height_cm/100)^2 , na.rm = TRUE),
    n_weight = sum(!is.na(weight_kg)),
    n_height = sum(!is.na(height_cm)),
    Total = n()
  )

t2 <- cbind("Total", t2)
names(t2)[1] <- "who_drug_active_ingredient_variant_x"

t1 <- rbind(t1,t2)
rm(t2)


# Curva epidemica ---------------------------------------------------------


