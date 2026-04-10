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
