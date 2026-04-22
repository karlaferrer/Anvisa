library (readxl)
library (janitor)
library(dplyr)


# Organização -------------------------------------------------------------

cases <- read_excel("GLP/BR_VigiLyze.xlsx", sheet = "Cases")
drugs <- read_excel("GLP/BR_VigiLyze.xlsx", sheet = "Drugs")
reactions <-  read_excel("GLP/BR_VigiLyze.xlsx", sheet = "Reactions")

#Clean column names
cases <- clean_names(cases)
drugs <- clean_names(drugs)
reactions <- clean_names(reactions)

#selecionar casos de interesse
drugs2 <- drugs |>
  filter(who_drug_active_ingredient_variant %in%
           c("Semaglutide",
             "Dulaglutide",
             "Liraglutide",
             "Lixisenatide",
             "Tirzepatide"
             #"Insulin degludec;Liraglutide")
           ))|>
  #filter (Role %in% "Suspect") |>
  distinct()

#pares unicos medicamento e ID
#tabela pro join de características
unicos <- drugs2 |> 
  select(umc_report_id, who_drug_active_ingredient_variant) |> 
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
             
#juntar dados demográficos
unicos <- unicos |> 
  left_join(cases, by ="umc_report_id", keep = F) 

# Tabela 1 ----------------------------------------------------------------

#proporcao de sexo feminino  
round(prop.table(table(unicos$who_drug_active_ingredient_variant_x, unicos$sex),1)*100,1)


#total de NA para idade
sum(!is.na(unicos$height_cm))

# idade, sexo, IMC por medicamento
t1 <- unicos |> 
  group_by(who_drug_active_ingredient_variant.x) |> 
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
names(t2)[1] <- "who_drug_active_ingredient_variant.x"

t1 <- rbind(t1,t2)
rm(t2)

# Curva epidemica ---------------------------------------------------------
library (lubridate)

#drugs2$teste <- as.Date(drugs2$start_date, format = c("%Y-%m-%d"))

library(dplyr)
library(stringr)
library(lubridate)

#padronizar campos de dados para pegar o ano pelo menos
unicos <- unicos |> 
  mutate(
    data_pad = ymd(vigi_base_initial_date),
    month_yr = format_ISO8601(data_pad, precision = "ym")
  )


freq_mes <- unicos |> 
  group_by(month_yr, who_drug_active_ingredient_variant.x) |> 
  summarise( total =n())

library (ggplot2)
library(RColorBrewer)
freq_mes |> 
ggplot() +
  #geom_line(size = 0.3)+
  geom_line(aes(x= ym(month_yr),y=total,
            group = who_drug_active_ingredient_variant.x,
            color = who_drug_active_ingredient_variant.x)
            )+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  xlab(" ")+
  #ylim(c(0,550))+
  ylab("Total")+
  #labs(colour = NULL) +
  #scale_color_manual(values=c("black", "darkred")) +
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size = 10)) +
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_color_brewer(palette = "Set1")


# IMC mediana -------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)

#grafico de colunas
unicos |> 
  filter(who_drug_active_ingredient_variant.x %in% 
           c("Semaglutide", "Liraglutide", "Tirzepatide")) |> 
  mutate(
    IMC = weight_kg / (height_cm/100)^2,
    ano = year(data_pad)
  ) |> 
  group_by(ano, who_drug_active_ingredient_variant.x) |> 
  summarise(
    med_IMC = median(IMC, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = factor(ano), y = med_IMC,
             fill = who_drug_active_ingredient_variant.x)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom")+
  theme(text = element_text(size = 11)) +
  xlab("Ano")+
  ylab("Mediana do IMC")+ 
  #theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme_bw()+
  theme(legend.title = element_blank())


#boxplot
unicos |>
  ggplot(aes(x = factor(ano), y = IMC,
             fill = who_drug_active_ingredient_variant.x)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(10, 60)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 11)
  ) +
  labs(x = "Ano", y = "IMC")
