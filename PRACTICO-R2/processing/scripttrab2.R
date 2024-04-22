library(pacman)
p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven, summarytools, kableExtra, sjPlot, corrplot, sessioninfo, ggplot2)

# Instalar y cargar devtools
install.packages("devtools")
devtools::install_github("strengejacke/strengejacke", force = TRUE)
library(strengejacke)

# se cargan los datos
base <- read_dta("C:/Users/alond/Desktop/OFC-R/Trabajo-R-UAH-2024/PRACTICO-R2/input")

# aquí separo las variables de inetrés para luego hacer la bae de datos procesada
proc_data <- base %>%
  select(o6, o7, sexo, edad)


# la 06 es numérica, así que paso a convertirla en character antes de codoficicar
proc_data$o6 <- as.character(proc_data$o6)


proc_data$o6 <- recode(proc_data$o6, "-2='NA'; -1='NA'; 1='Sí'; 2='No'; 3='No'; 4='No'")


proc_data$o6 <- recode(proc_data$o6, "-2=NA; -1=NA; 1='Sí'; 2='No'; 3='No'; 4='No'")
proc_data$o7 <- recode(proc_data$o7, "-2=NA; -1=NA; 1=3; 2=2; 3=1; 4=0")
proc_data$edad <- recode(proc_data$edad, "-2=NA; -1=NA; 1=3; 2=2; 3=1; 4=0")
proc_data$sexo <- as.character(proc_data$sexo)  # Convertir la columna 'sexo' a tipo character
proc_data$sexo <- recode(proc_data$sexo, "-2='NA'; -1='NA'; 1='Hombre'; 2='Mujer'")

proc_data$razon <- set_labels(proc_data$razon,
                              labels=c( "Está en espera de una confirmación"=1,2,
                                        "Se dedica a labores de cuidado"=3,4,5,
                                        "Se dedica al trabajo doméstico"=10,
                                        "Dificultades para conseguir trabajo"=6,7,8,9,14,
                                        "Otro motivo"=11,12,13,15,16,17,18,19))


# para trabajar los datos procesados, crearé un objeto que recopile los datos de las variables
proc_data$muj_trab <- ifelse(proc_data$sexo == "Mujer", proc_data$o6, NA)
write.csv(proc_data, file = "C:/Users/alond/Desktop/OFC-R/Trabajo-R-UAH-2024/PRACTICO-R2/input/proc_data.csv", row.names = FALSE)

#recod
proc_data <- proc_data %>%
  mutate(across(starts_with("o"), ~ recode(., "c(-2,-1)=NA; 1=3; 2=2; 3=1; 4=0"))) %>%
  set_na(na = c(-2, -1)) %>%
  rename(busc_trab = o6, razon = o7) %>%
  mutate_at(vars(busc_trab, razon), set_labels, labels = c("Buscó trabajo en el último tiempo" = 1, "No buscó trabajo en el último tiempo" = 2)) %>%
  mutate_at(vars(razon), set_labels, labels = c("Está en espera de una confirmación" = 1:2, "Se dedica a labores de cuidado" = 3:5, "Se dedica al trabajo doméstico" = 10, "Dificultades para conseguir trabajo" = 6:9, "Otro motivo" = 11:19)) %>%
  mutate(edad = cut(edad, c(18, 25, 35, 45, 55, Inf), labels = c("18-24 años", "25-34 años", "35-44 años", "45-54 años", "55 o más años"), include.lowest = TRUE)) %>%
  mutate(sexo = set_labels(sexo, labels = c("Hombre" = 1, "Mujer" = 2)))

# tabla cruzada
tab_desc <- sjt.xtab(proc_data$o7, proc_data$edad, encoding = "UTF-8")
ruta_archivo <- "C:/Users/alond/Desktop/OFC-R/Trabajo-R-UAH-2024/PRACTICO-R2/output/ELSOC_ess_merit2016.RData"
save(proc_data, tab_desc, file = ruta_archivo)

proc_data <- proc_data %>%
  rename_with(~ tolower(gsub("o", "", .x)), starts_with("o")) %>%
  set_labels(busc_trab = "Búsqueda de trabajo", razon = "Por qué no ha buscado", edad = "Edad", sexo = "Sexo")

# Mostrar frecuencias
frq(proc_data$busc_trab)
frq(proc_data$razon)
frq(proc_data$edad)
frq(proc_data$sexo)

tab1 <- stargazer(proc_data, type = "text")

# ggplot para crear un gráfico de barras
ggplot(proc_data, aes(x = busc_trab, fill = sexo)) +
  geom_bar() +
  labs(title = "Mujeres fuera del mercado laboral",
       x = "Búsqueda de trabajo",
       y = "Cantidad") +
  scale_fill_manual(values = c("Hombre" = "coral", "Mujer" = "purple"))

ggplot(proc_data, aes(x = razon, fill = sexo)) +
  geom_bar() +
  labs(title = "Mujeres fuera del mercado laboral",
       x = "Razones",
       y = "Cantidad") +
  scale_fill_manual(values = c("Hombre" = "coral", "Mujer" = "purple"))



# Mostrar resúmenes agrupados
proc_data %>% 
  group_by(sexo, busc_trab) %>% 
  summarise(mean(busc_trab, na.rm = TRUE)) %>%
  group_by(sexo, edad) %>% 
  summarise(mean(razon, na.rm = TRUE))
## me di cuenta que este gráfico no sirve mucho

## asi que probaré con el siguiente
graph3 <- proc_data %>% ggplot(aes(x = busc_trab, fill = sexo)) + 
  geom_bar() +
  xlab("Búsqueda de trabajo") +
  ylab("Cantidad") + 
  labs(fill = "Sexo") +
  scale_fill_discrete(labels = c('Hombre', 'Mujer'))

graph3

tabla <- sjt.xtab(proc_data$razon, proc_data$sexo, encoding = "UTF-8")

# Generar la tabla con sjt.xtab
tabla <- sjt.xtab(proc_data$razon, proc_data$sexo, encoding = "UTF-8")

# Guardar la tabla en formato PDF
pdf("ruta_archivo/tabla.pdf")
print(tabla)
dev.off()

