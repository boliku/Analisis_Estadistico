setwd("I:/Mi unidad/Postdoc/Colaboraciones/Carlos Gamarra")
tabla_general <- read.delim2("./tabla general todos los casos.xlsx", header= TRUE)

# Load necessary library
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

#########################
## Preprocessing data ====
#########################
# Some cells were without number, I replace by NA
tabla_general[tabla_general == ""] <- NA

# Change from character t factor 

# Assuming cols_to_convert is a vector containing the names of the columns you want to convert
cols_to_convert <- c("enfermedad", "enfermedad_dicotomic", "tratamiento",
                     "tratamiento_dicot", "pareja_sexual", "disfuncion", 
                     "disfuncion_dicotomic", "menopausia", "interrogacion", "deseo_interrogacion", "psicologo")  # Add all column names you want to convert

# Convert selected columns to factor
tabla_general <- tabla_general %>%
  mutate_at(vars(cols_to_convert), factor)

tabla_general$enfermedad_dicotomic <- as.factor(ifelse(tabla_general$enfermedad=="control","control", "NEO"))
# Filter table, keeping woman with cancer
tabla_NEO <- tabla_general[tabla_general$enfermedad_dicotomic=="NEO",]

hist(tabla_NEO$edad)
median(tabla_NEO$edad)
mean(tabla_NEO$edad)
# Convert the age column to a factor with levels <=48 and >48
tabla_NEO <- tabla_NEO %>%
  mutate(age_group = ifelse(edad <= 48, '<=48', '>48'))
#Agrego columna con atencion interdisciplinaria -> 1-> NO, >1 SI
tabla_NEO$interdisciplinaria <- ifelse(tabla_NEO$cant_profesionales==1,"NO", "SI")

# Filter table, keeping woman with each cancer
tabla_ovario <- tabla_NEO[tabla_NEO$enfermedad=="ovario",]
tabla_endometrio <- tabla_NEO[tabla_NEO$enfermedad=="Endometrio",]
tabla_cervix <- tabla_NEO[tabla_NEO$enfermedad=="Cervix" | tabla_NEO$enfermedad=="Cervix tiene ovarios",]
tabla_cervix_sinOva <- tabla_NEO[tabla_NEO$enfermedad=="Cervix",]
tabla_cervix_conOva <- tabla_NEO[tabla_NEO$enfermedad=="Cervix tiene ovarios",]

#Filter table to keep menopausic in Control
tabla_general_Meno <- tabla_general[is.na(tabla_general$menopausia)|tabla_general$menopausia=="SI",]

# Filter control patients
tabla_control <- tabla_general %>%
  filter(enfermedad == "control")

# Loop through each column and perform wilcox-test
si_group <- tabla_control %>% 
  filter(menopausia == "SI")
no_group <- tabla_control %>% 
  filter(menopausia == "NO")

#######################
## An�lisis descriptivo y test de normalidad====
#######################

summary(tabla_general$edad)
summary(tabla_NEO$edad)
t.test(tabla_general$edad, tabla_NEO$edad)
wilcox.test(tabla_general$edad, tabla_NEO$edad)

# Comprobaci�n  distribucion normal y homogeneidad de varianza para cada columna numerica
normality_cols <- c("edad", "deseo_corregido", "exitacion_corregido", "lubricacion_corregido", 
                     "orgasmo_corregido", "s_global_corregido", "dolor_corregido", "total_corregido")


### Normality test para pacientes control ====
# Prueba de distribucion normal: Shapiro Wilk
for (col in normality_cols) {
  normal_test <- shapiro.test(tabla_control[,col])
  cat("Nomality test results for ", col, ":\n")
  print(normal_test)
}

# Normality test para pacientes control menopausicas
# Prueba de distribucion normal: Shapiro Wilk
for (col in normality_cols) {
  normal_test <- shapiro.test(no_group[,col])
  cat("Nomality test results for ", col, ":\n")
  print(normal_test)
}

# Normality test para pacientes control NO menopausicas
# Prueba de distribucion normal: Shapiro Wilk
for (col in normality_cols) {
  normal_test_meno <- shapiro.test(si_group[,col])
  cat("Nomality test results for ", col, ":\n")
  print(normal_test_meno)
}

#All values had not normal distribution in complete control cohort and in menopausal women
# Deseo y edad TIENENE distribucion normal en NO menopausicas

#Pruebas de homogeneidad de variancias: bartlett.test -> No hace falta que que no cumplen con normailidad

### Normality test para pacientes NEO ====
# Prueba de distribucion normal: Shapiro Wilk
for (col in normality_cols) {
  normal_test <- shapiro.test(tabla_NEO[,col])
  cat("Nomality test results for ", col, ":\n")
  print(normal_test)
}
#Ninguna tiene distribucion normal

# Normality test para pacientes ovarium cancer
# Prueba de distribucion normal: Shapiro Wilk
for (col in normality_cols) {
  normal_test <- shapiro.test(tabla_ovario[,col])
  cat("Nomality test results for ", col, ":\n")
  print(normal_test)
}
#La unica que tiene distribucion normal es edad
#Pruebas de homogeneidad de variancias: bartlett.test -> No hace falta que que no cumplen con normailidad

# Normality test para pacientes Endometrium cancer
# Prueba de distribucion normal: Shapiro Wilk
for (col in normality_cols) {
  normal_test <- shapiro.test(tabla_endometrio[,col])
  cat("Nomality test results for ", col, ":\n")
  print(normal_test)
}
#La unica que NO TIENE distribucion normal es dolor_corregido
# Pruebas de homogeneidad de variancias: bartlett.test -> 
# Ver si hace falta hacerlo si otro grupo a ser comparado tiene tmb distribucion normal
# Ya que este test compara la varianza entre los grupos a ser comparados

# Normality test para pacientes Cervix cancer
# Prueba de distribucion normal: Shapiro Wilk
for (col in normality_cols) {
  normal_test <- shapiro.test(tabla_cervix[,col])
  cat("Nomality test results for ", col, ":\n")
  print(normal_test)
}
#El unico que TIENE distribuci�n normal es la edad

# Normality test para pacientes Cervix con ovario cancer
# Prueba de distribucion normal: Shapiro Wilk
for (col in normality_cols) {
  normal_test <- shapiro.test(tabla_cervix_conOva[,col])
  cat("Nomality test results for ", col, ":\n")
  print(normal_test)
}
# Deseo corregido y orgasmo corregido NO TIENEN distribucion normal

# Normality test para pacientes Cervix sin ovario cancer
# Prueba de distribucion normal: Shapiro Wilk
for (col in normality_cols) {
  normal_test <- shapiro.test(tabla_cervix_sinOva[,col])
  cat("Nomality test results for ", col, ":\n")
  print(normal_test)
}
# s_global_corregido, exitacion_corregido, deseo_corregido y edad TIENEN distribucion normal



################################
## An�lisis de comparaci�n (para variables continuas: Wilcox-test y ANOVA)====
# T-test or Wilcox Test to compare disfunction values between different patients
################################

### Statistic comparison between menopausicas and pre-menopausicas en control on total and different domains of sexual dysfunction====

# Define columns for comparison
comparison_cols <- c("deseo_corregido", "exitacion_corregido", "lubricacion_corregido", 
                     "orgasmo_corregido", "s_global_corregido", "dolor_corregido", "total_corregido")

### Wilcox test between menopausal vs non menopausal controls ====

for (col in comparison_cols) {
  # Perform t-test
  wilcox_test_result <- wilcox.test(si_group[,col], no_group[,col])
  # Print results for current column
  cat("Wilcoxon test results for ", col, ":\n")
  print(wilcox_test_result)
}

"""
Diferencias en lubricacion y s_global entre menopausicas y no meopausicas en control
Wilcoxon test results for  lubricacion_corregido :

	Wilcoxon rank sum test with continuity correction

data:  si_group[, col] and no_group[, col]
W = 165.5, p-value = 0.04669
alternative hypothesis: true location shift is not equal to 0

Wilcoxon test  results for  s_global_corregido :

	Wilcoxon rank sum test with continuity correction

data:  si_group[, col] and no_group[, col]
W = 162, p-value = 0.03773
alternative hypothesis: true location shift is not equal to 0

"""
### Statistic comparison between control no menopausicas vs NEO on disfunction domains====

for (col in comparison_cols) {
  # Perform t-test
  wilcox_test_result <- wilcox.test(no_group[,col], tabla_NEO[,col])
  # Print results for current column
  cat("Wilcoxon test results for ", col, ":\n")
  print(wilcox_test_result)
}

"""
Wilcoxon test results for  deseo_corregido :

	Wilcoxon rank sum test with continuity correction

data:  no_group[, col] and tabla_NEO[, col]
W = 361, p-value = 0.05176
alternative hypothesis: true location shift is not equal to 0

Wilcoxon test results for  exitacion_corregido :

	Wilcoxon rank sum test with continuity correction

data:  no_group[, col] and tabla_NEO[, col]
W = 368.5, p-value = 0.03753
alternative hypothesis: true location shift is not equal to 0

Wilcoxon test results for  lubricacion_corregido :

	Wilcoxon rank sum test with continuity correction

data:  no_group[, col] and tabla_NEO[, col]
W = 390, p-value = 0.01175
alternative hypothesis: true location shift is not equal to 0

Wilcoxon test results for  orgasmo_corregido :

	Wilcoxon rank sum test with continuity correction

data:  no_group[, col] and tabla_NEO[, col]
W = 380.5, p-value = 0.01906
alternative hypothesis: true location shift is not equal to 0

Wilcoxon test results for  s_global_corregido :

	Wilcoxon rank sum test with continuity correction

data:  no_group[, col] and tabla_NEO[, col]
W = 414.5, p-value = 0.002846
alternative hypothesis: true location shift is not equal to 0

Wilcoxon test results for  dolor_corregido :

	Wilcoxon rank sum test with continuity correction

data:  no_group[, col] and tabla_NEO[, col]
W = 349, p-value = 0.08144
alternative hypothesis: true location shift is not equal to 0

Wilcoxon test results for  total_corregido :

	Wilcoxon rank sum test with continuity correction

data:  no_group[, col] and tabla_NEO[, col]
W = 385.5, p-value = 0.01668
alternative hypothesis: true location shift is not equal to 0
"""

# Graficos 
# Un grafico para cada valor de disfuncion
# Define the violin plot function para enfermedad dicot�mica (control vs NEO)
create_violin_plot <- function(data, column_name, group) {
  plot <- ggplot(data, aes(x = "", y = .data[[column_name]], fill = .data[[group]])) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.1, fill = "white") +
    labs(title = paste0("Violin Plot for ", column_name),
         x = "",
         y = column_name,
         fill = group) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    facet_grid(. ~ .data[[group]]) ##use facet_wrap() or facet_grid() to separate the plots based on the levels of the group variable
  
  print(plot)  # Ensure the plot is displayed
}

# Define the column names directly
comparision_cols <- c("deseo_corregido", "exitacion_corregido", "lubricacion_corregido", 
                      "orgasmo_corregido", "s_global_corregido", "dolor_corregido", "total_corregido")

# Loop through each column name (dysfunction domains) and create the plot for menopausicals vs non menopausicals
for (column_name in comparision_cols) {
  create_violin_plot(tabla_general_Meno, column_name)
}

# Loop through each column name (dysfunction domains) and create the plot for control menopausical vs NEO

for (column_name in comparision_cols) {
  create_violin_plot(tabla_general_Meno, column_name, "enfermedad_dicotomic")
}


# Creo una funcion para un grafico multiple donde aparezcan todos los datos de disfuncion
create_grouped_violin_plot <- function(data, group, column_names) {
  # Reshape the data into long format
  data_long <- gather(data, key = "variable", value = "value", all_of(column_names))
  pastel_palette <- brewer.pal(n = nlevels(data_long[[group]]), name = "Pastel1")
  
  # Create the grouped violin plot
  plot <- ggplot(data_long, aes(x = as.factor(.data[[group]]), y = value, fill = as.factor(.data[[group]]))) +
    geom_violin(scale ="width", trim = FALSE, adjust = .8) +
     geom_boxplot(width = 0.1, fill = "white") +
    labs(title = "Grouped Violin Plots",
         x = "Groups",
         y = "FSFI Values",
         fill = "Groups") +
    theme_minimal() +
    theme(axis.text.x = element_blank()) +
    facet_wrap(~ variable, ncol = length(column_names)) + 
    scale_y_continuous(limits = c(0, NA)) + 
    scale_fill_manual(values = pastel_palette)
  print(plot) 
}

create_grouped_violin_plot(tabla_general_Meno,
                           group = "enfermedad_dicotomic",
                           column_name = c("deseo_corregido", "exitacion_corregido", "lubricacion_corregido", "orgasmo_corregido", "s_global_corregido", "dolor_corregido"))


# p value 0.05176 deseo
# p-value = 0.03753 excitacion
# p-value = 0.01175 lubricacion
# p-value = 0.01906 orgasmo
#p-value = 0.002846 satisfaccion
#p-value = 0.08144 dolor
#p-value = 0.01668 total


### Kruskall Wallis (ANOVA no parametrico) to compare Control, ovario, endometrio, cervix ====
for (col in comparison_cols) {
  kruskal_result <- kruskal.test(tabla_general_Meno[, col] ~ tabla_general_Meno$enfermedad)
  pairwise_result <- pairwise.wilcox.test(tabla_general_Meno[, col], tabla_general_Meno$enfermedad, paired = FALSE)
  cat("Kruskal test results for", col, ":\n")
  print(kruskal_result)
  cat("Pairwise test results for multiple comparisons for", col, ":\n")
  print(pairwise_result)
}

# Plot for Kruskal Wallis

create_grouped_violin_plot(tabla_general_Meno,
                           group = "enfermedad",
                           column_name = c("deseo_corregido", "exitacion_corregido", "lubricacion_corregido", "orgasmo_corregido", "s_global_corregido", "dolor_corregido"))




################################
## An�lisis de comparaci�n entre variables dicot�micas y continuas ====
# T-test or Wilcox Test to compare dichotomic variable (yes or no) with continuous values of disfunction
################################

# Define a function to perform Wilcoxon test
wilcoxon_test_function <- function(data, column, condition_column, condition_value) {
  # Extract values based on condition
  condition_true <- data[data[[condition_column]] == condition_value, column]
  condition_false <- data[data[[condition_column]] != condition_value, column]
  
  # Perform Wilcoxon test
  wilcox_result <- wilcox.test(condition_true, condition_false)
  
  # Return the result
  return(wilcox_result)
}

# Loop through columns of each sexual domain for "interrogacion"
for (col in comparison_cols) {
  # Perform Wilcoxon test for each column
  wilcox_result <- wilcoxon_test_function(tabla_NEO, col, "interrogacion", "SI")
  
  # Print results
  cat("Wilcoxon test results for", col, ":\n")
  print(wilcox_result)
}

# Loop through columns of each sexual domain for "psicologo"
for (col in comparison_cols) {
  # Perform Wilcoxon test for each column
  wilcox_result <- wilcoxon_test_function(tabla_NEO, col, "psicologo", "SI")
  
  # Print results
  cat("Wilcoxon test results for", col, ":\n")
  print(wilcox_result)
}

# Loop through columns of each sexual domain for "interdisciplinaria"
for (col in comparison_cols) {
  # Perform Wilcoxon test for each column
  wilcox_result <- wilcoxon_test_function(tabla_NEO, col, "interdisciplinaria", "SI")
  
  # Print results
  cat("Wilcoxon test results for", col, ":\n")
  print(wilcox_result)
}


################################
## An�lisis de asociaci�n para variables dicot�micas (Fisher's test) ====
# Test de Fisher
################################

# Function to perform Fisher's exact test
perform_fisher_test <- function(data, var1, var2) {
  # Remove rows with NA in either of the variables
  filtered_data <- data %>% filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
  table <- table(filtered_data[[var1]], filtered_data[[var2]])
  test_result <- fisher.test(table)
  list(table = table, test_result = test_result)
}

# Perform Fisher's exact test for other parameters
# Replace 'other_parameter' with the actual column name of the other parameters you want to test
other_parameters <- c("interrogacion", "pareja_sexual", "psicologo", "interdisciplinaria", "tratamiento_dicot", "deseo_interrogacion")

# Function to perform Fisher's exact test on all parameters, excluding NA values
perform_tests <- function(data, parameters) {
  results <- list()
  for (param in parameters) {
    test_result <- perform_fisher_test(data, "disfuncion", param)
    results[[param]] <- test_result
  }
  return(results)
}

# Function to plot the results
plot_results <- function(table, title, p_value) {
  df <- as.data.frame(as.table(table))
  ggplot(df, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste0(title, "\nP-value: ", format.pval(p_value, digits = 3)),
         x = names(dimnames(table))[1], y = "Count", fill = names(dimnames(table))[2]) +
    theme_minimal()
}

# Association between "disfuncion" and other parameters in Complete cohort
results_complete <- perform_tests(tabla_NEO, other_parameters)
print("Fisher's test for other parameters in the complete cohort:")
for (param in names(results_complete)) {
  cat("\nParameter:", param, "\n")
  print(results_complete[[param]]$table)
  print(results_complete[[param]]$test_result)
  # Plot results for each parameter in the complete cohort
  plot_results(results_complete[[param]]$table, paste("Complete Cohort: Sexual Dysfunction vs", param), results_complete[[param]]$test_result$p.value)
}

# Association between "disfuncion" and other parameters in Patients aged <= 48
results_le50 <- perform_tests(tabla_NEO %>% filter(age_group == '<=48'), other_parameters)
print("Fisher's test for other parameters in patients aged <= 48:")
for (param in names(results_le50)) {
  cat("\nParameter:", param, "\n")
  print(results_le50[[param]]$table)
  print(results_le50[[param]]$test_result)
  plot_results(results_le50[[param]]$table, paste("Patients Aged <= 48: Sexual Dysfunction vs", param), results_le50[[param]]$test_result$p.value)
  # plot no se produce automaticamente, se puede hacer manualmente
}

plot_results(results_le50[["interrogacion"]]$table, paste("Patients Aged <= 48: Sexual Dysfunction vs", "interrogacion"), results_le50[["interrogacion"]]$test_result$p.value)
plot_results(results_le50[["interrogacion"]]$table)

# Association between "disfuncion" and other parameters in Patients aged > 48
results_gt50 <- perform_tests(tabla_NEO %>% filter(age_group == '>48'), other_parameters)
print("Fisher's test for other parameters in patients aged > 48:")
for (param in names(results_gt50)) {
  cat("\nParameter:", param, "\n")
  print(results_gt50[[param]]$table)
  print(results_gt50[[param]]$test_result)
  plot_results(results_gt50[[param]]$table, paste("Patients Aged > 48: Sexual Dysfunction vs", param), results_gt50[[param]]$test_result$p.value)
}

# Association between "disfuncion" and other parameters in Patients without pareja_sexual
results_notpart <- perform_tests(tabla_NEO %>% filter(pareja_sexual == 'NO'), other_parameters)
print("Fisher's test for other parameters in not partnered patients:")
for (param in names(results_notpart)) {
  cat("\nParameter:", param, "\n")
  print(results_notpart[[param]]$table)
  print(results_notpart[[param]]$test_result)
  plot_results(results_notpart[[param]]$table, paste("Patients not partnered: Sexual Dysfunction vs", param), results_notpart[[param]]$test_result$p.value)
}

# Association between "disfuncion" and other parameters in Patients with pareja_sexual
results_part <- perform_tests(tabla_NEO %>% filter(pareja_sexual == 'SI'), other_parameters)
print("Fisher's test for other parameters in partnered patients:")
for (param in names(results_part)) {
  cat("\nParameter:", param, "\n")
  print(results_part[[param]]$table)
  print(results_part[[param]]$test_result)
  plot_results(results_part[[param]]$table, paste("Patients not partnered: Sexual Dysfunction vs", param), results_part[[param]]$test_result$p.value)
}

# Association between "disfuncion" and other parameters in Patients with Endometrium cancer
results_endCa <- perform_tests(tabla_NEO %>% filter(enfermedad == 'Endometrio'), other_parameters)
print("Fisher's test for other parameters in patients with Endometrium cancer:")
for (param in names(results_endCa)) {
  cat("\nParameter:", param, "\n")
  print(results_endCa[[param]]$table)
  print(results_endCa[[param]]$test_result)
  plot_results(results_endCa[[param]]$table, paste("Patients with endometrium cancer: Sexual Dysfunction vs", param), results_endCa[[param]]$test_result$p.value)
}

# Association between "disfuncion" and other parameters in Patients with Ovarian cancer
results_ovCa <- perform_tests(tabla_NEO %>% filter(enfermedad == 'ovario'), other_parameters)
print("Fisher's test for other parameters in patients with Ovarian cancer:")
for (param in names(results_ovCa)) {
  cat("\nParameter:", param, "\n")
  print(results_ovCa[[param]]$table)
  print(results_ovCa[[param]]$test_result)
  plot_results(results_ovCa[[param]]$table, paste("Patients with ovarian cancer: Sexual Dysfunction vs", param), results_ovCa[[param]]$test_result$p.value)
}

# Association between "disfuncion" and other parameters in Patients with Cervical cancer
results_cerCa <- perform_tests(tabla_NEO %>% filter(enfermedad == 'Cervix'| enfermedad == 'Cervix tiene ovario'), other_parameters)
print("Fisher's test for other parameters in patients with Ovarian cancer:")
for (param in names(results_cerCa)) {
  cat("\nParameter:", param, "\n")
  print(results_cerCa[[param]]$table)
  print(results_cerCa[[param]]$test_result)
  plot_results(results_cerCa[[param]]$table, paste("Patients with cervical cancer: Sexual Dysfunction vs", param), results_cerCa[[param]]$test_result$p.value)
}

##########################
## Regresion multivariada
## Modelos lineales multivariados
##########################

tabla_NEO$interdisciplinaria<- as.factor(tabla_NEO$interdisciplinaria)
hist(tabla_NEO$total_corregido)
hist(log(tabla_NEO$total_corregido))
tabla_NEO$pareja_sexual <- relevel(
  tabla_NEO$pareja_sexual,
  ref = "SI" 
)
tabla_NEO$interrogacion <- relevel(
  tabla_NEO$interrogacion,
  ref = "SI" 
)


logit_model<- glm(total_corregido~ edad + interrogacion + deseo_interrogacion + psicologo + interdisciplinaria + enfermedad + tratamiento, data = tabla_NEO, family = Gamma(link = "inverse"))

logit_model<- glm(total_corregido~ edad + interrogacion, data = tabla_NEO, family = Gamma(link = "inverse"))

logit_model<- glm(total_corregido~ edad + interdisciplinaria, data = tabla_NEO, family = Gamma(link = "inverse"))
logit_model<- glm(total_corregido~ edad + tratamiento, data = tabla_NEO, family = Gamma(link = "inverse"))
logit_model<- glm(total_corregido~ edad + pareja_sexual, data = tabla_NEO, family = Gamma(link = "inverse"))
logit_model<- glm(total_corregido~ edad + pareja_sexual + interdisciplinaria, data = tabla_NEO, family = Gamma(link = "inverse"))
logit_model<- glm(total_corregido~ edad + pareja_sexual + interrogacion, data = tabla_NEO, family = Gamma(link = "inverse"))

logit_model<- glm(total_corregido~ psicologo, data = tabla_NEO, family = Gamma(link = "inverse"))
# Print the summary of the model
summary(logit_model)

#EL QUE DIO MEJOR VALOR CON MAS DE 1 VAIABLE INDEPENDIENTE FUE EL SGTE:
logit_model<- glm(total_corregido~ edad + pareja_sexual, data = tabla_NEO, family = Gamma(link = "inverse"))
summary(logit_model)

logit_model<- glm(total_corregido~ interdisciplinaria, data = tabla_NEO, family = Gamma(link = "inverse"))
summary(logit_model)


## Modelos lineales con mas de una variable independiente
lm_E_PS<-lm(total_corregido~ edad + pareja_sexual ,data = tabla_NEO)
summary(lm_E_PS)

lm_E_Interd<-lm(total_corregido~ edad + interdisciplinaria ,data = tabla_NEO)
summary(lm_E_Interd)

lm_E_Interrog<-lm(total_corregido~ edad + interrogacion ,data = tabla_NEO)
summary(lm_E_Interrog)

lm_E_Psico<-lm(total_corregido~ edad + psicologo ,data = tabla_NEO)
summary(lm_E_Psico)

lm_E_PS_Interrog <-lm(total_corregido~ edad + pareja_sexual + interrogacion ,data = tabla_NEO)
summary(lm_E_PS_Interrog)

#####################
## Correlaciones 
#####################
## Biserial Point correlation
tabla_NEO$interrogacion <- ifelse(tabla_NEO$interrogacion=="SI", 1, 0)

Bis_Cor_Total <- cor.test(tabla_NEO$total_corregido, tabla_NEO$interrogacion, use = c("all.obs"),method= "kendall")
Bis_Cor_Total

Bis_Cor_Deseo <- cor.test(tabla_NEO$deseo_corregido, tabla_NEO$interrogacion, use = c("all.obs"),method= "kendall")
Bis_Cor_Deseo

Bis_Cor_Exita <- cor.test(tabla_NEO$exitacion_corregido, tabla_NEO$interrogacion, use = c("all.obs"),method= "kendall")
Bis_Cor_Exita

Bis_Cor_Lubrica <- cor.test(tabla_NEO$lubricacion_corregido, tabla_NEO$interrogacion, use = c("all.obs"),method= "kendall")
Bis_Cor_Lubrica

Bis_Cor_Orgasmo <- cor.test(tabla_NEO$orgasmo_corregido, tabla_NEO$interrogacion, use = c("all.obs"),method= "kendall")
Bis_Cor_Orgasmo

Bis_Cor_Dolor <- cor.test(tabla_NEO$dolor_corregido, tabla_NEO$interrogacion, use = c("all.obs"),method= "kendall")
Bis_Cor_Dolor

Bis_Cor_Satisf <- cor.test(tabla_NEO$s_global_corregido, tabla_NEO$interrogacion, use = c("all.obs"),method= "kendall")
Bis_Cor_Satisf

#####################
## An�lisis de correspondencias m�ltiples 
#####################
#Los datos analzados deben ser factores
summary(tabla_NEO)
tabla_NEO$edad_dicotomic <- as.factor(ifelse(tabla_NEO$edad>=40, "A", "AM")) #A es para adultasy AM para adultas mayores
tabla_NEO$interrogacion <- as.factor(ifelse(tabla_NEO$interrogacion==1, "SI", "NO"))



library(FactoMineR)
library(factoextra)


MCA_tablaNEO<-MCA(tabla_NEO[,c(6,7,15,19:23)])

MCA_disf_interrog<-MCA(tabla_NEO[,c(15,19)])

MCA_tablaNEO<-MCA(tabla_NEO[,c(6,7,15,19:23)], graph=FALSE)
plot.MCA(MCA_tablaNEO,choix="ind",cex=0.7) #choix="ind" est� seleccionando el gr�fico de los individuos que incluye adem�s lascategor�as.

#Separacion x color segun si hay o no disfuncion sexual
plot.MCA(MCA_tablaNEO,choix="ind",invisible="var",habillage=3,cex=0.7)
#Elipses sobre disfuncion sexual
plotellipses(MCA_tablaNEO,keepvar=3,level=0.95)

#Separacion x color segun si hay o no disfuncion sexual
plot.MCA(MCA_tablaNEO,choix="ind",invisible="var",habillage=3,cex=0.7)
#Elipses sobre interrogacion
plotellipses(MCA_tablaNEO,keepvar=c(3,4,5,6),level=0.95)


# Create the plot with colors and ellipses

colors <- as.factor(tabla_NEO[, 15])

# Assume that the column 20 in `tabla_NEO` is also a factor for ellipses
ellipses <- as.factor(tabla_NEO[, 20])

library(FactoMineR)
library(factoextra)

# Extract the factor levels for coloring and ellipses
colors <- as.factor(tabla_NEO[, 15]) #Columna 15 es Disfuncion
ellipses <- as.factor(tabla_NEO[, 20])

# Create the MCA plot
fviz_mca_ind(MCA_tablaNEO,
             label = "none",          # Hide individual labels
             habillage = colors,      # Color by the column 15
             addEllipses = FALSE,      # Add concentration ellipses
             ellipse.level = 0.95,    # Confidence level
             palette = "jco",         # Choose a color palette
             ellipse.type = "t",
             geom = "point",
             pointsize = 2.5
)


# Add ellipses separately
fviz_ellipses(MCA_tablaNEO, 
              groups = ellipses, 
              habillage = ellipses,
              ellipse.type = "confidence",
              ellipse.level = 0.95,
              palette = "lancet")



