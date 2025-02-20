#Install packages
install.packages("agricolae") #agricolae pack for Newman-Keuls test
library(agricolae)
install.packages("emmeans")
library(emmeans)
install.packages("ggplot2") # for better graphics
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)

#load the data, specifying that they are separated by ;, in this case photosynthetic values of an algae
data_Osc_1 <- read.delim("Exp_Osc_1_Par.csv", sep=";")

#change , for . in case you need
data_Osc_1$Valor <- gsub(",", ".", data_Osc_1$Valor)

#changes the data types to the corresponding
data_Osc_1$Valor <- as.numeric(data_Osc_1$Valor)
data_Osc_1$Tratamiento <- as.factor(data_Osc_1$Tratamiento)
data_Osc_1$Dia <- as.factor(data_Osc_1$Dia)
data_Osc_1$Parametro <- as.factor(data_Osc_1$Parametro)

#Anova Multi-way
anova_mul1 <- aov(Valor ~ Tratamiento * Dia * Parametro, data = data_Osc_1)
summary(anova_mul1)
#You need to check for Pr(<F) values and signif. codes


# Create interaction column with factors that show significant differences 
data_Osc_1$Interaccion <- interaction(data_Osc_1$Tratamiento, data_Osc_1$Parametro, data_Osc_1$Dia)

# Adjust the anova model with the interaction column
modelo_anova_interaccion <- aov(Valor ~ Interaccion, data = data_Osc_1)


# Apply Newman Keuls test  post-hoc to the new model to differentiate groups
test_newman_keuls_interaccion <- SNK.test(modelo_anova_interaccion, "Interaccion")
print(test_newman_keuls_interaccion1)

# Create a dataframe with NK results, so letters can be applied to graphs
letter_groups <- as.data.frame(test_newman_keuls_interaccion$groups)
letter_groups$Interaccion <- rownames(letter_groups)
rownames(letter_groups) <- NULL
print(letter_groups)

#Calculate mean values and standar desviation for each treatment for graphics
data_resumen <- data_Osc_1 %>%
  group_by(Tratamiento, Dia, Parametro, Interaccion) %>%
  summarise(Valor_medio = mean(Valor, na.rm = TRUE), SD = sd(Valor, na.rm = TRUE)) %>%
  distinct()
data_resumen

#Combining letters dataframe to data dataframe
data_Osc_1_comb <- data_resumen %>% 
  left_join(letter_groups, by = "Interaccion")
print(data_Osc_1_comb)

#Add graphic to show results
ggplot(data_Osc_1_comb, aes(x = Tratamiento, y = Valor_medio, fill = Dia)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Valor_medio - SD, ymax = Valor_medio + SD),
                position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(label = groups, y = Valor_medio + SD), 
            position = position_dodge(0.9), vjust = -0.5) +
  facet_wrap(~ Parametro, scales = "free_y") +
  scale_fill_manual(values = c("#5D8736", "#A9C46C","#809D3C",  "#F4FFC3")) +
  labs(title = "Valores medios para cada parámetro y tratamiento",
       x = "Día",
       y = "Valor medio") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # Adjust top limit axis y (in case latters dont fit)
  theme_classic() + 
  theme(
    legend.text = element_text(size = 10),
    panel.background = element_blank(),
    strip.background = element_blank(),  # Removes gray background of titles
    strip.text = element_text(size = 12), # Adjust font size
    legend.position = "bottom", legend.title = element_blank() # Change legend position (top, bottom, left, right) and white background
  )
 
  

