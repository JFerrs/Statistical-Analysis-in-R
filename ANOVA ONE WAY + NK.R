#agricolae pack for Newman-Keuls test
install.packages("agricolae")
library(agricolae)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

#load the data, specifying that they are separated by ;, in this case phenol compounds of an algae
data_Fen_LOR <- read.delim("Estadistica_Fenoles_LO_Exp_Osc.csv", sep=";")

#change , for . in case you need
data_Fen_LOR$Valor <- gsub(",", ".", data_Fen_LOR$Valor)

#change value tipe from str to numeric
data_Fen_LOR$Valor <- as.numeric(data_Fen_LOR$Valor)

#restructuring the data for one-way ANOVA. In this case experiment had two treatments, darkness (Oscuridad) and light (Luz), each of them with 3 replicates
data_Fen_LOR <- data.frame(
  Tratamiento = rep(c("Oscuridad", "Luz"), each = 3),
  Valor = c(data_Fen_LOR$Valor)
)

#Anova time
anova_Fen_LOR <- aov(Valor ~ Tratamiento, data = data_Fen_LOR)
summary(anova_Fen_LOR)


#Calculate mean values and standar desviation for each treatment for future graphics
resumen_Fen_LOR <- data_Fen_LOR %>%
  group_by(Tratamiento) %>%
  summarise(Valor_medio = mean(Valor, na.rm = TRUE), SD = sd(Valor, na.rm = TRUE)) %>%
  distinct()
resumen_Fen_LOR

#Creating a boxplot to show data results
ggplot(resumen_Fen_LOR, aes(x = Tratamiento, y = Valor_medio, fill = Tratamiento)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Valor_medio - SD, ymax = Valor_medio + SD),
               position = position_dodge(0.9), width = 0.25) +
  #geom_text(aes(label = groups, y = Valor_medio + SD), 
            #position = position_dodge(0.9), vjust = -0.5) +
  #facet_wrap(~ groups, scales = "free_y") +                  # Not needed when its a one way ANOVA
  scale_fill_manual(values = c("#3674B5","#578FCA","#A1E3F9")) +
  labs(title = "mg/g de Violaxantina en peso seco para cada uno de los días de experimento",
       x = "Dia",
       y = "mg/g DW Viox") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # Adjust top limit axis y (in case latters dont fit)
  theme_classic()+
  theme(
    legend.position = "bottom", legend.title = element_blank() # Change legend position (top, bottom, left, right) and white background
    )




