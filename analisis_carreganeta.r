# Cargar datos

datos <- read.csv("carrega_neta.csv", header = TRUE, sep = ";")

head(datos)

str(datos)

summary(datos)

View(datos)

# Test estadístico

# Queremos comprobar si la carga neta a pH 7 aumenta después de la optimización

resultado <- t.test(datos$carga_pH_7_optimizated,
                    
                    datos$carga_pH_7,
                    
                    paired = TRUE,
                    
                    alternative = "greater")

resultado
# Extraer y formatear p-valor

pvalor <- resultado$p.value

pvalor <- format(resultado$p.value, digits = 4, nsmall = 3)

pvalor

# Cargar librerías

library(ggplot2)

library(tidyr)

library(dplyr)
# Pasar datos a formato largo

datos_largos <- datos %>%
  
  select(id, carga_pH_7, carga_pH_7_optimizated) %>%
  
  pivot_longer(cols = c(carga_pH_7, carga_pH_7_optimizated),
               
               names_to = "Condition",
               
               values_to = "Values")
datos_largos$Condition <- factor(datos_largos$Condition,
                                 
                                 levels = c("carga_pH_7", "carga_pH_7_optimizated"))

grafico_carrega_neta<-ggplot(datos_largos, aes(x = Condition, y = Values, fill = Condition)) +
  
  geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
  
  geom_line(aes(group = id), color = "#DBDBDB") +
  
  geom_point(color = "black", size = 0.8) +
  
  scale_fill_manual(values = c("carga_pH_7" = "#90EE90",
                               
                               "carga_pH_7_optimizated" = "#2E8B57"),
                    
                    labels = c("carga_pH_7" = "Abans",
                               
                               "carga_pH_7_optimizated" = "Després")) +
  
  scale_x_discrete(labels = c("carga_pH_7" = "AO",
                              
                              "carga_pH_7_optimizated" = "DO")) +
  
  labs(
    
    subtitle = paste("t-test aparellat, p =", pvalor),
    
    x = "Condició",
    
    y = "Càrrega neta a pH 7",
    
    fill = "Condició") +
  
  theme_classic() +
  
  theme(
    
    legend.position = "none",
    
    plot.title = element_text(hjust = 1),
    
    plot.subtitle = element_text(hjust = 0.5, size = 18),
    
    axis.title.x = element_text(size = 24),
    
    axis.title.y = element_text(size = 24),
    
    axis.text.x = element_text(size = 20),
    
    axis.text.y = element_text(size = 20),
    
    plot.background = element_rect(fill = "transparent", color = NA),
    
    panel.background = element_rect(fill = "transparent", color = NA)
    
  )

ggsave(
  
  filename = "diagrama_carreag_neta3_A0_poster.png",
  
  plot = grafico_carrega_neta,
  
  width = 15,
  
  height = 15,
  
  units = "cm",
  
  dpi = 600,
  
  bg = "transparent"
  
)

