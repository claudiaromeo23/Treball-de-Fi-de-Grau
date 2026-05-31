head(MICs)
str(MICs)
summary(MICs)
View(MICs)

diferencies <- MICs$Before - MICs$After
shapiro.test(diferencies)

resultado<-t.test(MICs$Before, MICs$After, paired = TRUE, alternative = "greater")
resultado

pvalor<-resultado$p.value
pvalor <- format(resultado$p.value, digits = 3, nsmall = 3)
pvalor

library(ggplot2)
library(tidyr)
library(dplyr)

datos_largos<-MICs %>%
  select(Id,Before,After) %>%
  pivot_longer(cols=c(After,Before),names_to = "Condition",values_to = "Values")
datos_largos$Condition<-factor(datos_largos$Condition,levels = c("Before", "After"))
grafico_mic<-ggplot(datos_largos, aes(x = Condition, y = Values, fill = Condition)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
  geom_line(aes(group = Id), color = "#DBDBDB") +
  geom_point(color = "black", size = 0.5) +
  scale_fill_manual(values = c("Before" = "#FFA07A", "After" = "#CD6839"),
                    labels = c("Before" = "Abans", "After" = "Després"))+
  scale_x_discrete(labels = c("Before" = "AO", "After" = "DO")) +
  labs(
    subtitle=paste("t-test aparellat, p =", pvalor),
    x = "Condició",
    y = "Valors de MIC (µM)",
    fill = "Condició")+
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
    panel.background = element_rect(fill = "transparent", color = NA))

ggsave(
      filename = "diagrama_MIC4_A0_poster.png",
      plot = grafico_mic,
      width = 15,
      height = 20,
      units = "cm",
      dpi = 600,
      bg = "transparent"
    )

