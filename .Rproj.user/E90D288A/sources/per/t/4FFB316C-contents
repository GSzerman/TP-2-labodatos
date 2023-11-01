install.packages("ggplot2")
library(ggplot2)

# Crear un gráfico de dispersión con ggplot
ggplot(fake_news, aes(x = negative, y = title_words, color = title_has_excl)) +
  geom_point() +
  labs(x = "Porcentaje de palabras negativas",
       y = "Número de palabras en el título",
       color = "Tiene signos de exclamación en el título") +
  ggtitle("Relación entre title_has_excl, negative y title_words")
