install.packages("ggplot2")
install.packages("rpart")

library(ggplot2)
library(rpart)

# EJERCICIO 2 : Clasificación


# - Exploración -



# gráfico de dispersión "porc palabras negativas" vs "numero de palabras en el titulo"
ggplot(fake_news, aes(x = negative, y = title_words, color = type)) +
  geom_point() +
  labs(x = "Porcentaje de palabras negativas",
       y = "Número de palabras en el título",
       color = "Tipo") +
  ggtitle("Relación entre title_has_excl, negative y title_words")



# gráfico de dispersión para "title_has_excl" vs "negative" respecto a "type"
ggplot(fake_news, aes(x = title_has_excl, y = negative, color = type)) +
  geom_point() +
  labs(x = "Título con exclamación", y = "Porcentaje de connotaciones negativas") +
  ggtitle("Gráfico de dispersión: title_has_excl vs negative por tipo de noticia")

# gráfico de dispersión para "title_has_excl" vs "title_words" respecto a "type"
ggplot(fake_news, aes(x = title_has_excl, y = title_words, color = type)) +
  geom_point() +
  labs(x = "Título con exclamación", y = "Número de palabras en el título") +
  ggtitle("Gráfico de dispersión: title_has_excl vs title_words por tipo de noticia")



# - Decision Tree -

# Establecemos una semilla para la reproducibilidad
set.seed(123)

# Creamos un vector de índices para el conjunto de entrenamiento
n_train <- sample(1:nrow(fake_news), replace = FALSE, 0.8 * nrow(fake_news))

# Creamos los conjuntos de entrenamiento y prueba
train_data <- fake_news[n_train, ]
test_data <- fake_news[-n_train, ]






# Construimos el clasificador de árbol de decisión
tree_model1 <- rpart(type ~ title_has_excl + negative + title_words, data = train_data)

# Pendiente: Ir desglosando entre las distintas combinaciones de variables

# Predecimos las clases en el conjunto de prueba
tree_predictions <- predict(tree_model1, test_data, type = "class")

# Calculamos la matriz de confusión
confusion_matrix_tree <- table(Real = test_data$type, Predicho = tree_predictions)

# Calculamos la precisión del modelo
accuracy_tree <- sum(diag(confusion_matrix_tree)) / sum(confusion_matrix_tree)





















################ Borrador ################

# Tomo de ejemplo los ejs de la guia de decision tree # 



pens <- penguins %>%
  filter(sex != is.na(sex))

#PARTE 1

#Ejercicio 0

ggplot(data=pens)+
  geom_point(mapping=aes(x=bill_length_mm,y=flipper_length_mm,color=sex))

ggplot(data=pens)+
  geom_boxplot(mapping=aes(x=sex,y=flipper_length_mm,color=sex))

ggplot(data=pens)+
  geom_boxplot(mapping=aes(x=sex,y=bill_length_mm,color=sex))

#Ejercicio 1

set.seed(124)

n_train <- sample(1:nrow(pens),replace = FALSE, 0.8*nrow(pens))

p_train <- pens[n_train,]
p_test <- pens[-n_train,]


m_a <- rpart(sex~bill_length_mm,data=p_train,minsplit=16)


p_test1 <- p_test %>%
  mutate(pred_sex=predict(m_a,newdata=p_test,type="class"))


accu= mean(p_test1$sex == p_test1$pred_sex)


#Ejercicio 2

m_b <- rpart(sex~flipper_length_mm,data=p_train,minsplit=12)

p_test1 <- p_test1 %>%
  mutate(pred_sex2 = predict(m_b,newdata=p_test,type="class"))

accu2=mean(p_test1$sex == p_test1$pred_sex2)



#Ejercicio 3

ggplot(data=p_train) +
  geom_point(mapping = aes(x=body_mass_g,y=flipper_length_mm,color=sex))


m_3 <- rpart(sex~body_mass_g+flipper_length_mm,data=p_train,minsplit=10)

p_test <- p_test %>%
  mutate(pred_peso_3=predict(m_3,newdata=p_test,type="class"))

accu3=mean(p_test$sex == p_test$pred_peso_3)


rpart.plot(m_3)

#Matriz de confusión --- tarea para casa


#Ejercicio 4

accus= c()

for(i in c(1:1000)){

  set.seed(i)

  n_train <- sample(1:nrow(pens),replace = FALSE, 0.8*nrow(pens))

  p_train <- pens[n_train,]
  p_test <- pens[-n_train,]

  modelo <- rpart(sex~body_mass_g+flipper_length_mm,data=p_train,minsplit=10)

  p_test <- p_test %>%
    mutate(pred_peso=predict(modelo,newdata=p_test,type="class"))

  accus=c(accus,mean(p_test$sex == p_test$pred_peso))
}

ggplot(data=as.data.frame(accus),mapping=aes(x=accus))+
  geom_density()


#Ejercicio 5