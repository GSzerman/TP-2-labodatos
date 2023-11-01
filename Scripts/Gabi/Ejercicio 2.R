install.packages("ggplot2")
library(ggplot2)

# Crear un gráfico de dispersión con ggplot
ggplot(fake_news, aes(x = negative, y = title_words, color = title_has_excl)) +
  geom_point() +
  labs(x = "Porcentaje de palabras negativas",
       y = "Número de palabras en el título",
       color = "Tiene signos de exclamación en el título") +
  ggtitle("Relación entre title_has_excl, negative y title_words")




# Tomo de ejemplo los ejs de la guia de decision tree

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