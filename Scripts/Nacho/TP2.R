library(class)
library(dplyr)

# Establecemos una semilla para la reproducibilidad
set.seed(123)

# Creamos un vector de índices para el conjunto de entrenamiento
n_train <- sample(1:nrow(fake_news), replace = FALSE, 0.8 * nrow(fake_news))

# Creamos los conjuntos de entrenamiento y prueba
train_data <- fake_news[n_train, ]
test_data <- fake_news[-n_train, ]


#Filtro los datos para hacer el Knn
train_filtrado <- train_data %>%
  select(title_words,negative,title_excl)

test_filtrado <- test_data %>%
  select(title_words,negative,title_excl)

pred_knn <- knn(train = train_filtrado,cl= train_data$type,test= test_filtrado,k=5)

#Evaluo la precision
mean(pred_knn ==test_data$type)

#Busco la presición base
nrow(test_data %>% filter(type=="fake"))/nrow(test_data %>% filter(type=="real"))
# La presición mínima tiene que ser mayor que 0,67

accus=c()
for(i in 1:50){
  knn_for <- knn(train = train_filtrado,cl= train_data$type,test= test_filtrado,k=i*2-1)
  accus=c(accus,mean(knn_for == test_data$type))
}

k=seq(from=1,to=99,by=2)
