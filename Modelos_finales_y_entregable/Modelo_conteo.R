### Archivos
data_train="input/datacountstudents.csv"
data_test="datasetx.csv"
nombre_output='output_conteo.csv'
### Librerias
library(tidyverse)
### Lectura de datos


train=read.csv(data_train)%>%
  dplyr::select(-id)%>%
  rename(y=yC)%>%
  filter(!is.na(y))%>% 
  dplyr::select(y,x3,x22,x24,x25)

### Entrenamiento modelo
model=glm(y~.,family="poisson",data = train)

### Testeo

test=read.csv(data_test)

p=round(predict.glm(model, newdata=test,type='response'))
data.frame(y_hat=p)%>%
  write.csv(nombre_output)

