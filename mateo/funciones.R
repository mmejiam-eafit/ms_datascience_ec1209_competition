

lasso=function(seed,x,y){
  library(glmnet)
  set.seed(seed)
  
  train_id=sample(length(y),0.8*length(y))
  x_train=x[train_id,]
  y_train=y[train_id]
  x_test=x[-train_id,]
  y_test=y[-train_id]
  cvfit = cv.glmnet(x_train, y_train, family = "binomial", type.measure = "auc")
  
  selected=as.numeric(coef(cvfit, s = "lambda.min"))[-1]!=0

  train=data.frame(x_train)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_train)
  
  model=glm(y~.,family=binomial(),data = train)
  
  test=data.frame(x_test)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_test)

  beta=rep(0,ncol(x))
  beta[selected]=coef(model)[-1]
  ans=list(selected=selected,beta=beta)
  return(ans)
}



bics=function(seed,x,y){
  library(glmnet)
  set.seed(seed)
  
  train_id=sample(length(y),0.8*length(y))
  x_train=x[train_id,]
  y_train=y[train_id]
  x_test=x[-train_id,]
  y_test=y[-train_id]
  
  
  bma.object<-bic.glm(x_train,y_train,maxCol = 55,glm.family="binomial")
  selected=bma.object$probne0>50
  

  train=data.frame(x_train)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_train)
  
  model=glm(y~.,family=binomial(),data = train)
  
  test=data.frame(x_test)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_test)
  
  beta=rep(0,ncol(x))
  beta[selected]=coef(model)[-1]
  ans=list(selected=selected,beta=beta)
  return(ans)
}



prob_inc=function(lista){
  selected=list()
  
  for (i in 1:length(lista)){
    selected[[i]]=lista[[i]]$selected*1
  }
  
  m_sel=do.call(rbind,selected)
  p=colMeans(m_sel)
  return(p)
  
}


get_betas=function(lista){
  betas=list()
  
  for (i in 1:length(lista)){
    betas[[i]]=lista[[i]]$beta
  }
  betas=do.call(rbind,betas)
  return(betas)
}

get_met=function(lista){
  spec=NULL
  global=NULL
  for (i in 1:length(lista)){
    spec=c(spec,lista[[i]]$spec)
    global=c(global,lista[[i]]$global)
  }
  met=list(spec=spec,global=global)
  return(met)
}





eval=function(seed,x,y){
  library(glmnet)
  set.seed(seed+1000)
  
  train_id=sample(length(y),0.8*length(y))
  x_train=x[train_id,]
  y_train=y[train_id]
  x_test=x[-train_id,]
  y_test=y[-train_id]

  train=data.frame(x_train)%>%
    mutate(y=y_train)
  
  model=glm(y~.,family=binomial(),data = train)
  
  test=data.frame(x_test)%>%
    mutate(y=y_test)
  
  p=predict.glm(model, newdata=test,type='response')
  
  auc_val=auc(y_test,p)
  y_hat=ifelse(p>0.5,1,0)
  
  accuracy=mean(y_train==y_hat)
  
  
  beta=coef(model)[-1]
  ans=list(spec=auc_val,global=accuracy,beta=beta)
  return(ans)
}




#### Conteos


lasso_c=function(seed,x,y){
  library(glmnet)
  set.seed(seed)
  
  train_id=sample(length(y),0.8*length(y))
  x_train=x[train_id,]
  y_train=y[train_id]
  x_test=x[-train_id,]
  y_test=y[-train_id]
  cvfit = cv.glmnet(x_train, y_train, family = "poisson", type.measure = "mse")
  selected=as.numeric(coef(cvfit, s = "lambda.min"))[-1]!=0
  
  train=data.frame(x_train)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_train)
  
  model=glm(y~.,family="poisson",data = train)
  
  test=data.frame(x_test)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_test)
  
  beta=rep(0,ncol(x))
  beta[selected]=coef(model)[-1]
  ans=list(selected=selected,beta=beta)
  return(ans)
}


eval_c=function(seed,x,y){
  library(glmnet)
  set.seed(seed+1000)
  
  train_id=sample(length(y),0.8*length(y))
  x_train=x[train_id,]
  y_train=y[train_id]
  x_test=x[-train_id,]
  y_test=y[-train_id]
  
  train=data.frame(x_train)%>%
    mutate(y=y_train)
  
  model=glm(y~.,family="poisson",data = train)
  
  test=data.frame(x_test)%>%
    mutate(y=y_test)
  
  p=round(predict.glm(model, newdata=test,type='response'))
  
  mse=mean((y_test-p)^2)
  
  y_hat=ifelse(p==0,0,1)
  y_train_0=ifelse(y_train==0,0,1)
  
  accuracy=mean(y_train_0==y_hat)
  beta=coef(model)[-1]
  ans=list(spec=accuracy,global=mse,beta=beta)
  return(ans)
}


bics_c=function(seed,x,y){
  library(glmnet)
  set.seed(seed)
  
  train_id=sample(length(y),0.8*length(y))
  x_train=x[train_id,]
  y_train=y[train_id]
  x_test=x[-train_id,]
  y_test=y[-train_id]
  
  
  bma.object<-bic.glm(x_train,y_train,maxCol = 55,glm.family=poisson())
  selected=bma.object$probne0>50
  train=data.frame(x_train)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_train)
  
  model=glm(y~.,family="poisson",data = train)
  
  test=data.frame(x_test)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_test)
  
  beta=rep(0,ncol(x))
  beta[selected]=coef(model)[-1]
  ans=list(selected=selected,beta=beta)
  return(ans)
}


### Continuo

lasso_n=function(seed,x,y){
  library(glmnet)
  set.seed(seed)
  
  train_id=sample(length(y),0.8*length(y))
  x_train=x[train_id,]
  y_train=y[train_id]
  x_test=x[-train_id,]
  y_test=y[-train_id]
  cvfit = cv.glmnet(x_train, y_train, family = 'gaussian', type.measure = "mse")
  selected=as.numeric(coef(cvfit, s = "lambda.min"))[-1]!=0
  
  train=data.frame(x_train)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_train)
  
  model=glm(y~.,family='gaussian',data = train)
  
  test=data.frame(x_test)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_test)
  
  beta=rep(0,ncol(x))
  beta[selected]=coef(model)[-1]
  ans=list(selected=selected,beta=beta)
  return(ans)
}


eval_n=function(seed,x,y){
  library(glmnet)
  set.seed(seed+1000)
  
  train_id=sample(length(y),0.8*length(y))
  x_train=x[train_id,]
  y_train=y[train_id]
  x_test=x[-train_id,]
  y_test=y[-train_id]
  
  train=data.frame(x_train)%>%
    mutate(y=y_train)
  
  model=glm(y~.,family=gaussian(),data = train)
  
  test=data.frame(x_test)%>%
    mutate(y=y_test)
  
  p=predict.glm(model, newdata=test,type='response')
  
  mse=mean((y_test-p)^2)
  
  y_hat=ifelse(p>(-1),0,1)
  y_train_0=ifelse(y_train>(-1),0,1)
  
  accuracy=mean(y_train_0==y_hat)
  beta=coef(model)[-1]
  ans=list(spec=accuracy,global=mse,beta=beta)
  return(ans)
}


bics_n=function(seed,x,y){
  library(glmnet)
  set.seed(seed)
  
  train_id=sample(length(y),0.8*length(y))
  x_train=x[train_id,]
  y_train=y[train_id]
  x_test=x[-train_id,]
  y_test=y[-train_id]
  
  
  bma.object<-bic.glm(x_train,y_train,maxCol = 55,glm.family=gaussian())
  selected=bma.object$probne0>50
  train=data.frame(x_train)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_train)
  
  model=glm(y~.,family=gaussian(),data = train)
  
  test=data.frame(x_test)%>%
    dplyr::select(colnames(x)[selected])%>%
    mutate(y=y_test)
  
  beta=rep(0,ncol(x))
  beta[selected]=coef(model)[-1]
  ans=list(selected=selected,beta=beta)
  return(ans)
}

