rm(list=ls()) #Clears environment

rfunction1 <- function(){
  setTimeLimit(3, transient = T)
  pikk=2
  setTimeLimit(transient = T)
}

rfunction2 <- function(){
  setTimeLimit(3, transient = T)
  repeat{
    x <- rnorm(100);
  }
  setTimeLimit(transient = T)
}



for (i in 1:10){

  fit = tryCatch(rfunction1(), error=function(e) e, warning=function(w) w)
  if(is(fit,"warning")){
    
    print("warning")
    message(fit)
    
  } else if(is(fit,"error")){
    
    print("error")
    message(fit)
    
  }else{
    print("no error")

  }
  
  repeat{
    x <- rnorm(100);
  }
  
  
}

for (i in 1:10){
  
  fit = tryCatch(rfunction2(), error=function(e) e, warning=function(w) w)

  print("Pikk")
  
}