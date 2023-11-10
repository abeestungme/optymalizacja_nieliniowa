#LAB 1
# METODA PRZESZUKIWANIA POTRÓJNEGO

ternary <- function(f, lower, upper, tol) {
  n <- ceiling(log(2 * tol / (upper - lower), 2/3))
  i = 0
  f.lower <- f(lower)
  f.upper <- f(upper)
  while (abs(upper - lower) > 2 * tol) {
    x1 <- (2 * lower + upper) / 3
    f.x1 <- f(x1)
    x2 <- (lower + 2 * upper) / 3
    f.x2 <- f(x2)
    if (f.x1 < f.x2) {
      upper <- x2
      f.upper <- f.x2
    } else {
      lower <- x1
      f.lower <- f.x1
    }
    i = i + 1
    cat("iteracja: ", i, "a = ", lower, "b = ", upper, "\n")
  }
  cat("Szacowana liczba iteracji: ", n)
  return((upper + lower) / 2)
}

f1 <- function(x){x^2*exp(1/x)}

f2 <- function(x){-(1+log(x))/x}

f3 <- function(x){log(x^2-1)+1/(x^2-1)}


####1
#z rysunku wybieramy przedzial do minimum
plot(f1, 0.1 , 5)

ternary(f1, 0.1, 1, 0.001)

####2
plot(f2, 0.5, 5)

ternary(f2, 0.5, 3, 0.0001)

####3
plot(f3, 1.1, 5)

ternary(f3, 1.1, 2, 0.00001)
ternary(f3, -2, -1.1, 0.00001)

################################################################################
#   LAB 2
################################################################################

golden <- function(f,lower, upper, tol){

  alpha <- (sqrt(5)-1)/2
  
  n <- ceiling(log(2 * tol / (upper - lower), alpha))
  i = 0
  
  x1 <- alpha * lower + (1-alpha) * upper
  f.x1 <- f(x1)
  x2 <- (1-alpha) * lower + alpha * upper
  f.x2 <- f(x2)
  while(abs(upper - lower) > 2 * tol){
    if (f.x1 < f.x2){ # do liczenia min
    #if (f.x1 > f.x2){ #do liczenia maks
      upper <- x2
      x2 <- x1
      f.x2 <- f.x1
      x1 <- alpha * lower + (1-alpha) * upper
      f.x1 <- f(x1)
    } else {
      lower <- x1
      x1 <- x2
      f.x1 <- f.x2
      x2 <- (1-alpha) * lower + alpha * upper
      f.x2 <- f(x2)
    }
    i = i + 1
    cat("iteracja: ", i, "a = ", lower, "b = ", upper, "\n")
  }
  cat("Szacowana liczba iteracji: ", n)
  return((upper + lower) / 2)
}

golden_max <- function(f,lower, upper, tol){
  
  alpha <- (sqrt(5)-1)/2
  n <- ceiling(log(2*tol/(upper-lower),alpha))
  iter = 0
  x1 <- alpha * lower + (1-alpha) * upper
  f.x1 <- f(x1)
  x2 <- (1-alpha) * lower + alpha * upper
  f.x2 <- f(x2)
  while(abs(upper - lower) > 2 * tol){
    if (f.x1 > f.x2){
      upper <- x2
      x2 <- x1
      f.x2 <- f.x1
      x1 <- alpha * lower + (1-alpha) * upper
      f.x1 <- f(x1)
    } else {
      lower <- x1
      x1 <- x2
      f.x1 <- f.x2
      x2 <- (1-alpha) * lower + alpha * upper
      f.x2 <- f(x2)
    }
    iter = iter + 1
    #cat(" iteracja: ", iter, "a= ",lower, "b= ", upper, "\n")
  }
  #cat("Liczba iteracji", n, "\n")
  return((lower + upper) / 2)
}

plot(f1, 0.1 , 5)

golden(f1, 0.1, 1, 0.001)

####2
plot(f2, 0.5, 5)

ternary(f2, 0.5, 3, 0.0001)
golden(f2, 0.5, 3, 0.0001)

####3
plot(f3, 1.1, 5)

ternary(f3, 1.1, 2, 10^(-8))
golden(f3, 1.1, 2, 10^(-8))
ternary(f3, -2, -1.1, 10^(-8))
golden(f3, -2, -1.1, 10^(-8))

#ZAD 2
# wyznacz maksyma lokalne funkcji
# a) y = log(x) / sqrt(x)
# b) y = (exp^(-x))/(x^2 - 1)

f4 <- function(x) { log(x) / sqrt(x) }
f5 <- function(x) { (exp(-x))/(x^2 - 1) }
plot(f4, 2, 20)
mf4 <- function(x) { - log(x) / sqrt(x) } #funkcja z minusem zeby obliczyc maksimum z algor. na min.
plot(mf4, 2, 20)

ternary(mf4, 5, 10, 0.001)
golden(mf4, 5, 10, 0.001)

# teraz funkcja z b)
mf5 <- function(x) { - (exp(-x))/(x^2 - 1) }
plot(mf5, 0, 0.8)
ternary(mf5, 0, 0.8, 0.001)
golden(f5, 0, 0.8, 0.001)

golden_improved <- function(f, lower, upper, tol) {
  i = 0
  ratio <- 2 / (3 + sqrt(5))
  x1 <- (1 - ratio) * lower + ratio * upper
  f.x1 <- f(x1)
  while (abs(upper - lower) > 2 * tol) {
    x2 <- (1 - ratio) * x1 + ratio * upper
    f.x2 <- f(x2)
    if (f.x1 < f.x2) {
      upper <- lower
      lower <- x2
    } else {
      lower <- x1
      x1 <- x2
      f.x1 <- f.x2
    }
    i = i + 1
    cat("iteracja: ", i, "a = ", lower, "b = ", upper, "\n")
  }
  cat("Liczba iteracji: ", i)
  return((upper + lower) / 2)
}

golden_improved(f1, 0.1, 1, 0.001)
golden(f1, 0.1, 1, 0.001)

######
#fibbonaci 
phi <- function(i) {
  if (i <= 1) {
    return(i)
  } else {
    return(phi(i-1) + phi(i-2))
  }
}
fib_k <-function(lower, upper, tol) {
  i <- 1
  l <- upper - lower
  while(phi(i) < l / tol) {
    i <- i + 1
  }
  return(i)
}
fibonacci <- function(f,lower, upper, tol){
  k <- fib_k(lower, upper, tol)
  c <- upper - phi(k-1)/phi(k)*(upper - lower)
  d <- lower + upper -c
  i <- 0
  while (i <= k-4) {
    if (f(c) < f(d)) {
      upper <- d
      i <- i + 1
    } else {
      lower <- c
      i <- i + 1
    }
    c <- upper - phi(k-i-1)/phi(k-i)*(upper - lower)
    d <- lower + upper -c
  }
  cat("Ilosc iteracji: ", k-2, "\n", "Ostatnie c: ", c, "\n Ostatnie d: ", d, "\n")
  return((lower + upper)/2)
}

#fibonacci(f1, 0.1, 1, 10^(-6))



##############################
#27.10

#biblioteka do liczenia pochodnych
library(Deriv)

bisection <- function(df, lower, upper, tol) {
  iteracja <- 1
  while (upper - lower > 2 * tol) {
    cat("Iteracja nr: ", iteracja, " a = ", lower, " b = ", upper, "\n")
    m <- (lower + upper) / 2
    df.m <- df(m)
    while (df.m == 0) {
      m <- (lower + upper) / 2 + runif(1, -tol, tol)
      df.m <- df(m)
    }
    if (df.m < 0) {
      lower <- m
    } else {
      upper <- m
    }
    iteracja <- iteracja + 1
  }
  cat("Iteracja nr: ", iteracja, " a = ", lower, " b = ", upper, "\n")
  return((upper + lower) / 2)
}

f6 <- function(x){(x^3-3)*exp(x)}


plot(f6,-5,2, ylim=c(-6,5))

df6 <- function(x){exp(x)*(x^3+3*x^2-3)} #pochodna f6 policzona recznie

bisection(df6, 0, 2, 0.001)
bisection(df6, -4, -1.5, 0.001)


#obliczanie maksimum z funkcji ujemnej
mf6 <- function(x){-(x^3-3)*exp(x)}

plot(mf6, -4, 2, ylim = c(-2,6))
mdf6 <- function(x){-exp(x)*(x^3+3*x^2-3)} #pochodna mf6
bisection(mdf6,-2.5,0,0.001)

#zmiana kodu by automatycznie liczyl maksimum
bisection_max <- function(df, lower, upper, tol) {
  iteracja <- 1
  while (upper - lower > 2 * tol) {
    cat("Iteracja nr: ", iteracja, " a = ", lower, " b = ", upper, "\n")
    m <- (lower + upper) / 2
    df.m <- df(m)
    while (df.m == 0) {
      m <- (lower + upper) / 2 + runif(1, -tol, tol)
      df.m <- df(m)
    }
    if (df.m > 0) {
      lower <- m
    } else {
      upper <- m
    }
    iteracja <- iteracja + 1
  }
  cat("Iteracja nr: ", iteracja, " a = ", lower, " b = ", upper, "\n")
  return((upper + lower) / 2)
}
bisection_max(df6,-2.5,0,0.001)

####
f6a <- f6
df_6a <- Deriv(f6a)
df_6a
plot(f6a, -5, 2)
plot(df_6a, 0, 2, 0.001)
bisection(df_6a, 0, 2, 0.001)


#####
#   f7

f7 <- function(x){(x^3)/(x^2-x-2)}
df7 <- Deriv(f7)
df7
plot(f7, 2.5, 5)

bisection(df7, 3.2, 3.7, 0.001)

plot(f7, -5, -1.2)
bisection_max(df7, -2, -1.2, 0.001)
plot(f7, -1, 2)

#####################
#     NEWTON
#####################

newton <- function(df, d2f, x, tol) {
  iteracja <- 1
  cat("Iteracja: ",iteracja, "; x = ",x,"\n")
  repeat {
    new.x <- x - df(x) / d2f(x)
    iteracja = iteracja +1
    cat("Iteracja: ",iteracja, "; x = ",new.x, "\n")
    if (abs(new.x - x) < tol) {
      return(new.x)
    }
    x <- new.x
  }
}


plot(f6, -4, 2, ylim=c(-6,2))

df2_df6 <- Deriv(df6)

newton(df6, df2_df6, 1, 0.001)
newton(df6, df2_df6, 1/5, 0.001)
newton(df6, df2_df6, 0.5, 0.001)
newton(df6, df2_df6, -1, 0.001)
#newton(df6, df2_df6, -10, 0.001)

for (i in c(1, 10, 0.5, -4, -10)) {
  cat("df6(",i,") = ", df6(i), "    df2_df6(", i, ") = ", df2_df6(i), "\n")
}


plot(f7, -1, 2)
newton(Deriv(f7), Deriv(Deriv(f7)), 1, 0.001)

##################################
#     3.11
#     LAB 03
##################################

secant <- function(df, x1, x2, tol) {
  df.x2 <- df(x2)
  iter = 1
  cat("Iteracja: ", iter, "; x1 = ", x1, "; x2 = ", x2, "\n")
  repeat {
    df.x1 <- df(x1)
    new.x <- x1 - df.x1 * (x1 - x2) / (df.x1 - df.x2)
    iter = iter + 1
    if (abs(new.x - x1) < tol) {
      return(new.x)
    }
    x2 <- x1
    df.x2 <- df.x1
    x1 <- new.x
    cat("Iteracja: ", iter, "; x1 = ", x1, "; x2 = ", x2, "\n")
  }
}

newton(df6, df2_df6, 1, 0.001)
secant(df6, -4, -3, 0.001)

md2f6 <- Deriv(mdf6)
md2f6

newton(mdf6, md2f6, -1, 0.001)

####
# f8
####

f8 <- function(x){
  (-exp(-x^2))
}

df8 <- Deriv(f8)

df2_f8 <- Deriv(df8)

plot(f8, -2, 2)

newton(df8, df2_f8, 0.3, 0.001)
#newton(df8, df2_f8, 0.5, 0.001) #blad - ciagla petla
#newton(df8, df2_f8, 0.9, 0.001)

df8(0.3)
df2_f8(0.3)

df8(0.5)
df2_f8(0.5)

df8(0.6)
df2_f8(0.6)

df8(0.6)/df2_f8(0.6)

0.6-2.14

df8(-1.54)
df2_f8(-1.54)
df8(-1.54)/df2_f8(-1.54)



#######################



library(rgl) # wczytanie bliblioteki

f1 <-function(x,y) {x^2+y^2} # podanie funkcji

x <- seq(-4, 4, length.out = 30) # podanie zakresu

y <- seq(-4, 4, length.out = 30) 

z <- outer(x,y, f1) # obliczenie wartości 

persp3d(x,y,z,col="lightblue",xlab="x",ylab="y",zlab="z",shade=0.8) #

library(numDeriv)

cauchy.const <- function(f, x, step, tol) {
  iteracja <- 1
  cat("Iteracja: ",iteracja," x= ",x," f(x)= ",f(x),"\n")
  repeat {
    new.x <- x - step * grad(f, x)
    iteracja <- iteracja + 1
    cat("Iteracja: ",iteracja," x= ",new.x," f(x)= ",f(new.x),"\n")
    if (dist(rbind(new.x,x)) < tol) {
      return(new.x)
    }
    x <- new.x
  }
}


golden <- function(f,lower, upper, tol){
  
  alpha <- (sqrt(5)-1)/2
  
  n <- ceiling(log(2*tol/(upper-lower), alpha))
  
  iteracja <- 0
  
  x1 <- alpha * lower + (1-alpha) * upper
  
  f.x1 <- f(x1)
  
  x2 <- (1-alpha) * lower + alpha * upper
  
  f.x2 <- f(x2)
  
  while(abs(upper - lower) > 2 * tol){
    
    if (f.x1 < f.x2){
      
      upper <- x2
      
      x2 <- x1
      
      f.x2 <- f.x1
      
      x1 <- alpha * lower + (1-alpha) * upper
      
      f.x1 <- f(x1)
      
    } else {
      
      lower <- x1
      
      x1 <- x2
      
      f.x1 <- f.x2
      
      x2 <- (1-alpha) * lower + alpha * upper
      
      f.x2 <- f(x2)
      
    }
    
    iteracja <- iteracja + 1
    
    #cat("Iteracja: ", iteracja, "a = ", lower, " b = ", upper, "\n")
    
  }
  
  #cat("Liczba iteracji: ", n)
  
  return((lower + upper) / 2)
  
}

library(numDeriv)

cauchy.golden <- function(f, x, tol) {
  
  iteracja <- 1
  
  cat("Iteracja: ",iteracja," x= ",x," f(x)= ",f(x),"\n")
  
  repeat {
    
    g <- function(a) {f(x - a * grad(f,x))}
    
    step <- golden(g,0,5,tol)
    
    new.x <- x - step * grad(f, x)
    
    iteracja <- iteracja + 1
    
    cat("Iteracja: ",iteracja," x= ",new.x," f(x)= ",f(new.x)," krok= ",step,"\n")
    
    if (dist(rbind(new.x,x)) < tol) {
      
      return(new.x)
      
    }
    
    x <- new.x
    
  }
  
}

f1 <- function(x,y) {x^2-x*y+y^2-2*x+y}

f1i <- function(x) {x[1]^2-x[1]*x[2]+x[2]^2-2*x[1]+x[2]}

f2 <- function(x,y){x+5*y+x*y-(x+y)^2}

f2i <- function(x){x[1]+5*x[2]+x[1]*x[2]-(x[1]+x[2])^2}

x <- seq(-4, 4, length.out = 30) # podanie zakresu

y <- seq(-4, 4, length.out = 30) 

z <- outer(x,y, f1) # obliczenie wartości 

persp3d(x,y,z,col="lightblue",xlab="x",ylab="y",zlab="z",shade=0.8) #

cauchy.const(f1i,c(-3,2),3/10,1/1000)

cauchy.golden(f1i,c(-10,5),0.001)

z <- outer(x,y, f2)

persp3d(x,y,z,col="lightblue",xlab="x",ylab="y",zlab="z",shade=0.8) #

#cauchy.const(f2i,c(-3,2),3/10,1/1000)

#cauchy.golden(f2i,c(-10,5),0.001)


cauchy.const.max <- function(f, x, step, tol) {
  iteracja <- 1
  cat("Iteracja: ",iteracja," x= ",x," f(x)= ",f(x),"\n")
  repeat {
    new.x <- x + step * grad(f, x)
    iteracja <- iteracja + 1
    cat("Iteracja: ",iteracja," x= ",new.x," f(x)= ",f(new.x),"\n")
    if (dist(rbind(new.x,x)) < tol) {
      return(new.x)
    }
    x <- new.x
  }
}

#cauchy.const.max(f2i, c(-3,2),0.3,0.001)



cauchy.golden.max <- function(f, x, tol) {
  iteracja <- 1
  cat("Iteracja: ",iteracja," x= ",x," f(x)= ",f(x),"\n")
  repeat {
    g <- function(a) {f(x + a * grad(f,x))}
    step <- golden(g,0,5,tol)
    new.x <- x - step * grad(f, x)
    iteracja <- iteracja + 1
    cat("Iteracja: ",iteracja," x= ",new.x," f(x)= ",f(new.x)," krok= ",step,"\n")
    if (dist(rbind(new.x,x)) < tol) {
      return(new.x)
    }
    x <- new.x
  }
}

##########
# 10.11.2023

f3 <- function(x){
  2*(x[1]^2-3*x[2])^2+(1-x[1])^2
}


#rysowanie f3
library(rgl) # wczytanie bliblioteki

f3i <- function(x,y){2*(x^2-3*y)^2+(1-x)^2} # podanie funkcji jako x i y zamiast x[1] i x[2]

x <- seq(-4, 4, length.out = 30) # podanie zakresu

y <- seq(-4, 4, length.out = 30) 

z <- outer(x,y, f3i) # obliczenie wartości 

persp3d(x,y,z,col="lightblue",xlab="x",ylab="y",zlab="z",shade=0.8) #


library(numDeriv)
newton <- function(f, x, tol) {
  iteracja <- 1
  repeat {
    new.x <- x - solve(hessian(f,x), grad(f,x))
    cat("Iteracja: ", iteracja, "; x= ",x,"; f(x): ",f(x),"\n")
    if (dist(rbind(new.x, x)) < tol) {
      cat("Iteracja: ", iteracja +1 , "; x= ",x,"; f(x): ",f(x),"\n")
      return(new.x)
    }
    x <- new.x
    iteracja = iteracja + 1
  }
}
newton(f3,c(-4,4),0.001)

#rysowanie f4
f4 <-function(x){
  4*x[1]*(1+x[2])+x[2]-2-4*x[1]^2-4*x[2]^2
}


#dla maksimum wiec funkcja odwrotna (-f(x))

mf4 <-function(x){
  -f4(x)
}

library(rgl) # wczytanie bliblioteki

f4i <- function(x,y){4*x*(1+y)+y-2-4*x^2-4*y^2} # podanie funkcji jako x i y zamiast x[1] i x[2]

x <- seq(-4, 4, length.out = 30) # podanie zakresu

y <- seq(-4, 4, length.out = 30) 

z <- outer(x,y, f4i) # obliczenie wartości 

persp3d(x,y,z,col="lightblue",xlab="x",ylab="y",zlab="z",shade=0.8) #

newton(f4,c(-4,4),0.001)#w newtonie obojetne czy f4 czy mf4
newton(mf4,c(-4,4),0.001)
cauchy.golden(mf4,c(-4,4),0.001)#w cauchy golden musi byc mf4


library(numDeriv)
fletcher.reeves <- function(f, x, tol) {
  iteracja <- 1
  beta <- 1
  d <- -grad(f,x)
  repeat {
    g <- function(a) {f(x + a * d)}
    step <- golden(g,0,5,tol)
    new.x <- x + step * d
    cat("Iteracja: ", iteracja , "; x= ",x,"; f(x): ",f(x),"\n")
    if (dist(rbind(new.x,x)) < tol) {
      cat("Iteracja: ", iteracja +1 , "; x= ",x,"; f(x): ",f(x),"\n")
      return(new.x)
    }
    beta <- (grad(f,new.x) %*% grad(f,new.x)) / (grad(f,x) %*% grad(f,x))
    d <- -grad(f,new.x) + as.vector(beta) * d
    x <- new.x
    iteracja <- iteracja +1
  }
}

#fletcher.reeves(f3,c(-4,4),0.001)
fletcher.reeves(mf4,c(-4,4),0.001)


fletcher.reeves.max <- function(f, x, tol) {
  iteracja <- 1
  beta <- 1
  d <- grad(f,x)
  repeat {
    g <- function(a) {f(x + a * d)}
    step <- golden_max(g,0,5,tol)
    new.x <- x + step * d
    cat("Iteracja: ", iteracja , "; x= ",x,"; f(x): ",f(x),"\n")
    if (dist(rbind(new.x,x)) < tol) {
      cat("Iteracja: ", iteracja +1 , "; x= ",x,"; f(x): ",f(x),"\n")
      return(new.x)
    }
    beta <- (grad(f,new.x) %*% grad(f,new.x)) / (grad(f,x) %*% grad(f,x))
    d <- grad(f,new.x) + as.vector(beta) * d
    x <- new.x
    iteracja <- iteracja +1
  }
}

fletcher.reeves.max(f4,c(-4,4), 0.001)

#forma kwadratowa
fk <- function(x){
  x[1]^2-x[1]*x[2]+x[2]^2-4*x[1]-x[2]
}

fki <- function(x,y){x^2-x*y+y^2-4*x-y}

x <- seq(-15, 15, length.out = 30) # podanie zakresu

y <- seq(-15, 15, length.out = 30) 

z <- outer(x,y, fki) # obliczenie wartości 

persp3d(x,y,z,col="lightblue",xlab="x",ylab="y",zlab="z",shade=0.8) #


fletcher.reeves(fk,c(-4,4),0.0001)
cauchy.golden(fk, c(-10,10),0.000001)

####################
#w projekcie trzeba uzyc metody quasi newtona
library(numDeriv)
library(matlib)
dfp <- function(f, x, tol) {
  n <- length(x)
  v <- diag(n)
  g <- function(a) {f(x-a*grad(f,x))}
  step <- golden(g,0,5,tol)
  iteracja <-1
  new.x <- x - step * grad(f, x)
  repeat {
    cat("Iteracja: ", iteracja , "; x= ",x,"; f(x): ",f(x),"\n")
    s <- grad(f,new.x) - grad(f,x)
    r <- new.x - x
    ma <- t(r) %*% s
    a <- (r %*% t(r))/ma[1,1]
    mb <- t(s) %*% v %*% s
    b <- -(v %*% s %*% t(s) %*% v)/mb[1,1]
    v <- v + a + b
    x <- new.x
    new.x <- x - v %*% grad(f,x)
    new.x <- new.x[,1]
    dist <- dist(rbind(new.x,x))
    if (dist(rbind(new.x, x)) < tol) {
      cat("Iteracja: ", iteracja +1 , "; x= ",x,"; f(x): ",f(x),"\n")
      return(new.x)
    }
    iteracja <- iteracja + 1
  }
}

dfp(f3,c(-4,4),0.000001)
dfp(f4,c(-4,4),0.001)
