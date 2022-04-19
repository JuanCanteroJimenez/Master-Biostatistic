### TAREA 7

### 1
A <- matrix(c(3,2,1,
              0,1,-1,
              1,1,1),byrow = TRUE, ncol=3, nrow=3)
B <- matrix(c(2,7,-3,
              0,1,-1,
              1,1,1),byrow = TRUE, ncol=3, nrow=3)
det(A)
### como el determinante de A es != 0, si posee inversa
solve(A)

det(B)
### como el determinante de B es = 0, no posee inversa

###2

result <- c(6,0,3)
solucion <- solve(A,result)
A%*%solucion

solve(B, result)

###3

A <- matrix(c(2,-1,
              -1,2),byrow=TRUE, ncol=2,nrow=2)
b <- c(1,0)
c <- c(0,1)

xs <- list(x1=c(1,1), x2 = c(2/3, 1/3), x3 = c(-1/3, 2/3), x4 = c(1, 2))

dummy <- lapply(xs, function(x, A, b, c){
  if (  all((A%*%x)==b) ){
    cat("El sistema AX=b, tiene a",x,"como solución")
  }
  if ( all ((A%*%x)==c)){
    cat("El sistema AX=c, tiene a",x,"como solución")
  }
}, A=A, b=b, c=c)

solve(A)%*%b ### x = c(2/3, 1/3)
A%*%c(2/3, 1/3)
solve(A)%*%c ### no existe una solución entre las proporcionadas
 
