library(ggplot2)
### Exercise 3.6.1 ###
#What geom would you use to draw a line chart? A boxplot? A histogram? An area chart?
geom_line()#line chart
geom_boxplot()#boxplot
geom_histogram()# histogram
geom_area()# area chart

### Exercise 3.6.2 ###
#Run this code in your head and predict what the output will look like. Then, run the code in R and check your predictions.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
### Este código produce un scatter plot con la variable displ en el eje x, la variable hwy en el eje y, y con los puntos coloreados por la variables drv. Además se genera una línea de regresión sin error estandar, ajustadas para cada grupo de drv. 

### Exercise 3.6.3 ###
#What does show.legend = FALSE do? What happens if you remove it? Why do you think I used it earlier in the chapter?
# La opción show.legend=FALSE oculta la leyenda del plot y si se fija como TRUE muestra la leyenda del plot

### Exercise 3.6.4 ###
#What does the se argument to geom_smooth() do?
#Añade bandas alrededor de la línea de regresión que indican el error estandar

### Exercise 3.6.5 ###
#Will these two graphs look different? Why/why not?
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
#No, en el primer caso las funciones geom_point() y geom_smooth() heredan los datos de la función ggplot, y en el segundo se define esto dentro de las funciones. Los datos y las representaciones son las mismas por lo que no cambia. 

### Exercise 3.6.6 ###
#Recreate the R code necessary to generate the following graphs.
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(group = drv), se = FALSE) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = drv)) +
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = drv)) +
  geom_smooth(aes(linetype = drv), se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 4, color = "white") +
  geom_point(aes(colour = drv))
