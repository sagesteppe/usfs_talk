library(rpart)
library(rpart.plot)
library(randomForest)
library(GGally)
library(reprtree)

pal <- setNames(
  c('#e41a1c', '#377eb8', '#4daf4a'),
  c('versicolor', 'setosa', 'virginica')
)

#### Create the pairs plots
ggpairs(iris, columns = 1:4, aes(color = Species), 
        upper = "blank", diag = "blank") + 
  scale_color_manual(values = pal) +
  theme_bw()

# first promising plot
ggplot(iris) +
  geom_point(aes(x = Petal.Length, y = Petal.Width, 
                 color = Species, shape = Species), size = 3) + 
  scale_color_manual(values = pal) + 
  theme_bw() + 
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = 'Scatter plot of Petal Width and Sepal Length in Iris', 
       x = 'Petal length (cm)', y  = 'Petal Width (cm)')

# first division plot
ggplot(iris) +
  geom_point(aes(x = Petal.Length, y = Petal.Width, 
                 color = Species, shape = Species), size = 3) + 
  scale_color_manual(values = pal) + 
  theme_bw() + 
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = 'Scatter plot of Petal Width and Sepal Length in Iris', 
       x = 'Petal length (cm)', y  = 'Petal Width (cm)') +
  
  geom_segment(x = 2.5, y = 0.1, xend = 2.5, yend = 2.5) +
  annotate("text", label = "petal length >= 2.5 cm", x = 2.6, y = 1, size = 5, 
           angle = 90) 

# second division plot
ggplot(iris) +
  geom_point(aes(x = Petal.Length, y = Petal.Width, 
                 color = Species, shape = Species), size = 3) + 
  scale_color_manual(values = pal) + 
  theme_bw() + 
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = 'Scatter plot of Petal Width and Sepal Length in Iris', 
       x = 'Petal length (cm)', y  = 'Petal Width (cm)') +
  
  geom_segment(x = 2.5, y = 0.1, xend = 2.5, yend = 2.5) +
  annotate("text", label = "petal length >= 2.5 cm", x = 2.6, y = 1, size = 5, 
           angle = 90) +

  geom_segment(x = 2.5, y = 1.75, xend = 8, yend = 1.75) +
  annotate("text", label = "petal width >= 1.8 cm", x = 3.5, y = 1.85, size = 5) 


# create a key

iris_model <- rpart(Species ~., data = iris)
rpart.plot(iris_model, type = 5)


## we can convert this into english quite quickly....

# 1. Petal length less than 2.5 cm .... I. setosa Pall ex. Link
# 1', Petal length greater than 2.5 cm ..... 2
# 2, petal width less than 1.8 cm .......... I. versicolor L.
# 2', petal width greater than 1.8 cm ...... I. virginica L.

## what we could have done....

iris_model <- randomForest(Species ~., data = iris, ntrees = 500, mtry = 3) 
reprtree:::plot.getTree(iris_model)

# you skeptical yet?
