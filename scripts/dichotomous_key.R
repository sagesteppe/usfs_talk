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
ggpairs(iris, columns = 1:4, aes(color = Species, shape = Species), 
        upper = "blank", diag = "blank") + 
  scale_color_manual(values = pal) +
  theme_bw() + 
  labs(title = 'screen prospective traits', subtitle = "") + 
  theme(
    text = element_text(family = "sans serif"),
    plot.title = element_text(size = 32, family = "sans serif", hjust = 0.5),
    plot.subtitle = element_text(size = 22))

ggsave('../results/Iris_Pairs.png', device = 'png', dpi = 150, units = "px",
       width = 1920, height = 1080)

# first promising plot
ggplot(iris) +
  geom_point(aes(x = Petal.Length, y = Petal.Width, 
                 color = Species, shape = Species), size = 3) + 
  scale_color_manual(values = pal) + 
  theme_bw() + 
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = 'Scatter plot of Petal Width and Sepal Length in Iris', 
       x = 'Petal length (cm)', y  = 'Petal Width (cm)') + 
  
  labs(title = 'evaluate useful traits', subtitle = "") + 
  theme(
    text = element_text(family = "sans serif"),
    plot.title = element_text(size = 32, family = "sans serif", hjust = 0.5),
    plot.subtitle = element_text(size = 22))

ggsave('../results/Iris_Petal_Vars.png', device = 'png', dpi = 150, units = "px",
       width = 1920, height = 1080)

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
           angle = 90) + 
  
  
  labs(title = 'identify first split', subtitle = "") + 
  theme(
    text = element_text(family = "sans serif"),
    plot.title = element_text(size = 32, hjust = 0.5),
    plot.subtitle = element_text(size = 22))


ggsave('../results/Iris_couplet1.png', device = 'png', dpi = 150, units = "px",
       width = 1920, height = 1080)

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

  geom_segment(x = 2.5, y = 1.8, xend = 8, yend = 1.8) +
  annotate("text", label = "petal width >= 1.8 cm", x = 3.5, y = 1.85, size = 5)  + 
  
  labs(title = 'identify second split', subtitle = "") + 
  theme(
    plot.title = element_text(size = 32, family = "sans serif", hjust = 0.5),
    plot.subtitle = element_text(size = 22))

ggsave('../results/Iris_couplet2.png', device = 'png', dpi = 150, units = "px",
       width = 1920, height = 1080)

# create a key

iris_model_cart <- rpart(Species ~., data = iris)
rpart.plot(iris_model_cart, type = 5)

png(filename = '../results/Iris_tree_key-sans.png', 
    width = 1728, height = 972, units = "px")
rpart.plot(iris_model_cart, type = 5, main = "", cex.main = 10, family  = "sans serif", cex = 2)
dev.off()

## we can convert this into english quite quickly....

# 1. Petal length less than 2.5 cm .... I. setosa Pall ex. Link
# 1', Petal length greater than 2.5 cm ..... 2
# 2, petal width less than 1.8 cm .......... I. versicolor L.
# 2', petal width greater than 1.8 cm ...... I. virginica L.

## what we could have done....

iris_model_rf <- randomForest(Species ~., data = iris, ntrees = 500, mtry = 3) 
reprtree:::plot.getTree(iris_model_rf)

png(filename = '../results/Iris_tree_key2.png', 
    width = 960, height = 540, units = "px")
reprtree:::plot.getTree(iris_model_rf, family  = "sans serif", cex = 1.1)
dev.off()

# Notice we have the same two main split features, petal length and then petal width. 

rm(pal, iris_model_rf, iris_model_cart)



