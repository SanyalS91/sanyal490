library(tidyverse)
library(ggplot2)

ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy ))

#Exercices 3.2.4
ggplot(data = mpg)
str(mpg)
?mpg
ggplot(data = mpg)+geom_point(mapping = aes(x=hwy,y=cyl))

#Office exercise
data <- read.csv("D:/Analytics/R/Projects/4 for office/brusselsgdpforggplot.csv")
View(data)
ggplot(data=data)+geom_point(mapping = aes(x=Years,y=GDP))
#........

ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, color=class))
#ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, size=class))

#alpha aesthetic: changing size and transparency
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha=class))
ggplot(data = mpg) + geom_point(mapping = aes(x= displ, y = hwy, shape= class))

#changing the colours
ggplot(data= mpg) + geom_point(mapping = aes(x=displ, y=hwy), color="red")

#Exercises 3.3.1
str(mpg)
?mpg
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, color=trans, size=1,
                                              stroke=1))
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, size=cty))
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, shape=cty))
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, alpha=cty))
?geom_point
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, shape=class, 
                                          colour="red", fill="blue", size=1, 
                                          stroke=0))
#Facets
ggplot(data = mpg)+geom_point(mapping = aes(x=displ, y=hwy))+
  facet_wrap(~ class, nrow = 2)
ggplot(data = mpg)+geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(drv~cyl)
View(mpg)
ggplot(data = mpg)+geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(.~cyl)

#Exercise 3.5.1
ggplot(data = mpg)+geom_point(mapping = aes(x=displ, y=hwy))+
  facet_wrap(~cty, nrow = 2)
ggplot(data = mpg)+geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(.~cty)
ggplot(data = mpg)+geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(trans~cty)
ggplot(data = mpg)+geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(drv~cyl)
ggplot(data = mpg)+geom_point(mapping = aes(x=drv, y=cyl))
ggplot(data = mpg)+geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(drv~cyl)
plot1 <- ggplot(data = mpg)+geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(drv~.)
?facet_wrap

#Changing bacgrounds and removing gridlines
plot1 + theme(panel.grid.major = element_line(color = "black", size = 0.5, 
                                              linetype = 3,
                                              lineend = "round"),
              panel.grid.minor = element_line(colour="black", size=0.5, linetype=3,
                                              lineend="round"))
plot1 + theme(panel.background = element_rect(fill = "lightpink", 
                                              colour = "lightpink"))
plot1+theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "lightpink"))

#Applying themes and changing axis size
install.packages("ggthemes")
library(ggthemes)
ggplot(data = mpg)+geom_point(mapping = aes(x=displ,y=hwy))+theme_economist()+ggtitle("Relationship between Displacement & Highway")+theme(plot.title = element_text(size = 11))
                        
#Changing plots
ggplot(data = mpg)+geom_smooth(mapping = aes(x=displ,y=hwy))

install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

#statistical transformations
ggplot(data = diamonds) + geom_bar(mapping = aes(x=cut))
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()