
#install.packages("ggrepel", dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages("ez", dependencies = TRUE, repos='http://cran.rstudio.com/')
library(ggplot2)
library("ggrepel")
library(ez)
library(scales)

housing <- read.csv("Rgraphics/dataSets/landdata-states.csv")
head(housing[1:5])

hist(housing$Home.Value)

ggplot(housing, aes(x = Home.Value)) + geom_histogram()

plot(Home.Value ~ Date,
     data=subset(housing, State == "MA"))
points(Home.Value ~ Date, col="red",
       data=subset(housing, State == "TX"))
legend(1975, 400000,
       c("MA", "TX"), title="State",
       col=c("black", "red"),
       pch=c(1, 1))

ggplot(subset(housing, State %in% c("MA", "TX")),
       aes(x=Date,
           y=Home.Value,
           color=State))+
  geom_point()

help.search("geom_",package="ggplot2")


hp2001Q1 <- subset(housing, Date == 2001.25) 
ggplot(hp2001Q1,aes(y = Structure.Cost, x = Land.Value)) + geom_point()

ggplot(hp2001Q1,
       aes(y = Structure.Cost, x = log(Land.Value))) +
  geom_point()

hp2001Q1$pred.SC <- predict(lm(Structure.Cost ~ log(Land.Value), data = hp2001Q1))
p1 <- ggplot(hp2001Q1, aes(x = log(Land.Value), y = Structure.Cost))
p1 + geom_point(aes(color = Home.Value)) + geom_line(aes(y = pred.SC))

p1 + geom_point(aes(color = Home.Value)) + geom_smooth()

p1 + geom_text(aes(label=State), size = 3)

#Error not working - no lines coming 
p1 + geom_point() + geom_text_repel(aes(label=State), size = 3)

p1 +
  geom_point(aes(size = 2),# incorrect! 2 is not a variable
             color="red") # this is fine -- all points red

p1 +
  geom_point(aes(color=Home.Value, shape = region))

p2 <- ggplot(housing, aes(x = Home.Value))
p2 + geom_histogram()

p2 + geom_histogram(stat = "bin", binwidth=4000)

housing.sum <- aggregate(housing["Home.Value"], housing["State"], FUN=mean)
rbind(head(housing.sum), tail(housing.sum))

ggplot(housing.sum, aes(x=State, y=Home.Value)) + 
  geom_bar(stat="identity")
#Here default stat = stat_count wont work

p3 <- ggplot(housing,
             aes(x = State,
                 y = Home.Price.Index)) + 
        theme(legend.position="top",
              axis.text=element_text(size = 6))
(p4 <- p3 + geom_point(aes(color = Date),
                       alpha = 0.5,
                       size = 1.5,
                       position = position_jitter(width = 0.25, height = 0)))

p4 + scale_x_discrete(name="State Abbreviation") +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"))

p4 +
  scale_x_discrete(name="State Abbreviation") +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"),
                         low = "blue", high = "red")

p4 +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"),
                         low = muted("blue"), high = muted("red"))


p4 +
  scale_color_gradient2(name="",
                        breaks = c(1976, 1994, 2013),
                        labels = c("'76", "'94", "'13"),
                        low = muted("blue"),
                        high = muted("red"),
                        mid = "gray60",
                        midpoint = 1994)

p5 <- ggplot(housing, aes(x = Date, y = Home.Value))
p5 + geom_line(aes(color = State))

(p5 <- p5 + geom_line() +
   facet_wrap(~State, ncol = 10))

p5 + theme_linedraw()

p5 + theme_light()

p5 + theme_minimal() +
  theme(text = element_text(color = "turquoise"))

theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "blue", fill = "black"),
        text=element_text(size = 12, family = "Serif", color = "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"),
        strip.background = element_rect(fill = muted("orange")))

p5 + theme_new

housing.byyear <- aggregate(cbind(Home.Value, Land.Value) ~ Date, data = housing, mean)
ggplot(housing.byyear,
       aes(x=Date)) +
  geom_line(aes(y=Home.Value), color="red") +
  geom_line(aes(y=Land.Value), color="blue")



library(tidyr)
home.land.byyear <- gather(housing.byyear,
                           value = "value",
                           key = "type",
                           Home.Value, Land.Value)
ggplot(home.land.byyear,
       aes(x=Date,
           y=value,
           color=type)) +
  geom_line()
