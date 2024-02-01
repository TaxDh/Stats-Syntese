library(Sleuth3)

data("case0201")
attach(case0201)

t.test(Depth~Year, var.equal = TRUE, conf.level = 0.9)#dernier test t à la main

