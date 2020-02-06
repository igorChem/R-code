# Package ggplot

library(ggplot2)

#+.gg  Modify a ggplot or theme object by adding on new components

### Adding objects to a ggplot object
p <- qplot(wt, mpg, colour = hp, data = mtcars)
p + coord_cartesian(ylim = c(0, 40))
p + scale_colour_continuous(breaks = c(100, 300))
p + guides(colour = "colourbar")
# Use a different data frame
m <- mtcars[1:10, ]
p %+% m


#aes
#Generate aesthetic mappings that describe how variables in the data
#are mapped to visual properties (aesthetics) of geoms.

aes(x = mpg, y = wt)
aes(x = mpg ^ 2, y = wt / cyl)

#annotate Create an annotation layer.

p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + annotate("text", x = 4, y = 25, label = "Some text")
p + annotate("text", x = 2:5, y = 25, label = "Some text")

p + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21,
alpha = .2)

p + annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25,
colour = "blue")
p + annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,
colour = "red", size = 1.5)
p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))

#autoplot Create a complete ggplot appropriate to a particular data type

autoplot(object, ...)

object  an object, whose class will determine the behaviour of autoplot


#fortify-multcomp  Fortify methods for objects produced by multcomp


if (require("multcomp")) {
amod <- aov(breaks ~ wool + tension, data = warpbreaks)
wht <- glht(amod, linfct = mcp(tension = "Tukey"))
fortify(wht)
ggplot(wht, aes(lhs, estimate)) + geom_point()
CI <- confint(wht)
fortify(CI)
ggplot(CI, aes(lhs, estimate, ymin = lwr, ymax = upr)) +
geom_pointrange()
fortify(summary(wht))
ggplot(mapping = aes(lhs, estimate)) +
geom_linerange(aes(ymin = lwr, ymax = upr), data = CI) +
geom_point(aes(size = p), data = summary(wht)) +
scale_size(trans = "reverse")
cld <- cld(wht)
fortify(cld)
}

#fortify.lm  Supplement the data fitted to a linear model with model fit statistics.


mod <- lm(mpg ~ wt, data = mtcars)
head(fortify(mod))
head(fortify(mod, mtcars))

plot(mod, which = 1)
qplot(.fitted, .resid, data = mod) +
geom_hline(yintercept = 0) +
geom_smooth(se = FALSE)
qplot(.fitted, .stdresid, data = mod) +
geom_hline(yintercept = 0) +
geom_smooth(se = FALSE)
qplot(.fitted, .stdresid, data = fortify(mod, mtcars),
colour = factor(cyl))
qplot(mpg, .stdresid, data = fortify(mod, mtcars), colour = factor(cyl))

plot(mod, which = 2)
# qplot(sample =.stdresid, data = mod, stat = "qq") + geom_abline()
plot(mod, which = 3)
qplot(.fitted, sqrt(abs(.stdresid)), data = mod) + geom_smooth(se = FALSE)
plot(mod, which = 4)
qplot(seq_along(.cooksd), .cooksd, data = mod, geom = "bar",
stat="identity")
plot(mod, which = 5)
qplot(.hat, .stdresid, data = mod) + geom_smooth(se = FALSE)
ggplot(mod, aes(.hat, .stdresid)) +
geom_vline(size = 2, colour = "white", xintercept = 0) +
geom_hline(size = 2, colour = "white", yintercept = 0) +
geom_point() + geom_smooth(se = FALSE)
qplot(.hat, .stdresid, data = mod, size = .cooksd) +
geom_smooth(se = FALSE, size = 0.5)
plot(mod, which = 6)
ggplot(mod, aes(.hat, .cooksd)) +
geom_vline(xintercept = 0, colour = NA) +
geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") +
geom_smooth(se = FALSE) +
geom_point()
qplot(.hat, .cooksd, size = .cooksd / .hat, data = mod) + scale_size_area()



#fortify.map Fortify method for map objects.


if (require("maps")) {
ca <- map("county", "ca", plot = FALSE, fill = TRUE)
head(fortify(ca))
qplot(long, lat, data = ca, geom = "polygon", group = group)
tx <- map("county", "texas", plot = FALSE, fill = TRUE)
head(fortify(tx))
qplot(long, lat, data = tx, geom = "polygon", group = group,
colour = I("white"))
}



#fortify.sp Fortify method for classes from the sp package.

if (require("maptools")) {
sids <- system.file("shapes/sids.shp", package="maptools")
nc1 <- readShapePoly(sids,
proj4string = CRS("+proj=longlat +datum=NAD27"))
nc1_df <- fortify(nc1)
}


#geom_boxplot Box and whiskers plot.


p <- ggplot(mtcars, aes(factor(cyl), mpg,disp))
p + geom_boxplot()
qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot")
p + geom_boxplot() + geom_jitter()
p + geom_boxplot() + coord_flip()
qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot") +
coord_flip()
p + geom_boxplot(notch = TRUE)
p + geom_boxplot(notch = TRUE, notchwidth = .3)
p + geom_boxplot(outlier.colour = "green", outlier.size = 3) # esse é bem legal 
#
 Add aesthetic mappings
#
 Note that boxplots are automatically dodged when any aesthetic is
#
 a factor
p + geom_boxplot(aes(fill = cyl))
p + geom_boxplot(aes(fill = factor(cyl)))
p + geom_boxplot(aes(fill = factor(vs)))
p + geom_boxplot(aes(fill = factor(am)))

library(plyr)
m <- ggplot(movies, aes(y = votes, x = rating,
group = round_any(rating, 0.5)))
m + geom_boxplot()
m + geom_boxplot() + scale_y_log10()
m + geom_boxplot() + coord_trans(y = "log10")
m + geom_boxplot() + scale_y_log10() + coord_trans(y = "log10")



m <- ggplot(movies, aes(x=rating))
m + geom_histogram()
m + geom_histogram(aes(y = ..density..)) + geom_density()

