# lmPG: a home brewed S4 linear regression class

The purpose is not to replace the S3 lm class, but to learn how it works (and
get some practice with S4!)

I have built a class definition, and a plot, print, summary and predict generic

### Usage:

```R
turnip <- lmp("mpg", c("wt", "hp", "qsec"), int = TRUE, df = mtcars)

turnip

plot(turnip)

summary(turnip)

predict(turnip,
 newdata = mtcars[, names(mtcars) %in% c("wt", "hp", "qsec")]))
```
