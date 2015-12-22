# lmPG: a home brewed S4 linear regression class

The purpose is not to replace the S3 lm class, but to learn how it works (and
get some practice with S4!)

I have built a class definition, and a plot, show, summary and predict generic.
Just as a demonstration, I have also included an accessor.

### Usage:

```R
# fit the model
turnip <- lmp("mpg", c("wt", "hp", "qsec"), int = TRUE, df = mtcars)

# show
turnip

# plot
plot(turnip)

# summary
summary(turnip)

# predict
predict(turnip,
 newdata = mtcars[, names(mtcars) %in% c("wt", "hp", "qsec")]))

# accessor to get the coefficients
getCoef(turnip)
```
