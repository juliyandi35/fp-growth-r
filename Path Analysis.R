library(psych)
library(lavaan)
library(semPlot)

set.seed(1234)
N <- 1000
iv1 <- rnorm(N, 0, 1)
iv2 <- rnorm(N, 0, 1)
iv3 <- rnorm(N, 0, 1)
mv <- rnorm(N, .2 * iv1 + -.2 * iv2 + .3 * iv3, 1)
dv <- rnorm(N, .8 * mv, 1)
data_1 <- data.frame(iv1, iv2, iv3, mv, dv)

summary(lm(mv ~ iv1 + iv2 + iv3, data_1))

summary(lm(dv ~ iv1 + iv2 + iv3 + mv, data_1))

psych::describe(data_1)
pairs.panels(data_1, pch='.')

m1_model <- '
dv ~ mv
mv ~ iv1 + iv2 + iv3
'

m1_fit <- sem(m1_model, data=data_1)

parameterestimates(m1_fit)

fitmeasures(m1_fit)

m2_model <- '
    dv ~ b1*mv
    mv ~ a1*iv1 + a2*iv2 + a3*iv3

    # indirect effects
    iv1_mv := a1*b1
    iv2_mv := a2*b1
    iv3_mv := a3*b1
'

m2_fit <- sem(m2_model, data=data_1)

parameterestimates(m2_fit, standardize=TRUE)

fit <- cfa(m1_model, data = data_1)

summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

semPaths(fit, 'std', layout = 'circle')

semPaths(fit,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)
