sm<-read.csv(file = "sugarMapleBiomass.csv")
head(sm)

plot(leaf~ba, data=sm)


#ajuste un RLS
m1.sm <- lm(leaf~ba, data=sm)
summary(m1.sm)

plot(m1.sm)

#WLS
m2.sm <- lm(leaf~ba, data=sm, weights = I(ba^-1))
summary(m2.sm)


c(AIC(m1.sm),AIC(m2.sm))
c(BIC(m1.sm),BIC(m2.sm))

plot(m2.sm)


m3.sm <- lm(leaf~ba, data=sm, weights = I(ba^-2))
summary(m3.sm)


c(AIC(m1.sm),AIC(m2.sm),AIC(m3.sm))
c(BIC(m1.sm),BIC(m2.sm),BIC(m3.sm))

plot(m3.sm)


require(nlme)
m2b.sm <- gls(leaf~ba, data=sm, weights = varFixed(~ba))
summary(m2b.sm)


###
m4.sm <- gls(leaf~ba, data=sm, weights = varPower(form=~ba))
summary(m4.sm)

c(AIC(m1.sm),AIC(m2.sm),AIC(m3.sm),AIC(m4.sm))
c(BIC(m1.sm),BIC(m2.sm),BIC(m3.sm),BIC(m4.sm))

plot(m4.sm)


##
m5.sm <- gls(leaf~ba, data=sm, weights = varConstPower(form=~ba))
summary(m5.sm)

c(AIC(m1.sm),AIC(m2.sm),AIC(m3.sm),AIC(m4.sm),AIC(m5.sm))
c(BIC(m1.sm),BIC(m2.sm),BIC(m3.sm),BIC(m4.sm),BIC(m5.sm))
