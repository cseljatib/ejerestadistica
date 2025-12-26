###
require(pscl)
data(bioChemists)
df<-bioChemists
head(df)

table(df$art)
table(df$mar)
table(df$phd)

head(df)

names(df)<-c("papers","genero","est.civil","nin.men5","prog.prest","papers.pguia")

write.csv(df, file="papersDocStud.csv",row.names = F)

#https://cran.r-project.org/web/packages/pscl/pscl.pdf

#mle stuff
#https://stats.oarc.ucla.edu/r/dae/poisson-regression/
require(ggplot2)
require(sandwich)
require(msm)

p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)

with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")

summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

## update m1 model dropping prog
m2 <- update(m1, . ~ . - prog)
## test model differences with chi square test
anova(m2, m1, test="Chisq")
