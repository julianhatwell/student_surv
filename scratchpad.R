library(survival)
library(asaur)
library(survminer)
library(muhaz)
library(MASS)
library(car)
library(ggplot2)
library(dplyr)

expit <- function(o) exp(o)/(1 + exp(o))
prog_length <- 52 * 3
weeks <- 1:prog_length

semester_ends <- c(13, 26, 40)
semester_ends <- semester_ends + rep(c(0, 53, 105), each = 3)
semester_ends <- c(0, semester_ends)

# residency codes: 
students <- read.csv("students.csv")

head(students)
gender_codes <- c("male", "female", "nonbin", "notsay")
residency_codes <- c("nonres", "resid", "intntnl")
students <- students %>% mutate(
  gender = factor(gender_codes[gender], levels = c("female", "male", "nonbin", "notsay"))
  , residency = residency_codes[residency]
)

# when can we intervene
View(students)

# smooth haz and survival       
# plot survivals for different covariates
# survdiff for different covariates
# model with cox.ph
# high correl between attend, week4, sem_test, visitlib, PCA?
# select model and diagnose


surv_students <- with(students, Surv(endweek - startweek, drops))
sfit <- with(students
             , survfit(surv_students ~ 1
                       , conf.type = "log-log"))
sfit
summary(sfit)
plot(sfit)

alph <- 0.05
# finding upper quantile
sfit_qtl <- quantile(sfit, 0.05)
medn <- sfit_qtl$`quantile`
lcl <- sfit_qtl$lower
ucl <- sfit_qtl$upper

plot(sfit, conf.int=T, mark="+", xlab="Time in weeks"
     , ylab="Active % of Cohort"
     , xlim=c(0, 156))
abline(h = 0.975, col = "red", lty = 3)
lines(rep(x = ucl, 2), y = c(0, 1-alph), col = "blue", lty = 2)
lines(rep(x = lcl, 2), y = c(0, 1-alph), col = "blue", lty = 2)
lines(rep(x = medn, 2), y = c(0, 1-alph), col = "green", lty = 2)

# muhaz smoothed instantaneous hazard estimation
sfit.smoothhaz <- muhaz(students$endweek, students$drops, max.time=156
                        , bw.grid=35, bw.method="global", b.cor="none")
plot(sfit.smoothhaz)

# getting smooth survival function from the smoothed hazard
haz <- sfit.smoothhaz$haz.est
times <- sfit.smoothhaz$est.grid
# numerically evaluate the integral. diff creates the widths
# cumsum adds up the areas of the resulting rectangles
surv <- exp(-cumsum(haz[1:(length(haz)-1)]*diff(times)))
lines(surv ~ times[1:(length(times) - 1)])

plot(sfit
     , col = "grey"
     , xlab="Time in weeks"
     , ylab="Active % of Cohort"
     , xlim=c(0, 156))
# smooth survival
lines(surv ~ times[1:(length(times) - 1)])

ggsurvevents(surv_students)

sfit.cox <- coxph(surv_students ~ fin +
                    hs_dip + 
                    founda +
                    week4_test + first_sem +
                    ft_pt +
                    visitlib +
                    attend_disc #+ strata(hs_dip)
                  , data = students
                  #, subset = (med < 4)
)
summary(sfit.cox)
# alterntive way to compare groups to cox prop haz
survdiff(surv_students ~ ft_pt
         , data = students, rho = 1) # rho 1 places more emph on early differences
# alterntive way to compare groups to cox prop haz
survdiff(surv_students ~ week4_test_disc + strata(age_disc)
         , data = students, rho = 1) # rho 1 places more emph on early differences

sfit <- with(students
             , survfit(surv_students ~ residency + oc
                       #, subset = (visitlib < 9)
                       , conf.type = "log-log"))
# plot(sfit, conf.int=FALSE, col=1:5
# , lwd = 2)

ggsurvplot(fit = sfit
           , data = students
           #, conf.int = TRUE
           , break.time.by = 52
           , color = "strata"
           , surv.scale = "percent"
           , censor = T
           , legend = "right"
           , xlab = "Time (weeks)"
           , ylab = "Active % of cohort"
           , risk.table = TRUE
           , xlim = c(1, prog_length)
           , ylim = c(0.5, 1.0)
           , ggtheme = theme_bw())


# checks
surv_students <- with(students, Surv(endweek - startweek, drops))
sfit.cox <- coxph(surv_students ~ fin +
                    hs_dip + 
                    founda +
                    week4_test + first_sem +
                    ft_pt +
                    visitlib +
                    attend_disc #+ strata(hs_dip)
                  , data = students
                  #, subset = (med < 4)
)
summary(sfit.cox)
# alterntive way to compare groups to cox prop haz
survdiff(surv_students ~ ft_pt
         , data = students, rho = 1) # rho 1 places more emph on early differences
# alterntive way to compare groups to cox prop haz
survdiff(surv_students ~ week4_test_disc + strata(age_disc)
         , data = students, rho = 1) # rho 1 places more emph on early differences

sfit <- with(students
             , survfit(surv_students ~ 1
                       #, subset = (visitlib < 9)
                       , conf.type = "log-log"))
# plot(sfit, conf.int=FALSE, col=1:5
# , lwd = 2)

ggsurvplot(fit = sfit
           , data = students
           #, conf.int = TRUE
           , break.time.by = 52
           , color = "strata"
           , surv.scale = "percent"
           , censor = T
           , legend = "right"
           , xlab = "Time (weeks)"
           , ylab = "Active % of cohort"
           , risk.table = TRUE
           , xlim = c(1, prog_length)
           , ylim = c(0.5, 1.0)
           , ggtheme = theme_bw())