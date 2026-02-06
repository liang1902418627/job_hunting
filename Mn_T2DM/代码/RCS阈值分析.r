library(ggrcs)
library(rms)

dd <- datadist(data)
options(datadist = 'dd')

fit <- cph(Surv(time, allcmm == 1) ~ rcs(AIP, 3), x = TRUE, y = TRUE, data = data)

ggrcs(data = data, fit = fit, x = "AIP")
singlercs(data = data, fit = fit, x = "AIP")

####阈值效应
fit4 <- cph(Surv(time, allcmm == 1) ~ LCI, x = TRUE, y = TRUE, data = data)
tb4 <- cuttab(fit4, "LCI", data)