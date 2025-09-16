source(here::here("scripts", "config.R"))

colonial <- import(here::here("datasets", "colonial_trade.dta"))

colonial <- colonial %>%
  mutate(ltrade_1 = log(trade+1))

out <- lapply(1:10, function(a){
  Ta <- colonial$trade * (10^a)
  ya <- log(1 + Ta)
  m  <- lm(ya ~ lyim + lyex + ldist + colony, data = transform(colonial, ya=ya))
  c(a=a, est=coef(m)["colony"], se=sqrt(vcov(m)["colony","colony"]))
})      ## Se usó IA para optimizar el bucle
res <- do.call(rbind, out)

res <- as.data.frame(res, stringsAsFactors = FALSE)
res[] <- lapply(res, function(x) as.numeric(x))

# Intervalos de confianza 95%
res$lo <- res$est.colony - 1.96 * res$se
res$hi <- res$est.colony  + 1.96 * res$se

# Coefplot
png(filename = file.path("outputs", "coefplot_log_t1.png"),
    width = 1200, height = 1000)
ggplot(res, aes(x = a, y = est.colony )) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, color = "#1f77b4") +
  geom_point(size = 2.4, color = "#1f77b4") +
  geom_line(color = "#1f77b4", linewidth = 0.6) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Coefplot: efecto de colony en ln(1 + 10^a · trade)",
    subtitle = "Intervalos de confianza al 95%",
    x = "a (k = 10^a: cambio de unidades)",
    y = "Coeficiente de colony"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
dev.off()


## Estimación de tau
## Diferencia de medias no condicional
with(colonial, {
  m1 <- mean(trade[colony==1], na.rm=TRUE)
  m0 <- mean(trade[colony==0], na.rm=TRUE)
  tau_star <- (m1 - m0) / m0
  tau_star
})

# Bootstrap IC 95%
set.seed(123)
B <- 1000
n <- nrow(colonial)
tboot <- replicate(B, {
  idx <- sample.int(n, n, replace=TRUE)
  d <- colonial[idx, ]
  m1 <- mean(d$trade[d$colony==1], na.rm=TRUE)
  m0 <- mean(d$trade[d$colony==0], na.rm=TRUE)
  (m1 - m0) / m0
})
quantile(tboot, c(.025, .975), na.rm=TRUE)

## regresión lineal sin transformar 
# Modelo lineal sin transformar
m <- lm(trade ~ colony + lyim + lyex + ldist, data = colonial)

# Predicciones contrafactuales
pred0 <- with(colonial, predict(m, newdata = data.frame(
  colony = 0, lyim = lyim, lyex = lyex, ldist = ldist
)))
pred1 <- with(colonial, predict(m, newdata = data.frame(
  colony = 1, lyim = lyim, lyex = lyex, ldist = ldist
)))

mu0 <- mean(pred0); mu1 <- mean(pred1)
tau_star_ra <- (mu1 - mu0) / mu0
tau_star_ra

# IC 95% por bootstrap
set.seed(123)
B <- 1000; n <- nrow(colonial)
tboot <- replicate(B, {
  idx <- sample.int(n, n, replace = TRUE)
  d <- colonial[idx, ]
  m <- lm(trade ~ colony + lyim + lyex + ldist, data = d)
  p0 <- with(d, predict(m, newdata = data.frame(colony=0, lyim=lyim, lyex=lyex, ldist=ldist)))
  p1 <- with(d, predict(m, newdata = data.frame(colony=1, lyim=lyim, lyex=lyex, ldist=ldist)))
  (mean(p1) - mean(p0)) / mean(p0)
})
quantile(tboot, c(.025, .975), na.rm = TRUE)
