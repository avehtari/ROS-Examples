setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Earnings")
library("arm")
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))

## classical regressions and graphs for earnings example

earnings_all <- read.csv("earnings.csv")
earnings_all$positive <- earnings_all$earn > 0
earnings <- earnings_all[earnings_all$positive, ]
n <- nrow(earnings)
height_jitter_add <- runif(n, -.2, .2)

## model on original scale
lm_0 <- lm(earn ~ height, data=earnings)
display(lm_0)

# plot linear model
pdf("heights1a.pdf", height=8.5, width=11)
par(mar=c(6,6,4,2)+.1)
plot(earnings$height + height_jitter_add, earnings$earn, xlab="height", ylab="earnings",
     cex=.8, cex.lab=3, pch=20, cex.axis=3, yaxt="n", mgp=c(4,1.5,0),
     col="gray10",  cex.main=3, main="Fitted linear model")
abline(coef(lm_0), lwd=2)
abline(0,0,col="gray")
axis(2, c(0,100000,200000), c("0","100000","200000"), mgp=c(4,1.1,0),cex.axis=3)
dev.off()

# ggplot version
gg_earnings <- ggplot(earnings, aes(x = height + height_jitter_add, y = earn)) +
  geom_point(alpha = 0.75) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_abline(intercept = coef(lm_0)[1], slope = coef(lm_0)[2], size = 1) +
  labs(x = "height", y = "earnings",
       title = "Fitted linear model")
gg_earnings


# plot extrapolation
pdf("heights1b.pdf", height=8.5, width=11)
par(mar=c(6,6,4,2)+.1)
plot(xlim=c(0,max(earnings$height)), ylim=c(-70000,200000), earnings$height + height_jitter_add, earnings$earn, xlab="height", ylab="earnings", cex=.8, cex.lab=3, pch=20, cex.axis=3, yaxt="n", mgp=c(4,1.5,0), col="gray10", cex.main=3, main="Extrapolation")
abline(coef(lm_0), lwd=2)
abline(0,0,col="gray")
intercept <- coef(lm_0)[1]
axis(2, c(intercept,0,100000,200000), c(round(intercept,-2),"0","100000","200000"), mgp=c(4,1.1,0),cex.axis=3)
dev.off()

# ggplot version, modifying the gg_earnings object we already created
gg_earnings +
  ylim(-70000, 200000) +
  xlim(0, 80) +
  labs(title = "Extrapolation")


# include male/female
lm_1 <- lm(earn ~ height + male, data=earnings)
display(lm_1)
coef1 <- coef(lm_1)

pdf("heights2.pdf", height=8.5, width=11)
par(mar=c(6,6,5,2)+.1)
plot(range(earnings$height), range(predict(lm_1)), type="n", xlab="height", ylab="predicted earnings", cex=.8, cex.lab=3, pch=20, cex.axis=3, mgp=c(4,1.5,0), yaxt="n", col="gray10",
      cex.main=3, main="Fitted regression, displayed as\nseparate lines for men and women", bty="l")
axis(2, c(20000,30000), cex.axis=3)
abline(coef1[1], coef1[2], col="red", lwd=2)
text(68, coef1[1] + coef1[2]*65, "women:\ny = -11,000 + 450x", cex=3, adj=0, col="red")
abline(coef1[1]+coef1[3], coef1[2], col="blue", lwd=2)
text(68, coef1[1]+coef1[3] + coef1[2]*65, "men:\ny = -2,000 + 450x", cex=3, adj=0, col="blue")
dev.off()

# ggplot version
ggplot(earnings, aes(height, earn)) +
  geom_blank() +
  geom_abline(
    intercept = c(coef1[1], coef1[1] + coef1[3]),
    slope = coef1[2],
    color = c("red", "blue")
  ) +
  coord_cartesian(
    ylim = range(predict(lm_1)),
    xlim = range(earnings$height)
  ) +
  annotate(
    "text",
    x = c(68, 68),
    y = c(coef1[1] + coef1[2] * 65, coef1[1] + coef1[3] + coef1[2] * 65),
    label = c("women:\ny = -11,000 + 450x", "men:\ny = -2,000 + 450x"),
    color = c("red", "blue"),
    size = 5, hjust = 0
  ) +
  labs(
    x = "height",
    y = "predicted earnings",
    title = "Fitted regression, displayed as\nseparate lines for men and women"
  )



lm_2 <- lm(earn ~ height + male + height*male, data=earnings)
display(lm_2)
coef2 <- coef(lm_2)

pdf("heights3.pdf", height=8.5, width=11)
par(mar=c(6,6,5,2)+.1)
plot(range(earnings$height), range(predict(lm_2)), type="n", xlab="height", ylab="predicted earnings", cex=.8, cex.lab=3, pch=20, cex.axis=3, mgp=c(4,1.5,0), yaxt="n", col="gray10", cex.main=3, main="Fitted regression with interactions,\nseparate lines for men and women", bty="l")
axis(2, c(20000,30000), cex.axis=3)
abline(coef2[1], coef2[2], col="red", lwd=2)
text(62, coef2[1] + coef2[2]*80, "women:\ny = -7,000 + 180x", cex=3, adj=0, col="red")
abline(coef2[1]+coef2[3], coef2[2]+coef2[4], col="blue", lwd=2)
text(68, coef2[1]+coef2[3] + (coef2[2]+coef2[4])*66, "men:\ny = -22,000 + 740x", cex=3, adj=0, col="blue")
dev.off()

# ggplot version
ggplot(earnings, aes(height, earn)) +
  geom_blank() +
  geom_abline(
    intercept = c(coef2[1], coef2[1] + coef2[3]),
    slope = c(coef2[2], coef2[2] + coef2[4]),
    color = c("red", "blue")
  ) +
  coord_cartesian(
    ylim = range(predict(lm_2)),
    xlim = range(earnings$height)
  ) +
  annotate(
    "text",
    x = c(62, 68),
    y = c(coef2[1] + coef2[2] * 80, coef2[1]+coef2[3] + (coef2[2]+coef2[4])*66),
    label = c("women:\ny = -7,000 + 180x", "men:\ny = -22,000 + 740x"),
    color = c("red", "blue"),
    size = 5, hjust = 0
  ) +
  labs(
    x = "height",
    y = "predicted earnings",
    title = "Fitted regression with interactions,\nseparate lines for men and women"
  )


# model on log scale
earnings$log_earn <- log(earnings$earn)
logmodel_1 <- lm(log_earn ~ height, data=earnings)
display(logmodel_1)

earnings$log10_earn <- log10(earnings$earn)
display(lm(log10_earn ~ height, data=earnings))

logmodel_2 <- lm(log_earn ~ height + male, data=earnings)
display(logmodel_2)

earnings$log_height <- log(earnings$height)
loglogmodel_2 <- lm(log_earn ~ log_height + male, data=earnings)
display(loglogmodel_2)

logmodel_3 <- lm(log_earn ~ height + male + height:male, data=earnings)
display(logmodel_3)

earnings$z_height <- with(earnings, (height - mean(height))/sd(height))
logmodel_3a <- lm(log_earn ~ z_height + male + z_height:male, data=earnings)
display(logmodel_3a)

M_1 <- stan_glm(log_earn ~ height + male, data = earnings)
sims <- as.matrix(M_1)
n_sims <- nrow(sims)

postscript("heights.log1a.ps", horizontal=TRUE)
par(mar=c(6,6,4,2)+.1)
plot(earnings$height + runif(n,-.2,.2), earnings$log_earn, xlab="height", ylab="log (earnings)", cex=.8, cex.lab=3, pch=20, cex.axis=3, yaxt="n", mgp=c(4,1.5,0), col="gray10",
      cex.main=3, main="Log regression, plotted on log scale")
axis(2, seq(6,12,2), mgp=c(4,1.1,0),cex.axis=3)
subset <- sample(n_sims, 10)
for (i in subset){
  curve(sims[i,1] + sims[i,2]*x, lwd=0.5, col="gray30", add=TRUE)
}
curve(coef(M_1)[1] + coef(M_1)[2]*x, add=TRUE)
dev.off()

# ggplot version
subset <- sample(n_sims, 10)
ggplot(earnings, aes(height, log_earn)) +
  geom_jitter(height = 0, width = 0.25) +
  geom_abline(
    intercept = sims[subset, 1],
    slope = sims[subset, 2],
    color = "darkgray"
  ) +
  geom_abline(
    intercept = coef(M_1)[1],
    slope = coef(M_1)[2]
  ) +
  labs(
    x = "height",
    y = "log(earnings)",
    title = "Log regression, plotted on log scale"
  )

####


fit_1a <- stan_glm(positive ~ height + male,
                   family = binomial(link = "logit"),
                   data = earnings_all)
sims_1a <- as.matrix(fit_1a)
fit_1b <- stan_glm(log_earn ~ height + male, data = earnings)
sims_1b <- as.matrix(fit_1b)

print(fit_1a, digits=2)
print(fit_1b, digits=2)

new <- data.frame(height = 68, male = 0)
pred_1a <- posterior_predict(fit_1a, newdata=new)
pred_1b <- posterior_predict(fit_1a, newdata=new)
pred <- ifelse(pred_1a == 1, exp(pred_1b), 0)

