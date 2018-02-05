setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Interactions")

pdf("interactions_male.pdf", height=4, width=6)
par(mar=c(3,3,2,1), mgp=c(1.7,.5,0), tck=-.01)
plot(c(0,12.5),c(0,.25), type="n", xaxs="i", yaxs="i",
  xlab="Home radon exposure (pCi/L)", ylab="Probability of lung cancer", bty="l", main="Example of an interaction") 
lines(c(0,20),.07409+c(0,20)*.0134)
lines(c(0,20),.00579+c(0,20)*.0026)
text(10, .07409+10*.0134 - .02, "Smokers")
text(10, .00579+10*.0026 + .01, "Nonsmokers")
dev.off()
