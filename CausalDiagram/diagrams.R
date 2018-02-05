setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/CausalDiagram")

pdf("diagram1.pdf", height=4.5, width=10)
par(mar=c(0,0,0,0))
plot(c(-.5,4), c(0,1.8), xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
text(0, 1, "x", cex=2.4)
text(1, 1, "z", cex=2.4)
text(3.5, .5, expression (y^0), cex=2)
text(3.5, 1.5, expression (y^1), cex=2)
text(c(0,1,3.5), c(1.8,1.8,1.8), c("Before","Treatment","Outcome"), cex=2)
text(1.9, .72, "Treatment 0 (z=0)", cex=2, srt=-16)
text(1.9, 1.27, "Treatment 1 (z=1)", cex=2, srt=14.3)
arrows(c(1.1,1.1), c(1,1), c(3.3,3.3), c(.5, 1.5))
arrows(-.3,.2,4,.2, length=.2)
text(1.8, .1, "Time", cex=2)
dev.off()

# Diagram 2 for causal inference

