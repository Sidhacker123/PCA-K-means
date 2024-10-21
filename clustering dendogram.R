
states<- row.names(USArrests)
states
USArrest
hc1 <- hclust(dist(USArrests),method="complete")
plot(hc1, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9)
hc2 <- cutree(hc1,3)
hc2
# Plot the dendrogram
plot(hc1, labels = rownames(USArrests), main = "Dendrogram of USArrests Data", sub = "", xlab = "", cex = 0.6)

# Add colored borders to show 3 clusters
rect.hclust(hc1, k = 3, border = 2:4)
x= scale(USArrests)
hc2 <-hclust(dist(x),method="complete")
plot(hc2,main = "Clustering with scale",sub="",xlab="",cex=0.9)


