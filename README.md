# K-clustering
K-clustering experiment with Expedia hotel destinations
##load destinations.cvs data
dest <- read.csv("~/Desktop/Expedia/destinations.csv", header = T, sep=",", stringsAsFactors = F)
head(dest, 10)

##I took little random sample for easy emplementation
set.seed(12345678)
mysam1  <- dest[sample(1:nrow(dest), 50, replace= FALSE),]
summary(mysam1)
## 1 forward
forward = regsubsets(d1~., mysam1, method ="forward")
summary(forward)
## its get me d35, d52,d57, d62, d89, d119, d145 forsubset to use for PCA
mysam2 <- mysam1[, c(36,53,58, 63, 90, 120, 146)]
mysam2
summary(mysam2)
predict(pca_dest1, 
        newdata=tail(dest, 10))

# cluster dendrogram
di <- dist(mysam1[,1:4], method="euclidean")
tree <- hclust(di, method="ward.D2")
mysam1$hcluster <- as.factor((cutree(tree, k=3)-2) %% 3 +1)
# that modulo business just makes the coming table look nicer
plot(tree, xlab="")
rect.hclust(tree, k=3, border="red")

## k-clustering with mysam1 from expedia
kmd.out=kmeans(mysam1,4, nstart=15)
kmd.out
plot(kmd.out$cluster,col=kmd.out$cluster,cex=2,pch=1,lwd=2)
points(kmd.out$cluster,col=kmd.out$chr, pch=19)
points(kmd.out$cluster,col=c(4,3,2,1)[kmd.out$cluster],pch=19)
plot(kmd.out$cluster,col=kmd.out$cluster,pch=19)
##k-clustering with all dest set
kmd1.out=kmeans(dest,4, nstart=15)
kmd1.out
plot(kmd1.out$cluster,col=kmd1.out$cluster,cex=2,pch=1,lwd=2)
points(kmd1.out$cluster,col=kmd1.out$chr, pch=19)
points(kmd1.out$cluster,col=c(4,3,2,1)[kmd1.out$cluster],pch=19)
plot(kmd1.out$cluster,col=kmd1.out$cluster,pch=19)

## hierirchical clustering
hc_mysam1.complete=hclust(dist(mysam1), method="complete")
plot(hc_mysam1.complete)
rect.hclust(hc_mysam1.complete, k=3, border="red")

hc_mysam1.single=hclust(dist(mysam1),method="single")
plot(hc_mysam1.single)
rect.hclust(hc_mysam1.single, k=4, border="green")  ## making border for  single

hc_mysam1.average=hclust(dist(mysam1),method="average")
plot(hc_mysam1.average)
rect.hclust(hc_mysam1.average, k=5, border="blue")

hc1.cut=cutree(hc_mysam1.complete,4)
table(hc1.cut,kmd.out$cluster)
table(hc1.cut,kmd.out$cluster)
plot(hc_mysam1.complete,labels=kmd.out$chr)

clus_h <- dist(h[,1:10], method="euclidean")
tree_h <- hclust(clus_h, method="ward.D2")
h$id <- as.factor((cutree(tree_h, k=3)-2) %% 3 +1)
# that modulo business just makes the coming table look nicer
plot(tree_h, xlab="")
rect.hclust(tree_h, k=3, border="red")
