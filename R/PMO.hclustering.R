"PMO.hclustering" <-
function(M, dim, c=3, hmethod="average", n=50, scale=TRUE, seed=100, distance="euclidean") {
  cl <- list();
	tr <- list();
	set.seed(seed);
  for (i in 1:n) {
	  P.M<- Plus.Minus.One.random.projection(d=dim, M, scaling=scale);
		if (distance == "euclidean")
		  d <- dist (t(P.M))
		else if (distance == "pearson")
			d <- as.dist(1 - cor(P.M))
	  else
	    stop("distance measure not implemented");
	  tr[i] <- list(hclust(d, method = hmethod));
		plot(tr[[i]], main="");
	  cl[i] <- list(rect.hclust(tr[[i]], k = c));
	}
  l <- list(cluster=cl, tree=tr);
  l
}
