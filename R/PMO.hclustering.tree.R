"PMO.hclustering.tree" <-
function(M, dim,  hmethod="average", n=50, scale=TRUE, seed=100, distance="euclidean") {
	tr <- list();
	set.seed(seed);
  for (i in 1:n) {
	  P.M<- Achlioptas.random.projection(d=dim, M, scaling=scale);
		if (distance == "euclidean")
		  d <- dist (t(P.M))
		else if (distance == "pearson")
			d <- as.dist(1 - cor(P.M))
	  else
	    stop("distance measure not implemented");
	  tr[i] <- list(hclust(d, method = hmethod));
	}
  tr
}
