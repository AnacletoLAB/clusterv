"Random.PAM.validity" <-
function(M, dim, pmethod="PMO", c=3, n=50, 
                                         scale=TRUE, seed=-1, AC=TRUE, distance="euclidean") {
  dim.Sim.M <- ncol(M);
	if (seed == -1)
	   seed <- round(runif(1,1,10000));
	# Computing the clusters in the original space	
	if (distance == "euclidean")
		d <- dist (t(M))
	else 
		d <- as.dist(1 - cor(M));
	cl.v <- pam (d,c,cluster.only=TRUE);	
	cl.orig <- Transform.vector.to.list(cl.v);
	
	
	# Perform multiple clusterings using randomized embeddings
	cl <- Multiple.Random.PAM (M=M, dim=dim, pmethod=pmethod, c=c, n=n, 
	                                    scale=scale, seed=seed, distance=distance);
	
	# Update similarity matrix
	Sim.M <- Do.similarity.matrix(cl, dim.Sim.M);
	
	# Computing the list of validity measures
	# Computing the validity indices vi
	c <- length(cl.orig);
	vi <- Validity.indices(cl.orig, c, Sim.M);
	
	# Computing overall (average) validity of the clustering:
	ov.vi <- sum(vi)/c;	
	
	# Computing the AC indices:
	if (AC == TRUE) {
	  ac <- AC.index(cl.orig, c, Sim.M);		
		res <- list (validity=vi, overall.validity=ov.vi, similarity.matrix=Sim.M, dimension=dim, 
	             cluster=cl, orig.cluster=cl.orig, AC=ac);
	}														 
	else															 
	  res <- list (validity=vi, overall.validity=ov.vi, similarity.matrix=Sim.M, dimension=dim, 
	              cluster=cl, orig.cluster=cl.orig);
	return(res);	
}
