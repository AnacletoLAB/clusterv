\name{Norm.hclustering}
\alias{Norm.hclustering}
\alias{Norm.hclustering.tree}


\title{ Multiple Hierarchical clusterings using Normal random projections }
\description{
 Multiple Hierarchical clusterings using Normal random projections of the data.
}
\usage{
Norm.hclustering(M, dim, c = 3, hmethod = "average", n = 50, 
                 scale = TRUE, seed = 100, distance="euclidean")

Norm.hclustering.tree(M, dim, hmethod = "average", n = 50, scale = TRUE, 
                      seed = 100, distance = "euclidean")
}

\arguments{
   \item{M}{ matrix of data: rows are variables and columns are examples }
 \item{dim}{ subspace dimension }
 \item{c}{ number of clusters }
 \item{hmethod}{ the agglomeration method to be used. This should be one of 
        "ward.D", "single", "complete", "average", "mcquitty", "median" or "centroid", 
        according to the \code{hclust}
        method of the package \code{\link{stats}}. }
 \item{n}{ number of  random projections }
 \item{scale}{  if TRUE (default) Normal random projections are scaled }
 \item{seed}{ numerical seed for the random generator }
 \item{distance}{ it must be one of the two: "euclidean" (default) or "pearson" (that is 1 - Pearson correlation) }
}

\value{
  a list with components "cluster" and "tree":
  \item{cluster }{list of the n clusterings obtained. Each element is in turn a list of vectors that correspond 
	to the clusters of the clustering. Each cluster is represented by a vector of integers whose values corresponds to 
	the indices of the columns (examples) of the original data. }
  \item{tree }{list of the trees generated by the multiple clusterings}
	
	\code{Norm.hclustering.tree} returns only the list of the trees.
}

\author{ Giorgio Valentini \email{valentini@di.unimi.it}}


\seealso{ \code{\link{norm.random.projection}}}
\examples{
\donttest{
# 20 hierarchical clusterings on multiple Normal projected data 
# with subspace dimension equal to 100
M <- generate.sample0(n=10, m=2, sigma=1, dim=800)
l <- Norm.hclustering(M, dim=100, hmethod = "average", n = 20, scale = TRUE)
# Equal as above, but only the trees are generated
l <- Norm.hclustering.tree(M, dim=100, hmethod = "average", n = 20, scale = TRUE)
# 10 hierarchical clusterings on multiple Normal projected data 
# with subspace dimension equal to 200
M <- generate.sample0(n=8, m=1, sigma=2, dim=1000)
l <- Norm.hclustering(M, dim=200, hmethod = "average", n = 10, scale = TRUE)
}
}
\keyword{ cluster }
