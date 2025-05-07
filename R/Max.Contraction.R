"Max.Contraction" <-
function(m, m.rid) {
  d <- dist(m); # it computes the euclidean distance between all pairs of rows of m
  d.rid <- dist(m.rid);
  contr <- max(d/d.rid);
  contr
}
