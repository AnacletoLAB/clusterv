"Min.Expansion" <-
function(m, m.rid) {
  d <- dist(m); # it computes the euclidean distance between all pairs of rows of m
  d.rid <- dist(m.rid);
  min.exps <- min(d.rid/d);
  min.exps
}
