# reformate data for MatrixEQTL
# author: Holger
reformate_for_matrixEQTL <- function(metab4_m_po, idvar = "pid") {
  print(hh(metab4_m_po))
  moveColFront(metab4_m_po, idvar)
  metab4_m_po = t(metab4_m_po)

  colnames(metab4_m_po) = metab4_m_po[idvar,]
  metab4_m_po = metab4_m_po[-1,]
  myrownames = rownames(metab4_m_po)
  metab4_m_po = apply(metab4_m_po, 2, function(x) as.numeric(x))
  metab4_m_po = data.table(id = myrownames, metab4_m_po)
  print(hh(metab4_m_po))
  metab4_m_po
}
