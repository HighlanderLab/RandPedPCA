li2 <- getLInv(pedigree(pedMeta2$fid, pedMeta2$mid, pedMeta2$id))
#li2 <- sparse2spam(getLInv(pedigree(pedMeta2$fid, pedMeta2$mid, pedMeta2$id)))

#li2 <- importLinv("~/Dropbox/Code/PedPCA/pedLInv.mtx")
#li2 <- Matrix::readMM("~/Dropbox/Code/PedPCA/pedLInv.mtx")
class(li2)


25/.6 * 16 / 60

t0 <- Sys.time()
pc0 <- rppca(li2)
Sys.time() - t0
plot(pc0)


# works for sparse non-spam
# hard to make package work for both
t0 <- Sys.time()
pc1 <- rppca(li2,  oracleFun=oraculumLiMatrixSolve)
Sys.time() - t0
plot(pc1)
class(li2)
t0 <- Sys.time()
cli2 <- Cholesky(li2)
Sys.time() - t0
class(cli2 )

t0 <- Sys.time()
pc2 <- rppca(cli2,  oracleFun=oraculumLiMatrixSolve)
Sys.time() - t0

?Cholesky

Sys.time(li2Sys.time() - t0
class(li2)
li2_csc <-as(li2, "ltRMatrix")   # Convert to compressed column (CSC) format

