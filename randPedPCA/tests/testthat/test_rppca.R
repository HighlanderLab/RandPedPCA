



# test

# Lin not centred

test_that("rppca on Linv", {
  expect_no_condition(
    pc <- rppca(pedLInv)
  )

  expect_warning(summary(pc))
  expect_false(pc$center)
  expect_false(pc$scale)
  expect_null(pc$varProps)

})

# Lin not centred

test_that("rppca on Linv with totVar", {
  expect_no_condition(
    pc <- rppca(pedLInv, totVar = 3521.534)
  )

  expect_no_condition(summary(pc))
  expect_true(dim(summary(pc)$importance)[1] == 3) # three rows
  expect_false(pc$center)
  expect_false(pc$scale)
  expect_true(!is.null(pc$varProps))

})

# Linv  centred
test_that("rppca on Linv centred", {
  expect_no_condition(
    pc <- rppca(pedLInv, center=T)
  )
  expect_warning(summary(pc))
  expect_true(pc$center)
  expect_false(pc$scale)
  expect_null(pc$varProps)

})

# Linv  centred
test_that("rppca on Linv centred with totVar", {
  expect_no_condition(
    pc <- rppca(pedLInv, center=T, totVar=2694.038)
  )
  expect_no_condition(summary(pc))
  expect_true(dim(summary(pc)$importance)[1] == 3) # three rows
  expect_true(pc$center)
  expect_false(pc$scale)
  expect_true(!is.null(pc$varProps))

})



test_that("rppca on pedigree", {
  ped <- pedigree(sire  = pedMeta$fid,
                  dam   = pedMeta$mid,
                  label = pedMeta$id)
  expect_no_error(pc2 <- rppca(ped))
  expect_no_condition(summary(pc2))
  expect_true(dim(summary(pc2)$importance)[1] == 3) # three rows
  expect_false(pc2$center)
  expect_false(pc2$scale)
  expect_true(!is.null(pc2$varProps))
})

test_that("rppca on pedigree with (redundant) totVar", {
  ped <- pedigree(sire  = pedMeta$fid,
                  dam   = pedMeta$mid,
                  label = pedMeta$id)
  expect_warning(pc2 <- rppca(ped, center=F, totVar=123))
  expect_true(dim(summary(pc2)$importance)[1] == 3) # three rows
  expect_no_condition(summary(pc2))
  expect_false(pc2$center)
  expect_false(pc2$scale)
  expect_true(!is.null(pc2$varProps))
})


test_that("rppca on pedigree, centred", {
  ped <- pedigree(sire  = pedMeta$fid,
                  dam   = pedMeta$mid,
                  label = pedMeta$id)
  expect_no_error(pc2 <- rppca(ped, center=T))
  expect_warning(summary(pc2)) # total variance unknown
  expect_true(pc2$center)
  expect_false(pc2$scale)
  expect_null(pc2$varProps)
})

test_that("rppca on pedigree, centered with totVar", {
  ped <- pedigree(sire  = pedMeta$fid,
                  dam   = pedMeta$mid,
                  label = pedMeta$id)
  expect_no_condition(pc2 <- rppca(ped, center=T, totVar=2694.038)) # this annoyingly throws a warning with an unhelpful suggestion
  expect_no_condition(summary(pc2))
  expect_true(dim(summary(pc2)$importance)[1] == 3) # three rows
  expect_true(pc2$center)
  expect_false(pc2$scale)
  expect_true(!is.null(pc2$varProps))
})


test_that("Comparing STD values between rppca on pedigree and L^-1 input", {
  expect_no_condition(
    pc <- rppca(pedLInv)
  )

  ped <- pedigree(sire  = pedMeta$fid,
                  dam   = pedMeta$mid,
                  label = pedMeta$id)
  expect_no_error(pc2 <- rppca(ped))
  expect_true(all(pc$sdev[1:2] - pc2$sdev[1:2] < 1e-10))
})


# variance estimates

test_that("Comparing Hutch++ estimate to inbreeding-based vals", {
  ped <- pedigree(sire  = pedMeta$fid,
                  dam   = pedMeta$mid,
                  label = pedMeta$id)
  tv <- sum(inbreeding(ped) + 1)
  expect_true(abs(log10(tv/hutchpp(pedLInv, num_queries=100))) < 0.02)
})


test_that("Comparing Hutch++ estimate to inbreeding-based vals (with centring)", {
  ped <- pedigree(sire  = pedMeta$fid,
                  dam   = pedMeta$mid,
                  label = pedMeta$id)
  ll <- getL(ped)
  llc <- apply(ll, 1, function(x) x - mean(x))
  a <- llc %*% t(llc)
  tv <- sum(diag(a))
  expect_true(abs(log10(tv/hutchpp(pedLInv, num_queries=100, center=T))) < 0.02)
})







# Subsampling -------------------------------------------------------------

test_that("Sub-sampling", {
  pc <- rppca(pedLInv)

  expect_error(dspc(1)) # error, input must inherit 'rppca'

  # default val of 'to' is 10k, greateer then number of individuals, no downsampling and no message
  expect_no_condition(pcd <- dspc(pc))

  expect_warning(pcd <- dspc(pcd)) # warning about overwriting existing index
  expect_message(dspc(pc, c(T, F))) # message about to which number of individuals we downsampled
  expect_message(dspc(pc, c(1,3))) # same here

  # though 100000 is greater than the number of individuals in this PCA
  # there is no error/warning. This is handled by R. To high index results in NA.
  expect_message(dspc(pc, c(1,3, 100000)))
})


# Plotting ----------------------------------------------------------------
test_that("Plotting",{
  pc <- rppca(pedLInv, center=T, totVar=2694.038)
  # length of 'col' is different from number of individuals in the PCA
  expect_warning(plot(pc, col=c("yellow", "green"), to = 0.5))
})
