context("my_seg_sites")

test_that("mypopgen_seg_sites works", {

	seqs<- data.frame(rbind(c("A","G","T","C"),c("A","G","T","T")))
	seqs2<- data.frame(rbind(c("A","G","T","C"),c("A","G","C","T")))
	seqs3<- data.frame(rbind(c("A","G","T","C"),c("A","G","T","C")))
  expect_equal( my_seg_sites(seqs), 4)
	expect_equal( my_seg_sites(seqs2), c(3, 4))
	expect_equal( my_seg_sites(seqs3), integer(0))
})