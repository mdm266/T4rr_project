context("my_pi")

test_that("mypopgen_pi works", {

	seqs<- data.frame(rbind(c("A","G","T","C"),c("A","G","T","T")))
	seqs2<- data.frame(rbind(c("A","G","T","C"),c("A","G","C","T")))
	seqs3<- data.frame(rbind(c("A","G","T","C"),c("A","G","T","C")))
  expect_equal( my_pi(seqs), 0.25)
	expect_equal( my_pi(seqs2), 0.5)
	expect_equal( my_pi(seqs3), 0) 
})