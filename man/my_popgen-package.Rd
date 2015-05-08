\name{mypopgen-package}
\alias{mypopgen-package}
\alias{mypopgen}
\docType{package}
\title{
Popgen R Functions
}
\description{
Personal iterations of commonly used popgen stats that fit into my analysis pipeline
}
\details{
\tabular{ll}{
Package: \tab my_popgen\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-04-10\cr
License: \tab GPL-3\cr
}
These functions take sequence information in the form of a data frame in R. Each row is a separate individual. Currently no missing data, and single base/sites calls. 

Can find segregating sites, and pi of all individuals.
}
\author{
Matthew Murray <mmurray7@wisc.edu>
}
\examples{
# test data 
# create example sequence 
# this creates 1kb of sequences and I added some random snps 
# this is not real data
bases<- c("A","T","C","G")
seq1 <- sample(bases,1000, replace=TRUE)
seqs<- as.data.frame(matrix(NA,nrow=9,ncol=1000))
seqs[1,]<-seq1
for ( i in 2:10){
snps<- sample(bases,3, replace=TRUE)
snp_pos<-sample(seq(1,1000),3)
seq2<- seq1
seq2[snp_pos]<-snps
seqs[i,]<-seq2
}

my_seg_sites(seqs)
my_pi(seqs)
}
