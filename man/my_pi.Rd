\name{my_pi}
\alias{my_pi}
\title{
Function to calculate pi
}
\description{
A fucntion to calculate necleotide diversity(pi).
}
\usage{
my_pi(my_seqs)
}

\arguments{
  \item{my_seqs}{
A dataframe of genomic sequence information, where each row is an individual sequence (haploid). 
}
}
\details{
Pairwise nucleotide diversity for whole sequence DNA 
}
\value{
Nucleotide diversity for the sample of sequences given
}
\references{
}
\author{
Matthew Murray
}
\note{
Will do all possible pairs. will be computationally intense if long or many sequences are used. Also only takes haploid (or fully inbred) sequence currently
}
\seealso{
}
\examples{
bases<- c("A","T","C","G")
seq1 <- sample(bases,1000, replace=TRUE)
seqs<- as.data.frame(matrix(NA,nrow=9,ncol=1000))
seqs[1,]<-seq1
for ( i in 2:10){
snps<- sample(bases,3, replace=TRUE)
snp_pos<-sample(seq(1,1000),3)
seq2<- seq1
seq2[snp_pos]<-snps
seqs[i,]<-seq2}
my_pi(seqs)
}