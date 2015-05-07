\name{my_seg_sites}
\alias{my_seg_sites}
\title{
Function to locate segregating sites 
}
\description{
Finds segregatating sites in complete DNA sequences.
}
\usage{
my_seg_sites(my_seqs)
}
\arguments{
  \item{my_seqs}{
A dataframe of genomic sequence information, where each row is an individual sequence (haploid). 
}
}
\details{
Locate Segregating sites.
}
\value{
A vector or the positon in the data frame that contains the segregating site. 
}
\references{
}
\author{
Matthew Murray
}
\note{
Only takes haploid (or fully inbred) sequence currently
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
my_seg_sites(seqs)
}
