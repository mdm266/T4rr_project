\name{my_allele_freq}
\alias{my_allele_freq}
\title{
Allele Frequency 
}
\description{
Calculates the major allele frequence from a set of Nucleotide bases for a given site.  
}
\usage{
my_allele_freq(my_seqs)
}

\arguments{
  \item{my_seqs}{
A vector of bases
}
}

\value{
Major allele freq
}

\author{
Matthew Murray
}
\examples{
m1_p1<- data.frame(sample(c(rep("A",50),rep("C",50))))
my_allele_freq(m1_p1)
}
