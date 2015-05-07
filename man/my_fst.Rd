\name{my_Fst}
\alias{my_Fst}
\title{
Funciton for  pairwise Fst 
}
\description{
A function to calculate pairwise Fst between two markers
}
\usage{
my_Fst(r,n,p)
}

\arguments{
  \item{r}{
Number of populations
}
  \item{n}{
A vector of the number of individuals in each population.
}
  \item{p}{
A vector of the major allele frequency in each population 
}
}
\details{
}
\value{
Pairwise Fst
}
\references{
Weir and Cockerham 1984
}
\author{
Matthew Murray
}
\examples{
m1_p1<- data.frame(sample(c(rep("A",50),rep("C",50))))
m1_p2<- data.frame(sample(c(rep("A",50),rep("C",50))))
freq_1_1= my_allele_freq(m1_p1)
freq_1_2= my_allele_freq(m1_p2)
my_Fst(2, c(50,50), c(freq_1_1,freq_1_2))
}
