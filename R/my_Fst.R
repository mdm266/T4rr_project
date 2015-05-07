#' calculate pairwise fst 
#' Weir and Cockerham 1984
#' give number of populations, number of individuals in each pop., 
#' and major allele frequency
#' 
#' 
#' @param r number of populations
#' @param n number of individuals in each population
#' @param p major allele freq in each population 
#' 
#' @return Pairwise Fst
#' 
#' @references Weir and Cockerham 1984


my_Fst<- function(r,n,p){
	n_bar<-sum(n/r) # average sample size 
avg_p<- sum((n*p)/(r*n_bar)) # average allele freq of major allele
S2<- sum((n*((p-avg_p)^2))/((r-1)*n_bar))
Fst<- S2/((avg_p)*(1-avg_p))
return (Fst)
}
