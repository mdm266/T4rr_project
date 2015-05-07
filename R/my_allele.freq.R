#' Major allele freq calculation
#' 
#' @description Calculates the major allele frequence from a set of Nucleotide bases for a given site.  
#' 
#' @details Major allele frequency
#' 
#' @param my_seqs A vector of bases
my_allele_freq<-function(my_seqs){
	sites=my_seg_sites(my_seqs)
	allele.freq=rep(NA,length(sites))
	for (i in 1:length(sites)){
		freq=max(table(my_seqs[,sites[1]]))/ sum(table(my_seqs[,sites[i]]))
		allele.freq[i]<-freq
	}
	return(allele.freq)
}