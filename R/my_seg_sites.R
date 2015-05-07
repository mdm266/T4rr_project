#' Segregating sites
#' 
#' Locate segregating sites 
#' @description A function to locate the segregating sites in a data frame of complete DNA sequecne
#' 
#'  @param my_seqs A dataframe of genomic sequence information, where each row is an individual sequence (haploid). 
#'  
#' @details A vector of the position of segregating sites 
#'  
#'  
#'  


my_seg_sites<-function(my_seqs){
	len<-dim(my_seqs)[1]
	sites<-vector()
	for (i in 2:len){
		sites<- c(sites,which(my_seqs[i,] != my_seqs[1,]))
	}
	return(unique(sort(sites)))
}