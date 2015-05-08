#' Nucleotide diversity
#'
#' Calculate pi from all combinations
#'
#' @param my_seqs A dataframe of genomic sequence information, where each row is an individual sequence (haploid).
#'
#' @details Pairwise nucleotide diversity for whole sequence DNA
#'
#'
#' @examples
#' bases<- c("A","T","C","G")
#' seq1 <- sample(bases,1000, replace=TRUE)
#' seqs<- as.data.frame(matrix(NA,nrow=9,ncol=1000))
#' seqs[1,]<-seq1
#' for ( i in 2:10){
#' snps<- sample(bases,3, replace=TRUE)
#' snp_pos<-sample(seq(1,1000),3)
#' seq2<- seq1
#' seq2[snp_pos]<-snps
#' seqs[i,]<-seq2}
#' my_pi(seqs)
my_pi<- function(my_seqs){
    n=dim(my_seqs)[1]
    all_combs=as.data.frame(combn(seq(1,n),2))
    pis<-vector()
    for (i in 1:length(all_combs)){
        pis<- c(pis,
        sum(my_seqs[all_combs[1,i],] !=my_seqs[all_combs[2,i],])/
            length(my_seqs[all_combs[i,2],]))
    }
    return(sum(pis))
}
