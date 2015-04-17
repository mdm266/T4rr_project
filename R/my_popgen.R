# Matthew Murray 
# mmurray7@wisc.edu
# updated 2015-4-10
# creating R package for Tools4RR class BMI 826 
# my_popgen functions is a group of popgen funcitons written 
# to easily integrate into my workflow  


# find segregating sites
my_seg_sites<-function(my_seqs){
	len<-dim(my_seqs)[1]
	sites<-vector()
	for (i in 2:len){
		sites<- c(sites,which(my_seqs[i,] != my_seqs[1,]))
	}
	return(unique(sort(sites)))
}

# calculate nucleotide diversity 
# all combinations method 
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
