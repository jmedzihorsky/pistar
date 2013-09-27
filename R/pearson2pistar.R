pearson2pistar <-
function(coeff)
{
	res <- as.vector(coeff)
	out <-  1 - ( (1-abs(res)) / (1+abs(res)) )^(1/2)
	return(out)
}
