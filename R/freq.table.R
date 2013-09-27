freq.table <-
function(x)
{
	x <- as.integer(x)
	return(table(factor(x, levels=min(x):max(x))))
}
