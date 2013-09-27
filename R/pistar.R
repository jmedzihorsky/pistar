#	`pistar` function
#	Juraj Medzihorsky
#	07 Septmeber 2013

pistar <-
	function(proc, 
			 ... )
{
	metacall <- match.call()

	if (proc=='uv') {
		out <- pistar.uv( ... )
	} else if (proc=='2by2') {
		out <- pistar.2by2( ... )	
	} else if (proc=='ct') {
		out <- pistar.ct( ... )
	} else if (proc=='ll') {
		out <- pistar.ll( ... )
	} else if (proc=='mvn') {
		out <- pistar.mvn( ... )
	} else {
		stop('unsuported proc')
	}


	out@call <- metacall

	return(out)
}
