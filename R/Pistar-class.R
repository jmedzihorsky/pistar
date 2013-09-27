#	Definitions of S4 class Pistar
#	Juraj Medzihorsky
#	28 August 2013


#	Hierachy:
#	---------
#	Pistar
#		PistarCT
#			Pistar2by2
#			PistarRCL
#				PistarLL
#		PistarUV
#			PistarDUV
#			PistarCUV
#		PistarBVN
#		PistarMVN



#	--------------
#	class `Pistar`
#	--------------


#	definition

setClass('Pistar',
		 representation(
						call 	= 'language',
						pistar 	= 'list',
						pred 	= 'list',
						param	= 'list'
						)
		 )


#	constructor

Pistar <- 
	function(call, 
			 pistar, 
			 pred,
			 param)
	{
		new('Pistar',
			call	= call,
			pistar	= pistar,
			pred	= pred,
			param	= param)
	}



#	--------------------
#	-	class `PistarCT`	
#	--------------------


#	definition

setClass('PistarCT',
		 contains = 'Pistar',
		 representation(
						data 	= 'array'												
						)
		 )


#	constructor

PistarCT <-
	function(call, 
			 pistar, 
			 pred,
			 data, 			 
			 param)
	{
		new('PistarCT',
			call	=	call,
			pistar	=	pistar,
			pred	=	pred,
			data	=	data,			
			param	=	param)
	}



#	--------------------------
#	-	-	class `Pistar2by2`
#	--------------------------


#	definition

setClass('Pistar2by2',
		 contains = 'PistarCT')


#	constructor

Pistar2by2 <-
	function(call, 
			 pistar, 
			 pred,
			 data, 			 
			 param)
	{
		new('Pistar2by2',
			call	=	call,
			pistar	=	pistar,
			pred	=	pred,
			data	=	data,						
			param	=	param)
	}





#	-------------------------
#	-	-	class `PistarRCL`
#	-------------------------


#	definition

setClass('PistarRCL',
		 contains = 'PistarCT',
		 representation(
						llrs = 'list',
						iter = 'list'
						)
		 )


#	constructor

PistarRCL <-
	function(call, 
			 pistar, 
			 pred,
			 data, 			 
			 param,
			 llrs,
			 iter)
	{
		new('PistarRCL',
			call	=	call,
			pistar	=	pistar,
			pred	=	pred,
			data	=	data,			
			param	=	param,
			llrs	=	llrs,
			iter	=	iter)
	}




#	----------------------------
#	-	-	-	class `PistarLL`
#	----------------------------


#	definition

setClass('PistarLL',
		 contains = 'PistarRCL'
		 )


#	constructor

PistarLL <-
	function(call, 
			 pistar, 
			 pred,
			 data, 			 
			 param,
			 llrs,
			 iter)
	{
		new('PistarLL',
			call	=	call,
			pistar	=	pistar,
			pred	=	pred,
			data	=	data,			
			param	=	param,
			llrs	=	llrs,
			iter	=	iter)
	}



#	--------------------
#	-	class `PistarUV`
#	--------------------


#	definition

setClass('PistarUV',
		 contains = 'Pistar',
		 representation(
						meth 	= 'character',
						conv 	= 'list',
						mess 	= 'list'
						)
		 )


#	constructor

PistarUV <-
	function(call, 
			 pistar, 
			 pred,
			 param,
			 meth,
			 conv,
			 mess)
	{
		new('PistarUV',
			call	=	call,
			pistar	=	pistar,
			pred	=	pred,
			param	=	param,
			meth	=	meth,
			conv	=	conv,
			mess	=	mess
		)
	}	



#	-------------------------
#	-	-	class `PistarDUV`
#	-------------------------


#	definition

setClass('PistarDUV',
		 contains = 'PistarUV',
		 representation(
						data = 'table'
						)
		 )


#	constructor

PistarDUV <-
	function(call, 
			 pistar, 
			 pred,
			 data, 			 
			 param,
			 meth,
			 conv,
			 mess)
	{
		new('PistarDUV',
			call	=	call,
			pistar	=	pistar,
			pred	=	pred,
			data	=	data,			
			param	=	param,
			meth	=	meth,
			conv	=	conv,
			mess	=	mess
		)
	}	
	


#	-------------------------
#	-	-	class `PistarCUV`
#	-------------------------


#	definition

setClass('PistarCUV',
		 contains = 'PistarUV',		
		 representation(
						data = 'numeric'
						)
		 )


PistarCUV <-
	function(call, 
			 pistar, 
			 pred,
			 data, 			 
			 param,
			 meth,
			 conv,
			 mess)
	{
		new('PistarCUV',
			call	=	call,
			pistar	=	pistar,
			pred	=	pred,
			data	=	data,			
			param	=	param,
			meth	=	meth,
			conv	=	conv,
			mess	=	mess
		)
	}	


#	-----------------
#	class `PistarBVN`
#	-----------------


#	definition

setClass('PistarBVN',
		 contains = 'Pistar',		
		 representation(
						data 		= 'data.frame',
						interval 	= 'numeric',
						conf 		= 'numeric',
						alt 		= 'character'
						)
		 )


#	constructor

PistarBVN <-
	function(call, 
			 pistar, 
			 pred,
			 data, 
			 param, 
			 interval, 
			 conf, 
			 alt)
	{
		new('PistarBVN',
			call	=	call,
			pistar	=	pistar,
			pred	=	pred,
			data	=	data, 
			param	=	param,			
			interval=	interval,
			conf	=	conf,
			alt		=	alt
		)
	}	



#	-----------------
#	class `PistarMVN`
#	-----------------


#	definition

setClass('PistarMVN',
		 contains = 'Pistar',		
		 representation(
						data 	= 'data.frame',
						trace	= 'list',
						iter	= 'list',
						test	= 'list'
						)
		 )


#	constructor

PistarMVN <-
	function(call, 
			 pistar, 
			 pred,
			 data, 
			 param, 
			 trace, 
			 iter, 
			 test
	)
	{
		new('PistarMVN',
			call	= call,
			pistar	= pistar,
			pred	= pred,
			data	= data, 
			param	= param,
			trace	= trace,
			iter	= iter,
			test	= test
		)
	}	
