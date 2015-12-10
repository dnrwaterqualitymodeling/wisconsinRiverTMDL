# Need to use 32-bit version of R for RODBC to work

dir_out="C:/Users/ruesca/Desktop/CNOP_adjust"

source("T:/Projects/Wisconsin_River/Code/calibration/adjust_cnop.r")

parameterization = rbind(
	## Western Coulees and Ridges
	c("r__CN2.mgt________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.120723,-0.120723),
	## Forest Transition
	c("r__CN2.mgt____A,B____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",-0.05,-0.05),
	c("r__CN2.mgt____C,D____62-67,72,200,201,307,313,68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.15,0.15),
	## Central Sands and Plains
	c("r__CN2.mgt____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0,0),
	c("r__CN2.mgt____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.151816,-0.151816),
	## Northern Highlands
	c("r__CN2.mgt________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.176367,-0.176367)
)

for (r in 1:nrow(parameterization)) {
	p = strsplit(parameterization[r,], "__")
	hydgrp = p[[1]][4]
	if (hydgrp == "") {
		hydgrp = LETTERS[1:4]
	} else {
		hydgrp = eval(parse(
			text=paste('c("', gsub(",", '","', hydgrp), '")', sep="")
		))
	}
	subbasins = p[[1]][6]
	subbasins = gsub("-", ":", subbasins)
	subbasins = eval(parse(
			text=paste("c(", subbasins, ')', sep="")
	))
	scalar = as.numeric(p[[2]][1])
	adjust_cnop(scalar=scalar,subbasins=subbasins,hydgrp=hydgrp,dir_out=dir_out)
}
