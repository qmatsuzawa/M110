



forvalues x=1999/2002{
clear

infile using "E:\NCHS\earlyDict.dct", using("E:\NCHS\MULT`x'.USAllCnty.txt")

save "E:\NCHS\death\mortality_`x'", replace 

}

/*
*MULT1999.AllCnty

forvalues x=2003/2008 {
clear

infile using "E:\NCHS\NBERDict.dct", using("E:\NCHS\MULT`x'.USAllCnty.txt")

save "E:\NCHS\death\mortality_`x'", replace 

}
