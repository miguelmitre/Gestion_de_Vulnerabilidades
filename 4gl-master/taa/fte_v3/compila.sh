####     compila   Cedente    ####
fglpc   INTC_GLOB.4gl 
fglpc   GLOB_CFOLS.4gl
fglpc   GLOB_REP.4gl
fglpc   GLOB_REPS.4gl 

fglpc   GLOB_CON.4gl
cat     GLOB_CON.4go  GLOB_REP.4go  GLOB_REPS.4go    >  GLOB_CON.4gi

fglpc   TCAAC006.4gl
cat     INTC_GLOB.4go   TCAAC006.4go     >  TCAAC006.4gi 

fglpc   TCAAB005S.4gl   TCAAB005.4gl
cat     INTC_GLOB.4go   TCAAB005S.4go   TCAAB005.4go  >  TCAAB005.4gi 

fglpc   TCAAB006S.4gl   TCAAB006.4gl
cat     INTC_GLOB.4go   TCAAB006S.4go   TCAAB006.4go  >  TCAAB006.4gi 

fglpc   TCAAB006L.4gl
cat     INTC_GLOB.4go   TCAAB006S.4go   TCAAB006L.4go  >  TCAAB006L.4gi 

fglpc   TCAAB007.4gl
cat     INTC_GLOB.4go   GLOB_REP.4go GLOB_REPS.4go TCAAB007.4go > TCAAB007.4gi 

fglpc   TCAAC007.4gl
cat     INTC_GLOB.4go   TCAAC007.4go     >  TCAAC007.4gi 

fglpc   TCAAC008.4gl
cat     TCAAC008.4go     >  TCAAC008.4gi 

fglpc   TCAAL006.4gl
cat     TCAAL006.4go     >  TCAAL006.4gi 

fglpc   TCAAL007.4gl
cat     GLOB_CFOLS.4go   TCAAL007.4go     >  TCAAL007.4gi 

fglpc   TCAAL008.4gl
cat     GLOB_CFOLS.4go   TCAAL008.4go     >  TCAAL008.4gi 

fglpc   TCAAL010.4gl
cat     TCAAL010.4go     >  TCAAL010.4gi 

fglpc   TCAAB011.4gl
cat     TCAAB011.4go     >  TCAAB011.4gi 

fglpc   TCAAB012.4gl
cat     TCAAB012.4go     >  TCAAB012.4gi 

fglpc   TCAAB014.4gl
cat     TCAAB014.4go     >  TCAAB014.4gi 


form4gl   TCAAB0051.per
form4gl   TCAAB006.per 
form4gl   TCAAB0071.per
form4gl   TCAAB0111.per
form4gl   TCAAB0112.per
form4gl   TCAAB012.per
form4gl   TCAAB0121.per
form4gl   TCAAB012_1.per
form4gl   TCAAB0141.per
form4gl   TCAAB0142.per
form4gl   TCAAB0143.per 
form4gl   TCAAB0144.per
form4gl   TCAAB0145.per
form4gl   TCAAC0061.per 
form4gl   TCAAC007.per 
form4gl   TCAAC0081.per
form4gl   TCAAL0061.per
form4gl   TCAAL0072.per 
form4gl   TCAAL0081.per
form4gl   TCAAL0082.per
form4gl   TCAAL0101.per 
form4gl   TCAAL0104.per

chmod   777    *.4gi
mv             *.4gi    ../exp

chmod  777  *.frm
mv          *.frm    ../exp
rm          *.4go
