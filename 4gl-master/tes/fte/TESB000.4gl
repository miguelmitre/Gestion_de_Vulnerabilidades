DATABASE safre_af 

MAIN 
DEFINE l_hoy DATE
DEFINE l_hora CHAR(008) 
DEFINE l_lanza char(400)
DEFINE l_reg_seg_modulo RECORD LIKE seg_modulo.*


SELECT a.* 
INTO l_reg_seg_modulo.*
FROM  seg_modulo a
WHERE a.modulo_cod = "taa"

LET l_hoy  = TODAY
LET l_hora = TIME

LET l_lanza = "nohup fglgo ",l_reg_seg_modulo.ruta_exp CLIPPED,"/",
              "TESB001 1>",l_reg_seg_modulo.ruta_listados CLIPPED,
              "/transf_",l_hoy USING"YYYYMMDD"," 2>&1 &"
RUN l_lanza


END MAIN
