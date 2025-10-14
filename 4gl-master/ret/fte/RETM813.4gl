DATABASE safre_af

FUNCTION RETM813(cla_where)
#r813----------------------
DEFINE f_nss        CHAR(021)
DEFINE enter        CHAR(001)
DEFINE cla_where    CHAR(600) ,
       txt_813      CHAR(1000)

DEFINE hoy          DATE 

DEFINE lastkey      ,
       band         ,
       i            ,
       arr_c        ,
       total_pa     ,
       linea        SMALLINT

DEFINE reg_813 RECORD 
       x                  CHAR(001)                          ,
       nss                LIKE ret_extemporanea.nss          ,
       fecha_envio        LIKE ret_extemporanea.fecha_envio  ,
       tipo_retiro        LIKE ret_transf_rx.tipo_retiro     ,
       fecha_ini_pen      LIKE ret_transf_rx.fecha_ini_pen   ,
       diag_registro      LIKE ret_transf_rx.diag_registro   ,
       fecha_valor_trans  LIKE ret_cza_lote.fecha_valor_trans,
       fecha_conversion   LIKE ret_extemporanea.fecha_conversion ,
       descripcion        LIKE ret_estado.descripcion
END RECORD

DEFINE reg_813_1 RECORD
       sec_pension        LIKE ret_transf_rx.sec_pension     , 
       periodo_pago       LIKE ret_extemporanea.periodo_pago ,
       tipo_seguro        LIKE ret_transf_rx.tipo_seguro     ,
       tipo_pension       LIKE ret_transf_rx.tipo_pension 
END RECORD

DEFINE a_reg_813_1 ARRAY[1000] OF RECORD
       sec_pension        LIKE ret_transf_rx.sec_pension     , 
       periodo_pago       LIKE ret_extemporanea.periodo_pago ,
       tipo_seguro        LIKE ret_transf_rx.tipo_seguro     ,
       tipo_pension       LIKE ret_transf_rx.tipo_pension 
END RECORD

DEFINE a_reg_813 ARRAY[1000] OF RECORD 
       x                  CHAR(001)                          ,
       nss                LIKE ret_extemporanea.nss          ,
       fecha_envio        LIKE ret_extemporanea.fecha_envio  ,
       tipo_retiro        LIKE ret_transf_rx.tipo_retiro     ,
       fecha_ini_pen      LIKE ret_transf_rx.fecha_ini_pen   ,
       diag_registro      LIKE ret_transf_rx.diag_registro   ,
       fecha_valor_trans  LIKE ret_cza_lote.fecha_valor_trans,
       fecha_conversion   LIKE ret_extemporanea.fecha_conversion ,
       descripcion        LIKE ret_estado.descripcion
END RECORD




LET hoy = TODAY

OPEN WINDOW retm8131 AT 2,2 WITH FORM "RETM8131" ATTRIBUTE(BORDER) 
DISPLAY "                           <Ctrl-c> Salir                                      " AT 1,1 ATTRIBUTE(REVERSE)
DISPLAY " RETM813            TRABAJADORES CON APORTACION EXTEMPORANEA                   " AT 3,1 ATTRIBUTE(REVERSE)
DISPLAY hoy USING"DD-MM-YY" AT 3,70 ATTRIBUTE(REVERSE)

CALL dibuja_pantalla(5,13,15)
CALL dibuja_pantalla(5,13,26)
CALL dibuja_pantalla(5,13,29)
CALL dibuja_pantalla(5,13,40)
CALL dibuja_pantalla(5,13,45)
CALL dibuja_pantalla(5,13,56)
CALL dibuja_pantalla(5,13,67)
CALL dibuja_pantalla(16,18,27)
CALL dibuja_pantalla(16,18,39)
CALL dibuja_pantalla(16,18,48)

LET txt_813 =
' SELECT " "                , ',
     ' a.nss                , ',
     ' a.fecha_envio        , ',
     ' b.tipo_retiro        , ',
     ' b.fecha_ini_pen      , ',
     ' b.diag_registro      , ',
     ' c.fecha_valor_trans  , ',
     ' a.fecha_conversion   , ',
     ' d.descripcion        , ',
     ' b.sec_pension        , ',
     ' a.periodo_pago       , ',
     ' b.tipo_seguro        , ',
     ' b.tipo_pension ',
' FROM   ret_extemporanea a   , ',
     ' ret_transf_rx    b   , ',
     ' ret_cza_lote     c   , ',
     ' ret_estado       d  ',
' WHERE  ',cla_where CLIPPED ,
' and    a.nss = b.nss ',
' AND    a.consecutivo_his = b.consecutivo ',
' AND    b.folio           = c.folio    ',
' AND    a.estado_registro = d.estado_solicitud '

PREPARE qry_813 FROM txt_813

DECLARE cur_813 CURSOR FOR qry_813

LET i = 1

FOREACH cur_813 INTO reg_813.*,reg_813_1.*

        LET a_reg_813[i].*   = reg_813.*
        LET a_reg_813_1[i].* = reg_813_1.*
        LET i                = i + 1
      

END FOREACH

IF i = 1 THEN
   DISPLAY "No Se Encontraron Registros ..." AT 20,2 
   SLEEP 2
END IF



CALL SET_COUNT(i-1)
 
LET int_flag = FALSE

INPUT ARRAY a_reg_813 WITHOUT DEFAULTS FROM scr_813.*
BEFORE ROW
    LET arr_c    = ARR_CURR()
    LET total_pa = ARR_COUNT()
    LET linea    = SCR_LINE()
   
    IF arr_c > total_pa THEN
       LET band= TRUE
       EXIT INPUT 
    END IF

    DISPLAY a_reg_813[arr_c].x 
    TO scr_813[linea].x

    DISPLAY a_reg_813[arr_c].nss
    TO scr_813[linea].nss ATTRIBUTE(REVERSE) 

    DISPLAY a_reg_813[arr_c].fecha_envio
    TO scr_813[linea].fecha_envio ATTRIBUTE(REVERSE) 

    DISPLAY a_reg_813[arr_c].tipo_retiro
    TO scr_813[linea].tipo_retiro ATTRIBUTE(REVERSE) 

    DISPLAY a_reg_813[arr_c].fecha_ini_pen
    TO scr_813[linea].fecha_ini_pen ATTRIBUTE(REVERSE) 

    DISPLAY a_reg_813[arr_c].diag_registro
    TO scr_813[linea].diag_registro ATTRIBUTE(REVERSE) 

    DISPLAY a_reg_813[arr_c].fecha_valor_trans
    TO scr_813[linea].fecha_valor_trans ATTRIBUTE(REVERSE) 

    DISPLAY a_reg_813[arr_c].fecha_conversion
    TO scr_813[linea].fecha_conversion ATTRIBUTE(REVERSE) 

    DISPLAY a_reg_813[arr_c].descripcion
    TO scr_813[linea].descripcion ATTRIBUTE(REVERSE) 

    DISPLAY a_reg_813_1[arr_c].* TO scr_813_1[1].*

AFTER FIELD x 
   
    IF a_reg_813[arr_c].x IS NOT NULL THEN 
       LET a_reg_813[arr_c].x     = NULL 
       DISPLAY BY NAME a_reg_813[arr_c].x
    END IF

    IF arr_c >= (total_pa) THEN
       LET lastkey = FGL_LASTKEY()
         IF ((lastkey = FGL_KEYVAL("down"))
          OR (lastkey = FGL_KEYVAL("return"))
          OR (lastkey = FGL_KEYVAL("tab"))
          OR (lastkey = FGL_KEYVAL("right")))
         THEN 
           ERROR"No hay más opciones en esa dirección."
           NEXT FIELD x
         END IF
    END IF

AFTER ROW
    LET arr_c = ARR_CURR()
    LET linea = SCR_LINE()

    DISPLAY a_reg_813[arr_c].*
    TO      scr_813[linea].*

ON KEY (RETURN)

   CALL RETM814(a_reg_813[arr_c].nss)

ON KEY (INTERRUPT)
   EXIT INPUT

END INPUT

CLOSE WINDOW retm8131

END FUNCTION

