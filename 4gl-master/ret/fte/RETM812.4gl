DATABASE safre_af
GLOBALS

DEFINE hoy DATE

DEFINE enter             CHAR(001),
       cla_where         CHAR(600)
DEFINE v_estado_registro SMALLINT
DEFINE v_descripcion     CHAR(050)

END GLOBALS

MAIN 
    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST

LET hoy = TODAY
OPEN WINDOW retm8121 AT 2,2 WITH FORM "RETM8121" ATTRIBUTE(BORDER) 
DISPLAY "                           <Ctrl-c> Salir                                      " AT 1,1 ATTRIBUTE(REVERSE)
DISPLAY " RETM812            TRABAJADORES CON APORTACION EXTEMPORANEA                   " AT 3,1 ATTRIBUTE(REVERSE)
DISPLAY hoy USING"DD-MM-YY" AT 3,70 ATTRIBUTE(REVERSE)


WHILE TRUE
CONSTRUCT BY NAME cla_where ON a.fecha_conversion,
                               a.fecha_envio     ,
                               a.estado_registro ,
                               a.nss

AFTER FIELD estado_registro

      CALL despliega_estado() 
      RETURNING v_estado_registro, v_descripcion

      DISPLAY v_estado_registro TO estado_registro
      DISPLAY v_descripcion     TO descripcion

ON KEY(ESC)
   LET v_descripcion = " "
   DISPLAY v_descripcion TO descripcion

   LET int_flag = FALSE
   EXIT CONSTRUCT

ON KEY(INTERRUPT)
   LET int_flag = TRUE
   CLEAR FORM 
   EXIT CONSTRUCT
END CONSTRUCT


IF int_flag = TRUE THEN
   LET int_flag = FALSE
   DISPLAY "Saliendo del Proceso..." AT 20,2 ATTRIBUTE(REVERSE)
   SLEEP 1
   EXIT PROGRAM 
ELSE
   CALL RETM813(cla_where)
END IF
END WHILE
END MAIN

############################################################################
FUNCTION dibuja_pantalla(ini_vert,fin_vert,pos_hor)
#dp------------------------------------------------

DEFINE i SMALLINT
DEFINE ini_vert     ,
       fin_vert     ,
       pos_hor      SMALLINT

FOR i = ini_vert TO fin_vert
    DISPLAY "|" AT i,pos_hor
END FOR

END FUNCTION

FUNCTION despliega_estado()
-----------------------------
     DEFINE v_null         SMALLINT
     DEFINE verif          SMALLINT
     DEFINE reg_ret_estado RECORD LIKE ret_estado.*

     DEFINE arr1_ret_estado ARRAY[30] OF RECORD LIKE ret_estado.*

     DEFINE arr_c                ,
            i             SMALLINT

OPTIONS 
   MESSAGE LINE LAST
     OPEN WINDOW retm8122 AT 9,28 WITH FORM "RETM8122" ATTRIBUTE(BORDER)

     DISPLAY "         ESTADO                   " AT 1,1   ATTRIBUTE(REVERSE)
     MESSAGE "<ENTER> Elegir Estado...        " ATTRIBUTE(REVERSE)


     LET i     = 1
     LET verif = 0
     INITIALIZE v_null TO NULL

     DECLARE cur_dsply_estado CURSOR FOR 

     SELECT a.* 
     FROM   ret_estado       a 
     WHERE  a.estado_solicitud IN(0,4)

     FOREACH cur_dsply_estado INTO reg_ret_estado.*

          IF reg_ret_estado.estado_solicitud = 0 THEN 
              LET reg_ret_estado.descripcion = "IDENTIFICADO"
          END IF 
               
          LET  arr1_ret_estado[i].* = reg_ret_estado.*

          LET i = i + 1

     END FOREACH

     CALL SET_COUNT(i-1)

     DISPLAY ARRAY arr1_ret_estado
                TO scr_ret_estado.*

     ON KEY(RETURN) 
        LET arr_c = ARR_CURR()
        EXIT DISPLAY

     ON KEY(INTERRUPT)
        LET verif = 1
        EXIT DISPLAY

     END DISPLAY
     CLOSE WINDOW retm8122

  IF verif = 1 THEN
     RETURN v_null,v_null

  ELSE

   RETURN arr1_ret_estado[arr_c].estado_solicitud ,
          arr1_ret_estado[arr_c].descripcion
  END IF

END FUNCTION
