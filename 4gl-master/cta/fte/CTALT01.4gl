--------------------------------------------------------------------------
-- Programa    : CTALT01 
-- Descripcion : PREVIO DE LIQUIDACION TRASPASO ENTRE SIEFORES I. X EDAD
-- Fecha       : 1 de SEP 2010
-- Autor       : Jesus Yanez Moreno
--------------------------------------------------------------------------

DATABASE safre_af
GLOBALS

   DEFINE g_resul             SMALLINT
  
   DEFINE lanzador            CHAR(1000),
          g_enter             CHAR(001)

   DEFINE g_today             , 
          fecha_valuacion     DATE

   DEFINE g_arr_siefore       ARRAY[5] OF  RECORD 
          razon_social        CHAR(15)  ,
          precio_del_dia      DEC(16,6)
   END RECORD 

   DEFINE g_seg_modulo        RECORD LIKE seg_modulo.* 

END GLOBALS

MAIN

  OPTIONS PROMPT LINE LAST,
          INPUT WRAP,
  ACCEPT KEY CONTROL-I
  DEFER INTERRUPT

    LET g_today = TODAY 

    SELECT a.*
    INTO   g_seg_modulo.*
    FROM   seg_modulo a
    WHERE  a.modulo_cod = 'cta'
    
    OPEN WINDOW ventana_1 AT 2,2 WITH 21 ROWS,78 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " CTALT01           PREVIO DE TRASPASO SIEFORES I. X EDAD                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY g_today USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

  MENU "PREVIO TRASPASO SIEFORE I. X EDAD"    
  COMMAND "Previo" "Ejecuta Previo de Traspaso Siefores I. x Edad"
       CALL previo()
  COMMAND "Consulta Previo" "Monitor de avance del Previo"
       CALL consulta_previo()
  COMMAND "Salir" "Salir del Programa"
       EXIT MENU
  END MENU

  CLOSE WINDOW ventana_1

END MAIN


FUNCTION ini_precio_sie()
#-------------------------

--- Se llena el arreglo con los precios
---------------------------------------

  DEFINE codigo_siefore  smallint  ,
         razon_social    char(15)  ,
         precio_del_dia  dec(16,6)

  DEFINE i, l_resul       smallint

  LET l_resul = 0

  FOR i = 1 TO 5
     INITIALIZE g_arr_siefore[i].* TO NULL
  END FOR

  LET i = 0

  DECLARE cur_1 CURSOR FOR 

  SELECT b.codigo_siefore ,
         a.razon_social, 
         b.precio_del_dia
  FROM   tab_siefore_local a, 
         glo_valor_accion  b
  WHERE  a.codigo_siefore  = b.codigo_siefore
  AND    b.codigo_siefore in (1,2,3,4,5)
  and    b.fecha_valuacion = fecha_valuacion
  ORDER BY 1

  FOREACH cur_1 INTO codigo_siefore ,
                     razon_social   ,
                     precio_del_dia

       LET g_arr_siefore[codigo_siefore].razon_social = 
           razon_social

       LET g_arr_siefore[codigo_siefore].precio_del_dia = 
           precio_del_dia

  END FOREACH

  FOR i = 1 TO 5 

   IF g_arr_siefore[i].precio_del_dia is null THEN
      LET l_resul = 1
      EXIT FOR
   END IF
  END FOR

  RETURN l_resul

END FUNCTION


FUNCTION previo()
#----------------

 DEFINE band_salir SMALLINT
 DEFINE ltotal_tes INTEGER
 DEFINE cuantos    smallint
 LET band_salir = 0
 LET g_resul    = 0


 SELECT count(*)
 into   cuantos
 FROM   safre_af:cta_ctrl_tes_ied a
 WHERE  a.tipo_proceso = 1
 AND    a.estado4 <> "OK"

 IF cuantos <> 0 THEN 
   PROMPT "PROCESO DE PREVIO AUN EJECUTANDOSE ...<Enter>..." FOR char g_enter 
   RETURN
 END IF

 SELECT "OK" 
 FROM   safre_af:cta_ctrl_tes_ied a
 WHERE  a.estado4 = "OK"
 AND    a.fecha is not null
 GROUP BY 1

 IF STATUS <> NOTFOUND THEN 

 WHILE TRUE
     PROMPT "PREVIO YA GENERADO,CONFIRMAR EJECUCION DE PREVIO...[S/N]?"FOR char g_enter
         IF g_enter MATCHES "[sSnN]" THEN
            IF g_enter MATCHES "[sS]" THEN
               DISPLAY "                                                                               " at 19,1
               DELETE FROM cta_ctrl_tes_ied
               DELETE FROM tes_ctr_folio_previo
               EXIT WHILE
            ELSE
               DISPLAY "                                                                               " at 19,1
               DISPLAY"PREVIO CANCELADO" AT 19,2 ATTRIBUTE(REVERSE) 
               SLEEP 2
               DISPLAY "                                                                               " at 19,1
               RETURN
            END IF
         END IF
 END WHILE
 END IF


 OPEN WINDOW ventana_2 AT 2,2 WITH FORM "CTALT01" ATTRIBUTE(BORDER)   

 DISPLAY " [Ctrl-c]Salir [ESC]Continuar                                                  " AT 1,1 ATTRIBUTE(REVERSE)

 DISPLAY " CTALT01      PREVIO DE LIQ DE TRASPASO SIEFORES I. X EDAD                     " AT 3,1 ATTRIBUTE(REVERSE)

 DISPLAY g_today USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

 INPUT BY NAME fecha_valuacion
 AFTER FIELD fecha_valuacion
    CALL ini_precio_sie() RETURNING g_resul
    IF g_resul = 1 THEN 
       PROMPT"PRECIOS DE SIEFORE INCOSISTENTES....<Enter> Para Continuar" 
       FOR char g_enter
       NEXT FIELD fecha_valuacion
    END IF

 ON KEY (ESC) 
    CALL ini_precio_sie() RETURNING g_resul
    IF g_resul = 1 THEN 
       PROMPT"PRECIOS DE SIEFORE INCOSISTENTES....<Enter> Para Continuar" 
       FOR char g_enter
       NEXT FIELD fecha_valuacion
    ELSE 
       EXIT INPUT
    END IF

 ON KEY (INTERRUPT)
    LET band_salir = 1
    EXIT INPUT  

 END INPUT

 IF band_salir THEN     
    CLOSE WINDOW ventana_2 
    RETURN
 END IF

 CALL SET_COUNT(5)

 DISPLAY ARRAY g_arr_siefore TO scr_1. *

 ON KEY (ESC)
    LET band_salir = 0    
    EXIT DISPLAY

 ON KEY (CONTROL-C) 
    LET band_salir = 1    
    EXIT DISPLAY

 END DISPLAY 

 IF band_salir = 1 THEN
    CLOSE WINDOW ventana_2 
 END IF

 SELECT COUNT(unique nss) 
 INTO   ltotal_tes
 FROM tes_solicitud a 
 WHERE tipo_traspaso = 13
 AND   estado        = 100   

 DISPLAY "Total de Registros a Procesar: ",ltotal_tes  AT 16,5

 WHILE TRUE
     PROMPT "CONFIRMAR EJECUCION DE PREVIO...[S/N]?"FOR char g_enter
         IF g_enter MATCHES "[sSnN]" THEN
            IF g_enter MATCHES "[sS]" THEN
               DISPLAY "                                                                               " at 19,1
               EXIT WHILE
            ELSE
               DISPLAY "                                                                               " at 19,1
               DISPLAY"PREVIO CANCELADO" AT 19,2 ATTRIBUTE(REVERSE) 
               SLEEP 2
               CLOSE WINDOW ventana_2
               DISPLAY "                                                                               " at 19,1
               RETURN
            END IF
         END IF
 END WHILE

 DISPLAY"Lanzando Ejecucion de Proceso ..." AT 17,5

 LET lanzador = "nohup fglgo ",g_seg_modulo.ruta_exp CLIPPED,"/",
              "CTALT02 ",fecha_valuacion," 1>",
              g_seg_modulo.ruta_listados CLIPPED,
              "/transf_ied_",fecha_valuacion USING"YYYYMMDD"," 2>&1 &"

 RUN lanzador

 INSERT INTO cta_ctrl_tes_ied 
 VALUES (1 ,
         fecha_valuacion ,
         "OK"            ,
         null            , 
         null            , 
         null            , 
         null            , 
         null            , 
         null            , 
         null            )

CLOSE WINDOW ventana_2
END FUNCTION


FUNCTION consulta_previo()
#-------------------------

 DEFINE reg_monitor RECORD 
        accion1 char(03),
        estado1 char(03),
        accion2 char(03),
        estado2 char(03),
        accion3 char(03),
        estado3 char(03),
        accion4 char(03),
        estado4 char(03)
 END RECORD


 OPEN WINDOW ventana_3 AT 2,2 WITH FORM "CTALT011" ATTRIBUTE(BORDER)   

 DISPLAY " [Ctrl-c]Salir                                                                 " AT 1,1 ATTRIBUTE(REVERSE)

 DISPLAY " CTALT01      MONITOR DE PREVIO LIQ TRASPASO SIEDFORE I. X EDAD                " AT 3,1 ATTRIBUTE(REVERSE)

 DISPLAY g_today USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

 SELECT a.accion1,
a.estado1,
a.accion2,
a.estado2,
a.accion3,
a.estado3,
a.accion4,
a.estado4
 INTO reg_monitor.*
 FROM cta_ctrl_tes_ied a
 WHERE a.tipo_proceso = 1
 
 DISPLAY BY name reg_monitor.* 

 SLEEP 5

 CLOSE WINDOW ventana_3
 RETURN

END FUNCTION
