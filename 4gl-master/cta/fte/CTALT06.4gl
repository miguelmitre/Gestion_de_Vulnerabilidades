--------------------------------------------------------------------------
-- Programa    : CTALT06 
-- Descripcion : LIQUIDACION TRASPASO ENTRE SIEFORES I. X EDAD
-- Fecha       : 1 de SEP 2010
-- Autor       : Jesus Yanez Moreno
#------------------------------MODIFICACIONES----------------------------#
#Requerimiento     => MLM-1139                                           #
#Fecha y Autor     => 09-Ago-2012      Alejandro Chagoya Salazar         #
#Descripcion       => Se muestran cifras antes de liquidar               #
##########################################################################

DATABASE safre_af
GLOBALS
   DEFINE g_folio             INTEGER

   DEFINE g_resul             SMALLINT
  
   DEFINE lanzador            CHAR(1000),
          g_enter             CHAR(001)

   DEFINE g_today             , 
          g_fecha_valuacion     DATE

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
    DISPLAY " CTALT06   LIQUIDACION DE TRASPASO SIEFORES I. X EDAD                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY g_today USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

  MENU "LIQUIDACION TRASPASO SIEFORE I. X EDAD"    
  COMMAND "Liquidar" "Ejecuta Liquidacion de Traspaso Siefores I. x Edad"
       CALL liquidacion()
  COMMAND "Consulta Liquidacion" "Monitor de avance de Liquidacion"
       CALL consulta_liquidacion()
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
  and    b.fecha_valuacion = g_fecha_valuacion
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


FUNCTION liquidacion()
#----------------
   DEFINE arreglo  ARRAY[5] OF RECORD
      siefore             SMALLINT,
      monto_en_pesos      DECIMAL(16,6),
      precio_accion       DECIMAL(16,6),
      monto_en_acciones   DECIMAL(16,6)
   END RECORD
   
   DEFINE r_suma RECORD
      reg_sb1,
      reg_sb2,
      reg_sb3,
      reg_sb4,
      reg_sb5        SMALLINT
   END RECORD,
      l_i            SMALLINT

 DEFINE band_salir SMALLINT
 DEFINE ltotal_tes INTEGER
 DEFINE cuantos    smallint
 LET band_salir = 0
 LET g_resul    = 0


 SELECT COUNT(*) INTO   cuantos
 FROM   safre_af:cta_ctrl_tes_ied_liq a
 WHERE  a.tipo_proceso = 2
 AND    a.estado3 <> "OK"

 IF cuantos <> 0 THEN 
   PROMPT "PROCESO DE LIQUIDACION AUN EJECUTANDOSE ...<Enter>..." FOR CHAR g_enter 
   RETURN
 END IF

 SELECT "OK" 
 FROM   safre_af:cta_ctrl_tes_ied_liq a
 WHERE  a.estado3 = "OK"
 AND    a.fecha is not null
 GROUP BY 1

 IF STATUS <> NOTFOUND THEN 
     PROMPT "LIQUIDACION YA GENERADA <Entre> Salir ?"FOR char g_enter
     RETURN
 END IF

 OPEN WINDOW ventana_2 AT 2,2 WITH FORM "CTALT062" ATTRIBUTE(BORDER)   

 DISPLAY " [Ctrl-c]Salir [ESC]Continuar                                                  " AT 1,1 ATTRIBUTE(REVERSE)
 DISPLAY " CTALT06    LIQUIDACION DE TRASPASO SIEFORES I. X EDAD                     " AT 3,1 ATTRIBUTE(REVERSE)
 DISPLAY g_today USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

 INPUT BY NAME g_folio, g_fecha_valuacion

 AFTER FIELD g_folio
   IF g_folio IS NULL OR g_folio = 0 THEN
      ERROR "INGRESE UN FOLIO VALIDO"
      NEXT FIELD g_folio
   END IF
   IF f_folio() = 1 THEN
      ERROR "FOLIO NO ENCONTRADO, VERIFIQUE"
      NEXT FIELD g_folio
   END IF

 AFTER FIELD g_fecha_valuacion
    IF g_fecha_valuacion IS NULL THEN
       ERROR "FECHA INVALIDA"
       NEXT FIELD g_fecha_valuacion
    END IF

    CALL ini_precio_sie() RETURNING g_resul
    IF g_resul = 1 THEN 
       PROMPT"PRECIOS DE SIEFORE INCOSISTENTES....<Enter> Para Continuar" 
       FOR char g_enter
       NEXT FIELD g_fecha_valuacion
    END IF

 ON KEY (ESC)

   ERROR ""
   ERROR "PROCESANDO INFORMACION"

   IF g_folio IS NULL OR g_folio = 0 THEN
      ERROR "INGRESE UN FOLIO VALIDO"
      NEXT FIELD g_folio
   END IF
   IF f_folio() = 1 THEN
      ERROR "FOLIO NO ENCONTRADO, VERIFIQUE"
      NEXT FIELD g_folio
   END IF

    IF g_fecha_valuacion IS NULL THEN
       ERROR "FECHA INVALIDA"
       NEXT FIELD g_fecha_valuacion
    END IF

    CALL ini_precio_sie() RETURNING g_resul
    IF g_resul = 1 THEN 
       PROMPT"PRECIOS DE SIEFORE INCOSISTENTES....<Enter> Para Continuar" 
       FOR char g_enter
       NEXT FIELD g_fecha_valuacion
    END IF

    EXIT INPUT

 ON KEY (CONTROL-C,INTERRUPT) 
    LET band_salir = 1
    EXIT INPUT  

 END INPUT

 IF band_salir THEN     
    CLOSE WINDOW ventana_2 
    RETURN
 END IF

#MLM-1139 INI

      DECLARE cur_2 CURSOR FOR
      SELECT siefore, SUM(monto_en_acciones)
      FROM   dis_provision
      WHERE  folio = g_folio
      AND    tipo_movimiento IN(55, 255)
      GROUP BY 1 ORDER BY 1

       FOR l_i = 1 TO 5
           INITIALIZE arreglo[l_i].* TO NULL
       END FOR

      LET l_i = 1

      FOREACH cur_2 INTO  arreglo[l_i].siefore,
                          arreglo[l_i].monto_en_acciones

#DISPLAY "i ",l_i," siefore ",arreglo[l_i].siefore, " fecha ",g_fecha_valuacion

         SELECT precio_del_dia INTO arreglo[l_i].precio_accion
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = g_fecha_valuacion
         AND    codigo_siefore = arreglo[l_i].siefore

         LET arreglo[l_i].monto_en_pesos = arreglo[l_i].monto_en_acciones * arreglo[l_i].precio_accion
         LET l_i = l_i + 1

      END FOREACH

   IF( ( l_i - 1 ) = 0 ) THEN
      ERROR " NO EXISTE INFORMACION DEL FOLIO "
      SLEEP 3
      CLOSE WINDOW ventana_2
      RETURN
   END IF

      LET r_suma.reg_sb1 = f_suma(1)
      LET r_suma.reg_sb2 = f_suma(2)
      LET r_suma.reg_sb3 = f_suma(3)
      LET r_suma.reg_sb4 = f_suma(4)
      LET r_suma.reg_sb5 = f_suma(5)

      DISPLAY r_suma.*  TO scr_2.*

   ERROR ""
   CALL SET_COUNT( l_i - 1 )

   DISPLAY ARRAY arreglo TO scr_1.*

     ON KEY (ESC)
        ERROR ""
        LET band_salir = 0    
        EXIT DISPLAY
    
     ON KEY (CONTROL-C,INTERRUPT) 
        LET band_salir = 1    
        EXIT DISPLAY
    
   END DISPLAY 

 IF band_salir = 1 THEN
    CLOSE WINDOW ventana_2
    RETURN
 END IF

 LET ltotal_tes = 0
 SELECT COUNT(UNIQUE nss) INTO ltotal_tes
 FROM tes_solicitud a 
 WHERE tipo_traspaso = 13
 AND   estado        = 100

DISPLAY "Total de Registros a Procesar: ",ltotal_tes  AT 19,5 ATTRIBUTE (REVERSE)

#MLM-1139 FIN

 WHILE TRUE
     PROMPT "CONFIRMAR EJECUCION DE LIQUIDACION...[S/N]?"FOR char g_enter
         IF g_enter MATCHES "[sSnN]" THEN
            IF g_enter MATCHES "[sS]" THEN
               DISPLAY "                                                                               " at 19,1
               EXIT WHILE
            ELSE
               DISPLAY "                                                                               " at 19,1
               DISPLAY"LIQUIDACION CANCELADA" AT 21,2 ATTRIBUTE(REVERSE) 
               SLEEP 2
               CLOSE WINDOW ventana_2
               DISPLAY "                                                                               " at 19,1
               RETURN
            END IF
         END IF
 END WHILE

 DISPLAY"Lanzando Ejecucion de Proceso ..." AT 17,5

 LET lanzador = "nohup fglgo ",g_seg_modulo.ruta_exp CLIPPED,"/",
              "CTALT07 ",g_folio, " ", g_fecha_valuacion," 1>",
              g_seg_modulo.ruta_listados CLIPPED,
              "/transf_liq_ied_",g_fecha_valuacion USING"YYYYMMDD"," 2>&1 &"


 RUN lanzador

 INSERT INTO cta_ctrl_tes_ied_liq 
 VALUES (2 ,
         g_fecha_valuacion ,
         "OK"            ,
         null            , 
         null            , 
         null            , 
         null            , 
         null            )


CLOSE WINDOW ventana_2
END FUNCTION


FUNCTION consulta_liquidacion()
#------------------------------

 DEFINE reg_monitor RECORD 
        accion1 char(03),
        estado1 char(03),
        accion2 char(03),
        estado2 char(03),
        accion3 char(03),
        estado3 char(03)
 END RECORD
 
 DEFINE
    vmaxfecha   DATE
    

 OPEN WINDOW ventana_3 AT 2,2 WITH FORM "CTALT061" ATTRIBUTE(BORDER)   

 DISPLAY " [Ctrl-c]Salir                                                                 " AT 1,1 ATTRIBUTE(REVERSE)

 DISPLAY " CTALT06  MONITOR DE LIQUIDACION TRASPASO SIEDFORE I. X EDAD                " AT 3,1 ATTRIBUTE(REVERSE)

 DISPLAY g_today USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

 SELECT MAX(fecha)
 INTO   vmaxfecha
 FROM   cta_ctrl_tes_ied_liq
   
 SELECT a.accion1,
        a.estado1,
        a.accion2,
        a.estado2,
        a.accion3,
        a.estado3
 INTO   reg_monitor.*
 FROM   cta_ctrl_tes_ied_liq a
 WHERE  a.tipo_proceso = 2
 AND    a.fecha = vmaxfecha
 
 DISPLAY BY name reg_monitor.* 

 SLEEP 5

 CLOSE WINDOW ventana_3
 RETURN

END FUNCTION

##########################################################################
FUNCTION f_suma(l_siefore)
DEFINE l_siefore  SMALLINT,
       l_regs     INTEGER

LET l_regs = 0

  SELECT COUNT(UNIQUE nss) INTO l_regs
  FROM   dis_provision
  WHERE  folio = g_folio
  AND    tipo_movimiento IN(55, 255)
  AND    siefore = l_siefore

RETURN l_regs

END FUNCTION

##########################################################################
FUNCTION f_folio()

    SELECT a.folio
    FROM   tes_ctr_folio_previo a
    WHERE  folio = g_folio 
    AND    a.tipo_traspaso = 13
    AND    a.estado = 102

  IF SQLCA.SQLCODE != 0 THEN
     RETURN 1
  END IF

RETURN 0   
END FUNCTION
