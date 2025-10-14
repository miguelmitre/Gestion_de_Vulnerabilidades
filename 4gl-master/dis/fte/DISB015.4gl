################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Fecha             => 22 DE JUNIO 1999.	          			       #
#By                => MIGUEL ANGEL HERNANDEZ MARTINEZ. 			       #
#Sistema           => DIS. 					               #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE g RECORD LIKE dis_cuenta.*
   DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
   DEFINE HOY DATE

   DEFINE g_usuario CHAR (08)
   DEFINE g_param_dis RECORD LIKE dis_parametro.*
   DEFINE G_LISTA CHAR(300)
   DEFINE G_IMPRE CHAR(300)
   DEFINE gimpresion CHAR(300)
   DEFINE hora CHAR (08)

   DEFINE v RECORD
      vfecha DATE
   END RECORD

   DEFINE vrcv RECORD
      vfecha DATE
   END RECORD

   DEFINE vviv RECORD
      vfecha DATE
   END RECORD
    
   DEFINE l_record ARRAY[1300] OF RECORD
      folio            INTEGER,
      tipo_movimiento  INTEGER,
      subcuenta        INTEGER,
      fecha_valor      DATE,
      fecha_conversion DATE,
      etiqueta         INTEGER,
      monto_en_pesos   DECIMAL(16,6)
   END RECORD

   DEFINE l_record2 ARRAY[1300] OF RECORD
      fecha_valor DATE,
      monto_en_pesos DECIMAL(16,6)
   END RECORD

   DEFINE l_record4 ARRAY[1300] OF RECORD
      fecha_valor     DATE,
      tipo_movimiento INTEGER,
      monto_en_pesos  DECIMAL(16,6)
   END RECORD

   DEFINE vpasswd CHAR(01)

   DEFINE g_reg4 RECORD
      super_cod  SMALLINT,
      super_desc CHAR(30),
      nip        INTEGER
   END RECORD

   DEFINE vnip INTEGER

   DEFINE opc CHAR(01)

   DEFINE l_record6 ARRAY[1300] OF RECORD
      fecha_valor     DATE,
      tipo_movimiento INTEGER,
      monto_en_pesos  DECIMAL(16,6)
   END RECORD

   DEFINE cont integer

   DEFINE vfecha_inicio DATE,
          vfecha_final  DATE,
          dias          SMALLINT
  
END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o
      DEFER INTERRUPT

-----   LET vpasswd = "N"

-----   CALL Aplica_passwd() RETURNING vpasswd
-----   IF vpasswd="S" THEN
-----      ERROR "Acceso aceptado"
-----      SLEEP 3
      CALL inicio()
      CALL proceso_principal()
-----   END IF 
END MAIN

FUNCTION Aplica_passwd()
   OPEN WINDOW ventana_4 AT 10,16 WITH FORM "DISB0158" ATTRIBUTE(BORDER)
   DISPLAY " [Esc] Procesar                     [Ctrl-c] Cancelar " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME g_reg4.*

      AFTER FIELD super_cod
         IF g_reg4.super_cod IS NULL THEN
            ERROR "ID no puede ser nulo"
            NEXT FIELD super_cod
         END IF

         SELECT super_desc,
                nip
           INTO g_reg4.super_desc,
                vnip 
           FROM tab_supervisor 
          WHERE super_cod = g_reg4.super_cod
          IF STATUS = NOTFOUND THEN
             ERROR "No existe este ID "
             NEXT FIELD super_cod
          END IF
         DISPLAY BY NAME g_reg4.super_desc
         NEXT FIELD nip

      AFTER FIELD nip
         IF g_reg4.nip <> vnip THEN
            ERROR "Clave de acceso no existe"
            LET g_reg4.nip = null
            NEXT FIELD super_cod
         END IF
         IF g_reg4.nip IS NULL OR
            g_reg4.nip = 0 THEN
            ERROR "Clave de acceso no existe"
            NEXT FIELD super_cod
         END IF
      ON KEY (ESC)
         IF g_reg4.super_cod IS NULL THEN
            ERROR "ID no puede ser nulo"
            NEXT FIELD super_cod
         END IF
         SELECT "x"
           FROM tab_supervisor 
          WHERE super_cod = g_reg4.super_cod
         IF STATUS = NOTFOUND THEN
            ERROR "No existe este ID "
            NEXT FIELD super_cod
         END IF
         IF g_reg4.nip IS NULL OR
            g_reg4.nip = 0 THEN
            ERROR "Clave de acceso no existe"
            NEXT FIELD super_cod
         END IF
         IF g_reg4.nip <> vnip THEN
            ERROR "Clave de acceso no existe"
            LET g_reg4.nip = null
            NEXT FIELD super_cod
         END IF

         LET vpasswd = "S"
         EXIT INPUT

      ON KEY (INTERRUPT)
         ERROR "Acceso denegado"
         SLEEP 2
         LET vpasswd = "N"
         EXIT INPUT
          
   END INPUT

   CLOSE WINDOW ventana_4
   RETURN vpasswd
END FUNCTION

FUNCTION inicio()

   SELECT  codigo_afore,USER
   INTO    w_codigo_afore,g_usuario
   FROM    tab_afore_local

   SELECT  *
   INTO    g_param_dis.*
   FROM    dis_parametro

   LET hoy = TODAY
END FUNCTION

FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 3,4 
   WITH 3 ROWS, 72 COLUMNS
   ATTRIBUTE(BORDER)
   DISPLAY " DISB015 " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)

   MENU "MENU "
      COMMAND "Aportes Dia" 
         CALL Aportes_dia()
      COMMAND "Aportes Vivienda"
         CALL Aportes_vivienda()
      COMMAND "Intereses Vivienda"
         CALL Intereses_vivienda() 
      COMMAND "Intereses retroactivos"
         CALL Intereses_retroactivos()
----      COMMAND "Actualiza etiquetas"
----         CALL Actualiza_etiqueta()
----      COMMAND "Integracion Intereses."
----         CALL Integra_intereses()
----      COMMAND "Respaldo 888"
----         CALL respaldo_888()
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION Aportes_dia()
    DEFINE pos INTEGER

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 10,4 WITH FORM "DISB0151" ATTRIBUTE( BORDER)
    DISPLAY "              (Ctrl-C) Salir          (ESC) Ejecutar                           " AT 1,1 ATTRIBUTE(REVERSE,green)
    LET int_flag= FALSE
    INPUT BY NAME v.*
        ON KEY (ESC)
            LET int_flag =FALSE
            EXIT INPUT
        ON KEY (control-c)
            LET int_flag = TRUE
            EXIT INPUT
    END INPUT
   
    IF int_flag = TRUE THEN
       LET int_flag = FALSE
       CLEAR FORM
       ERROR "Fecha no aceptada."
       SLEEP 2
       ERROR ""
       CLEAR SCREEN
       CLOSE WINDOW ventana_21
       RETURN
    END IF

    ERROR "PROCESANDO INFORMACION"
       DECLARE cursor_1 CURSOR FOR
       SELECT folio,
              tipo_movimiento,                                        
              subcuenta,                                                 
              fecha_valor,                                                
              fecha_conversion,                                               
              etiqueta,                                                 
              sum(monto_en_pesos)                                    
       FROM   dis_cuenta
       WHERE  subcuenta  in (4,8)
       AND    fecha_valor = v.vfecha    
       GROUP BY 1,2,3,4,5,6     
       ORDER BY 1,2,3

       LET pos = 1

       FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
       END FOREACH

    INITIALIZE l_record[pos].* TO NULL

    CLOSE WINDOW ventana_21
        IF (pos-1) >= 1 THEN
	  CALL  SET_COUNT(pos-1)
          ERROR ""
	  OPEN WINDOW ventana_2 AT 6,4 WITH FORM "DISB0152" ATTRIBUTE( BORDER)
	  DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
	  DISPLAY " (Ctrl-C) Salir            (Ctrl-P) Imprimir           (Ctrl-F) Archivo  " AT 1,1 ATTRIBUTE(REVERSE,green)
	  DISPLAY "                  Consulta Montos Exclusivos de un Dia                         " AT 3,1 ATTRIBUTE(REVERSE,green) 
   	
          DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (Control-p)
               ERROR "PROCESANDO IMPRESION..."
               CALL imprimir(pos)
            ON KEY (Control-f)
               ERROR "PROCESANDO LISTADO..."
               CALL archivo(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
          END DISPLAY

            CLEAR WINDOW ventana_2
            CLOSE WINDOW ventana_2
	 ELSE
	    ERROR "ARCHIVO ... VACIO"
            SLEEP 2
            ERROR ""
	 END IF 
END FUNCTION
#Reporte de archivo------------------------------------------------------------
FUNCTION archivo(pos)

   DEFINE pos INTEGER

   LET hora = TIME
   LET G_LISTA = g_param_dis.ruta_spool CLIPPED,"/","AFOAPDIAVIV"
   START REPORT rpt_cuenta_arc TO G_LISTA
   CALL cuentas_archi(pos)

END FUNCTION
 
FUNCTION cuentas_archi(pos)
          DEFINE i,pos INTEGER

             FOR i=1 TO (pos+1)
                LET g.folio           = l_record[i].folio
                LET g.tipo_movimiento = l_record[i].tipo_movimiento
                LET g.subcuenta       = l_record[i].subcuenta
                LET g.fecha_valor     = l_record[i].fecha_valor
                LET g.fecha_conversion= l_record[i].fecha_conversion
                LET g.etiqueta        = l_record[i].etiqueta
                LET g.monto_en_pesos  = l_record[i].monto_en_pesos

                IF g.fecha_valor IS NULL OR g.fecha_valor="12/31/1899" THEN
                   EXIT FOR
                END IF

                OUTPUT TO REPORT rpt_cuenta_arc(g.*)
             END FOR

             FINISH REPORT rpt_cuenta_arc
             ERROR "LISTADO GENERADO" 
             SLEEP 2
             ERROR ""

             LET G_LISTA = "chmod 777 ",g_param_dis.ruta_spool CLIPPED,"/",
                           "AFOAPDIAVIV" 
             RUN G_LISTA
END FUNCTION

REPORT rpt_cuenta_arc(g)
       DEFINE g RECORD LIKE dis_cuenta.*

       FORMAT
         PAGE HEADER
           PRINT COLUMN 1,  "FOLIO",
                 COLUMN 9,  "MOV.",
                 COLUMN 15, "SUB.",
                 COLUMN 27, "FEC.VAL.",
                 COLUMN 40, "FEC.CONV.",
                 COLUMN 52, "ETIQ.",
                 COLUMN 62, "PESOS"
         SKIP 1 LINE
         ON EVERY ROW
           PRINT COLUMN 1,g.folio USING "######",
                 COLUMN 7,g.tipo_movimiento USING "###",
                 COLUMN 15,g.subcuenta USING "##",
                 COLUMN 27,g.fecha_valor USING "dd-mm-yyyy",
                 COLUMN 40,g.fecha_conversion USING "dd-mm-yyyy",
                 COLUMN 52,g.etiqueta USING "&&",
                 COLUMN 62,g.monto_en_pesos 
END REPORT
#Reporte de impresora----------------------------------------------------------
FUNCTION imprimir(pos)
        DEFINE pos INTEGER

        LET hora = TIME

        LET G_IMPRE = g_param_dis.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
                               ".LIS_APORT_DIA_",HOY USING "DD-MM-YYYY",
                                 "_",hora CLIPPED

        START REPORT rpt_cuenta_imp TO G_IMPRE
        CALL cuentas_impri(pos)

        LET gimpresion = "lp ",G_IMPRE
        RUN gimpresion
END FUNCTION

FUNCTION cuentas_impri(pos)
      DEFINE i,pos INTEGER

         FOR i=1 TO (pos+1)
            LET g.folio           = l_record[i].folio
            LET g.tipo_movimiento = l_record[i].tipo_movimiento
            LET g.subcuenta       = l_record[i].subcuenta
            LET g.fecha_valor     = l_record[i].fecha_valor
            LET g.fecha_conversion= l_record[i].fecha_conversion
            LET g.etiqueta        = l_record[i].etiqueta
            LET g.monto_en_pesos  = l_record[i].monto_en_pesos

            IF g.fecha_valor IS NULL OR g.fecha_valor="12/31/1899" THEN
                   EXIT FOR
            END IF

            OUTPUT TO REPORT rpt_cuenta_imp(g.*)
         END FOR

         FINISH REPORT rpt_cuenta_imp
         ERROR "LISTADO GENERADO"
         SLEEP 2
         ERROR ""
END FUNCTION

REPORT rpt_cuenta_imp(g)
     DEFINE g RECORD LIKE dis_cuenta.*

     OUTPUT
       TOP MARGIN 1
       BOTTOM MARGIN 0
       LEFT MARGIN   0
       RIGHT MARGIN  0
       PAGE LENGTH   60

       FORMAT
       PAGE HEADER
          PRINT COLUMN 02,"DISB015",
                COLUMN 20,"LISTADO DE APORTACION DEL DIA AFORE",
                COLUMN 65, TODAY USING "mm/dd/yy"
          SKIP 2 LINE

          PRINT COLUMN 1,  "FOLIO",
                COLUMN 9,  "MOV.",
                COLUMN 15, "SUB.",
                COLUMN 27, "FEC.VAL.",
                COLUMN 40, "FEC.CONV.",
                COLUMN 52, "ETIQ.",
                COLUMN 62, "PESOS"
          SKIP 1 LINE

       ON EVERY ROW
          PRINT COLUMN 1,g.folio USING "######",
                COLUMN 8,g.tipo_movimiento USING "#####",
                COLUMN 15,g.subcuenta USING "#####",
                COLUMN 27,g.fecha_valor USING "dd-mm-yyyy",
                COLUMN 40,g.fecha_conversion USING "dd-mm-yyyy",
                COLUMN 52,g.etiqueta USING "&&",
                COLUMN 62,g.monto_en_pesos

       PAGE TRAILER
          SKIP 2 LINES
          PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
          PAUSE "Presione enter para continuar...."

       ON LAST ROW
          SKIP 4 LINES
          PRINT COLUMN 2, "Total de registros encontrados: ",
          COUNT(*) USING "<<<<"
END REPORT

FUNCTION Aportes_vivienda()
   DEFINE pos                INTEGER

   CLEAR SCREEN
   OPEN WINDOW ventana_003 AT 10,4 WITH FORM "DISB0153" ATTRIBUTE(BORDER)
   DISPLAY "              (Ctrl-C) Salir          (ESC) Ejecutar                           " AT 1,1 ATTRIBUTE(REVERSE,green)

   LET int_flag = FALSE 
   INPUT BY NAME vviv.*
      ON KEY (ESC)
         LET int_flag = FALSE
         EXIT INPUT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      CLEAR FORM
      ERROR "Fecha no encontrada."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_003
      RETURN
   END IF

   LET vfecha_inicio = vviv.vfecha - 1 UNITS MONTH

   CALL Ultimo_dia_mes(vfecha_inicio) RETURNING dias

   LET vfecha_final = MDY(MONTH(vfecha_inicio),dias,YEAR(vfecha_inicio))

   ERROR "PROCESANDO INFORMACION"

   DECLARE cursor_3 CURSOR FOR
   SELECT fecha_valor,
          sum(monto_en_pesos)
   FROM   dis_cuenta
   WHERE  fecha_conversion BETWEEN vfecha_inicio AND vfecha_final
   AND    subcuenta IN (4,8)
   AND    tipo_movimiento <> 3
   GROUP  BY 1

   UNION

   SELECT fecha_valor,
          sum(monto_en_pesos)
   FROM   cta_saldo_viv
   GROUP BY 1
   ORDER BY 1

   LET pos = 1
   FOREACH cursor_3 INTO l_record2[pos].*
        LET pos = pos + 1
   END FOREACH
  
   INITIALIZE l_record2[pos].* TO NULL

   CLOSE WINDOW ventana_003

   IF (pos-1) >= 1 THEN
     CALL  SET_COUNT(pos-1)
     ERROR ""
     OPEN WINDOW ventana_4 AT 6,6 WITH FORM "DISB0154" ATTRIBUTE( BORDER)
     DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
     DISPLAY " (Ctrl-C) Salir       (Ctrl-P) Imprimir       (Ctrl-F) Archivo                 " AT 1,1 ATTRIBUTE(REVERSE,green)
     DISPLAY "              Consulta de Aportaciones de Vivienda                             " AT 3,1 ATTRIBUTE(REVERSE,green)

     DISPLAY ARRAY l_record2 TO scr_2.*
        ON KEY (Control-f)
           ERROR "PROCESANDO LISTADO..."
           CALL archivo2(pos)
        ON KEY (Control-p)
           ERROR "PROCESANDO IMPRESION..."
           CALL imprimir2(pos)
        ON KEY (INTERRUPT)
           EXIT DISPLAY
     END DISPLAY

     CLEAR WINDOW ventana_4
     CLOSE WINDOW ventana_4
   ELSE
     ERROR "ARCHIVO ... VACIO"
     SLEEP 2
     ERROR ""
   END IF

END FUNCTION 

#Reporte de archivo_3----------------------------------------------------------

FUNCTION archivo2(pos)
   DEFINE pos INTEGER

   LET hora = TIME
   LET G_LISTA = g_param_dis.ruta_spool CLIPPED,"/",
                 "AFOAPVIV"

   START REPORT rpt_cuenta2_arc TO G_LISTA
   CALL cuentas_rpt2_arc(pos)

END FUNCTION

FUNCTION cuentas_rpt2_arc(pos)
   DEFINE i,pos INTEGER

   FOR i=1 TO (pos+1)
     LET g.fecha_valor     = l_record2[i].fecha_valor
     LET g.monto_en_pesos  = l_record2[i].monto_en_pesos

     IF g.fecha_valor IS NULL OR g.fecha_valor="12/31/1899" THEN
            EXIT FOR
     END IF

     OUTPUT TO REPORT rpt_cuenta2_arc(g.*)
   END FOR
          
   FINISH REPORT rpt_cuenta2_arc
   ERROR "LISTADO GENERADO"
   SLEEP 2
   ERROR ""

   LET G_LISTA = "chmod 777 ",g_param_dis.ruta_spool CLIPPED,"/",
                 "AFOAPVIV"
   RUN G_LISTA

END FUNCTION

REPORT rpt_cuenta2_arc(g)
   DEFINE g RECORD LIKE dis_cuenta.*

   FORMAT
   PAGE HEADER
     PRINT COLUMN 5, "FEC.VAL.",
           COLUMN 18, "PESOS"
     SKIP 1 LINE

   ON EVERY ROW
     PRINT COLUMN 5,g.fecha_valor USING "dd-mm-yyyy",
           COLUMN 18,g.monto_en_pesos

END REPORT
 
#Reporte de impresion_3--------------------------------------------------------

FUNCTION imprimir2(pos)
   DEFINE pos INTEGER

   LET hora = TIME

   LET G_IMPRE = g_param_dis.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
                               ".LIS_APORT_VIV_",HOY USING "DD-MM-YYYY",
                                 "_",hora CLIPPED

   START REPORT rpt_cuenta2_imp TO G_IMPRE
   CALL cuentas_rpt2_imp(pos)

   LET gimpresion = "lp ",G_IMPRE
   RUN gimpresion

END FUNCTION

FUNCTION cuentas_rpt2_imp(pos)
   DEFINE i,pos INTEGER

   FOR i=1 TO (pos+1)
       LET g.fecha_valor     = l_record2[i].fecha_valor
       LET g.monto_en_pesos  = l_record2[i].monto_en_pesos

       IF g.fecha_valor IS NULL OR g.fecha_valor="12/31/1899" THEN
           EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_cuenta2_imp(g.*)
   END FOR

   FINISH REPORT rpt_cuenta2_imp
   ERROR "LISTADO GENERADO" 
   SLEEP 2
   ERROR ""

END FUNCTION

REPORT rpt_cuenta2_imp(g)
   DEFINE g RECORD LIKE dis_cuenta.*

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

   FORMAT
   PAGE HEADER
      PRINT COLUMN 02,"DISB015",
            COLUMN 20,"LISTADO DE APORTACION DE VIVIENDA AFORE",
            COLUMN 65, TODAY USING "mm/dd/yy"
      SKIP 2 LINE

      PRINT COLUMN 17, "FEC.VAL.",
            COLUMN 52, "PESOS"
      SKIP 1 LINE

   ON EVERY ROW
      PRINT COLUMN 17,g.fecha_valor USING "dd-mm-yyyy",
            COLUMN 52,g.monto_en_pesos

   PAGE TRAILER
      SKIP 2 LINES
      PRINT COLUMN 60,"Pagina",PAGENO USING "<<<<"
         PAUSE "Presione enter para continuar...."

   ON LAST ROW
      SKIP 3 LINES
      PRINT COLUMN 2, "Total de registros encontrados: ",
          COUNT(*) USING "<<<<"

END REPORT

FUNCTION Intereses_vivienda()
   DEFINE pos                INTEGER

   DECLARE cursor_5 CURSOR FOR

   SELECT fecha_valor ,
          tipo_movimiento,
          sum(monto_en_pesos)
   FROM   dis_cuenta
   WHERE  subcuenta IN (4,8)
   AND    tipo_movimiento = 3
   GROUP  BY 1,2
   ORDER  BY 1,2

   ERROR "PROCESANDO INFORMACION"

   LET pos = 1
   FOREACH cursor_5 INTO l_record4[pos].*
         LET pos = pos + 1
   END FOREACH

   INITIALIZE l_record4[pos].* TO NULL

   CLEAR SCREEN

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      ERROR ""
      OPEN WINDOW ventana_6 AT 6,4 WITH FORM "DISB0155" ATTRIBUTE( BORDER)
      DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
      DISPLAY " (Ctrl-C) Salir          (Ctrl-P) Imprimir           (Ctrl-F) Archivo          " AT 1,1 ATTRIBUTE(REVERSE,green)
      DISPLAY "                   Consulta de Intereses de Vivienda                            " AT 3,1 ATTRIBUTE(REVERSE,green)

      DISPLAY ARRAY l_record4 TO scr_1.*
         ON KEY (Control-f)
            ERROR "PROCESANDO LISTADO..."
            CALL archivo4(pos)
         ON KEY (Control-p)
            ERROR "PROCESO IMPRESION..." 
            CALL imprimir4(pos)
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY

      CLEAR WINDOW ventana_6
      CLOSE WINDOW ventana_6
   ELSE
      ERROR "ARCHIVO ... VACIO"
      SLEEP 2
      ERROR ""
   END IF

END FUNCTION

#Reporte de archivo_5----------------------------------------------------------
FUNCTION archivo4(pos)
   DEFINE pos INTEGER

   LET hora = TIME
   LET G_LISTA = g_param_dis.ruta_spool CLIPPED,"/",
                 "AFOINTVIV"

   START REPORT rpt_cuenta4_arc TO G_LISTA
   CALL cuentas_rpt4_arc(pos) 

END FUNCTION

FUNCTION cuentas_rpt4_arc(pos)
   DEFINE i,pos INTEGER

   FOR i=1 TO (pos+1)
      LET g.fecha_valor     = l_record4[i].fecha_valor
      LET g.tipo_movimiento = l_record4[i].tipo_movimiento
      LET g.monto_en_pesos  = l_record4[i].monto_en_pesos

      IF g.fecha_valor IS NULL OR g.fecha_valor="12/31/1899" THEN
          EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_cuenta4_arc(g.*)
   END FOR

   FINISH REPORT rpt_cuenta4_arc
   ERROR "LISTADO GENERADO"
   SLEEP 2
   ERROR ""

   LET G_LISTA = "chmod 777 ",g_param_dis.ruta_spool CLIPPED,"/",
                   "AFOINTVIV"
   RUN G_LISTA

END FUNCTION

REPORT rpt_cuenta4_arc(g)
   DEFINE g RECORD LIKE dis_cuenta.*

   FORMAT
   PAGE HEADER
       PRINT COLUMN 5, "FEC.VAL.",
             COLUMN 22, "TIP.MOVM.",
             COLUMN 42, "PESOS"
       SKIP 1 LINE

   ON EVERY ROW
       PRINT COLUMN 5,g.fecha_valor USING "dd-mm-yyyy",
             COLUMN 22,g.tipo_movimiento,
             COLUMN 42,g.monto_en_pesos

END REPORT
 
#Reporte de impresion_5--------------------------------------------------------
FUNCTION imprimir4(pos)
   DEFINE pos INTEGER

   LET hora = TIME

   LET G_IMPRE = g_param_dis.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
                               ".LIS_INT_VIV_",HOY USING "DD-MM-YYYY",
                                 "_",hora CLIPPED

   START REPORT rpt_cuenta4_imp TO G_IMPRE
   CALL cuentas_rpt4_imp(pos)

   LET gimpresion = "lp ",G_IMPRE
   RUN gimpresion

END FUNCTION

FUNCTION cuentas_rpt4_imp(pos)
   DEFINE i,pos INTEGER

   FOR i=1 TO (pos+1)
      LET g.fecha_valor     = l_record4[i].fecha_valor
      LET g.tipo_movimiento = l_record4[i].tipo_movimiento
      LET g.monto_en_pesos  = l_record4[i].monto_en_pesos

      IF g.fecha_valor IS NULL OR g.fecha_valor="12/31/1899" THEN
             EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_cuenta4_imp(g.*)
   END FOR

   FINISH REPORT rpt_cuenta4_imp
   ERROR "LISTADO GENERADO" 
   SLEEP 2
   ERROR ""

END FUNCTION
           
REPORT rpt_cuenta4_imp(g)
   DEFINE g RECORD LIKE dis_cuenta.*

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

   FORMAT
   PAGE HEADER
      PRINT COLUMN 02,"DISB015",
            COLUMN 20,"LISTADO DE INTERESES DE VIVIENDA AFORE",
            COLUMN 65, TODAY USING "mm/dd/yy"
      SKIP 2 LINE

      PRINT COLUMN 17, "FEC.VAL.",
            COLUMN 30, "TIP.MOVM.",
            COLUMN 52, "PESOS"
      SKIP 1 LINE

   ON EVERY ROW
      PRINT COLUMN 17,g.fecha_valor USING "dd-mm-yyyy",
            COLUMN 30,g.tipo_movimiento,
            COLUMN 52,g.monto_en_pesos

   PAGE TRAILER
      SKIP 2 LINES
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
         PAUSE "Presione enter para continuar...."

   ON LAST ROW
      SKIP 3 LINES
      PRINT COLUMN 2, "Total de registros encontrados: ",
         COUNT(*) USING "<<<<"

END REPORT

FUNCTION Actualiza_etiqueta()

   CLEAR SCREEN

   OPEN WINDOW ventana_21 AT 10,4 WITH FORM "DISB0156" ATTRIBUTE(BORDER)
   DISPLAY "              (Ctrl-C) Salir          (ESC) Ejecutar                           " AT 1,1 ATTRIBUTE(REVERSE,green)

   LET int_flag = FALSE
   INPUT BY NAME v.vfecha
      ON KEY (ESC)
         LET int_flag = FALSE
         EXIT INPUT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
       LET int_flag = FALSE
       CLEAR FORM
       ERROR "Fecha no aceptado..."
       SLEEP 2
       ERROR ""
       CLEAR SCREEN
       CLOSE WINDOW ventana_21
       RETURN
   END IF                       

   IF v.vfecha IS NULL OR v.vfecha = "12/31/1899"  THEN 
      ERROR "ARCHIVO VACIO.."
      LET int_flag = TRUE
   ELSE
      ERROR "PORCESANDO INFORMACION..."

      SELECT "x"
        FROM cta_tasa_viv
       WHERE fecha_aplica = v.vfecha
       IF STATUS = NOTFOUND THEN
          ERROR "NO PUEDES ACTUALIZAR ETIQUETAS PORQUE ESTA FECHA NO EXISTE"
          SLEEP 3
          CLEAR SCREEN
          CLOSE WINDOW ventana_21
          RETURN
       END IF

-----------      LOCK TABLE dis_cuenta IN EXCLUSIVE MODE;
         UPDATE dis_cuenta
            SET etiqueta = 1
          WHERE subcuenta IN (4,8)
            AND tipo_movimiento NOT IN (3,888)
            AND fecha_valor < v.vfecha
            AND fecha_conversion < v.vfecha
            AND etiqueta = 0  
-----------      UNLOCK TABLE dis_cuenta

      CLEAR WINDOW ventana_21
      CLOSE WINDOW ventana_21
    END IF
   ERROR ""
END FUNCTION

FUNCTION Integra_intereses()
   DEFINE
      g_int RECORD
      nss            CHAR(11),
      subcuenta      INTEGER,
      fecha_valor    DATE,
      monto_en_pesos DECIMAl(16,6)
   END RECORD,
   g_param RECORD LIKE glo_parametro.*,
   vid_aportante  CHAR(11),
   usuario        CHAR(08),
   vmonto_nor     DECIMAL(16,6),
   vmonto_rem     DECIMAL(16,6)

  SELECT *,user
  INTO g_param.*,usuario
  FROM glo_parametro                     

  CLEAR SCREEN

  OPEN WINDOW ventana_21 AT 10,4 WITH FORM "DISB0157" ATTRIBUTE( BORDER)
  DISPLAY "              (Ctrl-C) Salir          (ESC) Ejecutar                           " AT 1,1 ATTRIBUTE(REVERSE,green)
  LET int_flag= FALSE
  INPUT BY NAME v.vfecha
   ON KEY (ESC)
      LET int_flag =FALSE
      EXIT INPUT
   ON KEY (control-c)
      LET int_flag = TRUE
      EXIT INPUT
  END INPUT

  IF int_flag = TRUE THEN
     LET int_flag = FALSE
     CLEAR FORM
     ERROR "Fecha no encontrada."
     SLEEP 2
     ERROR ""
     CLEAR SCREEN
     CLOSE WINDOW  ventana_21
     RETURN
  END IF                     

  LET vmonto_nor = 0
  LET vmonto_rem = 0

  ERROR "Buscando Fecha ..."
 
  DECLARE curint2 CURSOR FOR
  SELECT "x"
    FROM dis_cuenta
   WHERE subcuenta in (4,8)
     AND tipo_movimiento = 3
     AND fecha_conversion = v.vfecha
 
   OPEN curint2
   FETCH curint2
   IF STATUS <> NOTFOUND THEN
      ERROR "NO PUEDES INTEGRAR PORQUE YA ESTAN INTEGRADOS A ESTA FECHA"
      SLEEP 3
      CLOSE curint2
      CLEAR SCREEN
      CLOSE WINDOW  ventana_21
      RETURN
   END IF
   CLOSE curint2

   DECLARE curint3 CURSOR FOR
   SELECT "x"
     FROM cta_interes_viv
    WHERE subcuenta in (4,8)
      AND tipo_movimiento = 3
      AND fecha_conversion = v.vfecha
   OPEN curint3
   FETCH curint3
   IF STATUS = NOTFOUND THEN
      ERROR "NO EXISTE ESTA FECHA PARA INTEGRACION"
      SLEEP 3
      CLEAR SCREEN
      CLOSE WINDOW  ventana_21
      CLOSE curint3
      RETURN
   END IF
   CLOSE curint3

   DECLARE curint CURSOR FOR
   SELECT nss,
          subcuenta,
          fecha_valor,
          sum(monto_en_pesos)
   FROM cta_interes_viv
   GROUP BY 1,2,3
   ORDER BY 1,2,3        

  ERROR "PROCESANDO INFORMACION..."

  FOREACH curint INTO g_int.*

     LET vid_aportante   = "INFONAVIT"

     IF g_int.fecha_valor = "04/01/1999" THEN
        LET vmonto_nor = g_int.monto_en_pesos / 46.2096 * 12.7890
        LET vmonto_rem = g_int.monto_en_pesos / 46.2096 * 33.4206
                                                                       
        INSERT INTO dis_cuenta VALUES
           (
           3,                       # tipo_movimiento
           g_int.subcuenta,         # subcuenta
           0,                       # siefore
           88888,                   # folio
           g_int.nss,               # nss
           NULL,                    # curp
           0,                       # folio_sua
           g_int.fecha_valor,       # fecha_pago
           g_int.fecha_valor,       # fecha_valor
           v.vfecha,                # fecha_conversion
           vmonto_nor,              # monto_en_pesos
           0,                       # monto_en_acciones
           0,                       # precio_accion
           0,                       # dias_cotizados
           NULL,                    # sucursal
           "INFONAVIT",              # id_aportante
           5,                       # estado
           TODAY,                   # fecha_proceso
           usuario,                 # usiario
           TODAY,                   # fecha_archivo
           1)                       # etiqueta
        INSERT INTO dis_cuenta VALUES
           (
           3,                       # tipo_movimiento
           g_int.subcuenta,         # subcuenta
           0,                       # siefore
           88888,                   # folio
           g_int.nss,               # nss
           NULL,                    # curp
           0,                       # folio_sua
           g_int.fecha_valor,       # fecha_pago
           g_int.fecha_valor,       # fecha_valor
           v.vfecha,                # fecha_conversion
           vmonto_rem,              # monto_en_pesos
           0,                       # monto_en_acciones
           0,                       # precio_accion
           0,                       # dias_cotizados
           NULL,                    # sucursal
           "REMANENTE",              # id_aportante
           5,                       # estado
           TODAY,                   # fecha_proceso
           usuario,                 # usiario
           TODAY,                   # fecha_archivo
           1)                       # etiqueta
                                                                  
        LET vmonto_nor = 0
        LET vmonto_rem = 0
     ELSE                        
        IF g_int.fecha_valor = "05/01/2000" THEN
           LET vmonto_nor = g_int.monto_en_pesos / 27.6924 * 9.1121 
           LET vmonto_rem = g_int.monto_en_pesos / 27.6924 * 18.5803

           INSERT INTO dis_cuenta VALUES(
              3,                       # tipo_movimiento
              g_int.subcuenta,         # subcuenta
              0,                       # siefore
              88888,                   # folio
              g_int.nss,               # nss
              NULL,                    # curp
              0,                       # folio_sua
              g_int.fecha_valor,       # fecha_pago
              g_int.fecha_valor,       # fecha_valor
              v.vfecha,                # fecha_conversion
              vmonto_nor,              # monto_en_pesos
              0,                       # monto_en_acciones
              0,                       # precio_accion
              0,                       # dias_cotizados
              NULL,                    # sucursal
              "INFONAVIT",              # id_aportante
              5,                       # estado
              TODAY,                   # fecha_proceso
              usuario,                 # usiario
              TODAY,                   # fecha_archivo
              1)                       # etiqueta
           INSERT INTO dis_cuenta VALUES(
              3,                       # tipo_movimiento
              g_int.subcuenta,         # subcuenta
              0,                       # siefore
              88888,                   # folio
              g_int.nss,               # nss
              NULL,                    # curp
              0,                       # folio_sua
              g_int.fecha_valor,       # fecha_pago
              g_int.fecha_valor,       # fecha_valor
              v.vfecha,                # fecha_conversion
              vmonto_rem,              # monto_en_pesos
              0,                       # monto_en_acciones
              0,                       # precio_accion
              0,                       # dias_cotizados
              NULL,                    # sucursal
              "REMANENTE",              # id_aportante
              5,                       # estado
              TODAY,                   # fecha_proceso
              usuario,                 # usiario
              TODAY,                   # fecha_archivo
              1)                       # etiqueta
           LET vmonto_nor = 0
           LET vmonto_rem = 0
        ELSE
           INSERT INTO dis_cuenta VALUES(
              3,                       # tipo_movimiento
              g_int.subcuenta,         # subcuenta
              0,                       # siefore
              88888,                   # folio
              g_int.nss,               # nss
              NULL,                    # curp
              0,                       # folio_sua
              g_int.fecha_valor,       # fecha_pago
              g_int.fecha_valor,       # fecha_valor
              v.vfecha,                # fecha_conversion
              g_int.monto_en_pesos,    # monto_en_pesos
              0,                       # monto_en_acciones
              0,                       # precio_accion
              0,                       # dias_cotizados
              NULL,                    # sucursal
              vid_aportante,            # id_aportante
              5,                       # estado
              TODAY,                   # fecha_proceso
              usuario,                 # usiario
              TODAY,                   # fecha_archivo
              1)                       # etiqueta
        END IF
     END IF

  END FOREACH

  WHENEVER ERROR CONTINUE  
     DROP TABLE cuentasint;

     CREATE TABLE "informix".cuentasint 
       (
         tipo_movimiento integer,
         subcuenta integer,
         siefore smallint,
         folio decimal(10,0),
         nss char(11),
         curp char(18),
         folio_sua char(6),
         fecha_pago date,
         fecha_valor date,
         fecha_conversion date,
         monto_en_pesos decimal(16,6),
         monto_en_acciones decimal(16,6),
         precio_accion decimal(16,6),
         dias_cotizados integer,
         sucursal char(10),
         id_aportante char(11),
         estado smallint,
         fecha_proceso date,
         usuario char(8),
         fecha_archivo date,
         etiqueta integer
       );
     revoke all on "informix".cuentasint from "public";

     create index "informix".cuentasint_1 on "informix".cuentasint 
        (nss,subcuenta,tipo_movimiento,fecha_valor);
     
     create index "informix".cuentasint_2 on "informix".cuentasint 
        (subcuenta,tipo_movimiento,fecha_valor,etiqueta);

  WHENEVER ERROR STOP
 
  ERROR ""
  CLEAR SCREEN
  CLOSE WINDOW  ventana_21
END FUNCTION                                          

FUNCTION respaldo_888()
   DEFINE
      vfecha       DATE,
      vchar,vchar2 CHAR(99),
      vprograma    CHAR(18)

   CLEAR SCREEN


   PROMPT "Fecha archivo,igual fecha aplica calculo int. [MM-DD-YYYY] ... " FOR vfecha
  
   ERROR "PROCESANDO INFORMACION"

   LET g_lista = g_param_dis.ruta_envio CLIPPED,"/ctas888"

----   UNLOAD TO "/respprod/INTVIV/ctas888"
   UNLOAD TO g_lista
   SELECT * FROM dis_cuenta                                        
   WHERE subcuenta in (4,8) AND tipo_movimiento = 888 
   
   LET vchar = "mv ",g_param_dis.ruta_envio CLIPPED,"/ctas888 ",
                     g_param_dis.ruta_envio CLIPPED,"/ctas888.",
                     vfecha USING "dd-mm-yyyy"

--   LET vchar="mv /respprod/INTVIV/ctas888 /respprod/INTVIV/ctas888." CLIPPED,
--              vfecha USING "dd-mm-yyyy"
  RUN vchar

   ERROR "PORCESO TERMINADO"
   SLEEP 2
   ERROR ""

END FUNCTION

FUNCTION Intereses_retroactivos()
   DEFINE pos                INTEGER

   DECLARE cursor_7 CURSOR FOR

   SELECT fecha_valor,
          tipo_movimiento,
          sum(monto_en_pesos)
   FROM   safre_tmp:cta_interes_viv
   WHERE  subcuenta in (4,8)
   AND    tipo_movimiento = 3
   GROUP  BY 1,2
   ORDER  BY 1,2

   ERROR "PROCESANDO INFORMACION"

   LET pos = 1
   FOREACH cursor_7 INTO l_record6[pos].*
     LET pos = pos + 1
   END FOREACH
                                                                 
   INITIALIZE l_record6[pos].* TO NULL

   CLEAR SCREEN

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      ERROR ""
      OPEN WINDOW ventana_6 AT 6,4 WITH FORM "DISB0155" ATTRIBUTE( BORDER)
      DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
      DISPLAY " (Ctrl-C) Salir          (Ctrl-P) Imprimir           (Ctrl-F) Archivo          " AT 1,1 ATTRIBUTE(REVERSE,green)
      DISPLAY "              Consulta de Intereses Retroactivos de Vivienda                   " AT 3,1 ATTRIBUTE(REVERSE,green)
      DISPLAY ARRAY l_record6 TO scr_1.*
         ON KEY (Control-f)
            ERROR "PROCESANDO LISTADO..."
            CALL archivo6(pos)
         ON KEY (Control-p)
            ERROR "PROCESO IMPRESION..."
            CALL imprimir6(pos)
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY

      CLEAR WINDOW ventana_6
      CLOSE WINDOW ventana_6
   ELSE
      ERROR "ARCHIVO ... VACIO"
      SLEEP 2
      ERROR ""
   END IF
END FUNCTION                 

#Reporte de archivo----------------------------------------------------------
FUNCTION archivo6(pos)
   DEFINE pos INTEGER

   LET hora = TIME
   LET G_LISTA = g_param_dis.ruta_spool CLIPPED,"/",
                 "INTRETVIV"

   START REPORT rpt_cuenta6_arc TO G_LISTA
   CALL cuentas_rpt6_arc(pos)

END FUNCTION              

FUNCTION cuentas_rpt6_arc(pos)
   DEFINE i,pos INTEGER

   FOR i=1 TO (pos+1)
      LET g.fecha_valor     = l_record6[i].fecha_valor
      LET g.tipo_movimiento = l_record6[i].tipo_movimiento
      LET g.monto_en_pesos  = l_record6[i].monto_en_pesos

      IF g.fecha_valor IS NULL OR g.fecha_valor="12/31/1899" THEN
          EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_cuenta6_arc(g.*)
   END FOR

   FINISH REPORT rpt_cuenta6_arc
   ERROR "LISTADO GENERADO"
   SLEEP 2
   ERROR ""
                                          
  LET G_LISTA = "chmod 777 ",g_param_dis.ruta_spool CLIPPED,"/",
                   "INTRETVIV"
   RUN G_LISTA

END FUNCTION                          

REPORT rpt_cuenta6_arc(g)
   DEFINE g RECORD LIKE cta_interes_viv.*

   FORMAT
   PAGE HEADER
       PRINT COLUMN 5, "FEC.VAL.",
             COLUMN 22, "TIP.MOVM.",
             COLUMN 42, "PESOS"
       SKIP 1 LINE
   ON EVERY ROW
       PRINT COLUMN 5,g.fecha_valor USING "dd-mm-yyyy",
             COLUMN 22,g.tipo_movimiento,
             COLUMN 42,g.monto_en_pesos
END REPORT                             

#Reporte de impresion--------------------------------------------------------
FUNCTION imprimir6(pos)
   DEFINE pos INTEGER

   LET hora = TIME

   LET G_IMPRE = g_param_dis.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
                               ".LIS_INT_RET_VIV_",HOY USING "DD-MM-YYYY",
                                 "_",hora CLIPPED

   START REPORT rpt_cuenta6_imp TO G_IMPRE
   CALL cuentas_rpt6_imp(pos)

   LET gimpresion = "lp ",G_IMPRE
   RUN gimpresion

END FUNCTION                                  

FUNCTION cuentas_rpt6_imp(pos)
   DEFINE i,pos INTEGER

   FOR i=1 TO (pos+1)
      LET g.fecha_valor     = l_record6[i].fecha_valor
      LET g.tipo_movimiento = l_record6[i].tipo_movimiento
      LET g.monto_en_pesos  = l_record6[i].monto_en_pesos

      IF g.fecha_valor IS NULL OR g.fecha_valor="12/31/1899" THEN
             EXIT FOR
      END IF
      OUTPUT TO REPORT rpt_cuenta6_imp(g.*)
   END FOR

   FINISH REPORT rpt_cuenta6_imp
   ERROR "LISTADO GENERADO"
   SLEEP 2
   ERROR ""
END FUNCTION                               

REPORT rpt_cuenta6_imp(g)
   DEFINE g RECORD LIKE cta_interes_viv.*

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60
   FORMAT
   PAGE HEADER
      PRINT COLUMN 02,"DISB015",
            COLUMN 20,"LISTADO DE INTERESES RETROACTIVOS DE VIVIENDA",
            COLUMN 65, TODAY USING "mm/dd/yy"
      SKIP 2 LINE
      PRINT COLUMN 17, "FEC.VAL.",
            COLUMN 30, "TIP.MOVM.",
            COLUMN 52, "PESOS"
      SKIP 1 LINE                                                
   ON EVERY ROW
      PRINT COLUMN 17,g.fecha_valor,
            COLUMN 30,g.tipo_movimiento,
            COLUMN 52,g.monto_en_pesos
   PAGE TRAILER
      SKIP 2 LINES
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
         PAUSE "Presione enter para continuar...."
   ON LAST ROW
      SKIP 3 LINES
      PRINT COLUMN 2, "Total de registros encontrados: ",
         COUNT(*) USING "<<<<"
END REPORT                                                  

FUNCTION ultimo_dia_mes(fecha)              
   DEFINE                                   
      fecha DATE,                           
      dias  INTEGER                         
                                            
   CASE MONTH(fecha)                        
       WHEN  1 LET dias=31                  
       WHEN  2 IF YEAR(fecha) MOD 4 = 0 THEN
                  LET dias = 29             
               ELSE                         
                  LET dias = 28             
               END IF                       
       WHEN  3 LET dias=31                  
       WHEN  4 LET dias=30                  
       WHEN  5 LET dias=31                  
       WHEN  6 LET dias=30                  
       WHEN  7 LET dias=31                  
       WHEN  8 LET dias=31                  
       WHEN  9 LET dias=30                  
       WHEN 10 LET dias=31                  
       WHEN 11 LET dias=30                  
       WHEN 12 LET dias=31                  
   END CASE

   RETURN dias
END FUNCTION  
