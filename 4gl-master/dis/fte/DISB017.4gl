################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			                   #
#Owner             => E.F.P.        			    	                            #
#Programa DISB017  => Cifras Control Liquidacion dis_cuenta                    #
#Fecha             => 27 DE MAYO 1999 . 	       			                      #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ. 			                #
#Sistema           => DIS. 					                                     #
--------------------------------------------------------------------------------
#Fecha ini modif   => 06 diciembre 2004                                        #
#Fecha fni modif   =>                                                          #
#Autor             => ALEJANDRO RAMIREZ                                        #
#Descripcion       => Adecuación layout (031001) (031002) (031003) (031008)    #
#                     (031009) de acuerdo a circular 22-6 multi siefore,       #
#                     utilizacion de preparados para utilizar multiseifore     #
#                     para dispersion y comision                               #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE g RECORD LIKE dis_cuenta.*
    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
    DEFINE HOY			DATE

    DEFINE g_usuario      CHAR (08)
    DEFINE g_param_dis    RECORD LIKE seg_modulo.*
    DEFINE G_LISTA                  CHAR(300)
    DEFINE G_IMPRE                  CHAR(300)
    DEFINE impresion               CHAR(300)
    DEFINE hora                     CHAR (08)

    DEFINE v RECORD
              folio              INTEGER
    END RECORD

    DEFINE l_record   ARRAY[300] OF RECORD
            siefore               SMALLINT,  --c22-6
            tipo_movimiento       INTEGER,
            subcuenta             INTEGER,
            monto_en_pesos        DECIMAL(16,6),
            monto_en_acciones     DECIMAL(16,6)
    END RECORD

END GLOBALS

MAIN
    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY control-o

    DEFER INTERRUPT

    SELECT  codigo_afore,USER
    INTO    w_codigo_afore,g_usuario
    FROM    tab_afore_local

    SELECT  ruta_listados
    INTO    g_param_dis.ruta_listados
    FROM    seg_modulo
    WHERE   modulo_cod='dis'

    LET HOY = TODAY

    OPEN WINDOW ventana_1 AT 3,4 
    WITH 3 ROWS,72 COLUMNS
    ATTRIBUTE( BORDER)
    DISPLAY " DISB017 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)
	
    MENU "MENU "
      COMMAND "Cifras de Control." 
        CALL Consulta()
      COMMAND "Salir" "Salir del Programa"
        EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1

END MAIN

FUNCTION Consulta()
    DEFINE pos                     INTEGER   

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 10,4 WITH FORM "DISB0172" ATTRIBUTE( BORDER)
    DISPLAY "              (Ctrl-C) Salir          (ESC) Ejecutar                           " AT 1,1 ATTRIBUTE(REVERSE,green)

    LET int_flag= FALSE

    INPUT BY NAME v.folio
      ON KEY (ESC)
        LET int_flag =FALSE
        ERROR "PROCESANDO INFORMACION...."
        EXIT INPUT
      ON KEY (control-c)
        LET int_flag = TRUE
        EXIT INPUT
    END INPUT
   
    IF int_flag = TRUE THEN
       LET int_flag = FALSE
       CLEAR FORM
       ERROR "Folio no aceptado..."
       SLEEP 2
       ERROR ""
       CLEAR SCREEN
       CLOSE WINDOW ventana_21
       RETURN
    END IF

    DECLARE cursor_1 CURSOR FOR

    SELECT    siefore,                --c22-6
              tipo_movimiento,
              subcuenta,
              sum(monto_en_pesos),
              sum(monto_en_acciones)
    FROM  dis_cuenta
    WHERE folio = v.folio
    GROUP BY 1,2,3                                                   
    ORDER BY 3,1,2

    LET pos = 1

    FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
    END FOREACH

    INITIALIZE l_record[pos].* TO NULL

    CLOSE WINDOW ventana_21

    IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      ERROR ""
      OPEN WINDOW ventana_2 AT 6,4 WITH FORM "DISB0171" ATTRIBUTE( BORDER)
      DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
      DISPLAY " (Ctrl-C) Salir          (Ctrl-P) Imprimir        (Ctrl-F) Archivo             " AT 1,1 ATTRIBUTE(REVERSE,green)
      DISPLAY "           Cifras de Control de Archivo de Liquidacion / AFORE                  " AT 3,1 ATTRIBUTE(REVERSE,green) 
   	
      DISPLAY ARRAY l_record TO scr_1.*
         ON KEY (Control-p)
            ERROR "PROCESANDO IMPRESION...."
            CALL imprimir()
         ON KEY (Control-f)
            ERROR "PROCESANDO LISTADO...."
            CALL archivo()
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY

      CLEAR WINDOW ventana_2
      CLOSE WINDOW ventana_2
    ELSE
      ERROR "ARCHIVO ... VACIO."
      SLEEP 2
      ERROR ""
    END IF 

END FUNCTION


FUNCTION archivo()

    LET hora = TIME
    LET G_LISTA = g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                                                       ".ARC_CIF_DIS_",
                               HOY USING "DD-MM-YYYY","_",hora CLIPPED
    START REPORT rpt_cuenta_arc TO G_LISTA
    CALL cuentas_archi()

END FUNCTION
 
FUNCTION cuentas_archi()
    DEFINE i INTEGER

    FOR i=1 TO 300
      LET g.siefore           = l_record[i].siefore       --c22-6
      LET g.tipo_movimiento   = l_record[i].tipo_movimiento
      LET g.subcuenta         = l_record[i].subcuenta
      LET g.monto_en_pesos    = l_record[i].monto_en_pesos
      LET g.monto_en_acciones = l_record[i].monto_en_acciones

      IF g.tipo_movimiento IS NULL OR g.tipo_movimiento = 0 THEN
           EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_cuenta_arc(g.*)
    END FOR

    FINISH REPORT rpt_cuenta_arc

    ERROR "ARCHIVO GENERADO...." 
    SLEEP 2
    ERROR ""

    LET G_LISTA = "chmod 777 ",g_param_dis.ruta_listados CLIPPED,
                       "/",g_usuario CLIPPED,".ARC_CIF_DIS_",
                      HOY USING "DD-MM-YYYY","_",hora CLIPPED
    RUN G_LISTA

END FUNCTION

REPORT rpt_cuenta_arc(g)
    DEFINE g RECORD LIKE dis_cuenta.*

    FORMAT
     PAGE HEADER
       PRINT COLUMN 02, "SIEF.",     --c22-6 
             COLUMN 15, "MOV.",
             COLUMN 20, "SUB.",
             COLUMN 23, "MONTO EN PESOS",
             COLUMN 40, "MONTO EN ACCIONES"
       SKIP 1 LINE

     ON EVERY ROW
       PRINT COLUMN 02,g.siefore USING "&&&",      --c22-6
             COLUMN 07,g.tipo_movimiento USING "#####",
             COLUMN 13,g.subcuenta USING "#####",
             COLUMN 20,g.monto_en_pesos,
             COLUMN 40,g.monto_en_acciones

END REPORT

FUNCTION imprimir()

    LET hora = TIME

    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                              ".LIS_CIF_DIS_",HOY USING "DD-MM-YYYY",
                                "_",hora CLIPPED

    START REPORT rpt_cuenta_imp TO G_IMPRE

    CALL cuentas_impri()

    LET impresion = "lp ",G_IMPRE
    ---- TPY LET impresion = "lp -dafoafi_1 ",G_IMPRE
    RUN impresion

END FUNCTION

FUNCTION cuentas_impri()
    DEFINE i INTEGER

    FOR i=1 TO 300
      LET g.siefore           = l_record[i].siefore    --c22-6
      LET g.tipo_movimiento   = l_record[i].tipo_movimiento
      LET g.subcuenta         = l_record[i].subcuenta
      LET g.monto_en_pesos    = l_record[i].monto_en_pesos
      LET g.monto_en_acciones = l_record[i].monto_en_acciones

      IF g.tipo_movimiento IS NULL OR g.tipo_movimiento = 0 THEN
            EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_cuenta_imp(g.*)
    END FOR

    FINISH REPORT rpt_cuenta_imp

    ERROR "LISTADO GENERADO...."
    SLEEP 2
    ERROR ""

END FUNCTION

REPORT rpt_cuenta_imp(g)
    DEFINE g RECORD LIKE dis_cuenta.*
   DEFINE vident_arch CHAR(01),
          vtipo_arch  CHAR(20)

    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT
      PAGE HEADER
        SELECT ident_arch
        INTO   vident_arch
        FROM   dis_cza_aporte
        WHERE  folio = v.folio

        CASE
           WHEN vident_arch = "0"
              LET vtipo_arch = "DISPERSION"
           WHEN vident_arch = "1"
              LET vtipo_arch = "ACLARACION ORDINARIA"
           WHEN vident_arch = "2"
              LET vtipo_arch = "ACLARACION ESPECIAL"
           WHEN vident_arch = "3"
              LET vtipo_arch = "ASIGNACION"
           WHEN vident_arch = "4"
              LET vtipo_arch = "SEPARACION CUENTAS"
        END CASE

        PRINT COLUMN 02,"DISB017",
              COLUMN 12,"CIFRAS CONTROL LIQUIDACION DE ",vtipo_arch CLIPPED,
              COLUMN 65, TODAY USING "mm/dd/yyyy"
        SKIP 2 LINE

        PRINT COLUMN 02, "FOLIO: ",v.folio USING "----------"
        PRINT

        PRINT COLUMN 02, "SIEF.",                          --c22-6
              COLUMN 15, "MOV.",
              COLUMN 20, "SUB.",
              COLUMN 25, "MONTO EN PESOS",
              COLUMN 40, "MONTO EN ACCIONES"
        SKIP 1 LINE

      ON EVERY ROW
        PRINT COLUMN 02,g.siefore USING "&&&",              --c22-6
              COLUMN 13,g.tipo_movimiento USING "#####",
              COLUMN 15,g.subcuenta USING "#####",
              COLUMN 23,g.monto_en_pesos,
              COLUMN 40,g.monto_en_acciones
      
      PAGE TRAILER
        SKIP 2 LINES
        PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
        PAUSE "Presione enter para continuar...."

      ON LAST ROW
        SKIP 4 LINES
        PRINT COLUMN 2, "Total de registros encontrados: ",
        COUNT(*) USING "<<<<"

END REPORT
