################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Progrma  DISB018  => INTERESES POR SUBCUENTA                                  #
#Fecha             => 07 DE JULIO 1999. 	       			       #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ. 			       #
#Sistema           => DIS.   				               #
################################################################################
DATABASE safre_af 

GLOBALS
    DEFINE g RECORD LIKE dis_cuenta.*
    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
    DEFINE HOY			DATE

    DEFINE g_usuario      CHAR (08)
    DEFINE g_param_dis    RECORD LIKE dis_parametro.*
    DEFINE G_LISTA                  CHAR(300)
    DEFINE G_IMPRE                  CHAR(300)
    DEFINE gimpresion               CHAR(300)
    DEFINE hora                     CHAR (08)
   
    DEFINE   v         RECORD
           fecha DATE
    END RECORD

    DEFINE l_record   ARRAY[300] OF RECORD
            folio                 INTEGER,                                
            fecha_valor           DATE,
            fecha_conversion      DATE,
            subcuenta             INTEGER,
            tipo_movimiento       INTEGER,
            monto_en_pesos        DECIMAL(16,6) 
    END RECORD

END GLOBALS

MAIN
    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY control-o

    DEFER INTERRUPT

    CALL inicio()
END MAIN

FUNCTION inicio()
    SELECT  codigo_afore,USER
    INTO    w_codigo_afore,g_usuario
    FROM    tab_afore_local

    SELECT  ruta_spool
    INTO    g_param_dis.ruta_spool
    FROM    dis_parametro

    LET HOY = TODAY

    OPEN WINDOW ventana_1 AT 3,4 
    WITH 3 ROWS,72 COLUMNS
    ATTRIBUTE( BORDER)
    DISPLAY " DISB018 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)
	
    MENU "MENU "
      COMMAND "INTERESES POR SUBCUENTA"
        CALL proceso_principal()
      COMMAND "Salir" "Salir del Programa"
        EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION proceso_principal()
    DEFINE pos                     INTEGER   
 
    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 10,4 WITH FORM "DISB0182" ATTRIBUTE( BORDER)
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
       ERROR "Fecha no aceptado."
       SLEEP 2
       ERROR ""
       CLEAR SCREEN
       CLOSE WINDOW ventana_21
       RETURN
    END IF

    ERROR "PROCESANDO INFORMACION"

    DECLARE cursor_1 CURSOR FOR

    SELECT  folio,
            fecha_valor,
            fecha_conversion,
            subcuenta,
            tipo_movimiento,
            sum(monto_en_pesos)
    FROM  dis_cuenta
    WHERE subcuenta IN (4,8)
    AND   tipo_movimiento = 3
    AND   fecha_conversion = v.fecha
    GROUP BY 1,2,3,4,5
    ORDER BY 2,3,4

    LET pos = 1

    FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
    END FOREACH

    INITIALIZE l_record[pos].* TO NULL

    CLOSE WINDOW ventana_21

    IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      ERROR ""
      OPEN WINDOW ventana_2 AT 6,4 WITH FORM "DISB0181" ATTRIBUTE( BORDER)
      DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
      DISPLAY " (Ctrl-C) Salir            (Ctrl-P) Imprimir           (Ctrl-F) Archivo  " AT 1,1 ATTRIBUTE(REVERSE,green)
      DISPLAY " DISB018                    INTERESES POR SUBCUENTA                           " AT 3,1 ATTRIBUTE (REVERSE,green)
   	
      DISPLAY ARRAY l_record TO scr_1.*
         ON KEY (Control-p)
            ERROR "PROCESANDO IMPRESION..."
            CALL imprimir()
         ON KEY (Control-f)
            ERROR "PROCESANDO LISTADO..."
            CALL archivo()
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


FUNCTION archivo()

    LET hora = TIME
    LET G_LISTA = g_param_dis.ruta_spool CLIPPED,"/",
                                     "INT.",
                               HOY USING "DD-MM-YYYY"
    START REPORT rpt_cuenta_arc TO G_LISTA
    CALL cuentas_archi()

END FUNCTION
 
FUNCTION cuentas_archi()
    DEFINE i INTEGER

    FOR i=1 TO 300
      LET g.folio           = l_record[i].folio
      LET g.fecha_valor     = l_record[i].fecha_valor 
      LET g.fecha_conversion= l_record[i].fecha_conversion
      LET g.subcuenta       = l_record[i].subcuenta
      LET g.tipo_movimiento = l_record[i].tipo_movimiento
      LET g.monto_en_pesos  = l_record[i].monto_en_pesos

      IF g.folio IS NULL OR g.folio < 0 THEN
           EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_cuenta_arc(g.*)
    END FOR

    FINISH REPORT rpt_cuenta_arc

    ERROR "LISTADO GENERADO" 
    SLEEP 2
    ERROR ""

    LET G_LISTA = "chmod 777 ",g_param_dis.ruta_spool CLIPPED,
                       "/","IN.",
                      HOY USING "DD-MM-YYYY"
    RUN G_LISTA

END FUNCTION

REPORT rpt_cuenta_arc(g)
    DEFINE g RECORD LIKE dis_cuenta.*

    FORMAT
     PAGE HEADER
       PRINT COLUMN 0,"FOLIO",
             COLUMN 08,"F.VALOR",  
             COLUMN 20,"F.CONVER",
             COLUMN 30,"SUBCTA",
             COLUMN 40,"TIP.MOV.",
             COLUMN 54,"MONTO EN PESOS"
       SKIP 1 LINE

     ON EVERY ROW
       PRINT COLUMN 0,g.folio USING "&&&&&",                
             COLUMN 08,g.fecha_valor USING "DD-MM-YYYY",      
             COLUMN 20,g.fecha_conversion USING "DD-MM-YYYY",
             COLUMN 32,g.subcuenta USING "##",
             COLUMN 42,g.tipo_movimiento USING "###",
             COLUMN 51,g.monto_en_pesos

END REPORT

FUNCTION imprimir()

    LET hora = TIME

    LET G_IMPRE = g_param_dis.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
                              ".LIS_INT_SUB_",HOY USING "DD-MM-YYYY",
                                "_",hora CLIPPED

    START REPORT rpt_cuenta_imp TO G_IMPRE

    CALL cuentas_impri()

    LET gimpresion = "lp ",G_IMPRE   
    ---- TPY LET gimpresion = "lp -dafoafi_1 ",G_IMPRE   
    RUN gimpresion

END FUNCTION

FUNCTION cuentas_impri()
    DEFINE i INTEGER

    FOR i=1 TO 300

      LET g.folio           = l_record[i].folio
      LET g.fecha_valor     = l_record[i].fecha_valor
      LET g.fecha_conversion= l_record[i].fecha_conversion
      LET g.subcuenta       = l_record[i].subcuenta
      LET g.tipo_movimiento = l_record[i].tipo_movimiento
      LET g.monto_en_pesos  = l_record[i].monto_en_pesos
     
      IF g.folio IS NULL OR g.folio < 0 THEN
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
        PRINT COLUMN 12,"INTERESE RETROACTIVOS POR SUBCUENTA",
              COLUMN 65, TODAY USING "mm/dd/yyyy"
        SKIP 2 LINE

          PRINT COLUMN 0,"FOLIO",
             COLUMN 07,"FEC.VALOR",  
             COLUMN 20,"FEC.CONVER.",
             COLUMN 30,"SUBCUENTA",
             COLUMN 40,"TIP.MOVIM.",
             COLUMN 54,"MONTO EN PESOS"
          SKIP 1 LINE

      ON EVERY ROW
          PRINT COLUMN 2,g.folio,
             COLUMN 08,g.fecha_valor ,
             COLUMN 19,g.fecha_conversion,
             COLUMN 35,g.subcuenta,
             COLUMN 45,g.tipo_movimiento,
             COLUMN 51,g.monto_en_pesos
          SKIP 1 LINE

      PAGE TRAILER
        SKIP 2 LINES
        PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
        PAUSE "Presione enter para continuar...."

      ON LAST ROW
        SKIP 4 LINES
        PRINT COLUMN 2, "Total de registros encontrados: ",
        COUNT(*) USING "<<<<"

END REPORT
