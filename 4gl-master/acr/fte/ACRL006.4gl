###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )		          #
#Owner             => E.F.P.        				          #
#Fecha             => 7 DE JUNIO 1999 . 	     		          #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ. 		          #
#Sistema           => TRA. 					          #
#Programa          => ACRL006                                             #
###########################################################################
DATABASE safre_af
GLOBALS

    DEFINE g RECORD LIKE dis_cuenta.*
    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
    DEFINE HOY			DATE

    DEFINE g_usuario      CHAR (08)
    DEFINE g_param_dis    RECORD LIKE seg_modulo.*
    DEFINE G_LISTA                  CHAR(300)
    DEFINE G_IMPRE                  CHAR(300)
    DEFINE gimpresion               CHAR(300)
    DEFINE hora                     CHAR (08)

    DEFINE v RECORD
            vfolio                  INTEGER   
    END RECORD
    
    DEFINE l_record   ARRAY[300] OF RECORD
              folio                 INTEGER,
              tipo_movimiento       INTEGER,
              subcuenta             INTEGER,
              fecha_valor           DATE,
              fecha_conversion      DATE,
              monto_en_acciones     DECIMAL(16,6),
              monto_en_pesos        DECIMAL(16,6)
    END RECORD

END GLOBALS

MAIN
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
	ACCEPT KEY control-o
	DEFER INTERRUPT

    CALL inicio()
    CALL proceso_principal()
END MAIN

FUNCTION inicio()

    SELECT  codigo_afore,USER
    INTO    w_codigo_afore,g_usuario
    FROM    tab_afore_local

    SELECT  *
    INTO    g_param_dis.*
    FROM    seg_modulo
    WHERE   modulo_cod = 'acr'

    LET HOY = TODAY
END FUNCTION

FUNCTION proceso_principal()

    OPEN WINDOW ventana_1 AT 3,4  WITH 3 ROWS,72 COLUMNS ATTRIBUTE( BORDER)
    DISPLAY " ACRL006 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)
	
    MENU "MENU "
       COMMAND "Consulta " "Consulta de Liquidacion."
           CALL Consulta()
       COMMAND "Salir" "Salir del Programa"
           EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1
 
END FUNCTION

FUNCTION Consulta()
    DEFINE pos                     SMALLINT   

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 10,4 WITH FORM "ACRL0022" ATTRIBUTE( BORDER)
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
       ERROR "Folio no aceptado."
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
           sum(monto_en_acciones),
           sum(monto_en_pesos)                                    
    FROM dis_cuenta                                                         
    WHERE folio = v.vfolio 
    AND  subcuenta  in (4,8)
    AND  tipo_movimiento in (236)   
    GROUP BY 1,2,3,4,5                                                   
    ORDER BY 1,2 ,3

    LET pos = 1

    FOREACH cursor_1 INTO l_record[pos].*
       LET pos = pos + 1
    END FOREACH

    INITIALIZE l_record[pos].* TO NULL

    CLOSE WINDOW ventana_21

    IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      ERROR ""
      OPEN WINDOW ventana_2 AT 6,4 WITH FORM "ACRL0021" ATTRIBUTE( BORDER)
      DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
      DISPLAY " (Ctrl-C) Salir            (Ctrl-P) Imprimir           (Ctrl-F) Archivo  " AT 1,1 ATTRIBUTE(REVERSE,green)
      DISPLAY " ACRL006                  Consulta de Liquidacion                          " AT 3,1 ATTRIBUTE(REVERSE,green) 
      DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE,green)
   	
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
#Reporte de archivo------------------------------------------------------------
FUNCTION archivo()

    LET hora = TIME
    LET G_LISTA = g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                    ".LIQ_ACR_", HOY USING "DDMMYY","_",hora CLIPPED

    START REPORT rpt_cuenta_arc TO G_LISTA
    CALL cuentas_archi()

END FUNCTION
 
FUNCTION cuentas_archi()
    DEFINE i INTEGER

    FOR i=1 TO 300
       LET g.folio                = l_record[i].folio
       LET g.tipo_movimiento      = l_record[i].tipo_movimiento
       LET g.subcuenta            = l_record[i].subcuenta
       LET g.fecha_valor          = l_record[i].fecha_valor
       LET g.fecha_conversion     = l_record[i].fecha_conversion
       LET g.monto_en_acciones    = l_record[i].monto_en_acciones
       LET g.monto_en_pesos       = l_record[i].monto_en_pesos

       IF g.fecha_valor IS NULL OR g.fecha_valor="12/31/1899" THEN
           EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_cuenta_arc(g.*)
    END FOR

    FINISH REPORT rpt_cuenta_arc
    ERROR "LISTADO GENERADO" 
    SLEEP 2
    ERROR ""

    LET G_LISTA = "chmod 777 ",g_param_dis.ruta_listados CLIPPED,
                          "/",g_usuario CLIPPED,".LIQ_ACR_",
                          HOY USING "DDMMYY","_",hora CLIPPED
    RUN G_LISTA

END FUNCTION

REPORT rpt_cuenta_arc(g)
    DEFINE g RECORD LIKE dis_cuenta.*

    FORMAT
      PAGE HEADER
        PRINT COLUMN 30,"LISTADO DE LIQUIDACION AFORE/INFONAVIT(TRANS.ACR)"
        PRINT COLUMN 2,"FOL.",
              COLUMN 7,"MOV.",
              COLUMN 15, "SUB.",
              COLUMN 27, "FEC.VAL.",
              COLUMN 40, "FEC.CONV.",
              COLUMN 52, "ACCIONES",
              COLUMN 72, "PESOS"
        SKIP 1 LINE

      ON EVERY ROW
        PRINT COLUMN  2,g.folio USING "#####",
              COLUMN 7, g.tipo_movimiento USING "#####",
              COLUMN 15,g.subcuenta USING "#####",
              COLUMN 27,g.fecha_valor USING "dd-mm-yyyy",
              COLUMN 40,g.fecha_conversion USING "dd-mm-yyyy",
              COLUMN 52,g.monto_en_acciones,
              COLUMN 72,g.monto_en_pesos 

END REPORT
#Reporte de impresora----------------------------------------------------------
FUNCTION imprimir()

    LET hora = TIME

    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
          ".LIS_LIQ_ACR_",HOY USING "DDMMYY", "_",hora CLIPPED

    START REPORT rpt_cuenta_imp TO G_IMPRE
    CALL cuentas_impri()

    LET gimpresion = "lp ",G_IMPRE
    RUN gimpresion

END FUNCTION

FUNCTION cuentas_impri()
    DEFINE i INTEGER

    FOR i=1 TO 300
       LET g.folio                = l_record[i].folio
       LET g.tipo_movimiento      = l_record[i].tipo_movimiento
       LET g.subcuenta            = l_record[i].subcuenta
       LET g.fecha_valor          = l_record[i].fecha_valor
       LET g.fecha_conversion     = l_record[i].fecha_conversion
       LET g.monto_en_acciones    = l_record[i].monto_en_acciones
       LET g.monto_en_pesos       = l_record[i].monto_en_pesos

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
       PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
       PRINT COLUMN 2,"ACRL006",
             COLUMN 30,"LISTADO DE LIQUIDACION AFORE/INFONAVIT(TRANS.ACR)",
             COLUMN 97, TODAY USING "mm/dd/yyyy"
       SKIP 2 LINE
       PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
       PRINT COLUMN 2,"FOLIO",
             COLUMN 8,"MOVIMIENTO",
             COLUMN 20, "SUBCUENTA",
             COLUMN 33, "FECHA VALOR",
             COLUMN 45, "FECHA LIQUIDACION",
             COLUMN 68, "MONTO EN ACCIONES",
             COLUMN 93, "MONTO EN PESOS"
       SKIP 2 LINE

    ON EVERY ROW
       PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
       PRINT COLUMN 2,g.folio USING "#####",
             COLUMN  8,g.tipo_movimiento USING "#####",
             COLUMN 20,g.subcuenta USING "#####",
             COLUMN 33,g.fecha_valor USING "dd-mm-yyyy",
             COLUMN 48,g.fecha_conversion USING "dd-mm-yyyy",
             COLUMN 60,g.monto_en_acciones USING "###############&.######", 
             COLUMN 85,g.monto_en_pesos USING "###############&.######"

    PAGE TRAILER
       SKIP 2 LINES
       PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
       PAUSE "Presione enter para continuar...."

    ON LAST ROW
       SKIP 4 LINES
       PRINT COLUMN 2, "Total de registros encontrados: ",
       COUNT(*) USING "<<<<"

END REPORT
