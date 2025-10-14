#########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                      #
#Propietario       => EFP                                               #
#Programa          => ACRL010                                           #
#Descripcion       => REPORTE DE LIQUIDACION DE ANUALIDADES GARANTIZADAS#
#Sistema           => ACR                                               #
#Fecha creacion    => 06 ENERO 2010                                     #
#Por               => STEFANIE DANIELA VERA PIÑA                        #
#########################################################################

DATABASE safre_af

GLOBALS

    DEFINE g           RECORD LIKE dis_cuenta.*
    DEFINE v_afore     RECORD LIKE tab_afore_local.*
    DEFINE g_param_dis RECORD LIKE seg_modulo.*

    DEFINE l_record   ARRAY[300] OF RECORD
        folio             INTEGER,
        tipo_movimiento   INTEGER,
        subcuenta         INTEGER,
        fecha_valor       DATE,
        fecha_conversion  DATE,
        monto_en_acciones DECIMAL(16,6),
        monto_en_pesos    DECIMAL(16,6)
    END RECORD

    DEFINE 
	     g_usuario  CHAR (08),
        G_LISTA    CHAR(300),
        G_IMPRE    CHAR(300),
        gimpresion CHAR(300),
        hora       CHAR (08)

    DEFINE
	     HOY        DATE

    DEFINE 
	     vfolio     INTEGER   
    

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
#----------------

    SELECT  t.*, USER
    INTO    v_afore.*, g_usuario
    FROM    tab_afore_local t

    SELECT  *
    INTO    g_param_dis.*
    FROM    seg_modulo
    WHERE   modulo_cod = 'acr'

    LET HOY = TODAY

END FUNCTION


FUNCTION proceso_principal()
#---------------------------

    OPEN WINDOW v1 AT 3,4  WITH 3 ROWS,72 COLUMNS ATTRIBUTE( BORDER)
    DISPLAY " ACRL010 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)
	
    MENU "MENU "
       COMMAND "Consulta " "Consulta de Liquidacion."
           CALL Consulta()
       COMMAND "Salir" "Salir del Programa"
           EXIT MENU
    END MENU

    CLOSE WINDOW v1
 
END FUNCTION


FUNCTION Consulta()
#------------------

    DEFINE pos SMALLINT   

    CLEAR SCREEN

    OPEN WINDOW v2 AT 10,4 WITH FORM "ACRL0102" ATTRIBUTE( BORDER)
    DISPLAY " (Ctrl-C) Salir                                       (ESC) Ejecutar                           " AT 1,1 ATTRIBUTE(REVERSE,green)

    LET int_flag= FALSE

    INPUT BY NAME vfolio
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
        ERROR " FOLIO NO ACEPTADO "
        SLEEP 2
        ERROR ""

        CLEAR SCREEN
        CLOSE WINDOW v2
        RETURN
    END IF

    ERROR " PROCESANDO INFORMACION "
    DECLARE cursor_1 CURSOR FOR
    SELECT folio,
           tipo_movimiento,                                        
           subcuenta,                                                 
           fecha_valor,                                                
           fecha_conversion,                                               
           sum(monto_en_acciones) * -1,
           sum(monto_en_pesos) * -1                                    
    FROM dis_cuenta                                                         
    WHERE folio = vfolio 
    AND  subcuenta in (4,8)
    AND  tipo_movimiento in (234)   
    GROUP BY 1,2,3,4,5                                                   
    ORDER BY 1,2 ,3

    LET pos = 1

    FOREACH cursor_1 INTO l_record[pos].*
        LET pos = pos + 1
    END FOREACH

    INITIALIZE l_record[pos].* TO NULL

    CLOSE WINDOW v2

    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)

        ERROR ""

        OPEN WINDOW v3 AT 6,4 WITH FORM "ACRL0101" ATTRIBUTE( BORDER)
        DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
        DISPLAY " (Ctrl-C) Salir              (Ctrl-P) Imprimir            (Ctrl-F) Archivo  " AT 1,1 ATTRIBUTE(REVERSE,green)
        DISPLAY " ACRL010     CONSULTA LIQUIDACION DE ANUALIDADES GARANTIZADAS               " AT 3,1 ATTRIBUTE(REVERSE,green) 
        DISPLAY HOY USING " dd-mm-yyyy " AT 3,64 ATTRIBUTE(REVERSE,green)
   	
        DISPLAY ARRAY l_record TO scr_1.*

            ON KEY (Control-p)
               ERROR " PROCESANDO IMPRESION "
               CALL imprimir()
            ON KEY (INTERRUPT)
               EXIT DISPLAY
        END DISPLAY

        CLEAR WINDOW v3
        CLOSE WINDOW v3
    ELSE
        ERROR " ARCHIVO VACIO "
        SLEEP 2
        ERROR ""
    END IF 

END FUNCTION


FUNCTION imprimir()
#i-----------------

    LET hora = TIME

    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
          ".LIQ_ANU_GAR",HOY USING "DDMMYY", "_",hora[1,2],hora[4,5]

    START REPORT rpt_cuenta_imp TO G_IMPRE
    CALL cuentas_impri()

    LET gimpresion = "lp ",G_IMPRE
    RUN gimpresion

END FUNCTION


FUNCTION cuentas_impri()
#-----------------------

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
    ERROR " LISTADO GENERADO "
    SLEEP 2
    ERROR ""

END FUNCTION


REPORT rpt_cuenta_imp(g)
#-----------------------

    DEFINE g RECORD LIKE dis_cuenta.*

    DEFINE tot_viv DECIMAL(18,6)
    DEFINE tot_reg INTEGER

    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT
    PAGE HEADER

        SELECT SUM(monto_en_pesos)
        INTO   tot_viv
        FROM   dis_cuenta
        WHERE  folio = vfolio

        SELECT COUNT(*)
        INTO   tot_reg
        FROM   acr_det_ced_ag d
        WHERE  d.folio  = vfolio
        AND    d.estado = 0

        PRINT COLUMN  2,"CVE. AFORE ",v_afore.codigo_afore USING "&&&",
              COLUMN 20,v_afore.razon_social
        SKIP 1 LINE

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 2,"ACRL010",
              COLUMN 30,"LISTADO DE LIQUIDACION ANUALIDADES GARANTIZADAS",
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
        PRINT COLUMN 2, "Total saldo vivienda     : ",
                         tot_viv USING "############&.######"
        PRINT
        PRINT COLUMN 2, "Total registros a enviar : ", tot_reg

END REPORT
