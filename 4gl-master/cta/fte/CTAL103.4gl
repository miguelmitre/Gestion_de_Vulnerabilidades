#############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                         #
#Propietario       => E.F.P.                                                #
#Fecha             => 02 DE DICIEMBRE 2004.                                 #
#Por               => ISABEL FONSECA FRIAS                                  #
#Sistema           => CTA                                                   #
#Programa          => CTAL103                                               #
#                  => TIEMPO DE RESPUESTA ORDEN DE SELECCION                #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE enter              CHAR(1)
    DEFINE g_usuario          CHAR(8)
    DEFINE G_LISTA            CHAR(300)
    DEFINE G_IMPRE            CHAR(300)
    DEFINE G_IMPRE1           CHAR(300)
    DEFINE gimpresion         CHAR(300)
    DEFINE gimpresion1        CHAR(300)
    DEFINE hora               CHAR(8)
    DEFINE HOY                DATE
    DEFINE t_nss              CHAR(11)
    DEFINE t_fecha_inicio     DATETIME year to second
    DEFINE t_fecha_fin        DATETIME year to second

    DEFINE vfecha             DATE

    DEFINE l_record  ARRAY[500] OF RECORD
        nss                   CHAR(11),
        fecha_inicio          DATETIME year to second,
        fecha_fin             DATETIME year to second,
        resultado             INTERVAL  MINUTE TO SECOND
    END RECORD

    DEFINE g  RECORD
        nss                   CHAR(11),
        fecha_inicio          DATETIME year to second,
        fecha_fin             DATETIME year to second,
        resultado             INTERVAL  MINUTE TO SECOND
    END RECORD

    DEFINE resultado        INTERVAL  MINUTE TO SECOND

END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o
        DEFER INTERRUPT

    CALL STARTLOG('CTAL103.log')
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    SELECT  codigo_afore,USER
    INTO    w_codigo_afore,g_usuario
    FROM    tab_afore_local


    SELECT  *
    INTO    g_seg_modulo.*
    FROM    seg_modulo
    WHERE   modulo_cod = 'afi'

    LET HOY = TODAY

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,4 WITH 3 ROWS,72 COLUMNS ATTRIBUTE( BORDER)
    DISPLAY " CTAL103 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)

    MENU "MENU "
        COMMAND "Consulta " "Consulta Tiempo de Respuesta Ord. Selec."
            CALL Consulta()
        COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION Consulta()
#c-----------------

    DEFINE pos SMALLINT

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 10,4 WITH FORM "CTAL1032" ATTRIBUTE( BORDER)
    DISPLAY "              (Ctrl-C) Salir          (ESC) Ejecutar                           " AT 1,1 ATTRIBUTE(REVERSE,green)

    LET int_flag= FALSE

    INPUT BY NAME vfecha
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
      CLEAR SCREEN
      CLOSE WINDOW ventana_21
      RETURN
    END IF

    ERROR "PROCESANDO INFORMACION"
    DECLARE cursor_1 CURSOR FOR
    SELECT nss,
           fecha_inicio,
           fecha_fin
    INTO   t_nss,t_fecha_inicio,t_fecha_fin
    FROM   cta_solicitud_regimen
    where to_char(fecha_inicio,'%m%d%Y')= vfecha
    ORDER BY 1,2

    LET pos = 1

    FOREACH cursor_1 INTO l_record[pos].*
       INITIALIZE l_record[pos].resultado TO NULL
       LET l_record[pos].resultado = l_record[pos].fecha_fin - l_record[pos].fecha_inicio
       LET pos = pos + 1
    END FOREACH

    INITIALIZE l_record[pos].* TO NULL

    CLOSE WINDOW ventana_21

    IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      ERROR ""
      OPEN WINDOW ventana_2 AT 6,4 WITH FORM "CTAL1031" ATTRIBUTE( BORDER)
      DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
      DISPLAY " (Ctrl-C) Salir            (Ctrl-P) Imprimir                                   " AT 1,1 ATTRIBUTE(REVERSE,green)
      DISPLAY " CTAL103     Consulta Tiempo de Respuesta Orden de Seleccion                   " AT 3,1 ATTRIBUTE(REVERSE,green) 
      DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE,green)

      DISPLAY ARRAY l_record TO scr_1.*
        ON KEY (Control-p)
          ERROR "PROCESANDO IMPRESION..."
          CALL imprimir(pos-1)
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

FUNCTION imprimir(pos)
#impr----------------

    DEFINE i,
           pos SMALLINT

    LET hora    = TIME
    LET hora    = hora[1,2],hora[4,5],hora[7,8]

    LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".TIEMPO_ORS.",HOY USING "DDMMYY", "_",hora CLIPPED

    LET G_IMPRE1 = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".TIEMPO_ORS1.",HOY USING "DDMMYY", "_",hora CLIPPED

    START REPORT rpt_cuenta_imp TO G_IMPRE
    START REPORT rpt_cuenta_imp1 TO G_IMPRE1

       FOR i=1 TO (pos+1)
          LET g.nss               = l_record[i].nss
          LET g.fecha_inicio      = l_record[i].fecha_inicio
          LET g.fecha_fin         = l_record[i].fecha_fin
          LET g.resultado         = l_record[i].resultado 
          
          IF g.resultado > "1:00"  THEN
             OUTPUT TO REPORT rpt_cuenta_imp(g.*)
          ELSE
             OUTPUT TO REPORT rpt_cuenta_imp1(g.*)
          END IF

       END FOR

    FINISH REPORT rpt_cuenta_imp
    FINISH REPORT rpt_cuenta_imp1

    ERROR "LISTADO GENERADO"
    SLEEP 2
    ERROR ""

    LET gimpresion = "lp ",G_IMPRE
    LET gimpresion1 = "lp ",G_IMPRE1
    RUN gimpresion
    RUN gimpresion1

END FUNCTION

REPORT rpt_cuenta_imp(g)
#rci--------------------

    DEFINE g  RECORD
        nss          CHAR(11),
        fecha_inicio DATETIME year to second,
        fecha_fin    DATETIME year to second,
        resultado    INTERVAL  MINUTE TO SECOND
    END RECORD

    DEFINE pos1      SMALLINT 

    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT
      PAGE HEADER
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 2,"CTAL103",
              COLUMN 30, "   TIEMPO DE RESPUESTA ORDEN DE SELECCION >= 60 SEG.   ",
              COLUMN 97, TODAY USING "mm/dd/yyyy"
        SKIP 2 LINE
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

       PRINT  COLUMN  5, "NSS",
              COLUMN 20, "SOLICITUD",
              COLUMN 40, "RESPUESTA",
              COLUMN 60, "TIEMPO REAL RESP"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

     ON EVERY ROW
        IF g.nss IS NOT NULL THEN
           LET pos1 = pos1 + 1
           PRINT COLUMN  5,g.nss ,
                 COLUMN 20,g.fecha_inicio,
                 COLUMN 40,g.fecha_fin,
                 COLUMN 60,g.resultado
        END IF
       ON LAST ROW
         SKIP 4 LINES
	 PRINT COLUMN 2, "Total Operado: ", pos1 

END REPORT

REPORT rpt_cuenta_imp1(g)
#rci--------------------

    DEFINE g  RECORD
        nss          CHAR(11),
        fecha_inicio DATETIME year to second,
        fecha_fin    DATETIME year to second,
        resultado    INTERVAL  MINUTE TO SECOND
    END RECORD

    DEFINE pos2      SMALLINT 

    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT
      PAGE HEADER
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 2,"CTAL103",
              COLUMN 30, "   TIEMPO DE RESPUESTA ORDEN DE SELECCION < 60 SEG.   ",
              COLUMN 97, TODAY USING "mm/dd/yyyy"
        SKIP 2 LINE
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

        PRINT  COLUMN  5, "NSS",
               COLUMN 20, "SOLICITUD",
               COLUMN 40, "RESPUESTA",
               COLUMN 60, "TIEMPO REAL RESP"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

     ON EVERY ROW
        IF g.nss IS NOT NULL THEN
           LET pos2 = pos2 + 1
           PRINT COLUMN  5,g.nss ,
                 COLUMN 20,g.fecha_inicio,
                 COLUMN 40,g.fecha_fin,
                 COLUMN 60,g.resultado
        END IF 
             
       ON LAST ROW
         SKIP 4 LINES
	 PRINT COLUMN 2, "Total Operado: ", pos2 

END REPORT

