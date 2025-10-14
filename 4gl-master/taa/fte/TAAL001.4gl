#############################################################################
#Proyecto          => safre_af.( MEXICO )                                   #
#Propietario       => E.F.P.                                                #
#Programa TAAL001  => Informacion de afi_solicitud traspasados              #
#Fecha             => 10 julio 1998                                         #
#Por               => RICARDO ARELLANO M.                                   #
#Sistema           => TRA                                                   #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE datos RECORD
        cve_cede       CHAR(03),
        des_afore      CHAR(30),
        tipo_traspaso  SMALLINT,
        tot_parcial    SMALLINT
    END RECORD

    DEFINE hoy           DATE
    DEFINE hora          CHAR(8)
    DEFINE g_usuario     CHAR(8)
    DEFINE aux_pausa     CHAR(1)
    DEFINE g_afore       RECORD LIKE tab_afore_local.*
    DEFINE g_paramgrales RECORD LIKE seg_modulo.*
    DEFINE g_lista       CHAR(100)
    DEFINE COMANDO       CHAR(100)
    DEFINE tot_total     SMALLINT
    DEFINE numero_2      SMALLINT
    DEFINE vfolio        INTEGER
    DEFINE opc           CHAR(1)

END GLOBALS

MAIN

    OPTIONS
      PROMPT LINE LAST    ,
      ACCEPT KEY CONTROL-I,
      INPUT WRAP

    DEFER INTERRUPT
      LET HOY = TODAY
      LET hora = TIME

    OPEN WINDOW ventana_1 AT 4,4 WITH 20 ROWS, 76 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " TAAL001      REPORTE DE TRASPASOS AFORE - AFORE (RECEP)                       " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                             < Ctrl-C > Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    PROMPT "Ingrese folio : " FOR vfolio

    DISPLAY "Folio : " , vfolio AT 15,15

    PROMPT "DESEA EJECUTAR EL PROCESO ? [S/N]..." FOR opc

    IF opc MATCHES "[Ss]" THEN
        ERROR "PROCESANDO INFORMACION "
        CALL reporte_solic() #a 
    ELSE
        ERROR "PROCESO CANCELADO" SLEEP 3
        EXIT PROGRAM
    END IF

END MAIN

FUNCTION reporte_solic()
#a----------------------

    SELECT COUNT(*)
    INTO   datos.tot_parcial
    FROM   taa_rcv_recepcion
    WHERE  folio = vfolio

    IF datos.tot_parcial IS NULL OR
       datos.tot_parcial <= 0 THEN
        ERROR "NO EXISTEN DATOS" sleep 2
        EXIT PROGRAM 
    ELSE
       INITIALIZE datos.* TO NULL

       SELECT * ,USER
       INTO   g_afore.*, g_usuario 
       FROM   tab_afore_local

       SELECT *
       INTO   g_paramgrales.*
       FROM   seg_modulo    
       WHERE  modulo_cod = 'taa'

       LET g_lista = g_paramgrales.ruta_listados CLIPPED, "/",g_usuario CLIPPED,
                     ".afili_rec.",hoy using "DDMMYY"

       START REPORT listado TO g_lista

       DECLARE cur1 CURSOR FOR
       SELECT a.cve_ced_cuenta,b.afore_desc,a.tipo_traspaso,count(*)
       FROM   taa_rcv_recepcion a, tab_afore b
       WHERE  a.folio = vfolio
       AND    a.cve_ced_cuenta = b.afore_cod
       AND    a.tipo_registro = "25"
       GROUP BY 1,2,3
       ORDER BY 1,3

       IF STATUS = NOTFOUND THEN
           ERROR "NO SE IMPRIME REPORTE" sleep 2
       ELSE
           FOREACH cur1 into datos.*
           LET tot_total = tot_total + datos.tot_parcial
           OUTPUT TO REPORT listado(datos.*)
           END FOREACH 
       END IF

       FINISH REPORT listado

       LET g_lista = g_paramgrales.ruta_listados CLIPPED, "/",g_usuario CLIPPED,
                     ".afili_rec",hoy using "DDMMYY"

       LET COMANDO = "lp ",g_lista
       --LET COMANDO = "vi ",g_lista
       RUN COMANDO

       ERROR "SE CONCLUYE PROCESO" SLEEP 2
    END IF

END FUNCTION

REPORT listado(datos)
#--------------------

    DEFINE datos RECORD
        cve_cede        CHAR(03),
        des_afore       CHAR(30),
        tipo_traspaso   SMALLINT,
        tot_parcial     SMALLINT
    END RECORD

    DEFINE campo RECORD
        cod_afore   SMALLINT,
        raz_social  CHAR(50),
        des_titulo  CHAR(23)
    END RECORD

    OUTPUT
        PAGE LENGTH 60
        LEFT MARGIN 0
        RIGHT MARGIN 132
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER

    SELECT a.codigo_afore,a.razon_social,b.afore_desc
    INTO campo.cod_afore,campo.raz_social,campo.des_titulo
    FROM tab_afore_local a,tab_afore b
    WHERE a.codigo_afore = b.afore_cod

    PRINT COLUMN 4,'\033e\033(s218T\033(s11H\033(s7B',"INFORMACION DE AFILIADOS TRASPASADOS A AFORE ",campo.des_titulo," FECHA : ",hoy  

    PRINT
    PRINT COLUMN 1,'\033e\033(s218T\033(s11H\033(s7N'
    PRINT COLUMN 2,campo.cod_afore,"     ",campo.raz_social
    PRINT COLUMN 4,"FOLIO : ", vfolio
    PRINT

    PRINT COLUMN 1,"\332\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\277"

    PRINT COLUMN 1,"\263",COLUMN 3,"CLAVE ",COLUMN 8,"\263",COLUMN 20,"AFORE",COLUMN 43,"\263",COLUMN 45, "ORIGEN", COLUMN 52,"\263",COLUMN 54,"AFILIADOS RECIBIDOS",COLUMN 74,"  \263"

    PRINT COLUMN 1,"\300\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\331"

ON EVERY ROW

    PRINT
    PRINT COLUMN 4,datos.cve_cede USING "&&&",
          COLUMN 11,datos.des_afore,
          COLUMN 46,datos.tipo_traspaso,
          COLUMN 60,datos.tot_parcial USING "&&&&&&"
    PRINT 

        PRINT 
        IF lineno > 57 THEN
            SKIP TO TOP OF PAGE
        END IF

ON LAST ROW

         PRINT COLUMN 1,"\332\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\277"

         PRINT COLUMN 1,"\263",COLUMN 3,"TOTAL ","\263 ",COLUMN 52,"\263",COLUMN 60,tot_total USING "&&&&&&",COLUMN 76,"\263" 

         PRINT COLUMN 1,"\300\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304304\304\331"

END REPORT
