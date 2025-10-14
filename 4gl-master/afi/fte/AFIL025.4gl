##############################################################################
#Project           => SAFRE (Mexico)
#Owner             => E.F.P.
#Programa AFIL025  => REPORTE DE BAJAS DE AFILIACION                      
#Fecha actualiz.   => 02 diciembre del 2002
#Actualizacion     => LAURA EUGENIA CORTES GUZMAN
#Fecha actualiz.   => 
#Sistema           => AFI
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE fecha_ini,  fecha_fin,
           HOY                    DATE,

           reg    RECORD  LIKE    afi_ctr_logico.*,

           max_fecha               DATE,
           max_hora                CHAR(08),
           enter                   CHAR(01),
           usuario                 CHAR(08),
           hora                    CHAR(08),
           ho_ra                   CHAR(06),
           GLISTA                  CHAR(300),

           interno                 SMALLINT,
           opera                   CHAR(40),
           regs                    INTEGER,

           g_paramgrales RECORD LIKE seg_modulo.*

END GLOBALS

############################################################################
MAIN
    OPTIONS
    PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I,
           INPUT WRAP
     DEFER INTERRUPT

  CALL init()
  OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL0251" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir       < ESC > Acepta Datos                                   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " AFIL025            R E P O R T E   D E   B A J A S                            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    INPUT fecha_ini FROM FORMONLY.fecha_ini

        AFTER FIELD fecha_ini
              IF fecha_ini IS NULL OR fecha_ini = " "   THEN
                 ERROR "  Digite correctamente la Fecha a emitir el Reporte" 
                 NEXT FIELD fecha_ini
              END IF

        ON KEY (ESC)
              IF fecha_ini IS NULL OR fecha_ini = " "   THEN
                 ERROR "  Digite correctamente la Fecha a emitir el Reporte" 
                 NEXT FIELD fecha_ini
              ELSE
                 SELECT UNIQUE "X" FROM  afi_ctr_logico a
                 WHERE a.factualiza = fecha_ini
                   AND a.operacion MATCHES "BAJA SOLICITUD"
                   AND a.status_interno = 5
                 IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "No existe informacion para este Periodo "
                    NEXT FIELD fecha_ini
                 END IF

                 EXIT INPUT
              END IF

        ON KEY (INTERRUPT)
              PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
              FOR CHAR enter
              EXIT PROGRAM


        ON KEY (CONTROL-C)
              PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
              FOR CHAR enter
              EXIT PROGRAM

    END INPUT

    DISPLAY " GENERANDO INFORMACION.... " AT 19,1 ATTRIBUTE(REVERSE)

    START REPORT afil025 TO GLISTA
##    START REPORT afil025 TO "afimama.txt"

    SELECT MAX(a.hora) INTO max_hora FROM afi_ctr_logico a
    WHERE  a.factualiza = max_fecha
      AND  a.hora       = max_hora
      AND  a.operacion MATCHES "BAJA SOLIC"


    DECLARE cur_apt CURSOR FOR
             SELECT a.* FROM afi_ctr_logico a
              WHERE a.factualiza = fecha_ini
                AND a.status_interno = 5
                AND a.operacion MATCHES "BAJA SOLICITUD"
             ORDER BY 2,1
    FOREACH cur_apt INTO reg.*
 
            SELECT MAX(a.factualiza) INTO max_fecha FROM afi_ctr_logico a
            WHERE  a.n_seguro = reg.n_seguro
              AND  a.n_folio = reg.n_folio
              AND  a.tipo_solicitud = reg.tipo_solicitud
              AND  a.factualiza <= reg.factualiza
              AND  a.status_interno <> 5
              

            IF STATUS != NOTFOUND THEN
               SELECT MAX(a.status_interno)  ##, a.operacion
               INTO   interno  ##, opera
               FROM   afi_ctr_logico a
               WHERE  a.n_seguro = reg.n_seguro
                 AND  a.n_folio = reg.n_folio
                 AND  a.tipo_solicitud = reg.tipo_solicitud
                 AND  a.factualiza = max_fecha
                 AND  a.status_interno <> 5
##               GROUP BY 2

               LET regs = regs + 1

               OUTPUT TO REPORT 
                  afil025(reg.*,regs, interno)
            END IF 

    END FOREACH
    FINISH REPORT afil025

    DISPLAY "ARCHIVO GENERADO EN : ",GLISTA CLIPPED AT 18,1 
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR" 
    FOR CHAR enter

END MAIN

############################################################################
FUNCTION init()
    INITIALIZE reg.*,   interno, opera, g_paramgrales.* TO NULL
    INITIALIZE GLISTA, hora, ho_ra, usuario TO NULL

    LET regs           = 0
    LET HOY            = TODAY
    LET hora           = TIME
    LET fecha_ini      = HOY

    LET ho_ra = hora[1,2],hora[4,5] CLIPPED

    SELECT *, USER
           INTO g_paramgrales.*,
                usuario
           FROM seg_modulo
           WHERE modulo_cod = "afi"

    LET GLISTA = g_paramgrales.ruta_listados CLIPPED,"/",usuario CLIPPED, 
                  ".BAJAS_" CLIPPED,                                  
                  TODAY USING "ddmmyy","_",ho_ra

END FUNCTION

############################################################################
REPORT afil025(reg, regs, interno)

    DEFINE reg      RECORD LIKE    afi_ctr_logico.*,

           interno                 SMALLINT,
           regs                    INTEGER

    OUTPUT
	         LEFT   MARGIN 0
	         RIGHT  MARGIN 0
	         TOP    MARGIN 0
	         BOTTOM MARGIN 0
            PAGE   LENGTH 66

    FORMAT
       PAGE HEADER
            PRINT COLUMN 48, "R E P O R T E   D E   B A J A S"
            PRINT COLUMN 113, "Prog. : AFIL025"
            PRINT COLUMN 113, "Fecha : ",TODAY USING "dd/mm/yyyy"

       PRINT COLUMN 01,  "==================================================",
                         "==================================================",
                         "=============================="  ##130

            PRINT COLUMN 001,"F O L I O",
                  COLUMN 015,"N. S. S.",
                  COLUMN 030,"F E C H A",
                  COLUMN 044,"TIP.SOL.",
                  COLUMN 056,"STATUS",
                  COLUMN 066,"H O R A",
                  COLUMN 078,"USUARIO",
                  COLUMN 089,"O P E R A C I O N",
                  COLUMN 111,"ST.ANTERIOR"

       PRINT COLUMN 01,  "==================================================",
                         "==================================================",
                         "=============================="  ##130

    ON EVERY ROW
            PRINT COLUMN 001,reg.n_folio USING "#########&",
                  COLUMN 015,reg.n_seguro,
                  COLUMN 030,reg.factualiza USING "dd/mm/yyyy",
                  COLUMN 047,reg.tipo_solicitud USING "#",
                  COLUMN 058,reg.status_interno USING "##",
                  COLUMN 066,reg.hora,
                  COLUMN 078,reg.usuario,
                  COLUMN 089,reg.operacion[1,20],
                  COLUMN 116,interno USING "##"

    ON LAST ROW
       SKIP 2 LINES
       PRINT COLUMN 01,  "==================================================",
                         "==================================================",
                         "=============================="  ##130

       PRINT COLUMN 05, "T O T A L   R E G I S T R O S : ", regs USING "########&"
END REPORT
