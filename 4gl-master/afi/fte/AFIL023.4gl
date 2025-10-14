##############################################################################
#Project           => SAFRE (Mexico)
#Owner             => E.F.P.
#Programa AFIL023  => REPORTE DE STATUS DE CURP SALIDA RENAPO(72,73,74)
#Fecha actualiz.   => 03 ABRIL 2001
#Actualizacion     => LAURA EUGENIA CORTES GUZMAN
#Fecha actualiz.   => 
#Sistema           => AFI
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE fecha_ini,  fecha_fin,
           HOY                    DATE,
           HORA                   CHAR(8),

           dispersa RECORD
                  n_seguro         LIKE afi_dispersa_curp.n_seguro,
                  status_renapo    LIKE afi_dispersa_curp.status_renapo,
                  fecha_asignacion LIKE afi_dispersa_curp.fecha_asignacion
           END RECORD,

           mae   RECORD
                  nombres          LIKE afi_mae_afiliado.nombres,
                  paterno          LIKE afi_mae_afiliado.paterno,
                  materno          LIKE afi_mae_afiliado.materno,
                  n_folio          LIKE afi_mae_afiliado.n_folio
           END RECORD,

           enter                   CHAR(01),
           usuario                 CHAR(08),
           GLISTA                  CHAR(300),

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
  OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL0231" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir       < ESC > Acepta Datos                                   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " AFIL023      REPORTE DE STATUS DE SALIDA DE RENAPO (72,73,74)                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    INPUT fecha_ini, fecha_fin  FROM FORMONLY.fecha_ini, FORMONLY.fecha_fin

        AFTER FIELD fecha_ini
              IF fecha_ini IS NULL OR fecha_ini = " "   THEN
                 ERROR "  Digite correctamente la Fecha Inicial" 
                       ATTRIBUTE(NORMAL)
                 NEXT FIELD fecha_ini
              END IF

        AFTER FIELD fecha_fin
              IF fecha_fin IS NULL OR fecha_fin = " " THEN
                 ERROR "  Digite correctamente la Fecha Final" 
                       ATTRIBUTE(NORMAL)
                 NEXT FIELD fecha_fin
              END IF

              IF fecha_ini > fecha_fin THEN
                 ERROR "  La Fecha Inicial no puede ser mayor a la Fecha Final"
                       ATTRIBUTE(NORMAL)
                 NEXT FIELD fecha_ini
              END IF

        ON KEY (ESC)
              IF fecha_ini IS NOT NULL  OR 
                 fecha_fin IS NOT NULL  AND
                 fecha_ini < fecha_fin  THEN
                 LET fecha_ini = fecha_ini
                 LET fecha_fin = fecha_fin

                 SELECT UNIQUE "X" FROM  afi_dispersa_curp a
                 WHERE a.n_seguro = n_seguro
                   AND a.cod_operacion = "02"
                   AND a.status_renapo in('72','73','74')
                   AND a.fecha_asignacion BETWEEN fecha_ini AND fecha_fin
                 IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "No existe informacion para este Periodo "
                    NEXT FIELD fecha_ini
                 END IF

                 EXIT INPUT
              ELSE
                 ERROR "  Digite correctamente la Fecha Inicial" 
                       ATTRIBUTE(NORMAL)
                 NEXT FIELD fecha_ini
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

    START REPORT afil023 TO GLISTA
##    START REPORT afil023 TO "afimama.txt"

    DECLARE cur_apt CURSOR FOR
             SELECT a.n_seguro, a.status_renapo, a.fecha_asignacion
               FROM  afi_dispersa_curp a
              WHERE a.cod_operacion = "02"
                AND a.status_renapo in('72','73','74')
                AND a.fecha_asignacion BETWEEN fecha_ini AND fecha_fin
             ORDER BY a.fecha_asignacion
    FOREACH cur_apt INTO dispersa.*
 
            SELECT b.nombres, b.paterno, b.materno, b.n_folio
              INTO mae.*
              FROM afi_mae_afiliado b
             WHERE b.n_seguro = dispersa.n_seguro

            IF STATUS != NOTFOUND THEN
               LET regs = regs + 1

               OUTPUT TO REPORT 
                  afil023(dispersa.*, mae.*, regs, fecha_ini, fecha_fin)
            END IF 

    END FOREACH
    FINISH REPORT afil023

    DISPLAY "ARCHIVO GENERADO EN : ",GLISTA CLIPPED AT 18,1 
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR" 
    FOR CHAR enter

END MAIN

############################################################################
FUNCTION init()
    INITIALIZE dispersa.*,   mae.*, g_paramgrales.* TO NULL
    INITIALIZE GLISTA, usuario TO NULL

    LET regs           = 0
    LET HOY            = TODAY
    LET HORA           = TIME
    LET fecha_ini      = HOY
    LET fecha_fin      = HOY

    SELECT *, USER
           INTO g_paramgrales.*,
                usuario
           FROM seg_modulo
           WHERE modulo_cod = "afi"

    LET GLISTA = g_paramgrales.ruta_listados CLIPPED,"/",usuario CLIPPED, 
                  ".NO_ASIG_REN" CLIPPED,                                  
                  "_",TODAY USING "ddmmyy", HORA[1,2], HORA[4,5]

END FUNCTION

############################################################################
REPORT afil023(dispersa, mae, regs, fecha_ini, fecha_fin)

    DEFINE dispersa RECORD
                  n_seguro         LIKE afi_dispersa_curp.n_seguro,
                  status_renapo    LIKE afi_dispersa_curp.status_renapo,
                  fecha_asignacion LIKE afi_dispersa_curp.fecha_asignacion
           END RECORD,

           mae   RECORD
                  nombres          LIKE afi_mae_afiliado.nombres,
                  paterno          LIKE afi_mae_afiliado.paterno,
                  materno          LIKE afi_mae_afiliado.materno,
                  n_folio          LIKE afi_mae_afiliado.n_folio
           END RECORD,

           fecha_ini               DATE,
           fecha_fin               DATE,
           fecha                   DATE,
           usuario                 CHAR(08),

           regs                    INTEGER

    OUTPUT
	         LEFT   MARGIN 0
	         RIGHT  MARGIN 0
	         TOP    MARGIN 0
	         BOTTOM MARGIN 0
            PAGE   LENGTH 66

    FORMAT
       PAGE HEADER
            PRINT COLUMN 67, "REPORTE DE STATUS DE SALIDA RENAPO (72,73,74)"
            PRINT COLUMN 77, fecha_ini USING "dd/mm/yyyy",
                              " A ",fecha_fin USING "dd/mm/yyyy"
            PRINT COLUMN 160, "Prog. : AFIL023"
            PRINT COLUMN 160, "Fecha : ",TODAY USING "dd/mm/yyyy"

       PRINT COLUMN 01, "==================================================",
             COLUMN 51, "==================================================",
             COLUMN 101, "==================================================",
             COLUMN 151, "===========================" ##177

            PRINT COLUMN 1, "CONSEC.",
                  COLUMN 12, "F O L I O ",
                  COLUMN 26, "N.  S.  S. ",
                  COLUMN 41, "APELLIDO PATERNO",
                  COLUMN 76, "APELLIDO MATERNO",
                  COLUMN 111, "N O M B R E S ",
                  COLUMN 146, "STATUS RENAPO",
                  COLUMN 163, "FECHA RECEPCION"

       PRINT COLUMN 01, "==================================================",
             COLUMN 51, "==================================================",
             COLUMN 101, "==================================================",
             COLUMN 151, "===========================" ##77

    ON EVERY ROW
            PRINT COLUMN 1, regs USING "######&",
                  COLUMN 12, mae.n_folio,
                  COLUMN 26, dispersa.n_seguro,
                  COLUMN 41, mae.paterno CLIPPED,
                  COLUMN 76, mae.materno CLIPPED,
                  COLUMN 111, mae.nombres CLIPPED,
                  COLUMN 151, dispersa.status_renapo,
                  COLUMN 165, dispersa.fecha_asignacion

END REPORT
