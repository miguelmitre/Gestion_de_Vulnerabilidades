##############################################################################
#Project           => SAFRE (Mexico)
#Owner             => E.F.P.
#Programa AFIR008  => REPORTE DE CONDICIONES
#Fecha actualiz.   => 03 ABRIL 2001
#Actualizacion     => LAURA EUGENIA CORTES GUZMAN
#Fecha actualiz.   => 
#Sistema           => AFI
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE fecha_ini,  fecha_fin,
           HOY                    DATE,

           cap_lote RECORD
                  lote             LIKE afi_ctr_lote.lote,
                  fecha            LIKE afi_ctr_lote.fecha,
                  estado           LIKE afi_ctr_lote.estado,
                  no_solic         LIKE afi_ctr_lote.no_solic
           END RECORD,

           cap_suc RECORD
                  sucursal         LIKE afi_recepcion.sucursal
           END RECORD,

           cap_recepcion RECORD
                  folio            LIKE afi_expediente.n_folio,
                  estado           LIKE afi_recepcion.estado_exp,
                  sucursal         LIKE afi_recepcion.sucursal
           END RECORD,

           enter                   CHAR(01),
           usuario                 CHAR(08),
           GLISTA                  CHAR(300),

           tot_rechazados          INTEGER,
           tot_suc_x_lote          INTEGER,
           tot_suc_sales           INTEGER,
           tot_completas           INTEGER,

           rechazados              INTEGER,
           completas               INTEGER,

           sw                     SMALLINT,

           v_sucursal           LIKE pro_mae_promotor.agenc_cod,
           g_paramgrales RECORD LIKE glo_parametro.*

END GLOBALS

############################################################################
MAIN
    OPTIONS
    PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I,
           INPUT WRAP
     DEFER INTERRUPT

  CALL init()
  OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL0221" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir       < ESC > Acepta Datos                                   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " AFIL022              ESTADISTICA DE CONDICION DOCUMENTAL                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    INPUT BY NAME fecha_ini, fecha_fin  WITHOUT DEFAULTS

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

    START REPORT afil022 TO GLISTA
##    START REPORT afil022 TO "afimama.txt"

    DECLARE cur_apt CURSOR FOR
            SELECT a.lote, a.fecha, a.estado, a.no_solic FROM afi_ctr_lote a
                   WHERE a.fecha BETWEEN fecha_ini AND fecha_fin
            ORDER BY 2, 1
    FOREACH cur_apt INTO cap_lote.*
 
         IF STATUS = NOTFOUND THEN
            ERROR " NO EXISTEN REGISTROS A EMITIR " ATTRIBUTE(NORMAL)
            SLEEP 1
            EXIT FOREACH
         END IF
         DECLARE cur_apt1 CURSOR FOR
            SELECT unique a.n_folio, b.estado_exp, b.sucursal
                   FROM  afi_expediente a, afi_recepcion b           
                   WHERE a.lote           = cap_lote.lote
                     AND a.n_folio        = b.n_folio                      
	                  AND a.tipo_solicitud = b.tipo_solicitud        
            ORDER BY b.sucursal

         FOREACH cur_apt1 INTO cap_recepcion.*
              IF sw = 0 THEN
                 LET v_sucursal = cap_recepcion.sucursal
                 LET tot_suc_x_lote = tot_suc_x_lote + 1
                 LET sw = 1
              END IF

              IF v_sucursal = cap_recepcion.sucursal THEN
                 CASE cap_recepcion.estado
                     WHEN 0
                        LET completas = completas + 1
                        LET tot_completas = tot_completas + 1
                     WHEN 1
                        LET rechazados = rechazados + 1
                        LET tot_rechazados = tot_rechazados + 1
                 END CASE
              END IF

              IF v_sucursal != cap_recepcion.sucursal THEN
                 OUTPUT TO REPORT 
                    afil022(cap_lote.fecha,  v_sucursal, 
                            completas,       rechazados,
                            tot_completas,   tot_rechazados,
                            fecha_ini,       fecha_fin)

                 LET v_sucursal   = cap_recepcion.sucursal
                 LET completas  = 0
                 LET rechazados = 0
                 LET tot_suc_x_lote = tot_suc_x_lote + 1

                 CASE cap_recepcion.estado
                     WHEN 0
                          LET completas = completas + 1
                          LET tot_completas = tot_completas + 1
                     WHEN 1
                          LET rechazados = rechazados + 1
                          LET tot_rechazados = tot_rechazados + 1
                 END CASE
              END IF
         END FOREACH
         OUTPUT TO REPORT 
              afil022(cap_lote.fecha,  v_sucursal, 
                      completas,       rechazados,
                      tot_completas,   tot_rechazados,
                      fecha_ini,       fecha_fin)
         LET sw = 0
         LET completas  = 0
         LET rechazados = 0

    END FOREACH
    FINISH REPORT afil022

    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR" 
    FOR CHAR enter
    EXIT PROGRAM

END MAIN

############################################################################
FUNCTION init()
    INITIALIZE cap_recepcion.*,   cap_lote.*, g_paramgrales.* TO NULL
    INITIALIZE GLISTA, usuario TO NULL

    LET sw             = 0
    LET tot_suc_sales  = 0
    LET tot_suc_x_lote = 0
    LET rechazados     = 0
    LET completas      = 0
    LET tot_rechazados = 0
    LET tot_completas  = 0
    LET HOY            = TODAY
    LET fecha_ini      = HOY
    LET fecha_fin      = HOY

    SELECT *, USER
           INTO g_paramgrales.*,
                usuario
           FROM glo_parametro

    LET GLISTA = g_paramgrales.ruta_spool CLIPPED,"/",usuario CLIPPED, 
                  ".AFIM008" CLIPPED,                                  
                  "_",TODAY USING "dd-mm-yy"

END FUNCTION

############################################################################
REPORT afil022(fecha,         sucursal,
               completas,     rechazados,
               tot_completas, tot_rechazados,
               fecha_ini,     fecha_fin)

    DEFINE sucursal                LIKE pro_mae_promotor.agenc_cod,
           fecha                   DATE,
           fecha_ini               DATE,
           fecha_fin               DATE,
           usuario                 CHAR(08),

           tot_rechazados          INTEGER,
           tot_completas           INTEGER,

           rechazados              INTEGER,
           completas               INTEGER


    OUTPUT
	         LEFT   MARGIN 0
	         RIGHT  MARGIN 0
	         TOP    MARGIN 0
	         BOTTOM MARGIN 0
            PAGE   LENGTH 66

    FORMAT
       PAGE HEADER
            PRINT COLUMN 28, "ESTADISTICA DE CONDICIONES "
            PRINT COLUMN 30, fecha_ini USING "dd/mm/yyyy",
                              " A ",fecha_fin USING "dd/mm/yyyy"
            PRINT COLUMN 63, "Prog. : AFIL022"
            PRINT COLUMN 63, "Fecha : ",TODAY USING "dd/mm/yyyy"
       PRINT COLUMN 01, "==================================================",
             COLUMN 51, "==============================="
            PRINT COLUMN 5, "SUCURSAL ",
                  COLUMN 18, "FECHA ",
                  COLUMN 37, "COMPLETAS ",
                  COLUMN 51, "RECHAZADAS "

       PRINT COLUMN 01, "==================================================",
             COLUMN 51, "==============================="

    ON EVERY ROW
            PRINT COLUMN 05, sucursal,
                  COLUMN 18, fecha      USING "dd/mm/yyyy",
                  COLUMN 37, completas  USING "########&",
                  COLUMN 52, rechazados USING "########&"

    ON LAST ROW
       SKIP 2 LINE
       PRINT COLUMN 01, "==================================================",
             COLUMN 51, "==============================="
       PRINT COLUMN 05, "T  O  T  A  L  E  S  : ",
             COLUMN 37, tot_completas  USING "########&",
             COLUMN 52, tot_rechazados USING "########&"


END REPORT
