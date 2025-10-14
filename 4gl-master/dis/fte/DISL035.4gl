###########################################################################
#Proyecto               => Sistema de Afores. (MEXICO)                    #
#Propietario            => E.F.P                                          #
#Programa     DISL035   => Reporte
#Fecha                  => 28 de junio 2001                               #
#Por                    => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Modulo                 => DIS.                                           #
###########################################################################
DATABASE safre_af
GLOBALS
    DEFINE vruta_listado        LIKE seg_modulo.ruta_listados

    DEFINE l_reg  RECORD
           nss                  CHAR(11),
           impt_ret             DECIMAL(9,2),
           impt_act_rec_ret     DECIMAL(9,2),
           impt_ces_vej         DECIMAL(9,2),
           impt_act_r_ces_vej   DECIMAL(9,2),
           impt_aport_vol       DECIMAL(9,2),
           impt_cuota_soc       DECIMAL(9,2),
           impt_act_cuo_soc     DECIMAL(9,2),
           impt_aport_est       DECIMAL(9,2),
           impt_act_aport_est   DECIMAL(9,2),
           impt_aport_esp       DECIMAL(9,2),
           impt_act_cuo_esp     DECIMAL(9,2)
    END RECORD

    DEFINE r       RECORD
           nss                  CHAR(11),
           suma_ret             DECIMAL(9,2),
           suma_ces             DECIMAL(9,2),
           suma_vol             DECIMAL(9,2),
           suma_soc             DECIMAL(9,2),
           suma_est             DECIMAL(9,2),
           suma_esp             DECIMAL(9,2)
    END RECORD

    DEFINE vfolio               INTEGER,
           HOY                  DATE,
           usuario              CHAR(8),
           aux_pausa            CHAR(1),
           hora                 CHAR(8),
           g_lista              CHAR(100),
           g_impre              CHAR(100)

END GLOBALS

MAIN

    OPTIONS
        PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o

    DEFER INTERRUPT

    CALL inicio()
    CALL proceso()

END MAIN

FUNCTION inicio()

    SELECT USER
    INTO   usuario
    FROM   glo_parametro

    SELECT ruta_listados
    INTO   vruta_listado
    FROM   seg_modulo
    WHERE  modulo_cod = "dis"

END FUNCTION

FUNCTION proceso()
 
    LET HOY = TODAY
    OPEN WINDOW v1 AT 3,3 WITH FORM "DISL0351" ATTRIBUTE (BORDER)
    DISPLAY " <ESC> Ejecutar                                           <Ctrol-c> Salir      " AT 1,1
    DISPLAY " DISL035            REPORTE DE APORTACIONES RECHAZADAS                         " AT 3,1 ATTRIBUTE (REVERSE) 
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE (REVERSE)

    INPUT BY NAME vfolio
        AFTER FIELD vfolio
            IF vfolio IS NULL THEN
                ERROR "Folio no puede ser nulo,favor verificar."
                SLEEP 3
                ERROR ""
                NEXT FIELD vfolio
            END IF

        ON KEY (ESC)
            IF vfolio IS NULL THEN
                ERROR "Folio no puede ser nulo,favor verificar."
                SLEEP 3
                ERROR ""
                NEXT FIELD vfolio
            END IF

            PROMPT "LISTADO EN IMPRECION [I] o ARCHIVO [A] ? " FOR CHAR aux_pausa

            WHILE TRUE
               IF aux_pausa MATCHES "[IiAa]" THEN
                  EXIT WHILE
               END IF
            END WHILE

            IF aux_pausa MATCHES "[Ii]" THEN
               CALL listado()
            ELSE
               CALL archivo()
            END IF

            CLEAR SCREEN
            EXIT INPUT

        ON KEY (INTERRUPT)
            ERROR "Proceso cancelado ..."
            SLEEP 2
            ERROR ""
            EXIT INPUT
    END INPUT

END FUNCTION

FUNCTION listado()

    LET HOY =  TODAY
    LET hora = TIME

    ERROR "PROCESANDO INFORMACION ..."

    DECLARE cur_1 CURSOR FOR
    SELECT n_seguro, 
           impt_ret       + impt_act_rec_ret,
           impt_ces_vej   + impt_act_r_ces_vej,
           impt_aport_vol,
           impt_cuota_soc + impt_act_cuo_soc,
           impt_aport_est + impt_act_aport_est,
           impt_aport_esp + impt_act_cuo_esp
    FROM   dis_det_aporte
    WHERE  folio   = vfolio
    AND    result_operacion = "02"

    OPEN cur_1
    FETCH cur_1

    IF SQLCA.SQLCODE <> 0 THEN
        ERROR " Folio no existe ..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        CLOSE cur_1
        INITIALIZE vfolio TO NULL
        NEXT FIELD vfolio
    END IF

    LET g_impre = vruta_listado CLIPPED,"/",usuario CLIPPED,
                  ".RECHAZO",
                  HOY USING "DD-MM-YYYY","_",hora CLIPPED

    START REPORT impresion TO g_impre
        FOREACH cur_1 INTO r.*
            OUTPUT TO REPORT impresion(r.*)
        END FOREACH
    FINISH REPORT impresion

    ERROR ""
    ERROR "LISTADO GENERADO ..."
    SLEEP 1
    ERROR ""

    LET g_lista = "vi ",g_impre
    RUN g_lista

END FUNCTION

REPORT impresion(r)

    DEFINE r       RECORD
           nss                  CHAR(11),
           suma_ret             DECIMAL(9,2),
           suma_ces             DECIMAL(9,2),
           suma_vol             DECIMAL(9,2),
           suma_soc             DECIMAL(9,2),
           suma_est             DECIMAL(9,2),
           suma_esp             DECIMAL(9,2)
    END RECORD

    OUTPUT
        TOP MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  0
        PAGE LENGTH  90

    FORMAT
        PAGE HEADER

            PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
            PRINT COLUMN 001,"DISL035",
                  COLUMN 160,HOY USING "DD/MM/YYYY"

            SKIP 2 LINES

            PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B' 

            PRINT COLUMN 30,"REPORTE DE APORTACIONES RECHAZADAS"

            SKIP 1 LINES

            PRINT COLUMN 01,'\033e\033(s218T\033(s11H\033(s7B' 

            PRINT COLUMN 01,"Folio :", vfolio USING "########"

            SKIP 1 LINES

            PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B' 

            PRINT COLUMN  06,"NSS",
                  COLUMN  18,"RETIRO",
                  COLUMN  33,"CESENTIA",
                  COLUMN  48,"VOLUNTARIAS",
                  COLUMN  66,"SOCIAL",
                  COLUMN  81,"ESTATAL",
                  COLUMN  98,"ESPECIAL"

           SKIP 2 LINES

        ON EVERY ROW

            PRINT COLUMN 01,r.nss ,
                  COLUMN 15,r.suma_ret USING "########&.&&",
                  COLUMN 31,r.suma_ces USING "########&.&&",
                  COLUMN 47,r.suma_vol USING "########&.&&",
                  COLUMN 63,r.suma_soc USING "########&.&&",
                  COLUMN 79,r.suma_est USING "########&.&&",
                  COLUMN 96,r.suma_esp USING "########&.&&"

         ON LAST ROW

            SKIP 2 LINES

            PRINT COLUMN 01,"Suma Total :",
                  COLUMN 15,SUM(r.suma_ret) USING "########&.&&",
                  COLUMN 31,SUM(r.suma_ces) USING "########&.&&",
                  COLUMN 47,SUM(r.suma_vol) USING "########&.&&",
                  COLUMN 63,SUM(r.suma_soc) USING "########&.&&",
                  COLUMN 79,SUM(r.suma_est) USING "########&.&&",
                  COLUMN 96,SUM(r.suma_esp) USING "########&.&&"

            SKIP 4 LINE
            PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<<<<<" 

         --PAGE TRAILER
END REPORT

FUNCTION archivo()

    LET HOY =  TODAY
    LET hora = TIME

    ERROR "PROCESANDO INFORMACION ..."

    DECLARE cur_1 CURSOR FOR
    SELECT n_seguro, 
           impt_ret       + impt_act_rec_ret,
           impt_ces_vej   + impt_act_r_ces_vej,
           impt_aport_vol,
           impt_cuota_soc + impt_act_cuo_soc,
           impt_aport_est + impt_act_aport_est,
           impt_aport_esp + impt_act_cuo_esp
    FROM   dis_det_aporte
    WHERE  folio   = vfolio
    AND    result_operacion = "02"

    OPEN cur_1
    FETCH cur_1

    IF SQLCA.SQLCODE <> 0 THEN
        ERROR " Folio no existe ..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        CLOSE cur_1
        INITIALIZE vfolio TO NULL
        NEXT FIELD vfolio
    END IF

    LET g_impre = vruta_listado CLIPPED,"/",usuario CLIPPED,
                  ".ARC_RECHAZO_",
                  HOY USING "DDMMYY","_",hora CLIPPED

    START REPORT archivo TO g_impre
        FOREACH cur_1 INTO r.*
            OUTPUT TO REPORT archivo(r.*)
        END FOREACH
    FINISH REPORT archivo

    ERROR ""
    ERROR "LISTADO GENERADO ..."
    SLEEP 1
    ERROR ""

    LET g_lista = "chmod 777 ",g_impre
    RUN g_lista

END FUNCTION

REPORT impresion(r)

    DEFINE r       RECORD
           nss                  CHAR(11),
           suma_ret             DECIMAL(9,2),
           suma_ces             DECIMAL(9,2),
           suma_vol             DECIMAL(9,2),
           suma_soc             DECIMAL(9,2),
           suma_est             DECIMAL(9,2),
           suma_esp             DECIMAL(9,2)
    END RECORD

    OUTPUT
        TOP MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  0
        PAGE LENGTH  90

    FORMAT
        PAGE HEADER

             PRINT COLUMN  06,"NSS",
                   COLUMN  18,"RETIRO",
                   COLUMN  33,"CESENTIA",
                   COLUMN  48,"VOLUNTARIAS",
                   COLUMN  66,"SOCIAL",
                   COLUMN  81,"ESTATAL",
                   COLUMN  98,"ESPECIAL"

            SKIP 2 LINES

         ON EVERY ROW

            PRINT COLUMN 01,r.nss ,
                  COLUMN 15,r.suma_ret USING "########&.&&",
                  COLUMN 31,r.suma_ces USING "########&.&&",
                  COLUMN 47,r.suma_vol USING "########&.&&",
                  COLUMN 63,r.suma_soc USING "########&.&&",
                  COLUMN 79,r.suma_est USING "########&.&&",
                  COLUMN 96,r.suma_esp USING "########&.&&"

         ON LAST ROW

            SKIP 2 LINES

            PRINT COLUMN 01,"Suma Total :",
                  COLUMN 15,SUM(r.suma_ret) USING "########&.&&",
                  COLUMN 31,SUM(r.suma_ces) USING "########&.&&",
                  COLUMN 47,SUM(r.suma_vol) USING "########&.&&",
                  COLUMN 63,SUM(r.suma_soc) USING "########&.&&",
                  COLUMN 79,SUM(r.suma_est) USING "########&.&&",
                  COLUMN 96,SUM(r.suma_esp) USING "########&.&&"

            SKIP 4 LINE
            PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<<<<<" 

         --PAGE TRAILER
END REPORT
