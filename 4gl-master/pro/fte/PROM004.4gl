################################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Sistema           => PRO.                                                     #
#Programa PROM004  => CONSULTA DE LOTES ENVIADOS Y RECIBIDOS                   #
#Fecha             => 11 DE MAYO DE 1997                                       #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha Modificac.  => 29 DE MARZO DEL 2004                                     #
#MODIFICADO POR    => LAURA EUGENIA CORTES GUZMAN                              #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 9 de Febrero del 2008                                    #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT version     #
#                  => 3.0   (v1)                                               #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1 
        inicio                INTEGER ,
        final                 INTEGER
    END RECORD

    DEFINE reg_3 RECORD #glo #reg_3 
        codven                LIKE pro_solicitud.codven        ,
        cod_promotor          LIKE pro_solicitud.cod_promotor  ,
        rfc                   LIKE pro_solicitud.rfc           ,
        paterno               LIKE pro_solicitud.paterno       ,
        materno               LIKE pro_solicitud.materno       ,
        nombres               LIKE pro_solicitud.nombres
    END RECORD

    DEFINE reg_4 RECORD #glo #reg_4 
        codven                LIKE pro_det_agte.codven        ,
        rfc_letras            LIKE pro_det_agte.rfc_letras    ,
        rfc_numeros           LIKE pro_det_agte.rfc_numeros   ,
        rfc_homonimia         LIKE pro_det_agte.rfc_homonimia ,
        paterno               LIKE pro_det_agte.paterno       ,
        materno               LIKE pro_det_agte.materno       ,
        nombre                LIKE pro_det_agte.nombre        ,
        diag_proceso          LIKE pro_det_agte.diag_proceso
    END RECORD

    DEFINE arr_1 ARRAY [4000] OF RECORD #glo #arr_1
        nro_lote              LIKE pro_ctr_lote.nro_lote         ,
        fecha_envio           LIKE pro_ctr_lote.fecha_envio      ,
        fecha_recepcion       LIKE pro_ctr_lote.fecha_recepcion  ,
        nro_de_registros      LIKE pro_ctr_lote.nro_de_registros
    END RECORD

    DEFINE arr_2 ARRAY [1000] OF RECORD #glo #arr_2
        marca                 CHAR(1)                         ,
        nro_lote              LIKE pro_ctr_envio.nro_lote         ,
        fecha_genera          LIKE pro_ctr_envio.fecha_genera     ,
        des_operacion         CHAR(19)                        ,
        nro_de_registros      LIKE pro_ctr_envio.nro_de_registros
    END RECORD

    DEFINE arr_3 ARRAY [1000] OF RECORD #glo #arr_3
        marca                 CHAR(1)                          ,
        codven                LIKE pro_solicitud.codven        ,
        cod_promotor          LIKE pro_solicitud.cod_promotor  ,
        rfc                   LIKE pro_solicitud.rfc           ,
        nombres               LIKE pro_solicitud.nombres
    END RECORD

    DEFINE arr_4 ARRAY [1000] OF RECORD #glo #arr_4
        marca                 CHAR(1)                           ,
        cod_promotor          LIKE pro_det_agte.cod_promotor    ,
        rfc                   CHAR(13)                          ,
        nombres               CHAR(30)                          ,
        diag_proceso          LIKE pro_det_agte.diag_proceso 
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(1)

    DEFINE #glo #smallint
        scr_l                 ,
        arr_c                 SMALLINT

    DEFINE #glo #integer
        i                     INTEGER
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        PROMPT LINE LAST     ,
        INPUT WRAP           ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG ("PROM004.log")
    CALL init() #i
    OPEN WINDOW prom0041 AT 3,2 WITH FORM "PROM0041" ATTRIBUTE( BORDER)
    DISPLAY " PROM004          CONSULTA DE LOTES ENVIADOS Y RECIBIDOS                       " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY "                             <Ctrl-C> = Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
        	
    INPUT BY NAME reg_1.* WITHOUT DEFAULTS
        AFTER FIELD inicio
            IF reg_1.inicio IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD inicio
            END IF

        AFTER FIELD final
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD inicio 
            END IF 

            IF reg_1.final IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD final
            END IF

        ON KEY (ESC)
            IF reg_1.inicio IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD inicio
            END IF

            IF reg_1.final IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD final
            END IF

            EXIT INPUT
    END INPUT
    CALL primer_paso() #pp
END MAIN

FUNCTION init()
#--------------
    LET HOY          = TODAY

    SELECT MIN(nro_lote) ,
           MAX(nro_lote) 
    INTO   reg_1.*
    FROM   pro_ctr_lote
END FUNCTION

FUNCTION inicializa()
#--------------------
    DEFINE
        i                    SMALLINT

    INITIALIZE arr_1 TO NULL

    FOR i = 1 TO 11
        DISPLAY arr_1[i].* TO scr_1[i].* ATTRIBUTE(NORMAL)
    END FOR
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DEFINE #loc #integer
        i_nro_lote            INTEGER

    DECLARE cur_1 CURSOR FOR
    SELECT *
    FROM   pro_ctr_lote
    WHERE  nro_lote BETWEEN reg_1.inicio AND reg_1.final 
    ORDER BY nro_lote

    LET i = 1
    FOREACH cur_1 INTO arr_1[i].*
        IF i = 4000 THEN
            ERROR "SOBREPASO CAPACIDAD MAXIMA DEL ARREGLO"
            EXIT FOREACH
        END IF
        LET i = i + 1
    END FOREACH
            
    CALL SET_COUNT(i-1)

    INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.*
        BEFORE FIELD nro_lote
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()
            
        ON KEY (CONTROL-M)
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()
            LET  i_nro_lote = arr_1[arr_c].nro_lote

            IF arr_1[arr_c].fecha_envio IS NOT NULL THEN
                CALL envio(i_nro_lote)     #e
            ELSE
                CALL recepcion(i_nro_lote) #r
            END IF
    END INPUT
END FUNCTION

FUNCTION envio(i_nro_lote)
#e------------------------
    DEFINE #loc #smallint
        i                     SMALLINT

    DEFINE #loc #integer
        i_nro_lote            INTEGER

    OPEN WINDOW prom0042 AT 7,5 WITH FORM "PROM0042" ATTRIBUTE(BORDER)
    DISPLAY "                         DETALLE DEL LOTE                                      " AT 1,1 ATTRIBUTE(REVERSE)

        DECLARE cur_2 CURSOR FOR
        SELECT ""               ,#marca
               nro_lote         ,
               fecha_genera     ,
               tipo_operacion   ,
               nro_de_registros
        FROM   pro_ctr_envio
        WHERE  nro_lote = i_nro_lote

        LET i = 1
        FOREACH cur_2 INTO arr_2[i].*
            CASE arr_2[i].des_operacion 
                WHEN "ALT"
                    LET arr_2[i].des_operacion = "ALTA"

                WHEN "REA"
                    LET arr_2[i].des_operacion = "REAC. 301"                              --(v1)  

                WHEN "REC"
                    LET arr_2[i].des_operacion = "RECHAZO-REENVIADO"

                WHEN "REE"                                                                    --(v1)1
                    LET arr_2[i].des_operacion = "REAC. 308 "                             --(v1)

                WHEN "BAJ"
                    LET arr_2[i].des_operacion = "BAJA"

                WHEN "REV"
                    LET arr_2[i].des_operacion = "REVALIDACION"

                WHEN "MOD"
                    LET arr_2[i].des_operacion = "MODIFICACION"
            END CASE

            LET i = i + 1
        END FOREACH

        CALL SET_COUNT(i-1)

        INPUT ARRAY arr_2 WITHOUT DEFAULTS FROM scr_1.*
            BEFORE FIELD marca 
                LET arr_c = ARR_CURR()
                LET scr_l = SCR_LINE()

            ON KEY (CONTROL-M)
               CASE arr_2[arr_c].des_operacion 
                   WHEN "ALTA"
                       CALL envios_altas() #ea

                   WHEN "REAC. 308"                                                         --(v1)2
                       CALL envios_reactivaciones() #eee                                    --(v1)
							  
                   WHEN "REAC. 301"                                                         --(v1)
                       CALL envios_reactivacion() #er                                       --(v1) 

                   WHEN "RECHAZO-REENVIADO"
                       CALL envios_rechazos() #er

                   WHEN "BAJA"
                       CALL envios_bajas(arr_2[arr_c].fecha_genera) #eb

                   WHEN "REVALIDACION"
                       CALL envios_revalidacion(arr_2[arr_c].fecha_genera) #er

                   WHEN "MODIFICACION"
                       CALL envios_modificacion(arr_2[arr_c].fecha_genera) #em
               END CASE
        END INPUT
    CLOSE WINDOW prom0042
END FUNCTION

FUNCTION recepcion(i_nro_lote)
#r----------------------------
    DEFINE #loc #smallint
        i                     SMALLINT

    DEFINE #loc #integer
        i_nro_lote            INTEGER

    OPEN WINDOW prom0042 AT 7,5 WITH FORM "PROM0042" ATTRIBUTE(BORDER)
    DISPLAY "                          DETALLE DEL LOTE                                     " AT 1,1 ATTRIBUTE(REVERSE)
        DECLARE cur_4 CURSOR FOR
        SELECT ""               ,#marca
               nro_lote         ,
               fecha_genera     ,
               tipo_operacion   ,
               nro_de_registros
        FROM   pro_recepcion
        WHERE  nro_lote = i_nro_lote

        LET i = 1
        FOREACH cur_4 INTO arr_2[i].*
            CASE arr_2[i].des_operacion
                WHEN "301"
                    LET arr_2[i].des_operacion = "ALTA"

                WHEN "302"
                    LET arr_2[i].des_operacion = "REVALIDACIONES"

                WHEN "401"
                    LET arr_2[i].des_operacion = "RECHAZOS REENVIADOS"

                WHEN "801"
                    LET arr_2[i].des_operacion = "REAC. 301"    --(v1)

                WHEN "303"
                    LET arr_2[i].des_operacion = "BAJA"

                WHEN "304"
                    LET arr_2[i].des_operacion = "MODIFICACION"

                WHEN "306"
                    LET arr_2[i].des_operacion = "AVISO EXAMEN"

                WHEN "307"
                    LET arr_2[i].des_operacion = "RESULTADO EXAMEN"

                WHEN "308"                                      --(v1)
                    LET arr_2[i].des_operacion = "REAC. 308"
            END CASE
            LET i = i + 1
        END FOREACH

        CALL SET_COUNT(i-1)

        INPUT ARRAY arr_2 WITHOUT DEFAULTS FROM scr_1.*
            BEFORE FIELD marca 
                LET arr_c = ARR_CURR()
                LET scr_l = SCR_LINE()

            ON KEY (CONTROL-M)
               CASE arr_2[arr_c].des_operacion 
                   WHEN "ALTA"
                       CALL recep_altas() #ra

                   WHEN "REAC. 308"                                            --(v1)3
                       CALL recep_reactivaciones() #ree                             --(v1)

                   WHEN "REVALIDACIONES"
                       CALL recep_revalidacion(arr_2[arr_c].nro_lote) #rr

                   WHEN "RECHAZOS REENVIADOS"
                       CALL recep_rechazos_reenviados() #rrr

                   WHEN "REAC. 301"
                       CALL recep_reactivacion() #rr

                   WHEN "BAJA"
                       CALL recep_bajas(arr_2[arr_c].nro_lote) #rb

                   WHEN "MODIFICACION"
                       CALL recep_modificacion(arr_2[arr_c].nro_lote) #rm

                   WHEN "AVISO EXAMEN"
                       CALL recep_aviso_examen(arr_2[arr_c].nro_lote) #rae

                   WHEN "RESULTADO EXAMEN"
                       CALL recep_resultado_examen(arr_2[arr_c].nro_lote) #rre
               END CASE

        END INPUT
    CLOSE WINDOW prom0042
END FUNCTION

FUNCTION envios_altas()
#ea--------------------
    OPEN WINDOW prom0044 AT 9,5 WITH FORM "PROM0044"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL CANDIDATO A PROMOTOR                              " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_3 CURSOR FOR 
    SELECT  A.codven       ,
            A.cod_promotor ,
            A.rfc          ,
            A.paterno      ,
            A.materno      ,
            A.nombres
    FROM    pro_envio_alta B, pro_solicitud A
    WHERE   B.fenvio         = arr_2[arr_c].fecha_genera
    AND     B.status_interno = 1
--ORI    AND     B.nro_solicitud         = A.nro_solicitud
    AND     B.codven                = A.codven       
         
    LET i = 1
    FOREACH cur_3 INTO reg_3.*
        LET arr_3[i].codven       = reg_3.codven       
        LET arr_3[i].cod_promotor = reg_3.cod_promotor
        LET arr_3[i].rfc          = reg_3.rfc
        LET arr_3[i].nombres      = reg_3.paterno CLIPPED," ",
                                    reg_3.materno CLIPPED," ",
                                    reg_3.nombres CLIPPED
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_3 TO scr_3.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0044
END FUNCTION

FUNCTION envios_reactivacion()
#er---------------------------
    OPEN WINDOW prom0044 AT 9,5 WITH FORM "PROM0044"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL CANDIDATO A PROMOTOR                    " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_8 CURSOR FOR 
    SELECT  A.codven       ,
            A.cod_promotor ,
            A.rfc          ,
            A.paterno      ,
            A.materno      ,
            A.nombres
    FROM    pro_envio_alta B, pro_solicitud A
    WHERE   B.fenvio         = arr_2[arr_c].fecha_genera
    AND     B.status_interno = 8
    AND     B.nro_solicitud         = A.nro_solicitud
                        
    LET i = 1
    FOREACH cur_8 INTO reg_3.*
        LET arr_3[i].codven       = reg_3.codven       
        LET arr_3[i].cod_promotor = reg_3.cod_promotor
        LET arr_3[i].rfc          = reg_3.rfc
        LET arr_3[i].nombres      = reg_3.paterno CLIPPED," ",
                                    reg_3.materno CLIPPED," ",
                                    reg_3.nombres CLIPPED
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_3 TO scr_3.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0044
END FUNCTION

FUNCTION envios_reactivaciones()              --(v1)4
#er---------------------------
    OPEN WINDOW prom0044 AT 9,5 WITH FORM "PROM0044"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL CANDIDATO A PROMOTOR                    " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_81 CURSOR FOR
    SELECT  A.codven       ,
            A.cod_promotor ,
            A.rfc          ,
            A.paterno      ,
            A.materno      ,
            A.nombres
    FROM    pro_envio_reac B, pro_solicitud A
    WHERE   B.fenvio         = arr_2[arr_c].fecha_genera
    AND     B.status_interno = 8
    AND     B.nro_solicitud         = A.nro_solicitud

    LET i = 1
    FOREACH cur_81 INTO reg_3.*
        LET arr_3[i].codven       = reg_3.codven
        LET arr_3[i].cod_promotor = reg_3.cod_promotor
        LET arr_3[i].rfc          = reg_3.rfc
        LET arr_3[i].nombres      = reg_3.paterno CLIPPED," ",
                                    reg_3.materno CLIPPED," ",
                                    reg_3.nombres CLIPPED
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_3 TO scr_3.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0044
END FUNCTION

FUNCTION envios_rechazos()
#er-----------------------
    OPEN WINDOW prom0044 AT 9,5 WITH FORM "PROM0044"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL CANDIDATO A PROMOTOR                    " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_10 CURSOR FOR 
    SELECT  A.codven       ,
            A.cod_promotor ,
            A.rfc          ,
            A.paterno      ,
            A.materno      ,
            A.nombres
    FROM    pro_envio_alta B, pro_solicitud A
    WHERE   B.fenvio         = arr_2[arr_c].fecha_genera
    AND     B.status_interno = 21
    AND     B.nro_solicitud         = A.nro_solicitud

    LET i = 1
    FOREACH cur_10 INTO reg_3.*
        LET arr_3[i].codven       = reg_3.codven       
        LET arr_3[i].cod_promotor = reg_3.cod_promotor
        LET arr_3[i].rfc          = reg_3.rfc
        LET arr_3[i].nombres      = reg_3.paterno CLIPPED," ",
                                    reg_3.materno CLIPPED," ",
                                    reg_3.nombres CLIPPED
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_3 TO scr_3.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0044
END FUNCTION

FUNCTION envios_bajas(d_fecha_genera) 
#eb----------------------------------
    DEFINE arr_6 ARRAY[500] OF RECORD #glo #arr_6
        marca                 CHAR(1)                            ,
        fecha_genera          LIKE pro_envio_scb.fecha_genera    ,
        codven                LIKE pro_mae_promotor.codven       ,
        cod_promotor          LIKE pro_mae_promotor.cod_promotor ,
        motivo_suspende       LIKE pro_envio_scb.motivo_suspende ,
        fecha_baja            LIKE pro_envio_scb.fecha_baja
    END RECORD

    DEFINE #loc #date
        d_fecha_genera        DATE

    OPEN WINDOW prom0046 AT 9,5 WITH FORM "PROM0046"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL CANDIDATO A PROMOTOR                              " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_6 CURSOR FOR 
    SELECT  ""                ,
            A.fecha_genera    ,
            B.codven          ,
            A.cod_promotor    ,
            A.motivo_suspende ,
            A.fecha_baja
    FROM    pro_envio_scb A, pro_mae_promotor B
    WHERE   A.fecha_genera = d_fecha_genera
    AND     A.cod_promotor = B.cod_promotor
                        
    LET i = 1
    FOREACH cur_6 INTO arr_6[i].*
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_6 TO scr_6.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0046
END FUNCTION

FUNCTION envios_revalidacion(d_fecha_genera) 
#er-----------------------------------------
    DEFINE arr_7 ARRAY[100] OF RECORD #glo #arr_7
        marca                 CHAR(1)                    ,
        cod_promotor          LIKE pro_envio_scb.cod_promotor ,
        calificacion          LIKE pro_capacitacion.calificacion ,
        horas                 LIKE pro_capacitacion.horas        ,
        fecha_genera          LIKE pro_capacitacion.fecha_genera 
    END RECORD

    DEFINE #loc #date
        d_fecha_genera        DATE

    OPEN WINDOW prom0047 AT 10,5 WITH FORM "PROM0047"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL CANDIDATO A PROMOTOR                              " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_7 CURSOR FOR 
    SELECT  ""             ,
            A.cod_promotor ,
            A.calificacion ,
            A.horas        ,
            A.fecha_genera
    FROM    pro_capacitacion A
    WHERE   A.fecha_genera = d_fecha_genera
                        
    LET i = 1
    FOREACH cur_7 INTO arr_7[i].*
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_7 TO scr_7.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0047
END FUNCTION

FUNCTION envios_modificacion(d_fecha_genera)
#em-----------------------------------------
    DEFINE arr_8 ARRAY[300] OF RECORD #loc #arr_8 
        marca                 CHAR(1)                            ,
        codven                LIKE pro_mae_promotor.codven       ,
        cod_promotor          LIKE pro_mae_promotor.cod_promotor ,
        rfc                   LIKE pro_mae_promotor.rfc          ,
        nombres               LIKE pro_mae_promotor.nombres
    END RECORD

    DEFINE reg_6 RECORD #loc #reg_6 
        codven                LIKE pro_mae_promotor.codven       ,
        cod_promotor          LIKE pro_mae_promotor.cod_promotor ,
        rfc                   LIKE pro_mae_promotor.rfc          ,
        paterno               LIKE pro_mae_promotor.paterno      ,
        materno               LIKE pro_mae_promotor.materno      ,
        nombres               LIKE pro_mae_promotor.nombres
    END RECORD

    DEFINE #loc #date
        d_fecha_genera        DATE

    OPEN WINDOW prom0048 AT 9,5 WITH FORM "PROM0048"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL PROMOTOR MODIFICADO                               " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_12 CURSOR FOR 
	 #ff
    SELECT  B.codven        ,
            B.cod_promotor  ,
            B.rfc           ,
            B.paterno       ,
            B.materno       ,
            B.nombres
    FROM    pro_envio_mod A, pro_mae_promotor B
    WHERE   A.fenvio       = d_fecha_genera
    AND     A.cod_promotor = B.cod_promotor
                        
    LET i = 1
    FOREACH cur_12 INTO reg_6.*
        LET arr_8[i].marca        = NULL
        LET arr_8[i].codven       = reg_6.codven
        LET arr_8[i].cod_promotor = reg_6.cod_promotor
        LET arr_8[i].rfc          = reg_6.rfc
        LET arr_8[i].nombres      = reg_6.paterno CLIPPED," ",
                                    reg_6.materno CLIPPED," ",
                                    reg_6.nombres CLIPPED
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_8 TO scr_8.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
    END DISPLAY

    CLOSE WINDOW prom0048
END FUNCTION

FUNCTION recep_bajas(i_folio)
#rb--------------------------
    DEFINE arr_5 ARRAY [100] OF RECORD #glo #arr_5
        marca                 CHAR(1)                          ,
        folio                 LIKE pro_recep_scb.folio         ,
        cod_promotor          LIKE pro_recep_scb.cod_promotor  ,
        fecha_scb             LIKE pro_recep_scb.fecha_scb     ,
        cve_scb               LIKE pro_recep_scb.cve_scb       ,
        fecha_proceso         LIKE pro_recep_scb.fecha_proceso ,
        lote_afore            LIKE pro_recep_scb.lote_afore
    END RECORD

    DEFINE #loc #integer
        i_folio               INTEGER

    OPEN WINDOW prom0045 AT 10,5 WITH FORM "PROM0045" ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL PROMOTOR DADO DE BAJA                   " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_05 CURSOR FOR 
    SELECT  ""            ,#marca
            folio         ,
            cod_promotor  ,
            fecha_scb     ,
            cve_scb       ,
            fecha_proceso ,
            lote_afore
    FROM    pro_recep_scb 
    WHERE   folio = i_folio
                        
    LET i = 1
    FOREACH cur_05 INTO arr_5[i].*
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_5 TO scr_5.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0045
END FUNCTION

FUNCTION recep_modificacion(i_folio)
#rm-----------------------------------
    DEFINE arr_10 ARRAY[100] OF RECORD #loc #arr_6 
        marca                 CHAR(1)                          ,
        cod_promotor          LIKE pro_resul_mod.cod_promotor  ,
        fecha_proceso         LIKE pro_resul_mod.fecha_proceso ,
	diag_proceso          LIKE pro_resul_mod.diag_proceso  ,
        nombres               LIKE pro_resul_mod.paterno
    END RECORD

    DEFINE reg_10 RECORD #loc #reg_10 
		  marca                 CHAR(1)                          ,
        cod_promotor          LIKE pro_resul_mod.cod_promotor  ,
        fecha_proceso         LIKE pro_resul_mod.fecha_proceso ,
		  diag_proceso          LIKE pro_resul_mod.diag_proceso  ,
        paterno               LIKE pro_resul_mod.paterno       ,
        materno               LIKE pro_resul_mod.materno       ,
        nombre                LIKE pro_resul_mod.nombre 
    END RECORD

    DEFINE #loc #integer
        i_folio               INTEGER

    OPEN WINDOW prom0040 AT 10,5 WITH FORM "PROM0040" ATTRIBUTE(BORDER)
    DISPLAY "                 RESULTADO DE ACTUALIZACION DE DATOS                           " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_13 CURSOR FOR 
    SELECT  ""            ,#marca
            cod_promotor  ,
            fecha_proceso ,
            diag_proceso  ,
            paterno       ,
            materno       ,
	    nombre
    FROM    pro_resul_mod 
    WHERE   folio = i_folio
                        
    LET i = 1
    FOREACH cur_13 INTO reg_10.*
        LET arr_10[i].marca        = NULL
        LET arr_10[i].cod_promotor = reg_10.cod_promotor
        LET arr_10[i].fecha_proceso= reg_10.fecha_proceso
        LET arr_10[i].diag_proceso = reg_10.diag_proceso
        LET arr_10[i].nombres      = reg_10.paterno CLIPPED," ",
                                     reg_10.materno CLIPPED," ",
                                     reg_10.nombre CLIPPED
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_10 TO scr_10.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0040
END FUNCTION

FUNCTION recep_aviso_examen(i_folio)
#rae--------------------------------
    DEFINE arr_5 ARRAY [100] OF RECORD #glo #arr_5
        marca                 CHAR(01)                             ,
        cod_promotor          LIKE pro_aviso_examen.cod_promotor   ,
        fecha_examen          LIKE pro_aviso_examen.fecha_examen   ,
        horario_examen        LIKE pro_aviso_examen.horario_examen ,
        nombres               CHAR(30)
    END RECORD

    DEFINE arr_6 ARRAY [100] OF RECORD #glo #arr_6
        marca                 CHAR(01)                             ,
        cod_promotor          LIKE pro_aviso_examen.cod_promotor   ,
        fecha_examen          LIKE pro_aviso_examen.fecha_examen   ,
        horario_examen        LIKE pro_aviso_examen.horario_examen ,
        nombres               LIKE pro_aviso_examen.nombres        ,
        paterno               LIKE pro_aviso_examen.paterno        ,
        materno               LIKE pro_aviso_examen.materno
    END RECORD

    DEFINE #loc #integer
        i_folio               INTEGER

    OPEN WINDOW prom0049 AT 10,5 WITH FORM "PROM0049" ATTRIBUTE(BORDER)
    DISPLAY "                     PROMOTORES CITADOS A EXAMEN                               " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_07 CURSOR FOR 
    SELECT  ""             ,#marca
            cod_promotor   ,
            fecha_examen   ,
            horario_examen ,
            nombres        ,
            paterno        ,
            materno 
    FROM    pro_aviso_examen 
    WHERE   folio = i_folio
                        
    LET i = 1
    FOREACH cur_07 INTO arr_6[i].*
        LET arr_5[i].marca          = arr_6[i].marca
        LET arr_5[i].cod_promotor   = arr_6[i].cod_promotor 
        LET arr_5[i].fecha_examen   = arr_6[i].fecha_examen
        LET arr_5[i].horario_examen = arr_6[i].horario_examen
        LET arr_5[i].nombres        = arr_6[i].nombres CLIPPED," ",
                                      arr_6[i].paterno CLIPPED," ",
                                      arr_6[i].materno CLIPPED
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_5 TO scr_9.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0049
END FUNCTION

FUNCTION recep_altas()
#ra-------------------
    DEFINE reg_5 RECORD #loc #reg_5 
        cod_promotor          LIKE pro_solicitud.cod_promotor ,
        rfc_letras            LIKE pro_det_agte.rfc_letras    ,
        rfc_numeros           LIKE pro_det_agte.rfc_numeros   ,
        rfc_homonimia         LIKE pro_det_agte.rfc_homonimia ,
        paterno               LIKE pro_solicitud.paterno      ,
        materno               LIKE pro_solicitud.materno      ,
        nombres               LIKE pro_solicitud.nombres      ,
        diag_proceso          LIKE pro_solicitud.diag_proceso
    END RECORD

    OPEN WINDOW prom0043 AT 9,5 WITH FORM "PROM0043"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL CANDIDATO A ",
            "PROMOTOR                    " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_5 CURSOR FOR 
    SELECT  A.cod_promotor ,
            A.rfc_letras   ,
            A.rfc_numeros  ,
            A.rfc_homonimia,
            A.paterno      ,
            A.materno      ,
            A.nombre       ,
            A.diag_proceso
    FROM    pro_det_agte A
    WHERE   A.folio          = arr_2[arr_c].nro_lote
    AND     A.status_interno in (4,6,9)

    LET i = 1
    FOREACH cur_5 INTO reg_5.*
        LET arr_4[i].marca        = ""
        LET arr_4[i].cod_promotor = reg_5.cod_promotor
        LET arr_4[i].rfc          = reg_5.rfc_letras  CLIPPED      ,
                                    reg_5.rfc_numeros USING"&&&&&&",
                                    reg_5.rfc_homonimia
        LET arr_4[i].nombres      = reg_5.nombres

        LET arr_4[i].diag_proceso = reg_5.diag_proceso
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_4 TO scr_3.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0043
END FUNCTION

FUNCTION recep_rechazos_reenviados()
#rrr--------------------------------
    DEFINE reg_5 RECORD #loc #reg_5 
        cod_promotor          LIKE pro_solicitud.cod_promotor ,
        rfc_letras            LIKE pro_det_agte.rfc_letras    ,
        rfc_numeros           LIKE pro_det_agte.rfc_numeros   ,
        rfc_homonimia         LIKE pro_det_agte.rfc_homonimia ,
        paterno               LIKE pro_solicitud.paterno      ,
        materno               LIKE pro_solicitud.materno      ,
        nombres               LIKE pro_solicitud.nombres      ,
        diag_proceso          LIKE pro_solicitud.diag_proceso
    END RECORD

    OPEN WINDOW prom0043 AT 9,5 WITH FORM "PROM0043"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL CANDIDATO A ",
            "PROMOTOR                              " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_11 CURSOR FOR 
    SELECT  A.cod_promotor ,
            A.rfc_letras   ,
            A.rfc_numeros  ,
            A.rfc_homonimia,
            A.paterno      ,
            A.materno      ,
            A.nombre       ,
            A.diag_proceso
    FROM    pro_det_agte A
    WHERE   A.folio          = arr_2[arr_c].nro_lote
    AND     A.status_interno = 41
                        
    LET i = 1
    FOREACH cur_11 INTO reg_5.*
        LET arr_4[i].marca        = ""
        LET arr_4[i].cod_promotor = reg_5.cod_promotor
        LET arr_4[i].rfc          = reg_5.rfc_letras  CLIPPED      ,
                                    reg_5.rfc_numeros USING"&&&&&&",
                                    reg_5.rfc_homonimia
        LET arr_4[i].nombres      = reg_5.nombres

        LET arr_4[i].diag_proceso = reg_5.diag_proceso
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_4 TO scr_3.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0043
END FUNCTION

FUNCTION recep_reactivacion()
#rr--------------------------
    DEFINE reg_5 RECORD #loc #reg_5 
        cod_promotor          LIKE pro_solicitud.cod_promotor ,
        rfc_letras            LIKE pro_det_agte.rfc_letras    ,
        rfc_numeros           LIKE pro_det_agte.rfc_numeros   ,
        rfc_homonimia         LIKE pro_det_agte.rfc_homonimia ,
        paterno               LIKE pro_solicitud.paterno      ,
        materno               LIKE pro_solicitud.materno      ,
        nombres               LIKE pro_solicitud.nombres      ,
        diag_proceso          LIKE pro_solicitud.diag_proceso
    END RECORD

    OPEN WINDOW prom0043 AT 9,5 WITH FORM "PROM0043"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL CANDIDATO A ",
            "PROMOTOR                    " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_9 CURSOR FOR 
    SELECT  A.cod_promotor ,
            A.rfc_letras   ,
            A.rfc_numeros  ,
            A.rfc_homonimia,
            A.paterno      ,
            A.materno      ,
            A.nombre       ,
            A.diag_proceso
    FROM    pro_det_agte A
    WHERE   A.folio          = arr_2[arr_c].nro_lote
    AND     A.status_interno = 40
                        
    LET i = 1
    FOREACH cur_9 INTO reg_5.*
        LET arr_4[i].marca        = ""
        LET arr_4[i].cod_promotor = reg_5.cod_promotor
        LET arr_4[i].rfc          = reg_5.rfc_letras  CLIPPED      ,
                                    reg_5.rfc_numeros USING"&&&&&&",
                                    reg_5.rfc_homonimia
        LET arr_4[i].nombres      = reg_5.nombres

        LET arr_4[i].diag_proceso = reg_5.diag_proceso
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_4 TO scr_3.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0043
END FUNCTION

FUNCTION recep_reactivaciones()   --(v1)5
#rr--------------------------
    DEFINE reg_5 RECORD #loc #reg_5
        cod_promotor          LIKE pro_solicitud.cod_promotor ,
        rfc_letras            LIKE pro_det_agte.rfc_letras    ,
        rfc_numeros           LIKE pro_det_agte.rfc_numeros   ,
        rfc_homonimia         LIKE pro_det_agte.rfc_homonimia ,
        paterno               LIKE pro_solicitud.paterno      ,
        materno               LIKE pro_solicitud.materno      ,
        nombres               LIKE pro_solicitud.nombres      ,
        diag_proceso          LIKE pro_solicitud.diag_proceso
    END RECORD

    OPEN WINDOW prom0043 AT 9,5 WITH FORM "PROM0043"ATTRIBUTE(BORDER)
    DISPLAY "                   DATOS DEL CANDIDATO A ",
            "PROMOTOR                    " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_91 CURSOR FOR
    SELECT  A.cod_promotor ,
            A.rfc_letras   ,
            A.rfc_numeros  ,
            A.rfc_homonimia,
            A.paterno      ,
            A.materno      ,
            A.nombre       ,
            A.diag_proceso
    FROM    pro_det_agte A
    WHERE   A.folio          = arr_2[arr_c].nro_lote
    AND     A.status_interno = 40
    AND     A.diag_proceso in ("7E", "7T")                                        --(v1)
    AND     A.cve_afore_consar = "308"                                            --(v1)

    LET i = 1
    FOREACH cur_91 INTO reg_5.*
        LET arr_4[i].marca        = ""
        LET arr_4[i].cod_promotor = reg_5.cod_promotor
        LET arr_4[i].rfc          = reg_5.rfc_letras  CLIPPED      ,
                                    reg_5.rfc_numeros USING"&&&&&&",
                                    reg_5.rfc_homonimia
        LET arr_4[i].nombres      = reg_5.nombres

        LET arr_4[i].diag_proceso = reg_5.diag_proceso
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)


    DISPLAY ARRAY arr_4 TO scr_3.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom0043
END FUNCTION


#ff
FUNCTION recep_revalidacion(i_folio)
#rr---------------------------------
    DEFINE arr_14 ARRAY[100] OF RECORD 
	       marca                 CHAR(1)                            ,
               cod_promotor          LIKE pro_det_revalida.cod_promotor ,
               fecha_reval           LIKE pro_det_revalida.fecha_reval  ,
	       diag_reval            LIKE pro_det_revalida.diag_reval   ,
               nombres               LIKE pro_mae_promotor.nombres
	   END RECORD,

           reg_14 RECORD 
               marca                 CHAR(1)                            ,
               cod_promotor          LIKE pro_det_revalida.cod_promotor ,
               fecha_reval           LIKE pro_det_revalida.fecha_reval  ,
               diag_reval            LIKE pro_det_revalida.diag_reval   ,
               paterno               LIKE pro_mae_promotor.paterno      ,
               materno               LIKE pro_mae_promotor.materno      ,
               nombres               LIKE pro_mae_promotor.nombres
           END RECORD,

           i_folio               INTEGER

    OPEN WINDOW prom00410 AT 10,5 WITH FORM "PROM00410" ATTRIBUTE(BORDER)
    DISPLAY "                      RESULTADO DE REVALIDA",
            "CION                                " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_14 CURSOR FOR 
    SELECT  ""             ,#marca
            A.cod_promotor ,
            A.fecha_reval  ,
            A.diag_reval   ,
            B.paterno      ,
            B.materno      ,
	    B.nombres
    FROM    pro_det_revalida A, pro_mae_promotor B
    WHERE   A.folio        = i_folio
      AND   A.cod_promotor = B.cod_promotor
                        
    LET i = 1
    FOREACH cur_14 INTO reg_14.*
        LET arr_14[i].marca        = NULL
        LET arr_14[i].cod_promotor = reg_14.cod_promotor
        LET arr_14[i].fecha_reval  = reg_14.fecha_reval  
        LET arr_14[i].diag_reval   = reg_14.diag_reval  
        LET arr_14[i].nombres      = reg_14.paterno CLIPPED," ",
                                     reg_14.materno CLIPPED," ",
                                     reg_14.nombres CLIPPED
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_14 TO scr_14.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom00410
END FUNCTION

FUNCTION recep_resultado_examen(i_folio)
#rre------------------------------------
    DEFINE arr_11 ARRAY[100] OF RECORD #loc #arr_11
		  marca                 CHAR(1)                            ,
        cod_promotor          LIKE pro_resul_examen.cod_promotor ,
	calificacion          LIKE pro_resul_examen.calificacion ,
        diag_proceso          LIKE pro_resul_examen.diag_proceso ,
        nombres               LIKE pro_mae_promotor.nombres
    END RECORD

    DEFINE reg_20 RECORD #loc #reg_20 
        cod_promotor          LIKE pro_resul_examen.cod_promotor ,
	calificacion          LIKE pro_resul_examen.calificacion ,
        diag_proceso          LIKE pro_resul_examen.diag_proceso ,
        paterno               LIKE pro_mae_promotor.paterno      ,
        materno               LIKE pro_mae_promotor.materno      ,
        nombres               LIKE pro_mae_promotor.nombres
    END RECORD

	 DEFINE #loc #smallint
		  i_folio               SMALLINT

    OPEN WINDOW prom00411 AT 9,5 WITH FORM "PROM00411"ATTRIBUTE(BORDER)
    DISPLAY "                        RESULTADO DEL ",
            "EXAMEN                                   " 
            AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_20 CURSOR FOR 
            SELECT  A.cod_promotor ,
                    A.calificacion ,
                    A.diag_proceso ,
                    B.paterno      ,
                    B.materno      ,
                    B.nombres
            FROM    pro_resul_examen A, pro_mae_promotor B
            WHERE   A.folio        = i_folio
	    AND     A.cod_promotor = B.cod_promotor

    LET i = 1
    FOREACH cur_20 INTO reg_20.*
        LET arr_11[i].marca        = ""
        LET arr_11[i].cod_promotor = reg_20.cod_promotor
        LET arr_11[i].calificacion = reg_20.calificacion
        LET arr_11[i].diag_proceso = reg_20.diag_proceso
        LET arr_11[i].nombres      = reg_20.paterno CLIPPED," ",
												reg_20.materno CLIPPED," ",
												reg_20.nombres
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_11 TO scr_11.*
        ON KEY (INTERRUPT)
            EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW prom00411
END FUNCTION
