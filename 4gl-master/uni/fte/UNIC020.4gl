################################################################################
#Owner             => E.F.P.
#Programa RETC020  => RECIBE ARCHIVOS DE RESPUESTA DE CONFRONTA            
#Por               => ARMANDO RODRIGUEZ CASTROPAREDES
#Fecha creacion    => 30 DE MARZO DEL 2001     
#Por               => ARMANDO RODRIGUEZ CASTROPAREDES
#Sistema           => UNI
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE
        g_param_uni           RECORD LIKE seg_modulo.* 

    DEFINE 
        HOY                   DATE
     
    DEFINE #char
        carga_reg             CHAR(330),
        usuario               CHAR(008),
        enter    	            CHAR(001),
        generar               CHAR(018),
        nombre_archivo        CHAR(018),
        archivo_traspaso      CHAR(200),
        c10_fecha_presenta    CHAR(010),
        c10_fecha_nac_uni     CHAR(010),
        c10_fecha_nac_cta1    CHAR(010) 

    DEFINE #glo #smallint
        s_recibido            ,
        s_confrontado         ,
        s_aceptado            ,
        s_solicitado          ,
        cuantos               ,
        cont                  ,
		  cont1                 ,
		  cont2                 SMALLINT

    DEFINE #integer
        ultimo_folio          INTEGER
END GLOBALS

MAIN
    OPTIONS
        PROMPT LINE LAST
        DEFER INTERRUPT

    CALL STARTLOG("UNIC020.log")

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_pla_trasp

        CREATE TEMP TABLE tmp_pla_trasp
        (
         n_registros          CHAR(330)
        )
    WHENEVER ERROR STOP

        CALL init()
	OPEN WINDOW unic0011 AT 2,2 WITH FORM "UNIC0011" ATTRIBUTE(BORDER)
	DISPLAY "                          < Ctrl-C > Salir                                 " AT 1,1 ATTRIBUTE(REVERSE)

        DISPLAY " UNIC020       RECIBE ARCHIVOS DE RESPUESTA DE CONFRONTA                                   " AT 3,1 ATTRIBUTE(REVERSE)

 	DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

	INPUT BY NAME generar WITHOUT DEFAULTS
            BEFORE FIELD generar
                LET generar = NULL
                CLEAR FORM

	    AFTER FIELD generar
	        IF generar IS NULL THEN
		    ERROR "Campo NO puede ser NULO"
		    NEXT FIELD generar
	        END IF

                SELECT nombre
                INTO   nombre_archivo
                FROM   uni_ctr_archivo
                WHERE  nombre = generar

                IF STATUS <> NOTFOUND THEN
                    ERROR "Este archivo ya se recibio"
                    NEXT FIELD generar
                END IF

                WHENEVER ERROR CONTINUE
                    SELECT *
                    INTO   g_param_uni.*
                    FROM   seg_modulo
	            WHERE  modulo_cod = "uni"

                    LET archivo_traspaso = g_param_uni.ruta_rescate CLIPPED,"/",
                                         generar CLIPPED

                    LOAD FROM archivo_traspaso DELIMITER "+" INSERT INTO tmp_pla_trasp
                      
                    SELECT count(*)
                    INTO   cuantos
                    FROM   tmp_pla_trasp
                    
                    IF cuantos = 0 THEN
                       DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                       AT 19,2 ATTRIBUTE(REVERSE)
                       SLEEP 2
                       NEXT FIELD generar
                    ELSE
                       EXIT INPUT
                    END IF
                WHENEVER ERROR STOP

	    ON KEY (INTERRUPT)
                ERROR " PROCESO CANCELADO "
                SLEEP 2
                EXIT PROGRAM 
	END INPUT
        DISPLAY " PROCESANDO INFORMACION " AT 21,1 ATTRIBUTE(REVERSE)

        CALL lee_archivo_plano() #lap

        PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
        FOR CHAR enter

        CLOSE WINDOW unic0011
END MAIN

FUNCTION init()
#i-------------
    LET HOY  = TODAY

    SELECT estado ,
           USER
    INTO   s_recibido ,
           usuario
    FROM   uni_status
    WHERE  descripcion = "RECIBIDO"

    SELECT estado
    INTO   s_confrontado
    FROM   uni_status
    WHERE  descripcion = "CONFRONTADO"

    SELECT estado
    INTO   s_aceptado
    FROM   uni_status
    WHERE  descripcion = "ACEPTADO CONFRONTA"

    SELECT estado
    INTO   s_solicitado
    FROM   uni_status
    WHERE  descripcion = "SOLICITADO"

END FUNCTION

FUNCTION lee_archivo_plano()
#lap------------------------
    DEFINE  motivo_rechazo1     CHAR(3)
    DEFINE  motivo_rechazo2     CHAR(3)
    DEFINE  motivo_rechazo3     CHAR(3)
    DEFINE  vnss                CHAR(11) 
    DEFINE  vresulta            CHAR(02)
    DEFINE  vdiag1              CHAR(02)
    DEFINE  vdiag2              CHAR(02)
    DEFINE  vdiag3              CHAR(02)
    DEFINE  vdiag4              CHAR(02)
    DEFINE  vdiag5              CHAR(02)
    DEFINE  vnss1               CHAR(11) 
    DEFINE  vresulta1           CHAR(02)
    DEFINE  vdiag11             CHAR(02)
    DEFINE  vdiag21             CHAR(02)
    DEFINE  vdiag31             CHAR(02)
    DEFINE  vdiag41             CHAR(02)
    DEFINE  vdiag51             CHAR(02)
    DEFINE  vfolio              INTEGER 

    DEFINE xnss           CHAR(11),
           xfolio         INTEGER

    DECLARE cur_1 CURSOR FOR
    SELECT  * 
    FROM    tmp_pla_trasp
   
    LET cont            = 0
    LET cont1           = 0
    LET cont2           = 0
    FOREACH cur_1 INTO carga_reg
        DISPLAY " TOTAL RECHAZADOS UNIFICADORES : ",cont  AT 17,8
        DISPLAY " TOTAL RECHAZADOS UNIFICADOS   : ",cont1  AT 18,8

        IF carga_reg[01,02] = "01" AND  
           carga_reg[28,29] = "02" THEN
	      LET motivo_rechazo1= carga_reg[30,32]
	      LET motivo_rechazo2= carga_reg[33,35]
	      LET motivo_rechazo3= carga_reg[36,38]

              SELECT  max(folio)
	      INTO    vfolio
	      FROM    uni_cza_notifica 

	      UPDATE  uni_cza_notifica
	      SET     motivo_rechazo = motivo_rechazo1
	      WHERE   folio = vfolio
	      AND     estado = s_aceptado

	      DISPLAY "ENCABEZADO CONTIENE ERROR ",motivo_rechazo1 AT 20,8
	      EXIT PROGRAM
        END IF
        IF carga_reg[1,2] = "02" THEN
            LET cont     = cont + 1 
	    LET vnss     = carga_reg[041,051] 
	    LET vresulta = carga_reg[269,270] 
	    LET vdiag1   = carga_reg[271,273]
	    LET vdiag2   = carga_reg[274,276]
  	    LET vdiag3   = carga_reg[277,279]
	    LET vdiag4   = carga_reg[280,282]
	    LET vdiag5   = carga_reg[283,285]

            SELECT nss_uni,
                   folio
            INTO   xnss,
                   xfolio
            FROM   uni_unificador
            WHERE  nss_uni = vnss
            AND    estado = s_aceptado
            GROUP BY 1,2

            UPDATE  uni_unificador
            SET     estado          = s_recibido,
	            resul_operacion = vresulta,
                    diag_proceso1   = vdiag1,
                    diag_proceso2   = vdiag2,
                    diag_proceso3   = vdiag3,
                    diag_proceso4   = vdiag4,
                    diag_proceso5   = vdiag5
            WHERE   nss_uni         = vnss
	    AND     estado          = s_aceptado

            UPDATE uni_unificado
            SET    estado = s_recibido
            WHERE  nss_uni = vnss
            AND    estado IN (25,40)
            AND    folio = xfolio
        ELSE
            IF carga_reg[1,2] = "03" THEN
                LET cont1    = cont1 + 1 
            END IF
		      LET vnss1    = carga_reg[047,057] 
		      LET vresulta1= carga_reg[270,271] 
		      LET vdiag11  = carga_reg[272,274]
		      LET vdiag21  = carga_reg[275,277]
		      LET vdiag31  = carga_reg[278,280]
		      LET vdiag41  = carga_reg[281,283]
		      LET vdiag51  = carga_reg[284,286]

            SELECT nss_uni,
                   folio
            INTO   xnss,
                   xfolio
            FROM   uni_unificado
            WHERE  nss_cta1 = vnss1
            AND    estado = s_aceptado
            GROUP BY 1,2

            UPDATE  uni_unificado
            SET     estado            = s_recibido,
	            resulta_operacion = vresulta1,
                    diag_proceso1     = vdiag11,
                    diag_proceso2     = vdiag21,
                    diag_proceso3     = vdiag31,
                    diag_proceso4     = vdiag41,
                    diag_proceso5     = vdiag51
            WHERE   nss_cta1          = vnss1
	    AND     estado = s_aceptado

            UPDATE uni_unificado
            SET    estado = s_recibido
            WHERE  nss_uni = xnss
            AND    folio = xfolio
            AND    estado IN (25,40)

            UPDATE uni_unificado
            SET    estado = s_recibido
            WHERE  nss_uni = xnss
            AND    folio = xfolio
            AND    estado IN (25,40)
        END IF
   END FOREACH
   IF cont >=1 THEN
      DISPLAY "EL ARCHIVO CONTIENE ",cont CLIPPED," SOLICITUDES RECHAZADAS" 
              AT 21,1 ATTRIBUTE (REVERSE)
   ELSE
      DISPLAY "ARCHIVO FUE ACEPTADO" AT 21,1 ATTRIBUTE(REVERSE)
   END IF
END FUNCTION
