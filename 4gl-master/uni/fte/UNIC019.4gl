################################################################################
#Owner             => E.F.P                                                    #
#Programa UNIC019  => RECIBE RESULTADO DE CONFRONTA VIA ARCHIVO PLANO          #
#                  => CARGA MASIVA DE RESULTADOS                               #
#Fecha creacion    => 20 DE MARZO DEL 2001                                     #
#By                => ARMANDO RODRIGUEZ CASTROPAREDES                          #
#Fecha actualizacion> 26 diciembre 2002                                        #
#Sistema           => UNI                                                      #
#Tipo act          => modificacion de act clave afore                          #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE
        g_param_uni           RECORD LIKE seg_modulo.*

    DEFINE reg_res RECORD
        nss_uni         CHAR(11),
        nss_cta1        CHAR(11),
        diag_unifica    CHAR(02),
        cve_afo_recep   CHAR(03)
    END RECORD

    DEFINE
        HOY                   DATE

    DEFINE #char
        carga_reg             CHAR(330),
        vrazon_social         CHAR(050),
        usuario               CHAR(008),
        enter    	      CHAR(001),
        resulta_oper          CHAR(002),
        generar               CHAR(020),
        nombre_archivo        CHAR(020),
        archivo_confronta     CHAR(200),
        bold                  CHAR(032),
        raya                  CHAR(120),
        imprime               CHAR(050),
        borra                 CHAR(050),
        cat                   CHAR(100),
        G_LISTA               CHAR(200)

    DEFINE #smallint
        s_recibido,
        s_confrontado,
        vcodigo_afore,
        xcont  ,
        ycont  ,
        cont_det  ,
        cuantos   ,
        cont                  SMALLINT

    DEFINE #integer
        vfolio                INTEGER
END GLOBALS

MAIN
    OPTIONS
        PROMPT LINE LAST
        DEFER INTERRUPT

    CALL STARTLOG("UNIC019.log")

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_pla_confronta

        CREATE TEMP TABLE tmp_pla_confronta(n_registros CHAR(27))
    WHENEVER ERROR STOP

    CALL init()
    OPEN WINDOW unic0191 AT 2,2 WITH FORM "UNIC0191" ATTRIBUTE(BORDER)
    DISPLAY "       [Esc]  Iniciar                                    [Ctrl-C]  Salir       " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " UNIC019          RECIBE ARCHIVO RESULTADO DE CONFRONTA                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                            PARA CARGA MASIVA                                             " AT 4,1 ATTRIBUTE(REVERSE)


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
              ERROR " Este archivo ya se recibio "
		          SLEEP 3
		          EXIT PROGRAM
           END IF

           WHENEVER ERROR CONTINUE
                SELECT *
                INTO   g_param_uni.*
                FROM   seg_modulo
	              WHERE  modulo_cod = "uni"

                LET archivo_confronta= g_param_uni.ruta_rescate CLIPPED,"/",
                                       generar CLIPPED

                LOAD FROM archivo_confronta DELIMITER "+" INSERT INTO tmp_pla_confronta

                SELECT count(*)
                INTO   cuantos
                FROM   tmp_pla_confronta

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
    #DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
    ERROR " PROCESANDO INFORMACION "

    CALL lee_archivo_plano() #lap
    CALL actualiza() #a
    ERROR " "

    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
    FOR CHAR enter

    CLOSE WINDOW unic0191
END MAIN

FUNCTION init()
#i-------------
    LET HOY  = TODAY

    SELECT estado,
           USER
    INTO   s_confrontado,
           usuario
    FROM   uni_status
    WHERE  descripcion = "CONFRONTADO"

    SELECT estado
    INTO   s_recibido
    FROM   uni_status
    WHERE  descripcion = "RECIBIDO"

    SELECT codigo_afore,
           razon_social
    INTO   vcodigo_afore,
           vrazon_social
    FROM   tab_afore_local

    LET bold  = '\033e\033(s218T\033(s12H\033(s7B'
    LET raya = '_____________________________________________',
               '_____________________________________________',
               '____________________'

END FUNCTION

FUNCTION lee_archivo_plano()
#lap------------------------

    CALL valida_respuesta()   #vr

    IF xcont > 0 THEN
       DISPLAY " TOTAL INCONSISTENCIAS     : ",xcont AT 20,8
       ERROR "El archivo con inconsistencias en la afore"
       SLEEP 3
       ERROR ""
       EXIT PROGRAM
    END IF
    IF ycont > 0 THEN
       DISPLAY " TOTAL INCONSISTENCIAS     : ",ycont AT 21,8
       ERROR "El archivo con inconsistencias en el diagnostico"
       SLEEP 3
       ERROR ""
       EXIT PROGRAM
    END IF
    DECLARE cur_1 CURSOR FOR
    SELECT  *
    FROM    tmp_pla_confronta

    LET cont            = 0
    LET cont_det        = 0
    FOREACH cur_1 INTO carga_reg
       LET cont = cont + 1
       DISPLAY " TOTAL UNIFICADORES : ",cont     AT 16,8
       DISPLAY " TOTAL RECHAZOS     : ",cont_det AT 17,8

       CALL carga_respuesta()  #cr
	     INITIALIZE  reg_res.*  TO NULL

    END FOREACH

    #CALL reporte_error()

END FUNCTION

FUNCTION valida_respuesta()
#cr-----------------------
    DEFINE valida       CHAR(330)
    DEFINE xnss         CHAR(11)
    DEFINE xcta1        CHAR(11)
    DEFINE nss_uni      CHAR(11)
    DEFINE nss_cta1     CHAR(11)
    DEFINE afore        CHAR(03)
    DEFINE diagnostico  CHAR(02)
    DEFINE xafore       CHAR(03)
    DEFINE xdiag        CHAR(02)
    DEFINE xfolio       INTEGER
    DEFINE val RECORD
        nss_uni      CHAR(11),
        nss_cta1     CHAR(11),
        afore        CHAR(03),
        diagnostico  CHAR(02)
    END RECORD

    CREATE TEMP TABLE tmp_pla_verifica( nss_uni    CHAR(11),
                                        nss_cta1   CHAR(11),
                                        afore      CHAR(3),
                                        diagostico CHAR(02))
    LET nss_uni         = ""
    LET nss_cta1        = ""
    LET afore           = ""
    LET diagnostico     = ""

    DECLARE val_1 CURSOR FOR
    SELECT  *
    FROM    tmp_pla_confronta
    FOREACH val_1 INTO valida
       LET nss_uni         = valida[001,011]
       LET nss_cta1        = valida[012,022]
       LET afore           = valida[023,025]
       LET diagnostico     = valida[026,027]

       INSERT INTO tmp_pla_verifica VALUES(nss_uni,nss_cta1,afore,diagnostico)
    END FOREACH

    LET xnss = ""
    LET xcont = 0
    LET ycont = 0
    LET xfolio = 0

    DECLARE val_2 CURSOR FOR
    SELECT *
    FROM   tmp_pla_verifica
    ORDER BY nss_uni
    FOREACH val_2 INTO val.*
       IF val.nss_uni = xnss THEN
          SELECT MAX(a.folio)
          INTO   xfolio
          FROM   uni_unificado a
          WHERE  a.nss_uni  = val.nss_uni
          AND    a.nss_cta1 = val.nss_cta1
          AND    a.estado   = s_recibido

          IF xdiag <> val.diagnostico THEN
             INSERT INTO uni_unificado_err VALUES(xfolio         ,
                                                  val.nss_uni    ,
                                                  val.nss_cta1   ,
                                                  val.diagnostico,
                                                  1)
             INSERT INTO uni_unificado_err VALUES(xfolio,
                                                  xnss  ,
                                                  xcta1 ,
                                                  xdiag ,
                                                  1)
             LET ycont = ycont +1
          END IF

          IF xafore <> val.afore THEN
             INSERT INTO uni_unificador_err VALUES(
                                                   xfolio     ,
                                                   val.nss_uni,
                                                   val.afore  ,
                                                   1)
             INSERT INTO uni_unificador_err VALUES(
                                                   xfolio ,
                                                   xnss   ,
                                                   xafore ,
                                                   1)
             LET xcont = xcont +1
          END IF

       END IF
       LET xnss   = val.nss_uni
       LET xcta1  = val.nss_cta1
       LET xafore = val.afore
       LET xdiag  = val.diagnostico
    END FOREACH

END FUNCTION
FUNCTION carga_respuesta()
#cr-----------------------

   LET reg_res.nss_uni         = carga_reg[001,011]
   LET reg_res.nss_cta1        = carga_reg[012,022]
   LET reg_res.cve_afo_recep   = carga_reg[023,025]
   LET reg_res.diag_unifica    = carga_reg[026,027]

   SELECT max(folio)
   INTO   vfolio
   FROM   uni_unificado
   WHERE  estado = s_recibido
   AND    nss_cta1 = reg_res.nss_cta1

   IF reg_res.cve_afo_recep = "000" THEN
      UPDATE uni_unificador
      SET    cve_afo_recep = reg_res.cve_afo_recep,
             estado        = s_confrontado
      WHERE  nss_uni       = reg_res.nss_uni
      AND    estado        = s_recibido
      AND    folio         = vfolio

      CASE reg_res.diag_unifica
  	     WHEN "02"
                 UPDATE uni_unificado
                 SET    diag_unifica = reg_res.diag_unifica,
                        estado       = s_confrontado
                 WHERE  nss_uni      = reg_res.nss_uni
	               AND    nss_cta1     = reg_res.nss_cta1
	               AND    estado       = s_recibido
                 AND    folio        = vfolio

         WHEN "04"
                 UPDATE uni_unificado
                 SET    diag_unifica = reg_res.diag_unifica,
                        estado       = s_confrontado
                 WHERE  nss_uni      = reg_res.nss_uni
	               AND    nss_cta1     = reg_res.nss_cta1
	               AND    estado       = s_recibido 
                 AND    folio        = vfolio

	       WHEN "05"
                 UPDATE uni_unificado
                 SET    diag_unifica = reg_res.diag_unifica,
                        estado       = s_confrontado
                 WHERE  nss_uni      = reg_res.nss_uni
		             AND    nss_cta1     = reg_res.nss_cta1
		             AND    estado       = s_recibido
                 AND    folio        = vfolio

	       WHEN "06"
                 UPDATE uni_unificado
                 SET    diag_unifica = reg_res.diag_unifica,
                        estado       = s_confrontado
                 WHERE  nss_uni      = reg_res.nss_uni
		             AND    nss_cta1     = reg_res.nss_cta1
		             AND    estado       = s_recibido
                 AND    folio        = vfolio

	       OTHERWISE
		      display reg_res.*
		      --sleep 2
		      --exit program
		             LET cont_det = cont_det + 1
		             WHENEVER ERROR CONTINUE
                    INSERT INTO uni_unificado_err VALUES(vfolio              ,
                                                         reg_res.nss_cta1    ,
                                                         reg_res.diag_unifica,
                                                         1)
                 WHENEVER ERROR STOP
      END CASE
   ELSE
      SELECT "X"
      FROM   uni_unificador
      WHERE  nss_uni      = reg_res.nss_uni
      AND    cve_ent_nss  = reg_res.cve_afo_recep
      AND    folio        = vfolio
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN
         UPDATE uni_unificador
         #SET    estado        = s_confrontado
         SET    cve_afo_recep = reg_res.cve_afo_recep,
                estado        = s_confrontado
         WHERE  nss_uni       = reg_res.nss_uni
	       AND    estado        = s_recibido
         AND    folio        = vfolio

         CASE reg_res.diag_unifica

            WHEN "01"
               UPDATE uni_unificado
               SET    diag_unifica = reg_res.diag_unifica,
                      estado       = s_confrontado
               WHERE  nss_uni      = reg_res.nss_uni
		           AND    nss_cta1     = reg_res.nss_cta1
		           AND    estado       = s_recibido
               AND    folio        = vfolio
		        OTHERWISE
		           LET cont_det = cont_det + 1
               INSERT INTO uni_unificado_err VALUES(
	        	                                        vfolio              ,
                                                    reg_res.nss_cta1    ,
                                                    reg_res.diag_unifica,
                                                    1)
         END CASE
      ELSE
         SELECT "X"
         FROM   uni_unificado
         WHERE   nss_cta1     = reg_res.nss_cta1
         AND    (cve_ent_cta1 = reg_res.cve_afo_recep
                 OR cve_ent_cta1 IN(SELECT afore_cod
                                    FROM   tab_afore))
         AND    folio        = vfolio
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
            UPDATE uni_unificador
            #SET    estado        = s_confrontado
            SET    cve_afo_recep = reg_res.cve_afo_recep,
                   estado        = s_confrontado
            WHERE  nss_uni       = reg_res.nss_uni
	          AND    estado        = s_recibido
            AND    folio         = vfolio

            CASE reg_res.diag_unifica

		           WHEN "01"
                  UPDATE uni_unificado
                  SET    diag_unifica = reg_res.diag_unifica,
                         estado       = s_confrontado
                  WHERE  nss_uni      = reg_res.nss_uni
			            AND    nss_cta1     = reg_res.nss_cta1
			            AND    estado       = s_recibido
                  AND    folio        = vfolio

		           OTHERWISE
		              display reg_res.*
		              --sleep 2
		              --exit program
		              LET cont_det = cont_det + 1
                  INSERT INTO uni_unificado_err VALUES(
	       	                                             vfolio              ,
                                                       reg_res.nss_cta1    ,
                                                       reg_res.diag_unifica,
                                                       1)
            END CASE
         ELSE
            SELECT "X"
            FROM   uni_unificado
            WHERE  nss_cta1     = reg_res.nss_cta1
	          AND    (   cve_ent_cta1 = reg_res.cve_afo_recep
	                  OR cve_ent_cta1 = "000")
            AND    folio         = vfolio
	          GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               UPDATE uni_unificador
               #SET    estado        = s_confrontado
               SET    cve_afo_recep = reg_res.cve_afo_recep,
                      estado        = s_confrontado
               WHERE  nss_uni       = reg_res.nss_uni
	             AND    estado        = s_recibido
               AND    folio         = vfolio

               CASE reg_res.diag_unifica

		              WHEN "01"
                     UPDATE uni_unificado
                     SET    diag_unifica = reg_res.diag_unifica,
                            estado       = s_confrontado
                     WHERE  nss_uni      = reg_res.nss_uni
			               AND    nss_cta1     = reg_res.nss_cta1
			               AND    estado       = s_recibido
                     AND    folio        = vfolio

		              OTHERWISE
		                 display reg_res.*
		                 --sleep 2
		                 --exit program
		                 LET cont_det = cont_det + 1
                     INSERT INTO uni_unificado_err VALUES(
	       	                                                vfolio              ,
                                                          reg_res.nss_cta1    ,
                                                          reg_res.diag_unifica,
                                                          1)
               END CASE
            ELSE
		           display reg_res.*
		           --display "aqui"
		           --sleep 2
		           --exit program
	             LET cont_det = cont_det + 1
               INSERT INTO uni_unificador_err VALUES(
	     	                                   vfolio               ,
                                           reg_res.nss_uni      ,
                                           reg_res.cve_afo_recep,
                                           1)
            END IF
         END IF
      END IF
   END IF
END FUNCTION
FUNCTION actualiza()
#a------------------

	 SELECT max(folio)
	 INTO   vfolio
	 FROM   uni_unificador
	 WHERE  estado = s_confrontado

	 UPDATE uni_cza_notifica
	 SET    estado = s_confrontado
	 WHERE  folio  = vfolio

	 UPDATE uni_sum_notifica
	 SET    estado = s_confrontado
	 WHERE  folio = vfolio

	 INSERT INTO uni_ctr_archivo VALUES(generar,vfolio,HOY,21)

END FUNCTION
