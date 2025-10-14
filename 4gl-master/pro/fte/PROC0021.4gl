################################################################################
#PROYECTO          => SISTEMA DE AFORES ( MEXICO )                             #
#OWNER             => E.F.P.                                                   #
#PROGRAMA PROC0021 => REVERSO.RECEPCION DE CERTIFICACIOEN DE AGENTES PROMOTORES#
#FECHA             => 05 DE FEBRERO DEL 2000                                   #
#BY                => FRANCO ESTEBAN ULLOA VIDELA                              #
#FECHA ACTUALIZ.   => 11 DE ENERO DEL 2001                                     #
#ACTUALIZACION     => FRANCO ESTEBAN ULLOA VIDELA                              #
#SISTEMA           => PRO 					               #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE 
        reg_3                 RECORD LIKE pro_his_revalida.*,

        reg_1 RECORD 
               folio          CHAR(20)
        END RECORD,

        HOY                   DATE,
        enter                 CHAR(001),
        vnro_solicitud        INTEGER   
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
	ACCEPT KEY CONTROL-I

    CALL STARTLOG("PROC0021.log")

    CALL init() #i
    OPEN WINDOW proc00211 AT 4,4 WITH FORM "PROC00211" ATTRIBUTE(BORDER)
    DISPLAY "                           < Ctrl-C > Sa",
            "lir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROC0021     REVERSO CARGA DE ARCHIVOS GENERADO ",
            "POR CONSA                     " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME reg_1.folio WITHOUT DEFAULTS
        AFTER FIELD folio
	    IF reg_1.folio IS NULL THEN
	        ERROR " CAMPO NO PUEDE SER NULO "
		NEXT FIELD folio
	    END IF

        ON KEY (ESC)
	    IF reg_1.folio IS NULL THEN
	        ERROR " CAMPO NO PUEDE SER NULO "
		NEXT FIELD folio
	    END IF

            EXIT INPUT
       
        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

    END INPUT
    
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
    CALL primer_paso() #pp
    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DEFINE reg_2 RECORD
              cod_promotor          LIKE pro_recep_scb.cod_promotor ,
              fecha_scb             LIKE pro_recep_scb.fecha_scb ,
              cve_scb               LIKE pro_recep_scb.cve_scb
           END RECORD

    DEFINE reg_4 RECORD
	     cod_promotor          CHAR(10) ,
             diag_proceso          CHAR(2)  ,
	     status                SMALLINT ,
	     status_interno        SMALLINT ,
	     motivo_suspende       CHAR(2)  ,
	     fecha_baja            DATE     ,
             fecha_suspende        DATE
           END RECORD

    DEFINE reg_5 RECORD
             nro_solicitud         INTEGER,
             fenvio                DATE,
             cod_promotor          CHAR(10)
           END RECORD,

           c10_cod_promotor            CHAR(10),

           s_tipo_operacion      SMALLINT

    DECLARE cur_3 CURSOR FOR
       SELECT tipo_operacion
       FROM   pro_recepcion
       WHERE  nro_lote = reg_1.folio

    FOREACH cur_3 INTO s_tipo_operacion
      CASE s_tipo_operacion
      WHEN 301 --REVERSO DE LA RECEPCION DE ALTAS-----
                DELETE
                FROM   pro_det_agte
                WHERE  folio = reg_1.folio

               DECLARE cur_ymi CURSOR FOR
                  SELECT nro_solicitud,fenvio
                  FROM   pro_solicitud
                  WHERE  folio = reg_1.folio

               FOREACH cur_ymi INTO  reg_5.nro_solicitud,
                                     reg_5.fenvio

                  SELECT  UNIQUE cod_promotor
                  INTO    reg_5.cod_promotor
                  FROM    pro_envio_alta
                  WHERE   nro_solicitud = reg_5.nro_solicitud
                  AND     fenvio = reg_5.fenvio
                
                  UPDATE pro_solicitud 
                  SET    pro_solicitud.folio          = "" ,
                         pro_solicitud.status         = 1  ,
                         pro_solicitud.status_interno = 3  , #aqui tenia 1
                         pro_solicitud.cod_promotor   = reg_5.cod_promotor,
                         pro_solicitud.diag_proceso   = "" ,
                         pro_solicitud.fecha_registro = "" ,
                         pro_solicitud.fecha_proceso  = "" 
                  WHERE  pro_solicitud.folio          = reg_1.folio
                  AND    pro_solicitud.status_interno = 4
                  AND    pro_solicitud.nro_solicitud  = reg_5.nro_solicitud
            
                  UPDATE pro_solicitud 
                  SET    pro_solicitud.folio          = "" ,
                         pro_solicitud.status         = 1  ,
                         pro_solicitud.status_interno = 8  ,
                         pro_solicitud.cod_promotor   = "" ,
                         pro_solicitud.diag_proceso   = "" ,
                         pro_solicitud.fecha_registro = "" ,
                         pro_solicitud.fecha_proceso  = "" 
                  WHERE  pro_solicitud.folio          = reg_1.folio
                  AND    pro_solicitud.status_interno = 40

                  UPDATE pro_solicitud 
                  SET    pro_solicitud.folio          = "" ,
                         pro_solicitud.status         = 1  ,
                         pro_solicitud.status_interno = 21 ,
                         pro_solicitud.cod_promotor   = "" ,
                         pro_solicitud.diag_proceso   = "" ,
                         pro_solicitud.fecha_registro = "" ,
                         pro_solicitud.fecha_proceso  = "" 
                  WHERE  pro_solicitud.folio          = reg_1.folio
                  AND    pro_solicitud.status_interno = 41
               END FOREACH

      WHEN 302 --REVERSO DE LA RECEPCION DE REVALIDACIONES----------------
                DECLARE cur_1 CURSOR FOR
                   SELECT cod_promotor
                   FROM   pro_det_revalida
                   WHERE  folio = reg_1.folio

                FOREACH cur_1 INTO c10_cod_promotor
                    SELECT *
                    INTO   reg_3.*
                    FROM   pro_his_revalida
                    WHERE  cod_promotor = c10_cod_promotor 
        
                    IF STATUS <> NOTFOUND THEN
                        DELETE 
                        FROM   pro_mae_promotor
                        WHERE  cod_promotor = c10_cod_promotor 

                        INSERT INTO pro_mae_promotor VALUES(reg_3.*)

                        DELETE
                        FROM   pro_his_revalida
                        WHERE  cod_promotor = c10_cod_promotor 
                    END IF
                END FOREACH
    
                DELETE
                FROM   pro_det_revalida
                WHERE  folio = reg_1.folio;

      WHEN 303--REVERSO DE LA RECEPCION DE BAJAS--------------------
                DECLARE cur_2 CURSOR FOR
                   SELECT cod_promotor    ,
                          fecha_scb ,
                          cve_scb
                   FROM   pro_recep_scb
                   WHERE  folio = reg_1.folio
           
                FOREACH cur_2 INTO reg_2.*
                    UPDATE pro_envio_scb
                    SET    status_interno  = 1   , 
                           folio_recepcion = NULL
                    WHERE  cod_promotor    = reg_2.cod_promotor
                    AND    status_interno  = 5

                    UPDATE pro_mae_promotor
                    SET    status_interno = 3, #aqui tenia 1
                           fecha_suspende = NULL
                    WHERE  cod_promotor   = reg_2.cod_promotor
                    AND    status_interno = 5
                END FOREACH

                DELETE
                FROM   pro_recep_scb
                WHERE  folio = reg_1.folio

      WHEN 304
              DELETE
              FROM  pro_resul_mod
              WHERE folio = reg_1.folio

      WHEN 306
              DELETE
              FROM  pro_aviso_examen
              WHERE folio = reg_1.folio

      WHEN 307 --REVERSO DEL RESULTADO DEL EXAMEN
               DECLARE cur_4 CURSOR FOR
##                   SELECT A.cod_promotor    ,
                   SELECT A.codven          ,
                          A.diag_proceso    ,
                          A.status          ,
                          A.status_interno  ,
                          A.motivo_suspende ,
                          A.fecha_baja      ,
                          A.fecha_suspende
                   FROM   pro_his_examen A
                   WHERE  A.folio = reg_1.folio
                   
               FOREACH cur_4 INTO reg_4.*
                  UPDATE pro_mae_promotor
                     SET    diag_proceso    = reg_4.diag_proceso    ,
                            status          = reg_4.status          ,
                            status_interno  = reg_4.status_interno  ,
                            motivo_suspende = reg_4.motivo_suspende ,
                            fecha_baja      = reg_4.fecha_baja      ,
                            fecha_suspende  = reg_4.fecha_suspende
                  WHERE  cod_promotor       = reg_4.cod_promotor
               END FOREACH

               DELETE
               FROM   pro_resul_examen
               WHERE  folio = reg_1.folio
  
               DELETE
               FROM   pro_his_examen
               WHERE  folio = reg_1.folio
      END CASE
    END FOREACH

    CALL reversa_tablas_globales()#rtg
END FUNCTION

FUNCTION reversa_tablas_globales()
#rtg------------------------------
    DELETE
    FROM   pro_folio
    WHERE  folio = reg_1.folio

    DELETE
    FROM   pro_recepcion
    WHERE  nro_lote = reg_1.folio
   
    DELETE
    FROM   pro_cza_agte
    WHERE  folio = reg_1.folio

    DELETE
    FROM   pro_ctr_lote
    WHERE  nro_lote = reg_1.folio
    AND    fecha_recepcion IS NOT NULL
END FUNCTION
