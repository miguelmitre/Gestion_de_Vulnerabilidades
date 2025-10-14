################################################################################
#PROYECTO          => SISTEMA DE AFORES ( MEXICO )                             #
#OWNER             => E.F.P.                                                   #
#PROGRAMA PROC0021 => REVERSO.RECEPCION DE CERTIFICACIOEN DE AGENTES PROMOTORES#
#FECHA             => 05 DE FEBRERO DEL 2000                                   #
#BY                => FRANCO ESTEBAN ULLOA VIDELA                              #
#FECHA ACTUALIZ.   => 11 DE ENERO DEL 2001                                     #
#ACTUALIZACION     => FRANCO ESTEBAN ULLOA VIDELA                              #
#SISTEMA           => PRO 					               #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 27 de Febrero del 2008                                   #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT version     #
#                  => 3.0   (v1)                                               #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 24 de Septiembre del 2008                                #
#Observacion       => Solo se permitira reversar el ultimo folio recibido      #
#                  => contenido en la tabla pro_recepcion                      #
#                  => (v2)                                                     #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 28 de septiembre del 2009                                #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT             #
#                  => con fecha 29/07/2009                                     #
#                  => (v10)                                                    #
#SISTEMA           => PRO                                                      #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE 
      reg_3           RECORD LIKE pro_his_revalida.*

   DEFINE reg_1 RECORD
      folio           CHAR(20)
   END RECORD

   DEFINE
      HOY             DATE,
      v_max_fecha     DATE,
      v_fecha_uni     DATE,
      v_fecha_res     DATE,
      enter           CHAR(001),
      vcod_promotor   CHAR(10),             --(v10)
      v_mot           CHAR(2),              --(v10)
      v_diag          CHAR(2),              --(v10)
      v_sta           SMALLINT,             --(v10)
      v_sta_int       SMALLINT,             --(v10)
      v_fbaja         DATE,                 --(v10)
      v_fsus          DATE                  --(v10)

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
   DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " PROC0021                REVERSO CARGA DE ARCHIVO                              " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg_1.folio WITHOUT DEFAULTS
      AFTER FIELD folio
         IF reg_1.folio IS NULL THEN
            ERROR " CAMPO NO PUEDE SER NULO "
            NEXT FIELD folio
         END IF

         LET v_fecha_uni = NULL                       --(v2)
         LET v_fecha_res = NULL                       --(v2)

         SELECT unique(fecha_genera)                  --(v2)
         INTO v_fecha_uni                             --(v2)
         FROM pro_recepcion                           --(v2)
         WHERE nro_lote = reg_1.folio                 --(v2)

         LET v_fecha_res = HOY - 6 units day          --(v2)

         IF v_fecha_uni < v_fecha_res  THEN  
            ERROR "Folio no se puede reversar o no existe"
            NEXT FIELD folio
         END IF

      ON KEY (ESC)
         IF reg_1.folio IS NULL THEN
            ERROR " CAMPO NO PUEDE SER NULO "
            NEXT FIELD folio
         END IF

         LET v_fecha_uni = NULL                       --(v2)
         LET v_fecha_res = NULL                       --(v2)

         SELECT unique(fecha_genera)                  --(v2)
         INTO v_fecha_uni                             --(v2)
         FROM pro_recepcion                           --(v2)
         WHERE nro_lote = reg_1.folio                 --(v2)

         LET v_fecha_res = HOY - 6 units day          --(v2)

         IF v_fecha_uni < v_fecha_res  THEN
            ERROR "Folio no se puede reversar o no existe"
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
      cod_promotor      LIKE pro_recep_scb.cod_promotor ,
      fecha_scb         LIKE pro_recep_scb.fecha_scb ,
      cve_scb           LIKE pro_recep_scb.cve_scb
   END RECORD

   DEFINE reg_4 RECORD
      cod_promotor      CHAR(10) ,
      diag_proceso      CHAR(2)  ,
      status            SMALLINT ,
      status_interno    SMALLINT ,
      motivo_suspende   CHAR(2)  ,
      fecha_baja        DATE     ,
      fecha_suspende    DATE
   END RECORD

   DEFINE reg_5 RECORD
      nro_solicitud     INTEGER,
      fenvio            DATE,
      cod_promotor      CHAR(10)
   END RECORD

   DEFINE reg_6 RECORD                            --(v1)
      cod_promotor      CHAR(10),                 --(v1)
      folio             INTEGER                   --(v1)
   END RECORD

   DEFINE
      c10_cod_promotor   CHAR(10),
      s_tipo_operacion   SMALLINT

   DECLARE cur_3 CURSOR FOR
   SELECT tipo_operacion
   FROM   pro_recepcion
   WHERE  nro_lote = reg_1.folio

   FOREACH cur_3 INTO s_tipo_operacion

     CASE s_tipo_operacion
        WHEN "301" --REVERSO DE LA RECEPCION DE ALTAS-----

           DELETE                                          --(v1)
           FROM   pro_det_agte                             --(v1)
           WHERE  folio = reg_1.folio                      --(v1)
           AND    status_interno in (4,41,6)               --(v1)

           DELETE                                          --(v1)
           FROM   pro_det_agte                             --(v1)
           WHERE  folio = reg_1.folio                      --(v1)
           AND    status_interno in (40)                   --(v1)
           AND    cod_promotor NOT IN (SELECT cod_promotor
                                       FROM   pro_mae_promotor
                                       WHERE  diag_proceso IN ("7X"))       --(v10)

           DECLARE cur_ymi CURSOR FOR                       --(v1)
           SELECT nro_solicitud,fenvio                   --(v1)
           FROM   pro_solicitud                          --(v1)
           WHERE  folio = reg_1.folio                    --(v1)
           AND    status_interno in (4,41)               --(v1)

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
                     --pro_solicitud.cod_promotor   = "" ,                              --(v1)
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
                     pro_solicitud.status_interno = 21 ,
                     pro_solicitud.cod_promotor   = "" ,
                     pro_solicitud.diag_proceso   = "" ,
                     pro_solicitud.fecha_registro = "" ,
                     pro_solicitud.fecha_proceso  = "" 
              WHERE  pro_solicitud.folio          = reg_1.folio
              AND    pro_solicitud.status_interno = 41

           END FOREACH


           DECLARE cur_ymi2 CURSOR FOR
           SELECT nro_solicitud,fenvio
           FROM   pro_solicitud
           WHERE  folio = reg_1.folio
           AND    status_interno in (40)                  --(v1)
           AND    cod_promotor NOT IN (SELECT cod_promotor
                                       FROM   pro_mae_promotor
                                       WHERE  diag_proceso IN ("7X"))        --(v10)

           FOREACH cur_ymi2 INTO  reg_5.nro_solicitud,
                                  reg_5.fenvio

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
              AND    cod_promotor NOT IN (SELECT cod_promotor
                                          FROM   pro_mae_promotor
                                          WHERE  diag_proceso IN ("7X"))            --(v10)
           END FOREACH


        WHEN "801"                                    --(v1)
                                                        -- Igual que 301

           DELETE
           FROM   pro_det_agte
           WHERE  folio = reg_1.folio
           AND    status_interno in (4,41,6)               --(v1) 

           DELETE
           FROM   pro_det_agte
           WHERE  folio = reg_1.folio
           AND    status_interno in (40)               --(v1)
           AND    cod_promotor NOT IN (SELECT cod_promotor
                                       FROM   pro_mae_promotor
                                       WHERE  diag_proceso IN ("7X"))        --(v10)

           DECLARE cur_ymi3 CURSOR FOR
           SELECT nro_solicitud,fenvio
           FROM   pro_solicitud
           WHERE  folio = reg_1.folio
           AND    status_interno in (4,41)                  --(v1)

           FOREACH cur_ymi3 INTO  reg_5.nro_solicitud,
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
                     pro_solicitud.status_interno = 21 ,
                     pro_solicitud.cod_promotor   = "" ,
                     pro_solicitud.diag_proceso   = "" ,
                     pro_solicitud.fecha_registro = "" ,
                     pro_solicitud.fecha_proceso  = "" 
              WHERE  pro_solicitud.folio          = reg_1.folio
              AND    pro_solicitud.status_interno = 41
           END FOREACH

           DECLARE cur_ymi4 CURSOR FOR
           SELECT nro_solicitud,fenvio
           FROM   pro_solicitud
           WHERE  folio = reg_1.folio
           AND    status_interno in (40)                  --(v1)
           AND    cod_promotor NOT IN (SELECT cod_promotor
                                       FROM   pro_mae_promotor
                                       WHERE  diag_proceso IN ("7X"))        --(v10)

           FOREACH cur_ymi4 INTO  reg_5.nro_solicitud,
                                  reg_5.fenvio

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
              AND    cod_promotor NOT IN (SELECT cod_promotor
                                          FROM   pro_mae_promotor
                                          WHERE  diag_proceso IN ("7X"))        --(v10)
           END FOREACH

        WHEN "401"

           DELETE
           FROM   pro_det_agte
           WHERE  folio = reg_1.folio
           AND    status_interno in (4,41,6)               --(v1) 

           DELETE
           FROM   pro_det_agte
           WHERE  folio = reg_1.folio
           AND    status_interno in (40)               --(v1)
           AND    cod_promotor NOT IN (SELECT cod_promotor
                                       FROM   pro_mae_promotor
                                       WHERE  diag_proceso IN ("7X"))        --(v10)

           DECLARE cur_ymi5 CURSOR FOR
           SELECT nro_solicitud,fenvio
           FROM   pro_solicitud
           WHERE  folio = reg_1.folio
           AND    status_interno in (4,41)                  --(v1)

           FOREACH cur_ymi5 INTO  reg_5.nro_solicitud,
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
                     pro_solicitud.status_interno = 21 ,
                     pro_solicitud.cod_promotor   = "" ,
                     pro_solicitud.diag_proceso   = "" ,
                     pro_solicitud.fecha_registro = "" ,
                     pro_solicitud.fecha_proceso  = "" 
              WHERE  pro_solicitud.folio          = reg_1.folio
              AND    pro_solicitud.status_interno = 41
           END FOREACH


           DECLARE cur_ymi6 CURSOR FOR
           SELECT nro_solicitud,fenvio
           FROM   pro_solicitud
           WHERE  folio = reg_1.folio
           AND    status_interno in (40)                  --(v1)
           AND    cod_promotor NOT IN (SELECT cod_promotor
                                       FROM   pro_mae_promotor
                                       WHERE  diag_proceso IN ("7X"))        --(v10)

           FOREACH cur_ymi6 INTO reg_5.nro_solicitud,
                                 reg_5.fenvio

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
              AND    cod_promotor NOT IN (SELECT cod_promotor
                                          FROM   pro_mae_promotor
                                          WHERE  diag_proceso IN ("7X"))           --(v10)
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

                 LET v_max_fecha = null

                 SELECT MAX(fecha_genera)                  --(v2)
                 INTO   v_max_fecha                        --(v2)
                 FROM   pro_capacitacion                   --(v2)
                 WHERE  cod_promotor = c10_cod_promotor    --(v2)
                 AND    estado = 5                         --(v2)

                 UPDATE pro_capacitacion
                 SET    estado = 3    --(v2)
                 WHERE  cod_promotor = c10_cod_promotor    --(v2)
                 AND    estado = 5                         --(v2)
                 AND    fecha_genera = v_max_fecha         --(v2) 

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
                     fecha_suspende = NULL,
                     motivo_suspende = NULL      --(v100)
              WHERE  cod_promotor   = reg_2.cod_promotor
              AND    status_interno = 5
           END FOREACH

           DELETE
           FROM   pro_recep_scb
           WHERE  folio = reg_1.folio


        WHEN 304

--(v10) inicia

           LET    vcod_promotor = NULL

           DECLARE cur_44 CURSOR FOR
           SELECT  cod_promotor
           INTO    vcod_promotor
           FROM    pro_resul_mod
           WHERE   folio           = reg_1.folio
           AND     diag_proceso    IN ("2G","1I") 

           FOREACH cur_44 INTO  vcod_promotor

              LET  v_mot       = NULL
              LET  v_diag      = NULL
              LET  v_sta       = NULL
              LET  v_sta_int   = NULL
              LET  v_fbaja     = NULL
              LET  v_fsus      = NULL

              SELECT p.motivo_suspende,
                     p.diag_proceso,
                     p.status,
                     p.status_interno,
                     p.fecha_baja,
                     p.fecha_suspende
              INTO   v_mot,
                     v_diag,
                     v_sta,
                     v_sta_int,
                     v_fbaja,
                     v_fsus 
              FROM   pro_his_baja p
              WHERE  p.cod_promotor = vcod_promotor
              AND    p.folio        = reg_1.folio 

              IF STATUS <> NOTFOUND THEN   --si existe
                 UPDATE pro_mae_promotor
                 SET    diag_proceso    = v_diag    ,
                        motivo_suspende = v_mot     ,
                        status          = v_sta     ,
                        status_interno  = v_sta_int ,
                        fecha_baja      = v_fbaja   ,
                        fecha_suspende  = v_fsus
                 WHERE  cod_promotor    = vcod_promotor 
                 AND  diag_proceso       = "2G"

                 UPDATE pro_mae_promotor
                 SET    diag_proceso  = v_diag
                 WHERE  cod_promotor  = vcod_promotor
                 AND  diag_proceso    = "1I"
              END IF

              DELETE
              FROM pro_his_baja
              WHERE folio        = reg_1.folio
              AND cod_promotor = vcod_promotor

           END FOREACH

--(v10) finaliza

           DELETE
           FROM  pro_resul_mod
           WHERE folio = reg_1.folio


        WHEN 306
           DELETE
           FROM  pro_aviso_examen
           WHERE folio = reg_1.folio

        WHEN 307 --REVERSO DEL RESULTADO DEL EXAMEN
           DECLARE cur_4 CURSOR FOR
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
              WHERE  cod_promotor    = reg_4.cod_promotor
           END FOREACH

           DELETE
           FROM   pro_resul_examen
           WHERE  folio = reg_1.folio

           DELETE
           FROM   pro_his_examen
           WHERE  folio = reg_1.folio


        WHEN 308 --REVERSO DE REACTIVACIONES--                          --(v1)

           DELETE                                                       --(v1)
           FROM   pro_det_agte                                          --(v1)
           WHERE  folio = reg_1.folio                                   --(v1)
           AND    status_interno in (61,62)                             --(v10)

           DELETE                                                       --(v1)
           FROM   pro_det_agte                                          --(v1)
           WHERE  folio = reg_1.folio                                   --(v1)
           AND    status_interno in (40,62)                             --(v10)
           AND    cod_promotor IN (SELECT cod_promotor                                  --(v1)
                                   FROM   pro_mae_promotor 
                                   WHERE  diag_proceso IN ("7X"))                       --(v10)

           DECLARE cur_5 CURSOR FOR                                     --(v1)
           SELECT nro_solicitud,fenvio                                  --(v1)
           FROM   pro_solicitud                                         --(v1)
           WHERE  folio = reg_1.folio                                   --(v1)
           AND    status_interno in (40,62)                             --(v1)
           AND    cod_promotor IN (SELECT cod_promotor                  --(v1)
                                   FROM   pro_mae_promotor              --(v1)
                                   WHERE  diag_proceso IN ("7X"))       --(v10)


           FOREACH cur_5 INTO  reg_5.nro_solicitud,
                                reg_5.fenvio

              UPDATE pro_solicitud
              SET    pro_solicitud.folio          = "" ,
                     pro_solicitud.status         = 1  ,
                     pro_solicitud.status_interno = 8  ,
                     pro_solicitud.diag_proceso   = "" ,
                     pro_solicitud.fecha_registro = "" ,
                     pro_solicitud.fecha_proceso  = ""
              WHERE  pro_solicitud.folio          = reg_1.folio
              AND    pro_solicitud.status_interno in (40,62)     --(v10) 
              AND    cod_promotor IN (SELECT cod_promotor                   --(v1)
                                      FROM   pro_mae_promotor               --(v1)
                                      WHERE  diag_proceso IN ("7X"))        --(v10)

           END FOREACH

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
