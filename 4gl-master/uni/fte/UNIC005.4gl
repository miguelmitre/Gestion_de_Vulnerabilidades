########################################################################
#Owner             => E.F.P.                                           #
#Programa UNIC005  => RECIBE ARCHIVOS OP 30 / 31 / 32                  #
#Por               => ARMANDO RODRIGUEZ CASTROPAREDES                  #
#Fecha creacion    => 11 DE OCTUBRE DE 1999                            #
#Por               => ARMANDO RODRIGUEZ CASTROPAREDES                  #
#fecha actualiza   => 15 noviembre 2003                                #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                  #
#fecha actualiza   => 03 septiembre 2004                               #
#fecha actualiza   => 30 septiembre 2004                               #
#fecha actualiza   => 14 enero 2005                                    #
#fecha actualiza   => 28 enero 2005                                    #
#fecha actualiza   => 24 mayo 2007                                     #
#Fecha Act         => 26 marzo 2008                                    #
#Fecha Act         => 07 agosto 2008                                   #
#Fecha Act         => 29 agosto 2008                                   #
#Sistema           => UNI                                              #
########################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_param_uni        RECORD LIKE seg_modulo.*

   DEFINE HOY                DATE

   DEFINE carga_reg          CHAR(570),
          usuario            CHAR(008),
          enter              CHAR(001),
          generar            CHAR(018),
          nombre_archivo     CHAR(018),
          archivo_traspaso   CHAR(200),
          c10_fecha_presenta CHAR(010),
          c10_fecha_nac_uni  CHAR(010),
          c10_fecha_nac_cta1 CHAR(010)

   DEFINE vgenerado,
          vaceptado,
          vaperturado,
          vtraspasado,
          s_recibido,
          s_aceptado,
          s_solicitado,
          s_intra,
          s_traspaso,
          s_liquidado,
          codigo,
          cuantos,
          cont,
          cont1,
          cont2              SMALLINT

   DEFINE ultimo_folio       INTEGER

   DEFINE reg_a              RECORD LIKE afi_mae_afiliado.*
   DEFINE reg_b              RECORD LIKE cta_ctr_cuenta.*

   DEFINE vregimen,
          vguarda,
          vfn_regimen        CHAR(500)

   DEFINE apertura           CHAR(20)

   DEFINE vmarca,             --nvo unificacion inversa
          vdesmarca          CHAR(200)

   DEFINE vcta_administra        CHAR(200),
          vcta_administra_salida CHAR(200)

   DEFINE v_sql_1            CHAR(50)
   DEFINE gi_cuantos_asi     INTEGER
   DEFINE gi_cuantos_inv     INTEGER

END GLOBALS
###############################################################################
MAIN
   OPTIONS
       PROMPT LINE LAST
       DEFER INTERRUPT

   CALL STARTLOG("UNIC005.log")

   WHENEVER ERROR CONTINUE
       DROP TABLE tmp_pla_trasp

       CREATE TEMP TABLE tmp_pla_trasp
       (
        n_registros          CHAR(570)
       )
   WHENEVER ERROR STOP

   CALL inicio()

   OPEN WINDOW unic0011 AT 2,2 WITH FORM "UNIC0051" ATTRIBUTE(BORDER)
   DISPLAY "                          < Ctrl-C > Salir                                 " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY " UNIC005       RECIBE RESPUESTA DE TRASPASO,CERTIFICACION                              " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                 Y ASIGNADOS POR UNIFICACION DE CUENTAS                                " AT 4,1 ATTRIBUTE(REVERSE)

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

   CALL lee_archivo_plano()

   PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
   FOR CHAR enter

   LET apertura = "fglgo UNIC015.4gi"
   RUN apertura

   CLOSE WINDOW unic0011
END MAIN
##########################################################################
FUNCTION inicio()

   LET HOY  = TODAY

   SELECT estado ,
          USER
   INTO   s_recibido ,
          usuario
   FROM   uni_status
   WHERE  descripcion = "RECIBIDO"

   SELECT estado
   INTO   s_aceptado
   FROM   uni_status
   WHERE  descripcion = "ACEPTADO CONFRONTA"

   SELECT codigo_afore
   INTO   codigo
   FROM   tab_afore_local

   SELECT estado
   INTO   s_solicitado
   FROM   uni_status
   WHERE  descripcion = "SOLICITADO"

   SELECT estado
   INTO   s_traspaso
   FROM   uni_status
   WHERE  descripcion = "TRASPASADO"

   SELECT estado
   INTO   s_intra
   FROM   uni_status
   WHERE  descripcion = "INTRA AFORE"

   SELECT estado
   INTO   s_liquidado
   FROM   uni_status
   WHERE  descripcion = "LIQUIDADO"

   LET vgenerado = 10
   LET vaceptado = 25
   LET vaperturado = 40
   LET vtraspasado = 100

   LET vregimen = " EXECUTE PROCEDURE sp_crea_regimen (?,?,?)"
   PREPARE genera_regimen FROM vregimen

   LET vguarda = " EXECUTE PROCEDURE sp_guarda_regimen (?)"
   PREPARE guarda_regimen FROM vguarda

   LET v_sql_1 = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
   PREPARE stmt1 FROM v_sql_1

   LET vfn_regimen = "EXECUTE PROCEDURE fn_regimen_inv(?,?,?,?,?,?)"
   PREPARE stmt2 FROM vfn_regimen

   LET vmarca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
   PREPARE marcaje FROM vmarca

   LET vdesmarca = " EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?)"
   PREPARE desmarca FROM vdesmarca

END FUNCTION
##########################################################################
FUNCTION lee_archivo_plano()

   DEFINE nss_tra         CHAR(11),
          nss_tra1        CHAR(11),
          nss_cert        CHAR(11),
          nss_cert1       CHAR(11),
          nss_asi         CHAR(11),
          nss_asi1        CHAR(11),
          nss_asi2        CHAR(11),
          vrfc            CHAR(13),
          c10_fcert       CHAR(10),
          c10_fnac        CHAR(10),
          c10_fsol        CHAR(10),
          vsexo           CHAR(01),
          ent_nacimiento  CHAR(02),
          vsello          CHAR(24),
          vcod            CHAR(02),
          vrec_cod1       CHAR(03),
          vrec_cod2       CHAR(03),
          vrec_cod3       CHAR(03),
          vrec_cod4       CHAR(03),
          vrec_cod5       CHAR(03),
          vnombre_canase  CHAR(50),
          xpaterno        CHAR(40),
          xmaterno        CHAR(40),
          xnombres        CHAR(40),
          vtipo_operacion SMALLINT,
          vsolicitud      SMALLINT,
          vfolio_tra      INTEGER,
          vfolio_cert     INTEGER,
          folio_cert2     INTEGER,
          vfolio_asi      INTEGER,
          vnumero         INTEGER,
          fecha_1a_afilia DATE,
          f_nacimiento    DATE,
          f_cert          DATE,
          f_sol           DATE

   DEFINE uni_tra         RECORD LIKE uni_det_traspaso.*
   DEFINE uni_cer         RECORD LIKE uni_det_certifica.*
   DEFINE uni_asi         RECORD LIKE uni_det_traspaso.*

   LET cont            = 0
   LET cont1           = 0
   LET cont2           = 0

   DECLARE cur_1 CURSOR FOR
   SELECT  *
   FROM    tmp_pla_trasp

   ### 15 Dic 2009 Proceso de Fallecidos
   LET   gi_cuantos_asi   = 0
   LET   gi_cuantos_inv   = 0

   FOREACH cur_1 INTO carga_reg
      ### 15 Dic 2009 Proceso de Fallecidos
      DISPLAY " TOTAL REGISTROS PROCESADOS            : ",cont  AT 13,8
      DISPLAY " TOTAL ACEPTADOS                       : ",cont1 AT 14,8
      DISPLAY " TOTAL RECHAZADOS                      : ",cont2 AT 15,8

      DISPLAY "SE APERTURARON ",gi_cuantos_asi CLIPPED," CUENTAS DE ASIGNACION MISMA AFORE" AT 17,2
      DISPLAY "SE APERTURARON ",gi_cuantos_inv CLIPPED," CUENTAS DE CERTIFICACION POR INDICADOR INVERSA" AT 18,2
      ###

      LET vcod      = ""
      LET vrec_cod1 = ""
      LET vrec_cod2 = ""
      LET vrec_cod3 = ""
      LET vrec_cod4 = ""

      LET nss_asi1  = ""
      LET nss_asi2  = ""

      IF carga_reg[1,2] = "01" THEN
         LET c10_fsol       = carga_reg[21,22],"/",
                              carga_reg[23,24],"/",
                              carga_reg[17,20]
         LET f_sol          = c10_fsol
      END IF

      IF carga_reg[1,2] = "02" THEN
         LET cont = cont + 1
         LET vtipo_operacion = carga_reg[13,14]

         CASE carga_reg[13,14]
            WHEN "31"     ---------------# traspaso x unificacion

               IF carga_reg[262,263] = "05" THEN
                  LET cont1 = cont1 + 1
                  LET nss_tra        = carga_reg[15,25]
                  LET c10_fcert      = carga_reg[311,312],"/",
                                       carga_reg[313,314],"/",
                                       carga_reg[307,310]
                  LET f_cert         = c10_fcert
                  LET fecha_1a_afilia = carga_reg[468,475]

                  UPDATE  uni_det_traspaso
                  SET     estado          = vaceptado,
                          fecha_certifica = f_cert,
                          fecha_1a_afil   = fecha_1a_afilia
                  WHERE   nss             = nss_tra
                  AND     estado          = vgenerado

               ELSE
                  LET cont2 = cont2 + 1
                  LET nss_tra = carga_reg[15,25]

                  SELECT folio
                  INTO   vfolio_tra
                  FROM   uni_det_traspaso
                  WHERE  nss     = nss_tra
                  AND    estado  = vgenerado

                  SELECT *
                  INTO   uni_tra.*
                  FROM   uni_det_traspaso
                  WHERE  nss     = nss_tra
                  AND    estado  = vgenerado
                  AND    folio   = vfolio_tra

                  LET vnombre_canase = ""
                  LET vcod      = carga_reg[262,263]
                  LET vrec_cod1 = carga_reg[264,266]
                  LET vrec_cod2 = carga_reg[267,269]
                  LET vrec_cod3 = carga_reg[270,272]
                  LET vrec_cod4 = carga_reg[273,275]
                  LET vrec_cod5 = carga_reg[276,279]
                  LET vnombre_canase = carga_reg[397,446]

                  CALL registra_error(uni_tra.*,
                                      vcod,
                                      vrec_cod1,
                                      vrec_cod2,
                                      vrec_cod3,
                                      vrec_cod4,
                                      vrec_cod5,
                                      f_sol)

                  DELETE
                  FROM   uni_det_traspaso
                  WHERE  folio   = vfolio_tra
                  AND    nss     = nss_tra
                  AND    estado  = vgenerado
                  AND    folio   = vfolio_tra

                  SELECT "X"
                  FROM   uni_unificador
                  WHERE  nss_uni  = nss_tra
                  AND    estado   = s_solicitado
                  AND    folio    = vfolio_tra

                  IF STATUS = NOTFOUND THEN

                     SELECT nss_uni
                     INTO   nss_tra1
                     FROM   uni_unificado
                     WHERE  nss_cta1 = nss_tra
                     AND    estado   = s_solicitado
                     AND    folio    = vfolio_tra
                     GROUP BY 1

                     IF vrec_cod1  = "442"  THEN
                        LET xpaterno = ""
                        LET xmaterno = ""
                        LET xnombres = ""

                        IF vnombre_canase MATCHES "*$*"  THEN
                           CALL separa_nombre(vnombre_canase)
                              RETURNING xpaterno,
                                        xmaterno,
                                        xnombres
                        ELSE
                           CALL separa_nombre1(vnombre_canase)
                              RETURNING xpaterno,
                                        xmaterno,
                                        xnombres
                        END IF

                        UPDATE uni_unificado
                        SET    estado   = s_aceptado,
                               paterno_cta1 = xpaterno,
                               materno_cta1 = xmaterno,
                               nombre_cta1  = xnombres
                        WHERE  nss_uni  = nss_tra1
                        AND    nss_cta1 = nss_tra
                        AND    folio    = vfolio_tra
                        AND    estado   = s_solicitado
                     ELSE
                        UPDATE uni_unificado
                        SET    estado   = s_aceptado
                        WHERE  nss_uni  = nss_tra1
                        AND    estado   = s_solicitado
                        AND    folio    = vfolio_tra
                     END IF
                  ELSE
                     IF vrec_cod1  = "442" THEN
                        LET xpaterno = ""
                        LET xmaterno = ""
                        LET xnombres = ""

                        IF vnombre_canase MATCHES "*$*" THEN
                           CALL separa_nombre(vnombre_canase)
                              RETURNING xpaterno,
                                        xmaterno,
                                        xnombres
                        ELSE
                            CALL separa_nombre1(vnombre_canase)
                            RETURNING xpaterno,xmaterno,xnombres
                        END IF

                        UPDATE uni_unificador
                        SET    estado   = s_aceptado,
                               paterno_uni = xpaterno,
                               materno_uni = xmaterno,
                               nombre_uni  = xnombres
                        WHERE  nss_uni  = nss_tra
                        AND    estado   = s_solicitado
                        AND    folio    = vfolio_tra
                     ELSE
                        UPDATE uni_unificador
                        SET    estado   = s_aceptado
                        WHERE  nss_uni  = nss_tra
                        AND    estado   = s_solicitado
                        AND    folio    = vfolio_tra
                     END IF
                  END IF
               END IF

            WHEN "30"            ----------# Certificados x unificacion
               IF carga_reg[262,263] = "01" THEN
                  LET cont1 = cont1 + 1
                  LET nss_cert       = carga_reg[15,25]
                  LET c10_fcert      = carga_reg[311,312],"/",
                                       carga_reg[313,314],"/",
                                       carga_reg[307,310]
                  LET f_cert         = c10_fcert
                  LET vrfc           = carga_reg[334,346]
                  LET c10_fnac       = carga_reg[451,452],"/",
                                       carga_reg[453,454],"/",
                                       carga_reg[447,450]
                  LET f_nacimiento   = c10_fnac
                  LET vsexo          = carga_reg[465,465]
                  LET ent_nacimiento = carga_reg[466,467]
                  LET fecha_1a_afilia = carga_reg[468,475]

                  UPDATE  uni_det_certifica
                  SET     estado          = s_aceptado,
                          rfc             = vrfc,
                          fecha_nac       = f_nacimiento,
                          sexo            = vsexo,
                          ent_nac         = ent_nacimiento,
                          fecha_certifica = f_cert,
                          fecha_1a_afil   = fecha_1a_afilia
                  WHERE   nss             = nss_cert
                  AND     estado          = vgenerado

                  CALL certifica_inversa(nss_cert,f_cert)
               ELSE
                  LET cont2 = cont2 + 1
                  LET nss_cert = carga_reg[15,25]

                  SELECT folio
                  INTO   vfolio_cert
                  FROM   uni_det_certifica
                  WHERE  nss     = nss_cert
                  AND    estado  = vgenerado

                  SELECT *
                  INTO   uni_cer.*
                  FROM   uni_det_certifica
                  WHERE  nss     = nss_cert
                  AND    folio   = vfolio_cert
                  AND    estado  = vgenerado

                  LET vnombre_canase = ""
                  LET vcod      = carga_reg[262,263]
                  LET vrec_cod1 = carga_reg[264,266]
                  LET vrec_cod2 = carga_reg[267,269]
                  LET vrec_cod3 = carga_reg[270,272]
                  LET vrec_cod4 = carga_reg[273,275]
                  LET vrec_cod5 = carga_reg[276,279]
                  LET vnombre_canase = carga_reg[397,446]

                  CALL registra_error(uni_cer.*,
                                      vcod,
                                      vrec_cod1,
                                      vrec_cod2,
                                      vrec_cod3,
                                      vrec_cod4,
                                      vrec_cod5,
                                      f_sol)

                  DELETE
                  FROM    uni_det_certifica
                  WHERE   nss    = nss_cert
                  AND     estado = vgenerado
                  AND     folio  = vfolio_cert

                  SELECT "X"
                  FROM   uni_unificador
                  WHERE  nss_uni = nss_cert
                  AND    estado  = s_solicitado
                  AND    folio   = vfolio_cert

                  IF STATUS = NOTFOUND  THEN
                     SELECT nss_uni
                     INTO   nss_cert1
                     FROM   uni_unificado
                     WHERE  nss_cta1 = nss_cert
                     AND    estado   = s_solicitado
                     AND    folio    = vfolio_cert

                     IF (vrec_cod1  = "792" OR
                         vrec_cod1  = "783" OR
                         vrec_cod1  = "784") THEN

                        UPDATE uni_unificado
                        SET    estado   = s_aceptado,
                               nombre_imss_cta1 = vnombre_canase
                        WHERE  nss_uni  = nss_cert1
                        AND    nss_cta1 = nss_cert
                        AND    estado   = s_solicitado
                     ELSE
                        UPDATE uni_unificado
                        SET    estado   = s_aceptado
                        WHERE  nss_uni  = nss_cert1
                        AND    nss_cta1 = nss_cert
                        AND    estado   = s_solicitado
                     END IF
                  ELSE
                     IF (vrec_cod1  = "792" OR
                         vrec_cod1  = "783" OR
                         vrec_cod1  = "784") THEN

                        UPDATE uni_unificador
                        SET    estado   = s_aceptado,
                               nombre_imss_uni = vnombre_canase
                        WHERE  nss_uni  = nss_cert
                        AND    estado   = s_solicitado
                     ELSE
                        UPDATE uni_unificador
                        SET    estado   = s_aceptado
                        WHERE  nss_uni  = nss_cert
                        AND    estado   = s_solicitado
                     END IF
                  END IF
               END IF

            WHEN "32"  -------------# Asignados por unificacion
               CASE carga_reg[262,263]
                  WHEN "60" --------# asignado misma afore
                     LET cont1 = cont1 + 1
                     LET nss_asi        = carga_reg[15,25]
                     LET c10_fcert      = carga_reg[311,312],"/",
                                          carga_reg[313,314],"/",
                                          carga_reg[307,310]
                     LET f_cert         = c10_fcert
                     LET vrfc           = carga_reg[334,346]
                     LET c10_fnac       = carga_reg[451,452],"/",
                                          carga_reg[453,454],"/",
                                          carga_reg[447,450]
                     LET f_nacimiento   = c10_fnac
                     LET vsexo          = carga_reg[465,465]
                     LET ent_nacimiento = carga_reg[466,467]
                     LET fecha_1a_afilia = carga_reg[468,475]
                     LET vsello         = carga_reg[518,541]

                     UPDATE  uni_det_asignado
                     SET     estado          = vtraspasado,
                             rfc             = vrfc,
                             fecha_nac       = f_nacimiento,
                             sexo            = vsexo,
                             ent_nac         = ent_nacimiento,
                             fecha_certifica = f_cert,
                             fecha_1a_afil   = fecha_1a_afilia
                     WHERE   nss             = nss_asi
                     AND     estado          = vgenerado

                     SELECT "X"
                     FROM   uni_unificador
                     WHERE  nss_uni = nss_asi
                     AND    estado  = s_solicitado
                     GROUP BY 1

                     IF STATUS <> NOTFOUND THEN

                        SELECT MAX(folio) + 1
                        INTO   vnumero
                        FROM   uni_folio

                        IF vnumero IS NULL THEN
                           LET vnumero = 1
                        END IF

                        INSERT INTO uni_folio
                        VALUES(vnumero)

                        LET vsolicitud = 3
                        LET nss_asi2 = ""

                        DECLARE asi_afi1 CURSOR FOR
                        SELECT nss_cta1, folio
                        FROM   uni_unificado
                        WHERE  nss_uni = nss_asi
                        AND    tipo_ent_cta1 = "01"
                        AND    cve_ent_cta1  = codigo
                        AND    estado  IN( 30,40)

                        FOREACH asi_afi1 INTO nss_asi2, folio_cert2
                           EXIT FOREACH
                        END FOREACH

                        ### 10 Dic 09 --- procesos de fallecidos
                        IF  nss_asi2   IS NULL   THEN
                            LET   nss_asi2   = nss_asi
                            SELECT folio
                              INTO folio_cert2
                              FROM uni_unificador
                             WHERE nss_uni      = nss_asi
                               AND tipo_ent_nss = '59'
                               AND estado       = 30
                        END IF

                        CALL afilia_unificador(nss_asi,
                                               nss_asi2,
                                               vsolicitud,
                                               vnumero,
                                               vsello,
                                               f_cert,
                                               carga_reg[13,14],
                                               folio_cert2)
                     ELSE
                        SELECT  MAX(folio) + 1
                        INTO    vnumero
                        FROM    uni_folio_virtual

                        IF vnumero IS NULL THEN
                           LET vnumero = 1
                        END IF

                        INSERT INTO uni_folio_virtual
                        VALUES(vnumero)

                        LET vsolicitud = 4

                        SELECT nss_uni
                        INTO   nss_asi2
                        FROM   uni_unificado
                        WHERE  nss_cta1 = nss_asi
                        AND    estado   = s_solicitado
                        GROUP BY 1

                        SELECT "X"
                        FROM   uni_unificador
                        WHERE  nss_uni = nss_asi2
                        AND    cve_ent_nss = codigo
                        AND    tipo_ent_nss = "01"
                        GROUP BY 1

                        IF SQLCA.SQLCODE <> 0 THEN
                           DECLARE asi_afi CURSOR FOR
                           SELECT  nss_cta1, folio
                           FROM    uni_unificado
                           WHERE   nss_uni = nss_asi2
                           AND     cve_ent_cta1 = codigo
                           AND     tipo_ent_cta1 = "01"

                           LET     nss_asi2     =  ''
                           FOREACH asi_afi INTO nss_asi2, folio_cert2
                              EXIT FOREACH
                           END FOREACH

                           ### 10 Dic 09 --- procesos de fallecidos
                           IF  nss_asi2    IS NULL THEN
                               LET nss_asi2  =  nss_asi
                               SELECT  folio
                                 INTO  folio_cert2
                                 FROM  uni_unificado
                                WHERE  nss_cta1 = nss_asi
                                  AND  cve_ent_cta1 = codigo
                                  AND  tipo_ent_cta1 = "59"
                           END IF
                        ELSE
                           LET nss_asi2  =  nss_asi
                           SELECT  folio
                             INTO  folio_cert2
                             FROM  uni_unificado
                            WHERE  nss_cta1 = nss_asi
                              AND  cve_ent_cta1 = codigo
                              AND  tipo_ent_cta1 = "59"
                        END IF

                        CALL afilia_unificador(nss_asi,
                                               nss_asi2,
                                               vsolicitud,
                                               vnumero,
                                               vsello,
                                               f_cert,
                                               carga_reg[13,14],
                                               folio_cert2)
                     END IF

                     SELECT "X"
                     FROM   uni_unificador
                     WHERE  nss_uni = nss_asi
                     AND    estado  = s_solicitado

                     IF STATUS = NOTFOUND THEN
                        LET  vfolio_asi = ""
                        LET  nss_asi1   = ""

                        SELECT max(folio)
                        INTO   vfolio_asi
                        FROM   uni_unificado
                        WHERE  nss_cta1 = nss_asi
                        AND    estado   = s_solicitado

                        SELECT nss_uni
                        INTO   nss_asi1
                        FROM   uni_unificado
                        WHERE  nss_cta1 = nss_asi
                        AND    estado   = s_solicitado
                        AND    folio    = vfolio_asi
                        GROUP BY 1

                        ### 10 dic 2009 Proceso de fallecidos
                        ### UPDATE uni_unificador
                        ### SET    estado          = s_traspaso,
                        ###        estado_traspaso = 1,
                        ###        estado_familia  = 1
                        ### WHERE  nss_uni         = nss_asi1
                        ### AND    estado          = s_solicitado
                        ### AND    cve_ent_nss     = codigo
                        ### AND    folio           = vfolio_asi

                        UPDATE uni_unificado
                        SET    estado          = s_traspaso,
                               estado_traspaso = 1,
                               estado_unifica  = 1
                        WHERE  nss_cta1        = nss_asi
                        AND    estado          = s_solicitado
                        AND    cve_ent_cta1    = codigo
                        AND    folio           = vfolio_asi
                     ELSE
                        SELECT max(folio)
                        INTO   vfolio_asi
                        FROM   uni_unificador
                        WHERE  nss_uni  = nss_asi
                        AND    estado   = s_solicitado

                        UPDATE uni_unificador
                        SET    estado          = s_traspaso,
                               estado_traspaso = 1,
                               estado_familia  = 1
                        WHERE  nss_uni         = nss_asi
                        AND    estado          = s_solicitado
                        AND    cve_ent_nss     = codigo

                        ### 10 dic 2009 Proceso de fallecidos
                        ###UPDATE uni_unificado
                        ###SET    estado          = s_traspaso,
                        ###       estado_traspaso = 1,
                        ###       estado_unifica  = 1
                        ###WHERE  nss_cta1        = nss_asi2
                        ###AND    nss_uni         = nss_asi
                        ###AND    estado          = s_solicitado
                        ###AND    cve_ent_cta1    = codigo
                     END IF

                  WHEN "61" ---------------------# Asignado extra afore
                     LET cont1 = cont1 + 1
                     LET nss_asi        = carga_reg[15,25]
                     LET c10_fcert      = carga_reg[311,312],"/",
                                          carga_reg[313,314],"/",
                                          carga_reg[307,310]
                     LET f_cert         = c10_fcert
                     LET vrfc           = carga_reg[334,346]
                     LET c10_fnac       = carga_reg[451,452],"/",
                                          carga_reg[453,454],"/",
                                          carga_reg[447,450]
                     LET f_nacimiento   = c10_fnac
                     LET vsexo          = carga_reg[465,465]
                     LET ent_nacimiento = carga_reg[466,467]
                     LET vsello         = carga_reg[518,541]

                     UPDATE  uni_det_asignado
                     SET     estado          = s_aceptado,
                             rfc             = vrfc,
                             fecha_nac       = f_nacimiento,
                             sexo            = vsexo,
                             ent_nac         = ent_nacimiento,
                             fecha_certifica = f_cert
                     WHERE   nss             = nss_asi
                     AND     estado          = vgenerado

                  WHEN "02"---------------------# rechazados
                     LET cont2 = cont2 + 1
                     LET nss_asi = carga_reg[15,25]

                     SELECT folio
                     INTO   vfolio_asi
                     FROM   uni_det_asignado
                     WHERE  nss     = nss_asi
                     AND    estado  = vgenerado

                     SELECT *
                     INTO   uni_asi.*
                     FROM   uni_det_asignado
                     WHERE  nss     = nss_asi
                     AND    folio   = vfolio_asi
                     AND    estado  = vgenerado


                     LET vnombre_canase = ""
                     LET vcod      = carga_reg[262,263]
                     LET vrec_cod1 = carga_reg[264,266]
                     LET vrec_cod2 = carga_reg[267,269]
                     LET vrec_cod3 = carga_reg[270,272]
                     LET vrec_cod4 = carga_reg[273,275]
                     LET vrec_cod5 = carga_reg[276,279]
                     LET vnombre_canase = carga_reg[397,446]

                     CALL registra_error(uni_asi.*,
                                         vcod,
                                         vrec_cod1,
                                         vrec_cod2,
                                         vrec_cod3,
                                         vrec_cod4,
                                         vrec_cod5,
                                         f_sol)

                     DELETE
                     FROM    uni_det_asignado
                     WHERE   nss    = nss_asi
                     AND     estado = vgenerado
                     AND     folio  = vfolio_asi

                     SELECT "X"
                     FROM   uni_unificador
                     WHERE  nss_uni  = nss_asi
                     AND    estado   = s_solicitado
                     AND    folio    = vfolio_asi

                     IF STATUS = NOTFOUND  THEN

                        SELECT nss_uni
                        INTO   nss_asi1
                        FROM   uni_unificado
                        WHERE  nss_cta1 = nss_asi
                        AND    estado   = s_solicitado
                        AND    folio    = vfolio_asi

                        IF (vrec_cod1  = "792" OR
                            vrec_cod1  = "783" OR
                            vrec_cod1  = "784") THEN

                           UPDATE uni_unificado
                           SET    estado           = s_aceptado,
                                  nombre_imss_cta1 = vnombre_canase
                           WHERE  nss_uni  = nss_asi1
                           AND    nss_cta1 = nss_asi
                           AND    estado   = s_solicitado
                           AND    folio    = vfolio_asi
                        ELSE
                           UPDATE uni_unificado
                           SET    estado   = s_aceptado
                           WHERE  nss_uni  = nss_asi1
                           AND    nss_cta1 = nss_asi
                           AND    estado   = s_solicitado
                           AND    folio    = vfolio_asi
                        END IF
                     ELSE
                        SELECT max(folio)
                        INTO   vfolio_asi
                        FROM   uni_unificado
                        WHERE  nss_uni  = nss_asi
                        AND    estado   IN (30,40)

                        IF (vrec_cod1  = "792" OR
                            vrec_cod1  = "783" OR
                            vrec_cod1  = "784") THEN

                            UPDATE uni_unificador
                            SET    estado          = s_aceptado,
                                   nombre_imss_uni = vnombre_canase
                            WHERE  nss_uni  = nss_asi
                            AND    estado   IN (30,40)
                            AND    folio    = vfolio_asi
                        ELSE
                           UPDATE uni_unificador
                           SET    estado   = s_aceptado
                           WHERE  nss_uni  = nss_asi
                           AND    estado   IN (30,40)
                           AND    folio    = vfolio_asi
                        END IF
                     END IF
               END CASE
         END CASE
      END IF
   END FOREACH

   INSERT INTO uni_ctr_archivo
   VALUES(generar,
          0,
          HOY,
          vtipo_operacion)

END FUNCTION
##########################################################################
FUNCTION afilia_unificador(nss_inf,
                           nss_cam,
                           xsolicitud,
                           xnumero,
                           xsello,
                           xfecha,
                           xcve_operacion,
                           folio_unifica)

   DEFINE tipo_unificador   CHAR(10)
   DEFINE nss_inf      CHAR(11),
          nss_cam      CHAR(11),
          xsolicitud   SMALLINT,
          xnumero      DECIMAL(10,0),
          xsello       CHAR(24),
          xfecha       DATE,
          xcve_operacion     CHAR(2)

   DEFINE folio_unifica         INTEGER
   DEFINE reg_10 RECORD LIKE afi_mae_afiliado.*
   DEFINE reg_11 RECORD LIKE afi_domicilio.*
   DEFINE reg_12 RECORD LIKE afi_mae_afiliado.*
   DEFINE reg_13 RECORD LIKE cta_ctr_cuenta.*
   DEFINE reg_14 RECORD LIKE afi_telefono.*
   DEFINE reg_15 RECORD LIKE afi_mae_benefici.*
   DEFINE reg_16 RECORD LIKE afi_mae_patron.*

   DEFINE v_existe      ,           --v_existe,
          v_edad        ,           --v_edad,
          v_criterio    ,           --v_criterio,
          v_ind_edad    SMALLINT,   --v_ind_edad,
          v_curp        CHAR(18),   --v_curp,
          v_rfc         CHAR(13),   --v_rfc,
          v_fena        DATE,       --v_fena
          v_tipo_proc   ,
          v_tipo_trasp  ,
          v_medio       ,
          v_rechazo       SMALLINT,
          v_folioatencion INTEGER

   DEFINE x_tipo_regimen     SMALLINT,
          x_estado           SMALLINT,
          inf_siefore_rcv    SMALLINT,
          cam_siefore_rcv    SMALLINT,
          x_proceso          SMALLINT,
          x_medio            CHAR(2),
          v_tipo_proceso     SMALLINT,
          v_tipo_traspaso    SMALLINT

   DEFINE xx_tipo_solicitud  SMALLINT

   DEFINE v_cod_proceso      SMALLINT,
          v_tipo_trabajador  SMALLINT,
          opc                CHAR(1)

   IF xsello = "INVERSA" THEN
      SELECT tipo_solicitud
      INTO   xx_tipo_solicitud
      FROM   afi_mae_afiliado
      WHERE  n_seguro = nss_inf
   ELSE
      LET xx_tipo_solicitud = 5
   END IF

   SELECT *
   INTO   reg_12.*
   FROM   afi_mae_afiliado
   WHERE  n_seguro = nss_inf
   AND    tipo_solicitud = xx_tipo_solicitud

   SELECT "X"
   FROM   afi_his_afiliado
   WHERE  n_seguro = nss_inf
   AND    tipo_solicitud = xx_tipo_solicitud

   IF STATUS = NOTFOUND THEN
      INSERT INTO afi_his_afiliado VALUES(reg_12.*)
   END IF

   SELECT *
   INTO   reg_13.*
   FROM   cta_ctr_cuenta
   WHERE  nss = nss_inf

   SELECT "X"
   FROM   cta_his_cuenta
   WHERE  nss = nss_inf

   IF STATUS = NOTFOUND THEN
      INSERT INTO cta_his_cuenta VALUES(reg_13.*)
   END IF

   IF xsello = "INVERSA" THEN
      LET nss_cam = nss_inf
   END IF

   SELECT *
   INTO   reg_10.*
   FROM   afi_mae_afiliado
   WHERE  n_seguro = nss_cam

   IF xsello = "INVERSA" THEN
      LET xsello = reg_10.sello_electronico
   END IF
   
   #OP 32 no heredar CURP
   LET reg_10.n_unico = NULL

   UPDATE afi_mae_afiliado
   SET    n_unico            = reg_10.n_unico,
          n_rfc              = reg_10.n_rfc,
          paterno            = reg_10.paterno,
          materno            = reg_10.materno,
          nombres            = reg_10.nombres,
          fena               = reg_10.fena,
          n_folio            = xnumero,
          edo_civil          = reg_10.edo_civil,
          localn             = reg_10.localn,
          estadon            = reg_10.estadon,
          tiptr              = reg_10.tiptr,
          cod_promotor       = reg_10.codven,
          sexo               = reg_10.sexo,
          fentcons           = xfecha,
          frecafor           = reg_10.frecafor,
          femision           = reg_10.femision,
          finitmte           = reg_10.finitmte,
          finicta            = reg_10.finicta,
          agenc_cod          = reg_10.agenc_cod,
          nacionalidad       = reg_10.nacionalidad,
          tip_prob           = reg_10.tip_prob,
          fol_prob           = reg_10.fol_prob,
          doc_prob           = reg_10.doc_prob,
          ind_infonavit      = reg_10.ind_infonavit,
          documento_1        = reg_10.documento_1,
          documento_2        = reg_10.documento_2,
          documento_3        = reg_10.documento_3,
          documento_4        = reg_10.documento_4,
          documento_5        = reg_10.documento_5,
          documento_6        = reg_10.documento_6,
          envio_dom          = reg_10.envio_dom,
          entidad_curp       = reg_10.entidad_curp,
          asigna_curp        = reg_10.asigna_curp,
          const_curp         = reg_10.const_curp,
          status_captura     = reg_10.status_captura,
          tipo_solicitud     = xsolicitud, #unificacion,
          fecha_elaboracion  = reg_10.fecha_elaboracion,
          lote               = reg_10.lote,
          fecha_envio        = reg_10.fecha_envio,
          cod_esq_comision   = reg_10.cod_esq_comision,
          ubicacion          = reg_10.ubicacion,
          fecha_1a_afil      = reg_10.fecha_1a_afil,
          indicador_c        = reg_10.indicador_c,
          indicador_d        = reg_10.indicador_d,
          indicador_e        = reg_10.indicador_e,
          cod_error_origen   = reg_10.cod_error_origen,
          folio_edo_cta      = reg_10.folio_edo_cta,
          cod_afore_ced      = codigo,
          salario_base_comis = reg_10.salario_base_comis,
          salario_actual     = reg_10.salario_actual,
          fecha_actualiza_sa = reg_10.fecha_actualiza_sa,
          coduni_n1          = reg_10.coduni_n1,
          indicador_comision = reg_10.indicador_comision,
          codven             = reg_10.codven,
          sello_electronico  = xsello
   WHERE  n_seguro           = nss_inf
   AND    tipo_solicitud     = xx_tipo_solicitud

   IF xsello <> "INVERSA" THEN
      DECLARE cur_dom CURSOR FOR
      SELECT *
      FROM   afi_domicilio
      WHERE  nss            = nss_cam
      AND    n_folio        = reg_10.n_folio
      AND    tipo_solicitud = reg_10.tipo_solicitud

      FOREACH cur_dom INTO reg_11.*
         LET reg_11.nss             = nss_inf
         LET reg_11.tipo_solicitud  = xsolicitud
         LET reg_11.n_folio         = xnumero
         LET reg_11.usuario         = usuario

         INSERT INTO afi_domicilio VALUES(reg_11.*)

      END FOREACH
   #--------telefono
      DECLARE cur_tel CURSOR FOR
      SELECT *
      FROM   afi_telefono
      WHERE  nss            = nss_cam
      AND    n_folio        = reg_10.n_folio
      AND    tipo_solicitud = reg_10.tipo_solicitud

      FOREACH cur_tel INTO reg_14.*
         LET reg_14.nss             = nss_inf
         LET reg_14.tipo_solicitud  = xsolicitud
         LET reg_14.n_folio         = xnumero
         LET reg_14.usuario         = usuario

         INSERT INTO afi_telefono VALUES(reg_14.*)
      END FOREACH
   #-------beneficiarios
      DECLARE cur_ben CURSOR FOR
      SELECT *
      FROM   afi_mae_benefici
      WHERE  n_seguro       = nss_cam
      AND    n_folio        = reg_10.n_folio
      AND    tipo_solicitud = reg_10.tipo_solicitud

      FOREACH cur_ben INTO reg_15.*
         LET reg_15.n_seguro        = nss_inf
         LET reg_15.tipo_solicitud  = xsolicitud
         LET reg_15.n_folio         = xnumero

         INSERT INTO afi_mae_benefici VALUES(reg_15.*)
      END FOREACH
   #-------patrones
      DECLARE cur_pat CURSOR FOR
      SELECT *
      FROM   afi_mae_patron
      WHERE  n_seguro       = nss_cam
      AND    n_folio        = reg_10.n_folio
      AND    tipo_solicitud = reg_10.tipo_solicitud

      FOREACH cur_pat INTO reg_16.*
         LET reg_16.n_seguro        = nss_inf
         LET reg_16.tipo_solicitud  = xsolicitud
         LET reg_16.n_folio         = xnumero

         INSERT INTO afi_mae_patron VALUES(reg_16.*)
      END FOREACH

      LET v_tipo_proc  =  1
      LET v_tipo_trasp = 11
      LET v_medio      = 10

      DECLARE curs1 CURSOR FOR stmt1
	    OPEN  curs1 USING nss_inf,
	                      HOY
	    FETCH curs1 INTO v_existe,
	                     v_edad,
	                     v_criterio,
	                     v_ind_edad,
	                     v_curp,
	                     v_rfc,
	                     v_fena
	    CLOSE curs1

   	  DECLARE cur_fn_regimen CURSOR FOR stmt2
      OPEN cur_fn_regimen USING nss_inf,
                                v_ind_edad,
                                v_ind_edad,
                                v_tipo_proc, -- 1
                                v_tipo_trasp,--11
                                v_medio      --10
      FETCH cur_fn_regimen INTO v_existe,
                                v_ind_edad,
                                v_rechazo,
                                v_folioatencion
      CLOSE cur_fn_regimen

      IF v_rechazo <> 0 THEN
	       INSERT INTO safre_tmp:rch_apertura VALUES (nss_inf,
	                                                  v_rechazo)
      END IF
   END IF

   ### 15 dic 2009 Proceso de fallecidos
   IF    xsolicitud = 3   THEN
         LET tipo_unificador =  'UNIFICADOR'
   ELSE
         LET tipo_unificador =  'UNIFICADO '
   END IF

   INSERT INTO uni_apertura_cuenta
         VALUES ( folio_unifica,
                  tipo_unificador,
                  xcve_operacion,
                  nss_inf,
                  xnumero,
                  xsolicitud)

    IF    xcve_operacion = 32     THEN
          LET   gi_cuantos_asi    =  gi_cuantos_asi   + 1
    ELSE
          LET   gi_cuantos_inv    =  gi_cuantos_inv   + 1
    END IF
END FUNCTION
##########################################################################
FUNCTION registra_error(reg5)

   DEFINE reg5    RECORD LIKE uni_err_solicitud.*

   IF     reg5.folio          IS  NULL THEN
       LET  reg5.folio         = 0
   END IF
   IF     reg5.tipo_registro  IS  NULL THEN
       LET  reg5.tipo_registro = '  '
   END IF
   IF     reg5.nss            IS  NULL THEN
       LET  reg5.nss           = '           '
   END IF
   IF     reg5.rfc            IS  NULL THEN
       LET  reg5.rfc           = '           '
   END IF
   IF     reg5.paterno        IS  NULL THEN
       LET  reg5.paterno       = '           '
   END IF
   IF     reg5.nombre         IS  NULL THEN
       LET  reg5.nombre        = '           '
   END IF
   IF     reg5.fecha_nac      IS  NULL THEN
       LET  reg5.fecha_nac     = '01/01/1899'
   END IF
   IF     reg5.sexo           IS  NULL THEN
       LET  reg5.sexo          = ' '
   END IF
   IF     reg5.ent_nac        IS  NULL THEN
       LET  reg5.ent_nac       = '  '
   END IF
   IF     reg5.nacionalidad   IS  NULL THEN
       LET  reg5.nacionalidad  = '   '
   END IF
   IF     reg5.cve_afo_ced    IS  NULL THEN
       LET  reg5.cve_afo_ced   = '   '
   END IF
   IF     reg5.estado         IS  NULL THEN
       LET  reg5.estado        = 0
   END IF

   INSERT INTO uni_err_solicitud
   VALUES(reg5.*)

END FUNCTION
##########################################################################
FUNCTION separa_nombre(nombre_var)
   DEFINE xpaterno,
          xmaterno,
          xnombres      CHAR(40),
          nombre_var    CHAR(50),
          longitud      CHAR(150),
          i,
          algo,
          pos           INTEGER,
          opc           CHAR(1),
          nom1          CHAR(40),
          nom2          CHAR(40)

   LET algo = 0
   LET pos = 0

   LET longitud = LENGTH(nombre_var)

   FOR i=1 TO longitud

      IF nombre_var[i] = "$" then
         IF algo=0 then
            LET xpaterno = nombre_var[1,i-1]
            LET pos = i+1
            LET algo = 1
         ELSE
            IF nombre_var[i] ="$" AND nombre_var[i-1]="$" THEN
               LET xmaterno = ""
               LET algo = 0
               LET pos = i+1
            ELSE
               LET xmaterno = nombre_var[pos,i-1]
               LET algo = 0
               LET pos = i+1
            END IF
         END IF
      END IF
   END FOR

   LET nom1 = nombre_var[pos]

   LET nom2 = nombre_var[i-1]

   IF nom1 = " " THEN
      LET i = i + 1
   END IF

   LET xnombres = nombre_var[pos,i-1]
   RETURN xpaterno,
          xmaterno,
          xnombres
END FUNCTION
##########################################################################
FUNCTION separa_nombre1(nombre_var)
   DEFINE xpaterno,
          xmaterno,
          xnombres      CHAR(40),
          nombre_var    CHAR(50),
          longitud      CHAR(150),
          i,
          algo,
          pos           INTEGER,
          opc           CHAR(1),
          nom1          CHAR(40),
          nom2          CHAR(40)

   LET algo = 0
   LET pos = 0

   LET longitud = LENGTH(nombre_var)

   FOR i=1 TO longitud
      IF nombre_var[i] = " " then
         IF algo=0 then
            LET xpaterno = nombre_var[1,i-1]
            LET pos = i+1
            LET algo = 1
         ELSE
            IF nombre_var[i] =" " AND nombre_var[i-1]=" " THEN
               LET xmaterno = ""
               LET algo = 0
               LET pos = i+1
            ELSE
               LET xmaterno = nombre_var[pos,i-1]
               LET algo = 0
               LET pos = i+1
            END IF
         END IF
      END IF
   END FOR

   LET nom1 = nombre_var[pos]
   LET nom2 = nombre_var[i-1]

   IF nom1 = " " THEN
      LET i = i + 1
   END IF

   LET xnombres = nombre_var[pos,i-1]

   RETURN xpaterno,
          xmaterno,
          xnombres
END FUNCTION
#################################################################
FUNCTION certifica_inversa(nss_cert,f_cert)

   DEFINE nss_cert         CHAR(11),
          f_cert           DATE,
          vfecha_causa     DATE,
          nss_cert2        CHAR(11),
          folio_cert2      INTEGER

   DEFINE vnumero          INTEGER,
          vsolicitud       SMALLINT

   DEFINE vcorrelativo     ,
          vmarca_causa     ,
          vestado_marca    ,
          vrechazo_cod     ,
          vconvive_cod     SMALLINT

   DEFINE x_extra_uni      ,
          x_marca_causa    ,
          xrechazo         ,
          xmarca           SMALLINT

   LET vcorrelativo  = 0
   LET vmarca_causa  = 0
   LET vestado_marca = 0
   LET vrechazo_cod  = 0
   LET vconvive_cod  = 0

   DECLARE cert_afi1 CURSOR FOR
   SELECT nss_uni,folio
   FROM   uni_unificado
   WHERE  nss_uni IN(SELECT a.nss_uni
                     FROM   uni_unificador a
                     WHERE  a.nss_uni = nss_cert
                     AND    a.estado  = 30)
   AND    status_convoca = 3
   AND    estado         = 40

   FOREACH cert_afi1 INTO nss_cert2,
                          folio_cert2

      SELECT MAX(folio) + 1
      INTO   vnumero
      FROM   uni_folio

      IF vnumero IS NULL THEN
          LET vnumero = 1
      END IF

      INSERT INTO uni_folio
      VALUES(vnumero)

      LET vsolicitud = 3

      CALL afilia_unificador(nss_cert,
                             nss_cert2,
                             vsolicitud,
                             vnumero,
                             "INVERSA",
                             f_cert,30,
                             folio_cert2)

      UPDATE uni_det_certifica
      SET    estado = 100
      WHERE  nss    = nss_cert
      AND    estado = 25

      LET x_extra_uni = 130
      LET x_marca_causa = 243

      EXECUTE desmarca USING nss_cert,          # nss
                             x_extra_uni,       # marca_entra
                             vcorrelativo ,     # correlativo
                             vestado_marca,     # estado_marca
                             x_marca_causa,     # marca_causa
                             usuario            # usuario

      LET x_marca_causa = 0
      LET vfecha_causa  = ""

      INSERT INTO cta_rehabilitada
      VALUES (vnumero , --  folio
              nss_cert, --- nss
              0,        --- monto_retiro
              0,        --- monto_cesantia
              0,        --- monto_voluntaria
              0,        --- monto_vivienda97
              0,        --- monto_cuota_soc
              0,        --- monto_sar
              0,        --- monto_vivienda92
              today,    --- fecha_rehabilita
              243,      --- marca_cod
              today,    --- fecha_actualiza
              0,        --- estado
              USER      --- usuario
             )

      LET  xrechazo = 0

      LET x_extra_uni = 243

      DECLARE cur_mar CURSOR FOR marcaje
      OPEN  cur_mar USING nss_cert,         #pnss
                          x_extra_uni,      #marca_entra
                          vcorrelativo,     #correlativo
                          vestado_marca,    #estado_marca
                          vrechazo_cod,     #codigo_rechazo
                          x_marca_causa,    #marca_causa
                          vfecha_causa,     #fecha_causa
                          usuario           #usuario

      FETCH cur_mar INTO xmarca,
                         xrechazo
      CLOSE cur_mar

      UPDATE uni_unificador
      SET    estado  = 40
      WHERE  folio   = folio_cert2
      AND    nss_uni = nss_cert

      UPDATE uni_unificado
      SET    estado   = 120
      WHERE  folio    = folio_cert2
      AND    nss_uni  = nss_cert2
      AND    nss_cta1 = nss_cert
      AND    estado   = 110

      EXIT FOREACH
   END FOREACH

END FUNCTION
#################################################################
