###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa          => NOTIFICA CUENTAS UNIFICADAS                             #
#Fecha             => 24 de marzo del 2000                                    #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Fecha act         => 3 de diciembre 2003                                     #
#Fecha act         => 20 DE ABRIL DE 2004                                     #
#Modificado por    => OMAR SANDOVAL BADILLO                                   #
#Fecha modificacion=> 29 junio 2004                                           #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ                         #
#Fecha modificacion=> 28 abril 2005                                           #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE enter char(001)

   DEFINE g_cza_not RECORD
          folio             INTEGER,
          folio_liquida     INTEGER,
          tipo_registro     CHAR(2),
          ident_servicio    CHAR(2),
          ident_operacion   CHAR(2),
          tipo_ent_origen   CHAR(2),
          cve_ent_origen    CHAR(3),
          tipo_ent_destino  CHAR(2),
          cve_ent_destino   CHAR(3),
          fecha_presenta    DATE,
          consec_lote       SMALLINT,
          resulta_operacion CHAR(2),
          motivo_rechazo    CHAR(3),
          estado            SMALLINT
   END RECORD

   DEFINE reg_2 RECORD LIKE uni_unificador.*

   DEFINE reg_3 RECORD LIKE uni_unificado.*

   DEFINE g_sum_not RECORD
          folio                INTEGER,
          tipo_registro        CHAR(2),
          total_detalle        INTEGER,
          total_nss_uni        INTEGER,
          total_cta_uni        INTEGER,
          estado               SMALLINT
   END RECORD

   DEFINE reg_4 RECORD
          folio             INTEGER,
          tipo_registro     CHAR(2),
          ident_servicio    CHAR(2),
          ident_operacion   CHAR(2),
          tipo_ent_origen   CHAR(2),
          cve_ent_origen    CHAR(3),
          tipo_ent_destino  CHAR(2),
          cve_ent_destino   CHAR(3),
          fecha_presenta    DATE,
          consec_lote       SMALLINT,
          resulta_operacion CHAR(2),
          motivo_rechazo    CHAR(3),
          estado            SMALLINT
   END RECORD

   DEFINE reg_5 RECORD
          folio                INTEGER,
          tipo_registro        CHAR(2),
          total_detalle        INTEGER,
          total_nss_uni        INTEGER,
          total_cta_uni        INTEGER,
          estado               SMALLINT
   END RECORD

   DEFINE cont              ,
          cont1             ,
          tot_registros     INTEGER

   DEFINE HOY                 DATE,
          AYER                DATE,
          vfliquida           DATE,
          G_LISTA             CHAR(500),
          cat                 CHAR(500),
          borra               CHAR(200),
          aux_pausa           CHAR(1),
          char                CHAR(1),
          vpregunta           CHAR(1),
          codigo              INTEGER, 
          vfolio2             INTEGER, 
         vfolio               INTEGER

   DEFINE g_paramgrales   RECORD LIKE seg_modulo.*

   DEFINE cla_sel CHAR(100)

END GLOBALS
#####################################################################
MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   CALL STARTLOG ("UNIC008.log")
   CALL inicio()
   CALL proceso_principal()

END MAIN
#####################################################################
FUNCTION inicio()

   LET HOY = TODAY
   LET AYER = TODAY - 1 units day

   SELECT codigo_afore
   INTO   codigo
   FROM   tab_afore_local

END FUNCTION
#####################################################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0081" ATTRIBUTE(BORDER)
   DISPLAY "UNIC008         GENERA NOTIFICACION DE CUENTAS UNIFICADAS                            " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "    [ Esc ] Grabar                                        [ Ctrl-C ] Salir   " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   INPUT BY NAME vfliquida,vpregunta 

      AFTER FIELD vfliquida
         SELECT "X"
         FROM   uni_unificador
         WHERE  fnotifica = vfliquida
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            PROMPT "NO HAY REGISTROS POR NOTIFICAR EN LA FECHA INGRESADA"
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         IF vfliquida IS NULL THEN
            ERROR "NO EXISTE LIQUIDACION EN ESTA FECHA"
            NEXT FIELD vfliquida
         END IF

      AFTER  FIELD vpregunta
         IF vpregunta = "S" OR vpregunta = "s"  THEN
            ERROR " PROCESANDO INFORMACION " 

            CALL genera_reporte()

            CALL actualiza_estado()

            ERROR " "

            DISPLAY "ARCHIVO GENERADO EN : ",g_paramgrales.ruta_envio CLIPPED,
                    "/",HOY USING"YYYYMMDD",".22UN" AT 18,1
            PROMPT "PROCESO FINALIZADO ...[ENTER] PARA CONTINUAR "
            FOR enter
            EXIT INPUT
         ELSE
            ERROR" PROCESO CANCELADO "
            SLEEP 2
            EXIT PROGRAM 
         END IF

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM
   END INPUT

   CLEAR WINDOW ventana_1
   CLOSE WINDOW ventana_1

END FUNCTION
#####################################################################
FUNCTION genera_reporte()

   DEFINE xnss_cta1  CHAR(11)
   DEFINE carga  CHAR(200)

   SELECT * 
   INTO   g_paramgrales.*
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,
                 "/" CLIPPED,
                 "DETUNIF" CLIPPED

   LET cont = 0
   LET cont1 = 0

   LET cla_sel = "SELECT * ",
                 "FROM   uni_unificado ",
                 "WHERE  folio = ? ",
                 "AND    nss_uni = ? ",
                 "AND    estado = 95 "

   PREPARE claexe FROM cla_sel
   DECLARE cur_3 CURSOR FOR claexe

      START REPORT listado_2 TO G_LISTA
         DECLARE cur_2 CURSOR FOR
         SELECT *
         FROM   uni_unificador
         WHERE  fnotifica = vfliquida
         AND    estado = 95
         ORDER BY ident_movimiento,nss_uni

         FOREACH cur_2 INTO reg_2.*
            LET cont = cont + 1

            SELECT n_unico
            INTO   reg_2.curp_uni
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_2.nss_uni
            AND    tipo_solicitud not in (3)

            IF reg_2.tipo_ent_nss = "59" THEN
               SELECT "X"
               FROM  uni_det_asignado
               WHERE nss = reg_2.nss_uni
               GROUP BY 1

               IF STATUS <> NOTFOUND THEN
                  SELECT curp,
                         rfc,
                         paterno,
                         materno,
                         nombre,
                         sexo,
                         ent_nac,
                         cve_doc_proba,
                         fecha_nac
                  INTO   reg_2.curp_uni,
                         reg_2.rfc_uni,
                         reg_2.paterno_uni,
                         reg_2.materno_uni,
                         reg_2.nombre_uni,
                         reg_2.sexo_uni,
                         reg_2.ent_nac_uni,
                         reg_2.tipo_documento,
                         reg_2.fecha_nac_uni
                  FROM  uni_det_asignado
                  WHERE nss = reg_2.nss_uni
                  GROUP BY 1,2,3,4,5,6,7,8,9
               ELSE
                  SELECT n_unico,
                         n_rfc,
                         paterno,
                         materno,
                         nombres,
                         sexo,
                         estadon,
                         "",
                         fena
                  INTO   reg_2.curp_uni,
                         reg_2.rfc_uni,
                         reg_2.paterno_uni,
                         reg_2.materno_uni,
                         reg_2.nombre_uni,
                         reg_2.sexo_uni,
                         reg_2.ent_nac_uni,
                         reg_2.tipo_documento,
                         reg_2.fecha_nac_uni
                  FROM   afi_mae_afiliado
                  WHERE n_seguro = reg_2.nss_uni

                  IF reg_2.curp_uni IS NOT NULL THEN
                     LET reg_2.tipo_documento = 5
                  END IF
               END IF
            END IF

            IF (reg_2.tipo_ent_nss = "00" OR
                reg_2.tipo_ent_nss = "08") THEN
               SELECT curp,
                      rfc,
                      paterno,
                      materno,
                      nombre,
                      sexo,
                      ent_nac,
                      cve_doc_proba,
                      fecha_nac
               INTO   reg_2.curp_uni,
                      reg_2.rfc_uni,
                      reg_2.paterno_uni,
                      reg_2.materno_uni,
                      reg_2.nombre_uni,
                      reg_2.sexo_uni,
                      reg_2.ent_nac_uni,
                      reg_2.tipo_documento,
                      reg_2.fecha_nac_uni
               FROM  uni_det_certifica
               WHERE nss = reg_2.nss_uni
               GROUP BY 1,2,3,4,5,6,7,8,9
            END IF

            IF reg_2.paterno_uni = "N/A" THEN
                LET reg_2.paterno_uni = ""
            END IF

            IF reg_2.materno_uni = "N/A" THEN
               LET reg_2.materno_uni = ""
            END IF

            IF (reg_2.fliquida is null OR
                reg_2.fliquida MATCHES " *" )THEN
               LET reg_2.fliquida = reg_2.fnotifica
            END IF

            IF reg_2.fnotifica > reg_2.fliquida THEN
               LET reg_2.fnotifica = reg_2.fliquida
            END IF

            OUTPUT TO REPORT listado_2(reg_2.*) #l2
         END FOREACH
      FINISH REPORT listado_2

      LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,
                    "/" CLIPPED, "CZAUNIF" CLIPPED

      DECLARE cur_4 CURSOR FOR
      SELECT *
      FROM   uni_cza_notifica
      WHERE  folio  = reg_2.folio

      START REPORT listado_4 TO G_LISTA 
         FOREACH cur_4 INTO reg_4.*
            LET g_cza_not.tipo_ent_origen  = "01"
            LET g_cza_not.cve_ent_origen   = codigo
            LET g_cza_not.tipo_ent_destino = "03"
            LET g_cza_not.cve_ent_destino  = "001"  
            LET g_cza_not.fecha_presenta   = HOY 
            LET g_cza_not.consec_lote      = 1 

            OUTPUT TO REPORT listado_4(reg_4.*) #l4
         END FOREACH
      FINISH REPORT listado_4

      LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,
                    "/" CLIPPED, "SUMUNIF" CLIPPED

      DECLARE cur_5 CURSOR FOR
      SELECT *
      FROM   uni_sum_notifica
      WHERE  folio  = reg_2.folio

      START REPORT listado_5 TO G_LISTA
         FOREACH cur_5 INTO reg_5.*
            OUTPUT TO REPORT listado_5(reg_5.*) #l5
         END FOREACH
      FINISH REPORT listado_5

      LET cat = "cat ",g_paramgrales.ruta_envio CLIPPED,"/CZAUNIF ",
                       g_paramgrales.ruta_envio CLIPPED,"/DETUNIF ",
                       g_paramgrales.ruta_envio CLIPPED,"/SUMUNIF > ",
                       g_paramgrales.ruta_envio CLIPPED,"/",
                       HOY USING"YYYYMMDD",".22UN"
      RUN cat

      LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/CZAUNIF "
      RUN borra
      LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/DETUNIF "
      RUN borra
      LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/SUMUNIF "
      RUN borra 
END FUNCTION
#####################################################################
FUNCTION actualiza_estado()

   UPDATE uni_unificador
   SET    estado = 96
   WHERE  fnotifica = vfliquida
   AND    fliquida  <= vfliquida
   AND    estado = 95

   UPDATE uni_unificado
   SET    estado = 96
   WHERE  fnotifica = vfliquida
   AND    fliquida  <= vfliquida
   AND    estado = 95

END FUNCTION
#####################################################################
REPORT listado_2(reg_2)

   DEFINE reg_2 RECORD LIKE uni_unificador.*
   DEFINE reg_3 RECORD LIKE uni_unificado.*

   OUTPUT
      PAGE LENGTH 1
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001,"02"        ,#tipo_registro
            COLUMN 003,cont USING"&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 013,reg_2.tipo_ent_uni,
            COLUMN 015,reg_2.cve_ent_uni,
            COLUMN 018,"01",
            COLUMN 020,codigo USING "&&&",
            COLUMN 023,reg_2.curp_uni,
            COLUMN 041,reg_2.nss_uni,
            COLUMN 052,reg_2.rfc_uni,
            COLUMN 065,reg_2.paterno_uni,
            COLUMN 105,reg_2.materno_uni,
            COLUMN 145,reg_2.nombre_uni,
            COLUMN 235,reg_2.sexo_uni,
            COLUMN 236,reg_2.ent_nac_uni,
            COLUMN 238,reg_2.fecha_nac_uni USING "yyyymmdd", 
            COLUMN 246,reg_2.tipo_documento,
            COLUMN 247,codigo USING "&&&",
            COLUMN 250,reg_2.num_ctas_asoc USING "&&",
            COLUMN 252,reg_2.fliquida   USING "yyyymmdd",
            COLUMN 260,reg_2.fnotifica  USING "yyyymmdd",
            COLUMN 286,reg_2.ident_movimiento USING "&&",
            COLUMN 288,reg_2.id_credito_43,
            COLUMN 289,42 SPACES 

      OPEN cur_3 USING reg_2.folio,reg_2.nss_uni

      WHILE TRUE
         FETCH cur_3 INTO reg_3.*

         LET cont = cont + 1

         IF STATUS = NOTFOUND THEN
            LET cont = cont - 1
            LET cont1 = cont1 + 1

            CLOSE cur_3

            EXIT WHILE
         END IF

         SELECT n_unico
         INTO   reg_3.curp_cta1
         FROM   afi_mae_afiliado
         WHERE  n_seguro = reg_3.nss_cta1

         IF reg_3.tipo_ent_cta1 = "59" THEN
            SELECT "X"
            FROM  uni_det_asignado
            WHERE nss = reg_3.nss_cta1

            IF STATUS <> NOTFOUND THEN
               SELECT curp,
                      rfc,
                      paterno,
                      materno,
                      nombre,
                      sexo,
                      ent_nac,
                      fecha_nac
               INTO   reg_3.curp_cta1,
                      reg_3.rfc_cta1,
                      reg_3.paterno_cta1,
                      reg_3.materno_cta1,
                      reg_3.nombre_cta1,
                      reg_3.sexo_cta1,
                      reg_3.ent_nac_cta1,
                      reg_3.fecha_nac_cta1
               FROM   uni_det_asignado
               WHERE  nss = reg_3.nss_cta1
               GROUP BY 1,2,3,4,5,6,7,8
            ELSE
               SELECT curp,
                      rfc,
                      paterno,
                      materno,
                      nombre,
                      sexo,
                      ent_nac,
                      fecha_nac
               INTO   reg_3.curp_cta1,
                      reg_3.rfc_cta1,
                      reg_3.paterno_cta1,
                      reg_3.materno_cta1,
                      reg_3.nombre_cta1,
                      reg_3.sexo_cta1,
                      reg_3.ent_nac_cta1,
                      reg_3.fecha_nac_cta1
               FROM   uni_err_solicitud
               WHERE  nss = reg_3.nss_cta1
               GROUP BY 1,2,3,4,5,6,7,8
            END IF
         END IF

         IF (reg_3.tipo_ent_cta1 = "00"  OR
             reg_3.tipo_ent_cta1 = "08") THEN
            SELECT curp,
                   rfc,
                   paterno,
                   materno,
                   nombre,
                   sexo,
                   ent_nac,
                   fecha_nac
            INTO   reg_3.curp_cta1,
                   reg_3.rfc_cta1,
                   reg_3.paterno_cta1,
                   reg_3.materno_cta1,
                   reg_3.nombre_cta1,
                   reg_3.sexo_cta1,
                   reg_3.ent_nac_cta1,
                   reg_3.fecha_nac_cta1
            FROM   uni_det_certifica
            WHERE  nss = reg_3.nss_cta1
            GROUP BY 1,2,3,4,5,6,7,8
         END IF

         IF reg_3.paterno_cta1 = "N/A" THEN
            LET reg_3.paterno_cta1 = ""
         END IF

         IF reg_3.materno_cta1 = "N/A" THEN
            LET reg_3.materno_cta1 = ""
         END IF

         PRINT COLUMN 001,"03",
               COLUMN 003,cont USING"&&&&&&&&&&" ,#cont_servicio ,
               COLUMN 013,reg_3.nss_uni,
               COLUMN 024,"01",
               COLUMN 026,codigo USING "&&&",
               COLUMN 029,reg_3.curp_cta1,
               COLUMN 047,reg_3.nss_cta1,
               COLUMN 058,reg_3.rfc_cta1,
               COLUMN 071,reg_3.paterno_cta1,
               COLUMN 111,reg_3.materno_cta1,
               COLUMN 151,reg_3.nombre_cta1,
               COLUMN 241,reg_3.sexo_cta1,
               COLUMN 242,reg_3.ent_nac_cta1,
               COLUMN 244,reg_3.fecha_nac_cta1 USING "yyyymmdd",
               COLUMN 257,reg_3.nss_cta2,
               COLUMN 287,reg_2.id_credito_43,
               COLUMN 288,43 SPACES 
      END WHILE
END REPORT
#####################################################################
REPORT listado_4(reg_4)

   DEFINE reg_4 RECORD
          folio             INTEGER,
          tipo_registro     CHAR(2),
          ident_servicio    CHAR(2),
          ident_operacion   CHAR(2),
          tipo_ent_origen   CHAR(2),
          cve_ent_origen    CHAR(3),
          tipo_ent_destino  CHAR(2),
          cve_ent_destino   CHAR(3),
          fecha_presenta    DATE,
          consec_lote       SMALLINT,
          resulta_operacion CHAR(2),
          motivo_rechazo    CHAR(3),
          estado            SMALLINT
   END RECORD

   OUTPUT
      PAGE LENGTH 1
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001,"01",
            COLUMN 003,"02", 
            COLUMN 005,"22",
            COLUMN 007,"01",
            COLUMN 009,codigo USING "&&&",
            COLUMN 012,"03",
            COLUMN 014,"001",
            COLUMN 017,HOY USING "YYYYMMDD",
            COLUMN 025,"001",
            COLUMN 028, 303 SPACES

END REPORT
#####################################################################
REPORT listado_5(reg_5)

   DEFINE reg_5 RECORD
          folio                INTEGER,
          tipo_registro        CHAR(2),
          total_detalle        INTEGER,
          total_nss_uni        INTEGER,
          total_cta_uni        INTEGER,
          estado               SMALLINT
   END RECORD

   OUTPUT
      PAGE LENGTH 1
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001,"09",#tipo_registro
            COLUMN 003,cont USING "&&&&&&&&&" ,#cont_servicio ,
            COLUMN 012,cont1 USING "&&&&&&&&&",
            COLUMN 021,cont - cont1 USING "&&&&&&&&&",
            COLUMN 030,301 SPACES

END REPORT
