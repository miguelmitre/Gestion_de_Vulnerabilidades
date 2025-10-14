######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Propietario       => EFP                                            #
#Programa          => ACRC021                                        #
#Descripcion       => RECIBE ARCHIVO OPER.01 ANUALIDADES GARANTIZADAS#
#Sistema           => ACR                                            #
#Fecha creacion    => 17 NOVIEMBRE 2009                              #
#Por               => STEFANIE DANIELA VERA PINA                     #
#Modificado        => DMR 05 SEPTIEMBRE 2013                         #
#                  => Estandarizacion de Programas de Anual. Garanti.#
#                  => y adicionalmente montos en sumario DEC(18,6)   #
#                  => MPT 13/09/2012 version 2.0.1    CPL-1410       #
#Modificado        => DMR 31/oct/2013 STARTLOG Personalizado         #
######################################################################
DATABASE safre_af

GLOBALS
   DEFINE reg_cza_acr_ag RECORD
      tipo_registro           CHAR(2),
      ident_servicio          CHAR(2),
      ident_operacion         CHAR(2),
      tipo_ent_origen         CHAR(2),
      cve_ent_origen          CHAR(3),
      tipo_ent_destino        CHAR(2),
      cve_ent_destino         CHAR(3),
      ent_fed_envio_lote      CHAR(3),
      fecha_presentacion      DATE,
      consec_lote_dia         SMALLINT,
      cve_mod_recepcion       CHAR(2),
      cod_result_operac       CHAR(2),
      rechazo                 CHAR(9)
   END RECORD

   DEFINE reg_det_acr_ag RECORD
      tipo_registro           CHAR(2),
      cont_servicio           DECIMAL(10,0),
      tipo_recep_cuenta       CHAR(2),
      cve_recep_cuenta        CHAR(3),
      tipo_ced_cuenta         CHAR(2),
      cve_ced_cuenta          CHAR(3),
      tipo_transferencia      CHAR(2),
      fecha_presentacion      DATE,
      curp_infonavit          CHAR(18),
      nss_infonavit           CHAR(11),
      rfc_infonavit           CHAR(13),
      paterno_infonavit       CHAR(40),
      materno_infonavit       CHAR(40),
      nombres_infonavit       CHAR(40),
      ident_lote_devol        CHAR(16),
      nss_afore               CHAR(11),
      rfc_afore               CHAR(13),
      paterno_afore           CHAR(40),
      materno_afore           CHAR(40),
      nombres_afore           CHAR(40),
      aivs_v92                DECIMAL(22,6),
      aivs_v97                DECIMAL(22,6),
      nombre_imss             CHAR(50),
      num_cred_infonavit      DECIMAL(10,0),
      cod_result_operac       CHAR(002),
      diag_proceso            CHAR(015)
   END RECORD

   DEFINE reg_sum_acr_ag RECORD
      tipo_registro           CHAR(2),
      cant_reg_det            DECIMAL(9,0),
      sum_aivs_v92            DECIMAL(22,6),
      sum_aivs_v97            DECIMAL(22,6)
   END RECORD

   DEFINE g_seg_modulo   RECORD LIKE seg_modulo.*

   DEFINE
      HOY                     DATE

   DEFINE
      enter                   CHAR(1),
      generar                 CHAR(20),
      archivo_traspaso        CHAR(200),
      c_paso_cza              CHAR(8),
      c_paso_det              CHAR(8),
      c_fecha_cza             CHAR(10),
      c_fecha_det             CHAR(10),
      g_usuario               CHAR(8),
      ejecuta                 CHAR(300)

   DEFINE
      pmarca_causa            ,
      vmarca_estado           ,
      vcodigo_rechazo         ,
      xcodigo_marca           ,
      xcodigo_rechazo         SMALLINT

   DEFINE
      cuantos                 ,
      vfolio                  ,
      vtotal_reg              ,
      vtotal_aprob            ,
      vtotal_rech             ,
      vtotal_pend             INTEGER
END GLOBALS


MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV("USER")||".ACRC021.log")
   CALL inicio()

   OPEN WINDOW v1 AT 3,3 WITH FORM "ACRC0211"  ATTRIBUTE( BORDER )
   DISPLAY " ACRC021  RECIBE ARCHIVO OPER.01 ANUALIDADES GARANTIZADAS                " AT 3,1 ATTRIBUTE (REVERSE)
   DISPLAY "<ESC> Ejecutar                                          <Ctrl-C> Salir " AT 1,1
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE (REVERSE)

   INPUT BY NAME generar
      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR " CAMPO NO PUEDE SER NULO "
            NEXT FIELD generar
         END IF

         SELECT "X"
         FROM  acr_ctr_arh a
         WHERE a.nombre_archivo = generar

         IF SQLCA.SQLCODE = 0 THEN
            PROMPT "ARCHIVO YA PROCESADO,[Enter] P/SALIR" FOR enter
            EXIT INPUT
         END IF

      ON KEY (ESC)
         IF generar IS NULL THEN
            ERROR " CAMPO NO PUEDE SER NULO "
            NEXT FIELD generar
         END IF

         CALL limpieza(g_seg_modulo.ruta_rescate,generar)

         DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
         SLEEP 3

         LET archivo_traspaso = g_seg_modulo.ruta_rescate CLIPPED,
                                "/",generar CLIPPED

         LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_acr_ag

         SELECT count(*)
         INTO   cuantos
         FROM   safre_tmp:tmp_acr_ag

         IF cuantos = 0 THEN
            DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
            AT 19,2 ATTRIBUTE(REVERSE)
            SLEEP 3
            NEXT FIELD generar
         END IF

         CALL crea_tablas()
         CALL validacion_previa()
         CALL lee_archivo_plano()

         SELECT COUNT(*)
         INTO   vtotal_reg
         FROM   safre_tmp:det_acr_ag

         SELECT COUNT(*)
         INTO   vtotal_aprob
         FROM   safre_tmp:det_acr_ag c
         WHERE  c.estado = 0

         SELECT COUNT(*)
         INTO   vtotal_rech
         FROM   safre_tmp:det_acr_ag c
         WHERE  c.estado <> 0

         INSERT INTO acr_ctr_arh
         VALUES(generar,vtotal_reg,vtotal_aprob,vtotal_rech,vtotal_pend,HOY,
                g_usuario)

         DISPLAY "TOTAL DE REGISTROS .. : ",vtotal_reg AT 10,6
         DISPLAY "TOTAL ACEPTADOS ..... : ",vtotal_aprob AT 12,6
         DISPLAY "TOTAL RECHAZADOS .... : ",vtotal_rech AT 14,6

         PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter

         EXIT INPUT

      ON KEY (INTERRUPT)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
         FOR enter
         EXIT INPUT
   END INPUT

   DISPLAY "                                " AT 10,6
   DISPLAY "                                " AT 12,6
   DISPLAY "                                " AT 14,6
   DISPLAY "                                " AT 18,2

   CLOSE WINDOW v1
END MAIN


FUNCTION inicio()
#----------------
   LET HOY = TODAY

   SELECT USER
   INTO   g_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   g_seg_modulo.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp

      DROP TABLE tmp_acr_ag

      CREATE TABLE tmp_acr_ag
        (n_registros           CHAR(730))

      DATABASE safre_af
   WHENEVER ERROR STOP
END FUNCTION


FUNCTION crea_tablas()
#---------------------
   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp

      DROP TABLE cza_acr_ag
      DROP TABLE det_acr_ag
      DROP TABLE sum_acr_ag
   WHENEVER ERROR STOP

   CREATE TABLE cza_acr_ag (
      tipo_registro           CHAR(2),
      ident_servicio          CHAR(2),
      ident_operacion         CHAR(2),
      tipo_ent_origen         CHAR(2),
      cve_ent_origen          CHAR(3),
      tipo_ent_destino        CHAR(2),
      cve_ent_destino         CHAR(3),
      ent_fed_envio_lote      CHAR(3),
      fecha_presentacion      DATE,
      consec_lote_dia         SMALLINT,
      cve_mod_recepcion       CHAR(2),
      cod_result_operac       CHAR(2),
      rechazo                 CHAR(9)
      );

   CREATE TABLE det_acr_ag (
      tipo_registro           CHAR(2),
      cont_servicio           DECIMAL(10,0),
      tipo_recep_cuenta       CHAR(2),
      cve_recep_cuenta        CHAR(3),
      tipo_ced_cuenta         CHAR(2),
      cve_ced_cuenta          CHAR(3),
      tipo_transferencia      CHAR(2),
      fecha_presentacion      DATE,
      curp_infonavit          CHAR(18),
      nss_infonavit           CHAR(11),
      rfc_infonavit           CHAR(13),
      paterno_infonavit       CHAR(40),
      materno_infonavit       CHAR(40),
      nombres_infonavit       CHAR(40),
      ident_lote_devol        CHAR(16),
      nss_afore               CHAR(11),
      rfc_afore               CHAR(13),
      paterno_afore           CHAR(40),
      materno_afore           CHAR(40),
      nombres_afore           CHAR(40),
      aivs_v92                DECIMAL(22,6),
      aivs_v97                DECIMAL(22,6),
      nombre_imss             CHAR(50),
      num_cred_infonavit      DECIMAL(10,0),
      cod_result_operac       CHAR(002),
      diag_proceso            CHAR(015),
      estado                  SMALLINT,
      rechazo_cod             SMALLINT
      );

   CREATE TABLE sum_acr_ag (
      tipo_registro          CHAR(2),
      cant_reg_det           DECIMAL(9,0),
      sum_aivs_v92           DECIMAL(22,6),
      sum_aivs_v97           DECIMAL(22,6)
      );

   DATABASE safre_af
END FUNCTION


FUNCTION validacion_previa()
#---------------------------
   DEFINE
      c2_tipo_registro      CHAR(2)

   DEFINE
      sw_1                  ,
      sw_2                  ,
      sw_9                  SMALLINT

   DECLARE cur_2 CURSOR FOR
   SELECT UNIQUE(n_registros[1,2])
   FROM   safre_tmp:tmp_acr_ag

   LET sw_1 = 0
   LET sw_2 = 0
   LET sw_9 = 0

   FOREACH cur_2 INTO c2_tipo_registro
      CASE c2_tipo_registro
         WHEN "01"
            LET sw_1 = 1
         WHEN "02"
            LET sw_2 = 1
         WHEN "09"
            LET sw_9 = 1
      END CASE
   END FOREACH

   IF sw_1 = 0 THEN
      PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" 
      FOR enter
      EXIT PROGRAM
   END IF

   IF sw_2 = 0 THEN
      PROMPT "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE"
      FOR enter
      EXIT PROGRAM
   END IF

   IF sw_9 = 0 THEN
      PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" 
      FOR enter
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION lee_archivo_plano()
#---------------------------
   DEFINE
      cont                INTEGER

   DEFINE
      carga_reg           CHAR(730),
      c2_ident_operacion  CHAR(2)

   DECLARE cur_tmp_acr CURSOR FOR
   SELECT  *
   FROM safre_tmp:tmp_acr_ag

   LET cont = 0
   LET c2_ident_operacion = ""

   FOREACH cur_tmp_acr INTO carga_reg
      LET cont = cont + 1

   #-- ENCABEZADO SOLICITUD DE SALDO DE CREDITO CON ANUALIDADES GARANTIZADAS --#

      IF carga_reg[5,6] = "01" THEN
         LET c2_ident_operacion = "01"
         LET reg_cza_acr_ag.tipo_registro      = carga_reg[001,002]
         LET reg_cza_acr_ag.ident_servicio     = carga_reg[003,004]
         LET reg_cza_acr_ag.ident_operacion    = carga_reg[005,006]
         LET reg_cza_acr_ag.tipo_ent_origen    = carga_reg[007,008]
         LET reg_cza_acr_ag.cve_ent_origen     = carga_reg[009,011]
         LET reg_cza_acr_ag.tipo_ent_destino   = carga_reg[012,013]
         LET reg_cza_acr_ag.cve_ent_destino    = carga_reg[014,016]
         LET reg_cza_acr_ag.ent_fed_envio_lote = carga_reg[017,019]
         LET c_paso_cza                        = carga_reg[020,027]
         LET reg_cza_acr_ag.consec_lote_dia    = carga_reg[028,030]

         LET c_fecha_cza = c_paso_cza[5,6],"/",
                           c_paso_cza[7,8],"/",
                           c_paso_cza[1,4]

         LET reg_cza_acr_ag.fecha_presentacion = c_fecha_cza

         INSERT INTO safre_tmp:cza_acr_ag
         VALUES(reg_cza_acr_ag.*)
      END IF

      #-- DETALLE SOLICITUD DE SALDO DE CREDITO CON ANUALIDADES GARANTIZADAS --#

      IF carga_reg[1,2] = "02" AND c2_ident_operacion = "01" THEN
         LET reg_det_acr_ag.tipo_registro         = carga_reg[001,002]
         LET reg_det_acr_ag.cont_servicio         = carga_reg[003,012]
         LET reg_det_acr_ag.tipo_recep_cuenta     = carga_reg[013,014]
         LET reg_det_acr_ag.cve_recep_cuenta      = carga_reg[015,017]
         LET reg_det_acr_ag.tipo_ced_cuenta       = carga_reg[018,019]
         LET reg_det_acr_ag.cve_ced_cuenta        = carga_reg[020,022]
         LET reg_det_acr_ag.tipo_transferencia    = carga_reg[023,024]
         LET c_paso_det                           = carga_reg[025,032]
         LET reg_det_acr_ag.curp_infonavit        = carga_reg[041,058]
         LET reg_det_acr_ag.nss_infonavit         = carga_reg[059,069]
         LET reg_det_acr_ag.rfc_infonavit         = carga_reg[085,097]
         LET reg_det_acr_ag.paterno_infonavit     = carga_reg[098,137]
         LET reg_det_acr_ag.materno_infonavit     = carga_reg[138,177]
         LET reg_det_acr_ag.nombres_infonavit     = carga_reg[178,217]
         LET reg_det_acr_ag.ident_lote_devol      = carga_reg[240,255]
         LET reg_det_acr_ag.nss_afore             = carga_reg[271,281]
         LET reg_det_acr_ag.rfc_afore             = carga_reg[282,294]
         LET reg_det_acr_ag.paterno_afore         = carga_reg[325,364]
         LET reg_det_acr_ag.materno_afore         = carga_reg[365,404]
         LET reg_det_acr_ag.nombres_afore         = carga_reg[405,444]
         LET reg_det_acr_ag.aivs_v92              = carga_reg[490,504]
         LET reg_det_acr_ag.aivs_v97              = carga_reg[565,579]
         LET reg_det_acr_ag.nombre_imss           = carga_reg[588,637]
         LET reg_det_acr_ag.num_cred_infonavit    = carga_reg[638,647]
         LET reg_det_acr_ag.cod_result_operac     = carga_reg[648,649]
         LET reg_det_acr_ag.diag_proceso          = carga_reg[650,664]

         LET c_fecha_det = c_paso_det[5,6],"/",
                           c_paso_det[7,8],"/",
                           c_paso_det[1,4]

         LET reg_det_acr_ag.fecha_presentacion  = c_fecha_det

         LET reg_det_acr_ag.aivs_v92        = reg_det_acr_ag.aivs_v92 / 1000000

         LET reg_det_acr_ag.aivs_v97        = reg_det_acr_ag.aivs_v97 / 1000000

         INSERT INTO safre_tmp:det_acr_ag
         VALUES( reg_det_acr_ag.*,0,0 )

         SELECT "X"
         FROM  cta_act_marca
         WHERE marca_cod = 234
         AND   nss = reg_det_acr_ag.nss_infonavit
         GROUP BY 1

         LET vmarca_estado   = 0
         LET vcodigo_rechazo = 0
         LET pmarca_causa    = 0

         IF SQLCA.SQLCODE <> 0 THEN
            LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                          "'",reg_det_acr_ag.nss_infonavit,"'",
                          ",",234,
                          ",",reg_det_acr_ag.cont_servicio,
                          ",",vmarca_estado,
                          ",",vcodigo_rechazo,
                          ",",pmarca_causa,
                          ",","'","'", ",",
                          "'",g_usuario,"'",")"

            LET ejecuta = ejecuta CLIPPED

            PREPARE clausula_spl234 FROM ejecuta
            DECLARE cursor_marca234 CURSOR FOR clausula_spl234
            OPEN cursor_marca234

            FETCH cursor_marca234 INTO xcodigo_marca, xcodigo_rechazo

               IF xcodigo_rechazo <> 0 THEN
                  UPDATE safre_tmp:det_acr_ag
                  SET    safre_tmp:det_acr_ag.estado = xcodigo_rechazo
                  WHERE  safre_tmp:det_acr_ag.nss_infonavit = reg_det_acr_ag.nss_infonavit
               END IF

            CLOSE cursor_marca234
         END IF

         SELECT "X"
         FROM  cta_act_marca
         WHERE marca_cod = 235
         AND   nss = reg_det_acr_ag.nss_infonavit
         GROUP BY 1

         LET vmarca_estado   = 0
         LET vcodigo_rechazo = 0
         LET pmarca_causa    = 0

         IF SQLCA.SQLCODE <> 0 THEN
            LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                          "'",reg_det_acr_ag.nss_infonavit,"'",
                          ",",235,
                          ",",reg_det_acr_ag.cont_servicio,
                          ",",vmarca_estado,
                          ",",vcodigo_rechazo,
                          ",",pmarca_causa,
                          ",","'","'", ",",
                          "'",g_usuario,"'",")"

            LET ejecuta = ejecuta CLIPPED

            PREPARE clausula_spl235 FROM ejecuta
            DECLARE cursor_marca235 CURSOR FOR clausula_spl235
            OPEN cursor_marca235

            FETCH cursor_marca235 INTO xcodigo_marca, xcodigo_rechazo

               IF xcodigo_rechazo <> 0 THEN
                  UPDATE safre_tmp:det_acr_ag
                  SET    safre_tmp:det_acr_ag.estado = xcodigo_rechazo
                  WHERE  safre_tmp:det_acr_ag.nss_infonavit = reg_det_acr_ag.nss_infonavit
               END IF

            CLOSE cursor_marca235
         END IF
      END IF

      #-- SUMARIO SOLICITUD DE SALDO DE CREDITO CON ANUALIDADES GARANTIZADAS --#

      IF carga_reg[1,2] = "09" AND c2_ident_operacion = "01" THEN
         LET c2_ident_operacion = ""
         LET reg_sum_acr_ag.tipo_registro = carga_reg[001,002]
         LET reg_sum_acr_ag.cant_reg_det  = carga_reg[003,011]
         LET reg_sum_acr_ag.sum_aivs_v92  = carga_reg[057,074]
         LET reg_sum_acr_ag.sum_aivs_v97  = carga_reg[135,152]

         LET reg_sum_acr_ag.sum_aivs_v92 = reg_sum_acr_ag.sum_aivs_v92 / 1000000
         LET reg_sum_acr_ag.sum_aivs_v97 = reg_sum_acr_ag.sum_aivs_v97 / 1000000

         INSERT INTO safre_tmp:sum_acr_ag
         VALUES(reg_sum_acr_ag.*)
      END IF
   END FOREACH
END FUNCTION

