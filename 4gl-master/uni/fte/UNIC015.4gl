###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.    					      #
#Programa          => APERTURA DE CUENTAS INDIVIDUALES A TRASPASAR            #
#Fecha             => 10 DE AGOSTO DEL 2000                                   #
#Actualizado       => MIGUEL ANGEL HERNANDEZ MARTINEZ.                        #
#Fecha             => 01 julio de 2004                                        #
#fecha actualiza   => 14 enero 2005                                           #
#Fecha actualiza   => 25 mayo 2007                                            #
#Fecha Act         => 26 marzo 2008                                           #
#Fecha Act         => 07 agosto 2008                                          #
#Fecha Act         => 29 agosto 2008                                          #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE enter char(001)
   DEFINE reg_1  RECORD LIKE uni_det_traspaso.*

   DEFINE cont,
          tot_registros,
          vnum_opera,
          vnumero,
          ultimo_folio,
          vfolio2,
          vfolio            INTEGER

   DEFINE vestado_causa,
          vmarca_causa,
          vcorrelativo,
          vextra_uni,
          vextra_cta1,
          vconvive_cod,
          vrechazo_cod,
          vcodigo_afore,
          xgenerado,
          xaceptado,
          xaperturado,
          xtraspasado,
          vsolicitado,
          vaceptado,
          sw,
          cont_tra,
          cont_cer,
          cont_asi,
          vtraspaso         SMALLINT

   DEFINE vfecha_causa,
          HOY               DATE

   DEFINE G_LISTA           CHAR(500),
          cat               CHAR(500),
          borra             CHAR(200),
          vmarca            CHAR(100),
          aux_pausa         CHAR(001),
          char              CHAR(001),
          vpregunta         CHAR(001),
          vclave_entidad    CHAR(003),
          usuario           CHAR(008),
          hora              CHAR(008)

   DEFINE g_paramgrales   RECORD LIKE seg_modulo.*

   DEFINE vregimen          CHAR(100)

   DEFINE tmp_folio         INTEGER,
          tipo_proceso      SMALLINT,
          sel_where         CHAR(800),
          nombre_completo   CHAR(40)

   DEFINE x_total_cedentes  SMALLINT

   DEFINE vcta_administra        CHAR(200),
          vcta_administra_salida CHAR(200)

   DEFINE vfn_regimen        CHAR(500),
          v_sql_1            CHAR(50)

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
END GLOBALS
##########################################################################
MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT

   CALL STARTLOG("UNIC015.log")

   CALL inicio()
   CALL proceso_principal()

END MAIN
##########################################################################
FUNCTION inicio()

   LET HOY = TODAY

   SELECT codigo_afore,
          USER
   INTO   vcodigo_afore,
          usuario
   FROM   tab_afore_local

   LET  vclave_entidad = "09 "

   SELECT estado
   INTO   vaceptado
   FROM   uni_status
   WHERE  descripcion = "ACEPTADO CONFRONTA"

   SELECT estado
   INTO   vsolicitado
   FROM   uni_status
   WHERE  descripcion = "SOLICITADO"

   SELECT estado
   INTO   vtraspaso
   FROM   uni_status
   WHERE  descripcion = "TRASPASADO"

   LET xgenerado   = 10
   LET xaceptado   = 25
   LET xaperturado = 40
   LET xtraspasado = 100

   SELECT marca_cod
   INTO   vextra_uni
   FROM   tab_marca
   WHERE  marca_desc = "UNIFICACION EXTRA-AFORE"

   SELECT marca_cod
   INTO   vextra_cta1
   FROM   tab_marca
   WHERE  marca_desc = "UNIFICACION EXTRA-AFORE U"

   LET sel_where = " SELECT NVL(TRIM(a.paterno),' ')||' '",
                         "||NVL(TRIM(a.materno),' ')||' '",
                         "||NVL(TRIM(a.nombres),' ') nombre",
                   " FROM   afi_mae_afiliado a,uni_apertura_cuenta b",
                   " WHERE  a.n_seguro = ? ",
                   " AND    a.n_folio  = b.n_folio ",
                   " AND    a.tipo_solicitud = b.tipo_solicitud ",
                   " UNION ",
                   " SELECT NVL(TRIM(a.paterno),' ')||' '",
                         "||NVL(TRIM(a.materno),' ')||' '",
                         "||NVL(TRIM(a.nombres),' ') nombre",
                   " FROM   afi_solicitud a,uni_apertura_cuenta b",
                   " WHERE  a.n_seguro = ? ",
                   " AND    a.n_folio  = b.n_folio ",
                   " AND    a.tipo_solicitud = b.tipo_solicitud "

   PREPARE query_nom FROM sel_where

   LET vmarca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
   PREPARE marcaje FROM vmarca

   LET vmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
   PREPARE eje_desmarca FROM vmarca

   LET vregimen = " EXECUTE PROCEDURE sp_crea_regimen (?,?,?)"
   PREPARE genera_regimen FROM vregimen

   LET v_sql_1 = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
   PREPARE stmt1 FROM v_sql_1

   LET vfn_regimen = "EXECUTE PROCEDURE fn_regimen_inv(?,?,?,?,?,?)"
   PREPARE stmt2 FROM vfn_regimen

---cuentas administradas
   {LET vcta_administra = " EXECUTE FUNCTION fn_cuenta_entrante (?,?,?,?,?,?,?,?)"
   PREPARE cta_administra FROM vcta_administra}
---cuentas administradas

   LET vrechazo_cod  = 0
   LET vconvive_cod  = 0
   LET vcorrelativo  = 0
   LET vmarca_causa  = 0
   LET vfecha_causa  = ""

END FUNCTION
##########################################################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0151" ATTRIBUTE(BORDER)
   DISPLAY "UNIC015         APERTURA CUENTAS INDIVIDUALES POR UNIFICACION                        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                               DE CUENTAS                                            " AT 4,1 ATTRIBUTE(REVERSE)

   DISPLAY " < ESC > Iniciar                                            < Ctrl-C > Salir   " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   INPUT BY NAME vpregunta
      AFTER FIELD vpregunta
         IF vpregunta MATCHES "[Nn]" THEN
            ERROR " PROCESO CANCELADO "
            SLEEP 2
            ERROR ""
            EXIT PROGRAM
         ELSE
            LET sw = 0

            SELECT "X"
            FROM   uni_det_traspaso
            WHERE  estado = vaceptado
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               LET sw = 1
            END IF

            SELECT "X"
            FROM   uni_det_certifica
            WHERE  estado = vaceptado
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               LET sw = 1
            END IF

            SELECT "X"
            FROM   uni_det_asignado
            WHERE  estado = vaceptado
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               LET sw = 1
            END IF

            IF sw = 0 THEN
               PROMPT " NO HAY CUENTAS PENDIENTES POR APERTURAR"
               FOR CHAR enter
               EXIT PROGRAM
            END IF
         END IF

      ON KEY (ESC)
         IF vpregunta MATCHES "[Nn]" THEN
            ERROR " PROCESO CANCELADO "
            SLEEP 2
            ERROR ""
         ELSE
            ERROR " PROCESANDO INFORMACION "

            CALL carga_solicitud()
            CALL actualiza_historico()

            PROMPT "PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
            FOR CHAR enter

            EXIT INPUT
         END IF
      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM
   END INPUT

   CLEAR WINDOW ventana_1
   CLOSE WINDOW ventana_1

END FUNCTION
##########################################################################
FUNCTION carga_solicitud()

   DEFINE vtipo SMALLINT

   LET cont_tra = 0
   LET cont_cer = 0
   LET cont_asi = 0

   ---- traspaso

   DECLARE sol_tra CURSOR FOR
   SELECT c.*
   FROM   uni_det_traspaso c
   WHERE  c.estado = xaceptado
   FOR UPDATE

   LET vtipo = 1

   FOREACH sol_tra INTO reg_1.*
      LET tipo_proceso = reg_1.cve_operacion

      CALL apertura_cuenta(reg_1.*,vtipo)

      LET cont_tra = cont_tra + 1

      UPDATE uni_det_traspaso
      SET    uni_det_traspaso.estado = xaperturado
      WHERE CURRENT OF sol_tra

      DISPLAY "SE ABRIERON ",cont_tra CLIPPED," CUENTAS DE TRASPASOS" AT 17,2
   END FOREACH

   ---- certifica
   DECLARE sol_cer CURSOR FOR
   SELECT a.*
   FROM   uni_det_certifica a
   WHERE  a.estado = xaceptado
   FOR UPDATE

   LET vtipo = 2

   FOREACH sol_cer INTO reg_1.*
      LET tipo_proceso = reg_1.cve_operacion

      CALL apertura_cuenta(reg_1.*,vtipo)

      SELECT COUNT(*)
      INTO   x_total_cedentes
      FROM   taa_cd_det_cedido
      WHERE  n_seguro = reg_1.nss
      AND    estado IN (12,103)

      IF x_total_cedentes > 0 THEN
          UPDATE taa_cd_det_cedido
          SET   estado = 98
          WHERE n_seguro    = reg_1.nss
          AND   estado in (103,12)
      END IF

      LET cont_cer = cont_cer + 1

      UPDATE uni_det_certifica
      SET    uni_det_certifica.estado = xtraspasado
      WHERE CURRENT OF sol_cer

      SELECT "X"
      FROM   uni_unificador
      WHERE  folio = reg_1.folio
      AND    nss_uni = reg_1.nss
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         UPDATE uni_unificado
         SET    estado = 40,
                estado_traspaso = estado_traspaso + 1
         WHERE  folio = reg_1.folio
         AND    nss_cta1 = reg_1.nss
      ELSE
         UPDATE uni_unificador
         SET    estado = 40,
                estado_traspaso = estado_traspaso + 1
         WHERE  folio = reg_1.folio
         AND    nss_uni = reg_1.nss
      END IF

      DISPLAY "SE ABRIERON ",cont_cer CLIPPED," CUENTAS DE CERTIFICADOS" AT 18,2
   END FOREACH

   ---- asigando
   DECLARE sol_asi CURSOR FOR
   SELECT b.*
   FROM   uni_det_asignado b
   WHERE  b.estado = xaceptado
   FOR UPDATE

   LET vtipo = 1

   FOREACH sol_asi INTO reg_1.*
      LET tipo_proceso = reg_1.cve_operacion

      CALL apertura_cuenta(reg_1.*,vtipo)

      LET cont_asi = cont_asi + 1

      UPDATE uni_det_asignado
      SET    uni_det_asignado.estado = xaperturado
      WHERE CURRENT OF sol_asi

      DISPLAY "SE ABRIERON ",cont_asi CLIPPED," CUENTAS DE ASIGNADOS" AT 19,2

   END FOREACH

END FUNCTION
##########################################################################
FUNCTION apertura_cuenta(g_reg2,vtipo)

   DEFINE g_reg2  RECORD LIKE uni_det_traspaso.*
   DEFINE vtipo               SMALLINT
   DEFINE xnss                CHAR(11)
   DEFINE xnss_1              CHAR(11)
   DEFINE vfecha_transfer     DATE
   DEFINE vlote_trasp         SMALLINT
   DEFINE vporcentaje         SMALLINT
   DEFINE vsolicitud          SMALLINT
   DEFINE wc_tipo             CHAR(02)

   IF (g_reg2.cve_doc_proba IS NULL OR
       g_reg2.cve_doc_proba MATCHES " *") THEN
       LET g_reg2.cve_doc_proba = 6
   END IF

   IF (g_reg2.sexo IS NULL OR
       g_reg2.sexo MATCHES " *") THEN
       LET g_reg2.sexo = 1
   END IF

   SELECT "X"
   FROM   uni_unificador
   WHERE  folio   = g_reg2.folio
   AND    nss_uni = g_reg2.nss
   AND    estado  = vsolicitado
   GROUP BY 1

   IF STATUS <> NOTFOUND THEN
      DECLARE cta_1 CURSOR FOR
      SELECT a.nss_cta1, a.tipo_ent_cta1
      FROM   uni_unificado a
      WHERE  a.folio = g_reg2.folio
      AND    a.nss_uni = g_reg2.nss
      AND    a.cve_ent_cta1 = vcodigo_afore
      AND    a.estado IN (30,40)
      AND    a.tipo_ent_cta1 IN ("01", "59")   ---### 15 Dic 2009 proceso de fallecidos
      ORDER BY 2                               ---### 15 Dic 2009 proceso de fallecidos

      FOREACH cta_1 INTO xnss, wc_tipo
         EXIT FOREACH
      END FOREACH

      LET vsolicitud = 3
      LET vnumero = ""

      SELECT MAX(folio) + 1
      INTO   vnumero
      FROM   uni_folio

      IF vnumero IS NULL OR vnumero = 0  THEN
         LET vnumero = 1
      END IF

      INSERT INTO uni_folio
      VALUES(vnumero)

      CALL afilia_unificador(g_reg2.folio,
                             g_reg2.nss,
                             g_reg2.cve_afo_ced,
                             xnss,
                             vsolicitud,
                             vnumero,
                             g_reg2.fecha_certifica,
                             vtipo,
                             g_reg2.cve_operacion)
   ELSE
      LET vnumero    = ""
      LET xnss       = ""
      LET xnss_1     = ""
      LET vsolicitud = 4

      SELECT MAX(folio) + 1
      INTO   vnumero
      FROM   uni_folio_virtual

      IF vnumero IS NULL OR vnumero = 0 THEN
         LET vnumero = 1
      END IF

      INSERT INTO uni_folio_virtual
      VALUES(vnumero)

      LET hora = TIME

      IF g_reg2.nacionalidad IS NULL OR
         g_reg2.nacionalidad MATCHES " *" THEN

         IF g_reg2.ent_nac = 35 THEN
            LET g_reg2.nacionalidad = "EXT"
         ELSE
             LET g_reg2.nacionalidad = "MEX"
         END IF
      END IF

      SELECT a.nss_uni
      INTO   xnss
      FROM   uni_unificado a
      WHERE  a.folio = g_reg2.folio
      AND    a.nss_cta1 = g_reg2.nss

      SELECT "X"
      FROM   uni_unificador
      WHERE  folio    = g_reg2.folio
      AND    nss_uni = xnss
      AND    cve_ent_nss = vcodigo_afore
      AND    tipo_ent_nss = "01"
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         DECLARE ape_1 CURSOR FOR
         SELECT a.nss_cta1
         FROM   uni_unificado a
         WHERE  a.folio = g_reg2.folio
         AND    a.nss_uni = xnss
         AND    a.cve_ent_cta1 = vcodigo_afore
         AND    a.tipo_ent_cta1 = "01"

         FOREACH ape_1 INTO xnss_1
            LET xnss = xnss_1
            EXIT FOREACH
         END FOREACH

         ####    15 Dic 2009 Proceso de fallecidos
         IF   xnss  IS NULL   THEN
              DECLARE  d_selfal     CURSOR  FOR
               SELECT  nss, tipo_registro
                 FROM  uni_fallecido
                WHERE  folio    = g_reg2.folio
                  AND  consecutivo   IN  ( SELECT a.consecutivo
                                             FROM uni_fallecido a
                                            WHERE a.nss      = g_reg2.nss
                                              AND a.folio    = g_reg2.folio)
                  AND  tipo_registro IN ('01', '59' )
                  AND  cve_afore      = vcodigo_afore
                ORDER BY 2
             FOREACH  d_selfal    INTO xnss, wc_tipo
                EXIT FOREACH
             END FOREACH
         END IF
         ###
      END IF

      CALL afilia_unificado(g_reg2.folio,
                            g_reg2.nss,
                            g_reg2.cve_afo_ced,
                            xnss,
                            vsolicitud,
                            vnumero,
                            g_reg2.fecha_certifica,
                            vtipo,
                            g_reg2.cve_operacion)

   END IF

END FUNCTION
##########################################################################
FUNCTION afilia_unificador(folio_det,
                           nss_inf,
                           x_cve_afo_ced,
                           nss_cam,
                           xsolicitud,
                           xnumero,
                           xfecha,
                           xtipo,
                           xcve_operacion)

   DEFINE folio_det    INTEGER,
          nss_inf      CHAR(11),
          nss_cam      CHAR(11),
          xsolicitud   SMALLINT,
          xnumero      DECIMAL(10,0),
          xfecha       DATE,
          xtipo        SMALLINT

   DEFINE reg_10 RECORD LIKE afi_mae_afiliado.*
   DEFINE reg_11 RECORD LIKE afi_domicilio.*
   DEFINE reg_13 RECORD LIKE cta_ctr_cuenta.*
   DEFINE reg_14 RECORD LIKE afi_telefono.*
   DEFINE reg_15 RECORD LIKE afi_mae_benefici.*
   DEFINE reg_16 RECORD LIKE afi_mae_patron.*
   DEFINE reg_17 RECORD LIKE afi_mae_afiliado.*
   DEFINE reg_18 RECORD LIKE afi_mae_benefici.*
   DEFINE reg_19 RECORD LIKE afi_mae_patron.*
   DEFINE reg_20 RECORD LIKE afi_mae_afiliado.*

   DEFINE xmarca         SMALLINT,
          xrechazo       CHAR(03),
          x_tipo_regimen,
          x_estado,
          x_cve_afo_ced  SMALLINT

--- cuentas administradas
   DEFINE v_cod_proceso     SMALLINT,
          xcve_operacion    CHAR(2),
          v_tipo_trabajador SMALLINT
--- cuentas administradas

   DEFINE lr_cta RECORD LIKE cta_ctr_cuenta.*
   DEFINE ls_marca       SMALLINT
   DEFINE li_consecutivo LIKE ret_sol_issste_par.consecutivo

   SELECT "X"
   FROM   afi_solicitud
   WHERE  n_seguro = nss_inf
   AND    tipo_solicitud = 3
   GROUP BY 1

   IF STATUS = NOTFOUND THEN
      SELECT *
      INTO   reg_10.*
      FROM   afi_mae_afiliado
      WHERE  n_seguro = nss_cam

      IF reg_10.cod_esq_comision IS NULL THEN
      	 LET reg_10.cod_esq_comision = 0
      END IF

      LET reg_17.*                 = reg_10.*
      LET reg_17.n_folio           = xnumero
      LET reg_17.fentcons          = xfecha
      LET reg_17.frecafor          = xfecha
      LET reg_17.finicta           = xfecha
      LET reg_17.fecha_elaboracion = xfecha
      LET reg_17.fecha_envio       = xfecha
      LET reg_17.tipo_solicitud    = xsolicitud
      LET reg_17.n_seguro          = nss_inf
      LET reg_17.cod_afore_ced     = x_cve_afo_ced

      IF xtipo = 2 THEN
         LET reg_17.status_interno = 100
      ELSE
         LET reg_17.status_interno = 65
      END IF

      INSERT INTO afi_solicitud
      VALUES(reg_17.*)

      IF xtipo = 2 THEN
      	 SELECT "X"
      	 FROM   afi_mae_afiliado
      	 WHERE  n_seguro = reg_17.n_seguro

      	 IF SQLCA.SQLCODE = 0 THEN
      	 	  SELECT *
      	 	  INTO   reg_20.*
      	 	  FROM   afi_mae_afiliado
      	    WHERE  n_seguro = reg_17.n_seguro

      	    INSERT INTO afi_his_afiliado VALUES (reg_20.*)
            DELETE
            FROM   cta_afi_nip
            WHERE  nss = reg_20.n_seguro

            DELETE
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_20.n_seguro

            SELECT "X"
            FROM   cta_ctr_cuenta
            WHERE  cta_ctr_cuenta.nss = reg_20.n_seguro
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
                SELECT b.*
                INTO   lr_cta.*
                FROM   cta_ctr_cuenta b
                WHERE  b.nss = reg_20.n_seguro

                INSERT INTO cta_his_cuenta VALUES (lr_cta.*)

                DELETE
                FROM   cta_ctr_cuenta
                WHERE  nss = reg_20.n_seguro
            END IF

            IF reg_20.tipo_solicitud = 5 THEN
                UPDATE afi_det_asignado
                SET    fecha_afiliacion = reg_17.fentcons,
                       estado_asignado  = 100
                WHERE  n_seguro       = reg_20.n_seguro
                AND    n_folio        = reg_20.n_folio
                AND    tipo_solicitud = reg_20.tipo_solicitud
            END IF

               DECLARE cur_desmarca2 CURSOR FOR
               SELECT mc.marca_cod,
                      mc.correlativo
                 FROM cta_act_marca mc
                WHERE mc.nss = reg_20.n_seguro
                  AND mc.marca_cod = 150

               FOREACH cur_desmarca2 INTO ls_marca, li_consecutivo
                     CALL f_desmarca_cuenta (ls_marca         ,     --ls_marca_ent
                                             reg_20.n_seguro,     --lc_nss
                                             li_consecutivo   ,     --li_consecutivo
                                             usuario              --lc_usuario
                                             )
               END FOREACH
      	 END IF

         INSERT INTO afi_mae_afiliado VALUES(reg_17.*)

--- cuentas administradas
{
         SELECT cod_proceso
         INTO   v_cod_proceso
         FROM   tab_tipo_proceso
         WHERE  tipo_proceso = xsolicitud
         AND    cod_operacion = xcve_operacion

         LET v_tipo_trabajador = 0

         DECLARE cur_cta_administra CURSOR FOR cta_administra

         OPEN  cur_cta_administra USING nss_inf, #nss
                             xnumero,            #folio
                             xsolicitud,         #tipo_solicitud
                             reg_10.n_unico,     #CURP
                             xfecha,             #fecha_ingreso
                             v_cod_proceso,      #cod_proceso
                             v_tipo_trabajador,  #fecha_causa
                             usuario             #usuario

         FETCH cur_cta_administra INTO xrechazo
         CLOSE cur_cta_administra}
--- cuentas administradas

         SELECT *
         INTO   reg_13.*
         FROM   cta_ctr_cuenta
         WHERE  nss = nss_cam

         LET reg_13.nss      = nss_inf
         LET reg_13.usuario  = usuario

         SELECT "X"
         FROM   cta_ctr_cuenta
         WHERE  nss = reg_13.nss
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
         	  INSERT INTO cta_ctr_cuenta
            VALUES(reg_13.*)
         END IF

         LET  xrechazo = 0

         DECLARE cur_mar CURSOR FOR marcaje

         OPEN  cur_mar USING nss_inf,          #pnss
                             vextra_uni,       #marca_entra
                             vcorrelativo,     #correlativo
                             vestado_causa,    #estado_marca
                             vrechazo_cod,     #codigo_rechazo
                             vmarca_causa,     #marca_causa
                             vfecha_causa,     #fecha_causa
                             usuario           #usuario

         FETCH cur_mar INTO xmarca,
                            xrechazo
         CLOSE cur_mar
      END IF
   ELSE
      SELECT *
      INTO   reg_10.*
      FROM   afi_mae_afiliado
      WHERE  n_seguro = nss_cam

      SELECT n_folio
      INTO   xnumero
      FROM   afi_solicitud
      WHERE  n_seguro = nss_inf
      AND    tipo_solicitud = 3
   END IF
---
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
   {SELECT "X"
   FROM   cta_regimen
   WHERE  nss = nss_inf
   GROUP BY 1

   IF STATUS = 100 THEN
      SELECT "X"
      FROM   afi_regimen
      WHERE  nss = nss_inf
      GROUP BY 1

      IF STATUS = 100 THEN
         SELECT UNIQUE a.tipo_regimen,
                b.estado
         INTO   x_tipo_regimen,
                x_estado
         FROM   cta_nss_regimen a,tab_regimen_inv b
         WHERE  a.nss = nss_cam
         AND    a.tipo_regimen = b.tipo_regimen
         AND    a.siefore_rcv = b.codigo_siefore

         EXECUTE genera_regimen USING nss_inf,
                                      x_tipo_regimen,
                                      x_estado
      END IF
   END IF}
---
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
#--------beneficiarios

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

      INSERT INTO afi_beneficiario VALUES(reg_15.*)

      IF xtipo = 2 THEN
         INSERT INTO afi_mae_benefici VALUES(reg_15.*)
      END IF

   END FOREACH

#--------patrones

   SELECT "X"
   FROM   afi_mae_patron
   WHERE  n_seguro       = nss_cam
   AND    n_folio        = reg_10.n_folio
   AND    tipo_solicitud = reg_10.tipo_solicitud
   GROUP BY 1

   IF STATUS <> NOTFOUND THEN
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

         INSERT INTO afi_patron VALUES(reg_16.*)

         IF reg_16.reg_fed_contrib IS NOT NULL THEN
            IF xtipo = 2 THEN
               INSERT INTO afi_mae_patron VALUES(reg_16.*)
            END IF
         END IF
      END FOREACH
   END IF

   INSERT INTO uni_apertura_cuenta VALUES(folio_det,
                                          "UNIFICADOR",
                                          tipo_proceso,
                                          nss_inf,
                                          xnumero,
                                          xsolicitud)

END FUNCTION
##########################################################################
FUNCTION afilia_unificado(folio_det,
                          nss_inf,
                          x_cve_afo_ced,
                          nss_cam,
                          xsolicitud,
                          xnumero,
                          xfecha,
                          xtipo,
                          xcve_operacion)

   DEFINE folio_det    INTEGER,
          nss_inf      CHAR(11),
          nss_cam      CHAR(11),
          xsolicitud   SMALLINT,
          xnumero      DECIMAL(10,0),
          xfecha       DATE,
          xtipo        SMALLINT

   DEFINE reg_10 RECORD LIKE afi_mae_afiliado.*
   DEFINE reg_11 RECORD LIKE afi_domicilio.*
   DEFINE reg_13 RECORD LIKE cta_ctr_cuenta.*
   DEFINE reg_14 RECORD LIKE afi_telefono.*
   DEFINE reg_15 RECORD LIKE afi_mae_benefici.*
   DEFINE reg_16 RECORD LIKE afi_mae_patron.*
   DEFINE reg_17 RECORD LIKE afi_mae_afiliado.*
   DEFINE reg_18 RECORD LIKE afi_mae_benefici.*
   DEFINE reg_19 RECORD LIKE afi_mae_patron.*

   DEFINE xmarca         SMALLINT,
          xrechazo       CHAR(03),
          opc            CHAR(1),
          x_tipo_regimen SMALLINT,
          x_estado       SMALLINT,
          x_cve_afo_ced  SMALLINT

---cuentas administradas
   DEFINE v_cod_proceso     SMALLINT,
          xcve_operacion    CHAR(2),
          v_tipo_trabajador SMALLINT
---cuentas administradas

   SELECT "X"
   FROM   afi_solicitud
   WHERE  n_seguro = nss_inf
   AND    tipo_solicitud = 4
   GROUP BY 1

   IF STATUS = NOTFOUND THEN
      SELECT *
      INTO   reg_10.*
      FROM   afi_mae_afiliado
      WHERE  n_seguro = nss_cam

      IF reg_10.cod_esq_comision IS NULL THEN
      	 LET reg_10.cod_esq_comision = 0
      END IF

      LET reg_17.*                 = reg_10.*
      LET reg_17.n_folio           = xnumero
      LET reg_17.fentcons          = xfecha
      LET reg_17.frecafor          = xfecha
      LET reg_17.finicta           = xfecha
      LET reg_17.fecha_elaboracion = xfecha
      LET reg_17.fecha_envio       = xfecha
      LET reg_17.tipo_solicitud    = xsolicitud
      LET reg_17.n_seguro          = nss_inf
      LET reg_17.cod_afore_ced     = x_cve_afo_ced

      IF xtipo = 2 THEN
         LET reg_17.status_interno = 100
      ELSE
         LET reg_17.status_interno = 65
      END IF

      INSERT INTO afi_solicitud VALUES(reg_17.*)

      IF xtipo = 2 THEN
         INSERT INTO afi_mae_afiliado VALUES(reg_17.*)

--- cuentas administradas
{
         SELECT a.cod_proceso
         INTO   v_cod_proceso
         FROM   tab_tipo_proceso a
         WHERE  a.tipo_proceso = xsolicitud
         AND    a.cod_operacion = xcve_operacion

         LET v_tipo_trabajador = 0

         DECLARE cur_cta_administra_1 CURSOR FOR cta_administra

         OPEN  cur_cta_administra_1 USING nss_inf, #nss
                             xnumero,            #folio
                             xsolicitud,         #tipo_solicitud
                             reg_10.n_unico,     #CURP
                             xfecha,             #fecha_ingreso
                             v_cod_proceso,      #cod_proceso
                             v_tipo_trabajador,  #fecha_causa
                             usuario             #usuario

         FETCH cur_cta_administra_1 INTO xrechazo
         CLOSE cur_cta_administra_1}
--- cuentas administradas

         SELECT *
         INTO   reg_13.*
         FROM   cta_ctr_cuenta
         WHERE  nss = nss_cam

         LET reg_13.nss      = nss_inf
         LET reg_13.usuario  = usuario

         INSERT INTO cta_ctr_cuenta VALUES(reg_13.*)

         LET  xrechazo = 0

         DECLARE cur_mar1 CURSOR FOR marcaje

         OPEN  cur_mar1 USING
               nss_inf,          #pnss
               vextra_cta1,      #marca_entra
               vcorrelativo,     #correlativo
               vestado_causa,    #estado_marca
               vrechazo_cod,     #codigo_rechazo
               vmarca_causa,     #marca_causa
               vfecha_causa,     #fecha_causa
               usuario           #usuario

           FETCH cur_mar1 INTO xmarca,
                               xrechazo
         CLOSE cur_mar1
      END IF
   ELSE
      SELECT *
      INTO   reg_10.*
      FROM   afi_mae_afiliado
      WHERE  n_seguro = nss_cam

      SELECT n_folio
      INTO   xnumero
      FROM   afi_solicitud
      WHERE  n_seguro = nss_inf
      AND    tipo_solicitud = 4
   END IF
--
   LET v_tipo_proc  =  1
   LET v_tipo_trasp = 11
   LET v_medio      = 10

   DECLARE curs12 CURSOR FOR stmt1
	 OPEN  curs12 USING nss_inf,
	                   HOY
	 FETCH curs12 INTO v_existe,
	                  v_edad,
	                  v_criterio,
	                  v_ind_edad,
	                  v_curp,
	                  v_rfc,
	                  v_fena
	 CLOSE curs12

   DECLARE cur_fn_regimen2 CURSOR FOR stmt2
   OPEN cur_fn_regimen2 USING nss_inf,
                             v_ind_edad,
                             v_ind_edad,
                             v_tipo_proc, -- 1
                             v_tipo_trasp,--11
                             v_medio      --10
   FETCH cur_fn_regimen2 INTO v_existe,
                             v_ind_edad,
                             v_rechazo,
                             v_folioatencion
   CLOSE cur_fn_regimen2

   IF v_rechazo <> 0 THEN
	    INSERT INTO safre_tmp:rch_apertura VALUES (nss_inf,
	                                               v_rechazo)
   END IF

   {SELECT "X"
   FROM   cta_regimen
   WHERE  nss = nss_inf
   GROUP BY 1

   IF STATUS = 100 THEN
      SELECT "X"
      FROM   afi_regimen
      WHERE  nss = nss_inf
      GROUP BY 1

      IF STATUS = 100 THEN
         SELECT UNIQUE a.tipo_regimen,
                b.estado
         INTO   x_tipo_regimen,
                x_estado
         FROM   cta_nss_regimen a,tab_regimen_inv b
         WHERE  a.nss = nss_cam
         AND    a.tipo_regimen = b.tipo_regimen
         AND    a.siefore_rcv = b.codigo_siefore

         EXECUTE genera_regimen USING nss_inf,
                                      x_tipo_regimen,
                                      x_estado
      END IF
   END IF}
---
   DECLARE cur_dom1 CURSOR FOR
   SELECT *
   FROM   afi_domicilio
   WHERE  nss            = nss_cam
   AND    n_folio        = reg_10.n_folio
   AND    tipo_solicitud = reg_10.tipo_solicitud

   FOREACH cur_dom1 INTO reg_11.*

      LET reg_11.nss             = nss_inf
      LET reg_11.tipo_solicitud  = xsolicitud
      LET reg_11.n_folio         = xnumero
      LET reg_11.usuario         = usuario

      INSERT INTO afi_domicilio VALUES(reg_11.*)

   END FOREACH
#--------telefono
   DECLARE cur_tel1 CURSOR FOR
   SELECT *
   FROM   afi_telefono
   WHERE  nss            = nss_cam
   AND    n_folio        = reg_10.n_folio
   AND    tipo_solicitud = reg_10.tipo_solicitud

   FOREACH cur_tel1 INTO reg_14.*

      LET reg_14.nss             = nss_inf
      LET reg_14.tipo_solicitud  = xsolicitud
      LET reg_14.n_folio         = xnumero
      LET reg_14.usuario         = usuario

      INSERT INTO afi_telefono VALUES(reg_14.*)

   END FOREACH
#--------beneficiarios
   DECLARE cur_ben1 CURSOR FOR
   SELECT *
   FROM   afi_mae_benefici
   WHERE  n_seguro       = nss_cam
   AND    n_folio        = reg_10.n_folio
   AND    tipo_solicitud = reg_10.tipo_solicitud

   FOREACH cur_ben1 INTO reg_15.*

      LET reg_15.n_seguro        = nss_inf
      LET reg_15.tipo_solicitud  = xsolicitud
      LET reg_15.n_folio         = xnumero

      INSERT INTO afi_beneficiario VALUES(reg_15.*)

      IF xtipo = 2 THEN
         INSERT INTO afi_mae_benefici VALUES(reg_15.*)
      END IF

   END FOREACH
#--------patrones
   SELECT "X"
   FROM   afi_mae_patron
   WHERE  n_seguro       = nss_cam
   AND    n_folio        = reg_10.n_folio
   AND    tipo_solicitud = reg_10.tipo_solicitud
   GROUP BY 1

   IF STATUS <> NOTFOUND THEN
      DECLARE cur_pat1 CURSOR FOR
      SELECT *
      FROM   afi_mae_patron
      WHERE  n_seguro       = nss_cam
      AND    n_folio        = reg_10.n_folio
      AND    tipo_solicitud = reg_10.tipo_solicitud

      FOREACH cur_pat1 INTO reg_16.*

         LET reg_16.n_seguro        = nss_inf
         LET reg_16.tipo_solicitud  = xsolicitud
         LET reg_16.n_folio         = xnumero

         INSERT INTO afi_patron VALUES(reg_16.*)

         IF reg_16.reg_fed_contrib IS NOT NULL THEN
            IF xtipo = 2 THEN
               INSERT INTO afi_mae_patron VALUES(reg_16.*)
            END IF
         END IF
      END FOREACH
   END IF

   INSERT INTO uni_apertura_cuenta
   VALUES(folio_det,
          "UNIFICADO",
          tipo_proceso,
          nss_inf,
          xnumero,
          xsolicitud)

END FUNCTION
##########################################################################
FUNCTION actualiza_historico()

   UPDATE  uni_cza_traspaso
   SET     estado = vtraspaso
   WHERE   folio  = vfolio

   UPDATE  uni_cza_certifica
   SET     estado = vtraspaso
   WHERE   folio  = vfolio

   UPDATE  uni_cza_asignado
   SET     estado = vtraspaso
   WHERE   folio  = vfolio

   UPDATE  uni_sum_asignado
   SET     estado = vtraspaso
   WHERE   folio  = vfolio

   UPDATE  uni_sum_traspaso
   SET     estado = vtraspaso
   WHERE   folio  = vfolio

   UPDATE  uni_sum_certifica
   SET     estado = vtraspaso
   WHERE   folio  = vfolio

END FUNCTION
###################################################
FUNCTION detalle_proceso()

   DEFINE aper_ctas  ARRAY[900] OF RECORD
          folio            INTEGER,
          tipo_nss         CHAR(15),
          tipo_operacion   SMALLINT,
          n_seguro         CHAR(11),
          n_folio          INTEGER,
          tipo_solicitud   SMALLINT
   END RECORD

   DEFINE tmp_aper ARRAY[900] OF RECORD
          folio            INTEGER,
          tipo_nss         CHAR(15),
          tipo_operacion   SMALLINT,
          n_seguro         CHAR(11),
          n_folio          INTEGER,
          nombres          CHAR(35),
          tipo_solicitud   SMALLINT
   END RECORD

   DEFINE posx         SMALLINT

   OPEN WINDOW ventana_2 AT 2,2 WITH FORM "UNIC0152" ATTRIBUTE(BORDER)

   DISPLAY " <Ctrl-C> Salir" AT 2,1
   DISPLAY " Folio   Tipo    Tipo  N.S.S.    Folio               Nombre             Tipo   " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "        N.S.S.   Proc.           Afil.                                  Sol.   " AT 4,1 ATTRIBUTE(REVERSE)

   DECLARE aper_cta CURSOR FOR
   SELECT *
   FROM   uni_apertura_cuenta

   LET posx = 1

   FOREACH aper_cta INTO aper_ctas[posx].*

      LET tmp_aper[posx].folio          = aper_ctas[posx].folio
      LET tmp_aper[posx].tipo_nss       = aper_ctas[posx].tipo_nss
      LET tmp_aper[posx].tipo_operacion = aper_ctas[posx].tipo_operacion
      LET tmp_aper[posx].n_seguro       = aper_ctas[posx].n_seguro
      LET tmp_aper[posx].n_folio        = aper_ctas[posx].n_folio
      LET tmp_aper[posx].tipo_solicitud = aper_ctas[posx].tipo_solicitud

      DECLARE cur_nom CURSOR FOR query_nom

      OPEN  cur_nom USING aper_ctas[posx].n_seguro,
                          aper_ctas[posx].n_seguro
      FETCH cur_nom INTO nombre_completo
      CLOSE cur_nom

      LET tmp_aper[posx].nombres = nombre_completo CLIPPED

      LET posx = posx + 1
   END FOREACH

   LET posx = posx - 1

   IF posx > 0 THEN
      CALL SET_COUNT(posx)

      DISPLAY ARRAY tmp_aper TO scr_1.*
         ON KEY(INTERRUPT)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "REGISTROS ... VACIO"
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana_2
END FUNCTION

FUNCTION f_desmarca_cuenta(ls_marca_ent, lc_nss, li_consecutivo, lc_usuario)
   DEFINE ls_marca_ent   SMALLINT
   DEFINE lc_nss         CHAR(11)
   DEFINE li_consecutivo LIKE ret_sol_issste_par.consecutivo
   DEFINE lc_usuario     LIKE ret_sol_issste_par.usuario_captura

   DEFINE ls_estado_marca  ,
          ls_codigo_rechazo,
          ls_marca_causa   SMALLINT

   LET ls_estado_marca   = 0
   LET ls_codigo_rechazo = 0
   LET ls_marca_causa    = 0

   EXECUTE eje_desmarca USING lc_nss                    ,
                              ls_marca_ent              ,
                              li_consecutivo            ,
                              ls_estado_marca           ,
                              ls_marca_causa            ,
                              lc_usuario
END FUNCTION