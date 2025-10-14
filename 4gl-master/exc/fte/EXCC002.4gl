###########################################################################
#Proyecto               => Sistema de Afores. (MEXICO)                    #
#Propietario            => E.F.P                                          #
#Programa     EXCC002   => Reporte consulta de  pagos en exceso           #
#Fecha                  => 20 de Marzo de 2002                            #
#Modificado             => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Fecha modifica         => 12 de junio de 2002.                           #
#Fecha modifica         => 11 de septiembre de 2002.                      #
#Modificado             => OMAR SANDOVAL BADILLO.                         #
#Fecha modifica         => 11 de febrero  2005.                           #
#Modificado             => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Fecha modifica         => 11 de abril  2005.                             #
#Modificado             => CESAR DAVID CHAVEZ MARTINEZ.                   #
#Fecha modifica         => 24 de marzo  2008.                             #
#Modulo                 => EXC.                                           #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE x_folio            INTEGER,
          hoy                DATE,
          usuario            CHAR(8),
          vruta_listado      CHAR(40),
          nss                CHAR(8),
          g_lista            CHAR(100),
          g_impre            CHAR(100),
          vtipo_rechazo      SMALLINT,
          vtipo_rechazo2     SMALLINT,
          opc                CHAR(1)

   DEFINE l_reg RECORD
          folio              INTEGER,
          nss                CHAR(11),
          fecha_pago         DATE,
          fecha_pago_sua     CHAR(6),
          monto_ret          DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_ces_vej_pat  DECIMAL(16,6),
          monto_ces_vej_tra  DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_par_viv      DECIMAL(18,6),
          tipo_diagnostico   CHAR(3),
          monto_retiro       DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE l_reg2  ARRAY[900] OF RECORD
          folio              INTEGER,
          nss                CHAR(11),
          monto_retiro       DECIMAL (16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6),
          tipo_diagnostico   CHAR(3)
   END RECORD

   DEFINE l_reg3 RECORD
          folio              INTEGER,
          nss                CHAR(11),
          subcuenta          SMALLINT,
          tipo_movimiento    INTEGER,
          siefore            SMALLINT,
          fecha_valor        DATE,
          fecha_conversion   DATE,
          monto_en_pesos     DECIMAL(16,6),
          monto_en_acciones  DECIMAL(16,6)
   END RECORD

   DEFINE l_reg5 RECORD
          nss                CHAR(11),
          consec_reg_lote    SMALLINT,
          fecha_pago         DATE,
          folio_pago_sua     CHAR(6),
          reg_patronal_imss  CHAR(11)
   END RECORD

   DEFINE l_reg4  ARRAY[900] OF RECORD
          folio              INTEGER,
          nss                CHAR(11),
          subcuenta          SMALLINT,
          tipo_movimiento    INTEGER,
          siefore            SMALLINT,
          fecha_valor        DATE,
          fecha_conversion   DATE,
          monto_en_pesos     DECIMAL(16,6),
          monto_en_acciones  DECIMAL(16,6)
   END RECORD

   DEFINE l_reg6  ARRAY[900] OF RECORD
          nss                CHAR(11),
          subcuenta          SMALLINT,
          siefore            SMALLINT,
          monto_en_acciones  DECIMAL(16,6),
          monto_pesos_act    DECIMAL(16,6),
          monto_pesos_his    DECIMAL(16,6),
          monto_comision     DECIMAL(16,6),
          monto_plusvalia    DECIMAL(16,6)
   END RECORD

   DEFINE pos                SMALLINT,
          i                  SMALLINT,
          cla_where          CHAR(600),
          sel_where          CHAR(600)

   DEFINE x_folio_his        INTEGER,
          monto_ret_his      DECIMAL(16,6),
          monto_cv_his       DECIMAL(16,6),
          monto_act_ret_his  DECIMAL(16,6),
          monto_act_cv_his   DECIMAL(16,6)

   DEFINE x_monto_comi_ret      DECIMAL(16,6),
          x_monto_comi_ces_vej  DECIMAL(16,6),
          x_monto_plus_ret      DECIMAL(16,6),
          x_monto_plus_ces_vej  DECIMAL(16,6),
          x_monto_vivienda      DECIMAL(16,6),
          x_monto_plus_vivienda DECIMAL(16,6)

   DEFINE monto_historico_ret   DECIMAL(16,6),
          monto_historico_cv    DECIMAL(16,6)

   DEFINE l_reg7 RECORD
          subcuenta             SMALLINT,
          siefore               SMALLINT,
          tipo_movimiento       SMALLINT,
          monto_en_pesos        DECIMAL(16,6),
          monto_en_acciones     DECIMAL(16,6)
   END RECORD

   DEFINE xx_precio_accion      DECIMAL(16,6)

END GLOBALS
#####################################################################
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   LET hoy = TODAY

   OPEN WINDOW v1 AT 3,3 WITH FORM "EXCC0021" ATTRIBUTE (BORDER)
   DISPLAY " EXCC002               CONSULTA DE PAGOS EN EXCESO                                " AT 3,1 ATTRIBUTE (REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE (REVERSE)

   CALL inicio()

   MENU "CONSULTAS"
      COMMAND "Aceptados" "Consulta de pagos en exceso aceptados"
         CALL proceso(1)

      COMMAND "Rechazados" "Consulta de pagos en exceso rechazados"
         CALL proceso(2)

      COMMAND "Pendientes" "Consulta de pagos en exceso pendientes"
         CALL proceso(3)

      COMMAND KEY (O) "prOvision" "Consulta de provosion de pagos en exceso"
         CALL proceso_provision(4)

      COMMAND KEY (E) "prE-liquidacion" "Consulta de pre-liquidacion de pagos en exceso"
         CALL proceso_preliq(6)

      COMMAND "Liquidados" "Consulta de liquidacion de pagos en exceso"
         CALL proceso_cuenta(5)

      COMMAND KEY (G) "reGistro 08" "Consulta del montos en lo(s) registro(s) 08"
         CALL deposito_archivo()

      COMMAND "Salir" "Salir de menu de reportes"
         EXIT MENU
   END MENU

   CLOSE WINDOW v1

END MAIN
#####################################################################
FUNCTION inicio()

   SELECT USER,
          ruta_listados
   INTO   usuario,
          vruta_listado
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "exc"

END FUNCTION
#####################################################################
FUNCTION proceso(vtipo_rechazo)

   DEFINE vtipo_rechazo      SMALLINT,
          x_resul_operacion  CHAR(2)

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "EXCC0022" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrol-p) Impresion            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)

      CASE vtipo_rechazo
         WHEN 1
            LET x_resul_operacion = "01"
            DISPLAY "                         PAGOS EN EXCESO ACEPTADOS                           " AT 3,1 ATTRIBUTE(REVERSE)
         WHEN 2
            LET x_resul_operacion = "02"
            DISPLAY "                          PAGOS EN EXCESO RECHAZADOS                           " AT 3,1 ATTRIBUTE(REVERSE)
         WHEN 3
            LET x_resul_operacion = "03"
            DISPLAY  "                         PAGOS EN EXCESO PENDIENTES                           " AT 3,1 ATTRIBUTE(REVERSE)
      END CASE

      LET int_flag = FALSE

      CONSTRUCT cla_where ON folio,
                             nss
                        FROM folio,
                             nss

         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = " SELECT folio,",
                             " nss,",
                             " fecha_pago,",
                             " folio_pago_sua,",
                             " monto_ret,",
                             " monto_act_ret,",
                             " monto_ces_vej_pat,",
                             " monto_ces_vej_tra,",
                             " monto_act_ces_vej,",
                             #" monto_aport_pat,",
                             " monto_par_viv,",
                             " tipo_diagnostico,",
                             " SUM(monto_ret + monto_act_ret) monto_retiro,",
                             " SUM(monto_ces_vej_pat + monto_ces_vej_tra + monto_act_ces_vej) monto_ces_vej ",
                      " FROM   exc_det_exceso ",
                      " WHERE  result_operacion = ",x_resul_operacion CLIPPED,
                      " AND ",cla_where CLIPPED,
                      " GROUP BY 1,2,3,4,5,6,7,8,9,10,11",
                      " ORDER BY 1,2 "

      PREPARE exe_sel FROM sel_where

      DECLARE cursor_1 CURSOR FOR exe_sel

      LET pos = 1

      FOREACH cursor_1 INTO l_reg.*

         LET l_reg2[pos].nss              = l_reg.nss
         LET l_reg2[pos].folio            = l_reg.folio
         LET l_reg2[pos].monto_retiro     = l_reg.monto_retiro
         LET l_reg2[pos].monto_ces_vej    = l_reg.monto_ces_vej
         LET l_reg2[pos].monto_aport_pat  = l_reg.monto_par_viv
         LET l_reg2[pos].tipo_diagnostico = l_reg.tipo_diagnostico

         LET pos = pos +1
      END FOREACH

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_reg2 TO scr_1.*
            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."

               LET vtipo_rechazo2 = vtipo_rechazo

               CALL listado_pendientes(pos,vtipo_rechazo2)

            ON KEY (INTERRUPT)
               EXIT DISPLAY

            ON KEY (control-c)
               EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "Registro inexistente ..."
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF
   END IF

   CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION proceso_provision(vtipo_rechazo)

   DEFINE vtipo_rechazo      SMALLINT,
          x_fecha_pago       CHAR(8),
          vperiodo_pago      CHAR(6),
          vclave_ent_recep   CHAR(3)

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_3 AT 6,3 WITH FORM "EXCC0024" ATTRIBUTE (BORDER)
      DISPLAY " (Enter) Consulta              (Ctrol-p) Impresion            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                           PAGOS EN EXCESO PROVISION                            " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE
      LET x_folio = NULL

      INPUT BY NAME x_folio WITHOUT DEFAULTS

         AFTER FIELD x_folio
            IF x_folio IS NULL THEN
               ERROR "Este campo no puede ser nulo"
               NEXT FIELD x_folio
            END IF

            SELECT "X"
            FROM   dis_ctrl_proceso
            WHERE  folio = x_folio
            AND    proceso_cod = "EXC"
            AND    etapa_cod = 1

            IF SQLCA.SQLCODE <> 0 THEN
               PROMPT "FOLIO NO EXISTE,VERIFICAR POR FAVOR" ATTRIBUTE (REVERSE)
                       FOR opc ATTRIBUTE (REVERSE)
               NEXT FIELD x_folio
            END IF
            EXIT INPUT

         ON KEY(ESC)
            IF x_folio IS NULL THEN
               ERROR "Este campo no puede ser nulo"
               NEXT FIELD x_folio
            END IF

            SELECT "X"
            FROM   dis_ctrl_proceso
            WHERE  folio = x_folio
            AND    proceso_cod = "EXC"
            AND    etapa_cod = 1

            IF SQLCA.SQLCODE <> 0 THEN
               PROMPT "FOLIO NO EXISTE,VERIFICAR POR FAVOR" ATTRIBUTE (REVERSE)
                       FOR opc ATTRIBUTE (REVERSE)
               NEXT FIELD x_folio
            END IF
            EXIT INPUT

         ON KEY(INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT INPUT
      END INPUT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_3
         RETURN
      END IF

      ERROR "PROCESANDO INFORMACION"

      DECLARE cursor_his CURSOR FOR
      SELECT a.nss,
             a.consec_reg_lote,
             a.fecha_pago,
             a.folio_pago_sua,
             a.reg_patronal_imss,
             a.periodo_pago,
             a.clave_ent_recep
      FROM   exc_det_exceso a
      WHERE  a.folio = x_folio
      AND    a.result_operacion = "01"
      GROUP BY 1,2,3,4,5,6,7
      ORDER BY 1,2

      LET pos = 1

      FOREACH cursor_his INTO l_reg5.*,
                              vperiodo_pago,
                              vclave_ent_recep

         LET x_fecha_pago = l_reg5.fecha_pago USING "YYYYMMDD"

         DECLARE cursor_pro CURSOR FOR
         SELECT  b.subcuenta,
                 b.siefore,
                 b.tipo_movimiento,
                 b.monto_en_pesos,
                 b.monto_en_acciones
         FROM    dis_provision b
         WHERE   b.folio            = x_folio
         AND     b.nss              = l_reg5.nss
         AND     b.consecutivo_lote = l_reg5.consec_reg_lote
         GROUP BY 1,2,3,4,5
         ORDER BY 1,2

         LET i = 1

         FOREACH cursor_pro INTO l_reg7.*

            LET monto_act_ret_his = 0

            IF l_reg7.subcuenta = 1 THEN

               SELECT a.folio,
                      SUM((a.impt_ret+a.impt_act_rec_ret)/100)
               INTO   x_folio_his,
                      monto_ret_his
               FROM   dis_det_aporte a
               WHERE  a.n_seguro          = l_reg5.nss
               AND    a.periodo_pago      = vperiodo_pago
               AND    a.fech_pago         = x_fecha_pago
               AND    a.folio_pago_sua    = l_reg5.folio_pago_sua
               AND    a.reg_patronal_imss = l_reg5.reg_patronal_imss
               AND    a.cve_ent_receptora = vclave_ent_recep
               AND    a.folio NOT IN(SELECT folio
                                   FROM   safre_tmp:tmp_folios_pgo13)
               AND    a.result_operacion[2] = "1"
               GROUP BY 1

               IF monto_ret_his IS NULL THEN
                  LET monto_ret_his = 0
               END IF

               SELECT SUM((b.impt_int_ret+b.impt_int_act_rr)/100)
               INTO   monto_act_ret_his
               FROM   dis_det_interes b
               WHERE  b.n_seguro          = l_reg5.nss
               AND    b.fech_pago         = x_fecha_pago
               AND    b.folio_pago_sua    = l_reg5.folio_pago_sua
               AND    b.reg_patronal_imss = l_reg5.reg_patronal_imss

               IF monto_act_ret_his IS NULL THEN
                  LET monto_act_ret_his = 0
               END IF

               LET monto_historico_ret = monto_ret_his + monto_act_ret_his

               SELECT c.monto_comi_ret
               INTO   x_monto_comi_ret
               FROM   exc_exceso_comis c
               WHERE  c.folio           = x_folio
               AND    c.nss             = l_reg5.nss
               AND    c.consec_reg_lote = l_reg5.consec_reg_lote

               SELECT d.monto_plus_ret
               INTO   x_monto_plus_ret
               FROM   exc_exceso_plu_min d
               WHERE  d.folio           = x_folio
               AND    d.nss             = l_reg5.nss
               AND    d.consec_reg_lote = l_reg5.consec_reg_lote

               LET l_reg6[pos].nss               = l_reg5.nss
               LET l_reg6[pos].subcuenta         = l_reg7.subcuenta
               LET l_reg6[pos].siefore           = l_reg7.siefore
               LET l_reg6[pos].monto_en_acciones = l_reg7.monto_en_acciones
               LET l_reg6[pos].monto_pesos_act   = l_reg7.monto_en_pesos
               LET l_reg6[pos].monto_pesos_his   = monto_historico_ret
               LET l_reg6[pos].monto_comision    = x_monto_comi_ret
               LET l_reg6[pos].monto_plusvalia   = x_monto_plus_ret
            END IF

            IF l_reg7.subcuenta = 2 THEN
               SELECT a.folio,
                      SUM((a.impt_ces_vej+a.impt_act_r_ces_vej)/100)
               INTO   x_folio_his,
                      monto_cv_his
               FROM   dis_det_aporte a
               WHERE  a.n_seguro          = l_reg5.nss
               AND    a.periodo_pago      = vperiodo_pago
               AND    a.fech_pago         = x_fecha_pago
               AND    a.folio_pago_sua    = l_reg5.folio_pago_sua
               AND    a.reg_patronal_imss = l_reg5.reg_patronal_imss
               AND    a.cve_ent_receptora = vclave_ent_recep
               AND    a.folio NOT IN(SELECT folio
                                   FROM   safre_tmp:tmp_folios_pgo13)
               AND    a.result_operacion[2] = "1"
               GROUP BY 1

               IF monto_cv_his IS NULL THEN
                  LET monto_cv_his = 0
               END IF

               SELECT SUM((b.impt_int_ces_vej+b.impt_int_act_rcv)/100)
               INTO   monto_act_cv_his
               FROM   dis_det_interes b
               WHERE  b.n_seguro          = l_reg5.nss
               AND    b.fech_pago         = x_fecha_pago
               AND    b.folio_pago_sua    = l_reg5.folio_pago_sua
               AND    b.reg_patronal_imss = l_reg5.reg_patronal_imss

               IF monto_act_cv_his IS NULL THEN
                  LET monto_act_cv_his = 0
               END IF

               LET monto_historico_cv = monto_cv_his + monto_act_cv_his

               SELECT c.monto_comi_ces_vej
               INTO   x_monto_comi_ces_vej
               FROM   exc_exceso_comis c
               WHERE  c.folio           = x_folio
               AND    c.nss             = l_reg5.nss
               AND    c.consec_reg_lote = l_reg5.consec_reg_lote

               SELECT d.monto_plus_ces_vej
               INTO   x_monto_plus_ces_vej
               FROM   exc_exceso_plu_min d
               WHERE  d.folio           = x_folio
               AND    d.nss             = l_reg5.nss
               AND    d.consec_reg_lote = l_reg5.consec_reg_lote

               LET l_reg6[pos].nss               = l_reg5.nss
               LET l_reg6[pos].subcuenta         = l_reg7.subcuenta
               LET l_reg6[pos].siefore           = l_reg7.siefore
               LET l_reg6[pos].monto_en_acciones = l_reg7.monto_en_acciones
               LET l_reg6[pos].monto_pesos_act   = l_reg7.monto_en_pesos
               LET l_reg6[pos].monto_pesos_his   = monto_historico_cv
               LET l_reg6[pos].monto_comision    = x_monto_comi_ces_vej
               LET l_reg6[pos].monto_plusvalia   = x_monto_plus_ces_vej
            END IF

            IF l_reg7.subcuenta = 4 THEN

               SELECT d.monto_aport_pat
               INTO   x_monto_vivienda
               FROM   exc_det_exceso d
               WHERE  d.folio           = x_folio
               AND    d.nss             = l_reg5.nss
               AND    d.consec_reg_lote = l_reg5.consec_reg_lote

               SELECT d.monto_plus_pat
               INTO   x_monto_plus_vivienda
               FROM   exc_exceso_plu_min d
               WHERE  d.folio           = x_folio
               AND    d.nss             = l_reg5.nss
               AND    d.consec_reg_lote = l_reg5.consec_reg_lote

               LET l_reg6[pos].nss               = l_reg5.nss
               LET l_reg6[pos].subcuenta         = l_reg7.subcuenta
               LET l_reg6[pos].siefore           = l_reg7.siefore
               LET l_reg6[pos].monto_en_acciones = l_reg7.monto_en_acciones
               LET l_reg6[pos].monto_pesos_act   = l_reg7.monto_en_pesos
               LET l_reg6[pos].monto_pesos_his   = x_monto_vivienda
               LET l_reg6[pos].monto_comision    = 0
               LET l_reg6[pos].monto_plusvalia   = x_monto_plus_vivienda
            END IF

            LET pos = pos + 1
         END FOREACH
      END FOREACH

      ERROR ""

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_reg6 TO scr_1.*

            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."

               LET vtipo_rechazo2 = vtipo_rechazo

               CALL listado_pendientes(pos,vtipo_rechazo2)

            ON KEY (INTERRUPT)
               EXIT DISPLAY

            ON KEY (control-c)
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_3
      ELSE
         ERROR "Registro inexistente ..."
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_3
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION proceso_preliq(vtipo_rechazo)

   DEFINE vtipo_rechazo           SMALLINT,
          total_liquidado_pesos   DECIMAL(16,6),
          total_liquidado_accion  DECIMAL(16,6),
          total_pesos_final       DECIMAL(16,6),
          total_accion_final      DECIMAL(16,6)

   DEFINE x_folio                 INTEGER,
          x_subcuenta             CHAR(3),
          x_fecha                 DATE,
          opc                     CHAR(1),
          x_precio_accion         DECIMAL(16,6),
          x_siefore               SMALLINT

   DEFINE x_precio_rcv            DECIMAL(16,6),
          x_precio_rcv2           DECIMAL(16,6),
          x_precio_viv            DECIMAL(19,14),
          xx_precio_del_dia       DECIMAL(19,14)

   DEFINE precio_sie1 ,
          precio_sie2 ,
          precio_sie3 ,
          precio_sie4 ,
          precio_sie5 ,
          precio_sie11 DECIMAL(19,14)

   DEFINE subtotal_sie1  ,
          subtotal_sie2  ,
          subtotal_sie3  ,
          subtotal_sie4  ,
          subtotal_sie5  ,
          subtotal_sie11 DECIMAL(16,6)

   DEFINE --subtotal_sie2            DECIMAL(16,6),
          --subtotal_sie1            DECIMAL(16,6),
          subtotal_part            DECIMAL(19,14)


   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_4 AT 6,3 WITH FORM "EXCC0025" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrol-p) Impresion            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                        PAGOS EN EXCESO PRE-LIQUIDACION                         " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      LET x_folio = NULL
      LET x_fecha = NULL
      LET x_subcuenta = NULL

      INPUT BY NAME x_folio,
                    x_subcuenta,
                    x_fecha WITHOUT DEFAULTS

         AFTER FIELD x_folio
            IF x_folio IS NULL THEN
               ERROR "Este campo no puede ser nulo"
               NEXT FIELD x_folio
            END IF

            SELECT "X"
            FROM   dis_ctrl_proceso
            WHERE  folio = x_folio
            AND    proceso_cod = "EXC"
            AND    etapa_cod = 1

            IF SQLCA.SQLCODE <> 0 THEN
               PROMPT "FOLIO NO EXISTE,VERIFICAR POR FAVOR" ATTRIBUTE (REVERSE)
                       FOR opc ATTRIBUTE (REVERSE)
               NEXT FIELD x_folio
            END IF

         AFTER FIELD x_subcuenta

            IF x_subcuenta IS NULL THEN
               ERROR "Grupo de subcuenta no puede ser nulo"
               NEXT FIELD x_subcuenta
            END IF

         AFTER FIELD x_fecha
            IF x_fecha IS NULL THEN
               ERROR "Fecha de presio de accion no puede ser nula"
               NEXT FIELD x_fecha
            END IF

            IF x_subcuenta = "RCV" THEN
               LET x_siefore = 2
            END IF

            IF x_subcuenta = "VIV" THEN
               LET x_siefore = 11
            END IF

            SELECT precio_del_dia
            INTO   x_precio_accion
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = x_fecha
            AND    codigo_siefore  = x_siefore

            IF SQLCA.SQLCODE <> 0 THEN
               PROMPT "PRECIO ACCION NO EXISTE,VERIFICAR POR FAVOR"
                      ATTRIBUTE(REVERSE)
                  FOR opc ATTRIBUTE (REVERSE)
               NEXT FIELD x_fecha
            END IF

            EXIT INPUT

         ON KEY(ESC)
            IF x_folio IS NULL THEN
               ERROR "Este campo no puede ser nulo"
               NEXT FIELD x_folio
            END IF

            SELECT "X"
            FROM   dis_ctrl_proceso
            WHERE  folio = x_folio
            AND    proceso_cod = "EXC"
            AND    etapa_cod = 1

            IF SQLCA.SQLCODE <> 0 THEN
               PROMPT "FOLIO NO EXISTE,VERIFICAR POR FAVOR" ATTRIBUTE (REVERSE)
                       FOR opc ATTRIBUTE (REVERSE)
               NEXT FIELD x_folio
            END IF

            IF x_fecha IS NULL THEN
               ERROR "Fecha de precio de accion no puede ser nula"
               NEXT FIELD x_fecha
            END IF

            SELECT precio_del_dia
            INTO   x_precio_accion
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = x_fecha
            AND    codigo_siefore = x_siefore

            LET xx_precio_accion = x_precio_accion

            IF SQLCA.SQLCODE <> 0 THEN
               PROMPT "PRECI ACCION NO EXISTE,VERIFICAR POR FAVOR"
                      ATTRIBUTE (REVERSE)
                  FOR opc ATTRIBUTE (REVERSE)
               NEXT FIELD x_fecha
            END IF

            EXIT INPUT

         ON KEY(INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT INPUT
      END INPUT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_4
         RETURN
      END IF

      ERROR "PROCESANDO INFORMACION"

      LET sel_where =  " SELECT folio,",
                              " nss,",
                              " subcuenta,",
                              " tipo_movimiento,",
                              " siefore,",
                              " fecha_valor,",
                              " fecha_conversion,",
                              " monto_en_pesos,",
                              " monto_en_acciones "

      IF x_subcuenta = "RCV" THEN
         LET sel_where = sel_where CLIPPED,
                       " FROM   dis_provision ",
                       " WHERE  folio = ",x_folio CLIPPED,
                       " AND    subcuenta IN (1,2) ",
                       " AND    tipo_movimiento BETWEEN 540 AND 555 ",
                       " ORDER BY 2,3 "
      ELSE
         IF x_subcuenta = "VIV" THEN
            LET sel_where = sel_where CLIPPED,
                          " FROM   dis_provision ",
                          " WHERE folio = ",x_folio CLIPPED,
                          " AND   subcuenta = 4 ",
                          " AND    tipo_movimiento BETWEEN 540 AND 555 ",
                          " ORDER BY 2,3 "
{
         ELSE
            LET sel_where = sel_where CLIPPED,
                          " FROM   dis_provision ",
                          " WHERE folio = ",x_folio CLIPPED,
                          " AND    tipo_movimiento BETWEEN 540 AND 555 ",
                          " ORDER BY 2,3 "
}
         END IF
      END IF

      PREPARE exe_sel_4 FROM sel_where
      DECLARE cursor_4 CURSOR FOR exe_sel_4

      LET pos = 1
      LET total_liquidado_pesos = 0
      LET total_liquidado_accion = 0

      LET subtotal_part = 0
      LET x_precio_rcv  = 0
      LET x_precio_rcv2 = 0
      LET x_precio_viv  = 0
      LET xx_precio_del_dia = 0
      LET x_precio_accion   = 0

      INITIALIZE precio_sie1  TO NULL
      INITIALIZE precio_sie2  TO NULL
      INITIALIZE precio_sie3  TO NULL
      INITIALIZE precio_sie4  TO NULL
      INITIALIZE precio_sie5  TO NULL
      INITIALIZE precio_sie11 TO NULL

      LET subtotal_sie1 = 0
      LET subtotal_sie2 = 0
      LET subtotal_sie3 = 0
      LET subtotal_sie4 = 0
      LET subtotal_sie5 = 0
      LET subtotal_sie11= 0

      FOREACH cursor_4 INTO l_reg3.*

         LET l_reg4[pos].folio             = l_reg3.folio
         LET l_reg4[pos].nss               = l_reg3.nss
         LET l_reg4[pos].subcuenta         = l_reg3.subcuenta
         LET l_reg4[pos].tipo_movimiento   = l_reg3.tipo_movimiento
         LET l_reg4[pos].siefore           = l_reg3.siefore
         LET l_reg4[pos].fecha_valor       = l_reg3.fecha_valor
         LET l_reg4[pos].fecha_conversion  = x_fecha

         IF l_reg3.siefore = 11 THEN
            LET l_reg3.fecha_conversion = l_reg3.fecha_valor
         END IF

         SELECT precio_del_dia
         INTO   x_precio_accion
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = l_reg3.fecha_conversion
         AND    codigo_siefore  = l_reg3.siefore

         LET l_reg4[pos].monto_en_pesos   = l_reg3.monto_en_acciones *
                                            x_precio_accion

         LET l_reg4[pos].monto_en_acciones = l_reg3.monto_en_acciones

         LET total_liquidado_pesos  = total_liquidado_pesos +
                                      l_reg4[pos].monto_en_pesos

         LET total_liquidado_accion = total_liquidado_accion +
                                      l_reg4[pos].monto_en_acciones

         LET xx_precio_del_dia = x_precio_accion

         CASE l_reg3.siefore
            WHEN 1
            	 LET precio_sie1   = xx_precio_del_dia
            	 LET subtotal_sie1 = subtotal_sie1 + l_reg3.monto_en_acciones

               {LET x_precio_rcv2 = xx_precio_del_dia
               LET subtotal_sie1 = subtotal_sie1 + l_reg3.monto_en_acciones}
            WHEN 2
            	 LET precio_sie2   = xx_precio_del_dia
            	 LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones

               {LET x_precio_rcv  = xx_precio_del_dia
               LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones}
            WHEN 3
            	 LET precio_sie3   = xx_precio_del_dia
            	 LET subtotal_sie3 = subtotal_sie3 + l_reg3.monto_en_acciones

               {LET x_precio_rcv  = xx_precio_del_dia
               LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones}
            WHEN 4
            	 LET precio_sie4   = xx_precio_del_dia
            	 LET subtotal_sie4 = subtotal_sie4 + l_reg3.monto_en_acciones

               {LET x_precio_rcv  = xx_precio_del_dia
               LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones}
            WHEN 5
            	 LET precio_sie5   = xx_precio_del_dia
            	 LET subtotal_sie5 = subtotal_sie5 + l_reg3.monto_en_acciones

               {LET x_precio_rcv  = xx_precio_del_dia
               LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones}
            WHEN 11
            	 LET precio_sie11   = xx_precio_del_dia
            	 LET subtotal_sie11 = subtotal_sie11 + l_reg3.monto_en_acciones

               {LET x_precio_viv  = xx_precio_del_dia
               LET subtotal_part = subtotal_part + l_reg3.monto_en_acciones}
         END CASE

         LET pos = pos +1
      END FOREACH

      {DISPLAY BY NAME x_precio_rcv,
                      x_precio_rcv2,
                      x_precio_viv

      DISPLAY BY NAME subtotal_sie2,
                      subtotal_sie1,
                      subtotal_part}

      DISPLAY BY NAME precio_sie1  ,
                      precio_sie2  ,
                      precio_sie3  ,
                      precio_sie4  ,
                      precio_sie5  ,
                      precio_sie11 ,

                      subtotal_sie1 ,
                      subtotal_sie2 ,
                      subtotal_sie3 ,
                      subtotal_sie4 ,
                      subtotal_sie5 ,
                      subtotal_sie11


      LET total_pesos_final = total_liquidado_pesos
      LET total_accion_final = total_liquidado_accion

      DISPLAY BY NAME total_pesos_final
      DISPLAY BY NAME total_accion_final

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_reg4 TO scr_1.*
            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."

               LET vtipo_rechazo2 = vtipo_rechazo

               CALL listado_pendientes(pos,vtipo_rechazo2)

            ON KEY (control-c)
               INITIALIZE l_reg3.* TO NULL
               EXIT DISPLAY
         END DISPLAY
         LET pos = 0
         CLOSE WINDOW ventana_4
      ELSE
         ERROR "Registro inexistente ..."
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_4
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION proceso_cuenta(vtipo_rechazo)

   DEFINE vtipo_rechazo           SMALLINT,
          total_liquidado_pesos   DECIMAL(16,6),
          total_liquidado_accion  DECIMAL(16,6),
          total_pesos_final       DECIMAL(16,6),
          total_accion_final      DECIMAL(16,6)

   DEFINE xx_precio_del_dia       DECIMAL(19,14)

   DEFINE precio_sie1 ,
          precio_sie2 ,
          precio_sie3 ,
          precio_sie4 ,
          precio_sie5 ,
          precio_sie11 DECIMAL(19,14)

   DEFINE subtotal_sie1  ,
          subtotal_sie2  ,
          subtotal_sie3  ,
          subtotal_sie4  ,
          subtotal_sie5  ,
          subtotal_sie11 DECIMAL(16,6)

   DEFINE --subtotal_sie2            DECIMAL(16,6),
          --subtotal_sie1            DECIMAL(16,6),
          subtotal_part            DECIMAL(19,14)

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "EXCC0023" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrol-p) Impresion            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                            PAGOS EN EXCESO  LIQUIDADOS                      " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON folio,
                             nss,
                             subcuenta,
                             tipo_movimiento,
                             siefore
                        FROM folio,
                             nss,
                             subcuenta,
                             tipo_movimiento,
                             siefore

         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      ERROR "PROCESANDO INFORMACION ..."

      LET sel_where =  " SELECT folio,",
                              " nss,",
                              " subcuenta,",
                              " tipo_movimiento,",
                              " siefore,",
                              " fecha_valor,",
                              " fecha_conversion,",
                              " monto_en_pesos,",
                              " monto_en_acciones ",
                       " FROM   dis_cuenta ",
                       " WHERE ",cla_where CLIPPED,
                       " AND    tipo_movimiento BETWEEN 540 AND 555 ",
                       " ORDER BY 1,2 "

      PREPARE exe_sel_2 FROM sel_where
      DECLARE cursor_2 CURSOR FOR exe_sel_2

      LET pos = 1
      LET total_liquidado_pesos = 0
      LET total_liquidado_accion = 0

      INITIALIZE precio_sie1  TO NULL
      INITIALIZE precio_sie2  TO NULL
      INITIALIZE precio_sie3  TO NULL
      INITIALIZE precio_sie4  TO NULL
      INITIALIZE precio_sie5  TO NULL
      INITIALIZE precio_sie11 TO NULL

      LET subtotal_sie1 = 0
      LET subtotal_sie2 = 0
      LET subtotal_sie3 = 0
      LET subtotal_sie4 = 0
      LET subtotal_sie5 = 0
      LET subtotal_sie11= 0

      FOREACH cursor_2 INTO l_reg3.*

         LET l_reg4[pos].folio             = l_reg3.folio
         LET l_reg4[pos].nss               = l_reg3.nss
         LET l_reg4[pos].subcuenta         = l_reg3.subcuenta
         LET l_reg4[pos].tipo_movimiento   = l_reg3.tipo_movimiento
         LET l_reg4[pos].siefore           = l_reg3.siefore
         LET l_reg4[pos].fecha_valor       = l_reg3.fecha_valor
         LET l_reg4[pos].fecha_conversion  = l_reg3.fecha_conversion
         LET l_reg4[pos].monto_en_pesos    = l_reg3.monto_en_pesos
         LET l_reg4[pos].monto_en_acciones = l_reg3.monto_en_acciones

         LET total_liquidado_pesos  = total_liquidado_pesos +
                                      l_reg4[pos].monto_en_pesos
         LET total_liquidado_accion = total_liquidado_accion +
                                      l_reg4[pos].monto_en_acciones

         IF l_reg3.siefore = 11 THEN
            LET l_reg3.fecha_conversion = l_reg3.fecha_valor
         END IF

         SELECT precio_del_dia
         INTO   xx_precio_del_dia
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = l_reg3.fecha_conversion
         AND    codigo_siefore = l_reg3.siefore

         CASE l_reg3.siefore
            WHEN 1
            	 LET precio_sie1   = xx_precio_del_dia
            	 LET subtotal_sie1 = subtotal_sie1 + l_reg3.monto_en_acciones

               {LET x_precio_rcv2 = xx_precio_del_dia
               LET subtotal_sie1 = subtotal_sie1 + l_reg3.monto_en_acciones}
            WHEN 2
            	 LET precio_sie2   = xx_precio_del_dia
            	 LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones

               {LET x_precio_rcv  = xx_precio_del_dia
               LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones}
            WHEN 3
            	 LET precio_sie3   = xx_precio_del_dia
            	 LET subtotal_sie3 = subtotal_sie3 + l_reg3.monto_en_acciones

               {LET x_precio_rcv  = xx_precio_del_dia
               LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones}
            WHEN 4
            	 LET precio_sie4   = xx_precio_del_dia
            	 LET subtotal_sie4 = subtotal_sie4 + l_reg3.monto_en_acciones

               {LET x_precio_rcv  = xx_precio_del_dia
               LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones}
            WHEN 5
            	 LET precio_sie5   = xx_precio_del_dia
            	 LET subtotal_sie5 = subtotal_sie5 + l_reg3.monto_en_acciones

               {LET x_precio_rcv  = xx_precio_del_dia
               LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones}
            WHEN 11
            	 LET precio_sie11   = xx_precio_del_dia
            	 LET subtotal_sie11 = subtotal_sie11 + l_reg3.monto_en_acciones

               {LET x_precio_viv  = xx_precio_del_dia
               LET subtotal_part = subtotal_part + l_reg3.monto_en_acciones}
         END CASE

         LET pos = pos + 1
      END FOREACH

      {DISPLAY BY NAME x_precio_rcv,
                      x_precio_rcv2,
                      x_precio_viv

      DISPLAY BY NAME subtotal_sie2,
                      subtotal_sie1,
                      subtotal_part}

      DISPLAY BY NAME precio_sie1  ,
                      precio_sie2  ,
                      precio_sie3  ,
                      precio_sie4  ,
                      precio_sie5  ,
                      precio_sie11 ,

                      subtotal_sie1 ,
                      subtotal_sie2 ,
                      subtotal_sie3 ,
                      subtotal_sie4 ,
                      subtotal_sie5 ,
                      subtotal_sie11

      LET total_pesos_final = total_liquidado_pesos
      LET total_accion_final = total_liquidado_accion

      DISPLAY BY NAME total_pesos_final
      DISPLAY BY NAME total_accion_final

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_reg4 TO scr_1.*
            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."

               LET vtipo_rechazo2 = vtipo_rechazo

               CALL listado_pendientes(pos,vtipo_rechazo2)

            ON KEY (INTERRUPT)
               EXIT DISPLAY

            ON KEY (control-c)
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "Registro inexistente ..."
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION listado_pendientes(pos,vtipo_rechazo2)
   DEFINE vtipo_rechazo2     SMALLINT,
          pos                SMALLINT,
          i                  SMALLINT

   DEFINE g_reg RECORD
          folio              INTEGER,
          nss                CHAR(11),
          subcuenta          SMALLINT,
          tipo_movimiento    INTEGER,
          siefore            SMALLINT,
          fecha_valor        DATE,
          fecha_conversion   DATE,
          monto_en_pesos     DECIMAL(16,6),
          monto_en_acciones  DECIMAL(16,6),
          xx_sum_pes1        DECIMAL(16,2),
          xx_sum_acc1        DECIMAL(16,6),
          xx_sum_pes2        DECIMAL(16,2),
          xx_sum_acc2        DECIMAL(16,6),
          xx_sum_pes3        DECIMAL(16,2),
          xx_sum_acc3        DECIMAL(16,6),
          xx_sum_pes4        DECIMAL(16,2),
          xx_sum_acc4        DECIMAL(16,6),
          xx_sum_pes5        DECIMAL(16,2),
          xx_sum_acc5        DECIMAL(16,6),
          xx_sum_pes11       DECIMAL(16,2),
          xx_sum_acc11       DECIMAL(16,6)
   END RECORD

   DEFINE g_reg1 RECORD
          folio              INTEGER,
          nss                CHAR(11),
          subcuenta          SMALLINT,
          siefore            SMALLINT,
          monto_en_acciones  DECIMAL(16,6),
          monto_pesos_act    DECIMAL(16,6),
          monto_pesos_his    DECIMAL(16,6),
          monto_comision     DECIMAL(16,6),
          monto_plusvalia    DECIMAL(16,6)
   END RECORD

   DEFINE opc                CHAR(1)
   DEFINE flag               SMALLINT

   ERROR "PROCESANDO INFORMACION ..."

   LET g_reg.xx_sum_pes1  = 0
   LET g_reg.xx_sum_acc1  = 0
   LET g_reg.xx_sum_pes2  = 0
   LET g_reg.xx_sum_acc2  = 0
   LET g_reg.xx_sum_pes3  = 0
   LET g_reg.xx_sum_acc3  = 0
   LET g_reg.xx_sum_pes4  = 0
   LET g_reg.xx_sum_acc4  = 0
   LET g_reg.xx_sum_pes5  = 0
   LET g_reg.xx_sum_acc5  = 0
   LET g_reg.xx_sum_pes11 = 0
   LET g_reg.xx_sum_acc11 = 0

   LET g_impre = vruta_listado CLIPPED,"/",
                 usuario CLIPPED,
                 ".IMPTPAGOEXCES"

   LET i = 1

   IF vtipo_rechazo2 = 5 THEN --LIQUIDACION
      START REPORT impresion_2 TO g_impre

      FOR i = 1 TO (pos)

         LET g_reg.folio             = l_reg4[i].folio
         LET g_reg.nss               = l_reg4[i].nss
         LET g_reg.subcuenta         = l_reg4[i].subcuenta
         LET g_reg.siefore           = l_reg4[i].siefore
         LET g_reg.tipo_movimiento   = l_reg4[i].tipo_movimiento
         LET g_reg.fecha_valor       = l_reg4[i].fecha_valor
         LET g_reg.fecha_conversion  = l_reg4[i].fecha_conversion
         LET g_reg.monto_en_pesos    = l_reg4[i].monto_en_pesos
         LET g_reg.monto_en_acciones = l_reg4[i].monto_en_acciones

         CASE g_reg.siefore
            WHEN 1
               LET g_reg.xx_sum_pes1 = g_reg.xx_sum_pes1 +
                                       l_reg4[i].monto_en_pesos * -1
               LET g_reg.xx_sum_acc1 = g_reg.xx_sum_acc1 +
                                       l_reg4[i].monto_en_acciones * -1
            WHEN 2
               LET g_reg.xx_sum_pes2 = g_reg.xx_sum_pes2 +
                                       l_reg4[i].monto_en_pesos * -1
               LET g_reg.xx_sum_acc2 = g_reg.xx_sum_acc2 +
                                       l_reg4[i].monto_en_acciones * -1
            WHEN 3
               LET g_reg.xx_sum_pes3 = g_reg.xx_sum_pes3 +
                                       l_reg4[i].monto_en_pesos * -1
               LET g_reg.xx_sum_acc3 = g_reg.xx_sum_acc3 +
                                          l_reg4[i].monto_en_acciones * -1
            WHEN 4
               LET g_reg.xx_sum_pes4 = g_reg.xx_sum_pes4 +
                                       l_reg4[i].monto_en_pesos * -1
               LET g_reg.xx_sum_acc4 = g_reg.xx_sum_acc4 +
                                       l_reg4[i].monto_en_acciones * -1
            WHEN 5
               LET g_reg.xx_sum_pes5 = g_reg.xx_sum_pes5 +
                                       l_reg4[i].monto_en_pesos * -1
               LET g_reg.xx_sum_acc5 = g_reg.xx_sum_acc5 +
                                       l_reg4[i].monto_en_acciones * -1
            WHEN 11
               LET g_reg.xx_sum_pes11 = g_reg.xx_sum_pes11 +
                                        l_reg4[i].monto_en_pesos * -1
               LET g_reg.xx_sum_acc11 = g_reg.xx_sum_acc11 +
                                        l_reg4[i].monto_en_acciones * -1
         END CASE

         IF pos = i  THEN
            EXIT FOR
         END IF
         IF g_reg.nss IS NULL THEN
            EXIT FOR
         END IF

         OUTPUT TO REPORT impresion_2(g_reg.*,vtipo_rechazo2)
      END FOR

      FINISH REPORT impresion_2
   ELSE
      IF vtipo_rechazo2 = 6 THEN --PRELIQUIDACION
         START REPORT impresion_2 TO g_impre

         FOR i = 1 TO (pos)

            LET g_reg.folio             = l_reg4[i].folio
            LET g_reg.nss               = l_reg4[i].nss
            LET g_reg.subcuenta         = l_reg4[i].subcuenta
            LET g_reg.siefore           = l_reg4[i].siefore
            LET g_reg.tipo_movimiento   = l_reg4[i].tipo_movimiento
            LET g_reg.fecha_valor       = l_reg4[i].fecha_valor
            LET g_reg.fecha_conversion  = l_reg4[i].fecha_conversion
            LET g_reg.monto_en_pesos    = l_reg4[i].monto_en_pesos
            LET g_reg.monto_en_acciones = l_reg4[i].monto_en_acciones

            #IF g_reg.nss IS NULL THEN
               #EXIT FOR
            #END IF

            CASE g_reg.siefore
               WHEN 1
                  LET g_reg.xx_sum_pes1 = g_reg.xx_sum_pes1 +
                                          l_reg4[i].monto_en_pesos * -1
                  LET g_reg.xx_sum_acc1 = g_reg.xx_sum_acc1 +
                                          l_reg4[i].monto_en_acciones * -1
               WHEN 2
                  LET g_reg.xx_sum_pes2 = g_reg.xx_sum_pes2 +
                                          l_reg4[i].monto_en_pesos * -1
                  LET g_reg.xx_sum_acc2 = g_reg.xx_sum_acc2 +
                                          l_reg4[i].monto_en_acciones * -1
               WHEN 3
                  LET g_reg.xx_sum_pes3 = g_reg.xx_sum_pes3 +
                                          l_reg4[i].monto_en_pesos * -1
                  LET g_reg.xx_sum_acc3 = g_reg.xx_sum_acc3 +
                                          l_reg4[i].monto_en_acciones * -1
               WHEN 4
                  LET g_reg.xx_sum_pes4 = g_reg.xx_sum_pes4 +
                                          l_reg4[i].monto_en_pesos * -1
                  LET g_reg.xx_sum_acc4 = g_reg.xx_sum_acc4 +
                                          l_reg4[i].monto_en_acciones * -1
               WHEN 5
                  LET g_reg.xx_sum_pes5 = g_reg.xx_sum_pes5 +
                                          l_reg4[i].monto_en_pesos * -1
                  LET g_reg.xx_sum_acc5 = g_reg.xx_sum_acc5 +
                                          l_reg4[i].monto_en_acciones * -1
               WHEN 11
                  LET g_reg.xx_sum_pes11 = g_reg.xx_sum_pes11 +
                                           l_reg4[i].monto_en_pesos * -1
                  LET g_reg.xx_sum_acc11 = g_reg.xx_sum_acc11 +
                                           l_reg4[i].monto_en_acciones * -1
            END CASE

            IF pos = i  THEN
               EXIT FOR
            END IF

            OUTPUT TO REPORT impresion_2(g_reg.*,vtipo_rechazo2)
         END FOR

         FINISH REPORT impresion_2
      ELSE
         IF vtipo_rechazo2 = 4 THEN
            START REPORT impresion_3 TO g_impre

            FOR i = 1 TO (pos)

               LET g_reg1.folio             = x_folio
               LET g_reg1.nss               = l_reg6[i].nss
               LET g_reg1.subcuenta         = l_reg6[i].subcuenta
               LET g_reg1.siefore           = l_reg6[i].siefore
               LET g_reg1.monto_en_acciones = l_reg6[i].monto_en_acciones
               LET g_reg1.monto_pesos_act   = l_reg6[i].monto_pesos_act
               LET g_reg1.monto_pesos_his   = l_reg6[i].monto_pesos_his
               LET g_reg1.monto_comision    = l_reg6[i].monto_comision
               LET g_reg1.monto_plusvalia   = l_reg6[i].monto_plusvalia

               IF pos = i  THEN
                  EXIT FOR
               END IF
               IF g_reg1.nss IS NULL THEN
                  EXIT FOR
               END IF

               OUTPUT TO REPORT impresion_3(g_reg1.*,vtipo_rechazo2)
            END FOR

            FINISH REPORT impresion_3
         ELSE
            START REPORT impresion_1 TO g_impre

            FOREACH cursor_1 INTO l_reg.*
               OUTPUT TO REPORT impresion_1(l_reg.*,vtipo_rechazo2)
            END FOREACH

            FINISH REPORT impresion_1
         END IF
      END IF
   END IF

   ERROR ""
   ERROR "LISTADO GENERADO ..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   #LET g_lista = "vi ",g_impre
   RUN g_lista

END FUNCTION
###############################################################################
REPORT impresion_1(l_reg,vtipo_rechazo2)

   DEFINE l_reg RECORD
          folio              INTEGER,
          nss                CHAR(11),
          fecha_pago         DATE,
          fecha_pago_sua     CHAR(6),
          monto_ret          DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_ces_vej_pat  DECIMAL(16,6),
          monto_ces_vej_tra  DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_par_viv      DECIMAL(18,6),
          tipo_diagnostico   CHAR(3),
          monto_retiro       DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE vtipo_rechazo2     SMALLINT

   DEFINE L1                 CHAR(01),
          L2                 CHAR(02),
          L3                 CHAR(03),
          L4                 CHAR(04),
          L5                 CHAR(05),
          L6                 CHAR(06),
          L7                 CHAR(07),
          L8                 CHAR(08),
          L9                 CHAR(09),
          L10                CHAR(10)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  90

   FORMAT
      PAGE HEADER
         LET L1  = "\304"
         LET L2  = "\304\304"
         LET L3  = "\304\304\304"
         LET L4  = "\304\304\304\304"
         LET L5  = "\304\304\304\304\304"
         LET L6  = "\304\304\304\304\304\304"
         LET L7  = "\304\304\304\304\304\304\304"
         LET L8  = "\304\304\304\304\304\304\304\304"
         LET L9  = "\304\304\304\304\304\304\304\304\304"
         LET L10 = "\304\304\304\304\304\304\304\304\304\304"

         IF vtipo_rechazo2 = 1 THEN

            PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
            PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
            PRINT COLUMN 01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\277'
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN  02,"EXCC002",
                  COLUMN  45,"REPORTE DE PAGOS EN EXCESO ACEPTADOS ",
                  COLUMN 124,hoy USING "DD-MM-YYYY",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\331'

            PRINT COLUMN  01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                L10,L10,L10,L8,'\277'

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN  02,"FOLIO",
                  COLUMN  08,"NSS",
                  COLUMN  16,"FECHA ",
                  COLUMN  24,"FOLIO",
                  COLUMN  32,"Mto.",
                  COLUMN  43,"Mto. Act.",
                  COLUMN  58,"Mto.Ces. y Vej.",
                  COLUMN  75,"Mto.Ces. y Vej.",
                  COLUMN  95,"Mto.Act.",
                  COLUMN 110,"Mto.Aport.",
                  COLUMN 123,"Tipo",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN  16,"PAGO ",
                  COLUMN  24,"SUA",
                  COLUMN  31,"RETIRO",
                  COLUMN  44,"RETIRO",
                  COLUMN  61,"PATRONAL",
                  COLUMN  77,"TRABAJADOR",
                  COLUMN  94,"Ces. y Vej.",
                  COLUMN 109,"PARTICIPACION",
                  COLUMN 124,"DIAGNOSTICO",
                  COLUMN 135,"|"

            PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\331'
         ELSE
            IF vtipo_rechazo2 = 2 THEN

               PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
               PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
               PRINT COLUMN 01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                       L10,L10,L10,L8,'\277'
               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN  02,"EXCC002",
                     COLUMN  45,"REPORTE DE PAGOS EN EXCESO RECHAZADOS",
                     COLUMN 124,hoy USING "DD-MM-YYYY",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"
               PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                       L10,L10,L10,L8,'\331'

               PRINT COLUMN 01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                       L10,L10,L10,L8,'\277'

               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN  02,"FOLIO",
                     COLUMN  08,"NSS",
                     COLUMN  16,"FECHA ",
                     COLUMN  24,"FOLIO",
                     COLUMN  32,"Mto.",
                     COLUMN  43,"Mto. Act.",
                     COLUMN  58,"Mto.Ces. y Vej.",
                     COLUMN  75,"Mto.Ces. y Vej.",
                     COLUMN  95,"Mto.Act.",
                     COLUMN 110,"Mto.Aport.",
                     COLUMN 123,"Tipo",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"

               PRINT COLUMN  01,"|",
                     COLUMN  16,"PAGO ",
                     COLUMN  24,"SUA",
                     COLUMN  31,"RETIRO",
                     COLUMN  44,"RETIRO",
                     COLUMN  61,"PATRONAL",
                     COLUMN  77,"TRABAJADOR",
                     COLUMN  94,"Ces. y Vej.",
                     COLUMN 109,"PARTICIPACION",
                     COLUMN 124,"DIAGNOSTICO",
                     COLUMN 135,"|"

               PRINT COLUMN 01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                       L10,L10,L10,L8,'\331'
            ELSE
               PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
               PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
               PRINT COLUMN 01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                       L10,L10,L10,L8,'\277'
               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN  02,"EXCC002",
                     COLUMN  45,"REPORTE DE PAGOS EN EXCESO PENDIENTES",
                     COLUMN 124,hoy USING "DD-MM-YYYY",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"
               PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                       L10,L10,L10,L8,'\331'

               PRINT COLUMN 01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                       L10,L10,L10,L8,'\277'

               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN  02,"FOLIO",
                     COLUMN  08,"NSS",
                     COLUMN  16,"FECHA ",
                     COLUMN  24,"FOLIO",
                     COLUMN  32,"Mto.",
                     COLUMN  43,"Mto. Act.",
                     COLUMN  58,"Mto.Ces. y Vej.",
                     COLUMN  75,"Mto.Ces. y Vej.",
                     COLUMN  95,"Mto.Act.",
                     COLUMN 110,"Mto.Aport.",
                     COLUMN 123,"Tipo",
                     COLUMN 135,"|"
               PRINT COLUMN  01,"|",
                     COLUMN 135,"|"

               PRINT COLUMN  01,"|",
                     COLUMN  16,"PAGO ",
                     COLUMN  24,"SUA",
                     COLUMN  31,"RETIRO",
                     COLUMN  44,"RETIRO",
                     COLUMN  61,"PATRONAL",
                     COLUMN  77,"TRABAJADOR",
                     COLUMN  94,"Ces. y Vej.",
                     COLUMN 109,"PARTICIPACION",
                     COLUMN 124,"DIAGNOSTICO",
                     COLUMN 135,"|"

               PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                       L10,L10,L10,L8,'\331'
          END IF
       END IF

      ON EVERY ROW
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H',
                COLUMN 001,'\033e\033(s218T\033(s21H\033(s7B'

         PRINT COLUMN 03,l_reg.folio USING  "<<<<<<",
               COLUMN 13,l_reg.nss,
               COLUMN 26,l_reg.fecha_pago,
               COLUMN 39,l_reg.fecha_pago_sua,
               COLUMN 40,l_reg.monto_ret USING "#,###,##&.##",
               COLUMN 65,l_reg.monto_act_ret USING "#,###,##&.##",
               COLUMN 97,l_reg.monto_ces_vej_pat USING "#,###,##&.##"  ,
               COLUMN 124,l_reg.monto_ces_vej_tra USING "#,###,##&.##" ,
               COLUMN 152,l_reg.monto_act_ces_vej USING "#,###,##&.##" ,
               COLUMN 178,l_reg.monto_par_viv USING "#,###,##&.######" ,
               COLUMN 205,l_reg.tipo_diagnostico

      PAGE TRAILER
         SKIP 3 LINE
         PRINT COLUMN 90," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H',
               COLUMN 001,'\033e\033(s218T\033(s21H\033(s7B'

         PRINT COLUMN 03," Total de Registros: ",COUNT(*) USING "<<<<",
               COLUMN 38,"TOTAL: ",SUM(l_reg.monto_ret) USING "#,###,##&.##",
               COLUMN 65,SUM(l_reg.monto_act_ret) USING "#,###,##&.##",
               COLUMN 97,SUM(l_reg.monto_ces_vej_pat) USING "#,###,##&.##",
               COLUMN 124,SUM(l_reg.monto_ces_vej_tra) USING "#,###,##&.##",
               COLUMN 152,SUM(l_reg.monto_act_ces_vej) USING "#,###,##&.##",
               COLUMN 178,SUM(l_reg.monto_par_viv) USING "#,###,##&.######"
END REPORT
################################################################################
REPORT impresion_2(l_reg3,vtipo_rechazo2)

   DEFINE l_reg3 RECORD
          folio              INTEGER,
          nss                CHAR(11),
          subcuenta          SMALLINT,
          tipo_movimiento    INTEGER,
          siefore            SMALLINT,
          fecha_valor        DATE,
          fecha_conversion   DATE,
          monto_en_pesos     DECIMAL(16,6),
          monto_en_acciones  DECIMAL(16,6),
          xx_sum_pes1        DECIMAL(16,2),
          xx_sum_acc1        DECIMAL(16,6),
          xx_sum_pes2        DECIMAL(16,2),
          xx_sum_acc2        DECIMAL(16,6),
          xx_sum_pes3        DECIMAL(16,2),
          xx_sum_acc3        DECIMAL(16,6),
          xx_sum_pes4        DECIMAL(16,2),
          xx_sum_acc4        DECIMAL(16,6),
          xx_sum_pes5        DECIMAL(16,2),
          xx_sum_acc5        DECIMAL(16,6),
          xx_sum_pes11       DECIMAL(16,2),
          xx_sum_acc11       DECIMAL(16,6)
   END RECORD

   DEFINE vtipo_rechazo2     SMALLINT
   DEFINE siefore_x          SMALLINT,
          xpos               SMALLINT,
          x_sie              CHAR(10)

   DEFINE L1           CHAR(01),
          L2           CHAR(02),
          L3           CHAR(03),
          L4           CHAR(04),
          L5           CHAR(05),
          L6           CHAR(06),
          L7           CHAR(07),
          L8           CHAR(08),
          L9           CHAR(09),
          L10          CHAR(10)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  80

   FORMAT
      PAGE HEADER
            LET L1  = "\304"
            LET L2  = "\304\304"
            LET L3  = "\304\304\304"
            LET L4  = "\304\304\304\304"
            LET L5  = "\304\304\304\304\304"
            LET L6  = "\304\304\304\304\304\304"
            LET L7  = "\304\304\304\304\304\304\304"
            LET L8  = "\304\304\304\304\304\304\304\304"
            LET L9  = "\304\304\304\304\304\304\304\304\304"
            LET L10 = "\304\304\304\304\304\304\304\304\304\304"

            PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
            PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
            PRINT COLUMN 01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\277'
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            IF vtipo_rechazo2 = 5 THEN
               PRINT COLUMN  01,"|",
                     COLUMN  02,"EXCC002",
                     COLUMN  45,"REPORTE DE PAGOS EN EXCESO LIQUIDADOS",
                     COLUMN 124,hoy USING "DD-MM-YYYY",
                     COLUMN 135,"|"
            ELSE
               PRINT COLUMN  01,"|",
                     COLUMN  02,"EXCC002",
                     COLUMN  45,"REPORTE DE PAGOS EN EXCESO DE PRE-LIQUIDACION",
                     COLUMN 124,hoy USING "DD-MM-YYYY",
                     COLUMN 135,"|"
            END IF

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\331'

            PRINT COLUMN  01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                L10,L10,L10,L8,'\277'

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN  02,"Folio",
                  COLUMN  12,"NSS",
                  COLUMN  22,"Subcuenta",
                  COLUMN  33,"Siefore",
                  COLUMN  42,"Tipo Mov.",
                  COLUMN  57,"Fecha Valor",
                  COLUMN  74,"Fecha Liquid.",
                  COLUMN  94,"Mto. en Pesos",
                  COLUMN 114,"Mto. Accion/Particion",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\331'

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H',
                 COLUMN 001,'\033e\033(s218T\033(s21H\033(s7B'

      ON EVERY ROW
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H',
                 COLUMN 001,'\033e\033(s218T\033(s21H\033(s7B'

         PRINT   COLUMN 03,l_reg3.folio USING "<<<<<<",
                 COLUMN 16,l_reg3.nss,
                 COLUMN 38,l_reg3.subcuenta,
                 COLUMN 53,l_reg3.siefore,
                 COLUMN 64,l_reg3.tipo_movimiento,
                 COLUMN 96,l_reg3.fecha_valor USING "DD-MM-YYYY",
                 COLUMN 125,l_reg3.fecha_conversion USING "DD-MM-YYYY",
                 COLUMN 156,l_reg3.monto_en_pesos USING "#,###,##&.##",
                 COLUMN 196,l_reg3.monto_en_acciones USING "#,###,##&.######"

      PAGE TRAILER
         SKIP 3 LINE
         PRINT COLUMN 90," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         #PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H',
                #COLUMN 001,'\033e\033(s218T\033(s21H\033(s7B'

         PRINT COLUMN 125,"Subtotal: ",
               COLUMN 134,"Siefore 1 ",
               COLUMN 150,l_reg3.xx_sum_pes1,
               COLUMN 194,l_reg3.xx_sum_acc1

         PRINT COLUMN 135,"Siefore 2 ",
               COLUMN 150,l_reg3.xx_sum_pes2,
               COLUMN 194,l_reg3.xx_sum_acc2

         PRINT COLUMN 135,"Siefore 3 ",
               COLUMN 150,l_reg3.xx_sum_pes3,
               COLUMN 194,l_reg3.xx_sum_acc3

         PRINT COLUMN 135,"Siefore 4 ",
               COLUMN 150,l_reg3.xx_sum_pes4,
               COLUMN 194,l_reg3.xx_sum_acc4

         PRINT COLUMN 135,"Siefore 5 ",
               COLUMN 150,l_reg3.xx_sum_pes5,
               COLUMN 194,l_reg3.xx_sum_acc5

         PRINT COLUMN 135,"Siefore 11",
               COLUMN 150,l_reg3.xx_sum_pes11,
               COLUMN 194,l_reg3.xx_sum_acc11

         SKIP 2 LINE

         PRINT COLUMN 03," Total de Registros: ",COUNT(*) USING "<<<<",
               COLUMN 125,"TOTAL: ",
               COLUMN 156,SUM(l_reg3.monto_en_pesos) USING "#,###,##&.##",
               COLUMN 196,SUM(l_reg3.monto_en_acciones) USING "#,###,##&.######"

END REPORT
################################################################################
REPORT impresion_3(g_reg1,vtipo_rechazo2)

   DEFINE g_reg1 RECORD
          folio              INTEGER,
          nss                CHAR(11),
          subcuenta          SMALLINT,
          siefore            SMALLINT,
          monto_en_acciones  DECIMAL(16,6),
          monto_pesos_act    DECIMAL(16,6),
          monto_pesos_his    DECIMAL(16,6),
          monto_comision     DECIMAL(16,6),
          monto_plusvalia    DECIMAL(16,6)
   END RECORD

   DEFINE l_reg99         RECORD
          xx_sum_pes1        DECIMAL(16,6),
          xx_sum_acc1        DECIMAL(16,6),
          xx_sum_pes2        DECIMAL(16,6),
          xx_sum_acc2        DECIMAL(16,6),
          xx_sum_pes3        DECIMAL(16,6),
          xx_sum_acc3        DECIMAL(16,6),
          xx_sum_pes4        DECIMAL(16,6),
          xx_sum_acc4        DECIMAL(16,6),
          xx_sum_pes5        DECIMAL(16,6),
          xx_sum_acc5        DECIMAL(16,6),
          xx_sum_pes11       DECIMAL(16,6),
          xx_sum_acc11       DECIMAL(16,6),
          sie                SMALLINT
   END RECORD

   DEFINE vtipo_rechazo2     SMALLINT

   DEFINE L1           CHAR(01),
          L2           CHAR(02),
          L3           CHAR(03),
          L4           CHAR(04),
          L5           CHAR(05),
          L6           CHAR(06),
          L7           CHAR(07),
          L8           CHAR(08),
          L9           CHAR(09),
          L10          CHAR(10)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  80

   FORMAT
      PAGE HEADER
            LET L1  = "\304"
            LET L2  = "\304\304"
            LET L3  = "\304\304\304"
            LET L4  = "\304\304\304\304"
            LET L5  = "\304\304\304\304\304"
            LET L6  = "\304\304\304\304\304\304"
            LET L7  = "\304\304\304\304\304\304\304"
            LET L8  = "\304\304\304\304\304\304\304\304"
            LET L9  = "\304\304\304\304\304\304\304\304\304"
            LET L10 = "\304\304\304\304\304\304\304\304\304\304"

            LET l_reg99.xx_sum_pes1  = 0
            LET l_reg99.xx_sum_acc1  = 0
            LET l_reg99.xx_sum_pes2  = 0
            LET l_reg99.xx_sum_acc2  = 0
            LET l_reg99.xx_sum_pes3  = 0
            LET l_reg99.xx_sum_acc3  = 0
            LET l_reg99.xx_sum_pes4  = 0
            LET l_reg99.xx_sum_acc4  = 0
            LET l_reg99.xx_sum_pes5  = 0
            LET l_reg99.xx_sum_acc5  = 0
            LET l_reg99.xx_sum_pes11 = 0
            LET l_reg99.xx_sum_acc11 = 0

            PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
            PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
            PRINT COLUMN 01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\277'
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN  02,"EXCC002",
                  COLUMN  45,"REPORTE DE PAGOS EN EXCESO DE PROVISION",
                  COLUMN 124,hoy USING "DD-MM-YYYY",
                  COLUMN 135,"|"

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\331'

            PRINT COLUMN  01,'\332',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                L10,L10,L10,L8,'\277'

            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN  02,"Folio",
                  COLUMN  10,"NSS",
                  COLUMN  19,"Subcuenta ",
                  COLUMN  30,"Siefore ",
                  COLUMN  44,"Acciones/Part.",
                  COLUMN  64,"Pesos Actuales",
                  COLUMN  84,"Pesos Historicos",
                  COLUMN 105,"Comision",
                  COLUMN 120,"Plusvalia",
                  COLUMN 135,"|"
            PRINT COLUMN  01,"|",
                  COLUMN 135,"|"

            PRINT COLUMN  01,'\300',L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,
                                    L10,L10,L10,L8,'\331'

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H',
                 COLUMN 001,'\033e\033(s218T\033(s21H\033(s7B'

      ON EVERY ROW
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H',
                 COLUMN 001,'\033e\033(s218T\033(s18H\033(s7B'

         CASE g_reg1.siefore
            WHEN 1
               LET l_reg99.xx_sum_pes1 = l_reg99.xx_sum_pes1 +
                                         g_reg1.monto_en_acciones
               LET l_reg99.xx_sum_acc1 = l_reg99.xx_sum_acc1 +
                                         g_reg1.monto_pesos_act
            WHEN 2
               LET l_reg99.xx_sum_pes2 = l_reg99.xx_sum_pes2 +
                                         g_reg1.monto_en_acciones
               LET l_reg99.xx_sum_acc2 = l_reg99.xx_sum_acc2 +
                                         g_reg1.monto_pesos_act
            WHEN 3
               LET l_reg99.xx_sum_pes3 = l_reg99.xx_sum_pes3 +
                                         g_reg1.monto_en_acciones
               LET l_reg99.xx_sum_acc3 = l_reg99.xx_sum_acc3 +
                                         g_reg1.monto_pesos_act
            WHEN 4
               LET l_reg99.xx_sum_pes4 = l_reg99.xx_sum_pes4 +
                                         g_reg1.monto_en_acciones
               LET l_reg99.xx_sum_acc4 = l_reg99.xx_sum_acc4 +
                                         g_reg1.monto_pesos_act
            WHEN 5
               LET l_reg99.xx_sum_pes5 = l_reg99.xx_sum_pes5 +
                                         g_reg1.monto_en_acciones
               LET l_reg99.xx_sum_acc5 = l_reg99.xx_sum_acc5 +
                                         g_reg1.monto_pesos_act
            WHEN 11
               LET l_reg99.xx_sum_pes11 = l_reg99.xx_sum_pes11 +
                                          g_reg1.monto_en_acciones
               LET l_reg99.xx_sum_acc11 = l_reg99.xx_sum_acc11 +
                                          g_reg1.monto_pesos_act
         END CASE

         PRINT   COLUMN 03,g_reg1.folio USING "<<<<<<",
                 COLUMN 10,g_reg1.nss,
                 COLUMN 18,g_reg1.subcuenta,
                 COLUMN 40,g_reg1.siefore,
                 COLUMN 55,g_reg1.monto_en_acciones,
                 COLUMN 82,g_reg1.monto_pesos_act,
                 COLUMN 110,g_reg1.monto_pesos_his,
                 COLUMN 135,g_reg1.monto_comision,
                 COLUMN 153,g_reg1.monto_plusvalia

      PAGE TRAILER
         SKIP 3 LINE
         PRINT COLUMN 90," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE

         PRINT COLUMN 25,"Subtotal: ",
               COLUMN 45,"Siefore 1 ",
               COLUMN 54,l_reg99.xx_sum_pes1,
               COLUMN 82,l_reg99.xx_sum_acc1

         PRINT COLUMN 45,"Siefore 2 ",
               COLUMN 54,l_reg99.xx_sum_pes2,
               COLUMN 82,l_reg99.xx_sum_acc2
               
         PRINT COLUMN 45,"Siefore 3 ",
               COLUMN 54,l_reg99.xx_sum_pes3,
               COLUMN 82,l_reg99.xx_sum_acc3
         
         PRINT COLUMN 45,"Siefore 4 ",
               COLUMN 54,l_reg99.xx_sum_pes4,
               COLUMN 82,l_reg99.xx_sum_acc4
               
         PRINT COLUMN 45,"Siefore 5 ",
               COLUMN 54,l_reg99.xx_sum_pes5,
               COLUMN 82,l_reg99.xx_sum_acc5

         PRINT COLUMN 45,"Siefore 11",
               COLUMN 54,l_reg99.xx_sum_pes11,
               COLUMN 82,l_reg99.xx_sum_acc11

         SKIP 2 LINE
         PRINT COLUMN 001,'\033e\033(s218T\033(s18H\033(s7B'

         PRINT COLUMN 25,"TOTAL: ",
               COLUMN 56,SUM(g_reg1.monto_en_acciones) USING "---------&.&&&&&&",
               COLUMN 83,SUM(g_reg1.monto_pesos_act)   USING "---------&.&&&&&&",
               COLUMN 111,SUM(g_reg1.monto_pesos_his)  USING "---------&.&&&&&&",
               COLUMN 137,SUM(g_reg1.monto_comision)   USING "---------&.&&&&&&",
               COLUMN 154,SUM(g_reg1.monto_plusvalia)  USING "---------&.&&&&&&"

         SKIP 2 LINE
         PRINT COLUMN 03," Total de Registros: ",COUNT(*) USING "<<<<"

END REPORT
#########################################################################
FUNCTION suma_dias_habiles(diaActual,dia_hab)

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE,
          numDias       SMALLINT,
          dia_hab       SMALLINT

   LET diaTmp = diaActual

   FOR contador = 1 TO dia_hab
      IF contador = 1 THEN
         CALL habil_siguiente (diaTmp)
              RETURNING diaTmp
      ELSE
         LET diaTmp = diaTmp + 1 UNITS DAY

         CALL habil_siguiente (diaTmp)
              RETURNING diaTmp
      END IF
   END FOR

   RETURN diaTmp

END FUNCTION
#########################################################################
FUNCTION habil_siguiente(diaActual)

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE

   DEFINE diaHabilSig   DATE,
          diaSemana     SMALLINT,
          feriado       SMALLINT,
          finSemana     SMALLINT

   LET diaHabilSig = diaActual

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilSig)

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF

      SELECT *
      FROM   tab_feriado
      WHERE  feria_fecha = diaHabilSig

      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF

      IF feriado = 1 OR finSemana = 1 THEN
         LET diaHabilSig = diaHabilSig + 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilSig

END FUNCTION
###############################################################################
FUNCTION deposito_archivo()

   DEFINE sum1  RECORD
          aceptados_tot       DECIMAL(16,2),
          monto_ret           DECIMAL(16,2),
          monto_act_ret       DECIMAL(16,2),
          monto_ces_vej_pat   DECIMAL(16,2),
          monto_ces_vej_tra   DECIMAL(16,2),
          monto_act_ces_vej   DECIMAL(16,2),
          monto_par_viv       DECIMAL(16,6),
          monto_comi_ret      DECIMAL(16,2),
          monto_comi_ces_vej  DECIMAL(16,2),
          monto_plus_ret      DECIMAL(16,2),
          monto_plus_ces_vej  DECIMAL(16,2),
          monto_plus_pat      DECIMAL(16,2),
          monto_min_ret       DECIMAL(16,2),
          monto_min_ces_vej   DECIMAL(16,2)
   END RECORD

   DEFINE x_fecha                DATE,
          x_origen               CHAR(10),
          x_folio                INTEGER,
          x_precio_acc_dia       DECIMAL(16,6),
          x_monto_en_pesos       DECIMAL(16,6),
          x_fecha_valor          DATE

   DEFINE x_nss                  CHAR(11),
          x_consec_reg_lote      INTEGER,
          x_subcuenta            SMALLINT,
          x_tipo_movimiento      SMALLINT,
          x_monto_en_acciones    DECIMAL(16,6),
          x_total_plus_ret       DECIMAL(16,2),
          x_total_plus_ces       DECIMAL(16,2),
          xxx_total_plus_ret     DECIMAL(16,2),
          xxx_total_plus_ces     DECIMAL(16,2)

   DEFINE cla_sel                CHAR(200),
          vfolio                 INTEGER

   DEFINE vmonto_sub_rcv         DECIMAL(16,2),
          vmonto_sub_rcv2        DECIMAL(16,2),
          vmonto_sub_rcv3        DECIMAL(16,2),
          vmonto_sub_rcv4        DECIMAL(16,2),
          vmonto_sub_rcv5        DECIMAL(16,2),
          vmonto_sub_viv         DECIMAL(16,6),
          x_acc_plus_ret,
          x_acc_plus_ces_vej,
          x_precio_accion_ini    DECIMAL(16,6),
          ini_monto_plus_ret,
          ini_monto_plus_ces,
          fin_monto_plus_ret,
          fin_monto_plus_ces     DECIMAL(16,6),
          total_plus_ret,
          total_plus_ces         DECIMAL(16,2)

   DEFINE total_reg              SMALLINT

   DEFINE x_mes                  INTEGER,
          x_ano                  INTEGER,
          dias                   INTEGER,
          hoy2                   DATE,
          xx_fecha               DATE,
          x_fecha_proceso        DATE

   DEFINE x_siefore          SMALLINT,
	  x_registros        SMALLINT,
	  x_acciones         DECIMAL(16,6)

   DEFINE x_sub_acep         DECIMAL(16,2),
          x_sub_plu          DECIMAL(16,2),
          x_sub_comi         DECIMAL(16,2),
          sel_txt            CHAR(1000)

   OPEN WINDOW ventana_5 AT 6,3 WITH FORM "EXCC0026" ATTRIBUTE( BORDER)
   DISPLAY " (Enter) Consulta                                             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "                        REGISTRO 08 DE PAGOS EN EXCESO                          " AT 3,1 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   LET x_folio = NULL
   LET x_fecha = NULL

   INPUT BY NAME x_folio,
                 x_fecha WITHOUT DEFAULTS

      AFTER FIELD x_folio
         IF x_folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD x_folio
         END IF

         SELECT fecha_proceso
         INTO   x_fecha_proceso
         FROM   dis_ctrl_proceso
         WHERE  folio = x_folio
         AND    proceso_cod = "EXC"
         AND    etapa_cod = 1

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "FOLIO NO EXISTE,VERIFICAR POR FAVOR" ATTRIBUTE (REVERSE)
                    FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD x_folio
         END IF

      AFTER FIELD x_fecha
         IF x_fecha IS NULL THEN
            ERROR "Fecha de presio de accion no puede ser nula"
            NEXT FIELD x_fecha
         END IF
{
         LET x_mes = MONTH(x_fecha_proceso)
         LET x_ano = YEAR(x_fecha_proceso)

         LET hoy2 = MDY(x_mes,1,x_ano)
         LET dias = 14

         CALL suma_dias_habiles(hoy2,dias)
              RETURNING xx_fecha

         IF x_fecha > xx_fecha THEN
            PROMPT "SOBRE PASO LA FECHA DE LIQUIDACION DEL ARCHIVO " ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD x_fecha
         END IF
}
         SELECT "X"
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = x_fecha
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "PRECIO ACCION NO EXISTE,VERIFICAR POR FAVOR" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD x_fecha
         END IF

         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_5
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION"


   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_exc_dep_exceso
   WHENEVER ERROR STOP

   CALL genera_tmp_cuenta (x_folio)

   DECLARE cur_cuenta CURSOR FOR
   SELECT a.siefore,
          SUM(ROUND(a.monto_en_acciones,2))*-1,
	  COUNT(UNIQUE a.nss)
   FROM   tmp_dis_cuenta a
   WHERE  a.folio = x_folio
   GROUP BY 1
   ORDER BY 1

   LET total_reg = 0
   LET x_registros = 0

   LET x_sub_acep = 0
   LET x_sub_plu = 0
   LET x_sub_comi = 0

   FOREACH cur_cuenta INTO x_siefore,
                           x_acciones,
			   x_registros

      IF x_siefore <> 11 THEN
         LET sel_txt = "SELECT SUM(ROUND(monto_ret,2) + ",
                               " ROUND(monto_act_ret,2) + ",
                               " ROUND(monto_ces_vej_pat,2) + ",
                               " ROUND(monto_act_ces_vej,2)) ",
                       " FROM   exc_det_exceso ",
                       " WHERE  folio =", x_folio ,
                       " AND    nss IN (SELECT UNIQUE a.nss ",
                                       "FROM tmp_dis_cuenta a ",
                                       "WHERE a.folio =", x_folio,
                                       "AND  a.siefore =", x_siefore," ) ",
                       " AND    result_operacion = '01' "

         PREPARE exe_sel1 FROM sel_txt

         DECLARE cursor_11 CURSOR FOR exe_sel1

         FOREACH cursor_11 INTO x_sub_acep
         END FOREACH

         LET sel_txt = "SELECT SUM(ROUND(monto_plus_ret,2) + ",
                               "ROUND(monto_plus_ces_vej,2)) ",
                       "FROM   exc_exceso_plu_min ",
                       "WHERE  folio = ",x_folio,
                       "AND    nss IN (SELECT UNIQUE a.nss ",
                                      "FROM tmp_dis_cuenta a ",
                                      "WHERE a.folio = ",x_folio,
                                      "AND  a.siefore = ",x_siefore," )"

         PREPARE exe_sel2 FROM sel_txt
         DECLARE cursor_12 CURSOR FOR exe_sel2

         FOREACH cursor_12 INTO x_sub_plu
         END FOREACH

         LET sel_txt = "SELECT SUM(ROUND(monto_comi_ret,2) + ",
                              "ROUND(monto_comi_ces_vej,2))*-1 ",
                       "FROM   exc_exceso_comis ",
                       "WHERE  folio = ",x_folio,
                       "AND    nss IN (SELECT UNIQUE a.nss ",
                                      "FROM   tmp_dis_cuenta a ",
                                      "WHERE  a.folio = ",x_folio,
                                      "AND    a.siefore = ",x_siefore," )"
         PREPARE exe_sel3 FROM sel_txt
         DECLARE cursor_13 CURSOR FOR exe_sel3

         FOREACH cursor_13 INTO x_sub_comi
         END FOREACH
      ELSE
         SELECT precio_del_dia
         INTO   x_precio_acc_dia
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = x_fecha
         AND    codigo_siefore = x_siefore
      END IF

      CASE x_siefore
	       WHEN 1 LET vmonto_sub_rcv  = x_sub_acep + x_sub_plu + x_sub_comi
	       WHEN 2 LET vmonto_sub_rcv2 = x_sub_acep + x_sub_plu + x_sub_comi
	       WHEN 3 LET vmonto_sub_rcv3 = x_sub_acep + x_sub_plu + x_sub_comi
	       WHEN 4 LET vmonto_sub_rcv4 = x_sub_acep + x_sub_plu + x_sub_comi
	       WHEN 5 LET vmonto_sub_rcv5 = x_sub_acep + x_sub_plu + x_sub_comi
	       WHEN 11 LET vmonto_sub_viv = x_acciones
      END CASE
      LET total_reg = total_reg + x_registros
   END FOREACH

   DISPLAY BY NAME total_reg,
                   vmonto_sub_rcv,
                   vmonto_sub_rcv2,
                   vmonto_sub_rcv3,
                   vmonto_sub_rcv4,
                   vmonto_sub_rcv5,
                   vmonto_sub_viv

   ERROR ""

   PROMPT "PRESIONE ENTER PARA SALIR " FOR opc

   CLOSE WINDOW ventana_5

END FUNCTION
###############################################################################
FUNCTION genera_tmp_cuenta (p_folio)
   DEFINE p_folio          INTEGER,
          v_nombre_tabla   CHAR(20),
          sel_his          CHAR(2000),
          tot_registros    SMALLINT,
          x_cuantos        INTEGER

   SELECT count(*)
   INTO   x_cuantos
   FROM   dis_cuenta
   WHERE  folio = p_folio

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta;
   WHENEVER ERROR STOP

   IF x_cuantos > 0 THEN
   LET sel_his = " SELECT a.* ",
               " FROM   dis_cuenta a ",
               " WHERE a.nss IN(SELECT  b.nss FROM exc_det_exceso b ",
                               " WHERE b.folio = ",p_folio,")",
               " AND    a.subcuenta IN (1,2,4) ",
               " AND    a.tipo_movimiento BETWEEN 540 AND 555 ",
               " INTO TEMP tmp_dis_cuenta " CLIPPED

   LET sel_his = sel_his CLIPPED

   PREPARE eje_sel_his FROM sel_his

   EXECUTE eje_sel_his

ELSE
   LET sel_his = " SELECT a.* ",
                 " FROM   dis_provision a ",
                 " WHERE a.nss IN(SELECT  b.nss FROM exc_det_exceso b ",
                 " WHERE b.folio = ",p_folio,")",
                 " AND    a.subcuenta IN (1,2,4) ",
                 " AND    a.tipo_movimiento BETWEEN 540 AND 555 ",
                 " INTO TEMP tmp_dis_cuenta " CLIPPED

    LET sel_his = sel_his CLIPPED

    PREPARE eje_sel_his1 FROM sel_his

    EXECUTE eje_sel_his1

END IF

   CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta(folio,
                                                  subcuenta,
                                                  tipo_movimiento
                                                 )
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION
######################################################################
