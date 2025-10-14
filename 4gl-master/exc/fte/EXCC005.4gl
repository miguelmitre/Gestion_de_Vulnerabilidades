###########################################################################
#Proyecto               => Sistema de Afores. (MEXICO)                    #
#Propietario            => E.F.P                                          #
#Programa     EXCC005   => Consulta de  pagos en exceso  ISSSTE           #
#Creado por             => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Fecha creacion         => 11 de febrero de 2010.                         #
#Modulo                 => EXC.                                           #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE x_folio                   INTEGER,
          hoy                       DATE,
          usuario                   CHAR(8),
          vruta_listado             CHAR(40),
          nss                       CHAR(8),
          g_lista                   CHAR(100),
          g_impre                   CHAR(100),
          vtipo_rechazo             SMALLINT,
          vtipo_rechazo2            SMALLINT,
          opc                       CHAR(1)

   DEFINE l_reg ARRAY[10000] OF RECORD
          trag                      CHAR(1),
          consec_reg_lote           INTEGER,
          folio                     INTEGER,
          curp                      CHAR(18),
          nss                       CHAR(11),
          clave_ent_orig            CHAR(3),
          result_operacion          CHAR(2),
          result_desc               CHAR(10),
          tipo_diagnostico          CHAR(3)
   END RECORD
   
   DEFINE l_reg1 ARRAY[10000] OF RECORD
          impt_sar_issste           DECIMAL(18,2),
          impt_ret_issste           DECIMAL(18,2),
          impt_cv_patron            DECIMAL(18,2),
          impt_ahorro_solid         DECIMAL(18,2),
          impt_fondo_viv92          DECIMAL(18,2),
          aplic_int_fondo_viv92     DECIMAL(18,6),
          impt_fondo_viv08          DECIMAL(18,2),
          aplic_int_fondo_viv08     DECIMAL(18,6)
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
          curp               CHAR(18),
          nss                CHAR(11),
          consec_reg_lote    SMALLINT,
          impt_sar_issste    DECIMAL(18,2),
          impt_ret_issste    DECIMAL(18,2),
          impt_cv_patron     DECIMAL(18,2),
          impt_ahorro_solid  DECIMAL(18,2),
          impt_fondo_viv92   DECIMAL(18,2),
          impt_fondo_viv08   DECIMAL(18,2)
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
          curp               CHAR(18),
          nss                CHAR(11),
          subcuenta          SMALLINT,
          siefore            SMALLINT,
          monto_en_acciones  DECIMAL(16,6),
          monto_pesos_act    DECIMAL(16,6),
          monto_pesos_sol    DECIMAL(16,2)
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
          monto_en_acciones     DECIMAL(16,6),
          monto_pesos_sol       DECIMAL(16,2)
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

   OPEN WINDOW v1 AT 3,2 WITH FORM "EXCC0051" ATTRIBUTE (BORDER)
   DISPLAY " EXCC005               CONSULTA DE PAGOS EN EXCESO ISSSTE                           " AT 3,1 ATTRIBUTE (REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE (REVERSE)

   CALL inicio()

   MENU "CONSULTAS"
      COMMAND "Resultado de Operación" "Pagos en exceso ISSSTE"
         CALL proceso()
         
      COMMAND KEY (O) "prOvision" "Pagos en exceso provision ISSSTE"
         CALL proceso_provision(4)

      COMMAND KEY (E) "prE-liquidacion" "Pre-liquidacion de pagos en exceso ISSSTE"
         CALL proceso_preliq(6)

      COMMAND "Liquidados" "Liquidacion de pagos en exceso ISSSTE"
         CALL proceso_cuenta(5)

      COMMAND KEY (G) "reGistro 08" "Montos en lo(s) registro(s) 08 ISSSTE"
         CALL deposito_archivo()

      COMMAND "Salir" "Salir de menu de consultas"
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
FUNCTION proceso()

   DEFINE vtipo_rechazo      SMALLINT,
          x_resul_operacion  CHAR(2)

   DEFINE sql_stat            INTEGER,
          item_row_cnt        SMALLINT,
          cont_inp            SMALLINT,
          cur_row             SMALLINT,
          scr_row             SMALLINT,
          p_cur_row           SMALLINT
          
   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,2 WITH FORM "EXCC0052" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrol-p) Impresion            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                     DETALLE PAGOS EN EXCESO ISSSTE                             " AT 5,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON folio,
                             result_operacion,
                             curp,
                             clave_ent_orig
                        FROM x_folio,
                             x_result_operacion,
                             x_curp,
                             x_clave_ent_orig

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

      LET sel_where = " SELECT ' ',",
                             " consec_reg_lote,",
                             " folio,",
                             " curp,",
                             " nss,",
                             " clave_ent_orig,",
                             " result_operacion,",
                             " CASE result_operacion ",
                             "    WHEN '01' THEN 'ACEPTADOS' ",
                             "    WHEN '02' THEN 'RECHAZADOS' ",
                             "    WHEN '03' THEN 'PENDIENTES' ",
                             "    WHEN '04' THEN 'PARCIALES' ",
                             " END AS result_des ,",
                             " tipo_diagnostico ",
                      " FROM   exc_det_exceso_issste ",
                      " WHERE  ",cla_where CLIPPED,
                      " ORDER BY 2,6,7,9,4 "

      PREPARE exe_sel FROM sel_where

      DECLARE cursor_1 CURSOR FOR exe_sel

      LET pos = 1

      FOREACH cursor_1 INTO l_reg[pos].*
         LET pos = pos +1
      END FOREACH

      LET pos = pos - 1
      IF (pos) >= 1 THEN
         CALL SET_COUNT(pos)
      
         LET cont_inp = TRUE
      
         WHILE (cont_inp = TRUE)
      
            INPUT ARRAY l_reg WITHOUT DEFAULTS FROM scr_1.*
            ATTRIBUTES(MAXCOUNT = pos,COUNT = pos) 
      
               AFTER ROW
                  LET cur_row = ARR_CURR()
                  LET scr_row = SCR_LINE()
      
                  DISPLAY l_reg[cur_row].* TO scr_1[scr_row].*
      
               BEFORE ROW
                  LET cur_row = ARR_CURR()
                  LET scr_row = SCR_LINE()
      
                  IF (cur_row = pos + 1) THEN
                     LET cont_inp = TRUE
                  ELSE
                     LET scr_row = SCR_LINE()
      
                     DISPLAY l_reg[cur_row].* TO scr_1[scr_row].*
                     ATTRIBUTE (REVERSE)
      
                     LET cont_inp = FALSE
      
                     CALL proc_items(cur_row)
                          RETURNING sql_stat,
                                    item_row_cnt
                  END IF
     
               ON KEY(CONTROL-V)
                  CALL disp_array_items(item_row_cnt)
     
               ON KEY (CONTROL-C)
                  EXIT INPUT
            END INPUT
         END WHILE
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "Registro inexistente ..."
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF
   END IF
END FUNCTION
###############################################################################
FUNCTION proceso_provision(vtipo_rechazo)

   DEFINE vtipo_rechazo  SMALLINT,
          x_fecha_pago   CHAR(8)

   DEFINE total_acciones_prel  DECIMAL(18,6),
          total_pesos_prel     DECIMAL(18,6),
          total_pesos_sol      DECIMAL(18,2)

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_3 AT 6,2 WITH FORM "EXCC0054" ATTRIBUTE (BORDER)
      DISPLAY " (Enter) Consulta                                             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
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
      SELECT a.curp,
             b.n_seguro,
             a.consec_reg_lote,
             a.impt_sar_issste,
             a.impt_ret_issste,
             a.impt_cv_patron,
             a.impt_ahorro_solid,
             a.impt_fondo_viv92,
             a.impt_fondo_viv08
      FROM   exc_det_exceso_issste a, afi_mae_afiliado b
      WHERE  a.folio = x_folio
      AND    a.result_operacion IN ("01","04")
      AND    a.curp = b.n_unico
      ORDER BY 3

      LET pos = 1
      LET total_acciones_prel = 0
      LET total_pesos_prel    = 0
      LET total_pesos_sol     = 0
      
      FOREACH cursor_his INTO l_reg5.*

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

         FOREACH cursor_pro INTO l_reg7.subcuenta,
                                 l_reg7.siefore,
                                 l_reg7.tipo_movimiento,
                                 l_reg7.monto_en_pesos,
                                 l_reg7.monto_en_acciones

            CASE
               WHEN l_reg7.subcuenta = 13 
                  LET l_reg7.monto_pesos_sol = l_reg5.impt_sar_issste
               WHEN l_reg7.subcuenta = 30
                  LET l_reg7.monto_pesos_sol = l_reg5.impt_ret_issste
               WHEN l_reg7.subcuenta = 31
                  LET l_reg7.monto_pesos_sol = l_reg5.impt_cv_patron
               WHEN l_reg7.subcuenta = 33
                  LET l_reg7.monto_pesos_sol = l_reg5.impt_ahorro_solid
               WHEN l_reg7.subcuenta = 14
                  LET l_reg7.monto_pesos_sol = l_reg5.impt_fondo_viv92
               WHEN l_reg7.subcuenta = 35
                  LET l_reg7.monto_pesos_sol = l_reg5.impt_fondo_viv08                  
            END CASE
            
            LET l_reg6[pos].curp              = l_reg5.curp
            LET l_reg6[pos].nss               = l_reg5.nss
            LET l_reg6[pos].subcuenta         = l_reg7.subcuenta
            LET l_reg6[pos].siefore           = l_reg7.siefore
            LET l_reg6[pos].monto_en_acciones = l_reg7.monto_en_acciones
            LET l_reg6[pos].monto_pesos_act   = l_reg7.monto_en_pesos
            LET l_reg6[pos].monto_pesos_sol   = l_reg7.monto_pesos_sol
            
            LET total_acciones_prel = total_acciones_prel + l_reg6[pos].monto_en_acciones
            LET total_pesos_prel    = total_pesos_prel + l_reg6[pos].monto_pesos_act
            LET total_pesos_sol     = total_pesos_sol + l_reg6[pos].monto_pesos_sol
            
            LET pos = pos + 1
         END FOREACH
      END FOREACH
                                                                     
      DISPLAY "Totale de Montos"  AT 17,22 ATTRIBUTE(REVERSE)                                                                             
      DISPLAY total_acciones_prel AT 17,42 ATTRIBUTE(REVERSE)        
      DISPLAY total_pesos_prel    AT 17,55 ATTRIBUTE(REVERSE)        
      DISPLAY total_pesos_sol     AT 17,71 ATTRIBUTE(REVERSE)         
      
      ERROR ""

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_reg6 TO scr_1.*

            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."

               LET vtipo_rechazo2 = vtipo_rechazo

               --CALL listado_pendientes(pos,vtipo_rechazo2)

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

   DEFINE subtotal_part            DECIMAL(19,14)

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_4 AT 6,2 WITH FORM "EXCC0055" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
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
               LET x_siefore = 12
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
                       " AND    subcuenta IN (13,30,31,33) ",
                       " AND    tipo_movimiento BETWEEN 540 AND 555 ",
                       " ORDER BY 2,3 "
      ELSE
         IF x_subcuenta = "VIV" THEN
            LET sel_where = sel_where CLIPPED,
                          " FROM   dis_provision ",
                          " WHERE folio = ",x_folio CLIPPED,
                          " AND   subcuenta IN (14,35) ",
                          " AND    tipo_movimiento BETWEEN 540 AND 555 ",
                          " ORDER BY 2,3 "
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

         IF l_reg3.siefore = 12 THEN
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
            WHEN 2
            	 LET precio_sie2   = xx_precio_del_dia
            	 LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones
            WHEN 3
            	 LET precio_sie3   = xx_precio_del_dia
            	 LET subtotal_sie3 = subtotal_sie3 + l_reg3.monto_en_acciones
            WHEN 4
            	 LET precio_sie4   = xx_precio_del_dia
            	 LET subtotal_sie4 = subtotal_sie4 + l_reg3.monto_en_acciones
            WHEN 5
            	 LET precio_sie5   = xx_precio_del_dia
            	 LET subtotal_sie5 = subtotal_sie5 + l_reg3.monto_en_acciones
            WHEN 12
            	 LET precio_sie11   = xx_precio_del_dia
            	 LET subtotal_sie11 = subtotal_sie11 + l_reg3.monto_en_acciones
         END CASE

         LET pos = pos +1
      END FOREACH

      DISPLAY "Siefore        1           2           3           4           5          12 " AT 13,1
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

               --CALL listado_pendientes(pos,vtipo_rechazo2)

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

   DEFINE subtotal_part            DECIMAL(19,14)

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,2 WITH FORM "EXCC0053" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                            PAGOS EN EXCESO  LIQUIDADOS                         " AT 3,1 ATTRIBUTE(REVERSE)

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
                       " AND    tipo_movimiento IN (543,544,553,554) ",
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

         IF l_reg3.siefore = 12 THEN
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
            WHEN 2
            	 LET precio_sie2   = xx_precio_del_dia
            	 LET subtotal_sie2 = subtotal_sie2 + l_reg3.monto_en_acciones
            WHEN 3
            	 LET precio_sie3   = xx_precio_del_dia
            	 LET subtotal_sie3 = subtotal_sie3 + l_reg3.monto_en_acciones
            WHEN 4
            	 LET precio_sie4   = xx_precio_del_dia
            	 LET subtotal_sie4 = subtotal_sie4 + l_reg3.monto_en_acciones
            WHEN 5
            	 LET precio_sie5   = xx_precio_del_dia
            	 LET subtotal_sie5 = subtotal_sie5 + l_reg3.monto_en_acciones
            WHEN 12
            	 LET precio_sie11   = xx_precio_del_dia
            	 LET subtotal_sie11 = subtotal_sie11 + l_reg3.monto_en_acciones
         END CASE

         LET pos = pos + 1
      END FOREACH

      DISPLAY "Siefore        1           2           3           4           5          12 " AT 13,1
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

               --CALL listado_pendientes(pos,vtipo_rechazo2)

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
{FUNCTION listado_pendientes(pos,vtipo_rechazo2)
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
   DEFINE li_folio INTEGER
   DEFINE lc_sql CHAR(1000)

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
         

         PRINT   COLUMN 03,g_reg1.folio USING "<<<<<<",
                 COLUMN 10,g_reg1.nss,
                 COLUMN 18,g_reg1.subcuenta,
                 COLUMN 40,g_reg1.siefore,
                 COLUMN 55,g_reg1.monto_en_acciones,
                 COLUMN 82,g_reg1.monto_pesos_act,
                 COLUMN 110,g_reg1.monto_pesos_his,
                 COLUMN 135,g_reg1.monto_comision,
                 COLUMN 153,g_reg1.monto_plusvalia
                 
         LET li_folio = g_reg1.folio

      PAGE TRAILER
         SKIP 3 LINE
         PRINT COLUMN 90," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
                  
         LET lc_sql = "SELECT  b.siefore,        ",
                      " SUM(b.monto_en_pesos),   ",
                      " SUM(b.monto_en_acciones) ",
                      "FROM    dis_provision b   ",
                      "WHERE   b.folio            = ", li_folio,
                      " AND     b.nss IN (SELECT UNIQUE nss ",
                                      "  FROM   exc_det_exceso ",
                                      "  WHERE  folio = " ,li_folio,
                                      "  AND    result_operacion = '01') ",
                      "GROUP BY 1 ",
                      "ORDER BY 1 "
         PREPARE st_001 FROM lc_sql
         
         DECLARE cur_subtotales CURSOR FOR st_001
         
         FOREACH cur_subtotales INTO g_reg1.siefore,
         	                           g_reg1.monto_pesos_act,
         	                           g_reg1.monto_en_acciones
         	  CASE g_reg1.siefore
                  WHEN 1
                     LET l_reg99.xx_sum_pes1 = l_reg99.xx_sum_pes1 +
                                               g_reg1.monto_pesos_act
                     LET l_reg99.xx_sum_acc1 = l_reg99.xx_sum_acc1 +
                                               g_reg1.monto_en_acciones
                  WHEN 2
                     LET l_reg99.xx_sum_pes2 = l_reg99.xx_sum_pes2 +
                                               g_reg1.monto_pesos_act
                     LET l_reg99.xx_sum_acc2 = l_reg99.xx_sum_acc2 +
                                               g_reg1.monto_en_acciones
                  WHEN 3
                     LET l_reg99.xx_sum_pes3 = l_reg99.xx_sum_pes3 +
                                               g_reg1.monto_pesos_act
                     LET l_reg99.xx_sum_acc3 = l_reg99.xx_sum_acc3 +
                                               g_reg1.monto_en_acciones
                  WHEN 4
                     LET l_reg99.xx_sum_pes4 = l_reg99.xx_sum_pes4 +
                                               g_reg1.monto_pesos_act
                     LET l_reg99.xx_sum_acc4 = l_reg99.xx_sum_acc4 +
                                               g_reg1.monto_en_acciones
                  WHEN 5
                     LET l_reg99.xx_sum_pes5 = l_reg99.xx_sum_pes5 +
                                               g_reg1.monto_pesos_act
                     LET l_reg99.xx_sum_acc5 = l_reg99.xx_sum_acc5 +
                                               g_reg1.monto_en_acciones
                  WHEN 11
                     LET l_reg99.xx_sum_pes11 = l_reg99.xx_sum_pes11 +
                                                g_reg1.monto_pesos_act
                     LET l_reg99.xx_sum_acc11 = l_reg99.xx_sum_acc11 +
                                                g_reg1.monto_en_acciones
            END CASE
         END FOREACH

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
}
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
          
   DEFINE reg_1 RECORD
   	      siefore              SMALLINT,
   	      subcuenta            SMALLINT,
   	      monto_en_pesos       DECIMAL(22,6),
   	      monto_en_acciones    DECIMAL(22,6)
   END RECORD
   
   DEFINE p_tabafore  RECORD  LIKE tab_afore_local.*
   
   DEFINE vsiefore_desc   CHAR(8),
          vestado         SMALLINT,
          qry1            CHAR(1000),
          pos             SMALLINT,
          x_precio_accion DECIMAL(18,6)
   
   DEFINE l_reg8 ARRAY[900] OF RECORD
          subcuenta            SMALLINT,
          subcuenta_desc       CHAR(17),
   	      monto_en_pesos       DECIMAL(22,6),
   	      siefore_cod          SMALLINT,
   	      siefore_desc         CHAR(8), 
   	      entidad              CHAR(10)
   END RECORD
   
   OPEN WINDOW ventana_5 AT 6,2 WITH FORM "EXCC0056" ATTRIBUTE( BORDER)
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
   
   SELECT *,
          USER 
   INTO   p_tabafore.*,
          usuario
   FROM   tab_afore_local
   
   SELECT estado
   INTO   vestado
   FROM   exc_dep_exceso_issste
   WHERE  folio          = x_folio
   GROUP BY 1
   
   IF vestado = 2 THEN
      LET qry1 = "SELECT siefore, ",
                        "DECODE(subcuenta, 30, 1, 31, 1, 14, 2, 13 , 3, 35, 4, 33, 7), ",
                        "SUM(monto_en_pesos), ",
                        "SUM(monto_en_acciones) ",
                 "FROM   dis_provision ",
                 "WHERE  folio = ",x_folio," ",
                 "GROUP BY 1,2 ",
                 "ORDER BY 1,2 " CLIPPED
   ELSE
      LET qry1 = "SELECT siefore, ",
                        "DECODE(subcuenta, 30, 1, 31, 1, 14, 2, 13 , 3, 35, 4, 33, 7), ",
                        "SUM(monto_en_pesos), ",
                        "SUM(monto_en_acciones) ",
                 "FROM   dis_cuenta ",
                 "WHERE  folio = ",x_folio," ",
                 "GROUP BY 1,2 ",
                 "ORDER BY 1,2 " CLIPPED
   END IF
   
   PREPARE claexe1 FROM qry1

   DECLARE cur_1 CURSOR FOR claexe1
   
   LET pos = 1
   
   FOREACH cur_1 INTO reg_1.*
   
      IF vestado = 2 THEN
      	 SELECT precio_del_dia
         INTO   x_precio_accion
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = x_fecha
         AND    codigo_siefore  = reg_1.siefore

         LET reg_1.monto_en_pesos   = reg_1.monto_en_acciones *
                                            x_precio_accion
      END IF
      
      SELECT siefore_desc
   	  INTO   vsiefore_desc
   	  FROM   tab_siefore
   	  WHERE  afore_cod   = p_tabafore.codigo_afore
   	  AND    siefore_cod = reg_1.siefore
       
      CASE reg_1.subcuenta
         WHEN 1 
            LET l_reg8[pos].entidad = "ISSSTE"
            LET l_reg8[pos].subcuenta_desc = "RCV ISSSTE"
         WHEN 3 
            LET l_reg8[pos].entidad = "ISSSTE"
            LET l_reg8[pos].subcuenta_desc = "RETIRO SAR ISSSTE"
         WHEN 7 
            LET l_reg8[pos].entidad = "ISSSTE"
            LET l_reg8[pos].subcuenta_desc = "AHORRO SOLIDARIO"
         WHEN 2 
            LET l_reg8[pos].entidad = "FOVISSSTE"
            LET l_reg8[pos].subcuenta_desc = "VIVIENDA 92"
         WHEN 4 
            LET l_reg8[pos].entidad = "FOVISSSTE"
            LET l_reg8[pos].subcuenta_desc = "VIVIENDA 08"
      END CASE
      
      LET l_reg8[pos].subcuenta      =  reg_1.subcuenta
      LET l_reg8[pos].monto_en_pesos =  reg_1.monto_en_pesos
      LET l_reg8[pos].siefore_cod    =  reg_1.siefore
      LET l_reg8[pos].siefore_desc   =  vsiefore_desc
      
      LET pos = pos + 1
   END FOREACH
   
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg8 TO scr_1.*

         ON KEY (control-p)
            ERROR "PROCESANDO IMPRESION..."

            LET vtipo_rechazo2 = vtipo_rechazo

            --CALL listado_pendientes(pos,vtipo_rechazo2)

         ON KEY (INTERRUPT)
            EXIT DISPLAY

         ON KEY (control-c)
            EXIT DISPLAY
      END DISPLAY
      CLOSE WINDOW ventana_5
   ELSE
      ERROR "Registro inexistente ..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana_5
   END IF
   
   CLEAR SCREEN

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
                               " WHERE b.folio = ",p_folio," AND result_operacion = '01')",
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
                 " WHERE b.folio = ",p_folio," AND result_operacion = '01')",
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
FUNCTION proc_items(p_cur_row)

   DEFINE sql_stat      INTEGER,
          p_cur_row     SMALLINT,
          item_row_cnt  SMALLINT

   CALL sel_ord_item(p_cur_row)
        RETURNING sql_stat,
                  item_row_cnt
 
   CALL disp_four_items()

   RETURN sql_stat,
          item_row_cnt

END FUNCTION
###############################################################
FUNCTION disp_array_items(p_item_row_cnt)

   DEFINE p_item_row_cnt SMALLINT

   CALL SET_COUNT(p_item_row_cnt)

   DISPLAY ARRAY l_reg1 TO scr_2.*
      ON KEY(ESC)
         EXIT DISPLAY
   END DISPLAY
END FUNCTION
#############################################################
FUNCTION sel_ord_item(p_cur_row)

   DEFINE sql_text   CHAR(650),
          sql_stat   INTEGER,
          p_cur_row  SMALLINT,
          row_cnt    SMALLINT,
          x_tablas   CHAR(20)

   CALL null_items()
   
   LET sql_text = " SELECT  impt_sar_issste,",       
                          " impt_ret_issste,",     
                          " impt_cv_patron,",        
                          " impt_ahorro_solid,",     
                          " impt_fondo_viv92,",      
                          " aplic_int_fondo_viv92,", 
                          " impt_fondo_viv08,",      
                          " aplic_int_fondo_viv08 ", 
                  " FROM  exc_det_exceso_issste ",
                  " WHERE folio = ? ",
                  " AND   curp = ? ",
                  " AND   consec_reg_lote = ? ",
                  " AND   clave_ent_orig = ? "

   PREPARE sel_item_stmt FROM sql_text
   DECLARE sel_item_curs CURSOR FOR sel_item_stmt

   WHENEVER ERROR CONTINUE

   OPEN sel_item_curs USING l_reg[p_cur_row].folio,
                            l_reg[p_cur_row].curp,
                            l_reg[p_cur_row].consec_reg_lote,
                            l_reg[p_cur_row].clave_ent_orig
   WHENEVER ERROR STOP

   LET sql_stat = SQLCA.SQLCODE

   LET row_cnt = 1

   WHILE ((NOT sql_stat) AND (row_cnt<= 1))

      WHENEVER ERROR CONTINUE

      FETCH sel_item_curs INTO l_reg1[row_cnt].*

      WHENEVER ERROR STOP

      LET sql_stat = SQLCA.SQLCODE

      IF (NOT sql_stat) THEN
         LET row_cnt = row_cnt + 1
      END IF
   END WHILE

   IF (sql_stat = 100) THEN
      LET sql_stat = 0
   END IF

   RETURN sql_stat,
          row_cnt - 1
END FUNCTION
################################################################
FUNCTION disp_four_items()

   DEFINE i SMALLINT

   FOR i = 1 TO 1
      DISPLAY l_reg1[i].* TO scr_2[i].*
   END FOR
END FUNCTION
###############################################################
FUNCTION null_items()

   DEFINE i SMALLINT

   INITIALIZE l_reg1[1].* TO NULL

   FOR i = 1 TO 100
      LET l_reg1[i].* = l_reg1[1].*
   END FOR
END FUNCTION
################################################################
