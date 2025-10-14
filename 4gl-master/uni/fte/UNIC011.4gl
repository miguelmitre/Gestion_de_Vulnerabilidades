###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa          => LIQUIDACION UNIFICACION EXTRA-AFORE                     #
#Fecha             => 22 de septiembre del 2000                               #
#Fecha Act         => 10 junio 2008                                           #
#Actualizado       => MIGUEL ANGEL HERNANDEZ MARTINEZ                         #
#Fecha Act         => 1 septiembre 2008                                       #
#Sistema           => UNI                                                     #
#CPL-1521          =>Sustituir arreglo por record
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE reg_3 RECORD
          nss_uni         CHAR(11),
          nss_cta1        CHAR(11),
          folio           INTEGER,
          cve_ent_cta1    CHAR(03),
          consecutivo1    INTEGER,
          estado          SMALLINT
   END RECORD

   DEFINE HOY,
          fecha_informe,
          fecha_liquida,
          fecha_liquida_viv,
          vfecha_inicio,
          vfecha_inicio1,
          vfecha_final,
          vfecha,
          vfecha1,
          vfinicio        DATE

   DEFINE enter           CHAR(1),
          char            CHAR(1),
          vpregunta       CHAR(1),
          usuario         CHAR(8),
          nss_unifi       CHAR(11),
          nss_unificador  CHAR(11),
          vdesmarca       CHAR(100),
          vsaldo          CHAR(100),
          vprov_cargo     CHAR(100),
          vprov_abono     CHAR(100),
          vliquida        CHAR(100),
          indevidas       CHAR(100),
          vid             CHAR(06),
          vtraspaso       CHAR(40),
          rliquida        CHAR(40)

   DEFINE vsubcuenta,
          vgrupo,
          vmarca_causa,
          vcorrelativo,
          vestado_marca   SMALLINT

   DEFINE codigo,
          tipo_mov,
          vextra_uni,
          vextra_cta1,
          vmov_aporte,
          vmov_interes,
          vmov_extra,
          vmov_extra_int,
          vdia,
          dia,
          dia1,
          marca,
          vfolio1,
          vfolio_sua,
          vfolio_liquida INTEGER

   DEFINE x_programa      CHAR(07),
          detecta         CHAR(150),
          vtransferencia  CHAR(100),
          status_ind      SMALLINT,
          vfn_regimen     CHAR(500),
          v_sql_1         CHAR(50)

   DEFINE v_existe,
          v_edad,
          v_criterio,
          v_ind_edad      SMALLINT,
          v_curp          CHAR(18),
          v_rfc           CHAR(13),
          v_fena          DATE,
          v_tipo_proc,
          v_tipo_trasp,
          v_medio,
          v_rechazo       SMALLINT,
          v_folioatencion INTEGER

   DEFINE sw_ind          SMALLINT

   DEFINE cam_siefore_rcv SMALLINT,
          inf_siefore_rcv SMALLINT,
          x_tipo_traspaso SMALLINT,
          x_medio         CHAR(2),
          x_existe        SMALLINT,
          x_rechazo       SMALLINT,
          x_folio         INTEGER
END GLOBALS
#####################################################################
MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT

   LET x_programa = "UNIC011"

   CALL STARTLOG ("UNIC011.log")
   CALL inicio()
   CALL proceso_principal()

END MAIN
#####################################################################
FUNCTION inicio()

   LET tipo_mov = 240
   LET HOY = TODAY

   SELECT codigo_afore,
          user
   INTO   codigo,
          usuario
   FROM   tab_afore_local

   LET vtraspaso      = 40  --- "TRASPASADO"
   LET rliquida       = 100 --- "LIQUIDADO"

   LET vmov_aporte    = 1   ---  "APORTACION"
   LET vmov_interes   = 4   --- "INTERES RECIBIDO"

   LET vmov_extra     = 242 --- "UNIFICACION EXTRA-AFORE"
   LET vmov_extra_int = 244 --- "UNIFICACION EXTRA-AFORE INTERES"

   LET vextra_uni     = 243 --- "UNIFICACION EXTRA-AFORE"
   LET vextra_cta1    = 244 --- "UNIFICACION EXTRA-AFORE U"

   LET vdesmarca     = " EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "

   LET vsaldo      = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
   PREPARE saldo_dia FROM vsaldo

   LET vprov_cargo = " EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
   PREPARE cargo     FROM vprov_cargo

   LET vprov_abono = " EXECUTE FUNCTION fn_prov_abono_sie (?,?,?,?,?,?,?,?,?,?,?) "
   PREPARE abono     FROM vprov_abono

   LET vliquida    = " EXECUTE FUNCTION fn_liquida (?,?,?,?,?) "
   PREPARE liquida   FROM vliquida

   LET indevidas   = " EXECUTE FUNCTION fn_sol_transf_sie (?,?,?,?,?,?,?) "
   PREPARE vindevidas FROM indevidas

   LET vtransferencia = " EXECUTE FUNCTION fn_ind_transferencia(?,?,?) "
   PREPARE transferencia FROM vtransferencia

   LET vfn_regimen = "EXECUTE PROCEDURE fn_regimen_inv(?,?,?,?,?,?)"
   PREPARE stmt2 FROM vfn_regimen

   LET v_sql_1 = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
   PREPARE fnacimiento FROM v_sql_1

   LET vcorrelativo  = 0
   LET vmarca_causa  = 0
   LET vestado_marca = 0

   LET  vfinicio = HOY
   LET  vdia     = DAY(vfinicio) -1
   LET  vfinicio = vfinicio - vdia UNITS DAY
   LET  vfecha1  = vfinicio

   LET detecta = " SELECT a.nss_cta1,",
                         "a.nss_uni,",
                         "a.folio,",
                         "a.estado ",
                 " FROM   uni_unificado a",
                 " WHERE  a.estado = 100",
                 " GROUP BY 1,2,3,4",
                 " ORDER BY 3"
   PREPARE actualiza FROM detecta

   CALL valida_regimen()

   LET fecha_liquida = habil_siguiente(vfecha1)

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_rehabilitada
      DROP TABLE tmp_solicitud_comple
      DROP TABLE tmp_indevidas
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_rehabilitada
      (nss CHAR(11) )

   CREATE TEMP TABLE tmp_solicitud_comple
      (folio    INTEGER,
       nss_uni  CHAR(11),
       nss_cta1 CHAR(11),
       estado   SMALLINT
      )

   CREATE TEMP TABLE tmp_indevidas
      (nss           CHAR(11),
       subcuenta     SMALLINT,
       siefore_ced   SMALLINT,
       siefore_rec   SMALLINT,
       tipo_traspaso SMALLINT,
       folio_liquida INTEGER,
       medio         SMALLINT
      )
END FUNCTION
#############################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0111" ATTRIBUTE(BORDER)
   DISPLAY "UNIC011                  UNIFICACION DE CUENTAS                                   " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "     [Esc]  Iniciar                                        [Ctrl-C]  Salir   " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   INPUT BY NAME vpregunta WITHOUT DEFAULTS

      AFTER FIELD vpregunta
         IF vpregunta IS NULL THEN
            ERROR "DEBE PONER ALGUNA OPCION PARA EJECUTAR"
            SLEEP 2
            NEXT FIELD vpregunta
         END IF

         IF vpregunta MATCHES '[Ss]' THEN

            CALL UNIC022(x_programa)         #actualiza cuentas traspasadas
            CALL actualiza_complementarias() #actualiza cuentas con estado
                                             #(95,96,100) a estado 60

            LET marca = 0

            SELECT "X"
            FROM   uni_unificador
            WHERE  estado  IN (40,95,96,100)
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               LET marca = 1
            END IF

            IF marca = 0  THEN
               ERROR "NO HAY CUENTAS POR UNIFICAR EXTRA-FORE"
               SLEEP 3
               EXIT PROGRAM
            END IF

            --IF fecha_liquida <> HOY THEN
            --   ERROR "ESTE PROCESO SOLO OPERA EL PRIMER DIA HABIL DEL MES"
            --   SLEEP 3
            --   ERROR ""
            --   EXIT PROGRAM
            --END IF

            ERROR " PROCESANDO INFORMACION "

            SELECT MAX(folio) + 1
            INTO   vfolio_liquida
            FROM   glo_folio

            IF vfolio_liquida IS NULL THEN
               LET vfolio_liquida = 1
            END IF

            INSERT INTO glo_folio VALUES(vfolio_liquida)

            DISPLAY "FOLIO LIQUIDACION    : ",vfolio_liquida  AT 18,1

            CALL liquidacion()

            ERROR ""

            PROMPT "PROCESO FINALIZADO...[Enter] para salir"
            FOR CHAR enter
            EXIT PROGRAM
         ELSE
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
#############################################
FUNCTION actualiza_complementarias()

   #CPL-1521
   #Sustituir arreglo por record
   --DEFINE reg_01 ARRAY[32700] OF RECORD
   DEFINE reg_01 RECORD
          nss_cta1 CHAR(11),
          nss_uni  CHAR(11),
          folio    INTEGER,
          estado   SMALLINT
   END RECORD

   DEFINE i           INTEGER ,
          vsubcuenta  SMALLINT,
          vgrupo      SMALLINT

   DEFINE xsubcuenta  SMALLINT,
          xsiefore    SMALLINT,
          xacciones   DECIMAL(18,6),
          xpesos      DECIMAL(18,6)

   LET i = 1
   LET vsubcuenta = 0
   LET vgrupo     = 0

   DECLARE query_com CURSOR FOR actualiza
   FOREACH query_com INTO reg_01.*
      DECLARE ss_saldo CURSOR FOR saldo_dia
      FOREACH ss_saldo USING reg_01.nss_cta1,
                            vsubcuenta,
                            vgrupo,
                            HOY
                      INTO  xsubcuenta,
                            xsiefore,
                            xacciones,
                            xpesos

         IF xacciones > 0 THEN

            SELECT "X"
            FROM   tmp_solicitud_comple
            WHERE  folio    = reg_01.folio
            AND    estado   = reg_01.estado
            AND    nss_uni  = reg_01.nss_uni
            AND    nss_cta1 = reg_01.nss_cta1

            IF SQLCA.SQLCODE <> 0 THEN
               INSERT INTO tmp_solicitud_comple
               VALUES (reg_01.folio,
                       reg_01.nss_uni,
                       reg_01.nss_cta1,
                       reg_01.estado
                      )
            END IF

            UPDATE uni_unificado
            SET    estado = 60
            WHERE  folio    = reg_01.folio
            AND    estado   = reg_01.estado
            AND    nss_uni  = reg_01.nss_uni
            AND    nss_cta1 = reg_01.nss_cta1
         ELSE
            IF xsubcuenta = 14 AND xpesos > 0 THEN
               SELECT "X"
               FROM   tmp_solicitud_comple
               WHERE  folio    = reg_01.folio
               AND    estado   = reg_01.estado
               AND    nss_uni  = reg_01.nss_uni
               AND    nss_cta1 = reg_01.nss_cta1

               IF SQLCA.SQLCODE <> 0 THEN
                  INSERT INTO tmp_solicitud_comple
                  VALUES (reg_01.folio,
                          reg_01.nss_uni,
                          reg_01.nss_cta1,
                          reg_01.estado
                         )
               END IF

               UPDATE uni_unificado
               SET    estado = 60
               WHERE  folio    = reg_01.folio
               AND    estado   = reg_01.estado
               AND    nss_uni  = reg_01.nss_uni
               AND    nss_cta1 = reg_01.nss_cta1
            END IF
         END IF

      END FOREACH
      --LET i = i + 1
   END FOREACH

END FUNCTION
#############################################
FUNCTION liquidacion()

   DEFINE total_pesos    DECIMAL(16,6),
          monto_ult_mes  DECIMAL(16,6),
          vinteres_per   DECIMAL(16,6),
          vinteres_mes   DECIMAL(16,6),
          vinteres       DECIMAL(16,6),
          xpesos_viv     DECIMAL(16,6),
          dia_cotizado   INTEGER,
          vclave_afore   INTEGER,
          vcontador      INTEGER,
          fecha_valor    DATE,
          fcert_uni      DATE,
          fcert_cta1     DATE,
          xsubcuenta     SMALLINT,
          xsiefore       SMALLINT,
          xacciones      DECIMAL(16,6),
          xpesos         DECIMAL(16,6)

   DEFINE cam_siefore_rcv SMALLINT,
          inf_siefore_rcv SMALLINT,
          x_tipo_traspaso SMALLINT,
          x_medio         CHAR(2),
          x_existe        SMALLINT,
          x_rechazo       SMALLINT,
          x_folio         INTEGER

   DEFINE opc             CHAR(01)

   DEFINE x_correlativo   INTEGER,
          x_extra_uni     SMALLINT,
          x_marca_causa   SMALLINT,
          sw              SMALLINT,
          x_marca_ret     SMALLINT

   DEFINE flag             SMALLINT,
          ind_siefore      SMALLINT,
          ind_decimos_nss  SMALLINT,
          ind_decimos_cta1 SMALLINT

   DEFINE xregimen         SMALLINT     --nvo121006

   DEFINE contcomp        INTEGER,
          contuni        INTEGER,
          contcta1       INTEGER

   LET dia               = DAY(fecha_liquida)
   LET vfecha_final      = fecha_liquida - dia UNITS DAY
   LET dia1              = DAY(vfecha_final) - 1
   LET vfecha_inicio     = vfecha_final  - dia1 UNITS DAY
   LET fecha_liquida_viv = fecha_liquida - dia UNITS DAY + 1 UNITS DAY
   LET fecha_informe     = vfecha_final  + 1 UNITS MONTH
   LET vfecha_inicio1    = vfecha_inicio + 1 UNITS DAY

   LET x_tipo_traspaso   = 4
   LET x_medio           = "10"

   LET ind_decimos_nss  = 0
   LET ind_decimos_cta1 = 0

   DECLARE cur_3 CURSOR FOR
   SELECT nss_uni,
          nss_cta1,
          folio,
          cve_ent_cta1,
          cont_servicio,
          estado
   FROM   uni_unificado
   WHERE  estado IN (40,60)
   AND    estado_traspaso >= 1
   AND    estado_unifica = 1

   LET contuni = 0
   LET contcta1 = 0
   LET contcomp = 0

   FOREACH cur_3 INTO reg_3.*
   	  
   	  #CPL-1653
   	  #No liquidar si falta algún unificado
   	  SELECT "X"
   	  FROM   uni_unificado
   	  WHERE  estado          = 40
   	  AND    estado_traspaso = 0
   	  AND    nss_uni         = reg_3.nss_uni
   	  AND    folio           = reg_3.folio
   	  GROUP BY 1
   	  
   	  IF SQLCA.SQLCODE = 0 THEN
         CONTINUE FOREACH
   	  END IF

      SELECT "X"
      FROM   uni_unificador
      WHERE  folio = reg_3.folio
      AND    nss_uni = reg_3.nss_uni
      AND    estado NOT IN (40,60,95,100)
      GROUP BY 1

      IF SQLCA.SQLCODE <> 0 THEN
         SELECT "X"
         FROM   uni_unificado
         WHERE  folio = reg_3.folio
         AND    nss_uni = reg_3.nss_uni
         AND    estado NOT IN (40,60,95,100)
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            LET vcontador = ""

            SELECT cont_servicio
            INTO   vcontador
            FROM   uni_unificador
            WHERE  nss_uni = reg_3.nss_uni
            AND    folio   = reg_3.folio
            GROUP BY 1

            LET sw = 0

            LET flag = 0
            LET ind_siefore = 1

            IF reg_3.estado = 40 THEN
               LET contcta1 = contcta1 + 1

--               DISPLAY "UNIFICADOS PROCESADOS : ",contcta1  AT 15,1
            ELSE
               LET contcomp = contcomp + 1

--               DISPLAY "UNIFICADOS COMPLEMENTARIOS PROCESADOS : ",contcomp  AT 16,1
            END IF

            DECLARE s_saldo CURSOR FOR saldo_dia
            FOREACH s_saldo USING reg_3.nss_cta1,
                                  vsubcuenta,
                                  vgrupo,
                                  HOY
                            INTO  xsubcuenta,
                                  xsiefore,
                                  xacciones,
                                  xpesos

               SELECT "X"
               FROM   cta_regimen
               WHERE  nss            = reg_3.nss_uni
               AND    codigo_siefore = xsiefore
               AND    subcuenta      = xsubcuenta
               GROUP BY 1

               IF SQLCA.SQLCODE = 0 THEN
                  LET xregimen = 0
               ELSE
                  LET xregimen = 1

                  LET inf_siefore_rcv = xsiefore

                  SELECT codigo_siefore
                  INTO   cam_siefore_rcv
                  FROM   cta_regimen
                  WHERE  nss            =  reg_3.nss_uni
                  AND    codigo_siefore <> xsiefore
                  AND    subcuenta      =  xsubcuenta
                  GROUP BY 1
               END IF

               IF xacciones > 0 THEN
                  CALL liquida_cuenta(reg_3.nss_cta1,
                                      xsubcuenta,
                                      xsiefore,
                                      xacciones,
                                      xpesos,
                                      reg_3.cve_ent_cta1, --- afore cedente
                                      reg_3.estado,
                                      reg_3.consecutivo1)

                  CALL integra_cuenta(reg_3.nss_uni,
                                      xsubcuenta,
                                      xsiefore,
                                      xacciones,
                                      xpesos,
                                      reg_3.cve_ent_cta1, --- afore cedente
                                      reg_3.estado,
                                      vcontador)

                  LET sw = 1

                  IF xsubcuenta = 4 OR
                  	 xsubcuenta = 8 OR
                  	 xsubcuenta = 14 OR
                  	 xsubcuenta = 35 THEN
                     LET flag = 0
                  ELSE
                     LET flag = 1
                  END IF

                  IF xsiefore = 2 THEN
                     LET ind_siefore = 1
                  END IF
               ELSE
               	{
                  IF xsubcuenta = 14 AND xpesos > 0 THEN
                     CALL liquida_cuenta(reg_3.nss_cta1,
                                         xsubcuenta,
                                         xsiefore,
                                         xacciones,
                                         xpesos,
                                         reg_3.cve_ent_cta1, --- afore cedente
                                         reg_3.estado,
                                         reg_3.consecutivo1)

                     CALL integra_cuenta(reg_3.nss_uni,
                                         xsubcuenta,
                                         xsiefore,
                                         xacciones,
                                         xpesos,
                                         reg_3.cve_ent_cta1, --- afore cedente
                                         reg_3.estado,
                                         vcontador)
                  LET sw = 1
                  END IF
                 }
               END IF

               IF flag = 1 AND xregimen = 1 THEN
                  SELECT "X"
                  FROM   tes_solicitud
                  WHERE  nss             = reg_3.nss_uni
                  AND    folio_solicitud = vfolio_liquida
                  AND    estado          = 100
                  GROUP BY 1

                  IF SQLCA.SQLCODE <> 0 THEN

                     INSERT INTO tmp_indevidas
                     VALUES(reg_3.nss_uni,   --nss
                            xsubcuenta,      --subcuenta
                            inf_siefore_rcv, --siefore_ced
                            cam_siefore_rcv, --siefore_rec
                            x_tipo_traspaso, --tipo traspaso  = 4
                            vfolio_liquida,  --folio liquidacion
                            x_medio          --medio = 10
                            )
                  END IF
               END IF

               LET  xsubcuenta = 0
               LET  xsiefore   = 0
               LET  xacciones  = 0
               LET  xpesos     = 0
            END FOREACH

            IF sw = 1 THEN
               SELECT COUNT(*),
                      correlativo
               INTO   x_marca_ret,
                      x_correlativo
               FROM   cta_act_marca
               WHERE  nss = reg_3.nss_uni
               AND    marca_cod = 140
               GROUP BY 2

               IF  x_marca_ret = 1 THEN

                   LET x_extra_uni = 140
                   LET x_marca_causa = 243

                   PREPARE marcaje_ret FROM vdesmarca
                   EXECUTE marcaje_ret USING reg_3.nss_uni, # nss
                                         x_extra_uni,       # marca_entra
                                         x_correlativo,     # correlativo
                                         vestado_marca,     # estado_marca
                                         x_marca_causa,     # marca_causa
                                         usuario            # usuario

                   INSERT INTO tmp_rehabilitada
                   VALUES (reg_3.nss_uni)
               END IF
            END IF

            IF reg_3.estado = 60 THEN
               INSERT INTO uni_complementario
               VALUES (vfolio_liquida,      --- folio_liquida
                       reg_3.nss_cta1,      --- nss_cta1
                       reg_3.nss_uni,       --- nss_uni
                       hoy,               --- fliquida
                       USER                 --- usuario
                      )

               CALL actualiza_complemento(reg_3.nss_uni,reg_3.nss_cta1)
            ELSE
               CALL actualiza_control(reg_3.nss_uni,reg_3.nss_cta1)
            END IF
            CALL ind_transferencia(reg_3.nss_cta1)   --se agrega 270206
         END IF
      END IF

   END FOREACH

   CALL registra_liq()
   CALL fn_indevidas()         -- ingreso 10-06-2008
   CALL rehabilita_cuenta()

   UPDATE STATISTICS FOR TABLE uni_complementario

   #realizar los ajustes de revalorización de bono
   CALL verifica_bono(vfolio_liquida, usuario)

END FUNCTION
#####################################################################
FUNCTION fn_indevidas()

   DEFINE reg_ind RECORD
          nss           CHAR(11),
          subcuenta     SMALLINT,
          siefore_ced   SMALLINT,
          siefore_rec   SMALLINT,
          tipo_traspaso SMALLINT,
          folio_liquida INTEGER,
          medio         SMALLINT
   END RECORD

   DECLARE cur_indevidas CURSOR FOR
   SELECT *
   FROM   tmp_indevidas

   LET sw_ind = 0

   FOREACH cur_indevidas INTO reg_ind.*
      DECLARE s_indevidas CURSOR FOR vindevidas
      OPEN    s_indevidas USING reg_ind.nss,
                                reg_ind.subcuenta,
                                reg_ind.siefore_ced,
                                reg_ind.siefore_rec,
                                reg_ind.tipo_traspaso,
                                reg_ind.folio_liquida,
                                reg_ind.medio
         FETCH s_indevidas INTO x_existe,
                                x_rechazo,
                                x_folio

         IF x_rechazo = 0 THEN
            LET sw_ind = sw_ind + 1
         END IF
      CLOSE   s_indevidas
   END FOREACH
END FUNCTION
#####################################################################
FUNCTION ind_transferencia(nss_cta1)

   DEFINE x_ind_transferencia  SMALLINT,
          nss_cta1             CHAR(11)

   LET x_ind_transferencia = 7

   DECLARE s_indicador CURSOR FOR transferencia
   OPEN    s_indicador USING nss_cta1           , ---nss
                             x_ind_transferencia, ---ind_transferencia
                             fecha_liquida        ---fecha_ind_transf
      FETCH s_indicador INTO status_ind
   CLOSE   s_indicador

END FUNCTION
#####################################################################
FUNCTION liquida_cuenta(reg_4)

   DEFINE reg_4 RECORD
          nss               CHAR(11),
          subcuenta         INTEGER,
          siefore           SMALLINT,
          acciones          DECIMAL(16,6),
          pesos             DECIMAL(16,6),
          cedente           CHAR(03),
          estado            SMALLINT,
          consecutivo       INTEGER
   END RECORD

   DEFINE xcargo        SMALLINT
   DEFINE xliq          SMALLINT

   DEFINE opc           CHAR(1)

   LET reg_4.pesos    = reg_4.pesos    * -1
   LET reg_4.acciones = reg_4.acciones * -1
{
   IF  reg_4.subcuenta = 14 THEN
      LET  reg_4.acciones    = 0
   END IF
}
   IF (reg_4.subcuenta = 3 OR reg_4.subcuenta = 10) THEN
      UPDATE cta_saldo_vol
      SET    saldo_acciones = 0,
             fecha_saldo    = fecha_liquida,
             usuario        = usuario
      WHERE  nss            = reg_4.nss
      AND    subcuenta      = reg_4.subcuenta
      AND    fecha_saldo    <= fecha_liquida
   END IF

   IF reg_4.estado = 60 THEN
      LET vid = "UC-",reg_4.cedente USING "&&&" CLIPPED
   ELSE
      LET vid = "UN-",reg_4.cedente USING "&&&" CLIPPED
   END IF

   DECLARE s_cargo CURSOR FOR cargo
   OPEN    s_cargo USING vfolio_liquida,
                         vfolio_sua,
                         reg_4.nss,
                         reg_4.subcuenta,
                         vmov_extra,
                         reg_4.consecutivo,
                         reg_4.siefore,
                         reg_4.acciones,
                         reg_4.pesos,
                         vid,
                         HOY

   FETCH   s_cargo INTO  xcargo
   CLOSE   s_cargo

   IF xcargo < 0 THEN
      ERROR "No se provisiono la subcuenta ",reg_4.subcuenta CLIPPED,
            " del NSS: ",reg_4.nss CLIPPED," del folio ",vfolio_liquida CLIPPED
      SLEEP 3
      EXIT PROGRAM
   END IF

END FUNCTION
#####################################################################
FUNCTION integra_cuenta(reg_5)
   DEFINE reg_5 RECORD
          nss               CHAR(11),
          subcuenta         INTEGER,
          siefore           SMALLINT,
          acciones          DECIMAL(16,6),
          pesos             DECIMAL(16,6),
          cedente           CHAR(03),
          estado            SMALLINT,
          consecutivo       INTEGER
   END RECORD

   DEFINE xabono        SMALLINT
   DEFINE xliq          SMALLINT
   DEFINE vsiefore      SMALLINT
   DEFINE txt_cla       CHAR(200)
{
   IF  reg_5.subcuenta = 14 THEN
      LET  reg_5.acciones    = 0
   END IF
}
   IF (reg_5.subcuenta = 3   OR
       reg_5.subcuenta = 10) THEN

      LET vsiefore = ""

      SELECT a.codigo_siefore
      INTO   vsiefore
      FROM   cta_regimen a
      WHERE  a.nss = reg_5.nss
      AND    a.subcuenta = reg_5.subcuenta

      LET txt_cla = "EXECUTE PROCEDURE crea_saldo_vol(",
                     vfolio_liquida,",",
                     '"',reg_5.nss,'"',",",
                     vsiefore,",",
                     reg_5.subcuenta,",",
                     '"',fecha_liquida,'"',",",
                     '"',fecha_liquida,'"',",",
                     reg_5.pesos,",",
                     reg_5.acciones,",",
                     '"',usuario,'"'," )"
      LET txt_cla = txt_cla CLIPPED

      PREPARE claexe FROM txt_cla
      EXECUTE claexe
   END IF
   IF reg_5.estado = 60 THEN
      LET vid = "UC-",reg_5.cedente USING "&&&" CLIPPED
   ELSE
      LET vid = "UN-",reg_5.cedente USING "&&&" CLIPPED
   END IF

   DECLARE s_abono CURSOR FOR abono
   OPEN    s_abono USING vfolio_liquida,
                         vfolio_sua,
                         reg_5.nss,
                         reg_5.subcuenta,
                         reg_5.siefore,
                         vmov_aporte,
                         reg_5.consecutivo,
                         reg_5.acciones,
                         reg_5.pesos,
                         vid,
                         HOY
   FETCH   s_abono INTO  xabono
   CLOSE   s_abono

   IF xabono < 0 THEN
      ERROR "No se provisiono la subcuenta ",reg_5.subcuenta CLIPPED,
            " del NSS: ",reg_5.nss CLIPPED," del folio ",vfolio_liquida CLIPPED
      SLEEP 3
      EXIT PROGRAM
   END IF

END FUNCTION
#####################################################################
FUNCTION actualiza_control(nss_u,nss_un)

   DEFINE nss_u          CHAR(11),
          nss_un         CHAR(11),
          xcodigo        SMALLINT,
          xrechazo       SMALLINT,
          vmarca         SMALLINT,
          xmarca         SMALLINT,
          vdias          INTEGER,
          ejecuta        CHAR(300),
          vhora          DATETIME HOUR TO SECOND

   LET vdias = 0

   SELECT  dias_cotizados
   INTO    vdias
   FROM    cta_ctr_cuenta
   WHERE   nss  = nss_un

   IF vdias IS NULL THEN
      LET vdias = 0
   END IF

   UPDATE  cta_ctr_cuenta
   SET     ind_actividad     = 1,
           fecha_actividad   = HOY,
           fecha_ult_rcv     = HOY,
           fecha_ult_general = HOY,
           --dias_cotizados    = dias_cotizados + vdias,
           fecha_actividad   = HOY
   WHERE   nss               = nss_u

   UPDATE  uni_unificado
   SET     folio_liquida = vfolio_liquida,
           fliquida      = fecha_liquida,
           fnotifica     = fecha_liquida,
           estado        = 95
   WHERE   nss_cta1      = nss_un
   AND     estado        = 40

   UPDATE  uni_unificador
   SET     folio_liquida = vfolio_liquida,
           fliquida      = fecha_liquida,
           fnotifica     = fecha_liquida,
           estado        = 95
   WHERE   nss_uni       = nss_u
   AND     estado        = 40

END FUNCTION
#####################################################################
FUNCTION actualiza_complemento(nss_u,nss_un)

   DEFINE nss_u          CHAR(11)
   DEFINE nss_un         CHAR(11)

   DEFINE reg_comple RECORD
          folio    INTEGER,
          nss_uni  CHAR(11),
          nss_cta1 CHAR(11),
          estado   SMALLINT
   END RECORD

   DEFINE txt_comple CHAR(200)

   LET txt_comple = " SELECT * ",
                    " FROM   tmp_solicitud_comple ",
                    " WHERE  nss_uni = ","'",nss_u,"'",
                    " AND    nss_cta1 = ","'",nss_un,"'"
   PREPARE actualiza_comple FROM txt_comple

   DECLARE cur_comple CURSOR FOR actualiza_comple

   FOREACH cur_comple INTO reg_comple.*
      UPDATE  uni_unificado
      SET     estado  = reg_comple.estado
      WHERE  folio    = reg_comple.folio
      AND    nss_uni  = reg_comple.nss_uni
      AND    nss_cta1 = reg_comple.nss_cta1
   END FOREACH

END FUNCTION
#############################################
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

END FUNCTION #habil_siguiente
#############################################
FUNCTION registra_liq()

   INSERT INTO dis_cuenta
   SELECT *
   FROM   dis_provision
   WHERE  folio = vfolio_liquida

END FUNCTION
#############################################
FUNCTION valida_regimen()
   DEFINE nsub      SMALLINT,
          total_reg SMALLINT,
          nss_val   CHAR(11),
          nss_val1  CHAR(11)

   DEFINE x_tipo_regimen     SMALLINT,
          x_estado           SMALLINT

   DEFINE inf_siefore_rcv    SMALLINT,
          cam_siefore_rcv    SMALLINT,
          x_proceso          SMALLINT,
          x_medio            CHAR(2)

   DEFINE vregimen CHAR(500)

   LET vregimen = " EXECUTE PROCEDURE sp_crea_regimen (?,?,?)"
   PREPARE genera_regimen FROM vregimen

   LET nsub      = 0
   LET total_reg = 0

   DECLARE regimen CURSOR FOR
   SELECT nss_uni
   FROM   uni_unificador
   WHERE  estado = 40
   AND    cve_ent_nss <> codigo

   LET v_tipo_proc  =  1
   LET v_tipo_trasp = 11
   LET v_medio      = 10

   FOREACH regimen INTO nss_val
      SELECT "X"
      FROM   cta_regimen
      WHERE  nss = nss_val
      GROUP BY 1

      IF STATUS = 100 THEN
         DECLARE curs1 CURSOR FOR fnacimiento
         OPEN  curs1 USING nss_val,
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
         OPEN cur_fn_regimen USING nss_val,
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
            INSERT INTO safre_tmp:rch_apertura VALUES (nss_val,
                                                       v_rechazo)
         END IF
      END IF
   END FOREACH
END FUNCTION
#########################################################################
FUNCTION rehabilita_cuenta()

   DEFINE f_subcuenta       INTEGER,
          f_siefore         INTEGER,
          f_monto_acc       DECIMAL(16,6),
          f_monto_pes       DECIMAL(16,6),
          v_subcuenta       INTEGER,
          v_grupo           INTEGER

   DEFINE retiro97_ini,
          cv_ini,
          cs_ini,
          sar_imss_ini,
          sar_issste_ini,
          viv_97_imss_ini,
          viv_92_imss_ini,
          viv_92_issste_ini,
          vol_ini,
          com_ini  DECIMAL(16,6)


   DEFINE reh RECORD
          nss CHAR(11)
   END RECORD

   LET f_subcuenta       = 0
   LET f_monto_pes       = 0

   LET retiro97_ini = 0
   LET cv_ini = 0
   LET cs_ini = 0
   LET sar_imss_ini = 0
   LET sar_issste_ini = 0
   LET viv_97_imss_ini = 0
   LET viv_92_imss_ini = 0
   LET viv_92_issste_ini = 0
   LET vol_ini = 0
   LET com_ini = 0

   CREATE INDEX tmp_rehabilita_1 ON tmp_rehabilitada(nss)

   UPDATE STATISTICS FOR TABLE tmp_rehabilitada

   DECLARE cur_reh CURSOR FOR
   SELECT nss
   FROM   tmp_rehabilitada
   group by 1

   FOREACH cur_reh INTO reh.nss
      DECLARE cur_reh2 CURSOR FOR
      SELECT  subcuenta,
              monto_en_pesos
      FROM    dis_cuenta
      WHERE   folio = vfolio_liquida
      AND     nss = reh.nss

      FOREACH cur_reh2 INTO f_subcuenta,
                           f_monto_pes

         CASE
            WHEN f_subcuenta = 1
               LET retiro97_ini = retiro97_ini + f_monto_pes
            WHEN f_subcuenta = 2 OR f_subcuenta = 6 OR f_subcuenta = 9
               LET cv_ini  = cv_ini  + f_monto_pes
            WHEN f_subcuenta = 5
               LET cs_ini  = cs_ini  + f_monto_pes
            WHEN f_subcuenta = 7
               LET sar_imss_ini = sar_imss_ini + f_monto_pes
            WHEN f_subcuenta = 13
               LET sar_issste_ini = sar_issste_ini + f_monto_pes
            WHEN f_subcuenta = 4
               LET viv_97_imss_ini = viv_97_imss_ini + f_monto_pes
            WHEN f_subcuenta = 8
               LET viv_92_imss_ini = viv_92_imss_ini + f_monto_pes
            WHEN f_subcuenta = 14
               LET viv_92_issste_ini = viv_92_issste_ini + f_monto_pes
            WHEN f_subcuenta = 3 OR f_subcuenta = 10
               LET vol_ini = vol_ini + f_monto_pes
            WHEN f_subcuenta = 11 OR f_subcuenta = 12
               LET com_ini = com_ini + f_monto_pes
         END CASE
      END FOREACH

      INSERT INTO cta_rehabilitada
      VALUES (vfolio_liquida , --folio
              reh.nss        , --nss
              retiro97_ini   , --monto_retiro
              cv_ini         , --monto_cesantia
              vol_ini        , --monto_voluntaria
              viv_97_imss_ini, --monto_vivienda97
              cs_ini         , --monto_cuota_soc
              sar_imss_ini   , --monto_sar
              viv_92_imss_ini, --monto_vivienda92
              hoy          , --fecha_rehabilita
              243            , --marca_cod
              hoy          , --fecha_actualiza
              0              , --estado
              USER             --usuario
             )
   END FOREACH

END FUNCTION
################################################################################
FUNCTION verifica_bono(li_folio_liquida, lc_usuario)
   DEFINE li_folio_liquida INTEGER
   DEFINE lc_unificador    CHAR(11)
   DEFINE lc_unificado     CHAR(11)
   DEFINE lc_usuario       CHAR(08)

   DEFINE lr_bono_unificado, lr_bono_unificador RECORD
          nss	            CHAR(11)     ,
          curp	          CHAR(18)     ,
          fecha_redencion	DATE         ,
          fecha_registro	DATE         ,
          udis	          DECIMAL(22,6),
          pesos	          DECIMAL(22,6),
          proceso	        CHAR(3)      ,
          usuario	        CHAR(8)
   END RECORD

   DEFINE ld_udis                  DECIMAL(22,6)
   DEFINE ld_pesos                 DECIMAL(22,6)
   DEFINE ls_alguno_registrado     SMALLINT
   DEFINE ls_unificador_registrado SMALLINT
   DEFINE ld_fecha_redencion       DATE

   DEFINE lc_sql CHAR(500)

   LET lc_sql = " SELECT *            ",
                " FROM   cta_act_bono ",
                " WHERE  nss = ?      "
   PREPARE get_act_bono FROM lc_sql

   LET lc_sql = " INSERT INTO cta_his_bono VALUES( ",
                "    ?    ,                        ", --nss
                "    ?    ,                        ", --curp
                "    ?    ,                        ", --fecha_redencion
                "    ?    ,                        ", --fecha_registro
                "    ?    ,                        ", --udis
                "    ?    ,                        ", --pesos
                "    ?    ,                        ", --proceso
                "    ?    ,                        ", --fecha_baja
                "    'UNI',                        ", --proceso_baja
                "    NULL ,                        ", --fecha_reingreso
                "    ?                             ", --usuario
                " )                                "
   PREPARE inserta_historico FROM lc_sql

   LET lc_sql = " SELECT SUM(monto_en_acciones), ",
                "        SUM(monto_en_pesos)     ",
                " FROM   dis_cuenta              ",
                " WHERE  subcuenta = 36          ",
                " AND    nss       = ?           "
   PREPARE get_udis_actual FROM lc_sql

   DECLARE cur_bono_liquida CURSOR FOR
   SELECT UNIQUE nss
   FROM   dis_cuenta
   WHERE  subcuenta         = 36
   AND    monto_en_acciones > 0
   AND    folio             = li_folio_liquida

   FOREACH cur_bono_liquida INTO lc_unificador
      #Verificar si el unificador ya esta en la tabla de bono
      SELECT "X"
      FROM   cta_act_bono
      WHERE  nss = lc_unificador

      IF SQLCA.SQLCODE = 0 THEN
         LET ls_unificador_registrado = 1
      ELSE
      	 LET ls_unificador_registrado = 0
      END IF

   	  #Verificar si algún unificado tiene registro en cta_act_bono
   	  SELECT "X"
   	  FROM   cta_act_bono
      WHERE  nss IN (SELECT nss_cta1
                     FROM   uni_unificado
                     WHERE  estado IN (95,96,100)
                     AND    nss_uni = lc_unificador)

      IF SQLCA.SQLCODE = 0 THEN
         LET ls_alguno_registrado = 1
      ELSE
      	 LET ls_alguno_registrado = 0
      END IF

      DECLARE get_unificados CURSOR FOR
      SELECT nss_cta1
      FROM   uni_unificado
      WHERE  estado IN (95,96,100)
      AND    nss_uni = lc_unificador

      INITIALIZE lr_bono_unificado.* TO NULL

      #Procesar a los unificados
      FOREACH get_unificados INTO lc_unificado

      	 #Si alguno ya esta registrado
      	 IF ls_alguno_registrado = 1 THEN
            SELECT "X"
            FROM   cta_act_bono
            WHERE  nss = lc_unificado

            IF SQLCA.SQLCODE = 0 THEN
            	 #Obtener registro actual
            	 EXECUTE get_act_bono USING lc_unificado
            	                      INTO  lr_bono_unificado.*

               #Guardar el historico
            	 EXECUTE inserta_historico USING lr_bono_unificado.nss            ,
                                               lr_bono_unificado.curp           ,
                                               lr_bono_unificado.fecha_redencion,
                                               lr_bono_unificado.fecha_registro ,
                                               lr_bono_unificado.udis           ,
                                               lr_bono_unificado.pesos          ,
                                               lr_bono_unificado.proceso        ,
                                               HOY                              ,
                                               lc_usuario
               #Eliminar la activa
               DELETE
               FROM   cta_act_bono
               WHERE  nss = lc_unificado
            END IF
         ELSE
            #Si no hay ningun unificado registrado
            #y el unificador no esta registrado
            #Buscar los datos de la fecha de redencion
            IF ls_unificador_registrado = 0 THEN

            	 INITIALIZE ld_fecha_redencion TO NULL

               #Buscar el registro en dis_det_bono
               sql
                  SELECT to_date("01"||"/01/"||a.fecha_reden[1,4],'%m/%d/%Y')
                  INTO   $ld_fecha_redencion
                  FROM   dis_det_bono     a,
                         afi_mae_afiliado b
                  WHERE  a.n_unico            = b.n_unico
                  AND    a.ind_tipo_bono_isss = 1
                  AND    b.n_seguro           = $lc_unificado
                  GROUP BY 1
               END sql

               #Si no hay datos en dis_det_bono
               #Buscamos en traspaso
               IF ld_fecha_redencion IS NULL THEN
                  SELECT fecha_red_bono
                  INTO   ld_fecha_redencion
                  FROM   taa_viv_recepcion
                  WHERE  nss          = lc_unificado
                  AND    importe_bono > 0
                  GROUP BY 1
               END IF
            END IF
         END IF
      END FOREACH

      #Obtener saldo actual
      EXECUTE get_udis_actual USING lc_unificador
                              INTO  ld_udis,
                                    ld_pesos

      #Si el unificador ya esta en la tabla de bono
      IF ls_unificador_registrado = 1 THEN
      	 #Obtener registro actual
         EXECUTE get_act_bono USING lc_unificador
         	                    INTO  lr_bono_unificador.*

         #Guardar el historico
         EXECUTE inserta_historico USING lr_bono_unificador.nss            ,
                                         lr_bono_unificador.curp           ,
                                         lr_bono_unificador.fecha_redencion,
                                         lr_bono_unificador.fecha_registro ,
                                         lr_bono_unificador.udis           ,
                                         lr_bono_unificador.pesos          ,
                                         lr_bono_unificador.proceso        ,
                                         HOY                               ,
                                         lc_usuario

         #Actualizar registro
         UPDATE cta_act_bono
         SET    udis           = ld_udis,
                fecha_registro = HOY
         WHERE  nss = lc_unificador
      ELSE
      	 #Obtener curp
      	 SELECT n_unico
      	 INTO   lr_bono_unificador.curp
      	 FROM   afi_mae_afiliado
      	 WHERE  n_seguro = lc_unificador

      	 IF lr_bono_unificado.fecha_redencion IS NOT NULL THEN
      	    LET lr_bono_unificador.fecha_redencion = lr_bono_unificado.fecha_redencion
      	 ELSE
      	 	  LET lr_bono_unificador.fecha_redencion = ld_fecha_redencion
      	 END IF

      	 LET lr_bono_unificador.nss            = lc_unificador
      	 LET lr_bono_unificador.fecha_registro = HOY
      	 LET lr_bono_unificador.udis           = ld_udis
      	 LET lr_bono_unificador.pesos          = ld_pesos
      	 LET lr_bono_unificador.proceso        = 'UNI'
      	 LET lr_bono_unificador.usuario        = lc_usuario

      	 INSERT INTO cta_act_bono VALUES(lr_bono_unificador.*)
      END IF
   END FOREACH
END FUNCTION
################################################################################