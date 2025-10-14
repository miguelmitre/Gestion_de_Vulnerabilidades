###############################################################################
#proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa          => TRASPASO INTRA AFORE                                    #
#Fecha             => 8 de mayo del 2000                                      #
#Actualizado       => MIGUEL ANGEL HERNANDEZ MARTINEZ.                        #
#Fecha actualiza   =>  26 julio 2004                                          #
#Fecha actualiza   =>  01 marzo 2005                                          #
#Fecha actualiza   =>  27 febrero 2006                                        #
#Fecha act         => 14 junio 2007                                           #
#Fecha Act         => 1 septiembre 2008                                       #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE HOY                   ,
          fecha_informe         ,
          fecha_liquida         ,
          fecha_liquida_viv     ,
          vfecha_inicio         ,
          vfecha_final          ,
          vfecha_mes_intra      ,
          vfecha                ,
          vfecha1               ,
          vfinicio              DATE

   DEFINE enter                 CHAR(1),
          char                  CHAR(1),
          vpregunta             CHAR(1),
          vunifica              CHAR(2),
          usuario               CHAR(8),
          nss_unifi             CHAR(11),
          nss_unificador        CHAR(11),
          vdesmarca             CHAR(100),
          vsaldo                CHAR(100),
          vprov_cargo           CHAR(100),
          vprov_abono           CHAR(100),
          vliquida              CHAR(100),
          indevidas             CHAR(100),
          vid                   CHAR(06)

   DEFINE vsubcuenta            ,
          vgrupo                ,
          vmarca_causa          ,
          vcorrelativo          ,
          vestado_marca         SMALLINT

   DEFINE marca                 ,
          vestado               ,
          vsolicitado           ,
          vconfronta            ,
          vintra                ,
          vconsecutivo          ,
          vconsecutivo1         ,
          vintra_uni            ,
          vintra_cta1           ,
          vmarca_intra          ,
          vmarca_intrau         ,
          vmov_aporte           ,
          vmov_interes          ,
          vmov_intra            ,
          vmov_intra_int        ,
          codigo               ,
          tipo_mov             ,
          vdia                 ,
          dia                  ,
          dia1                 ,
          vfolio_liquida     ,
          vfolio_sua         ,
          vfolio2            ,
          vfolio             INTEGER

   DEFINE vtransferencia     CHAR(100),
          status_ind         SMALLINT

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

   CALL STARTLOG ("UNIC010.log")
   CALL inicio()
   CALL proceso_principal()

END MAIN
#####################################################################
FUNCTION inicio()

   LET vunifica = "01"
   LET vfolio_sua = 0
   LET HOY      = TODAY

   SELECT codigo_afore,
          user
   INTO   codigo,
          usuario
   FROM   tab_afore_local

   LET vid  = "UN-",codigo CLIPPED

   LET vconfronta     = 20 --- "CONFRONTADO"
   LET vsolicitado    = 30 --- "SOLICITADO"
   LET vintra         = 90 --- "INTRA AFORE"

   LET vmov_aporte    = 1  --- "APORTACION"
   LET vmov_interes   = 4 --- "INTERES RECIBIDO"

   LET vmov_intra     = 241 --- "UNIFICACION INTRA-AFORE"
   LET vmov_intra_int = 243 --- "UNIFICACION INTRA-AFORE INTERES"

   LET vmarca_intra   = 241 --- "UNIFICACION INTRA-AFORE"
   LET vmarca_intrau  = 242 --- "UNIFICACION INTRA-AFORE U"

   LET vdesmarca   = " EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "

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

   LET vcorrelativo  = 0
   LET vmarca_causa  = 0
   LET vestado_marca = 0

   LET  vfinicio = HOY
   LET  vdia     = DAY(vfinicio) -1
   LET  vfinicio = vfinicio - vdia UNITS DAY
   LET  vfecha1  = vfinicio

   LET fecha_liquida = habil_siguiente(vfecha1)

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_rehabilitada
      DROP TABLE tmp_indevidas
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_rehabilitada
      (nss CHAR(11) )

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
#####################################################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0101" ATTRIBUTE(BORDER)
   DISPLAY "UNIC010           LIQUIDACION DE TRASPASOS INTRA AFORE                             " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "     [Esc]  Iniciar                                        [Ctrl-C]  Salir   " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   INPUT BY NAME vpregunta WITHOUT DEFAULTS

      AFTER FIELD vpregunta
         IF vpregunta IS NULL THEN
            ERROR "DEBE PONER ALGUNA OPCION PARA EJECUTAR"
            NEXT FIELD vpregunta
         END IF

         IF vpregunta MATCHES '[Ss]' THEN
            LET marca = 0

            SELECT "X"
            FROM   uni_unificador
            WHERE  estado  = vintra
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               LET marca = 1
            END IF

            IF marca = 0  THEN
               ERROR "NO HAY CUENTAS POR UNIFICAR INTRAFORE"
               SLEEP 3
               EXIT PROGRAM
            END IF

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
#####################################################################
FUNCTION liquidacion()

   DEFINE total_pesos    DECIMAL(16,6),
          monto_ult_mes  DECIMAL(16,6),
          vinteres_per   DECIMAL(16,6),
          vinteres_mes   DECIMAL(16,6),
          vinteres       DECIMAL(16,6),
          dia_cotizado   INTEGER,
          contasi        INTEGER,
          contuni        INTEGER,
          contcta1       INTEGER,
          fecha_valor    DATE,
          fcert_uni      DATE,
          fcert_cta1     DATE,
          nss_1          CHAR(11),
          nss_2          CHAR(11),
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

   LET vinteres_mes  = 0
   LET vinteres      = 0
   LET vsubcuenta    = 0
   LET vgrupo        = 0

   LET dia               = DAY(fecha_liquida)
   LET dia1              = dia - 1
   LET vfecha_final      = fecha_liquida - dia UNITS DAY
   LET vfecha_inicio     = fecha_liquida - dia1 UNITS DAY -1 UNITS MONTH
   LET fecha_liquida_viv = fecha_liquida - dia UNITS DAY +1 UNITS DAY
   LET fecha_informe     = vfecha_final +1 UNITS MONTH

   LET x_tipo_traspaso   = 4
   LET x_medio           = "10"

   LET ind_decimos_nss = 0
   LET ind_decimos_cta1 = 0

   DECLARE cur_2 CURSOR FOR
   SELECT nss_uni,
          cont_servicio,
          folio
   FROM   uni_unificador
   WHERE  estado  = vintra
   ORDER BY 1

   LET contuni = 0
   LET contcta1 = 0

   FOREACH cur_2 INTO nss_unificador,
                      vconsecutivo,
                      vfolio

      LET contuni = contuni + 1

      DISPLAY "UNIFICADORES PROCESADOS : ",contuni  AT 15,1

      DECLARE cur_3 CURSOR FOR
      SELECT nss_cta1,
             cont_servicio,
             estado
      FROM   uni_unificado
      WHERE  folio        = vfolio
      AND    nss_uni      = nss_unificador
      AND    estado       = vintra
      AND    diag_unifica = vunifica

      FOREACH cur_3 INTO nss_unifi,
                         vestado,
                         vconsecutivo1

         LET contcta1 = contcta1 + 1

         DISPLAY "UNIFICADOS PROCESADOS   : ",contcta1  AT 16,1

         LET flag = 0
         LET ind_siefore = 1

         DECLARE s_saldo CURSOR FOR saldo_dia
         FOREACH s_saldo USING nss_unifi,
                               vsubcuenta,
                               vgrupo,
                               HOY
                         INTO  xsubcuenta,
                               xsiefore,
                               xacciones,
                               xpesos

            SELECT "X"                            ---nvo121006
            FROM   cta_regimen
            WHERE  nss            = nss_unificador     ---unificador
            AND    codigo_siefore = xsiefore           ---siefore_unificado
            AND    subcuenta      = xsubcuenta         ---subcuenta_unificado
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN             ---nvo121006
               LET xregimen = 0
            ELSE
               LET xregimen = 1

               LET inf_siefore_rcv = xsiefore

               SELECT codigo_siefore
               INTO   cam_siefore_rcv
               FROM   cta_regimen
               WHERE  nss            =  nss_unificador
               AND    codigo_siefore <> xsiefore
               AND    subcuenta      =  xsubcuenta
               GROUP BY 1
            END IF

            IF xacciones > 0 THEN
               CALL liquida_cuenta(nss_unifi,
                                   xsubcuenta,
                                   xsiefore,
                                   xacciones,
                                   xpesos,
                                   vconsecutivo1)

               CALL integra_cuenta(nss_unificador,
                                   xsubcuenta,
                                   xsiefore,
                                   xacciones,
                                   xpesos,
                                   vconsecutivo)

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
                  CALL liquida_cuenta(nss_unifi,
                                      xsubcuenta,
                                      xsiefore,
                                      xacciones,
                                      xpesos,
                                      vconsecutivo1)

                  CALL integra_cuenta(nss_unificador,
                                      xsubcuenta,
                                      xsiefore,
                                      xacciones,
                                      xpesos,
                                      vconsecutivo1)
                  LET sw = 1
               END IF
               }
            END IF

            IF flag = 1 AND xregimen = 1 THEN               ---nvo161006
                  SELECT "X"
                  FROM   tes_solicitud
                  WHERE  nss             = nss_unificador
                  AND    folio_solicitud = vfolio_liquida
                  AND    estado          = 100
                  GROUP BY 1

                  IF SQLCA.SQLCODE <> 0 THEN

                     INSERT INTO tmp_indevidas
                     VALUES(nss_unificador,   --nss
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
            WHERE  nss = nss_unificador
            AND    marca_cod = 140
            GROUP BY 2

            IF  x_marca_ret = 1 THEN

                LET x_extra_uni = 140
                LET x_marca_causa = 241

                PREPARE marcaje_ret FROM vdesmarca
                EXECUTE marcaje_ret USING nss_unificador,   # nss
                                         x_extra_uni,       # marca_entra
                                         x_correlativo,     # correlativo
                                         vestado_marca,     # estado_marca
                                         x_marca_causa,     # marca_causa
                                         usuario            # usuario

                INSERT INTO tmp_rehabilitada
                VALUES (nss_unificador)
            END IF

         END IF

         CALL actualiza_control(nss_unifi,nss_unificador)
         CALL ind_transferencia(nss_unifi)   --se agrega 270206
      END FOREACH
   END FOREACH

   CALL registra_liq()
   CALL fn_indevidas()         -- ingreso 10-06-2008
   CALL rehabilita_cuenta()

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
   OPEN    s_indicador USING nss_cta1,
                             x_ind_transferencia,
                             fecha_liquida
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
          consecutivo       INTEGER
   END RECORD

   DEFINE xcargo        SMALLINT
   DEFINE xliq          SMALLINT

   LET reg_4.pesos    = reg_4.pesos    * -1
   LET reg_4.acciones = reg_4.acciones * -1
{
   IF  reg_4.subcuenta = 14 THEN
      LET  reg_4.acciones    = 0
   END IF
}
   IF (reg_4.subcuenta = 3   OR
       reg_4.subcuenta = 10) THEN
      UPDATE cta_saldo_vol
      SET    saldo_acciones = 0,
             fecha_saldo    = fecha_liquida,
             usuario        = usuario
      WHERE  nss            = reg_4.nss
      AND    subcuenta      = reg_4.subcuenta
      AND    fecha_saldo    <= fecha_liquida
   END IF

   DECLARE s_cargo CURSOR FOR cargo
   OPEN    s_cargo USING vfolio_liquida,
                         vfolio_sua,
                         reg_4.nss,
                         reg_4.subcuenta,
                         vmov_intra,
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
          xrechazo       SMALLINT,
          xcodigo        SMALLINT,
          vmarca         SMALLINT,
          xmarca         SMALLINT,
          vdias          SMALLINT,
          ejecuta        CHAR(300),
          vhora          DATETIME HOUR TO SECOND

   LET vdias = 0

   SELECT  dias_cotizados
   INTO    vdias
   FROM    cta_ctr_cuenta
   WHERE   nss  = nss_u

   IF vdias IS NULL THEN
      LET vdias = 0
   END IF

   UPDATE  cta_ctr_cuenta
   SET     ind_actividad     = 1,
           fecha_actividad   = HOY,
           fecha_ult_rcv     = HOY,
           fecha_ult_general = HOY,
           dias_cotizados    = dias_cotizados + vdias,
           fecha_actividad   = HOY
   WHERE   nss               = nss_un

   UPDATE  uni_unificado
   SET     folio_liquida = vfolio_liquida,
           fliquida      = fecha_liquida,
           fnotifica     = fecha_liquida,
           estado        = 95
   WHERE   folio         = vfolio
   AND     nss_cta1      = nss_u

   UPDATE  uni_unificador
   SET     folio_liquida = vfolio_liquida,
           fliquida      = fecha_liquida,
           fnotifica     = fecha_liquida,
           estado        = 95
   WHERE   folio         = vfolio
   AND     nss_uni       = nss_un

END FUNCTION
#####################################################################
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
#####################################################################
FUNCTION registra_liq()

   INSERT INTO dis_cuenta
   SELECT *
   FROM   dis_provision
   WHERE  folio = vfolio_liquida

END FUNCTION
#########################################################################
FUNCTION rehabilita_cuenta()

   DEFINE f_subcuenta       INTEGER,
          f_siefore         INTEGER,
          f_monto_acc       DECIMAL(22,6),
          f_monto_pes       DECIMAL(22,6),
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
          com_ini  DECIMAL(22,6)

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
      VALUES (vfolio_liquida,  --- folio
              reh.nss,         --- nss
              retiro97_ini,    --- monto_retiro
              cv_ini,          --- monto_cesantia
              vol_ini,         --- monto_voluntaria
              viv_97_imss_ini, --- monto_vivienda97
              cs_ini,          --- monto_cuota_soc
              sar_imss_ini,    --- monto_sar
              viv_92_imss_ini, --- monto_vivienda92
              today,           --- fecha_rehabilita
              241,             --- marca_cod
              today,           --- fecha_actualiza
              0,               --- estado
              USER             --- usuario
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