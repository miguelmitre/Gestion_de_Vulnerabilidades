###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa          => TRASPASO DE IDENTIFICACION POR EDAD                     #
#Fecha             => 22 de agosto de 2010
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES                         #
#Sistema           => CTA                                                     #
###############################################################################
DATABASE safre_af

GLOBALS
  DEFINE reg_3 RECORD
      nss              CHAR(11),
      grupo_regimen    SMALLINT,
      siefore_ced      SMALLINT,
      siefore_rec      SMALLINT,
      folio_solicitud  INTEGER
   END RECORD

   DEFINE  #glo #date
      HOY                 ,
      fecha_informe       ,
      fecha_liquida       ,
      vfecha_liquida      ,
      vfecha_inicio       ,
      vfecha_inicio1      ,
      vfecha_final        ,
      vfecha              ,
      vfecha1             ,
      vfinicio            DATE

   DEFINE  #glo #char
      enter               CHAR(1),
      char                CHAR(1),
      vpregunta           CHAR(1),
      usuario             CHAR(8),
      nss_unifi           CHAR(11),
      nss_unificador      CHAR(11),
      vdesmarca           CHAR(100),
      vsaldo              CHAR(100),
      vprov_cargo         CHAR(100),
      vprov_abono         CHAR(100),
      vliquida            CHAR(100),
      indevidas           CHAR(100),
      vid                 CHAR(06),
      vtraspaso           CHAR(40),
      rliquida            CHAR(40)

   DEFINE  #glo #smallint
      vsubcuenta          ,
      vgrupo              ,
      vmarca_causa        ,
      vcorrelativo        ,
      vestado_marca       SMALLINT

   DEFINE  #glo #integer
      codigo              ,
      tipo_mov            ,
      vextra_uni          ,
      vextra_cta1         ,
      vmov_aporte         ,
      vmov_cargo          ,
      vdia                ,
      dia                 ,
      dia1                ,
      marca               ,
      vfolio1        	  ,
      vfolio_sua     	  ,
      vfolio_liquida 	  INTEGER

   DEFINE x_programa    CHAR(07)

   DEFINE detecta       CHAR(150)

   DEFINE vtransferencia     CHAR(100),
          status_ind         SMALLINT

END GLOBALS
-----------------------------------------------------------
MAIN
   LET vfolio_liquida = ARG_VAL(1)
   LET vfecha_liquida = ARG_VAL(2)

   DISPLAY "INICIA PRELIQUIDACION DE IDENTIFICACION POR EDAD DEL  ",
            vfecha_liquida

   LET x_programa = "CTAC043"

   CALL STARTLOG ("CTAC043.log")
   CALL inicio()
   CALL proceso_principal()

END MAIN
-----------------------------------------------------------
FUNCTION inicio()

   LET HOY = TODAY 

   SELECT codigo_afore,
          user
   INTO   codigo,
          usuario
   FROM   tab_afore_local

   LET vtraspaso      = 40  --"TRASPASADO"
   LET rliquida       = 100 --"LIQUIDADO"

   LET vmov_aporte    = 55  --"APORTACION"

   LET vmov_cargo     = 255 --"CARGO"
   LET vid            =  "ID EDAD"

   LET vdesmarca     = " EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "

   LET vprov_cargo = " EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
   PREPARE cargo     FROM vprov_cargo

   LET vprov_abono = " EXECUTE FUNCTION fn_prov_abono_sie (?,?,?,?,?,?,?,?,?,?,?) "
   PREPARE abono     FROM vprov_abono

   LET vliquida    = " EXECUTE FUNCTION fn_liquida (?,?,?,?,?) "
   PREPARE liquida   FROM vliquida

   LET vfolio_sua    = 0
   LET vcorrelativo  = 0
   LET vmarca_causa  = 0
   LET vestado_marca = 0
   #LET vfecha_liquida  = hoy

END FUNCTION
-----------------------------------------------------------
FUNCTION proceso_principal()

      CALL registra_etapa()

      CALL preliquidacion()

END FUNCTION
-----------------------------------------------------------
FUNCTION preliquidacion()

   DEFINE total_pesos    DECIMAL(16,6),
          cont           INTEGER,
          xsubcuenta     SMALLINT,
          xsiefore       SMALLINT,
          vprecio        DECIMAL(16,6),
          xprecio        DECIMAL(16,6),
          xprecio_s1     DECIMAL(16,6),
          xprecio_s2     DECIMAL(16,6),
          xprecio_s3     DECIMAL(16,6),
          xprecio_s4     DECIMAL(16,6),
          xprecio_s5     DECIMAL(16,6),
          xacciones      DECIMAL(16,6),
          xpesos         DECIMAL(16,6)

   DEFINE opc             CHAR(01),
          vresulta        CHAR(100),
          vcurp           CHAR(18),
          vhora_final     CHAR(08)

   DEFINE x_correlativo   INTEGER,
          x_extra_uni     SMALLINT,
          sw              SMALLINT,
          vsiefore        SMALLINT,
          x_marca_ret     SMALLINT

   DEFINE flag             SMALLINT

---genera registro en dis_provision

      DECLARE cur_3 cursor for
      SELECT precio_del_dia,
             codigo_siefore
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = vfecha_liquida
      AND    codigo_siefore in(1,2,3,4,5)
      FOREACH cur_3 INTO vprecio,vsiefore

         CASE  vsiefore
            WHEN 1
               LET xprecio_s1   = vprecio
            WHEN 2
               LET xprecio_s2   = vprecio
            WHEN 3
               LET xprecio_s3   = vprecio
            WHEN 4
               LET xprecio_s4   = vprecio
            WHEN 5
               LET xprecio_s5   = vprecio
         END CASE
      END FOREACH

      DECLARE cur_4 CURSOR FOR
      SELECT nss,
             grupo_regimen,
             siefore_ced,
             siefore_rec,
             folio_solicitud   
      FROM   tes_solicitud
      WHERE  tipo_traspaso = 13
      AND    estado = 100
      ORDER BY 1

      FOREACH cur_4 INTO reg_3.*

         DECLARE cur_5 CURSOR FOR
         SELECT subcuenta
         FROM   tab_agrupa_subcta_regimen
         WHERE  grupo_regimen = reg_3.grupo_regimen

         FOREACH cur_5 INTO vsubcuenta

            SELECT monto_en_acciones,
                   monto_en_pesos
            INTO   xacciones,
                   xpesos
            FROM   safre_tmp:tmp_saldo_edad
            WHERE  nss       = reg_3.nss
            AND    subcuenta = vsubcuenta
            AND    siefore   = reg_3.siefore_ced

            IF xacciones > 0 THEN
               LET xprecio = 0
               CASE reg_3.siefore_ced
                  WHEN 1
                     LET xprecio   = xprecio_s1 
                  WHEN 2
                     LET xprecio   = xprecio_s2 
                  WHEN 3
                     LET xprecio   = xprecio_s3 
                  WHEN 4
                     LET xprecio   = xprecio_s4 
                  WHEN 5
                     LET xprecio   = xprecio_s5
               END CASE
               CALL provisiona_cargo(reg_3.nss,
                                     vcurp,
                                     vsubcuenta,
                                     reg_3.siefore_ced,
                                     xacciones,
                                     xpesos,
                                     xprecio,
                                     reg_3.folio_solicitud)

               LET xprecio = 0
               CASE reg_3.siefore_rec
                  WHEN 1
                     LET xacciones = xpesos / xprecio_s1 
                     LET xprecio   = xprecio_s1 
                  WHEN 2
                     LET xacciones = xpesos / xprecio_s2 
                     LET xprecio   = xprecio_s2 
                  WHEN 3
                     LET xacciones = xpesos / xprecio_s3 
                     LET xprecio   = xprecio_s3 
                  WHEN 4
                     LET xacciones = xpesos / xprecio_s4 
                     LET xprecio   = xprecio_s4 
                  WHEN 5
                     LET xacciones = xpesos / xprecio_s5 
                     LET xprecio   = xprecio_s5
               END CASE
               CALL provisiona_abono(reg_3.nss,
                                     vcurp,
                                     vsubcuenta,
                                     reg_3.siefore_rec, ---cargo
                                     xacciones,
                                     xpesos,
                                     xprecio,
                                     reg_3.folio_solicitud)
            END IF

         END FOREACH

         UPDATE tes_solicitud
         SET    estado = 102,
                folio  = vfolio_liquida
         WHERE  nss = reg_3.nss
         AND    tipo_traspaso = 13
         AND    grupo_regimen = reg_3.grupo_regimen
         AND    estado = 100

      END FOREACH

   LET   vresulta = "TOTAL DE TRABAJADORES TRANSFERIDOS ",cont CLIPPED
   LET   vhora_final = TIME

   --- actualiza etapa
   UPDATE  dis_ctrl_proceso
   SET     hora_final  = vhora_final,
           parametro2  = TODAY,
           resultado   = vresulta
   WHERE   folio       = vfolio_liquida
   AND     etapa_cod   = 2
   AND     proceso_cod = x_programa

END FUNCTION
-----------------------------------------------------------------
FUNCTION provisiona_cargo(reg_4)

   DEFINE reg_4 RECORD
          nss               CHAR(11),
          curp              CHAR(18),
          subcuenta         INTEGER,
          siefore_ced       SMALLINT,
          acciones          DECIMAL(16,6),
          pesos             DECIMAL(16,6),
          precio            DECIMAL(16,6),
          consecutivo       INTEGER
   END RECORD

   DEFINE xcargo        SMALLINT
   DEFINE xliq          SMALLINT

   DEFINE opc           CHAR(1)

   LET reg_4.pesos    = reg_4.pesos    * -1
   LET reg_4.acciones = reg_4.acciones * -1

   INSERT INTO dis_provision values(  vmov_cargo,
                                      reg_4.subcuenta,
                                      reg_4.siefore_ced,
                                      vfolio_liquida,
                                      reg_4.consecutivo,
                                      reg_4.nss,
                                      reg_4.curp,
                                      vfolio_sua ,
                                      vfecha_liquida,
                                      vfecha_liquida,
                                      vfecha_liquida,
                                      reg_4.pesos,
                                      reg_4.acciones,
                                      reg_4.precio,
                                      0,
                                      "",
                                      vid,
                                      5,
                                      today,
                                      user,
                                      today,
                                      0)

END FUNCTION
------------------------------------------------
FUNCTION provisiona_abono(reg_5)
   DEFINE reg_5 RECORD
          nss               CHAR(11),
          curp              CHAR(18),
          subcuenta         INTEGER,
          siefore_rec       SMALLINT,
          acciones          DECIMAL(16,6),
          pesos             DECIMAL(16,6),
          precio            DECIMAL(16,6),
          consecutivo       INTEGER
   END RECORD

   DEFINE xabono        SMALLINT
   DEFINE xliq          SMALLINT
   DEFINE vsiefore      SMALLINT
   DEFINE txt_cla       CHAR(200)


   INSERT INTO dis_provision values(  vmov_aporte,
                                      reg_5.subcuenta,
                                      reg_5.siefore_rec,
                                      vfolio_liquida,
                                      reg_5.consecutivo,
                                      reg_5.nss,
                                      reg_5.curp,
                                      vfolio_sua ,
                                      vfecha_liquida,
                                      vfecha_liquida,
                                      vfecha_liquida,
                                      reg_5.pesos,
                                      reg_5.acciones,
                                      reg_5.precio,
                                      0,
                                      "",
                                      vid,
                                      5,
                                      today,
                                      user,
                                      today,
                                      0)

END FUNCTION
-----------------------------------------------------
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
--------------------------------------------------------------
FUNCTION registra_liq()

   INSERT INTO dis_cuenta
   SELECT *
   FROM   dis_provision
   WHERE  folio = vfolio_liquida

END FUNCTION
--------------------------------------------------------------
FUNCTION registra_etapa()
   DEFINE xfolio_dia   SMALLINT
   DEFINE hora_inicial CHAR(08),
          hora_final   CHAR(08)

   LET hora_inicial = TIME
   LET hora_final   = NULL
   LET xfolio_dia   =  0

   INSERT INTO dis_ctrl_proceso
   VALUES (TODAY,                   -- fecha_proceso
           "CTAC043",               -- proceso_cod
           2,                       -- etapa_cod
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           vfecha_liquida,          -- parametro1
           NULL,                    -- parametro2
           NULL,                    -- parametro3
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           vfolio_liquida,          -- folio
           "Inicia Preliquidacion", -- resultado
           USER,                    -- usuario
           0                        -- consecutivo
          )

END FUNCTION
