-----------------------------------------------------------------------------------
-- Owner             => E.F.P.                                                    -
-- Programa RETL837D => GENERA EL ARCHIVO DE DETALLE DE LOS ANEXOS 134 Y 137      -
-- Fecha creacion    => ABRIL 2019                                                -
-- Sistema           => RET                                                       -
-----------------------------------------------------------------------------------

DATABASE safre_af
GLOBALS

    DEFINE gr_fechas        RECORD
                              inicio          DATE,
                              fin             DATE
                            END RECORD

    DEFINE gr_ruta_modulo   RECORD LIKE seg_modulo.*
    DEFINE HOY              DATE
    DEFINE gs_codigo_afore  SMALLINT
    DEFINE enter            CHAR(001)
    DEFINE gc_usuario       CHAR(020)
    DEFINE lc_cadena        CHAR(2000)

END GLOBALS

-------------------------------------------------------------------------------

MAIN

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()                     -- ok
    CALL f_ini()                              -- ok
    CALL f_abre_ventana()                     -- ok
    CALL f_tablas_tmp()                       -- ok
    CALL f_genera_tmp_dis_cuenta(gr_fechas.*) -- ok

    CALL f_genera_reportes()

    CLOSE WINDOW win_rpt_anexos

END MAIN

-------------------------------------------------------------------------------
--- init : Inicializa las variables globales que se usaran en el programa    --
-------------------------------------------------------------------------------

FUNCTION f_ini() -- ok

  DEFINE lc_prepare  CHAR(300)

  LET HOY = TODAY

  CALL f_get_usuario()
       RETURNING gc_usuario

  ----- PARAMETROS DE ENTRADA -----
  LET gr_fechas.inicio    = ARG_VAL(1)
  LET gr_fechas.fin       = ARG_VAL(2)

  SELECT *
    INTO gr_ruta_modulo.*
    FROM seg_modulo
   WHERE modulo_cod = "ret"

  ----- CODIGOS AFORES -----
  SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local

  DISPLAY ""

END FUNCTION

-------------------------------------------------------------------------------
-- f_genera_reportes : Ejecuta las instrucciones generar los reportes de     --
--                     cifras de control para los anexos 134 y 137           --
-------------------------------------------------------------------------------

FUNCTION f_genera_reportes() -- ok

  DISPLAY " GENERANDO ARCHIVOS PLANOS DE REPORTE DE DETALLE ...       " AT 19,1 ATTRIBUTE(REVERSE)
  SLEEP 1

  CALL f_genera_detalle_134()
  CALL f_genera_plano_134()

  CALL f_genera_detalle_137()
  CALL f_genera_plano_137()

END FUNCTION

-------------------------------------------------------------------------------
-- f_genera_tmp_dis_cuenta : Genera el extracto de dis_cuenta que contiene   --
--                           unicamente los movimientos de retiros y de      --
--                           solicitud de saldo del periodo seleccionado     --
-------------------------------------------------------------------------------

FUNCTION f_genera_tmp_dis_cuenta(pr_fechas) --ok

  DEFINE pr_fechas RECORD
                     fecha_ini    DATE,
                     fecha_fin    DATE
                   END RECORD

  DISPLAY " OBTENIENDO DATOS DE REPORTES DE DETALLE  ...              " AT 19,1 ATTRIBUTE(REVERSE)

  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_cifras_cta_nxos
  WHENEVER ERROR STOP

  SELECT * , '8' origen_recursos
    FROM dis_cuenta
   WHERE fecha_conversion BETWEEN pr_fechas.fecha_ini AND pr_fechas.fecha_fin
     AND (
          tipo_movimiento IN (820, 825, 830, 840, 850, 860, 880,       -- RETIROS TOTALES
                              817, 818, 819,                           -- VIVIENDA VENTANILLA
                              841,                                     -- PMG
                              10 ,                                     -- ISR
                              835)                                     -- DISPOSICION
          OR

          tipo_movimiento IN (851, 852, 853, 854, 855, 857, 858,       -- RETIROS TOTALES
                              859, 864, 867,                           -- RETIROS TOTALES
                              10 )                                     -- ISR
         )
    INTO TEMP tmp_cifras_cta_nxos

  -----

  -- Se actualizan los movimientos de tipo I (disposiciones) a B
  UPDATE tmp_cifras_cta_nxos
     SET tipo_movimiento = 852
   WHERE tipo_movimiento = 864

  -----
  -- Se actualiza el tipo de prestacion a 07 (Banxico) para los registros con subcta 19
    UPDATE tmp_cifras_cta_nxos
    SET    origen_recursos  = "7"
    WHERE  subcuenta        = 19


  UPDATE STATISTICS FOR TABLE tmp_cifras_cta_nxos

  -- Todos los movimientos de vivienda por ley INFONAVIT se convierten
  -- a su retiro correspondiente

  CALL f_modifica_viv_infonavit()  --ok
  
  CALL f_modifica_cancelacion()

END FUNCTION

------------------------------------------------------------------------------
-- f_genera_detalle_134 : Genera e inserta en base de datos los registros    -
--                        del archivo de detalle para el anexo 134           -
------------------------------------------------------------------------------
FUNCTION f_genera_detalle_134()

  DEFINE lr_cuenta     RECORD
                         nss                     CHAR(11)        ,
                         curp                    CHAR(18)        ,
                         fecha_liquida           DATE            ,
                         folio                   DECIMAL(11,0)   ,
                         consecutivo             DECIMAL(11,0)   ,
                         tipo_movimiento         SMALLINT
                       END RECORD

  DEFINE lr_montos     RECORD
                         folio                   LIKE dis_cuenta.folio           ,
                         siefore                 LIKE dis_cuenta.siefore         ,
                         subcuenta               LIKE dis_cuenta.subcuenta       ,
                         monto_pesos             LIKE dis_cuenta.monto_en_pesos
                       END RECORD
  DEFINE lr_dat_not_viv RECORD
                        secuencia_pen      CHAR(2),
                        regimen            CHAR(2),
                        tipo_seguro        CHAR(2),
                        tipo_pension       CHAR(2),
                        tipo_prestacion    CHAR(2),
                        tipo_retiro        CHAR(1),
                        fecha_solicitud    DATE  ,
                        fecha_ini_pen      DATE
                        END RECORD


  DEFINE lr_resotx     RECORD LIKE ret_solicitud_tx.*
  DEFINE lr_pmg        RECORD LIKE pen_solicitud_pmg.*
  DEFINE lc_comando    CHAR(1000)
  DEFINE lc_basura     CHAR(01)
  DEFINE lc_tipodet    CHAR(03)
  DEFINE ls_encontrado SMALLINT
  DEFINE ls_cadena_aux VARCHAR(30)
  DEFINE v_valida      INTEGER
  DEFINE v_valida_viv  INTEGER
  DEFINE ls_curp_aux   CHAR(18)
  DEFINE aux_retitro   SMALLINT
  

  DEFINE lr_reg_134    RECORD
                         tipo_registro           CHAR(02)        ,   --01
                         nss                     CHAR(11)        ,   --02
                         paterno                 CHAR(40)        ,   --03
                         materno                 CHAR(40)        ,   --04
                         nombres                 CHAR(40)        ,   --05
                         curp                    CHAR(18)        ,   --06
                         secuencia_pen           CHAR(02)        ,   --07
                         regimen                 CHAR(02)        ,   --08
                         tipo_seguro             CHAR(02)        ,   --09
                         tipo_pension            CHAR(02)        ,   --10

                         tipo_prestacion         CHAR(02)        ,   --11
                         tipo_retiro             CHAR(01)        ,   --12
                         fecha_solicitud         DATE            ,   --13
                         fecha_operacion         DATE            ,   --14
                         resultado_diagnostico   CHAR(02)        ,   --15
                         motivo_diagnostico      CHAR(03)        ,   --16
                         estatus_vivienda        CHAR(02)        ,   --17
                         fecha_ini_pension       DATE            ,   --18
                         fecha_liquidacion       DATE            ,   --19
                         siefore                 CHAR(02)        ,   --20

                         mto_retiro_97           DECIMAL(22,2)   ,   --21
                         mto_cesantia_vejez      DECIMAL(22,2)   ,   --22
                         mto_cuota_social        DECIMAL(22,2)   ,   --23
                         mto_retiro_92           DECIMAL(22,2)   ,   --24
                         mto_vivienda_92         DECIMAL(22,2)   ,   --25
                         mto_vivienda_97         DECIMAL(22,2)   ,   --26
                         indicador_pension       CHAR(01),           --27
                         forma_pago              CHAR(03),           --28
                         medio_solic             CHAR(03)            --29
                       END RECORD

    DEFINE ls_tipo_pago       SMALLINT

  -- -----------------------------------------------------------------------------

  DISPLAY " GENERANDO REPORTE DE DETALLE - ANEXO 134 ...              " AT 19,1 ATTRIBUTE(REVERSE)
  SLEEP 1

  DECLARE cur_movs_IMSS CURSOR WITH HOLD FOR
      SELECT UNIQUE(nss)      ,
             curp             ,
             fecha_conversion ,
             folio            ,
             consecutivo_lote ,
             tipo_movimiento
        FROM tmp_cifras_cta_nxos
       WHERE tipo_movimiento IN (820, 825, 830, 840, 850, 860, 880,     -- RETIROS TOTALES
                                 817, 819,                              -- VIVIENDA VENTANILLA
                                 841,                                   -- PMG
                                 835)                                   -- DISPOSICION 
       ORDER BY 2, 1

  FOREACH cur_movs_IMSS INTO lr_cuenta.*
      LET v_valida = 0
      LET v_valida_viv = 0
      LET lr_reg_134.tipo_registro         = "02"                                   --01
      LET lr_reg_134.nss                   = lr_cuenta.nss                          --02
      LET lr_reg_134.medio_solic           = "001"                                  --29
       
      CALL f_retsoltx_imss(lr_cuenta.nss, lr_cuenta.consecutivo)
           RETURNING lr_resotx.*

      LET ls_encontrado = 0
     #  traer de afi_mae_afilado curp, validar que solo cuente con un solo curp
     #   si cuenta con mas de un curp  mandar a log con el nss y mensaje de que se cuenta con mas de un CURP

      CALL f_datos(lr_cuenta.nss, lr_cuenta.curp)
           RETURNING ls_curp_aux,
                     lr_reg_134.paterno,                                            --03
                     lr_reg_134.materno,                                            --04
                     lr_reg_134.nombres,                                            --05
                     ls_encontrado

      IF ls_encontrado = 0 THEN
         LET lr_reg_134.paterno  = lr_resotx.paterno_sol                            --03
         LET lr_reg_134.materno  = lr_resotx.materno_sol                            --04
         LET lr_reg_134.nombres  = lr_resotx.nombre_sol                             --05
      END IF
   LET lr_reg_134.curp = ls_curp_aux
   LET lr_reg_134.secuencia_pen         = lr_resotx.sec_pension                       --07
   LET lr_reg_134.regimen               = lr_resotx.regimen                           --08
   LET lr_reg_134.tipo_seguro           = lr_resotx.tipo_seguro                       --09
   LET lr_reg_134.tipo_pension          = lr_resotx.tipo_pension                      --10
   LET lr_reg_134.tipo_prestacion       = lr_resotx.tipo_prestacion USING "&&"
   LET lr_reg_134.fecha_ini_pension     = lr_resotx.fecha_ini_pen                    --18
   LET lr_reg_134.motivo_diagnostico    = lr_resotx.diag_registro
   LET lc_tipodet      = "302"

   LET lr_reg_134.tipo_retiro =  lr_resotx.tipo_retiro                                --12
   LET lr_reg_134.fecha_solicitud       = lr_resotx.fecha_solicitud                   --13
   CALL f_fecha_operacion(lr_cuenta.Folio, lr_resotx.tipo_retiro, 134)
           RETURNING lr_reg_134.fecha_operacion                                        --14

       IF lr_cuenta.tipo_movimiento = 817  THEN
          LET lr_reg_134.fecha_operacion = lr_cuenta.fecha_liquida                  --14
          LET lr_reg_134.motivo_diagnostico    = '400'                              --16
          CALL fn_obt_datos_valida_vivi(lr_cuenta.nss, lr_cuenta.consecutivo)
          RETURNING lr_dat_not_viv.*

          LET lr_reg_134.secuencia_pen = lr_dat_not_viv.secuencia_pen         --07
          LET lr_reg_134.regimen = lr_dat_not_viv.regimen                     --08
          LET lr_reg_134.tipo_seguro = lr_dat_not_viv.tipo_seguro             --09
          LET lr_reg_134.tipo_pension = lr_dat_not_viv.tipo_pension           --10
          LET lr_reg_134.tipo_prestacion = lr_dat_not_viv.tipo_prestacion USING "&&"     --11
          LET lr_reg_134.tipo_retiro = lr_dat_not_viv.tipo_retiro             --12
          LET lr_reg_134.fecha_solicitud = lr_dat_not_viv.fecha_solicitud     --13
          LET lr_reg_134.estatus_vivienda = '01'                            --17
          LET lr_reg_134.fecha_ini_pension = f_obtiene_fip(lr_cuenta.nss, lr_dat_not_viv.secuencia_pen)                       --18
          
          LET lr_reg_134.medio_solic           = "005"                                  --29
      END IF

      IF lr_cuenta.tipo_movimiento = 819 THEN
            LET lr_reg_134.secuencia_pen = 'NA'                                     --07
            LET lr_reg_134.regimen = '73'                                           --08
            LET lr_reg_134.tipo_seguro ='NA'                                        --09
            LET lr_reg_134.tipo_pension ='NA'                                       --10
            LET lr_reg_134.tipo_prestacion = '00'                                   --11
            LET lr_reg_134.tipo_retiro = '2'                                        --12
            LET lr_reg_134.fecha_solicitud = "01/01/0001"                           --13
            LET lr_reg_134.fecha_operacion = lr_cuenta.fecha_liquida                --14
            LET lr_reg_134.motivo_diagnostico    = '400'                              --16
            LET lr_reg_134.estatus_vivienda = '01'                                --17
            LET lc_tipodet      = "302"
          
          LET lr_reg_134.medio_solic           = "005"                                  --29

      END IF

        {SELECT estado_sub_viv
        INTO lr_reg_134.estatus_vivienda                                                  --17
        FROM ret_monto_viv rmv
        WHERE nss = lr_cuenta.nss
        AND consecutivo = lr_cuenta.consecutivo}

        
        LET lc_comando = '',
                        '\n SELECT FIRST 1 estado_sub_viv                     ',
                        '\n   FROM ret_monto_viv                       ',
                        '\n   WHERE nss          = "',lr_cuenta.nss,'"       ',
                        '\n   AND consecutivo   =  ',lr_cuenta.consecutivo,' ',
                        '\n   AND estado_sub_viv IS NOT NULL                 '
        PREPARE con_viv_134 FROM lc_comando
        EXECUTE con_viv_134 INTO lr_reg_134.estatus_vivienda
        

      LET lr_reg_134.fecha_liquidacion     = lr_cuenta.fecha_liquida                --19

      IF lr_resotx.diag_registro = "400" THEN
          LET lr_reg_134.resultado_diagnostico = "01"                                --15
        --LET lr_reg_134.motivo_diagnostico    = "   "                                --16
      ELSE
          IF lr_cuenta.tipo_movimiento = 817 OR lr_cuenta.tipo_movimiento = 819 THEN
            LET lr_reg_134.resultado_diagnostico = "01"                             --15
            --LET lr_reg_134.motivo_diagnostico    = "  "                           --16
          ELSE
             LET lr_reg_134.resultado_diagnostico = "01"                             --15
            LET lr_reg_134.motivo_diagnostico    = lr_resotx.diag_registro          --16

         END IF
      END IF

      LET lr_reg_134.siefore               =  "00"                                  --20
      LET lr_reg_134.mto_retiro_97         =  0                                     --21
      LET lr_reg_134.mto_cesantia_vejez    =  0                                     --22
      LET lr_reg_134.mto_cuota_social      =  0                                     --23
      LET lr_reg_134.mto_retiro_92         =  0                                     --24
      LET lr_reg_134.mto_vivienda_92       =  0                                     --25
      LET lr_reg_134.mto_vivienda_97       =  0                                     --26
      LET lr_reg_134.indicador_pension     =  "0"                                   --27

      -- Folio solicitud
      CASE lc_tipodet

          WHEN "302"
              IF lr_cuenta.tipo_movimiento <> 841 THEN

                  SELECT curp
                  INTO   lr_reg_134.curp
                  FROM   ret_solicitud_tx
                  WHERE  nss          = lr_cuenta.nss
                  AND    consecutivo  = lr_cuenta.consecutivo
                  GROUP BY 1

              ELSE
                  INITIALIZE lr_pmg.* TO NULL

                  LET lr_reg_134.indicador_pension = "1"

                  LET lc_comando = '',
                                   '\n SELECT *                                         ',
                                   '\n   FROM pen_solicitud_pmg                         ',
                                   '\n  WHERE nss          = "',lr_cuenta.nss,'"        ',
                                   '\n    AND consecutivo  =  ',lr_cuenta.consecutivo,' '

                  PREPARE sel_pmg FROM lc_comando
                  EXECUTE sel_pmg INTO lr_pmg.*

                  IF SQLCA.SQLCODE = NOTFOUND THEN
                     INITIALIZE lr_pmg.* TO NULL
                  ELSE

                     LET lr_reg_134.curp                     = lr_pmg.curp             --06
                     LET lr_reg_134.secuencia_pen            = lr_pmg.sec_pension      --07
                     LET lr_reg_134.regimen                  = lr_pmg.regimen          --08
                     LET lr_reg_134.tipo_seguro              = lr_pmg.tipo_seguro      --09
                     LET lr_reg_134.tipo_pension             = lr_pmg.tipo_pension     --10
                     LET lr_reg_134.tipo_prestacion          = '02'                    --11
                     LET lr_reg_134.tipo_retiro              = lr_pmg.tipo_retiro      --12
                     LET lr_reg_134.fecha_solicitud          = lr_pmg.fecha_solicitud  --13
                     LET lr_reg_134.fecha_operacion = f_fecha_operacion (lr_cuenta.Folio, lr_pmg.tipo_retiro, 135) --14
                     IF lr_reg_134.fecha_operacion IS NULL THEN
                         LET lr_reg_134.fecha_operacion = "01/01/0001"
                     END IF
                     IF lr_pmg.diag_registro = "400" OR lr_pmg.diag_registro = "E87" THEN
                         LET lr_reg_134.resultado_diagnostico = "01"                    --15
                         LET lr_reg_134.motivo_diagnostico   = lr_pmg.diag_registro     --16
                     ELSE
                         LET lr_reg_134.resultado_diagnostico = "02"                    --15
                        LET lr_reg_134.motivo_diagnostico    = lr_pmg.diag_registro    --16
                        LET lr_reg_134.fecha_liquidacion     = "01/01/0001"            --19
                     END IF

                     LET lr_reg_134.estatus_vivienda         = lr_pmg.estado_sub_viv   --17
                  END IF
                  -- CPL-2902 FIN
              END IF
      END CASE
      
      IF lr_reg_134.tipo_retiro = "D" OR lr_reg_134.tipo_retiro = "E" OR lr_reg_134.tipo_retiro = "J" THEN
         -- SE QUEDA CON LA ASIGNACION YA DADA
      ELSE
          LET lr_reg_134.fecha_ini_pension     = "01/01/0001"                        --18
      END IF


      IF lr_cuenta.tipo_movimiento = 819 THEN
         LET ls_cadena_aux = "819,818"
      ELSE
         LET ls_cadena_aux = lr_cuenta.tipo_movimiento
      END IF

      IF lr_cuenta.tipo_movimiento = 817 THEN
         LET ls_cadena_aux = "817,818"
      ELSE
         LET ls_cadena_aux = lr_cuenta.tipo_movimiento
      END IF

      LET lc_cadena = '',
                      '\n SELECT folio                ,                              ',
                      '\n        siefore              ,                              ',
                      '\n        subcuenta            ,                              ',
                      '\n        ROUND(NVL(SUM(monto_en_pesos) * -1, 0), 2)          ',
                      '\n   FROM tmp_cifras_cta_nxos                                 ',
                      '\n  WHERE nss              = "',lr_cuenta.nss,'"              ',
                      '\n    AND fecha_conversion = "',lr_cuenta.fecha_liquida,'"    ',
                      '\n    AND consecutivo_lote =  ',lr_cuenta.consecutivo,'       ',
                      '\n    AND folio            =  ',lr_cuenta.folio       ,'       ',
                      --'\n    AND tipo_movimiento IN (10,',ls_cadena_aux,')           ',
                      '\n  GROUP BY 1,2,3                                            ',
                      '\n  ORDER BY 1,2,3                                            '

      PREPARE cmimss FROM lc_cadena
      DECLARE cur_montos_IMSS CURSOR WITH HOLD FOR cmimss

      FOREACH cur_montos_IMSS INTO lr_montos.*

          CASE lr_montos.subcuenta

              -- Retiro 97
              WHEN 1
                  LET lr_reg_134.mto_retiro_97      = lr_reg_134.mto_retiro_97 + lr_montos.monto_pesos
                  LET lr_reg_134.siefore            = lr_montos.siefore USING "&&"

              -- CV
              WHEN 2
                  LET lr_reg_134.mto_cesantia_vejez = lr_reg_134.mto_cesantia_vejez + lr_montos.monto_pesos
                  LET lr_reg_134.siefore            = lr_montos.siefore USING "&&"

              WHEN 6
                  LET lr_reg_134.mto_cesantia_vejez = lr_reg_134.mto_cesantia_vejez + lr_montos.monto_pesos
                  LET lr_reg_134.siefore            = lr_montos.siefore USING "&&"

              WHEN 9
                  LET lr_reg_134.mto_cesantia_vejez = lr_reg_134.mto_cesantia_vejez + lr_montos.monto_pesos
                  LET lr_reg_134.siefore            = lr_montos.siefore USING "&&"

              -- CS
              WHEN 5
                  LET lr_reg_134.mto_cuota_social = lr_reg_134.mto_cuota_social + lr_montos.monto_pesos
                  LET lr_reg_134.siefore          = lr_montos.siefore USING "&&"

              -- Retiro 92
              WHEN 7
                  LET lr_reg_134.mto_retiro_92    = lr_reg_134.mto_retiro_92 + lr_montos.monto_pesos
                  LET lr_reg_134.siefore          = lr_montos.siefore USING "&&"

              -- Vivienda 92
              WHEN 8
                  LET lr_reg_134.mto_vivienda_92  = lr_reg_134.mto_vivienda_92 + lr_montos.monto_pesos

              -- Vivienda 97
              WHEN 4
                  LET lr_reg_134.mto_vivienda_97  = lr_reg_134.mto_vivienda_97 + lr_montos.monto_pesos

          END CASE

      END FOREACH
      FREE cur_montos_IMSS
      
      SELECT tipo_pago
      INTO   ls_tipo_pago
      FROM   ret_beneficiario
      WHERE  nss          = lr_cuenta.nss
      AND    consecutivo  = lr_cuenta.consecutivo
       
      CASE ls_tipo_pago
      WHEN 5
         LET lr_reg_134.forma_pago = '001'                                  --28
         
      WHEN 1
         LET lr_reg_134.forma_pago = '002'                                  --28
         
      OTHERWISE
         LET lr_reg_134.forma_pago = '003'                                  --28
      END CASE

      IF lr_reg_134.medio_solic = '005' THEN
        LET lr_reg_134.forma_pago = '003'                                  --28
      END IF 
      
      LET lr_reg_134.fecha_ini_pension = lr_reg_134.fecha_ini_pension CLIPPED
      
      IF lr_reg_134.fecha_ini_pension IS NULL OR lr_reg_134.fecha_ini_pension = "" THEN
          LET lr_reg_134.fecha_ini_pension     = "01/01/0001"                        --18
      END IF

      INSERT INTO safre_tmp:tmp_anexo07034
      VALUES (lr_reg_134.*)

      INITIALIZE lr_cuenta.* TO NULL

  END FOREACH
  FREE cur_movs_IMSS

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_detalle_137 : Genera e inserta en base de datos los registros    #
#                        del archivo de detalle para el anexo 115           #
#---------------------------------------------------------------------------#
FUNCTION f_genera_detalle_137()

  DEFINE lr_cuenta  RECORD
                      curp                   CHAR(18)        ,
                      nss                    CHAR(11)        ,
                      fecha_liquida          DATE            ,
                      folio                  DECIMAL(11,0)   ,
                      consecutivo            DECIMAL(11,0)   ,
                      tipo_movimiento        SMALLINT
                    END RECORD

  DEFINE lr_montos  RECORD
                      folio                  LIKE dis_cuenta.folio           ,
                      siefore                LIKE dis_cuenta.siefore         ,
                      subcuenta              LIKE dis_cuenta.subcuenta       ,
                      monto_pesos            LIKE dis_cuenta.monto_en_pesos
                    END RECORD

  DEFINE lr_resotx  RECORD LIKE ret_sol_issste_tx.*
  DEFINE lc_basura  CHAR(01)
  DEFINE lc_tipodet CHAR(03)
  DEFINE lc_comando CHAR(1500)
  DEFINE ls_curp_aux CHAR(18)


  DEFINE lr_reg_137 RECORD
                      tipo_registro          CHAR(02)        ,   --01
                      curp                   CHAR(18)        ,   --02
                      paterno                CHAR(40)        ,   --03
                      materno                CHAR(40)        ,   --04
                      nombres                CHAR(40)        ,   --05
                      secuencia_pen          CHAR(02)        ,   --06
                      regimen                CHAR(02)        ,   --07
                      tipo_seguro            CHAR(02)        ,   --08
                      tipo_pension           CHAR(02)        ,   --09
                      tipo_prestacion        CHAR(02)        ,   --10

                      tipo_retiro            CHAR(01)        ,   --11
                      fecha_solicitud        DATE            ,   --12
                      fecha_operacion        DATE            ,   --13
                      estatus_vivienda       CHAR(02)        ,   --14
                      fecha_ini_pension      DATE            ,   --15
                      fecha_liquidacion      DATE            ,   --16
                      origen_recursos        CHAR(01)        ,   --17
                      siefore                CHAR(02)        ,   --18
                      mto_retiro_08          DECIMAL(12,2)   ,   --19
                      mto_cesantia_vejez     DECIMAL(12,2)   ,   --20

                      mto_sar92              DECIMAL(12,2)   ,   --21
                      mto_ahorro_sol         DECIMAL(12,2)   ,   --22
                      mto_comp_retiro        DECIMAL(12,2)   ,   --23
                      mto_fovissste92        DECIMAL(12,2)   ,   --24
                      mto_fovissste08        DECIMAL(12,2)   ,   --25
                      forma_pago             CHAR(03)        ,   --26
                      medio_solic            CHAR(03)        ,   --27
                      mto_cuota_social       DECIMAL(12,2)       --28
                    END RECORD,
         lr_pmg RECORD 
             folio_lote          INTEGER ,
             curp                CHAR(18),   
             sec_pension         CHAR(02),   
             regimen             CHAR(02),   
             tipo_seguro         CHAR(02),   
             tipo_pension        CHAR(02),   
             tipo_prestacion     CHAR(02),
             tipo_retiro         CHAR(01),
             diag_registro       char(3),
             fecha_ini_pen       DATE    ,
             fecha_captura       DATE 
             END RECORD 
  DEFINE
      lc_regimen    CHAR(02)    ,
      lc_prestacion CHAR(03)    ,
      ls_encontrado SMALLINT    ,
      lc_origen      CHAR(1)

    DEFINE ls_tipo_pago       SMALLINT

  -- -----------------------------------------------------------------------------

  DISPLAY " GENERANDO REPORTE DE DETALLE - ANEXO 137 ...              " AT 19,1 ATTRIBUTE(REVERSE)

  -- DISPLAY "UNO"
  DECLARE cur_movs_ISSSTE CURSOR WITH HOLD FOR
    SELECT UNIQUE(a.n_unico)    ,
           b.nss                ,
           b.fecha_conversion   ,
           b.folio              ,
           b.consecutivo_lote   ,
           b.tipo_movimiento    ,
           b.origen_recursos
      FROM afi_mae_afiliado    a,
           tmp_cifras_cta_nxos b
     WHERE b.tipo_movimiento IN (851, 852, 853, 854, 855, 857, 858,       -- RETIROS TOTALES
                                 859, 864, 867)                           -- RETIROS TOTALES
                               # 10 )                                     -- ISR
       AND b.nss  = a.n_seguro
     ORDER BY 2, 1

  FOREACH cur_movs_ISSSTE INTO lr_cuenta.*,lc_origen

    -- DISPLAY "lr_cuenta", lr_cuenta.*
    INITIALIZE lr_reg_137.* TO NULL

    LET lr_reg_137.tipo_registro      = "02"                                         --01
    LET lr_reg_137.curp               = lr_cuenta.curp                               --02

    CALL f_retsoltx_issste(lr_cuenta.curp, lr_cuenta.consecutivo)
         RETURNING lr_resotx.*

    LET ls_encontrado = 0
    CALL f_datos(lr_cuenta.nss, lr_cuenta.curp)
         RETURNING ls_curp_aux,
                   lr_reg_137.paterno,                                               --03
                   lr_reg_137.materno,                                               --04
                   lr_reg_137.nombres,                                               --05
                   ls_encontrado

    IF ls_encontrado = 0 THEN
       LET lr_reg_137.paterno  = lr_resotx.paterno_afore                             --03
       LET lr_reg_137.materno  = lr_resotx.materno_afore                             --04
       LET lr_reg_137.nombres  = lr_resotx.nombre_afore                              --05
    END IF

    LET lr_reg_137.secuencia_pen      = lr_resotx.sec_pension                        --06
    --DISPLAY "secuencia_pen: ", lr_reg_137.secuencia_pen

    CASE lr_resotx.regimen
        WHEN 'RO'
            LET lr_reg_137.regimen = '02'
        WHEN 'DT'
            LET lr_reg_137.regimen = '01'                                            --07
    END CASE

    LET lr_reg_137.tipo_retiro        = lr_resotx.tipo_retiro                        --11
    IF lr_resotx.tipo_retiro = 'E' AND lr_cuenta.tipo_movimiento = 855 THEN
        LET lr_reg_137.tipo_seguro        = 'NA'                                     --08
        LET lr_reg_137.tipo_pension       = 'NA'                                     --09
        LET lr_reg_137.tipo_prestacion    = lr_resotx.tipo_prestacion USING "&&"     --10
        LET lr_reg_137.fecha_ini_pension  = '01/01/0001'                             --15
    ELSE

        LET lr_reg_137.tipo_seguro        = lr_resotx.tipo_seguro                    --08
        LET lr_reg_137.tipo_pension       = lr_resotx.tipo_pension                   --09
        LET lr_reg_137.tipo_prestacion    = lr_resotx.tipo_prestacion USING "&&"     --10
        LET lr_reg_137.fecha_ini_pension  = lr_resotx.fecha_ini_pen                  --15
    END IF

    -- DISPLAY "DOS"
  
    LET lc_tipodet = "302"
    LET lr_reg_137.fecha_solicitud    = lr_resotx.fecha_solicitud                    --12

    -- DISPLAY "TRES"
    CALL f_fecha_operacion(lr_cuenta.Folio, lr_resotx.tipo_retiro, 137)
         RETURNING lr_reg_137.fecha_operacion                                        --13

    LET lr_reg_137.estatus_vivienda   = ""                                           --14

    IF (lr_resotx.curp        IS NOT NULL)
       AND
       (lr_resotx.consecutivo IS NOT NULL)
       AND
       (lr_resotx.folio       IS NOT NULL) THEN

       -- DISPLAY "CUATRO"
       LET lc_comando = '',
                        '\n SELECT FIRST 1 estado_sub_viv                     ',
                        '\n   FROM ret_monto_viv_issste                       ',
                        '\n  WHERE curp          = "',lr_resotx.curp,'"       ',
                        '\n    AND consecutivo   =  ',lr_resotx.consecutivo,' ',
                        '\n    AND folio         =  ',lr_resotx.folio,'       ',
                        '\n    AND estado_sub_viv IS NOT NULL                 '

       -- DISPLAY lc_comando CLIPPED
       PREPARE con_viv FROM lc_comando
       EXECUTE con_viv INTO lr_reg_137.estatus_vivienda

       IF SQLCA.SQLCODE = NOTFOUND THEN
          LET lr_reg_137.estatus_vivienda   = ""                                     --14
       END IF

    ELSE

       -- DISPLAY "curp        :",lr_resotx.curp
       -- DISPLAY "consecutivo :",lr_resotx.consecutivo
       -- DISPLAY "folio       :",lr_resotx.folio
       -- DISPLAY "NO HAY ESTATUS DE VIVIENDA"

      LET lr_reg_137.estatus_vivienda   = ""                                        --14

    END IF


    LET lr_reg_137.fecha_liquidacion  = lr_cuenta.fecha_liquida                      --16

    LET lr_reg_137.origen_recursos    = lc_origen                                          --17
    LET lr_reg_137.siefore            = "  "                                         --18
    LET lr_reg_137.mto_retiro_08      = 0                                            --19
    LET lr_reg_137.mto_cesantia_vejez = 0                                            --20
    LET lr_reg_137.mto_sar92          = 0                                            --21
    LET lr_reg_137.mto_ahorro_sol     = 0                                            --22
    LET lr_reg_137.mto_comp_retiro    = 0                                            --23
    LET lr_reg_137.mto_fovissste92    = 0                                            --24
    LET lr_reg_137.mto_fovissste08    = 0                                            --25
    LET lr_reg_137.mto_cuota_social   = 0                                            --28
    
    SELECT tipo_pago
    INTO   ls_tipo_pago
    FROM   ret_beneficiario
    WHERE  nss          = lr_cuenta.nss
    AND    consecutivo  = lr_cuenta.consecutivo
     
    CASE ls_tipo_pago
    WHEN 5
       LET lr_reg_137.forma_pago = '001'                                             --26
       
    WHEN 1
       LET lr_reg_137.forma_pago = '002'                                             --26
       
    OTHERWISE
       LET lr_reg_137.forma_pago = '003'                                             --26
    END CASE
      
    LET lr_reg_137.medio_solic        = "001"                                        --27

    -- Folio solicitud
    CASE lc_tipodet

        WHEN "302"
            IF lr_cuenta.tipo_movimiento <> 857 AND lr_cuenta.tipo_movimiento <> 859 THEN
                SELECT regimen
                INTO   lc_regimen
                FROM   ret_sol_issste_tx
                WHERE  curp         = lr_cuenta.curp
                AND    consecutivo  = lr_cuenta.consecutivo
            ELSE
               INITIALIZE lr_pmg.* TO NULL 
                  LET lc_comando = '',
                                   '\n select   folio_lote    ,   ',
                                   '\n          curp          ,   ',
                                   '\n          sec_pension   ,   ',
                                   '\n          regimen       ,   ',
                                   '\n          tipo_seguro   ,   ',
                                   '\n          tipo_pension  ,   ',
                                   '\n          tipo_prestacion  ,',
                                   '\n          tipo_retiro      ,',
                                   '\n          diag_registro    ,',
                                   '\n          fecha_ini_pen    ,',
                                   '\n          fecha_captura     ',
                                   '\n from     pen_solicitud_iss ',
                                   '\n  WHERE nss          = "',lr_cuenta.nss,'"        ',
                                   '\n    AND consecutivo  =  ',lr_cuenta.consecutivo,' '   
                                   
                  PREPARE sel_pmg_iss FROM lc_comando
                  EXECUTE sel_pmg_iss INTO lr_pmg.*
                  
                  IF SQLCA.SQLCODE = NOTFOUND THEN 
                     INITIALIZE lr_pmg.* TO NULL                        
                  ELSE                                               
                     
                     LET lr_reg_137.curp                     = lr_pmg.curp             --06        
                     LET lr_reg_137.secuencia_pen            = lr_pmg.sec_pension      --07
                     LET lr_reg_137.regimen                  = lr_pmg.regimen          --08 
                     LET lr_reg_137.tipo_seguro              = lr_pmg.tipo_seguro      --09
                     LET lr_reg_137.tipo_pension             = lr_pmg.tipo_pension     --10
                     LET lr_reg_137.tipo_prestacion          = lr_pmg.tipo_prestacion  --11
                     LET lr_reg_137.tipo_retiro              = lr_pmg.tipo_retiro      --12
                     LET lr_reg_137.fecha_solicitud          = lr_pmg.fecha_captura  --13
                     LET lr_reg_137.estatus_vivienda          = "00"                 --14
                     CALL f_fecha_operacion (lr_cuenta.folio, lr_pmg.tipo_retiro,137)
                            RETURNING lr_reg_137.fecha_operacion                         --19
                     IF lr_reg_137.fecha_operacion IS NULL THEN 
                         LET lr_reg_137.fecha_operacion = "01/01/0001"
                     END IF 
                     LET lr_reg_137.fecha_ini_pension        = lr_pmg.fecha_ini_pen    --18 
                     CASE lr_pmg.regimen
                         WHEN 'RO'
                             LET lr_reg_137.regimen = '02'
                         WHEN 'DT'
                             LET lr_reg_137.regimen = '01'                                            --07
                     END CASE                
                  END IF

            END IF

    END CASE

    -- DISPLAY "CINCO"
    LET lc_comando = '',
                     '\n SELECT folio                ,                           ',
                     '\n        siefore              ,                           ',
                     '\n        subcuenta            ,                           ',
                     '\n        ROUND(NVL(SUM(monto_en_pesos) * -1, 0), 2)       ',
                     '\n   FROM tmp_cifras_cta_nxos                              ',
                     '\n  WHERE nss              = "',lr_cuenta.nss,'"           ',
                     '\n    AND fecha_conversion = "',lr_cuenta.fecha_liquida,'" ',
                     '\n    AND consecutivo_lote =  ',lr_cuenta.consecutivo,'    ',
                     '\n    AND folio            =  ',lr_cuenta.folio          ,'',
                     '\n    AND origen_recursos  =  ',lc_origen                ,'',
                     '\n  GROUP BY 1,2,3                                         ',
                     '\n  ORDER BY 1,2,3                                         '
    PREPARE cmi FROM lc_comando

    -- DISPLAY lc_comando

    DECLARE cur_montos_ISSSTE CURSOR WITH HOLD FOR cmi
    FOREACH cur_montos_ISSSTE INTO lr_montos.*

      CASE lr_montos.subcuenta

           -- Retiro 08
           WHEN 30
               LET lr_reg_137.mto_retiro_08 = lr_reg_137.mto_retiro_08 +  lr_montos.monto_pesos
               LET lr_reg_137.siefore       = lr_montos.siefore USING "&&"

           -- CV
           WHEN 31
               LET lr_reg_137.mto_cesantia_vejez = lr_reg_137.mto_cesantia_vejez +  lr_montos.monto_pesos
               LET lr_reg_137.siefore            = lr_montos.siefore USING "&&"

           -- CS
           WHEN 32
               LET lr_reg_137.mto_cuota_social = lr_reg_137.mto_cuota_social +  lr_montos.monto_pesos     --28
               LET lr_reg_137.siefore         = lr_montos.siefore USING "&&"

           -- SAR 92
           WHEN 13
               LET lr_reg_137.mto_sar92       = lr_reg_137.mto_sar92 + lr_montos.monto_pesos
               LET lr_reg_137.siefore         = lr_montos.siefore USING "&&"

           -- SAR 92 BANXICO
           WHEN 19
               LET lr_reg_137.mto_sar92       = lr_reg_137.mto_sar92 + lr_montos.monto_pesos

           -- Fovissste 92
           WHEN 14
               LET lr_reg_137.mto_fovissste92 = lr_reg_137.mto_fovissste92 +  lr_montos.monto_pesos

           -- Fovissste 08
           WHEN 35
               LET lr_reg_137.mto_fovissste08 = lr_reg_137.mto_fovissste08 +  lr_montos.monto_pesos

           -- Ahorro solidario
           WHEN 33
               LET lr_reg_137.mto_ahorro_sol  = lr_reg_137.mto_ahorro_sol +  lr_montos.monto_pesos
               LET lr_reg_137.siefore         = lr_montos.siefore USING "&&"

           WHEN 34
               LET lr_reg_137.mto_ahorro_sol  = lr_reg_137.mto_ahorro_sol +  lr_montos.monto_pesos
               LET lr_reg_137.siefore         = lr_montos.siefore USING "&&"

           -- Complementarias de retiro
           WHEN 11
               LET lr_reg_137.mto_comp_retiro = lr_reg_137.mto_comp_retiro +  lr_montos.monto_pesos
               LET lr_reg_137.siefore         = lr_montos.siefore USING "&&"
           WHEN 12
               LET lr_reg_137.mto_comp_retiro = lr_reg_137.mto_comp_retiro +  lr_montos.monto_pesos
               LET lr_reg_137.siefore         = lr_montos.siefore USING "&&"
           WHEN 24
               LET lr_reg_137.mto_comp_retiro = lr_reg_137.mto_comp_retiro +  lr_montos.monto_pesos
               LET lr_reg_137.siefore         = lr_montos.siefore USING "&&"
           WHEN 25
               LET lr_reg_137.mto_comp_retiro = lr_reg_137.mto_comp_retiro +  lr_montos.monto_pesos
               LET lr_reg_137.siefore         = lr_montos.siefore USING "&&"
      END CASE

    END FOREACH
    FREE cur_montos_ISSSTE

    
    IF lr_reg_137.siefore = "0" OR lr_reg_137.siefore IS NULL OR
       lr_reg_137.siefore = ""  OR lr_reg_137.siefore = "  "  THEN
       LET lr_reg_137.siefore = "00"
    END IF
    IF lr_reg_137.secuencia_pen = "0" OR lr_reg_137.secuencia_pen IS NULL OR
       lr_reg_137.secuencia_pen = "" OR lr_reg_137.secuencia_pen = " 0"   THEN
       LET lr_reg_137.secuencia_pen = "  "
    END IF
    IF lr_reg_137.fecha_ini_pension IS NULL THEN 
        LET lr_reg_137.fecha_ini_pension = '01/01/0001'
    END IF 
    INSERT INTO safre_tmp:tmp_anexo07037
    VALUES (lr_reg_137.*)

    INITIALIZE lr_cuenta.* TO NULL

  END FOREACH
  FREE cur_movs_ISSSTE

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_plano_134 : Genera el archivo plano de las cifras de control del #
#                      anexo 134                                            #
#---------------------------------------------------------------------------#
FUNCTION f_genera_plano_134()

    DEFINE pr_encabezado    RECORD
                              tipo_reg                CHAR(002),
                              identifica              CHAR(002),
                              tipo_ent                CHAR(003),
                              clave_ent               CHAR(003),
                              fecha_ip                CHAR(008),
                              fecha_fp                CHAR(008),
                              complemento             CHAR(262)
                            END RECORD

    DEFINE lr_detalle_0703  RECORD
                              tipo_registro           CHAR(02)        ,   --01
                              nss                     CHAR(11)        ,   --02
                              paterno                 CHAR(40)        ,   --03
                              materno                 CHAR(40)        ,   --04
                              nombres                 CHAR(40)        ,   --05
                              curp                    CHAR(18)        ,   --06
                              secuencia_pen           CHAR(02)        ,   --07
                              regimen                 CHAR(02)        ,   --08
                              tipo_seguro             CHAR(02)        ,   --09
                              tipo_pension            CHAR(02)        ,   --10

                              tipo_prestacion         CHAR(02)        ,   --11
                              tipo_retiro             CHAR(01)        ,   --12
                              fecha_solicitud         DATE            ,   --13
                              fecha_operacion         DATE            ,   --14
                              resultado_diagnostico   CHAR(02)        ,   --15
                              motivo_diagnostico      CHAR(03)        ,   --16
                              estatus_vivienda        CHAR(02)        ,   --17
                              fecha_ini_pension       DATE            ,   --18
                              fecha_liquidacion       DATE            ,   --19
                              siefore                 CHAR(02)        ,   --20

                              mto_retiro_97           DECIMAL(12,2)   ,   --21
                              mto_cesantia_vejez      DECIMAL(12,2)   ,   --22
                              mto_cuota_social        DECIMAL(12,2)   ,   --23
                              mto_retiro_92           DECIMAL(12,2)   ,   --24
                              mto_vivienda_92         DECIMAL(12,2)   ,   --25
                              mto_vivienda_97         DECIMAL(12,2)   ,   --26
                              indicador_pension       CHAR(01),           --27
                              forma_pago              CHAR(03),           --28
                              medio_solic             CHAR(03)            --29
                            END RECORD

    DEFINE
        lc_ruta             CHAR(100)   ,
        lc_nombre           VARCHAR(30)   ,
        lc_comando          CHAR(200)

    -- -----------------------------------------------------------------------------

    LET lc_nombre   = HOY USING "YYYYMMDD","_AF_",gs_codigo_afore USING "&&&","_007_00000.024"

    LET lc_ruta     = gr_ruta_modulo.ruta_envio CLIPPED , "/", lc_nombre
    LET lc_ruta     = lc_ruta CLIPPED

    LET pr_encabezado.tipo_reg    = "01"
    LET pr_encabezado.identifica  = "07"
    LET pr_encabezado.tipo_ent    = "001"  -- AFORE
    LET pr_encabezado.clave_ent   = gs_codigo_afore    --"568"  -- COPPEL
    LET pr_encabezado.fecha_ip    = gr_fechas.inicio  USING "YYYYMMDD"
    LET pr_encabezado.fecha_fp    = gr_fechas.fin     USING "YYYYMMDD"
    LET pr_encabezado.complemento = ""

    START REPORT rpt_34 TO lc_ruta

    INITIALIZE lr_detalle_0703.* TO NULL

    DECLARE cur_rep_IMSS CURSOR FOR
        SELECT *
          FROM safre_tmp:tmp_anexo07034
         WHERE nss IS NOT NULL
         ORDER BY 1, 2

    FOREACH cur_rep_IMSS INTO lr_detalle_0703.*

        OUTPUT TO REPORT rpt_34(pr_encabezado.*, lr_detalle_0703.*)
        INITIALIZE lr_detalle_0703.* TO NULL

    END FOREACH

    FINISH REPORT rpt_34

    -- DISPLAY lc_nombre

    -- Asigna permisos
    LET lc_comando = "chmod 777 ", lc_ruta
    LET lc_comando = lc_comando CLIPPED
    RUN lc_comando

   CALL  f_lib_borra_lineas(gr_ruta_modulo.ruta_envio, lc_nombre)

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_plano_137 : Genera el archivo plano de las cifras de control del #
#                      anexo 137                                            #
#---------------------------------------------------------------------------#
FUNCTION f_genera_plano_137()

    DEFINE pr_encabezado   RECORD
                            tipo_reg                CHAR(002),
                            identifica              CHAR(002),
                            tipo_ent                CHAR(003),
                            clave_ent               CHAR(003),
                            fecha_ip                CHAR(008),
                            fecha_fp                CHAR(008),
                            complemento             CHAR(272)
                          END RECORD


    DEFINE lr_detalle_0704 RECORD
                             tipo_registro          CHAR(02)        ,   --01
                             curp                   CHAR(18)        ,   --02
                             paterno                CHAR(40)        ,   --03
                             materno                CHAR(40)        ,   --04
                             nombres                CHAR(40)        ,   --05
                             secuencia_pen          CHAR(02)        ,   --06
                             regimen                CHAR(02)        ,   --07
                             tipo_seguro            CHAR(02)        ,   --08
                             tipo_pension           CHAR(02)        ,   --09
                             tipo_prestacion        CHAR(02)        ,   --10

                             tipo_retiro            CHAR(01)        ,   --11
                             fecha_solicitud        DATE            ,   --12
                             fecha_operacion        DATE            ,   --13
                             estatus_vivienda       CHAR(02)        ,   --14
                             fecha_ini_pension      DATE            ,   --15
                             fecha_liquidacion      DATE            ,   --16
                             origen_recursos        CHAR(01)        ,   --17
                             siefore                CHAR(02)        ,   --18
                             mto_retiro_08          DECIMAL(12,2)   ,   --19
                             mto_cesantia_vejez     DECIMAL(12,2)   ,   --20

                             mto_sar92              DECIMAL(12,2)   ,   --21
                             mto_ahorro_sol         DECIMAL(12,2)   ,   --22
                             mto_comp_retiro        DECIMAL(12,2)   ,   --23
                             mto_fovissste92        DECIMAL(12,2)   ,   --24
                             mto_fovissste08        DECIMAL(12,2)   ,   --25
                             forma_pago             CHAR(03)        ,   --26
                             medio_solic            CHAR(03)        ,   --27
                             mto_cuota_social       DECIMAL(12,2)       --28
                          END RECORD

    DEFINE
        lc_ruta             CHAR(100),
        lc_nombre           VARCHAR(30),
        lc_comando          CHAR(200)

    -- -----------------------------------------------------------------------------

    LET lc_nombre   = HOY USING "YYYYMMDD","_AF_",gs_codigo_afore USING "&&&","_018_00000.008"

    LET lc_ruta     = gr_ruta_modulo.ruta_envio CLIPPED , "/", lc_nombre
    LET lc_ruta     = lc_ruta CLIPPED

    LET pr_encabezado.tipo_reg     = "01"
    LET pr_encabezado.identifica   = "18"
    LET pr_encabezado.tipo_ent     = "001"
    LET pr_encabezado.clave_ent    = "568"
    LET pr_encabezado.fecha_ip     = gr_fechas.inicio  USING "YYYYMMDD"
    LET pr_encabezado.fecha_fp     = gr_fechas.fin     USING "YYYYMMDD"
    LET pr_encabezado.complemento  = ""

    START REPORT rpt_37 TO lc_ruta

    -- INITIALIZE lr_detalle_0704.* TO NULL

    DECLARE cur_rep_ISSSTE CURSOR FOR
        SELECT *
          FROM safre_tmp:tmp_anexo07037
         WHERE curp IS NOT NULL
        ORDER BY 1, 2

    FOREACH cur_rep_ISSSTE INTO lr_detalle_0704.*
    --DISPLAY "sec_pen: ", lr_detalle_0704.secuencia_pen

        OUTPUT TO REPORT rpt_37(pr_encabezado.*, lr_detalle_0704.*)
        -- INITIALIZE lr_detalle_0704.* TO NULL

    END FOREACH

    FINISH REPORT rpt_37

    -- DISPLAY lc_nombre

    -- Asigna permisos
    LET lc_comando = "chmod 777 ", lc_ruta
    LET lc_comando = lc_comando CLIPPED
    RUN lc_comando
    
    CALL  f_lib_borra_lineas(gr_ruta_modulo.ruta_envio, lc_nombre)

END FUNCTION



-------------------------------------------------------------------------------
-- f_abre_ventana : Abre la forma donde se depliegan los mensajes de al      --
--                  usuario en la generacion del anexo                       --
-------------------------------------------------------------------------------

FUNCTION f_abre_ventana() -- ok

  OPEN WINDOW win_rpt_anexos AT 4,4 WITH 20 ROWS, 75 COLUMNS
       ATTRIBUTE(BORDER, PROMPT LINE LAST -1)

    DISPLAY " Ctrl-C : Salir                                            Esc : Ejecutar  " AT 1,2 ATTRIBUTE(REVERSE)
    DISPLAY " RETL837D           GENERACION DE LOS ANEXOS 134 Y 137                     " AT 3,2 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

  CURRENT WINDOW IS win_rpt_anexos

END FUNCTION

------------------------------------------------------------------------------
-- f_modifica_viv_infonavit : Busca y modifica el tipo de movimiento 817 de  -
--                            los registros que tengan pago de vivienda de   -
--                            acuerdo a los cambios de la ley de infonavit   -
------------------------------------------------------------------------------
FUNCTION f_modifica_viv_infonavit() -- ok

  DEFINE lr_solicitud   RECORD
                          folio           LIKE ret_solicitud_tx.folio         ,
                          nss             LIKE ret_solicitud_tx.nss           ,
                          consecutivo     LIKE ret_solicitud_tx.consecutivo
                        END RECORD

  DEFINE ls_movimiento  SMALLINT

  -----------------------------------------------------------------------------

  INITIALIZE lr_solicitud.* TO NULL

  DECLARE cur_817 CURSOR FOR
   SELECT folio            ,
          nss              ,
          consecutivo_lote
     FROM tmp_cifras_cta_nxos
    WHERE tipo_movimiento  IN (817, 819)

  FOREACH cur_817 INTO lr_solicitud.*

    SELECT A.movimiento
      INTO ls_movimiento
      FROM tab_retiro A         ,
           ret_solicitud_tx B
     WHERE A.tipo_retiro      = B.tipo_retiro
       AND B.folio            = lr_solicitud.folio
       AND B.nss              = lr_solicitud.nss
       AND B.consecutivo      = lr_solicitud.consecutivo
       AND B.estado_solicitud = 8

    IF SQLCA.SQLCODE = NOTFOUND OR ls_movimiento IS NULL THEN
       -- NO SE ACTUALIZA
    ELSE
       UPDATE tmp_cifras_cta_nxos
          SET tipo_movimiento  = ls_movimiento
        WHERE folio            = lr_solicitud.folio
          AND nss              = lr_solicitud.nss
          AND consecutivo_lote = lr_solicitud.consecutivo
          AND tipo_movimiento  IN (817,819)
    END IF

  END FOREACH
  FREE cur_817

END FUNCTION

------------------------------------------------------------------------------
-- f_modifica_cancelacion   : Busca y modifica el tipo de movimiento 818     -
--                            si es el unico movimiento que se tiene         -
------------------------------------------------------------------------------
FUNCTION f_modifica_cancelacion() -- ok

  DEFINE lr_solicitud   RECORD
    folio              LIKE dis_cuenta.folio            ,
    nss                LIKE dis_cuenta.nss              ,
    consecutivo        LIKE dis_cuenta.consecutivo_lote ,
    fecha_conversion   LIKE dis_cuenta.fecha_conversion ,
    id_aportante       LIKE dis_cuenta.id_aportante
  END RECORD

  DEFINE ls_movimiento  SMALLINT

  -----------------------------------------------------------------------------

  INITIALIZE lr_solicitud.*, ls_movimiento TO NULL

  DECLARE cur_818 CURSOR FOR
   SELECT folio            ,
          nss              ,
          consecutivo_lote ,
          fecha_conversion ,
          id_aportante
     FROM tmp_cifras_cta_nxos
    WHERE tipo_movimiento  = 818

  FOREACH cur_818 INTO lr_solicitud.*

    SELECT UNIQUE tipo_movimiento
      INTO ls_movimiento
     FROM tmp_cifras_cta_nxos
     WHERE folio             = lr_solicitud.folio
       AND nss               = lr_solicitud.nss
       AND consecutivo_lote  = lr_solicitud.consecutivo
       AND fecha_conversion  = lr_solicitud.fecha_conversion
       AND tipo_movimiento   IN  (817,819)

    IF SQLCA.SQLCODE = NOTFOUND OR ls_movimiento IS NULL THEN
       IF lr_solicitud.id_aportante CLIPPED = "CANINT" THEN
           UPDATE tmp_cifras_cta_nxos
              SET tipo_movimiento  = 817
            WHERE folio            = lr_solicitud.folio
              AND nss              = lr_solicitud.nss
              AND consecutivo_lote = lr_solicitud.consecutivo
              AND fecha_conversion = lr_solicitud.fecha_conversion
              AND tipo_movimiento  = 818
       END IF
       
       IF lr_solicitud.id_aportante CLIPPED = "GPO234" THEN
           UPDATE tmp_cifras_cta_nxos
              SET tipo_movimiento  = 819
            WHERE folio            = lr_solicitud.folio
              AND nss              = lr_solicitud.nss
              AND consecutivo_lote = lr_solicitud.consecutivo
              AND fecha_conversion = lr_solicitud.fecha_conversion
              AND tipo_movimiento  = 818
       END IF
    END IF
    INITIALIZE lr_solicitud.*, ls_movimiento TO NULL

  END FOREACH
  FREE cur_818

END FUNCTION
--------------------------------------------------------------------------------
--fn_obt_datos_valida_vivi : Obtiene los datos para la operacion 60
--
--------------------------------------------------------------------------------
FUNCTION fn_obt_datos_valida_vivi(p_nss,p_consecutivo)
DEFINE  p_nss            CHAR(18),
        p_consecutivo    DECIMAL(11,0)
DEFINE  lr_datos_viv       RECORD
        secuencia_pen      CHAR(2),                                          --07
        regimen            CHAR(2),                                          --08
        tipo_seguro        CHAR(2),                                          --09
        tipo_pension       CHAR(2),                                          --10
        tipo_prestacion    CHAR(2),                                          --11
        tipo_retiro        CHAR(1),                                          --12
        fecha_solicitud    DATE ,                                            --13
        fecha_captura      DATE
        END RECORD

     SELECT sec_pension,
            regimen,
            tipo_seguro,
            tipo_pension,
            tipo_prestacion,
            tipo_retiro,
            fecha_carga,
            fecha_captura
     INTO lr_datos_viv.secuencia_pen,
          lr_datos_viv.regimen,
          lr_datos_viv.tipo_seguro,
          lr_datos_viv.tipo_pension,
          lr_datos_viv.tipo_prestacion,
          lr_datos_viv.tipo_retiro,
          lr_datos_viv.fecha_solicitud,
          lr_datos_viv.fecha_captura
     FROM ret_notifica_vivienda
     WHERE nss = p_nss
     AND consecutivo = p_consecutivo
    AND estado = 8

  RETURN lr_datos_viv.*

END FUNCTION

-----------------------------------------------------------------------------
-- f_tablas_tmp : Genera las tablas temporales donde se almacenan          --
--                registros para los anexos 114 y 115                      --
-----------------------------------------------------------------------------

FUNCTION f_tablas_tmp()  -- ok

  DATABASE safre_tmp

  WHENEVER ERROR CONTINUE
      DROP TABLE tmp_anexo07034
      DROP TABLE tmp_anexo07037
  WHENEVER ERROR STOP

  --------------------------------

  CREATE TABLE tmp_anexo07034(
      tipo_registro           CHAR(02)        ,   --01
      nss                     CHAR(11)        ,   --02
      paterno                 CHAR(40)        ,   --03
      materno                 CHAR(40)        ,   --04
      nombres                 CHAR(40)        ,   --05
      curp                    CHAR(18)        ,   --06
      secuencia_pen           CHAR(02)        ,   --07
      regimen                 CHAR(02)        ,   --08
      tipo_seguro             CHAR(02)        ,   --09
      tipo_pension            CHAR(02)        ,   --10

      tipo_prestacion         CHAR(02)        ,   --11
      tipo_retiro             CHAR(01)        ,   --12
      fecha_solicitud         DATE            ,   --13
      fecha_operacion         DATE            ,   --14
      resultado_diagnostico   CHAR(02)        ,   --15
      motivo_diagnostico      CHAR(03)        ,   --16
      estatus_vivienda        CHAR(02)        ,   --17
      fecha_ini_pension       DATE            ,   --18
      fecha_liquidacion       DATE            ,   --19
      siefore                 CHAR(02)        ,   --20

      mto_retiro_97           DECIMAL(22,2)   ,   --21
      mto_cesantia_vejez      DECIMAL(22,2)   ,   --22
      mto_cuota_social        DECIMAL(22,2)   ,   --23
      mto_retiro_92           DECIMAL(22,2)   ,   --24
      mto_vivienda_92         DECIMAL(22,2)   ,   --25
      mto_vivienda_97         DECIMAL(22,2)   ,   --26
      indicador_pension       CHAR(01)        ,   --27
      forma_pago              CHAR(03)        ,   --28
      medio_solic             CHAR(03)            --29
      )

  GRANT ALL ON tmp_anexo07034 TO PUBLIC

  --------------------------------

  CREATE TABLE tmp_anexo07037(
      tipo_registro           CHAR(02)        ,   --01
      curp                    CHAR(18)        ,   --02
      paterno                 CHAR(40)        ,   --03
      materno                 CHAR(40)        ,   --04
      nombres                 CHAR(40)        ,   --05
      secuencia_pen           CHAR(02)        ,   --06
      regimen                 CHAR(02)        ,   --07
      tipo_seguro             CHAR(02)        ,   --08
      tipo_pension            CHAR(02)        ,   --09
      tipo_prestacion         CHAR(02)        ,   --10

      tipo_retiro             CHAR(01)        ,   --11
      fecha_solicitud         DATE            ,   --12
      fecha_operacion         DATE            ,   --13
      estatus_vivienda        CHAR(02)        ,   --14
      fecha_ini_pension       DATE            ,   --15
      fecha_liquidacion       DATE            ,   --16
      origen_recursos         CHAR(01)        ,   --17
      siefore                 CHAR(02)        ,   --18
      mto_retiro_08           DECIMAL(12,2)   ,   --19
      mto_cesantia_vejez      DECIMAL(12,2)   ,   --20

      mto_sar92               DECIMAL(12,2)   ,   --21
      mto_ahorro_sol          DECIMAL(12,2)   ,   --22
      mto_comp_retiro         DECIMAL(12,2)   ,   --23
      mto_fovissste92         DECIMAL(12,2)   ,   --24
      mto_fovissste08         DECIMAL(12,2)   ,   --25
      forma_pago              CHAR(03)        ,   --26
      medio_solic             CHAR(03)        ,   --27
      mto_cuota_social        DECIMAL(12,2)       --28
      )

  GRANT ALL ON tmp_anexo07037 TO PUBLIC

  --------------------------------

  DATABASE safre_af

END FUNCTION

------------------------------------------------------------------------------
-- rpt_34 : Reporte que genera el archivo plano de cifras del anexo 134    -
------------------------------------------------------------------------------
REPORT rpt_34(pr_encabezado, pr_detalle_34)  --ok

  DEFINE pr_encabezado    RECORD
                           tipo_reg                CHAR(002),
                           identifica              CHAR(002),
                           tipo_ent                CHAR(003),
                           clave_ent               CHAR(003),
                           fecha_ip                CHAR(008),
                           fecha_fp                CHAR(008),
                           complemento             CHAR(262)
                         END RECORD

  DEFINE pr_detalle_34    RECORD
                            tipo_registro           CHAR(02),       --01
                            nss                     CHAR(11),       --02
                            paterno                 CHAR(40),       --03
                            materno                 CHAR(40),       --04
                            nombres                 CHAR(40),       --05
                            curp                    CHAR(18),       --06
                            secuencia_pen           CHAR(02),       --07
                            regimen                 CHAR(02),       --08
                            tipo_seguro             CHAR(02),       --09
                            tipo_pension            CHAR(02),       --10

                            tipo_prestacion         CHAR(02),       --11
                            tipo_retiro             CHAR(01),       --12
                            fecha_solicitud         DATE    ,       --13
                            fecha_operacion         DATE    ,       --14
                            resultado_diagnostico   CHAR(02),       --15
                            motivo_diagnostico      CHAR(03),       --16
                            estatus_vivienda        CHAR(02),       --17
                            fecha_ini_pension       DATE    ,       --18
                            fecha_liquidacion       DATE    ,       --19
                            siefore                 CHAR(02),       --20

                            mto_retiro_97           DECIMAL(12,2),  --21
                            mto_cesantia_vejez      DECIMAL(12,2),  --22
                            mto_cuota_social        DECIMAL(12,2),  --23
                            mto_retiro_92           DECIMAL(12,2),  --24
                            mto_vivienda_92         DECIMAL(12,2),  --25
                            mto_vivienda_97         DECIMAL(12,2),  --26
                            indicador_pension       CHAR(01),       --27
                            forma_pago              CHAR(03),       --28
                            medio_solic             CHAR(03)        --29
                          END RECORD

  ----------------------------------------------------------------------------

  OUTPUT
      PAGE LENGTH   10
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

  FORMAT
     FIRST PAGE HEADER
        PRINT
            pr_encabezado.tipo_reg    ,
            pr_encabezado.identifica  ,
            pr_encabezado.tipo_ent    ,
            pr_encabezado.clave_ent   ,
            pr_encabezado.fecha_ip    ,
            pr_encabezado.fecha_fp    ,
            pr_encabezado.complemento

      ON EVERY ROW
         CALL f_siefore_consar(pr_detalle_34.siefore ) RETURNING pr_detalle_34.siefore
         PRINT
            pr_detalle_34.tipo_registro                                  ,  --01
            pr_detalle_34.nss                                            ,  --02
            pr_detalle_34.paterno                                        ,  --03
            pr_detalle_34.materno                                        ,  --04
            pr_detalle_34.nombres                                        ,  --05
            pr_detalle_34.curp                                           ,  --06
            pr_detalle_34.secuencia_pen                                  ,  --07
            pr_detalle_34.regimen                                        ,  --08
            pr_detalle_34.tipo_seguro                                    ,  --09
            pr_detalle_34.tipo_pension                                   ,  --10
            pr_detalle_34.tipo_prestacion                                ,  --11
            pr_detalle_34.tipo_retiro                                    ,  --12
            pr_detalle_34.fecha_solicitud          USING "YYYYMMDD"      ,  --13
            pr_detalle_34.fecha_operacion          USING "YYYYMMDD"      ,  --14
            pr_detalle_34.resultado_diagnostico                          ,  --15
            pr_detalle_34.motivo_diagnostico                             ,  --16
            pr_detalle_34.estatus_vivienda         USING "&&"            ,  --17
            pr_detalle_34.fecha_ini_pension        USING "YYYYMMDD"      ,  --18
            pr_detalle_34.fecha_liquidacion        USING "YYYYMMDD"      ,  --19
            pr_detalle_34.siefore                  USING "&&"            ,  --20
            pr_detalle_34.mto_retiro_97      * 100 USING "&&&&&&&&&&&&&" ,  --21
            pr_detalle_34.mto_cesantia_vejez * 100 USING "&&&&&&&&&&&&&" ,  --22
            pr_detalle_34.mto_cuota_social   * 100 USING "&&&&&&&&&&&&&" ,  --23
            pr_detalle_34.mto_retiro_92      * 100 USING "&&&&&&&&&&&&&" ,  --24
            pr_detalle_34.mto_vivienda_92    * 100 USING "&&&&&&&&&&&&&" ,  --25
            pr_detalle_34.mto_vivienda_97    * 100 USING "&&&&&&&&&&&&&" ,  --26
            pr_detalle_34.indicador_pension                              ,  --27
            pr_detalle_34.forma_pago                                     ,  --28
            pr_detalle_34.medio_solic                                       --29

END REPORT

------------------------------------------------------------------------------
-- rpt_37 : Reporte que genera el archivo plano de cifras del anexo 137    -
------------------------------------------------------------------------------
REPORT rpt_37(pr_encabezado, pr_detalle_37) -- ok

  DEFINE pr_encabezado RECORD
                         tipo_reg                CHAR(002),
                         identifica              CHAR(002),
                         tipo_ent                CHAR(003),
                         clave_ent               CHAR(003),
                         fecha_ip                CHAR(008),
                         fecha_fp                CHAR(008),
                         complemento             CHAR(272)
                       END RECORD

  DEFINE pr_detalle_37 RECORD
                         tipo_registro           CHAR(02)      ,  --01
                         curp                    CHAR(18)      ,  --02
                         paterno                 CHAR(40)      ,  --03
                         materno                 CHAR(40)      ,  --04
                         nombres                 CHAR(40)      ,  --05
                         secuencia_pen           CHAR(02)      ,  --06
                         regimen                 CHAR(02)      ,  --07
                         tipo_seguro             CHAR(02)      ,  --08
                         tipo_pension            CHAR(02)      ,  --09
                         tipo_prestacion         CHAR(02)      ,  --10

                         tipo_retiro             CHAR(01)      ,  --11
                         fecha_solicitud         DATE          ,  --12
                         fecha_operacion         DATE          ,  --13
                         estatus_vivienda        CHAR(02)      ,  --14
                         fecha_ini_pension       DATE          ,  --15
                         fecha_liquidacion       DATE          ,  --16
                         origen_recursos         CHAR(01)      ,  --17
                         siefore                 CHAR(02)      ,  --18
                         mto_retiro_08           DECIMAL(12,2) ,  --19
                         mto_cesantia_vejez      DECIMAL(12,2) ,  --20

                         mto_sar92               DECIMAL(12,2) ,  --21
                         mto_ahorro_sol          DECIMAL(12,2) ,  --22
                         mto_comp_retiro         DECIMAL(12,2) ,  --23
                         mto_fovissste92         DECIMAL(12,2) ,  --24
                         mto_fovissste08         DECIMAL(12,2) ,  --25
                         forma_pago              CHAR(03)      ,  --26
                         medio_solic             CHAR(03)      ,  --27
                         mto_cuota_social        DECIMAL(12,2)    --28
                       END RECORD

  -----------------------------------------------------------------------------

  OUTPUT
      PAGE LENGTH   10
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

  FORMAT
      FIRST PAGE HEADER
         PRINT
            pr_encabezado.tipo_reg    ,
            pr_encabezado.identifica  ,
            pr_encabezado.tipo_ent    ,
            pr_encabezado.clave_ent   ,
            pr_encabezado.fecha_ip    ,
            pr_encabezado.fecha_fp    ,
            pr_encabezado.complemento

      -- DETALLE
      ON EVERY ROW
         CALL f_siefore_consar(pr_detalle_37.siefore) RETURNING pr_detalle_37.siefore
         PRINT
            pr_detalle_37.tipo_registro                                  ,  --01
            pr_detalle_37.curp                                           ,  --02
            pr_detalle_37.paterno                                        ,  --03
            pr_detalle_37.materno                                        ,  --04
            pr_detalle_37.nombres                                        ,  --05
            pr_detalle_37.secuencia_pen                                  ,  --06
            pr_detalle_37.regimen                                        ,  --07
            pr_detalle_37.tipo_seguro                                    ,  --08
            pr_detalle_37.tipo_pension                                   ,  --09
            pr_detalle_37.tipo_prestacion                                ,  --10
            pr_detalle_37.tipo_retiro                                    ,  --11
            pr_detalle_37.fecha_solicitud          USING "YYYYMMDD"      ,  --12
            pr_detalle_37.fecha_operacion          USING "YYYYMMDD"      ,  --13
            pr_detalle_37.estatus_vivienda         USING "&&"            ,  --14
            pr_detalle_37.fecha_ini_pension        USING "YYYYMMDD"      ,  --15
            pr_detalle_37.fecha_liquidacion        USING "YYYYMMDD"      ,  --16
            pr_detalle_37.origen_recursos                                ,  --17
            pr_detalle_37.siefore                  USING "&&"            ,  --18
            pr_detalle_37.mto_retiro_08      * 100 USING "&&&&&&&&&&&&&" ,  --19
            pr_detalle_37.mto_cesantia_vejez * 100 USING "&&&&&&&&&&&&&" ,  --20

            pr_detalle_37.mto_sar92          * 100 USING "&&&&&&&&&&&&&" ,  --21
            pr_detalle_37.mto_ahorro_sol     * 100 USING "&&&&&&&&&&&&&" ,  --22
            pr_detalle_37.mto_comp_retiro    * 100 USING "&&&&&&&&&&&&&" ,  --23
            pr_detalle_37.mto_fovissste92    * 100 USING "&&&&&&&&&&&&&" ,  --24
            pr_detalle_37.mto_fovissste08    * 100 USING "&&&&&&&&&&&&&" ,  --25
            pr_detalle_37.forma_pago                                     ,  --26
            pr_detalle_37.medio_solic                                    ,  --27
            pr_detalle_37.mto_cuota_social   * 100 USING "&&&&&&&&&&&&&"    --28

END REPORT

-------------------------------------------------------------------------------

FUNCTION f_retsoltx_imss(p_nss, p_consecutivo)

  DEFINE p_nss         LIKE dis_cuenta.nss
  DEFINE p_consecutivo LIKE dis_cuenta.consecutivo_lote

  DEFINE lr_retsoltx  RECORD LIKE ret_solicitud_tx.*


  INITIALIZE lr_retsoltx.* TO NULL

  LET lc_cadena = '',
                  '\n SELECT *                                ',
                  '\n   FROM ret_solicitud_tx                 ',
                  '\n  WHERE nss         = "',p_nss,'"        ',
                  '\n    AND consecutivo =  ',p_consecutivo,' '

  PREPARE con_ret FROM lc_cadena
  EXECUTE con_ret INTO lr_retsoltx.*

  {IF lr_retsoltx.nss = "49897203821" THEN 
    DISPLAY "nss: ", lr_retsoltx.nss
    DISPLAY "consecutivo: ", lr_retsoltx.consecutivo
    DISPLAY "diag_registro: ", lr_retsoltx.diag_registro
  END IF }

  IF SQLCA.SQLCODE = NOTFOUND THEN

    INITIALIZE lr_retsoltx.* TO NULL

     LET lr_retsoltx.sec_pension     = ""
     LET lr_retsoltx.regimen         = ""
     LET lr_retsoltx.tipo_seguro     = ""
     LET lr_retsoltx.tipo_pension    = ""
     LET lr_retsoltx.tipo_retiro     = ""
     LET lr_retsoltx.fecha_solicitud = "          "
     LET lr_retsoltx.fecha_ini_pen   = "          "
     LET lr_retsoltx.paterno_sol     = ""
     LET lr_retsoltx.materno_sol     = ""
     LET lr_retsoltx.nombre_sol      = ""
     LET lr_retsoltx.nss             = ""
     LET lr_retsoltx.consecutivo     = ""



  END IF

  RETURN lr_retsoltx.*

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION f_retsoltx_issste(p_curp, p_consecutivo)

  DEFINE p_curp        LIKE dis_cuenta.curp
  DEFINE p_consecutivo LIKE dis_cuenta.consecutivo_lote

  DEFINE lr_retsoltx   RECORD LIKE ret_sol_issste_tx.*


  INITIALIZE lr_retsoltx.* TO NULL

  LET lc_cadena = '',
                  '\n SELECT *                                ',
                  '\n   FROM ret_sol_issste_tx                ',
                  '\n  WHERE curp        = "',p_curp,'"       ',
                  '\n    AND consecutivo =  ',p_consecutivo,' '

  PREPARE con_ret_issste FROM lc_cadena
  EXECUTE con_ret_issste INTO lr_retsoltx.*

  IF SQLCA.SQLCODE = NOTFOUND THEN

    INITIALIZE lr_retsoltx.* TO NULL

     LET lr_retsoltx.sec_pension     = "  "
     LET lr_retsoltx.regimen         = ""
     LET lr_retsoltx.tipo_seguro     = ""
     LET lr_retsoltx.tipo_pension    = ""
     LET lr_retsoltx.tipo_retiro     = ""
     LET lr_retsoltx.fecha_solicitud = "          "
     LET lr_retsoltx.fecha_ini_pen   = "          "
     LET lr_retsoltx.nombre_afore    = ""
     LET lr_retsoltx.paterno_afore   = ""
     LET lr_retsoltx.materno_afore   = ""

  END IF

  RETURN lr_retsoltx.*

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION f_get_usuario()

  -- OBTENER EL LOGIN DEL USUARIO QUE ESTA EJECUTANDO EL PROCESO ACTUAL

  DEFINE lv_usu   VARCHAR(10)

  SELECT MAX(USER)
    INTO lv_usu
    FROM tab_afore_local

  RETURN lv_usu

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION f_datos(p_nss, p_curp)

  DEFINE p_nss         VARCHAR(11)
  DEFINE p_curp        VARCHAR(18)

  DEFINE l_nss         SMALLINT
  DEFINE l_curp        SMALLINT

  DEFINE lc_cuantos    SMALLINT
  DEFINE lc_curp       CHAR(18)
  DEFINE lc_paterno    VARCHAR(40)
  DEFINE lc_materno    VARCHAR(40)
  DEFINE lc_nombres    VARCHAR(40)
  DEFINE ls_encontrado SMALLINT

  ---------------------------------
  LET p_nss  = p_nss  CLIPPED
  LET p_curp = p_curp CLIPPED

  LET l_nss  = LENGTH(p_nss)
  LET l_curp = LENGTH(p_curp)

  LET ls_encontrado = 1
  LET lc_paterno    = ""
  LET lc_materno    = ""
  LET lc_nombres    = ""
  LET lc_cuantos    = 0
  ---------------------------------

  CASE
    WHEN l_nss = 11 AND l_curp = 18

          LET lc_cadena = '',
                          '\n SELECT count(*), n_unico, paterno, materno, nombres    ',
                          '\n   FROM afi_mae_afiliado             ',
                          '\n  WHERE n_seguro = "',p_nss,'"       ',
                          '\n    AND n_unico  = "',p_curp,'"      ',
                          '\n  GROUP BY 2,3,4,5                   '
          PREPARE con_afi_1 FROM lc_cadena
          EXECUTE con_afi_1 INTO lc_cuantos,
                                 lc_curp,
                                 lc_paterno,   --03
                                 lc_materno,   --04
                                 lc_nombres    --05

          IF SQLCA.SQLCODE = NOTFOUND THEN

             LET lc_cadena = '',
                             '\n SELECT count(*),n_unico, paterno, materno, nombres    ',
                             '\n   FROM afi_mae_afiliado             ',
                             '\n  WHERE n_seguro = "',p_nss,'"       ',
                             '\n  GROUP BY 2,3,4,5                 '

             PREPARE con_afi_12 FROM lc_cadena
             EXECUTE con_afi_12 INTO lc_cuantos,
                                     lc_curp,
                                     lc_paterno,   --03
                                     lc_materno,   --04
                                     lc_nombres    --05

             IF SQLCA.SQLCODE = NOTFOUND THEN
               LET lc_paterno    = ""
               LET lc_materno    = ""
               LET lc_nombres    = ""
               LET ls_encontrado = 0
            END IF

          END IF

    WHEN l_curp = 18 AND l_nss <> 11

          LET lc_cadena = '',
                          '\n SELECT FIRST 1 n_unico, paterno, materno, nombres  ',
                          '\n   FROM afi_mae_afiliado                   ',
                          '\n  WHERE n_unico  = "',p_curp,'"            '

          PREPARE con_afi_2 FROM lc_cadena
          EXECUTE con_afi_2 INTO lc_curp,
                                 lc_paterno,   --03
                                 lc_materno,   --04
                                 lc_nombres    --05

          IF SQLCA.SQLCODE = NOTFOUND THEN
            LET lc_paterno    = ""
            LET lc_materno    = ""
            LET lc_nombres    = ""
            LET ls_encontrado = 0
          END IF

    WHEN l_nss = 11 AND l_curp <> 18

          LET lc_cadena = '',
                          '\n SELECT count(*), n_unico, paterno, materno, nombres    ',
                          '\n   FROM afi_mae_afiliado             ',
                          '\n  WHERE n_seguro = "',p_nss,'"       ',
                          '\n  GROUP BY 2,3,4,5                    '

          PREPARE con_afi_3 FROM lc_cadena
          EXECUTE con_afi_3 INTO lc_cuantos,
                                 lc_curp,      --02
                                 lc_paterno,   --03
                                 lc_materno,   --04
                                 lc_nombres    --05

          IF SQLCA.SQLCODE = NOTFOUND THEN
             LET lc_curp       = ""
            LET lc_paterno    = ""
            LET lc_materno    = ""
            LET lc_nombres    = ""
            LET ls_encontrado = 0
          END IF

  END CASE

   IF lc_cuantos > 1 THEN
          DISPLAY "                                             " AT 18,1 ATTRIBUTE(REVERSE)
          DISPLAY "El nss", p_nss, "cuenta con mas de una CURP"   AT 18,1 ATTRIBUTE(REVERSE)
          CALL ERRORLOG("El nss:"|| p_nss ||"cuenta con mas de una CURP ")

          LET lc_curp       = ""
          LET lc_paterno    = ""
          LET lc_materno    = ""
          LET lc_nombres    = ""
          LET ls_encontrado = 0
          LET lc_cuantos    = 0
        END IF

  RETURN lc_curp, lc_paterno, lc_materno, lc_nombres, ls_encontrado

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION f_fecha_operacion(p_folio, p_tipo_ret, p_anexo)

  DEFINE p_folio       INTEGER
  DEFINE p_tipo_ret    CHAR(1)
  DEFINE p_anexo       SMALLINT

  DEFINE ld_fecha      DATE

  INITIALIZE ld_fecha TO NULL

  -- DISPLAY "p_folio    ",p_folio
  -- DISPLAY "p_tipo_ret ",p_tipo_ret

  IF p_folio IS NULL OR p_tipo_ret IS NULL THEN

    -- NO SE EJECUTA EL QUERY
     LET ld_fecha = ""

  ELSE

    CASE

      WHEN p_anexo = 134


         LET lc_cadena = '',
                         '\n SELECT MAX(fecha_envio)               ',
                         '\n   FROM ret_ctr_envio_lote             ',
                         '\n  WHERE folio       =  ',p_folio,'     ',
                         '\n    AND tipo_retiro = "',p_tipo_ret,'" '

         PREPARE fecope FROM lc_cadena
         EXECUTE fecope INTO ld_fecha

         IF SQLCA.SQLCODE = NOTFOUND OR ld_fecha IS NULL THEN
             LET ld_fecha = ""
         END IF

      WHEN p_anexo = 137

            LET lc_cadena = '',
                            '\n SELECT MAX(fecha_envio)              ',
                            '\n   FROM ret_ctr_envio                 ',
                            '\n  WHERE folio          = ',p_folio,'  ',
                            '\n    AND tipo_operacion = "DISP_I"     '

            PREPARE fecope2 FROM lc_cadena
            EXECUTE fecope2 INTO ld_fecha

            IF SQLCA.SQLCODE = NOTFOUND OR ld_fecha IS NULL THEN
               LET ld_fecha = ""
               --- Se busca por si es PMG ISSSTE
               LET lc_cadena = '',                                       
                           '\n SELECT fecha_envio     ',                                  
                           '\n FROM pen_envio         ',                                  
                           '\n WHERE folio_lote in (SELECT unique folio_op72 ',           
                           '\n                      FROM    pen_ctr_pago_det_iss ',           
                           '\n                      WHERE   folio_liquida = ', p_folio,')'                                                
               PREPARE fecope1 FROM lc_cadena
               EXECUTE fecope1 INTO ld_fecha                                      
               IF SQLCA.SQLCODE = NOTFOUND OR ld_fecha IS NULL THEN
                  LET ld_fecha = ""
               END IF 
            END IF
      OTHERWISE
         LET lc_cadena = '',
                                '\n SELECT fecha_envio     ',
                                '\n FROM pen_envio         ',
                                '\n WHERE folio_lote in (SELECT unique folio_op78 ',    
                                '\n                      FROM    pen_ctr_pago_det ',
                                '\n                      WHERE   folio_liquida = ', p_folio,')'
                PREPARE fecop_pmg FROM lc_cadena
                EXECUTE fecop_pmg INTO ld_fecha

                IF SQLCA.SQLCODE = NOTFOUND OR ld_fecha IS NULL THEN
                    LET ld_fecha = "01/01/0001"
                END IF
    END CASE

  END IF

  RETURN ld_fecha

END FUNCTION

-------------------------------------------------------------------------------
FUNCTION f_siefore_consar(ls_siefore)

DEFINE ls_siefore       SMALLINT
DEFINE ls_siefore_cns   SMALLINT

SELECT NVL(cod_siefore_cns,0)
INTO  ls_siefore_cns
FROM   tab_siefore_local
WHERE  codigo_siefore >= 14
AND    codigo_siefore = ls_siefore


IF ls_siefore_cns IS NULL THEN
   LET ls_siefore_cns = 0
END IF

RETURN ls_siefore_cns
END FUNCTION


-------------------------------------------------------------------------------
FUNCTION f_obtiene_fip(lc_nss, lc_sec_pension)
   DEFINE lc_nss           CHAR(11)
         ,lc_sec_pension   CHAR(02)
         ,ld_fecha         DATE

   SELECT   MAX(UNIQUE fecha_ini_pen)
   INTO     ld_fecha
   FROM    ret_det_datamart
   WHERE   nss = lc_nss
   AND     sec_pension = lc_sec_pension
   
   IF SQLCA.SQLCODE = 100 THEN 
      LET ld_fecha = "01/01/0001"
   END IF 
   
   RETURN ld_fecha

END FUNCTION