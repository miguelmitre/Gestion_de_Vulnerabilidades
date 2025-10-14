DATABASE safre_af
################################################################################
GLOBALS
   DEFINE gi_folio     INTEGER
   DEFINE gc_usuario   CHAR(08)

   DEFINE gs_afore     SMALLINT
   DEFINE hoy          DATE

   DEFINE gr_seg_modulo RECORD
        modulo_cod     CHAR(04),
       ruta_envio     CHAR(40)
   END RECORD

   DEFINE gc_log       CHAR(50)
   DEFINE gi_registros INTEGER

   DEFINE gr_reporte RECORD
       tipo_registro           SMALLINT     ,
      consecutivo_envio       DECIMAL(11,0),
      dia_corte               SMALLINT     ,
      mes_corte               SMALLINT     ,
      nombre_completo         CHAR(120)    ,
      calle_num               CHAR(62)     ,
      colonia                 CHAR(60)     ,
      cp                      CHAR(05)     ,
      centro_reparto          CHAR(05)     ,
      entidad_federal         CHAR(40)     ,
      munic_delega            CHAR(40)     ,
      nss                     CHAR(11)     ,
      rfc                     CHAR(13)     ,
      curp                    CHAR(18)     ,
      saldo_ini_ret           DECIMAL(22,6),
      saldo_ini_viv           DECIMAL(22,6),
      saldo_trans_ret         DECIMAL(22,6),
      saldo_trans_viv         DECIMAL(22,6),
      saldo_rem_ret           DECIMAL(22,6),
      saldo_rem_viv           DECIMAL(22,6),
      ley_pen                 CHAR(06)     ,
      regimen                 CHAR(02)     ,
      seguro                  CHAR(02)     ,
      pension                 CHAR(02)     ,
      pen_entidad             CHAR(50)     ,
      pen_monto_trans         DECIMAL(22,6),
      pen_fecha_trans         DATE         ,
      afo_entidad             CHAR(50)     ,
      afo_medio_pago          CHAR(40)     ,
      afo_rec_entregado       DECIMAL(22,6),
      afo_fecha_entrega       DATE         ,
      afo_isr                 DECIMAL(22,6),
      infonavit_entidad       CHAR(50)     ,
      infonavit_numcta        CHAR(20)     ,
      infonavit_entregado     DECIMAL(22,6),
      infonavit_fecha_entrega DATE         ,
      infonavit_isr           DECIMAL(22,6),
      fecha_emision           DATE         ,
      fecha_ini_pen           DATE         ,
      monto_pen               DECIMAL(22,6),
      saldo_fin               DECIMAL(22,6)
   END RECORD

   DEFINE gc_archivo        CHAR(500)
   DEFINE gs_tipo_informe   SMALLINT
   DEFINE gs_pmg            SMALLINT --CPL-2167

END GLOBALS
MAIN
   LET gi_folio       = ARG_VAL(1)

   DISPLAY "INICIA PROCESO DE ESTADO DE CUENTA CON FOLIO: ", gi_folio
   CALL STARTLOG(FGL_GETENV("USER")||".RETL843.log")

   LET hoy          = TODAY
   LET gi_registros = 0

   LET gr_seg_modulo.modulo_cod = "cta"

   SELECT USER
   INTO   gc_usuario
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   SELECT codigo_afore
   INTO   gs_afore
   FROM   safre_af:tab_afore_local

   SELECT ruta_envio
   INTO   gr_seg_modulo.ruta_envio
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   #Cambiar estado
   UPDATE cta_ctr_proceso
   SET    estado = 2
   WHERE  folio  = gi_folio
   AND    estado = 1

   SELECT tipo_informe
   INTO   gs_tipo_informe
   FROM   cta_ctr_proceso
   WHERE  folio  = gi_folio
   AND    estado = 2
   GROUP BY 1

   CALL init()
   CALL archivo()
   LET gc_log = "chmod 777 *RETL843.log"
   RUN gc_log

   DISPLAY "TERMINA PROCESO DE ESTADO DE CUENTA:",gi_registros
END MAIN

#*******************************************************************************
#Función : init()
#Objetivo: Prepares de datos generales, domicilio y accesos a datamart
#Parámetros de entrada: Ninguno
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION init()
   DEFINE lc_sql CHAR(2000)

   LET lc_sql = " SELECT n_unico                      , ",
                "        n_rfc                        , ",
                "        NVL(TRIM(nombres),' ')||' '||  ",
                "        NVL(TRIM(paterno),' ')||' '||  ",
                "        NVL(TRIM(materno),' ')         ",
                " FROM   afi_mae_afiliado               ",
                " WHERE  n_seguro = ?                   "
   PREPARE get_afil FROM lc_sql

   LET lc_sql = " SELECT FIRST 1                               ",
                "        NVL(TRIM(dom.calle),' ')  || ' ' ||   ",
                "        NVL(TRIM(dom.numero),' ') || ' ' ||   ",
                "        NVL(TRIM(dom.depto),' ')           ,  ", --calle_num
                "        dom.colonia                        ,  ", --colonia
                "        dom.codpos                         ,  ", --cp
                "        NVL(rep.centro_reparto,'00000')    ,  ", --centro_reparto
                "        edo.estad_desc                     ,  ", --entidad_federal
                "        del.deleg_desc                        ", --munic_delega
                " FROM   safre_af:afi_mae_afiliado       a,    ",
                "        safre_af:afi_domicilio        dom,    ",
                "        OUTER safre_af:tab_estado     edo,    ",
                "        OUTER safre_af:tab_delegacion del,    ",
                "        OUTER safre_af:tab_reparto    rep     ",
                " WHERE  dom.nss            = a.n_seguro       ",
                " AND    dom.nss            = ?                ",
                " AND    dom.n_folio        = a.n_folio        ",
                " AND    dom.tipo_solicitud = a.tipo_solicitud ",
                " AND    dom.marca_envio    = 'X'              ",
                " AND    edo.estad_cod      = dom.estado       ",
                " AND    del.estad_cod      = dom.estado       ",
                " AND    del.deleg_cod      = dom.delega       ",
                " AND    rep.codigo_postal  = dom.codpos       "
               #" AND    dom.dom_cod        = 1                " --personal #CPL-2093
   PREPARE get_domicilio FROM lc_sql

   LET lc_sql = " SELECT FIRST 1                                          ",
                "        DECODE (a.regimen, 'RO' , 'REGIMEN ORDINARIO'  , ",
                "                           'DT' , 'DECIMO TRANSITORIO'), ",
                "        a.regimen               ,                        ",
                "        a.tipo_seguro           ,                        ",
                "        a.tipo_pension          ,                        ",
                "        a.fecha_ini_pen         ,                        ",
                "        a.fecha_resolucion                               ",
                " FROM   ret_datamart_issste   a ,                        ",
                "        ret_sol_issste_tx     b                          ",
                " WHERE  a.nss              = b.nss                       ",
                " AND    a.sec_pension      = b.sec_pension               ",
                " AND    b.nss              = ?                           ",
                " AND    b.consecutivo      = ?                           "
   PREPARE get_pension_disp FROM lc_sql

   LET lc_sql = " SELECT FIRST 1                                          ",
                "        DECODE (a.regimen, 'RO' , 'REGIMEN ORDINARIO'  , ",
                "                           'DT' , 'DECIMO TRANSITORIO'), ",
                "        a.regimen               ,                        ",
                "        a.tipo_seguro           ,                        ",
                "        a.tipo_pension          ,                        ",
                "        a.fecha_ini_pen         ,                        ",
                "        a.fecha_resolucion                               ",
                " FROM   ret_datamart_issste   a ,                        ",
                "        pen_solicitud_iss     b                          ",
                " WHERE  a.nss              = b.nss                       ",
                " AND    a.sec_pension      = b.sec_pension               ",
                " AND    b.nss              = ?                           ",
                " AND    b.consecutivo      = ?                           "
   PREPARE get_pension_pmg FROM lc_sql
   
   
   LET lc_sql = "EXECUTE FUNCTION safre_tmp:fn_valua_bono_issste(?, ?, ?)"
   PREPARE get_bono_valuado FROM lc_sql
END FUNCTION

#*******************************************************************************
#Función : archivo()
#Objetivo: Flujo principal del proceso, ejecuta las funciones necesarias para
#          recopilar la información de cada Nss
#Parámetros de entrada: Ninguno
#Valores de retorno   : Ninguno
#Variables globales   : gc_archivo Nombre y ruta del archivo
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION archivo()
   DEFINE ld_fecha_fin      DATE    ,
          lc_hora_fin       CHAR(8) ,
          lc_comando        CHAR(500)

   DEFINE ld_fecha_ini_reporte ,
          ld_fecha_fin_reporte DATE

   DEFINE li_folio       INTEGER
   DEFINE ld_consec_disp DECIMAL(11,0)

   LET gc_archivo = gr_seg_modulo.ruta_envio CLIPPED,"/",
                    gc_usuario               CLIPPED,".",
                    "EC_FINAL."                         ,
                    gi_folio                 USING "<<<<<<<<<<<<<<<<<<<<"
   DISPLAY "gc_archivo: ",gc_archivo CLIPPED

   START REPORT rpt_edc_final TO gc_archivo

   #Encabezado
   OUTPUT TO REPORT rpt_edc_final(1)

   CALL Ingresa_etapa(gi_folio,2,0,"Inicia calculo de estado de cuenta")

   DECLARE cur_detalle CURSOR FOR
   SELECT nss         ,
          fecha_inicio,
          fecha_fin
   FROM   cta_ctr_proceso
   WHERE  folio = gi_folio
   AND    tipo_informe = 19 --- Estado de cuenta ISSSTE

   FOREACH cur_detalle INTO gr_reporte.nss      ,
                             ld_fecha_ini_reporte,
                             ld_fecha_fin_reporte
      #Inicializar
      CALL inicializa()

       #curp, rfc, nombre
      EXECUTE get_afil USING gr_reporte.nss
                       INTO  gr_reporte.curp,
                             gr_reporte.rfc ,
                             gr_reporte.nombre_completo

      #domicilio
      EXECUTE get_domicilio USING gr_reporte.nss
                            INTO  gr_reporte.calle_num      ,
                                  gr_reporte.colonia        ,
                                  gr_reporte.cp             ,
                                  gr_reporte.centro_reparto ,
                                  gr_reporte.entidad_federal,
                                  gr_reporte.munic_delega

      IF gs_afore = 568 THEN --Coppel
         #ultimo día del mes de corte
         LET gr_reporte.fecha_emision = ld_fecha_fin_reporte
         LET gr_reporte.dia_corte     = DAY(gr_reporte.fecha_emision)
         LET gr_reporte.mes_corte     = MONTH(gr_reporte.fecha_emision)
      END IF

      CALL saldos_disp(ld_fecha_ini_reporte,ld_fecha_fin_reporte)
                           RETURNING gr_reporte.consecutivo_envio, --folio trans
                                     li_folio                    , --folio trans
                                     ld_consec_disp                --consecutivo disp

      DISPLAY "ld_fecha_ini_reporte: ",ld_fecha_ini_reporte
      DISPLAY "ld_fecha_fin_reporte: ",ld_fecha_fin_reporte
      DISPLAY "gr_reporte.consecutivo_envio: ",gr_reporte.consecutivo_envio
      DISPLAY "li_folio: ",li_folio
      DISPLAY "ld_consec_disp: ",ld_consec_disp
      DISPLAY "gr_reporte.nss: ",gr_reporte.nss

      #detalle de la transferencia
      CALL get_datos_ret(gr_reporte.nss,  --nss
                         ld_consec_disp,  --consecutivo disp
                         li_folio       ) --folio trans

      --SI ES NTI LO MANDA EN BLANCOS
      IF gr_reporte.nss[1] MATCHES "[iI]" THEN
         LET gr_reporte.nss = "           "
      END IF

      #Detalle 02
      OUTPUT TO REPORT rpt_edc_final(2)
      CALL despliega()
   END FOREACH

   #Sumario
   OUTPUT TO REPORT rpt_edc_final(99)

   FINISH REPORT rpt_edc_final

   #Permisos
   LET lc_comando = "chmod 777 ", gc_archivo CLIPPED
   RUN lc_comando

   CALL Actualiza_etapa(gi_folio,2,gi_registros,"Termina calculo de estado de cuenta")
   CALL Actualiza_etapa(gi_folio,1,gi_registros,gc_archivo)

   UPDATE cta_ctr_proceso
   SET    estado = 3
   WHERE  folio  = gi_folio
   AND    estado = 2

END FUNCTION

#*******************************************************************************
#Función : inicializa()
#Objetivo: Inicializa el record del reporte para c/Nss
#Parámetros de entrada: Ninguno
#Valores de retorno   : Ninguno
#Variables globales   : gr_reporte. Record del reporte
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION inicializa()
   LET gr_reporte.tipo_registro           = 2     --
   LET gr_reporte.consecutivo_envio       = NULL  --     --archivo
   LET gr_reporte.dia_corte               = NULL  --1    --archivo
   LET gr_reporte.mes_corte               = NULL  --2    --archivo
   LET gr_reporte.nombre_completo         = NULL  --3    --archivo
   LET gr_reporte.calle_num               = NULL  --3    --archivo
   LET gr_reporte.colonia                 = NULL  --3    --archivo
   LET gr_reporte.cp                      = NULL  --3    --archivo
   LET gr_reporte.centro_reparto          = NULL  --3    --archivo
   LET gr_reporte.entidad_federal         = NULL  --3    --archivo
   LET gr_reporte.munic_delega            = NULL  --3    --archivo
   --LET gr_reporte.nss                     =     --6    --archivo
   LET gr_reporte.rfc                     = NULL  --5    --archivo
   LET gr_reporte.curp                    = NULL  --4    --archivo
   LET gr_reporte.saldo_ini_ret           = 0     --7    --saldos
   LET gr_reporte.saldo_ini_viv           = 0     --8    --saldos
   LET gr_reporte.saldo_trans_ret         = 0     --9    --saldos
   LET gr_reporte.saldo_trans_viv         = 0     --10   --saldos
   LET gr_reporte.saldo_rem_ret           = 0     --11   --saldos
   LET gr_reporte.saldo_rem_viv           = 0     --12   --saldos
   LET gr_reporte.ley_pen                 = NULL  --13   --get_datos_ret
   LET gr_reporte.regimen                 = NULL  --14   --get_datos_ret
   LET gr_reporte.seguro                  = NULL  --15   --get_datos_ret
   LET gr_reporte.pension                 = NULL  --16   --get_datos_ret
   LET gr_reporte.pen_entidad             = NULL  --17   --get_datos_ret
   LET gr_reporte.pen_monto_trans         = 0     --18   --saldos
   LET gr_reporte.pen_fecha_trans         = NULL  --19   --saldos
   LET gr_reporte.afo_entidad             = NULL  --20   --get_datos_ret
   LET gr_reporte.afo_medio_pago          = NULL  --21   --get_datos_ret
   LET gr_reporte.afo_rec_entregado       = 0     --22   --saldos
   LET gr_reporte.afo_fecha_entrega       = NULL  --23   --saldos
   LET gr_reporte.afo_isr                 = 0     --24   --saldos
   LET gr_reporte.infonavit_entidad       = NULL  --25   --get_datos_ret
   LET gr_reporte.infonavit_numcta        = NULL  --26   --get_datos_ret
   LET gr_reporte.infonavit_entregado     = 0     --27   --saldos
   LET gr_reporte.infonavit_fecha_entrega = NULL  --28   --saldos
   LET gr_reporte.infonavit_isr           = 0     --29   --saldos
   LET gr_reporte.fecha_emision           = NULL  --HOY   --30
   LET gr_reporte.fecha_ini_pen           = NULL  --31   --get_datos_ret
   LET gr_reporte.monto_pen               = 0     --32   --get_datos_ret
   LET gr_reporte.saldo_fin               = 0     --33   --saldos
END FUNCTION

#*******************************************************************************
#Función : saldos_disp
#Objetivo: Determina saldo inicial, saldo transferido y entregado, saldos de la
#          transferencia, saldos de la disposición(divididos en retiro y
#          vivienda)
#          Se emplea para nss sin transferencia, busca las disposiciones del mes
#Parámetros de entrada: ld_fecha_ini_reporte
#                       ld_fecha_fin_reporte
#                       => Periodo del reporte
#Valores de retorno   : Ninguno
#Variables globales   :
#gr_reporte.saldo_ini_viv          =>saldo inicial de vivienda
#gr_reporte.saldo_ini_ret          =>saldo inicial de retiro
#gr_reporte.saldo_trans_viv        =>saldo transferido de vivienda
#gr_reporte.saldo_trans_ret        =>saldo transferido de retiro
#gr_reporte.infonavit_entregado    =>saldo vivienda entregado en la disposición
#gr_reporte.afo_rec_entregado      =>saldo retiro entregado en la disposición
#gr_reporte.afo_isr                =>retención ISR en la disposición
#gr_reporte.infonavit_fecha_entrega=>fecha liquidacion disposición
#gr_reporte.afo_fecha_entrega      =>fecha liquidacion disposición
#
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION saldos_disp(ld_fecha_ini_reporte, ld_fecha_fin_reporte)
   DEFINE ld_fecha_ini_reporte ,
          ld_fecha_fin_reporte DATE

   DEFINE ls_subcuenta SMALLINT     ,
          ls_siefore   SMALLINT     ,
          ld_pesos     DECIMAL(22,6),
          ld_acciones  DECIMAL(22,6)

   DEFINE ld_fecha_ini       DATE
   DEFINE ls_tipo_movimiento SMALLINT

   DEFINE lr_consecutivo RECORD
           transferencia  DECIMAL(11,0),
           disposicion    DECIMAL(11,0)
   END RECORD

   DEFINE li_folio               INTEGER
   DEFINE ls_cuantos             SMALLINT
   DEFINE ld_fecha_disp          DATE

   INITIALIZE lr_consecutivo.* TO NULL

   --*******************************************************************************
   #Temporal de dis_cuenta
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta
   WHENEVER ERROR STOP

   SELECT *
   FROM   dis_cuenta
   WHERE  nss = gr_reporte.nss
   INTO TEMP tmp_dis_cuenta

   CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta(tipo_movimiento, fecha_conversion)
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

   LET ls_cuantos = 0

   DECLARE cur_datos_disp CURSOR FOR
   SELECT consecutivo_lote,
          fecha_conversion
   FROM   tmp_dis_cuenta
   WHERE  tipo_movimiento IN (SELECT movimiento
                                FROM tab_ret_issste)
   AND    fecha_conversion BETWEEN ld_fecha_ini_reporte AND ld_fecha_fin_reporte
   GROUP BY 1,2
   ORDER BY 2,1

   FOREACH cur_datos_disp INTO lr_consecutivo.disposicion,
                               ld_fecha_disp

      LET gr_reporte.infonavit_fecha_entrega = ld_fecha_disp
      LET gr_reporte.afo_fecha_entrega       = ld_fecha_disp

      LET ls_cuantos = ls_cuantos + 1

      --************************************************************************
      #PRIMERA DISPOSICION
      --************************************************************************
      IF ls_cuantos = 1 THEN
         LET ld_fecha_ini = ld_fecha_disp - 1 UNITS DAY
         CALL saldo_dia(gr_reporte.nss, ld_fecha_ini)

         DECLARE cur_saldo_ini_disp CURSOR FOR
         SELECT subcuenta        ,
                siefore          ,
                monto_en_acciones,
                monto_en_pesos
         FROM   tmp_saldo_dia
         ORDER BY 1,2

         FOREACH cur_saldo_ini_disp INTO ls_subcuenta,
                                         ls_siefore  ,
                                         ld_acciones ,
                                         ld_pesos
            IF ls_subcuenta = 4  OR
               ls_subcuenta = 8  OR
               ls_subcuenta = 14 OR
               ls_subcuenta = 35 THEN
               LET gr_reporte.saldo_ini_viv = gr_reporte.saldo_ini_viv + ld_pesos
            ELSE
               LET gr_reporte.saldo_ini_ret = gr_reporte.saldo_ini_ret + ld_pesos
            END IF
         END FOREACH
      END IF

      --************************************************************************
      #Saldo de la DISPOSICION actual
      #Hay ISR
      DECLARE cur_disposicion_disp CURSOR FOR
      SELECT subcuenta              ,
             tipo_movimiento        ,
             (monto_en_pesos) * (-1)
      FROM   tmp_dis_cuenta
      WHERE  nss              = gr_reporte.nss
      AND    consecutivo_lote = lr_consecutivo.disposicion

      FOREACH cur_disposicion_disp INTO ls_subcuenta      ,
                                        ls_tipo_movimiento,
                                        ld_pesos

         #Se Acumula saldos transferidos y entregados
         #Se divide en vivienda y el resto
         IF ls_subcuenta = 4  OR
            ls_subcuenta = 8  OR
            ls_subcuenta = 14 OR
            ls_subcuenta = 35 THEN

            #No hay ISR en vivienda
            LET gr_reporte.saldo_trans_viv     = gr_reporte.saldo_trans_viv     + ld_pesos
            LET gr_reporte.infonavit_entregado = gr_reporte.infonavit_entregado + ld_pesos
         ELSE
             LET gr_reporte.saldo_trans_ret   = gr_reporte.saldo_trans_ret   + ld_pesos

             IF ls_tipo_movimiento <> 10 THEN
                LET gr_reporte.afo_rec_entregado = gr_reporte.afo_rec_entregado + ld_pesos
             ELSE
                 LET gr_reporte.afo_isr           = gr_reporte.afo_isr           + ld_pesos
             END IF
         END IF
      END FOREACH
   END FOREACH

   #redondear
   CALL redondea(gr_reporte.saldo_ini_ret      ) RETURNING gr_reporte.saldo_ini_ret
   CALL redondea(gr_reporte.saldo_ini_viv      ) RETURNING gr_reporte.saldo_ini_viv
   CALL redondea(gr_reporte.saldo_trans_ret    ) RETURNING gr_reporte.saldo_trans_ret
   CALL redondea(gr_reporte.saldo_trans_viv    ) RETURNING gr_reporte.saldo_trans_viv
   CALL redondea(gr_reporte.saldo_rem_ret      ) RETURNING gr_reporte.saldo_rem_ret
   CALL redondea(gr_reporte.saldo_rem_viv      ) RETURNING gr_reporte.saldo_rem_viv
   CALL redondea(gr_reporte.pen_monto_trans    ) RETURNING gr_reporte.pen_monto_trans
   CALL redondea(gr_reporte.afo_rec_entregado  ) RETURNING gr_reporte.afo_rec_entregado
   CALL redondea(gr_reporte.afo_isr            ) RETURNING gr_reporte.afo_isr
   CALL redondea(gr_reporte.infonavit_entregado) RETURNING gr_reporte.infonavit_entregado
   CALL redondea(gr_reporte.infonavit_isr      ) RETURNING gr_reporte.infonavit_isr
   CALL redondea(gr_reporte.saldo_fin          ) RETURNING gr_reporte.saldo_fin

   --*******************************************************************************
   #Saldo final
   IF gs_afore = 568 THEN --Coppel
      CALL saldo_dia(gr_reporte.nss, ld_fecha_fin_reporte)
      CALL saldo_final() RETURNING gr_reporte.saldo_rem_ret,
                                   gr_reporte.saldo_rem_viv
   ELSE
      LET gr_reporte.saldo_rem_ret = gr_reporte.saldo_ini_ret - gr_reporte.saldo_trans_ret
      LET gr_reporte.saldo_rem_viv = gr_reporte.saldo_ini_viv - gr_reporte.saldo_trans_viv
   END IF

   IF gr_reporte.saldo_rem_ret < 0 THEN
      LET gr_reporte.saldo_rem_ret = 0
   END IF

   IF gr_reporte.saldo_rem_viv < 0 THEN
      LET gr_reporte.saldo_rem_viv = 0
   END IF

   LET gr_reporte.saldo_fin     = gr_reporte.saldo_rem_ret + gr_reporte.saldo_rem_viv

   IF gr_reporte.saldo_fin < 0 THEN
      LET gr_reporte.saldo_fin = 0
   END IF

   RETURN lr_consecutivo.disposicion,
          li_folio                  ,
          lr_consecutivo.disposicion

END FUNCTION

#*******************************************************************************
#Función : saldo_dia
#Objetivo: Valua el saldo al día del nss
#Parámetros de entrada: lc_nss         => Nss Trabajador
#                       ld_fecha_saldo => Fecha a la que se obtiene el saldo
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION saldo_dia(lc_nss, ld_fecha_saldo)
   DEFINE lc_nss           CHAR(11),
          ld_fecha_saldo   DATE

   DEFINE ls_num_siefore_g SMALLINT      ,
          ls_num_siefore_t SMALLINT      ,
          ls_num_siefore_v SMALLINT      ,
          ld_precio_viv    DECIMAL(19,14),
          ld_fecha_viv     DATE

   DEFINE ld_acciones_nominal,
          ld_acciones_real   ,
          ld_pesos_real       DECIMAL(22,6)

   DEFINE lc_paso CHAR(100)

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_saldo
      DROP TABLE tmp_saldo_dia
   WHENEVER ERROR STOP

   LET ld_fecha_viv = MDY(MONTH(ld_fecha_saldo),1,YEAR(ld_fecha_saldo))

   #Verificar que existan precios
   SELECT "X"
   FROM   glo_valor_accion
   WHERE  codigo_siefore  = 0
   AND    fecha_valuacion = ld_fecha_saldo

   IF SQLCA.SQLCODE = NOTFOUND THEN
      INSERT INTO glo_valor_accion (codigo_siefore,
                                    precio_del_dia,
                                    fecha_valuacion)
      VALUES (0,0,ld_fecha_saldo)
   END IF

   SELECT COUNT(*)
   INTO   ls_num_siefore_t
   FROM   tab_siefore_local

   SELECT COUNT(DISTINCT codigo_siefore)
   INTO   ls_num_siefore_g
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = ld_fecha_saldo
   AND    codigo_siefore  NOT IN (11)

   SELECT COUNT(DISTINCT codigo_siefore)
   INTO   ls_num_siefore_v
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = ld_fecha_viv
   AND    codigo_siefore  = 11

   LET ls_num_siefore_g = ls_num_siefore_g + ls_num_siefore_v

   IF ls_num_siefore_t <>  ls_num_siefore_g THEN
      DISPLAY "Verifique siefores y precios de accion: ",ld_fecha_saldo,
              " Vivienda: ",ld_fecha_viv
   END IF

   SELECT subcuenta                               ,
          siefore                                 ,
          SUM(monto_en_acciones) monto_en_acciones,
          SUM(monto_en_pesos)    monto_en_pesos
   FROM   tmp_dis_cuenta
   WHERE  fecha_conversion <= ld_fecha_saldo
   GROUP BY 1,2
   ORDER BY 1,2
   INTO TEMP tmp_saldo

   sql
   SELECT subcuenta        ,
          siefore          ,
          monto_en_acciones,
          CASE WHEN siefore = 0 THEN
                   monto_en_pesos
               ELSE
                   monto_en_acciones * precio_del_dia
           END monto_en_pesos
   FROM  tmp_saldo       ,
         glo_valor_accion
   WHERE subcuenta NOT IN (4,8,14,35)
   AND   siefore         = codigo_siefore
   AND   fecha_valuacion = $ld_fecha_saldo
   UNION ALL
   SELECT subcuenta                         ,
          siefore                           ,
          monto_en_acciones                 ,
          monto_en_acciones * precio_del_dia monto_en_pesos
   FROM   tmp_saldo       ,
          glo_valor_accion
   WHERE  subcuenta IN (4,8,14,35)
   AND    siefore         = codigo_siefore
   AND    fecha_valuacion = $ld_fecha_viv
   INTO TEMP tmp_saldo_dia
   END sql

   SELECT monto_en_acciones
   INTO   ld_acciones_nominal
   FROM   tmp_saldo_dia
   WHERE  subcuenta = 36

   IF ld_acciones_nominal IS NULL THEN
       LET ld_acciones_nominal = 0
   END IF

   #Valuar bono de pension
   IF ld_acciones_nominal > 0 THEN
      EXECUTE get_bono_valuado USING gr_reporte.nss     ,
                                      ld_acciones_nominal,
                                      gr_reporte.pen_fecha_trans
                                INTO  ld_acciones_real,
                                     ld_pesos_real

      UPDATE tmp_saldo_dia
      SET    monto_en_pesos = ld_pesos_real
      WHERE  subcuenta = 36
   END IF

END FUNCTION

#*******************************************************************************
#Función : saldo_final
#Objetivo: Determina saldo final del trabajador dividido en retiro y vivienda
#Parámetros de entrada: Ninguno
#Valores de retorno   : ld_saldo_fin_ret => saldo final retiro
#                       ld_saldo_fin_viv => saldo final vivienda
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION saldo_final()
   DEFINE ls_subcuenta SMALLINT     ,
          ls_siefore   SMALLINT     ,
          ld_pesos     DECIMAL(22,6),
          ld_acciones  DECIMAL(22,6)

   DEFINE ld_saldo_fin_ret,
          ld_saldo_fin_viv DECIMAL(22,6)

   DECLARE cur_saldo_fin CURSOR FOR
   SELECT subcuenta        ,
          siefore          ,
          monto_en_acciones,
          monto_en_pesos
   FROM   tmp_saldo_dia
   ORDER BY 1,2

   LET ld_saldo_fin_ret = 0
   LET ld_saldo_fin_viv = 0

   FOREACH cur_saldo_fin INTO ls_subcuenta,
                              ls_siefore  ,
                              ld_acciones ,
                              ld_pesos
         IF ls_subcuenta = 4  OR
            ls_subcuenta = 8  OR
            ls_subcuenta = 14 OR
            ls_subcuenta = 35 THEN
            LET ld_saldo_fin_viv = ld_saldo_fin_viv + ld_pesos
         ELSE
            LET ld_saldo_fin_ret = ld_saldo_fin_ret + ld_pesos
         END IF
   END FOREACH

   #redondear
   LET ld_saldo_fin_ret = redondea(ld_saldo_fin_ret)
   LET ld_saldo_fin_viv = redondea(ld_saldo_fin_viv)

   RETURN ld_saldo_fin_ret,
          ld_saldo_fin_viv

END FUNCTION

#*******************************************************************************
#Función : get_datos_ret
#Objetivo: Obtiene los detalles datamart de la transferencia y disposicion
#Parámetros de entrada: lc_nss              => Nss trabajador
#                       ld_consecutivo_disp => Consecutivo disposición
#                       li_folio_trans      => Folio liquidación de transferencia
#Valores de retorno   : Ninguno
#Variables globales   :
#gr_reporte.ley_pen          => Ley pension
#gr_reporte.regimen          => Regimen
#gr_reporte.seguro           => Tipo seguro
#gr_reporte.pension          => Tipo pensión
#gr_reporte.pen_entidad      => Entidad a que se transfiere saldos transferencia
#gr_reporte.fecha_ini_pen    => Fecha inicio de pensión
#gr_reporte.fecha_emision    => Fecha resolución
#gr_reporte.monto_pen        => Monto constitutivo
#gr_reporte.infonavit_numcta => Número cuenta infonavit
#gr_reporte.afo_entidad      => Entidad a que se transfiere saldos retiro
#                               disposición
#gr_reporte.afo_medio_pago   => Medio pago de la disposición
#gr_reporte.infonavit_entidad=> Entidad a que se transfiere saldos vivienda
#                               disposición
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION get_datos_ret(lc_nss, ld_consecutivo_disp, li_folio_trans)
   DEFINE lc_nss              CHAR(11)     ,
          ld_consecutivo_disp DECIMAL(11,0),
          li_folio_trans      INTEGER

   DEFINE lc_num_cuenta,
          lc_clabe       CHAR(20)
   DEFINE lc_cve_destino CHAR(08)

   EXECUTE get_pension_disp USING lc_nss,
                                  ld_consecutivo_disp
                            INTO  gr_reporte.ley_pen      ,
                                  gr_reporte.regimen      ,
                                  gr_reporte.seguro       ,
                                  gr_reporte.pension      ,
                                  gr_reporte.pen_entidad  ,
                                  lc_cve_destino          ,
                                  gr_reporte.fecha_ini_pen,
                                  gr_reporte.fecha_emision,
                                  gr_reporte.monto_pen

   -- Si no se obtuvieron datos con la consulta anterior se busca en los de PMG
   IF gr_reporte.regimen IS NULL THEN
       EXECUTE get_pension_pmg USING lc_nss,
                                     ld_consecutivo_disp
                               INTO  gr_reporte.ley_pen      ,
                                     gr_reporte.regimen      ,
                                     gr_reporte.seguro       ,
                                     gr_reporte.pension      ,
                                     gr_reporte.fecha_ini_pen,
                                     gr_reporte.fecha_emision
       LET gr_reporte.pen_entidad   = " "
       LET gr_reporte.monto_pen     = 0
       LET lc_cve_destino           = "        "
   END IF

   IF ld_consecutivo_disp IS NOT NULL THEN
      #Medio de pago y cuenta bancaria
      CASE gs_afore
         WHEN 564 --MLM
            sql
            SELECT trim(a.num_cuenta),
                   trim(a.clabe)     ,
                   b.descripcion
            INTO   $lc_num_cuenta            ,
                   $lc_clabe                 ,
                   $gr_reporte.afo_medio_pago
            FROM   ret_beneficiario a,
                   OUTER tab_pago   b
            WHERE  a.nss         = $lc_nss
            AND    a.consecutivo = $ld_consecutivo_disp
            AND    a.tipo_pago   = b.tipo_pago
            END sql

            IF lc_clabe IS NOT NULL THEN
               LET gr_reporte.infonavit_numcta = lc_clabe
            ELSE
               LET gr_reporte.infonavit_numcta = lc_num_cuenta
            END IF

            LET gr_reporte.fecha_emision = TODAY

         WHEN 568 --Coppel
            sql
             SELECT trim(a.num_cuenta)     ,
                   trim(b.descripcion[9,40]) tipo_pago
            INTO   $gr_reporte.infonavit_numcta,
                   $gr_reporte.afo_entidad
            FROM   ret_beneficiario a,
                   OUTER tab_pago   b
            WHERE  a.nss         = $lc_nss
            AND    a.consecutivo = $ld_consecutivo_disp
            AND    a.tipo_pago   = b.tipo_pago
            END sql

            IF gr_reporte.afo_entidad = 'BANCOMER' THEN
               LET gr_reporte.afo_entidad = 'OTROS BANCOS'
            END IF

            CASE
               WHEN gr_reporte.afo_entidad = 'BANCOPPEL'    OR
                    gr_reporte.afo_entidad = 'OTROS BANCOS'

                  LET gr_reporte.afo_medio_pago = 'TRANSFERENCIA'

               WHEN gr_reporte.afo_entidad = 'CAJAS COPPEL'

                  LET gr_reporte.afo_medio_pago = 'VENTANILLA'
            END CASE

            LET gr_reporte.infonavit_entidad = 'BANCO'

         OTHERWISE
            sql
             SELECT trim(a.num_cuenta)     ,
                   b.descripcion tipo_pago
            INTO   $gr_reporte.infonavit_numcta,
                   $gr_reporte.afo_medio_pago
            FROM   ret_beneficiario a,
                   OUTER tab_pago   b
            WHERE  a.nss         = $lc_nss
            AND    a.consecutivo = $ld_consecutivo_disp
            AND    a.tipo_pago   = b.tipo_pago
            END sql
      END CASE
   END IF

   IF gs_afore = 568 THEN--Coppel
      #CPL-1099
      #Todo caso que no cuente con una resolución
      #debe ser considerado como regimen 97
      IF gr_reporte.regimen IS NULL THEN
         --LET gr_reporte.ley_pen = 'LEY 97'
         --LET gr_reporte.regimen = '97'
      END IF
   END IF

END FUNCTION

#*******************************************************************************
#Función : Ingresa_etapa
#Objetivo: Inserta etapa de control en dis_ctrl_proceso
#Parámetros de entrada:
#li_folio     => Folio proceso
#ls_etapa_cod => Num. etapa
#ls_estado    => parametro2
#lc_resultado => descripcion
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION Ingresa_etapa(li_folio,ls_etapa_cod,ls_estado,lc_resultado)

   DEFINE li_folio         INTEGER     ,
          ls_etapa_cod     DECIMAL(2,0),
          lc_resultado     CHAR(50)    ,
          ls_estado        SMALLINT

   DEFINE lc_hora_inicial  CHAR(08),
          lc_hora_final    CHAR(08)

   LET lc_hora_inicial = TIME
   LET lc_hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES(
       TODAY          ,   -- fecha_proceso
       "CTA"          ,   -- proceso_cod
       ls_etapa_cod   ,   -- etapa_cod
       lc_hora_inicial,   -- lc_hora_inicial
       lc_hora_final  ,   -- lc_hora_final
       NULL           ,   -- parametro1
       ls_estado      ,   -- parametro2
       NULL           ,   -- parametro3
       NULL           ,   -- parametro4
       NULL           ,   -- parametro5
       li_folio       ,   -- folio
       lc_resultado   ,   -- resultado
       USER           ,   -- usuario
       0                  -- consecutivo
      )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso etapa ",
            ls_etapa_cod," ",STATUS
      EXIT PROGRAM
   END IF

END FUNCTION

#*******************************************************************************
#Función : Actualiza_etapa
#Objetivo: Actualiza etapa de control en dis_ctrl_proceso
#Parámetros de entrada:
#li_folio     => Folio proceso
#ls_etapa_cod => Num. etapa
#li_pos       => Numero registros
#lc_resultado => descripcion
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION Actualiza_etapa(li_folio,ls_etapa_cod,li_pos,lc_resultado)

   DEFINE li_folio         INTEGER     ,
          ls_etapa_cod     DECIMAL(2,0),
          lc_resultado     CHAR(50)    ,
          li_pos           INTEGER

   DEFINE lc_hora_final    CHAR(08)

   LET lc_hora_final   = TIME

   UPDATE dis_ctrl_proceso
   SET    hora_final = lc_hora_final,
          parametro3 = li_pos       ,
          resultado  = lc_resultado
   WHERE  folio       = li_folio
   AND    proceso_cod = 'CTA'
   AND    etapa_cod   = ls_etapa_cod

END FUNCTION

#*******************************************************************************
#Función : despliega
#Objetivo: despliega valores que se imprimen en el reporte
#Parámetros de entrada:
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION despliega()

DISPLAY "_____________________________________________________________________"
DISPLAY "nss: ",gr_reporte.nss
DISPLAY "#1    tipo_registro          :", gr_reporte.tipo_registro           USING "&&"
DISPLAY "#2    consecutivo_envio      :", gr_reporte.consecutivo_envio       USING "&&&&&&&&&&&"
DISPLAY "#3    dia_corte              :", gr_reporte.dia_corte               USING "&&"
DISPLAY "#4    mes_corte              :", gr_reporte.mes_corte               USING "&&"
DISPLAY "#5    nombre_completo        :", gr_reporte.nombre_completo         CLIPPED
DISPLAY "#6    calle_num              :", gr_reporte.calle_num               CLIPPED
DISPLAY "#7    colonia                :", gr_reporte.colonia                 CLIPPED
DISPLAY "#8    cp                     :", gr_reporte.cp                      CLIPPED
DISPLAY "#9    Centro de reparto      :", gr_reporte.centro_reparto          CLIPPED
DISPLAY "#10  entidad_federal        :", gr_reporte.entidad_federal         CLIPPED
DISPLAY "#11  munic_delega           :", gr_reporte.munic_delega            CLIPPED
DISPLAY "#12  nss                    :", gr_reporte.nss                     CLIPPED
DISPLAY "#13  rfc                    :", gr_reporte.rfc                     CLIPPED
DISPLAY "#14  curp                   :", gr_reporte.curp                    CLIPPED
DISPLAY "#15  saldo_ini_ret          :", gr_reporte.saldo_ini_ret           USING "&&&&&&&&&&.&&"
DISPLAY "#16  saldo_ini_viv          :", gr_reporte.saldo_ini_viv           USING "&&&&&&&&&&.&&"
DISPLAY "#17  saldo_trans_ret        :", gr_reporte.saldo_trans_ret         USING "&&&&&&&&&&.&&"
DISPLAY "#18  saldo_trans_viv        :", gr_reporte.saldo_trans_viv         USING "&&&&&&&&&&.&&"
DISPLAY "#19  saldo_rem_ret          :", gr_reporte.saldo_rem_ret           USING "&&&&&&&&&&.&&"
DISPLAY "#20  saldo_rem_viv          :", gr_reporte.saldo_rem_viv           USING "&&&&&&&&&&.&&"
DISPLAY "#21  ley_pen                :", gr_reporte.ley_pen                 CLIPPED
DISPLAY "#22  regimen                :", gr_reporte.regimen                 CLIPPED
DISPLAY "#23  seguro                 :", gr_reporte.seguro                  CLIPPED
DISPLAY "#24  pension                :", gr_reporte.pension                 CLIPPED
DISPLAY "#25  pen_entidad            :", gr_reporte.pen_entidad             CLIPPED
DISPLAY "#26  pen_monto_trans        :", gr_reporte.pen_monto_trans         USING "&&&&&&&&&&.&&"
DISPLAY "#27  pen_fecha_trans        :", gr_reporte.pen_fecha_trans         USING "DDMMYYYY"
DISPLAY "#28  afo_entidad            :", gr_reporte.afo_entidad             CLIPPED
DISPLAY "#29  afo_medio_pago         :", gr_reporte.afo_medio_pago          CLIPPED
DISPLAY "#30  afo_rec_entregado      :", gr_reporte.afo_rec_entregado       USING "&&&&&&&&&&.&&"
DISPLAY "#31  afo_fecha_entrega      :", gr_reporte.afo_fecha_entrega       USING "DDMMYYYY"
DISPLAY "#32  afo_isr                :", gr_reporte.afo_isr                 USING "&&&&&&&&&&.&&"
DISPLAY "#33  infonavit_entidad      :", gr_reporte.infonavit_entidad       CLIPPED
DISPLAY "#34  infonavit_numcta       :", gr_reporte.infonavit_numcta        CLIPPED
DISPLAY "#35  infonavit_entregado    :", gr_reporte.infonavit_entregado     USING "&&&&&&&&&&.&&"
DISPLAY "#36  infonavit_fecha_entrega:", gr_reporte.infonavit_fecha_entrega USING "DDMMYYYY"
DISPLAY "#37  infonavit_isr          :", gr_reporte.infonavit_isr           USING "&&&&&&&&&&.&&"
DISPLAY "#38  fecha_emision          :", gr_reporte.fecha_emision           USING "DDMMYYYY"
DISPLAY "#39  fecha_ini_pen          :", gr_reporte.fecha_ini_pen           USING "DDMMYYYY"
DISPLAY "#40  monto_pen              :", gr_reporte.monto_pen               USING "&&&&&&&&&&.&&"
DISPLAY "#41  saldo_fin              :", gr_reporte.saldo_fin               USING "&&&&&&&&&&.&&"
DISPLAY "_____________________________________________________________________"

END FUNCTION

#*******************************************************************************
#Función : redondea
#Objetivo: Redondea 2 decimales
#Parámetros de entrada: ld_pesos6 => Saldo a 6 decimales
#Valores de retorno   : ld_pesos2 => Saldo a 2 decimales
#Variables globales   :
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION redondea(ld_pesos6)

   DEFINE ld_pesos6 DECIMAL(22,6)
   DEFINE ld_pesos2 DECIMAL(22,2)

   LET ld_pesos2 = ld_pesos6 * 1

   RETURN ld_pesos2

END FUNCTION

#*******************************************************************************
#Función : rpt_edc_final
#Objetivo: Reporte de archivo plano de estado de cuenta de pensionados
#Parámetros de entrada: ls_tipo_registro => tipo de registro a imprimir
#                                           encabezado, detalle, sumario
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
REPORT rpt_edc_final(ls_tipo_registro)
   DEFINE ls_tipo_registro SMALLINT

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      CASE ls_tipo_registro
         WHEN 1  --Encabezado
            PRINT COLUMN 001, '01'                            ,
                  COLUMN 003, '30'                            ,
                  COLUMN 005, '01'                            ,
                  COLUMN 007, gs_afore        USING "&&&"     ,
                  COLUMN 010, '15'                            ,
                  COLUMN 012, hoy             USING "DDMMYYYY",
                  COLUMN 020, '001'                           ,
                  COLUMN 023, gs_tipo_informe USING "&&"

         WHEN 2 --Detalle
             PRINT COLUMN 001, gr_reporte.tipo_registro                     USING "&&"           ,
                   COLUMN 003, gr_reporte.consecutivo_envio                 USING "&&&&&&&&&&&"  ,
                   COLUMN 014, gr_reporte.dia_corte                         USING "&&"           ,
                   COLUMN 016, gr_reporte.mes_corte                         USING "&&"           ,
                   COLUMN 018, gr_reporte.nombre_completo                   CLIPPED              ,
                   COLUMN 138, gr_reporte.calle_num                         CLIPPED              ,
                   COLUMN 200, gr_reporte.colonia                           CLIPPED              ,
                   COLUMN 260, gr_reporte.cp                                CLIPPED              ,
                   COLUMN 265, gr_reporte.centro_reparto                    CLIPPED              ,
                   COLUMN 270, gr_reporte.entidad_federal                   CLIPPED              ,
                   COLUMN 310, gr_reporte.munic_delega                      CLIPPED              ,
                   COLUMN 350, gr_reporte.nss                               CLIPPED              ,
                   COLUMN 361, gr_reporte.rfc                               CLIPPED              ,
                   COLUMN 374, gr_reporte.curp                              CLIPPED              ,
                   COLUMN 392, gr_reporte.saldo_ini_ret                     USING "&&&&&&&&&&.&&",
                   COLUMN 405, gr_reporte.saldo_ini_viv                     USING "&&&&&&&&&&.&&",
                   COLUMN 418, gr_reporte.saldo_trans_ret                   USING "&&&&&&&&&&.&&",
                   COLUMN 431, gr_reporte.saldo_trans_viv                   USING "&&&&&&&&&&.&&",
                   COLUMN 444, gr_reporte.saldo_rem_ret                     USING "&&&&&&&&&&.&&",
                   COLUMN 457, gr_reporte.saldo_rem_viv                     USING "&&&&&&&&&&.&&",
                   COLUMN 470, gr_reporte.ley_pen                           CLIPPED              ,
                   COLUMN 476, gr_reporte.regimen                           CLIPPED              ,
                   COLUMN 478, gr_reporte.seguro                            CLIPPED              ,
                   COLUMN 480, gr_reporte.pension                           CLIPPED              ,
                   COLUMN 482, gr_reporte.pen_entidad                       CLIPPED              ,
                   COLUMN 532, gr_reporte.pen_monto_trans                   USING "&&&&&&&&&&.&&",
                   COLUMN 545, gr_reporte.pen_fecha_trans                   USING "DDMMYYYY"     ,
                   COLUMN 553, gr_reporte.afo_entidad                       CLIPPED              ,
                   COLUMN 603, gr_reporte.afo_medio_pago                    CLIPPED              ,
                   COLUMN 643, gr_reporte.afo_rec_entregado                 USING "&&&&&&&&&&.&&",
                   COLUMN 656, gr_reporte.afo_fecha_entrega                 USING "DDMMYYYY"     ,
                   COLUMN 664, gr_reporte.afo_isr                           USING "&&&&&&&&&&.&&",
                   COLUMN 677, gr_reporte.infonavit_entidad                 CLIPPED              ,
                   COLUMN 727, gr_reporte.infonavit_numcta                  CLIPPED              ,
                   COLUMN 747, gr_reporte.infonavit_entregado               USING "&&&&&&&&&&.&&",
                   COLUMN 760, gr_reporte.infonavit_fecha_entrega           USING "DDMMYYYY"     ,
                   COLUMN 768, gr_reporte.infonavit_isr                     USING "&&&&&&&&&&.&&",
                   COLUMN 781, gr_reporte.fecha_emision                     USING "DDMMYYYY"     ,
                   COLUMN 789, gr_reporte.fecha_ini_pen                     USING "DDMMYYYY"     ,
                   COLUMN 797, gr_reporte.monto_pen                         USING "&&&&&&&&&&.&&",
                   COLUMN 810, gr_reporte.saldo_fin                         USING "&&&&&&&&&&.&&"

            LET gi_registros = gi_registros + 1

         WHEN 99 --Sumario
             PRINT COLUMN 001, '99'                         ,
                  COLUMN 003, hoy          USING "DDMMYYYY",
                  COLUMN 011, '00000001'                          ,
                  COLUMN 019, gi_registros USING "&&&&&&&&"
      END CASE

END REPORT