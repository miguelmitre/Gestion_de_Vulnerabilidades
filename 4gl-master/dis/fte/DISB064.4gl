###############################################################################
#Proyecto          => Safre                                                   #
#Propietario       => E.F.P.                                                  #
#Modulo            => DIS.                                                    #
#Programa          => DISB064                                                 #
#Descripcion       => Genara Saldos e intereses vivienda p/envio a Procesar   #
#Fecha Inicio      => 29-enero-2004.                                          #
#Fecha Termino     =>                                                         #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
#                  => ALEJANDRO RAMIREZ                                       #
#Modify.           => Alejandro Ramirez 28 Abril 2004                         #
#Descr.            => Se agrego el reporte impreso del sumario                #
###############################################################################

DATABASE safre_af
GLOBALS

   DEFINE  i             INTEGER

   DEFINE
      g_param_dis        RECORD LIKE seg_modulo.* ,
      w_codigo_afore     LIKE tab_afore_local.codigo_afore,
      hoy                DATE,
      g_usuario          CHAR (08)

    DEFINE G_LISTA       CHAR(300)
    DEFINE G_LISTA2      CHAR(300)
    DEFINE G_LISTA3      CHAR(300)
    DEFINE G_LISTA_IMP   CHAR(300)

    DEFINE v_rep2        CHAR(300)
    DEFINE v_rep3        CHAR(300)
    DEFINE v_rep4        CHAR(300)

    DEFINE l_record  RECORD
          nss             CHAR(11)
    END RECORD


    DEFINE v_rep RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),
      ident_operacion    CHAR(02),
      tipo_ent_origen    CHAR(02),
      cve_ent_origen     CHAR(03),
      tipo_ent_destino   CHAR(02),
      cve_ent_destino    CHAR(03),
      fec_presentacion   CHAR(08),
      consec_lote        CHAR(03),
      result_operacion   CHAR(02),
      motivo_rechazo1    CHAR(03),
      motivo_rechazo2    CHAR(03),
      motivo_rechazo3    CHAR(03),
      filler             CHAR(102)
   END RECORD

   DEFINE vdeta RECORD
      tipo_registro      CHAR(02),
      curp               CHAR(18),
      nss                CHAR(11),
      saldo_viv97        DECIMAL(14,2),
      saldo_viv92        DECIMAL(14,2),
      interes_viv97      DECIMAL(14,2),
      interes_viv92      DECIMAL(14,2),
      part_viv97         DECIMAL(18,6),  ----jerry
      part_viv92         DECIMAL(18,6),  ----jerry
      result_operacion   CHAR(02),
      diag_proceso       CHAR(09),  ----jerry
      ind_proceso        CHAR(01),  ----jerry
      filler             CHAR(07)   ----jerry
   END RECORD

   DEFINE v_sum RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),      ----jerry
      ident_operacion    CHAR(02),      ----jerry
      tipo_ent_origen    CHAR(02),      ----jerry
      cve_ent_origen     CHAR(03),      ----jerry
      tipo_ent_destino   CHAR(02),      ----jerry
      cve_ent_destino    CHAR(03),      ----jerry
      fec_presentacion   CHAR(08),      ----jerry
      consec_lote        CHAR(03),      ----jerry
      reg_detalle        INTEGER,
      tot_saldo_viv97    DECIMAL(15,2),
      tot_saldo_viv92    DECIMAL(15,2),
      tot_interes_viv97  DECIMAL(15,2),
      tot_interes_viv92  DECIMAL(15,2),
      tot_part_viv97     DECIMAL(18,6), ----jerry
      tot_part_viv92     DECIMAL(18,6), ----jerry
      filler             CHAR(08)
   END RECORD

   DEFINE intviv         DECIMAL(13,2)
   DEFINE intsub         SMALLINT
   DEFINE sdoviv         DECIMAL(13,2)
   DEFINE sdosub         SMALLINT

   DEFINE hora           CHAR (08)
   DEFINE vnum_lote      INTEGER
   DEFINE vlote          INTEGER
   DEFINE vconsec        INTEGER
   DEFINE prog           SMALLINT
   DEFINE det_cza        SMALLINT
   DEFINE pos            INTEGER

   DEFINE g_saldo        CHAR(600),
          g_saldo2       CHAR(600),
          g_saldo1       CHAR(600),
          g_interes      CHAR(600),
          g_sin_cta      CHAR(600),
          g_fecha_corte  DATE
   DEFINE acelera_on    CHAR(30),
          acelera_off    CHAR(30)
   DEFINE opc            CHAR(01)

   DEFINE cont_deta      INTEGER
   DEFINE suma_viv97     DECIMAL(15,2)
   DEFINE suma_viv92     DECIMAL(15,2)
   DEFINE suma_int97     DECIMAL(15,2)
   DEFINE suma_int92     DECIMAL(15,2)
   DEFINE suma_part97    DECIMAL(18,6) --jerry
   DEFINE suma_part92    DECIMAL(18,6) --jerry

   DEFINE vsaldo97   DECIMAL(15,6),  ----jerry
          vsaldo92   DECIMAL(15,6)   ----jerry

   DEFINE ant_saldo_viv97,
          ant_saldo_viv92 DECIMAL(15,2)

   DEFINE band           SMALLINT
   DEFINE enter          CHAR(1)
   DEFINE vfec_habil     CHAR(6)
   DEFINE pos1           CHAR(01)
   DEFINE pos2           CHAR(01)
   DEFINE pos3           CHAR(01)
   DEFINE pos4           CHAR(01)


   DEFINE vmarca_cod SMALLINT,
          impresion  CHAR(250)

   DEFINE vfecha_ant DATE

   DEFINE cla_sel     CHAR(1000),
          cla_upd     CHAR(1000),
          vhora_final CHAR(08),
          vresultado  CHAR(50),
          vrow        INTEGER

   DEFINE vprecio_del_dia DECIMAL(22,14)

END GLOBALS

MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST  ,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   LET g_fecha_corte = ARG_VAL(1)

   CALL Inicializa()

   LET vfec_habil = g_fecha_corte USING "yymmdd"

   CALL Layout_cza()

   CALL Layout_det()

   CALL Layout_sum()

   CALL genera_archivo()

   LET vhora_final = TIME
   LET vresultado = "PROCESO SALDOS VIVIENDA TERMINADO"

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM dis_ctrl_proceso ",
                 "WHERE proceso_cod = 'DISB064' ",
                  " AND etapa_cod = 8 " CLIPPED

   PREPARE claexe99 FROM cla_sel
   DECLARE cur_proceso99 CURSOR FOR claexe99
   OPEN cur_proceso99
      FETCH cur_proceso99 INTO vrow
   CLOSE cur_proceso99

   LET cla_upd = "UPDATE dis_ctrl_proceso ",
            "SET    dis_ctrl_proceso.hora_final = ","'",vhora_final,"'",",",
            "       dis_ctrl_proceso.folio      = 0 ",",",
            "       dis_ctrl_proceso.resultado  = ","'",vresultado,"'",
            " WHERE  dis_ctrl_proceso.proceso_cod  = 'DISB064' ",
            " AND    dis_ctrl_proceso.etapa_cod    = 8 ",
            " AND    dis_ctrl_proceso.consecutivo = ",vrow CLIPPED
   PREPARE claexe11 FROM cla_upd
   EXECUTE claexe11

END MAIN

FUNCTION Inicializa()

   LET hoy = TODAY

   SELECT  ruta_envio
   INTO    g_param_dis.ruta_envio
   FROM    seg_modulo
   WHERE   modulo_cod='dis'

   SELECT  codigo_afore,USER
   INTO    w_codigo_afore,g_usuario
   FROM    tab_afore_local

   SELECT precio_del_dia
   INTO   vprecio_del_dia
   FROM   glo_valor_accion
   WHERE  codigo_siefore = 11
   AND    fecha_valuacion = g_fecha_corte

let vprecio_del_dia = 1

END FUNCTION

##----------------------------------------------------------------------------
## Layout Encabezado
##----------------------------------------------------------------------------
FUNCTION Layout_cza()

   LET vconsec  = 1

   LET G_LISTA = g_param_dis.ruta_envio CLIPPED,"/CZA_SDO"
----   LET G_LISTA = "CZA_SDO"


   LET v_rep.tipo_registro      = '01'
   LET v_rep.ident_servicio     = '12'
   LET v_rep.ident_operacion    = '01'
   LET v_rep.tipo_ent_origen    = '01'
   LET v_rep.cve_ent_origen     = w_codigo_afore
   LET v_rep.tipo_ent_destino   = '03'
   LET v_rep.cve_ent_destino    = '001'
   LET v_rep.fec_presentacion   = TODAY USING "YYYYMMDD"
   LET v_rep.consec_lote        = '001'
   LET v_rep.result_operacion   = '  '
   LET v_rep.motivo_rechazo1    = '   '
   LET v_rep.motivo_rechazo2    = '   '
   LET v_rep.motivo_rechazo3    = '   '
   FOR i = 1 to 102
       LET v_rep.filler[i]    = ' '
   END FOR


   START REPORT rep_cza TO G_LISTA
      OUTPUT TO REPORT rep_cza(v_rep.*)
   FINISH REPORT rep_cza

    DISPLAY  "ENCABEZADO GENERADO...."

END FUNCTION

REPORT rep_cza(v_rep)  ----jerry

    DEFINE v_rep RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),
      ident_operacion    CHAR(02),
      tipo_ent_origen    CHAR(02),
      cve_ent_origen     CHAR(03),
      tipo_ent_destino   CHAR(02),
      cve_ent_destino    CHAR(03),
      fec_presentacion   CHAR(08),
      consec_lote        CHAR(03),
      result_operacion   CHAR(02),
      motivo_rechazo1    CHAR(03),
      motivo_rechazo2    CHAR(03),
      motivo_rechazo3    CHAR(03),
      filler             CHAR(102)
--      filler2            CHAR(01) ----jerry
   END RECORD

   OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
   FORMAT
      ON EVERY ROW
         PRINT COLUMN 1,v_rep.tipo_registro,
                        v_rep.ident_servicio,
                        v_rep.ident_operacion,
                        v_rep.tipo_ent_origen,
                        v_rep.cve_ent_origen,
                        v_rep.tipo_ent_destino,
                        v_rep.cve_ent_destino,
                        v_rep.fec_presentacion,
                        v_rep.consec_lote,
                        v_rep.result_operacion,
                        v_rep.motivo_rechazo1,
                        v_rep.motivo_rechazo2,
                        v_rep.motivo_rechazo3,
                        v_rep.filler
END REPORT

##----------------------------------------------------------------------------
## Layout Detalle
##----------------------------------------------------------------------------
FUNCTION Layout_det()

   DEFINE vcurp    CHAR(18)
   DEFINE vnss     CHAR(11)
   DEFINE i        INTEGER
   DEFINE tot      INTEGER

   LET cont_deta   = 0
   LET suma_viv97  = 0.0
   LET suma_viv92  = 0.0
   LET suma_int97  = 0.0
   LET suma_int92  = 0.0
   LET suma_part97 = 0.0
   LET suma_part92 = 0.0
   LET vsaldo97   = 0   ----jerry
   LET vsaldo92   = 0   ----jerry
   LET band = FALSE

   LET acelera_on = "set pdqpriority high"
   LET acelera_on = acelera_on CLIPPED

   LET acelera_off = "set pdqpriority off"
   LET acelera_off = acelera_on CLIPPED

   PREPARE exe_acelera_on  FROM acelera_on   ----jerry
   PREPARE exe_acelera_off FROM acelera_off  ----jerry


   EXECUTE exe_acelera_on

   SELECT unique nss
   FROM   safre_af:cta_saldo_viv
   INTO   TEMP tmp_cta_saldo_viv

   SELECT unique nss
   FROM   safre_af:cta_ctr_cuenta
   INTO   TEMP tmp_cta_ctr_cuenta

   create index tmpcta_saldoviv_1 on tmp_cta_saldo_viv(nss);

   create index tmpcta_ctrcta_1 on tmp_cta_ctr_cuenta(nss);

   EXECUTE exe_acelera_off

   UPDATE STATISTICS FOR TABLE tmp_cta_saldo_viv;
   UPDATE STATISTICS FOR TABLE tmp_cta_ctr_cuenta;

   EXECUTE exe_acelera_on

   DECLARE cursor_1 CURSOR FOR
   SELECT  nss
   FROM    tmp_cta_ctr_cuenta


------   LET G_LISTA2 = g_param_dis.ruta_envio CLIPPED,"/EXCEP.VIV"  ----jerry
-------   LET G_LISTA2 = "EXCEP.VIV"
------   START REPORT rep_excepcion TO G_LISTA2


   LET G_LISTA2 = g_param_dis.ruta_envio CLIPPED,"/DET_SDO"
-----   LET G_LISTA2 = "DET_SDO"



   START REPORT rep_det TO G_LISTA2

   FOREACH cursor_1 INTO l_record.*

         LET vdeta.saldo_viv97   = 0
         LET vdeta.saldo_viv92   = 0
         LET vdeta.interes_viv97 = 0
         LET vdeta.interes_viv92 = 0
         LET vdeta.part_viv97    = 0
         LET vdeta.part_viv92    = 0
         LET vsaldo97   = 0   --jerry
         LET vsaldo92   = 0   --jerry
         LET pos1 = "0"
         LET pos2 = "0"
         LET pos3 = "0"
         LET pos4 = "0"
         LET vdeta.ind_proceso   = " "


      LET vdeta.tipo_registro       = "02"

      SELECT n_unico
      INTO   vdeta.curp
      FROM   safre_af:afi_mae_afiliado
      WHERE  n_seguro = l_record.nss

      IF vdeta.curp IS NULL THEN
         LET vdeta.curp = " "
      END IF

      LET vdeta.nss = l_record.nss

      SELECT nss
      FROM   tmp_cta_saldo_viv
      WHERE  nss = vdeta.nss

      IF STATUS <> 100 THEN  ---- si encontro nss ----
         -- PARA SALDO DE VIV97
         SELECT sum(monto_en_pesos)
         INTO   vsaldo97              --jerry
         FROM   cta_saldo_viv
         WHERE  nss = l_record.nss
         AND    subcuenta = 4
         IF vsaldo97 IS NULL THEN
            LET vsaldo97 = 0
         END IF

         LET vdeta.saldo_viv97 = vsaldo97    --jerry

         IF vdeta.saldo_viv97 IS NULL THEN
            LET vdeta.saldo_viv97 = 0
         END IF

         -- PARA SALDO DE VIV92
         SELECT sum(monto_en_pesos)
         INTO   vsaldo92              --jerry
         FROM   cta_saldo_viv
         WHERE  nss =l_record.nss
         AND    subcuenta = 8
         IF vsaldo92 IS NULL THEN
            LET vsaldo92 = 0
         END IF

         LET vdeta.saldo_viv92 = vsaldo92

         IF vdeta.saldo_viv92 IS NULL THEN
            LET vdeta.saldo_viv92 = 0
         END IF

         -- PARA INTERES DE VIV97
         SELECT sum(monto_en_pesos)
         INTO   vdeta.interes_viv97
         FROM   safre_tmp:cta_interes_viv
         WHERE  nss  = l_record.nss
         AND    subcuenta = 4

         IF vdeta.interes_viv97 IS NULL THEN
            LET vdeta.interes_viv97 = 0
         END IF

         -- PARA INTERES DE VIV92
         SELECT sum(monto_en_pesos)
         INTO   vdeta.interes_viv92
         FROM   safre_tmp:cta_interes_viv
         WHERE  nss = l_record.nss
         AND    subcuenta = 8

         IF vdeta.interes_viv92 IS NULL THEN
            LET vdeta.interes_viv92 = 0
         END IF

         IF vdeta.saldo_viv97 < 0 THEN
            LET pos1 = "-"
         ELSE
            LET pos1 = "0"
         END IF

         IF vdeta.saldo_viv92 < 0 THEN
            LET pos2 = "-"
         ELSE
            LET pos2 = "0"
         END IF
 
         IF vdeta.interes_viv97 < 0 THEN
            LET pos3 = "-"
         ELSE
            LET pos3 = "0"
         END IF
 
         IF vdeta.interes_viv92 < 0 THEN
            LET pos4 = "-"
         ELSE
            LET pos4 = "0"
         END IF

         LET vdeta.saldo_viv97 = vdeta.saldo_viv97 - vdeta.interes_viv97
         LET vdeta.saldo_viv92 = vdeta.saldo_viv92 - vdeta.interes_viv92
         LET vdeta.part_viv97  = vsaldo97 * vprecio_del_dia   --jerry
         LET vdeta.part_viv92  = vsaldo92 * vprecio_del_dia   --jerry

            -- PARA EL SUMARIO
            LET suma_viv97        = suma_viv97 +  vdeta.saldo_viv97
            LET suma_viv92        = suma_viv92 +  vdeta.saldo_viv92
            LET suma_int97        = suma_int97 +  vdeta.interes_viv97
            LET suma_int92        = suma_int92 +  vdeta.interes_viv92
            LET suma_part97       = suma_part97+ vdeta.part_viv97
            LET suma_part92       = suma_part92+ vdeta.part_viv92

            LET vdeta.result_operacion = '  '
            LET vdeta.diag_proceso     = ' '
            LET vdeta.ind_proceso      = " "   ----jerry
            LET vdeta.filler           = '        '

         SELECT marca_cod    ----jerry
         INTO   vmarca_cod
         FROM   cta_act_marca
         WHERE  nss = l_record.nss
         AND    marca_cod in (120,130)

         IF STATUS=100 THEN ---- no encontro marca ----
            LET vdeta.ind_proceso      = " "   ----jerry
         ELSE         
            IF (vdeta.saldo_viv97 + vdeta.saldo_viv92) <> 0 THEN
               IF vmarca_cod = 120 THEN
                  LET vdeta.ind_proceso = "1"
               ELSE
                  LET vdeta.ind_proceso = "2"
               END IF
            END IF
         END IF
      ELSE
         ---- imprime nss que no tienen saldo en vivienda (cta_saldo_viv)
         LET vdeta.saldo_viv97   = 0
         LET vdeta.saldo_viv92   = 0
         LET vdeta.interes_viv97 = 0
         LET vdeta.interes_viv92 = 0
         LET vdeta.part_viv97 = 0
         LET vdeta.part_viv92 = 0
         LET pos1 = "0"
         LET pos2 = "0"
         LET pos3 = "0"
         LET pos4 = "0"
         LET vdeta.ind_proceso   = " "

      END IF

      LET cont_deta         = cont_deta  +  1  --jerry

      OUTPUT TO REPORT rep_det(vdeta.*)  ----jerry

   END FOREACH
   FINISH REPORT rep_det

   DISPLAY "DETALLE GENERADO...."

END FUNCTION

REPORT rep_det(vdeta) ----jerry
   DEFINE vdeta RECORD
      tipo_registro      CHAR(02),
      curp               CHAR(18),
      nss                CHAR(11),
      saldo_viv97        DECIMAL(14,2),
      saldo_viv92        DECIMAL(14,2),
      interes_viv97      DECIMAL(14,2),
      interes_viv92      DECIMAL(14,2),
      part_viv97         DECIMAL(18,6),
      part_viv92         DECIMAL(18,6),
      result_operacion   CHAR(02),
      diag_proceso       CHAR(09),  ----jerry
      ind_proceso        CHAR(01),  ----jerry
      filler             CHAR(07)   ----jerry
--      filler2            CHAR(01)
   END RECORD

   OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
   FORMAT
      ON EVERY ROW

         LET vdeta.saldo_viv97   = vdeta.saldo_viv97 * 100
         LET vdeta.saldo_viv92   = vdeta.saldo_viv92 * 100
         LET vdeta.interes_viv97 = vdeta.interes_viv97 * 100
         LET vdeta.interes_viv92 = vdeta.interes_viv92 * 100
         LET vdeta.part_viv97    = vdeta.part_viv97 * 1000000
         LET vdeta.part_viv92    = vdeta.part_viv92 * 1000000

         PRINT COLUMN 1,vdeta.tipo_registro,
                      vdeta.curp,
                      vdeta.nss,
                      pos1,
                      vdeta.saldo_viv97      USING "&&&&&&&&&&&&&&",
                      pos2,
                      vdeta.saldo_viv92      USING "&&&&&&&&&&&&&&",
                      pos3,
                      vdeta.interes_viv97    USING "&&&&&&&&&&&&&&",
                      pos4,
                      vdeta.interes_viv92    USING "&&&&&&&&&&&&&&",
                      vdeta.part_viv97       USING "&&&&&&&&&&&&&&&",
                      vdeta.part_viv92       USING "&&&&&&&&&&&&&&&",
                      vdeta.result_operacion,
                      vdeta.diag_proceso,
                      vdeta.ind_proceso,
                      vdeta.filler
--                     vdeta.filler2              ,"|"  #17 ----jerry

END REPORT

REPORT rep_excepcion(vnss,
                     vsaldo_viv97,
                     vsaldo_viv92,
                     vinteres_viv97,
                     vinteres_viv92)

   DEFINE 
      vnss                CHAR(11),
      vsaldo_viv97        DECIMAL(14,2),
      vsaldo_viv92        DECIMAL(14,2),
      vinteres_viv97      DECIMAL(14,2),
      vinteres_viv92      DECIMAL(14,2)

   OUTPUT
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
   FORMAT
      ON EVERY ROW
         PRINT vnss,"|",
               vsaldo_viv97    USING "----------.--","|",
               vsaldo_viv92    USING "----------.--","|",
               vinteres_viv97  USING "----------.--","|",
               vinteres_viv92  USING "----------.--"

END REPORT

##----------------------------------------------------------------------------
## Layout Sumario
##----------------------------------------------------------------------------
FUNCTION Layout_sum()

   LET G_LISTA3 = g_param_dis.ruta_envio CLIPPED,"/SUM_SDO"
----   LET G_LISTA3 = "SUM_SDO"
   LET G_LISTA_IMP = g_param_dis.ruta_envio CLIPPED,"/IMPRIME"



   LET v_sum.tipo_registro      = '09'
   LET v_sum.ident_servicio     = '12'
   LET v_sum.ident_operacion    = '01'
   LET v_sum.tipo_ent_origen    = '01'
   LET v_sum.cve_ent_origen     = w_codigo_afore
   LET v_sum.tipo_ent_destino   = '03'
   LET v_sum.cve_ent_destino    = '001'
   LET v_sum.fec_presentacion   = hoy USING "YYYYMMDD" ----jerry
   LET v_sum.consec_lote        = '001'

   LET v_sum.reg_detalle        = cont_deta USING '&&&&&&&&&'
   LET v_sum.tot_saldo_viv97    = suma_viv97
   LET v_sum.tot_saldo_viv92    = suma_viv92
   LET v_sum.tot_interes_viv97  = suma_int97
   LET v_sum.tot_interes_viv92  = suma_int92
   LET v_sum.tot_part_viv97     = suma_part97
   LET v_sum.tot_part_viv92     = suma_part92

   LET v_sum.tot_saldo_viv97    = v_sum.tot_saldo_viv97 * 100
   LET v_sum.tot_saldo_viv92    = v_sum.tot_saldo_viv92 * 100
   LET v_sum.tot_interes_viv97  = v_sum.tot_interes_viv97   * 100
   LET v_sum.tot_interes_viv92  = v_sum.tot_interes_viv92   * 100
   LET v_sum.tot_part_viv97     = v_sum.tot_part_viv97  * 1000000
   LET v_sum.tot_part_viv92     = v_sum.tot_part_viv92  * 1000000


   START REPORT rep_sum TO G_LISTA3
   --PARA LA IMPRESION
   START REPORT r_report_imp TO G_LISTA_IMP

      OUTPUT TO REPORT rep_sum(v_sum.*)
      --Manda a impresion el sumario
      OUTPUT TO REPORT r_report_imp(v_sum.*)

   FINISH REPORT rep_sum
   FINISH REPORT r_report_imp

   DISPLAY  "SUMARIO GENERADO...."

   LET impresion = "lp ",G_LISTA_IMP
   RUN impresion

END FUNCTION

REPORT rep_sum(v_sum)   ----jerry
   DEFINE v_sum RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),      ----jerry
      ident_operacion    CHAR(02),      ----jerry
      tipo_ent_origen    CHAR(02),      ----jerry
      cve_ent_origen     CHAR(03),      ----jerry
      tipo_ent_destino   CHAR(02),      ----jerry
      cve_ent_destino    CHAR(03),      ----jerry
      fec_presentacion   CHAR(08),      ----jerry
      consec_lote        CHAR(03),      ----jerry
      reg_detalle        INTEGER,
      tot_saldo_viv97    DECIMAL(15,2),
      tot_saldo_viv92    DECIMAL(15,2),
      tot_interes_viv97  DECIMAL(15,2),
      tot_interes_viv92  DECIMAL(15,2),
      tot_part_viv97     DECIMAL(18,6), ----jerry
      tot_part_viv92     DECIMAL(18,6), ----jerry
      filler             CHAR(08)       ----jerry
--      filler2            CHAR(01)
   END RECORD

   OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
   FORMAT
      ON EVERY ROW
         PRINT COLUMN 1,v_sum.tipo_registro,
                        v_sum.ident_servicio,
                        v_sum.ident_operacion,
                        v_sum.tipo_ent_origen,
                        v_sum.cve_ent_origen,
                        v_sum.tipo_ent_destino,
                        v_sum.cve_ent_destino,
                        v_sum.fec_presentacion,
                        v_sum.consec_lote,

                        v_sum.reg_detalle       USING '&&&&&&&&&',
                        v_sum.tot_saldo_viv97   USING '&&&&&&&&&&&&&&&',
                        v_sum.tot_saldo_viv92   USING '&&&&&&&&&&&&&&&',
                        v_sum.tot_interes_viv97 USING '&&&&&&&&&&&&&&&',
                        v_sum.tot_interes_viv92 USING '&&&&&&&&&&&&&&&',
                        v_sum.tot_part_viv97    USING '&&&&&&&&&&&&&&&&&&',
                        v_sum.tot_part_viv92    USING '&&&&&&&&&&&&&&&&&&',
                        v_sum.filler
END REPORT



##----------------------------------------------------------------------------
## Reporte para la impresion del sumario                  
##----------------------------------------------------------------------------

REPORT r_report_imp (v_sum2) 

   DEFINE v_sum2 RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),      ----jerry
      ident_operacion    CHAR(02),      ----jerry
      tipo_ent_origen    CHAR(02),      ----jerry
      cve_ent_origen     CHAR(03),      ----jerry
      tipo_ent_destino   CHAR(02),      ----jerry
      cve_ent_destino    CHAR(03),      ----jerry
      fec_presentacion   CHAR(08),      ----jerry
      consec_lote        CHAR(03),      ----jerry
      reg_detalle        INTEGER,
      tot_saldo_viv97    DECIMAL(15,2),
      tot_saldo_viv92    DECIMAL(15,2),
      tot_interes_viv97  DECIMAL(15,2),
      tot_interes_viv92  DECIMAL(15,2),
      tot_part_viv97     DECIMAL(18,6), ----jerry
      tot_part_viv92     DECIMAL(18,6), ----jerry
      filler             CHAR(08)       ----jerry
--      filler2            CHAR(01)
   END RECORD

  OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   90

  FORMAT
     PAGE HEADER

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 02,'\033(s7B',"Pantalla DISB064",'\033(s0B',"                                                                                                                          ",'\033(s7B',"Fecha : ",'\033(s0B',today USING "dd-mm-yyyy"
      SKIP 2 LINE
      PRINT COLUMN 70,'\033(s7B',"Resultados Finales de Intereses de Vivienda",'\033(s0B'
      SKIP 1 LINE

      PRINT COLUMN  002,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'

      SKIP 1 LINE

      PRINT COLUMN 02,'\033(s7B',"     Num regis      Mto Sdoviv97       Mto Sdoviv92         Mto Intviv97       Mto Intviv92          Partviv97         Partviv92 ",'\033(s0B'
      PRINT COLUMN  002,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'
            SKIP 1 LINE


      PRINT COLUMN 002,v_sum2.reg_detalle CLIPPED,
            COLUMN 020,v_sum2.tot_saldo_viv97      USING "&&&&&&&&&&&&.&&",
            COLUMN 040,v_sum2.tot_saldo_viv92      USING "&&&&&&&&&&&&.&&",
            COLUMN 060,v_sum2.tot_interes_viv97    USING "&&&&&&&&&&&&.&&",
            COLUMN 080,v_sum2.tot_interes_viv92    USING "&&&&&&&&&&&&.&&",
            COLUMN 100,v_sum2.tot_part_viv97       USING "&&&&&&&&&&&&.&&",
            COLUMN 120,v_sum2.tot_part_viv92       USING "&&&&&&&&&&&&.&&"



END REPORT
##----------------------------------------------------------------------------
## Funcion que concatena las partes del archivo a enviarse
##----------------------------------------------------------------------------
FUNCTION genera_archivo()

  DEFINE
      ejecuta CHAR(200)

   LET ejecuta="cat ",g_param_dis.ruta_envio CLIPPED,"/CZA_SDO ",
                      g_param_dis.ruta_envio CLIPPED,"/DET_SDO ",
                      g_param_dis.ruta_envio CLIPPED,"/SUM_SDO > ",
                      g_param_dis.ruta_envio CLIPPED,"/",vfec_habil,".VIV"

------  let ejecuta = "cat CZA_SDO DET_SDO SUM_SDO > ",vfec_habil,".VIV"

   RUN ejecuta

END FUNCTION
