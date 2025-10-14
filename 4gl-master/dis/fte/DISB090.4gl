###############################################################################
#Proyecto          => Safre                                                   #
#Propietario       => E.F.P.                                                  #
#Modulo            => DIS.                                                    #
#Programa          => DISB090                                                 #
#Descripcion       => Genara Saldos e intereses vivienda p/envio a Procesar   #
#Fecha Inicio      => 22-abril-2005.                                          #
#Fecha Termino     =>                                                         #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE i INTEGER

   DEFINE g_param_dis     RECORD LIKE seg_modulo.* ,
          w_codigo_afore  LIKE tab_afore_local.codigo_afore,
          hoy             DATE,
          g_usuario       CHAR (08)

    DEFINE G_LISTA       CHAR(300)
    DEFINE G_LISTA2      CHAR(300)
    DEFINE G_LISTA3      CHAR(300)
    DEFINE G_LISTA_IMP   CHAR(300)

    DEFINE v_rep2        CHAR(300)
    DEFINE v_rep3        CHAR(300)
    DEFINE v_rep4        CHAR(300)

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

   DEFINE vprecio_del_dia DECIMAL(22,14),
          vfactor         DECIMAL(22,14)

   DEFINE r_sdo RECORD
      nss   CHAR(11),
      viv97 DECIMAL(22,6),
      viv92 DECIMAL(22,6) 
   END RECORD

END GLOBALS

MAIN

   LET g_fecha_corte = "03/31/2005"

   CALL Inicializa()

   LET vfec_habil = g_fecha_corte USING "yymmdd"

   CALL Layout_cza()

   CALL Genera_saldo()

   CALL Layout_det()

   CALL Layout_sum()

   CALL genera_archivo()

   LET vhora_final = TIME
   LET vresultado = "PROCESO SALDOS VIVIENDA TERMINADO"

   DISPLAY vresultado
   DISPLAY vhora_final

END MAIN

FUNCTION Inicializa()

   LET hoy = TODAY

   SELECT  ruta_envio
   INTO    g_param_dis.ruta_envio
   FROM    seg_modulo
   WHERE   modulo_cod='dis'

   SELECT  codigo_afore,
           USER
   INTO    w_codigo_afore,
           g_usuario
   FROM    tab_afore_local

   SELECT precio_del_dia
   INTO   vprecio_del_dia
   FROM   glo_valor_accion
   WHERE  codigo_siefore = 11
   AND    fecha_valuacion = g_fecha_corte + 1

   LET vfactor = 0.04670936425562

END FUNCTION

##----------------------------------------------------------------------------
## Layout Encabezado
##----------------------------------------------------------------------------
FUNCTION Layout_cza()

   LET vconsec  = 1

   LET G_LISTA = g_param_dis.ruta_envio CLIPPED,"/CZA_SDO"

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

FUNCTION Genera_saldo()
   DEFINE r_viv  RECORD
      nss  CHAR(11),
      sub  SMALLINT,
      part DECIMAL(22,6)
   END RECORD

   WHENEVER ERROR CONTINUE
   DATABASE safre_tmp
   DROP TABLE tmp_saldo_viv
   CREATE TABLE tmp_saldo_viv
     (
       nss char(11),
       viv97 decimal(22,6),
       viv92 decimal(22,6)
     );
   DATABASE safre_af
   WHENEVER ERROR STOP
   
   LET acelera_on = "set pdqpriority high"    
   LET acelera_on = acelera_on CLIPPED        
   PREPARE exe_acelera_on  FROM acelera_on    
   EXECUTE exe_acelera_on                     

   DECLARE cur_viv CURSOR FOR
   SELECT  nss,
           subcuenta,
           sum(monto_en_acciones)
   FROM    dis_cuenta
   WHERE   fecha_conversion <= g_fecha_corte
   AND     subcuenta in (4,8)
{
and nss in (
"01441306170",
"01441804364",
"01442028021",
"01442035703",
"01442225981",
"01442319610",
"01442344972",
"01442349500")
}

   GROUP   BY nss,subcuenta
   ORDER   BY nss,subcuenta

   LET G_LISTA2 = g_param_dis.ruta_envio CLIPPED,"/sdo"

   START REPORT rep_sdo TO G_LISTA2

   FOREACH cur_viv INTO r_viv.*
      OUTPUT TO REPORT rep_sdo(r_viv.*)
   END FOREACH

   FINISH REPORT rep_sdo

   DATABASE safre_tmp

   CREATE INDEX tmp_saldo_viv_1 ON tmp_saldo_viv (nss);

   LET acelera_off = "set pdqpriority off"
   LET acelera_off = acelera_off CLIPPED
   PREPARE exe_acelera_off FROM acelera_off
   EXECUTE exe_acelera_off 

   UPDATE STATISTICS FOR TABLE tmp_saldo_viv

   DATABASE safre_af

END FUNCTION

REPORT rep_sdo(r_viv) ----jerry
   DEFINE r_viv  RECORD
      nss  CHAR(11),
      sub  SMALLINT,
      part DECIMAL(22,6)
   END RECORD

   DEFINE vviv97 DECIMAL(22,6),
          vviv92 DECIMAL(22,6)

   OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
   FORMAT
      ON EVERY ROW

         IF r_viv.sub = 4 THEN
            LET vviv97 = r_viv.part
         ELSE
            LET vviv92 = r_viv.part
         END IF

      AFTER GROUP OF r_viv.nss

         IF vviv97 IS NULL THEN
            LET vviv97 = 0
         END IF
         IF vviv92 IS NULL THEN

            LET vviv92 = 0
         END IF

         INSERT INTO safre_tmp:tmp_saldo_viv VALUES
            (r_viv.nss,
             vviv97,
             vviv92)
         LET vviv97 = 0
         LET vviv92 = 0

END REPORT


##----------------------------------------------------------------------------
## Layout Detalle
##----------------------------------------------------------------------------
FUNCTION Layout_det()

   DEFINE vcurp    CHAR(18)
   DEFINE vnss     CHAR(11)
   DEFINE i        INTEGER
   DEFINE tot      INTEGER

   LET vdeta.saldo_viv97   = 0  --id_04
   LET vdeta.saldo_viv92   = 0  --id_05
   LET vdeta.interes_viv97 = 0  --id_06
   LET vdeta.interes_viv92 = 0  --id_07
   LET vdeta.part_viv97    = 0  --id_08
   LET vdeta.part_viv92    = 0  --id_09
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
   PREPARE exe_acelera_on2 FROM acelera_on      
   EXECUTE exe_acelera_on2                      

   DECLARE cursor_1 CURSOR FOR
   SELECT  nss,
           viv97,
           viv92
   FROM    safre_tmp:tmp_saldo_viv

   LET G_LISTA2 = g_param_dis.ruta_envio CLIPPED,"/DET_SDO"

   START REPORT rep_det TO G_LISTA2

   FOREACH cursor_1 INTO r_sdo.*

      LET vdeta.nss = r_sdo.nss

      LET vdeta.ind_proceso = " "

      LET vdeta.tipo_registro = "02"

      SELECT n_unico
      INTO   vdeta.curp
      FROM   safre_af:afi_mae_afiliado
      WHERE  n_seguro = r_sdo.nss

      IF vdeta.curp IS NULL THEN
         LET vdeta.curp = " "
      END IF

      LET vdeta.part_viv97  = r_sdo.viv97 * vfactor               --id_08
      LET vdeta.saldo_viv97 = vdeta.part_viv97 * vprecio_del_dia  --id_04
     
      LET vdeta.part_viv92  = r_sdo.viv92 * vfactor               --id_09
      LET vdeta.saldo_viv92 = vdeta.part_viv92 * vprecio_del_dia  --id_05
     
      LET vdeta.interes_viv97 = 0                                   --id_06
      LET vdeta.interes_viv92 = 0                                   --id_07

      LET pos1 = "0"
      LET pos2 = "0"
      LET pos3 = "0"
      LET pos4 = "0"

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

      LET vdeta.result_operacion = '  '
      LET vdeta.diag_proceso     = ' '
      LET vdeta.ind_proceso      = " "   ----jerry
      LET vdeta.filler           = '        '

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
   END RECORD

   DEFINE vsaldo_viv97 DECIMAL(24,0)
   DEFINE vsaldo_viv92 DECIMAL(24,0)
   DEFINE vinter_viv97 DECIMAL(24,0)
   DEFINE vinter_viv92 DECIMAL(24,0)
   DEFINE vparti_viv97 DECIMAL(24,0)
   DEFINE vparti_viv92 DECIMAL(24,0)

   OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
   FORMAT
      ON EVERY ROW
      -- PARA EL SUMARIO
         LET suma_viv97  = suma_viv97  + vdeta.saldo_viv97
         LET suma_viv92  = suma_viv92  + vdeta.saldo_viv92
         LET suma_int97  = suma_int97  + vdeta.interes_viv97
         LET suma_int92  = suma_int92  + vdeta.interes_viv92
         LET suma_part97 = suma_part97 + vdeta.part_viv97
         LET suma_part92 = suma_part92 + vdeta.part_viv92

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

   LET vdeta.saldo_viv97   = 0  --id_04
   LET vdeta.saldo_viv92   = 0  --id_05
   LET vdeta.interes_viv97 = 0  --id_06
   LET vdeta.interes_viv92 = 0  --id_07
   LET vdeta.part_viv97    = 0  --id_08
   LET vdeta.part_viv92    = 0  --id_09

END REPORT

##----------------------------------------------------------------------------
## Layout Sumario
##----------------------------------------------------------------------------
FUNCTION Layout_sum()

   LET G_LISTA3 = g_param_dis.ruta_envio CLIPPED,"/SUM_SDO"

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
--   LET v_sum.tot_part_viv97     = v_sum.tot_part_viv97  * 1000000
--   LET v_sum.tot_part_viv92     = v_sum.tot_part_viv92  * 1000000


   START REPORT rep_sum TO G_LISTA3
      OUTPUT TO REPORT rep_sum(v_sum.*)
   FINISH REPORT rep_sum

   DISPLAY  "SUMARIO GENERADO...."

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
   END RECORD

   DEFINE c18_part97 CHAR(18),
          c18_part92 CHAR(18),
          c19_part97 CHAR(19),
          c19_part92 CHAR(19)

   OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
   FORMAT
      ON EVERY ROW

         IF v_sum.tot_part_viv97 IS NULL OR
            v_sum.tot_part_viv97 = 0 THEN
            LET c18_part97 = "000000000000000000"
         ELSE
            LET c19_part97 = v_sum.tot_part_viv97 USING "&&&&&&&&&&&&.&&&&&&"
            LET c18_part97 = c19_part97[1,12],c19_part97[14,19]
         END IF

         IF v_sum.tot_part_viv92 IS NULL OR
            v_sum.tot_part_viv92 = 0 THEN
            LET c18_part92 = "000000000000000000"
         ELSE
            LET c19_part92 = v_sum.tot_part_viv92 USING "&&&&&&&&&&&&.&&&&&&"
            LET c18_part92 = c19_part92[1,12],c19_part92[14,19]
         END IF

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
                        c18_part97,
                        c18_part92,
--                        v_sum.tot_part_viv97    USING '&&&&&&&&&&&&&&&&&&',
--                        v_sum.tot_part_viv92    USING '&&&&&&&&&&&&&&&&&&',
                        v_sum.filler
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
