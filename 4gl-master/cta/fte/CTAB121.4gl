#############################################################################
#Proyecto            => SAFRE ( MEXICO )                                    #
#Propietario         => E.F.P.                                              #
#Programa            => CTAB121                                             #
#Descripcion         => GENERA REPORTE DE REGISTROS SALDO CERO              #
#Por                 => OMAR SABNDOVAL BADILLO                              #
#Fecha               => 10 de noviembre de 2006                             #
#Sistema             => CTA.                                                #
#############################################################################
DATABASE safre_af
GLOBALS

   DEFINE g_parametro RECORD LIKE seg_modulo.*

   DEFINE hoy         DATE,
          opc         CHAR(1),
          g_usuario   CHAR(8),
          sp_01       CHAR(50)

   DEFINE fecha_inicio_sc DATE,
          p_fecha_corte   DATE,
          p_fecha_lote    DATE

   DEFINE l_reg_01 RECORD
          nss              CHAR(11),
          tipo_solicitud   SMALLINT,
          curp             CHAR(18),
          fentcons         DATE
   END RECORD   

   DEFINE l_reg_02 RECORD
          nss              CHAR(11),
          tipo_solicitud   SMALLINT,
          curp             CHAR(18),
          fentcons         DATE
   END RECORD   

   DEFINE cve_ent_origen CHAR(3)

END GLOBALS
#############################################################################
MAIN

   SELECT *,
          USER
   INTO   g_parametro.*,
          g_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   SELECT codigo_afore
   INTO   cve_ent_origen
   FROM   tab_afore_local

   LET hoy = TODAY

   LET p_fecha_corte= ARG_VAL(1)
   LET p_fecha_lote = ARG_VAL(2)
   
   CALL genera_archivo()
   CALL Actualiza_etapa(4)   --Termina proceso cuentas '01' saldo cero

END MAIN

#############################################################################
FUNCTION genera_archivo()

   DEFINE v_arch_saldo_cero     CHAR(100),
          genera_encabezado     CHAR(200),
          genera_detalle        CHAR(200),
          genera_sumario        CHAR(200),
          permisos              CHAR(200)

   DEFINE l_reg_cta  RECORD LIKE cta_saldo_cero.*

   DEFINE pos_afiliado          INTEGER,
          pos_no_afiliado       INTEGER,
          pos_asignado          INTEGER

   DEFINE query_01              CHAR(200)
    
   LET v_arch_saldo_cero = g_parametro.ruta_envio CLIPPED,"/",
                           g_usuario CLIPPED,".RSC.",TODAY USING "DDMMYYYY"

   START REPORT r_report TO v_arch_saldo_cero

   LET genera_encabezado = cve_ent_origen,"|",
                           p_fecha_lote USING "YYYYMMDD","|"

   OUTPUT TO REPORT r_report(genera_encabezado,335,1)

   LET query_01 = " SELECT *",
                  " FROM   cta_saldo_cero",
                  " WHERE  fecha_corte = '",p_fecha_corte,"'",
                  " ORDER BY nss "

   PREPARE Q1 FROM query_01
   DECLARE cur_sc CURSOR FOR Q1

   LET pos_afiliado    = 0
   LET pos_no_afiliado = 0
   LET pos_asignado    = 0

   FOREACH cur_sc INTO l_reg_cta.*

      IF l_reg_cta.tipo_trabajador = "01" THEN
         LET pos_afiliado = pos_afiliado + 1
      ELSE
         IF l_reg_cta.tipo_trabajador = "02" THEN
            LET pos_no_afiliado = pos_no_afiliado + 1
         ELSE
            LET pos_asignado = pos_asignado + 1          ---nvo por layout
         END IF
      END IF

      LET l_reg_cta.diagnostico = "  "
      LET genera_detalle = l_reg_cta.nss_rep,"|",
                           l_reg_cta.curp,"|",
                           l_reg_cta.fecha_operacion USING "YYYYMMDD","|",
                           l_reg_cta.id_cuenta,"|",
                           l_reg_cta.tipo_trabajador,"|",
                           #l_reg_cta.operacion,"|", -- modificacion
                           l_reg_cta.diagnostico,"|"

      OUTPUT TO REPORT r_report(genera_detalle,335,2)
   END FOREACH

   LET genera_sumario = cve_ent_origen,"|",
                        p_fecha_lote USING "YYYYMMDD","|",
                        pos_afiliado,"|", -- modificacion
                        pos_no_afiliado,"|",
                        pos_asignado,"|"

   OUTPUT TO REPORT r_report(genera_sumario,335,3)
   FINISH REPORT r_report

   LET permisos = "chmod 777 ",v_arch_saldo_cero CLIPPED
   RUN permisos

END FUNCTION
#############################################################################
FUNCTION Actualiza_etapa(vetapa_cod)

   DEFINE vetapa_cod   SMALLINT

   DEFINE fecha_inicial      DATETIME YEAR TO SECOND,
          fecha_finaliza     DATETIME YEAR TO SECOND

   DEFINE cla_sel            CHAR(400)

   LET fecha_finaliza = CURRENT

   UPDATE cta_ctr_saldo_cero
      SET fecha_fin = fecha_finaliza
    WHERE fecha_corte = p_fecha_corte
      AND etapa = vetapa_cod

END FUNCTION

