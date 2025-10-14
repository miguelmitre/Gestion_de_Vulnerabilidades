################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Owner             => E.F.P                                                    #
#Programa COMB011  => CONSOLIDA COMISIONES,BONOS RESPONSABLES                  #
#Fecha             => 06 mayo 1998.                                            #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha Actualiza   => 11 mayo 1998.                                            #
#By                =>                                                          #
#Sistema           => COM.                                                     #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE
      g_comr ARRAY[1000] OF RECORD 
         cod_resp_uni  LIKE com_comis_respon.cod_resp_uni,
         coduni        LIKE com_comis_respon.coduni,
         nivel         LIKE com_comis_respon.nivel,
         fecha_desde   LIKE com_comis_respon.fecha_desde,
         fecha_hasta   LIKE com_comis_respon.fecha_hasta,
         sum_comision  LIKE com_comis_respon.total_comision
      END RECORD,
      
      g_param_com RECORD LIKE com_parametro.*,
 
      vfecha_corte DATE,
      vfecha_desde DATE,
      vfecha_hasta DATE,

      gusuario CHAR(08),
      hoy      DATE,
      opc      CHAR(01),
      cla_sel  CHAR(300),
      i        INTEGER,

      vingreso_acum       DECIMAL(16,2),
      vtotal_comision_fun DECIMAL(16,2),
      vcomision_resp      DECIMAL(16,2),
      vbono_resp          DECIMAL(16,2),
      vbono_prom          DECIMAL(16,2),
      vimpto_cargo        DECIMAL(16,2),
      vtotal_ingreso      DECIMAL(16,2),
      vmonto_iva          DECIMAL(16,2),
      vmonto_iva2         DECIMAL(16,2),

      vcod_tipo_prom LIKE com_tipo_promotor.cod_tipo_prom,
      vcod_puesto    LIKE tab_puesto.cod_puesto,
      vfiscal_cod    LIKE com_tipo_promotor.indicador_ispt

END GLOBALS

MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O

   DEFER INTERRUPT

   SELECT *,USER INTO g_param_com.*,gusuario FROM com_parametro
 
   LET hoy = TODAY
   
   OPEN WINDOW ventana1 AT 3,2 WITH FORM "COMB0111" ATTRIBUTE(BORDER)
      DISPLAY " COMB012   CONSOLIDACION COMISIONES Y BONOS RESPONSABLES                      " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY hoy USING "dd-mm-yyyy" AT 1,63 ATTRIBUTE(REVERSE)
      DISPLAY "  < E S C > p/procesar                           < CTRL-C > p/cancelar                   " AT 3,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME vfecha_desde,vfecha_hasta
      AFTER FIELD vfecha_desde
         IF vfecha_desde IS NULL THEN
            ERROR "Fecha de desde NO puede ser NULA" 
            NEXT FIELD vfecha_desde
         END IF
      AFTER FIELD vfecha_hasta
         IF vfecha_hasta IS NULL THEN
            ERROR "Fecha de hasta NO puede ser NULA" 
            NEXT FIELD vfecha_hasta
         END IF
      ON KEY (ESC)
         IF vfecha_desde IS NULL THEN
            ERROR "Fecha de desde NO puede ser NULA" 
            NEXT FIELD vfecha_desde
         END IF
         IF vfecha_hasta IS NULL THEN
            ERROR "Fecha de hasta NO puede ser NULA" 
            NEXT FIELD vfecha_hasta
         END IF
         PROMPT "Deseas emitir calculo [S/N]..." FOR opc
         IF opc MATCHES "[Ss]" THEN
            ERROR "Procesando Informacion"
            CALL inicializa()
            CALL proceso_principal()
            ERROR "Proceso concluido"
            PROMPT "Oprima [ENTER] p/terminar..." FOR opc
         ELSE
            ERROR "Proceso Cancelado"
            SLEEP 2	
         END IF
         EXIT INPUT
      ON KEY (INTERRUPT)
         ERROR "Proceso Cancelado"
         SLEEP 2	
         EXIT INPUT
   END INPUT
END MAIN

FUNCTION inicializa()
   LET vtotal_comision_fun = 0
   LET vcomision_resp = 0
   LET vbono_resp = 0
   LET vbono_prom = 0
   LET vingreso_acum = 0
END FUNCTION

FUNCTION proceso_principal()
   LET cla_sel = "SELECT ",
                 "cod_resp_uni,",
                 "coduni,",
                 "nivel,",
                 "MIN(fecha_desde),",
                 "MAX(fecha_hasta),",
                 "SUM(total_comision) ",
                 "FROM com_comis_respon ",
---                 "WHERE fecha_corte <= ","'",vfecha_corte,"'",
                 "WHERE fecha_desde >= ","'",vfecha_desde,"'",
                 " AND  fecha_hasta <= ","'",vfecha_hasta,"'",
                 " AND consolida = 0 ",
                 " AND (codven is null OR codven =' ') ", 
                 "GROUP BY 1,2,3 ",
                 "ORDER BY 1,2,3" CLIPPED

   PREPARE claexe3 FROM cla_sel
   DECLARE cursor3 CURSOR FOR claexe3
   OPEN cursor3
   FETCH cursor3
   IF STATUS=100 THEN
      CLOSE cursor3
      ERROR "No existen comisiones de responsbles por consolidar"
      SLEEP 2
      RETURN 
   END IF
   CLOSE cursor3
   LET i = 1
   FOREACH cursor3 INTO g_comr[i].*
      DISPLAY "Registros procesados...", i USING "#####" AT 12,18

      CALL consolida_respon()
      INSERT INTO com_ingres_respon 
         VALUES(
            g_comr[i].cod_resp_uni,	# cod_resp_uni
            g_comr[i].coduni,           # coduni_n1
            g_comr[i].nivel,            # nivel
            vcod_puesto,                # cod_puesto
            g_comr[i].fecha_desde,      # fecha_desde
            g_comr[i].fecha_hasta,      # fecha_hasta
            vfecha_corte, 		# fecha_corte
            g_comr[i].sum_comision,     # total_comision_fun
            0,                          # total_otras_percep
            vbono_resp,                 # total_bono
            0,                          # total_premio
            vfiscal_cod,		# cod_tabla_impto
            vmonto_iva,			# monto_iva
            vimpto_cargo,		# monto_impto
            vtotal_ingreso,     	# total_ingreso
            TODAY,			# fecha_calculo
	    gusuario)			# usuario 

      UPDATE com_comis_respon
         SET consolida = 1
       WHERE consolida = 0
--         AND fecha_corte <= vfecha_corte
         AND fecha_desde >= vfecha_desde
         AND fecha_hasta <= vfecha_hasta
         AND (codven IS NULL OR codven=" ")
         AND cod_resp_uni = g_comr[i].cod_resp_uni

      UPDATE com_bono_respon
         SET consolida = 1
       WHERE consolida = 0
--         AND fecha_corte <= vfecha_corte
         AND fecha_desde >= vfecha_desde
         AND fecha_hasta <= vfecha_hasta
         AND (codven IS NULL OR codven=" ")
         AND cod_resp_uni = g_comr[i].cod_resp_uni

      LET i = i + 1
   END FOREACH
{
   FOR i = 1 to 1000
      DISPLAY g_comr[i].*
      IF g_comr[i].cod_resp_uni IS NULL THEN
         EXIT FOR
      END IF
   END FOR
}

END FUNCTION

FUNCTION consolida_respon()
   LET vingreso_acum     = 0

   ### BONO RESPONSABLE
   SELECT sum(total_bono)
     INTO vbono_resp
     FROM com_bono_respon
    WHERE cod_resp_uni = g_comr[i].cod_resp_uni
--      AND fecha_corte <= vfecha_corte
      AND fecha_desde >= vfecha_desde
      AND fecha_hasta <= vfecha_hasta
      AND consolida = 0
      AND (codven IS NULL OR codven=" ")
      IF vbono_resp IS NULL THEN
         LET vbono_resp =0
      END IF

      let vtotal_comision_fun = g_comr[i].sum_comision + vbono_resp

   ### TIPO PROMOTOR Y CATEGORIA FISCAL
   SELECT cod_puesto,cod_esq_ispt
     INTO vcod_puesto,vfiscal_cod
     FROM tab_puesto,com_respon_unidad
    WHERE cod_resp_uni = g_comr[i].cod_resp_uni
      AND cod_puesto = puesto_resp

   LET vingreso_acum = vtotal_comision_fun

   CALL calculo_impuesto()
      
END FUNCTION

FUNCTION calculo_impuesto()
   DEFINE   
      vlimite_inferior  DECIMAL(16,2),
      vexceden_lf       DECIMAL(16,2),
      vporcen_exced_li  DECIMAL(05,2),
      vimpto_marginal   DECIMAL(16,2),
      vcuota_fija       DECIMAL(16,2),
      vporcentaje_iva   DECIMAL(05,2),
      vporcentaje_iva2  DECIMAL(05,2),
      vdesc_esq_ispt    CHAR(50)

   LET vlimite_inferior  = 0
   LET vexceden_lf       = 0
   LET vporcen_exced_li  = 0
   LET vimpto_marginal   = 0
   LET vcuota_fija       = 0
   LET vimpto_cargo      = 0

   SELECT porcentaje_iva,desc_esq_ispt
     INTO vporcentaje_iva,vdesc_esq_ispt
     FROM com_esq_ispt
    WHERE cod_esq_ispt = vfiscal_cod
    IF vporcentaje_iva IS NULL THEN
       LET vporcentaje_iva = 0
    END IF

   ###  TARIFA MENSUAL  ###
   SELECT limite_inf,porcentaje,cuota_fija
     INTO vlimite_inferior,vporcen_exced_li,vcuota_fija
     FROM com_cuadro_ispt
    WHERE cod_esq_ispt = vfiscal_cod
      AND limite_inf <= vingreso_acum
      AND limite_sup >= vingreso_acum

   LET vmonto_iva = vingreso_acum * vporcentaje_iva / 100 

   IF vdesc_esq_ispt MATCHES '[HON*]' THEN
      LET vporcentaje_iva2 = (vporcentaje_iva / 3) * 2
      LET vmonto_iva2 = vingreso_acum * vporcentaje_iva2 / 100
   ELSE
      LET vmonto_iva2 = 0
   END IF

   LET vexceden_lf = vingreso_acum - vlimite_inferior

   LET vimpto_marginal = vexceden_lf * (vporcen_exced_li / 100)

      # ISR o RETENCION
   LET vimpto_cargo = vimpto_marginal + vcuota_fija

   LET vtotal_ingreso = vingreso_acum - vimpto_cargo - vmonto_iva2 + vmonto_iva

END FUNCTION

