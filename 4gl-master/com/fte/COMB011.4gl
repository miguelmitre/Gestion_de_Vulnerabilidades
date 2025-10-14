################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Owner             => E.F.P                                                    #
#Programa COMB011  => CONSOLIDA COMISIONES,BONOS PROMOTORES                    #
#Fecha             => 06 mayo 1998.                                            #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha Actualiza   => 11 mayo 1998.                                            #
#By                =>                                                          #
#Sistema           => COM.                                                     #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE
      g_comp ARRAY[1000] OF RECORD 
         codven        LIKE com_comis_resumen.codven,
         fecha_desde   LIKE com_comis_resumen.fecha_desde,
         fecha_hasta   LIKE com_comis_resumen.fecha_hasta,
         sum_comision  LIKE com_comis_resumen.total_comision,
         sum_premio    LIKE com_comis_resumen.comis_calculada,
         sum_percep    LIKE com_comis_resumen.monto_percep
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
      DISPLAY " COMB011     CONSOLIDACION COMISIONES Y BONOS PROMOTORES                     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY hoy USING "dd-mm-yyyy" AT 1,63 ATTRIBUTE(REVERSE)
      DISPLAY "  < E S C > p/procesar                           < CTRL-C > p/cancelar                   " AT 3,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME vfecha_desde,vfecha_hasta
      AFTER FIELD vfecha_desde
         IF vfecha_desde IS NULL THEN
            ERROR "Fecha NO puede ser NULA" 
            NEXT FIELD vfecha_desde
         END IF
      AFTER FIELD vfecha_hasta
         IF vfecha_hasta IS NULL THEN
            ERROR "Fecha NO puede ser NULA" 
            NEXT FIELD vfecha_hasta
         END IF
      ON KEY (ESC)
         IF vfecha_desde IS NULL THEN
            ERROR "Fecha NO puede ser NULA" 
            NEXT FIELD vfecha_desde
         END IF
         IF vfecha_hasta IS NULL THEN
            ERROR "Fecha NO puede ser NULA" 
            NEXT FIELD vfecha_hasta
         END IF

         PROMPT "Deseas emitir calculo [S/N]..." FOR opc
         IF opc MATCHES "[Ss]" THEN
            ERROR "Procesando Informacion"
            CALL inicializa()
            CALL proceso_principal()
            ERROR "Proceso concluido"
            PROMPT "Oprima [ENTER] p/terminar" FOR opc
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
                 "codven,",
                 "MIN(fecha_desde),",
                 "MAX(fecha_hasta),",
                 "SUM(total_comision),",
                 "SUM(comis_calculada),",
                 "SUM(monto_percep) ",
                 "FROM com_comis_resumen ",
                 ----"WHERE fecha_corte <= ","'",vfecha_corte,"'",
                 "WHERE fecha_desde >= ","'",vfecha_desde,"'", 
                 " AND fecha_hasta <= ","'",vfecha_hasta,"'",
                 " AND consolida = 0 ",
                 "GROUP BY 1 ",
                 "ORDER BY 1" CLIPPED

   PREPARE claexe1 FROM cla_sel
   DECLARE cursor1 CURSOR FOR claexe1
   OPEN cursor1
   FETCH cursor1
   IF STATUS=100 THEN
      CLOSE cursor1
      ERROR "No existen comisiones de promotores por consolidar"
      SLEEP 2
      RETURN 
   END IF
   CLOSE cursor1
   LET i = 1
   FOREACH cursor1 INTO g_comp[i].*
      DISPLAY "Registros procesados ... ", i USING "#####" AT 12,18

      CALL consolida_promotor()
      INSERT INTO com_ingres_resum 
         VALUES(
            g_comp[i].codven,		# codven
            vcod_tipo_prom,		# cod_tipo_prom
            "",				# coduni_n1
            g_comp[i].fecha_desde,      # fecha_desde
            g_comp[i].fecha_hasta,      # fecha_hasta
            vfecha_corte, 		# fecha_corte
            g_comp[i].sum_comision,     # total_comision_pro
            vtotal_comision_fun,        # total_comision_fun
            g_comp[i].sum_percep,       # total_otras_percep
            vbono_prom,                 # total_bono
            g_comp[i].sum_premio,       # comis_calculada
            vfiscal_cod,		# cod_tabla_impto
            vmonto_iva,			# monto_iva
            vimpto_cargo,		# monto_impto
            vtotal_ingreso,     	# total_ingreso
            TODAY,			# fecha_calculo
	    gusuario)			# usuario 

      UPDATE com_comis_resumen 
         SET consolida = 1
       WHERE consolida = 0
--         AND fecha_corte <= vfecha_corte
          AND fecha_desde >= vfecha_desde
          AND fecha_hasta <= vfecha_hasta
         AND codven = g_comp[i].codven

      UPDATE com_bono_resumen 
         SET consolida = 1
       WHERE consolida = 0
--         AND fecha_corte <= vfecha_corte
          AND fecha_desde >= vfecha_desde
          AND fecha_hasta <= vfecha_hasta
         AND codven = g_comp[i].codven

      UPDATE com_comis_respon
         SET consolida = 1
       WHERE consolida = 0
--         AND fecha_corte <= vfecha_corte
          AND fecha_desde >= vfecha_desde
          AND fecha_hasta <= vfecha_hasta
         AND codven = g_comp[i].codven

      UPDATE com_bono_respon
         SET consolida = 1
       WHERE consolida = 0
--         AND fecha_corte <= vfecha_corte
          AND fecha_desde >= vfecha_desde
          AND fecha_hasta <= vfecha_hasta
         AND codven = g_comp[i].codven

      LET i = i + 1
   END FOREACH

END FUNCTION

FUNCTION consolida_promotor()

   LET vingreso_acum = 0

   ### BONO PROMOTOR
   SELECT sum(total_bono)
     INTO vbono_prom
     FROM com_bono_resumen
    WHERE codven = g_comp[i].codven
--      AND fecha_corte <= vfecha_corte
      AND fecha_desde >= vfecha_desde
      AND fecha_hasta <= vfecha_hasta
      AND consolida = 0
      IF vbono_prom IS NULL THEN
         LET vbono_prom =0
      END IF

   ### COMISION RESPONSABLE
   SELECT sum(total_comision)
     INTO vcomision_resp 
     FROM com_comis_respon
    WHERE codven = g_comp[i].codven
--      AND fecha_corte <= vfecha_corte
      AND fecha_desde >= vfecha_desde
      AND fecha_hasta <= vfecha_hasta
      AND consolida = 0
      IF vcomision_resp IS NULL THEN
         LET vcomision_resp =0
      END IF

   ### BONO RESPONSABLE
   SELECT sum(total_bono)
     INTO vbono_resp 
     FROM com_bono_respon
    WHERE codven = g_comp[i].codven
--      AND fecha_corte <= vfecha_corte
      AND fecha_desde >= vfecha_desde
      AND fecha_hasta <= vfecha_hasta
      AND consolida = 0
      IF vbono_resp IS NULL THEN
         LET vbono_resp =0
      END IF
      let vtotal_comision_fun = vcomision_resp + vbono_resp

   ### TIPO PROMOTOR Y CATEGORIA FISCAL
   SELECT cod_tipo_prom,indicador_ispt
     INTO vcod_tipo_prom,vfiscal_cod
     FROM com_tipo_promotor,pro_mae_promotor
    WHERE codven = g_comp[i].codven
      AND cod_tipo_prom = nivel

   LET vingreso_acum = (g_comp[i].sum_comision + vtotal_comision_fun +
                       g_comp[i].sum_percep + vbono_prom +
                       g_comp[i].sum_premio)

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
      vporcentaje_iva2   DECIMAL(05,2),
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
