{===============================================================================
Proyecto       => AFORES ( MEXICO )
Propietario    => E.F.P.
Modulo         => RET
Programa       => RETL738
Descripcion    => ANEXO 135 Reporte de Retiros Parciales IMSS.
Fecha creacion => Enero, 2020
================================================================================}
DATABASE safre_af

GLOBALS

    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*
    DEFINE gr_fechas        RECORD
                            fecha_ini        DATE,
                            fecha_fin        DATE
                            END RECORD

    DEFINE
       HOY                      DATE

    DEFINE
        enter                   CHAR(1) ,
        gc_usuario              CHAR(20),
        lc_nombre               CHAR(45),
        mc_comando              CHAR(3000)
        
    DEFINE
        gs_codigo_afore         SMALLINT

    DEFINE  gc_ruta_det_01       CHAR(200),
            gc_ruta_det_03       CHAR(200),
            gc_ruta_det_04       CHAR(200),
            gc_ruta_det_05       CHAR(200),
            gc_ruta_det_06       CHAR(200),
            gc_ruta_det_07       CHAR(200)

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP              ,
        PROMPT LINE LAST        ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()  
    CALL init()     
    CALL f_captura_periodo()

END MAIN


#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()
DEFINE mc_comando_aux CHAR(800)
    -- -----------------------------------------------------------------------------

    -- Inicialización de variables globales
    LET HOY                 = TODAY
    LET gc_usuario          = f_lib_obten_user()

    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local
    GROUP BY 1

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"    

    LET lc_nombre = HOY USING "YYYYMMDD","_AF_",gs_codigo_afore USING "&&&","_007_00000.025.gpg"

    DISPLAY ""

END FUNCTION


#---------------------------------------------------------------------------#
# f_captura_periodo : Función que le permite al usuario capturar un rango   #
#                     de fechas que serviran como criterios de busqueda     #
#                     para los anexos 135                                   #
#---------------------------------------------------------------------------#
FUNCTION f_captura_periodo()

    DEFINE gr_fechas RECORD
        fecha_ini           DATE    , 
        fecha_fin           DATE
    END RECORD 

    DEFINE
        ls_bnd_captura              ,
        ls_bnd_fecha                SMALLINT

    DEFINE
        lc_cad_msg                  CHAR(75)

    -- -----------------------------------------------------------------------------

    -- Inicialización de varibales
    INITIALIZE gr_fechas.* TO NULL
    LET ls_bnd_captura = 0
    LET ls_bnd_fecha   = 0

    OPEN WINDOW w_RETL738 AT 2,2 WITH FORM "RETL7381"  ATTRIBUTE(BORDER)

    DISPLAY " Ctrl-C : Salir                                             Esc : Ejecutar " AT 1,2 ATTRIBUTE(REVERSE)
    DISPLAY " RETL738            GENERACION DEL ANEXO 135                    " AT 3,2 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
   
    IF (INT_FLAG = TRUE) THEN
        LET INT_FLAG = FALSE
    END IF
   
    INPUT BY NAME gr_fechas.* WITHOUT DEFAULTS

        AFTER FIELD fecha_ini
            CALL f_lib_valida_fechas(gr_fechas.fecha_ini) RETURNING ls_bnd_fecha, lc_cad_msg

            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_ini
            END IF

        AFTER FIELD fecha_fin
            CALL f_lib_valida_fechas(gr_fechas.fecha_fin) RETURNING ls_bnd_fecha, lc_cad_msg

            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_fin
            ELSE
                IF (gr_fechas.fecha_ini > gr_fechas.fecha_fin) THEN
                    LET gr_fechas.fecha_ini = NULL
                    CALL f_lib_error_msg("ERROR: FECHA INICIAL MAYOR A LA FINAL")
                    NEXT FIELD fecha_ini
                END IF
            END IF

        ON KEY (ESC)
            -- Valida la fechas
            CALL f_lib_valida_fechas(gr_fechas.fecha_ini) RETURNING ls_bnd_fecha, lc_cad_msg

            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_ini
            END IF

            CALL f_lib_valida_fechas(gr_fechas.fecha_fin) RETURNING ls_bnd_fecha, lc_cad_msg

            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_fin
            ELSE
                IF (gr_fechas.fecha_ini > gr_fechas.fecha_fin) THEN
                    LET gr_fechas.fecha_ini = NULL
                    CALL f_lib_error_msg("ERROR: FECHA INICIAL MAYOR A LA FINAL")
                    NEXT FIELD fecha_ini
                END IF
            END IF

            LET ls_bnd_captura = 1  -- Las fechas son válidas

            -- Solicitar confirmación al usuario 
            WHILE TRUE
                PROMPT "¿DESEA GENERAR EL ANEXO? (S/N) : " ATTRIBUTE(REVERSE) FOR enter
                IF ( enter MATCHES "[sSnN]" ) THEN
                    IF ( enter MATCHES "[sS]" ) THEN
                        CALL f_genera_anexos(gr_fechas.*)
                        EXIT INPUT
                    ELSE
                        CALL f_lib_error_msg("PROCESO CANCELADO")
                        EXIT INPUT
                    END IF
                END IF
            END WHILE

            EXIT INPUT

        ON KEY (INTERRUPT, CONTROL-C)
            LET ls_bnd_captura = 0
            LET INT_FLAG = FALSE
            CALL f_lib_error_msg("PROCESO CANCELADO")
            EXIT INPUT

    END INPUT 

    CLOSE WINDOW w_RETL738

END FUNCTION


#---------------------------------------------------------------------------#
# f_genera_anexos :                                                         #
#---------------------------------------------------------------------------#
FUNCTION f_genera_anexos(pr_fechas)

   DEFINE pr_fechas RECORD 
      fecha_ini DATE,
      fecha_fin DATE
   END RECORD 

   LET gc_ruta_det_01 = gr_seg_modulo.ruta_envio CLIPPED, "/DET_01_ANEXO135"
   LET gc_ruta_det_03 = gr_seg_modulo.ruta_envio CLIPPED, "/DET_03_ANEXO135"
   LET gc_ruta_det_04 = gr_seg_modulo.ruta_envio CLIPPED, "/DET_04_ANEXO135"
   LET gc_ruta_det_05 = gr_seg_modulo.ruta_envio CLIPPED, "/DET_05_ANEXO135"
   LET gc_ruta_det_06 = gr_seg_modulo.ruta_envio CLIPPED, "/DET_06_ANEXO135"
   LET gc_ruta_det_07 = gr_seg_modulo.ruta_envio CLIPPED, "/DET_07_ANEXO135"

   -- CALL f_tablas_tmp() 
   DATABASE safre_af     
   CALL f_genera_tmp_dis_cuenta(pr_fechas.*)
   CALL fn_genera_table_temp_aux(pr_fechas.*)
   CALL f_genera_reportes(pr_fechas.*)
   
END FUNCTION 


FUNCTION f_genera_tmp_dis_cuenta(pr_fechas_dc)

   DEFINE pr_fechas_dc RECORD
      fecha_ini DATE,
      fecha_fin DATE
   END RECORD
   
   DISPLAY "                                                           " AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY " OBTENIENDO DATOS DE REPORTES DE DETALLE  ...              " AT 18,1 ATTRIBUTE(REVERSE)
   SLEEP 1

   WHENEVER ERROR CONTINUE
   DROP TABLE tmp_dis_cta_nxo135

   SELECT *
   FROM   dis_cuenta 
   WHERE  1 = 2
   INTO TEMP tmp_dis_cta_nxo135

   DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY " VALIDANDO REGISTROS...                                     " AT 18,1 ATTRIBUTE(REVERSE)
  
   INSERT INTO tmp_dis_cta_nxo135
   SELECT *
   FROM   dis_cuenta
   WHERE  fecha_conversion BETWEEN pr_fechas_dc.fecha_ini AND pr_fechas_dc.fecha_fin
   AND  tipo_movimiento IN (875,876,877,83,870)
                                                                                                                                                                       
   IF SQLCA.SQLCODE != 0 THEN  
      DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE)
      DISPLAY " ERROR AL INSERTAR LA INFORMACION VER LOG DE ERRORES        " AT 18,1 ATTRIBUTE(REVERSE)
      SLEEP 1
      CALL ERRORLOG(SQLCA.SQLCODE||"ERROR AL INSERTAR EN LA TABLA TEMPORAL ")
      EXIT PROGRAM         
   ELSE
      CREATE INDEX xie1_tmp_dis_cta_nxo135 ON tmp_dis_cta_nxo135(folio,nss,consecutivo_lote)
      CREATE INDEX xie2_tmp_dis_cta_nxo135 ON tmp_dis_cta_nxo135(nss,consecutivo_lote,folio)
      CREATE INDEX xie3_tmp_dis_cta_nxo135 ON tmp_dis_cta_nxo135(nss,fecha_conversion)
      UPDATE STATISTICS FOR TABLE tmp_dis_cta_nxo135
   END IF

   WHENEVER ERROR STOP

END FUNCTION 


FUNCTION fn_genera_table_temp_aux(pr_fecha_ini, pr_fecha_fin )

    DEFINE  pr_fecha_ini,
            pr_fecha_fin DATE

    DEFINE v_tabla_mov      CHAR(40)
    DEFINE v_query          CHAR(3000)

    WHENEVER ERROR CONTINUE
       DROP TABLE tmp_det_03
       DROP TABLE tmp_det_04
    WHENEVER ERROR STOP

    SELECT UNIQUE rps.nss,
           rps.curp,
           rps.consecutivo,
           rps.ind_suspende,
           0 tipo_movimiento
    FROM   ret_parcial_sub rps, 
           ret_parcial_op70 rpo
    WHERE  rps.nss = rpo.nss
    AND    rps.consecutivo = rpo.consecutivo
    AND    rps.estado = 122
    AND    DATE(rpo.fecha_carga) BETWEEN pr_fecha_ini AND pr_fecha_fin
    AND    rpo.folio_op70 NOT IN (SELECT UNIQUE folio
                                  FROM ret_par_reintegro_des)
    UNION
    SELECT UNIQUE nss,
           curp,
           consecutivo_lote,
           1,
           tipo_movimiento
    FROM dis_cuenta
    WHERE fecha_conversion BETWEEN pr_fecha_ini AND pr_fecha_fin
    AND tipo_movimiento = 1343
    INTO TEMP tmp_det_03

    CREATE INDEX xie1_tmp_det_03 ON tmp_det_03(nss,consecutivo)

    SELECT UNIQUE
           dc.nss,
           dv.curp,
           dc.consecutivo_lote,
           dc.folio,
           dv.fecha,
           dv.monto_neto,
           dv.fecha_liquida,
           rmd.num_resolucion,
           rmd.semanas_reintegro,
           rmd.fecha_retiro
    FROM   tmp_dis_cta_nxo135 dc,
           int_det_voluntaria dv,
           ret_mto_devol rmd
    WHERE  dv.folio           = dc.folio 
      AND  dv.nss             = dc.nss
      AND  rmd.nss            = dv.nss
      AND  rmd.mto_reintegro  = dv.monto_neto
      AND  dc.tipo_movimiento = 83 
      AND  dv.estado          = 2
      AND  dv.tipo_movimiento = 'A'
      AND  dv.tipo_aportacion = 'R'
      AND  dv.resul_operacion = '01'
    ORDER  BY dc.nss
    INTO TEMP tmp_det_04

    CREATE INDEX xie1_tmp_det_04 ON tmp_det_04(nss,consecutivo_lote,folio)
    -- Para la historia del retiro (en caso de existir en la afore)

    SELECT *
    FROM   dis_cuenta 
    WHERE  1 = 2
    INTO TEMP tmp_ret_par_reintegro_135

    DECLARE cur_tablas_movimientos CURSOR FOR
    SELECT tabname
    FROM   systables
    WHERE  tabname    = 'dis_cuenta'
       OR  tabname LIKE 'dis_cuenta__'
    ORDER  BY tabname

    FOREACH cur_tablas_movimientos INTO v_tabla_mov

        LET v_query = "\n INSERT INTO tmp_ret_par_reintegro_135                        ",
                      "\n SELECT *                                                     ",
                      "\n FROM   ",v_tabla_mov CLIPPED," dc                            ",
                      "\n WHERE  dc.nss IN (SELECT UNIQUE aux.nss FROM tmp_det_04 aux) ",
                      "\n   AND  dc.tipo_movimiento IN (875,876,877,83,870)            "
        LET v_query = v_query CLIPPED
        PREPARE prp_movimientos_anexo_133 FROM v_query
        EXECUTE prp_movimientos_anexo_133

    END FOREACH -- cur_tablas_movimientos

    CREATE INDEX xie1_tmp_ret_par_reintegro_135 ON tmp_ret_par_reintegro_135(folio,nss,consecutivo_lote)
    CREATE INDEX xie2_tmp_ret_par_reintegro_135 ON tmp_ret_par_reintegro_135(nss,consecutivo_lote,folio)
    CREATE INDEX xie3_tmp_ret_par_reintegro_135 ON tmp_ret_par_reintegro_135(nss,fecha_conversion)
            
    UPDATE STATISTICS FOR TABLE tmp_det_03
    UPDATE STATISTICS FOR TABLE tmp_det_04
    UPDATE STATISTICS FOR TABLE tmp_ret_par_reintegro_135

END FUNCTION 

 
-------------------------------------------------------------------------------
-- f_genera_reportes : Ejecuta las instrucciones generar reporte 135           -- 
-------------------------------------------------------------------------------
FUNCTION f_genera_reportes(pr_fechas) 

   DEFINE pr_fechas RECORD 
      fecha_ini DATE,
      fecha_fin DATE
   END RECORD

   CALL fn_inicializa_consulta_01()
   CALL fn_inicializa_consulta_03()
   CALL fn_inicializa_consulta_04()
   CALL fn_inicializa_consulta_05()
   CALL fn_inicializa_consulta_06()
   CALL fn_inicializa_consulta_07()

   DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE) 
   DISPLAY " GENERANDO DETALLE 01  ...       " AT 18,1 ATTRIBUTE(REVERSE)
   SLEEP 1
   CALL fn_genera_detalle_01()
   DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY " GENERANDO DETALLE 03...       " AT 18,1 ATTRIBUTE(REVERSE)
   SLEEP 1
   CALL fn_genera_detalle_03(pr_fechas.*)
   DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY " GENERANDO DETALLE 04...       " AT 18,1 ATTRIBUTE(REVERSE)
   SLEEP 1
   CALL fn_genera_detalle_04()
   DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY " GENERANDO DETALLE 05...       " AT 18,1 ATTRIBUTE(REVERSE)
   SLEEP 1
   CALL fn_genera_detalle_05(pr_fechas.*)
   DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE) 
   DISPLAY " GENERANDO DETALLE 06...       " AT 18,1 ATTRIBUTE(REVERSE)
   SLEEP 1
   CALL fn_genera_detalle_06()
   DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE) 
   DISPLAY " GENERANDO DETALLE 07...       " AT 18,1 ATTRIBUTE(REVERSE)
   SLEEP 1
   CALL fn_genera_detalle_07(pr_fechas.*)
   DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE) 
   DISPLAY " GENERANDO ARCHIVO PLANO DE DETALLE...       " AT 18,1 ATTRIBUTE(REVERSE)
   SLEEP 1
   CALL fn_consolida_detalles()
   DISPLAY "ARCHIVO GENERADO : " AT 8,5
    
   DISPLAY "ANEXO 135" AT 9,5  
   DISPLAY gr_seg_modulo.ruta_envio    CLIPPED, "/",lc_nombre  AT 10,10
   DISPLAY "" AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY "" AT 19,1 ATTRIBUTE(REVERSE)
   CALL f_lib_error_msg("PROCESO FINALIZADO")
  
   SLEEP 1
   
END FUNCTION


FUNCTION fn_inicializa_consulta_01()
DEFINE lc_comando   CHAR (3000)

    LET lc_comando = "\n SELECT UNIQUE dc.nss,                             ",
                    "\n        dc.consecutivo_lote,                       ",
                    "\n        dc.folio,                                  ",
                    "\n        dc.fecha_conversion,                       ",
                    "\n        NVL(SUM(dc.monto_en_pesos),0)              ",
                    "\n FROM   tmp_dis_cta_nxo135 dc,                     ",
                    "\n        ret_parcial        rp                      ",
                    "\n WHERE  rp.folio             = dc.folio            ",
                    "\n   AND  rp.nss               = dc.nss              ",
                    "\n   AND  rp.consecutivo       = dc.consecutivo_lote ",
                    "\n   AND  dc.tipo_movimiento  IN (875,876,877)       ",
                    "\n   AND  rp.estado_solicitud IN (8,13,14)           ",
                    "\n   AND  rp.tipo_prestacion   = 6                   ",
                    "\n GROUP  BY 1,2,3,4                                 ",
                    "\n ORDER  BY dc.nss                                  "
    LET lc_comando = lc_comando CLIPPED
    PREPARE prp_det1 FROM lc_comando
    DECLARE cur_det1 CURSOR FOR prp_det1

     LET lc_comando =' ',
                    '\n SELECT UNIQUE  nss,    ',
                    '\n curp,            ',
                    '\n fecha_solicitud, ',
                    '\n fecha_envio  ,   ',
                    '\n fecha_resolucion, ',
                    '\n CASE             ',
                    '\n WHEN tipo_desempleo = "A" THEN 0  ',
                    '\n WHEN tipo_desempleo = "B" THEN 1  ',
                    '\n END     ,                         ',   
                    '\n CASE                              ',
                    '\n WHEN tipo_desempleo = "A" THEN 3 ',
                    '\n WHEN tipo_desempleo = "B" THEN 4 ',
                    '\n WHEN tipo_desempleo = "C" THEN 1 ',
                    '\n ELSE 2                     ',
                    '\n END        ,               ',   
                    '\n CASE                       ',
                    '\n WHEN tipo_desempleo = "A" THEN 1 ',
                    '\n WHEN tipo_desempleo = "B" THEN 2 ',
                    '\n END    ,                         ',
                    '\n tipo_pago ,                      ',
                    '\n num_resolucion    ',
                    '\n FROM ret_parcial         ',
	                  '\n WHERE nss = ?            ',
                    '\n AND consecutivo = ?      ',
                    '\n AND folio = ?            ',
                    '\n AND estado_solicitud IN (8,13,14) ',
                    '\n AND tipo_prestacion = 6  ',
                    '\n AND nss IS NOT NULL      ',
                    '\n AND  nss <> ""   '
    LET lc_comando = lc_comando CLIPPED
    PREPARE prp_par_1 FROM  lc_comando
    
    LET lc_comando=' ',
                '\n SELECT MAX(DATE(fecha_carga))             ',
                '\n FROM ret_op07_esp                         ',
                '\n WHERE nss = ?                             ',
                '\n AND estado IN (51,52)                     '
    LET lc_comando = lc_comando CLIPPED
    PREPARE prp_fech_carga FROM lc_comando
    
   --fecha_solicitud_cert_imss
    LET lc_comando=' ',
                '\n SELECT UNIQUE DATE(fecha_carga_afore)      ',
                '\n FROM ret_parcial_resol              ',
                '\n WHERE nss = ?                       ',
                '\n AND num_resolucion = ?              ',
                '\n AND diag_procesar IN ("400","106")  ',
                '\n AND fecha_carga_afore = (SELECT MAX(fecha_carga_afore) ',
                '\n                          FROM   ret_parcial_resol      ',
                '\n                          WHERE  nss = ?                ',
                '\n                          AND num_resolucion = ?        ',
                '\n                          AND diag_procesar IN ("400","106"))'
    LET lc_comando = lc_comando CLIPPED
    PREPARE prp_fech_af FROM lc_comando

    --Para cert_pago
    LET lc_comando=' ',
                '\n SELECT mto_pago, mto_1, mto_2          ',
                '\n FROM ret_ctr_pago                      ',
                '\n WHERE nss = ?                          ',
                '\n AND consecutivo = ?                    '
   LET lc_comando = lc_comando CLIPPED    
   PREPARE prp_mts FROM lc_comando

    --salarios base 
    LET lc_comando=' ',
                '\n SELECT UNIQUE salario_base_a, ',
                '\n               salario_base_b  ',
                '\n FROM ret_parcial_resol        ',
                '\n WHERE nss = ?                 ',
                '\n AND num_resolucion = ?        ',
                '\n AND diag_procesar IN ("400","106")',
                '\n AND fecha_carga_afore = (SELECT MAX(fecha_carga_afore) ',
                '\n                          FROM   ret_parcial_resol      ',
                '\n                          WHERE  nss = ?                ',
                '\n                          AND num_resolucion = ?        ',
                '\n                          AND diag_procesar IN ("400","106"))'
    LET lc_comando = lc_comando CLIPPED
    PREPARE prp_salarios FROM lc_comando

    --folio_op_imss 
    LET lc_comando = "\n SELECT rpo.folio_op_imss                                            ",
                     "\n FROM   ret_parcial_op66 rpo                                         ",
                     "\n WHERE  rpo.nss            = ?                                       ",
                     "\n   AND  rpo.num_resolucion = ?                                       ",
                     "\n   AND  rpo.id    = (SELECT MAX(aux.id)                              ",
                     "\n                     FROM   ret_parcial_op66 aux                     ",
                     "\n                     WHERE  aux.nss            = rpo.nss             ",
                     "\n                       AND  aux.num_resolucion = rpo.num_resolucion  ",
                     "\n                       AND  aux.diag_procesar  IN ('400','106'))     "
    LET lc_comando = lc_comando CLIPPED
	PREPARE prp_folio_op FROM lc_comando

    LET lc_comando = " ",
       "\n SELECT COUNT(*)",
       "\nFROM  ret_ws_notifica_app",
       "\nWHERE nss = ? ",
       "\nAND   consecutivo = ? ",
       "\nAND aplicacion_origen <> '07' "
    PREPARE prp_sol_app FROM lc_comando
    
    LET lc_comando = " ",
       "\n SELECT COUNT(*)",
       "\nFROM  ret_ws_notifica_app",
       "\nWHERE nss = ? ",
       "\nAND   consecutivo = ? ",
       "\nAND aplicacion_origen = '07' "
    PREPARE prp_sol_afo_web FROM lc_comando

    LET lc_comando = " ",
       "\n SELECT FIRST 1 DECODE(tipo_pago,1,'002',5,'001','003')",
       "\nFROM  ret_beneficiario",
       "\nWHERE nss = ? ",
       "\nAND   consecutivo = ? "
    PREPARE prp_medio_pago FROM lc_comando

    LET lc_comando = ' ',
	                '\n SELECT saldocuentaindividualantesretiro     ',
                    '\n FROM ret_bus_diag16                         ',
                    '\n WHERE nss = ? 					            ',
                    '\n AND folio = ?                               '
	PREPARE prp_saldo FROM lc_comando

	LET lc_comando = ' ',
	                '\n SELECT importepagadotrabajador     ',
                    '\n FROM ret_bus_diag16                         ',
                    '\n WHERE nss = ? 					            ',
                    '\n AND folio = ?                               '
	PREPARE prp_imp_pagado FROM lc_comando

END FUNCTION 


--------------------------------------------------------------------------------
-- fn_genera_detalle_01:Información de Retiros por Desempleo Autorizados con  --
--                      primer o único pago (Desinversión),                   --
--                      por fecha de liquidación.                             --
--------------------------------------------------------------------------------
FUNCTION fn_genera_detalle_01()

    DEFINE lr_ret_parcial_01    RECORD 
        nss				    CHAR(11),
        consecutivo         DECIMAL(11,0) ,
        folio               INTEGER ,
        fecha_conversion    DATE,
        tot_monto_p         DECIMAL(11,2)
    END RECORD 

    DEFINE v_mto_pago                DECIMAL(16,6),
           v_mto_1                   DECIMAL(16,6),
           v_mto_2                   DECIMAL(16,6),
           v_tipo_pago               SMALLINT,
           v_valida_app              SMALLINT,
           v_valida_afo_web          SMALLINT

   DEFINE v_dif_mto1                 DECIMAL(16,6),
          v_dif_mto2                 DECIMAL(16,6)

   DEFINE lr_reg_det01               RECORD  
      tipo_registro                 CHAR(2),         --01
      nss                           CHAR(11),        --02
      curp                          CHAR(18),        --03
      fecha_solicitud_retiro        DATE,            --04
      tipo_derecho                  DECIMAL(1,0),    --05
      fecha_solicitud_cert_afore    DATE,            --06
      fecha_solicitud_cert_imss     DATE,            --07
      fecha_desinvercion_rcv        DATE,            --08
      tipo_derecho_cert_imss        DECIMAL(1,0),    --09
      obs_tipo_derecho_cert_imss    DECIMAL(2,0),    --10
      cert_pago                     DECIMAL(2,0),    --11
      mto_total_rcv_ant_retiro      DECIMAL(11,2),   --12
      mto_total_desinvertido        DECIMAL(11,2),   --13
      fecha_liquidacion             DATE,            --14
      mto_pagado                    DECIMAL(11,2),   --15
      fecha_conc_vigencia           DATE,            --16
      num_semanas_cot_descontar     DECIMAL(3,0) ,   --17
      salario_base_tipo_a           DECIMAL(11,2),   --18
      salario_base_tipo_b           DECIMAL(11,2),   --19
      num_resolucion                CHAR(6) ,        --20
      folio_cert_imss               CHAR(22),        --21
      medio_solicitud               CHAR(3),         --22
      forma_pago                    CHAR(3)          --23
   END RECORD 

   INITIALIZE lr_reg_det01.* TO NULL
   INITIALIZE lr_ret_parcial_01.* TO NULL
              
   START REPORT  rpt_det_01 TO gc_ruta_det_01
   FOREACH cur_det1 INTO lr_ret_parcial_01.*
    	
        LET lr_reg_det01.tipo_registro = '01' --01
    	IF  lr_ret_parcial_01.nss IS NOT NULL THEN

    	EXECUTE prp_par_1 USING lr_ret_parcial_01.nss,
                                lr_ret_parcial_01.consecutivo,
    	                        lr_ret_parcial_01.folio
    	   INTO  lr_reg_det01.nss,                                --02
                 lr_reg_det01.curp,                               --03
                 lr_reg_det01.fecha_solicitud_retiro,             --04
                 lr_reg_det01.fecha_solicitud_cert_afore,         --06
                 lr_reg_det01.fecha_solicitud_cert_imss ,         --07
                 lr_reg_det01.tipo_derecho,                       --05
                 lr_reg_det01.tipo_derecho_cert_imss,             --09
                 lr_reg_det01.obs_tipo_derecho_cert_imss,         --10
                 v_tipo_pago,
                 lr_reg_det01.num_resolucion                      --20
    	
    	  LET lr_reg_det01.fecha_desinvercion_rcv = lr_ret_parcial_01.fecha_conversion   --08
    	
    	    CASE lr_reg_det01.tipo_derecho
    	    WHEN 0
    	       LET lr_reg_det01.cert_pago = 1                                           --11
    	       LET lr_reg_det01.fecha_conc_vigencia= lr_ret_parcial_01.fecha_conversion --16
    	       
    	    WHEN 1
               --[CPL-3396] Aproximación por diferencia de saldo para insuficiencia
               INITIALIZE v_mto_pago,v_mto_1,v_mto_2,v_dif_mto1,v_dif_mto2 TO NULL

    	       EXECUTE prp_mts USING  lr_reg_det01.nss,
    	                              lr_ret_parcial_01.consecutivo
    	                        INTO  v_mto_pago,
    	                              v_mto_1,
    	                              v_mto_2
                                      
               IF v_mto_pago IS NOT NULL THEN 
                   IF v_mto_pago = v_mto_1 THEN
                      LET lr_reg_det01.cert_pago = 3                                 --11
                   ELSE
                      IF v_mto_pago = v_mto_2 THEN
                         LET lr_reg_det01.cert_pago = 2                                 --11
                      ELSE
                         LET v_dif_mto1 = v_mto_pago - v_mto_1
                         LET v_dif_mto2 = v_mto_pago - v_mto_2
                         
                         IF v_dif_mto1 < 0 THEN
                            LET v_dif_mto1 = v_dif_mto1 * -1
                         END IF
                         IF v_dif_mto2 < 0 THEN
                            LET v_dif_mto2 = v_dif_mto2 * -1
                         END IF

                         IF v_dif_mto1 > v_dif_mto2 THEN
                            LET lr_reg_det01.cert_pago = 2                                 --11
                         ELSE
                            LET lr_reg_det01.cert_pago = 3                                 --11
                         END IF
                      END IF
                   END IF
               END IF   --//[CPL-3396]

               LET lr_reg_det01.fecha_conc_vigencia = lr_ret_parcial_01.fecha_conversion  --16
               
    	    END CASE
    	
    	    LET lr_reg_det01.mto_total_rcv_ant_retiro = 0                           --12
    	    
    	    EXECUTE prp_saldo USING lr_ret_parcial_01.nss,
    	                            lr_ret_parcial_01.folio 
    	                       INTO lr_reg_det01.mto_total_rcv_ant_retiro           --12
    	    
    	    LET lr_reg_det01.fecha_liquidacion = lr_ret_parcial_01.fecha_conversion --14
    	    
    	    EXECUTE prp_imp_pagado USING lr_ret_parcial_01.nss,
    	                            lr_ret_parcial_01.folio 
    	                       INTO lr_reg_det01.mto_total_desinvertido             --13

    	    LET lr_reg_det01.mto_pagado = lr_ret_parcial_01.tot_monto_p             --15
    	    LET lr_reg_det01.num_semanas_cot_descontar = 0                          --17

    	    EXECUTE prp_salarios USING lr_reg_det01.nss,
    	                               lr_reg_det01.num_resolucion,
    	                               lr_reg_det01.nss,
    	                               lr_reg_det01.num_resolucion
    	                         INTO  lr_reg_det01.salario_base_tipo_a,            --18
    	                               lr_reg_det01.salario_base_tipo_b             --19


    	    EXECUTE prp_folio_op USING lr_reg_det01.nss,
    	                               lr_reg_det01.num_resolucion
								 INTO   lr_reg_det01.folio_cert_imss --21

            LET lr_reg_det01.folio_cert_imss = "0000000000000000000000" --25
    	
    	  --Se valida si el retiro nace como app
          EXECUTE prp_sol_app USING lr_ret_parcial_01.nss,
                                    lr_ret_parcial_01.consecutivo
          INTO v_valida_app
          
          IF v_valida_app > 0 THEN
             LET lr_reg_det01.medio_solicitud = "002" -- APP
          ELSE
             --Se valida si el retiro nace como afore web
              EXECUTE prp_sol_afo_web USING lr_ret_parcial_01.nss,
                                        lr_ret_parcial_01.consecutivo
              INTO v_valida_afo_web
              
              IF v_valida_afo_web > 0 THEN
              	  LET lr_reg_det01.medio_solicitud = "003" -- PORTAL
              ELSE 
              	  LET lr_reg_det01.medio_solicitud = "001" -- OFICINA UEAP
              END IF
          END IF

          --Evalúa el tipo pago y su corresponduente valor
          EXECUTE prp_medio_pago USING lr_ret_parcial_01.nss,
                                       lr_ret_parcial_01.consecutivo
          INTO lr_reg_det01.forma_pago

          OUTPUT TO REPORT rpt_det_01(lr_reg_det01.*)

    	END IF 
    	INITIALIZE lr_ret_parcial_01.* TO NULL 
    	INITIALIZE lr_reg_det01.* TO NULL

    END FOREACH
    FINISH REPORT rpt_det_01
    
END FUNCTION


FUNCTION fn_inicializa_consulta_03()

   DEFINE lc_comando     CHAR(3000)
    
	--prp_fecha_reinversion
	LET lc_comando = ' ',
					'\n SELECT MAX(DATE(fecha_carga))    ',
					'\n FROM ret_parcial_op70            ',
					'\n WHERE nss = ?                    ',
					'\n AND consecutivo = ?              ',
					'\n AND (resultado_op = "01"         ',
					'\n OR (resultado_op = "02" AND diagnostico = "036") ',
					'\n OR (resultado_op = "02" AND diagnostico = "049")) '
	PREPARE prp_fecha_reinversion FROM lc_comando
    
	--prp_fecha_reinversion tipo movimiento 1343
	LET lc_comando = ' ',
					'\n SELECT UNIQUE MAX(fecha_conversion) ',
					'\n FROM dis_cuenta                     ',
					'\n WHERE nss = ?                       ',
					'\n AND consecutivo_lote = ?            ',
					'\n AND tipo_movimiento = ?             '
	PREPARE prp_fecha_reinversion_1343 FROM lc_comando

	--Fecha reintegro de samanas 
	LET lc_comando=' ', 
	    '\n SELECT MAX(DATE(fecha_carga))   ',
        '\n FROM ret_parcial_op69           ',
        '\n WHERE nss = ?                   ',
        '\n AND consecutivo = ?             '
	PREPARE prp_fecha_reint_sem FROM lc_comando
	
	LET lc_comando =  ' ', 
	    '\n SELECT MAX(num_sem_procesar)     ',
      '\n FROM ret_parcial_op70            ',
      '\n WHERE nss = ?                    ',
      '\n AND consecutivo = ?              ',
    	'\n AND (resultado_op = "01"         ',
			'\n OR (resultado_op = "02" AND diagnostico = "036") ',
			'\n OR (resultado_op = "02" AND diagnostico = "049")) '

	PREPARE prp_num_sem FROM lc_comando
	
	LET lc_comando = ' ', 
					'\n SELECT rp.num_resolucion  ',
					'\n FROM ret_parcial rp       ',
					'\n WHERE rp.nss = ?          ',
					'\n AND rp.consecutivo = ?    '
	PREPARE prp_num_resol  FROM lc_comando
	
END FUNCTION 


--------------------------------------------------------------------------------
--fn_genera_detalle_03:Retiros parciales por Desempleo No pagadas al          --
--                     trabajador por suspensión del derecho o termino        --
--                     de tiempo (Reinversión de las parcialidades que        --
--                     no se liquidaron en su totalidad al trabajador),       --
--                     por fecha de reintegro.                                --
--------------------------------------------------------------------------------
FUNCTION fn_genera_detalle_03(pr_fechas_03)

    DEFINE pr_fechas_03 RECORD 
       fecha_ini DATE,
       fecha_fin DATE 
    END RECORD  

    DEFINE  lr_ret_pracial_sub RECORD 
       nss                        CHAR(11),
       curp                       CHAR(18),
       consecutivo                DECIMAL(11,0),
       ind_suspende               INTEGER, 
       tipo_mov                   SMALLINT
    END RECORD   
    
    DEFINE lr_detalle_03                RECORD 
        tipo_registro                   CHAR(2),        --01
        nss                             CHAR(11),       --02
        curp                            CHAR(18),       --03
        tipo_reinversion                INTEGER,        --04
        fecha_reinversion               DATE,           --05
        fecha_reintegro_semanas         DATE,           --06
        det_tpo_reinversion             CHAR(30),       --07
        num_semanas_cot_reint           DECIMAL(3,0) ,  --08
        num_resolucion                  CHAR(6)         --09
    END RECORD

    DECLARE cur_detalle_03 CURSOR WITH HOLD  FOR 
       SELECT UNIQUE(rps.nss),
       rps.curp,
       rps.consecutivo,
       rps.ind_suspende,
       rps.tipo_movimiento
       FROM tmp_det_03 rps
       ORDER BY 1
       
       START REPORT rpt_det_03 TO gc_ruta_det_03
       FOREACH cur_detalle_03 INTO lr_ret_pracial_sub.*

        LET lr_detalle_03.tipo_registro = '03'                               --01
        
        LET lr_detalle_03.nss = lr_ret_pracial_sub.nss                       --02
        LET lr_detalle_03.curp = lr_ret_pracial_sub.curp                     --03
        LET lr_detalle_03.tipo_reinversion = lr_ret_pracial_sub.ind_suspende --04
        
        EXECUTE prp_fecha_reinversion USING lr_ret_pracial_sub.nss,
                                            lr_ret_pracial_sub.consecutivo
                                       INTO lr_detalle_03.fecha_reinversion       --05

        EXECUTE prp_fecha_reint_sem USING lr_ret_pracial_sub.nss,
                                          lr_ret_pracial_sub.consecutivo
                                    INTO  lr_detalle_03.fecha_reintegro_semanas  --06

       IF lr_ret_pracial_sub.tipo_mov = 1343 THEN
           EXECUTE prp_fecha_reinversion_1343 USING lr_ret_pracial_sub.nss,
                                                    lr_ret_pracial_sub.consecutivo,
                                                    lr_ret_pracial_sub.tipo_mov
                                               INTO lr_detalle_03.fecha_reinversion       --05
       END IF

       LET lr_detalle_03.det_tpo_reinversion = '                              ' --07
       LET lr_detalle_03.num_semanas_cot_reint = 0                              --08

       EXECUTE prp_num_resol USING lr_ret_pracial_sub.nss,
	                               lr_ret_pracial_sub.consecutivo
          INTO lr_detalle_03.num_resolucion                                     --09

        OUTPUT TO REPORT rpt_det_03(lr_detalle_03.*)
        INITIALIZE lr_detalle_03.* TO NULL

    END FOREACH 
    FINISH REPORT rpt_det_03
    FREE cur_detalle_03
                   
END FUNCTION 


FUNCTION fn_inicializa_consulta_04()

    DEFINE    lc_comando       CHAR(3000)

    LET lc_comando = "\n SELECT rp.fecha_solicitud,                                                         ",
                     "\n        rp.consecutivo,                                                             ",
                     "\n        rp.folio                                                                    ",
                     "\n FROM   ret_parcial rp                                                              ",
                     "\n WHERE  rp.nss               = ?                                                    ",
                     "\n   AND  rp.num_resolucion    = ?                                                    ",
                     "\n   AND  rp.consecutivo       = (SELECT MAX(aux.consecutivo)                         ",
                     "\n                                FROM   ret_parcial aux                              ",
                     "\n                                WHERE  aux.nss            = rp.nss                  ",
                     "\n                                AND  aux.num_resolucion   = rp.num_resolucion   )   "
    PREPARE prp_datos_retiro FROM lc_comando

    LET lc_comando = "\n SELECT SUM(monto_en_pesos)       ",
                     "\n FROM   tmp_ret_par_reintegro_135 ",
                     "\n WHERE  nss              = ?      ",
                     "\n   AND  consecutivo_lote = ?      "
    PREPARE prp_monto_retiro FROM lc_comando

END FUNCTION 


--------------------------------------------------------------------------------
--fn_genera_detalle_04: Reintegro de recursos solicitados y pagados a         --
--                      petición del trabajador, por fecha de reintegro.      --
-------------------------------------------------------------------------------- 
FUNCTION fn_genera_detalle_04()

    DEFINE lr_int_det_voluntaria_04 RECORD
        nss                 LIKE dis_cuenta.nss,
        curp                LIKE int_det_voluntaria.curp,
        consecutivo_lote    LIKE dis_cuenta.consecutivo_lote,
        folio               LIKE dis_cuenta.folio,
        fecha_reintegro     LIKE int_det_voluntaria.fecha,
        monto_neto          LIKE int_det_voluntaria.monto_neto,
        fecha_liquida       LIKE int_det_voluntaria.fecha_liquida,
        num_resolucion      LIKE ret_mto_devol.num_resolucion,
        semanas_reintegro   LIKE ret_mto_devol.semanas_reintegro,
        fecha_retiro        LIKE ret_mto_devol.fecha_retiro
    END RECORD 

    DEFINE v_consecutivo_ret    LIKE ret_parcial.consecutivo
    DEFINE v_folio_ret          LIKE ret_parcial.folio

    DEFINE lr_detalle_04               RECORD 
        tipo_registro                  CHAR(2),         --01         
        nss                            CHAR(11),        --02
        curp                           CHAR(18),        --03
        fecha_reintegro                DATE,            --04
        fecha_sol_ret_parcial          DATE,            --05
        num_resolucion_ret_parcial     CHAR(6),         --06
        mto_total_pagado               DECIMAL(11,2),   --07
        mto_total_reintegrar           DECIMAL(11,2),   --08
        num_semanas_cot_descontadas    DECIMAL(3,0) ,   --09
        num_semanas_cot_reint          DECIMAL(3,0) ,   --10
        fecha_reintegro_semanas        DATE             --11
    END RECORD

    DECLARE cur_detalle_04 CURSOR WITH HOLD FOR
    SELECT nss,
           curp,
           consecutivo_lote,
           folio,
           fecha,
           monto_neto,
           fecha_liquida,
           num_resolucion,
           semanas_reintegro,
           fecha_retiro
    FROM   tmp_det_04
    ORDER  BY nss
        
    START REPORT rpt_det_04 TO gc_ruta_det_04
    FOREACH cur_detalle_04 INTO lr_int_det_voluntaria_04.*

        LET lr_detalle_04.tipo_registro = '04'                                                     --01
        LET lr_detalle_04.nss = lr_int_det_voluntaria_04.nss                                       --02
        LET lr_detalle_04.curp = lr_int_det_voluntaria_04.curp                                     --03
        LET lr_detalle_04.fecha_reintegro = lr_int_det_voluntaria_04.fecha_reintegro               --04

        EXECUTE prp_datos_retiro USING lr_int_det_voluntaria_04.nss,
                                       lr_int_det_voluntaria_04.num_resolucion
                                 INTO  lr_detalle_04.fecha_sol_ret_parcial,                           --05
                                       v_consecutivo_ret,
                                       v_folio_ret
                                       
        LET lr_detalle_04.num_resolucion_ret_parcial  = lr_int_det_voluntaria_04.num_resolucion    --06

        EXECUTE prp_monto_retiro USING lr_int_det_voluntaria_04.nss,
                                       v_consecutivo_ret
                                 INTO  lr_detalle_04.mto_total_pagado

        IF lr_detalle_04.mto_total_pagado IS NULL THEN
              LET lr_detalle_04.mto_total_pagado = 0    --07
        END IF

        LET lr_detalle_04.mto_total_reintegrar        = lr_int_det_voluntaria_04.monto_neto        --08
        LET lr_detalle_04.num_semanas_cot_descontadas = 0                                          --09 Se informa en ceros 
        LET lr_detalle_04.num_semanas_cot_reint       = lr_int_det_voluntaria_04.semanas_reintegro --10
        LET lr_detalle_04.fecha_reintegro_semanas     = lr_int_det_voluntaria_04.fecha_liquida     --11

        OUTPUT TO REPORT rpt_det_04(lr_detalle_04.*)
        INITIALIZE lr_detalle_04.* TO NULL

    END FOREACH
    FINISH REPORT rpt_det_04
    FREE cur_detalle_04

END FUNCTION 


FUNCTION fn_inicializa_consulta_05()

   DEFINE lc_comando      CHAR(3000)

   LET lc_comando = ' ',
	    '\nSELECT UNIQUE diag_procesar,fecha_ini_vigencia ',
        '\nFROM ret_parcial_resol   ',
        '\nWHERE nss = ?            ',
        '\nAND num_resolucion = ?   ',
        '\n AND fecha_carga_afore = (SELECT MAX(fecha_carga_afore) ',
        '\n                          FROM   ret_parcial_resol      ',
        '\n                          WHERE  nss = ?                ',
        '\n                          AND num_resolucion = ? )      '

   PREPARE prp_diag_pro FROM lc_comando

   LET lc_comando = ' ',
       '\n SELECT COUNT(*)',
       '\nFROM  ret_ws_notifica_app',
       '\nWHERE nss = ? ',
       '\nAND   consecutivo = ? '
   PREPARE prp_sol_app05 FROM lc_comando
	
END FUNCTION 


--------------------------------------------------------------------------------
--fn_genera_detalle_05:Retiros parciales por Desempleo y Matrimonio           --
--                     Rechazados, por fecha de solicitud de retiro.          --
--------------------------------------------------------------------------------
FUNCTION fn_genera_detalle_05(pr_fechas_05)

   DEFINE pr_fechas_05 RECORD 
           fecha_ini DATE,
           fecha_fin DATE 
   END RECORD
   
   DEFINE lr_ret_parcial_05    RECORD 
        nss                      CHAR(11),
        consecutivo              DECIMAL(11,0),
        curp                     CHAR(18),
        tipo_prestacion          INTEGER,
        tipo_desempleo           CHAR(1),
        num_resolucion           INTEGER,
        diag_cuenta_ind          CHAR(3),
        fecha_solicitud          DATE 
   END RECORD 

   DEFINE lr_detalle_05               RECORD 
        tipo_registro                  CHAR(2),			  --01	
        nss                            CHAR(11),          --02
        curp                           CHAR(18),          --03
        fecha_sol_retiro			   DATE,              --04	
        tipo_derecho_sol               DECIMAL(1,0),      --05
        diagnostico_cert               CHAR(3),           --06    
        diagnostico_sol_pago           CHAR(3),           --07
        prestacion                     DECIMAL(2,0),      --08
        num_resolucion                 CHAR(6),           --09
        medio_solicitud                CHAR(3),           --10
        fecha_certificacion            DATE               --11
   END RECORD

   DEFINE v_valida_app      SMALLINT,
          v_valida_afo_web  SMALLINT
   
    DECLARE cur_detalle_05 CURSOR WITH HOLD  FOR 
    SELECT UNIQUE(nss),
           consecutivo,
           curp,
           tipo_prestacion,
           tipo_desempleo,
           num_resolucion,
           diag_cuenta_ind,
           fecha_solicitud 
    FROM   ret_parcial
    WHERE  fecha_genera BETWEEN pr_fechas_05.fecha_ini AND pr_fechas_05.fecha_fin
      AND  estado_solicitud = 20
      AND  (diag_cuenta_ind <> '400' AND diag_cuenta_ind <> '' AND diag_cuenta_ind IS NOT NULL)
    ORDER  BY nss

        START REPORT rpt_det_05 TO gc_ruta_det_05
        FOREACH cur_detalle_05 INTO lr_ret_parcial_05.*
        LET lr_detalle_05.tipo_registro = '05'                                  --01
        LET lr_detalle_05.nss = lr_ret_parcial_05.nss                           --02
        LET lr_detalle_05.curp = lr_ret_parcial_05.curp                         --03
        LET lr_detalle_05.fecha_sol_retiro = lr_ret_parcial_05.fecha_solicitud  --04

        CASE lr_ret_parcial_05.tipo_prestacion
        WHEN 7
            LET lr_detalle_05.tipo_derecho_sol = 3                              --05
        WHEN 6 
            IF lr_ret_parcial_05.tipo_desempleo = 'A' THEN 
                LET lr_detalle_05.tipo_derecho_sol= 0                           --05
            END IF 
            IF lr_ret_parcial_05.tipo_desempleo = 'B' THEN  
                LET lr_detalle_05.tipo_derecho_sol= 1                           --05
            END IF 
        END CASE 

        EXECUTE prp_diag_pro USING lr_ret_parcial_05.nss,
		                           lr_ret_parcial_05.num_resolucion,
		                           lr_ret_parcial_05.nss,
		                           lr_ret_parcial_05.num_resolucion
						     INTO  lr_detalle_05.diagnostico_cert,                --06
                                   lr_detalle_05.fecha_certificacion              --11
       
        LET lr_detalle_05.diagnostico_sol_pago = lr_ret_parcial_05.diag_cuenta_ind  --07
        LET lr_detalle_05.prestacion = lr_ret_parcial_05.tipo_prestacion            --08
        LET lr_detalle_05.num_resolucion = lr_ret_parcial_05.num_resolucion         --09

        --Medio de solicitud
        EXECUTE prp_sol_app05 USING lr_ret_parcial_05.nss,
                                    lr_ret_parcial_05.consecutivo
           INTO v_valida_app
          
        IF v_valida_app > 0 THEN
           LET lr_detalle_05.medio_solicitud = "002" -- APP
        ELSE
           --Se valida si el retiro nace como afore web
              EXECUTE prp_sol_afo_web USING lr_ret_parcial_05.nss,
                                        lr_ret_parcial_05.consecutivo
              INTO v_valida_afo_web
              
              IF v_valida_afo_web > 0 THEN
              	  LET lr_detalle_05.medio_solicitud = "003" -- PORTAL
              ELSE 
              	  LET lr_detalle_05.medio_solicitud = "001" -- OFICINA UEAP
              END IF
        END IF
    
        OUTPUT TO REPORT rpt_det_05 (lr_detalle_05.*)
        END FOREACH 
        FINISH REPORT rpt_det_05
        FREE cur_detalle_05
        INITIALIZE lr_detalle_05.* TO NULL

END FUNCTION 


FUNCTION fn_inicializa_consulta_06()

   DEFINE lc_comando CHAR (3000)

   LET lc_comando = ' ',
                    '\n SELECT UNIQUE nss, ',
                    '\n        consecutivo_lote, ',
                    '\n        folio, ' ,
                    '\n        fecha_conversion , ',
                    '\n        NVL(SUM(monto_en_pesos),0) ',
                    '\n FROM tmp_dis_cta_nxo135  ',
                    '\n WHERE tipo_movimiento = 870   ',
                    '\n GROUP BY 1,2,3,4              ',
                    '\n ORDER BY nss '
    LET lc_comando = lc_comando CLIPPED
    PREPARE prp_det6 FROM lc_comando
    DECLARE cur_det6 CURSOR FOR prp_det6

   LET lc_comando = ' ',
                '\n SELECT UNIQUE nss,         ',
				'\n		   curp,               ',
				'\n		   fecha_solicitud,    ',
                '\n        fecha_fall_mat_des, ',
				'\n		   fecha_envio         ',
				'\n FROM   ret_parcial         ',
				'\n WHERE  nss = ?             ',
				'\n   AND  consecutivo = ?     ',
				'\n   AND  folio = ?           '
	LET lc_comando = lc_comando CLIPPED
	PREPARE prp_par_6 FROM lc_comando

	LET lc_comando = ' ',
	                '\n SELECT UNIQUE fecha_fin_vigencia 	',
                  '\n FROM ret_parcial_resol      ',
                  '\n WHERE nss = ?               ',
                  '\n AND num_resolucion = ?      ',
                  '\n AND diag_procesar IN ("400","106") ',
                  '\n AND fecha_carga_afore = (SELECT MAX(fecha_carga_afore) ',
                  '\n                          FROM   ret_parcial_resol      ',
                  '\n                          WHERE  nss = ?                ',
                  '\n                          AND num_resolucion = ?        ',
                  '\n                          AND diag_procesar IN ("400","106"))'
	PREPARE prp_fecha_vig FROM lc_comando
	
	LET lc_comando = ' ',
	                '\n SELECT MAX(fecha_envio)      ',
					'\n FROM ret_parcial_op16        ',
					'\n WHERE nss = ?                ',
					'\n AND consecutivo = ?          '
	PREPARE prp_fecha_envio FROM lc_comando

    LET lc_comando = " ",
       "\n SELECT COUNT(*)",
       "\nFROM  ret_ws_notifica_app",
       "\nWHERE nss = ? ",
       "\nAND   consecutivo = ? ",
       "\nAND aplicacion_origen <> '07' "
    PREPARE prp_sol_app06 FROM lc_comando

    LET lc_comando = " ",
       "\n SELECT FIRST 1 DECODE(tipo_pago,1,'002',5,'001','003')",
       "\nFROM  ret_beneficiario",
       "\nWHERE nss = ? ",
       "\nAND   consecutivo = ? "
    PREPARE prp_medio_pago06 FROM lc_comando
	
	
END FUNCTION 


--------------------------------------------------------------------------------
-- fn_genera_detalle_06: Información de Retiros Parciales por Matrimonio      --
--                       liquidados en el periodo                             --
--------------------------------------------------------------------------------
FUNCTION fn_genera_detalle_06()

     DEFINE lr_ret_parcial_06   RECORD 
            nss				    CHAR(11),
            consecutivo         DECIMAL(11,0) ,
            folio               INTEGER ,
            fecha_conversion    DATE,
            tot_monto_p         DECIMAL(11,2)
    END RECORD 

    DEFINE lr_detalle_06               RECORD 
        tipo_registro                  CHAR(2),			  --01	
        nss                            CHAR(11),          --02
        curp                           CHAR(18),          --03
        fecha_sol_trabajador           DATE,              --04
        fecha_matrimonio               DATE,              --05
        fecha_cert					   DATE,              --06
        fecha_conclucion_vig           DATE,              --07
        fecha_notif_pago               DATE,              --08
        fecha_liquidacion              DATE,              --09
        importe_rcv_ant_ret            DECIMAL(11,2),     --10
        mto_pagado                     DECIMAL(11,2),     --11
        medio_solicitud                CHAR(3),           --12
        forma_pago                     CHAR(3)            --13        
    END RECORD 

    DEFINE v_valida_app      SMALLINT,
           v_valida_afo_web  SMALLINT

    START REPORT rpt_det_06 TO gc_ruta_det_06
    FOREACH cur_det6 INTO lr_ret_parcial_06.*
    
    LET lr_detalle_06.tipo_registro = '06'                                      --01

    EXECUTE prp_par_6 USING lr_ret_parcial_06.nss,
                            lr_ret_parcial_06.consecutivo,
                            lr_ret_parcial_06.folio
                      INTO lr_detalle_06.nss,                                   --02
                           lr_detalle_06.curp,                                  --03
                           lr_detalle_06.fecha_sol_trabajador,                  --04
                           lr_detalle_06.fecha_matrimonio,                      --05
                           lr_detalle_06.fecha_cert                             --06
                           
	LET lr_detalle_06.fecha_conclucion_vig = lr_ret_parcial_06.fecha_conversion --07

    EXECUTE prp_fecha_envio USING  lr_ret_parcial_06.nss,
	                               lr_ret_parcial_06.consecutivo
						    INTO lr_detalle_06.fecha_notif_pago                  --08

    LET lr_detalle_06.fecha_liquidacion = lr_ret_parcial_06.fecha_conversion    --09

    EXECUTE prp_saldo USING lr_ret_parcial_06.nss,
                            lr_ret_parcial_06.folio 
                      INTO lr_detalle_06.importe_rcv_ant_ret                     --10
   
    LET lr_detalle_06.mto_pagado = lr_ret_parcial_06.tot_monto_p                 --11

    --Medio de solicitud y forma de pago
    EXECUTE prp_sol_app06 USING lr_ret_parcial_06.nss,
                                lr_ret_parcial_06.consecutivo
       INTO v_valida_app
          
        IF v_valida_app > 0 THEN
           LET lr_detalle_06.medio_solicitud = "002" -- APP
        ELSE
           --Se valida si el retiro nace como afore web
              EXECUTE prp_sol_afo_web USING lr_ret_parcial_06.nss,
                                        lr_ret_parcial_06.consecutivo
              INTO v_valida_afo_web
              
              IF v_valida_afo_web > 0 THEN
              	  LET lr_detalle_06.medio_solicitud = "003" -- PORTAL
              ELSE 
              	  LET lr_detalle_06.medio_solicitud = "001" -- OFICINA UEAP
              END IF
        END IF

   EXECUTE prp_medio_pago06 USING lr_ret_parcial_06.nss,
                                lr_ret_parcial_06.consecutivo
      INTO lr_detalle_06.forma_pago

    OUTPUT TO REPORT rpt_det_06(lr_detalle_06.*)
    END FOREACH 
    FINISH REPORT rpt_det_06
   
    INITIALIZE lr_detalle_06.* TO NULL
    
END FUNCTION 


FUNCTION fn_inicializa_consulta_07()

	DEFINE lc_comando  CHAR(3000)

    LET lc_comando = "\n SELECT UNIQUE rp.nss,                                   ",
                     "\n        rp.consecutivo,                                  ",    
                     "\n        rp.curp,                                         ",
                     "\n        rp.fecha_solicitud,                              ",
                     "\n        tmp.fecha_conversion,                            ",
                     "\n        rpo.fecha_envio,                                 ",
                     "\n        rp.tipo_prestacion,                              ",
                     "\n        rp.tipo_desempleo,                               ",
                     "\n        rp.num_resolucion,                               ",
                     "\n        rbd.importepagadotrabajador                      ",
                     "\n FROM   tmp_dis_cta_nxo135 tmp,                          ",
                     "\n        ret_parcial        rp,                           ",
                     "\n        ret_parcial_op16   rpo,                          ",
                     "\n        ret_bus_diag16     rbd                           ",
                     "\n WHERE  rp.nss                    = tmp.nss              ",
                     "\n   AND  rp.consecutivo            = tmp.consecutivo_lote ",
                     "\n   AND  rp.folio                  = tmp.folio            ",
                     "\n   AND  rp.estado_solicitud      IN (13,14)              ",
                     "\n   AND  rp.tipo_prestacion       IN (6,7)                ",
                     "\n   AND  rpo.nss                   = rp.nss               ",
                     "\n   AND  rpo.consecutivo           = rp.consecutivo       ",
                     "\n   AND  rpo.folio                 = rp.folio             ",
                     "\n   AND  rbd.nss                   = rpo.nss               ",
                     "\n   AND  rbd.consecutivotrabajador = rpo.consecutivo       ",
                     "\n   AND  rbd.folio                 = rpo.folio       ",
                     "\n ORDER  BY rp.nss                                       "
    LET lc_comando = lc_comando CLIPPED
    PREPARE prp_par_7 FROM  lc_comando
    DECLARE cur_det7 CURSOR FOR prp_par_7   

    LET lc_comando = ' ',
       '\n SELECT COUNT(*)',
       '\nFROM  ret_ws_notifica_app',
       '\nWHERE nss = ? ',
       '\nAND   consecutivo = ? '
    PREPARE prp_sol_app07 FROM lc_comando

    LET lc_comando = " ",
       "\n SELECT FIRST 1 DECODE(tipo_pago,1,'002',5,'001','003')",
       "\nFROM  ret_beneficiario",
       "\nWHERE nss = ? ",
       "\nAND   consecutivo = ? "
    PREPARE prp_medio_pago07 FROM lc_comando
    
END FUNCTION 


--------------------------------------------------------------------------------
-- fn_genera_detalle_07:Aviso de confirmación de pago de los Retiros          --
--                      parciales por Desempleo y Matrimonio(operación 16)    --
--------------------------------------------------------------------------------
FUNCTION fn_genera_detalle_07(pr_fechas_07)

    DEFINE pr_fechas_07 RECORD
                     fecha_ini    DATE,
                     fecha_fin    DATE
                   END RECORD
                
    DEFINE lr_detalle_07        RECORD 
        tipo_registro                  CHAR(2),			  --01	
        nss                            CHAR(11),          --02
        curp                           CHAR(18),          --03
        fecha_sol_retiro               DATE,              --04
        fecha_liquidacion              DATE,              --05
        fecha_conf_pago                DATE,              --06
        prestacion                     DECIMAL(2,0),      --07
        tipo_proceso_ret_desemp        DECIMAL(1,0),      --08
        importe_pagado_trab            DECIMAL(11,2),     --09 
        num_resolucion                 CHAR(6),           --10
        medio_solicitud                CHAR(3),           --11
        forma_pago                     CHAR(3)            --12
    END RECORD 
 
    DEFINE tpo_desempleo     CHAR(1)
    DEFINE v_consecutivo     DECIMAL(10,0)
    DEFINE v_valida_app      SMALLINT,
           v_valida_afo_web  SMALLINT

    INITIALIZE lr_detalle_07.* TO NULL

    START REPORT rpt_det_07 TO gc_ruta_det_07
    FOREACH cur_det7 INTO lr_detalle_07.nss,                             --02
                          v_consecutivo,
                          lr_detalle_07.curp,                            --03
                          lr_detalle_07.fecha_sol_retiro,                --04
                          lr_detalle_07.fecha_liquidacion,               --05
                          lr_detalle_07.fecha_conf_pago,                 --06
                          lr_detalle_07.prestacion,                      --07
                          tpo_desempleo,                   
                          lr_detalle_07.num_resolucion,                  --10
                          lr_detalle_07.importe_pagado_trab              --09
    
    IF (lr_detalle_07.nss IS NOT NULL) AND (LENGTH(lr_detalle_07.nss CLIPPED) > 0) THEN

    LET lr_detalle_07.tipo_registro = '07' -- 01
                          
   CASE lr_detalle_07.prestacion
    WHEN 7
        LET lr_detalle_07.tipo_proceso_ret_desemp = 0                               --08
        --LET lr_detalle_07.importe_pagado_trab = imp_pagado7                        --09
    WHEN 6
        --LET lr_detalle_07.importe_pagado_trab = imp_pagado6                        --09
        IF tpo_desempleo = 'A' THEN 
            LET lr_detalle_07.tipo_proceso_ret_desemp = 1                           --08
        END IF 
        IF tpo_desempleo = 'B' THEN 
            LET lr_detalle_07.tipo_proceso_ret_desemp = 2                           --08
        END IF 
    END CASE 

    --Medio solicitud y forma de pago
    EXECUTE prp_sol_app06 USING lr_detalle_07.nss,
                                v_consecutivo
       INTO v_valida_app
          
        IF v_valida_app > 0 THEN
           LET lr_detalle_07.medio_solicitud = "002" -- APP
        ELSE
           --Se valida si el retiro nace como afore web
              EXECUTE prp_sol_afo_web USING lr_detalle_07.nss,
                                            v_consecutivo
              INTO v_valida_afo_web
              
              IF v_valida_afo_web > 0 THEN
              	  LET lr_detalle_07.medio_solicitud = "003" -- PORTAL
              ELSE 
              	  LET lr_detalle_07.medio_solicitud = "001" -- OFICINA UEAP
              END IF
        END IF

   EXECUTE prp_medio_pago06 USING lr_detalle_07.nss,
                                  v_consecutivo
      INTO lr_detalle_07.forma_pago
    
    OUTPUT TO REPORT rpt_det_07(lr_detalle_07.*)
    INITIALIZE lr_detalle_07.* TO NULL

    END IF 
    END FOREACH 
    FINISH REPORT  rpt_det_07
   
END FUNCTION 


FUNCTION fn_formato_fecha(p_fecha)

    DEFINE p_fecha      DATE

    DEFINE r_char_fecha CHAR(8)

    IF (p_fecha IS NULL) THEN
        LET r_char_fecha = "00010101"
    ELSE
        LET r_char_fecha = p_fecha USING "YYYYMMDD"
    END IF

    RETURN r_char_fecha

END FUNCTION -- fn_formato_fecha


--------------------------------------------------------------------------------
-- rpt_det_01: Reporte detalle 01                                             --
--------------------------------------------------------------------------------
REPORT rpt_det_01(pr_det_01)
    
   DEFINE pr_det_01                     RECORD  
      tipo_registro                 CHAR(2),         --01
      nss                           CHAR(11),        --02
      curp                          CHAR(18),        --03
      fecha_solicitud_retiro        DATE,            --04
      tipo_derecho                  DECIMAL(1,0),    --05
      fecha_solicitud_cert_afore    DATE,            --06
      fecha_solicitud_cert_imss     DATE,            --07
      fecha_desinvercion_rcv        DATE,            --08
      tipo_derecho_cert_imss        DECIMAL(1,0),    --09
      obs_tipo_derecho_cert_imss    DECIMAL(2,0),    --10
      cert_pago                     DECIMAL(2,0),    --11
      mto_total_rcv_ant_retiro      DECIMAL(11,2),   --12
      mto_total_desinvertido        DECIMAL(11,2),   --13
      fecha_liquidacion             DATE,            --14
      mto_pagado                    DECIMAL(11,2),   --15
      fecha_conc_vigencia           DATE,            --16
      num_semanas_cot_descontar     DECIMAL(3,0) ,   --17
      salario_base_tipo_a           DECIMAL(11,2),   --18
      salario_base_tipo_b           DECIMAL(11,2),   --19
      num_resolucion                CHAR(6) ,        --20
      folio_cert_imss               CHAR(22),        --21
      medio_solicitud               CHAR(3),         --22
      forma_pago                    CHAR(3)          --23
   END RECORD 

   --------------------------------------------------------------------------------
   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

      FORMAT 
      ON EVERY ROW
      PRINT 
        COLUMN 01, pr_det_01.tipo_registro                       ,--01
        COLUMN 03, pr_det_01.nss                                 ,--02
        COLUMN 14, pr_det_01.curp                                ,--03
        COLUMN 32, pr_det_01.fecha_solicitud_retiro              USING "YYYYMMDD"          ,--04
        COLUMN 40, pr_det_01.tipo_derecho                        USING "&"                 ,--05
        COLUMN 41, pr_det_01.fecha_solicitud_cert_afore          USING "YYYYMMDD"          ,--06
        COLUMN 49, pr_det_01.fecha_solicitud_cert_imss           USING "YYYYMMDD"          ,--07
        COLUMN 57, pr_det_01.fecha_desinvercion_rcv              USING "YYYYMMDD"          ,--08
        COLUMN 65, pr_det_01.tipo_derecho_cert_imss              USING "&"                 ,--09
        COLUMN 66, pr_det_01.obs_tipo_derecho_cert_imss          USING "&&"                ,--10
        COLUMN 68, pr_det_01.cert_pago                           USING "&&"                ,--11
        COLUMN 70, pr_det_01.mto_total_rcv_ant_retiro            * 100 USING "&&&&&&&&&&&&&"    ,--12
        COLUMN 83, pr_det_01.mto_total_desinvertido              * 100 USING "&&&&&&&&&&&&&"    ,--13
        COLUMN 96, pr_det_01.fecha_liquidacion                   USING "YYYYMMDD"               ,--14
        COLUMN 104,pr_det_01.mto_pagado                          * 100 USING "&&&&&&&&&&&&&"    ,--15
        COLUMN 117,pr_det_01.fecha_conc_vigencia                 USING "YYYYMMDD"               ,--16
        COLUMN 125,pr_det_01.num_semanas_cot_descontar           USING "&&&"                    ,--17
        COLUMN 128,pr_det_01.salario_base_tipo_a                 * 100 USING "&&&&&&&&&&&&&"    ,--18
        COLUMN 141,pr_det_01.salario_base_tipo_b                 * 100 USING "&&&&&&&&&&&&&"    ,--19
        COLUMN 154,pr_det_01.num_resolucion                      USING "&&&&&&"                 ,--20
        COLUMN 160,pr_det_01.folio_cert_imss                                                    ,--21
        COLUMN 182,pr_det_01.medio_solicitud                                                    ,--22
        COLUMN 185,pr_det_01.forma_pago                                                         ,--23
        COLUMN 188,63 SPACES
      
END REPORT 


--------------------------------------------------------------------------------
-- rpt_det_03: Reporte detalle 03                                            --
--------------------------------------------------------------------------------
REPORT rpt_det_03(pr_det_03)
     
     DEFINE     pr_det_03                     RECORD  
        tipo_registro                   CHAR(2),        --01
        nss                             CHAR(11),       --02
        curp                            CHAR(18),       --03
        tipo_reinversion                INTEGER,        --04
        fecha_reinversion               DATE,           --05
        fecha_reintegro_semanas         DATE,           --06
        det_tpo_reinversion             CHAR(30),       --07
        num_semanas_cot_reint           DECIMAL(3,0) ,  --08
        num_resolucion                  CHAR(6)         --09
     END RECORD 

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT 

        ON EVERY ROW

            PRINT COLUMN  1,pr_det_03.tipo_registro,                                                 -- ID  1 - Tipo de Registro
                  COLUMN  3,pr_det_03.nss,                                                           -- ID  2 - NSS
                  COLUMN 14,pr_det_03.curp,                                                          -- ID  3 - CURP
                  COLUMN 32,pr_det_03.tipo_reinversion                        USING "&&",            -- ID  4 - Tipo de Reinversión
                  COLUMN 34,pr_det_03.fecha_reinversion                       USING "YYYYMMDD",      -- ID  5 - Fecha de Reinversión
                  COLUMN 42,pr_det_03.fecha_reintegro_semanas                 USING "YYYYMMDD",      -- ID  6 - Fecha de reintegro de semanas de cotización
                  COLUMN 50,pr_det_03.det_tpo_reinversion,                                           -- ID  7 - Detalle tipo de reinversión (Otro)
                  COLUMN 80,pr_det_03.num_semanas_cot_reint                   USING "&&&",           -- ID  8 - Número de semanas de cotización reintegradas
                  COLUMN 83,pr_det_03.num_resolucion                          USING "&&&&&&",        -- ID  9 - Número de resolución
                  COLUMN 89,162 SPACE                                                                -- ID 10 - Filler

END REPORT 


--------------------------------------------------------------------------------
-- rpt_det_04: Reporte detalle 04                                            --
--------------------------------------------------------------------------------
REPORT rpt_det_04(pr_det_04)  

     DEFINE pr_det_04                    RECORD  
        tipo_registro                  CHAR(2),         --01         
        nss                            CHAR(11),        --02
        curp                           CHAR(18),        --03
        fecha_reintegro                DATE,            --04
        fecha_sol_ret_parcial          DATE,            --05
        num_resolucion_ret_parcial     CHAR(6),         --06
        mto_total_pagado               DECIMAL(11,2),   --07
        mto_total_reintegrar           DECIMAL(11,2),   --08
        num_semanas_cot_descontadas    DECIMAL(3,0) ,   --09
        num_semanas_cot_reint          DECIMAL(3,0) ,   --10
        fecha_reintegro_semanas        DATE             --11
        END RECORD

    DEFINE v_f_sol_retiro         CHAR(8)
 
    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT 
      
        ON EVERY ROW
        
            LET v_f_sol_retiro = fn_formato_fecha(pr_det_04.fecha_sol_ret_parcial)

            PRINT COLUMN  1,pr_det_04.tipo_registro,                                      -- ID  1 - Tipo de Registro
                  COLUMN  3,pr_det_04.nss,                                                -- ID  2 - NSS
                  COLUMN 14,pr_det_04.curp,                                               -- ID  3 - CURP
                  COLUMN 32,pr_det_04.fecha_reintegro              USING "yyyymmdd",      -- ID  4 - Fecha de Reintegro
                  COLUMN 40,v_f_sol_retiro,                                               -- ID  5 - Fecha de Solicitud del retiro parcial por desempleo
                  COLUMN 48,pr_det_04.num_resolucion_ret_parcial   USING "&&&&&&",        -- ID  6 - Numero de resolucion del retiro parcial
                  COLUMN 54,(pr_det_04.mto_total_pagado * 100)     USING "&&&&&&&&&&&&&", -- ID  7 - Monto total pagado al trabajador
                  COLUMN 67,(pr_det_04.mto_total_reintegrar * 100) USING "&&&&&&&&&&&&&", -- ID  8 - Monto Total a reintegrar
                  COLUMN 80,pr_det_04.num_semanas_cot_descontadas  USING "&&&",           -- ID  9 - Numero de semanas de cotización descontadas
                  COLUMN 83,pr_det_04.num_semanas_cot_reint        USING "&&&",           -- ID 10 - Numero de semanas de cotización a reintegrar
                  COLUMN 86,pr_det_04.fecha_reintegro_semanas      USING "yyyymmdd",      -- ID 11 - Fecha de reintegro de semanas de cotizacion
                  COLUMN 94, 157 SPACE                                                    -- ID 12 - Filler

END REPORT 


--------------------------------------------------------------------------------
-- rpt_det_05: Reporte detalle 05                                            --
--------------------------------------------------------------------------------
REPORT rpt_det_05(pr_det_05)

   DEFINE pr_det_05 RECORD
      tipo_registro                  CHAR(2),			--01
      nss                            CHAR(11),          --02
      curp                           CHAR(18),          --03
      fecha_sol_retiro			     DATE,              --04
      tipo_derecho_sol               DECIMAL(1,0),      --05
      diagnostico_cert               CHAR(3),           --06
      diagnostico_sol_pago           CHAR(3),           --07
      prestacion                     DECIMAL(2,0),      --08
      num_resolucion                 CHAR(6),           --09
      medio_solicitud                CHAR(3),           --10
      fecha_certificacion            DATE               --11
   END RECORD

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT 
      
        ON EVERY ROW
            PRINT COLUMN  1,pr_det_05.tipo_registro,                           -- ID  1 - Tipo de Registro
                  COLUMN  3,pr_det_05.nss,                                     -- ID  2 - NSS
                  COLUMN 14, pr_det_05.curp,                                   -- ID  3 - CURP
                  COLUMN 32, pr_det_05.fecha_sol_retiro      USING "yyyymmdd", -- ID  4 - Fecha de solicitud de retiro
                  COLUMN 40, pr_det_05.tipo_derecho_sol      USING "&",        -- ID  5 - Tipo de derecho solicitado
                  COLUMN 41, pr_det_05.diagnostico_cert      USING "&&&",      -- ID  6 - Diagnóstico de certificación
                  COLUMN 44, pr_det_05.diagnostico_sol_pago,                   -- ID  7 - Diagnóstico de Solicitud de Pago
                  COLUMN 47, pr_det_05.prestacion            USING "&&",       -- ID  8 - Prestación
                  COLUMN 49, pr_det_05.num_resolucion        USING "&&&&&&",   -- ID  9 - Número de resolución
                  COLUMN 55, pr_det_05.medio_solicitud,                        -- ID 10 - Medio de solicitud
                  COLUMN 58, pr_det_05.fecha_certificacion   USING "yyyymmdd", -- ID 11 - Fecha de certificación
                  COLUMN 66, 185 SPACE                                         -- ID 12 - Filler

END REPORT 


--------------------------------------------------------------------------------
-- rpt_det_06: Reporte detalle 06                                           --
--------------------------------------------------------------------------------
REPORT rpt_det_06(pr_det_06)

    DEFINE pr_det_06                    RECORD  
        tipo_registro                  CHAR(2),			  --01	
        nss                            CHAR(11),          --02
        curp                           CHAR(18),          --03
        fecha_sol_trabajador           DATE,              --04
        fecha_matrimonio               DATE,              --05
        fecha_cert					   DATE,              --06
        fecha_conclucion_vig           DATE,              --07
        fecha_notif_pago               DATE,              --08
        fecha_liquidacion              DATE,              --09
        importe_rcv_ant_ret            DECIMAL(11,2),     --10
        mto_pagado                     DECIMAL(11,2),     --11
        medio_solicitud                CHAR(3),           --12
        forma_pago                     CHAR(3)            --13
    END RECORD 

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT 

        ON EVERY ROW

            PRINT COLUMN  1,pr_det_06.tipo_registro,                                     -- ID  1 - Tipo de Registro
                  COLUMN  3,pr_det_06.nss,                                               -- ID  2 - NSS
                  COLUMN 14,pr_det_06.curp,                                              -- ID  3 - CURP
                  COLUMN 32,pr_det_06.fecha_sol_trabajador        USING "yyyymmdd",      -- ID  4 - Fecha de Solicitud del Trabajador
                  COLUMN 40,pr_det_06.fecha_matrimonio            USING "yyyymmdd",      -- ID  5 - Fecha de Matrimonio
                  COLUMN 48,pr_det_06.fecha_cert                  USING "yyyymmdd",      -- ID  6 - Fecha de certificación
                  COLUMN 56,pr_det_06.fecha_conclucion_vig        USING "yyyymmdd",      -- ID  7 - Fecha de Conclusión de Vigencia
                  COLUMN 64,pr_det_06.fecha_notif_pago            USING "yyyymmdd",      -- ID  8 - Fecha de notificación de pago
                  COLUMN 72,pr_det_06.fecha_liquidacion           USING "yyyymmdd",      -- ID  9 - Fecha de Liquidación
                  COLUMN 80,(pr_det_06.importe_rcv_ant_ret * 100) USING "&&&&&&&&&&&&&", -- ID 10 - Importe RCV antes de retiro
                  COLUMN 93,(pr_det_06.mto_pagado * 100)          USING "&&&&&&&&&&&&&", -- ID 11 - Monto pagado
                  COLUMN 106,pr_det_06.medio_solicitud,                                  -- ID 12 - Medio de solicitud
                  COLUMN 109,pr_det_06.forma_pago,                                       -- ID 13 - Forma de pago
                  COLUMN 112, 139 SPACE                                                  -- ID 14

END REPORT 


--------------------------------------------------------------------------------
-- rpt_det_07: Reporte detalle 07                                           --
--------------------------------------------------------------------------------
REPORT rpt_det_07(pr_det_07)

   DEFINE pr_det_07                   RECORD  
        tipo_registro                  CHAR(2),			  --01	
        nss                            CHAR(11),          --02
        curp                           CHAR(18),          --03
        fecha_sol_retiro               DATE,              --04
        fecha_liquidacion              DATE,              --05
        fecha_conf_pago                DATE,              --06
        prestacion                     DECIMAL(2,0),      --07
        tipo_proceso_ret_desemp        DECIMAL(1,0),      --08
        importe_pagado_trab            DECIMAL(11,2),     --09 
        num_resolucion                 CHAR(6),           --10
        medio_solicitud                CHAR(3),           --11
        forma_pago                     CHAR(3)            --12
   END RECORD 

   OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

   FORMAT 
      
   ON EVERY ROW

        PRINT COLUMN  1,pr_det_07.tipo_registro,                                     -- ID  1 - Tipo de Registro
              COLUMN  3,pr_det_07.nss,                                               -- ID  2 - NSS
              COLUMN 14,pr_det_07.curp,                                              -- ID  3 - CURP
              COLUMN 32,pr_det_07.fecha_sol_retiro            USING "yyyymmdd",      -- ID  4 - Fecha de solicitud de retiro
              COLUMN 40,pr_det_07.fecha_liquidacion           USING "yyyymmdd",      -- ID  5 - Fecha de Liquidación
              COLUMN 48,pr_det_07.fecha_conf_pago             USING "yyyymmdd",      -- ID  6 - Fecha de confirmación de pago
              COLUMN 56,pr_det_07.prestacion                  USING "&&",            -- ID  7 - Prestación
              COLUMN 58,pr_det_07.tipo_proceso_ret_desemp     USING "&",             -- ID  8 - Tipo de Proceso de Retiro por Desempleo
              COLUMN 59,(pr_det_07.importe_pagado_trab * 100) USING "&&&&&&&&&&&&&", -- ID  9 - Importe pagado al trabajador
              COLUMN 72,pr_det_07.num_resolucion              USING "&&&&&&",        -- ID 10 - Numero de Resolución
              COLUMN 78,pr_det_07.medio_solicitud,                                   -- ID 11 - Medio de solicitud
              COLUMN 81,pr_det_07.forma_pago,                                        -- ID 12 - Forma de pago
              COLUMN 84,167 SPACE                                                    -- ID 13 - Filler

END REPORT 


--------------------------------------------------------------------------------
--fn_consolida_detalles: concatena los archivos en uno solo 
--
--------------------------------------------------------------------------------
FUNCTION fn_consolida_detalles()

   DEFINE lc_comando   CHAR(800)
   DEFINE lc_comando_aux CHAR(800)
 
   LET lc_comando = "cd ",gr_seg_modulo.ruta_envio CLIPPED,"/; cat DET_01_ANEXO135 DET_03_ANEXO135 DET_04_ANEXO135 DET_05_ANEXO135 DET_06_ANEXO135 DET_07_ANEXO135 > ", lc_nombre
   LET lc_comando_aux = lc_comando CLIPPED 
   RUN lc_comando_aux
   CALL f_lib_borra_lineas(gr_seg_modulo.ruta_envio, lc_nombre)
   LET lc_comando = ''
   LET lc_comando = "chmod 777 ", gr_seg_modulo.ruta_envio CLIPPED,"/",lc_nombre;
   LET lc_comando_aux = lc_comando CLIPPED
   RUN lc_comando_aux
   LET lc_comando_aux = ''
   LET lc_comando_aux = "cd ",gr_seg_modulo.ruta_envio CLIPPED,"/; rm DET_01_ANEXO135"
   RUN lc_comando_aux
   LET lc_comando_aux = ''
   LET lc_comando_aux = "cd ",gr_seg_modulo.ruta_envio CLIPPED,"/; rm DET_03_ANEXO135"
   RUN lc_comando_aux
   LET lc_comando_aux = ''
   LET lc_comando_aux = "cd ",gr_seg_modulo.ruta_envio CLIPPED,"/; rm DET_04_ANEXO135"
   RUN lc_comando_aux
   LET lc_comando_aux = ''
   LET lc_comando_aux = "cd ",gr_seg_modulo.ruta_envio CLIPPED,"/; rm DET_05_ANEXO135"
   RUN lc_comando_aux
   LET lc_comando_aux = ''
   LET lc_comando_aux = "cd ",gr_seg_modulo.ruta_envio CLIPPED,"/; rm DET_06_ANEXO135"
   RUN lc_comando_aux
   LET lc_comando_aux = ''
   LET lc_comando_aux = "cd ",gr_seg_modulo.ruta_envio CLIPPED,"/; rm DET_07_ANEXO135"
   RUN lc_comando_aux
 
END FUNCTION 
