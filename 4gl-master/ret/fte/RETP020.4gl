
DATABASE safre_af

# MAIN
# Función principal que identifica las solicitudes datamart y las almacena en una nueva tabla para su identificación y marca.
MAIN 

   DEFINE v_dia_ejecucion DATE 
   DEFINE v_fecha         CHAR(10)
   DEFINE v_log           CHAR(100)
   DEFINE v_usuario       CHAR(20)
   DEFINE v_msg_log       CHAR(200)
   DEFINE v_query         CHAR(2000)

   DEFINE v_folio             DECIMAL(11,0)
   DEFINE i,j                 INTEGER
   DEFINE v_nombre_trab       CHAR(40)
   DEFINE v_paterno_trab      CHAR(40)
   DEFINE v_materno_trab      CHAR(40)
   DEFINE v_id_aceptado       SMALLINT
   DEFINE v_consecutivo_marca DECIMAL(11,0)
   
   DEFINE v_marca_entra  SMALLINT
   DEFINE v_xcod_rechazo SMALLINT
   DEFINE v_num_nss      SMALLINT

   DEFINE v_ret_trabajador_atributo_pension RECORD 
      id_ret_trab_atrib_pension DECIMAL(11,0),
      nss                       CHAR(11),
      curp                      CHAR(18),
      nombre_trabajador         CHAR(120),
      edad_trabajador           SMALLINT,
      tipo_proceso              CHAR(6),
      regimen                   CHAR(2),
      tipo_seguro               CHAR(2),
      tipo_pension              CHAR(2),
      tipo_prestacion           SMALLINT,
      tipo_retiro               CHAR,
      diagnostico               CHAR(3),
      fecha_recepcion           DATE,      
      codigo_marca              SMALLINT,
      fecha_inicio_marca        DATE,
      fecha_fin_marca           DATE,
      fecha_identificacion      DATE,
      folio_identificacion      DECIMAL(11,0)
   END RECORD

   DEFINE c_marca_trabajador SMALLINT
   DEFINE cero               SMALLINT

   INITIALIZE v_ret_trabajador_atributo_pension.* TO NULL
   INITIALIZE v_nombre_trab,v_paterno_trab,v_materno_trab,v_id_aceptado,v_consecutivo_marca,v_num_nss TO NULL
   INITIALIZE v_marca_entra,v_xcod_rechazo TO NULL

   PREPARE prp_marca_cuenta    FROM "EXECUTE PROCEDURE marca_cuenta(?,?,?,?,?,?,?,?)"
   PREPARE prp_desmarca_cuenta FROM "EXECUTE PROCEDURE desmarca_cuenta(?,?,?,?,?,?)"
   PREPARE prp_diagnostico     FROM "SELECT FIRST 1 id_aceptado FROM tab_diag_procesar_disp WHERE diag_procesar = ? "   

   LET v_dia_ejecucion = TODAY
   LET v_fecha = v_dia_ejecucion
   LET c_marca_trabajador = 935
   LET cero               = 0

   PREPARE prp_usuario FROM "SELECT FIRST 1 USER FROM sysusers"
   EXECUTE prp_usuario INTO v_usuario
   FREE prp_usuario
   
   LET v_log = "RETP020_",v_usuario CLIPPED,'_',v_fecha[4,5],v_fecha[1,2],v_fecha[7,10],'.log'
   CALL STARTLOG(v_log)

   DISPLAY "   INICIA RETP020 IDENTIFICACIÓN DE REGISTROS DATAMART PARA TRABAJADOR CON ATRIBUTO DE PENSIÓN   "
   CALL ERRORLOG("   INICIA RETP020 IDENTIFICACIÓN DE REGISTROS DATAMART PARA TRABAJADOR CON ATRIBUTO DE PENSIÓN   ")

   PREPARE obtiene_folio FROM "CALL fn_obten_glo_folio()"
   EXECUTE obtiene_folio INTO v_folio
   
   LET v_msg_log = "   DIA DE EJECUCIÓN: ",v_dia_ejecucion," FOLIO: ",v_folio
   DISPLAY v_msg_log
   CALL ERRORLOG(v_msg_log)

   LET v_msg_log = "   INICIA EJECUCIÓN REGISTROS DEL DÍA: ",CURRENT HOUR TO SECOND
   DISPLAY v_msg_log   
   CALL ERRORLOG(v_msg_log)
   
   --Busca solicitudes del día 
   LET v_query = " SELECT nss,                 ",
                 "         curp,               ",
                 "         nombre_datamart,    ",
                 "         paterno_datamart,   ",
                 "         materno_datamart,   ",
                 "         tipo_retiro,        ",
                 "         regimen,            ",
                 "         tipo_seguro,        ",
                 "         tipo_pension,       ",
                 "         tipo_prestacion,    ",
                 "         diag_datamart,      ",
                 "         'ISSSTE',           ",
                 "         fecha_resolucion    ",
                 "  FROM ret_datamart_issste   ",
                 "  WHERE fecha_resolucion = ? ",
                 "  AND tipo_retiro NOT IN ('A') ",
                 " UNION                       ",
                 "  SELECT nss,                ",
                 "        curp,                ",
                 "        nombre_afore,        ",
                 "        paterno_afore,       ",
                 "        materno_afore,       ",
                 "        tipo_retiro,         ",
                 "        regimen,             ",
                 "        tipo_seguro,         ",
                 "        tipo_pension,        ",
                 "        tipo_prestacion,     ",
                 "        diag_datamart,       ",
                 "        'IMSS',              ",
                 "        fecha_carga_afore::DATE ",
                 "  FROM ret_det_datamart      ",
                 "  WHERE fecha_carga_afore::DATE = ? ",
                 "  AND tipo_retiro NOT IN ('D','U') "
   PREPARE prp_busca_solicitudes FROM v_query
   DECLARE cur_busca_solicitudes CURSOR FOR prp_busca_solicitudes
   OPEN cur_busca_solicitudes USING v_dia_ejecucion,v_dia_ejecucion
   LET i = 1 
   LET j = 1
   FOREACH cur_busca_solicitudes INTO v_ret_trabajador_atributo_pension.nss,
                                      v_ret_trabajador_atributo_pension.curp,
                                      v_nombre_trab,
                                      v_paterno_trab,
                                      v_materno_trab,
                                      v_ret_trabajador_atributo_pension.tipo_retiro,
                                      v_ret_trabajador_atributo_pension.regimen,
                                      v_ret_trabajador_atributo_pension.tipo_seguro,
                                      v_ret_trabajador_atributo_pension.tipo_pension,
                                      v_ret_trabajador_atributo_pension.tipo_prestacion,
                                      v_ret_trabajador_atributo_pension.diagnostico,
                                      v_ret_trabajador_atributo_pension.tipo_proceso,
                                      v_ret_trabajador_atributo_pension.fecha_recepcion

      IF v_ret_trabajador_atributo_pension.nss IS NULL THEN 
         IF v_ret_trabajador_atributo_pension.curp IS NULL THEN 
            LET v_msg_log = "   REGISTRO SIN NSS Y SIN CURP   "
            DISPLAY v_msg_log
            CALL ERRORLOG(v_msg_log)
         
            INITIALIZE v_ret_trabajador_atributo_pension.* TO NULL
            INITIALIZE v_nombre_trab,v_paterno_trab,v_materno_trab,v_id_aceptado,v_consecutivo_marca,v_num_nss TO NULL
            INITIALIZE v_marca_entra,v_xcod_rechazo TO NULL
         
            LET i = i + 1
            CONTINUE FOREACH
         ELSE 
            SELECT COUNT(*)
            INTO v_num_nss
            FROM afi_mae_afiliado 
            WHERE n_unico = v_ret_trabajador_atributo_pension.curp

            IF v_num_nss IS NOT NULL AND v_num_nss = 1 THEN 
               SELECT n_seguro
               INTO v_ret_trabajador_atributo_pension.nss
               FROM afi_mae_afiliado 
               WHERE n_unico = v_ret_trabajador_atributo_pension.curp
            ELSE
               LET v_msg_log = "   NO SE LOCALIZÓ EL NSS PARA LA CURP: ",v_ret_trabajador_atributo_pension.curp
               DISPLAY v_msg_log               
               CALL ERRORLOG(v_msg_log)

               INITIALIZE v_ret_trabajador_atributo_pension.* TO NULL
               INITIALIZE v_nombre_trab,v_paterno_trab,v_materno_trab,v_id_aceptado,v_consecutivo_marca,v_num_nss TO NULL
               INITIALIZE v_marca_entra,v_xcod_rechazo TO NULL
          
               LET i = i + 1
               CONTINUE FOREACH
            END IF
         END IF
      END IF

      --Cuentas con INHABILITACION  MARCA (130,140 y 120) 
      IF fn_cuenta_inhabilitada(v_ret_trabajador_atributo_pension.nss) = TRUE THEN 
      
        LET v_msg_log = "   CUENTA INHABILITACION EL NSS  ",v_ret_trabajador_atributo_pension.nss
        DISPLAY v_msg_log               
        CALL ERRORLOG(v_msg_log)

        INITIALIZE v_ret_trabajador_atributo_pension.* TO NULL
        INITIALIZE v_nombre_trab,v_paterno_trab,v_materno_trab,v_id_aceptado,v_consecutivo_marca,v_num_nss TO NULL
        INITIALIZE v_marca_entra,v_xcod_rechazo TO NULL

        LET i = i + 1
        CONTINUE FOREACH
      END IF  
      
      --Validando si existe registro duplicado en la tabla: 
      SELECT 'OK'
      FROM ret_trabajador_atributo_pension
      WHERE nss = v_ret_trabajador_atributo_pension.nss
      AND   curp = v_ret_trabajador_atributo_pension.curp
      AND   tipo_proceso    = v_ret_trabajador_atributo_pension.tipo_proceso
      AND   regimen         = v_ret_trabajador_atributo_pension.regimen
      AND   tipo_seguro     = v_ret_trabajador_atributo_pension.tipo_seguro
      AND   tipo_pension    = v_ret_trabajador_atributo_pension.tipo_pension
      AND   tipo_prestacion = v_ret_trabajador_atributo_pension.tipo_prestacion
      AND   tipo_retiro     = v_ret_trabajador_atributo_pension.tipo_retiro
      AND   diagnostico     = v_ret_trabajador_atributo_pension.diagnostico
      AND   fecha_recepcion = v_ret_trabajador_atributo_pension.fecha_recepcion
      GROUP BY 1

      IF STATUS = NOTFOUND THEN --Si no es duplicado procede
         --Genera datos para insertar         
         LET v_ret_trabajador_atributo_pension.nombre_trabajador    = v_nombre_trab CLIPPED,' ',v_paterno_trab CLIPPED,' ',v_materno_trab CLIPPED
         LET v_ret_trabajador_atributo_pension.codigo_marca         = c_marca_trabajador
         LET v_ret_trabajador_atributo_pension.fecha_identificacion = v_dia_ejecucion
         LET v_ret_trabajador_atributo_pension.folio_identificacion = v_folio
         CALL f_obten_edad(v_ret_trabajador_atributo_pension.nss,v_dia_ejecucion)
            RETURNING v_ret_trabajador_atributo_pension.edad_trabajador
         
         --Inserta registro         
         INSERT INTO ret_trabajador_atributo_pension(id_ret_trab_atrib_pension,
                                                     nss,
                                                     curp,
                                                     nombre_trabajador,
                                                     edad_trabajador,
                                                     tipo_proceso,
                                                     regimen,
                                                     tipo_seguro,
                                                     tipo_pension,
                                                     tipo_prestacion,
                                                     tipo_retiro,
                                                     diagnostico,
                                                     fecha_recepcion,
                                                     codigo_marca,
                                                     fecha_inicio_marca,
                                                     fecha_fin_marca,
                                                     fecha_identificacion,
                                                     folio_identificacion)
            VALUES(seq_ret_trab_atrib_pension.NEXTVAL,
                   v_ret_trabajador_atributo_pension.nss,
                   v_ret_trabajador_atributo_pension.curp,
                   v_ret_trabajador_atributo_pension.nombre_trabajador,
                   v_ret_trabajador_atributo_pension.edad_trabajador,
                   v_ret_trabajador_atributo_pension.tipo_proceso,
                   v_ret_trabajador_atributo_pension.regimen,
                   v_ret_trabajador_atributo_pension.tipo_seguro,
                   v_ret_trabajador_atributo_pension.tipo_pension,
                   v_ret_trabajador_atributo_pension.tipo_prestacion,
                   v_ret_trabajador_atributo_pension.tipo_retiro,
                   v_ret_trabajador_atributo_pension.diagnostico,
                   v_ret_trabajador_atributo_pension.fecha_recepcion,
                   v_ret_trabajador_atributo_pension.codigo_marca,
                   v_ret_trabajador_atributo_pension.fecha_inicio_marca,
                   v_ret_trabajador_atributo_pension.fecha_fin_marca,
                   v_ret_trabajador_atributo_pension.fecha_identificacion,
                   v_ret_trabajador_atributo_pension.folio_identificacion)

         --Valida marca
         SELECT id_ret_trab_atrib_pension
         INTO v_ret_trabajador_atributo_pension.id_ret_trab_atrib_pension
         FROM ret_trabajador_atributo_pension
         WHERE nss = v_ret_trabajador_atributo_pension.nss
         AND   curp = v_ret_trabajador_atributo_pension.curp
         AND   tipo_proceso    = v_ret_trabajador_atributo_pension.tipo_proceso
         AND   regimen         = v_ret_trabajador_atributo_pension.regimen
         AND   tipo_seguro     = v_ret_trabajador_atributo_pension.tipo_seguro
         AND   tipo_pension    = v_ret_trabajador_atributo_pension.tipo_pension
         AND   tipo_prestacion = v_ret_trabajador_atributo_pension.tipo_prestacion
         AND   tipo_retiro     = v_ret_trabajador_atributo_pension.tipo_retiro
         AND   diagnostico     = v_ret_trabajador_atributo_pension.diagnostico
         AND   fecha_recepcion = v_ret_trabajador_atributo_pension.fecha_recepcion
         
         IF v_ret_trabajador_atributo_pension.diagnostico <> '101' AND v_ret_trabajador_atributo_pension.diagnostico <> '400' THEN
            EXECUTE prp_diagnostico USING v_ret_trabajador_atributo_pension.diagnostico
               INTO v_id_aceptado
               
            IF v_id_aceptado IS NULL THEN 
               LET v_msg_log = "   DIAGNÓSTICO SIN REGISTRO EN CATÁLOGO: ",v_ret_trabajador_atributo_pension.diagnostico
               DISPLAY v_msg_log               
               CALL ERRORLOG(v_msg_log)
               LET v_id_aceptado = 0
            END IF
         ELSE 
            LET v_id_aceptado = 1
         END IF

         IF v_id_aceptado IS NOT NULL THEN 
            IF v_id_aceptado = 1 THEN 
               SELECT 'OK'
               FROM cta_act_marca 
               WHERE nss = v_ret_trabajador_atributo_pension.nss
               AND   marca_cod = c_marca_trabajador
               GROUP BY 1

               IF STATUS = NOTFOUND THEN
                  UPDATE ret_trabajador_atributo_pension
                  SET fecha_inicio_marca = v_dia_ejecucion
                  WHERE id_ret_trab_atrib_pension = v_ret_trabajador_atributo_pension.id_ret_trab_atrib_pension
                  
                  EXECUTE prp_marca_cuenta USING v_ret_trabajador_atributo_pension.nss,
                                                 c_marca_trabajador,
                                                 v_ret_trabajador_atributo_pension.id_ret_trab_atrib_pension,
                                                 cero,
                                                 cero,
                                                 c_marca_trabajador,
                                                 v_ret_trabajador_atributo_pension.fecha_inicio_marca,
                                                 v_usuario                                                 
                     INTO v_marca_entra,v_xcod_rechazo
                  IF v_marca_entra <> c_marca_trabajador THEN 
                     LET v_msg_log = "   ERROR AL MARCAR REGISTRO: ",v_ret_trabajador_atributo_pension.id_ret_trab_atrib_pension," DETALLE: ",v_xcod_rechazo
                     DISPLAY v_msg_log
                     CALL ERRORLOG(v_msg_log)
                  END IF
               END IF
            END IF
         END IF
                   
          LET j = j + 1
      END IF

      INITIALIZE v_ret_trabajador_atributo_pension.* TO NULL
      INITIALIZE v_nombre_trab,v_paterno_trab,v_materno_trab,v_id_aceptado,v_consecutivo_marca,v_num_nss TO NULL
      INITIALIZE v_marca_entra,v_xcod_rechazo TO NULL
         
      LET i = i + 1
   END FOREACH 
   CLOSE cur_busca_solicitudes
   FREE cur_busca_solicitudes
   FREE prp_busca_solicitudes
   LET i = i - 1
   LET j = j - 1

   LET v_msg_log = "   FINALIZA EJECUCIÓN REGISTROS DEL DÍA: ",CURRENT HOUR TO SECOND
   DISPLAY v_msg_log
   CALL ERRORLOG(v_msg_log)

   LET v_msg_log = "   TOTAL DE REGISTROS PROCESADOS: ",i," TOTAL DE REGISTROS INSERTADOS: ",j
   DISPLAY v_msg_log   
   CALL ERRORLOG(v_msg_log)

   
   --++++++++++++++++++   Busca solicitudes históricas  +++++++++++++++++++++++
   LET v_msg_log = "   INICIA EJECUCIÓN HISTÓRICA: ",CURRENT HOUR TO SECOND
   DISPLAY v_msg_log   
   CALL ERRORLOG(v_msg_log)   

   --Busca solicitudes HOSTÓRICAS 
   LET v_query = " SELECT nss,                 ",
                 "         curp,               ",
                 "         nombre_datamart,    ",
                 "         paterno_datamart,   ",
                 "         materno_datamart,   ",
                 "         tipo_retiro,        ",
                 "         regimen,            ",
                 "         tipo_seguro,        ",
                 "         tipo_pension,       ",
                 "         tipo_prestacion,    ",
                 "         diag_datamart,      ",
                 "         'ISSSTE',           ",
                 "         fecha_resolucion    ",
                 "  FROM ret_datamart_issste   ",
                 "  WHERE fecha_resolucion < ? ",
                 "  AND tipo_retiro NOT IN ('A') ",
                 " UNION                       ",
                 "  SELECT nss,                ",
                 "        curp,                ",
                 "        nombre_afore,        ",
                 "        paterno_afore,       ",
                 "        materno_afore,       ",
                 "        tipo_retiro,         ",
                 "        regimen,             ",
                 "        tipo_seguro,         ",
                 "        tipo_pension,        ",
                 "        tipo_prestacion,     ",
                 "        diag_datamart,       ",
                 "        'IMSS',              ",
                 "        fecha_carga_afore::DATE ",
                 "  FROM ret_det_datamart      ",
                 "  WHERE fecha_carga_afore::DATE < ? ",
                 "  AND tipo_retiro NOT IN ('D','U') "
   PREPARE prp_busca_solicitudes_his FROM v_query
   DECLARE cur_busca_solicitudes_his CURSOR FOR prp_busca_solicitudes_his
   OPEN cur_busca_solicitudes_his USING v_dia_ejecucion,v_dia_ejecucion
   LET i = 1 
   LET j = 1
   FOREACH cur_busca_solicitudes_his INTO v_ret_trabajador_atributo_pension.nss,
                                          v_ret_trabajador_atributo_pension.curp,
                                          v_nombre_trab,
                                          v_paterno_trab,
                                          v_materno_trab,
                                          v_ret_trabajador_atributo_pension.tipo_retiro,
                                          v_ret_trabajador_atributo_pension.regimen,
                                          v_ret_trabajador_atributo_pension.tipo_seguro,
                                          v_ret_trabajador_atributo_pension.tipo_pension,
                                          v_ret_trabajador_atributo_pension.tipo_prestacion,
                                          v_ret_trabajador_atributo_pension.diagnostico,
                                          v_ret_trabajador_atributo_pension.tipo_proceso,
                                          v_ret_trabajador_atributo_pension.fecha_recepcion

      IF v_ret_trabajador_atributo_pension.nss IS NULL THEN 
         IF v_ret_trabajador_atributo_pension.curp IS NULL THEN 
            LET v_msg_log = "   REGISTRO SIN NSS Y SIN CURP   "
            DISPLAY v_msg_log
            CALL ERRORLOG(v_msg_log)
         
            INITIALIZE v_ret_trabajador_atributo_pension.* TO NULL
            INITIALIZE v_nombre_trab,v_paterno_trab,v_materno_trab,v_id_aceptado,v_consecutivo_marca,v_num_nss TO NULL
            INITIALIZE v_marca_entra,v_xcod_rechazo TO NULL
         
            LET i = i + 1
            CONTINUE FOREACH
         ELSE 
            SELECT COUNT(*)
            INTO v_num_nss
            FROM afi_mae_afiliado 
            WHERE n_unico = v_ret_trabajador_atributo_pension.curp

            IF v_num_nss IS NOT NULL AND v_num_nss = 1 THEN 
               SELECT n_seguro
               INTO v_ret_trabajador_atributo_pension.nss
               FROM afi_mae_afiliado 
               WHERE n_unico = v_ret_trabajador_atributo_pension.curp
            ELSE
               LET v_msg_log = "   NO SE LOCALIZÓ EL NSS PARA LA CURP: ",v_ret_trabajador_atributo_pension.curp
               DISPLAY v_msg_log               
               CALL ERRORLOG(v_msg_log)

               INITIALIZE v_ret_trabajador_atributo_pension.* TO NULL
               INITIALIZE v_nombre_trab,v_paterno_trab,v_materno_trab,v_id_aceptado,v_consecutivo_marca,v_num_nss TO NULL
               INITIALIZE v_marca_entra,v_xcod_rechazo TO NULL
          
               LET i = i + 1
               CONTINUE FOREACH
            END IF
         END IF
      END IF

      --Cuentas con INHABILITACION  MARCA (130,140 y 120) 
      IF fn_cuenta_inhabilitada(v_ret_trabajador_atributo_pension.nss) = TRUE THEN 
      
        LET v_msg_log = "   CUENTA INHABILITACION EL NSS  ",v_ret_trabajador_atributo_pension.nss
        DISPLAY v_msg_log               
        CALL ERRORLOG(v_msg_log)

        INITIALIZE v_ret_trabajador_atributo_pension.* TO NULL
        INITIALIZE v_nombre_trab,v_paterno_trab,v_materno_trab,v_id_aceptado,v_consecutivo_marca,v_num_nss TO NULL
        INITIALIZE v_marca_entra,v_xcod_rechazo TO NULL

        LET i = i + 1
        CONTINUE FOREACH
      END IF  

      --Validando si existe registro duplicado en la tabla:
      SELECT 'OK'
      FROM ret_trabajador_atributo_pension
      WHERE nss = v_ret_trabajador_atributo_pension.nss
      AND   curp = v_ret_trabajador_atributo_pension.curp
      AND   tipo_proceso    = v_ret_trabajador_atributo_pension.tipo_proceso
      AND   regimen         = v_ret_trabajador_atributo_pension.regimen
      AND   tipo_seguro     = v_ret_trabajador_atributo_pension.tipo_seguro
      AND   tipo_pension    = v_ret_trabajador_atributo_pension.tipo_pension
      AND   tipo_prestacion = v_ret_trabajador_atributo_pension.tipo_prestacion
      AND   tipo_retiro     = v_ret_trabajador_atributo_pension.tipo_retiro
      AND   diagnostico     = v_ret_trabajador_atributo_pension.diagnostico
      AND   fecha_recepcion = v_ret_trabajador_atributo_pension.fecha_recepcion
      GROUP BY 1

      IF STATUS = NOTFOUND THEN --Si no es duplicado procede
         --Genera datos para insertar         
         LET v_ret_trabajador_atributo_pension.nombre_trabajador    = v_nombre_trab CLIPPED,' ',v_paterno_trab CLIPPED,' ',v_materno_trab CLIPPED
         LET v_ret_trabajador_atributo_pension.codigo_marca         = c_marca_trabajador
         LET v_ret_trabajador_atributo_pension.fecha_identificacion = v_dia_ejecucion
         LET v_ret_trabajador_atributo_pension.folio_identificacion = v_folio
         CALL f_obten_edad(v_ret_trabajador_atributo_pension.nss,v_dia_ejecucion)
            RETURNING v_ret_trabajador_atributo_pension.edad_trabajador
         
         --Inserta registro
         INSERT INTO ret_trabajador_atributo_pension(id_ret_trab_atrib_pension,
                                                     nss,
                                                     curp,
                                                     nombre_trabajador,
                                                     edad_trabajador,
                                                     tipo_proceso,
                                                     regimen,
                                                     tipo_seguro,
                                                     tipo_pension,
                                                     tipo_prestacion,
                                                     tipo_retiro,
                                                     diagnostico,
                                                     fecha_recepcion,
                                                     codigo_marca,
                                                     fecha_inicio_marca,
                                                     fecha_fin_marca,
                                                     fecha_identificacion,
                                                     folio_identificacion)
            VALUES(seq_ret_trab_atrib_pension.NEXTVAL,
                   v_ret_trabajador_atributo_pension.nss,
                   v_ret_trabajador_atributo_pension.curp,
                   v_ret_trabajador_atributo_pension.nombre_trabajador,
                   v_ret_trabajador_atributo_pension.edad_trabajador,
                   v_ret_trabajador_atributo_pension.tipo_proceso,
                   v_ret_trabajador_atributo_pension.regimen,
                   v_ret_trabajador_atributo_pension.tipo_seguro,
                   v_ret_trabajador_atributo_pension.tipo_pension,
                   v_ret_trabajador_atributo_pension.tipo_prestacion,
                   v_ret_trabajador_atributo_pension.tipo_retiro,
                   v_ret_trabajador_atributo_pension.diagnostico,
                   v_ret_trabajador_atributo_pension.fecha_recepcion,
                   v_ret_trabajador_atributo_pension.codigo_marca,
                   v_ret_trabajador_atributo_pension.fecha_inicio_marca,
                   v_ret_trabajador_atributo_pension.fecha_fin_marca,
                   v_ret_trabajador_atributo_pension.fecha_identificacion,
                   v_ret_trabajador_atributo_pension.folio_identificacion)

         --Valida marca
         SELECT id_ret_trab_atrib_pension
         INTO v_ret_trabajador_atributo_pension.id_ret_trab_atrib_pension
         FROM ret_trabajador_atributo_pension
         WHERE nss = v_ret_trabajador_atributo_pension.nss
         AND   curp = v_ret_trabajador_atributo_pension.curp
         AND   tipo_proceso    = v_ret_trabajador_atributo_pension.tipo_proceso
         AND   regimen         = v_ret_trabajador_atributo_pension.regimen
         AND   tipo_seguro     = v_ret_trabajador_atributo_pension.tipo_seguro
         AND   tipo_pension    = v_ret_trabajador_atributo_pension.tipo_pension
         AND   tipo_prestacion = v_ret_trabajador_atributo_pension.tipo_prestacion
         AND   tipo_retiro     = v_ret_trabajador_atributo_pension.tipo_retiro
         AND   diagnostico     = v_ret_trabajador_atributo_pension.diagnostico
         AND   fecha_recepcion = v_ret_trabajador_atributo_pension.fecha_recepcion
         
         IF v_ret_trabajador_atributo_pension.diagnostico <> '101' AND v_ret_trabajador_atributo_pension.diagnostico <> '400' THEN      
            EXECUTE prp_diagnostico USING v_ret_trabajador_atributo_pension.diagnostico
               INTO v_id_aceptado
            
            IF v_id_aceptado IS NULL THEN 
               LET v_msg_log = "   DIAGNÓSTICO SIN REGISTRO EN CATÁLOGO: ",v_ret_trabajador_atributo_pension.diagnostico
               DISPLAY v_msg_log               
               CALL ERRORLOG(v_msg_log)
               LET v_id_aceptado = 0
            END IF
         ELSE 
            LET v_id_aceptado = 1
         END IF

         IF v_id_aceptado IS NOT NULL THEN 
            IF v_id_aceptado = 1 THEN 
               SELECT 'OK'
               FROM cta_act_marca 
               WHERE nss = v_ret_trabajador_atributo_pension.nss
               AND   marca_cod = c_marca_trabajador
               GROUP BY 1

               IF STATUS = NOTFOUND THEN
                  UPDATE ret_trabajador_atributo_pension
                  SET fecha_inicio_marca = v_dia_ejecucion
                  WHERE id_ret_trab_atrib_pension = v_ret_trabajador_atributo_pension.id_ret_trab_atrib_pension
                  
                  EXECUTE prp_marca_cuenta USING v_ret_trabajador_atributo_pension.nss,
                                                 c_marca_trabajador,
                                                 v_ret_trabajador_atributo_pension.id_ret_trab_atrib_pension,
                                                 cero,
                                                 cero,
                                                 c_marca_trabajador,
                                                 v_ret_trabajador_atributo_pension.fecha_inicio_marca,
                                                 v_usuario                                                 
                     INTO v_marca_entra,v_xcod_rechazo
                  IF v_marca_entra <> c_marca_trabajador THEN 
                     LET v_msg_log = "   ERROR AL MARCAR REGISTRO: ",v_ret_trabajador_atributo_pension.id_ret_trab_atrib_pension," DETALLE: ",v_xcod_rechazo
                     DISPLAY v_msg_log
                     CALL ERRORLOG(v_msg_log)
                  END IF
               END IF
            END IF
         END IF
                   
          LET j = j + 1
      END IF

      INITIALIZE v_ret_trabajador_atributo_pension.* TO NULL
      INITIALIZE v_nombre_trab,v_paterno_trab,v_materno_trab,v_id_aceptado,v_consecutivo_marca,v_num_nss TO NULL
      INITIALIZE v_marca_entra,v_xcod_rechazo TO NULL
         
      LET i = i + 1
   END FOREACH
   CLOSE cur_busca_solicitudes_his
   FREE cur_busca_solicitudes_his
   FREE prp_busca_solicitudes_his
   LET i = i - 1
   LET j = j - 1

   LET v_msg_log = "   FINALIZA EJECUCIÓN REGISTROS HISTÓRICOS: ",CURRENT HOUR TO SECOND
   DISPLAY v_msg_log
   CALL ERRORLOG(v_msg_log)

   LET v_msg_log = "   TOTAL DE REGISTROS PROCESADOS: ",i," TOTAL DE REGISTROS INSERTADOS: ",j
   DISPLAY v_msg_log   
   CALL ERRORLOG(v_msg_log)

   DISPLAY "   FINALIZA RETP020 IDENTIFICACIÓN DE REGISTROS DATAMART PARA TRABAJADOR CON ATRIBUTO DE PENSIÓN   "
   CALL ERRORLOG("   FINALIZA RETP020 IDENTIFICACIÓN DE REGISTROS DATAMART PARA TRABAJADOR CON ATRIBUTO DE PENSIÓN   ")

END MAIN 


#+f_obten_edad
#+
#+ Función que obtiene la edad del trabajador en base a su registro de afi_mae_afiliado
#+ 
#+ @param p_nss   CHAR(11) NSS del trabajador a consultar
#+ @param p_fecha DATE     Fecha en base a la que se realiza la consulta
#+
#+ @returnType SMALLINT
#+ @return r_edad          Edad del trabajador al momento de la consulta
#+
FUNCTION f_obten_edad(p_nss,p_fecha)

   DEFINE p_nss               CHAR(11)
   DEFINE p_fecha             DATE 

   DEFINE r_existe            SMALLINT
   DEFINE r_edad              SMALLINT
   DEFINE r_criterio          SMALLINT
   DEFINE r_ind_edad          SMALLINT
   DEFINE v_curp              CHAR(18)
   DEFINE v_rfc               CHAR(13)
   DEFINE v_fena              DATE
   DEFINE v_msg_log          CHAR(100)

   DEFINE lc_instruccion      CHAR(5000)

   INITIALIZE r_edad TO NULL

   LET lc_instruccion = "EXECUTE FUNCTION fn_fnacimiento(?,?)"
   PREPARE exe_fnac FROM lc_instruccion

   SELECT 'OK'
   FROM afi_mae_afiliado 
   WHERE n_seguro = p_nss
   GROUP BY 1

   IF STATUS = NOTFOUND THEN 
      LET v_msg_log = "NO SE PUDO OBTENER EDAD PARA NSS: ",p_nss
      DISPLAY v_msg_log
      CALL ERRORLOG(v_msg_log)
   ELSE 
      EXECUTE exe_fnac USING p_nss,p_fecha INTO r_existe    ,
                                                r_edad      ,
                                                r_criterio  ,
                                                r_ind_edad  ,
                                                v_curp      ,
                                                v_rfc       ,
                                                v_fena
      FREE exe_fnac
   END IF
   
   RETURN r_edad

END FUNCTION

#+f_obten_edad
#+
#+ Función que busca si tiena las marcas 120,130, y 140 
#+ 
#+ @param p_nss   CHAR(11) NSS del trabajador a consultar
#+
#+ @returnType SMALLINT
#+ @return r_activa       True: Cuenta marcada , False: Cuenta sin marcas(120,130,140)
#+
FUNCTION fn_cuenta_inhabilitada(p_nss)

   DEFINE p_nss               CHAR(11)
   DEFINE v_tot_marca         INTEGER 
   DEFINE r_activa            SMALLINT 

   LET v_tot_marca = 0 
   LET r_activa    = FALSE 

   SELECT COUNT(*) INTO v_tot_marca
   FROM cta_act_marca 
   WHERE nss = p_nss
   AND   marca_cod IN (120,130,140)

   IF v_tot_marca > 0 THEN
      LET r_activa = TRUE 
   END IF 

   RETURN r_activa 
END FUNCTION