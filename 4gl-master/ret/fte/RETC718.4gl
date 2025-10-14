DATABASE safre_af 
MAIN
	OPTIONS 
	  PROMPT LINE LAST,
	  INPUT WRAP,
	  ACCEPT KEY control-o
  
  DEFER INTERRUPT
  --se invoca a la funcion donde se captura la nss y los periodos
  CALL f_cap_noti_apo_ext()  
END MAIN

{=========================================================================
Clave: 
Nombre: f_cap_noti_apo_ext
Fecha Creación: 01/12/2011
Autor: Mauricio Sanchez Vargas
Narrativa Del Proceso que Realiza:
Funcion que muestra la pantalla de captura para la consulta de notificaciones 
de aportaciones extemporaneas
 Parametros: Entrada: 
             Salida:                       
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION f_cap_noti_apo_ext()
 DEFINE
  HOY                       DATE,
  lc_nss                    LIKE ret_extemporanea.nss,
  ld_fecha_conversion_ini   LIKE ret_extemporanea.fecha_conversion,
  ld_fecha_conversion_fin   LIKE ret_extemporanea.fecha_conversion,
  ld_fecha_envio_ini        LIKE ret_extemporanea.fecha_envio,
  ld_fecha_envio_fin        LIKE ret_extemporanea.fecha_envio,
  li_cta_reg                INTEGER,
  lc_sql                    CHAR(1800)
  
  WHENEVER ERROR CONTINUE
  
  LET HOY =  TODAY
  LET ld_fecha_conversion_ini =""
  LET ld_fecha_conversion_fin =""
  LET ld_fecha_envio_ini      =""
  LET ld_fecha_envio_fin      =""
  
  OPEN WINDOW RETC7181 AT 2,2 WITH FORM "RETC7181" ATTRIBUTE (BORDER) 
  -- se despliegan las instrucciones en pantalla para el control del programa
  DISPLAY " <CTRL-C> SALIR      <ESC> EJECUTAR                            GENERAL RETIRO " AT 1,1 ATTRIBUTE(REVERSE)
  DISPLAY " RETC718       CONSULTA NOTIFICACION APORTACIONES EXTEMPORANEAS             " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING "DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)
  
  --ERROR "CAPTURAR FECHAS CON FORMATO DD/MM/AAAA "                
  LET INT_FLAG = FALSE
  --PROMPT "INGRESAR EL NSS A CONSULTAR"       
  INITIALIZE lc_nss                  TO NULL 
  INITIALIZE ld_fecha_conversion_ini TO NULL
  INITIALIZE ld_fecha_conversion_fin TO NULL
  INITIALIZE ld_fecha_envio_ini      TO NULL
  INITIALIZE ld_fecha_envio_fin      TO NULL 

  --Se capturan los criterios de busqueda
  INPUT BY NAME lc_nss,
                ld_fecha_conversion_ini,
                ld_fecha_conversion_fin,
                ld_fecha_envio_ini,
                ld_fecha_envio_fin 
  --WITHOUT DEFAULTS
  
   --se muestran comentarios para la captura
   BEFORE FIELD lc_nss                      
   ERROR " INGRESAR EL NSS A CONSULTAR"    
   BEFORE FIELD ld_fecha_conversion_ini
     ERROR " INGRESAR EL PERIODO DE IDENTIFICACION DE LA APORTACION"    
   BEFORE FIELD ld_fecha_conversion_fin
     ERROR " INGRESAR EL PERIODO DE IDENTIFICACION DE LA APORTACION"    
   BEFORE FIELD ld_fecha_envio_ini
     ERROR " INGRESAR EL PERIODO DE NOTIFICACION DE LA APORTACION"        
   BEFORE FIELD ld_fecha_envio_fin
     ERROR " INGRESAR EL PERIODO DE NOTIFICACION DE LA APORTACION"            
           
   --salir
   ON KEY (CONTROL-C)   
      EXIT INPUT
   
   --se ejecuta la consulta
   ON KEY (ESC)
    {
    LET lc_nss                  = GET_FLDBUF(lc_nss)
    LET ld_fecha_conversion_ini = GET_FLDBUF(ld_fecha_conversion_ini)
    LET ld_fecha_conversion_fin = GET_FLDBUF(ld_fecha_conversion_fin)
    LET ld_fecha_envio_ini      = GET_FLDBUF(ld_fecha_envio_ini)
    LET ld_fecha_envio_fin      = GET_FLDBUF(ld_fecha_envio_fin)           
    }
    --se valida que si se captura la fecha inicial de aportacion se capture tambien la final
    IF ld_fecha_conversion_ini IS NOT NULL AND  ld_fecha_conversion_fin IS NULL THEN
      ERROR " SE REQUIERE CAPTURAR LA FECHA FINAL DE APORTACION ..."            
      SLEEP 2
      NEXT FIELD ld_fecha_conversion_fin                                                          
    END IF
    --se valida que si se captura la fecha final de aportacion se capture tambien la inicial
    IF ld_fecha_conversion_ini IS NULL AND  ld_fecha_conversion_fin IS NOT NULL THEN
      ERROR " SE REQUIERE CAPTURAR LA FECHA INICIAL DE APORTACION ..."            
      SLEEP 2
      NEXT FIELD ld_fecha_conversion_ini                                                        
    END IF
    --se valida que si se captura la fecha inicial de notificacion se capture tambien la final
    IF ld_fecha_envio_ini IS NOT NULL AND  ld_fecha_envio_fin IS NULL THEN
      ERROR " SE REQUIERE CAPTURAR LA FECHA FINAL DE NOTIFICACION ..."            
      SLEEP 2
      NEXT FIELD ld_fecha_envio_fin                                                       
    END IF
    --se valida que si se captura la fecha final de notificacion se capture tambien la inicial
    IF ld_fecha_envio_ini IS NULL AND  ld_fecha_envio_fin IS NOT NULL THEN
      ERROR " SE REQUIERE CAPTURAR LA FECHA INICIAL DE NOTIFICACION ..."            
      SLEEP 2
      NEXT FIELD ld_fecha_envio_ini                                                        
    END IF
    --se valida que se capture al menos un criterio de busqueda
    IF lc_nss IS NULL AND ld_fecha_conversion_ini IS NULL AND ld_fecha_envio_ini IS NULL THEN
      ERROR " SE REQUIERE CAPTURAR POR LO MENOS UN CRITERIO ..."      
      SLEEP 2                                                  
      NEXT FIELD lc_nss
    END IF
    
    --se asigna la consulta a la variable y se le iran agregando condiciones dependiendo de lo que se capture
    LET lc_sql = " SELECT COUNT(*) \n",
                 " FROM ret_extemporanea \n",
                 " WHERE sec_pension IS NOT NULL \n"                                
    
    --si se capturo el nss se agrega a la consulta
    IF lc_nss IS NOT NULL THEN    
      LET lc_sql = lc_sql CLIPPED," AND nss = '",lc_nss, "'\n"   
    END IF
    
    --si se capturo la fecha de conversion se agrega a la consulta
    IF ld_fecha_conversion_ini IS NOT NULL THEN
      LET lc_sql = lc_sql CLIPPED, " AND fecha_conversion BETWEEN  '",ld_fecha_conversion_ini,"' AND '",ld_fecha_conversion_fin, "'\n"
    END IF
    
    --si se capturo la fecha de envio se agrega a la consulta
    IF ld_fecha_envio_ini IS NOT NULL THEN
      LET lc_sql = lc_sql CLIPPED, " AND fecha_envio BETWEEN '",ld_fecha_envio_ini,"' AND '",ld_fecha_envio_fin, "'\n"
    END IF  
    
    --DISPLAY "CUENTA REGISTROS ",lc_sql
    PREPARE prp_cta_ret_ext FROM lc_sql
    EXECUTE prp_cta_ret_ext INTO li_cta_reg
    
    IF li_cta_reg > 0 THEN
      --llama a la funcion que reliaza la consultar
      CALL f_cons_noti_apo_ext(lc_nss,ld_fecha_conversion_ini,ld_fecha_conversion_fin,ld_fecha_envio_ini,ld_fecha_envio_fin,li_cta_reg)
      
      --se inicializa a nulos los campos para que al regresar de la consulta no queden datos erroneos
      INITIALIZE lc_nss                  TO NULL 
      INITIALIZE ld_fecha_conversion_ini TO NULL 
      INITIALIZE ld_fecha_conversion_fin TO NULL 
      INITIALIZE ld_fecha_envio_ini      TO NULL 
      INITIALIZE ld_fecha_envio_fin      TO NULL 
      
      --se vuelve a realizar el despliegue para limpiar los campos
      DISPLAY BY NAME lc_nss,
                      ld_fecha_conversion_ini,
                      ld_fecha_conversion_fin,
                      ld_fecha_envio_ini,
                      ld_fecha_envio_fin
      
      --se indica que inicie en el campo de nss
      NEXT FIELD lc_nss
    ELSE
      ERROR "NO EXISTEN REGISTROS CON ESTOS CRITERIOS"
    END IF  
   END INPUT        
    
 CLOSE WINDOW RETC7181
 
END FUNCTION

{=========================================================================
Clave: 
Nombre: f_cons_noti_apo_ext
Fecha Creación: 01/12/2011
Autor: Mauricio Sanchez Vargas
Narrativa Del Proceso que Realiza:
Funcion que muestra la pantalla de la consulta de notificaciones de 
aportaciones extemporaneas
 Parametros: Entrada: 
             Salida:                       
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION f_cons_noti_apo_ext(lc_nss,ld_fecha_conversion_ini,ld_fecha_conversion_fin,ld_fecha_envio_ini,ld_fecha_envio_fin,li_cta_reg)
 DEFINE
  HOY                       DATE,
  lr_ret_extemporanea       RECORD LIKE ret_extemporanea.*,
  larr_ret_extemporanea     ARRAY[600] OF RECORD 
    cursorx                 CHAR(1),
    nss                     LIKE ret_extemporanea.nss,
    fecha_conversion        LIKE ret_extemporanea.fecha_conversion,
    periodo_pago            LIKE ret_extemporanea.periodo_pago,
    fecha_envio             LIKE ret_extemporanea.fecha_envio,
    descripcion             CHAR(30)        
  END RECORD,
  larr_ret_extemporanea_aux ARRAY[600] OF RECORD 
    nss                     LIKE ret_extemporanea.nss,
    fecha_conversion        LIKE ret_extemporanea.fecha_conversion,
    periodo_pago            LIKE ret_extemporanea.periodo_pago,
    fecha_envio             LIKE ret_extemporanea.fecha_envio,
    descripcion             CHAR(30),
    sec_pension             LIKE ret_extemporanea.sec_pension
  END RECORD,
  lc_des_diag_proc          CHAR(30),
  lr_ret_det_datamart       RECORD LIKE ret_det_datamart.*,
  larr_ret_det_datamart     ARRAY[100] OF RECORD 
    sec_pension             LIKE ret_det_datamart.sec_pension,
    diag_datamart           LIKE ret_det_datamart.diag_datamart,
    regimen                 LIKE ret_det_datamart.regimen,
    tipo_seguro             LIKE ret_det_datamart.tipo_seguro,
    tipo_pension            LIKE ret_det_datamart.tipo_pension,
    tipo_prestacion         LIKE ret_det_datamart.tipo_prestacion,
    fecha_ini_pen           LIKE ret_det_datamart.fecha_ini_pen,
    estado                  LIKE ret_det_datamart.estado
  END RECORD,  
  li_indice                 SMALLINT,
  lc_nss                    LIKE ret_extemporanea.nss,
  ld_fecha_conversion_ini   LIKE ret_extemporanea.fecha_conversion,
  ld_fecha_conversion_fin   LIKE ret_extemporanea.fecha_conversion,
  ld_fecha_envio_ini        LIKE ret_extemporanea.fecha_envio,
  ld_fecha_envio_fin        LIKE ret_extemporanea.fecha_envio,
  lc_sql                    CHAR(1800),
  lc_descripcion            CHAR(30),
  lc_descripcion_aux        CHAR(30),
  li_cta_reg                SMALLINT,
  li_pos_arr                SMALLINT,
  lc_condicion              CHAR(30),
  diag_envio_procesar_aux   CHAR(3),
  lc_desc_regimen           LIKE tab_regimen.descripcion,
  lc_desc_seguro            LIKE tab_seguro.descripcion,
  lc_desc_pension           LIKE tab_pension.descripcion,
  lc_desc_prestacion        LIKE tab_prestacion.descripcion,
  lc_desc_estado            LIKE tab_estado.estad_desc,
  lc_sec_pension            LIKE ret_extemporanea.sec_pension,
  li_pos_arr_aux            SMALLINT  
  
   
  LET HOY = TODAY
  LET li_pos_arr     = 0
  LET li_pos_arr_aux = 0
  
  --se asigna la consulta a la variable y se le iran agregando condiciones dependiendo de lo que se capture
  LET lc_sql = " SELECT * \n",
               " FROM ret_extemporanea \n",
               " WHERE sec_pension IS NOT NULL \n"                              
  
  --si se capturo el nss se agrega a la consulta
  IF lc_nss IS NOT NULL THEN    
    LET lc_sql = lc_sql CLIPPED," AND nss = '",lc_nss, "'\n"   
  END IF
  
  --si se capturo la fecha de conversion se agrega a la consulta
  IF ld_fecha_conversion_ini IS NOT NULL THEN
    LET lc_sql = lc_sql CLIPPED, " AND fecha_conversion BETWEEN  '",ld_fecha_conversion_ini,"' AND '",ld_fecha_conversion_fin, "'\n"
  END IF
  
  --si se capturo la fecha de envio se agrega a la consulta
  IF ld_fecha_envio_ini IS NOT NULL THEN
    LET lc_sql = lc_sql CLIPPED, " AND fecha_envio BETWEEN '",ld_fecha_envio_ini,"' AND '",ld_fecha_envio_fin, "'\n"
  END IF  
    
  --se inicializa el indice del arreglo
  LET li_indice = 1
  --se ejecuta la consulta de aportaciones extemporaneas    
  PREPARE prp_cons_ret_ext FROM lc_sql
  DECLARE cur_ret_ext CURSOR FOR prp_cons_ret_ext
  FOREACH cur_ret_ext INTO lr_ret_extemporanea.*
   
   --se consulta la descripcion de diag_envio_procesar
   CALL f_obt_desc_env_proc(lr_ret_extemporanea.diag_envio_procesar) RETURNING lc_descripcion
   
   --se transfieren los datos del registro al arreglo
   LET larr_ret_extemporanea[li_indice].cursorx = ''
   LET larr_ret_extemporanea[li_indice].nss = lr_ret_extemporanea.nss   
   LET larr_ret_extemporanea[li_indice].fecha_conversion = lr_ret_extemporanea.fecha_conversion USING "DD-MM-YYYY" 
   LET larr_ret_extemporanea[li_indice].periodo_pago = lr_ret_extemporanea.periodo_pago 
   LET larr_ret_extemporanea[li_indice].fecha_envio = lr_ret_extemporanea.fecha_envio USING "DD-MM-YYYY" 
   LET diag_envio_procesar_aux =lr_ret_extemporanea.diag_envio_procesar
   LET lc_descripcion_aux =diag_envio_procesar_aux,"-",lc_descripcion
   --LET larr_ret_extemporanea[li_indice].descripcion = lr_ret_extemporanea.diag_envio_procesar CLIPPED,"-",lc_descripcion CLIPPED
   LET larr_ret_extemporanea[li_indice].descripcion = lc_descripcion_aux   
   
   --se asignan valores al arreglo auxiliar que contiene el campo sec_pension
   LET larr_ret_extemporanea_aux[li_indice].nss = lr_ret_extemporanea.nss   
   LET larr_ret_extemporanea_aux[li_indice].fecha_conversion = lr_ret_extemporanea.fecha_conversion USING "DD-MM-YYYY" 
   LET larr_ret_extemporanea_aux[li_indice].periodo_pago = lr_ret_extemporanea.periodo_pago 
   LET larr_ret_extemporanea_aux[li_indice].fecha_envio = lr_ret_extemporanea.fecha_envio USING "DD-MM-YYYY"    
   LET larr_ret_extemporanea_aux[li_indice].descripcion = lc_descripcion_aux
   LET larr_ret_extemporanea_aux[li_indice].sec_pension = lr_ret_extemporanea.sec_pension
   
   --se incremente el indice del arreglo
   LET li_indice = li_indice + 1
  END FOREACH
     
  -- se abre la ventana para mostrar los resultados
  OPEN WINDOW RETC7182 AT 2,2                                                WITH FORM "RETC7182"  ATTRIBUTE (BORDER)
  DISPLAY " <CTRL-C> SALIR     <ESC>EJECUTAR                              GENERAL RETIRO " AT 1,1  ATTRIBUTE(REVERSE)
  DISPLAY "  RETC718      CONSULTA NOTIFICACION APORTACIONES EXTEMPORANEAS               "  AT 3,1  ATTRIBUTE(REVERSE)
  DISPLAY HOY USING "DD-MM-YYYY"                                                            AT 3,69 ATTRIBUTE(REVERSE)
 
   --se ejecuta la consulta de resoluciones para el primer registro
   --ON KEY (ESC)
       LET lc_sql = " SELECT * \n",
                 " FROM ret_det_datamart \n",
                 " WHERE nss = '", larr_ret_extemporanea[1].nss, "'\n",
                 " AND sec_pension = '",larr_ret_extemporanea_aux[1].sec_pension,"'"
    
    PREPARE prp_cons_datamart0 FROM lc_sql
    --DECLARE cur_datamart CURSOR FOR prp_cons_datamart
    EXECUTE prp_cons_datamart0 INTO lr_ret_det_datamart.*           
    
    --obtiene descripciones
    LET lc_desc_regimen    = f_obt_desc_regimen(lr_ret_det_datamart.regimen)
    LET lc_desc_seguro     = f_obt_desc_seguro(lr_ret_det_datamart.tipo_seguro)
    LET lc_desc_pension    = f_obt_desc_pension(lr_ret_det_datamart.tipo_pension)
    LET lc_desc_prestacion = f_obt_desc_prestacion(lr_ret_det_datamart.tipo_prestacion)
    LET lc_desc_estado     = f_obt_desc_estado(lr_ret_det_datamart.estado)        
             
    DISPLAY
     lr_ret_det_datamart.sec_pension,
     lr_ret_det_datamart.diag_datamart,
     lr_ret_det_datamart.regimen,
     lc_desc_regimen,
     lr_ret_det_datamart.tipo_seguro,
     lc_desc_seguro,
     lr_ret_det_datamart.tipo_pension,
     lc_desc_pension,
     lr_ret_det_datamart.tipo_prestacion,
     lc_desc_prestacion,
     lr_ret_det_datamart.fecha_ini_pen,
     lr_ret_det_datamart.estado,
     lc_desc_estado
    TO
     sec_pension,
     diag_datamart,
     regimen,
     des_regimen,     
     tipo_seguro,         
     des_tipo_seguro,     
     tipo_pension,        
     des_tipo_pension,    
     tipo_prestacion,     
     des_tipo_prestacion, 
     fecha_ini_pen,       
     estado,              
     des_estado  
   
  -- despliega la consulta de aportaciones extemporaneas    
  INPUT ARRAY larr_ret_extemporanea WITHOUT DEFAULTS FROM scr_det1.* ATTRIBUTE(COUNT = li_cta_reg)
                
   --se muestran las resoluciones
   BEFORE ROW 
         
     --se asigna la posicion del cursor
     LET li_pos_arr     = ARR_CURR()
     --se asigna el tamaño del arreglo
     LET li_pos_arr_aux = ARR_COUNT()  
     --Al llegar al ultimo registro se pasa el cursor al primero              
     IF li_pos_arr > li_pos_arr_aux THEN
        CALL FGL_SETCURRLINE( 1,1 )
     END IF
     
     --se limpian las variables     
     INITIALIZE lr_ret_det_datamart.sec_pension     TO NULL
     INITIALIZE lr_ret_det_datamart.diag_datamart   TO NULL
     INITIALIZE lr_ret_det_datamart.regimen         TO NULL
     INITIALIZE lc_desc_regimen                     TO NULL
     INITIALIZE lr_ret_det_datamart.tipo_seguro     TO NULL
     INITIALIZE lc_desc_seguro                      TO NULL
     INITIALIZE lr_ret_det_datamart.tipo_pension    TO NULL
     INITIALIZE lc_desc_pension                     TO NULL
     INITIALIZE lr_ret_det_datamart.tipo_prestacion TO NULL
     INITIALIZE lc_desc_prestacion                  TO NULL
     INITIALIZE lr_ret_det_datamart.fecha_ini_pen   TO NULL
     INITIALIZE lr_ret_det_datamart.estado          TO NULL
     INITIALIZE lc_desc_estado                      TO NULL
     
    LET li_pos_arr = ARR_CURR() 
    --se prepara la consulta de los detalles de la resolucion
    LET lc_sql = " SELECT * \n",
                 " FROM ret_det_datamart \n",
                 " WHERE nss = '", larr_ret_extemporanea[li_pos_arr].nss, "'\n",
                 " AND sec_pension = '",larr_ret_extemporanea_aux[li_pos_arr].sec_pension,"'"
    
    PREPARE prp_cons_datamart1 FROM lc_sql
    --DECLARE cur_datamart CURSOR FOR prp_cons_datamart
    EXECUTE prp_cons_datamart1 INTO lr_ret_det_datamart.*           
    
    --obtiene descripciones
    LET lc_desc_regimen    = f_obt_desc_regimen(lr_ret_det_datamart.regimen)
    LET lc_desc_seguro     = f_obt_desc_seguro(lr_ret_det_datamart.tipo_seguro)
    LET lc_desc_pension    = f_obt_desc_pension(lr_ret_det_datamart.tipo_pension)
    LET lc_desc_prestacion = f_obt_desc_prestacion(lr_ret_det_datamart.tipo_prestacion)
    LET lc_desc_estado     = f_obt_desc_estado(lr_ret_det_datamart.estado)        
        
    DISPLAY
     lr_ret_det_datamart.sec_pension,
     lr_ret_det_datamart.diag_datamart,
     lr_ret_det_datamart.regimen,
     lc_desc_regimen,
     lr_ret_det_datamart.tipo_seguro,
     lc_desc_seguro,
     lr_ret_det_datamart.tipo_pension,
     lc_desc_pension,
     lr_ret_det_datamart.tipo_prestacion,
     lc_desc_prestacion,
     lr_ret_det_datamart.fecha_ini_pen,
     lr_ret_det_datamart.estado,
     lc_desc_estado
    TO
     sec_pension,
     diag_datamart,
     regimen,
     des_regimen,     
     tipo_seguro,         
     des_tipo_seguro,     
     tipo_pension,        
     des_tipo_pension,    
     tipo_prestacion,     
     des_tipo_prestacion, 
     fecha_ini_pen,       
     estado,              
     des_estado          		   
  
   {--se cambia la funcionalidad
    LET li_pos_arr = ARR_CURR()
    --se verifica que exista el nss en la tabla ret_solicitud_saldo para consultar resoluciones
    SELECT COUNT(*)
    INTO li_cta_reg
    FROM ret_solicitud_saldo
    WHERE nss = larr_ret_extemporanea[li_pos_arr].nss
    
    IF li_cta_reg > 0 THEN
       LET lc_sql = " SELECT * \n",
                    " FROM ret_det_datamart \n",
                    " WHERE nss = '", larr_ret_extemporanea[li_pos_arr].nss, "'"
        
       --se inicializa el indice del segundo arreglo 
       LET li_indice = 1 
       PREPARE prp_cons_datamart FROM lc_sql
       DECLARE cur_datamart CURSOR FOR prp_cons_datamart
       FOREACH cur_datamart INTO lr_ret_det_datamart.*             
         
         --se transfieren los registros al arreglo
         LET larr_ret_det_datamart[li_indice].sec_pension     = lr_ret_det_datamart.sec_pension    
         LET larr_ret_det_datamart[li_indice].diag_datamart   = lr_ret_det_datamart.diag_datamart  
         LET larr_ret_det_datamart[li_indice].regimen         = lr_ret_det_datamart.regimen        
         LET larr_ret_det_datamart[li_indice].tipo_seguro     = lr_ret_det_datamart.tipo_seguro    
         LET larr_ret_det_datamart[li_indice].tipo_pension    = lr_ret_det_datamart.tipo_pension   
         LET larr_ret_det_datamart[li_indice].tipo_prestacion = lr_ret_det_datamart.tipo_prestacion
         LET larr_ret_det_datamart[li_indice].fecha_ini_pen   = lr_ret_det_datamart.fecha_ini_pen USING "DD-MM-YYYY"  
         LET larr_ret_det_datamart[li_indice].estado          = lr_ret_det_datamart.estado          
         
         --se incrementa el indice del segundo arreglo
         LET li_indice = li_indice +1         
       END FOREACH
    END IF 
    CALL SET_COUNT(li_indice - 1)
    --se despliega el arreglo de resoluciones
    DISPLAY ARRAY larr_ret_det_datamart TO scr_det2.*           
  }
   --EXIT DISPLAY
    --salir
   ON KEY (CONTROL-C)   
    EXIT INPUT    
      --CALL f_cap_noti_apo_ext()
   --ON KEY (ESC)
     --EXIT DISPLAY 
       
  END INPUT
  CLOSE WINDOW RETC7182
END FUNCTION  

{=========================================================================
Clave: 
Nombre: f_obt_desc_env_proc
Fecha Creación: 01/12/2011
Autor: Mauricio Sanchez Vargas
Narrativa Del Proceso que Realiza:
Funcion que obtiene la descripcion del diag_envio_procesar
 Parametros: Entrada: 
             Salida:                       
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION f_obt_desc_env_proc(li_diag_envio_procesar)
 DEFINE 
  li_diag_envio_procesar LIKE ret_extemporanea.diag_envio_procesar,
  lc_desc_envio_procesar LIKE tab_diag_sol_envio.descripcion
  
  SELECT UNIQUE descripcion 
  INTO lc_desc_envio_procesar
  FROM tab_diag_sol_envio
  WHERE diag_envio_procesar = li_diag_envio_procesar
  
 RETURN lc_desc_envio_procesar
END FUNCTION

{=========================================================================
Clave: 
Nombre: f_obt_desc_regimen
Fecha Creación: 07/12/2011
Autor: Mauricio Sanchez Vargas
Narrativa Del Proceso que Realiza:
Funcion que obtiene la descripcion del regimen
 Parametros: Entrada: 
             Salida:                       
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION f_obt_desc_regimen(li_regimen)
 DEFINE 
  li_regimen      LIKE tab_regimen.regimen,
  lc_desc_regimen LIKE tab_regimen.descripcion
  
  SELECT descripcion 
  INTO lc_desc_regimen
  FROM tab_regimen
  WHERE regimen = li_regimen
  
 RETURN lc_desc_regimen
END FUNCTION

{=========================================================================
Clave: 
Nombre: f_obt_desc_seguro
Fecha Creación: 07/12/2011
Autor: Mauricio Sanchez Vargas
Narrativa Del Proceso que Realiza:
Funcion que obtiene la descripcion del seguro
 Parametros: Entrada: 
             Salida:                       
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION f_obt_desc_seguro(li_seguro)
 DEFINE 
  li_seguro       LIKE tab_seguro.clave,
  lc_desc_seguro  LIKE tab_seguro.descripcion
  
  SELECT descripcion 
  INTO lc_desc_seguro
  FROM tab_seguro
  WHERE clave = li_seguro
  
 RETURN lc_desc_seguro
END FUNCTION

{=========================================================================
Clave: 
Nombre: f_obt_desc_pension
Fecha Creación: 07/12/2011
Autor: Mauricio Sanchez Vargas
Narrativa Del Proceso que Realiza:
Funcion que obtiene la descripcion de la pension
 Parametros: Entrada: 
             Salida:                       
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION f_obt_desc_pension(li_pension)
 DEFINE 
  li_pension       LIKE tab_pension.tipo_pension,
  lc_desc_pension  LIKE tab_pension.descripcion
  
  SELECT descripcion 
  INTO lc_desc_pension
  FROM tab_pension
  WHERE tipo_pension = li_pension
  
 RETURN lc_desc_pension
END FUNCTION

{=========================================================================
Clave: 
Nombre: f_obt_desc_prestacion
Fecha Creación: 07/12/2011
Autor: Mauricio Sanchez Vargas
Narrativa Del Proceso que Realiza:
Funcion que obtiene la descripcion de la prestacion
 Parametros: Entrada: 
             Salida:                       
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION f_obt_desc_prestacion(li_prestacion)
 DEFINE 
  li_prestacion       LIKE tab_prestacion.tipo_prestacion,
  lc_desc_prestacion  LIKE tab_prestacion.descripcion
  
  SELECT descripcion 
  INTO lc_desc_prestacion
  FROM tab_prestacion
  WHERE tipo_prestacion = li_prestacion
  
 RETURN lc_desc_prestacion
END FUNCTION

{=========================================================================
Clave: 
Nombre: f_obt_desc_estado
Fecha Creación: 07/12/2011
Autor: Mauricio Sanchez Vargas
Narrativa Del Proceso que Realiza:
Funcion que obtiene la descripcion de estado
 Parametros: Entrada: 
             Salida:                       
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION f_obt_desc_estado(li_estado)
 DEFINE 
  li_estado       LIKE ret_estado.estado_solicitud,
  lc_desc_estado  LIKE ret_estado.descripcion
  
  SELECT descripcion
  INTO lc_desc_estado
  FROM ret_estado
  WHERE estado_solicitud = li_estado
  
 RETURN lc_desc_estado
END FUNCTION

