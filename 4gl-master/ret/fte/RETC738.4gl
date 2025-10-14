#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC738  => EJECUCION DE PROVISION DE DERECHO OTORGADO                #
#Fecha creacion    => 8 DE DICIEMBRE DE 2011                                    #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 3 DE MAYO DE 2012                                         #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     Adecuaciones para incluir las modificaciones a la Ley del #
#                     INFONAVIT 73                                              #
#Sistema           => RET                                                       #
#------------------------------MODIFICACIONES-----------------------------------#
#Requerimiento     => INV-1491                                                  #
#Fecha y Autor     => 27-Ago-2012  -  Alejandro Chagoya Salazar                 #
#Descripcion       => Se agrega la fecha como parametro de entrada, para        #
#                  => realizar la provision por dia.                            #
#################################################################################
#Requerimiento     => CPL-1014                                                  #
#Fecha y Autor     => 02-Oct-2012  -  Alejandro Chagoya Salazar                 #
#Descripcion       => Agregar la validacion Solo para las cuentas con tipo      #
#                  => pensión IP(INCAPACIDAD PERMANENTE) se considera el        #
#                  => porcentaje de valuación, para TODOS los tipos de retiro   #
#################################################################################
#Requerimiento     => CPL-1147                                                  #
#Fecha y Autor     => 13-Feb-2013  -  Alejandro Chagoya Salazar                 #
#Descripcion       => se recalculan pesos en lugar de acciones para vivienda    #
#                  => cuando no se alcanza a cubrir el monto constitutivo       #
#-------------------------------------------------------------------------------#
#Modificacion      => CPL-1199                                                  #
#Fecha y Autor     => 28-02-2013 Alejandro Chagoya Salazar                      #
#Descripcion       => Se agrega funcion f_primero para obtener el primer dia    #
#                  => del mes de la liquidacion para vivienda                   #
#-------------------------------------------------------------------------------#
#Modificacion      => CPL-1506      22-01-2014   Alejandro Chagoya Salazar      #
#Descripcion       => Se agrega validacion para diagnostico 505, solo aplica    #
#                  => retiros diferentes  a D o S y sin desinversion            #
#################################################################################
#Requerimiento     => CPL-1567   13-Mzo-2014    Alejandro Chagoya Salazar       #
#Descripcion       => Se valida solicitud de reiversion para evitar duplicados  #
#################################################################################
#Requerimiento     => CPL-1618  05-Mayo-2014    Alejandro Chagoya Salazar       #
#Descripcion       => cuentas con FIP != entre saldos previos y datamart se     #
#                  => diagnostican con 505                                      #
#################################################################################
#Requerimiento     => CPL-1673  04-Julio-2014    Alejandro Chagoya Salazar      #
#Descripcion       => Se elimina validacion de tipo_tramite en la opcion previo #
#################################################################################
#Requerimiento     => CPL-1745  01-Octubre-2014    Phelippe Ricardo dos Santos  #
#Descripcion       =>Validar porcentaje para los retiros tipo A y B             #
#                  =>con Tipo Seguro 'RT' y Tipo de Pensión 'IP'                #
#-----------------------------------------------------------------------------  #
#Modificacion      => CPL-1720 31-Octubre-2014 Javier Gonzalez Jeronimo         #
#Descripcion       => Se ejecuta el SP fn_inserta_marca_pen en la funcion       #
#                  => primer_paso_pre                                           #
#################################################################################
#Req CPL-2255 19/02/2016 CMR se agrega validacion para solicitudes con datos =  #
#Req CPL-2265 22/02/2016 CMR se modifica cursor para ir los saldos de las subcta#
#-----------------------------------------------------------------------------  #
#Modificacion      => CPL-2578 24/04/2017 Ricardo Perez Ramirez                 #
#Descripcion       => Diagnostico incorrecto en el otorgamiento de pensión      #
#                  => "Confirmación DATAMART"                                   #

#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gar_precio_acc ARRAY [90] OF RECORD
        estado          SMALLINT     ,
        fecha           DATE         ,
        siefore         SMALLINT     ,
        precio_dia      DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD
        derecho_otorgado        LIKE ret_estado.estado_solicitud    ,
        provisionado            LIKE ret_estado.estado_solicitud    ,
        preliquidado            LIKE ret_estado.estado_solicitud    ,  #cpl-2255
        rechazado_sal           LIKE ret_estado.estado_solicitud    ,
        liquida_saldo           LIKE ret_estado.estado_solicitud    ,
        rechazado_derecho       LIKE ret_estado.estado_solicitud    ,
        sol_reinversion         LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_tipo_tramite RECORD
        transferencia           LIKE tab_tipo_tramite.cod_tramite   ,
        disposicion             LIKE tab_tipo_tramite.cod_tramite   ,
        reinversion             LIKE tab_tipo_tramite.cod_tramite
    END RECORD

    DEFINE
        gr_datamart_do          RECORD LIKE   ret_datamart_do.*

    DEFINE gr_acept_datamart RECORD
        diagnostico            LIKE   ret_det_datamart.diagnostico         ,
        ind_env_recep_trans    LIKE   ret_det_datamart.ind_env_recep_trans ,
        ind_env_recep_disp     LIKE   ret_det_datamart.ind_env_recep_disp  ,
        estado                 LIKE   ret_det_datamart.estado
    END RECORD

    DEFINE
        gdt_fecha_viv           ,
        HOY                     DATE,
        CERO                    SMALLINT

    DEFINE
        enter                   CHAR(001) ,
        gc_usuario              CHAR(015) ,
        gc_error                CHAR(1000),
        gc_prepare              CHAR(300)

    DEFINE
        gs_coppel               ,
        gs_metlife              ,
        gs_mov_viv97            ,
        gs_procesa              ,
        gs_sieviv               ,
        gs_cod_tramite          ,
        gs_num_siefores         , #-- Indica el numero de siefores que se usan actualmente
        gs_codigo_afore         SMALLINT,
        m_fecha                 DATE,    #ACS 27Ago2012
        gi_rechazo              ,
        gi_cuantos              ,
        gi_id_registro          INT

    DEFINE   m_saldos           SMALLINT
    DEFINE   m_reinversion      SMALLINT
    DEFINE   mr_param           RECORD LIKE safre_af:seg_modulo.*
    DEFINE   m_ruta             VARCHAR(255)
    DEFINE   m_hoy              DATE

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I
    CALL STARTLOG(FGL_GETENV("USER")||".RETC738.log")
    CALL f_tablas_tmp()
    CALL init()
    CALL f_abre_ventana()

    MENU "D. OTORGADO"
        COMMAND "Previo" "Previo"
            CALL previo()

        COMMAND "pRovision" "Provisión"
            CALL provision()

        COMMAND "Salir" "Salir del Programa"
          EXIT MENU
    END MENU
END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------
    LET  m_fecha = TODAY

    LET HOY                 = TODAY
    LET gs_sieviv           = 11
    LET gs_metlife          = 564
    LET gi_rechazo          = 0
    LET gi_cuantos          = 0
    LET CERO                = 0
    LET m_saldos = 0
    LET m_reinversion = 0
    LET m_ruta = ""
    LET m_hoy = TODAY

    INITIALIZE mr_param.* TO NULL

    --Parametros globales y usuario que ejecuta
    SELECT a.* INTO mr_param.*
    FROM safre_af:seg_modulo a
    WHERE a.modulo_cod = "ret"

    ----- FECHA DE VIVIENDA -----
    {Se comenta por #MLM-1701
    LET gc_prepare = " EXECUTE FUNCTION fn_obten_fecha_val(?) "
    PREPARE eje_fecha_viv FROM gc_prepare
    EXECUTE eje_fecha_viv USING HOY INTO gdt_fecha_viv}

    LET gdt_fecha_viv = f_primero()    #MLM-1701
    LET gc_prepare = " "

    -- MOVIMIENTO DE TRANSFERENCIA LEY INFONAVIT 73
    SELECT codigo
    INTO   gs_mov_viv97
    FROM   tab_movimiento
    WHERE  descripcion  = "RETIRO TRANSFERENCIA VIVIENDA 97 REGIMEN 73"

    -- Obtenemos el numero de siefores actual en el sistema
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore NOT IN (11,12,13)

    ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_coppel
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*COPPEL*"

    ----- TIPOS DE TRAMITE  -----
    SELECT A.cod_tramite
    INTO   gr_tipo_tramite.transferencia
    FROM   tab_tipo_tramite A
    WHERE  A.descripcion = "TRANSFERENCIA"

    SELECT A.cod_tramite
    INTO   gr_tipo_tramite.disposicion
    FROM   tab_tipo_tramite A
    WHERE  A.descripcion = "DISPOSICION"

    SELECT A.cod_tramite
    INTO   gr_tipo_tramite.reinversion
    FROM   tab_tipo_tramite A
    WHERE  A.descripcion = "DEVOLUCION"

     ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.derecho_otorgado
    FROM   ret_estado A
    WHERE  A.descripcion = "DERECHO OTORGADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO DERECHO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado_sal
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO SALDO"

    SELECT A.estado_solicitud   #ACS 106
    INTO   gr_edo.liquida_saldo
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO SALDO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado_derecho
    FROM   ret_estado A
    WHERE  A.descripcion = "DERECHO NO OTORGADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.sol_reinversion
    FROM   ret_estado A
    WHERE  A.descripcion = "SOLICITUD REINVERSION"
    
    SELECT A.estado_solicitud          #cpl-2255
    INTO   gr_edo.preliquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO DERECHO"

    --- CALCULO MONTO CONSTITUTIVO ---
    #LET gc_prepare = "EXECUTE FUNCTION safre_af:fn_calc_constitutivo_2_5(?,?,?)"
    #PREPARE eje_mto_cons FROM gc_prepare

    --PREPARE para obtener el consecutivo
    LET gc_prepare = ""
    LET gc_prepare = " EXECUTE PROCEDURE safre_af:fn_obten_ret_consecutivo()"
    LET gc_prepare = gc_prepare CLIPPED
    PREPARE p_consec FROM gc_prepare

    #Prepare sentencia para actualizar Solicitud de Saldo
    LET gc_prepare =
                 "UPDATE safre_af:ret_solicitud_saldo ",
                 "   SET estado_solicitud    = 120    ",
                 " WHERE nss                 = ?      ",
                 "   AND estado_solicitud    = ?      ",
                 "   AND id_solicitud_saldo  = ?      "

    LET gc_prepare = gc_prepare CLIPPED
    PREPARE p_upd_solicitud FROM gc_prepare

    #Prepare sentencia para actualizar Solicitud de Saldo
    LET gc_prepare = ""
    LET gc_prepare = "UPDATE safre_af:ret_datamart_do SET " ,
                 " estado = 1 ",
                 " WHERE nss = ?",
                 " AND estado = 0 ",
                 " AND DATE(fecha_recep) = ? "
    LET gc_prepare = gc_prepare CLIPPED
    PREPARE p_upd_datamart FROM gc_prepare

    # PREPARE para marcar_la cuenta
    LET gc_prepare = ""
    LET gc_prepare = "EXECUTE PROCEDURE safre_af:marca_cuenta(?,?,?,?,?,?,?,?)"
    LET gc_prepare = gc_prepare CLIPPED
    PREPARE p_marca FROM gc_prepare

    CREATE TEMP TABLE tmp_error(
    nss         CHAR(11),
    error_msj   VARCHAR(255)
    ) WITH NO LOG;

    -- CPL-1720
    ----- CODIGO DE TRAMITE -----
    SELECT cod_tramite
    INTO   gs_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA"

    ----- INSERTA MARCA PENSIONADO -----
    LET gc_prepare = ""
    LET gc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?,?) "
    PREPARE eje_marca_pen FROM gc_prepare


END FUNCTION

#---------------------------------------------------------------------------#
# previo : Realiza las validaciones previas de las solicitudes              #
#---------------------------------------------------------------------------#
FUNCTION previo()
   DISPLAY "                  PREVIO DE RESOLUCIONES DERECHO OTORGADO                    " AT 3,1 ATTRIBUTE(REVERSE)

   LET gi_rechazo = 0 
   LET gi_cuantos = 0
   LET m_reinversion = 0
   
   DELETE FROM tmp_error WHERE 1 = 1

   CALL f_despliega_info_pre() RETURNING gs_procesa

   IF gs_procesa THEN
      DISPLAY "                                                                      " AT 8,1
      CALL primer_paso_pre()
   END IF

   PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter
   DISPLAY "                                                                      " AT 13,1
   DISPLAY "                                                                      " AT 14,1
   DISPLAY "                                                                      " AT 15,1
   DISPLAY "                                                                      " AT 16,1
   DISPLAY "                                                                      " AT 17,1
   DISPLAY "                                                                      " AT 18,1
   #CLOSE WINDOW RETC7381
END FUNCTION

#------------------------------------------------------------#
# primer_paso_pre: Validaciones Previas para las solicitdes  #
#------------------------------------------------------------#
FUNCTION primer_paso_pre()
   DEFINE ld_saldo_sbctas      DECIMAL (13,2),
          ld_saldo_trans       DECIMAL (13,2)
   DEFINE lc_diagnostico       CHAR(3)
   DEFINE ld_fecha_asig    DATE,
          ld_fecha_ini     DATE

    DEFINE
        ls_resp             ,
        ls_folio            ,
        ls_ind_fip          ,   #CPL-2324
        ls_bandera          ,
        ls_band_005         SMALLINT 

   # Inicializando las variables a NULL
   INITIALIZE gr_datamart_do.*, gr_acept_datamart.*  TO NULL
    LET ls_ind_fip = 1   #CPL-2324
    LET ls_band_005 = 0

   # Barriendo todas las solicitudes registradas
   DECLARE cur_dtmrt_do CURSOR FOR
      SELECT * FROM ret_datamart_do
      WHERE  estado = 0
      AND    DATE(fecha_recep) = m_fecha

   FOREACH cur_dtmrt_do INTO gr_datamart_do.*
      
      LET ls_band_005 = 0
      LET ls_bandera =  f_sol_duplicada()
      
      IF ls_bandera = 0 THEN 
         CONTINUE FOREACH
      END IF
   
      LET  gi_id_registro  = NULL
      # Obteniendo el id consecutivo de la solicitud rechazada
      EXECUTE p_consec INTO gi_id_registro

    -- CPL-1720
    -- Ejecuta el SP que inserta la marca de trabajador pensionado
    LET ls_folio = 0
    
    EXECUTE eje_marca_pen USING gr_datamart_do.nss          ,
                                gr_datamart_do.curp         ,
                                ls_folio                    ,
                                gi_id_registro              ,
                                gr_datamart_do.tipo_retiro  ,
                                gs_cod_tramite              ,
                                HOY
                          INTO  ls_resp

      LET gi_cuantos = gi_cuantos + 1
      DISPLAY "REGISTROS PROCESADOS: ", gi_cuantos USING "<<<<<<<<<" AT 13,1

      # Validando que el diagnostico no sea diferente a 101-Resolución Aceptada, se ser así solo se guarda como rechazada
      IF gr_datamart_do.diag_datamart != "101" THEN    --diagnostico rechazado por PROCESAR
         LET gc_error = "DIAGNOSTICO (", gr_datamart_do.diag_datamart CLIPPED, ") INCORRECTO, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
         LET  lc_diagnostico = NULL
         CALL f_insert_rechazo(lc_diagnostico)
         CONTINUE FOREACH
      END IF
      
      IF gr_datamart_do.tipo_tramite = 5 OR gr_datamart_do.tipo_tramite = "005" THEN   #se trae validacion de la linea 453
            
            IF gr_datamart_do.tipo_seguro = "RT" AND gr_datamart_do.tipo_pension = "IP" THEN
               IF gr_datamart_do.porcentaje_val IS NULL THEN
                  LET gr_datamart_do.porcentaje_val = 0
               END IF  
               CASE gr_datamart_do.tipo_retiro
                  WHEN "A"
                     IF gr_datamart_do.porcentaje_val < 25.00  THEN
                         LET gr_acept_datamart.diagnostico = 0
                         LET gr_acept_datamart.estado = gr_edo.rechazado_derecho
                         LET  gc_error = "NO CUMPLE CON LOS PORCENTAJES DE VALIDACION NSS: ", gr_datamart_do.nss CLIPPED," TIPO TRAMITE : ", gr_datamart_do.tipo_tramite CLIPPED
                         CALL f_insert_rechazo(gr_acept_datamart.diagnostico)
                         EXECUTE p_upd_datamart USING gr_datamart_do.nss, m_fecha
                         CONTINUE FOREACH
                     END IF
                  WHEN "B"
                     IF gr_datamart_do.porcentaje_val < 50.00  THEN
                         LET gr_acept_datamart.diagnostico = 0
                         LET gr_acept_datamart.estado = gr_edo.rechazado_derecho
                         LET  gc_error = "NO CUMPLE CON LOS PORCENTAJES DE VALIDACION NSS: ", gr_datamart_do.nss CLIPPED," TIPO TRAMITE : ", gr_datamart_do.tipo_tramite CLIPPED
                         CALL f_insert_rechazo(gr_acept_datamart.diagnostico)
                         EXECUTE p_upd_datamart USING gr_datamart_do.nss, m_fecha
                         CONTINUE FOREACH
                     END IF
               END CASE
            END IF

            # Diagnosticando como 0 y estado 112-DERECHO NO OTORGADO
            LET gr_acept_datamart.diagnostico = 0
            LET gr_acept_datamart.estado = gr_edo.rechazado_derecho
            LET ls_band_005 = 1
            CALL f_inserta_sol_reinversion()
      END IF


      IF  gr_datamart_do.tipo_seguro = "RT" AND gr_datamart_do.tipo_pension = "IP" THEN --> CPL-1745
         IF	gr_datamart_do.porcentaje_val IS NULL THEN
         	  LET gr_datamart_do.porcentaje_val = 0
         END IF	  
         CASE gr_datamart_do.tipo_retiro
            WHEN "A"
               IF gr_datamart_do.porcentaje_val < 25.00  THEN
               CALL f_insert_extempo()
               CALL f_inserta_sol_reinversion()
               CONTINUE FOREACH
               #--display "tipo A" SLEEP 2
               END IF
            WHEN "B"
            	 IF gr_datamart_do.porcentaje_val < 50.00  THEN
               CALL f_insert_extempo()
               CALL f_inserta_sol_reinversion()
               CONTINUE FOREACH
               #--display "tipo B" SLEEP 2
               END IF
         END CASE
      END IF   --> CPL-1745

      LET gr_acept_datamart.diagnostico  = 501 #diagnostico de aceptado
      LET gr_acept_datamart.estado = gr_edo.derecho_otorgado # Se asigna el estado de derecho otorgado

      SELECT NVL(SUM(monto_en_acciones), 0) INTO ld_saldo_trans
      FROM dis_cuenta
      WHERE nss = gr_datamart_do.nss
      AND subcuenta IN (1,2,4,5,6,7,8,9)

      IF ld_saldo_trans <= 0 AND gr_acept_datamart.diagnostico = 501 THEN	#se agrega validacion
          LET gr_acept_datamart.diagnostico  = 502
          LET gr_acept_datamart.estado = gr_edo.rechazado_derecho
      END IF

#CPL-2324 INI
    IF (gr_datamart_do.tipo_retiro = "B" AND
        gr_datamart_do.tipo_seguro = "RJ" AND gr_datamart_do.tipo_prestacion = "1" AND 
       (gr_datamart_do.tipo_pension = "AS" OR gr_datamart_do.tipo_pension = "OR"
        OR gr_datamart_do.tipo_pension = "VI" OR gr_datamart_do.tipo_pension = "VO"
        OR gr_datamart_do.tipo_pension = "IP" OR gr_datamart_do.tipo_pension = "RE"
        OR gr_datamart_do.tipo_pension = "CE" OR gr_datamart_do.tipo_pension = "VE"
        OR gr_datamart_do.tipo_pension = "IN" OR gr_datamart_do.tipo_pension = "PP")) THEN

        SELECT NVL(SUM(monto_en_acciones),0) INTO ld_saldo_sbctas
        FROM dis_cuenta
        WHERE nss = gr_datamart_do.nss
        AND subcuenta IN (1,2,5,6,9)

    ELSE
    	  IF (gr_datamart_do.tipo_retiro = "U" AND gr_datamart_do.tipo_prestacion <> "23") THEN
    	  	  SELECT NVL(SUM(monto_en_acciones),0) INTO ld_saldo_sbctas
            FROM dis_cuenta
            WHERE nss = gr_datamart_do.nss
            AND subcuenta IN (1,2,4,5,6,7,9)
        ELSE
            SELECT NVL(SUM(monto_en_acciones),0) INTO ld_saldo_sbctas
            FROM dis_cuenta
            WHERE nss = gr_datamart_do.nss
            AND subcuenta IN (1,2,4,5,6,9)
        END IF
    END IF
#CPL-2324 FIN

      IF ld_saldo_sbctas <= 0 AND gr_acept_datamart.diagnostico = 501 THEN
         LET gr_acept_datamart.diagnostico  = 509     --- CPL-2578
         LET gr_acept_datamart.estado = gr_edo.rechazado_derecho
      END IF

      #Valida matriz de derechos
      SELECT "OK" FROM ret_matriz_derecho a
      WHERE a.regimen        = gr_datamart_do.regimen
      AND a.tipo_seguro      = gr_datamart_do.tipo_seguro
      AND a.tipo_pension     = gr_datamart_do.tipo_pension
      AND a.tipo_prestacion  = gr_datamart_do.tipo_prestacion
      AND a.tipo_retiro      = gr_datamart_do.tipo_retiro
      GROUP BY 1

       IF STATUS = NOTFOUND THEN
          LET  gc_error = "MATRIZ DE DERECHO NO IDENTIFICADA, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
          LET lc_diagnostico  = 507
          CALL f_insert_rechazo(lc_diagnostico)
          CONTINUE FOREACH
       END IF

      #Valida cuenta en laudo
      SELECT "OK"
        FROM cta_act_marca
       WHERE nss = gr_datamart_do.nss
         AND marca_cod = 590     # Validando que la cuenta no se encuentre marcada como 590-TRAMITE JUDICIAL

      IF SQLCA.SQLCODE = 0 THEN
         #Se rechaza la solicitud de datamart
         LET  gc_error = "CUENTA EN LAUDO EN LA AFORE SIN TRANSFERENCIA, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
         LET  lc_diagnostico = 504
         CALL f_insert_rechazo(lc_diagnostico)
         CONTINUE FOREACH
      END IF
      
      LET ld_fecha_ini = NULL
                                
      SELECT finicta INTO ld_fecha_ini
      FROM safre_af:afi_mae_afiliado
      WHERE n_seguro = gr_datamart_do.nss
      
      LET ld_fecha_asig = NULL
      
      SELECT fecha_asignacion
      INTO ld_fecha_asig
      FROM safre_af:afi_det_asignado
      WHERE n_seguro = gr_datamart_do.nss
      AND fecha_asignacion = (SELECT MAX(fecha_asignacion)
                              FROM safre_af:afi_det_asignado
                              WHERE n_seguro = gr_datamart_do.nss)
      
      IF SQLCA.SQLCODE = 0 THEN    # Existe o existio como asignado
         LET ld_fecha_ini = ld_fecha_asig
      END IF

#CPL-1506 INI
       IF gr_datamart_do.fecha_ini_pen < ld_fecha_ini THEN
         IF (gr_datamart_do.tipo_retiro != "D" AND gr_datamart_do.tipo_retiro != "S") THEN
              SELECT "OK"
              FROM ret_solicitud_saldo
              WHERE nss = gr_datamart_do.nss
              AND estado_solicitud = gr_edo.liquida_saldo
              GROUP BY 1

             IF SQLCA.SQLCODE = 100 THEN    --NO existe desinversion
                LET gr_acept_datamart.diagnostico = 505          #resolucion pendiente
                LET gr_acept_datamart.estado = gr_edo.rechazado_derecho
                LET gc_error = "FIP MENOR A FECHA INICIO CUENTA. NSS: ",gr_datamart_do.nss CLIPPED, "-", ld_fecha_ini
                CALL ERRORLOG(gc_error CLIPPED)
             END IF
         END IF
#CPL-2324 INI
       ELSE
         IF (gr_datamart_do.tipo_retiro = "B" AND
             gr_datamart_do.tipo_seguro = "RJ" AND gr_datamart_do.tipo_prestacion = "1" AND 
            (gr_datamart_do.tipo_pension = "AS" OR gr_datamart_do.tipo_pension = "OR"
             OR gr_datamart_do.tipo_pension = "VI" OR gr_datamart_do.tipo_pension = "VO"
             OR gr_datamart_do.tipo_pension = "IP" OR gr_datamart_do.tipo_pension = "RE"
             OR gr_datamart_do.tipo_pension = "CE" OR gr_datamart_do.tipo_pension = "VE"
             OR gr_datamart_do.tipo_pension = "IN" OR gr_datamart_do.tipo_pension = "PP")) THEN

                LET ls_ind_fip = 2

         END IF
#CPL-2324 FIN
       END IF
#CPL-1506 FIN

      # Realizando las validaciones para los diferentes Tipo Trámite:
      # 3-Resolución con Saldo Previo
      # 4-Resolución sin Saldo Previo
      # 5-Trámite Informativo
      # 6-Resolución Extemporanea

#CPL-1673 INI

      IF gr_acept_datamart.diagnostico = 501 AND  ls_band_005 = 0 THEN
            CALL f_valida_tipo_Tramite(ls_ind_fip)

      END IF
#CPL-1673 INI

      # Guardando las solicitudes
      CALL f_guarda_solicitud()

      EXECUTE p_upd_datamart USING gr_datamart_do.nss, m_fecha

      # Inicializando las variables a NULL
      INITIALIZE gr_datamart_do.*, gr_acept_datamart.* TO NULL
   END FOREACH

   IF gi_rechazo > 0 THEN
     LET m_ruta = ""
     LET m_ruta = mr_param.ruta_listados CLIPPED ,"/rpt_datamart_rechazo-",m_hoy USING "DDMMYY"

     UNLOAD TO m_ruta
     SELECT * FROM tmp_error

     DISPLAY "SE GENERA EL ARCHIVO CON CUENTAS RECHAZADAS " AT 17,1
     DISPLAY  m_ruta AT 18,1 ATTRIBUTE(REVERSE)
   END IF

END FUNCTION
-----------------------------------------
FUNCTION f_insert_extempo()    --> CPL-1745

   DEFINE lr_ext_datamart      RECORD LIKE ret_ext_datamart.*
   DEFINE lc_diagnostico       CHAR(3)
   
   INITIALIZE lr_ext_datamart.* TO NULL
   
   LET lr_ext_datamart.id_ext_datamart  = 0
   LET lr_ext_datamart.folio_cliente    = gr_datamart_do.nss
   LET lr_ext_datamart.folio_t_procesar = gr_datamart_do.folio_t_procesar
   LET lr_ext_datamart.nss              = gr_datamart_do.nss
   LET lr_ext_datamart.curp             = gr_datamart_do.curp
   LET lr_ext_datamart.fecha_carga      = TODAY
   LET lr_ext_datamart.sec_pension      = gr_datamart_do.sec_pension
   LET lr_ext_datamart.tipo_tramite     = 4 # Resolución Pendiente de Marca

      # Insertando la solicitud de extemporaneas
      WHENEVER ERROR CONTINUE
      SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
      LOCK TABLE ret_ext_datamart IN EXCLUSIVE MODE;

      INSERT INTO safre_af:ret_ext_datamart VALUES(lr_ext_datamart.*)
      IF SQLCA.SQLERRD[3] = 0 THEN
         LET gc_error = "NO SE INSERTÓ SOL. DE EXTEMPORANEAS, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
         CALL ERRORLOG(gc_error CLIPPED)
         SET LOCK MODE TO NOT WAIT;
         UNLOCK TABLE ret_ext_datamart;
         WHENEVER ERROR STOP
         RETURN
      END IF
      SET LOCK MODE TO NOT WAIT;
      UNLOCK TABLE ret_ext_datamart;
      WHENEVER ERROR STOP

      LET gc_error = "SE GENERA SOLICITUD EXTEMPORANEA, CUENTA EN PROCESO OPERATIVO, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
      LET gr_acept_datamart.diagnostico = 506
      #LET gr_datamart_do.tipo_tramite = 6     #extemporaneas
      LET lc_diagnostico  = 506
      CALL f_insert_rechazo(lc_diagnostico)

END FUNCTION
------------------------------------------> CPL-1745
#---------------------------------------------------------------------------#
# f_insert_rechazo : Inserta la solicitud rechazada                         #
#---------------------------------------------------------------------------#
FUNCTION f_insert_rechazo(pc_diagnostico)
   DEFINE pc_diagnostico      CHAR(3)

   CALL ERRORLOG(gc_error CLIPPED)

   INSERT INTO tmp_error VALUES(gr_datamart_do.nss,gc_error)

   # Insertando la solicitud rechazada
   SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
   LOCK TABLE ret_rech_datamart IN EXCLUSIVE MODE;

      INSERT INTO ret_rech_datamart
         VALUES(gi_id_registro,                       # id_registro
                gr_datamart_do.folio_t_procesar,      # folio_t_procesar
                gr_datamart_do.nss,                   # nss
                gr_datamart_do.ent_origen,            # entidad_origen
                gr_datamart_do.tipo_tramite,          # tipo_tramite
                gr_datamart_do.num_issste,            # num_issste
                gr_datamart_do.sec_pension,           # sec_pension
                NULL,                                 # folio
                gr_datamart_do.curp,                  # curp
                gr_datamart_do.nombre_datamart,       # nombre_datamart
                gr_datamart_do.nombre_afore,          # nombre_afore
                gr_datamart_do.paterno_afore,         # paterno_afore
                gr_datamart_do.materno_afore,         # materno_afore
                gr_datamart_do.tipo_movimiento,       # tipo_movimiento
                gr_datamart_do.tipo_retiro,           # tipo_retiro
                gr_datamart_do.regimen,               # regimen
                gr_datamart_do.tipo_seguro,           # tipo_seguro
                gr_datamart_do.tipo_pension,          # tipo_pensio
                gr_datamart_do.tipo_prestacion,       # tipo_prestacion
                gr_datamart_do.cve_pension,           # clave_pension
                gr_datamart_do.art_negativa,          # art_negativa
                gr_datamart_do.frac_negativa,         # frac_negativa
                gr_datamart_do.num_considerando,      # num_considerando
                gr_datamart_do.fecha_ini_pen,         # fecha_ini_pen
                gr_datamart_do.fec_ini_pago,          # fec_ini_pago
                gr_datamart_do.fecha_resolucion,      # fecha_resolucion
                gr_datamart_do.clave_aseguradora,     # clave_aseguradora
                gr_datamart_do.porcentaje_val,        # porcentaje_val
                gr_datamart_do.semanas_cotizadas,     # semanas_cotizadas
                gr_datamart_do.fec_carga_datamart,    # fec_carga_datamart
                gr_datamart_do.diag_datamart,         # diag_datamart
                gr_datamart_do.estado_sub_viv,        # estado_sub_viv
                gr_datamart_do.monto_sol_imss,        # monto_sol_imss
                gr_datamart_do.transf_previa,         # transf_previa
                gr_datamart_do.consec_procesar,       # num_consec_procesar
                gr_datamart_do.tot_resol_porcesar,    # total_resol_procesar
                CURRENT,                              # fecha_carga_afore
                gr_edo.rechazado_derecho,             # estado
                pc_diagnostico,                       # diagnostico
                NULL,                                 # ind_env_recep_trans
                NULL)                                 # ind_env_recep_disp

   IF SQLCA.SQLERRD[3] = 0 THEN
      # Se envía el error de inserción al Log de errores
      LET gc_error = "NO SE INSERTO LA SOLICITUD DE RECHAZO, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED ," NSS : ", gr_datamart_do.nss CLIPPED
      CALL ERRORLOG(gc_error CLIPPED)
   END IF
   SET LOCK MODE TO NOT WAIT;
   UNLOCK TABLE ret_rech_datamart;

   EXECUTE p_upd_datamart USING gr_datamart_do.nss, m_fecha

   LET gi_rechazo = gi_rechazo + 1
   DISPLAY "REGISTROS RECHAZADOS: ", gi_rechazo USING "<<<<<<<" AT 14,1

END FUNCTION


FUNCTION provision()

    CALL f_tablas_tmp()

    CALL f_obtiene_precios_accion(HOY)

    CALL f_despliega_info_prov() RETURNING gs_procesa

    IF gs_procesa THEN

        ----- CONSULTA DE SALDOS -----
        LET gc_prepare = "EXECUTE FUNCTION fn_consulta_saldos4(?,?,?,?,?,?,?) "
        PREPARE eje_con_saldos FROM gc_prepare

        CALL primer_paso()      #-- Provisionar registros de Retiros B y C
        CALL segundo_paso()     #-- Provisionar registros de Retiros A
        CALL tercer_paso()      #-- Calcula los montos de Disposicion y Reinversion
        CALL cuarto_paso()      #-- Se afectan las tablas fisicas
    END IF

    DISPLAY "                                                                      " AT 6,1
    DISPLAY "                                                                      " AT 7,1
    DISPLAY "                                                                      " AT 8,1
    DISPLAY "                                                                      " AT 9,1
    DISPLAY "                                                                      " AT 11,1
    DISPLAY "                                                                      " AT 14,1
    DISPLAY "                                                                      " AT 15,1
    DISPLAY "                                                                      " AT 16,1
    DISPLAY "                                                                      " AT 17,1

END FUNCTION


#------------------------------------------------------------------------------#
# f_despliega_info_pre : Solicita la Fecha de Recepción de Solicitudes         #
#                        para realizar el previo.                              #
#------------------------------------------------------------------------------#
FUNCTION f_despliega_info_pre()
   DEFINE
        ls_flag,
        l_cuantos              SMALLINT

    INPUT m_fecha  WITHOUT DEFAULTS FROM fecha
       AFTER FIELD fecha
          DISPLAY "                                                                      " AT 8,1
          IF m_fecha > TODAY THEN
             ERROR "ERROR, LA FECHA NO PUEDE SER MAYOR AL DIA ACTUAL"
             NEXT FIELD fecha
          END IF

          # Validando que existan solicitudes sin procesar para esa fecha
          LET l_cuantos = 0
          SELECT COUNT(*) INTO l_cuantos
          FROM   ret_datamart_do
          WHERE  estado = 0
          AND    DATE(fecha_recep) = m_fecha

          IF l_cuantos = 0 THEN
             ERROR "NO EXISTEN REGISTROS A PROCESAR. VERIFIQUE"
             NEXT FIELD fecha
          ELSE
             DISPLAY "                                                                      " AT 8,1
             DISPLAY  "REGISTROS ENCONTRADOS: ",l_cuantos USING "<<<<<<<<<<" AT 8,1 ATTRIBUTE (REVERSE)
             LET ls_flag = TRUE
          END IF

       ON KEY (ESC)
          IF m_fecha > TODAY THEN
             ERROR "ERROR, LA FECHA NO PUEDE SER MAYOR AL DIA ACTUAL"
             NEXT FIELD fecha
          END IF

          DISPLAY "                                                                      " AT 8,1
          # Validando que existan solicitudes sin procesar para esa fecha
          LET l_cuantos = 0
          SELECT COUNT(*) INTO l_cuantos
          FROM   ret_datamart_do
          WHERE  estado = 0
          AND    DATE(fecha_recep) = m_fecha

          IF l_cuantos = 0 THEN
             ERROR "NO EXISTEN REGISTROS A PROCESAR. VERIFIQUE"
             NEXT FIELD fecha
          ELSE
             DISPLAY "                                                                      " AT 11,1
             DISPLAY  "REGISTROS ENCONTRADOS: ",l_cuantos USING "<<<<<<<<<<" AT 8,1 ATTRIBUTE (REVERSE)
             LET ls_flag = TRUE
          END IF

          EXIT INPUT

       ON KEY (CONTROL-C, INTERRUPT)
          ERROR "PROCESO CANCELADO" SLEEP 2
          ERROR ""
          EXIT PROGRAM

    END INPUT

    RETURN ls_flag
END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info_prov : Despliega la informacion de los registros a provisionar#
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info_prov()

    DEFINE lar_ret ARRAY[6] OF RECORD
        tipo_retiro        LIKE ret_trans_issste.tipo_retiro ,
        desc_retiro        LIKE tab_ret_issste.descripcion   ,
        num_cap            INTEGER                           ,
        num_prov           INTEGER
    END RECORD

    DEFINE lr_soli RECORD
        tipo_retiro        SMALLINT                                 ,
        edo_soli           LIKE ret_trans_issste.estado_solicitud   ,
        num_regs           INTEGER
    END RECORD

    DEFINE li_folio_trans  LIKE ret_trans_issste.folio

    DEFINE
        li_tot_prov         INTEGER

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    -- -----------------------------------------------------------------------------

    #CALL f_abre_ventana()

#INV-1491  --> INI
          INPUT m_fecha  WITHOUT DEFAULTS FROM fecha
             AFTER FIELD fecha
                IF m_fecha IS NULL THEN
                      ERROR "ERROR, INGRESE UNA FECHA VALIDA"
                      NEXT FIELD fecha
                END IF

                IF m_fecha > TODAY THEN
                   ERROR "ERROR, LA FECHA NO PUEDE SER MAYOR AL DIA ACTUAL"
                   NEXT FIELD fecha
                END IF

             ON KEY (ESC)
                IF m_fecha IS NULL THEN
                      ERROR "ERROR, INGRESE UNA FECHA VALIDA"
                      NEXT FIELD fecha
                END IF
                EXIT INPUT

             ON KEY (CONTROL-C, INTERRUPT)
                ERROR "PROCESO CANCELADO" SLEEP 2
                ERROR ""
                EXIT PROGRAM
          END INPUT

    WHILE TRUE

      PROMPT "¿DESEA EJECUTAR PROVISION CON ESTA FECHA? (S/N)" FOR CHAR enter
      IF enter MATCHES "[SsNn]" THEN
        IF enter MATCHES "[Ss]" THEN
           CALL f_valida_provisiona() RETURNING ls_flag, li_tot_prov

           IF ls_flag = 1 THEN
              IF li_tot_prov = 0 THEN
                 CALL f_error_msg("NO EXISTEN REGISTROS PARA PROVISIONAR")
                 LET ls_flag = 0
              ELSE
                 WHILE TRUE
                    PROMPT "SE PROVISIONARAN ", li_tot_prov, " CUENTAS. ¿ESTA SEGURO? (S/N)  " FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                       IF enter MATCHES "[sS]" THEN
                           LET ls_flag = 1
                       ELSE
                           CALL f_error_msg("PROCESO CANCELADO")
                           LET ls_flag = 0
                       END IF
                       EXIT WHILE
                    END IF
                 END WHILE
              END IF
           END IF
           EXIT WHILE
        ELSE
           ERROR "PROCESO CANCELADO" SLEEP 2
           ERROR ""
           EXIT PROGRAM
        END IF
      ELSE
        ERROR "SOLO PRESIONE S o N"
      END IF
    END WHILE
#INV-1491  --> FIN

    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# fn_obten_fec_notifica : Obtiene la fecha de inicio de notificacion de la  #
#                         resolución de negativa de pension                 #
#---------------------------------------------------------------------------#
FUNCTION fn_obten_fec_notifica(p_nss)

   DEFINE p_nss       CHAR(11)
   DEFINE v_fecha     DATE


    SELECT DATE(MAX(fecha_recep))
      INTO v_fecha
      FROM ret_datamart_do
     WHERE nss = p_nss
       AND tipo_retiro = 'D'
       AND sec_pension = (SELECT UNIQUE MAX(sec_pension)
                            FROM ret_datamart_do
                           WHERE nss = p_nss
                             AND tipo_retiro = 'D')

    RETURN v_fecha


END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Realiza la provision de aquellos nss con tipo de pension    #
#               por muerte (VI, VO, OR, AS) y de los retiros B y C          #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_transf    RECORD LIKE ret_trans_imss.*
    DEFINE lr_saldos    RECORD LIKE ret_mto_solicitud_saldo.*
    DEFINE lr_mto_vent  RECORD LIKE ret_monto_ventanilla.*

    DEFINE ls_ind_saldo_fip LIKE ret_solicitud_saldo.ind_saldo_fip

    DEFINE
        l_edo_viv           ,
        ls_movimiento       ,
        ls_subcuenta        ,
        ls_siefore          ,
        ls_ind_sip_u        SMALLINT
        
    DEFINE v_fecha_calculo  DATE
    
    -- -----------------------------------------------------------------------------

    INITIALIZE lr_mto_vent.* TO NULL
    LET ls_ind_sip_u = 0

    DISPLAY "PROVISIONANDO CUENTAS CON SALDO PREVIO     ....  " AT 6,2

    DECLARE cur_prov CURSOR FOR
    SELECT *
    FROM   safre_tmp:tmp_trans_imss
    WHERE  tipo_retiro IN ("B","C","U")

    -- Iniciamos ciclo para cada nss
    FOREACH cur_prov INTO lr_transf.*

        IF lr_transf.estado_solicitud <> gr_edo.rechazado_derecho THEN

            LET lr_mto_vent.nss             = lr_transf.nss
            LET lr_mto_vent.folio           = lr_transf.folio
            LET lr_mto_vent.consecutivo     = lr_transf.consecutivo
            LET lr_mto_vent.cod_tramite     = gr_tipo_tramite.transferencia
            LET lr_mto_vent.tipo_retiro     = lr_transf.tipo_retiro

            SELECT movimiento
            INTO   ls_movimiento
            FROM   tab_retiro
            WHERE  tipo_retiro = lr_transf.tipo_retiro

#CPL-2324 INI
            IF (lr_transf.tipo_retiro = "B" AND
                lr_transf.tipo_seguro = "RJ" AND lr_transf.tipo_prestacion = "1" AND 
               (lr_transf.tipo_pension = "AS" OR lr_transf.tipo_pension = "OR"
                OR lr_transf.tipo_pension = "VI" OR lr_transf.tipo_pension = "VO"
                OR lr_transf.tipo_pension = "IP" OR lr_transf.tipo_pension = "RE"
                OR lr_transf.tipo_pension = "CE" OR lr_transf.tipo_pension = "VE"
                OR lr_transf.tipo_pension = "IN" OR lr_transf.tipo_pension = "PP")) THEN

                LET ls_movimiento = 806

            END IF
#CPL-2324 FIN

            DECLARE cur_saldos CURSOR FOR
            SELECT subcuenta
            FROM   tab_agrupa_subcta
            WHERE  grupo = lr_transf.grupo
            ORDER BY 1

            SELECT ind_saldo_fip
            INTO   ls_ind_saldo_fip
            FROM   ret_solicitud_saldo
            WHERE  nss                  = lr_transf.nss
            AND    folio_cargo          = lr_transf.folio_sol_saldo
            AND    id_solicitud_saldo   = lr_transf.consec_sol_saldo
            AND    estado_solicitud    <> gr_edo.rechazado_sal

            IF ls_ind_saldo_fip  = 2 THEN
                CALL f_genera_tmp_cuenta(lr_transf.nss)
            END IF
            
            #DISPLAY "GEnera Historicos dis_cuenta.."

            -- Ciclo para cada subcuenta del NSS provisionado
            FOREACH cur_saldos INTO ls_subcuenta

                INITIALIZE lr_saldos.* TO NULL

                LET l_edo_viv = NULL
                LET ls_ind_sip_u = 0

                SELECT estado_sub_viv
                INTO   l_edo_viv
                FROM   ret_det_datamart
                WHERE  nss          = lr_transf.nss
                AND    id_registro  = lr_transf.consecutivo

                IF l_edo_viv = 1 AND ls_ind_saldo_fip <> 1 THEN
                    IF (ls_subcuenta = 4) OR (ls_subcuenta = 8) THEN
                         CALL f_calcula_vivienda (lr_transf.nss, ls_subcuenta, ls_ind_saldo_fip)
                    END IF
                END IF

                SELECT *
                INTO   lr_saldos.*
                FROM   ret_mto_solicitud_saldo
                WHERE  nss                  = lr_transf.nss
                AND    folio_cargo          = lr_transf.folio_sol_saldo
                AND    id_solicitud_saldo   = lr_transf.consec_sol_saldo
                AND    subcuenta            = ls_subcuenta

                IF ls_subcuenta = 4 THEN
                    LET ls_siefore  = 11
                ELSE
                    LET ls_siefore  = 10
                END IF
                
                 IF (lr_transf.tipo_retiro = "U" AND lr_transf.regimen = 73 AND
                     lr_transf.tipo_seguro = "PR" AND lr_transf.tipo_prestacion = "25" AND 
                    (lr_transf.tipo_pension = "IP" OR lr_transf.tipo_pension = "IN"
                     OR lr_transf.tipo_pension = "CE" OR lr_transf.tipo_pension = "VE")) THEN
                     	
                     	IF ls_subcuenta = 1 THEN                
                         LET ls_ind_sip_u = 1         #CPL-3628 SE agrega un indicador para saber si se paga el saldo a la fecha inicio de pensión
                      END IF

                 END IF
                 
                LET v_fecha_calculo = HOY
                
                IF lr_transf.tipo_retiro = "U" AND (lr_transf.tipo_prestacion = "22" OR
                                                    lr_transf.tipo_prestacion = "23") THEN
                   IF ls_subcuenta <> 7 THEN
                      LET v_fecha_calculo = fn_obten_fec_notifica(lr_transf.nss)
                      
                      
                      
                      IF v_fecha_calculo IS NULL THEN
                         LET v_fecha_calculo = HOY
                      END IF
                   END IF
                END IF

                -- Si no se paga a FIP se obtiene el saldo completo de la subcuenta
                IF ls_ind_saldo_fip = 1 OR lr_transf.tipo_retiro = "C" OR (lr_transf.tipo_retiro = "U" AND ls_ind_sip_u = 0) THEN
                    CALL f_saldo_dia(lr_transf.nss, ls_subcuenta, v_fecha_calculo)
                        RETURNING lr_saldos.mto_en_pesos        ,
                                  lr_saldos.mto_en_acciones
                END IF

                IF lr_saldos.mto_en_pesos IS NOT NULL THEN

                    IF ls_subcuenta = 4 THEN
                        IF l_edo_viv = 1 THEN
                            LET lr_saldos.mto_en_pesos  = lr_saldos.mto_en_acciones * gar_precio_acc[ls_siefore].precio_dia
                        ELSE
                            LET lr_saldos.mto_en_pesos      = 0
                            LET lr_saldos.mto_en_acciones   = 0
                        END IF
                    END IF

#CPL-1014 -INI
                    IF lr_transf.tipo_pension != "IP" THEN    #Incapacidad permanente
                       LET lr_transf.porcentaje_val    = 100
                    END IF

                    LET lr_saldos.mto_en_pesos    = (lr_saldos.mto_en_pesos * lr_transf.porcentaje_val)/100
                    LET lr_saldos.mto_en_acciones = (lr_saldos.mto_en_acciones * lr_transf.porcentaje_val)/100
#CPL-1014 -FIN
                    IF ls_subcuenta <> 4 THEN
                        LET lr_saldos.mto_en_acciones = lr_saldos.mto_en_pesos
                    END IF

                    IF lr_saldos.mto_en_pesos > 0 THEN
                        CALL f_provisiona_subcta(lr_transf.curp             ,
                                                 lr_transf.nss              ,
                                                 ls_subcuenta               ,
                                                 lr_transf.consecutivo      ,
                                                 lr_saldos.mto_en_acciones  ,
                                                 lr_saldos.mto_en_pesos     ,
                                                 HOY                        ,
                                                 ls_movimiento              ,
                                                 ls_siefore                 )

                        -- Complementamos la tabla de calculos para transferencias
                        LET lr_mto_vent.siefore         = ls_siefore
                        LET lr_mto_vent.subcuenta       = ls_subcuenta
                        LET lr_mto_vent.mto_acciones    = lr_saldos.mto_en_acciones
                        LET lr_mto_vent.mto_pesos       = lr_saldos.mto_en_pesos

                        IF lr_mto_vent.siefore = 11 THEN
                            LET lr_mto_vent.mto_acciones    = f_redondea_val(lr_mto_vent.mto_acciones,2)
                            LET lr_mto_vent.mto_pesos       = f_redondea_val(lr_mto_vent.mto_pesos,2)
                        END IF
                        IF lr_mto_vent.siefore = 10 OR lr_mto_vent.siefore = 11 THEN
                           INSERT INTO safre_tmp:tmp_monto_ventanilla
                           VALUES(lr_mto_vent.*)
                        END IF
                    END IF -- Saldo mayor a cero
                END IF -- Subcuenta con saldo a provisionar

            END FOREACH -- Subcuenta por NSS

            UPDATE safre_tmp:tmp_trans_imss
            SET    estado_solicitud     = gr_edo.provisionado
            WHERE  nss                  = lr_transf.nss
            AND    consecutivo          = lr_transf.consecutivo

        END IF -- Diagnostico de Rechazo

    END FOREACH -- Provision por NSS

    DISPLAY "(TERMINADO)" AT 6,51

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Realiza el calculo de los retiros A con pago de acuerdo    #
#                al monto solicitado por el IMSS                            #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE lr_transf    RECORD LIKE ret_trans_imss.*
    DEFINE lr_saldos    RECORD LIKE ret_mto_solicitud_saldo.*
    DEFINE lr_mto_vent  RECORD LIKE ret_monto_ventanilla.*

    DEFINE ls_mto_pesos_total LIKE ret_trans_imss.monto_sol_calculado --6 decimales
    DEFINE ls_mto_calculado   LIKE ret_trans_imss.monto_sol_calculado

    DEFINE lr_mto_const RECORD
        siefore             LIKE dis_cuenta.siefore             ,
        subcuenta           LIKE dis_cuenta.subcuenta           ,
        monto_pesos         LIKE dis_cuenta.monto_en_acciones   ,
        monto_acc           LIKE dis_cuenta.monto_en_acciones
    END RECORD

    DEFINE ls_ind_saldo_fip LIKE ret_solicitud_saldo.ind_saldo_fip

    DEFINE
        l_edo_viv           ,
        ls_movimiento       ,
        ls_subcuenta        ,
        ls_siefore          SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY "PROVISIONANDO CUENTAS CON MTO CONSTITUTIVO ....  " AT 7,2

    INITIALIZE lr_mto_vent.* TO NULL

    DECLARE cur_mc CURSOR FOR
    SELECT *
    FROM   safre_tmp:tmp_trans_imss
    WHERE  tipo_retiro = "A"

    -- Iniciamos ciclo para cada nss
    FOREACH cur_mc INTO lr_transf.*

        IF lr_transf.estado_solicitud <> gr_edo.rechazado_derecho THEN

            LET lr_mto_vent.nss             = lr_transf.nss
            LET lr_mto_vent.folio           = lr_transf.folio
            LET lr_mto_vent.consecutivo     = lr_transf.consecutivo
            LET lr_mto_vent.cod_tramite     = 1 -- trasferencia
            LET lr_mto_vent.tipo_retiro     = lr_transf.tipo_retiro

            SELECT movimiento
            INTO   ls_movimiento
            FROM   tab_retiro
            WHERE  tipo_retiro = lr_transf.tipo_retiro

            LET ls_mto_pesos_total  = 0
            LET ls_mto_calculado    = 0

            --Crea la tabla donde se almacenan los montos a provisionar
            CALL f_genera_tmp_mto_no_pago()

            DECLARE cur_saldos_mc CURSOR FOR
            SELECT subcuenta
            FROM   tab_agrupa_subcta
            WHERE  grupo = lr_transf.grupo
            ORDER BY 1

            SELECT ind_saldo_fip
            INTO   ls_ind_saldo_fip
            FROM   ret_solicitud_saldo
            WHERE  nss                  = lr_transf.nss
            AND    folio_cargo          = lr_transf.folio_sol_saldo
            AND    id_solicitud_saldo   = lr_transf.consec_sol_saldo
            AND    estado_solicitud    <> gr_edo.rechazado_sal

            IF ls_ind_saldo_fip  = 2 THEN
                CALL f_genera_tmp_cuenta(lr_transf.nss)
            END IF
            
            #DISPLAY "GEnera Historicos dis_cuenta.."

            -- Ciclo para cada subcuenta del NSS provisionado
            FOREACH cur_saldos_mc INTO ls_subcuenta

                INITIALIZE lr_saldos.* TO NULL

                LET l_edo_viv = NULL

                SELECT estado_sub_viv
                INTO   l_edo_viv
                FROM   ret_det_datamart
                WHERE  nss          = lr_transf.nss
                AND    id_registro  = lr_transf.consecutivo

                IF l_edo_viv = 1 AND ls_ind_saldo_fip <> 1 THEN
                    IF (ls_subcuenta = 4) OR (ls_subcuenta = 8) THEN
                         CALL f_calcula_vivienda (lr_transf.nss, ls_subcuenta, ls_ind_saldo_fip)
                    END IF
                END IF

                SELECT *
                INTO   lr_saldos.*
                FROM   ret_mto_solicitud_saldo
                WHERE  nss                  = lr_transf.nss
                AND    folio_cargo          = lr_transf.folio_sol_saldo
                AND    id_solicitud_saldo   = lr_transf.consec_sol_saldo
                AND    subcuenta            = ls_subcuenta

                IF ls_subcuenta = 4 THEN
                    LET ls_siefore  = 11
                ELSE
                    LET ls_siefore  = 10
                END IF

                IF ls_ind_saldo_fip = 1 THEN
                    CALL f_saldo_dia(lr_transf.nss, ls_subcuenta, HOY)
                        RETURNING lr_saldos.mto_en_pesos        ,
                                  lr_saldos.mto_en_acciones
                END IF

                IF lr_saldos.mto_en_pesos IS NOT NULL THEN

                    -- Req. CPL-945
                    -- Casos Tipo A,97,IM,IN. No se debera considerar el porcentaje
                    -- de valuación contenido en Datamart como se esta haciendo actualmente.
#CPL-1014 -INI
                    #Se comenta por requerimiento cpl-1014 -ACS02102012
                    {IF gs_codigo_afore = gs_coppel THEN
                        IF ((lr_transf.tipo_retiro = "A")  AND (lr_transf.regimen = 97) AND
                            (lr_transf.tipo_seguro = "IM") AND (lr_transf.tipo_pension = "IN") ) THEN
                            LET lr_transf.porcentaje_val    = 0
                        END IF
                    END IF }

                    IF lr_transf.tipo_pension != "IP" THEN
                       LET lr_transf.porcentaje_val    = 100
                    END IF
#CPL-1014 -FIN
                    IF (lr_transf.porcentaje_val > 0) AND (lr_transf.porcentaje_val IS NOT NULL) THEN
                        #Se agrega multiplicacion de acciones por % --> #CPL-1147
                        LET lr_saldos.mto_en_pesos     = (lr_saldos.mto_en_pesos * lr_transf.porcentaje_val)/100
                        LET lr_saldos.mto_en_acciones  = (lr_saldos.mto_en_acciones * lr_transf.porcentaje_val)/100
                    END IF

                    IF ls_subcuenta <> 4 OR (ls_subcuenta = 4 AND l_edo_viv = 1) THEN

                        LET ls_mto_pesos_total = ls_mto_pesos_total + lr_saldos.mto_en_pesos

                        INSERT INTO tmp_mto_no_pago VALUES(ls_siefore,
                                                           ls_subcuenta,
                                                           lr_saldos.mto_en_pesos,
                                                           lr_saldos.mto_en_acciones   #CPL-1147
                                                          )
                    END IF -- Id de vivenda aceptado
                END IF -- Subcuenta con saldo a provisionar
            END FOREACH -- Subcuenta por NSS

            -- Realizamos el calculo del Pago del monto consitutivo
            IF ls_mto_pesos_total > 0 THEN
                LET gc_prepare = "EXECUTE FUNCTION safre_af:fn_calc_constitutivo_2_5(?,?,?)"
                PREPARE eje_mto_cons FROM gc_prepare

                DECLARE c_mto_const CURSOR FOR eje_mto_cons

                FOREACH c_mto_const USING lr_transf.nss         ,
                                          lr_transf.consecutivo ,
                                          ls_mto_pesos_total
                                    INTO  lr_mto_const.siefore      ,
                                          lr_mto_const.subcuenta    ,
                                          lr_mto_const.monto_pesos

                    IF lr_mto_const.subcuenta   = 4 THEN
                       #CPL-1147 INI
                       LET ls_siefore              = lr_mto_const.siefore

                       INITIALIZE lr_saldos.* TO NULL

                       SELECT t.monto_pesos, t.monto_acciones
                        INTO lr_saldos.mto_en_pesos, lr_saldos.mto_en_acciones
                       FROM   tmp_mto_no_pago t
                       WHERE  t.subcuenta = lr_mto_const.subcuenta

                       #si no alcanza a cubrir el monto constitutivo, se calculan pesos
                       IF lr_saldos.mto_en_pesos = lr_mto_const.monto_pesos THEN
                          LET lr_mto_const.monto_pesos = lr_saldos.mto_en_acciones * gar_precio_acc[ls_siefore].precio_dia
                          LET lr_mto_const.monto_acc   = lr_saldos.mto_en_acciones
                       ELSE   #si se transfiere el monto_constitutivo, se calculan acciones
                          LET lr_mto_const.monto_acc  = lr_mto_const.monto_pesos/gar_precio_acc[ls_siefore].precio_dia
                       END IF
                       #CPL-1147 FIN

                    ELSE   #SI subcuenta != Viv(4)
                        LET lr_mto_const.monto_acc  = lr_mto_const.monto_pesos
                    END IF

                    LET ls_mto_calculado = ls_mto_calculado + lr_mto_const.monto_pesos

                    CALL f_provisiona_subcta(lr_transf.curp             ,
                                             lr_transf.nss              ,
                                             lr_mto_const.subcuenta     ,
                                             lr_transf.consecutivo      ,
                                             lr_mto_const.monto_acc     ,
                                             lr_mto_const.monto_pesos   ,
                                             HOY                        ,
                                             ls_movimiento              ,
                                             lr_mto_const.siefore       )

                    -- Complementamos la tabla de calculos para transferencias
                    LET lr_mto_vent.siefore         = lr_mto_const.siefore
                    LET lr_mto_vent.subcuenta       = lr_mto_const.subcuenta
                    LET lr_mto_vent.mto_acciones    = lr_mto_const.monto_acc
                    LET lr_mto_vent.mto_pesos       = lr_mto_const.monto_pesos

                    IF lr_mto_vent.siefore = 11 THEN
                        --LET lr_mto_vent.mto_acciones    = f_redondea_val(lr_mto_vent.mto_acciones,2)
                        LET lr_mto_vent.mto_pesos       = f_redondea_val(lr_mto_vent.mto_pesos,2)
                    END IF

                    INSERT INTO safre_tmp:tmp_monto_ventanilla
                    VALUES(lr_mto_vent.*)


                END FOREACH -- Calculo proporcional
            END IF -- Monto total mayor a cero


            UPDATE safre_tmp:tmp_trans_imss
            SET    estado_solicitud     = gr_edo.provisionado   ,
                   monto_sol_calculado  = ls_mto_calculado
            WHERE  nss                  = lr_transf.nss
            AND    consecutivo          = lr_transf.consecutivo

        END IF -- Diagnostico de Rechazo

    END FOREACH -- Provision por NSS

    DISPLAY "(TERMINADO)" AT 7,51

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Se calculan los montos que corresponden a reinversion y     #
#               disposicion a partir de lo provisionado para transferencias #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()

    DEFINE lr_trans_imss RECORD LIKE ret_trans_imss.*
    DEFINE lr_provision  RECORD LIKE dis_provision.*
    DEFINE lr_reinvierte RECORD LIKE ret_reinversion.*

    -- -----------------------------------------------------------------------------

    DISPLAY "CALCULANDO MONTOS REINVERSION Y DISPOSICION ...  " AT 8,2

    INITIALIZE lr_trans_imss.* TO NULL
    INITIALIZE lr_provision.* TO NULL
    INITIALIZE lr_reinvierte.* TO NULL

    DECLARE cur_DO CURSOR FOR
    SELECT *
    FROM   safre_tmp:tmp_trans_imss
    WHERE estado_solicitud != gr_edo.rechazado_derecho
    AND tipo_retiro NOT IN ("D","S")
    ORDER BY tipo_retiro, nss

    -- Iniciamos ciclo para cada nss
    FOREACH cur_DO INTO lr_trans_imss.*

        IF lr_trans_imss.tipo_retiro = "A" THEN
            CALL f_calculos_tipo_A(lr_trans_imss.*)
        ELSE
            CALL f_calcula_remanente(lr_trans_imss.*)
        END IF

        -- Insertamos las solicitudes de reinversion
        SELECT "OK"
        FROM   safre_tmp:tmp_monto_ventanilla
        WHERE  nss          = lr_trans_imss.nss
        AND    cod_tramite  = gr_tipo_tramite.reinversion
        GROUP BY 1

        IF STATUS <> NOTFOUND THEN
            LET lr_reinvierte.id_reinversion        = 0
            LET lr_reinvierte.id_solicitud_saldo    = lr_trans_imss.consec_sol_saldo
            LET lr_reinvierte.fecha_registro        = CURRENT YEAR TO SECOND
            LET lr_reinvierte.id_tipo_origen_reinv  = 8
            LET lr_reinvierte.estado_solicitud      = gr_edo.sol_reinversion

            INSERT INTO safre_tmp:tmp_reinversion
            VALUES(lr_reinvierte.*)

        END IF

        INITIALIZE lr_reinvierte.* TO NULL

{Se comenta ACS 01082012
        IF gs_codigo_afore <> gs_coppel THEN

            -- Insertamos las solicitudes de reinversion
            SELECT "OK"
            FROM   safre_tmp:tmp_monto_ventanilla
            WHERE  nss          = lr_trans_imss.nss
            AND    cod_tramite  = gr_tipo_tramite.disposicion
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                LET lr_reinvierte.id_reinversion        = 0
                LET lr_reinvierte.id_solicitud_saldo    = lr_trans_imss.consec_sol_saldo
                LET lr_reinvierte.fecha_registro        = CURRENT YEAR TO SECOND
                LET lr_reinvierte.id_tipo_origen_reinv  = 9
                LET lr_reinvierte.estado_solicitud      = gr_edo.sol_reinversion

                INSERT INTO safre_tmp:tmp_reinversion
                VALUES(lr_reinvierte.*)
            END IF

        END IF -- Coppel
}
        INITIALIZE lr_trans_imss.* TO NULL
    END FOREACH

-- insertar en ret_reinversion si esta marcado como 3

-- si la afore es coppel no se reinvierte disposiciones

    DISPLAY "(TERMINADO)" AT 8,51

END FUNCTION

#---------------------------------------------------------------------------#
# cuarto_paso : Se afectan las tablas fisicas del proceso una vez que se    #
#               termino correctamente                                       #
#---------------------------------------------------------------------------#
FUNCTION cuarto_paso()

    DEFINE li_folio LIKE ret_trans_imss.folio

    DEFINE
      ld_saldo_sbctas      DECIMAL (13,2)

    DEFINE
        lr_provision        ,
        lr_tmp_prov         RECORD LIKE dis_provision.*

    DEFINE lr_datos RECORD LIKE ret_trans_imss.*

    DEFINE lr_tots RECORD
        ret_a           INTEGER     ,
        ret_b           INTEGER     ,
        ret_c           INTEGER     ,
        ret_u           INTEGER     ,
        total           INTEGER
    END RECORD

    DEFINE
        ls_edo_sol          SMALLINT,
        l_query    VARCHAR(255),
        l_and      VARCHAR(100)
    DEFINE v_diagnostico   CHAR(03)
    DEFINE v_estado        SMALLINT

    LET l_query = ""   LET l_and = ""
    -- -----------------------------------------------------------------------------
    IF m_fecha IS NULL THEN
       LET l_and = " "
    ELSE
       LET l_and = "AND DATE(fecha_carga_afore) = '", m_fecha, "'"    #INV-1491
    END IF

    -- -----------------------------------------------------------------------------

    LET li_folio        = f_ultimo_folio()
    LET lr_tots.ret_a   = 0
    LET lr_tots.ret_b   = 0
    LET lr_tots.ret_c   = 0
    LET lr_tots.total   = 0

    DISPLAY "CONSOLIDANDO EN TABLAS                     ...." AT 9,2
    DISPLAY "FOLIO :   ", li_folio AT 11,16
    DISPLAY "PROVISIONADOS" AT 13,6
    DISPLAY "RETIRO A   : ", lr_tots.ret_a  AT 14,10
    DISPLAY "RETIRO B   : ", lr_tots.ret_b  AT 15,10
    DISPLAY "RETIRO C   : ", lr_tots.ret_c  AT 16,10

    -- Copiamos los calculos de trans/disp/reinv de la tabla temporal a la definitiva
    UPDATE safre_tmp:tmp_monto_ventanilla
    SET    folio = li_folio
    WHERE  folio = 1

    INSERT INTO ret_monto_ventanilla
    SELECT *
    FROM   safre_tmp:tmp_monto_ventanilla
    WHERE  folio = li_folio

    -- Copiamos la provision de la tabla temporal a la definitiva
    UPDATE safre_tmp:tmp_prov_derecho
    SET    folio = li_folio
    WHERE  folio = 1

    INSERT INTO dis_provision
    SELECT *
    FROM   safre_tmp:tmp_prov_derecho
    WHERE  folio = li_folio

    -- Copiamos la tabla de transferencias temporal a la definitiva
    UPDATE safre_tmp:tmp_trans_imss
    SET    folio    = li_folio
    WHERE  folio    = 1

    INSERT INTO ret_trans_imss
    SELECT *
    FROM   safre_tmp:tmp_trans_imss
    WHERE  folio    = li_folio
    AND    tipo_retiro NOT IN ("D","S")
    AND    grupo <> 0

    -- Actualiza el folio y el estado de la solicitud de los registros provisionados
    DECLARE cur_soli CURSOR FOR
    SELECT *
    FROM   safre_tmp:tmp_trans_imss
--    WHERE  estado_solicitud = gr_edo.provisionado
    ORDER BY nss

    #actualiza el folio para que se puedan notificar
    #-- jgj Verificar para proceso definitivo!

    UPDATE ret_det_datamart SET estado = 118,         --liquidado
                                folio = li_folio
    WHERE tipo_retiro IN ("D","S")
    AND estado = 108   --derecho otorgado
    AND DATE(fecha_carga_afore) = m_fecha    #INV-1491

    FOREACH cur_soli INTO lr_datos.*

       SELECT NVL(ABS(SUM(monto_en_acciones)),0) INTO ld_saldo_sbctas
       FROM dis_provision
       WHERE nss = lr_datos.nss
       AND folio = li_folio
       AND subcuenta IN (1,2,4,5,6,7,9)
       AND tipo_movimiento <> gs_mov_viv97

       IF ld_saldo_sbctas <= 0 THEN
          
          INITIALIZE v_diagnostico, v_estado TO NULL
          SELECT diagnostico,
                 estado
            INTO v_diagnostico,
                 v_estado
            FROM ret_det_datamart
           WHERE nss              = lr_datos.nss
             AND id_registro      = lr_datos.consecutivo
          
          IF v_diagnostico = '501' THEN
             UPDATE ret_det_datamart
             SET    diagnostico      = 509, estado = gr_edo.rechazado_derecho
--          SET    diagnostico      = 502, estado = gr_edo.rechazado_derecho  --- CPL-2578 
             WHERE  nss              = lr_datos.nss
             AND    id_registro      = lr_datos.consecutivo
             AND    diagnostico      = 501
             AND    estado           = gr_edo.derecho_otorgado
          
             IF SQLCA.SQLERRD[3] > 0 THEN
                 CALL f_gen_reinv_por_rechazo (lr_datos.*)
             END IF

             # CPL-1702 INI Se inserta en el historico de diagnosticos - CAT
             CALL f_inserta_his_diagnostico (0,lr_datos.consecutivo, lr_datos.nss, gr_edo.derecho_otorgado, 501)
             # CPL-1702 FIN
          ELSE
             CALL f_inserta_his_diagnostico (0,lr_datos.consecutivo, lr_datos.nss, v_estado, v_diagnostico)
          END IF
       END IF

-- Actualizamos la tabla de solicitudes de transferencias

        UPDATE ret_det_datamart
        SET    folio            = li_folio              ,
               estado           = gr_edo.provisionado
        WHERE  nss              = lr_datos.nss
        AND    id_registro      = lr_datos.consecutivo
        AND    estado           = gr_edo.derecho_otorgado

        CASE lr_datos.tipo_retiro
            WHEN "A"
                LET lr_tots.ret_a = lr_tots.ret_a + 1
                DISPLAY "RETIRO A   : ", lr_tots.ret_a  AT 14,10
            WHEN "B"
                LET lr_tots.ret_b = lr_tots.ret_b + 1
                DISPLAY "RETIRO B   : ", lr_tots.ret_b  AT 15,10
            WHEN "C"
                LET lr_tots.ret_c = lr_tots.ret_c + 1
                DISPLAY "RETIRO C   : ", lr_tots.ret_c  AT 16,10
            WHEN "U"
                LET lr_tots.ret_u = lr_tots.ret_u + 1
                DISPLAY "RETIRO U   : ", lr_tots.ret_u  AT 17,10
        END CASE

    END FOREACH

    CALL f_actualiza_rechazos (li_folio)

    DISPLAY "(TERMINADO)" AT 9,51
    PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter
    #CLOSE WINDOW RETC7381

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_provisiona : Verifica si existen registros a provisionar. En     #
#                       caso de existir la funcion pasa las solicitudes a   #
#                       la tabla temporal y regresa el numero de solicitudes#
#---------------------------------------------------------------------------#
FUNCTION f_valida_provisiona()

    DEFINE lr_datos_saldo RECORD
        id_solicitud_saldo  LIKE ret_solicitud_saldo.id_solicitud_saldo ,
        folio_cargo         LIKE ret_solicitud_saldo.folio_cargo
    END RECORD

    DEFINE lr_trans RECORD LIKE ret_trans_imss.*

    DEFINE
        li_tot_soli         INTEGER

    DEFINE
        ls_id_saldo_fip     ,
        ls_aseguradora      ,
        ls_procesa          SMALLINT

    DEFINE
        lc_query            CHAR(700)   ,
        lc_cve_destino      CHAR(1)

    DEFINE
        ld_fip_saldo        DATE,
        l_and               VARCHAR(100)

    -- -----------------------------------------------------------------------------

    LET li_tot_soli = 0

    LET l_and = ""
    IF m_fecha IS NULL THEN
       LET l_and = ""
    ELSE
       LET l_and = " AND DATE(fecha_carga_afore) ='",m_fecha,"'"       #INV-1491
    END IF

     -- Pasamos todas las solicitudes en estado "Solicitud Saldo"  y rechazos a la tabla temporal
    LET lc_query = ""
    LET lc_query =
    "INSERT INTO safre_tmp:tmp_trans_imss ",
    "SELECT 1         ,",                --folio
    " nss             ,",
    " id_registro     ,",                --consecutivo
    " curp            ,",
    " sec_pension     ,",
    " tipo_retiro     ,",
    " regimen         ,",
    " tipo_seguro     ,",
    " tipo_pension    ,",
    " tipo_prestacion ,",
    " porcentaje_val  ,",
    " fecha_ini_pen   ,",
    " 0               ,",                --folio_sol_saldo
    " 0               ,",                --consec_sol_saldo
    " monto_sol_imss  ,",
    " 0               ,",                --mto_const_calculado
    " 0               ,",                --grupo
    " USER            ,",                --usuario
    " estado           ",                --estado_solicitud
    " FROM   ret_det_datamart ",
    " WHERE  estado       = ",gr_edo.derecho_otorgado,
    " AND    tipo_retiro IN ('A','B','C','D','S','U')",
    " AND    folio IS NULL ",
    " AND diagnostico IN (501) ",
    l_and CLIPPED
    LET lc_query = lc_query CLIPPED
    #DISPLAY "query ", lc_query
    PREPARE pre_dt_mart FROM lc_query
    EXECUTE pre_dt_mart

    LET lc_query = ""
    LET lc_query =
    "INSERT INTO safre_tmp:tmp_trans_imss ",
    " SELECT 1         ,",                --folio
    " nss             ,",
    " id_registro     ,",                --consecutivo
    " curp            ,",
    " sec_pension     ,",
    " tipo_retiro     ,",
    " regimen         ,",
    " tipo_seguro     ,",
    " tipo_pension    ,",
    " tipo_prestacion ,",
    " porcentaje_val  ,",
    " fecha_ini_pen   ,",
    " 0               ,",                --folio_sol_saldo
    " 0               ,",                --consec_sol_saldo
    " monto_sol_imss  ,",
    " 0               ,",                --mto_const_calculado
    " 0               ,",                --grupo
    " USER            ,",                --usuario
    " estado           ",                --estado_solicitud
    " FROM   ret_rech_datamart ",
    " WHERE  estado       = ",gr_edo.rechazado_derecho,
    " AND    folio IS NULL ",
    l_and CLIPPED
    LET lc_query = lc_query CLIPPED
    #DISPLAY "query ", lc_query
    PREPARE pre_dt_mart1 FROM lc_query
    EXECUTE pre_dt_mart1

    LET lc_query = ""
    LET lc_query =
    "INSERT INTO safre_tmp:tmp_trans_imss ",
    " SELECT 1         ,",                --folio
    " nss             ,",
    " id_registro     ,",                --consecutivo
    " curp            ,",
    " sec_pension     ,",
    " tipo_retiro     ,",
    " regimen         ,",
    " tipo_seguro     ,",
    " tipo_pension    ,",
    " tipo_prestacion ,",
    " porcentaje_val  ,",
    " fecha_ini_pen   ,",
    " 0               ,",                --folio_sol_saldo
    " 0               ,",                --consec_sol_saldo
    " monto_sol_imss  ,",
    " 0               ,",                --mto_const_calculado
    " 0               ,",                --grupo
    " USER            ,",                --usuario
    " estado           ",                --estado_solicitud
    " FROM   ret_det_datamart ",
    " WHERE  diagnostico IN (502,505,509)",
    " AND    folio IS NULL ",
    l_and CLIPPED
    LET lc_query = lc_query CLIPPED
    #DISPLAY "query ", lc_query
    PREPARE pre_dt_mart2 FROM lc_query
    EXECUTE pre_dt_mart2

    SELECT COUNT(*)
    INTO   li_tot_soli
    FROM   safre_tmp:tmp_trans_imss

    IF li_tot_soli <= 0 THEN
        CALL f_error_msg("NO EXISTEN REGISTROS PARA PROVISIONAR")
        LET ls_procesa  = 0
    ELSE
        DECLARE cur_sol_saldo CURSOR FOR
        SELECT *
        FROM   safre_tmp:tmp_trans_imss
        ORDER BY nss

        FOREACH cur_sol_saldo INTO lr_trans.*

            SELECT clave_aseguradora
            INTO   ls_aseguradora
            FROM   ret_det_datamart
            WHERE  nss              = lr_trans.nss
            AND    id_registro      = lr_trans.consecutivo


            IF lr_trans.tipo_retiro = "A" THEN
                IF (gs_codigo_afore = gs_metlife) OR (ls_aseguradora = 999) THEN
                    LET lc_cve_destino = "I"
                ELSE
                    LET lc_cve_destino = "A"
                END IF
            ELSE
                LET lc_cve_destino = "G"
            END IF

            IF lr_trans.tipo_retiro = "U" THEN
                LET lc_cve_destino = "I"
            END IF

#Se cambia a estado 106  ACS -- 28-jun-2012
            LET lc_query = " SELECT id_solicitud_saldo           , ",
                           "        CAST(fecha_ini_pen AS DATE)  , ",
                           "        ind_saldo_fip                , ",
                           "        MAX(folio_cargo)               ",
                           " FROM   ret_solicitud_saldo            ",
                           " WHERE  nss               = ?          ",
                           " AND    estado_solicitud  = ?          ",
                           " GROUP BY 1,2,3                        "

            PREPARE prp_sol_saldo FROM lc_query
            EXECUTE prp_sol_saldo USING lr_trans.nss            ,
                                        gr_edo.liquida_saldo
                                  INTO  lr_trans.consec_sol_saldo       ,
                                        ld_fip_saldo                    ,
                                        ls_id_saldo_fip                 ,
                                        lr_trans.folio_sol_saldo

            #LET lr_trans.estado_solicitud = 0

            IF ls_id_saldo_fip = 2 THEN
                IF ld_fip_saldo <> lr_trans.fecha_ini_pen THEN

                    LET lr_trans.estado_solicitud = gr_edo.rechazado_derecho

                    #CPL-1618 INI
                    UPDATE ret_det_datamart SET diagnostico = 505
                    WHERE  nss              = lr_trans.nss
                    AND    id_registro      = lr_trans.consecutivo
                    AND    diagnostico = 501
                    #CPL-1618 FIN

                    # CPL-1702 INI Se inserta en el historico de diagnosticos - CAT
                    CALL f_inserta_his_diagnostico (0,lr_trans.consecutivo, lr_trans.nss, gr_edo.derecho_otorgado, 501)
                    # CPL-1702 FIN

                END IF
            END IF

            SELECT UNIQUE grupo
            INTO   lr_trans.grupo
            FROM   ret_matriz_derecho
            WHERE  tipo_retiro      = lr_trans.tipo_retiro
            AND    regimen          = lr_trans.regimen
            AND    tipo_seguro      = lr_trans.tipo_seguro
            AND    tipo_pension     = lr_trans.tipo_pension
            AND    tipo_prestacion  = lr_trans.tipo_prestacion
            AND    cve_destino      = lc_cve_destino

            UPDATE safre_tmp:tmp_trans_imss
            SET    grupo            = lr_trans.grupo                ,
                   folio_sol_saldo  = lr_trans.folio_sol_saldo      ,
                   estado_solicitud = lr_trans.estado_solicitud     ,
                   consec_sol_saldo = lr_trans.consec_sol_saldo
            WHERE  nss              = lr_trans.nss
            AND consecutivo         = lr_trans.consecutivo        #CPL-1734

        END FOREACH

        LET ls_procesa  = 1
    END IF

    RETURN ls_procesa, li_tot_soli

END FUNCTION

#---------------------------------------------------------------------------#
# f_provisiona_subcta : Inserta los montos que se usaran en la provision    #
#                       en la tabla temporal de provisiones                 #
#---------------------------------------------------------------------------#
FUNCTION f_provisiona_subcta(pr_provi, ps_sie)

    DEFINE pr_provi RECORD
        curp        LIKE ret_trans_issste.curp          ,
        nss         LIKE ret_trans_issste.nss           ,
        subcta      SMALLINT                            ,
        consec      LIKE ret_trans_issste.consecutivo   ,
        acciones    DECIMAL(16,6)                       ,
        pesos       DECIMAL(16,6)                       ,
        fecha_proc  DATE                                ,
        tipo_mov    SMALLINT
    END RECORD

    DEFINE lr_provision RECORD LIKE dis_provision.*

    DEFINE
        ps_sie              SMALLINT

    DEFINE
        ldt_fec_proc        DATE

    -- -----------------------------------------------------------------------------

    IF (pr_provi.subcta = 4) OR (pr_provi.subcta = 8) THEN
        LET ldt_fec_proc        = gdt_fecha_viv
        LET pr_provi.pesos      = f_redondea_val(pr_provi.pesos,2)
        --LET pr_provi.acciones   = f_redondea_val(pr_provi.acciones,2)

        -- Se actualiza el movimiento de vivienda para los retiros B
        IF pr_provi.tipo_mov = 810 THEN
            LET pr_provi.tipo_mov = gs_mov_viv97
        END IF
    ELSE
        LET ldt_fec_proc        = pr_provi.fecha_proc
    END IF

    LET lr_provision.tipo_movimiento    = pr_provi.tipo_mov
    LET lr_provision.subcuenta          = pr_provi.subcta
    LET lr_provision.siefore            = ps_sie
    LET lr_provision.folio              = 1
    LET lr_provision.consecutivo_lote   = pr_provi.consec
    LET lr_provision.nss                = pr_provi.nss
    LET lr_provision.curp               = pr_provi.curp
    LET lr_provision.folio_sua          = NULL
    LET lr_provision.fecha_pago         = HOY
    LET lr_provision.fecha_valor        = ldt_fec_proc
    LET lr_provision.fecha_conversion   = HOY
    LET lr_provision.monto_en_pesos     = -pr_provi.pesos
    LET lr_provision.monto_en_acciones  = -pr_provi.acciones
    LET lr_provision.precio_accion      = gar_precio_acc[ps_sie].precio_dia
    LET lr_provision.dias_cotizados     = 0
    LET lr_provision.sucursal           = ""
    LET lr_provision.id_aportante       = "RETIRO"
    LET lr_provision.estado             = gr_edo.provisionado
    LET lr_provision.fecha_proceso      = HOY
    LET lr_provision.usuario            = gc_usuario
    LET lr_provision.fecha_archivo      = HOY
    LET lr_provision.etiqueta           = 1

    INSERT INTO safre_tmp:tmp_prov_derecho
    VALUES (lr_provision.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_calculos_tipo_A : Determina los montos a pagar de Disposiciones y       #
#                     reinversion para los tipos de retiro A                #
#---------------------------------------------------------------------------#
FUNCTION f_calculos_tipo_A(pr_transferencia)

    DEFINE pr_transferencia RECORD LIKE ret_trans_imss.*

    DEFINE lr_mto_sol_saldo RECORD LIKE ret_mto_solicitud_saldo.*

    DEFINE
        lr_mto_disposicion  ,
        lr_mto_reinversion  ,
        lr_mto_transf       RECORD LIKE ret_monto_ventanilla.*

    DEFINE lr_saldo RECORD
        nss             LIKE ret_det_datamart.nss           ,
        subcuenta       SMALLINT                            ,
        grupo           SMALLINT                            ,
        fecha           DATE
    END RECORD

    DEFINE lr_montos RECORD
        subcuenta           SMALLINT     ,
        siefore             SMALLINT     ,
        monto_acc           LIKE dis_cuenta.monto_en_acciones,
        monto_pesos         LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE
        lc_saldo_dia            CHAR(100)

    DEFINE
        ls_subcta               SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_mto_disposicion.* TO NULL
    INITIALIZE lr_mto_reinversion.* TO NULL
    INITIALIZE lr_mto_transf.* TO NULL
    INITIALIZE lr_montos.* TO NULL

    -- Para el tipo A

    -- Si no se paga el Monto constitutivo se marca el remanente del pago de FIP para reinversion

    -- Si se paga el Monto constitutivo
    --   Si el tipo de Pension <> IP : se marca el remanente del pago para Disposicion
    --   Si el tipo de Pension = IP : El faltante para sumar el pago FIP se marca para disposicion
    --                                El remanente se marca para reinversion

    CALL f_calcula_remanente(pr_transferencia.*)

    IF pr_transferencia.tipo_pension = "IP" THEN

        -- Tenemos calculado la transferencia y la reinversion provisional
        -- Calculamos disposicion y reinviersion final

        DECLARE cur_tipo_A CURSOR FOR
        SELECT *
        FROM   safre_tmp:tmp_monto_ventanilla
        WHERE  nss          = pr_transferencia.nss
        AND    siefore     <> 11
        AND    cod_tramite  = gr_tipo_tramite.transferencia
        ORDER BY subcuenta

        FOREACH cur_tipo_A INTO lr_mto_disposicion.*

            INITIALIZE lr_mto_reinversion.* TO NULL
            INITIALIZE lr_mto_sol_saldo.* TO NULL

            SELECT NVL(mto_en_pesos, 0)
            INTO   lr_mto_sol_saldo.mto_en_pesos
            FROM   ret_mto_solicitud_saldo
            WHERE  nss          = pr_transferencia.nss
            AND    folio_cargo  = pr_transferencia.folio_sol_saldo
            AND    subcuenta    = lr_mto_disposicion.subcuenta

            SELECT NVL(mto_pesos, 0)
            INTO   lr_mto_reinversion.mto_pesos
            FROM   safre_tmp:tmp_monto_ventanilla
            WHERE  nss          = pr_transferencia.nss
            AND    siefore     <> 11
            AND    cod_tramite  = gr_tipo_tramite.reinversion
            AND    subcuenta    = lr_mto_disposicion.subcuenta

            LET lr_mto_disposicion.cod_tramite  = gr_tipo_tramite.disposicion
            LET lr_mto_disposicion.mto_pesos    = lr_mto_sol_saldo.mto_en_pesos - lr_mto_disposicion.mto_pesos
            LET lr_mto_disposicion.mto_acciones = lr_mto_disposicion.mto_pesos

            INSERT INTO safre_tmp:tmp_monto_ventanilla
            VALUES (lr_mto_disposicion.*)

            LET lr_mto_reinversion.mto_pesos    = lr_mto_reinversion.mto_pesos - lr_mto_disposicion.mto_pesos
            LET lr_mto_reinversion.mto_acciones = lr_mto_reinversion.mto_pesos

            IF lr_mto_reinversion.mto_pesos > 0 THEN
                UPDATE safre_tmp:tmp_monto_ventanilla
                SET    mto_pesos    = lr_mto_reinversion.mto_pesos      ,
                       mto_acciones = lr_mto_reinversion.mto_acciones
                WHERE  nss          = pr_transferencia.nss
                AND    siefore     <> 11
                AND    cod_tramite  = gr_tipo_tramite.reinversion
                AND    subcuenta    = lr_mto_disposicion.subcuenta
            ELSE
                DELETE
                FROM   safre_tmp:tmp_monto_ventanilla
                WHERE  nss          = pr_transferencia.nss
                AND    siefore     <> 11
                AND    cod_tramite  = gr_tipo_tramite.reinversion
                AND    subcuenta    = lr_mto_disposicion.subcuenta
            END IF

        END FOREACH

    END IF -- Tipo de Pension = IP

END FUNCTION
#---------------------------------------------------------------------------#
# f_calcula_remanente : Determina los montos a pagar de Disposiciones y     #
#                       Reinversion de acuerdo al tipo de retiro            #
#---------------------------------------------------------------------------#
FUNCTION f_calcula_remanente(pr_transferencia)

    DEFINE pr_transferencia RECORD LIKE ret_trans_imss.*

    DEFINE
        lr_mto_vent         ,
        lr_mto_transf       RECORD LIKE ret_monto_ventanilla.*

    DEFINE lr_saldo RECORD
        nss             LIKE ret_det_datamart.nss           ,
        subcuenta       SMALLINT                            ,
        grupo           SMALLINT                            ,
        fecha           DATE
    END RECORD

    DEFINE lr_montos RECORD
        subcuenta           SMALLINT     ,
        siefore             SMALLINT     ,
        monto_acc           LIKE dis_cuenta.monto_en_acciones,
        monto_pesos         LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE
        lc_saldo_dia            CHAR(100)

    DEFINE
        ls_subcta               SMALLINT
    -- -----------------------------------------------------------------------------

    INITIALIZE lr_mto_vent.* TO NULL
    INITIALIZE lr_mto_transf.* TO NULL
    INITIALIZE lr_montos.* TO NULL

    -- Para el tipo B se marca para reinversion los montos de Ret 97 y los montos
    -- remanentes en caso de haber pagado a la FIP

    -- Para el tipo C solo se marca para disposicion los montos de Sar 92 y Viv 92

    LET lr_mto_vent.nss             = pr_transferencia.nss
    LET lr_mto_vent.folio           = pr_transferencia.folio
    LET lr_mto_vent.consecutivo     = pr_transferencia.consecutivo
    LET lr_mto_vent.tipo_retiro     = "X"

    IF pr_transferencia.estado_solicitud = gr_edo.rechazado_derecho THEN
        LET lr_mto_vent.cod_tramite = gr_tipo_tramite.reinversion
    ELSE
        IF (pr_transferencia.tipo_retiro = "C" OR pr_transferencia.tipo_retiro = "U") OR
           (pr_transferencia.tipo_retiro = "A" AND pr_transferencia.tipo_pension <> "IP" ) THEN
            LET lr_mto_vent.cod_tramite     = gr_tipo_tramite.disposicion
        ELSE
            LET lr_mto_vent.cod_tramite     = gr_tipo_tramite.reinversion
        END IF
    END IF

    -- ciclamos sobre las cuentas restantes para pagar el remanente.
    LET lr_saldo.nss        = pr_transferencia.nss
    LET lr_saldo.subcuenta  = 0
    LET lr_saldo.grupo      = 0
    LET lr_saldo.fecha      = HOY

    LET lc_saldo_dia = " EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
    PREPARE eje_saldo_rem FROM lc_saldo_dia
    DECLARE cur_rem CURSOR FOR eje_saldo_rem

    FOREACH cur_rem USING lr_saldo.*
                    INTO  lr_montos.*

        LET lr_mto_vent.mto_pesos       = 0
        LET lr_mto_vent.mto_acciones    = 0

        -- Calculamos los montos de remanente para vivienda y montos desinvertidos.
        IF (lr_montos.siefore = 10) OR (lr_montos.siefore = 11) THEN

            IF (pr_transferencia.tipo_retiro <> "B") OR
               (pr_transferencia.tipo_retiro = "B" AND lr_montos.siefore <> 11) THEN

                IF (lr_mto_vent.cod_tramite = gr_tipo_tramite.disposicion) OR
                   (lr_mto_vent.cod_tramite = gr_tipo_tramite.reinversion AND lr_montos.siefore <> 11) THEN

                    LET lr_mto_vent.siefore         = lr_montos.siefore
                    LET lr_mto_vent.subcuenta       = lr_montos.subcuenta

                    SELECT *
                    INTO   lr_mto_transf.*
                    FROM   safre_tmp:tmp_monto_ventanilla
                    WHERE  nss          = pr_transferencia.nss
                    AND    cod_tramite  = gr_tipo_tramite.transferencia
                    AND    subcuenta    = lr_montos.subcuenta

                    IF lr_mto_transf.mto_pesos IS NULL THEN
                        LET lr_mto_transf.mto_pesos = 0
                    END IF

                    IF lr_mto_transf.mto_acciones IS NULL THEN
                        LET lr_mto_transf.mto_acciones = 0
                    END IF

                    LET lr_mto_vent.mto_pesos       = lr_montos.monto_pesos - lr_mto_transf.mto_pesos
                    LET lr_mto_vent.mto_acciones    = lr_montos.monto_acc - lr_mto_transf.mto_acciones

                    IF lr_mto_vent.mto_pesos > 0 THEN

                        IF lr_mto_vent.siefore = 11 THEN
                            LET lr_mto_vent.mto_acciones = f_redondea_val(lr_mto_vent.mto_acciones,2)
                        END IF

                        INSERT INTO safre_tmp:tmp_monto_ventanilla
                        VALUES(lr_mto_vent.*)
                    END IF
                END IF -- Disposicion o Reinversion con subcta <> 4
            END IF  -- subcuenta 4
        END IF -- siefore 10 y 11

        INITIALIZE lr_montos.* TO NULL
        INITIALIZE lr_mto_transf.* TO NULL

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  provision de transferencias                              #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW RETC7381 AT 2,2 WITH FORM "RETC7381" ATTRIBUTE(BORDER)
    DISPLAY " RETC738                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "             EJECUCION DE PROVISIONES CON DERECHO OTORGADO                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 1,58 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(76)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR  "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_ultimo_folio : Obtiene el ultimo folio que se usara para procesar los   #
#                  registros de disposiciones                               #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_folio()

    DEFINE
        li_ult_folio     INTEGER


    SELECT MAX(A.folio) + 1
    INTO   li_ult_folio
    FROM   glo_folio A

    INSERT INTO glo_folio
    VALUES (li_ult_folio)

    RETURN li_ult_folio


END FUNCTION

#---------------------------------------------------------------------------#
# f_redondea_val : Redondea el monto dado por p_monto_redondear a tantos    #
#                  como se indique en p_redondea                            #
#---------------------------------------------------------------------------#
FUNCTION f_redondea_val(p_monto_redondear, p_redondea)

    DEFINE
        p_monto_redondear DECIMAL(16,6)

    DEFINE
        p_redondea       SMALLINT


    DEFINE
        ls_monto_return   DECIMAL(16,2)

    PREPARE round_id FROM "SELECT NVL(ROUND(?, ?),0) FROM systables WHERE tabid=1"
    DECLARE round_cur CURSOR FOR round_id

    OPEN round_cur USING p_monto_redondear, p_redondea
    FETCH round_cur INTO ls_monto_return

    CLOSE round_cur

    RETURN ls_monto_return


END FUNCTION

#---------------------------------------------------------------------------#
# f_saldo_dia : Obtiene el saldo actual de la subcuenta en la fecha indicada#
#               En caso de que el monto no corresponda a la siefore 10 o 11 #
#               se regresa el monto en ceros para evitar que se considere   #
#               incorrectamente en la provision                             #
#---------------------------------------------------------------------------#
FUNCTION f_saldo_dia(pr_datos)

    DEFINE pr_datos RECORD
        nss             LIKE ret_det_datamart.nss       ,
        subcuenta       SMALLINT                        ,
        fecha           DATE
    END RECORD

    DEFINE
        lc_prp_saldo            CHAR(200)

    DEFINE lr_saldo RECORD
        nss             LIKE ret_det_datamart.nss           ,
        subcuenta       SMALLINT                            ,
        grupo           SMALLINT                            ,
        fecha           DATE
    END RECORD

    DEFINE lr_montos RECORD
        subcuenta           SMALLINT     ,
        siefore             SMALLINT     ,
        monto_acc           LIKE dis_cuenta.monto_en_acciones,
        monto_pesos         LIKE dis_cuenta.monto_en_pesos
    END RECORD

    -- -----------------------------------------------------------------------------

    LET lr_saldo.nss            =  pr_datos.nss
    LET lr_saldo.subcuenta      =  pr_datos.subcuenta
    LET lr_saldo.grupo          =  0
    LET lr_saldo.fecha          =  pr_datos.fecha

    INITIALIZE lr_montos.* TO NULL

    LET lc_prp_saldo = " EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prp_saldo

    DECLARE cur_saldo CURSOR FOR eje_saldo_dia
    --OPEN cur_saldo USING lr_saldo.*
    --FETCH cur_saldo INTO lr_montos.*
    --CLOSE cur_saldo
      
      FOREACH cur_saldo USING lr_saldo.*   #cpl-2265
                        INTO  lr_montos.*
         IF (lr_montos.siefore <> 10) AND (lr_montos.siefore <> 11) THEN
            LET lr_montos.monto_pesos   = 0
            LET lr_montos.monto_acc     = 0
         ELSE
            EXIT FOREACH
         END IF
      END FOREACH
    
    RETURN lr_montos.monto_pesos    ,
           lr_montos.monto_acc

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion a la fecha dada  #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(fecha_precios)

    DEFINE #loc #date
        fecha_precios         DATE

    DEFINE #loc #char
        v_precios_accion      CHAR(100)

    DEFINE lr_precio_acc RECORD #loc #lr_precio_acc Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE #loc #smallint
        ls_sie                SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion(?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.siefore = gs_sieviv THEN

            LET ls_sie = gs_sieviv

            SELECT 0                ,
                   fecha_valuacion  ,
                   codigo_siefore   ,
                   precio_del_dia
            INTO   gar_precio_acc[ls_sie].*
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = gdt_fecha_viv
            AND    codigo_siefore  = gs_sieviv

            IF STATUS = NOTFOUND THEN
                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", gdt_fecha_viv USING "DD/MM/yyyy",
                                 " -- SIEFORE ", gs_sieviv CLIPPED
                PROMPT lc_mensaje FOR CHAR enter
                EXIT PROGRAM
            END IF
        ELSE

            IF lr_precio_acc.estado <> 0 THEN
                LET lc_siefore = lr_precio_acc.siefore

                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                                 " -- SIEFORE ", lc_siefore CLIPPED

                PROMPT lc_mensaje FOR CHAR enter
                EXIT PROGRAM
            ELSE
                LET ls_sie                    = lr_precio_acc.siefore
                LET gar_precio_acc[ls_sie].*  = lr_precio_acc.*
            END IF

        END IF

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_tmp_mto_no_pago : Crea la tabla temporal donde se almacenan los  #
#                            montos a pagar antes de realizar el            #
#                            proporcional del monto solicitado por el IMSS  #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_mto_no_pago()

#CPL-1147 INI   #Se agrega columna de acciones
    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_mto_no_pago
        CREATE TEMP TABLE tmp_mto_no_pago
        (
             siefore           SMALLINT        ,
             subcuenta         SMALLINT        ,
             monto_pesos       DECIMAL(20,6)   ,
             monto_acciones    DECIMAL(20,6)
        )
    WHENEVER ERROR STOP
#CPL-1147 FIN

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_trans_imss
        DROP TABLE tmp_reinversion
        DROP TABLE tmp_prov_derecho
        DROP TABLE tmp_monto_ventanilla
        DROP TABLE tmp_mto_sol_saldo
    WHENEVER ERROR STOP

    --------------------------------

    SELECT *
    FROM   safre_af:ret_monto_ventanilla
    WHERE  0 = 1
    INTO TEMP tmp_monto_ventanilla

    --------------------------------
#Se cambia forma de crear la tabla ACS 01082012
CREATE TEMP  table tmp_reinversion
  (
    id_reinversion       SMALLINT ,
    id_solicitud_saldo   DECIMAL(11,0),
    fecha_registro       DATETIME YEAR TO SECOND,
    id_tipo_origen_reinv SMALLINT,
    estado_solicitud     SMALLINT

  ) WITH NO LOG
{
    SELECT *
    FROM   safre_af:ret_reinversion
    WHERE  0 = 1
    INTO TEMP tmp_reinversion
}
    --------------------------------

    CREATE TABLE tmp_trans_imss
      (
        folio                   INTEGER                 ,
        nss                     char(11)                ,
        consecutivo             decimal(11,0) not null  ,
        curp                    char(18)                ,
        sec_pension             char(2)                 ,
        tipo_retiro             char(1)                 ,
        regimen                 char(2)                 ,
        tipo_seguro             char(2)                 ,
        tipo_pension            char(2)                 ,
        tipo_prestacion         smallint                ,
        porcentaje_val          DECIMAL(5,2)            ,
        fecha_ini_pen           DATE                    ,
        folio_sol_saldo         INTEGER                 ,
        consec_sol_saldo        decimal(11,0) not null  ,
        monto_sol_imss          decimal(15,2)           ,
        monto_sol_calculado     decimal(16,6)           ,
        grupo                   smallint                ,
        usuario                 char(30)                ,
        estado_solicitud        smallint
      )

    GRANT ALL ON tmp_trans_imss TO PUBLIC

    --------------------------------

    CREATE TABLE tmp_prov_derecho (
     tipo_movimiento    SMALLINT NOT NULL       ,
     subcuenta          SMALLINT NOT NULL       ,
     siefore            SMALLINT                ,
     folio              DECIMAL(10,0) NOT NULL  ,
     consecutivo_lote   INTEGER                 ,
     nss                CHAR(11) NOT NULL       ,
     curp               CHAR(18)                ,
     folio_sua          CHAR(6)                 ,
     fecha_pago         DATE                    ,
     fecha_valor        DATE                    ,
     fecha_conversion   DATE                    ,
     monto_en_pesos     DECIMAL(16,6)           ,
     monto_en_acciones  DECIMAL(16,6)           ,
     precio_accion      DECIMAL(16,6)           ,
     dias_cotizados     INTEGER                 ,
     sucursal           CHAR(10)                ,
     id_aportante       CHAR(11)                ,
     estado             SMALLINT                ,
     fecha_proceso      DATE                    ,
     usuario            CHAR(8)                 ,
     fecha_archivo      DATE                    ,
     etiqueta           INTEGER
      )

    CREATE INDEX tmp_prov_derecho_1 ON tmp_prov_derecho
        (folio,subcuenta,tipo_movimiento,estado)

    CREATE INDEX tmp_prov_derecho_2 ON tmp_prov_derecho
        (nss)

    CREATE INDEX tmp_prov_derecho_3 ON tmp_prov_derecho
        (folio,subcuenta)

    GRANT ALL ON tmp_prov_derecho TO PUBLIC

    --------------------------------

    CREATE TABLE tmp_mto_sol_saldo
      (
        id_solicitud_saldo  DECIMAL(11,0)   ,
        siefore             SMALLINT        ,
        subcuenta           SMALLINT        ,
        folio_cargo         DECIMAL(11,0)   ,
        nss                 CHAR(11)        ,
        curp                CHAR(18)        ,
        precio_accion       DECIMAL(19,14)  ,
        fecha_valuacion     DATE            ,
        mto_en_acciones     DECIMAL(22,6)   ,
        mto_en_pesos        DECIMAL(22,6)
      )

    GRANT ALL ON tmp_mto_sol_saldo TO PUBLIC
    --------------------------------

    DATABASE safre_af

END FUNCTION

#############################################################################
#CPL-1199 INI
FUNCTION f_primero()
 DEFINE l_liq,
        l_fec1   DATE,
        l_dia    SMALLINT

LET l_dia = -1    LET l_fec1 = NULL
LET l_liq = TODAY + 1 UNITS DAY

LET l_dia = WEEKDAY(l_liq)

 CASE l_dia
   WHEN 6   #sabado
     LET l_liq = l_liq + 2 UNITS DAY
   WHEN 0   #domingo
     LET l_liq = l_liq + 1 UNITS DAY
 END CASE

LET l_fec1 = MDY(MONTH(l_liq),1,YEAR(l_liq))

RETURN l_fec1

END FUNCTION

#CPL-1199 FIN


###################################################################################
FUNCTION f_actualiza_rechazos(li_folio)

DEFINE l_cuenta       SMALLINT,
       l_and          VARCHAR(100)

DEFINE
       lc_query       CHAR(1500)

DEFINE li_folio LIKE ret_trans_imss.folio

LET l_cuenta = 0

LET l_and = ""
IF m_fecha IS NULL THEN
       LET l_and = ""
ELSE
       LET l_and = " AND DATE(fecha_carga_afore) ='",m_fecha,"'"       #INV-1491
END IF

-- Pasamos todas las solicitudes en estado "Solicitud Saldo"  y rechazos a la tabla temporal
LET lc_query = ""
LET lc_query =
"SELECT count(*) \n",
"FROM ret_rech_datamart \n",
"WHERE nss IN \n",
"(SELECT nss FROM safre_tmp:tmp_trans_imss)\n",
l_and CLIPPED
LET lc_query = lc_query CLIPPED

PREPARE pre_rech_mart FROM lc_query
EXECUTE pre_rech_mart INTO l_cuenta

IF l_cuenta > 0 THEN
    UPDATE ret_rech_datamart SET folio = li_folio
    WHERE nss IN (
    SELECT nss FROM safre_tmp:tmp_trans_imss )
    AND folio IS NULL
END IF

LET l_cuenta = 0

LET lc_query = ""
LET lc_query =
"SELECT count(*) \n",
"FROM ret_det_datamart \n",
"WHERE nss IN \n",
"(SELECT nss FROM safre_tmp:tmp_trans_imss)\n",
"AND estado = ", gr_edo.rechazado_derecho, "\n",
"AND diagnostico IN (502,505,509)\n",
l_and CLIPPED
LET lc_query = lc_query CLIPPED

PREPARE pre_dg_mart FROM lc_query
EXECUTE pre_dg_mart INTO l_cuenta

IF l_cuenta > 0 THEN
    UPDATE ret_det_datamart SET folio = li_folio
    WHERE estado = gr_edo.rechazado_derecho
    AND diagnostico IN (502,505,509)
    AND nss IN (
    SELECT nss FROM safre_tmp:tmp_trans_imss )
    AND folio IS NULL

END IF

END FUNCTION


#------------------------------------------------#
# f_valida_tipo_Tramite: CPL-1673                #
#------------------------------------------------#
FUNCTION f_valida_tipo_Tramite(ls_ind_fip)

   DEFINE ls_ind_fip  SMALLINT

   DEFINE lr_ext_datamart      RECORD LIKE ret_ext_datamart.*
   DEFINE ls_marca,
          ls_cod_rechazo,
          ls_error             SMALLINT
   DEFINE li_sol_saldo         INT
   DEFINE lc_diagnostico       CHAR(3)
   DEFINE lr_marca     RECORD
          ls_marca_activa      SMALLINT
        END RECORD,
        ls_marca_921           SMALLINT

   DEFINE ls_marca_op          SMALLINT

   LET ls_marca = 0   LET ls_cod_rechazo = 0    LET ls_error = 0

   INITIALIZE lr_ext_datamart.*, lr_marca.*, ls_marca_op, li_sol_saldo, ls_marca_921 TO NULL

   SELECT id_solicitud_saldo INTO li_sol_saldo
     FROM ret_solicitud_saldo
    WHERE nss = gr_datamart_do.nss
      AND estado_solicitud = gr_edo.liquida_saldo

   # Validando que los Tipos de Retiro D-Negativa de Pensión y S-Pensión Retiro Programado tengan
   # solicitud de saldos previos liquidada.
   IF gr_datamart_do.tipo_retiro = 'D' OR
      gr_datamart_do.tipo_retiro = 'S' THEN

      IF li_sol_saldo IS NULL THEN # En caso de no tener Solicitud de S.P.
         LET gc_error = "SIN SOLICITUD DE SALDO PREVIOS, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
         CALL ERRORLOG(gc_error CLIPPED)
         #LET gr_datamart_do.tipo_tramite = 5   #tipo tramite informativo
      ELSE
         CALL f_inserta_sol_reinversion()
         # 3-Resolución con Saldo Previo
         LET gr_datamart_do.tipo_tramite = 3
      END IF   # FIN Solicitud de Saldo Previo
   END IF      # FIN Tipo Retiro D y S

   IF gr_datamart_do.tipo_retiro = 'A' OR
      gr_datamart_do.tipo_retiro = 'B' OR
      gr_datamart_do.tipo_retiro = 'C' OR
      gr_datamart_do.tipo_retiro = 'U' THEN

      # Validando que no tenga la marca 921
      LET ls_marca = 921

      SELECT marca_cod INTO ls_marca_921
       FROM cta_act_marca
       WHERE nss = gr_datamart_do.nss
        AND marca_cod = ls_marca
       GROUP BY 1

      IF ls_marca_921 IS NULL THEN
           CASE gr_datamart_do.tipo_retiro
                 WHEN "A"
                    LET ls_marca = 800
                 WHEN "B"
#CPL-2324 INI
                    IF (gr_datamart_do.tipo_seguro = "RJ" AND gr_datamart_do.tipo_prestacion = "1" AND 
                       (gr_datamart_do.tipo_pension = "AS" OR gr_datamart_do.tipo_pension = "OR"
                        OR gr_datamart_do.tipo_pension = "VI" OR gr_datamart_do.tipo_pension = "VO"
                        OR gr_datamart_do.tipo_pension = "IP" OR gr_datamart_do.tipo_pension = "RE"
                        OR gr_datamart_do.tipo_pension = "CE" OR gr_datamart_do.tipo_pension = "VE"
                        OR gr_datamart_do.tipo_pension = "IN" OR gr_datamart_do.tipo_pension = "PP")) THEN
                        LET ls_marca = 806
                    ELSE
                        LET ls_marca = 810
                    END IF
#CPL-2324 FIN
                 WHEN "C"
                    LET ls_marca = 815
                 WHEN "U"
                    LET ls_marca = 805
           END CASE

           DECLARE cur_tmp CURSOR FOR
           SELECT a.marca_activa
           FROM cta_convivencia a
           WHERE a.marca_activa IN (SELECT b.marca_cod
                                    FROM cta_act_marca b
                                    WHERE b.nss = gr_datamart_do.nss)
           AND a.marca_entra = ls_marca
           AND rechazo_cod > 0

           FOREACH cur_tmp INTO lr_marca.*
               IF ls_marca_op IS NULL AND lr_marca.ls_marca_activa IS NOT NULL THEN
                    LET ls_marca_op = lr_marca.ls_marca_activa
                    LET lr_ext_datamart.id_ext_datamart  = 0
                    LET lr_ext_datamart.folio_cliente    = gr_datamart_do.nss
                    LET lr_ext_datamart.folio_t_procesar = gr_datamart_do.folio_t_procesar
                    LET lr_ext_datamart.nss              = gr_datamart_do.nss
                    LET lr_ext_datamart.curp             = gr_datamart_do.curp
                    LET lr_ext_datamart.fecha_carga      = TODAY
                    LET lr_ext_datamart.sec_pension      = gr_datamart_do.sec_pension

                    # Validando si la cuenta se encuentra en proceso operativo de Traspaso Afore-Afore
                    SELECT "OK"
                      FROM cta_act_marca a,
                           taa_cd_tipo_traspaso b
                     WHERE a.marca_cod = b.marca_cod
                       AND a.nss = gr_datamart_do.nss
                       AND a.marca_cod = ls_marca_op
                     GROUP BY 1

                    # En caso de que la marca operativa corresponda a una de Traspaso Afore Afore
                    IF SQLCA.SQLCODE = 0 THEN
                       LET lr_ext_datamart.tipo_tramite     = 5 # Resolución Nueva Afore
                    ELSE
                       LET lr_ext_datamart.tipo_tramite     = 4 # Resolución Pendiente de Marca
                    END IF

                    # Insertando la solicitud de extemporaneas
                    WHENEVER ERROR CONTINUE
                    SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
                    LOCK TABLE ret_ext_datamart IN EXCLUSIVE MODE;

                    INSERT INTO safre_af:ret_ext_datamart VALUES(lr_ext_datamart.*)
                    IF SQLCA.SQLERRD[3] = 0 THEN
                       LET gc_error = "NO SE INSERTÓ SOL. DE EXTEMPORANEAS, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
                       CALL ERRORLOG(gc_error CLIPPED)
                       SET LOCK MODE TO NOT WAIT;
                       UNLOCK TABLE ret_ext_datamart;
                       WHENEVER ERROR STOP
                       RETURN
                    END IF
                    SET LOCK MODE TO NOT WAIT;
                    UNLOCK TABLE ret_ext_datamart;
                    WHENEVER ERROR STOP

                    LET gc_error = "SE GENERA SOLICITUD EXTEMPORANEA, CUENTA EN PROCESO OPERATIVO, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
                    LET gr_acept_datamart.diagnostico = 506
                    LET gr_datamart_do.tipo_tramite = 6     #extemporaneas
                    LET lc_diagnostico  = 506
                    CALL f_insert_rechazo(lc_diagnostico)
                    RETURN
               ELSE
                   CONTINUE FOREACH
               END IF
           END FOREACH
           # 4-Resolución sin Saldo Previo
           LET gr_datamart_do.tipo_tramite = 4
           IF ls_marca_op IS NULL AND li_sol_saldo IS NULL THEN
               CALL fn_inserta_sol_saldo(gr_datamart_do.folio_t_procesar, gr_datamart_do.nss, gr_datamart_do.curp,
                                        gr_datamart_do.ent_origen, gr_datamart_do.tipo_tramite,
                                        gr_datamart_do.nombre_afore, gr_datamart_do.paterno_afore, gr_datamart_do.materno_afore,
                                        gr_datamart_do.num_issste, "",
                                        HOY, gr_datamart_do.fecha_ini_pen,ls_ind_fip, #CPL-2324
                                        gr_datamart_do.tipo_pension, "5", # para identificar resoluciones sin saldo previo
                                        "0", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        gr_datamart_do.estado_sub_viv, HOY + 30) RETURNING ls_error
               IF ls_error = 1 THEN
                   LET  gc_error = "NO EXISTE EN EL MAESTRO DE AFILIADOS. NSS: ",gr_datamart_do.nss CLIPPED
                   CALL ERRORLOG(gc_error CLIPPED)
               END IF
               IF ls_error = 2 THEN
                   LET  gc_error = "NO EXISTE CONVIVENCIA CON MARCA 921 PARA INSERTAR LA SOLICITUD DE SALDO. NSS: ",gr_datamart_do.nss CLIPPED
                   CALL ERRORLOG(gc_error CLIPPED)
               END IF
           END IF      # ls_marca_op IS NULL  sin marca operativa
      ELSE #si tiene marca 921
          # 3-Resolución con Saldo Previo
          LET gr_datamart_do.tipo_tramite = 3
          # Quitando la marca 921-CONSULTA SALDO PREVIO
          LET ls_marca   = 921
          LET gc_prepare = "EXECUTE  PROCEDURE  desmarca_cuenta(?,?,?,?,?,?)"
          LET gc_prepare = gc_prepare   CLIPPED
          PREPARE  p_desmarca_921_t4   FROM   gc_prepare
          EXECUTE  p_desmarca_921_t4   USING  gr_datamart_do.nss,
                                           ls_marca,
                                           li_sol_saldo,
                                           CERO,
                                           ls_marca,
                                           gc_usuario
      END IF   # ls_marca_921 IS NULL

     CASE gr_datamart_do.tipo_retiro
       WHEN "A"
          LET ls_marca = 800
       WHEN "B"
#CPL-2324 INI
          IF (gr_datamart_do.tipo_seguro = "RJ" AND gr_datamart_do.tipo_prestacion = "1" AND 
             (gr_datamart_do.tipo_pension = "AS" OR gr_datamart_do.tipo_pension = "OR"
              OR gr_datamart_do.tipo_pension = "VI" OR gr_datamart_do.tipo_pension = "VO"
              OR gr_datamart_do.tipo_pension = "IP" OR gr_datamart_do.tipo_pension = "RE"
              OR gr_datamart_do.tipo_pension = "CE" OR gr_datamart_do.tipo_pension = "VE"
              OR gr_datamart_do.tipo_pension = "IN" OR gr_datamart_do.tipo_pension = "PP")) THEN
              LET ls_marca = 806
          ELSE
              LET ls_marca = 810
          END IF
#CPL-2324 FIN
       WHEN "C"
          LET ls_marca = 815
       WHEN "U"
          LET ls_marca = 805
     END CASE

     DECLARE cur_marca3 CURSOR FOR p_marca
     OPEN cur_marca3 USING
              gr_datamart_do.nss,  #pnss
              ls_marca,            #pmarca_entra
              gi_id_registro,      #pcorrelativo
              CERO,                #pestado_marca
              CERO,                #pcodigo_rechazo
              ls_marca,            #pmarca_causa
              HOY,                 #pfecha_causa
              gc_usuario           #pusuario

     FETCH cur_marca3 INTO ls_marca, ls_cod_rechazo

     IF ls_cod_rechazo != 0 THEN    #No convive
         LET  gc_error = "CUENTA EN PROCESO OPERATIVO, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED,
                         ls_marca USING "<<<<<<"
         CALL ERRORLOG(gc_error CLIPPED)
         LET gr_acept_datamart.diagnostico = 506
         LET gr_datamart_do.tipo_tramite = 6     #extemporaneas
         LET  lc_diagnostico = 506
         CALL f_insert_rechazo(lc_diagnostico)
         RETURN
     END IF

   END IF      # FIN Tipo Retiro A,B,C

END FUNCTION

#-----------------------------------------------------------------------------#
# f_inserta_sol_reinversion(): Método que inserta la solicitud de reinversión #
#-----------------------------------------------------------------------------#
FUNCTION f_inserta_sol_reinversion()
   DEFINE
      lr_reinversion       RECORD LIKE   ret_reinversion.*

   DEFINE
      li_sol_saldo         INT,
      ls_tipo_reinv        ,
      ls_marca             ,
      ls_cod_rechazo       SMALLINT

   DEFINE
      lc_tipo_ret          CHAR(100)

   INITIALIZE lr_reinversion.*, li_sol_saldo TO NULL
   LET ls_tipo_reinv  = 0
   LET ls_marca       = 0
   LET lc_tipo_ret    = "RETIRO TIPO ", gr_datamart_do.tipo_retiro
   LET lc_tipo_ret    = lc_tipo_ret CLIPPED

   SELECT id_solicitud_saldo INTO li_sol_saldo
     FROM ret_solicitud_saldo
    WHERE nss = gr_datamart_do.nss
      AND estado_solicitud = gr_edo.liquida_saldo

    SELECT "X" FROM safre_af:ret_reinversion
    WHERE id_solicitud_saldo = li_sol_saldo
    GROUP BY 1

   -- NO existe sol de reinversion para evitar duplicar #CPL-1567
   IF SQLCA.SQLCODE = 100 AND li_sol_saldo IS NOT NULL THEN
      # Obteniendo el Tipo Reinversion por tipo de retiro
      SELECT id_tipo_origen_reinv
        INTO ls_tipo_reinv
        FROM ret_tipo_origen_reinv
       WHERE desc_origen_reinv = lc_tipo_ret

      LET lr_reinversion.id_reinversion = 0
      LET lr_reinversion.id_solicitud_saldo = li_sol_saldo
      LET lr_reinversion.fecha_registro = CURRENT
      LET lr_reinversion.id_tipo_origen_reinv = ls_tipo_reinv
      LET lr_reinversion.estado_solicitud = gr_edo.sol_reinversion

      # Insertando la solicitud de reinversión
      WHENEVER ERROR CONTINUE
      SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
      LOCK TABLE ret_reinversion IN EXCLUSIVE MODE;

      INSERT INTO safre_af:ret_reinversion VALUES(lr_reinversion.*)
      IF SQLCA.SQLERRD[3] = 0 THEN
         LET gc_error = "NO SE INSERTÓ LA REINVERSION, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
         CALL ERRORLOG(gc_error CLIPPED)
         SET LOCK MODE TO NOT WAIT;
         UNLOCK TABLE ret_reinversion;
         WHENEVER ERROR STOP
         RETURN
      END IF
      SET LOCK MODE TO NOT WAIT;
      UNLOCK TABLE ret_reinversion;
      WHENEVER ERROR STOP

      EXECUTE p_upd_solicitud USING gr_datamart_do.nss, gr_edo.liquida_saldo , li_sol_saldo

      # Quitando la marca 921-CONSULTA SALDO PREVIO
      LET ls_marca   = 921
      LET gc_prepare = "EXECUTE  PROCEDURE  desmarca_cuenta(?,?,?,?,?,?)"
      LET gc_prepare =  gc_prepare   CLIPPED
      PREPARE  p_desmarca       FROM  gc_prepare
      EXECUTE  p_desmarca       USING  gr_datamart_do.nss,
                                       ls_marca,
                                       li_sol_saldo,
                                       CERO,
                                       ls_marca,
                                       gc_usuario

      # Marcando la cuenta con 922-REINVERTIDOS
      LET ls_marca  = 922
      DECLARE cur_marca2 CURSOR FOR p_marca
      OPEN cur_marca2 USING
          gr_datamart_do.nss,  #pnss
          ls_marca,            #pmarca_entra
          li_sol_saldo,        #pcorrelativo
          CERO,                #pestado_marca
          CERO,                #pcodigo_rechazo
          ls_marca,            #pmarca_causa
          HOY,                 #pfecha_causa
          gc_usuario           #pusuario

      FETCH cur_marca2 INTO ls_marca, ls_cod_rechazo
      IF ls_cod_rechazo != 0 THEN
         LET gc_error = "NO SE MARCÓ POR 922-REINVERTIDOS, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
         CALL ERRORLOG(gc_error CLIPPED)
      END IF

      LET m_reinversion = m_reinversion + 1
      DISPLAY "SOLICITUD DE REINVERSION INSERTADAS: ", m_reinversion USING "<<<<<<<" AT 16,1

      CLOSE cur_marca2
   END IF
END FUNCTION

#-------------------------------------------------------------------------------------------#
# f_guarda_solicitud(): Guarda la solicitud con su diagnostico, ya sea Aceptada o Rechazada #
#-------------------------------------------------------------------------------------------#
FUNCTION f_guarda_solicitud()

   DEFINE 
      ls_sql_code          ,
      pc_diagnostico        SMALLINT
   # Validando el diagnóstico de la solicitud

   IF gr_acept_datamart.diagnostico = 501 OR     # 501-Confirmación exitosa
      gr_acept_datamart.diagnostico = 502 OR     # 502-Cuenta con saldo cero en todas sus subcuentas sin transferencia de recursos
      gr_acept_datamart.diagnostico = 505 OR     # 505-Cuenta con traspaso posterior a FIP
      gr_acept_datamart.diagnostico = 0   OR     # 0-Solicitud Informativa
      gr_acept_datamart.diagnostico = 509 THEN   # 509-Solicitud Aceptada, con saldo pero sin recursos a transferir

      # Se elimina un registro existente en datamart
      SELECT "OK"
      FROM ret_det_datamart
      WHERE nss = gr_datamart_do.nss
      AND sec_pension = gr_datamart_do.sec_pension
      AND tipo_movimiento = gr_datamart_do.tipo_movimiento
      AND diag_datamart = gr_datamart_do.diag_datamart
      
      LET ls_sql_code = SQLCA.SQLCODE
      CASE ls_sql_code
         WHEN 0
            SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
            LOCK TABLE ret_det_datamart IN EXCLUSIVE MODE;
               DELETE FROM ret_det_datamart
               WHERE nss = gr_datamart_do.nss
               AND sec_pension = gr_datamart_do.sec_pension
               AND tipo_movimiento = gr_datamart_do.tipo_movimiento
               AND diag_datamart = gr_datamart_do.diag_datamart
               
            IF SQLCA.SQLERRD[3] = 0 THEN
               # Se envía el error de inserción al Log de errores
               LET gc_error = "NO SE ELIMINÓ LA SOLICITUD DE DATAMART, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
               CALL ERRORLOG(gc_error CLIPPED)
            END IF
            SET LOCK MODE TO NOT WAIT;
            UNLOCK TABLE ret_det_datamart;
      END CASE
      
      # Realizando el guardado de la solicitud Aceptada en ret_det_datamart
      WHENEVER ERROR CONTINUE
      SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
      LOCK TABLE ret_det_datamart IN EXCLUSIVE MODE;
         INSERT INTO safre_af:ret_det_datamart
            VALUES(
               gi_id_registro                         ,        # id_registro
               gr_datamart_do.folio_t_procesar        ,        # folio_t_procesar
               gr_datamart_do.nss                     ,        # nss
               gr_datamart_do.ent_origen              ,        # entidad_origen
               gr_datamart_do.tipo_tramite            ,        # tipo_tramite
               gr_datamart_do.num_issste              ,        # num_issste
               gr_datamart_do.sec_pension             ,        # sec_pension
               NULL                                   ,        # folio
               gr_datamart_do.curp                    ,        # curp
               gr_datamart_do.nombre_datamart         ,        # nombre_datamart
               gr_datamart_do.nombre_afore            ,        # nombre_afore
               gr_datamart_do.paterno_afore           ,        # paterno_afore
               gr_datamart_do.materno_afore           ,        # materno_afore
               gr_datamart_do.tipo_movimiento         ,        # tipo_movimiento
               gr_datamart_do.tipo_retiro             ,        # tipo_retiro
               gr_datamart_do.regimen                 ,        # regimen
               gr_datamart_do.tipo_seguro             ,        # tipo_seguro
               gr_datamart_do.tipo_pension            ,        # tipo_pension
               gr_datamart_do.tipo_prestacion         ,        # tipo_prestacion
               gr_datamart_do.cve_pension             ,        # clave_pension
               gr_datamart_do.art_negativa            ,        # art_negativa
               gr_datamart_do.frac_negativa           ,        # frac_negativa
               gr_datamart_do.num_considerando        ,        # num_considerando
               gr_datamart_do.fecha_ini_pen           ,        # fecha_ini_pen
               gr_datamart_do.fec_ini_pago            ,        # fec_ini_pago
               gr_datamart_do.fecha_resolucion        ,        # fecha_resolucion
               gr_datamart_do.clave_aseguradora       ,        # clave_aseguradora
               gr_datamart_do.porcentaje_val          ,        # porcentaje_val
               gr_datamart_do.semanas_cotizadas       ,        # semanas_cotizadas
               gr_datamart_do.fec_carga_datamart      ,        # fec_carga_datamart
               gr_datamart_do.diag_datamart           ,        # diag_datamart
               gr_datamart_do.estado_sub_viv          ,        # estado_sub_viv
               gr_datamart_do.monto_sol_imss          ,        # monto_sol_imss
               gr_datamart_do.transf_previa           ,        # transf_previa
               gr_datamart_do.consec_procesar         ,        # num_consec_procesar
               gr_datamart_do.tot_resol_porcesar      ,        # total_resol_procesar
               CURRENT                                ,        # fecha_carga_afore
               gr_acept_datamart.estado               ,        # estado
               gr_acept_datamart.diagnostico          ,        # diagnostico
               NULL                                   ,        # ind_env_recep_trans
               NULL                                            # ind_env_recep_disp
            )

      IF SQLCA.SQLERRD[3] = 0 THEN
         LET gc_error = "ERROR AL INSERTAR EN ret_det_datamart, FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
         CALL ERRORLOG(gc_error CLIPPED)
         WHENEVER ERROR STOP
         SET LOCK MODE TO NOT WAIT;
         UNLOCK TABLE ret_det_datamart;
         RETURN
      END IF
      SET LOCK MODE TO NOT WAIT;
      UNLOCK TABLE ret_det_datamart;
      WHENEVER ERROR STOP
   END IF
END FUNCTION

#-------------------------------------------------------------------------#
# fn_inserta_sol_saldo(): Inserta la solicitud de saldo para desinversion #
#-------------------------------------------------------------------------#
FUNCTION fn_inserta_sol_saldo( p_folio_procesar, p_nss, p_curp, p_ent_origen, p_tpo_tramite,
                               p_nombre, p_paterno, p_materno, p_num_issste, p_folio_issste, p_fec_sol_tra,
                               p_fec_ip, p_ind_sol_saldo_fip, p_cve_pension, p_ind_port,
                               p_ind_tra_post_fip, p_ret97, p_cs, p_cv, p_acum_viv97, p_act_viv97,
                               p_ahorro_sol, p_cv_issste, p_ret08_issste, p_cs_issste, p_aport_comp,
                               p_apor_vol, p_apor_lp, p_fov08, p_saldo_fov08,
                               p_status_viv, p_fec_venc)

DEFINE
        p_folio_procesar       CHAR(50),
        p_nss                  CHAR(11),
        p_curp                 CHAR(18),
        p_ent_origen           CHAR(3),
        p_tpo_tramite          CHAR(3),
        p_nombre               VARCHAR(40),
        p_paterno              VARCHAR(40),
        p_materno              VARCHAR(40),
        p_num_issste           CHAR(18),
        p_folio_issste         CHAR(14),
        p_fec_sol_tra          DATE,
        p_fec_ip               DATE,
        p_ind_sol_saldo_fip    CHAR(1),
        p_cve_pension          CHAR(2),
        p_ind_port             CHAR(1),
        p_ind_tra_post_fip     CHAR(1),
        p_ret97                DECIMAL(13,2),
        p_cs                   DECIMAL(13,2),
        p_cv                   DECIMAL(13,2),
        p_acum_viv97           DECIMAL(13,2),
        p_act_viv97            DECIMAL(13,2),
        p_ahorro_sol           DECIMAL(13,2),
        p_cv_issste            DECIMAL(13,2),
        p_ret08_issste         DECIMAL(13,2),
        p_cs_issste            DECIMAL(13,2),
        p_aport_comp           DECIMAL(13,2),
        p_apor_vol             DECIMAL(13,2),
        p_apor_lp              DECIMAL(13,2),
        p_fov08                DECIMAL(13,2),
        p_saldo_fov08          DECIMAL(13,2),
        p_status_viv           CHAR(1),
        p_fec_venc             DATE

    DEFINE   l_id_sol_consec    DECIMAL(11,0);
    DEFINE   l_folio            DECIMAL(11,0);
    DEFINE   r_marca            SMALLINT;
    DEFINE   l_marca_sol        SMALLINT;
    DEFINE   l_estado_sol       SMALLINT;
    DEFINE   lc_edo_viv_fov     CHAR(01);
    DEFINE   lc_ind_vivienda    CHAR(02);
    DEFINE   p_rechazo          SMALLINT;
    DEFINE   ld_diagnostico     CHAR(3);


    LET l_marca_sol = 921;
    LET l_estado_sol = 100;
    LET l_id_sol_consec = 0;
    LET l_folio = 0;
    LET r_marca = 0;
    LET ld_diagnostico = "101";
    LET lc_edo_viv_fov = "";
    LET lc_ind_vivienda = "";


    SELECT "OK" FROM afi_mae_afiliado
    WHERE n_seguro = p_nss;

    IF STATUS = NOTFOUND THEN
       RETURN 1     # NSS no existe en el maestro de Afiliados
    END IF
    
    SELECT edo_viv_fovissste, ind_vivienda
    INTO   lc_edo_viv_fov, lc_ind_vivienda
    FROM   ret_datamart_comp
    WHERE  folio_t_procesar = p_folio_procesar
    AND    nss              = p_nss
    AND    fecha_carga      = (SELECT MIN (fecha_carga)
                               FROM ret_datamart_comp
                               WHERE folio_t_procesar = p_folio_procesar
                               AND nss = p_nss)

     --SELECT NVL(MAX(consecutivo + 1),1), USER INTO l_id_sol_consec,gc_usuario
     --FROM ret_consecutivo;
     --INSERT INTO ret_consecutivo VALUES(l_id_sol_consec);
     
     EXECUTE p_consec INTO l_id_sol_consec

    # DECLARE cur_marca_saldo CURSOR FOR p_marca
    #
    # OPEN cur_marca_saldo USING p_nss,       #pnss
    #                    l_marca_sol,         #pmarca_entra
    #                    l_id_sol_consec,     #pcorrelativo
    #                    CERO,                #pestado_marca
    #                    CERO,                #pcodigo_rechazo
    #                    l_marca_sol,         #pmarca_causa
    #                    HOY,                 #pfecha_causa
    #                    gc_usuario           #pusuario
    #
    #FETCH cur_marca_saldo INTO r_marca, p_rechazo
    #
    # IF p_rechazo != 0 THEN -- si NO marco
    #     RETURN 2    #No existe convivencia de Marcas
    # END IF

      SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
      LOCK TABLE ret_solicitud_saldo IN EXCLUSIVE MODE;
      
      INSERT INTO ret_solicitud_saldo VALUES (
            l_id_sol_consec     ,     --id_solicitud_saldo
            p_folio_procesar    ,
            l_folio             ,     --folio_cargo
            p_nss               ,     --nss
            p_curp              ,     --curp
            p_ent_origen        ,
            p_tpo_tramite       ,
            p_nombre            ,     --nombre
            p_paterno           ,     --apellido_paterno
            p_materno           ,     --apellido_materno
            p_num_issste        ,
            p_folio_issste      ,
            p_fec_sol_tra       ,     --fecha_sol_trabajador
            p_fec_ip            ,     --fecha_ini_pen
            p_ind_sol_saldo_fip ,     --ind_saldo_fip
            p_cve_pension       ,     --clave_pension
            p_ind_port          ,     --ind_portabilidad
            p_ind_tra_post_fip  ,     --ind_trasp_post_fip
            p_ret97             ,     --pes_ret97_post
            p_cs                ,     --pes_cs_post
            p_cv                ,     --pes_cv_post
            p_acum_viv97        ,     --pes_viv97_post
            p_act_viv97         ,     --pes_saldo_viv97
            p_ahorro_sol        ,
            p_cv_issste         ,
            p_ret08_issste      ,
            p_cs_issste         ,
            p_aport_comp        ,
            p_apor_vol          ,
            p_apor_lp           ,
            p_fov08             ,
            p_saldo_fov08       ,
            p_status_viv        ,     --estado_vivienda
            lc_edo_viv_fov      ,     --edo_viv_fovissste
            lc_ind_vivienda     ,     --ind_vivienda
            p_fec_venc          ,     --fecha_vencimiento
            CURRENT             ,     --f_recep_procesar
            ""                  ,     --f_envio_a_safre
            ""                  ,     --f_recep_respuesta_safre
            ""                  ,     --f_respuesta_a_procesar
            ld_diagnostico      ,     --diag_recep_afore
            ""                  ,     --diag_envio_procesar
            ""                  ,     --ind_recep_procesar
            gc_usuario          ,     --usuario_carga
            l_estado_sol              --estado_solicitud
         );

    IF SQLCA.SQLCODE != 0 THEN
             LET gc_error = "ERROR AL INSERTAR EN ret_solicitud_saldo, NSS:",p_nss
             CALL ERRORLOG(gc_error CLIPPED)
    END IF
    
    SET LOCK MODE TO NOT WAIT;
    UNLOCK TABLE ret_solicitud_saldo;

    LET m_saldos = m_saldos + 1
    DISPLAY "SOLICITUD DE SALDOS INSERTADAS: ", m_saldos USING "<<<<<<<" AT 15,1
    RETURN 0

END FUNCTION

########################################################################################################
FUNCTION f_calcula_vivienda (ls_nss, lc_subcuenta, li_id)

    DEFINE ls_nss         CHAR(11)
    DEFINE lc_subcuenta   CHAR(1)
    DEFINE li_id          SMALLINT
    DEFINE l_reg_mto_sal  SMALLINT
    DEFINE lr_sol_saldo   RECORD LIKE ret_solicitud_saldo.*

    DEFINE lr_monto_saldo RECORD LIKE ret_mto_solicitud_saldo.*

    DEFINE lr_siefore RECORD
        activo              SMALLINT        ,
        pesos_retiro        DECIMAL(16,6)   ,
        pesos_cv            DECIMAL(16,6)   ,
        pesos_cs            DECIMAL(16,6)   ,
        pesos_estatal       DECIMAL(16,6)   ,
        pesos_esp           DECIMAL(16,6)   ,
        pesos_SAR92         DECIMAL(16,6)
    END RECORD

    DEFINE lr_mto_viv RECORD
        pesos_viv97         DECIMAL(14,2) ,
        pesos_viv92         DECIMAL(14,2)
    END RECORD

    DEFINE lr_saldo_traspaso RECORD
        ret_97      LIKE ret_solicitud_saldo.pes_ret97_post ,
        cs          LIKE ret_solicitud_saldo.pes_cs_post    ,
        cv          LIKE ret_solicitud_saldo.pes_cv_post    ,
        viv97       LIKE ret_solicitud_saldo.pes_viv97_post
    END RECORD

    DEFINE lr_saldo_dia RECORD
        subcuenta   LIKE dis_provision.subcuenta        ,
        siefore     LIKE dis_provision.siefore          ,
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE
        ldt_fip                 ,
        ld_fecha_saldo          DATE

    DEFINE #loc #smallint
        ls_insert               ,
        ls_siefore              ,
        ls_subcta               SMALLINT

    DECLARE cur_viv CURSOR FOR
    SELECT *
    FROM   ret_solicitud_saldo
    WHERE  nss                  = ls_nss
    AND    diag_envio_procesar  = 501
    AND    ind_saldo_fip        = li_id
    AND    estado_solicitud     = gr_edo.liquida_saldo
    ORDER BY nss

    -- Iniciamos ciclo para cada nss
    FOREACH cur_viv INTO lr_sol_saldo.*

        INITIALIZE lr_monto_saldo.* TO NULL

        LET lr_siefore.activo           = FALSE
        LET lr_siefore.pesos_retiro     = 0
        LET lr_siefore.pesos_cv         = 0
        LET lr_siefore.pesos_cs         = 0
        LET lr_siefore.pesos_estatal    = 0
        LET lr_siefore.pesos_esp        = 0
        LET lr_siefore.pesos_SAR92      = 0

        -- Se inicializan variables
        LET ls_insert                   = 0
        LET lr_mto_viv.pesos_viv97      = 0
        LET lr_mto_viv.pesos_viv92      = 0

        -- Si el indicador es 2 informamos el monto a la FIP
        IF lr_sol_saldo.ind_saldo_fip   = 2 THEN
            LET ldt_fip = lr_sol_saldo.fecha_ini_pen
        END IF

        IF lr_sol_saldo.ind_trasp_post_fip = 2 THEN
            LET lr_saldo_traspaso.ret_97    = 0
            LET lr_saldo_traspaso.cs        = 0
            LET lr_saldo_traspaso.cv        = 0
            LET lr_saldo_traspaso.viv97     = 0
        END IF

        EXECUTE eje_con_saldos USING lr_sol_saldo.nss   ,
                                     HOY                ,
                                     ldt_fip            ,
                                     lr_saldo_traspaso.*
                               INTO  lr_siefore.activo              ,
                                     lr_monto_saldo.fecha_valuacion ,
                                     lr_monto_saldo.precio_accion   ,
                                     lr_siefore.pesos_retiro        ,
                                     lr_siefore.pesos_cv            ,
                                     lr_siefore.pesos_cs            ,
                                     lr_siefore.pesos_estatal       ,
                                     lr_siefore.pesos_esp           ,
                                     lr_mto_viv.pesos_viv97

        LET lr_monto_saldo.id_solicitud_saldo   = lr_sol_saldo.id_solicitud_saldo
        LET lr_monto_saldo.folio_cargo          = lr_sol_saldo.folio_cargo
        LET lr_monto_saldo.nss                  = lr_sol_saldo.nss
        LET lr_monto_saldo.curp                 = lr_sol_saldo.curp
        LET ls_siefore                          = lr_siefore.activo

        DECLARE cur_subcta CURSOR FOR
        SELECT subcuenta
        FROM   tab_agrupa_subcta
        WHERE  grupo     = 8
        AND    subcuenta = lc_subcuenta
        ORDER BY 1

        -- Iniciamos ciclo para cada subcuenta del nss actual
        FOREACH cur_subcta INTO ls_subcta

            LET l_reg_mto_sal = 0

            SELECT count(*)
            INTO l_reg_mto_sal
            FROM ret_mto_solicitud_saldo
            WHERE nss = ls_nss
            AND subcuenta = ls_subcta
            AND folio_cargo = lr_sol_saldo.folio_cargo

            IF l_reg_mto_sal > 0 AND (ls_subcta = 4 OR ls_subcta = 8) THEN
                 CONTINUE FOREACH
            END IF

            LET lr_saldo_dia.subcuenta = 0
            LET lr_saldo_dia.siefore   = 0
            LET lr_saldo_dia.monto_acc = 0
            LET lr_saldo_dia.monto_pes = 0

            -- Determinamos a que fecha se va a obtener el saldo
            IF (ls_subcta = 4) OR (ls_subcta = 8) THEN
               LET ld_fecha_saldo  = gdt_fecha_viv
            END IF

            CALL f_obten_saldo_subcta(lr_sol_saldo.nss, ls_subcta, ld_fecha_saldo)
                RETURNING lr_saldo_dia.*

            CASE ls_subcta
                WHEN 4
                    LET lr_monto_saldo.mto_en_pesos = lr_mto_viv.pesos_viv97
                WHEN 8
                    LET lr_monto_saldo.mto_en_pesos = lr_saldo_dia.monto_pes
            END CASE

            LET lr_monto_saldo.subcuenta        = ls_subcta

            #Se asigna siefore de vivienda ACS 28062012
            IF lr_monto_saldo.subcuenta = 4 OR lr_monto_saldo.subcuenta = 8 THEN
                LET lr_monto_saldo.siefore          = gs_sieviv
                LET lr_monto_saldo.precio_accion    = gar_precio_acc[gs_sieviv].precio_dia
            END IF

            LET lr_monto_saldo.mto_en_acciones  = lr_monto_saldo.mto_en_pesos/lr_monto_saldo.precio_accion

            {IF lr_monto_saldo.subcuenta = 4 OR lr_monto_saldo.subcuenta = 8 THEN
                LET lr_monto_saldo.mto_en_acciones = f_redondea_val(lr_monto_saldo.mto_en_acciones, 2)
            END IF}

            IF lr_saldo_dia.monto_acc > 0 THEN

                INSERT INTO safre_tmp:tmp_mto_sol_saldo
                VALUES (lr_monto_saldo.*)

                -- Copiamos la tabla de montos temporal a la definitiva
                INSERT INTO ret_mto_solicitud_saldo
                SELECT *
                FROM   safre_tmp:tmp_mto_sol_saldo
                WHERE nss = ls_nss
                AND subcuenta = lc_subcuenta

            END IF -- lr_saldo_dia.monto_acc > 0

            LET lr_monto_saldo.siefore            = NULL
            LET lr_monto_saldo.precio_accion      = NULL
            LET lr_monto_saldo.subcuenta          = NULL
            LET lr_monto_saldo.mto_en_acciones    = NULL

        END FOREACH -- Subcuentas

    END FOREACH -- Siguiente NSS
END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_saldo_subcta : Obtiene el saldo del nss de la subcuenta indicada  #
#                        a la fecha dada por pr_datos.fec_saldo             #
#---------------------------------------------------------------------------#
FUNCTION f_obten_saldo_subcta(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_trans_issste.nss           ,
        subcta      LIKE dis_provision.subcuenta        ,
        fec_saldo   LIKE dis_provision.fecha_conversion
    END RECORD

    DEFINE lr_sal_dia RECORD
        subcta      LIKE dis_provision.subcuenta        ,
        siefore     LIKE dis_provision.siefore          ,
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_acciones
    END RECORD

    DEFINE ld_saldo_dia_viv LIKE dis_provision.monto_en_acciones

    DEFINE
        ls_grupo            SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_grupo            = 0
    LET lr_sal_dia.subcta   = 0
    LET lr_sal_dia.siefore  = 0

    IF pr_datos.subcta <> 4 AND pr_datos.subcta <> 8 THEN #MLM-2378
      DECLARE cur_saldo_sub CURSOR FOR eje_saldo_dia

      OPEN cur_saldo_sub USING pr_datos.nss       ,
                         pr_datos.subcta    ,
                         ls_grupo           ,
                         pr_datos.fec_saldo

      FETCH cur_saldo_sub INTO lr_sal_dia.*

      CLOSE cur_saldo_sub
    END IF

    IF lr_sal_dia.subcta <> 19 THEN

        IF lr_sal_dia.monto_acc IS NULL OR lr_sal_dia.monto_acc = 0 THEN
            --Si no tiene saldo en la subcuenta se manda el saldo como cero
            LET lr_sal_dia.monto_acc = 0
            LET lr_sal_dia.monto_pes = 0
        END IF

        -- Verificamos si no existe un sobregiro en vivienda
        #IF lr_sal_dia.siefore = gs_sieviv THEN
        IF pr_datos.subcta = 4 OR pr_datos.subcta = 8 THEN

            LET ld_saldo_dia_viv = 0

            SELECT SUM(monto_en_acciones)
            INTO   ld_saldo_dia_viv
            FROM   dis_cuenta
            WHERE  nss       = pr_datos.nss
            AND    siefore   = gs_sieviv
            AND    subcuenta = pr_datos.subcta

            LET  lr_sal_dia.subcta = pr_datos.subcta  #MLM-2378
            LET  lr_sal_dia.siefore = gs_sieviv       #MLM-2378
            LET  lr_sal_dia.monto_acc = ld_saldo_dia_viv   #MLM-2378
            LET  lr_sal_dia.monto_pes = ld_saldo_dia_viv * gar_precio_acc[11].precio_dia #MLM-2378


            IF ld_saldo_dia_viv < 0 OR ld_saldo_dia_viv IS NULL THEN
                LET ld_saldo_dia_viv = 0
            END IF

            -- Si lo que se obtiene al primer dia natural es mayor a lo que hay
            -- actualmente en la cuenta individual, tomamos el saldo al dia
            IF lr_sal_dia.monto_acc > ld_saldo_dia_viv THEN
                LET lr_sal_dia.monto_acc = ld_saldo_dia_viv
            END IF
        END IF
    END IF

    RETURN lr_sal_dia.*

END FUNCTION
#---------------------------------------------------------------------------#
# f_genera_tmp_cuenta : Genera la tabla temporal tmp_dis_cuenta que         #
#                       contiene la informacion historica de la cuenta      #
#                       individual del nss                                  #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta(p_nss)

    DEFINE
         p_nss                  CHAR(11)  ,
         lc_nom_tabla           CHAR(20)  ,
         lc_sel_hist            CHAR(1500)

     WHENEVER ERROR CONTINUE
         DROP TABLE tmp_dis_cuenta
     WHENEVER ERROR STOP

     DECLARE cur_his CURSOR FOR
     SELECT tabname
     FROM   systables
     WHERE  tabname matches "dis_cuenta??"

     FOREACH cur_his INTO lc_nom_tabla

        LET lc_sel_hist = lc_sel_hist CLIPPED,
                          " SELECT * ",
                          " FROM ",lc_nom_tabla          ,
                          " WHERE nss = ","'",p_nss,"'"  ,
                          " UNION ALL "
    END FOREACH

    CLOSE cur_his

    LET lc_sel_hist = lc_sel_hist CLIPPED,
                      " SELECT * ",
                      " FROM dis_cuenta ",
                      " WHERE nss = ","'",p_nss,"'"  ,
                      " INTO TEMP tmp_dis_cuenta "
                      
    #DISPLAY lc_sel_hist

    PREPARE eje_sel_his FROM lc_sel_hist
    EXECUTE eje_sel_his

    CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta (tipo_movimiento)
    UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION

#------------------------------------------------------------------------ #
# Inserta los registros enviados en tabla de control ret_his_diagnostico  #
#------------------------------------------------------------------------ #
FUNCTION f_inserta_his_diagnostico (ls_id_historico, li_consecutivo, lc_nss, ls_estado, lc_diagnostico)

   DEFINE
      ls_id_historico          SMALLINT,
      li_consecutivo           INTEGER,
      lc_nss                   CHAR(11),
      ls_estado                SMALLINT,
      lc_diagnostico           CHAR(3)
      #lr_captura_diag       RECORD LIKE   ret_his_diagnostico.*
   DEFINE lc_error         CHAR(1000)

   # Insertando el historico del diagnostico
   WHENEVER ERROR CONTINUE
   SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
   LOCK TABLE ret_his_diagnostico IN EXCLUSIVE MODE;


   INSERT INTO safre_af:ret_his_diagnostico VALUES(ls_id_historico, li_consecutivo, lc_nss, ls_estado, lc_diagnostico)
   IF SQLCA.SQLERRD[3] = 0 THEN
      LET lc_error = "NO SE INSERTO EL REGISTRO, DIAGNOSTICO: ", lc_diagnostico CLIPPED," NSS : ", lc_nss CLIPPED
      CALL ERRORLOG(lc_error CLIPPED)
      SET LOCK MODE TO NOT WAIT;
      UNLOCK TABLE ret_his_diagnostico;
      WHENEVER ERROR STOP
      RETURN
   END IF
   SET LOCK MODE TO NOT WAIT;
   UNLOCK TABLE ret_his_diagnostico;
   WHENEVER ERROR STOP

END FUNCTION
################################################################################
FUNCTION f_sol_duplicada()
   DEFINE
      ls_bandera        ,
      pc_diagnostico     SMALLINT
      
   LET ls_bandera = 1
   
   SELECT "ok"
   FROM    ret_det_datamart
   WHERE   folio_t_procesar  = gr_datamart_do.folio_t_procesar 
   AND     nss               = gr_datamart_do.nss   
   AND     tipo_retiro       = gr_datamart_do.tipo_retiro 
   AND     sec_pension       = gr_datamart_do.sec_pension  
   AND     regimen           = gr_datamart_do.regimen 
   AND     tipo_pension      = gr_datamart_do.tipo_pension  
   AND     diag_datamart     = gr_datamart_do.diag_datamart 
   AND     estado        IN  (gr_edo.provisionado, gr_edo.preliquidado)
   
   LET ls_bandera = SQLCA.SQLCODE
   IF ls_bandera = 0 THEN 
      LET pc_diagnostico = 0
      LET gc_error = "SOLICITUD DUPLICADA , FOLIO PROCESAR: ", gr_datamart_do.folio_t_procesar CLIPPED," NSS : ", gr_datamart_do.nss CLIPPED
      CALL f_insert_rechazo(pc_diagnostico)
      RETURN ls_bandera
   END IF
   
   RETURN ls_bandera
END FUNCTION

#-----------------------------------------------------------------------------#
# f_gen_reinv_por_rechazo():  Función que genera la desm #
#-----------------------------------------------------------------------------#
FUNCTION f_gen_reinv_por_rechazo(pr_datos)

   DEFINE pr_datos RECORD LIKE ret_trans_imss.*
   	
   DEFINE
      lr_reinversion       RECORD LIKE   ret_reinversion.*

   DEFINE
      li_sol_saldo         INT,
      ls_marca             ,
      ls_cod_rechazo       SMALLINT
      
   DEFINE   lc_prepare      CHAR(300)

   INITIALIZE lr_reinversion.*, li_sol_saldo TO NULL
   LET ls_marca       = 0
   
   --DISPLAY "NSS a rechazar: ", pr_datos.nss
   --DISPLAY "Id_solicitud_saldo: ", pr_datos.consec_sol_saldo
   
   LET lc_prepare = " EXECUTE PROCEDURE safre_af:marca_cuenta(?,?,?,?,?,?,?,?)"
   PREPARE eje_marca FROM lc_prepare

   CASE pr_datos.tipo_retiro
       WHEN "A"
          LET ls_marca = 800
       WHEN "B"
          IF (pr_datos.tipo_seguro = "RJ" AND pr_datos.tipo_prestacion = "1" AND 
             (pr_datos.tipo_pension = "AS" OR pr_datos.tipo_pension = "OR"
              OR pr_datos.tipo_pension = "VI" OR pr_datos.tipo_pension = "VO"
              OR pr_datos.tipo_pension = "IP" OR pr_datos.tipo_pension = "RE"
              OR pr_datos.tipo_pension = "CE" OR pr_datos.tipo_pension = "VE"
              OR pr_datos.tipo_pension = "IN" OR pr_datos.tipo_pension = "PP")) THEN
              LET ls_marca = 806
          ELSE
              LET ls_marca = 810
          END IF
       WHEN "C"
          LET ls_marca = 815
       WHEN "U"
          LET ls_marca = 805
   END CASE
   
   # Quitando la marca de Transferencias
      LET gc_prepare = "EXECUTE  PROCEDURE  desmarca_cuenta(?,?,?,?,?,?)"
      LET gc_prepare =  gc_prepare   CLIPPED
      PREPARE  p_desmarca_rech   FROM  gc_prepare
      EXECUTE  p_desmarca_rech  USING  pr_datos.nss,
                                       ls_marca,
                                       pr_datos.consecutivo,
                                       CERO,
                                       ls_marca,
                                       gc_usuario
    
    LET li_sol_saldo = pr_datos.consec_sol_saldo

    SELECT "X" FROM safre_af:ret_reinversion
    WHERE id_solicitud_saldo = li_sol_saldo
    GROUP BY 1

   -- NO existe sol de reinversion para evitar duplicar #CPL-1567
   IF SQLCA.SQLCODE = 100 AND li_sol_saldo IS NOT NULL THEN

      LET lr_reinversion.id_reinversion = 0
      LET lr_reinversion.id_solicitud_saldo = li_sol_saldo
      LET lr_reinversion.fecha_registro = CURRENT
      LET lr_reinversion.id_tipo_origen_reinv = 8
      LET lr_reinversion.estado_solicitud = gr_edo.sol_reinversion

       # Insertando la solicitud de reinversión
      WHENEVER ERROR CONTINUE
      SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
      LOCK TABLE ret_reinversion IN EXCLUSIVE MODE;
      
      INSERT INTO safre_af:ret_reinversion VALUES(lr_reinversion.*)
      IF SQLCA.SQLERRD[3] = 0 THEN
         LET gc_error = "NO SE INSERTÓ LA REINVERSION POR RESOLUCION RECHAZADA, NSS : ", pr_datos.nss CLIPPED
         CALL ERRORLOG(gc_error CLIPPED)
         RETURN
      END IF
      
      SET LOCK MODE TO NOT WAIT;
      UNLOCK TABLE ret_reinversion;
      WHENEVER ERROR STOP

      --EXECUTE p_upd_solicitud USING pr_datos.nss, gr_edo.liquida_saldo , li_sol_saldo

      # Marcando la cuenta con 922-REINVERTIDOS
      LET ls_marca  = 922
      
      DECLARE cur_reinv CURSOR FOR eje_marca
      OPEN cur_reinv USING 
               pr_datos.nss,
               ls_marca,
               pr_datos.consec_sol_saldo,
               CERO,     
               CERO,     
               ls_marca,  
               HOY,       
               gc_usuario
      FETCH cur_reinv INTO ls_marca, ls_cod_rechazo
      
      IF ls_cod_rechazo != 0 THEN
         LET gc_error = "NO SE MARCÓ 922-REINVERTIDOS, NSS : ", pr_datos.nss CLIPPED
         CALL ERRORLOG(gc_error CLIPPED)  
      END IF
      
      CLOSE cur_reinv 
   END IF
END FUNCTION