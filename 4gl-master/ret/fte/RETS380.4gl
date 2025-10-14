#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa RETS380 => ANEXO 138.                                             #
#Sistema           => RET                                                   #
#Autor             => Luis Eduardo Avila Mojica                             #
#Fecha             => 23 ENERO  DE 2020                                     #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE gdt_fecha_ini               DATE
    DEFINE gdt_fecha_fin               DATE
    DEFINE gc_nombre_anexo             CHAR(050)
    
    DEFINE gs_codigo_afore             SMALLINT
    DEFINE gs_bnd_argumentos           SMALLINT
    
    DEFINE G_LISTA_DET1                CHAR(050)
    DEFINE G_LISTA_DET2                CHAR(050)
    DEFINE G_LISTA_DET3                CHAR(050)
    DEFINE mc_comando                  CHAR(500)
    
    DEFINE HOY                         DATE
    
    DEFINE gr_detalle1  RECORD
        tipo_registro      CHAR(002),
        curp               CHAR(018),
        fecha_sol_ret      CHAR(008),
        fecha_sol_cert_afo CHAR(008),
        fecha_emision      CHAR(008),
        clasif_derecho     CHAR(001),
        monto_rcv_antes    CHAR(013),
        fecha_liquidacion  CHAR(008),
        monto_pago         CHAR(013),
        promedio_sueldo    CHAR(013),
        regimen            CHAR(001),
        tipo_admin         CHAR(001),
        anio_bimes_coti    CHAR(003),
        num_concesion      CHAR(006),
        medio_sol          CHAR(003),
        forma_pago         CHAR(003),
        filler             CHAR(001)
    END RECORD
    
    DEFINE gr_detalle2  RECORD
        tipo_registro      CHAR(002),
        curp               CHAR(018),
        fecha_sol_ret      CHAR(008),
        diag_certificacion CHAR(003),
        num_concesion      CHAR(006),
        medio_sol          CHAR(003),
        filler             CHAR(070)
    END RECORD
    
    DEFINE gr_detalle3  RECORD
        tipo_registro      CHAR(002),
        curp               CHAR(018),
        num_concesion      CHAR(006),
        fecha_sol_ret      CHAR(008),
        fecha_liquidacion  CHAR(008),
        fecha_confir_pago  CHAR(008),
        imp_pagado         CHAR(013),
        clasif_derecho     CHAR(001),
        medio_sol          CHAR(003),
        forma_pago         CHAR(003),
        filler             CHAR(040)
    END RECORD
    
    DEFINE gar_precio_acc ARRAY [99] OF RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP              ,
        PROMPT LINE LAST        ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL fn_inicio()
    CALL fn_abre_ventana()

    IF (gs_bnd_argumentos = 2) THEN
        CALL fn_carga_informacion(gdt_fecha_ini, gdt_fecha_fin)
        CALL fn_inizializa_sql()
        CALL fn_genera_anexo()
    ELSE
        CALL f_lib_error_msg("NUMERO INCORRECTO DE ARGUMENTOS")
    END IF

    CLOSE WINDOW win_anexo_138

END MAIN

#################################################################################
#Función                        : inicio()                                      #
#Objetivo                       : Función que inicializa las variables globales #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : gr_seg_modulo ,                               #
#                                 gs_codigo_afore ,                             #
#                                 gdt_fecha_ini ,                               #
#                                 gdt_fecha_fin                                 #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : Luis Eduardo Avila Mojica                     #
#Fecha                          : 23/01/2020                                    #
#################################################################################
FUNCTION fn_inicio()

    DEFINE ls_count      SMALLINT
    DEFINE ls_estatus    SMALLINT

    -- Inicialización de variables
    LET gs_bnd_argumentos   = NUM_ARGS()

    IF (gs_bnd_argumentos = 2) THEN
        LET gdt_fecha_ini           = ARG_VAL(1)
        LET gdt_fecha_fin           = ARG_VAL(2)
    END IF
    
    LET HOY = TODAY

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local
    GROUP BY 1
    
    FOR ls_count = 0 TO 10
        LET gc_nombre_anexo = HOY USING "YYYYMMDD", "_AF_", gs_codigo_afore USING "&&&", "_018_",ls_count USING "&&&&&",".009.gpg"
        
        LET mc_comando  = " "
        LET mc_comando = "ls ", gr_seg_modulo.ruta_envio    CLIPPED, "/",gc_nombre_anexo
        LET mc_comando  = mc_comando CLIPPED
        RUN mc_comando RETURNING ls_estatus
        
        IF ls_estatus <> 0 THEN
            EXIT FOR
        END IF
    END FOR

END FUNCTION

#---------------------------------------------------------------------------#
# fn_abre_ventana : Abre la forma donde se depliegan los mensajes de al     #
#                  usuario en la generacion del anexo                       #
#---------------------------------------------------------------------------#
FUNCTION fn_abre_ventana()

    OPEN WINDOW win_anexo_138 AT 4,4 WITH 20 ROWS, 75 COLUMNS
        ATTRIBUTE(BORDER, PROMPT LINE LAST -1)

    DISPLAY " Ctrl-C : Salir                                            Esc : Ejecutar  " AT 1,2 ATTRIBUTE(REVERSE)
    DISPLAY " RETS380            GENERACION DEL ANEXO 138                               " AT 3,2 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    -- -----------------------------------------------------------------------------

    CURRENT WINDOW IS win_anexo_138

END FUNCTION

#-------------------------------------------------------------------------------#
# fn_carga_informacion()                                                        #
#-------------------------------------------------------------------------------#
FUNCTION fn_carga_informacion(pdt_fecha_ini, pdt_fecha_fin)

    DEFINE pdt_fecha_ini        DATE
    DEFINE pdt_fecha_fin        DATE

    -- -----------------------------------------------------------------------------

    DISPLAY " OBTENIENDO DATOS DE LA CUENTA INDIVIDUAL - ANEXO 138 ...   " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dis_cuenta
    
        SELECT *
        FROM   dis_cuenta
        WHERE  1 = 2 
        INTO TEMP tmp_dis_cuenta
        
        ------------------------------------------------------------------------

        DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE)
        DISPLAY " VALIDANDO REGISTROS dis_cuenta                             " AT 18,1 ATTRIBUTE(REVERSE)
      
        LET mc_comando = "INSERT INTO tmp_dis_cuenta       ",
                         "  SELECT *                       ",
                         "  FROM dis_cuenta                ",
                         "  WHERE    tipo_movimiento = 856 ",
                         "    AND fecha_conversion BETWEEN '",pdt_fecha_ini,"' and '",pdt_fecha_fin,"' ",
                         "    AND nss IN (SELECT UNIQUE nss ",
                         "                     FROM ret_parcial_issste ",
                         "                   WHERE estado_solicitud = 80) "

      PREPARE sql_totales FROM mc_comando
      
      IF SQLCA.SQLCODE != 0 THEN  
         DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE)
         DISPLAY " ERROR AL INSERTAR LA INFORMACION VER LOG DE ERRORES        " AT 18,1 ATTRIBUTE(REVERSE)
         CALL ERRORLOG(SQLCA.SQLCODE||"ERROR AL INSERTAR EN LA TABLA TEMPORAL tmp_dis_cuenta")
         EXIT PROGRAM
      END IF
      
      EXECUTE  sql_totales

      IF SQLCA.SQLCODE != 0 THEN
         DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE)
         DISPLAY " ERROR AL INSERTAR LA INFORMACION VER LOG DE ERRORES        " AT 18,1 ATTRIBUTE(REVERSE)
         CALL ERRORLOG(SQLCA.SQLCODE||"ERROR AL INSERTAR EN LA TABLA TEMPORAL tmp_dis_cuenta")
         EXIT PROGRAM
      END IF

    WHENEVER ERROR STOP
    

END FUNCTION

FUNCTION fn_inizializa_sql()

    DEFINE v_query         CHAR(5000)
    
    -- DETALLE 1
    -- Recupera los NSS a reportar en el detalle 1
    LET v_query = "\n SELECT UNIQUE nss,              ",
                  "\n               folio,            ",
                  "\n               consecutivo_lote, ",
                  "\n               fecha_conversion  ",
                  "\n     FROM tmp_dis_cuenta         ",
                  "\n   ORDER BY fecha_conversion     "
    LET v_query = v_query CLIPPED
    PREPARE prp_det1 FROM v_query
    DECLARE cur_det1 CURSOR FOR prp_det1
    
    -- Recupera CURP, fecha, regimen y concesion asociado al nss, folio y consecutivo
    LET v_query = "\n SELECT curp,                 ",
                  "\n        fecha_captura,        ",
                  "\n        regimen,              ",
                  "\n        num_concesion         ",
                  "\n     FROM ret_parcial_issste  ",
                  "\n   WHERE nss              = ? ",
                  "\n     AND folio            = ? ",
                  "\n     AND consecutivo      = ? ",
                  "\n     AND estado_solicitud = 80 "
    LET v_query = v_query CLIPPED
    PREPARE prp_info_ret_par_issste FROM v_query

    -- Recupera Fecha Solicitud de Certificación
    LET v_query = "\n SELECT fecha_envio         ",
                  "\n   FROM ret_ctr_envio       ",
                  "\n  WHERE folio           = ? ",
                  "\n    AND tipo_operacion  = 'AV52' "
    LET v_query = v_query CLIPPED
    PREPARE prp_rec_fec_soli FROM v_query
    
    -- Valida Tipo de Administración
    LET v_query = "\n SELECT COUNT(*)                ",
                  "\n     FROM dis_cuenta            ",
                  "\n   WHERE nss              = ?   ",
                  "\n     AND folio            = ?   ",
                  "\n     AND consecutivo_lote = ?   ",
                  "\n     AND subcuenta        <> 19 "
    LET v_query = v_query CLIPPED
    PREPARE prp_val_tip_admin FROM v_query
    
    -- Recupera fecha de emision y salario promedio
    LET v_query = "\n SELECT fecha_resolucion,    ",
                  "\n        NVL(MAX(salario_promedio),0)",
                  "\n     FROM ret_datamart_par_issste ",
                  "\n   WHERE nss              = ?     ",
                  --"\n     AND folio            = ?     ",
                  "\n     AND fecha_resolucion IN ( SELECT MAX(fecha_resolucion)   ",
                  "\n                                 FROM ret_datamart_par_issste ",
                  "\n                                WHERE nss              = ? )  ",
                  "\n GROUP BY 1                                                   ",
                  "\n ORDER BY 1                                                   "
                  --"\n                                  AND folio            = ? )  "
    LET v_query = v_query CLIPPED
    PREPARE prp_fec_emi_sal_pro FROM v_query
    
    -- Valida Clasificación del derecho pagado
    LET v_query = "\n SELECT COUNT(*)  ",
                  "\n     FROM ret_monto_par_issste   ",
                  "\n   WHERE mto_a_pagar = mto_75sbc ",
                  "\n     AND curp        = ?    ",
                  "\n     AND folio       = ?    ",
                  "\n     AND consecutivo = ?    "
    LET v_query = v_query CLIPPED
    PREPARE prp_val_clasif_derec_pag FROM v_query
    
    -- Recupera monto pagado
    LET v_query = "\n SELECT NVL(SUM(monto_en_pesos),0) ",
                  "\n     FROM dis_cuenta               ",
                  "\n   WHERE nss              = ?      ",
                  "\n     AND folio            = ?      ",
                  "\n     AND consecutivo_lote = ?      "
    LET v_query = v_query CLIPPED
    PREPARE prp_monto_pagado FROM v_query
    
    
    -- DETALLE 2
    -- Recupera curp, fecha_captura, diagnostico y concesion detalle 2
    LET v_query = "\n SELECT UNIQUE curp,          ",
                  "\n               fecha_captura, ",
                  "\n               diag_procesar, ",
                  "\n               num_concesion  ",
                  "\n     FROM ret_parcial_issste  ",
                  "\n   WHERE folio IN (SELECT UNIQUE folio ",
                  "\n                       FROM ret_cza_lote ",
                  "\n                     WHERE fecha_carga  BETWEEN ? AND ? ) ",
                  "\n     AND estado_solicitud = 90 ",
                  "\n     AND diag_procesar <> '400' " --CPL-4127
    LET v_query = v_query CLIPPED
    PREPARE prp_det2 FROM v_query
    DECLARE cur_det2 CURSOR FOR prp_det2
    
    
    -- DETALLE 3
    LET v_query = "\n SELECT UNIQUE rpi.nss,           ",
                  "\n               rpi.folio,          ",
                  "\n               rpi.consecutivo,    ",
                  "\n               rpi.curp,           ",
                  "\n               rpi.fecha_captura,  ",
                  "\n               rpi.num_concesion,  ",
                  "\n               rce.fecha_envio     ",
                  "\n     FROM ret_parcial_issste rpi, ret_ctr_envio rce ",
                  "\n   WHERE rpi.folio = rce.folio     ",
                  "\n     AND rpi.estado_solicitud = 80 ",
                  "\n     AND rce.fecha_envio BETWEEN ? AND ? ",
                  "\n     AND rce.tipo_operacion  = 'AV56'    ", 
                  "\n   ORDER BY 7                            "
    LET v_query = v_query CLIPPED
    PREPARE prp_det3 FROM v_query
    DECLARE cur_det3 CURSOR FOR prp_det3
    
    -- Recupera fecha de liquidacion
    LET v_query = "\n SELECT UNIQUE fecha_conversion  ",
                  "\n     FROM dis_cuenta         ",
                  "\n   WHERE nss              = ?    ",
                  "\n     AND folio            = ?    ",
                  "\n     AND consecutivo_lote = ?    "
    LET v_query = v_query CLIPPED
    PREPARE prp_fec_liq FROM v_query
    
    
    
END FUNCTION

#-------------------------------------------------------------------------------#
# fn_genera_anexo()                                                             #
#-------------------------------------------------------------------------------#
FUNCTION fn_genera_anexo()


    CALL fn_genera_det1()      #-- Genera el detalle 1 del anexo
    CALL fn_genera_det2()      #-- Genera el detalle 2 del anexo
    CALL fn_genera_det3()      #-- Genera el detalle 3 del anexo

    CALL fn_concatena_arch()  #-- Concatena los archivos
    
END FUNCTION

FUNCTION fn_genera_det1()

    DEFINE lr_dis RECORD
        nss             LIKE dis_cuenta.nss,
        folio           LIKE dis_cuenta.folio,
        consecutivo     LIKE dis_cuenta.consecutivo_lote
    END RECORD
    
    DEFINE ldf_fecha_sol_ret         DATE
    DEFINE ldf_fecha_sol_cert        DATE
    DEFINE ldf_fecha_emision         DATE
    DEFINE ldf_fecha_liquidacion     DATE
    
    DEFINE ld_monto_rcv              DECIMAL(13,2)
    DEFINE ld_monto_pagado           DECIMAL(13,2)
    DEFINE ld_promedio_sueldo        DECIMAL(13,2)
    
    DEFINE lc_monto_rcv              CHAR(014)
    DEFINE lc_monto_pagado           CHAR(014)
    DEFINE lc_promedio_sueldo        CHAR(014)
    DEFINE lc_regimen                CHAR(002)
    DEFINE ls_val_tip_admin          SMALLINT
    DEFINE ls_val_clasif_derec       SMALLINT
    DEFINE lc_num_concesion          CHAR(10)
    
    INITIALIZE lr_dis.*, gr_detalle1.* TO NULL
    LET ld_monto_rcv          = 0
    LET ld_monto_pagado       = 0
    LET ld_promedio_sueldo    = 0
    LET gr_detalle1.tipo_registro  = '01'
    LET gr_detalle1.anio_bimes_coti = 0
    LET gr_detalle1.anio_bimes_coti = gr_detalle1.anio_bimes_coti USING "&&&"
    LET gr_detalle1.filler = 7 SPACE
    
    LET ls_val_tip_admin    = 0
    LET ls_val_clasif_derec = 0

    LET G_LISTA_DET1 = gr_seg_modulo.ruta_envio CLIPPED, "/DET1_ANEXO_138"
    LET G_LISTA_DET1 = G_LISTA_DET1 CLIPPED
    
    START REPORT rpt_det1 TO G_LISTA_DET1
        FOREACH cur_det1 INTO lr_dis.*,
                              ldf_fecha_liquidacion
        
            LET gr_detalle1.fecha_liquidacion = ldf_fecha_liquidacion USING "YYYYMMDD"
            -- Se recupera CURP, fecha, regimen y concesion asociado al nss, folio y consecutivo
            EXECUTE prp_info_ret_par_issste USING lr_dis.nss   ,
                                                  lr_dis.folio ,
                                                  lr_dis.consecutivo
                                             INTO gr_detalle1.curp   ,
                                                  ldf_fecha_sol_ret  ,
                                                  lc_regimen         ,
                                                  lc_num_concesion

            LET lc_num_concesion = lc_num_concesion USING "&&&&&&&&&&"
            LET gr_detalle1.num_concesion = lc_num_concesion[05,10]

            LET gr_detalle1.fecha_sol_ret      = ldf_fecha_sol_ret USING "YYYYMMDD"

            -- Recupera Fecha Solicitud de Certificación
            EXECUTE prp_rec_fec_soli USING lr_dis.folio
                                      INTO ldf_fecha_sol_cert
                        
            LET gr_detalle1.fecha_sol_cert_afo = ldf_fecha_sol_cert USING "YYYYMMDD"

            IF lc_regimen = 'DT' THEN
            	  LET gr_detalle1.regimen = 1
            ELSE
            	  LET gr_detalle1.regimen = 2
            END IF
            
            -- Valida Tipo de Administración
            EXECUTE prp_val_tip_admin USING lr_dis.nss   ,
                                            lr_dis.folio ,
                                            lr_dis.consecutivo
                                       INTO ls_val_tip_admin
            IF ls_val_tip_admin > 0 THEN
                LET gr_detalle1.tipo_admin = 2
            ELSE
                LET gr_detalle1.tipo_admin = 1
            END IF
            
            -- Se recupera fecha de emision y salario promedio
            EXECUTE prp_fec_emi_sal_pro USING lr_dis.nss   ,
                                              lr_dis.nss
                                         INTO ldf_fecha_emision,
                                              ld_promedio_sueldo
            LET gr_detalle1.fecha_emision = ldf_fecha_emision USING "YYYYMMDD"
            LET lc_promedio_sueldo = ld_promedio_sueldo USING "&&&&&&&&&&&.&&"
            LET gr_detalle1.promedio_sueldo = lc_promedio_sueldo[01,11],
                                              lc_promedio_sueldo[13,14]

            -- Se recupera el monto de rcv
            CALL fn_recupera_rcv(lr_dis.nss, gr_detalle1.curp, lr_dis.folio, lr_dis.consecutivo)
                                RETURNING ld_monto_rcv
            LET lc_monto_rcv = ld_monto_rcv USING "&&&&&&&&&&&.&&"
            LET gr_detalle1.monto_rcv_antes = lc_monto_rcv[01,11],
                                              lc_monto_rcv[13,14]
            
            -- Se valida Clasificación del derecho pagado
            EXECUTE prp_val_clasif_derec_pag USING gr_detalle1.curp   ,
                                                   lr_dis.folio ,
                                                   lr_dis.consecutivo
                                              INTO ls_val_clasif_derec
            IF ls_val_clasif_derec > 0 THEN
                LET gr_detalle1.clasif_derecho = 1
            ELSE
                LET gr_detalle1.clasif_derecho = 2
            END IF
            
            -- Se recupera el monto pagado
            EXECUTE prp_monto_pagado USING lr_dis.nss   ,
                                           lr_dis.folio ,
                                           lr_dis.consecutivo
                                      INTO ld_monto_pagado
            LET lc_monto_pagado = ld_monto_pagado USING "&&&&&&&&&&&.&&"
            LET gr_detalle1.monto_pago = lc_monto_pagado[01,11],
                                         lc_monto_pagado[13,14]

            LET gr_detalle1.medio_sol = "001"            #Presencial
            CALL fn_obtiene_forma_pago(lr_dis.nss, lr_dis.consecutivo) 
                                      RETURNING gr_detalle1.forma_pago
    
            OUTPUT TO REPORT rpt_det1(gr_detalle1.*)
    
        END FOREACH
    FINISH REPORT rpt_det1
    
    
END FUNCTION

FUNCTION fn_recupera_rcv(p_nss, p_curp, p_folio, p_consecutivo)

    DEFINE p_nss           LIKE ret_parcial_issste.nss
    DEFINE p_curp          LIKE ret_parcial_issste.curp
    DEFINE p_folio         LIKE ret_parcial_issste.folio
    DEFINE p_consecutivo   LIKE ret_parcial_issste.consecutivo
    
    DEFINE ls_siefore      LIKE ret_monto_issste.siefore
    DEFINE ls_sie          SMALLINT
    
    DEFINE ld_acc_ant      LIKE dis_cuenta.monto_en_acciones
    DEFINE ld_monto_rcv    DECIMAL(13,2)
    
    DEFINE lr_liquida RECORD
        subcta              LIKE dis_cuenta.subcuenta           , 
        mto_acc             LIKE dis_cuenta.monto_en_acciones   , 
        fec_conver          LIKE dis_cuenta.fecha_conversion
    END RECORD
    
    LET ld_acc_ant      = 0
    LET ld_monto_rcv    = 0
    
    SELECT UNIQUE siefore
      INTO ls_siefore
      FROM dis_cuenta
     WHERE curp             = p_curp
       AND folio            = p_folio
       AND consecutivo_lote = p_consecutivo
       --AND subcuenta IN (30,31,32)
       
     LET ls_sie              = ls_siefore
     
-- Ciclo para obtener el monto pagado al trabajador (Se cambia el signo ya que es un retiro)
        DECLARE cur_liquida CURSOR FOR 
            SELECT subcuenta        , 
                   monto_en_acciones,
                   fecha_conversion
            FROM   dis_cuenta
            WHERE  curp             = p_curp
            AND    siefore          = ls_siefore
            AND    folio            = p_folio            

        FOREACH cur_liquida INTO lr_liquida.*
        
            CALL fn_obtiene_precios_accion(lr_liquida.fec_conver)
            
            -- Acumulamos el monto antes del retiro
            SELECT NVL(SUM(monto_en_acciones),0) * gar_precio_acc[ls_sie].precio_dia
            INTO   ld_acc_ant
            FROM   dis_cuenta
            WHERE  nss              =  p_nss
            AND    siefore          =  ls_siefore
            AND    folio            <> p_folio
            AND    fecha_conversion <= lr_liquida.fec_conver
            AND    subcuenta        =  lr_liquida.subcta
            
            LET ld_monto_rcv  = ld_monto_rcv + ld_acc_ant

        END FOREACH -- Monto pagado
        
        RETURN ld_monto_rcv
        
END FUNCTION

#---------------------------------------------------------------------------#
# fn_obtiene_precios_accion : Obtiene los precios de accion a la fecha dada #
#---------------------------------------------------------------------------#
FUNCTION fn_obtiene_precios_accion(fecha_precios)

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

    LET ls_sie = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion_isss(?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

    IF lr_precio_acc.estado <> 0 THEN
        LET lc_siefore = lr_precio_acc.siefore
    
        LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                         " -- SIEFORE ", lc_siefore CLIPPED
        DISPLAY lc_mensaje AT 17,1 ATTRIBUTE(REVERSE)
    
        --PROMPT lc_mensaje FOR CHAR enter
        --EXIT PROGRAM
    ELSE
        LET ls_sie                    = lr_precio_acc.siefore
        LET gar_precio_acc[ls_sie].*  = lr_precio_acc.*
    END IF


    END FOREACH

END FUNCTION

#----------------------------------------------------------------------------------------------#
# fn_obtiene_forma_pago : Obtiene la forma de pago considerando equivalencias en los catalogos #
#----------------------------------------------------------------------------------------------#
FUNCTION fn_obtiene_forma_pago(p_nss, p_consecutivo)

    DEFINE
        p_nss               CHAR(11),
        p_consecutivo       DECIMAL (11,0)
    
    DEFINE ls_tipo_pago     SMALLINT
    DEFINE lc_forma_pago    CHAR(003)
    
    SELECT UNIQUE tipo_pago
      INTO ls_tipo_pago
      FROM ret_beneficiario
     WHERE nss = p_nss
       AND consecutivo = p_consecutivo
     
     IF ls_tipo_pago = 5 THEN
     	  LET lc_forma_pago = "001"       #Cheque
     ELSE
     	  IF ls_tipo_pago = 1 THEN
     	  	  LET lc_forma_pago = "002"   #Orden de Pago
     	  ELSE
     	  	  LET lc_forma_pago = "003"   #Transferencia
     	  END IF
     END IF
     
     RETURN lc_forma_pago

END FUNCTION

#---------------------------------------------------------------------------#
# rpt_det1 : Reporte que genera el detalle 1 del anexo 138                  #
#---------------------------------------------------------------------------#
REPORT rpt_det1(pr_detalle)

    DEFINE pr_detalle  RECORD
        tipo_registro      CHAR(002),
        curp               CHAR(018),
        fecha_sol_ret      CHAR(008),
        fecha_sol_cert_afo CHAR(008),
        fecha_emision      CHAR(008),
        clasif_derecho     CHAR(001),
        monto_rcv_antes    CHAR(013),
        fecha_liquidacion  CHAR(008),
        monto_pago         CHAR(013),
        promedio_sueldo    CHAR(013),
        regimen            CHAR(001),
        tipo_admin         CHAR(001),
        anio_bimes_coti    CHAR(003),
        num_concesion      CHAR(006),
        medio_sol          CHAR(003),
        forma_pago         CHAR(003),
        filler             CHAR(001)
    END RECORD


    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

        PRINT
            COLUMN 001, pr_detalle.tipo_registro                          , -- Tipo de registro
            COLUMN 003, pr_detalle.curp                                   , -- CURP
            COLUMN 021, pr_detalle.fecha_sol_ret                          , -- Fecha de Solicitud del retiro
            COLUMN 029, pr_detalle.fecha_sol_cert_afo                     , -- Fecha de Solicitud de certificación Afore
            COLUMN 037, pr_detalle.fecha_emision                          , -- Fecha de emisión de resolución
            COLUMN 045, pr_detalle.clasif_derecho                         , -- Clasificación del derecho pagado
            COLUMN 046, pr_detalle.monto_rcv_antes                        , -- Monto total de RCV antes del retiro
            COLUMN 059, pr_detalle.fecha_liquidacion                      , -- Fecha de Liquidación o Pago
            COLUMN 067, pr_detalle.monto_pago                             , -- Monto Pagado 
            COLUMN 080, pr_detalle.promedio_sueldo                        , -- Promedio del Sueldo Básico de los últimos 5 años
            COLUMN 093, pr_detalle.regimen                                , -- Régimen Trabajador
            COLUMN 094, pr_detalle.tipo_admin                             , -- Tipo de Administración
            COLUMN 095, pr_detalle.anio_bimes_coti                        , -- Años de Bimestres de cotización a descontar
            COLUMN 098, pr_detalle.num_concesion                          , -- Numero de Concesión
            COLUMN 104, pr_detalle.medio_sol                              , -- Medio de Solicitud
            COLUMN 107, pr_detalle.forma_pago                             , -- Forma de Pago
            COLUMN 110, pr_detalle.filler                                   -- Filler
            
END REPORT

FUNCTION fn_genera_det2()
    
    DEFINE ldf_fecha_sol_ret         DATE
    DEFINE lc_comando                CHAR(500)
    
    INITIALIZE gr_detalle2.* TO NULL
    LET gr_detalle2.tipo_registro  = '02'
    LET gr_detalle2.filler = 70 SPACE

    LET G_LISTA_DET2 = gr_seg_modulo.ruta_envio CLIPPED, "/DET2_ANEXO_138"
    LET G_LISTA_DET2 = G_LISTA_DET2 CLIPPED
    
    START REPORT rpt_det2 TO G_LISTA_DET2
        FOREACH cur_det2 USING gdt_fecha_ini ,
                               gdt_fecha_fin
                          INTO gr_detalle2.curp          ,
                               ldf_fecha_sol_ret         ,
                               gr_detalle2.diag_certificacion ,
                               gr_detalle2.num_concesion
            LET gr_detalle2.fecha_sol_ret      = ldf_fecha_sol_ret USING "YYYYMMDD"
            LET gr_detalle2.diag_certificacion = gr_detalle2.diag_certificacion USING "&&&"
            LET gr_detalle2.medio_sol = "001"
    
            OUTPUT TO REPORT rpt_det2(gr_detalle2.*)
    
        END FOREACH
    FINISH REPORT rpt_det2
    
    
END FUNCTION

#---------------------------------------------------------------------------#
# rpt_det2 : Reporte que genera el detalle 2 del anexo 138                  #
#---------------------------------------------------------------------------#
REPORT rpt_det2(pr_detalle)

    DEFINE pr_detalle  RECORD
        tipo_registro      CHAR(002),
        curp               CHAR(018),
        fecha_sol_ret      CHAR(008),
        diag_certificacion CHAR(003),
        num_concesion      CHAR(006),
        medio_sol          CHAR(003),
        filler             CHAR(070)
    END RECORD


    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

        PRINT
            COLUMN 001, pr_detalle.tipo_registro                          , -- Tipo de registro
            COLUMN 003, pr_detalle.curp                                   , -- CURP
            COLUMN 021, pr_detalle.fecha_sol_ret                          , -- Fecha de Solicitud del retiro
            COLUMN 029, pr_detalle.diag_certificacion                     , -- Diagnóstico de certificación del derecho
            COLUMN 032, pr_detalle.num_concesion                          , -- Numero de Concesión
            COLUMN 038, pr_detalle.medio_sol                              , -- Medio de Solicitud
            COLUMN 041, pr_detalle.filler                                   -- Filler
            
END REPORT

FUNCTION fn_genera_det3()

    DEFINE lr_datos RECORD
        nss            LIKE dis_cuenta.nss,
        folio           LIKE dis_cuenta.folio,
        consecutivo     LIKE dis_cuenta.consecutivo_lote
    END RECORD
    
    DEFINE ld_monto_pagado           DECIMAL(13,2)
    
    DEFINE lc_monto_pagado           CHAR(014)
    DEFINE lc_comando                CHAR(500)
    
    DEFINE ldf_fecha_sol_ret         DATE
    DEFINE ldf_fecha_liquidacion     DATE
    DEFINE ldf_fecha_conf_pago       DATE
    
    DEFINE ls_val_clasif_derec       SMALLINT
    DEFINE lc_num_concesion          CHAR(10)
    
    INITIALIZE lr_datos.*, gr_detalle3.* TO NULL
    LET gr_detalle3.tipo_registro  = '03'
    LET gr_detalle3.filler         = 46 SPACE
    LET ls_val_clasif_derec        = 0

    LET G_LISTA_DET3 = gr_seg_modulo.ruta_envio CLIPPED, "/DET3_ANEXO_138"
    LET G_LISTA_DET3 = G_LISTA_DET3 CLIPPED
    
    START REPORT rpt_det3 TO G_LISTA_DET3
        FOREACH cur_det3 USING gdt_fecha_ini ,
                               gdt_fecha_fin
                          INTO lr_datos.*                ,
                               gr_detalle3.curp          ,
                               ldf_fecha_sol_ret   ,
                               lc_num_concesion    ,
                               ldf_fecha_conf_pago
                                   
            LET gr_detalle3.fecha_confir_pago = ldf_fecha_conf_pago USING "YYYYMMDD"

            LET lc_num_concesion = lc_num_concesion USING "&&&&&&&&&&"
            LET gr_detalle3.num_concesion = lc_num_concesion[05,10]
        
            LET gr_detalle3.fecha_sol_ret      = ldf_fecha_sol_ret USING "YYYYMMDD"
            
            -- Se recupera fecha de liquidacion
            EXECUTE prp_fec_liq USING lr_datos.nss   ,
                                      lr_datos.folio ,
                                      lr_datos.consecutivo
                                 INTO ldf_fecha_liquidacion
            LET gr_detalle3.fecha_liquidacion = ldf_fecha_liquidacion USING "YYYYMMDD"
            
            -- Se valida Clasificación del derecho pagado
            EXECUTE prp_val_clasif_derec_pag USING gr_detalle3.curp   ,
                                                   lr_datos.folio ,
                                                   lr_datos.consecutivo
                                              INTO ls_val_clasif_derec
            IF ls_val_clasif_derec > 0 THEN
                LET gr_detalle3.clasif_derecho = 1
            ELSE
                LET gr_detalle3.clasif_derecho = 2
            END IF
            
            -- Se recupera el monto pagado
            EXECUTE prp_monto_pagado USING lr_datos.nss   ,
                                           lr_datos.folio ,
                                           lr_datos.consecutivo
                                      INTO ld_monto_pagado
            LET lc_monto_pagado = ld_monto_pagado USING "&&&&&&&&&&&.&&"
            LET gr_detalle3.imp_pagado = lc_monto_pagado[01,11],
                                         lc_monto_pagado[13,14]
            
            LET gr_detalle3.medio_sol = "001"            #Presencial
            CALL fn_obtiene_forma_pago(lr_datos.nss, lr_datos.consecutivo) 
                                      RETURNING gr_detalle3.forma_pago
    
            OUTPUT TO REPORT rpt_det3(gr_detalle3.*)
    
        END FOREACH
    FINISH REPORT rpt_det3
    
    
END FUNCTION

#---------------------------------------------------------------------------#
# rpt_det3 : Reporte que genera el detalle 3 del anexo 138                  #
#---------------------------------------------------------------------------#
REPORT rpt_det3(pr_detalle)

    DEFINE pr_detalle  RECORD
        tipo_registro      CHAR(002),
        curp               CHAR(018),
        num_concesion      CHAR(006),
        fecha_sol_ret      CHAR(008),
        fecha_liquidacion  CHAR(008),
        fecha_confir_pago  CHAR(008),
        imp_pagado         CHAR(013),
        clasif_derecho     CHAR(001),
        medio_sol          CHAR(003),
        forma_pago         CHAR(003),
        filler             CHAR(040)
    END RECORD


    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

        PRINT
            COLUMN 001, pr_detalle.tipo_registro                          , -- Tipo de registro
            COLUMN 003, pr_detalle.curp                                   , -- CURP
            COLUMN 021, pr_detalle.num_concesion                          , -- Numero de Concesión
            COLUMN 027, pr_detalle.fecha_sol_ret                          , -- Fecha de Solicitud del retiro
            COLUMN 035, pr_detalle.fecha_liquidacion                      , -- Fecha de la Liquidación de Recursos al trabajador
            COLUMN 043, pr_detalle.fecha_confir_pago                      , -- Fecha en la que la administradora confirmó el pago a Procesar
            COLUMN 051, pr_detalle.imp_pagado                             , -- Monto pagado al trabajador
            COLUMN 064, pr_detalle.clasif_derecho                         , -- Clasificación del pago que se le realizó al trabajador
            COLUMN 065, pr_detalle.medio_sol                              , -- Medio de Solicitud
            COLUMN 068, pr_detalle.forma_pago                             , -- Forma de Pago
            COLUMN 071, pr_detalle.filler                                   -- Filler
            
END REPORT

#---------------------------------------------------------------------------#
# fn_concatena_arch : Concatena los archivos generados                      #
#---------------------------------------------------------------------------#
FUNCTION fn_concatena_arch()

    DEFINE
        comando     ,
        cat         CHAR(500)

    LET cat = ""
    LET cat = "cat ", G_LISTA_DET1 CLIPPED, " ",
                      G_LISTA_DET2 CLIPPED, " ",
                      G_LISTA_DET3 CLIPPED,
                     " > ",gr_seg_modulo.ruta_envio CLIPPED,"/",gc_nombre_anexo CLIPPED

    LET cat = cat CLIPPED
    RUN cat

    WHENEVER ERROR CONTINUE

        LET comando = "chmod 777 ", G_LISTA_DET1 CLIPPED
        RUN comando

        LET comando = "chmod 777 ", G_LISTA_DET2 CLIPPED
        RUN comando

        LET comando = "chmod 777 ", G_LISTA_DET3 CLIPPED
        RUN comando

        LET comando = "chmod 777 ", gr_seg_modulo.ruta_envio CLIPPED,"/",gc_nombre_anexo CLIPPED
        RUN comando
    
        -- Elimina los archivos temporales que conformaban el reporte
        LET comando = "rm ", G_LISTA_DET1 CLIPPED, " ",
                             G_LISTA_DET2 CLIPPED, " ",
                             G_LISTA_DET3 CLIPPED

        LET comando = comando CLIPPED
        RUN comando

    WHENEVER ERROR STOP

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_crea_log : Abre el archivo de bitacora correspondiente al usuario   #
#                  que ejecuta el programa                                  #
#---------------------------------------------------------------------------#
FUNCTION f_lib_crea_log()

    DEFINE
        lc_nom_programa         VARCHAR(15) ,
        lc_nombre_log           VARCHAR(40)

    -- -----------------------------------------------------------------------------

    LET lc_nom_programa = ARG_VAL(0)
    LET lc_nombre_log   = lc_nom_programa CLIPPED || "_" || f_lib_obten_user() CLIPPED || ".log"

    CALL STARTLOG(lc_nombre_log CLIPPED)

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_error_msg :Formatea y despliega los mensajes de error en la pantalla#
#---------------------------------------------------------------------------#
FUNCTION f_lib_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(75)

    DEFINE
        lc_enter            CHAR

    -- -----------------------------------------------------------------------------

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR lc_enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_obten_user : Obtiene el nombre del usuario activo en el programa    #
#---------------------------------------------------------------------------#
FUNCTION f_lib_obten_user()

    DEFINE
        pc_usuario              CHAR(20)

    -- -----------------------------------------------------------------------------

    SELECT USER
    INTO   pc_usuario
    FROM   SYSTABLES
    WHERE  SYSTABLES.tabid = 1

    RETURN pc_usuario CLIPPED

END FUNCTION