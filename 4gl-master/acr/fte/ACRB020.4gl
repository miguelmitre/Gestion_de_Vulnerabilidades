##############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                            #
#Propietario       => E.F.P.                                                 #
#Programa ACRB020  => GENERA ARCHIVO DE SOLICITUDES RECHAZADAS (CRED. GAR.)  #
#Por               => MAURO MUNIZ CABALLERO                                  #
#Fecha creacion    => 1 DE AGOSTO DE 2002                                    #
#Sistema           => ACR                                                    #
##############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_cza_devol_sol RECORD
        tipo_registro        CHAR(02) ,
        ident_servicio       CHAR(02) ,
        ident_operacion      CHAR(02) ,
        tipo_ent_origen      CHAR(02) ,
        cve_ent_origen       CHAR(03) ,
        tipo_ent_destino     CHAR(02) ,
        cve_ent_destino      CHAR(03) ,
        ent_fed_envio_lote   CHAR(03) ,
        fecha_presentacion   DATE     ,
        consec_lote_dia      SMALLINT ,
        cve_mod_recepcion    CHAR(02) ,
        cod_result_operac    CHAR(02) ,
        mot_rechazo_lote     CHAR(09) 
    END RECORD

    DEFINE reg_det_devol_sol RECORD 
        tipo_registro        CHAR(002)     ,
        cont_servicio        DECIMAL(10,0) ,
        tipo_recep_cuenta    CHAR(002)     ,
        cve_recep_cuenta     CHAR(003)     ,
        tipo_ced_cuenta      CHAR(002)     ,
        cve_ced_cuenta       CHAR(003)     ,
        tipo_transferencia   CHAR(002)     ,
        fecha_presentacion   DATE          ,
        n_unico_infonavit    CHAR(018)     ,
        nss_infonavit        CHAR(011)     ,
        rfc_infonavit        CHAR(013)     ,
        paterno_infonavit    CHAR(040)     ,
        materno_infonavit    CHAR(040)     ,
        nombres_infonavit    CHAR(040)     ,
        ident_lote_devol     CHAR(016)     ,
        nss_afore            CHAR(011)     ,
        rfc_afore            CHAR(013)     ,
        paterno_afore        CHAR(040)     ,
        materno_afore        CHAR(040)     ,
        nombres_afore        CHAR(040)     ,
        ult_aport_viv_97     DECIMAL(15,2) ,
        cod_result_operac    CHAR(002)     ,
        diag_proceso         CHAR(015)     ,
        nombre_imss          CHAR(050)     ,
        num_cred_infonavit   DECIMAL(10,0) ,
        motivo_devolucion    CHAR(002)     ,
        periodo_pago         CHAR(6)
    END RECORD

    DEFINE reg_sum_devol_sol RECORD
         tipo_registro        CHAR(02)      ,
         cantidad_reg_det     DECIMAL(9,0)  ,
         sum_ult_apor_viv97   DECIMAL(15,2)
    END RECORD

    DEFINE g_param_taa RECORD LIKE seg_modulo.*

    DEFINE
        fecha_conver          ,
        fecha_envio           ,
        fecha_pr              ,        
        HOY                   ,
        vfecha_presentacion   DATE

    DEFINE 
        enter            CHAR(1),
        c2_motivo_devol  CHAR(2),
        HORA             CHAR(5),
        c5_HORA          CHAR(5),
        periodo_1        CHAR(6),
        c8_presenta      CHAR(8),
        g_usuario        CHAR(8),
        nom_arch         CHAR(50),
        g_cza            CHAR(100),
        g_det            CHAR(100),
        g_sum            CHAR(100),
        cat              CHAR(300)

    DEFINE 
        existe                ,
        bandera               ,
        dev                   ,
        sw_1                  ,
        s_lotes_num           ,
        s_lotes_correlativo   ,
        s_codigo_afore        SMALLINT,
        contar_devol          ,
        s_contador            INTEGER

    DEFINE 
        ult_aport_v97         DECIMAL(15,6),
        cont_reg              DECIMAL(10,0),
        vsum_ult_ap_viv97     DECIMAL(15,2)

    DEFINE g       RECORD
        total                 INTEGER,
        motivo_devolucion     CHAR(2),
        descripcion           CHAR(30)
    END RECORD

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("ACRB020.log")
    CALL inicio() #i
    CALL proceso_principal() #pp

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ACRB0011 AT 4,4 WITH FORM "ACRB0011" ATTRIBUTE(BORDER)
    DISPLAY " ACRB020    GENERA ARCHIVO SOLIC. CRED. GARAN. DEVUELTAS                       " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY " < CTRL-C > Salir " AT 1,26              
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfecha_presentacion WITHOUT DEFAULTS

        BEFORE FIELD vfecha_presentacion
            LET vfecha_presentacion = HOY

        AFTER FIELD vfecha_presentacion
            IF vfecha_presentacion IS NULL THEN
                ERROR "FECHA CONTESTACION RECHAZOS NO PUEDE SER NULA"
                NEXT FIELD vfecha_presentacion
            ELSE
              SELECT MAX(fecha_presentacion)
              INTO   fecha_pr
              FROM   acr_det_tra_cred
          
              SELECT "OK"
              FROM   safre_tmp:det_devol_cred
              WHERE  fecha_presentacion = fecha_pr
              GROUP BY 1

              IF SQLCA.SQLCODE = 0 THEN
              	 ERROR " NO EXISTEN RECHAZOS A ENVIAR "
              	 NEXT FIELD vfecha_presentacion
              END IF                      	
            END IF
        EXIT INPUT

	ON KEY (INTERRUPT)
	    EXIT PROGRAM
    END INPUT

    WHILE TRUE
        PROMPT "ESTA SEGURO ¿S/N? " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    LET c8_presenta  = vfecha_presentacion USING "YYYYMMDD"
    LET fecha_conver = MDY(MONTH(vfecha_presentacion),
                           1,YEAR(vfecha_presentacion))

    CALL crea_tabla() #ct
    CALL verifica_devolucion() #vd

    SELECT COUNT(*)
    INTO   dev
    FROM   safre_tmp:det_devol_cred

    IF dev > 0 THEN
         DISPLAY "GENERANDO ARCHIVO DE DEVOLUCION " AT 17,1
         CALL genera_cza_devol_sol()    #gcds
         CALL genera_det_devol_sol()    #gdds
         CALL genera_sum_devol_sol()    #gsds

         LET cat = "cat ",g_param_taa.ruta_envio CLIPPED,"/CDA ",
                          g_param_taa.ruta_envio CLIPPED,"/DDA ",
                          g_param_taa.ruta_envio CLIPPED,"/SDA > ",
                          g_param_taa.ruta_envio CLIPPED,"/",
                          "DEV_CRED.",HOY USING "YYMMDD"

         RUN cat

         LET nom_arch = g_param_taa.ruta_envio CLIPPED,"/", 
                        "DEV_CRED.",HOY USING "YYMMDD"

         LET nom_arch = nom_arch CLIPPED

         CALL impresion_reporte()

         DISPLAY "Archivo : ",nom_arch AT 17,1 ATTRIBUTE(REVERSE)

         PROMPT "PROCESO FINALIZADO, [Enter] p/salir " FOR enter
     ELSE
         PROMPT "No hay solicitudes a devolver. Proceso finalizado" FOR enter
     END IF                      

     CLOSE WINDOW ACRB0011

END FUNCTION

FUNCTION inicio()
#i---------------

    LET HOY     = TODAY
    LET HORA    = TIME
    LET c5_HORA = HORA

    SELECT codigo_afore, user
    INTO   s_codigo_afore, g_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_param_taa.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr'

END FUNCTION

FUNCTION crea_tabla()
#ct------------------

    WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
    DROP TABLE det_devol_cred


    CREATE TABLE det_devol_cred
       (
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        tipo_transferencia    CHAR(002)     ,
        fecha_presentacion    DATE          ,
        n_unico_infonavit     CHAR(018)     ,
        nss_infonavit         CHAR(011)     ,
        rfc_infonavit         CHAR(013)     ,
        paterno_infonavit     CHAR(040)     ,
        materno_infonavit     CHAR(040)     ,
        nombres_infonavit     CHAR(040)     ,
        ident_lote_devol      CHAR(016)     ,
        nss_afore             CHAR(011)     ,
        rfc_afore             CHAR(013)     ,
        paterno_afore         CHAR(040)     ,
        materno_afore         CHAR(040)     ,
        nombres_afore         CHAR(040)     ,
        ult_aport_viv_97      DECIMAL(15,02),
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(015)     ,
        nombre_imss           CHAR(050)     ,
        num_cred_infonavit    DECIMAL(10,0) ,
        motivo_devolucion     SMALLINT      ,
        periodo_pago          CHAR(6)
       )

    DATABASE safre_af
    WHENEVER ERROR STOP


END FUNCTION

FUNCTION verifica_devolucion()
#vd---------------------------

    DEFINE suma_v97 DECIMAL(18,6)
    DEFINE suma_v92 DECIMAL(18,6)
    DEFINE suma_tot DECIMAL(18,6)
    DEFINE edo_proc SMALLINT
    DEFINE bnd_proc SMALLINT

    LET bandera      = 0
    LET suma_v97     = 0
    LET suma_v92     = 0
    LET suma_tot     = 0
    LET s_contador   = 1
    LET contar_devol = 1

     
    DECLARE cursor_1 CURSOR FOR
    SELECT tipo_registro      ,
           cont_servicio      ,
           tipo_recep_cuenta  ,
           cve_recep_cuenta   ,
           tipo_ced_cuenta    ,
           cve_ced_cuenta     ,
           tipo_transferencia ,
           fecha_presentacion ,
           n_unico_infonavit  ,
           nss_infonavit      ,
           rfc_infonavit      ,
           paterno_infonavit  ,
           materno_infonavit  ,
           nombres_infonavit  ,
           ident_lote_devol   ,
           nss_afore          ,
           rfc_afore          ,
           paterno_afore      ,
           materno_afore      ,
           nombres_afore      ,
           sdo_viv_97         ,
           cod_result_operac  ,
           diag_proceso       ,
           nombre_imss        ,
           num_cred_infonavit ,
           estado             ,
           periodo_pago  
    FROM   acr_det_tra_cred
    WHERE  estado > 1
    AND    fecha_presentacion = fecha_pr

    FOREACH cursor_1 INTO reg_det_devol_sol.*

        LET reg_det_devol_sol.cont_servicio = contar_devol

        INSERT INTO safre_tmp:det_devol_cred
        VALUES (reg_det_devol_sol.*)

        LET contar_devol = contar_devol + 1
    END FOREACH

END FUNCTION

FUNCTION genera_cza_devol_sol()
#gcds--------------------------

    LET reg_cza_devol_sol.tipo_registro      = "01"
    LET reg_cza_devol_sol.ident_servicio     = "02"
    LET reg_cza_devol_sol.ident_operacion    = "06"
    LET reg_cza_devol_sol.tipo_ent_origen    = "01"
    LET reg_cza_devol_sol.cve_ent_origen     = s_codigo_afore
    LET reg_cza_devol_sol.tipo_ent_destino   = "04"
    LET reg_cza_devol_sol.cve_ent_destino    = "002" 
    LET reg_cza_devol_sol.ent_fed_envio_lote = "009"
    LET reg_cza_devol_sol.fecha_presentacion = c8_presenta
    LET reg_cza_devol_sol.consec_lote_dia    = 5
    LET reg_cza_devol_sol.cve_mod_recepcion  = "  "
    LET reg_cza_devol_sol.cod_result_operac  = NULL
    LET reg_cza_devol_sol.mot_rechazo_lote   = NULL

    LET g_cza = g_param_taa.ruta_envio CLIPPED, "/CDA "

    START REPORT listado_1 TO g_cza
        OUTPUT TO REPORT listado_1(reg_cza_devol_sol.*) #1
    FINISH REPORT listado_1

END FUNCTION

REPORT listado_1(reg_cza_devol_sol)
#1---------------------------------

    DEFINE reg_cza_devol_sol RECORD
        tipo_registro        CHAR(02) ,
        ident_servicio       CHAR(02) ,
        ident_operacion      CHAR(02) ,
        tipo_ent_origen      CHAR(02) ,
        cve_ent_origen       CHAR(03) ,
        tipo_ent_destino     CHAR(02) ,
        cve_ent_destino      CHAR(03) ,
        ent_fed_envio_lote   CHAR(03) ,
        fecha_presentacion   DATE     ,
        consec_lote_dia      SMALLINT ,
        cve_mod_recepcion    CHAR(02) ,
        cod_result_operac    CHAR(02) ,
        mot_rechazo_lote     CHAR(09) 
    END RECORD

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 
        PRINT
            COLUMN 01,reg_cza_devol_sol.tipo_registro      ,
                      reg_cza_devol_sol.ident_servicio     ,
                      reg_cza_devol_sol.ident_operacion    ,
                      reg_cza_devol_sol.tipo_ent_origen    ,
                      reg_cza_devol_sol.cve_ent_origen     ,
                      reg_cza_devol_sol.tipo_ent_destino   ,
                      reg_cza_devol_sol.cve_ent_destino    ,
                      reg_cza_devol_sol.ent_fed_envio_lote ,
                      c8_presenta                          ,
                      reg_cza_devol_sol.consec_lote_dia    USING "&&&",
                      reg_cza_devol_sol.cve_mod_recepcion  ,
                      reg_cza_devol_sol.cod_result_operac  ,
                      reg_cza_devol_sol.mot_rechazo_lote   ,
                      687 spaces 
END REPORT

FUNCTION genera_det_devol_sol()
#gdds--------------------------

    LET vsum_ult_ap_viv97 = 0 

    DECLARE cur_1 CURSOR FOR
    SELECT tipo_transferencia ,
           n_unico_infonavit  ,
           nss_infonavit      ,
           rfc_infonavit      ,
           paterno_infonavit  ,
           materno_infonavit  ,
           nombres_infonavit  ,
           ident_lote_devol   ,
           nss_afore          ,
           rfc_afore          ,
           paterno_afore      ,
           materno_afore      ,
           nombres_afore      ,
           ult_aport_viv_97   ,
           cod_result_operac  ,
           diag_proceso       ,
           nombre_imss        ,
           num_cred_infonavit ,
           motivo_devolucion  ,
           periodo_pago
    FROM   safre_tmp:det_devol_cred A
    ORDER BY 1
 
    LET reg_det_devol_sol.tipo_registro      = "02"
    LET reg_det_devol_sol.cont_servicio      = cont_reg
    LET reg_det_devol_sol.tipo_recep_cuenta  = "04"
    LET reg_det_devol_sol.cve_recep_cuenta   = "002"
    LET reg_det_devol_sol.tipo_ced_cuenta    = "01"
    LET reg_det_devol_sol.cve_ced_cuenta     = s_codigo_afore
    LET reg_det_devol_sol.fecha_presentacion = c8_presenta

    LET sw_1     = 0
    LET cont_reg = 1

    LET g_det = g_param_taa.ruta_envio CLIPPED,"/DDA "

    START REPORT listado_2 TO g_det
        FOREACH cur_1 INTO reg_det_devol_sol.tipo_transferencia ,
                           reg_det_devol_sol.n_unico_infonavit  ,
                           reg_det_devol_sol.nss_infonavit      ,
                           reg_det_devol_sol.rfc_infonavit      ,
                           reg_det_devol_sol.paterno_infonavit  ,
                           reg_det_devol_sol.materno_infonavit  ,
                           reg_det_devol_sol.nombres_infonavit  ,
                           reg_det_devol_sol.ident_lote_devol   ,
                           reg_det_devol_sol.nss_afore          ,
                           reg_det_devol_sol.rfc_afore          ,
                           reg_det_devol_sol.paterno_afore      ,
                           reg_det_devol_sol.materno_afore      ,
                           reg_det_devol_sol.nombres_afore      ,
                           reg_det_devol_sol.ult_aport_viv_97   ,
                           reg_det_devol_sol.cod_result_operac  ,
                           reg_det_devol_sol.diag_proceso       ,
                           reg_det_devol_sol.nombre_imss        ,
                           reg_det_devol_sol.num_cred_infonavit ,
                           reg_det_devol_sol.motivo_devolucion  ,
                           reg_det_devol_sol.periodo_pago

            LET vsum_ult_ap_viv97 = vsum_ult_ap_viv97 + 
                                    reg_det_devol_sol.ult_aport_viv_97
            LET sw_1 = 1

            OUTPUT TO REPORT listado_2(reg_det_devol_sol.*) #2

            DISPLAY "NUMERO REGISTROS ARCHIVO DEVOL. : ", cont_reg AT 14,15
            LET cont_reg = cont_reg + 1  
        END FOREACH

        IF sw_1 = 0 THEN
            DISPLAY "NO SE ENCONTRARON REGISTROS " AT 19,1 ATTRIBUTE(REVERSE)
            SLEEP 3
            EXIT PROGRAM
        END IF
        LET cont_reg = cont_reg - 1  
    FINISH REPORT listado_2
END FUNCTION

REPORT listado_2(reg_det_devol_sol)
#2---------------------------------

    DEFINE reg_det_devol_sol RECORD
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        tipo_transferencia    CHAR(002)     ,
        fecha_presentacion    DATE          ,
        n_unico_infonavit     CHAR(018)     ,
        nss_infonavit         CHAR(011)     ,
        rfc_infonavit         CHAR(013)     ,
        paterno_infonavit     CHAR(040)     ,
        materno_infonavit     CHAR(040)     ,
        nombres_infonavit     CHAR(040)     ,
        ident_lote_devol      CHAR(016)     ,
        nss_afore             CHAR(011)     ,
        rfc_afore             CHAR(013)     ,
        paterno_afore         CHAR(040)     ,
        materno_afore         CHAR(040)     ,
        nombres_afore         CHAR(040)     ,
        ult_aport_viv_97      DECIMAL(15,2) ,
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(015)     ,
        nombre_imss           CHAR(050)     ,
        num_cred_infonavit    DECIMAL(10,0) ,
        motivo_devolucion     SMALLINT      ,
        periodo_pago          CHAR(6)
    END RECORD

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 
        PRINT
            COLUMN 01,reg_det_devol_sol.tipo_registro       ,
                     cont_reg USING "&&&&&&&&&&"            ,
                     reg_det_devol_sol.tipo_recep_cuenta    ,
                     reg_det_devol_sol.cve_recep_cuenta     ,
                     reg_det_devol_sol.tipo_ced_cuenta      ,
                     reg_det_devol_sol.cve_ced_cuenta       ,
                     reg_det_devol_sol.tipo_transferencia   ,
                     c8_presenta                            ,
                     8 spaces                               ,
                     reg_det_devol_sol.n_unico_infonavit    ,
                     reg_det_devol_sol.nss_infonavit        ,
                     15 spaces                              ,
                     reg_det_devol_sol.rfc_infonavit        ,
                     reg_det_devol_sol.paterno_infonavit    ,
                     reg_det_devol_sol.materno_infonavit    ,
                     reg_det_devol_sol.nombres_infonavit    ,
                     22 spaces                              ,
                     reg_det_devol_sol.ident_lote_devol     ,
                     15 spaces                              ,
                     reg_det_devol_sol.nss_afore            ,
                     reg_det_devol_sol.rfc_afore            ,
                     30 spaces                              ,
                     reg_det_devol_sol.paterno_afore        ,
                     reg_det_devol_sol.materno_afore        ,
                     reg_det_devol_sol.nombres_afore        ,
                     45 spaces                              ,
                     reg_det_devol_sol.ult_aport_viv_97 USING "&&&&&&&&&&&&&&&",
                     78 spaces                              ,
                     reg_det_devol_sol.cod_result_operac    ,
                     reg_det_devol_sol.diag_proceso         ,
                     reg_det_devol_sol.nombre_imss          ,
                     reg_det_devol_sol.num_cred_infonavit USING "&&&&&&&&&&",
                     43 spaces                              ,
                     reg_det_devol_sol.motivo_devolucion  USING "&&",
                     8 spaces                               ,
                     reg_det_devol_sol.periodo_pago         ,
                     12 spaces
END REPORT

FUNCTION genera_sum_devol_sol()
#gstc--------------------------

    LET reg_sum_devol_sol.tipo_registro      = "09"
    LET reg_sum_devol_sol.cantidad_reg_det   = cont_reg
    LET reg_sum_devol_sol.sum_ult_apor_viv97 = vsum_ult_ap_viv97  

    LET g_sum = g_param_taa.ruta_envio CLIPPED,"/SDA "

    START REPORT listado_3 TO g_sum
        OUTPUT TO REPORT listado_3(reg_sum_devol_sol.*) #3
    FINISH REPORT listado_3

END FUNCTION

REPORT listado_3(reg_sum_devol_sol)
#3---------------------------------

    DEFINE reg_sum_devol_sol RECORD
         tipo_registro        CHAR(02)      ,
         cantidad_reg_det     DECIMAL(9,0)  ,
         sum_ult_apor_viv97   DECIMAL(15,2)

    END RECORD

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 
        PRINT
            COLUMN 01,reg_sum_devol_sol.tipo_registro      ,
                   reg_sum_devol_sol.cantidad_reg_det   USING "&&&&&&&&&",
                   45 spaces                            ,
                   reg_sum_devol_sol.sum_ult_apor_viv97 USING "&&&&&&&&&&&&&&&",
                   659 spaces
END REPORT

FUNCTION impresion_reporte()
#ir-------------------------

  DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore

  DEFINE G_IMPRE    CHAR(300)
  DEFINE gimpresion CHAR(300)
  DEFINE hora       CHAR(8)

  SELECT  codigo_afore
  INTO    w_codigo_afore
  FROM    tab_afore_local

  LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
              ".DEV_CRED.",HOY USING "DD-MM-YYYY"

  START REPORT _sol_imp TO  G_IMPRE

  OUTPUT TO REPORT _sol_imp(g.*)

  FINISH REPORT _sol_imp

  LET gimpresion = "lp ",G_IMPRE
  --LET gimpresion = "vi ",G_IMPRE
  RUN gimpresion

END FUNCTION

REPORT _sol_imp(g)
#dvsi-------------

   DEFINE g RECORD
      total              INTEGER,
      motivo_devolucion   CHAR(2),
      descripcion         CHAR(30)
   END RECORD

   DEFINE i RECORD LIKE tab_afore_local.*
  
  OUTPUT
   TOP MARGIN 1
   BOTTOM MARGIN 0
   LEFT MARGIN 0
   RIGHT MARGIN 0
   PAGE LENGTH 60
  FORMAT
   PAGE HEADER
     SELECT razon_social
     INTO i.razon_social
     FROM tab_afore_local

     PRINT COLUMN 01,i.razon_social,
           COLUMN 68,TODAY USING "dd-mm-yyyy"
     SKIP 2 LINE
     PRINT COLUMN 08,"TOTAL DE DEVOLUCIONES DE AFORE."
     PRINT COLUMN 08,"CREDITOS EN GARANTIA 43 BIS. "
     PRINT COLUMN 04,"----------------------------------------------------------------------"
     PRINT COLUMN 04,"TOTAL DE REGISTROS",
           COLUMN 25,"MOTIVO DE DEVOLUCION",
           COLUMN 50,"DESCRIPCION"
     PRINT COLUMN 04,"----------------------------------------------------------------------"
   ON EVERY ROW
    DECLARE cur_2 CURSOR FOR

    SELECT COUNT(*), dd.motivo_devolucion
    FROM safre_tmp:det_devol_cred dd
    GROUP BY 2
    ORDER BY 2

    FOREACH cur_2 INTO g.total, g.motivo_devolucion
     CASE g.motivo_devolucion 
        WHEN 8
           LET g.descripcion = "SALDO CERO"
        WHEN 4
           LET g.descripcion = "PROCESO DE RETIRO"
        WHEN 11
           LET g.descripcion = "PROCESO DE TRASPASO"
        WHEN 13
           LET g.descripcion = "SALDO PREVIAMENTE TRANSFERIDO"
        WHEN 14
           LET g.descripcion = "MONTOS NO COINCIDEN"
        WHEN 19
           LET g.descripcion = "PROCESO SEPARACION CUENTAS"
        WHEN 20
           LET g.descripcion = "PROCESO DEV PAGOS EXCESO"
      END CASE 

      PRINT COLUMN 08,g.total,
            COLUMN 35,g.motivo_devolucion,
            COLUMN 50,g.descripcion
     END FOREACH 

     ON LAST ROW
       SKIP 4 LINES
      PRINT COLUMN 2, "Total de registros encontrados: ", g.total USING "<<<<" 

     PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar...."
END REPORT

FUNCTION periodo()
#p----------------

    DEFINE 
        s_anyo    SMALLINT,
        c_anyo    CHAR(4),
        mes       CHAR(2),
        c_periodo CHAR(6)

    LET s_anyo = YEAR(fecha_envio)

    CASE MONTH(fecha_envio)
        WHEN 1
            LET mes = "06"
            LET s_anyo = s_anyo - 1
        WHEN 2
            LET mes = "06"
            LET s_anyo = s_anyo - 1
        WHEN 3
            LET mes = "01"
        WHEN 4
            LET mes = "01"
        WHEN 5
            LET mes = "02"
        WHEN 6 
            LET mes = "02"
        WHEN 7
            LET mes = "03"
        WHEN 8
            LET mes = "03"
        WHEN 9
            LET mes = "04"
        WHEN 10
            LET mes = "04"
        WHEN 11
            LET mes = "05"
        WHEN 12
            LET mes = "05"
    END CASE

    LET c_anyo = s_anyo
    LET c_periodo = c_anyo,mes

    RETURN c_periodo

END FUNCTION

