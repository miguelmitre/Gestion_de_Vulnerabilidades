############################################################################
#Propietario       => E.F.P.                                               #
#Programa ACRB028  => GENERA ARCHIVO MODIFICACION SALDO CREDITO GARANTIA   #
#Fecha creacion    => 1 DE JULIO DE 2002                                   #
#Por               => MAURO MUNIZ CABALLERO                                #
#Sistema           => ACR (TRA)                                            #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_cza_tra_cred RECORD
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        cve_ent_destino       CHAR(03) ,
        ent_fed_envio_lote    CHAR(03) ,
        fecha_presentacion    DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recep         CHAR(02) ,
        cod_result_operac     CHAR(02) ,
        rechazo               CHAR(09)
    END RECORD

    DEFINE reg_det_tra_cred RECORD
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
        tipo_modifica         CHAR(001)     ,
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
        sdo_viv_97            DECIMAL(15,02),
        sdo_viv_92            DECIMAL(15,02),
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(015)     ,
        nombre_imss           CHAR(050)     ,
        num_cred_infonavit    DECIMAL(10,0) ,
        int_viv_97            DECIMAL(15,02),
        int_viv_92            DECIMAL(15,02),
        periodo_pago          CHAR(6)
    END RECORD

    DEFINE reg_sum_tra_cred RECORD
        tipo_registro        CHAR(02)      ,
        cant_reg_det         DECIMAL(9,0)  ,
        sum_sdo_viv97        DECIMAL(15,2) ,
        sum_sdo_viv92        DECIMAL(15,2) ,
        sum_int_viv97        DECIMAL(15,2) ,
        sum_int_viv92        DECIMAL(15,2)
    END RECORD

    DEFINE g_param_taa RECORD LIKE seg_modulo.*

    DEFINE
        HOY                   ,
        d_fecha_cza           ,
        d_fecha_det           DATE

    DEFINE
        enter                 CHAR(1),
        lote_rechazado        CHAR(2),
        c_paso_cza            CHAR(8),
        c_paso_det            CHAR(8),
        g_usuario             CHAR(8),
        c_fecha_cza           CHAR(10),
        c_fecha_det           CHAR(10),
        g_det                 CHAR(100),
        g_cza                 CHAR(100),
        g_sum                 CHAR(100),
        archivo_traspaso      CHAR(200),
        cat                   CHAR(300),
        ejecuta               CHAR(300)

    DEFINE
        generar               DATE,
        xcodigo_marca         ,
        xcodigo_rechazo       ,
        bnd_proc              ,
        edo_proc              SMALLINT,
        cuantos               ,
        s_codigo_afore        ,
        cont                  INTEGER

    DEFINE g RECORD
        total                 INTEGER,
        tipo_transferencia    CHAR(2),
        descripcion           CHAR(20),
        marca                 SMALLINT,
        marca_desc            CHAR(20)
    END RECORD

    DEFINE HORA CHAR (08)
    DEFINE saldo_viv_97 DECIMAL(18,6)
    DEFINE saldo_viv_92 DECIMAL(18,6)

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS 
        INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("ACRB028.log")
    CALL inicio() #i
    CALL proceso_principal() #pp

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "ACRB0281" ATTRIBUTE(BORDER)

    DISPLAY " ACRB028  GENERA ARCHIVO MODIFICACION DE SALDO CRED GARANTIA              " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                           < Ctrl-C > Salir                               " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET generar = HOY

    INPUT BY NAME generar WITHOUT DEFAULTS
        AFTER FIELD generar
        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        ELSE
            EXIT INPUT
        END IF

        ON KEY (INTERRUPT)
            DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE)
            SLEEP 2
            EXIT PROGRAM
    END INPUT

    WHILE TRUE
        PROMPT "ESTA SEGURO S/N ? " FOR enter

        IF enter MATCHES "[sS]" THEN
            EXIT WHILE
        ELSE
           DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
           EXIT PROGRAM
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION" AT 19,1

    CALL validacion_previa() #vp
    CALL genera_detalle()    #lap
    CALL genera_encabezado() #lap
    CALL genera_sumario()    #lap
    ---CALL impresion_reporte() 

    LET cat = "cat ",g_param_taa.ruta_envio CLIPPED,"/CMS ",
                     g_param_taa.ruta_envio CLIPPED,"/DMS ",
                     g_param_taa.ruta_envio CLIPPED,"/SMS > ",
                     g_param_taa.ruta_envio CLIPPED,"/",
                     "MOD_SALDO.",HOY USING "MMDD","-",
                     HORA[1,2],HORA[4,5]

    RUN cat 

    PROMPT "Proceso finalizado, presione [Enter] p/salir" FOR enter

    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION inicio()
#i---------------

    LET HOY  = TODAY
    LET HORA = TIME

    SELECT codigo_afore, USER
    INTO   s_codigo_afore, g_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_param_taa.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr'

    CREATE TEMP TABLE tmp_monto
        (nss       CHAR(11),
         monto_v97 DECIMAL(18,6),
         monto_v92 DECIMAL(18,6))
END FUNCTION

FUNCTION validacion_previa()
#vp------------------------

    DEFINE
        c2_tipo_registro  CHAR(2)

    DEFINE
        tot_mod   INTEGER

    DEFINE
        sw_1  ,
        sw_2  ,
        sw_9  SMALLINT

    SELECT COUNT(*)
    INTO   tot_mod
    FROM   acr_det_modifica m
    WHERE  m.estado = 0

    IF tot_mod IS NULL OR
       tot_mod <= 0 THEN
        PROMPT "NO HAY REGISTROS A ENVIAR, [Enter] p/salir " FOR enter
        EXIT PROGRAM
    END IF

END FUNCTION

FUNCTION genera_detalle()
#lap---------------------

    DEFINE reg_mod RECORD LIKE acr_det_modifica.*

    DEFINE
        mot_dev SMALLINT
 
    DEFINE
        sdo_v97 DECIMAL(18,6),
        sdo_v92 DECIMAL(18,6)
 
    DEFINE 
        carga_reg            CHAR(730),
        c2_ident_operacion   CHAR(2)

    DEFINE 
        fecha_conver DATE

    LET edo_proc = 0
    LET bnd_proc = 0

    DECLARE cur_1 CURSOR FOR
    SELECT e.tipo_registro     ,
           e.cont_servicio     ,
           e.tipo_recep_cuenta ,
           e.cve_recep_cuenta  ,
           e.tipo_ced_cuenta   ,
           e.cve_ced_cuenta    ,
           e.tipo_transferencia,
           e.fecha_presentacion,
           e.n_unico_infonavit ,
           e.nss_infonavit     ,
           d.tipo_modifica     ,
           e.rfc_infonavit     ,
           e.paterno_infonavit ,
           e.materno_infonavit ,
           e.nombres_infonavit ,
           e.ident_lote_devol  ,
           e.nss_afore         ,
           e.rfc_afore         ,
           e.paterno_afore     ,
           e.materno_afore     ,
           e.nombres_afore     ,
           e.sdo_viv_97        ,
           e.sdo_viv_92        ,
           e.cod_result_operac ,
           e.diag_proceso      ,
           e.nombre_imss       ,
           e.num_cred_infonavit,
           e.int_viv_97        ,
           e.int_viv_92        ,
           e.periodo_pago      ,
           d.viv_97            ,
           d.viv_92 
    FROM   acr_det_tra_cred e, acr_det_modifica d
    WHERE  e.nss_afore = d.nss_afore
    AND    e.estado    = 1
    AND    d.estado    = 0

    LET cont = 0

    LET g_det = g_param_taa.ruta_envio CLIPPED,"/DMS" 

    START REPORT listado_2 TO g_det

    FOREACH cur_1 INTO reg_det_tra_cred.tipo_registro       ,
                       reg_det_tra_cred.cont_servicio       ,
                       reg_det_tra_cred.tipo_recep_cuenta   ,
                       reg_det_tra_cred.cve_recep_cuenta    ,
                       reg_det_tra_cred.tipo_ced_cuenta     ,
                       reg_det_tra_cred.cve_ced_cuenta      ,
                       reg_det_tra_cred.tipo_transferencia  ,
                       reg_det_tra_cred.fecha_presentacion  ,
                       reg_det_tra_cred.n_unico_infonavit   ,
                       reg_det_tra_cred.nss_infonavit       ,
                       reg_det_tra_cred.tipo_modifica       ,
                       reg_det_tra_cred.rfc_infonavit       ,
                       reg_det_tra_cred.paterno_infonavit   ,
                       reg_det_tra_cred.materno_infonavit   ,
                       reg_det_tra_cred.nombres_infonavit   ,
                       reg_det_tra_cred.ident_lote_devol    ,
                       reg_det_tra_cred.nss_afore           ,
                       reg_det_tra_cred.rfc_afore           ,
                       reg_det_tra_cred.paterno_afore       ,
                       reg_det_tra_cred.materno_afore       ,
                       reg_det_tra_cred.nombres_afore       ,
                       saldo_viv_97                         ,
                       saldo_viv_92                         ,
                       reg_det_tra_cred.cod_result_operac   ,
                       reg_det_tra_cred.diag_proceso        ,
                       reg_det_tra_cred.nombre_imss         ,
                       reg_det_tra_cred.num_cred_infonavit  ,
                       reg_det_tra_cred.int_viv_97          ,
                       reg_det_tra_cred.int_viv_92          ,
                       reg_det_tra_cred.periodo_pago        ,
                       reg_det_tra_cred.sdo_viv_97          ,
                       reg_det_tra_cred.sdo_viv_92  

      {
        LET reg_det_tra_cred.sdo_viv_97 = 
            reg_det_tra_cred.sdo_viv_97 + saldo_viv_97

        LET reg_det_tra_cred.sdo_viv_92 =
            reg_det_tra_cred.sdo_viv_92 + saldo_viv_92
      }

        LET reg_det_tra_cred.fecha_presentacion = generar

        LET cont = cont + 1

        INSERT INTO tmp_monto 
        VALUES(reg_det_tra_cred.nss_afore,
               reg_det_tra_cred.sdo_viv_97,
               reg_det_tra_cred.sdo_viv_92)

        OUTPUT TO REPORT listado_2(reg_det_tra_cred.*) #2

        DISPLAY "Registros procesados : ", cont AT 14,11

        UPDATE  acr_det_modifica
        SET     estado = 1,
                fecha_presentacion = generar
        WHERE   nss_afore = reg_det_tra_cred.nss_afore
        AND     estado = 0

    END FOREACH

    SLEEP 3

    FINISH REPORT listado_2

END FUNCTION

REPORT listado_2(reg_det_tra_cred)
#rl2------------------------------

    DEFINE reg_det_tra_cred RECORD
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
        tipo_modifica         CHAR(001)     ,
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
        sdo_viv_97            DECIMAL(15,02),
        sdo_viv_92            DECIMAL(15,02),
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(015)     ,
        nombre_imss           CHAR(050)     ,
        num_cred_infonavit    DECIMAL(10,0) ,
        int_viv_97            DECIMAL(15,02),
        int_viv_92            DECIMAL(15,02),
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
            COLUMN 01,reg_det_tra_cred.tipo_registro        ,
                      cont                                  USING "&&&&&&&&&&",
                      reg_det_tra_cred.tipo_recep_cuenta    ,
                      reg_det_tra_cred.cve_recep_cuenta     ,
                      reg_det_tra_cred.tipo_ced_cuenta      ,
                      reg_det_tra_cred.cve_ced_cuenta       USING "&&&",
                      2 SPACES                              ,
                      reg_det_tra_cred.fecha_presentacion   USING "YYYYMMDD",
                      8 SPACES                              ,
                      reg_det_tra_cred.n_unico_infonavit    ,
                      reg_det_tra_cred.nss_infonavit        ,
                      14 spaces                             ,
                      reg_det_tra_cred.tipo_modifica        ,
                      reg_det_tra_cred.rfc_infonavit        ,
                      reg_det_tra_cred.paterno_infonavit    ,
                      reg_det_tra_cred.materno_infonavit    ,
                      reg_det_tra_cred.nombres_infonavit    ,
                      22 spaces                             ,
                      reg_det_tra_cred.ident_lote_devol     ,
                      15 spaces                             , 
                      reg_det_tra_cred.nss_afore            ,
                      reg_det_tra_cred.rfc_afore            ,
                      30 spaces                             ,
                      reg_det_tra_cred.paterno_afore        ,
                      reg_det_tra_cred.materno_afore        ,
                      reg_det_tra_cred.nombres_afore         ,
                      45 spaces                             ,
                      reg_det_tra_cred.sdo_viv_97 * 100 USING "&&&&&&&&&&&&&&&",
                      60 spaces                             ,
                      reg_det_tra_cred.sdo_viv_92 * 100 USING "&&&&&&&&&&&&&&&",
                      3 spaces                              ,
                      reg_det_tra_cred.cod_result_operac    ,
                      reg_det_tra_cred.diag_proceso         ,
                      reg_det_tra_cred.nombre_imss          ,
                      reg_det_tra_cred.num_cred_infonavit USING "&&&&&&&&&&",
                      reg_det_tra_cred.int_viv_97      USING "&&&&&&&&&&&&&&&",
                      reg_det_tra_cred.int_viv_92      USING "&&&&&&&&&&&&&&&",
                      23 spaces                             ,
                      reg_det_tra_cred.periodo_pago         ,
                      12 spaces

END REPORT

FUNCTION genera_encabezado()
#gctc-----------------------

    LET reg_cza_tra_cred.tipo_registro      = "01"
    LET reg_cza_tra_cred.ident_servicio     = "02"
    LET reg_cza_tra_cred.ident_operacion    = "31"
    LET reg_cza_tra_cred.tipo_ent_origen    = "01"
    LET reg_cza_tra_cred.cve_ent_origen     = s_codigo_afore
    LET reg_cza_tra_cred.tipo_ent_destino   = "04"
    LET reg_cza_tra_cred.cve_ent_destino    = '002'
    LET reg_cza_tra_cred.ent_fed_envio_lote = "009"
    LET reg_cza_tra_cred.fecha_presentacion = generar
    LET reg_cza_tra_cred.consec_lote_dia    = 1
    LET reg_cza_tra_cred.cve_mod_recep      = "02"
    LET reg_cza_tra_cred.cod_result_operac  = NULL
    LET reg_cza_tra_cred.rechazo            = NULL

    LET g_cza = g_param_taa.ruta_envio CLIPPED,"/CMS"

    START REPORT listado_1 TO g_cza
        OUTPUT TO REPORT listado_1(reg_cza_tra_cred.*) #1
    FINISH REPORT listado_1 

END FUNCTION

REPORT listado_1(reg_cza_tra_cred)
#1--------------------------------

    DEFINE reg_cza_tra_cred RECORD
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        cve_ent_destino       CHAR(03) ,
        ent_fed_envio_lote    CHAR(03) ,
        fecha_presentacion    DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recep         CHAR(02) ,
        cod_result_operac     CHAR(02) ,
        rechazo               CHAR(09)
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
            COLUMN 01,reg_cza_tra_cred.tipo_registro      ,
                      reg_cza_tra_cred.ident_servicio     ,
                      reg_cza_tra_cred.ident_operacion    ,
                      reg_cza_tra_cred.tipo_ent_origen    ,
                      reg_cza_tra_cred.cve_ent_origen     ,
                      reg_cza_tra_cred.tipo_ent_destino   ,
                      reg_cza_tra_cred.cve_ent_destino    ,
                      reg_cza_tra_cred.ent_fed_envio_lote ,
                      reg_cza_tra_cred.fecha_presentacion USING "YYYYMMDD",
                      reg_cza_tra_cred.consec_lote_dia    USING "&&&",
                      2 SPACES                            ,
                      reg_cza_tra_cred.cod_result_operac  ,
                      reg_cza_tra_cred.rechazo            ,
                      687 spaces

END REPORT

FUNCTION genera_sumario()
#gstc--------------------

    LET reg_sum_tra_cred.tipo_registro = "09"
    LET reg_sum_tra_cred.cant_reg_det  = cont
    LET reg_sum_tra_cred.sum_int_viv97 = 0
    LET reg_sum_tra_cred.sum_int_viv92 = 0

    SELECT SUM(monto_v97)
    INTO   reg_sum_tra_cred.sum_sdo_viv97
    FROM   tmp_monto

    SELECT SUM(monto_v92)
    INTO   reg_sum_tra_cred.sum_sdo_viv92
    FROM   tmp_monto

    LET g_sum = g_param_taa.ruta_envio CLIPPED,"/SMS"

    START REPORT listado_3 TO g_sum
        OUTPUT TO REPORT listado_3(reg_sum_tra_cred.*) #3
    FINISH REPORT listado_3

END FUNCTION

REPORT listado_3(reg_sum_tra_cred)
#3---------------------------------

    DEFINE reg_sum_tra_cred RECORD
        tipo_registro        CHAR(02)      ,
        cant_reg_det         DECIMAL(9,0)  ,
        sum_sdo_viv97        DECIMAL(15,2) ,
        sum_sdo_viv92        DECIMAL(15,2) ,
        sum_int_viv97        DECIMAL(15,2) ,
        sum_int_viv92        DECIMAL(15,2)
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
            COLUMN 01,reg_sum_tra_cred.tipo_registro      ,
                      reg_sum_tra_cred.cant_reg_det   USING"&&&&&&&&&",
                      45 spaces                           ,
                  reg_sum_tra_cred.sum_sdo_viv97 * 100 USING"&&&&&&&&&&&&&&&",
                      60 spaces                           ,
                  reg_sum_tra_cred.sum_sdo_viv92 * 100 USING"&&&&&&&&&&&&&&&",
                      reg_sum_tra_cred.sum_int_viv97   USING"&&&&&&&&&&&&&&&",
                      reg_sum_tra_cred.sum_int_viv92   USING"&&&&&&&&&&&&&&&",
                      554 spaces

END REPORT

FUNCTION impresion_reporte()
#ir-------------------------

    DEFINE G_IMPRE        CHAR(300)
    DEFINE c_impre        CHAR(300)
    DEFINE gimpresion     CHAR(300)

    LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".CRED_GARANTIA.",HOY USING "DD-MM-YYYY",
                  "_",HORA CLIPPED

    START REPORT det_tra_cred_imp TO  G_IMPRE

    OUTPUT TO REPORT det_tra_cred_imp(g.*)

    FINISH REPORT det_tra_cred_imp

    LET gimpresion = "lp ",G_IMPRE
    RUN gimpresion

    LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
    RUN c_impre

END FUNCTION

REPORT det_tra_cred_imp(g)
#dtai---------------------

    DEFINE g RECORD
        nss                 CHAR(11),
        tipo_transferencia  CHAR(2),
        descripcion         CHAR(20),
        marca               SMALLINT,
        marca_desc          CHAR(20)
    END RECORD

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0
        PAGE LENGTH 70

    FORMAT
        PAGE HEADER

        PRINT COLUMN 01,s_codigo_afore,
            COLUMN 68,TODAY USING "dd-mm-yyyy"
        SKIP 2 LINE
        PRINT COLUMN 08,"   REGISTROS SOLICITADOS PARA MARCA DE CREDITO EN GARANTIA   "
        SKIP 1 LINE 
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        PRINT COLUMN 02,"NSS",
              COLUMN 15,"TIPO DE TRANSFERENCIA",
              COLUMN 43,"MARCA"
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        SKIP 1 LINE 
    ON EVERY ROW

      DECLARE cursor CURSOR FOR
       
      SELECT a.nss_afore,
             a.tipo_transferencia,
             b.marca_cod
      FROM   acr_det_tra_cred a, cta_ctr_cuenta b
      WHERE  a.nss_afore = b.nss
      GROUP BY 2
      ORDER BY 2

      FOREACH cursor INTO g.nss,
                          g.tipo_transferencia,
                          g.marca

          WHENEVER ERROR CONTINUE

          SELECT c.marca_desc
          INTO   g.marca_desc
          FROM   tab_marca c
          WHERE  c.marca_cod = g.marca

          WHENEVER ERROR STOP

          CASE g.tipo_transferencia
              WHEN "03" LET g.descripcion = "TRANSFERENCIA TOTAL"
              WHEN "04" LET g.descripcion = "ULTIMA APORTACION"
              WHEN "16" LET g.descripcion = "CREDITO EN GARANTIA"
              WHEN "18" LET g.descripcion = "USO DE CREDITO"
              WHEN "70" LET g.descripcion = "TRANSFERENCIA TOTAL"
              WHEN "71" LET g.descripcion = "ULTIMA APORTACION"
          END CASE
       
        PRINT COLUMN 08,g.nss,
              COLUMN 15,g.tipo_transferencia,
              COLUMN 18,g.descripcion,
              COLUMN 45,g.marca,
              COLUMN 50,g.marca_desc
      END FOREACH

     --ON LAST ROW
        --SKIP 4 LINES

     PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar."

END REPORT

