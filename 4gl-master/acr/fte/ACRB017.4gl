#############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                           #
#Propietario       => E.F.P.                                                #
#Programa ACRB017  => GENERA TRANSFERENCIA DE SALDOS CREDITO EN GARANTIA    #
#Por               => MAURO MUNIZ CABALLERO                                 #
#Fecha creacion    => 1 DE JULIO DE 2002                                    #
#Fecha Actualiza   => 4 DE AGOSTO DE 2004                                   #
#Actualiza         => MAURO MUNIZ CABALLERO                                 #
#                 Se adecuo para el proceso de participaciones              #
#Sistema           => ACR                                                   #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_cza_tra_ctas RECORD 
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
        cod_result_operac     CHAR(02) ,
        rechazo               CHAR(09)  
    END RECORD

    DEFINE reg_det_tra_ctas RECORD
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
        partic_v97            DECIMAL(22,06),
        sdo_viv_97            DECIMAL(15,02),
        partic_v92            DECIMAL(22,06),
        sdo_viv_92            DECIMAL(15,02),
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(015)     ,
        nombre_imss           CHAR(050)     ,
        num_cred_infonavit    DECIMAL(10,0) ,
        int_viv_97            DECIMAL(15,02),
        int_viv_92            DECIMAL(15,02),
        periodo_pago          CHAR(6)
    END RECORD

    DEFINE reg_sum_tra_ctas RECORD
        tipo_registro        CHAR(02)      ,
        cant_reg_det         DECIMAL(9,0)  ,
        sum_partic_v97       DECIMAL(22,6) ,
        sum_sdo_viv97        DECIMAL(15,2)
    END RECORD

    DEFINE g_param_taa RECORD LIKE seg_modulo.*

    DEFINE
        HOY                  ,
        fecha_envio          ,
        fecha_envio1         ,
        fecha_envio2         ,
        fecha_envio3         ,
        fecha_envio_banx     ,
        fecha_presentacion   DATE

    DEFINE
        enter     CHAR(1)   ,
        HORA      CHAR(5)   ,
        c5_HORA   CHAR(5)   ,
        periodo_1 CHAR(6)   ,
        c8_HOY    CHAR(8)   ,
        vusuario  CHAR(8)   ,
        nom_arch  CHAR(50)  ,
        comma     CHAR(50)  ,
        g_cza     CHAR(100) ,
        g_det     CHAR(100) ,
        g_sum     CHAR(100) ,
        cat       CHAR(300) 

    DEFINE
        existe                ,
        s_periodo             ,
        sw_1                  ,
        s_lotes_num           ,
        s_lotes_correlativo   ,
        s_codigo_afore        SMALLINT,
        vfolio                INTEGER

    DEFINE
        cont_reg          DECIMAL(10,0),
        vsum_sdo_viv97    DECIMAL(15,2),
        vsum_sdo_viv92    DECIMAL(15,2)

    DEFINE
        ult_aport_hist    ,
        ult_aport_v97     ,
        sdo_viv_v97       ,
        sdo_tot_v97       DECIMAL(15,6),
        saldo_total97     ,
        saldo_pesos97     DECIMAL(15,2)

    DEFINE g_reg RECORD
        nss     CHAR(11),
        par97   DECIMAL(22,2),
        viv97   DECIMAL(15,2)
    END RECORD

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG ("ACRB017.log")
    CALL inicio() #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i-------------

    LET HOY     = TODAY
    LET HORA    = TIME
    LET c5_HORA = HORA

    LET s_lotes_num = 4

    SELECT codigo_afore
    INTO   s_codigo_afore
    FROM   tab_afore_local

    SELECT *, user
    INTO   g_param_taa.*, vusuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr'

    LET cont_reg = 1
    LET reg_det_tra_ctas.sdo_viv_97 = 0
    LET reg_det_tra_ctas.sdo_viv_92 = 0
    LET reg_sum_tra_ctas.sum_sdo_viv97 = 0
    LET ult_aport_v97  = 0
    LET ult_aport_hist = 0
    LET sdo_viv_v97    = 0
    LET sdo_tot_v97    = 0
    LET saldo_total97  = 0

    INITIALIZE g_reg.* TO NULL

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW acrb0031 AT 4,4 WITH FORM "ACRB0021" ATTRIBUTE(BORDER)
    DISPLAY " ACRB017      GENERA ARCHIVO SALDOS CREDITOS EN GARANTIA                       " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY " < CTRL-C > Salir " AT 1,26              
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,60 ATTRIBUTE(REVERSE)

    LET fecha_envio = HOY
    LET c8_HOY  = HOY USING "YYYYMMDD"

    INPUT BY NAME fecha_envio WITHOUT DEFAULTS
        AFTER FIELD fecha_envio
            IF fecha_envio IS NULL THEN
                ERROR "LA FECHA DE ENVIO NO PUEDE SER NULA"
                NEXT FIELD fecha_envio
            END IF
        EXIT INPUT

        ON KEY ( INTERRUPT )
            EXIT PROGRAM
    END INPUT

    WHILE TRUE
        PROMPT "ESTA SEGURO S/N ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    LET fecha_envio2 = MDY(MONTH(fecha_envio),1,YEAR(fecha_envio))
    LET fecha_envio3 = fecha_envio2 + 1 UNITS MONTH 

    CALL habil_siguiente(fecha_envio2) RETURNING fecha_envio1

    --LET comma = 'fglgo ACRB039 ',fecha_envio3
    --RUN comma

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL crea_tablas()            #ct
    CALL genera_cza_tra_ctas()    #gctc
    CALL genera_det_tra_ctas()    #gdtc
    CALL genera_sum_tra_ctas()    #gstc

    LET cat = "cat ",g_param_taa.ruta_envio CLIPPED,"/CCG ",
                     g_param_taa.ruta_envio CLIPPED,"/DCG ",
                     g_param_taa.ruta_envio CLIPPED,"/SCG > ",
                     g_param_taa.ruta_envio CLIPPED,"/",
                     "CRED_GARANTIA.",hoy USING "MMDD","-",
                     hora[1,2],hora[4,5]

    RUN cat

    LET nom_arch = g_param_taa.ruta_envio CLIPPED,"/",
                   "CRED_GARANTIA.",hoy USING "MMDD","-",
                   hora[1,2],hora[4,5]

    LET nom_arch = nom_arch CLIPPED

    DISPLAY "Archivo : ",nom_arch AT 19,1 ATTRIBUTE(REVERSE)

    PROMPT "PROCESO FINALIZADO, [Enter] p/salir " FOR enter
    CLOSE WINDOW acrb0031

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE sdo_credito
    WHENEVER ERROR STOP

    CREATE TABLE sdo_credito
       (nss    CHAR(11),
        par97  DECIMAL(22,2),
        viv97  DECIMAL(15,2))

        DATABASE safre_af

END FUNCTION

FUNCTION genera_cza_tra_ctas()
#gctc-------------------------

    LET reg_cza_tra_ctas.tipo_registro      = "01"
    LET reg_cza_tra_ctas.ident_servicio     = "02"
    LET reg_cza_tra_ctas.ident_operacion    = "09"
    LET reg_cza_tra_ctas.tipo_ent_origen    = "01"
    LET reg_cza_tra_ctas.cve_ent_origen     = s_codigo_afore
    LET reg_cza_tra_ctas.tipo_ent_destino   = "04"
    LET reg_cza_tra_ctas.cve_ent_destino    = '002'
    LET reg_cza_tra_ctas.ent_fed_envio_lote = "009"
    LET reg_cza_tra_ctas.fecha_presentacion = fecha_envio 
    LET reg_cza_tra_ctas.consec_lote_dia    = 1
    LET reg_cza_tra_ctas.cod_result_operac  = NULL
    LET reg_cza_tra_ctas.rechazo            = NULL
  
    LET g_cza = g_param_taa.ruta_envio CLIPPED,"/CCG"

    START REPORT listado_1 TO g_cza
        OUTPUT TO REPORT listado_1(reg_cza_tra_ctas.*) #1
    FINISH REPORT listado_1

END FUNCTION

REPORT listado_1(reg_cza_tra_ctas)
#1--------------------------------

    DEFINE reg_cza_tra_ctas RECORD
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
            COLUMN 01,reg_cza_tra_ctas.tipo_registro      ,
                      reg_cza_tra_ctas.ident_servicio     ,
                      reg_cza_tra_ctas.ident_operacion    ,
                      reg_cza_tra_ctas.tipo_ent_origen    ,
                      reg_cza_tra_ctas.cve_ent_origen     ,
                      reg_cza_tra_ctas.tipo_ent_destino   ,
                      reg_cza_tra_ctas.cve_ent_destino    ,
                      reg_cza_tra_ctas.ent_fed_envio_lote ,
                      reg_cza_tra_ctas.fecha_presentacion USING "YYYYMMDD",
                      reg_cza_tra_ctas.consec_lote_dia    USING "&&&",
                      2 spaces                            ,
                      reg_cza_tra_ctas.cod_result_operac  ,
                      reg_cza_tra_ctas.rechazo            ,
                      687 spaces

END REPORT

FUNCTION genera_det_tra_ctas()
#gdtc-------------------------

    DEFINE
        dt_fc_vl             ,
        fecha_envio_archivo  DATE

    DEFINE
        c8_banxico     CHAR(08),
        fe_cha         DATE

    DEFINE
        vprecio_accion DECIMAL(19,14)

    INITIALIZE fe_cha TO NULL

    SELECT @precio_del_dia
      INTO vprecio_accion
      FROM glo_valor_accion
     WHERE @fecha_valuacion = fecha_envio2
       AND @codigo_siefore  = 11

    SELECT UNIQUE MAX(a.fecha_presentacion)
    INTO   fe_cha
    FROM   acr_det_tra_cred a

    DECLARE cur_1 CURSOR FOR
    SELECT  A.tipo_transferencia   ,
            A.n_unico_infonavit    ,
            A.nss_infonavit        ,
            A.rfc_infonavit        ,
            A.paterno_infonavit    ,
            A.materno_infonavit    ,
            A.nombres_infonavit    ,
            A.ident_lote_devol     ,
            A.nss_afore            ,
            A.rfc_afore            ,
            A.paterno_afore        ,
            A.materno_afore        ,
            A.nombres_afore        ,
            A.cod_result_operac    ,
            A.diag_proceso         ,
            A.nombre_imss          ,
            A.num_cred_infonavit   ,
            A.periodo_pago  
    FROM    acr_det_tra_cred A
    WHERE   A.estado = 0
    AND     A.fecha_presentacion = fe_cha

    LET reg_det_tra_ctas.tipo_registro      = "02"
    LET reg_det_tra_ctas.cont_servicio      = cont_reg
    LET reg_det_tra_ctas.tipo_recep_cuenta  = "04"
    LET reg_det_tra_ctas.cve_recep_cuenta   = "002"
    LET reg_det_tra_ctas.tipo_ced_cuenta    = "01"
    LET reg_det_tra_ctas.cve_ced_cuenta     = s_codigo_afore
    LET reg_det_tra_ctas.fecha_presentacion = fecha_envio
    LET reg_det_tra_ctas.int_viv_97         = 0 
    LET reg_det_tra_ctas.int_viv_92         = 0 

    LET vsum_sdo_viv97 = 0
    LET vsum_sdo_viv92 = 0

    LET g_det = g_param_taa.ruta_envio CLIPPED,"/DCG" 

    START REPORT listado_2 TO g_det
        LET sw_1 = 0
        FOREACH cur_1 INTO reg_det_tra_ctas.tipo_transferencia ,
                           reg_det_tra_ctas.n_unico_infonavit  ,
                           reg_det_tra_ctas.nss_infonavit      ,
                           reg_det_tra_ctas.rfc_infonavit      ,
                           reg_det_tra_ctas.paterno_infonavit  ,
                           reg_det_tra_ctas.materno_infonavit  ,
                           reg_det_tra_ctas.nombres_infonavit  ,
                           reg_det_tra_ctas.ident_lote_devol   ,
                           reg_det_tra_ctas.nss_afore          ,
                           reg_det_tra_ctas.rfc_afore          ,
                           reg_det_tra_ctas.paterno_afore      ,
                           reg_det_tra_ctas.materno_afore      ,
                           reg_det_tra_ctas.nombres_afore      ,
                           reg_det_tra_ctas.cod_result_operac  ,
                           reg_det_tra_ctas.diag_proceso       ,
                           reg_det_tra_ctas.nombre_imss        ,
                           reg_det_tra_ctas.num_cred_infonavit ,
                           reg_det_tra_ctas.periodo_pago

        LET g_reg.nss   = reg_det_tra_ctas.nss_afore
        LET g_reg.viv97 = 0
        LET g_reg.par97 = 0

        SELECT NVL(SUM(monto_en_acciones),0)
        INTO   sdo_tot_v97
        FROM   dis_cuenta
        WHERE  nss = reg_det_tra_ctas.nss_afore
        AND    subcuenta = 4

        IF sdo_tot_v97 IS NULL OR sdo_tot_v97 < 0 THEN
            LET sdo_tot_v97 = 0
            LET sdo_viv_v97 = 0
            LET g_reg.viv97 = 0
            LET g_reg.par97 = 0
        ELSE
            LET sdo_viv_v97 = sdo_tot_v97 * vprecio_accion
            LET g_reg.viv97 = sdo_viv_v97
            LET g_reg.par97 = sdo_tot_v97
        END IF

        LET reg_det_tra_ctas.partic_v97 = g_reg.par97
        LET reg_det_tra_ctas.sdo_viv_97 = g_reg.viv97

        LET sw_1 = 1
        OUTPUT TO REPORT listado_2(reg_det_tra_ctas.*) #2

        DISPLAY "Registros procesados : ", cont_reg AT 14,11

        INSERT INTO safre_tmp:sdo_credito VALUES(g_reg.*)

        LET cont_reg = cont_reg + 1

        LET ult_aport_v97  = 0
        LET sdo_viv_v97    = 0
        LET sdo_tot_v97    = 0
        LET saldo_total97  = 0

        UPDATE acr_det_tra_cred
        SET    estado = 1,
               sdo_viv_97 = g_reg.viv97,
               sdo_viv_92 = 0
        WHERE  nss_afore = reg_det_tra_ctas.nss_afore
        AND    estado = 0
 
        LET sdo_tot_v97 = 0
        LET g_reg.viv97 = 0
        LET g_reg.par97 = 0

        END FOREACH

  {
    IF sw_1 = 0 THEN
        DISPLAY "NO SE ENCONTRARON REGISTROS " AT 19,1 ATTRIBUTE(REVERSE)
        SLEEP 3
        EXIT PROGRAM
    END IF
    }

    LET cont_reg = cont_reg - 1

    FINISH REPORT listado_2 #2

END FUNCTION

REPORT listado_2(reg_det_tra_ctas)
#2--------------------------------

    DEFINE reg_det_tra_ctas RECORD
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
        partic_v97            DECIMAL(22,06),
        sdo_viv_97            DECIMAL(15,02),
        partic_v92            DECIMAL(22,06),
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
            COLUMN 01,reg_det_tra_ctas.tipo_registro        ,
                      cont_reg                              USING "&&&&&&&&&&",
                      reg_det_tra_ctas.tipo_recep_cuenta    ,
                      reg_det_tra_ctas.cve_recep_cuenta     ,
                      reg_det_tra_ctas.tipo_ced_cuenta      ,
                      reg_det_tra_ctas.cve_ced_cuenta       USING "&&&",
                      reg_det_tra_ctas.tipo_transferencia   ,
                      reg_det_tra_ctas.fecha_presentacion   USING "YYYYMMDD",
                      8 SPACES                              ,
                      reg_det_tra_ctas.n_unico_infonavit    ,
                      reg_det_tra_ctas.nss_infonavit        ,
                      15 spaces                             ,
                      reg_det_tra_ctas.rfc_infonavit        ,
                      reg_det_tra_ctas.paterno_infonavit    ,
                      reg_det_tra_ctas.materno_infonavit    ,
                      reg_det_tra_ctas.nombres_infonavit    ,
                      22 spaces                             ,
                      reg_det_tra_ctas.ident_lote_devol     ,
                      15 spaces                             ,
                      reg_det_tra_ctas.nss_afore            ,
                      reg_det_tra_ctas.rfc_afore            ,
                      30 spaces                             ,
                      reg_det_tra_ctas.paterno_afore        ,
                      reg_det_tra_ctas.materno_afore        ,
                      reg_det_tra_ctas.nombres_afore        ,
                      30 spaces                             ,
                  reg_det_tra_ctas.partic_v97 * 1000000 USING "&&&&&&&&&&&&&&&",
                      reg_det_tra_ctas.sdo_viv_97 * 100 USING "&&&&&&&&&&&&&&&",
                      78 spaces                             ,
                      reg_det_tra_ctas.cod_result_operac    ,
                      reg_det_tra_ctas.diag_proceso         ,
                      reg_det_tra_ctas.nombre_imss          ,
                      reg_det_tra_ctas.num_cred_infonavit USING "&&&&&&&&&&",
                      53 spaces                             ,
                      reg_det_tra_ctas.periodo_pago         ,
                      12 spaces
END REPORT

FUNCTION genera_sum_tra_ctas()
#gstc-------------------------

    LET reg_sum_tra_ctas.tipo_registro = "09"
    LET reg_sum_tra_ctas.cant_reg_det  = cont_reg

    SELECT SUM(par97),
           SUM(viv97)
    INTO   reg_sum_tra_ctas.sum_partic_v97,
           reg_sum_tra_ctas.sum_sdo_viv97
    FROM   safre_tmp:sdo_credito

    LET g_sum = g_param_taa.ruta_envio CLIPPED,"/SCG"

    START REPORT listado_3 TO g_sum
        OUTPUT TO REPORT listado_3(reg_sum_tra_ctas.*) #3
    FINISH REPORT listado_3

END FUNCTION

REPORT listado_3(reg_sum_tra_ctas)
#3--------------------------------

    DEFINE reg_sum_tra_ctas RECORD
        tipo_registro        CHAR(02)      ,
        cant_reg_det         DECIMAL(9,0)  ,
        sum_partic_v97       DECIMAL(22,6) ,
        sum_sdo_viv97        DECIMAL(15,2)
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
            COLUMN 01,reg_sum_tra_ctas.tipo_registro      ,
                      reg_sum_tra_ctas.cant_reg_det   USING"&&&&&&&&&",
                      30 spaces                           ,
          reg_sum_tra_ctas.sum_partic_v97 * 1000000 USING"&&&&&&&&&&&&&&&&&&",
                  reg_sum_tra_ctas.sum_sdo_viv97 * 100 USING"&&&&&&&&&&&&&&&",
                      656 spaces
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

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

    DEFINE
        diaTmp DATE,
        contador   SMALLINT,
        diaActual    DATE

    DEFINE
       diaHabilSig DATE,
       diaSemana   SMALLINT,
       feriado     SMALLINT,
       finSemana   SMALLINT

    LET diaHabilSig = diaActual

    WHILE TRUE
        LET feriado   = 0
        LET finSemana = 0
        LET diaSemana = WEEKDAY(diaHabilSig)

        IF diaSemana = 0 OR diaSemana = 6 THEN
            LET finSemana = 1
        END IF
            SELECT *
            FROM   safre_af:tab_feriado
            WHERE  feria_fecha = diaHabilSig

            IF STATUS <> NOTFOUND THEN
               LET feriado = 1
            END IF

            IF feriado = 1 OR finSemana = 1 THEN
                LET diaHabilSig = diaHabilSig + 1 UNITS DAY
            ELSE
                EXIT WHILE
            END IF
    END WHILE

    RETURN diaHabilSig

END FUNCTION

