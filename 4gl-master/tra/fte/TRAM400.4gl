##############################################################################
#Owner                      =>  E.F.P.
#Programa TRAM400           =>  MANTENIMIENTO DE SOLIC. TRA-UNI-SAR-ISSSTE
#Fecha creacion             =>  ABRIL 2013
#By                         =>  GONZALO
#Modificado  By             =>  MARCO ANTONIO GONZALEZ ROJAS
#                           =>  Cuando se agrega una Solicitud se le pone 
#                               status = 20 (Capturado ) Fuente = 5 por 
#                               Captura.
#Fecha de Mod               =>  27 DE ABRIL   DEL 2010  
#Ultima   Mod               =>  DICIEMBRE DEL 2013  CPL-1453
#Sistema                    =>  TRA-UNIFICACION SAR-ISSSTE
##############################################################################
DATABASE safre_af

GLOBALS
DEFINE i_corr                INTEGER
  DEFINE band_conf            SMALLINT
  DEFINE g                    ,
         u                    ,
         vt                   , 
         rr                   ,
         x                    , 
         y                    , 
         z                    , 
         ba                   , 
         gg                   , 
         fff                  , 
         actual_habil         , 
         w                    , 
         valida_modulo SMALLINT

  DEFINE eje_docto     CHAR(180),
         nss_serv      CHAR(011),
         cad_nom       CHAR(100)

  DEFINE tot_saldo_viv LIKE tra_mae_icefa_issste.saldo_viv_92   ,
         tot_saldo_sar LIKE tra_mae_icefa_issste.saldo_sar_92   ,
         sal_saldo_sar LIKE tra_mae_icefa_issste.saldo_sar_92   ,
         sal_saldo_viv LIKE tra_mae_icefa_issste.saldo_sar_92   ,
         int_saldo_sar LIKE tra_mae_icefa_issste.saldo_sar_92   ,
         int_saldo_viv LIKE tra_mae_icefa_issste.saldo_sar_92   ,
         vv_ident      LIKE dev_det_normal.ident_lote_solic

  DEFINE reg_fusion_icefa record like safre_tmp:tra_fusion_icefa.*


  DEFINE reg_saldo     RECORD 
         folio              integer  ,
         n_seguro           char(011),
         cont_servicio      integer
  END RECORD

  DEFINE vv_folio           ,
         d_corr       INTEGER

  DEFINE reg_ap RECORD
         tipo_icefa       LIKE tra_det_trasp_sal_issste.tipo_icefa        ,
         fech_mov_banxico LIKE tra_det_trasp_sal_issste.fech_mov_banxico  ,
         sal_int          CHAR(007)                                ,
         sar_92           LIKE tra_det_trasp_sal_issste.saldo_sar_92      ,
         viv_92           LIKE tra_det_trasp_sal_issste.saldo_viv_92
  END RECORD

  DEFINE reg_ap_dev RECORD 
         estatus         char(010),
         orig_tipo_trasp     like dev_det_normal.orig_tipo_trasp  ,
         acciones_sar_92     like dev_det_normal.acciones_sar_92  ,
         saldo_sar_92_banx   like dev_det_normal.saldo_sar_92_banx,
         saldo_sar_pagar      like dev_det_normal.saldo_sar_pagar  ,
         saldo_viv_922       like dev_det_normal.saldo_viv_92    ,
         saldo_sar_921       like dev_det_normal.saldo_sar_92     ,
         saldo_viv_921       like dev_det_normal.saldo_viv_92     ,
         comis_saldo_sar     like dev_det_normal.comis_saldo_sar  ,
         saldo_sar_paGar_p      like dev_det_planchada.saldo_sar_pagar_p,
         saldo_viv_92_p      like dev_det_planchada.saldo_viv_92_p,
         fecha_presentacion  like dev_det_confronta.fecha_presentacion,
         fecha_devol         like dev_det_normal.fecha_devol,
         diagnostico         like dev_det_normal.diagnostico
  END RECORD

  DEFINE arr_reg_ap_dev ARRAY[50] OF RECORD 
         estatus         char(010),
         orig_tipo_trasp     like dev_det_normal.orig_tipo_trasp  ,
         acciones_sar_92     like dev_det_normal.acciones_sar_92  ,
         saldo_sar_92_banx   like dev_det_normal.saldo_sar_92_banx,
         saldo_sar_pagar       like dev_det_normal.saldo_sar_pagar  ,
         saldo_viv_922       like dev_det_normal.saldo_viv_92    ,
         saldo_sar_921       like dev_det_normal.saldo_sar_92     ,
         saldo_viv_921       like dev_det_normal.saldo_viv_92     ,
         comis_saldo_sar     like dev_det_normal.comis_saldo_sar  ,
         saldo_sar_pagar_p      like dev_det_planchada.saldo_sar_pagar_p,
         saldo_viv_92_p      like dev_det_planchada.saldo_viv_92_p,
         fecha_presentacion  like dev_det_confronta.fecha_presentacion,
         fecha_devol         like dev_det_normal.fecha_devol,
         diagnostico         like dev_det_normal.diagnostico
  END RECORD

  DEFINE arr_reg_ap ARRAY[50] OF RECORD 
         tipo_icefa like tra_det_trasp_sal_issste.tipo_icefa            ,
         fech_mov_banxico like tra_det_trasp_sal_issste.fech_mov_banxico,
         sal_int char(007)                                       ,
         sar_92 like tra_det_trasp_sal_issste.saldo_sar_92              ,
         viv_92 like tra_det_trasp_sal_issste.saldo_viv_92
  END RECORD

  DEFINE arr_cu ARRAY[50] OF RECORD 
         c1 smallint,
         c2 smallint
  END RECORD

  DEFINE reg_1 RECORD #glo #reg_1
         n_folio               LIKE afi_mae_afiliado.n_folio        ,
         tipo_solicitud        LIKE afi_mae_afiliado.tipo_solicitud ,
         n_seguro              LIKE afi_mae_afiliado.n_seguro       ,
         n_rfc                 LIKE afi_mae_afiliado.n_rfc          ,
         n_unico               LIKE afi_mae_afiliado.n_unico        ,
         fentcons              LIKE afi_mae_afiliado.fentcons       ,
         paterno               LIKE afi_mae_afiliado.paterno        ,
         materno               LIKE afi_mae_afiliado.materno        ,
         nombres               LIKE afi_mae_afiliado.nombres 
  END RECORD

  DEFINE reg_2 RECORD #glo #reg_2
         n_folio_tra           LIKE tra_mae_icefa_issste.n_folio_tra      ,
         nss                   LIKE tra_mae_icefa_issste.nss              ,
         rfc                   LIKE tra_mae_icefa_issste.rfc              ,
         fecha_solic_tra       LIKE tra_mae_icefa_issste.fecha_solic_tra  ,
         fecha_captura         LIKE tra_mae_icefa_issste.fecha_captura    ,
         paterno2              LIKE tra_mae_icefa_issste.paterno          ,
         materno2              LIKE tra_mae_icefa_issste.materno          ,
         nombres2              LIKE tra_mae_icefa_issste.nombres          ,
	     cve_sector            LIKE tra_mae_icefa_issste.cve_sector       ,
	     desc_sector           CHAR(15)                                   ,
         origen_traspaso       SMALLINT,
         descripcion           CHAR(025),
         icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod        ,
         des_icefa             CHAR(20)                            ,
         nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta      ,
         saldo_sar_92          LIKE tra_mae_icefa_issste.saldo_sar_92     ,
         saldo_viv_92          LIKE tra_mae_icefa_issste.saldo_viv_92     ,
         fecha_comp_icefa      LIKE tra_mae_icefa_issste.fecha_comp_icefa ,
         fuente                LIKE tra_mae_icefa_issste.fuente           ,
         fecha_proceso         LIKE tra_mae_icefa_issste.fecha_proceso    ,
         status                LIKE tra_mae_icefa_issste.status           ,
         des_estado            CHAR(20)                            ,
         fecha_a_liquidar      DATE                                ,
         fecha_liquidacion     DATE                                ,
         correlativo           LIKE tra_mae_icefa_issste.correlativo      ,
         usuario               LIKE tra_mae_icefa_issste.usuario          ,
         diag_proceso          CHAR(3)                             ,
         des_diagnostico       CHAR(50)                            
       END RECORD

  DEFINE gr_val                  RECORD
         id_procesar             LIKE tra_mae_icefa_issste.id_procesar
                                 END RECORD

  DEFINE reg_2_m RECORD #glo #reg_2
         n_folio_tra           LIKE tra_mae_icefa_issste.n_folio_tra      ,
         nss                   LIKE tra_mae_icefa_issste.nss              ,
         rfc                   LIKE tra_mae_icefa_issste.rfc              ,
         fecha_solic_tra       LIKE tra_mae_icefa_issste.fecha_solic_tra  ,
         fecha_captura         LIKE tra_mae_icefa_issste.fecha_captura    ,
         paterno2              LIKE tra_mae_icefa_issste.paterno          ,
         materno2              LIKE tra_mae_icefa_issste.materno          ,
         nombres2              LIKE tra_mae_icefa_issste.nombres          ,
         icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod        ,
         des_icefa             CHAR(20)                                   ,
         id_procesar           CHAR(08)                                   ,
         origen_traspaso       smallint                                   ,
         descripcion           char(025)                                  ,
         nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta      ,
         saldo_sar_92          LIKE tra_mae_icefa_issste.saldo_sar_92     ,
         saldo_viv_92          LIKE tra_mae_icefa_issste.saldo_viv_92     ,
         fuente                LIKE tra_mae_icefa_issste.fuente           ,
         usuario               LIKE tra_mae_icefa_issste.usuario          ,
         fecha_genera          LIKE tra_mae_icefa_issste.fecha_genera     ,
         fecha_proceso         LIKE tra_mae_icefa_issste.fecha_proceso    ,
         fecha_liquidacion     DATE                                       ,
         status                LIKE tra_mae_icefa_issste.status           ,
         des_estado            CHAR(20)                                   ,
         correlativo           LIKE tra_mae_icefa_issste.correlativo      ,
         diag_proceso          CHAR(3)                                    ,
         des_diagnostico       CHAR(50)
       END RECORD

  DEFINE arr_1  ARRAY[100] OF RECORD #glo #arr_1
        n_folio_tra           LIKE tra_mae_icefa_issste.n_folio_tra      ,
        nss                   LIKE tra_mae_icefa_issste.nss              ,
        rfc                   LIKE tra_mae_icefa_issste.rfc              ,
        fecha_solic_tra       LIKE tra_mae_icefa_issste.fecha_solic_tra  ,
        fecha_captura         LIKE tra_mae_icefa_issste.fecha_captura    ,
        fecha_comp_icefa      LIKE tra_mae_icefa_issste.fecha_comp_icefa ,
        paterno2              LIKE tra_mae_icefa_issste.paterno          ,
        materno2              LIKE tra_mae_icefa_issste.materno          ,
        nombres2              LIKE tra_mae_icefa_issste.nombres          ,
	    cve_sector            LIKE tra_mae_icefa_issste.cve_sector       ,
	    desc_sector           CHAR(015)                                  ,
        origen_traspaso       SMALLINT                                   ,
        descripcion           CHAR(025)                                  ,
        icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod        ,
        des_icefa             CHAR(20)                                   ,
        id_procesar           CHAR(08)                                   ,
        nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta      ,
        saldo_sar_92          LIKE tra_mae_icefa_issste.saldo_sar_92     ,
        saldo_viv_92          LIKE tra_mae_icefa_issste.saldo_viv_92     ,
        fuente                LIKE tra_mae_icefa_issste.fuente           ,
        usuario               LIKE tra_mae_icefa_issste.usuario          ,
        fecha_genera          DATE                                       ,  
        fecha_proceso         DATE                                       ,
        fecha_liquidacion     DATE                                       ,
        estado                LIKE tra_mae_icefa_issste.status           ,
        des_estado            CHAR(20)                                   ,
        correlativo           LIKE tra_mae_icefa_issste.correlativo      ,
        diag_proceso          CHAR(3)                                    ,
        des_diagnostico       CHAR(50)
    END RECORD

  DEFINE arr_1_c  ARRAY[100] OF RECORD #glo #arr_1
	    c1                    SMALLINT,
	    c2                    SMALLINT,
        n_folio_tra           LIKE tra_mae_icefa_issste.n_folio_tra      ,
        nss                   LIKE tra_mae_icefa_issste.nss              ,
        rfc                   LIKE tra_mae_icefa_issste.rfc              ,
        fecha_solic_tra       LIKE tra_mae_icefa_issste.fecha_solic_tra  ,
        fecha_captura         LIKE tra_mae_icefa_issste.fecha_captura    ,
        fecha_comp_icefa      LIKE tra_mae_icefa_issste.fecha_comp_icefa ,
        paterno2              LIKE tra_mae_icefa_issste.paterno          ,
        materno2              LIKE tra_mae_icefa_issste.materno          ,
        nombres2              LIKE tra_mae_icefa_issste.nombres          ,
	    cve_sector            LIKE tra_mae_icefa_issste.cve_sector       ,
	    desc_sector           CHAR(015)                                  ,
        origen_traspaso       SMALLINT                                   ,
        descripcion           CHAR(025)                                  ,
        icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod        ,
        des_icefa             CHAR(20)                                   ,
        id_procesar           CHAR(08)                                   ,
        nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta      ,
        saldo_sar_92          LIKE tra_mae_icefa_issste.saldo_sar_92     ,
        saldo_viv_92          LIKE tra_mae_icefa_issste.saldo_viv_92     ,
        fuente                LIKE tra_mae_icefa_issste.fuente           ,
        usuario               LIKE tra_mae_icefa_issste.usuario          ,
        fecha_genera          DATE                                       ,  
        fecha_proceso         DATE                                       ,
        fecha_liquidacion     DATE                                       ,
        estado                LIKE tra_mae_icefa_issste.status           ,
        des_estado            CHAR(20)                                   ,
        correlativo           LIKE tra_mae_icefa_issste.correlativo      ,
        diag_proceso          CHAR(3)                                    ,
        des_diagnostico       CHAR(50)
    END RECORD

  DEFINE arr_m  ARRAY[100] OF RECORD #arr_m
         n_folio_tra           LIKE tra_mae_icefa_issste.n_folio_tra      ,
         nss                   LIKE tra_mae_icefa_issste.nss              ,
         rfc                   LIKE tra_mae_icefa_issste.rfc              ,
         fecha_solic_tra       LIKE tra_mae_icefa_issste.fecha_solic_tra  ,
         fecha_captura         LIKE tra_mae_icefa_issste.fecha_captura    ,
         fecha_comp_icefa        LIKE tra_mae_icefa_issste.fecha_comp_icefa,
         paterno2              LIKE tra_mae_icefa_issste.paterno          ,
         materno2              LIKE tra_mae_icefa_issste.materno          ,
         nombres2              LIKE tra_mae_icefa_issste.nombres          ,
         icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod        ,
         des_icefa             CHAR(20)                                   ,
         id_procesar           CHAR(08)                                   ,
         origen_traspaso       SMALLINT                                   ,
         descripcion           CHAR(025)                                  ,
         nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta      ,
         saldo_sar_92          LIKE tra_mae_icefa_issste.saldo_sar_92     ,
         saldo_viv_92          LIKE tra_mae_icefa_issste.saldo_viv_92     ,
         fuente                LIKE tra_mae_icefa_issste.fuente           ,
         usuario               LIKE tra_mae_icefa_issste.usuario          ,
         fecha_genera          LIKE tra_mae_icefa_issste.fecha_genera     ,
         fecha_proceso         LIKE tra_mae_icefa_issste.fecha_proceso    ,
         fecha_liquidacion     DATE                                       ,
         estado                LIKE tra_mae_icefa_issste.status           ,
         des_estado            CHAR(20)                                   ,
         correlativo           LIKE tra_mae_icefa_issste.correlativo      ,
         diag_proceso          CHAR(3)                                    ,
         des_diagnostico       CHAR(50)
    END RECORD

  DEFINE arr_m_c  ARRAY[100] OF RECORD #arr_m
         c1                    SMALLINT, 
         c2                    SMALLINT, 
         n_folio_tra           LIKE tra_mae_icefa_issste.n_folio_tra      ,
         nss                   LIKE tra_mae_icefa_issste.nss              ,
         rfc                   LIKE tra_mae_icefa_issste.rfc              ,
         fecha_solic_tra       LIKE tra_mae_icefa_issste.fecha_solic_tra  ,
         fecha_captura         LIKE tra_mae_icefa_issste.fecha_captura    ,
         fecha_comp_icefa      LIKE tra_mae_icefa_issste.fecha_comp_icefa ,
         paterno2              LIKE tra_mae_icefa_issste.paterno          ,
         materno2              LIKE tra_mae_icefa_issste.materno          ,
         nombres2              LIKE tra_mae_icefa_issste.nombres          ,
         icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod        ,
         des_icefa             CHAR(20)                                   ,
         id_procesar           CHAR(08)                                   ,
         origen_traspaso       smallint                                   ,
         descripcion           char(025)                                  ,
         nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta      ,
         saldo_sar_92          LIKE tra_mae_icefa_issste.saldo_sar_92     ,
         saldo_viv_92          LIKE tra_mae_icefa_issste.saldo_viv_92     ,
         fuente                LIKE tra_mae_icefa_issste.fuente           ,
         usuario               LIKE tra_mae_icefa_issste.usuario          ,
         fecha_genera          LIKE tra_mae_icefa_issste.fecha_genera     ,
         fecha_proceso         LIKE tra_mae_icefa_issste.fecha_proceso    ,
         fecha_liquidacion     DATE                                       ,
         estado                LIKE tra_mae_icefa_issste.status           ,
         des_estado            CHAR(20)                                   ,
         correlativo           LIKE tra_mae_icefa_issste.correlativo      ,
         diag_proceso          CHAR(3)                                    ,
         des_diagnostico       CHAR(50)
    END RECORD
    
  DEFINE arr_11  ARRAY[100] OF RECORD #arr_11
         correlativo           DECIMAL(8,0)
    END RECORD

  DEFINE #char
        HORA                  CHAR(008) ,
        c2_ok                 CHAR(002) ,
        x_x                   CHAR(300) ,
        ejecuta               CHAR(200) ,
        x_buscar              CHAR(100) ,
        c8_usuario            CHAR(008) ,
        glo_usuario           CHAR(008) ,
        enter                 CHAR(001) ,
        enter2                CHAR(001) ,
        aux_pausa             CHAR(001) ,
        vdes_icefa            CHAR(020)

  DEFINE #date
        HOY        DATE

  DEFINE #integer
        folio_seleccionado    ,
        i_n_folio             ,
        vrowid                ,
        vfolio                INTEGER

  DEFINE #smallint
        cuantos               ,
        sw_5                  ,
        sw_4                  ,
        sw_3                  ,
        sw_2                  ,
        sw_1                  ,
        sw_11                 ,
        arr_c                 ,
        scr_l                 ,
        ya_existe             ,
        folio_reg             ,
        n_seguro_reg          ,
        rfc_reg               ,
        i                     SMALLINT

  DEFINE arr_100 ARRAY[1000] OF RECORD #arr_100
        folio2                LIKE tra_det_rechazo.folio       ,
        n_seguro              LIKE tra_mae_icefa_issste.nss             ,
        cve_ced_cuenta        LIKE tra_mae_icefa_issste.icefa_cod       ,
        nro_ctrl_icefa        LIKE tra_mae_icefa_issste.nro_int_cta     ,
        nombres               LIKE tra_det_rechazo.nombres
    END RECORD

  DEFINE arr_101 ARRAY[1000] OF RECORD 
        n_seguro_ent          LIKE tra_det_rechazo.n_seguro_ent,
        rfc_ent               LIKE tra_det_rechazo.rfc_ent    
    END RECORD
 
  DEFINE arr_2 ARRAY[1000] OF RECORD
        cod_error             INTEGER ,
        des_error             CHAR(60)
    END RECORD

  DEFINE reg_200 RECORD
         folio2                LIKE tra_det_rechazo.folio       ,
         n_seguro              LIKE tra_mae_icefa_issste.nss             ,
         cve_ced_cuenta        LIKE tra_mae_icefa_issste.icefa_cod       ,
         nro_ctrl_icefa        LIKE tra_mae_icefa_issste.nro_int_cta     ,
         paterno               LIKE tra_det_rechazo.paterno     ,
         materno               LIKE tra_det_rechazo.materno     ,
         nombres               LIKE tra_det_rechazo.nombres
    END RECORD
    
  DEFINE r_pas   RECORD 
        n_seguro_ent          LIKE tra_det_rechazo.n_seguro_ent,
        rfc_ent               LIKE tra_det_rechazo.rfc_ent   
    END RECORD
     
  DEFINE #smallint
        s_nro_icefa           ,
        item_row_cnt          ,
        cont_inp              SMALLINT

  DEFINE #integer
        folio                 ,
        folio_interno         ,
        sql_stat              ,
        i_cont_reg            INTEGER

  DEFINE #serial            #correlativo para elimina
        e_correlativo         LIKE tra_mae_icefa_issste.correlativo

  DEFINE gar_enc              ARRAY[15] OF RECORD 
         folio                LIKE afi_mae_afiliado.n_folio,
         tipo                 LIKE afi_mae_afiliado.tipo_solicitud,
         nss                  LIKE afi_mae_afiliado.n_seguro,
         curp                 LIKE afi_mae_afiliado.n_unico
         END RECORD

  DEFINE gs_cual              SMALLINT 
  
END GLOBALS

MAIN
  OPTIONS PROMPT LINE LAST,
  INPUT WRAP,
  ACCEPT KEY CONTROL-I
  DEFER INTERRUPT

  CALL init()

  OPEN WINDOW ventana_1 AT 2,2 WITH 21 ROWS,78 COLUMNS ATTRIBUTE(BORDER)
  DISPLAY "TRAM400          MANTENIMIENTO SOLICITUDES UNIFICACION SAR-ISSSTE              " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
  DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

  LET valida_modulo      =          0
  LET valida_modulo      =         ARG_VAL(1)
  LET nss_serv                =        ARG_VAL(2)

  IF valida_modulo = 1 THEN
    
     CALL consulta()
     EXIT PROGRAM
     
  END IF

  MENU "ICEFAS"
       COMMAND "Agrega" "Agrega Afiliados"
            CALL agrega() 
            #CALL inicializa()

       COMMAND "Consulta" "Consulta Información Registrada "
            CALL consulta() #c
            #CALL inicializa()

       COMMAND "Modifica" "Modifica Afiliados"
            CALL seleccion() 
            #CALL inicializa() #i

       COMMAND "Busqueda" "Busca Afiliado"
            CALL ayuda() RETURNING reg_1.n_folio        ,
                                   reg_1.tipo_solicitud  
 
             
                                 
       COMMAND "Salir" "Salir del Programa"
            EXIT MENU
  END MENU
  CLOSE WINDOW ventana_1
END MAIN

FUNCTION init()
#i-------------
    LET HOY  = TODAY
    LET HORA = TIME


    SELECT user
    INTO glo_usuario
    FROM tab_afore_local A
    GROUP BY 1

END FUNCTION

FUNCTION inicializa()
#i-------------------
    INITIALIZE reg_1 TO NULL
    INITIALIZE arr_1 TO NULL
    INITIALIZE arr_m TO NULL
    INITIALIZE gr_val.* TO NULL            
    LET  x = 0
    CLEAR FORM
END FUNCTION

FUNCTION inicializa_reg_1()
#ir------------------------
    INITIALIZE reg_1.* TO NULL
    INITIALIZE gr_val.* TO NULL            
    DISPLAY BY NAME reg_1.*
END FUNCTION

### Agrega registros a tra_mae_icefa_issste    Abr 2010 
FUNCTION agrega()      
#a--------------      
  DEFINE    ls_control                SMALLINT 
  DEFINE    li_i                      INTEGER 
  DEFINE    li_x                      INTEGER

    OPEN WINDOW tram4001 AT 2,2 WITH FORM "TRAM4001"                                                   ATTRIBUTE( BORDER)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [Esc] Ingreso [Ctrl-C] Salir                                                  " AT 1,1   ATTRIBUTE(BOLD)
    ###DISPLAY " [Ctrl-B] Pantalla de ingreso de ICEFAS                                        " AT 2,1
    DISPLAY " TRAMI400             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1   ATTRIBUTE(REVERSE) 
    DISPLAY HOY                                                            USING "dd-mm-yyyy" AT 3,67  ATTRIBUTE(REVERSE)
    DISPLAY " AGREGA "                                                                        AT 1,65  ATTRIBUTE(REVERSE,BOLD)
    DISPLAY "                            INGRESO DE ICEFAS                                  " AT 10,1  ATTRIBUTE(REVERSE)
    DISPLAY "                 INFORMACION REGISTRADA EN AFILIACION                          " AT 19,1  ATTRIBUTE(REVERSE)

    LET   ls_control  = 0  

    WHILE TRUE 
         LET      folio_reg = FALSE
         INITIALIZE reg_1 TO NULL
     
         INPUT BY NAME reg_1.n_folio        , 
                       reg_1.tipo_solicitud ,
                       reg_1.n_seguro       ,
             ###       reg_1.n_rfc          ,
                       reg_1.n_unico     WITHOUT DEFAULTS
                       
             AFTER FIELD n_folio
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD n_unico
                 END IF
     
                 IF reg_1.n_folio IS NULL THEN
                     NEXT FIELD n_seguro
                 END IF
     
     
             AFTER FIELD tipo_solicitud
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD n_folio
                 END IF
     
                 IF ( reg_1.tipo_solicitud > 5  AND reg_1.tipo_solicitud <> 8) THEN
                     ERROR"TIPO SOLICITUD INVALIDO"
                     NEXT FIELD tipo_solicitud
                 END IF
     
                 LET      li_i   =   1 
                 SELECT n_folio        ,
                        tipo_solicitud ,
                        n_seguro       ,
                        n_rfc          ,
                        n_unico        ,
                        fentcons       ,
                        paterno        ,
                        materno        ,
                        nombres        ,
                        USER
                 INTO   reg_1.*,c8_usuario
                 FROM   afi_mae_afiliado
                 WHERE  n_folio        = reg_1.n_folio
                 AND    tipo_solicitud = reg_1.tipo_solicitud
                 AND    tipo_solicitud <> 5
     
                 IF STATUS = NOTFOUND THEN
                     ERROR "FOLIO NO EXISTE EN afi_mae_afiliado....NO ES UN AFILIADO ",
                           "DE LA AFORE"
                     NEXT FIELD n_folio
                 ELSE 
                    LET folio_reg = TRUE
                 END IF
     
                 LET  gar_enc[li_i].folio   =  reg_1.n_folio
                 LET  gar_enc[li_i].tipo    =  reg_1.tipo_solicitud 
                 LET  gar_enc[li_i].nss     =  reg_1.n_seguro
                 LET  gar_enc[li_i].curp    =  reg_1.n_unico
                 LET  li_i   = li_i   + 1
                 
                 DISPLAY BY NAME reg_1.*
                 
	         IF reg_1.tipo_solicitud = 5 THEN
                     CALL despliega_mens(reg_1.n_seguro,"NSS asignado aún no vigente  :")
	             NEXT FIELD n_seguro
	         END IF
                 LET  ls_control    =  0 
                 EXIT INPUT 
     
             AFTER FIELD n_seguro
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD n_folio
                 END IF
     
                 IF reg_1.n_seguro IS NULL THEN
                     NEXT FIELD n_unico
                 END IF
    
                 LET      li_i   =   1 
                 DECLARE  c_nss CURSOR FOR 
                  SELECT  n_folio        ,
                          tipo_solicitud ,
                          n_seguro       ,
                          n_rfc          ,
                          n_unico        ,
                          fentcons       ,
                          paterno        ,
                          materno        ,
                          nombres        ,
                          USER
                   INTO   reg_1.*,c8_usuario
                   FROM   afi_mae_afiliado
                  WHERE   n_seguro = reg_1.n_seguro
                    AND   tipo_solicitud <> 5
    
                 FOREACH  c_nss       INTO   reg_1.*
                     LET  gar_enc[li_i].folio   =  reg_1.n_folio
                     LET  gar_enc[li_i].tipo    =  reg_1.tipo_solicitud 
                     LET  gar_enc[li_i].nss     =  reg_1.n_seguro
                     LET  gar_enc[li_i].curp    =  reg_1.n_unico
                     LET  li_i   = li_i   + 1
                 END FOREACH 
 
                 IF STATUS = NOTFOUND THEN
                     ERROR " NSS INEXISTENTE "
                     NEXT FIELD n_seguro
                 ELSE
                      LET folio_reg = TRUE
                 END IF
     
                 DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
	         IF reg_1.tipo_solicitud = 5 THEN
                      CALL despliega_mens(reg_1.n_seguro,"NSS asignado aún no vigente  :")
	              NEXT FIELD n_seguro
	         END IF

                 LET  ls_control    =  0 
                 EXIT INPUT 
     
             AFTER FIELD n_unico
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD n_seguro
                 END IF
     
                 IF reg_1.n_unico IS NULL THEN
                     NEXT FIELD n_folio
                 END IF
     
                 LET      li_i   =   1 
                 DECLARE  c_curp CURSOR FOR 
                  SELECT  n_folio        ,
                          tipo_solicitud ,
                          n_seguro       ,
                          n_rfc          ,
                          n_unico        ,
                          fentcons       ,
                          paterno        ,
                          materno        ,
                          nombres        ,
                          USER
                   INTO   reg_1.*,c8_usuario
                   FROM   afi_mae_afiliado
                  WHERE   n_unico = reg_1.n_unico
                    AND   tipo_solicitud <> 5
     
                 FOREACH  c_curp      INTO   reg_1.*
                     LET  gar_enc[li_i].folio   =  reg_1.n_folio
                     LET  gar_enc[li_i].tipo    =  reg_1.tipo_solicitud 
                     LET  gar_enc[li_i].nss     =  reg_1.n_seguro
                     LET  gar_enc[li_i].curp    =  reg_1.n_unico
                     LET  li_i   = li_i   + 1
                 END FOREACH 
 
                 IF STATUS = NOTFOUND THEN
                     ERROR " CURP INEXISTENTE "
                     NEXT FIELD n_unico
                 ELSE
                     LET folio_reg = TRUE
                 END IF
       
                 DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
	         IF reg_1.tipo_solicitud = 5 THEN
                     CALL despliega_mens(reg_1.n_seguro,"NSS asignado aun no vigente  :")
	             NEXT FIELD n_seguro
	         END IF

                 LET  ls_control    =  0 
                 EXIT INPUT 
     
             ON KEY (INTERRUPT, CONTROL-C )
                 INITIALIZE reg_2.* TO NULL
                 INITIALIZE gr_val.* TO NULL            
                 DISPLAY reg_2.n_folio_tra      TO n_folio_tra
                 DISPLAY reg_2.nss              TO nss
                 DISPLAY reg_2.rfc              TO rfc
                 DISPLAY reg_2.fecha_captura    TO fecha_captura
                 DISPLAY reg_2.paterno2         TO paterno2
                 DISPLAY reg_2.materno2         TO materno2
                 DISPLAY reg_2.nombres2         TO nombres2
                 DISPLAY reg_2.icefa_cod        TO icefa_cod
                 DISPLAY reg_2.nro_int_cta      TO nro_int_cta
                 DISPLAY reg_2.des_icefa        TO des_icefa
                 DISPLAY reg_2.usuario          TO usuario
                 DISPLAY reg_2.cve_sector       TO cve_sector
                 DISPLAY reg_2.desc_sector      TO desc_sector
                 DISPLAY reg_2.origen_traspaso  TO origen_traspaso
                 DISPLAY reg_2.descripcion      TO descripcion
  
                 LET  ls_control =  1
                 EXIT INPUT
       END INPUT
       IF  ls_control   =    1  THEN 
           EXIT WHILE 
       END IF 

       DISPLAY " [Enter] Seleccione para Capturar  [Control-C] Cancelar                   " AT 2,1
       CALL SET_COUNT(li_i - 1)
       DISPLAY ARRAY gar_enc  TO scr_x.*
          ON KEY ( INTERRUPT )
              INITIALIZE reg_2.* TO NULL
              INITIALIZE gr_val.* TO NULL            
              DISPLAY reg_2.n_folio_tra      TO n_folio_tra
              DISPLAY reg_2.nss              TO nss
              DISPLAY reg_2.rfc              TO rfc
              DISPLAY reg_2.fecha_captura    TO fecha_captura
              DISPLAY reg_2.paterno2         TO paterno2
              DISPLAY reg_2.materno2         TO materno2
              DISPLAY reg_2.nombres2         TO nombres2
              DISPLAY reg_2.icefa_cod        TO icefa_cod
              DISPLAY reg_2.nro_int_cta      TO nro_int_cta
              DISPLAY reg_2.des_icefa        TO des_icefa
              DISPLAY reg_2.usuario          TO usuario
              DISPLAY reg_2.cve_sector       TO cve_sector
              DISPLAY reg_2.desc_sector      TO desc_sector
              DISPLAY reg_2.origen_traspaso  TO origen_traspaso
              DISPLAY reg_2.descripcion      TO descripcion

              LET  ls_control =  1
              EXIT DISPLAY
           
          ON KEY (CONTROL-M) 
              
              ### busco el que requiero asigno  
              LET     li_x                     =  ARR_CURR()
              LET     reg_1.n_folio            =  gar_enc[li_x].folio
              LET     reg_1.tipo_solicitud     =  gar_enc[li_x].tipo
              SELECT  n_folio        ,
                      tipo_solicitud ,
                      n_seguro       ,
                      n_rfc          ,
                      n_unico        ,
                      fentcons       ,
                      paterno        ,
                      materno        ,
                      nombres        ,
                      USER
                INTO  reg_1.*,c8_usuario
                FROM  afi_mae_afiliado
               WHERE  n_folio        = reg_1.n_folio
                AND   tipo_solicitud = reg_1.tipo_solicitud
     
              DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
              CALL f_SigueInput() 
              LET  ls_control =  0
              EXIT DISPLAY
       END DISPLAY
       IF  ls_control   =    1  THEN 
           EXIT WHILE 
       END IF 
   END WHILE 
CLOSE WINDOW tram4001
END FUNCTION

FUNCTION consulta()
#c-----------------

  DEFINE #loc #date
        d_fecha_genera          DATE

  DEFINE #loc #char
        c11_n_seguro          CHAR(11),
        c_des_estado          char(035)

  DEFINE #loc #smallint
        s_lote_genera         SMALLINT ,
        i                     SMALLINT

  DEFINE #loc #integer
        i_correlativo         INTEGER,
        li_estatus            SMALLINT


  OPEN WINDOW tram1002 AT 2,2 WITH FORM "TRAM4002" ATTRIBUTE( BORDER)
  DISPLAY " TRAMI400              DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
  DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
  DISPLAY "" AT 1,1
  DISPLAY "" AT 2,1

  IF  valida_modulo = 1 THEN
      ###    opcion eliminadas por el momento 20 Abr 2010 
      ### DISPLAY "[Ctrl-c]Salir [F2]Aportes [F3]Devoluciones                " AT 1,1  ATTRIBUTE(BOLD)

      DISPLAY "[Ctrl-F]Aportes [Ctrl-c]Salir                                    " AT 1,1  ATTRIBUTE(BOLD)
  ELSE
      ###    opciones eliminadas por el momento 20 Abr 2010 
      ###    DISPLAY " [Ctrl-c]Salir [F1]Confirmar [F2]Aportes [F3]Devoluciones " AT 1,1  ATTRIBUTE(BOLD)
      ###    DISPLAY " [F4]Documentos [Ctrl-p]Eliminar                          " AT 2,1  ATTRIBUTE(BOLD)

      DISPLAY " [Ctrl-E]Confirma [Ctrl-F]Aportes [Ctrl-G]Elimina [Ctrl-C]Salir  " AT 1,1  ATTRIBUTE(BOLD)
  END IF
  DISPLAY "            DATOS DEL REG. UNIFICACION SAR-ISSSTE             " AT 8,1  ATTRIBUTE(REVERSE)
  DISPLAY " CONSULTA "                                                     AT 1,65 ATTRIBUTE(REVERSE,BOLD)

  LET sw_1 = 0

  WHILE TRUE

          WHENEVER ERROR CONTINUE
          DROP TABLE ver_consulta
          WHENEVER ERROR STOP

          CREATE TEMP TABLE ver_consulta
          (
           n_seguro char(011),
           n_folio_tra           char(8)      ,
           nss                  char(11)      ,
           rfc                  char(13)      ,
           fecha_solic_tra       date         ,
           fecha_captura         date         ,
           fecha_comp_icefa      date         ,
           paterno2              char(40)     ,
           materno2              char(40)     ,
           nombres2              char(40)     ,
	       cve_sector            smallint     ,
	       desc_sector           char(015)    ,
           origen_traspaso       SMALLINT     ,
           descripcion           CHAR(025)    ,
           icefa_cod             smallint     ,
           des_icefa             CHAR(20)     ,             
           id_procesar           CHAR(08)     ,             
           nro_int_cta           char(30)     ,
           saldo_sar_92          decimal(16,2),
           saldo_viv_92          decimal(16,2),
           fuente                smallint     ,
           usuario               char(8)      ,
           fecha_genera          DATE         ,  
           fecha_proceso         DATE         ,
           fecha_liquidacion     DATE         ,
           estado                smallint     ,
           des_estado            CHAR(20)     ,
           correlativo           integer      ,
           diag_proceso          CHAR(3)      ,
           des_diagnostico       CHAR(50)     ,
	       fecha_genera1          DATE        ,
	       lote_genera           smallint)

          DISPLAY "                  DATOS DEL REG. DE UNIFICACION SAR-ISSSTE                      " AT 8,1 ATTRIBUTE(REVERSE)
          DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

          IF valida_modulo = 1 THEN

                LET reg_1.n_seguro = nss_serv
                SELECT  n_folio        ,
                        tipo_solicitud ,
                        n_seguro       ,
                        n_rfc          ,
                        n_unico        ,
                        fentcons       ,
                        paterno        ,
                        materno        ,
                        nombres
                  INTO  reg_1.*
                  FROM  afi_mae_afiliado
                 WHERE  n_seguro = reg_1.n_seguro

                IF STATUS = NOTFOUND THEN
                    ERROR " NSS INEXISTENTE "
		            SLEEP 5   
                    EXIT PROGRAM
                END IF
                
                DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
          ELSE 

                LET sw_1 =  0
                INPUT BY NAME reg_1.n_folio        ,
                              reg_1.tipo_solicitud ,
                              reg_1.n_seguro       ,
                              reg_1.n_rfc    WITHOUT DEFAULTS

                    AFTER FIELD n_folio
                        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                            NEXT FIELD n_rfc  
                        END IF

                        IF reg_1.n_folio IS NULL THEN
                            NEXT FIELD n_seguro
                        END IF

                    AFTER FIELD tipo_solicitud
                        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                            NEXT FIELD n_folio
                        END IF
            
                        IF ( reg_1.tipo_solicitud > 5  AND reg_1.tipo_solicitud <> 8) THEN
                            ERROR"TIPO SOLICITUD INVALIDO"
                            NEXT FIELD tipo_solicitud
                        END IF

                        SELECT  n_folio        ,
                                tipo_solicitud ,
                                n_seguro       ,
                                n_rfc          ,
                                n_unico        ,
                                fentcons      ,
                                paterno        ,
                                materno        ,
                                nombres
                          INTO  reg_1.*
                          FROM  afi_mae_afiliado
                         WHERE  n_folio        = reg_1.n_folio
                           AND  tipo_solicitud = reg_1.tipo_solicitud
            
                        IF STATUS = NOTFOUND THEN
                            ERROR "FOLIO NO EXISTE EN afi_mae_afiliado....NO ES UN AFILIADO ",
                                  "DE LA AFORE"
                            NEXT FIELD n_folio
                        END IF
                        DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
                        EXIT INPUT

                    AFTER FIELD n_seguro
                        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                            NEXT FIELD n_folio
                        END IF

                        IF reg_1.n_seguro IS NULL THEN
                            NEXT FIELD n_rfc
                        END IF
            
                        SELECT  n_folio        ,
                                tipo_solicitud ,
                                n_seguro       ,
                                n_rfc          ,
                                n_unico        ,
                                fentcons       ,
                                paterno        ,
                                materno        ,
                                nombres
                          INTO  reg_1.*
                          FROM  afi_mae_afiliado
                         WHERE  n_seguro = reg_1.n_seguro
            
                        IF STATUS = NOTFOUND THEN
                            ERROR " NSS INEXISTENTE "
                            NEXT FIELD n_seguro
                        END IF
                        DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
                        EXIT INPUT
            
                    AFTER FIELD n_rfc
                        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                            NEXT FIELD n_seguro
                        END IF
            
                        IF reg_1.n_rfc IS NULL THEN
                            NEXT FIELD n_folio
                        END IF
            
                        SELECT  n_folio        ,
                                tipo_solicitud ,
                                n_seguro       ,
                                n_rfc          ,
                                n_unico        ,
                                fentcons       ,
                                paterno        ,
                                materno        ,
                                nombres
                          INTO  reg_1.*
                          FROM  afi_mae_afiliado
                         WHERE  n_rfc = reg_1.n_rfc
            
                        IF  STATUS = NOTFOUND THEN
                            ERROR " RFC INEXISTENTE "
                            NEXT FIELD n_rfc
                        END IF
                        DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
                        EXIT INPUT
            
                    ON KEY ( INTERRUPT )
                        LET sw_1 = 1
                        EXIT INPUT
                END INPUT
          END IF

          IF sw_1 = 1 THEN
              EXIT WHILE
          END IF

          INSERT INTO ver_consulta
          SELECT  A.n_seguro         ,
                  A.n_folio_tra      ,
                  A.nss              ,
                  A.rfc              ,
                  A.fecha_solic_tra  ,
                  A.fecha_captura    ,
                  A.fecha_comp_icefa ,
                  A.paterno          ,
                  A.materno          ,
                  A.nombres          ,
	              A.cve_sector       ,
	              " "                ,
                  A.origen_traspaso  ,
                  " "                ,
                  A.icefa_cod        ,
                  B.icefa_desc       ,
                  A.id_procesar      ,
                  A.nro_int_cta      ,
                  A.saldo_sar_92     ,
                  A.saldo_viv_92     ,
                  A.fuente           ,
                  A.usuario          , #usuario
                  A.fecha_genera     ,
                  A.fecha_proceso    ,
                  " "                , #fecha_liquidacion
                  A.status           , #estado
                  " "                , #des_estado
                  A.correlativo      , #correlativo
                  " "                , #diag_proceso
                  " "                , #des_diagnostico
                  A.fecha_genera     ,
                  A.lote_genera
            FROM  tra_mae_icefa_issste A, tab_icefa B
           WHERE  A.n_seguro       = reg_1.n_seguro
             AND  A.tipo_solicitud = reg_1.tipo_solicitud
             AND  A.icefa_cod      = B.icefa_cod
      
          INSERT INTO ver_consulta
          SELECT  A.n_seguro         ,
                  " "                ,
                  A.n_seguro_ent     ,
                  A.rfc_ent          ,
                  A.fech_presentacion,
                  " "                ,
                  " "                ,
                  A.paterno          ,
                  A.materno          ,
                  A.nombre           ,
	              A.cve_sector       ,
	              " "                ,
                  " "                ,
                  " "                ,
                  A.cve_ced_cuenta   ,
                  B.icefa_desc       ,
                  " "                ,
                  A.nro_ctrl_icefa   ,
                  A.saldo_sar_92     ,
                  A.saldo_viv_92     ,
                  3                  ,
                  " "                ,
                  " "                ,
                  A.fech_presentacion,
                  " "                , #fecha_liquidacion
                  A.estado           , #estado
                  " "                , #des_estado
                  " "                ,
                  " "                , #diag_proceso
                  " "                , #des_diagnostico
                  " "                ,
                  " "
            FROM  tra_det_sin_normal A, tab_icefa B
           WHERE  A.n_seguro       = reg_1.n_seguro
             AND  A.cve_ced_cuenta = B.icefa_cod
      
          DECLARE cur_1 CURSOR FOR 
          SELECT * from ver_consulta
      
          SELECT  count(*)
            INTO  cuantos
            FROM  ver_consulta        
        ###WHERE  tra_mae_icefa_issste.n_seguro       = reg_1.n_seguro
        ###  AND  tra_mae_icefa_issste.tipo_solicitud = reg_1.tipo_solicitud
      
          DISPLAY "    Icefa: "  AT 8,1 ATTRIBUTE(REVERSE)
          DISPLAY "/"  AT 8,14 ATTRIBUTE(REVERSE)
      
          LET i = 1
      
          FOREACH cur_1 INTO c11_n_seguro,arr_1[i].*,d_fecha_genera,s_lote_genera
      
              LET arr_cu[i].c1 = i
              LET arr_cu[i].c2 = cuantos
      
              SELECT  a.desc_sector 
	          INTO    arr_1[i].desc_sector
	          FROM    tra_cve_sector a
	          WHERE   a.cve_sector = arr_1[i].cve_sector
      
              SELECT  a.descripcion 
	          INTO    arr_1[i].descripcion 
	          FROM    safre_af:tra_tab_tiptra a
	          WHERE   a.origen_traspaso  = arr_1[i].origen_traspaso 
      
              SELECT  des_estado
              INTO    arr_1[i].des_estado
              FROM    tra_status
              WHERE   estado = arr_1[i].estado 
       
              IF arr_1[i].estado = "3" THEN
                
                  CALL codigo_rechazo(reg_1.n_seguro       ,
				                              arr_1[i].nss         ,
                                      arr_1[i].rfc         ,
                                      arr_1[i].icefa_cod   ,
                                      arr_1[i].nro_int_cta ,
                                     #arr_1[i].id_procesar ,  #CPL-1180
                                      d_fecha_genera       ,
                                      s_lote_genera ) #cr
                                      RETURNING arr_1[i].diag_proceso
                  
                  SELECT  A.des_error 
                  INTO    arr_1[i].des_diagnostico
                  FROM    tab_rch_icefa  A
                  WHERE   A.cod_error = arr_1[i].diag_proceso
                  
              END IF
              
              IF ( arr_1[i].estado =  8   OR
	               arr_1[i].estado = 15   OR
	               arr_1[i].estado = 16   OR
	               arr_1[i].estado = 17   OR 
	               arr_1[i].estado = 10 ) THEN
      
                 IF arr_1[i].estado = 10 THEN
      
                    SELECT  max(A.fech_mov_banxico)j
                      INTO  arr_1[i].fecha_liquidacion
                      FROM  tra_det_trasp_sal_issste A
                     WHERE  A.n_seguro               =    c11_n_seguro
                       AND  A.n_seguro_ent           =    arr_1[i].nss
                       AND  A.rfc_ent                =    arr_1[i].rfc
                       AND  A.cve_ced_cuenta         =    arr_1[i].icefa_cod
                       AND  A.nro_ctrl_icefa         =    arr_1[i].nro_int_cta
                       AND  A.tipo_icefa             =    "C"    
      
                 ELSE 
                    
                    SELECT  max(A.fech_mov_banxico)j
                      INTO  arr_1[i].fecha_liquidacion
                      FROM  tra_det_trasp_sal_issste A
                     WHERE  A.n_seguro              =     c11_n_seguro
                       AND  A.n_seguro_ent          =     arr_1[i].nss
                       AND  A.rfc_ent               =     arr_1[i].rfc
                       AND  A.cve_ced_cuenta        =     arr_1[i].icefa_cod
                       AND  A.nro_ctrl_icefa        =     arr_1[i].nro_int_cta
                       AND  A.tipo_icefa            =     "N"    
                 END IF

                 CALL habil_siguiente(arr_1[i].fecha_liquidacion) RETURNING arr_1[i].fecha_liquidacion
                 
              END IF

              IF  arr_1[i].estado = "5" THEN
                  LET arr_1[i].des_diagnostico = null
                  CALL codigo_devol(arr_1[i].nss         ,
                                    arr_1[i].rfc         ,
                                    arr_1[i].icefa_cod   ,
                                    arr_1[i].nro_int_cta ,
                                    d_fecha_genera       ,
                                    s_lote_genera) #cd
                                    
                                    RETURNING arr_1[i].diag_proceso
                                    
                  SELECT  A.des_devol
                    INTO  arr_1[i].des_diagnostico
                    FROM  tab_devolucion A
                   WHERE  A.cod_devol = arr_1[i].diag_proceso
                   
              END IF

              IF (arr_1[i].estado = 8 OR 
	              arr_1[i].estado = 15 OR
	              arr_1[i].estado = 16 OR
	              arr_1[i].estado = 17 OR
	              arr_1[i].estado = 41 OR
	              arr_1[i].estado = 7  ) THEN

	            SELECT  SUM(saldo_sar_92),
		                SUM(saldo_viv_92)
                INTO    sal_saldo_sar ,
		                sal_saldo_viv 
                FROM    tra_det_trasp_sal_issste A  
	            WHERE   A.n_seguro              =      reg_1.n_seguro
  	            AND     A.n_seguro_ent          =      arr_1[i].nss
	            AND     A.rfc_ent               =      arr_1[i].rfc
	            AND     A.cve_ced_cuenta        =      arr_1[i].icefa_cod
	            AND     A.nro_ctrl_icefa        =      arr_1[i].nro_int_cta
         
	            SELECT  SUM(int_sar_92) ,
		                SUM(int_viv_92)
                INTO    int_saldo_sar,
		                int_saldo_viv
                FROM    tra_det_trasp_int_issste A  
	            WHERE   A.n_seguro               =      reg_1.n_seguro
	            AND     A.n_seguro_ent_ced       =      arr_1[i].nss
	            AND     A.rfc_ent_ced            =      arr_1[i].rfc
	            AND     A.cve_ced_cuenta         =      arr_1[i].icefa_cod
	            AND     A.nro_ctrl_icefa         =      arr_1[i].nro_int_cta
         
                    LET arr_1[i].saldo_sar_92      = sal_saldo_sar + int_saldo_sar
                    LET arr_1[i].saldo_viv_92      = sal_saldo_viv + int_saldo_viv
                    
              END IF

              LET i = i + 1
              
          END FOREACH
          
          IF i = 1 THEN
             CALL despliega_mens(reg_1.n_seguro,"Solicitud inexistente para el nss:")
          ELSE

          FOR u = 1 TO i
        
               LET arr_1_c[u].c1                             =      arr_cu[u].c1 
               LET arr_1_c[u].c2                             =      arr_cu[u].c2
               LET arr_1_c[u].n_folio_tra                    =      arr_1[u].n_folio_tra
               LET arr_1_c[u].nss                            =      arr_1[u].nss
               LET arr_1_c[u].rfc                            =      arr_1[u].rfc
               LET arr_1_c[u].fecha_solic_tra                =      arr_1[u].fecha_solic_tra
               LET arr_1_c[u].fecha_captura                  =      arr_1[u].fecha_captura
               LET arr_1_c[u].fecha_comp_icefa               =      arr_1[u].fecha_comp_icefa
               LET arr_1_c[u].paterno2                       =      arr_1[u].paterno2
               LET arr_1_c[u].materno2                       =      arr_1[u].materno2
               LET arr_1_c[u].nombres2                       =      arr_1[u].nombres2
               LET arr_1_c[u].cve_sector                     =      arr_1[u].cve_sector
               LET arr_1_c[u].desc_sector                    =      arr_1[u].desc_sector
               LET arr_1_c[u].origen_traspaso                =      arr_1[u].origen_traspaso
               LET arr_1_c[u].descripcion                    =      arr_1[u].descripcion     
               LET arr_1_c[u].icefa_cod                      =      arr_1[u].icefa_cod
               LET arr_1_c[u].des_icefa                      =      arr_1[u].des_icefa
               LET arr_1_c[u].id_procesar                    =      arr_1[u].id_procesar 
               LET arr_1_c[u].nro_int_cta                    =      arr_1[u].nro_int_cta
               LET arr_1_c[u].saldo_sar_92                   =      arr_1[u].saldo_sar_92
               LET arr_1_c[u].saldo_viv_92                   =      arr_1[u].saldo_viv_92
               LET arr_1_c[u].fuente                         =      arr_1[u].fuente
               LET arr_1_c[u].usuario                        =      arr_1[u].usuario
               LET arr_1_c[u].fecha_genera                   =      arr_1[u].fecha_genera
               LET arr_1_c[u].fecha_proceso                  =      arr_1[u].fecha_proceso
               LET arr_1_c[u].fecha_liquidacion              =      arr_1[u].fecha_liquidacion
               LET arr_1_c[u].estado                         =      arr_1[u].estado
               LET arr_1_c[u].des_estado                     =      arr_1[u].des_estado
               LET arr_1_c[u].correlativo                    =      arr_1[u].correlativo
               LET arr_1_c[u].diag_proceso                   =      arr_1[u].diag_proceso
               LET arr_1_c[u].des_diagnostico                =      arr_1[u].des_diagnostico
          
          END FOR
          CALL SET_COUNT(i-1)
       
          DISPLAY ARRAY arr_1_c TO scr_1.*

               ON KEY ( INTERRUPT )
                
	           IF valida_modulo = 1  THEN
	            
                  CALL inicializa()
	              EXIT DISPLAY
	              
	           END IF
	           
                  CALL inicializa()
	              EXIT DISPLAY

               ON KEY (CONTROL-E)
                
	           IF valida_modulo = 1  THEN
	            
                  CALL inicializa()
                  EXIT DISPLAY
                  
	           END IF
	            
                  LET g = ARR_CURR()
                  
                  IF   arr_1_c[g].estado = 20                    
                    OR arr_1_c[g].estado = 77
                    OR arr_1_c[g].estado = 3        THEN 

                       IF   arr_1_c[g].estado    = 20
                            OR arr_1_c[g].estado = 77      THEN
                            LET    li_estatus   = 1                            
                       END IF
                       
                       IF   arr_1_c[g].estado  = 3      THEN
                        
                           LET    li_estatus   = 9
                           
                       END IF                          
                                             
                                                     
                       PROMPT "DESEA CONFIRMAR ENVIO S/N ? " FOR CHAR enter
                       IF enter MATCHES "[sS]" THEN

                           UPDATE  tra_mae_icefa_issste
                              SET  tra_mae_icefa_issste.status           = li_estatus,
                                   tra_mae_icefa_issste.fecha_solic_tra  = TODAY,
                                   tra_mae_icefa_issste.fecha_comp_icefa = TODAY
                            WHERE  tra_mae_icefa_issste.correlativo      = arr_1_c[g].correlativo                     
                           
            
                           LET arr_1_c[g].estado = li_estatus
     
                           SELECT  a.des_estado
                           INTO    arr_1_c[g].des_estado
                           FROM    tra_status  a
                           WHERE   a.estado = arr_1_c[g].estado
     
                           LET arr_1_c[g].fecha_comp_icefa = TODAY
     
                           ERROR "REGISTRO CONFIRMADO..."
                           SLEEP 2
                           ERROR" "
     
                           DISPLAY arr_1_c[g].estado           TO scr_1.status
                           DISPLAY arr_1_c[g].des_estado       TO scr_1.des_status
                           DISPLAY arr_1_c[g].fecha_comp_icefa TO scr_1.fecha_comp_icefa
     
                       END IF
                       
                  ELSE
                    
                    ERROR "El registro tiene un STATUS que no puede ser confirmado .."
                    SLEEP 3
                    ERROR " "
                    
                  END IF

               ON KEY (CONTROL-F)

                  LET i = ARR_CURR()
                  LET e_correlativo = arr_1_c[i].correlativo

                  OPEN WINDOW tram4009 AT 03,11 WITH FORM "TRAM4009" ATTRIBUTE(BORDER)
                  DISPLAY " [Ctrl-C]Salir                                                   " AT 1,1  ATTRIBUTE(BOLD)
                  DISPLAY "          CONSULTA APORTES SAR ISSSTE Y VIV ISSSTE               " AT 2,1 ATTRIBUTE(REVERSE)

                  DECLARE cur_ap CURSOR FOR
                  SELECT  a.tipo_icefa,
                          a.fech_mov_banxico,
                          "SALDO",
                          a.saldo_sar_92 ,
                          a.saldo_viv_92 ,
                          a.folio
                    FROM  tra_det_trasp_sal_issste a
                   WHERE  a.n_seguro           =       reg_1.n_seguro
                     AND  a.n_seguro_ent       =       arr_1_c[i].nss
                     AND  a.rfc_ent            =       arr_1_c[i].rfc
                     AND  a.cve_ced_cuenta     =       arr_1_c[i].icefa_cod
                     AND  a.nro_ctrl_icefa     =       arr_1_c[i].nro_int_cta
                   ORDER  BY 1

                  LET rr = 1

                  FOREACH cur_ap INTO reg_ap.*,vv_folio

                      LET arr_reg_ap[rr].* = reg_ap.*
                      LET rr = rr + 1

                      SELECT  a.tipo_icefa,
                              a.fech_mov_banxico,
                              "INTERES",
                              a.int_sar_92 ,
                              a.int_viv_92
                        INTO  reg_ap.*
                        FROM  tra_det_trasp_int_issste a
                       WHERE  a.n_seguro           = reg_1.n_seguro
                         AND  a.n_seguro_ent_ced   = arr_1_c[i].nss
                         AND  a.rfc_ent_ced        = arr_1_c[i].rfc
                         AND  a.cve_ced_cuenta     = arr_1_c[i].icefa_cod
                         AND  a.nro_ctrl_icefa     = arr_1_c[i].nro_int_cta
                         AND  a.folio = vv_folio
                      
                      LET arr_reg_ap[rr].* = reg_ap.*
                      LET rr = rr + 1
                      
                      DECLARE cur_udi CURSOR FOR 
                      SELECT  "A"                ,
                              a.fecha_mov_banxico,
                              "UDI"              ,
                              a.saldo_udi        ,
                              0
                        FROM  safre_tmp:tra_det_udi a
                       WHERE  a.n_seguro           = reg_1.n_seguro
                         AND  a.n_seguro_ent       = arr_1_c[i].nss
                         AND  a.rfc_ent            = arr_1_c[i].rfc
                         AND  a.cve_ced_cuenta     = arr_1_c[i].icefa_cod
                         AND  a.nro_ctrl_icefa     = arr_1_c[i].nro_int_cta

                      FOREACH cur_udi INTO   reg_ap.*
                          LET arr_reg_ap[rr].* = reg_ap.*
                          LET rr = rr + 1
                      END FOREACH
                  END FOREACH

                  CALL SET_COUNT(rr-1)

                  DISPLAY ARRAY arr_reg_ap TO scr_ap.*
                       ON KEY(INTERRUPT)
                          EXIT DISPLAY
                  END DISPLAY
                  CLOSE WINDOW tram4009

               ON KEY (CONTROL-G)
                  IF valida_modulo = 1  THEN
                     CALL inicializa()
                     EXIT DISPLAY
                  END IF

                  LET i = ARR_CURR()
                  LET e_correlativo = arr_1_c[i].correlativo

                  IF   arr_1_c[i].estado  = 1
                   OR  arr_1_c[i].estado  = 20
                   OR  arr_1_c[i].estado  = 77 THEN

                      CALL Pregunta(1)
                      
                      IF  aux_pausa MATCHES "[Ss]"   THEN
                          DELETE FROM tra_mae_icefa_issste
                           WHERE correlativo = e_correlativo

                          ERROR "Registro ICEFA Eliminado .." SLEEP 2
                          ERROR ""
                      ELSE
                          ERROR "Eliminación de ICEFA Cancelada .." SLEEP 2
                          ERROR ""
                      END IF
                      
                      CALL inicializa()
                      
                  ELSE
                    
                      LET sw_5 = 1
                      PROMPT "Registro NO puede ser eliminado ....<ENTER> Para Continuar"
                              ATTRIBUTE(REVERSE) FOR enter
                      CALL inicializa()
                      INITIALIZE arr_1_c TO NULL
                      CLEAR FORM
                      EXIT DISPLAY
                  END IF
                  CLEAR FORM
                  EXIT DISPLAY     
          END DISPLAY
          END IF
   END WHILE
   CLOSE WINDOW tram1002

END FUNCTION

FUNCTION elimina(e_correlativo)
#el-----------------------------------
    DEFINE e_correlativo LIKE tra_mae_icefa_issste.correlativo


    LET e_correlativo = arr_1[i].correlativo
 
 DELETE FROM tra_mae_icefa_issste
 WHERE correlativo = e_correlativo

END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE reg_5 RECORD
        nss                   LIKE tra_mae_icefa_issste.nss         ,
        rfc                   LIKE tra_mae_icefa_issste.rfc         ,
        icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod   ,
        nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta
    END RECORD

    OPEN WINDOW tram1005 AT 2,2 WITH FORM "TRAM4005" ATTRIBUTE( BORDER)
    DISPLAY " TRAM400             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY "--               DATOS DEL REG. DE UNIFICACION SAR-ISSSTE                  " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " [ Esc ] Modifica [ Ctrl-C ] Salir                                             " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_3 = 0
    INPUT BY NAME reg_2_m.n_folio_tra      ,
                  reg_2_m.nss              ,
                  reg_2_m.rfc              ,
                  reg_2_m.fecha_solic_tra  ,
                  reg_2_m.fecha_captura    ,
                  reg_2_m.paterno2         ,
                  reg_2_m.materno2         ,
                  reg_2_m.nombres2         ,
                  reg_2_m.icefa_cod        ,
                  reg_2_m.id_procesar       , 
                  reg_2_m.des_icefa        ,
                  reg_2_m.origen_traspaso  ,
                  reg_2_m.descripcion      ,
                  reg_2_m.nro_int_cta      WITHOUT DEFAULTS

        BEFORE FIELD n_folio_tra
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()
            DISPLAY "    Icefa: "  AT 8,1 ATTRIBUTE(REVERSE)
            DISPLAY "/"  AT 8,14 ATTRIBUTE(REVERSE)
            DISPLAY arr_m_c[arr_c].c1 TO c1
            DISPLAY arr_m_c[arr_c].c2 TO c2
            IF sw_3 = 0 THEN
                LET sw_3                       = 1
                LET reg_2_m.n_folio_tra        = arr_m[arr_c].n_folio_tra
                LET reg_2_m.nss                = arr_m[arr_c].nss
                LET reg_2_m.rfc                = arr_m[arr_c].rfc
                LET reg_2_m.fecha_solic_tra    = arr_m[arr_c].fecha_solic_tra
                LET reg_2_m.fecha_captura      = arr_m[arr_c].fecha_captura
                LET reg_2_m.paterno2           = arr_m[arr_c].paterno2
                LET reg_2_m.materno2           = arr_m[arr_c].materno2
                LET reg_2_m.nombres2           = arr_m[arr_c].nombres2
                LET reg_2_m.icefa_cod          = arr_m[arr_c].icefa_cod
                LET reg_2_m.des_icefa          = arr_m[arr_c].des_icefa
                LET reg_2_m.id_procesar        = arr_m[arr_c].id_procesar
                LET reg_2_m.origen_traspaso    = arr_m[arr_c].origen_traspaso
                LET reg_2_m.descripcion        = arr_m[arr_c].descripcion    
                LET reg_2_m.nro_int_cta        = arr_m[arr_c].nro_int_cta
                LET reg_2_m.saldo_sar_92       = arr_m[arr_c].saldo_sar_92
                LET reg_2_m.saldo_viv_92       = arr_m[arr_c].saldo_viv_92
                LET reg_2_m.fuente             = arr_m[arr_c].fuente
                LET reg_2_m.usuario            = arr_m[arr_c].usuario
                LET reg_2_m.fecha_genera       = arr_m[arr_c].fecha_genera
                LET reg_2_m.fecha_proceso      = arr_m[arr_c].fecha_proceso
                LET reg_2_m.fecha_liquidacion  = arr_m[arr_c].fecha_liquidacion
                LET reg_2_m.status             = arr_m[arr_c].estado
                LET reg_2_m.des_estado         = arr_m[arr_c].des_estado
                LET reg_2_m.correlativo        = arr_m[arr_c].correlativo
                LET reg_2_m.diag_proceso       = arr_m[arr_c].diag_proceso
                LET reg_2_m.des_diagnostico    = arr_m[arr_c].des_diagnostico
                                                    
                
                LET reg_5.nss              = arr_m[arr_c].nss
                LET reg_5.rfc              = arr_m[arr_c].rfc
                LET reg_5.icefa_cod        = arr_m[arr_c].icefa_cod
                LET reg_5.nro_int_cta      = arr_m[arr_c].nro_int_cta
                
            END IF

            DISPLAY BY NAME reg_1.* 
            DISPLAY reg_2_m.n_folio_tra       TO n_folio_tra
            DISPLAY reg_2_m.nss               TO nss
            DISPLAY reg_2_m.rfc               TO rfc
            DISPLAY reg_2_m.fecha_solic_tra   TO fecha_solic_tra
            DISPLAY reg_2_m.fecha_captura     TO fecha_captura
            DISPLAY reg_2_m.paterno2          TO paterno2
            DISPLAY reg_2_m.materno2          TO materno2
            DISPLAY reg_2_m.nombres2          TO nombres2
            DISPLAY reg_2_m.icefa_cod         TO icefa_cod
            DISPLAY reg_2_m.des_icefa         TO des_icefa
            DISPLAY reg_2_m.id_procesar       TO id_procesar
            DISPLAY reg_2_m.origen_traspaso   TO origen_traspaso
            DISPLAY reg_2_m.descripcion       TO descripcion
            DISPLAY reg_2_m.nro_int_cta       TO nro_int_cta
            DISPLAY reg_2_m.saldo_sar_92      TO saldo_sar_92
            DISPLAY reg_2_m.saldo_viv_92      TO saldo_viv_92
            DISPLAY reg_2_m.fuente            TO fuente 
            DISPLAY reg_2_m.usuario           TO usuario
            DISPLAY reg_2_m.fecha_genera      TO fecha_genera
            DISPLAY reg_2_m.fecha_proceso     TO fecha_proceso 
            DISPLAY reg_2_m.fecha_liquidacion TO fecha_liquidacion
            DISPLAY reg_2_m.status            TO status
            DISPLAY reg_2_m.des_estado        TO des_estado
            DISPLAY reg_2_m.correlativo       TO correlativo   
            DISPLAY reg_2_m.diag_proceso      TO diag_proceso
            DISPLAY reg_2_m.des_diagnostico   TO des_diagnostico


        AFTER FIELD paterno2
            
            IF reg_2_m.paterno2 IS NULL THEN
                  LET reg_2_m.paterno2 = " "
	        END IF

        AFTER FIELD nombres2
            
            IF reg_2_m.nombres2 IS NULL THEN
	           LET reg_2_m.nombres2 = " "
            END IF

        AFTER FIELD icefa_cod

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres2
            END IF

            IF reg_2_m.icefa_cod IS NULL THEN   #JIRA CPL-1453 Diciembre 2013 Se Añade Clave Icefa 199 
               CALL despliega_icefas()          #Cuentas Inactivas Canceladas Peis
               RETURNING reg_2_m.icefa_cod ,
                         reg_2_m.des_icefa

               IF reg_2_m.icefa_cod = 0 THEN 
                  NEXT FIELD icefa_cod 
               END IF
            ELSE
            	 INITIALIZE reg_2_m.des_icefa TO NULL
               SELECT  icefa_desc
                 INTO  reg_2_m.des_icefa
                 FROM  tab_icefa
                WHERE  icefa_cod = reg_2_m.icefa_cod

               IF STATUS = NOTFOUND THEN
                  ERROR " ICEFA Inexistente "
                  NEXT FIELD icefa_cod
               ELSE #JIRA CPL-1453 Diciembre 2013 Se Añade Clave Icefa 199 
               	    #Cuentas Inactivas Canceladas Peis
	                IF ( reg_2_m.icefa_cod <> '178' AND                    
	                	   reg_2_m.icefa_cod <> '199' ) THEN                 
	                	                                                     
	                	 ERROR " ICEFA NO VALIDA "                        
	                	 LET  reg_2_m.icefa_cod =  NULL                   
	                	 DISPLAY BY NAME reg_2_m.icefa_cod                
	                	 NEXT FIELD icefa_cod                             
	                	                                                  
	                END IF # reg_2_m.icefa_cod <> '178' AND 
                  
               END IF # icefa_desc  IF STATUS = NOTFOUND THEN reg_2_m.icefa_cod <> '199'
               
            END IF

            DISPLAY reg_2_m.icefa_cod TO icefa_cod
            DISPLAY reg_2_m.des_icefa TO des_icefa

            SELECT  a.* 
	        INTO  reg_fusion_icefa.*
	        FROM  safre_tmp:tra_fusion_icefa a
	        WHERE  a.cve_ced_cuenta_org = reg_2_m.icefa_cod

            IF reg_fusion_icefa.cve_ced_cuenta_act <> reg_2_m.icefa_cod THEN
	            ERROR"ICEFA Fusionada...",reg_2_m.icefa_cod," ",
	                                     reg_2_m.des_icefa
	           SLEEP 2
               SELECT  icefa_desc
               INTO  reg_2_m.des_icefa
               FROM  tab_icefa
               WHERE  icefa_cod = reg_fusion_icefa.cve_ced_cuenta_act

               LET reg_2_m.icefa_cod = reg_fusion_icefa.cve_ced_cuenta_act
	    END IF
            DISPLAY reg_2_m.icefa_cod TO icefa_cod
            DISPLAY reg_2_m.des_icefa TO des_icefa

            #Inicia Nva Mod
        AFTER FIELD id_procesar
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD icefa_cod
            END IF

            IF   reg_2_m.id_procesar     IS NULL   
             OR  reg_2_m.id_procesar      =    0  THEN 
                 ERROR " ID_PROCESAR NO puede ser blanco o cero " 
                 SLEEP 2
                 NEXT FIELD  id_procesar
            ELSE
                 NEXT FIELD  origen_traspaso
            END IF 

        AFTER FIELD origen_traspaso

            IF reg_2_m.origen_traspaso IS NULL THEN
                CALL despliega_origen_traspaso() RETURNING reg_2_m.origen_traspaso,
                                                           reg_2_m.descripcion
                SELECT "OK"
                  FROM tra_tab_tiptra a
                 WHERE a.origen_traspaso = reg_2_m.origen_traspaso 

                IF ( STATUS = NOTFOUND ) THEN
                     ERROR " TIPO TRASPASO Inexistente "
                     LET reg_2_m.origen_traspaso   =  NULL
                     DISPLAY BY NAME reg_2_m.origen_traspaso
                     NEXT FIELD origen_traspaso
                END IF 
            ELSE
                SELECT  a.descripcion 
                  INTO  reg_2_m.descripcion
                  FROM  tra_tab_tiptra a
                 WHERE  a.origen_traspaso  = reg_2_m.origen_traspaso

                IF STATUS = NOTFOUND THEN
                     ERROR " TIPO TRASPASO Inexistente "
                     LET reg_2_m.origen_traspaso       =       NULL
                     LET reg_2_m.descripcion           =       NULL
                     DISPLAY BY NAME reg_2_m.origen_traspaso
                     DISPLAY BY NAME reg_2_m.descripcion
                     NEXT FIELD origen_traspaso
                END IF
            END IF

            DISPLAY BY NAME reg_2_m.origen_traspaso
            DISPLAY BY NAME reg_2_m.descripcion

            #Fin de la Nva Mod

        AFTER FIELD nro_int_cta
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD icefa_cod
            END IF

        ON KEY ( INTERRUPT )
            LET sw_4 = 1
            EXIT INPUT

        ON KEY (ESC)
            ERROR "Verificando Información"

            IF reg_2_m.nss IS NULL AND reg_2_m.rfc IS NULL THEN
                ERROR " NSS Y RFC AMBOS NO pueden ser espacio "
                NEXT FIELD nss
            END IF

            IF reg_2_m.paterno2 IS NULL THEN
	             LET reg_2_m.paterno2 = " "
            END IF

            IF reg_2_m.nombres2 IS NULL THEN
	             LET reg_2_m.nombres2 = " "
            END IF
            
            IF (reg_2_m.icefa_cod IS NULL ) OR ( reg_2_m.icefa_cod <> 178 AND reg_2_m.icefa_cod <> 199 ) THEN #JIRA CPL-1453 Diciembre 2013 Se Añade Clave Icefa 199    
                ERROR " CODIGO ICEFA NO puede ser espacio O dif. de 178 ó 199 "                               #Cuentas Inactivas Canceladas Peis 
                NEXT FIELD icefa_cod
            END IF

            IF reg_2_m.origen_traspaso IS NULL THEN
                ERROR " ORIGEN TRASPASO NO puede ser espacio "
                NEXT FIELD origen_traspaso
            END IF

            #ff
            SELECT  "OK"
              FROM  tra_mae_icefa_issste
             WHERE  n_seguro    = reg_1.n_seguro
               AND  nss         = reg_2_m.nss
               AND  rfc         = reg_2_m.rfc
               AND  icefa_cod   = reg_2_m.icefa_cod
               AND  id_procesar = reg_2_m.id_procesar 
             GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR "ICEFA Duplicada" ATTRIBUTE(REVERSE) SLEEP 3
                LET enter = "N"
                NEXT FIELD n_folio_tra
            END IF

            LET sw_4 = 1
            LET sw_3 = 2

            ERROR ""
            PROMPT "DESEA MODIFICAR LA SOLICITUD DE TRASPASO S/N " 
            FOR CHAR enter
            IF  enter MATCHES"[Ss]" THEN
               # ERROR ""
               # PROMPT "DESEA REENVIAR LA SOLICITUD DE TRASPASO S/N " FOR CHAR enter
               # IF enter MATCHES"[Ss]" THEN
               #      UPDATE  tra_mae_icefa_issste
               #      SET     tra_mae_icefa_issste.n_folio_tra      = reg_2_m.n_folio_tra     ,
               #              tra_mae_icefa_issste.nss              = reg_2_m.nss             ,
               #              tra_mae_icefa_issste.rfc              = reg_2_m.rfc             ,
               #              tra_mae_icefa_issste.fecha_captura    = reg_2_m.fecha_captura   ,
               #              tra_mae_icefa_issste.paterno          = reg_2_m.paterno2        ,
               #              tra_mae_icefa_issste.materno          = reg_2_m.materno2        ,
               #              tra_mae_icefa_issste.nombres          = reg_2_m.nombres2        ,
               #              tra_mae_icefa_issste.icefa_cod        = reg_2_m.icefa_cod       ,
               #              tra_mae_icefa_issste.id_procesar      = reg_2_m.id_procesar     ,
               #              tra_mae_icefa_issste.origen_traspaso  = reg_2_m.origen_traspaso,
               #              tra_mae_icefa_issste.nro_int_cta      = reg_2_m.nro_int_cta     ,
               #              tra_mae_icefa_issste.status           = 9                       ,
               #              tra_mae_icefa_issste.fecha_proceso    = HOY                     ,
		       #              tra_mae_icefa_issste.usuario          = glo_usuario
               #       WHERE  tra_mae_icefa_issste.n_seguro         = reg_1.n_seguro
               #       AND    tra_mae_icefa_issste.correlativo      = reg_2_m.correlativo
               # ELSE
                     UPDATE  tra_mae_icefa_issste
                        SET  tra_mae_icefa_issste.n_folio_tra      = reg_2_m.n_folio_tra     ,
                             tra_mae_icefa_issste.nss              = reg_2_m.nss             ,
                             tra_mae_icefa_issste.rfc              = reg_2_m.rfc             ,
                             tra_mae_icefa_issste.fecha_captura    = reg_2_m.fecha_captura   ,
                             tra_mae_icefa_issste.paterno          = reg_2_m.paterno2        ,
                             tra_mae_icefa_issste.materno          = reg_2_m.materno2        ,
                             tra_mae_icefa_issste.nombres          = reg_2_m.nombres2        ,
                             tra_mae_icefa_issste.icefa_cod        = reg_2_m.icefa_cod       ,
                             tra_mae_icefa_issste.id_procesar      = reg_2_m.id_procesar     ,
                             tra_mae_icefa_issste.origen_traspaso  = reg_2_m.origen_traspaso,
                             tra_mae_icefa_issste.nro_int_cta      = reg_2_m.nro_int_cta     ,
                             tra_mae_icefa_issste.fecha_proceso    = HOY,
	                         tra_mae_icefa_issste.usuario          = glo_usuario
                      WHERE  tra_mae_icefa_issste.n_seguro         = reg_1.n_seguro
                        AND  tra_mae_icefa_issste.correlativo      = reg_2_m.correlativo
                END IF
                ERROR "ICEFA MODIFICADA" SLEEP 3 
                ERROR " "

                INITIALIZE reg_2_m.*  TO NULL
                INITIALIZE gr_val.* TO NULL            
    
                DISPLAY reg_2_m.n_folio_tra      TO n_folio_tra
                DISPLAY reg_2_m.nss              TO nss
                DISPLAY reg_2_m.rfc              TO rfc
                DISPLAY reg_2_m.fecha_solic_tra  TO fecha_solic_tra
                DISPLAY reg_2_m.fecha_captura    TO fecha_captura
                DISPLAY reg_2_m.paterno2         TO paterno2
                DISPLAY reg_2_m.materno2         TO materno2
                DISPLAY reg_2_m.nombres2         TO nombres2
                DISPLAY reg_2_m.icefa_cod        TO icefa_cod
                DISPLAY reg_2_m.des_icefa        TO des_icefa
                DISPLAY reg_2_m.id_procesar      TO id_procesar
                DISPLAY reg_2_m.origen_traspaso  TO origen_traspaso 
                DISPLAY reg_2_m.descripcion       TO descripcion
                DISPLAY reg_2_m.nro_int_cta      TO nro_int_cta
                DISPLAY reg_2_m.saldo_sar_92     TO saldo_sar_92
                DISPLAY reg_2_m.saldo_viv_92     TO saldo_viv_92
                DISPLAY reg_2_m.fuente           TO fuente
                DISPLAY reg_2_m.usuario          TO fuente
                DISPLAY reg_2_m.fecha_genera     TO fecha_genera
                DISPLAY reg_2_m.fecha_proceso    TO fecha_proceso 
                DISPLAY reg_2_m.fecha_liquidacion  TO fecha_liquidacion
                DISPLAY reg_2_m.correlativo      TO correlativo   
                CLEAR FORM
                EXIT INPUT
           #ELSE
                CLEAR FORM 
                EXIT INPUT
           #END IF
    END INPUT
    CLOSE WINDOW tram1005
END FUNCTION

FUNCTION despliega_icefas()
#di------------------------
 DEFINE aux_val              SMALLINT
 
 DEFINE l_reg                ARRAY[1000] OF RECORD
        codigo               INTEGER,
        descripcion          CHAR(50)
                                        END RECORD
 DEFINE x_x                  CHAR(100),
        x_buscar             CHAR(30)
        
 DEFINE pos                  SMALLINT

 OPEN WINDOW vent_1 AT 05,12 WITH FORM "TRAM4010" ATTRIBUTE(BORDER)
 DISPLAY "            I C E F A S                                  " AT 2,1 ATTRIBUTE(REVERSE)
 INPUT BY NAME x_buscar
     BEFORE FIELD x_buscar
         LET x_buscar = "*"

     AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
             ERROR "Descripcion a Buscar NO puede ser nulo"
             NEXT FIELD x_buscar
         ELSE
             EXIT INPUT
         END IF
 END INPUT

 WHILE TRUE                                   #JIRA CPL-1453 Diciembre 2013 Se Añade Clave Icefa 199
     LET x_x = " SELECT * FROM tab_icefa ",   #Cuentas Inactivas Canceladas Peis
               " WHERE icefa_desc MATCHES ",'"',x_buscar CLIPPED,'"',
               " AND icefa_cod IN ( '178','199' ) "    CLIPPED    
               
     PREPARE curg19 FROM x_x
     DECLARE cur_g19 CURSOR FOR curg19
     LET pos = 1
     FOREACH cur_g19 INTO l_reg[pos].*
        LET pos = pos + 1
        IF  pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
        END IF
     END FOREACH
     IF (pos-1) < 1 THEN
          ERROR "ARCHIVO ICEFAS... VACIO"
     END IF
     CALL SET_COUNT(pos-1)
     DISPLAY ARRAY l_reg TO scr_1.*
        ON KEY ( INTERRUPT )
           LET pos = 0
           EXIT DISPLAY
        ON KEY ( CONTROL-M )
           LET pos = ARR_CURR()
           EXIT DISPLAY
       END DISPLAY
       IF  pos <> 0 THEN
           EXIT WHILE
       END IF
 END WHILE
 CLOSE WINDOW vent_1
 RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION despliega_sector()
#di------------------------

 DEFINE aux_val  SMALLINT
 DEFINE l_reg ARRAY[1000] OF RECORD
        codigo  INTEGER,
        descripcion CHAR(50)
 END RECORD
 DEFINE x_x  char(100),
 x_buscar  char(30)
 DEFINE pos  SMALLINT

 OPEN WINDOW vent_1 AT 05,12 WITH FORM "TRAM4010" ATTRIBUTE(BORDER)
 DISPLAY "                      CLAVE DE SECTOR                                          " AT 2,1 ATTRIBUTE(REVERSE)
 INPUT BY NAME x_buscar
            BEFORE FIELD x_buscar
                LET x_buscar = "*"

     AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
             ERROR "Descripcion a Buscar NO puede ser nulo"
             NEXT FIELD x_buscar
         ELSE
             EXIT INPUT
         END IF
 END INPUT

 WHILE TRUE
       LET x_x = " SELECT * FROM tra_cve_sector ",
                 " WHERE desc_sector MATCHES ",'"',x_buscar CLIPPED,'"',
                 " ORDER BY 1 " CLIPPED
       PREPARE curg_cve19 FROM x_x
       DECLARE cur_cve CURSOR FOR curg_cve19
       LET pos = 1
       FOREACH cur_cve INTO l_reg[pos].*
        LET pos = pos + 1
        IF pos >= 1000 THEN
    ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
    EXIT FOREACH
        END IF
       END FOREACH
       IF (pos-1) < 1 THEN
          ERROR "ARCHIVO CVE SECTOR... VACIO"
       END IF
       CALL SET_COUNT(pos-1)
       DISPLAY ARRAY l_reg TO scr_1.*
               ON KEY ( INTERRUPT )
           LET pos = 0
           EXIT DISPLAY
        ON KEY ( CONTROL-M )
           LET pos = ARR_CURR()
           EXIT DISPLAY
       END DISPLAY
       IF pos <> 0 THEN
   EXIT WHILE
       END IF
 END WHILE
 CLOSE WINDOW vent_1
 RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION despliega_tipo_comp_icefas()
#dtci--------------------------------
    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo                INTEGER ,
        descripcion       CHAR(50)
    END RECORD

    DEFINE
        x_x        CHAR(100) ,
        x_buscar       CHAR(030)

    DEFINE
        pos                   ,
        aux_val               SMALLINT

    OPEN WINDOW pantallap1 AT 05,12 WITH FORM "TRAM1010" ATTRIBUTE(BORDER)
    DISPLAY "              TIPOS  DE COMPROBANTES (ICEFAS)            "
            AT 2,1 ATTRIBUTE(REVERSE)

    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
     IF x_buscar IS NULL THEN
         ERROR "DESCRIPCION A BUSCAR NO PUEDE SER NULA"
         NEXT FIELD x_buscar
     ELSE
         EXIT INPUT
     END IF
 END INPUT

 WHILE TRUE
     LET x_x = " SELECT * FROM tab_tipo_com ",
               " WHERE  tipcom_desc MATCHES ",'"',x_buscar CLIPPED,'"',
               " ORDER BY 1 " CLIPPED

     PREPARE pre_4 FROM x_x
     DECLARE cur_4 CURSOR FOR pre_4

     LET pos = 1
     FOREACH cur_4 INTO l_reg[pos].*
         LET pos = pos + 1
  IF pos >= 1000 THEN
      ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
      EXIT FOREACH
  END IF
     END FOREACH

     IF (pos-1) < 1 THEN
         ERROR "TABLA TIPO COMPROBANTES ICEFAS... VACIO"
     END IF

     CALL SET_COUNT(pos-1)
     DISPLAY ARRAY l_reg TO scr_1.*
         ON KEY ( INTERRUPT )
      LET pos = 0
      EXIT DISPLAY
  ON KEY ( CONTROL-M )
      LET pos = ARR_CURR()
      EXIT DISPLAY
     END DISPLAY

     IF pos <> 0 THEN
         EXIT WHILE
     END IF
 END WHILE
 CLOSE WINDOW pantallap1
 RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION seleccion()
#s------------------
    DEFINE #loc #date
        d_fecha_genera        DATE

    DEFINE #loc #char
        c_des_estado          CHAR(35) ,
        c11_n_seguro          CHAR(11)

    DEFINE
        s_lote_genera         ,
        i                     ,
        vcont                 SMALLINT

    OPEN WINDOW tram1003 AT 2,2 WITH FORM "TRAM4003" ATTRIBUTE( BORDER)
    DISPLAY " TRAM400             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY "                       DATOS DEL REG. DE UNIFICACION SAR-ISSSTE                 " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_1 = 0
    WHILE TRUE
        DISPLAY "                        DATOS DEL REG. UNI-SAR--ISSSTE                         " AT 8,1   ATTRIBUTE(REVERSE)
        CALL inicializa() #i
        LET      gs_cual  = 0 
        INPUT BY NAME reg_1.n_folio        ,
                      reg_1.tipo_solicitud ,
                      reg_1.n_seguro       ,
                      reg_1.n_rfc          ,
                      reg_1.n_unico  WITHOUT DEFAULTS
    
            AFTER FIELD n_folio
                
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD n_unico
                END IF
    
                IF reg_1.n_folio IS NULL THEN
                    NEXT FIELD n_seguro
                ELSE
                    NEXT FIELD tipo_solicitud
                END IF
    
            AFTER FIELD tipo_solicitud
                
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD n_folio
                END IF
 
                IF     reg_1.tipo_solicitud    IS NULL  
                OR     reg_1.tipo_solicitud =  0  THEN 
                       NEXT FIELD n_folio
                END IF
                 
                LET    gs_cual   = 1    #  folio y tipo  

                EXIT INPUT
    
            AFTER FIELD n_seguro
                
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD n_folio
                END IF
    
                IF reg_1.n_seguro IS NULL THEN
                    NEXT FIELD n_rfc
                END IF
    
                LET    gs_cual   = 2    #  n_seguro      

                
                EXIT INPUT
    
            AFTER FIELD n_rfc
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD n_seguro
                END IF
    
                IF reg_1.n_rfc IS NULL THEN
                    NEXT FIELD n_unico
                END IF
    
                LET    gs_cual   = 3    #  rfc           
                
                EXIT INPUT
    
            AFTER FIELD n_unico
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD n_rfc
                END IF
    
                IF reg_1.n_unico IS NULL THEN
                    NEXT FIELD n_folio
                END IF
    
                LET    gs_cual   = 4    #  n_unico      
                
                EXIT INPUT
    
        ON KEY ( INTERRUPT )
            LET sw_1 = 1
            EXIT INPUT
        END INPUT

        IF sw_1 = 1 THEN
            EXIT WHILE
        END IF
  
       
        IF    gs_cual = 1   THEN    #  folio - tipo 
              DECLARE  cur_33 CURSOR FOR 
               SELECT  A.n_seguro         , A.n_folio_tra      , A.nss              , A.rfc              ,
                       A.fecha_solic_tra  , A.fecha_captura    , A.fecha_comp_icefa , A.paterno          ,
                       A.materno          , A.nombres          , A.icefa_cod        , B.icefa_desc       ,
                       A.id_procesar      ,
                       A.origen_traspaso  , " "                , A.nro_int_cta      , A.saldo_sar_92     ,
                       A.saldo_viv_92     , A.fuente           , A.usuario          , A.fecha_genera     ,
                       A.fecha_proceso    , " "                , #fecha_liquidacion
                       A.status           , #estado
                       " "                , #des_estado
                       A.correlativo      , " "                , #diag_proceso
                       " "                , #des_diag_proceso
                       A.fecha_genera     , A.lote_genera      , C.n_folio          , C.tipo_solicitud   ,
                       C.n_seguro         , C.n_rfc            , C.n_unico          , C.fentcons         ,
                       C.paterno          , C.materno          , C.nombres          
                 FROM  tra_mae_icefa_issste A, 
                       afi_mae_afiliado     C,
                OUTER  tab_icefa            B
                WHERE  A.n_folio          = C.n_folio
                  AND  A.tipo_solicitud   = C.tipo_solicitud
                  AND  A.icefa_cod        = B.icefa_cod
                  AND  A.n_folio          = reg_1.n_folio  
                  AND  A.tipo_solicitud   = reg_1.tipo_solicitud
                  AND  A.status          IN (1, 3, 9, 20, 77)
    
               LET     cuantos   =   0
               SELECT  COUNT(*)
                 INTO  cuantos
                 FROM  tra_mae_icefa_issste A,
                       afi_mae_afiliado     C,
                OUTER  tab_icefa            B
                WHERE  A.n_folio          = C.n_folio
                  AND  A.tipo_solicitud   = C.tipo_solicitud
                  AND  A.icefa_cod        = B.icefa_cod
                  AND  A.n_folio          = reg_1.n_folio  
                  AND  A.tipo_solicitud   = reg_1.tipo_solicitud
                  AND  A.status          IN (1, 3, 9, 20, 77)
       
               IF      cuantos   IS NULL
                OR     cuantos   =   0   THEN 
                   ERROR " Folio y Tipo de Solicitud Inexistente "
                   LET sw_1 = 0
                   CONTINUE WHILE
               END IF
 
               LET i = 1
               FOREACH cur_33 INTO c11_n_seguro   , arr_m[i].*     , d_fecha_genera ,
                                   s_lote_genera  , reg_1.*        
                   LET arr_cu[i].c1 = i
                   LET arr_cu[i].c2 = cuantos
                   LET i = i + 1
               END FOREACH
        END IF 

        IF    gs_cual = 2   THEN    #  n_seguro    
              DECLARE  cur_34 CURSOR FOR
               SELECT  A.n_seguro         , A.n_folio_tra      , A.nss              , A.rfc              ,
                       A.fecha_solic_tra  , A.fecha_captura    , A.fecha_comp_icefa , A.paterno          ,
                       A.materno          , A.nombres          , A.icefa_cod        , B.icefa_desc       ,
                       A.id_procesar      ,
                       A.origen_traspaso  , " "                , A.nro_int_cta      , A.saldo_sar_92     ,
                       A.saldo_viv_92     , A.fuente           , A.usuario          , A.fecha_genera     ,
                       A.fecha_proceso    , " "                , #fecha_liquidacion
                       A.status           , #estado
                       " "                , #des_estado
                       A.correlativo      , " "                , #diag_proceso
                       " "                , #des_diag_proceso
                       A.fecha_genera     , A.lote_genera      , C.n_folio          , C.tipo_solicitud   ,
                       C.n_seguro         , C.n_rfc            , C.n_unico          , C.fentcons         ,
                       C.paterno          , C.materno          , C.nombres          
                 FROM  tra_mae_icefa_issste A,
                       afi_mae_afiliado     C,
                OUTER  tab_icefa            B
                WHERE  A.n_folio          = C.n_folio
                  AND  A.tipo_solicitud   = C.tipo_solicitud
                  AND  A.icefa_cod        = B.icefa_cod
                  AND  A.n_seguro         = reg_1.n_seguro 
                  AND  A.status          IN (1, 3, 9, 20, 77)

               LET     cuantos  = 0 
               SELECT  COUNT(*)
                 INTO  cuantos
                 FROM  tra_mae_icefa_issste A,
                       afi_mae_afiliado     C,
                OUTER  tab_icefa            B
                WHERE  A.n_folio          = C.n_folio
                  AND  A.tipo_solicitud   = C.tipo_solicitud
                  AND  A.icefa_cod        = B.icefa_cod
                  AND  A.n_seguro         = reg_1.n_seguro 
                  AND  A.status          IN (1, 3, 9, 20, 77)

               IF      cuantos   IS NULL
                OR     cuantos   =   0   THEN 
                   ERROR " NSS Inexistente "
                   LET sw_1 = 0
                   CONTINUE WHILE
               END IF
 
               LET i = 1
               FOREACH cur_34 INTO c11_n_seguro   , arr_m[i].*     , d_fecha_genera ,
                                   s_lote_genera  , reg_1.*        
                   LET arr_cu[i].c1 = i
                   LET arr_cu[i].c2 = cuantos
                   LET i = i + 1
               END FOREACH
        END IF

        IF    gs_cual = 3   THEN    #  n_rfc   
              DECLARE  cur_35 CURSOR FOR
               SELECT  A.n_seguro         , A.n_folio_tra      , A.nss              , A.rfc              ,
                       A.fecha_solic_tra  , A.fecha_captura    , A.fecha_comp_icefa , A.paterno          ,
                       A.materno          , A.nombres          , A.icefa_cod        , B.icefa_desc       ,
                       A.id_procesar      ,
                       A.origen_traspaso  , " "                , A.nro_int_cta      , A.saldo_sar_92     ,
                       A.saldo_viv_92     , A.fuente           , A.usuario          , A.fecha_genera     ,
                       A.fecha_proceso    , " "                , #fecha_liquidacion
                       A.status           , #estado
                       " "                , #des_estado
                       A.correlativo      , " "                , #diag_proceso
                       " "                , #des_diag_proceso
                       A.fecha_genera     , A.lote_genera      , C.n_folio          , C.tipo_solicitud   ,
                       C.n_seguro         , C.n_rfc            , C.n_unico          , C.fentcons         ,
                       C.paterno          , C.materno          , C.nombres          
                 FROM  tra_mae_icefa_issste A,
                       afi_mae_afiliado     C,
                OUTER  tab_icefa            B
                WHERE  A.n_folio          = C.n_folio
                  AND  A.tipo_solicitud   = C.tipo_solicitud
                  AND  A.icefa_cod        = B.icefa_cod
                  AND  A.rfc              = reg_1.n_rfc    
                  AND  A.status          IN (1, 3, 9, 20, 77)

               LET     cuantos = 0 
               SELECT  COUNT(*)
                 INTO  cuantos
                 FROM  tra_mae_icefa_issste A,
                       afi_mae_afiliado     C,
                OUTER  tab_icefa            B
                WHERE  A.n_folio          = C.n_folio
                  AND  A.tipo_solicitud   = C.tipo_solicitud
                  AND  A.icefa_cod        = B.icefa_cod
                  AND  A.n_rfc            = reg_1.n_rfc
                  AND  A.status          IN (1, 3, 9, 20, 77)

               IF      cuantos   IS NULL
                OR     cuantos   =   0   THEN 
                   ERROR " RFC Inexistente "
                   LET sw_1 = 0
                   CONTINUE WHILE
               END IF

               LET i = 1
               FOREACH cur_35 INTO c11_n_seguro   , arr_m[i].*     , d_fecha_genera ,
                                   s_lote_genera  , reg_1.*        
                   LET arr_cu[i].c1 = i
                   LET arr_cu[i].c2 = cuantos
                   LET i = i + 1
               END FOREACH
        END IF

        IF    gs_cual = 4   THEN    #  n_rfc
              DECLARE  cur_36 CURSOR FOR
               SELECT  A.n_seguro         , A.n_folio_tra      , A.nss              , A.rfc              ,
                       A.fecha_solic_tra  , A.fecha_captura    , A.fecha_comp_icefa , A.paterno          ,
                       A.materno          , A.nombres          , A.icefa_cod        , B.icefa_desc       ,
                       A.id_procesar      ,
                       A.origen_traspaso  , " "                , A.nro_int_cta      , A.saldo_sar_92     ,
                       A.saldo_viv_92     , A.fuente           , A.usuario          , A.fecha_genera     ,
                       A.fecha_proceso    , " "                , #fecha_liquidacion
                       A.status           , #estado
                       " "                , #des_estado
                       A.correlativo      , " "                , #diag_proceso
                       " "                , #des_diag_proceso
                       A.fecha_genera     , A.lote_genera      , C.n_folio          , C.tipo_solicitud   ,
                       C.n_seguro         , C.n_rfc            , C.n_unico          , C.fentcons         ,
                       C.paterno          , C.materno          , C.nombres          
                 FROM  tra_mae_icefa_issste A,
                       afi_mae_afiliado     C,
                OUTER  tab_icefa            B
                WHERE  A.n_folio          = C.n_folio
                  AND  A.tipo_solicitud   = C.tipo_solicitud
                  AND  A.icefa_cod        = B.icefa_cod
                  AND  C.n_unico          = reg_1.n_unico
                  AND  A.status          IN (1, 3, 9, 20, 77)

               LET     cuantos   =  0 
               SELECT  COUNT(*)
                 INTO  cuantos
                 FROM  tra_mae_icefa_issste A,
                       afi_mae_afiliado     C,
                OUTER  tab_icefa            B
                WHERE  A.n_folio          = C.n_folio
                  AND  A.tipo_solicitud   = C.tipo_solicitud
                  AND  A.icefa_cod        = B.icefa_cod
                  AND  C.n_unico          = reg_1.n_unico
                  AND  A.status          IN (1, 3, 9, 20, 77)

               IF      cuantos   IS NULL
                OR     cuantos   =   0   THEN 
                   ERROR " CURP Inexistente "
                   LET sw_1 = 0
                   CONTINUE WHILE
               END IF

               LET i = 1
               FOREACH cur_36 INTO c11_n_seguro   , arr_m[i].*     , d_fecha_genera ,
                                   s_lote_genera  , reg_1.*        
                   LET arr_cu[i].c1 = i
                   LET arr_cu[i].c2 = cuantos
                   LET i = i + 1
               END FOREACH
        END IF 

        IF     i  =  1   THEN   
           EXIT WHILE 
        END IF

        DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
        DISPLAY " Icefa: "                                                       AT  8, 1  ATTRIBUTE(REVERSE)
        DISPLAY "/"                                                              AT  8, 14 ATTRIBUTE(REVERSE)
        DISPLAY " [Flechas] Revisar Registros [ENTER] Modificar                " AT 2,1
        FOR u = 1 TO i
        
            SELECT  des_estado
              INTO  arr_m[u].des_estado
              FROM  tra_status
             WHERE  estado = arr_m[u].estado 
    
            SELECT  descripcion 
              INTO  arr_m[u].descripcion
              FROM  tra_tab_tiptra a
             WHERE  origen_traspaso = arr_m[u].origen_traspaso
    
            IF arr_m[u].estado = "3" THEN
                CALL codigo_rechazo(reg_1.n_seguro       , arr_m[u].nss         , arr_m[u].rfc         ,
                                    arr_m[u].icefa_cod   , arr_m[u].nro_int_cta , d_fecha_genera       ,
                                    s_lote_genera)                                                        #cr
                                    RETURNING arr_m[u].diag_proceso
                SELECT  A.des_error 
                  INTO  arr_1[u].des_diagnostico
                  FROM  tab_rch_icefa  A
                 WHERE  A.cod_error = arr_1[u].diag_proceso
            END IF
    
            IF ( arr_1[u].estado = 8 
	         OR  arr_1[u].estado = 15
	         OR  arr_1[u].estado = 16
	         OR  arr_1[u].estado = 17) THEN
             
                SELECT  max(A.fech_mov_banxico)j
                  INTO  arr_1[u].fecha_liquidacion
                  FROM  tra_det_trasp_sal_issste A
                 WHERE  A.n_seguro       = c11_n_seguro
                   AND  A.n_seguro_ent   = arr_1[u].nss
                   AND  A.rfc_ent        = arr_1[u].rfc
                   AND  A.cve_ced_cuenta = arr_1[u].icefa_cod
                   AND  A.nro_ctrl_icefa = arr_1[u].nro_int_cta
                   AND  A.tipo_icefa     = "N"    
                CALL habil_siguiente(arr_1[u].fecha_liquidacion) RETURNING arr_1[u].fecha_liquidacion
            END IF
    
            IF arr_m[u].estado = "5" THEN
                LET arr_m[u].des_diagnostico = null
                CALL codigo_devol(arr_m[u].nss         , arr_m[u].rfc         , arr_m[u].icefa_cod   ,
                                  arr_m[u].nro_int_cta , d_fecha_genera       , s_lote_genera) #cd
                                  RETURNING arr_m[u].diag_proceso
    
                SELECT  A.des_devol
                  INTO  arr_m[u].des_diagnostico
                  FROM  tab_devolucion A
                 WHERE  A.cod_devol = arr_m[u].diag_proceso
            END IF
    
            LET arr_m_c[u].c1                        = arr_cu[u].c1 
            LET arr_m_c[u].c2                        = arr_cu[u].c2
            LET arr_m_c[u].n_folio_tra               = arr_m[u].n_folio_tra
            LET arr_m_c[u].nss                       = arr_m[u].nss
            LET arr_m_c[u].rfc                       = arr_m[u].rfc
            LET arr_m_c[u].fecha_solic_tra           = arr_m[u].fecha_solic_tra
            LET arr_m_c[u].fecha_captura             = arr_m[u].fecha_captura
            LET arr_m_c[u].fecha_comp_icefa          = arr_m[u].fecha_comp_icefa
            LET arr_m_c[u].paterno2                  = arr_m[u].paterno2
            LET arr_m_c[u].materno2                  = arr_m[u].materno2
            LET arr_m_c[u].nombres2                  = arr_m[u].nombres2
            LET arr_m_c[u].icefa_cod                 = arr_m[u].icefa_cod
            LET arr_m_c[u].des_icefa                 = arr_m[u].des_icefa
            LET arr_m_c[u].id_procesar               = arr_m[u].id_procesar
            LET arr_m_c[u].origen_traspaso           = arr_m[u].origen_traspaso
            LET arr_m_c[u].descripcion               = arr_m[u].descripcion     
            LET arr_m_c[u].nro_int_cta               = arr_m[u].nro_int_cta
            LET arr_m_c[u].saldo_sar_92              = arr_m[u].saldo_sar_92
            LET arr_m_c[u].saldo_viv_92              = arr_m[u].saldo_viv_92
            LET arr_m_c[u].fuente                    = arr_m[u].fuente
            LET arr_m_c[u].usuario                   = arr_m[u].usuario
            LET arr_m_c[u].fecha_genera              = arr_m[u].fecha_genera
            LET arr_m_c[u].fecha_proceso             = arr_m[u].fecha_proceso
            LET arr_m_c[u].fecha_liquidacion         = arr_m[u].fecha_liquidacion
            LET arr_m_c[u].estado                    = arr_m[u].estado
            LET arr_m_c[u].des_estado                = arr_m[u].des_estado
            LET arr_m_c[u].correlativo               = arr_m[u].correlativo
            LET arr_m_c[u].diag_proceso              = arr_m[u].diag_proceso
            LET arr_m_c[u].des_diagnostico           = arr_m[u].des_diagnostico
        END FOR

        LET sw_5 = 0
        CALL SET_COUNT(i-1)
        DISPLAY ARRAY arr_m_c TO scr_1.*
            ON KEY ( CONTROL-M)
                LET arr_c = ARR_CURR()
                LET scr_l = SCR_LINE()
       
                
                EXIT DISPLAY
    
            ON KEY ( INTERRUPT )
                CALL inicializa() #i
                LET x = 1
                EXIT DISPLAY
        END DISPLAY
        IF x = 1 THEN
             EXIT WHILE
        END IF 
        IF sw_5 = 0 THEN
            CALL modifica() #m
            DISPLAY "                                                                               " AT 2,1 
            CLEAR FORM
        END IF
    END WHILE
    CLOSE WINDOW tram1003
END FUNCTION

FUNCTION trac011()
#t----------------
##############################################################################
#Owner             => E.F.P.
#Programa TRAC011  => CONSULTA DE ICEFAS RECHAZADAS         
#Fecha creacion    => 29 MAYO DE 1998
#By                => FRANCO E. ULLOA V.             
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 31 DE ENERO DEL 2005
#Sistema           => TRA-ICE-ISSSTE
##############################################################################

   define sw_1 smallint
   LET HOY = TODAY

   CALL init()
   OPEN WINDOW tram1006 AT 2,2 WITH FORM "TRAM1006" ATTRIBUTE(BORDER)
   DISPLAY " TRAC011             CONSULTA DE ICEFAS RECHAZADAS                             " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

WHILE TRUE
   INPUT BY NAME folio WITHOUT DEFAULTS
       BEFORE FIELD folio 
           LET folio = NULL

       AFTER FIELD folio
           IF folio IS NULL THEN
               LET x_x = #" SELECT A.rowid   ,",
                                " SELECT A.folio          ,",
                                " A.n_seguro       ,",
                                " A.cve_ced_cuenta ,",
                                " A.nro_ctrl_icefa ,",
                                " A.paterno        ,",
                                " A.materno        ,",
                                " A.nombres        ,",
                                " A.n_seguro_ent   ,",
                                " A.rfc_ent         ",
                         " FROM   tra_det_rechazo A    ",
                         " ORDER BY n_seguro" CLIPPED
               EXIT INPUT
           ELSE
               SELECT "OK"
               FROM   tra_det_rechazo A
               WHERE  A.folio = folio
               GROUP BY 1

               IF STATUS = NOTFOUND THEN
                   ERROR "FOLIO INEXISTENTE"
                   NEXT FIELD folio
               ELSE
                   LET x_x =# " SELECT A.rowid         ,",
                                    " SELECT A.folio   ,",
                                    " A.n_seguro       ,",
                                    " A.cve_ced_cuenta ,",
                                    " A.nro_ctrl_icefa ,",
                                    " A.paterno        ,",
                                    " A.materno        ,",
                                    " A.nombres        ,",
                                    " A.n_seguro_ent   ,",
                                    " A.rfc_ent         ",
                             " FROM   tra_det_rechazo A    ",
                             " WHERE  A.folio = ",folio,
                             " ORDER BY n_seguro" CLIPPED
                    EXIT INPUT
               END IF
           END IF

       ON KEY (INTERRUPT)
         LET sw_11 = 1
         EXIT INPUT 
   END INPUT

   IF sw_11 = 1 THEN
      LET sw_11 = 0 
      EXIT WHILE
   END IF

   PREPARE pre_1 FROM x_x
   DECLARE cur_100 CURSOR FOR pre_1

   LET i_cont_reg = 1

   FOREACH cur_100 INTO reg_200.*,r_pas.*

       LET arr_100[i_cont_reg].folio2         = reg_200.folio2
       LET arr_100[i_cont_reg].n_seguro       = reg_200.n_seguro
       LET arr_100[i_cont_reg].cve_ced_cuenta = reg_200.cve_ced_cuenta
       LET arr_100[i_cont_reg].nro_ctrl_icefa = reg_200.nro_ctrl_icefa
       LET arr_100[i_cont_reg].nombres        = reg_200.paterno CLIPPED," ",
                                                reg_200.materno CLIPPED," ",
                                                reg_200.nombres CLIPPED

       let arr_101[i_cont_reg].n_seguro_ent   = r_pas.n_seguro_ent
       let arr_101[i_cont_reg].rfc_ent        = r_pas.rfc_ent
       LET i_cont_reg = i_cont_reg + 1
       IF i_cont_reg = 1001 THEN
           ERROR"EL ARREGLO FUE LLENADO A SU MAXIMA CAPACIDAD"
           EXIT FOREACH
       END IF
   END FOREACH
IF i_cont_reg = 1 THEN
 CONTINUE WHILE
END IF
   CALL SET_COUNT(i_cont_reg - 1)

  LET cont_inp = TRUE
   WHILE (cont_inp = TRUE)
       INPUT ARRAY arr_100 WITHOUT DEFAULTS FROM scr_1.*

           BEFORE ROW
               LET arr_c = ARR_CURR()
               LET scr_l = SCR_LINE()

               IF (arr_c = i_cont_reg ) THEN
                   LET cont_inp = TRUE
                   EXIT INPUT
               ELSE

                   LET cont_inp = FALSE
                   CALL proc_items(arr_c) RETURNING sql_stat,item_row_cnt

               END IF

           ON KEY (CONTROL-M)
                LET c2_ok = NULL
                SELECT A.n_folio       ,
                       A.tipo_solicitud,
                       "ok"
                INTO   reg_1.n_folio,
                       reg_1.tipo_solicitud,
                       c2_ok
                FROM   tra_mae_icefa_issste A
                WHERE  A.n_seguro     = arr_100[arr_c].n_seguro
                AND    A.nss          = arr_101[arr_c].n_seguro_ent
                AND    A.rfc          = arr_101[arr_c].rfc_ent
                AND    A.icefa_cod    = arr_100[arr_c].cve_ced_cuenta
                AND    A.nro_int_cta  = arr_100[arr_c].nro_ctrl_icefa
                GROUP BY 1,2,3

                IF STATUS = NOTFOUND THEN
                    ERROR "ICEFA NO EXISTE EN EL MAESTRO DE ICEFAS"
                    LET reg_1.n_seguro = arr_100[arr_c].n_seguro
                END IF

                LET sw_1 = TRUE
                EXIT INPUT

           ON KEY (INTERRUPT)
               CALL disp_four_items()
               FOR i = 1 TO 1000
                   INITIALIZE arr_100[i].* TO NULL
               END FOR
               CLEAR FORM
               EXIT INPUT
       END INPUT

       IF sw_1 = 1 then
          EXIT WHILE 
       END IF 
       END WHILE

   IF sw_1 = 1 then
     EXIT WHILE 
  END IF 
  END WHILE
CLOSE WINDOW tram1006
RETURN
END FUNCTION

FUNCTION disp_four_items()
#dfi----------------------
    DEFINE
        i                     SMALLINT

    FOR i = 1 TO 3
        DISPLAY arr_2[i].* TO scr_2[i].*
    END FOR
END FUNCTION

FUNCTION sel_ord_item_pre()
#soip----------------------
    DEFINE
        sql_text              CHAR(1000)

    DEFINE
        sql_stat              INTEGER

    LET sql_text = "SELECT cod_error     ,",
                   "       des_error      ",
                   "FROM   tab_rch_icefa ",
                   "WHERE  cod_error = ?  "
    
    WHENEVER ERROR CONTINUE
        PREPARE sel_item_stmt FROM sql_text
    WHENEVER ERROR STOP

    LET sql_stat = SQLCA.SQLCODE

    IF (NOT sql_stat ) THEN
        WHENEVER ERROR CONTINUE
            DECLARE sel_item_curs CURSOR FOR sel_item_stmt
        WHENEVER ERROR STOP
     
        LET sql_stat = SQLCA.SQLCODE
    END IF
    RETURN sql_stat
END FUNCTION

FUNCTION sel_ord_item(arr_c)
#soi________________________
    DEFINE
        s_diag_proceso_1      ,
        row_cnt               ,
        arr_c                 SMALLINT

    DEFINE
        sql_stat              INTEGER

    CALL sel_ord_item_pre() returning sql_stat
    CALL null_items()

    SELECT A.diag_proceso_1
    INTO   s_diag_proceso_1
    FROM   tra_det_rechazo A
    WHERE  A.n_seguro       = arr_100[arr_c].n_seguro
    AND    A.cve_ced_cuenta = arr_100[arr_c].cve_ced_cuenta
    AND    A.nro_ctrl_icefa = arr_100[arr_c].nro_ctrl_icefa
    AND    A.folio          = arr_100[arr_c].folio2
   #AND    A.correlativo    = arr_100[arr_c].correlativo


    OPEN sel_item_curs USING s_diag_proceso_1

    LET sql_stat = SQLCA.SQLCODE
    LET row_cnt  = 1

    LET arr_c = 1
    WHILE ((NOT sql_stat) AND (row_cnt <= 100))
        LET arr_c = arr_c + 1
        WHENEVER ERROR CONTINUE
            FETCH sel_item_curs INTO arr_2[arr_c].*
        WHENEVER ERROR STOP

        LET sql_stat = SQLCA.SQLCODE

        IF (NOT sql_stat) THEN
            LET row_cnt = row_cnt + 1
        END IF
        IF (sql_stat = 100) THEN
            LET sql_stat = 0
        END IF

        RETURN sql_stat, row_cnt - 1
    END WHILE
END FUNCTION

FUNCTION proc_items(arr_c)
#pi-----------------------
    DEFINE #smallint
        item_row_cnt          ,
        arr_c                 SMALLINT

    DEFINE #integer
        sql_stat              INTEGER

    CALL sel_ord_item(arr_c) RETURNING sql_stat,item_row_cnt

    CALL disp_four_items()   RETURN sql_stat,item_row_cnt
END FUNCTION


FUNCTION null_items()
#ni------------------
    DEFINE
        i                     SMALLINT

    INITIALIZE arr_2[1].* TO NULL

    FOR i = 2 TO 5
        LET arr_2[i].* = arr_2[1].*
    END FOR
END FUNCTION

FUNCTION actualiza_det_sol_rech(reg_3)
#adsr---------------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        nss                   LIKE tra_mae_icefa_issste.nss         ,
        rfc                   LIKE tra_mae_icefa_issste.rfc         ,
        icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod   ,
        nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta ,
        estado                SMALLINT  
    END RECORD

        IF reg_3.nss IS NOT NULL AND reg_3.nss <> "" THEN
            IF reg_3.nro_int_cta IS NOT NULL AND reg_3.nro_int_cta <> "" THEN
                UPDATE tra_det_rechazo
                SET    tra_det_rechazo.estado = 9
                WHERE  tra_det_rechazo.n_seguro_ent   = reg_3.nss
                AND    tra_det_rechazo.cve_ced_cuenta = reg_3.icefa_cod 
                AND    tra_det_rechazo.nro_ctrl_icefa = reg_3.nro_int_cta
                AND    tra_det_rechazo.estado         = 3
            ELSE
                SELECT count(*)
                INTO   s_nro_icefa
                FROM   tra_det_rechazo
                WHERE  tra_det_rechazo.n_seguro_ent   = 9
                AND    tra_det_rechazo.cve_ced_cuenta = reg_3.icefa_cod 
                AND    tra_det_rechazo.estado         = 3

                IF s_nro_icefa = 1 THEN
                    UPDATE tra_det_rechazo
                    SET    tra_det_rechazo.estado = 9
                    WHERE  tra_det_rechazo.n_seguro_ent   = reg_3.nss
                    AND    tra_det_rechazo.cve_ced_cuenta = reg_3.icefa_cod 
                    AND    tra_det_rechazo.estado         = 3
               #ELSE
                    {
                    ERROR "EXISTE MAS DE UN REGISTRO CON ESAS ",
                                "CARACTERISTICAS"
                          NEXT FIELD n_folio_tra
                    }
                END IF
            END IF
        ELSE
            IF reg_3.rfc <> "             " THEN
               IF reg_3.nro_int_cta <> "" THEN
                    UPDATE tra_det_rechazo
                    SET    tra_det_rechazo.estado = 9
                    WHERE  tra_det_rechazo.rfc_ent        = reg_3.rfc
                    AND    tra_det_rechazo.cve_ced_cuenta = reg_3.icefa_cod 
                    AND    tra_det_rechazo.nro_ctrl_icefa = reg_3.nro_int_cta
                    AND    tra_det_rechazo.estado         = 3
                ELSE
                    SELECT count(*)
                    INTO   s_nro_icefa
                    FROM   tra_det_rechazo
                    WHERE  tra_det_rechazo.rfc_ent        = reg_3.rfc
                    AND    tra_det_rechazo.cve_ced_cuenta = reg_3.icefa_cod 
                    AND    tra_det_rechazo.estado         = 3

                    IF s_nro_icefa = 1 THEN
                        UPDATE tra_det_rechazo
                        SET    tra_det_rechazo.estado = 9
                        WHERE  tra_det_rechazo.rfc_ent        = reg_3.rfc
                        AND    tra_det_rechazo.cve_ced_cuenta = reg_3.icefa_cod 
                        AND    tra_det_rechazo.estado         = 3
                    ELSE
                        {
                          ERROR "EXISTE MAS DE UN REGISTRO CON ESAS ",
                                "CARACTERISTICAS"
                          NEXT FIELD n_folio_tra
                        }
                    END IF
                END IF
            END IF
        END IF
END FUNCTION

FUNCTION ayuda()
#a--------------
    DEFINE l_reg ARRAY[3000] OF RECORD
        n_folio        INTEGER  ,
        nombre                CHAR(50) ,
        tipo_solicitud        SMALLINT
    END RECORD

    DEFINE reg RECORD 
        n_folio        INTEGER  ,
        paterno               CHAR(50) ,
        materno               CHAR(50) ,
        nombres               CHAR(50) ,
        tipo_solicitud        SMALLINT
    END RECORD

    DEFINE #date
        hoy                   DATE

    DEFINE #char
        k                     CHAR(200) ,
        enter                 CHAR(001) ,
        x_x        CHAR(600) ,
        x_buscar2             CHAR(070) ,
        x_buscar              CHAR(100)

    DEFINE #smallint
        G_PARAMETRO           ,
        aux_val               SMALLINT

    DEFINE #integer
        pos                   INTEGER

    OPEN WINDOW tram4004 AT 05,12 WITH FORM "TRAM4004" ATTRIBUTE(BORDER)
    DISPLAY "                BUSQUEDA DE AFILIADOS                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [Esc] Busqueda [Flechas] Revisar Registros [Ctrl-C] Salir   " AT 1,1  ATTRIBUTE(BOLD)
    CALL inicializa()
    CONSTRUCT BY NAME x_buscar ON paterno,materno,nombres
        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT)
            LET x = 1
           EXIT CONSTRUCT
    END CONSTRUCT

    if x = 1
    then 
        close window tram4004
        return "",""
    end if

    DISPLAY "PROCESANDO INFORMACION" AT 13,2 ATTRIBUTE (REVERSE)
    LET x_x = " SELECT n_folio,paterno,materno,nombres,tipo_solicitud ",
              " FROM   afi_mae_afiliado ",
              " WHERE  ",x_buscar,
              " ORDER BY paterno " CLIPPED

    PREPARE pre_2 FROM x_x
    DECLARE cur_2 CURSOR FOR pre_2

    LET pos = 1
    FOREACH cur_2 INTO reg.*
        LET l_reg[pos].n_folio = reg.n_folio
        LET l_reg[pos].nombre  = reg.paterno CLIPPED," ",
                                 reg.materno CLIPPED," ",
                                 reg.nombres CLIPPED
        LET l_reg[pos].tipo_solicitud = reg.tipo_solicitud
 LET pos = pos + 1
 IF pos >= 3000 THEN
      ERROR "FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO"
      EXIT FOREACH
 END IF
    END FOREACH

    CALL SET_COUNT(pos-1)
    DISPLAY "                      " AT 13,2
        DISPLAY ARRAY l_reg TO scr_1.*
     ON KEY ( INTERRUPT )
         LET pos = 0
  EXIT DISPLAY

     ON KEY ( CONTROL-M )
  LET pos = ARR_CURR()
  EXIT DISPLAY
 END DISPLAY
 IF pos <> 0 THEN
 END IF
    CLOSE WINDOW tram4004
if pos <> 0  then
    RETURN l_reg[pos].n_folio ,l_reg[pos].tipo_solicitud
else 
    return "",""
end if
END FUNCTION

FUNCTION codigo_rechazo(reg_3)
#cr---------------------------
    DEFINE reg_3 RECORD #loc #reg_3
	   n_seguro              LIKE tra_mae_icefa_issste.n_seguro             ,
           nss                   LIKE tra_mae_icefa_issste.nss            ,
           rfc                   LIKE tra_mae_icefa_issste.rfc            ,
           icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod      ,
           nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta    ,
          #id_procesar           LIKE tra_det_rechazo_iss.id_procesar     ,
           fecha_genera          LIKE tra_mae_icefa_issste.fecha_genera   ,
           lote_genera           LIKE tra_mae_icefa_issste.lote_genera
    END RECORD

    DEFINE #loc #char
        c3_diag_proceso_1     CHAR(3)

    DEFINE reg_4 RECORD #loc #reg_4
        c10_fecha_genera      CHAR(10) ,
        c08_fecha_genera      CHAR(08) ,
        lote_genera           CHAR(03)
    END RECORD
    
    LET reg_4.c10_fecha_genera = reg_3.fecha_genera
    LET reg_4.c08_fecha_genera = reg_4.c10_fecha_genera[07,10],
                                 reg_4.c10_fecha_genera[01,02],
                                 reg_4.c10_fecha_genera[04,05]
    LET reg_4.lote_genera      = reg_3.lote_genera USING"&&&"


                #--- INICIALIZACIÓN DE VARIABLES         ---#  PWD
                #Jira Cpl-1180                
                INITIALIZE c3_diag_proceso_1 TO NULL                
               
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1                             
                FROM   safre_af:tra_det_rechazo_iss A
                WHERE  A.n_seguro                = reg_3.n_seguro 
		            AND    A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
               #AND    A.id_procesar             = reg_3.id_procesar
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1   
                
                IF ( c3_diag_proceso_1 IS NULL OR
                	   c3_diag_proceso_1 = ""    OR
                	   c3_diag_proceso_1 = " " )   THEN 
                	   	
                	LET c3_diag_proceso_1  = NULL
            
                END IF 
                                  
                #-------------------------------------------#
                RETURN c3_diag_proceso_1

    WHENEVER ERROR STOP
END FUNCTION

FUNCTION codigo_devol(reg_3)
#cd-------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        nss                   LIKE tra_mae_icefa_issste.nss          ,
        rfc                   LIKE tra_mae_icefa_issste.rfc          ,
        icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod    ,
        nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta  ,
        fecha_genera          LIKE tra_mae_icefa_issste.fecha_genera ,
        lote_genera           LIKE tra_mae_icefa_issste.lote_genera
    END RECORD

    DEFINE #loc #char
        c2_status_registro    CHAR(02)

    DEFINE reg_4 RECORD #loc #reg_4
        c10_fecha_genera      CHAR(10) ,
        c08_fecha_genera      CHAR(08) ,
        lote_genera           CHAR(03)
    END RECORD
    
    LET reg_4.c10_fecha_genera = reg_3.fecha_genera
    LET reg_4.c08_fecha_genera = reg_4.c10_fecha_genera[07,10],
                                 reg_4.c10_fecha_genera[01,02],
                                 reg_4.c10_fecha_genera[04,05]
    LET reg_4.lote_genera      = reg_3.lote_genera USING"&&&"

    IF reg_3.nss IS NOT NULL THEN
        IF reg_3.rfc IS NOT NULL THEN
            IF reg_3.nro_int_cta IS NOT NULL THEN
                SELECT A.status_registro
                INTO   c2_status_registro
                FROM   tra_det_devol A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro
            ELSE
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro
            END IF
        ELSE
            IF reg_3.nro_int_cta <> " " THEN
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            ELSE
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            END IF
        END IF
    ELSE
        IF reg_3.rfc <> " " THEN
            IF reg_3.nro_int_cta <> " " THEN
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.rfc                     = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            ELSE
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.rfc                     = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            END IF
        END IF
    END IF

END FUNCTION

FUNCTION Pregunta(tipo)
#p--------------------
define tipo smallint
CASE tipo 
WHEN 1
     PROMPT "Esta seguro de eliminar S/N ? " 
     FOR CHAR aux_pausa
     EXIT CASE
WHEN 2
     PROMPT "Esta seguro de mandar a confronta S/N ? " 
     FOR CHAR aux_pausa
     EXIT CASE
WHEN 3
     PROMPT "Esta seguro de eliminar marca S/N ? " 
     FOR CHAR aux_pausa
     EXIT CASE
OTHERWISE 
     EXIT CASE
END CASE
END FUNCTION

FUNCTION habil_siguiente(x_fecha)
#hs------------------------------
    DEFINE x_fecha    DATE
    DEFINE sig_fecha    DATE
    DEFINE dia_semana SMALLINT
    DEFINE ant_habil  SMALLINT

    LET sig_fecha  = x_fecha

    WHILE TRUE

        LET dia_semana = WEEKDAY(sig_fecha)

        IF dia_semana = 0 OR dia_semana = 6 THEN
           LET sig_fecha  = sig_fecha + 1
           CONTINUE WHILE 
        END IF
    
        SELECT "ok" 
        FROM   tab_feriado
        WHERE  feria_fecha = sig_fecha
       
        IF STATUS <> NOTFOUND THEN
           LET sig_fecha  = sig_fecha + 1
           CONTINUE WHILE
        ELSE 
           EXIT WHILE 

        END IF
    END WHILE

    RETURN sig_fecha  
    
END FUNCTION
  
{ 
###   .... Rutina fuera por el momento .... 20 Abr 2010 
FUNCTION busca_indebidos()
#bi-----------------------
define ind smallint
    DEFINE vestado CHAR(010)

    DEFINE l_reg ARRAY[3000] OF RECORD
        folio        INTEGER  ,
        n_seguro       char(011),
        n_seguro_ent   char(011),
        rfc            char(013),
        rfc_ent        char(013),
        cve_ced_cuenta char(003),
        fech_mov_banxico date,
        nro_ctrl_icefa  char(030),
        estado   char(010)
    END RECORD


    DEFINE reg RECORD 
        folio        INTEGER  ,
        n_seguro       char(011),
        n_seguro_ent   char(011),
        rfc            char(013),
        rfc_ent        char(013),
        cve_ced_cuenta char(003),
        fech_mov_banxico date,
        nro_ctrl_icefa char(030),
        estado smallint
    END RECORD

    DEFINE #date
        hoy                   DATE

    DEFINE #char
        k                     CHAR(200) ,
        enter                 CHAR(001) ,
        x_x                   CHAR(600) ,
        x_buscar2             CHAR(270) ,
        x_buscar1             CHAR(270) ,
        x_buscar              CHAR(200)

    DEFINE #smallint
        G_PARAMETRO           ,
        aux_val               SMALLINT

    DEFINE #integer
        pos                   INTEGER

    OPEN WINDOW tram1007 AT 05,12 WITH FORM "TRAM1007" ATTRIBUTE(BORDER)
    DISPLAY "              MARCA DE POSIBLES TRASPASOS INDEBIDOS                            " 
    AT 2,1 ATTRIBUTE(REVERSE)

  CONSTRUCT BY NAME x_buscar1 ON folio,n_seguro,cve_ced_cuenta,fech_mov_banxico
        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT)
            LET x = 1
           EXIT CONSTRUCT
    END CONSTRUCT

if x = 1
then 
let x = 0
close window tram1007
return
end if

    #DISPLAY "PROCESANDO INFORMACION" AT 13,2 ATTRIBUTE (REVERSE)
  LET x_x = " SELECT folio,n_seguro,n_seguro_ent,rfc,rfc_ent,cve_ced_cuenta,",
            " fech_mov_banxico,nro_ctrl_icefa,estado ",
            " FROM   tra_det_trasp_sal_issste ",
            " WHERE  ",x_buscar1,
            " AND    estado IN (15,16,17,41,7,8) ",
            " ORDER BY folio,n_seguro,cve_ced_cuenta " CLIPPED


    PREPARE pre_21 FROM x_x
    DECLARE cur_21 CURSOR FOR pre_21

    LET pos = 1
    FOREACH cur_21 INTO reg.*
        LET l_reg[pos].folio            = reg.folio
        LET l_reg[pos].n_seguro         = reg.n_seguro
        LET l_reg[pos].n_seguro_ent     = reg.n_seguro_ent
        LET l_reg[pos].rfc              = reg.rfc
        LET l_reg[pos].rfc_ent          = reg.rfc_ent
        LET l_reg[pos].cve_ced_cuenta   = reg.cve_ced_cuenta
        LET l_reg[pos].fech_mov_banxico = reg.fech_mov_banxico
        LET l_reg[pos].nro_ctrl_icefa   = reg.nro_ctrl_icefa
        SELECT des_estado
        INTO l_reg[pos].estado
        FROM tra_status
        WHERE estado = reg.estado

  
 LET pos = pos + 1
 IF pos >= 3000 THEN
      ERROR "FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO"
      EXIT FOREACH
 END IF
    END FOREACH


IF pos = 1 THEN
   CALL despliega_mens(x_buscar1[11,21],"Solicitud inexistente para el nss:")
ELSE
    CALL SET_COUNT(pos-1)
    #DISPLAY "                      " AT 13,2

        DISPLAY ARRAY l_reg TO scr_1.*
     ON KEY ( INTERRUPT )
         LET pos = 0
         EXIT DISPLAY

     ON KEY (RETURN)
     LET ind = ARR_CURR()

        SELECT estado 
        into fff
        FROM tra_det_trasp_sal_issste
        WHERE  tra_det_trasp_sal_issste.folio          = l_reg[ind].folio
        AND    tra_det_trasp_sal_issste.n_seguro       = l_reg[ind].n_seguro
        AND    tra_det_trasp_sal_issste.n_seguro_ent   = l_reg[ind].n_seguro_ent
        AND    tra_det_trasp_sal_issste.rfc_ent        = l_reg[ind].rfc_ent
        AND    tra_det_trasp_sal_issste.cve_ced_cuenta = l_reg[ind].cve_ced_cuenta
        AND    tra_det_trasp_sal_issste.nro_ctrl_icefa = l_reg[ind].nro_ctrl_icefa
###
###if fff = 15 or 
###   fff = 17 then
###
###ERROR "Cuenta no se puede marcar ..."
###
###ELSE
###
  CALL consulta_dev(l_reg[ind].folio,
                    l_reg[ind].n_seguro        ,
                    l_reg[ind].n_seguro_ent    ,
                    l_reg[ind].rfc             ,
                    l_reg[ind].rfc_ent         ,
                    l_reg[ind].cve_ced_cuenta  ,
                    l_reg[ind].fech_mov_banxico,
                    l_reg[ind].nro_ctrl_icefa)
 RETURNING vestado
IF vestado IS NOT null THEN
 LET l_reg[ind].estado = vestado
 DISPLAY BY NAME l_reg[ind].estado
END IF
# END IF
 END DISPLAY
 END IF
    CLOSE WINDOW tram1007
END FUNCTION

FUNCTION consulta_dev(reg_dev)
#c---------------------------

  DEFINE v_estado CHAR(010)

    DEFINE reg_dev RECORD 
        folio        INTEGER  ,
        n_seguro       char(011),
        n_seguro_ent   char(011),
        rfc            char(013),
        rfc_ent        char(013),
        cve_ced_cuenta char(003),
        fech_mov_banxico date,
        nro_ctrl_icefa char(030)
    END RECORD

    DEFINE reg_dev1 RECORD LIKE tra_det_trasp_sal_issste.*

    DEFINE #loc #date
        d_fecha_genera        DATE

    DEFINE #loc #char
        c11_n_seguro          CHAR(11)

    DEFINE #loc #smallint
        s_lote_genera         SMALLINT ,
        i                     SMALLINT

    DEFINE #loc #integer
        i_correlativo         INTEGER

    OPEN WINDOW tram1002 AT 2,2 WITH FORM "TRAM10I2" ATTRIBUTE( BORDER)
    DISPLAY " TRAM100             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ F2 ] Marcar [ F3 ] Desmarcar  [ Ctrl-c ] Salir     "
            AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY "                  DATOS DEL REGISTRO DE ICEFA-AFORE ISSSTE                       " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_1 = 0
    DISPLAY "                  DATOS DEL REGISTRO DE ICEFA-AFORE ISSSTE                       " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)


            SELECT n_folio        ,
                   tipo_solicitud ,
                   n_seguro       ,
                   n_rfc          ,
                   n_unico        ,
                   fentcons       ,
                   paterno        ,
                   materno        ,
                   nombres
            INTO   reg_1.*
            FROM   afi_mae_afiliado A
            WHERE  A.n_seguro        = reg_dev.n_seguro


            IF STATUS = NOTFOUND THEN
                ERROR " NSS INEXISTENTE "
                SLEEP 3
            END IF
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
    LET i = 1

    #DECLARE cur_dev CURSOR FOR

    SELECT A.n_seguro         ,
           A.n_folio_tra      ,
           A.nss              ,
           A.rfc              ,
           A.fecha_solic_tra  ,
           A.fecha_captura    ,
           A.fecha_comp_icefa ,
           A.paterno          ,
           A.materno          ,
           A.nombres          ,
           A.icefa_cod        ,
           B.icefa_desc       ,
           A.nro_int_cta      ,
           A.saldo_sar_92     ,
           A.saldo_viv_92     ,
           A.fuente           ,
           A.usuario          ,#usuario
           A.fecha_genera     ,
           A.fecha_proceso    ,
           " "                 ,#fecha_liquidacion
           A.status           ,#estado
           " "                 ,#des_estado
           A.correlativo      ,#correlativo
           " "                 ,#diag_proceso
           " "                 ,#des_diagnostico
           A.fecha_genera     ,
           A.lote_genera
    INTO c11_n_seguro,arr_1[i].*,d_fecha_genera,s_lote_genera
    FROM   tra_mae_icefa_issste A, tab_icefa B
    WHERE  A.n_seguro           = reg_dev.n_seguro
    AND    A.nss                = reg_dev.n_seguro_ent
    AND    A.rfc                = reg_dev.rfc_ent
    AND    A.icefa_cod          = reg_dev.cve_ced_cuenta
    AND    A.nro_int_cta        = reg_dev.nro_ctrl_icefa
    AND    A.icefa_cod          = B.icefa_cod

    #LET i = 1
    #FOREACH cur_dev INTO c11_n_seguro,arr_1[i].*,d_fecha_genera,s_lote_genera

        LET arr_cu[i].c1 = i
        LET arr_cu[i].c2 = cuantos

   LET arr_1[i].des_diagnostico = " " 

        SELECT des_estado
        INTO   arr_1[i].des_estado
        FROM   tra_status
        WHERE  estado = arr_1[i].estado 

        IF arr_1[i].estado = "3" THEN
            CALL codigo_rechazo(reg_1.n_seguro    ,
				arr_1[i].nss         ,
                                arr_1[i].rfc         ,
                                arr_1[i].icefa_cod   ,
                                arr_1[i].nro_int_cta ,
                                d_fecha_genera       ,
                                s_lote_genera
                               ) #cr
            RETURNING arr_1[i].diag_proceso
            SELECT A.des_error 
            INTO   arr_1[i].des_diagnostico
            FROM   tab_rch_icefa  A
            WHERE  A.cod_error = arr_1[i].diag_proceso
	    LET arr_1[i].des_diagnostico = " "
        END IF

         
            SELECT max(A.fech_mov_banxico)j
            INTO   arr_1[i].fecha_liquidacion
            FROM   tra_det_trasp_sal_issste A
            WHERE  A.n_seguro       = c11_n_seguro
            AND    A.n_seguro_ent   = arr_1[i].nss
            AND    A.rfc_ent        = arr_1[i].rfc
            AND    A.cve_ced_cuenta = arr_1[i].icefa_cod
            AND    A.nro_ctrl_icefa = arr_1[i].nro_int_cta
            AND    A.tipo_icefa     = "N"    

            CALL habil_siguiente(arr_1[i].fecha_liquidacion)
            RETURNING arr_1[i].fecha_liquidacion

        IF arr_1[i].estado = "5" THEN
        LET arr_1[i].des_diagnostico = " "
            CALL codigo_devol(arr_1[i].nss         ,
                              arr_1[i].rfc         ,
                              arr_1[i].icefa_cod   ,
                              arr_1[i].nro_int_cta ,
                              d_fecha_genera       ,
                              s_lote_genera
                             ) #cd
            RETURNING arr_1[i].diag_proceso
            SELECT A.des_devol
            INTO   arr_1[i].des_diagnostico
            FROM   tab_devolucion A
            WHERE  A.cod_devol = arr_1[i].diag_proceso
	    LET arr_1[i].des_diagnostico = " "
        END IF
        LET i = i + 1
    #END FOREACH

    DISPLAY "    Icefa: "  AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "/"  AT 8,14 ATTRIBUTE(REVERSE)


FOR u = 1 TO 1
        
LET arr_1_c[u].c1           = 1
LET arr_1_c[u].c2           = 1
LET arr_1_c[u].n_folio_tra  = arr_1[u].n_folio_tra
LET arr_1_c[u].nss          = arr_1[u].nss
LET arr_1_c[u].rfc          = arr_1[u].rfc
LET arr_1_c[u].fecha_solic_tra  = arr_1[u].fecha_solic_tra
LET arr_1_c[u].fecha_captura  = arr_1[u].fecha_captura
LET arr_1_c[u].fecha_comp_icefa = arr_1[u].fecha_comp_icefa
LET arr_1_c[u].paterno2     = arr_1[u].paterno2
LET arr_1_c[u].materno2     = arr_1[u].materno2
LET arr_1_c[u].nombres2     = arr_1[u].nombres2
LET arr_1_c[u].icefa_cod    = arr_1[u].icefa_cod
LET arr_1_c[u].des_icefa    = arr_1[u].des_icefa
LET arr_1_c[u].nro_int_cta  = arr_1[u].nro_int_cta
LET arr_1_c[u].saldo_sar_92  = arr_1[u].saldo_sar_92
LET arr_1_c[u].saldo_viv_92  = arr_1[u].saldo_viv_92
LET arr_1_c[u].fuente  = arr_1[u].fuente
LET arr_1_c[u].usuario  = arr_1[u].usuario
LET arr_1_c[u].fecha_genera  = arr_1[u].fecha_genera
LET arr_1_c[u].fecha_proceso  = arr_1[u].fecha_proceso
LET arr_1_c[u].fecha_liquidacion  = arr_1[u].fecha_liquidacion
LET arr_1_c[u].estado  = arr_1[u].estado
LET arr_1_c[u].des_estado  = arr_1[u].des_estado
LET arr_1_c[u].correlativo  = arr_1[u].correlativo
LET arr_1_c[u].diag_proceso  = arr_1[u].diag_proceso
LET arr_1_c[u].des_diagnostico  = arr_1[u].des_diagnostico

END FOR

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY arr_1_c TO scr_1.*
   
        ON KEY ( INTERRUPT )
            CALL inicializa()
     EXIT DISPLAY


        ON KEY (F3)


       CASE arr_1_c[1].estado 
       WHEN 15 
            CALL Pregunta(3)
            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tra_mae_icefa_issste 
	       SET tra_mae_icefa_issste.status  = 8,
		   tra_mae_icefa_issste.usuario = glo_usuario
	       WHERE tra_mae_icefa_issste.correlativo = arr_1_c[1].correlativo

        UPDATE tra_det_trasp_sal_issste
        SET    tra_det_trasp_sal_issste.estado  = 8
        WHERE  tra_det_trasp_sal_issste.folio          = reg_dev.folio
        AND    tra_det_trasp_sal_issste.n_seguro       = reg_dev.n_seguro
        AND    tra_det_trasp_sal_issste.n_seguro_ent   = reg_dev.n_seguro_ent
        AND    tra_det_trasp_sal_issste.rfc_ent        = reg_dev.rfc_ent
        AND    tra_det_trasp_sal_issste.cve_ced_cuenta = reg_dev.cve_ced_cuenta
        AND    tra_det_trasp_sal_issste.nro_ctrl_icefa = reg_dev.nro_ctrl_icefa
        AND    tra_det_trasp_sal_issste.fech_mov_banxico = reg_dev.fech_mov_banxico

        DELETE from dev_icefa_no_afo
        WHERE   n_seguro         = reg_dev.n_seguro
        AND     n_seguro_ent     = reg_dev.n_seguro_ent
        AND     rfc_ent          = reg_dev.rfc_ent
        AND     cve_ced_cuenta   = reg_dev.cve_ced_cuenta
        AND     nro_ctrl_icefa   = reg_dev.nro_ctrl_icefa
        AND     fech_mov_banxico = reg_dev.fech_mov_banxico

               SELECT des_estado
               INTO v_estado
               FROM tra_status
               WHERE estado =  8

               ERROR "Cuenta desmarcada ..." 
	       SLEEP 3
		
            ELSE
	       ERROR "Modificacion de estado cancelada ..."
	       SLEEP 3
            END IF
           EXIT CASE
        WHEN 16
           CALL Pregunta(3)
	   IF aux_pausa MATCHES "[Ss]" THEN

              SELECT A.correlativo 
	      into   d_corr
	      FROM   tra_mae_icefa_issste A 
	      WHERE  A.n_seguro    = reg_dev.n_seguro
	      AND    A.nss         = reg_dev.n_seguro_ent
	      AND    A.rfc         = reg_dev.rfc_ent
	      AND    A.icefa_cod   = reg_dev.cve_ced_cuenta
	      AND    A.nro_int_cta = reg_dev.nro_ctrl_icefa

              CALL f_desmarca_cuenta(reg_dev.n_seguro,270,d_corr)

               UPDATE tra_mae_icefa_issste 
	       SET tra_mae_icefa_issste.status = 8,
		   tra_mae_icefa_issste.usuario = glo_usuario
	       WHERE tra_mae_icefa_issste.correlativo = arr_1_c[1].correlativo

        UPDATE tra_det_trasp_sal_issste
        SET    tra_det_trasp_sal_issste.estado  = 8
        WHERE  tra_det_trasp_sal_issste.folio          = reg_dev.folio
        AND    tra_det_trasp_sal_issste.n_seguro       = reg_dev.n_seguro
        AND    tra_det_trasp_sal_issste.n_seguro_ent   = reg_dev.n_seguro_ent
        AND    tra_det_trasp_sal_issste.rfc_ent        = reg_dev.rfc_ent
        AND    tra_det_trasp_sal_issste.cve_ced_cuenta = reg_dev.cve_ced_cuenta
        AND    tra_det_trasp_sal_issste.nro_ctrl_icefa = reg_dev.nro_ctrl_icefa
        AND    tra_det_trasp_sal_issste.fech_mov_banxico = reg_dev.fech_mov_banxico

       DELETE from dev_det_val_conf 
       WHERE  n_seguro        = reg_dev.n_seguro
       AND    n_seguro_ent    = reg_dev.n_seguro_ent
       AND    rfc_ent         = reg_dev.rfc_ent
       AND    icefa_receptora = reg_dev.cve_ced_cuenta
       AND    nro_ctrl_icefa  = reg_dev.nro_ctrl_icefa
       AND    orig_tipo_trasp = "08"

               SELECT des_estado
               INTO v_estado
               FROM tra_status
               WHERE estado =  8

               ERROR "Cuenta desmarcada ..." 
	       SLEEP 3
		
            ELSE
	       ERROR "Modificacion de estado cancelada ..."
	       SLEEP 3
            END IF
         EXIT CASE

     OTHERWISE

         ERROR "Cuenta no se puede desmarcar ...."
	 SLEEP 3
	 EXIT CASE
      END CASE
      EXIT DISPLAY

        ON KEY (F2)
            #LET i = ARR_CURR()
            #LET e_correlativo = arr_1_c[i].correlativo

            IF arr_1_c[1].estado = 17 OR 
               arr_1_c[1].estado = 15 OR 
               arr_1_c[1].estado = 16 THEN
              ERROR "Cuenta no se puede marcar ..."
              SLEEP 3
              EXIT DISPLAY
            END IF

            CALL Pregunta(2)
            IF aux_pausa MATCHES "[Ss]" THEN

        UPDATE tra_mae_icefa_issste 
        SET status =  15,
	    usuario = glo_usuario
        WHERE n_seguro    = reg_dev.n_seguro
        and   nss         = reg_dev.n_seguro_ent
        and   rfc         = reg_dev.rfc_ent
        and   icefa_cod   = reg_dev.cve_ced_cuenta
        and   nro_int_cta = reg_dev.nro_ctrl_icefa

        UPDATE tra_det_trasp_sal_issste
        SET    tra_det_trasp_sal_issste.estado  = 15
        WHERE  tra_det_trasp_sal_issste.folio          = reg_dev.folio
        AND    tra_det_trasp_sal_issste.n_seguro       = reg_dev.n_seguro
        AND    tra_det_trasp_sal_issste.n_seguro_ent   = reg_dev.n_seguro_ent
        AND    tra_det_trasp_sal_issste.rfc_ent        = reg_dev.rfc_ent
        AND    tra_det_trasp_sal_issste.cve_ced_cuenta = reg_dev.cve_ced_cuenta
        AND    tra_det_trasp_sal_issste.nro_ctrl_icefa = reg_dev.nro_ctrl_icefa
        AND    tra_det_trasp_sal_issste.fech_mov_banxico = reg_dev.fech_mov_banxico

 SELECT des_estado
 INTO v_estado
 FROM tra_status
 WHERE estado =  15


    SELECT *
    INTO reg_dev1.*
    FROM tra_det_trasp_sal_issste
    WHERE  tra_det_trasp_sal_issste.folio          = reg_dev.folio
    AND    tra_det_trasp_sal_issste.n_seguro       = reg_dev.n_seguro
    AND    tra_det_trasp_sal_issste.n_seguro_ent   = reg_dev.n_seguro_ent
    AND    tra_det_trasp_sal_issste.rfc_ent        = reg_dev.rfc_ent
    AND    tra_det_trasp_sal_issste.cve_ced_cuenta = reg_dev.cve_ced_cuenta
    AND    tra_det_trasp_sal_issste.nro_ctrl_icefa = reg_dev.nro_ctrl_icefa
    AND    tra_det_trasp_sal_issste.fech_mov_banxico = reg_dev.fech_mov_banxico
    
    INSERT INTO dev_icefa_no_afo
    VALUES 
("08",
reg_dev1.n_seguro,
reg_dev1.n_seguro_ent,
reg_dev1.rfc_ent,
reg_dev1.nombre_ent,
reg_dev1.cve_ced_cuenta,
reg_dev1.nro_ctrl_icefa,
reg_dev1.fech_presentacion,
reg_dev1.fech_mov_banxico,
reg_dev1.tipo_icefa,
100,
0,
0)

               ERROR "ICEFA IDENTIFICADA... " SLEEP 2
               ERROR ""
               CALL inicializa_reg_1()
            ELSE 
               ERROR "IDENTIFICACION DE ICEFA CANCELADA..." SLEEP 2
               ERROR ""
               CALL inicializa_reg_1()
               LET v_estado = null
            END IF
               CLEAR FORM
        EXIT DISPLAY
    END DISPLAY
    CLOSE WINDOW tram1002
RETURN v_estado
END FUNCTION
   ### hasta aqui rutina fuera por el momento 20 Abr 2010
   }   

FUNCTION f_desmarca_cuenta(vnss,vmarca_entra,dd_corr)
#fd---------------------------------------------------
define dd_corr integer
DEFINE ejecuta CHAR(300)
DEFINE vnss char(11),
vmarca_entra smallint ,
vmarca_estado smallint,
vcodigo_rechazo smallint,
vusuario   char(008)
DEFINE xcodigo_marca smallint,
xcodigo_rechazo smallint

select user
INTO vusuario
FROM tab_afore_local
group by 1

LET ejecuta = "EXECUTE PROCEDURE desmarca_cuenta(","'",vnss,"'",
						 ",",vmarca_entra,",",
						 dd_corr,",40,270,",
						 "'",vusuario,"')"
LET ejecuta = ejecuta CLIPPED
PREPARE clausula_spl2 FROM ejecuta
EXECUTE clausula_spl2


END FUNCTION

FUNCTION despliega_mens(vnss,cadena)
#di----------------------------------

DEFINE vnss CHAR(011)
DEFINE cadena CHAR(100)

  OPEN WINDOW tram4020 AT 12,20 WITH FORM "TRAM4020" ATTRIBUTE(BORDER)
    DISPLAY  cadena AT  4,3
    DISPLAY BY NAME vnss ATTRIBUTE(REVERSE)
    SLEEP 4
    CLOSE WINDOW tram4020
END FUNCTION

FUNCTION despliega_origen_traspaso()
#di------------------------

 DEFINE aux_val  SMALLINT
 
 DEFINE l_reg ARRAY[1000] OF RECORD
        origen_traspaso    SMALLINT,
        descripcion        CHAR(25)
 END RECORD
 
 DEFINE x_x                char(100),
        x_buscar           CHAR(30)
 DEFINE pos                SMALLINT

 OPEN WINDOW vent_1 AT 05,12 WITH FORM "TRAM4010" ATTRIBUTE(BORDER)
 DISPLAY "                ORIGEN TIPO TRASPASO                                           " AT 2,1 ATTRIBUTE(REVERSE)
 INPUT BY NAME x_buscar
            BEFORE FIELD x_buscar
                LET x_buscar = "*"

     AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
             ERROR "Tip.Traspaso a Buscar NO puede ser nulo"
             NEXT FIELD x_buscar
         ELSE
             EXIT INPUT
         END IF
 END INPUT

 WHILE TRUE
       LET x_x = " SELECT * FROM tra_tab_tiptra ",
                 " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
                 " ORDER BY 1 " CLIPPED

       PREPARE curg_cve20 FROM x_x
       DECLARE cur_cve20 CURSOR FOR curg_cve20
       LET pos = 1
       FOREACH cur_cve20 INTO l_reg[pos].*
        LET pos = pos + 1
        IF pos >= 1000 THEN
    ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
    EXIT FOREACH
        END IF
       END FOREACH
       IF (pos-1) < 1 THEN
          ERROR "ARCHIVO TIPO TRASPASO... VACIO"
       END IF
       CALL SET_COUNT(pos-1)
       DISPLAY ARRAY l_reg TO scr_1.*
               ON KEY ( INTERRUPT )
           LET pos = 0
           EXIT DISPLAY
        ON KEY ( CONTROL-M )
           LET pos = ARR_CURR()
           EXIT DISPLAY
       END DISPLAY
       IF pos <> 0 THEN
   EXIT WHILE
       END IF
 END WHILE
 CLOSE WINDOW vent_1
 RETURN l_reg[pos].origen_traspaso,l_reg[pos].descripcion
END FUNCTION

### Continua Input 
FUNCTION f_SigueInput()

  LET sw_2                  = 0
  LET reg_2.n_folio_tra     = NULL
  LET reg_2.icefa_cod       = NULL
  LET reg_2.des_icefa       = NULL
  LET reg_2.nro_int_cta     = NULL
  LET reg_2.cve_sector      = NULL
  LET reg_2.desc_sector     = NULL
  LET reg_2.origen_traspaso = NULL
  LET reg_2.descripcion     = NULL

    
  DISPLAY "                                                                          " AT 2,1
  DISPLAY reg_2.n_folio_tra      TO   n_folio_tra
  DISPLAY reg_2.icefa_cod        TO   icefa_cod
  DISPLAY reg_2.des_icefa        TO   des_icefa
  DISPLAY reg_2.nro_int_cta      TO   nro_int_cta
  DISPLAY reg_2.cve_sector       TO   cve_sector
  DISPLAY reg_2.desc_sector      TO   desc_sector
  DISPLAY reg_2.origen_traspaso  TO   origen_traspaso
  DISPLAY reg_2.descripcion      TO   descripcion

  INPUT BY NAME reg_2.n_folio_tra      ,
                reg_2.nss              ,
                reg_2.rfc              ,
                reg_2.paterno2         ,
                reg_2.materno2         ,
                reg_2.nombres2         ,
                reg_2.cve_sector       ,
                reg_2.desc_sector      ,
                reg_2.origen_traspaso  ,
                reg_2.descripcion      ,
                reg_2.icefa_cod        ,
                reg_2.des_icefa        ,
                reg_2.nro_int_cta      ,
                gr_val.id_procesar  WITHOUT DEFAULTS

      BEFORE FIELD n_folio_tra
          IF sw_2 = 0 THEN
              LET sw_2 = 1
              LET reg_2.nss                  = reg_1.n_seguro
              LET reg_2.rfc                  = reg_1.n_rfc
              LET reg_2.paterno2             = reg_1.paterno
              LET reg_2.materno2             = reg_1.materno
              LET reg_2.nombres2             = reg_1.nombres
              LET reg_2.fecha_captura        = HOY
              LET reg_2.fecha_proceso        = HOY
              LET reg_2.status               = 20
              LET reg_2.fuente               = 1
              LET reg_2.usuario              = c8_usuario

              DISPLAY reg_2.nss              TO nss
              DISPLAY reg_2.rfc              TO rfc
              DISPLAY reg_2.paterno2         TO paterno2
              DISPLAY reg_2.materno2         TO materno2
              DISPLAY reg_2.nombres2         TO nombres2
              DISPLAY reg_2.fecha_captura    TO fecha_captura
              DISPLAY reg_2.usuario          TO usuario
              DISPLAY reg_1.tipo_solicitud   TO tipo_solicitud2
          END IF

      BEFORE FIELD origen_traspaso
    
      
          DISPLAY BY NAME reg_2.origen_traspaso
          DISPLAY BY NAME reg_2.descripcion

      AFTER FIELD n_folio_tra

      AFTER FIELD paterno2
          IF reg_2.paterno2 IS NULL THEN
              ERROR " CAMPO NO PUEDE SER NULO "
              NEXT FIELD paterno2
          END IF

      AFTER FIELD nombres2
          IF reg_2.nombres2 IS NULL THEN
              ERROR " CAMPO NO PUEDE SER NULO "
              NEXT FIELD nombres2
          END IF

      AFTER FIELD cve_sector

          IF reg_2.cve_sector IS NULL THEN
              CALL despliega_sector() RETURNING reg_2.cve_sector ,
                                                reg_2.desc_sector
              IF (reg_2.cve_sector <> 1 AND
                  reg_2.cve_sector <> 2 ) THEN
                  NEXT FIELD cve_sector
              END IF
          ELSE
              SELECT  a.desc_sector
                INTO  reg_2.desc_sector
                FROM  tra_cve_sector a
               WHERE  a.cve_sector = reg_2.cve_sector

              IF STATUS = NOTFOUND THEN
                  ERROR " CVE INEXISTENTE "
                  NEXT FIELD cve_sector
              END IF
          END IF

          DISPLAY BY NAME reg_2.cve_sector
          DISPLAY BY NAME reg_2.desc_sector

      AFTER FIELD origen_traspaso

          IF reg_2.origen_traspaso IS NULL THEN
              CALL despliega_origen_traspaso() RETURNING reg_2.origen_traspaso,
                                                         reg_2.descripcion
              SELECT "OK"
                FROM tra_tab_tiptra a
               WHERE a.origen_traspaso = reg_2.origen_traspaso

              IF   STATUS = NOTFOUND   THEN
                   ERROR " TIPO TRASPASO INEXISTENTE "
                   LET reg_2.origen_traspaso   =  NULL
                   DISPLAY BY NAME reg_2.origen_traspaso
                   NEXT FIELD origen_traspaso
              END IF
          ELSE
              SELECT  a.descripcion
                INTO  reg_2.descripcion
                FROM  tra_tab_tiptra a
               WHERE  a.origen_traspaso  = reg_2.origen_traspaso

              IF   STATUS = NOTFOUND THEN
                   ERROR " TIPO TRASPASO INEXISTENTE "
                   LET reg_2.origen_traspaso       =       NULL
                   LET reg_2.descripcion           =       NULL
                   DISPLAY BY NAME reg_2.origen_traspaso
                   DISPLAY BY NAME reg_2.descripcion
                   NEXT FIELD origen_traspaso
              END IF
          END IF

          DISPLAY BY NAME reg_2.origen_traspaso
          DISPLAY BY NAME reg_2.descripcion

      AFTER FIELD icefa_cod

          IF reg_2.icefa_cod IS NULL THEN
              CALL despliega_icefas() RETURNING reg_2.icefa_cod ,
                                                reg_2.des_icefa
              IF reg_2.icefa_cod = 0 THEN
                  NEXT FIELD icefa_cod
              END IF
          ELSE
          	  INITIALIZE  reg_2.des_icefa TO NULL #JIRA CPL-1453 Diciembre 2013 Se Añade Clave Icefa 199
              SELECT icefa_desc                   #Cuentas Inactivas Canceladas Peis 
              INTO   reg_2.des_icefa
              FROM   tab_icefa
              WHERE  icefa_cod = reg_2.icefa_cod

              IF STATUS = NOTFOUND THEN
                  ERROR " ICEFA INEXISTENTE "
                  LET reg_2.icefa_cod   = NULL      
                  LET reg_2.des_icefa   = NULL      
                  DISPLAY  BY NAME  reg_2.icefa_cod 
                  DISPLAY  BY NAME  reg_2.des_icefa
                  NEXT FIELD icefa_cod
              ELSE 
              	 IF ( reg_2.icefa_cod <> '178' AND   
                      reg_2.icefa_cod <> '199' ) THEN
                      ERROR " ICEFA DEBE SER 178 ó 199 "
                      LET reg_2.icefa_cod   = NULL      
                      LET reg_2.des_icefa   = NULL      
                      DISPLAY  BY NAME  reg_2.icefa_cod 
                      DISPLAY  BY NAME  reg_2.des_icefa 
                      NEXT FIELD icefa_cod
              	 ELSE     
                     DISPLAY reg_2.icefa_cod TO icefa_cod   
                     DISPLAY reg_2.des_icefa TO des_icefa  
                 END IF
                
              END IF
              
          END IF

          DISPLAY reg_2.icefa_cod TO icefa_cod
          DISPLAY reg_2.des_icefa TO des_icefa     
                                   
          
      AFTER FIELD nro_int_cta
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
              NEXT FIELD icefa_cod
          END IF

      AFTER FIELD id_procesar
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
              NEXT FIELD nro_int_cta
          END IF
          IF  gr_val.id_procesar    IS NULL  THEN
              ERROR " id_procesar no puede estar vacio "
              NEXT FIELD id_procesar
          END IF

      ON KEY (INTERRUPT)
          INITIALIZE reg_2.* TO NULL
          INITIALIZE gr_val.* TO NULL
          DISPLAY reg_2.n_folio_tra      TO n_folio_tra
          DISPLAY reg_2.nss              TO nss
          DISPLAY reg_2.rfc              TO rfc
          DISPLAY reg_2.fecha_captura    TO fecha_captura
          DISPLAY reg_2.paterno2         TO paterno2
          DISPLAY reg_2.materno2         TO materno2
          DISPLAY reg_2.nombres2         TO nombres2
          DISPLAY reg_2.icefa_cod        TO icefa_cod
          DISPLAY reg_2.nro_int_cta      TO nro_int_cta
          DISPLAY reg_2.des_icefa        TO des_icefa
          DISPLAY reg_2.usuario          TO usuario
          DISPLAY reg_2.cve_sector       TO cve_sector
          DISPLAY reg_2.desc_sector      TO desc_sector
          DISPLAY reg_2.origen_traspaso  TO origen_traspaso
          DISPLAY reg_2.descripcion      TO descripcion
          EXIT INPUT
 
       
      

      ON KEY (ESC)
          
          IF reg_1.n_seguro[1,10]  <> reg_2.nss[1,10] THEN
              LET w = 1
          END IF
          IF reg_1.n_rfc[1,10]     <> reg_2.rfc[1,10] THEN
              LET y = 1
          END IF

          IF w = 1 THEN
              LET z = z + 1
          END IF
          IF y = 1 THEN
              LET z = z + 1
          END IF

          CASE z
               WHEN 1
                   IF w = 1 THEN
                      ERROR "Precaucion Diferencia en NSS..."
                   END IF
                   IF y = 1 THEN
                      ERROR "Precaucion Diferencia en RFC..."
                   END IF
                   EXIT CASE
               WHEN 2
                   ERROR "Diferencias en NSS y RFC..."
                   NEXT FIELD nss
                   EXIT CASE
          END CASE

          LET ba      = 0
          LET cad_nom = "DIFERENCIAS EN : "

          IF reg_2.paterno2 <> reg_1.paterno THEN
             LET ba = 1
             LET cad_nom = cad_nom CLIPPED," paterno"
          END IF

          IF reg_2.materno2 <> reg_1.materno THEN
             LET ba = 1
             LET cad_nom = cad_nom CLIPPED," ,materno"
          END IF

          IF reg_2.nombres2 <> reg_1.nombres THEN
              LET ba = 1
              LET cad_nom = cad_nom CLIPPED," ,nombre"
          END IF

          IF ba = 1 THEN
              DISPLAY cad_nom AT 22,2 ATTRIBUTE(REVERSE)
              SLEEP 2
          END IF

          PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
          IF enter MATCHES "[sS]" THEN
          ELSE
              NEXT FIELD n_folio_tra
          END IF

          DISPLAY "VERIFICANDO INFORMACION" AT 22,2 ATTRIBUTE(REVERSE)
          SLEEP 2
          DISPLAY "                       " AT 22,2

          IF reg_2.paterno2 IS NULL THEN
              ERROR " PATERNO NO puede ser espacio "
              NEXT FIELD paterno2
          END IF

          IF reg_2.nombres2 IS NULL THEN
              ERROR " NOMBRE NO puede ser espacio "
              NEXT FIELD nombres2
          END IF

          #JIRA CPL-1453 Diciembre 2013 Se Añade Clave Icefa 199
          #Cuentas Inactivas Canceladas Peis

          IF ( reg_2.icefa_cod IS NULL ) OR ( reg_2.icefa_cod <> 178 AND reg_2.icefa_cod <> 199 ) THEN    #OJO CAMBIO
              ERROR " CODIGO ICEFA NO puede ser espacio O dif. de 178 ó 199"
              NEXT FIELD icefa_cod
          END IF

          IF reg_2.fecha_captura IS NULL THEN
              ERROR " FECHA CAPTURA NO puede ser espacio "
              NEXT FIELD fecha_captura
          END IF
          
          IF reg_2.fecha_comp_icefa > HOY THEN
              ERROR " FECHA NO pueder mayor al dia actual "
              NEXT FIELD fecha_comp_icefa
          END IF

          ### 19 Abr 2010 -- revisar duplicidad en 5 campos 
          SELECT "OK"
          FROM   tra_mae_icefa_issste
          WHERE  n_seguro       = reg_1.n_seguro
          AND    nss            = reg_2.nss
          AND    rfc            = reg_2.rfc
          AND    icefa_cod      = reg_2.icefa_cod
          AND    id_procesar    = gr_val.id_procesar
          GROUP BY 1

          IF STATUS = NOTFOUND THEN
              LET enter = "S"
          ELSE
              ERROR "ICEFA Duplicada, Verifique " ATTRIBUTE(REVERSE)
              LET enter = "N"
              NEXT FIELD n_folio_tra
          END IF

          IF enter = "S" THEN
              IF reg_2.nss is null then
                 LET reg_2.nss = " "
              END IF

              IF reg_2.rfc is null then
                 LET reg_2.rfc = " "
              END IF

              IF reg_2.nro_int_cta is null then
                 LET reg_2.nro_int_cta = " "
              END IF

              LET i_corr = 0

              SELECT max(a.correlativo)
              INTO   i_corr
              FROM   safre_af:tra_mae_icefa_issste a

              IF i_corr < 1000000 THEN
                  LET i_corr = 1000000
              ELSE
                  LET i_corr = i_corr + 1
              END IF

              ### status en alta = 20  19 Abr 2010
              LET    reg_2.status           =          20 # Capturada
              INSERT INTO tra_mae_icefa_issste
                     VALUES(reg_1.n_folio          ,
                            reg_1.tipo_solicitud   ,
                            reg_1.n_seguro         ,
                            reg_2.nss              ,
                            reg_2.rfc              ,
                            reg_2.paterno2         ,
                            reg_2.materno2         ,
                            reg_2.nombres2         ,
                            reg_2.n_folio_tra      ,
                            reg_2.icefa_cod        ,
                            gr_val.id_procesar     ,
                            reg_2.nro_int_cta      ,
                            " ",                            #fecha_solic_tra
                            " ",                            #fecha_comp_icefa
                            0                      ,        #saldo_sar_92
                            0                      ,        #saldo_viv_92
                            reg_2.origen_traspaso  ,
                            reg_2.fecha_captura    ,
                            reg_2.fecha_proceso    ,
                            " "                    ,        #lote_genera
                            " "                    ,        #fecha_genera
                            reg_2.status           ,        #status
                            "5"                    ,        #fuente
                            i_corr                 ,        #correlativo
                            reg_2.usuario          ,
                            0                      ,        #n_envios
                            " "                    ,        #diagnostico
                            reg_2.cve_sector
                              )

               DISPLAY "ICEFA Insertada","" AT 22,2 ATTRIBUTE(REVERSE)
               SLEEP 3
               DISPLAY "               ","" AT 22,2
           ELSE
               DISPLAY "CAPTURA Abortada","" AT 22,2 ATTRIBUTE(REVERSE)
               SLEEP 3
               DISPLAY "                ","" AT 22,2
           END IF

           LET sw_2 = 0
           INITIALIZE reg_2.*  TO NULL
           INITIALIZE gr_val.* TO NULL
           DISPLAY reg_2.n_folio_tra      TO n_folio_tra
           DISPLAY reg_2.nss              TO nss
           DISPLAY reg_2.rfc              TO rfc
           DISPLAY reg_2.fecha_captura    TO fecha_captura
           DISPLAY reg_2.paterno2         TO paterno2
           DISPLAY reg_2.materno2         TO materno2
           DISPLAY reg_2.nombres2         TO nombres2
           DISPLAY reg_2.cve_sector       TO cve_sector
           DISPLAY reg_2.desc_sector      TO desc_sector
           DISPLAY reg_2.origen_traspaso  TO origen_traspaso
           DISPLAY reg_2.descripcion      TO descripcion
           DISPLAY reg_2.icefa_cod        TO icefa_cod
           DISPLAY reg_2.nro_int_cta      TO nro_int_cta
           DISPLAY reg_2.des_icefa        TO des_icefa
           DISPLAY reg_2.usuario          TO usuario
           EXIT INPUT 
  END INPUT
END FUNCTION

