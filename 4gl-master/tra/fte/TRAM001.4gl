##############################################################################
#Owner             => E.F.P.
#Programa TRAM001  => MANTENIMIENTO SOLICITUDES DE TRASPASOS DE ICE_AFO 
#Fecha creacion    => 23 ENERO DE 1998     
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 03 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af
GLOBALS
define icefa_ant     smallint
define nss_serv      CHAR(011)
define valida_modulo SMALLINT
define tot_saldo_viv like tra_mae_icefa.saldo_viv_92 
define tot_saldo_sar like tra_mae_icefa.saldo_sar_92 

define sal_saldo_sar like tra_mae_icefa.saldo_sar_92 
define sal_saldo_viv like tra_mae_icefa.saldo_sar_92 
define int_saldo_sar like tra_mae_icefa.saldo_sar_92 
define int_saldo_viv like tra_mae_icefa.saldo_sar_92 


define reg_saldo RECORD 
       folio              integer  ,
       n_seguro           char(011),
       cont_servicio      integer
end record

define reg_fusion_icefa record like safre_tmp:tra_fusion_icefa.*
define vt smallint
define vv_ident like dev_det_normal.ident_lote_solic
define rr smallint
define vv_folio integer
define reg_ap RECORD
       tipo_icefa like tra_det_trasp_sal.tipo_icefa,
       fech_mov_banxico like tra_det_trasp_sal.fech_mov_banxico,
       sal_int char(007),
       sar_92 like tra_det_trasp_sal.saldo_sar_92,
       viv_92 like tra_det_trasp_sal.saldo_viv_92
END RECORD

define reg_ap_dev RECORD 
       estatus         char(010),
       orig_tipo_trasp     like dev_det_normal.orig_tipo_trasp  ,
       acciones_sar_92     like dev_det_normal.acciones_sar_92  ,
       saldo_sar_92_banx   like dev_det_normal.saldo_sar_92_banx,
       saldo_sar_pagar      like dev_det_normal.saldo_sar_pagar  ,
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

define arr_reg_ap_dev ARRAY[50] OF RECORD 
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
       tipo_icefa like tra_det_trasp_sal.tipo_icefa            ,
       fech_mov_banxico like tra_det_trasp_sal.fech_mov_banxico,
       sal_int char(007)                                       ,
       sar_92 like tra_det_trasp_sal.saldo_sar_92              ,
       viv_92 like tra_det_trasp_sal.saldo_viv_92
END RECORD

define d_corr integer

define u smallint
DEFINE arr_cu ARRAY[50] OF RECORD 
       c1 smallint,
       c2 smallint
END RECORD
    DEFINE x smallint
    DEFINE y smallint
    DEFINE z smallint
    DEFINE ba smallint

    DEFINE gg smallint

    DEFINE fff smallint

    DEFINE cad_nom char(100)

    DEFINE actual_habil       SMALLINT

    DEFINE w smallint

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
           n_folio_tra           LIKE tra_mae_icefa.n_folio_tra      ,
           nss                   LIKE tra_mae_icefa.nss              ,
           rfc                   LIKE tra_mae_icefa.rfc              ,
           fecha_solic_tra       LIKE tra_mae_icefa.fecha_solic_tra  ,
           fecha_captura         LIKE tra_mae_icefa.fecha_captura    ,
           paterno2              LIKE tra_mae_icefa.paterno          ,
           materno2              LIKE tra_mae_icefa.materno          ,
           nombres2              LIKE tra_mae_icefa.nombres          ,
           icefa_cod             LIKE tra_mae_icefa.icefa_cod        ,
           des_icefa             CHAR(20)                            ,
           nro_int_cta           LIKE tra_mae_icefa.nro_int_cta      ,
           saldo_sar_92          LIKE tra_mae_icefa.saldo_sar_92     ,
           saldo_viv_92          LIKE tra_mae_icefa.saldo_viv_92     ,
           tipo_comp_icefa       LIKE tra_mae_icefa.tipo_comp_icefa  ,
           tipcom_desc           char(20),
           fecha_comp_icefa      LIKE tra_mae_icefa.fecha_comp_icefa ,
           fuente                LIKE tra_mae_icefa.fuente           ,
           fecha_proceso         LIKE tra_mae_icefa.fecha_proceso    ,
           status                LIKE tra_mae_icefa.status           ,
           des_estado            CHAR(20)                            ,
           fecha_a_liquidar      DATE                                ,
           fecha_liquidacion     DATE                                ,
           correlativo           LIKE tra_mae_icefa.correlativo      ,
           usuario               LIKE tra_mae_icefa.usuario          ,
           diag_proceso          CHAR(3)                             ,
           des_diagnostico       CHAR(30)
       END RECORD

    DEFINE reg_2_m RECORD #glo #reg_2
           n_folio_tra           LIKE tra_mae_icefa.n_folio_tra      ,
           nss                   LIKE tra_mae_icefa.nss              ,
           rfc                   LIKE tra_mae_icefa.rfc              ,
           fecha_solic_tra       LIKE tra_mae_icefa.fecha_solic_tra  ,
           fecha_captura         LIKE tra_mae_icefa.fecha_captura    ,
           paterno2              LIKE tra_mae_icefa.paterno          ,
           materno2              LIKE tra_mae_icefa.materno          ,
           nombres2              LIKE tra_mae_icefa.nombres          ,
           icefa_cod             LIKE tra_mae_icefa.icefa_cod        ,
           des_icefa             CHAR(20)                            ,
           nro_int_cta           LIKE tra_mae_icefa.nro_int_cta      ,
           saldo_sar_92          LIKE tra_mae_icefa.saldo_sar_92     ,
           saldo_viv_92          LIKE tra_mae_icefa.saldo_viv_92     ,
           fuente                LIKE tra_mae_icefa.fuente           ,
           usuario               LIKE tra_mae_icefa.usuario          ,
           tipo_comp_icefa       LIKE tra_mae_icefa.tipo_comp_icefa  ,
           tipcom_desc           char(20),
           fecha_genera          LIKE tra_mae_icefa.fecha_genera   ,
           fecha_proceso         LIKE tra_mae_icefa.fecha_proceso    ,
           fecha_liquidacion     DATE                                ,
           status                LIKE tra_mae_icefa.status           ,
           des_estado            CHAR(20)                            ,
           correlativo           LIKE tra_mae_icefa.correlativo      ,
           diag_proceso          CHAR(3)                             ,
           des_diagnostico       CHAR(30)
       END RECORD

    DEFINE arr_1  ARRAY[100] OF RECORD #glo #arr_1
        n_folio_tra           LIKE tra_mae_icefa.n_folio_tra      ,
        nss                   LIKE tra_mae_icefa.nss              ,
        rfc                   LIKE tra_mae_icefa.rfc              ,
        fecha_solic_tra       LIKE tra_mae_icefa.fecha_solic_tra  ,
        fecha_captura         LIKE tra_mae_icefa.fecha_captura    ,
        fecha_comp_icefa      LIKE tra_mae_icefa.fecha_comp_icefa,
        paterno2              LIKE tra_mae_icefa.paterno          ,
        materno2              LIKE tra_mae_icefa.materno          ,
        nombres2              LIKE tra_mae_icefa.nombres          ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod        ,
        des_icefa             CHAR(20)                            ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta      ,
        saldo_sar_92          LIKE tra_mae_icefa.saldo_sar_92     ,
        saldo_viv_92          LIKE tra_mae_icefa.saldo_viv_92     ,
        fuente                LIKE tra_mae_icefa.fuente           ,
        usuario               LIKE tra_mae_icefa.usuario          ,
        tipo_comp_icefa       LIKE tra_mae_icefa.tipo_comp_icefa  ,
        tipcom_desc         char(20),
        fecha_genera          DATE         ,  
        fecha_proceso         DATE         ,
        fecha_liquidacion     DATE         ,
        estado                LIKE tra_mae_icefa.status           ,
        des_estado            CHAR(20)                            ,
        correlativo           LIKE tra_mae_icefa.correlativo      ,
        diag_proceso          CHAR(3)                             ,
        des_diagnostico       CHAR(30)
    END RECORD

    DEFINE arr_1_c  ARRAY[100] OF RECORD #glo #arr_1
	c1                    SMALLINT,
	c2                    SMALLINT,
        n_folio_tra           LIKE tra_mae_icefa.n_folio_tra      ,
        nss                   LIKE tra_mae_icefa.nss              ,
        rfc                   LIKE tra_mae_icefa.rfc              ,
        fecha_solic_tra       LIKE tra_mae_icefa.fecha_solic_tra  ,
        fecha_captura         LIKE tra_mae_icefa.fecha_captura    ,
        fecha_comp_icefa      LIKE tra_mae_icefa.fecha_comp_icefa ,
        paterno2              LIKE tra_mae_icefa.paterno          ,
        materno2              LIKE tra_mae_icefa.materno          ,
        nombres2              LIKE tra_mae_icefa.nombres          ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod        ,
        des_icefa             CHAR(20)                            ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta      ,
        saldo_sar_92          LIKE tra_mae_icefa.saldo_sar_92     ,
        saldo_viv_92          LIKE tra_mae_icefa.saldo_viv_92     ,
        fuente                LIKE tra_mae_icefa.fuente           ,
        usuario               LIKE tra_mae_icefa.usuario          ,
        tipo_comp_icefa       LIKE tra_mae_icefa.tipo_comp_icefa  ,
        tipcom_desc           char(20),
        fecha_genera          DATE         ,  
        fecha_proceso         DATE         ,
        fecha_liquidacion     DATE         ,
        estado                LIKE tra_mae_icefa.status           ,
        des_estado            CHAR(20)                            ,
        correlativo           LIKE tra_mae_icefa.correlativo      ,
        diag_proceso          CHAR(3)                             ,
        des_diagnostico       CHAR(30)
    END RECORD

    DEFINE arr_m  ARRAY[100] OF RECORD #arr_m
        n_folio_tra           LIKE tra_mae_icefa.n_folio_tra      ,
        nss                   LIKE tra_mae_icefa.nss              ,
        rfc                   LIKE tra_mae_icefa.rfc              ,
        fecha_solic_tra       LIKE tra_mae_icefa.fecha_solic_tra  ,
        fecha_captura         LIKE tra_mae_icefa.fecha_captura    ,
        fecha_comp_icefa        LIKE tra_mae_icefa.fecha_comp_icefa,
        paterno2              LIKE tra_mae_icefa.paterno          ,
        materno2              LIKE tra_mae_icefa.materno          ,
        nombres2              LIKE tra_mae_icefa.nombres          ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod        ,
        des_icefa             CHAR(20)                       ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta      ,
        saldo_sar_92          LIKE tra_mae_icefa.saldo_sar_92     ,
        saldo_viv_92          LIKE tra_mae_icefa.saldo_viv_92     ,
        fuente                LIKE tra_mae_icefa.fuente          ,
        usuario               LIKE tra_mae_icefa.usuario         ,
        tipo_comp_icefa       LIKE tra_mae_icefa.tipo_comp_icefa  ,
        tipcom_desc           char(20),
        fecha_genera          LIKE tra_mae_icefa.fecha_genera   ,
        fecha_proceso         LIKE tra_mae_icefa.fecha_proceso    ,
        fecha_liquidacion     DATE                           ,
        estado                LIKE tra_mae_icefa.status           ,
        des_estado            CHAR(20)                       ,
        correlativo           LIKE tra_mae_icefa.correlativo      ,
        diag_proceso          CHAR(3)                        ,
        des_diagnostico       CHAR(30)
    END RECORD

    DEFINE arr_m_c  ARRAY[100] OF RECORD #arr_m
        c1                    SMALLINT, 
        c2                    SMALLINT, 
        n_folio_tra           LIKE tra_mae_icefa.n_folio_tra      ,
        nss                   LIKE tra_mae_icefa.nss              ,
        rfc                   LIKE tra_mae_icefa.rfc              ,
        fecha_solic_tra       LIKE tra_mae_icefa.fecha_solic_tra  ,
        fecha_captura         LIKE tra_mae_icefa.fecha_captura    ,
        fecha_comp_icefa        LIKE tra_mae_icefa.fecha_comp_icefa,
        paterno2              LIKE tra_mae_icefa.paterno          ,
        materno2              LIKE tra_mae_icefa.materno          ,
        nombres2              LIKE tra_mae_icefa.nombres          ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod        ,
        des_icefa             CHAR(20)                       ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta      ,
        saldo_sar_92          LIKE tra_mae_icefa.saldo_sar_92     ,
        saldo_viv_92          LIKE tra_mae_icefa.saldo_viv_92     ,
        fuente                LIKE tra_mae_icefa.fuente          ,
        usuario               LIKE tra_mae_icefa.usuario         ,
        tipo_comp_icefa       LIKE tra_mae_icefa.tipo_comp_icefa  ,
        tipcom_desc        char(20), 
        fecha_genera          LIKE tra_mae_icefa.fecha_genera   ,
        fecha_proceso         LIKE tra_mae_icefa.fecha_proceso    ,
        fecha_liquidacion     DATE                           ,
        estado                LIKE tra_mae_icefa.status           ,
        des_estado            CHAR(20)                       ,
        correlativo           LIKE tra_mae_icefa.correlativo      ,
        diag_proceso          CHAR(3)                        ,
        des_diagnostico       CHAR(30)
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
 i            SMALLINT

    DEFINE arr_100 ARRAY[1000] OF RECORD #arr_100
        folio2                LIKE tra_det_rechazo.folio       ,
        n_seguro              LIKE tra_mae_icefa.nss             ,
        cve_ced_cuenta        LIKE tra_mae_icefa.icefa_cod       ,
        nro_ctrl_icefa        LIKE tra_mae_icefa.nro_int_cta     ,
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
        n_seguro              LIKE tra_mae_icefa.nss             ,
        cve_ced_cuenta        LIKE tra_mae_icefa.icefa_cod       ,
        nro_ctrl_icefa        LIKE tra_mae_icefa.nro_int_cta     ,
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
 e_correlativo         LIKE tra_mae_icefa.correlativo

END GLOBALS
MAIN
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
  ACCEPT KEY CONTROL-I
  DEFER INTERRUPT

####### quitar

######

    CALL init()

    OPEN WINDOW ventana_1 AT 2,2 WITH 21 ROWS,78 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " TRAM001       MANTENIMIENTO SOLICITUDES DE TRA-ICE-AFO-IMSS                   " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    #CALL inicializa()


LET valida_modulo = 0
LET valida_modulo = ARG_VAL(1)
LET nss_serv      = ARG_VAL(2)

IF valida_modulo = 1 THEN

    CALL consulta()
    EXIT PROGRAM

END IF


    MENU "ICEFAS"
    COMMAND "Agrega" "Agrega Icefas"
       CALL agrega() #a
       #CALL inicializa()
    COMMAND "Consulta" "Consulta Icefas "
       CALL consulta() #c
       #CALL inicializa()

  COMMAND "Modifica" "Modifica Icefas"
       CALL seleccion() #s
       #CALL inicializa() #i

    COMMAND "Busqueda" "Busca afiliado"
       CALL ayuda() RETURNING reg_1.n_folio        ,
                              reg_1.tipo_solicitud

  COMMAND "Rechazos" "Permite consultar Icefas rechazadas"
           CALL trac011()

  COMMAND "Traspasos Indebidos" "Permite identificar Traspasos Indebidos"
           CALL busca_indebidos() 
                                 
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

WHENEVER ERROR CONTINUE

SELECT "OK" 
   FROM systables
WHERE tabname = "view_saldos"
IF (STATUS = NOTFOUND) THEN
   CREATE TEMP TABLE view_saldos(subcuenta smallint,monto_en_pesos decimal(16,2))
ELSE
   DROP TABLE  view_saldos
END IF
END FUNCTION

FUNCTION inicializa()
#i-------------------
    INITIALIZE reg_1 TO NULL
    INITIALIZE arr_1 TO NULL
    INITIALIZE arr_m TO NULL
 LET  x = 0
    CLEAR FORM
END FUNCTION

FUNCTION inicializa_reg_1()
#ir------------------------
    INITIALIZE reg_1.* TO NULL
    DISPLAY BY NAME reg_1.*
END FUNCTION

FUNCTION agrega()
#a---------------

    OPEN WINDOW tram0011 AT 2,2 WITH FORM "TRAM0011" ATTRIBUTE( BORDER)
    DISPLAY " TRAM001             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY "                            INGRESO DE ICEFAS                                     " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Esc ] Ingreso        [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " [ Ctrl-b ] Pantalla de ingreso de ICEFAS " AT 2,1
    DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY "                            INGRESO DE ICEFAS                              " AT 8,1 ATTRIBUTE(REVERSE)

    LET folio_reg = FALSE

    INPUT BY NAME reg_1.n_folio        , 
                  reg_1.tipo_solicitud ,
                  reg_1.n_seguro       ,
                  reg_1.n_rfc          ,
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

            IF  reg_1.tipo_solicitud > 5 THEN
                ERROR"TIPO SOLICITUD INVALIDO"
                NEXT FIELD tipo_solicitud
            END IF

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

            IF STATUS = NOTFOUND THEN
                ERROR "FOLIO NO EXISTE EN afi_mae_afiliado....NO ES UN AFILIADO ",
                      "DE LA AFORE"
                NEXT FIELD n_folio
            ELSE 
               LET folio_reg = TRUE
            END IF
            DISPLAY BY NAME reg_1.*
	    IF reg_1.tipo_solicitud = 5 THEN
           CALL despliega_mens(reg_1.n_seguro,"Nss asignado aun no vigente  :")
	   NEXT FIELD n_seguro
	   END IF
        AFTER FIELD n_seguro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_folio
            END IF

            IF reg_1.n_seguro IS NULL THEN
                NEXT FIELD n_rfc
            END IF

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
            WHERE  n_seguro = reg_1.n_seguro

            IF STATUS = NOTFOUND THEN
                ERROR " NSS INEXISTENTE "
                NEXT FIELD n_seguro
            ELSE
                 LET folio_reg = TRUE
            END IF
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
	    IF reg_1.tipo_solicitud = 5 THEN
           CALL despliega_mens(reg_1.n_seguro,"Nss asignado aun no vigente  :")
	   NEXT FIELD n_seguro
	   END IF
        AFTER FIELD n_rfc
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_seguro
            END IF

            IF reg_1.n_rfc IS NULL THEN
                NEXT FIELD n_unico
            END IF
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
            WHERE  n_rfc = reg_1.n_rfc

            IF STATUS = NOTFOUND THEN
                ERROR " RFC INEXISTENTE "
                NEXT FIELD n_rfc
            ELSE
                  LET folio_reg = TRUE
            END IF 
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
	    IF reg_1.tipo_solicitud = 5 THEN
           CALL despliega_mens(reg_1.n_seguro,"Nss asignado aun no vigente  :")
	   NEXT FIELD n_seguro
	   END IF

        AFTER FIELD n_unico
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_rfc
            END IF

            IF reg_1.n_unico IS NULL THEN
                NEXT FIELD n_folio
            END IF

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
            WHERE  n_unico = reg_1.n_unico

            IF STATUS = NOTFOUND THEN
                ERROR " CURP INEXISTENTE "
                NEXT FIELD n_unico
            ELSE
                LET folio_reg = TRUE
            END IF
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
	    IF reg_1.tipo_solicitud = 5 THEN
           CALL despliega_mens(reg_1.n_seguro,"Nss asignado aun no vigente  :")
	   NEXT FIELD n_seguro
	   END IF


        ON KEY (CONTROL-B)
	    IF reg_1.tipo_solicitud = 5 THEN
           CALL despliega_mens(reg_1.n_seguro,"Nss asignado aun no vigente  :")
	   NEXT FIELD n_seguro
	   END IF
            LET sw_2 = 0
            LET reg_2.n_folio_tra     = NULL
            LET reg_2.icefa_cod       = NULL
            LET reg_2.des_icefa       = NULL
            LET reg_2.nro_int_cta     = NULL
            LET reg_2.tipo_comp_icefa = NULL

            IF folio_reg = FALSE
            THEN 
              ERROR "AFILIADO INEXISTENTE ..."
              LET folio_reg    = FALSE
              LET n_seguro_reg = FALSE
              LET rfc_reg      = FALSE
              NEXT FIELD n_folio
            END IF

            DISPLAY reg_2.n_folio_tra TO n_folio_tra
            DISPLAY reg_2.icefa_cod   TO icefa_cod
            DISPLAY reg_2.des_icefa   TO des_icefa
            DISPLAY reg_2.nro_int_cta TO nro_int_cta
          
            INPUT BY NAME reg_2.n_folio_tra      ,
                          reg_2.nss              ,
                          reg_2.rfc              ,
                          reg_2.paterno2         ,
                          reg_2.materno2         ,
                          reg_2.nombres2         ,
                          reg_2.icefa_cod        ,
                          reg_2.des_icefa        ,
                          reg_2.nro_int_cta      ,
                          reg_2.tipo_comp_icefa   WITHOUT DEFAULTS

                BEFORE FIELD n_folio_tra
                    IF sw_2 = 0 THEN
                        LET sw_2 = 1
                        LET reg_2.nss              = reg_1.n_seguro
                        LET reg_2.rfc              = reg_1.n_rfc
                        LET reg_2.paterno2         = reg_1.paterno
                        LET reg_2.materno2         = reg_1.materno
                        LET reg_2.nombres2         = reg_1.nombres
                        LET reg_2.fecha_captura    = HOY
                        LET reg_2.fecha_proceso    = HOY
                        LET reg_2.status           = 20
                        LET reg_2.fuente           = 1
                        LET reg_2.usuario          = c8_usuario

                        DISPLAY reg_2.nss              TO nss
                        DISPLAY reg_2.rfc              TO rfc
                        DISPLAY reg_2.paterno2         TO paterno2
                        DISPLAY reg_2.materno2         TO materno2
                        DISPLAY reg_2.nombres2         TO nombres2
                        DISPLAY reg_2.fecha_captura    TO fecha_captura
                        DISPLAY reg_2.usuario          TO usuario

                    END IF

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

         AFTER FIELD icefa_cod

                    LET icefa_ant = reg_2.icefa_cod

                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD nombres
                    END IF

                 IF reg_2.icefa_cod IS NULL THEN
                    CALL despliega_icefas() RETURNING reg_2.icefa_cod ,
                                                      reg_2.des_icefa
                    LET icefa_ant = reg_2.icefa_cod
                   IF reg_2.icefa_cod = 0 THEN 
                      NEXT FIELD icefa_cod 
                   END IF
                 ELSE
                    LET icefa_ant = reg_2.icefa_cod
                   SELECT icefa_desc
                   INTO   reg_2.des_icefa
                   FROM   tab_icefa
                   WHERE  icefa_cod = reg_2.icefa_cod

                   IF STATUS = NOTFOUND THEN
                      ERROR " ICEFA INEXISTENTE "
                      NEXT FIELD icefa_cod
                  END IF
                END IF
                LET icefa_ant = reg_2.icefa_cod
                DISPLAY reg_2.icefa_cod TO icefa_cod
                DISPLAY reg_2.des_icefa TO des_icefa

                SELECT a.* 
		INTO  reg_fusion_icefa.*
		FROM  safre_tmp:tra_fusion_icefa a
		WHERE a.cve_ced_cuenta_org = reg_2.icefa_cod

                IF reg_fusion_icefa.cve_ced_cuenta_act <>
		   reg_2.icefa_cod THEN
		   ERROR"ICEFA FUSIONADA...",reg_2.icefa_cod," ",
			   reg_2.des_icefa
		   SLEEP 2

                   SELECT icefa_desc
                   INTO   reg_2.des_icefa
                   FROM   tab_icefa
                   WHERE  icefa_cod = reg_fusion_icefa.cve_ced_cuenta_act

                   LET reg_2.icefa_cod = reg_fusion_icefa.cve_ced_cuenta_act

		 END IF
                DISPLAY reg_2.icefa_cod TO icefa_cod
                DISPLAY reg_2.des_icefa TO des_icefa

         AFTER FIELD nro_int_cta
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD icefa_cod
                    END IF
		    IF reg_2.nro_int_cta IS NULL THEN
		      LET reg_2.nro_int_cta = 30 SPACES
		      END IF
		    CALL formatea(reg_2.nro_int_cta,icefa_ant)
		    RETURNING reg_2.nro_int_cta

		    DISPLAY reg_2.nro_int_cta TO nro_int_cta

         AFTER FIELD tipo_comp_icefa
                    IF reg_2.tipo_comp_icefa IS NULL THEN
                        CALL despliega_tipo_comp_icefas() #dtci
                        RETURNING reg_2.tipo_comp_icefa,
                                  reg_2.tipcom_desc
                        DISPLAY reg_2.tipcom_desc TO tipcom_desc
                    ELSE
                        SELECT tipcom_desc
                        INTO   reg_2.tipcom_desc
                        FROM   tab_tipo_com
                        WHERE  tipcom_cod = reg_2.tipo_comp_icefa

                        IF STATUS = NOTFOUND THEN
                            ERROR "TIPO DE COMPROBANTE INEXISTENTE"
                            NEXT FIELD tipo_comp_icefa
                        END IF
                        DISPLAY reg_2.tipcom_desc TO tipcom_desc
                    END IF

                ON KEY (INTERRUPT)
                    INITIALIZE reg_2.* TO NULL
                    DISPLAY reg_2.n_folio_tra      TO n_folio_tra
                    DISPLAY reg_2.nss              TO nss
                    DISPLAY reg_2.rfc              TO rfc
                    DISPLAY reg_2.fecha_captura    TO fecha_captura
                    DISPLAY reg_2.paterno2         TO paterno2
                    DISPLAY reg_2.materno2         TO materno2
                    DISPLAY reg_2.nombres2         TO nombres2
                    DISPLAY reg_2.icefa_cod        TO icefa_cod
                    DISPLAY reg_2.nro_int_cta      TO nro_int_cta
                    DISPLAY reg_2.tipo_comp_icefa  TO tipo_comp_icefa
                    DISPLAY reg_2.des_icefa        TO des_icefa
                    DISPLAY reg_2.usuario          TO usuario
                    EXIT INPUT

                ON KEY (ESC)
		LET w = 0
		LET y = 0
		LET z = 0

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
		    ERROR"Precaucion Diferencia en nss..."
		  END IF
		  IF y = 1 THEN
		    ERROR"Precaucion Diferencia en rfc..."
		  END IF
                  EXIT CASE
                WHEN 2
		    ERROR"Diferencias en nss y rfc..."      
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
                     DISPLAY cad_nom AT 19,2 ATTRIBUTE(REVERSE)
                     SLEEP 2
                 END IF
                 WHILE TRUE
                   PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
                   IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                       EXIT WHILE
                    ELSE
                     DISPLAY "                                                                                  " at 19,1 
                     DISPLAY"ALTA CANCELADA" AT 19,2 ATTRIBUTE(REVERSE) SLEEP 2
                     SLEEP 2
                     DISPLAY "                                                                                  " at 19,1 
                     NEXT FIELD n_folio_tra
                    END IF
                   END IF
                 END WHILE
                 DISPLAY "VERIFICANDO INFORMACION" AT 19,2 ATTRIBUTE(REVERSE)
                    SLEEP 2
                    DISPLAY "                       " AT 19,2 

                    IF reg_2.paterno2 IS NULL THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD paterno2
                    END IF

                    IF reg_2.nombres2 IS NULL THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD nombres2
                    END IF

                    IF reg_2.icefa_cod IS NULL THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD icefa_cod
                    END IF

                    IF reg_2.fecha_captura IS NULL THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD fecha_captura
                    END IF

                    IF reg_2.tipo_comp_icefa IS NULL THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD tipo_comp_icefa
                    END IF

                    IF reg_2.fecha_comp_icefa > HOY THEN
                        ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                        NEXT FIELD fecha_comp_icefa
                    END IF

                    SELECT "OK"
                    FROM   tra_mae_icefa
                    WHERE  n_seguro       = reg_1.n_seguro  
                    AND    nss            = reg_2.nss
                    AND    rfc            = reg_2.rfc
                    AND    icefa_cod      = reg_2.icefa_cod
                    AND    nro_int_cta    = reg_2.nro_int_cta 
                    AND    tipo_solicitud = reg_1.tipo_solicitud
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        LET enter = "S"
                    ELSE
                        ERROR "ICEFA DUPLICADA" ATTRIBUTE(REVERSE)
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

                        INSERT INTO tra_mae_icefa
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
                                      reg_2.nro_int_cta      ,
                                      " ",
                                      reg_2.tipo_comp_icefa  ,
                                      " ",
                                      0                     ,#saldo_sar_92
                                      0                     ,#saldo_viv_92
                                      reg_2.fecha_captura    ,
                                      reg_2.fecha_proceso    ,
                                      " "                     ,#lote_genera
                                      " "                     ,#fecha_genera
                                      20                     ,#status
                                      "1"                    ,
                                      0                      ,#correlativo
                                      reg_2.usuario          ,
                                      " "                     ,#n_envios
                                      " "                      #diagnostico
                                     )
                        DISPLAY "ICEFA INSERTADA","" AT 19,2 ATTRIBUTE(REVERSE)
                        SLEEP 3  
                        DISPLAY "               ","" AT 19,2 
                    ELSE
                        DISPLAY "CAPTURA ABORTADA","" AT 19,2 ATTRIBUTE(REVERSE)
                        SLEEP 3  
                        DISPLAY "                ","" AT 19,2 
                    END IF

                    LET sw_2 = 0
                    INITIALIZE reg_2.*  TO NULL
                    DISPLAY reg_2.n_folio_tra      TO n_folio_tra
                    DISPLAY reg_2.nss              TO nss
                    DISPLAY reg_2.rfc              TO rfc
                    DISPLAY reg_2.fecha_captura    TO fecha_captura
                    DISPLAY reg_2.paterno2         TO paterno2
                    DISPLAY reg_2.materno2         TO materno2
                    DISPLAY reg_2.nombres2         TO nombres2
                    DISPLAY reg_2.icefa_cod        TO icefa_cod
                    DISPLAY reg_2.nro_int_cta      TO nro_int_cta
                    DISPLAY reg_2.tipo_comp_icefa  TO tipo_comp_icefa
                    DISPLAY reg_2.tipcom_desc      TO tipcom_desc
                    DISPLAY reg_2.des_icefa        TO des_icefa
                    DISPLAY reg_2.usuario          TO usuario
                    NEXT FIELD n_folio_tra
            END INPUT
     NEXT FIELD n_folio
  END INPUT
CLOSE WINDOW tram0011
END FUNCTION

FUNCTION consulta()
#c-----------------

    DEFINE #loc #date
        d_fecha_genera        DATE

    DEFINE #loc #char
        c11_n_seguro          CHAR(11),
        c_des_estado          char(035)

    DEFINE #loc #smallint
        s_lote_genera         SMALLINT ,
        i                     SMALLINT

    DEFINE #loc #integer
        i_correlativo         INTEGER


    OPEN WINDOW tram0012 AT 2,2 WITH FORM "TRAM0012" ATTRIBUTE( BORDER)
    DISPLAY " TRAM001             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
  IF valida_modulo = 1 THEN
    DISPLAY " [Ctrl-c]Salir  [F2]Aportes  [F3]Devoluciones  "
            AT 1,1 ATTRIBUTE(BOLD)
  ELSE 
    DISPLAY " [Ctrl-c]Salir  [F2]Aportes  [F3]Devoluciones  [Ctrl-p]Eliminar"
            AT 1,1 ATTRIBUTE(BOLD)
  END IF
    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_1 = 0

    WHILE TRUE

WHENEVER ERROR CONTINUE
DROP TABLE ver_consulta
WHENEVER ERROR STOP

CREATE TEMP TABLE ver_consulta
(
        n_seguro char(011),
        n_folio_tra           decimal(8),
        nss                  char(11),
        rfc                  char(13),
        fecha_solic_tra       date,
        fecha_captura         date,
        fecha_comp_icefa      date,
        paterno2              char(40),
        materno2              char(40),
        nombres2              char(40),
        icefa_cod             smallint,
        des_icefa             CHAR(20) ,             
        nro_int_cta           char(30),
        saldo_sar_92          decimal(16,2),
        saldo_viv_92          decimal(16,2),
        fuente                smallint,
        usuario               char(8),
        tipo_comp_icefa       smallint,
        tipcom_desc           char(020),
        fecha_genera          DATE         ,  
        fecha_proceso         DATE         ,
        fecha_liquidacion     DATE         ,
        estado                smallint,
        des_estado            CHAR(20)                ,
        correlativo           integer,
        diag_proceso          CHAR(3)            ,
        des_diagnostico       CHAR(30),
	fecha_genera1          DATE ,
	lote_genera           smallint)


    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    IF valida_modulo = 1 THEN

       LET reg_1.n_seguro = nss_serv

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
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_1.n_seguro

            IF STATUS = NOTFOUND THEN
                ERROR " NSS INEXISTENTE "
		SLEEP 5   
                EXIT PROGRAM
            END IF

            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
    ELSE 

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

            IF  reg_1.tipo_solicitud > 5 THEN  
                ERROR"TIPO SOLICITUD INVALIDO"
                NEXT FIELD tipo_solicitud
            END IF

            SELECT n_folio        ,
                   tipo_solicitud ,
                   n_seguro       ,
                   n_rfc          ,
                   n_unico        ,
                   fentcons      ,
                   paterno        ,
                   materno        ,
                   nombres
            INTO   reg_1.*
            FROM   afi_mae_afiliado
            WHERE  n_folio        = reg_1.n_folio
            AND    tipo_solicitud = reg_1.tipo_solicitud

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
            FROM   afi_mae_afiliado
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
            FROM   afi_mae_afiliado
            WHERE  n_rfc = reg_1.n_rfc

            IF STATUS = NOTFOUND THEN
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
           A.tipo_comp_icefa  ,
           " "                 ,#tipcom_desc
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
    FROM   tra_mae_icefa A, tab_icefa B
    WHERE  A.n_seguro       = reg_1.n_seguro
    AND    A.tipo_solicitud = reg_1.tipo_solicitud
    AND    A.icefa_cod      = B.icefa_cod

    INSERT INTO ver_consulta
    SELECT A.n_seguro         ,
           " "                ,
           A.n_seguro_ent     ,
           A.rfc_ent          ,
           A.fech_presentacion,
           " " ,
           " ",
           A.paterno          ,
           A.materno          ,
           A.nombre           ,
           A.cve_ced_cuenta   ,
           B.icefa_desc       ,
           A.nro_ctrl_icefa   ,
           A.saldo_sar_92     ,
           A.saldo_viv_92     ,
           3                  ,
           " ",
           " ",
           " "                 ,#tipcom_desc
           " ",
           A.fech_presentacion,
           " "                 ,#fecha_liquidacion
           A.estado            ,#estado
           " "                 ,#des_estado
           " ",
           " "                 ,#diag_proceso
           " "                 ,#des_diagnostico
           " ",
           " "
    FROM   safre_tmp:tra_det_trasp_sal A, tab_icefa B
    WHERE  A.n_seguro       = reg_1.n_seguro
    AND    A.cve_ced_cuenta = B.icefa_cod

    DECLARE cur_1 CURSOR FOR 
    SELECT * from ver_consulta

    SELECT count(*)
    INTO   cuantos
    FROM   tra_mae_icefa
    WHERE  tra_mae_icefa.n_seguro       = reg_1.n_seguro
    AND    tra_mae_icefa.tipo_solicitud = reg_1.tipo_solicitud


    DISPLAY "    Icefa: "  AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "/"  AT 8,14 ATTRIBUTE(REVERSE)

    LET i = 1

    FOREACH cur_1 INTO c11_n_seguro,arr_1[i].*,d_fecha_genera,s_lote_genera

        LET arr_cu[i].c1 = i
        LET arr_cu[i].c2 = cuantos

        SELECT tipcom_desc
        INTO   arr_1[i].tipcom_desc
        FROM   tab_tipo_com
        WHERE  tipcom_cod = arr_1[i].tipo_comp_icefa


        SELECT des_estado
        INTO   arr_1[i].des_estado
        FROM   tra_status
        WHERE  estado = arr_1[i].estado 

        IF arr_1[i].estado = "3" THEN
            CALL codigo_rechazo(reg_1.n_seguro   ,
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
        END IF
        IF ( arr_1[i].estado = 8 OR
	     arr_1[i].estado = 15 OR
	     arr_1[i].estado = 16 OR
	     arr_1[i].estado = 17  OR 
	     arr_1[i].estado = 10) THEN

           IF arr_1[i].estado = 10 THEN

            SELECT max(A.fech_mov_banxico)j
            INTO   arr_1[i].fecha_liquidacion
            FROM   tra_det_trasp_sal A
            WHERE  A.n_seguro       = c11_n_seguro
            AND    A.n_seguro_ent   = arr_1[i].nss
            AND    A.rfc_ent        = arr_1[i].rfc
            AND    A.cve_ced_cuenta = arr_1[i].icefa_cod
            AND    A.nro_ctrl_icefa = arr_1[i].nro_int_cta
            AND    A.tipo_icefa     = "C"    

           ELSE 

            SELECT max(A.fech_mov_banxico)j
            INTO   arr_1[i].fecha_liquidacion
            FROM   tra_det_trasp_sal A
            WHERE  A.n_seguro       = c11_n_seguro
            AND    A.n_seguro_ent   = arr_1[i].nss
            AND    A.rfc_ent        = arr_1[i].rfc
            AND    A.cve_ced_cuenta = arr_1[i].icefa_cod
            AND    A.nro_ctrl_icefa = arr_1[i].nro_int_cta
            AND    A.tipo_icefa     = "N"    
           END IF




            CALL habil_siguiente(arr_1[i].fecha_liquidacion)
            RETURNING arr_1[i].fecha_liquidacion

        END IF

        IF arr_1[i].estado = "5" THEN
        LET arr_1[i].des_diagnostico = null
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
        END IF


       IF (arr_1[i].estado = 8 OR 
	   arr_1[i].estado = 15 OR
	   arr_1[i].estado = 16 OR
	   arr_1[i].estado = 17 OR
	   arr_1[i].estado = 41 OR
	   arr_1[i].estado = 7  ) THEN

	   SELECT SUM(saldo_sar_92),
		  SUM(saldo_viv_92)
           INTO   sal_saldo_sar ,
		  sal_saldo_viv 
           FROM   tra_det_trasp_sal A  
	   WHERE  A.n_seguro          = reg_1.n_seguro
	   AND    A.n_seguro_ent      = arr_1[i].nss
	   AND    A.rfc_ent          = arr_1[i].rfc
	   AND    A.cve_ced_cuenta   = arr_1[i].icefa_cod
	   AND    A.nro_ctrl_icefa = arr_1[i].nro_int_cta

	   SELECT  SUM(int_sar_92) ,
		   SUM(int_viv_92)
           INTO   int_saldo_sar,
		  int_saldo_viv
           FROM   tra_det_trasp_int A  
	   WHERE  A.n_seguro         = reg_1.n_seguro
	   AND    A.n_seguro_ent_ced     = arr_1[i].nss
	   AND    A.rfc_ent_ced          = arr_1[i].rfc
	   AND    A.cve_ced_cuenta   = arr_1[i].icefa_cod
	   AND    A.nro_ctrl_icefa   = arr_1[i].nro_int_cta

            LET arr_1[i].saldo_sar_92 = sal_saldo_sar + int_saldo_sar
            LET arr_1[i].saldo_viv_92 = sal_saldo_viv + int_saldo_viv
          END IF

        LET i = i + 1
    END FOREACH
IF i = 1 THEN
   CALL despliega_mens(reg_1.n_seguro,"Solicitud inexistente para el nss:")
ELSE

FOR u = 1 TO i
        
LET arr_1_c[u].c1           = arr_cu[u].c1 
LET arr_1_c[u].c2           = arr_cu[u].c2
LET arr_1_c[u].n_folio_tra  = arr_1[u].n_folio_tra
LET arr_1_c[u].nss          = arr_1[u].nss
LET arr_1_c[u].rfc          = arr_1[u].rfc
LET arr_1_c[u].fecha_solic_tra  = arr_1[u].fecha_solic_tra
LET arr_1_c[u].fecha_captura  = arr_1[u].fecha_captura
LET arr_1_c[u].fecha_comp_icefa  = arr_1[u].fecha_comp_icefa
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
LET arr_1_c[u].tipo_comp_icefa  = arr_1[u].tipo_comp_icefa
LET arr_1_c[u].tipcom_desc  = arr_1[u].tipcom_desc
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
	   IF valida_modulo = 1  THEN
	      EXIT PROGRAM
	   END IF
            CALL inicializa()
     EXIT DISPLAY

        ON KEY (F2)

            let i = ARR_CURR()
            LET e_correlativo = arr_1_c[i].correlativo


    OPEN WINDOW tram0019 AT 11,16 WITH FORM "TRAM0019" ATTRIBUTE(BORDER)
    DISPLAY "                     CONSULTA APORTES SAR Y VIV 92               " 
    AT 2,1 ATTRIBUTE(REVERSE)



DECLARE cur_ap CURSOR FOR
select a.tipo_icefa,
       a.fech_mov_banxico,
       "SALDO",
       a.saldo_sar_92 ,
       a.saldo_viv_92 ,
       a.folio
FROM tra_det_trasp_sal a
WHERE   a.n_seguro = reg_1.n_seguro
AND     a.n_seguro_ent = arr_1_c[i].nss
AND     a.rfc_ent = arr_1_c[i].rfc
AND     a.cve_ced_cuenta = arr_1_c[i].icefa_cod
AND     a.nro_ctrl_icefa = arr_1_c[i].nro_int_cta
ORDER BY 1

LET rr = 1

FOREACH cur_ap INTO reg_ap.*,vv_folio

  LET arr_reg_ap[rr].* = reg_ap.*
  LET rr = rr + 1

select a.tipo_icefa,
       a.fech_mov_banxico,
       "INTERES",
       a.int_sar_92 ,
       a.int_viv_92
INTO   reg_ap.*
FROM   tra_det_trasp_int a
WHERE   a.n_seguro           = reg_1.n_seguro
AND     a.n_seguro_ent_ced   = arr_1_c[i].nss
AND     a.rfc_ent_ced        = arr_1_c[i].rfc
AND     a.cve_ced_cuenta     = arr_1_c[i].icefa_cod
AND     a.nro_ctrl_icefa     = arr_1_c[i].nro_int_cta
AND     a.folio = vv_folio

  LET arr_reg_ap[rr].* = reg_ap.*
  LET rr = rr + 1

DECLARE cur_udi CURSOR FOR 
select "A"                ,
       a.fecha_mov_banxico,
       "UDI"              ,
       a.saldo_udi        ,
       0
FROM   safre_tmp:tra_det_udi a
WHERE   a.n_seguro           = reg_1.n_seguro
AND     a.n_seguro_ent       = arr_1_c[i].nss
AND     a.rfc_ent            = arr_1_c[i].rfc
AND     a.cve_ced_cuenta     = arr_1_c[i].icefa_cod
AND     a.nro_ctrl_icefa     = arr_1_c[i].nro_int_cta

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
CLOSE WINDOW tram0019

  ON KEY (F3)

     LET i             = ARR_CURR()
     LET e_correlativo = arr_1_c[i].correlativo


    OPEN WINDOW tram0010 AT 13,12 WITH FORM "TRAM0010" ATTRIBUTE(BORDER)
    
    DISPLAY "              CONSULTA DEVOLUCION DE TRASPASO INDEBIDO            "
    AT 2,1 ATTRIBUTE(REVERSE)

DECLARE cur_ap_dev CURSOR FOR
select a.orig_tipo_trasp     ,
       a.acciones_sar_92     ,
       a.saldo_sar_92_banx   ,
       a.saldo_sar_pagar     ,
       a.saldo_viv_92       ,
       a.comis_saldo_sar     ,
       a.fecha_devol         ,
       a.ident_lote_solic
FROM   dev_det_liquidadas a
WHERE   a.n_seguro       = reg_1.n_seguro
AND     a.n_seguro_ent   = arr_1_c[i].nss
AND     a.rfc_ent        = arr_1_c[i].rfc
AND     a.cve_ced_cuenta = arr_1_c[i].icefa_cod
AND     a.nro_ctrl_icefa = arr_1_c[i].nro_int_cta

LET rr = 1

FOREACH cur_ap_dev INTO reg_ap_dev.orig_tipo_trasp   ,
			reg_ap_dev.acciones_sar_92   ,
			reg_ap_dev.saldo_sar_92_banx ,
			reg_ap_dev.saldo_sar_pagar   ,
			reg_ap_dev.saldo_viv_922     ,
			reg_ap_dev.comis_saldo_sar   ,
			reg_ap_dev.fecha_devol       ,      
			vv_ident

 IF reg_ap_dev.orig_tipo_trasp = "08" OR
    reg_ap_dev.orig_tipo_trasp = "09" THEN

      LET reg_ap_dev.saldo_sar_921 = 0
      LET reg_ap_dev.saldo_viv_921 = 0

      SELECT a.fecha_presentacion
      INTO reg_ap_dev.fecha_presentacion
      FROM  dev_det_confronta a
      WHERE   a.n_seguro       = reg_1.n_seguro
      AND     a.n_seguro_ent   = arr_1_c[i].nss
      AND     a.rfc_ent        = arr_1_c[i].rfc
      AND     a.cve_ced_cuenta = arr_1_c[i].icefa_cod
      AND     a.nro_ctrl_icefa = arr_1_c[i].nro_int_cta
      AND     YEAR(a.fecha_presentacion) = YEAR(reg_ap_dev.fecha_devol)
      AND     MONTH(a.fecha_presentacion) = MONTH(reg_ap_dev.fecha_devol)
      AND     a.orig_tipo_trasp = reg_ap_dev.orig_tipo_trasp


 ELSE
    SELECT a.saldo_sar_92, 
	   a.saldo_viv_92,
	   a.fecha_presentacion
    INTO   reg_ap_dev.saldo_sar_921,
	   reg_ap_dev.saldo_viv_921,
	   reg_ap_dev.fecha_presentacion
    FROM   dev_det_confronta a
    WHERE   a.n_seguro       = reg_1.n_seguro
    AND     a.n_seguro_ent   = arr_1_c[i].nss
    AND     a.rfc_ent        = arr_1_c[i].rfc
    AND     a.cve_ced_cuenta = arr_1_c[i].icefa_cod
    AND     a.nro_ctrl_icefa = arr_1_c[i].nro_int_cta
    AND     YEAR(a.fecha_presentacion) = YEAR(reg_ap_dev.fecha_devol)
    AND     MONTH(a.fecha_presentacion) = MONTH(reg_ap_dev.fecha_devol)
    AND     a.orig_tipo_trasp = reg_ap_dev.orig_tipo_trasp

 END IF

   SELECT a.saldo_sar_pagar_p ,
	  a.saldo_viv_92_p    
   INTO   reg_ap_dev.saldo_sar_pagar_p,
	  reg_ap_dev.saldo_viv_92_p
   FROM   dev_det_planchada a 
   WHERE   a.n_seguro         = reg_1.n_seguro
   AND     a.n_seguro_ent     = arr_1_c[i].nss
   AND     a.rfc_ent          = arr_1_c[i].rfc
   AND     a.cve_ced_cuenta   = arr_1_c[i].icefa_cod
   AND     a.nro_ctrl_icefa   = arr_1_c[i].nro_int_cta
   AND     a.orig_tipo_trasp  = reg_ap_dev.orig_tipo_trasp
   AND     a.ident_lote_solic = vv_ident


   IF reg_ap_dev.saldo_sar_pagar_p IS NULL THEN  
      LET reg_ap_dev.saldo_sar_pagar_p = 0
   END IF
   IF reg_ap_dev.saldo_viv_92_p IS NULL THEN  
      LET reg_ap_dev.saldo_viv_92_p = 0
   END IF

   LET reg_ap_dev.estatus = "LIQUIDADA"  

  LET arr_reg_ap_dev[rr].* = reg_ap_dev.*
  LET rr = rr + 1
END FOREACH

DECLARE cur_dev_2 CURSOR FOR 
SELECT "DEVUELTA"      ,
       a.fecha_devol   ,
       a.orig_tipo_trasp,
       a.diagnostico
INTO   reg_ap_dev.estatus           ,
       reg_ap_dev.fecha_presentacion,
       reg_ap_dev.orig_tipo_trasp
FROM   dev_det_normal a
WHERE   a.n_seguro         = reg_1.n_seguro
AND     a.n_seguro_ent     = arr_1_c[i].nss
AND     a.rfc_ent          = arr_1_c[i].rfc
AND     a.cve_ced_cuenta   = arr_1_c[i].icefa_cod
AND     a.nro_ctrl_icefa   = arr_1_c[i].nro_int_cta
AND     a.status = 2
      

INITIALIZE reg_ap_dev.* TO NULL

FOREACH cur_dev_2 INTO   reg_ap_dev.estatus           ,
                         reg_ap_dev.fecha_presentacion,
                         reg_ap_dev.orig_tipo_trasp   ,
			 reg_ap_dev.diagnostico

  LET arr_reg_ap_dev[rr].* = reg_ap_dev.*
  LET rr = rr + 1


END FOREACH

INITIALIZE reg_ap_dev.* TO NULL

DECLARE cur_dev_3 CURSOR FOR 
SELECT "RECHAZO" ,
       a.orig_tipo_trasp,
       a.saldo_sar_92_banx,
       a.saldo_sar_pagar,
       a.saldo_viv_92 
 FROM dev_det_rechazada a
 WHERE   a.n_seguro         = reg_1.n_seguro
 AND     a.n_seguro_ent     = arr_1_c[i].nss
 AND     a.rfc_ent          = arr_1_c[i].rfc
 AND     a.cve_ced_cuenta   = arr_1_c[i].icefa_cod
 AND     a.nro_ctrl_icefa   = arr_1_c[i].nro_int_cta
      
 FOREACH cur_dev_3 INTO reg_ap_dev.estatus          ,
			reg_ap_dev.orig_tipo_trasp  ,
			reg_ap_dev.saldo_sar_92_banx,
			reg_ap_dev.saldo_sar_pagar  ,
			reg_ap_dev.saldo_viv_922


 SELECT a.acciones_sar_92 ,
	a.comis_saldo_sar ,
	a.fecha_devol     
 INTO   reg_ap_dev.acciones_sar_92 ,
	reg_ap_dev.comis_saldo_sar ,
	reg_ap_dev.fecha_devol   
 FROM   dev_det_normal a
 WHERE   a.n_seguro         = reg_1.n_seguro
 AND     a.n_seguro_ent     = arr_1_c[i].nss
 AND     a.rfc_ent          = arr_1_c[i].rfc
 AND     a.cve_ced_cuenta   = arr_1_c[i].icefa_cod
 AND     a.nro_ctrl_icefa   = arr_1_c[i].nro_int_cta
 AND     a.orig_tipo_trasp  = reg_ap_dev.orig_tipo_trasp
# AND     YEAR(fecha_devol)  = YEAR(reg_ap_dev.fecha_presentacion)
# AND     MONTH(fecha_devol) = MONTH(reg_ap_dev.fecha_presentacion)
 AND     a.saldo_sar_pagar  = reg_ap_dev.saldo_sar_pagar

 IF reg_ap_dev.orig_tipo_trasp = "08" OR
    reg_ap_dev.orig_tipo_trasp = "09" THEN

      LET reg_ap_dev.saldo_sar_921 = 0
      LET reg_ap_dev.saldo_viv_921 = 0

      SELECT a.fecha_presentacion
      INTO reg_ap_dev.fecha_presentacion
      FROM  dev_det_confronta a
      WHERE   a.n_seguro       = reg_1.n_seguro
      AND     a.n_seguro_ent   = arr_1_c[i].nss
      AND     a.rfc_ent        = arr_1_c[i].rfc
      AND     a.cve_ced_cuenta = arr_1_c[i].icefa_cod
      AND     a.nro_ctrl_icefa = arr_1_c[i].nro_int_cta
      AND     YEAR(a.fecha_presentacion) = YEAR(reg_ap_dev.fecha_devol)
      AND     MONTH(a.fecha_presentacion) = MONTH(reg_ap_dev.fecha_devol)
      AND     a.orig_tipo_trasp = reg_ap_dev.orig_tipo_trasp


 ELSE
    SELECT a.saldo_sar_92, 
	   a.saldo_viv_92,
	   a.fecha_presentacion
    INTO   reg_ap_dev.saldo_sar_921,
	   reg_ap_dev.saldo_viv_921,
	   reg_ap_dev.fecha_presentacion
    FROM   dev_det_confronta a
    WHERE   a.n_seguro       = reg_1.n_seguro
    AND     a.n_seguro_ent   = arr_1_c[i].nss
    AND     a.rfc_ent        = arr_1_c[i].rfc
    AND     a.cve_ced_cuenta = arr_1_c[i].icefa_cod
    AND     a.nro_ctrl_icefa = arr_1_c[i].nro_int_cta
    AND     YEAR(a.fecha_presentacion) = YEAR(reg_ap_dev.fecha_devol)
    AND     MONTH(a.fecha_presentacion) = MONTH(reg_ap_dev.fecha_devol)
    AND     a.orig_tipo_trasp = reg_ap_dev.orig_tipo_trasp

 END IF

  LET arr_reg_ap_dev[rr].* = reg_ap_dev.*
  LET rr = rr + 1

END FOREACH



 CALL SET_COUNT(rr-1)

 DISPLAY ARRAY arr_reg_ap_dev TO scr_ap_dev.*

 ON KEY(INTERRUPT)
    EXIT DISPLAY
 END DISPLAY
CLOSE WINDOW tram0010


        ON KEY (Control-p)
	   IF valida_modulo = 1 THEN
            LET i = ARR_CURR()
	   ELSE 
            LET i = ARR_CURR()
            LET e_correlativo = arr_1_c[i].correlativo

            IF  arr_1_c[i].estado <> 3
            AND arr_1_c[i].estado <> 5
            AND arr_1_c[i].estado <> 6
            AND arr_1_c[i].estado <> 9
            AND arr_1_c[i].estado <> 20 THEN

                CASE arr_1_c[1].estado
                    WHEN 1
                        LET c_des_estado ="ICEFA CONFIRMADA, "
                    WHEN 2
                        LET c_des_estado ="ICEFA ENVIADA A PROCESAR, "
                    WHEN 4
                        LET c_des_estado ="ICEFA ACEPTADA PROCESAR, "
                    WHEN 41
                        LET c_des_estado ="ICEFA ACEPTADA BANCO, "
                    WHEN 7
                        LET c_des_estado ="ICEFA PROVISIONADA , "
                    WHEN 8
                        LET c_des_estado ="ICEFA LIQUIDADA, "
                    WHEN 17
                        LET c_des_estado ="DEVOL. EXITOSA, "
                END CASE

                LET sw_5 = 1
                PROMPT c_des_estado CLIPPED,
                       "NO PUEDE SER MODIFICADA..<ENTER> PARA CONTINUAR"
                        ATTRIBUTE(REVERSE) FOR enter
                INITIALIZE arr_1_c TO NULL
                CLEAR FORM
            EXIT DISPLAY
          ELSE 
            CALL Pregunta(1)
            IF aux_pausa MATCHES "[Ss]" THEN
              DELETE FROM tra_mae_icefa
              WHERE correlativo = e_correlativo
               ERROR "ICEFA ELIMINADA..." SLEEP 2
               ERROR ""
               CALL inicializa_reg_1()
            ELSE 
               ERROR "ELIMINACION DE ICEFA CANCELADA..." SLEEP 2
               ERROR ""
               CALL inicializa_reg_1()
            END IF
         END IF
               CLEAR FORM
        EXIT DISPLAY
	 END IF
    END DISPLAY
    END IF
    END WHILE
    CLOSE WINDOW tram0012

END FUNCTION

FUNCTION elimina(e_correlativo)
#el-----------------------------------
    DEFINE e_correlativo LIKE tra_mae_icefa.correlativo


    LET e_correlativo = arr_1[i].correlativo
 
 DELETE FROM tra_mae_icefa
 WHERE correlativo = e_correlativo

END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE reg_5 RECORD
        nss                   LIKE tra_mae_icefa.nss         ,
        rfc                   LIKE tra_mae_icefa.rfc         ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod   ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta
    END RECORD

    OPEN WINDOW tram0015 AT 2,2 WITH FORM "TRAM0015" ATTRIBUTE( BORDER)
    DISPLAY " TRAM001             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " [ Esc ] Modifica      [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
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
                  reg_2_m.des_icefa        ,
                  reg_2_m.nro_int_cta      ,
                  reg_2_m.tipo_comp_icefa  WITHOUT DEFAULTS

        BEFORE FIELD n_folio_tra
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()
    DISPLAY "    Icefa: "  AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "/"  AT 8,14 ATTRIBUTE(REVERSE)
   DISPLAY arr_m_c[arr_c].c1 TO c1
   DISPLAY arr_m_c[arr_c].c2 TO c2
            IF sw_3 = 0 THEN
                LET sw_3                   = 1
                LET reg_2_m.n_folio_tra      = arr_m[arr_c].n_folio_tra
                LET reg_2_m.nss              = arr_m[arr_c].nss
                LET reg_2_m.rfc              = arr_m[arr_c].rfc
                LET reg_2_m.fecha_solic_tra  = arr_m[arr_c].fecha_solic_tra
                LET reg_2_m.fecha_captura    = arr_m[arr_c].fecha_captura
                LET reg_2_m.paterno2         = arr_m[arr_c].paterno2
                LET reg_2_m.materno2         = arr_m[arr_c].materno2
                LET reg_2_m.nombres2         = arr_m[arr_c].nombres2
                LET reg_2_m.icefa_cod        = arr_m[arr_c].icefa_cod
                LET reg_2_m.des_icefa        = arr_m[arr_c].des_icefa
                LET reg_2_m.nro_int_cta      = arr_m[arr_c].nro_int_cta
                LET reg_2_m.saldo_sar_92     = arr_m[arr_c].saldo_sar_92
                LET reg_2_m.saldo_viv_92     = arr_m[arr_c].saldo_viv_92
                LET reg_2_m.fuente           = arr_m[arr_c].fuente
                LET reg_2_m.usuario          = arr_m[arr_c].usuario
                LET reg_2_m.tipo_comp_icefa  = arr_m[arr_c].tipo_comp_icefa
                LET reg_2_m.tipcom_desc     = arr_m[arr_c].tipcom_desc
                LET reg_2_m.fecha_genera     = arr_m[arr_c].fecha_genera
                LET reg_2_m.fecha_proceso    = arr_m[arr_c].fecha_proceso
                LET reg_2_m.fecha_liquidacion = arr_m[arr_c].fecha_liquidacion
                LET reg_2_m.status           = arr_m[arr_c].estado
                LET reg_2_m.des_estado       = arr_m[arr_c].des_estado
                LET reg_2_m.correlativo      = arr_m[arr_c].correlativo
                LET reg_2_m.diag_proceso     = arr_m[arr_c].diag_proceso
                LET reg_2_m.des_diagnostico  = arr_m[arr_c].des_diagnostico

                
                LET reg_5.nss              = arr_m[arr_c].nss
                LET reg_5.rfc              = arr_m[arr_c].rfc
                LET reg_5.icefa_cod        = arr_m[arr_c].icefa_cod
                LET reg_5.nro_int_cta      = arr_m[arr_c].nro_int_cta
            END IF

            DISPLAY BY NAME reg_1.* 
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
            DISPLAY reg_2_m.nro_int_cta      TO nro_int_cta
            DISPLAY reg_2_m.saldo_sar_92     TO saldo_sar_92
            DISPLAY reg_2_m.saldo_viv_92     TO saldo_viv_92
            DISPLAY reg_2_m.fuente           TO fuente 
            DISPLAY reg_2_m.usuario          TO usuario
            DISPLAY reg_2_m.tipo_comp_icefa  TO tipo_comp_icefa
            DISPLAY reg_2_m.tipcom_desc     TO tipcom_desc
            DISPLAY reg_2_m.fecha_genera     TO fecha_genera
            DISPLAY reg_2_m.fecha_proceso    TO fecha_proceso 
            DISPLAY reg_2_m.fecha_liquidacion    TO fecha_liquidacion
            DISPLAY reg_2_m.status           TO status
            DISPLAY reg_2_m.des_estado       TO des_estado
            DISPLAY reg_2_m.correlativo      TO correlativo   
            DISPLAY reg_2_m.diag_proceso     TO diag_proceso
            DISPLAY reg_2_m.des_diagnostico  TO des_diagnostico


        AFTER FIELD paterno2
            IF reg_2_m.paterno2 IS NULL THEN
		LET reg_2_m.paterno2 = " "
		{
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD paterno2
		}
            END IF

        AFTER FIELD nombres2
            IF reg_2_m.nombres2 IS NULL THEN
		LET reg_2_m.nombres2 = " "
		{
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD nombres2
		}
            END IF

 AFTER FIELD icefa_cod

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres2
            END IF

            IF reg_2_m.icefa_cod IS NULL THEN
               CALL despliega_icefas()
               RETURNING reg_2_m.icefa_cod ,
                         reg_2_m.des_icefa

               IF reg_2_m.icefa_cod = 0 THEN 
                  NEXT FIELD icefa_cod 
               END IF
            ELSE
               SELECT icefa_desc
               INTO   reg_2_m.des_icefa
               FROM   tab_icefa
               WHERE  icefa_cod = reg_2_m.icefa_cod

               IF STATUS = NOTFOUND THEN
                  ERROR " ICEFA INEXISTENTE "
                  NEXT FIELD icefa_cod
               END IF
            END IF

     DISPLAY reg_2_m.icefa_cod TO icefa_cod
     DISPLAY reg_2_m.des_icefa TO des_icefa

                SELECT a.* 
		INTO  reg_fusion_icefa.*
		FROM  safre_tmp:tra_fusion_icefa a
		WHERE a.cve_ced_cuenta_org = reg_2_m.icefa_cod

                IF reg_fusion_icefa.cve_ced_cuenta_act <>
		   reg_2_m.icefa_cod THEN
		   ERROR"ICEFA FUSIONADA...",reg_2_m.icefa_cod," ",
			   reg_2_m.des_icefa
		   SLEEP 2

                   SELECT icefa_desc
                   INTO   reg_2_m.des_icefa
                   FROM   tab_icefa
                   WHERE  icefa_cod = reg_fusion_icefa.cve_ced_cuenta_act

                   LET reg_2_m.icefa_cod = reg_fusion_icefa.cve_ced_cuenta_act
		 END IF
                DISPLAY reg_2_m.icefa_cod TO icefa_cod
                DISPLAY reg_2_m.des_icefa TO des_icefa

 AFTER FIELD nro_int_cta
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD icefa_cod
            END IF

 AFTER FIELD tipo_comp_icefa
            IF reg_2_m.tipo_comp_icefa IS NULL THEN
                CALL despliega_tipo_comp_icefas() #dtci
                RETURNING reg_2_m.tipo_comp_icefa,
                          reg_2_m.tipcom_desc
                DISPLAY reg_2_m.tipcom_desc TO tipcom_desc
            ELSE
                SELECT tipcom_desc
                INTO   reg_2_m.tipcom_desc
                FROM   tab_tipo_com
                WHERE  tipcom_cod = reg_2_m.tipo_comp_icefa

                IF STATUS = NOTFOUND THEN
                    ERROR "TIPO DE COMPROBANTE INEXISTENTE"
                    NEXT FIELD tipo_comp_icefa
                END IF
                DISPLAY reg_2_m.tipcom_desc TO tipcom_desc
            END IF

        ON KEY ( INTERRUPT )
            LET sw_4 = 1
            EXIT INPUT

        ON KEY (ESC)
            ERROR "VERIFICANDO INFORMACION"

            IF reg_2_m.nss IS NULL AND reg_2_m.rfc IS NULL THEN
                ERROR " NSS Y RFC AMBOS NO PUEDEN SER NULOS "
                NEXT FIELD nss
            END IF

            IF reg_2_m.paterno2 IS NULL THEN
	       LET reg_2_m.paterno2 = " "
	       {
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD paterno2
		}
            END IF

            IF reg_2_m.nombres2 IS NULL THEN
	       LET reg_2_m.nombres2 = " "
	       {
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD nombres2
		}
            END IF
            IF reg_2_m.icefa_cod IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO "
                NEXT FIELD icefa_cod
            END IF

#ff
            IF  reg_5.nss         <> reg_2_m.nss
            OR  reg_5.rfc         <> reg_2_m.rfc
            OR  reg_5.icefa_cod   <> reg_2_m.icefa_cod
            OR  reg_5.nro_int_cta <> reg_2_m.nro_int_cta THEN

                SELECT "OK"
                FROM   tra_mae_icefa
                WHERE  n_seguro    = reg_1.n_seguro
                AND    nss         = reg_2_m.nss
                AND    rfc         = reg_2_m.rfc
                AND    icefa_cod   = reg_2_m.icefa_cod
                AND    nro_int_cta = reg_2_m.nro_int_cta 
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                    ERROR "ICEFA DUPLICADA" ATTRIBUTE(REVERSE) SLEEP 3
                    LET enter = "N"
                    NEXT FIELD n_folio_tra
                END IF
            END IF

            LET sw_4 = 1
            LET sw_3 = 2

            WHILE TRUE
            ERROR ""
               PROMPT "DESEA MODIFICAR LA SOLICITUD DE TRASPASO S/N " 
               FOR CHAR enter
               IF enter MATCHES"[SsNn]" THEN
                 EXIT WHILE 
               END IF
            END WHILE
        IF enter MATCHES "[Ss]" THEN
         IF reg_2_m.status = 3 OR 
	    reg_2_m.status = 5 OR 
	    reg_2_m.status = 6 THEN

            WHILE TRUE
               ERROR ""
               PROMPT "DESEA REENVIAR LA SOLICITUD DE TRASPASO S/N " 
               FOR CHAR enter
                 IF enter MATCHES"[SsNn]" THEN
                    EXIT WHILE 
                 END IF
            END WHILE
             IF enter MATCHES "[Ss]" THEN
	     {
                   CALL actualiza_det_sol_rech(reg_2_m.nss         ,
                                               reg_2_m.rfc         ,
                                               reg_2_m.icefa_cod   ,
                                               reg_2_m.nro_int_cta ,
                                               9) #adsr
                }
             UPDATE tra_mae_icefa
             SET    tra_mae_icefa.n_folio_tra      = reg_2_m.n_folio_tra     ,
                    tra_mae_icefa.nss              = reg_2_m.nss             ,
                    tra_mae_icefa.rfc              = reg_2_m.rfc             ,
                    tra_mae_icefa.fecha_captura    = reg_2_m.fecha_captura   ,
                    tra_mae_icefa.paterno          = reg_2_m.paterno2        ,
                    tra_mae_icefa.materno          = reg_2_m.materno2        ,
                    tra_mae_icefa.nombres          = reg_2_m.nombres2        ,
                    tra_mae_icefa.icefa_cod        = reg_2_m.icefa_cod       ,
                    tra_mae_icefa.nro_int_cta      = reg_2_m.nro_int_cta     ,
                    tra_mae_icefa.tipo_comp_icefa  = reg_2_m.tipo_comp_icefa ,
                    tra_mae_icefa.status           = 9                       ,
                    tra_mae_icefa.fecha_proceso    = HOY                     ,
		    tra_mae_icefa.usuario          = glo_usuario
              WHERE  tra_mae_icefa.n_seguro        = reg_1.n_seguro
              AND    tra_mae_icefa.correlativo     = reg_2_m.correlativo
             ELSE
              UPDATE tra_mae_icefa
              SET    tra_mae_icefa.n_folio_tra      = reg_2_m.n_folio_tra     ,
                     tra_mae_icefa.nss              = reg_2_m.nss             ,
                     tra_mae_icefa.rfc              = reg_2_m.rfc             ,
                     tra_mae_icefa.fecha_captura    = reg_2_m.fecha_captura   ,
                     tra_mae_icefa.paterno          = reg_2_m.paterno2        ,
                     tra_mae_icefa.materno          = reg_2_m.materno2        ,
                     tra_mae_icefa.nombres          = reg_2_m.nombres2        ,
                     tra_mae_icefa.icefa_cod        = reg_2_m.icefa_cod       ,
                     tra_mae_icefa.nro_int_cta      = reg_2_m.nro_int_cta     ,
                     tra_mae_icefa.tipo_comp_icefa  = reg_2_m.tipo_comp_icefa ,
                     tra_mae_icefa.fecha_proceso    = HOY,
		     tra_mae_icefa.usuario          = glo_usuario
               WHERE  tra_mae_icefa.n_seguro         = reg_1.n_seguro
               AND    tra_mae_icefa.correlativo      = reg_2_m.correlativo
             END IF
          ELSE
              UPDATE tra_mae_icefa
              SET    tra_mae_icefa.n_folio_tra      = reg_2_m.n_folio_tra     ,
                     tra_mae_icefa.nss              = reg_2_m.nss             ,
                     tra_mae_icefa.rfc              = reg_2_m.rfc             ,
                     tra_mae_icefa.fecha_captura    = reg_2_m.fecha_captura   ,
                     tra_mae_icefa.paterno          = reg_2_m.paterno2        ,
                     tra_mae_icefa.materno          = reg_2_m.materno2        ,
                     tra_mae_icefa.nombres          = reg_2_m.nombres2        ,
                     tra_mae_icefa.icefa_cod        = reg_2_m.icefa_cod       ,
                     tra_mae_icefa.nro_int_cta      = reg_2_m.nro_int_cta     ,
                     tra_mae_icefa.tipo_comp_icefa  = reg_2_m.tipo_comp_icefa ,
                     tra_mae_icefa.fecha_proceso    = HOY,
		     tra_mae_icefa.usuario          = glo_usuario
               WHERE  tra_mae_icefa.n_seguro         = reg_1.n_seguro
               AND    tra_mae_icefa.correlativo      = reg_2_m.correlativo

          END IF

            ERROR "ICEFA MODIFICADA" SLEEP 3 
            ERROR " "
         #   INITIALIZE reg_1.*  TO NULL
            INITIALIZE reg_2_m.*  TO NULL
         #   DISPLAY reg_1.* 

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
            DISPLAY reg_2_m.nro_int_cta      TO nro_int_cta
            DISPLAY reg_2_m.saldo_sar_92     TO saldo_sar_92
            DISPLAY reg_2_m.saldo_viv_92     TO saldo_viv_92
            DISPLAY reg_2_m.fuente           TO fuente
            DISPLAY reg_2_m.usuario          TO fuente
            DISPLAY reg_2_m.tipo_comp_icefa  TO tipo_comp_icefa
            DISPLAY reg_2_m.tipcom_desc     TO tipcom_desc
            DISPLAY reg_2_m.fecha_genera     TO fecha_genera
            DISPLAY reg_2_m.fecha_proceso    TO fecha_proceso 
            DISPLAY reg_2_m.fecha_liquidacion  TO fecha_liquidacion
            DISPLAY reg_2_m.correlativo      TO correlativo   
            CLEAR FORM
            EXIT INPUT
          ELSE
            CLEAR FORM 
            EXIT INPUT
          END IF
    END INPUT
    CLOSE WINDOW tram0015
END FUNCTION

FUNCTION despliega_icefas()
#di------------------------
 DEFINE aux_val  SMALLINT
 DEFINE l_reg ARRAY[1000] OF RECORD
        codigo  INTEGER,
        descripcion CHAR(50)
 END RECORD
 DEFINE x_x  char(100),
 x_buscar  char(30)
 DEFINE pos  SMALLINT

 OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
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

 WHILE TRUE
       LET x_x = " SELECT * FROM tab_icefa ",
                 " WHERE icefa_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                 " ORDER BY 1 " CLIPPED
       PREPARE curg19 FROM x_x
       DECLARE cur_g19 CURSOR FOR curg19
       LET pos = 1
       FOREACH cur_g19 INTO l_reg[pos].*
        LET pos = pos + 1
        IF pos >= 1000 THEN
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

    OPEN WINDOW pantallap1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
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

    OPEN WINDOW tram0013 AT 2,2 WITH FORM "TRAM0013" ATTRIBUTE( BORDER)
    DISPLAY " TRAM001             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_1 = 0
    WHILE TRUE
    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)
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
            END IF


        AFTER FIELD tipo_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_folio
            END IF

            IF  reg_1.tipo_solicitud > 5 THEN
                ERROR"TIPO SOLICITUD INVALIDO"
                NEXT FIELD tipo_solicitud
            END IF

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
            FROM   afi_mae_afiliado
            WHERE  n_folio        = reg_1.n_folio
            AND    tipo_solicitud = reg_1.tipo_solicitud

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
            FROM   afi_mae_afiliado
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
                NEXT FIELD n_unico
            END IF

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
            FROM   afi_mae_afiliado
            WHERE  n_rfc = reg_1.n_rfc

            IF STATUS = NOTFOUND THEN
                ERROR " RFC INEXISTENTE "
                NEXT FIELD n_rfc
            END IF
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
            EXIT INPUT

        AFTER FIELD n_unico
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_rfc
            END IF

            IF reg_1.n_unico IS NULL THEN
                NEXT FIELD n_folio
            END IF

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
            FROM   afi_mae_afiliado
            WHERE  n_unico = reg_1.n_unico

            IF STATUS = NOTFOUND THEN
                ERROR " CURP INEXISTENTE "
                NEXT FIELD n_unico
            END IF
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
            EXIT INPUT

    ON KEY ( INTERRUPT )
               LET sw_1 = 1
               EXIT INPUT
    END INPUT

    IF sw_1 = 1 THEN
        EXIT WHILE
    END IF

    DECLARE cur_33 CURSOR FOR 
    SELECT A.n_seguro         ,
           A.n_folio_tra      ,
           A.nss              ,
           A.rfc              ,
           A.fecha_solic_tra  ,
           A.fecha_captura    ,
           A.fecha_comp_icefa,
           A.paterno          ,
           A.materno          ,
           A.nombres          ,
           A.icefa_cod        ,
           B.icefa_desc       ,
           A.nro_int_cta      ,
           A.saldo_sar_92     ,
           A.saldo_viv_92     ,
           A.fuente           ,
           A.usuario          ,
           A.tipo_comp_icefa  ,
           " "                 ,#tipcom_desc
           A.fecha_genera     ,
           A.fecha_proceso    ,
           " "                 ,#fecha_liquidacion
           A.status           ,#estado
           " "                 ,#des_estado
           A.correlativo      ,
           " "                 ,#diag_proceso
           " "                 ,#des_diag_proceso
           A.fecha_genera     ,
           A.lote_genera
    FROM   tra_mae_icefa A, OUTER tab_icefa B
    WHERE  A.n_seguro       = reg_1.n_seguro
    AND    A.tipo_solicitud = reg_1.tipo_solicitud
    AND    A.icefa_cod      = B.icefa_cod

    SELECT count(*)
    INTO   cuantos
    FROM   tra_mae_icefa
    WHERE  tra_mae_icefa.n_seguro       = reg_1.n_seguro
    AND    tra_mae_icefa.tipo_solicitud = reg_1.tipo_solicitud


    DISPLAY "    Icefa: "  AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "/"  AT 8,14 ATTRIBUTE(REVERSE)
    ERROR " <ENTER>  SELECCIONA REGISTRO A MODIFICAR"
    LET i = 1

    FOREACH cur_33 INTO c11_n_seguro   ,
                        arr_m[i].*     ,
                        d_fecha_genera ,
                        s_lote_genera

        LET arr_cu[i].c1 = i
        LET arr_cu[i].c2 = cuantos

        SELECT tipcom_desc
        INTO   arr_m[i].tipcom_desc
        FROM   tab_tipo_com
        WHERE  tipcom_cod = arr_m[i].tipo_comp_icefa

        SELECT des_estado
        INTO   arr_m[i].des_estado
        FROM   tra_status
        WHERE  estado = arr_m[i].estado 

        IF arr_m[i].estado = "3" THEN
            CALL codigo_rechazo(reg_1.n_seguro    ,
				arr_m[i].nss         ,
                                arr_m[i].rfc         ,
                                arr_m[i].icefa_cod   ,
                                arr_m[i].nro_int_cta ,
                                d_fecha_genera       ,
                                s_lote_genera
                               ) #cr
            RETURNING arr_m[i].diag_proceso


            SELECT A.des_error 
            INTO   arr_1[i].des_diagnostico
            FROM   tab_rch_icefa  A
            WHERE  A.cod_error = arr_1[i].diag_proceso
        END IF

#ff
        IF ( arr_1[i].estado = 8 OR
	     arr_1[i].estado = 15 OR
	     arr_1[i].estado = 16 OR
	     arr_1[i].estado = 17) THEN
         
            SELECT max(A.fech_mov_banxico)j
            INTO   arr_1[i].fecha_liquidacion
            FROM   tra_det_trasp_sal A
            WHERE  A.n_seguro       = c11_n_seguro
            AND    A.n_seguro_ent   = arr_1[i].nss
            AND    A.rfc_ent        = arr_1[i].rfc
            AND    A.cve_ced_cuenta = arr_1[i].icefa_cod
            AND    A.nro_ctrl_icefa = arr_1[i].nro_int_cta
            AND    A.tipo_icefa     = "N"    

            CALL habil_siguiente(arr_1[i].fecha_liquidacion)
            RETURNING arr_1[i].fecha_liquidacion

        END IF

        IF arr_m[i].estado = "5" THEN
        LET arr_m[i].des_diagnostico = null
            CALL codigo_devol(arr_m[i].nss         ,
                              arr_m[i].rfc         ,
                              arr_m[i].icefa_cod   ,
                              arr_m[i].nro_int_cta ,
                              d_fecha_genera       ,
                              s_lote_genera
                             ) #cd
            RETURNING arr_m[i].diag_proceso

            SELECT A.des_devol
            INTO   arr_m[i].des_diagnostico
            FROM   tab_devolucion A
            WHERE  A.cod_devol = arr_m[i].diag_proceso
        END IF

        LET i = i + 1
    END FOREACH


IF i = 1 THEN
   CALL despliega_mens(reg_1.n_seguro,"Solicitud inexistente para el nss:")
ELSE

FOR u = 1 TO i
        
LET arr_m_c[u].c1           = arr_cu[u].c1 
LET arr_m_c[u].c2           = arr_cu[u].c2
LET arr_m_c[u].n_folio_tra  = arr_m[u].n_folio_tra
LET arr_m_c[u].nss          = arr_m[u].nss
LET arr_m_c[u].rfc          = arr_m[u].rfc
LET arr_m_c[u].fecha_solic_tra  = arr_m[u].fecha_solic_tra
LET arr_m_c[u].fecha_captura    = arr_m[u].fecha_captura
LET arr_m_c[u].fecha_comp_icefa = arr_m[u].fecha_comp_icefa
LET arr_m_c[u].paterno2      = arr_m[u].paterno2
LET arr_m_c[u].materno2      = arr_m[u].materno2
LET arr_m_c[u].nombres2      = arr_m[u].nombres2
LET arr_m_c[u].icefa_cod     = arr_m[u].icefa_cod
LET arr_m_c[u].des_icefa     = arr_m[u].des_icefa
LET arr_m_c[u].nro_int_cta   = arr_m[u].nro_int_cta
LET arr_m_c[u].saldo_sar_92  = arr_m[u].saldo_sar_92
LET arr_m_c[u].saldo_viv_92  = arr_m[u].saldo_viv_92
LET arr_m_c[u].fuente        = arr_m[u].fuente
LET arr_m_c[u].usuario       = arr_m[u].usuario
LET arr_m_c[u].tipo_comp_icefa    = arr_m[u].tipo_comp_icefa
LET arr_m_c[u].tipcom_desc        = arr_m[u].tipcom_desc
LET arr_m_c[u].fecha_genera       = arr_m[u].fecha_genera
LET arr_m_c[u].fecha_proceso      = arr_m[u].fecha_proceso
LET arr_m_c[u].fecha_liquidacion  = arr_m[u].fecha_liquidacion
LET arr_m_c[u].estado             = arr_m[u].estado
LET arr_m_c[u].des_estado         = arr_m[u].des_estado
LET arr_m_c[u].correlativo        = arr_m[u].correlativo
LET arr_m_c[u].diag_proceso       = arr_m[u].diag_proceso
LET arr_m_c[u].des_diagnostico    = arr_m[u].des_diagnostico

END FOR




    LET sw_5 = 0
    CALL SET_COUNT(i-1)
    DISPLAY ARRAY arr_m_c TO scr_1.*
        ON KEY ( CONTROL-M)
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

            IF  arr_m_c[arr_c].estado <> 3
            AND arr_m_c[arr_c].estado <> 5
            AND arr_m_c[arr_c].estado <> 6
            AND arr_m_c[arr_c].estado <> 9
            AND arr_m_c[arr_c].estado <> 20 THEN

                CASE arr_m_c[arr_c].estado
                    WHEN 1
                        LET c_des_estado ="ICEFA CONFIRMADA, "
                    WHEN 2
                        LET c_des_estado ="ICEFA ENVIADA A PROCESAR, "
                    WHEN 4
                        LET c_des_estado ="ICEFA ACEPTADA PROCESAR, "
                    WHEN 41
                        LET c_des_estado ="ICEFA ACEPTADA BANCO, "
                    WHEN 7
                        LET c_des_estado ="ICEFA PROVISIONADA , "
                    WHEN 8
                        LET c_des_estado ="ICEFA LIQUIDADA, "
                    WHEN 17
                        LET c_des_estado ="DEVOL. EXITOSA, "
                END CASE

                LET sw_5 = 1
                PROMPT c_des_estado CLIPPED,
                       "NO PUEDE SER MODIFICADA..<ENTER> PARA CONTINUAR"
                        ATTRIBUTE(REVERSE) FOR enter
                INITIALIZE arr_m_c TO NULL
                CLEAR FORM
            END IF
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
   END IF 
    END WHILE
    CLOSE WINDOW tram0013
END FUNCTION

FUNCTION trac011()
#t----------------
##############################################################################
#Owner             => E.F.P.
#Programa TRAC011  => CONSULTA DE ICEFAS RECHAZADAS             
#Fecha creacion    => 29 MAYO DE 1998       
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 03 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
   define sw_1 smallint
   LET HOY = TODAY

   CALL init()
   OPEN WINDOW tram0016 AT 2,2 WITH FORM "TRAM0016" ATTRIBUTE(BORDER)
   DISPLAY " TRAC011             CONSULTA DE ICEFAS RECHAZADAS                             " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

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
                FROM   tra_mae_icefa A
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
CLOSE WINDOW tram0016
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
        nss                   LIKE tra_mae_icefa.nss         ,
        rfc                   LIKE tra_mae_icefa.rfc         ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod   ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta ,
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

    OPEN WINDOW tram0014 AT 05,12 WITH FORM "TRAM0014" ATTRIBUTE(BORDER)
    DISPLAY "                BUSQUEDA DE AFILIADOS                        " 
    AT 2,1 ATTRIBUTE(REVERSE)
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
close window tram0014
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
    CLOSE WINDOW tram0014
if pos <> 0  then
    RETURN l_reg[pos].n_folio ,l_reg[pos].tipo_solicitud
else 
    return "",""
end if
END FUNCTION

FUNCTION codigo_rechazo(reg_3)
#cr---------------------------
    DEFINE reg_3 RECORD #loc #reg_3
	n_seguro              LIKE tra_mae_icefa.n_seguro     ,
        nss                   LIKE tra_mae_icefa.nss          ,
        rfc                   LIKE tra_mae_icefa.rfc          ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod    ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta  ,
        fecha_genera          LIKE tra_mae_icefa.fecha_genera ,
        lote_genera           LIKE tra_mae_icefa.lote_genera
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


                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.n_seguro                = reg_3.n_seguro 
		AND    A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1

    WHENEVER ERROR STOP
END FUNCTION

FUNCTION codigo_devol(reg_3)
#cd-------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        nss                   LIKE tra_mae_icefa.nss          ,
        rfc                   LIKE tra_mae_icefa.rfc          ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod    ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta  ,
        fecha_genera          LIKE tra_mae_icefa.fecha_genera ,
        lote_genera           LIKE tra_mae_icefa.lote_genera
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

    OPEN WINDOW tram0017 AT 05,12 WITH FORM "TRAM0017" ATTRIBUTE(BORDER)
    DISPLAY "               MARCA DE POSIBLES TRASPASOS INDEBIDOS                           " 
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
close window tram0017
return
end if

    #DISPLAY "PROCESANDO INFORMACION" AT 13,2 ATTRIBUTE (REVERSE)
  LET x_x = " SELECT folio,n_seguro,n_seguro_ent,rfc,rfc_ent,cve_ced_cuenta,",
            " fech_mov_banxico,nro_ctrl_icefa,estado ",
            " FROM   tra_det_trasp_sal ",
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
        FROM tra_det_trasp_sal
        WHERE  tra_det_trasp_sal.folio          = l_reg[ind].folio
        AND    tra_det_trasp_sal.n_seguro       = l_reg[ind].n_seguro
        AND    tra_det_trasp_sal.n_seguro_ent   = l_reg[ind].n_seguro_ent
        AND    tra_det_trasp_sal.rfc_ent        = l_reg[ind].rfc_ent
        AND    tra_det_trasp_sal.cve_ced_cuenta = l_reg[ind].cve_ced_cuenta
        AND    tra_det_trasp_sal.nro_ctrl_icefa = l_reg[ind].nro_ctrl_icefa
{
if fff = 15 or 
   fff = 17 then

ERROR "Cuenta no se puede marcar ..."

ELSE
}
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
    CLOSE WINDOW tram0017
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

    DEFINE reg_dev1 RECORD LIKE tra_det_trasp_sal.*

    DEFINE #loc #date
        d_fecha_genera        DATE

    DEFINE #loc #char
        c11_n_seguro          CHAR(11)

    DEFINE #loc #smallint
        s_lote_genera         SMALLINT ,
        i                     SMALLINT

    DEFINE #loc #integer
        i_correlativo         INTEGER

    OPEN WINDOW tram0012 AT 2,2 WITH FORM "TRAM0012" ATTRIBUTE( BORDER)
    DISPLAY " TRAM001             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ F2 ] Marcar [ F3 ] Desmarcar  [ Ctrl-c ] Salir     "
            AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_1 = 0
    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)
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
           A.tipo_comp_icefa  ,
           " "                 ,#tipcom_desc
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
    FROM   tra_mae_icefa A, tab_icefa B
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

        SELECT tipcom_desc
        INTO   arr_1[i].tipcom_desc
        FROM   tab_tipo_com
        WHERE  tipcom_cod = arr_1[i].tipo_comp_icefa

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
            FROM   tra_det_trasp_sal A
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
LET arr_1_c[u].tipo_comp_icefa  = arr_1[u].tipo_comp_icefa
LET arr_1_c[u].tipcom_desc  = arr_1[u].tipcom_desc
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
               UPDATE tra_mae_icefa 
	       SET tra_mae_icefa.status  = 8,
		   tra_mae_icefa.usuario = glo_usuario
	       WHERE tra_mae_icefa.correlativo = arr_1_c[1].correlativo

        UPDATE tra_det_trasp_sal
        SET    tra_det_trasp_sal.estado  = 8
        WHERE  tra_det_trasp_sal.folio          = reg_dev.folio
        AND    tra_det_trasp_sal.n_seguro       = reg_dev.n_seguro
        AND    tra_det_trasp_sal.n_seguro_ent   = reg_dev.n_seguro_ent
        AND    tra_det_trasp_sal.rfc_ent        = reg_dev.rfc_ent
        AND    tra_det_trasp_sal.cve_ced_cuenta = reg_dev.cve_ced_cuenta
        AND    tra_det_trasp_sal.nro_ctrl_icefa = reg_dev.nro_ctrl_icefa
        AND    tra_det_trasp_sal.fech_mov_banxico = reg_dev.fech_mov_banxico

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
	      FROM   tra_mae_icefa A 
	      WHERE  A.n_seguro    = reg_dev.n_seguro
	      AND    A.nss         = reg_dev.n_seguro_ent
	      AND    A.rfc         = reg_dev.rfc_ent
	      AND    A.icefa_cod   = reg_dev.cve_ced_cuenta
	      AND    A.nro_int_cta = reg_dev.nro_ctrl_icefa

              CALL f_desmarca_cuenta(reg_dev.n_seguro,270,d_corr)

               UPDATE tra_mae_icefa 
	       SET tra_mae_icefa.status = 8,
		   tra_mae_icefa.usuario = glo_usuario
	       WHERE tra_mae_icefa.correlativo = arr_1_c[1].correlativo

        UPDATE tra_det_trasp_sal
        SET    tra_det_trasp_sal.estado  = 8
        WHERE  tra_det_trasp_sal.folio          = reg_dev.folio
        AND    tra_det_trasp_sal.n_seguro       = reg_dev.n_seguro
        AND    tra_det_trasp_sal.n_seguro_ent   = reg_dev.n_seguro_ent
        AND    tra_det_trasp_sal.rfc_ent        = reg_dev.rfc_ent
        AND    tra_det_trasp_sal.cve_ced_cuenta = reg_dev.cve_ced_cuenta
        AND    tra_det_trasp_sal.nro_ctrl_icefa = reg_dev.nro_ctrl_icefa
        AND    tra_det_trasp_sal.fech_mov_banxico = reg_dev.fech_mov_banxico

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

        UPDATE tra_mae_icefa 
        SET status =  15,
	    usuario = glo_usuario
        WHERE n_seguro    = reg_dev.n_seguro
        and   nss         = reg_dev.n_seguro_ent
        and   rfc         = reg_dev.rfc_ent
        and   icefa_cod   = reg_dev.cve_ced_cuenta
        and   nro_int_cta = reg_dev.nro_ctrl_icefa

        UPDATE tra_det_trasp_sal
        SET    tra_det_trasp_sal.estado  = 15
        WHERE  tra_det_trasp_sal.folio          = reg_dev.folio
        AND    tra_det_trasp_sal.n_seguro       = reg_dev.n_seguro
        AND    tra_det_trasp_sal.n_seguro_ent   = reg_dev.n_seguro_ent
        AND    tra_det_trasp_sal.rfc_ent        = reg_dev.rfc_ent
        AND    tra_det_trasp_sal.cve_ced_cuenta = reg_dev.cve_ced_cuenta
        AND    tra_det_trasp_sal.nro_ctrl_icefa = reg_dev.nro_ctrl_icefa
        AND    tra_det_trasp_sal.fech_mov_banxico = reg_dev.fech_mov_banxico

 SELECT des_estado
 INTO v_estado
 FROM tra_status
 WHERE estado =  15


    SELECT *
    INTO reg_dev1.*
    FROM tra_det_trasp_sal
    WHERE  tra_det_trasp_sal.folio          = reg_dev.folio
    AND    tra_det_trasp_sal.n_seguro       = reg_dev.n_seguro
    AND    tra_det_trasp_sal.n_seguro_ent   = reg_dev.n_seguro_ent
    AND    tra_det_trasp_sal.rfc_ent        = reg_dev.rfc_ent
    AND    tra_det_trasp_sal.cve_ced_cuenta = reg_dev.cve_ced_cuenta
    AND    tra_det_trasp_sal.nro_ctrl_icefa = reg_dev.nro_ctrl_icefa
    AND    tra_det_trasp_sal.fech_mov_banxico = reg_dev.fech_mov_banxico
    
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
    CLOSE WINDOW tram0012
RETURN v_estado
END FUNCTION
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

  OPEN WINDOW tram00120 AT 12,20 WITH FORM "TRAM00120" ATTRIBUTE(BORDER)
    DISPLAY  cadena AT  4,3
    DISPLAY BY NAME vnss ATTRIBUTE(REVERSE)
    SLEEP 4
    IF valida_modulo = 1 THEN
       EXIT PROGRAM
    END IF
    CLOSE WINDOW tram00120
END FUNCTION


FUNCTION formatea(nro_ctrl_anterior,f_cve_ced_cuenta)
#f----------------------------------------------------

DEFINE nro_ctrl_anterior    CHAR(030)
DEFINE v_format             CHAR(030)
DEFINE i_format             INTEGER
DEFINE f_cve_ced_cuenta     CHAR(003)
DEFINE nro_ctrl_nuevo       CHAR(030)
DEFINE k                    SMALLINT
DEFINE g_icefa           LIKE    safre_tmp:tra_icefa_ref.icefa            ,
       g_icefa_envio     LIKE    safre_tmp:tra_icefa_ref.icefa_envio      ,
       g_formato_icefa   LIKE    safre_tmp:tra_icefa_ref.formato_icefa    ,
       g_tipo_formato    LIKE    safre_tmp:tra_icefa_ref.tipo_formato     ,
       g_raz_social      LIKE    tab_afore_local.razon_social             ,
       g_cod_afore       LIKE    tab_afore_local.codigo_afore             ,
       g_formato         CHAR(30)                                         ,
       g_num_linea       INTEGER                                          ,
       si_formatea       SMALLINT                                         ,
       x                 SMALLINT                                         ,
       y                 SMALLINT

DEFINE    reg               RECORD 
          linea                     CHAR(01)
END   RECORD

DEFINE p_nro             CHAR(003)


    LET f_cve_ced_cuenta = f_cve_ced_cuenta USING"&&&"

    LET g_num_linea =  0

    LET si_formatea =  0

   ############ 
   #Inicia Rutina para Formaterar Numero de Control Icefa 
   ############

   SELECT a.icefa         ,
          a.icefa_envio   ,
          a.formato_icefa ,
          a.tipo_formato
   INTO   g_icefa         ,
          g_icefa_envio   ,
          g_formato_icefa ,
          g_tipo_formato 
   FROM safre_tmp:tra_icefa_ref a
   WHERE  a.icefa = f_cve_ced_cuenta
       CASE g_tipo_formato
        WHEN 1

               LET g_formato[1,8]  = g_formato_icefa
               LET g_formato[9,30] = " "
	       LET nro_ctrl_nuevo= g_formato

          EXIT CASE
        WHEN 2
	    LET u = 1

            FOR k = 1 TO 30 
	        IF nro_ctrl_anterior[k] <> " " THEN
                   LET v_format[u] = nro_ctrl_anterior[k] 
		   LET u = u + 1
                 END IF
            END FOR

            LET g_formato[1,18]  =  "000000000000000000"
            LET i_format         =  v_format

	    IF i_format IS NULL THEN 
	       LET i_format = 0
            END IF

            LET g_formato[19,30] =  i_format USING "&&&&&&&&&&&&"
	    LET nro_ctrl_nuevo   = g_formato

            EXIT CASE

        WHEN  3
            IF g_icefa = "071" THEN
              IF nro_ctrl_anterior[1,24] = "                        " THEN
                 FOR x = 25 TO 30
                  IF nro_ctrl_anterior[x] = " "  THEN
                     LET si_formatea = 1
                     EXIT  FOR
                  END IF
                 END FOR
               ELSE
                 LET si_formatea = 1
               END IF
               IF si_formatea THEN
                  LET g_formato[1,24]  = "                        "
                  LET g_formato[25,30] = nro_ctrl_anterior[1,6]
                                         USING   "&&&&&&"
	          LET nro_ctrl_nuevo = g_formato
               END IF
             ELSE
               IF nro_ctrl_anterior[10,30] = "                     " THEN
                  FOR x = 1 TO 9
                    IF nro_ctrl_anterior[x] = " " THEN
                       LET si_formatea = 1
                       EXIT  FOR
                    END IF
                  END FOR
               ELSE
                  LET si_formatea = 1
               END IF
               IF si_formatea THEN
                  LET g_formato[1,9]   = nro_ctrl_anterior[1,9]
                                         USING     "&&&&&&&&&"
                  LET g_formato[10,30] = "                     "
	          LET nro_ctrl_nuevo = g_formato
               END IF
             END IF

         WHEN 4
             IF g_icefa = "021" THEN
               IF nro_ctrl_anterior[1,30] <> 
                  "                              "  THEN
                  LET g_formato[1,30] = "                              "
                  LET si_formatea = 1
	          LET nro_ctrl_nuevo = g_formato
               END IF
             ELSE
               IF nro_ctrl_anterior[9,30] = "                      " THEN
                  FOR x = 1 TO 8
                    IF nro_ctrl_anterior[x] = " " THEN
                      LET si_formatea =  1
                      EXIT  FOR
                    END IF
                  END FOR
               ELSE
                  LET si_formatea =  1
               END IF
             IF si_formatea                THEN
                LET g_formato[1,8] = nro_ctrl_anterior[1,8]
                                     USING     "&&&&&&&&"
                LET g_formato[9,30]= " "
	        LET nro_ctrl_nuevo = g_formato
             END IF
           END IF
        WHEN  5
           IF nro_ctrl_anterior[1,30] <> 
              "                              "  THEN
              LET g_formato[1,30] = " "
              LET si_formatea     =  1
	      LET nro_ctrl_nuevo = g_formato
           END IF
        WHEN  6

	    LET u = 1

            FOR k = 1 TO 30 
	        IF nro_ctrl_anterior[k] <> " " THEN
                   LET v_format[u] = nro_ctrl_anterior[k] 
		   LET u = u + 1
                 END IF
            END FOR

            LET g_formato[1,14]  =  "00000000000000"
            LET i_format         =  v_format

	    IF i_format IS NULL THEN 
	       LET i_format = 0
            END IF

            LET g_formato[15,30] =  i_format USING "&&&&&&&&&&&&&&&&"
	    LET nro_ctrl_nuevo   = g_formato

            EXIT CASE

	    OTHERWISE 
	      EXIT CASE
        END CASE


   ##########   
   #Termina  Rutina   para   Formatear   Numero de Control Icefa  ####
   ##########   
    RETURN    nro_ctrl_nuevo

END FUNCTION
