########################################################################
#Proyecto          => SISTEMA DE safre_af.( MEXICO )        
#Owner             => E.F.P                                
#Programa SEPM100  => MANTENIMIENTO SOL ACLARACION Y CORRECCION NSS
#Fecha creacion    => 11 ENERO 2005
#By                => JESUS YAÑEZ MORENO
#Fecha actualiz.   => 23 sep 2005
#Actualizacion     => Jesus Yanez Moreno
#Sistema           => SEP
#Modificacion      :  Jesus Yañez Moreno
#Descripcion       :  Se modifica para soportar el idSolicitudSeparacion
#                     y ya no el folio,se quita la ejecucion del programa
#                     SEPC001 y SEPC007 y SEPL001 del MENU
#Fecha             : 3 abr 2010
#########################################################################

DATABASE safre_af

GLOBALS

DEFINE comando               char(100)
DEFINE i_corr                INTEGER
  DEFINE band_conf            SMALLINT
  DEFINE g                    ,
         u                    ,
         vt                   , 
         rr                   ,
         x                    , 
         ba                   , 
         gg                   , 
         fff                  , 
         actual_habil         , 
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
         saldo_sar_pagar_p      like dev_det_planchada.saldo_sar_pagar_p,
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
           des_icefa             CHAR(20)                            ,
           nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta      ,
           saldo_sar_92          LIKE tra_mae_icefa_issste.saldo_sar_92     ,
           saldo_viv_92          LIKE tra_mae_icefa_issste.saldo_viv_92     ,
           fuente                LIKE tra_mae_icefa_issste.fuente           ,
           usuario               LIKE tra_mae_icefa_issste.usuario          ,
           fecha_genera          LIKE tra_mae_icefa_issste.fecha_genera   ,
           fecha_proceso         LIKE tra_mae_icefa_issste.fecha_proceso    ,
           fecha_liquidacion     DATE                                ,
           status                LIKE tra_mae_icefa_issste.status           ,
           des_estado            CHAR(20)                            ,
           correlativo           LIKE tra_mae_icefa_issste.correlativo      ,
           diag_proceso          CHAR(3)                             ,
           des_diagnostico       CHAR(30)
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
        des_icefa             CHAR(20)                       ,
        nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta      ,
        saldo_sar_92          LIKE tra_mae_icefa_issste.saldo_sar_92     ,
        saldo_viv_92          LIKE tra_mae_icefa_issste.saldo_viv_92     ,
        fuente                LIKE tra_mae_icefa_issste.fuente          ,
        usuario               LIKE tra_mae_icefa_issste.usuario         ,
        fecha_genera          LIKE tra_mae_icefa_issste.fecha_genera   ,
        fecha_proceso         LIKE tra_mae_icefa_issste.fecha_proceso    ,
        fecha_liquidacion     DATE                           ,
        estado                LIKE tra_mae_icefa_issste.status           ,
        des_estado            CHAR(20)                       ,
        correlativo           LIKE tra_mae_icefa_issste.correlativo      ,
        diag_proceso          CHAR(3)                        ,
        des_diagnostico       CHAR(30)
    END RECORD

    DEFINE arr_m_c  ARRAY[100] OF RECORD #arr_m
        c1                    SMALLINT, 
        c2                    SMALLINT, 
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
        des_icefa             CHAR(20)                       ,
        nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta      ,
        saldo_sar_92          LIKE tra_mae_icefa_issste.saldo_sar_92     ,
        saldo_viv_92          LIKE tra_mae_icefa_issste.saldo_viv_92     ,
        fuente                LIKE tra_mae_icefa_issste.fuente          ,
        usuario               LIKE tra_mae_icefa_issste.usuario         ,
        fecha_genera          LIKE tra_mae_icefa_issste.fecha_genera   ,
        fecha_proceso         LIKE tra_mae_icefa_issste.fecha_proceso    ,
        fecha_liquidacion     DATE                           ,
        estado                LIKE tra_mae_icefa_issste.status           ,
        des_estado            CHAR(20)                       ,
        correlativo           LIKE tra_mae_icefa_issste.correlativo      ,
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

END GLOBALS
MAIN
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
  ACCEPT KEY CONTROL-I
  DEFER INTERRUPT

    CALL STARTLOG("SEPM100.log")
    CALL init()

    OPEN WINDOW ventana_1 AT 2,2 WITH 21 ROWS,78 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " SEPM100    MANTENIMIENTO SOLICITUD ACLARACION O CORRECCION NSS                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    #CALL inicializa()


LET valida_modulo = 0
LET valida_modulo = ARG_VAL(1)
LET nss_serv      = ARG_VAL(2)

IF valida_modulo = 1 THEN

    CALL consulta()
    EXIT PROGRAM

END IF


    MENU "SEPARACION"
    COMMAND "Agrega" "Agrega Solicitud Aclaracion o Correccion nss"
       CALL agrega() #a
       #CALL inicializa()
    COMMAND "Consulta" "Consulta Solicitud Aclaracion o Correccion nss "
       CALL consulta() #c

    COMMAND "Mod OP 27" "Modifica Detalle 3 Operacion 27"
       CALL consulta_o27() #co27

    COMMAND "Busqueda" "Busca afiliado"
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
--    INITIALIZE reg_1 TO NULL
--    INITIALIZE arr_1 TO NULL
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
    DEFINE vv_corr           integer  ,
           xresult           smallint ,
           xmarca_entra      smallint
    DEFINE l_indice_agrega   smallint
    DEFINE reg_2 RECORD #loc #reg_2
           tipo_reclamante   LIKE sep_det_reg_sol_reclamante.tipo_reclamante,
           reclamante_desc   CHAR(025)                                      ,
           fecha_captura     DATE                                           ,
           fecha_proceso     DATE                                           ,
           nss               CHAR(11)                                       ,
           paterno2          CHAR(40)                                       ,
           materno2          CHAR(40)                                       ,
           nombres2          CHAR(40)                                       ,
           estado            SMALLINT                                       ,
           usuario           CHAR(10)
    END RECORD

    OPEN WINDOW sepm1001 AT 2,2 WITH FORM "SEPM1001" ATTRIBUTE( BORDER)
    DISPLAY " SEPM100             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY "                            INGRESO DE SEPARACION                                     " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Esc ] Ingreso        [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " [ Ctrl-b ] Ingreso de Aclaracion y Correccion de nss " AT 2,1
    DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY "                         INGRESO DE RECLAMANTE                             " AT 8,1 ATTRIBUTE(REVERSE)

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
	         LET reg_2.tipo_reclamante      = NULL
	         LET reg_2.reclamante_desc      = NULL

            IF folio_reg = FALSE
            THEN 
              ERROR "AFILIADO INEXISTENTE ..."
              LET folio_reg    = FALSE
              LET n_seguro_reg = FALSE
              LET rfc_reg      = FALSE
              NEXT FIELD n_folio
            END IF

            DISPLAY reg_2.tipo_reclamante TO  tipo_reclamante
            DISPLAY reg_2.reclamante_desc TO  reclamante_desc         

            INPUT BY NAME reg_2.tipo_reclamante  ,
                          reg_2.paterno2         ,
                          reg_2.materno2         ,
                          reg_2.nombres2         WITHOUT DEFAULTS

                BEFORE FIELD tipo_reclamante                    
                    IF sw_2 = 0 THEN
                        LET sw_2 = 1
                        LET reg_2.fecha_captura    = HOY
                        LET reg_2.fecha_proceso    = HOY
                        LET reg_2.estado           = 00
                        LET reg_2.usuario          = c8_usuario

                        DISPLAY reg_2.fecha_captura    TO fecha_captura
                        DISPLAY reg_2.usuario          TO usuario

                    END IF

         AFTER FIELD tipo_reclamante
                 IF reg_2.tipo_reclamante IS NULL THEN
                    CALL despliega_tipo_reclamante() 
                    RETURNING reg_2.tipo_reclamante ,
                              reg_2.reclamante_desc

                   IF (reg_2.tipo_reclamante <> 1 AND 
		                 reg_2.tipo_reclamante <> 2 ) THEN
                      NEXT FIELD tipo_reclamante
                   END IF
                 ELSE
                   SELECT a.reclamante_desc
                   INTO   reg_2.reclamante_desc
                   FROM   sep_tipo_reclamante a
                   WHERE  a.tipo_reclamante = reg_2.tipo_reclamante

                   IF STATUS = NOTFOUND THEN
                      ERROR " TIPO RECLAMANTE INEXISTENTE "
                      NEXT FIELD tipo_reclamante
                  END IF
                END IF

                DISPLAY BY NAME reg_2.tipo_reclamante
                DISPLAY BY NAME reg_2.reclamante_desc

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

                ON KEY (INTERRUPT)
                    INITIALIZE reg_2.* TO NULL
                    DISPLAY reg_2.tipo_reclamante  TO tipo_reclamante
                    DISPLAY reg_2.reclamante_desc  TO reclamante_desc
                    DISPLAY reg_2.fecha_captura    TO fecha_captura
                    DISPLAY reg_2.paterno2         TO paterno2
                    DISPLAY reg_2.materno2         TO materno2
                    DISPLAY reg_2.nombres2         TO nombres2
                    DISPLAY reg_2.usuario          TO usuario
                    EXIT INPUT

              ON KEY(CONTROL-E)
	             	

              ON KEY (ESC)

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
                     NEXT FIELD tipo_reclamante
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

                    IF reg_2.tipo_reclamante IS NULL THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD tipo_reclamante
                    END IF

                    IF reg_2.fecha_captura IS NULL THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD fecha_captura
                    END IF

                    SELECT "OK"
                    FROM   sep_det_reg_sol_reclamante a
                    WHERE  a.n_seguro       = reg_1.n_seguro  
                    AND    a.tipo_solicitud = reg_1.tipo_solicitud
                    AND    a.paterno        = reg_2.paterno2
                    AND    a.materno        = reg_2.materno2
                    AND    a.nombres        = reg_2.nombres2
                    AND    a.estado         in (0,1,2,21,2,3,4)
                    AND    a.folio          IS NULL
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        LET enter = "S"
                    ELSE
                        ERROR "SOLICITUD DUPLICADA" ATTRIBUTE(REVERSE)
                        LET enter = "N"
                        NEXT FIELD tipo_reclamante
                    END IF

                    IF enter = "S" THEN
                       IF reg_2.paterno2 is null then
                          LET reg_2.paterno2 = " "
                       END IF

                       IF reg_2.materno2 is null then
                          LET reg_2.materno2 = " "
                       END IF

                       IF reg_2.nombres2 is null then
                          LET reg_2.nombres2 = " "
                       END IF

                        INSERT INTO sep_det_reg_sol_reclamante
                               VALUES("",
                                      reg_1.n_folio          ,
                                      reg_1.tipo_solicitud   ,
                                      reg_1.n_seguro         ,
                                      reg_2.tipo_reclamante  ,
                                      reg_2.nss              ,   
                                      reg_2.paterno2         ,
                                      reg_2.materno2         ,
                                      reg_2.nombres2         ,
                                      0                      ,#correlativo   
                                      reg_2.fecha_captura    ,
                                      reg_2.fecha_proceso    ,
                                      0                      ,#estado
                                      reg_2.usuario          
                                     )
            SELECT a.correlativo
            INTO   vv_corr
            FROM   sep_det_reg_sol_reclamante a
            WHERE  a.n_seguro = reg_1.n_seguro 
            AND    a.fecha_captura = reg_2.fecha_captura
            AND    a.fecha_proceso = reg_2.fecha_proceso
            AND    a.estado = 0

            CALL f_marca_cuenta(reg_1.n_seguro ,vv_corr) 
            RETURNING xmarca_entra,xresult

            IF xresult = 0 THEN 

               UPDATE sep_det_reg_sol_reclamante
               SET estado = 21
               WHERE  correlativo = vv_corr 
             ERROR ""
             DISPLAY "REG MARCADO E INSERTADO...","" AT 19,2 ATTRIBUTE(REVERSE)
               SLEEP 3  
               DISPLAY "               ","" AT 19,2 
            ELSE 
             DISPLAY "CAPTURA NO PROCEDE...","" AT 19,2 ATTRIBUTE(REVERSE)
               SLEEP 3  
               DISPLAY "               ","" AT 19,2 
            END IF
          ELSE
               DISPLAY "ABORTANDO CAPTURA","" AT 19,2 ATTRIBUTE(REVERSE)
               SLEEP 3  
               DISPLAY "                ","" AT 19,2 
          END IF

                    LET sw_2 = 0
                    INITIALIZE reg_2.*  TO NULL
                    DISPLAY reg_2.tipo_reclamante  TO tipo_reclamante
                    DISPLAY reg_2.reclamante_desc  TO reclamante_desc
                    DISPLAY reg_2.fecha_captura    TO fecha_captura
                    DISPLAY reg_2.paterno2         TO paterno2
                    DISPLAY reg_2.materno2         TO materno2
                    DISPLAY reg_2.nombres2         TO nombres2
                    DISPLAY reg_2.usuario          TO usuario
                    NEXT FIELD tipo_reclamante
            END INPUT
     NEXT FIELD n_folio
  END INPUT
CLOSE WINDOW sepm1001
END FUNCTION

FUNCTION consulta()
#c-----------------
DEFINE arr_1  ARRAY[100] OF RECORD #glo #arr_1
  tipo_reclamante       LIKE sep_det_reg_sol_reclamante.tipo_reclamante   ,
  reclamante_desc       LIKE sep_tipo_reclamante.reclamante_desc          ,
  fecha_captura         LIKE sep_det_reg_sol_reclamante.fecha_captura     ,
  folio                 LIKE sep_det_solicitud.folio                      ,
  fecha_marca_infosar   LIKE sep_det_solicitud.fecha_marca_infosar        ,
  nss                   LIKE sep_det_reg_sol_reclamante.nss               ,
  paterno2              LIKE sep_det_reg_sol_reclamante.paterno           ,
  materno2              LIKE sep_det_reg_sol_reclamante.materno           ,
  nombres2              LIKE sep_det_reg_sol_reclamante.nombres           ,
  diag_confronta        LIKE sep_det_solicitud.diag_confronta             ,
  des_diagnostico       LIKE sep_diagnostico.des_diagnostico              ,
  clasifica_separacion  LIKE sep_det_solicitud.clasifica_separacion       ,
  des_clasificacion     LIKE sep_clasificacion.des_clasificacion          ,
  credito_infonavit     LIKE sep_det_solicitud.credito_infonavit          ,
  traspaso_previo       LIKE sep_det_solicitud.traspaso_previo            ,
  estado                LIKE sep_det_reg_sol_reclamante.estado            ,
  des_estado            CHAR(20)                                          ,
  correlativo           LIKE sep_det_reg_sol_reclamante.correlativo       ,
  usuario               LIKE sep_det_reg_sol_reclamante.usuario           ,
  tipo_entidad_nss_involucrado char(02)                                   ,
  clave_entidad_involucrado    char(003) ,
  resultado_operacion          char(02),
  diag_proc1                   char(003),
  resultado_operacion03        char(02),
  diag_proc103                 char(03)
END RECORD

DEFINE arr_1_c  ARRAY[100] OF RECORD #glo #arr_1
  c1                    smallint ,
  c2                    smallint ,
  tipo_reclamante       LIKE sep_det_reg_sol_reclamante.tipo_reclamante   ,
  reclamante_desc       LIKE sep_tipo_reclamante.reclamante_desc          ,
  fecha_captura         LIKE sep_det_reg_sol_reclamante.fecha_captura     ,
  folio                 LIKE sep_det_solicitud.folio                      ,
  fecha_marca_infosar   LIKE sep_det_solicitud.fecha_marca_infosar        ,
  nss                   LIKE sep_det_reg_sol_reclamante.nss               ,
  paterno2              LIKE sep_det_reg_sol_reclamante.paterno           ,
  materno2              LIKE sep_det_reg_sol_reclamante.materno           ,
  nombres2              LIKE sep_det_reg_sol_reclamante.nombres           ,
  diag_confronta        LIKE sep_det_solicitud.diag_confronta    ,
  des_diagnostico       LIKE sep_diagnostico.des_diagnostico              ,
  clasifica_separacion  LIKE sep_det_solicitud.clasifica_separacion,
  des_clasificacion     LIKE sep_clasificacion.des_clasificacion          ,
  credito_infonavit     LIKE sep_det_solicitud.credito_infonavit   ,
  traspaso_previo       LIKE sep_det_solicitud.traspaso_previo     ,
  estado                LIKE sep_det_reg_sol_reclamante.estado              ,
  des_estado            CHAR(20)                                            ,
  correlativo           LIKE sep_det_reg_sol_reclamante.correlativo         ,
  usuario               LIKE sep_det_reg_sol_reclamante.usuario        ,    
  tipo_entidad_nss_involucrado char(02)                                   ,
  clave_entidad_involucrado    char(003) ,
  resultado_operacion          char(02),
  diag_proc1                   char(003),
  resultado_operacion03        char(02),
  diag_proc103                 char(03)
END RECORD

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


    OPEN WINDOW sepm1002 AT 2,2 WITH FORM "SEPM1002" ATTRIBUTE( BORDER)
    DISPLAY " SEPM100             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
  IF valida_modulo = 1 THEN
    DISPLAY "[Ctrl-c]Salir                              "
            AT 1,1 ATTRIBUTE(BOLD)
  ELSE 
    DISPLAY " [Ctrl-c]Salir [Ctrl-v]Confirmar [Ctrl-o]Reenviar  "
            AT 1,1 ATTRIBUTE(BOLD)

--    DISPLAY " [F4]Documentos [Ctrl-p]Eliminar "
--            AT 2,1 ATTRIBUTE(BOLD)
    DISPLAY " [Ctrl-p]Eliminar "
            AT 2,1 ATTRIBUTE(BOLD)
  END IF

    DISPLAY "                    SOL ACLARACION - CORRECCION NSS                                " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_1 = 0

    WHILE TRUE

WHENEVER ERROR CONTINUE
DROP TABLE ver_consulta
WHENEVER ERROR STOP


    DISPLAY "                    SOL ACLARACION - CORRECCION NSS                                " AT 8,1 ATTRIBUTE(REVERSE)
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

    DECLARE cur_1 CURSOR FOR 
    SELECT
       a.tipo_reclamante      ,
       d.reclamante_desc      ,
       a.fecha_captura        ,
       a.folio                ,
       b.fecha_marca_infosar  ,
       a.nss                  ,
       a.paterno              ,
       a.materno              ,
       a.nombres              ,
       b.diag_confronta       ,
       " "                    ,
       b.clasifica_separacion ,
       " "                    ,
       b.credito_infonavit    ,
       b.traspaso_previo      ,
       a.estado               ,
       e.des_estado           ,
       a.correlativo          ,
       a.usuario              ,
       c.tipo_entidad_nss_involucrado,
       c.clave_entidad_involucrado,
       b.resultado_operacion ,
       b.diag_proc1          ,
       c.resultado_operacion ,
       c.diag_proc1 
FROM   sep_det_reg_sol_reclamante a ,
       OUTER (sep_det_solicitud    b ,
       OUTER sep_det03_solicitud        c) ,
       sep_tipo_reclamante        d ,
       sep_estado_separacion      e
WHERE  a.n_seguro          = reg_1.n_seguro
AND    a.correlativo       = b.idSolicitudSeparacion
AND    b.idSolicitudSeparacion = c.idSolicitudSeparacion
AND    a.tipo_reclamante   = d.tipo_reclamante 
AND    a.estado            = e.estado

    SELECT count(*)
    INTO   cuantos
    FROM   sep_det_reg_sol_reclamante a
    WHERE  a.n_seguro       = reg_1.n_seguro
    AND    a.tipo_solicitud = reg_1.tipo_solicitud

    DISPLAY "  Reg  : "  AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "/"  AT 8,12 ATTRIBUTE(REVERSE)

    LET i = 1

    FOREACH cur_1 INTO arr_1[i].*

        SELECT a.des_diagnostico,b.des_clasificacion
        INTO   arr_1[i].des_diagnostico,arr_1[i].des_clasificacion
        FROM   sep_diagnostico a ,
               OUTER sep_clasificacion  b
        WHERE  a.diagnostico = arr_1[i].diag_confronta
        AND    a.diagnostico = b.diagnostico
        AND    b.clasificacion = arr_1[i].clasifica_separacion

        LET arr_cu[i].c1 = i
        LET arr_cu[i].c2 = cuantos
        LET i = i + 1
    END FOREACH

IF i = 1 THEN
   CALL despliega_mens(reg_1.n_seguro,"Solicitud inexistente para el nss:")
ELSE

FOR u = 1 TO i
       LET arr_1_c[u].c1               = arr_cu[u].c1
       LET arr_1_c[u].c2               = arr_cu[u].c2
       LET arr_1_c[u].tipo_reclamante  = arr_1[u].tipo_reclamante
       LET arr_1_c[u].reclamante_desc  = arr_1[u].reclamante_desc
       LET arr_1_c[u].fecha_captura    = arr_1[u].fecha_captura
      IF (arr_1[u].estado = 55 or 
          arr_1[u].estado = 8 or
          arr_1[u].estado = 9 or
          arr_1[u].estado = 10 or
          arr_1[u].estado = 11) then
       LET arr_1_c[u].folio            = arr_1[u].folio
      else 
       LET arr_1_c[u].folio            =  " "
      end if 
       LET arr_1_c[u].fecha_marca_infosar  = arr_1[u].fecha_marca_infosar
       LET arr_1_c[u].nss                  = arr_1[u].nss
       LET arr_1_c[u].paterno2             = arr_1[u].paterno2
       LET arr_1_c[u].materno2             = arr_1[u].materno2
       LET arr_1_c[u].nombres2             = arr_1[u].nombres2
       LET arr_1_c[u].diag_confronta       = arr_1[u].diag_confronta
       LET arr_1_c[u].des_diagnostico    = arr_1[u].des_diagnostico
       LET arr_1_c[u].clasifica_separacion  = arr_1[u].clasifica_separacion
       LET arr_1_c[u].des_clasificacion  = arr_1[u].des_clasificacion
       LET arr_1_c[u].credito_infonavit     = arr_1[u].credito_infonavit
       LET arr_1_c[u].traspaso_previo       = arr_1[u].traspaso_previo
       LET arr_1_c[u].estado                = arr_1[u].estado
       LET arr_1_c[u].des_estado            = arr_1[u].des_estado
       LET arr_1_c[u].correlativo           = arr_1[u].correlativo
       LET arr_1_c[u].usuario               = arr_1[u].usuario  
       LET arr_1_c[u].tipo_entidad_nss_involucrado = 
                           arr_1[u].tipo_entidad_nss_involucrado
       LET arr_1_c[u].clave_entidad_involucrado = 
                           arr_1[u].clave_entidad_involucrado
       LET arr_1_c[u].resultado_operacion = 
                           arr_1[u].resultado_operacion
       LET arr_1_c[u].diag_proc1 = 
                           arr_1[u].diag_proc1
       LET arr_1_c[u].resultado_operacion03 = 
                           arr_1[u].resultado_operacion03
       LET arr_1_c[u].diag_proc103 = 
                           arr_1[u].diag_proc103

END FOR

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_1_c TO scr_1.*

    ON KEY ( INTERRUPT )
	     IF valida_modulo = 1  THEN
	        EXIT PROGRAM
	     END IF

         INITIALIZE reg_1 TO NULL
         INITIALIZE arr_1 TO NULL
         INITIALIZE arr_m TO NULL
         LET  x = 0
         CLEAR FORM
        EXIT DISPLAY

    ON KEY(CONTROL-O)

       --LET g = ARR_CURR()
       --LET band_conf = 0
       --CALL SEPB012 (reg_1.n_seguro)     
       --RETURNING     band_conf
   IF valida_modulo THEN
     LET g = ARR_CURR()
   ELSE
     LET g = ARR_CURR()
     IF (arr_1_c[g].estado = 6  OR 
         arr_1_c[g].estado = 5) THEN 
        CALL captura_nss_reclamante()
        RETURNING arr_1_c[g].nss
 
      
        IF (arr_1_c[g].nss IS NOT NULL AND 
            arr_1_c[g].nss <> " " ) THEN
        DISPLAY BY NAME arr_1_c[g].nss 
        END IF

    IF arr_1_c[g].nss <> " " THEN

      WHILE TRUE
           PROMPT "DESEA CONFIRMAR SOLICITUD S/N ? " FOR CHAR enter
           IF enter MATCHES "[sSnN]" THEN
              IF enter MATCHES "[sS]" THEN
                  UPDATE sep_det_reg_sol_reclamante  
                  SET    sep_det_reg_sol_reclamante.estado         = 21,
                         sep_det_reg_sol_reclamante.nss            = 
                         arr_1_c[g].nss,
			                sep_det_reg_sol_reclamante.fecha_proceso  = TODAY
                  WHERE  sep_det_reg_sol_reclamante.correlativo =
                         arr_1_c[g].correlativo

                        LET arr_1_c[g].estado = 21

                        SELECT a.des_estado
                        INTO   arr_1_c[g].des_estado
                        FROM   sep_estado_separacion  a
                        WHERE  a.estado = arr_1_c[g].estado

                     ERROR"REGISTRO MODIFICADO PARA REENVIO..FALTA CALIFICAR..."
                        SLEEP 2
                        ERROR" " 
                               
                        DISPLAY arr_1_c[g].estado  TO scr_1.estado
                        DISPLAY arr_1_c[g].des_estado TO scr_1.des_estado
                      --DISPLAY arr_1_c[g].fecha_proceso TO scr_1.fecha_proceso

                        EXIT WHILE
                    ELSE
                   --  LET arr_1_c[g].nss = " "
                   --  DISPLAY BY NAME arr_1_c[g].nss 
                       ERROR"ALTA CANCELADA" 
                       SLEEP 2
                       ERROR" "
                       EXIT WHILE
                    END IF
                   END IF
                 END WHILE
           END IF
          ELSE 
                 ERROR"ESTADO NO MODIFICABLE"
                 SLEEP 2 
                 ERROR" "
          END IF 
      END IF
    ON KEY(CONTROL-V)
    IF valida_modulo = 1 THEN
     LET g = ARR_CURR()
    ELSE
     LET g = ARR_CURR()
     IF ( arr_1_c[g].estado = 2  OR  
          arr_1_c[g].estado = 1 ) THEN 
     -- CALL SEPB012 (reg_1.n_seguro)
     -- RETURNING     band_conf 

        CALL captura_nss_reclamante()
        RETURNING arr_1_c[g].nss

        DISPLAY BY NAME arr_1_c[g].nss 

      IF band_conf = 1 THEN
         ERROR "DOCUMENTACION INCOMPLETA..." 
         SLEEP 2
      END IF

    IF arr_1_c[g].nss <> " " THEN

      WHILE TRUE
           PROMPT "DESEA CONFIRMAR SOLICITUD S/N ? " FOR CHAR enter
           IF enter MATCHES "[sSnN]" THEN
              IF enter MATCHES "[sS]" THEN
                  UPDATE sep_det_reg_sol_reclamante  
                  SET    sep_det_reg_sol_reclamante.estado         = 1,
                         sep_det_reg_sol_reclamante.nss            = 
                         arr_1_c[g].nss,
               sep_det_reg_sol_reclamante.fecha_proceso  = TODAY
                  WHERE  sep_det_reg_sol_reclamante.correlativo =
                         arr_1_c[g].correlativo

                        LET arr_1_c[g].estado = 1 

                        SELECT a.des_estado
                        INTO   arr_1_c[g].des_estado
                        FROM   sep_estado_separacion  a
                        WHERE  a.estado = arr_1_c[g].estado

                        ERROR"REGISTRO CONFIRMADO..."
                        SLEEP 2
                        ERROR" " 
                               
                        DISPLAY arr_1_c[g].estado  TO scr_1.estado
                        DISPLAY arr_1_c[g].des_estado TO scr_1.des_estado
                      --DISPLAY arr_1_c[g].fecha_proceso TO scr_1.fecha_proceso

                        EXIT WHILE
                    ELSE
                     LET arr_1_c[g].nss = " "
                     DISPLAY BY NAME arr_1_c[g].nss 
                     ERROR"ALTA CANCELADA" 
                     SLEEP 2
                     ERROR" "
                     EXIT WHILE
                    END IF
                   END IF
                 END WHILE
           END IF
          ELSE 
                 ERROR"ESTADO NO MODIFICABLE"
                 SLEEP 2 
                 ERROR" "
          END IF 
        END IF
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
            AND arr_1_c[i].estado <> 0 THEN

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
              DELETE FROM sep_det_reg_sol_reclamante
              WHERE correlativo = e_correlativo
               ERROR "ICEFA ELIMINADA..." SLEEP 2
               ERROR ""
                INITIALIZE reg_1.* TO NULL
                DISPLAY BY NAME reg_1.*
            ELSE 
               ERROR "ELIMINACION CANCELADA..." SLEEP 2
               ERROR ""
                INITIALIZE reg_1.* TO NULL
                DISPLAY BY NAME reg_1.*
            END IF
         END IF
               CLEAR FORM
        EXIT DISPLAY
	 END IF
    END DISPLAY
    END IF
    END WHILE
    CLOSE WINDOW sepm1002

END FUNCTION

FUNCTION consulta_o27()
#c---------------------
DEFINE arr_1  ARRAY[100] OF RECORD #glo #arr_1
  tipo_reclamante       LIKE sep_det_reg_sol_reclamante.tipo_reclamante   ,
  reclamante_desc       LIKE sep_tipo_reclamante.reclamante_desc          ,
  fecha_captura         LIKE sep_det_reg_sol_reclamante.fecha_captura     ,
  folio                 LIKE sep_det_solicitud.folio                      ,
  fecha_marca_infosar   LIKE sep_det_solicitud.fecha_marca_infosar        ,
  nss                   LIKE sep_det_reg_sol_reclamante.nss               ,
  paterno2              LIKE sep_det_reg_sol_reclamante.paterno           ,
  materno2              LIKE sep_det_reg_sol_reclamante.materno           ,
  nombres2              LIKE sep_det_reg_sol_reclamante.nombres           ,
  diag_confronta        LIKE sep_det_solicitud.diag_confronta             ,
  des_diagnostico       LIKE sep_diagnostico.des_diagnostico              ,
  clasifica_separacion  LIKE sep_det_solicitud.clasifica_separacion       ,
  des_clasificacion     LIKE sep_clasificacion.des_clasificacion          ,
  credito_infonavit     LIKE sep_det_solicitud.credito_infonavit          ,
  traspaso_previo       LIKE sep_det_solicitud.traspaso_previo            ,
  estado                LIKE sep_det_reg_sol_reclamante.estado            ,
  des_estado            CHAR(20)                                          ,
  correlativo           LIKE sep_det_reg_sol_reclamante.correlativo       ,
  usuario               LIKE sep_det_reg_sol_reclamante.usuario           , 
  tipo                  LIKE sep_det03_solicitud.tipo_entidad_nss_involucrado ,
  desc_tipo             CHAR(12)                                          ,
  cve_entidad           LIKE sep_det03_solicitud.clave_entidad_involucrado ,
  desc_cve_ent          CHAR(15)               ,
  vrow            INTEGER 
END RECORD

DEFINE arr_1_c  ARRAY[100] OF RECORD #glo #arr_1
  c1                    smallint ,
  c2                    smallint ,
  tipo_reclamante       LIKE sep_det_reg_sol_reclamante.tipo_reclamante   ,
  reclamante_desc       LIKE sep_tipo_reclamante.reclamante_desc          ,
  fecha_captura         LIKE sep_det_reg_sol_reclamante.fecha_captura     ,
  folio                 LIKE sep_det_solicitud.folio                      ,
  fecha_marca_infosar   LIKE sep_det_solicitud.fecha_marca_infosar        ,
  nss                   LIKE sep_det_reg_sol_reclamante.nss               ,
  paterno2              LIKE sep_det_reg_sol_reclamante.paterno           ,
  materno2              LIKE sep_det_reg_sol_reclamante.materno           ,
  nombres2              LIKE sep_det_reg_sol_reclamante.nombres           ,
  diag_confronta        LIKE sep_det_solicitud.diag_confronta    ,
  des_diagnostico       LIKE sep_diagnostico.des_diagnostico              ,
  clasifica_separacion  LIKE sep_det_solicitud.clasifica_separacion,
  des_clasificacion     LIKE sep_clasificacion.des_clasificacion          ,
  credito_infonavit     LIKE sep_det_solicitud.credito_infonavit   ,
  traspaso_previo       LIKE sep_det_solicitud.traspaso_previo     ,
  estado                LIKE sep_det_reg_sol_reclamante.estado              ,
  des_estado            CHAR(20)                                            ,
  correlativo           LIKE sep_det_reg_sol_reclamante.correlativo         ,
  usuario               LIKE sep_det_reg_sol_reclamante.usuario             ,
  tipo                  LIKE sep_det03_solicitud.tipo_entidad_nss_involucrado ,
  desc_tipo             CHAR(30)                                          ,
  cve_entidad           LIKE sep_det03_solicitud.clave_entidad_involucrado ,
  desc_cve_ent          CHAR(30)   ,             
  vrow            INTEGER 
END RECORD

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

    DEFINE actualiza SMALLINT ,
           act_tipo  CHAR(002),
           act_cve_entidad CHAR(003)

    OPEN WINDOW sepm1002 AT 2,2 WITH FORM "SEPM10021" ATTRIBUTE( BORDER)
    DISPLAY " SEPM100             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1

    DISPLAY "[Ctrl-c]Salir [Ctrl-n]Modificar Tipo Entidad"
            AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY "                    SOL ACLARACION - CORRECCION NSS                                " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_1 = 0

    WHILE TRUE

WHENEVER ERROR CONTINUE
DROP TABLE ver_consulta
WHENEVER ERROR STOP


    DISPLAY "                    SOL ACLARACION - CORRECCION NSS                                " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)


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

    IF sw_1 = 1 THEN
        EXIT WHILE
    END IF

    DECLARE cur_1_o27 CURSOR 
      FOR SELECT
       a.tipo_reclamante      ,
       d.reclamante_desc      ,
       a.fecha_captura        ,
       a.folio                ,
       b.fecha_marca_infosar  ,
       a.nss                  ,
       a.paterno              ,
       a.materno              ,
       a.nombres              ,
       b.diag_confronta       ,
       " "                    ,
       b.clasifica_separacion ,
       " "                    ,
       b.credito_infonavit    ,
       b.traspaso_previo      ,
       a.estado               ,
       e.des_estado           ,
       a.correlativo          ,
       a.usuario              ,
       c.tipo_entidad_nss_involucrado ,
       " "    ,
       c.clave_entidad_involucrado  ,
       " ",
       c.rowid
FROM   sep_det_reg_sol_reclamante a ,
       OUTER (sep_det_solicitud    b ,
       OUTER sep_det03_solicitud        c) ,
       sep_tipo_reclamante        d ,
       sep_estado_separacion      e
WHERE  a.n_seguro          = reg_1.n_seguro
AND    a.correlativo    = b.idSolicitudSeparacion
AND    b.idSolicitudSeparacion = c.idSolicitudSeparacion
AND    a.nss               = c.nss_asociado
AND    a.tipo_reclamante   = d.tipo_reclamante 
AND    a.estado            = e.estado

    SELECT count(*)
    INTO   cuantos
    FROM   sep_det_reg_sol_reclamante a
    WHERE  a.n_seguro       = reg_1.n_seguro
    AND    a.tipo_solicitud = reg_1.tipo_solicitud


    DISPLAY "  Reg  : "  AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "/"  AT 8,12 ATTRIBUTE(REVERSE)

    LET i = 1

    FOREACH cur_1_o27 INTO arr_1[i].*

        SELECT a.des_diagnostico,b.des_clasificacion
        INTO   arr_1[i].des_diagnostico,arr_1[i].des_clasificacion
        FROM   sep_diagnostico a ,
               OUTER sep_clasificacion  b
        WHERE  a.diagnostico = arr_1[i].diag_confronta
        AND    a.diagnostico = b.diagnostico
        AND    b.clasificacion = arr_1[i].clasifica_separacion

        SELECT a.afore_desc 
        INTO   arr_1[i].desc_cve_ent 
        FROM   tab_afore a 
        WHERE  a.afore_cod = arr_1[i].cve_entidad

        CASE arr_1[i].tipo 
         WHEN "00" LET arr_1[i].desc_tipo = "SIN ENTIDAD REGISTRADA"
         WHEN "01" LET arr_1[i].desc_tipo = "AFORE"
         WHEN "59" LET arr_1[i].desc_tipo = "CUENTA ASIGNADA"
        END CASE 
        LET arr_cu[i].c1 = i
        LET arr_cu[i].c2 = cuantos
        LET i = i + 1
    END FOREACH

IF i = 1 THEN
   CALL despliega_mens(reg_1.n_seguro,"Solicitud inexistente para el nss:")
ELSE

FOR u = 1 TO i
       LET arr_1_c[u].c1               = arr_cu[u].c1
       LET arr_1_c[u].c2               = arr_cu[u].c2
       LET arr_1_c[u].tipo_reclamante  = arr_1[u].tipo_reclamante
       LET arr_1_c[u].reclamante_desc  = arr_1[u].reclamante_desc
       LET arr_1_c[u].fecha_captura    = arr_1[u].fecha_captura
      IF (arr_1[u].estado = 55 or 
          arr_1[u].estado = 8 or
          arr_1[u].estado = 9 or
          arr_1[u].estado = 10 or
          arr_1[u].estado = 11) then
       LET arr_1_c[u].folio            = arr_1[u].folio
      else 
       LET arr_1_c[u].folio            =  " "
      end if 
       LET arr_1_c[u].fecha_marca_infosar  = arr_1[u].fecha_marca_infosar
       LET arr_1_c[u].nss                  = arr_1[u].nss
       LET arr_1_c[u].paterno2             = arr_1[u].paterno2
       LET arr_1_c[u].materno2             = arr_1[u].materno2
       LET arr_1_c[u].nombres2             = arr_1[u].nombres2
       LET arr_1_c[u].diag_confronta       = arr_1[u].diag_confronta
       LET arr_1_c[u].des_diagnostico    = arr_1[u].des_diagnostico
       LET arr_1_c[u].clasifica_separacion  = arr_1[u].clasifica_separacion
       LET arr_1_c[u].des_clasificacion  = arr_1[u].des_clasificacion
       LET arr_1_c[u].credito_infonavit     = arr_1[u].credito_infonavit
       LET arr_1_c[u].traspaso_previo       = arr_1[u].traspaso_previo
       LET arr_1_c[u].estado                = arr_1[u].estado
       LET arr_1_c[u].des_estado            = arr_1[u].des_estado
       LET arr_1_c[u].correlativo           = arr_1[u].correlativo
       LET arr_1_c[u].usuario               = arr_1[u].usuario  
       LET arr_1_c[u].tipo                  = arr_1[u].tipo 
       LET arr_1_c[u].desc_tipo             = arr_1[u].desc_tipo 
       LET arr_1_c[u].cve_entidad           = arr_1[u].cve_entidad
       LET arr_1_c[u].desc_cve_ent          = arr_1[u].desc_cve_ent
       LET arr_1_c[u].vrow                  = arr_1[u].vrow
END FOR

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_1_c TO scr_1.*

    ON KEY ( INTERRUPT )
	     IF valida_modulo = 1  THEN
	        EXIT PROGRAM
	     END IF

         INITIALIZE reg_1 TO NULL
         INITIALIZE arr_1 TO NULL
         INITIALIZE arr_m TO NULL
         LET  x = 0
         CLEAR FORM
        EXIT DISPLAY

   ON KEY(CONTROL-N)
     LET g = ARR_CURR()

        CALL modifica_tipo_ent(arr_1_c[g].correlativo,
                               reg_1.n_seguro ,
                               arr_1_c[g].nss,
                               arr_1_c[g].tipo ,
                               arr_1_c[g].cve_entidad,
                               arr_1_c[g].vrow)
        RETURNING actualiza ,act_tipo, act_cve_entidad

        IF actualiza = 1 THEN 
           LET arr_1_c[g].tipo = act_tipo 
           LET arr_1_c[g].cve_entidad = act_cve_entidad
     
        SELECT a.afore_desc 
        INTO   arr_1_c[g].desc_cve_ent 
        FROM   tab_afore a 
        WHERE  a.afore_cod = arr_1_c[g].cve_entidad

        IF STATUS = NOTFOUND THEN LET arr_1_c[g].desc_cve_ent = " " END IF

        CASE arr_1_c[g].tipo 
         WHEN "00" LET arr_1_c[g].desc_tipo = "SIN ENTIDAD REGISTRADA"
         WHEN "01" LET arr_1_c[g].desc_tipo = "AFORE"
         WHEN "59" LET arr_1_c[g].desc_tipo = "CUENTA ASIGNADA"
        END CASE 

        DISPLAY arr_1_c[g].tipo TO tipo_entidad_nss_involucrado
        DISPLAY BY NAME arr_1_c[g].desc_tipo
        DISPLAY arr_1_c[g].cve_entidad TO clave_entidad_involucrado
        DISPLAY BY NAME arr_1_c[g].desc_cve_ent

       END IF

    END DISPLAY
    END IF
    END WHILE
    CLOSE WINDOW sepm1002

END FUNCTION

FUNCTION despliega_tipo_reclamante()
#di---------------------------------

 DEFINE aux_val  SMALLINT
 DEFINE l_reg ARRAY[1000] OF RECORD
        codigo  INTEGER,
        descripcion CHAR(50)
 END RECORD
 DEFINE x_x  char(100),
 x_buscar  char(30)
 DEFINE pos  SMALLINT

 OPEN WINDOW vent_1 AT 05,12 WITH FORM "SEPM1010" ATTRIBUTE(BORDER)
 DISPLAY "                 TIPO RECLAMANTE                    " AT 2,1 ATTRIBUTE(REVERSE)
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
       LET x_x = " SELECT * FROM sep_tipo_reclamante ",
                 " WHERE reclamante_desc MATCHES ",'"',x_buscar CLIPPED,'"',
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
          ERROR "ARCHIVO TIPO RECLA... VACIO"
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

    OPEN WINDOW tram1004 AT 05,12 WITH FORM "SEPM1004" ATTRIBUTE(BORDER)
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
close window tram1004
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
    CLOSE WINDOW tram1004
if pos <> 0  then
    RETURN l_reg[pos].n_folio ,l_reg[pos].tipo_solicitud
else 
    return "",""
end if
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

FUNCTION f_marca_cuenta(vnss,dd_corr)
#--marca la cuenta en proceso de separación de cuentas
-----------------------------------------------------

DEFINE vnss     char(011)
DEFINE dd_corr  integer
DEFINE vmarcaentra smallint
DEFINE lmarca char(100)
DEFINE vestadomarca smallint
DEFINE vrechazocod  smallint
DEFINE vmarcacausa smallint
DEFINE vfechacausa date
DEFINE vusuario char(010)
DEFINE xmarca smallint
DEFINE xrechazo smallint

LET vestadomarca = 0
LET vmarcaentra = 280
LET vrechazocod = 0
LET vmarcacausa = 0
LET vfechacausa = ""

SELECT USER
INTO   vusuario
FROM   tab_afore_local
group by 1

LET lmarca = "EXECUTE PROCEDURE marca_cuenta(?,?,?,?,?,?,?,?)"

PREPARE marcaje FROM lmarca

DECLARE cur_marca CURSOR FOR marcaje

FOREACH cur_marca USING vnss        ,
                        vmarcaentra ,
                        dd_corr     ,
                        vestadomarca ,
                        vrechazocod ,
                        vmarcacausa ,
                        vfechacausa ,
                        vusuario
                  INTO xmarca    ,
                       xrechazo
END FOREACH

if xrechazo <> 0 THEN
   ERROR "Marca 280 rechazada por convivencia : ",xrechazo clipped
   sleep 2
else
   ERROR "Marca 280 procedente ..."
   sleep 2
end if

return xmarca ,xrechazo

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

  OPEN WINDOW tram10020 AT 12,20 WITH FORM "SEPM10020" ATTRIBUTE(BORDER)
    DISPLAY  cadena AT  4,3
    DISPLAY BY NAME vnss ATTRIBUTE(REVERSE)
    SLEEP 4
    CLOSE WINDOW tram10020
END FUNCTION

FUNCTION captura_nss_reclamante()
#cnr---------------------------

DEFINE vnss  char(011)

  OPEN WINDOW sepb0123 AT 12,20 WITH FORM "SEPB0123" ATTRIBUTE(BORDER)

       INPUT BY NAME vnss WITHOUT DEFAULTS
       AFTER FIELD   vnss  
       ON KEY (ESC)
                 WHILE TRUE
                   PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
                   IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                       EXIT WHILE
                    ELSE
                     LET vnss = NULL
                     DISPLAY BY NAME vnss
                     NEXT FIELD vnss
                    END IF
                   END IF
                 END WHILE

              SELECT "OK" 
              FROM   sep_det_reg_sol_reclamante  A
              WHERE  A.n_seguro = reg_1.n_seguro 
              AND    A.nss      = vnss
              AND    A.folio    IS NULL

              IF STATUS <> NOTFOUND THEN
                 ERROR "Registro Duplicado..." 
                 SLEEP 2
                 ERROR "" 
                  NEXT FIELD vnss
              END IF 
              
        EXIT INPUT
      ON KEY (INTERRUPT)
                  LET vnss = " "
                  DISPLAY BY NAME vnss
                  EXIT INPUT
       END INPUT
    CLOSE WINDOW sepb0123
    RETURN vnss
END FUNCTION


FUNCTION modifica_tipo_ent(correlativo,nss_separado,nss_separador,tipo,cve_entidad,vrow)
#cnr------------------------------------------------------------------------------

DEFINE nss_separado ,nss_separador CHAR(011)
DEFINE correlativo INTEGER
DEFINE tipo,tipo_original CHAR(002)
DEFINE cve_entidad,cve_entidad_original CHAR(003)
DEFINE bb ,vrow SMALLINT
DEFINE fecha DATE 
DEFINE hora CHAR(008)
DEFINE lusuario CHAR(010)
DEFINE act SMALLINT,
       nula CHAR(001)

LET tipo_original = tipo
LET cve_entidad_original = cve_entidad
LET act = 0

  OPEN WINDOW sepb01231 AT 12,20 WITH FORM "SEPB01231" ATTRIBUTE(BORDER)

       INPUT BY NAME tipo , cve_entidad WITHOUT DEFAULTS
       AFTER FIELD   tipo 
            IF tipo = "00" THEN LET cve_entidad = " " DISPLAY BY name cve_entidad NEXT FIELD tipo END IF
            IF tipo = "  " OR tipo = "" OR tipo IS NULL THEN ERROR "Tipo No Valido" NEXT FIELD tipo END IF
       AFTER FIELD cve_entidad
            IF tipo = "01" OR tipo = "59" THEN 
                 IF cve_entidad = " " OR cve_entidad IS NULL OR cve_entidad = "" THEN 
                     ERROR"Cve de Afore no puede ser nula con tipo 01 , 02"
                     NEXT FIELD tipo
                 END IF
                 SELECT "OK" 
                 FROM   tab_afore a
                 WHERE  a.afore_cod = cve_entidad 
                 IF STATUS = NOTFOUND THEN 
                    ERROR "Cve de Afore ",cve_entidad , " no existe, verificar..."
                    LET cve_entidad = " "
                    DISPLAY cve_entidad
                    NEXT FIELD cve_entidad 
                 END IF
            END IF 
       ON KEY (ESC)
                 WHILE TRUE
                   PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
                   IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                       LET bb = 1
                       EXIT WHILE
                    ELSE
                       LET bb = 0
                       EXIT WHILE
                    END IF
                   END IF
                 END WHILE
           IF bb = 1 THEN 
            IF tipo = "  " OR tipo = "" OR tipo IS NULL THEN ERROR "Tipo No Valido" NEXT FIELD tipo END IF
            IF tipo = "00" AND cve_entidad <> " " and cve_entidad <> "" and cve_entidad IS NOT NULL THEN
               ERROR"Cve de Entidad debe ser nula para tipo 00"
               NEXT FIELD tipo
            END IF
            IF tipo = "01" OR tipo = "59" THEN 
                 IF cve_entidad = " " OR cve_entidad IS NULL OR cve_entidad = "" THEN 
                     ERROR"Cve de Afore no puede ser nula con tipo 01 , 02"
                     NEXT FIELD tipo
                 END IF

                SELECT USER 
                INTO lusuario
                FROM tab_afore_local
                GROUP BY 1

                LET fecha = TODAY
                LET hora = TIME

                 SELECT "OK" 
                 FROM   tab_afore a
                 WHERE  a.afore_cod = cve_entidad 
                 IF STATUS = NOTFOUND THEN 
                    ERROR "Cve de Afore ",cve_entidad , " no existe, verificar..."
                    LET cve_entidad = " "
                    DISPLAY cve_entidad
                    NEXT FIELD cve_entidad 
                 END IF
            END IF 
               UPDATE sep_det03_solicitud 
               SET tipo_entidad_nss_involucrado = tipo ,
                   clave_entidad_involucrado    = cve_entidad   
               WHERE idSolicitudSeparacion = correlativo

               INSERT into sep_bitacora_mod_o27
               VALUES (correlativo ,
                      nss_separado,
                      nss_separador,
                      tipo_original,
                      cve_entidad_original ,
                      tipo ,
                      cve_entidad,
                      lusuario,
                      fecha,hora)
               LET act = 1 
        END IF    
        EXIT INPUT
      ON KEY (INTERRUPT)
           ERROR"Saliendo sin Actualizar.."
                  EXIT INPUT
       END INPUT
    CLOSE WINDOW sepb01231

     RETURN act ,tipo,cve_entidad

END FUNCTION
