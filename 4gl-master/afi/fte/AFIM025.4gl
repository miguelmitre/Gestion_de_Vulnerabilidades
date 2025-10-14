############################################################################
#Proyecto          => SISTEMA DE safre_af.( MEXICO )                       #
#Propietario       => E.F.P                                                #
#Programa AFIM025  => MANTENEDOR DE ICEFAS                                 #
#Fecha creacion    => 23 ENERO DE 1998                                     #
#Autor             => FRANCO ESTEBAN ULLOA VIDELA                          #
#Fecha actualiz.   => 02 ENERO 2001                                        #
#Actualizacion     => JESUS DAVID YAÑEZ MORENO                             #
#Sistema           => TRA  	                                           #
############################################################################

DATABASE safre_af
GLOBALS
    DEFINE actual_habil       SMALLINT
    DEFINE x smallint
    DEFINE reg_1 RECORD #glo #reg_1
        n_folio               LIKE afi_mae_afiliado.n_folio        ,
        tipo_solicitud        LIKE afi_mae_afiliado.tipo_solicitud ,
        n_seguro              LIKE afi_mae_afiliado.n_seguro       ,
        n_rfc                 LIKE afi_mae_afiliado.n_rfc          ,
        n_unico               LIKE afi_mae_afiliado.n_unico        ,
        fentcons              LIKE afi_mae_afiliado.fentcons      ,
        paterno               LIKE afi_mae_afiliado.paterno        ,
        materno               LIKE afi_mae_afiliado.materno        ,
        nombres               LIKE afi_mae_afiliado.nombres 
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2
        n_folio_tra           LIKE afi_icefa.n_folio_tra      ,
        nss                   LIKE afi_icefa.nss              ,
        rfc                   LIKE afi_icefa.rfc              ,
        fecha_solic_tra       LIKE afi_icefa.fecha_solic_tra  ,
        fecha_captura         LIKE afi_icefa.fecha_captura    ,
        paterno2              LIKE afi_icefa.paterno          ,
        materno2              LIKE afi_icefa.materno          ,
        nombres2              LIKE afi_icefa.nombres          ,
        icefa_cod             LIKE afi_icefa.icefa_cod        ,
        des_icefa             CHAR(20)                       ,
        nro_int_cta           LIKE afi_icefa.nro_int_cta      ,
        saldo_sar_92          LIKE afi_icefa.saldo_sar_92     ,
        saldo_viv_92          LIKE afi_icefa.saldo_viv_92     ,
        tipo_comp_icefa       LIKE afi_icefa.tipo_comp_icefa  ,
        tipcom_desc           LIKE tab_tipo_com.tipcom_desc     ,
        fecha_comp_icefa      LIKE afi_icefa.fecha_comp_icefa ,
        usuario               LIKE afi_icefa.usuario          ,
        fecha_proceso         LIKE afi_icefa.fecha_proceso    ,
        status                LIKE afi_icefa.status           ,
        des_estado            CHAR(20)                       ,
        fecha_a_liquidar      DATE                           ,
        fecha_liquidacion     DATE                           ,
        correlativo           LIKE afi_icefa.correlativo      ,
        diag_proceso          CHAR(2)                        ,
        des_diagnostico       CHAR(30)
    END RECORD

    DEFINE arr_1  ARRAY[100] OF RECORD #glo #arr_1
        n_folio_tra           LIKE afi_icefa.n_folio_tra      ,
        nss                   LIKE afi_icefa.nss              ,
        rfc                   LIKE afi_icefa.rfc              ,
        fecha_solic_tra       LIKE afi_icefa.fecha_solic_tra  ,
        fecha_captura         LIKE afi_icefa.fecha_captura    ,
        paterno2              LIKE afi_icefa.paterno          ,
        materno2              LIKE afi_icefa.materno          ,
        nombres2              LIKE afi_icefa.nombres          ,
        icefa_cod             LIKE afi_icefa.icefa_cod        ,
        des_icefa             CHAR(20)                       ,
        nro_int_cta           LIKE afi_icefa.nro_int_cta      ,
        saldo_sar_92          LIKE afi_icefa.saldo_sar_92     ,
        saldo_viv_92          LIKE afi_icefa.saldo_viv_92     ,
        tipo_comp_icefa       LIKE afi_icefa.tipo_comp_icefa  ,
        tipcom_desc           LIKE tab_tipo_com.tipcom_desc     ,
        fecha_comp_icefa      LIKE afi_icefa.fecha_comp_icefa ,
        usuario               LIKE afi_icefa.usuario          ,
        fecha_proceso         LIKE afi_icefa.fecha_proceso    ,
        estado                LIKE afi_icefa.status           ,
        des_estado            CHAR(20)                       ,
        fecha_a_liquidar      DATE                           ,
        fecha_liquidacion     DATE                           ,
        correlativo           LIKE afi_icefa.correlativo      ,
        diag_proceso          CHAR(2)                        ,
        des_diagnostico       CHAR(30)
    END RECORD

    DEFINE arr_m  ARRAY[100] OF RECORD #arr_m
        n_folio_tra           LIKE afi_icefa.n_folio_tra      ,
        nss                   LIKE afi_icefa.nss              ,
        rfc                   LIKE afi_icefa.rfc              ,
        fecha_solic_tra       LIKE afi_icefa.fecha_solic_tra  ,
        fecha_captura         LIKE afi_icefa.fecha_captura    ,
        paterno2              LIKE afi_icefa.paterno          ,
        materno2              LIKE afi_icefa.materno          ,
        nombres2              LIKE afi_icefa.nombres          ,
        icefa_cod             LIKE afi_icefa.icefa_cod        ,
        des_icefa             CHAR(20)                       ,
        nro_int_cta           LIKE afi_icefa.nro_int_cta      ,
        saldo_sar_92          LIKE afi_icefa.saldo_sar_92     ,
        saldo_viv_92          LIKE afi_icefa.saldo_viv_92     ,
        tipo_comp_icefa       LIKE afi_icefa.tipo_comp_icefa  ,
        tipcom_desc           LIKE tab_tipo_com.tipcom_desc     ,
        fecha_comp_icefa      LIKE afi_icefa.fecha_comp_icefa ,
        usuario               LIKE afi_icefa.usuario          ,
        fecha_proceso         LIKE afi_icefa.fecha_proceso    ,
        estado                LIKE afi_icefa.status           ,
        des_estado            CHAR(20)                       ,
        fecha_a_liquidar      DATE                           ,
        fecha_liquidacion     DATE                           ,
        correlativo           LIKE afi_icefa.correlativo      ,
        diag_proceso          CHAR(2)                        ,
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
        enter                 CHAR(001) ,
        enter2                CHAR(001) ,
        aux_pausa             CHAR(001) ,
	vdes_icefa            CHAR(020)

    DEFINE #date
	HOY		      DATE

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
	i    		      SMALLINT

    DEFINE arr_100 ARRAY[1000] OF RECORD #arr_100
        folio2                LIKE tra_det_rechazo.folio       ,
        n_seguro              LIKE afi_icefa.nss             ,
        cve_ced_cuenta        LIKE afi_icefa.icefa_cod       ,
        nro_ctrl_icefa        LIKE afi_icefa.nro_int_cta     ,
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
        n_seguro              LIKE afi_icefa.nss             ,
        cve_ced_cuenta        LIKE afi_icefa.icefa_cod       ,
        nro_ctrl_icefa        LIKE afi_icefa.nro_int_cta     ,
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
	e_correlativo         LIKE afi_icefa.correlativo

    DEFINE
        ACCION  CHAR(1)

END GLOBALS

MAIN
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
 	ACCEPT KEY CONTROL-I
 	DEFER INTERRUPT

    CALL STARTLOG("AFIM025.log") 
    CALL init()
    OPEN WINDOW tram0011 AT 3,2 WITH FORM "AFIM0251" ATTRIBUTE( BORDER)
    DISPLAY " AFIM025             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY "                            INGRESO DE ICEFAS                              " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    CALL inicializa()

    CASE
        WHEN ACCION = 'A' OR ACCION = 'M' OR ACCION = 'E' 
            MENU "ICEFAS"
                COMMAND "Agrega" "Agrega Icefas"
                    CALL agrega() #a
	            CALL inicializa()
                COMMAND "Consulta" "Consulta Icefas "
 	            CALL consulta() #c
 	            CALL inicializa()
                COMMAND "Modifica" "Modifica Icefas"
                    CALL seleccion() #s
                    CALL inicializa() #i
                COMMAND "Salir" "Salir del Programa"
 	            EXIT MENU
            END MENU
        WHEN ACCION = 'C' 
            MENU "ICEFAS"
                COMMAND "Consulta" "Consulta Icefas "
 	            CALL consulta() #c
 	            CALL inicializa()
                COMMAND "Salir" "Salir del Programa"
 	            EXIT MENU
            END MENU
    END CASE

    CLOSE WINDOW tram0011

END MAIN

FUNCTION init()
#i-------------

    LET HOY  = TODAY
    LET HORA = TIME

    LET reg_1.n_folio        = ARG_VAL(1)
    LET reg_1.tipo_solicitud = ARG_VAL(2)
    LET ACCION               = ARG_VAL(3)

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
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Esc ] Ingreso        [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " [ Ctrl-b ] Pantalla de ingreso de ICEFAS " AT 2,1
    DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY "                            INGRESO DE ICEFAS                              " AT 8,1 ATTRIBUTE(REVERSE)



    INPUT BY NAME reg_1.n_folio        , 
                  reg_1.tipo_solicitud ,
                  reg_1.n_seguro       ,
                  reg_1.n_rfc          ,
                  reg_1.n_unico     WITHOUT DEFAULTS
    BEFORE INPUT
    LET folio_reg = FALSE
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
            FROM   afi_solicitud 
            WHERE  n_folio        = reg_1.n_folio
            AND    tipo_solicitud = reg_1.tipo_solicitud
    DISPLAY BY NAME reg_1.*

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

            IF  reg_1.tipo_solicitud <> 1
            AND reg_1.tipo_solicitud <> 2 THEN
                ERROR"TIPO SOLICITUD NO CORRESPONDE"
                NEXT FIELD tipo_solicitud
           END IF

            IF STATUS = NOTFOUND THEN
                ERROR "FOLIO NO EXISTE EN afi_mae_afiliado....NO ES UN AFILIADO ",
                      "DE LA AFORE"
                NEXT FIELD n_folio
            ELSE 
               LET folio_reg = TRUE
            END IF
            DISPLAY BY NAME reg_1.*

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
            FROM   afi_solicitud
            WHERE  n_seguro = reg_1.n_seguro

            IF STATUS = NOTFOUND THEN
                ERROR " NSS INEXISTENTE "
                NEXT FIELD n_seguro
            ELSE
                 LET folio_reg = TRUE
            END IF
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres

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
            FROM   afi_solicitud
            WHERE  n_rfc = reg_1.n_rfc

            IF STATUS = NOTFOUND THEN
                ERROR " RFC INEXISTENTE "
                NEXT FIELD n_rfc
            ELSE
                  LET folio_reg = TRUE
            END IF 
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres

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
            FROM   afi_solicitud
            WHERE  n_unico = reg_1.n_unico

            IF STATUS = NOTFOUND THEN
                ERROR " CURP INEXISTENTE "
                NEXT FIELD n_unico
            ELSE
                LET folio_reg = TRUE
            END IF
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres

        ON KEY (CONTROL-B)
            LET sw_2 = 0
            LET reg_2.n_folio_tra     = NULL
            LET reg_2.icefa_cod       = NULL
            LET reg_2.des_icefa       = NULL
            LET reg_2.nro_int_cta     = NULL
            LET reg_2.tipo_comp_icefa = NULL

            IF folio_reg = FALSE
            THEN 
              ERROR "AFILIADO INEXISTENTE ..."
              LET folio_reg = FALSE
              LET n_seguro_reg = FALSE
              LET rfc_reg = FALSE
              NEXT FIELD n_folio
            END IF

            DISPLAY reg_2.n_folio_tra TO n_folio_tra
            DISPLAY reg_2.icefa_cod   TO icefa_cod
            DISPLAY reg_2.des_icefa   TO des_icefa
            DISPLAY reg_2.nro_int_cta TO nro_int_cta
           
            INPUT BY NAME reg_2.n_folio_tra      ,
	                       reg_2.nss              ,
                          reg_2.rfc              ,
                          reg_2.fecha_solic_tra  ,
                          reg_2.fecha_captura    ,
                          reg_2.paterno2         ,
                          reg_2.materno2         ,
                          reg_2.nombres2         ,
                          reg_2.icefa_cod        ,
                          reg_2.des_icefa        ,
                          reg_2.nro_int_cta      ,
                          reg_2.tipo_comp_icefa  ,
                          reg_2.fecha_comp_icefa WITHOUT DEFAULTS

                BEFORE FIELD n_folio_tra
                    IF sw_2 = 0 THEN
                        LET sw_2 = 1
                        LET reg_2.nss              = reg_1.n_seguro
                        LET reg_2.rfc              = reg_1.n_rfc
                        LET reg_2.paterno2         = reg_1.paterno
                        LET reg_2.materno2         = reg_1.materno
                        LET reg_2.nombres2         = reg_1.nombres
                        LET reg_2.fecha_solic_tra  = HOY
                        LET reg_2.fecha_captura    = HOY
                        LET reg_2.fecha_comp_icefa = ""
                        LET reg_2.fecha_proceso    = HOY
                        LET reg_2.status           = 1
                        LET reg_2.usuario          = c8_usuario
                    LET reg_2.fecha_comp_icefa = HOY
                    DISPLAY BY NAME reg_2.fecha_comp_icefa

                        DISPLAY reg_2.nss              TO nss
                        DISPLAY reg_2.rfc              TO rfc
                        DISPLAY reg_2.paterno2         TO paterno2
                        DISPLAY reg_2.materno2         TO materno2
                        DISPLAY reg_2.nombres2         TO nombres2
                        DISPLAY reg_2.fecha_solic_tra  TO fecha_solic_tra
                        DISPLAY reg_2.fecha_captura    TO fecha_captura
                        DISPLAY reg_2.fecha_comp_icefa TO fecha_comp_icefa
                        DISPLAY reg_2.fecha_proceso    TO fecha_proceso   
                        DISPLAY reg_2.usuario          TO usuario
                    END IF

                AFTER FIELD n_folio_tra


                AFTER FIELD fecha_solic_tra
                    IF reg_2.fecha_solic_tra IS NULL THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD fecha_solic_tra
                    END IF

                    IF reg_2.fecha_solic_tra > HOY THEN
                        ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                        NEXT FIELD fecha_solic_tra
                    END IF

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
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD nombres
                    END IF

		    IF reg_2.icefa_cod IS NULL THEN
		        CALL despliega_icefas() #di
                             RETURNING reg_2.icefa_cod ,
                                       reg_2.des_icefa

		        IF reg_2.icefa_cod = 0 THEN 
                            NEXT FIELD icefa_cod 
                        END IF
	            ELSE
		        SELECT icefa_desc
                        INTO   reg_2.des_icefa
			FROM   tab_icefa
		        WHERE  icefa_cod = reg_2.icefa_cod

		        IF STATUS = NOTFOUND THEN
	     	          ERROR " ICEFA INEXISTENTE "
		          NEXT FIELD icefa_cod
		        END IF
	            END IF

	            DISPLAY reg_2.icefa_cod TO icefa_cod
		    DISPLAY reg_2.des_icefa TO des_icefa

	        AFTER FIELD nro_int_cta
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD icefa_cod
                    END IF


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

	        AFTER FIELD fecha_comp_icefa

                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD tipo_comp_icefa
                    END IF
{
                    IF reg_2.fecha_comp_icefa IS NULL  AND 
                       reg_2.tipo_comp_icefa <> 4 THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD fecha_comp_icefa
                    END IF
}
                    IF reg_2.fecha_comp_icefa > HOY THEN
                        ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                        NEXT FIELD fecha_comp_icefa
                    END IF
       
                ON KEY (INTERRUPT)
                    INITIALIZE reg_2.* TO NULL
                    DISPLAY reg_2.n_folio_tra      TO n_folio_tra
                    DISPLAY reg_2.nss              TO nss
                    DISPLAY reg_2.rfc              TO rfc
                    DISPLAY reg_2.fecha_solic_tra  TO fecha_solic_tra
                    DISPLAY reg_2.fecha_captura    TO fecha_captura
                    DISPLAY reg_2.paterno2         TO paterno2
                    DISPLAY reg_2.materno2         TO materno2
                    DISPLAY reg_2.nombres2         TO nombres2
                    DISPLAY reg_2.icefa_cod        TO icefa_cod
                    DISPLAY reg_2.nro_int_cta      TO nro_int_cta
                    DISPLAY reg_2.saldo_sar_92     TO saldo_sar_92
                    DISPLAY reg_2.saldo_viv_92     TO saldo_viv_92
                    DISPLAY reg_2.tipo_comp_icefa  TO tipo_comp_icefa
                    DISPLAY reg_2.des_icefa        TO des_icefa
                    DISPLAY reg_2.fecha_comp_icefa TO fecha_comp_icefa
                    DISPLAY reg_2.usuario          TO usuario
                    DISPLAY reg_2.fecha_proceso    TO fecha_proceso 
                   #DISPLAY reg_2.correlativo      TO correlativo   

                    EXIT INPUT

                ON KEY (ESC)
                    DISPLAY "VERIFICANDO INFORMACION" AT 19,2 ATTRIBUTE(REVERSE)
                    SLEEP 2
                    DISPLAY "                       " AT 19,2 
                    IF reg_2.fecha_solic_tra IS NULL THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD fecha_solic_tra
                    END IF

                    IF reg_2.fecha_solic_tra > HOY THEN
                        ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                        NEXT FIELD fecha_solic_tra
                    END IF

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
{
                    IF reg_2.fecha_comp_icefa IS NULL  AND 
                       reg_2.tipo_comp_icefa <> 4 THEN
                        ERROR " CAMPO NO PUEDE SER NULO "
                        NEXT FIELD fecha_comp_icefa
                    END IF
}
                    IF reg_2.fecha_comp_icefa > HOY THEN
                        ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                        NEXT FIELD fecha_comp_icefa
                    END IF


                    SELECT "OK"
                    FROM   afi_icefa
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

                        INSERT INTO afi_icefa
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
                                      reg_2.fecha_solic_tra  ,
                                      reg_2.tipo_comp_icefa  ,
                                      reg_2.fecha_comp_icefa ,
                                      0                     ,#saldo_sar_92
                                      0                     ,#saldo_viv_92
                                      reg_2.fecha_captura    ,
                                      reg_2.fecha_proceso    ,
                                      ""                     ,#lote_genera
                                      ""                     ,#fecha_genera
                                      1                      ,#status
                                      "I"                    ,
                                      0                      ,#correlativo
                                      reg_2.usuario          ,
												  ""                     ,#n_envios
												  ""                      #diagnostico
                                     )
                        DISPLAY "PREICEFA INSERTADA","" AT 19,2 ATTRIBUTE(REVERSE)
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
                    DISPLAY reg_2.fecha_solic_tra  TO fecha_solic_tra
                    DISPLAY reg_2.fecha_captura    TO fecha_captura
                    DISPLAY reg_2.paterno2         TO paterno2
                    DISPLAY reg_2.materno2         TO materno2
                    DISPLAY reg_2.nombres2         TO nombres2
                    DISPLAY reg_2.icefa_cod        TO icefa_cod
                    DISPLAY reg_2.nro_int_cta      TO nro_int_cta
                    DISPLAY reg_2.tipo_comp_icefa  TO tipo_comp_icefa
                    DISPLAY reg_2.tipcom_desc      TO tipcom_desc
	                 DISPLAY reg_2.des_icefa        TO des_icefa
                    DISPLAY reg_2.fecha_comp_icefa TO fecha_comp_icefa
                    DISPLAY reg_2.usuario          TO usuario
                    DISPLAY reg_2.fecha_proceso    TO fecha_proceso 
                    NEXT FIELD n_folio_tra
            END INPUT
	    NEXT FIELD n_folio
    END INPUT
END FUNCTION

FUNCTION consulta()
#c-----------------
    DEFINE #loc #date
        d_fecha_genera        DATE

    DEFINE #loc #char
        c11_n_seguro          CHAR(11)

    DEFINE #loc #smallint
        s_lote_genera         SMALLINT ,
        i                     SMALLINT

    DEFINE #loc #integer
        i_correlativo         INTEGER

    OPEN WINDOW tram0012 AT 3,2 WITH FORM "AFIM0252" ATTRIBUTE( BORDER)
    DISPLAY " AFIM025             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Ctrl-c ] Salir              [ Ctrl-p ] Eliminar         "
            AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_1 = 0
    WHILE TRUE
    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)
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

            IF  reg_1.tipo_solicitud <> 1
            AND reg_1.tipo_solicitud <> 2 THEN
                ERROR"TIPO SOLICITUD NO CORRESPONDE"
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
            FROM   afi_solicitud
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
            FROM   afi_solicitud
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
            FROM   afi_solicitud
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

    DECLARE cur_1 CURSOR FOR 
    SELECT A.n_seguro         ,
           A.n_folio_tra      ,
           A.nss              ,
           A.rfc              ,
           A.fecha_solic_tra  ,
           A.fecha_captura    ,
           A.paterno          ,
           A.materno          ,
           A.nombres          ,
           A.icefa_cod        ,
           B.icefa_desc       ,
           A.nro_int_cta      ,
           A.saldo_sar_92     ,
           A.saldo_viv_92     ,
           A.tipo_comp_icefa  ,
           ""                 ,#tipcom_desc
           A.fecha_comp_icefa ,
           A.usuario          ,
           A.fecha_proceso    ,
           A.status           ,#estado
           ""                 ,#des_estado
           ""                 ,#fecha_a_liquidar
           ""                 ,#fecha_liquidacion
           A.correlativo      ,#correlativo
           ""                 ,#diag_proceso
           ""                 ,#des_diagnostico
           A.fecha_genera     ,
           A.lote_genera
    FROM   afi_icefa A, tab_icefa B
    WHERE  A.n_seguro       = reg_1.n_seguro
    AND    A.tipo_solicitud = reg_1.tipo_solicitud
    AND    A.icefa_cod      = B.icefa_cod
	   
    SELECT count(*)
    INTO   cuantos
    FROM   afi_icefa
    WHERE  afi_icefa.n_seguro       = reg_1.n_seguro
    AND    afi_icefa.tipo_solicitud = reg_1.tipo_solicitud

    DISPLAY " Nro.Icefas ",cuantos AT 8,1 ATTRIBUTE(REVERSE)
    LET i = 1
    FOREACH cur_1 INTO c11_n_seguro,arr_1[i].*,d_fecha_genera,s_lote_genera
        SELECT tipcom_desc
        INTO   arr_1[i].tipcom_desc
        FROM   tab_tipo_com
        WHERE  tipcom_cod = arr_1[i].tipo_comp_icefa

        SELECT des_estado
        INTO   arr_1[i].des_estado
        FROM   tra_status
        WHERE  estado = arr_1[i].estado 

        IF arr_1[i].estado = "3" THEN
            CALL codigo_rechazo(arr_1[i].nss         ,
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
#ff
        IF (arr_1[i].estado = 41  or
            arr_1[i].estado = 7   or
            arr_1[i].estado = 8 ) THEN
         
            SELECT max(A.fech_mov_banxico)j
            INTO   arr_1[i].fecha_a_liquidar
            FROM   tra_det_trasp_sal A
            WHERE  A.n_seguro       = c11_n_seguro
            AND    A.n_seguro_ent   = arr_1[i].nss
            AND    A.rfc_ent        = arr_1[i].rfc
            AND    A.cve_ced_cuenta = arr_1[i].icefa_cod
            AND    A.nro_ctrl_icefa = arr_1[i].nro_int_cta
            AND    A.tipo_icefa     = "N"    

            CALL habil_siguiente(arr_1[i].fecha_a_liquidar)
            RETURNING arr_1[i].fecha_a_liquidar
        END IF

        IF ( arr_1[i].estado = 8 ) THEN
         
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
        LET arr_1[i].des_diagnostico = null
        IF arr_1[i].estado = "5" THEN
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
        LET i = i + 1
    END FOREACH
    CALL SET_COUNT(i-1)
    DISPLAY ARRAY arr_1 TO scr_1.*
	  
        ON KEY ( INTERRUPT )
            CALL inicializa()
	    EXIT DISPLAY

        ON KEY (Control-p)
            LET i = ARR_CURR()
            LET e_correlativo = arr_1[i].correlativo
            CALL Pregunta()
            IF aux_pausa MATCHES "[Ss]" THEN
	       DELETE FROM afi_icefa
	       WHERE correlativo = e_correlativo
               ERROR "ICEFA ELIMINADA..." SLEEP 2
               ERROR ""
               CALL inicializa_reg_1()
            ELSE 
               ERROR "ELIMINACION DE ICEFA CANCELADA..." SLEEP 2
               ERROR ""
               CALL inicializa_reg_1()
            END IF
               CLEAR FORM
	       EXIT DISPLAY
    END DISPLAY
    END WHILE
    CLOSE WINDOW tram0012
END FUNCTION

FUNCTION elimina(e_correlativo)
#el-----------------------------------
    DEFINE e_correlativo LIKE afi_icefa.correlativo


    LET e_correlativo = arr_1[i].correlativo
	
	DELETE FROM afi_icefa
	WHERE correlativo = e_correlativo

END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE reg_5 RECORD
        nss                   LIKE afi_icefa.nss         ,
        rfc                   LIKE afi_icefa.rfc         ,
        icefa_cod             LIKE afi_icefa.icefa_cod   ,
        nro_int_cta           LIKE afi_icefa.nro_int_cta
    END RECORD

    OPEN WINDOW tram0015 AT 3,2 WITH FORM "AFIM0255" ATTRIBUTE( BORDER)
    DISPLAY " AFIM025             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " [ Esc ] Modifica      [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_3 = 0
    DISPLAY " Nro.Icefas ",cuantos AT 8,1 ATTRIBUTE(REVERSE)
    INPUT BY NAME reg_2.n_folio_tra      ,
                  reg_2.nss              ,
                  reg_2.rfc              ,
                  reg_2.fecha_solic_tra  ,
                  reg_2.fecha_captura    ,
                  reg_2.paterno2         ,
                  reg_2.materno2         ,
                  reg_2.nombres2         ,
                  reg_2.icefa_cod        ,
                  reg_2.des_icefa        ,
                  reg_2.nro_int_cta      ,
                  #reg_2.saldo_sar_92     ,
                  #reg_2.saldo_viv_92     ,
                  reg_2.tipo_comp_icefa  ,
                  reg_2.fecha_comp_icefa WITHOUT DEFAULTS

        BEFORE FIELD n_folio_tra
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

            IF sw_3 = 0 THEN
                LET sw_3                   = 1
                LET reg_2.n_folio_tra      = arr_m[arr_c].n_folio_tra
                LET reg_2.nss              = arr_m[arr_c].nss
                LET reg_2.rfc              = arr_m[arr_c].rfc
                LET reg_2.fecha_solic_tra  = arr_m[arr_c].fecha_solic_tra
                LET reg_2.fecha_captura    = arr_m[arr_c].fecha_captura
                LET reg_2.paterno2         = arr_m[arr_c].paterno2
                LET reg_2.materno2         = arr_m[arr_c].materno2
                LET reg_2.nombres2         = arr_m[arr_c].nombres2
                LET reg_2.icefa_cod        = arr_m[arr_c].icefa_cod
                LET reg_2.des_icefa        = arr_m[arr_c].des_icefa
                LET reg_2.nro_int_cta      = arr_m[arr_c].nro_int_cta
                LET reg_2.saldo_sar_92     = arr_m[arr_c].saldo_sar_92
                LET reg_2.saldo_viv_92     = arr_m[arr_c].saldo_viv_92
                LET reg_2.tipo_comp_icefa  = arr_m[arr_c].tipo_comp_icefa
                LET reg_2.fecha_comp_icefa = arr_m[arr_c].fecha_comp_icefa
                LET reg_2.usuario          = arr_m[arr_c].usuario
                LET reg_2.fecha_proceso    = arr_m[arr_c].fecha_proceso
                LET reg_2.status           = arr_m[arr_c].estado
                LET reg_2.des_estado       = arr_m[arr_c].des_estado
                LET reg_2.correlativo      = arr_m[arr_c].correlativo
                LET reg_2.diag_proceso     = arr_m[arr_c].diag_proceso
                LET reg_2.des_diagnostico  = arr_m[arr_c].des_diagnostico

                
                LET reg_5.nss              = arr_m[arr_c].nss
                LET reg_5.rfc              = arr_m[arr_c].rfc
                LET reg_5.icefa_cod        = arr_m[arr_c].icefa_cod
                LET reg_5.nro_int_cta      = arr_m[arr_c].nro_int_cta
            END IF

            DISPLAY BY NAME reg_1.* 
            DISPLAY reg_2.n_folio_tra      TO n_folio_tra
            DISPLAY reg_2.nss              TO nss
            DISPLAY reg_2.rfc              TO rfc
            DISPLAY reg_2.fecha_solic_tra  TO fecha_solic_tra
            DISPLAY reg_2.fecha_captura    TO fecha_captura
            DISPLAY reg_2.paterno2         TO paterno2
            DISPLAY reg_2.materno2         TO materno2
            DISPLAY reg_2.nombres2         TO nombres2
            DISPLAY reg_2.icefa_cod        TO icefa_cod
            DISPLAY reg_2.nro_int_cta      TO nro_int_cta
            DISPLAY reg_2.saldo_sar_92     TO saldo_sar_92
            DISPLAY reg_2.saldo_viv_92     TO saldo_viv_92
            DISPLAY reg_2.tipo_comp_icefa  TO tipo_comp_icefa
	    DISPLAY reg_2.des_icefa        TO des_icefa
            DISPLAY reg_2.fecha_comp_icefa TO fecha_comp_icefa
            DISPLAY reg_2.usuario          TO usuario
            DISPLAY reg_2.fecha_proceso    TO fecha_proceso 
            DISPLAY reg_2.status           TO status
            DISPLAY reg_2.des_estado       TO des_status
            DISPLAY reg_2.correlativo      TO correlativo   
            DISPLAY reg_2.diag_proceso     TO diag_proceso
            DISPLAY reg_2.des_diagnostico  TO des_diagnostico

    #    AFTER FIELD n_folio_tra

        AFTER FIELD fecha_solic_tra
            IF reg_2.fecha_solic_tra IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD fecha_solic_tra
            END IF

            IF reg_2.fecha_solic_tra > HOY THEN
                ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                NEXT FIELD fecha_solic_tra
            END IF

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
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres2
            END IF

	    IF reg_2.icefa_cod IS NULL THEN
	        CALL despliega_icefas()
                RETURNING reg_2.icefa_cod ,
                          reg_2.des_icefa

	        IF reg_2.icefa_cod = 0 THEN 
                    NEXT FIELD icefa_cod 
                END IF
	    ELSE
	        SELECT icefa_desc
                INTO   reg_2.des_icefa
		FROM   tab_icefa
		WHERE  icefa_cod = reg_2.icefa_cod

		IF STATUS = NOTFOUND THEN
	     	    ERROR " ICEFA INEXISTENTE "
		    NEXT FIELD icefa_cod
		END IF
	    END IF

	    DISPLAY reg_2.icefa_cod TO icefa_cod
	    DISPLAY reg_2.des_icefa TO des_icefa

	AFTER FIELD nro_int_cta
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD icefa_cod
            END IF

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

	AFTER FIELD fecha_comp_icefa
            IF reg_2.fecha_comp_icefa IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD fecha_comp_icefa
            END IF

            IF reg_2.fecha_comp_icefa > HOY THEN
                ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                NEXT FIELD fecha_comp_icefa
            END IF

        ON KEY ( INTERRUPT )
            LET sw_4 = 1
            EXIT INPUT

        ON KEY (ESC)
            ERROR "VERIFICANDO INFORMACION"

            IF reg_2.nss IS NULL AND reg_2.rfc IS NULL THEN
                ERROR " NSS Y RFC AMBOS NO PUEDEN SER NULOS "
                NEXT FIELD nss
            END IF

            IF reg_2.fecha_solic_tra > HOY THEN
                ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                NEXT FIELD fecha_solic_tra
            END IF

            IF reg_2.paterno2 IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD paterno2
            END IF

            IF reg_2.nombres2 IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD nombres2
            END IF

            IF reg_2.icefa_cod IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO "
                NEXT FIELD icefa_cod
            END IF

#ff
            IF  reg_5.nss         <> reg_2.nss
            OR  reg_5.rfc         <> reg_2.rfc
            OR  reg_5.icefa_cod   <> reg_2.icefa_cod
            OR  reg_5.nro_int_cta <> reg_2.nro_int_cta THEN

                SELECT "OK"
                FROM   afi_icefa
                WHERE  n_seguro    = reg_1.n_seguro
                AND    nss         = reg_2.nss
                AND    rfc         = reg_2.rfc
                AND    icefa_cod   = reg_2.icefa_cod
                AND    nro_int_cta = reg_2.nro_int_cta 
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                    ERROR "ICEFA DUPLICADA" ATTRIBUTE(REVERSE) SLEEP 3
                    LET enter = "N"
                    NEXT FIELD n_folio_tra
                END IF
            END IF

            IF reg_2.fecha_comp_icefa > HOY THEN
                ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                NEXT FIELD fecha_comp_icefa
            END IF

            LET sw_4 = 1
            LET sw_3 = 2

            WHILE TRUE
           
            ERROR ""
            PROMPT "DESEA REENVIAR LA SOLICITUD DE TRASPASO S/N " FOR CHAR enter

            IF enter MATCHES"[SsNn]" THEN
                IF enter MATCHES "[Ss]" THEN
                    IF reg_2.status = 3 THEN
                        CALL actualiza_det_sol_rech(reg_2.nss         ,
                                                    reg_2.rfc         ,
                                                    reg_2.icefa_cod   ,
                                                    reg_2.nro_int_cta ,
                                                    9) #adsr
                    END IF

                    UPDATE afi_icefa
                    SET    afi_icefa.n_folio_tra      = reg_2.n_folio_tra     ,
                           afi_icefa.nss              = reg_2.nss             ,
                           afi_icefa.rfc              = reg_2.rfc             ,
                           afi_icefa.fecha_solic_tra  = reg_2.fecha_solic_tra ,
                           afi_icefa.fecha_captura    = reg_2.fecha_captura   ,
                           afi_icefa.paterno          = reg_2.paterno2        ,
                           afi_icefa.materno          = reg_2.materno2        ,
                           afi_icefa.nombres          = reg_2.nombres2        ,
                           afi_icefa.icefa_cod        = reg_2.icefa_cod       ,
                           afi_icefa.nro_int_cta      = reg_2.nro_int_cta     ,
                           afi_icefa.saldo_sar_92     = reg_2.saldo_sar_92    ,
                           afi_icefa.saldo_viv_92     = reg_2.saldo_viv_92    ,
                           afi_icefa.tipo_comp_icefa  = reg_2.tipo_comp_icefa ,
                           afi_icefa.fecha_comp_icefa = reg_2.fecha_comp_icefa,
                           afi_icefa.status           = 9                     ,
                           afi_icefa.usuario          = reg_2.usuario         ,
                           afi_icefa.fecha_proceso    = HOY
                    WHERE  afi_icefa.n_seguro         = reg_1.n_seguro
                    AND    afi_icefa.correlativo      = reg_2.correlativo
                ELSE
                    UPDATE afi_icefa
                    SET    afi_icefa.n_folio_tra      = reg_2.n_folio_tra     ,
                           afi_icefa.nss              = reg_2.nss             ,
                           afi_icefa.rfc              = reg_2.rfc             ,
                           afi_icefa.fecha_solic_tra  = reg_2.fecha_solic_tra ,
                           afi_icefa.fecha_captura    = reg_2.fecha_captura   ,
                           afi_icefa.paterno          = reg_2.paterno2        ,
                           afi_icefa.materno          = reg_2.materno2        ,
                           afi_icefa.nombres          = reg_2.nombres2        ,
                           afi_icefa.icefa_cod        = reg_2.icefa_cod       ,
                           afi_icefa.nro_int_cta      = reg_2.nro_int_cta     ,
                           afi_icefa.saldo_sar_92     = reg_2.saldo_sar_92    ,
                           afi_icefa.saldo_viv_92     = reg_2.saldo_viv_92    ,
                           afi_icefa.tipo_comp_icefa  = reg_2.tipo_comp_icefa ,
                           afi_icefa.fecha_comp_icefa = reg_2.fecha_comp_icefa,
                           afi_icefa.usuario          = reg_2.usuario         ,
                           afi_icefa.fecha_proceso    = HOY
                    WHERE  afi_icefa.n_seguro         = reg_1.n_seguro
                    AND    afi_icefa.correlativo      = reg_2.correlativo
                END IF
                EXIT WHILE
            END IF
            END WHILE

            ERROR "ICEFA MODIFICADA" SLEEP 3 
				ERROR " "
            INITIALIZE reg_1.*  TO NULL
            INITIALIZE reg_2.*  TO NULL
            DISPLAY reg_1.* 

            DISPLAY reg_2.n_folio_tra      TO n_folio_tra
            DISPLAY reg_2.nss              TO nss
            DISPLAY reg_2.rfc              TO rfc
            DISPLAY reg_2.fecha_solic_tra  TO fecha_solic_tra
            DISPLAY reg_2.fecha_captura    TO fecha_captura
            DISPLAY reg_2.paterno2         TO paterno2
            DISPLAY reg_2.materno2         TO materno2
            DISPLAY reg_2.nombres2         TO nombres2
            DISPLAY reg_2.icefa_cod        TO icefa_cod
            DISPLAY reg_2.nro_int_cta      TO nro_int_cta
            DISPLAY reg_2.saldo_sar_92     TO saldo_sar_92
            DISPLAY reg_2.saldo_viv_92     TO saldo_viv_92
            DISPLAY reg_2.tipo_comp_icefa  TO tipo_comp_icefa
	    DISPLAY reg_2.des_icefa        TO des_icefa
            DISPLAY reg_2.fecha_comp_icefa TO fecha_comp_icefa
            DISPLAY reg_2.usuario          TO usuario
            DISPLAY reg_2.fecha_proceso    TO fecha_proceso 
            DISPLAY reg_2.correlativo      TO correlativo   
            CLEAR FORM
            EXIT INPUT
    END INPUT
    CLOSE WINDOW tram0015
END FUNCTION

FUNCTION despliega_icefas()
#di------------------------
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT

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
        descripcion	      CHAR(50)
    END RECORD

    DEFINE
        x_x		      CHAR(100) ,
        x_buscar	      CHAR(030)

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

    OPEN WINDOW tram0013 AT 3,2 WITH FORM "AFIM0253" ATTRIBUTE( BORDER)
    DISPLAY " AFIM025             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
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

            IF  reg_1.tipo_solicitud <> 1
            AND reg_1.tipo_solicitud <> 2 THEN
                ERROR"TIPO SOLICITUD NO CORRESPONDE"
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
            FROM   afi_solicitud
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
            FROM   afi_solicitud
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
            FROM   afi_solicitud
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
            FROM   afi_solicitud
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
           A.paterno          ,
           A.materno          ,
           A.nombres          ,
           A.icefa_cod        ,
           B.icefa_desc       ,
           A.nro_int_cta      ,
           A.saldo_sar_92     ,
           A.saldo_viv_92     ,
           A.tipo_comp_icefa  ,
           ""                 ,#tipcom_desc
           A.fecha_comp_icefa ,
           A.usuario          ,
           A.fecha_proceso    ,
           A.status           ,#estado
           ""                 ,#des_estado
           ""                 ,#fecha_a_liquidar
           ""                 ,#fecha_liquidacion
           A.correlativo      ,
           ""                 ,#diag_proceso
           ""                 ,#des_diag_proceso
           A.fecha_genera     ,
           A.lote_genera
    FROM   afi_icefa A, OUTER tab_icefa B
    WHERE  A.n_seguro       = reg_1.n_seguro
    AND    A.tipo_solicitud = reg_1.tipo_solicitud
    AND    A.icefa_cod      = B.icefa_cod

    SELECT count(*)
    INTO   cuantos
    FROM   afi_icefa
    WHERE  afi_icefa.n_seguro       = reg_1.n_seguro
    AND    afi_icefa.tipo_solicitud = reg_1.tipo_solicitud

    DISPLAY " Nro.Icefas ",cuantos AT 8,1 ATTRIBUTE(REVERSE) 
    ERROR " <ENTER>  SELECCIONA REGISTRO A MODIFICAR"
    LET i = 1

    FOREACH cur_33 INTO c11_n_seguro   ,
                        arr_m[i].*     ,
                        d_fecha_genera ,
                        s_lote_genera

        SELECT tipcom_desc
        INTO   arr_m[i].tipcom_desc
        FROM   tab_tipo_com
        WHERE  tipcom_cod = arr_m[i].tipo_comp_icefa

        SELECT des_estado
        INTO   arr_m[i].des_estado
        FROM   tra_status
        WHERE  estado = arr_m[i].estado 

        IF arr_m[i].estado = "3" THEN
            CALL codigo_rechazo(arr_m[i].nss         ,
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
        IF (arr_m[i].estado = 41  or
            arr_m[i].estado = 7   or
            arr_m[i].estado = 8 ) THEN

            SELECT A.fech_mov_banxico
            INTO   arr_m[i].fecha_a_liquidar
            FROM   tra_det_trasp_sal A
            WHERE  A.n_seguro       = c11_n_seguro
            AND    A.n_seguro_ent   = arr_m[i].nss
            AND    A.rfc_ent        = arr_m[i].rfc
            AND    A.cve_ced_cuenta = arr_m[i].icefa_cod
            AND    A.nro_ctrl_icefa = arr_m[i].nro_int_cta
            AND    A.tipo_icefa     = "N"      
            AND    A.ident_lote_solic[6,9] =   year(d_fecha_genera)
            AND    A.ident_lote_solic[10,11] = month(d_fecha_genera)
            AND    A.ident_lote_solic[12,13] = day(d_fecha_genera)
        CALL habil_siguiente(arr_m[i].fecha_a_liquidar)
            RETURNING arr_m[i].fecha_a_liquidar

        END IF

        LET arr_m[i].des_diagnostico = null
        IF arr_m[i].estado = "5" THEN
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
	
    LET sw_5 = 0
    CALL SET_COUNT(i-1)
    DISPLAY ARRAY arr_m TO scr_1.*
        ON KEY ( CONTROL-M)
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

            IF  arr_m[arr_c].estado <> 1
            AND arr_m[arr_c].estado <> 3
            AND arr_m[arr_c].estado <> 5
            AND arr_m[arr_c].estado <> 6
            AND arr_m[arr_c].estado <> 9
            AND arr_m[arr_c].estado <> 50 THEN

                CASE arr_m[arr_c].estado
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
                INITIALIZE arr_m TO NULL
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

    END WHILE
    CLOSE WINDOW tram0013
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
        nss                   LIKE afi_icefa.nss         ,
        rfc                   LIKE afi_icefa.rfc         ,
        icefa_cod             LIKE afi_icefa.icefa_cod   ,
        nro_int_cta           LIKE afi_icefa.nro_int_cta ,
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
{
{FUNCTION ayuda()
#a--------------
    DEFINE l_reg ARRAY[3000] OF RECORD
        n_folio         DECIMAL(10,0)  ,
        nombre                CHAR(50) ,
        tipo_solicitud        SMALLINT
    END RECORD

    DEFINE reg RECORD 
        n_folio         DECIMAL(10,0)  ,
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
        x_x		      CHAR(600) ,
        x_buscar2             CHAR(070) ,
        x_buscar              CHAR(100)

    DEFINE #smallint
        G_PARAMETRO           ,
        aux_val               SMALLINT

    DEFINE #integer
        pos                   INTEGER

    OPEN WINDOW tram0014 AT 05,12 WITH FORM "AFIM0254" ATTRIBUTE(BORDER)
    DISPLAY "                BUSQUEDA DE AFILIADOS                        " 
    AT 2,1 ATTRIBUTE(REVERSE)

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
              " FROM   afi_solicitud ",
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
}
FUNCTION codigo_rechazo(reg_3)
#cr---------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        nss                   LIKE afi_icefa.nss          ,
        rfc                   LIKE afi_icefa.rfc          ,
        icefa_cod             LIKE afi_icefa.icefa_cod    ,
        nro_int_cta           LIKE afi_icefa.nro_int_cta  ,
        fecha_genera          LIKE afi_icefa.fecha_genera ,
        lote_genera           LIKE afi_icefa.lote_genera
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

 
{
display "nss ",reg_3.nss
display "rfc ",reg_3.rfc
display "icefa_cod ",reg_3.icefa_cod
display "nro_int_cta ",reg_3.nro_int_cta
display "fecha_genera ",reg_4.c08_fecha_genera
display "lote_genera ",reg_4.lote_genera
prompt "espera" for char enter
}
 
    WHENEVER ERROR CONTINUE
    IF reg_3.nss <> " " THEN
        IF reg_3.rfc <> " " THEN
            IF reg_3.nro_int_cta <> " " THEN
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
               # AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
               # AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            ELSE
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            END IF
        ELSE
            IF reg_3.nro_int_cta <> " " THEN
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            ELSE
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            END IF
        END IF
    ELSE
        IF reg_3.rfc <> " " THEN
            IF reg_3.nro_int_cta <> " " THEN
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.rfc                     = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            ELSE
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.rfc                     = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            END IF
        END IF
    END IF
    WHENEVER ERROR STOP
END FUNCTION

FUNCTION codigo_devol(reg_3)
#cd-------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        nss                   LIKE afi_icefa.nss          ,
        rfc                   LIKE afi_icefa.rfc          ,
        icefa_cod             LIKE afi_icefa.icefa_cod    ,
        nro_int_cta           LIKE afi_icefa.nro_int_cta  ,
        fecha_genera          LIKE afi_icefa.fecha_genera ,
        lote_genera           LIKE afi_icefa.lote_genera
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



{
display "nss ",reg_3.nss
display "rfc ",reg_3.rfc
display "icefa_cod ",reg_3.icefa_cod
display "nro_int_cta ",reg_3.nro_int_cta
display "fecha_genera ",reg_4.c08_fecha_genera
display "lote_genera ",reg_4.lote_genera
prompt "espera" for char enter
}

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
               #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
               #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro
            ELSE
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
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
                #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            ELSE
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
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
                #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            ELSE
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.rfc                     = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                #AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                #AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            END IF
        END IF
    END IF

END FUNCTION

FUNCTION Pregunta()
#p-----------------
     PROMPT "Esta seguro de eliminar S/N ? " 
     FOR CHAR aux_pausa
END FUNCTION

FUNCTION habil_siguiente(x_fecha)
#hs------------------------------
    DEFINE x_fecha	   DATE
    DEFINE sig_fecha	   DATE
    DEFINE dia_semana	SMALLINT
    DEFINE ant_habil 	SMALLINT

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
