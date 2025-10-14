##############################################################################
#Owner             => E.F.P.
#Programa TRAL002  => INFORME DE ICEFAS RECHAZADAS         
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 17 DE FEBRERO DEL 2005 
#Sistema           => TRA-ICE-IMSS   
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE arr_1 ARRAY[6] OF RECORD #glo #arr_1
        orden                 SMALLINT ,
        campo                 SMALLINT
    END RECORD

    DEFINE arreglo ARRAY[6] OF RECORD #glo #arreglo
        orden                 SMALLINT
    END RECORD

    DEFINE reg_1 RECORD #glo #reg_1
        n_folio               LIKE tra_mae_icefa.n_folio              ,
        folio                 LIKE tra_det_rechazo.folio            ,
        n_seguro_ent          LIKE tra_det_rechazo.n_seguro_ent     ,
        rfc_ent               LIKE tra_det_rechazo.rfc_ent          ,
        cve_ced_cuenta        LIKE tra_det_rechazo.cve_ced_cuenta   ,
        nro_ctrl_icefa        LIKE tra_det_rechazo.nro_ctrl_icefa   ,
        paterno               LIKE tra_det_rechazo.paterno          ,
        materno               LIKE tra_det_rechazo.materno          ,
        nombres               LIKE tra_det_rechazo.nombres          ,
        ident_lote_solic      LIKE tra_det_rechazo.ident_lote_solic ,
        diag_proceso_1        LIKE tab_rch_icefa.cod_error      ,
        des_error             LIKE tab_rch_icefa.des_error      ,
        nro_ocurrencias       SMALLINT
    END RECORD

    DEFINE reg_2 RECORD #reg_2
        s_folio               SMALLINT ,
        s_n_seguro            SMALLINT ,
        s_paterno             SMALLINT ,
        s_materno             SMALLINT ,
        s_nombres             SMALLINT ,
        s_diag_proceso_1      SMALLINT
    END RECORD

    DEFINE #date
        HOY                   DATE

    DEFINE #glo #char
        RUTA                  CHAR(100) ,
        G_LISTA               CHAR(100) ,
        lp                    CHAR(100) ,
        c10_rfc               CHAR(010) ,
        k                     char(500) ,
        text_1                CHAR(500) ,
        text_2                CHAR(300) ,
        x_buscar              CHAR(500) ,
        enter                 CHAR(001)

    DEFINE #glo #integer
        nro_registro          INTEGER

    DEFINE  #smallint
        sw_1                  ,
        salir_while           ,
        s_primera_vez         ,
        i                     ,
        nro_orden             ,
        arr_c                 ,
        scr_l                 SMALLINT

    DEFINE  g_glob            RECORD
            codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
            razon_social      LIKE safre_af:tab_afore_local.razon_social
                              END RECORD

END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL init() 
    OPEN WINDOW tral0021 AT 4,4 WITH FORM "TRAL0021" ATTRIBUTE(BORDER)
    DISPLAY " TRAL002    GENERA INFORME DE ICEFAS RECHAZADAS AFORE-IMSS                     " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    WHILE TRUE
        LET salir_while = false
        CONSTRUCT BY NAME x_buscar ON folio          ,
                                      n_seguro       ,
                                      paterno        ,
                                      materno        ,
                                      nombres        ,
                                      diag_proceso_1
            ON KEY (ESC)
                EXIT CONSTRUCT

            ON KEY (INTERRUPT)
                EXIT PROGRAM
        END CONSTRUCT
    
        LET nro_orden = 0

        DISPLAY "ORDEN" AT 5,17
        INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.*
            BEFORE FIELD orden
                LET arr_c = ARR_CURR()
                LET scr_l = SCR_LINE()

            AFTER FIELD orden

            ON KEY (CONTROL-M)
                IF nro_orden = 6 THEN
                    LET nro_orden = 1
                ELSE
                    LET nro_orden = nro_orden + 1
                END IF

                FOR i = 1 TO 6
                    IF arr_1[i].orden = nro_orden THEN
                        LET arr_1[i].orden = NULL
                        DISPLAY arr_1[i].orden TO scr_1[i].orden
                    END IF
                END FOR 

                LET arr_1[arr_c].orden = nro_orden
                DISPLAY arr_1[arr_c].orden TO scr_1[scr_l].orden

            ON KEY (ESC)
                LET arr_1[1].campo = 2
                LET arr_1[2].campo = 3
                LET arr_1[3].campo = 7
                LET arr_1[4].campo = 8
                LET arr_1[5].campo = 9
                LET arr_1[6].campo = 10
                LET sw_1 = 0

                FOR i= 1 TO 6
                    LET arreglo[i].orden = 0 
                END FOR

                FOR i=1 TO 6
                    CASE arr_1[i].orden
                        WHEN 1
                            LET sw_1 = 1
                            LET arreglo[1].orden = arr_1[i].campo
                        WHEN 2
                            LET sw_1 = 1
                            LET arreglo[2].orden = arr_1[i].campo
                        WHEN 3
                            LET sw_1 = 1
                            LET arreglo[3].orden = arr_1[i].campo
                        WHEN 4
                            LET sw_1 = 1
                            LET arreglo[4].orden = arr_1[i].campo
                        WHEN 5
                            LET sw_1 = 1
                            LET arreglo[5].orden = arr_1[i].campo
                        WHEN 6
                            LET sw_1 = 1
                            LET arreglo[6].orden = arr_1[i].campo
                        OTHERWISE
                    END CASE
                END FOR

                LET s_primera_vez = TRUE
                FOR i = 1 TO 6
                    IF arreglo[i].orden <> 0 AND s_primera_vez THEN
                        LET s_primera_vez = FALSE
                        LET text_2 = text_2 CLIPPED," ",
                                     arreglo[i].orden USING"##"
                    ELSE
                        IF arreglo[i].orden <> 0 THEN
                            LET text_2 = text_2 CLIPPED,",",
                                         arreglo[i].orden USING"##"
                        END IF
                    END IF
                END FOR

                IF s_primera_vez THEN 
                    LET text_2 = "2,3,7,8,9,10"
                END IF

                LET salir_while = true
                EXIT INPUT

            ON KEY (INTERRUPT)
                EXIT INPUT
        END INPUT

        IF salir_while THEN
            EXIT WHILE
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
    LET text_1 = " SELECT 0               , " ,     #n_folio
                 "        folio           , " ,     #folio
                 "        n_seguro_ent    , " ,     #n_seguro_ent
                 "        rfc_ent         , " ,     #rfc_ent
                 "        cve_ced_cuenta  , " ,     #cve_ced_cuenta
                 "        nro_ctrl_icefa  , " ,     #nro_ctrl_icefa
                 "        paterno         , " ,     #paterno
                 "        materno         , " ,     #materno
                 "        nombres         , " ,     #nombres
                 "        ident_lote_solic, " ,     #ident_lote_solic
                 "        diag_proceso_1  , " ,     #diag_proceso_1
                 "        0               , " ,     #des_error
                 "        COUNT(*)          " ,     #nro_ocurrencias
                 " FROM   tra_det_rechazo      " ,
                 " WHERE  ",x_buscar


    LET text_1 = text_1 CLIPPED ," GROUP BY 1,2,3,4,5,6,7,8,9,10,11 ORDER BY ",
                 text_2 CLIPPED
 
    PREPARE pre_1 FROM text_1
    DECLARE cur_1 CURSOR FOR pre_1

    LET G_LISTA = RUTA CLIPPED,"/RECHAZO"
    LET nro_registro = 0
    START REPORT listado TO G_LISTA
        FOREACH cur_1 INTO reg_1.*
            CALL rescata_n_folio(reg_1.n_seguro_ent    ,
                                 reg_1.rfc_ent         ,
                                 reg_1.cve_ced_cuenta  ,
                                 reg_1.nro_ctrl_icefa  ,
                                 reg_1.ident_lote_solic
                                ) #rnf
            RETURNING reg_1.n_folio

            WHENEVER ERROR CONTINUE
                SELECT des_error
                INTO   reg_1.des_error
                FROM   tab_rch_icefa
                WHERE  cod_error = reg_1.diag_proceso_1
            WHENEVER ERROR STOP

            OUTPUT TO REPORT listado (reg_1.*)
        END FOREACH
    FINISH REPORT listado
   
    WHILE TRUE
        PROMPT "DESEA IMPRIMIR  S/N " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
               DISPLAY "ARCHIVO GENERADO EN: ",RUTA CLIPPED,"/RECHAZO" AT 18,2
               SLEEP 5 
               DISPLAY " "
               LET lp = "lp ",G_LISTA CLIPPED
               RUN lp 
               EXIT WHILE  
            ELSE
               DISPLAY "ARCHIVO GENERADO EN: ",RUTA CLIPPED,"/RECHAZO" AT 18,2
               SLEEP 5 
               DISPLAY " "
               EXIT WHILE   
            END IF
        END IF
    END WHILE

    DISPLAY "PROCESO FINALIZADO ..." AT 19,2
    SLEEP 2
    DISPLAY " " 
    EXIT PROGRAM

    CLOSE WINDOW tral0021

END MAIN
   
FUNCTION init()
#-------------
    LET HOY = TODAY 

    SELECT ruta_listados
    INTO   RUTA
    FROM   seg_modulo
    WHERE modulo_cod = "tra"

    SELECT codigo_afore,
           razon_social
      INTO g_glob.*
      FROM safre_af:tab_afore_local

END FUNCTION

REPORT listado(reg_1)
#l-------------------
    DEFINE reg_1 RECORD #loc #reg_1
        n_folio               LIKE tra_mae_icefa.n_folio              ,
        folio                 LIKE tra_det_rechazo.folio            ,
        n_seguro_ent          LIKE tra_det_rechazo.n_seguro_ent     ,
        rfc_ent               LIKE tra_det_rechazo.rfc_ent          ,
        cve_ced_cuenta        LIKE tra_det_rechazo.cve_ced_cuenta   ,
        nro_ctrl_icefa        LIKE tra_det_rechazo.nro_ctrl_icefa   ,
        paterno               LIKE tra_det_rechazo.paterno          ,
        materno               LIKE tra_det_rechazo.materno          ,
        nombres               LIKE tra_det_rechazo.nombres          ,
        ident_lote_solic      LIKE tra_det_rechazo.ident_lote_solic ,
        diag_proceso_1        LIKE tab_rch_icefa.cod_error      ,
        des_error             LIKE tab_rch_icefa.des_error      ,
        nro_ocurrencias       SMALLINT
    END RECORD

    DEFINE
        c30_nombre            CHAR(30)

    OUTPUT
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0
        PAGE LENGTH 45

    FORMAT
    PAGE HEADER
	PRINT '\033e\033(10U\033&l1O\033&k2S\033(9H'
        PRINT  
            COLUMN  01,"------------------------------------------------------",
            COLUMN  55,"------------------------------------------------------",
            COLUMN 109,"---------------------------------------------------"

        PRINT  

        PRINT COLUMN 001,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED
        PRINT COLUMN 001,"TRAL002"
        PRINT COLUMN 060,"REPORTE DE ICEFAS RECHAZADAS AFORE-IMSS"

        PRINT  

        PRINT  
            COLUMN 001,"N REG",
            COLUMN 010,"N OCURR",
            COLUMN 020,"FOLIO",
            COLUMN 029,"NSS ICE",
            COLUMN 041,"BAN",
            COLUMN 048,"NRO.CUENTA"   ,
            COLUMN 082,"NOMBRE"       ,
            COLUMN 113,"RZO"          ,
            COLUMN 121,"DESCRIPCION"

        PRINT  
            COLUMN  01,"------------------------------------------------------",
            COLUMN  55,"------------------------------------------------------",
            COLUMN 109,"---------------------------------------------------"

    ON EVERY ROW
        LET nro_registro = nro_registro + 1
        LET c30_nombre = reg_1.paterno CLIPPED ," ",
                         reg_1.materno CLIPPED ," ",
                         reg_1.nombres CLIPPED

        PRINT  
              COLUMN 001,nro_registro    USING"######"   ,
              COLUMN 010,reg_1.nro_ocurrencias USING"##" ,
              COLUMN 013,reg_1.folio                     ,
              COLUMN 027,reg_1.n_seguro_ent              ,
              COLUMN 041,reg_1.cve_ced_cuenta USING"&&&",
              COLUMN 048,reg_1.nro_ctrl_icefa[1,30] ,
              COLUMN 082,c30_nombre                      ,
              COLUMN 113,reg_1.diag_proceso_1 USING"&&&" ,
              COLUMN 121,reg_1.des_error
END REPORT
FUNCTION rescata_n_folio(reg_3)
#rnf---------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        nss                   LIKE tra_mae_icefa.nss                   ,
        rfc                   LIKE tra_mae_icefa.rfc                   ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod             ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta           ,
        ident_lote_solic      LIKE tra_det_trasp_sal.ident_lote_solic
    END RECORD

    DEFINE reg_4 RECORD #loc #reg_4
        fecha_genera          CHAR(10) ,
        lote_genera           SMALLINT
    END RECORD
    
    DEFINE #loc #integer
        i_n_folio           INTEGER

    LET reg_4.fecha_genera = reg_3.ident_lote_solic[10,11],"/",
                             reg_3.ident_lote_solic[12,13],"/",
                             reg_3.ident_lote_solic[06,09]

    LET reg_4.lote_genera  = reg_3.ident_lote_solic[14,16]

    IF reg_3.nss IS NOT NULL AND reg_3.nss <> "" THEN
        IF reg_3.nro_int_cta <> " " THEN
            LET i_n_folio = 0
            SELECT tra_mae_icefa.n_folio
            INTO   i_n_folio
            FROM   tra_mae_icefa
            WHERE  tra_mae_icefa.nss          = reg_3.nss
            AND    tra_mae_icefa.icefa_cod    = reg_3.icefa_cod 
            AND    tra_mae_icefa.nro_int_cta  = reg_3.nro_int_cta
            AND    tra_mae_icefa.fecha_genera = reg_4.fecha_genera
            AND    tra_mae_icefa.lote_genera  = reg_4.lote_genera
            AND    tra_mae_icefa.status       = 3
            GROUP BY 1

            RETURN i_n_folio

        ELSE
            LET i_n_folio = 0
            SELECT tra_mae_icefa.n_folio
            INTO   i_n_folio
            FROM   tra_mae_icefa
            WHERE  tra_mae_icefa.nss          = reg_3.nss
            AND    tra_mae_icefa.icefa_cod    = reg_3.icefa_cod 
            AND    tra_mae_icefa.fecha_genera = reg_4.fecha_genera
            AND    tra_mae_icefa.lote_genera  = reg_4.lote_genera
            AND    tra_mae_icefa.status       = 3
            GROUP BY 1
      
            RETURN i_n_folio

        END IF
    ELSE
        IF reg_3.rfc <> " " THEN
            IF reg_3.nro_int_cta <> " " THEN
                LET i_n_folio = 0
                SELECT tra_mae_icefa.n_folio
                INTO   i_n_folio
                FROM   tra_mae_icefa
                WHERE  tra_mae_icefa.rfc          = reg_3.rfc
                AND    tra_mae_icefa.icefa_cod    = reg_3.icefa_cod 
                AND    tra_mae_icefa.nro_int_cta  = reg_3.nro_int_cta
                AND    tra_mae_icefa.fecha_genera = reg_4.fecha_genera
                AND    tra_mae_icefa.lote_genera  = reg_4.lote_genera
                AND    tra_mae_icefa.status       = 3
                GROUP BY 1

                RETURN i_n_folio

            ELSE
                LET i_n_folio = 0
                SELECT tra_mae_icefa.n_folio
                INTO   i_n_folio
                FROM   tra_mae_icefa
                WHERE  tra_mae_icefa.rfc          = reg_3.rfc
                AND    tra_mae_icefa.icefa_cod    = reg_3.icefa_cod 
                AND    tra_mae_icefa.fecha_genera = reg_4.fecha_genera
                AND    tra_mae_icefa.lote_genera  = reg_4.lote_genera
                AND    tra_mae_icefa.status       = 3
                GROUP BY 1

                RETURN i_n_folio

            END IF
        END IF
    END IF
END FUNCTION
