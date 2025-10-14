#############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                         #
#Porpietario       => E.F.P.                                                #
#Fecha             => 09 DE JULIO 1999.                                     #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ.                      #
#Sistema           => TAA.                                                  #
#Programa          => TAAL004                                               #
#Modifico          => EDUARDO JOAQUIN RESENDIZ MEDINA                       #
#Fecha             => 26 DE ENERO DEL 2005                                  #
#Modifico          => JOSUE LISANDRO HUERTA SIERRA                          #
#Fecha             => 05 DE MAYO DE 2008                                    #
#                     MULTISIEFORES CIRCULAR 69-2                           #
#############################################################################
DATABASE safre_af

GLOBALS
    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
    DEFINE HOY  DATE

    DEFINE g_usuario    CHAR (08)
    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*
    DEFINE G_IMPRE      CHAR(300)
    DEFINE gimpresion   CHAR(300)
    DEFINE cla_where    CHAR(300)
    DEFINE hora         CHAR(08)
    DEFINE pos          SMALLINT

    DEFINE l_record         ARRAY [200] OF RECORD
         cve_ced_cuenta       CHAR(3)      ,
         siefore_cod          SMALLINT     ,
         siefore_desc         CHAR(8)      ,
         no_tot_acc           DECIMAL(22,6),
         saldo_subc           DECIMAL(22,2)
    END RECORD
END GLOBALS

MAIN
    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY control-o
    DEFER INTERRUPT

    CALL inicio()

END MAIN

FUNCTION inicio()
    SELECT  codigo_afore,USER
    INTO    w_codigo_afore,g_usuario
    FROM    tab_afore_local

    SELECT  ruta_listados
    INTO    g_seg_modulo.ruta_listados
    FROM    seg_modulo
    WHERE   modulo_cod = 'taa'

    LET HOY = TODAY

    CREATE TEMP TABLE valor_rpt
      (folio           INTEGER,
       codigo_siefore  SMALLINT,            #campo nuevo
       cve_ced_cuenta  CHAR(04),
       tipo            SMALLINT,
       saldo_ret97     DECIMAL(16,6),
       saldo_ces_vej   DECIMAL(16,6),
       vol_pes         DECIMAL(16,6),
       saldo_cuo_soc   DECIMAL(16,6),
       saldo_ret92     DECIMAL(16,6),
       saldo_avp       DECIMAL(16,6),
       saldo_aar       DECIMAL(16,6),
       saldo_viv97     DECIMAL(16,6),
       saldo_viv92     DECIMAL(16,6),
       saldo_issste    DECIMAL(16,6))

    OPEN WINDOW ventana_1 AT 3,4 WITH 3 ROWS,72 COLUMNS ATTRIBUTE( BORDER)
    DISPLAY " TAAL004 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)
        
    MENU "MENU "
       COMMAND "Consulta " "Reporte provision siefores cedentes."
           CALL Consulta()
       COMMAND "Salir" "Salir del Programa"
           EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION Consulta()
    CLEAR SCREEN
    OPEN WINDOW ventana_21 AT 10,4 WITH FORM "TAAL0041" ATTRIBUTE( BORDER)
    DISPLAY "              (Ctrl-C) Salir          (ESC) Ejecutar                           " AT 1,1 ATTRIBUTE(REVERSE,green)
    LET int_flag= FALSE

    CONSTRUCT cla_where ON folio FROM folio
       ON KEY(ACCEPT, ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
       ON KEY(INTERRUPT)
           LET int_flag = TRUE
           EXIT CONSTRUCT
    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        CLEAR FORM
        ERROR "Folio no aceptado."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        CLOSE WINDOW ventana_21
        RETURN
    END IF

    ERROR "PROCESANDO INFORMACION"

    CALL crea_montos()

    DECLARE cur_valor_rpt CURSOR FOR
     SELECT a.cve_ced_cuenta, b.siefore_cod, a.siefore, SUM(a.no_tot_acc), SUM(a.saldo_subc)
       FROM recep_rcv a , tab_siefore b
      WHERE b.siefore_desc = a.siefore
      GROUP BY 1,2,3
      ORDER BY 1,2

    LET pos = 1

    FOREACH cur_valor_rpt INTO l_record[pos].*
        LET pos = pos + 1
    END FOREACH

    CLOSE WINDOW ventana_21

    IF (pos-1) >= 1 THEN
       CALL  SET_COUNT(pos-1)
       ERROR ""
       OPEN WINDOW ventana_2 AT 2,4 WITH FORM "TAAL0042" ATTRIBUTE( BORDER)
       DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE)
       DISPLAY " (Ctrl-C) Salir                                    (Ctrl-P) Imprimir    " AT 1,1 ATTRIBUTE(REVERSE,green)
       DISPLAY " TAAL004                   Montos de Siefores Cedentes                         " AT 3,1 ATTRIBUTE(REVERSE,green) 
       DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE,green)

       DISPLAY ARRAY l_record TO scr_1.*
          ON KEY (Control-p)
             ERROR "PROCESANDO IMPRESION..."
             CALL imprimir()
          ON KEY (INTERRUPT)
             EXIT DISPLAY
       END DISPLAY

       CLEAR WINDOW ventana_2
       CLOSE WINDOW ventana_2
    ELSE
       ERROR "ARCHIVO ... VACIO"
       SLEEP 2
       ERROR ""
    END IF 

END FUNCTION

FUNCTION crea_montos()
#cm-------------------
    DEFINE qry_llena_rcv     CHAR(900)

    WHENEVER ERROR CONTINUE
        DROP TABLE recep_rcv
    WHENEVER ERROR STOP

    CREATE TEMP TABLE recep_rcv
        (folio                INTEGER      ,
         cont_servicio        DECIMAL(10,0),
         cve_ced_cuenta       CHAR(3)      ,
         tipo_traspaso        SMALLINT     ,
         fecha_mov_banxico    DATE         ,
         nss                  CHAR(11)     ,
         cve_subcta           CHAR(2)      ,
         prctj_subc           DECIMAL(16,6),
         saldo_subc           DECIMAL(15,2),
         no_tot_acc           DECIMAL(22,6),
         siefore              CHAR(8)      ,
         precio_acc           DECIMAL(15,6))

    LET qry_llena_rcv = 
    " INSERT INTO recep_rcv  ",
    " SELECT folio          ,",
         " cont_servicio    ,",
         " cve_ced_cuenta   ,",
         " tipo_traspaso    ,",
         " fecha_mov_banxico,",
         " nss              ,",
         " cve_subcta_1     ,",
         " prctj_subc_1     ,",
         " saldo_subc_1     ,",
         " no_tot_acc_1     ,",
         " siefore_1        ,",
         " precio_acc_1      ",
    " FROM taa_rcv_recepcion ",
   " WHERE ",cla_where        ,
     " AND cve_subcta_1 IS NOT NULL "
    PREPARE llena_subcta_1 FROM qry_llena_rcv
    EXECUTE llena_subcta_1

    LET qry_llena_rcv = 
    " INSERT INTO recep_rcv  ",
    " SELECT folio          ,",
         " cont_servicio    ,",
         " cve_ced_cuenta   ,",
         " tipo_traspaso    ,",
         " fecha_mov_banxico,",
         " nss              ,",
         " cve_subcta_2     ,",
         " prctj_subc_2     ,",
         " saldo_subc_2     ,",
         " no_tot_acc_2     ,",
         " siefore_2        ,",
         " precio_acc_2      ",
    " FROM taa_rcv_recepcion ",
   " WHERE ",cla_where        ,
     " AND cve_subcta_1 IS NOT NULL "
    PREPARE llena_subcta_2 FROM qry_llena_rcv
    EXECUTE llena_subcta_2

    LET qry_llena_rcv = 
    " INSERT INTO recep_rcv  ",
    " SELECT folio          ,",
         " cont_servicio    ,",
         " cve_ced_cuenta   ,",
         " tipo_traspaso    ,",
         " fecha_mov_banxico,",
         " nss              ,",
         " cve_subcta_3     ,",
         " prctj_subc_3     ,",
         " saldo_subc_3     ,",
         " no_tot_acc_3     ,",
         " siefore_3        ,",
         " precio_acc_3      ",
    " FROM taa_rcv_recepcion ",
   " WHERE ",cla_where        ,
     " AND cve_subcta_1 IS NOT NULL "
    PREPARE llena_subcta_3 FROM qry_llena_rcv
    EXECUTE llena_subcta_3

    LET qry_llena_rcv = 
    " INSERT INTO recep_rcv  ",
    " SELECT folio          ,",
         " cont_servicio    ,",
         " cve_ced_cuenta   ,",
         " tipo_traspaso    ,",
         " fecha_mov_banxico,",
         " nss              ,",
         " cve_subcta_4     ,",
         " prctj_subc_4     ,",
         " saldo_subc_4     ,",
         " no_tot_acc_4     ,",
         " siefore_4        ,",
         " precio_acc_4      ",
    " FROM taa_rcv_recepcion ",
   " WHERE ",cla_where        ,
     " AND cve_subcta_1 IS NOT NULL "
    PREPARE llena_subcta_4 FROM qry_llena_rcv
    EXECUTE llena_subcta_4

    LET qry_llena_rcv = 
    " INSERT INTO recep_rcv  ",
    " SELECT folio          ,",
         " cont_servicio    ,",
         " cve_ced_cuenta   ,",
         " tipo_traspaso    ,",
         " fecha_mov_banxico,",
         " nss              ,",
         " cve_subcta_5     ,",
         " prctj_subc_5     ,",
         " saldo_subc_5     ,",
         " no_tot_acc_5     ,",
         " siefore_5        ,",
         " precio_acc_5      ",
    " FROM taa_rcv_recepcion ",
   " WHERE ",cla_where        ,
     " AND cve_subcta_1 IS NOT NULL "
    PREPARE llena_subcta_5 FROM qry_llena_rcv
    EXECUTE llena_subcta_5

    LET qry_llena_rcv = 
    " INSERT INTO recep_rcv  ",
    " SELECT folio          ,",
         " cont_servicio    ,",
         " cve_ced_cuenta   ,",
         " tipo_traspaso    ,",
         " fecha_mov_banxico,",
         " nss              ,",
         " cve_subcta_6     ,",
         " prctj_subc_6     ,",
         " saldo_subc_6     ,",
         " no_tot_acc_6     ,",
         " siefore_6        ,",
         " precio_acc_6      ",
    " FROM taa_rcv_recepcion ",
   " WHERE ",cla_where        ,
     " AND cve_subcta_1 IS NOT NULL "
    PREPARE llena_subcta_6 FROM qry_llena_rcv
    EXECUTE llena_subcta_6

    LET qry_llena_rcv = 
    " INSERT INTO recep_rcv  ",
    " SELECT folio          ,",
         " cont_servicio    ,",
         " cve_ced_cuenta   ,",
         " tipo_traspaso    ,",
         " fecha_mov_banxico,",
         " nss              ,",
         " cve_subcta_7     ,",
         " prctj_subc_7     ,",
         " saldo_subc_7     ,",
         " no_tot_acc_7     ,",
         " siefore_7        ,",
         " precio_acc_7      ",
    " FROM taa_rcv_recepcion ",
   " WHERE ",cla_where        ,
     " AND cve_subcta_1 IS NOT NULL "
    PREPARE llena_subcta_7 FROM qry_llena_rcv
    EXECUTE llena_subcta_7

    LET qry_llena_rcv = 
    " INSERT INTO recep_rcv  ",
    " SELECT folio          ,",
         " cont_servicio    ,",
         " cve_ced_cuenta   ,",
         " tipo_traspaso    ,",
         " fecha_mov_banxico,",
         " nss              ,",
         " cve_subcta_8     ,",
         " prctj_subc_8     ,",
         " saldo_subc_8     ,",
         " no_tot_acc_8     ,",
         " siefore_8        ,",
         " precio_acc_8      ",
    " FROM taa_rcv_recepcion ",
   " WHERE ",cla_where        ,
     " AND cve_subcta_1 IS NOT NULL "
    PREPARE llena_subcta_8 FROM qry_llena_rcv
    EXECUTE llena_subcta_8

    DELETE FROM recep_rcv
    WHERE  no_tot_acc = 0
    OR     no_tot_acc IS NULL

    CREATE INDEX recep_rcv1
    ON recep_rcv(folio)
;
    CREATE INDEX recep_rcv2
    ON recep_rcv(nss)
;
    UPDATE STATISTICS FOR TABLE recep_rcv

END FUNCTION

#Reporte de impresora----------------------------------------------------------
FUNCTION imprimir()
    DEFINE i   SMALLINT
    LET hora = TIME

    LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
             ".LIS_PROV_TRA.",HOY USING "DDMMYY","_",hora[1,2], hora[4,5] CLIPPED

    START REPORT rpt_cuenta_imp TO G_IMPRE

    FOR i = 1 TO pos
          OUTPUT TO REPORT rpt_cuenta_imp(l_record[i].*)
    END FOR

    FINISH REPORT rpt_cuenta_imp
    ERROR ""

    ERROR "LISTADO GENERADO..."
    SLEEP 2
    ERROR ""

    LET gimpresion = "lp ",G_IMPRE
    RUN gimpresion

END FUNCTION

REPORT rpt_cuenta_imp(h)

    DEFINE h RECORD
         cve_ced_cuenta       CHAR(3)      ,
         siefore_cod          SMALLINT     ,
         siefore_desc         CHAR(8)      ,
         no_tot_acc           DECIMAL(22,6),
         saldo_subc           DECIMAL(22,2)
    END RECORD

    DEFINE vdesc_afo   CHAR(20)
    DEFINE v_folios    CHAR(100)
    DEFINE vcod_afo    SMALLINT
    DEFINE v_folio     INTEGER
    DEFINE l_desc_sie  CHAR(100)
    DEFINE sel_where   CHAR(300)

    DEFINE r_val_sie RECORD
        cod_siefore     SMALLINT,
        desc_siefore    CHAR(8),
        precio_accion   DECIMAL(15,6)
    END RECORD

    DEFINE l_tot_siefore ARRAY[20] OF RECORD
        siefore         SMALLINT,
        desc_siefore    CHAR(8),
        tot_acciones    DECIMAL(22,6),
        tot_pesos       DECIMAL(22,6)
    END RECORD

    OUTPUT
        TOP MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  0
        PAGE LENGTH   60

    ORDER EXTERNAL BY h.cve_ced_cuenta

    FORMAT
        FIRST PAGE HEADER
            LET sel_where = " SELECT UNIQUE folio ",
                            " FROM   recep_rcv d "

            LET sel_where = sel_where CLIPPED
            PREPARE qry_tot_fol FROM sel_where
            DECLARE c_folio CURSOR FOR qry_tot_fol
            FOREACH c_folio INTO v_folio
               LET v_folios = v_folios CLIPPED, v_folio USING "#########"
            END FOREACH

            SELECT t.razon_social, t.codigo_afore
              INTO vdesc_afo, vcod_afo 
              FROM tab_afore_local t

            PRINT '\033e\033(10U\033&k2S\033&l12d'
            PRINT COLUMN 3,"TAAL004",
                  COLUMN 16,"LISTADO DE PROVISION SIEFORE CEDENTES",
                  COLUMN 66, TODAY USING "mm/dd/yyyy"
            SKIP 1 LINE

            PRINT COLUMN 3,vcod_afo USING "###","  ",vdesc_afo
            PRINT 
            PRINT COLUMN 3,"Folio :",v_folios CLIPPED
            SKIP 1 LINE

            PRINT "---------------------------------------------------------------------------"
            PRINT COLUMN  2, "AFORE",
                  COLUMN 10, "SIEFORE",
                  COLUMN 30, "MONTO EN ACCIONES",
                  COLUMN 60, "MONTO EN PESOS"
            PRINT "---------------------------------------------------------------------------"
            SKIP 1 LINE

        PAGE HEADER
            PRINT '\033e\033(10U\033&k2S\033&l12d'
            PRINT COLUMN 3,"TAAL004",
                  COLUMN 16,"LISTADO DE PROVISION SIEFORE CEDENTES",
                  COLUMN 66, TODAY USING "mm/dd/yyyy"
            SKIP 1 LINE

            PRINT COLUMN 3,vcod_afo USING "###","  ",vdesc_afo
            PRINT 
            PRINT COLUMN 3,"Folio :",v_folios CLIPPED
            SKIP 1 LINE

            PRINT "---------------------------------------------------------------------------"
            PRINT COLUMN  2, "AFORE",
                  COLUMN 10, "SIEFORE",
                  COLUMN 30, "MONTO EN ACCIONES",
                  COLUMN 60, "MONTO EN PESOS"
            PRINT "---------------------------------------------------------------------------"
            SKIP 1 LINE

        ON EVERY ROW

             PRINT COLUMN  2, h.cve_ced_cuenta USING "###",
                   COLUMN  8, h.siefore_cod USING "#",
                   COLUMN 10, h.siefore_desc,
                   COLUMN 20, h.no_tot_acc USING "#,###,###,###,##&.######",
                   COLUMN 48, h.saldo_subc USING "#,###,###,###,##&.######"

        AFTER GROUP OF h.cve_ced_cuenta
            PRINT
            PRINT COLUMN  5, "TOTAL AFORE: ",
                  COLUMN 20,GROUP SUM(h.no_tot_acc) USING "#,###,###,###,##&.######",
                  COLUMN 48,GROUP SUM(h.saldo_subc)    USING "#,###,###,###,##&.######"
            SKIP 2 LINES

        ON LAST ROW

           PRINT
           PRINT "---------------------------------------------------------------------------"
           PRINT "---------------------------------------------------------------------------"
           PRINT
           PRINT COLUMN 5, "TOTAL   : ",
                 COLUMN 20, SUM(h.no_tot_acc) USING "#,###,###,###,##&.######",
                 COLUMN 48, SUM(h.saldo_subc) USING "#,###,###,###,##&.######"
           PRINT

        PAGE TRAILER
           SKIP 2 LINES
           PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
           PAUSE "Presione enter para continuar...."
           PRINT "==========================================================================="
END REPORT
