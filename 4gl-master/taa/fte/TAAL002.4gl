#############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                         #
#Propietario       => E.F.P.                                                #
#Fecha             => 7 DE MAYO 1999 .                                      #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                       #
#Modificado        => 16 DE JUNIO 1999.                                     #
#Modificado        => 18 DE SEPIEMBRE DE 2006                               #
#Por               => MAURO MUÑIZ CABALLERO                                 #
#Modificado        => 18 DE MARZO DE 2008                                  #
#Por               => JOSUE LISANDRO HUERTA SIERRA                          #
#Sistema           => TAA                                                   #
#Programa          => TAAL002                                               #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
    DEFINE g_seg_modulo   RECORD LIKE seg_modulo.*

    DEFINE l_record ARRAY[250] OF RECORD
        siefore           SMALLINT,
        desc_siefore      CHAR(8),
        subcuenta         SMALLINT,
        desc_subcta       CHAR(35),
        monto_en_acciones DECIMAL(16,6),
        monto_en_pesos    DECIMAL(16,6)
    END RECORD

    DEFINE g RECORD
        siefore           SMALLINT,
        desc_siefore      CHAR(8),
        subcuenta         SMALLINT,
        desc_subcta       CHAR(35),
        monto_en_acciones DECIMAL(16,6),
        monto_en_pesos    DECIMAL(16,6)
    END RECORD

    DEFINE g_usuario   CHAR(8)
    DEFINE hora        CHAR(8)
    DEFINE G_LISTA     CHAR(300)
    DEFINE G_IMPRE     CHAR(300)
    DEFINE gimpresion  CHAR(300)
    DEFINE cla_where   CHAR(1000)
    DEFINE sel_where   CHAR(2000)

    DEFINE HOY            DATE
    DEFINE v_fecha_valor  DATE

    DEFINE pos         SMALLINT
    DEFINE i           SMALLINT

    DEFINE folio       INTEGER
    DEFINE vtot_reg    INTEGER

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP
        --ACCEPT KEY control-o

    CALL STARTLOG ("TAAL002.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY                 = TODAY
    LET g.monto_en_pesos    = 0
    LET g.monto_en_acciones = 0
    LET pos                 = 1

    SELECT  codigo_afore,USER
    INTO    w_codigo_afore,g_usuario
    FROM    tab_afore_local

    SELECT  *
    INTO    g_seg_modulo.*
    FROM    seg_modulo
    WHERE   modulo_cod = 'taa'

    LET hora = TIME
    LET hora = hora[1,2],hora[4,5]

    LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".PROV_REC.",HOY USING "DDMMYY", "_",hora CLIPPED

    LET G_IMPRE = G_IMPRE CLIPPED

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,4 WITH 3 ROWS,72 COLUMNS ATTRIBUTE( BORDER)
    DISPLAY " TAAL002 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)

    MENU "MENU "
        COMMAND "Consulta " "Consulta de Provision Afore Receptora"
            CALL Consulta()
        COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION Consulta()
#c-----------------

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 10,4 WITH FORM "TAAL0022" ATTRIBUTE( BORDER)

    DISPLAY "              (Ctrl-C) Salir          (ESC) Ejecutar                           " AT 1,1 ATTRIBUTE(REVERSE,green)

    LET int_flag = FALSE

    CONSTRUCT cla_where ON folio FROM folio

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

    LET sel_where = " SELECT d.siefore, ",
                           " s.razon_social, ",
                           " d.subcuenta,   ",
                           " t.subct_desc,  ",
                           " sum(d.monto_en_acciones),",
                           " sum(d.monto_en_pesos), ",
                           " d.fecha_valor ",
                    " FROM   dis_provision d , tab_subcuenta t, tab_siefore_local s",
                    " WHERE  ", cla_where CLIPPED ,
                    " AND    d.subcuenta = t.subct_cod  ",
                    " AND    s.codigo_siefore = d.siefore ",
                    " GROUP BY 1,2,3,4,7 ",
                    " ORDER BY 1,3,4 "

    LET sel_where = sel_where CLIPPED

    PREPARE qry_consul FROM sel_where
    DECLARE cursor_1 CURSOR FOR qry_consul

    START REPORT rpt_cuenta_imp TO G_IMPRE

    FOREACH cursor_1 INTO l_record[pos].*, v_fecha_valor
        LET g.siefore           = l_record[pos].siefore
        LET g.desc_siefore      = l_record[pos].desc_siefore
        LET g.subcuenta         = l_record[pos].subcuenta
        LET g.desc_subcta       = l_record[pos].desc_subcta
        LET g.monto_en_acciones = l_record[pos].monto_en_acciones
        LET g.monto_en_pesos    = l_record[pos].monto_en_pesos

        LET pos = pos + 1

        OUTPUT TO REPORT rpt_cuenta_imp(g.*)
    END FOREACH

    FINISH REPORT rpt_cuenta_imp

    INITIALIZE l_record[pos].* TO NULL

    CLOSE WINDOW ventana_21

    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)

        ERROR ""

        OPEN WINDOW ventana_2 AT 6,2 WITH FORM "TAAL0021" ATTRIBUTE( BORDER)
        DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
        DISPLAY " (Ctrl-C) Salir            (Ctrl-P) Imprimir                               " AT 1,1 ATTRIBUTE(REVERSE,green)
        DISPLAY " TAAL002         Consulta de Provision  Afore  Receptora                   " AT 2,1 ATTRIBUTE(REVERSE,green)
        DISPLAY HOY USING " dd-mm-yyyy " AT 2,60 ATTRIBUTE(REVERSE,green)

        DISPLAY ARRAY l_record TO scr_1.*

        ON KEY (Control-p)
            ERROR "PROCESANDO IMPRESION..."
            CALL imprimir()

        ON KEY (INTERRUPT)
            LET folio                   = NULL
            LET sel_where               = NULL 
            LET cla_where               = NULL

            FOR i = 1 TO pos
                INITIALIZE l_record[pos].* TO NULL
            END FOR

            INITIALIZE g.*             TO NULL
            LET pos                     = 1
            CLEAR FORM
            EXIT DISPLAY

        END DISPLAY

        CLEAR WINDOW ventana_2
        CLOSE WINDOW ventana_2
    ELSE
        LET folio     = NULL
        LET sel_where = NULL 
        ERROR "ARCHIVO ... VACIO"
        SLEEP 2
        ERROR ""
    END IF 

END FUNCTION

FUNCTION imprimir()
#i-----------------

    LET G_LISTA = "chmod 777 ",G_IMPRE
    RUN G_LISTA

    LET gimpresion = "lp ",G_IMPRE
    RUN gimpresion

    ERROR "LISTADO GENERADO"
    SLEEP 2
    ERROR ""

END FUNCTION

REPORT rpt_cuenta_imp(g)
#ri---------------------

    DEFINE g RECORD
        siefore            SMALLINT,
        desc_siefore       CHAR(8),
        subcuenta          SMALLINT,
        desc_subcta        CHAR(35),
        monto_en_acciones  DECIMAL(16,6),
        monto_en_pesos     DECIMAL(16,6)
    END RECORD

    DEFINE vdesc_afo   CHAR(20)
    DEFINE v_folios    CHAR(100)
    DEFINE vcod_afo    SMALLINT
    DEFINE v_folio     INTEGER
    DEFINE v_rcv_pes   DECIMAL(18,6)
    DEFINE v_rcv_acc   DECIMAL(18,6)
    DEFINE v_viv_acc   DECIMAL(18,6)
    DEFINE v_viv_pes   DECIMAl(18,6)
    DEFINE v_fov_pes   DECIMAL(18,6)
    DEFINE v_fov_acc   DECIMAL(18,6)
    DEFINE v_bon_pes   DECIMAL(18,6)
    DEFINE v_bon_acc   DECIMAL(18,6)
    DEFINE sel_val_sie CHAR(600)
    DEFINE cont        SMALLINT
    DEFINE l_valor_sie CHAR(100)
    DEFINE l_desc_sie  CHAR(100)
    DEFINE sel_pesos   CHAR(300)
    DEFINE i           SMALLINT

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

    DEFINE v_fecha_valor1 DATE

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  0
        PAGE LENGTH   60

    ORDER EXTERNAL BY g.siefore, g.subcuenta

    FORMAT
        FIRST PAGE HEADER
            LET cont = 1
            LET sel_where = " SELECT UNIQUE folio ",
                            " FROM   dis_provision d ",
                            " WHERE  ", cla_where CLIPPED 

            LET sel_where = sel_where CLIPPED
            PREPARE qry_tot_fol FROM sel_where
            DECLARE c_folio CURSOR FOR qry_tot_fol
            FOREACH c_folio INTO v_folio
               LET v_folios = v_folios CLIPPED, v_folio USING "#########"
            END FOREACH

            SELECT t.razon_social, t.codigo_afore
              INTO vdesc_afo, vcod_afo 
              FROM tab_afore_local t
              
            LET v_fecha_valor1 = MDY(MONTH(v_fecha_valor), "01",
                                     YEAR(v_fecha_valor))
              
            LET sel_val_sie = " SELECT g.codigo_siefore, ",
                              "        t.razon_social, ",
                              "        g.precio_del_dia ",
                              "   FROM glo_valor_accion g, tab_siefore_local t",
                              "  WHERE g.codigo_siefore = t.codigo_siefore ",
                              "    AND g.codigo_siefore NOT IN (0,11,12) ",
                              "    AND g.fecha_valuacion = '", v_fecha_valor,"'",
                              " UNION ALL",
                              " SELECT g.codigo_siefore, ",
                              "        t.razon_social, ",
                              "        g.precio_del_dia ",
                              " FROM glo_valor_accion g, tab_siefore_local t",
                              " WHERE g.codigo_siefore = t.codigo_siefore ",
                              " AND g.codigo_siefore  IN (11,12) ",
                              " AND g.fecha_valuacion = '", v_fecha_valor1,"'",
                              " ORDER BY 1"

            LET sel_val_sie = sel_val_sie CLIPPED
            
            PREPARE qry_val_sie FROM sel_val_sie
            DECLARE cur_val_sie CURSOR FOR qry_val_sie

            LET l_desc_sie = ""

            FOREACH cur_val_sie INTO r_val_sie.*
                LET l_desc_sie  = l_desc_sie  CLIPPED, "   ", r_val_sie.desc_siefore , ":"
                LET l_valor_sie = l_valor_sie CLIPPED, " ", r_val_sie.precio_accion USING "$$$&.######" CLIPPED
            END FOREACH

            PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
            PRINT COLUMN 12,"TAAL002",
                  COLUMN 25,"LISTADO DE PROVISION AFORE / AFORE (RECEPTORA)",
                  COLUMN 75, TODAY USING "mm/dd/yyyy"
            SKIP 1 LINE
            PRINT COLUMN 12,vcod_afo USING "###","  ",vdesc_afo
            PRINT 
            PRINT COLUMN 12,"Folio :",v_folios CLIPPED,
                  COLUMN 82, "Fecha Provision:  ", v_fecha_valor
            SKIP 1 LINE
            PRINT
            PRINT "PRECIOS VALOR ACCION :   "
            PRINT l_desc_sie
            PRINT l_valor_sie
            SKIP 1 LINE
            PRINT "----------------------------------------------------------------------------------------------------------------"
            PRINT COLUMN  2, "SUBCUENTA",
                  COLUMN 50, "SIEFORE",
                  COLUMN 71, "MONTO EN ACCIONES",
                  COLUMN 98, "MONTO EN PESOS"
            PRINT "----------------------------------------------------------------------------------------------------------------"
            SKIP 1 LINE

        PAGE HEADER
            PRINT COLUMN 2,"TAAL002",
                  COLUMN 10,"LISTADO DE PROVISION AFORE / AFORE (RECEPTORA)",
                  COLUMN 75, TODAY USING "mm/dd/yyyy"
            SKIP 1 LINE
            PRINT COLUMN 12,vcod_afo USING "###","  ",vdesc_afo
            PRINT 
            PRINT COLUMN 12,"Folio :",v_folios CLIPPED,
                  COLUMN 82, "Fecha Provision:  ", v_fecha_valor
            SKIP 1 LINE
            PRINT
            PRINT "PRECIOS VALOR ACCION :   "
            PRINT l_desc_sie
            PRINT l_valor_sie
            PRINT
            PRINT "----------------------------------------------------------------------------------------------------------------"
            PRINT COLUMN  2, "SUBCUENTA",
                  COLUMN 50, "SIEFORE",
                  COLUMN 71, "MONTO EN ACCIONES",
                  COLUMN 98, "MONTO EN PESOS"
            PRINT "----------------------------------------------------------------------------------------------------------------"
            SKIP 1 LINE

        ON EVERY ROW
             PRINT COLUMN  2,g.subcuenta USING "##",
                   COLUMN  7,g.desc_subcta ,
                   COLUMN 50,g.siefore   USING "##",
                   COLUMN 54,g.desc_siefore,
                   COLUMN 64,g.monto_en_acciones USING "#,###,###,###,##&.######",
                   COLUMN 82,g.monto_en_pesos    USING "#,###,###,###,##&.######"

        AFTER GROUP OF g.siefore
            LET sel_where = " SELECT COUNT(UNIQUE nss) ",
                            " FROM   dis_provision d ",
                            " WHERE  ", cla_where CLIPPED ,
                            " AND    d.siefore = ",g.siefore

            LET sel_where = sel_where CLIPPED
            PREPARE qry_tot_reg FROM sel_where
            DECLARE cursor_2 CURSOR FOR qry_tot_reg
            OPEN cursor_2
            FETCH cursor_2 INTO vtot_reg

            PRINT
            PRINT COLUMN 2,"Total por ", g.desc_siefore,
                  COLUMN 64,GROUP SUM(g.monto_en_acciones) USING "#,###,###,###,##&.######",
                  COLUMN 82,GROUP SUM(g.monto_en_pesos)    USING "#,###,###,###,##&.######"
            PRINT COLUMN 2,"Total de registros ",vtot_reg
            SKIP 2 LINES

            IF g.siefore <> 11 AND
               g.siefore <> 12 AND
               g.siefore <> 0 THEN
                LET l_tot_siefore[cont].siefore      =  g.siefore
                LET l_tot_siefore[cont].desc_siefore =  g.desc_siefore
                LET l_tot_siefore[cont].tot_acciones =  GROUP SUM(g.monto_en_acciones)
                LET l_tot_siefore[cont].tot_pesos    =  GROUP SUM(g.monto_en_pesos)
                LET cont = cont + 1
            END IF

        ON LAST ROW
           PRINT
           PRINT "----------------------------------------------------------------------------------------------------------------"
           PRINT "----------------------------------------------------------------------------------------------------------------"
           PRINT
           PRINT COLUMN 15, "TOTALES POR SIEFORE"
           PRINT
           PRINT COLUMN 50, "SIEFORE",
                 COLUMN 71, "MONTO EN ACCIONES",
                 COLUMN 98, "MONTO EN PESOS"
           PRINT

           FOR i = 1 TO cont
               PRINT COLUMN 50, l_tot_siefore[i].siefore USING "##",
                     COLUMN 54, l_tot_siefore[i].desc_siefore,
                     COLUMN 64, l_tot_siefore[i].tot_acciones USING "#,###,###,###,##&.######",
                     COLUMN 82, l_tot_siefore[i].tot_pesos USING "#,###,###,###,##&.######"
           END FOR

           PRINT "----------------------------------------------------------------------------------------------------------------"
           PRINT "----------------------------------------------------------------------------------------------------------------"


           LET sel_pesos = " SELECT sum(monto_en_acciones), sum(monto_en_pesos) ",
                           " FROM   dis_provision d ",
                           " WHERE  ", cla_where CLIPPED ,
                           " AND    d.subcuenta IN (4, 8)"

            PREPARE qry_viv_pesos FROM sel_pesos
            EXECUTE qry_viv_pesos INTO v_viv_acc, v_viv_pes

            LET sel_pesos = " SELECT sum(monto_en_acciones), sum(monto_en_pesos)  ",
                            " FROM   dis_provision d ",
                            " WHERE  ", cla_where CLIPPED ,
                            " AND    d.subcuenta IN (14, 35)"

            PREPARE qry_fov_pesos FROM sel_pesos
            EXECUTE qry_fov_pesos INTO v_fov_acc, v_fov_pes

            LET sel_pesos = " SELECT sum(monto_en_acciones), sum(monto_en_pesos) ",
                            " FROM   dis_provision d ",
                            " WHERE  ", cla_where CLIPPED ,
                            " AND    d.subcuenta NOT IN (4, 8, 14, 35, 36)"

            PREPARE qry_rcv_pesos FROM sel_pesos
            EXECUTE qry_rcv_pesos INTO v_rcv_acc, v_rcv_pes

            LET sel_pesos = " SELECT sum(monto_en_acciones), sum(monto_en_pesos) ",
                            " FROM   dis_provision d ",
                            " WHERE  ", cla_where CLIPPED ,
                            " AND    d.subcuenta IN (36)"

            PREPARE qry_bon_pesos FROM sel_pesos
            EXECUTE qry_bon_pesos INTO v_bon_acc, v_bon_pes

            SKIP 2 LINES
            PRINT COLUMN 15, "SUBTOTALES",
                  COLUMN 71, "TOTAL ACCIONES",
                  COLUMN 98, "TOTAL PESOS"
            PRINT
            PRINT "RCV",
                  COLUMN 64,v_rcv_acc   USING "#,###,###,###,##&.######",
                  COLUMN 82,v_rcv_pes   USING "#,###,###,###,##&.######"
            PRINT "VIVIENDA",
                  COLUMN 64,v_viv_acc   USING "#,###,###,###,##&.######",
                  COLUMN 82,v_viv_pes   USING "#,###,###,###,##&.######"
            PRINT "FONDO DE VIVIENDA",
                  COLUMN 64,v_fov_acc   USING "#,###,###,###,##&.######",
                  COLUMN 82,v_fov_pes   USING "#,###,###,###,##&.######"
            PRINT "BONO DE PENSION",
                  COLUMN 64,v_bon_acc   USING "#,###,###,###,##&.######",
                  COLUMN 82,v_bon_pes   USING "#,###,###,###,##&.######"

        PAGE TRAILER
           SKIP 2 LINES
           PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
           PAUSE "Presione enter para continuar...."
           PRINT "================================================================================================================="

        END REPORT

