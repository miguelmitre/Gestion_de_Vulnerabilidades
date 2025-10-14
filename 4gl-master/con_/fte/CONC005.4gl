###############################################################################
#Proyecto          => AFORE (MEXICO)                                          #
#Propietario       => E.F.P                                                   #
#Programa CONC005  => CONSULTA DE REGISTROS CONTABLES(poliza)                 #
#Sistema           => CON                                                     #
#Autor             => Eduardo Joaquin Resendiz Medina                         #
#Fecha             => 15 Febrero 2006                                         #
###############################################################################

DATABASE safre_af
GLOBALS

    DEFINE g_usuario         CHAR(8)  ,
           G_IMPRE           CHAR(200),
           HORA              CHAR(08) ,
           HOY               DATE     ,
           vfolio            INTEGER  ,
           g_afore           RECORD LIKE tab_afore_local.*,
           g_paramgrales     RECORD LIKE seg_modulo.*

    DEFINE v_afore    LIKE tab_afore_local.codigo_afore
    
END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    COMMENT LINE LAST
    DEFER INTERRUPT

    CALL inicio()   #i
    CALL STARTLOG("CONC005.log")
    CALL proceso_principal()   #pp

END MAIN

FUNCTION proceso_principal()

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CONC0051" ATTRIBUTE(BORDER)
    DISPLAY " CONC005          CONSULTA DE REGISTROS CONTABLES (POLIZA)                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "CONSULTA "
        COMMAND "Registros" "Consulta Registros contables "
           CALL consulta()
           CLEAR FORM

         COMMAND "Salir" "Salir de Programa"
           EXIT MENU
    END MENU

END FUNCTION

FUNCTION inicio()

    LET HOY = TODAY

    LET HORA = TIME

    SELECT *, USER 
    INTO   g_afore.*, g_usuario 
    FROM   tab_afore_local
   
END FUNCTION
FUNCTION consulta()
#c-----------------
    DEFINE arr_c          ,
        flag              ,
        i                 INTEGER
    DEFINE x_busca        CHAR(100)
    DEFINE txt_2          CHAR(700)

    DEFINE arr_2  ARRAY[2000] OF RECORD
        proceso_cod       CHAR(5)      ,
        sumarizadora      CHAR(3)      ,
        cuenta            CHAR(09 )    ,
        siefore           SMALLINT     ,
--        desc_cuenta       CHAR(25)     ,
        tipo              CHAR(1)      ,
        identificador     CHAR(1)      ,
        fecha_emision     DATE         ,
        folio             INTEGER      ,
        cargo             DECIMAL(15,2),
        abono             DECIMAL(15,2),
        estado            CHAR(2)
    END RECORD

    DEFINE arr_1 ARRAY[2000] OF record
        proceso           CHAR(5)      ,
        folio             INTEGER      ,
        cuenta            CHAR(9)      ,
        desc_cuenta       CHAR(25)     ,
        siefore           SMALLINT     ,
        cargo             DECIMAL(15,2),
        abono             DECIMAL(15,2),
        estado            CHAR(2)
    END RECORD

    DEFINE vdesc_cuenta   CHAR(25)
    DEFINE vdesc_edo      CHAR(15)

    DEFINE vfecha_emision DATE
    DEFINE fecha_emision  DATE
    DEFINE sum_tot_cargo  DECIMAL(15,2)
    DEFINE sum_tot_abono  DECIMAL(15,2)
    DEFINE v_sum_cargo    CHAR(500)
    DEFINE v_sum_abono    CHAR(500)

    DEFINE cont_ino       SMALLINT 
    DEFINE cur_row        SMALLINT
    DEFINE scr_row        SMALLINT
    DEFINE row_cnt        SMALLINT
    DEFINE cont_inp       SMALLINT

    INITIALIZE x_busca TO NULL

    CLEAR FORM

    LET int_flag              = FALSE

    CONSTRUCT BY NAME x_busca ON fecha_emision,
                                 a.proceso_cod,
                                 folio

    AFTER FIELD fecha_emision 
    IF NOT FIELD_TOUCHED(fecha_emision) THEN
       ERROR "Fecha debe ser ingresada "
       NEXT FIELD fecha_emision
    END IF

        ON KEY (CONTROL-C)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT) 
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (Esc)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT
      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         DISPLAY "                                                                               " AT 8,1
         DISPLAY "                                                                               " AT 9,1

         RETURN
      END IF

    LET vfecha_emision = x_busca[16,25]
    DISPLAY vfecha_emision TO fecha_emision

    LET txt_2 = "SELECT  a.proceso_cod, ",
                "a.sumarizadora, ",
                "a.cuenta, ",
                "a.siefore, ",
--                "a.descripcion, ",
                "a.tipo, ",
                "a.identificador, ",
                "b.fecha_emision, ",
                "b.folio, ",
                "0, ",
                "sum(b.importe), ",
                "b.estado ",
                "FROM con_traductor a, con_transaccion b ",
                "WHERE  ",x_busca CLIPPED,
                --"AND b.estado            = 40 ",
                "AND a.proceso_cod       = b.proceso_cod ",
                "AND a.transaccion_cod   = b.transaccion_cod ",
                "AND a.siefore           = b.siefore ",
                "AND a.identificador     = b.identificador ",
                "GROUP BY 1,2,3,4,5,6,7,8,11 ",
                "ORDER BY a.proceso_cod, b.folio,a.sumarizadora,a.siefore "

    PREPARE pre_1 FROM txt_2
    DECLARE cur_1 CURSOR FOR pre_1

    LET sum_tot_cargo = 0
    LET sum_tot_abono = 0

    LET i = 1
    FOREACH cur_1 INTO arr_2[i].*

    DECLARE cur_3 CURSOR FOR
    SELECT b.descripcion
    FROM   con_transaccion a,con_traductor b
    WHERE  a.proceso_cod       = arr_2[i].proceso_cod
    AND    b.cuenta            = arr_2[i].cuenta
    AND    a.proceso_cod       = b.proceso_cod
    AND    a.transaccion_cod   = b.transaccion_cod

    FOREACH cur_3 INTO   vdesc_cuenta

       LET arr_1[i].desc_cuenta   = vdesc_cuenta

    END FOREACH

    IF arr_2[i].tipo = "C" THEN
       LET arr_1[i].proceso     = arr_2[i].proceso_cod
       LET arr_1[i].folio       = arr_2[i].folio
       LET arr_1[i].cuenta      = arr_2[i].cuenta
--       LET arr_1[i].desc_cuenta = arr_2[i].desc_cuenta
       LET arr_1[i].siefore     = arr_2[i].siefore
       LET arr_1[i].cargo       = arr_2[i].abono
       LET arr_1[i].abono       = 0
       LET arr_1[i].estado      = arr_2[i].estado
    ELSE
       LET arr_1[i].proceso     = arr_2[i].proceso_cod
       LET arr_1[i].folio       = arr_2[i].folio
       LET arr_1[i].cuenta      = arr_2[i].cuenta
--       LET arr_1[i].desc_cuenta = arr_2[i].desc_cuenta
       LET arr_1[i].siefore     = arr_2[i].siefore
       LET arr_1[i].cargo       = 0
       LET arr_1[i].abono       = arr_2[i].abono
       LET arr_1[i].estado      = arr_2[i].estado
    END IF 



    LET sum_tot_cargo = sum_tot_cargo + arr_1[i].cargo
    LET sum_tot_abono = sum_tot_abono + arr_1[i].abono


        LET i = i + 1
    END FOREACH


    INITIALIZE arr_1[i].* TO NULL
    INITIALIZE arr_2[i].* TO NULL

    IF i = 1 THEN
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        RETURN
    END IF

    CALL SET_COUNT(i-1)
    DISPLAY "                  Utilice las flechas para navegar                            " AT 8,1
    DISPLAY " PROCESO. FOLIO CUENTA    DESCRIPCION    SIE        CARGOS         ABONOS.EDO " AT 9,1  ATTRIBUTE(REVERSE)
    DISPLAY "                                                    CARGOS         ABONOS     " AT 17,1  ATTRIBUTE(REVERSE)
    DISPLAY "                           [CONTROL-P] IMPRIME REPORTE                        " AT 2,1

    DISPLAY sum_tot_cargo TO total_cargo
    DISPLAY sum_tot_abono TO total_abono
    DISPLAY ARRAY arr_1 TO scr_1.*

        ON KEY ( control-v )
            LET i = ARR_CURR()

        ON KEY ( control-c )
            FOR i = 1 TO 8
               DISPLAY arr_1[i].* TO scr_1[i].*
            END FOR

        ON KEY (CONTROL-P)
            CALL genera_reporte(x_busca)

    END DISPLAY

         DISPLAY "                                                                               " AT 8,1
         DISPLAY "                                                                               " AT 9,1
         DISPLAY "                                                                               " AT 17,1

END FUNCTION

FUNCTION genera_reporte(v_busca)

    DEFINE  vfecha_reporte DATE
    DEFINE  vlote          SMALLINT
    DEFINE  vfolio         INTEGER
    DEFINE  vcopia         CHAR(200)
    DEFINE  ejecuta        CHAR(200)
    DEFINE  vnombre        CHAR(20)
    DEFINE txt_1           CHAR(600)
    DEFINE v_busca         CHAR(100)
    DEFINE sum_tot_cargo   DECIMAL(15,2)
    DEFINE sum_tot_abono   DECIMAL(15,2)
    DEFINE i               INTEGER
    DEFINE mod_perm        CHAR(700)
    DEFINE imprime         CHAR(700)

     DEFINE rec_1  RECORD
        proceso           CHAR(5)      ,
        folio             INTEGER      ,
        cuenta            CHAR(9)      ,
        desc_cuenta       CHAR(25)     ,
        siefore           SMALLINT     ,
        cargo             DECIMAL(15,2),
        abono             DECIMAL(15,2),
        estado            CHAR(2)
    END RECORD

    DEFINE rec_2   RECORD
        proceso_cod       CHAR(5)      ,
        sumarizadora      CHAR(3)      ,
        cuenta            CHAR(09 )    ,
        siefore           SMALLINT     ,
--        desc_cuenta       CHAR(25)     ,
        tipo              CHAR(1)      ,
        identificador     CHAR(1)      ,
        fecha_emision     DATE         ,
        folio             INTEGER      ,
        cargo             DECIMAL(15,2),
        abono             DECIMAL(15,2),
        estado            CHAR(2)
    END RECORD

    DEFINE vdesc_cuenta    CHAR(25)
    DEFINE vdesc_edo       CHAR(15)

    SELECT * 
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = "con"

    SELECT codigo_afore
    INTO   v_afore
    FROM   tab_afore_local

    LET HORA = TIME

    LET G_IMPRE  = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,
                   "POLIZA_PREV_" CLIPPED,HOY USING "YYMMDD" CLIPPED

    START REPORT poliza_previo_rpt TO G_IMPRE

    LET txt_1 = "SELECT  a.proceso_cod, ",
                "a.sumarizadora, ",
                "a.cuenta, ",
                "a.siefore, ",
--                "a.descripcion, ",
                "a.tipo, ",
                "a.identificador, ",
                "b.fecha_emision, ",
                "b.folio, ",
                "0, ",
                "sum(b.importe), ",
                "b.estado ",
                "FROM con_traductor a, con_transaccion b ",
                "WHERE  ",v_busca CLIPPED,
                --"AND b.estado            = 40 ",
                "AND a.proceso_cod       = b.proceso_cod ",
                "AND a.transaccion_cod   = b.transaccion_cod ",
                "AND a.siefore           = b.siefore ",
                "AND a.identificador     = b.identificador ",
                "GROUP BY 1,2,3,4,5,6,7,8,11 ",
                "ORDER BY a.proceso_cod, b.folio,a.sumarizadora,a.siefore "

    PREPARE pre_2 FROM txt_1
    DECLARE cur_2 CURSOR FOR pre_2

    LET sum_tot_cargo = 0
    LET sum_tot_abono = 0

    LET i = 1
    FOREACH cur_1 INTO rec_2.*

    DECLARE cur_4 CURSOR FOR 
    SELECT b.descripcion
    FROM   con_transaccion a,con_traductor b
    WHERE  a.proceso_cod       = rec_2.proceso_cod
    AND    b.cuenta            = rec_2.cuenta
    AND    a.proceso_cod       = b.proceso_cod
    AND    a.transaccion_cod   = b.transaccion_cod

    FOREACH cur_4 INTO   vdesc_cuenta

       LET rec_1.desc_cuenta   = vdesc_cuenta

    END FOREACH

    IF rec_2.tipo = "C" THEN
       LET rec_1.proceso     = rec_2.proceso_cod
       LET rec_1.folio       = rec_2.folio
       LET rec_1.cuenta      = rec_2.cuenta
--       LET rec_1.desc_cuenta = rec_2.desc_cuenta            
       LET rec_1.siefore     = rec_2.siefore
       LET rec_1.cargo       = rec_2.abono
       LET rec_1.abono       = 0
       LET rec_1.estado      = rec_2.estado
    ELSE
       LET rec_1.proceso     = rec_2.proceso_cod
       LET rec_1.folio       = rec_2.folio
       LET rec_1.cuenta      = rec_2.cuenta
--       LET rec_1.desc_cuenta = rec_2.desc_cuenta
       LET rec_1.siefore     = rec_2.siefore
       LET rec_1.cargo       = 0
       LET rec_1.abono       = rec_2.abono
       LET rec_1.estado      = rec_2.estado
    END IF 

    LET sum_tot_cargo = sum_tot_cargo + rec_1.cargo
    LET sum_tot_abono = sum_tot_abono + rec_1.abono

    OUTPUT TO REPORT poliza_previo_rpt(rec_1.*,rec_2.fecha_emision)

        LET i = i + 1
        END FOREACH

        FINISH REPORT poliza_previo_rpt

        LET mod_perm = 'chmod 777 ',G_IMPRE CLIPPED
        RUN mod_perm

        LET imprime = "lp ",G_IMPRE
        --LET imprime = "vi ",G_IMPRE
        RUN imprime

        ERROR "REPORTE IMPRESO" 

END FUNCTION 

REPORT poliza_previo_rpt(rep_1,vfecha_emision)

     DEFINE    rep_1 RECORD
        proceso           CHAR(5)      ,
        folio             INTEGER      ,
        cuenta            CHAR(9)      ,
        desc_cuenta       CHAR(25)     ,
        siefore           SMALLINT     ,
        cargo             DECIMAL(15,2),
        abono             DECIMAL(15,2),
        estado            CHAR(2)
    END RECORD

    DEFINE vfecha_emision DATE
    DEFINE vrazon_social  CHAR(10)

    DEFINE
        signo             CHAR(01)     ,
        sig_imp           CHAR(18)     ,
        vconsec           INTEGER      ,
        vdescripcion      CHAR(80)     ,
        vtipo_tran        CHAR(2)      ,
        vcuenta           CHAR(10)     ,
        vcar_cre          CHAR(1)      ,
        vauxiliar         INTEGER

    DEFINE vdesc_edo      CHAR(15)

    OUTPUT
      PAGE   LENGTH 90
      LEFT   MARGIN 0
      RIGHT  MARGIN 0
      TOP    MARGIN 1
      BOTTOM MARGIN 0



    FORMAT
        PAGE HEADER
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        --PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s10B'
        PRINT COLUMN 01,"AFORE: ",v_afore,
              COLUMN 100,"FECHA HOY     ", TODAY USING "dd-mm-yyyy"
        PRINT COLUMN 100,"FECHA EMISION ", vfecha_emision USING "dd-mm-yyyy"
        SKIP 2 LINE
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        --PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s10B'
        PRINT COLUMN 08,"               REPORTE DE POLIZA PREVIA                    "
        SKIP 1 LINE 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        --PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s10B'
        PRINT COLUMN 01,"---------------------------------------------------------------------",
              COLUMN 01,"----------------------------------------------------"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        --PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s10B'
        SKIP 1 LINE 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        --PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s10B'
        PRINT COLUMN 01,"PROCESO",
              COLUMN 10,"FOLIO",
              COLUMN 20,"CUENTA",
              COLUMN 30,"DESCRIPCION",
              COLUMN 60,"SIEFORE",
              COLUMN 80,"CARGO",
              COLUMN 98,"ABONO",
              COLUMN 105,"ESTADO"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        --PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s10B'
        PRINT COLUMN 01,"---------------------------------------------------------------------",
              COLUMN 01,"----------------------------------------------------"
        SKIP 1 LINE

     ON EVERY ROW

    SELECT razon_social
    INTO vrazon_social
    FROM tab_siefore_local
    WHERE codigo_siefore = rep_1.siefore

    IF rep_1.estado = 10 THEN
      LET vdesc_edo = "REGISTRADO"
    END IF
    IF rep_1.estado = 20 THEN
      LET vdesc_edo = "CONCILIADO"
    END IF
    IF rep_1.estado = 40 THEN
      LET vdesc_edo = "CONTABILIZADO"
    END IF

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        --PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s10B'
        PRINT COLUMN 01,rep_1.proceso,
              COLUMN 05,rep_1.folio,
              COLUMN 20,rep_1.cuenta,
              COLUMN 30,rep_1.desc_cuenta CLIPPED,
              COLUMN 57,rep_1.siefore   USING "##"," ",vrazon_social,
              COLUMN 60,rep_1.cargo     USING "#############&.##",
              COLUMN 75,rep_1.abono     USING "#############&.##",
              COLUMN 105,rep_1.estado   CLIPPED," ", vdesc_edo CLIPPED

     ON LAST ROW
        SKIP 1 LINE
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        --PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s10B'
        PRINT COLUMN 01,"---------------------------------------------------------------------",
              COLUMN 01,"----------------------------------------------------"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        --PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s10B'
        PRINT COLUMN 01,"TOTALES:",
              COLUMN 70, SUM(rep_1.cargo) USING "#############&.##",
              COLUMN 86, SUM(rep_1.abono) USING "#############&.##"

     PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 90,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar."

END REPORT

