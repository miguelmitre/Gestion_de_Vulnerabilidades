############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa CTAB131  => CONSULTA LIBERACION DE PENDIENTES Y REGENERADAS OP 06#
#Fecha             => 10 DE AGOSTO DE 2010                                 #
#Autor             => FERNANDO HERRERA HERNANDEZ                           #
#Sistema           => TAA                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE arr_oss ARRAY[20000] OF RECORD
        id_registro        INTEGER, 
        fecha_presentacion DATE,
        nss                CHAR(11),
        curp               CHAR(18),
        n_folio            DECIMAL(10,0),
        nombre             CHAR(120),
        afore              CHAR(20),
        tipo_traspaso      CHAR(80), 
        cod_operacion      CHAR(20),
        cod_promotor       CHAR(10),
        nombre_prom        CHAR(120)
    END RECORD

    DEFINE reg_rep RECORD
        id_registro        INTEGER,
        fecha_presentacion DATE,
        nss                CHAR(11),
        curp               CHAR(18),
        n_folio            DECIMAL(10,0),
        nombre             CHAR(120),
        afore              CHAR(20),
        tipo_traspaso      CHAR(80), 
        cod_operacion      CHAR(20),
        cod_promotor       CHAR(10),
        nombre_prom        CHAR(120)
    END RECORD

    DEFINE
        fecha_presentacion DATE,
        n_seguro           CHAR(11),
        curp               CHAR(18),
        n_folio            DECIMAL(10,0)

    DEFINE
        g_afore       RECORD LIKE tab_afore_local.*,
        g_paramgrales RECORD LIKE seg_modulo.*

    DEFINE
        HOY       DATE

    DEFINE
        enter     CHAR(1),
        HORA      CHAR(8),
        g_usuario CHAR(8)

    DEFINE
        i              SMALLINT  ,
        pos            INTEGER   ,
        sel_where      CHAR(1000),
        cla_where      CHAR(1000),
        tot_aceptadas  SMALLINT  ,
        tot_proc_oper  SMALLINT  ,
        tot_incs_dato  SMALLINT  ,
        total_reg      INTEGER

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP

    CALL STARTLOG("TAAB026.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY   = TODAY
    LET HORA  = TIME

    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    SELECT *
      INTO g_afore.*
      FROM tab_afore_local

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TAAB0261" ATTRIBUTE( BORDER)
    DISPLAY " SAFRE v2.0 (OP 06-1)                                                          " AT 4,1
    DISPLAY " TAAB026    CONSULTA LIBERACION DE PENDIENTES Y REGENERADAS                    " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    MENU " LIB PENDIENT Y REG "
        COMMAND "Consulta" " Consulta solicitudes lib pendientes y regeneradas"
            CALL Consulta()
        COMMAND "Salir " " Salir de Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION Inicializa()
#iz------------------

    DEFINE j SMALLINT

    INITIALIZE arr_oss TO NULL

    FOR j = 1 TO 12
        DISPLAY arr_oss[i].* TO scr_1[i].* ATTRIBUTE (NORMAL)
    END FOR

    LET total_reg = 0

    CLEAR FORM

END FUNCTION

FUNCTION Consulta()
#C-----------------

    DISPLAY "             CTRL-C [SALIR]                     CTRL-P [IMPRIMIR]              " AT 5,1 ATTRIBUTE(REVERSE)
    DISPLAY "ID     |F PRES  |NSS        |CURP              |FOLIO SOL |NOMBRE TRABAJADOR " AT 6,1
    DISPLAY "AFORE           |TIPO TAA   |COD OPERA         |COD PROM  |NOMBRE PROMOTOR   " AT 7,1

    LET int_flag = FALSE

    LET tot_aceptadas  = 0
    LET tot_proc_oper  = 0
    LET tot_incs_dato  = 0

    CONSTRUCT cla_where ON dv.fecha_presentacion,
                           dv.n_seguro,
                           dv.curp,
                           dv.n_folio

                      FROM fecha_presentacion,
                           n_seguro,
                           curp,
                           n_folio

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = "SELECT ' ', dv.fecha_presentacion,",
                    " dv.n_seguro,",
                    " dv.curp,",
                    " dv.n_folio,",
                    " NVL(TRIM(dv.paterno),' ') || ' ' ||",
                    " NVL(TRIM(dv.materno),' ') || ' ' ||",
                    " NVL(TRIM(dv.nombres), ' '),",
                    " dv.cve_ced_cuenta || ' ' ||",
                    " ta.afore_desc,",
                    " dv.tipo_traspaso || ' ' ||",
                    " tt.descripcion,",
                    " dv.cod_result_operac || ' ' ||",
                    " CASE dv.cod_result_operac WHEN '01' THEN 'ACEPTADO'",
                                              " ELSE 'INCONSISTENCIA'",
                    " END CASE,",
                    " dv.cod_promotor,",
                    " NVL(TRIM(pm.paterno),' ') || ' ' ||",
                    " NVL(TRIM(pm.materno),' ') || ' ' ||",
                    " NVL(TRIM(pm.nombres), ' ')",
             " FROM   taa_det_devol dv,",
             " OUTER  tab_afore ta,",
             " OUTER  tab_tipo_traspaso tt,",
             " OUTER  pro_mae_promotor pm",
             " WHERE  ", cla_where CLIPPED,
             " AND    dv.cve_ced_cuenta = ta.afore_cod",
             " AND    dv.tipo_traspaso  = tt.tipo_traspaso",
             " AND    dv.cod_promotor   = pm.cod_promotor",
             " AND    tt.modulo_cod     = 'taa'",
             " AND    tt.id_opera       = '09' ",
             "ORDER BY dv.cod_result_operac, dv.fecha_presentacion, dv.n_seguro"

    LET sel_where = sel_where CLIPPED

    DISPLAY BY NAME total_reg

    PREPARE qry_oss_consulta FROM sel_where

    DECLARE cur_oss CURSOR FOR qry_oss_consulta

    LET pos = 1

    FOREACH cur_oss INTO reg_rep.*

        LET arr_oss[pos].fecha_presentacion = reg_rep.fecha_presentacion
        LET arr_oss[pos].nss                = reg_rep.nss 
        LET arr_oss[pos].curp               = reg_rep.curp
        LET arr_oss[pos].n_folio            = reg_rep.n_folio    
        LET arr_oss[pos].nombre             = reg_rep.nombre    
        LET arr_oss[pos].afore              = reg_rep.afore          
        LET arr_oss[pos].tipo_traspaso      = reg_rep.tipo_traspaso
        LET arr_oss[pos].cod_operacion      = reg_rep.cod_operacion
        LET arr_oss[pos].cod_promotor       = reg_rep.cod_promotor
        LET arr_oss[pos].nombre_prom        = reg_rep.nombre_prom  

        CASE reg_rep.cod_operacion[1,2]
          WHEN '01' 
             LET tot_aceptadas = tot_aceptadas + 1
          OTHERWISE  
             LET tot_proc_oper = tot_proc_oper + 1
        END CASE

        LET arr_oss[pos].id_registro = pos

        LET pos = pos + 1

    END FOREACH

    LET total_reg = pos - 1

    DISPLAY BY NAME total_reg

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY arr_oss TO scr_1.*
        ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

            ON KEY (CONTROL-p)
                CALL imprime_reporte()

            ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT DISPLAY
        END DISPLAY

        IF int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR "BUSQUEDA TERMINADA..."
            SLEEP 2
            ERROR ""
            CLEAR FORM
            RETURN
        ELSE
            CLEAR FORM
        END IF
    ELSE
        ERROR "REGISTROS CON ESAS CONDICIONES NO EXISTEN"
        SLEEP 2
        ERROR ""
    END IF

END FUNCTION

FUNCTION imprime_reporte()

    DEFINE tot_array   INTEGER
    DEFINE i           INTEGER
    DEFINE v_ruta      CHAR(400)
    DEFINE comando     CHAR(400)

    LET v_ruta = g_paramgrales.ruta_listados CLIPPED,"/L_", g_usuario CLIPPED,
                 ".CONS_06REC_01_", HOY USING "DDMMYYYY", "_",
                 HORA[1,2], HORA[4,5], HORA[7,8]

    START REPORT rep_oss TO v_ruta

    LET pos = pos - 1

    FOR i = 1 TO pos
        OUTPUT TO REPORT rep_oss(arr_oss[i].*)
    END FOR

    FINISH REPORT rep_oss
    
    ERROR "LISTADO GENERADO EN LA RUTA:", v_ruta
    SLEEP 2
    ERROR ""

    --LET comando = " lp ", v_ruta CLIPPED
    --RUN comando

END FUNCTION

REPORT rep_oss(reg)

    DEFINE reg RECORD
        id_registro        INTEGER,
        fecha_presentacion DATE,
        nss                CHAR(11),
        curp               CHAR(18),
        n_folio            DECIMAL(10,0),
        nombre             CHAR(120),
        afore              CHAR(20),
        tipo_traspaso      CHAR(80),
        cod_operacion      CHAR(20),
        cod_promotor       CHAR(10),
        nombre_prom        CHAR(120)
    END RECORD

    DEFINE lsubtotal       INTEGER

    DEFINE
        codigo_afore       SMALLINT,
        razon_social       CHAR(50)

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0
        PAGE LENGTH   90

    ORDER EXTERNAL BY reg.cod_operacion

    FORMAT
        PAGE HEADER
           PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
            
           SELECT a.codigo_afore, a.razon_social
             INTO codigo_afore, razon_social
             FROM tab_afore_local a
  
           PRINT COLUMN 001,"AFORE : ", codigo_afore USING "###", " ",
                            razon_social CLIPPED
           PRINT
           PRINT COLUMN 001,"MODULO: ",g_paramgrales.modulo_desc CLIPPED
           PRINT
           PRINT COLUMN 001,"SAFRE v2.0 (OP 06-1)                            "
           PRINT COLUMN 001,"PROGRAMA: TAAB026 ",
                 COLUMN 035,"LISTADO DE CONSULTA LIBERACION DE PENDIENTES ",
                            "Y REGENERADAS OP 06",
                 COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
           PRINT

           PRINT
           PRINT COLUMN 001,"ID",
                 COLUMN 008,"F PRESENT",
                 COLUMN 019,"NSS",
                 COLUMN 031,"CURP",
                 COLUMN 050,"FOLIO SOL",
                 COLUMN 061,"NOMBRE TRABAJADOR",
                 COLUMN 112,"AFORE"
           PRINT COLUMN 008,"TIPO TRASPASO",
                 COLUMN 050,"CODIGO OPERACION",
                 COLUMN 068,"COD PROM",
                 COLUMN 079,"NOMBRE PROMOTOR"

           SKIP 1 LINE

        ON EVERY ROW
           PRINT COLUMN 001,reg.id_registro        USING "######",
                 COLUMN 008,reg.fecha_presentacion USING "DD/MM/YYYY",
                 COLUMN 019,reg.nss                CLIPPED,
                 COLUMN 031,reg.curp               CLIPPED,
                 COLUMN 050,reg.n_folio            USING "##########",
                 COLUMN 061,reg.nombre             CLIPPED,
                 COLUMN 112,reg.afore              CLIPPED
           PRINT COLUMN 008,reg.tipo_traspaso      CLIPPED,
                 COLUMN 050,reg.cod_operacion      CLIPPED,
                 COLUMN 068,reg.cod_promotor       CLIPPED,
                 COLUMN 079,reg.nombre_prom        CLIPPED

           AFTER GROUP OF reg.cod_operacion
             SKIP 1 LINES

             IF reg.cod_operacion[1,2] = '01' THEN
                LET lsubtotal = tot_aceptadas
             ELSE
                LET lsubtotal = tot_proc_oper
             END IF

             #LET lsubtotal = GROUP SUM(reg.id_registro)

             PRINT COLUMN 001, "Subtotal Código Resultado de la Operación ", reg.cod_operacion CLIPPED, ": ", lsubtotal USING "#,###,##&",
                   COLUMN 055, (lsubtotal / total_reg) * 100 USING "##&.&&", "%"
             SKIP 1 LINES

        PAGE TRAILER
           SKIP 2 LINE
           PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

        ON LAST ROW
           PRINT
           PRINT COLUMN 01,"TOTAL REGISTROS SOLICITUD ACEPTADA(01) : ", tot_aceptadas
                 USING "##,##&"
           PRINT COLUMN 01,"TOTAL REGISTROS INCONSISTENTES         : ", tot_proc_oper
                 USING "##,##&"
           PRINT COLUMN 01,"TOTAL REGISTROS                        : ", COUNT(*)
                 USING "##,##&"

END REPORT
