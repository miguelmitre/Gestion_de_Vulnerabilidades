############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa TAAB017  => CONSULTA NSS NO ATENDIDOS AFORE RECEPTORA            #
#Fecha             => 10 DE OCTUBRE DE 2001                                #
#Autor             => MAURO MUNIZ CABALLERO                                #
#Sistema           => TAA                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg               ARRAY[30000] OF RECORD
        n_seguro             CHAR(11),
        curp                 CHAR(18),
        tipo_traspaso        CHAR(02),
        estado             SMALLINT,
        clave_afore          SMALLINT,
        rech_proc_operat           CHAR(3),
        fecha_presentacion   DATE    ,
        f_actualiza          DATE
    END RECORD

    DEFINE 
        n_seguro             CHAR(11),
        fecha_presentacion   DATE    ,
        f_actualiza          DATE    ,
        folio                INTEGER ,
        total_reg            INTEGER ,
        pfecha_presentacion  DATE

    DEFINE
        g_afore              RECORD LIKE tab_afore_local.*,
        g_paramgrales        RECORD LIKE seg_modulo.*

    DEFINE
        HOY                  DATE

    DEFINE
        enter                CHAR(1),
        HORA                 CHAR(8),
        g_usuario            CHAR(8) 

    DEFINE
        i                    SMALLINT  ,
        pos                  INTEGER   ,
        sel_where            CHAR(1000),
        cla_where            CHAR(1500)

    DEFINE sum_reg           ARRAY[30] OF RECORD
        fecha_presentacion   DATE,
        tipo_traspaso        CHAR(02),
        desc_tipo_trasp      CHAR(80),
        tot_reg_det          SMALLINT
    END RECORD

    DEFINE
        pos2                 SMALLINT,
        total_sum            SMALLINT

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP

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

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TAAB0171" ATTRIBUTE( BORDER)

    DISPLAY " TAAB017         CONSULTA TRASPASOS NO ATENDIDOS RECEPTORA                     " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    DISPLAY " SAFRE v2.0 (OP 14-1) " AT 4,1

    MENU " TRASPASOS RECIBIDOS "
        COMMAND "Consulta" " Consulta No Atendidos"
            CALL Consulta()
        COMMAND "Salir " " Salir de Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION Inicializa()
#iz------------------

    DEFINE j SMALLINT

    INITIALIZE reg TO NULL

    FOR j = 1 TO 12
        DISPLAY reg[i].* TO scr_1[i].* ATTRIBUTE (NORMAL)
    END FOR

    CLEAR FORM

END FUNCTION

FUNCTION Consulta()
#C-----------------

    DISPLAY "   NSS              CURP      TRASP    EDO   AFO   RECH     FPRES  FRECEP" AT 5,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    CONSTRUCT cla_where ON a.n_seguro,
                           a.curp,
                           a.tipo_traspaso,
                           a.estado,
                           a.cve_ced_cuenta, 
                          a.rech_proc_operat,
                           a.fecha_presentacion,
                           a.f_actualiza
                      FROM n_seguro,
                           curp,
                           tipo_traspaso,
                           estado,
                           cve_ced_cuenta,
                           rech_proc_operat,
                           fecha_presentacion,
                           f_actualiza

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = " SELECT a.n_seguro,",
                           " a.curp,",
                           " a.tipo_traspaso,",
                           " a.estado,",
                           " a.cve_ced_cuenta,",
                           " a.rech_proc_operat,",
                           " a.fecha_presentacion,",
                           " a.f_actualiza",
                    " FROM   taa_det_no_aten a, ",
                    " OUTER  tab_tipo_traspaso b, ",
                    " OUTER  tab_afore c", 
                    " WHERE ", cla_where CLIPPED,
                    " AND a.tipo_traspaso = b.tipo_traspaso",
                    " AND a.cve_ced_cuenta = c.afore_cod",
                    " AND b.id_opera = '09'",
                    " ORDER BY a.fecha_presentacion DESC,",
                    " a.tipo_traspaso, a.n_seguro"

    LET sel_where = sel_where CLIPPED

    #LET sel_where = "echo ",sel_where clipped, " > x.sql"  
    #RUN sel_where

    PREPARE qry_consul FROM sel_where 

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    FOREACH cursor_c INTO reg[pos].n_seguro THRU reg[pos].f_actualiza
        --CASE reg[pos].estado
            --WHEN  1 LET reg[pos].desc_estado = 'ORDINARIO'
            --WHEN 12 LET reg[pos].desc_estado = 'UNIFACION'
            --WHEN 51 LET reg[pos].desc_estado = 'ASIGNACION'
        --END CASE

        --LET reg[pos].desc_estado = 'NO ATENDIDA'

        LET pos = pos + 1
    END FOREACH

    LET total_reg = pos - 1

    DISPLAY BY NAME total_reg

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY reg TO scr_1.*
        ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

          ON KEY(CONTROL-P)
             CALL imprime_reporte()

          ON KEY(CONTROL-M)
             LET pos = ARR_CURR()
             LET pfecha_presentacion = reg[pos].fecha_presentacion
             CALL muestra_sumatoria(pfecha_presentacion)

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
        END IF
    ELSE
        ERROR "REGISTROS CON ESAS CONDICIONES NO EXISTEN"
        SLEEP 2
        ERROR ""
    END IF

END FUNCTION

FUNCTION muestra_sumatoria(pfecha_presentacion)

  DEFINE
    pfecha_presentacion      DATE

  OPEN WINDOW ventana_2 AT 6,6 WITH FORM "TAAB0172" ATTRIBUTE(BORDER)
    DISPLAY " TAAB017   SUMATORIA REGISTROS POR TIPO DE TRASPASO                            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,58 ATTRIBUTE(REVERSE)
    DISPLAY " SAFRE v2.0 (OP 14-1) " AT 4,1

    DISPLAY "  FECHA PRES  TIPO TRASP                                 TOTAL                 " AT 5,1 ATTRIBUTE(REVERSE)

    LET pos2      = 1
    LET total_sum = 0

    DECLARE c2 CURSOR FOR
    SELECT a.fecha_presentacion, a.tipo_traspaso, b.descripcion, a.tot_reg_det
    FROM   taa_sum_no_aten_tt a, OUTER tab_tipo_traspaso b
    WHERE  a.tipo_traspaso      = b.tipo_traspaso
    AND    a.fecha_presentacion = pfecha_presentacion
    AND    b.id_opera           = '09'
    ORDER BY 1,2
    FOREACH c2 INTO sum_reg[pos2].fecha_presentacion THRU 
                    sum_reg[pos2].tot_reg_det

        LET total_sum = total_sum + sum_reg[pos2].tot_reg_det

        LET pos2 = pos2 + 1
    END FOREACH

    DISPLAY BY NAME total_sum

    IF (pos2-1) >= 1 THEN
        CALL SET_COUNT(pos2-1)
        DISPLAY ARRAY sum_reg TO scr_2.*
        ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

          ON KEY(CONTROL-P)
             CALL imprime_reporte2()

          ON KEY (CONTROL-M)
             CLEAR FORM
             EXIT DISPLAY

          ON KEY (INTERRUPT)
             LET int_flag = TRUE
             CLEAR FORM
             EXIT DISPLAY

        END DISPLAY

        IF int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR "BUSQUEDA TERMINADA..."
            SLEEP 2
            ERROR ""
            CLEAR FORM
            --RETURN
        END IF
    ELSE
        ERROR "REGISTROS CON ESAS CONDICIONES NO EXISTEN"
        SLEEP 2
        ERROR ""
    END IF
 
  CLOSE WINDOW ventana_2

END FUNCTION

FUNCTION imprime_reporte()

  DEFINE tot_array      INTEGER
  DEFINE i              INTEGER
  DEFINE v_ruta         CHAR(400)
  DEFINE comando        CHAR(400)
  DEFINE rtipo_consulta SMALLINT
  DEFINE mensaje1       CHAR(25)
  DEFINE mensaje2       CHAR(25)
  DEFINE mensaje3       CHAR(25)
  DEFINE mensaje4       CHAR(25)
  DEFINE mensaje5       CHAR(25)
  DEFINE vn_rep         CHAR(02)

  LET v_ruta   = g_paramgrales.ruta_listados CLIPPED,"/L_", g_usuario CLIPPED,
                 ".CONS_14TAA_01_", HOY USING "DDMMYYYY", "_",
                 HORA[1,2], HORA[4,5], HORA[7,8]

  START REPORT rep_op14 TO v_ruta

    LET pos = pos - 1
    
    FOR i = 1 TO pos
        OUTPUT TO REPORT rep_op14(reg[i].*)
    END FOR

  FINISH REPORT rep_op14

  ERROR "LISTADO GENERADO EN LA RUTA:", v_ruta
  SLEEP 3
  ERROR ""

  --LET comando = " lp ", v_ruta CLIPPED
  --RUN comando

END FUNCTION

REPORT rep_op14(reg)

  DEFINE reg RECORD
    n_seguro           CHAR(11),
    curp               CHAR(18),
    tipo_traspaso      CHAR(02),
    estado             SMALLINT,
    clave_afore        SMALLINT,
    rech_proc_operat           CHAR(3),
    fecha_presentacion  DATE,
    f_actualiza        DATE
  END RECORD

  DEFINE lsubtotal      INTEGER

  DEFINE
    codigo_afore        SMALLINT,
    razon_social        CHAR(50)

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0
    PAGE LENGTH   90

    ORDER EXTERNAL BY reg.tipo_traspaso

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
      PRINT COLUMN 001,"SAFRE v2.0 (OP 14-1)                            "
      PRINT COLUMN 001,"PROGRAMA: TAAB017 ",
            COLUMN 025,"LISTADO DE CONSULTA TRASPASOS NO ATENDIDOS OP 14",
            COLUMN 110,"FECHA: ",TODAY USING "dd-mm-yyyy"
      PRINT

      PRINT
      PRINT COLUMN 001,"NSS",
            COLUMN 018,"CURP",
            COLUMN 042,"TIPO TRASPASO",
            COLUMN 051, " ESTADO",
            COLUMN 058," AFORE",
            COLUMN 067, " RECHAZO PROC OP",
            COLUMN 084," FECHA PRES",
            COLUMN 095," FECHA RECEP" 

      SKIP 1 LINE

    ON EVERY ROW

       PRINT COLUMN 001,reg.n_seguro,
             COLUMN 013,reg.curp,
             COLUMN 033,reg.tipo_traspaso USING "&&",
             COLUMN 047,reg.estado,
             COLUMN 070,reg.clave_afore USING "###",
             COLUMN 085,reg.rech_proc_operat,
             COLUMN 114,reg.fecha_presentacion USING "DD/MM/YYYY",
             COLUMN 132,reg.f_actualiza        USING "DD/MM/YYYY"

       AFTER GROUP OF reg.tipo_traspaso
         --LET lsubtotal = GROUP SUM(reg.tipo_traspaso)
         LET lsubtotal = GROUP COUNT(*)

         PRINT COLUMN 004, "Suma Subtotal Tipo de Traspaso ", 
                           reg.tipo_traspaso CLIPPED, 
                           ": ", lsubtotal USING "#,###,##&",
               COLUMN 110, (lsubtotal / total_reg) * 100 USING "##&.&&", "%"

       SKIP 1 LINES

    PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

    ON LAST ROW
       SKIP 1 LINE
       PRINT COLUMN 004,"TOTAL DE REGISTROS: ", total_reg USING "#,###,##&"

END REPORT

FUNCTION imprime_reporte2()

  DEFINE i2              INTEGER
  DEFINE v_ruta2         CHAR(400)
  DEFINE comando2        CHAR(400)

  LET v_ruta2 = g_paramgrales.ruta_listados CLIPPED,"/L_", g_usuario CLIPPED,
                ".CONS_14TAA_02_", HOY USING "DDMMYYYY", "_",
                HORA[1,2], HORA[4,5], HORA[7,8]

  START REPORT rep_op14a TO v_ruta2

    LET pos2 = pos2 - 1

    FOR i2 = 1 TO pos2
        OUTPUT TO REPORT rep_op14a(sum_reg[i2].*)
    END FOR

  FINISH REPORT rep_op14a

  ERROR "LISTADO GENERADO EN LA RUTA:", v_ruta2
  SLEEP 2
  ERROR ""

  --LET comando = " lp ", v_ruta2 CLIPPED
  --RUN comando

END FUNCTION

REPORT rep_op14a(sum_reg)

  DEFINE sum_reg RECORD
    fecha_presentacion  DATE,
    tipo_traspaso       CHAR(02),
    desc_tipo_trasp     CHAR(80),
    tot_reg_det         SMALLINT
  END RECORD

  DEFINE
    codigo_afore        SMALLINT,
    razon_social        CHAR(50)

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0
    PAGE LENGTH   90

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
      PRINT COLUMN 001,"SAFRE v2.0 (OP 14-1)                            "
      PRINT COLUMN 001,"PROGRAMA: TAAB017 ",
            COLUMN 025,"LISTADO DE CONSULTA SUMATORIA POR TIPO DE TRASPASO SOLICITUDES NO ATENDIDAS OP 14",
            COLUMN 110,"FECHA: ",TODAY USING "dd-mm-yyyy"
      PRINT

      PRINT
      PRINT COLUMN 001,"FECHA PRES",
            COLUMN 012,"TIPO TRASPASO",
            COLUMN 070,"TOTAL"

      SKIP 1 LINE

    ON EVERY ROW

       PRINT COLUMN 001,sum_reg.fecha_presentacion USING "DD/MM/YYYY",
             COLUMN 012,sum_reg.tipo_traspaso USING "&&",
             COLUMN 015,sum_reg.desc_tipo_trasp[1,55],
             COLUMN 070,sum_reg.tot_reg_det USING "#,###,##&"

       SKIP 1 LINES

    PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

    ON LAST ROW
       SKIP 1 LINE
       PRINT COLUMN 001,"TOTAL DE REGISTROS: ",
             COLUMN 070,total_sum USING "#,###,##&"

END REPORT
