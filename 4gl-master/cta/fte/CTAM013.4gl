######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Owner             => E.F.P.                                         #
#Programa CTAM013  => CONSULTA CUOTA DE MERCADO - DOMICILIO INCORREC.#
#Fecha             => 21 de Julio de 2010.                           #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                     #
#Sistema           => CTA.                                           #
######################################################################
DATABASE safre_af

GLOBALS
  DEFINE g_reg1       RECORD
    tipo_solicitud    SMALLINT,
    desc_solicitud    CHAR(30),
    n_folio           DECIMAL(10,0),
    n_seguro          CHAR(011),
    nombre            CHAR(120)
  END RECORD

  DEFINE l_record1    ARRAY[1000] OF RECORD
    tipo_solicitud    CHAR(40),
    n_folio           DECIMAL(10,0),
    n_seguro          CHAR(011),
    nombre            CHAR(120),
    domicilio         CHAR(300),
    marca_envio       CHAR(1),
    rechazo           CHAR(40)
  END RECORD

  DEFINE g_reg2       RECORD
    tipo_solicitud    CHAR(40),
    n_folio           DECIMAL(10,0),
    n_seguro          CHAR(011),
    nombre            CHAR(120),
    domicilio         CHAR(300),
    marca_envio       CHAR(1)
  END RECORD
   
  DEFINE g_param_dis  RECORD LIKE seg_modulo.*

  DEFINE sw_1         SMALLINT,
    aux_pausa         CHAR(01),
    HOY               DATE,
    aux_estad_desc    CHAR(40),
    seg_usuario       CHAR(08),
    pos               SMALLINT,
    cla_where         CHAR(900),
    sel_where         CHAR(900),
    sel_where1        CHAR(1500),
    pos2              SMALLINT,
    g_lista           CHAR(300),
    g_impre           CHAR(300),
    vfecha_ini        DATE,
    gtotal            INTEGER,
    l                 SMALLINT,
    vnss              CHAR(11),
    vcurp             CHAR(18),
    vtot_reg          SMALLINT,
    vfactualiza_reg   DATE,
    vrowid_reg        INTEGER,
    sel_his           CHAR(2500)

  DEFINE
    total_arh         INTEGER,
    total_reg         INTEGER,
    total_sd          INTEGER,
    total_dd          INTEGER,
    f_estado          SMALLINT,
    dom_duplicado     SMALLINT,
    hora              CHAR(8)

END GLOBALS
##########################################################################
MAIN
  OPTIONS 
    PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY CONTROL-O

  DEFER INTERRUPT

  CALL STARTLOG("CTAM013.log")
  CALL inicio()
  CALL proceso()

END MAIN
##########################################################################
FUNCTION inicio()

  SELECT USER,*
  INTO   seg_usuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'cta'

  SELECT ruta_listados, modulo_desc
  INTO   g_param_dis.ruta_listados, g_param_dis.modulo_desc
  FROM   seg_modulo
  WHERE  modulo_cod = 'cta'

  LET hoy  = TODAY
  LET hora = TIME

END FUNCTION
##########################################################################
FUNCTION proceso()

  LET HOY = TODAY

  OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CTAM0131" ATTRIBUTE( BORDER)
    DISPLAY " CTAM013   CONSULTA TRABAJADORES CON DOMICILIO INCONSISTENTE                       " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)
    DISPLAY " SAFRE v2.0 (1)" AT 4,1

    MENU "CONSULTA"
      COMMAND "Consulta" "Consulta Trabajadores con Domicilio Inconsistente"
        CALL Inicializa()
        CALL Consulta()
        CALL Inicializa()
      COMMAND "Salir" "Salir del Programa"
        EXIT MENU
    END MENU

END FUNCTION
#############################################################################
FUNCTION Inicializa()
  DEFINE j SMALLINT

  LET j               = 0
  LET sw_1            = 0
  LET total_arh       = 0
  LET total_reg       = 0
  LET total_sd        = 0
  LET total_dd        = 0

  DISPLAY "                                                                                      " AT 19,1

  INITIALIZE g_reg1.*  TO NULL
  INITIALIZE l_record1 TO NULL

  FOR j = 1 TO 1      
      INITIALIZE l_record1[j].* TO NULL
      DISPLAY l_record1[j].*
  END FOR

  DISPLAY "                                                                               " AT 16,1

END FUNCTION
################################################################################
FUNCTION Consulta()

  DISPLAY "" AT 1,1
  DISPLAY "" AT 2,1
  DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
  DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

  LET pos = 2

  IF (pos-1) >= 1 THEN
     CALL  SET_COUNT(pos-1)

     LET int_flag = FALSE

     CONSTRUCT cla_where ON a.n_seguro
                       FROM n_seguro

       BEFORE CONSTRUCT
         BEFORE FIELD n_seguro
           MESSAGE "Capture el NSS."

         AFTER FIELD n_seguro
           MESSAGE ""
           EXIT CONSTRUCT
       
       ON KEY (CONTROL-M)
          LET int_flag = FALSE
          EXIT CONSTRUCT

       ON KEY (CONTROL-C)
          LET int_flag = TRUE
          EXIT CONSTRUCT
     END CONSTRUCT

     IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
     END IF

     ERROR "Generando información. . ."

     DISPLAY BY NAME total_reg
     DISPLAY BY NAME total_sd
     DISPLAY BY NAME total_dd

     LET sel_where = "SELECT a.tipo_solicitud,",
                     " b.desc_solicitud,",
                     " a.n_folio,",
                     " a.n_seguro,",
                     " NVL(TRIM(c.nombres), ' ') || ' ' ||",    
                     " NVL(TRIM(c.paterno), ' ') || ' ' ||",
                     " NVL(TRIM(c.materno), ' ')",
                     " FROM   safre_tmp:cuota_cta_est a,",
                     "        tab_tipo_solic b,",
                     "        afi_mae_afiliado c",
                     " WHERE ", cla_where CLIPPED,
                     " AND a.tipo_solicitud <> 5",
                     " AND a.n_seguro = c.n_seguro",
                     " AND a.tipo_solicitud = b.tipo_solicitud"
     PREPARE query_p FROM sel_where
     DECLARE cursor_p CURSOR FOR query_p

     LET pos       = 1
     LET total_arh = 0
     FOREACH cursor_p INTO g_reg1.*

       ---- SIN DOMICILIO ----
       LET f_estado = NULL

       SELECT MAX(B.estado)
       INTO   f_estado
       FROM   safre_af:afi_domicilio B
       WHERE  B.nss            = g_reg1.n_seguro
       AND    B.n_folio        = g_reg1.n_folio
       AND    B.tipo_solicitud = g_reg1.tipo_solicitud
       AND    B.marca_envio    = "X"
       IF f_estado IS NULL THEN

          LET total_reg                     = total_reg + 1
          LET total_sd                      = total_sd  + 1

          LET l_record1[pos].tipo_solicitud = g_reg1.tipo_solicitud USING "<<",
                                              " ", g_reg1.desc_solicitud CLIPPED
          LET l_record1[pos].n_folio        = g_reg1.n_folio
          LET l_record1[pos].n_seguro       = g_reg1.n_seguro
          LET l_record1[pos].nombre         = g_reg1.nombre CLIPPED
          LET l_record1[pos].domicilio      = NULL
          LET l_record1[pos].marca_envio    = NULL
          LET l_record1[pos].rechazo        = 'TRABAJADOR SIN DOMICILIO'

          LET pos       = pos + 1
       ELSE 
          ---- DOMICILIO DUPLICADO POR MARCA ENVIO ----
          SELECT COUNT(*)
          INTO   dom_duplicado
          FROM   safre_af:afi_domicilio b
          WHERE  b.nss            = g_reg1.n_seguro
          AND    b.n_folio        = g_reg1.n_folio
          AND    b.tipo_solicitud = g_reg1.tipo_solicitud
          AND    b.marca_envio    = "X"
          IF dom_duplicado > 1 THEN 

             LET sel_where1 = "SELECT a.tipo_solicitud || ' ' || ",
                              "b.desc_solicitud, ",
                              "a.n_folio, ",
                              "a.nss, ",
                              "NVL(TRIM(c.nombres), ' ') || ' ' || ",
                              "NVL(TRIM(c.paterno), ' ') || ' ' || ",
                              "NVL(TRIM(c.materno), ' '), ",
                              "'CALLE ' || '' || ",
                              "NVL(TRIM(a.calle),   ' ') || ', ' || ",
                              "'NUM EXT ' || '' || ",
                              "NVL(TRIM(a.numero),  ' ') || ', ' || ",
                              "'NUM INT ' || '' || ",
                              "NVL(TRIM(a.depto),   ' ') || ', ' || ",
                              "'COLONIA ' || '' || ",
                              "NVL(TRIM(a.colonia), ' ') || ', ' || ",
                              "'DELEG ' || '' || ",
                              "NVL(a.delega, '') || ' ' || ",
                              "NVL(TRIM(d.deleg_desc), ' ') || ', ' || ",
                              "'CIUDAD ' || '' || ",
                              "NVL(a.ciudad, '') || ' ' || ",
                              "NVL(TRIM(e.ciudad_desc), ' ') || ', ' || ",
                              "'ESTADO ' || '' || ",
                              "NVL(a.estado, '') || ' ' || ",
                              "NVL(TRIM(f.estad_desc), ' ') || ', ' || ",
                              "'CP ' || '' || ",
                              "a.codpos, ",
                              "a.marca_envio ",
                       "FROM   afi_domicilio a, ",
                              "tab_tipo_solic b, ",
                              "afi_mae_afiliado c, ",
                              "tab_delegacion d, ",
                              "tab_ciudad e, ",
                              "tab_estado f ",
                       "WHERE  a.nss            = ", g_reg1.n_seguro,
                       "AND    a.n_folio        = ", g_reg1.n_folio,
                       "AND    a.tipo_solicitud = ", g_reg1.tipo_solicitud,
                       "AND    a.nss            = c.n_seguro ",
                       "AND    a.n_folio        = c.n_folio ",
                       "AND    a.tipo_solicitud = c.tipo_solicitud ",
                       "AND    a.tipo_solicitud = b.tipo_solicitud ",
                       "AND    a.delega         = d.deleg_cod ",
                       "AND    a.ciudad         = e.ciudad_cod ",
                       "AND    a.estado         = f.estad_cod "

             PREPARE query_d FROM sel_where1
             DECLARE cursor_d CURSOR FOR query_d

             FOREACH cursor_d INTO g_reg2.*
               LET total_reg                     = total_reg + 1
               LET total_dd                      = total_dd  + 1

               LET l_record1[pos].tipo_solicitud = g_reg2.tipo_solicitud CLIPPED
               LET l_record1[pos].n_folio        = g_reg2.n_folio
               LET l_record1[pos].n_seguro       = g_reg2.n_seguro
               LET l_record1[pos].nombre         = g_reg2.nombre CLIPPED
               LET l_record1[pos].domicilio      = g_reg2.domicilio CLIPPED
               LET l_record1[pos].marca_envio    = g_reg2.marca_envio
               LET l_record1[pos].rechazo        = 'INDICADOR DE MARCA DE ENVIO DUPLICADO'
               LET pos                           = pos + 1
             
             END FOREACH
          END IF
       END IF
     END FOREACH

     ERROR "Proceso finalizado. . ."

     DISPLAY BY NAME total_reg
     DISPLAY BY NAME total_sd
     DISPLAY BY NAME total_dd

     IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        DISPLAY ARRAY l_record1 TO scr_1.*

          ON KEY (CONTROL-P)
             ERROR "PROCESANDO IMPRESION..."
             CALL impresion(pos)

          ON KEY (INTERRUPT)
             INITIALIZE l_record1 TO NULL
             DISPLAY "                                                                               " AT 19,1
             CLEAR FORM
             EXIT DISPLAY
        END DISPLAY

        RETURN
        CLOSE WINDOW ventana_1
     ELSE
        ERROR "CONSULTA VACIA"
        SLEEP 2
        ERROR ""
        CLEAR FORM
        RETURN
     END IF

     DISPLAY "" AT 1,1
     DISPLAY "" AT 2,1
     DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
     DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

     DISPLAY BY NAME  g_reg1.*
  END IF

  CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION impresion(pos)

  DEFINE r_reg1       RECORD
    tipo_solicitud    CHAR(40),
    n_folio           DECIMAL(10,0),
    n_seguro          CHAR(011),
    nombre            CHAR(120),
    domicilio         CHAR(300),
    marca_envio       CHAR(1),
    rechazo           CHAR(40)
  END RECORD

  DEFINE
    i,
    pos               SMALLINT

  LET g_impre = g_param_dis.ruta_listados CLIPPED,"/","L_", seg_usuario CLIPPED,
                ".CONS_DOM_TRAB_01_", hoy USING "ddmmyyyy" CLIPPED, "_",
                hora[1,2], hora[4,5], hora[7,8]

  DISPLAY "Nombre reporte: ", g_impre AT 19,1

  START REPORT rpt_tabrdeta TO g_impre

    FOR i = 1 TO (pos - 1)
       LET r_reg1.tipo_solicitud = l_record1[i].tipo_solicitud
       LET r_reg1.n_folio        = l_record1[i].n_folio
       LET r_reg1.n_seguro       = l_record1[i].n_seguro
       LET r_reg1.nombre         = l_record1[i].nombre
       LET r_reg1.domicilio      = l_record1[i].domicilio
       LET r_reg1.marca_envio    = l_record1[i].marca_envio
       LET r_reg1.rechazo        = l_record1[i].rechazo

       IF r_reg1.n_folio IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tabrdeta(i, r_reg1.*)
    END FOR
  
    FINISH REPORT rpt_tabrdeta

    ERROR "LISTADO GENERADO..."
    SLEEP 2
    ERROR ""

    --LET g_lista = "lp ",g_impre
    --LET g_lista = "vi ",g_impre
    --RUN g_lista
END FUNCTION
#####################################################################

################################################################################
REPORT rpt_tabrdeta(li, lg_reg2)

   DEFINE li           SMALLINT

   DEFINE lg_reg2      RECORD
     tipo_solicitud    CHAR(40),
     n_folio           DECIMAL(10,0),
     n_seguro          CHAR(011),
     nombre            CHAR(120),
     domicilio         CHAR(300),
     marca_envio       CHAR(1),
     rechazo           CHAR(40)
   END RECORD

   DEFINE 
     codigo_afore      SMALLINT,
     razon_social      CHAR(50)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d' 

         SELECT a.codigo_afore, a.razon_social
           INTO codigo_afore, razon_social
           FROM tab_afore_local a

         PRINT COLUMN 001,"AFORE : ", codigo_afore USING "###", " ",
                          razon_social CLIPPED
         PRINT
         PRINT COLUMN 001,"MODULO: ", g_param_dis.modulo_desc CLIPPED
         PRINT
         PRINT COLUMN 001,"SAFRE v2.0 (1)"
         PRINT COLUMN 001,"PROGRAMA: CTAM013 ",
               COLUMN 055,"LISTADO DE CONSULTA TRABAJADORES CON DOMICILIO INCONSISTENTE",
               COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
         PRINT

         PRINT COLUMN 001,"ID",
               COLUMN 007,"TIPO SOLICITUD",
               COLUMN 040,"FOLIO SOL",
               COLUMN 052,"NSS",
               COLUMN 065,"NOMBRE",
               COLUMN 100,"DOMICILIO",
               COLUMN 300,"MARCA ENVIO",
               COLUMN 315,"RECHAZO"

      ON EVERY ROW
         PRINT COLUMN 001, li USING "<<<<<",
               COLUMN 007, lg_reg2.tipo_solicitud CLIPPED,
               COLUMN 040, lg_reg2.n_folio USING "<<<<<<<<<<",
               COLUMN 052, lg_reg2.n_seguro CLIPPED,
               COLUMN 065, lg_reg2.nombre CLIPPED,
               COLUMN 100, lg_reg2.domicilio CLIPPED,
               COLUMN 300, lg_reg2.marca_envio CLIPPED,
               COLUMN 315, lg_reg2.rechazo CLIPPED

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 01," Total de registros                        : ",
         COUNT(*) USING "<<<&"
         PRINT COLUMN 01," Total de registros sin domicilio          : ",
         total_sd USING "<<<&"
         PRINT COLUMN 01," Total de registros con domicilio duplicado: ",
         total_dd USING "<<<&"

END REPORT
#####################################################################
