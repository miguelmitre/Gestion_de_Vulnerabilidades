######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Owner             => E.F.P.                                         #
#Programa CTAB130  => CONSULTA RECEPCION ARCHIVO OP 99 REGISTROS CON #
#                     SALDO CERO.                                    #
#Fecha             => 08 de Octubre de 2009.                         #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                     #
#Sistema           => CTA.                                           #
######################################################################
DATABASE safre_af

GLOBALS
  DEFINE g_reg        RECORD
    fecha_corte       DATE,
    nss               CHAR(11),
    curp              CHAR(18)
  END RECORD

  DEFINE l_record     ARRAY[30000] OF RECORD 
    fecha_corte       DATE,
    nss               CHAR(11),
    curp              CHAR(18)
  END RECORD

  DEFINE g_reg2       RECORD
    cfecha_corte      DATE,
    cnss              CHAR(11),
    ccurp             CHAR(18),
    ctipo_trabajador  CHAR(02),
    cdesc_tipo_trab   CHAR(20),
    cid_registro      CHAR(02),
    cdesc_id_registro CHAR(35),
    cdiagnostico      CHAR(03),
    cdesc_diagnostico CHAR(75)
  END RECORD

  DEFINE l_record2    ARRAY[30000] OF RECORD 
    cfecha_corte      DATE,
    cnss              CHAR(11),
    ccurp             CHAR(18),
    ctipo_trabajador  CHAR(02),
    cdesc_tipo_trab   CHAR(20),
    cid_registro      CHAR(02),
    cdesc_id_registro CHAR(35),
    cdiagnostico      CHAR(03),
    cdesc_diagnostico CHAR(75)
  END RECORD

  DEFINE l_record3    ARRAY[1000] OF RECORD
    nombre            CHAR(120)
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
    pos2              SMALLINT,
    cla_where2        CHAR(2000),
    sel_where2        CHAR(2000),
    g_lista           CHAR(300),
    g_impre           CHAR(300),
    g_lista2          CHAR(300),
    g_impre2          CHAR(300),
    g_impre3          CHAR(300),
    vfecha_ini        DATE,
    gtotal            INTEGER,
    l                 SMALLINT

  DEFINE
    total_arh         INTEGER,
    total_reg         INTEGER

END GLOBALS
##########################################################################
MAIN
  OPTIONS 
    PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY control-o

  DEFER INTERRUPT

  CALL STARTLOG("CTAB130.log")
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

END FUNCTION
##########################################################################
FUNCTION proceso()

  LET HOY = TODAY

  OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CTAB1301" ATTRIBUTE( BORDER)
    DISPLAY " CTAB130  CONSULTA RECEPCION ARCHIVO OP 99 REGISTROS SALDO CERO                    " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    MENU "CONSULTA"
      COMMAND "Recepcion Op 99" "Consulta Recepcion Archivo Op 99"
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

  INITIALIZE g_reg.*  TO NULL
  INITIALIZE l_record TO NULL

  FOR j = 1 TO 10      
      INITIALIZE l_record[j].* TO NULL
      DISPLAY l_record[j].*

      INITIALIZE l_record2[j].* TO NULL
      DISPLAY l_record2[j].*
  END FOR

  DISPLAY "                                                                               " AT 16,1

END FUNCTION
################################################################################
FUNCTION Consulta()

  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_diag_sdo_cero
    CREATE TEMP TABLE tmp_diag_sdo_cero (fecha_corte DATE,
                                         nss         CHAR(11),
                                         curp        CHAR(18));
  WHENEVER ERROR STOP

  LET pos = 2

  DISPLAY BY NAME g_reg.*   
  DISPLAY BY NAME total_arh 

  IF (pos-1) >= 1 THEN
     CALL  SET_COUNT(pos-1)

     LET int_flag = FALSE

     CONSTRUCT cla_where ON fecha_corte,
                            nss,
                            curp
                       FROM fecha_corte,
                            nss,
                            curp
       
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

     LET sel_where = "SELECT fecha_corte, nss, curp",
                     " FROM  cta_saldo_cero",
                     " WHERE ", cla_where CLIPPED,
                     " ORDER BY fecha_corte, nss, curp"

     PREPARE query_p FROM sel_where
     DECLARE cursor_p CURSOR FOR query_p

     LET pos       = 1
     LET total_arh = 0
     FOREACH cursor_p INTO l_record[pos].*

       LET g_reg.fecha_corte = l_record[pos].fecha_corte
       LET g_reg.nss         = l_record[pos].nss
       LET g_reg.curp        = l_record[pos].curp

       INSERT INTO tmp_diag_sdo_cero VALUES (g_reg.*)

       LET total_arh = total_arh + 1
       LET pos       = pos + 1

     END FOREACH

     DISPLAY BY NAME total_arh

     IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        DISPLAY ARRAY l_record TO scr_1.*
        ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
          ON KEY (CONTROL-M)
             LET pos = ARR_CURR()
             CALL extrae_datos()
             INITIALIZE l_record TO NULL
             EXIT DISPLAY

          ON KEY (INTERRUPT)
             INITIALIZE l_record TO NULL
             CLEAR FORM
             EXIT DISPLAY
        END DISPLAY

        RETURN
        CLOSE WINDOW ventana_1
     ELSE
        ERROR "CONSULTA RECEPCION ARCHIVO OP 99 VACIA"
        SLEEP 2
        ERROR ""
        CLEAR FORM
        RETURN
     END IF

     DISPLAY "" AT 1,1
     DISPLAY "" AT 2,1
     DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
     DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

     DISPLAY BY NAME  g_reg.*
  END IF

  CLEAR SCREEN
END FUNCTION
################################################################################

################################################################################
FUNCTION extrae_datos()
  DEFINE g_impre1      CHAR(300)

  DEFINE sel_ins       CHAR(900)

  ERROR "Desplegando detalle." SLEEP 2

  LET sel_where2 = "SELECT a.fecha_corte,",
                   "       a.nss,",
                   "       a.curp,",
                   "       b.tipo_trabajador,",
                   "       c.desc_tipo_trabajador,",
                   "       b.id_cuenta,",
                   "       d.desc_id_registro,",
                   "       b.diagnostico,",
                   "       e.desc_diagnostico",
                   " FROM  tmp_diag_sdo_cero a,",
                   "       cta_saldo_cero b,",
                   " OUTER tab_tipo_trabajador c,",
                   " OUTER tab_id_reg_sdo_cero d,",
                   " OUTER tab_diag_sdo_cero e",
                   " WHERE a.fecha_corte     = b.fecha_corte",
                   " AND   a.nss             = b.nss",
                   " AND   b.curp IS NOT NULL",
                   " AND   b.tipo_trabajador = c.tipo_trabajador",
                   " AND   c.operacion       = '99'",
                   " AND   b.id_cuenta       = d.id_registro",
                   " AND   b.diagnostico     = e.diagnostico",
                   " UNION ALL ",
                   "SELECT a.fecha_corte,",
                   "       a.nss,",
                   "       a.curp,",
                   "       b.tipo_trabajador,",
                   "       c.desc_tipo_trabajador,",
                   "       b.id_cuenta,",
                   "       d.desc_id_registro,",
                   "       b.diagnostico,",
                   "       e.desc_diagnostico",
                   " FROM  tmp_diag_sdo_cero a,",
                   "       cta_saldo_cero b,",
                   " OUTER tab_tipo_trabajador c,",
                   " OUTER tab_id_reg_sdo_cero d,",
                   " OUTER tab_diag_sdo_cero e",
                   " WHERE a.fecha_corte     = b.fecha_corte",
                   " AND   a.nss             = b.nss",
                   " AND   b.curp IS NULL",
                   " AND   b.tipo_trabajador = c.tipo_trabajador",
                   " AND   c.operacion       = '99'",
                   " AND   b.id_cuenta       = d.id_registro",
                   " AND   b.diagnostico     = e.diagnostico",
                   " ORDER BY 1,8,2,3"

  LET sel_where2 = sel_where2 CLIPPED
  
  LET pos2 = 2

  IF (pos2-1) >= 1 THEN
     CALL  SET_COUNT(pos2-1)

     PREPARE query2 FROM sel_where2
     DECLARE cursor_2 CURSOR FOR query2

     LET pos2      = 1
     LET total_reg = 0

     FOREACH cursor_2 INTO l_record2[pos2].*

       LET pos2      = pos2      + 1
       LET total_reg = total_reg + 1    

     END FOREACH

     DISPLAY BY NAME total_reg

     ERROR "Consulta finalizada. . ."

     IF (pos2-1) >= 1 THEN
        CALL SET_COUNT(pos2-1)

        DISPLAY ARRAY l_record2 TO scr_2.*
        ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
        ON KEY (CONTROL-M)
           LET pos2 = ARR_CURR()
           LET g_reg2.cfecha_corte       = l_record2[pos2].cfecha_corte
           LET g_reg2.cnss               = l_record2[pos2].cnss
           LET g_reg2.ccurp              = l_record2[pos2].ccurp
           LET g_reg2.ctipo_trabajador   = l_record2[pos2].ctipo_trabajador
           LET g_reg2.cdesc_tipo_trab    = l_record2[pos2].cdesc_tipo_trab
           LET g_reg2.cid_registro       = l_record2[pos2].cid_registro
           LET g_reg2.cdesc_id_registro  = l_record2[pos2].cdesc_id_registro
           LET g_reg2.cdiagnostico       = l_record2[pos2].cdiagnostico
           LET g_reg2.cdesc_diagnostico  = l_record2[pos2].cdesc_diagnostico

           CALL extrae_datos2(g_reg2.*)

        ON KEY (CONTROL-P)
           ERROR "PROCESANDO IMPRESION..."
           CALL impresion2(pos2)

        ON KEY (INTERRUPT)
           INITIALIZE l_record2 TO NULL
           CLEAR FORM
           EXIT DISPLAY
        END DISPLAY
     ELSE
       ERROR "CONSULTA DE STATUS VACIA"
       SLEEP 2
       ERROR ""
       CLEAR FORM
     END IF

     DISPLAY "" AT 1,1
     DISPLAY "" AT 2,1
     DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
     DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

  END IF

END FUNCTION

#####################################################################

################################################################################
FUNCTION impresion2(pos2)
   DEFINE
     i2,
     pos2    SMALLINT

   LET g_impre2 = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                  ".RECEP_DET_ARCH_OP99_",hoy USING "ddmmyyyy" CLIPPED

   DISPLAY "Nombre reporte: ", g_impre2 AT 20,1

   START REPORT rpt_tabrdeta2 TO g_impre2

   FOR i2 = 1 TO (pos2 - 1)
       LET g_reg2.cfecha_corte       = l_record2[i2].cfecha_corte
       LET g_reg2.cnss               = l_record2[i2].cnss
       LET g_reg2.ccurp              = l_record2[i2].ccurp
       LET g_reg2.ctipo_trabajador   = l_record2[i2].ctipo_trabajador
       LET g_reg2.cdesc_tipo_trab    = l_record2[i2].cdesc_tipo_trab
       LET g_reg2.cid_registro       = l_record2[i2].cid_registro
       LET g_reg2.cdesc_id_registro  = l_record2[i2].cdesc_id_registro
       LET g_reg2.cdiagnostico       = l_record2[i2].cdiagnostico
       LET g_reg2.cdesc_diagnostico  = l_record2[i2].cdesc_diagnostico

      IF g_reg2.cfecha_corte IS NULL THEN
         EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_tabrdeta2(i2, g_reg2.*)
   END FOR

   FINISH REPORT rpt_tabrdeta2

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   --LET g_lista2 = "lp ",g_impre2
   --LET g_lista2 = "vi ",g_impre2
   --RUN g_lista2
END FUNCTION
#####################################################################
#####################################################################
REPORT rpt_tabrdeta2(li2, g_reg2)

   DEFINE
     li2               SMALLINT

   DEFINE g_reg2       RECORD
     cfecha_corte      DATE,
     cnss              CHAR(11),
     ccurp             CHAR(18),
     ctipo_trabajador  CHAR(02),
     cdesc_tipo_trab   CHAR(20),
     cid_registro      CHAR(02),
     cdesc_id_registro CHAR(35),
     cdiagnostico      CHAR(03),
     cdesc_diagnostico CHAR(75)
   END RECORD

   DEFINE
     paterno           CHAR(40),
     materno           CHAR(40),
     nombres           CHAR(40)

   DEFINE
     codigo_afore      SMALLINT,
     razon_social      CHAR(50)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
      
      ORDER EXTERNAL BY g_reg2.cdiagnostico, g_reg2.ctipo_trabajador

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
         PRINT COLUMN 001,"PROGRAMA: CTAB130 ",
               COLUMN 035,"LISTADO DE CONSULTA DETALLE RECEPCION ARCHIVO OP 99 - REGISTROS CON SALDO CERO ",
               COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
         PRINT

         PRINT COLUMN 01,"ID",
               COLUMN 07,"FECHA CORTE",
               COLUMN 19,"NSS",
               COLUMN 32,"CURP",
               COLUMN 52,"NOMBRE"
         PRINT COLUMN 07,"TIPO TRABAJADOR",
               COLUMN 32,"IDENTIFICADOR REGISTRO"
         PRINT COLUMN 07,"DIAGNOSTICO"
         SKIP 1 LINE

      ON EVERY ROW
         SELECT a.nombres, a.paterno, a.materno
         INTO   nombres, paterno, materno
         FROM   afi_mae_afiliado a
         WHERE  a.n_seguro  = g_reg2.cnss
         IF STATUS = NOTFOUND THEN
            SELECT a.nombre, a.paterno, a.materno
            INTO   nombres, paterno, materno
            FROM   afi_mae_afiliado a
            WHERE  a.n_unico = g_reg2.ccurp
            IF STATUS = NOTFOUND THEN
               LET nombres = NULL
               LET paterno = NULL
               LET materno = NULL
            END IF
         END IF

         PRINT COLUMN 01,li2 USING "####&",
               COLUMN 07,g_reg2.cfecha_corte        USING "DD/MM/YYYY",
               COLUMN 19,g_reg2.cnss                CLIPPED,
               COLUMN 32,g_reg2.ccurp               CLIPPED,
               COLUMN 52,nombres                    CLIPPED, " ",
                         paterno                    CLIPPED, " ",
                         materno                    CLIPPED 

         #AFTER GROUP OF g_reg2.cnss
         AFTER GROUP OF g_reg2.ctipo_trabajador
           PRINT COLUMN 07,g_reg2.ctipo_trabajador  CLIPPED,
                 COLUMN 10,g_reg2.cdesc_tipo_trab   CLIPPED,
                 COLUMN 32,g_reg2.cid_registro      CLIPPED,
                 COLUMN 35,g_reg2.cdesc_id_registro CLIPPED
           PRINT COLUMN 07,g_reg2.cdiagnostico      CLIPPED,
                 COLUMN 11,g_reg2.cdesc_diagnostico CLIPPED
           
         AFTER GROUP OF g_reg2.cdiagnostico
           PRINT
           PRINT COLUMN 01," Total de registros : ",
           GROUP COUNT(*) USING "<<<<"
           PRINT

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 01," Total de registros : ",
         COUNT(*) USING "<<<<"

END REPORT

#####################################################################
FUNCTION extrae_datos2(f_reg2)

  DEFINE f_reg2       RECORD
    cfecha_corte      DATE,
    cnss              CHAR(11),
    ccurp             CHAR(18),
    ctipo_trabajador  CHAR(02),
    cdesc_tipo_trab   CHAR(20),
    cid_registro      CHAR(02),
    cdesc_id_registro CHAR(35),
    cdiagnostico      CHAR(03),
    cdesc_diagnostico CHAR(75)
  END RECORD

  DEFINE g_reg3       RECORD
    nss               CHAR(11),
    curp              CHAR(18),
    fecha_corte       DATE,
    nombre            CHAR(120),
    marca_cod         SMALLINT,
    marca_desc        CHAR(25),
    fecha_inicio      DATE,
    tipo_trabajador   CHAR(02),
    desc_tipo_trab    CHAR(20),
    id_registro       CHAR(02),
    desc_id_registro  CHAR(35),
    diagnostico       CHAR(03),
    desc_diagnostico  CHAR(75)
  END RECORD

  DEFINE
    nombres           CHAR(40),
    paterno           CHAR(40),
    materno           CHAR(40)

  DEFINE
    vrowid            INTEGER,
    vhora_ini         DATETIME HOUR TO SECOND 

  OPEN WINDOW ventana_2 AT 3,3 WITH FORM "CTAB1302" ATTRIBUTE( BORDER)
    DISPLAY " CTAB130           CONSULTA RECEPCION ARCHIVO OP 99                                " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    ERROR "Desplegando detalle." SLEEP 2

    INITIALIZE g_reg3.* TO NULL

    LET g_reg3.nss              = f_reg2.cnss
    LET g_reg3.curp             = f_reg2.ccurp
    LET g_reg3.fecha_corte      = f_reg2.cfecha_corte
    LET g_reg3.tipo_trabajador  = f_reg2.ctipo_trabajador
    LET g_reg3.desc_tipo_trab   = f_reg2.cdesc_tipo_trab
    LET g_reg3.id_registro      = f_reg2.cid_registro
    LET g_reg3.desc_id_registro = f_reg2.cdesc_id_registro
    LET g_reg3.diagnostico      = f_reg2.cdiagnostico
    LET g_reg3.desc_diagnostico = f_reg2.cdesc_diagnostico
        
    SELECT a.nombres, a.paterno, a.materno
    INTO   nombres, paterno, materno
    FROM   afi_mae_afiliado a
    WHERE  a.n_seguro  = g_reg2.cnss
    IF STATUS = NOTFOUND THEN
       SELECT a.nombre, a.paterno, a.materno, a.nss
       INTO   nombres, paterno, materno, g_reg3.nss
       FROM   afi_mae_afiliado a
       WHERE  a.n_unico = g_reg2.ccurp
       IF STATUS = NOTFOUND THEN
          LET nombres = NULL
          LET paterno = NULL
          LET materno = NULL  
       END IF
    END IF

    LET g_reg3.nombre = nombres CLIPPED, " ", 
                        paterno CLIPPED, " ",
                        materno CLIPPED

    LET g_reg3.fecha_inicio = ""
    LET vhora_ini           = ""
    LET g_reg3.marca_cod    = ""
    LET g_reg3.marca_desc   = "CUENTA LIBRE"

    SELECT MAX(a.fecha_ini)
    INTO   g_reg3.fecha_inicio
    FROM   cta_act_marca a
    WHERE  a.nss = g_reg3.nss
    IF STATUS <> NOTFOUND THEN
       SELECT MAX(hora_ini)
       INTO   vhora_ini
       FROM   cta_act_marca a
       WHERE  a.nss       = g_reg3.nss
       AND    a.fecha_ini = g_reg3.fecha_inicio
       IF STATUS <> NOTFOUND THEN 
          SELECT a.marca_cod, b.marca_desc
          INTO   g_reg3.marca_cod, g_reg3.marca_desc
          FROM   cta_act_marca a, tab_marca b
          WHERE  a.nss       = g_reg3.nss
          AND    a.marca_cod = b.marca_cod
          AND    a.fecha_ini = g_reg3.fecha_inicio
          AND    a.hora_ini  = vhora_ini
       END IF
    END IF

    {IF g_reg3.nss[1,1] = "I" THEN
       LET g_reg3.nss = f_reg2.cnss
    END IF}
    
    INPUT BY NAME g_reg3.fecha_corte WITHOUT DEFAULTS
      BEFORE FIELD fecha_corte
        DISPLAY BY NAME g_reg3.*

      AFTER FIELD folio
        DISPLAY BY NAME g_reg3.*
        ERROR "Consulta terminada." SLEEP 2

        ON KEY (CONTROL-P)
           ERROR "PROCESANDO IMPRESION..."
           CALL impresion3(g_reg3.*)
           DISPLAY "Nombre reporte: ", g_impre3 AT 20,1
           SLEEP 5
  
        ON KEY (INTERRUPT)
           PROMPT "Presione <ENTER> para salir." FOR aux_pausa
           INITIALIZE g_reg3.*  TO NULL
           EXIT INPUT
    END INPUT
    
    CLOSE WINDOW ventana_2
END FUNCTION
################################################################################
FUNCTION impresion3(l_reg3)
  DEFINE l_reg3       RECORD
    nss               CHAR(11),
    curp              CHAR(18),
    fecha_corte       DATE,
    nombre            CHAR(120),
    marca_cod         SMALLINT,
    marca_desc        CHAR(25),
    fecha_inicio      DATE,
    tipo_trabajador   CHAR(02),
    desc_tipo_trab    CHAR(20),
    id_registro       CHAR(02),
    desc_id_registro  CHAR(35),
    diagnostico       CHAR(03),
    desc_diagnostico  CHAR(75)
  END RECORD
                            
  LET g_impre3 = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                 ".", l_reg3.nss CLIPPED,
                 "_DET_ARCH_OP99b_",hoy USING "ddmmyyyy" CLIPPED

  DISPLAY "Nombre reporte: ", g_impre3 AT 20,1

  START REPORT rpt_tabrdeta3 TO g_impre3 
    OUTPUT TO REPORT rpt_tabrdeta3(l_reg3.*)                                     
  FINISH REPORT rpt_tabrdeta3

  ERROR "LISTADO GENERADO..."
  SLEEP 2
  ERROR ""

  DISPLAY "Nombre reporte: ", g_impre3 AT 20,1

   --LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   --RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabrdeta3(l_reg3)
   DEFINE l_reg3       RECORD
     nss               CHAR(11),
     curp              CHAR(18),
     fecha_corte       DATE,
     nombre            CHAR(120),
     marca_cod         SMALLINT,
     marca_desc        CHAR(25),
     fecha_inicio      DATE,
     tipo_trabajador   CHAR(02),
     desc_tipo_trab    CHAR(20),
     id_registro       CHAR(02),
     desc_id_registro  CHAR(35),
     diagnostico       CHAR(03),
     desc_diagnostico  CHAR(75)
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
         PRINT COLUMN 001,"PROGRAMA: CTAB130 ",
               COLUMN 035,"LISTADO DE CONSULTA DETALLE RECEPCION ARCHIVO OP 99 - REGISTROS CON SALDO CERO POR REGISTRO",
               COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
         PRINT                  

         SKIP 1 LINE

      ON EVERY ROW
         PRINT 
         PRINT COLUMN 01,"NSS: ", l_reg3.nss CLIPPED,
               COLUMN 18,"CURP: ", l_reg3.curp CLIPPED,
               COLUMN 45,"Fecha Corte: ", l_reg3.fecha_corte USING "DD-MM-YYYY"
         PRINT COLUMN 01,"NOMBRE: ", l_reg3.nombre CLIPPED
         PRINT 
         PRINT COLUMN 01,"Marca Activa   : ", l_reg3.marca_cod USING "###", " ",
                                              l_reg3.marca_desc CLIPPED
         PRINT COLUMN 01,"Fecha Inicio   : ", l_reg3.fecha_inicio USING "DD-MM-YYYY"             
         PRINT
         PRINT COLUMN 01,"Tipo Trabajador: ", l_reg3.tipo_trabajador CLIPPED, " ",
                                              l_reg3.desc_tipo_trab CLIPPED
         PRINT COLUMN 01,"Id Registro    : ", l_reg3.id_registro CLIPPED, " ",
                                              l_reg3.desc_id_registro CLIPPED
         PRINT COLUMN 01,"Diagnostico    : ", l_reg3.diagnostico CLIPPED, " ",
                                              l_reg3.desc_diagnostico CLIPPED
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 01," Total de registros : ",
         COUNT(*) USING "<<<<"

END REPORT

#####################################################################
