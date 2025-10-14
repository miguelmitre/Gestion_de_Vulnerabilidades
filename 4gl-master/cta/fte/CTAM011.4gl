######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Owner             => E.F.P.                                         #
#Programa CTAM011  => CONSULTA FLUJO DE INFORMACION AFILIADO.        #
#Fecha             => 13 de Noviembre de 2009.                       #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                     #
#Sistema           => CTA.                                           #
######################################################################
DATABASE safre_af

GLOBALS
  DEFINE g_reg1       RECORD
    nss               CHAR(011),
    curp              CHAR(018),
    rfc               CHAR(013),
    nombre            CHAR(120),
    fecha_cert        DATE,
    fecha_aper        DATE,
    fecha_naci        DATE,
    edad              SMALLINT,
    reg_edad          SMALLINT,
    reg_actual        SMALLINT
  END RECORD

  DEFINE l_record1    ARRAY[10] OF RECORD
    nss               CHAR(011),
    curp              CHAR(018),
    rfc               CHAR(013),
    nombre            CHAR(120),
    fecha_cert        DATE,
    fecha_aper        DATE,
    fecha_naci        DATE,
    edad              SMALLINT,
    reg_edad          SMALLINT,
    reg_actual        SMALLINT
  END RECORD
   
  DEFINE g_reg2       RECORD
    cve_op            CHAR(002),
    desc_op           CHAR(003),
    folio             DECIMAL(10,0),
    tipo_solicitud    SMALLINT,
    estado            SMALLINT,
    cve_cod_op        CHAR(03),
    desc_cod_op       CHAR(080),
    cve_diag          CHAR(15),
    desc_cve_diag     CHAR(080),
    fecha_proc        DATE
  END RECORD

  DEFINE l_record2    ARRAY[10000] OF RECORD 
    cve_op            CHAR(002),
    desc_op           CHAR(003),
    folio             DECIMAL(10,0),
    tipo_solicitud    SMALLINT,
    estado            SMALLINT,
    cve_cod_op        CHAR(02),
    desc_cod_op       CHAR(080),
    cve_diag          CHAR(15),
    desc_cve_diag     CHAR(080),
    fecha_proc        DATE
  END RECORD

  DEFINE
    fconv_ini         DATE,
    fconv_fin         DATE

  DEFINE g_reg3       RECORD
    siefore           SMALLINT,
    subcuenta         SMALLINT,
    tipo_movimiento   SMALLINT,
    folio             INTEGER,
    id_aportante      CHAR(11),
    monto_en_acciones DECIMAL(22,6),
    monto_en_pesos    DECIMAL(22,6),
    fecha_conversion  DATE
  END RECORD

  DEFINE l_record3    ARRAY[10000] OF RECORD 
    siefore           SMALLINT,
    subcuenta         SMALLINT,
    tipo_movimiento   SMALLINT,
    folio             INTEGER,
    id_aportante      CHAR(11),
    monto_en_acciones DECIMAL(22,6),
    monto_en_pesos    DECIMAL(22,6),
    fecha_conversion  DATE
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
    cla_where2        CHAR(300),
    sel_where2        CHAR(300),
    cla_where3        CHAR(300),
    sel_where3        CHAR(300),
    v_sql_1           CHAR(300),
    g_lista           CHAR(300),
    g_impre           CHAR(300),
    g_lista2          CHAR(300),
    g_impre2          CHAR(300),
    g_impre3          CHAR(300),
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
    total_reg         INTEGER

  DEFINE
    v_edad            SMALLINT,
    v_criterio        SMALLINT,
    v_crea_fecha      DATE

END GLOBALS
##########################################################################
MAIN
  OPTIONS 
    PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY CONTROL-O

  DEFER INTERRUPT

  CALL STARTLOG("CTAM011.log")
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

  LET v_sql_1 = "EXECUTE PROCEDURE fn_edad_multisie(?,?)"
  PREPARE stmt1 FROM v_sql_1

END FUNCTION
##########################################################################
FUNCTION proceso()

  LET HOY = TODAY

  OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CTAM0111" ATTRIBUTE( BORDER)
    DISPLAY " CTAM011           CONSULTA HISTORIAL PROCESOS POR AFILIADO                        " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    MENU "CONSULTA"
      COMMAND "Consulta" "Consulta Historial de Procesos por Afiliado"
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

  DISPLAY "                                                                                      " AT 19,1

  INITIALIZE g_reg1.*  TO NULL
  INITIALIZE l_record1 TO NULL
  INITIALIZE g_reg2.*  TO NULL
  INITIALIZE l_record2 TO NULL

  FOR j = 1 TO 1      
      INITIALIZE l_record1[j].* TO NULL
      DISPLAY l_record1[j].*
  END FOR

  FOR j = 1 TO 1      
      INITIALIZE l_record2[j].* TO NULL
      DISPLAY l_record2[j].*
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

     CONSTRUCT cla_where ON a.n_seguro, a.n_unico
                       FROM nss, curp

       BEFORE CONSTRUCT
         BEFORE FIELD nss
           MESSAGE "Capture el NSS."

         BEFORE FIELD curp
           MESSAGE "Capture la CURP."

       AFTER CONSTRUCT
         AFTER FIELD nss
           MESSAGE ""
           IF FIELD_TOUCHED(nss) THEN
              LET vnss = GET_FLDBUF(nss)

              IF vnss IS NULL THEN
                 NEXT FIELD curp
              ELSE
                 EXIT CONSTRUCT
              END IF
           END IF

         AFTER FIELD curp
           MESSAGE ""
           IF FIELD_TOUCHED(curp) THEN
              LET vcurp = GET_FLDBUF(curp)
          
              IF vnss  IS NULL OR 
                 vcurp IS NULL THEN
                 ERROR "Debe capturar un valor obligatoriamente."
                 NEXT FIELD nss
              ELSE
                 EXIT CONSTRUCT
              END IF 
           END IF
       
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

     LET sel_where = "SELECT a.n_seguro,",
                     " a.n_unico,",
                     " a.n_rfc,",
                     " NVL(TRIM(a.nombres), ' ') || ' ' ||",    
                     " NVL(TRIM(a.paterno), ' ') || ' ' ||",
                     " NVL(TRIM(a.materno), ' '),",
                     " a.fentcons,",
                     " a.finicta,",
                     " a.fena",
                     " FROM   afi_mae_afiliado a",
                     " WHERE ", cla_where CLIPPED

     PREPARE query_p FROM sel_where
     DECLARE cursor_p CURSOR FOR query_p

     LET pos       = 1
     LET total_arh = 0
     FOREACH cursor_p INTO l_record1[pos].*

       CALL calcula_edad_sie(l_record1[pos].fecha_naci)
       LET l_record1[pos].edad     = v_edad
       LET l_record1[pos].reg_edad = v_criterio

       SELECT COUNT(*)
       INTO   vtot_reg
       FROM   cta_nss_regimen
       WHERE  nss           = l_record1[pos].nss
       AND    grupo_regimen = 1
       IF vtot_reg > 1 THEN
          SELECT MAX(factualiza)
          INTO   vfactualiza_reg
          FROM   cta_nss_regimen
          WHERE  nss           = l_record1[pos].nss
          AND    grupo_regimen = 1

          SELECT MAX(rowid)
          INTO   vrowid_reg
          FROM   cta_nss_regimen
          WHERE  nss           = l_record1[pos].nss
          AND    grupo_regimen = 1
          AND    factualiza    = vfactualiza_reg

          SELECT codigo_siefore
          INTO   l_record1[pos].reg_actual
          FROM   cta_nss_regimen
          WHERE  nss           = l_record1[pos].nss
          AND    grupo_regimen = 1
          AND    factualiza    = vfactualiza_reg
          AND    rowid         = vrowid_reg
       ELSE
         SELECT codigo_siefore
         INTO   l_record1[pos].reg_actual
         FROM   cta_nss_regimen
         WHERE  nss           = l_record1[pos].nss
         AND    grupo_regimen = 1
       END IF

       LET total_arh = total_arh + 1
       LET pos       = pos + 1

     END FOREACH

     DISPLAY BY NAME total_arh

     IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        DISPLAY ARRAY l_record1 TO scr_1.*
          ON KEY (CONTROL-M)
             LET pos = ARR_CURR()
             LET g_reg1.nss        = l_record1[pos].nss
             LET g_reg1.curp       = l_record1[pos].curp
             LET g_reg1.rfc        = l_record1[pos].rfc
             LET g_reg1.nombre     = l_record1[pos].nombre
             LET g_reg1.fecha_cert = l_record1[pos].fecha_cert
             LET g_reg1.fecha_aper = l_record1[pos].fecha_aper
             LET g_reg1.fecha_naci = l_record1[pos].fecha_naci
             LET g_reg1.edad       = l_record1[pos].edad
             LET g_reg1.reg_edad   = l_record1[pos].reg_edad
             LET g_reg1.reg_actual = l_record1[pos].reg_actual
             CALL extrae_datos(g_reg1.*)
             INITIALIZE l_record1 TO NULL
             EXIT DISPLAY

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
FUNCTION calcula_edad_sie(vfecha_naci)

  DEFINE
    vfecha_naci    DATE

  LET v_crea_fecha = HOY

  DECLARE curs1 CURSOR FOR stmt1
  OPEN  curs1 USING vfecha_naci, v_crea_fecha
  FETCH curs1 INTO v_edad, v_criterio
  CLOSE curs1

  SELECT ind_edad
  INTO   v_criterio
  FROM tab_rango_edad
  WHERE v_edad BETWEEN edad_min AND edad_max;

  LET v_edad     = v_edad using "##"
  LET v_criterio = v_criterio using "##"

END FUNCTION
################################################################################
FUNCTION impresion(pos)

  DEFINE
    i,
    pos               SMALLINT

  LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                ".CONS_PROC_TRAB.", g_reg1.nss CLIPPED, "_",
                hoy USING "ddmmyyyy" CLIPPED

  DISPLAY "Nombre reporte: ", g_impre AT 19,1

  START REPORT rpt_tabrdeta TO g_impre

    FOR i = 1 TO (pos - 1)
       LET g_reg2.cve_op         = l_record2[i].cve_op
       LET g_reg2.desc_op        = l_record2[i].desc_op
       LET g_reg2.folio          = l_record2[i].folio
       LET g_reg2.tipo_solicitud = l_record2[i].tipo_solicitud
       LET g_reg2.estado         = l_record2[i].estado
       LET g_reg2.cve_cod_op     = l_record2[i].cve_cod_op
       LET g_reg2.desc_cod_op    = l_record2[i].desc_cod_op
       LET g_reg2.cve_diag       = l_record2[i].cve_diag
       LET g_reg2.desc_cve_diag  = l_record2[i].desc_cve_diag
       LET g_reg2.fecha_proc     = l_record2[i].fecha_proc

       IF g_reg2.cve_op IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tabrdeta(i, g_reg2.*)
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
     cve_op            CHAR(002),
     desc_op           CHAR(003),
     folio             DECIMAL(10,0),
     tipo_solicitud    SMALLINT,
     estado            SMALLINT,
     cve_cod_op        CHAR(03),
     desc_cod_op       CHAR(080),
     cve_diag          CHAR(15),
     desc_cve_diag     CHAR(080),
     fecha_proc        DATE
   END RECORD

   DEFINE
     vdiag_proceso     CHAR(02),
     vdiagn_desc       CHAR(80)

   DEFINE lreg_proc    RECORD
     cve_modulo        CHAR(003),
     desc_modulo       CHAR(020),
     cve_proceso       CHAR(003),
     desc_proceso      CHAR(045),
     cve_operacion     CHAR(002),
     desc_operacion    CHAR(080)
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
         PRINT COLUMN 001,"PROGRAMA: CTAM011 ",
               COLUMN 065,"LISTADO DE CONSULTA HISTORIAL PROCESOS POR AFILIADO",
               COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
         PRINT

         PRINT COLUMN 001,"NSS: ",  g_reg1.nss,
               COLUMN 020,"CURP: ", g_reg1.curp,
               COLUMN 050,"RFC: ",  g_reg1.rfc
         PRINT COLUMN 001,"Nombre: ", g_reg1.nombre CLIPPED
         PRINT COLUMN 001,"Fecha Certificacion: ",   g_reg1.fecha_cert
                                                     USING "DD-MM-YYYY",
               COLUMN 035,"Fecha Apertura Cuenta: ", g_reg1.fecha_aper
                                                     USING "DD-MM-YYYY",
               COLUMN 072,"Fecha Nacimiento: ",      g_reg1.fecha_naci
                                                     USING "DD-MM-YYYY"
         PRINT COLUMN 001,"Edad: ", g_reg1.edad USING "<<<",
               COLUMN 012,"Regimen por Edad: ", g_reg1.reg_edad USING "<",
               COLUMN 035,"Regimen Actual: ", g_reg1.reg_actual USING "<"  
         SKIP 2 LINE

         PRINT COLUMN 001,"ID",
               COLUMN 007,"OPERACION",
               COLUMN 017,"FOLIO REF",
               COLUMN 027,"TIPO SOLICITUD",
               COLUMN 043,"ESTADO",
               COLUMN 051,"CODIGO OPERACION",
               COLUMN 076,"DIAGNOSTICO",
               COLUMN 132,"FECHA PROCESO"

      ON EVERY ROW
         PRINT COLUMN 001, li USING "<<<<<",
               COLUMN 007, lg_reg2.cve_op CLIPPED,
               COLUMN 010, lg_reg2.desc_op CLIPPED,
               COLUMN 017, lg_reg2.folio USING "<<<<<<<<<<",
               COLUMN 027, lg_reg2.tipo_solicitud USING "<<",
               COLUMN 043, lg_reg2.estado CLIPPED,
               COLUMN 051, lg_reg2.cve_cod_op CLIPPED,
               COLUMN 054, lg_reg2.desc_cod_op[1,20] CLIPPED,
               COLUMN 076, lg_reg2.cve_diag[1,3] CLIPPED,
               COLUMN 080, lg_reg2.desc_cve_diag[1,50] CLIPPED,
               COLUMN 132, lg_reg2.fecha_proc USING "DD-MM-YYYY" 

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 01," Total de registros : ",
         COUNT(*) USING "<<<<"

         SKIP 2 LINE

         PRINT COLUMN 01,"Nomenclatura: "

         SKIP 1 LINE

         PRINT COLUMN 01,"Operacion / Proceso / Modulo"
         DECLARE l_1 CURSOR FOR  
         SELECT unique c.cve_modulo, c.desc_modulo,
                a.cve_proceso, a.desc_proceso,
                d.cve_operacion, d.desc_operacion, d.secuencia 
         FROM   tab_cta_proceso a, tmp_his_afi b, tab_cta_modulo c,
                tab_cta_operacion d
         WHERE  a.cve_proceso   = desc_op
         AND    d.cve_operacion = cve_op
         AND    a.cve_modulo    = c.cve_modulo
         AND    a.cve_proceso   = d.cve_proceso
         AND    b.secuencia     = d.secuencia
         ORDER BY d.cve_operacion, d.secuencia
         FOREACH l_1 INTO lreg_proc.*
           PRINT COLUMN 01,lreg_proc.cve_operacion  CLIPPED, " ",
                           lreg_proc.desc_operacion CLIPPED, " / ",
                           lreg_proc.cve_proceso    CLIPPED, " ",
                           lreg_proc.desc_proceso   CLIPPED, " / ",
                           lreg_proc.cve_modulo     CLIPPED, " ",
                           lreg_proc.desc_modulo    CLIPPED 
         END FOREACH

END REPORT
#####################################################################

#####################################################################
FUNCTION extrae_datos(f_reg2)

  DEFINE f_reg2       RECORD
    nss               CHAR(011),
    curp              CHAR(018),
    rfc               CHAR(013),
    nombre            CHAR(120),
    fecha_cert        DATE,
    fecha_aper        DATE,
    fecha_naci        DATE,
    edad              SMALLINT,
    reg_edad          SMALLINT,
    reg_actual        SMALLINT
  END RECORD

  ERROR "Desplegando detalle." SLEEP 2

  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_his_afi  
    CREATE TEMP TABLE tmp_his_afi(cve_op            CHAR(002),
                                  desc_op           CHAR(003),
                                  folio             DECIMAL(10,0),  
                                  tipo_solicitud    SMALLINT,
                                  estado            SMALLINT, 
                                  cve_cod_op        CHAR(03), 
                                  desc_cod_op       CHAR(080),
                                  cve_diag          CHAR(15),
                                  desc_cve_diag     CHAR(080),
                                  fecha_proc        DATE,
                                  secuencia         SMALLINT);
  WHENEVER ERROR STOP

  CALL afiliacion(f_reg2.nss)
  CALL traspasos(f_reg2.nss)
  CALL marca_cuenta(f_reg2.nss)
  CALL unificacion(f_reg2.nss)

  LET sel_where2 = "SELECT * ",
                   " FROM tmp_his_afi",
                   " ORDER BY 10 desc,5 desc,6, 1"

  DISPLAY "" AT 1,1
  DISPLAY "" AT 2,1
  DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
  DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

  LET pos2 = 2

  IF (pos2-1) >= 1 THEN
     CALL  SET_COUNT(pos2-1)

     PREPARE query2 FROM sel_where2
     DECLARE cursor_2 CURSOR FOR query2

     LET pos2      = 1
     LET total_arh = 0

     FOREACH cursor_2 INTO l_record2[pos2].*

       LET pos2      = pos2      + 1
       LET total_arh = total_arh + 1

     END FOREACH

     DISPLAY BY NAME total_arh

     ERROR "Consulta finalizada. . ."

     IF (pos2-1) >= 1 THEN
        CALL SET_COUNT(pos2-1)

        DISPLAY ARRAY l_record2 TO scr_2.*
        ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
        ON KEY (CONTROL-B)
           LET pos = ARR_CURR()
           CALL extrae_datos2(f_reg2.*)

        ON KEY (CONTROL-M)
           LET pos2 = ARR_CURR()

        ON KEY (CONTROL-P)
           ERROR "PROCESANDO IMPRESION..."
           CALL impresion(pos2)

        ON KEY (INTERRUPT)
           INITIALIZE l_record2 TO NULL
           CLEAR FORM
           DISPLAY "                                                                               " AT 19,1
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

  CLEAR SCREEN

END FUNCTION
################################################################################

################################################################################
FUNCTION afiliacion(qnss)

   DEFINE qnss          CHAR(011)
   DEFINE sel_ins       CHAR(900)

   ##### ASIGNADO #####
   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT clave_operacion, ",
                 "'ASI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "estado_asignado, ",
                 "'0', ",
                 "'ASIGNADO',",
                 "status_cuenta, ",
                 "CASE WHEN status_cuenta = 1 THEN 'CUENTA ACTIVA' ",
                      "WHEN status_cuenta = 2 THEN 'CUENTA INACTIVA' ",
                 "END CASE, ",
                 "fecha_asignacion, 1",
                 "FROM afi_det_asignado ",
                 "WHERE n_seguro = '", qnss, "'"
   PREPARE exe_ins1 FROM sel_ins
   EXECUTE exe_ins1

   ##### RECEPCION SOLICITUD #####
   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT ", 
                 "CASE WHEN tipo_solicitud =  1 THEN '10' ",
                      "WHEN tipo_solicitud =  2 THEN '04' ",
                      "WHEN tipo_solicitud =  8 THEN '50' ",
                 "END CASE, ",
                 "'AFI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "'0', ",
                 "estado_sol, ",
                 "'CAPTURADO', ",
                 "estado_exp, ",
                 "'SOL AFI RECIBIDA', ",
                 "fecha_actualiza, 2 ",
                 "FROM   afi_recepcion ",
                 "WHERE  n_seguro = '", qnss, "'"
   PREPARE exe_ins2 FROM sel_ins
   EXECUTE exe_ins2

   ##### CONTROL DE SOLICITUD #####
   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT ", 
                 "CASE WHEN tipo_solicitud =  1 THEN '10' ",
                      "WHEN tipo_solicitud =  2 THEN '04' ",
                      "WHEN tipo_solicitud =  8 THEN '50' ",
                      "WHEN tipo_solicitud =  9 THEN '54' ",
                      "WHEN tipo_solicitud = 11 THEN '52' ",
                      "WHEN tipo_solicitud = 12 THEN '52' ",                                            
                 "END CASE, ",
                 "'AFI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "status_interno, ",
                 "cod_operacion, ",
                 "'EN PROCESO', ",
                 "diag_proceso, ",
                 "'ENVIADO A CERTIFICAR', ",
                 "factualiza, 3 ",
                 "FROM   afi_ctr_solicitud ",
                 "WHERE  n_seguro          = '", qnss, "'",
                 "AND    cod_operacion     = '0'"
   PREPARE exe_ins3 FROM sel_ins
   EXECUTE exe_ins3

   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT ", 
                 "CASE WHEN tipo_solicitud =  1 THEN '10' ",
                      "WHEN tipo_solicitud =  8 THEN '50' ",
                      "WHEN tipo_solicitud = 11 THEN '52' ",
                      "WHEN tipo_solicitud = 12 THEN '52' ",                                                                  
                 "END CASE, ",
                 "'AFI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "status_interno, ",
                 "cod_operacion, ",
                 "'RECHAZADO', ",
                 "diag_proceso, ",
                 "rdeta_desc_c, ",
                 "factualiza, 3 ",
                 "FROM   afi_ctr_solicitud, ",
                 "OUTER  tab_rdeta ",
                 "WHERE  n_seguro          = '", qnss, "'",
                 "AND    tipo_solicitud   IN (1,8,11,12) ",
                 "AND    cod_operacion    IN ('02','2','11','12') ",
                 "AND    diag_proceso[1,3] = rdeta_cod ",
                 "AND    modulo_cod        = 'afi'"
   PREPARE exe_ins4 FROM sel_ins
   EXECUTE exe_ins4

   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT ", 
                 "CASE WHEN tipo_solicitud =  2 THEN '04' ",
                      "WHEN tipo_solicitud =  9 THEN '54' ",
                 "END CASE, ",
                 "'AFI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "status_interno, ",
                 "cod_operacion, ",
                 "'RECHAZADO', ",
                 "diag_proceso, ",
                 "rdeta_desc_c, ",
                 "factualiza, 3 ",
                 "FROM   afi_ctr_solicitud, ",
                 "OUTER  tab_rdeta ",
                 "WHERE  n_seguro          = '", qnss, "'",
                 "AND    tipo_solicitud   IN (2,9) ",
                 "AND    cod_operacion    IN ('02','2','11','12') ",
                 "AND    diag_proceso[1,3] = rdeta_cod ",
                 "AND    modulo_cod        = 'taa' "
   PREPARE exe_ins5 FROM sel_ins
   EXECUTE exe_ins5               

   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT ", 
                 "CASE WHEN tipo_solicitud =  1 THEN '10' ",
                      "WHEN tipo_solicitud =  2 THEN '04' ",
                      "WHEN tipo_solicitud =  8 THEN '50' ",
                      "WHEN tipo_solicitud =  9 THEN '54' ",
                      "WHEN tipo_solicitud = 11 THEN '52' ",
                      "WHEN tipo_solicitud = 12 THEN '52' ",                    
                 "END CASE, ",
                 "'AFI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "status_interno, ",
                 "cod_operacion, ",
                 "'PENDIENTE', ",
                 "diag_proceso, ",
                 "'', ",
                 "factualiza, 3 ",
                 "FROM   afi_ctr_solicitud ",
                 "WHERE  n_seguro          = '", qnss, "'",
                 "AND    cod_operacion     = '03'"
   PREPARE exe_ins6 FROM sel_ins
   EXECUTE exe_ins6               

   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT ", 
                 "CASE WHEN tipo_solicitud =  1 THEN '10' ",
                      "WHEN tipo_solicitud =  2 THEN '04' ",
                      "WHEN tipo_solicitud =  8 THEN '50' ",
                      "WHEN tipo_solicitud =  9 THEN '54' ",
                      "WHEN tipo_solicitud = 11 THEN '52' ",
                      "WHEN tipo_solicitud = 12 THEN '52' ",                    
                 "END CASE, ",  
                 "'AFI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "status_interno, ",
                 "cod_operacion, ",
                 "'PENDIENTE PROC EXTERNO', ",
                 "diag_proceso, ",
                 "'', ",
                 "factualiza, 3 ",
                 "FROM   afi_ctr_solicitud ",
                 "WHERE  n_seguro          = '", qnss, "'",
                 "AND    cod_operacion     = '90'"
   PREPARE exe_ins7 FROM sel_ins
   EXECUTE exe_ins7               

   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT ", 
                 "CASE WHEN tipo_solicitud =  1 THEN '10' ",
                      "WHEN tipo_solicitud =  2 THEN '04' ",
                      "WHEN tipo_solicitud =  8 THEN '50' ",
                      "WHEN tipo_solicitud =  9 THEN '54' ",
                      "WHEN tipo_solicitud = 11 THEN '52' ",
                      "WHEN tipo_solicitud = 12 THEN '52' ",                      
                 "END CASE, ",  
                 "'AFI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "status_interno, ",
                 "cod_operacion, ",
                 "'EN ACLARACION', ",
                 "diag_proceso, ",
                 "'', ",
                 "factualiza, 3 ",
                 "FROM   afi_ctr_solicitud ",
                 "WHERE  n_seguro          = '", qnss, "'",
                 "AND    cod_operacion    IN('08','18')"
   PREPARE exe_ins8 FROM sel_ins
   EXECUTE exe_ins8               

   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT ", 
                 "CASE WHEN tipo_solicitud =  1 THEN '10' ",
                      "WHEN tipo_solicitud =  2 THEN '04' ",
                      "WHEN tipo_solicitud =  8 THEN '50' ",
                      "WHEN tipo_solicitud =  9 THEN '54' ",
                      "WHEN tipo_solicitud = 11 THEN '52' ",
                      "WHEN tipo_solicitud = 12 THEN '52' ",                    
                 "END CASE, ",  
                 "'AFI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "status_interno, ",
                 "cod_operacion, ",
                 "'ACEPTADO', ",
                 "diag_proceso, ",
                 "'ACEPTADO CERTIFICACION', ",
                 "factualiza, 3 ",
                 "FROM   afi_ctr_solicitud ",
                 "WHERE  n_seguro           = '", qnss, "'",
                 "AND    cod_operacion NOT IN ('0','2','02','11','12','03','08','18','90')"
   PREPARE exe_ins9 FROM sel_ins
   EXECUTE exe_ins9             

   ##### POSIBLES DUPLICADOS #####
   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT '36', ",
                 "'PDS', ",
                 "folio, ",
                 "id_origen_reg, ",
                 "diag_aclara, ",
                 "cod_result, ",
                 "CASE WHEN cod_result = '01' THEN 'ACEPTADO SOL' ",
                      "WHEN cod_result = '02' THEN 'RECHAZADO SOL' ",
                      "WHEN cod_result = ' '  THEN 'SOLICITUD' ",
                      "WHEN cod_result = '0'  THEN 'SOLICITUD' ",
                 "END CASE, ",
                 "diag_proceso1, ",
                 "'POSIBLES DUPLICADOS SOL', ",
                 "factualiza, 4 ",
                 "FROM   afi_det_dup_sol ",
                 "WHERE  nss_sol = '", qnss, "'" 
   PREPARE exe_ins10 FROM sel_ins
   EXECUTE exe_ins10

   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT '36', ",
                 "'PDS', ",
                 "folio, ",
                 "id_origen_reg, ",
                 "diag_aclara, ",
                 "cod_result, ",
                 "CASE WHEN cod_result = '01' THEN 'ACEPTADO ASO' ",
                      "WHEN cod_result = '02' THEN 'RECHAZADO ASO' ",
                      "WHEN cod_result = ' '  THEN 'ASOCIADO' ",
                      "WHEN cod_result = '0'  THEN 'ASOCIADO' ",
                 "END CASE, ",
                 "diag_proceso1, ",
                 "'POSIBLES DUPLICADOS ASO', ",
                 "factualiza, 5 ",
                 "FROM   afi_det_dup_aso ",
                 "WHERE  nss_sol = '", qnss, "'" 
   PREPARE exe_ins11 FROM sel_ins
   EXECUTE exe_ins11

   ##### VALIDACION DE IMAGENES #####
   INSERT INTO tmp_his_afi
   SELECT '06',
          'AFI',
          n_folio,
          tipo_traspaso,
          estado,
          cod_result_operac,
          'ACEPTADO',
          diag_proceso,
          'ACEPTADO VERIFICACION IMAGENES',
          f_actualiza, 6
   FROM   taa_det_devol
   WHERE  n_seguro          = qnss          
   AND    cod_result_operac = '01'

   INSERT INTO tmp_his_afi
   SELECT '06',
          'AFI',
          n_folio,
          tipo_traspaso,
          estado,
          cod_result_operac,
          'RECHAZADO',
          diag_proceso,
          desc_rech,
          f_actualiza, 6
   FROM   taa_det_devol, 
          tab_dev_taa
   WHERE  n_seguro          = qnss          
   AND    cod_result_operac = '02'
   AND    diag_proceso[1,3] = cod_rech
   AND    tipo_diag         = 1
   AND    diag_proceso IS NOT NULL

   INSERT INTO tmp_his_afi
   SELECT '06',
          'AFI',
          n_folio,
          tipo_traspaso,
          estado,
          cod_result_operac,
          'RECHAZADO',
          motivo_rechazo,
          desc_rech,
          f_actualiza, 6
   FROM   taa_det_devol, 
          tab_dev_taa
   WHERE  n_seguro          = qnss          
   AND    cod_result_operac = '02'
   AND    motivo_rechazo    = cod_rech
   AND    tipo_diag         = 2
   AND    motivo_rechazo   IS NOT NULL

   INSERT INTO tmp_his_afi
   SELECT '06',
          'AFI',
          n_folio,
          tipo_traspaso,
          estado,
          cod_result_operac,
          'RECHAZADO',
          diag_imagen[1,2],
          desc_rech,
          f_actualiza, 6
   FROM   taa_det_devol, 
          tab_dev_taa
   WHERE  n_seguro          = qnss          
   AND    cod_result_operac = '02'
   AND    diag_imagen[1,2]  = cod_rech
   AND    tipo_diag         = 2
   AND    diag_imagen      IS NOT NULL

   INSERT INTO tmp_his_afi
   SELECT '06',
          'AFI',
          n_folio,
          tipo_traspaso,
          estado,
          cod_result_operac,
          'RECHAZADO',
          diag_imagen[3,4],
          desc_rech,
          f_actualiza, 6
   FROM   taa_det_devol, 
          tab_dev_taa
   WHERE  n_seguro          = qnss          
   AND    cod_result_operac = '02'
   AND    diag_imagen[3,4]  = cod_rech
   AND    tipo_diag         = 2
   AND    diag_imagen      IS NOT NULL

   INSERT INTO tmp_his_afi
   SELECT '06',
          'AFI',
          n_folio,
          tipo_traspaso,
          estado,
          cod_result_operac,
          'RECHAZADO',
          diag_imagen[5,6],
          desc_rech,
          f_actualiza, 6
   FROM   taa_det_devol, 
          tab_dev_taa
   WHERE  n_seguro          = qnss          
   AND    cod_result_operac = '02'
   AND    diag_imagen[5,6]  = cod_rech
   AND    tipo_diag         = 2
   AND    diag_imagen      IS NOT NULL

   INSERT INTO tmp_his_afi
   SELECT '06',
          'AFI',
          n_folio,
          tipo_traspaso,
          estado,
          cod_result_operac,
          'RECHAZADO',
          diag_imagen[7,8],
          desc_rech,
          f_actualiza, 6
   FROM   taa_det_devol, 
          tab_dev_taa
   WHERE  n_seguro          = qnss          
   AND    cod_result_operac = '02'
   AND    diag_imagen[7,8]  = cod_rech
   AND    tipo_diag         = 2
   AND    diag_imagen      IS NOT NULL

   INSERT INTO tmp_his_afi
   SELECT '06',
          'AFI',
          n_folio,
          tipo_traspaso,
          estado,
          cod_result_operac,
          'RECHAZADO',
          diag_imagen[9,10],
          desc_rech,
          f_actualiza, 6
   FROM   taa_det_devol, 
          tab_dev_taa
   WHERE  n_seguro          = qnss          
   AND    cod_result_operac = '02'
   AND    diag_imagen[9,10] = cod_rech
   AND    tipo_diag         = 2
   AND    diag_imagen      IS NOT NULL

   ##### REGISTRO Y NO AFILIADOS POR INTERNET #####
   INSERT INTO tmp_his_afi
   SELECT cve_operacion,
          'AFI',
          n_folio,
          tipo_trab,    ### 1 - Trab Afil, 2 - Trab No Afil
          tipo_reg_noa, ### 1 - Independiente, 2 - ISSSTE
          cod_operacion,
          'ACEPTADO',
          '0',
          'REG POR INTERNET',
          fecha_alta, 7
   FROM   afi_det_reg_internet
   WHERE  n_seguro = qnss          

   ##### TRASPASO POR INTERNET #####
   INSERT INTO tmp_his_afi
   SELECT '54',
          'AFI',
          n_folio,
          tipo_traspaso,
          '0', 
          '0',
          'ACEPTADO',
          '0',
          'TAA POR INTERNET',
          fecha_cert, 8
   FROM   afi_det_internet
   WHERE  nss = qnss          

   ##### CARGA MASIVA Y APERTURA DE CUENTA #####
   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT '0', ", 
                 "'AFI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "status_interno, ",
                 "'0', ",
                 "'PROCESADO', ",
                 "'0', ",
                 "operacion, ",
                 "factualiza, 9 ",
                 "FROM   afi_ctr_logico ",
                 "WHERE  n_seguro = '", qnss, "'"
   PREPARE exe_ins12 FROM sel_ins
   EXECUTE exe_ins12
                 
   ##### DISTRIBUCION CURP #####
   INSERT INTO tmp_his_afi
   SELECT '71',
          'CRP',
          '0',
          '0',
          status_renapo,
          cod_operacion,
          'ACEPTADO',
          diag_proceso,
          'DISTRIBUCION CURP',
          fecha_actualiza, 10
   FROM   afi_dispersa_curp
   WHERE  n_seguro      = qnss          
   AND    cod_operacion = '01'

   INSERT INTO tmp_his_afi
   SELECT '71',
          'CRP',
          '0',
          '0',
          status_renapo,
          cod_operacion,
          'RECHAZADO',
          diag_proceso[1,2],
          des_rechazo,
          fecha_actualiza, 10
   FROM   afi_dispersa_curp,
   OUTER  tab_rch_curp
   WHERE  n_seguro          = qnss          
   AND    cod_operacion     = '02'
   AND    diag_proceso[1,2] = curp_cod_rech

   ##### LIBERACION DE TRAMITE DE CURP #####
   INSERT INTO tmp_his_afi
   SELECT '71',
          'CRP',
          '0',
          ident_tip_lib,
          status_renapo,
          cod_operacion,
          'RECHAZADO LIB TRAM CURP',
          diag_proceso[1,3],
          rdeta_desc_c,
          fecha_proceso, 11
   FROM   afi_lib_tram_curp,
   OUTER  tab_rdeta
   WHERE  nss_solicitud     = qnss          
   AND    cod_operacion     = '02'
   AND    modulo_cod        = 'afi'
   AND    diag_proceso[1,3] = rdeta_cod

   ##### NOTIFICACION DE DOMICILIOS #####
   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT '17', ",
                 "'AFI', ",
                 "n_folio, ",
                 "tipo_solicitud, ",
                 "status_interno, ",
                 "cod_resultado, ",
                 "CASE WHEN cod_resultado = ' '  THEN 'EN PROCESO' ",
                      "WHEN cod_resultado = '01' THEN 'ACEPTADO' ",
                      "WHEN cod_resultado = '02' THEN 'RECHAZADO' ",
                 "END CASE, ",
                 "diag_proceso, ",
                 "rdeta_desc_c, ",
                 "factualiza, 12 ",
                 "FROM afi_ctr_domicilio, ",
                 "OUTER tab_rdeta ",
                 "WHERE nss = '", qnss, "'",
                 " AND diag_proceso = rdeta_cod "
   PREPARE exe_ins13 FROM sel_ins
   EXECUTE exe_ins13

   ##### ACTUALIZACION DE DOMICILIOS #####
   INSERT INTO tmp_his_afi
   SELECT '15',
          'MDO',
          n_folio,
          tipo_solicitud,
          '0',
          cod_operacion,
          'ENVIADO',
          diag_proceso,
          'ACT DOMICILIO',
          fecha_actualiza, 13
   FROM   afi_mae_modifica
   WHERE  n_seguro          = qnss 
   AND    cod_operacion     = '12'

   ##### MODIFICACION DE DATOS #####
   INSERT INTO tmp_his_afi
   SELECT '13',
          'MOD',
          n_folio,
          tipo_solicitud,
          '0',
          cod_operacion,
          'CAPTURADA',
          diag_proceso,
          'MOD CAPTURADA',
          fecha_registro, 14
   FROM   afi_ctr_curp
   WHERE  nss            = qnss 
   AND    cod_operacion IN (' ','0')
   AND    cve_operacion  = '13'

   INSERT INTO tmp_his_afi
   SELECT '13',
          'MOD',
          n_folio,
          tipo_solicitud,
          '0',
          cod_operacion,
          'ACEPTADO',
          diag_proceso,
          'MOD ACEPTADO',
          fecha_respuesta, 14
   FROM   afi_ctr_curp
   WHERE  nss                = qnss 
   AND    cod_operacion NOT IN (' ','0','02','03')
   AND    cve_operacion     = '13'

   INSERT INTO tmp_his_afi
   SELECT '13',
          'MOD',
          n_folio,
          tipo_solicitud,
          '0',
          cod_operacion,
          'RECHAZADO',
          diag_proceso,
          rdeta_desc_c,
          fecha_respuesta, 14
   FROM   afi_ctr_curp,
   OUTER  tab_rdeta
   WHERE  nss               = qnss 
   AND    cod_operacion     = '02'
   AND    diag_proceso      = rdeta_cod
   AND    cve_operacion     = '13'

   INSERT INTO tmp_his_afi
   SELECT '13',
          'MOD',
          n_folio,
          tipo_solicitud,
          '0',
          cod_operacion,
          'PENDIENTE RENAPO',
          diag_proceso,
          rdeta_desc_c,
          fecha_respuesta, 14
   FROM   afi_ctr_curp,
   OUTER  tab_rdeta
   WHERE  nss               = qnss 
   AND    cod_operacion     = '03'
   AND    diag_proceso      = rdeta_cod
   AND    cve_operacion     = '13'

   ##### NOTIFICACION DE MODIFICACION DE DATOS DEL TRABAJADOR (PROCANASE) #####
   LET sel_ins = 
   "INSERT INTO tmp_his_afi ",
   "SELECT '72', ",
   "'MOD', ",
   "'0', ",
   "'0', ",
   "'0', ",
   "'0', ",
   "'NOTIFICACION', ",
   "status_cta, ",
   "CASE WHEN status_cta = '0' THEN 'REG CON DIF BDNSAR VS PROCANASE' ",
   "WHEN status_cta = '1' THEN 'REG CON DIF BDNSAR VS PROCANASE ENV PREV' ",
   "WHEN status_cta = '2' THEN 'REG SIN DIF BDNSAR VS PROCANASE POR ACT IMSS' ",
   "END CASE, ",
   "factualiza, 15 ",
   "FROM afi_det_notifica ",
   "WHERE n_seguro = '", qnss, "'"
   PREPARE exe_ins14 FROM sel_ins
   EXECUTE exe_ins14

   ##### NOTIFICACION DE MODIFICACION DE DATOS DEL TRABAJADOR #####
   INSERT INTO tmp_his_afi
   SELECT '54',
          'MOD',
          n_folio,
          tipo_solicitud,
          status_interno,
          cod_result_op,
          'ACEPTADO',
          motivo_rechazo1,
          'MOD ACEPTADO',
          factualiza, 16
   FROM   afi_ctr_det_op54
   WHERE  n_seguro          = qnss 
   AND    cod_result_op     = '01'

   INSERT INTO tmp_his_afi
   SELECT '54',
          'MOD',
          n_folio,
          tipo_solicitud,
          status_interno,
          cod_result_op,
          'RECHAZADO',
          motivo_rechazo1,
          rdeta_desc_c, 
          factualiza, 16
   FROM   afi_ctr_det_op54,
   OUTER  tab_rdeta
   WHERE  n_seguro        = qnss 
   AND    cod_result_op   = '02'
   AND    motivo_rechazo1 = rdeta_cod

   INSERT INTO tmp_his_afi
   SELECT '54',
          'MOD',
          n_folio,
          tipo_solicitud,
          status_interno,
          cod_result_op,
          'RECHAZADO',
          motivo_rechazo2,
          rdeta_desc_c, 
          factualiza, 16
   FROM   afi_ctr_det_op54,
   OUTER  tab_rdeta
   WHERE  n_seguro        = qnss 
   AND    cod_result_op   = '02'
   AND    motivo_rechazo2 = rdeta_cod

   INSERT INTO tmp_his_afi
   SELECT '54',
          'MOD',
          n_folio,
          tipo_solicitud,
          status_interno,
          cod_result_op,
          'RECHAZADO',
          motivo_rechazo3,
          rdeta_desc_c, 
          factualiza, 16
   FROM   afi_ctr_det_op54,
   OUTER  tab_rdeta
   WHERE  n_seguro        = qnss 
   AND    cod_result_op   = '02'
   AND    motivo_rechazo3 = rdeta_cod
   
   ##### SOLICITUDES DE ACTIVACION DEL TRABAJADOR #####
   LET sel_ins = "INSERT INTO tmp_his_afi ",
                 "SELECT '63', ",
                 "'ACT', ",
                 "'0', ",
                 "'0', ",
                 "status_interno, ",
                 "cod_operacion, ",
                 "CASE WHEN cod_operacion = ' '  THEN 'EN PROCESO' ",
                      "WHEN cod_operacion = '01' THEN 'ACEPTADO' ",
                      "WHEN cod_operacion = '02' THEN 'RECHAZADO' ",
                 "END CASE, ",
                 "status_interno, ",
                 "CASE WHEN status_interno = '10'  THEN 'CAPTURADA' ",
                      "WHEN status_interno = '30'  THEN 'ENVIADA' ",
                      "WHEN status_interno = '40'  THEN 'RECHAZADO' ",
                      "WHEN status_interno = '100' THEN 'ACEPTADO' ",
                 "END CASE, ",
                 "fecha_actualiza, 17 ",
                 "FROM afi_sol_activacion ",
                 "WHERE nss = '", qnss, "'"
   PREPARE exe_ins15 FROM sel_ins
   EXECUTE exe_ins15

   INSERT INTO tmp_his_afi
   SELECT '63',
          'ACT',
          '0',
          '0',
          '0',
          cod_operacion,
          'RECHAZADO',
          mot_rech1,
          rdeta_desc_c, 
          fecha_actualiza, 17
   FROM   afi_rch_activacion,
   OUTER  tab_rdeta
   WHERE  nss             = qnss 
   AND    cod_operacion   = '02'
   AND    mot_rech1       = rdeta_cod

   INSERT INTO tmp_his_afi
   SELECT '63',
          'ACT',
          '0',
          '0',
          '0',
          cod_operacion,
          'RECHAZADO',
          mot_rech2,
          rdeta_desc_c, 
          fecha_actualiza, 17
   FROM   afi_rch_activacion,
   OUTER  tab_rdeta
   WHERE  nss             = qnss 
   AND    cod_operacion   = '02'
   AND    mot_rech2       = rdeta_cod

   INSERT INTO tmp_his_afi
   SELECT '63',
          'ACT',
          '0',
          '0',
          '0',
          cod_operacion,
          'RECHAZADO',
          mot_rech3,
          rdeta_desc_c, 
          fecha_actualiza, 17
   FROM   afi_rch_activacion,
   OUTER  tab_rdeta
   WHERE  nss             = qnss 
   AND    cod_operacion   = '02'
   AND    mot_rech3       = rdeta_cod

END FUNCTION
################################################################################
FUNCTION traspasos(qnss)

   DEFINE qnss          CHAR(011)
   DEFINE sel_ins       CHAR(900)

   ##### NO ATENDIDAS #####
   INSERT INTO tmp_his_afi
   SELECT '14',
          'TAR',
          '0',
          tipo_traspaso,
          estado,
          '14',
          'NO ATENDIDA',
          cve_ced_cuenta,
          afore_desc, 
          f_actualiza, 18
   FROM   taa_det_no_aten,
   OUTER  tab_afore
   WHERE  n_seguro            = qnss
   AND    rech_proc_operat   IS NULL
   AND    cve_ced_cuenta      = afore_cod

   INSERT INTO tmp_his_afi
   SELECT '14',
          'TAR',
          '0',
          tipo_traspaso,
          rech_proc_operat,
          '14',
          'NO ATENDIDA',
          cve_ced_cuenta,
          afore_desc,
          f_actualiza, 18
   FROM   taa_det_no_aten,
   OUTER  tab_afore
   WHERE  n_seguro             = qnss          
   AND    rech_proc_operat    IS NOT NULL
   AND    cve_ced_cuenta       = afore_cod

   ##### NOTIFICACION DE CUENTAS OP 53 #####
   INSERT INTO tmp_his_afi
   SELECT '53',
          'TAR', 
          repetido, 
          tipo_traspaso,
          '0',
          '53',
          'NOT CUENTAS',
          cve_ent_ced,
          afore_desc,
          fecha_actualiza, 19
   FROM   taa_det_notifica, 
   OUTER  tab_afore
   WHERE  nss_afore   = qnss
   AND    cve_ent_ced = afore_cod

   ##### SALDOS PREVIOS #####
   INSERT INTO tmp_his_afi
   SELECT '29',
          'TAR', 
          folio, 
          tipo_traspaso,
          estado_reg,
          cve_subcta,
          'SUB SDOS',
          siefore,
          'SIEFORE',
          fecha_proceso, 20
   FROM   det_tra_sdo_previo
   WHERE  nss = qnss

   ##### TAA RECEPTORA #####
   LET sel_ins = "INSERT INTO tmp_his_afi ",
   "SELECT ident_operacion, ",
          "'TAR', ",
          "folio, ",
          "tipo_traspaso, ",
          "estado, ",
          "'0', ",
          "'LIQUIDADA', ",
          "cve_ced_cuenta, ",
          "afore_desc, ",
          "fecha_mov_banxico, ",
          "CASE WHEN ident_operacion = '09' THEN 21 ",
               "WHEN ident_operacion = '12' THEN 22 ",
          "END CASE ",
   "FROM   taa_viv_recepcion,  ",
   "OUTER  tab_afore ",
   "WHERE  nss              = '", qnss, "'",
   "AND    cve_ced_cuenta   = afore_cod "
   PREPARE exe_ins16 FROM sel_ins
   EXECUTE exe_ins16

   ##### TAA CEDENTE #####
   INSERT INTO tmp_his_afi
   SELECT '09',
          'TAC', 
          folio, 
          tipo_traspaso,
          estado,
          '0',
          '0',
          cve_recep_cuenta,
          afore_desc,
          fecha_trasp, 23
   FROM   taa_cd_det_cedido, 
   OUTER  tab_afore
   WHERE  n_seguro         = qnss
   AND    cve_recep_cuenta = afore_cod

END FUNCTION
################################################################################
FUNCTION marca_cuenta(qnss)

   DEFINE qnss          CHAR(011)
   DEFINE sel_ins       CHAR(900)

   ##### MARCA ACTIVA #####
   INSERT INTO tmp_his_afi 
   SELECT '08', 
          'MCA',
          a.correlativo,
          '0',
          a.marca_cod,
          a.estado_marca,
          c.marca_desc,
          a.marca_causa,
          b.marca_desc,
          a.fecha_ini, 24
   FROM   cta_act_marca a,
   OUTER  tab_marca b,
   OUTER  tab_estado_marca c 
   WHERE  a.nss          = qnss 
   AND    a.marca_causa  = b.marca_cod
   AND    a.estado_marca = c.marca_estado

   ##### MARCA HISTORICA #####
   INSERT INTO tmp_his_afi
   SELECT '08',
          'MCH', 
          a.correlativo, 
          '0',
          a.marca_cod,
          a.estado_marca,
          c.marca_desc,
          a.marca_causa,
          b.marca_desc,
          a.fecha_fin, 25
   FROM   cta_his_marca a, 
   OUTER  tab_marca b,
   OUTER  tab_estado_marca c
   WHERE  a.nss            = qnss
   AND    a.fecha_fin IS NOT NULL
   AND    a.marca_causa    = b.marca_cod
   AND    a.estado_marca   = c.marca_estado

END FUNCTION

################################################################################
FUNCTION unificacion(qnss)

   DEFINE qnss          CHAR(011)
   DEFINE sel_ins       CHAR(900)

   ##### REGISTRO POR UNIFICACION #####
   INSERT INTO tmp_his_afi
   SELECT cve_operacion,
          'UNI', 
          folio,
          tipo_registro,
          estado,
          '0',
          '0',
          cve_afo_ced,
          afore_desc,
          fecha_certifica, 26
   FROM   uni_det_certifica, 
   OUTER  tab_afore
   WHERE  nss         = qnss
   AND    cve_afo_ced = afore_cod

   ##### UNIFICACIONES #####
   INSERT INTO tmp_his_afi
   SELECT '00',
          'UNI', 
          folio,
          '0',
          estado,
          '0',
          '0',
          cve_afo_aclara,
          afore_desc,
          fnotifica, 27
   FROM   uni_unificado, 
   OUTER  tab_afore
   WHERE  nss_cta1       = qnss
   AND    cve_afo_aclara = afore_cod

   ##### REGISTRO POR UNIFICACION (ASIGNACIONES MISMA AFORE 60-32) #####
   INSERT INTO tmp_his_afi
   SELECT cve_operacion,
          'UNI', 
          folio,
          tipo_registro,
          estado,
          '0',
          '0',
          cve_afo_ced,
          afore_desc,
          fecha_certifica, 28
   FROM   uni_det_asignado, 
   OUTER  tab_afore
   WHERE  nss         = qnss
   AND    cve_afo_ced = afore_cod

END FUNCTION
################################################################################
FUNCTION extrae_datos2(f_reg2)
  DEFINE f_reg2          RECORD
    nss                  CHAR(011),
    curp                 CHAR(018),
    rfc                  CHAR(013),
    nombre               CHAR(120),
    fecha_cert           DATE,
    fecha_aper           DATE,
    fecha_naci           DATE,
    edad                 SMALLINT,
    reg_edad             SMALLINT,
    reg_actual           SMALLINT
  END RECORD

  DEFINE vconsulta       SMALLINT

  DEFINE v_nombre_tabla  CHAR(40),
         v_anio          SMALLINT,
         v_anio_ini      SMALLINT,
         v_anio_fin      SMALLINT

  DEFINE v_anio_c        CHAR(02)

  DEFINE v_fecha         DATE

  OPEN WINDOW ventana_2 AT 3,3 WITH FORM "CTAM0112" ATTRIBUTE( BORDER)
    DISPLAY " CTAM011           CONSULTA HISTORIAL PROCESOS POR AFILIADO                        " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

    ERROR "Desplegando detalle." SLEEP 2

    INITIALIZE g_reg3.* TO NULL

    LET fconv_ini = MDY(01,01,YEAR(HOY))
    LET fconv_fin = TODAY
    LET vconsulta = FALSE
    
    INPUT BY NAME fconv_ini, fconv_fin WITHOUT DEFAULTS
      BEFORE FIELD fconv_ini

        DISPLAY BY NAME f_reg2.nss 
        DISPLAY BY NAME f_reg2.curp 
        DISPLAY BY NAME f_reg2.rfc 
        DISPLAY BY NAME f_reg2.nombre
        DISPLAY BY NAME f_reg2.fecha_cert
        DISPLAY BY NAME f_reg2.fecha_aper
        DISPLAY BY NAME f_reg2.fecha_naci
        DISPLAY BY NAME f_reg2.edad
        DISPLAY BY NAME f_reg2.reg_edad
        DISPLAY BY NAME f_reg2.reg_actual
        DISPLAY BY NAME fconv_ini
        DISPLAY BY NAME fconv_fin
    
      AFTER FIELD fconv_ini
        IF fconv_ini IS NULL THEN
           ERROR "La fecha inicial no puede ser nula."
           NEXT FIELD fconv_ini
        ELSE
           NEXT FIELD fconv_fin
        END IF

      AFTER FIELD fconv_fin
        IF fconv_fin IS NULL THEN
           ERROR "La fecha fin no puede ser nula."
           NEXT FIELD fecha_fin
        ELSE
           IF fconv_ini > fconv_fin THEN
              ERROR "La fecha inicial no puede ser mayor a la fecha final."
              NEXT FIELD fconv_ini
           ELSE
              LET vconsulta = TRUE
              EXIT INPUT 
           END IF
        END IF

        ON KEY (INTERRUPT)
           PROMPT "Presione <ENTER> para salir." FOR aux_pausa
           INITIALIZE g_reg3.*  TO NULL
           EXIT INPUT
    END INPUT

    IF vconsulta = TRUE THEN

       WHENEVER ERROR CONTINUE
          DROP TABLE tmp_dis_cuenta
       WHENEVER ERROR STOP

       LET sel_his = ""

       LET v_anio_ini = YEAR( fconv_ini )
       LET v_anio_fin = YEAR( fconv_fin )
 

       FOR v_anio = v_anio_ini TO v_anio_fin
           LET v_fecha  = MDY(1,1,v_anio)
           LET v_anio_c = v_fecha USING "YY"
   
           LET v_nombre_tabla = "dis_cuenta",v_anio_c CLIPPED
   
           SELECT "tabla"
           FROM   systables
           WHERE  tabname = v_nombre_tabla
           IF SQLCA.SQLCODE = 0 THEN
              LET sel_his = sel_his CLIPPED,
                           " SELECT siefore,subcuenta,tipo_movimiento,",
                           " folio,id_aportante,monto_en_acciones,",
                           " monto_en_pesos,fecha_conversion",
                           " FROM  ",v_nombre_tabla          ,
                           " WHERE nss = ","'",f_reg2.nss,"'"  ,
                           " UNION ALL "
           END IF
        END FOR
   
        LET sel_his = sel_his CLIPPED,
                      " SELECT siefore,subcuenta,tipo_movimiento,",
                      " folio,id_aportante,monto_en_acciones,",
                      " monto_en_pesos,fecha_conversion",
                      " FROM dis_cuenta ",
                      " WHERE nss = ","'",f_reg2.nss,"'"  ,
                      " AND fecha_conversion >= ","'",fconv_ini,"'",
                      " AND fecha_conversion <= ","'",fconv_fin,"'",
                      " INTO TEMP tmp_dis_cuenta "
        PREPARE eje_sel_his FROM sel_his
        EXECUTE eje_sel_his
   
        UPDATE STATISTICS FOR TABLE tmp_dis_cuenta
        CREATE INDEX tmp_dis_cuenta2 on tmp_dis_cuenta (fecha_conversion,siefore)
        UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

        LET sel_where3 = "SELECT *",
                         " FROM tmp_dis_cuenta",
                         " ORDER BY 8 desc, 1,2,3,4,5"

        LET pos2 = 2

        IF (pos2-1) >= 1 THEN
           CALL  SET_COUNT(pos2-1)

           PREPARE query3 FROM sel_where3
           DECLARE cursor_3 CURSOR FOR query3

           LET pos2      = 1
           LET total_arh = 0

           FOREACH cursor_3 INTO l_record3[pos2].*

             LET pos2      = pos2      + 1
             LET total_arh = total_arh + 1    
      
           END FOREACH

           DISPLAY BY NAME total_arh

           ERROR "Consulta finalizada. . ."

           IF (pos2-1) >= 1 THEN
              CALL SET_COUNT(pos2-1)
      
              DISPLAY ARRAY l_record3 TO scr_3.*
              ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
      
              ON KEY (CONTROL-P)
                 ERROR "PROCESANDO IMPRESION..."
                 CALL impresion2(pos2)
      
              ON KEY (INTERRUPT)
                 INITIALIZE l_record3 TO NULL
                 EXIT DISPLAY
              END DISPLAY
           ELSE
             ERROR "CONSULTA DE STATUS VACIA"
             SLEEP 2
             ERROR ""
           END IF

           DISPLAY "" AT 1,1
           DISPLAY "" AT 2,1
           DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
           DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)
        END IF
    END IF 

    CLOSE WINDOW ventana_2
END FUNCTION
################################################################################
FUNCTION impresion2(pos)
  DEFINE
    i,
    pos               SMALLINT

  LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                ".CONS_PROC_TRAB_SDO.", g_reg1.nss CLIPPED, "_",
                hoy USING "ddmmyyyy" CLIPPED

  DISPLAY "Nombre reporte: ", g_impre AT 19,1

  START REPORT rpt_tabrdeta2 TO g_impre

    FOR i = 1 TO (pos - 1)
       LET g_reg3.siefore           = l_record3[i].siefore
       LET g_reg3.subcuenta         = l_record3[i].subcuenta
       LET g_reg3.tipo_movimiento   = l_record3[i].tipo_movimiento
       LET g_reg3.folio             = l_record3[i].folio
       LET g_reg3.id_aportante      = l_record3[i].id_aportante
       LET g_reg3.monto_en_acciones = l_record3[i].monto_en_acciones
       LET g_reg3.monto_en_pesos    = l_record3[i].monto_en_pesos
       LET g_reg3.fecha_conversion  = l_record3[i].fecha_conversion

       IF g_reg3.siefore IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tabrdeta2(i, g_reg3.*)
    END FOR
  
    FINISH REPORT rpt_tabrdeta2

    ERROR "LISTADO GENERADO..."
    SLEEP 2
    ERROR ""

    --LET g_lista = "lp ",g_impre
    --LET g_lista = "vi ",g_impre
    --RUN g_lista
END FUNCTION
#####################################################################

################################################################################
REPORT rpt_tabrdeta2(li, lg_reg3)

   DEFINE li           SMALLINT

   DEFINE lg_reg3      RECORD
     siefore           SMALLINT,
     subcuenta         SMALLINT,
     tipo_movimiento   SMALLINT,
     folio             INTEGER,
     id_aportante      CHAR(11),
     monto_en_acciones DECIMAL(22,6),
     monto_en_pesos    DECIMAL(22,6),
     fecha_conversion  DATE
   END RECORD

   DEFINE
     vrazon_social     CHAR(08),
     vsubct_desc       CHAR(40),
     vdescripcion      CHAR(80) 

   DEFINE
     vdiag_proceso     CHAR(02),
     vdiagn_desc       CHAR(80)

   DEFINE lreg_proc    RECORD
     cve_operacion     CHAR(003),
     desc_operacion    CHAR(045)
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
         PRINT COLUMN 001,"PROGRAMA: CTAM011 ",
               COLUMN 065,"LISTADO DE CONSULTA HISTORIAL PROCESOS POR AFILIADO (SALDOS)",
               COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
         PRINT

         PRINT COLUMN 001,"NSS: ",  g_reg1.nss,
               COLUMN 020,"CURP: ", g_reg1.curp,
               COLUMN 050,"RFC: ",  g_reg1.rfc
         PRINT COLUMN 001,"Nombre: ", g_reg1.nombre CLIPPED
         PRINT COLUMN 001,"Fecha Certificacion: ",   g_reg1.fecha_cert
                                                     USING "DD-MM-YYYY",
               COLUMN 035,"Fecha Apertura Cuenta: ", g_reg1.fecha_aper
                                                     USING "DD-MM-YYYY",
               COLUMN 072,"Fecha Nacimiento: ",      g_reg1.fecha_naci
                                                     USING "DD-MM-YYYY"
         PRINT COLUMN 001,"Edad: ", g_reg1.edad USING "<<<",
               COLUMN 012,"Regimen por Edad: ", g_reg1.reg_edad USING "<",
               COLUMN 035,"Regimen Actual: ", g_reg1.reg_actual USING "<"  
         PRINT COLUMN 001,"Fecha Inicial Saldos: ", 
                          fconv_ini USING "DD-MM-YYYY",
                          " Fecha Final Saldos: ", 
                          fconv_fin USING "DD-MM-YYYY"
         SKIP 2 LINE

         PRINT COLUMN 001,"ID",
               COLUMN 007,"SIEFORE",
               COLUMN 019,"SUBCUENTA",
               COLUMN 043,"TIPO MOVIMIENTO",
               COLUMN 088,"FOLIO",
               COLUMN 096,"ID APORT",
               COLUMN 109,"MONTO EN ACCIONES",
               COLUMN 129,"MONTO EN PESOS",
               COLUMN 150,"F CONVER"

      ON EVERY ROW

         SELECT a.razon_social
           INTO vrazon_social
           FROM tab_siefore_local a
          WHERE a.codigo_siefore = lg_reg3.siefore

         SELECT a.subct_desc
           INTO vsubct_desc
           FROM tab_subcuenta a
          WHERE a.subct_cod = lg_reg3.subcuenta

         SELECT a.descripcion
           INTO vdescripcion
           FROM tab_movimiento a
          WHERE a.codigo = lg_reg3.tipo_movimiento

         PRINT COLUMN 001,li USING "<<<<<",
               COLUMN 007,lg_reg3.siefore USING "<<",
               COLUMN 009,vrazon_social CLIPPED,
               COLUMN 019,lg_reg3.subcuenta USING "<<",
               COLUMN 022,vsubct_desc[1,20] CLIPPED,
               COLUMN 043,lg_reg3.tipo_movimiento USING "<<<",
               COLUMN 047,vdescripcion[1,40] CLIPPED,
               COLUMN 088,lg_reg3.folio USING "<<<<<<&",
               COLUMN 096,lg_reg3.id_aportante CLIPPED,
               COLUMN 109,lg_reg3.monto_en_acciones USING "-##,###,##&.#####&",
               COLUMN 129,lg_reg3.monto_en_pesos USING "-##,###,##&.#####&",
               COLUMN 150,lg_reg3.fecha_conversion USING "DDMMYYYY"

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 01," Total de registros : ",
         COUNT(*) USING "<<<<"

END REPORT
#####################################################################
