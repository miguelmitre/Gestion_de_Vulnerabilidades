######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Owner             => E.F.P.                                         #
#Programa CTAB131  => CONSULTA REGISTROS HABILITADOS DE LA MARCA 151 #
#                     SALDO CERO.                                    #
#Fecha             => 16 de Diciembre del 2010                       # 
#Modificado por    => FERNANDO HERRERA HERNANDEZ                     #
#Sistema           => CTA.                                           #
######################################################################
DATABASE safre_af

GLOBALS

  DEFINE
    cfecha_inicio      DATE,
    cfecha_fin         DATE

  DEFINE g_reg         RECORD
    cnss               CHAR(11),
    ccurp              CHAR(18),
    ctipo_solicitud    SMALLINT,
    cdesc_tipo_sol     CHAR(20),
    cfentcons          DATE,
    cfecha_conversion  DATE,
    ctipo_mov          SMALLINT,
    cdesc_mov          CHAR(75)
  END RECORD

  DEFINE l_record      ARRAY[30000] OF RECORD 
    cnss               CHAR(11),
    ccurp              CHAR(18),
    ctipo_solicitud    SMALLINT,
    cdesc_tipo_sol     CHAR(20),
    cfentcons          DATE,
    cfecha_conversion  DATE,
    ctipo_mov          SMALLINT,
    cdesc_mov          CHAR(75)
  END RECORD

  DEFINE g_param_dis   RECORD LIKE seg_modulo.*

  DEFINE sw_1          SMALLINT,
    aux_pausa          CHAR(01),
    HOY                DATE,
    HORA               CHAR(8),
    aux_estad_desc     CHAR(40),
    seg_usuario        CHAR(08),
    pos                SMALLINT,
    cla_where          CHAR(900),
    sel_where          CHAR(900),
    pos2               SMALLINT,
    cla_where2         CHAR(2000),
    sel_where2         CHAR(2000),
    g_lista            CHAR(300),
    g_impre            CHAR(300),
    g_lista2           CHAR(300),
    g_impre2           CHAR(300),
    g_impre3           CHAR(300),
    vfecha_ini         DATE,
    gtotal             INTEGER,
    l                  SMALLINT 

  DEFINE
    total_reg          INTEGER

  DEFINE 
    v_fecha_conversion DATE,
    v_tipo_movimiento  SMALLINT,
    v_tip_mov_desc     CHAR(75),
    vconfirma          CHAR(1)

END GLOBALS
##########################################################################
MAIN
  OPTIONS 
    PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY control-o

  DEFER INTERRUPT

  CALL STARTLOG("CTAB131.log")
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

  LET HOY  = TODAY
  LET HORA = TIME

  OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CTAB1311" ATTRIBUTE( BORDER)
    DISPLAY " CTAB131     CONSULTA REGISTROS HABILITADOS MARCA SALDO CERO                   " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    MENU "CONSULTA"
      COMMAND "Registros Habilitados" "Consulta Registros Habilitados"
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
  LET total_reg       = 0

  DISPLAY "                                                                                                                                                            " AT 20,1

  FOR j = 1 TO 10      
      INITIALIZE l_record[j].* TO NULL
      DISPLAY l_record[j].*
  END FOR

  DISPLAY "                                                                               " AT 16,1

END FUNCTION
################################################################################
FUNCTION Consulta()

  LET pos = 2

  IF (pos-1) >= 1 THEN
     CALL  SET_COUNT(pos-1)

     LET int_flag = FALSE

     INPUT BY NAME cfecha_inicio, cfecha_fin
       BEFORE INPUT
         LET cfecha_inicio = MDY(MONTH(HOY),1,YEAR(HOY))
         --LET cfecha_inicio = cfecha_inicio - 1 UNITS DAY

         CALL f_dia_ultimo_mes(HOY)
         RETURNING cfecha_fin

         DISPLAY BY NAME cfecha_inicio 
         DISPLAY BY NAME cfecha_fin
         DISPLAY BY NAME total_reg

       AFTER FIELD cfecha_inicio 
         IF cfecha_inicio IS NULL THEN
            ERROR "El campo fecha inicio no debe ser nulo."
            SLEEP 2
            NEXT FIELD cfecha_inicio
         END IF

       AFTER FIELD cfecha_fin
         IF cfecha_fin IS NULL THEN
            ERROR "El campo fecha fin no debe ser nulo."
            SLEEP 2
            NEXT FIELD cfecha_fin
         END IF
  
         IF cfecha_inicio > cfecha_fin THEN
            ERROR "La fecha inicio no puede ser mayor a la fecha fin."
            SLEEP 2
            NEXT FIELD cfecha_inicio
         ELSE 
            EXIT INPUT
         END IF

       ON KEY (CONTROL-C)
          LET int_flag = TRUE
          EXIT INPUT

     END INPUT

     IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
     END IF


     SELECT 'X' 
     FROM   safre_tmp:cuota_afore
     GROUP BY 1
     IF STATUS = NOTFOUND THEN
        ERROR "Generando cuota de mercado."
        CALL genera_cuota()
     ELSE 
        WHILE TRUE
          PROMPT "La cuota ya esta generada, desea generarla nuevamente S/N: "
          FOR vconfirma 

          IF vconfirma MATCHES "[sS]" THEN
             ERROR "Generando cuota de mercado."
             CALL genera_cuota()
             EXIT WHILE
          ELSE
             EXIT WHILE
          END IF
        END WHILE
     END IF

     ERROR "Generando información de marcas."

     LET sel_where = "SELECT m.nss,",
                     "       m.curp,",
                     "       m.tipo_solicitud,",
                     "       ts.desc_solicitud,",
                     "       m.fentcons",
                     " FROM  safre_tmp:cuota_afore m,",
                     "       cta_his_marca c,",
                     "       tab_tipo_solic ts",
                     " WHERE m.nss = c.nss",
                     " AND   c.marca_cod = 160",
                     " AND   c.fecha_fin IS NOT NULL",
                     " AND   c.fecha_fin >= '", cfecha_inicio, "'",
                     " AND   c.fecha_fin <= '", cfecha_fin, "'",
                     " AND   c.marca_causa NOT IN (120,130,150,151,160)",
                     " AND   m.tipo_solicitud   = ts.tipo_solicitud ",
                     " ORDER BY m.fentcons, nss, curp"

     PREPARE query_p FROM sel_where
     DECLARE cursor_p CURSOR FOR query_p

     LET pos       = 1
     LET total_reg = 0
     FOREACH cursor_p INTO l_record[pos].*

       --LET g_reg.cnss         = l_record[pos].cnss
       --LET g_reg.ccurp        = l_record[pos].ccurp

       DECLARE cur_12 CURSOR FOR
       SELECT a.fecha_conversion,
              a.tipo_movimiento
       FROM   dis_cuenta a
       WHERE  a.nss               = l_record[pos].cnss
       AND    a.fecha_conversion <= cfecha_fin
       AND    a.tipo_movimiento NOT BETWEEN 100 AND 110
       AND    a.tipo_movimiento NOT BETWEEN 900 AND 999
       ORDER BY 1 DESC
       FOREACH cur_12 INTO v_fecha_conversion,
                           v_tipo_movimiento
         EXIT FOREACH
       END FOREACH

       IF v_fecha_conversion IS NULL        OR
          v_fecha_conversion = '12/31/1899' THEN
          LET v_fecha_conversion = '' 
       END IF

       LET l_record[pos].cfecha_conversion = v_fecha_conversion
      
       SELECT a.descripcion
       INTO   v_tip_mov_desc
       FROM   tab_movimiento a
       WHERE  a.codigo = v_tipo_movimiento

       LET l_record[pos].ctipo_mov = v_tipo_movimiento
       LET l_record[pos].cdesc_mov = v_tip_mov_desc 

       LET total_reg = total_reg + 1
       LET pos       = pos + 1

     END FOREACH

     ERROR "Proceso finalizado."

     DISPLAY BY NAME total_reg

     IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        DISPLAY ARRAY l_record TO scr_1.*
        ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
          {ON KEY (CONTROL-M)
             LET pos = ARR_CURR()
             CALL extrae_datos()
             INITIALIZE l_record TO NULL
             EXIT DISPLAY}

          ON KEY (INTERRUPT)
             INITIALIZE l_record TO NULL
             CLEAR FORM
             EXIT DISPLAY

          ON KEY (CONTROL-P)
             ERROR "PROCESANDO IMPRESION..."
             CALL impresion2(pos)
        END DISPLAY

        RETURN
        CLOSE WINDOW ventana_1
     ELSE
        ERROR "CONSULTA HABILITACION DE CUENTAS VACIA"
        SLEEP 2
        ERROR ""
        CLEAR FORM
        RETURN
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
FUNCTION f_dia_ultimo_mes(vhoy)
  DEFINE       
    vhoy       DATE,
    v_mes      SMALLINT,
    v_dia      SMALLINT,
    v_anio     SMALLINT,
    v_fin_mes  DATE,
    v_cociente SMALLINT 

  LET v_mes  = MONTH(vhoy)
  LET v_dia  = DAY(vhoy)
  LET v_anio = YEAR(vhoy)

  IF v_mes = 4  OR
     v_mes = 6  OR
     v_mes = 9  OR
     v_mes = 11 THEN
     LET v_fin_mes = MDY(MONTH(vhoy),30,YEAR(vhoy))
  END IF

  IF v_mes = 1  OR
     v_mes = 3  OR
     v_mes = 5  OR
     v_mes = 7  OR
     v_mes = 8  OR
     v_mes = 10 OR
     v_mes = 12 THEN
     LET v_fin_mes = MDY(MONTH(vhoy),31,YEAR(vhoy))
  END IF

  IF v_mes = 2 THEN
     LET v_cociente = v_anio / 4

     IF v_anio - (v_cociente * 4) <> 0 THEN
        LET v_fin_mes = MDY(MONTH(vhoy),28,YEAR(vhoy)) 
     ELSE
        LET v_fin_mes = MDY(MONTH(vhoy),29,YEAR(vhoy)) 
     END IF
  END IF

  RETURN v_fin_mes
END FUNCTION
################################################################################

################################################################################
FUNCTION genera_cuota()
#gc--------------------

  DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
      DROP TABLE cuota_afore
      DROP TABLE nss_inhab
    WHENEVER ERROR STOP

    CREATE TABLE cuota_afore
      (nss            char(11),
       tipo_solicitud smallint,
       curp           char(18),
       fentcons       date)

    CREATE TEMP TABLE nss_inhab 
      (nss  CHAR(11)) WITH NO LOG

    INSERT INTO nss_inhab
    SELECT t.n_seguro
      FROM safre_af:taa_cd_det_cedido t
     WHERE t.estado      in (103,12)
       AND t.fecha_trasp <= cfecha_fin ;

    INSERT INTO nss_inhab
    SELECT t.nss_cta1
      FROM safre_af:uni_unificado t
     WHERE t.estado   in (60, 100)
       AND t.fliquida <= cfecha_fin

    INSERT INTO nss_inhab
    SELECT c.nss
      FROM safre_af:cta_act_marca c
     WHERE c.marca_cod = 150

    CREATE INDEX nss_inhab1 on nss_inhab (nss)
    UPDATE STATISTICS FOR TABLE nss_inhab

    INSERT INTO cuota_afore
    SELECT a.n_seguro, a.tipo_solicitud, a.n_unico, a.fentcons
      FROM safre_af:afi_mae_afiliado a
     WHERE a.n_seguro NOT IN (SELECT nss
                              FROM nss_inhab)
       AND a.fentcons     <= cfecha_fin
       AND a.finicta      <= cfecha_fin

    INSERT INTO cuota_afore
    SELECT b.n_seguro,
           b.tipo_solicitud,
           a.n_unico,
           a.fentcons
    FROM   safre_af:afi_mae_afiliado a,
           safre_af:afi_his_afiliado b
    WHERE  a.n_seguro        = b.n_seguro
    AND    b.n_seguro   NOT IN (SELECT nss
                                FROM nss_inhab)
    AND    a.fentcons        > cfecha_fin
    AND    b.fentcons       <= cfecha_fin
    AND    b.tipo_solicitud  = 5
    AND    a.tipo_solicitud <> 2

    CREATE INDEX cuota_afo1 on cuota_afore (nss)
    CREATE INDEX cuota_afo2 on cuota_afore (fentcons)

    UPDATE STATISTICS FOR TABLE cuota_afore;

  DATABASE safre_af

END FUNCTION
################################################################################

################################################################################
FUNCTION impresion2(pos2)
   DEFINE
     i2,
     pos2    SMALLINT

   LET g_impre2 = g_param_dis.ruta_listados CLIPPED,"/L_",seg_usuario CLIPPED,
                  ".CONS_99CTA_01_",HOY USING "ddmmyyyy" CLIPPED,
                  HORA[1,2], HORA[4,5], HORA[7,8]

   DISPLAY "Nombre reporte: ", g_impre2 CLIPPED AT 20,1

   START REPORT rpt_tabrdeta2 TO g_impre2

   FOR i2 = 1 TO (pos2 - 1)
       LET g_reg.cnss               = l_record[i2].cnss
       LET g_reg.ccurp              = l_record[i2].ccurp
       LET g_reg.ctipo_solicitud    = l_record[i2].ctipo_solicitud
       LET g_reg.cdesc_tipo_sol     = l_record[i2].cdesc_tipo_sol
       LET g_reg.cfentcons          = l_record[i2].cfentcons
       LET g_reg.cfecha_conversion  = l_record[i2].cfecha_conversion
       LET g_reg.ctipo_mov          = l_record[i2].ctipo_mov
       LET g_reg.cdesc_mov          = l_record[i2].cdesc_mov

      IF g_reg.cnss IS NULL THEN
         EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_tabrdeta2(i2, g_reg.*)
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
     cnss              CHAR(11),
     ccurp             CHAR(18),
     ctipo_solicitud   SMALLINT,
     cdesc_tipo_sol    CHAR(20),
     cfentcons         DATE,
     cfecha_conversion DATE,
     ctipo_mov         SMALLINT,
     cdesc_mov         CHAR(75)
   END RECORD

   DEFINE
     codigo_afore      SMALLINT,
     razon_social      CHAR(50)

   OUTPUT
      TOP MARGIN    1
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
               COLUMN 035,"LISTADO DE CONSULTA CUENTAS HABILITADAS DE LA MARCA DE SALDO CERO ",
               COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
         PRINT

         PRINT COLUMN 01,"ID",
               COLUMN 08,"NSS",
               COLUMN 20,"CURP",
               COLUMN 39,"TIPO SOLICITUD",
               COLUMN 63,"FECHA CERT",
               COLUMN 74,"FECHA LIQ",
               COLUMN 85,"TIPO MOVIMIENTO"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,li2 USING "####&",
               COLUMN 08,g_reg2.cnss                CLIPPED,
               COLUMN 20,g_reg2.ccurp               CLIPPED,
               COLUMN 39,g_reg2.ctipo_solicitud     USING "##",
               COLUMN 42,g_reg2.cdesc_tipo_sol      CLIPPED,
               COLUMN 63,g_reg2.cfentcons           USING "DD/MM/YYYY",
               COLUMN 74,g_reg2.cfecha_conversion   USING "DD/MM/YYYY",
               COLUMN 85,g_reg2.ctipo_mov           USING "###",
               COLUMN 89,g_reg2.cdesc_mov           CLIPPED

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 01," Total de registros : ",
         COUNT(*) USING "<<<<"

END REPORT

#####################################################################
