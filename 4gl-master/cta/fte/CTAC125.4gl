###############################################################################
# Programa     => CTAC125                                                     #
# Descripcion  => REPORTE ESTADISTICO DE REGISTRO                             #
# Sistema      => CTA                                                         #
# Por          => FERNANDO HERRERA HERNANDEZ                                  #
# Fecha        => 14 DE MAYO DE 2009                                          #
# Modificado   =>                                                             #
# Fecha        =>                                                             #
###############################################################################
DATABASE safre_af

GLOBALS
  DEFINE g_reg           RECORD
    tipo_solicitud       SMALLINT,
    desc_solicitud       CHAR(25),
    total                INTEGER
  END RECORD

  DEFINE fecha_corte     DATE
  DEFINE vfecha_corte    CHAR(10)
  DEFINE vfecha_ini      CHAR(10)
  DEFINE fecha_ini       DATE

  DEFINE cta_reg         INTEGER
  DEFINE cta_asi         INTEGER
  DEFINE total_ts        INTEGER

  DEFINE g_param         RECORD LIKE seg_modulo.*

  DEFINE hoy             DATE,
         num             INTEGER,
         opc             CHAR(1),
         enter           CHAR(1),
         hora1           CHAR(4),
         usuario         CHAR(8),
         hora            CHAR(8),
         g_eco           CHAR(100),
         nom_arch        CHAR(100),
         rnom_arch       CHAR(100)
 
  DEFINE cla_where       CHAR(300),
         cla_sel         CHAR(900)
 
  DEFINE salida          CHAR(300),
         rsalida         CHAR(300),
         vimprime        CHAR(500)

  DEFINE arr1            ARRAY[1000] OF RECORD
    tipo_solicitud       SMALLINT,
    desc_solicitud       CHAR(25),
    total                INTEGER
    END RECORD

  DEFINE i               INTEGER
  DEFINE j               INTEGER
  DEFINE pantalla        SMALLINT
  DEFINE tot_total       INTEGER
  DEFINE vstatus_interno INTEGER

  DEFINE g_reg2          RECORD
    cod_operacion        CHAR(02),
    desc_cod_op          CHAR(40),
    desc_tip_reg         CHAR(20),
    tot_cod_op           INTEGER,
    tipo_registro        SMALLINT
  END RECORD

  DEFINE r_reg2          RECORD
    cod_operacion        CHAR(02),
    desc_cod_op          CHAR(40),
    desc_tip_reg         CHAR(20),
    tot_cod_op           INTEGER,
    tipo_registro        SMALLINT
  END RECORD

  DEFINE arr2            ARRAY[1000] OF RECORD
    cod_operacion        CHAR(02),
    desc_cod_op          CHAR(40),
    desc_tip_reg         CHAR(20),
    tot_cod_op           INTEGER
    END RECORD

  DEFINE rarr2           ARRAY[1000] OF RECORD
    cod_operacion        CHAR(02),
    desc_cod_op          CHAR(40),
    desc_tip_reg         CHAR(20),
    tot_cod_op           INTEGER
    END RECORD

  DEFINE total_est       INTEGER

END GLOBALS

#########################################################################
MAIN
  OPTIONS
    PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY CONTROL-I,
    COMMENT LINE LAST
    DEFER INTERRUPT

  CALL STARTLOG("CTAC125.log")
  CALL Inicializa()
  CALL proceso() #P

END MAIN
#########################################################################

FUNCTION proceso()
#p----------------
  OPEN WINDOW ventana1 AT 2,2 WITH FORM "CTAC1251" ATTRIBUTE(BORDER)

  DISPLAY " CTAC125                  ESTADISTICO REGISTRO                                 " AT 3,1 ATTRIBUTE(REVERSE)

  DISPLAY hoy USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

  MENU "ESTADISTICO REGISTRO"
    COMMAND "Consulta Cuota" "Consulta cuota por pantalla estadistico registro"
       CALL Inicializa()
       LET pantalla = 1
       CALL Genera_consulta() #Gc
       CLEAR SCREEN
       CLEAR SCREEN
    COMMAND "Reporte cuota" "Reporte cuota estadistico registro"
       CALL Inicializa()
       LET pantalla = 0
       CALL Genera_consulta() #Gc
    COMMAND "Consulta detalle estadistico regitro"
       CALL Genera_detalle() 
       CLEAR SCREEN
    COMMAND "Salida" "Salir del Programa"
       EXIT MENU
  END MENU

END FUNCTION
############################################################################
FUNCTION Inicializa()

    SELECT *,
          USER
    INTO   g_param.*,
           usuario
    FROM   seg_modulo
    WHERE  modulo_cod = "cta"
   
    LET hoy       = TODAY
    LET hora      = TIME
    LET hora1     = hora[1,2],hora[4,5]
    LET num       = 1

    LET salida    = g_param.ruta_listados CLIPPED,"/",usuario CLIPPED,
                    ".EST_REGISTRO.", hoy USING "DDMMYY","_", hora1

    LET nom_arch  = usuario CLIPPED,
                    ".EST_REGISTRO.", hoy USING "DDMMYY","_", hora1

    LET rsalida   = g_param.ruta_listados CLIPPED,"/",usuario CLIPPED,
                    ".EST_DET_REG.", hoy USING "DDMMYY","_", hora1

    LET rnom_arch = usuario CLIPPED,
                    ".EST_DET_REG.", hoy USING "DDMMYY","_", hora1

    LET i         = 1
    LET j         = 1
    LET pantalla  = 0

    INITIALIZE arr1[i].* TO NULL
    INITIALIZE arr2[j].* TO NULL
    INITIALIZE g_reg.*   TO NULL
    INITIALIZE g_reg2.*  TO NULL
    LET fecha_corte       = NULL
    LET vfecha_ini        = NULL
    LET fecha_ini         = NULL
    LET cta_reg           = 0
    LET cta_asi           = 0
    LET total_ts          = 0
    LET tot_total         = 0

END FUNCTION
########################################################################
FUNCTION Genera_consulta()

  INPUT BY NAME fecha_corte
    AFTER FIELD fecha_corte

      IF fecha_corte IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD fecha_corte
      ELSE
         EXIT INPUT
      END IF

    ON KEY ( INTERRUPT )
       EXIT PROGRAM
  END INPUT

  WHILE TRUE
    PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR enter
    IF enter MATCHES "[sSnN]" THEN
       IF enter MATCHES "[sS]" THEN
          EXIT WHILE
       ELSE
          ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE)
          SLEEP 2
          EXIT PROGRAM
       END IF
    END IF
  END WHILE

  LET vfecha_corte = fecha_corte
  LET vfecha_ini   = vfecha_corte[1,2], "/01/", vfecha_corte[7,10]
  LET fecha_ini    = vfecha_ini

  CALL genera_cuota (fecha_corte)

  DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

  IF pantalla = 0 THEN
     CALL Genera_reporte()
     ERROR "REPORTE GENERADO"
     RETURN
  END IF

  DECLARE cur_solicitud CURSOR FOR 
  SELECT a.tipo_solicitud, b.desc_solicitud, count(*)
    FROM safre_tmp:cuota_cta_est a, tab_tipo_solic b
   WHERE a.tipo_solicitud = b.tipo_solicitud
   GROUP BY 1,2
   ORDER BY 1,2
  FOREACH cur_solicitud INTO g_reg.*
    LET total_ts  = total_ts + g_reg.total

    IF g_reg.tipo_solicitud <> 5 THEN
       IF g_reg.tipo_solicitud <> 8 THEN
          IF g_reg.tipo_solicitud <> 12 THEN
             LET cta_reg = cta_reg + g_reg.total
          END IF
       END IF
    ELSE
       LET cta_asi = cta_asi + g_reg.total
    END IF

    LET tot_total = cta_reg + cta_asi      
       
    LET arr1[i].tipo_solicitud    = g_reg.tipo_solicitud
    LET arr1[i].desc_solicitud    = g_reg.desc_solicitud
    LET arr1[i].total             = g_reg.total

    LET i                         = i + 1
  END FOREACH

  OPEN WINDOW ventana2 AT 2,2 WITH FORM "CTAC1252" ATTRIBUTE(BORDER)
    DISPLAY "  < Ctrl - B > Detalle                                                         " AT 3,1 ATTRIBUTE(BOLD)

    DISPLAY " CTAC125              CONSULTA ESTADISTICO REGISTRO                         " AT 4,1 ATTRIBUTE(REVERSE)

    DISPLAY hoy USING "DD-MM-YYYY" AT 4,68 ATTRIBUTE(REVERSE)

    DISPLAY "CUENTAS AL ", fecha_corte USING "DD-MM-YYYY" AT 5,27

    DISPLAY BY NAME tot_total
    DISPLAY BY NAME cta_reg
    DISPLAY BY NAME cta_asi
    DISPLAY BY NAME total_ts

    IF (i - 1) >= 1 THEN
       CALL SET_COUNT(i-1)
       DISPLAY ARRAY arr1 TO scr_1.*
   
         ON KEY (INTERRUPT)
            EXIT DISPLAY

         ON KEY (CONTROL - B)
            CALL detalle()
            EXIT DISPLAY
            CLOSE WINDOW ventana2
            RETURN
       END DISPLAY
    ELSE
      ERROR "No existen registros. . ."
    END IF

    PROMPT  "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE)
    FOR enter

    DISPLAY "CERRANDO CONSULTA " AT 19,1 ATTRIBUTE(REVERSE)
   
    CLEAR SCREEN
  CLOSE WINDOW ventana2
  RETURN

END FUNCTION
##########################################################################
FUNCTION detalle()

  OPEN WINDOW ventana3 AT 2,2 WITH FORM "CTAC1253" ATTRIBUTE(BORDER)
    DISPLAY "                                                                               " AT 3,1 ATTRIBUTE(BOLD)

    DISPLAY " CTAC125              CONSULTA DETALLE DE REGISTRO                          " AT 4,1 ATTRIBUTE(REVERSE)

    DISPLAY hoy USING "DD-MM-YYYY" AT 4,68 ATTRIBUTE(REVERSE)

    DISPLAY "DETALLE DEL REGISTRO DEL ", fecha_ini USING "DD-MM-YYYY",
            " AL ", fecha_corte USING "DD-MM-YYYY" AT 5,14

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL conforma_registro()

    DECLARE cur_est CURSOR FOR
    SELECT a.cod_operacion, b.desc_cod_op, b.desc_tip_reg, a.total,
           b.tipo_registro
      FROM tmp_estadistico a, cta_afi_cod_op b
     WHERE a.cod_operacion = b.cod_operacion
       AND a.tipo_registro = b.tipo_registro
     ORDER BY b.tipo_registro, a.cod_operacion
    FOREACH cur_est INTO g_reg2.*

      IF (g_reg2.tipo_registro = 3)    OR
         (g_reg2.tipo_registro = 4)    OR 
         (g_reg2.tipo_registro = 5     AND
          g_reg2.cod_operacion = '00') THEN
         LET g_reg2.tot_cod_op = g_reg2.tot_cod_op * -1
      END IF
  
      LET total_est = total_est + g_reg2.tot_cod_op

      LET arr2[j].cod_operacion = g_reg2.cod_operacion
      LET arr2[j].desc_cod_op   = g_reg2.desc_cod_op
      LET arr2[j].desc_tip_reg  = g_reg2.desc_tip_reg
      LET arr2[j].tot_cod_op    = g_reg2.tot_cod_op
  
      LET j                         = j + 1
    END FOREACH

    DISPLAY "CONSULTA TERMINADA     " AT 19,1 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME total_est   
  
    IF (j - 1) >= 1 THEN
       CALL SET_COUNT(j-1)
       DISPLAY ARRAY arr2 TO scr_2.*

         ON KEY (INTERRUPT)
            PROMPT  "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE)
            FOR enter

            DISPLAY "CERRANDO CONSULTA " AT 19,1 ATTRIBUTE(REVERSE) 
            SLEEP 2
            EXIT PROGRAM

       END DISPLAY
    ELSE
      ERROR "No existen registros. . ."
    END IF

    DISPLAY "                       " AT 19,1 ATTRIBUTE(REVERSE)

    CLEAR SCREEN
  CLOSE WINDOW ventana3
  RETURN

END FUNCTION
##########################################################################
FUNCTION Genera_reporte()

  DEFINE r_reg           RECORD
    tipo_solicitud       SMALLINT,
    desc_solicitud       CHAR(25),
    total                INTEGER
  END RECORD
  
  LET num = 0

  START REPORT rep_solicitud TO salida

  DECLARE cur_reporte CURSOR FOR 
  SELECT a.tipo_solicitud, b.desc_solicitud, count(*)
    FROM safre_tmp:cuota_cta_est a, tab_tipo_solic b
   WHERE a.tipo_solicitud = b.tipo_solicitud
   GROUP BY 1,2
   ORDER BY 1,2
  FOREACH cur_reporte INTO r_reg.*
    LET num       = num + 1

    LET total_ts  = total_ts + r_reg.total

    IF r_reg.tipo_solicitud <> 5 THEN
       IF r_reg.tipo_solicitud <> 12 THEN
          LET cta_reg = cta_reg + r_reg.total
       END IF
    ELSE
       LET cta_asi = cta_asi + r_reg.total
    END IF

    LET tot_total = cta_reg + cta_asi      

       
    OUTPUT TO REPORT rep_solicitud(r_reg.*, cta_reg, cta_asi, tot_total, num,
                                   fecha_corte, fecha_ini)

    LET r_reg.total = 0
  END FOREACH
  FINISH REPORT rep_solicitud

  DISPLAY "Nombre del Reporte: ", salida AT 18,2

  {LET g_eco = "echo ", nom_arch CLIPPED, " > ",
              g_param.ruta_listados CLIPPED,"/rescate.CIFRAS_TOT_AFI.",
              hoy USING "DDMMYY","_",hora1

  RUN g_eco}

END FUNCTION
##########################################################################
REPORT rep_solicitud(r_reg, rcta_reg, rcta_asi, rtot_total, rnum, 
                     rfecha_corte, rfecha_ini)

  DEFINE r_reg RECORD
    tipo_solicitud     SMALLINT,
    desc_solicitud     CHAR(25),
    total              INTEGER
    END RECORD

  DEFINE rcta_reg      INTEGER
  DEFINE rcta_asi      INTEGER
  DEFINE rtot_total    INTEGER
  DEFINE rnum          SMALLINT
  DEFINE rfecha_ini    DATE
  DEFINE rfecha_corte  DATE

  DEFINE L1            CHAR(01)
  DEFINE L5            CHAR(05)
  DEFINE L10           CHAR(10)
  DEFINE cod_afore     SMALLINT
  DEFINE descripcion   CHAR(30)
  DEFINE codigo_estado SMALLINT
  DEFINE totales       INTEGER
  DEFINE estado_total  INTEGER
    
  OUTPUT
    PAGE   LENGTH 90
    TOP    MARGIN 0
    BOTTOM MARGIN 0
    RIGHT  MARGIN 150
    LEFT   MARGIN 0
    ORDER BY r_reg.tipo_solicitud, r_reg.desc_solicitud

  FORMAT
    PAGE HEADER
      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
      PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

      LET totales = 0

      LET L1  = "\304"
      LET L5  = "\304\304\304\304\304"
      LET L10 = "\304\304\304\304\304\304\304\304\304\304"

      SELECT a.codigo_afore,b.afore_desc
      INTO   cod_afore,descripcion
      FROM   tab_afore_local a,tab_afore b
      WHERE  a.codigo_afore = b.afore_cod
  
      PRINT
      PRINT COLUMN 1,'\033e\033(s218T\033(s11H\033(s7N'
      PRINT COLUMN 06,cod_afore,"     ",descripcion,
            COLUMN 92,"FECHA : ",TODAY USING"DD/MM/YYYY"
      SKIP 4 LINES

      PRINT COLUMN 12, "R E P O R T E     E S T A D I S T I C O   D E   R E G I S T R O  D E L  ", rfecha_ini USING "DD/MM/YYYY", "  A L  ", rfecha_corte USING "DD/MM/YYYY"
      SKIP 2 LINES

      PRINT
      PRINT COLUMN 9, "PROGRAMA : CTAC125"

      PRINT
      PRINT COLUMN 9, "PAGINA   : ",PAGENO USING"####"

      PRINT
      PRINT '\033e\033(s218T\033(s14H\033(s7B'
      SKIP 1 LINES

      PRINT COLUMN 10,"\332",L10,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L10,L10,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\277"

      PRINT COLUMN 10,"|             |",
                      "              |",
                      "              |",
                      "              |",
                      "                                |",
                      "    NUMERO    |"
  
      PRINT COLUMN 10,"|             |",
                      " T. SOLICITUD |",
                      " DESC. SOLIC. |",
                      "    STATUS    |",
                      "    DESCRIPCION  STATUS         |",
                      "      DE      |"
  
      PRINT COLUMN 10,"|             |",
                      "              |",
                      "              |",
                      "              |",
                      "                                |",
                      "   REGISTROS  |"
  

      PRINT COLUMN 10,"\300",L10,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L10,L10,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\331"

    ON EVERY ROW
      SKIP 1 LINES
      
      PRINT
      PRINT COLUMN 014, rnum                    USING "###",
            COLUMN 031, r_reg.tipo_solicitud    USING "##",
            COLUMN 042, r_reg.desc_solicitud    CLIPPED,
            COLUMN 103, r_reg.total             USING "##,###,##&"
  

      PRINT COLUMN 10,"\304",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L5,L1,L1
          
    ON LAST ROW
      SKIP 3 LINES
  
      PRINT COLUMN 10,"\332",L10,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L10,L10,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\277"
  

      PRINT COLUMN 10,"|             |",
                      "              |",
                      "              |",
                      "              |",
                      "       TOTALES:                 |",
            COLUMN 102,SUM(r_reg.total) USING "###,###,###",
                      "     |"
      SKIP 1 LINES


      PRINT COLUMN 10,"\300",L10,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L10,L10,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\331"
      SKIP 1 LINES

      PRINT COLUMN 9, "TOTAL REGISTRADAS AL ", rfecha_corte, ": ",  
                      rcta_reg USING "##,###,###",
            COLUMN 9, "Menos T.S. 8-NO AFILIADOS y 12-REGISTRO INTERNET"

      PRINT COLUMN 9, "TOTAL ASIGNADAS AL   ", rfecha_corte, ": ",  
                      rcta_asi USING "##,###,###"

      PRINT COLUMN 9, "TOTAL                          : ", 
                      rtot_total USING "##,###,###"

END REPORT
#########################################################################
FUNCTION genera_cuota ( l_fecha_corte )
#gc------------------------------------

    DEFINE l_fecha_corte DATE
    DEFINE v_comando     CHAR(200)

    DATABASE safre_tmp
        LET v_comando = "EXECUTE PROCEDURE amafore_cta_est('",l_fecha_corte,"')"

        PREPARE eje_cuota FROM v_comando
        ERROR "GENERANDO CUOTA DE MERCADO ..."
        EXECUTE eje_cuota
        ERROR "INFORMACION GENERADA"
        SLEEP 3
        ERROR ""
    DATABASE safre_af

END FUNCTION
#########################################################################
FUNCTION Genera_detalle()

  DEFINE limite SMALLINT

  OPEN WINDOW ventana4 AT 2,2 WITH FORM "CTAC1254" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl - P > Imprimir                                                         " AT 3,1 ATTRIBUTE(BOLD)

    DISPLAY " CTAC125              CONSULTA DETALLE DE REGISTRO                          " AT 4,1 ATTRIBUTE(REVERSE)

    DISPLAY hoy USING "DD-MM-YYYY" AT 4,68 ATTRIBUTE(REVERSE)

    INPUT BY NAME fecha_ini, fecha_corte
      AFTER FIELD fecha_ini
        IF fecha_ini IS NULL THEN
           ERROR "Campo NO puede ser NULO"
           NEXT FIELD fecha_ini
        ELSE
           NEXT FIELD fecha_corte
        END IF
  
      AFTER FIELD fecha_corte
        IF fecha_corte IS NULL THEN
           ERROR "Campo NO puede ser NULO"
           NEXT FIELD fecha_corte
        ELSE
           IF fecha_corte < fecha_ini THEN
              ERROR "Fecha Final -NO- puede ser menor a la Fecha Inicial"
              NEXT FIELD fecha_corte
           ELSE
              EXIT INPUT
           END IF 
        END IF
  
      ON KEY ( INTERRUPT )
         EXIT PROGRAM
    END INPUT
  
    WHILE TRUE
      PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR enter
      IF enter MATCHES "[sSnN]" THEN
         IF enter MATCHES "[sS]" THEN
            EXIT WHILE
         ELSE
            ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE)
            SLEEP 2
            EXIT PROGRAM
         END IF
      END IF
    END WHILE
  
    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
  
    DISPLAY "DETALLE DEL REGISTRO DEL ", fecha_ini USING "DD-MM-YYYY",
            " AL ", fecha_corte USING "DD-MM-YYYY" AT 5,14
 
    CALL conforma_registro() 
  
    #START REPORT rep_detalle TO rsalida
    DECLARE cur_rest CURSOR FOR
    SELECT a.cod_operacion, b.desc_cod_op, b.desc_tip_reg, a.total,
           b.tipo_registro
      FROM tmp_estadistico a, cta_afi_cod_op b
     WHERE a.cod_operacion = b.cod_operacion
       AND a.tipo_registro = b.tipo_registro
     ORDER BY b.tipo_registro, a.cod_operacion
    FOREACH cur_rest INTO r_reg2.*
  
      IF (r_reg2.tipo_registro = 3)    OR
         (r_reg2.tipo_registro = 4)    OR 
         (r_reg2.tipo_registro = 5     AND
          r_reg2.cod_operacion = '00') THEN
         LET r_reg2.tot_cod_op = r_reg2.tot_cod_op * -1
      END IF
    
      LET total_est = total_est + r_reg2.tot_cod_op
  
      LET rarr2[j].cod_operacion = r_reg2.cod_operacion
      LET rarr2[j].desc_cod_op   = r_reg2.desc_cod_op
      LET rarr2[j].desc_tip_reg  = r_reg2.desc_tip_reg
      LET rarr2[j].tot_cod_op    = r_reg2.tot_cod_op
      LET j                      = j + 1

      {OUTPUT TO REPORT rep_detalle (r_reg2.*, total_est, 
                                    fecha_corte, fecha_ini)}
  
    END FOREACH
    #FINISH REPORT rep_detalle
  
    DISPLAY "CONSULTA TERMINADA     " AT 19,1 ATTRIBUTE(REVERSE)
  
    DISPLAY BY NAME total_est   
    
    IF (j - 1) >= 1 THEN
       CALL SET_COUNT(j-1)
       DISPLAY ARRAY rarr2 TO scr_3.*

       ON KEY (CONTROL - P)
          LET limite = j
          LET limite = limite - 1
          ERROR "Generando reporte"
          START REPORT rep_detalle TO rsalida
          FOR j = 1 TO limite

            LET r_reg2.cod_operacion = rarr2[j].cod_operacion
            LET r_reg2.desc_cod_op   = rarr2[j].desc_cod_op
            LET r_reg2.desc_tip_reg  = rarr2[j].desc_tip_reg
            LET r_reg2.tot_cod_op    = rarr2[j].tot_cod_op

            OUTPUT TO REPORT rep_detalle (r_reg2.*, total_est, 
                                          fecha_corte, fecha_ini)
          END FOR
          FINISH REPORT rep_detalle

          DISPLAY "Nombre del Reporte: ", rsalida AT 21,2
          ERROR "Reporte generado"
  
       ON KEY (INTERRUPT)
          PROMPT  "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE)
          FOR enter
  
          DISPLAY "CERRANDO CONSULTA " AT 19,1 ATTRIBUTE(REVERSE) 
          SLEEP 2
          EXIT PROGRAM
  
       END DISPLAY
    ELSE
       ERROR "No existen registros. . ."
    END IF
  
    DISPLAY "                       " AT 19,1 ATTRIBUTE(REVERSE)
  
    CLEAR SCREEN
    CLOSE WINDOW ventana4
    RETURN

END FUNCTION
#########################################################################
REPORT rep_detalle(rdet, rtotal_est, rfecha_corte, rfecha_ini)

  DEFINE rdet            RECORD
    cod_operacion        CHAR(02),
    desc_cod_op          CHAR(40),
    desc_tip_reg         CHAR(20),
    tot_cod_op           INTEGER,
    tipo_registro        SMALLINT
    END RECORD

  DEFINE rtotal_est    INTEGER
  DEFINE rfecha_corte  DATE
  DEFINE rfecha_ini    DATE

  DEFINE L1            CHAR(01)
  DEFINE L5            CHAR(05)
  DEFINE L10           CHAR(10)
  DEFINE cod_afore     SMALLINT
  DEFINE descripcion   CHAR(30)
  DEFINE codigo_estado SMALLINT
  DEFINE totales       INTEGER
  DEFINE estado_total  INTEGER
    
  OUTPUT
    PAGE   LENGTH 90
    TOP    MARGIN 0
    BOTTOM MARGIN 0
    RIGHT  MARGIN 150
    LEFT   MARGIN 0
    #ORDER BY r_reg.tipo_solicitud, r_reg.desc_solicitud

  FORMAT
    PAGE HEADER
      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
      PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

      LET totales = 0

      LET L1  = "\304"
      LET L5  = "\304\304\304\304\304"
      LET L10 = "\304\304\304\304\304\304\304\304\304\304"

      SELECT a.codigo_afore,b.afore_desc
      INTO   cod_afore,descripcion
      FROM   tab_afore_local a,tab_afore b
      WHERE  a.codigo_afore = b.afore_cod
  
      PRINT
      PRINT COLUMN 1,'\033e\033(s218T\033(s11H\033(s7N'
      PRINT COLUMN 06,cod_afore,"     ",descripcion,
            COLUMN 92,"FECHA : ",TODAY USING"DD/MM/YYYY"
      SKIP 4 LINES

      PRINT COLUMN 12, "R E P O R T E   D E T A L L E   E S T A D I S T I C O   D E   R E G I S T R O   D E L   ", rfecha_ini USING "DD/MM/YYYY", "   A L   ", rfecha_corte USING "DD/MM/YYYY"
      SKIP 2 LINES

      PRINT
      PRINT COLUMN 9, "PROGRAMA : CTAC125"

      PRINT
      PRINT COLUMN 9, "PAGINA   : ",PAGENO USING"####"

      PRINT
      PRINT '\033e\033(s218T\033(s14H\033(s7B'
      SKIP 1 LINES

      PRINT COLUMN 10,"\332",L10,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L10,L10,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\277"

      PRINT COLUMN 10,"| CODIGO  |",
                      "                                        |",
                      "                                        |",
                      "    NUMERO    |"
  
      PRINT COLUMN 10,"|OPERACION|",
                      "      DESCRIPCION CODIGO OPERACION      |",
                      "      DESCRIPCION TIPO DE REGISTRO      |",
                      "      DE      |"
  
      PRINT COLUMN 10,"|         |",
                      "                                        |",
                      "                                        |",
                      "   REGISTROS  |"
  

      PRINT COLUMN 10,"\300",L10,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L10,L10,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\331"

    ON EVERY ROW
      SKIP 1 LINES

      PRINT
      PRINT COLUMN 014, rdet.cod_operacion      USING "&&",
            COLUMN 021, rdet.desc_cod_op        CLIPPED, 
            COLUMN 062, rdet.desc_tip_reg       CLIPPED,
            COLUMN 104, rdet.tot_cod_op         USING "-#,###,##&"
  

      PRINT COLUMN 10,"\304",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L5,L1,L1
          
    ON LAST ROW
      SKIP 3 LINES
  
      PRINT COLUMN 10,"\332",L10,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\302",L10,L10,L10,L1,L1,
                      "\302",L10,L1,L1,L1,L1,
                      "\277"
  
      PRINT COLUMN 10,"|         |",
                      "                                        |",
                      "       TOTALES:                         |",
            COLUMN 104,SUM(rdet.tot_cod_op) USING "-#,###,###",
                      "   |"
      SKIP 1 LINES

      PRINT COLUMN 10,"\300",L10,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\301",L10,L10,L10,L1,L1,
                      "\301",L10,L1,L1,L1,L1,
                      "\331"

END REPORT
#########################################################################
FUNCTION conforma_registro()

  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_estadistico
    CREATE TEMP TABLE tmp_estadistico (tipo_registro SMALLINT,
                                       cod_operacion CHAR(02),
                                       total INTEGER) WITH NO LOG ; 
  WHENEVER ERROR STOP
  
  -----REGISTRADAS PRIMERA VEZ------ 
  INSERT INTO tmp_estadistico 
  SELECT c.tipo_registro, b.cod_operacion, COUNT(*)
    FROM afi_mae_afiliado a, afi_ctr_solicitud b, cta_afi_cod_op c
   WHERE a.n_seguro       = b.n_seguro
     AND a.n_folio        = b.n_folio
     AND a.tipo_solicitud = b.tipo_solicitud
     AND a.finicta BETWEEN fecha_ini AND fecha_corte
     AND a.fentcons       = b.fentcons
     AND b.cod_operacion <> "1"
     AND c.cod_operacion  = b.cod_operacion
     AND c.tipo_registro  = 1
   GROUP BY 1,2;
  -----REGISTRADAS PRIMERA VEZ------ 
  
  -----REGISTRO POR UNIFICACION----- 
  INSERT INTO tmp_estadistico 
  SELECT "2", "00", COUNT(*)
    FROM uni_det_certifica
   WHERE fecha_certifica >= fecha_ini   
     AND fecha_certifica <= fecha_corte
     AND estado = 100
   GROUP BY 1,2
  -----REGISTRO POR UNIFICACION----- 
  
  ----------UNIFICACIONES-----------
  INSERT INTO tmp_estadistico 
  SELECT "3", "00", COUNT(b.nss_cta1)
    FROM safre_af:uni_unificado b
   WHERE b.fnotifica >= fecha_ini 
     AND b.fnotifica <= fecha_corte
     AND b.estado    IN(100,60)
   GROUP BY 1,2
  ----------UNIFICACIONES-----------
  
  --CUENTAS SALDO CERO REGISTRADAS--
  INSERT INTO tmp_estadistico 
  SELECT "4", "99", COUNT(*)
    FROM cta_his_marca a
   WHERE a.marca_cod  = 150
     AND a.fecha_ini >= fecha_ini
     AND a.fecha_ini <= fecha_corte
   GROUP BY 1,2
  --CUENTAS SALDO CERO REGISTRADAS--
  
  ---------DIFERENTE AFORE----------
  ---- TAA CEDENTE
  INSERT INTO tmp_estadistico 
  SELECT "5", "00", COUNT(*)
    FROM safre_af:taa_cd_det_cedido b
   WHERE b.fecha_trasp   >= fecha_ini 
     AND b.fecha_trasp   <= fecha_corte
     AND b.estado        IN (103,12)
     AND b.tipo_traspaso IN ('01','12','21','24','38','55');
      
  ---- TAA RECEPTORA
  INSERT INTO tmp_estadistico 
  SELECT "5", "05", COUNT(*)
    FROM taa_viv_recepcion
   WHERE fecha_mov_banxico >= fecha_ini  
     AND fecha_mov_banxico <= fecha_corte
     AND ident_operacion    = '09'
  ---------DIFERENTE AFORE----------
  
  -----ASIGNACIONES MISMA AFORE-----
  INSERT INTO tmp_estadistico 
  SELECT c.tipo_registro, b.cod_operacion, COUNT(*)
    FROM afi_mae_afiliado a, afi_ctr_solicitud b, cta_afi_cod_op c
   WHERE a.n_seguro       = b.n_seguro
     AND a.n_folio        = b.n_folio
     AND a.tipo_solicitud = b.tipo_solicitud
     AND a.finicta BETWEEN fecha_ini AND fecha_corte
     AND a.fentcons       = b.fentcons
     AND b.cod_operacion <> "1"
     AND c.cod_operacion  = b.cod_operacion
     AND c.tipo_registro  = 6
   GROUP BY 1,2;
   
  ---- REGISTRO POR UNIFICACION(60-32)
  INSERT INTO tmp_estadistico
  SELECT "6", "32", COUNT(*)
    FROM uni_det_asignado
   WHERE cve_afo_ced     IN (SELECT codigo_afore
                               FROM tab_afore_local)
     AND fecha_certifica >= fecha_ini
     AND fecha_certifica <= fecha_corte
     AND estado           = 100
  GROUP BY 1,2;
  ---- REGISTRO POR UNIFICACION(60-32)  
  -----ASIGNACIONES MISMA AFORE-----

END FUNCTION
