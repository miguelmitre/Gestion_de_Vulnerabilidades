#********************************************************************#
#Proyecto     => safre_af                                            #
#Propietario  => E.F.P.                                              #
#Programa     => CTAL007                                             #
#Descripcion  => REPORTE DE CUENTAS REHABILITADAS cta_rehabilitada   #
#Por          => OMAR SANDOVAL BADILLO                               #
#Fecha        => 13 de octubre 2005.                                 #
#modificaicon => 12 de diciembre 2005.                               #
#Sistema      => CTA.                                                #
#********************************************************************#
DATABASE  safre_af

GLOBALS
   DEFINE g_parametro RECORD LIKE seg_modulo.*

   DEFINE hoy                DATE,
          usuario            CHAR(08),
          opc                CHAR(01),
          ejecuta            CHAR(200),
          hora_inicial       CHAR(08),
          hora_final         CHAR(08),
          g_impre            CHAR(200),
          g_lista            CHAR(200),
          cla_where          CHAR(200),
          sel_where          CHAR(200)

   DEFINE reg_01 ARRAY[8000] OF RECORD
          nss                CHAR(11)
   END RECORD

   DEFINE reg_02 ARRAY[1000] OF RECORD
          v_retiro           DECIMAL(10,6),
          v_cv               DECIMAL(10,6),
          v_vol              DECIMAL(10,6),
          v_viv97            DECIMAL(10,6),
          v_cs               DECIMAL(10,6),
          v_sar              DECIMAL(10,6),
          v_viv92            DECIMAL(10,6)
   END RECORD

   DEFINE  fecha_rehabilita_ini    DATE,
           fecha_rehabilita_fin    DATE,
           x_fecha_genera          DATE

   DEFINE respuesta                CHAR(1)

END GLOBALS
#####################################################################
MAIN
   DEFER INTERRUPT
   OPTIONS
       PROMPT LINE LAST,
       INPUT WRAP,
       ACCEPT KEY CONTROL-O

   CALL STARTLOG("CTAL007.log")

   SELECT *,
          USER
   INTO   g_parametro.*,
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   LET hoy = TODAY

   CALL inicializa()

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "CTAL0071" ATTRIBUTE(BORDER)
   DISPLAY " CTAL007                   REPORTE DE CUENTAS REHABILITADAS                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

   MENU "REPORTE"
      COMMAND "Consulta" "Reporte de cuentas rehabilitadas"
         CALL proceso()
      COMMAND "Salida" "Salida del programa"
         EXIT MENU
   END MENU

   #CLOSE WINDOW ventana1
END MAIN
#####################################################################
FUNCTION inicializa()
   INITIALIZE fecha_rehabilita_ini TO null
   INITIALIZE fecha_rehabilita_fin TO null
   INITIALIZE reg_01 TO NULL
   INITIALIZE reg_02 TO NULL
END FUNCTION
#####################################################################
FUNCTION proceso()

   DEFINE  i                  SMALLINT

   DEFINE sql_stat     INTEGER,
          item_row_cnt SMALLINT,
          row_cnt      SMALLINT,
          cont_inp     SMALLINT,
          cur_row      SMALLINT,
          scr_row      SMALLINT

   LET fecha_rehabilita_ini = hoy
   LET fecha_rehabilita_fin = hoy
   DISPLAY BY NAME fecha_rehabilita_ini
   DISPLAY BY NAME fecha_rehabilita_fin

   INPUT BY NAME fecha_rehabilita_ini,
                 fecha_rehabilita_fin WITHOUT DEFAULTS

      AFTER FIELD fecha_rehabilita_ini
         IF fecha_rehabilita_ini > hoy THEN
            ERROR "LA FECHA NO PUEDE SER MAYOR AL HOY ..."
            NEXT FIELD fecha_rehabilita_ini
         END IF

      AFTER FIELD fecha_rehabilita_fin
         IF fecha_rehabilita_fin IS NULL THEN
            ERROR "LA FECHA FIN NO PUEDE SER NULA ..."
            NEXT FIELD fecha_rehabilita_fin
         END IF

         IF fecha_rehabilita_ini > fecha_rehabilita_fin THEN
            ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FINAL ..."
            NEXT FIELD fecha_rehabilita_ini
         END IF

         IF fecha_rehabilita_fin > hoy THEN
            ERROR "LA FECHA NO PUEDE SER MAYOR AL HOY ..."
            NEXT FIELD fecha_rehabilita_fin
         END IF

      ON KEY(ESC)
         LET INT_FLAG = FALSE

         IF fecha_rehabilita_fin IS NULL THEN
            ERROR "LA FECHA INICIAL NO PUEDE SER NULA ..."
            NEXT FIELD fecha_rehabilita_fin
         END IF

         IF fecha_rehabilita_ini > hoy THEN
            ERROR "LA FECHA NO PUEDE SER MAYOR AL HOY ..."
            NEXT FIELD fecha_rehabilita_ini
         END IF

         IF fecha_rehabilita_ini > fecha_rehabilita_fin THEN
            ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FINAL ..."
            NEXT FIELD fecha_rehabilita_ini
         END IF

         IF fecha_rehabilita_fin > hoy THEN
            ERROR "LA FECHA NO PUEDE SER MAYOR AL HOY ..."
            NEXT FIELD fecha_rehabilita_fin
         END IF
         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         CLEAR FORM
         EXIT INPUT

   END INPUT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      ERROR "PROCESO CANCELADO "
      CALL inicializa()
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION ..."

   IF fecha_rehabilita_ini IS NULL THEN
      LET sel_where = " SELECT nss ",
                   " FROM   cta_rehabilitada",
                   " WHERE fecha_rehabilita <= ","'",fecha_rehabilita_fin,"'",
                   " GROUP BY 1",
                   " ORDER BY 1"
   ELSE
      LET sel_where = " SELECT nss ",
                   " FROM   cta_rehabilitada",
                   " WHERE fecha_rehabilita BETWEEN ",
                                                   "'",fecha_rehabilita_ini,"'",
                                           " AND ","'",fecha_rehabilita_fin,"'",
                   " GROUP BY 1",
                   " ORDER BY 1"
   END IF

   PREPARE query_01 FROM sel_where
   DECLARE cur_01 CURSOR FOR query_01 

   DISPLAY "<Ctrl-F> Archivo  <Ctrl-P> Impresion" AT 2,40
   DISPLAY "    NSS    " AT 7,4 ATTRIBUTE(REVERSE)
   DISPLAY "     RETIRO        CV        VOL       VIV97      CS        SAR      VIV92     " AT 14,1 ATTRIBUTE(REVERSE)

   LET i = 1
   FOREACH cur_01 INTO reg_01[i].*
      LET i = i + 1
   END FOREACH

   LET i = i - 1

   IF i >= 1 THEN
      CALL SET_COUNT(i)
      LET cont_inp = TRUE  

      WHILE (cont_inp = TRUE)
         INPUT ARRAY reg_01 WITHOUT DEFAULTS FROM scr_nss.*
         ATTRIBUTES(MAXCOUNT=i,COUNT=i)

            AFTER ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               DISPLAY reg_01[cur_row].* TO scr_nss[scr_row].*

            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               IF (cur_row = i + 1) THEN
                  LET cont_inp = TRUE
               ELSE
                  LET scr_row = SCR_LINE()

                  DISPLAY reg_01[cur_row].* TO scr_nss[scr_row].*
                  ATTRIBUTE(REVERSE)

                  LET cont_inp = FALSE

                  CALL proc_items(cur_row)
                       RETURNING sql_stat,
                                 item_row_cnt

            END IF

            ON KEY(CONTROL-P)
               PROMPT "ESTA SEGURO DE GENERAR EL REPORTE ? S/N :" 
                  FOR respuesta

                  IF respuesta MATCHES "[SsNn]" THEN
                     IF respuesta MATCHES "[Nn]" THEN
                        ERROR "GENERACION DE REPORTE CANCELADO ..."
                        CLEAR FORM
                        RETURN
                     ELSE
                        ERROR "PROCESANDO IMPRESION ..."
                        CALL imprime()
                     END IF
                  END IF

            ON KEY(CONTROL-F)
               PROMPT "ESTA SEGURO DE GENERAR EL ARCHIVO ? S/N :" 
                  FOR respuesta

                  IF respuesta MATCHES "[SsNn]" THEN
                     IF respuesta MATCHES "[Nn]" THEN
                        ERROR "GENERACION DE ARCHIVO CANCELADO ..."
                        CLEAR FORM
                        RETURN
                     ELSE
                        ERROR "GENERANDO ARCHIVO ..."
                        CALL archivo()
                     END IF
                  END IF

            ON KEY (INTERRUPT)
               DISPLAY "           " AT 7,4 
               DISPLAY "                                                                               " AT 14,1 
               CALL null_items()
               CALL inicializa()
               CLEAR FORM
               EXIT INPUT
         END INPUT
      END WHILE
   ELSE
      DISPLAY "           " AT 7,4 
      DISPLAY "                                                                               " AT 14,1 
      ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR ..."
      CLEAR FORM
      RETURN
   END IF
 
END FUNCTION
#####################################################################
FUNCTION proc_items(p_cur_row)

   DEFINE sql_status INTEGER,
          p_cur_row SMALLINT,
          item_row_cnt SMALLINT

   CALL sel_ord_item(p_cur_row)
        RETURNING sql_status,
                 item_row_cnt

   CALL disp_four_items()

   RETURN sql_status,
         item_row_cnt
END FUNCTION
#####################################################################
FUNCTION sel_ord_item(p_cur_row)

   DEFINE sql_status  INTEGER,
          p_cur_row   SMALLINT,
          row_cnt     SMALLINT,
          sql_txt     CHAR(250)

   CALL null_items()

   LET sql_txt =  " SELECT monto_retiro,",
                          "monto_cesantia,",
                          "monto_voluntaria,",
                          "monto_vivienda97,",
                          "monto_cuota_soc,",
                          "monto_sar,",
                          "monto_vivienda92",
                  " FROM   cta_rehabilitada ",
                  " WHERE  nss = ? ",
                  " GROUP BY 1,2,3,4,5,6,7 "

   PREPARE sel_item_stmt FROM sql_txt
   DECLARE sel_item_curs CURSOR FOR sel_item_stmt

   WHENEVER ERROR CONTINUE
      OPEN sel_item_curs USING reg_01[p_cur_row].nss
   WHENEVER ERROR STOP

   LET sql_status = SQLCA.SQLCODE
   LET row_cnt = 1

   WHILE ((NOT sql_status) AND (row_cnt<= 10))
      WHENEVER ERROR CONTINUE
         FETCH sel_item_curs INTO reg_02[row_cnt].*
      WHENEVER ERROR STOP
         LET sql_status = SQLCA.SQLCODE
         IF (NOT sql_status) THEN
            LET row_cnt = row_cnt + 1
         END IF
   END WHILE

   IF (sql_status = 100) THEN
      LET sql_status = 0
   END IF

   RETURN sql_status,
          row_cnt - 1
END FUNCTION
#####################################################
FUNCTION disp_four_items()
   DEFINE i SMALLINT

   FOR i = 1 TO 6
      DISPLAY reg_02[i].* TO scr_montos[i].*
   END FOR

END FUNCTION
#####################################################
FUNCTION null_items()

   DEFINE i SMALLINT
   INITIALIZE reg_02[1].* TO NULL

   FOR i = 1 TO 6
      LET reg_02[i].* = reg_02[1].*
   END FOR
END FUNCTION
###############################################################
FUNCTION imprime()

   DEFINE pos   SMALLINT,
          i     SMALLINT

   DEFINE l_reg   ARRAY[8000] OF RECORD
        #folio                integer,
        nss                  char(11),     
        monto_retiro         decimal(10,6),
        monto_cesantia       decimal(10,6),
        monto_voluntaria     decimal(10,6),
        monto_vivienda97     decimal(10,6),
        monto_cuota_soc      decimal(10,6),
        monto_sar            decimal(10,6),
        monto_vivienda92     decimal(10,6),
        fecha_rehabilita     date,         
     #fecha_inhabilita     date,         
        marca_cod            smallint,        
        fecha_actualiza      date,         
        estado               smallint,   
        usuario              char(8)      
   END RECORD
        
   LET g_impre = g_parametro.ruta_listados CLIPPED,"/",usuario CLIPPED,
                 ".INH",hoy USING "DDMMYYYY"

   START REPORT rpt_rehabilitada TO g_impre

   DECLARE cur_02 CURSOR FOR
   SELECT *
   FROM   cta_rehabilitada
   WHERE  fecha_rehabilita = fecha_rehabilita

   LET i = 1
   FOREACH cur_02 INTO l_reg[i].*
      IF l_reg[i].nss IS NULL THEN
         EXIT FOREACH
      END IF

      OUTPUT TO REPORT rpt_rehabilitada(l_reg[i].*)

      LET i = i + 1
   END FOREACH

   FINISH REPORT rpt_rehabilitada

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
###############################################################
REPORT rpt_rehabilitada(l_reg)

   DEFINE L1            CHAR(01)
   DEFINE L2            CHAR(02)
   DEFINE L3            CHAR(03)
   DEFINE L4            CHAR(04)
   DEFINE L5            CHAR(05)
   DEFINE L10           CHAR(10)

   DEFINE l_reg RECORD
        #folio                INTEGER,
        nss                  CHAR(11),
        monto_retiro         DECIMAL(10,6),
        monto_cesantia       DECIMAL(10,6),
        monto_voluntaria     DECIMAL(10,6),
        monto_vivienda97     DECIMAL(10,6),
        monto_cuota_soc      DECIMAL(10,6),
        monto_sar            DECIMAL(10,6),
        monto_vivienda92     DECIMAL(10,6),
        fecha_rehabilita     DATE,
     #fecha_inhabilita     DATE,
        marca_cod            SMALLINT,
        fecha_actualiza      DATE,
        estado               SMALLINT,
        usuario              CHAR(8)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN 0
      RIGHT MARGIN 0
      PAGE LENGTH 60

   FORMAT
   PAGE HEADER

      LET  L1  = "\304"
      LET  L2  = "\304\304"
      LET  L3  = "\304\304\304"
      LET  L4  = "\304\304\304\304"
      LET  L5  = "\304\304\304\304\304"
      LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
      PRINT COLUMN 1,'\033e\033(s218T\033(s10H\033(s7B'

      PRINT COLUMN 02," CTAL007 ",
            COLUMN 36," REPORTE DE CUENTAS REHABILITADAS ",
            COLUMN 93,hoy USING "DD-MM-YYYY"
      SKIP 3 LINE

      PRINT COLUMN 1,'\033e\033(s218T\033(s14H\033(s7B'
      PRINT COLUMN 04,"FECHA REHABILITACION: ",
                       fecha_rehabilita_ini USING "DD-MM-YYYY"," AL ",
                       fecha_rehabilita_fin USING "DD-MM-YYYY"

      PRINT COLUMN 01,'\033e\033(s218T\033(s17H\033(s7B'
      PRINT COLUMN 04,"\332",L10,L10,"\302",L10,L10,"\302",L10,L10,"\302",L10,L10,"\302",L10,L10,"\302",L10,L10,"\302",L10,L10,"\302",L10,L10,"\277"

      PRINT COLUMN 04,"|         NSS",
            COLUMN 25,"|        RETIRO",
            COLUMN 46,"|         CV",
            COLUMN 67,"|         VOL",
            COLUMN 88,"|        VIV97",
            COLUMN 109,"|         CS",
            COLUMN 130,"|         SAR",
            COLUMN 151,"|        VIV92       |"

      PRINT COLUMN 04,"\300",L10,L10,"\301",L10,L10,"\301",L10,L10,"\301",L10,L10,"\301",L10,L10,"\301",L10,L10,"\301",L10,L10,"\301",L10,L10,"\331"

   ON EVERY ROW
      PRINT COLUMN 09,l_reg.nss,
            COLUMN 28,l_reg.monto_retiro,
            COLUMN 49,l_reg.monto_cesantia,
            COLUMN 70,l_reg.monto_voluntaria,
            COLUMN 92,l_reg.monto_vivienda97,
            COLUMN 112,l_reg.monto_cuota_soc,
            COLUMN 135,l_reg.monto_sar,
            COLUMN 155,l_reg.monto_vivienda92
      SKIP 1 LINE

   PAGE TRAILER
      PRINT COLUMN 80," Pagina : ",PAGENO USING "<<<<<"
      SKIP 2 LINE

   ON LAST ROW
      SKIP 2 LINE
      PRINT COLUMN 04,"_________________________________________________________________________________________________________________________________________________________________________"
      SKIP 1 LINE
      PRINT COLUMN 04,"Total de registros : ",COUNT(*) USING "<<<<<"

END REPORT
###############################################################
FUNCTION archivo()

   DEFINE pos   SMALLINT,
          i     SMALLINT

   DEFINE l_reg   ARRAY[8000] OF RECORD
        nss                  char(11),
        monto_retiro         decimal(10,6),
        monto_cesantia       decimal(10,6),
        monto_voluntaria     decimal(10,6),
        monto_vivienda97     decimal(10,6),
        monto_cuota_soc      decimal(10,6),
        monto_sar            decimal(10,6),
        monto_vivienda92     decimal(10,6),
        fecha_rehabilita     date,
        #fecha_inhabilita     date,
        marca_cod            smallint,
        fecha_actualiza      date,
        estado               smallint,
        usuario              char(8)
   END RECORD

   DEFINE sql_txt_02   CHAR(300)

   LET g_impre = g_parametro.ruta_listados CLIPPED,"/",usuario CLIPPED,
                 "REH-",hoy USING "DDMMYYYY"

   START REPORT arc_rehabilitada TO g_impre

   IF fecha_rehabilita_ini IS NULL THEN
      LET sql_txt_02 = " SELECT * ",
                    " FROM   cta_rehabilitada ",
                    " WHERE  fecha_rehabilita <= ","'",fecha_rehabilita_fin,"'",
                    " ORDER BY 1"
   ELSE
      LET sql_txt_02 = " SELECT * ",
                   " FROM   cta_rehabilitada ",
                   " WHERE  fecha_rehabilita BETWEEN ",
                                                  "'",fecha_rehabilita_ini,"'",
                                          " AND ","'",fecha_rehabilita_fin,"'",
                   " ORDER BY 1"
   END IF

   PREPARE query_02 FROM sql_txt_02
   DECLARE cur_03 CURSOR FOR query_02

   LET i = 1
   FOREACH cur_03 INTO l_reg[i].*
      IF l_reg[i].nss IS NULL THEN
         EXIT FOREACH
      END IF

      OUTPUT TO REPORT arc_rehabilitada(l_reg[i].*,i)

      LET i = i + 1
   END FOREACH

   FINISH REPORT arc_rehabilitada

   PROMPT "EL ARCHIVO SE GENERO EN LA RUTA: ",g_impre FOR opc

   ERROR "ARCHIVO GENERADO..."
   SLEEP 2
   ERROR ""

END FUNCTION
###############################################################
REPORT arc_rehabilitada(l_reg,cuantos)

   DEFINE l_reg RECORD
        nss                  CHAR(11),
        monto_retiro         DECIMAL(9,2),
        monto_cesantia       DECIMAL(9,2),
        monto_voluntaria     DECIMAL(9,2),
        monto_vivienda97     DECIMAL(9,2),
        monto_cuota_soc      DECIMAL(9,2),
        monto_sar            DECIMAL(9,2),
        monto_vivienda92     DECIMAL(9,2),
        fecha_rehabilita     DATE,
        #fecha_inhabilita     DATE,
        marca_cod            SMALLINT,
        fecha_actualiza      DATE,
        estado               SMALLINT,
        usuario              CHAR(8)
   END RECORD

   DEFINE l_reg_3 RECORD
        tipo_registro        CHAR(2),
        consecutivo_envio    SMALLINT,
        nss                  CHAR(11),
        tipo_movimiento      CHAR(3),
        descripcion          CHAR(80),
        fecha_rehabilita     DATE,
        fecha_inhabilita     DATE,
        monto_retiro         DECIMAL(9,2),
        monto_cesantia       DECIMAL(9,2),
        monto_voluntaria     DECIMAL(9,2),
        monto_vivienda97     DECIMAL(9,2),
        monto_cuota_soc      DECIMAL(9,2),
        monto_sar            DECIMAL(9,2),
        monto_vivienda92     DECIMAL(9,2),
        fecha_proceso        DATE,
        estado               SMALLINT,
        usuario              CHAR(8)
   END RECORD

   DEFINE cuantos   SMALLINT

   OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN 0
      RIGHT MARGIN 0
      PAGE LENGTH 1

   FORMAT

   ON EVERY ROW
      SELECT descripcion
      INTO   l_reg_3.descripcion
      FROM   tab_movimiento
      WHERE  codigo = l_reg.marca_cod

      SELECT fecha_ini   --fecha_inhabilita
      INTO   l_reg_3.fecha_inhabilita
      FROM   cta_his_marca
      WHERE  nss       = l_reg.nss
      AND    fecha_fin = l_reg.fecha_rehabilita
      AND    fecha_fin IS NOT NULL
      AND    marca_cod = 140

      LET l_reg_3.tipo_registro    = "33"
      LET l_reg_3.consecutivo_envio= cuantos
      LET l_reg_3.nss              = l_reg.nss
      LET l_reg_3.tipo_movimiento  = l_reg.marca_cod
      LET l_reg_3.fecha_rehabilita = l_reg.fecha_rehabilita
      LET l_reg_3.monto_retiro     = l_reg.monto_retiro
      LET l_reg_3.monto_cesantia   = l_reg.monto_cesantia
      LET l_reg_3.monto_voluntaria = l_reg.monto_voluntaria
      LET l_reg_3.monto_vivienda97 = l_reg.monto_vivienda97
      LET l_reg_3.monto_cuota_soc  = l_reg.monto_cuota_soc
      LET l_reg_3.monto_sar        = l_reg.monto_sar
      LET l_reg_3.monto_vivienda92 = l_reg.monto_vivienda92
      LET l_reg_3.fecha_proceso    = TODAY
      LET l_reg_3.estado           = l_reg.estado
      LET l_reg_3.usuario          = l_reg.usuario

      PRINT l_reg_3.tipo_registro,
            l_reg_3.consecutivo_envio USING "&&&&&",
            l_reg_3.nss,
            l_reg_3.tipo_movimiento  USING "&&&",
            l_reg_3.descripcion,
            l_reg_3.fecha_rehabilita USING "DDMMYYYY",
            l_reg_3.fecha_inhabilita USING "DDMMYYYY",
            l_reg_3.monto_retiro     USING "&&&&&&&&.&&",
            l_reg_3.monto_cesantia   USING "&&&&&&&&.&&",
            l_reg_3.monto_voluntaria USING "&&&&&&&&.&&",
            l_reg_3.monto_vivienda97 USING "&&&&&&&&.&&",
            l_reg_3.monto_cuota_soc  USING "&&&&&&&&.&&",
            l_reg_3.monto_sar        USING "&&&&&&&&.&&",
            l_reg_3.monto_vivienda92 USING "&&&&&&&&.&&",
            l_reg_3.fecha_proceso USING "DDMMYYYY"

END REPORT
#eof
