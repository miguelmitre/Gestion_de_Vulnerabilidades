#############################################################################
#Proyecto          => AFORE(MEXICO)                                         #
#Propietario       => E.F.P                                                 #
#Programa CTAR002  => REVERSO DE ESTADOS DE CUENTA FINALES X FOLIO          #
#Sistema           => CTA                                                   #
#Autor             => MIGUEL ANGEL HERNANDEZ MARTINEZ                       #
#Fecha             => 19 DE JULIO 2006                                      #
#############################################################################
DATABASE safre_af
GLOBALS

   DEFINE seg_modulo  RECORD LIKE seg_modulo.*

   DEFINE opc                   CHAR(01),
          g_usuario             CHAR(08),
          HOY                   DATE    ,
          gs_afore              SMALLINT

   DEFINE total_registros       SMALLINT

   DEFINE gr_captura RECORD
   	      tipo_informe SMALLINT,
   	      fecha_ini    DATE    ,
   	      fecha_fin    DATE
   END RECORD

END GLOBALS
#############################################################################
MAIN

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   SELECT *,
          USER
   INTO   seg_modulo.*,
          g_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   SELECT codigo_afore
   INTO   gs_afore
   FROM   safre_af:tab_afore_local

   LET HOY = TODAY

   CALL STARTLOG("CTAR002.log")
   CALL inicio()

END MAIN
#############################################################################
FUNCTION inicio()

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "CTAR0021" ATTRIBUTE(BORDER)
   DISPLAY " CTAR001            REVERSO DE ESTADOS DE CUENTA FINALES                       " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY "<Esc> Consulta" AT 4,1

   MENU "REVERSO"
      COMMAND "Finales" "Reverso de estado de cuenta finales"
         CALL proceso()

      COMMAND "Pensionados" "Reverso de estado de cuenta de pensionados"
         CALL fn_pensionados()

      COMMAND "Salida" "Salida del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1

END FUNCTION
#############################################################################
FUNCTION proceso()

   DEFINE vtipo_edocta    SMALLINT,
          vfolio          INTEGER

   INITIALIZE vtipo_edocta TO NULL
   INITIALIZE vfolio   TO NULL

   INPUT BY NAME vtipo_edocta,
                 vfolio WITHOUT DEFAULTS

      AFTER FIELD vtipo_edocta
         IF vtipo_edocta IS NULL OR vtipo_edocta = " " THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vtipo_edocta
         END IF

         SELECT "X"
         FROM   tab_tipo_informe
         WHERE  tipo_informe = vtipo_edocta
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "EL TIPO DE INFORME NO EXISTE ..."
            NEXT FIELD vtipo_edocta
         END IF

      AFTER FIELD vfolio
         IF vfolio IS NULL OR vfolio = " " THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vfolio
         END IF

         SELECT "X"
         FROM   cta_ctr_proceso
         WHERE  tipo_informe = vtipo_edocta
         AND    folio  = vfolio
         AND    estado       >= 3
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO SE ENCONTRARON REGISTROS PARA REVERSAR ...1"
            NEXT FIELD vtipo_edocta
         END IF

      ON KEY(ESC)
         IF vtipo_edocta IS NULL OR vtipo_edocta = " " THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vtipo_edocta
         END IF

         IF vfolio IS NULL OR vfolio = " " THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vfolio
         END IF

         SELECT "X"
         FROM   cta_ctr_proceso
         WHERE  tipo_informe = vtipo_edocta
         AND    folio  = vfolio
         AND    estado       >= 3
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO SE ENCONTRARON REGISTROS PARA REVERSAR ...2"
            NEXT FIELD vtipo_edocta
         END IF

         ERROR "PROCESANDO INFORMACION ..."

         CALL primer_paso(vtipo_edocta,
                    vfolio)
         EXIT INPUT

      ON KEY(INTERRUPT)
         ERROR "PROCESO CANCELADO ..."
         CLEAR FORM
         EXIT INPUT
   END INPUT

END FUNCTION
#############################################################################
FUNCTION primer_paso(vtipo_edocta,vfolio)

   DEFINE i     SMALLINT

   DEFINE vtipo_edocta   SMALLINT,
          vfolio     INTEGER

   DEFINE l_reg_01 ARRAY[500] OF RECORD
      cuantos            SMALLINT,
      fecha_fin          DATE
   END RECORD

   DECLARE cursor_01 CURSOR FOR
   SELECT count(*),
          fecha_fin
   FROM   cta_ctr_proceso
   WHERE  folio   = vfolio
   AND    estado       >= 3
   --AND    fecha_fin BETWEEN '01/01/2005' AND '03/31/2005'
   AND    tipo_informe = vtipo_edocta
   GROUP BY 2

   LET i = 1
   LET total_registros = 0

   FOREACH cursor_01 INTO l_reg_01[i].*
      LET total_registros = total_registros + l_reg_01[i].cuantos
      LET i = i + 1
   END FOREACH

   LET i = i - 1

   IF i >= 1 THEN
      CALL SET_COUNT(i)
      DISPLAY "           REGISTROS ENCONTRADOS      FECHA DE LIQUIDACION                     " AT 12,1 ATTRIBUTE(REVERSE)
      DISPLAY "                                                                               " AT 10,1 ATTRIBUTE(REVERSE)
      DISPLAY "<Enter> Por fecha liquidacion                           <ESC> Todo el folio    " AT 11,1

      DISPLAY ARRAY l_reg_01 TO scr_01.*

         ON KEY(CONTROL-M)
            LET i = ARR_CURR()
            DISPLAY "Total de Registros : ",l_reg_01[i].cuantos AT 19,1

            PROMPT "Desea continuar con el proceso de Reverso S/N ?: " FOR opc

            IF opc MATCHES "[SsNn]" THEN
               IF opc = "N" OR opc = "n" THEN
                  INITIALIZE l_reg_01 TO NULL
                  INITIALIZE total_registros TO NULL
                  DISPLAY "                                   " AT 19,1
                  DISPLAY "                                                                               " AT 12,1
                  DISPLAY "                                                                               " AT 10,1
                  DISPLAY "                                                                               " AT 11,1
                  CLEAR FORM
                  ERROR "PROCESO CANCELADO ..."
               ELSE
                  CALL reverso_finales(vtipo_edocta,
                                       vfolio,
                                       l_reg_01[i].fecha_fin,1)
               END IF
               CLEAR FORM
               DISPLAY "                                                                               " AT 12,1
               DISPLAY "                                   " AT 19,1
               DISPLAY "                                                                               " AT 10,1
               DISPLAY "                                                                               " AT 11,1
            END IF
            EXIT DISPLAY

         ON KEY(ESC)
            DISPLAY "Total de Registros : ",total_registros AT 19,1

            PROMPT "Desea continuar con el proceso de Reverso S/N ?: " FOR opc

            IF opc MATCHES "[SsNn]" THEN
               IF opc = "N" OR opc = "n" THEN
                  INITIALIZE l_reg_01 TO NULL
                  INITIALIZE total_registros TO NULL
                  DISPLAY "                                   " AT 19,1
                  DISPLAY "                                                                               " AT 12,1
                  DISPLAY "                                                                               " AT 10,1
                  DISPLAY "                                                                               " AT 11,1
                  CLEAR FORM
                  ERROR "PROCESO CANCELADO ..."
               ELSE
                  CALL reverso_finales(vtipo_edocta,
                                       vfolio,l_reg_01[i].fecha_fin,1)
               END IF
               CLEAR FORM
               DISPLAY "                                                                               " AT 12,1
               DISPLAY "                                   " AT 19,1
               DISPLAY "                                                                               " AT 10,1
               DISPLAY "                                                                               " AT 11,1
            END IF
            EXIT DISPLAY
         ON KEY(INTERRUPT)
            ERROR "REVERSO CANCELADO ..."
            INITIALIZE l_reg_01 TO NULL
            DISPLAY "                                                                               " AT 12,1
            DISPLAY "                 " AT 11,1
            CLEAR FORM
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "PROCESO CANCELADO ..."
      CLEAR FORM
   END IF

END FUNCTION
#############################################################################
FUNCTION reverso_finales(vtipo_edocta,vfolio,vfecha_fin,sw_accion)

   DEFINE i     SMALLINT

   DEFINE vtipo_edocta   SMALLINT,
          vfolio     INTEGER,
          vfecha_fin    DATE,
          sw_accion     SMALLINT,
          sel_txt       CHAR(350),
          sel_txt1      CHAR(200)

   DEFINE l_reg_02 ARRAY[500] OF RECORD
      folio              INTEGER,
      fecha_fin          DATE
   END RECORD

   DEFINE x_estado      INTEGER,
          opc           CHAR(1)

   LET x_estado = 0

   IF sw_accion = 0 THEN
      LET sel_txt1 = " AND fecha_fin = ",vfecha_fin,
                     " AND estado  >= 3 " CLIPPED
   ELSE
      LET sel_txt1 = " AND estado  >= 3 " CLIPPED,
                     " GROUP BY 1,2 "
   END IF

   LET sel_txt = "SELECT folio, ",
                        "fecha_fin ",
                 " FROM   cta_ctr_proceso ",
                 " WHERE  folio   =  ",vfolio,
                 " AND    tipo_informe =  ",vtipo_edocta

   LET sel_txt = sel_txt CLIPPED,sel_txt1

   PREPARE eje_sel_1  FROM  sel_txt

   DECLARE cursor_02 CURSOR FOR eje_sel_1

   LET i = 1

   ERROR "PROCESANDO INFORMACION ..."

   FOREACH cursor_02 INTO l_reg_02[i].*
      UPDATE cta_ctr_cuenta
      SET (fecha_informe,
           tipo_informe) = ((SELECT a.fecha_fin,
                                    a.tipo_informe
                             FROM   cta_ctr_proceso a
                             WHERE  a.folio        = l_reg_02[i].folio
                             AND  a.tipo_edo     = 2
                             AND    a.estado       >= 3
                             AND    a.fecha_fin  = l_reg_02[i].fecha_fin
                             AND    a.tipo_informe = vtipo_edocta
                             AND    a.nss          = cta_ctr_cuenta.nss))
      WHERE nss IN(SELECT nss
                   FROM   cta_ctr_proceso
                   WHERE  folio        = l_reg_02[i].folio
                   AND    tipo_edo     = 2
                   AND    estado       >= 3
                   AND    fecha_fin  = l_reg_02[i].fecha_fin
                   AND    tipo_informe = vtipo_edocta)

      LET i = i + 1
   END FOREACH

   SELECT UNIQUE estado
   INTO   x_estado
   FROM   cta_ctr_proceso
   WHERE  folio        = vfolio
   AND    tipo_informe = vtipo_edocta

   LET x_estado = x_estado + 1

   UPDATE cta_ctr_proceso
   SET    estado       = x_estado
   WHERE  folio        = vfolio
   AND    estado       >= 3
   AND    tipo_informe = vtipo_edocta

   ERROR "PROCESO TERMINADO ..."

END FUNCTION
#############################################################################
FUNCTION fn_pensionados()
   DEFINE ls_proceso    SMALLINT
   DEFINE li_procesados INTEGER
   DEFINE lc_msg        CHAR(100)
   DEFINE lc_enter      CHAR(01)

   OPEN WINDOW ventana2 AT 2,2 WITH FORM "CTAR0022" ATTRIBUTE(BORDER)
   DISPLAY " CTAR001            REVERSO DE ESTADOS DE CUENTA PENSIONADOS                   " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY "<Esc> Aceptar                                              <CTRL-C> Cancelar" AT 4,1

   CALL fn_crea_tablas()
   LET ls_proceso = fn_captura()

   IF ls_proceso = 1 THEN
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana2
      RETURN
   END IF

   LET ls_proceso = fn_confirmacion()
   IF ls_proceso = 1 THEN
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana2
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION..."
   LET li_procesados = fn_reverso()
   ERROR ""

   LET ls_proceso = fn_verfica_reverso()
   IF ls_proceso = 1 THEN
      LET lc_msg = "ERROR AL APLICAR EL REVERSO. NOTIFIQUE A SISTEMAS"
   ELSE
   	  LET lc_msg = "REVERSO APLICADO CORRECTAMENTE. REG REVERSADOS:", li_procesados USING "############&"
   END IF

   PROMPT lc_msg CLIPPED FOR CHAR lc_enter

   CLOSE WINDOW ventana2
END FUNCTION
#############################################################################
FUNCTION fn_crea_tablas()
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_tab_informe
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_tab_informe(tipo_informe SMALLINT)

   CASE gs_afore
      WHEN 564 --MLM
         INSERT INTO tmp_tab_informe VALUES(14) --TRANSFERENCIAS REGIMEN 73
         INSERT INTO tmp_tab_informe VALUES(15) --TRANSFERENCIAS REGIMEN 97
         INSERT INTO tmp_tab_informe VALUES(16) --DISPOSICIONES REGIMEN 73
         INSERT INTO tmp_tab_informe VALUES(17) --DISPOSICIONES REGIMEN 97
      WHEN 568 --CPL
         INSERT INTO tmp_tab_informe VALUES(14) --TRANSFERENCIAS REGIMEN 73
         INSERT INTO tmp_tab_informe VALUES(15) --TRANSFERENCIAS REGIMEN 97
         INSERT INTO tmp_tab_informe VALUES(16) --DISPOSICIONES REGIMEN 73
         INSERT INTO tmp_tab_informe VALUES(17) --DISPOSICIONES REGIMEN 97
   END CASE
END FUNCTION
#############################################################################
FUNCTION fn_captura()
   DEFINE ls_flag    SMALLINT

   LET ls_flag = 1
   INITIALIZE gr_captura.* TO NULL


   INPUT BY NAME gr_captura.* WITHOUT DEFAULTS
   	  BEFORE FIELD tipo_informe
         CASE gs_afore
            WHEN 564 --MLM
               DISPLAY "Tipo Inf: <14>TRANS.Reg73 <15>TRANS.Reg97 <16>DISP.Reg73 <17>DISP.Reg97     " AT 20,1

            WHEN 568 --CPL
            	 DISPLAY "Tipo Inf: <14>TRANS.Reg73 <15>TRANS.Reg97 <16>DISP.Reg73 <17>DISP.Reg97     " AT 20,1

            WHEN 578 --PST
            	 DISPLAY " Tipo Inf: <14>TRANS. IMSS Reg73 <15>TRANS. IMSS Reg97                      " AT 17,1
               DISPLAY " <16>TRANS. ISSSTE CTA INDIV. <17>TRANS. ISSSTE DECIMO TRANS                " AT 18,1
               DISPLAY " <18>DISP. IMSS Reg73  <19>DISP. IMSS Reg97                                 " AT 19,1
               DISPLAY " <20>DISP. ISSSTE CTA INDIV.  <21>DISP. ISSSTE DECIMO TRANS                 " AT 20,1

            OTHERWISE
               DISPLAY " Tipo Inf: <14>TRANS.Reg73 <15>TRANS.Reg97                                  " AT 20,1
         END CASE

   	  AFTER FIELD tipo_informe
   	  	 IF gr_captura.tipo_informe IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR EL TIPO DE INFORME A REVERSAR"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD tipo_informe
   	  	 ELSE
         	  #Verifica informe valido
         	  SELECT "X"
         	  FROM   tmp_tab_informe
         	  WHERE  tipo_informe = gr_captura.tipo_informe
         	  GROUP BY 1

         	  IF SQLCA.SQLCODE <> 0 THEN
         	     ERROR "DEBE INDICAR UN TIPO DE INFORME VALIDO"
   	  	 	     SLEEP 2
   	  	 	     ERROR ""
   	  	 	     NEXT FIELD tipo_informe
         	  END IF
         END IF

         CASE gs_afore
            WHEN 564 --MLM
               DISPLAY "                                                                            " AT 20,1

            WHEN 568 --CPL
            	 DISPLAY "                                                                            " AT 20,1

            WHEN 578 --PST
            	 DISPLAY "                                                                            " AT 17,1
               DISPLAY "                                                                            " AT 18,1
               DISPLAY "                                                                            " AT 19,1
               DISPLAY "                                                                            " AT 20,1

            OTHERWISE
               DISPLAY "                                                                            " AT 20,1
         END CASE

      AFTER FIELD fecha_ini
   	  	 IF gr_captura.fecha_ini IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR LA FECHA DE INICIO"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD fecha_ini
         END IF

      AFTER FIELD fecha_fin
   	  	 IF gr_captura.fecha_fin IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR LA FECHA FIN"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD fecha_fin
         END IF

      ON KEY (esc)
      	 IF gr_captura.tipo_informe IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR EL TIPO DE INFORME A REVERSAR"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD tipo_informe
   	  	 ELSE
         	  #Verifica informe valido
         	  SELECT "X"
         	  FROM   tmp_tab_informe
         	  WHERE  tipo_informe = gr_captura.tipo_informe
         	  GROUP BY 1

         	  IF SQLCA.SQLCODE <> 0 THEN
         	     ERROR "DEBE INDICAR UN TIPO DE INFORME VALIDO"
   	  	 	     SLEEP 2
   	  	 	     ERROR ""
   	  	 	     NEXT FIELD tipo_informe
   	  	 	  ELSE
   	  	 	  	 CASE gs_afore
                  WHEN 564 --MLM
                     DISPLAY "                                                                            " AT 20,1

                  WHEN 568 --CPL
                  	 DISPLAY "                                                                            " AT 20,1

                  WHEN 578 --PST
                  	 DISPLAY "                                                                            " AT 17,1
                     DISPLAY "                                                                            " AT 18,1
                     DISPLAY "                                                                            " AT 19,1
                     DISPLAY "                                                                            " AT 20,1

                  OTHERWISE
                     DISPLAY "                                                                            " AT 20,1
               END CASE
         	  END IF
         END IF



         IF gr_captura.fecha_ini IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR LA FECHA DE INICIO"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD fecha_ini
         END IF

         IF gr_captura.fecha_fin IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR LA FECHA FIN"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD fecha_fin
         END IF

         #Validar que exista información por reversar
         SELECT "X"
         FROM   cta_ctr_proceso
         WHERE  tipo_informe = gr_captura.tipo_informe
         AND    fecha_fin BETWEEN gr_captura.fecha_ini AND gr_captura.fecha_fin
         AND    estado       <> 1
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO EXISTE INFORMACION POR REVERSAR CON LOS CRITERIOS INDICADOS"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD tipo_informe
         END IF

      	 LET ls_flag = 0
         EXIT INPUT

      ON KEY (CONTROL-C, INTERRUPT)
         LET ls_flag = 1
         EXIT INPUT
   END INPUT

   RETURN ls_flag
END FUNCTION
#############################################################################
FUNCTION fn_confirmacion()
   DEFINE ls_flag  SMALLINT
   DEFINE lc_msg   CHAR(100)
   DEFINE lc_enter CHAR(01)

   LET ls_flag = 1
   LET lc_msg  = " DESEA EJECUTAR EL PROCESO S/N? "

   WHILE TRUE
      PROMPT lc_msg CLIPPED FOR lc_enter

      IF lc_enter MATCHES "[sSnN]" THEN
         IF lc_enter MATCHES "[sS]" THEN
            LET ls_flag = 0
            EXIT WHILE
         ELSE
            LET ls_flag = 1
            EXIT WHILE
         END IF
      ELSE
         ERROR "SOLO INDIQUE S o N "
         SLEEP 2
         ERROR ""
         CONTINUE WHILE
      END IF
   END WHILE

   RETURN ls_flag
END FUNCTION
################################################################################
FUNCTION fn_reverso()
   DEFINE li_procesados INTEGER

   sql
   UPDATE cta_ctr_proceso
   SET    estado = 1 ,
          folio  = NULL
   WHERE tipo_informe = $gr_captura.tipo_informe
   AND    fecha_fin BETWEEN $gr_captura.fecha_ini AND $gr_captura.fecha_fin
   END sql

   LET li_procesados = SQLCA.SQLERRD[3]

   RETURN li_procesados
END FUNCTION
################################################################################
FUNCTION fn_verfica_reverso()
   DEFINE ls_flag SMALLINT

   LET ls_flag = 1

   SELECT "X"
   FROM   cta_ctr_proceso
   WHERE  tipo_informe = gr_captura.tipo_informe
   AND    fecha_fin BETWEEN gr_captura.fecha_ini AND gr_captura.fecha_fin
   AND    estado       <> 1
   GROUP BY 1

   #No debería existir información pendiente de reversar
   IF SQLCA.SQLCODE <> 0 THEN
      LET ls_flag = 0
   END IF

   RETURN ls_flag
END FUNCTION
################################################################################