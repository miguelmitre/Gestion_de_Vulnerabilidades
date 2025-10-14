############################################################################
#Proyecto     => AFORE (MEXICO)                                            #
#Propietario  => E.F.P                                                     #
#Programa     => UNIC037. Consulta de archivo de respuesta por diagnostico #
#Sistema      => UNI                                                       #
#Autor        => Miguel Angel Hernandez Martinez.                          #
#Fecha        => 17 de marzo de 2003.                                      #
#Modificado   => Omar Sandoval Badillo.                                    #
#Fecha act    => 01 de marzo de 2004.                                      #
############################################################################
DATABASE safre_af
GLOBALS

   DEFINE enter                 CHAR(1),
          g_usuario             CHAR(8),
          aux_pausa             CHAR(01),
          hora_tope             CHAR(08),
          HORA                  CHAR(08),
          HOY                   DATE,
          ANIO                  DATE,
          vfolio                INTEGER,
          rechazo               SMALLINT,
          sw_1                  SMALLINT,
          pos                   SMALLINT,
          sel_where             CHAR(500),
          cla_where             CHAR(500),
          x_result_operacion    CHAR(2)

   DEFINE g_afore               RECORD LIKE tab_afore_local.*

   DEFINE l_record ARRAY[10] OF RECORD
          cuantos           SMALLINT,
          result_operacion  CHAR(2)
   END RECORD

   DEFINE l_reg ARRAY[10] OF RECORD
          aceptados         SMALLINT,
          rechazos          SMALLINT,
          rechazos_safre    SMALLINT
   END RECORD

   DEFINE reg ARRAY[5000] OF RECORD
          folio                 INTEGER,
          consecutivo_reg       SMALLINT,
          nss_uni               CHAR(11),
          diagnostico_nss_uni   CHAR(2),
          nss_cta1              CHAR(11),
          diagnostico_nss_cta1  CHAR(2),
	  estado                SMALLINT
   END RECORD

END GLOBALS
##############################################################################
MAIN

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      COMMENT LINE LAST,
      ACCEPT KEY control-i

   DEFER INTERRUPT

   LET HOY = TODAY

   CALL STARTLOG("UNIC037.log")
   CALL proceso_principal()

END MAIN
###############################################################################
FUNCTION proceso_principal()

   DEFINE opc      CHAR(1)

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0371" ATTRIBUTE(BORDER)
   DISPLAY " (Ctrl-C) Salir " AT 2,1
   DISPLAY "                  CONSULTA REGISTROS UNIFICACION OP. 73                        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 2,66 

   LET int_flag = FALSE

   CONSTRUCT cla_where ON folio
                     FROM folio
      ON KEY (control-m)
         ERROR "PROCESANDO INFORMACION..."
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (control-c)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   LET sel_where = " SELECT COUNT(*),",
                           "result_operacion",
                   " FROM   uni_det_solicitud ",
                   " WHERE ",cla_where CLIPPED,
                   " GROUP BY 2 "

   PREPARE query FROM sel_where

   DECLARE cursor_1 CURSOR FOR query

   LET pos = 1
   LET sw_1 = 1
   LET l_record[pos].cuantos = 0

   FOREACH cursor_1 INTO l_record[pos].cuantos,
                         l_record[pos].result_operacion

      CASE l_record[pos].result_operacion
         WHEN "01"
           LET l_reg[sw_1].aceptados      = l_record[pos].cuantos
         WHEN "02"
           LET l_reg[sw_1].rechazos       = l_record[pos].cuantos
         WHEN "03"
           LET l_reg[sw_1].rechazos_safre = l_record[pos].cuantos
      END CASE

      IF l_reg[sw_1].aceptados = 0 AND
         l_reg[sw_1].rechazos = 0  AND
         l_reg[sw_1].rechazos_safre = 0 THEN
         CONTINUE FOREACH
      END IF

      LET pos = pos + 1
   END FOREACH

   ERROR ""

   DISPLAY "              Aceptados             Rechazos             Rechazos " AT 9,1
   DISPLAY "              PROCESAR              PROCESAR              SAFRE " AT 10,1
   DISPLAY " (Ctrl-P) Aceptados    (Ctrl-E) Rechazo PROCESAR     (Ctrl-F) Rechazo SAFRE" AT 20,1

   IF (sw_1) >= 1 THEN
      CALL SET_COUNT(sw_1)

      DISPLAY ARRAY l_reg TO scr_1.*
         ON KEY (CONTROL-P)
            LET x_result_operacion = "01"

            CALL consulta(cla_where,x_result_operacion)

         ON KEY (CONTROL-E)
            LET x_result_operacion = "02"

            CALL consulta(cla_where,x_result_operacion)

         ON KEY (CONTROL-F)
            LET x_result_operacion = "03"

            CALL consulta(cla_where,x_result_operacion)

         ON KEY (INTERRUPT)

            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "REGISTROS ... VACIO"
      SLEEP 2
      ERROR ""
   END IF

   CLEAR SCREEN 
   CLOSE WINDOW ventana_1

END FUNCTION
########################################################################
FUNCTION consulta(cla_where,
                  x_result_operacion)

   DEFINE sel_where             CHAR(500),
          cla_where             CHAR(500),
          x_result_operacion    CHAR(2),
	  x_folio               INTEGER,
	  opc                   CHAR(1)

   OPEN WINDOW ventana_2 AT 2,2 WITH FORM "UNIC0372" ATTRIBUTE(BORDER)

   IF x_result_operacion <> "01" THEN
      DISPLAY " (Ctrl-C) Salir    (Ctrl-G) Reenvio de Rechazo" AT 2,1
   ELSE
      DISPLAY " (Ctrl-C) Salir " AT 2,1
   END IF

   DISPLAY "                           CONSULTA A DETALLE                                  " AT 3,1 ATTRIBUTE(REVERSE)

   LET sel_where = " SELECT a.folio,",
                           "a.consecutivo_reg,",
                           "a.nss_uni,",
                           "a.diagnostico_nss_uni,",
                           "a.nss_cta1,",
                           "a.diagnostico_nss_cta1,",
			   "a.estado ",
                   " FROM   uni_det_solicitud a",
                   " WHERE  ",cla_where CLIPPED,
                   " AND    a.result_operacion = ","'",x_result_operacion CLIPPED,"'",
                   " ORDER BY 1,2,3"

   PREPARE query_consulta FROM sel_where

   DECLARE cursor_2 CURSOR FOR query_consulta

   LET pos = 1

   FOREACH cursor_2 INTO reg[pos].*
      LET x_folio = reg[pos].folio
      LET pos = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY reg TO scr_1.*

         ON KEY (CONTROL-G)
            IF x_result_operacion <> "01" THEN
               CALL reenvio(x_result_operacion,x_folio)
            END IF

         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "REGISTROS ... VACIO"
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana_2
END FUNCTION
########################################################################
FUNCTION reenvio(x_result_operacion,x_folio)

   DEFINE opc                 CHAR(1),
          x_folio             INTEGER,
	  x_result_operacion  CHAR(2),
          sel_where           CHAR(200),
          cla_where           CHAR(200),
	  pos                 SMALLINT,
	  pos1                SMALLINT,
	  cuantos_act         INTEGER

   DEFINE reg1  RECORD
          folio              INTEGER,
          consecutivo_reg    SMALLINT,
          nss_uni            CHAR(11),
          nss_cta1           CHAR(11)
   END RECORD

   OPEN WINDOW ventana_3 AT 3,17 WITH FORM "UNIC0373" ATTRIBUTE(BORDER)
   DISPLAY "     ACTUALIZACION DE DIAGNOSTICO      " AT 1,1 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON nss_uni,
                          diagnostico_nss_uni
                     FROM nss_uni,
                          diagnostico
      ON KEY (ESC)
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = FALSE THEN
   
      PROMPT "Requiere realizar Reenvió (S/N) " ATTRIBUTE (REVERSE)
             FOR opc ATTRIBUTE (REVERSE)

      IF opc MATCHES "[Ss]" THEN

         LET sel_where = " SELECT a.folio,",
                                 "a.consecutivo_reg,",
                                 "a.nss_uni,",
                                 "a.nss_cta1,",
				 "a.estado ",
                         " FROM   uni_det_solicitud a",
                         " WHERE  ",cla_where CLIPPED,
            --             " AND    a.result_operacion = ","'",x_result_operacion CLIPPED,"'",
			 " AND    a.folio = ",x_folio CLIPPED,
                         " ORDER BY 1,2,3"

         PREPARE query_actualiza FROM sel_where

         DECLARE cursor_3 CURSOR FOR query_actualiza

         LET pos = 1
         LET pos1 = 0
         LET cuantos_act = 0

         FOREACH cursor_3 INTO reg1.*
            UPDATE uni_solicitud
            SET    uni_solicitud.estado = 10
            WHERE  uni_solicitud.nss_uni = reg1.nss_uni
            AND    uni_solicitud.nss_cta1 = reg1.nss_cta1
            AND    uni_solicitud.estado = 30

            SELECT DBINFO("SQLCA.SQLERRD2")
	    INTO   cuantos_act
	    FROM   systables
	    WHERE  tabid = 1

            LET pos1 = pos1 + cuantos_act

            DISPLAY "No. Registros: ",pos1 AT 6,1 ATTRIBUTE (REVERSE)
         END FOREACH

	 PROMPT "Total Reg. Actualizados:",pos1 USING "<<<<&" ATTRIBUTE (REVERSE)
		FOR opc ATTRIBUTE(REVERSE)
      ELSE
         ERROR " PROCESO DE ACTUALIZACION CANCELADO .. "
      END IF
   ELSE
      ERROR " PROCESO CANCELADO .. "
   END IF

   ERROR ""

   CLOSE WINDOW ventana_3
END FUNCTION
########################################################################
