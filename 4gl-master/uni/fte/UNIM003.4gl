###############################################################################
#Proyecto          => AFORE (MEXICO)                                          #
#Propietario       => E.F.P          	                                      # 
#Programa UNIM003  => MANTENIMIENTO SOLICITUDES DE UNIFICACION DE CUENTAS     #
#Sistema           => UNI                                                     #
#Autor             => Miguel Angel Hernandez Martinez.                        #
#Fecha             => 03 de marzo de 2003.                                    #
#Modificado por    => Omar Sandoval Badillo                                   #
#Fecha Modifica    => 11 de marzo de 2004                                     #
###############################################################################
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
          cla_where             CHAR(500)

   DEFINE g_afore               RECORD LIKE tab_afore_local.*

   DEFINE g_uni RECORD LIKE uni_solicitud.*

   DEFINE g_mae_filiado RECORD 
          paterno           CHAR(40),
          materno           CHAR(40),
          nombres           CHAR(40),
          tipo_solicitud    SMALLINT,
          folio             DECIMAL(8,0)
   END RECORD

   DEFINE l_record ARRAY[5000] OF RECORD LIKE uni_solicitud.*

   DEFINE l_record1 ARRAY[3000] OF RECORD
          nss_uni           CHAR(11),
          nss_cta1          CHAR(11),
          fecha_solicitud   DATE
   END RECORD

END GLOBALS
###############################################################################
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      COMMENT LINE LAST

   DEFER INTERRUPT

   CALL STARTLOG("uni/fte/op73/UNIM003_errlog")
   CALL crea_tab_temp()

   CALL inicio()   #i
   CALL proceso_principal()   #pp
END MAIN
###############################################################################
FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIM0031" ATTRIBUTE(BORDER)
   DISPLAY " UNIM003       MANTENIMIENTO DE SOLICITUDES DE UNIFICACION                     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   DISPLAY "                      INFORMACION DE LOS NSS UNIFICADORES                      " AT 5,1 ATTRIBUTE(REVERSE)
   DISPLAY "                      INFORMACION DE LOS NSS UNIFICADOS                        " AT 11,1 ATTRIBUTE(REVERSE)
   DISPLAY "                                                                               " AT 18,1 ATTRIBUTE(REVERSE)

   MENU "SOLICITUD "
      COMMAND "Agrega" "Agrega solicitud "
         CALL agrega(0)
      COMMAND "Consulta" "Consulta solicitud "
         CALL consulta()
      COMMAND "Modifica" "Modifica solicitud "
         CALL modifica()
      COMMAND "Elimina" "Elimina solicitud "
         CALL elimina()
      COMMAND "NSS Complementario" "Ingreso de NSS's adicionales a solicitud"
	 CALL agrega(1)
      COMMAND "Salir" "Salir de Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1

END FUNCTION
###############################################################################
FUNCTION inicio()

   INITIALIZE g_uni.*,    
	      g_afore.*, 
	      g_usuario TO NULL
   INITIALIZE g_mae_filiado.* TO NULL

   DISPLAY g_uni.*

   LET rechazo = 0
   LET HOY     = TODAY
   LET ANIO    = TODAY -1 UNITS YEAR

   LET HORA = TIME
   LET hora_tope = "23:59:00"

   SELECT *,
          USER 
   INTO   g_afore.*,
          g_usuario 
   FROM   tab_afore_local

   LET sw_1 = 0

END FUNCTION
###############################################################################
FUNCTION agrega(x_men)

   DEFINE diaActual        DATE,
          sw               SMALLINT,
          digito           SMALLINT,
	  err_var          CHAR(20)

   DEFINE x_men            SMALLINT

   LET sw   = 0
   LET g_uni.folio = 0

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " AGREGAR " AT 1,68 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-B] Ingresar Registros " AT 1,48 
   DISPLAY " [Ctrl-C] Salir " AT 1,1

   INPUT BY NAME g_uni.nss_uni, 
                 g_uni.curp,
                 g_uni.paterno,
                 g_uni.materno,
                 g_uni.nombre,
                 g_uni.nss_cta1,
                 g_uni.curp_cta1,
                 g_uni.paterno_cta1,
                 g_uni.materno_cta1,
                 g_uni.nombre_cta1,
                 g_uni.fecha_solicitud WITHOUT DEFAULTS

      AFTER FIELD nss_uni
         IF g_uni.nss_uni IS NULL THEN
            ERROR "  Debe ingresar NSS p/ capturar solicitud"
            NEXT FIELD nss_uni
         END IF
         ERROR ""

         IF LENGTH(g_uni.nss_uni) < 11 THEN
	    ERROR "N.S.S debe de contener 11 digitos"
	    NEXT FIELD nss_uni
         END IF

         DISPLAY "                          " AT 18,2 ATTRIBUTE (REVERSE)

         CALL digito_verif(g_uni.nss_uni[1,10],10)
              RETURNING digito

         IF digito = 32000 THEN
            ERROR "N.S.S. contiene digitos"
            NEXT FIELD nss_uni
         END IF
         ERROR "" 

         IF LENGTH(g_uni.nss_uni) = 11 AND digito <> g_uni.nss_uni[11] THEN
            ERROR "Digito Verificador Invalido, el digito debe ser: ",
                  digito
            NEXT FIELD nss_uni
         END IF
         ERROR ""

--valida si tiene familias asociadas <> de estado 10
	 IF x_men = 0 THEN
            SELECT "X"
            FROM   uni_solicitud 
            WHERE  nss_uni = g_uni.nss_uni
	    AND    estado <> 10
	    GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
               ERROR "El NSS cuenta con un estado diferente a captura" 
               LET sw = 1
               NEXT FIELD nss_uni
            END IF
         END IF   
         
--valida el consecutivo mayor para seguir el incremento
	 SELECT NVL(MAX(consecutivo_reg + 1),0)
	 INTO   sw_1
	 FROM   uni_solicitud
	 WHERE  nss_uni = g_uni.nss_uni

	 IF sw_1 = 0 THEN
	    LET sw_1 = 1
         END IF

--valida si existe y esta en estado = 30
	 IF x_men = 1 THEN
            SELECT "X"
            FROM   uni_det_solicitud 
            WHERE  nss_uni = g_uni.nss_uni
	    -- AND    estado = 30
	    AND    result_operacion = "01"

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR "El NSS no tiene familias asociadas"
               LET sw = 1
               NEXT FIELD nss_uni
            END IF
         END IF   

         SELECT a.n_unico,           #Curp
                a.paterno,
                a.materno,
                a.nombres
         INTO   g_uni.curp,
                g_mae_filiado.paterno,
                g_mae_filiado.materno,
                g_mae_filiado.nombres
         FROM   afi_mae_afiliado a
         WHERE  a.n_seguro = g_uni.nss_uni

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "El NSS no existe en la Afore"
            LET sw = 1
            NEXT FIELD curp  
         ELSE
            LET g_uni.paterno = g_mae_filiado.paterno CLIPPED
            LET g_uni.materno = g_mae_filiado.materno CLIPPED
            LET g_uni.nombre  = g_mae_filiado.nombres CLIPPED

            DISPLAY BY NAME g_uni.nombre
            DISPLAY BY NAME g_uni.paterno
            DISPLAY BY NAME g_uni.materno
            DISPLAY BY NAME g_uni.curp
            NEXT FIELD nss_cta1
         END IF

      AFTER FIELD nombre
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD nss_uni
         END IF

      AFTER FIELD paterno
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD materno
         END IF

      AFTER FIELD materno
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD nombre 
         END IF

      AFTER FIELD curp
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD paterno  
         END IF

      AFTER FIELD nss_cta1
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD curp
         END IF

         IF g_uni.nss_cta1 IS NULL THEN
            ERROR "  Debe ingresar NSS p/ capturar solicitud"
            NEXT FIELD nss_cta1
         END IF
         ERROR ""

         IF LENGTH(g_uni.nss_cta1) < 11 THEN
	    ERROR "N.S.S debe de contener 11 digitos"
	    NEXT FIELD nss_cta1
         END IF

         CALL digito_verif(g_uni.nss_cta1[1,10],10)
              RETURNING digito

         IF digito = 32000 THEN
            ERROR "N.S.S. solo contiene digitos"
            NEXT FIELD nss_cta1
         END IF
         ERROR "" 

         IF LENGTH(g_uni.nss_cta1) = 11 AND digito <> g_uni.nss_cta1[11] THEN
            ERROR "Digito Verificador Invalido, el digito debe ser: ",
                  digito
            NEXT FIELD nss_cta1
         END IF
         ERROR ""

         SELECT a.paterno,
                a.materno,
                a.nombres
         INTO   g_mae_filiado.paterno,
                g_mae_filiado.materno,
                g_mae_filiado.nombres
         FROM   afi_mae_afiliado a
         WHERE  a.n_seguro = g_uni.nss_cta1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "El NSS no existe en la Afore"
            SLEEP 2
            ERROR ""
         ELSE
            LET g_uni.paterno_cta1 = g_mae_filiado.paterno CLIPPED
            LET g_uni.materno_cta1 = g_mae_filiado.materno CLIPPED
            LET g_uni.nombre_cta1  = g_mae_filiado.nombres CLIPPED

            DISPLAY BY NAME g_uni.nombre_cta1
            DISPLAY BY NAME g_uni.paterno_cta1
            DISPLAY BY NAME g_uni.materno_cta1

            NEXT FIELD fecha_solicitud
         END IF

      AFTER FIELD nombre_cta1
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD nss_cta1
         END IF

      AFTER FIELD paterno_cta1
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD nombre_cta1
         END IF

      AFTER FIELD materno_cta1
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD paterno_cta1
         END IF

      AFTER FIELD curp_cta1
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD curp_cta1
	 END IF

      AFTER FIELD fecha_solicitud
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD materno_cta1
         END IF

         IF g_uni.fecha_solicitud IS NULL THEN
            ERROR "FECHA SOLICITUD NO PUEDE SER NULA"
            NEXT FIELD fecha_solicitud
         END IF
         ERROR ""

         IF g_uni.fecha_solicitud > HOY THEN
            ERROR "FECHA SOLICITUD NO PUEDE SER MAYOR A HOY"
            NEXT FIELD fecha_solicitud
         END IF
         ERROR ""

         LET g_uni.consecutivo_reg = sw_1

         SELECT "X"
         FROM   uni_solicitud
         WHERE  nss_uni = g_uni.nss_uni
         AND    nss_cta1 = g_uni.nss_cta1
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            SELECT "X"
            FROM   uni_solicitud
            WHERE  nss_uni = g_uni.nss_cta1
            AND    nss_cta1 = g_uni.nss_uni
            GROUP BY 1

	    IF SQLCA.SQLCODE <> 0 THEN   
            
               LET g_uni.estado     = 10
               LET g_uni.factualiza = HOY
               LET g_uni.usuario    = g_usuario

               LET sw_1 = valida_unificador(g_uni.*)
		     
               INITIALIZE g_uni.nss_cta1,
	                  g_uni.curp_cta1, 
                          g_uni.nombre_cta1,
                          g_uni.paterno_cta1,
                          g_uni.materno_cta1 TO NULL

               DISPLAY BY NAME g_uni.nss_cta1,
	                       g_uni.curp_cta1, 
                               g_uni.nombre_cta1,
                               g_uni.paterno_cta1,
                               g_uni.materno_cta1

               NEXT FIELD nss_cta1
            ELSE
                PROMPT "La solicitud ya esta registrada pulse <ENTER> para continuar" ATTRIBUTE(REVERSE) FOR enter ATTRIBUTE(REVERSE)
                CALL inicio()
                LET int_flag = TRUE
	        CLEAR FORM
	        NEXT FIELD nss_uni
            END IF
         ELSE
            PROMPT "La solicitud ya esta registrada pulse <ENTER> para continuar" ATTRIBUTE(REVERSE) FOR enter ATTRIBUTE(REVERSE)
            SLEEP 2
            ERROR "" 
            CALL inicio()
            LET int_flag = TRUE
	    CLEAR FORM
	    NEXT FIELD nss_uni
         --   EXIT INPUT
         END IF

         CLEAR FORM

      ON KEY (CONTROL-B)
	 CALL valida_dos(x_men)
	 EXIT INPUT
      ON KEY (CONTROL-C)
         ERROR ""
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      CALL inicio()
      DISPLAY "                          " AT 18,2 ATTRIBUTE (REVERSE)
      ERROR "AGREGAR CANCELADO..."
      SLEEP 2
      ERROR ""
      CLEAR FORM
      CLEAR SCREEN  
   END IF

END FUNCTION
###############################################################################
FUNCTION ingresa_solicitud(x_men)

   DEFINE reg_4 RECORD LIKE uni_solicitud.*

   DEFINE f_hoy            DATE,
          dia              SMALLINT,
          vfecha_inf       DATE,
	  err_var          CHAR(20),
	  nss_tmp          CHAR(100)

   DEFINE x_men            SMALLINT

   LET HORA = TIME

   IF x_men = 1 THEN
      DELETE FROM uni_solicitud
      WHERE nss_uni = g_uni.nss_uni
   END IF

   INSERT INTO uni_solicitud
   SELECT *
   FROM   tmp_uni_solicitud  

   INITIALIZE g_uni.* TO NULL
   DISPLAY g_uni.*

   DELETE FROM tmp_uni_solicitud
   DELETE FROM tmp_uni_ano

   CLEAR FORM
END FUNCTION
###############################################################################
FUNCTION consulta()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,2 WITH FORM "UNIM0032" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                       SOLICITUDES DE UNIFICACION                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON nss_uni,
                             nss_cta1
                        FROM nss_uni,
                             nss_cta1
         ON KEY (control-m)
            ERROR "PROCESANDO INFORMACION..."
	    SLEEP 2
	    ERROR ""
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
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = " SELECT nss_uni,",
                              "nss_cta1,",
                              "fecha_solicitud",
                      " FROM   uni_solicitud ",
                      " WHERE ",cla_where CLIPPED,
                      " AND    estado = 10 ",
                      " ORDER BY 1 "

      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1

      FOREACH cursor_1 INTO l_record1[pos].nss_uni,
                            l_record1[pos].nss_cta1,
                            l_record1[pos].fecha_solicitud

         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record1[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record1 TO scr_1.*
            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE SOLICITUDES .. VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION modifica()

   DEFINE diaActual        DATE,
          sw               SMALLINT,
          digito           SMALLINT

   LET sw   = 0
   LET sw_1 = 0

   LET pos  = 2

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,2 WITH FORM "UNIM0032" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                  Escoja con <ENTER> la solicitud a modificar               " AT 2,1
      DISPLAY "                       SOLICITUDES DE UNIFICACION                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON nss_uni,
                             nss_cta1
                        FROM nss_uni,
                             nss_cta1
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
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = " SELECT * ",
                      " FROM   uni_solicitud ",
                      " WHERE  ",cla_where CLIPPED,
                      " AND    estado = 10 ",
                      " ORDER BY 1 "

      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1

      FOREACH cursor_2 INTO l_record[pos].*

         LET l_record1[pos].nss_uni  = l_record[pos].nss_uni
         LET l_record1[pos].nss_cta1 = l_record[pos].nss_cta1
         LET l_record1[pos].fecha_solicitud = l_record[pos].fecha_solicitud

         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record1[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record1 TO scr_1.*
            ON KEY (control-m)
               LET pos = ARR_CURR()
               LET g_uni.nss_uni         = l_record[pos].nss_uni
               LET g_uni.nombre          = l_record[pos].nombre
               LET g_uni.paterno         = l_record[pos].paterno
               LET g_uni.materno         = l_record[pos].materno
               LET g_uni.curp            = l_record[pos].curp

               LET g_uni.nss_cta1        = l_record[pos].nss_cta1
	       LET g_uni.curp_cta1       = l_record[pos].curp_cta1
               LET g_uni.nombre_cta1     = l_record[pos].nombre_cta1
               LET g_uni.paterno_cta1    = l_record[pos].paterno_cta1
               LET g_uni.materno_cta1    = l_record[pos].materno_cta1

               LET g_uni.fecha_solicitud = l_record[pos].fecha_solicitud

               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "Debe elegir un registro."
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE SOLICITUDES .. VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME g_uni.nss_uni,
		    g_uni.curp,
                    g_uni.paterno,
                    g_uni.materno,
                    g_uni.nombre,
                    g_uni.nss_cta1,
		    g_uni.curp_cta1,
                    g_uni.paterno_cta1,
                    g_uni.materno_cta1,
                    g_uni.nombre_cta1,
                    g_uni.fecha_solicitud WITHOUT DEFAULTS


         BEFORE FIELD nss_uni
	    NEXT FIELD curp

         AFTER FIELD nss_uni
            IF g_uni.nss_uni IS NULL THEN
               ERROR "  Debe ingresar NSS p/ capturar solicitud"
               NEXT FIELD nss_uni
            END IF
            ERROR ""

            CALL digito_verif(g_uni.nss_uni[1,10],10)
                 RETURNING digito

            IF digito = 32000 THEN
               ERROR "N.S.S. solo contiene digitos"
               NEXT FIELD nss_uni
            END IF
            ERROR "" 

            IF LENGTH(g_uni.nss_uni) = 11 AND digito <> g_uni.nss_uni[11] THEN
               ERROR "Digito Verificador Invalido, el digito debe ser: ",
                     digito
               NEXT FIELD nss_uni
            END IF
            ERROR ""

            SELECT a.n_unico,           #Curp
                   a.paterno,
                   a.materno,
                   a.nombres
            INTO   g_uni.curp,
                   g_mae_filiado.paterno,
                   g_mae_filiado.materno,
                   g_mae_filiado.nombres
            FROM   afi_mae_afiliado a
            WHERE  a.n_seguro = g_uni.nss_uni

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR "El NSS no existe en la Afore"
               LET sw = 1
               NEXT FIELD nombre
            ELSE
               LET g_uni.paterno = g_mae_filiado.paterno CLIPPED
               LET g_uni.materno = g_mae_filiado.materno CLIPPED
               LET g_uni.nombre  = g_mae_filiado.nombres CLIPPED

               DISPLAY BY NAME g_uni.nombre
               DISPLAY BY NAME g_uni.paterno
               DISPLAY BY NAME g_uni.materno
               DISPLAY BY NAME g_uni.curp
               NEXT FIELD nss_cta1
            END IF

         AFTER FIELD nombre
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD nss_uni
            END IF

         AFTER FIELD paterno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD nombre
            END IF

         AFTER FIELD materno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD paterno
            END IF

         AFTER FIELD curp
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD materno
            END IF

         BEFORE FIELD nss_cta1
	    NEXT FIELD curp_cta1

         AFTER FIELD nss_cta1
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD curp
            END IF

            IF g_uni.nss_cta1 IS NULL THEN
               ERROR "  Debe ingresar NSS p/ capturar solicitud"
               NEXT FIELD nss_cta1
            END IF
            ERROR ""

            CALL digito_verif(g_uni.nss_cta1[1,10],10)
                 RETURNING digito

            IF digito = 32000 THEN
               ERROR "N.S.S. solo contiene digitos"
               NEXT FIELD nss_cta1
            END IF
            ERROR "" 

            IF LENGTH(g_uni.nss_cta1) = 11 AND digito <> g_uni.nss_cta1[11] THEN
               ERROR "Digito Verificador Invalido, el digito debe ser: ",
                      digito
               NEXT FIELD nss_cta1
            END IF
            ERROR ""

            SELECT a.paterno,
                   a.materno,
                   a.nombres
            INTO   g_mae_filiado.paterno,
                   g_mae_filiado.materno,
                   g_mae_filiado.nombres
            FROM   afi_mae_afiliado a
            WHERE  a.n_seguro = g_uni.nss_cta1

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR "El NSS no existe en la Afore"
               SLEEP 2
               ERROR ""
            ELSE
               LET g_uni.paterno_cta1 = g_mae_filiado.paterno CLIPPED
               LET g_uni.materno_cta1 = g_mae_filiado.materno CLIPPED
               LET g_uni.nombre_cta1  = g_mae_filiado.nombres CLIPPED

               DISPLAY BY NAME g_uni.nombre_cta1
               DISPLAY BY NAME g_uni.paterno_cta1
               DISPLAY BY NAME g_uni.materno_cta1

               NEXT FIELD fecha_solicitud
            END IF

         AFTER FIELD nombre_cta1
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD nss_cta1
            END IF

         AFTER FIELD paterno_cta1
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD nombre_cta1
            END IF

         AFTER FIELD materno_cta1
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD paterno_cta1
            END IF

         AFTER FIELD fecha_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD materno_cta1
            END IF

            IF g_uni.fecha_solicitud IS NULL THEN
               ERROR "FECHA SOLICITUD NO PUEDE SER NULA"
               NEXT FIELD fecha_solicitud
            END IF

            ERROR ""

            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE uni_solicitud
               SET    nss_uni         = g_uni.nss_uni,
                      nombre          = g_uni.nombre,
                      paterno         = g_uni.paterno,
                      materno         = g_uni.materno,
                      curp            = g_uni.curp,
                      nss_cta1        = g_uni.nss_cta1,
		      curp_cta1       = g_uni.curp_cta1,
                      nombre_cta1     = g_uni.nombre_cta1,
                      paterno_cta1    = g_uni.paterno_cta1,
                      materno_cta1    = g_uni.materno_cta1,
                      fecha_solicitud = g_uni.fecha_solicitud
               WHERE  nss_uni  = l_record[pos].nss_uni
               AND    nss_cta1 = l_record[pos].nss_cta1
               AND    estado = 10

               ERROR "REGISTRO MODIFICADO"
               SLEEP 2
               ERROR ""
            ELSE
               ERROR "PROCESO DE MODIFICAR CANCELADO."
               SLEEP 2
               ERROR ""
            END IF

            CALL inicio()

            EXIT INPUT
         ON KEY (INTERRUPT)
            CALL inicio()
            EXIT INPUT
      END INPUT

      CLEAR FORM
   ELSE
      ERROR "ARCHIVO DE SOLICITUDES... VACIO."
   END IF

   CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,2 WITH FORM "UNIM0032" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                  Escoja con <ENTER> la solicitud a modificar               " AT 2,1
      DISPLAY "                       SOLICITUDES DE UNIFICACION                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON nss_uni,
                             nss_cta1
                        FROM nss_uni,
                             nss_cta1
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
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = " SELECT * ",
                      " FROM   uni_solicitud ",
                      " WHERE  ",cla_where CLIPPED,
                      " AND    estado = 10 ",
                      " ORDER BY 1 "

      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1

      FOREACH cursor_3 INTO l_record[pos].*

         LET l_record1[pos].nss_uni = l_record[pos].nss_uni
         LET l_record1[pos].nss_cta1 = l_record[pos].nss_cta1
         LET l_record1[pos].fecha_solicitud = l_record[pos].fecha_solicitud

         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record1[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record1 TO scr_1.*
            ON KEY (control-m)
               LET pos = ARR_CURR()
               LET g_uni.nss_uni         = l_record[pos].nss_uni
               LET g_uni.nombre          = l_record[pos].nombre
               LET g_uni.paterno         = l_record[pos].paterno
               LET g_uni.materno         = l_record[pos].materno
               LET g_uni.curp            = l_record[pos].curp

               LET g_uni.nss_cta1        = l_record[pos].nss_cta1
	       LET g_uni.curp_cta1       = l_record[pos].curp_cta1
               LET g_uni.nombre_cta1     = l_record[pos].nombre_cta1
               LET g_uni.paterno_cta1    = l_record[pos].paterno_cta1
               LET g_uni.materno_cta1    = l_record[pos].materno_cta1

               LET g_uni.fecha_solicitud = l_record[pos].fecha_solicitud

               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "Debe elegir un registro."
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE SOLICITUDES .. VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME g_uni.nss_uni,
                      g_uni.nombre,
                      g_uni.paterno,
                      g_uni.materno,
                      g_uni.curp,
                      g_uni.nss_cta1,
		      g_uni.curp_cta1,
                      g_uni.nombre_cta1,
                      g_uni.paterno_cta1,
                      g_uni.materno_cta1,
                      g_uni.fecha_solicitud

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM uni_solicitud
         WHERE nss_uni  = g_uni.nss_uni
         AND   nss_cta1 = g_uni.nss_cta1
         AND   estado   = 10

         ERROR "REGISTRO ELIMINADO" 
         SLEEP 2
      ELSE
         ERROR "ELIMINAR CANCELADO" 
         SLEEP 2
      END IF

      ERROR ""

      CALL inicio()

      CLEAR FORM
   ELSE
      ERROR "ARCHIVO DE SOLICITUDES... VACIO."
   END IF

   CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION Pregunta()
   PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE

   DEFINE diaHabilSig   DATE,
          diaSemana     SMALLINT,
          feriado       SMALLINT,
          finSemana     SMALLINT

   LET diaHabilSig = diaActual +1 UNITS DAY

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilSig) 

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF

      SELECT *
      FROM   tab_feriado
      WHERE  feria_fecha = diaHabilSig

      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF

      IF feriado = 1 OR finSemana = 1 THEN
         LET diaHabilSig = diaHabilSig + 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilSig 

END FUNCTION
###############################################################################
FUNCTION digito_verif(valor,longitud )

   DEFINE cadena     CHAR(20),
          valor      CHAR(10),
          longitud   SMALLINT,
          suma       SMALLINT,
          sumachar   CHAR(2),
          digito     SMALLINT,
          i,j        SMALLINT,
          temp       CHAR(2),
          ultima     SMALLINT,
          t          SMALLINT 

   DEFINE x ARRAY[10] OF CHAR(1)

   LET x[1]  =valor[1]
   LET x[2]  =valor[2]
   LET x[3]  =valor[3]
   LET x[4]  =valor[4]
   LET x[5]  =valor[5]
   LET x[6]  =valor[6]
   LET x[7]  =valor[7]
   LET x[8]  =valor[8]
   LET x[9]  =valor[9]
   LET x[10] =valor[10]
 
   FOR t = 1 TO 10
      IF x[t] <> "0" AND
         x[t] <> "1" AND
         x[t] <> "2" AND
         x[t] <> "3" AND
         x[t] <> "4" AND
         x[t] <> "5" AND
         x[t] <> "6" AND
         x[t] <> "7" AND
         x[t] <> "8" AND
         x[t] <> "9" THEN

         LET digito = 32000
         RETURN  digito
      END IF
   END FOR

   LET j = 0
   FOR i = 1 TO longitud
      LET j = j + 1
      IF i MOD 2 = 0 THEN
         LET temp      = valor[i] * 2
         LET cadena[j] = temp[1]

         IF LENGTH(temp) > 1 THEN
            LET j = j + 1
            LET cadena[j] = temp[2]
         END IF
      ELSE
         LET cadena[j] = valor[i]
      END IF
   END FOR

   LET suma = 0
   FOR i = 1 TO j
      LET suma = suma + cadena[i]
   END FOR

   LET sumachar = suma
   LET ultima   = LENGTH(sumachar)  

   LET digito = 10 - sumachar[ultima]  

   IF digito  = 10 THEN
      LET digito = 0
   END IF

   RETURN digito  

END FUNCTION
################################################################################
FUNCTION valida_unificador(reg_4)

   DEFINE reg_4 RECORD LIKE uni_solicitud.*

   DEFINE err_var          CHAR(20),
	  x_cuantos        SMALLINT

   IF HORA > hora_tope THEN
      CALL habil_siguiente(reg_4.factualiza)
      RETURNING reg_4.factualiza
   END IF

   WHILE TRUE
      PROMPT "Desea Grabar Solicitud S/N ? " FOR CHAR aux_pausa
      IF aux_pausa MATCHES "[SsNn]" THEN
         EXIT WHILE
      END IF
   END WHILE

   IF aux_pausa MATCHES "[Ss]" THEN
      ERROR "Procesando Informacion.... Espere un momento"

      DISPLAY "No. Registro: ", sw_1 AT 18,2 ATTRIBUTE(REVERSE)

      INSERT INTO tmp_uni_solicitud
      VALUES (reg_4.*)

      LET sw_1 = sw_1 + 1

      IF SQLCA.SQLCODE < 0 THEN
         LET err_var = "ERROR INSERT Agregar",
		       g_uni.nss_uni,
		       g_uni.nss_cta1,
                       err_get(SQLCA.SQLCODE)

         CALL errorlog(err_var CLIPPED)
         ERROR err_var CLIPPED
         ERROR "EL REGISTRO NO EXISTE"
      ELSE
         ERROR "SOLICITUD ALMACENADA TEMPORALMENTE" 
      END IF
   ELSE
      ERROR "Solicitud no fue Registrada"
      SLEEP 2
      ERROR ""
   END IF

   RETURN sw_1
END FUNCTION
################################################################################
FUNCTION valida_dos(x_men)

   DEFINE reg_4 RECORD LIKE uni_solicitud.*

   DEFINE indice      ,
	  sw          ,
          cont        SMALLINT

   DEFINE ga_nss   ARRAY[10] OF RECORD
          nss_uni    CHAR(11),
	  nss_cta1   CHAR(11)
   END RECORD

   DEFINE x RECORD
	  nss      CHAR(11),
	  ano      SMALLINT,
	  curp     CHAR(18),
	  paterno  CHAR(40),
	  materno  CHAR(40),
	  nombre   CHAR(40),
	  consecutivo_reg   SMALLINT
   END RECORD

   DEFINE x_prueba  CHAR(11)

   DEFINE x_cuantos SMALLINT,
	  x_estado  SMALLINT

   DEFINE x_cuantos_nss SMALLINT

   DEFINE x_men         SMALLINT

   DEFINE xx_men  ARRAY[5000] OF RECORD LIKE uni_solicitud.*
	 
   IF x_men = 1 THEN
      UPDATE uni_det_solicitud 
      SET estado = 50
      WHERE nss_uni = g_uni.nss_uni
--       AND   estado = 30 
      AND   result_operacion = "01"

      UPDATE uni_solicitud 
      SET estado    = 10
      WHERE nss_uni = g_uni.nss_uni
      AND   estado  = 30

      INSERT INTO tmp_uni_solicitud
      SELECT *
      FROM   uni_solicitud
      WHERE  nss_uni = g_uni.nss_uni
   END IF

   DECLARE cust_nss CURSOR FOR
   SELECT nss_uni,
          nss_cta1
   FROM   tmp_uni_solicitud

   LET indice = 1
   LET sw     = 0

   FOREACH cust_nss INTO ga_nss[indice].*

      SELECT COUNT(*)
      INTO   cont
      FROM   afi_mae_afiliado
      WHERE  n_seguro = ga_nss[indice].nss_uni

      IF cont > 0 THEN
         LET sw = 1
      END IF

      SELECT COUNT(*)
      INTO   cont
      FROM   afi_mae_afiliado
      WHERE  n_seguro = ga_nss[indice].nss_cta1

      IF cont > 0 THEN
         LET sw = 1
      END IF

      LET indice = indice + 1
   END FOREACH

   INSERT INTO tmp_uni_ano
   SELECT nss_uni,
	  nss_uni[3,4],
	  curp,
	  paterno,
	  materno,
	  nombre,
	  consecutivo_reg
   FROM   tmp_uni_solicitud

   INSERT INTO tmp_uni_ano
   SELECT nss_cta1,
	  nss_cta1[3,4],
	  curp_cta1,
	  paterno_cta1,
	  materno_cta1,
	  nombre_cta1,
	  consecutivo_reg
   FROM   tmp_uni_solicitud

   IF x_men = 0 THEN
      DECLARE cur_tmp CURSOR FOR
      SELECT * 
      FROM   tmp_uni_ano
      ORDER BY 2

      FOREACH cur_tmp INTO x.*
         UPDATE tmp_uni_solicitud
         SET    nss_uni = x.nss,
                curp    = x.curp,
	        paterno = x.paterno,
	        materno = x.materno,
	        nombre  = x.nombre

         LET x_prueba = x.nss 
         EXIT FOREACH
      END FOREACH

      DECLARE prueba CURSOR FOR
      SELECT *
      FROM   tmp_uni_ano
      WHERE  nss <> x_prueba

      FOREACH prueba INTO x.*
         UPDATE tmp_uni_solicitud
         SET    nss_cta1     = x.nss,
                curp_cta1    = x.curp,
	        paterno_cta1 = x.paterno,
	        materno_cta1 = x.materno,
	        nombre_cta1  = x.nombre
         WHERE consecutivo_reg = x.consecutivo_reg 
      END FOREACH
   END IF

---opcion nueva reordena los NSS's
   IF x_men = 1 THEN
      {
      DECLARE cur_tmp1 SCROLL CURSOR FOR
      SELECT * 
      FROM   tmp_uni_ano
      ORDER BY 2

      OPEN cur_tmp1
      FETCH FIRST cur_tmp1 INTO x.*
         UPDATE tmp_uni_solicitud
         SET    nss_uni = x.nss,
	        curp    = x.curp,
	        paterno = x.paterno,
	        materno = x.materno,
      	        nombre  = x.nombre
      CLOSE cur_tmp1
      FREE  cur_tmp1

      LET x_prueba = x.nss 
      }
----
      DECLARE cur_tmp1 CURSOR FOR
      SELECT * 
      FROM   tmp_uni_ano
      ORDER BY 2

      FOREACH cur_tmp1 INTO x.*
         UPDATE tmp_uni_solicitud
         SET    nss_uni = x.nss,
                curp    = x.curp,
	        paterno = x.paterno,
	        materno = x.materno,
	        nombre  = x.nombre

         LET x_prueba = x.nss 
         EXIT FOREACH
      END FOREACH
----
      DECLARE prueba5 CURSOR FOR
      SELECT *
      FROM   tmp_uni_ano
      WHERE  nss <> x_prueba

      FOREACH prueba5 INTO x.*
         UPDATE tmp_uni_solicitud
         SET    nss_cta1     = x.nss,
                curp_cta1    = x.curp,
	        paterno_cta1 = x.paterno,
	        materno_cta1 = x.materno,
	        nombre_cta1  = x.nombre
         WHERE  consecutivo_reg = x.consecutivo_reg 
      END FOREACH
   END IF

   IF sw = 1 THEN
      CALL ingresa_solicitud(x_men)
      ERROR "REGISTROS INGRESADOS"
   ELSE
      PROMPT "NSS CAPTURADOS NO EXISTEN EN LA AFORE, NO PROCEDE REGISTRO",
             " DE FAMILIA"  FOR enter
   END IF

   CLEAR FORM
END FUNCTION
#################################################################
FUNCTION crea_tab_temp()
   SELECT *
   FROM   uni_solicitud
   WHERE  1 = 0
   INTO TEMP tmp_uni_solicitud

   SELECT nss_uni nss,
	  estado ano,
	  curp,
	  paterno,
	  materno,
	  nombre,
	  consecutivo_reg estado
   FROM   uni_solicitud
   WHERE  1 = 0
   INTO TEMP tmp_uni_ano

END FUNCTION
#################################################################
