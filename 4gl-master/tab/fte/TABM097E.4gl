##############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                          #
#Propietario       => E.F.P.                                                 #
#Programa TABM097  => CATALOGO DIAGNOSTICO DE AFORE                          #
#Fecha             => 17 DE MAYO DEL 2004                                    #
#Autor             => ERIKA PAOLA VERA PIÑA.                                 #
##############################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_param_dis   RECORD LIKE seg_modulo.*

	DEFINE aux_pausa       CHAR(1),
		    sw_1            SMALLINT,
		    hoy             DATE,
		    usuario         CHAR(8),
			 pos             INTEGER,
			 sel_where       CHAR(30000),
			 cla_where       CHAR(30000),
			 g_impre         CHAR(300),
			 g_lista         CHAR(300)

   DEFINE g_reg           RECORD
	       codigo          CHAR(2),
			 descripcion     CHAR(120),
			 factualiza      DATE,
			 usuario         CHAR(8)
	END RECORD

	DEFINE l_record        ARRAY[1000] OF RECORD
	       codigo          CHAR(2),
			 descripcion     CHAR(120)
	END RECORD
END GLOBALS
-------------------------------------------------------------------------------
MAIN
   OPTIONS PROMPT LINE LAST,
	   INPUT WRAP,
	   ACCEPT KEY control-o
		
      DEFER INTERRUPT
      CALL inicio()
      CALL proceso()
END MAIN
-------------------------------------------------------------------------------
FUNCTION inicio()
   SELECT USER,
	       ruta_listados
   INTO   usuario,
		    g_param_dis.ruta_listados
	FROM   seg_modulo
	WHERE  modulo_cod = "tab"
END FUNCTION
-------------------------------------------------------------------------------
FUNCTION proceso()

   LET HOY = TODAY

	   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0971" ATTRIBUTE( BORDER)

		DISPLAY " TABM097              CATALOGO DIAGNOSTICO DE AFORE                            " AT 3,1 ATTRIBUTE(REVERSE)
		DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

		MENU "CATALOGO DE DIAGNOSTICO "
		   COMMAND "Agrega" "Agrega Diagnostico de Afore "
		      CALL Agrega()
		      CLEAR SCREEN
		   COMMAND "Consulta" "Consulta Diagnostico de Afore"
		      CALL Consulta()
		      CLEAR SCREEN
		   COMMAND "Modifica" "Modifica Diagnostico de Afore"
		      CALL Modifica()
		      CLEAR SCREEN
		   COMMAND "Elimina" "Elimina Diagnostico de Afore"
		      CALL Elimina()
		      CLEAR SCREEN
		   COMMAND "Salir" "Salir del Programa"
		      EXIT MENU
		END MENU
      CLOSE WINDOW ventana_1
END FUNCTION
------------------------------------------------------------------------------
FUNCTION Inicializa()
   LET sw_1 = 0
	
   INITIALIZE g_reg.* TO NULL
	
   DISPLAY BY NAME g_reg.codigo,
                   g_reg.descripcion
END FUNCTION
------------------------------------------------------------------------------
FUNCTION Agrega()

   DISPLAY "" AT 1,1
	
   DISPLAY "" AT 2,1
	
   DISPLAY " ( ESC ) Agrega                 (CTRL-C) Salir                                 " AT 1,1 ATTRIBUTE(BOLD)
	
   DISPLAY " AGREGA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
	
   LET g_reg.codigo      = NULL
	LET g_reg.descripcion = NULL
	
   LET sw_1 = 0
	
   INPUT BY NAME g_reg.codigo,
	              g_reg.descripcion
	
	BEFORE FIELD codigo
	   IF sw_1 = 0 THEN
		   LET sw_1 = 1
			DISPLAY BY NAME g_reg.codigo
		END IF

	AFTER FIELD codigo
	   IF g_reg.codigo IS NULL THEN
		   ERROR " --->CODIGO NO PUEDE SER NULO "
			NEXT FIELD codigo
		END IF
		
		SELECT "X"
		FROM   tab_diag_afore
		WHERE  codigo = g_reg.codigo
		
      IF STATUS <> NOTFOUND THEN
         ERROR " --->CODIGO YA INGRESADO "
         NEXT FIELD codigo
      END IF

	AFTER FIELD descripcion
	   IF g_reg.descripcion IS NULL THEN
			ERROR " --->DESCRIPCION NO PUEDE SER NULO"
			NEXT FIELD descripcion
      END IF

	ON KEY(ESC)
	   IF g_reg.codigo IS NULL THEN
		   ERROR " --->CODIGO NO PUEDE SER NULO"
			NEXT FIELD codigo
	   END IF
		
	   SELECT "X"
		FROM tab_diag_afore
		WHERE codigo = g_reg.codigo
		
      IF STATUS <> NOTFOUND THEN
         ERROR " --->CODIGO YA INGRESADO"
         NEXT FIELD codigo
      END IF
		
      IF g_reg.descripcion IS NULL THEN
         ERROR " --->DESCRIPCION NO PUEDE SER NULO"
         NEXT FIELD descripcion
      END IF

      LET g_reg.factualiza = TODAY
      LET g_reg.usuario = usuario
		
      INSERT INTO tab_diag_afore 
		VALUES(g_reg.*)
		
      ERROR " --->REGISTRO INGRESADO " 
		SLEEP 2
      ERROR ""
      CALL Inicializa()
      NEXT FIELD codigo
   ON KEY (INTERRUPT)
      CALL Inicializa()
      EXIT INPUT
   END INPUT
END FUNCTION
------------------------------------------------------------------------------
FUNCTION Consulta()

   LET pos = 2
	
   IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
		
      OPEN WINDOW ventana_2 AT 3,3 WITH FORM "TABM0972" ATTRIBUTE( BORDER)
		
      DISPLAY " (ENTER) Consulta             (CTRL-P) Imprimir              (CTRL-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY "              D I A G N O S T I C O     D E     A F O R E                      " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

		 LET int_flag = FALSE

       CONSTRUCT cla_where ON codigo
                         FROM codigo
          ON KEY (CONTROL-M)
             ERROR " --->PROCESANDO INFORMACION... "
             SLEEP 2
             LET int_flag = FALSE
             EXIT CONSTRUCT
          ON KEY (CONTROL-C)
             LET int_flag = TRUE
             EXIT CONSTRUCT
       END CONSTRUCT

       IF int_flag = TRUE THEN
          LET int_flag = FALSE
          ERROR " --->PROCESO CANCELADO... "
          SLEEP 2
          ERROR ""
          CLEAR SCREEN
          CLOSE WINDOW ventana_2
          RETURN
       END IF

       LET sel_where = " SELECT * ",
		                 " FROM tab_diag_afore ",
							  " WHERE ",cla_where CLIPPED,
							  " ORDER BY 1 "

       PREPARE query FROM sel_where

       DECLARE cursor_1 CURSOR FOR query
		 
       LET pos = 1
		 
       FOREACH cursor_1 INTO l_record[pos].*
		    LET l_record[pos].codigo      = l_record[pos].codigo
			 LET l_record[pos].descripcion = l_record[pos].descripcion
          LET pos = pos + 1
       END FOREACH
		 
       INITIALIZE l_record[pos].* TO NULL

       IF (pos-1) >= 1 THEN
          CALL  SET_COUNT(pos-1)
          ERROR ""
			 
          DISPLAY ARRAY l_record TO scr_1.*
			    ON KEY (CONTROL-M)
				    LET pos = ARR_CURR()
					 LET g_reg.codigo      = l_record[pos].codigo
					 LET g_reg.descripcion = l_record[pos].descripcion
					 EXIT DISPLAY
             ON KEY (CONTROL-P)
                ERROR " --->PROCESANDO INFORMACION... "
                CALL impresion(pos)
             ON KEY (INTERRUPT)
				    CALL Inicializa()
                EXIT DISPLAY
          END DISPLAY
			 
          CLOSE WINDOW ventana_2
       ELSE
          ERROR " --->ARCHIVO... VACIO "
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
			 RETURN
       END IF
    END IF

	 DISPLAY BY NAME g_reg.codigo,
	                 g_reg.descripcion

	   IF int_flag = TRUE THEN
		   LET int_flag = FALSE
			ERROR " --->CONSULTA CANCELADA ..."
			SLEEP 2
			ERROR ""
			CLEAR FORM 
			CLEAR SCREEN
			RETURN
		ELSE
		   PROMPT " Con <ENTER> Sale de la Consulta" for aux_pausa
		END IF
		
	 CLEAR FORM	
    CLEAR SCREEN
END FUNCTION
------------------------------------------------------------------------------
FUNCTION  Modifica()

   LET pos = 2
	
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
		
      OPEN WINDOW ventana_2 AT 3,3 WITH FORM "TABM0972" ATTRIBUTE(BORDER)
		
      DISPLAY " (ENTER) Modifica                                            (CTRL-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
		
      DISPLAY "               Escoja con <ENTER> el Registro a Modificar                      " AT 2,1
		
      DISPLAY "              D I A G N O S T I C O     D E     A F O R E                      " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE
		
      CONSTRUCT cla_where ON codigo
                        FROM codigo
         ON KEY (CONTROL-M)
            ERROR " --->PROCESANDO INFORMACION... "
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR " --->PROCESO CANCELADO... "
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = " SELECT * ",
		                " FROM tab_diag_afore ",
							 " WHERE ",cla_where CLIPPED,
				          " ORDER BY 1 "

		PREPARE query1 FROM sel_where

		DECLARE cursor_2 CURSOR FOR query1

		LET pos = 1

	   FOREACH cursor_2 INTO l_record[pos].*
	      LET pos = pos + 1
	   END FOREACH

	   INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""
			
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.codigo      = l_record[pos].codigo
               LET g_reg.descripcion = l_record[pos].descripcion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR " --->DEBE ELEGIR UN REGISTRO. "
               LET pos = ARR_CURR()
         END DISPLAY
			
         CLOSE WINDOW ventana_2
      ELSE
         ERROR " --->ARCHIVO... VACIO "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF
		
      DISPLAY "" AT 1,1
		DISPLAY "" AT 2,1
		DISPLAY " (CTRL-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
		DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)
								

		INPUT BY NAME g_reg.codigo,
		              g_reg.descripcion WITHOUT DEFAULTS

      BEFORE FIELD codigo
        NEXT FIELD descripcion
			
      AFTER FIELD descripcion
         IF g_reg.descripcion IS NULL THEN
            ERROR " --->LA DESCRIPCION NO DEBE SER NULO."
            NEXT FIELD descripcion
         END IF

         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            UPDATE tab_diag_afore SET
                   descripcion = g_reg.descripcion
            WHERE  codigo = g_reg.codigo
				
            ERROR " --->REGISTRO MODIFICADO "
            SLEEP 2
            ERROR ""
            CALL Inicializa()
         ELSE
            ERROR " --->PROCESO DE MODIFICAR CANCELADO. "
            SLEEP 2
            ERROR ""
         END IF
			
      EXIT INPUT
		
      ON KEY (INTERRUPT)
         CALL Inicializa()
         EXIT INPUT
      END INPUT
   ELSE
      ERROR " --->ARCHIVO DE INFORMES... VACIO. "
	END IF
   CLEAR SCREEN
END FUNCTION
------------------------------------------------------------------------------
FUNCTION Elimina()

   LET pos = 2
	
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
		
      OPEN WINDOW ventana_2 AT 3,3 WITH FORM "TABM0972" ATTRIBUTE(BORDER)
		
      DISPLAY " (ENTER) Elimina                                             (CTRL-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
		
      DISPLAY "               Escoja con <ENTER> el Registro a Eliminar                       " AT 2,1
		
      DISPLAY "              D I A G N O S T I C O    D E     A F O R E                       " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

	   LET int_flag = FALSE
	
      CONSTRUCT cla_where ON codigo
                        FROM codigo
         ON KEY (CONTROL-M)
            ERROR " --->PROCESANDO INFORMACION... "
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR " --->PROCESO CANCELADO... "
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF
		
		LET sel_where = " SELECT * ",
		                " FROM tab_diag_afore ",
							 " WHERE ",cla_where CLIPPED,
				          " ORDER BY 1 "

      PREPARE query2 FROM sel_where
		
      DECLARE cursor_3 CURSOR FOR query2
		
      LET pos = 1
		
      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
			ERROR ""
			
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.codigo      = l_record[pos].codigo
               LET g_reg.descripcion = l_record[pos].descripcion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR " --->DEBE ELEGIR UN REGISTRO. "
               LET pos = ARR_CURR()
         END DISPLAY
			
         CLOSE WINDOW ventana_2
      ELSE
         ERROR " --->ARCHIVO... VACIO "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF
																					
		DISPLAY "" AT 1,1
		DISPLAY "" AT 2,1
		DISPLAY " (CTRL-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
		DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME g_reg.codigo,
		                g_reg.descripcion
							 
      CALL Pregunta()
			
      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM tab_diag_afore
         WHERE codigo = g_reg.codigo
			
         ERROR " --->REGISTRO ELIMINADO. "
         SLEEP 2
         ERROR ""
      ELSE
         ERROR " --->PROCESO CANCELADO. "
         SLEEP 2
         ERROR ""
      END IF
		
      CALL Inicializa()
   ELSE
      ERROR " --->ARCHIVO... VACIO. "
   END IF
   CLEAR SCREEN
END FUNCTION
------------------------------------------------------------------------------
FUNCTION Pregunta()
   PROMPT " --->ESTA SEGURO S/N ? " FOR CHAR aux_pausa
END FUNCTION
------------------------------------------------------------------------------
FUNCTION impresion(pos)

   DEFINE i, pos SMALLINT

	LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",
		           usuario CLIPPED,".IMPDIAG",
					  hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabtipinfo To g_impre

   FOR i = 1 TO (pos+1)
      LET g_reg.codigo        = l_record[i].codigo
      LET g_reg.descripcion   = l_record[i].descripcion
		
      IF g_reg.codigo IS NULL THEN
         EXIT FOR
      END IF
		
      OUTPUT TO REPORT rpt_tabtipinfo(g_reg.*)
   END FOR
	
   FINISH REPORT rpt_tabtipinfo
	
   ERROR " --->LISTADO GENERADO..."
   SLEEP 2
   ERROR ""
	
   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
------------------------------------------------------------------------------
REPORT rpt_tabtipinfo(g_reg)

   DEFINE g_reg           RECORD
          codigo          CHAR(2),
          descripcion     CHAR(120),
          factualiza      DATE,
          usuario         CHAR(8)
   END RECORD
	
   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   88
		
   FORMAT
      PAGE HEADER
		   PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
		   PRINT COLUMN 01,'\033(s13H\033(s7B'
         PRINT COLUMN 02," TABM097 ",
               COLUMN 128,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE
       
		   PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'
			
         PRINT COLUMN 28," CATALOGO DIAGNOSTICO DE AFORE "
			
			PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'
			SKIP 2 LINE
						 
         PRINT COLUMN 05,"CODIGO",
               COLUMN 15,"DESCRIPCION"
         SKIP 1 LINE
			
      ON EVERY ROW
		   PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'
         PRINT COLUMN 05,g_reg.codigo ,
               COLUMN 15,g_reg.descripcion
					
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
			
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
------------------------------------------------------------------------------
