################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => Carlos Welsh. 					       #
#Programa TABM022  => CATALOGO ARCHIVO DE CUENTA-BANCO.
#Fecha             => 27 Noviembre 1996. 				       #
#By                => JUAN DAVID HERNANDEZ OYARCE. 			       #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ.                         #
#Fecha modifica    => 16 Diciembre 1999.                                       #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af

GLOBALS

        DEFINE g_param_dis            RECORD LIKE seg_modulo.*

        DEFINE g_reg                  RECORD LIKE pro_cta_banco.*

        DEFINE l_record ARRAY[300] OF RECORD LIKE pro_cta_banco.*

	DEFINE HOY		DATE,
               aux_pausa        CHAR(01),
               pos              SMALLINT,
               seg_usuario          CHAR(08),
               cla_where        CHAR(200),
               sel_where        CHAR(200),
               g_lista          CHAR(300),
               g_impre          CHAR(300),
               hora             CHAR(08)
END GLOBALS
#########################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
     
        CALL inicio()
        CALL proceso()
END MAIN
#########################################################################
FUNCTION inicio()
       SELECT USER 
       INTO   seg_usuario
       FROM   glo_parametro
 
       SELECT ruta_listados
       INTO   g_param_dis.ruta_listados
       FROM   seg_modulo
       WHERE modulo_cod = 'tab'
END FUNCTION
##########################################################################
FUNCTION proceso()

	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0221" ATTRIBUTE( BORDER)
	DISPLAY " TABM022              MANTENIMIENTO A CUENTA - BANCO                                     " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO CUENTAS"
	   COMMAND "Agrega" "Agrega Cuentas"
	      CALL Agrega()
              CLEAR SCREEN
           COMMAND "Consulta" "Consulta Cuentas"
	      CALL Consulta()
              CLEAR SCREEN
           COMMAND "Modifica" "Modifica Cuentas"
	      CALL Modifica()
              CLEAR SCREEN
           COMMAND "Elimina" "Elimina Cuentas"
	      CALL Elimina()
              CLEAR SCREEN
           COMMAND "Salir" "Salir del Programa"
	      EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION Inicializa()
	INITIALIZE g_reg.* TO NULL
	DISPLAY BY NAME g_reg.*
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                               " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

	INPUT BY NAME  g_reg.*
	      AFTER FIELD siefore
		    IF g_reg.siefore IS NULL THEN
		       ERROR "Codigo de Siefore no puede ser nulo"
		       NEXT FIELD  siefore
		    END IF
              AFTER FIELD banco
		     IF g_reg.banco IS NULL THEN
		        ERROR "Codigo de Banco NO puede ser nulo"
		        NEXT FIELD  banco
		     END IF 
	      AFTER FIELD plaza
		    IF g_reg.plaza IS NULL THEN
		       ERROR "Codigo de Plaza no puede ser nulo"
		       NEXT FIELD  plaza
		    END IF
              AFTER FIELD sucursal
		     IF g_reg.sucursal IS NULL THEN
		        ERROR "Codigo de Sucursal NO puede ser nulo"
		        NEXT FIELD  sucursal
		     END IF 
              AFTER FIELD n_cuenta
		     IF g_reg.n_cuenta IS NULL THEN
		        ERROR "Codigo de cuenta NO puede ser nulo"
		        NEXT FIELD  n_cuenta
		     END IF 
              ON KEY ( ESC )
		    IF g_reg.siefore IS NULL THEN
		       ERROR "Codigo de Siefore no puede ser nulo"
		       NEXT FIELD  siefore
		    END IF

		    IF g_reg.banco IS NULL THEN
		       ERROR "Codigo de Banco NO puede ser nulo"
		       NEXT FIELD  banco
		    END IF 

		    IF g_reg.plaza IS NULL THEN
		       ERROR "Codigo de Plaza no puede ser nulo"
		       NEXT FIELD  plaza
		    END IF

		    IF g_reg.sucursal IS NULL THEN
		       ERROR "Codigo de Sucursal NO puede ser nulo"
		       NEXT FIELD  sucursal
		    END IF 

		    IF g_reg.n_cuenta IS NULL THEN
		       ERROR "Codigo de cuenta NO puede ser nulo"
		       NEXT FIELD  n_cuenta
		    END IF 

		    SELECT "X" 
                    FROM pro_cta_banco
		    WHERE siefore  = g_reg.siefore
                    AND   banco    = g_reg.banco
                    AND   plaza    = g_reg.plaza
                    AND   sucursal = g_reg.sucursal
                    AND   n_cuenta = g_reg.n_cuenta

		    IF STATUS <> NOTFOUND THEN
		       ERROR "Cuenta ya Ingresada"
		       NEXT FIELD n_cuenta
		    END IF

                    INSERT INTO pro_cta_banco VALUES ( g_reg.* ) 

		    ERROR "REGISTRO INGRESADO" 
                    SLEEP 2
		    ERROR ""

                    CALL Inicializa()
		    NEXT FIELD siefore
                 ON KEY (INTERRUPT)
                    CALL Inicializa()
                    EXIT INPUT
	END INPUT
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Consulta()
         LET pos = 2
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0222" ATTRIBUTE( BORDER)
	    DISPLAY " (ENTER) Consulta            (Ctrl-p) Impresion              (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                           C  U  E  N  T  A  S                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   	
            LET int_flag = FALSE

            CONSTRUCT cla_where ON   siefore ,
                                     banco, 
                                     plaza,
                                     sucursal,
                                     n_cuenta
                                FROM siefore,
                                     banco,
                                     plaza,
                                     sucursal,
                                     n_cuenta
               ON KEY (control-m)
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

            LET sel_where = "SELECT * FROM pro_cta_banco WHERE ",
                             cla_where CLIPPED,
                            "ORDER BY 1 "
  
            PREPARE query FROM sel_where

            DECLARE cursor_1 CURSOR FOR query

            LET pos = 1
            FOREACH cursor_1 INTO l_record[pos].*
               LET pos = pos +1
            END FOREACH

            INITIALIZE l_record[pos].* TO NULL

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1) 
               DISPLAY ARRAY l_record TO scr_1.*
                  ON KEY (control-p)
                     ERROR "PROCESANDO IMPRESION..."
                     CALL impresion(pos)
                  ON KEY (INTERRUPT)
                     EXIT DISPLAY
	       END DISPLAY

	       CLOSE WINDOW ventana_2
	    ELSE
	       ERROR "ARCHIVO DE dis_cuenta.... VACIO"
               SLEEP 2
               ERROR ""
               CLOSE WINDOW ventana_2
	    END IF
        END IF
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
        LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0222" ATTRIBUTE( BORDER)
           DISPLAY " (ENTER) Consulta                                       (Ctrl-c) Salir         " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                 Escoja con < ENTER > la cuenta a modificar                    " AT 2,1 
	   DISPLAY "                               C  U  E  N  T  A  S                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

           LET int_flag = FALSE

           CONSTRUCT cla_where ON   siefore ,
                                    banco, 
                                    plaza,
                                    sucursal,
                                    n_cuenta
                               FROM siefore,
                                    banco,
                                    plaza,
                                    sucursal,
                                    n_cuenta
              ON KEY (control-m)
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

           LET sel_where = "SELECT * FROM pro_cta_banco WHERE ",
                            cla_where CLIPPED,
                           "ORDER BY 1 "
  
           PREPARE query1 FROM sel_where

           DECLARE cursor_2 CURSOR FOR query1

           LET pos = 1
           FOREACH cursor_2 INTO l_record[pos].*
              LET pos = pos +1
           END FOREACH

           INITIALIZE l_record[pos].* TO NULL

           IF (pos-1) >= 1 THEN
              CALL SET_COUNT(pos-1) 
	      DISPLAY ARRAY l_record TO scr_1.* 
                 ON KEY (CONTROL-M)
                    LET pos = ARR_CURR()
	            LET g_reg.siefore  =  l_record[pos].siefore
                    LET g_reg.banco    =  l_record[pos].banco
                    LET g_reg.plaza    =  l_record[pos].plaza
                    LET g_reg.sucursal =  l_record[pos].sucursal
                    EXIT DISPLAY
                 ON KEY (INTERRUPT)
	            ERROR "Usted debe escojer un registro"
                    LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2

	      DISPLAY "" AT 1,1
	      DISPLAY "" AT 2,1
	      DISPLAY " ( Esc ) Modifica              (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
              DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)
	      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
                 BEFORE FIELD siefore
		       NEXT FIELD n_cuenta
                 AFTER FIELD n_cuenta
		       IF g_reg.banco IS NULL THEN
                          ERROR "Numero de Cuenta NO puede ser nulo"
                          NEXT FIELD  n_cuenta
                       END IF 

                 CALL Pregunta()

                 IF aux_pausa MATCHES "[Ss]" THEN
                    UPDATE pro_cta_banco SET
                           n_cuenta = g_reg.n_cuenta
		    WHERE siefore   = g_reg.siefore
                    AND   banco     = g_reg.banco
                    AND   plaza     = g_reg.plaza
                    AND   sucursal  = g_reg.sucursal

		    ERROR "REGISTRO MODIFICADO" 
                    SLEEP 2

                    CALL Inicializa()
                 ELSE 
                    ERROR "PROCESO DE MODIFICACION,CANCELADA"
                    SLEEP 2
                 END IF

                 ERROR ""
		 EXIT INPUT
	      ON KEY ( INTERRUPT )
                    CALL Inicializa()
                    EXIT INPUT
  	   END INPUT
	ELSE
	   ERROR "ARCHIVO DE CIUDADES.... VACIO"
	END IF
    END IF
END FUNCTION
################################################################################
FUNCTION Elimina()
         LET pos = 2
	 IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0222" ATTRIBUTE( BORDER)
           DISPLAY " (ENTER) Consulta                                       (Ctrl-c) Salir         " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                 Escoja con < ENTER > la cuenta a eliminar                     " AT 2,1 
	   DISPLAY "                               C  U  E  N  T  A  S                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

   
           LET int_flag = FALSE

           CONSTRUCT cla_where ON   siefore ,
                                    banco, 
                                    plaza,
                                    sucursal,
                                    n_cuenta
                               FROM siefore,
                                    banco,
                                    plaza,
                                    sucursal,
                                    n_cuenta
              ON KEY (control-m)
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

           LET sel_where = "SELECT * FROM pro_cta_banco WHERE ",
                            cla_where CLIPPED,
                           "ORDER BY 1 "
  
           PREPARE query2 FROM sel_where

           DECLARE cursor_3 CURSOR FOR query2

           LET pos = 1
           FOREACH cursor_3 INTO l_record[pos].*
              LET pos = pos +1
           END FOREACH

           INITIALIZE l_record[pos].* TO NULL

           IF (pos-1) >= 1 THEN
              CALL SET_COUNT(pos-1) 
              DISPLAY ARRAY l_record TO scr_1.* 
                 ON KEY (CONTROL-M)
                    LET pos = ARR_CURR()
	            LET g_reg.siefore  =  l_record[pos].siefore
                    LET g_reg.banco    =  l_record[pos].banco
                    LET g_reg.plaza    =  l_record[pos].plaza
                    LET g_reg.sucursal =  l_record[pos].sucursal
                    LET g_reg.n_cuenta =  l_record[pos].n_cuenta
                    EXIT DISPLAY
                 ON KEY (INTERRUPT)
	            ERROR "Usted debe escojer un registro"
                    LET pos = ARR_CURR()
	       END DISPLAY
	       CLOSE WINDOW ventana_2

	       DISPLAY "" AT 1,1
	       DISPLAY "" AT 2,1
	       DISPLAY " ( Esc ) Elimina                (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
               DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
               DISPLAY BY NAME  g_reg.*

               CALL Pregunta()

               IF aux_pausa MATCHES "[Ss]" THEN
                  DELETE FROM pro_cta_banco
                  WHERE siefore  = g_reg.siefore
                  AND   banco    = g_reg.banco
                  AND   plaza    = g_reg.plaza
                  AND   sucursal = g_reg.sucursal
                  AND   n_cuenta = g_reg.n_cuenta

                  ERROR "REGISTRO ELIMINADO" 
                  SLEEP 2
               ELSE
                  ERROR "ELIMINAR CANCELADO" 
                  SLEEP 2
               END IF
               ERROR ""
               CALL Inicializa()
            ELSE
               ERROR "ARCHIVO DE CIUDADES.... VACIO"
            END IF
         END IF
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                 ".IMPCUENTA_B",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_cuenta_banco TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.siefore  =  l_record[i].siefore
       LET g_reg.banco    =  l_record[i].banco
       LET g_reg.plaza    =  l_record[i].plaza
       LET g_reg.sucursal =  l_record[i].sucursal
       LET g_reg.n_cuenta =  l_record[i].n_cuenta
   
       IF g_reg.siefore IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_cuenta_banco(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_cuenta_banco

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_cuenta_banco(g_reg)
   DEFINE g_reg                  RECORD LIKE pro_cta_banco.*

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM022 ",
               COLUMN 20," LISTADO DE CATALOGO DE dis_cuenta DE BANCOS ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"SIEFORE",
               COLUMN 15,"BANCO",
               COLUMN 30,"PLAZA",
               COLUMN 45,"SUCURSAL",
               COLUMN 60,"CUENTA"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 1,g_reg.siefore,
               COLUMN 15,g_reg.banco,
               COLUMN 30,g_reg.plaza,
               COLUMN 45,g_reg.sucursal,
               COLUMN 60,g_reg.n_cuenta
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
