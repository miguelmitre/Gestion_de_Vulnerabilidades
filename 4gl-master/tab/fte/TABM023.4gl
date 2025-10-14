################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => E.F.P             				       #
#Programa TABM023  => CATALOGO ARCHIVO DE VALOR - COMISION                     #
#Fecha             => 20 Junio de 2000.  				       #
#By                => MIGUEL ANGEL HERNANDEZ MARTINEZ.                         #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS
        DEFINE g_param_dis            RECORD LIKE seg_modulo.*

        --DEFINE g_reg                  RECORD LIKE dis_val_comision.*
        DEFINE g_reg RECORD 
	       tipo_comision        SMALLINT,       
	       subcuenta            SMALLINT,       
	       val_porcentaje       DECIMAL(16,6),  
	       val_pesos            DECIMAL(16,6),  
	       siefore              SMALLINT,       
	       porcentaje_sie       DECIMAL(5,2),   
	       antiguedad_desde     INTEGER,        
	       antiguedad_hasta     INTEGER        
	END RECORD

        --DEFINE reg      ARRAY[1000] OF RECORD LIKE dis_val_comision.*
        DEFINE reg ARRAY[1000] OF RECORD 
	       tipo_comision        SMALLINT,       
	       subcuenta            SMALLINT,       
	       val_porcentaje       DECIMAL(16,6),  
	       val_pesos            DECIMAL(16,6),  
	       siefore              SMALLINT,       
	       porcentaje_sie       DECIMAL(5,2),   
	       antiguedad_desde     INTEGER,        
	       antiguedad_hasta     INTEGER        
	END RECORD

        DEFINE l_record ARRAY[1000] OF RECORD 
	       tipo_comision        SMALLINT,       
	       subcuenta            SMALLINT,       
	       val_porcentaje       DECIMAL(16,6),  
	       val_pesos            DECIMAL(16,6),  
	       siefore              SMALLINT,       
	       porcentaje_sie       DECIMAL(5,2),   
	       antiguedad_desde     INTEGER,        
	       antiguedad_hasta     INTEGER        
	END RECORD

	DEFINE HOY		DATE,
               aux_pausa        CHAR(1),
               pos              SMALLINT,
               seg_usuario          CHAR(08),
               cla_where        CHAR(500),
               sel_where        CHAR(500),
               g_lista          CHAR(300),
               g_impre          CHAR(300)

        DEFINE total_porcentaje SMALLINT,
               porcentaje       SMALLINT,
               campo_max        SMALLINT
       
        DEFINE arr_c            ,
               scr_l            SMALLINT            
END GLOBALS
###############################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT

        CALL inicio()
        CALL proceso()
END MAIN
###############################################################################
FUNCTION inicio()

        SELECT USER
        INTO   seg_usuario
        FROM   glo_parametro

        SELECT ruta_listados
        INTO   g_param_dis.ruta_listados
        FROM   seg_modulo
        WHERE modulo_cod = 'tab'
END FUNCTION
##############################################################################
FUNCTION proceso()    

	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0231" ATTRIBUTE(BORDER)
	DISPLAY " TABM023                 CATALOGO DE VALOR - COMISION                          " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO COMISIONES"
	   COMMAND "Agrega" "Agrega Comisiones"
	      CALL Agrega()
              CLEAR SCREEN
           COMMAND "Consulta" "Consulta Comisiones"
	      CALL Consulta()
              CLEAR SCREEN
           COMMAND "Modifica" "Modifica Comisiones"
	      CALL Modifica()
              CLEAR SCREEN
           COMMAND "Elimina" "Elimina Comisiones"
	      CALL Elimina()
              CLEAR SCREEN
           COMMAND "Salir" "Salir del Programa"
	      EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION Inicializa()
    DEFINE i                                SMALLINT     

    INITIALIZE g_reg.* TO NULL
    DISPLAY BY NAME g_reg.*

    INITIALIZE reg TO NULL                         
    FOR i = 1 TO 10                                      
       DISPLAY reg[i].* TO scr_1[i].*             
    END FOR                                              

END FUNCTION
################################################################################
FUNCTION Agrega()
        DEFINE i     SMALLINT
        DEFINE bandera  CHAR(01)
        DEFINE vsiefore  SMALLINT

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                               " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

	INPUT ARRAY  reg FROM scr_1.*
            BEFORE FIELD tipo_comision
                LET arr_c = ARR_CURR() 
                LET scr_l = SCR_LINE() 

	     AFTER FIELD tipo_comision
		    IF reg[arr_c].tipo_comision IS NULL THEN
		       ERROR "Tipo de Comision no puede ser nulo"
		       NEXT FIELD  tipo_comision
		    END IF

              AFTER FIELD subcuenta
		     IF reg[arr_c].subcuenta IS NULL THEN
		        ERROR "Codigo de Subcuenta NO puede ser nulo"
		        NEXT FIELD  subcuenta
		     END IF 

	      AFTER FIELD val_porcentaje
		    IF reg[arr_c].val_porcentaje IS NULL THEN
		       ERROR "Valor de Porcentaje no puede ser nulo"
		       NEXT FIELD  val_porcentaje
		    END IF

                    IF total_porcentaje > 100 THEN
                        ERROR "Sobre pasa el porcentaje..."
                        NEXT FIELD val_porcentaje
                    END IF

              AFTER FIELD val_pesos
		    IF reg[arr_c].val_pesos IS NULL THEN
		       ERROR "Valor en pesos NO puede ser nulo"
		       NEXT FIELD  val_pesos
		    END IF 


	      AFTER FIELD siefore
		    IF reg[arr_c].siefore IS NULL THEN
		       ERROR "Siefore no puede ser nulo"
		       NEXT FIELD  siefore
		    END IF
                    
                    SELECT SUM(val_porcentaje)
                    INTO   porcentaje
                    FROM   dis_val_comision
                    WHERE  tipo_comision = reg[arr_c].tipo_comision
                    AND    subcuenta     = reg[arr_c].subcuenta
                    AND    siefore       = reg[arr_c].siefore

                    LET total_porcentaje = reg[arr_c].val_porcentaje + porcentaje

                    IF total_porcentaje > 100 THEN
                        ERROR "Sobre pasa el porcentaje..."
                        NEXT FIELD siefore
                    END IF

                    SELECT MAX(antiguedad_hasta)
                    INTO   campo_max
                    FROM   dis_val_comision
                    WHERE  tipo_comision = reg[arr_c].tipo_comision
                    AND    subcuenta     = reg[arr_c].subcuenta
                    AND    siefore       = reg[arr_c].siefore

                    IF campo_max IS NOT NULL THEN
                        LET reg[arr_c].antiguedad_desde = campo_max + 1
                        DISPLAY reg[arr_c].antiguedad_desde TO scr_1[scr_l].antiguedad_desde     
                        NEXT FIELD antiguedad_hasta 
                    ELSE
                        IF arr_c = 1 THEN 
                            LET reg[arr_c].antiguedad_desde = 0
                            NEXT FIELD antiguedad_hasta 
                        ELSE 
                            LET reg[arr_c].antiguedad_desde = reg[arr_c-1].antiguedad_hasta + 1
                            NEXT FIELD antiguedad_hasta 
                        END IF 

                    END IF

	      AFTER FIELD antiguedad_hasta
		    IF reg[arr_c].antiguedad_hasta IS NULL THEN
		       ERROR "Antiguedad hasta no puede ser nulo"
		       NEXT FIELD  antiguedad_hasta
		    END IF

                    IF reg[arr_c].antiguedad_desde > reg[arr_c].antiguedad_hasta THEN   
                        ERROR "El Hasta DEBE ser MAYOR que el desde"
                        NEXT FIELD antiguedad_hasta 
                    END IF 

                    INSERT INTO dis_val_comision VALUES ( reg[arr_c].*,TODAY ) 

              ON KEY ( ESC )

		    ERROR "REGISTRO(S) INGRESADO(S)" 
                    SLEEP 1
		    ERROR ""

                    CALL Inicializa()
                    CLEAR SCREEN
                    EXIT INPUT
                 ON KEY (INTERRUPT)
                   LET i = ARR_CURR()                                       
                   LET reg[i].tipo_comision = NULL 
                   DISPLAY reg[i].tipo_comision TO scr_1[i].tipo_comision 

                   FOR i = 1 TO 100
                       IF reg[i].tipo_comision IS NOT NULL THEN
                           DELETE FROM dis_val_comision
                           WHERE tipo_comision  = reg[i].tipo_comision
                           AND   subcuenta      = reg[i].subcuenta
                           AND   val_porcentaje = reg[i].val_porcentaje
                           AND   val_pesos      = reg[i].val_pesos
                           AND   antiguedad_desde = reg[i].antiguedad_desde
                           AND   antiguedad_hasta = reg[i].antiguedad_hasta
                       ELSE
                           EXIT FOR
                       END IF
                   END FOR

                    CALL Inicializa()
                    EXIT INPUT
	END INPUT
END FUNCTION
################################################################################
FUNCTION Consulta()
         LET pos = 2 
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 3,3 WITH FORM "TABM0231" ATTRIBUTE(BORDER)
	    DISPLAY " (ENTER) Consulta                (Ctrl-p) Impresion          (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                             C O M I S I O N E S                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

            LET int_flag = FALSE

            CONSTRUCT cla_where ON   tipo_comision,
                                     subcuenta
                                FROM tipo_comision,
                                     subcuenta
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
               SLEEP 1
               ERROR ""
               CLEAR SCREEN
               CLOSE WINDOW ventana_2
               RETURN
            END IF

            LET sel_where = "SELECT tipo_comision,",
	                           "subcuenta,",
	                           "val_porcentaje,",   
	                           "val_pesos,",
	                           "siefore,",
	                           "porcentaje_sie,",
	                           "antiguedad_desde,",
	                           "antiguedad_hasta",
	                     " FROM dis_val_comision WHERE ",
                             cla_where CLIPPED,
                            "ORDER BY 1,2,5,7 "
  
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
               SLEEP 1
               CLOSE WINDOW ventana_2
	    END IF
        END IF
END FUNCTION
################################################################################
FUNCTION  Modifica()
    DEFINE i       SMALLINT

    LET pos = 2
    IF (pos-1) >= 1 THEN
    CALL  SET_COUNT(pos-1)
    OPEN WINDOW ventana_2 AT 3,3 WITH FORM "TABM0231" ATTRIBUTE( BORDER)
    DISPLAY " (ENTER) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
    --DISPLAY "                 Escoja con < ENTER > la comision a modificar                  " AT 2,1 
    DISPLAY "                               C O M I S I O N E S                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

    LET int_flag = FALSE

    CONSTRUCT cla_where ON   tipo_comision,
                             subcuenta
                        FROM tipo_comision,
                             subcuenta
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
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW ventana_2
            RETURN
        END IF

            LET sel_where = "SELECT tipo_comision,",
	                           "subcuenta,",
	                           "val_porcentaje,",   
	                           "val_pesos,",
	                           "siefore,",
	                           "porcentaje_sie,",
	                           "antiguedad_desde,",
	                           "antiguedad_hasta",
	                     " FROM dis_val_comision WHERE ",
                        cla_where CLIPPED,
                        "ORDER BY 1,2,5,7 "
  
        PREPARE query1 FROM sel_where

        DECLARE cursor_2 CURSOR FOR query1

        LET pos = 1
        FOREACH cursor_2 INTO l_record[pos].*
            LET pos = pos +1
        END FOREACH

        IF (pos-1) >= 1 THEN
            CALL SET_COUNT(pos-1) 
            CLOSE WINDOW ventana_2

            OPEN WINDOW ventana_3 AT 3,3 WITH FORM "TABM0231" ATTRIBUTE( BORDER)
	    DISPLAY " (ESC) Modifica                  (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
            DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                               C O M I S I O N E S                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

	    INPUT ARRAY l_record WITHOUT DEFAULTS FROM scr_1.*

                BEFORE FIELD tipo_comision
                    LET arr_c = ARR_CURR()      
                    LET scr_l = SCR_LINE()      
      		    NEXT FIELD val_porcentaje

	        AFTER FIELD val_porcentaje
		    IF l_record[arr_c].val_porcentaje IS NULL THEN
		        ERROR "Valor de Porcentaje no puede ser nulo"
		        NEXT FIELD  val_porcentaje
		    END IF

                AFTER FIELD val_pesos
		    IF l_record[arr_c].val_pesos IS NULL THEN
		        ERROR "Valor en pesos NO puede ser nulo"
		        NEXT FIELD  val_pesos
		    END IF 
                       
                ON KEY (ESC)

                    CALL Pregunta()

                    IF aux_pausa MATCHES "[Ss]" THEN
                        LET i = ARR_CURR()
                                                             
                        FOR i = 1 TO 100 
                            IF l_record[i].tipo_comision IS NOT NULL THEN 
                                UPDATE dis_val_comision SET
                                   val_porcentaje =l_record[i].val_porcentaje,
                                   val_pesos      =l_record[i].val_pesos
		                WHERE tipo_comision   =l_record[i].tipo_comision
                                AND   subcuenta       =l_record[i].subcuenta
                                AND   siefore         =l_record[i].siefore
                                AND   antiguedad_desde=l_record[i].antiguedad_desde
                                AND   antiguedad_desde=l_record[i].antiguedad_desde
                            ELSE
                                EXIT FOR
                            END IF 
                        END FOR

		        ERROR "REGISTRO MODIFICADO" 

                        CALL Inicializa()
                    ELSE
                        ERROR "PROCESO DE MODIFICACION,CANCELADO"
                    END IF
                    SLEEP 1
                    ERROR ""
		    EXIT INPUT
	        ON KEY ( INTERRUPT )
                    CALL Inicializa()
                    EXIT INPUT
  	    END INPUT
	ELSE
	    ERROR "ARCHIVO DE CIUDADES.... VACIO"
	END IF
     CLOSE WINDOW ventana_3
     END IF
END FUNCTION
################################################################################
FUNCTION Elimina()
    DEFINE i        SMALLINT

    LET pos = 2
    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)
	OPEN WINDOW ventana_2 AT 3,3 WITH FORM "TABM0231" ATTRIBUTE( BORDER)
        DISPLAY " (ENTER) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY "                               C O M I S I O N E S                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)


        CONSTRUCT cla_where ON   tipo_comision,
                                 subcuenta
                            FROM tipo_comision,
                                 subcuenta
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
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW ventana_2
            RETURN
        END IF

            LET sel_where = "SELECT tipo_comision,",
	                           "subcuenta,",
	                           "val_porcentaje,",   
	                           "val_pesos,",
	                           "siefore,",
	                           "porcentaje_sie,",
	                           "antiguedad_desde,",
	                           "antiguedad_hasta",
	                     " FROM dis_val_comision WHERE ",
                         cla_where CLIPPED,
                         "ORDER BY 1,2,5,7 "
  
        PREPARE query2 FROM sel_where

        DECLARE cursor_3 CURSOR FOR query2

        LET pos = 1
        FOREACH cursor_3 INTO l_record[pos].*
            LET pos = pos +1
        END FOREACH

        INITIALIZE l_record[pos].* TO NULL

        IF (pos-1) >= 1 THEN
            CALL SET_COUNT(pos-1) 

    	    DISPLAY "" AT 1,1
	    DISPLAY "" AT 2,1
	    DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
            DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                  Escoja con < ENTER > la comision a eliminar                  " AT 2,1 

	    DISPLAY ARRAY l_record TO scr_1.*

                ON KEY (control-m)
                    LET i = ARR_CURR()                                       

                    SELECT MAX(antiguedad_desde)
                    INTO   campo_max
                    FROM   dis_val_comision
                    WHERE  tipo_comision    = l_record[i].tipo_comision
                    AND    subcuenta        = l_record[i].subcuenta
                    AND    siefore          = l_record[i].siefore

                    IF l_record[i].antiguedad_desde = campo_max THEN
                        LET g_reg.tipo_comision   =l_record[i].tipo_comision
                        LET g_reg.subcuenta       =l_record[i].subcuenta
                        LET g_reg.val_porcentaje  =l_record[i].val_porcentaje
                        LET g_reg.val_pesos       =l_record[i].val_pesos
                        LET g_reg.siefore         =l_record[i].siefore
                        LET g_reg.antiguedad_desde=l_record[i].antiguedad_desde
                        LET g_reg.antiguedad_hasta=l_record[i].antiguedad_hasta
                    ELSE
                        ERROR "DEBE SER EL ULTIMO REGISTRO.."
                        SLEEP 2
                        ERROR ""
                        EXIT DISPLAY
                    END IF

                    CALL Pregunta()

                    IF aux_pausa MATCHES "[Ss]" THEN
                        DELETE FROM dis_val_comision
                        WHERE tipo_comision    = g_reg.tipo_comision
                        AND   subcuenta        = g_reg.subcuenta
                        AND   siefore          = g_reg.siefore
                        AND   antiguedad_desde = g_reg.antiguedad_desde

                        ERROR "REGISTRO ELIMINADO" 
                        SLEEP 1
                    ELSE
                        ERROR "ELIMINAR CANCELADO" 
                        SLEEP 1
                    END IF
                    EXIT DISPLAY

            END DISPLAY
            ERROR ""
            CALL Inicializa()
        ELSE
	    ERROR "ARCHIVO DE CIUDADES.... VACIO"
            SLEEP 1
	END IF
    END IF
    CLOSE WINDOW ventana_2
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                 ".IMPVAL_COM",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_val_comision TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.tipo_comision     =  l_record[i].tipo_comision
       LET g_reg.subcuenta         =  l_record[i].subcuenta
       LET g_reg.val_porcentaje    =  l_record[i].val_porcentaje
       LET g_reg.val_pesos         =  l_record[i].val_pesos
       LET g_reg.siefore           =  l_record[i].siefore
       LET g_reg.antiguedad_desde  =  l_record[i].antiguedad_desde
       LET g_reg.antiguedad_hasta  =  l_record[i].antiguedad_hasta
   
       IF g_reg.tipo_comision IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_val_comision(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_val_comision

   ERROR "LISTADO GENERADO..."
   SLEEP 1
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_val_comision(g_reg)

   --DEFINE g_reg                  RECORD LIKE dis_val_comision.*
        DEFINE g_reg  RECORD 
	       tipo_comision        SMALLINT,       
	       subcuenta            SMALLINT,       
	       val_porcentaje       DECIMAL(16,6),  
	       val_pesos            DECIMAL(16,6),  
	       siefore              SMALLINT,       
	       porcentaje_sie       DECIMAL(5,2),   
	       antiguedad_desde     INTEGER,        
	       antiguedad_hasta     INTEGER        
	END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM023 ",
               COLUMN 24," LISTADO DE CATALOGO DE VALOR-COMISION ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 02,"TIPO",
               COLUMN 08,"SUBCUENTA",
               COLUMN 19,"PORCENTAJE",
               COLUMN 38,"PESOS",
               COLUMN 48,"SIEFORE",
               COLUMN 58,"DESDE",
               COLUMN 65,"HASTA"
               
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 3,g_reg.tipo_comision     USING "###",
               COLUMN 11,g_reg.subcuenta        USING "##",
               COLUMN 18,g_reg.val_porcentaje   USING "##&.######",
               COLUMN 30,g_reg.val_pesos        USING "########&.######",
               COLUMN 48,g_reg.siefore          USING "#####",
               COLUMN 58,g_reg.antiguedad_desde USING "#####",
               COLUMN 65,g_reg.antiguedad_hasta USING "#####"
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
--------------------------------------------------------------------------------
{
           INITIALIZE l_record[pos].* TO NULL

           IF (pos-1) >= 1 THEN
              CALL SET_COUNT(pos-1) 
              DISPLAY ARRAY l_record TO scr_1.* 
                 ON KEY (CONTROL-M)
                    LET pos = ARR_CURR()
	            LET g_reg.tipo_comision    =  l_record[pos].tipo_comision
                    LET g_reg.subcuenta        =  l_record[pos].subcuenta
                    LET g_reg.val_porcentaje   =  l_record[pos].val_porcentaje
                    LET g_reg.val_pesos        =  l_record[pos].val_pesos
                    LET g_reg.siefore          =  l_record[pos].siefore
                    LET g_reg.antiguedad_desde =  l_record[pos].antiguedad_desde
                    LET g_reg.antiguedad_hasta =  l_record[pos].antiguedad_hasta
                    EXIT DISPLAY
                 ON KEY (INTERRUPT)
		    ERROR "Usted debe escojer un registro"
                    LET pos = ARR_CURR()
	      END DISPLAY
}
--------------------------------------------------------------------------------
