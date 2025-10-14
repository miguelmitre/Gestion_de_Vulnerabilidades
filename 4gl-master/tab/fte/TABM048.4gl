################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.                                                   #
#Programa TABM048  =>                                                          #
#Fecha             =>                                                          #
#Por               =>                                    		       #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS
         DEFINE g_param_dis	RECORD LIKE dis_parametro.*

	 DEFINE aux_pausa	CHAR(1),
		sw_1		SMALLINT,
		usuario		CHAR(8),
		hoy		DATE,
		pos		INTEGER,
		sel_where	CHAR(300),
		cla_where	CHAR(300),
		g_impre		CHAR(300),
		g_lista		CHAR(300)

         DEFINE g_reg		RECORD 
                layout_cod      SMALLINT, 
                tipo_reg        SMALLINT, 
                campo_cod       SMALLINT, 
                campo_desc      CHAR(40), 
                tipo_dato       CHAR(1),  
                long            SMALLINT, 
                decimal         SMALLINT, 
                pos_ini         SMALLINT, 
                pos_fin         SMALLINT, 
                valor_cte       CHAR(10), 
                num_valida      SMALLINT
	 END RECORD
 
         DEFINE l_record	ARRAY[2000] OF RECORD
                layout_cod      SMALLINT, 
                tipo_reg        SMALLINT, 
                campo_cod       SMALLINT, 
                campo_desc      CHAR(40), 
                tipo_dato       CHAR(1),  
                long            SMALLINT, 
                decimal         SMALLINT, 
                pos_ini         SMALLINT, 
                pos_fin         SMALLINT, 
                valor_cte       CHAR(10), 
                num_valida      SMALLINT
	 END RECORD

         DEFINE l_reg   	ARRAY[1000] OF RECORD
                campo_cod       SMALLINT, 
                pos_ini         SMALLINT, 
                pos_fin         SMALLINT
	 END RECORD
END GLOBALS
#####################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY CONTROL-O

	DEFER INTERRUPT
        CALL inicio()
        CALL proceso()
END MAIN
#####################################################################
FUNCTION inicio()
   SELECT USER,*
   INTO   usuario
   FROM   dis_parametro

   SELECT ruta_spool
   INTO   g_param_dis.ruta_spool
   FROM   dis_parametro
END FUNCTION
#####################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0481" ATTRIBUTE( BORDER)
	DISPLAY " TABM048                                                                     " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CAMPOS   "
	   COMMAND "Agrega" "Agrega "
	       CALL Agrega()
           COMMAND "Consulta" "Consulta "
	       CALL Consulta()
           COMMAND "Modifica" "Modifica "
	       CALL Modifica()
           COMMAND "Elimina" "Elimina "
	       CALL Elimina()
           COMMAND "Salir" "Salir del Programa"
	       EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION Inicializa()
        LET sw_1 = 0
	----INITIALIZE g_reg.* TO NULL
         -- g_reg.layout_cod   
          LET g_reg.tipo_reg    = null
          LET g_reg.campo_cod  = null
          LET g_reg.campo_desc= null
          LET g_reg.tipo_dato = null
          LET g_reg.long    = null
          LET g_reg.decimal= null
          LET g_reg.pos_ini= null
          LET g_reg.pos_fin= null
          LET g_reg.valor_cte = null
          LET g_reg.num_valida= null
	DISPLAY BY NAME g_reg.*
END FUNCTION
################################################################################
FUNCTION Agrega()
 
        DEFINE vpos_fin       SMALLINT,
               vcampo_cod     SMALLINT

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

        LET g_reg.layout_cod = NULL
        LET sw_1 = 0
        LET vpos_fin = 0
        LET vcampo_cod = 0

	INPUT BY NAME  g_reg.*
	      BEFORE FIELD layout_cod	

	      AFTER FIELD layout_cod	
		    IF g_reg.layout_cod IS NULL THEN
		       ERROR "Codigo de layout_cod NO puede ser nulo"
		       NEXT FIELD  layout_cod	
		    END IF

              AFTER FIELD tipo_reg
		    IF g_reg.tipo_reg IS NULL THEN
		       ERROR "Tipo registro NO puede ser nulo"
		       NEXT FIELD  tipo_reg
		    END IF 
             
                    SELECT MAX(campo_cod)
                    INTO   vcampo_cod
                    FROM   tab_campo
                    WHERE  layout_cod = g_reg.layout_cod
                    AND    tipo_reg   = g_reg.tipo_reg
                   
                    IF vcampo_cod IS NULL THEN 
                       LET g_reg.campo_cod =  1         
                       DISPLAY BY NAME g_reg.campo_cod 
                       NEXT FIELD campo_desc                             
                    ELSE
                       LET g_reg.campo_cod = vcampo_cod + 1         
                       DISPLAY BY NAME g_reg.campo_cod 
                       NEXT FIELD campo_desc                             
                    END IF
              AFTER FIELD campo_desc
		    IF g_reg.campo_desc IS NULL THEN
		       ERROR "Descripcion de campo NO puede ser nulo"
		       NEXT FIELD  campo_desc
		    END IF 
              AFTER FIELD tipo_dato
		    IF g_reg.tipo_dato IS NULL THEN
		       ERROR "Tipo de dato NO puede ser nulo"
		       NEXT FIELD  tipo_dato
		    END IF 
              AFTER FIELD long

              AFTER FIELD decimal
                    SELECT MAX(pos_fin)
                    INTO   vpos_fin
                    FROM   tab_campo
                    WHERE  layout_cod = g_reg.layout_cod
                    AND    tipo_reg   = g_reg.tipo_reg
                   
                    IF vpos_fin IS NULL THEN 
                       LET g_reg.pos_ini =  1         
                       DISPLAY BY NAME g_reg.pos_ini 
                    ELSE
                       LET g_reg.pos_ini = vpos_fin + 1         
                       DISPLAY BY NAME g_reg.pos_ini 
                    END IF

                    LET g_reg.pos_fin = g_reg.long + g_reg.pos_ini - 1
                    DISPLAY BY NAME g_reg.pos_fin 
                    NEXT FIELD valor_cte                             

              AFTER FIELD valor_cte

              AFTER FIELD num_valida

	      ON KEY( ESC )
                    IF g_reg.pos_ini = 0  or g_reg.pos_ini is null THEN
                          ERROR "No tiene posicion inicial..."
                          NEXT FIELD long
                    END IF 
                    IF g_reg.pos_fin = 0  or g_reg.pos_fin is null THEN
                          ERROR "No tiene posicion final..."
                          NEXT FIELD long
                    END IF 
                       
                    INSERT INTO tab_campo 
                    VALUES (g_reg.*) 

		    ERROR "REGISTRO INGRESADO"
                    SLEEP 2
		    ERROR ""
                    CALL Inicializa()
		    NEXT FIELD tipo_reg  	

              ON KEY(INTERRUPT)
                    CALL Inicializa()
                    EXIT INPUT

              ON KEY(CONTROL-C)
                    CALL Inicializa()
                    EXIT INPUT
        END INPUT
END FUNCTION
################################################################################
FUNCTION Consulta()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0482" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta           (Ctrl-p) Imprimir            (Ctrl-C) Salir        " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                                                                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON   layout_cod,
                                    tipo_reg 
                               FROM layout_cod,
                                    tipo_reg
	      ON KEY(CONTROL-M)
		       ERROR "PROCESANDO INFORMACION..."
		       SLEEP 2
		       LET int_flag = FALSE
		       EXIT CONSTRUCT

	      ON KEY(CONTROL-C)
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

	   LET sel_where = "SELECT * FROM tab_campo WHERE ",
                            cla_where CLIPPED,
			   "ORDER BY 1,2,3 "

	   PREPARE query FROM sel_where

	   DECLARE cursor_1 CURSOR FOR query

   	   LET pos = 1

	   FOREACH cursor_1 INTO l_record[pos].*
              LET pos = pos+1
           END FOREACH             

	   INITIALIZE l_record[pos].* TO NULL

	   IF (pos-1) >= 1 THEN
	      CALL  SET_COUNT(pos-1)
              ERROR ""	
	      DISPLAY ARRAY l_record TO scr_1.*
		  ON KEY(CONTROL-P)
                     ERROR "PROCESANDO INFORMACION..."
--		     CALL impresion(pos)
                  ON KEY(INTERRUPT)
                     EXIT DISPLAY
                  ON KEY(CONTROL-C)
                     EXIT DISPLAY
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE ... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
	   END IF
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
        DEFINE vpos_fin      SMALLINT,
               vlong         SMALLINT,
               vvlong        SMALLINT,
               pos_fin_ant   SMALLINT,
               campo_cod_ant SMALLINT

	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0482" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta                                        (Ctrl-C) Salir       " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "          Escoja con <ENTER> el                    a modificar                " AT 2,1
	   DISPLAY "                                                                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON layout_cod,
                                  tipo_reg 
                             FROM layout_cod,
                                  tipo_reg
	      ON KEY(CONTROL-M)
		ERROR " PROCESANDO INFORMACION....."
		LET int_flag = FALSE
		EXIT CONSTRUCT

	      ON KEY(CONTROL-C)
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

	   LET sel_where = "SELECT * FROM tab_campo WHERE ",
                           cla_where CLIPPED,
			   "ORDER BY 1,2,3 "

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
		 ON KEY(CONTROL-M)
		    LET pos = ARR_CURR()
                    LET g_reg.layout_cod = l_record[pos].layout_cod
                    LET g_reg.tipo_reg   = l_record[pos].tipo_reg
                    LET g_reg.campo_cod  = l_record[pos].campo_cod
                    LET g_reg.campo_desc = l_record[pos].campo_desc
                    LET g_reg.tipo_dato  = l_record[pos].tipo_dato
                    LET g_reg.long       = l_record[pos].long
                    LET g_reg.decimal    = l_record[pos].decimal
                    LET g_reg.pos_ini    = l_record[pos].pos_ini
                    LET g_reg.pos_fin    = l_record[pos].pos_fin
                    LET g_reg.valor_cte  = l_record[pos].valor_cte
                    LET g_reg.num_valida = l_record[pos].num_valida
		    EXIT DISPLAY

                 ON KEY(CONTROL-C)
                    ERROR "Debe elegir un registro."
		    LET pos = ARR_CURR()

                 ON KEY(INTERRUPT)
                    ERROR "Debe elegir un registro."
		    LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE ... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	   DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

	   INPUT BY NAME g_reg.* WITHOUT DEFAULTS
              BEFORE FIELD layout_cod
                    NEXT FIELD campo_desc

              AFTER FIELD campo_desc
		    IF g_reg.campo_desc IS NULL THEN
		       ERROR "La descripcion de campo NO puede ser nulo"
		       NEXT FIELD  campo_desc
		    END IF 
              AFTER FIELD tipo_dato
		    IF g_reg.tipo_dato IS NULL THEN
		       ERROR "Tipo de dato NO puede ser nulo"
		       NEXT FIELD  tipo_dato
		    END IF 
              AFTER FIELD long
                    SELECT long, campo_cod  INTO vlong, campo_cod_ant 
                           FROM   tab_campo
                           WHERE  layout_cod = g_reg.layout_cod
                             AND  tipo_reg   = g_reg.tipo_reg
                             AND  campo_cod  = g_reg.campo_cod
                   
                    LET campo_cod_ant = campo_cod_ant - 1
                    SELECT pos_fin  INTO pos_fin_ant
                           FROM   tab_campo
                           WHERE  layout_cod = g_reg.layout_cod
                             AND  tipo_reg   = g_reg.tipo_reg
                             AND  campo_cod  = campo_cod_ant

                    LET g_reg.pos_ini = pos_fin_ant + 1
                    LET g_reg.pos_fin = g_reg.pos_ini + g_reg.long - 1
{
                    LET vvlong = vlong - g_reg.long

                    SELECT MAX(pos_fin)
                    INTO   vpos_fin
                    FROM   tab_campo
                    WHERE  layout_cod = g_reg.layout_cod
                    AND    tipo_reg   = g_reg.tipo_reg
                    AND    campo_cod  = g_reg.campo_cod

                    LET g_reg.pos_fin = vpos_fin - vvlong
}

                    DISPLAY BY NAME g_reg.pos_ini,g_reg.pos_fin 
                      
                    IF g_reg.tipo_dato = "#" THEN
                       NEXT FIELD decimal                             
                    ELSE
                       LET g_reg.decimal = NULL 
                       DISPLAY BY NAME g_reg.decimal
                       NEXT FIELD valor_cte
                    END IF
              AFTER FIELD decimal
                    NEXT FIELD valor_cte
              AFTER FIELD valor_cte

              AFTER FIELD num_valida

              CALL Pregunta()

	      IF aux_pausa MATCHES "[Ss]" THEN
	         UPDATE tab_campo SET
                        campo_desc = g_reg.campo_desc,
                        tipo_dato  = g_reg.tipo_dato,
		        long       = g_reg.long,
                        decimal    = g_reg.decimal,
                        pos_ini    = g_reg.pos_ini,
                        pos_fin    = g_reg.pos_fin,
                        valor_cte  = g_reg.valor_cte,
                        num_valida = g_reg.num_valida
   		 WHERE  layout_cod = g_reg.layout_cod	
                 AND    tipo_reg   = g_reg.tipo_reg
                 AND    campo_cod  = g_reg.campo_cod
	
                 DECLARE cursor4 CURSOR FOR
                 SELECT campo_cod,
                        pos_ini - vvlong,
                        pos_fin - vvlong
                 FROM   tab_campo
                 WHERE  layout_cod  = g_reg.layout_cod
                 AND    tipo_reg    = g_reg.tipo_reg
                 AND    campo_cod   > g_reg.campo_cod
                
                 LET pos = 1
             
                 FOREACH cursor4 INTO l_reg[pos].*
                     UPDATE tab_campo SET
                            pos_ini  = l_reg[pos].pos_ini,
                            pos_fin  = l_reg[pos].pos_fin
                     WHERE  layout_cod  = g_reg.layout_cod
                     AND    tipo_reg    = g_reg.tipo_reg
                     AND    campo_cod   = l_reg[pos].campo_cod

	             LET pos = pos + 1
                 END FOREACH

		 ERROR "REGISTRO MODIFICADO"
		 SLEEP 2
		 ERROR ""

		 CALL Inicializa()
	      ELSE
		 ERROR "PROCESO DE MODIFICAR CANCELADO."
		 SLEEP 2
		 ERROR ""
	      END IF

	      EXIT INPUT

	   ON KEY(CONTROL-C)
	      CALL Inicializa()
	      EXIT INPUT

	   ON KEY(INTERRUPT)
	      CALL Inicializa()
	      EXIT INPUT
	   END INPUT
	ELSE
	   ERROR "ARCHIVO DE ... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Elimina()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0482" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta                                        (Ctrl-C) Salir       " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "         Escoja con <ENTER>                       a eliminar                  " AT 2,1
	   DISPLAY "                                                                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON layout_cod,
                                  tipo_reg 
                             FROM layout_cod,
                                  tipo_reg
	      ON KEY(CONTROL-M)
		ERROR "PROCESANDO INFORMACION..."
		LET int_flag = FALSE
		EXIT CONSTRUCT
	      ON KEY(CONTROL-C)
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

	   LET sel_where = "SELECT * FROM tab_campo WHERE ",
                            cla_where CLIPPED,
			   "ORDER BY 1,2,3 "

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
		 ON KEY(CONTROL-M)
		    LET pos = ARR_CURR()
                    LET g_reg.layout_cod = l_record[pos].layout_cod
                    LET g_reg.tipo_reg   = l_record[pos].tipo_reg
                    LET g_reg.campo_cod  = l_record[pos].campo_cod
                    LET g_reg.campo_desc = l_record[pos].campo_desc
                    LET g_reg.tipo_dato  = l_record[pos].tipo_dato
                    LET g_reg.long       = l_record[pos].long
                    LET g_reg.decimal    = l_record[pos].decimal
                    LET g_reg.pos_ini    = l_record[pos].pos_ini
                    LET g_reg.pos_fin    = l_record[pos].pos_fin
                    LET g_reg.valor_cte  = l_record[pos].valor_cte
                    LET g_reg.num_valida = l_record[pos].num_valida
		    EXIT DISPLAY

                 ON KEY(CONTROL-C)
                    ERROR "Debe elegir un registro."
		    LET pos = ARR_CURR()

                 ON KEY(INTERRUPT)
                    ERROR "Debe elegir un registro."
		    LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE ... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	   DISPLAY " ELIMINA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

	   DISPLAY BY NAME g_reg.*
	      CALL Pregunta()

	   IF aux_pausa MATCHES "[Ss]" THEN

	      DELETE FROM tab_campo
	      WHERE layout_cod	 = g_reg.layout_cod	
              AND   tipo_reg     = g_reg.tipo_reg
              AND   campo_cod    = g_reg.campo_cod

              DECLARE cursor5 CURSOR FOR
              SELECT campo_cod ,
                     pos_ini,
                     pos_fin
              FROM   tab_campo
              WHERE  layout_cod  = g_reg.layout_cod
              AND    tipo_reg    = g_reg.tipo_reg
              AND    campo_cod   > g_reg.campo_cod
                
              LET pos = 1
             
              FOREACH cursor5 INTO l_reg[pos].*
                 UPDATE tab_campo SET
                        campo_cod = l_reg[pos].campo_cod - 1,
                        pos_ini   = l_reg[pos].pos_ini - g_reg.long,
                        pos_fin   = l_reg[pos].pos_fin - g_reg.long
                 WHERE  layout_cod  = g_reg.layout_cod
                 AND    tipo_reg    = g_reg.tipo_reg
                 AND    campo_cod   = l_reg[pos].campo_cod

	         LET pos = pos + 1
              END FOREACH

	      ERROR "REGISTRO ELIMINADO."
	      SLEEP 2
	      ERROR ""
	   ELSE
	      ERROR "PROCESO CANCELADO."
	      SLEEP 2
	      ERROR ""
	   END IF

	   CALL Inicializa()
	ELSE
	   ERROR "ARCHIVO DE STATUS DE PROMOTORES... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
{
FUNCTION impresion(pos)
	DEFINE i, pos SMALLINT

	LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,".IMPTSTAT",hoy USING "dd-mm-yyyy" CLIPPED

	START REPORT rpt_pro_status_interno To g_impre

	FOR i = 1 TO (pos+1)
	   LET g_reg.status_interno	 = l_record[i].codigo
	   LET g_reg.desc_status_corta = l_record[i].descripcion

	   IF g_reg.status_interno	 IS NULL THEN
	      EXIT FOR
	   END IF

	   OUTPUT TO REPORT rpt_pro_status_interno(g_reg.*)
	END FOR

	FINISH REPORT rpt_pro_status_interno

	ERROR "LISTADO GENERADO..."
	SLEEP 2
	ERROR ""

	LET g_lista = "lp ",g_impre
	RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_pro_status_interno(g_reg)
         DEFINE g_reg		RECORD 
		status_interno	SMALLINT,
		desc_status_corta  	CHAR(40)
	 END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM019 ",
               COLUMN 19," LISTADO DE CATALOGO DE STATUS DE PROMOTORES ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 20,"DESCRIPCION STATUS"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 10,g_reg.status_interno ,
               COLUMN 25,g_reg.desc_status_corta
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
	 PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
}
