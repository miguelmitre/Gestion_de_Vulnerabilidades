################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.                                                   #
#Programa COMM008  => CATALOGO DE ESQUEMA DE BONOS                             #
#Sistema           => COM. 					               #
#By                => JUAN DAVID HERNANDEZ OYARCE. 			       #
#Fecha             => 03 Mayo 1997.	 				       #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha ULTIMA MODI => 26 julio 1997.                                           # 
#Fecha ULTIMA MODI => 16 enero 2001.                                           #
#Fecha ULTIMA MODI => 18 marzo 2001.                                           #
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE g_master RECORD 
      cod_esq_bono        LIKE com_esq_bono.cod_esq_bono,
      desc_esq_bono       LIKE com_esq_bono.desc_esq_bono,
      criterio_cod        LIKE tab_criterio.criterio_cod,
      criterio_desc       LIKE tab_criterio.criterio_desc,
      abreviatura         LIKE tab_criterio.abreviatura,
      calculo_cod         LIKE tab_criterio.calculo_cod,
      minimo_solicitudes  LIKE com_esq_bono.minimo_solicitudes,
      fecha_crea          LIKE com_esq_bono.fecha_crea,
      fecha_desde         LIKE com_esq_bono.fecha_desde,
      fecha_hasta         LIKE com_esq_bono.fecha_hasta
   END RECORD

   DEFINE x_reg ARRAY[500] OF RECORD
      vrango_desde,
      vrango_hasta integer
   END RECORD

   DEFINE g_detalle ARRAY[500] OF RECORD 
      rango_desde     LIKE com_cuadro_bono.rango_desde,
      rango_hasta     LIKE com_cuadro_bono.rango_hasta,
      monto_bono      LIKE com_cuadro_bono.monto_bono,
      fecha_actualiza LIKE com_cuadro_bono.fecha_actualiza
   END RECORD

   DEFINE g_com_parametro RECORD LIKE com_parametro.*

   DEFINE 
      hoy         DATE,
      arr_c       SMALLINT,
      scr_l	  SMALLINT,
      g_usuario	  CHAR(8),
      opc	  CHAR(1),
      dio_control SMALLINT
END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   LET hoy = DATE

   SELECT *,USER 
     INTO g_com_parametro.*,g_usuario 
     FROM com_parametro

   OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMM0081" ATTRIBUTE( BORDER)
   DISPLAY " COMM008                  ESQUEMA DE BONOS                                 " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY "                             CUADRO DE BONOS                               " AT 8,1 ATTRIBUTE(REVERSE) 
   DISPLAY hoy USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)
	
   MENU "ESQUEMA"
      COMMAND "Agrega" "Agrega Esquema"
         CALL Inicializa()
         CALL Agrega()
	 CALL Inicializa()
      COMMAND "Consulta" "Consulta Esquema"
	 CALL Inicializa()
	 CALL Consulta()
	 CALL Inicializa()
      COMMAND "Modifica" "Modifica Esquema"
	 CALL Inicializa()
	 CALL Modifica()
	 CALL Inicializa()
      COMMAND "Elimina" "Elimina Esquema"
	 CALL Inicializa()
	 CALL Elimina()
	 CALL Inicializa()
      COMMAND "Salir" "Salir del Programa"
	 EXIT MENU
   END MENU
   CLOSE WINDOW ventana_1
END MAIN

FUNCTION Inicializa()
   DEFINE 
      i SMALLINT

   INITIALIZE g_master.* TO NULL
   DISPLAY BY NAME g_master.* 
   INITIALIZE g_detalle TO NULL
   FOR i = 1 TO 10
      DISPLAY g_detalle[i].* TO scr_1[i].*
   END FOR

END FUNCTION

FUNCTION Agrega()
   DEFINE 
      i SMALLINT

   DISPLAY "" AT 1, 1
   DISPLAY "" AT 2,1
   DISPLAY " AGREGA " AT 1,70 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " [ Esc ] Agrega     [ Ctrl-c ] Salir sin Agregar" AT 2,1 ATTRIBUTE(BOLD)

   INPUT BY NAME g_master.*
      BEFORE FIELD cod_esq_bono
         LET DIO_CONTROL = FALSE
      AFTER FIELD cod_esq_bono
	 IF g_master.cod_esq_bono IS NULL THEN
	    ERROR "Esquema de bono NO puede ser NULO"
	    NEXT FIELD cod_esq_bono
	 END IF

	 SELECT "X" 
           FROM com_esq_bono
	  WHERE cod_esq_bono = g_master.cod_esq_bono
	 IF STATUS <> NOTFOUND THEN
	    ERROR "Esquema bono YA EXISTE"
	    LET g_master.cod_esq_bono = NULL
	    DISPLAY BY NAME g_master.cod_esq_bono
	    NEXT FIELD cod_esq_bono
	 END IF

      AFTER FIELD desc_esq_bono
	 IF g_master.desc_esq_bono IS NULL THEN
	    ERROR "Descripcion Esquema NO puede ser NULO"
	    NEXT FIELD desc_esq_bono
	 END IF
	 LET g_master.fecha_crea = TODAY
	 DISPLAY BY NAME g_master.fecha_crea

      AFTER FIELD criterio_cod
  	 IF g_master.criterio_cod IS NULL THEN
	    CALL Despliega_criterios() 
               RETURNING g_master.criterio_cod,
		         g_master.criterio_desc,
		         g_master.abreviatura,
			 g_master.calculo_cod
	 ELSE
	    SELECT criterio_cod, 
 	           criterio_desc, 
		   abreviatura,
		   calculo_cod
	      INTO g_master.criterio_cod, 
	 	   g_master.criterio_desc,
		   g_master.abreviatura,
		   g_master.calculo_cod
	      FROM tab_criterio
	     WHERE criterio_cod = g_master.criterio_cod
	    IF STATUS = NOTFOUND THEN
	       ERROR "Criterio Inexistente"
	       NEXT FIELD criterio_cod
	    END IF
	 END IF

         IF g_master.calculo_cod = "P" THEN
            DISPLAY "PORCENTAJE" AT 09,45
         ELSE
            DISPLAY "CANTIDAD  " AT 09,45
         END IF

	 DISPLAY BY NAME 
            g_master.criterio_cod,
	    g_master.criterio_desc,
	    g_master.abreviatura,
	    g_master.calculo_cod
          
      AFTER FIELD minimo_solicitudes
	 IF g_master.minimo_solicitudes IS NULL THEN
	    ERROR "NO puede ser NULO"
	    NEXT FIELD minimo_solicitudes
	 END IF

      AFTER FIELD fecha_desde
         IF g_master.fecha_desde IS NULL THEN
	    ERROR "Fecha Vigencia Desde NO puede ser NULO"
	    NEXT FIELD fecha_desde
	 END IF

      AFTER FIELD fecha_hasta
	 IF g_master.fecha_hasta IS NULL THEN
	    ERROR "Fecha Vigencia Hasta NO puede ser NULO"
	    NEXT FIELD fecha_hasta
	 END IF
	 IF g_master.fecha_desde > g_master.fecha_hasta THEN
	    ERROR "Fecha Vigencia desde NO puede ser mayor que Fecha Vigencia HASTA"
	    NEXT FIELD fecha_desde
	 ELSE
	    CALL Ingresa_detalle()
	    IF DIO_CONTROL THEN
	       CALL Inicializa()
	       EXIT INPUT
	    END IF
	 END IF
      ON KEY ( ESC )
	 ERROR "Para Agregar Esquema Comercial. Debe ingresar sus Tramos"
      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 EXIT INPUT
   END INPUT
END FUNCTION

FUNCTION Despliega_criterios()
   DEFINE 
      l_reg ARRAY[500] OF RECORD
         codigo           SMALLINT,
         descripcion      CHAR(50),
         abreviatura         CHAR(05),
      	 calculo          CHAR(01)
      END RECORD

   DEFINE 
      opc      SMALLINT,
      cod      DECIMAL(10,0),
      desc     CHAR(60),
      pos      SMALLINT,
      x_buscar CHAR(60),
      x_texto  CHAR(200)

   OPEN WINDOW vent_x AT 07,12 WITH FORM "COMM0083" ATTRIBUTE(BORDER)
   DISPLAY "                    C R I T E R I O S                       " AT 2,1 ATTRIBUTE(REVERSE)
   INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
   END INPUT

   WHILE TRUE
      LET x_texto = " SELECT criterio_cod,criterio_desc, ",
                           " abreviatura,calculo_cod ",
                       "FROM tab_criterio WHERE criterio_desc MATCHES ",'"',x_buscar CLIPPED,'"' CLIPPED
      PREPARE cur1001a FROM x_texto
      DECLARE cur_8a CURSOR FOR cur1001a

      LET pos = 1

      FOREACH cur_8a INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 500 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
          ERROR "ARCHIVO CRITERIOS..... VACIO"
      END IF
      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY
         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY
      END DISPLAY
      IF pos <> 0 THEN
         EXIT WHILE
      END IF
   END WHILE

   CLOSE WINDOW vent_x

   RETURN l_reg[pos].codigo,
          l_reg[pos].descripcion,
          l_reg[pos].abreviatura,
          l_reg[pos].calculo
END FUNCTION

FUNCTION despliega_indi_bonos()
   DEFINE 
      l_reg ARRAY[500] OF RECORD
         codigo      CHAR(10),
         descripcion CHAR(50)
      END RECORD

   DEFINE 
      opc      SMALLINT,
      cod      DECIMAL(10,0),
      desc     CHAR(60),
      pos      SMALLINT,
      x_buscar CHAR(60),
      x_texto  CHAR(200)

   OPEN WINDOW vent_x AT 07,12 WITH FORM "COMM0082" ATTRIBUTE(BORDER)
   DISPLAY "                 E S Q U E M A    B O N O                " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
   END INPUT

   WHILE TRUE
      LET x_texto = " SELECT cod_esq_bono,desc_esq_bono ",
                      " FROM com_esq_bono ",
                     " WHERE desc_esq_bono MATCHES ",
                         '"',x_buscar CLIPPED,'"' CLIPPED,
                     " ORDER BY cod_esq_bono "

      PREPARE cur1001 FROM x_texto
      DECLARE cur_8 CURSOR FOR cur1001

      LET pos = 1

      FOREACH cur_8 INTO cod,desc
         LET l_reg[pos].codigo = cod
         LET l_reg[pos].descripcion = desc
         LET pos = pos + 1
         IF pos >= 500 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO com_esq_bono..... VACIO"
      END IF

      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg TO scr_1.*
         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY
         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
   END WHILE

   CLOSE WINDOW vent_x

   RETURN l_reg[pos].codigo

END FUNCTION

FUNCTION Ingresa_detalle()
   DEFINE i SMALLINT

   INPUT ARRAY g_detalle FROM scr_1.*
      BEFORE FIELD rango_desde
         LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()

      AFTER FIELD rango_desde
         IF arr_c = 1 THEN
	    LET g_detalle[arr_c].rango_desde = 0  #.01
	    DISPLAY g_detalle[arr_c].rango_desde TO scr_1[scr_l].rango_desde
	    NEXT FIELD rango_hasta
	 ELSE
            LET g_detalle[arr_c].rango_desde=g_detalle[arr_c-1].rango_hasta + 1
 	    DISPLAY g_detalle[arr_c].rango_desde TO scr_1[scr_l].rango_desde
	    NEXT FIELD rango_hasta
	 END IF

      BEFORE FIELD rango_hasta
	 LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()
				
      AFTER FIELD rango_hasta
         IF g_detalle[arr_c].rango_hasta IS NULL THEN
            ERROR "Salario Minimo Hasta NO puede ser NULO"
	   NEXT FIELD rango_hasta
  	 END IF
	 IF g_detalle[arr_c].rango_desde >= g_detalle[arr_c].rango_hasta THEN
	    ERROR "El Hasta DEBE ser MAYOR que el desde"
	    LET g_detalle[arr_c].rango_hasta = NULL
	    DISPLAY g_detalle[arr_c].rango_hasta TO scr_1[scr_l].rango_hasta
	    NEXT FIELD rango_hasta
  	 END IF

      BEFORE FIELD monto_bono
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD monto_bono
      	 IF g_detalle[arr_c].monto_bono IS NULL THEN
       	    ERROR "Monto bono NO puede ser NULO"
	    NEXT FIELD monto_bono
	 END IF
	 LET g_detalle[arr_c].fecha_actualiza = TODAY
	 DISPLAY g_detalle[arr_c].fecha_actualiza TO
	         scr_1[scr_l].fecha_actualiza
				
      ON KEY (INTERRUPT)
	 CALL Inicializa()
	 LET DIO_CONTROL = TRUE
	 EXIT INPUT
      ON KEY (ESC)
         CALL Esta_seguro()
	 IF opc MATCHES "[Ss]"THEN
    	    LET i = ARR_CURR()
	    LET g_detalle[i].rango_desde = NULL
  	 END IF
	 FOR i = 1 TO 100 
	    IF g_detalle[i].rango_desde IS NOT NULL THEN
	       IF g_detalle[i].rango_hasta IS NULL THEN
	  	  ERROR "Minimo Hasta NO puede ser NULO"
		  NEXT FIELD rango_hasta
	       END IF
	       IF g_detalle[i].rango_desde>=g_detalle[i].rango_hasta THEN
		  ERROR "Minimo Hasta DEBE ser MAYOR que el desde"
		  NEXT FIELD rango_hasta
	       END IF
	       IF g_detalle[i].monto_bono IS NULL THEN
	      	  ERROR "Monto bono NO puede ser NULO"
		  NEXT FIELD monto_bono
	       END IF
            ELSE
	       EXIT FOR
	    END IF
	 END FOR

 	 INSERT INTO com_esq_bono 
 	    VALUES (g_master.cod_esq_bono,
 	            g_master.desc_esq_bono,
 	            g_master.fecha_crea,
        	    g_usuario,
 	            g_master.fecha_desde,
 	            g_master.fecha_hasta,
                    g_master.minimo_solicitudes,
                    g_master.criterio_cod)

	 FOR i = 1 TO 100 
	    IF g_detalle[i].rango_desde IS NOT NULL THEN
	       INSERT INTO com_cuadro_bono 
	       	  VALUES (g_master.cod_esq_bono,
		    	  g_detalle[i].rango_desde,
 	                  g_detalle[i].rango_hasta,
 	                  g_detalle[i].monto_bono,
 	                  g_detalle[i].fecha_actualiza,
			  g_usuario)
	    ELSE
	       EXIT FOR
	    END IF
	 END FOR

         ERROR "REGISTRO(S) INGRSADO(S)" SLEEP 2 ERROR ""

         CALL Inicializa()

         EXIT INPUT
   END INPUT
END FUNCTION

FUNCTION Esta_seguro()
   PROMPT "Esta Seguro S/N " FOR CHAR opc
END FUNCTION

FUNCTION Consulta()
   DEFINE i SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " CONSULTA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " [ Ctrl-c ] Salir" AT 2,1 ATTRIBUTE(BOLD)

   LET INT_FLAG = FALSE

   INPUT BY NAME g_master.cod_esq_bono
      BEFORE FIELD cod_esq_bono
	 LET DIO_CONTROL = FALSE
      AFTER FIELD cod_esq_bono
	 IF g_master.cod_esq_bono IS NULL THEN
            CALL despliega_indi_bonos() RETURNING g_master.cod_esq_bono 
         ELSE
	    SELECT cod_esq_bono,
                   desc_esq_bono,
                   fecha_crea,
 	           fecha_desde,
                   fecha_hasta,
                   criterio_cod,
                   minimo_solicitudes
	      INTO g_master.cod_esq_bono,
                   g_master.desc_esq_bono,
 	           g_master.fecha_crea,
                   g_master.fecha_desde,
 	           g_master.fecha_hasta,
                   g_master.criterio_cod,
 	           g_master.minimo_solicitudes
	      FROM com_esq_bono
	     WHERE cod_esq_bono = g_master.cod_esq_bono
	    IF STATUS = NOTFOUND THEN
	       ERROR "Esquema de bono Inexistente"
	       NEXT FIELD cod_esq_bono
	    ELSE
	       SELECT criterio_cod, 
 		      criterio_desc, 
		      abreviatura,
		      calculo_cod
		 INTO g_master.criterio_cod, 
		      g_master.criterio_desc,
		      g_master.abreviatura,
		      g_master.calculo_cod
		 FROM tab_criterio
	        WHERE criterio_cod = g_master.criterio_cod
	       EXIT INPUT
	    END IF
	 END IF

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT
   
   IF INT_FLAG THEN
      RETURN
   END IF

   IF g_master.calculo_cod = "P" THEN
      DISPLAY "PORCENTAJE" AT 09,45
   ELSE
      DISPLAY "CANTIDAD  " AT 09,45
   END IF
   
   LET i = 1

   DECLARE cursor_1 CURSOR FOR 
   SELECT rango_desde,
          rango_hasta,
          monto_bono,
 	  fecha_actualiza
     FROM com_cuadro_bono 
    WHERE cod_esq_bono = g_master.cod_esq_bono
    ORDER BY 1

   FOREACH cursor_1 
      INTO g_detalle[i].rango_desde,
      	   g_detalle[i].rango_hasta,
           g_detalle[i].monto_bono,
           g_detalle[i].fecha_actualiza
      LET i = i + 1
   END FOREACH

   DISPLAY BY NAME g_master.*

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY g_detalle TO scr_1.*
      ON KEY ( INTERRUPT )
         CALL Inicializa()
	 EXIT DISPLAY
   END DISPLAY
END FUNCTION

FUNCTION Modifica()
   DEFINE i SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " MODIFICA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " [ Esc ] Modifica     [ Ctrl-c ] Salir sin Modificar" AT 2,1 ATTRIBUTE(BOLD)

   INPUT BY NAME g_master.*
      BEFORE FIELD cod_esq_bono
         LET DIO_CONTROL = FALSE
      AFTER FIELD cod_esq_bono
         IF g_master.cod_esq_bono IS NULL THEN
            CALL despliega_indi_bonos() RETURNING g_master.cod_esq_bono

	    SELECT cod_esq_bono,
                   desc_esq_bono,
                   fecha_crea,
 	           fecha_desde,
                   fecha_hasta,
                   criterio_cod,
                   minimo_solicitudes
	      INTO g_master.cod_esq_bono,
                   g_master.desc_esq_bono,
 	           g_master.fecha_crea,
                   g_master.fecha_desde,
 	           g_master.fecha_hasta,
                   g_master.criterio_cod,
 		   g_master.minimo_solicitudes
	      FROM com_esq_bono
	     WHERE cod_esq_bono = g_master.cod_esq_bono
         ELSE
	    SELECT cod_esq_bono,
                   desc_esq_bono,
                   fecha_crea,
 	           fecha_desde,
                   fecha_hasta,
                   criterio_cod,
                   minimo_solicitudes
	      INTO g_master.cod_esq_bono,
                   g_master.desc_esq_bono,
 	           g_master.fecha_crea,
                   g_master.fecha_desde,
 	           g_master.fecha_hasta,
                   g_master.criterio_cod,
 		   g_master.minimo_solicitudes
	      FROM com_esq_bono
	     WHERE cod_esq_bono = g_master.cod_esq_bono
	    IF STATUS = NOTFOUND THEN
	       ERROR "Esquema de bono Inexistente"
	       NEXT FIELD cod_esq_bono
	    END IF
	 END IF

	 DISPLAY BY NAME g_master.*

	 DECLARE curx CURSOR FOR
	 SELECT "X" 
           FROM com_bono_resumen
	  WHERE cod_esq_bono = g_master.cod_esq_bono
	 OPEN curx
	 FETCH curx
	    IF STATUS <> 100 THEN
   	       ERROR "No se puede modificar porque existen calculos con este esquema"
   	       SLEEP 2
   	       CLOSE curx
   	       EXIT INPUT
	    END IF
	 CLOSE curx

         SELECT criterio_cod, 
 	        criterio_desc, 
		abreviatura,
		calculo_cod
	   INTO g_master.criterio_cod, 
	        g_master.criterio_desc,
		g_master.abreviatura,
		g_master.calculo_cod
	   FROM tab_criterio
	  WHERE criterio_cod = g_master.criterio_cod

         IF g_master.calculo_cod = "P" THEN
            DISPLAY "PORCENTAJE" AT 09,45
         ELSE
            DISPLAY "CANTIDAD  " AT 09,45
         END IF

	 LET i = 1

	 DECLARE cursor_2 CURSOR FOR 
         SELECT rango_desde,
                rango_hasta,
 	        monto_bono,
 	        fecha_actualiza
	   FROM com_cuadro_bono 
          WHERE cod_esq_bono = g_master.cod_esq_bono
	  ORDER BY 1

	 FOREACH cursor_2 INTO g_detalle[i].rango_desde,
          		       g_detalle[i].rango_hasta,
 	            	       g_detalle[i].monto_bono,
 	            	       g_detalle[i].fecha_actualiza
	    LET i = i + 1
	 END FOREACH

         DISPLAY BY NAME g_master.*

	 FOR i = 1 TO 10 
	    DISPLAY g_detalle[i].* TO scr_1[i].*
	 END FOR

	 NEXT FIELD desc_esq_bono

      AFTER FIELD desc_esq_bono
         IF FGL_LASTKEY()=FGL_KEYVAL("UP") OR
            FGL_LASTKEY()=FGL_KEYVAL("LEFT") THEN
            NEXT FIELD desc_esq_bono
	 END IF
	 IF g_master.desc_esq_bono IS NULL THEN
	    ERROR "Descripcion Esquema NO puede ser NULO"
	    NEXT FIELD desc_esq_bono
	 END IF
      AFTER FIELD criterio_cod
  	 IF g_master.criterio_cod IS NULL THEN
	    CALL Despliega_criterios() 
                 RETURNING g_master.criterio_cod,
	                   g_master.criterio_desc,
			   g_master.abreviatura,
			   g_master.calculo_cod

	 ELSE
	    SELECT criterio_cod, 
 	           criterio_desc, 
		   abreviatura,
		   calculo_cod
	      INTO g_master.criterio_cod, 
		   g_master.criterio_desc,
		   g_master.abreviatura,
		   g_master.calculo_cod
	      FROM tab_criterio
	     WHERE criterio_cod = g_master.criterio_cod
	    IF STATUS = NOTFOUND THEN
	       ERROR "Criterio Inexistente"
	       NEXT FIELD criterio_cod
	    END IF
	 END IF
	 DISPLAY BY NAME g_master.criterio_cod,
	                 g_master.criterio_desc,
		         g_master.abreviatura,
			 g_master.calculo_cod

      AFTER FIELD minimo_solicitudes
	 IF g_master.minimo_solicitudes IS NULL THEN
	    ERROR "NO puede ser NULO"
	    NEXT FIELD minimo_solicitudes
	 END IF
	 NEXT FIELD fecha_desde

      AFTER FIELD fecha_desde
         IF FGL_LASTKEY()=FGL_KEYVAL("UP") OR
            FGL_LASTKEY()=FGL_KEYVAL("LEFT") THEN
            NEXT FIELD desc_esq_bono
	 END IF
	 IF g_master.fecha_desde IS NULL THEN
	    ERROR "Fecha Vigencia Desde NO puede ser NULO"
	    NEXT FIELD fecha_desde
	 END IF

      AFTER FIELD fecha_hasta
         IF g_master.fecha_hasta IS NULL THEN
	    ERROR "Fecha Vigencia Hasta NO puede ser NULO"
	    NEXT FIELD fecha_hasta
	 END IF
	 IF g_master.fecha_desde > g_master.fecha_hasta THEN
	    ERROR "Fecha Vigencia desde NO puede ser mayor que Fecha Vigencia HASTA"
	    NEXT FIELD fecha_desde
	 ELSE
	    CALL Modifica_detalle(i-1)
	    IF DIO_CONTROL THEN
	       CALL Inicializa()
	       EXIT INPUT
	    END IF
	 END IF

      ON KEY ( ESC )
	 UPDATE com_esq_bono 
            SET desc_esq_bono      = g_master.desc_esq_bono,
 	        fecha_desde        = g_master.fecha_desde,
 	        fecha_hasta        = g_master.fecha_hasta,
                minimo_solicitudes = g_master.minimo_solicitudes,
 		criterio_cod       = g_master.criterio_cod
	  WHERE cod_esq_bono       = g_master.cod_esq_bono

	 ERROR "REGISTRO(S) MODIFICADOS(S)" SLEEP 2 ERROR ""

	 CALL Inicializa()

         NEXT FIELD cod_esq_bono

      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 EXIT INPUT
   END INPUT

END FUNCTION

FUNCTION Modifica_detalle(LARGO)
   DEFINE 
      i     SMALLINT,
      LARGO SMALLINT,
      pos   SMALLINT

   CALL SET_COUNT(LARGO)

   LET g_detalle[1].rango_desde = 1

   INPUT ARRAY g_detalle WITHOUT DEFAULTS FROM scr_1.*
      BEFORE FIELD rango_desde
	 LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()
	 DISPLAY g_detalle[1].rango_desde TO scr_1[1].rango_desde

      AFTER FIELD rango_desde
	 IF arr_c = 1 THEN
	    LET g_detalle[arr_c].rango_desde = 0 #.01
	    DISPLAY g_detalle[arr_c].rango_desde TO scr_1[scr_l].rango_desde
	    NEXT FIELD rango_hasta
	 ELSE
            LET g_detalle[arr_c].rango_desde=g_detalle[arr_c-1].rango_hasta + 1
 	    DISPLAY g_detalle[arr_c].rango_desde TO scr_1[scr_l].rango_desde
	    NEXT FIELD rango_hasta
         END IF

      BEFORE FIELD rango_hasta
	 LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()

      AFTER FIELD rango_hasta
	 IF g_detalle[arr_c].rango_hasta IS NULL THEN
	    ERROR "Salario Minimo Hasta NO puede ser NULO"
	    NEXT FIELD rango_hasta
	 END IF
	 IF g_detalle[arr_c].rango_desde >= g_detalle[arr_c].rango_hasta THEN
            ERROR "Salario Minimo Hasta DEBE ser MAYOR que el desde"
	    LET g_detalle[arr_c].rango_hasta = NULL
	    DISPLAY g_detalle[arr_c].rango_hasta TO scr_1[scr_l].rango_hasta
	    NEXT FIELD rango_hasta
	 END IF
			
      BEFORE FIELD monto_bono
	 LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()

      AFTER FIELD monto_bono
	 IF g_detalle[arr_c].monto_bono IS NULL THEN
	    ERROR "Monto bono NO puede ser NULO"
	    NEXT FIELD monto_bono
	 END IF


	 LET g_detalle[arr_c].fecha_actualiza = TODAY
	 DISPLAY g_detalle[arr_c].fecha_actualiza TO
	         scr_1[scr_l].fecha_actualiza

      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 LET DIO_CONTROL = TRUE
	 EXIT INPUT

      ON KEY ( ESC )
         CALL Esta_seguro()
         IF opc MATCHES "[Ss]"THEN
	    LET pos = 0
	    FOR i = 1 TO 100
	       IF g_detalle[i].rango_hasta IS NOT NULL THEN
		  LET pos = pos + 1
	       ELSE
		  EXIT FOR
	       END IF
	    END FOR
	 ELSE
	    ERROR "REGISTRO(S) NO MODIFICADOS(S)" SLEEP 2 ERROR ""

	    CALL Inicializa()

            LET DIO_CONTROL = TRUE

	    EXIT INPUT
	 END IF

	 FOR i = 1 TO pos 
	    IF g_detalle[i].rango_desde IS NOT NULL THEN
	       IF g_detalle[i].rango_hasta IS NULL THEN
	          ERROR "Salario Minimo Hasta NO puede ser NULO"
	          NEXT FIELD rango_hasta
	       END IF
	       IF g_detalle[i].rango_desde >= g_detalle[i].rango_hasta THEN
		  ERROR "Salario Minimo Hasta DEBE ser MAYOR que el desde"
		  NEXT FIELD rango_hasta
	       END IF
	       IF g_detalle[i].monto_bono IS NULL THEN
	          ERROR "Monto bono NO puede ser NULO"
	          NEXT FIELD monto_bono
	       END IF
	    ELSE
	       EXIT FOR
	    END IF
	 END FOR

	 UPDATE com_esq_bono 
            SET desc_esq_bono       = g_master.desc_esq_bono,
 	        fecha_desde         = g_master.fecha_desde,
 	        fecha_hasta         = g_master.fecha_hasta,
                criterio_cod        = g_master.criterio_cod,
		minimo_solicitudes  = g_master.minimo_solicitudes
	  WHERE cod_esq_bono        = g_master.cod_esq_bono

	 DELETE 
           FROM com_cuadro_bono
	  WHERE cod_esq_bono = g_master.cod_esq_bono

	 FOR i = 1 TO pos 
	    IF g_detalle[i].rango_desde IS NOT NULL THEN
	       INSERT INTO com_cuadro_bono 
		  VALUES (g_master.cod_esq_bono,
		          g_detalle[i].rango_desde,
 	                  g_detalle[i].rango_hasta,
 	                  g_detalle[i].monto_bono,
 	                  g_detalle[i].fecha_actualiza,
			  g_usuario)
	    ELSE
	       EXIT FOR
	    END IF
	 END FOR

         ERROR "REGISTRO(S) MODIFICADOS(S)" SLEEP 2 ERROR ""

         CALL Inicializa()

	 EXIT INPUT
   END INPUT
END FUNCTION

FUNCTION Elimina()
   DEFINE 
      i SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ELIMINA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "[ Ctrl-c ] Salir sin Eliminar" AT 2,1 ATTRIBUTE(BOLD)

   INPUT BY NAME g_master.cod_esq_bono
      BEFORE FIELD cod_esq_bono
	 LET DIO_CONTROL = FALSE

      AFTER FIELD cod_esq_bono
         IF g_master.cod_esq_bono IS NULL THEN
            CALL despliega_indi_bonos() RETURNING g_master.cod_esq_bono 
	    SELECT cod_esq_bono,
                   desc_esq_bono,
                   fecha_crea,
 	           fecha_desde,
                   fecha_hasta,
                   criterio_cod,
                   minimo_solicitudes
	      INTO g_master.cod_esq_bono,g_master.desc_esq_bono,
 	           g_master.fecha_crea,g_master.fecha_desde,
 	           g_master.fecha_hasta,g_master.criterio_cod,
		   g_master.minimo_solicitudes
	      FROM com_esq_bono
	     WHERE cod_esq_bono = g_master.cod_esq_bono
         ELSE
	    SELECT cod_esq_bono,
                   desc_esq_bono,
                   fecha_crea,
 	           fecha_desde,
                   fecha_hasta,
                   criterio_cod,
                   minimo_solicitudes
	      INTO g_master.cod_esq_bono,
                   g_master.desc_esq_bono,
 	           g_master.fecha_crea,
                   g_master.fecha_desde,
 	           g_master.fecha_hasta,
                   g_master.criterio_cod,
		   g_master.minimo_solicitudes
	      FROM com_esq_bono
	     WHERE cod_esq_bono = g_master.cod_esq_bono
	    IF STATUS = NOTFOUND THEN
	       ERROR "Esquema de bono Inexistente"
	       NEXT FIELD cod_esq_bono
	    END IF
	 END IF

	 DISPLAY BY NAME g_master.*

	 DECLARE curxx CURSOR FOR
	 SELECT "X" 
           FROM com_bono_resumen
	  WHERE cod_esq_bono = g_master.cod_esq_bono

	 OPEN curxx
	 FETCH curxx
	    IF STATUS <> 100 THEN
   	       ERROR "No se puede eliminar porque existen calculos con este esquema"
   	       SLEEP 2
   	       CLOSE curxx
   	       EXIT INPUT
	    END IF
   	 CLOSE curxx

         SELECT criterio_cod, 
 	        criterio_desc, 
		abreviatura,
		calculo_cod
	   INTO g_master.criterio_cod, 
	        g_master.criterio_desc,
		g_master.abreviatura,
		g_master.calculo_cod
	   FROM tab_criterio
	  WHERE criterio_cod = g_master.criterio_cod

         IF g_master.calculo_cod = "P" THEN
            DISPLAY "PORCENTAJE" AT 09,45
         ELSE
            DISPLAY "CANTIDAD  " AT 09,45
         END IF

	 LET i = 1

	 DECLARE cursor_3 CURSOR FOR 
         SELECT rango_desde,
                rango_hasta,
 	        monto_bono,
 	        fecha_actualiza
	   FROM com_cuadro_bono 
          WHERE cod_esq_bono = g_master.cod_esq_bono
	  ORDER BY 1

	 FOREACH cursor_3 INTO 
            g_detalle[i].rango_desde,
            g_detalle[i].rango_hasta,
 	    g_detalle[i].monto_bono,
 	    g_detalle[i].fecha_actualiza

            LET i = i + 1
         END FOREACH

         DISPLAY BY NAME g_master.*

         FOR i = 1 TO 10 
	    DISPLAY g_detalle[i].* TO scr_1[i].*
	 END FOR

         LET opc = "n"

         CALL Esta_seguro()

         IF opc MATCHES "[Ss]" THEN	
            DELETE 
              FROM com_esq_bono
	     WHERE cod_esq_bono = g_master.cod_esq_bono

            DELETE 
              FROM com_cuadro_bono
             WHERE cod_esq_bono = g_master.cod_esq_bono

            ERROR "REGISTRO(S) ELIMINADOS(S)" SLEEP 2 ERROR ""

            CALL Inicializa()
	    EXIT INPUT
	 END IF
      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 EXIT INPUT
   END INPUT
END FUNCTION
