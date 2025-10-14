################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.                                                   #
#Programa COMM015  => MANTENEDOR DE ESQUEMA DE ISPT                            #
#Fecha             => 23 febrero 1999.			                       #
#By                => GERARDO ALFONSO VEGA PAREDES.			       #
#Sistema           => COM. 					               #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE 
      g_reg RECORD 
         cod_esq_ispt   LIKE com_esq_ispt.cod_esq_ispt,
         desc_esq_ispt  LIKE com_esq_ispt.desc_esq_ispt,
         monto_excento  LIKE com_esq_ispt.monto_excento,
         porcentaje_iva LIKE com_esq_ispt.porcentaje_iva,
         fecha_creacion LIKE com_esq_ispt.fecha_creacion,
         fecha_desde    LIKE com_esq_ispt.fecha_desde,
         fecha_hasta    LIKE com_esq_ispt.fecha_hasta
      END RECORD,

      x_reg ARRAY[500] OF RECORD
       	 vlimite_inf,vlimite_sup integer
      END RECORD,

      g_det ARRAY[500] OF RECORD 
         limite_inf  LIKE com_cuadro_ispt.limite_inf,
         limite_sup  LIKE com_cuadro_ispt.limite_sup,
         cuota_fija  LIKE com_cuadro_ispt.cuota_fija,
         porcentaje  LIKE com_cuadro_ispt.porcentaje,
         fecha_actualiza LIKE com_cuadro_ispt.fecha_actualiza
      END RECORD,

      hoy DATE,
      arr_c,scr_l SMALLINT,
      g_com_parametro RECORD LIKE com_parametro.*,
      g_usuario CHAR(8),
      dio_control SMALLINT,
      aux_pausa CHAR(1)
END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   LET HOY = DATE
   SELECT *,USER INTO g_com_parametro.*,g_usuario FROM com_parametro
   OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMM0151" ATTRIBUTE( BORDER)
   DISPLAY " COMM015                E S Q U E M A   I S P T                                   " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY "                        C U A D R O    I S P T                                    " AT 7,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

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
################################################################################
FUNCTION Inicializa()
   DEFINE i				SMALLINT
   INITIALIZE g_reg.* TO NULL
   DISPLAY BY NAME g_reg.* 
   INITIALIZE g_det TO NULL
   FOR i = 1 TO 10
      DISPLAY g_det[i].* TO scr_1[i].*
   END FOR
END FUNCTION
################################################################################
FUNCTION Agrega()
   DEFINE i SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " [ Esc ] Agrega     [ Ctrl-c ] Salir sin Agregar" AT 2,1 ATTRIBUTE(BOLD)
   INPUT BY NAME g_reg.*
      BEFORE FIELD cod_esq_ispt
         LET DIO_CONTROL = FALSE
      AFTER FIELD cod_esq_ispt
         IF g_reg.cod_esq_ispt IS NULL THEN
	    ERROR "Esquema de Ispt NO puede ser NULO"
	    NEXT FIELD cod_esq_ispt
         END IF
         SELECT "X" 
           FROM com_esq_ispt
          WHERE cod_esq_ispt = g_reg.cod_esq_ispt
	     IF STATUS <> NOTFOUND THEN
	        ERROR "Esquema Ispt YA EXISTE"
	        LET g_reg.cod_esq_ispt = NULL
	        DISPLAY BY NAME g_reg.cod_esq_ispt
	        NEXT FIELD cod_esq_ispt
	     END IF
      AFTER FIELD desc_esq_ispt
         IF g_reg.desc_esq_ispt IS NULL THEN
	    ERROR "Descripcion Esquema NO puede ser NULO"
	    NEXT FIELD desc_esq_ispt
         END IF
         LET g_reg.fecha_creacion = TODAY
         DISPLAY BY NAME g_reg.fecha_creacion
      AFTER FIELD fecha_desde
         IF g_reg.fecha_desde IS NULL THEN
	    ERROR "Fecha Vigencia Desde NO puede ser NULO"
	    NEXT FIELD fecha_desde
         END IF
      AFTER FIELD fecha_hasta
         IF g_reg.fecha_hasta IS NULL THEN
	    ERROR "Fecha Vigencia Hasta NO puede ser NULO"
	    NEXT FIELD fecha_hasta
         END IF
         IF g_reg.fecha_desde > g_reg.fecha_hasta THEN
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
################################################################################
FUNCTION despliega_indi_comisiones()
   DEFINE aux_pausa                SMALLINT
   DEFINE cod                      DECIMAL(10,0)
   DEFINE desc             CHAR(60)
   DEFINE l_reg ARRAY[500] OF RECORD
      codigo           CHAR(10),
      descripcion      CHAR(50)
   END RECORD
   DEFINE pos              SMALLINT
   DEFINE x_buscar         CHAR(60)
   DEFINE x_texto          CHAR(200)

   OPEN WINDOW vent_x AT 07,12 WITH FORM "COMM0152" ATTRIBUTE(BORDER)
   DISPLAY "                 E S Q U E M A    I S P T                " AT 2,1 ATTRIBUTE(REVERSE)
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
      LET x_texto = "SELECT cod_esq_ispt,desc_esq_ispt ",
                      "FROM com_esq_ispt ",
                      "WHERE desc_esq_ispt MATCHES ",
                      "'",x_buscar CLIPPED,"'" CLIPPED
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
         ERROR "ARCHIVO com_esq_comis..... VACIO"
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
#############################################################################
#OJO                                                  
################################################################################
FUNCTION Ingresa_detalle()
   DEFINE i SMALLINT
   INPUT ARRAY g_det FROM scr_1.*
     BEFORE FIELD limite_inf
        LET arr_c = ARR_CURR()
	LET scr_l = SCR_LINE()
     AFTER FIELD limite_inf
        IF arr_c = 1 THEN
	   LET g_det[arr_c].limite_inf = 0  #.01
	   DISPLAY g_det[arr_c].limite_inf TO scr_1[scr_l].limite_inf
	   NEXT FIELD limite_sup
	ELSE
     	   LET g_det[arr_c].limite_inf=g_det[arr_c-1].limite_sup+1
	   DISPLAY g_det[arr_c].limite_inf TO scr_1[scr_l].limite_inf
	   NEXT FIELD limite_sup
	END IF
     BEFORE FIELD limite_sup
	LET arr_c = ARR_CURR()
	LET scr_l = SCR_LINE()
				
     AFTER FIELD limite_sup
        IF g_det[arr_c].limite_sup IS NULL THEN
           ERROR "El limite superior NO puede ser NULO"
	   NEXT FIELD limite_sup
	END IF
	IF g_det[arr_c].limite_inf >= g_det[arr_c].limite_sup THEN
	   ERROR "El Limite superior DEBE ser MAYOR que el Limite inferiro"
	   LET g_det[arr_c].limite_sup = NULL
	   DISPLAY g_det[arr_c].limite_sup TO scr_1[scr_l].limite_sup
	   NEXT FIELD limite_sup
	END IF
     BEFORE FIELD cuota_fija
        LET arr_c = ARR_CURR()
        LET scr_l = SCR_LINE()
     AFTER FIELD cuota_fija
     	IF g_det[arr_c].cuota_fija IS NULL THEN
       	   ERROR "Cuota Fija NO puede ser NULO"
	   NEXT FIELD cuota_fija
	END IF
     BEFORE FIELD porcentaje
        LET arr_c = ARR_CURR()
        LET scr_l = SCR_LINE()
     AFTER FIELD porcentaje
     	IF g_det[arr_c].porcentaje IS NULL THEN
       	   ERROR "Porcentaje NO puede ser NULO"
	   NEXT FIELD porcentaje
	END IF
	LET g_det[arr_c].fecha_actualiza = TODAY
	DISPLAY g_det[arr_c].fecha_actualiza TO
	        scr_1[scr_l].fecha_actualiza
				
     ON KEY ( INTERRUPT )
	CALL Inicializa()
	LET DIO_CONTROL = TRUE
	EXIT INPUT
     ON KEY ( ESC )
        CALL Esta_seguro()
	IF aux_pausa MATCHES "[Ss]"THEN
    	   LET i = ARR_CURR()
	   LET g_det[i].limite_inf = NULL
	   DISPLAY g_det[i].limite_inf TO scr_1[i].limite_inf
	END IF
	FOR i = 1 TO 100 
	   IF g_det[i].limite_inf IS NOT NULL THEN
	      IF g_det[i].limite_sup IS NULL THEN
	  	 ERROR "Limite superior NO puede ser NULO"
		 NEXT FIELD limite_sup
	      END IF
	      IF g_det[i].limite_inf>=g_det[i].limite_sup THEN
		 ERROR "Limite superior DEBE ser MAYOR que el Limite inferior"
		 NEXT FIELD limite_sup
	      END IF
	      IF g_det[i].cuota_fija IS NULL THEN
	  	 ERROR "Cuota fija NO puede ser NULO"
		 NEXT FIELD cuota_fija
	      END IF
     	      IF g_det[i].porcentaje IS NULL THEN
       	         ERROR "Porcentaje NO puede ser NULO"
	         NEXT FIELD porcentaje
	      END IF
           ELSE
	      EXIT FOR
	   END IF
	END FOR
	INSERT INTO com_esq_ispt 
	   VALUES (g_reg.cod_esq_ispt,
 	           g_reg.desc_esq_ispt,
                   g_reg.monto_excento,
                   g_reg.porcentaje_iva,
 	           g_reg.fecha_desde,
 	           g_reg.fecha_hasta,
 	           g_reg.fecha_creacion,
        	   g_usuario)
	 FOR i = 1 TO 100 
	    IF g_det[i].limite_inf IS NOT NULL THEN
	       INSERT INTO com_cuadro_ispt 
	       	  VALUES (g_reg.cod_esq_ispt,
		    	  g_det[i].limite_inf,
 	                  g_det[i].limite_sup,
 	                  g_det[i].cuota_fija,
                          g_det[i].porcentaje,
 	                  g_det[i].fecha_actualiza,
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
################################################################################
FUNCTION Esta_seguro()
	PROMPT "Esta Seguro S/N " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Consulta()
   DEFINE i			SMALLINT
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " [ Ctrl-c ] Salir" AT 2,1 ATTRIBUTE(BOLD)
   INPUT BY NAME g_reg.cod_esq_ispt
      BEFORE FIELD cod_esq_ispt
         LET DIO_CONTROL = FALSE
      AFTER FIELD cod_esq_ispt
         IF g_reg.cod_esq_ispt IS NULL THEN
            CALL despliega_indi_comisiones() RETURNING g_reg.cod_esq_ispt 
         ELSE
            CALL tra_datos()
            IF STATUS = NOTFOUND THEN
	       ERROR "Esquema de Ispt Inexistente"
	       NEXT FIELD cod_esq_ispt
	    ELSE
               EXIT INPUT
            END IF
         END IF
   END INPUT
   LET i = 1
   DECLARE cursor_1 CURSOR FOR 
   SELECT limite_inf,limite_sup,
          cuota_fija,porcentaje,
          fecha_actualiza
     FROM com_cuadro_ispt 
    WHERE cod_esq_ispt = g_reg.cod_esq_ispt
    ORDER BY 1
   FOREACH cursor_1 INTO g_det[i].limite_inf,
             		 g_det[i].limite_sup,
 	            	 g_det[i].cuota_fija,
                         g_det[i].porcentaje,
 	            	 g_det[i].fecha_actualiza
      LET i = i + 1
   END FOREACH
   DISPLAY BY NAME g_reg.*
   CALL SET_COUNT(i-1)
   DISPLAY ARRAY g_det TO scr_1.*
      ON KEY ( INTERRUPT )
         CALL Inicializa()
	 EXIT DISPLAY
   END DISPLAY
END FUNCTION
################################################################################
FUNCTION tra_datos()
   SELECT cod_esq_ispt, desc_esq_ispt,
          monto_excento, porcentaje_iva,
          fecha_creacion, fecha_desde,
          fecha_hasta
     INTO g_reg.cod_esq_ispt,   g_reg.desc_esq_ispt,
          g_reg.monto_excento,  g_reg.porcentaje_iva,
          g_reg.fecha_creacion, g_reg.fecha_desde,
          g_reg.fecha_hasta
     FROM com_esq_ispt
    WHERE cod_esq_ispt = g_reg.cod_esq_ispt
END FUNCTION

FUNCTION Modifica()
   DEFINE i			SMALLINT
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " [ Esc ] Modifica     [ Ctrl-c ] Salir sin Modificar" AT 2,1 ATTRIBUTE(BOLD)
   INPUT BY NAME g_reg.*
      BEFORE FIELD cod_esq_ispt
         LET DIO_CONTROL = FALSE
      AFTER FIELD cod_esq_ispt
         IF g_reg.cod_esq_ispt IS NULL THEN
            CALL despliega_indi_comisiones() RETURNING g_reg.cod_esq_ispt
            CALL tra_datos()
         ELSE
            CALL tra_datos()
	    IF STATUS = NOTFOUND THEN
	       ERROR "Esquema Ispt Inexistente"
	       NEXT FIELD cod_esq_ispt
	    END IF
         END IF
         DISPLAY BY NAME g_reg.*

	 LET i = 1
	 DECLARE cursor_2 CURSOR FOR 
         SELECT limite_inf,limite_sup,
                cuota_fija,porcentaje,
                fecha_actualiza
	   FROM com_cuadro_ispt 
          WHERE cod_esq_ispt = g_reg.cod_esq_ispt
	  ORDER BY 1
	 FOREACH cursor_2 INTO g_det[i].limite_inf,
 	            	       g_det[i].limite_sup,
 	            	       g_det[i].cuota_fija,
 	            	       g_det[i].porcentaje,
 	            	       g_det[i].fecha_actualiza
	    LET i = i + 1
	 END FOREACH
	 DISPLAY BY NAME g_reg.*
	 FOR i = 1 TO 10 
	    DISPLAY g_det[i].* TO scr_1[i].*
	 END FOR
	 NEXT FIELD desc_esq_ispt

      AFTER FIELD desc_esq_ispt
         IF FGL_LASTKEY()=FGL_KEYVAL("UP") OR
            FGL_LASTKEY()=FGL_KEYVAL("LEFT") THEN
            NEXT FIELD desc_esq_ispt
	 END IF
	 IF g_reg.desc_esq_ispt IS NULL THEN
	    ERROR "Descripcion Esquema NO puede ser NULO"
	    NEXT FIELD desc_esq_ispt
	 END IF
      AFTER FIELD fecha_desde
         IF FGL_LASTKEY()=FGL_KEYVAL("UP") OR
            FGL_LASTKEY()=FGL_KEYVAL("LEFT") THEN
	    NEXT FIELD desc_esq_ispt
	 END IF
	 IF g_reg.fecha_desde IS NULL THEN
	    ERROR "Fecha Vigencia Desde NO puede ser NULO"
	    NEXT FIELD fecha_desde
	 END IF
      AFTER FIELD fecha_hasta
	 IF g_reg.fecha_hasta IS NULL THEN
	    ERROR "Fecha Vigencia Hasta NO puede ser NULO"
	    NEXT FIELD fecha_hasta
	 END IF
	 IF g_reg.fecha_desde > g_reg.fecha_hasta THEN
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
	 UPDATE com_esq_ispt 
            SET desc_esq_ispt = g_reg.desc_esq_ispt,
 		monto_excento = g_reg.monto_excento,
 		porcentaje_iva = g_reg.porcentaje_iva,
 	        fecha_desde = g_reg.fecha_desde,
 	        fecha_hasta = g_reg.fecha_hasta
	  WHERE cod_esq_ispt = g_reg.cod_esq_ispt
	 ERROR "REGISTRO(S) MODIFICADOS(S)" SLEEP 2 ERROR ""
	 CALL Inicializa()
	 NEXT FIELD cod_esq_ispt
      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 EXIT INPUT
   END INPUT
END FUNCTION
################################################################################
FUNCTION Modifica_detalle(LARGO)
   DEFINE i	SMALLINT
   DEFINE LARGO	SMALLINT
   DEFINE pos 	SMALLINT
   CALL SET_COUNT(LARGO)

   LET g_det[1].limite_inf = 1
   INPUT ARRAY g_det WITHOUT DEFAULTS FROM scr_1.*
       BEFORE FIELD limite_inf
	  LET arr_c = ARR_CURR()
	  LET scr_l = SCR_LINE()
	  DISPLAY g_det[1].limite_inf TO scr_1[1].limite_inf
       AFTER FIELD limite_inf
	  IF arr_c = 1 THEN
	     LET g_det[arr_c].limite_inf = 0 #.01
	     DISPLAY g_det[arr_c].limite_inf TO scr_1[scr_l].limite_inf
	     NEXT FIELD limite_sup
          ELSE
             LET g_det[arr_c].limite_inf=g_det[arr_c-1].limite_sup+1
	     DISPLAY g_det[arr_c].limite_inf TO scr_1[scr_l].limite_inf
	     NEXT FIELD limite_sup
	  END IF
       BEFORE FIELD limite_sup
          LET arr_c = ARR_CURR()
	  LET scr_l = SCR_LINE()
       AFTER FIELD limite_sup
	  IF g_det[arr_c].limite_sup IS NULL THEN
	     ERROR "Limite superior NO puede ser NULO"
	     NEXT FIELD limite_sup
	  END IF
	  IF g_det[arr_c].limite_inf >= g_det[arr_c].limite_sup THEN
	     ERROR "Limite superior DEBE ser MAYOR que el Limite inferior"
	     LET g_det[arr_c].limite_sup = NULL
	     DISPLAY g_det[arr_c].limite_sup TO scr_1[scr_l].limite_sup
	     NEXT FIELD limite_sup
	  END IF
       BEFORE FIELD cuota_fija
	  LET arr_c = ARR_CURR()
	  LET scr_l = SCR_LINE()
       AFTER FIELD cuota_fija
	  IF g_det[arr_c].cuota_fija IS NULL THEN
	     ERROR "Cuota fija NO puede ser NULA"
	     NEXT FIELD cuota_fija
	  END IF
#OJO>> Actualizar la fecha solo si existe cambio en el registro#
	  LET g_det[arr_c].fecha_actualiza = TODAY
	  DISPLAY g_det[arr_c].fecha_actualiza TO scr_1[scr_l].fecha_actualiza
      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 LET DIO_CONTROL = TRUE
	 EXIT INPUT
      ON KEY ( ESC )
	 CALL Esta_seguro()
	 IF aux_pausa MATCHES "[Ss]"THEN
	    LET pos = 0
	    FOR i = 1 TO 100
	       IF g_det[i].limite_sup IS NOT NULL THEN
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
	    IF g_det[i].limite_inf IS NOT NULL THEN
	       IF g_det[i].limite_sup IS NULL THEN
	          ERROR "Limite superiot NO puede ser NULO"
		  NEXT FIELD limite_sup
	       END IF
	       IF g_det[i].limite_inf >= g_det[i].limite_sup THEN
	          ERROR "Limite superior DEBE ser MAYOR que el Limite inferior"
		  NEXT FIELD limite_sup
	       END IF
	       IF g_det[i].cuota_fija IS NULL THEN
	          ERROR "Cuota fija NO puede ser NULA"
		  NEXT FIELD cuota_fija
	       END IF
	    ELSE
	       EXIT FOR
	    END IF
	 END FOR
         UPDATE com_esq_ispt 
            SET desc_esq_ispt = g_reg.desc_esq_ispt,
                monto_excento= g_reg.monto_excento,
		porcentaje_iva= g_reg.porcentaje_iva,
 	        fecha_desde = g_reg.fecha_desde,
 	        fecha_hasta = g_reg.fecha_hasta
	  WHERE cod_esq_ispt = g_reg.cod_esq_ispt
	  DELETE FROM com_cuadro_ispt
	   WHERE cod_esq_ispt = g_reg.cod_esq_ispt
         FOR i = 1 TO pos 
	    IF g_det[i].limite_inf IS NOT NULL THEN
	       INSERT INTO com_cuadro_ispt 
		  VALUES (g_reg.cod_esq_ispt,
			  g_det[i].limite_inf,
 	                  g_det[i].limite_sup,
 	                  g_det[i].cuota_fija,
 	                  g_det[i].porcentaje,
 	                  g_det[i].fecha_actualiza,
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
################################################################################
FUNCTION Elimina()
   DEFINE i			SMALLINT
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ELIMINA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "[ Ctrl-c ] Salir sin Eliminar" AT 2,1 ATTRIBUTE(BOLD)
   INPUT BY NAME g_reg.cod_esq_ispt
      BEFORE FIELD cod_esq_ispt
         LET DIO_CONTROL = FALSE
      AFTER FIELD cod_esq_ispt
	 IF g_reg.cod_esq_ispt IS NULL THEN
            CALL despliega_indi_comisiones() RETURNING g_reg.cod_esq_ispt 
            CALL tra_datos()
         ELSE
            CALL tra_datos()
	    IF STATUS = NOTFOUND THEN
	       ERROR "Esquema de Comision Inexistente"
	       NEXT FIELD cod_esq_ispt
	    END IF
	 END IF

	 DISPLAY BY NAME g_reg.*

         LET i = 1
	 DECLARE cursor_3 CURSOR FOR 
         SELECT limite_inf,limite_sup,
                cuota_fija,porcentaje,
                fecha_actualiza
           FROM com_cuadro_ispt 
          WHERE cod_esq_ispt = g_reg.cod_esq_ispt
	  ORDER BY 1
	 FOREACH cursor_3 INTO g_det[i].limite_inf,
 	       		       g_det[i].limite_sup,
 	            	       g_det[i].cuota_fija,
 	            	       g_det[i].porcentaje,
 	            	       g_det[i].fecha_actualiza
            LET i = i + 1
	 END FOREACH
	 DISPLAY BY NAME g_reg.*
	 FOR i = 1 TO 10 
	    DISPLAY g_det[i].* TO scr_1[i].*
	 END FOR
         LET aux_pausa = "n"
	 CALL Esta_seguro()
	 IF aux_pausa MATCHES "[Ss]" THEN	
	    DELETE FROM com_esq_ispt
	     WHERE cod_esq_ispt = g_reg.cod_esq_ispt
	    DELETE FROM com_cuadro_ispt
	     WHERE cod_esq_ispt = g_reg.cod_esq_ispt
	    ERROR "REGISTRO(S) ELIMINADOS(S)" SLEEP 2 ERROR ""
	    CALL Inicializa()
	    EXIT INPUT
	 END IF
      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 EXIT INPUT
   END INPUT
END FUNCTION
