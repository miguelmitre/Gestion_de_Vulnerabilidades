################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Modulo            => COM. 					               #
#Programa          => COMC005                                                  #
#Descripcion       => CAMBIA cod_promotor a otro de com_comis_detalle          #
#Autor             => GERARDO ALFONSO VEGA PAREDES.			       #
#Fecha             => 27 agosto 2003.   				       #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE 
      g_reg0 RECORD
        vcod_Promotor CHAR(10),
	vfecha_inicio DATE,
	vfecha_fin    DATE,
        vnombre       CHAR(50)
      END RECORD,

      reg_consul   RECORD 
         codven     	    LIKE com_comis_detalle.codven,
         n_folio            LIKE com_comis_detalle.n_folio,
         nss                LIKE com_comis_detalle.nss,
  	 fentcons           LIKE com_comis_detalle.fentcons,
         fecha_corte        LIKE com_comis_detalle.fecha_corte,
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
	 num_sm             LIKE com_comis_detalle.num_sm,
         monto_comision     LIKE com_comis_detalle.monto_comision
      END RECORD,

      g_reg ARRAY[1000] OF RECORD 
         codven     	    LIKE com_comis_detalle.codven,
         n_folio            LIKE com_comis_detalle.n_folio,
         nss                LIKE com_comis_detalle.nss,
  	 fentcons           LIKE com_comis_detalle.fentcons,
         fecha_corte        LIKE com_comis_detalle.fecha_corte,
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
	 num_sm             LIKE com_comis_detalle.num_sm,
         monto_comision     LIKE com_comis_detalle.monto_comision
      END RECORD

   DEFINE vpaterno     CHAR(50),
          vmaterno     CHAR(50),
          vnombres     CHAR(50),
          vmenu         CHAR(01),
          aux_pausa	CHAR(1),
          hoy	        DATE,
          sw_1          SMALLINT,
          cla_sel 	CHAR(500), 
          cla_sel2	CHAR(500), 
          vaccion       smallint,
          cla_where     CHAR(800), 
          vcomando      SMALLINT,
          opc     	CHAR(01),
          total         DECIMAL(12,2),
          registros     INTEGER,
          longitud      integer,
          arr_c         SMALLINT,
          scr_l         SMALLINT,
          i             INTEGER,
          enter         CHAR(01)

   DEFINE vcuantos INTEGER

END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o
	
	DEFER INTERRUPT

	LET HOY = TODAY

	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0051" ATTRIBUTE(BORDER)
	DISPLAY " COMC005              CAMBIO DE COMISION A PROMOTORES                          " AT 3,1 ATTRIBUTE(REVERSE) 

	DISPLAY hoy USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 6,1 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 8,1 ATTRIBUTE(REVERSE)

--         DISPLAY "                                                                               " AT 19,1 ATTRIBUTE(REVERSE)

	MENU "MODIFICA"
           COMMAND "Modifica"   "Modifica comision promotor"
              CALL Inicializa()
	      CALL Consulta()
           COMMAND "Salir" "Salir del Programa"
	      EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END MAIN

FUNCTION Inicializa()
   DEFINE i smallint
   LET sw_1 = 0
   INITIALIZE g_reg TO NULL 
   for i=1 to 11
       DISPLAY g_reg[i].* TO scr_1[i].*
   end for

   LET total = 0
   LET registros = 0
   LET cla_where=NULL
   LET cla_sel=NULL

END FUNCTION

FUNCTION Consulta()
   DEFINE
      pat CHAR(40),
      mat char(40),
      nom char(40),
      pos SMALLINT   

   DISPLAY "                                      " AT 1,1
   DISPLAY "                                      " AT 2,1
   DISPLAY " [ESC] Procesar " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C] Salir " AT 1,63 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg0.*
      AFTER FIELD vcod_promotor
         IF g_reg0.vcod_promotor IS NULL THEN
            ERROR "El promotor no puede ser nulo"
            NEXT FIELD vcod_promotor
         END IF

         SELECT paterno,
                materno,
                nombres
         INTO   vpaterno,
                vmaterno,
                vnombres
         FROM   pro_mae_promotor
         WHERE  cod_promotor = g_reg0.vcod_promotor

         LET g_reg0.vnombre = vpaterno CLIPPED," ",
                              vmaterno CLIPPED," ",
                              vnombres CLIPPED

         DISPLAY g_reg0.vnombre TO scr_0.vnombre

      AFTER FIELD vfecha_inicio
         IF g_reg0.vfecha_inicio IS NULL THEN
            ERROR "La fecha no puede ser nula"
            NEXT FIELD vfecha_inicio
         END IF
      AFTER FIELD vfecha_fin
         IF g_reg0.vfecha_fin IS NULL THEN
            ERROR "La fecha no puede ser nula"
            NEXT FIELD vfecha_fin
         END IF

         SELECT COUNT(*)
         INTO   vcuantos
         FROM   com_comis_detalle
         WHERE  codven = g_reg0.vcod_promotor
         AND    fentcons BETWEEN g_reg0.vfecha_inicio AND g_reg0.vfecha_fin
       --AND    fecha_corte <= "04/20/2004"
         AND    fecha_corte <> "05/31/2004"
         AND    fecha_corte <> "04/30/2004"
         IF vcuantos = 0 THEN
            ERROR "NO EXISTEN REGISTROS CON ESTE RANGO DE FECHAS"
            NEXT FIELD vfecha_inicio
         END IF

      ON KEY (ESC)
         IF g_reg0.vcod_promotor IS NULL THEN
            ERROR "El promotor no puede ser nulo"
            NEXT FIELD vcod_promotor
         END IF
         IF g_reg0.vfecha_inicio IS NULL THEN
            ERROR "La fecha no puede ser nula"
            NEXT FIELD vfecha_inicio
         END IF
         IF g_reg0.vfecha_fin IS NULL THEN
            ERROR "La fecha no puede ser nula"
            NEXT FIELD vfecha_fin
         END IF
         EXIT INPUT
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      ERROR "OPERACION CANCELADA..."
      SLEEP 3
      ERROR ""
      LET INT_FLAG = FALSE
      RETURN
   END IF

   INITIALIZE reg_consul.* TO NULL
   LET pos = 0


   DECLARE cursor_1111 CURSOR FOR
          SELECT  codven, n_folio, nss, fentcons, fecha_corte,
                  salario_base_comis, num_sm, monto_comision 
            FROM  com_comis_detalle 
           WHERE  codven    = g_reg0.vcod_promotor
             AND  fentcons  BETWEEN g_reg0.vfecha_inicio
             AND  g_reg0.vfecha_fin
   --          AND    fecha_corte <= "04/20/2004"
             AND    fecha_corte <> "05/31/2004"
             AND    fecha_corte <> "04/30/2004"

     	  ORDER BY 2

          LET pos = 1
   FOREACH cursor_1111 INTO reg_consul.*

         LET g_reg[pos].codven     	   = reg_consul.codven
         LET g_reg[pos].n_folio            = reg_consul.n_folio
         LET g_reg[pos].nss                = reg_consul.nss
  	 LET g_reg[pos].fentcons           = reg_consul.fentcons
         LET g_reg[pos].fecha_corte        = reg_consul.fecha_corte
         LET g_reg[pos].salario_base_comis = reg_consul.salario_base_comis
	 LET g_reg[pos].num_sm             = reg_consul.num_sm
         LET g_reg[pos].monto_comision     = reg_consul.monto_comision

         LET pos = pos + 1
         IF pos >= 9000 THEN
            ERROR "Sobrepaso la capacidad del arreglo"
            EXIT FOREACH
         END IF 
   END FOREACH

   ERROR ""

   IF (pos-1) < 1 THEN
         ERROR "NO EXISTEN REGISTROS... INTENTE NUEVAMENTE"
         SLEEP 2
         ERROR ""
         RETURN
   END IF

   CALL  SET_COUNT(pos-1)

   LET INT_FLAG = FALSE

   INPUT ARRAY g_reg WITHOUT DEFAULTS FROM scr_1.*
         BEFORE FIELD codven
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

         AFTER FIELD codven
            IF g_reg[arr_c].codven IS NULL THEN
               ERROR "El Promotor no puede ser nulo"
               NEXT FIELD codven
            END IF

            SELECT "X"
            FROM   pro_mae_promotor
            WHERE  cod_promotor = g_reg[arr_c].codven
            IF STATUS = NOTFOUND THEN
               ERROR "El promotor digitado no existe"
               NEXT FIELD codven
            END IF

         ON KEY (INTERRUPT, CONTROL-C)
            LET INT_FLAG = TRUE
            EXIT INPUT

         ON KEY (ESC)
            FOR i = 1 TO ARR_CURR()
               UPDATE com_comis_detalle
               SET    codven  = g_reg[i].codven,
                      estado_comision = 0
               WHERE  codven  = g_reg0.vcod_promotor
               AND    n_folio = g_reg[i].n_folio
               AND    nss     = g_reg[i].nss
            END FOR  
            EXIT INPUT
   END INPUT    

   IF INT_FLAG THEN
         LET INT_FLAG = FALSE
         ERROR "Operacion Cancelada"
         RETURN
   END IF

   ERROR " RESGITROS ACTUALIZADOS..."
   SLEEP 3
   ERROR ""

   call Inicializa()
   CLEAR SCREEN
END FUNCTION

FUNCTION Despliega_promotores()
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		DECIMAL(10,0)
	DEFINE pat,mat,nom		CHAR(60)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		CHAR(10),
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_buscar		CHAR(60)
	DEFINE x_texto		CHAR(100)

	OPEN WINDOW vent_1 AT 07,12 WITH FORM "COMC0032" ATTRIBUTE(BORDER)
	DISPLAY "                 P R O M O T O R E S                     " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
#	WHILE TRUE
	      LET x_texto = " SELECT cod_promotor,paterno,materno,nombres ",
			    " FROM pro_mae_promotor WHERE paterno MATCHES ",'"',x_buscar CLIPPED,'"'," ORDER BY 2 " CLIPPED
	      PREPARE curg8 FROM x_texto
	      DECLARE cur_g8 CURSOR FOR curg8
	      LET pos = 1
	      FOREACH cur_g8 INTO cod,pat,mat,nom
		      LET l_reg[pos].codigo = cod
		      LET l_reg[pos].descripcion = pat CLIPPED," ",
		                               mat CLIPPED," ",
		                               nom CLIPPED
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO PROMOTORES..... VACIO"
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
#	      IF pos <> 0 THEN
#	         EXIT WHILE
	#      END IF
	#END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
