###############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Modulo            => COM. 					               #
#Programa          => COMC004                                                  #
#Descripcion       => DESCARGA DE PAGO A PROMOTORES                            #
#Autor             => GERARDO ALFONSO VEGA PAREDES.			       #
#Fecha             => 11 agosto 2003.   				       #
#Modificado por    => Laura Eugenia Cortes Guzman                              #
#Fecha             => 11 septiembre 2003.        			       #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE 
      g_reg0 RECORD
	fecha_corte DATE,
	fecha_pago  DATE
      END RECORD,

      g_reg ARRAY[10000] OF RECORD 
         codven     	    LIKE com_comis_detalle.codven,
  	 n_folio            LIKE com_comis_detalle.n_folio,
  	 nss                LIKE com_comis_detalle.nss,
         tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud,
--         fentcons           LIKE com_comis_detalle.fentcons,
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
	 num_sm             LIKE com_comis_detalle.num_sm,
         monto_comision     LIKE com_comis_detalle.monto_comision,
         pago               CHAR(01)
      END RECORD, 

      g_reg1 ARRAY[10000] OF RECORD 
         codven     	    LIKE com_comis_detalle.codven,
  	 n_folio            LIKE com_comis_detalle.n_folio,
  	 nss                LIKE com_comis_detalle.nss,
         tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud,
--         fentcons           LIKE com_comis_detalle.fentcons,
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
	 num_sm             LIKE com_comis_detalle.num_sm,
         monto_comision     LIKE com_comis_detalle.monto_comision,
         pago               CHAR(01)
      END RECORD, 

      vmenu     CHAR(01),
      aux_pausa	CHAR(1),
      HOY	DATE,
      SW_1      SMALLINT,
      cla_sel 	CHAR(500), 
      cla_sel2	CHAR(500), 
      vaccion   smallint,
      cla_where CHAR(800), 
      vcomando  SMALLINT,
      opc 	CHAR(01),
      total     DECIMAL(12,2),
      registros INTEGER,
      longitud  integer
   DEFINE arr_c SMALLINT,
          scr_l SMALLINT
   DEFINE i INTEGER

END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o
	
	DEFER INTERRUPT

	LET HOY = TODAY

	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0041" ATTRIBUTE(BORDER)
	DISPLAY " COMC004                   PAGO  DE  COMISIONES                                " AT 3,1 ATTRIBUTE(REVERSE) 

	DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 5,1 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 7,1 ATTRIBUTE(REVERSE)

         DISPLAY "                                                                               " AT 19,1 ATTRIBUTE(REVERSE)

	MENU "CONSULTA"
           COMMAND "Pago" "Pago Comisiones"
              CALL Inicializa()
	      CALL Consulta()
           COMMAND "Salir" "Salir del Programa"
	      EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END MAIN

FUNCTION Inicializa()
    define i smallint
        LET sw_1 = 0
	INITIALIZE g_reg TO NULL 
        for i=1 to 11
	   DISPLAY g_reg[i].* TO scr_1[i].*
        end for

	LET total = 0
	LET registros = 0
        LET cla_where=NULL
        LET cla_sel=NULL

	DISPLAY registros,total TO scr_3.*

---        CLEAR SCREEN
END FUNCTION

FUNCTION Consulta()
   DEFINE
      pat CHAR(40),
      mat char(40),
      nom char(40),
      pos SMALLINT   

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " [ESC] Procesar " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C] Salir " AT 1,63 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE
{
   CONSTRUCT cla_where ON fecha_corte FROM com_comis_detalle.fecha_corte
      ON KEY (ESC)
         LET vcomando = 2
         EXIT CONSTRUCT
      ON KEY (INTERRUPT)
         LET vcomando=1
         EXIT CONSTRUCT
   END CONSTRUCT

   IF vcomando = 1 THEN
      LET INT_FLAG = FALSE
      ERROR "Operacion abortada"
      LET vcomando = 0
      RETURN
   END IF
}
   INPUT BY NAME g_reg0.fecha_corte, g_reg0.fecha_pago
      AFTER FIELD fecha_corte
         IF g_reg0.fecha_corte IS NULL THEN
            ERROR "La fecha no puede ser nula"
            NEXT FIELD fecha_corte
         END IF
      AFTER FIELD fecha_pago
         IF g_reg0.fecha_pago IS NULL THEN
            ERROR "La fecha no puede ser nula"
            NEXT FIELD fecha_pago
         END IF
      ON KEY (ESC)
         IF g_reg0.fecha_corte IS NULL THEN
            ERROR "La fecha no puede ser nula"
            NEXT FIELD fecha_corte
         END IF
         IF g_reg0.fecha_pago IS NULL THEN
            ERROR "La fecha no puede ser nula"
            NEXT FIELD fecha_pago
         END IF
         EXIT INPUT
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      ERROR "OPERACION CANCELADA..."
      LET INT_FLAG = FALSE
      RETURN
   END IF

   LET cla_sel2="SELECT a.codven,",
                       "a.n_folio,",
                       "a.nss,",
                       "a.tipo_solicitud,",
                       "AVG(a.salario_base_comis),",
		       "AVG(a.num_sm),",
                       "SUM(a.monto_comision), ",
                       "'N' ",
                "FROM   com_comis_detalle a",
               " WHERE  a.fecha_corte  < ","'",g_reg0.fecha_corte,"'",
               " AND    a.comis_pagada <> 'S' ",
               " AND    a.tipo_pago in (20,30,40) ",
               " GROUP  BY 1,2,3,4,8 ",
     	       " ORDER BY 1,2" CLIPPED

       ERROR "Buscando Informacion"

       PREPARE claexe FROM cla_sel2
       DECLARE cursor_1111 CURSOR FOR claexe
       LET pos = 1

##la      LET g_reg0.fecha_pago = hoy 
      FOREACH cursor_1111 INTO g_reg1[pos].*
         LET g_reg[pos].codven             = g_reg1[pos].codven
  	 LET g_reg[pos].nss                = g_reg1[pos].nss
  	 LET g_reg[pos].n_folio            = g_reg1[pos].n_folio
  	 LET g_reg[pos].tipo_solicitud     = g_reg1[pos].tipo_solicitud
--  	 LET g_reg[pos].fentcons           = g_reg1[pos].fentcons
         LET g_reg[pos].salario_base_comis = g_reg1[pos].salario_base_comis
	 LET g_reg[pos].num_sm             = g_reg1[pos].num_sm
         LET g_reg[pos].monto_comision     = g_reg1[pos].monto_comision
         LET g_reg[pos].pago               = g_reg1[pos].pago

         LET total = total + g_reg1[pos].monto_comision
         LET registros = registros + 1
         LET pos = pos + 1
         IF pos >= 9000 THEN
            ERROR "Sobrepaso la capacidad del arreglo"
            EXIT FOREACH
         END IF 
      END FOREACH

##      DISPLAY g_reg0.fecha_pago TO scr_0.fecha_pago
      DISPLAY registros,total TO scr_3.*

      ERROR ""
      IF pos = 1 THEN
         ERROR "NO EXISTE INFORMACION.... "
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
            NEXT FIELD pago
         BEFORE FIELD pago
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()
         AFTER FIELD pago
            IF g_reg[arr_c].pago IS NULL THEN
               ERROR "EL PAGO NO PUEDE SER NULO"
               NEXT FIELD pago
            END IF

         ON KEY (INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT INPUT
         ON KEY (ESC)

            FOR i = 1 TO ARR_CURR()
               IF g_reg[i].pago = "S" THEN
                  UPDATE com_comis_detalle
                  SET    com_comis_detalle.comis_pagada = g_reg[i].pago,
                         com_comis_detalle.fecha_pago   = g_reg0.fecha_pago
                  WHERE  com_comis_detalle.codven       = g_reg[i].codven
                  AND    com_comis_detalle.tipo_pago in (20,30,40)
                  AND    com_comis_detalle.n_folio      = g_reg[i].n_folio
                  AND    com_comis_detalle.tipo_solicitud = g_reg[i].tipo_solicitud
                  AND    com_comis_detalle.nss            = g_reg[i].nss
               END IF
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
{
      IF (pos-1) >= 1 THEN
	 DISPLAY ARRAY g_reg TO scr_1.*
            ON KEY (INTERRUPT)
               DISPLAY "                                      " AT 8,41
               EXIT DISPLAY
	 END DISPLAY
      ELSE
         ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
         SLEEP 3
         error ""
      END IF
}

      call Inicializa()
      CLEAR SCREEN
END FUNCTION

FUNCTION Despliega_promotores()
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		DECIMAL(10,0)
	DEFINE pat,mat,nom		CHAR(60)
	DEFINE l_reg ARRAY[10000] OF RECORD
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
