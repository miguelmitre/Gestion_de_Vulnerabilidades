##############################################################################
#Project           => SAFRE (Mexico)
#Owner             => E.F.P.
#Programa CTACL001 => CONSULTA DE SALDOS POR CUENTA INDIVIDUAL
#Fecha actualiz.   => 04 NOVIEMBRE 1998
#Actualizacion     => HECTOR M. FERNANDEZ A.
#Actualizacion     => ARMANDO RODRIGUEZ CASTROPAREDES
#Fecha actualiz.   => 10 noviembre 2005
#Sistema           => CTA
#Req:MLM-754       => JCPV 02/08/2011. Identificar subctas 36,37(31)            ##############################################################################
DATABASE safre_af
GLOBALS
   DEFINE item_row_cnt        SMALLINT,
          cont_inp            SMALLINT,
          cur_row             SMALLINT,
          scr_row             SMALLINT,
          sql_stat            INTEGER,
          p_cur_row           SMALLINT

   DEFINE i         SMALLINT
   DEFINE j         SMALLINT
   DEFINE k         SMALLINT
   DEFINE xsiefore  SMALLINT
   DEFINE xprecio   DECIMAL(16,14)
   DEFINE xacciones DECIMAL(16,6)
   DEFINE xpesos    DECIMAL(16,6)

   DEFINE det1 ARRAY[20] OF RECORD
       subct_prueba     CHAR(1),
       subct_cod        SMALLINT,
       subct_corta      CHAR(5),
       monto_pesos      DECIMAL(16,6),
       monto_acciones   DECIMAL(16,6)
   END RECORD

   DEFINE det2 ARRAY[8] OF RECORD
      siefore_cod       SMALLINT,
      siefore_nom       char(8),
      pesos_sie         DECIMAL(16,6),
      acciones_sie      DECIMAL(16,6)
   END RECORD

	DEFINE aux_pausa	CHAR(1)
	DEFINE txt_cursor	CHAR(500)

	DEFINE enc		RECORD
      n_seguro         CHAR(11),
      nombre           CHAR(60),
      fec_1            DATE,
      fec_2            DATE,
      actividad        CHAR(12),
      marca            CHAR(50),
      fec_marca        DATE
	END RECORD

   DEFINE tmonto_pesos       DECIMAL(16,6)
   DEFINE tmonto_acciones    DECIMAL(16,6)
   DEFINE tmonto_acciones1   DECIMAL(16,6)
   DEFINE tmonto_vivienda    DECIMAL(16,6)
   DEFINE tmonto_participa   DECIMAL(16,6)
   DEFINE tcuenta_sar        DECIMAL(16,6)
   DEFINE tcuenta_viv        DECIMAL(16,6)
   DEFINE tcuenta_pesos      DECIMAL(16,6)
   DEFINE tcuenta_accion     DECIMAL(16,6)
	DEFINE a,b,c 		        CHAR(30)
	DEFINE mm    		        CHAR(50)
	DEFINE am    		        SMALLINT
	DEFINE vprecio_accion	  DECIMAL(16,6)
	DEFINE vprecio_accion1    DECIMAL(16,6)
	DEFINE vprecio_participa  DECIMAL(16,14)
	DEFINE vfecha_accion  	  DATE
	DEFINE d,e   		        DATE
	DEFINE fm    		        DATE
	DEFINE fc    		        DATE
	DEFINE desc_larga	        CHAR(60)
	DEFINE linea		        SMALLINT
	DEFINE cm        	        SMALLINT
	DEFINE HOY		           DATE
	DEFINE folisua            CHAR(6)
	DEFINE folio1             SMALLINT
   DEFINE ACCION             CHAR(1)
END GLOBALS

MAIN
	OPTIONS
	PROMPT LINE LAST,
	       ACCEPT KEY CONTROL-I,
	       INPUT WRAP
	DEFER INTERRUPT

        LET enc.n_seguro = ARG_VAL(1)
        LET ACCION       = ARG_VAL(2)
        
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTACL0011" ATTRIBUTE(BORDER)
	#OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTACL0011A" ATTRIBUTE(BORDER)
	#OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTACL0011A"
	DISPLAY " CTACL001       CONSULTA SALDOS DE CUENTA INDIVIDUAL                           " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY "SUBCUENTA        PESOS           SIEFORE         PESOS          ACCIONES       " AT 6,1 ATTRIBUTE(REVERSE)
	DISPLAY "  FECHA     MOVTO DESC  ID APORTANTE  SIEFORE   MONTO PESOS    MONTO ACCIONES  " AT 15,1 ATTRIBUTE(REVERSE)
	#DISPLAY "VALUACION AL                TOTAL CUENTA INDIVIDUAL      FECHA DE               " AT 18,1 ATTRIBUTE(REVERSE)
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

        IF ACCION = 'C' THEN
	        CALL Consulta()
        END IF

	MENU "SALDOS"
	     COMMAND "Consultar" "Consultar Saldo"
	              CALL Consulta()
	     COMMAND "Despliega" "Despliega Saldo"
                 CALL Despliega_a_todos()
	     COMMAND "Salir" "Salir del Programa"
		      EXIT PROGRAM
	END MENU
END MAIN

FUNCTION Consulta()
#c-----------------

   DEFINE xfecha  DATE
   DEFINE xhora   datetime hour to second
   
   WHENEVER ERROR CONTINUE
      DROP TABLE precio_dia
      CREATE TEMP TABLE precio_dia(
                        precio decimal(19,14),
                        codigo smallint)
      DROP TABLE ayuda
      CREATE TEMP TABLE ayuda(
                        control            CHAR(9),
                        control_num        SMALLINT,
                        descripcion        CHAR(17))
      INSERT INTO ayuda VALUES("CONTROL-T",1,"TOTAL CUENTA");
      INSERT INTO ayuda VALUES("CONTROL-P",2,"PRECIOS DEL DIA");
      INSERT INTO ayuda VALUES("CONTROL-F",3,"FECHAS TRABAJADOR");
      INSERT INTO ayuda VALUES("CONTROL-U",4,"DOMICILIO");
      INSERT INTO ayuda VALUES("CONTROL-W",5,"TELEFONO");
      #INSERT INTO ayuda VALUES("CONTROL-M",6,"DETALLE");
      INSERT INTO ayuda VALUES("CONTROL-E",7,"INDICADORES");
   WHENEVER ERROR STOP

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
	DISPLAY " CONSULTA " AT 1,68 ATTRIBUTE ( REVERSE )
	DISPLAY " [Ctrl-C]Salir" AT 2,1
	LET enc.n_seguro = NULL
	LET enc.nombre = NULL
 	LET enc.fec_1 = NULL
 	LET enc.fec_2 = NULL
 	LET enc.marca = NULL
   LET enc.actividad = NULL
 	LET enc.fec_marca = NULL
	INITIALIZE enc.* TO NULL
   LET enc.n_seguro = ARG_VAL(1) #pasa parametro
   DISPLAY BY NAME enc.n_seguro  #pasa parametro

	INPUT BY NAME enc.* WITHOUT DEFAULTS

	      BEFORE FIELD n_seguro
		     LET desc_larga = NULL
		     DISPLAY BY NAME desc_larga

	      AFTER FIELD n_seguro
		    IF enc.n_seguro IS NULL THEN
		       CALL Despliega_nss() 
		       IF enc.n_seguro IS NULL THEN NEXT FIELD n_seguro END IF
		    ELSE
		       SELECT "X" 
		       FROM afi_mae_afiliado
		       WHERE n_seguro = enc.n_seguro
		       IF STATUS = NOTFOUND THEN
		          ERROR "Afiliado Inexistente"
		          NEXT FIELD n_seguro
		       END IF
		    END IF

		    SELECT paterno,materno,nombres,fentcons,finicta
		    INTO a,b,c,d,e
		    FROM afi_mae_afiliado
		    WHERE n_seguro = enc.n_seguro

		    IF STATUS <> NOTFOUND THEN
{
                       SELECT  marca_cod,
                               fecha_act_marca,
                               estado_cuenta,
                               fecha_edo_cuenta
                       INTO    cm,
                               fm,
                               am,
                               fc
                       FROM    cta_ctr_cuenta
                       WHERE   nss = enc.n_seguro
}
                       LET xfecha = ""
                       LET xhora  = ""
                       LET cm     = ""
                       LET fm     = ""
                       LET mm     = ""

                       SELECT  MAX(fecha_ini)
                       INTO    xfecha
                       FROM    cta_act_marca
                       WHERE   nss = enc.n_seguro
                
                       IF STATUS <> NOTFOUND  THEN
                           SELECT  MAX(hora_ini)
                           INTO    xhora
                           FROM    cta_act_marca
                           WHERE   nss = enc.n_seguro
			                  AND     fecha_ini = xfecha

                           IF STATUS <> NOTFOUND  THEN
                
                               SELECT  MAX(marca_cod),
                                       fecha_ini
                               INTO    cm,
                                       fm
                               FROM    cta_act_marca
                               WHERE   nss       = enc.n_seguro
			                      AND     fecha_ini = xfecha
			                      AND     hora_ini  = xhora
			                      GROUP BY 2

                               SELECT  marca_desc
                               INTO    mm
                               FROM    tab_marca
                               WHERE   marca_cod = cm

		                         LET enc.marca = mm
		                         LET enc.fec_marca = fm
                           END IF
                       END IF
                       IF enc.marca IS NULL THEN 
			                 LET fm = "" 
		                    LET enc.marca = "CUENTA LIBRE"
		                    LET enc.fec_marca = fm
                       END IF

                       SELECT  ind_actividad,
                               fecha_actividad
                       INTO    am,
                               fc
                       FROM    cta_ctr_cuenta
                       WHERE   nss = enc.n_seguro
                       CASE am
                           WHEN 1
                               LET enc.actividad = "ACTIVA"
                           WHEN 0
                               LET enc.actividad = "INACTIVA"
                       END CASE
                       IF cm = 120 or cm = 130 or cm = 140 THEN
                           LET enc.actividad = "INHABILITADA"
                       END IF
                       IF cm = 150  THEN
                           LET enc.actividad = "CANCELADA"
                       END IF

		       LET enc.nombre = a CLIPPED," ",b CLIPPED," ",c CLIPPED
		       LET enc.fec_1 = d
		       LET enc.fec_2 = e
		       DISPLAY BY NAME enc.*
		    END IF

		    ERROR "Buscando Datos..."
		    CALL Despliega_pantalla_iz()
		    INITIALIZE enc.* TO NULL
		    DISPLAY BY NAME enc.*
                        LET tmonto_pesos      = NULL
                        LET tmonto_acciones   = NULL
                        LET tmonto_acciones1  = NULL
                        LET tmonto_vivienda   = NULL
                        LET tmonto_participa  = NULL
                        LET tcuenta_pesos     = NULL
                        LET tcuenta_accion    = NULL
                        LET vprecio_accion    = NULL
                        LET vprecio_accion1   = NULL
                        LET vprecio_participa = NULL
                        LET vfecha_accion     = NULL
                        #DISPLAY BY NAME tmonto_pesos
                        #DISPLAY BY NAME tmonto_acciones
                        #DISPLAY BY NAME tmonto_acciones1
                        #DISPLAY BY NAME tmonto_vivienda
                        #DISPLAY BY NAME tmonto_participa
                        #DISPLAY BY NAME tcuenta_pesos
                        #DISPLAY BY NAME tcuenta_accion
                        #DISPLAY BY NAME vprecio_accion
                        #DISPLAY BY NAME vprecio_accion1
                        #DISPLAY BY NAME vprecio_participa
                        #DISPLAY BY NAME vfecha_accion 
                    #exit input
                    NEXT FIELD n_seguro

	      ON KEY ( INTERRUPT )
                 LET tmonto_pesos    = NULL
                 LET tmonto_acciones = NULL
                 LET tmonto_acciones1 = NULL
                 LET tmonto_vivienda = NULL
                 LET tmonto_participa= NULL
                 LET tcuenta_pesos   = NULL
                 LET tcuenta_accion  = NULL
                 LET vprecio_accion  = NULL
                 LET vprecio_accion1 = NULL
                 LET vprecio_participa  = NULL
                 LET vfecha_accion   = NULL

	         INITIALIZE enc.* TO NULL

                 #DISPLAY BY NAME tmonto_pesos
                 #DISPLAY BY NAME tmonto_acciones
                 #DISPLAY BY NAME tmonto_acciones1
                 #DISPLAY BY NAME tmonto_vivienda
                 #DISPLAY BY NAME tmonto_participa
                 #DISPLAY BY NAME tcuenta_pesos
                 #DISPLAY BY NAME tcuenta_accion
                 #DISPLAY BY NAME vprecio_accion
                 #DISPLAY BY NAME vprecio_accion1
                 #DISPLAY BY NAME vprecio_participa
                 #DISPLAY BY NAME vfecha_accion 
		 EXIT INPUT
	END INPUT
END FUNCTION

FUNCTION Despliega_nss()
#di---------------------

	DEFINE aux_pausa	     CHAR(1)
	DEFINE txt      	     CHAR(500)
	DEFINE n_busqueda	     CHAR(50)
	DEFINE n_busqueda1     CHAR(50)
	DEFINE l_reg ARRAY[400] OF RECORD
	      n_seguro	        CHAR(11),
	      n_unico	        CHAR(18),
	      n_rfc	           CHAR(13),
	      nombre	        CHAR(50)
	END RECORD
	DEFINE i	              SMALLINT
	DEFINE HACER 	    	  SMALLINT
	DEFINE pat,mat,nom	  CHAR(50)

	OPEN WINDOW v1 AT  4,4 WITH FORM "CTACL0012" ATTRIBUTE(BORDER)
	DISPLAY "          USTED PUEDE UTILIZAR EL * (ASTERICO) COMO COMODIN                " AT 1,1 ATTRIBUTE(REVERSE)

   LET HACER = TRUE
	INPUT BY NAME n_busqueda,n_busqueda1
	    AFTER FIELD n_busqueda
		    IF n_busqueda IS NULL THEN
		       NEXT FIELD n_busqueda
		    END IF
	   AFTER FIELD n_busqueda1
		    IF n_busqueda1 IS NULL THEN
		       NEXT FIELD n_busqueda
		    END IF
		    LET txt = NULL
		    LET txt = "SELECT n_seguro, ",
                              "       n_unico, ",
                              "       n_folio, ",
                              "       paterno,materno,nombres ",
                              "FROM afi_mae_afiliado ",
                              " WHERE paterno MATCHES ",'"',
                              n_busqueda CLIPPED,'"',
                              " AND materno MATCHES ",'"',
                              n_busqueda1 CLIPPED,'"',
			      " ORDER BY 4,5" CLIPPED
		    EXIT INPUT

		ON KEY ( INTERRUPT )
		   LET HACER = FALSE
		   EXIT INPUT
	END INPUT

	IF HACER THEN
	   PREPARE cur1 FROM txt
	   DECLARE cursor_1 cursor FOR cur1
	   LET i = 1
	   FOREACH cursor_1 INTO l_reg[i].n_seguro,
                            l_reg[i].n_unico,
                            l_reg[i].n_rfc,
			                   pat,mat,nom
	           LET l_reg[i].nombre = pat CLIPPED," ",
	                                 mat CLIPPED," ",
	                                 nom CLIPPED
	           LET i = i + 1
		   IF i >= 400  THEN
		      ERROR "Sobrepaso Capacidad Maxima del Arreglo"
		      EXIT FOREACH
		   END IF
	   END FOREACH
           CALL SET_COUNT(i-1)
	   DISPLAY ARRAY l_reg TO scr_1.*
		   ON KEY ( CONTROL-M )
		      LET i = ARR_CURR()
		      LET enc.n_seguro = l_reg[i].n_seguro
		      EXIT DISPLAY
		   ON KEY ( INTERRUPT )
		      LET enc.n_seguro = NULL
		      EXIT DISPLAY
	   END DISPLAY
	END IF
	CLOSE WINDOW v1
END FUNCTION

FUNCTION Despliega_pantalla_iz()
#dpi----------------------------

	DEFINE det1 ARRAY[40] OF RECORD
	    subct_prueba     CHAR(1),
	    subct_cod        SMALLINT,
	    subct_corta      CHAR(5),
	    monto_pesos      DECIMAL(16,6),
	    monto_acciones   DECIMAL(16,6)
 	END RECORD
    
	DEFINE det2 ARRAY[5] OF RECORD
      siefore_cod       SMALLINT,
      siefore_nom       CHAR(8),
      pesos_sie         DECIMAL(16,6),
      acciones_sie      DECIMAL(16,6)
 	END RECORD

	DEFINE i	           SMALLINT
	DEFINE j	           SMALLINT
	DEFINE k	           SMALLINT
	DEFINE xcontrol_num SMALLINT
	DEFINE xsiefore     SMALLINT
	DEFINE xxcontrol    CHAR(9)
	DEFINE xprecio      DECIMAL(19,14)
	DEFINE xacciones    DECIMAL(16,6)
	DEFINE xpesos       DECIMAL(16,6)
	DEFINE rpesos_viv   DECIMAL(12,2)

   LET vprecio_accion  = 0
   LET vprecio_accion1 = 0
   LET vprecio_participa  = 0

	SELECT "X"
	FROM   glo_valor_accion
        WHERE  codigo_siefore = 1
        AND    fecha_valuacion = today

        IF SQLCA.SQLCODE <> 0 THEN
	   SELECT MAX(fecha_valuacion)
	   INTO   vfecha_accion  
	   FROM   glo_valor_accion
           WHERE  codigo_siefore = 1
        ELSE
           LET vfecha_accion = today
        END IF

---carga precio del dia
   SELECT "X"
   FROM   precio_dia
   GROUP BY 1
   IF SQLCA.SQLCODE <> 0 THEN
      INSERT INTO precio_dia
      SELECT precio_del_dia,
             codigo_siefore
	   FROM   glo_valor_accion
	   WHERE  fecha_valuacion = vfecha_accion
   END IF

----saldos totales
	DECLARE cursor_2 CURSOR FOR
	   SELECT "", subct_cod, subct_corta, 0, 0
           FROM   tab_subcuenta
	   WHERE  subct_cod > 0
	   ORDER  BY 2

	   LET i = 1
	   LET k = 1
      LET tmonto_pesos     = 0
      LET tmonto_vivienda  = 0
      LET tcuenta_pesos    = 0

	FOREACH cursor_2 INTO det1[i].*
           IF det1[i].subct_cod = 19 THEN
	      SELECT SUM(monto_en_pesos),
                     SUM(monto_en_acciones)
	      INTO   det1[i].monto_pesos,
                     det1[i].monto_acciones 
              FROM   dis_cuenta
	      WHERE  nss       = enc.n_seguro
	      AND    subcuenta = det1[i].subct_cod
      ELSE
         ---pesos para multisiefore
         DECLARE mul_sie CURSOR FOR
         SELECT siefore,
                "",
                "",
                SUM(monto_en_acciones)
         FROM   dis_cuenta
         WHERE  nss       = enc.n_seguro
         AND    subcuenta = det1[i].subct_cod
         GROUP BY 1
         ORDER BY 1
	      LET xsiefore  = 0
	      LET xprecio   = 0
	      LET xacciones = 0
	      LET xpesos    = 0
         FOREACH mul_sie INTO det2[k].*
                              #xsiefore,
                              #xpesos,
                              #xacciones
            SELECT precio
            INTO   xprecio
            FROM   precio_dia
            WHERE  codigo = det2[k].siefore_cod

            IF det1[i].subct_cod = 4  OR
               det1[i].subct_cod = 8  OR
               det1[i].subct_cod = 14  OR
               det1[i].subct_cod = 35  THEN
               LET rpesos_viv = det2[k].acciones_sie * xprecio
               LET det2[k].pesos_sie = rpesos_viv
            ELSE
               LET det2[k].pesos_sie = det2[k].acciones_sie * xprecio
            END IF
            ---acumula
            LET det1[i].monto_acciones = det1[i].monto_acciones + 
                                         det2[k].acciones_sie
            LET det1[i].monto_pesos    = det1[i].monto_pesos    + 
                                         det2[k].pesos_sie
         END FOREACH
      END IF

      IF det1[i].monto_pesos IS NULL THEN
         LET det1[i].monto_pesos = 0
      END IF
      IF det1[i].monto_acciones IS NULL THEN
         LET det1[i].monto_acciones = 0
      END IF
      IF det1[i].subct_cod = 4  OR
         det1[i].subct_cod = 8  OR 
         det1[i].subct_cod = 14 OR 
         det1[i].subct_cod = 35 THEN
         LET tmonto_vivienda = tmonto_vivienda + det1[i].monto_pesos
      ELSE
         LET tmonto_pesos    = tmonto_pesos    + det1[i].monto_pesos
      END IF
	   LET i = i + 1
	END FOREACH
	ERROR ""
   LET i = i - 1 

   IF i >= 1 THEN
      CALL SET_COUNT(i)
      LET cont_inp = TRUE
     
      LET tcuenta_pesos  = tmonto_pesos + tmonto_vivienda
      LET tcuenta_viv    = tmonto_vivienda
      LET tcuenta_sar    = tmonto_pesos

      #DISPLAY BY NAME tmonto_pesos
      #DISPLAY BY NAME tmonto_vivienda
      #DISPLAY BY NAME tcuenta_pesos
      #DISPLAY BY NAME vfecha_accion

	   #DISPLAY "[Enter]Detalle  [CTRL-T]Total  [CTRL-P]Precios  [CTRL-A]Ayuda "
	   #AT 2,17
	   DISPLAY "              [ENTER]Detalle          [CTRL-V]Ventana de Ayuda "
	   AT 2,17
	DISPLAY "                                                      " AT 13,29
	DISPLAY "                                                      " AT 14,29

      LET cur_row = NULL
      LET scr_row = NULL

      WHILE (cont_inp = TRUE)
         INPUT ARRAY det1 WITHOUT DEFAULTS FROM scr_1.*
         ATTRIBUTES(MAXCOUNT = i,COUNT = i)

            AFTER ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()
               DISPLAY det1[cur_row].* TO scr_1[scr_row].*

            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

                  DISPLAY det1[cur_row].* TO scr_1[scr_row].*
                  LET cont_inp = FALSE

                  CALL despliega_array2(cur_row+1,
                                        det1[cur_row].*,
                                        enc.n_seguro)

             ON KEY ( CONTROL-M )
                 LET i = ARR_CURR()
                 LET j = SCR_LINE()
                 DISPLAY det1[i].* TO scr_1[j].* ATTRIBUTE(REVERSE)

                 SELECT subct_desc
                 INTO   desc_larga
                 FROM   tab_subcuenta
                 WHERE  subct_cod = det1[i].subct_cod

                 DISPLAY BY NAME desc_larga ATTRIBUTE(REVERSE)
	              DISPLAY " DETALLE DE " AT 13,29
	              DISPLAY " LA SUBCUENTA" AT 14,29
                 CALL Detalle(det1[i].subct_cod)
	   DISPLAY "              [ENTER]Detalle          [CTRL-V]Ventana de Ayuda "
	   AT 2,17
      DISPLAY BY NAME desc_larga ATTRIBUTE(NORMAL)
	   DISPLAY "                                                      " AT 13,29
	   DISPLAY "                                                      " AT 14,29
      DISPLAY det1[i].* TO scr_1[j].* ATTRIBUTE(NORMAL)

              ON KEY (CONTROL-V)
                 LET xcontrol_num = ""
                 CALL ayuda()
                 RETURNING xcontrol_num
                 CASE xcontrol_num
                    WHEN 1
                       CALL total_cuenta()
                    WHEN 2
                       CALL valuacion_siefore()
                    WHEN 3
                       CALL muestra_fechas()
                    WHEN 4
                       CALL domicilio()
                    WHEN 5
                       CALL telefono()
                    #WHEN 6
                 #LET i = ARR_CURR()
                 #LET j = SCR_LINE()
                 #DISPLAY det1[i].* TO scr_1[j].* ATTRIBUTE(REVERSE)

                 #SELECT subct_desc
                 #INTO   desc_larga
                 #FROM   tab_subcuenta
                 #WHERE  subct_cod = det1[i].subct_cod

                 #DISPLAY BY NAME desc_larga ATTRIBUTE(REVERSE)
	              #DISPLAY " DETALLE DE " AT 13,29
	              #DISPLAY " LA SUBCUENTA" AT 14,29
                 #CALL Detalle(det1[i].subct_cod)
	   #DISPLAY "              [ENTER]Detalle          [CTRL-V]Ventana de Ayuda "
	   #AT 2,17
      #DISPLAY BY NAME desc_larga ATTRIBUTE(NORMAL)
	   #DISPLAY "                                                      " AT 13,29
	   #DISPLAY "                                                      " AT 14,29
      #DISPLAY det1[i].* TO scr_1[j].* ATTRIBUTE(NORMAL)
                    WHEN 7
                       CALL consulta_indicadores()
                 END CASE

              ON KEY (CONTROL-P)
                 CALL valuacion_siefore()
              ON KEY (CONTROL-F)
                 CALL muestra_fechas()
              ON KEY (CONTROL-T)
                 CALL total_cuenta()
              ON KEY (CONTROL-U)
                 CALL domicilio()
              ON KEY (CONTROL-W)
                 CALL telefono()
              ON KEY (CONTROL-E)
                 CALL consulta_indicadores()

              ON KEY ( INTERRUPT )
	              INITIALIZE det1 TO NULL
                 INITIALIZE enc.* TO NULL
                 LET tmonto_pesos    = NULL
                 LET tmonto_vivienda = NULL
                 LET tcuenta_pesos   = NULL
                 #DISPLAY "" AT 22,35
         		  CLEAR FORM
		           EXIT INPUT
         END INPUT
      END WHILE
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS ..."
      SLEEP 2
      ERROR ""
   END IF
{
 	DISPLAY ARRAY det1 TO scr_1.*
   ON KEY ( INTERRUPT )
	   INITIALIZE det1 TO NULL
      INITIALIZE enc.* TO NULL
         LET tmonto_pesos    = NULL
         LET tmonto_vivienda = NULL
         LET tcuenta_pesos   = NULL
         DISPLAY "" AT 22,35
		   CLEAR FORM
		   EXIT DISPLAY
	ON KEY ( CONTROL-M )
	   LET i = ARR_CURR()
	   LET j = SCR_LINE()
	   DISPLAY det1[i].* TO scr_1[j].* ATTRIBUTE(REVERSE)
	   SELECT subct_desc
      INTO   desc_larga
      FROM   tab_subcuenta
	   WHERE  subct_cod = det1[i].subct_cod
#arc
      #DISPLAY "TOTAL HISTORICO" AT 18,58 ATTRIBUTE(REVERSE)
	   DISPLAY BY NAME desc_larga ATTRIBUTE(REVERSE)
	   CALL Detalle(det1[i].subct_cod)
	DISPLAY "[Enter] Detalle Movimientos Subcuenta   [CTRL-V] Acciones"
	AT 2,20
		   DISPLAY det1[i].* TO scr_1[j].* ATTRIBUTE(NORMAL)
		ON KEY ( CONTROL-V )
		   LET i = ARR_CURR()
		   CALL Despliega_monto_acciones(det1[i].subct_corta,
	END DISPLAY
}
END FUNCTION

FUNCTION despliega_array2(p_cur_row,det1,enc)

   DEFINE sql_stat      INTEGER,
          p_cur_row     SMALLINT,
          item_row_cnt  SMALLINT,
          sql_text      CHAR(650),
          row_cnt       SMALLINT,
          xnombre_sie   CHAR(8),
          x_tablas      CHAR(20)

   DEFINE det1  RECORD
       subct_prueba     CHAR(1),
       subct_cod        SMALLINT,
       subct_corta      CHAR(5),
       monto_pesos      DECIMAL(16,6),
       monto_acciones   DECIMAL(16,6)
   END RECORD

   DEFINE enc  RECORD
      n_seguro  CHAR(11)
   END RECORD
 
   DEFINE cuenta SMALLINT
   DEFINE rrpesos_viv  DECIMAL(12,2)

   DECLARE xmul_sie CURSOR FOR
   SELECT siefore,
          "",
          SUM(monto_en_pesos),
          SUM(monto_en_acciones)
   FROM   dis_cuenta
   WHERE  nss       = enc.n_seguro
   AND    subcuenta = det1.subct_cod
   GROUP BY 1
   ORDER BY 1

   LET xsiefore  = 0
   LET xprecio   = 0
   LET xacciones = 0
   LET xpesos    = 0

   WHENEVER ERROR CONTINUE
      OPEN xmul_sie
   WHENEVER ERROR STOP

   LET sql_stat = SQLCA.SQLCODE
   LET row_cnt = 1
   LET cuenta = 0

   WHILE ((NOT sql_stat) AND (row_cnt<= 100))
      WHENEVER ERROR CONTINUE
         LET det2[row_cnt].acciones_sie = 0 
         LET det2[row_cnt].pesos_sie    = 0
         LET det2[row_cnt].siefore_nom  = ""
         FETCH xmul_sie INTO det2[row_cnt].*
            IF det2[row_cnt].siefore_cod = 0 THEN
               LET det2[row_cnt].siefore_cod  = 0
               LET det2[row_cnt].acciones_sie = 0
               LET det2[row_cnt].siefore_nom  = ""
               LET det1.monto_acciones = det2[row_cnt].acciones_sie
               LET det1.monto_pesos    = det2[row_cnt].pesos_sie
            ELSE
               SELECT precio
               INTO   xprecio
               FROM   precio_dia
               WHERE  codigo = det2[row_cnt].siefore_cod
               --- 2 decimales aivs
               IF det2[row_cnt].siefore_cod = 11 OR
                  det2[row_cnt].siefore_cod = 12 THEN
                  LET rrpesos_viv  = det2[row_cnt].acciones_sie *
                                     xprecio
                  LET det2[row_cnt].pesos_sie = rrpesos_viv
               ELSE
                  LET det2[row_cnt].pesos_sie = det2[row_cnt].acciones_sie *
                                                xprecio
               END IF
               SELECT razon_social
               INTO   xnombre_sie
               FROM   tab_siefore_local
               WHERE  codigo_siefore = det2[row_cnt].siefore_cod

               LET det2[row_cnt].siefore_nom  = xnombre_sie
               LET xnombre_sie = ""
---arc

               ---acumula
               LET det1.monto_acciones = det1.monto_acciones +
                                         det2[row_cnt].acciones_sie
               LET det1.monto_pesos    = det1.monto_pesos    +
                                         det2[row_cnt].pesos_sie
               IF det2[row_cnt].acciones_sie = 0 and
                  det2[row_cnt].pesos_sie    = 0 then
                  LET det2[row_cnt].siefore_cod  = 0
                  LET det2[row_cnt].siefore_nom  = ""
                  IF row_cnt > 1 THEN
                     LET det2[row_cnt].siefore_cod  = ""
                     LET det2[row_cnt].siefore_nom  = ""
                     LET det2[row_cnt].pesos_sie    = ""
                     LET det2[row_cnt].acciones_sie = ""
                  END IF
                     --arc prueba
               END IF
            END IF
      WHENEVER ERROR STOP

      LET sql_stat = SQLCA.SQLCODe

      IF (NOT sql_stat) THEN
         LET row_cnt = row_cnt + 1
      END IF
      LET cuenta = cuenta + 1
   END WHILE

   LET cuenta = cuenta - 1

   IF (sql_stat = 100) THEN
      LET sql_stat = 0
   END IF

   LET item_row_cnt = row_cnt - 1

   CALL dis_six_items(cuenta)
END FUNCTION

FUNCTION dis_six_items(cuenta)
   DEFINE i,cuenta   SMALLINT

   FOR i = 1 TO cuenta
      DISPLAY det2[i].* TO scr_3[i].*
   END FOR
END FUNCTION
FUNCTION valuacion_siefore()
#vs-------------------------

	DEFINE det9 ARRAY[20] OF RECORD
	    siefore            SMALLINT,
	    nsiefore           CHAR(17),
	    precio             DECIMAL(16,14)
 	END RECORD
	DEFINE i			SMALLINT

	DISPLAY "                                                                               " AT 15,1 ATTRIBUTE(REVERSE)
	OPEN WINDOW v11 AT  16,43 WITH FORM "CTACL0014"
	DISPLAY "    SIEFORE              PRECIO      " AT 2,1 ATTRIBUTE (REVERSE)
	DISPLAY "    VALUACION AL DIA  ",vfecha_accion USING "DD-MM-YYYY","     "  AT 1,1 ATTRIBUTE (REVERSE)

	DECLARE cursor_22 CURSOR FOR
	   SELECT codigo,
             "",
             precio
      FROM   precio_dia
      #WHERE  codigo <> 0
	   ORDER BY 1

	LET i = 1
	FOREACH cursor_22 INTO det9[i].*
       SELECT razon_social
       INTO   det9[i].nsiefore
       FROM   tab_siefore_local
       WHERE  codigo_siefore = det9[i].siefore
	    LET i = i + 1
	END FOREACH
	ERROR ""
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY det9 TO scr_1.*
		ON KEY ( control-m )
		   INITIALIZE det9 TO NULL
		   EXIT DISPLAY
	END DISPLAY
   CLOSE WINDOW v11
	DISPLAY "  FECHA     MOVTO DESC  ID APORTANTE  SIEFORE   MONTO PESOS    MONTO ACCIONES  " AT 15,1 ATTRIBUTE(REVERSE)
END FUNCTION
FUNCTION muestra_fechas()
#mf----------------------

	DEFINE det10 ARRAY[20] OF RECORD
	    apertura           DATE,
	    registro           DATE,
	    retiro             DATE,
	    primer_aporte      DATE,
            vol_patron         DATE,
            vol_ventanilla     DATE
 	END RECORD
	DEFINE i			SMALLINT

	DISPLAY "                                                                               " AT 15,1 ATTRIBUTE(REVERSE)
	OPEN WINDOW v12 AT   16,43 WITH FORM "CTACL0015"
	DISPLAY "    FECHAS DE LA CUENTA INDIVIDUAL   " AT 1,1 ATTRIBUTE (REVERSE)

	DECLARE cursor_23 CURSOR FOR
           SELECT "",
                  "",
                  "",
                  fecha_pri_rcv,
                  "",
                  ""
           FROM   cta_ctr_cuenta
           WHERE  nss = enc.n_seguro

	   LET i = 1
	FOREACH cursor_23 INTO det10[i].*

           SELECT MAX(fecha_ult_ret)
           INTO   det10[i].retiro
           FROM   ret_cta_vol
           WHERE  n_seguro = enc.n_seguro

           LET det10[i].vol_patron     = NULL
           LET det10[i].vol_ventanilla = NULL

           SELECT "X"
           FROM   taa_rcv_recepcion
           WHERE  nss = enc.n_seguro
           GROUP BY 1

           IF SQLCA.SQLCODE = 0 THEN
              DECLARE tra_1 CURSOR FOR
              SELECT fecha_vol_pat,
                     fecha_vol_ven 
              FROM   taa_rcv_recepcion
              WHERE  nss = enc.n_seguro
              AND    fecha_mov_banxico = e
              AND    ident_operacion = '09'

              FOREACH tra_1 INTO det10[i].vol_patron,
                                 det10[i].vol_ventanilla
                 EXIT FOREACH
              END FOREACH
           END IF

           --LET det10[i].registro = d
           --LET det10[i].apertura = e
           LET det10[i].registro = e
           LET det10[i].apertura = d
 
	   LET i = i + 1
	END FOREACH
	ERROR ""
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY det10 TO scr_4.*
		ON KEY ( control-m )
		   INITIALIZE det10 TO NULL
		   EXIT DISPLAY
	END DISPLAY
   CLOSE WINDOW v12
	DISPLAY "  FECHA     MOVTO DESC  ID APORTANTE  SIEFORE   MONTO PESOS    MONTO ACCIONES  " AT 15,1 ATTRIBUTE(REVERSE)
END FUNCTION
FUNCTION total_cuenta()
#tc--------------------
	DEFINE det11 ARRAY[10] OF RECORD
	    sar                DECIMAL(16,6),
	    vivienda           DECIMAL(16,6),
	    total              DECIMAL(16,6)
 	END RECORD
	DEFINE i			SMALLINT

	DISPLAY "                                                                               " AT 15,1 ATTRIBUTE(REVERSE)
	OPEN WINDOW v13 AT  16,2  WITH FORM "CTACL0016"
	DISPLAY "  TOTAL CUENTA INDIVIDUAL       " AT 1,1 ATTRIBUTE (REVERSE)

	DECLARE cursor_tc CURSOR FOR
	   SELECT "",
                  "",
                  precio
           FROM   precio_dia
           WHERE  codigo = 1
	   ORDER BY 1

	LET i = 1
	FOREACH cursor_tc INTO det11[i].*
 
      LET det11[i].sar      = tcuenta_sar
      LET det11[i].vivienda = tcuenta_viv
      LET det11[i].total    = tcuenta_pesos
      LET i = i + 1
	END FOREACH
	ERROR ""
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY det11 TO scr_5.*
		ON KEY ( control-m )
		   INITIALIZE det11 TO NULL
		   EXIT DISPLAY
	END DISPLAY
   CLOSE WINDOW v13
	DISPLAY "  FECHA     MOVTO DESC  ID APORTANTE  SIEFORE   MONTO PESOS    MONTO ACCIONES  " AT 15,1 ATTRIBUTE(REVERSE)
END FUNCTION
{
FUNCTION ayuda()
#ay-------------

	DEFINE det12 ARRAY[20] OF RECORD
	    control            CHAR(9),
	    descripcion        CHAR(17)
 	END RECORD
	DEFINE i			SMALLINT

	DISPLAY "                                                                               " AT 15,1 ATTRIBUTE(REVERSE)
	OPEN WINDOW v14 AT  16,40 WITH FORM "CTACL0017"
	DISPLAY " VENTANAS DE CONSULTA DE INFORMACION " AT 1,1 ATTRIBUTE (REVERSE)

	DECLARE ayu_1 CURSOR FOR
	   SELECT *
      FROM   ayuda
	   ORDER BY 1

	LET i = 1
	FOREACH ayu_1 INTO det12[i].*
      IF det12[i].descripcion = "DETALLE" THEN
         LET det12[i].control = "ENTER"
      END IF
	   LET i = i + 1
	END FOREACH
	ERROR ""
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY det12 TO scr_5.*
		ON KEY ( control-m )
		   INITIALIZE det12 TO NULL
		   EXIT DISPLAY
	END DISPLAY
   CLOSE WINDOW v14
	DISPLAY "  FECHA     MOVTO DESC  ID APORTANTE  SIEFORE   MONTO PESOS    MONTO ACCIONES  " AT 15,1 ATTRIBUTE(REVERSE)
END FUNCTION
}
FUNCTION  ayuda()
    DEFINE pos        INTEGER
    DEFINE vcontrol   CHAR(09)
    DEFINE cla_where  CHAR(50)
    DEFINE sel_where  CHAR(50)
    DEFINE l_record  ARRAY[10] OF RECORD
        control         CHAR(09),
        control_num     SMALLINT,
        descripcion     CHAR(20)
    END RECORD

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
	DISPLAY "                                                                               " AT 15,1 ATTRIBUTE(REVERSE)
	OPEN WINDOW v14 AT  16,30 WITH FORM "CTACL0017"
	   DISPLAY "   VENTANAS DE CONSULTA DE INFORMACION   " AT 1,1 ATTRIBUTE (REVERSE)
      DISPLAY "Escoja con [ENTER] la consulta requerida " AT 2,1
   
      LET int_flag = FALSE

      CONSTRUCT cla_where   ON control 
                          FROM control

         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW v14
         RETURN
      END IF

      LET sel_where = "SELECT * FROM ayuda WHERE ",cla_where CLIPPED,
                      "ORDER BY 1 "
  
      PREPARE query1 FROM sel_where

      DECLARE ven_1 CURSOR FOR query1

      LET pos = 1
      FOREACH ven_1 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1) 
          DISPLAY ARRAY l_record TO scr_5.* 

             ON KEY (CONTROL-M)
                LET pos = ARR_CURR()
                LET vcontrol     = l_record[pos].control
                EXIT DISPLAY

             ON KEY (CONTROL-C)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

             ON KEY (INTERRUPT)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

          END DISPLAY
          CLOSE WINDOW v14
       ELSE
          ERROR "EL PROCESO NO EXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW v14
          RETURN
       END IF

    END IF

    RETURN l_record[pos].control_num
END FUNCTION

FUNCTION domicilio()
#do-----------------
	DEFINE det13 ARRAY[20] OF RECORD
	    calle              CHAR(40),
	    numero             CHAR(10),
	    depto              CHAR(10),
	    colonia            CHAR(60),
	    delega             INTEGER,
       delega_desc        CHAR(40),
       ciudad             SMALLINT,
       ciudad_desc        CHAR(40),
       estado             SMALLINT,
       estado_desc        CHAR(25),
       codpos             CHAR(5)
 	END RECORD
	DEFINE xsolicitud		SMALLINT
	DEFINE xfolio			DECIMAL(8)
	DEFINE i			      SMALLINT

	OPEN WINDOW v15 AT   7,30 WITH FORM "CTACL0018"
	DISPLAY "  DOMICILIO REGISTRADO PARA ENVIO DE INFORMACION  " AT 1,1 ATTRIBUTE (REVERSE)

   LET xfolio     = ""
   LET xsolicitud = ""

   SELECT n_folio,
          tipo_solicitud
   INTO   xfolio,
          xsolicitud
   FROM   afi_mae_afiliado
   WHERE  n_seguro = enc.n_seguro

	DECLARE cursor_dom CURSOR FOR
      SELECT calle,       
             numero,
             depto,
             colonia,
             delega,
             "",  #delega_desc,
             ciudad,
             "",  #ciudad_desc,
             estado,
             "",  #estado_desc,
             codpos      
      FROM   afi_domicilio
      WHERE  nss = enc.n_seguro
      AND    n_folio = xfolio
      AND    tipo_solicitud = xsolicitud
      AND    marca_envio = "X"

	LET i = 1
	FOREACH cursor_dom INTO det13[i].*
       SELECT deleg_desc
       INTO   det13[i].delega_desc
       FROM   tab_delegacion
       WHERE  deleg_cod = det13[i].delega

       SELECT ciudad_desc
       INTO   det13[i].ciudad_desc
       FROM   tab_ciudad
       WHERE  ciudad_cod = det13[i].ciudad

       SELECT estad_desc
       INTO   det13[i].estado_desc
       FROM   tab_estado
       WHERE  estad_cod = det13[i].estado

	    LET i = i + 1
	END FOREACH
	ERROR ""
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY det13 TO scr_6.*
		ON KEY ( control-m )
		   INITIALIZE det13 TO NULL
		   EXIT DISPLAY
	END DISPLAY
   CLOSE WINDOW v15
END FUNCTION
FUNCTION telefono()
#tel----------------
	DEFINE det14 ARRAY[20] OF RECORD
      telefono             char(40),
      tel_cod              smallint,
      cve_lada             char(3),
      extension            char(5)
 	END RECORD
	DEFINE xsolicitud		SMALLINT
	DEFINE xfolio			DECIMAL(8)
	DEFINE i			      SMALLINT

	DISPLAY "                                                                               " AT 15,1 ATTRIBUTE(REVERSE)
	OPEN WINDOW v16 AT  16,43 WITH FORM "CTACL0019"
	DISPLAY "   TELEFONOS DEL TRABAJADOR          " AT 1,1 ATTRIBUTE (REVERSE)

   LET xfolio     = ""
   LET xsolicitud = ""

   SELECT n_folio,
          tipo_solicitud
   INTO   xfolio,
          xsolicitud
   FROM   afi_mae_afiliado
   WHERE  n_seguro = enc.n_seguro

	DECLARE cursor_tel CURSOR FOR
      SELECT telefono,
             tel_cod,
             cve_lada,
             extension 
      FROM   afi_telefono
      WHERE  nss = enc.n_seguro
      AND    n_folio = xfolio
      AND    tipo_solicitud = xsolicitud

	LET i = 1
	FOREACH cursor_tel INTO det14[i].*
	    LET i = i + 1
	END FOREACH
	ERROR ""
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY det14 TO scr_7.*
		ON KEY ( control-m )
		   INITIALIZE det14 TO NULL
		   EXIT DISPLAY
	END DISPLAY
   CLOSE WINDOW v16
	DISPLAY "  FECHA     MOVTO DESC  ID APORTANTE  SIEFORE   MONTO PESOS    MONTO ACCIONES  " AT 15,1 ATTRIBUTE(REVERSE)
END FUNCTION
FUNCTION consulta_indicadores()
#ci----------------------------
	DEFINE xindicador    CHAR(50)

   LET xindicador = 'fglgo CTAC101 ', enc.n_seguro CLIPPED
   RUN xindicador

END FUNCTION
FUNCTION Detalle(x_subct)
#d-----------------------

   DEFINE x_subct	SMALLINT

   DEFINE r_reg ARRAY[2000] OF    RECORD
         f_pago	        DATE,
         tipo_mvto      INTEGER,
         desc_mvto      CHAR(14),
         id_aportante   CHAR(11),                               #754
         siefore        SMALLINT,
         mto_pesos      DECIMAL(16,6),
         mto_accion    	DECIMAL(16,6),
         folio_sua      CHAR(6),
         folio          INTEGER,
         consecutivo    INTEGER
   END RECORD
   
   DEFINE p,j,x_key,
          dias_cs_acum,
          dias_cs_efec	SMALLINT

   DEFINE x_concepto    CHAR(45),
          ano, mes      CHAR(4)

   DEFINE final         RECORD
          folio        	   LIKE dis_cuenta.folio,
          folio_sua        LIKE dis_cuenta.folio_sua,
          periodo_pago     LIKE dis_det_aporte.periodo_pago,
          fecha_valor  	   LIKE dis_cuenta.fecha_valor,
          fecha_conversion	LIKE dis_cuenta.fecha_conversion,
          salario_diario	LIKE dis_det_aporte.ult_salario_diario,
          precio_accion    LIKE dis_cuenta.precio_accion,
          dias_cotizados 	LIKE dis_cuenta.dias_cotizados,
          id_aportante     LIKE dis_cuenta.id_aportante
   END RECORD

   DEFINE fc6_pago         CHAR(08)
   DEFINE fc10_pago        CHAR(10)
	
   IF x_subct = 4 OR x_subct = 8 OR x_subct = 14 THEN
      LET txt_cursor = " SELECT fecha_conversion, ", 
                            " tipo_movimiento, ",
                            " '', ",
                            " id_aportante, ",                    #754
                            " siefore, ",
                            " monto_en_pesos, ",
                            " monto_en_acciones, ",
                            " folio_sua, ",
                            " folio, ",
                            " consecutivo_lote ",
                       " FROM   dis_cuenta ",
                       " WHERE  nss       = ",'"',enc.n_seguro,'"',
                       " AND    subcuenta = ", x_subct CLIPPED,
                       #" AND    tipo_movimiento NOT IN (888) ",
                       " ORDER  BY 1 DESC,4,2 " CLIPPED
   ELSE
      LET txt_cursor = " SELECT fecha_conversion, ", 
                            " tipo_movimiento, ",
                            " '', ",
                            " id_aportante, ",                    #754
                            " siefore, ",
                            " monto_en_pesos, ",
                            " monto_en_acciones, ",
                            " folio_sua, ",
                            " folio, ",
                            " consecutivo_lote ",
                     " FROM   dis_cuenta ",
                     " WHERE  nss       = ",'"',enc.n_seguro,'"',
                     " AND    subcuenta = ", x_subct CLIPPED,
                     #" AND    tipo_movimiento NOT IN (888) ",
                     " ORDER  BY 1 DESC,4,2 " CLIPPED
   END IF

   INITIALIZE r_reg TO NULL

   PREPARE sql_3 FROM txt_cursor
   DECLARE cursor_3 CURSOR FOR sql_3

   LET j = 1
   LET tmonto_pesos    = 0
   LET tmonto_acciones = 0

   FOREACH cursor_3 INTO r_reg[j].*
      LET tmonto_pesos    = tmonto_pesos    + r_reg[j].mto_pesos
      LET tmonto_acciones = tmonto_acciones + r_reg[j].mto_accion
      SELECT descripcion
      INTO   r_reg[j].desc_mvto
      FROM   tab_movimiento
      WHERE  codigo = r_reg[j].tipo_mvto
      LET j = j + 1
   END FOREACH
   IF j >1 THEN
      ----- totales historicos
      #DISPLAY tmonto_pesos TO ttmonto_pesos
      #DISPLAY tmonto_acciones TO ttmonto_acciones
      CALL SET_COUNT(j-1)
      #DISPLAY "[Enter] Detalle Movimientos Subcuenta    [CTRL-V] Acciones    "
      #AT 2,17
      DISPLAY "[Enter] Datos Complementarios Movimiento                      "
      AT 2,17

      SELECT descripcion
      INTO   x_concepto
      FROM   tab_movimiento
      WHERE  codigo = r_reg[j].tipo_mvto

      #DISPLAY x_concepto AT 22,35 ATTRIBUTE(REVERSE)

      DISPLAY ARRAY r_reg TO scr_2.*
{
         #ON KEY ( UP )
         ON KEY ( RIGHT )
           SELECT descripcion
           INTO   x_concepto
           FROM   tab_movimiento
           WHERE  codigo = r_reg[j].tipo_mvto

           DISPLAY x_concepto AT 22,35 ATTRIBUTE(REVERSE)

         #ON KEY ( DOWN )
         ON KEY ( LEFT )
           SELECT descripcion
           INTO   x_concepto
           FROM   tab_movimiento
           WHERE  codigo = r_reg[j].tipo_mvto

           DISPLAY x_concepto AT 22,35 ATTRIBUTE(REVERSE)

           LET x_key = fgl_keyval("DOWN")
}
         ON KEY ( CONTROL-M )
            LET j = ARR_CURR()

            SELECT descripcion
            INTO   x_concepto
            FROM   tab_movimiento
            WHERE  codigo = r_reg[j].tipo_mvto
  
            #DISPLAY x_concepto AT 22,35 ATTRIBUTE(REVERSE)

      IF x_subct = 7  OR
         x_subct = 8  THEN
          SELECT ct.folio,
                 ct.folio_sua,
                 " ",
                 ct.fecha_valor,
                 ct.fecha_conversion,
                 0,
                 ct.precio_accion,
                 ct.dias_cotizados,
                 ct.id_aportante
          INTO   final.*
          FROM   dis_cuenta ct
          WHERE  ct.nss                = enc.n_seguro
          AND    ct.subcuenta          = x_subct
          AND    ct.tipo_movimiento    = r_reg[j].tipo_mvto
          AND    ct.monto_en_pesos     = r_reg[j].mto_pesos
          AND    ct.consecutivo_lote   = r_reg[j].consecutivo
       END IF
            
       IF r_reg[j].tipo_mvto = 3 OR r_reg[j].tipo_mvto= 100 THEN
	   SELECT ct.folio,
	          ct.folio_sua,
                  " ",                     #ct.periodo_pago
	          ct.fecha_valor,
	          ct.fecha_conversion,
                  "0",
	          ct.precio_accion,
	          ct.dias_cotizados,
	          ct.id_aportante
	   INTO   final.*
	   FROM   dis_cuenta ct 
	   WHERE  ct.nss              = enc.n_seguro
	   AND    ct.subcuenta        = x_subct
	   AND    ct.tipo_movimiento  = r_reg[j].tipo_mvto
	   AND    ct.fecha_conversion = r_reg[j].f_pago
           AND    ct.folio_sua        = r_reg[j].folio_sua 
           AND    ct.monto_en_pesos   = r_reg[j].mto_pesos
           AND    ct.consecutivo_lote = r_reg[j].consecutivo

        ELSE 
           IF r_reg[j].tipo_mvto > 200 AND 
              r_reg[j].tipo_mvto < 499 THEN
              SELECT ct.folio,
                     ct.folio_sua,
                     " ",  -- ya no existe cuentas ct.periodo_pago
                     ct.fecha_valor,
                     ct.fecha_conversion,
                     "0",
                     ct.precio_accion,
                     ct.dias_cotizados,
                     ct.id_aportante
              INTO   final.*
              FROM   dis_cuenta ct
              WHERE  ct.nss              = enc.n_seguro
              AND    ct.subcuenta        = x_subct
              AND    ct.tipo_movimiento  = r_reg[j].tipo_mvto
              AND    ct.fecha_conversion = r_reg[j].f_pago
              AND    ct.folio_sua        = r_reg[j].folio_sua
              AND    ct.monto_en_pesos   = r_reg[j].mto_pesos
              AND    ct.consecutivo_lote = r_reg[j].consecutivo
           ELSE  
              LET fc10_pago = r_reg[j].f_pago
              LET fc6_pago  = fc10_pago[07,10],
                              fc10_pago[01,02],
                              fc10_pago[04,05]
			  
              IF x_subct = 5
	      AND r_reg[j].f_pago <= "05/01/2002" THEN
	         SELECT ct.folio,
	                ct.folio_sua,
                        " ",
	                ct.fecha_valor,
	                ct.fecha_conversion,
                        " " ,
	                ct.precio_accion,
	                ct.dias_cotizados,
	                ct.id_aportante
	         INTO   final.*
	         FROM   dis_cuenta ct
	         WHERE  ct.nss              = enc.n_seguro
	         AND  ct.subcuenta        = x_subct
	         AND  ct.tipo_movimiento  = r_reg[j].tipo_mvto
                 AND  ct.fecha_conversion = r_reg[j].f_pago 
	         AND  ct.folio            = r_reg[j].folio
                 AND  ct.monto_en_acciones= r_reg[j].mto_accion
                #AND  ct.consecutivo_lote = r_reg[j].consecutivo
              ELSE
	         SELECT ct.folio,
	                ct.folio_sua,
                        hp.periodo_pago,
	                ct.fecha_valor,
	                ct.fecha_conversion,
                        hp.ult_salario_diario/100 ,
	                ct.precio_accion,
	                ct.dias_cotizados,
	                ct.id_aportante
	         INTO   final.*
	         FROM   dis_cuenta ct, dis_det_aporte hp
	         WHERE  ct.nss               = enc.n_seguro
	         AND    ct.subcuenta         = x_subct
	         AND    ct.tipo_movimiento   = r_reg[j].tipo_mvto
                 AND    ct.fecha_conversion  = r_reg[j].f_pago 
	         AND    ct.folio             = r_reg[j].folio
                 AND    ct.folio_sua         = r_reg[j].folio_sua 
                 AND    ct.monto_en_acciones = r_reg[j].mto_accion
                 AND    ct.consecutivo_lote  = r_reg[j].consecutivo
	         AND    hp.folio             = ct.folio
	         AND    hp.n_seguro          = ct.nss
                 AND    hp.folio_pago_sua    = ct.folio_sua
                 AND    hp.consec_reg_lote   = ct.consecutivo_lote

		 IF STATUS = NOTFOUND THEN
		    IF x_subct = 4  THEN
			      -----aqui fallo
	          SELECT ct.folio,
	                 ct.folio_sua,
                    " ",
	                 ct.fecha_valor,
	                 ct.fecha_conversion,
                    " " ,
	                 ct.precio_accion,
	                 ct.dias_cotizados,
	                 ct.id_aportante
		       INTO   final.*
	          FROM   dis_cuenta ct
	          WHERE  ct.nss              = enc.n_seguro
	          AND    ct.subcuenta        = x_subct
	          AND    ct.tipo_movimiento  = r_reg[j].tipo_mvto
          	 AND    ct.fecha_conversion = r_reg[j].f_pago 
		       AND    ct.folio            = r_reg[j].folio
             AND    ct.monto_en_acciones= r_reg[j].mto_accion
             AND    ct.monto_en_pesos   = r_reg[j].mto_pesos
             AND    ct.consecutivo_lote = r_reg[j].consecutivo
          ELSE
	         SELECT ct.folio,
	                 ct.folio_sua,
                    " ",
	                 ct.fecha_valor,
	                 ct.fecha_conversion,
                    " " ,
	                 ct.precio_accion,
	                 ct.dias_cotizados,
	                 ct.id_aportante
		       INTO   final.*
	          FROM   dis_cuenta ct
	          WHERE  ct.nss              = enc.n_seguro
	          AND    ct.subcuenta        = x_subct
	          AND    ct.tipo_movimiento  = r_reg[j].tipo_mvto
             AND    ct.fecha_conversion = r_reg[j].f_pago 
		       AND    ct.folio            = r_reg[j].folio
             AND    ct.monto_en_acciones= r_reg[j].mto_accion
             AND    ct.consecutivo_lote = r_reg[j].consecutivo
          END IF
                 END IF
              END IF
           END IF
        END IF

                  #IF (x_subct = 4 OR x_subct = 8) AND
                  IF (x_subct = 4 OR x_subct = 8 OR x_subct = 14) AND
                     r_reg[j].tipo_mvto = 3       THEN
                      LET ano = YEAR(final.fecha_conversion) USING "&&&&" 
                      LET mes = MONTH(final.fecha_conversion)-1 USING "&&"	
                      LET final.periodo_pago = ano CLIPPED, mes CLIPPED
                  END IF

                  IF r_reg[j].tipo_mvto < 100 THEN
                      SELECT dias_cotz_bimestre - dias_ausent_bimest
                      INTO   dias_cs_efec
                      FROM   dis_det_aporte
                      WHERE  n_seguro        = enc.n_seguro
                      AND    folio           = final.folio
                      AND    periodo_pago    = final.periodo_pago
                      AND    consec_reg_lote = r_reg[j].consecutivo
                  END IF

                  IF x_subct = 5 THEN
                      IF final.fecha_conversion <= "05/01/2002" THEN
                          SELECT SUM(dias_cotz_bimestre - dias_ausent_bimest -
				      dias_incap_bimest),
			         SUM(dias_cotz_bimestre)
                          INTO   dias_cs_acum,
			         final.dias_cotizados
                          FROM   dis_det_aporte
                          WHERE  folio = final.folio
                          AND    n_seguro        = enc.n_seguro
                          #AND    consec_reg_lote = r_reg[j].consecutivo

		      ELSE

                          SELECT SUM(dias_cotz_bimestre - dias_ausent_bimest -
				      dias_incap_bimest),
			         SUM(dias_cotz_bimestre)
                          INTO   dias_cs_acum,
			         final.dias_cotizados
                          FROM   dis_det_aporte
                          WHERE  n_seguro        = enc.n_seguro
                          AND    consec_reg_lote = r_reg[j].consecutivo
                          AND    folio = final.folio
		      END IF
                      LET dias_cs_efec = dias_cs_acum
                    
                  END IF

		  OPEN WINDOW ventana_4 AT  6,38 WITH 11 ROWS, 42 COLUMNS
		  #ATTRIBUTE(BORDER)
		  DISPLAY "            DATOS COMPLEMENTARIOS             "
                          AT 1,1 ATTRIBUTE(REVERSE)

		  DISPLAY " Folio Interno                 ",
                          final.folio  AT 2,1
		  DISPLAY " Folio SUA                     ",
                          final.folio_sua AT 3,1
		  DISPLAY " Periodo Pago                  ",
                          final.periodo_pago AT 4,1
		  DISPLAY " Fecha Valor                   ",
                          final.fecha_valor      USING "dd-mm-yyyy"  AT 5,1
		  DISPLAY " Fecha Conversion              ",
                          final.fecha_conversion USING "dd-mm-yyyy"  AT 6,1
		  DISPLAY " Salario Integrado Periodo     $",
                          final.salario_diario   USING "##,##&.&&"  AT 7,1
		  DISPLAY " Precio Accion                 $ ",
                          final.precio_accion    USING "##&.&&&&"  AT 8,1
		  DISPLAY " Dias Cotizados / Efectivos    ",
                          final.dias_cotizados," / ", dias_cs_efec   AT 9,1
		  DISPLAY " Identificacion Aportante      ",
                          final.id_aportante                         AT 10,1
                  IF x_subct = 5 THEN
		                  DISPLAY " Dias Acumulados Efectivos CS  ",
                          dias_cs_acum                               AT 9,1
                  END IF

	          PROMPT " Presione [Enter] para Salir" FOR CHAR aux_pausa
		  CLOSE WINDOW ventana_4

             ON KEY ( INTERRUPT )
                  LET tmonto_pesos    = NULL
                  LET tmonto_acciones = NULL
                  #DISPLAY tmonto_pesos TO ttmonto_pesos
                  #DISPLAY tmonto_acciones TO ttmonto_acciones
                  #DISPLAY "" AT 22,35

	          INITIALIZE r_reg TO NULL
		  #FOR p = 1 TO 8
		  #FOR p = 1 TO 7
		  FOR p = 1 TO 6
	        DISPLAY r_reg[p].* TO scr_2[p].*
		  END FOR
		  EXIT DISPLAY
	     END DISPLAY
    ELSE
        ERROR "NO HAY MOVIMIENTOS"
    END IF
    LET desc_larga = NULL
    #DISPLAY BY NAME desc_larga
END FUNCTION

FUNCTION Despliega_a_todos()
	DEFINE aux_pausa	CHAR(1)
	DEFINE txt      	CHAR(300)
	DEFINE n_busqueda	CHAR(50)
	DEFINE l_reg ARRAY[2000] OF RECORD
	      n_seguro		CHAR(11),
	      n_unico		CHAR(18),
	      n_rfc		CHAR(13),
	      nombre		CHAR(50)
	END RECORD
	DEFINE i		SMALLINT
	DEFINE HACER 		SMALLINT
	DEFINE pat,mat,nom	CHAR(50)

	OPEN WINDOW v1 AT  4,4 WITH FORM "CTACL0013" ATTRIBUTE(BORDER)
	DISPLAY "          USTED PUEDE UTILIZAR EL * (ASTERICO) COMO COMODIN                " AT 1,1 ATTRIBUTE(REVERSE)
   	LET HACER = TRUE
	INPUT BY NAME n_busqueda
	      AFTER FIELD n_busqueda
		  IF n_busqueda IS NULL THEN
		      NEXT FIELD n_busqueda
		  END IF
		  LET txt = NULL
		  LET txt = "SELECT n_seguro,n_unico,n_folio,paterno,materno,",
		      "nombres FROM afi_mae_afiliado WHERE paterno MATCHES ",'"',n_busqueda CLIPPED,'"' CLIPPED," ORDER BY 4,5" CLIPPED
		    EXIT INPUT
		ON KEY ( INTERRUPT )
		   LET HACER = FALSE
		   EXIT INPUT
	END INPUT
	IF HACER THEN
	   PREPARE cur10 FROM txt
	   DECLARE crsor_101 cursor FOR cur10
	   LET i = 1
	   FOREACH crsor_101 INTO l_reg[i].n_seguro,l_reg[i].n_unico,l_reg[i].n_rfc,
			         pat,mat,nom
	           LET l_reg[i].nombre = pat CLIPPED," ",
	                                 mat CLIPPED," ",
	                                 nom CLIPPED
	           LET i = i + 1
		   IF i >= 400  THEN
		      ERROR "Sobrepaso Capacidad Maxima del Arreglo"
		      EXIT FOREACH
		   END IF
	   END FOREACH
           CALL SET_COUNT(i-1)
	   DISPLAY ARRAY l_reg TO scr_1.*
		   ON KEY ( CONTROL-M )
		      LET i = ARR_CURR()
		      LET enc.n_seguro = l_reg[i].n_seguro
		      EXIT DISPLAY
		   ON KEY ( INTERRUPT )
		      LET enc.n_seguro = NULL
		      EXIT DISPLAY
	   END DISPLAY
	END IF
	CLOSE WINDOW v1
END FUNCTION

