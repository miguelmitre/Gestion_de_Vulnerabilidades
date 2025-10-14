################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Programa          => COMC014                                                  #
#Descripcion       => CONSULTA COMISIONES DE PROMOTORES                        #
#Fecha             => 09 Mayo 1997.     				       #
#By                => GERARDO ALFONSO VEGA PAREDES.			       #
#Sistema           => COM. 				                       #
#Fecha ult. mod.   => 18 febrero 2003                                          #
#Modif.            => 13 Mayo 2004 Alejandro Ramirez                           #
#Descr.            => Se cambio la consulta de INPUT BY NAME A CONSTRUCT       #
#                  => abriendo asi la posibilidad de consulta.                 #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE 
      g_reg0 RECORD
	codven        CHAR(10),
--	fentcons      DATE,
	fecha_corte   DATE,
	cod_tipo_prom SMALLINT,
	coduni_n1     CHAR(10)
      END RECORD,
      g_param_dis     RECORD LIKE seg_modulo.*,
      w_codigo_afore  LIKE tab_afore_local.codigo_afore,
      g_usuario       CHAR (08),
      g_reg2 RECORD
	vnombres       CHAR(50)
      END RECORD,

      g_reg ARRAY[5500] OF RECORD 
         n_folio     	    LIKE com_comis_detalle.n_folio,
  	 n_seguro           LIKE afi_mae_afiliado.n_seguro,
	 tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud,
	 vfentcons  	    LIKE com_comis_detalle.fentcons,
	 vfecha_corte       LIKE com_comis_detalle.fecha_corte,
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
	 num_sm             DECIMAL(12,2),
         monto_comision     LIKE com_comis_detalle.monto_comision,
         estado             CHAR(03)
      END RECORD, 

      nuevo RECORD 
         n_folio     	    LIKE com_comis_detalle.n_folio,
  	 n_seguro           LIKE afi_mae_afiliado.n_seguro,
	 tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud,
	 vfentcons  	    LIKE com_comis_detalle.fentcons,
	 vfecha_corte       LIKE com_comis_detalle.fecha_corte,
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
	 num_sm             DECIMAL(12,2),
         monto_comision     LIKE com_comis_detalle.monto_comision,
         estado             CHAR(03),
         nombre_cli         CHAR(60)
      END RECORD, 

      g_reg3 ARRAY[1000] OF RECORD
         n_folio     	    LIKE afi_solicitud.n_folio,
  	 n_seguro           LIKE afi_solicitud.n_seguro,
	 tipo_solicitud     LIKE afi_solicitud.tipo_solicitud,
	 frecafor  	    LIKE afi_solicitud.fentcons,
         salario_base_comis LIKE afi_solicitud.salario_base_comis,
         status_interno     LIKE afi_solicitud.status_interno,
	 descripcion        LIKE afi_status_int.estado_desc
      END RECORD, 

   g_reg4 ARRAY[6] OF RECORD	
      n_seguro       LIKE pro_mae_promotor.seguro,
      paterno        LIKE pro_mae_promotor.paterno,
      materno        LIKE pro_mae_promotor.materno,
      nombres        LIKE pro_mae_promotor.nombres

   END RECORD,

   g_reg5 ARRAY[6] OF RECORD	
      n_seguro       LIKE pro_mae_promotor.seguro,
      nombre         CHAR(50)
   END RECORD,

   g_reg6 ARRAY[6] OF RECORD	
      n_folio        LIKE afi_solicitud.n_folio,
      n_seguro       LIKE afi_solicitud.n_seguro,
      tipo_solicitud LIKE afi_solicitud.tipo_solicitud,
      paterno        LIKE afi_solicitud.paterno,
      materno        LIKE afi_solicitud.materno,
      nombres        LIKE afi_solicitud.nombres,
      observacion    LIKE afi_rechaza_cert.observacion
   END RECORD,

   g_reg7 ARRAY[6] OF RECORD	
      n_folio        LIKE afi_solicitud.n_folio,
      n_seguro       LIKE afi_solicitud.n_seguro,
      tipo_solicitud LIKE afi_solicitud.tipo_solicitud,
      nombre         CHAR(50),
      observacion     LIKE afi_rechaza_cert.observacion
   END RECORD,

   nombre CHAR(50),

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
      pagada    DECIMAL(12,2),
      registros INTEGER,
      longitud  integer,
      vcheca    INTEGER,
      ban       SMALLINT,
      vvalor    CHAR(01)
END GLOBALS

MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o
	
	DEFER INTERRUPT

        CALL STARTLOG("COMC013.log")

        SELECT  ruta_rescate
        INTO    g_param_dis.ruta_rescate
        FROM    seg_modulo
        WHERE   modulo_cod='com'

        SELECT  codigo_afore,USER
        INTO    w_codigo_afore,g_usuario
        FROM    tab_afore_local

	LET HOY = TODAY

	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0141" ATTRIBUTE( BORDER)
	DISPLAY " COMC014               CONSULTA  DE  PRODUCCION                                " AT 3,1 ATTRIBUTE(REVERSE) 

	DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 9,1 ATTRIBUTE(REVERSE)

	DISPLAY "                                                    [Enter] Nombre Cliente     " AT 11,1 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 18,1 ATTRIBUTE(REVERSE)

	MENU "CONSULTA"
           COMMAND "Consulta" "Consulta Comisiones"
              CALL Inicializa2()
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
        for i=1 to 6
	   DISPLAY g_reg[i].* TO scr_1[i].*
        end for

	LET total = 0
        LET pagada = 0
	LET registros = 0

	DISPLAY registros,total TO scr_3.*

        CLEAR SCREEN

END FUNCTION

FUNCTION Inicializa2()

   DATABASE safre_af
   WHENEVER ERROR CONTINUE
   DROP TABLE tt_presenta
   DROP TABLE tt_inicial

   WHENEVER ERROR STOP



   LET cla_where=NULL
   LET cla_sel=NULL
   INITIALIZE g_reg0.*,g_reg2.* TO NULL
   DISPLAY "                                                                      " AT 8,20   ---@@
   DISPLAY BY NAME g_reg0.*,g_reg2.*
   LET vcheca = 0
   LET ban = 0

END FUNCTION

FUNCTION Consulta()
   DEFINE
      pat CHAR(40),
      mat char(40),
      nom char(40),
      pos SMALLINT,
      enter char(01) 

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " [ESC] Procesar " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C] Salir " AT 1,63 ATTRIBUTE(REVERSE)


     LET INT_FLAG = FALSE

     CONSTRUCT cla_where
         ON    a.codven,
               a.fecha_corte
         FROM  com_comis_detalle.codven,
               com_comis_detalle.fecha_corte


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

     IF cla_where = " 1=1" THEN
          ERROR "Es preciso ingresar el numero de promotor"
          SLEEP 3
          LET INT_FLAG = FALSE
          ERROR ""
          LET vcomando = 0
          RETURN
     END IF

     IF cla_where[1,13] = "a.fecha_corte" THEN
          ERROR "Es preciso ingresar el numero de promotor"
          SLEEP 3
          LET INT_FLAG = FALSE
          ERROR ""
          LET vcomando = 0
          RETURN
     END IF

     IF cla_where[1,08] = "a.codven" THEN

          LET vvalor = ' '
          LET cla_sel= "SELECT 'x' ",
                      "FROM   com_comis_detalle a ",
                      " WHERE ",cla_where CLIPPED,
                      " GROUP BY 1 "

          PREPARE valida FROM cla_sel
          EXECUTE valida INTO vvalor

          IF vvalor IS NULL THEN
             ERROR "El numero de promotor no existe"
             SLEEP 3
             LET INT_FLAG = FALSE
             ERROR ""
             LET vcomando = 0
             RETURN
          END IF
    END IF


         {
         WHENEVER ERROR CONTINUE
         DROP TABLE tt_inicial
         DROP TABLE tt_presenta

         WHENEVER ERROR STOP
         }


         -- Solo muestra en pantalla los datos del promotor 
         LET cla_sel ="SELECT a.codven,",
                           "a.fecha_corte,",
                           "a.cod_tipo_prom,",
                           "a.coduni_n1,",
                           "trim(b.paterno)||' '||",
                           "trim(b.materno)||' '||trim(b.nombres) as nom ",
                   "FROM   com_comis_detalle a,pro_mae_promotor b ",
                   " WHERE ",cla_where CLIPPED,
             --    " WHERE a.codven = '",g_reg0.codven CLIPPED,
             --    "' AND   a.fecha_corte ='",g_reg0.fecha_corte CLIPPED,
                   "  AND   a.codven = b.cod_promotor ",
                   " ORDER  BY 2 ",
                   " INTO TEMP tt_presenta " 

         PREPARE q7 FROM cla_sel
         EXECUTE q7

         LET vcheca = 0
         SELECT COUNT(*)
         INTO   vcheca
         FROM   tt_presenta

         IF vcheca = 0
         THEN
            WHENEVER ERROR CONTINUE
            DROP TABLE tt_presenta

            WHENEVER ERROR STOP

            -- Solo muestra en pantalla los datos del jefe o responsable 
            LET cla_sel ="SELECT a.codven,",
                           "a.fecha_corte,",
                           "a.cod_tipo_prom,",
                           "a.coduni_n1,",
                           "b.nombre_resp_uni ",
                   "FROM   com_comis_detalle a,com_respon_unidad b ",
                   " WHERE ",cla_where CLIPPED,
              --   " WHERE a.codven = '",g_reg0.codven CLIPPED,
              --   "' AND   a.fecha_corte ='",g_reg0.fecha_corte CLIPPED,
                   "  AND   a.codven = b.cod_resp_uni ",
                   " ORDER  BY 2 ",
                   " INTO TEMP tt_presenta " 
   
            PREPARE q8 FROM cla_sel
            EXECUTE q8
         END IF
 


    --   PREPARE claexe2 FROM cla_sel
    --   DECLARE cursor_2 SCROLL CURSOR FOR claexe2

         DECLARE cursor_2 SCROLL CURSOR FOR SELECT * FROM tt_presenta
         OPEN cursor_2

         CALL primer_row()



         LET cla_sel ="SELECT a.n_folio, ",
                       "a.nss, ",
                       "a.tipo_solicitud, ",
                       "a.fentcons, ",
                       "a.fecha_corte, ",
                       "a.salario_base_comis, ",
                       "a.num_sm, ",
                       "a.monto_comision, ",
                       "a.estado_comision ",
                       "FROM com_comis_detalle a, pro_mae_promotor  c ",
                       "WHERE a.codven       = c.cod_promotor ",
                       "AND   ",cla_where CLIPPED,
                  --   "AND   a.codven       = '",g_reg0.codven CLIPPED,
                  --   "' AND   a.fecha_corte  = '",g_reg0.fecha_corte CLIPPED,
                       "  ORDER BY 2,09 ",
                       "INTO TEMP tt_inicial  "

         PREPARE q1 FROM cla_sel
         EXECUTE q1


         SELECT COUNT(*)
         INTO   vcheca
         FROM   tt_inicial

         IF vcheca = 0
         THEN

            --PARA LA BUSQUEDA ESPECIFICA DE UN JEFE SE REQUIERE:
            --FECHA_CORTE Y NOMINA O NOMBRE
            WHENEVER ERROR CONTINUE
            DROP TABLE tt_inicial

            WHENEVER ERROR STOP

            --VERIFICAMOS SI SE TRATA DE ALGUN JEFE
            LET cla_sel ="SELECT a.n_folio,  ",
                         "a.nss,  ",
                         "a.tipo_solicitud,  ",
                         "a.fentcons,  ",
                         "a.fecha_corte,  ",
                         "a.salario_base_comis,  ",
                         "a.num_sm,  ",
                         "a.monto_comision,  ",
                         "a.estado_comision  ",
                         "FROM   com_comis_detalle a,com_respon_unidad c  ",
                         "WHERE  a.codven = c.cod_resp_uni  ",
                         "AND    ",cla_where CLIPPED,
                    --   "AND    a.fecha_corte = '",g_reg0.fecha_corte CLIPPED,
                    --   "' AND    a.codven = '",g_reg0.codven CLIPPED,
                         "  AND    a.nivel <> 1  ",
                         "ORDER BY 2,09  ",
                         "INTO TEMP tt_inicial  "

            PREPARE q2 FROM cla_sel
            EXECUTE q2

            LET vcheca = 0
            SELECT COUNT(*)
            INTO   vcheca
            FROM   tt_inicial

            IF vcheca > 0 THEN
               LET ban = 1
            ELSE
              ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES..."
              SLEEP 2
            END IF

         END IF

   DISPLAY " [Ctrl-I] Imprime  [Ctrl-E] Descrip estado " AT 8,36 ATTRIBUTE(REVERSE)
	 CALL ver_arreglo()
END FUNCTION

FUNCTION primer_row()
   FETCH FIRST cursor_2 INTO g_reg0.*,g_reg2.*
   IF STATUS=100 THEN
      ERROR "No hay registros en esta direccion"
   ELSE
      CALL despliega_row()
   END IF
END FUNCTION

FUNCTION despliega_row()
   DISPLAY BY NAME g_reg0.*,g_reg2.*
END FUNCTION

FUNCTION imprime()

   DEFINE G_IMPRE    CHAR(2000)
   DEFINE G_LISTA    CHAR(300)
   DEFINE impresion  CHAR(300)
   DEFINE xpaterno2  CHAR(40)
   DEFINE xmaterno2  CHAR(40)
   DEFINE xnombres2  CHAR(40)
   DEFINE i          INTEGER

   LET G_IMPRE = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                              ".RJefe_",HOY USING "DDMMYYYY"

    LET G_LISTA = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                              ".AJefe_",HOY USING "DDMMYYYY"

    -- PARA IMPRESION
    START REPORT rpt_cuenta_imp TO G_IMPRE

 -- FOR i=1 TO 9000
    FOR i=1 TO registros

        LET nuevo.n_folio               = g_reg[i].n_folio
        LET nuevo.n_seguro              = g_reg[i].n_seguro
        LET nuevo.tipo_solicitud        = g_reg[i].tipo_solicitud
        LET nuevo.vfentcons             = g_reg[i].vfentcons
        LET nuevo.vfecha_corte          = g_reg[i].vfecha_corte
        LET nuevo.salario_base_comis    = g_reg[i].salario_base_comis
        LET nuevo.num_sm                = g_reg[i].num_sm
        LET nuevo.monto_comision        = g_reg[i].monto_comision
        LET nuevo.estado                = g_reg[i].estado

   IF nuevo.n_folio IS NULL OR
      nuevo.n_folio = ' ' THEN
      EXIT FOR
   END IF


   SELECT paterno,materno,nombres
   INTO   xpaterno2,xmaterno2,xnombres2
   FROM   afi_mae_afiliado
   WHERE  n_seguro = nuevo.n_seguro

   IF     xpaterno2 IS NULL THEN
          LET xpaterno2 = ' '
   END IF

   IF     xmaterno2 IS NULL THEN
          LET xmaterno2 = ' '
   END IF

   IF     xnombres2 IS NULL THEN
          LET xnombres2 = ' '
   END IF

   LET nuevo.nombre_cli = xpaterno2 CLIPPED||' '||
                          xmaterno2 CLIPPED||' '||
                          xnombres2 CLIPPED
 


   -- PARA LA IMPRESION
   OUTPUT TO REPORT rpt_cuenta_imp(nuevo.*)

   END FOR

   FINISH REPORT rpt_cuenta_imp
   ERROR "LISTADO 1 GENERADO...."
   SLEEP 2
   ERROR ""
 
   LET impresion = "lp ",G_IMPRE
--   RUN impresion


END FUNCTION

REPORT rpt_cuenta_imp(lnuevo)

  DEFINE
    lnuevo RECORD
     n_folio            LIKE com_comis_detalle.n_folio,
     n_seguro           LIKE afi_mae_afiliado.n_seguro,
     tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud,
     vfentcons          LIKE com_comis_detalle.fentcons,
     vfecha_corte       LIKE com_comis_detalle.fecha_corte,
     salario_base_comis LIKE com_comis_detalle.salario_base_comis,
     num_sm             LIKE com_comis_detalle.num_sm,
     monto_comision     LIKE com_comis_detalle.monto_comision,
     estado             CHAR(03),
     nombre_cli         CHAR(45)
   END RECORD

   OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   90
--    ORDER BY lnuevo.estado_comision desc
  FORMAT
     PAGE HEADER

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 02,'\033(s7B',"COMC014                                                                                                                                    ",today USING "dd-mm-yyyy",'\033(s0B'
      SKIP 2 LINE
      PRINT COLUMN 80,'\033(s7B',"ESTADO DE CUENTA DE RESPONSABLE",'\033(s0B'
      SKIP 1 LINE


      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'

      SKIP 1 LINE


      PRINT COLUMN 01,'\033(s7B',"Num promotor  Nombre promotor                    Grp Venta   Fecha corte   ",'\033(s0B'
      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'
      SKIP 1 LINE
      ON EVERY ROW

      PRINT COLUMN 002,lnuevo.n_seguro CLIPPED




END REPORT


FUNCTION ver_arreglo()
   DEFINE
      pat    CHAR(40),
      mat    char(40),
      nom    char(40),
      pos    INTEGER,
      fhasta DATE,
      i      SMALLINT

   LET total = 0
   LET pagada = 0
   LET registros = 0

   display "codven:",g_reg0.codven
   display "fec_corte:",g_reg0.fecha_corte


   LET cla_sel2="SELECT * FROM tt_inicial ORDER BY estado_comision"

         ERROR "Buscando Informacion"

         PREPARE claexe FROM cla_sel2
         DECLARE cursor_1111 CURSOR FOR claexe
	 LET pos = 1
	 FOREACH cursor_1111 INTO g_reg[pos].*

	    LET total = total + g_reg[pos].monto_comision
	    LET registros = registros + 1

	    LET pos = pos + 1
            IF pos >= 9000 THEN
               ERROR "Sobrepaso la capacidad del arreglo"
               EXIT FOREACH
            END IF 
         END FOREACH

         DISPLAY registros,total TO scr_3.*

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg TO scr_1.*
	       ON KEY (CONTROL-E)
                  LET i = ARR_CURR()
		  CALL Busca_estado(g_reg[i].estado) 
	       ON KEY (CONTROL-I)
	          CALL imprime() 
               ON KEY (CONTROL-M)
                  LET i = ARR_CURR()
                  CALL Mostrar_nombre(g_reg[i].n_seguro)
               ON KEY (INTERRUPT)
                  DISPLAY "                                             " AT 8,36
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
-----         DISPLAY "" AT 8,55 ATTRIBUTE(REVERSE)
         call Inicializa()
         CLEAR SCREEN
END FUNCTION


FUNCTION Busca_estado(vestado)
  DEFINE vestado    LIKE com_comis_detalle.estado_comision
  DEFINE pos        INTEGER

  DEFINE l_record   ARRAY[300] OF RECORD
                codigo         SMALLINT,
                descripcion    CHAR(70)
         END RECORD
         DECLARE cursor_1 CURSOR FOR
         SELECT codigo,descripcion FROM tab_edo_comision
         WHERE codigo  = vestado 
         ORDER BY 1
         LET pos = 1
         FOREACH cursor_1 INTO l_record[pos].*
                 LET pos = pos + 1
         END FOREACH
         IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
         -- OPEN WINDOW ventana_2 AT 5,10 WITH FORM "COMC0095" ATTRIBUTE( BORDER)
                                  -- r,c
            OPEN WINDOW ventana_2 AT 5,3 WITH FORM "COMC0095" ATTRIBUTE( BORDER)

 
            DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE,yellow)
            DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,YELLOW)
            DISPLAY "                    Descripcion de Estado                                        " AT 3,1 ATTRIBUTE(REVERSE,green)
 
            DISPLAY ARRAY l_record TO scr_9.*
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW ventana_2
         ELSE
            ERROR "ARCHIVO DE CANAL.... VACIO"
         END IF


END FUNCTION

FUNCTION Busca_pendientes()

   DEFINE registros2 INTEGER,
	  pos        SMALLINT,
	  parte1     CHAR(26),
	  parte2     CHAR(10),
	  parte3     CHAR(100),
          i          SMALLINT

-- DISPLAY "                                        " AT 8,41

   OPEN WINDOW ventana_2 AT 10,2 WITH FORM "COMC0143" 

   DISPLAY "                             ESTADO SOLICITUDES                                " AT 2,1 ATTRIBUTE(REVERSE) 


   DISPLAY "                                                     [Enter] Nombre Cliente    " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)

   IF cla_where[23]="a" AND cla_where[31]="n" THEN
      LET longitud = 0
      LET parte1   = ""
      LET parte2   = ""
   
      LET longitud  = length(cla_where)
      LET parte1    = cla_where[1,26]
      LET parte2    = "a.frecafor"
   
      IF cla_where[38] = "'" THEN
         LET parte3 = cla_where[37,49]
      ELSE
         LET parte3 = cla_where[37,74]
      END IF

      LET cla_where = parte1 CLIPPED," ",parte2,parte3 CLIPPED
   ELSE
      LET longitud = 0
      LET parte1   = ""
      LET parte2   = ""
   
      LET parte1    = cla_where[1,21]
   
      LET cla_where = parte1 CLIPPED

   END IF

   LET registros2 = 0

   LET cla_sel2="SELECT a.n_folio,",
                       "a.n_seguro,",
		       "a.tipo_solicitud,",
                       "a.frecafor,",
                       "a.salario_base_comis,",
                       "a.status_interno, ",
		       "b.estado_desc ",
                "FROM   afi_solicitud a,",
		"       tab_status_afi b ",
                "WHERE  a.status_interno = b.estado_cod ",
----               " AND    status_interno in (40,42,45,50,55) ",
               " AND    status_interno < 100 ",
               " AND    a.frecafor <= '",g_reg0.fecha_corte,
               "' AND a.codven = '",g_reg0.codven CLIPPED,
     	       "' ORDER BY 4,1,2" CLIPPED

         ERROR "Buscando Informacion"

         PREPARE claexe_pend FROM cla_sel2
         DECLARE cursor_pend CURSOR FOR claexe_pend
	 LET pos = 1
	 FOREACH cursor_pend INTO g_reg3[pos].*
		 LET registros2 = registros2 + 1
	         LET pos = pos + 1
                 IF pos >= 9000 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF 
         END FOREACH

         DISPLAY registros2 TO scr_1.*

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg3 TO scr_0.*
               ON KEY (CONTROL-M)
                  LET i = ARR_CURR()
                  CALL Mostrar_nombre2(g_reg3[i].n_folio,
                                       g_reg3[i].n_seguro,
                                       g_reg3[i].tipo_solicitud)
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
----         CLEAR SCREEN
	 CLOSE WINDOW ventana_2
END FUNCTION


FUNCTION Mostrar_nombre(vnss)
   DEFINE vnss    CHAR(11),
          pos     INTEGER

   OPEN WINDOW ventana001 AT 09,2 WITH FORM "COMC0144" 

   DISPLAY "                             NOMBRE DEL CLIENTE                                " AT 2,1 ATTRIBUTE(REVERSE) 

   DISPLAY "                                                              [Ctrl-C] Salir   " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)


   LET cla_sel2="SELECT a.n_seguro,",
                       "a.paterno,",
                       "a.materno,",
                       "a.nombres ",
                "FROM   afi_mae_afiliado a ",
                "WHERE  a.n_seguro  = '",vnss CLIPPED,
                "'"

         ERROR "Buscando Informacion"

         PREPARE claexe_cte2 FROM cla_sel2
         DECLARE cursor_cte2 CURSOR FOR claexe_cte2
	 LET pos = 1
	 FOREACH cursor_cte2 INTO g_reg4[pos].*
            LET g_reg5[pos].n_seguro         = g_reg4[pos].n_seguro
            LET g_reg5[pos].nombre = g_reg4[pos].paterno CLIPPED," ",
                                     g_reg4[pos].materno CLIPPED," ",
                                     g_reg4[pos].nombres CLIPPED
	         LET pos = pos + 1
                 IF pos >= 900 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF 
         END FOREACH

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg5 TO scr_0.*
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
----         CLEAR SCREEN
	 CLOSE WINDOW ventana001
END FUNCTION



FUNCTION Mostrar_nombre2(vfolio,vnss,vsolicitud)
   DEFINE vfolio     INTEGER,
          vnss       CHAR(11),
          vsolicitud SMALLINT,
          pos        INTEGER

   OPEN WINDOW ventana001 AT 10,2 WITH FORM "COMC0145" 

   DISPLAY "                 NOMBRE CLIENTE Y ESTADO AFILIACION                            " AT 2,1 ATTRIBUTE(REVERSE) 

   DISPLAY "                                                              [Ctrl-C] Salir   " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)


   LET cla_sel2="SELECT a.n_folio,",
                       "a.n_seguro,",
                       "a.tipo_solicitud,",
                       "a.paterno,",
                       "a.materno,",
                       "a.nombres,",
                       "d.observacion ",
                "FROM   afi_solicitud a,outer ",
                       "afi_rechaza_cert d, ",
                       "tab_status_afi ",
                "WHERE  a.n_folio  = ",vfolio,
               " AND    a.n_seguro = ","'",vnss,"'",
               " AND    a.tipo_solicitud = ",vsolicitud,
               " AND    d.n_folio = a.n_folio ",
               " AND    d.n_seguro = a.n_seguro ",
               " AND    d.tipo_solicitud = a.tipo_solicitud ",
               " AND    a.status_interno < 100 ",
               " AND    a.status_interno = estado_cod ",
     	       " ORDER BY 1,2" CLIPPED

         ERROR "Buscando Informacion"

         PREPARE claexe_cte7 FROM cla_sel2
         DECLARE cursor_cte7 CURSOR FOR claexe_cte7
	 LET pos = 1
	 FOREACH cursor_cte7 INTO g_reg6[pos].*
            LET g_reg7[pos].n_folio        = g_reg6[pos].n_folio
            LET g_reg7[pos].n_seguro       = g_reg6[pos].n_seguro
            LET g_reg7[pos].tipo_solicitud = g_reg6[pos].tipo_solicitud
     
            LET g_reg7[pos].nombre = g_reg6[pos].paterno CLIPPED," ",
                                     g_reg6[pos].materno CLIPPED," ",
                                     g_reg6[pos].nombres CLIPPED
            LET g_reg7[pos].observacion = g_reg6[pos].observacion

	    LET pos = pos + 1
            IF pos >= 9000 THEN
               ERROR "Sobrepaso la capacidad del arreglo"
               EXIT FOREACH
            END IF 
         END FOREACH

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg7 TO scr_0.*
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
----         CLEAR SCREEN
	 CLOSE WINDOW ventana001
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

	OPEN WINDOW vent_1 AT 07,12 WITH FORM "COMC0142" ATTRIBUTE(BORDER)
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
