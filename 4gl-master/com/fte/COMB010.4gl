################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.             				       #
#Programa COMB010  => PROCESO BATCH ACTUALIZA coduni_n1 en com detalle y resum #
#                  => DE pro_mae_promotor.                                     #
#Fecha             => 24 abril 1998.     				       #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Sistema           => COM. 					               #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE aux_pausa     CHAR(1)
   DEFINE HOY		DATE
   DEFINE g_param_com	RECORD LIKE com_parametro.*
   DEFINE g_usuario	CHAR(8)
   DEFINE g_opcion	CHAR(1)
   DEFINE g_desde	DATE
   DEFINE g_hasta	DATE
   DEFINE g_record RECORD
      fecha_desde	DATE,
      fecha_hasta	DATE
   END RECORD,

   cod_tipo_promo LIKE com_tipo_promotor.cod_tipo_prom,
   nivel          LIKE pro_mae_promotor.nivel,
   codven         LIKE pro_mae_promotor.codven,
   cla_where      CHAR(300),
   cla_sel	  CHAR(300),
   vcomando       SMALLINT,
   vnivel         SMALLINT,
   vcodven        CHAR(10),
   codven_ini     CHAR(10),
   codven_fin     CHAR(10)
END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   LET HOY = DATE

   SELECT *,USER INTO g_param_com.*,g_usuario FROM com_parametro

   OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMB0101" ATTRIBUTE( BORDER)

   DISPLAY " COMB010  ACTUALIZACION GRUPO VENTA EN PROVISION Y RESUMEN                " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

   INPUT BY NAME g_record.*
      AFTER FIELD fecha_desde
	 IF g_record.fecha_desde IS NULL THEN
	    ERROR "Fecha Desde NO puede ser NULA"
	    NEXT FIELD fecha_desde
         END IF
      AFTER FIELD fecha_hasta
         IF g_record.fecha_hasta IS NULL THEN
            ERROR "Fecha Hasta NO puede ser NULA"
	    NEXT FIELD fecha_hasta
	 END IF
	 IF g_record.fecha_hasta < g_record.fecha_desde  THEN
	    ERROR "Fecha Hasta NO puede ser menor que Fecha Desde"
	    NEXT FIELD fecha_desde
	 END IF
         EXIT INPUT

      ON KEY ( INTERRUPT )
	 EXIT PROGRAM
   END INPUT

   CONSTRUCT cla_where ON cod_tipo_prom,codven
                     FROM cod_tipo_prom,codven
      ON KEY (ESC)
         LET vcomando = 2
         EXIT CONSTRUCT
      ON KEY (INTERRUPT)
         LET vcomando=1
         EXIT CONSTRUCT
   END CONSTRUCT

   IF vcomando = 1 THEN
      ERROR "Operacion abortada"
      EXIT PROGRAM 
   END IF

   PROMPT "Deseas emitir Actulizacion [S/N]...." for aux_pausa
   IF aux_pausa MATCHES '[Ss]' THEN
      CALL proceso_principal()
      PROMPT "Proceso Finalizo Normalmente..Presione < ENTER > para Salir"
      FOR CHAR aux_pausa
   ELSE	
      ERROR "Proceso Cancelado" SLEEP 2
   END IF

END MAIN

################################################################################

FUNCTION Proceso_principal()
   DEFINE g_reg  			RECORD
       codven			LIKE afi_mae_afiliado.codven,
       indicador_comision	LIKE afi_mae_afiliado.indicador_comision,
       coduni_n1                LIKE afi_mae_afiliado.coduni_n1
   END RECORD

	DEFINE l_SM		DECIMAL(12,2)
	DEFINE l_NUMSM		DECIMAL(12,2)
	DEFINE AA,num 		SMALLINT
	DEFINE vtipo            CHAR(44)
        DEFINE txt_up           CHAR(300)

        LET cla_sel = "SELECT codven,indicador_comision,agenc_cod ",
                      "FROM pro_mae_promotor,com_tipo_promotor ",
	              "WHERE nivel = cod_tipo_prom AND ",
                      cla_where CLIPPED,
                      " ORDER BY codven"
        LET vtipo = cla_where[6,50] CLIPPED

        ERROR "Procesando Informacion..."

        PREPARE claexe FROM cla_sel
	DECLARE cursor_1 CURSOR FOR claexe

	LET num = 0
	FOREACH cursor_1 INTO g_reg.*
           LET num = num + 1
           LET txt_up = "UPDATE com_comis_resumen ", 
                      "  SET coduni_n1 = ","'",g_reg.coduni_n1,"'",
                     " WHERE codven = ","'",g_reg.codven,"'",
                     " AND fecha_desde >= ","'",g_record.fecha_desde,"'",
                     " AND fecha_hasta <= ","'",g_record.fecha_hasta,"'" CLIPPED

          PREPARE cent_up FROM txt_up
          EXECUTE cent_up 

           LET txt_up = "UPDATE com_comis_detalle ",
                       "  SET  coduni_n1       =","'",g_reg.coduni_n1,"'",
                       " WHERE codven = ","'",g_reg.codven,"'",
                       " AND fentcons BETWEEN ","'",g_record.fecha_desde,"'",
                       " AND ","'",g_record.fecha_hasta,"'" CLIPPED

          PREPARE cent_up2 FROM txt_up
          EXECUTE cent_up2 

--           LET txt_up = "UPDATE afi_mae_afiliado ",
--                       " SET cod_esq_comision =",g_reg.indicador_comision,",",
--                       "     coduni_n1         =","'",g_reg.coduni_n1,"'",",",
--                       "     agenc_cod         =","'",g_reg.coduni_n1,"'",
--                       " WHERE codven = ","'",g_reg.codven,"'",
--                       " AND fentcons BETWEEN ","'",g_record.fecha_desde,"'",
--                       " AND ","'",g_record.fecha_hasta,"'" CLIPPED

--          PREPARE cent_up FROM txt_up
--          EXECUTE cent_up 
            
	  DISPLAY "Numero de Registros Procesados ",num USING "###,###" AT 17,10

	END FOREACH

        ERROR ""
END FUNCTION
