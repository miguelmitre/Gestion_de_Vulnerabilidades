###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => E.F.P 
#Programa          => Listado de Validacion de Recaudacion (ICEFAS)
#Fecha             => 28 Enero 1998
#By                => Jose Manuel Vizcaino
#Sistema           => Contabilidad. 
########################################################################### D
DATABASE safre_af
GLOBALS
     DEFINE hora     DATE
     DEFINE hoy      DATE
     DEFINE vcurp_cod_rech      CHAR(8)
     DEFINE vcont    INTEGER
     DEFINE varchivo   CHAR(13)
     DEFINE vquery              CHAR(500)
     DEFINE resp                CHAR(1)
     DEFINE vfolio              CHAR(4)
     DEFINE vfecha_conversion   DATE
     DEFINE vtransaccion        CHAR(1)
     
     DEFINE gr_prog RECORD
            transaccion       CHAR(5),
	    monto             DECIMAL(16,2),
	    transa_desc       CHAR(25),
	    subct_desc        CHAR(25),
	    folio             INTEGER,
	    debito_credito    CHAR(1)
     END RECORD

     DEFINE G_LISTA     CHAR(250)
     DEFINE imprime     CHAR(250)
     DEFINE g_paramgrales   RECORD LIKE glo_parametro.*
     DEFINE g_usuario   CHAR(8)
END GLOBALS

MAIN

    OPTIONS INPUT  WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I,
    FORM LINE 3
    DEFER INTERRUPT
    CALL Est_carga()
END MAIN

############################################################################
    FUNCTION Est_carga()
############################################################################
   
    LET hora =                 TIME
    LET hoy =                  TODAY

    LET hora = TIME
    LET hoy = TODAY
    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CONTL001" 
    ATTRIBUTE (BORDER)
    DISPLAY " CONTL002   REPORTE PARA VALIDACION DE RECAUDACION (ICEFAS)     " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                           < Ctrl-C > Salir                                " AT 1,2 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,62 ATTRIBUTE(REVERSE)

           SELECT    USER
           INTO g_usuario FROM tab_afore_local

           SELECT * INTO g_paramgrales.*
	   FROM glo_parametro

           PROMPT "Desea efectuar el proceso S/N ? " 
	   ATTRIBUTE (REVERSE)
	   FOR RESP
	   ATTRIBUTE (REVERSE)
	    
  IF resp MATCHES"[sS]" THEN
	   
            INPUT BY NAME vfolio, vtransaccion, vfecha_conversion
              BEFORE FIELD vfolio
		  ERROR "PROPORCIONE EL NUMERO DE FOLIO"
		  ATTRIBUTE (REVERSE)
		  SLEEP 2
		  ERROR " "
	     
	      AFTER FIELD vfolio
		 SELECT unique(folio) FROM con_his_archivo
		   WHERE folio = vfolio
                 IF STATUS = NOTFOUND THEN 
		    ERROR "FOLIO NO CONTABILIZADO ... VERIFIQUE"
		    ATTRIBUTE (REVERSE)
		    SLEEP 2
		    ERROR " "
	            INITIALIZE vfolio TO NULL
		    NEXT FIELD vfolio
		 END IF
	      
	      BEFORE FIELD vtransaccion 
		 ERROR "Teclee (P) Para Provision y (L) Para Liquidacion"
	         ATTRIBUTE (REVERSE)
		 SLEEP 2
	      AFTER FIELD vtransaccion
	         IF vtransaccion = "P" THEN
		    SELECT unique(fecha_contab) INTO vfecha_conversion
			FROM con_his_archivo
                       WHERE folio = vfolio
			 AND transaccion MATCHES"31*"
			 DISPLAY BY NAME vfecha_conversion
		         NEXT FIELD vfecha_conversion
		 END IF
	         
		 IF vtransaccion = "L" THEN
		    SELECT unique(fecha_contab) INTO vfecha_conversion
			FROM con_his_archivo
                       WHERE folio = vfolio
			 AND transaccion MATCHES"32*"
			 DISPLAY BY NAME vfecha_conversion
		         NEXT FIELD vfecha_conversion
		 END IF
	      AFTER FIELD vfecha_conversion
		IF vtransaccion = "P" OR "L" THEN
		   CALL genera_reporte_contable(vtransaccion) 
	        END IF
	    
     END INPUT
   END IF
END FUNCTION
     
###########################################################################    
     FUNCTION genera_reporte_contable(vtransa)
###########################################################################    
       DEFINE vtransa     CHAR(1)

	     --START REPORT listado TO G_LISTA
	     --START REPORT listado TO "/u/FTE/CENVIO/reporte_icefas"
	     START REPORT listado TO "/LISTADOS/reporte_icefas"
	     --START REPORT listado TO "contab.out"
      
      #-------
        IF vtransa = "L" THEN
      #-------
	    ERROR "GENERANDO REPORTE LIQUIDACION APORTACIONES"
             ATTRIBUTE (REVERSE)
	     SLEEP 2 ERROR " "
	       DECLARE c_carga CURSOR FOR 
         SELECT transaccion, round(monto, 2), tab_transaccion.transa_desc,
	        subct_corta, folio, debito_credito
	     FROM con_his_archivo, outer tab_subcuenta, tab_transaccion
           WHERE con_his_archivo.transaccion[5,6] = tab_subcuenta.subct_cod
             AND con_his_archivo.transaccion[1,4] = tab_transaccion.transa_cod
	     AND folio = vfolio
	     AND fecha_conversion = vfecha_conversion
	     AND transaccion MATCHES "32*"
	 ORDER BY 1
            FOREACH c_carga INTO gr_prog.*
                 OUTPUT TO REPORT listado(gr_prog.*)
              END FOREACH 
            --FINISH REPORT listado
                ERROR"LISTADO GENERADO" SLEEP 2
	    LET G_LISTA =  "chmod 777 ",g_paramgrales.ruta_spool CLIPPED,"/",g_usuario CLIPPED,".LISTADO_PROGRAMAS_SISTEMA_SAFRE",HOY USING "dd-mm-yy","_",hora CLIPPED
	    RUN G_LISTA

      END IF
    
     #-------
      IF vtransa = "P" THEN
     #-------
	    ERROR "GENERANDO REPORTE PROVISION APORTACIONES"
             ATTRIBUTE (REVERSE)
	     SLEEP 7
	     ERROR " "
	       
	     DECLARE c_carga2 CURSOR FOR 
         SELECT transaccion, round(monto, 2), tab_transaccion.transa_desc,
	        subct_corta, folio, debito_credito
	     FROM con_his_archivo, outer tab_subcuenta, tab_transaccion
           WHERE con_his_archivo.transaccion[5,6] = tab_subcuenta.subct_cod
             AND con_his_archivo.transaccion[1,4] = tab_transaccion.transa_cod
	     AND folio = vfolio
	     --AND fecha_contab = vfecha_conversion
	     AND transaccion MATCHES "31*"
	 ORDER BY 1
          FOREACH c_carga2 INTO gr_prog.*
		   OUTPUT TO REPORT listado(gr_prog.*)
               END FOREACH 
            FINISH REPORT listado
                ERROR"LISTADO GENERADO" SLEEP 2
	    LET G_LISTA =  "chmod 777 ",g_paramgrales.ruta_spool CLIPPED,"/",g_usuario CLIPPED,".LISTADO_PROGRAMAS_SISTEMA_SAFRE",HOY USING "dd-mm-yy","_",hora CLIPPED
	    RUN G_LISTA

     #-------
      END IF
     #-------

END FUNCTION

#############################################################################
FUNCTION Pregunta()
   DEFINE aux_pausa    CHAR(1)
   
   PROMPT "Desea Emitir Informe S/N ? " FOR CHAR aux_pausa
END FUNCTION

############################################################################
REPORT listado(rpt)
############################################################################

     DEFINE rpt RECORD
            transaccion       CHAR(5),
	    monto             DECIMAL(16,2),
	    transa_desc       CHAR(15),
	    subct_desc        CHAR(25),
	    folio             INTEGER,
            debito_credito    CHAR(1)
     END RECORD

	DEFINE l_estado		CHAR(16)
	DEFINE 
            aux_sexo			CHAR(10)   ,
            razon_social                CHAR(40)

    OUTPUT
        PAGE LENGTH 90
        TOP  MARGIN 0
        BOTTOM MARGIN 0
	LEFT MARGIN 0
	RIGHT MARGIN 0


    FORMAT
    
    PAGE HEADER

	PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"========================================"           ,
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"====="
        PRINT
            COLUMN 001, razon_social,       
            COLUMN 140,"FECHA   :",hoy USING "DD/MM/YY"    
            --COLUMN 149,hora [1,5]
        PRINT
            COLUMN 001, "CONTL001"
        PRINT
	    COLUMN 001, "FOLIO GENERADO :", vfolio USING "&&&"
	PRINT
	    COLUMN 035,"            REPORTE PARA VALIDACION DE RECAUDACION                  ",
            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
	PRINT
            COLUMN 001,"----------------------------------------"           ,
            COLUMN 040,"----------------------------------------"           ,
            COLUMN 080,"----------------------------------------"           ,
            COLUMN 120,"----------------------------------------"           ,
            COLUMN 160,"-----"
       SKIP 1 LINES
        PRINT 
	    COLUMN 001, "Transaccion", 
	    COLUMN 022, "Monto",
	    COLUMN 040, "Descripcion Transaccion",
	    COLUMN 065, "Descripcion Subcuenta",
	    COLUMN 080, "Debito/Credito"

	PRINT
	    COLUMN 001,"========================================"           ,
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"====="

    ON EVERY ROW
         LET vcont = vcont + 1

         PRINT 
	    COLUMN 001, rpt.transaccion,
	    COLUMN 018, rpt.monto,
	    COLUMN 040, rpt.transa_desc,
	    COLUMN 070, rpt.subct_desc,
            COLUMN 080, rpt.debito_credito
     #----------
     ON LAST ROW
     #----------
      
       SKIP 1 LINES 
       PRINT
	     COLUMN 60, "================"
       PRINT 
	 COLUMN 60, "TOTAL GENERAL TRANSACCIONES GENERADAS :", vcont USING "#####"
       SKIP 2 LINES 
       
       ERROR "PROCESO FINALIZADO"
       ATTRIBUTE (REVERSE)
       SLEEP 2
       ERROR " "

            LET G_LISTA =  "/LISTADOS/reporte_icefas"
            LET imprime = "lp ", G_LISTA
            RUN imprime
   
   EXIT PROGRAM
END REPORT
