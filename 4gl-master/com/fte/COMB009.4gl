################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P                                                    #
#Programa COMB009  => PROCESO BATCH CALCULA BONOS EJECUTIVOS(RESUMEN)          #
#Fecha             => 02 noviembre 1997.
#By                => GERARDO ALFONSO VEGA PAREDES.			       #
#Fecha Actualiza   =>                   				       #
#By                =>                              			       #
#Sistema           => COM. 					               #
################################################################################

DATABASE safre_af
GLOBALS
	DEFINE g_param_com		RECORD LIKE com_parametro.*
	DEFINE l_reg  		RECORD LIKE com_comis_respon.*

	DEFINE aux_pausa		,
	       g_opcion			CHAR(1),
	       g_usuario		CHAR(8),
               seltxt			CHAR(400)

	DEFINE HOY			DATE,
               vcomando			SMALLINT,
               opc CHAR(01)

END GLOBALS

################################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT

	SELECT *,USER INTO g_param_com.*,g_usuario FROM com_parametro
	LET HOY = DATE

	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMB0081" ATTRIBUTE( BORDER)
	DISPLAY " COMB009           CALCULO DE BONOS A EJECUTIVOS                               " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

{
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

            ON KEY (ESC)
               LET vcomando = 2

	    ON KEY ( INTERRUPT )
               LET vcomando=1
	END INPUT

        IF vcomando = 1 THEN
           ERROR "Operacion abortada"
           EXIT PROGRAM 
        END IF
}

        PROMPT "Desea emitir Calculo [S/N]...." for aux_pausa
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
#pp-------------------------
	DEFINE vtotal_bono              DECIMAL(12,2),
	       AA,num                   ,
               renglon			SMALLINT,
               vcriterio		SMALLINT,
               vcod_esq_bono    	SMALLINT,
               vcont			INTEGER,
               vporcentaje              DECIMAL(14,2)

        SELECT COUNT(*)
        INTO   vcont
        FROM   com_comis_respon 
   	WHERE  estado_comision = 1

        IF vcont = 0 or vcont IS NULL THEN
     	   ERROR "No existen Registros a ser procesados"
           SLEEP 3
           RETURN
        END IF
        DISPLAY "Numero de registros a procesar:", vcont using "###,##&" AT 9,10 
	DECLARE cursor_1 CURSOR FOR 
	    SELECT a.*
            FROM   com_comis_respon a
	    WHERE  a.estado_comision  = 1
	    ORDER  BY coduni

	LET num = 0
        LET renglon = 11 
	FOREACH cursor_1 INTO l_reg.*
            LET num = num + 1

            CALL busca_comision1(l_reg.coduni,l_reg.nivel)
                 RETURNING vcod_esq_bono,vcriterio
          
            CALL busca_comision(vcriterio,
                                vcod_esq_bono,
                                l_reg.total_comision,
                                l_reg.promedio_sm,
                                l_reg.total_afiliados,
                                l_reg.total_sm)
                 RETURNING vtotal_bono,vporcentaje
		
            IF vtotal_bono IS NOT NULL THEN
 
                INSERT INTO com_bono_respon VALUES(
                   l_reg.cod_resp_uni,       # cod_resp_uni
                   l_reg.coduni,             # coduni
                   l_reg.nivel,              # nivel
                   l_reg.cod_puesto,         # cod_puesto
                   l_reg.codven,             # codven
                   l_reg.fecha_desde,        # fecha_desde
                   l_reg.fecha_hasta,        # fecha_hasta
                   l_reg.fecha_corte,        # fecha_corte
                   l_reg.total_afiliados,    # total_afiliados
                   l_reg.total_sm,           # total_sm
                   l_reg.promedio_sm,        # promedio_sm
                   l_reg.total_comision,     # total_comision
                   vcod_esq_bono,            # cod_esq_bono
                   vporcentaje,              # porcentaje_bono
                   vtotal_bono,              # total_bono
                   0,                        # estado_bono
                   TODAY,                    # fecha_calculo
                   g_usuario,                # usuario 
                   0)                        # consolida
                
                UPDATE com_comis_respon
	        SET    estado_comision = 2               ,  #Calculada
                       fecha_calculo   = TODAY           ,
                       usuario         = g_usuario
                WHERE  cod_resp_uni    = l_reg.cod_resp_uni
                AND    coduni          = l_reg.coduni
                AND    nivel           = l_reg.nivel
                AND    estado_comision = 1
            END IF

	DISPLAY "Registros Procesados : ",num USING "###,##&" AT renglon,21

	END FOREACH

    DISPLAY "Registros Procesados : ",num USING "###,##&" AT renglon,21
END FUNCTION

FUNCTION busca_comision1(pcoduni,pnivel)
#bc-------------------------------------

    DEFINE pcoduni		CHAR(10),
           pnivel		SMALLINT,
           lcod_esq_bono	SMALLINT,
           lcod_resp_uni	CHAR(10),
           lcriterio            SMALLINT

    SELECT tab_puesto.cod_esq_bono,
           com_respon_unidad.cod_resp_uni
    INTO   lcod_esq_bono,
           lcod_resp_uni
    FROM   tab_puesto,
           com_respon_unidad,
           com_dat_uni_com
    WHERE  com_dat_uni_com.cod_uni = pcoduni
    AND    com_dat_uni_com.nivel = pnivel
    AND    com_respon_unidad.cod_resp_uni = com_dat_uni_com.cod_resp_uni
    AND    com_respon_unidad.puesto_resp  = tab_puesto.cod_puesto   

    IF STATUS = NOTFOUND THEN
       ERROR "Responsable Unidad: ",lcod_resp_uni," sin Indicador Bono"
       RETURN 0,0
    ELSE
       SELECT criterio_cod INTO lcriterio
       FROM com_esq_bono
       WHERE cod_esq_bono = lcod_esq_bono

       RETURN lcod_esq_bono,lcriterio
    END IF

END FUNCTION

###############################################################################
FUNCTION busca_comision(pcriterio,pcod_esq_bono,ptotal_comision,ppromedio_sm,
                        ptotal_afiliados,ptotal_sm)
#bc----------------------------------------------------------------------------

    DEFINE pcriterio        		SMALLINT,
           pcod_esq_bono                SMALLINT,
           ptotal_comision              DECIMAL(14,2),
           ppromedio_sm                 DECIMAL(6,2),
           ptotal_afiliados             INTEGER,
           ptotal_sm                    DECIMAL(8,2)

    DEFINE ltotal_comision		DECIMAL(12,2),
           lmonto_comision		DECIMAL(12,2),
           lmonto_comision2		DECIMAL(12,2),
           lporcentaje   		DECIMAL(12,2),
           vmonto                       DECIMAL(12,2)

    LET vmonto = 0
    LET ltotal_comision = 0

    CASE pcriterio
        WHEN 1
            SELECT monto_bono
            INTO   lporcentaje
            FROM   com_cuadro_bono
            WHERE  cod_esq_bono = pcod_esq_bono
            AND    rango_desde     <= ppromedio_sm
            AND    rango_hasta     >= ppromedio_sm
       LET ltotal_comision = ptotal_comision * (lporcentaje/100)
        WHEN 2
            SELECT monto_bono
            INTO   lporcentaje
            FROM   com_cuadro_bono
            WHERE  cod_esq_bono = pcod_esq_bono
            AND    rango_desde     <= pptotal_afiliados 
	    AND    rango_hasta     >= ptotal_afiliados  
       LET ltotal_comision = ptotal_comision * (lporcentaje/100)
        WHEN 3
            SELECT monto_bono
            INTO   lporcentaje
            FROM   com_cuadro_bono
            WHERE  cod_esq_bono = pcod_esq_bono
            AND    rango_desde     <= ptotal_afiliados 
            AND    rango_hasta     >= ptotal_afiliados
         ----   AND    rango_desde     <= ppromedio_sm
         ----   AND    rango_hasta     >= ppromedio_sm
            
        LET ltotal_comision = ptotal_comision * (lporcentaje/100)

       ---- LET ltotal_comision = ptotal_sm * (lporcentaje/100)

        OTHERWISE
            ERROR "Criterio No reconocible"
    END CASE



    RETURN ltotal_comision,lporcentaje

END FUNCTION

###############################################################################
FUNCTION pide_enter()
#pe------------------
	PROMPT "Presione < ENTER > para Salir...." FOR CHAR aux_pausa
END FUNCTION

################################################################## Fin COMB007 
