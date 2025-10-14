################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P                                                    #
#Programa COMB008  => PROCESO BATCH CALCULA COMISIONES EJECUTIVOS(RESUMEN)     #
#                  => CALCULO BONO 1 O GARANTIZADO     EJECUTIVOS(RESUMEN)     #
#Fecha             => 25 agosto 1997.	 				       #
#By                => HECTOR FERNANDEZ ARCINIEGA.  			       #
#Fecha Actualiza   =>                   				       #
#By                =>                              			       #
#Sistema           => COM. 					               #
################################################################################

DATABASE safre_af
GLOBALS
	DEFINE g_param_com	RECORD LIKE com_parametro.*
	DEFINE l_reg  		RECORD LIKE com_comis_respon.*

	DEFINE aux_pausa		,
	       g_opcion			CHAR(1),
	       g_usuario		CHAR(8),
               seltxt			CHAR(400)

	DEFINE HOY			DATE,
               vcomando			SMALLINT,
               vsal_min   DECIMAL(6,2),
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
	DISPLAY " COMB008           CALCULO DE COMISIONES A EJECUTIVOS                          " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

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
	DEFINE vtotal_comision          DECIMAL(12,2),
	       AA,num                   ,
               renglon			SMALLINT,
               vcriterio		SMALLINT,
               vcont			INTEGER

        SELECT COUNT(*)
        INTO   vcont
        FROM   com_comis_respon a, com_esq_bono b
        --FROM   com_comis_respon a, com_esq_comis b
   	WHERE  estado_comision = 0
          AND  b.cod_esq_bono = a.cod_esq_comision

        IF vcont = 0 or vcont IS NULL THEN
     	   ERROR "No existen Registros a ser procesados"
           SLEEP 3
           RETURN
        END IF
        DISPLAY "Numero de registros a procesar:", vcont using "###,##&" AT 9,10 
	DECLARE cursor_1 CURSOR FOR 
	    SELECT a.*,
                   b.criterio_cod
            FROM   com_comis_respon    a,
                   com_esq_bono b
                   --esquema_comision b
	    WHERE  a.estado_comision  = 0
            AND    b.cod_esq_bono = a.cod_esq_comision
	    ORDER  BY coduni

	LET num = 0
        LET renglon = 11 
	FOREACH cursor_1 INTO l_reg.*, vcriterio
            LET num = num + 1
	    
            CALL busca_comision(vcriterio,
                                l_reg.cod_esq_comision,
                                l_reg.total_afiliados,
                                l_reg.promedio_sm,l_reg.comision_grupo)
                 RETURNING vtotal_comision
		
{
display "cod resp ",l_reg.cod_resp_uni
display "criterio ",vcriterio
display "esq com  ",l_reg.cod_esq_comision
display "tot afi  ",l_reg.total_afiliados
display "prom sm  ",l_reg.promedio_sm
display "com gpo  ",l_reg.comision_grupo
exit program
}

            IF vtotal_comision IS NOT NULL THEN
                UPDATE com_comis_respon
	        ----SET    total_comision  = 0,
	        SET    total_comision  = vtotal_comision ,
                       estado_comision = 1               ,  #Calculada
                       fecha_calculo   = TODAY           ,
                       usuario         = g_usuario
                WHERE  cod_resp_uni    = l_reg.cod_resp_uni
                AND    coduni          = l_reg.coduni
                AND    nivel           = l_reg.nivel
                AND    estado_comision = 0
            END IF

	DISPLAY "Registros Procesados : ",num USING "###,##&" AT renglon,21

	END FOREACH


{
    UPDATE com_comis_resumen 
    SET    estado_comision = 3,       #Niveles calculos comis a jefes estruc
           fecha_calculo      = TODAY
    WHERE  com_comis_resumen.estado_comision = 2
}

    DISPLAY "Registros Procesados : ",num USING "###,##&" AT renglon,21
END FUNCTION

###############################################################################
FUNCTION busca_comision(pcriterio,pcod_esq_comis,ptotal_afiliados,ppromedio_sm,pcomision_grupo)
#bc----------------------------------------------------------------------------

    DEFINE pcriterio        		SMALLINT,
           pcod_esq_comis               SMALLINT,
           ptotal_afiliados             INTEGER,
           ppromedio_sm                 DECIMAL(6,2),
           pcomision_grupo              DECIMAL(12,2)

    DEFINE ltotal_comision		DECIMAL(12,2),
           lmonto_comision		DECIMAL(12,2),
           lmonto_comision2		DECIMAL(12,2),
           vmonto                       DECIMAL(12,2),
           vmeta_afilia			INTEGER

    LET vmonto = 0

    SELECT meta_afilia
      INTO vmeta_afilia
      FROM com_dat_uni_com
     WHERE nivel = l_reg.nivel
       AND cod_uni = l_reg.coduni
       AND cod_resp_uni = l_reg.cod_resp_uni
        IF STATUS = NOTFOUND THEN
     ERROR "NO EXISTE Unidad comercial para este responsable",l_reg.cod_resp_uni
           CALL pide_enter()
           EXIT PROGRAM
        END IF
    IF vmeta_afilia IS NULL THEN
       LET vmeta_afilia = 0
    END IF
{
display "vmeta_afilia ",vmeta_afilia
display "total_afiliados ",ptotal_afiliados
display "nivel ",l_reg.nivel
display "cod_uni ",l_reg.coduni
display " cod_resp_uni ",l_reg.cod_resp_uni
prompt '' for opc
}

    IF ptotal_afiliados < vmeta_afilia THEN
       LET lmonto_comision = 0
    ELSE
       CASE pcriterio
           WHEN 1
               SELECT monto_comision
               INTO   lmonto_comision2
               FROM   com_cuadro_comis
               WHERE  cod_esq_comision = pcod_esq_comis
               AND    rango_desde     <= ppromedio_sm
               AND    rango_hasta     >= ppromedio_sm
           WHEN 2
               SELECT monto_comision
               INTO   lmonto_comision2
               FROM   com_cuadro_comis
               WHERE  cod_esq_comision = pcod_esq_comis
               AND    rango_desde     <= ppromedio_sm  ------ptotal_afiliados 
	       AND    rango_hasta     >= ppromedio_sm  ------ptotal_afiliados  
           WHEN 3
               SELECT monto_comision
               INTO   lmonto_comision2
               FROM   com_cuadro_comis
               WHERE  cod_esq_comision = pcod_esq_comis
               AND    rango_desde     <= ppromedio_sm
               AND    rango_hasta     >= ppromedio_sm

               ----AND    rango_desde     <= ptotal_afiliados  
               ----AND    rango_hasta     >= ptotal_afiliados  
               ----AND    rango_desde     <= ppromedio_sm
               ----AND    rango_hasta     >= ppromedio_sm
               
   ###            SELECT SUM(total_comision)
   ###            INTO vmonto
   ###            FROM com_comis_resumen
   ###            WHERE  coduni_n1       = l_reg.coduni
   ###            #AND    nivel           = l_reg.nivel
   ###            AND    estado_comision = 1
   
                  SELECT max(monto_sm) 
                    INTO vsal_min
                    FROM tab_salario_minimo          
                 
----               LET lmonto_comision = pcomision_grupo * (lmonto_comision2/100)

---LET lmonto_comision=l_reg.total_sm*vsal_min * (365/12)*(lmonto_comision2/100)

          OTHERWISE
             ERROR "Criterio No reconocible"
      END CASE
   END IF

    LET ltotal_comision = 0

    IF lmonto_comision2 IS NOT NULL THEN
       IF pcriterio = 1 THEN
          LET lmonto_comision = pcomision_grupo * (lmonto_comision2/100)
       END IF
       IF pcriterio = 2 THEN
          LET lmonto_comision = lmonto_comision2
       END IF
    END IF
#    RETURN ltotal_comision

    RETURN lmonto_comision

END FUNCTION

###############################################################################
FUNCTION pide_enter()
#pe------------------
	PROMPT "Presione < ENTER > para Salir...." FOR CHAR aux_pausa
END FUNCTION

################################################################## Fin COMB007 
