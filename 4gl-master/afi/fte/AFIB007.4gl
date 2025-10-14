###########################################################################
#Proyecto          => SAFRE  ( MEXICO )                                   #
#Propietario       => E.F.P.                                              #
#Programa AFIB007  => SUBE AFILIADOS POR TRASPASO LIQUIDADOS AL MAESTRO   #
#Sistema           => AFI                                                 #
#Autor             => MAURO MUNIZ CABALLERO                               #
#Fecha actualiz.   => 18 DE ENERO DE 2001                                 #
###########################################################################

DATABASE safre_af
GLOBALS
    DEFINE HOY	     DATE
    DEFINE diaSig    DATE
    DEFINE fecha_bnx DATE
    DEFINE HORA	     CHAR(8)
    DEFINE generar   CHAR(1)
    DEFINE enter     CHAR(1)
    DEFINE accion    SMALLINT

    DEFINE g_usuario       CHAR(8)
    DEFINE g_glo_parametro   RECORD LIKE glo_parametro.*
    DEFINE G_LISTA	   CHAR(300)

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
	    ACCEPT KEY CONTROL-I
	    DEFER INTERRUPT

    CALL STARTLOG("AFIB007.log")
    CALL inicio() 		#i

    IF accion THEN
	CALL traspasa_datos()     #td
    ELSE
        CALL proceso_principal()  #pp
    END IF

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY    = TODAY
    LET HORA   = TIME
    LET accion = ARG_VAL(1)

    SELECT *, USER
    INTO   g_glo_parametro.*, g_usuario
    FROM   glo_parametro

    LET G_LISTA = g_glo_parametro.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
                ".SUBE_TRASP_MTO" CLIPPED,
                "_",HOY USING "dd-mm-yy","_",HORA CLIPPED
 
END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "AFIB0071" ATTRIBUTE(BORDER)
    DISPLAY " AFIB007        SUBE AFILIADOS POR TRASPASO AL MAESTRO                           " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                          [ Ctrl-C ] Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
    
    INPUT BY NAME generar
          AFTER FIELD generar
                IF generar NOT MATCHES "[SsNn]" THEN
                    ERROR "Opcion solo puede ser S o N"
                ELSE
                    IF generar MATCHES "[Nn]" THEN 
                        ERROR "PROCESO CANCELADO" SLEEP 2
                        EXIT PROGRAM 
                    ELSE
                        ERROR "Procesando Informacion... Espere un momento"

                        CALL Traspasa_datos()
                    END IF
                    EXIT INPUT
                END IF

          ON KEY ( INTERRUPT )
             EXIT PROGRAM

    END INPUT		

END FUNCTION

FUNCTION Traspasa_datos()
#td---------------------

    DEFINE afi		RECORD LIKE afi_solicitud.*
    
    DEFINE mensaje      CHAR(50)
    DEFINE HAY		SMALLINT
    DEFINE i  		SMALLINT
    DEFINE cont  	INTEGER
    DEFINE v_rowid      INTEGER

    LET cont    = 1
    
    DECLARE cursor_1 CURSOR FOR 
        SELECT rowid,* 
        FROM   afi_solicitud A
        WHERE  A.status_interno = 75

    #START REPORT listado_2 TO G_LISTA
    FOREACH cursor_1 INTO v_rowid,afi.*
    
    IF afi.finicta IS NULL THEN
	SELECT max(fecha_mov_banxico)
	INTO   fecha_bnx
	FROM   taa_rcv_recepcion
	WHERE  nss_afo_recep = afi.n_seguro 
	AND    ident_operacion = "09"

	CALL habil_siguiente(fecha_bnx) RETURNING diaSig 

        UPDATE afi_solicitud 
        SET    status_interno = 100,
               status_captura = 100,
               status         = 100,
               finicta        = diaSig
        WHERE  n_seguro       = afi.n_seguro
        AND    n_folio        = afi.n_folio
        AND    tipo_solicitud = afi.tipo_solicitud
        AND    status_interno = 75
    ELSE
        UPDATE afi_solicitud 
        SET    status_interno = 100,
               status_captura = 100,
	       status         = 100
        WHERE  n_seguro       = afi.n_seguro
        AND    n_folio        = afi.n_folio
        AND    tipo_solicitud = afi.tipo_solicitud
        AND    status_interno = 75
    END IF

        LET HAY = FALSE

        SELECT "X"
        FROM   afi_mae_afiliado m
        WHERE  m.n_seguro       = afi.n_seguro
        AND    m.tipo_solicitud = 5

        IF SQLCA.SQLCODE = 0 THEN
            DELETE FROM afi_mae_afiliado
            WHERE  n_seguro       = afi.n_seguro
            AND    tipo_solicitud = 5

				DELETE FROM afi_mae_siefore
            WHERE  n_seguro       = afi.n_seguro
        END IF 

        SELECT COUNT(*) 
        INTO   HAY 
        FROM   afi_mae_afiliado
        WHERE  n_seguro = afi.n_seguro

        IF NOT HAY THEN

            IF afi.n_unico IS NOT NULL AND 
               afi.n_unico <> "                  " AND
               LENGTH(afi.n_unico) = 18 THEN
                LET afi.status_interno = 200
                LET afi.status_captura = 0
            ELSE
                LET afi.status_interno = 100
                LET afi.status_captura = 0
                LET afi.n_unico = NULL
            END IF

	    LET afi.status = NULL

            INSERT INTO afi_mae_afiliado VALUES (afi.*) 

            INSERT INTO afi_mae_patron    #------- Patrones
            SELECT * 
            FROM   afi_patron
            WHERE  n_folio = afi.n_folio
            AND    tipo_solicitud = afi.tipo_solicitud

            INSERT INTO afi_mae_siefore    #------- Regimen de inversion
            SELECT * 
            FROM   afi_siefore
            WHERE  n_seguro = afi.n_seguro
      
            INSERT INTO afi_mae_benefici   #------- Beneficiarios
            SELECT * 
            FROM   afi_beneficiario
            WHERE  n_folio = afi.n_folio
            AND    tipo_solicitud = afi.tipo_solicitud
            AND    n_seguro = afi.n_seguro
 
				IF afi.tipo_solicitud <> 5 THEN
                INSERT INTO cta_ctr_cuenta     #------ Control cuenta
                   VALUES ( afi.n_seguro,	#nss
                            afi.fentcons,	#fentcons
                            "",			#fecha_pri_aporte
                            "",			#fecha_ult_aporte
                            "",			#fecha_ult_anterior
                            1,			#estado_cuenta
                            afi.fentcons,	#fecha_edo_cuenta
                            0,			#estado_proceso
                            "",			#fecha_edo_proceso
                            0,			#accionista_rcv
                            0,			#accionista_vol
                            0,			#aportante_viv
                            0,			#cambio_estado
                            "",			#periodo_ult_aporte
                            0,			#dias_cotizados
                            0,			#tipo_informe
                            "",			#fecha_informe
                            HOY,		#fecha_actualiza
                            g_usuario		#usuario
                          )
				END IF
        END IF

        LET cont = cont + 1
    END FOREACH
        
    DISPLAY "Total de registros incorporados: ",cont
    AT 13,5
    IF cont = 0 THEN
        ERROR "NO SE HICIERON TRASPASOS AL MAESTRO DE AFILIADOS"
        SLEEP 5
    END IF
END FUNCTION

REPORT listado_2(w_aux)
#rl2-------------------
    DEFINE w_aux  RECORD LIKE afi_solicitud.*

    DEFINE aux_sexo	 CHAR(10)
    DEFINE cont          INTEGER

    OUTPUT
	LEFT MARGIN   0
	PAGE LENGTH  60
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT
            COLUMN 20," TRASPASOS AL MAESTRO DE AFILIADOS (TRASPASO)"
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"
        PRINT
            COLUMN 01,"N.S.S.  "      ,
            COLUMN 13,"C.U.R.P.  "    ,
            COLUMN 33,"R.F.C. "       ,
            COLUMN 48,"Paterno"       ,
            COLUMN 68,"Materno"       ,
            COLUMN 88,"Nombres"       
        PRINT
            COLUMN 05,"Fech.Nac."     ,
            COLUMN 17,"Sexo"          ,
            COLUMN 28,"Fech.Frecafor" ,
            COLUMN 43,"Estado Afiliado" 
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
    ON EVERY ROW

	SELECT sexo_desc 
        INTO   aux_sexo 
        FROM   tabsexo
	WHERE  sexo_cod = w_aux.sexo

        PRINT
            COLUMN 01,w_aux.n_seguro                   ,
            COLUMN 13,w_aux.n_unico                    ,
            COLUMN 33,w_aux.n_rfc                      ,
            COLUMN 48,w_aux.paterno CLIPPED            ,
            COLUMN 68,w_aux.materno CLIPPED            ,
            COLUMN 88,w_aux.nombres CLIPPED
        PRINT
            COLUMN 05,w_aux.fena  USING "dd-mm-yyyy"   ,
            COLUMN 17,aux_sexo    ,
            COLUMN 28,w_aux.frecafor USING "dd-mm-yyyy",
            COLUMN 43,"REGISTRADO"
    
    ON LAST ROW
           SELECT COUNT(*)
           INTO   cont
           FROM   afi_2

        PRINT
        PRINT
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"

        PRINT
        PRINT
           COLUMN 03,"NUMERO TOTAL DE REGISTROS ---> ",cont
END REPORT

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

   DEFINE diaTmp	DATE,
   	  contador	SMALLINT,
	  diaActual	DATE
   
   DEFINE diaHabilSig	DATE,
	  diaSemana	SMALLINT,
	  feriado	SMALLINT,
	  finSemana	SMALLINT
	  

   LET diaHabilSig = diaActual

	WHILE TRUE
   	    LET feriado   = 0
   	    LET finSemana = 0
   	    LET diaSemana = WEEKDAY(diaHabilSig)  

   	    IF diaSemana = 0 OR diaSemana = 6 THEN
      	        LET finSemana = 1
   	    END IF
  	     
   	    SELECT *
   	    FROM   safre_af:tab_feriado 
   	    WHERE  feria_fecha = diaHabilSig
    	
   	    IF STATUS <> NOTFOUND THEN
       	        LET feriado = 1
   	    END IF 
		
   	    IF feriado = 1 OR finSemana = 1 THEN
       	        LET diaHabilSig = diaHabilSig + 1 UNITS DAY
   	    ELSE
       	        EXIT WHILE
   	    END IF
	END WHILE

	RETURN diaHabilSig

END FUNCTION

