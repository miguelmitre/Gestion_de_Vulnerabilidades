#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa          => archivo                                               #
#Fecha actualiz.   => 28 DE JULIO DE 2003                                   #
#Actualizacion     => MAURO MUNIZ CABALLERO                                 #
#Sistema           => AFILIADOS                                             #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE hora   DATE
    DEFINE hoy    DATE
    DEFINE vfecha DATE

    DEFINE enter  CHAR(1)

    DEFINE gr_folios RECORD 
        n_folio         CHAR(10),
        tipo_solicitud  SMALLINT,
        ubicacion       LIKE afi_mae_afiliado.ubicacion,
        indicador_b     LIKE afi_mae_afiliado.indicador_b,
        n_seguro        CHAR(11),
        nombres         CHAR(40),
        paterno         CHAR(40),
        materno         CHAR(40),
        vstatus         CHAR(40)
    END RECORD

    DEFINE reg_control RECORD      
        estado_cuenta SMALLINT,
        marca_cod     SMALLINT,
        fecha_ini     DATE
    END RECORD

    DEFINE
        sw_1     ,
        digito   ,
        contador SMALLINT


    DEFINE
        vindicador_b LIKE afi_mae_afiliado.indicador_b

    DEFINE gmaster ARRAY [9000] OF RECORD
        ubicacion LIKE afi_mae_afiliado.ubicacion ,
        n_folio   LIKE afi_mae_afiliado.n_folio   ,
        n_seguro  LIKE afi_mae_afiliado.n_seguro  ,
        npm       CHAR(80)
    END RECORD

    DEFINE
        g_hora CHAR(8),
        pat    CHAR(30),
        mat    CHAR(30),
        nom    CHAR(30)

    DEFINE
        vfolchar	DECIMAL(10,0)
    

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS INPUT  WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I,
    FORM LINE 3

    CALL STARTLOG('AFIM017.log')
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    LET hora = TIME
    LET hoy = TODAY

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "AFIM0171" ATTRIBUTE(BORDER)

    DISPLAY "AFIM017             MANTENIMIENTO FOLIOS DE AFILIADOS                          " AT 3,1 
    ATTRIBUTE (REVERSE)  

    DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

    CLEAR FORM

    INITIALIZE gr_folios.* TO NULL

    MENU "Cajas"
        COMMAND "Consulta" "Consulta Cajas Afiliados"
            CLEAR FORM
            CALL Consulta_afi_mae_afiliado()
        COMMAND KEY(N) "coNs Caja" "Consulta Cajas Afiliados"
            CLEAR FORM
            CALL Consulta_caja()
        COMMAND "Modifica" "Modifica Cajas Afiliados"
            CLEAR FORM
            CALL Modifica_afi_mae_afiliado()
        COMMAND "Reasigna" "Reasigna Cajas Afiliados"
            CLEAR FORM
            CALL reasigna()
        COMMAND "Listado" "Listado X Caja Afiliados"
            CLEAR FORM
            CALL listado()
        COMMAND "Inserta" "Inserta Posicion y Caja Afiliados"
            CLEAR FORM
            CALL inserta_afi_mae_afiliado()
        COMMAND "Salir" "Salir del Menu Actual"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION Consulta_caja()
#cc---------------------

    DEFINE 
        corre CHAR(100)

    DEFINE 
        i ,
        j SMALLINT

    OPEN WINDOW ventana_2 AT 2,2 WITH FORM "AFIM0172" ATTRIBUTE(BORDER)
    DISPLAY " AFIM0172                CONSULTA POR CAJA                                          " AT 3,1 ATTRIBUTE(REVERSE)

    LET vindicador_b = NULL

    INPUT BY NAME vindicador_b WITHOUT DEFAULTS

        AFTER FIELD vindicador_b
            IF vindicador_b IS NULL THEN
                ERROR "La caja no puede ser valor nulo"
                NEXT FIELD vindicador_b
            END IF

        EXIT INPUT

        ON KEY ( INTERRUPT )
            EXIT INPUT

    END INPUT

    IF vindicador_b IS NULL THEN
        CLOSE WINDOW ventana_2
        RETURN
    END IF
 
    ERROR "Procesando informacion" 

    LET contador = 0
    LET j        = ARR_CURR()

    DECLARE cursor_1 CURSOR FOR
        SELECT ubicacion,
               n_folio,
               n_seguro,
               " ",
               paterno,
               materno,
               nombres
        FROM   afi_mae_afiliado
        WHERE  indicador_b = vindicador_b
        ORDER BY 1

        LET i = 1

        FOREACH cursor_1 INTO gmaster[i].ubicacion,
                              gmaster[i].n_folio,
                              gmaster[i].n_seguro,
                              gmaster[i].npm,
                              pat,
                              mat,
                              nom

             LET contador       = contador + 1
             LET gmaster[j].npm = pat CLIPPED," ",
                                  mat CLIPPED," ",
                                  nom CLIPPED

            LET i = i + 1
            LET j = j + 1
        END FOREACH

    DISPLAY BY NAME contador

    CALL SET_COUNT(i-1)
    ERROR "Proceso Terminado"

    DISPLAY ARRAY gmaster to scr_1.*

    ON KEY ( CONTROL-M )
        LET i = ARR_CURR()
        EXIT DISPLAY 

    ON KEY ( INTERRUPT )
        EXIT DISPLAY

    END DISPLAY

END FUNCTION

FUNCTION listado()
#l----------------

    DEFINE corre CHAR(100)

    LET corre = "fglgo /home/axxi/afi/objetos/AFOL004"
    RUN corre

END FUNCTION

FUNCTION reasigna()
#r-----------------

    DEFINE corre     CHAR(100)

    LET corre = "fglgo /home/axxi/afi/objetos/AFOL003"
    RUN corre

END FUNCTION

FUNCTION Consulta_afi_mae_afiliado()
#m--------------------------

    DEFINE vresp     CHAR(1)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " (Ctrl-c) Salir                    " AT 1,1 ATTRIBUTE(CYAN)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,CYAN)

    INPUT BY NAME gr_folios.n_folio, gr_folios.tipo_solicitud

    BEFORE FIELD n_folio
        CALL pant_cod_barras()
        DISPLAY BY NAME gr_folios.n_folio, gr_folios.tipo_solicitud
        NEXT FIELD tipo_solicitud

    BEFORE FIELD tipo_solicitud
        IF gr_folios.tipo_solicitud < 1 OR 
           gr_folios.tipo_solicitud > 3 THEN
            ERROR "Tipo de solicitud: 1) Registro, 2) Traspaso, 5) asignacion"
            SLEEP 3
            NEXT FIELD tipo_solicitud
        END IF

	SELECT n_folio, 
               tipo_solicitud,
               ubicacion, 
               indicador_b, 
               n_seguro, 
               nombres, 
               paterno, 
               materno,
               status_interno
        INTO   gr_folios.*
        FROM   afi_mae_afiliado
        WHERE  n_folio = gr_folios.n_folio
        AND    tipo_solicitud = gr_folios.tipo_solicitud

        IF SQLCA.SQLCODE <> 0 THEN
	    ERROR "NO SE ENCONTRARON REGISTROS CON LAS CONDICIONES"
	    ATTRIBUTE (REVERSE)
	    SLEEP 2
	    ERROR " "
            CLEAR FORM
	    RETURN 
	END IF

        SELECT marca_cod,
               marca_causa,
               MAX(fecha_ini)
        INTO   reg_control.estado_cuenta
        FROM   cta_act_marca
        WHERE  nss = gr_folios.n_seguro
        AND    marca_cod = 5
        GROUP BY 1,2

        CASE gr_folios.vstatus
	    WHEN   0 LET gr_folios.vstatus = "CAPTURADO "
	    WHEN  10 LET gr_folios.vstatus = "SOLICITUD INCOMPLETA "
	    WHEN  20 LET gr_folios.vstatus = "SOLICITUD COMPLETA "
	    WHEN  30 LET gr_folios.vstatus = "EXTRACCION/ENVIO "
	    WHEN  40 LET gr_folios.vstatus = "RECHAZADO PROCESAR "
	    WHEN  50 LET gr_folios.vstatus = "PENDIENTE PROCESAR "
	    WHEN  60 LET gr_folios.vstatus = "APROBADO PROCESAR "
	    WHEN 100 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 110 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 120 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 130 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 140 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 150 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 160 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 190 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 200 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
        END CASE
          
        IF reg_control.estado_cuenta = 5 THEN
           IF reg_control.marca_cod = 220 THEN
              LET gr_folios.vstatus = "AFIL. TRASPASADO EL DIA ",
                                     reg_control.fecha_ini
                                     USING "dd/mm/yyyy"
           END IF

           IF reg_control.marca_cod > 399 AND
              reg_control.marca_cod < 500 THEN
               LET gr_folios.vstatus = "AFIL. RETIRADO EL DIA ",
                                       reg_control.fecha_ini
                                       USING "dd/mm/yyyy"
           END IF
        END IF

        DISPLAY BY NAME gr_folios.*
 
        PROMPT "                     Presione < ENTER >  Para Continuar                        "
        ATTRIBUTE (REVERSE)
	FOR vresp
        ATTRIBUTE (REVERSE)
	CLEAR FORM
	RETURN
    END INPUT

END FUNCTION

FUNCTION Modifica_afi_mae_afiliado()
#mmaa-------------------------------

    DEFINE vresp1 CHAR(1)
    DEFINE vrowid INTEGER
    DEFINE vdesc  CHAR(30)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " (Ctrl-c) Salir                    " AT 1,1 ATTRIBUTE(CYAN)
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,CYAN)

    INITIALIZE gr_folios.* TO NULL

    INPUT BY NAME gr_folios.n_folio, 
                  gr_folios.tipo_solicitud,
                  gr_folios.ubicacion,
                  gr_folios.indicador_b WITHOUT DEFAULTS

    AFTER FIELD n_folio
        NEXT FIELD tipo_solicitud

    AFTER FIELD tipo_solicitud
        IF gr_folios.tipo_solicitud < 1 OR
           gr_folios.tipo_solicitud > 2 THEN
            ERROR "Tipo de solicitud: 1) Registro, 2) Traspaso"
            SLEEP 3
            NEXT FIELD tipo_solicitud
        END IF                        

        SELECT MIN(rowid) 
        INTO   vrowid 
        FROM   afi_mae_afiliado
        WHERE  n_folio = gr_folios.n_folio
        AND    tipo_solicitud = gr_folios.tipo_solicitud

	SELECT n_folio, 
               tipo_solicitud,
               ubicacion, 
               indicador_b, 
	       n_seguro, 
               nombres, 
               paterno, 
               materno,
	       status_interno
        INTO   gr_folios.*
        FROM   afi_mae_afiliado
        WHERE  rowid = vrowid

        IF SQLCA.SQLCODE <> 0 THEN
	    ERROR "NO SE ENCONTRARON REGISTROS CON LAS CONDICIONES"
	    ATTRIBUTE (REVERSE)
	    SLEEP 2
	    ERROR " "
            CLEAR FORM
	    RETURN
	END IF
       
        CASE gr_folios.vstatus
	    WHEN   0 LET gr_folios.vstatus = "CAPTURADO "
	    WHEN  10 LET gr_folios.vstatus = "SOLICITUD INCOMPLETA "
	    WHEN  20 LET gr_folios.vstatus = "SOLICITUD COMPLETA "
	    WHEN  30 LET gr_folios.vstatus = "EXTRACCION/ENVIO "
	    WHEN  40 LET gr_folios.vstatus = "RECHAZADO PROCESAR "
	    WHEN  50 LET gr_folios.vstatus = "PENDIENTE PROCESAR "
	    WHEN  60 LET gr_folios.vstatus = "APROBADO PROCESAR "
	    WHEN 100 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 110 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 120 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 130 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 140 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 150 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 160 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 190 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	    WHEN 200 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	END CASE

        IF reg_control.estado_cuenta = 5 THEN
           IF reg_control.marca_cod = 220 THEN
              LET gr_folios.vstatus = "AFIL. TRASPASADO EL DIA ",
                                     reg_control.fecha_ini
                                     USING "dd/mm/yyyy"
           END IF

           IF reg_control.marca_cod > 399 AND
              reg_control.marca_cod < 500 THEN
               LET gr_folios.vstatus = "AFIL. RETIRADO EL DIA ",
                                       reg_control.fecha_ini
                                       USING "dd/mm/yyyy"
           END IF
        END IF

	DISPLAY BY NAME gr_folios.* 

        AFTER FIELD ubicacion

        IF reg_control.estado_cuenta = 5 THEN
            PROMPT "Registro NO se puede modificar " 
            ATTRIBUTE (REVERSE)
	    FOR enter
            ATTRIBUTE (REVERSE)
	    CLEAR FORM
            LET vresp1 = "N"
        ELSE
            PROMPT "Desea Modificar Este Registro S/N :                                      "
            ATTRIBUTE (REVERSE)
	    FOR vresp1
            ATTRIBUTE (REVERSE)
	    CLEAR FORM
        END IF

	IF vresp1 MATCHES"[sS]" THEN
            UPDATE afi_mae_afiliado
            SET    indicador_b = gr_folios.indicador_b,
                   ubicacion   = gr_folios.ubicacion
            WHERE  rowid       = vrowid

            ERROR "Modificacion Realizada .."
            ATTRIBUTE (REVERSE)
            SLEEP 2
	    ERROR " "
        END IF

        CLEAR FORM
        INITIALIZE gr_folios.* TO NULL

    END INPUT

    INITIALIZE gr_folios.* TO NULL

END FUNCTION

FUNCTION inserta_afi_mae_afiliado()
#iama------------------------------

    DEFINE vresp1 CHAR(1)
    DEFINE vrowid INTEGER
    DEFINE vdesc  CHAR(30)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " (Ctrl-c) Salir                    " AT 1,1 ATTRIBUTE(CYAN)
    DISPLAY " Inserta  " AT 1,65 ATTRIBUTE(REVERSE,CYAN)

    INITIALIZE gr_folios.* TO NULL

    INPUT BY NAME gr_folios.n_folio, gr_folios.tipo_solicitud WITHOUT DEFAULTS

    BEFORE FIELD n_folio
        CALL pant_cod_barras()
        DISPLAY BY NAME gr_folios.n_folio, gr_folios.tipo_solicitud
        NEXT FIELD tipo_solicitud

    BEFORE FIELD tipo_solicitud
        IF gr_folios.tipo_solicitud < 1 OR
           gr_folios.tipo_solicitud > 2 THEN
            ERROR "Tipo de solicitud: 1) Registro, 2) Traspaso"
            SLEEP 3
            NEXT FIELD tipo_solicitud
        END IF                        

        WHENEVER ERROR CONTINUE
        SELECT MIN(rowid) 
        INTO   vrowid 
        FROM   afi_mae_afiliado
	WHERE  n_folio = gr_folios.n_folio
        AND    tipo_solicitud = gr_folios.tipo_solicitud   
        WHENEVER ERROR STOP
      
	SELECT n_folio, 
               tipo_solicitud,
               ubicacion, 
               indicador_b, 
	       n_seguro, 
               nombres, 
               paterno, 
               materno,
	       status_interno
        INTO   gr_folios.*
        FROM   afi_mae_afiliado
        WHERE  rowid = vrowid

        IF SQLCA.SQLCODE <> 0 THEN
	    ERROR "NO SE ENCONTRO COMO ACEPTADO "
	    ATTRIBUTE (REVERSE)
	    SLEEP 2
	    ERROR " "
            CLEAR FORM
	    RETURN
	ELSE
	    IF gr_folios.ubicacion IS NULL THEN
                LET gr_folios.ubicacion   = 0 
                LET gr_folios.indicador_b = 0 

                SELECT caja,consec 
                INTO   gr_folios.indicador_b,
                       gr_folios.ubicacion
                FROM   afi_caja_folio

                IF gr_folios.ubicacion = 300 THEN
                    LET gr_folios.indicador_b   = gr_folios.indicador_b + 1
                    LET gr_folios.ubicacion = 1
                ELSE
                    LET gr_folios.ubicacion = gr_folios.ubicacion + 1  
                END IF

                CASE gr_folios.vstatus
	            WHEN   0 LET gr_folios.vstatus = "CAPTURADO "
	            WHEN  10 LET gr_folios.vstatus = "SOLICITUD INCOMPLETA "
	            WHEN  20 LET gr_folios.vstatus = "SOLICITUD COMPLETA "
	            WHEN  30 LET gr_folios.vstatus = "EXTRACCION/ENVIO "
	            WHEN  40 LET gr_folios.vstatus = "RECHAZADO PROCESAR "
	            WHEN  50 LET gr_folios.vstatus = "PENDIENTE PROCESAR "
	            WHEN  60 LET gr_folios.vstatus = "APROBADO PROCESAR "
	            WHEN 100 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	            WHEN 110 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	            WHEN 120 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	            WHEN 130 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	            WHEN 140 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	            WHEN 150 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	            WHEN 160 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	            WHEN 190 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	            WHEN 200 LET gr_folios.vstatus = "AFILIADO REGISTRADO "
	        END CASE

	        DISPLAY BY NAME gr_folios.* 
	        LET vresp1 = "S"

                PROMPT "Desea Modificar Este Registro S/N :                                      "
                ATTRIBUTE (REVERSE)
	        FOR vresp1
                ATTRIBUTE (REVERSE)
	        CLEAR FORM

	        IF vresp1 MATCHES"[sS]" THEN
	            UPDATE afi_mae_afiliado
	            SET    indicador_b = gr_folios.indicador_b,
		           ubicacion = gr_folios.ubicacion
                    WHERE  rowid = vrowid

	            UPDATE afi_caja_folio 
	            SET    caja   = gr_folios.indicador_b,
	                   consec = gr_folios.ubicacion

                    ERROR "Realizada .."
                    ATTRIBUTE (REVERSE)
                    SLEEP 1
	            ERROR " "
                END IF

                CLEAR FORM
                INITIALIZE gr_folios.* TO NULL
	    ELSE
	        DISPLAY BY NAME gr_folios.* 
	        ERROR "Afiliado ya asignado"
            END IF
        END IF

    END INPUT

    INITIALIZE gr_folios.* TO NULL

END FUNCTION

FUNCTION pant_cod_barras()

   DEFINE pn_folio	CHAR(11)

   OPEN WINDOW pvent AT 10,10 WITH FORM "AFIM0173"
   ATTRIBUTE(BORDER)

      INPUT BY NAME pn_folio
        BEFORE FIELD pn_folio
           LET pn_folio = NULL
        AFTER FIELD pn_folio
          IF pn_folio IS NULL THEN
             ERROR "ERROR, EL CAMPO ES OBLIGATORIO"
             NEXT FIELD pn_folio
          END IF 

          IF pn_folio[01,01] MATCHES "[Tt]" THEN
             LET gr_folios.n_folio        = pn_folio[02,11]
             LET gr_folios.tipo_solicitud = 2 
          ELSE
             LET gr_folios.n_folio        = pn_folio
             LET gr_folios.tipo_solicitud = 1 
          END IF
          EXIT INPUT

      END INPUT

   CLOSE WINDOW pvent


END FUNCTION
