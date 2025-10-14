############################################################################
#Proyecto          => Sistema de Afores. (MEXICO)                          #
#Propietario       => E.F.P                                                #
#Programa          => CONSULTA dis_det_aporte                              #
#Sistema           => DIS.                                                 #
#Fecha             => 13 de marzo     de 2002.                             #
############################################################################

DATABASE safre_af

GLOBALS


DEFINE  q_resul	ARRAY[100] OF RECORD
	n_seguro		LIKE 	dis_det_aporte.n_seguro,
	n_rfc			LIKE 	dis_det_aporte.n_rfc,
	nom_trabajador		LIKE 	dis_det_aporte.nom_trabajador,
	reg_patronal_imss	LIKE	dis_det_aporte.reg_patronal_imss,
	rfc_patron		LIKE	dis_det_aporte.rfc_patron
        END RECORD


DEFINE	v_folio			LIKE	dis_det_aporte.folio,
        hoy                     DATE

END GLOBALS


MAIN

	OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        COMMENT LINE LAST
        DEFER INTERRUPT

	CALL STARTLOG("DISB038.log")
	--CALL inicio()            #i
	CALL proceso_principal() #pp                

END MAIN



FUNCTION proceso_principal()
#mr----------------------

LET hoy = TODAY

OPEN WINDOW ventana_1 AT 2,2 WITH FORM "DISB0381" ATTRIBUTE(BORDER)
    DISPLAY " DISB038             RECHAZOS ARCHIVO DISPERSION                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY hoy USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "Rechazo" 
        COMMAND "Consulta" "Consulta Registros"
                 CALL consulta()
        COMMAND "Salir" "Salir de Programa"
                 EXIT MENU
    END MENU    

END FUNCTION                      

FUNCTION consulta()

DEFINE	i	SMALLINT,
        cancela SMALLINT


    LET cancela = 1
    LET int_flag = FALSE

    INPUT BY NAME v_folio

    BEFORE FIELD v_folio
           DISPLAY "Ingresa Folio:"  TO FORMONLY.mensaje
   
    AFTER FIELD v_folio
       DISPLAY "[ESC] p/procesar" AT 3,62 ATTRIBUTE(REVERSE)
    ON KEY (INTERRUPT) 
       LET int_flag = FALSE
       LET cancela = 0
       EXIT INPUT
    END INPUT

    DISPLAY "                " AT 3,62 ATTRIBUTE(REVERSE)
    DISPLAY hoy USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    IF cancela = 1 THEN
                                                                            
        DECLARE cursor_1 CURSOR FOR 
        SELECT n_seguro, 
               n_rfc, 
               nom_trabajador,
               reg_patronal_imss,
               rfc_patron
        FROM   dis_det_aporte
        WHERE  folio = v_folio
          AND  result_operacion = "02"
        ORDER BY 1

	LET i = 1

	FOREACH cursor_1 INTO q_resul[i].* 
            LET i = i + 1 

            IF i > 100 THEN
               EXIT FOREACH
            END IF

        END FOREACH

        IF i > 1 THEN
	   CALL SET_COUNT(i-1)


           OPEN WINDOW ventana_2  at 6,2 with form "DISB0382" attribute(border)
           DISPLAY " < Ctrl-C > Salir " AT 1,1

	   DISPLAY ARRAY q_resul TO scr_1.*

            ON KEY ( INTERRUPT )
            LET int_flag = FALSE
   	    EXIT DISPLAY
	   END DISPLAY

           CLOSE WINDOW ventana_2
        ELSE 
            ERROR "No Existen registros ..."
            SLEEP 2
            ERROR ""
            CLEAR FORM
        END IF
        CLEAR FORM
    ELSE
        DISPLAY "" 
        CLEAR FORM
    END IF


END FUNCTION

