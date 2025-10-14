##############################################################################
#Owner             => E.F.P.
#Programa SEPREP   => PANTALLA PARA GENERAR OPERACION(27)
#Fecha creacion    => 11 DE ABRIL DEL 2005 
#By                => MARCO ANTONIO GONZALEZ ROJAS
#Sistema           => SEPARACION DE CUENTAS 
##############################################################################
DATABASE safre_af
	
GLOBALS

        DEFINE
                g_reg_fol      RECORD
                folio          INTEGER
        END RECORD
 
        DEFINE  g_num_regs     INTEGER

        DEFINE  g_enter        CHAR(01)  ,
                g_comando      CHAR(100) ,
                g_hora         CHAR(05)

END GLOBALS

GLOBALS "SEPRPTS.4gl"

MAIN
#m ---------------------------
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    LET g_hora                =                    TIME

    CALL init()

OPEN WINDOW w_popup   AT 2,2
WITH 19 ROWS, 75 COLUMNS
ATTRIBUTE(BORDER, FORM LINE 4)
OPEN FORM f_popup  from "SEPL001"
DISPLAY FORM f_popup    

    DISPLAY "                               <Ctrl-c> Salir                                  " AT 1,1 ATTRIBUTE(REVERSE) 
    DISPLAY " SEPL001                  REPORTE OPERACION (27)                               " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME g_reg_fol.* WITHOUT DEFAULTS

       BEFORE FIELD folio

          DISPLAY BY NAME g_reg_fol.folio
     
       AFTER FIELD folio
          IF (g_reg_fol.folio IS NULL) THEN
             ERROR"FOLIO NO PUEDE SER NULO,INTENTE NUEVAMENTE"
             NEXT FIELD folio
          END IF
        ON KEY (INTERRUPT)
            EXIT PROGRAM

        
        ON KEY (ESC)
            IF (g_reg_fol.folio IS NULL) THEN
                ERROR"CAMPO NO PUEDE SER NULO"
                NEXT FIELD folio
            END IF
            DISPLAY "PROCESANDO INFORMACION" AT 19,1 ATTRIBUTE(REVERSE)
            CALL  consulta()
            IF g_num_regs > 1 THEN
                CALL verif_genrepsiono()
            END IF

    EXIT INPUT
    END INPUT
CLOSE WINDOW w_popup 
END MAIN
################################################################################
FUNCTION consulta()
#c---------------
 
    SELECT COUNT(*)
    INTO g_num_regs
    FROM sep_det_solicitud a
    WHERE a.folio  =  g_reg_fol.folio

    IF ( g_num_regs = 0 OR g_num_regs IS NULL ) THEN
       PROMPT "NO EXISTE INFORMACION PARA ESE FOLIO <enter para continuar>" ATTRIBUTE (REVERSE) FOR CHAR g_enter
      RETURN

    END IF


END FUNCTION
################################################################################
FUNCTION verif_genrepsiono()
   
    LET g_folio                =          g_reg_fol.folio

    WHILE TRUE
      PROMPT "DESEA GENERAR REPORTE S/N? " ATTRIBUTE (REVERSE) FOR CHAR g_enter
        IF g_enter MATCHES "[sSnN]" THEN
            IF g_enter MATCHES "[sS]" THEN
                DISPLAY"PROCESANDO REPORTE..." AT 19,1 ATTRIBUTE(REVERSE) SLEEP
2
                DISPLAY " "
		CALL asigna_globales()
                CALL genera_reporte()
 
                DISPLAY"PROCESO CONCLUIDO RUTA DEL REPORTE GENERADO ==>"," ",g_seg_modulo.ruta_listados AT 19,1 ATTRIBUTE(REVERSE) SLEEP 5

                PROMPT " <Enter para continuar>" ATTRIBUTE (REVERSE) FOR CHAR g_enter

                EXIT WHILE
            ELSE
                DISPLAY"PROCESO CANCELADO..." AT 19,1 ATTRIBUTE(REVERSE) SLEEP 3

                EXIT WHILE
            END IF
        END IF
    END WHILE
 
END FUNCTION
################################################################################
FUNCTION init()
#i ---------------

#### VARIABLES PARA LIB_SEPQRY "y" SEPREPGRAL  #############

    SELECT  USER
    INTO    g_usuario
    FROM    tab_afore_local

    SELECT  *
    INTO    g_seg_modulo.*
    FROM    seg_modulo
    WHERE   modulo_cod = 'sep'

    LET hoy = TODAY

    SELECT MAX(folio)
    INTO g_reg_fol.folio
    FROM sep_det_solicitud


END FUNCTION
################################################################################
FUNCTION asigna_globales()
   
#== ASIGNACION DE VARIABLES QUE SE UTILIZA EL PROGRAMA SEPREPGRAL.4gl===

   LET  g_tabname                      =                "sep_det_solicitud a"

   LET  g_tabname_1                    =                "sep_det03_solicitud b"

   LET  g_nombre_prog                  =                "SEPL001"

   LET  g_tipo_desc1                   =                "REPORTE OPERACION (27)"

   SELECT COUNT(*)
   INTO g_total_cuentas      
   FROM safre_af:sep_det_solicitud a
   WHERE a.folio     =  g_folio 

   IF ( g_total_cuentas IS NULL ) THEN
      LET g_total_cuentas = 0
   END IF

END FUNCTION

