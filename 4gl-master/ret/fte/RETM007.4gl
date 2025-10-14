#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa RETM0007 => CATALOGO DE CODIGOS DE RECHAZOS INTERNOS DE RETIROS   #
#Fecha             => 22 Julio 2002       				    #
#Por               => LUIS ENRIQUE AVILA GUZMAN.        		    #
#Fecha Modificacion=> 22 Julio 2002.                                        #
#Modificado        => LUIS ENRIQUE AVILA GUZMAN.                            #
#Sistema           => RET. (tabla ret_txt_rch_interno)
#############################################################################
DATABASE safre_af
GLOBALS
    DEFINE g_param_dis     RECORD LIKE dis_parametro.*

    DEFINE reg             RECORD 
	   cod_rechazo     LIKE ret_txt_rch_interno.cod_rechazo   ,
	   desc_corta      LIKE ret_txt_rch_interno.desc_corta    ,
	   desc_larga1     LIKE ret_txt_rch_interno.desc_larga1   ,
	   desc_larga2     LIKE ret_txt_rch_interno.desc_larga2   ,
	   desc_larga3     LIKE ret_txt_rch_interno.desc_larga3   ,
	   desc_larga4     LIKE ret_txt_rch_interno.desc_larga4   ,
	   desc_larga5     LIKE ret_txt_rch_interno.desc_larga5   ,
	   desc_larga6     LIKE ret_txt_rch_interno.desc_larga6   ,
	   desc_larga7     LIKE ret_txt_rch_interno.desc_larga7   ,
	   desc_larga8     LIKE ret_txt_rch_interno.desc_larga8   ,
	   desc_larga9     LIKE ret_txt_rch_interno.desc_larga9   ,
	   desc_larga10    LIKE ret_txt_rch_interno.desc_larga10
    END RECORD

    DEFINE l_record        ARRAY[5] OF RECORD
	   cod_rechazo     LIKE ret_txt_rch_interno.cod_rechazo   ,
	   desc_corta      LIKE ret_txt_rch_interno.desc_corta    ,
	   desc_larga1     LIKE ret_txt_rch_interno.desc_larga1   ,
	   desc_larga2     LIKE ret_txt_rch_interno.desc_larga2   ,
	   desc_larga3     LIKE ret_txt_rch_interno.desc_larga3   ,
	   desc_larga4     LIKE ret_txt_rch_interno.desc_larga4   ,
	   desc_larga5     LIKE ret_txt_rch_interno.desc_larga5   ,
	   desc_larga6     LIKE ret_txt_rch_interno.desc_larga6   ,
	   desc_larga7     LIKE ret_txt_rch_interno.desc_larga7   ,
	   desc_larga8     LIKE ret_txt_rch_interno.desc_larga8   ,
	   desc_larga9     LIKE ret_txt_rch_interno.desc_larga9   ,
	   desc_larga10    LIKE ret_txt_rch_interno.desc_larga10
    END RECORD

    DEFINE aux_estad_desc  CHAR(40),
           aux_pausa       CHAR(01),
           x_x             CHAR(150),
           x_buscar        CHAR(30),
           hoy             DATE,
           seg_usuario     CHAR(08),
           siono           CHAR(01),
           pos             INTEGER,
           cla_where       CHAR(300),
           sel_where       CHAR(300),
           g_lista         CHAR(300),
           g_impre         CHAR(300),
           hora            CHAR(08)

END GLOBALS

#####################################################################
MAIN
    OPTIONS PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I,
            INPUT WRAP 
    DEFER INTERRUPT
    CALL inicio()
    CALL proceso()
END MAIN

#####################################################################
FUNCTION inicio()
    SELECT USER,*
    INTO   seg_usuario
    FROM glo_parametro

    SELECT ruta_spool
    INTO g_param_dis.ruta_spool
    FROM dis_parametro
END FUNCTION

#####################################################################
FUNCTION proceso()
    LET HOY = TODAY
    OPEN WINDOW ventana AT 2,1 WITH FORM "RETM0071"
#   OPEN WINDOW ventana AT 1,1 WITH FORM "RETM0071" ATTRIBUTE( BORDER)
    DISPLAY " RETM007           CATALOGO DE CODIGOS DE RECHAZO INTERNO                       " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

    MENU "RECHAZOS INTERNOS"
         COMMAND "Agrega" "Agrega Códigos de Rechazo"
             CALL Agrega()
         COMMAND "Consulta" "Consulta Códigos de Rechazo"
             CALL Consulta()
         COMMAND "Modifica" "Modifica Códigos de Rechazo"
             CALL Modifica()
         COMMAND "Elimina" "Elimina Códigos de Rechazo"
             CALL Elimina()
         COMMAND "Salir" "Salir del Programa"
             EXIT MENU
    END MENU

    CLOSE WINDOW ventana
END FUNCTION

#####################################################################
FUNCTION Inicializa()
    INITIALIZE reg.* TO NULL
    LET reg.desc_corta   = NULL
    LET reg.desc_larga1  = NULL
    LET reg.desc_larga2  = NULL
    LET reg.desc_larga3  = NULL
    LET reg.desc_larga4  = NULL
    LET reg.desc_larga5  = NULL
    LET reg.desc_larga6  = NULL
    LET reg.desc_larga7  = NULL
    LET reg.desc_larga8  = NULL
    LET reg.desc_larga9  = NULL
    LET reg.desc_larga10 = NULL
    DISPLAY BY NAME reg.*
END FUNCTION

#####################################################################
FUNCTION Agrega()
        CALL Inicializa()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                               " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

	INPUT BY NAME  reg.*
             AFTER  FIELD cod_rechazo
		 IF reg.cod_rechazo =  0     OR
		    reg.cod_rechazo IS NULL THEN
                    ERROR "El código debe ser mayor a 0"
                    NEXT FIELD cod_rechazo
		 END IF

             AFTER  FIELD desc_corta
		 IF reg.desc_corta IS NULL THEN
                    ERROR "La descripción corta no puede ser nula"
                    NEXT FIELD desc_corta
		 END IF

             AFTER  FIELD desc_larga1
		 IF reg.desc_larga1 IS NULL THEN
                    ERROR "La descripción larga no puede ser nula"
                    NEXT FIELD desc_larga1
		 END IF
 
             ON KEY (ESC)
		 SELECT "X" 
                 FROM   ret_txt_rch_interno
		 WHERE  cod_rechazo = reg.cod_rechazo

		 IF STATUS <> NOTFOUND THEN
		    ERROR "Este Código ya EXISTE"
		    SLEEP 2
		    NEXT FIELD cod_rechazo
		 ELSE  
                    INSERT INTO ret_txt_rch_interno 
		        VALUES ( reg.*,
				 seg_usuario,
				 TODAY
			       ) 
		    ERROR "REGISTRO INGRESADO" 
		    SLEEP 2
		    ERROR ""
                    CALL Inicializa()
		    NEXT FIELD cod_rechazo
                 END IF
             ON KEY (INTERRUPT)
                 CALL Inicializa()
                 EXIT INPUT
        END INPUT
        CLEAR SCREEN
END FUNCTION

############################################################
FUNCTION Consulta()
       CALL Inicializa()
       CALL SET_COUNT(pos-1)
       DISPLAY " (Enter) Consulta              (Ctrl-C) Salir                                 " AT 1,1 ATTRIBUTE(REVERSE)

       LET int_flag = FALSE
       CONSTRUCT cla_where 
		 ON   cod_rechazo
		 FROM cod_rechazo
          ON KEY (control-m)
             LET int_flag = FALSE
             EXIT CONSTRUCT     
          ON KEY (control-c)
             LET int_flag = TRUE
             EXIT CONSTRUCT
       END CONSTRUCT
       IF int_flag = TRUE THEN
          LET int_flag = FALSE
          ERROR "BUSQUEDA CANCELADA..."
          SLEEP 2
          ERROR ""
          CLEAR SCREEN
          RETURN
       ELSE  
          LET sel_where = "SELECT * FROM ret_txt_rch_interno WHERE ",
	   		   cla_where CLIPPED
          PREPARE query1 FROM sel_where
          DECLARE cursor_1 CURSOR FOR query1     
          LET pos = 1
          FOREACH cursor_1 INTO l_record[pos].*
             LET pos = pos + 1
          END FOREACH
	  LET pos = pos - 1
          IF pos > 0 THEN
	     CALL  SET_COUNT(pos-1)
             LET reg.* = l_record[pos].*
             DISPLAY BY NAME reg.*
          END IF
       END IF
    CLEAR SCREEN
END FUNCTION

#####################################################################
FUNCTION  Modifica()
      CALL Inicializa()
      CALL SET_COUNT(pos-1)
      DISPLAY " (Enter) Modifica                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

       CONSTRUCT cla_where 
		 ON   cod_rechazo
		 FROM cod_rechazo
          ON KEY (control-m)
             LET int_flag = FALSE
             EXIT CONSTRUCT     
          ON KEY (control-c)
             LET int_flag = TRUE
             EXIT CONSTRUCT
       END CONSTRUCT

       IF int_flag = TRUE THEN
          LET int_flag = FALSE
          ERROR "BUSQUEDA CANCELADA..."
          SLEEP 2
          ERROR ""
          CLEAR SCREEN
          RETURN
       END IF

       LET sel_where = "SELECT * FROM ret_txt_rch_interno WHERE ",
			cla_where CLIPPED

       PREPARE query2 FROM sel_where
    
       DECLARE cursor_2 CURSOR FOR query2     

       LET pos = 1

       FOREACH cursor_2 INTO l_record[pos].*
          LET pos = pos + 1
       END FOREACH
       LET pos = pos - 1
       IF pos > 0 THEN
	  CALL  SET_COUNT(pos-1)
	  LET reg.* = l_record[pos].*
	  DISPLAY BY NAME reg.*
       ELSE
          ERROR "REGISTRO ... NO EXISTE"
          SLEEP 2
          ERROR ""
          RETURN
       END IF 

       DISPLAY "" AT 1,1
       DISPLAY "" AT 2,1
       DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
       DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

       INPUT BY NAME  reg.* WITHOUT DEFAULTS
             AFTER  FIELD cod_rechazo
		 IF reg.cod_rechazo =  0     OR
		    reg.cod_rechazo IS NULL THEN
                    ERROR "El código debe ser mayor a 0"
                    NEXT FIELD cod_rechazo
		 END IF

             AFTER  FIELD desc_corta
		 IF reg.desc_corta IS NULL THEN
                    ERROR "La descripción corta no puede ser nula"
                    NEXT FIELD desc_corta
		 END IF

             AFTER  FIELD desc_larga1
		 IF reg.desc_larga1 IS NULL THEN
                    ERROR "La descripción larga no puede ser nula"
                    NEXT FIELD desc_larga1
		 END IF

             ON KEY (ESC)
                 CALL pregunta()
                 IF aux_pausa MATCHES "[Ss]" THEN
                    UPDATE ret_txt_rch_interno
		       SET cod_rechazo  = reg.cod_rechazo  ,
		           desc_corta   = reg.desc_corta   ,
		           desc_larga1  = reg.desc_larga1  ,
		           desc_larga2  = reg.desc_larga2  ,
		           desc_larga3  = reg.desc_larga3  ,
		           desc_larga4  = reg.desc_larga4  ,
		           desc_larga5  = reg.desc_larga5  ,
		           desc_larga6  = reg.desc_larga6  ,
		           desc_larga7  = reg.desc_larga7  ,
		           desc_larga8  = reg.desc_larga8  ,
		           desc_larga9  = reg.desc_larga9  ,
		           desc_larga10 = reg.desc_larga10 ,
		           usuario      = seg_usuario      ,
		           factualiza   = TODAY
              WHERE cod_rechazo = reg.cod_rechazo

	      ERROR "REGISTRO MODIFICADO" SLEEP 1
	      ERROR ""
              CALL Inicializa()
           ELSE
              ERROR "PROCESO DE MODIFICACION,CANCELADO..."
              SLEEP 2
           END IF

           ERROR ""
           EXIT INPUT
        ON KEY (INTERRUPT)
           CALL inicializa()
           EXIT INPUT
        END INPUT
    CLEAR SCREEN
END FUNCTION

######################################################################
FUNCTION Elimina()
    CALL Inicializa()
    CALL SET_COUNT(pos-1)
    DISPLAY " (Enter) Elimina                                             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    CONSTRUCT cla_where 
        ON   cod_rechazo
        FROM cod_rechazo
    ON KEY (control-m)
        LET int_flag = FALSE
        EXIT CONSTRUCT     
    ON KEY (control-c)
        LET int_flag = TRUE
        EXIT CONSTRUCT
    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = "SELECT * FROM ret_txt_rch_interno WHERE ",
 	             cla_where CLIPPED

    PREPARE query3 FROM sel_where
    
    DECLARE cursor_3 CURSOR FOR query3     

    LET pos = 1

    FOREACH cursor_3 INTO l_record[pos].*
        LET pos = pos + 1
    END FOREACH
    LET pos = pos - 1
    IF pos > 0 THEN
        CALL  SET_COUNT(pos-1)
        LET reg.* = l_record[pos].*
        DISPLAY BY NAME reg.* 
    ELSE
        ERROR "REGISTRO .... NO EXISTE"
        SLEEP 2
        ERROR ""
        RETURN
    END IF

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

    DISPLAY BY NAME  reg.*
    CALL Pregunta()

    IF aux_pausa MATCHES "[Ss]" THEN
        DELETE 
        FROM   ret_txt_rch_interno
        WHERE  cod_rechazo = reg.cod_rechazo

        ERROR "REGISTRO ELIMINADO" 
        SLEEP 2
    ELSE
        ERROR "ELIMINAR CANCELADO" 
        SLEEP 2
    END IF

    ERROR ""
    CALL Inicializa()
    CLEAR SCREEN
END FUNCTION

################################################################################
FUNCTION Pregunta()
    PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION

