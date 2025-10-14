################################################################################
#Proyecto          => SISTEMA DE safre_af.( MEXICO )                             #
#Owner             => CARLOS WELSH. 					       #
#Programa TABM029  => MANTENEDOR ARCHIVO DE PARAMETROS
#Fecha             => 27 MARZO DE 1997
#By                => JUAN DAVID HERNANDEZ OYARCE
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE aux_pausa			CHAR(1)
	DEFINE g_reg				RECORD
    	       dias_recepcion     SMALLINT,
    	       dias_envio_certifi SMALLINT,
    	       tamano_max_lote    SMALLINT,
    	       docto_probatorio   CHAR(1),
    	       credencial_elector CHAR(1),
    	       credencial_imss    CHAR(1),
    	       hoja_rosa_imss     CHAR(1),
    	       comprobante_icefa  CHAR(1),
    	       carta_poder_imss   CHAR(1),
    	       carta_poder_renapo CHAR(1),
    	       no_especificado_1  CHAR(1),
    	       no_especificado_2  CHAR(1),
    	       no_especificado_3  CHAR(1),

	       ruta_spool	   CHAR(50),
	       ruta_list_estadist  CHAR(50),
	       ruta_envio_afiliad  CHAR(50),
	       ruta_recepcion_afi  CHAR(50),
	       ruta_envio_promoto  CHAR(50),
	       ruta_recepcion_pro  CHAR(50),
	       ruta_ejecuta_afili  CHAR(50),
	       ruta_ejecuta_pro    CHAR(50),
	       ruta_ejecuta_tabla  CHAR(50),
	       ruta_ejecuta_menu   CHAR(50),
	       ruta_ejecuta_estad  CHAR(50),
	       ruta_ejecuta_admin  CHAR(50),
	       ruta_ejecuta_dispe  CHAR(50)
	END RECORD
	DEFINE g_reg2				RECORD
	       nivel_5			CHAR(60),
	       nivel_4			CHAR(60),
	       nivel_3			CHAR(60),
	       nivel_2			CHAR(60),
	       nivel_1			CHAR(60)
	END RECORD

	DEFINE HOY                      DATE
END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
		INPUT WRAP,
		ACCEPT KEY CONTROL-I

	DEFER INTERRUPT 

        LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 4,2 WITH FORM "TABM0291" ATTRIBUTE(BORDER)
	DISPLAY "TABM029             MANTENIMIENTO PARAMETROS GENERALES                         " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	##################
	# LADO IZQUIERDO #
	##################
	CALL FGL_DRAWBOX(4,31,4,1)
	CALL FGL_DRAWBOX(3,31,7,1)
	CALL FGL_DRAWBOX(12,31,9,1)
	################
	# LADO DERECHO #
	################
	CALL FGL_DRAWBOX(4,45,4,31)
	CALL FGL_DRAWBOX(6,45,7,31)
	CALL FGL_DRAWBOX(9,45,12,31)
	MENU "PARAMETROS GENERALES"
	     COMMAND "Mantenimiento" "Mantenimiento Parametros Generales"
	             CALL Inicializa()
	             CALL Mantenimiento()
	             CALL Inicializa()
	     COMMAND "Salir" "Salir Mantenimiento Parametros Generales"
	             EXIT PROGRAM
	END MENU
END MAIN
###############################################################################
FUNCTION Inicializa()
	 INITIALIZE g_reg.* TO NULL
	 DISPLAY BY NAME g_reg.*
END FUNCTION
###############################################################################
FUNCTION Mantenimiento()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " MANTENIMIENTO " AT 1,60 ATTRIBUTE ( REVERSE )
	DISPLAY " [ Esc ] Grabar       [ Ctrl-C ] Salir    [ Ctrl-p ] Mas" AT 2,1
	SELECT * INTO g_reg.*,g_reg2.* FROM glo_parametro
	INPUT BY NAME g_reg.* WITHOUT DEFAULTS
	      AFTER FIELD dias_recepcion
	             IF g_reg.dias_recepcion IS NULL THEN
			ERROR "Dias Recepcion NO puede ser NULO"
	                NEXT FIELD dias_recepcion
	             END IF
	      AFTER FIELD dias_envio_certifi
	            IF g_reg.dias_envio_certifi IS NULL THEN
		       ERROR "Dias Envio Certificacion NO puede ser NULO"
	               NEXT FIELD dias_envio_certifi
		    END IF
	      AFTER FIELD tamano_max_lote
	            IF g_reg.tamano_max_lote IS NULL THEN
		       ERROR "Tamano Maximo Lote NO puede ser NULO"
	               NEXT FIELD tamano_max_lote
		    END IF
	      AFTER FIELD docto_probatorio
	            IF g_reg.docto_probatorio IS NULL THEN
		       ERROR "Documento Probatorio NO puede ser NULO"
	               NEXT FIELD docto_probatorio
		    END IF
                    IF NOT Verifica_s_n(g_reg.docto_probatorio) THEN
	               NEXT FIELD docto_probatorio
	            END IF
	      AFTER FIELD credencial_elector
	            IF g_reg.credencial_elector IS NULL THEN
		       ERROR "Credencial Elector NO puede ser NULO"
	               NEXT FIELD credencial_elector
		    END IF
                    IF NOT Verifica_s_n(g_reg.credencial_elector) THEN
	               NEXT FIELD credencial_elector
	            END IF
	      AFTER FIELD credencial_imss
	            IF g_reg.credencial_imss IS NULL THEN
		       ERROR "Credencial Imms NO puede ser NULO"
	               NEXT FIELD credencial_imss
		    END IF
                    IF NOT Verifica_s_n(g_reg.credencial_imss) THEN
	               NEXT FIELD credencial_imss
	            END IF
	      AFTER FIELD hoja_rosa_imss
	            IF g_reg.hoja_rosa_imss IS NULL THEN
		       ERROR "Hoja Rosa Imms NO puede ser NULO"
	               NEXT FIELD hoja_rosa_imss
		    END IF
                    IF NOT Verifica_s_n(g_reg.hoja_rosa_imss) THEN
	               NEXT FIELD hoja_rosa_imss
	            END IF
	      AFTER FIELD comprobante_icefa
	            IF g_reg.comprobante_icefa IS NULL THEN
		       ERROR "Comprobante Icefa NO puede ser NULO"
	               NEXT FIELD comprobante_icefa
		    END IF
                    IF NOT Verifica_s_n(g_reg.comprobante_icefa) THEN
	               NEXT FIELD comprobante_icefa
	            END IF
	      AFTER FIELD carta_poder_imss
	            IF g_reg.carta_poder_imss IS NULL THEN
		       ERROR "Carta Poder Imms NO puede ser NULO"
	               NEXT FIELD carta_poder_imss
		    END IF
                    IF NOT Verifica_s_n(g_reg.carta_poder_imss) THEN
	               NEXT FIELD carta_poder_imss
	            END IF
	      AFTER FIELD carta_poder_renapo
	            IF g_reg.carta_poder_renapo IS NULL THEN
		       ERROR "Carta Poder Renapo NO puede ser NULO"
	               NEXT FIELD carta_poder_renapo
		    END IF
                    IF NOT Verifica_s_n(g_reg.carta_poder_renapo) THEN
	               NEXT FIELD carta_poder_renapo
	            END IF
	      AFTER FIELD no_especificado_1
	            IF g_reg.no_especificado_1 IS NULL THEN
		       ERROR "Documento No Especificado Uno NO puede ser NULO"
	               NEXT FIELD no_especificado_1
		    END IF
                    IF NOT Verifica_s_n(g_reg.no_especificado_1) THEN
	               NEXT FIELD no_especificado_1
	            END IF
	      AFTER FIELD no_especificado_2
	            IF g_reg.no_especificado_2 IS NULL THEN
		       ERROR "Documento No Especificado Dos NO puede ser NULO"
	               NEXT FIELD no_especificado_2
		    END IF
                    IF NOT Verifica_s_n(g_reg.no_especificado_2) THEN
	               NEXT FIELD no_especificado_2
	            END IF
	      AFTER FIELD no_especificado_3
	            IF g_reg.no_especificado_3 IS NULL THEN
		       ERROR "Documento No Especificado Tres NO puede ser NULO"
	               NEXT FIELD no_especificado_3
		    END IF
                    IF NOT Verifica_s_n(g_reg.no_especificado_3) THEN
	               NEXT FIELD no_especificado_3
	            END IF
	      ON KEY ( INTERRUPT )
		 CALL Inicializa()
		 EXIT INPUT
	      ON KEY ( ESC )
	         IF g_reg.dias_recepcion IS NULL THEN
		    ERROR "Dias Recepcion NO puede ser NULO"
	            NEXT FIELD dias_recepcion
	         END IF
	         IF g_reg.dias_envio_certifi IS NULL THEN
		    ERROR "Dias Envio Certificacion NO puede ser NULO"
	            NEXT FIELD dias_envio_certifi
		 END IF
	         IF g_reg.tamano_max_lote IS NULL THEN
		    ERROR "Tamano Maximo Lote NO puede ser NULO"
	            NEXT FIELD tamano_max_lote
		 END IF
	         IF g_reg.docto_probatorio IS NULL THEN
		    ERROR "Documento Probatorio NO puede ser NULO"
	            NEXT FIELD docto_probatorio
		 END IF
                 IF NOT Verifica_s_n(g_reg.docto_probatorio) THEN
	            NEXT FIELD docto_probatorio
	         END IF
	         IF g_reg.credencial_elector IS NULL THEN
		    ERROR "Credencial Elector NO puede ser NULO"
	            NEXT FIELD credencial_elector
		 END IF
                 IF NOT Verifica_s_n(g_reg.credencial_elector) THEN
	            NEXT FIELD credencial_elector
	         END IF
	         IF g_reg.credencial_imss IS NULL THEN
		    ERROR "Credencial Imms NO puede ser NULO"
	            NEXT FIELD credencial_imss
		 END IF
                 IF NOT Verifica_s_n(g_reg.credencial_imss) THEN
	            NEXT FIELD credencial_imss
	         END IF
	         IF g_reg.hoja_rosa_imss IS NULL THEN
		    ERROR "Hoja Rosa Imms NO puede ser NULO"
	            NEXT FIELD hoja_rosa_imss
		 END IF
                 IF NOT Verifica_s_n(g_reg.hoja_rosa_imss) THEN
	            NEXT FIELD hoja_rosa_imss
	         END IF
	         IF g_reg.comprobante_icefa IS NULL THEN
		    ERROR "Comprobante Icefa NO puede ser NULO"
	            NEXT FIELD comprobante_icefa
		 END IF
                 IF NOT Verifica_s_n(g_reg.comprobante_icefa) THEN
	            NEXT FIELD comprobante_icefa
	         END IF
	         IF g_reg.carta_poder_imss IS NULL THEN
		    ERROR "Carta Poder Imms NO puede ser NULO"
	            NEXT FIELD carta_poder_imss
		 END IF
                 IF NOT Verifica_s_n(g_reg.carta_poder_imss) THEN
	            NEXT FIELD carta_poder_imss
	         END IF
	         IF g_reg.carta_poder_renapo IS NULL THEN
		    ERROR "Carta Poder Renapo NO puede ser NULO"
	            NEXT FIELD carta_poder_renapo
		 END IF
                 IF NOT Verifica_s_n(g_reg.carta_poder_renapo) THEN
	            NEXT FIELD carta_poder_renapo
	         END IF
	         IF g_reg.no_especificado_1 IS NULL THEN
		    ERROR "Documento No Especificado Uno NO puede ser NULO"
	            NEXT FIELD no_especificado_1
		 END IF
                 IF NOT Verifica_s_n(g_reg.no_especificado_1) THEN
	            NEXT FIELD no_especificado_1
	         END IF
	         IF g_reg.no_especificado_2 IS NULL THEN
		    ERROR "Documento No Especificado Dos NO puede ser NULO"
	            NEXT FIELD no_especificado_2
		 END IF
                 IF NOT Verifica_s_n(g_reg.no_especificado_2) THEN
	            NEXT FIELD no_especificado_2
	         END IF
	         IF g_reg.no_especificado_3 IS NULL THEN
		    ERROR "Documento No Especificado Tres NO puede ser NULO"
	            NEXT FIELD no_especificado_3
		 END IF
                 IF NOT Verifica_s_n(g_reg.no_especificado_3) THEN
	            NEXT FIELD no_especificado_3
	         END IF
		 DELETE FROM glo_parametro 
		 INSERT INTO glo_parametro VALUES ( g_reg.*,"","","","","")
		 UPDATE glo_parametro SET
		        com_nivel5 = g_reg2.nivel_5,
		        com_nivel4 = g_reg2.nivel_4,
		        com_nivel3 = g_reg2.nivel_3,
		        com_nivel2 = g_reg2.nivel_2,
		        com_nivel1 = g_reg2.nivel_1
		 ERROR "REGISTRO ACTUALIZADO" SLEEP 1
		 ERROR ""
		 EXIT INPUT
	      ON KEY ( CONTROL-P )
		 CALL Ingresa_segunda_pantalla()
	      ON KEY ( INTERRUPT )
		 CALL Inicializa()
		 EXIT INPUT
	END INPUT
END FUNCTION
###############################################################################
FUNCTION Verifica_s_n(aux_var)
	DEFINE aux_var 				CHAR(1)
	CASE aux_var
	     WHEN "S"  RETURN TRUE
	     WHEN "N"  RETURN TRUE
	     OTHERWISE ERROR "SOLO PUEDE SER S/N" RETURN FALSE
	END CASE
END FUNCTION
###############################################################################
FUNCTION Ingresa_segunda_pantalla()
	OPEN WINDOW ventana_2 AT 4,2 WITH FORM "TABM0292" ATTRIBUTE(BORDER)
	DISPLAY "                    MANTENIMIENTO PARAMETROS GENERALES                         " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " MANTENIMIENTO " AT 1,60 ATTRIBUTE ( REVERSE )
	DISPLAY " [ Esc ] Grabar       [ Ctrl-C ] Salir" AT 2,1
	SELECT com_nivel5,com_nivel4,com_nivel3,com_nivel2,nivel_1
	INTO g_reg2.* FROM glo_parametro
	INPUT BY NAME g_reg2.* WITHOUT DEFAULTS
	      AFTER FIELD com_nivel5
	            IF g_reg2.nivel_5 IS NULL THEN
		       ERROR "NIVEL NO PUEDE SER NULO"
		       NEXT FIELD com_nivel5
	            END IF
	      AFTER FIELD com_nivel4
	            IF g_reg2.nivel_4 IS NULL THEN
		       ERROR "NIVEL NO PUEDE SER NULO"
		       NEXT FIELD com_nivel4
	            END IF
	      AFTER FIELD com_nivel3
	            IF g_reg2.nivel_3 IS NULL THEN
		       ERROR "NIVEL NO PUEDE SER NULO"
		       NEXT FIELD com_nivel3
	            END IF
	      AFTER FIELD com_nivel2
	            IF g_reg2.nivel_2 IS NULL THEN
		       ERROR "NIVEL NO PUEDE SER NULO"
		       NEXT FIELD com_nivel2
	            END IF
	      AFTER FIELD com_nivel1
	            IF g_reg2.nivel_1 IS NULL THEN
		       ERROR "NIVEL NO PUEDE SER NULO"
		       NEXT FIELD com_nivel1
	            END IF
	      ON KEY ( ESC )
	            IF g_reg2.nivel_5 IS NULL THEN
		       ERROR "NIVEL NO PUEDE SER NULO"
		       NEXT FIELD com_nivel5
	            END IF
	            IF g_reg2.nivel_4 IS NULL THEN
		       ERROR "NIVEL NO PUEDE SER NULO"
		       NEXT FIELD com_nivel4
	            END IF
	            IF g_reg2.nivel_3 IS NULL THEN
		       ERROR "NIVEL NO PUEDE SER NULO"
		       NEXT FIELD com_nivel3
	            END IF
	            IF g_reg2.nivel_2 IS NULL THEN
		       ERROR "NIVEL NO PUEDE SER NULO"
		       NEXT FIELD com_nivel2
	            END IF
	            IF g_reg2.nivel_1 IS NULL THEN
		       ERROR "NIVEL NO PUEDE SER NULO"
		       NEXT FIELD com_nivel1
	            END IF
	            EXIT INPUT
	END INPUT
	CLOSE WINDOW ventana_2
END FUNCTION
