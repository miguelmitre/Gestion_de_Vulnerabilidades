##########################################################################
#Propietario       => E.F.P                                              #
#Programa          => Consulta de Codigos Postales                       #
#Sistema           => AFILIADOS                                          #
#Fecha             => 26 de Noviembre 1997                               #
#Modificado        => 26 Noviembre 1997                                  #
#Por               => Jose Manuel Vizcaino                               #
#Modificado        => 23 DE JULIO DE 2003  adecuacion para version 2.0   #
#Por               => MAURO MUÑIZ CABALLERO                              #
##########################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_afiliados RECORD
        codpos        LIKE afi_domicilio.codpos,
        colonia       LIKE afi_domicilio.colonia,
        delega        LIKE afi_domicilio.delega,
        desc_delega_1 CHAR(50),
        ciudad        INTEGER,
        desc_ciudad_1 CHAR(50),
        estado        INTEGER,
        desc_estado_1 CHAR(50)
    END RECORD

END GLOBALS

MAIN

    OPTIONS
        MESSAGE LINE LAST -1,
        PROMPT LINE LAST

     CALL principal()

END MAIN

FUNCTION principal()
#p------------------
                 
        DEFINE HOY  DATE
        LET HOY = today

	OPEN WINDOW w_codigos AT 2,2 WITH FORM "AFIM0271"
	ATTRIBUTE (BORDER)

        DISPLAY " AFIM027               CONSULTA CODIGOS POSTALES                                    " AT 3,1 ATTRIBUTE(REVERSE)

	DISPLAY "                                < Ctrl-C > Salir                               " AT 1,2 ATTRIBUTE(REVERSE)
 	DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

             MENU "Codigos_Postales"
		  COMMAND "Consulta" "Consulta Codigos Postales"
			   CALL inicializa()
			   CALL Consulta_direcciones()
                  COMMAND "Salir" "Salir del Menu Actual"
			   EXIT MENU
			   CLOSE WINDOW w_codigos
             END MENU
END FUNCTION

FUNCTION inicializa()
#i-------------------

    CLEAR FORM

END FUNCTION

FUNCTION Consulta_direcciones()
#cd----------------------------

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " Consulta " AT 1,67 ATTRIBUTE(REVERSE)
	DISPLAY " Ctrl-C  Salir           " AT 1,1 ATTRIBUTE(BOLD)

	INPUT BY NAME g_afiliados.codpos

       BEFORE FIELD codpos
		  INITIALIZE g_afiliados.* TO NULL
		  CLEAR FORM
       
       AFTER FIELD codpos
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD codpos
               END IF
               IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                  NEXT FIELD codpos
               END IF
	       IF g_afiliados.codpos IS NULL THEN
                  CALL Despliega_codigo_postal()
                  RETURNING g_afiliados.codpos, 
                            g_afiliados.colonia,
                            g_afiliados.delega,
                            g_afiliados.desc_delega_1,
                            g_afiliados.ciudad,
                            g_afiliados.desc_ciudad_1,
                            g_afiliados.estado,
                            g_afiliados.desc_estado_1

                   IF g_afiliados.colonia IS NULL THEN
                      ERROR "Este Codigo Postal no existe en el catalogo"
                      NEXT FIELD codpos
                   END IF
               ELSE
                  SELECT "X" 
                  FROM tab_codpos
                  WHERE cpos_cod = g_afiliados.codpos

                  IF STATUS = 100 THEN
                     ERROR "Cod. Post. no existe en catalogo, pon valor NULO p/desplegar pantalla de Codigos"
                     NEXT FIELD codpos
                  END IF

                  CALL Despliega_colonias(g_afiliados.codpos)
                  RETURNING 
                            g_afiliados.colonia,
                            g_afiliados.delega,
                            g_afiliados.desc_delega_1,
                            g_afiliados.ciudad,
                            g_afiliados.desc_ciudad_1,
                            g_afiliados.estado,
                            g_afiliados.desc_estado_1

               END IF
                  DISPLAY BY NAME g_afiliados.codpos,
                                  g_afiliados.colonia,
                                  g_afiliados.delega,
                                  g_afiliados.desc_delega_1,
                                  g_afiliados.ciudad,
                                  g_afiliados.desc_ciudad_1,
                                  g_afiliados.estado,
                                  g_afiliados.desc_estado_1 
 
	END INPUT
END FUNCTION

