################################################################################
# Proyecto          => E.F.P.                                                  #
# Programa ARCB043  => MENU DE CONSULTA DE ACREDITADOS                         #
# By                => JOSE LUIS SALDIVAR CARDOSO                              #
# Fecha creacion    => 07 DE FEBRERO DEL 2003                                  #
# Sistema           => ACR                                                     #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE    fecha    		DATE
    DEFINE    opcion   		CHAR(01)
    DEFINE    seg_modulo	RECORD LIKE seg_modulo.*
    DEFINE    ruta		CHAR(120)
  
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
	INPUT WRAP         ,
	ACCEPT KEY CONTROL-I

    SELECT *
      INTO seg_modulo.* 
      FROM seg_modulo
     WHERE modulo_cod = 'acr'

    LET ruta = seg_modulo.ruta_exp

    LET  fecha = TODAY
    OPEN WINDOW ACRB043 AT 4,4 WITH FORM "ACRB043" ATTRIBUTE(BORDER)
    DISPLAY "  ACRB043                < CTRL-C > Salir                                      " AT 3,1 ATTRIBUTE(REVERSE)

    INPUT BY NAME opcion,fecha WITHOUT DEFAULTS
	AFTER FIELD opcion

	   CASE opcion
	       WHEN  1
                LET ruta = "fglgo ", seg_modulo.ruta_exp CLIPPED, "/ACRB040.4gi"
                LET ruta = ruta CLIPPED

	         RUN ruta 
               WHEN  2
                LET ruta = "fglgo ", seg_modulo.ruta_exp CLIPPED, "/ACRB041.4gi"
	         RUN ruta 
               WHEN  3
                LET ruta = "fglgo ", seg_modulo.ruta_exp CLIPPED, "/ACRB042.4gi"
	         RUN ruta 
               WHEN  4
		 EXIT INPUT
            END CASE

        ON KEY(INTERRUPT)
	   EXIT INPUT

    END INPUT
    CLOSE WINDOW ACRB043

END MAIN

