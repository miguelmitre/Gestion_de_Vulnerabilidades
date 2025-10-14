#################################################################################
#Proyecto          => SISTEMA DE AFORE ( SAFRE )                                #
#Owner             => E.F.P.                                                    #
#Programa RETC807  => INCORPORA PAGO DE DISPOSICION DE RECURSOS TIPO RET. "D"   #
#Fecha creacion    => 27 ENERO DEL 2004                                         #
#By                => JOSE LUIS SALDIVAR CARDOSO                                #
#Fecha actualiza   => 07 DE JULIO DEL 2004                                      #
#Actualizacion     => LAURA EUGENIA CORTES GUZMAN                               #
#Fecha actualiza   => 19 DE DICIEMBRE DE 2007                                   #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     Actualizacion para soporte de multisiefores               #
#Fecha actualiza   => 18 DE MAYO DE 2010                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     Se modifica el funcionamiento del programa para convertir #
#                     el proceso en un lanzador del programa general RETC960    #
#                     Este programa se puede eliminar en cuanto se haga el      #
#                     cambio a manejar un unico programa de liquidacion         #
#Sistema           => RET                                                       #
#################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_datos RECORD
        nom_prog        CHAR(010) ,
        tipo_ret        CHAR(001)
    END RECORD 

    DEFINE
        HOY             DATE

    DEFINE
        gc_comando      CHAR(100)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC807.log")
   
    LET gr_datos.nom_prog   = ARG_VAL(0)
    LET gr_datos.tipo_ret   = "D"
    LET HOY                 = TODAY

    LET gc_comando = " fglgo RETC861 ", gr_datos.nom_prog CLIPPED, " ", gr_datos.tipo_ret, " ", HOY
    RUN gc_comando

END MAIN
