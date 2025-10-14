#################################################################################
#Proyecto          => SISTEMA DE AFORE ( SAFRE )                                #
#Owner             => E.F.P.                                                    #
#Programa RETC8281 => INCORPORA PAGO DE DISPOSICION DE RECURSOS TIPO RET. "P"   #
#Fecha creacion    => 10 DE DICIEMBRE DE 2013                                   #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
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

    CALL f_lib_abre_log("RETC8281")   
   
    LET gr_datos.nom_prog   = ARG_VAL(0)
    LET gr_datos.tipo_ret   = "P"
    LET HOY                 = TODAY

    LET gc_comando = " fglgo RETC861 ", gr_datos.nom_prog CLIPPED, " ", gr_datos.tipo_ret, " ", HOY
    RUN gc_comando

END MAIN

