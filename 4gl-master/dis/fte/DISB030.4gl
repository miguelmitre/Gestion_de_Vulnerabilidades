###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Owner             => E.F.P                                               #
#Programa DISB030  => Genera saldo de las liquidaciones del dia corriente #
#Fecha             => 26 de octubre 2001.                                 #
#Por               => CLAUDIA URZUA ROSAS.                                #
#Sistema           => DIS.                                                #
###########################################################################

DATABASE safre_af


DEFINE		eje_saldo_cuenta	CHAR(40)


MAIN


CALL startlog ("DISB030.log")

LET eje_saldo_cuenta = "execute procedure saldo_cuenta()"

LET eje_saldo_cuenta = eje_saldo_cuenta CLIPPED

PREPARE eje_saldo FROM eje_saldo_cuenta

EXECUTE eje_saldo

END MAIN
