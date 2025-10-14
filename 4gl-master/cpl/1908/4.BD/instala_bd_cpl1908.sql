UNLOAD TO /safre_prc/sep/rescate/sep_movimientos_invadido_cpl19082.unl
SELECT   *
FROM     sep_movimientos_invadido
WHERE    nss= '33907324801' ;

UNLOAD TO /safre_prc/sep/rescate/sep_reg_patro_separador_cpl1908.unl
SELECT   *
FROM     sep_reg_patro_separador
WHERE    idSolicitudSeparacion   =  528;

--
DELETE   FROM sep_movimientos_invadido
WHERE    nss = '33907324801';

DELETE FROM sep_reg_patro_separador
WHERE    idSolicitudSeparacion   =  528;