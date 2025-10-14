
--
DELETE   FROM sep_movimientos_invadido
WHERE    nss = '33907324801';

DELETE FROM sep_reg_patro_separador
WHERE    idSolicitudSeparacion   =  528;

--

LOAD FROM /safre_prc/sep/rescate/sep_movimientos_invadido_cpl1908.unl
INSERT INTO sep_movimientos_invadido;

LOAD FROM /safre_prc/sep/rescate/sep_reg_patro_separador_cpl1908.unl
INSERT INTO sep_reg_patro_separador;