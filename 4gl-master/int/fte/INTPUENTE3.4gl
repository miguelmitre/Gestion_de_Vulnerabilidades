DATABASE safre_af

DEFINE v_ruta_envio  CHAR(100)
DEFINE v_comando     CHAR(200)
DEFINE v_ruta_exp    CHAR(100)
DEFINE v_archivo     CHAR(250)
DEFINE v_hora        CHAR(10)
DEFINE v_idSolicitud INTEGER
DEFINE v_safredir    CHAR(100)
DEFINE v_ejecuta     CHAR(100)
DEFINE v_cuantos     INTEGER
DEFINE v_hoy         DATE
DEFINE v_idtposolicitud LIKE catalogosistema.idcatalogo
DEFINE c_idtposolicitud CHAR(40)

DEFINE p_tposolicitud    SMALLINT
DEFINE c_tposolicitud    CHAR(3)

DEFINE v_fechaini    DATE
DEFINE v_fechafin    DATE


--Los valores posibles de este parametro son 1,2 u 8


MAIN

     LET p_tposolicitud = ARG_VAL(1)
     LET v_fechaini     = ARG_VAL(2)
     LET v_fechafin     = ARG_VAL(3)

     display "p_tposolicitud:",p_tposolicitud

     LET c_tposolicitud = p_tposolicitud
     LET c_tposolicitud = "'", c_tposolicitud CLIPPED, "'"

     display "c_tposolicitud:", c_tposolicitud


     SELECT idcatalogo
       INTO v_idtposolicitud
     FROM catalogosistema
     WHERE cvenatural = c_tposolicitud
     AND idpadre IN ( SELECT idcatalogo
                      FROM catalogosistema
                      WHERE cvenatural="TPOSOLICITUD"
                      AND idpadre IS NULL)


     LET c_idtposolicitud = v_idtposolicitud


     -- *** Se Procede a Contabilizar el Numero de Solicitudes Migradas, para regresar este numero ***.
     SELECT count(sa.idsolicitud)
        INTO v_cuantos
     FROM solicitudafi sa
     WHERE  sa.stamcertificafi="15-Por Confirmar"
     AND    (sa.stamdocumentafi="Documentos Recibidos MC" OR sa.stamdocumentafi="Documentos Validados MC")
     AND    sa.stamprocessafi ="Revisada Documentos"
     AND    sa.stamdigimageafi="Completa Img"
     AND    sa.stamresulvallogica="Aceptada Val Doc"
     AND    sa.stamresulclasifdoc="Aceptada Clasif Doc"
     AND    sa.stamfile="Por Enviar Arch Fisico"
     AND    sa.idtposolicitud =v_idtposolicitud
     AND    sa.frecepcion >= v_fechaini
     AND    sa.frecepcion <= v_fechafin;


     --**** Se Ejecuta el SPL que realiza la migracion de los datos a SV2 ****
     LET v_ejecuta = "EXECUTE PROCEDURE sp_eje_datos_captura(",
                           c_idtposolicitud CLIPPED, ",'" ,v_fechaini, "','" ,v_fechafin,"')"

     display "v_ejecuta:", v_ejecuta

     PREPARE eje_spl FROM v_ejecuta

     EXECUTE eje_spl

     EXIT PROGRAM (v_cuantos)
END MAIN




