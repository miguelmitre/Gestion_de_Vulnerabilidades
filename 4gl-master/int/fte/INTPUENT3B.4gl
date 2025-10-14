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

DEFINE v_fechaini    DATE
DEFINE v_fechafin    DATE


--Los valores posibles de este parametro son 1,2 u 8


MAIN
     --**ACTUALIZADO 7 NOV: En esta version solo se recibe un parámetro
     LET p_tposolicitud = ARG_VAL(1)
     
     display "p_tposolicitud:",p_tposolicitud

     SELECT idcatalogo
       INTO v_idtposolicitud
     FROM catalogosistema
     WHERE cvenatural= p_tposolicitud
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
     AND    sa.idtposolicitud =v_idtposolicitud;
           
     --**** Se Ejecuta el SPL que realiza la migracion de los datos a SV2 ****
     LET v_ejecuta = "EXECUTE PROCEDURE sp_eje_datos_captura_b(",c_idtposolicitud CLIPPED,")"

     PREPARE eje_spl FROM v_ejecuta

     EXECUTE eje_spl 
     
     EXIT PROGRAM (v_cuantos)
END MAIN




