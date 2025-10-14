
 
CREATE PROCEDURE "safre".sp_cierra_marca( p_nss           CHAR(11),
                                  p_tipo_traspaso  SMALLINT
                                  )
DEFINE v_marca_causa  SMALLINT ;
DEFINE v_rechazo      SMALLINT ;
DEFINE v_marca_cod    SMALLINT ;
DEFINE v_existe       SMALLINT ;
DEFINE v_estado_marca SMALLINT ;
DEFINE v_correlativo  INTEGER  ;
DEFINE v_fecha_causa  DATE     ;
DEFINE v_usuario      CHAR(10) ;

LET v_existe       = 1 ;
LET v_correlativo  = 0 ;

LET v_fecha_causa  = TODAY;
LET v_usuario      = USER;

IF (p_tipo_traspaso = 12 OR
    p_tipo_traspaso = 20) THEN
      SELECT a.marca_cod
      INTO   v_marca_cod
      FROM   cta_act_marca a
      WHERE  a.marca_cod in (243,244)
        AND  a.nss        =  p_nss;
END IF
IF (p_tipo_traspaso = 24 OR
    p_tipo_traspaso = 25) THEN
      SELECT a.marca_cod
      INTO   v_marca_cod
      FROM   cta_act_marca a
      WHERE  a.marca_cod   =  281
        AND  a.nss         =  p_nss;
END IF

SELECT a.marca_cod
INTO   v_marca_causa
FROM   taa_cd_tipo_traspaso a
WHERE  a.tipo_traspaso = p_tipo_traspaso;

         LET v_estado_marca = 30 ;

         EXECUTE PROCEDURE desmarca_cuenta ( p_nss         , -- nss
                                             v_marca_cod   , -- marca_entra
                                             v_correlativo , -- correlativo
                                             v_estado_marca, -- estado_marca
                                             v_marca_causa , -- marca_causa
                                             v_usuario  );   -- usuario

END PROCEDURE;

