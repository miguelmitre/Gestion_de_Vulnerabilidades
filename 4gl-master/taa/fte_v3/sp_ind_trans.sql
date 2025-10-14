 
CREATE FUNCTION "safre".fn_ind_transferencia( p_nss       CHAR(11),
                                              p_indicador SMALLINT,
                                              p_fecha_ind_transf DATE
                                     )
RETURNING SMALLINT ;

DEFINE v_usuario          CHAR(10) ;
DEFINE v_status           SMALLINT ;
DEFINE v_indicador        SMALLINT ;
DEFINE v_fecha_ind_transf DATE ;


LET v_status = 0 ;

   SELECT ind_transferencia,
          fecha_ind_transf ,
          usuario
     INTO v_indicador      ,
          v_fecha_ind_transf,
          v_usuario
     FROM cta_ctr_cuenta
    WHERE nss = p_nss  ;

   IF p_indicador = 1 THEN    ---  Inicia transferencia de decimos
      IF v_indicador = 0 THEN
         ---  Actualizar en cta_ctr_cuenta
         ---  Insertar historico en cta_his_ind_dec
         LET v_status = 1 ;

      END IF
   ELSE   ---  Termina transferencia
      IF EXISTS (  SELECT ind_transferencia
                     FROM tab_ind_transferencia
                    WHERE ind_transferencia = p_indicador
                ) THEN
         IF v_indicador = 0 OR
            v_indicador = 1 THEN
            LET v_status = 1 ;
         END IF
      END IF
   END IF

   IF v_status = 1 THEN
      IF NOT EXISTS (
                      SELECT subcuenta              ,
                             SUM(monto_en_acciones)
                        FROM dis_cuenta
                       WHERE nss = p_nss
                         AND subcuenta NOT IN (3,10,4,8,14)
                         AND siefore = 2
                      GROUP BY 1
                      HAVING SUM(monto_en_acciones) > 0
                    ) THEN
         UPDATE cta_ctr_cuenta
            SET ind_transferencia = p_indicador ,
                fecha_ind_transf  = p_fecha_ind_transf
          WHERE nss = p_nss ;

         INSERT INTO cta_his_ind_dec VALUES ( p_nss       ,
                                              v_indicador ,
                                              v_fecha_ind_transf ,
                                              v_usuario          ,
                                              TODAY
                                             );
      ELSE
         LET v_status = 0 ;
      END IF
   END IF

RETURN v_status ;

END FUNCTION
;


 

 

 

 

 

 

