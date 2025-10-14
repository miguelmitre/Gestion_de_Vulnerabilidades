DATABASE safre_af

GLOBALS
   DEFINE x_nss          CHAR(11),
          x_fecha        DATE,
          x_fecha_pago   CHAR(8),
          xx_fecha_pago  DATE

   DEFINE pos            INTEGER

END GLOBALS

MAIN
   display "INICIO DE PROCESO"

   CALL ind_actividad()

   display "FIN DE PROCESO"
END MAIN

FUNCTION ind_actividad()

   LET pos = 0

   SELECT nss,
          max(fecha_conversion) fecha
   FROM   dis_cuenta, safre_tmp:cuota
   WHERE  nss = n_seguro
   AND   subcuenta in (1,2,5,6,9)
   AND   tipo_movimiento = 1
   GROUP BY 1
   INTO TEMP tmp_activo

   INSERT INTO tmp_activo
   SELECT nss,
          max(fecha_conversion) fecha
   FROM   dis_cuenta04, safre_tmp:cuota
   WHERE  nss = n_seguro
   AND   subcuenta in (1,2,5,6,9)
   AND   tipo_movimiento = 1
   GROUP BY 1

   INSERT INTO safre_tmp:tmp_ind_activo
   SELECT nss,
          max(fecha)
   FROM   tmp_activo
   GROUP BY 1
END FUNCTION
