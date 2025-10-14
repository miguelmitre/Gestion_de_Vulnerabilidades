--------------------------------------------------------------------------------
--Proyecto                    => SAFRE                                         #
--Propietario                 => E.F.P.                                        #
--Programa PENM100            => Calculo de devengados y mensualidad corriente #
--                            => para PMG                                      #
--Fecha creacion              => ABRIL 2015                                    #
--Sistema                     => PMG                                           #
--Autor                       => Emmanuel Reyes Pérez                          #
--------------------------------------------------------------------------------
  DROP FUNCTION fn_calcula_devengado_ren;
CREATE FUNCTION fn_calcula_devengado_ren (pd_mensualidad       DECIMAL (10,2),
                                          pd_fecha_ultima_mens DATE          , --para saber inicio de calculo
                                          pd_fecha_hasta       DATE          )

-- tipo de variables de retorno
  RETURNING DECIMAL (10,2),
            DECIMAL (10,2);

--Variables utilizadas en la funcion
  DEFINE ld_aux_fec_desde  DATE           ; --Fecha inicio busqueda (incremental)
  DEFINE ld_aux_fec_hasta  DATE           ; --Fecha hasta
  DEFINE ls_mes            SMALLINT       ; --Obtiene mes para fecha aux
  DEFINE ls_anio           SMALLINT       ; --Obtiene año para fecha aux
  DEFINE ls_tipo_pago      SMALLINT       ; --Define si el importe de pago es normal u 11p
  DEFINE ld_acumulador     DECIMAL (10,2) ; --Acumula el monto devengado
  DEFINE ld_importe        DECIMAL (10,2) ; --Recibe el importe de pago
  DEFINE ld_mens_actual    DECIMAL (10,2) ; --Recupera la mensualidad correspondiente a la fecha hasta

    --Variables de exception
   DEFINE li_sql_err      INT        ;
   DEFINE li_isam_err     INT        ;
   DEFINE lc_error_info   CHAR(100)  ;
   DEFINE lc_err_tec_desc CHAR(100)  ;
   DEFINE lc_diagnostico  CHAR(3)    ;

   ON EXCEPTION

          SET li_sql_err, li_isam_err, lc_error_info

          RAISE EXCEPTION li_sql_err, li_isam_err, lc_error_info;

   END EXCEPTION;
   
   IF pd_fecha_ultima_mens > pd_fecha_hasta THEN
      RAISE EXCEPTION -46, 0, "FECHA HASTA NO PUEDE SER MAYOR";
   END IF 
   
   --Generando la fecha desde
   LET ls_mes  = MONTH(pd_fecha_ultima_mens);
   LET ls_anio =  YEAR(pd_fecha_ultima_mens);
   
   LET ld_aux_fec_desde  = (MDY(ls_mes,1,ls_anio) + 1 units MONTH)  ;

   --Generando fecha hasta
   LET ls_mes  = MONTH(pd_fecha_hasta);
   LET ls_anio =  YEAR(pd_fecha_hasta);
   
   LET ld_aux_fec_hasta  = (MDY(ls_mes,1,ls_anio) - 1 units MONTH)  ;

   LET ld_acumulador  = 0; --Limpia el acumulador
   LET ld_mens_actual = 0;
   LET ls_tipo_pago   = 0;

   -----------------------------------------------------------------------------
   -- Determina que tipo de pago se utilizará
   -----------------------------------------------------------------------------
   SELECT 1
     INTO ls_tipo_pago
     FROM tab_pmg_historica
    WHERE importe_mensual = pd_mensualidad;

    IF DBINFO('sqlca.sqlerrd2') = 0 THEN
      SELECT 2
        INTO ls_tipo_pago
        FROM tab_pmg_historica
       WHERE importe_mensual_11p = pd_mensualidad;

   END IF

   IF ls_tipo_pago = 0 THEN
      RAISE EXCEPTION -56,0,"NO SE ENCONTRÓ EL MONTO RECIBIDO";
   END IF 
   -----------------------------------------------------------------------------
   -- Cuando el tipo de pago es importe_mensual
   -----------------------------------------------------------------------------

   IF ls_tipo_pago = 1 THEN 
      WHILE ld_aux_fec_desde <= ld_aux_fec_hasta

         LET ld_importe = 0; --Limpia la variable

         SELECT MAX(importe_mensual)
           INTO ld_importe
           FROM tab_pmg_historica
          WHERE fecha_desde <= ld_aux_fec_desde;
         
         IF DBINFO('sqlca.sqlerrd2') = 0 THEN
            RAISE EXCEPTION -56,0,"NO SE ENCONTRÓ EL MONTO";
         END IF
         --acumula montos
         LET ld_acumulador = ld_acumulador + ld_importe;
         --Incrementa fecha aux
         LET ld_aux_fec_desde = ld_aux_fec_desde + 1 units MONTH;
      END WHILE;

   END IF

   -----------------------------------------------------------------------------
   -- Cuando el tipo de pago es importe_mensual_11p
   -----------------------------------------------------------------------------

   IF ls_tipo_pago = 2 THEN 
      WHILE ld_aux_fec_desde <= ld_aux_fec_hasta

        LET ld_importe = 0; --Limpia la variable

        SELECT MAX(importe_mensual_11p)
          INTO ld_importe
          FROM tab_pmg_historica
         WHERE fecha_desde <= ld_aux_fec_desde;
         
         IF DBINFO('sqlca.sqlerrd2') = 0 THEN
            RAISE EXCEPTION -56,0,"NO SE ENCONTRÓ EL MONTO";
         END IF
         
         --acumula montos
         LET ld_acumulador = ld_acumulador + ld_importe;
         --Incrementa fecha aux
         LET ld_aux_fec_desde = ld_aux_fec_desde + 1 units MONTH;
      END WHILE;

   END IF

   -----------------------------------------------------------------------------
   --Recupera la mensualidad actual de acuerdo al tipo de pago
   -----------------------------------------------------------------------------
   IF ls_tipo_pago = 1 THEN

        SELECT MAX(importe_mensual)
          INTO ld_mens_actual
          FROM tab_pmg_historica
         WHERE fecha_desde <= pd_fecha_hasta;

        IF DBINFO('sqlca.sqlerrd2') = 0 THEN
           RAISE EXCEPTION -56,0,"NO SE ENCONTRÓ EL MONTO MENS ACTUAL";
        END IF

   END IF

   IF ls_tipo_pago = 2 THEN

        SELECT MAX(importe_mensual_11p)
          INTO ld_mens_actual
          FROM tab_pmg_historica
         WHERE fecha_desde <= pd_fecha_hasta;

        IF DBINFO('sqlca.sqlerrd2') = 0 THEN
            RAISE EXCEPTION -56,0,"NO SE ENCONTRÓ EL MONTO MENS ACTUAL";
         END IF

   END IF

   RETURN ld_acumulador,
          ld_mens_actual;

END FUNCTION;