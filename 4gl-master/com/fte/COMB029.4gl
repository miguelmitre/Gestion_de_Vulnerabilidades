################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Owner             => E.F.P                                                    #
#Programa COMB003  => PROCESO BATCH CALCULA LAS COMISIONES RESUMEN TODOS CALC  #
#Sistema           => COM.                                                     #
#By                => GERARDO ALFONSO VEGA PAREDES                             #
#Fecha Actualiza   => 04 septiembre 2004.                                      #
################################################################################
DATABASE safre_af

GLOBALS

   DEFINE aux_pausa   CHAR(1)

   DEFINE hoy	      DATE

   DEFINE g_usuario   CHAR(8)

   DEFINE g_opcion    CHAR(1)

   DEFINE hora_inicial CHAR(8),
          hora_final   CHAR(8),
          vhora_max    CHAR(8),
          vhora_final  CHAR(8),
          cla_sel      CHAR(300),
          cla_upd      CHAR(450)

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE g_bat RECORD LIKE dis_ctrl_proceso.*

   DEFINE vmensaje CHAR(50)

   DEFINE vrow INTEGER

END GLOBALS

MAIN

   CALL STARTLOG("COMB029.log")

   LET hoy = ARG_VAL(1)

   SELECT *, 
          USER 
   INTO   g_param.*,
          g_usuario 
   FROM   seg_modulo
   WHERE  modulo_cod = "com"

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   com_ctrl_proceso ",
                 "WHERE  proceso_cod = 'COMB029' ",
                " AND    etapa_cod   = 7 " CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   CALL Proceso_principal()

   LET vhora_final = TIME

   LET vmensaje = "CIERRE FINALIZADO"

   LET cla_upd = "UPDATE com_ctrl_proceso ",
                 "SET    hora_final   = ","'",vhora_final,"'",",",
                 "       resultado    = ","'",vmensaje,"'",
                " WHERE  proceso_cod  = 'COMB029' ",
                " AND    etapa_cod    = 7 ",
                " AND    consecutivo = ",vrow CLIPPED

   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe

   DISPLAY "FIN AGRUPACION"

END MAIN

FUNCTION Proceso_principal()
   DEFINE g_reg RECORD
      codven             LIKE com_comis_detalle.codven,
      cod_tipo_prom      LIKE com_comis_detalle.cod_tipo_prom,
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      nivel              LIKE com_comis_detalle.nivel,
      fecha_corte        LIKE com_comis_detalle.fecha_corte,
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      monto_comision     LIKE com_comis_detalle.monto_comision,
      promedio_sm        DECIMAL(12,2),
      feminima           DATE,
      femaxima           DATE,
      num_sm             DECIMAL(12,2),
      cod_esq_comision   LIKE com_comis_detalle.cod_esq_comision,
      contador           SMALLINT
   END RECORD

   DEFINE AA,num           SMALLINT
   DEFINE l_monto_comision DECIMAL(12,2)
   DEFINE x_nivel          SMALLINT
   DEFINE x_ind_bono       SMALLINT,
          x_cod_tabla_ispt SMALLINT,
          vfiscal_cod      SMALLINT,
          vmonto_percep    DECIMAL(12,2),
          cla_sel          CHAR(300),
          entro            SMALLINT,
          vpago            SMALLINT

   LET entro = FALSE

   DECLARE cursor_1 CURSOR FOR 
   SELECT codven,
          0,              --cod_tipo_prom,
          coduni_n1,
          nivel,
          MAX(fecha_corte),
          SUM(salario_base_comis)/COUNT(*),
          SUM(monto_comision),
          SUM (num_sm)/COUNT(*),
          "",
          "",
--          MIN(fentcons),
--          MAX(fentcons),
          SUM(num_sm),
          0,             --cod_esq_comision,
          COUNT(*)
   FROM   com_comis_detalle
   WHERE  fecha_corte = hoy
   GROUP  BY 1,2,3,4,12
   ORDER  BY 1,2,3,4

   LET num = 0

   FOREACH cursor_1 INTO g_reg.*

      LET num = num + 1
        
      SELECT SUM(monto)
      INTO   vmonto_percep
      FROM   com_percepcion
      WHERE  cod_tipo_prom =  g_reg.cod_tipo_prom
      AND    min_afili     >= g_reg.contador
      IF vmonto_percep IS NULL THEN
         LET vmonto_percep = 0
      END IF

      INSERT INTO com_comis_resumen VALUES  
        (g_reg.codven,            # codven
         g_reg.cod_tipo_prom,     # cod_tipo_prom
         g_reg.coduni_n1,         # coduni_n1
         g_reg.nivel,             # nivel
         g_reg.feminima,          # fecha_desde
         g_reg.femaxima,          # fecha_hasta
         g_reg.fecha_corte,       # fecha_corte
         g_reg.salario_base_comis,# prom_salario_base
         g_reg.contador,          # total_afiliados
         g_reg.num_sm,            # total_sm
         g_reg.promedio_sm,       # promedio_sm
         g_reg.cod_esq_comision,  # cod_esq_comision
         g_reg.monto_comision,    # total_comision -Este se calcula si pasa meta
         g_reg.monto_comision,    # comis_calculada -Se guarda comsion calculada
         vmonto_percep,           # monto_percep
         0,                       # total_bono
         300,                     # estado_comision
         hoy,                     # fecha_calculo
         g_usuario,               # usuario
         0,                       # consolida
         0)                       # pago      

   END FOREACH

END FUNCTION
