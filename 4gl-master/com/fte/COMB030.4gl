################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Owner             => E.F.P                                                    #
#Programa COMB030  => CALCULA PREMIO A PROMOTORES                              #
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

   DEFINE opc CHAR(01)

END GLOBALS

MAIN

   CALL STARTLOG("COMB030.log")

   LET hoy = ARG_VAL(1)

   SELECT *, 
          USER 
   INTO   g_param.*,
          g_usuario 
   FROM   seg_modulo
   WHERE  modulo_cod = "com"

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   com_ctrl_proceso ",
                 "WHERE  proceso_cod = 'COMB030' ",
                " AND    etapa_cod   = 9" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   CALL Proceso_principal()

   LET vhora_final = TIME

   LET vmensaje = "PREMIO FINALIZADO"

   LET cla_upd = "UPDATE com_ctrl_proceso ",
                 "SET    hora_final   = ","'",vhora_final,"'",",",
                 "       resultado    = ","'",vmensaje,"'",
                " WHERE  proceso_cod  = 'COMB030' ",
                " AND    etapa_cod    = 9 ",
                " AND    consecutivo = ",vrow CLIPPED

   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe

   DISPLAY "FIN PREMIO"

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
      promedio_sm        LIKE com_comis_resumen.promedio_sm,
      feminima           DATE,
      femaxima           DATE,
      num_sm             LIKE com_comis_detalle.num_sm,
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
          vpago            SMALLINT,
          vmonto_bono      DECIMAL(12,2)

   DEFINE vfecha_inicial  date,
          vfecha_primer_d date,
          vfecha_final    date,
          vmes            smallint,
          vano            smallint,
          vdias           smallint

   LET vfecha_primer_d = MDY(MONTH(hoy),1,YEAR(hoy))

   LET vfecha_inicial = vfecha_primer_d - 2 UNITS MONTH

   LET vfecha_final   = vfecha_primer_d - 1 UNITS MONTH

   CALL ultimo_dia_mes(vfecha_final) RETURNING vdias

   LET vfecha_final = MDY(MONTH(vfecha_final),vdias,YEAR(vfecha_final))

   DECLARE cursor_1 CURSOR FOR 
   SELECT codven,                            --codven
          0,                                 --cod_tipo_prom
          coduni_n1,                         --coduni_n1
          nivel,                             --nivel
          MAX(fecha_corte),                  --fecha_corte
          SUM(salario_base_comis),           --salario_base_comis
          SUM(monto_comision),               --monto_comision
          SUM(num_sm)/COUNT(*),              --promdio_sm
          "",                                --feminima
          "",                                --femaxima
          SUM(num_sm),                       --num_sm
          0,                                 --cod_esq_comision
          COUNT(*)                           --contador
   FROM   com_comis_detalle
   WHERE  fecha_corte = hoy
   AND    fentcons between vfecha_inicial AND vfecha_final
   AND    nivel       = 1 --- solo se paga premio a nivel 1
   AND    estado_comision in (260,280) --premio solo edo_com c/reca
--   AND    cod_esq_comision = 30
   GROUP  BY 1,2,3,4,12
   ORDER  BY 1,2,3,4

   LET num = 0

   FOREACH cursor_1 INTO g_reg.*

      SELECT monto_bono
      INTO   vmonto_bono
      FROM   com_cuadro_bono
      WHERE  cod_esq_bono = 10            --codigo bono (premio)
      AND    rango_desde <= g_reg.num_sm
      AND    rango_hasta >= g_reg.num_sm
      IF vmonto_bono IS NULL THEN
         LET vmonto_bono = 0
      END IF
      LET g_reg.monto_comision = vmonto_bono

      IF g_reg.monto_comision > 0 THEN

         INSERT INTO com_comis_detalle VALUES(
            g_reg.codven,             #codven
            1,                        #nivel
            g_reg.cod_tipo_prom,      #cod_tipo_prom
            g_reg.coduni_n1,          #coduni_n1
            "VARIOS",                 #nss
            0,                        #n_folio
            0,                        #tipo_solicitud
            "",                       #fentcons
            0,                        #tipo_pago % de anticipo
            hoy,                      #fecha_corte
            g_reg.salario_base_comis, #salario_base_comis
            g_reg.num_sm,             #num_sm
            g_reg.cod_esq_comision,   #cod_esq_comision
            0,                        #cod_esq_premio
            g_reg.contador,           #porcent_comision --se guarda num afi
            g_reg.monto_comision,     #monto_comision
            "N",                      #comis_pagada
            null,                     #fecha_pago
            290,                      #estado_comision -- estado de premio
            hoy,                      #fecha_calculo
            g_usuario)                #usuario
      END IF

   END FOREACH

END FUNCTION

FUNCTION ultimo_dia_mes(fecha)
   DEFINE
      fecha  DATE,
      vdias  INTEGER

   CASE MONTH(fecha)
       WHEN  1 LET vdias=31
       WHEN  2 IF YEAR(fecha) MOD 4 = 0 THEN
                  LET vdias = 29
               ELSE
                  LET vdias = 28
               END IF
       WHEN  3 LET vdias=31
       WHEN  4 LET vdias=30
       WHEN  5 LET vdias=31
       WHEN  6 LET vdias=30
       WHEN  7 LET vdias=31
       WHEN  8 LET vdias=31
       WHEN  9 LET vdias=30
       WHEN 10 LET vdias=31
       WHEN 11 LET vdias=30
       WHEN 12 LET vdias=31
   END CASE
   RETURN vdias
END FUNCTION

