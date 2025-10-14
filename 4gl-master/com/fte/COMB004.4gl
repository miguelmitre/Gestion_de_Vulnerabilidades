##############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                          #
#Owner             => E.F.P.                                                 #
#Sistema           => COM.                                                   #
#Programa          => COMB004                                                #
#Descripcion       => PROCESO BATCH CALCULA LAS COMISIONES (COMSION_DETALLE) #
#                  => DE ACUERDO A LA RECAUDACION                            #
#By                => GERARDO ALFONSO VEGA PAREDES                           #
#Fecha             => 21 enero 2005.                                         #
##############################################################################

DATABASE safre_af

GLOBALS

   DEFINE aux_pausa       CHAR(1)
   DEFINE hoy             DATE
   DEFINE g_usuario       CHAR(8)
   DEFINE g_opcion        CHAR(1)
   DEFINE g_desde         DATE
   DEFINE g_hasta         DATE

   DEFINE g_record RECORD
      g_opcion CHAR(1)
   END RECORD

   DEFINE g_reg   RECORD LIKE com_comis_detalle.*

   DEFINE g_reg1  RECORD LIKE com_comis_detalle.*

   DEFINE AA,num SMALLINT,
          l_monto_comision,
          vmonto              DECIMAL(12,2),
          vcont,vcontafi      INTEGER,
          vcriterio	      SMALLINT,
          vcodven             CHAR(10),
          cla_sel             CHAR(900),
          opc                 CHAR(01),
          vcont_afi           INTEGER,
          vcont_afi2          INTEGER,
          vminimo_solicitudes SMALLINT,
          vmonto_comision     DECIMAL(12,2),
          vpago               DECIMAL(12,2),
          vsuma_salario       DECIMAL(12,2)


   DEFINE hora_inicial CHAR(8),
          hora_final   CHAR(8),
          cla_upd      CHAR(450)

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE g_param_com RECORD LIKE com_parametro.*

   DEFINE g_bat RECORD LIKE dis_ctrl_proceso.*

   DEFINE vestado_comision     SMALLINT,
          vestado_comision_ant SMALLINT

   DEFINE vmensaje CHAR(50)

   DEFINE vcomision DECIMAL(12,2)


   DEFINE vporcentaje DECIMAL(6,2)

   DEFINE vrow INTEGER

   DEFINE ejecuta CHAR(200)


   ----- VARIABLES  PARA NUEVO ESQ COM ------
   DEFINE l_sm              DECIMAL(12,2),
          vtot_sm           DECIMAL(12,2),
          vcod_esq_comision SMALLINT,
          vanticipo         DECIMAL(8,2)

END GLOBALS

MAIN

   CALL STARTLOG("COMB004.log")

   LET hoy = ARG_VAL(1)

   CALL Inicializa()

   CALL Calculo_afiliacion()

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   com_ctrl_proceso ",
                 "WHERE  proceso_cod = 'COMB003' ",
                " AND    etapa_cod   = 2" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   LET hora_final = TIME

   LET vmensaje = "CALCULO RELIQUIDACION FINALIZADA"

   LET cla_upd = "UPDATE com_ctrl_proceso ",
                 "SET    hora_final   = ","'",hora_final,"'",",",
                 "       resultado    = ","'",vmensaje,"'",
                " WHERE  proceso_cod  = 'COMB003' ",
                " AND    etapa_cod    = 2 ",
                " AND    consecutivo = ",vrow CLIPPED

   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe

   DISPLAY "FIN CALCULO RELIQUIDACION"

END MAIN

FUNCTION Inicializa()

   SELECT *,
          USER 
   INTO   g_param.*,
          g_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "com"

   SELECT *
   INTO   g_param_com.*
   FROM   com_parametro

   LET vcod_esq_comision = 0

   LET vcomision = 0

END FUNCTION

FUNCTION Calculo_afiliacion()

   CREATE TEMP TABLE cont_afi
      (codven       char(10),
       cuantos      integer,
       suma_salario decimal(12,2)
      );

   CREATE index cont_afi_1 on cont_afi(codven);

   DECLARE curcont CURSOR FOR
   SELECT codven,
          COUNT(*),
          SUM(salario_base_comis)
   FROM   com_comis_detalle
   WHERE  estado_comision = 50
   GROUP  BY 1
   ORDER  BY 1

   FOREACH curcont INTO vcodven,vcontafi,vsuma_salario
      INSERT INTO cont_afi values(vcodven,vcontafi,vsuma_salario)
      LET vcont_afi = vcont_afi + 1
   END FOREACH

   DECLARE cursor_1 CURSOR FOR 
   SELECT a.*,
          b.criterio_cod,
          cuantos,
          suma_salario
   FROM   com_comis_detalle a,
          com_esq_comis b,
          cont_afi c
   WHERE  a.estado_comision  = 50
   AND    b.cod_esq_comision = a.cod_esq_comision
   AND    a.codven = c.codven
   ORDER  BY codven,n_folio

   FOREACH cursor_1 INTO g_reg.*,vcriterio,vcont_afi,vsuma_salario

      CALL Obtiene_esquema_comision()         

      SELECT minimo_requerido
      INTO   vminimo_solicitudes
      FROM   com_esq_comis 
      WHERE  cod_esq_comision = vcod_esq_comision
   
      IF STATUS = NOTFOUND THEN
         DISPLAY "No existe Esquma Comision ",
                  g_reg.cod_esq_comision," para el Promotor ",g_reg.codven
         EXIT PROGRAM
      END IF 
   
      IF vcont_afi < vminimo_solicitudes THEN
         LET l_monto_comision = 0
         LET vestado_comision = 56  --edo comis no pag por no cumplir min sol
      ELSE
   
         LET l_monto_comision = 0
   
         CASE vcriterio
            WHEN 1
---               CALL Porcentaje() RETURNING l_monto_comision
            WHEN 2
               CALL Cantidad() RETURNING l_monto_comision
            WHEN 3
               OTHERWISE
               LET l_monto_comision = 0
               DISPLAY "Criterio No reconocible"
         END CASE

         LET vcomision = g_reg.salario_base_comis * (l_monto_comision/100)*30.4

         LET g_reg.monto_comision = vcomision - g_reg.monto_comision --ajuste

         CASE vcod_esq_comision
            WHEN 10
               LET vestado_comision = 60
            WHEN 20
               LET vestado_comision = 70
            WHEN 30
               LET vestado_comision = 80
         END CASE 

      END IF 

      IF l_monto_comision IS NOT NULL THEN
         CALL Comision_afiliacion()
      END IF

   END FOREACH

END FUNCTION

FUNCTION Obtiene_esquema_comision()

   SELECT monto_sm
   INTO   l_sm
   FROM   tab_salario_minimo a
   WHERE  a.fecha_hasta_sm IS NULL 

   LET vtot_sm = vsuma_salario / l_sm

   --- obtiene esquema de acuerdo a la suma de salario--
   SELECT cod_esq_comision,
          anticipo
   INTO   vcod_esq_comision,
          vanticipo
   FROM   com_esq_comis
   WHERE  prod_desde <= vtot_sm
   AND    prod_hasta >= vtot_sm
   AND    cod_esq_comision > 0
   AND    cod_esq_comision <= 30

END FUNCTION

FUNCTION Comision_afiliacion()

   LET cla_sel = "UPDATE com_comis_detalle ",
                 "SET    monto_comision = ",g_reg.monto_comision,",",
                       " fecha_calculo = ","'",hoy,"'",",",
                       " cod_esq_comision=",vcod_esq_comision,",",
                       " estado_comision = ",vestado_comision,
                " WHERE  n_folio         = ",g_reg.n_folio,
                " AND    tipo_solicitud  = ",g_reg.tipo_solicitud,
                " AND    nivel           = ",g_reg.nivel,
                " AND    codven          = ","'",g_reg.codven,"'",
                " AND    estado_comision = 2 " CLIPPED

   PREPARE claexe FROM cla_sel
   EXECUTE claexe

END FUNCTION

FUNCTION cantidad()

   SELECT monto_comision
   INTO   l_monto_comision
   FROM   com_cuadro_comis
   WHERE  cod_esq_comision = vcod_esq_comision
   AND    rango_desde     <= g_reg.num_sm
   AND    rango_hasta     >= g_reg.num_sm

   RETURN l_monto_comision

END FUNCTION
