-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => COMB032                                                   --
-- Descripcion  => Programa que provisiona afiliaciones en com_comis_detalle --
--                 dejando afi_mae_afiliado.indicador_comision=20 donde=0    --
--                 insertando en com_comis_detalle.estado_comision=200 sin   --
--                 salario y com_comis_detalle.estado_comision=210 con       --
--                 salario lista para pagar comision.                        --
-- Sistema      => COM                                                       --
-- Por          => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 12 octubre 2004.                                          --
-- Modificado   =>                                                           --
-- Fecha        =>                                                           --
-------------------------------------------------------------------------------

DATABASE safre_af

GLOBALS

   DEFINE g_reg RECORD
      codven             LIKE afi_mae_afiliado.codven,
      coduni_n1          LIKE afi_mae_afiliado.coduni_n1,
      nss                LIKE afi_mae_afiliado.n_seguro,
      n_folio            LIKE afi_mae_afiliado.n_folio,
      tipo_solicitud     LIKE afi_mae_afiliado.tipo_solicitud,
      salario_base_comis LIKE afi_mae_afiliado.salario_base_comis,
      salario_actual     LIKE afi_mae_afiliado.salario_actual,
      fentcons           LIKE afi_mae_afiliado.fentcons,
      cod_esq_comision   LIKE afi_mae_afiliado.cod_esq_comision,
      tipo_promotor      LIKE pro_mae_promotor.nivel,
      meta_afi1          LIKE com_tipo_promotor.meta_afi1,
      meta_afi2          LIKE com_tipo_promotor.meta_afi2,
      meta_afi3          LIKE com_tipo_promotor.meta_afi3,
      meta_tra1          LIKE com_tipo_promotor.meta_tra1,
      meta_tra2          LIKE com_tipo_promotor.meta_tra2,
      meta_tra3          LIKE com_tipo_promotor.meta_tra3,
      cod_tipo_prom      LIKE com_tipo_promotor.cod_tipo_prom
   END RECORD

   DEFINE g_rev RECORD LIKE com_reverso.*

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE g_meta RECORD
      codven    CHAR(10),
      num_afi   INTEGER,
      calidad   DECIMAL(12,2),
      suamsm    DECIMAL(12,2) 
   END RECORD

   DEFINE vcod_resp_uni     CHAR(10),
          vuni_superior     CHAR(10), 
          vcod_puesto       SMALLINT,
          vcod_esq_comision SMALLINT,
          vnivel            SMALLINT,
          l_SM              DECIMAL(12,2),
          l_NUMSM           DECIMAL(12,2),
          AA                SMALLINT,
          num               SMALLINT,
          vcod_esq_ispt     SMALLINT,
          vtipo_pago        SMALLINT,
          aux_pausa         CHAR(1),
          hoy               DATE,
          g_usuario         CHAR(8),
          g_opcion          CHAR(1),
          g_desde           DATE,
          g_hasta           DATE,
          cod_tipo_promo    LIKE com_tipo_promotor.cod_tipo_prom,
          cla_where         CHAR(300),
          cla_sel           CHAR(1000),
          vcomando          SMALLINT,
          vcodven           CHAR(10),
          codven_ini        CHAR(10),
          codven_fin        CHAR(10),
          fecha_desde       DATE,
          fecha_hasta       DATE,
          tipo_solicitud    SMALLINT,
          opc               CHAR(01),
          vcomplemento      CHAR(01),
          vencontrado       SMALLINT,
          vsalario_comis    DECIMAL(12,2)
    
   DEFINE hora_inicial CHAR(8),
          hora_final   CHAR(8),
          vhora_max    CHAR(8),
          vhora_final  CHAR(8),
          cla_upd      CHAR(450)

   DEFINE g_bat RECORD LIKE dis_ctrl_proceso.*

   DEFINE vestado_comision    SMALLINT,
          vindicador_comision SMALLINT,
          vmensaje            CHAR(50),
          vfecha_ano          DATE,
          vnss                CHAR(11),
          vsalario            DECIMAL(12,2),
          vtipo               SMALLINT

   DEFINE vnum_afi  INTEGER,
          vcalidad  DECIMAL(12,2),
          vsumasm   DECIMAL(12,2)

   DEFINE vrow INTEGER

   DEFINE ejecuta CHAR(200)

END GLOBALS

MAIN
   DISPLAY "INICIO CALCULO COMISION MASA SALARIAL"

   CALL STARTLOG("COMB032.log")

   CALL inicializa()

   LET hoy = ARG_VAL(1)

   CALL Provision()

   LET ejecuta = "nohup time fglgo COMB033.4gi ",hoy," &"
   RUN ejecuta

END MAIN

FUNCTION Inicializa()
   SELECT *,
          USER 
   INTO   g_param.*,
          g_usuario 
   FROM   seg_modulo
   WHERE  modulo_cod = "com"

END FUNCTION

FUNCTION Provision()

   LET cla_sel="SELECT a.cod_promotor,",
                      "b.agenc_cod,", --nuevo cambio se toma de pro en lugar afi
                      "a.n_seguro,",
                      "a.n_folio,",
                      "a.tipo_solicitud,",
                      "a.salario_base_comis,",
                      "a.salario_actual,",
                      "a.fentcons,",
                      "d.indicador_comision,",
                      "b.nivel,",
                      "d.indicador_ispt,",
                      "d.meta_afi1,",
                      "d.meta_afi2,",
                      "d.meta_afi3,",
                      "d.meta_tra1,",
                      "d.meta_tra2,",
                      "d.meta_tra3,", 
                      "d.cod_tipo_prom ",
               "FROM   afi_mae_afiliado  a,",
                      "pro_mae_promotor  b,",
                      "com_tipo_promotor d ",
              " WHERE  a.fentcons           <= ","'",hoy,"'",
              " AND    a.frecafor           >= '04/05/2004' ",
              " AND    a.cod_promotor        = b.cod_promotor ",
              " AND    a.tipo_solicitud in (1,2) ",
              " AND    a.indicador_comision = 0 ",
              " AND    b.nivel              = d.cod_tipo_prom ",
              " ORDER BY a.n_folio " CLIPPED

   PREPARE claexe1 FROM cla_sel
   DECLARE cursor_1 CURSOR FOR claexe1

   LET vencontrado = FALSE

   FOREACH cursor_1 INTO g_reg.*

      LET g_rev.n_folio        = g_reg.n_folio
      LET g_rev.tipo_solicitud = g_reg.tipo_solicitud
      LET g_rev.fecha_corte    = hoy
      
      CALL Provisiona()

      CALL Actualiza_edo_afiliacion()

      CALL Inserta_reverso()

   END FOREACH

END FUNCTION

FUNCTION Actualiza_edo_afiliacion()

   LET vindicador_comision = 20 --nuevo estado de provision sobre masa salarial

   LET cla_sel="UPDATE afi_mae_afiliado ",
               "SET    indicador_comision = ",vindicador_comision,
              " WHERE  n_folio        = ",g_reg.n_folio,
              " AND    tipo_solicitud = ",g_reg.tipo_solicitud CLIPPED
   PREPARE claexeupd1 FROM cla_sel
   EXECUTE claexeupd1

END FUNCTION

FUNCTION Provisiona()

   DEFINE vsalario DECIMAL(12,2)

   SELECT monto_sm 
   INTO   l_sm
   FROM   tab_salario_minimo a
   WHERE (a.fecha_hasta_sm>=g_reg.fentcons AND a.fecha_desde_sm<=g_reg.fentcons)
   --AND a.fecha_hasta_sm IS NOT NULL)
   OR    (a.fecha_desde_sm<=g_reg.fentcons AND a.fecha_hasta_sm IS NULL)

   LET l_NUMSM = 0

   IF g_reg.salario_actual IS NULL OR g_reg.salario_actual = 0 THEN
      LET vsalario = g_reg.salario_base_comis
   ELSE
      LET vsalario = g_reg.salario_actual
   END IF

   IF vsalario IS NULL THEN
      LET vsalario = 0
   END IF

   LET l_NUMSM = vsalario / l_SM

   IF vsalario = 0 OR  vsalario IS NULL THEN
      LET vestado_comision = 200 --estado=sin salario diario prov.
   ELSE
      LET vestado_comision = 210 --estado=con salario diario prov.en cert
   END IF

   LET vnivel = 1

   INSERT INTO com_comis_detalle VALUES(
      g_reg.codven,             #codven
      vnivel,                   #nivel
      g_reg.tipo_promotor,      #cod_tipo_prom
      g_reg.coduni_n1,          #coduni_n1
      g_reg.nss,                #nss
      g_reg.n_folio,            #n_folio
      g_reg.tipo_solicitud,     #tipo_solicitud
      g_reg.fentcons,           #fentcons
      0,                        #tipo_pago % de anticipo
      hoy,                      #fecha_corte 
      vsalario,                 #salario_base_comis
      l_numsm,                  #num_sm
      g_reg.cod_esq_comision,   #cod_esq_comision
      0,                        #cod_esq_premio
      0,                        #porcent_comision
      0,                        #monto_comision
      "N",                      #comis_pagada
      null,                     #fecha_pago
      vestado_comision,         #estado_comision
      hoy,                      #fecha_calculo
      g_usuario)                #usuario

   IF STATUS < 0 THEN
      DISPLAY "  ERROR ",STATUS 
      DISPLAY g_reg.n_folio
      DISPLAY g_reg.tipo_solicitud
   ELSE
      LET vencontrado = TRUE
   END IF
                                
END FUNCTION

FUNCTION Salario_minimo()

   SELECT monto_sm 
   INTO   l_sm
   FROM   tab_salario_minimo a
   WHERE (a.fecha_hasta_sm>=g_reg.fentcons AND a.fecha_desde_sm<=g_reg.fentcons)
   --AND a.fecha_hasta_sm IS NOT NULL)
   OR    (a.fecha_desde_sm<=g_reg.fentcons AND a.fecha_hasta_sm IS NULL)

   LET l_numsm = 0

   LET l_numsm = g_reg.salario_base_comis / l_sm / 30

END FUNCTION

FUNCTION Inserta_reverso()
   SELECT "g"
   FROM   com_reverso
   WHERE  n_folio        = g_rev.n_folio
   AND    tipo_solicitud = g_rev.tipo_solicitud
   AND    fecha_corte    = g_rev.fecha_corte
   IF SQLCA.SQLCODE = NOTFOUND THEN
      INSERT INTO com_reverso VALUES (g_rev.*)
   END IF
END FUNCTION
