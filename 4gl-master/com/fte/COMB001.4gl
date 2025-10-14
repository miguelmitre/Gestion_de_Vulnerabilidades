------------------------------------------------------------------------------
--Proyecto          => Sistema de Afores. ( MEXICO )                    
--Propietario       => E.F.P.                                           
--Modulo            => COM.                                             
--Programa          => COMB001  
--Descripcion       => PROVISIONA afiliaciones y traspasos  ( DETALLE ) 
--Fecha             => 12 enero 2005.                                   
--Autor             => GERARDO ALFONSO VEGA PAREDES.                    
------------------------------------------------------------------------------
--Modificacion      => (v2) Se agrego funcionalidad con fecha_actualiza_sa
--                  => utilizandola como periodo de pago
--Autor             => GERARDO ALFONSO VEGA PAREDES.
------------------------------------------------------------------------------
--Modificacion      => (v3) Se cambio afi_mae_afiliado por afi_solicitud para
--                  => incluir solicitudes de traspasos que no hayan recibido
--                  => liquidacion.
--Autor             => GERARDO ALFONSO VEGA PAREDES.
------------------------------------------------------------------------------
--Modificacion      => (v4) Se agreo numero interno a com_comis_detalle para 
--                  => manejarlo como numero de nomina, falta utilizarlo 
--Autor             => GERARDO ALFONSO VEGA PAREDES.
------------------------------------------------------------------------------
--Modificacion      => (v5) Se quito tabla com_tipo_promotor del query       
--                  => principal por el cambio en la tabla de 
--                  => com_tipo_promotor el cual consiste en n codigos de tipo
--                  => de acuerdo a multiples rangos de produccion.
--Autor             => GERARDO ALFONSO VEGA PAREDES.
--Fecha             => 11 junio 2005.
------------------------------------------------------------------------------

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
      indicador_ispt     LIKE com_tipo_promotor.indicador_ispt,
      meta_afi1          LIKE com_tipo_promotor.meta_afi1,
      meta_afi2          LIKE com_tipo_promotor.meta_afi2,
      meta_afi3          LIKE com_tipo_promotor.meta_afi3,
      meta_tra1          LIKE com_tipo_promotor.meta_tra1,
      meta_tra2          LIKE com_tipo_promotor.meta_tra2,
      meta_tra3          LIKE com_tipo_promotor.meta_tra3,
      cod_tipo_prom      LIKE com_tipo_promotor.cod_tipo_prom,
      fecha_actualiza_sa LIKE afi_mae_afiliado.fecha_actualiza_sa --v2
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
          l_sm              DECIMAL(12,2),
          l_numsm           DECIMAL(12,2),
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

   DEFINE vperiodo_corte  CHAR(06),
          vperiodo_pago   CHAR(06),
          vperiodo_limite CHAR(06)

END GLOBALS

MAIN
   DISPLAY "INICIO CALCULO COMISION ANTICIPO" 

   CALL STARTLOG("COMB001.log")

   CALL inicializa()


   LET hoy = ARG_VAL(1)
   LET vperiodo_limite = ARG_VAL(2)

LET hoy = "05/30/2005" -- OJO
LET vperiodo_limite = "200410"   --OJO

   CALL Provision()

   LET ejecuta = "nohup time fglgo COMB002.4gi ",hoy," &"   --prog calcula comi
-----   RUN ejecuta  OJO

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
                      "b.agenc_cod,",
                      "a.n_seguro,",
                      "a.n_folio,",
                      "a.tipo_solicitud,",
                      "a.salario_base_comis,",
                      "a.salario_actual,",
                      "a.fentcons,",
                      "0,",     --d.indicadro_comision--v5
                      "b.nivel,",
                      "0,",     --d.indicador_ispt    --v5
                      "0,",     --d.meta_afi1         --v5
                      "0,",     --d.meta_afi2         --v5
                      "0,",     --d.meta_afi3         --v5
                      "0,",     --d.meta_tra1         --v5
                      "0,",     --d.meta_tra2         --v5
                      "0,",     --d.meta_tra3         --v5
                      "0,",     --d.cod_tipo_prom     --v5
                      "a.fecha_actualiza_sa ",             --v2
               "FROM   afi_solicitud  a,",                 --v3
                      "pro_mae_promotor  b ",
                      --"com_tipo_promotr d ",             --v5
              " WHERE  a.fentcons           <= ","'",hoy,"'",
              " AND    a.cod_promotor        = b.cod_promotor ",
              " AND    a.tipo_solicitud in (1,2) ",
              " AND    a.indicador_comision = 0 ",                 --v3
              " AND    b.nivel              > 0 ",
              --" AND    b.nivel            = d.cod_tipo_prom ",   --v5
              " AND    a.status_interno in (70,75,100) ",          --v3
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

   LET vindicador_comision = 10  --estado para afi_mae_afiliado

   LET cla_sel="UPDATE afi_mae_afiliado ",
               "SET    indicador_comision = ",vindicador_comision,
              " WHERE  n_folio        = ",g_reg.n_folio,
              " AND    tipo_solicitud = ",g_reg.tipo_solicitud CLIPPED
   PREPARE claexeupd1 FROM cla_sel
   EXECUTE claexeupd1

   LET cla_sel="UPDATE afi_solicitud ",
               "SET    indicador_comision = ",vindicador_comision,
              " WHERE  n_folio        = ",g_reg.n_folio,
              " AND    tipo_solicitud = ",g_reg.tipo_solicitud CLIPPED
   PREPARE claexeupd2 FROM cla_sel
   EXECUTE claexeupd2

END FUNCTION

FUNCTION Provisiona()

   DEFINE vsalario     DECIMAL(12,2),
          vsalario_his DECIMAL(12,2)

   SELECT monto_sm
   INTO   l_sm
   FROM   tab_salario_minimo a
   WHERE (a.fecha_desde_sm<=g_reg.fecha_actualiza_sa AND 
          a.fecha_hasta_sm>=g_reg.fecha_actualiza_sa)
   OR    (a.fecha_desde_sm<=g_reg.fecha_actualiza_sa AND 
          a.fecha_hasta_sm IS NULL)

   LET l_NUMSM = 0

   IF g_reg.salario_actual IS NULL OR g_reg.salario_actual = 0 THEN
      LET vsalario = g_reg.salario_base_comis
   ELSE
      LET vsalario = g_reg.salario_actual
   END IF

   IF vsalario IS NULL THEN
      LET vsalario = 0
   END IF

   LET vsalario_his = vsalario

   LET l_NUMSM = vsalario / l_SM  -- numero de salarios

   IF l_numsm > 25 THEN
      LET l_numsm = 25
      LET vsalario = l_numsm * l_sm 
   END IF

   IF vsalario = 0 OR  vsalario IS NULL THEN
      LET vestado_comision = 0   -- estado = sin salario diario provisionado
   ELSE
      LET vestado_comision = 2   -- estado = con salario diario provisionado

      -- valida periodo pago de afiliacion contra periodo de fecha de corte ----
      IF g_reg.fecha_actualiza_sa IS NULL OR 
         g_reg.fecha_actualiza_sa = " " THEN 
         LET vestado_comision = 1  -- estado = sin pago como x periodo_pago nulo
      ELSE
         IF vestado_comision = 2 THEN
            ---CALL Periodo(hoy) RETURNING vperiodo_corte
            ---CALL Periodo_limite(vperiodo_corte) RETURNING vperiodo_limite
            CALL Periodo(g_reg.fecha_actualiza_sa) RETURNING vperiodo_pago
      
            IF vperiodo_pago >= vperiodo_limite THEN
               LET vestado_comision = 2 --estado = pago comision
            ELSE
               LET vestado_comision = 3 --estado = no pago comis x perpgo<perlim
            END IF
         END IF
      END IF
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
      vsalario_his,             #salario_historico--porcent_comis
      0,                        #monto_comision
      0,                        #comis_pagada
      g_reg.fecha_actualiza_sa, #fecha_pago     --se guarda fecha periodo pago
      vestado_comision,         #estado_comision
      hoy,                      #fecha_calculo
      g_usuario,                #usuario
      "0")                      #num_interno   --v4

   IF STATUS < 0 THEN
      DISPLAY "  ERROR ",STATUS 
      DISPLAY g_reg.n_folio
      DISPLAY g_reg.tipo_solicitud
   ELSE
      LET vencontrado = TRUE
   END IF
                                
END FUNCTION

FUNCTION Periodo(fecha)

   DEFINE fecha    DATE,
          vano     CHAR(04),
          vmes     CHAR(02),
          vperiodo CHAR(06)

   LET vano = YEAR(fecha)
   LET vmes = MONTH(fecha)

   CASE
     WHEN vmes = "1"
        LET vmes = 2
     WHEN vmes = "3"
        LET vmes = 4
     WHEN vmes = "5"
        LET vmes = 6
     WHEN vmes = "7"
        LET vmes = 8
     WHEN vmes = "9"
        LET vmes = 10
     WHEN vmes = "11"
        LET vmes = 12
   END CASE

   LET vperiodo = vano,vmes USING "&&"

   RETURN vperiodo
END FUNCTION

FUNCTION Periodo_limite(periodo)
   DEFINE periodo  CHAR(06),
          vmes     CHAR(02),
          vano     CHAR(04),
          vperiodo CHAR(06),
          cont     SMALLINT

   LET cont = 0
   LET vmes = periodo[5,6]
   LET vano = periodo[1,4]

   WHILE TRUE

      LET vmes = vmes - 2
      IF vmes = 0 THEN
         LET vmes = "12"
         LET vano = vano - 1
      END IF

      LET cont = cont + 1

      IF cont = 2 THEN
         EXIT WHILE
      END IF

   END WHILE

   LET vperiodo = vano,vmes USING "&&"

   RETURN vperiodo

END FUNCTION

FUNCTION Inserta_reverso()
   INSERT INTO com_reverso VALUES (g_rev.*)
END FUNCTION
