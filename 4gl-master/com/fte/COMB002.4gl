-------------------------------------------------------------------------------
--Proyecto          => Sistema de Afores. ( MEXICO )                          #
--Propietario       => E.F.P.                                                 #
--Modulo            => COM.                                                   #
--Programa          => COMB002                                                #
--Descripcion       => PROCESO BATCH CALCULA LAS COMISIONES (COMSION_DETALLE) #
--                  => A AFILIACIONES QUE TIENEN SALARIO                      #
--Fecha             => 12 enero 2005.                                         #
--Autor             => GERARDO ALFONSO VEGA PAREDES                           #
-------------------------------------------------------------------------------
--Modificacion      => (v2) Se quito division vtot_sum = vsuma_salario / l_sm
--                  => ya que la vtot_sm = vsuma_slario el cual se suma desde
--                  => el cursor secundario con la columna num_sm.
--                  => ademas se agreogo codven al query de actualizacion de
--                  => comision; funcion Comision_afiliacion()
--Autor             => GERARDO ALFONSO VEGA PAREDES.
------------------------------------------------------------------------------
--Modificacion      => (v3) Se inicializao la variable vcomision=0 ya que 
--                  => cuando esta era nula traia el valor del registro 
--                  => anterior
--Autor             => GERARDO ALFONSO VEGA PAREDES.
------------------------------------------------------------------------------
--Modificacion      => (v4) Se agreo numero interno a com_comis_detalle para
--                  => manejarlo como numero de nomina, falta utilizarlo
--Autor             => GERARDO ALFONSO VEGA PAREDES.
------------------------------------------------------------------------------
--Modificacion      => (v5) 
--                  => 
--Autor             => GERARDO ALFONSO VEGA PAREDES.
------------------------------------------------------------------------------


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
          vcriterio	         SMALLINT,
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

   CALL STARTLOG("COMB002.log")

   LET hoy = ARG_VAL(1)

   CALL Inicializa()

   CALL Calculo_afiliacion()

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   com_ctrl_proceso ",
                 "WHERE  proceso_cod = 'COMB001' ",
                " AND    etapa_cod   = 1" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   LET hora_final = TIME

   LET vmensaje = "COMISION ANTICIPO FINALIZADA"

   LET cla_upd = "UPDATE com_ctrl_proceso ",
                 "SET    hora_final   = ","'",hora_final,"'",",",
                 "       resultado    = ","'",vmensaje,"'",
                " WHERE  proceso_cod  = 'COMB001' ",
                " AND    etapa_cod    = 1 ",
                " AND    consecutivo = ",vrow CLIPPED

   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe

   DISPLAY "FIN CALCULO COMISION ANTICIPO"

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
          SUM(num_sm)
          --SUM(salario_base_comis) --v2
   FROM   com_comis_detalle
   WHERE  estado_comision = 2
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
   WHERE  a.estado_comision  = 2
   AND    b.cod_esq_comision = a.cod_esq_comision
   AND    a.codven = c.codven  
   ORDER  BY codven,n_folio    

   FOREACH cursor_1 INTO g_reg.*,vcriterio,vcont_afi,vsuma_salario

      LET l_monto_comision = 0
      LET vmonto_comision = 0

      IF g_reg.cod_esq_comision = 0 THEN
         LET l_monto_comision = 0
         LET vmonto_comision = 0
         LET vestado_comision = 4     --edo comis no pag por esq_com = 0
      ELSE

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
            LET vmonto_comision = 0
            LET vcomision = 0   --v3
            LET vestado_comision = 6  --edo comis no pag por no cumplir min sol
         ELSE
   
            LET l_monto_comision = 0
            LET vmonto_comision = 0
            LET vcomision = 0   --v3
   
            CASE vcriterio
               WHEN 1
-----                  CALL Porcentaje() RETURNING l_monto_comision
               WHEN 2
                  CALL Cantidad() RETURNING l_monto_comision
               WHEN 3
               OTHERWISE
                  LET l_monto_comision = 0
                  DISPLAY "Criterio No reconocible"
            END CASE

            LET vmonto_comision = g_reg.salario_base_comis *   --100 %
                                 (l_monto_comision/100) * 30.4

            LET vcomision = g_reg.salario_base_comis * 
                           (l_monto_comision/100)*(vanticipo/100) * 30.4 --80 %

            LET l_monto_comision = vcomision

            --edo_com=esq_com, se iguala para saber con que esq_com se
            --pago en registro para pagar la recaudacion

            LET vestado_comision = vcod_esq_comision 

         END IF 
      END IF

      IF l_monto_comision IS NOT NULL THEN
         CALL Comision_afiliacion()
      END IF

   END FOREACH

END FUNCTION

FUNCTION Obtiene_esquema_comision()

--   SELECT monto_sm                            --v5
--   INTO   l_sm                                --v5
--   FROM   tab_salario_minimo a                --v5
--   WHERE  a.fecha_hasta_sm IS NULL            --v5

--   LET vtot_sm = vsuma_salario / l_sm  --v2

   LET vtot_sm = vsuma_salario           --v2


   --- se obtiene esq com de acuerdo al tipo_promotor y el rango produccion

   SELECT indicador_comision                     --v5
   INTO   vcod_esq_comision                      --v5
   FROM   com_tipo_promotor                      --v5
   WHERE  cod_tipo_prom = g_reg.cod_tipo_prom    --v5
   AND    prod_hasta   >= vtot_sum               --v5
   AND    prod_desde   <= vtot_sum               --v5

   --- obtiene esquema de acuerdo a la suma de salario--
--   SELECT cod_esq_comision,                    --v5
   SELECT anticipo                               --v5
--   INTO   vcod_esq_comision,                   --v5
   INTO   vanticipo                              --v5
   FROM   com_esq_comis
--   WHERE  prod_desde <= vtot_sm                --v5
--   AND    prod_hasta >= vtot_sm                --v5
--   AND    cod_esq_comision > 0                 --v5
   WHERE  cod_esq_comision = vcod_esq_comision   --v5

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

FUNCTION Comision_afiliacion()

   LET cla_sel = "UPDATE com_comis_detalle ",
                 "SET    monto_comision = ",l_monto_comision,",",
                       " comis_pagada   = ",vmonto_comision,",",
                       " fecha_calculo = ","'",hoy,"'",",",
                       " cod_esq_comision= ",vcod_esq_comision,",",
                       " estado_comision = ",vestado_comision,
                " WHERE  n_folio         = ",g_reg.n_folio,
                " AND    tipo_solicitud  = ",g_reg.tipo_solicitud,
                " AND    nivel           = ",g_reg.nivel,
                " AND    codven     = ","'",g_reg.codven,"'",   --v2
                " AND    estado_comision = 2 " CLIPPED

   PREPARE claexe FROM cla_sel
   EXECUTE claexe

   IF vestado_comision >= 10 THEN
      CALL Inserta_com_respon()     --comision gerencia y subdireccion
   END IF

END FUNCTION

FUNCTION Inserta_com_respon()

   DEFINE vnum_sm  DECIMAL(6,2),
          vnum_afi INTEGER,
          vcoduni2 CHAR(10),
          vmonto_comision2 DECIMAL(12,2),
          vmonto_comision3 DECIMAL(12,2),
          l_monto_comision2 DECIMAL(12,2),
          l_monto_comision3 DECIMAL(12,2)

   SELECT dat.cod_resp_uni,
          niv1.coduni_n1,
          tab.cod_puesto,
          tab.cod_esq_comision,
          niv2.coduni_n2
   INTO   g_reg.codven,
          g_reg.coduni_n1,
          g_reg.cod_tipo_prom,
          g_reg.cod_esq_comision,
          vcoduni2
   FROM   com_nivel1        niv1,
          com_nivel2        niv2,
          com_dat_uni_com   dat,
          com_respon_unidad res,
          tab_puesto        tab
   WHERE  niv1.coduni_n1       = g_reg.coduni_n1
   AND    niv1.uni_superior_n1 = niv2.coduni_n2
   AND    dat.cod_uni          = niv1.coduni_n1
   AND    dat.nivel            = 1
   AND    res.cod_resp_uni     = dat.cod_resp_uni
   AND    res.puesto_resp      = tab.cod_puesto

   IF STATUS <> NOTFOUND THEN  --status de si encontro registro
      LET l_monto_comision  = 0
      LET l_monto_comision2 = 0
      LET l_monto_comision3 = 0
      LET vmonto_comision2  = 0
      LET vmonto_comision3  = 0

      LET vcod_esq_comision = g_reg.cod_esq_comision  --esq gerencia

      CALL cantidad() RETURNING l_monto_comision

      LET l_monto_comision2 = vcomision * (l_monto_comision / 100)      --80%
      LET vmonto_comision2 = vmonto_comision * (l_monto_comision / 100) --100%

      INSERT INTO com_comis_detalle VALUES(
         g_reg.codven,             #codven
         2,                        #nivel               --nivel gerente
         g_reg.cod_tipo_prom,      #cod_tipo_prom
         g_reg.coduni_n1,          #coduni_n1
         g_reg.nss,                #nss
         g_reg.n_folio,            #n_folio
         g_reg.tipo_solicitud,     #tipo_solicitud
         g_reg.fentcons,           #fentcons
         0,                        #tipo_pago % de anticipo
         hoy,                      #fecha_corte
         g_reg.salario_base_comis, #salario_base_comis
         g_reg.num_sm,             #num_sm
         g_reg.cod_esq_comision,   #cod_esq_comision
         0,                        #cod_esq_premio
         0,                        #porcent_comision
         l_monto_comision2,        #monto_comision
         vmonto_comision2,         #comis_pagada
         null,                     #fecha_pago
         vestado_comision,         #estado_comision
         hoy,                      #fecha_calculo
         g_usuario,                #usuario
         "0")                      #num_interno    --v4

         ---- INGRESA SUBDIRECTOR ----
         SELECT dat.cod_resp_uni,
                niv2.coduni_n2,
                tab.cod_puesto,
                tab.cod_esq_comision
         INTO   g_reg.codven,
                g_reg.coduni_n1,
                g_reg.cod_tipo_prom,
                g_reg.cod_esq_comision
         FROM   com_nivel2        niv2,
                com_dat_uni_com   dat,
                com_respon_unidad res,
                tab_puesto        tab
         WHERE  niv2.coduni_n2       = vcoduni2
         AND    dat.cod_uni          = niv2.coduni_n2
         AND    dat.nivel            = 2
         AND    res.cod_resp_uni     = dat.cod_resp_uni
         AND    res.puesto_resp      = tab.cod_puesto

         IF STATUS <> NOTFOUND THEN  --status de si encontro registro
            LET l_monto_comision = 0
      
            LET vcod_esq_comision = g_reg.cod_esq_comision  --esq subdireccion
      
            CALL cantidad() RETURNING l_monto_comision

            LET l_monto_comision3=vcomision * (l_monto_comision / 100)    --80%
            LET vmonto_comision3=vmonto_comision *(l_monto_comision / 100)--100%

            INSERT INTO com_comis_detalle VALUES(
               g_reg.codven,             #codven
               3,                        #nivel           --nivel subdirector
               g_reg.cod_tipo_prom,      #cod_tipo_prom
               g_reg.coduni_n1,          #coduni_n1
               g_reg.nss,                #nss
               g_reg.n_folio,            #n_folio
               g_reg.tipo_solicitud,     #tipo_solicitud
               g_reg.fentcons,           #fentcons
               0,                        #tipo_pago % de anticipo
               hoy,                      #fecha_corte
               g_reg.salario_base_comis, #salario_base_comis
               g_reg.num_sm,             #num_sm
               g_reg.cod_esq_comision,   #cod_esq_comision
               0,                        #cod_esq_premio
               0,                        #porcent_comision
               l_monto_comision3,        #monto_comision
               vmonto_comision3,         #comis_pagada
               null,                     #fecha_pago
               vestado_comision,         #estado_comision
               hoy,                      #fecha_calculo
               g_usuario,                #usuario
               "0")                        #num_interno   --v4
         ELSE
            INSERT INTO com_comis_detalle VALUES(
               g_reg.codven,             #codven
               3,                        #nivel
               g_reg.cod_tipo_prom,      #cod_tipo_prom
               g_reg.coduni_n1,          #coduni_n1
               g_reg.nss,                #nss
               g_reg.n_folio,            #n_folio
               g_reg.tipo_solicitud,     #tipo_solicitud
               g_reg.fentcons,           #fentcons
               0,                        #tipo_pago % de anticipo
               hoy,                      #fecha_corte
               g_reg.salario_base_comis, #salario_base_comis
               g_reg.num_sm,             #num_sm
               0,                        #cod_esq_comision
               0,                        #cod_esq_premio
               0,                        #porcent_comision
               0,                        #monto_comision
               0,                        #comis_pagada
               null,                     #fecha_pago
               32,                       #estado_comision   -- no encotro respon
               hoy,                      #fecha_calculo
               g_usuario,                #usuario
               "0")                        #num_interno    --v4
         END IF
   ELSE
      INSERT INTO com_comis_detalle VALUES(
         g_reg.codven,             #codven
         2,                        #nivel
         g_reg.cod_tipo_prom,      #cod_tipo_prom
         g_reg.coduni_n1,          #coduni_n1
         g_reg.nss,                #nss
         g_reg.n_folio,            #n_folio
         g_reg.tipo_solicitud,     #tipo_solicitud
         g_reg.fentcons,           #fentcons
         0,                        #tipo_pago % de anticipo
         hoy,                      #fecha_corte
         g_reg.salario_base_comis, #salario_base_comis
         g_reg.num_sm,             #num_sm
         0,                        #cod_esq_comision
         0,                        #cod_esq_premio
         0,                        #porcent_comision
         0,                        #monto_comision
         0,                        #comis_pagada
         null,                     #fecha_pago
         32,                       #estado_comision   -- no encotro respon
         hoy,                      #fecha_calculo
         g_usuario,                #usuario
         "0")                      #num_interno   --v4

   END IF
   LET g_reg.codven           = NULL
   LET g_reg.coduni_n1        = NULL
   LET g_reg.cod_tipo_prom    = NULL
   LET g_reg.cod_esq_comision = NULL
   LET vcoduni2               = NULL
END FUNCTION
