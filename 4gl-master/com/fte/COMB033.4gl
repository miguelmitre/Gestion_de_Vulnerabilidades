-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => COMB033                                                   --
-- Descripcion  => PROGRAMA QUE CALCULA COMISION NUEVA EN com_comis_detalle  --
-- Sistema      => COM                                                       --
-- Por          => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 12 octubre 2004.                                          --
-- Modificado   =>                                                           --
-- Fecha        =>                                                           --
-------------------------------------------------------------------------------

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
          hola                integer,
          cla_sel             CHAR(900),
          opc                 CHAR(01),
          vcont_afi           INTEGER,
          vcont_afi2          INTEGER,
          vminimo_solicitudes SMALLINT,
          vmonto_comision     DECIMAL(12,2),
          vpago               DECIMAL(12,2)

   DEFINE hora_inicial CHAR(8),
          hora_final   CHAR(8),
          vhora_max    CHAR(8),
          vhora_final  CHAR(8),
          cla_upd      CHAR(450)

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE g_param_com RECORD LIKE com_parametro.*

   DEFINE g_bat RECORD LIKE dis_ctrl_proceso.*

   DEFINE vestado_comision     SMALLINT,
          vestado_comision_ant SMALLINT

   DEFINE vmensaje CHAR(50)

   DEFINE vcomision DECIMAL(12,2)

   DEFINE vanticipo DECIMAL(6,2)

   DEFINE vporcentaje DECIMAL(6,2)

   DEFINE vrow INTEGER

   DEFINE ejecuta CHAR(200)

END GLOBALS

MAIN

   CALL STARTLOG("COMB033.log")

   LET hoy = ARG_VAL(1)

   SELECT *,
          USER 
   INTO   g_param.*,
          g_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "com"

   SELECT *
   INTO   g_param_com.*
   FROM   com_parametro

   CALL Calculo_afiliacion()

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   com_ctrl_proceso ",
                 "WHERE  proceso_cod = 'COMB032' ",
                " AND    etapa_cod   = 1" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   LET vhora_final = TIME

   LET vmensaje = "COMISION MASA SALARIAL REGISTRO CALCULADA"

   LET cla_upd = "UPDATE com_ctrl_proceso ",
                 "SET    hora_final   = ","'",vhora_final,"'",",",
                 "       resultado    = ","'",vmensaje,"'",
                " WHERE  proceso_cod  = 'COMB032' ",
                " AND    etapa_cod    = 1 ",
                " AND    consecutivo = ",vrow CLIPPED

   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe

   DISPLAY "FIN CALCULO MASA SALARIAL REGISTRO"

END MAIN

FUNCTION Calculo_afiliacion()

   CREATE TEMP TABLE cont_afi
      (codven char(10),
       cuantos integer);

   CREATE index cont_afi_1 on cont_afi(codven);

   DECLARE curcont CURSOR FOR
   SELECT codven,
          COUNT(*)
   FROM   com_comis_detalle
   WHERE  estado_comision = 210 --toma registros provisionados
   GROUP  BY 1
   ORDER  BY 1

   FOREACH curcont INTO vcodven,vcontafi
      INSERT INTO cont_afi values(vcodven,vcontafi)
      LET vcont_afi = vcont_afi + 1
   END FOREACH

   DECLARE cursor_1 CURSOR FOR 
   SELECT a.*,
          b.criterio_cod,
          cuantos
   FROM   com_comis_detalle a,
          com_esq_comis b,
          cont_afi c
   WHERE  a.estado_comision  = 210 --toma registros provisionados
   AND    b.cod_esq_comision = a.cod_esq_comision
   AND    a.codven = c.codven
   ORDER  BY codven,n_folio

   LET num = 0

   FOREACH cursor_1 INTO g_reg.*,vcriterio,vcont_afi

      LET num = num + 1

      SELECT minimo_requerido
      INTO   vminimo_solicitudes
      FROM   com_esq_comis 
      WHERE  cod_esq_comision = g_reg.cod_esq_comision

      IF STATUS = NOTFOUND THEN
         DISPLAY "No existe Esquma Comision ",
                  g_reg.cod_esq_comision," para el Promotor ",g_reg.codven
         EXIT PROGRAM
      END IF 

      IF vcont_afi < vminimo_solicitudes THEN
         LET l_monto_comision = 0
         LET vestado_comision = 205 --por no cumplir con minimo sol en registro
         CALL Comision_afiliacion()
      ELSE
         LET l_monto_comision = 0

         CASE vcriterio
            WHEN 1
               CALL Porcentaje() RETURNING l_monto_comision
            WHEN 2
               CALL Cantidad() RETURNING l_monto_comision
            WHEN 3
            OTHERWISE
               LET l_monto_comision = 0
               DISPLAY "Criterio No reconocible"
         END CASE

         IF g_reg.cod_esq_comision = 0 THEN
            LET vestado_comision = 215 --edo comis no pgda x esq_com=0 en regis
         ELSE

            LET l_monto_comision = l_monto_comision * g_reg.num_sm

            IF g_reg.num_sm < 6 THEN
               LET vestado_comision = 220 --edo comis pgda x num_sm<6 en cert 
            ELSE
               LET vestado_comision = 230 --edo comis pgda x num_sm>=6 en cert 
            END IF
         END IF  

         CALL Comision_afiliacion()
      END IF 

   END FOREACH

END FUNCTION

FUNCTION Comision_afiliacion()

   LET cla_sel = "UPDATE com_comis_detalle ",
                 "SET    monto_comision = ",l_monto_comision,",",
                       " fecha_calculo = ","'",hoy,"'",",",
                       " estado_comision = ",vestado_comision,
                " WHERE  n_folio         = ",g_reg.n_folio,
                " AND    tipo_solicitud  = ",g_reg.tipo_solicitud,
                " AND    nivel           = ",g_reg.nivel,
                " AND    codven          = ","'",g_reg.codven,"'",
                " AND    estado_comision = 210 " CLIPPED

   PREPARE claexe FROM cla_sel
   EXECUTE claexe

   IF vestado_comision <> 215 THEN
      CALL Inserta_com_direccion()
      CALL Inserta_com_respon()
   END IF

END FUNCTION

FUNCTION Porcentaje()

   SELECT monto_comision
   INTO   vporcentaje
   FROM   com_cuadro_comis
   WHERE  cod_esq_comision = g_reg.cod_esq_comision
   AND    rango_desde     <= g_reg.num_sm
   AND    rango_hasta     >= g_reg.num_sm

   LET l_monto_comision  = g_reg.salario_base_comis * 
                           vporcentaje / 100

   RETURN l_monto_comision
END FUNCTION

FUNCTION cantidad()

   SELECT monto_comision
   INTO   l_monto_comision
   FROM   com_cuadro_comis
   WHERE  cod_esq_comision = g_reg.cod_esq_comision
   AND    rango_desde     <= g_reg.num_sm
   AND    rango_hasta     >= g_reg.num_sm

   RETURN l_monto_comision
END FUNCTION

FUNCTION Inserta_com_direccion()

   DEFINE vcodven_ori           CHAR(10),
          vcoduni_n1_ori        CHAR(10),
          vcod_tipo_prom_ori    SMALLINT,
          vcod_esq_comision_ori SMALLINT

   LET vcodven_ori           = g_reg.codven
   LET vcoduni_n1_ori        = g_reg.coduni_n1
   LET vcod_tipo_prom_ori    = g_reg.cod_tipo_prom
   LET vcod_esq_comision_ori = g_reg.cod_esq_comision

   LET g_reg.codven           = "246"
   LET g_reg.coduni_n1        = "01"
   LET g_reg.cod_tipo_prom    = 10 
   LET g_reg.cod_esq_comision = 110

   CALL cantidad() RETURNING l_monto_comision


   LET l_monto_comision = l_monto_comision * g_reg.num_sm
            
   INSERT INTO com_comis_detalle VALUES(
      g_reg.codven,             #codven
      4,                        #nivel
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
      l_monto_comision,         #monto_comision
      "N",                      #comis_pagada
      null,                     #fecha_pago
      vestado_comision,         #estado_comision
      hoy,                      #fecha_calculo
      g_usuario)                #usuario

   LET g_reg.codven           = vcodven_ori
   LET g_reg.coduni_n1        = vcoduni_n1_ori
   LET g_reg.cod_tipo_prom    = vcod_tipo_prom_ori
   LET g_reg.cod_esq_comision = vcod_esq_comision_ori
      
END FUNCTION

FUNCTION Inserta_com_respon()

   DEFINE vnum_sm  DECIMAL(6,2),
          vnum_afi INTEGER
   DEFINE xsql smallint

   SELECT dat.cod_resp_uni,
          niv.coduni_n1,
          tab.cod_puesto,
          tab.cod_esq_comision
   INTO   g_reg.codven,
          g_reg.coduni_n1,
          g_reg.cod_tipo_prom,
          g_reg.cod_esq_comision 
   FROM   com_nivel1        niv,
          com_dat_uni_com   dat,
          com_respon_unidad res,
          tab_puesto        tab
   WHERE  niv.coduni_n1    = g_reg.coduni_n1
   AND    dat.cod_uni      = niv.coduni_n1
   AND    dat.nivel        = 1
   AND    res.cod_resp_uni = dat.cod_resp_uni
   AND    res.puesto_resp  = tab.cod_puesto

   IF SQLCA.SQLCODE <> NOTFOUND THEN --estado de si encontro
      IF g_reg.num_sm >= 0 THEN

         CALL cantidad() RETURNING l_monto_comision

         LET l_monto_comision = l_monto_comision * g_reg.num_sm
      
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
            g_reg.cod_esq_comision,   #cod_esq_comision
            0,                        #cod_esq_premio
            0,                        #porcent_comision
            l_monto_comision,         #monto_comision
            "N",                      #comis_pagada
            null,                     #fecha_pago
            vestado_comision,         #estado_comision
            hoy,                      #fecha_calculo
            g_usuario)                #usuario
      ELSE

         LET vestado_comision = 235 -- Estado no pagado a jefes por no cumplir
                                    -- con 1 salarios minimos monto_comision=0
         LET l_monto_comision = 0

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
            g_reg.cod_esq_comision,   #cod_esq_comision
            0,                        #cod_esq_premio
            0,                        #porcent_comision
            l_monto_comision,         #monto_comision
            "N",                      #comis_pagada
            null,                     #fecha_pago
            vestado_comision,         #estado_comision
            hoy,                      #fecha_calculo
            g_usuario)                #usuario

      END IF
   END IF
END FUNCTION

