-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => COMB027                                                   --
-- Descripcion  => Calcula comision TIPO C de comision tipo b en 1ra rec     --
-- Sistema      => COM                                                       --
-- Autor        => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 6 agosto 2004.                                           --
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

   DEFINE AA,num SMALLINT,
          l_monto_comision,
          vmonto              DECIMAL(12,2),
          l_sm                DECIMAL(12,2),
          vcont,vcontafi      INTEGER,
          vcriterio           SMALLINT,
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

   DEFINE g_bat RECORD LIKE dis_ctrl_proceso.*

   DEFINE vestado_comision     SMALLINT,
          vestado_comision_ant SMALLINT,
          vestado_comision_ori SMALLINT

   DEFINE vmensaje CHAR(50)

   DEFINE vcomision DECIMAL(12,2)

   DEFINE vanticipo DECIMAL(6,2)

   DEFINE vporcentaje DECIMAL(6,2)

   DEFINE vrow INTEGER

   DEFINE ejecuta CHAR(200)

   DEFINE vfecha_recep_arch DATE,
          vmes_recep        SMALLINT,
          vano_recep        SMALLINT

   DEFINE vult_salario_diario DECIMAL(12,2),
          vsalario            DECIMAL(12,2)

END GLOBALS

MAIN

   CALL STARTLOG("COMB027.log")

   LET hoy = ARG_VAL(1)
   SELECT *,
          USER
   INTO   g_param.*,
          g_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "com"

   DISPLAY "INICIO COMISION TIPO C"

   CALL Calculo_afiliacion()

   LET ejecuta = "nohup time fglgo COMB028.4gi ",hoy," &"
   RUN ejecuta

END MAIN

FUNCTION Calculo_afiliacion()

   WHENEVER ERROR CONTINUE
   DATABASE safre_tmp
   DROP TABLE cont_afi
   CREATE TABLE cont_afi
      (codven char(10),
       cuantos integer);
   CREATE index cont_afi_1 on cont_afi(codven);
   DATABASE safre_af
   WHENEVER ERROR STOP

   DECLARE curcont CURSOR FOR
   SELECT codven,
          COUNT(*)
   FROM   com_comis_detalle
   WHERE  estado_comision = 70
   AND    nivel = 1
   GROUP  BY 1
   ORDER  BY 1

   FOREACH curcont INTO vcodven,vcontafi
      INSERT INTO safre_tmp:cont_afi values(vcodven,vcontafi)
      LET vcont_afi = vcont_afi + 1
   END FOREACH

   SELECT MONTH(fecha_recep_arch),
          YEAR(fecha_recep_arch)
   INTO   vmes_recep,
          vano_recep
   FROM   com_calendario
   WHERE  MONTH(fecha_recep_arch) = MONTH(hoy)
   AND    YEAR(fecha_recep_arch) = YEAR(hoy)

   DECLARE cursor_1 CURSOR FOR
   SELECT a.*,
          b.criterio_cod,
          cuantos
   FROM   com_comis_detalle a,
          com_esq_comis b,
          safre_tmp:cont_afi c    --cambio solicitado por J.Mendez 3-6-04
   WHERE  a.estado_comision  = 70 --se agrego estado cero p/pagar en recau
   AND    b.cod_esq_comision = a.cod_esq_comision
   AND    a.codven = c.codven
   AND    a.nivel = 1
   ORDER  BY codven,n_folio

   LET num = 0

   LET vult_salario_diario = 0
   LET vsalario = 0

   FOREACH cursor_1 INTO g_reg.*,vcriterio,vcont_afi

      LET num = num + 1

      LET vestado_comision_ori = g_reg.estado_comision

      LET g_reg.cod_esq_comision = 30 --se cambia a 30 para que tome
                                      --esquema tipo C bimestral

      SELECT minimo_requerido
      INTO   vminimo_solicitudes
      FROM   com_esq_comis
      WHERE  cod_esq_comision = g_reg.cod_esq_comision -- esquema tipo c >=8
                                                       -- num_sm pago bimestral

      IF STATUS = NOTFOUND THEN
         DISPLAY "No existe Esquma Comision ",
                  g_reg.cod_esq_comision," para el Promotor ",g_reg.codven
         EXIT PROGRAM
      END IF

      IF vcont_afi < vminimo_solicitudes THEN
         LET l_monto_comision = 0
      ELSE
         LET l_monto_comision = 0

         SELECT MAX(fecha_recepcion) -- obtiene fecha recepcion arch bimestral
         INTO   vfecha_recep_arch
         FROM   dis_cza_aporte
         WHERE  MONTH(fecha_recepcion) = vmes_recep
         AND    YEAR(fecha_recepcion) = vano_recep

         SELECT MAX(ult_salario_diario/100)
         INTO   vult_salario_diario
         FROM   dis_det_aporte
         WHERE  n_seguro        = g_reg.nss
         AND    fecha_recepcion = vfecha_recep_arch

         IF STATUS = NOTFOUND THEN
            LET vult_salario_diario = 0
            LET vestado_comision = 105 --s/c TC bim num_sm<=8 no encont 1ra rec
            LET l_monto_comision = 0   --en espera arch bim 2da rec
         ELSE
            SELECT monto_sm
            INTO   l_sm
            FROM   tab_salario_minimo a
            WHERE (a.fecha_hasta_sm>=g_reg.fentcons
            AND    a.fecha_desde_sm<=g_reg.fentcons)
            OR    (a.fecha_desde_sm<=g_reg.fentcons
            AND a.fecha_hasta_sm IS NULL)

            LET g_reg.num_sm = vult_salario_diario / l_sm

            LET vestado_comision = 85 --c/c TB bim negativa numsm < 8 1ra rec
            LET l_monto_comision = g_reg.monto_comision * -1
            CALL Comision_afiliacion()

            LET vestado_comision = 95 -- c/c TC bim numsm<=8 en 1ra rec
            CALL Cantidad() RETURNING l_monto_comision
            CALL Comision_afiliacion()

         END IF
      END IF

   END FOREACH

END FUNCTION

FUNCTION Comision_afiliacion()

   LET cla_sel = "UPDATE com_comis_detalle ",
                 "SET    fecha_calculo = ","'",hoy,"'",",",
                       " estado_comision = ",vestado_comision,
                " WHERE  n_folio         = ",g_reg.n_folio,
                " AND    tipo_solicitud  = ",g_reg.tipo_solicitud,
                " AND    nivel           = ",g_reg.nivel,
                " AND    codven          = ","'",g_reg.codven,"'",
                " AND    estado_comision  = 70 " CLIPPED

   PREPARE claexe FROM cla_sel
   EXECUTE claexe

   IF vestado_comision <> 100 THEN
      INSERT INTO com_comis_detalle VALUES(
         g_reg.codven,             #codven
         g_reg.nivel,              #nivel
         g_reg.cod_tipo_prom,      #cod_tipo_prom
         g_reg.coduni_n1,          #coduni_n1
         g_reg.nss,                #nss
         g_reg.n_folio,            #n_folio
         g_reg.tipo_solicitud,     #tipo_solicitud
         g_reg.fentcons,           #fentcons
         0,                        #tipo_pago % de anticipo
         hoy,                      #fecha_corte
         vult_salario_diario,      #salario_base_comis
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

