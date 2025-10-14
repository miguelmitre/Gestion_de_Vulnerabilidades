-------------------------------------------------------------------------------
-- Proyecto     => AFORE                                                     --
-- Propietario  => E.F.P.                                                    --
-- Modulo       => COM                                                       --
-- Programa     => COMB003                                                   --
-- Descripcion  => Calcula comision donde edo_comis in (10,20,30) de         --
--              => com_comis_detalle de acuerdo a 1ra recaudacion            --
-- Autor        => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 16 enero 2005.                                            --
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

   DEFINE AA SMALLINT,
          l_monto_comision,
          vmonto_ajustado,
          vmonto              DECIMAL(12,2),
          l_sm                DECIMAL(12,2),
          vcont,vcontafi      INTEGER,
          vcriterio	         SMALLINT,
          vcodven             CHAR(10),
          hola                integer,
          cla_sel             CHAR(900),
          opc                 CHAR(01),
          vcont_afi           INTEGER,
          vcont_afi2          INTEGER,
          vminimo_solicitudes SMALLINT,
          vfecha_corte        DATE,
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

   DEFINE vfolio     INTEGER,
          vmes_recep SMALLINT,
          vano_recep SMALLINT

   DEFINE vult_salario_diario DECIMAL(12,2),
          vsalario            DECIMAL(12,2)

END GLOBALS

MAIN

   CALL STARTLOG("COMB003.log")

   LET hoy = ARG_VAL(1)

   SELECT *,
          USER 
   INTO   g_param.*,
          g_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "com"

   DISPLAY "INICIO CALCULO RELIQUIDACION"

   CALL Reliquidacion()

   LET ejecuta = "nohup time fglgo COMB004.4gi ",hoy," &" --prog calc com reliq
   RUN ejecuta

END MAIN

FUNCTION Reliquidacion()

   SELECT MONTH(fecha_recep_arch),
          YEAR(fecha_recep_arch)
   INTO   vmes_recep,
          vano_recep
   FROM   com_calendario
   WHERE  MONTH(fecha_recep_arch) = MONTH(hoy)
   AND    YEAR(fecha_recep_arch) = YEAR(hoy)

   SELECT MAX(folio)       -- obtiene fol arch bimes
   INTO   vfolio
   FROM   dis_cza_aporte
	WHERE  MONTH(fecha_recepcion) = vmes_recep
   AND    YEAR(fecha_recepcion)  = vano_recep

   IF vfolio IS NOT NULL THEN
      LET vfolio = 0
   END IF

   SELECT monto_sm
   INTO   l_sm
   FROM   tab_salario_minimo a
   WHERE  a.fecha_hasta_sm IS NULL

   DECLARE cursor_1 CURSOR FOR 
   SELECT *
   FROM   com_comis_detalle
   WHERE  estado_comision  in (10,20,30)
----   AND nivel = 1
   ORDER  BY codven,n_folio

   LET vult_salario_diario = 0
   LET vsalario = 0

   FOREACH cursor_1 INTO g_reg.*

      SELECT MAX(ult_salario_diario)/100
      INTO   vult_salario_diario
      FROM   dis_det_aporte
      WHERE  folio    = vfolio
      AND    n_seguro = g_reg.nss
     
      IF vult_salario_diario IS NULL THEN
         LET vult_salario_diario = 0
         LET l_monto_comision = 0
         LET vestado_comision_ant = 34 --act reg s/1ra reca y com=0
         CALL Actualiza_estado()
      ELSE
         LET g_reg.num_sm = vult_salario_diario / l_sm
         LET l_monto_comision = 0
         LET vestado_comision_ant = 40 --act reg ori c/1ra reca y com<>0
         LET vestado_comision    = 50 --ins reg nvo c/1ra reca y com<>0
         CALL Actualiza_estado()
         CALL Inserta_reg_nuevo()
      END IF
  
   END FOREACH

END FUNCTION

FUNCTION Inserta_reg_nuevo()

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
         0,                        #comis_pagada
         null,                     #fecha_pago
         vestado_comision,         #estado_comision
         hoy,                      #fecha_calculo
         g_usuario)                #usuario

END FUNCTION

FUNCTION Actualiza_estado()

   LET cla_sel = "UPDATE com_comis_detalle ",
                 "SET    estado_comision = ",vestado_comision_ant,
                " WHERE  n_folio         = ",g_reg.n_folio,
                " AND    tipo_solicitud  = ",g_reg.tipo_solicitud,
                " AND    nivel           = ",g_reg.nivel,
                " AND    codven          = ","'",g_reg.codven,"'",
                " AND    estado_comision = ",g_reg.estado_comision

   PREPARE claexe FROM cla_sel
   EXECUTE claexe

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
