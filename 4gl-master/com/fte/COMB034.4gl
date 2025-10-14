-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => COMB034                                                   --
-- Descripcion  => Calcula comision donde edo_comis in (220,230) de          --
--              => com_comis_detalle de acuerdo a 1ra recaudacion            --
-- Sistema      => COM                                                       --
-- Autor        => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 23 octubre 2004.                                          --
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

   DEFINE AA,num SMALLINT,
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

   DEFINE vfecha_recep_arch DATE,
          vmes_recep        SMALLINT,
          vano_recep        SMALLINT

   DEFINE vult_salario_diario DECIMAL(12,2),
          vsalario            DECIMAL(12,2)

END GLOBALS

MAIN

   CALL STARTLOG("COMB034.log")

   LET hoy = ARG_VAL(1)

   SELECT *,
          USER 
   INTO   g_param.*,
          g_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "com"

   DISPLAY "INICIO COMISION MASA SALARIAL EN RECAUDACION"

   CALL Calculo_afiliacion()

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   com_ctrl_proceso ",
                 "WHERE  proceso_cod = 'COMB034' ",
                " AND    etapa_cod   = 2" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   LET vhora_final = TIME

   LET vmensaje = "COMISION MASA SALARIAL EN RECAUDACION CALCULADA"

   LET cla_upd = "UPDATE com_ctrl_proceso ",
                 "SET    hora_final   = ","'",vhora_final,"'",",",
                 "       resultado    = ","'",vmensaje,"'",
                " WHERE  proceso_cod  = 'COMB034' ",
                " AND    etapa_cod    = 2 ",
                " AND    consecutivo = ",vrow CLIPPED

   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe

   DISPLAY "FIN CALCULO MASA SALARIAL RECAUDACION"

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
   WHERE  estado_comision in (200,220,230)
----   AND    nivel = 1
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

   SELECT monto_sm
   INTO   l_sm
   FROM   tab_salario_minimo a
   WHERE (a.fecha_hasta_sm>=hoy
   AND    a.fecha_desde_sm<=hoy)
   OR    (a.fecha_desde_sm<=hoy 
   AND a.fecha_hasta_sm IS NULL)

   DECLARE cursor_1 CURSOR FOR 
   SELECT a.*,
          b.criterio_cod,
          cuantos
   FROM   com_comis_detalle a,
          com_esq_comis b,
          safre_tmp:cont_afi c
   WHERE  a.estado_comision  in (200,220,230)
   AND    b.cod_esq_comision = a.cod_esq_comision
   AND    a.codven = c.codven
----   AND    a.nivel = 1
   ORDER  BY codven,n_folio

   LET num = 0

   LET vult_salario_diario = 0
   LET vsalario = 0

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

--      IF g_reg.cod_esq_comision = 0 THEN
--         LET l_monto_comision = 0
--         LET vestado_comision_ant = 236 --edocomis no pgda x esq_com=0 en reca
--         CALL Actualiza_estado()
--      ELSE


         IF vcont_afi < vminimo_solicitudes THEN
            LET l_monto_comision = 0
            LET vestado_comision_ant=237 --por no cumplir c/min solci en recauda
            CALL Actualiza_estado()
         ELSE
            LET l_monto_comision = 0

            SELECT MAX(fecha_recepcion) -- obtiene fec recepcion arch bimes
            INTO   vfecha_recep_arch
            FROM   dis_cza_aporte
	    WHERE  MONTH(fecha_recepcion) = vmes_recep
            AND    YEAR(fecha_recepcion)  = vano_recep
   
            SELECT MAX(ult_salario_diario/100)
            INTO   vult_salario_diario
            FROM   dis_det_aporte
            WHERE  n_seguro        = g_reg.nss
            AND    fecha_recepcion = vfecha_recep_arch
  
            IF vult_salario_diario IS NULL THEN
----            IF SQLCA.SQLCODE = NOTFOUND THEN
----            IF STATUS = NOTFOUND THEN
               LET vult_salario_diario = 0
               LET vestado_comision_ant = 240 --no encontro sal en 1ra recauda
               LET l_monto_comision = g_reg.monto_comision 
               CALL Actualiza_estado()
            ELSE
               LET g_reg.num_sm = vult_salario_diario / l_sm

               CALL Cantidad() RETURNING l_monto_comision

               LET l_monto_comision = l_monto_comision * g_reg.num_sm
    
               IF g_reg.num_sm < 6 THEN
                  LET vestado_comision_ant=250 --regant c/numsm<6 en rec
                  LET vestado_comision=260 --c/com 1r reca x numsm<6 en reca
               ELSE
                  LET vestado_comision_ant=270 --regant c/numsm>=6 en rec
                  LET vestado_comision=280 --c/com 1r reca x numsm>=6en reca

--display "vestado_comision ",vestado_comision
--display "g_reg.edo_comis  ",g_reg.estado_comision
--display "g_reg.n_folio    ",g_reg.n_folio
--display "g_reg.nss        ",g_reg.nss
--display "vfecha_recep_arc ",vfecha_recep_arch
--display "tipo_solict      ",g_reg.tipo_solicitud
--display "g_reg.num_sm     ",g_reg.num_sm
--display "ult_salario      ",vult_salario_diario
--display "l_sm             ",l_sm
--display "monto_comision   ",l_monto_comision
--prompt '' for opc
              END IF


               LET vmonto_ajustado = l_monto_comision - g_reg.monto_comision

               CALL Comision_afiliacion()
               CALL Actualiza_estado()

            END IF
         END IF 
--      END IF 

   END FOREACH

END FUNCTION

FUNCTION Comision_afiliacion()

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
         vmonto_ajustado,          #monto_comision
         "N",                      #comis_pagada
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
