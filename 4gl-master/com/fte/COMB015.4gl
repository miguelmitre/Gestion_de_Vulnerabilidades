##############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                          #
#Owner             => E.F.P.                                                 #
#Programa          => COMB015                                                #
#Descripcion       => PROCESO BATCH CALCULA LAS COMISIONES (COMSION_DETALLE) #
#                  => QUE ACTUALIZAN EL SALARIO POR DISPERSION               #
#Sistema           => COM.                                                   #
#By                => GERARDO ALFONSO VEGA PAREDES.                          #
#Fecha             => 21 abril 2002.                                         #
##############################################################################

DATABASE safre_af

GLOBALS
   DEFINE aux_pausa CHAR(1)
   DEFINE hoy       DATE
   DEFINE g_com_parametro RECORD LIKE com_parametro.*
   DEFINE g_usuario CHAR(8)
   DEFINE g_opcion  CHAR(1)
   DEFINE g_desde   DATE
   DEFINE g_hasta   DATE

   DEFINE g_record RECORD
      g_opcion CHAR(1)
   END RECORD

   DEFINE g_reg   RECORD LIKE com_comis_detalle.*

   DEFINE g_reg1  RECORD LIKE com_comis_detalle.*

   DEFINE AA                  SMALLINT,
          l_monto_comision    DECIMAL(12,2),
          vmonto              DECIMAL(12,2),
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

   DEFINE g_param RECORD LIKE com_parametro.*

   DEFINE g_bat RECORD LIKE dis_ctrl_proceso.*

   DEFINE vestado_comision     SMALLINT,
          vestado_comision_ant SMALLINT

   DEFINE vmensaje CHAR(50)

   DEFINE vnss CHAR(11)

   DEFINE vsalario DECIMAL(12,2),
          l_sm     DECIMAL(12,2),
          l_numsm  DECIMAL(12,2)

END GLOBALS

MAIN

   DISPLAY "INICIO CALCULO COMISION DISPERSION" 

   CALL STARTLOG("COMB015.log")

   LET hoy = ARG_VAL(1)

   SELECT *,
          USER 
   INTO   g_param.*,
          g_usuario
   FROM   com_parametro

   LET cla_sel = "SELECT MAX(hora_inicial) ",
                 "FROM   com_ctrl_proceso ",
                 "WHERE  proceso_cod = 'COMB015' ",
                " AND    etapa_cod   = 4" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vhora_max
   CLOSE cur_proceso9

   SELECT *
   INTO   g_bat.*
   FROM   com_ctrl_proceso
   WHERE  com_ctrl_proceso.proceso_cod  = "COMB015"
   AND    com_ctrl_proceso.etapa_cod    = 4
   AND    com_ctrl_proceso.hora_inicial = vhora_max

   CALL Calculo_sin_salario1()

   CALL Comision_resumen()

--   CALL Calculo_sin_salario2()

   LET vhora_final = TIME

   LET cla_upd = "UPDATE com_ctrl_proceso ",
                 "SET    hora_final   = ","'",vhora_final,"'",",",
                 "       resultado    = ","'",vmensaje,"'",
                " WHERE  proceso_cod  = 'COMB015' ",
                " AND    etapa_cod    = 4 ",
                " AND    hora_inicial = ","'",vhora_max,"'" CLIPPED

   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe

   DISPLAY "FIN CALCULO COMISION DISPERSION"

END MAIN

FUNCTION Calculo_sin_salario1()

   CREATE TEMP TABLE cont_afi
      (codvent char(10),
       contafit integer);

   CREATE index cont_afi_1 on cont_afi(codvent);

   DECLARE curcont CURSOR FOR
   SELECT codven,
          COUNT(*)
   FROM   com_comis_detalle
   WHERE  estado_comision = 0
   GROUP  BY 1
   ORDER  BY 1

   FOREACH curcont INTO vcodven,vcontafi
      INSERT INTO cont_afi values(vcodven,vcontafi)
      LET vcont_afi = vcont_afi + 1
   END FOREACH

   IF vcont_afi IS NULL OR vcont_afi = 0 THEN
      DISPLAY "NO EXISTEN DATOS PARA SER PROCESADOS"
      LET vmensaje = "NO EXISTEN DATOS PARA SER CALCULADOS"
      RETURN
   ELSE
      LET vmensaje = "CALCULO COMISION DISPERSION TERMINADO"
   END IF

   DECLARE cursor_1 CURSOR FOR 
   SELECT a.*,
          b.criterio_cod,
          contafit,
          d.n_seguro
   FROM   com_comis_detalle a,
          com_esq_comis     b,
          cont_afi          c,
          afi_mae_afiliado  d
   WHERE  a.estado_comision  = 0
   AND    b.cod_esq_comision = a.cod_esq_comision
   AND    a.codven = c.codvent
   AND    a.n_folio = d.n_folio
   ORDER  BY a.codven,a.n_folio

   FOREACH cursor_1 INTO g_reg.*,vcriterio,vcont_afi,vnss

      CALL Busca_salario()
     
      IF l_numsm > 0 THEN

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
         ELSE
            LET l_monto_comision = 0
   
            CASE vcriterio
               WHEN 1
                  CALL cantidad() RETURNING l_monto_comision
               WHEN 2
               WHEN 3
               OTHERWISE
                  LET l_monto_comision = 0
                  DISPLAY "Criterio No reconocible"
            END CASE
         END IF 

         IF l_monto_comision IS NOT NULL THEN
            CALL Actualiza_estado()
         END IF

      END IF

   END FOREACH

END FUNCTION

FUNCTION Actualiza_estado()
   LET cla_sel = "UPDATE com_comis_detalle ",
                 "SET    num_sm             = ",l_numsm,",",
                       " salario_base_comis = ",vsalario,",",
                       " monto_comision     = ",l_monto_comision,",",
                       " fecha_calculo      = ","'",hoy,"'",",",
                       " estado_comision    = 40 ",
                 "WHERE  n_folio         = ",g_reg.n_folio,
                " AND    tipo_solicitud  = ",g_reg.tipo_solicitud,
                " AND    nivel           = ",g_reg.nivel,
                " AND    codven          = ","'",g_reg.codven,"'",
                " AND    estado_comision = 0 " CLIPPED

         PREPARE claexe FROM cla_sel
         EXECUTE claexe
END FUNCTION

FUNCTION cantidad()
   SELECT monto_comision
   INTO   l_monto_comision
   FROM   com_cuadro_comis
   WHERE  cod_esq_comision = g_reg.cod_esq_comision
   AND    rango_desde     <= l_numsm
   AND    rango_hasta     >= l_numsm

   RETURN l_monto_comision
END FUNCTION

FUNCTION Busca_salario()
   DEFINE vfecha_limite    DATE,
          vfecha_recepcion DATE,
          vfech_pago       CHAR(08),
          vperiodo_pago    CHAR(06)

   LET vfecha_limite = MDY(MONTH(g_reg.fentcons)+4,
                             DAY(g_reg.fentcons),
                            YEAR(g_reg.fentcons)+1)

   SELECT fecha_recepcion,
          max(fech_pago),
          max(periodo_pago)
   INTO   vfecha_recepcion,
          vfech_pago,
          vperiodo_pago
   FROM   dis_det_aporte
   WHERE  n_seguro         = vnss
   AND    fecha_recepcion >= g_reg.fentcons
   AND    fecha_recepcion <= vfecha_limite
   GROUP  BY 1

   IF STATUS = NOTFOUND THEN
      LET cla_sel = "UPDATE com_comis_detalle ",
                    "SET    estado_comision = 1 ",
                    "WHERE  n_folio         = ",g_reg.n_folio,
                   " AND    tipo_solicitud  = ",g_reg.tipo_solicitud,
                   " AND    nivel           = ",g_reg.nivel,
                   " AND    codven          = ","'",g_reg.codven,"'",
                   " AND    estado_comision = 0 " CLIPPED

      PREPARE claexe2 FROM cla_sel
      EXECUTE claexe2

      LET l_numsm = 0

   ELSE

--display "nss          ",vnss
--display "fecha recep  ",vfecha_recepcion
--display "fech pago    ",vfech_pago
--display "periodo pago ",vperiodo_pago

--display "fentcons     ",g_reg.fentcons
--display "f.limite     ",vfecha_limite
--display "status       ",status
--prompt '1' for opc


      SELECT max(ult_salario_diario) / 100
      INTO   vsalario
      FROM   dis_det_aporte
      WHERE  n_seguro        = vnss
      AND    fecha_recepcion = vfecha_recepcion
      AND    fech_pago       = vfech_pago
      AND    periodo_pago    = vperiodo_pago

      SELECT monto_sm
      INTO   l_sm
      FROM   tab_salario_minimo a
      WHERE (a.fecha_hasta_sm>=g_reg.fentcons 
      AND    a.fecha_desde_sm<=g_reg.fentcons)
      --AND a.fecha_hasta_sm IS NOT NULL)
      OR    (a.fecha_desde_sm<=g_reg.fentcons AND a.fecha_hasta_sm IS NULL)
   
      LET l_numsm = 0
   
      LET l_numsm = vsalario / l_sm
   END IF

END FUNCTION

FUNCTION Comision_resumen()

   DEFINE g_reg2 RECORD
      codven         LIKE com_comis_detalle.codven,
      cod_tipo_prom  LIKE com_comis_detalle.cod_tipo_prom,
      coduni_n1      LIKE com_comis_detalle.coduni_n1,
      nivel          LIKE com_comis_detalle.nivel,
      fecha_corte    LIKE com_comis_detalle.fecha_corte,
      monto_comision LIKE com_comis_detalle.monto_comision,
      promedio_sm    LIKE com_comis_detalle.monto_comision,
      feminima       DATE,
      femaxima       DATE,
      num_sm         LIKE com_comis_detalle.num_sm,
      contador       SMALLINT
   END RECORD

   DEFINE AA,num           SMALLINT
   DEFINE l_monto_comision DECIMAL(12,2)
   DEFINE x_nivel	   SMALLINT
   DEFINE x_ind_bono	   SMALLINT,
          x_cod_tabla_ispt SMALLINT,
          vfiscal_cod 	   SMALLINT,
          vmonto_percep	   DECIMAL(12,2),
          cla_sel	   CHAR(300)

   DEFINE entro SMALLINT

   LET entro = FALSE

   DECLARE cursor_2 CURSOR FOR 
   SELECT codven,
          cod_tipo_prom,
          coduni_n1,
          nivel,
          MAX(fecha_corte),
          SUM(monto_comision),
          SUM (num_sm)/COUNT(*),
          MIN(fentcons),
          MAX(fentcons),
          SUM(num_sm),
          COUNT(*)
   FROM   com_comis_detalle
   WHERE  estado_comision = 40  -- Calculado comision sin salario
   GROUP  BY 1,2,3,4
   ORDER  BY 1,2,3,4

   LET num = 0

   FOREACH cursor_2 INTO g_reg2.*

      LET num = num + 1
        
      IF g_reg2.cod_tipo_prom IS NULL THEN
         DISPLAY "PROMOTOR ",g_reg2.codven," SIN TIPO PROMOTOR ASIGNADO"
         LET entro = TRUE
         EXIT FOREACH
      END IF

      SELECT SUM(monto)                        
      INTO   vmonto_percep                       
      FROM   com_percepcion                       
      WHERE  cod_tipo_prom =  g_reg2.cod_tipo_prom
      AND    min_afili     >= g_reg2.contador
      IF vmonto_percep IS NULL THEN            
         LET vmonto_percep = 0                 
      END IF                                   
 
      INSERT INTO com_comis_resumen VALUES  
        (g_reg2.codven,         # codven
         g_reg2.cod_tipo_prom,  # cod_tipo_prom
         g_reg2.coduni_n1,      # coduni_n1
         g_reg2.nivel,          # nivel
         g_reg2.feminima,       # fecha_desde
         g_reg2.femaxima,       # fecha_hasta
         g_reg2.fecha_corte,    # fecha_corte
         g_reg2.contador,       # total_afiliados
         g_reg2.num_sm,         # total_sm
         g_reg2.promedio_sm,    # promedio_sm
         g_reg2.monto_comision, # total_comision
         0,                     # total_premio
         vmonto_percep,         # monto_percep
         0,                     # estado_comision
         hoy,                   # fecha_calculo
         g_usuario,             # usuario
         0)                     # consolida

      LET cla_sel = "UPDATE com_comis_detalle ",
                    "SET    estado_comision = 50,",  -- Agrupado con salario
                    "       fecha_calculo = ","'",hoy,"'",
                    "WHERE  estado_comision = 40 ", 
                    "AND    codven        = ",g_reg2.codven,
                    "AND    coduni_n1     = ",g_reg2.coduni_n1,
                    "AND    cod_tipo_prom = ",g_reg2.cod_tipo_prom

      PREPARE claexe3 FROM cla_sel
      EXECUTE claexe3

   END FOREACH

   IF num = 0 THEN
      LET vmensaje = "NO HAY DATOS PARA SER AGRUPADOS DISPERSION "
      DISPLAY vmensaje
   ELSE
      IF num > 0 AND entro THEN
         LET vmensaje =  "PROMOTOR ",g_reg2.codven," SIN TIPO PROMOTOR ASIGNADO"
         DISPLAY vmensaje
      ELSE
         LET vmensaje = "AGRUPACION PROMOTOR DISPERSION TERMINADO"
----         DISPLAY vmensaje
      END IF 
   END IF

END FUNCTION

