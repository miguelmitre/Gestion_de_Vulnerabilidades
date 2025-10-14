#*************************************************************************
#*******
#******* PROGRAMA : CTAB118.4gl
#*******
#******* OBJETIVO : 
#*******           
#******* ELABORO  : Claudia U.
#******* FECHA    : 20-SEPTIEMBRE-2005
#******* MODIFICO   Josue Lisandro Huerta Sierra
#******* FECHA    : 1 DE JULIO DE 2008 
#******* Ordenes de Selección Multisiefores
#*************************************************************************


DATABASE safre_af


DEFINE v_fecha_operacion      ,
       v_hoy    DATE
DEFINE v_hoy_exacto  DATETIME YEAR TO SECOND

DEFINE v_ruta_envio  CHAR(100) ,
       v_archivo     CHAR(150) ,
       v_usuario     CHAR(10)  ,
       v_hora        CHAR(08)

DEFINE  reg_ordselsie RECORD
        nss             CHAR(11), 
        curp            CHAR(18),
        rfc             CHAR(13),
        tipo_reg_cza    CHAR(02), 
        id_operacion    CHAR(02), 
        fecha_recep_ord DATE    ,
        hora_recep_ord  CHAR(08),
        cve_confirma_op CHAR(10),
        cod_result      CHAR(02),
        motivo_procede  CHAR(03),
        fecha_ejecuta   DATE    ,
        hora_ejecuta    CHAR(08),
        tipo_reg_det    CHAR(02),
        medio_recep     CHAR(02),
        cve_subcta      CHAR(02),
        tipo_movimiento CHAR(02),
        siefore_ced     CHAR(08),
        siefore_rec     CHAR(08),
        no_acc_compra   DECIMAL(18,6),
        no_acc_venta    DECIMAL(18,6) ,
        precio_compra   DECIMAL(18,6) ,
        precio_venta    DECIMAL(18,6) ,
        importe_asoc    DECIMAL(18,2) ,
        fecha_operacion DATE          ,
        tipo_reg_sum    CHAR(02)      ,
        total_registros INTEGER
END RECORD

DEFINE reg_cza_ordselsie RECORD LIKE cta_cza_sie_acep.*

DEFINE reg_sum_ordselsie RECORD LIKE cta_sum_sie_acep.*

DEFINE reg_mto RECORD
       no_acc_compra  DECIMAL(18,6),
       no_acc_venta   DECIMAL(18,6),
       precio_compra  DECIMAL(18,6),
       precio_venta   DECIMAL(18,6),
       importe_asoc   DECIMAL(18,6)
       END RECORD


MAIN
    CALL STARTLOG("CTAB118.log")
    CALL fn_inicio()
    IF fn_lee_datos () THEN
       CALL fn_obtiene_mtos()
       CALL fn_genera_archivo()
       CALL limpia_blancos()
       ERROR "ARCHIVO GENERADO :",v_archivo ATTRIBUTE(NORMAL)
       SLEEP 6
       ERROR ""
    END IF
END MAIN

FUNCTION fn_inicio()

DEFINE v_act_mto ,
       v_lee_nss CHAR(300),
       v_hoy_c   CHAR(12 )


    SELECT ruta_envio,
           USER      
      INTO v_ruta_envio,
           v_usuario   
      FROM seg_modulo
     WHERE modulo_cod = "cta"

     LET v_hoy  = TODAY
     LET v_hora = TIME
     LET v_hora = v_hora[1,2],v_hora[4,5],v_hora[7,8]

     LET v_archivo = v_ruta_envio CLIPPED,"/",v_usuario CLIPPED,
                    ".ORSELSIE_ACEP.",v_hoy USING "YYMMDD","-",v_hora CLIPPED


     LET v_act_mto = "UPDATE cta_det_sie_acep   ",
                     "   SET no_acc_compra = ? ,",
                     "       no_acc_venta  = ? ,",
                     "       importe_asoc  = ? ,",
                     "       precio_compra = ? ,",
                     "       precio_venta  = ?  ",
                     " WHERE CURRENT OF c_act_mto "

     LET v_lee_nss = " SELECT c.cve_subcta, c.siefore_ced, siefore_rec ",
                     "   FROM cta_det_sie_acep c",
                     "  WHERE c.fecha_operacion = ? ",
                     "    AND c.nss =  ? ",
                     "    AND c.cve_subcta = ? "

     PREPARE eje_act_mto FROM v_act_mto
     PREPARE eje_lee_nss FROM v_lee_nss

END FUNCTION


FUNCTION fn_lee_datos ()

DEFINE  v_status          SMALLINT
DEFINE  v_respuesta       CHAR(1)

LET v_status = TRUE

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTAB1181" ATTRIBUTE(BORDER)
   DISPLAY " CTAB118    GENERA ARCHIVO ORDENES DE SELECCION ACEPTADAS                      " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY v_hoy USING "dd-mm-yyyy" AT 3,62 ATTRIBUTE(REVERSE)

   INPUT BY NAME v_fecha_operacion,
                 v_respuesta

         BEFORE FIELD v_fecha_operacion 
                IF fn_valida_fecha(0) THEN
                   DISPLAY BY NAME v_fecha_operacion
                ELSE
                   NEXT FIELD v_fecha_operacion
                END IF

         AFTER FIELD v_fecha_operacion
               IF v_fecha_operacion IS NULL THEN
                  ERROR "Debe indicar Fecha de la Operacion"
                  SLEEP 3
                  ERROR ""
                  NEXT FIELD v_fecha_operacion
               ELSE
                  IF fn_valida_fecha(1) THEN
                     NEXT FIELD v_respuesta
                  ELSE
                     NEXT FIELD v_fecha_operacion
                  END IF
               END IF
                  
         AFTER FIELD v_respuesta
               IF v_respuesta NOT MATCHES "[SN]" THEN
                  CLEAR FORM
                  NEXT FIELD v_respuesta
               ELSE
                  IF v_respuesta = "N" THEN
                     LET v_status = FALSE
                     ERROR "PROCESO CANCELADO."
                     SLEEP 2
                     EXIT INPUT
                  ELSE
                     EXIT INPUT
                  END IF
               END IF

         ON KEY (INTERRUPT)
            LET v_status = FALSE
            LET int_flag = FALSE
            EXIT INPUT

         ON KEY (CONTROL-C)
            LET v_status = FALSE
            LET int_flag = FALSE
            EXIT INPUT

  END INPUT

RETURN v_status

END FUNCTION

FUNCTION fn_valida_fecha(v_tipo_valida)

DEFINE  l_fecha_operacion DATE
DEFINE  v_status          ,
        v_tipo_valida     SMALLINT

LET v_status = TRUE

---   Verifica que haya datos 

IF v_tipo_valida = 0 THEN

    SELECT MAX(fecha_operacion)
      INTO l_fecha_operacion
      FROM cta_cza_sie_acep
ELSE
    SELECT MAX(fecha_operacion)
      INTO l_fecha_operacion
      FROM cta_cza_sie_acep
     WHERE fecha_operacion = v_fecha_operacion

END IF

IF l_fecha_operacion IS NULL OR
   l_fecha_operacion = "12/31/1899" THEN
   ERROR " NO EXISTEN DATOS PARA GENERAR ARCHIVO."
   SLEEP 3
   ERROR ""
   LET v_status = FALSE
ELSE
   LET v_fecha_operacion = l_fecha_operacion
END IF

RETURN v_status

END FUNCTION

FUNCTION fn_obtiene_mtos()

DEFINE reg_lee_mto RECORD
       nss               CHAR(11),
       grupo_regimen     SMALLINT,
       tipo_movimiento   SMALLINT,
       folio             INTEGER ,
       siefore           SMALLINT,
       fecha_conversion  DATE    ,
       monto_en_acciones DECIMAL(16,6),
       monto_en_pesos    DECIMAL(16,6) 
END RECORD

DEFINE v_comando CHAR(100)

 
SELECT unique nss ,
       cve_subcta ,
       folio_solicitud       
  FROM cta_det_sie_acep       
 WHERE fecha_operacion = v_fecha_operacion
  INTO TEMP tmp_det_sie_acep       

CREATE INDEX tmp_det_sie_acep_1 ON tmp_det_sie_acep(nss,folio_solicitud)

UPDATE STATISTICS FOR TABLE tmp_det_sie_acep


DECLARE c_lee_mtos CURSOR FOR

   SELECT s.nss              ,
          s.cve_subcta       ,
          d.tipo_movimiento  ,
          d.folio            ,
          d.siefore          ,
          NVL(d.fecha_conversion, today) ,
          NVL(SUM(d.monto_en_acciones),0),
          NVL(SUM(d.monto_en_pesos),0)
     FROM tmp_det_sie_acep s,
          OUTER (dis_cuenta    d,
          tes_solicitud t)
    WHERE d.nss             = t.nss
      AND d.tipo_movimiento IN (1,210)
      AND t.folio_solicitud = s.folio_solicitud
      AND t.folio           = d.folio
      AND d.subcuenta IN  (SELECT a.subcuenta
                             FROM tab_agrupa_subcta_regimen a
                            WHERE a.grupo_regimen =  t.grupo_regimen)
      AND s.cve_subcta = t.grupo_regimen
    GROUP BY 1,2,3,4,5,6
    ORDER BY 1,2

   START REPORT r_actualiza_mtos TO "salida.mto"

         FOREACH c_lee_mtos INTO reg_lee_mto.*
            OUTPUT TO REPORT r_actualiza_mtos ( reg_lee_mto.* )
         END FOREACH 

   FINISH REPORT r_actualiza_mtos

   LET v_comando = " chmod 777 salida.mto"
   RUN v_comando

END FUNCTION

REPORT r_actualiza_mtos( l_reg_lee_mto )

DEFINE  l_reg_lee_mto        RECORD
        nss                   CHAR(11),
        grupo_regimen         SMALLINT,
        tipo_movimiento       SMALLINT,
        folio                 INTEGER ,
        siefore               SMALLINT,
        fecha_conversion      DATE    ,
        acciones              DECIMAL(16,6),
        pesos                 DECIMAL(16,6)
        END RECORD

DEFINE  v_cve_subcta          CHAR(02)
DEFINE  siefore_ced           CHAR(08)
DEFINE  siefore_rec           CHAR(08)
DEFINE  v_precio_compra       DECIMAL(12,6)
DEFINE  v_precio_venta        DECIMAL(12,6)
DEFINE  vfecha_liquida        DATE
DEFINE  diatmp                DATE

OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 40

ORDER EXTERNAL BY l_reg_lee_mto.nss

FORMAT

BEFORE GROUP OF l_reg_lee_mto.nss
       LET reg_mto.no_acc_compra = 0
       LET reg_mto.no_acc_venta  = 0
       LET reg_mto.precio_compra = 0
       LET reg_mto.precio_venta  = 0
       LET reg_mto.importe_asoc  = 0

ON EVERY ROW

          IF l_reg_lee_mto.tipo_movimiento = 1 THEN
              LET reg_mto.no_acc_compra = l_reg_lee_mto.acciones
              LET reg_mto.importe_asoc  = l_reg_lee_mto.pesos
          ELSE
              LET reg_mto.no_acc_venta  = l_reg_lee_mto.acciones
          END IF

       DECLARE c_act_mto  CURSOR
                 FOR eje_lee_nss 
         FOREACH c_act_mto USING v_fecha_operacion ,
                                 l_reg_lee_mto.nss ,
                                 l_reg_lee_mto.grupo_regimen
                            INTO v_cve_subcta,
                                 siefore_ced ,
                                 siefore_rec


            IF l_reg_lee_mto.tipo_movimiento = 1 OR 
               l_reg_lee_mto.tipo_movimiento = 210 THEN
{                EXECUTE eje_act_mto USING reg_mto.no_acc_compra ,
                                          reg_mto.no_acc_venta  ,
                                          reg_mto.importe_asoc  ,
                                          v_precio_compra       ,
                                          v_precio_venta
}
            #PRECIO DE ACCIONES PARA LAS SUBCUENTAS QUE TUVIERON
            # LIQUIDACION DE TRASPASO ENTRE SIEFORES
                SELECT r.precio_del_dia,
                       c.precio_del_dia
                  INTO v_precio_compra,
                       v_precio_venta
                  FROM glo_valor_accion c,
                       glo_valor_accion r,
                       tab_siefore_local tc,
                       tab_siefore_local tr
                 WHERE tc.razon_social   = siefore_ced
                   AND tc.codigo_siefore = c.codigo_siefore
                   AND c.fecha_valuacion = l_reg_lee_mto.fecha_conversion
                   AND tr.razon_social   = siefore_rec
                   AND tr.codigo_siefore = r.codigo_siefore
                   AND r.fecha_valuacion = l_reg_lee_mto.fecha_conversion

                PRINT l_reg_lee_mto.acciones
                PRINT l_reg_lee_mto.pesos
                PRINT l_reg_lee_mto.grupo_regimen

                IF l_reg_lee_mto.tipo_movimiento = 1 THEN
                ###ACTUALIZA DATOS DE LA COMPRA DE ACCIONES
                     UPDATE cta_det_sie_acep
                        SET no_acc_compra = l_reg_lee_mto.acciones,
                            precio_compra = v_precio_compra,
                            importe_asoc  = l_reg_lee_mto.pesos
                      WHERE nss = l_reg_lee_mto.nss
                        AND cve_subcta = l_reg_lee_mto.grupo_regimen
                        AND fecha_operacion = v_fecha_operacion
                ELSE
                ###ACTUALIZA LA INFORMACION DE LA VENTA DE ACCIONES
                     UPDATE cta_det_sie_acep
                        SET no_acc_venta  = l_reg_lee_mto.acciones,
                            precio_venta  = v_precio_venta
                      WHERE nss = l_reg_lee_mto.nss
                        AND cve_subcta = l_reg_lee_mto.grupo_regimen
                        AND fecha_operacion = v_fecha_operacion
                END IF 
            ELSE
             #OBTENER EL DIA HABIL SIGUIENTE DE LA FECHA DE OPERACION
             # QUE ES LA FECHA DE LIQUIDACION
                LET diatmp = v_fecha_operacion + 1
                CALL habil_siguiente(diatmp) RETURNING vfecha_liquida

            #PRECIO DE ACCIONES PARA LAS SUBCUENTAS QUE NO TUVIERON
            # LIQUIDACION DE TRASPASO ENTRE SIEFORES
                SELECT r.precio_del_dia,
                       c.precio_del_dia
                  INTO v_precio_compra,
                       v_precio_venta
                  FROM glo_valor_accion c,
                       glo_valor_accion r,
                       tab_siefore_local tc,
                       tab_siefore_local tr
                 WHERE tc.razon_social   = siefore_ced
                   AND tc.codigo_siefore = c.codigo_siefore
                   AND c.fecha_valuacion = vfecha_liquida
                   AND tr.razon_social   = siefore_rec
                   AND tr.codigo_siefore = r.codigo_siefore
                   AND r.fecha_valuacion = vfecha_liquida

                UPDATE cta_det_sie_acep
                   SET no_acc_compra = 0,
                       no_acc_venta  = 0,
                       precio_compra = v_precio_compra,
                       precio_venta  = v_precio_venta,
                       importe_asoc  = 0
                 WHERE nss = l_reg_lee_mto.nss
                   AND cve_subcta = l_reg_lee_mto.grupo_regimen
                   AND fecha_operacion = v_fecha_operacion
            END IF
         END FOREACH
END REPORT

FUNCTION fn_genera_archivo()

DECLARE c_orselsie CURSOR FOR
   
   SELECT cza.nss              ,
          cza.curp             ,
          cza.rfc              ,
          cza.tipo_registro    ,
          cza.id_operacion     ,
          cza.fecha_recep_ord  ,
          cza.hora_recep_ord   ,
          cza.cve_confirma_op  ,
          cza.cod_result       ,
          cza.motivo_procede   ,
          cza.fecha_ejecuta    ,
          cza.hora_ejecuta     ,
          det.tipo_registro    ,
          det.medio_recep      ,
          det.cve_subcta       ,
          det.tipo_movimiento  ,
          det.siefore_ced      ,
          det.siefore_rec      ,
          det.no_acc_compra    ,
          det.no_acc_venta     ,
          det.precio_compra    ,
          det.precio_venta     ,
          det.importe_asoc     ,
          det.fecha_operacion  ,
          sumario.tipo_registro,
          sumario.total_registros     
     FROM cta_cza_afil_acep  cza,
          cta_det_sie_acep   det,
          cta_sum_afil_acep  sumario
    WHERE cza.nss             = det.nss
      AND sumario.nss         = cza.nss
      AND cza.fecha_operacion = det.fecha_operacion
      AND sumario.fecha_operacion = cza.fecha_operacion
      AND det.fecha_operacion = v_fecha_operacion
    ORDER BY 1,15

START REPORT rep_ord_sel_sie TO v_archivo

FOREACH c_orselsie INTO reg_ordselsie.*

   OUTPUT TO REPORT rep_ord_sel_sie ( reg_ordselsie.*)

END FOREACH

CLOSE c_orselsie

FINISH  REPORT rep_ord_sel_sie 

END FUNCTION


REPORT rep_ord_sel_sie ( reg_ordselsie )

DEFINE  reg_ordselsie RECORD
        nss             CHAR(11), 
        curp            CHAR(18),
        rfc             CHAR(13),
        tipo_reg_cza    CHAR(02), 
        id_operacion    CHAR(02), 
        fecha_recep_ord DATE    ,
        hora_recep_ord  CHAR(08),
        cve_confirma_op CHAR(10),
        cod_result      CHAR(02),
        motivo_procede  CHAR(03),
        fecha_ejecuta   DATE    ,
        hora_ejecuta    CHAR(08),
        tipo_reg_det    CHAR(02),
        medio_recep     CHAR(02),
        cve_subcta      CHAR(02),
        tipo_movimiento CHAR(02),
        siefore_ced     CHAR(08),
        siefore_rec     CHAR(08),
        no_acc_compra   DECIMAL(18,6),
        no_acc_venta    DECIMAL(18,6) ,
        precio_compra   DECIMAL(18,6) ,
        precio_venta    DECIMAL(18,6) ,
        importe_asoc    DECIMAL(18,2) ,
        fecha_operacion DATE          ,
        tipo_reg_sum    CHAR(02)      ,
        total_registros INTEGER
END RECORD


OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 3

ORDER EXTERNAL BY reg_ordselsie.nss

FORMAT
   FIRST PAGE HEADER
         SELECT cza.*
           INTO reg_cza_ordselsie.*
           FROM cta_cza_sie_acep cza
          WHERE fecha_operacion = v_fecha_operacion

         PRINT COLUMN 1, reg_cza_ordselsie.tipo_registro  ,
               COLUMN 3, reg_cza_ordselsie.id_servicio    ,
               COLUMN 5, reg_cza_ordselsie.id_operacion   ,
               COLUMN 7, reg_cza_ordselsie.tipo_ent_dest  ,
               COLUMN 9, reg_cza_ordselsie.cve_ent_dest   ,
               COLUMN 12,reg_cza_ordselsie.tipo_ent_origen, 
               COLUMN 14,reg_cza_ordselsie.cve_ent_origen , 
               COLUMN 17,v_hoy                             USING "YYYYMMDD",
               COLUMN 25,reg_cza_ordselsie.consecutivo     USING "&&&"     , 
               COLUMN 28,reg_cza_ordselsie.cve_mod_recep  ,
               COLUMN 30,reg_cza_ordselsie.cod_result     , 
               COLUMN 38,reg_cza_ordselsie.diagnostico1   , 
               COLUMN 46,reg_cza_ordselsie.diagnostico2   , 
               COLUMN 54,reg_cza_ordselsie.diagnostico3   , 
               COLUMN 62, 89 SPACES 

BEFORE GROUP OF reg_ordselsie.nss
         PRINT COLUMN 1, reg_ordselsie.tipo_reg_cza       ,
               COLUMN 3, reg_ordselsie.id_operacion       ,
               COLUMN 5, reg_ordselsie.nss                ,
               COLUMN 16,reg_ordselsie.curp               ,
               COLUMN 34,reg_ordselsie.rfc                ,
               COLUMN 47,reg_ordselsie.fecha_recep_ord     USING "YYYYMMDD", 
               COLUMN 55,reg_ordselsie.hora_recep_ord[1,2],
                         reg_ordselsie.hora_recep_ord[4,5],
                         reg_ordselsie.hora_recep_ord[7,8],
               COLUMN 61,reg_ordselsie.cve_confirma_op    ,
               COLUMN 71,reg_ordselsie.cod_result         ,
               COLUMN 73,reg_ordselsie.motivo_procede     ,
               COLUMN 76,reg_ordselsie.fecha_ejecuta       USING "YYYYMMDD",
               COLUMN 84,reg_ordselsie.hora_ejecuta[1,2],
                         reg_ordselsie.hora_ejecuta[4,5], 
                         reg_ordselsie.hora_ejecuta[7,8], 
               COLUMN 90,61 SPACES 

ON EVERY ROW
         PRINT COLUMN 1, reg_ordselsie.tipo_reg_det       ,
               COLUMN 3, reg_ordselsie.nss                ,
               COLUMN 14,reg_ordselsie.curp               ,
               COLUMN 32,reg_ordselsie.medio_recep        ,
               COLUMN 34,reg_ordselsie.cve_subcta         ,
               COLUMN 36,reg_ordselsie.tipo_movimiento    ,
               COLUMN 38, 8 SPACES                        ,
               COLUMN 46,reg_ordselsie.siefore_rec        , 
               COLUMN 54,reg_ordselsie.no_acc_compra *1000000 
                         USING "&&&&&&&&&&&&&&&"      ,
               COLUMN 69,reg_ordselsie.no_acc_venta *(-1000000 )
                         USING "&&&&&&&&&&&&&&&"      ,
               COLUMN 84,reg_ordselsie.precio_compra *1000000 
                         USING "&&&&&&&&&&&&&&&"      ,
               COLUMN 99,reg_ordselsie.precio_venta *1000000 
                         USING "&&&&&&&&&&&&&&&"      ,
               COLUMN 114,reg_ordselsie.importe_asoc*100
                         USING "&&&&&&&&&&&&&&&"      ,
               COLUMN 129,22 SPACES 

AFTER GROUP OF reg_ordselsie.nss
         PRINT COLUMN 1, reg_ordselsie.tipo_reg_sum       ,
               COLUMN 3, v_hoy                            USING "YYYYMMDD",
               COLUMN 11,reg_ordselsie.total_registros    USING "&&&&&&&&&&&&&&&",
               COLUMN 26,125 SPACES
ON LAST ROW
         SELECT sumario.*
           INTO reg_sum_ordselsie.*
           FROM cta_sum_sie_acep sumario
          WHERE fecha_operacion = v_fecha_operacion

         PRINT COLUMN 1, reg_sum_ordselsie.tipo_registro  ,
               COLUMN 3, reg_sum_ordselsie.id_servicio    ,
               COLUMN 5, reg_sum_ordselsie.id_operacion   ,
               COLUMN 7, reg_sum_ordselsie.tipo_ent_dest  ,
               COLUMN 9, reg_sum_ordselsie.cve_ent_dest   ,
               COLUMN 12,reg_sum_ordselsie.tipo_ent_origen, 
               COLUMN 14,reg_sum_ordselsie.cve_ent_origen , 
               COLUMN 17,v_hoy                             USING "YYYYMMDD",
               COLUMN 25,reg_sum_ordselsie.total_registros USING "&&&&&&&&&&&&&&&",
               COLUMN 40, 111 SPACES 
END REPORT


FUNCTION limpia_blancos()

DEFINE v_comando CHAR(500)

    LET v_comando = "cd " ,v_ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ",v_archivo CLIPPED,
               " > ",v_usuario CLIPPED,".limpia"
    RUN v_comando

    LET v_comando = "cd ",v_ruta_envio CLIPPED,
               "; mv ",v_usuario CLIPPED,".limpia ",v_archivo 
    RUN v_comando

END FUNCTION

FUNCTION habil_siguiente(diaActual)

    DEFINE
        diaTmp      DATE,
        contador    SMALLINT,
        diaActual   DATE

    DEFINE
        diaHabilSig DATE,
        diaSemana   SMALLINT,
        feriado     SMALLINT,
        finSemana   SMALLINT

    LET diaHabilSig = diaActual

    WHILE TRUE
        LET feriado   = 0
        LET finSemana = 0
        LET diaSemana = WEEKDAY(diaHabilSig)

        IF diaSemana = 0 OR diaSemana = 6 THEN
            LET finSemana = 1
        END IF

        SELECT *
        FROM   tab_feriado 
        WHERE  feria_fecha = diaHabilSig

        IF STATUS <> NOTFOUND THEN
            LET feriado = 1
        END IF

        IF feriado = 1 OR finSemana = 1 THEN
            LET diaHabilSig = diaHabilSig + 1 UNITS DAY
        ELSE
            EXIT WHILE
        END IF
    END WHILE

    RETURN diaHabilSig

END FUNCTION

