################################################################################
#Proyecto          => SISTEMA DE AFORE.( MEXICO )                              #
#MOdulo            => COM 					               #
#Programa          => COML002                                                  #
#Desscripcion      => GENERA EL ARCHIVO DE PRODUCCION HASTA LA FECHA           #
#By                => LUIS ENRIQUE AVILA GUZMAN        	 		       #
#Fecha             => 16 DE JUNIO DE 2003                                      #
#Autor             => GERARDO ALFONSO VEGA PAREDES                             #
#Fecha modificacion=> 12 agosto 2003.                                          #
#Desc cambio       => tipo_pago controla el estado de cada afiliacion          #
#Fecha modificacion=> 9 septiembre 2003.                                       #
#Desc cambio       => Incluir cambio de promotor en este listado               #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
        fecha_hasta           DATE 
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        NOM_ARCHIVO           CHAR(200) ,
        seg_usuario           CHAR(008) ,
        RUTA                  CHAR(200) ,
        G_LISTA               CHAR(200) ,
        enter                 CHAR(001)

    DEFINE #loc #integer
        cont_de_registros     INTEGER

    DEFINE  reg_prod          RECORD
        agenc_cod             LIKE pro_mae_promotor.agenc_cod,
        codven                LIKE pro_mae_promotor.codven,
        cod_promotor          LIKE afi_mae_afiliado.cod_promotor,
        nombres_pro           LIKE pro_mae_promotor.nombres,
        paterno_pro           LIKE pro_mae_promotor.paterno,
        materno_pro           LIKE pro_mae_promotor.materno,
        n_seguro              LIKE afi_mae_afiliado.n_seguro,
        n_folio               LIKE afi_mae_afiliado.n_folio,
        finicta               LIKE afi_mae_afiliado.finicta,
        tipo_solicitud        LIKE afi_mae_afiliado.tipo_solicitud,
        nombres               LIKE afi_mae_afiliado.nombres,
        paterno               LIKE afi_mae_afiliado.paterno,
        materno               LIKE afi_mae_afiliado.materno,
        salario_actual        LIKE afi_mae_afiliado.salario_actual,
        fecha_recepcion       LIKE dis_det_aporte.fecha_recepcion,
        ultima_fecha          LIKE cta_ctr_cuenta.fecha_ult_general,
        saldo                 LIKE dis_cuenta.monto_en_pesos,
        comision              DECIMAL(12,2)
    END RECORD

    DEFINE  g_reg ARRAY[1000] OF RECORD
        agenc_cod             LIKE pro_mae_promotor.agenc_cod,
        codven                LIKE pro_mae_promotor.codven,
        cod_promotor          LIKE afi_mae_afiliado.cod_promotor,
        nombres_pro           LIKE pro_mae_promotor.nombres,
        paterno_pro           LIKE pro_mae_promotor.paterno,
        materno_pro           LIKE pro_mae_promotor.materno,
        n_seguro              LIKE afi_mae_afiliado.n_seguro,
        n_folio               LIKE afi_mae_afiliado.n_folio,
        finicta               LIKE afi_mae_afiliado.finicta,
        tipo_solicitud        LIKE afi_mae_afiliado.tipo_solicitud,
        nombres               LIKE afi_mae_afiliado.nombres,
        paterno               LIKE afi_mae_afiliado.paterno,
        materno               LIKE afi_mae_afiliado.materno,
        salario_actual        LIKE afi_mae_afiliado.salario_actual,
        fecha_recepcion       LIKE dis_det_aporte.fecha_recepcion,
        ultima_fecha          LIKE cta_ctr_cuenta.fecha_ult_general,
        saldo                 LIKE dis_cuenta.monto_en_pesos,
        comision              DECIMAL(12,2)
    END RECORD

   DEFINE r_pro RECORD
      agenc_cod       LIKE pro_mae_promotor.agenc_cod,
      codven          LIKE pro_mae_promotor.codven,
      cod_promotor    LIKE afi_mae_afiliado.cod_promotor,
      nombres_pro     LIKE pro_mae_promotor.nombres,
      paterno_pro     LIKE pro_mae_promotor.paterno,
      materno_pro     LIKE pro_mae_promotor.materno,
      n_seguro        LIKE afi_solicitud.n_seguro,
      n_folio         LIKE afi_solicitud.n_folio,
      frecafor        LIKE afi_solicitud.frecafor,
      tipo_solicitud  LIKE afi_solicitud.tipo_solicitud,
      paterno         LIKE afi_solicitud.paterno,
      materno         LIKE afi_solicitud.materno,
      nombres         LIKE afi_solicitud.nombres,
      salario_actual  LIKE afi_solicitud.salario_actual,
      status_interno  LIKE afi_solicitud.status_interno,
      estado_desc     LIKE tab_status_afi.estado_desc,
      observacion     CHAR(50)
   END RECORD

   DEFINE i INTEGER

    DEFINE valor_accion       DECIMAL(12,6)
    DEFINE salario_minimo     DECIMAL(12,6)
    DEFINE sal_min            DECIMAL(12,6)
    DEFINE total_acciones     DECIMAL(12,6)
    DEFINE ultima_general     DATE
    DEFINE antigua            DATE

    DEFINE vreg         INTEGER,
           vmultiplo    INTEGER,
           vreg_pagados INTEGER,
           vcontador    INTEGER

   DEFINE ejecuta CHAR(800)

   DEFINE vtipo_pago SMALLINT,
          opc        CHAR(01)

   DEFINE vcod_promotor CHAR(10)

define verifica integer
END GLOBALS


MAIN
   DEFER INTERRUPT
   OPTIONS
       ACCEPT KEY CONTROL-I,
       INPUT WRAP,
       PROMPT LINE LAST

   CALL init()

   OPEN WINDOW coml0021 AT 4,4 WITH FORM "COML0021" ATTRIBUTE(BORDER)
   DISPLAY "                             <Ctrl-C> Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " COML0021              GENERACION DE PRODUCCION DE PROMOTORES                  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg_1.* WITHOUT DEFAULTS
      AFTER FIELD fecha_hasta
         IF reg_1.fecha_hasta IS NULL THEN
            ERROR "CAMPO NO PUEDE SER NULO"
            NEXT FIELD fecha_hasta
         END IF

      ON KEY (ESC)
         EXIT INPUT
 
      ON KEY (INTERRUPT)
         PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
         EXIT PROGRAM
        
   END INPUT

   DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

   CALL Inicializa_tabla()
   CALL primer_paso()

   SLEEP 10
   PROMPT " ARCHIVO GENERADO...<ENTER> PARA SALIR " FOR CHAR enter

   CLOSE WINDOW coml0021

END MAIN

FUNCTION Inicializa_tabla()

   WHENEVER ERROR STOP
   DATABASE safre_tmp
   DROP TABLE tmp_afi_sal_men;
   CREATE TABLE tmp_afi_sal_men 
     (
       agenc_cod char(10),
       codven char(10),
       cod_promotor char(10),
       nombresp char(40),
       paternop char(40),
       maternop char(40),
       n_seguro char(11),
       n_folio decimal(8,0),
       finicta date,
       tipo_solicitud smallint,
       nombresa char(40),
       paternoa char(40),
       maternoa char(40),
       salario_actual decimal(12,2),
       fecha_recepcion date,
       ultima_fecha date,
       saldo decimal(16,6),
       comision integer
     );
   DATABASE safre_af
   WHENEVER ERROR CONTINUE
END FUNCTION

FUNCTION init()
   LET HOY                   = TODAY
   LET reg_1.fecha_hasta     = HOY

   SELECT ruta_envio ,
          USER
   INTO   RUTA ,
          seg_usuario
   FROM   seg_modulo   
   WHERE  modulo_cod = "com"

 LET RUTA = "/safre/com/fte"

END FUNCTION

FUNCTION primer_paso()

   DEFINE reg_pro RECORD LIKE pro_mae_promotor.*

   DEFINE reg_afi RECORD LIKE afi_mae_afiliado.*

   DEFINE reg_cta RECORD LIKE dis_cuenta.*

   SELECT A.precio_del_dia
   INTO   valor_accion
   FROM   glo_valor_accion A
   WHERE  A.fecha_valuacion = reg_1.fecha_hasta

   SELECT B.monto_sm
   INTO   salario_minimo
   FROM   tab_salario_minimo B
   WHERE  B.fecha_hasta_sm is null
   AND    B.fecha_desde_sm is not null

let verifica = 0

   DECLARE cur_1 CURSOR FOR 
   SELECT DISTINCT
          D.agenc_cod,
          D.codven,
          C.cod_promotor,
          D.nombres,
          D.paterno,
          D.materno,
          C.n_seguro,
          C.n_folio,
          C.finicta,
          C.tipo_solicitud,
          C.nombres,
          C.paterno,
          C.materno,
          C.salario_actual,
          min(E.fecha_recepcion)
   FROM   afi_mae_afiliado C, pro_mae_promotor D,OUTER dis_det_aporte E
   WHERE  C.finicta             <= reg_1.fecha_hasta
   AND    C.indicador_comision  =   0
   AND    C.tipo_solicitud      IN  (1,2)
   AND    C.cod_promotor        <>  0
   AND    C.n_folio             IS  NOT NULL
   AND    C.cod_promotor        =   D.cod_promotor
   AND    C.n_seguro            = E.n_seguro
   GROUP  BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14
   ORDER  BY 3

----    LET NOM_ARCHIVO = "PROMO1_", HOY USING"DDMMYYYY"
   LET NOM_ARCHIVO = "PRUEBA_PROMO_1_", HOY USING"DDMMYYYY"

   LET G_LISTA = RUTA CLIPPED,"/",NOM_ARCHIVO CLIPPED

   START REPORT listado_1 TO G_LISTA
   LET cont_de_registros = 0

   FOREACH cur_1 INTO reg_prod.*

      SELECT SUM(E.monto_en_acciones)
      INTO   total_acciones
      FROM   dis_cuenta E
      WHERE  E.nss       =   reg_prod.n_seguro
      AND    E.subcuenta NOT IN (4,8)
             
      LET reg_prod.saldo  =  total_acciones * valor_accion
      LET sal_min         =  reg_prod.salario_actual / salario_minimo
  
      LET antigua         =  HOY - 4 UNITS MONTH
  
      SELECT H.fecha_ult_general
      INTO   ultima_general
      FROM   cta_ctr_cuenta H
      WHERE  H.nss        =  reg_prod.n_seguro

         LET reg_prod.comision        =  0
         LET reg_prod.ultima_fecha    =  ultima_general

         IF sal_min <= 5 THEN
            call graba_afi_sal_men()
         END IF

         IF sal_min > 5 AND sal_min <= 10 THEN
            IF reg_prod.ultima_fecha IS NOT NULL THEN
               LET vtipo_pago = 30      -- cumple con fecha y sdi
               LET reg_prod.comision = 200
            ELSE
               LET vtipo_pago = 25      -- no cumple con fecha y sdi
               LET reg_prod.comision = 0
            END IF
            CALL Graba_comision()
         ELSE 
            IF sal_min > 10 THEN
               IF reg_prod.ultima_fecha IS NOT NULL THEN
                  LET vtipo_pago = 40      -- cumple con fecha y sdi
                  LET reg_prod.comision = 400
               ELSE
                  LET vtipo_pago = 35      -- no cumple con fecha y sdi
                  LET reg_prod.comision = 0
               END IF
               CALL Graba_comision()
            END IF
         END IF
  
         IF sal_min > 5 THEN
            OUTPUT TO REPORT listado_1(reg_prod.*)
         END IF

   END FOREACH
   FINISH REPORT listado_1

-------- comisiones menores a 3 salrios minimos -----------
   DECLARE cur_min CURSOR FOR
   SELECT *
   FROM   safre_tmp:tmp_afi_sal_men
   ORDER  BY cod_promotor,n_folio

-----  LET NOM_ARCHIVO = "PROMO2_", HOY USING"DDMMYYYY"
   LET NOM_ARCHIVO = "PRUEBA_PROMO_2_", HOY USING"DDMMYYYY"

   LET G_LISTA = RUTA CLIPPED,"/",NOM_ARCHIVO CLIPPED

   START REPORT listado_2 TO G_LISTA
   
   LET vcontador = 0
   LET vmultiplo = 0
   LET vreg      = 0
   LET vreg_pagados = 0
   LET sal_min = 0

   FOREACH cur_min INTO reg_prod.*

      LET sal_min = reg_prod.salario_actual / salario_minimo

      SELECT COUNT(*)
      INTO   vreg
      FROM   safre_tmp:tmp_afi_sal_men
      WHERE  cod_promotor = reg_prod.cod_promotor

      LET vmultiplo = vreg / 3

      LET vreg_pagados = vmultiplo * 3

      IF vmultiplo > 0 THEN
         IF vcontador <= vreg_pagados THEN
            LET vcontador = vcontador + 1

            IF reg_prod.ultima_fecha IS NOT NULL THEN
               LET vtipo_pago = 20
               LET reg_prod.comision = 66.67
            ELSE
               LET vtipo_pago = 15
               LET reg_prod.comision = 0
            END IF

            CALL Graba_comision()

            OUTPUT TO REPORT listado_2(reg_prod.*)  
         ELSE
            LET vcontador = 0
         END IF
      ELSE
         IF reg_prod.ultima_fecha IS NOT NULL THEN
            LET vtipo_pago = 10
            LET reg_prod.comision = 0
         ELSE
            LET vtipo_pago = 5
            LET reg_prod.comision = 0
         END IF
         OUTPUT TO REPORT listado_2(reg_prod.*)  
         CALL Graba_comision()
      END IF

   END FOREACH

   FINISH REPORT listado_2

   --------- Emite listado de promotores que no tienen com_comis_detalle ------
   DECLARE cur_no CURSOR FOR
   SELECT d.agenc_cod,
          d.codven,
          s.cod_promotor,
          D.nombres,
          D.paterno,
          D.materno,
          s.n_seguro,
          s.n_folio,
          s.frecafor,
          s.tipo_solicitud,
          s.paterno,
          s.materno,
          s.nombres,
          s.salario_actual,
          s.status_interno,
          t.estado_desc,
          ""
   FROM   afi_solicitud  s, 
          tab_status_afi t,
          pro_mae_promotor d
   WHERE  s.cod_promotor    NOT IN (SELECT codven FROM com_comis_detalle)
   AND    s.cod_promotor    = d.cod_promotor
 AND   s.tipo_solicitud in (1,2)
   AND    s.frecafor       <= reg_1.fecha_hasta
   AND    s.status_interno < 100
   AND    s.status_interno  = t.estado_cod


-----  LET NOM_ARCHIVO = "PROMO3_", HOY USING"DDMMYYYY"
   LET nom_archivo = "PRUEBA_PROMO_3_",hoy USING "DDMMYYYY"

   LET G_LISTA = RUTA CLIPPED,"/",NOM_ARCHIVO CLIPPED

   START REPORT listado_3 TO G_LISTA

   FOREACH cur_no INTO r_pro.*

      OUTPUT TO REPORT listado_3(r_pro.*)

   END FOREACH

   FINISH REPORT listado_3


    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    LET ejecuta="cd ",RUTA CLIPPED,"/",
    "; cat PRUEBA_PROMO_1_",hoy USING"DDMMYYYY",
    " PRUEBA_PROMO_2_",hoy USING"DDMMYYYY",
    " PRUEBA_PROMO_3_",hoy USING"DDMMYYYY",
    " > PRODUC_PROMO_",hoy USING"DDMMYYYY" CLIPPED

    RUN ejecuta

{
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
 LET ejecuta = "cd ",RUTA CLIPPED,"; rm PRUEBA_PROMO_1_",HOY USING"DDMMYYYY"
 RUN ejecuta

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
 LET ejecuta = "cd ",RUTA CLIPPED,"; rm PRUEBA_PROMO_2_",HOY USING"DDMMYYYY"
 RUN ejecuta

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
 LET ejecuta = "cd ",RUTA CLIPPED,"; rm PRUEBA_PROMO_3_",HOY USING"DDMMYYYY"
 RUN ejecuta
}

 LET ejecuta = "cd ",RUTA CLIPPED,"; sort -u ",
               "PRODUC_PROMO_",HOY USING"DDMMYYYY" CLIPPED,
               " > paso "
 RUN ejecuta

 LET ejecuta = "cd ",RUTA CLIPPED,"; mv paso ",
               "PRODUC_PROMO_",HOY USING"DDMMYYYY" CLIPPED
 RUN ejecuta

END FUNCTION 

FUNCTION Graba_comision()
   SELECT "X"
   FROM   com_comis_detalle
   WHERE  nss            = reg_prod.n_seguro
   AND    n_folio        = reg_prod.n_folio
   AND    tipo_solicitud = reg_prod.tipo_solicitud

   IF STATUS = NOTFOUND THEN
           INSERT INTO com_comis_detalle VALUES (
              reg_prod.cod_promotor,        -- codven
              1,                            -- nivel
              10,                           -- cod_tipo_prom
              reg_prod.agenc_cod,           -- coduni_n1
              reg_prod.n_seguro,            -- nss
              reg_prod.n_folio,             -- n_folio
              reg_prod.tipo_solicitud,      -- tipo_solicitud
              reg_prod.finicta,             -- fentcons
              vtipo_pago,                   -- tipo_pago
              reg_1.fecha_hasta,            -- fecha_corte
              reg_prod.salario_actual,      -- salario_base_comis
              sal_min,                      -- num_sm
              1,                            -- cod_esq_comision
              0,                            -- cod_esq_premio
              0,                            -- porcent_comision
              reg_prod.comision,            -- monto_comision
              "N",                          -- comis_pagada
              "",                           -- fecha_pago
              1,                            -- estado_comision
              today,                        -- fecha_calculo
              seg_usuario                   -- usuario
              ) 
   END IF
{
           UPDATE afi_mae_afiliado
           SET    indicador_comision = 1
           WHERE  n_folio            = reg_prod.n_folio
           AND    tipo_solicitud     = reg_prod.tipo_solicitud
           AND    indicador_comision = 0
}
END FUNCTION

FUNCTION graba_afi_sal_men()
   INSERT INTO safre_tmp:tmp_afi_sal_men VALUES (reg_prod.*)
END FUNCTION

REPORT listado_1(reg_2)
    DEFINE reg_2              RECORD
        agenc_cod             LIKE pro_mae_promotor.agenc_cod,
        codven                LIKE pro_mae_promotor.codven,
        cod_promotor          LIKE afi_mae_afiliado.cod_promotor,
        nombres_pro           LIKE pro_mae_promotor.nombres,
        paterno_pro           LIKE pro_mae_promotor.paterno,
        materno_pro           LIKE pro_mae_promotor.materno,
        n_seguro              LIKE afi_mae_afiliado.n_seguro,
        n_folio               LIKE afi_mae_afiliado.n_folio,
        finicta               LIKE afi_mae_afiliado.finicta,
        tipo_solicitud        LIKE afi_mae_afiliado.tipo_solicitud,
        nombres               LIKE afi_mae_afiliado.nombres,
        paterno               LIKE afi_mae_afiliado.paterno,
        materno               LIKE afi_mae_afiliado.materno,
        salario_actual        LIKE afi_mae_afiliado.salario_actual,
        fecha_recepcion       LIKE dis_det_aporte.fecha_recepcion,
        ultima_fecha          LIKE cta_ctr_cuenta.fecha_ult_general,
        saldo                 LIKE dis_cuenta.monto_en_pesos,
        comision              DECIMAL(12,2)
    END RECORD

   DEFINE r_edo RECORD
      n_seguro        LIKE afi_solicitud.n_seguro,
      n_folio         LIKE afi_solicitud.n_folio,
      frecafor        LIKE afi_solicitud.frecafor,
      tipo_solicitud  LIKE afi_solicitud.tipo_solicitud,
      paterno         LIKE afi_solicitud.paterno,
      materno         LIKE afi_solicitud.materno,
      nombres         LIKE afi_solicitud.nombres,
      salario_actual  LIKE afi_solicitud.salario_actual,
      status_interno  LIKE afi_solicitud.status_interno,
      estado_desc     LIKE tab_status_afi.estado_desc,
      observacion     CHAR(50)
   END RECORD

   DEFINE desc CHAR(100)

   DEFINE vfecha_rechazo DATE

   DEFINE nombre     CHAR(60),
          nombre_pro CHAR(60),
          nombre_sol CHAR(60)

    OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   1

    FORMAT

   ON EVERY ROW


{
        SELECT codven
        INTO   vcod_promotor
        FROM   com_comis_detalle
        WHERE  nss            = reg_2.n_seguro
        AND    n_folio        = reg_2.n_folio
        AND    tipo_solicitud = reg_2.tipo_solicitud

        IF STATUS <> NOTFOUND THEN
           IF reg_2.cod_promotor <> vcod_promotor THEN
              LET reg_2.cod_promotor = vcod_promotor
           END IF
        END IF
}

        SELECT paterno,
               materno,
               nombres
        INTO   reg_2.paterno,
               reg_2.materno,
               reg_2.nombres
        FROM   pro_mae_promotor
        WHERE  cod_promoto = reg_2.cod_promotor

        LET cont_de_registros = cont_de_registros + 1

        LET nombre =     reg_2.paterno CLIPPED," ",
                         reg_2.materno CLIPPED," ",
                         reg_2.nombres CLIPPED
        LET nombre_pro = reg_2.paterno_pro CLIPPED," ",
                         reg_2.materno_pro CLIPPED," ",
                         reg_2.nombres_pro CLIPPED

        PRINT
            COLUMN 001,reg_2.agenc_cod                         ,"|",
            COLUMN 012,reg_2.codven                            ,"|",
            COLUMN 023,reg_2.cod_promotor                      ,"|",
            COLUMN 034,nombre_pro                              ,"|",
            COLUMN 095,reg_2.n_seguro                          ,"|",
            COLUMN 107,reg_2.n_folio                           ,"|",
            COLUMN 114,reg_2.finicta        USING "DDMMYYYY"   ,"|",
            COLUMN 125,reg_2.tipo_solicitud                    ,"|",
            COLUMN 127,nombre                                  ,"|",
            COLUMN 188,reg_2.salario_actual USING "##########&&.&&&&&&","|",
            COLUMN 208,reg_2.fecha_recepcion USING "DDMMYYYY"   ,"|",
            COLUMN 217,reg_2.saldo          USING "##########&&.&&&&&&","|",
            COLUMN 237,reg_2.comision       USING    "##&&.&&" ,"|",
            COLUMN 245," ","|"
   AFTER GROUP OF reg_2.cod_promotor
      DECLARE cur_estado CURSOR FOR
      SELECT s.n_seguro,
             s.n_folio,
             s.frecafor,
             s.tipo_solicitud,
             s.paterno,
             s.materno,
             s.nombres,
             s.salario_actual,
             s.status_interno,
             t.estado_desc,
             ""
      FROM   afi_solicitud  s, OUTER 
             tab_status_afi t
      WHERE  s.cod_promotor    = reg_2.cod_promotor   
      AND    s.frecafor       <= reg_1.fecha_hasta
AND   s.tipo_solicitud in (1,2)
      AND    s.status_interno < 100
      AND    s.status_interno  = t.estado_cod
    
      FOREACH cur_estado INTO r_edo.*

        SELECT MAX(f_rechazo)
        INTO   vfecha_rechazo
        FROM   afi_rechaza_cert
        WHERE  n_folio        = r_edo.n_folio
        AND    tipo_solicitud = r_edo.tipo_solicitud
        AND    n_seguro       = r_edo.n_seguro

        SELECT observacion
        INTO   r_edo.observacion
        FROM   afi_rechaza_cert
        WHERE  n_folio        = r_edo.n_folio
        AND    tipo_solicitud = r_edo.tipo_solicitud
        AND    n_seguro       = r_edo.n_seguro
        AND    f_rechazo      = vfecha_rechazo
   
        LET nombre_sol = r_edo.paterno CLIPPED," ",
                         r_edo.materno CLIPPED," ",
                         r_edo.nombres CLIPPED

        LET desc = r_edo.estado_desc CLIPPED," ",r_edo.observacion CLIPPED

        PRINT
            COLUMN 001,reg_2.agenc_cod                         ,"|",
            COLUMN 012,reg_2.codven                            ,"|",
            COLUMN 023,reg_2.cod_promotor                      ,"|",
            COLUMN 034,nombre_pro                              ,"|",
            COLUMN 095,r_edo.n_seguro                          ,"|",
            COLUMN 107,r_edo.n_folio                           ,"|",
            COLUMN 114,r_edo.frecafor       USING "DDMMYYYY"   ,"|",
            COLUMN 125,r_edo.tipo_solicitud                    ,"|",
            COLUMN 127,nombre_sol                              ,"|",
            COLUMN 188,r_edo.salario_actual USING "##########&&.&&&&&&","|",
            COLUMN 208,"        ","|",
            COLUMN 217,0                    USING "##########&&.&&&&&&","|",
            COLUMN 237,0                    USING    "##&&.&&" ,"|",
            COLUMN 245,desc CLIPPED,                                    "|"

            LET nombre_sol = ""
            LET desc       = ""
      END FOREACH

END REPORT 

function stop(r_edo,vfecha_rechazo)
   DEFINE r_edo RECORD
      n_seguro        LIKE afi_solicitud.n_seguro,
      n_folio         LIKE afi_solicitud.n_folio,
      frecafor        LIKE afi_solicitud.frecafor,
      tipo_solicitud  LIKE afi_solicitud.tipo_solicitud,
      paterno         LIKE afi_solicitud.paterno,
      materno         LIKE afi_solicitud.materno,
      nombres         LIKE afi_solicitud.nombres,
      salario_actual  LIKE afi_solicitud.salario_actual,
      status_interno  LIKE afi_solicitud.status_interno,
      estado_desc     LIKE tab_status_afi.estado_desc,
      observacion     CHAR(50)
   END RECORD
define vfecha_rechazo date
   define opc char(01)
   display "n_folio    ",r_edo.n_folio
   display "sol        ",r_edo.tipo_solicitud
   display "nss        ",r_edo.n_seguro
   display "fecha rech ",vfecha_rechazo
   display "observa    ",r_edo.observacion
   prompt '' for opc
end function

function stop2(reg_2)
    DEFINE reg_2              RECORD
        agenc_cod             LIKE pro_mae_promotor.agenc_cod,
        codven                LIKE pro_mae_promotor.codven,
        cod_promotor          LIKE afi_mae_afiliado.cod_promotor,
        nombres_pro           LIKE pro_mae_promotor.nombres,
        paterno_pro           LIKE pro_mae_promotor.paterno,
        materno_pro           LIKE pro_mae_promotor.materno,
        n_seguro              LIKE afi_mae_afiliado.n_seguro,
        n_folio               LIKE afi_mae_afiliado.n_folio,
        finicta               LIKE afi_mae_afiliado.finicta,
        tipo_solicitud        LIKE afi_mae_afiliado.tipo_solicitud,
        nombres               LIKE afi_mae_afiliado.nombres,
        paterno               LIKE afi_mae_afiliado.paterno,
        materno               LIKE afi_mae_afiliado.materno,
        salario_actual        LIKE afi_mae_afiliado.salario_actual,
        fecha_recepcion       LIKE dis_det_aporte.fecha_recepcion,
        ultima_fecha          LIKE cta_ctr_cuenta.fecha_ult_general,
        saldo                 LIKE dis_cuenta.monto_en_pesos,
        comision              DECIMAL(12,2)
    END RECORD

   define opc char(01)

 display "cod_promotor    ",reg_2.cod_promotor
 display "codven          ",reg_2.codven      
 display "nss             ",reg_2.n_seguro
 display "n_folio         ",reg_2.n_folio
 display "tipo soli       ",reg_2.tipo_solicitud
prompt '' for opc
end function

REPORT listado_2(reg_2)
    DEFINE reg_2              RECORD
        agenc_cod             LIKE pro_mae_promotor.agenc_cod,
        codven                LIKE pro_mae_promotor.codven,
        cod_promotor          LIKE afi_mae_afiliado.cod_promotor,
        nombres_pro           LIKE pro_mae_promotor.nombres,
        paterno_pro           LIKE pro_mae_promotor.paterno,
        materno_pro           LIKE pro_mae_promotor.materno,
        n_seguro              LIKE afi_mae_afiliado.n_seguro,
        n_folio               LIKE afi_mae_afiliado.n_folio,
        finicta               LIKE afi_mae_afiliado.finicta,
        tipo_solicitud        LIKE afi_mae_afiliado.tipo_solicitud,
        nombres               LIKE afi_mae_afiliado.nombres,
        paterno               LIKE afi_mae_afiliado.paterno,
        materno               LIKE afi_mae_afiliado.materno,
        salario_actual        LIKE afi_mae_afiliado.salario_actual,
        fecha_recepcion       LIKE dis_det_aporte.fecha_recepcion,
        ultima_fecha          LIKE cta_ctr_cuenta.fecha_ult_general,
        saldo                 LIKE dis_cuenta.monto_en_pesos,
        comision              DECIMAL(12,2)
    END RECORD

   DEFINE r_edo RECORD
      n_seguro        LIKE afi_solicitud.n_seguro,
      n_folio         LIKE afi_solicitud.n_folio,
      frecafor        LIKE afi_solicitud.frecafor,
      tipo_solicitud  LIKE afi_solicitud.tipo_solicitud,
      paterno         LIKE afi_solicitud.paterno,
      materno         LIKE afi_solicitud.materno,
      nombres         LIKE afi_solicitud.nombres,
      salario_actual  LIKE afi_solicitud.salario_actual,
      status_interno  LIKE afi_solicitud.status_interno,
      estado_desc     LIKE tab_status_afi.estado_desc,
      observacion     CHAR(50)
   END RECORD

   DEFINE desc CHAR(100)

   DEFINE vfecha_rechazo DATE

   DEFINE nombre     CHAR(60),
          nombre_pro CHAR(60),
          nombre_sol CHAR(60)

    OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   1

    FORMAT

   ON EVERY ROW
        LET cont_de_registros = cont_de_registros + 1

{
        SELECT codven
        INTO   vcod_promotor
        FROM   com_comis_detalle
        WHERE  nss            = reg_2.n_seguro
        AND    n_folio        = reg_2.n_folio
        AND    tipo_solicitud = reg_2.tipo_solicitud

        IF STATUS <> NOTFOUND THEN
           IF reg_2.cod_promotor <> vcod_promotor THEN
              LET reg_2.cod_promotor = vcod_promotor
           END IF
        END IF
}

              SELECT paterno,
                     materno,
                     nombres
              INTO   reg_2.paterno,
                     reg_2.materno,
                     reg_2.nombres
              FROM   pro_mae_promotor
              WHERE  cod_promoto = reg_2.cod_promotor

        LET nombre =     reg_2.paterno CLIPPED," ",
                         reg_2.materno CLIPPED," ",
                         reg_2.nombres CLIPPED
        LET nombre_pro = reg_2.paterno_pro CLIPPED," ",
                         reg_2.materno_pro CLIPPED," ",
                         reg_2.nombres_pro CLIPPED

        PRINT
            COLUMN 001,reg_2.agenc_cod                         ,"|",
            COLUMN 012,reg_2.codven                            ,"|",
            COLUMN 023,reg_2.cod_promotor                      ,"|",
            COLUMN 034,nombre_pro                              ,"|",
            COLUMN 095,reg_2.n_seguro                          ,"|",
            COLUMN 107,reg_2.n_folio                           ,"|",
            COLUMN 114,reg_2.finicta        USING "DDMMYYYY"   ,"|",
            COLUMN 125,reg_2.tipo_solicitud                    ,"|",
            COLUMN 127,nombre                                  ,"|",
            COLUMN 188,reg_2.salario_actual USING "##########&&.&&&&&&","|",
            COLUMN 208,reg_2.fecha_recepcion USING "DDMMYYYY"   ,"|",
            COLUMN 217,reg_2.saldo          USING "##########&&.&&&&&&","|",
            COLUMN 237,reg_2.comision       USING    "##&&.&&" ,"|",
            COLUMN 245," ","|"
   AFTER GROUP OF reg_2.cod_promotor
      DECLARE cur_estado2 CURSOR FOR
      SELECT s.n_seguro,
             s.n_folio,
             s.frecafor,
             s.tipo_solicitud,
             s.paterno,
             s.materno,
             s.nombres,
             s.salario_actual,
             s.status_interno,
             t.estado_desc,
             ""
      FROM   afi_solicitud s, OUTER 
             tab_status_afi  t
      WHERE  s.cod_promotor    = reg_2.cod_promotor   
      AND    s.frecafor       <= reg_1.fecha_hasta
AND   s.tipo_solicitud in (1,2)
      AND    s.status_interno <= 90
      AND    s.status_interno  = t.estado_cod
    
      FOREACH cur_estado2 INTO r_edo.*
        SELECT MAX(f_rechazo)
        INTO   vfecha_rechazo
        FROM   afi_rechaza_cert
        WHERE  n_folio        = r_edo.n_folio
        AND    tipo_solicitud = r_edo.tipo_solicitud
        AND    n_seguro       = r_edo.n_seguro

        SELECT observacion
        INTO   r_edo.observacion
        FROM   afi_rechaza_cert
        WHERE  n_folio        = r_edo.n_folio
        AND    tipo_solicitud = r_edo.tipo_solicitud
        AND    n_seguro       = r_edo.n_seguro
        AND    f_rechazo      = vfecha_rechazo


        LET nombre_sol = r_edo.paterno CLIPPED," ",
                         r_edo.materno CLIPPED," ",
                         r_edo.nombres CLIPPED

        LET desc = r_edo.estado_desc CLIPPED," ",r_edo.observacion CLIPPED

        PRINT
            COLUMN 001,reg_2.agenc_cod                         ,"|",
            COLUMN 012,reg_2.codven                            ,"|",
            COLUMN 023,reg_2.cod_promotor                      ,"|",
            COLUMN 034,nombre_pro                              ,"|",
            COLUMN 095,r_edo.n_seguro                          ,"|",
            COLUMN 107,r_edo.n_folio                           ,"|",
            COLUMN 114,r_edo.frecafor       USING "DDMMYYYY"   ,"|",
            COLUMN 125,r_edo.tipo_solicitud                    ,"|",
            COLUMN 127,nombre_sol                              ,"|",
            COLUMN 188,r_edo.salario_actual USING "##########&&.&&&&&&","|",
            COLUMN 208,"        ","|",
            COLUMN 217,0                    USING "##########&&.&&&&&&","|",
            COLUMN 237,0                    USING    "##&&.&&" ,"|",
            COLUMN 245,desc CLIPPED,                                    "|"

            LET nombre_sol = ""
            LET desc       = ""
      END FOREACH

END REPORT 


REPORT listado_3(r_pro)

   DEFINE r_pro RECORD
      agenc_cod       LIKE pro_mae_promotor.agenc_cod,
      codven          LIKE pro_mae_promotor.codven,
      cod_promotor    LIKE afi_mae_afiliado.cod_promotor,
      nombres_pro     LIKE pro_mae_promotor.nombres,
      paterno_pro     LIKE pro_mae_promotor.paterno,
      materno_pro     LIKE pro_mae_promotor.materno,
      n_seguro        LIKE afi_solicitud.n_seguro,
      n_folio         LIKE afi_solicitud.n_folio,
      frecafor        LIKE afi_solicitud.frecafor,
      tipo_solicitud  LIKE afi_solicitud.tipo_solicitud,
      paterno         LIKE afi_solicitud.paterno,
      materno         LIKE afi_solicitud.materno,
      nombres         LIKE afi_solicitud.nombres,
      salario_actual  LIKE afi_solicitud.salario_actual,
      status_interno  LIKE afi_solicitud.status_interno,
      estado_desc     LIKE tab_status_afi.estado_desc,
      observacion     CHAR(50)
   END RECORD

   DEFINE desc CHAR(100)

    DEFINE vfecha_rechazo DATE

   DEFINE nombre     CHAR(60),
          nombre_pro CHAR(60),
          nombre_sol CHAR(60)

    OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   1

    FORMAT

   ON EVERY ROW
        LET cont_de_registros = cont_de_registros + 1

        LET nombre =     r_pro.paterno CLIPPED," ",
                         r_pro.materno CLIPPED," ",
                         r_pro.nombres CLIPPED
        LET nombre_pro = r_pro.paterno_pro CLIPPED," ",
                         r_pro.materno_pro CLIPPED," ",
                         r_pro.nombres_pro CLIPPED

        SELECT MAX(f_rechazo)
        INTO   vfecha_rechazo
        FROM   afi_rechaza_cert
        WHERE  n_folio        = r_pro.n_folio
        AND    tipo_solicitud = r_pro.tipo_solicitud
        AND    n_seguro       = r_pro.n_seguro

        SELECT observacion
        INTO   r_pro.observacion
        FROM   afi_rechaza_cert
        WHERE  n_folio        = r_pro.n_folio
        AND    tipo_solicitud = r_pro.tipo_solicitud
        AND    n_seguro       = r_pro.n_seguro
        AND    f_rechazo      = vfecha_rechazo
       
        LET desc = r_pro.estado_desc CLIPPED," ",r_pro.observacion CLIPPED

        PRINT
            COLUMN 001,r_pro.agenc_cod                         ,"|",
            COLUMN 012,r_pro.codven                            ,"|",
            COLUMN 023,r_pro.cod_promotor                      ,"|",
            COLUMN 034,nombre_pro                              ,"|",
            COLUMN 095,r_pro.n_seguro                          ,"|",
            COLUMN 107,r_pro.n_folio                           ,"|",
            COLUMN 114,r_pro.frecafor       USING "DDMMYYYY"   ,"|",
            COLUMN 125,r_pro.tipo_solicitud                    ,"|",
            COLUMN 127,nombre                                  ,"|",
            COLUMN 188,r_pro.salario_actual USING "##########&&.&&&&&&","|",
            COLUMN 208,"       "                                ,"|",
            COLUMN 217,0                    USING "##########&&.&&&&&&","|",
            COLUMN 237,0                    USING    "##&&.&&" ,"|",
            COLUMN 245," ","|",
            COLUMN 245,desc CLIPPED,                            "|"

            LET nombre_sol = ""
            LET desc       = ""

END REPORT 
