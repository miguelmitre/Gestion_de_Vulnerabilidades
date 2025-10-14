################################################################################
#Proyecto          => SISTEMA DE AFORE.( MEXICO )                              #
#MOdulo            => COM 					               #
#Programa          => COMB017                                                  #
#Desscripcion      => Realiza el calculo del pago de comisiones                #
#By                => ALEJANDRO RAMIREZ                	 		       #
#                  => al 1er PROGRAMA, y toma como referencia afi_mae_afiliado #
#                  => y pro_mae_promotor para insertar la informacion nueva en #
#                  => com_comis_detalle                                        #
#Modifi.           => 8 ENE 2004 El calculo que nosotros haciamos para SDI     #
#                  => era tomando el salario actual del afi_mae_afiliado y por #
#                  => observacion de Luis Moyano ahora tomamos la minima fecha #
#                  => del dis_det_aporte (ult_salario_diario), por lo que ya no##                  => tenemos un segundo programa de calculo (COMB018)         #
#Modifi.           => Se agregaran a com_comis_detalle los registros que ya    ##                  => tengan su primer aportacion.                             #
#Modifi.           => (27 abrl 2004) Piden que cuando hayan dos aportaciones   ##                  => el mismo dia, se tome la mayor en monto                  #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
        fecha_desde DATE,
        fecha_hasta DATE 
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        seg_usuario           CHAR(008) ,
        enter                 CHAR(001)

    DEFINE  reg_prod          RECORD
        agenc_cod             LIKE pro_mae_promotor.agenc_cod,
        codven                LIKE pro_mae_promotor.codven,
        cod_promotor          LIKE afi_mae_afiliado.cod_promotor,
        nombres_pro           LIKE pro_mae_promotor.nombres,
        paterno_pro           LIKE pro_mae_promotor.paterno,
        materno_pro           LIKE pro_mae_promotor.materno,
        n_seguro              LIKE afi_mae_afiliado.n_seguro,
        n_folio               LIKE afi_mae_afiliado.n_folio,
     -- fentcons              LIKE afi_mae_afiliado.fentcons,
        fecha_elaboracion     LIKE afi_mae_afiliado.fecha_elaboracion,
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

    DEFINE comando            CHAR(300)
    DEFINE salario_minimo     DECIMAL(12,6)
    DEFINE ultima_general     DATE
    DEFINE sal_min            DECIMAL(12,6)
    DEFINE monto_comision     like com_comis_detalle.monto_comision
    DEFINE vtipo_pago         SMALLINT


END GLOBALS


MAIN
  DEFER INTERRUPT
  OPTIONS
       ACCEPT KEY CONTROL-I,
       INPUT WRAP,
       PROMPT LINE LAST

      CALL init()
      CALL primer_paso()

END MAIN

FUNCTION init()
   LET HOY = TODAY
   LET reg_1.fecha_desde = today
   LET reg_1.fecha_hasta = today

   SELECT USER
   INTO   seg_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "com"

END FUNCTION


FUNCTION primer_paso()

 DEFINE reg_pro RECORD LIKE pro_mae_promotor.*
 DEFINE cont  INTEGER

 LET cont = 0
  
 CLEAR SCREEN
 OPEN WINDOW w_3 AT 3,3 WITH FORM "COMB017" ATTRIBUTE(BORDER)
 DISPLAY "                             <Ctrl-C> Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
 DISPLAY " COMB017               GENERACION DE COMISIONES                                " AT 3,1 ATTRIBUTE(REVERSE)
 DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

 INPUT BY NAME reg_1.* WITHOUT DEFAULTS
     AFTER FIELD fecha_desde
        IF reg_1.fecha_desde IS NULL THEN
           ERROR "CAMPO NO PUEDE SER NULO"
           NEXT FIELD fecha_desde
        END IF


     AFTER FIELD fecha_hasta
         IF reg_1.fecha_hasta IS NULL THEN
            ERROR "CAMPO NO PUEDE SER NULO"
            NEXT FIELD fecha_hasta
         END IF

      ON KEY (ESC)
         EXIT INPUT

      ON KEY (INTERRUPT)
       --EXIT INPUT
         EXIT PROGRAM
         CLOSE WINDOW algo
 END INPUT
 CLOSE WINDOW w_3
 CLEAR SCREEN

 DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

 -- OBTENEMOS EL VALOR DEL SALARIO MINIMO
 SELECT B.monto_sm
 INTO   salario_minimo
 FROM   tab_salario_minimo B
 WHERE  B.fecha_hasta_sm is null
 AND    B.fecha_desde_sm is not null


 ------------------------------------------
 DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
 DECLARE cur_1 CURSOR FOR 
    SELECT DISTINCT D.agenc_cod, D.codven, C.cod_promotor,
           D.nombres, D.paterno, D.materno, C.n_seguro,
         --C.n_folio, C.fentcons, C.tipo_solicitud,
           C.n_folio, C.fecha_elaboracion, C.tipo_solicitud,
           C.nombres, C.paterno, C.materno,
           C.salario_actual
    FROM   afi_mae_afiliado C, pro_mae_promotor D
    WHERE  C.fentcons  BETWEEN reg_1.fecha_desde AND reg_1.fecha_hasta
    AND    C.indicador_comision  =   0
    AND    C.tipo_solicitud      IN  (1,2)
    AND    C.cod_promotor        =   D.cod_promotor
and c.frecafor < "04/05/2004"
    ORDER  BY 3

 FOREACH cur_1 INTO reg_prod.*

     SELECT "x" FROM com_comis_detalle
     WHERE  nss  = reg_prod.n_seguro
     AND    n_folio = reg_prod.n_folio
     AND    tipo_solicitud = reg_prod.tipo_solicitud
     IF STATUS = NOTFOUND THEN
 

          -- Parte modificada para calculo de sdi
          LET ultima_general =""
          LET reg_prod.salario_actual = NULL 

          DECLARE c_3 CURSOR FOR
            SELECT MIN(fecha_recepcion),ult_salario_diario/100
            --INTO   ultima_general,reg_prod.salario_actual
            FROM   dis_det_aporte
            WHERE  n_seguro = reg_prod.n_seguro
            GROUP BY 2
            ORDER BY 1,2 desc
          FOREACH c_3 INTO ultima_general,reg_prod.salario_actual
            EXIT FOREACH
          END FOREACH

          LET sal_min         =  reg_prod.salario_actual / salario_minimo

          IF sal_min IS NULL THEN
             let sal_min = 0
          END IF

          LET monto_comision  =  0


          ----------------------------------------------------------------------
          -----MENORES DE 6 SALARIOS
          --------------------------------------------------------------------
          CASE WHEN sal_min <= 5 AND sal_min > 1 AND
                    ultima_general IS NOT NULL

                    LET vtipo_pago = 20
                    LET monto_comision = 66.67

                 -- CALL Actualiza_comision()
                    {
                    SELECT COUNT(*)
                    INTO vreg
                    FROM com_comis_detalle
                    WHERE estado_comision = 0
                    AND codven = reg_prod.codven

                    LET vmultiplo = vreg / 3
                    LET vreg_pagados = vmultiplo * 3

                    IF vmultiplo > 0 THEN
                       IF vcontador <= vreg_pagados THEN
                          LET vcontador = vcontador + 1
                          IF ultima_general IS NOT NULL THEN
                             LET vtipo_pago = 20
                             LET reg_prod.monto_comision = 66.67
                          ELSE
                             LET vtipo_pago = 15
                             LET reg_prod.monto_comision = 0
                          END IF

                       -- CALL Actualiza_comision()
                       ELSE
                          LET vcontador = 0
                       END IF
                    ELSE
                       IF ultima_general IS NOT NULL THEN
                          LET vtipo_pago = 10
                          LET reg_prod.monto_comision = 0
                       ELSE
                          LET vtipo_pago = 5
                          LET reg_prod.monto_comision = 0
                       END IF

                   --  CALL Actualiza_comision()
                    END IF
                    }
          -------------------------------------------------------------------
          -----ENTRE 6 Y 10 SALARIOS
          -------------------------------------------------------------------
          WHEN sal_min > 5 AND sal_min <= 10

               IF ultima_general IS NOT NULL THEN
                  LET vtipo_pago = 30      -- cumple con fecha y sdi
                  LET monto_comision = 200
               ELSE
                  LET vtipo_pago = 25      -- no cumple con fecha y sdi
                  LET monto_comision = 0
               END IF


          ------------------------------------------------------------------
          -----MAYORES A 10 SALARIOS
          ------------------------------------------------------------------
          WHEN sal_min > 10

               IF ultima_general IS NOT NULL THEN
                  LET vtipo_pago = 40      -- cumple con fecha y sdi
                  LET monto_comision = 400
               ELSE
                  LET vtipo_pago = 35      -- no cumple con fecha y sdi
                  LET monto_comision = 0
               END IF

          END CASE


          -- Solo se agregaran los que si tuvieron su primera aporta
          IF sal_min <> 0 THEN

            INSERT INTO com_comis_detalle VALUES (
            reg_prod.cod_promotor,        -- codven
            1,                            -- nivel
            10,                           -- cod_tipo_prom
            reg_prod.agenc_cod,           -- coduni_n1
            reg_prod.n_seguro,            -- nss
            reg_prod.n_folio,             -- n_folio
            reg_prod.tipo_solicitud,      -- tipo_solicitud
          --reg_prod.fentcons,             -- fentcons
            reg_prod.fecha_elaboracion,    -- fec_firma
            vtipo_pago,                   -- tipo_pago
            reg_1.fecha_hasta,            -- fecha_corte
            reg_prod.salario_actual,      -- salario_base_comis SDI
            sal_min,                      -- num_sm
            1,                            -- cod_esq_comision
            0,                            -- cod_esq_premio
            0,                            -- porcent_comision
            monto_comision,               -- comision CON BASE AL SDI Y AL NSAL
            "N",                          -- comis_pagada
            "",                           -- fecha_pago
            1,                            -- estado_comision calculada
            today,                        -- fecha_calculo
            seg_usuario                   -- usuario
            ) 

           UPDATE afi_mae_afiliado
           SET    indicador_comision = 1
           WHERE  n_folio            = reg_prod.n_folio
           AND    tipo_solicitud     = reg_prod.tipo_solicitud
           AND    indicador_comision = 0                     

           LET cont = cont + 1

          END IF   --solo los que tengan su primer aportacion
     END IF

 END FOREACH



END FUNCTION
