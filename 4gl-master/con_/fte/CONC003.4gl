###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa          => GENERA FORMATOS DE INFORMACION CONSAR                   #
#Fecha             => 3 marzo 2003                                            #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
###############################################################################
DATABASE safre_af
GLOBALS
      DEFINE g_reg   RECORD LIKE con_transaccion

      DEFINE g_reg2  RECORD 
          proceso_cod       CHAR(05),
          descripcion       CHAR(80)
      END RECORD

      DEFINE reg_3  RECORD 
          fecha_emision     DATE,
          fecha_valor       DATE,
          identificador     SMALLINT,
          transaccion_cod   INTEGER,
          importe           DECIMAL(15,2),
          proceso_cod       CHAR(05)
      END RECORD

      DEFINE #glo #integer
          cont           ,
          vsolicitado    ,
          vaceptado      ,
          vconfronta     ,
          vcodigo_afore  ,
          vlote          ,
          vfolio         ,
          vfolio1        ,
          tot_registros  INTEGER

      DEFINE #glo #date
          HOY            ,
          vfecha         ,
          f_hoy          DATE

      DEFINE #glo #smallint
          x_liq          ,
          dia            SMALLINT

      DEFINE #glo #char
          G_LISTA	 CHAR(100),
          borra          CHAR(200),
          vclave_entidad CHAR(3),
          vproceso       CHAR(5),
          vproceso1      CHAR(5),
          vdescripcion   CHAR(80),
          vusuario       CHAR(8),
          aux_pausa      CHAR(1),
          vpregunta      ,
          enter          CHAR(1)

      DEFINE g_paramgrales    RECORD LIKE seg_modulo.*

END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL inicio()  #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET vfecha = ""
    LET vfolio = ""
    LET vproceso = ""
    LET vdescripcion = ""
    LET vpregunta = ""
    LET HOY = TODAY
    LET f_hoy = HOY
    LET dia   = DAY(f_hoy) - 1
    LET f_hoy = f_hoy - dia UNITS DAY

    SELECT codigo_afore,
           user
    INTO   vcodigo_afore,
           vusuario
    FROM   tab_afore_local

    LET  vclave_entidad = "09 "

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CONC0031" ATTRIBUTE(BORDER)
       DISPLAY "          [ Esc ] Iniciar                            [ Ctrl-C ] Salir          " AT 3,1 ATTRIBUTE(REVERSE)
       DISPLAY "CONC003         GENERA FORMATOS DE INFORMACION A CONSAR                        " AT 5,1 ATTRIBUTE(REVERSE)
       DISPLAY HOY USING "dd-mm-yyyy" AT 5,67 ATTRIBUTE(REVERSE)

       MENU "CONTABILIDAD "
           COMMAND "Semanal" "Genera formato semanal"
                   CALL semanal() #se
                   ERROR "" 
                   CLEAR FORM
   
           COMMAND "Mensual" "Genera formato mensual"
                   #CALL mensual() #mes
                   ERROR "" 
                   CLEAR FORM
   
           COMMAND "Salir" "Salir de Programa"
                   EXIT MENU
       END MENU
    CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION semanal()
#sem--------------
  DEFINE vdias   INTEGER
  DEFINE vproceso CHAR(5)

  LET  vdias        = 0
  LET  vproceso     = ""
  LET  vdescripcion = ""
  LET  vfolio       = ""
  LET  vfecha       = ""
  LET  vpregunta    = ""
  INPUT BY NAME vfecha,vfecha1 WITHOUT DEFAULTS
      AFTER FIELD vfecha
          IF vfecha IS NULL THEN
	      ERROR "LA FECHA NO PUEDE SER NULA"
	      SLEEP 2
	      ERROR ""
	      NEXT FIELD vfecha
	  END IF
          IF vfecha > HOY THEN
	      ERROR "LA FECHA NO PUEDE MAYOR QUE LA DEL DIA"
	      SLEEP 2
	      ERROR ""
	      NEXT FIELD vfecha
	  END IF
          SELECT "X"
          FROM   con_transaccion
          WHERE  fecha_emision >= vfecha
          AND    fecha_emision <= vfecha + 5 units day
          GROUP BY 1
          IF STATUS = NOTFOUND THEN
              ERROR "NO EXISTEN PROCESOS EN ESTE PERIODO PARA REPORTAR"
              SLEEP 3
              EXIT PROGRAM
          END IF

      AFTER FIELD vfecha1
          IF vfecha1 IS NULL THEN
	      ERROR "LA FECHA NO PUEDE SER NULA"
	      SLEEP 2
	      ERROR ""
	      NEXT FIELD vfecha1
	  END IF
          IF vfecha1 > HOY THEN
	      ERROR "LA FECHA NO PUEDE MAYOR QUE LA DEL DIA"
	      SLEEP 2
	      ERROR ""
	      NEXT FIELD vfecha1
	  END IF
          IF vfecha1 <= vfecha THEN
	      ERROR "LA FECHA FINAL NO PUEDE SER MENOR NI IGUAL QUE LA INICIAL"
	      SLEEP 2
	      ERROR ""
	      NEXT FIELD vfecha1
	  END IF
          SELECT "X"
          FROM   con_transaccion
          WHERE  fecha_emision >= vfecha
          AND    fecha_emision <= vfecha1
          GROUP BY 1
          IF STATUS = NOTFOUND THEN
              ERROR "NO EXISTEN PROCESOS EN ESTE PERIODO PARA REPORTAR"
              SLEEP 3
              EXIT PROGRAM
          END IF
      AFTER FIELD vpregunta

	  IF vpregunta = "S"
	  OR vpregunta = "s"  THEN

              ERROR" PROCESANDO INFORMACION "

              DECLARE cur_1 CURSOR FOR
                  SELECT fecha_emision
                  FROM   con_transaccion
                  WHERE  fecha_emision BETWEEN vfecha
                                       AND     vfecha1
                  GROUP BY 1
		  ORDER BY 1
              FOREACH cur_1 INTO FOR xfecha
                  DECLARE cur_2 CURSOR FOR
                      SELECT vproceso
                      FROM   con_transaccion
                      WHERE  fecha_emision = xfecha
                      GROUP BY 1
                  FOREACH cur_2 INTO FOR vproceso
		      SELECT "X"
		      FROM   con_proceso
		      WHERE  cod_proceso = vproceso
		      GROUP BY 1

		      IF STATUS <> NOTFOUND THEN
		          LET cont = cont + 1
                          CALL detalle1()      #det1
		      END IF
		      IF cont > 0 THEN
		          CALL cza_general()
		      END IF
                  END FOREACH
              END FOREACH
              END IF
              
              ERROR" REGISTRO INGRESADO "
	      SLEEP 2
	      EXIT INPUT
	      ERROR ""
	  ELSE
	      ERROR" PROCESO CANCELADO "
	      SLEEP 2
              CALL inicio()
	      EXIT INPUT
	      ERROR ""
          END IF

      ON KEY (INTERRUPT)
          ERROR " PROCESO CANCELADO "
          SLEEP 2
          CALL inicio()
          EXIT INPUT

    END INPUT

END FUNCTION

FUNCTION pregunta()
    ERROR ""

    PROMPT " DESEA GENERAR EL ARCHIVO  S/N  ?  " for CHAR aux_pausa
END FUNCTION                                                         

FUNCTION crea_tablas()
#ct-------------------

    CREATE TEMP TABLE con_cza
                  (tipo char(3),
                   total integer,
                   tamano integer,
                   archivo integer,
                   cve_afore integer,
                   fenvio date,
		   finicio date,
		   ffinal  date)
    CREATE TEMP TABLE con_det_cza
                  (tipo char(3),
                   fecha date)
    CREATE TEMP TABLE con_detalle
                  (tipo char(3),
                   proceso integer,
                   codigo integer,
                   importe decimal(16,6))
END FUNCTION
FUNCTION detalle_1()
#det1---------------
    DEFINE xproceso   CHAR(5)

    DECLARE cur_2 CURSOR FOR
	SELECT *
	FROM   con_concepto
    FOREACH cur_2 INTO FOR vproceso
    DECLARE cur_3 CURSOR FOR
        SELECT proceso_cod
        FROM   con_transaccion
        WHERE  fecha_emision BETWEEN vfecha
                             AND     vfecha1
	AND    proceso_cod = xproceso
        GROUP BY 1
    FOREACH cur_3 INTO FOR vproceso

    DECLARE rec_1 CURSOR FOR 
        SELECT fecha_emision
        FROM   con_transaccion
        WHERE  fecha_emision = vfecha_proceso
	AND    proceso_cod in("00001","00002","00003","00004","00006")
    FOREACH con_2 INTO vtransaccion,subcta,subcta1

        LET  g_reg.transaccion_cod = vtransaccion
        LET  total_pesos = ""

        IF (subcta = 4 OR
            subcta = 8) THEN
             LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2)",
                              " FROM   dis_provision ",
                              " WHERE  folio =","'", g_reg.folio,"'",
                              " AND    subcuenta in(","'", subcta,"'",")",
                              " AND    tipo_movimiento not in(888,999)"
             PREPARE con_15 FROM  principal
             DECLARE viv_3 CURSOR FOR con_15
             FOREACH viv_3 INTO total_pesos
             END FOREACH

             LET g_reg.importe       = total_pesos
             LET g_reg.fecha_valor   = f_conversion
             LET g_reg.fecha_emision = f_conversion
             LET g_reg.identificador = 1

             IF g_reg.importe <> 0 THEN
                 CALL registra_historico(g_reg.*) #rh
             END IF

        ELSE
            LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2)",
                             " FROM   dis_provision ",
                             " WHERE  folio =","'", g_reg.folio,"'",
                             " AND   (subcuenta in(","'", subcta,"'",")",
                             " OR    subcuenta in(","'", subcta1,"'","))",
                             " AND    tipo_movimiento not in(888,999) "
            PREPARE con_16 FROM  principal
            DECLARE rcv_3 CURSOR FOR con_16
            FOREACH rcv_3 INTO total_pesos
            END FOREACH

     LET vcomision = 0
            SELECT "X"
            FROM   tab_transaccion
            WHERE  transaccion_cod = g_reg.transaccion_cod
            AND    descripcion_1 MATCHES "COMISION*"
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
  LET vcomision = 1
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2)*(-1)",
                               " FROM   dis_provision ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND   (subcuenta in(","'", subcta,"'",")",
                               " OR    subcuenta in(","'", subcta1,"'","))",
                               " AND    tipo_movimiento = 100 "
                PREPARE con_17 FROM  principal
                DECLARE rcv_4 CURSOR FOR con_17
                FOREACH rcv_4 INTO total_pesos
                END FOREACH

            END IF
            LET g_reg.importe       = total_pesos
            LET g_reg.fecha_valor   = f_conversion
            LET g_reg.fecha_emision = f_conversion
            LET g_reg.identificador = 1

            IF g_reg.importe <> 0 THEN
                CALL registra_historico(g_reg.*) #rh

                LET g_reg.importe       = total_pesos / precio
                LET g_reg.identificador = 2

                IF vcomision = 0 THEN
                #Para ALLIANZ
                    CALL registra_historico(g_reg.*) #rh
                END IF
            END IF

       END IF
         
     END FOREACH
END FUNCTION                                                         
FUNCTION obtiene_acreditados()
#oa---------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE vfecha_sig      DATE
    DEFINE cont2           SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE vdia            SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


     DECLARE con_18 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_18 INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_pesos = ""
         LET  precio      = ""
         LET  f_conversion = ""
         LET  vfecha_sig   = ""

         IF (g_reg.transaccion_cod = 24019  OR
             g_reg.transaccion_cod = 29019) THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                               " fecha_conversion ",
                               " FROM   dis_cuenta ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND    subcuenta in(","'", subcta,"'",")",
                               " AND    tipo_movimiento in(1)",
                               #" AND    id_aportante = 'DEV-INF' ",
                               " AND    id_aportante = 'DEV. INF.' ",
                               " GROUP BY 2 "
             PREPARE con_19 FROM  principal
             DECLARE viv_5 CURSOR FOR con_19
             FOREACH viv_5 INTO total_pesos,f_conversion
             END FOREACH
         END IF
         IF (g_reg.transaccion_cod = 24029  OR
             g_reg.transaccion_cod = 29029) THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                               " fecha_conversion ",
                               " FROM   dis_cuenta ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND    subcuenta in(","'", subcta,"'",")",
                               " AND    tipo_movimiento = 4  ",
                               #" AND    id_aportante = 'DEV-INF' ",
                               " AND    id_aportante = 'DEV. INF.' ",
                               " GROUP BY 2 "
             PREPARE con_20 FROM  principal
             DECLARE viv_6 CURSOR FOR con_20
             FOREACH viv_6 INTO total_pesos,f_conversion
             END FOREACH
         END IF
         IF g_reg.transaccion_cod = 24319 THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                               " fecha_conversion ",
                               " FROM   dis_cuenta ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND  (subcuenta in(","'", subcta,"'",")",
                               " OR   subcuenta in(","'", subcta1,"'","))",
                               " AND    tipo_movimiento = 230  ",
                               " AND    id_aportante = 'ACR-ULT' ",
                               " GROUP BY 2 "
             PREPARE con_21 FROM  principal
             DECLARE viv_7 CURSOR FOR con_21
             FOREACH viv_7 INTO total_pesos,f_conversion
             END FOREACH
         END IF
         IF (g_reg.transaccion_cod = 24353 OR
             g_reg.transaccion_cod = 29353 ) THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                               " fecha_conversion ",
                               " FROM   dis_cuenta ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND  (subcuenta in(","'", subcta,"'",")",
                               " OR   subcuenta in(","'", subcta1,"'","))",
                               " AND    tipo_movimiento = 230  ",
                               " AND    id_aportante = 'ACR-TRA' ",
                               " GROUP BY 2 "
             PREPARE con_22 FROM  principal
             DECLARE viv_8 CURSOR FOR con_22
             FOREACH viv_8 INTO total_pesos,f_conversion
             END FOREACH
         END IF
         IF (g_reg.transaccion_cod = 24355 OR
             g_reg.transaccion_cod = 29355 ) THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                               " fecha_conversion ",
                               " FROM   dis_cuenta ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND  (subcuenta in(","'", subcta,"'",")",
                               " OR   subcuenta in(","'", subcta1,"'","))",
                               " AND    tipo_movimiento = 235  ",
                               " AND    id_aportante = 'ACR-TRA' ",
                               " GROUP BY 2 "
             PREPARE con_23 FROM  principal
             DECLARE viv_9 CURSOR FOR con_23
             FOREACH viv_9 INTO total_pesos,f_conversion
             END FOREACH
         END IF
         LET g_reg.importe       = total_pesos
         LET g_reg.fecha_valor   = f_conversion
         LET g_reg.identificador = 1

         LET vfecha_sig = f_conversion
         LET vdia       = DAY(vfecha_sig)
         LET vfecha_sig = vfecha_sig - vdia UNITS DAY
         LET cont2 = 1
         FOR cont2 = 1 TO 5
             CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig
         END FOR
         LET g_reg.fecha_emision = vfecha_sig
         LET g_reg.fecha_valor   = vfecha_sig
         IF g_reg.importe <> 0 THEN
             CALL registra_historico(g_reg.*) #rh
         END IF

     END FOREACH
END FUNCTION                                                         
FUNCTION retiros_ivrt()
#rivrt--------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(500)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


    DECLARE con_24 CURSOR FOR

        SELECT transaccion_cod,
               subcuenta,
               subcuenta1,
               subcuenta2
        FROM   tab_transaccion
        WHERE  proceso_cod = g_reg.proceso_cod
        AND    descripcion_1 NOT MATCHES "*FRACCION"

    FOREACH con_24 INTO vtransaccion,subcta,subcta1,subcta2

        LET  g_reg.transaccion_cod = vtransaccion
        LET  total_pesos = ""
        LET  precio      = ""
        LET  f_conversion = ""

        IF (subcta = 4 OR
            subcta = 8) THEN
            LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                             " fecha_conversion ",
                             " FROM   dis_cuenta ",
                             " WHERE  folio =","'", g_reg.folio,"'",
                             " AND    nss in ( ",
                             " SELECT n_seguro ",
                             " FROM   ret_his_det_pe17 ",
                             " WHERE  folio =","'", g_reg.folio,"'",
                             " AND    tipo_prestacion = 1 ",
                             " AND    diag_cuenta_ind = 12 ) ",
                             " AND  (subcuenta in(","'", subcta,"'",")",
                             " OR   subcuenta in(","'", subcta1,"'","))",
                             " GROUP BY 2 "
            PREPARE con_26 FROM  principal
            DECLARE viv_11 CURSOR FOR con_26
            FOREACH viv_11 INTO total_pesos,f_conversion
            END FOREACH

            LET g_reg.importe       = total_pesos
            LET g_reg.fecha_valor   = f_conversion
            LET g_reg.fecha_emision = f_conversion
            LET g_reg.identificador = 1

            IF g_reg.importe <> 0 THEN
                CALL registra_historico(g_reg.*) #rh
            END IF

        ELSE
            CASE vtransaccion
                WHEN 91001
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  " a.precio_accion, ",
                                  " a.fecha_conversion ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.nss in ( ",
                                  " SELECT b.n_seguro ",
                                  " FROM   ret_his_det_pe05 b ",
                                  " WHERE  b.folio =","'", g_reg.folio,"'",
                                  " AND    b.tipo_seguro = 'IM' ",
                                  " AND    b.tipo_pension = 'IN' ",
                                  " AND    b.diag_cuenta_ind = '01') ",
                                  " AND    a.subcuenta in(1,2,5,6,9) ",
                                  " AND    a.tipo_movimiento not in(888,999) ",
                                  " GROUP BY 2,3 "
                    PREPARE con_27 FROM  principal
                    DECLARE rcv_5 CURSOR FOR con_27
                    FOREACH rcv_5 INTO total_pesos,precio,f_conversion
                    END FOREACH

                 WHEN 91002
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                  " precio_accion, ",
                                  " fecha_conversion ",
                                  " FROM   dis_cuenta ",
                                  " WHERE  folio =","'", g_reg.folio,"'",
                                  " AND    nss in ( ",
                                  " SELECT n_seguro ",
                                  " FROM   ret_his_det_pe05 ",
                                  " WHERE  folio =","'", g_reg.folio,"'",
				  " AND    tipo_seguro in('IM','RT') ",
                                  " AND    tipo_pension not in('IN','IP') ",
                                  " AND    diag_cuenta_ind = '01') ",
                                  " AND    subcuenta in(1,2,5,6,9) ",
                                  " AND    tipo_movimiento not in(888,999) ",
                                  " GROUP BY 2,3 "
                    PREPARE con_28 FROM  principal
                    DECLARE rcv_6 CURSOR FOR con_28
                    FOREACH rcv_6 INTO total_pesos,precio,f_conversion
                    END FOREACH

                 WHEN 91003
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                  " precio_accion, ",
                                  " fecha_conversion ",
                                  " FROM   dis_cuenta ",
                                  " WHERE  folio =","'", g_reg.folio,"'",
                                  " AND    nss in ( ",
                                  " SELECT n_seguro ",
                                  " FROM   ret_his_det_pe05 ",
                                  " WHERE  folio =","'", g_reg.folio,"'",
                                  " AND    tipo_seguro = 'RT' ",
                                  " AND    tipo_pension = 'IP' ",
                                  " AND    diag_cuenta_ind = '01') ",
                                  " AND    subcuenta in(1,2,5,6,9) ",
                                  " AND    tipo_movimiento not in(888,999) ",
                                  " GROUP BY 2,3 "
                    PREPARE con_29 FROM  principal
                    DECLARE rcv_7 CURSOR FOR con_29
                    FOREACH rcv_7 INTO total_pesos,precio,f_conversion
                    END FOREACH

                 OTHERWISE
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                   " precio_accion, ",
                                   " fecha_conversion ",
                                   " FROM   dis_cuenta ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND    nss in ( ",
                                   " SELECT n_seguro ",
                                   " FROM   ret_his_det_pe05 ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND   diag_cuenta_ind  =  '01') ",
                                   " AND   (subcuenta in(","'", subcta,"'",")",
                                   " OR    subcuenta in(","'", subcta1,"'",")",
                                   " OR    subcuenta in(","'", subcta2,"'","))",
                                   " GROUP BY 2,3 "
                     PREPARE con_30 FROM  principal
                     DECLARE rcv_8 CURSOR FOR con_30
                     FOREACH rcv_8 INTO total_pesos,precio,f_conversion
                     END FOREACH

              END CASE
              LET g_reg.importe       = total_pesos
              LET g_reg.fecha_valor   = f_conversion
              LET g_reg.fecha_emision = f_conversion
              LET g_reg.identificador = 2

              IF g_reg.importe <> 0 THEN
		  IF g_reg.transaccion_cod < 91001  AND
                     g_reg.transaccion_cod > 91003  THEN
                      CALL registra_historico(g_reg.*) #rh
                  END IF

                  LET g_reg.importe       = total_pesos * precio
                  LET g_reg.identificador = 1

                  CALL registra_historico(g_reg.*) #rh
              END IF

         END IF
         
    END FOREACH
END FUNCTION                                                         
FUNCTION retiros_diversos()
#rd------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(500)
    DEFINE precio          DECIMAL(10,6)
    DEFINE vimporte_dif    DECIMAL(15,2)
    DEFINE vaccion_dif     DECIMAL(15,2)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_acciones  DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


    DECLARE con_31 CURSOR FOR

        SELECT transaccion_cod,
               subcuenta,
               subcuenta1,
               subcuenta2
        FROM   tab_transaccion
        WHERE  proceso_cod = g_reg.proceso_cod
        AND    descripcion_1 NOT MATCHES "*FRACCION"

    FOREACH con_31 INTO vtransaccion,subcta,subcta1,subcta2

        LET  g_reg.transaccion_cod = vtransaccion
        LET  total_pesos    = ""
        LET  total_acciones = ""
        LET  precio         = ""
        LET  f_conversion   = ""

        CASE vtransaccion 
            WHEN  24315
                LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_his_det_pe17 ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    tipo_prestacion = 3 ) ",
                                 " AND    subcuenta in(","'", subcta,"'",")",
                                 " GROUP BY 2 "
                PREPARE con_32 FROM  principal
                DECLARE viv_12 CURSOR FOR con_32
                FOREACH viv_12 INTO total_pesos,f_conversion
                END FOREACH
 
                LET g_reg.importe       = total_pesos
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 1

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN  29301
                LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_his_det_pe28 ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 " AND    subcuenta in(","'", subcta,"'",")",
                                 " AND    fecha_conversion = ","'",vfecha,"'",
                                 " GROUP BY 2 "
                PREPARE con_33 FROM  principal
                DECLARE viv_13 CURSOR FOR con_33
                FOREACH viv_13 INTO total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_pesos
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 1

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN  22315
                LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                 " ROUND(SUM(a.monto_en_pesos),2), ",
                                 " a.fecha_conversion ",
                                 " FROM   dis_cuenta a ",
                                 " WHERE  a.folio =","'", g_reg.folio,"'",
                                 " AND    a.nss in ( ",
                                 " SELECT b.n_seguro ",
                                 " FROM   ret_his_det_pe13 b )",
                                 #" WHERE  b.folio =","'", g_reg.folio,"'",")",
                                 " AND    a.subcuenta in(2,6,9) ",
                                 " AND    a.tipo_movimiento not in(10) ",
                                 " GROUP BY 3 "
                PREPARE con_34 FROM  principal
                DECLARE rcv_9 CURSOR FOR con_34
                FOREACH rcv_9 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 23315
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_his_det_pe13 )",
                                 #" WHERE  folio =","'", g_reg.folio,"'",")",
                                 " AND    subcuenta = 5 ",
                                 " AND    tipo_movimiento not in(10) ",
                                 " GROUP BY 3 "
                PREPARE con_35 FROM  principal
                DECLARE rcv_10 CURSOR FOR con_35
                FOREACH rcv_10 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
		#arc

		{
		CALL verifica_diferencia(g_reg.folio,436,g_reg.proceso_cod)
		RETURNING g_reg.importe

                IF g_reg.importe <> 0 THEN
                    LET g_reg.identificador = 2
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = g_reg.importe * precio
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
		}

            WHEN 21315
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_his_det_pe13 ) ",
                                 #" WHERE  folio =","'", g_reg.folio,"'",")",
                                 " AND    subcuenta = 1 ",
                                 " AND    tipo_movimiento not in(10) ",
                                 " GROUP BY 3 "
                PREPARE con_36 FROM  principal
                DECLARE rcv_11 CURSOR FOR con_36
                FOREACH rcv_11 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN  26510
                LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                 " ROUND(SUM(a.monto_en_pesos),2), ",
                                 " a.fecha_conversion ",
                                 " FROM   dis_cuenta a ",
                                 " WHERE  a.folio =","'", g_reg.folio,"'",
                                 " AND    a.nss in ( ",
                                 " SELECT b.n_seguro ",
                                 " FROM   ret_his_det_pe13 b )",
                                 #" WHERE  b.folio =","'", g_reg.folio,"'",")",
                                 " AND    a.subcuenta in(1) ",
                                 " AND    a.tipo_movimiento in(10) ",
                                 " GROUP BY 3 "
                PREPARE con_69 FROM  principal
                DECLARE rcv_28 CURSOR FOR con_69
                FOREACH rcv_28 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
            WHEN  26511
                LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                 " ROUND(SUM(a.monto_en_pesos),2), ",
                                 " a.fecha_conversion ",
                                 " FROM   dis_cuenta a ",
                                 " WHERE  a.folio =","'", g_reg.folio,"'",
                                 " AND    a.nss in ( ",
                                 " SELECT b.n_seguro ",
                                 " FROM   ret_his_det_pe13 b )",
                                 #" WHERE  b.folio =","'", g_reg.folio,"'",")",
                                 " AND    a.subcuenta in(5) ",
                                 " AND    a.tipo_movimiento in(10) ",
                                 " GROUP BY 3 "
                PREPARE con_70 FROM  principal
                DECLARE rcv_29 CURSOR FOR con_70
                FOREACH rcv_29 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
            WHEN  26512
                LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                 " ROUND(SUM(a.monto_en_pesos),2), ",
                                 " a.fecha_conversion ",
                                 " FROM   dis_cuenta a ",
                                 " WHERE  a.folio =","'", g_reg.folio,"'",
                                 " AND    a.nss in ( ",
                                 " SELECT b.n_seguro ",
                                 " FROM   ret_his_det_pe13 b )",
                                 #" WHERE  b.folio =","'", g_reg.folio,"'",")",
                                 " AND    a.subcuenta in(2,6,9) ",
                                 " AND    a.tipo_movimiento in(10) ",
                                 " GROUP BY 3 "
                PREPARE con_71 FROM  principal
                DECLARE rcv_30 CURSOR FOR con_71
                FOREACH rcv_30 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
            WHEN 26305
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_cta_vol ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 " AND    subcuenta in(","'", subcta,"'",")",
                                 " AND    tipo_movimiento = 490 ",
                                 " GROUP BY 3 "
                PREPARE con_37 FROM  principal
                DECLARE rcv_12 CURSOR FOR con_37
                FOREACH rcv_12 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 26307
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_cta_vol ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 " AND    subcuenta in(","'", subcta,"'",")",
                                 " AND    tipo_movimiento = 490 ",
                                 " GROUP BY 3 "
                PREPARE con_43 FROM  principal
                DECLARE rcv_18 CURSOR FOR con_43
                FOREACH rcv_18 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
#arc
            WHEN 26500
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_cta_vol ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 #" AND    subcuenta in(","'", subcta,"'",")",
                                 " AND    subcuenta in(3) ",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3 "
                PREPARE con_67 FROM  principal
                DECLARE rcv_26 CURSOR FOR con_67
                FOREACH rcv_26 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2
                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 26501
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_cta_vol ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 #" AND    subcuenta in(","'", subcta,"'",")",
                                 " AND    subcuenta in(10) ",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3 "
                PREPARE con_72 FROM  principal
                DECLARE rcv_31 CURSOR FOR con_72
                FOREACH rcv_31 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2
                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
            WHEN 23301
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 5 ",
                                 " AND    tipo_movimiento = 487 ",
                                 " GROUP BY 3 "
                PREPARE con_38 FROM  principal
                DECLARE rcv_13 CURSOR FOR con_38
                FOREACH rcv_13 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 21303
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 1 ",
                                 " AND    tipo_movimiento = 486 ",
                                 " GROUP BY 3 "
                PREPARE con_39 FROM  principal
                DECLARE rcv_14 CURSOR FOR con_39
                FOREACH rcv_14 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 22303
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(2,6,9) ",
                                 " AND    tipo_movimiento = 486 ",
                                 " GROUP BY 3 "
                PREPARE con_40 FROM  principal
                DECLARE rcv_15 CURSOR FOR con_40
                FOREACH rcv_15 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                #LET g_reg.importe       = total_pesos
                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    #LET g_reg.importe       = total_pesos * precio
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 23303
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 5 ",
                                 " AND    tipo_movimiento = 486 ",
                                 " GROUP BY 3 "
                PREPARE con_41 FROM  principal
                DECLARE rcv_16 CURSOR FOR con_41
                FOREACH rcv_16 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    #LET g_reg.importe       = total_pesos * precio
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
		LET vaccion_dif  = 0
		LET vimporte_dif = 0

		{
		CALL verifica_diferencia(g_reg.folio,486,g_reg.proceso_cod)
		RETURNING g_reg.importe

                IF g_reg.importe <> 0 THEN
                    LET g_reg.identificador = 2
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = g_reg.importe * precio
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
		}
            WHEN 28301
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_his_det_pe29 ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 " AND    subcuenta = 7 ",
                                 " AND    tipo_movimiento not in(10) ",
                                 " AND    fecha_conversion = ","'",vfecha,"'",
                                 " GROUP BY 3 "
                PREPARE con_42 FROM  principal
                DECLARE rcv_17 CURSOR FOR con_42
                FOREACH rcv_17 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 26520
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_his_det_pe29 ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 " AND    subcuenta = 7 ",
                                 " AND    tipo_movimiento in(10) ",
                                 " AND    fecha_conversion = ","'",vfecha,"'",
                                 " GROUP BY 3 "
                PREPARE con_68 FROM  principal
                DECLARE rcv_27 CURSOR FOR con_68
                FOREACH rcv_27 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
#arc aqui me quede 241002
            WHEN 21500
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 1 ",
                                 " AND    tipo_movimiento in ( ",
                                 " SELECT codigo ",
                                 " FROM   tab_movimiento ",
                                 " WHERE descripcion MATCHES '*PLAN PRIVADO*')",
                                 " GROUP BY 3 "
                PREPARE con_78 FROM  principal
                DECLARE rcv_36 CURSOR FOR con_78
                FOREACH rcv_36 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 22500
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(2,6,9) ",
                                 " AND    tipo_movimiento in ( ",
                                 " SELECT codigo ",
                                 " FROM   tab_movimiento ",
                                 " WHERE descripcion MATCHES '*PLAN PRIVADO*')",
                                 " GROUP BY 3 "
                PREPARE con_80 FROM  principal
                DECLARE rcv_37 CURSOR FOR con_80
                FOREACH rcv_37 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 23500
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 5  ",
                                 " AND    tipo_movimiento in ( ",
                                 " SELECT codigo ",
                                 " FROM   tab_movimiento ",
                                 " WHERE descripcion MATCHES '*PLAN PRIVADO*')",
                                 " GROUP BY 3 "
                PREPARE con_81 FROM  principal
                DECLARE rcv_38 CURSOR FOR con_81
                FOREACH rcv_38 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 24550
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 4  ",
                                 " AND    tipo_movimiento in ( ",
                                 " SELECT codigo ",
                                 " FROM   tab_movimiento ",
                                 " WHERE descripcion MATCHES '*PLAN PRIVADO*')",
                                 " GROUP BY 3 "
                PREPARE con_82 FROM  principal
                DECLARE rcv_39 CURSOR FOR con_82
                FOREACH rcv_39 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_pesos
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 1

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 26530
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 1 ",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_his_det_pe40) ",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3 "
                PREPARE con_83 FROM  principal
                DECLARE rcv_40 CURSOR FOR con_83
                FOREACH rcv_40 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 26531
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 5 ",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_his_det_pe40) ",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3 "
                PREPARE con_84 FROM  principal
                DECLARE rcv_41 CURSOR FOR con_84
                FOREACH rcv_41 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF

            WHEN 26532
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(2,6,9)  ",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_his_det_pe40) ",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3 "
                PREPARE con_85 FROM  principal
                DECLARE rcv_42 CURSOR FOR con_85
                FOREACH rcv_42 INTO total_acciones,total_pesos,f_conversion
                END FOREACH

                LET g_reg.importe       = total_acciones
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                LET g_reg.identificador = 2

                IF g_reg.importe <> 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1

                    CALL registra_historico(g_reg.*) #rh
                END IF
        END CASE
    END FOREACH
END FUNCTION                                                         

FUNCTION unificacion()
#ou-----------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE max_fecha       DATE
    #DEFINE subcta          CHAR(05)
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE tipo_mov        SMALLINT
    DEFINE i               SMALLINT
    DEFINE vdia            SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE pesos           DECIMAL(15,2)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


     DECLARE con_44 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_44 INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_pesos = ""
         LET  precio      = ""
         LET  f_conversion = ""

         IF (subcta = 4 OR
             subcta = 8) THEN
             LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                              " fecha_conversion ",
                              " FROM   dis_cuenta ",
                              " WHERE  folio =","'", g_reg.folio,"'",
                              " AND   ( subcuenta in(","'", subcta,"'",")",
                              " OR    subcuenta in(","'", subcta1,"'","))",
                              " AND    tipo_movimiento in(1,4)",
                              " GROUP BY 2 "
             PREPARE con_45 FROM  principal
             DECLARE viv_10 CURSOR FOR con_45
             FOREACH viv_10 INTO total_pesos,f_conversion
                 LET g_reg.importe       = total_pesos
		 LET i = 1
                 LET max_fecha = f_conversion -1 UNITS DAY
		 FOR i = 1 TO 5
                      CALL habil_siguiente(max_fecha) RETURNING max_fecha
                 END FOR
                 LET g_reg.fecha_emision = max_fecha
                 LET g_reg.fecha_valor   = max_fecha
                 LET g_reg.identificador = 1

                 IF g_reg.importe > 0 THEN
                     CALL registra_historico(g_reg.*) #rh
                 END IF
             END FOREACH
         ELSE
             LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2), ",
                              " fecha_conversion ",
                              " FROM   dis_cuenta ",
                              " WHERE  folio =","'", g_reg.folio,"'",
                              " AND   (subcuenta in(","'", subcta,"'",")",
                              " OR    subcuenta in(","'", subcta1,"'",")",
                              " OR    subcuenta in(","'", subcta2,"'","))",
                              " AND    tipo_movimiento = 1 ",
                              " GROUP BY 2 "
            PREPARE con_46 FROM  principal
            DECLARE rcv_19 CURSOR FOR con_46
            FOREACH rcv_19 INTO pesos,f_conversion
                LET g_reg.importe       = pesos
                LET g_reg.fecha_valor   = f_conversion
                LET g_reg.fecha_emision = f_conversion
                #LET g_reg.identificador = 2
                LET g_reg.identificador = 1

		LET i = 1
                LET max_fecha = f_conversion
                LET vdia      = DAY(max_fecha)
                LET max_fecha = max_fecha - vdia UNITS DAY
		FOR i = 1 TO 5
                   CALL habil_siguiente(max_fecha) RETURNING max_fecha
                END FOR
                LET g_reg.fecha_valor   = max_fecha
                LET g_reg.fecha_emision = max_fecha

                IF g_reg.importe > 0 THEN
                    CALL registra_historico(g_reg.*) #rh

                    LET g_reg.importe       = pesos
                    LET g_reg.identificador = 1

                    #CALL registra_historico(g_reg.*) #rh
                    #LET g_reg.importe       = g_reg.importe * -1 
                    #CALL registra_historico(g_reg.*) #rh
                END IF
            END FOREACH
         END IF
     END FOREACH
END FUNCTION

FUNCTION reverso_icefa()
#ri---------------------

    DEFINE vtransaccion     INTEGER
    DEFINE f_conversion     DATE
    DEFINE f_conversion_viv DATE
    DEFINE fhoy             DATE
    DEFINE fhoy1            DATE
    #DEFINE subcta          CHAR(05)
    DEFINE dia              SMALLINT
    DEFINE mes              SMALLINT
    DEFINE subcta           SMALLINT
    DEFINE subcta1          SMALLINT
    DEFINE subcta2          SMALLINT
    DEFINE tipo_mov         SMALLINT
    DEFINE principal        CHAR(350)
    DEFINE precio           DECIMAL(10,6)
    DEFINE importe          DECIMAL(15,2)
    DEFINE total_pesos      DECIMAL(15,2)
    DEFINE vviv_pesos       DECIMAL(15,2)
    DEFINE vsar_comision    DECIMAL(15,2)
    DEFINE vsar_comision_ant  DECIMAL(15,2)
    DEFINE vsar_pesos       DECIMAL(15,2)
    DEFINE vsar_pesos_ant   DECIMAL(15,2)
    DEFINE vsar_accion      DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10
    LET mes                   = 1
    LET vviv_pesos            = 0
    LET vsar_pesos            = 0
    LET vsar_pesos_ant        = 0
    LET vsar_accion           = 0


     DECLARE con_50 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_50 INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion
         LET  total_pesos = ""
         LET  precio      = ""
         LET  f_conversion = ""
         IF subcta = 8 THEN
             SELECT SUM(saldo_viv_92),
                    fecha_devol
             INTO   vviv_pesos,
                    f_conversion
             FROM   dev_det_normal
             WHERE  folio_devol  = g_reg.folio
             GROUP BY 2

             IF vviv_pesos IS NULL THEN
                 LET vviv_pesos = 0
             END IF

	     {
             LET fhoy = f_conversion + mes UNITS MONTH
             LET dia  = DAY(fhoy)
             LET fhoy = fhoy - dia UNITS DAY
             LET fhoy = fhoy + 1 UNITS DAY
             LET f_conversion_viv = fhoy
	     }

             LET g_reg.importe       = vviv_pesos * -1
             LET g_reg.fecha_valor   = f_conversion
             LET g_reg.fecha_emision = f_conversion
             LET g_reg.identificador = 1

             IF g_reg.importe <> 0 THEN
                  CALL registra_historico(g_reg.*) #rh
             END IF

         ELSE
             IF g_reg.transaccion_cod < 21515 THEN
                 SELECT SUM(saldo_sar_pagar),
                        ROUND(SUM(acciones_sar_92),2),
                        ROUND(SUM(saldo_sar_92_orig),2),
                        fecha_devol
                 INTO   vsar_pesos,
                        vsar_accion,
                        vsar_pesos_ant,
                        f_conversion
                 FROM   dev_det_normal
                 WHERE  folio_devol  = g_reg.folio
                 GROUP BY 4

                 IF vsar_pesos IS NULL THEN
                     LET vsar_pesos = 0
                 END IF
                 IF vsar_accion IS NULL THEN
                     LET vsar_accion = 0
                 END IF
                 IF vsar_pesos_ant IS NULL THEN
                     LET vsar_pesos_ant = 0
                 END IF

                 LET g_reg.importe       = vsar_accion * -1
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 2
	         LET vsar_comision       = 0

		 SELECT ROUND(SUM(monto_en_pesos),2)
		 INTO   vsar_comision
		 FROM   dis_cuenta
		 WHERE  folio = g_reg.folio
		 AND    tipo_movimiento = 110

                 IF STATUS = NOTFOUND THEN
                     LET vsar_comision = 0
                 END IF

                 IF g_reg.importe <> 0 THEN
                     IF g_reg.transaccion_cod <> 21512  THEN
                         CALL registra_historico(g_reg.*) #rh
        
                         LET g_reg.importe       = vsar_pesos * -1
                         LET g_reg.identificador = 1
	                 LET  g_reg.importe      = g_reg.importe +
					           vsar_comision

                         CALL registra_historico(g_reg.*) #rh
                     ELSE
                         LET g_reg.importe       = vsar_pesos_ant * -1
                         LET g_reg.identificador = 1

                         CALL registra_historico(g_reg.*) #rh
                     END IF
                 END IF
             ELSE
	         LET vsar_comision       = 0
	         LET vsar_comision_ant   = 0

                 SELECT fecha_devol
                 INTO   f_conversion
                 FROM   dev_det_normal
                 WHERE  folio_devol  = g_reg.folio
                 GROUP BY 1

                 IF g_reg.transaccion_cod = 21515 THEN
		     SELECT ROUND(SUM(comis_actual),2)
                     INTO   vsar_comision
                     FROM   safre_tmp:dev_detalle_comis 
		     WHERE  folio_devol = g_reg.folio

		     IF vsar_comision IS NULL THEN
		         LET vsar_comision = 0
		     END IF
                     LET g_reg.importe       = vsar_comision * -1
                 ELSE
		     SELECT ROUND(SUM(comis_anterior),2)
                     INTO   vsar_comision_ant
                     FROM   safre_tmp:dev_detalle_comis 
		     WHERE  folio_devol = g_reg.folio

		     IF vsar_comision_ant IS NULL THEN
		         LET vsar_comision_ant = 0
		     END IF
                     LET g_reg.importe       = vsar_comision_ant * -1
                 END IF
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 1

                 IF g_reg.importe <> 0 THEN
                      CALL registra_historico(g_reg.*) #rh
                 END IF
             END IF 
         END IF
     END FOREACH
END FUNCTION

FUNCTION intereses()
#i------------------

    DEFINE vtransaccion    INTEGER
    DEFINE xfolio          INTEGER
    DEFINE f_conversion    DATE
    DEFINE vfecha_sig      DATE
    DEFINE vfecha_fin      DATE
    DEFINE xfecha_ini      DATE
    DEFINE xfecha_fin      DATE
    DEFINE f_hoy           DATE
    DEFINE vid             CHAR(11)
    DEFINE dia             SMALLINT
    DEFINE vdia            SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE cont2           SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)
    DEFINE xpesos          DECIMAL(15,2)

    LET  g_reg.folio           = vfolio
    LET  g_reg.proceso_cod     = vproceso
    LET  g_reg.fecha_actualiza = HOY
    LET  g_reg.usuario         = vusuario
    LET  g_reg.estado          = 10

    LET  f_hoy       = ""
    LET  xfecha_ini  = ""
    LET  xfecha_fin  = ""
    LET  f_hoy       = HOY
    LET  dia         = DAY(f_hoy)
    LET  xfecha_fin  = f_hoy - dia UNITS DAY
    LET  dia         = DAY(xfecha_fin) -1
    LET  xfecha_ini  = xfecha_fin - dia UNITS DAY
    LET  total_pesos = ""
    LET  precio      = ""
    LET  f_conversion = vfecha

    DECLARE con_10 CURSOR FOR
        SELECT ROUND(SUM(monto_en_pesos),2),
               subcuenta,
               id_aportante
        FROM   safre_tmp:dis_cuenta_int
        WHERE  nss > 0
        AND    subcuenta in(4,8)
        AND    fecha_valor     <= vfecha
        AND    fecha_conversion = vfecha
        GROUP BY 2,3
    FOREACH con_10 INTO total_pesos,subcta,vid
        CASE subcta
            WHEN 4
                IF vid = "INFONAVIT" THEN
                    LET g_reg.transaccion_cod = 24500
		    LET xfolio = ""
		    LET xpesos = ""
		    SELECT folio
		    INTO   xfolio
		    FROM   con_transaccion
		    WHERE  proceso_cod  = "00003"
		    AND    fecha_emision BETWEEN xfecha_ini
					 AND     xfecha_fin
                    GROUP BY 1

		    SELECT SUM(monto_en_pesos)
		    INTO   xpesos
		    FROM   dis_provision
		    WHERE  folio           = xfolio
		    AND    subcuenta       = 4
		    AND    tipo_movimiento = 3

                    IF xpesos is null then
                        LET xpesos = 0
                    END IF
		    LET total_pesos = total_pesos - xpesos
                ELSE
                    LET g_reg.transaccion_cod = 24502
                END IF
            WHEN 8
                IF vid = "INFONAVIT" THEN
                    LET g_reg.transaccion_cod = 24510
                ELSE
                    LET g_reg.transaccion_cod = 24503
                END IF
        END CASE 

        LET g_reg.importe       = total_pesos
        LET g_reg.fecha_valor   = f_conversion
        LET g_reg.fecha_emision = f_conversion
        LET g_reg.identificador = 1

        LET vfecha_sig = f_conversion
        LET vfecha_fin = f_conversion
        LET vdia       = DAY(vfecha_sig)
        LET vfecha_sig = vfecha_sig - vdia UNITS DAY
        LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
        LET cont2 = 1
        FOR cont2 = 1 TO 5
            CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig
        END FOR
        LET vfecha_fin = vfecha_fin - 1 UNITS DAY
        CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin
        LET g_reg.fecha_emision = vfecha_sig
        LET g_reg.fecha_valor   = vfecha_sig

        IF g_reg.importe <> 0 THEN
            CALL registra_historico(g_reg.*) #rh
            LET g_reg.proceso_cod = "00027"
            LET g_reg.importe = g_reg.importe * -1
            LET g_reg.fecha_emision = vfecha_fin
            LET g_reg.fecha_valor   = vfecha_fin
            CALL registra_historico(g_reg.*) #rh
            LET g_reg.proceso_cod = vproceso
        END IF
    END FOREACH

END FUNCTION

FUNCTION icefa_afore()
#ia-------------------
    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(500)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos1    DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


    DECLARE con_52 CURSOR FOR

        SELECT transaccion_cod,
               subcuenta,
               subcuenta1,
               subcuenta2
        FROM   tab_transaccion
        WHERE  proceso_cod = g_reg.proceso_cod
        AND    descripcion_1 NOT MATCHES "*FRACCION"

    FOREACH con_52 INTO vtransaccion,subcta,subcta1,subcta2

        LET  g_reg.transaccion_cod = vtransaccion
        LET  total_pesos = ""
        LET  total_pesos1 = ""
        LET  precio      = ""
        LET  f_conversion = ""

        IF  subcta = 8  THEN
            LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                             " fecha_conversion ",
                             " FROM   dis_cuenta ",
                             " WHERE  folio =","'", g_reg.folio,"'",
                             " AND  (subcuenta in(","'", subcta,"'",")",
                             " OR   subcuenta in(","'", subcta1,"'","))",
                             " AND    tipo_movimiento not in(888,999) ",
                             " GROUP BY 2 "
            PREPARE con_60 FROM  principal
            DECLARE viv_15 CURSOR FOR con_60
            FOREACH viv_15 INTO total_pesos,f_conversion
            END FOREACH

            LET g_reg.importe       = total_pesos
            LET g_reg.fecha_valor   = f_conversion
            LET g_reg.fecha_emision = f_conversion
            LET g_reg.identificador = 1

            IF g_reg.importe <> 0 THEN
                CALL registra_historico(g_reg.*) #rh
            END IF

        ELSE
            CASE vtransaccion
                WHEN 90091
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  #" a.precio_accion, ",
                                  " ROUND(SUM(a.monto_en_pesos),2), ",
                                  " a.fecha_conversion ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.subcuenta = 7 ",
                                  " AND    a.tipo_movimiento = 3 ",
                                  " GROUP BY 3 "
                    PREPARE con_61 FROM  principal
                    DECLARE rcv_20 CURSOR FOR con_61
                    #FOREACH rcv_20 INTO total_pesos,precio,f_conversion
                    FOREACH rcv_20 INTO total_pesos,total_pesos1,f_conversion
                    END FOREACH

                WHEN 90092
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  #" a.precio_accion, ",
                                  " ROUND(SUM(a.monto_en_pesos),2), ",
                                  " a.fecha_conversion ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.subcuenta = 7 ",
                                  " AND    a.tipo_movimiento = 5 ",
                                  " GROUP BY 3 "
                    PREPARE con_62 FROM  principal
                    DECLARE rcv_21 CURSOR FOR con_62
                    #FOREACH rcv_21 INTO total_pesos,precio,f_conversion
                    FOREACH rcv_21 INTO total_pesos,total_pesos1,f_conversion
                    END FOREACH

                 OTHERWISE
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                   #" precio_accion, ",
                                   " ROUND(SUM(monto_en_pesos),2), ",
                                   " fecha_conversion ",
                                   " FROM   dis_cuenta ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND    subcuenta = 7 ",
                                   " AND    tipo_movimiento in(1,4) ",
                                   " GROUP BY 3 "
                     PREPARE con_63 FROM  principal
                     DECLARE rcv_22 CURSOR FOR con_63
                     #FOREACH rcv_22 INTO total_pesos,precio,f_conversion
                     FOREACH rcv_22 INTO total_pesos,total_pesos1,f_conversion
                     END FOREACH

              END CASE
              LET g_reg.importe       = total_pesos
              LET g_reg.fecha_valor   = f_conversion
              LET g_reg.fecha_emision = f_conversion
              LET g_reg.identificador = 2

              IF g_reg.importe <> 0 THEN
                  CALL registra_historico(g_reg.*) #rh

                  #LET g_reg.importe       = total_pesos * precio
                  LET g_reg.importe       = total_pesos1
                  LET g_reg.identificador = 1

                  CALL registra_historico(g_reg.*) #rh
              END IF

         END IF
         
    END FOREACH
END FUNCTION                                                         
FUNCTION interes_transito()
#ia-------------------
    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(500)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos1    DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


    DECLARE con_53 CURSOR FOR

        SELECT transaccion_cod,
               subcuenta,
               subcuenta1,
               subcuenta2
        FROM   tab_transaccion
        WHERE  proceso_cod = g_reg.proceso_cod
        AND    descripcion_1 NOT MATCHES "*FRACCION"

    FOREACH con_53 INTO vtransaccion,subcta,subcta1,subcta2

        LET  g_reg.transaccion_cod = vtransaccion
        LET  total_pesos = ""
        LET  total_pesos1 = ""
        LET  precio      = ""
        LET  f_conversion = ""

            CASE vtransaccion
                WHEN 21001
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  " ROUND(SUM(a.monto_en_pesos),2), ",
                                  " a.fecha_conversion ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.subcuenta = 1 ",
                                  " AND    a.tipo_movimiento = 3 ",
                                  " GROUP BY 3 "
                    PREPARE con_64 FROM  principal
                    DECLARE rcv_23 CURSOR FOR con_64
                    FOREACH rcv_23 INTO total_pesos,total_pesos1,f_conversion
                    END FOREACH

                WHEN 22001
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  " ROUND(SUM(a.monto_en_pesos),2), ",
                                  " a.fecha_conversion ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.subcuenta = 2 ",
                                  " AND    a.tipo_movimiento = 3 ",
                                  " GROUP BY 3 "
                    PREPARE con_65 FROM  principal
                    DECLARE rcv_24 CURSOR FOR con_65
                    FOREACH rcv_24 INTO total_pesos,total_pesos1,f_conversion
                    END FOREACH

                 WHEN 26003
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                   " ROUND(SUM(monto_en_pesos),2), ",
                                   " fecha_conversion ",
                                   " FROM   dis_cuenta ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND    subcuenta = 3 ",
                                   " AND    tipo_movimiento = 3  ",
                                   " GROUP BY 3 "
                     PREPARE con_66 FROM  principal
                     DECLARE rcv_25 CURSOR FOR con_66
                     FOREACH rcv_25 INTO total_pesos,total_pesos1,f_conversion
                     END FOREACH

              END CASE
              LET g_reg.importe       = total_pesos
              LET g_reg.fecha_valor   = f_conversion
              LET g_reg.fecha_emision = f_conversion
              LET g_reg.identificador = 2

              IF g_reg.importe <> 0 THEN
                  CALL registra_historico(g_reg.*) #rh

                  #LET g_reg.importe       = total_pesos * precio
                  LET g_reg.importe       = total_pesos1
                  LET g_reg.identificador = 1

                  CALL registra_historico(g_reg.*) #rh
              END IF

         
    END FOREACH
END FUNCTION                                                         
FUNCTION devolucion_pagos()
#dp------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE vf_conversion   DATE
    DEFINE f_conversion    DATE
    DEFINE f_proceso       DATE
    #DEFINE subcta          CHAR(05)
    DEFINE vcomision       SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    #DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos1    DECIMAL(22,6)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


     DECLARE con_79 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_79 INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_acciones = ""
         LET  total_pesos    = ""
         LET  precio         = ""
         LET  f_conversion   = ""
         LET  f_proceso      = ""

	 SELECT fecha_conversion
	 INTO   vf_conversion
	 FROM   dis_cuenta
	 WHERE  folio = g_reg.folio
	 GROUP BY 1
         IF subcta = 4 THEN
              LET principal = " SELECT SUM(ROUND(monto_en_pesos,2)),",
                              " fecha_conversion ",
                              " FROM   dis_provision ",
                              " WHERE  folio =","'", g_reg.folio,"'",
                              " AND  (subcuenta in(","'", subcta,"'",")",
                              " OR   subcuenta in(","'", subcta1,"'","))",
                              " GROUP BY 2 "
              PREPARE con_73 FROM  principal
              DECLARE viv_17 CURSOR FOR con_73
              FOREACH viv_17 INTO total_pesos,f_conversion
              END FOREACH

              LET g_reg.importe       = total_pesos
              LET g_reg.fecha_emision = vf_conversion
              LET g_reg.fecha_valor   = vf_conversion
              LET g_reg.identificador = 1

              IF g_reg.importe <> 0 THEN
                  CALL registra_historico(g_reg.*) #rh
              END IF

         ELSE
            CASE vtransaccion
                WHEN 21150
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  " ROUND(SUM(a.monto_en_pesos),2), ",
                                  " a.fecha_conversion ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.subcuenta = 1 ",
                                  #" AND    a.tipo_movimiento = 540 ",
                                  " AND a.tipo_movimiento between 540 and 555 ",
                                  " GROUP BY 3 "
                    PREPARE con_74 FROM  principal
                    DECLARE rcv_32 CURSOR FOR con_74
                    FOREACH rcv_32 INTO total_acciones,total_pesos,f_conversion
                    END FOREACH

                WHEN 21151
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  " ROUND(SUM(a.monto_en_pesos),2), ",
                                  " a.fecha_conversion ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.subcuenta in(2,6,9) ",
                                  " AND a.tipo_movimiento between 540 and 555 ",
                                  " GROUP BY 3 "
                    PREPARE con_75 FROM  principal
                    DECLARE rcv_33 CURSOR FOR con_75
                    FOREACH rcv_33 INTO total_acciones,total_pesos,f_conversion
                    END FOREACH

                 WHEN 21152
                    LET  principal = " SELECT SUM(monto_ret+monto_act_ret) ",
                                   " FROM   exc_det_exceso ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND    result_operacion = 01 "
                     PREPARE con_76 FROM  principal
                     DECLARE rcv_34 CURSOR FOR con_76
                     FOREACH rcv_34 INTO total_pesos
                     END FOREACH

		     LET total_pesos1 = 0

		     SELECT SUM(ROUND(monto_comi_ret,2))
		     INTO   total_pesos1
		     FROM   exc_exceso_comis
		     WHERE  folio    = g_reg.folio

		     IF total_pesos1 IS NULL THEN
			 LET total_pesos1 = 0
                     END IF
		     LET total_pesos = total_pesos + total_pesos1
		     LET total_pesos = total_pesos * -1

                 WHEN 21153
                    LET  principal = " SELECT SUM(monto_ces_vej_pat+monto_act_ces_vej) ",
                                   " FROM   exc_det_exceso ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND    result_operacion = 01 "
                     PREPARE con_77 FROM  principal
                     DECLARE rcv_35 CURSOR FOR con_77
                     FOREACH rcv_35 INTO total_pesos
                     END FOREACH

		     LET total_pesos1 = 0

		     SELECT SUM(ROUND(monto_comi_ces_vej,2))
		     INTO   total_pesos1
		     FROM   exc_exceso_comis
		     WHERE  folio    = g_reg.folio

		     IF total_pesos1 IS NULL THEN
			 LET total_pesos1 = 0
                     END IF
		     LET total_pesos = total_pesos + total_pesos1
		     LET total_pesos = total_pesos * -1
              END CASE
              LET g_reg.importe       = total_pesos
              LET g_reg.fecha_valor   = vf_conversion
              LET g_reg.fecha_emision = vf_conversion
              LET g_reg.identificador = 1

              IF g_reg.importe <> 0 THEN
                  CALL registra_historico(g_reg.*) #rh

                  LET g_reg.importe       = total_acciones
                  LET g_reg.identificador = 2
		  IF  (g_reg.transaccion_cod = 21152 OR
		       g_reg.transaccion_cod = 21153) THEN
                  ELSE
                      CALL registra_historico(g_reg.*) #rh
                  END IF
              END IF

         END IF
         
     END FOREACH
END FUNCTION
FUNCTION registra_historico(g_reg5)
#rp----------------------------
    DEFINE g_reg5  RECORD 
        folio             INTEGER,
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
        fecha_actualiza   DATE,
	usuario           CHAR(08),
        estado            SMALLINT
    END RECORD

    LET   g_reg5.estado  = 10

    IF (g_reg5.proceso_cod = "00016" OR
        g_reg5.proceso_cod = "00021" OR
        g_reg5.proceso_cod = "00022" OR
        g_reg5.proceso_cod = "00026" OR
        g_reg5.proceso_cod = "00027") THEN
        LET   g_reg5.estado  = 20
    END IF
    {
    IF g_reg5.proceso_cod = "00002" THEN
        LET g_reg5.proceso_cod = "00001"
    END IF
    IF g_reg5.proceso_cod = "00004" THEN
        LET g_reg5.proceso_cod = "00003"
    END IF
    }
       
    INSERT INTO con_transaccion VALUES(g_reg5.*)
                                       #g_reg5.folio,
                                       #g_reg5.fecha_emision,
                                       #g_reg5.fecha_valor,
                                       #g_reg5.identificador,
                                       #g_reg5.transaccion_cod,
                                       #g_reg5.importe,
                                       #g_reg5.proceso_cod,
                                       #g_reg5.fecha_actualiza,
                                       #g_reg5.usuario,
                                       #g_reg5.estado
                                       #)

END FUNCTION

FUNCTION  despliega_tipo()
    DEFINE pos        INTEGER
    DEFINE cla_where  CHAR(200)
    DEFINE sel_where  CHAR(200)
    DEFINE l_record  ARRAY[1000] OF RECORD
        codigo         CHAR(05),
        descripcion    CHAR(80)
    END RECORD

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,18 WITH FORM "CONC0012" ATTRIBUTE( BORDER)
      DISPLAY "    (Enter) Seleccionar                (Ctrl-C) Salir      " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "        Escoja con < ENTER > seleccionar el tipo           " AT 2,1
      DISPLAY "                                                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where   ON proceso_cod 
                          FROM proceso_cod

         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_proceso WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
  
      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1) 
          DISPLAY ARRAY l_record TO scr_1.* 

             ON KEY (CONTROL-M)
                LET pos = ARR_CURR()
                LET vproceso     = l_record[pos].codigo
                LET vdescripcion = l_record[pos].descripcion
                EXIT DISPLAY

             ON KEY (CONTROL-C)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

             ON KEY (INTERRUPT)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "EL PROCESO NO EXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
       END IF

    END IF

    RETURN l_record[pos].codigo, l_record[pos].descripcion
END FUNCTION

REPORT listado_3(reg_3)
#l3--------------------
        DEFINE reg_3 RECORD #glo #reg_3
            fecha_emision     DATE,
            fecha_valor       DATE,
            identificador     SMALLINT,
            transaccion_cod   INTEGER,
            importe           DECIMAL(15,2),
            proceso_cod       CHAR(05)
        END RECORD
   
    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,"001",                                 #origen
            COLUMN 003,",",                                   #separador
            COLUMN 004,reg_3.fecha_emision USING"MM/DD/YYYY", #fecha_emision
            COLUMN 015,",",                                   #separador
            COLUMN 016,reg_3.fecha_valor   USING"MM/DD/YYYY", #fecha_emision
            COLUMN 026,",",                                   #separador
            COLUMN 027,reg_3.identificador USING"&",          #identificador
            COLUMN 028,",",                                   #separador
            COLUMN 029,reg_3.transaccion_cod USING"&&&&&",    #transaccion
            COLUMN 034,",",                                   #separador
            COLUMN 035,reg_3.importe*100 USING"&&&&&&&&&&&&&&&",  #importe
            COLUMN 050,",",                                   #separador
            COLUMN 051,reg_3.proceso_cod,                     #proceso
            COLUMN 056,",",                                   #separador
            COLUMN 057,"0000000000",
            COLUMN 067,",",                                   #separador
            COLUMN 068,"0",
            COLUMN 069,",",                                   #separador
            COLUMN 070,"0",
            COLUMN 071,",",                                   #separador
            COLUMN 072,"0"
END REPORT

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------
   DEFINE diaTmp	DATE,
   	  contador	SMALLINT,
	  diaActual	DATE
   
   DEFINE diaHabilSig	DATE,
	  diaSemana	SMALLINT,
	  feriado	SMALLINT,
	  finSemana	SMALLINT
	  

   LET diaHabilSig = diaActual + 1 UNITS DAY

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

END FUNCTION #habil_siguiente

FUNCTION habil_anterior(diaActual)
#ha--------------------------------

   DEFINE diaTmp	      DATE,
   	    contador	   SMALLINT,
	       diaActual	   DATE,
          diaHabilAnt	DATE,
	       diaSemana	   SMALLINT,
	       feriado	      SMALLINT,
	       finSemana	   SMALLINT

   LET diaHabilAnt = diaActual

	WHILE TRUE
   	    LET feriado   = 0
   	    LET finSemana = 0
   	    LET diaSemana = WEEKDAY(diaHabilAnt)  

   	    IF diaSemana = 0 OR diaSemana = 6 THEN
      	        LET finSemana = 1
   	    END IF
  	     
   	    SELECT *
   	    FROM   tab_feriado 
   	    WHERE  feria_fecha = diaHabilAnt
    	
   	    IF STATUS <> NOTFOUND THEN
       	        LET feriado = 1
   	    END IF 
		
   	    IF feriado = 1 OR finSemana = 1 THEN
       	        LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
   	    ELSE
       	        EXIT WHILE
   	    END IF
	END WHILE

	RETURN diaHabilAnt
END FUNCTION #habil_anterior
FUNCTION concilia()
#co----------------------
    DEFINE vaccion_safre     DECIMAL(15,2)
    DEFINE vprecio_safre     DECIMAL(15,2)
    DEFINE vaccion_tesoreria DECIMAL(15,2)
    DEFINE vprecio_tesoreria DECIMAL(15,2)
    DEFINE vprecio           DECIMAL(10,6)
    DEFINE vdiferencia       DECIMAL(10,2)
    DEFINE vdif_concilia     DECIMAL(5,2)
    DEFINE vdif_fraccion     DECIMAL(5,2)
    DEFINE vneteo            CHAR(05)
    DEFINE vacciones         SMALLINT
    DEFINE vpesos            SMALLINT
    DEFINE vmovimiento       SMALLINT
    DEFINE ventero           INTEGER
    DEFINE folio_traspaso    INTEGER

    LET vproceso       = ""
    LET vdescripcion   = ""
    LET vfolio         = ""
    LET vfecha         = ""
    LET vneteo         = "00028"
    LET vacciones      = 2
    LET vpesos         = 1

    OPEN WINDOW ventana_3 AT 5,3 WITH FORM "CONC0014" ATTRIBUTE(BORDER)
    DISPLAY "          [ Esc ] Iniciar                            [ Ctrl-C ] Salir          " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "CONC001         CONCILIACION ENTRE TESORERIA Y SAFRE                           " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vproceso,
                  vdescripcion,
                  vfolio,
                  vfecha,
                  vaccion_safre,
                  vprecio_safre,
                  vaccion_tesoreria,
                  vprecio_tesoreria,
                  vdif_concilia  WITHOUT DEFAULTS

      AFTER  FIELD vproceso

         IF vproceso IS NULL THEN
             CALL despliega_tipo()
             RETURNING vproceso,vdescripcion
         ELSE
             SELECT "X"
             FROM   tab_proceso
             WHERE  proceso_cod = vproceso

             IF STATUS <> NOTFOUND THEN
                 SELECT descripcion
                 INTO   vdescripcion
                 FROM   tab_proceso
                 WHERE  proceso_cod = vproceso
             ELSE
                 ERROR "No existe este tipo de proceso" 
                 SLEEP 3
                 NEXT FIELD vproceso
             END IF
         END IF
         DISPLAY "DESCRIPCION DEL PROCESO" AT  5,23 ATTRIBUTE(REVERSE)
         DISPLAY BY NAME vproceso,
                         vdescripcion

         IF vproceso >= "00001"  AND
	    vproceso <= "00004"  THEN

             SELECT MAX(folio)
	     INTO   vfolio
	     FROM   con_transaccion
	     WHERE  proceso_cod = vproceso
	     AND    estado = 10

	     IF STATUS = NOTFOUND THEN
                 LET vfolio = 0
	     END IF
             DISPLAY BY NAME vfolio
             NEXT FIELD vfolio
         ELSE
	     NEXT FIELD vfecha
         END IF

      AFTER  FIELD vfolio

         IF (vfolio IS NULL OR 
	     vfolio = 0 ) THEN
             ERROR "El folio no puede ser nulo o cero"
             SLEEP 2
             ERROR ""
             NEXT FIELD vfolio
         ELSE
             SELECT "X"
             FROM   con_transaccion
             WHERE  proceso_cod   = vproceso
	     AND    folio         = vfolio
             AND    estado        = 10
             GROUP BY 1
             IF STATUS = NOTFOUND THEN
		 ERROR "El folio no existe o ya esta conciliado"
		 SLEEP 2
		 ERROR ""
		 NEXT FIELD vfolio
             END IF
         END IF

      AFTER  FIELD vfecha

         IF vfecha IS NULL THEN
             ERROR "La fecha no puede ser nula"
             SLEEP 2
             ERROR ""
             NEXT FIELD vfecha
         ELSE
             SELECT "X"
             FROM   con_transaccion
             WHERE  proceso_cod = vproceso
             AND    fecha_emision = vfecha
             AND    estado = 10
             GROUP BY 1

             IF STATUS = NOTFOUND THEN
                 ERROR "No existe proceso por conciliar" 
                 SLEEP 3
                 NEXT FIELD vfecha
             END IF
         END IF

         SELECT movimiento
         INTO   vmovimiento
         FROM   tab_proceso
         WHERE  proceso_cod = vproceso

         IF vmovimiento = -1 THEN

             SELECT SUM(importe)
             INTO   vprecio_safre
             FROM   con_transaccion
             WHERE  fecha_emision = vfecha
             AND    identificador = vpesos
             AND    proceso_cod   = vproceso
             AND    transaccion_cod NOT IN(
                                       SELECT transaccion_cod
                                       FROM   tab_transaccion
                                       WHERE  (descripcion_1 MATCHES "COMIS*"
                                       OR    descripcion_1 MATCHES "*VIV*"))

             IF (vproceso = "00023" OR
                 vproceso = "00012") THEN

                 LET   vprecio_safre = 0

                 SELECT SUM(importe)
                 INTO   vprecio_safre
                 FROM   con_transaccion
                 WHERE  fecha_emision = vfecha
                 AND    identificador = vpesos
                 AND    proceso_cod   = vproceso
                 AND    transaccion_cod NOT IN(
                                       SELECT transaccion_cod
                                       FROM   tab_transaccion
                                       WHERE  (descripcion_1 MATCHES "COMIS*"
                                       OR    descripcion_1 MATCHES "*VIV*"))
                 #AND    transaccion_cod not in(21200,91001,91002,91003)
                 AND    transaccion_cod not in(21200,91001,91002,
					       91003,21152,21153)
             END IF
             IF vproceso = "00011" THEN

                 LET   vprecio_safre = 0

                 SELECT SUM(importe)
                 INTO   vprecio_safre
                 FROM   con_transaccion
                 WHERE  fecha_emision = vfecha
                 AND    identificador = vpesos
                 AND    proceso_cod   = vproceso
                 AND    transaccion_cod in(21510)

             END IF
             IF vprecio_safre IS NULL THEN
                 ERROR "No hay movimiento de acciones en el proceso"
                 SLEEP 3
                 ERROR ""
                 NEXT FIELD vproceso
             END IF

             #DISPLAY BY NAME vaccion_safre
             DISPLAY BY NAME vprecio_safre

                 SELECT SUM(importe)
                 INTO   vaccion_safre
                 FROM   con_transaccion
                 WHERE  fecha_emision = vfecha
                 AND    identificador = 2
                 AND    proceso_cod   = vproceso
                 AND    transaccion_cod NOT IN(
                                       SELECT transaccion_cod
                                       FROM   tab_transaccion
                                       WHERE  (descripcion_1 MATCHES "COMIS*"
                                       OR    descripcion_1 MATCHES "VIV*"))

                 IF (vproceso = "00023" OR
                     vproceso = "00012") THEN

                     LET   vaccion_safre = 0
    
                     SELECT SUM(importe)
                     INTO   vaccion_safre
                     FROM   con_transaccion
                     WHERE  fecha_emision = vfecha
                     AND    identificador = 2
                     AND    proceso_cod   = vproceso
                     AND    transaccion_cod NOT IN(
                                       SELECT transaccion_cod
                                       FROM   tab_transaccion
                                       WHERE  (descripcion_1 MATCHES "COMIS*"
                                       OR    descripcion_1 MATCHES "*VIV*"))
                     #AND    transaccion_cod not in(21200,91001,91002,91003)
                     AND    transaccion_cod not in(21200,91001,91002,
					           91003,21152,21153)

                 END IF

                 IF vaccion_safre IS NULL THEN
                     ERROR "No hay movimiento de pesos en el proceso"
                     SLEEP 3
                     ERROR ""
                     NEXT FIELD vproceso
                 END IF

                 DISPLAY BY NAME vaccion_safre
                 INPUT BY NAME vprecio_tesoreria,
                               vaccion_tesoreria,
                               vdif_concilia
            

              AFTER  FIELD vprecio_tesoreria

              IF (vprecio_tesoreria IS NULL OR
                  vprecio_tesoreria = 0)    THEN
                  ERROR "El total de pesos no puede ser cero o nulo"
                  SLEEP 2
                  ERROR ""
                  NEXT FIELD vfecha
              ELSE

                  SELECT precio_del_dia
                  INTO   vprecio
                  FROM   glo_valor_accion
                  WHERE  fecha_valuacion = vfecha

                  LET vaccion_tesoreria = vprecio / vprecio_tesoreria

                  DISPLAY BY NAME vaccion_tesoreria

                  IF vprecio_safre < 0 THEN
                      IF vprecio_tesoreria > 0 THEN
		          LET vprecio_tesoreria = vprecio_tesoreria * -1
                          DISPLAY BY NAME vprecio_tesoreria
                      END IF
                  END IF
                  IF vprecio_safre > 0 THEN
                      IF vprecio_tesoreria < 0 THEN
		          LET vprecio_tesoreria = vprecio_tesoreria * -1
                          DISPLAY BY NAME vprecio_tesoreria
                      END IF
                  END IF
                  LET vdiferencia   = ""
                  LET vdiferencia   = vprecio_tesoreria - vprecio_safre
		  IF vdiferencia > 3 THEN
		      ERROR "La diferencia no puede ser mayor a 3 pesos"
		      SLEEP 3
		      EXIT PROGRAM
                  END IF
		  IF vdiferencia < -3 THEN
		      ERROR "La diferencia no puede ser mayor a 3 pesos"
		      SLEEP 3
		      EXIT PROGRAM
                  END IF
                  LET vdif_concilia = vprecio_tesoreria - vprecio_safre
                  WHILE TRUE
                      PROMPT "Desea Conciliar el Registro S/N ? "
                      FOR CHAR aux_pausa
                      IF aux_pausa MATCHES "[SsNn]" THEN
                          EXIT WHILE
                      END IF
                  END WHILE
                  IF aux_pausa MATCHES "[Ss]" THEN
                      IF vdif_concilia <> 0 THEN
                          CALL ingresa_ajuste(vfecha,vproceso,
                                              vdif_concilia,1)#ia
                      END IF

                  ELSE
                      ERROR "CONCILIACION CANCELADA"
                      SLEEP 2
                      ERROR ""
                      #EXIT PROGRAM
                      EXIT INPUT
                  END IF
                  DISPLAY BY NAME vdif_concilia
                  DISPLAY "  Pesos  " AT 11,56 ATTRIBUTE(REVERSE)
                  LET ventero      = vaccion_safre
                  LET ventero      = ventero + (1 * vmovimiento)
                  LET vdif_fraccion = ventero - vaccion_safre
                  IF vdif_fraccion <> 0 THEN
                      CALL ingresa_fraccion(vfecha,vproceso,vdif_fraccion,2)#ia
                  END IF

                  UPDATE con_transaccion
                  SET    estado = 20
                  WHERE  proceso_cod = vproceso
                  AND    fecha_emision = vfecha

                  IF vproceso = "00010" THEN

#arc hasta aqui
                      SELECT "X"
		      FROM   con_transaccion
		      WHERE  proceso_cod   = vproceso
		      AND    fecha_emision = vfecha
		      AND    estado        = 10
		      GROUP BY 1

		      IF STATUS = NOTFOUND THEN
                          SELECT "X"
		          FROM   con_transaccion
		          WHERE  proceso_cod   = "00009"
		          AND    fecha_emision = vfecha
		          AND    estado        = 10
		          GROUP BY 1
    
		          IF STATUS = NOTFOUND THEN
                              SELECT "X"
		              FROM   con_transaccion
		              WHERE  proceso_cod   = "00009"
		              AND    fecha_emision = vfecha
		              AND    estado        = 20
		              GROUP BY 1
    
		              IF STATUS <> NOTFOUND THEN
		                  ERROR "Registrando neteo de traspaso"
			          SLEEP 2
			          CALL registra_neteo(vfecha,vneteo)
                              ELSE
		                  ERROR "Falta conciliar el Traspaso A-A Ced."
		                  SLEEP 2
                              END IF
                          ELSE
		              ERROR "Falta conciliar el Traspaso A-A Cedente"
		              SLEEP 2
                          END IF
                      ELSE
                          SELECT folio
			  INTO   folio_traspaso
		          FROM   con_transaccion
		          WHERE  proceso_cod   = vproceso
		          AND    fecha_emision = vfecha
		          AND    estado        = 10
		          GROUP BY 1

		          ERROR "Falta conciliar folio Traspaso A-A Receptora ",
				 folio_traspaso CLIPPED
		          SLEEP 2
                      END IF
                  END IF
             END IF
             ERROR" REGISTRO INGRESADO "
	          SLEEP 3
	          EXIT INPUT
	          END INPUT
	          ERROR ""
         ELSE
             SELECT SUM(importe)
             INTO   vaccion_safre
             FROM   con_transaccion
             WHERE  fecha_emision = vfecha
             AND    identificador = vacciones
             AND    proceso_cod   = vproceso
             AND    transaccion_cod NOT IN(
                                       SELECT transaccion_cod
                                       FROM   tab_transaccion
                                       WHERE  (descripcion_1 MATCHES "COMIS*"
                                       OR    descripcion_1 MATCHES "VIV*"))
             IF vaccion_safre IS NULL THEN
                 ERROR "No hay movimiento de acciones en el proceso"
                 SLEEP 3
                 ERROR ""
                 NEXT FIELD vproceso
             END IF

             DISPLAY BY NAME vaccion_safre

                 SELECT SUM(importe)
                 INTO   vprecio_safre
                 FROM   con_transaccion
                 WHERE  fecha_emision = vfecha
                 AND    identificador = vpesos
                 AND    proceso_cod   = vproceso
                 AND    transaccion_cod NOT IN(
                                       SELECT transaccion_cod
                                       FROM   tab_transaccion
                                       WHERE  (descripcion_1 MATCHES "COMIS*"
                                       OR    descripcion_1 MATCHES "*VIV*"))

                 IF vprecio_safre IS NULL THEN
                     ERROR "No hay movimiento de pesos en el proceso"
                     SLEEP 3
                     ERROR ""
                     NEXT FIELD vproceso
                 END IF

                 DISPLAY BY NAME vprecio_safre
                 INPUT BY NAME vaccion_tesoreria,
                               vprecio_tesoreria,
                               vdif_concilia
            

              AFTER  FIELD vaccion_tesoreria

              IF (vaccion_tesoreria IS NULL OR
                  vaccion_tesoreria = 0)    THEN
                  ERROR "El total de acciones no puede ser cero o nulo"
                  SLEEP 2
                  ERROR ""
                  NEXT FIELD vfecha
              ELSE

{
                  SELECT precio_del_dia
                  INTO   vprecio
                  FROM   glo_valor_accion
                  WHERE  fecha_valuacion = vfecha
}

                  #LET vprecio_tesoreria = vprecio / vaccion_tesoreria
                  LET vprecio_tesoreria = 0

                  DISPLAY BY NAME vprecio_tesoreria
                  IF vaccion_safre < 0 THEN
                      IF vaccion_tesoreria > 0 THEN
		          LET vaccion_tesoreria = vaccion_tesoreria * -1
                          DISPLAY BY NAME vaccion_tesoreria
                      END IF
                  END IF
                  IF vaccion_safre > 0 THEN
                      IF vaccion_tesoreria < 0 THEN
		          LET vaccion_tesoreria = vaccion_tesoreria * -1
                          DISPLAY BY NAME vaccion_tesoreria
                      END IF
                  END IF

                  LET vdiferencia   = ""
                  LET vdiferencia = vaccion_tesoreria - vaccion_safre
		  IF vdiferencia > 3 THEN
		      ERROR "La diferencia no puede ser mayor a 3 titulos"
		      SLEEP 3
		      EXIT PROGRAM
                  END IF
		  IF vdiferencia < -3 THEN
		      ERROR "La diferencia no puede ser mayor a 3 titulos"
		      SLEEP 3
		      EXIT PROGRAM
                  END IF
                  LET vdif_concilia = vaccion_tesoreria - vaccion_safre
                  WHILE TRUE
                      PROMPT "Desea Conciliar el Registro S/N ? "
                      FOR CHAR aux_pausa
                      IF aux_pausa MATCHES "[SsNn]" THEN
                          EXIT WHILE
                      END IF
                  END WHILE
                  IF aux_pausa MATCHES "[Ss]" THEN

                      IF vdif_concilia <> 0 THEN
                         CALL ingresa_ajuste(vfecha,vproceso,vdif_concilia,2)#ia
                      END IF

                  ELSE
                      ERROR "CONCILIACION CANCELADA"
                      SLEEP 2
                      ERROR ""
                      #EXIT PROGRAM
                      EXIT INPUT
                  END IF

                  DISPLAY BY NAME vdif_concilia

                  LET ventero      = vaccion_safre
                  LET ventero      = ventero + (1 * vmovimiento)
                  #LET vdif_fraccion = ventero - vaccion_safre
                  LET vdif_fraccion = ventero - vaccion_tesoreria

                  IF vdif_fraccion <> 0 THEN
                      CALL ingresa_fraccion(vfecha,vproceso,vdif_fraccion,2)#ia
                  END IF

                  UPDATE con_transaccion
                  SET    estado = 20
                  WHERE  proceso_cod = vproceso
                  AND    fecha_emision = vfecha

                  IF vproceso = "00009" THEN

#arc hasta aqui
                      SELECT "X"
		      FROM   con_transaccion
		      WHERE  proceso_cod   = vproceso
		      AND    fecha_emision = vfecha
		      AND    estado        = 10
		      GROUP BY 1

		      IF STATUS = NOTFOUND THEN
                          SELECT "X"
		          FROM   con_transaccion
		          WHERE  proceso_cod   = "00010"
		          AND    fecha_emision = vfecha
		          AND    estado        = 10
		          GROUP BY 1
    
		          IF STATUS = NOTFOUND THEN
                              SELECT "X"
		              FROM   con_transaccion
		              WHERE  proceso_cod   = "00010"
		              AND    fecha_emision = vfecha
		              AND    estado        = 20
		              GROUP BY 1
    
		              IF STATUS <> NOTFOUND THEN
		                  ERROR "Registrando neteo de traspaso"
			          SLEEP 2
			          CALL registra_neteo(vfecha,vneteo)
                              ELSE
		                  ERROR "Falta conciliar el Traspaso A-A Ced."
		                  SLEEP 2
                              END IF
                          ELSE
		              ERROR "Falta conciliar el Traspaso A-A Cedente"
		              SLEEP 2
                          END IF
                      ELSE
                          SELECT folio
			  INTO   folio_traspaso
		          FROM   con_transaccion
		          WHERE  proceso_cod   = vproceso
		          AND    fecha_emision = vfecha
		          AND    estado        = 10
		          GROUP BY 1

		          ERROR "Falta conciliar folio Traspaso A-A Receptora ",
				 folio_traspaso CLIPPED
		          SLEEP 2
                      END IF
                  END IF
             END IF
             ERROR" REGISTRO INGRESADO "
	          SLEEP 3
	          EXIT INPUT
	          END INPUT
	          ERROR ""
      END IF
      EXIT INPUT

      ON KEY(CONTROL-C)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT INPUT

      ON KEY(INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT INPUT

    END INPUT 
    CLOSE WINDOW ventana_3

END FUNCTION
FUNCTION ingresa_ajuste(reg_a)
#ia---------------------------
    DEFINE reg_a   RECORD 
        fecha             DATE,
        proceso           CHAR(05),
        diferencia        DECIMAL(15,2),
        tipo              SMALLINT
    END RECORD
    DEFINE g_reg5  RECORD 
        folio             INTEGER,
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
        fecha_actualiza   DATE,
	     usuario           CHAR(08),
        estado            SMALLINT
    END RECORD
    DEFINE  vprecio       DECIMAL(10,6)
    DEFINE  vtransaccion  INTEGER
    DEFINE  xfolio        INTEGER

        SELECT MIN(transaccion_cod)
        INTO   vtransaccion
        FROM   tab_transaccion
        WHERE  proceso_cod = reg_a.proceso

        SELECT MAX(folio)
        INTO   xfolio
        FROM   con_transaccion
        WHERE  proceso_cod     = reg_a.proceso
        AND    transaccion_cod = vtransaccion
        AND    fecha_emision   = reg_a.fecha

        SELECT *
        INTO   g_reg5.*
        FROM   con_transaccion
        WHERE  fecha_emision   = reg_a.fecha
        AND    identificador   = reg_a.tipo
        AND    transaccion_cod = vtransaccion
        AND    proceso_cod     = reg_a.proceso
        AND    folio           = xfolio

        LET g_reg5.importe         = reg_a.diferencia
	     LET g_reg5.estado          = 20
	     LET g_reg5.usuario         = vusuario
	     LET g_reg5.fecha_actualiza = HOY

    INSERT INTO con_transaccion VALUES(g_reg5.*)

END FUNCTION
FUNCTION ingresa_fraccion(reg_a)
#ia---------------------------
    DEFINE reg_a   RECORD 
        fecha             DATE,
        proceso           CHAR(05),
        diferencia        DECIMAL(15,2),
        tipo              SMALLINT
    END RECORD
    DEFINE g_reg5  RECORD 
        folio             INTEGER,
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
        fecha_actualiza   DATE,
	     usuario           CHAR(08),
        estado            SMALLINT
    END RECORD
    DEFINE  vprecio       DECIMAL(10,6)
    DEFINE  vtransaccion  INTEGER

    SELECT transaccion_cod
    INTO   g_reg5.transaccion_cod
    FROM   tab_transaccion
    WHERE  proceso_cod = reg_a.proceso  
    AND    descripcion_1 MATCHES "*FRACCION"

    SELECT MAX(folio)
    INTO   g_reg5.folio
    FROM   con_transaccion
    WHERE  proceso_cod   = reg_a.proceso  
    AND    fecha_emision = reg_a.fecha

    SELECT precio_del_dia
    INTO   vprecio
    FROM   glo_valor_accion
    WHERE  fecha_valuacion = reg_a.fecha

    LET g_reg5.importe         = reg_a.diferencia
	 LET g_reg5.fecha_emision   = reg_a.fecha
	 LET g_reg5.fecha_valor     = reg_a.fecha
	 LET g_reg5.usuario         = vusuario
	 LET g_reg5.identificador   = reg_a.tipo
	 LET g_reg5.proceso_cod     = reg_a.proceso
	 LET g_reg5.fecha_actualiza = HOY
	 LET g_reg5.estado          = 20

    INSERT INTO con_transaccion VALUES(g_reg5.*)

    LET g_reg5.importe         = reg_a.diferencia * vprecio
    LET g_reg5.identificador   = 1

    INSERT INTO con_transaccion VALUES(g_reg5.*)

END FUNCTION
FUNCTION verifica_diferencia(reg_a)
#ia---------------------------
    DEFINE reg_a   RECORD 
        folio             INTEGER,
        tipo_mov          SMALLINT,
        proceso           CHAR(05)
    END RECORD
    DEFINE  vaccion_dif   DECIMAL(15,2)
    DEFINE  vimporte_dif  DECIMAL(15,2)
    DEFINE  vimporte      DECIMAL(15,2)

    IF (reg_a.tipo_mov = 486 OR 
        reg_a.tipo_mov = 487) THEN
        SELECT ROUND(SUM(monto_en_acciones),2)
        INTO   vaccion_dif
        FROM   dis_cuenta
        WHERE  folio           = reg_a.folio
        AND    tipo_movimiento = reg_a.tipo_mov
        AND    subcuenta in(1,2,5,6,9)
    ELSE
        SELECT ROUND(SUM(monto_en_acciones),2)
        INTO   vaccion_dif
        FROM   dis_cuenta
        WHERE  folio           = reg_a.folio
        AND    tipo_movimiento not in(486,487)
        AND    subcuenta in(1,2,5,6,9)
    END IF
    IF vaccion_dif IS NULL THEN 
        LET vaccion_dif = 0
    END IF
    SELECT SUM(importe)
    INTO   vimporte_dif
    FROM   con_transaccion
    WHERE  folio         = reg_a.folio
    AND    proceso_cod   = reg_a.proceso
    AND    identificador = 2

    IF vimporte_dif IS NULL THEN 
        LET vimporte_dif = 0
    END IF

    LET vimporte = 0
    LET vimporte = vaccion_dif - vimporte_dif
    RETURN vimporte
END FUNCTION
FUNCTION registra_neteo(reg_a)
#ia---------------------------
    DEFINE reg_a   RECORD 
        fecha             DATE,
        proceso           CHAR(05)
    END RECORD
    DEFINE g_reg5  RECORD 
        folio             INTEGER,
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
        fecha_actualiza   DATE,
	usuario           CHAR(08),
        estado            SMALLINT
    END RECORD
    DEFINE  vimporte      DECIMAL(15,2)
    DEFINE  ident         SMALLINT
    DEFINE  vtransaccion  INTEGER

    SELECT transaccion_cod
    INTO   g_reg5.transaccion_cod
    FROM   tab_transaccion
    WHERE  proceso_cod = reg_a.proceso  
    AND    descripcion_1 = "NETEO"

    DECLARE cur_net CURSOR FOR
        SELECT sum(importe),identificador
        FROM   con_transaccion
        WHERE  proceso_cod in("00009","00010")
        AND    fecha_emision = reg_a.fecha
        AND    transaccion_cod NOT IN(24011,29011,24351,29351,99991)
        AND    estado = 20
        GROUP BY 2
	ORDER BY 2
    FOREACH cur_net INTO vimporte,ident

	CASE ident
	    WHEN 1
                LET g_reg5.folio           = ""
                LET g_reg5.importe         = vimporte
	        LET g_reg5.fecha_emision   = reg_a.fecha
	        LET g_reg5.fecha_valor     = reg_a.fecha
	        LET g_reg5.usuario         = vusuario
	        LET g_reg5.identificador   = ident
	        LET g_reg5.proceso_cod     = reg_a.proceso
	        LET g_reg5.fecha_actualiza = HOY
	        LET g_reg5.estado          = 20

                INSERT INTO con_transaccion VALUES(g_reg5.*)

	    WHEN 2
                LET g_reg5.folio           = ""
                LET g_reg5.importe         = vimporte
	        LET g_reg5.fecha_emision   = reg_a.fecha
	        LET g_reg5.fecha_valor     = reg_a.fecha
	        LET g_reg5.usuario         = vusuario
	        LET g_reg5.identificador   = ident
	        LET g_reg5.proceso_cod     = reg_a.proceso
	        LET g_reg5.fecha_actualiza = HOY
	        LET g_reg5.estado          = 20
		IF vimporte > 0 THEN
                    INSERT INTO con_transaccion VALUES(g_reg5.*)
		END IF
        END CASE
    END FOREACH

END FUNCTION
