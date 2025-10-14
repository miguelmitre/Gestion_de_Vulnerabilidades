################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Por               => E.F.P.                                                   #
#Programa  INTB0901=> CARGA DE ARCHIVO DE PRECIO DE ACCI0ON PARA LA SIEFORES   #
#Fecha             => 09 de agosto de 2001                                     #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                          #
#Sistema           => INT                                                      #
#Modif. por        => Laura Eugenia Cortes Guzman                              #
#Fecha  Modif.     => 02 de Julio del 2004                                     #
#------------------------------------------------------------------------------#
#Modificacion      => CPL-1073                                                 #
#Fecha y Autor     => 29-11-2012 Alejandro Chagoya Salazar                     #
#Descripcion       => Se agrega precio=0 para la siefore 5                     #
################################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_param  RECORD       LIKE seg_modulo.* ,
          w_afore_cod           LIKE tab_siefore.afore_cod

   DEFINE reg RECORD            LIKE glo_valor_accion.*

   DEFINE HOY                   DATE

   DEFINE n_registros           CHAR(165),
          comando               CHAR(200),
          generar               CHAR(18),
          c_fecha_operacion     CHAR(10),
          c_fecha_valuacion     CHAR(10),
          opc                   CHAR(1)

END GLOBALS
#####################################################################
MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST  ,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT

   LET HOY = TODAY

   CALL ventana()

   CALL proceso()
   CLEAR SCREEN
   CLOSE WINDOW ventana_1

END MAIN
#####################################################################
FUNCTION ventana()
   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "INTB09011" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Ejecutar                                           < ",
           "Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " INTB0901       CARGA ARCHIVO DE ACCION PARA LAS ",
           "SIEFORES                  " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
END FUNCTION
#####################################################################
FUNCTION proceso()
   DEFINE dia           DATE,
          dia_valuacion DATE,
          enter         CHAR(1),
          v_precio_del_dia    DECIMAL(19,14)


   SELECT ruta_rescate
   INTO   g_param.ruta_rescate
   FROM   seg_modulo
   WHERE  modulo_cod = "int"

   LET INT_FLAG = FALSE

   INPUT BY NAME generar
      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
         END IF

      ON KEY (ESC)
         LET comando = "cd ",g_param.ruta_rescate CLIPPED,
                       "; ls > archivos_sie" CLIPPED
         RUN comando

         CREATE TEMP TABLE archivos
            (campo CHAR(100))

         LET comando = g_param.ruta_rescate CLIPPED,"/archivos_sie" CLIPPED

         LOAD FROM comando INSERT INTO archivos

         LET comando = "cd ",g_param.ruta_rescate CLIPPED,
                       "; rm archivos_sie" CLIPPED
         RUN comando

         SELECT "X"
         FROM   archivos
         WHERE  campo = generar

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"

            WHENEVER ERROR CONTINUE
            DROP TABLE archivos
            WHENEVER ERROR STOP

            NEXT FIELD generar
         ELSE
            WHENEVER ERROR CONTINUE
               DELETE FROM  con_pla_siefore
            WHENEVER ERROR STOP

            LET comando = g_param.ruta_rescate CLIPPED,"/",generar CLIPPED


            LOAD FROM comando INSERT INTO con_pla_siefore

            EXIT INPUT
         END IF
      ON KEY (CONTROL-C,INTERRUPT)
         ERROR "PROCESO CANCELADO..."
         SLEEP 2
         ERROR ""
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION "

   DECLARE cur_1 CURSOR FOR
   SELECT *
   FROM   con_pla_siefore

   FOREACH cur_1 INTO  n_registros
      LET reg.codigo_siefore     =  n_registros [001,003]
      LET reg.precio_del_dia     =  n_registros [004,015]
      LET c_fecha_operacion      =  n_registros [020,021],"/",
                                    n_registros [022,023],"/" ,
                                    n_registros [016,019]
      LET reg.fecha_operacion    =  c_fecha_operacion
      LET c_fecha_valuacion      =  n_registros [028,029],"/",
                                    n_registros [030,031],"/" ,
                                    n_registros [024,027]
      LET reg.fecha_valuacion    =  c_fecha_valuacion
      LET reg.monto_comis_saldos =  n_registros [032,046]
      LET reg.acc_circ_cap_fijo  =  n_registros [047,062]
      LET reg.acc_circ_res_esp   =  n_registros [064,079]
      LET reg.acc_circ_inv_perm  =  n_registros [081,096]
      LET reg.acc_circ_traba     =  n_registros [098,113]
      LET reg.acc_circ_traba_vol =  n_registros [115,130]
      LET reg.acc_circ_inv_afore =  n_registros [132,147]
      LET reg.acc_circ_totales   =  n_registros [149,165]

      IF reg.codigo_siefore = 0 OR
         reg.codigo_siefore = 1 OR
         reg.codigo_siefore = 2 OR
         reg.codigo_siefore = 3 OR
         reg.codigo_siefore = 4 OR
         reg.codigo_siefore = 5 OR
         reg.codigo_siefore = 11 THEN

         SELECT "a.X" FROM glo_valor_accion a
         WHERE a.fecha_valuacion = reg.fecha_valuacion
         AND   a.codigo_siefore  = reg.codigo_siefore
         IF SQLCA.SQLCODE = 0 THEN
            ERROR " NO INSERTO ...  EL REGISTRO YA FUE INGRESADO "
            SLEEP 3
         ELSE
                  INSERT INTO glo_valor_accion
                  VALUES (reg.*)
                  ERROR "REGISTRO INSERTADO ..."
                  SLEEP 2
         END IF

         ###  ACS Se agrega el valor de la Siefore 5, 
         ###  solo en caso que no se haya agregado con la carga del archivo
         ###  20 Nov 2012
          INITIALIZE v_precio_del_dia TO NULL
          SELECT precio_del_dia INTO v_precio_del_dia
               FROM glo_valor_accion g
               WHERE g.codigo_siefore = 5
               AND g.fecha_valuacion = reg.fecha_valuacion  -- fecha deseada
         
          IF v_precio_del_dia IS NULL THEN 
             INSERT INTO glo_valor_accion (codigo_siefore,precio_del_dia,fecha_valuacion)
                 VALUES (5,0,reg.fecha_valuacion )   -- fecha deseada
          END IF
          ##Termina

      ELSE
          ERROR "INCORRECTO El CODIGO DE LA SIEFORE...."
          SLEEP 3
      END IF
   END FOREACH
   ERROR ""

END FUNCTION

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE

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
