#*********************************************************************#
#Proyecto      => safre_af                                            #
#Propietario   => E.F.P.                                              #
#Programa      => EXCB005                                             #
#Descripcion   => PARAMETRO DE LIQUIDACION DEV. DE PAG. EN EXC. BATCH #
#Fecha         => 12 de octubre de 2001.                              #
#Por           => MIGUEL ANGEL HERNANDEZ MARTINEZ                     #
#Actualizacion => 19 de diciembre de 2001.                            #
#Actualizado   => 22 de febrero de 2002.                              #
#Actualizado   => 06 de junio de 2002.                                #
#Modificado    => OMAR SANDOVAL BADILLO                               #
#Actualizado   => 24 de octubre 2005                                  #
#Sistema       => EXC.                                                #
#*********************************************************************#
DATABASE  safre_af

GLOBALS
   DEFINE g_reg      RECORD
          folio             INTEGER,
          fecha_liquidacion DATE
   END RECORD

   DEFINE gparam_dev RECORD LIKE seg_modulo.*

   DEFINE hoy               DATE,
          usuario           CHAR(08),
          opc               CHAR(01),
          ejecuta           CHAR(200),
          hora_inicial      CHAR(08),
          hora_final        CHAR(08),
          parametro         CHAR(10),
          vresultado        CHAR(40)

   DEFINE x_mes             INTEGER,
          x_ano             INTEGER,
          x_fecha_proceso   DATE,
          hoy2              DATE,
          dias              INTEGER

   DEFINE periodo_ini       DATE,
          periodo_fin       DATE,
          fin_semana        SMALLINT,
          dia_feriado       SMALLINT


END GLOBALS
#*********************************************************************
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   SELECT *,
          USER
   INTO   gparam_dev.*, 
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   LET hoy =  TODAY

   OPEN WINDOW ventana1 AT 2,3 WITH FORM "EXCB0051" ATTRIBUTE(BORDER)

   DISPLAY " EXCB005            DEVOLUCION DE PAGOS EN EXCESO                            " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "LIQUIDACION SIDECO" 
      COMMAND "IMSS" "Solicitudes SIDECO IMSS."
         MENU "LIQUIDACION IMSS"
            COMMAND "RCV " "Liquidacion solicitudes de RCV."
               CALL liquidacion_rcv()
            COMMAND "VIVIENDA " "Liquidacion solicitudes de vivienda."
               CALL liquidacion_viv()
            COMMAND "Regresar" "Regresa a menu anterior."
               EXIT MENU
         END MENU
      COMMAND "ISSSTE" "Solicitudes SIDECO ISSSTE."
         MENU "LIQUIDACION ISSSTE"
            COMMAND "RCV " "Liquidacion solicitudes de RCV."
               CALL liquidacion_rcv_issste()
            COMMAND "VIVIENDA " "Liquidacion solicitudes de vivienda."
               CALL liquidacion_viv_issste()
            COMMAND "rEgresar" "Regresa a menu anterior."
               EXIT MENU
         END MENU                             
      COMMAND "Salir" "Salir del menu."
         EXIT MENU
   END MENU

   
   CLOSE WINDOW ventana1
END MAIN
#*********************************************************************
FUNCTION liquidacion_rcv()

   DISPLAY "                             LIQUIDACION DE RCV                              " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.folio,
                 g_reg.fecha_liquidacion

      AFTER FIELD folio
         IF g_reg.folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF

         SELECT fecha_proceso
         INTO   x_fecha_proceso
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 1
         AND    proceso_cod = "EXC"

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "FOLIO DE PAGOS EN EXCESO NO EXISTE" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF

         SELECT "X"
         FROM   dis_provision
         WHERE  folio = g_reg.folio
         AND    subcuenta IN (1,2)
         AND    estado = 6
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "FOLIO NO TIENE REGISTROS PARA LIQUIDAR" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF

         SELECT "X"
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 5
         AND    proceso_cod = "EXC"

         IF SQLCA.SQLCODE = 0 THEN
            PROMPT "FOLIO DE PAGOS EN EXCESO YA LIQUIDADO" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF

--osb -- dia 10 y 15 del mes actual

       LET hoy   =  TODAY
       LET hoy2  =  TODAY
       LET x_mes = MONTH(hoy2)
       LET x_ano = YEAR(hoy2)
       LET hoy2  = MDY(x_mes,1,x_ano)

       LET dias = 23
       CALL suma_dias_habiles(hoy2,dias)
            RETURNING periodo_fin

       LET dias = 10
       CALL suma_dias_habiles(hoy2,dias)
            RETURNING periodo_ini
{
       IF hoy < periodo_ini THEN
          PROMPT "FALTAN DIAS PARA LA LIQUIDACION " ATTRIBUTE(REVERSE)
                  FOR opc ATTRIBUTE(REVERSE)
          NEXT FIELD folio
       ELSE
          IF hoy > periodo_fin THEN
             PROMPT "FUERA DEL PERIODO DE LIQUIDACION " ATTRIBUTE(REVERSE)
                  FOR opc ATTRIBUTE (REVERSE)
             NEXT FIELD folio
          END IF
       END IF
}
       NEXT FIELD fecha_liquidacion

    BEFORE FIELD fecha_liquidacion
       LET g_reg.fecha_liquidacion =  TODAY
       DISPLAY BY NAME g_reg.fecha_liquidacion

    AFTER FIELD fecha_liquidacion
       IF g_reg.fecha_liquidacion IS NULL THEN
          ERROR "LA FECHA LIQUIDACION NO PUEDE SER NULA ..."
          NEXT FIELD fecha_liquidacion
       END IF

{
--codigo para validar periodo
       IF g_reg.fecha_liquidacion >= periodo_ini AND 
          g_reg.fecha_liquidacion <= periodo_fin THEN

          CALL habil_siguiente(g_reg.fecha_liquidacion)
               RETURNING g_reg.fecha_liquidacion

          IF fin_semana = 1 OR dia_feriado = 1 THEN
             CALL habil_siguiente(g_reg.fecha_liquidacion)
                  RETURNING g_reg.fecha_liquidacion
                            
             IF g_reg.fecha_liquidacion > periodo_fin THEN
                CALL habil_anterior(g_reg.fecha_liquidacion,1)
                     RETURNING g_reg.fecha_liquidacion
             END IF
          END IF
       ELSE
          PROMPT "FUERA DEL PERIODO DE LIQUIDACION " ATTRIBUTE(REVERSE)
                  FOR opc ATTRIBUTE (REVERSE)
          NEXT FIELD folio
       END IF
}
       DISPLAY BY NAME g_reg.fecha_liquidacion
--------------------------------

         EXIT INPUT
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      DISPLAY "                                                                               " AT 5,1
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL ejecuta_liquidacion_rcv()
   ELSE
      ERROR "Proceso de liquidacion CANCELADO"
      SLEEP 2
      ERROR ""
   END IF

   DISPLAY "                                                                               " AT 5,1

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
#*********************************************************************
FUNCTION ejecuta_liquidacion_rcv()

   LET hora_inicial = TIME
   LET hora_final   = NULL
   LET parametro    = "RCV"
   LET vresultado   = "Iniciando liquidacion de registros "

   INSERT INTO dis_ctrl_proceso 
   VALUES (x_fecha_proceso,         -- fecha_proceso
           "EXC",                   -- proceso_cod
           5,                       -- etapa_cod   -- LIQUIDACION
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           g_reg.fecha_liquidacion, -- parametro1
           parametro,               -- parametro2
           "",                      -- parametro3
           "",                      -- parametro4
           "",                      -- parametro5
           g_reg.folio,             -- folio
           vresultado,              -- resultado
           usuario,                 -- usuario
           0                        -- correlativo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..." 
   SLEEP 3
   ERROR ""

   LET ejecuta="nohup fglgo EXCB009.4gi ",parametro," ",g_reg.folio," & "
   RUN ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."
   SLEEP 2
   ERROR ""
END FUNCTION
#*********************************************************************
FUNCTION liquidacion_viv()

   DEFINE xx_mes               INTEGER,
          xxfecha_valor        DATE,
          fecha_tecleada       DATE

   DISPLAY "                        LIQUIDACION DE VIVIENDA                              " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.folio,
                 g_reg.fecha_liquidacion

      AFTER FIELD folio
         IF g_reg.folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF

         SELECT fecha_proceso
         INTO   x_fecha_proceso
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 1
         AND    proceso_cod = "EXC"

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "FOLIO DE PAGOS EN EXCESO NO EXISTE" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF

         SELECT "X"
         FROM   dis_provision
         WHERE  folio = g_reg.folio
         AND    subcuenta = 4
         AND    estado = 6
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "FOLIO DE VIVIENDA NO TIENE REGISTROS PARA LIQUIDAR" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF

         SELECT "X"
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 7
         AND    proceso_cod = "EXC"

         IF SQLCA.SQLCODE = 0 THEN
            PROMPT "FOLIO DE PAGOS EN EXCESO YA LIQUIDADO" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF
{
         SELECT parametro4
         INTO   xxfecha_valor
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 1
         AND    proceso_cod = "EXC"
}
    NEXT FIELD fecha_liquidacion

    BEFORE FIELD fecha_liquidacion
       LET g_reg.fecha_liquidacion =  TODAY
       DISPLAY BY NAME g_reg.fecha_liquidacion

    AFTER FIELD fecha_liquidacion
       IF g_reg.fecha_liquidacion IS NULL THEN
          ERROR "LA FECHA LIQUIDACION NO PUEDE SER NULA ..."
          NEXT FIELD fecha_liquidacion
       END IF

       IF g_reg.fecha_liquidacion > hoy THEN
          ERROR "LA FECHA LIQUIDACION NO PUEDE SER MAYOR AL HOY ..."
          NEXT FIELD fecha_liquidacion
       END IF

       LET fecha_tecleada = g_reg.fecha_liquidacion

         LET hoy   =  TODAY
         LET hoy2  =  TODAY
         LET x_mes = MONTH(hoy2)
         LET x_ano = YEAR(hoy2)
         LET xx_mes = MONTH(x_fecha_proceso) + 1 
         IF xx_mes = 13 THEN
            LET xx_mes = 1
	 END IF

         IF x_mes = xx_mes THEN
            LET x_fecha_proceso = MDY(MONTH(x_fecha_proceso + 1 UNITS MONTH),1,YEAR(hoy2))

            CALL habil_siguiente(x_fecha_proceso)
                 RETURNING g_reg.fecha_liquidacion
         ELSE
            LET hoy2 = hoy2 + 1 UNITS MONTH
            LET hoy2 = MDY(MONTH(hoy2),1,YEAR(hoy2))

            CALL habil_siguiente(hoy2)
                 RETURNING g_reg.fecha_liquidacion
         END IF

         IF fecha_tecleada <> g_reg.fecha_liquidacion THEN
            ERROR " FECHA DEBE DE SER EL PRIMER DIA HABIL DEL MES ACTUAL "
            NEXT FIELD folio
         END IF

         IF hoy < g_reg.fecha_liquidacion THEN
            PROMPT "FALTAN DIAS PARA LA LIQUIDACION DE VIVIENDA" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF

         EXIT INPUT
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      DISPLAY "                                                                               " AT 5,1
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL ejecuta_liquidacion_viv()
   ELSE
      ERROR "Proceso de liquidacion CANCELADO"
      SLEEP 2
      ERROR ""
   END IF

   DISPLAY "                                                                               " AT 5,1

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
#*********************************************************************
FUNCTION ejecuta_liquidacion_viv()

   LET hora_inicial = TIME
   LET hora_final   = NULL
   LET parametro    = "VIVIENDA"
   LET vresultado   = "Iniciando liquidacion de registros de VIVIENDA "

   INSERT INTO dis_ctrl_proceso 
   VALUES (x_fecha_proceso,         -- fecha_proceso
           "EXC",                   -- proceso_cod
           7,                       -- etapa_cod   -- LIQUIDACION
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           g_reg.fecha_liquidacion, -- parametro1
           parametro,               -- parametro2
           "",                      -- parametro3
           "",                      -- parametro4
           "",                      -- parametro5
           g_reg.folio,             -- folio
           vresultado,              -- resultado
           usuario,                 -- usuario
           0                        -- correlativo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..." 
   SLEEP 3
   ERROR ""

   LET ejecuta="nohup fglgo EXCB009.4gi ",parametro," ",g_reg.folio," & "
   RUN ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."
   SLEEP 2
   ERROR ""
END FUNCTION
#*********************************************************************
FUNCTION suma_dias_habiles(diaActual,dia_hab)

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE,
          numDias       SMALLINT,
          dia_hab       SMALLINT

   LET diaTmp = diaActual

   FOR contador = 1 TO dia_hab 
      IF contador = 1 THEN
         CALL habil_siguiente (diaTmp)
              RETURNING diaTmp
      ELSE
         LET diaTmp = diaTmp + 1 UNITS DAY

         CALL habil_siguiente (diaTmp)
              RETURNING diaTmp
      END IF
   END FOR

   RETURN diaTmp

END FUNCTION
#########################################################################
FUNCTION habil_siguiente(diaActual)

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE

   DEFINE diaHabilSig   DATE,
          diaSemana     SMALLINT,
          feriado       SMALLINT,
          finSemana     SMALLINT

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

   LET fin_semana  = finSemana
   LET dia_feriado = feriado

   RETURN diaHabilSig

END FUNCTION
##############################################################3
FUNCTION habil_anterior(diaActual,num_dia)
#ha---------------------------------------
    DEFINE #smallint
        cont_1                 ,
        feriado                ,
        finSemana              ,
        diaSemana              ,
        contador               ,
        num_dia                SMALLINT

    DEFINE #date
        diaActual              ,
        diaHabilAnt            ,
        diaTmp                 DATE

    LET cont_1      = 0
    #LET diaHabilAnt = diaActual - 1 UNITS DAY
    LET diaHabilAnt = diaActual

    WHILE TRUE
        LET feriado   = 0
        LET finSemana = 0
        LET diaSemana = WEEKDAY(diaHabilAnt)

        IF diaSemana = 0 OR diaSemana = 6 THEN
            LET finSemana = 1
        ELSE
            SELECT *
            FROM   tab_feriado
            WHERE  feria_fecha = diaHabilAnt

            IF STATUS <> NOTFOUND THEN
                LET feriado = 1
            END IF
        END IF

        IF feriado = 1 OR finSemana = 1 THEN
            LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
        ELSE
            LET cont_1      = cont_1 + 1

            IF cont_1 = num_dia THEN
                EXIT WHILE
            ELSE
                LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
            END IF
        END IF
    END WHILE

    RETURN diaHabilAnt
END FUNCTION
#*********************************************************************
FUNCTION liquidacion_rcv_issste()

   DEFINE
      vparametro1           CHAR(20) 

   DISPLAY "                             LIQUIDACION DE RCV ISSSTE                        " AT 5,1 ATTRIBUTE(REVERSE)
   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.folio,
                 g_reg.fecha_liquidacion

      AFTER FIELD folio
         IF g_reg.folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF

         SELECT fecha_proceso,
                parametro1
         INTO   x_fecha_proceso,
                vparametro1
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 1
         AND    proceso_cod = "EXC"

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "FOLIO DE PAGOS EN EXCESO NO EXISTE" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF
         
         IF vparametro1[1,4] = "BANX" THEN
            SELECT "X"
            FROM   dis_provision
            WHERE  folio = g_reg.folio
            AND    subcuenta = 19
            AND    estado = 6
            GROUP BY 1
         ELSE
            SELECT "X"
            FROM   dis_provision
            WHERE  folio = g_reg.folio
            AND    subcuenta IN (13,30,31,33)
            AND    estado = 6
            GROUP BY 1         	
         END IF

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "FOLIO NO TIENE REGISTROS PARA LIQUIDAR" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF

         SELECT "X"
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 5
         AND    proceso_cod = "EXC"

         IF SQLCA.SQLCODE = 0 THEN
            PROMPT "FOLIO DE PAGOS EN EXCESO YA LIQUIDADO" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF

       LET hoy   =  TODAY
       LET hoy2  =  TODAY
       LET x_mes = MONTH(hoy2)
       LET x_ano = YEAR(hoy2)
       LET hoy2  = MDY(x_mes,1,x_ano)

       LET dias = 5
       CALL suma_dias_habiles(hoy2,dias)
            RETURNING periodo_fin

       LET dias = 1
       CALL suma_dias_habiles(hoy2,dias)
            RETURNING periodo_ini
{
       IF hoy < periodo_ini THEN
          PROMPT "FALTAN DIAS PARA LA LIQUIDACION " ATTRIBUTE(REVERSE)
                  FOR opc ATTRIBUTE(REVERSE)
          NEXT FIELD folio
       ELSE
          IF hoy > periodo_fin THEN
             PROMPT "FUERA DEL PERIODO DE LIQUIDACION " ATTRIBUTE(REVERSE)
                  FOR opc ATTRIBUTE (REVERSE)
             NEXT FIELD folio
          END IF
       END IF
}
       NEXT FIELD fecha_liquidacion

    BEFORE FIELD fecha_liquidacion
       LET g_reg.fecha_liquidacion =  TODAY
       DISPLAY BY NAME g_reg.fecha_liquidacion

    AFTER FIELD fecha_liquidacion
       IF g_reg.fecha_liquidacion IS NULL THEN
          ERROR "LA FECHA LIQUIDACION NO PUEDE SER NULA ..."
          NEXT FIELD fecha_liquidacion
       END IF
{
--codigo para validar periodo
       IF g_reg.fecha_liquidacion >= periodo_ini AND 
          g_reg.fecha_liquidacion <= periodo_fin THEN

          CALL habil_siguiente(g_reg.fecha_liquidacion)
               RETURNING g_reg.fecha_liquidacion

          IF fin_semana = 1 OR dia_feriado = 1 THEN
             CALL habil_siguiente(g_reg.fecha_liquidacion)
                  RETURNING g_reg.fecha_liquidacion
                            
             IF g_reg.fecha_liquidacion > periodo_fin THEN
                CALL habil_anterior(g_reg.fecha_liquidacion,1)
                     RETURNING g_reg.fecha_liquidacion
             END IF
          END IF
       ELSE
          PROMPT "FUERA DEL PERIODO DE LIQUIDACION " ATTRIBUTE(REVERSE)
                  FOR opc ATTRIBUTE (REVERSE)
          NEXT FIELD folio
       END IF
}
       DISPLAY BY NAME g_reg.fecha_liquidacion
--------------------------------

         EXIT INPUT
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      DISPLAY "                                                                               " AT 5,1
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL ejecuta_liquidacion_rcv_issste()
   ELSE
      ERROR "Proceso de liquidacion CANCELADO"
      SLEEP 2
      ERROR ""
   END IF

   DISPLAY "                                                                               " AT 5,1

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
#*********************************************************************
FUNCTION ejecuta_liquidacion_rcv_issste()

   LET hora_inicial = TIME
   LET hora_final   = NULL
   LET parametro    = "RCV-ISSSTE"
   LET vresultado   = "Iniciando liquidacion de registros "

   INSERT INTO dis_ctrl_proceso 
   VALUES (x_fecha_proceso,         -- fecha_proceso
           "EXC",                   -- proceso_cod
           5,                       -- etapa_cod   -- LIQUIDACION
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           g_reg.fecha_liquidacion, -- parametro1
           parametro,               -- parametro2
           "",                      -- parametro3
           "",                      -- parametro4
           "",                      -- parametro5
           g_reg.folio,             -- folio
           vresultado,              -- resultado
           usuario,                 -- usuario
           0                        -- correlativo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..." 
   SLEEP 3
   ERROR ""

   LET ejecuta="nohup fglgo EXCB009.4gi ",parametro," ",g_reg.folio," & "
   RUN ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."
   SLEEP 2
   ERROR ""
END FUNCTION
#*********************************************************************
FUNCTION liquidacion_viv_issste()

   DEFINE xx_mes               INTEGER,
          xxfecha_valor        DATE,
          fecha_tecleada       DATE

   DISPLAY "                             LIQUIDACION DE VIVIENDA ISSSTE                  " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.folio,
                 g_reg.fecha_liquidacion

      AFTER FIELD folio
         IF g_reg.folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF

         SELECT fecha_proceso
         INTO   x_fecha_proceso
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 1
         AND    proceso_cod = "EXC"

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "FOLIO DE PAGOS EN EXCESO NO EXISTE" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF

         SELECT "X"
         FROM   dis_provision
         WHERE  folio = g_reg.folio
         AND    subcuenta IN (14,35)
         AND    estado = 6
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "FOLIO DE VIVIENDA NO TIENE REGISTROS PARA LIQUIDAR" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF

         SELECT "X"
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 7
         AND    proceso_cod = "EXC"

         IF SQLCA.SQLCODE = 0 THEN
            PROMPT "FOLIO DE PAGOS EN EXCESO YA LIQUIDADO" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF
{
         SELECT parametro4
         INTO   xxfecha_valor
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 1
         AND    proceso_cod = "EXC"
}
    NEXT FIELD fecha_liquidacion

    BEFORE FIELD fecha_liquidacion
       LET g_reg.fecha_liquidacion =  TODAY
       DISPLAY BY NAME g_reg.fecha_liquidacion

    AFTER FIELD fecha_liquidacion
       IF g_reg.fecha_liquidacion IS NULL THEN
          ERROR "LA FECHA LIQUIDACION NO PUEDE SER NULA ..."
          NEXT FIELD fecha_liquidacion
       END IF

       IF g_reg.fecha_liquidacion > hoy THEN
          ERROR "LA FECHA LIQUIDACION NO PUEDE SER MAYOR AL HOY ..."
          NEXT FIELD fecha_liquidacion
       END IF

       LET fecha_tecleada = g_reg.fecha_liquidacion

         LET hoy   =  TODAY
         LET hoy2  =  TODAY
         LET x_mes = MONTH(hoy2)
         LET x_ano = YEAR(hoy2)
         LET xx_mes = MONTH(x_fecha_proceso) + 1 
         IF xx_mes = 13 THEN
            LET xx_mes = 1
	       END IF
{
         IF x_mes = xx_mes THEN
            LET x_fecha_proceso = MDY(MONTH(x_fecha_proceso + 1 UNITS MONTH),1,YEAR(hoy2))

            CALL habil_siguiente(x_fecha_proceso)
                 RETURNING g_reg.fecha_liquidacion
         ELSE
            LET hoy2 = hoy2 + 1 UNITS MONTH
            LET hoy2 = MDY(MONTH(hoy2),1,YEAR(hoy2))

            CALL habil_siguiente(hoy2)
                 RETURNING g_reg.fecha_liquidacion
         END IF

         IF fecha_tecleada <> g_reg.fecha_liquidacion THEN
            ERROR "LA FECHA DEBE DE SER EL PRIMER DIA HABIL DEL MES ACTUAL"
            NEXT FIELD folio
         END IF

         IF hoy < g_reg.fecha_liquidacion THEN
            PROMPT "FALTAN DIAS PARA LA LIQUIDACION DE VIVIENDA" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
         END IF
}
         EXIT INPUT
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      DISPLAY "                                                                               " AT 5,1
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL ejecuta_liquidacion_viv_issste()
   ELSE
      ERROR "Proceso de liquidacion CANCELADO"
      SLEEP 2
      ERROR ""
   END IF

   DISPLAY "                                                                               " AT 5,1

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
#*********************************************************************
FUNCTION ejecuta_liquidacion_viv_issste()

   LET hora_inicial = TIME
   LET hora_final   = NULL
   LET parametro    = "VIV-ISSSTE"
   LET vresultado   = "Iniciando liquidacion de registros de VIVIENDA "

   INSERT INTO dis_ctrl_proceso 
   VALUES (x_fecha_proceso,         -- fecha_proceso
           "EXC",                   -- proceso_cod
           7,                       -- etapa_cod   -- LIQUIDACION
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           g_reg.fecha_liquidacion, -- parametro1
           parametro,               -- parametro2
           "",                      -- parametro3
           "",                      -- parametro4
           "",                      -- parametro5
           g_reg.folio,             -- folio
           vresultado,              -- resultado
           usuario,                 -- usuario
           0                        -- correlativo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..." 
   SLEEP 3
   ERROR ""

   LET ejecuta="nohup fglgo EXCB009.4gi ",parametro," ",g_reg.folio," & "
   RUN ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."
   SLEEP 2
   ERROR ""
END FUNCTION