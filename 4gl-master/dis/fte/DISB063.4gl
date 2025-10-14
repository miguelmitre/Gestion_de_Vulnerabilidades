###############################################################################
#Proyecto          => Safre                                                   #
#Propietario       => E.F.P.                                                  #
#Modulo            => DIS.                                                    #
#Programa          => DISB063                                                 #
#Descripcion       => Genara Saldos e intereses vivienda p/envio a Procesar   #
#Fecha Inicio      => 29-enero-2004.                                          #
#Fecha Termino     =>                                                         #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
#                  => ALEJANDRO RAMIREZ                                       #
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE
      w_codigo_afore     LIKE tab_afore_local.codigo_afore,
      HOY                DATE,
      usuario          CHAR (08)

   DEFINE g_fecha_corte  DATE
   DEFINE new_fecha        DATE
   DEFINE opc            CHAR(01)
   DEFINE sentencia      CHAR(300)
   DEFINE hora_inicial  CHAR(08),
          hora_final    CHAR(08),
          ejecuta       CHAR(200)

END GLOBALS

MAIN
   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST  ,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT
        
   LET HOY = TODAY

   OPEN WINDOW ventana_21 AT 2,2 WITH FORM "DISB0631" ATTRIBUTE( BORDER)
   DISPLAY " DISB063             GNERACION ARCHIVO SALDOS VIVIENDA                         " AT 1,1 ATTRIBUTE(REVERSE,green)
   DISPLAY hoy USING 'DD-MM-YYYY' AT 1,68 ATTRIBUTE(REVERSE)

   DISPLAY " [Ctrl-C] Salir                                              [ENTER] EJECUTAR " AT 3,1 ATTRIBUTE(REVERSE,green)

    LET int_flag= FALSE

    INPUT BY NAME g_fecha_corte
      AFTER FIELD g_fecha_corte
         PROMPT "Es correcta la fecha [S/N] ... " FOR opc
         IF opc MATCHES '[Ss]' THEN

           IF g_fecha_corte IS NULL THEN
              ERROR "LA FECHA DE CORTE NO PUEDE SER NULA  "
              NEXT FIELD g_fecha_corte 
           ELSE




              CALL Ejecuta_saldos()

              ERROR "PROCESO EN EJECUCION EN NOHUP,CRL-C PARA SALIR"

           END IF
         ELSE
            NEXT FIELD g_fecha_corte 
         END IF

      ON KEY (control-c)
        LET int_flag = TRUE
        EXIT INPUT
    END INPUT

    IF int_flag = TRUE THEN
       LET int_flag = FALSE
       CLEAR FORM
       CLEAR SCREEN
       CLOSE WINDOW ventana_21
    END IF
END MAIN

FUNCTION Ejecuta_saldos()

   SELECT USER
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "dis"

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB064",               -- proceso_cod
       8,                       -- etapa_cod     -- LIQUIDACION
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_fecha_corte,           -- parametro1
       "",                      -- parametro2
       "",                      -- parametro3
       "",                      -- parametro4
       "",                      -- parametro5
       "",                      -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)                       -- consecutivo
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Liquidacion ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   ERROR "Ejecutando Proceso de Saldos de vivienda por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB064.4gi ",g_fecha_corte CLIPPED,"  &"
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION
