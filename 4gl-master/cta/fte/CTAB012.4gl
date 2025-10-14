################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                              #
#Owner             => E.F.P.                                                   #
#Programa CTAB012  => DETALLE DE MOVIMIENTOS                                   #
#Fecha creacion    => 28 DE MARZO 2006                                         #
#Por               => OMAR SANDOVAL BADILLO                                    #
#Sistema           => CTA                                                      #
################################################################################
DATABASE safre_af
GLOBALS

   DEFINE x_fecha_recepcion DATE,
          HOY               DATE,
          enter             CHAR(1),
          opc               CHAR(1),
          sw                SMALLINT

   DEFINE ejecuta           CHAR(200)
   DEFINE parametro1        CHAR(20),
          x_hora_inicio     CHAR(8),
          usuario           CHAR(8),
          vfolio            INTEGER

   DEFINE gparam RECORD LIKE seg_modulo.*
           
END GLOBALS
####################################################
MAIN
   DEFER INTERRUPT
   OPTIONS
       INPUT WRAP,
       PROMPT LINE LAST -2,
       ACCEPT KEY CONTROL-I

   LET HOY = TODAY
   LET sw = 0
   LET x_hora_inicio = TIME


   SELECT *,
          USER
   INTO   gparam.*,
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   CALL menu()

END MAIN
####################################################
FUNCTION menu()

   OPEN WINDOW ventana_01 AT 2,2 WITH FORM "CTAB0121" ATTRIBUTE(BORDER)
   DISPLAY " CTAB012                      DETALLE DE MOVIMIENTOS                           " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "DETALLE MOVIMIENTOS"
      COMMAND "Por NSS" "Detalle de movimientos por NSS"
         CALL por_nss(0)
      COMMAND "Por CURP" "Detalle de movimientos por CURP"
         CALL por_nss(1)
      COMMAND "Salir" "Salida del programa "
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_01

END FUNCTION
####################################################
FUNCTION por_nss(dato)

   DEFINE nss    CHAR(11)
   DEFINE curp   CHAR(18)
   DEFINE dato   SMALLINT

   DEFINE bandera        SMALLINT
   DEFINE ruta_archivo   CHAR(300)

   OPEN WINDOW ventana_02 AT 2,2 WITH FORM "CTAB0122" ATTRIBUTE(BORDER)

   IF dato = 0 THEN 
      DISPLAY " GENERA POR NSS  " AT 2,1 
      DISPLAY " GENERACION POR NSS: " AT 8,9
   ELSE
      DISPLAY " GENERA POR CURP " AT 2,1 
      DISPLAY " GENERACION POR CURP: " AT 9,9
   END IF

   DISPLAY " <Ctrl-C> Salir " AT 2,62 
   DISPLAY " CTAB012                       DETALLE DE MOVIMIENTOS                          " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   LET nss  = NULL
   LET curp = NULL

   IF dato = 0 THEN
      INPUT BY NAME nss WITHOUT DEFAULTS
         AFTER FIELD nss
            IF nss IS NULL THEN
               ERROR "EL CAMPO NSS NO PUEDE SER NULO ..."
               NEXT FIELD nss
            END IF
   
         ON KEY (ESC)
            IF nss IS NULL THEN 
               ERROR "EL CAMPO NSS NO PUEDE SER NULO ..."
               NEXT FIELD nss
            END IF

            EXIT INPUT

         ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

         ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
       END INPUT
   ELSE
      INPUT BY NAME curp WITHOUT DEFAULTS
         AFTER FIELD curp
            IF curp IS NULL THEN
               ERROR "EL CAMPO NSS NO PUEDE SER NULO ..."
               NEXT FIELD curp
            END IF

         ON KEY (ESC)
            IF curp IS NULL THEN
               ERROR "EL CAMPO NSS NO PUEDE SER NULO ..."
               NEXT FIELD curp
            END IF

            EXIT INPUT

         ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

         ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
       END INPUT
   END IF

   ERROR " PROCESANDO INFORMACION ... " 
   SLEEP 2
--osb

   SELECT "X"
   FROM cta_folio
   
   IF SQLCA.SQLCODE = 0 THEN
      SELECT folio + 1
      INTO   vfolio
      FROM   cta_folio

      UPDATE cta_folio
      SET    folio = vfolio
   ELSE
      LET vfolio = 1

      INSERT INTO cta_folio
      VALUES (vfolio)
   END IF

   IF dato = 0 THEN
      LET parametro1 = nss
   ELSE
      LET parametro1 = curp
   END IF

   INSERT INTO cta_ctr_proceso
   VALUES (vfolio,              -- folio
           4,                   -- tipo_edo
           parametro1,          -- nss
           TODAY,               -- fecha_inicio
           "",                  -- fecha_fin
           x_hora_inicio,       -- hora_inicio
           "",                  -- hora_fin
           "",                  -- tipo_salida
           7,                   -- tipo_informe --detalle movimiento
           1,                   -- estado
           TODAY,               -- factualiza
           usuario              -- usuario
          )

    CALL ejecuta_lectura(4,vfolio)

{
    CALL CTAB013(nss,curp)
       RETURNING bandera,ruta_archivo

    IF bandera = 1 THEN
       DISPLAY "Archivo : ",ruta_archivo AT 18,2
       ERROR " PROCESO FINALIZADO ... "
       SLEEP 5
    ELSE
       ERROR " ARCHIVO NO GENERADO, NOTIFIQUE AL AREA DE SISTEMAS ... <Enter> PARA SALIR "
       PROMPT "" FOR opc
    END IF
}
    LET ejecuta = "nohup time fglgo CTAB013 ",nss," ",curp," ",vfolio," &"  
    RUN ejecuta
    ERROR "TERMINA GENERACION DEL DETALLE DE MOVIMIENTOS ..."
    SLEEP 2
    ERROR ""
    
    CLEAR SCREEN
    CLOSE WINDOW ventana_02

END FUNCTION
############################################################
FUNCTION ejecuta_lectura(x_tipo_proceso,x_folio_dia)

   DEFINE hora_inicial       CHAR(08),
          hora_final         CHAR(08),
          x_folio_dia        INTEGER,
          x_tipo_proceso     SMALLINT

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES (TODAY,                   -- fecha_proceso
           "CTA",                   -- proceso_cod
           1,                       -- etapa_cod   -- LECTURA
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           x_tipo_proceso,          -- parametro1
           NULL,                    -- parametro2
           NULL,                    -- parametro3
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           x_folio_dia,             -- folio
           NULL,                    -- resultado
           USER,                    -- usuario
           0                        -- consecutivo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

END FUNCTION
############################################################
#eof
