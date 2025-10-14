################################################################################
#Owner             => E.F.P.
#Programa CTAB0406.4gl => RECIBE Y CARGA ARCHIVO DE DOMICILIOS DE
#                         PATRONES PARA TRABAJADORES ASIGNADOS
#Por               => Silveria Contreras Garcia
#fecha:            => 11 de Mayo 2009
#Sistema           => CTA
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE
                nombre_archivo    CHAR(50),
                fecha_periodo     DATE,
                v_fecha_periodo   CHAR(10),
                v_mes_dia         CHAR(5),
        gparam_dev                      RECORD LIKE seg_modulo.*,
                hoy               DATE,
                usuario           CHAR(08),
                opc               CHAR(01),
                ejecuta           CHAR(200),
                hora_inicial      CHAR(08),
                hora_final        CHAR(08)
END GLOBALS

MAIN
   OPTIONS PROMPT LINE LAST, INPUT WRAP, ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST

        DEFER INTERRUPT
   CALL STARTLOG( "CTAB0406.log" )

   SELECT *, USER INTO gparam_dev.*, usuario
                FROM seg_modulo
                WHERE modulo_cod = "cta"

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2, 2 WITH FORM "CTAB04061"  ATTRIBUTE( BORDER )
   DISPLAY " CTAB0406                                                                      " AT 3,1 ATTRIBUTE( REVERSE )

   DISPLAY hoy USING "DD-MM-YYYY" AT 3, 66 ATTRIBUTE( REVERSE )
   DISPLAY "       CARGA ARCHIVO CON DIRECCIONES DEL PATRON PARA TRABAJADORES ASIGNADOS    " AT 5,1 ATTRIBUTE(REVERSE)

   MENU "CARGA ARCHIVO"
      COMMAND "Lectura de direcciones" "Lectura archivo de direcciones "
         CALL Lectura()
      COMMAND "Salida" "Salida del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1

END MAIN

FUNCTION Lectura()
   DEFINE
                hoy2                            DATE,
                Comando                         CHAR(200),
                edo                                     SMALLINT,
                reg_OK, reg_NO,
                tot_regs                                INTEGER

   LET hoy = TODAY
   LET INT_FLAG = FALSE
        INITIALIZE nombre_archivo, Comando TO NULL

   INPUT BY NAME nombre_archivo, fecha_periodo

      AFTER FIELD nombre_archivo
         IF nombre_archivo IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD nombre_archivo
         END IF

        LET Comando = "ls ", gparam_dev.ruta_rescate CLIPPED, " | grep '",
                      nombre_archivo CLIPPED, "' 1>/dev/null"
        RUN Comando RETURNING edo
        IF edo != 0 THEN
           PROMPT "NOMBRE DE ARCHIVO INCORRECTO" FOR opc
           NEXT FIELD nombre_archivo
        END IF

        SQL
             SELECT FIRST 1 1 FROM dis_ctrl_proceso
             WHERE parametro1 = $nombre_archivo
             AND proceso_cod = "CTAB0407"
             AND etapa_cod = 1
        END SQL

        IF SQLCA.SQLCODE = 0 THEN
            PROMPT "NOMBRE DE ARCHIVO YA CARGADO, VERIFICAR POR FAVOR"
                   ATTRIBUTE ( REVERSE ) FOR opc ATTRIBUTE ( REVERSE )
            NEXT FIELD nombre_archivo
        ELSE
            NEXT FIELD fecha_periodo
        END IF
         ----------------------------

        AFTER FIELD fecha_periodo
        IF fecha_periodo IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_periodo
        END IF

         LET v_fecha_periodo = fecha_periodo

         LET v_mes_dia =v_fecha_periodo[1,5]

         IF v_mes_dia <> "04/30" AND
            v_mes_dia <> "08/31" AND
            v_mes_dia <> "12/31" THEN


            ERROR "ERROR. LA FECHA NO REPRESENTA UN CUATRIMESTRE "
            NEXT FIELD fecha_periodo

         END IF


         EXIT INPUT

      ON KEY( CONTROL-C, INTERRUPT )
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc
   IF opc MATCHES '[Ss]' THEN
      ERROR "PROCESANDO INFORMACION ..."
      CALL Ejecuta_lectura()
   END IF

CLEAR FORM
CLEAR SCREEN

END FUNCTION

FUNCTION Ejecuta_lectura()
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES ( TODAY,                  -- fecha_proceso
           "CTAB0407",              -- proceso_cod
           1,                       -- etapa_cod   -- LECTURA
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           nombre_archivo,          -- parametro1
           fecha_periodo,           -- parametro2
           NULL,                    -- parametro3
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           NULL,                    -- folio
           NULL,                    -- resultado
           usuario,                 -- usuario
           0 )                      -- consecutivo

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTAR EN TABLA dis_ctrl_proceso Lectura Archivo",
                        STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   --ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..."
   ERROR "Ejecutando Lectura y Carga de Archivo..."
   SLEEP 3
   ERROR ""
   LET ejecuta = "nohup time fglgo CTAB0407.4gi ",
                 nombre_archivo CLIPPED," ",
                 fecha_periodo  CLIPPED," ",
                 " 1> ", usuario CLIPPED, ".CTAB0407.salida  ",
                 " 2> ", usuario CLIPPED, ".CTAB0407.error  & "

   ERROR "El proceso carga archivo ejecutando  por nohup ..."
   SLEEP 2
   RUN ejecuta
   ERROR "Verifique ARCHIVOS: ", usuario CLIPPED,".CTAB0407.salida y ", usuario CLIPPED,".CTAB0407.error"
   SLEEP 5
   ERROR ""
END FUNCTION

