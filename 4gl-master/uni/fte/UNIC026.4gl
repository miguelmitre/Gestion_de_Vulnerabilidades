###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.       					      #
#Programa   UNIC026=> GENERA ARCHIVO SOL. TRABAJADORES PARA PROCESAR          #
#Sistema           => UNI                                                     #
#Autor             => Miguel Angel Hernandez Martinez.                        #
#Fecha             => 03 de marzo de 2003.                                    #
###############################################################################
DATABASE safre_af
GLOBALS

   DEFINE reg_2 RECORD LIKE uni_solicitud.*

   DEFINE cont              ,
          cont1             ,
          cont2             ,
          cont3             ,
          tot_registros     INTEGER

   DEFINE HOY               DATE,
          G_LISTA           CHAR(500),
          cat               CHAR(500),
          borra             CHAR(200),
          vpregunta         CHAR(1),
          char              CHAR(1),
          vcodigo_afore     CHAR(3),
          vconfronta        INTEGER,
          vrecibido         INTEGER,
          codigo            INTEGER, 
          vlote             INTEGER, 
          vfolio2           INTEGER, 
          vfolio            INTEGER,
          enter             CHAR(1),
          cla_sel           CHAR(100),
          cla_sel1          CHAR(100),
          vlotes_cod        SMALLINT,
          vlotes_desc       CHAR(30),
          v_rep             CHAR(400), 
          vreport           CHAR(800)

   DEFINE g_paramgrales RECORD LIKE seg_modulo.*

END GLOBALS
################################################################################
MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT

   CALL STARTLOG("UNIC026.log")
   CALL inicio()

   CALL proceso_principal()

END MAIN
##############################################################################
FUNCTION inicio()

   LET HOY = TODAY
   LET vrecibido = 10

   SELECT codigo_afore
   INTO   codigo
   FROM   tab_afore_local

   SELECT * 
   INTO   g_paramgrales.*
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

END FUNCTION
##############################################################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0261" ATTRIBUTE(BORDER)
   DISPLAY "UNIC026            GENERA SOLICITUDES DE TRABAJADOR POR                              " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                         UNIFICACION DE CUENTAS                                      " AT 4,1 ATTRIBUTE(REVERSE)

   DISPLAY " < Esc > Ejecutar Proceso                                   < Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   INPUT BY NAME vpregunta
      AFTER FIELD vpregunta
         IF vpregunta MATCHES "[Nn]" THEN 
            ERROR "PROCESO CANCELADO"
            SLEEP 2
            EXIT PROGRAM
         END IF

         SELECT "X"
         FROM   uni_solicitud
         WHERE  estado = vrecibido
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO HAY SOLICITUDES PENDIENTES DE ENVIAR"
            SLEEP 2
            EXIT INPUT
         END IF
         ERROR ""

      ON KEY (ESC)
         IF vpregunta MATCHES "[Ss]" THEN 
            ERROR "PROCESANDO INFORMACION"
         ELSE
            ERROR "PROCESO CANCELADO"
            SLEEP 2
            EXIT PROGRAM
         END IF

         SELECT "X"
         FROM   uni_solicitud
         WHERE  estado = 10
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO HAY SOLICITUDES PENDIENTES DE ENVIAR"
            SLEEP 2
            EXIT INPUT
         END IF

         CALL genera_archivo() #gr

         ERROR" LISTADO GENERADO " 
         SLEEP 2
         ERROR ""
         EXIT INPUT

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT INPUT
   END INPUT

   CLEAR WINDOW ventana_1
   CLOSE WINDOW ventana_1

END FUNCTION
##############################################################################
FUNCTION genera_archivo()

   DEFINE permisos       CHAR(100),
	  hora           CHAR(8),
	  x_hora         CHAR(6),
          vfecha         DATE,
	  ejecuta        CHAR(200)

   DEFINE x_lotes_cod    SMALLINT,
	  x_lotes_desc   CHAR(40),
	  x_lotes_num    SMALLINT

   LET hora   = TIME
   LET x_hora = hora[1,2],hora[4,5],hora[7,8]
   LET x_lotes_cod  = 73
   LET x_lotes_desc = "UNIFICACION OPERACION 73"

   CALL miercoles_siguiente(HOY)
        RETURNING vfecha

   LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,
                 "/",vfecha USING"YYYYMMDD",
		 x_hora CLIPPED,
                 ".73UN" CLIPPED

   START REPORT r_report TO G_LISTA

      SELECT lotes_num
      INTO   x_lotes_num
      FROM   tab_lote
      WHERE  lotes_cod   = x_lotes_cod
      AND    lotes_fecha = TODAY

      IF x_lotes_num = 0 THEN
	 LET x_lotes_num = 1

         INSERT INTO tab_lote
         VALUES (TODAY,          -- lotes_fecha
                 x_lotes_cod,    -- lotes_cod
	         x_lotes_desc,   -- lotes_desc
	         0,              -- lotes_correlativo
	         x_lotes_num     -- lotes_num
       	        ) 
      ELSE
	 LET x_lotes_num = x_lotes_num + 1

         UPDATE tab_lote
         SET    lotes_num   = x_lotes_num 
         WHERE  lotes_cod   = x_lotes_cod
         AND    lotes_fecha = TODAY
      END IF 

      LET v_rep = codigo,"|",
                  vfecha,"|",
                  x_lotes_num,"|"

      OUTPUT TO REPORT r_report(v_rep,273,1)

      CALL genera_detalle()

      CALL genera_sumario()

   FINISH REPORT r_report

   LET permisos = "chmod 777 ",G_LISTA CLIPPED
   RUN permisos
{
   LET ejecuta = "fglgo UNIC026RPT.4gi ",hoy  
   RUN ejecuta
}
END FUNCTION
##############################################################################
FUNCTION genera_detalle()

   DECLARE cur_2 CURSOR FOR
   SELECT *
   FROM   uni_solicitud
   WHERE  estado = vrecibido
   FOR UPDATE

   LET cont = 1

   FOREACH cur_2 INTO reg_2.*

      LET v_rep = cont,"|",
                  reg_2.nss_cta1,"|",
                  reg_2.nss_uni,"|"

      OUTPUT TO REPORT r_report(v_rep,273,2)

      UPDATE uni_solicitud
      SET    consecutivo_reg = cont
      WHERE  CURRENT OF cur_2

      LET cont = cont + 1
   END FOREACH

END FUNCTION
##############################################################################
FUNCTION genera_sumario()

   DEFINE total_reg          SMALLINT,
          total_nss_uni      SMALLINT,
          total_nss_cta1     SMALLINT

   SELECT COUNT(*),
          COUNT(unique nss_uni),
          COUNT(nss_cta1)
   INTO   total_reg,
          total_nss_uni,
          total_nss_cta1
   FROM   uni_solicitud
   WHERE  estado = vrecibido

   LET v_rep = total_reg,"|",
               total_nss_cta1,"|",
               total_nss_uni,"|"

   OUTPUT TO REPORT r_report(v_rep,273,9)

   UPDATE uni_solicitud
   SET    estado     = 20,
	  factualiza = TODAY
   WHERE  estado     = vrecibido

END FUNCTION
#############################################################################
FUNCTION miercoles_siguiente(diaActual)
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

      IF diaSemana = 1  THEN

         SELECT *
         FROM   tab_feriado
         WHERE  feria_fecha = diaHabilSig

         IF STATUS <> NOTFOUND THEN
            LET feriado = 1
         END IF

         IF feriado = 1 THEN
            LET diaHabilSig = diaHabilSig + 1 UNITS DAY
            EXIT WHILE
         ELSE
            EXIT WHILE
         END IF
      END IF

      LET diaHabilSig = diaHabilSig +1 UNITS DAY
   END WHILE

   RETURN diaHabilSig

END FUNCTION
###############################################################################

