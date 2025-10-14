#############################################################################
#Proyecto          => AFORE(MEXICO)                                         #
#Propietario       => E.F.P                                                 #
#Programa CTAR001  => REVERSO DE ESTADOS DE CUENTA FINALES                  #
#Sistema           => CTA                                                   #
#Autor             => OMAR SANDOVAL BADILLO                                 #
#Fecha             => 26 DE DICIEMBRE 2005                                  #
#############################################################################
DATABASE safre_af
GLOBALS

   DEFINE seg_modulo  RECORD LIKE seg_modulo.*

   DEFINE opc                   CHAR(01),
          g_usuario             CHAR(08),
          HOY                   DATE,
          g_afore               RECORD LIKE tab_afore_local.*

   DEFINE total_registros       SMALLINT

END GLOBALS
#############################################################################
MAIN

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   SELECT *,
          USER
   INTO   seg_modulo.*,
          g_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   LET HOY = TODAY

   CALL STARTLOG("CTAR001.log")
   CALL inicio()

END MAIN
#############################################################################
FUNCTION inicio()

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "CTAR0011" ATTRIBUTE(BORDER)
   DISPLAY " CTAR001            REVERSO DE ESTADOS DE CUENTA FINALES                       " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY "<Esc> Consulta" AT 4,1

   MENU "REVERSO"
      COMMAND "Reverso" "Reverso de estado de cuenta finales"
         CALL proceso()
      COMMAND "Salida" "Salida del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1

END FUNCTION
#############################################################################
FUNCTION proceso()

   DEFINE vtipo_edocta    SMALLINT,
          vfecha_liq      DATE

   INITIALIZE vtipo_edocta TO NULL
   INITIALIZE vfecha_liq   TO NULL

   INPUT BY NAME vtipo_edocta,
                 vfecha_liq WITHOUT DEFAULTS

      AFTER FIELD vtipo_edocta
         IF vtipo_edocta IS NULL OR vtipo_edocta = " " THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vtipo_edocta
         END IF

         SELECT "X"
         FROM   tab_tipo_informe
         WHERE  tipo_informe = vtipo_edocta

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "EL TIPO DE INFORME NO EXISTE ..."
            NEXT FIELD vtipo_edocta
         END IF

      AFTER FIELD vfecha_liq
         IF vfecha_liq IS NULL OR vfecha_liq = " " THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vfecha_liq
         END IF

         SELECT "X"
         FROM   cta_ctr_proceso
         WHERE  tipo_informe = vtipo_edocta 
         AND    factualiza  >= vfecha_liq
         AND    estado       = 3
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO SE ENCONTRARON REGISTROS PARA REVERSAR ..."
            NEXT FIELD vtipo_edocta
         END IF

      ON KEY(ESC)
         IF vtipo_edocta IS NULL OR vtipo_edocta = " " THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vtipo_edocta
         END IF

         IF vfecha_liq IS NULL OR vfecha_liq = " " THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vfecha_liq
         END IF

         SELECT "X"
         FROM   cta_ctr_proceso
         WHERE  tipo_informe = vtipo_edocta 
         AND    factualiza  >= vfecha_liq
         AND    estado       = 3
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO SE ENCONTRARON REGISTROS PARA REVERSAR ..."
            NEXT FIELD vtipo_edocta
         END IF
         EXIT INPUT

      ON KEY(INTERRUPT)
         ERROR "PROCESO CANCELADO ..."
         CLEAR FORM
         EXIT INPUT
   END INPUT   

   ERROR "PROCESANDO INFORMACION ..."

   CALL primer_paso(vtipo_edocta,
                    vfecha_liq)

END FUNCTION
#############################################################################
FUNCTION primer_paso(vtipo_edocta,vfecha_liq)

   DEFINE i     SMALLINT

   DEFINE vtipo_edocta   SMALLINT,
          vfecha_liq     DATE

   DEFINE l_reg_01 ARRAY[500] OF RECORD
      cuantos            SMALLINT,
      fecha_fin          DATE
   END RECORD

   DECLARE cursor_01 CURSOR FOR
   SELECT count(*),
          fecha_fin
   FROM   cta_ctr_proceso
   WHERE  factualiza   = vfecha_liq
   AND    tipo_informe = vtipo_edocta
   AND    estado       = 3
   GROUP BY 2

   LET i = 1 
   LET total_registros = 0

   FOREACH cursor_01 INTO l_reg_01[i].*
      LET total_registros = total_registros + l_reg_01[i].cuantos 
      LET i = i + 1
   END FOREACH

   LET i = i - 1

   IF i >= 1 THEN
      CALL SET_COUNT(i)
      DISPLAY "           REGISTROS ENCONTRADOS      FECHA DE LIQUIDACION                     " AT 12,1 ATTRIBUTE(REVERSE)
      DISPLAY "<Enter> Continuar" AT 11,1

      DISPLAY ARRAY l_reg_01 TO scr_01.*

         ON KEY(CONTROL-M)
            DISPLAY "Total de Registros : ",total_registros AT 19,1

            PROMPT "Desea continuar con el proceso de Reverso S/N ?: " FOR opc

            IF opc MATCHES "[SsNn]" THEN
               IF opc = "N" OR opc = "n" THEN
                  INITIALIZE l_reg_01 TO NULL
                  INITIALIZE total_registros TO NULL
                  DISPLAY "                                   " AT 19,1
                  DISPLAY "                                                                               " AT 12,1
                  DISPLAY "                 " AT 11,1
                  CLEAR FORM
                  ERROR "PROCESO CANCELADO ..."
               ELSE
                  CALL reverso_finales(vtipo_edocta,
                                       vfecha_liq)
               END IF
               CLEAR FORM
               DISPLAY "                                                                               " AT 12,1
               DISPLAY "                                   " AT 19,1
               DISPLAY "                 " AT 11,1
            END IF
            EXIT DISPLAY

         ON KEY(INTERRUPT)
            ERROR "REVERSO CANCELADO ..."
            INITIALIZE l_reg_01 TO NULL
            DISPLAY "                                                                               " AT 12,1
            DISPLAY "                 " AT 11,1
            CLEAR FORM
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "PROCESO CANCELADO ..."
      CLEAR FORM
   END IF

END FUNCTION
#############################################################################
FUNCTION reverso_finales(vtipo_edocta,vfecha_liq)

   DEFINE i     SMALLINT

   DEFINE vtipo_edocta   SMALLINT,
          vfecha_liq     DATE

   DEFINE l_reg_02 ARRAY[500] OF RECORD
      folio              INTEGER,
      fecha_inicio       DATE,
      fecha_fin          DATE
   END RECORD

   DECLARE cursor_02 CURSOR FOR
   SELECT folio,
          fecha_inicio,
          fecha_fin
   FROM   cta_ctr_proceso
   WHERE  factualiza   = vfecha_liq
   AND    tipo_informe = vtipo_edocta
   AND    estado       = 3

   LET i = 1

   FOREACH cursor_02 INTO l_reg_02[i].*

      UPDATE cta_ctr_cuenta
      SET (fecha_informe,
           tipo_informe) = ((SELECT a.fecha_fin,
                                    a.tipo_informe
                             FROM   cta_ctr_proceso a
                             WHERE  a.tipo_edo     = 2
                             AND    a.tipo_informe = vtipo_edocta
                             AND    a.estado       = 3
                             AND    a.folio        = l_reg_02[i].folio
                             AND    a.nss          = cta_ctr_cuenta.nss))
      WHERE nss IN(SELECT nss
                   FROM   cta_ctr_proceso
                   WHERE  tipo_edo     = 2
                   AND    estado       = 3
                   AND    folio        = l_reg_02[i].folio
                   AND    tipo_informe = vtipo_edocta)

      UPDATE cta_ctr_proceso
      SET    estado       = estado + 1
      WHERE  factualiza  >= vfecha_liq
      AND    tipo_informe = vtipo_edocta
      AND    estado       = 3

      LET i = i + 1
   END FOREACH

   ERROR "PROCESO TERMINADO ..."
   SLEEP 2

END FUNCTION
#############################################################################
#eof
