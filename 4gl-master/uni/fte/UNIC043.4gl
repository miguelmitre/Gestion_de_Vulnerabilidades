#############################################################################
#Proyecto     => safre_af                                                   #
#Propietario  => E.F.P.                                                     #
#Programa     => UNIC043                                                    #
#Descripcion  => ELIMINA LA MARCA DE UNIFICACION (241-244)                  #
#Por          => OMAR SANDOVAL BADILLO                                      #
#Fecha        => 26 DE JULIO 2005                                           #
#Sistema      => UNI                                                        #
#############################################################################
DATABASE safre_af
GLOBALS

   DEFINE hoy       DATE,
          usuario   CHAR(8),
          cla_where CHAR(200),
          sel_where CHAR(200),
          opc       CHAR(1)

   DEFINE g_parametro RECORD LIKE seg_modulo.*

   DEFINE g_reg ARRAY[31999] OF RECORD LIKE cta_act_marca.*

   DEFINE clave_afore    SMALLINT

END GLOBALS
#############################################################################
MAIN

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   CALL STARTLOG("UNIC043.log")

   SELECT codigo_afore
   INTO   clave_afore
   FROM   tab_afore_local

   LET hoy = TODAY

   CALL inicio()

END MAIN
#############################################################################
FUNCTION inicio()

   SELECT *,
          USER
   INTO   g_parametro.*,
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   OPEN WINDOW v1 AT 2,2 WITH FORM "UNIC0431" ATTRIBUTE(BORDER)
   DISPLAY " UNIC043                  CONSULTA MARCAS ACTIVAS                              " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "DESMARCA"
      COMMAND "Consulta" "Consulta de registros con marcas activas"
         CALL proceso()
      COMMAND "Salida" "Salida del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW v1

END FUNCTION
#############################################################################
FUNCTION proceso()

   DEFINE l_reg  ARRAY[31999] OF RECORD
          nss_marca       CHAR(11),
          cod_marca       SMALLINT,
          fecha_ini       DATE,
          trap            CHAR(1)
   END RECORD

   DEFINE pos      INTEGER,
          arr_c    INTEGER,
          arr_l    INTEGER,
          arr_t    INTEGER,
          cont_inp INTEGER,
          i        INTEGER

   DISPLAY "        NSS          MARCA ACTIVA " AT 5,1 

   LET int_flag = FALSE

   CONSTRUCT cla_where ON a.nss,
                          a.marca_cod
                     FROM x_nss,
                          x_marca_cod

      ON KEY(ESC)
         ERROR "PROCESANDO INFORMACION..."
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY(CONTROL-C)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      DISPLAY "                                  " AT 5,1 
      CLEAR FORM
      RETURN
   END IF

   LET cont_inp = TRUE
   WHILE cont_inp

      INITIALIZE l_reg TO NULL
 
      LET sel_where = " SELECT a.nss,",
                              "a.marca_cod,",
                              "a.fecha_ini,",
                              "' '",
                      " FROM  cta_act_marca a",
                      " WHERE ",cla_where CLIPPED,
                      " ORDER BY 2,3,1"
      PREPARE query1 FROM sel_where
      DECLARE cur_1 CURSOR FOR query1

      LET pos = 1

      FOREACH cur_1 INTO g_reg[pos].*
      
         IF pos = 31999 THEN
            ERROR "SOBREPASO EL LIMITE DE REGISTROS, SE MOSTRARAN ",pos," REGISTROS"
            SLEEP 4
            EXIT FOREACH
         END IF

         LET l_reg[pos].nss_marca      = g_reg[pos].nss
         LET l_reg[pos].cod_marca      = g_reg[pos].marca_cod
         LET l_reg[pos].fecha_ini      = g_reg[pos].fecha_ini 
         LET l_reg[pos].trap           = NULL

         IF g_reg[pos].marca_cod IS NULL THEN
            EXIT FOREACH
         END IF

         LET pos = pos + 1
      END FOREACH

      ERROR ""
      LET pos = pos - 1

      IF pos >= 1 THEN
         CALL SET_COUNT(pos)

         DISPLAY " [Esc] Desmarca                                              [Ctrl-C] Salir    " AT 4,1
         DISPLAY "        NSS          MARCA ACTIVA        FECHA MARCA         SELECCION         " AT 5,1 ATTRIBUTE(REVERSE) 

         INPUT ARRAY l_reg WITHOUT DEFAULTS FROM scr_1.*
         ATTRIBUTES(MAXCOUNT = pos,COUNT = pos)

            AFTER ROW
               LET arr_c = ARR_CURR()
               LET arr_l = SCR_LINE()
               LET arr_t = ARR_COUNT()

               IF l_reg[arr_c].trap MATCHES "[X]" AND
                  (l_reg[arr_c].cod_marca = 241 OR
                   l_reg[arr_c].cod_marca = 242 OR
                   l_reg[arr_c].cod_marca = 243 OR
                   l_reg[arr_c].cod_marca = 244) OR 
                   l_reg[arr_c].trap IS NULL THEN
               ELSE
                  ERROR "EL REGISTRO NO PUEDE SER SELECCIONADO"
                  LET l_reg[arr_c].trap = NULL
               END IF

               DISPLAY l_reg[arr_c].* TO scr_1[arr_l].*

               BEFORE ROW
                  LET arr_c = ARR_CURR()
                  LET arr_l = SCR_LINE()
                  LET arr_t = ARR_COUNT()

                  IF (arr_c = pos + 1) THEN
                     LET cont_inp = TRUE
                  ELSE
                     LET arr_l = SCR_LINE()
                     DISPLAY l_reg[arr_c].* TO scr_1[arr_l].*
                     LET cont_inp = FALSE
                  END IF

            ON KEY(ESC)
               FOR i = 1 TO pos
                   SELECT "X"
                   FROM   uni_unificador
                   WHERE  nss_uni = l_reg[i].nss_marca
                   #AND    estado IN(10,80)
                   AND cve_afo_recep <> clave_afore 
                   GROUP BY 1

                   IF SQLCA.SQLCODE = 0 AND 
                      l_reg[i].trap MATCHES "[X]" THEN

                      UPDATE cta_his_marca
                      SET    fecha_fin = hoy,
                             estado_marca = 40,
                             usr_desmarca = usuario
                      WHERE  nss = l_reg[i].nss_marca
                      AND    marca_cod = l_reg[i].cod_marca

                      DELETE FROM cta_act_marca
                      WHERE  nss = l_reg[i].nss_marca
                      AND    marca_cod = l_reg[i].cod_marca
                   ELSE
                     SELECT "X"
                     FROM   uni_unificado
                     WHERE  nss_cta1 = l_reg[i].nss_marca
                     #AND    estado IN(10,80)
                     AND    diag_unifica <> "01"
                     GROUP BY 1

                     IF SQLCA.SQLCODE = 0 AND 
                        l_reg[i].trap MATCHES "[X]" THEN
                     
                         UPDATE cta_his_marca
                         SET    fecha_fin = hoy,
                                estado_marca = 40,
                                usr_desmarca = usuario
                         WHERE  nss = l_reg[i].nss_marca
                         AND    marca_cod = l_reg[i].cod_marca

                         DELETE FROM cta_act_marca
                         WHERE  nss = l_reg[i].nss_marca
                         AND    marca_cod = l_reg[i].cod_marca
                     END IF
                  END IF
               END FOR 
               EXIT INPUT
 
            ON KEY(INTERRUPT)
               ERROR "PROCESO CANCELADO ..."
               SLEEP 2
               ERROR ""
               EXIT INPUT
         END INPUT   
         CLEAR FORM
         DISPLAY "                                                                               " AT 4,1
         DISPLAY "                                                                               " AT 5,1 
      ELSE
         ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR"
         SLEEP 2
         EXIT WHILE
      END IF
   END WHILE

END FUNCTION
#############################################################################
#EOF
