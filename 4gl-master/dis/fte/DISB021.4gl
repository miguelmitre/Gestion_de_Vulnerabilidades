-------------------------------------------------------------------------------
--Proyecto      => Sistema de Afores. (MEXICO)
--Propietario   => E.F.P.
--Modulo        => DIS
--Programa      => DISB021
--Autor         => GERARDO ALFONSO VEGA PAREDES.
--Fecha         => 08 de MARZO 2010   
--Descripcion   => Reverso de Provision y Liquidacion Dispersion
-------------------------------------------------------------------------------

DATABASE safre_af

GLOBALS
   DEFINE hoy     DATE
   DEFINE opc     CHAR(01)
   DEFINE vfolio  INTEGER
   DEFINE vsubcta CHAR(07)
   DEFINE vconsec INTEGER
   

   DEFINE vpasswd       CHAR(01)

   DEFINE g_reg4 RECORD
          super_cod     SMALLINT,
          super_desc    CHAR(30),
          nip           INTEGER
   END RECORD

   DEFINE vnip          INTEGER

   DEFINE vproceso_cod  CHAR(05)

   DEFINE vfecha_liquidacion DATE
   
   DEFINE vcontador  SMALLINT

END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O
  
   DEFER INTERRUPT

--   LET vpasswd = "N"

--   CALL Aplica_passwd() RETURNING vpasswd

--   IF vpasswd="S" THEN
--      ERROR "Acceso aceptado"
--      SLEEP 2
--      ERROR ""
      CALL inicio()
--   END IF
END MAIN


FUNCTION Aplica_passwd()
   OPEN WINDOW ventana_4 AT 08,12 WITH FORM "DISB0213" ATTRIBUTE(BORDER)
   DISPLAY " [Esc] Procesar      [DISB021]      [Ctrl-c] Cancelar " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME g_reg4.*

      AFTER FIELD super_cod
         IF g_reg4.super_cod IS NULL THEN
            ERROR "ID no puede ser nulo"
            NEXT FIELD super_cod
         END IF

         SELECT super_desc,
                nip
         INTO g_reg4.super_desc,
              vnip 
         FROM tab_supervisor 
         WHERE super_cod = g_reg4.super_cod

         IF STATUS = NOTFOUND THEN
            ERROR "No existe este ID "
            NEXT FIELD super_cod
         END IF

         DISPLAY BY NAME g_reg4.super_desc
         NEXT FIELD nip

      AFTER FIELD nip
         IF g_reg4.nip <> vnip THEN
            ERROR "Clave de acceso no existe"
            LET g_reg4.nip = null
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip IS NULL OR
            g_reg4.nip = 0 THEN
            ERROR "Clave de acceso no existe"
            NEXT FIELD super_cod
         END IF

      ON KEY (ESC)
         IF g_reg4.super_cod IS NULL THEN
            ERROR "ID no puede ser nulo"
            NEXT FIELD super_cod
         END IF

         SELECT "x"
         FROM tab_supervisor 
         WHERE super_cod = g_reg4.super_cod

         IF STATUS = NOTFOUND THEN
            ERROR "No existe este ID "
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip IS NULL OR
            g_reg4.nip = 0 THEN
            ERROR "Clave de acceso no existe"
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip <> vnip THEN
            ERROR "Clave de acceso no existe"
            LET g_reg4.nip = null
            NEXT FIELD super_cod
         END IF

         LET vpasswd = "S"
         EXIT INPUT

      ON KEY (INTERRUPT)
         ERROR "Acceso denegado"
         SLEEP 2
         LET vpasswd = "N"
         EXIT INPUT


   END INPUT

   CLOSE WINDOW ventana_4
   RETURN vpasswd
END FUNCTION

FUNCTION inicio()
   LET hoy = TODAY

   OPEN WINDOW ventana AT 3,4
   WITH 3 ROWS, 72 COLUMNS
   ATTRIBUTE(BORDER)

   DISPLAY " DISB021 " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)

   MENU "Reverso Dispersion" 
      COMMAND "Carga y Provision" "Reverso Provision y Carga Archivo"
         CALL Reversar_provision()
      COMMAND "Liquidación" "Menu Reverso Liquidacion"
         CALL Menu_liquidacion()
      COMMAND "Salir" "Salir"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana
END FUNCTION

FUNCTION Menu_liquidacion()
   MENU "Reverso Liquidacion"
      COMMAND "Parcial"  "Reverso Parcial de Liquidacion"
         CALL Reversar_liquidacion_parcial()
      COMMAND "Completo" "Reverso Completo de Liquidacion"
         CALL Reversar_liquidacion_completa()
      COMMAND "Subscte"  "Reverso Liquidacion Subsecuente"
         CALL Reversar_liquidacion_subscte()
      COMMAND "Regresar" "Regresar a menu anterior"
         EXIT MENU
   END MENU

END FUNCTION

FUNCTION Reversar_provision()
   CLEAR SCREEN

   OPEN WINDOW ventana_1 AT 8,4 WITH FORM "DISB0211" ATTRIBUTE(BORDER)
   DISPLAY " (Control-c) Salir                         (ESC) Ejecutar Reverso              " AT 1,1 ATTRIBUTE(REVERSE,green)
   
   LET INT_FLAG = FALSE

   INPUT BY NAME vfolio
      ON KEY (ESC)
         LET INT_FLAG = FALSE
         EXIT INPUT

      ON KEY (Control-c)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      CLEAR FORM
      ERROR "CANCELACION DE REVERSO DE CARGA Y PROVISION"
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   SELECT folio        
   FROM   dis_cza_aporte
   WHERE  folio = vfolio
   GROUP  BY folio

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "FOLIO NO ENCONTRADO EN LA BASE DE DATOS..."
      SLEEP 2
      ERROR ""
      CLEAR WINDOW ventana_1
      CLOSE WINDOW ventana_1
      RETURN
   ELSE

      SELECT UNIQUE folio
      FROM   dis_cuenta
      WHERE  folio = vfolio
      AND    tipo_movimiento <> 3

      IF SQLCA.SQLCODE <> NOTFOUND THEN
         ERROR "NO PUEDES REVERSAR CARGA/PROVISION PORQUE EXISTE LIQUIDACION DE DISPERSION..."
         SLEEP 3
         ERROR ""
         CLEAR WINDOW ventana_1
         CLOSE WINDOW ventana_1
         RETURN
      END IF 

      PROMPT "Desea Reversar Carga y Provision Dispersion (S/N):  " FOR CHAR opc

      IF opc MATCHES "[Ss]" THEN
         ERROR "REVERSANDO CARGA Y PROVISION..."
         DELETE FROM  dis_ctrl_proceso WHERE folio = vfolio
         DELETE FROM  dis_cza_aporte   WHERE folio = vfolio
         DELETE FROM  dis_det_aporte   WHERE folio = vfolio
         DELETE FROM  dis_sum_aporte   WHERE folio = vfolio
         DELETE FROM  dis_cza_interes  WHERE folio = vfolio
         DELETE FROM  dis_det_interes  WHERE folio = vfolio
         DELETE FROM  dis_sum_interes  WHERE folio = vfolio
         DELETE FROM  dis_dep_aporte   WHERE folio = vfolio
         DELETE FROM  dis_provision    WHERE folio = vfolio
         SLEEP 2
         ERROR "" 
      ELSE 
         ERROR "CANCELACION DE REVERSO DE CARGA Y PROVISION..."
         SLEEP 2
      END IF

      CLEAR SCREEN
      CLOSE WINDOW ventana_1
   END IF
END FUNCTION

FUNCTION Reversar_liquidacion_parcial()
   CLEAR SCREEN

   OPEN WINDOW ventana_2 AT 8,4 WITH FORM "DISB0212" ATTRIBUTE(BORDER)
   DISPLAY " (Control-c) Salir                       (ESC) Ejecutar Reverso                " AT 1,1 ATTRIBUTE(REVERSE,green)
   
   LET INT_FLAG = FALSE

   INPUT BY NAME vfolio,vsubcta
      AFTER FIELD vfolio
         IF vfolio IS NULL THEN
            ERROR "EL FOLIO NO PUEDE SER NULO"
            NEXT FIELD vfolio
         END IF
      AFTER FIELD vsubcta
         IF (vsubcta <> "RCVVIV" AND
            vsubcta <> "EST")    OR
            vsubcta IS NULL  THEN
            ERROR "Se debe capturar RCVVIV o EST"
            NEXT FIELD vsubcta
         END IF
      ON KEY (ESC)

         IF vfolio IS NULL THEN
            ERROR "EL FOLIO NO PUEDE SER NULO"
            NEXT FIELD vfolio
         END IF

         IF (vsubcta <> "RCVVIV" AND
            vsubcta <> "EST")    OR
            vsubcta IS NULL  THEN
            ERROR "Se debe capturar RCVVIV o EST"
            NEXT FIELD vsubcta
         END IF

         LET INT_FLAG = FALSE
         EXIT INPUT

      ON KEY (Control-c)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      CLEAR FORM
      ERROR "CANCELACION DE REVERSO LIQUIDACION PARCIAL"
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_2
      RETURN
   END IF

   LET vfecha_liquidacion = NULL

   IF vsubcta = "RCVVIV" THEN

      SELECT "X"
      FROM   cta_saldo_vol
      WHERE  folio = vfolio
      AND    monto_en_acciones <> saldo_acciones
      GROUP  BY 1

      IF SQLCA.SQLCODE <> NOTFOUND THEN
         ERROR "NO PUEDES REVERSAR LIQ RCV PORQUE EXISTE RETIRO DE VOLUNTARIAS"
         SLEEP 3
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      SELECT UNIQUE fecha_conversion
      INTO   vfecha_liquidacion
      FROM   dis_cuenta
      WHERE  folio = vfolio
      AND    subcuenta in (1,2,3,4,11,15,17)
      AND    tipo_movimiento not in (7,8,38)

      IF vfecha_liquidacion <> hoy THEN
         ERROR "NO SE PUEDE REVERSAR LIQ RCVVIV, YA QUE LA FECHA LIQ ES DIFERENTE A LA DEL DIA"
         SLEEP 3
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      SELECT UNIQUE folio        
      FROM   dis_cuenta
      WHERE  folio = vfolio
      AND    subcuenta in (1,2,3,4,11,15,17)
      AND    tipo_movimiento not in (7,8,38)
   END IF

   IF vsubcta = "EST" THEN
      SELECT UNIQUE fecha_conversion
      INTO   vfecha_liquidacion
      FROM   dis_cuenta
      WHERE  folio = vfolio
      AND    subcuenta in (5,6,9)

      IF vfecha_liquidacion <> hoy THEN
         ERROR "NO SE PUEDE REVERSAR LIQ EST, YA QUE LA FECHA LIQ ES DIFERENTE A LA DEL DIA"
         SLEEP 3
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      SELECT UNIQUE folio        
      FROM   dis_cuenta
      WHERE  folio = vfolio
      AND    subcuenta in (5,6,9)
   END IF

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "FOLIO NO ENCONTRADO EN LA BASE DE DATOS..."
      SLEEP 2
      ERROR ""
      CLEAR WINDOW ventana_2
      CLOSE WINDOW ventana_2
      RETURN
   ELSE

      PROMPT "Desea Reversar Liquidacion Parcial Dispersion (S/N):  " FOR CHAR opc

      IF opc MATCHES "[Ss]" THEN

         IF vsubcta = "RCVVIV" THEN

            SELECT UNIQUE folio        
            FROM   dis_cuenta
            WHERE  folio = vfolio
            AND    subcuenta in (5,6,9)

            IF SQLCA.SQLCODE <> NOTFOUND THEN
               ERROR "PRIMERO TIENES QUE REVERSAR EST..." 
               SLEEP 3
               CLEAR SCREEN
               CLOSE WINDOW ventana_2
               RETURN
            END IF

            ERROR "REVERSANDO LIQUIDACION RCVVIV..."
            DELETE 
            FROM  dis_ctrl_proceso 
            WHERE folio = vfolio 
            AND   proceso_cod = "DISB007B"
            AND   parametro3 = "RCV"

            SELECT MIN(consecutivo) 
            INTO   vconsec
            FROM   dis_ctrl_proceso
            WHERE  folio = vfolio 
            AND proceso_cod = "DISB078B"

            DELETE 
            FROM  dis_ctrl_proceso 
            WHERE folio = vfolio 
            AND   proceso_cod = "DISB078B"
            AND   consecutivo = vconsec

            DELETE 
            FROM   dis_cuenta
            WHERE  folio = vfolio
            AND    subcuenta in (1,2,3,4,11,15,17)
            AND    tipo_movimiento not in (7,8,38)

            UPDATE dis_dep_aporte
            SET    estado = 2
            WHERE  folio  = vfolio
            AND    ident_pago[14,15] in (11,21,23,25,26,20,31,41,43,45,46,47,48,49,40)

            CALL Reversa_rehabilita()
            CALL Reversa_tes()
            CALL Reversa_saldo_vol()

         END IF

         IF vsubcta = "EST" THEN
            SELECT UNIQUE folio        
            FROM   dis_cuenta
            WHERE  folio = vfolio
            AND    subcuenta = 4
            AND    tipo_movimiento in (7,8,38)

            IF SQLCA.SQLCODE <> NOTFOUND THEN
               ERROR "PRIMERO TIENES QUE REVERSAR SUBSCTE EN LA OPCION DEL MENU Reveso Liquidacion..." 
               SLEEP 3
               CLEAR SCREEN
               CLOSE WINDOW ventana_2
               RETURN
            END IF

            ERROR "REVERSANDO LIQUIDACION EST..."
            DELETE 
            FROM  dis_ctrl_proceso 
            WHERE folio = vfolio 
            AND   proceso_cod = "DISB007B"
            AND   parametro3 = "EST"

            SELECT MIN(consecutivo) 
            INTO   vconsec
            FROM   dis_ctrl_proceso
            WHERE  folio = vfolio 
            AND proceso_cod = "DISB078B"

            DELETE 
            FROM  dis_ctrl_proceso 
            WHERE folio = vfolio 
            AND   proceso_cod = "DISB078B"
            AND   consecutivo = vconsec

            DELETE 
            FROM   dis_cuenta
            WHERE  folio = vfolio
            AND    subcuenta in (5,6,9)

            UPDATE dis_dep_aporte
            SET    estado = 2
            WHERE  folio  = vfolio
            AND    ident_pago[14,15] in (12,42,22,32)

            CALL Reversa_rehabilita()
            CALL Reversa_tes()

         END IF
      ELSE 
         ERROR "CANCELACION DE REVERSO DE LIQUIDACION..."
         SLEEP 2
      END IF

      CLEAR SCREEN
      CLOSE WINDOW ventana_2
   END IF
END FUNCTION

FUNCTION Reversa_tes()

   DELETE
   FROM   cta_solicitud_regimen
   WHERE  fecha_solicitud = vfecha_liquidacion
   AND    folio_solicitud in 
          (SELECT folio_solicitud 
           FROM   tes_solicitud
           WHERE  folio_origen    = vfolio  
           AND    fecha_solicitud = vfecha_liquidacion
           AND    estado          = 100)

   DELETE
   FROM   tes_solicitud
   WHERE  folio_origen    = vfolio
   AND    fecha_solicitud = vfecha_liquidacion
   AND    estado = 100

END FUNCTION

FUNCTION Reversa_rehabilita()
   
   INSERT INTO cta_act_marca
   SELECT *
   FROM   cta_his_inhabilitada
   WHERE  nss in (SELECT nss
                  FROM   cta_rehabilitada
                  WHERE  folio            = vfolio
                  AND    fecha_rehabilita = vfecha_liquidacion)

   DELETE
   FROM   cta_rehabilitada
   WHERE  folio            = vfolio
   AND    fecha_rehabilita = vfecha_liquidacion 

   DELETE 
   FROM   cta_his_inhabilitada
   WHERE  nss in (SELECT nss
                  FROM   cta_act_marca act,
                         tab_marca     tab
                  WHERE  act.marca_cod = tab.marca_cod
                  AND    act.marca_cod = 140
                  AND    tab.ind_habilita = 2)


END FUNCTION

FUNCTION Reversa_saldo_vol()

   DELETE
   FROM   cta_saldo_vol
   WHERE  folio = vfolio
   AND    monto_en_acciones = saldo_acciones

END FUNCTION

FUNCTION Reversar_liquidacion_completa()
   CLEAR SCREEN

   OPEN WINDOW ventana_1 AT 8,4 WITH FORM "DISB0211" ATTRIBUTE(BORDER)
   DISPLAY " (Control-c) Salir                         (ESC) Ejecutar Reverso              " AT 1,1 ATTRIBUTE(REVERSE,green)
   
   LET INT_FLAG = FALSE

   INPUT BY NAME vfolio
      ON KEY (ESC)
         LET INT_FLAG = FALSE
         EXIT INPUT

      ON KEY (Control-c)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      CLEAR FORM
      ERROR "CANCELACION DE REVERSO DE LIQUIDACION COMPLETA"
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   SELECT "X"
   FROM   cta_saldo_vol
   WHERE  folio = vfolio
   AND    monto_en_acciones <> saldo_acciones
   GROUP  BY 1

   IF SQLCA.SQLCODE <> NOTFOUND THEN
      ERROR "NO PUEDES REVERSAR LIQ DIS PORQUE EXISTE RETIRO DE VOLUNTARIAS"
      SLEEP 3
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   SELECT UNIQUE folio        
   FROM   dis_cuenta
   WHERE  folio = vfolio
   AND    subcuenta = 4
   AND    tipo_movimiento in (7,8,38)

   IF SQLCA.SQLCODE <> NOTFOUND THEN
      ERROR "PRIMERO TIENES QUE REVERSAR SUBSCTE EN LA OPCION DEL MENU Reveso Liquidacion..." 
      SLEEP 3
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   LET vcontador = NULL

   SELECT COUNT(UNIQUE fecha_conversion)
   INTO   vcontador
   FROM   dis_cuenta
   WHERE  folio = vfolio
   AND    subcuenta in (1,2,3,4,5,6,9,11,15,17)
   AND    tipo_movimiento not in (7,8,38)

   IF vcontador = 2 THEN
      ERROR "NO PUEDES REVERSAR LIQ COMPLETA,YA QUE LA FECHA LIQ DE RCV Y EST ES DIFERENTE"
      SLEEP 3
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   LET vfecha_liquidacion = NULL

   SELECT UNIQUE fecha_conversion
   INTO   vfecha_liquidacion
   FROM   dis_cuenta
   WHERE  folio = vfolio
   AND    subcuenta in (1,2,3,4,5,6,9,11,15,17)
   AND    tipo_movimiento not in (7,8,38)

   IF vfecha_liquidacion <> hoy THEN
      ERROR "NO SE PUEDE REVERSAR LIQ COMPLETA,YA QUE LA FECHA LIQ ES DIFERENTE A LA DEL DIA"
      SLEEP 3
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   SELECT UNIQUE folio
   FROM   dis_cuenta
   WHERE  folio = vfolio
   AND    subcuenta in (1,2,3,4,5,6,9,11,15,17)
   AND    tipo_movimiento not in (7,8,38)

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "FOLIO NO ENCONTRADO EN LA BASE DE DATOS..."
      SLEEP 2
      ERROR ""
      CLEAR WINDOW ventana_1
      CLOSE WINDOW ventana_1
      RETURN
   ELSE

      PROMPT "Desea Reversar Liquidacion Completa (S/N):  " FOR CHAR opc

      IF opc MATCHES "[Ss]" THEN

         ERROR "REVERSANDO LIQUIDACION COMPLETA..."
         DELETE 
         FROM  dis_ctrl_proceso 
         WHERE folio = vfolio 
         AND   proceso_cod = "DISB007B"
         AND   parametro3 <> "GAR"

         DELETE 
         FROM  dis_ctrl_proceso 
         WHERE folio = vfolio 
         AND   proceso_cod = "DISB078B"

         DELETE 
         FROM   dis_cuenta
         WHERE  folio = vfolio
         AND    subcuenta in (1,2,3,4,5,6,9,11,15,17)
         AND    tipo_movimiento not in (7,8,38)

         UPDATE dis_dep_aporte
         SET    estado = 2
         WHERE  folio  = vfolio
         AND ident_pago[14,15] in (11,12,21,22,23,25,26,20,31,32,41,42,43,45,46,47,48,49,40)

         CALL Reversa_rehabilita()
         CALL Reversa_tes()
         CALL Reversa_saldo_vol()

         SLEEP 2
         ERROR "" 
      ELSE 
         ERROR "CANCELACION DE REVERSO DE LIQUIDACION COMPLETA..."
         SLEEP 2
      END IF

      CLEAR SCREEN
      CLOSE WINDOW ventana_1
   END IF

END FUNCTION

FUNCTION Reversar_liquidacion_subscte()
   CLEAR SCREEN

   OPEN WINDOW ventana_2 AT 8,4 WITH FORM "DISB0214" ATTRIBUTE(BORDER)
   DISPLAY " (Control-c) Salir                       (ESC) Ejecutar Reverso                " AT 1,1 ATTRIBUTE(REVERSE,green)
   
   LET INT_FLAG = FALSE

   INPUT BY NAME vfolio,vsubcta
      AFTER FIELD vfolio
         IF vfolio IS NULL THEN
            ERROR "EL FOLIO NO PUEDE SER NULO"
            NEXT FIELD vfolio
         END IF
      AFTER FIELD vsubcta
         IF vsubcta <> "SUBSCTE" OR
            vsubcta IS NULL      THEN
            ERROR "Se debe capturar SUBSCTE"
            NEXT FIELD vsubcta
         END IF

      ON KEY (ESC)

         IF vfolio IS NULL THEN
            ERROR "EL FOLIO NO PUEDE SER NULO"
            NEXT FIELD vfolio
         END IF

         IF vsubcta <> "SUBSCTE" OR
            vsubcta IS NULL      THEN
            ERROR "Se debe capturar SUBSCTE"
            NEXT FIELD vsubcta
         END IF

         LET INT_FLAG = FALSE
         EXIT INPUT

      ON KEY (Control-c)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      CLEAR FORM
      ERROR "CANCELACION DE REVERSO LIQUIDACION SUBSECUENTE"
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_2
      RETURN
   END IF

   LET vfecha_liquidacion = NULL

   SELECT UNIQUE fecha_conversion
   INTO   vfecha_liquidacion
   FROM   dis_cuenta
   WHERE  folio = vfolio
   AND    subcuenta = 4
   AND    tipo_movimiento in (7,8,38)

   IF vfecha_liquidacion <> hoy THEN
      ERROR "NO SE PUEDE REVERSAR LIQ SUBSCTE, YA QUE LA FECHA LIQ ES DIFERENTE A LA DEL DIA"
      SLEEP 3
      CLEAR SCREEN
      CLOSE WINDOW ventana_2
      RETURN
   END IF

   SELECT UNIQUE folio        
   FROM   dis_cuenta
   WHERE  folio = vfolio
   AND    subcuenta = 4
   AND    tipo_movimiento in (7,8,38)

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "FOLIO NO ENCONTRADO EN LA BASE DE DATOS..."
      SLEEP 2
      ERROR ""
      CLEAR WINDOW ventana_2
      CLOSE WINDOW ventana_2
      RETURN
   ELSE

      PROMPT "Desea Reversar Liquidacion Dispersion (S/N):  " FOR CHAR opc

      IF opc MATCHES "[Ss]" THEN

         ERROR "REVERSANDO LIQUIDACION SUBSCTE..."
         DELETE 
         FROM  dis_ctrl_proceso 
         WHERE folio = vfolio 
         AND   proceso_cod = "DISB007B"
         AND   parametro3 = "GAR"

         DELETE
         FROM   dis_cuenta
         WHERE  folio = vfolio
         AND    subcuenta = 4
         AND    tipo_movimiento in (7,8,38)

         UPDATE dis_dep_aporte
         SET    estado = 2
         WHERE  folio  = vfolio
         AND    ident_pago[14,15] in (14,24,34,44)

         SLEEP 2
         ERROR "" 
      ELSE 
         ERROR "CANCELACION DE REVERSO DE LIQUIDACION SUBSECUENTE..."
         SLEEP 2
      END IF

      CLEAR SCREEN
      CLOSE WINDOW ventana_2
   END IF
END FUNCTION
