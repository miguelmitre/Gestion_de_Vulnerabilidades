######################################################################
#Proyecto            => Sistema de Afores. (MEXICO)                  #
#Propietario         => E.F.P.                                       #
#Modulo              => DIS                                          #
#Programa            => DISB050                                      #
#Descripcion         => Reverso Liq. Acl, rcv y viv, est             #
#Autor               => GERARDO ALFONSO VEGA PAREDES                 #
#Fecha               => 06 agosito 2003.                             #
#--------------------------------------------------------------------#
#Modificado          => (v1) Alejandro Ramirez 31-Oct-2005           #
#Descripcion         => Se agrega el borrado en cta_saldo_vol en la  #
#                    => liquidacion.                                 #
######################################################################
DATABASE safre_af

GLOBALS

  DEFINE hoy DATE

  DEFINE opc CHAR(01)

  DEFINE vfolio INTEGER

  DEFINE v RECORD    
     subcta  CHAR(06),
     vfolio1 INTEGER
  END RECORD,

  vpasswd CHAR(01)

   DEFINE g_reg4 RECORD
      super_cod  SMALLINT,
      super_desc CHAR(30),
      nip        INTEGER
   END RECORD,

   vnip INTEGER

   DEFINE vproceso_cod CHAR(05)

   DEFINE cla_sel CHAR(500),
          vcont   INTEGER

    DEFINE xfolio INTEGER,      --v1
           xnss   CHAR(11),
           xconsec INTEGER
END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O
  
   DEFER INTERRUPT

   LET vpasswd = "N"

   CALL Aplica_passwd() RETURNING vpasswd
   IF vpasswd="S" THEN
      ERROR "Acceso aceptado"
      SLEEP 2
      ERROR ""
      CALL inicio()
   END IF

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

  DISPLAY " DISB050 " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY hoy USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)

  MENU "INICIO"
     COMMAND "Reverso 1" "Reverso Liquidacion RCV y VIV o EST."
        CALL Reverso_1()
     COMMAND "Reverso 2" "Reverso Liquidacion Aclaraciones"
        CALL Reverso_2()
     COMMAND "Salir" "Salir del menu principal."
        EXIT MENU
  END MENU

  CLOSE WINDOW ventana

END FUNCTION

FUNCTION Reverso_1()

  CLEAR SCREEN

  OPEN WINDOW ventana_2 AT 8,4 WITH FORM "DISB0501" ATTRIBUTE(BORDER)

  DISPLAY " (Control-c) Salir        REVERSO 1              (ESC) Ejecutar                " AT 1,1 ATTRIBUTE(REVERSE,green)

  LET int_flag = FALSE

  INPUT BY NAME v.*
     AFTER FIELD subcta
        IF v.subcta <> 'RCVVIV' AND
           v.subcta <> 'EST' THEN
           ERROR "Debe capturar RCVVIV o EST"
           SLEEP 2
           NEXT FIELD subcta
        END IF

        IF v.subcta IS NULL THEN
           ERROR "El campo no puede ser nulo"
           SLEEP 2
           NEXT FIELD subcta
        END IF 

        IF v.subcta = "RCVVIV" THEN
           LET vproceso_cod = "00003"
        END IF

        IF v.subcta = "EST" THEN
           LET vproceso_cod = "00004"
        END IF

     AFTER FIELD vfolio1
        IF v.vfolio1 IS NULL THEN
           ERROR "El folio no puede ser nulo"
           NEXT FIELD vfolio1
        END IF

      { --ya no aplica
        SELECT COUNT(*)
        INTO   vcont
        FROM   dis_dep_aporte
        WHERE  folio =v.vfolio1

        IF vcont > 6 THEN
           ERROR "ESTE FOLIO ES DE ACLARACIONES, REVERSAR EN LA OTRA OPCION"
           NEXT FIELD vfolio1 
        END IF    
      }
        

        DECLARE cur_cont2 CURSOR FOR
        SELECT "X"
        FROM    con_transaccion
        WHERE   folio = v.vfolio1
        AND     proceso_cod = vproceso_cod
        AND     estado = 40
        OPEN cur_cont2
        FETCH cur_cont2
        IF STATUS <> NOTFOUND THEN
           ERROR "NO PUEDES REVERSAR PORQUE ESTA CONTABILIZADA LA CUENTA"
           SLEEP 3
           LET int_flag = TRUE
           CLOSE cur_cont2
           EXIT INPUT
        END IF

        CLOSE cur_cont2

      ON KEY (ESC)
         IF v.subcta <> 'RCVVIV' AND
            v.subcta <> 'EST' THEN 
            ERROR "Debe capturar RCVVIV o EST"
            SLEEP 2
            NEXT FIELD subcta
         END IF
         IF v.subcta IS NULL THEN
            ERROR "El campo no puede ser nulo"
            SLEEP 2
            NEXT FIELD subcta
         END IF 

         IF v.vfolio1 IS NULL THEN
            ERROR "El folio no puede ser nulo"
            NEXT FIELD vfolio1
         END IF

      {  --ya no aplica
        SELECT COUNT(*)
        INTO   vcont
        FROM   dis_dep_aporte
        WHERE  folio =v.vfolio1

        IF vcont > 6 THEN
           ERROR "ESTE FOLIO ES DE ACLARACIONES, REVERSAR EN LA OTRA OPCION"
           NEXT FIELD vfolio1 
        END IF    
       }
         LET int_flag = FALSE
         EXIT INPUT
      ON KEY (Control-c)
         LET int_flag = TRUE
         EXIT INPUT
  END INPUT

  IF int_flag = TRUE THEN
     LET int_flag = FALSE
     CLEAR FORM
     ERROR "PROCESO CANCELADO..."
     SLEEP 2
     ERROR ""
     CLEAR SCREEN
     CLOSE WINDOW ventana_2
     RETURN
  END IF

  IF v.subcta = "RCVVIV" THEN
     LET cla_sel = "SELECT folio ",
                   "FROM   dis_cuenta ",
                   "WHERE  folio = ",v.vfolio1,
                   "AND    subcuenta in (1,2,3,4,11,15,17) ",  --c22-11
                   "AND    tipo_movimiento in (1,2,100,101,102,103,104,105,106) "
                   CLIPPED
  ELSE
     LET cla_sel = "SELECT folio ",
                   "FROM   dis_cuenta ",
                   "WHERE  folio = ",v.vfolio1,
                   "AND    subcuenta in (5,6,9) ",
                   "AND    tipo_movimiento in (1,2,100,101,102,103,104,105,106) "
                   CLIPPED
  END IF

  PREPARE claexe_sel FROM cla_sel
  DECLARE cur_fol2 CURSOR FOR claexe_sel

  OPEN cur_fol2

  FETCH cur_fol2

  IF SQLCA.SQLCODE = NOTFOUND THEN
     ERROR "NO EXISTE ESTE FOLIO..."
     SLEEP 2
     ERROR ""
     CLEAR WINDOW ventana_2
     CLOSE WINDOW ventana_2
     CLOSE cur_fol2
     RETURN
  ELSE 
     ERROR ""
     PROMPT "Deseas reversar el folio S/N : " FOR CHAR opc
     IF opc MATCHES "[Ss]" THEN
       CASE
         WHEN v.subcta = "RCVVIV"
           ERROR "REVERSANDO INFORMACION..."

           UPDATE dis_dep_aporte
           SET    estado = 2
           WHERE  folio  = v.vfolio1
         AND    ident_pago[14,15] in (11,41,43,44,23,31,32,47,48,49,40) --c22-11

           --Borrando en cta_saldo_vol ---------------------v1
           LET xfolio  = 0 
           LET xnss    = 0
           LET xconsec = 0
         {
           DECLARE c_51 CURSOR FOR            
           SELECT folio,nss,consecutivo_lote
           FROM    dis_cuenta
           WHERE folio = v.vfolio1
           AND       tipo_movimiento in (1,2,7,100,101,102,103,104,105,106,107)
           AND       subcuenta = 3
           GROUP BY 1,2,3

           FOREACH c_51 INTO xfolio,xnss,xconsec

                  DELETE FROM cta_saldo_vol
                  WHERE folio                 =xfolio
                  AND       nss               =xnss
                  AND       consecutivo_lote  =xconsec
                  AND       subcuenta          =3

           END FOREACH   -----------------------------------v1
          }

           DELETE FROM dis_cuenta
           WHERE  folio = v.vfolio1
           AND    subcuenta in (1,2,3,4,11,15,17)   --c22-11
          AND tipo_movimiento in (1,2,7,100,101,102,103,104,105,106,107,108,109)

         WHEN v.subcta = "EST"
           ERROR "PROCESANDO INFORMACION....EST"

           UPDATE dis_dep_aporte
           SET    estado = 2
           WHERE  folio  = v.vfolio1
           AND    ident_pago[14,15] in (11,42)

           DELETE FROM dis_cuenta
           WHERE  folio     = v.vfolio1
           AND    subcuenta in (5,6,9)
         AND    tipo_movimiento in (1,2,100,101,102,103,104,105,106,107,108,109)

       END CASE  
     ELSE
        ERROR "PROCESO CANCELADO..."
        SLEEP 2
     END IF

     CLOSE cur_fol2
     CLEAR SCREEN
     CLOSE WINDOW ventana_2
  END IF
END FUNCTION

FUNCTION Reverso_2()

  CLEAR SCREEN

  OPEN WINDOW ventana_2 AT 8,4 WITH FORM "DISB0501" ATTRIBUTE(BORDER)

  DISPLAY " (Control-c) Salir          REVERSO 2            (ESC) Ejecutar                " AT 1,1 ATTRIBUTE(REVERSE,green)
   
  LET int_flag = FALSE

  INPUT BY NAME v.*
     BEFORE FIELD subcta
        NEXT FIELD vfolio1

     AFTER FIELD vfolio1
        IF v.vfolio1 IS NULL THEN
           ERROR "El folio no puede ser nulo"
           NEXT FIELD vfolio1
        END IF

        SELECT COUNT(*)
        INTO   vcont
        FROM   dis_det_aporte
        WHERE  folio =v.vfolio1

        IF v.vfolio1 <= 6 THEN
           ERROR "ESTE FOLIO NO ES DE ACLARACIONES, REVERSAR EN LA OTRA OPCION"
           NEXT FIELD vfolio1 
        END IF    

        LET vproceso_cod = "00001"
        
        DECLARE cur_cont CURSOR FOR
        SELECT "X"
        FROM    con_transaccion
        WHERE   folio = v.vfolio1
        AND     proceso_cod = vproceso_cod
        AND     estado = 40
        OPEN cur_cont
        FETCH cur_cont
        IF STATUS <> NOTFOUND THEN
           ERROR "NO PUEDES REVERSAR PORQUE ESTA CONTABILIZADA LA CUENTA"
           SLEEP 3
           LET int_flag = TRUE
           CLOSE cur_cont
           EXIT INPUT
        END IF

        CLOSE cur_cont

     ON KEY (ESC)
        IF v.vfolio1 IS NULL THEN
           ERROR "El folio no puede ser nulo"
           NEXT FIELD vfolio1
        END IF

        SELECT COUNT(*)
        INTO   vcont
        FROM   dis_det_aporte
        WHERE  folio =v.vfolio1
        IF v.vfolio1 <= 6 THEN
           ERROR "ESTE FOLIO NO ES DE ACLARACIONES, REVERSAR EN LA OTRA OPCION"
           NEXT FIELD vfolio1 
        END IF    

        LET int_flag = FALSE
        EXIT INPUT
     ON KEY (Control-c)
        LET int_flag = TRUE
        EXIT INPUT
  END INPUT

  IF int_flag = TRUE THEN
     LET int_flag = FALSE
     CLEAR FORM
     ERROR "PROCESADO CANCELADO..."
     SLEEP 2
     ERROR ""
     CLEAR SCREEN
     CLOSE WINDOW ventana_2
     RETURN
  END IF

  ERROR "BUSCANDO FOLIO..."
  DECLARE cur_fol CURSOR FOR
  SELECT folio
  FROM   dis_cuenta
  WHERE  folio = v.vfolio1
  AND    subcuenta in (1,2,3,4,5,6,9,11,15,17)  --c22-11
  AND    tipo_movimiento in (1,2,3,4,7,100,101,102,103,104,105,106,107,108,109)

  OPEN cur_fol
  FETCH cur_fol

  IF SQLCA.SQLCODE = NOTFOUND THEN
     ERROR "NO EXISTE ESTE FOLIO..."
     SLEEP 2
     ERROR ""
     CLEAR WINDOW ventana_2
     CLOSE WINDOW ventana_2
     CLOSE cur_fol
     RETURN
  ELSE 
     ERROR ""
     PROMPT "Deseas reversar el folio S/N : " FOR CHAR opc
     IF opc MATCHES "[Ss]" THEN

        ERROR "REVERSANDO INFORMACION..."
              
        UPDATE dis_dep_aporte
        SET    estado = 2
        WHERE  folio = v.vfolio1

        --Borrando en cta_saldo_vol ---------------------v1
        LET xfolio  = 0
        LET xnss    = 0
        LET xconsec = 0
  
      {
        DECLARE c_52 CURSOR FOR
        SELECT folio,nss,consecutivo_lote
        FROM    dis_cuenta
        WHERE folio = v.vfolio1
        AND       tipo_movimiento in (1,2,3,4,7,100,101,102,103,104,105,106,107)
        AND       subcuenta = 3
        GROUP BY 1,2,3

        FOREACH c_52 INTO xfolio,xnss,xconsec

               DELETE FROM cta_saldo_vol
               WHERE folio                 =xfolio
               AND       nss               =xnss
               AND       consecutivo_lote  =xconsec
               AND       subcuenta          =3

        END FOREACH   -----------------------------------v1
      }

        DELETE FROM dis_cuenta
        WHERE  folio = v.vfolio1
        AND    subcuenta in (1,2,3,4,5,6,9,11,15,17)    --c22-11
      AND tipo_movimiento in (1,2,3,4,7,100,101,102,103,104,105,106,107,108,109)

     ELSE
        ERROR "PROCESO CANCELADO..."
        SLEEP 2
     END IF


     CLOSE cur_fol

     CLEAR SCREEN
     CLOSE WINDOW ventana_2
  END IF
END FUNCTION
  
