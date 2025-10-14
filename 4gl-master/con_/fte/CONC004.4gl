###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.       					      #
#Programa CONC004  => INTERFASE CONTABLE (PAGOS RETIROS)                      #
#Fecha             => 11 de octubre del 2004                                  #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES/FERNANDO HERRERA HDZ    #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE ret_1 RECORD LIKE ret_beneficiario.*
   DEFINE HOY          DATE,
	  enter        CHAR(1),
	  char         CHAR(1),
	  usuario      CHAR(12),
	  cont         INTEGER, 
	  codigo       INTEGER,
          reg_modulo   RECORD LIKE seg_modulo.*,
          xfecha_pago  DATE
END GLOBALS
###############################################################################
MAIN

   OPTIONS 
     INPUT WRAP,
     PROMPT LINE LAST,
     ACCEPT KEY CONTROL-I
     DEFER INTERRUPT 

   CALL inicio()  #pp
   CALL carga_pagos()  #pp
   CALL genera_archivo() #ga

END MAIN
###############################################################################
FUNCTION inicio()

   SELECT proceso_cod
   FROM   tab_proceso
   WHERE  movimiento = -1
   AND    descripcion matches "RET*"
   INTO TEMP pago_proceso

   SELECT *, USER
   INTO   reg_modulo.*, usuario
   FROM   seg_modulo
   WHERE  modulo_cod = 'con'

   LET HOY = TODAY

END FUNCTION
###############################################################################
FUNCTION carga_pagos()
#pp-------------------------

   DEFINE pesos_conta      DECIMAL(16,2)
   DEFINE xpesos           DECIMAL(16,2)
   DEFINE xpesos1          DECIMAL(16,2)
   DEFINE tpesos           DECIMAL(16,2)
   DEFINE tben_pesos       DECIMAL(16,2)
   DEFINE xnss             CHAR(11)
   DEFINE xporcentaje      DECIMAL(3,2)
   DEFINE xfolio           INTEGER
   DEFINE xconsec          INTEGER
   DEFINE xnombre          CHAR(60)
   DEFINE con_t9           SMALLINT

   LET tpesos     = 0
   LET tben_pesos = 0
   LET con_t9     = 0

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CONC0041" ATTRIBUTE(BORDER)
      DISPLAY "        [ Enter ] Iniciar                            [ Ctrl-C ]  Salir         " AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY "CONC004         GENERA INTERFASE DE PAGOS RETIROS                              " AT 5,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING "dd-mm-yyyy" AT 5,67 ATTRIBUTE(REVERSE) 

   INPUT BY NAME xfecha_pago
      AFTER FIELD xfecha_pago
        IF xfecha_pago IS NULL THEN
           ERROR "Fecha de pago -NO PUEDE SER NULA-."
           SLEEP 2
           NEXT FIELD xfecha_pago
        END IF

        IF xfecha_pago > TODAY THEN
           ERROR "Fecha de pago -NO PUEDE SER MAYOR AL DIA DE HOY-."
           SLEEP 2
           NEXT FIELD xfecha_pago
        END IF

        SELECT 'X'
          FROM con_pago
         WHERE fecha_proceso = xfecha_pago
        GROUP BY 1
        IF STATUS <> NOTFOUND THEN
           ERROR "Fecha de pago -YA GENERADO-."
           SLEEP 2
           NEXT FIELD xfecha_pago
        END IF

        SELECT 'X' 
        FROM   con_transaccion
        WHERE  fecha_emision    = xfecha_pago
        AND    proceso_cod     IN (SELECT * FROM pago_proceso)
        AND    identificador    = 1
        AND    transaccion_cod <> 99991
        AND    estado IN (20,40)
        GROUP BY 1
        IF STATUS = NOTFOUND THEN
           ERROR "No existe pagos de retiros."
           SLEEP 2
           NEXT FIELD xfecha_pago
        END IF 

        EXIT INPUT

   END INPUT

   IF int_flag THEN
      ERROR "Proceso cancelado. . ."
      SLEEP 2
      EXIT PROGRAM
   END IF

   DECLARE cur_1 CURSOR FOR
   SELECT SUM(importe), folio
   FROM   con_transaccion
   WHERE  fecha_emision    = xfecha_pago
   AND    proceso_cod     IN (SELECT * FROM pago_proceso)
   AND    identificador    = 1
   AND    transaccion_cod <> 99991
   AND    estado IN (20,40)
   GROUP BY 2
   FOREACH cur_1 INTO pesos_conta, xfolio

      DECLARE cur_2 CURSOR FOR
      SELECT ROUND(SUM(monto_en_pesos),2), nss, consecutivo_lote
      FROM   dis_cuenta
      WHERE  folio = xfolio
      AND    tipo_movimiento <> 10
      GROUP BY 2,3
      FOREACH cur_2 INTO xpesos, xnss, xconsec

         DECLARE cur_3 cursor for
         SELECT *
         FROM   ret_beneficiario
         WHERE  nss         = xnss
         AND    consecutivo = xconsec
         FOREACH cur_3 into ret_1.*
             LET xnombre = ret_1.nombres CLIPPED," ",
                           ret_1.paterno CLIPPED," ",
                           ret_1.materno CLIPPED
             IF ret_1.porcentaje <> 100 then
                LET  xporcentaje =  0
                LET  xpesos1     =  0
                LET  xporcentaje =  ret_1.porcentaje / 100
                LET  xpesos1     = xpesos * xporcentaje
                INSERT INTO con_pago VALUES(xnss,
                                            xnombre,
                                            xpesos1,
                                            con_t9,
                                            xfecha_pago,
                                            today,
                                            usuario)
             ELSE
                INSERT INTO con_pago VALUES(xnss,
                                            xnombre,
                                            xpesos,
                                            con_t9,
                                            xfecha_pago,
                                            today,
                                            usuario)
             END IF
             LET con_t9 = con_t9 + 1
          END FOREACH
      END FOREACH
   END FOREACH

   -----# aportaciones voluntarias
   SELECT "X"
   FROM   con_transaccion a
   WHERE  a.fecha_emision = xfecha_pago
   AND    a.proceso_cod = 00019
   AND    a.identificador = 1
   AND    a.estado IN (20,40)
   GROUP BY 1
   IF SQLCA.SQLCODE = 0 THEN
      LET pesos_conta = ""
      LET xfolio = ""
      LET xnss   = ""
      LET xpesos = ""
      DECLARE vol_1 CURSOR FOR
      SELECT sum(importe),
             folio
      FROM   con_transaccion
      WHERE  fecha_emision   = xfecha_pago
      AND    proceso_cod     = "00019"
      AND    identificador   = 1
      AND    transaccion_cod <> 99991
      AND    estado IN (20,40)
      GROUP BY 2
      FOREACH vol_1 INTO pesos_conta, xfolio

         DECLARE vol_2 CURSOR FOR
         SELECT ROUND(SUM(monto_en_pesos),2),
                nss
         FROM   dis_cuenta
         WHERE  folio           = xfolio
         AND    tipo_movimiento = 490
         GROUP BY 2
         FOREACH vol_2 INTO xpesos, xnss 

            SELECT paterno,
                   materno,
                   nombres
            INTO   ret_1.paterno,
                   ret_1.materno,
                   ret_1.nombres
            FROM   afi_mae_afiliado
            WHERE  n_seguro = xnss

            LET xnombre = ""
            LET xnombre = ret_1.nombres CLIPPED," ",
                          ret_1.paterno CLIPPED," ",
                          ret_1.materno CLIPPED

            INSERT INTO con_pago VALUES(xnss,
                                        xnombre,
                                        xpesos,
                                        con_t9,
                                        xfecha_pago,
                                        today,
                                        usuario)

            LET con_t9 = con_t9 + 1
         END FOREACH
      END FOREACH
   END IF

END FUNCTION
###############################################################################
FUNCTION genera_archivo()
#ga----------------------
   DEFINE g_lista CHAR(200)
   DEFINE g_lista1 CHAR(200)
   DEFINE g_reg RECORD LIKE con_pago.* 
   DEFINE g_tra RECORD LIKE con_traductor.*

   LET g_lista  = reg_modulo.ruta_envio CLIPPED, "/SALF_AFP_", 
                 TODAY USING "yymmdd", ".NDF"

   LET g_lista1 = reg_modulo.ruta_envio CLIPPED, "/SASD_AFP_", 
                 TODAY USING "yymmdd", ".NDF"

   START REPORT rep_pagos TO g_lista
   START REPORT rep_t9    TO g_lista1
   DECLARE c_1 CURSOR FOR
   SELECT *
   FROM   con_pago
   WHERE  fecha_pago = xfecha_pago
   FOREACH c_1 INTO g_reg.*

      DECLARE c_2 CURSOR FOR
      SELECT *
      FROM   con_traductor
      WHERE  proceso_cod = 00050
      FOREACH c_2 INTO g_tra.*
         OUTPUT TO REPORT rep_pagos(g_tra.*, g_reg.*)
      END FOREACH

      OUTPUT TO REPORT rep_t9(g_reg.*)
   END FOREACH
   FINISH REPORT rep_pagos
   FINISH REPORT rep_t9

   DISPLAY "Archivos: ", g_lista CLIPPED AT 16,5
   DISPLAY "          ", g_lista CLIPPED AT 17,5

   WHILE TRUE
     PROMPT "Desea terminar el proceso (S)i: " FOR enter
     IF enter MATCHES "[Ss]" THEN
        EXIT WHILE
     ELSE
        ERROR "Solo debe presionar (S)i"
        SLEEP 2
        ERROR ""
     END IF
   END WHILE

END FUNCTION
###############################################################################
REPORT rep_t9(r_reg)
    DEFINE r_reg RECORD LIKE con_pago.*
    OUTPUT 
      PAGE   LENGTH 1
      LEFT   MARGIN 0
      RIGHT  MARGIN 0
      TOP    MARGIN 0
      BOTTOM MARGIN 0

    FORMAT
      ON EVERY ROW
         PRINT 
            COLUMN 001, "A", r_reg.consecutivo USING "&&&&" , #codigo T9
            COLUMN 006, 56 SPACES                           , #relleno 
            COLUMN 062, "SDE"                               , #tipo registro
            COLUMN 065, 18 SPACES                           , #relleno
            COLUMN 083, r_reg.nombre_benef                  , #nombre
            COLUMN 143, 20 SPACES                           , #relleno nombre
            COLUMN 163, "AF1"                               , #desc adic3
            COLUMN 166, 117 SPACES                          , #relleno
            COLUMN 283, r_reg.nss                           , #nss
            COLUMN 294, 29 SPACES 

END REPORT
###############################################################################
REPORT rep_pagos(r_tra, r_reg)
#l4--------------------
    DEFINE r_reg RECORD LIKE con_pago.*
    DEFINE r_tra RECORD LIKE con_traductor.* 
    DEFINE
        signo             CHAR(01),
        sig_imp           CHAR(18)

    OUTPUT 
      PAGE   LENGTH 1
      LEFT   MARGIN 0
      RIGHT  MARGIN 0
      TOP    MARGIN 0
      BOTTOM MARGIN 0

    FORMAT
      ON EVERY ROW
         CASE r_tra.tipo 
            WHEN "A"
               LET signo = "+" 
            WHEN "C"
               LET signo = "-"
         END CASE

         LET r_reg.importe_pago = r_reg.importe_pago * 1000
         LET sig_imp       = signo, r_reg.importe_pago USING "&&&&&&&&&&&&&&&&&"

         PRINT 
            COLUMN 001, r_tra.cuenta USING "&&&&&&&&&"      , #codigo cuenta
            COLUMN 011, 5 SPACES                            , #relleno1   
            COLUMN 016, YEAR(HOY)  USING "&&&&"             , #periodo contable
            COLUMN 020, "0"                                 , #periodo contable
            COLUMN 021, MONTH(HOY) USING "&&"               , #periodo contable
            COLUMN 023, today USING "YYYYMMDD"              , #fecha transacc
            COLUMN 031, 2 SPACES                            , #relleno2
            COLUMN 033, "M"                                 , #tipo registro  
            COLUMN 034, 14 SPACES                           , #num poliza-linea
            COLUMN 048, sig_imp                             , #importe
            COLUMN 066, 2 SPACES                            , #marca carg/abon
                                                              #ind asig
            COLUMN 068, "ER   "                             , #tipo diario
            COLUMN 073, "AFORE"                             , #origen
            COLUMN 078, r_tra.transaccion_cod USING "&&&&&" , #referncia
            COLUMN 083, 10 SPACES                           , #ref-rell3
            COLUMN 093, r_tra.descripcion[1,25]             , #descripcion
            COLUMN 118, 259 SPACES                          , #relleno
            COLUMN 377, "A", r_reg.consecutivo USING "&&&&" , #codigo T9
            COLUMN 382, 52 SPACES                             #relleno

END REPORT
###############################################################################
