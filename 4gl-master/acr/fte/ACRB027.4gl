#############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                           #
#Propietario       => E.F.P.                                                #
#Programa ACRB027  => ACTUALIZA SALDOS DE USO DE GARANTIA                   #
#Por               => MAURO MUNIZ CABALLERO                                 #
#Fecha creacion    => 4 DE AGOSTO DE 2002                                   #
#Modifico          => MAURO MUNIZ CABALLERO                                 #
#Fecha actualiza   => 4 DE AGOSTO DE 2004                                   #
#             Se adecuo para proceso de participaciones                     #
#Sistema           => ACR                                                   #
#Modifico          => DMR 30/JULIO/2014 MLM-2683 STARTLOG PERSONALIZADO,    #
#                  => Se modifico validacion para no permitir que se provi- #
#                     sione y liquide monto mayor al saldo es decir, evitar #
#                     quebrantos verificando saldo y montos comprometidos   #
#Modifico          => PST-1596 confirmacion de impresion de listados        #
#############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      contar           INTEGER,
      vfolio           INTEGER,
      vfecha_liquida   DATE,
      vfecha           DATE,
      hoy              DATE,
      hoy2             DATE,
      hoy_val          DATE,
      seguro           CHAR(11),
      enter            CHAR(01),
      hora             CHAR(05),
      g_usuario        CHAR(8)
   
   DEFINE
      g_lista          CHAR(100),
      comando          CHAR(100),
      ejecuta          CHAR(300)

   DEFINE
      xcodigo_marca    ,
      xcodigo_rechazo  ,
      marca            ,
      bnd_proc         ,
      edo_proc         SMALLINT,
      cuantos          ,
      s_codigo_afore   ,
      cont             INTEGER,
      sdo_tot_97       DECIMAL(18,2),
      sdo_viv_97       ,
      vprecio_accion   DECIMAL(18,6)

   DEFINE
      desc_afo         CHAR(25),
      v_desmarca       CHAR(300),
      vcorrelativo     INTEGER
      
   DEFINE
      registro         RECORD
                          subcuenta         LIKE dis_cuenta.subcuenta,
                          fecha_valor       LIKE dis_cuenta.fecha_valor,
                          fecha_conversion  LIKE dis_cuenta.fecha_conversion,
                          monto_en_pesos    LIKE dis_cuenta.monto_en_pesos,
                          monto_en_acciones LIKE dis_cuenta.monto_en_acciones,
                          descripcion       CHAR(15)
                       END RECORD

   DEFINE reg_aboctas  RECORD LIKE dis_provision.*
   DEFINE g_param_taa  RECORD LIKE seg_modulo.*
END GLOBALS


MAIN
   DEFER INTERRUPT
         OPTIONS INPUT WRAP,
         PROMPT LINE LAST,
         ACCEPT KEY CONTROL-I

   CALL STARTLOG (FGL_GETENV("USER")||".ACRB027.log")         #MLM-2683
   CALL inicio()             #i
   CALL proceso_principal()  #pp
END MAIN


FUNCTION inicio()
#i---------------
   LET hoy     = today
   LET hora    = TIME

   LET marca   = 236

   SELECT codigo_afore, razon_social
   INTO   s_codigo_afore, desc_afo
   FROM   tab_afore_local

   SELECT *, USER
   INTO   g_param_taa.*, g_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'

   LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------
   OPEN WINDOW ACRB0271 AT 4,4 WITH FORM "ACRB0271" ATTRIBUTE(BORDER)
   DISPLAY "ACRB027   LIQUIDACION DE SALDOS USO DE CREDITO EN GARANTIA                     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY " < CTRL-C > Salir " AT 1,26
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)

   INPUT BY NAME vfolio, vfecha_liquida
      AFTER FIELD vfolio
         IF vfolio IS NULL THEN
            ERROR "Campo folio NO puede ser NULO"
            NEXT FIELD vfolio
         ELSE
            SELECT "OK"                                   #MLM-2683
            FROM dis_cuenta
            WHERE folio = vfolio
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
               ERROR "FOLIO YA LIQUIDADO..."
               NEXT FIELD vfolio
            ELSE
               SELECT "OK" 
               FROM  dis_provision
               WHERE folio = vfolio
               AND   tipo_movimiento = 236
               GROUP BY 1

               IF SQLCA.SQLCODE <> 0 THEN
                  ERROR "FOLIO NO ESTA PROVISIONADO COMO USO DE GARANTIA"
                  NEXT FIELD vfolio
               ELSE
                  NEXT FIELD vfecha_liquida
               END IF
            END IF
         END IF

      AFTER FIELD vfecha_liquida
         IF vfecha_liquida IS NULL THEN
            ERROR "Campo fecha liquidacion NO puede ser NULO"
            NEXT FIELD vfecha_liquida
         --ELSE
             --LET hoy2 = vfecha_liquida
         END IF
      
         LET hoy2 = MDY(MONTH(vfecha_liquida),1,YEAR(vfecha_liquida))
      
         SELECT @precio_del_dia
         INTO vprecio_accion
         FROM glo_valor_accion
         WHERE @codigo_siefore  = 11
         AND   @fecha_valuacion = hoy2
      
         SELECT sum(pr.monto_en_pesos)
         INTO   sdo_viv_97
         FROM   dis_provision pr
         WHERE  pr.folio = vfolio
         AND    pr.subcuenta = 4
      
         LET sdo_tot_97 = sdo_viv_97
      
         DISPLAY sdo_tot_97 TO FORMONLY.monto
      
         EXIT INPUT
      
      ON KEY(INTERRUPT)
         ERROR "PROCESO CANCELADO"
         SLEEP 2
         ERROR ""
         EXIT PROGRAM   
   END INPUT

   WHILE TRUE
      PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR enter
      IF enter MATCHES "[sSnN]" THEN 
         IF enter MATCHES "[sS]" THEN
            EXIT WHILE
         ELSE
            ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
            SLEEP 2
            EXIT PROGRAM
         END IF
      END IF
   END WHILE

   DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

   CALL genera_det_tras_afo()   #gdt
   CALL genera_historicos()     #gh
   CALL genera_reporte()

   ERROR "PROCESO FINALIZADO   !!!"
   SLEEP 2
   ERROR ""

   PROMPT "Pulse  <ENTER> para Salir  " FOR enter

   CLOSE WINDOW ACRB0271
END FUNCTION


FUNCTION genera_det_tras_afo()
#gdts-------------------------
   DEFINE
      sdo_pesos_v97     DECIMAL(22,6),
      sdo_total_v97     DECIMAL(18,2),
      sdo_acc_v97       DECIMAL(18,2)

   DECLARE cur_1 CURSOR FOR
   SELECT *
   FROM   dis_provision
   WHERE  folio  = vfolio
   AND    estado = 5

   FOREACH cur_1 INTO reg_aboctas.*
      LET reg_aboctas.fecha_conversion  = TODAY
      LET reg_aboctas.fecha_valor       = hoy2  ---vfecha_liquida
      LET reg_aboctas.fecha_proceso     = hoy
      LET reg_aboctas.folio             = vfolio
      LET sdo_acc_v97                   = reg_aboctas.monto_en_acciones
      LET reg_aboctas.monto_en_acciones = sdo_acc_v97
      LET reg_aboctas.precio_accion     = vprecio_accion

      INSERT INTO dis_cuenta
      VALUES (reg_aboctas.*)
   END FOREACH

   UPDATE dis_provision
   SET    estado = 6
   WHERE  folio = vfolio
END FUNCTION


FUNCTION genera_historicos()
#gh-------------------------
   DEFINE
      reg_det      RECORD LIKE safre_tmp:det_garantia.*,
      reg_cza      RECORD LIKE safre_tmp:cza_garantia.*,
      reg_sum      RECORD LIKE safre_tmp:sum_garantia.*

   DECLARE cursor_cza CURSOR FOR
   SELECT *
   FROM   safre_tmp:cza_garantia

   FOREACH cursor_cza INTO reg_cza.*
      SELECT "X"
      FROM   acr_cza_garantia
      WHERE  folio = vfolio
   
      IF STATUS = NOTFOUND THEN
         INSERT INTO acr_cza_garantia
         VALUES (vfolio, reg_cza.*)
      END IF
   END FOREACH

   DECLARE cursor_sum CURSOR FOR
   SELECT *
   FROM   safre_tmp:sum_garantia

   FOREACH cursor_sum INTO reg_sum.*
      SELECT "X"
      FROM   acr_sum_garantia
      WHERE  folio = vfolio

      IF STATUS = NOTFOUND THEN
         INSERT INTO acr_sum_garantia
         VALUES (vfolio,
                 reg_sum.tipo_registro,
                 reg_sum.cant_reg_det,
                 reg_sum.sum_partic_v97,
                 reg_sum.sum_sdo_viv97,
                 0,
                 reg_sum.sum_int_viv97,
                 reg_sum.sum_int_viv92 )
      END IF
   END FOREACH

   DECLARE cursor_det CURSOR FOR
   SELECT *
   FROM   safre_tmp:det_garantia

   FOREACH cursor_det INTO reg_det.*
      SELECT "X"
      FROM   acr_det_garantia
      WHERE  folio = vfolio
      AND    nss_afore = reg_det.nss_afore 
      AND    cont_servicio = reg_det.cont_servicio

      IF SQLCA.SQLCODE <> 0 THEN
         INSERT INTO acr_det_garantia
         VALUES (vfolio, reg_det.*, hoy2)

         CALL desmarca_cuenta(reg_det.nss_afore,marca,g_usuario,reg_det.cont_servicio)
      END IF
   END FOREACH
END FUNCTION


FUNCTION genera_reporte()
#gr----------------------
   DECLARE reprec_1 CURSOR FOR

   SELECT a.subcuenta,
          a.fecha_valor,
          a.fecha_conversion,
          sum(a.monto_en_pesos),
          ""
   FROM   dis_cuenta a
   WHERE  a.folio = vfolio 
   GROUP BY 1,2,3
   ORDER BY 1

   LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", 
                 "LIQ_USO.",hoy USING "DDMMYY","-", 
                 hora[1,2],hora[4,5] 

   START REPORT reporte_1 TO g_lista  
      FOREACH reprec_1 INTO registro.*   
      
         CASE registro.subcuenta
            WHEN 4 LET registro.descripcion="VIVIENDA 97" 
            WHEN 8 LET registro.descripcion="VIVIENDA 92" 
         END CASE
      
         OUTPUT TO REPORT reporte_1 (registro.*,vfolio,hoy,hoy2) 
      
      END FOREACH
   FINISH REPORT reporte_1

   PROMPT  "DESEA IMPRIMIR LISTADO S/N ? " FOR enter      #PST-1596
   IF enter = "S"  or enter = "s" THEN
      LET comando = "lp ",g_lista
      RUN comando
   END IF
END FUNCTION


REPORT reporte_1(registro,vfolio,hoy,hoy2)
#r1---------------------------------------
   DEFINE
      registro       RECORD
                        subcuenta          LIKE dis_cuenta.subcuenta,
                        fecha_valor        LIKE dis_cuenta.fecha_valor,
                        fecha_conversion   LIKE dis_cuenta.fecha_conversion,
                        monto_en_pesos     LIKE dis_cuenta.monto_en_pesos,
                        monto_en_acciones  LIKE dis_cuenta.monto_en_acciones,
                        descripcion        CHAR(15)
                     END RECORD

   DEFINE
      vfolio         INTEGER 
      
   DEFINE
      hoy2           DATE
      
   DEFINE
      hoy            DATE  

   OUTPUT
      PAGE LENGTH   60
      LEFT MARGIN    0
      RIGHT MARGIN 132
      TOP MARGIN     0
      BOTTOM MARGIN  0

   FORMAT 
      PAGE HEADER
         PRINT
         SKIP 2 LINES
         PRINT
              COLUMN 50, hoy USING "DD/MM/YYYY"
         PRINT
         PRINT
              COLUMN 10,
                "CVE. ", s_codigo_afore USING "&&&","     ",desc_afo CLIPPED
         PRINT "------------------------------------------------------------"
         PRINT "FOLIO TRANFERENCIA: ",COLUMN 49,vfolio
         PRINT "FECHA LIQUIDACION : ",COLUMN 50,hoy2
         PRINT "------------------------------------------------------------"
         PRINT "SUBCUENTA",
         COLUMN 33,"MONTO PESOS",
         COLUMN 50,"PARTICIPACIONES"
         PRINT "------------------------------------------------------------"   

         SKIP 2 LINES
         
      ON EVERY ROW
         PRINT registro.subcuenta USING "&",
         COLUMN 3 ,registro.DESCRIPCION,
         COLUMN 27,registro.monto_en_pesos    USING "$$$$$$$$$&.&&&&&&",
         COLUMN 50,registro.monto_en_acciones USING "$$$$$$$$$&.&&&&&&"

      ON LAST ROW
         PRINT"------------------------------------------------------------"
         PRINT COLUMN 27,sum(registro.monto_en_pesos)    USING "$$$$$$$$$&.&&&&&&",
               COLUMN 50,sum(registro.monto_en_acciones) USING "$$$$$$$$$&.&&&&&&"
END REPORT


FUNCTION desmarca_cuenta(vnss,vmarca,vusuario,vcorrelativo)
#dc--------------------------------------------------------
   DEFINE
      vnss              CHAR(11),
      vmarca            SMALLINT,
      vusuario          CHAR(8),
      vcorrelativo      INTEGER,
      pestado_marca     SMALLINT,
      pmarca_causa      SMALLINT

   LET pestado_marca  = 0
   LET pmarca_causa   = 0

   PREPARE eje_desmarca FROM v_desmarca

   EXECUTE eje_desmarca
   USING vnss,
         vmarca,
         vcorrelativo,
         pestado_marca,
         pmarca_causa,
         vusuario

END FUNCTION

