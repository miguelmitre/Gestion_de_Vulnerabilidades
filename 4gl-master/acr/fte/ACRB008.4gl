#############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                           #
#Propietario       => E.F.P.                                                #
#Programa ACRB008  => ACTUALIZA SALDOS DE ACREDITADOS                       #
#Por               => MAURO MUNIZ CABALLERO                                 #
#Fecha creacion    => 4 DE JUNIO DE 1999                                    #
#Actualizacion     => MAURO MUNIZ CABALLERO                                 #
#Fecha actualiz.   => 4 DE AGOSTO DE 2004                                   #
#                Se adecuo para proceso de participaciones                  #
#Modificacion      => JOSUÉ LISANDRO HUERTA SIERRA                          #
#Fecha Modifica.   => 8 DE AGOSTO DE 2007                                   #
#            CAMBIO DE MARCA A 232 EN PROCESO DE TRANSF ACRED               #
#Sistema           => ACR                                                   #
#Modificacion      => DMR 15/Abril/2014   CPL-1585                          #
#                  => STARTLOG personalizado, Estandarizacion de codigo y se#
#                     Adecuo para imprimir solo en metlife reporte generado.#
#                     Ademas al insertar en acr_det_cedido sobraba 1 campo  #
#                     de safre_tmp:det_tra_acr                              #
#############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      xcodigo_marca      ,
      xcodigo_rechazo    ,
      marca              SMALLINT,
      desmarca           SMALLINT,
      contar             INTEGER,
      vfolio             INTEGER,
      vfecha_liquida     DATE,
      vfecha_valor       DATE,
      vfecha             DATE,
      hoy                DATE,
      hoy2               DATE,
      enter              CHAR(1),
      hora               CHAR(5),
      g_usuario          CHAR(8),
      seguro             CHAR(11),
      ejecuta            CHAR(300),
      v_desmarca         CHAR(300)
      
   DEFINE
      g_lista            CHAR(100),
      comando            CHAR(100)

   DEFINE
      precio_acc         DECIMAL(15,6)
      
   DEFINE
      bnd_proceso        SMALLINT

   DEFINE
      vcorrelativo       INTEGER

   DEFINE
      desc_afo           CHAR(25)   
   
   DEFINE
      reg_bat            RECORD
                            pid            INTEGER,
                            proceso_cod    INTEGER,
                            opera_cod      INTEGER,
                            vfolio         INTEGER
                         END RECORD
   
   DEFINE
      reg_aboctas        RECORD LIKE dis_provision.*
      	                  
   DEFINE                 
      g_seg_modulo       RECORD LIKE seg_modulo.*
      	
   DEFINE
      s_codigo_afore            LIKE tab_afore_local.codigo_afore
   
   DEFINE
      registro           RECORD
                            subcuenta         LIKE dis_cuenta.subcuenta,
                            fecha_valor       LIKE dis_cuenta.fecha_valor,
                            fecha_conversion  LIKE dis_cuenta.fecha_conversion,
                            monto_en_acciones LIKE dis_cuenta.monto_en_acciones,
                            monto_en_pesos    LIKE dis_cuenta.monto_en_pesos,
                            descripcion       CHAR(15)
                         END RECORD
END GLOBALS


MAIN
   DISPLAY " "
   DISPLAY ".1"

   CALL STARTLOG(FGL_GETENV("USER")||".ACRB008.log")
   CALL inicio()                   #i

   IF NOT bnd_proceso THEN
      DEFER INTERRUPT
      OPTIONS INPUT WRAP,
              PROMPT LINE LAST,
              ACCEPT KEY CONTROL-I

      CALL proceso_principal()     #pp
   ELSE
      CALL genera_det_tras_afo()   #gdt
      CALL genera_historicos()     #gh
   END IF

   CALL genera_reporte()
END MAIN


FUNCTION inicio()
#i-------------
   LET reg_bat.pid         = ARG_VAL(1)
   LET reg_bat.proceso_cod = ARG_VAL(2)
   LET reg_bat.opera_cod   = ARG_VAL(3)
   LET reg_bat.vfolio      = ARG_VAL(4)

   LET bnd_proceso = 0

   LET hoy      = TODAY
   LET hoy2     = MDY(MONTH(TODAY),1,YEAR(TODAY))
   LET hora     = TIME
   LET desmarca = 232
   LET marca    = 230

   IF reg_bat.pid THEN
      DISPLAY "INICIANDO PROCESO ..."
      LET bnd_proceso    = 1
      LET vfecha_valor   = MDY(MONTH(TODAY),1,YEAR(TODAY))
      LET vfecha_liquida = hoy
      LET vfolio         = reg_bat.vfolio
   END IF

   SELECT codigo_afore, razon_social, USER
   INTO   s_codigo_afore, desc_afo, g_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   g_seg_modulo.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'

   LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------
   OPEN WINDOW ACRB0081 AT 4,4 WITH FORM "ACRB0081" ATTRIBUTE(BORDER)
   DISPLAY " ACRB008 LIQUIDACION DE SALDOS DE TRANSFERENCIA ACREDITADOS                    " AT 3,1 ATTRIBUTE(REVERSE)
#  DISPLAY " < CTRL-C > Salir " AT 1,26
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)
   LET INT_FLAG = FALSE

   INPUT BY NAME vfolio, vfecha_liquida

      AFTER FIELD vfolio
         IF vfolio IS NULL OR vfolio = 0 THEN
            ERROR "Campo folio NO puede ser NULO o CERO"
            NEXT FIELD vfolio
         ELSE 
            NEXT FIELD vfecha_liquida
         END IF

      AFTER FIELD vfecha_liquida
         IF vfecha_liquida IS NULL THEN
            ERROR "Campo fecha liquidacion NO puede ser NULA"
            NEXT FIELD vfecha_liquida
         ELSE
            EXIT INPUT
         END IF

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
      SLEEP 2
      EXIT PROGRAM
   ELSE
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

      LET vfecha_valor = MDY(MONTH(vfecha_liquida),1,YEAR(vfecha_liquida))

      CALL genera_det_tras_afo()   #gdt
      CALL genera_historicos()     #gh

      PROMPT "Proceso Finalizado, Presione [Enter] para Salir" FOR enter

      CLOSE WINDOW ACRB0081
   END IF
END FUNCTION


FUNCTION genera_det_tras_afo()
#gdts-------------------------
   DEFINE
      vprecio_accion  DECIMAL(22,14)

   DEFINE
      vtot_pesos      DECIMAL(22,6),
      vsdo_pesos      DECIMAL(18,2)

   SELECT @precio_del_dia
   INTO vprecio_accion
   FROM glo_valor_accion
   WHERE @fecha_valuacion = vfecha_liquida
   AND   @codigo_siefore  = 11

   DECLARE cur_1 CURSOR FOR
   SELECT *
   FROM   dis_provision
   WHERE  folio = vfolio
   AND    subcuenta in(4,8)
   AND    tipo_movimiento = 230
   AND    estado = 5

   FOREACH cur_1 INTO reg_aboctas.*
      LET reg_aboctas.fecha_conversion = vfecha_liquida
      LET reg_aboctas.fecha_valor      = vfecha_valor
      LET reg_aboctas.folio            = vfolio
      LET reg_aboctas.precio_accion    = vprecio_accion

      LET vtot_pesos                   = reg_aboctas.monto_en_acciones
                                         * vprecio_accion
      LET vsdo_pesos                   = vtot_pesos

      LET reg_aboctas.monto_en_pesos   = vsdo_pesos

      LET reg_aboctas.fecha_proceso    = hoy
       
      INSERT INTO dis_cuenta
      VALUES (reg_aboctas.*)
   END FOREACH

   UPDATE dis_provision
   SET    estado = 6
   WHERE  folio  = vfolio
   AND    subcuenta in(4,8)
   AND    tipo_movimiento = 230

   IF bnd_proceso THEN
      DISPLAY "LIQUIDACION TERMINADA "
      SLEEP 2
   ELSE
      DISPLAY "LIQUIDACION TERMINADA " AT 19,1 ATTRIBUTE(REVERSE)
      SLEEP 2
   END IF
END FUNCTION


FUNCTION genera_historicos()
#gh-------------------------
   DEFINE
      reg_det    RECORD LIKE safre_tmp:det_tra_acr.*,
      reg_cza    RECORD LIKE safre_tmp:cza_tra_acr.*,
      reg_sum    RECORD LIKE safre_tmp:sum_tra_acr.*

   DECLARE cursor_cza CURSOR FOR
   SELECT *
   FROM   safre_tmp:cza_tra_acr

   FOREACH cursor_cza INTO reg_cza.*
      SELECT "X"
      FROM   acr_cza_cedido
      WHERE  folio = vfolio
      AND    fecha_presentacion = reg_cza.fecha_presentacion

      IF STATUS = NOTFOUND THEN
         INSERT INTO acr_cza_cedido
         VALUES (vfolio, reg_cza.*)
      END IF
   END FOREACH

   DECLARE cursor_sum CURSOR FOR
   SELECT *
   FROM   safre_tmp:sum_tra_acr

   FOREACH cursor_sum INTO reg_sum.*
      SELECT "X"
      FROM   acr_sum_cedido
      WHERE  folio = vfolio
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         INSERT INTO acr_sum_cedido
         VALUES (vfolio, reg_sum.*)
      END IF
   END FOREACH

   DECLARE cursor_det CURSOR FOR
   SELECT *
   FROM   safre_tmp:det_tra_acr

   FOREACH cursor_det INTO reg_det.*
      SELECT "X"
      FROM   acr_det_cedido
      WHERE  folio = vfolio
      AND    nss_afore = reg_det.nss_afore 
      AND    fecha_presentacion = reg_det.fecha_presentacion

      IF STATUS = NOTFOUND THEN                                   #CPL-1585
         INSERT INTO acr_det_cedido
         VALUES (vfolio, reg_det.tipo_registro THRU reg_det.estado, hoy2)
      END IF
   END FOREACH
 
   DECLARE cur_2 CURSOR FOR
   SELECT nss
   FROM   dis_provision 
   WHERE  folio = vfolio 
   AND    subcuenta in(4,8)
   AND    tipo_movimiento = 230
   AND    estado =  6
   
   FOREACH cur_2 INTO reg_det.nss_afore
      SELECT c.correlativo
      INTO   vcorrelativo
      FROM   cta_act_marca c
      WHERE  c.nss = reg_det.nss_afore
      AND    c.marca_cod = desmarca

      IF vcorrelativo IS NOT NULL AND vcorrelativo <> 0 THEN
         CALL desmarca_cuenta(reg_det.nss_afore,desmarca,g_usuario,vcorrelativo)
         LET vcorrelativo = NULL
      END IF
   END FOREACH

   IF bnd_proceso THEN
      DISPLAY "CARGA DE HISTORICO TERMINADA"
      SLEEP 2
   ELSE
      DISPLAY "CARGA DE HISTORICO TERMINADA" AT 19,1 ATTRIBUTE(REVERSE)
      SLEEP 2
   END IF
END FUNCTION


FUNCTION genera_reporte()
#gr----------------------
   DECLARE reprec_1 CURSOR FOR
   SELECT a.subcuenta,
          a.fecha_valor,
          a.fecha_conversion,
          sum(a.monto_en_acciones),
          sum(a.monto_en_pesos)
   FROM   dis_cuenta a
   WHERE  a.folio = vfolio
   AND    a.subcuenta in(4,8)
   AND    a.tipo_movimiento = 230
   GROUP BY 1,2,3
   ORDER BY 1

   LET g_lista = g_seg_modulo.ruta_listados CLIPPED, "/",
                 g_usuario CLIPPED,
                 ".LIQ_ACRED.",hoy USING "DDMMYY","-", 
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

   IF s_codigo_afore = 564 THEN         #METLIFE NO IMPRIME    #CPL-1585
      --LET comando = "lp ", g_lista
      --RUN comando
   ELSE
      LET comando = "lp ", g_lista
      RUN comando
   END IF                                                      #CPL-1585
END FUNCTION


REPORT reporte_1(registro,vfolio,hoy,hoy2)
#r1---------------------------------------
   DEFINE
      vfolio       INTEGER
                   
   DEFINE          
      hoy2         DATE
                   
   DEFINE          
      hoy          DATE
                   
   DEFINE          
      tot_viv      DECIMAL(18,6)
                   
   DEFINE          
      tot_reg      INTEGER

   
   DEFINE
      registro     RECORD
                      subcuenta         LIKE dis_cuenta.subcuenta,
                      fecha_valor       LIKE dis_cuenta.fecha_valor,
                      fecha_conversion  LIKE dis_cuenta.fecha_conversion,
                      monto_en_acciones LIKE dis_cuenta.monto_en_acciones,
                      monto_en_pesos    LIKE dis_cuenta.monto_en_pesos,
                      descripcion       CHAR(15)
                   END RECORD

   OUTPUT
      PAGE LENGTH    60
      LEFT MARGIN     0
      RIGHT MARGIN  132
      TOP MARGIN      0
      BOTTOM MARGIN   0

   FORMAT
      PAGE HEADER

      SELECT SUM(monto_en_pesos)
      INTO   tot_viv
      FROM   dis_cuenta
      WHERE  folio = vfolio
      
      SELECT COUNT(*)
      INTO   tot_reg
      FROM   acr_det_cedido d
      WHERE  d.folio = vfolio
      AND    d.estado = 0

      PRINT
      SKIP 2 LINES
      PRINT COLUMN 50, hoy USING "DD/MM/YYYY"
      PRINT
      PRINT
            COLUMN 10, "CVE. ", s_codigo_afore USING "&&&","      ", desc_afo CLIPPED
      PRINT "------------------------------------------------------------"
      PRINT "FOLIO TRANFERENCIA: ",COLUMN 49, vfolio
      PRINT "FECHA LIQUIDACION : ",COLUMN 50, hoy2
      PRINT "------------------------------------------------------------"
      PRINT "SUBCUENTA",
            COLUMN 25,"PARTICIPACIONES",
            COLUMN 45,"MONTO PESOS"
      PRINT "------------------------------------------------------------"
      SKIP 2 LINES
      
   ON EVERY ROW
      PRINT 
            COLUMN  3,registro.subcuenta USING "&",
            COLUMN  7,registro.DESCRIPCION,
            COLUMN 20,registro.monto_en_acciones USING "$$$$$$$$$&.&&&&&&",
            COLUMN 40,registro.monto_en_pesos    USING "$$$$$$$$$&.&&&&&&"

   ON LAST ROW 
      PRINT"------------------------------------------------------------"
      PRINT COLUMN 20,sum(registro.monto_en_acciones) USING "$$$$$$$$$&.&&&&&&",
            COLUMN 40,sum(registro.monto_en_pesos)    USING "$$$$$$$$$&.&&&&&&"
      PRINT
      PRINT COLUMN 2, "Total registros a enviar : ", tot_reg
END REPORT


FUNCTION desmarca_cuenta(vnss,vmarca,vusuario,vcorrelativo)
#dc--------------------------------------------------------
   DEFINE
      vnss             CHAR(11),
      vmarca           SMALLINT,
      vusuario         CHAR(8),
      vcorrelativo     INTEGER,
      pestado_marca    SMALLINT,
      pmarca_causa     SMALLINT

   LET pestado_marca = 0
   LET pmarca_causa  = 0

   PREPARE eje_desmarca FROM v_desmarca

   EXECUTE eje_desmarca
   USING vnss,
         vmarca,
         vcorrelativo,
         pestado_marca,
         pmarca_causa,
         vusuario
END FUNCTION
 
