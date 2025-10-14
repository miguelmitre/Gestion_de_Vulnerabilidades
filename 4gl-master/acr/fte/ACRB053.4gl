######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Propietario       => EFP                                            #
#Programa          => ACRB053                                        #
#Descripcion       => LIQUIDAR SALDOS DE TRANSFERENCIA POR           #
#                     ANUALIDADES GARANTIZADAS                       #
#Sistema           => ACR                                            #
#Fecha creacion    => 18 DICIEMBRE 2009                              #
#Por               => STEFANIE DANIELA VERA PIÑA                     #
#Por               => DMR 05/feb/2014 CPL-1520 STARTLOG personalizado#
#                  => Se modifico captura para no permitir fechas    #
#                  => nulas, invalidas, sin precio de la siefore 11 o#
#                  => que presione ESC y procese sin fecha conversion#
######################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      comando          CHAR(100),
      desc_afo         CHAR(25),
      enter            CHAR(1),
      ejecuta          CHAR(300),
      g_usuario        CHAR(8),
      g_lista          CHAR(100),
      hora             CHAR(5),
      seguro           CHAR(11),
      v_desmarca       CHAR(300)

   DEFINE
      vfecha_liquida   ,
      vfecha_valor     ,
      vfecha           ,
      HOY              ,
      HOY2             DATE

   DEFINE
      precio_acc       DECIMAL(15,6)

   DEFINE
      contar           ,
      vcorrelativo     ,
      vfolio           ,
      vtotal_reg       ,
      cont_liq         INTEGER

   DEFINE
      bnd_proceso      ,
      xcodigo_marca    ,
      xcodigo_rechazo  ,
      marca            ,
      desmarca         SMALLINT

   DEFINE
      reg_aboctas      RECORD LIKE dis_provision.*,
      g_seg_modulo     RECORD LIKE seg_modulo.*,
      s_codigo_afore          LIKE tab_afore_local.codigo_afore

   DEFINE
      registro         RECORD
                          subcuenta         LIKE dis_cuenta.subcuenta,
                          fecha_valor       LIKE dis_cuenta.fecha_valor,
                          fecha_conversion  LIKE dis_cuenta.fecha_conversion,
                          monto_en_acciones LIKE dis_cuenta.monto_en_acciones,
                          monto_en_pesos    LIKE dis_cuenta.monto_en_pesos,
                          descripcion       CHAR(15)
                       END RECORD

   DEFINE
      reg_bat          RECORD
                          pid               INTEGER,
                          proceso_cod       INTEGER,
                          opera_cod         INTEGER,
                          vfolio            INTEGER
                       END RECORD
END GLOBALS


MAIN
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST  ,
	   ACCEPT KEY CONTROL-I
	   DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV("USER")||".ACRB053.log")
   CALL inicio()            #i
   CALL proceso_principal() #pp
END MAIN


FUNCTION inicio()
#i---------------

   LET HOY      = TODAY
   LET HOY2     = MDY(MONTH(HOY),1,YEAR(HOY))
   LET hora     = TIME
   LET desmarca = 235

   SELECT codigo_afore,
          razon_social,
          USER
   INTO   s_codigo_afore,
          desc_afo,
          g_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   g_seg_modulo.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'

   LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW v1 AT 3,3 WITH FORM "ACRB0531" ATTRIBUTE(BORDER)
   DISPLAY "ACRB053  LIQUIDACION DE SALDOS DE ANUALIDADES GARANTIZADAS                       " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "<ESC> Ejecutar                                          <Ctrl-C> Salir " AT 1,1
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE
 
   INPUT BY NAME vfolio, vfecha_liquida

      AFTER FIELD vfolio
         IF vfolio IS NULL THEN
            ERROR " FOLIO NO PUEDE SER NULO "      
            NEXT FIELD vfolio
         ELSE
            SELECT COUNT(*)
            INTO   vtotal_reg
            FROM   dis_provision
            WHERE  folio = vfolio
            AND    subcuenta in(4,8)
            AND    tipo_movimiento = 234
            AND    estado = 5

            IF vtotal_reg > 0 THEN
               SELECT count(*)
               INTO cont_liq
               FROM dis_cuenta
               WHERE folio = vfolio

               IF cont_liq > 0 THEN
                  ERROR " FOLIO YA LIQUIDADO !!! "
                  NEXT FIELD vfolio
               ELSE
                  INITIALIZE vfecha_liquida TO NULL
                  NEXT FIELD vfecha_liquida
               END IF
            ELSE
               ERROR " NO EXISTE FOLIO COMO PROCESO DE ACREDITADOS A LIQUIDAR "
               NEXT FIELD vfolio
            END IF
         END IF

      AFTER FIELD vfecha_liquida
         IF vfecha_liquida IS NULL THEN
            ERROR " FECHA LIQUIDACION NO PUEDE SER NULA "
            NEXT FIELD vfecha_liquida
         ELSE
            SELECT "OK"
            FROM  glo_valor_accion
            WHERE fecha_valuacion = vfecha_liquida
            AND   codigo_siefore  = 11
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR " NO EXISTE PRECIO SIEFORE 11 PARA ESA FECHA LIQUIDACION "
               NEXT FIELD vfecha_liquida
            END IF
         END IF

      ON KEY (ESC)
         IF vfecha_liquida IS NULL OR vfecha_liquida = "" THEN
            ERROR " FECHA LIQUIDACION NO PUEDE SER NULA "
            NEXT FIELD vfecha_liquida
         END IF
 
         IF vfolio IS NULL THEN
            ERROR " FOLIO NO PUEDE SER NULO "      
            NEXT FIELD vfolio
         ELSE 
            SELECT COUNT(*)
            INTO   vtotal_reg
            FROM   dis_provision
            WHERE  folio = vfolio
            AND    subcuenta in(4,8)
            AND    tipo_movimiento = 234
            AND    estado = 5

            IF vtotal_reg > 0 THEN
               SELECT count(*)
               INTO cont_liq
               FROM dis_cuenta
               WHERE folio = vfolio

               IF cont_liq > 0 THEN
                  ERROR " FOLIO YA LIQUIDADO !!! "
                  NEXT FIELD vfolio
               ELSE
                  LET int_flag = 0 
                  EXIT INPUT
               END IF
            ELSE
               ERROR " NO EXISTE FOLIO COMO PROCESO DE ACREDITADOS A LIQUIDAR "
               NEXT FIELD vfolio
            END IF
         END IF

         IF vfecha_liquida IS NULL OR vfecha_liquida = "" THEN
            ERROR " FECHA LIQUIDACION NO PUEDE SER NULA "
            NEXT FIELD vfecha_liquida
         ELSE
            SELECT "OK"
            FROM  glo_valor_accion
            WHERE fecha_valuacion = vfecha_liquida
            AND   codigo_siefore  = 11
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR " NO EXISTE PRECIO SIEFORE 11 PARA ESA FECHA LIQUIDACION "
               NEXT FIELD vfecha_liquida
            END IF
         END IF

         EXIT INPUT

      ON KEY (INTERRUPT)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
         FOR enter
         LET int_flag = TRUE
         EXIT INPUT
         
   END INPUT

   IF int_flag = FALSE THEN
      LET vfecha_valor = MDY(MONTH(vfecha_liquida),1,YEAR(vfecha_liquida))

      CALL genera_det_tras_afo()   #gdt
      CALL genera_historicos()     #gh

      SELECT COUNT(*)
      INTO   vtotal_reg
      FROM   dis_cuenta
      WHERE  folio = vfolio
      AND    subcuenta in(4,8)
      AND    tipo_movimiento = 234

      DISPLAY "TOTAL DE REGISTROS   : ",vtotal_reg AT 14,10

      CALL genera_reporte()        #gr

      PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " 
      FOR CHAR enter
   END IF
END FUNCTION


FUNCTION genera_det_tras_afo()
#gdts-------------------------

   DEFINE
      vprecio_accion DECIMAL(22,14)

   DEFINE
      vtot_pesos     DECIMAL(22,6),
      vsdo_pesos     DECIMAL(18,2)

   SELECT @precio_del_dia
   INTO   vprecio_accion
   FROM   glo_valor_accion
   WHERE  @fecha_valuacion = vfecha_liquida
   AND    @codigo_siefore  = 11

   DECLARE cur_1 CURSOR FOR
   SELECT *
   FROM   dis_provision
   WHERE  folio = vfolio
   AND    subcuenta in(4,8)
   AND    tipo_movimiento = 234
   AND    estado = 5

   FOREACH cur_1 INTO reg_aboctas.*
      LET reg_aboctas.fecha_conversion = vfecha_liquida
      LET reg_aboctas.fecha_valor      = vfecha_valor
      LET reg_aboctas.fecha_proceso    = HOY
      LET reg_aboctas.folio            = vfolio
      LET reg_aboctas.precio_accion    = vprecio_accion

      LET vtot_pesos                   = reg_aboctas.monto_en_acciones
                                         * vprecio_accion
      LET vsdo_pesos                   = vtot_pesos

      LET reg_aboctas.monto_en_pesos   = vsdo_pesos

      INSERT INTO dis_cuenta VALUES (reg_aboctas.*)
   END FOREACH

   UPDATE dis_provision
   SET    estado = 6
   WHERE  folio  = vfolio
   AND    subcuenta in(4,8)
   AND    tipo_movimiento = 234
END FUNCTION


FUNCTION genera_historicos()
#gh-------------------------

   DEFINE reg_det RECORD LIKE safre_tmp:det_acr_ag.*
   DEFINE reg_cza RECORD LIKE safre_tmp:cza_acr_ag.*
   DEFINE reg_sum RECORD LIKE safre_tmp:sum_acr_ag.*

   DECLARE cursor_cza CURSOR FOR
   SELECT *
   FROM   safre_tmp:cza_acr_ag

   FOREACH cursor_cza INTO reg_cza.*
      SELECT "X"
      FROM   acr_cza_ced_ag
      WHERE  folio = vfolio
      AND    fecha_presentacion = reg_cza.fecha_presentacion

      IF STATUS = NOTFOUND THEN
         INSERT INTO acr_cza_ced_ag VALUES (vfolio, reg_cza.*)
      END IF
   END FOREACH

   DECLARE cursor_sum CURSOR FOR
   SELECT *
   FROM   safre_tmp:sum_acr_ag

   FOREACH cursor_sum INTO reg_sum.*
      SELECT "X"
      FROM   acr_sum_ced_ag
      WHERE  folio = vfolio
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         INSERT INTO acr_sum_ced_ag VALUES (vfolio, reg_sum.*)
      END IF
   END FOREACH

   DECLARE cursor_det CURSOR FOR
   SELECT *
   FROM   safre_tmp:det_acr_ag

   FOREACH cursor_det INTO reg_det.*
      SELECT "X"
      FROM   acr_det_ced_ag
      WHERE  folio = vfolio
      AND    nss_afore = reg_det.nss_afore 
      AND    fecha_presentacion = reg_det.fecha_presentacion

      IF STATUS = NOTFOUND THEN
         INSERT INTO acr_det_ced_ag VALUES (vfolio, reg_det.*, HOY2)
      END IF

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
   AND    a.tipo_movimiento = 234
   GROUP BY 1,2,3
   ORDER BY 1

   LET g_lista = g_seg_modulo.ruta_listados CLIPPED, "/",
                 g_usuario CLIPPED,
                 ".LIQ_ANU_GAR.",HOY USING "DDMMYY","-", 
                 hora[1,2],hora[4,5] 

   START REPORT reporte_1 TO g_lista
      FOREACH reprec_1 INTO registro.*

         CASE registro.subcuenta
            WHEN 4 LET registro.descripcion="VIVIENDA 97"
            WHEN 8 LET registro.descripcion="VIVIENDA 92"
         END CASE

         OUTPUT TO REPORT reporte_1 (registro.*,vfolio,HOY,HOY2)

      END FOREACH
   FINISH REPORT reporte_1

   WHILE TRUE
      PROMPT "DESEA IMPRIMIR EL REPORTE  S/N : ? " FOR char enter
      IF UPSHIFT(enter) = "S" OR UPSHIFT(enter) = "N" THEN
         EXIT WHILE
      END IF
   END WHILE

   IF enter = "S" OR  enter = "s" THEN
      LET comando = "lp ", g_lista
      RUN comando
      ERROR " REPORTE ENVIADO A IMPRESORA !!!"
      SLEEP 2.5
      ERROR ""
   ELSE
      ERROR " REPORTE CANCELADO !!!"
      SLEEP 2.5
      ERROR ""
   END IF
END FUNCTION


REPORT reporte_1(registro,vfolio,HOY,HOY2)
#r1---------------------------------------

   DEFINE
      vfolio   INTEGER,
      HOY2     DATE,
      HOY      DATE,
      tot_viv  DECIMAL(18,6),
      tot_reg  INTEGER

   DEFINE
      registro RECORD
                  subcuenta         LIKE dis_cuenta.subcuenta,
                  fecha_valor       LIKE dis_cuenta.fecha_valor,
                  fecha_conversion  LIKE dis_cuenta.fecha_conversion,
                  monto_en_acciones LIKE dis_cuenta.monto_en_acciones,
                  monto_en_pesos    LIKE dis_cuenta.monto_en_pesos,
                  descripcion       CHAR(15)
               END RECORD

   OUTPUT
      PAGE LENGTH 60
      LEFT MARGIN 0
      RIGHT MARGIN 132
      TOP MARGIN 0
      BOTTOM MARGIN 0

   FORMAT
   PAGE HEADER

   SELECT SUM(monto_en_pesos)
   INTO   tot_viv
   FROM   dis_cuenta
   WHERE  folio = vfolio

   SELECT COUNT(*)
   INTO   tot_reg
   FROM   acr_det_ced_ag d
   WHERE  d.folio = vfolio
   AND    d.estado = 0

   PRINT
   SKIP 2 LINES
   PRINT
        COLUMN 50, HOY USING "DD/MM/YYYY"
   PRINT
   PRINT
        COLUMN 10,
               "CVE. ", s_codigo_afore USING "&&&","      ", desc_afo CLIPPED
   PRINT "------------------------------------------------------------"
   PRINT "FOLIO TRANFERENCIA: ",COLUMN 49, vfolio
   PRINT "FECHA LIQUIDACION : ",COLUMN 50, HOY2
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
      vnss          CHAR(11),
      vmarca        SMALLINT,
      vusuario      CHAR(8),
      vcorrelativo  INTEGER,
      pestado_marca SMALLINT,
      pmarca_causa  SMALLINT

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

