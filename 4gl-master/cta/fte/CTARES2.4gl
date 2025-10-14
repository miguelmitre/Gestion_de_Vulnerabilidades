################################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Programa CTARES2  => RESARCIMIENTO DE CUENTAS CEDIDAS RECHAZADAS              #
#By                => CESAR DAVID CHAVEZ MARTINEZ                              #
#Fecha             => 26 DE OCTUBRE 2009                                       #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE gar_precios_jul ARRAY[5] OF RECORD
   	         siefore SMALLINT,
   	         precio  LIKE glo_valor_accion.precio_del_dia
          END RECORD

   DEFINE gar_precios_hoy ARRAY[5] OF RECORD
   	         siefore SMALLINT,
   	         precio  LIKE glo_valor_accion.precio_del_dia
          END RECORD

   DEFINE gar_precios_liq ARRAY[5] OF RECORD
   	         siefore SMALLINT,
   	         precio  LIKE glo_valor_accion.precio_del_dia
          END RECORD

   DEFINE gr_afore RECORD
   	      codigo_afore   SMALLINT,
          razon_social   CHAR(50)
        END RECORD

   DEFINE hoy DATE

   DEFINE gr_seg_modulo         RECORD LIKE seg_modulo.* ,
          w_tabafore            RECORD LIKE tab_afore_local.*

   DEFINE enter CHAR (1)
   DEFINE gc_archivo CHAR(200)
   DEFINE gc_usuario CHAR(8)

   DEFINE gr_captura RECORD
   	         folio           INTEGER,
   	         fecha_liquida DATE
   END RECORD

   DEFINE gi_folio           INTEGER,
          gi_tipo_movimiento SMALLINT,
          gc_id_aportante    CHAR(11)

END GLOBALS
################################################################################
MAIN
   DEFER INTERRUPT
   OPTIONS
       INPUT WRAP           ,
       PROMPT LINE LAST     ,
       MESSAGE LINE LAST    ,
       ACCEPT KEY CONTROL-I

   CALL STARTLOG("CTARES2.log")
   CALL init()

   OPEN WINDOW CTARES21 AT 4,4 WITH FORM "CTARES21" ATTRIBUTE (BORDER)
   DISPLAY "                                                                                 " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " CTARES2       RESARCIMIENTO DE CUENTAS CEDIDAS RECHAZADAS               " AT 3,1 ATTRIBUTE (REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

   MENU "Resarcimiento"
      COMMAND "Provisionar" "Realizar el calculo del monto a resarcir"
         CALL provisiona()

      COMMAND "Liquidar" "Realizar la liquidacion del monto a resarcir"
         CALL liquida()

      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW CTARES21
END MAIN
################################################################################
FUNCTION init()
   SELECT codigo_afore,
          razon_social
   INTO   gr_afore.codigo_afore,
          gr_afore.razon_social
   FROM   tab_afore_local

   LET HOY = TODAY

   SELECT *, USER
   INTO   w_tabafore.*, gc_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   gr_seg_modulo.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'cta'

   LET gi_tipo_movimiento = 620
   LET gc_id_aportante    = "PAC-PC0913"

END FUNCTION
################################################################################
FUNCTION provisiona()
   DEFINE lr_tmp_nss_resarce RECORD
   	      folio          INTEGER      ,
   	      nss            CHAR(11)     ,
   	      subcuenta      SMALLINT     ,
   	      siefore        SMALLINT     ,
   	      acciones       DECIMAL(22,6),
   	      pesos          DECIMAL(22,6)
   END RECORD

   DEFINE ld_tasa_jul,
          ld_tasa_ago,
          ld_tasa_sep,
          ld_saldo_inicial,
          ld_acciones     DECIMAL(22,6)

   DEFINE li_cont    INTEGER,
          li_siefore SMALLINT

   DEFINE li_folio INTEGER

   #Borrar ejecuciones anteriores
   WHENEVER ERROR CONTINUE
      SELECT "X"
      FROM   safre_tmp:tmp_nss_resarce
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
      	 INITIALIZE li_folio TO NULL

      	 SELECT folio
         INTO   li_folio
         FROM   safre_tmp:tmp_nss_resarce
         GROUP BY 1

         IF li_folio IS NOT NULL THEN
            DELETE
            FROM  dis_provision
            WHERE folio = li_folio
            AND   tipo_movimiento = gi_tipo_movimiento

            DELETE
            FROM  dis_cuenta
            WHERE folio = li_folio
            AND   tipo_movimiento = gi_tipo_movimiento
         END IF

      END IF
   WHENEVER ERROR STOP

   #Cargar la tabla con los nss a procesar
   CALL carga_nss()

   SELECT COUNT(*)
   INTO   li_cont
   FROM   safre_tmp:tmp_nss_resarce

   DISPLAY "REGISTROS A PROCESAR: ", li_cont AT 18,1 ATTRIBUTE(REVERSE)
   PROMPT "DESEA CONTINUAR S/N ? " FOR CHAR enter

   IF enter = "S" OR enter = "s" THEN
   ELSE
  	  ERROR "PROCESO CANCELADO"
  	  SLEEP 2
  	  ERROR ""
  	  DISPLAY "                                                " AT 18,1
  	  RETURN
   END IF

   ERROR "PROCESANDO INFORMACION..."

   #Validar y cargar precios del dia
   CALL valida_precios()

   DECLARE cur_detalle CURSOR FOR
   SELECT nss        ,
          subcuenta  ,
          siefore    ,
          pesos
   FROM   safre_tmp:tmp_nss_resarce
   ORDER BY nss

   #Obtener folio para la provision
   LET gi_folio = 0
   SELECT MAX( folio ) + 1 INTO gi_folio FROM glo_folio
   INSERT INTO glo_folio VALUES( gi_folio )

   FOREACH cur_detalle INTO lr_tmp_nss_resarce.nss      ,
                            lr_tmp_nss_resarce.subcuenta,
                            lr_tmp_nss_resarce.siefore  ,
                            lr_tmp_nss_resarce.pesos

      LET li_siefore = lr_tmp_nss_resarce.siefore

      LET lr_tmp_nss_resarce.acciones = lr_tmp_nss_resarce.pesos /
                                        gar_precios_hoy[li_siefore].precio

      #Provisionar cuenta
      IF lr_tmp_nss_resarce.pesos > 0 THEN
         INSERT INTO dis_provision VALUES(
         gi_tipo_movimiento                                ,
         lr_tmp_nss_resarce.subcuenta                      ,
         lr_tmp_nss_resarce.siefore                        ,
         gi_folio                                          ,
         0                                                 ,
         lr_tmp_nss_resarce.nss                            ,
         NULL                                              ,
         NULL                                              ,
         HOY                                               ,
         HOY                                               ,
         HOY                                               ,
         lr_tmp_nss_resarce.pesos                          ,
         lr_tmp_nss_resarce.acciones                       ,
         gar_precios_hoy[li_siefore].precio                ,
         0                                                 ,
         NULL                                              ,
         gc_id_aportante                                   ,
         0                                                 ,
         HOY                                               ,
         gc_usuario                                        ,
         HOY                                               ,
         0
         )
      END IF
   END FOREACH

   #Guardar calculos
   UPDATE safre_tmp:tmp_nss_resarce
   SET    folio        = gi_folio
   WHERE  1 = 1

   ERROR ""
   DISPLAY "PROCESO FINALIZADO FOLIO: ", gi_folio AT 18,1 ATTRIBUTE(REVERSE)
   PROMPT  "<ENTER PARA SALIR> " FOR CHAR enter

   DISPLAY "                                                " AT 18,1

   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION carga_nss()
   DEFINE lr_nss_plano RECORD
   	      folio          INTEGER      ,
   	      nss            CHAR(11)     ,
   	      subcuenta      SMALLINT     ,
   	      siefore        SMALLINT     ,
   	      pesos          DECIMAL(22,6)
   END RECORD

   DEFINE lc_archivo CHAR(200)

   WHENEVER ERROR CONTINUE
      DROP TABLE nss_tmp
   WHENEVER ERROR STOP

   #Cargar archivo plano
   CREATE TEMP TABLE nss_tmp(
      nss            CHAR(11)     ,
      siefore        SMALLINT     ,
   	  subcuenta      SMALLINT     ,
   	  pesos          DECIMAL(22,6)
   )

   LET lc_archivo = gr_seg_modulo.ruta_rescate CLIPPED, "/",
                                    "resarce_nss.unl"

   LOAD FROM lc_archivo INSERT INTO nss_tmp

   #Crear tabla de detalle
   DATABASE safre_tmp

      WHENEVER ERROR CONTINUE
         DROP TABLE tmp_nss_resarce
      WHENEVER ERROR STOP

      CREATE TABLE tmp_nss_resarce(
          folio          INTEGER      ,
          nss            CHAR(11)     ,
          subcuenta      SMALLINT     ,
          siefore        SMALLINT     ,
          pesos          DECIMAL(22,6)
      )

   DATABASE safre_af

   #Cargar tabla de detalle
   DECLARE cur_plano CURSOR FOR
   SELECT nss      ,
          siefore  ,
          subcuenta,
          pesos
   FROM   nss_tmp
   ORDER BY nss

   FOREACH cur_plano INTO lr_nss_plano.nss      ,
   	                      lr_nss_plano.siefore  ,
                          lr_nss_plano.subcuenta,
                          lr_nss_plano.pesos

      INITIALIZE lr_nss_plano.folio TO NULL

      INSERT INTO safre_tmp:tmp_nss_resarce VALUES(lr_nss_plano.*)
   END FOREACH

END FUNCTION
################################################################################
FUNCTION valida_precios()
   DEFINE ld_fecha_valuacion DATE,
          li_cont_siefore    SMALLINT,
          li_siefore         SMALLINT,
          ld_precio          LIKE glo_valor_accion.precio_del_dia,
          lc_mensaje         CHAR(72)

   #Obtener precios de accion del dia
   DECLARE cur_precios_hoy CURSOR FOR
   SELECT codigo_siefore,
          precio_del_dia
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = hoy
   AND    codigo_siefore IN (1,2,3,4,5)
   ORDER  BY 1

   LET li_cont_siefore = 0

   FOREACH cur_precios_hoy INTO li_siefore,
   	                            ld_precio

   	  IF ld_precio IS NULL THEN
   	  	 LET ld_precio = 0
   	  END IF

   	  LET gar_precios_hoy[li_siefore].siefore = li_siefore
   	  LET gar_precios_hoy[li_siefore].precio  = ld_precio

   	  LET li_cont_siefore = li_cont_siefore + 1
   END FOREACH

   IF li_cont_siefore < 5 THEN
   	  LET    lc_mensaje = "FALTA PRECIOS DE ACCION DE SIEFORES BASICAS DEL DIA: ", HOY USING "DD/MM/YYYY"
      PROMPT lc_mensaje FOR CHAR enter
      EXIT PROGRAM
   END IF

END FUNCTION
################################################################################
FUNCTION liquida()
   DEFINE msg1,
          msg2 CHAR(34)

   DEFINE lr_dis_cuenta RECORD LIKE dis_cuenta.*
   DEFINE li_siefore SMALLINT

   LET msg1 = "INGRESE FOLIO .................  :"
   LET msg2 = "INGRESE FECHA DE CONVERSION....  :"

   DISPLAY " < ESC > Aceptar                                       < Ctrl - C> Salir         " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY BY NAME msg1,
                   msg2

   INITIALIZE gr_captura.* TO NULL

   SELECT folio
   INTO   gr_captura.folio
   FROM   safre_tmp:tmp_nss_resarce
   GROUP BY 1

   INPUT BY NAME gr_captura.* WITHOUT DEFAULTS

   	  AFTER FIELD folio
   	  	IF gr_captura.folio IS NOT NULL THEN
   	  		 SELECT "X"
   	  		 FROM   dis_provision
   	  		 WHERE  folio           = gr_captura.folio
   	  		 AND    tipo_movimiento = gi_tipo_movimiento
           GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
            	 ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
   	  		 	   SLEEP 2
   	  		 	   ERROR ""
   	  		 	   NEXT FIELD folio
            END IF

            SELECT "X"
   	  	 	  FROM   dis_cuenta
   	  	 	  WHERE  folio           = gr_captura.folio
   	  	 	  AND    tipo_movimiento = gi_tipo_movimiento
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
            	 ERROR "FOLIO YA LIQUIDADO"
   	  	 	 	   SLEEP 2
   	  	 	 	   ERROR ""
   	  	 	 	   NEXT FIELD folio
            END IF
        ELSE
        	 ERROR "DEBE INDICAR FOLIO DE LA PROVISION"
   	  		 SLEEP 2
   	  		 ERROR ""
   	  		 NEXT FIELD folio
   	    END IF

   	  AFTER FIELD fecha_liquida
   	  	IF gr_captura.fecha_liquida IS NULL THEN
   	  		 ERROR "DEBE INDICAR FECHA CONVERSION"
   	  		 SLEEP 2
   	  		 ERROR ""
   	  		 NEXT FIELD fecha_liquida
   	  	ELSE
   	  		 CALL valida_precios_liq(gr_captura.fecha_liquida)
   	    END IF

       ON KEY (ESC)
       	 IF gr_captura.folio IS NOT NULL THEN
   	  	 	  SELECT "X"
   	  	 	  FROM   dis_provision
   	  	 	  WHERE  folio           = gr_captura.folio
   	  	 	  AND    tipo_movimiento = gi_tipo_movimiento
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
            	 ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
   	  	 	 	   SLEEP 2
   	  	 	 	   ERROR ""
   	  	 	 	   NEXT FIELD folio
            END IF

            SELECT "X"
   	  	 	  FROM   dis_cuenta
   	  	 	  WHERE  folio           = gr_captura.folio
   	  	 	  AND    tipo_movimiento = gi_tipo_movimiento
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
            	 ERROR "FOLIO YA LIQUIDADO"
   	  	 	 	   SLEEP 2
   	  	 	 	   ERROR ""
   	  	 	 	   NEXT FIELD folio
            END IF
         ELSE
         	  ERROR "DEBE INDICAR FOLIO DE LA PROVISION"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD folio
   	     END IF

   	     IF gr_captura.fecha_liquida IS NULL THEN
   	  	 	  ERROR "DEBE INDICAR FECHA CONVERSION"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
   	  	 	  NEXT FIELD fecha_liquida
   	  	 ELSE
   	  	 	  CALL valida_precios_liq(gr_captura.fecha_liquida)
   	     END IF

   	  	 EXIT INPUT

       ON KEY (INTERRUPT, CONTROL-C)
          PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
          EXIT PROGRAM
   END INPUT

   #Realizar la liquidacion
   DECLARE cur_liquida CURSOR FOR
   SELECT *
   FROM   dis_provision
   WHERE  folio           = gr_captura.folio
   AND    tipo_movimiento = gi_tipo_movimiento

   FOREACH cur_liquida INTO lr_dis_cuenta.*
   	  LET li_siefore = lr_dis_cuenta.siefore
   	  LET lr_dis_cuenta.monto_en_acciones = lr_dis_cuenta.monto_en_pesos /
   	                                        gar_precios_liq[li_siefore].precio

   	  LET lr_dis_cuenta.fecha_pago       = gr_captura.fecha_liquida
   	  LET lr_dis_cuenta.fecha_valor      = gr_captura.fecha_liquida
   	  LET lr_dis_cuenta.fecha_conversion = gr_captura.fecha_liquida
   	  LET lr_dis_cuenta.fecha_proceso    = HOY
   	  LET lr_dis_cuenta.fecha_archivo    = HOY

   	  LET lr_dis_cuenta.precio_accion    = gar_precios_liq[li_siefore].precio

   	  IF lr_dis_cuenta.monto_en_acciones > 0 THEN
   	     INSERT INTO dis_cuenta VALUES(lr_dis_cuenta.*)
   	  END IF
   END FOREACH

   DISPLAY "PROCESO FINALIZADO FOLIO: ", gi_folio AT 18,1 ATTRIBUTE(REVERSE)
   PROMPT  "<ENTER PARA SALIR> " FOR CHAR enter

   INITIALIZE msg1,
              msg2,
              gr_captura.* TO NULL

   DISPLAY BY NAME msg1,
                   msg2,
                   gr_captura.*

   DISPLAY "                                            " AT 18,1

   DISPLAY "                                                                                 " AT 1,1 ATTRIBUTE(REVERSE)
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION valida_precios_liq(ld_fecha_valuacion)
   DEFINE ld_fecha_valuacion DATE,
          li_cont_siefore    SMALLINT,
          li_siefore         SMALLINT,
          ld_precio          LIKE glo_valor_accion.precio_del_dia,
          lc_mensaje         CHAR(72)

   #Obtener precios de accion
   DECLARE cur_precios_liq CURSOR FOR
   SELECT codigo_siefore,
          precio_del_dia
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = ld_fecha_valuacion
   AND    codigo_siefore IN (1,2,3,4,5)
   ORDER  BY 1

   LET li_cont_siefore = 0

   FOREACH cur_precios_liq INTO li_siefore,
   	                        ld_precio

   	  IF ld_precio IS NULL THEN
   	  	 LET ld_precio = 0
   	  END IF

   	  LET gar_precios_liq[li_siefore].siefore = li_siefore
   	  LET gar_precios_liq[li_siefore].precio  = ld_precio

       LET li_cont_siefore = li_cont_siefore + 1
   END FOREACH

   IF li_cont_siefore < 5 THEN
   	  LET    lc_mensaje = "FALTA PRECIOS DE ACCION DE SIEFORES BASICAS DEL DIA: ", ld_fecha_valuacion USING "DD/MM/YYYY"
      PROMPT lc_mensaje FOR CHAR enter
      EXIT PROGRAM
   END IF
END FUNCTION
################################################################################