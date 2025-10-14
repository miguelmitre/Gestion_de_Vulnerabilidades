################################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Programa CTARES1  => RESARCIMIENTO DE CUENTAS CEDIDAS RECHAZADAS              #
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

   CALL STARTLOG("CTARES1.log")
   CALL init()

   OPEN WINDOW CTARES11 AT 4,4 WITH FORM "CTARES11" ATTRIBUTE (BORDER)
   DISPLAY "                                                                                 " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " CTARES1       RESARCIMIENTO DE CUENTAS CEDIDAS RECHAZADAS               " AT 3,1 ATTRIBUTE (REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

   MENU "Resarcimiento"
      COMMAND "Provisionar" "Realizar el calculo del monto a resarcir"
         CALL provisiona()

      COMMAND "Liquidar" "Realizar la liquidacion del monto a resarcir"
         CALL liquida()

      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW CTARES11
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
FUNCTION carga_rendimiento()
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_tasa_rend
   WHENEVER ERROR STOP

   CREATE TABLE tmp_tasa_rend(
   afore   SMALLINT     ,
   siefore SMALLINT     ,
   fecha   DATE         ,
   tasa    DECIMAL (22,6)
   )

   INSERT INTO tmp_tasa_rend VALUES(516,1,"07/01/2009",1.42)
   INSERT INTO tmp_tasa_rend VALUES(516,2,"07/01/2009",0.54)
   INSERT INTO tmp_tasa_rend VALUES(516,3,"07/01/2009",0.22)
   INSERT INTO tmp_tasa_rend VALUES(516,4,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(516,5,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(516,1,"08/01/2009",1.24)
   INSERT INTO tmp_tasa_rend VALUES(516,2,"08/01/2009",0.61)
   INSERT INTO tmp_tasa_rend VALUES(516,3,"08/01/2009",0.36)
   INSERT INTO tmp_tasa_rend VALUES(516,4,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(516,5,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(516,1,"09/01/2009",1.24)
   INSERT INTO tmp_tasa_rend VALUES(516,2,"09/01/2009",0.61)
   INSERT INTO tmp_tasa_rend VALUES(516,3,"09/01/2009",0.5 )
   INSERT INTO tmp_tasa_rend VALUES(516,4,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(516,5,"09/01/2009",0   )

   INSERT INTO tmp_tasa_rend VALUES(532,1,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(532,2,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(532,3,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(532,4,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(532,5,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(532,1,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(532,2,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(532,3,"08/01/2009",0.24)
   INSERT INTO tmp_tasa_rend VALUES(532,4,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(532,5,"08/01/2009",0.42)
   INSERT INTO tmp_tasa_rend VALUES(532,1,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(532,2,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(532,3,"09/01/2009",0.44)
   INSERT INTO tmp_tasa_rend VALUES(532,4,"09/01/2009",0.13)
   INSERT INTO tmp_tasa_rend VALUES(532,5,"09/01/2009",0.42)

   INSERT INTO tmp_tasa_rend VALUES(534,1,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(534,2,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(534,3,"07/01/2009",0.55)
   INSERT INTO tmp_tasa_rend VALUES(534,4,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(534,5,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(534,1,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(534,2,"08/01/2009",0.17)
   INSERT INTO tmp_tasa_rend VALUES(534,3,"08/01/2009",0.82)
   INSERT INTO tmp_tasa_rend VALUES(534,4,"08/01/2009",0.06)
   INSERT INTO tmp_tasa_rend VALUES(534,5,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(534,1,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(534,2,"09/01/2009",0.17)
   INSERT INTO tmp_tasa_rend VALUES(534,3,"09/01/2009",0.62)
   INSERT INTO tmp_tasa_rend VALUES(534,4,"09/01/2009",0.18)
   INSERT INTO tmp_tasa_rend VALUES(534,5,"09/01/2009",0   )

   INSERT INTO tmp_tasa_rend VALUES(538,1,"07/01/2009",0.37)
   INSERT INTO tmp_tasa_rend VALUES(538,2,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,3,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,4,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,5,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,1,"08/01/2009",0.25)
   INSERT INTO tmp_tasa_rend VALUES(538,2,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,3,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,4,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,5,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,1,"09/01/2009",0.25)
   INSERT INTO tmp_tasa_rend VALUES(538,2,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,3,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,4,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(538,5,"09/01/2009",0   )

   INSERT INTO tmp_tasa_rend VALUES(544,1,"07/01/2009",1.6 )
   INSERT INTO tmp_tasa_rend VALUES(544,2,"07/01/2009",0.54)
   INSERT INTO tmp_tasa_rend VALUES(544,3,"07/01/2009",0.64)
   INSERT INTO tmp_tasa_rend VALUES(544,4,"07/01/2009",0.36)
   INSERT INTO tmp_tasa_rend VALUES(544,5,"07/01/2009",0.35)
   INSERT INTO tmp_tasa_rend VALUES(544,1,"08/01/2009",1.48)
   INSERT INTO tmp_tasa_rend VALUES(544,2,"08/01/2009",0.72)
   INSERT INTO tmp_tasa_rend VALUES(544,3,"08/01/2009",0.84)
   INSERT INTO tmp_tasa_rend VALUES(544,4,"08/01/2009",0.3 )
   INSERT INTO tmp_tasa_rend VALUES(544,5,"08/01/2009",0.61)
   INSERT INTO tmp_tasa_rend VALUES(544,1,"09/01/2009",1.48)
   INSERT INTO tmp_tasa_rend VALUES(544,2,"09/01/2009",0.72)
   INSERT INTO tmp_tasa_rend VALUES(544,3,"09/01/2009",1.14)
   INSERT INTO tmp_tasa_rend VALUES(544,4,"09/01/2009",0.95)
   INSERT INTO tmp_tasa_rend VALUES(544,5,"09/01/2009",0.61)

   INSERT INTO tmp_tasa_rend VALUES(550,1,"07/01/2009",1.08)
   INSERT INTO tmp_tasa_rend VALUES(550,2,"07/01/2009",0.41)
   INSERT INTO tmp_tasa_rend VALUES(550,3,"07/01/2009",0.54)
   INSERT INTO tmp_tasa_rend VALUES(550,4,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(550,5,"07/01/2009",0.17)
   INSERT INTO tmp_tasa_rend VALUES(550,1,"08/01/2009",1.2 )
   INSERT INTO tmp_tasa_rend VALUES(550,2,"08/01/2009",0.89)
   INSERT INTO tmp_tasa_rend VALUES(550,3,"08/01/2009",1   )
   INSERT INTO tmp_tasa_rend VALUES(550,4,"08/01/2009",0.09)
   INSERT INTO tmp_tasa_rend VALUES(550,5,"08/01/2009",0.64)
   INSERT INTO tmp_tasa_rend VALUES(550,1,"09/01/2009",1.2 )
   INSERT INTO tmp_tasa_rend VALUES(550,2,"09/01/2009",0.89)
   INSERT INTO tmp_tasa_rend VALUES(550,3,"09/01/2009",0.76)
   INSERT INTO tmp_tasa_rend VALUES(550,4,"09/01/2009",0.16)
   INSERT INTO tmp_tasa_rend VALUES(550,5,"09/01/2009",0.64)

   INSERT INTO tmp_tasa_rend VALUES(552,1,"07/01/2009",2.1 )
   INSERT INTO tmp_tasa_rend VALUES(552,2,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,3,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,4,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,5,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,1,"08/01/2009",1.85)
   INSERT INTO tmp_tasa_rend VALUES(552,2,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,3,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,4,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,5,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,1,"09/01/2009",1.85)
   INSERT INTO tmp_tasa_rend VALUES(552,2,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,3,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,4,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(552,5,"09/01/2009",0   )

   INSERT INTO tmp_tasa_rend VALUES(554,1,"07/01/2009",0.5 )
   INSERT INTO tmp_tasa_rend VALUES(554,2,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,3,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,4,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,5,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,1,"08/01/2009",0.44)
   INSERT INTO tmp_tasa_rend VALUES(554,2,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,3,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,4,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,5,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,1,"09/01/2009",0.44)
   INSERT INTO tmp_tasa_rend VALUES(554,2,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,3,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,4,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(554,5,"09/01/2009",0   )

   INSERT INTO tmp_tasa_rend VALUES(556,1,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(556,2,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(556,3,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(556,4,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(556,5,"07/01/2009",0.45)
   INSERT INTO tmp_tasa_rend VALUES(556,1,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(556,2,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(556,3,"08/01/2009",0.2 )
   INSERT INTO tmp_tasa_rend VALUES(556,4,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(556,5,"08/01/2009",0.95)
   INSERT INTO tmp_tasa_rend VALUES(556,1,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(556,2,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(556,3,"09/01/2009",0.28)
   INSERT INTO tmp_tasa_rend VALUES(556,4,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(556,5,"09/01/2009",0.95)

   INSERT INTO tmp_tasa_rend VALUES(574,1,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(574,2,"07/01/2009",0.04)
   INSERT INTO tmp_tasa_rend VALUES(574,3,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(574,4,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(574,5,"07/01/2009",0.6 )
   INSERT INTO tmp_tasa_rend VALUES(574,1,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(574,2,"08/01/2009",0.49)
   INSERT INTO tmp_tasa_rend VALUES(574,3,"08/01/2009",0.51)
   INSERT INTO tmp_tasa_rend VALUES(574,4,"08/01/2009",0.11)
   INSERT INTO tmp_tasa_rend VALUES(574,5,"08/01/2009",1.23)
   INSERT INTO tmp_tasa_rend VALUES(574,1,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(574,2,"09/01/2009",0.49)
   INSERT INTO tmp_tasa_rend VALUES(574,3,"09/01/2009",0.75)
   INSERT INTO tmp_tasa_rend VALUES(574,4,"09/01/2009",0.66)
   INSERT INTO tmp_tasa_rend VALUES(574,5,"09/01/2009",1.23)

   INSERT INTO tmp_tasa_rend VALUES(576,1,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,2,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,3,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,4,"07/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,5,"07/01/2009",0.59)
   INSERT INTO tmp_tasa_rend VALUES(576,1,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,2,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,3,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,4,"08/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,5,"08/01/2009",1.01)
   INSERT INTO tmp_tasa_rend VALUES(576,1,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,2,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,3,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,4,"09/01/2009",0   )
   INSERT INTO tmp_tasa_rend VALUES(576,5,"09/01/2009",1.01)

   DATABASE safre_af
END FUNCTION
################################################################################
FUNCTION provisiona()
   DEFINE lr_tmp_nss_resarce RECORD
   	      folio          INTEGER      ,
   	      nss            CHAR(11)     ,
   	      afore_recep    SMALLINT     ,
   	      siefore        SMALLINT     ,
   	      acciones       DECIMAL(22,6),

   	      saldo_jul       DECIMAL(22,6),
   	      sal_prom_jul    DECIMAL(22,6),
   	      interes_jul     DECIMAL(22,6),
   	      tasa_jul        DECIMAL(22,6),

   	      saldo_ago       DECIMAL(22,6),
   	      sal_prom_ago    DECIMAL(22,6),
   	      interes_ago     DECIMAL(22,6),
   	      tasa_ago        DECIMAL(22,6),

   	      saldo_sep       DECIMAL(22,6),
   	      sal_prom_sep    DECIMAL(22,6),
   	      interes_sep     DECIMAL(22,6),
   	      tasa_sep        DECIMAL(22,6),

   	      interes_tot     DECIMAL(22,6)
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

   #Carga la tabla de rendimientos
   CALL carga_rendimiento()

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

   CALL valida_precios()

   DECLARE cur_detalle CURSOR FOR
   SELECT nss        ,
          afore_recep,
          siefore
   FROM   safre_tmp:tmp_nss_resarce
   ORDER BY nss

   #Obtener folio para la provision
   LET gi_folio = 0
   SELECT MAX( folio ) + 1 INTO gi_folio FROM glo_folio
   INSERT INTO glo_folio VALUES( gi_folio )

   FOREACH cur_detalle INTO lr_tmp_nss_resarce.nss,
   	                        lr_tmp_nss_resarce.afore_recep,
   	                        lr_tmp_nss_resarce.siefore

   	  #Obtener las tasas de acuerdo a la afore_recep y siefore
   	  CALL get_tasa(lr_tmp_nss_resarce.afore_recep, lr_tmp_nss_resarce.siefore)
   	       RETURNING ld_tasa_jul,
                     ld_tasa_ago,
                     ld_tasa_sep

      #Obtener saldo inicial en acciones
      CALL calcula_julio(lr_tmp_nss_resarce.nss,
                         ld_tasa_jul)
           RETURNING lr_tmp_nss_resarce.acciones    ,
                     lr_tmp_nss_resarce.saldo_jul   ,
                     lr_tmp_nss_resarce.sal_prom_jul,
                     lr_tmp_nss_resarce.interes_jul


      LET ld_saldo_inicial = lr_tmp_nss_resarce.saldo_jul +
                             lr_tmp_nss_resarce.interes_jul

      #Realizar calculos de agosto
      CALL calcula_agosto(lr_tmp_nss_resarce.nss,
                          ld_tasa_ago           ,
                          ld_saldo_inicial)
           RETURNING lr_tmp_nss_resarce.saldo_ago   ,
                     lr_tmp_nss_resarce.sal_prom_ago,
                     lr_tmp_nss_resarce.interes_ago ,
                     ld_saldo_inicial --Saldo final del ultimo día del mes anterior

      #--Saldo final del ultimo día del mes anterior + monto a resarcir del mes anterior
      LET ld_saldo_inicial = ld_saldo_inicial +
                             lr_tmp_nss_resarce.interes_ago

      #Realizar calculos de septiembre
      CALL calcula_septiembre(lr_tmp_nss_resarce.nss,
                              ld_tasa_sep           ,
                              ld_saldo_inicial)
           RETURNING lr_tmp_nss_resarce.saldo_sep   ,
                     lr_tmp_nss_resarce.sal_prom_sep,
                     lr_tmp_nss_resarce.interes_sep

      LET lr_tmp_nss_resarce.interes_tot = lr_tmp_nss_resarce.interes_jul +
                                           lr_tmp_nss_resarce.interes_ago +
                                           lr_tmp_nss_resarce.interes_sep

      LET li_siefore = lr_tmp_nss_resarce.siefore

      LET ld_acciones = lr_tmp_nss_resarce.interes_tot /
                        gar_precios_hoy[li_siefore].precio

      #Guardar calculos
      UPDATE safre_tmp:tmp_nss_resarce
      SET    folio        = gi_folio                       ,
             acciones     = lr_tmp_nss_resarce.acciones    ,
             saldo_jul    = lr_tmp_nss_resarce.saldo_jul   ,
             sal_prom_jul = lr_tmp_nss_resarce.sal_prom_jul,
             interes_jul  = lr_tmp_nss_resarce.interes_jul ,
             tasa_jul     = ld_tasa_jul                    ,
             saldo_ago    = lr_tmp_nss_resarce.saldo_ago   ,
             sal_prom_ago = lr_tmp_nss_resarce.sal_prom_ago,
             interes_ago  = lr_tmp_nss_resarce.interes_ago ,
             tasa_ago     = ld_tasa_ago                    ,
             saldo_sep    = lr_tmp_nss_resarce.saldo_sep   ,
             sal_prom_sep = lr_tmp_nss_resarce.sal_prom_sep,
             interes_sep  = lr_tmp_nss_resarce.interes_sep ,
             tasa_sep     = ld_tasa_sep                    ,
             interes_tot  = lr_tmp_nss_resarce.interes_tot
      WHERE  nss          = lr_tmp_nss_resarce.nss
      AND    afore_recep  = lr_tmp_nss_resarce.afore_recep
      AND    siefore      = lr_tmp_nss_resarce.siefore

      #Provisionar cuenta
      IF lr_tmp_nss_resarce.interes_tot > 0 THEN
         INSERT INTO dis_provision VALUES(
         gi_tipo_movimiento                                ,
         1                                                 ,
         lr_tmp_nss_resarce.siefore                        ,
         gi_folio                                          ,
         0                                                 ,
         lr_tmp_nss_resarce.nss                            ,
         NULL                                              ,
         NULL                                              ,
         HOY                                               ,
         HOY                                               ,
         HOY                                               ,
         lr_tmp_nss_resarce.interes_tot                    ,
         ld_acciones                                       ,
         gar_precios_hoy[li_siefore].precio,
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
   	      afore_recep    SMALLINT     ,
   	      siefore        SMALLINT     ,
   	      acciones       DECIMAL(22,6),

   	      saldo_jul       DECIMAL(22,6),
   	      sal_prom_jul    DECIMAL(22,6),
   	      interes_jul     DECIMAL(22,6),
   	      tasa_jul        DECIMAL(22,6),

   	      saldo_ago       DECIMAL(22,6),
   	      sal_prom_ago    DECIMAL(22,6),
   	      interes_ago     DECIMAL(22,6),
   	      tasa_ago        DECIMAL(22,6),

   	      saldo_sep       DECIMAL(22,6),
   	      sal_prom_sep    DECIMAL(22,6),
   	      interes_sep     DECIMAL(22,6),
   	      tasa_sep        DECIMAL(22,6),

   	      interes_tot     DECIMAL(22,6)
   END RECORD

   DEFINE lc_archivo CHAR(200)

   WHENEVER ERROR CONTINUE
      DROP TABLE nss_tmp
   WHENEVER ERROR STOP

   #Cargar archivo plano
   CREATE TEMP TABLE nss_tmp(
   afore_recep    SMALLINT,
   nss            CHAR(11),
   siefore        SMALLINT
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
   	      afore_recep    SMALLINT     ,
   	      siefore        SMALLINT     ,
   	      acciones       DECIMAL(22,6),

   	      saldo_jul       DECIMAL(22,6),
   	      sal_prom_jul    DECIMAL(22,6),
   	      interes_jul     DECIMAL(22,6),
   	      tasa_jul        DECIMAL(22,6),

   	      saldo_ago       DECIMAL(22,6),
   	      sal_prom_ago    DECIMAL(22,6),
   	      interes_ago     DECIMAL(22,6),
   	      tasa_ago        DECIMAL(22,6),

   	      saldo_sep       DECIMAL(22,6),
   	      sal_prom_sep    DECIMAL(22,6),
   	      interes_sep     DECIMAL(22,6),
   	      tasa_sep        DECIMAL(22,6),

   	      interes_tot     DECIMAL(22,6)
      )

   DATABASE safre_af

   #Cargar tabla de detalle
   DECLARE cur_plano CURSOR FOR
   SELECT *
   FROM   nss_tmp
   ORDER BY nss

   FOREACH cur_plano INTO lr_nss_plano.afore_recep,
                          lr_nss_plano.nss,
   	                      lr_nss_plano.siefore

   	  INITIALIZE lr_nss_plano.folio       ,
   	             lr_nss_plano.acciones    ,
                 lr_nss_plano.saldo_jul   ,
                 lr_nss_plano.sal_prom_jul,
                 lr_nss_plano.interes_jul ,
                 lr_nss_plano.tasa_jul    ,
                 lr_nss_plano.saldo_ago   ,
                 lr_nss_plano.sal_prom_ago,
                 lr_nss_plano.interes_ago ,
                 lr_nss_plano.tasa_ago    ,
                 lr_nss_plano.saldo_sep   ,
                 lr_nss_plano.sal_prom_sep,
                 lr_nss_plano.interes_sep ,
                 lr_nss_plano.tasa_sep    ,
                 lr_nss_plano.interes_tot  TO NULL

      INSERT INTO safre_tmp:tmp_nss_resarce VALUES(lr_nss_plano.*)
   END FOREACH
END FUNCTION
################################################################################
FUNCTION get_tasa(li_afore_recep,
                  li_siefore)

   DEFINE li_afore_recep,
          li_siefore    SMALLINT

   DEFINE ld_tasa_jul,
          ld_tasa_ago,
          ld_tasa_sep   DECIMAL(22,6)

   DEFINE ld_mes_jul,
          ld_mes_ago,
          ld_mes_sep   DATE

   LET ld_mes_jul = MDY (7,1, YEAR(hoy))
   LET ld_mes_ago = MDY (8,1, YEAR(hoy))
   LET ld_mes_sep = MDY (9,1, YEAR(hoy))

   INITIALIZE ld_tasa_jul,
              ld_tasa_ago,
              ld_tasa_sep TO NULL

   SELECT tasa
   INTO   ld_tasa_jul
   FROM   safre_tmp:tmp_tasa_rend
   WHERE  afore   = li_afore_recep
   AND    siefore = li_siefore
   AND    fecha   = ld_mes_jul

   SELECT tasa
   INTO   ld_tasa_ago
   FROM   safre_tmp:tmp_tasa_rend
   WHERE  afore   = li_afore_recep
   AND    siefore = li_siefore
   AND    fecha   = ld_mes_ago

   SELECT tasa
   INTO   ld_tasa_sep
   FROM   safre_tmp:tmp_tasa_rend
   WHERE  afore   = li_afore_recep
   AND    siefore = li_siefore
   AND    fecha   = ld_mes_sep

   IF ld_tasa_jul IS NULL THEN
      LET ld_tasa_jul = 0
   END IF

   IF ld_tasa_ago IS NULL THEN
      LET ld_tasa_ago = 0
   END IF

   IF ld_tasa_sep IS NULL THEN
      LET ld_tasa_sep = 0
   END IF

   RETURN ld_tasa_jul,
          ld_tasa_ago,
          ld_tasa_sep
END FUNCTION
################################################################################
FUNCTION valida_precios()
   DEFINE ld_fecha_valuacion DATE,
          li_cont_siefore    SMALLINT,
          li_siefore         SMALLINT,
          ld_precio          LIKE glo_valor_accion.precio_del_dia,
          lc_mensaje         CHAR(72)

   #Obtener precios de accion
   DECLARE cur_precios CURSOR FOR
   SELECT codigo_siefore,
          precio_del_dia
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = "07/31/2009"
   AND    codigo_siefore IN (1,2,3,4,5)
   ORDER  BY 1

   LET li_cont_siefore = 0

   FOREACH cur_precios INTO li_siefore,
   	                        ld_precio

   	  IF ld_precio IS NULL THEN
   	  	 LET ld_precio = 0
   	  END IF

   	  LET gar_precios_jul[li_siefore].siefore = li_siefore
   	  LET gar_precios_jul[li_siefore].precio  = ld_precio

       LET li_cont_siefore = li_cont_siefore + 1
   END FOREACH

   IF li_cont_siefore < 5 THEN
   	  LET    lc_mensaje = "FALTA PRECIOS DE ACCION DE SIEFORES BASICAS DEL DIA: 31/07/2009"
      PROMPT lc_mensaje FOR CHAR enter
      EXIT PROGRAM
   END IF

   #Obtener precios de accion del dia
   DECLARE cur_precios_hoy CURSOR FOR
   SELECT codigo_siefore,
          precio_del_dia
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = hoy
   AND    codigo_siefore IN (1,2,3,4,5)
   ORDER  BY 1

   FOREACH cur_precios_hoy INTO li_siefore,
   	                            ld_precio

   	  IF ld_precio IS NULL THEN
   	  	 LET ld_precio = 0
   	  END IF

   	  LET gar_precios_hoy[li_siefore].siefore = li_siefore
   	  LET gar_precios_hoy[li_siefore].precio  = ld_precio
   END FOREACH

END FUNCTION
################################################################################
FUNCTION calcula_julio(lc_nss, ld_tasa)
   DEFINE lc_nss     CHAR(11),
          ld_tasa            ,
          ld_acciones        ,
          ld_pesos   DECIMAL(22,6)

   DEFINE ld_acciones_tot,
          ld_pesos_tot   ,
          ld_saldo_prom  ,
          ld_intereses   DECIMAL(22,6)

   DEFINE li_siefore          SMALLINT
   DEFINE ld_fecha_conversion DATE
   DEFINE li_dias             SMALLINT

   LET ld_fecha_conversion = "07/31/2009"
   LET li_dias             = 1

   DECLARE cur_julio CURSOR FOR
   SELECT siefore,
          SUM(monto_en_acciones)
   FROM   dis_cuenta
   WHERE  nss               = lc_nss
   AND    fecha_conversion <= ld_fecha_conversion
   AND    siefore IN (1,2,3,4,5)
   GROUP BY 1
   ORDER BY 1

   LET ld_acciones_tot = 0
   LET ld_pesos_tot    = 0
   LET ld_saldo_prom   = 0
   LET ld_intereses    = 0

   FOREACH cur_julio INTO li_siefore,
   	                      ld_acciones

   	  IF ld_acciones IS NULL THEN
   	  	 LET ld_acciones = 0
   	  END IF

   	  LET ld_pesos = ld_acciones * gar_precios_jul[li_siefore].precio

   	  #Acumular
   	  LET ld_acciones_tot = ld_acciones_tot + ld_acciones
   	  LET ld_pesos_tot    = ld_pesos_tot    + ld_pesos
   END FOREACH

   LET ld_saldo_prom = ld_pesos_tot / li_dias
   LET ld_intereses = ld_saldo_prom * ld_tasa / 360 * li_dias /100

   {DISPLAY "lc_nss: ", lc_nss
   DISPLAY "ld_fecha_conversion: ", ld_fecha_conversion
   DISPLAY "li_siefore: ", li_siefore
   DISPLAY "ld_acciones: ", ld_acciones
   SLEEP 8}

   RETURN ld_acciones_tot,
          ld_pesos_tot   ,
          ld_saldo_prom  ,
          ld_intereses

END FUNCTION
################################################################################
FUNCTION calcula_agosto(lc_nss, ld_tasa, ld_saldo_inicial)
   DEFINE lc_nss     CHAR(11),
          ld_tasa            ,
          ld_acciones        ,
          ld_saldo_inicial   ,
          ld_pesos   DECIMAL(22,6)

   DEFINE ld_acciones_tot,
          ld_pesos_tot   ,
          ld_saldo_prom  ,
          ld_intereses   DECIMAL(22,6)

   DEFINE ld_fecha_conversion DATE
   DEFINE li_dias             SMALLINT

   DEFINE lar_saldo_diario ARRAY[31] OF RECORD
   	          saldo_diario DECIMAL(22,6)
          END RECORD


   LET li_dias         = 31
   LET ld_acciones_tot = 0
   LET ld_saldo_prom   = 0
   LET ld_intereses    = 0
   LET ld_pesos_tot    = ld_saldo_inicial

   FOR li_dias = 1 TO 31
   	  #Realizar calculo dia por dia
   	  LET ld_fecha_conversion = MDY(8,li_dias,YEAR(hoy))

   	  INITIALIZE ld_pesos TO NULL

   	  SELECT SUM(monto_en_pesos)
   	  INTO   ld_pesos
      FROM   dis_cuenta
      WHERE  nss              = lc_nss
      AND    fecha_conversion = ld_fecha_conversion
      AND    tipo_movimiento  <> 110
      AND    siefore IN (1,2,3,4,5)

      IF ld_pesos IS NULL THEN
      	 LET ld_pesos = 0
      END IF

      #Saldo al dia
      LET ld_pesos_tot = ld_pesos_tot + ld_pesos

      LET lar_saldo_diario[li_dias].saldo_diario = ld_pesos_tot
   END FOR

   LET ld_pesos_tot = 0

   #Obtener suma total
   FOR li_dias = 1 TO 31
   	  LET ld_pesos_tot = ld_pesos_tot + lar_saldo_diario[li_dias].saldo_diario
   END FOR

   LET li_dias = 31

   #Promedio del saldo diario
   LET ld_saldo_prom = ld_pesos_tot / 31
   LET ld_intereses  = ld_saldo_prom * ld_tasa / 360 * li_dias /100

   RETURN ld_pesos_tot   ,
          ld_saldo_prom  ,
          ld_intereses   ,
          lar_saldo_diario[31].saldo_diario

END FUNCTION
################################################################################
FUNCTION calcula_septiembre(lc_nss, ld_tasa, ld_saldo_inicial)
   DEFINE lc_nss     CHAR(11),
          ld_tasa            ,
          ld_acciones        ,
          ld_saldo_inicial   ,
          ld_pesos   DECIMAL(22,6)

   DEFINE ld_acciones_tot,
          ld_pesos_tot   ,
          ld_saldo_prom  ,
          ld_intereses   DECIMAL(22,6)

   DEFINE ld_fecha_conversion DATE
   DEFINE li_dias             SMALLINT

   DEFINE lar_saldo_diario ARRAY[29] OF RECORD
   	          saldo_diario DECIMAL(22,6)
          END RECORD

   LET ld_acciones_tot = 0
   LET ld_saldo_prom   = 0
   LET ld_intereses    = 0
   LET ld_pesos_tot    = ld_saldo_inicial

   FOR li_dias = 1 TO 29
   	  #Realizar calculo dia por dia
   	  LET ld_fecha_conversion = MDY(9,li_dias,YEAR(hoy))

   	  INITIALIZE ld_pesos TO NULL

   	  SELECT SUM(monto_en_pesos)
   	  INTO   ld_pesos
      FROM   dis_cuenta
      WHERE  nss              = lc_nss
      AND    fecha_conversion = ld_fecha_conversion
      AND    tipo_movimiento  <> 110
      AND    siefore IN (1,2,3,4,5)

      IF ld_pesos IS NULL THEN
      	 LET ld_pesos = 0
      END IF

      #Saldo al dia
      LET ld_pesos_tot = ld_pesos_tot + ld_pesos

      LET lar_saldo_diario[li_dias].saldo_diario = ld_pesos_tot
   END FOR

   LET ld_pesos_tot = 0

   #Obtener suma total
   FOR li_dias = 1 TO 29
   	  LET ld_pesos_tot = ld_pesos_tot + lar_saldo_diario[li_dias].saldo_diario
   END FOR

   LET li_dias = 29

   #Promedio del saldo diario
   LET ld_saldo_prom = ld_pesos_tot / 29
   LET ld_intereses  = ld_saldo_prom * ld_tasa / 360 * li_dias /100

   RETURN ld_pesos_tot   ,
          ld_saldo_prom  ,
          ld_intereses

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