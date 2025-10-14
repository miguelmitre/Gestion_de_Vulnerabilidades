#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0303 Y INTB03031                          #
#Descripcion       => APORTACIONES (VOL,COMPLE,INDEPEN,LARGO PLAZO, #
#                  => RETIRO,AHORRO SOLID)                          #
#Sistema           => INT .                                         #
#Fecha             => 17 de Noviembre del 2005.                     #
#Por               => LAURA EUGENIA CORTES GUZMAN                   #
#Fecha Modif.      => 17 de Noviembre del 2005.                     #
#Por               => LAURA EUGENIA CORTES GUZMAN                   #
#Fecha Modif.      => 27 OCTUBRE 2011                               #
#Por               => STEFANIE DANIELA VERA PIÑA                    #
#*******************************************************************#
DATABASE  safre_af

GLOBALS
   DEFINE
      aux_pausa       CHAR(1),
      enter           CHAR(01),
      generar         CHAR(200),
      letra           CHAR(01),
      log1            CHAR(40),
      usuario         CHAR(8),
      v_arch          CHAR(60),
      vreporte        CHAR(200),
      vruta_ejecuta   CHAR(40),
      vruta_rescate   CHAR(40)

   DEFINE
      v_folio_cod     INTEGER

   DEFINE
      vtot_acep       ,
      vtot_rech       ,
      vtot_reg        SMALLINT

END GLOBALS


MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   SELECT USER
   INTO   usuario
   FROM   seg_usuario
   GROUP BY 1

   CALL STARTLOG("INTB0303.log")

   OPEN WINDOW v1 AT 2,3 WITH 3 ROWS, 76 COLUMNS ATTRIBUTE(BORDER)
   MENU "APORTACIONES VENTANILLA"
      COMMAND "Carga Archivo" "Carga Archivo de Aportaciones Realizadas en Ventanilla"
         CALL inicio()
         CALL principal()
         CLEAR SCREEN
      COMMAND "Reversa Carga" "Reversa Carga de Archivo"
         CALL inicio()
         CALL reverso()
         CLEAR SCREEN
      COMMAND "Consulta Aceptados" "Consulta de Aportaciones Aceptadas en la Carga"
         CALL inicio()
         CALL consulta_a()
      COMMAND "Consulta Rechazos" "Consulta de Aportaciones Rechazadas en la Carga"
         CALL inicio()
         CALL consulta_b()
      COMMAND "Salir " "Salir"
         EXIT MENU
   END MENU
   CLOSE WINDOW v1

END MAIN


FUNCTION inicio()
#----------------

   SELECT ruta_exp,
          ruta_rescate
   INTO   vruta_ejecuta,
          vruta_rescate
   FROM   seg_modulo
   WHERE  modulo_cod = "int"

END FUNCTION


FUNCTION principal()
#-------------------

   DEFINE
      ejecuta       CHAR(200),
      vpausa        CHAR(1)

   DEFINE
      HOY           DATE

   DEFINE
      v_folio_cod   INTEGER

   LET HOY = TODAY

   LET ejecuta = "cd ",vruta_rescate CLIPPED,"; ls > archivos_vol" CLIPPED
   RUN ejecuta

   CREATE TEMP TABLE archivos_vol (lista  CHAR(100))

   LET ejecuta = vruta_rescate CLIPPED,"/archivos_vol" CLIPPED

   LOAD FROM ejecuta INSERT INTO archivos_vol

   LET ejecuta = "rm ",vruta_rescate CLIPPED,"/archivos_vol" CLIPPED
   RUN ejecuta

   OPEN WINDOW v2 AT 5,3 WITH FORM "INTB03031" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " INTB0303                 APORTACIONES VENTANILLA                           " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY  USING"DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

   INPUT BY NAME v_arch WITHOUT DEFAULTS
      AFTER FIELD v_arch
         IF v_arch IS NULL THEN
            ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO "
            NEXT FIELD v_arch
         END  IF

         SELECT "x"
         FROM   archivos_vol
         WHERE  lista = v_arch

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NOMBRE DE ARCHIVO INCORRECTO ... NO EXISTE "
            NEXT FIELD v_arch
         END IF

      ON KEY (ESC)
         IF v_arch IS NULL THEN
            ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO "
            NEXT FIELD v_arch
         END IF

         SELECT "x"
         FROM   archivos_vol
         WHERE  lista = v_arch

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO ... NO EXISTE"
            NEXT FIELD v_arch
         END IF

         SELECT "X"
         FROM   int_archivo_vol
         WHERE  nombre_archivo = v_arch

         IF SQLCA.SQLCODE = 0 THEN
            ERROR " NOMBRE DE ARCHIVO YA CARGADO "
            NEXT FIELD v_arch
         ELSE
            INSERT INTO int_archivo_vol
            VALUES (v_arch,      -- nombre del archivo
                    HOY,         -- fecha de carga del archivo
                    usuario,     -- usuario que carga el archivo
                    NULL         -- folio
                    )
         END IF

         DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)

         CALL Separa_archivo()

         CALL Sube_datos()
         RETURNING v_folio_cod

         CALL Actualiza_registros(v_folio_cod)

         PROMPT " PROCESO FINALIZADO,FOLIO ASIGNADO:",
                v_folio_cod USING "<<<<<<",
                " <ENTER> PARA SALIR " ATTRIBUTE (REVERSE)
         FOR vpausa

         EXIT INPUT

      ON KEY (INTERRUPT,CONTROL-C)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
         FOR enter
         EXIT INPUT

   END INPUT
   DROP TABLE archivos_vol
   CLOSE WINDOW v2

END FUNCTION


FUNCTION Separa_archivo()
#------------------------

   DEFINE
      ejecuta   CHAR(200)

   LET ejecuta = "sed -e '/^E/!d' ",vruta_rescate CLIPPED,"/",
                v_arch CLIPPED," >",vruta_rescate CLIPPED,"/cza_vol.31"
   RUN ejecuta

   LET ejecuta = "sed -e '/^D/!d' ",vruta_rescate CLIPPED,"/",
                v_arch CLIPPED," >",vruta_rescate CLIPPED,"/det_vol.31"
   RUN ejecuta

   LET ejecuta = "sed -e '/^S/!d' ",vruta_rescate CLIPPED,"/",
                v_arch CLIPPED," >",vruta_rescate CLIPPED,"/sum_vol.31"
   RUN ejecuta


--   PROMPT "Separa archivo" FOR aux_pausa

END FUNCTION


FUNCTION Sube_datos()
#--------------------

   DEFINE
      ejecuta             CHAR(200),
      vtipo_reporte       CHAR(1),
      vtipo_registro      CHAR(2),
      vresult_operacion   CHAR(2),
      vhora_recepcion     CHAR(8),
      vcontenido          CHAR(1)

   DEFINE
      vfecha_recepcion    DATE

   DEFINE
      v_folio_cod         INTEGER


   LET v_folio_cod = 0

   INSERT INTO safre_af:glo_folio
   VALUES(0)

   SELECT MAX(folio)
   INTO v_folio_cod
   FROM safre_af:glo_folio

   UPDATE int_archivo_vol
   SET folio = v_folio_cod
   WHERE  nombre_archivo = v_arch

   --------------------   GENERA adi   --------------------

   LET vreporte = vruta_rescate CLIPPED,"/adi"

   START REPORT salida TO vreporte
      LET vcontenido = "a"

      OUTPUT TO REPORT salida(vcontenido)
   FINISH REPORT salida

   --------------------   GENERA cza_vol.31   --------------------

   LET vreporte = vruta_rescate CLIPPED,"/vfolio_cza"

   START REPORT salida1 TO vreporte
      LET vtipo_reporte = "C"   ---- Cabeza
      LET vtipo_registro = "01"
      LET vfecha_recepcion = TODAY
      LET vhora_recepcion  = TIME

      OUTPUT TO REPORT salida1(v_folio_cod,
                               vtipo_reporte,
                               vtipo_registro, "",
                               vfecha_recepcion,
                               vhora_recepcion )
   FINISH REPORT salida1

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; paste vfolio_cza cza_vol.31 > cza1" CLIPPED
   RUN ejecuta

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "/;sed 's/	//g' cza1 > cza_vol.31 "
   RUN ejecuta

   LET ejecuta = "cd ",vruta_ejecuta CLIPPED, "/;",
##                 "DBDATE=y4md;export DBDATE;",
                 "dbload -d safre_af -c sube_volcza_301 -l /tmp/",
                 usuario CLIPPED,".VOL_CZA.log;"
   RUN ejecuta

   --------------------   GENERA det_vol.31   --------------------

   LET vreporte = vruta_rescate CLIPPED,"/vfolio_det"

   START REPORT salida1 TO vreporte
      LET vtipo_reporte = "D"   ---- Detalle
      LET vtipo_registro = "02"
      LET vresult_operacion  = "01"

      OUTPUT TO REPORT salida1(v_folio_cod,
                               vtipo_reporte,
                               vtipo_registro,
                               vresult_operacion,
                               "", "" )
   FINISH REPORT salida1

   -- Se crea archivo adi que contiene a\  y se pega en vfolio_det

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; cat adi vfolio_det > vfolio_det1" CLIPPED
   RUN ejecuta

   -- Se crea archivo vfolio_det con el numero de registros igual al
   -- num. registros que tiene el detalle

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; sed -f vfolio_det1 det_vol.31 > vfolio_det2" CLIPPED
   RUN ejecuta

   -- Se crea archivo vfolio_det borrando lineas que no sirven

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; sed -e '/^ /!d' vfolio_det2 > vfolio_det3" CLIPPED 
   RUN ejecuta

   -- Se crea det1 pegando datos genrales con el detalle 

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; paste vfolio_det3 det_vol.31 > det1" CLIPPED
   RUN ejecuta

   -- Se crea det eliminando espacio en blanco que se genera cuando
   -- se pegan los 2 archivos

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "/;sed 's/	//g' det1 > det_vol.31 "
   RUN ejecuta

   -- Se suben los datos del detalle
   -- PROMPT "DBLOAD" FOR aux_pausa

   LET ejecuta = "cd ",vruta_ejecuta CLIPPED, "/;",
                 "DBDATE=y4md;export DBDATE;",
                 "dbload -d safre_af -c sube_voldet_301 -l /tmp/",
                 usuario CLIPPED,".VOL_DET.log;"
   RUN ejecuta

   --------------------   GENERA sum_vol.31   --------------------

   LET vreporte = vruta_rescate CLIPPED,"/vfolio_sum"

   START REPORT salida1 TO vreporte
      LET vtipo_reporte = "S"   ---- Sumario
      LET vtipo_registro = "09"

      OUTPUT TO REPORT salida1(v_folio_cod,
                              vtipo_reporte,
                              vtipo_registro,
                              "", "", "" )
   FINISH REPORT salida1

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; paste vfolio_sum sum_vol.31 > sum1" CLIPPED
   RUN ejecuta

   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                    "/;sed 's/	//g' sum1 > sum_vol.31 "
   RUN ejecuta


   LET ejecuta = "cd ",vruta_ejecuta CLIPPED, "/;",
##                 "DBDATE=y4md;export DBDATE;",
                 "dbload -d safre_af -c sube_volsum_301 -l /tmp/",
                 usuario CLIPPED,".VOL_SUM.log;"
   RUN ejecuta

   -- Se borran todo los archivo auxiliares
{
   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "; rm vfolio_cza cza_vol.31 cza1 det_vol.31 sum_vol.31 ",
                 "adi vfolio_det vfolio_det1 vfolio_det2 vfolio_det3 ",
                 "det1 vfolio_sum sum1 "
   RUN ejecuta
}
   LET ejecuta = "cd ",vruta_rescate CLIPPED,
                 "/;DBDATE=mdy4;export DBDATE;"
   RUN ejecuta

   RETURN v_folio_cod

END FUNCTION


FUNCTION Actualiza_registros(v_folio_cod)
#----------------------------------------

   DEFINE reg RECORD
      numero                 INTEGER,
      tipo_movimiento        CHAR(1),
      nss                    CHAR(11),
      monto_neto             DECIMAL(17,2),
      monto_sin_iva          DECIMAL(17,2),
      impuesto_retenido      DECIMAL(17,2),
      tipo_aportacion        CHAR(1),
      curp                   CHAR(18)
   END RECORD

   DEFINE reg1 RECORD
      numero                 INTEGER,
      nss                    CHAR(11),
      monto_neto             DECIMAL(17,2),
      monto_sin_iva          DECIMAL(17,2),
      impuesto_retenido      DECIMAL(17,2),
      forma_pago             SMALLINT,
      tipo_retiro            CHAR(3)
   END RECORD

   DEFINE sum1 RECORD
      folio                  INTEGER,
      mto_tot_neto           DECIMAL(17,2),
      mto_tot_con_impuesto   DECIMAL(17,2),
      mto_tot_retenido       DECIMAL(17,2),
      mto_t_ret_pag_depo     DECIMAL(17,2),
      mto_t_ret_pag_cheq     DECIMAL(17,2)
   END RECORD

   DEFINE
      tipo_vol               CHAR(1),
      tipo_retiro            CHAR(3),
      vnss                   CHAR(11),
      vcta_concentra         CHAR(10)

   DEFINE
      tot_v                  ,
      tot_c                  ,
      tot_l                  ,
      tot_i                  ,
      tot_as                 ,
      tot_r                  ,
      tot_z                  ,
      tot_reintegro          ,
      v_folio_cod            INTEGER

   DEFINE
      estado                 SMALLINT

   DEFINE
      det_monto              DECIMAL(13,2),
      vimporte_total         DECIMAL(13,2),
      sum_importe_total      DECIMAL(13,2)

   LET tot_v  = 0
   LET tot_c  = 0
   LET tot_l  = 0
   LET tot_i  = 0
   LET tot_as = 0
   LET tot_r  = 0
   LET tot_z  = 0
   LET tot_reintegro = 0

   UPDATE int_cza_voluntaria
   SET    usuario = USER
   WHERE  folio = v_folio_cod

   UPDATE int_sum_voluntaria
   SET    usuario = USER
   WHERE  folio = v_folio_cod

   DECLARE cur0 CURSOR FOR
   SELECT a.folio,
          NVL( a.mto_tot_neto, 0 ) / 100,
          NVL( a.mto_tot_con_impuesto, 0 ) / 100,
          NVL( a.mto_tot_retenido, 0 ) / 100,
          NVL( a.mto_t_ret_pag_depo, 0 ) / 100,
          NVL( a.mto_t_ret_pag_cheq, 0 ) / 100
   FROM   int_sum_voluntaria a
   WHERE  a.folio = v_folio_cod

   FOREACH cur0 INTO sum1.*
      UPDATE int_sum_voluntaria
      SET    mto_tot_neto        = sum1.mto_tot_neto,
             mto_tot_con_impuesto= sum1.mto_tot_con_impuesto,
             mto_tot_retenido    = sum1.mto_tot_retenido,
             mto_t_ret_pag_depo  = sum1.mto_t_ret_pag_depo,
             mto_t_ret_pag_cheq  = sum1.mto_t_ret_pag_cheq,
             usuario             = USER
      WHERE  folio = sum1.folio
   END FOREACH

   DECLARE cur1 CURSOR FOR
   SELECT a.rowid,
          a.tipo_movimiento,
          a.nss,
          NVL( a.monto_neto, 0 ),
          NVL( a.monto_sin_iva, 0 ),
          NVL( a.impuesto_retenido, 0 ),
          a.tipo_aportacion,
          a.curp
   FROM   int_det_voluntaria a
   WHERE  a.folio = v_folio_cod
   AND    a.tipo_movimiento = "A"

   FOREACH cur1 INTO reg.*
      IF reg.tipo_movimiento = "A" THEN
         CASE reg.tipo_aportacion
            WHEN "V"           #-- VOLUNTARIA --#
               LET estado = 1
            WHEN "C"           #-- COMPLEMENTARIA --#
               LET estado = 1
            WHEN "L"           #-- LARGO PLAZO --#
               LET estado = 1
            WHEN "I"           #-- INDEPENDIENTE --#
               LET estado = 1 
            WHEN "R"           #-- REINTEGRO --#
               LET estado = 1
            OTHERWISE
               LET estado = 7  #-- APORTACION INCORRECTA --#
         END CASE
      END IF

      IF reg.tipo_movimiento = "S" THEN
         CASE reg.tipo_aportacion
            WHEN "A"
               LET estado = 1
            OTHERWISE
               LET estado = 7 #-- APORTACION INCORRECTA --#
         END CASE
      END IF

      IF reg.monto_neto          = 0 AND
         reg.monto_sin_iva       = 0 AND
         reg.impuesto_retenido   = 0 THEN

            UPDATE int_det_voluntaria
            SET    int_det_voluntaria.resul_operacion = "02",
                   int_det_voluntaria.estado   = 6, #-- MONTO DEL APORTE EN CEROS --#
                   int_det_voluntaria.medio    = reg.tipo_aportacion,
                   int_det_voluntaria.usuario  = USER
            WHERE  int_det_voluntaria.nss      = reg.nss
            AND    int_det_voluntaria.rowid    = reg.numero
            CONTINUE FOREACH
      END IF

      LET reg1.monto_neto          = reg.monto_neto /100
      LET reg1.monto_sin_iva       = reg.monto_sin_iva /100
      LET reg1.impuesto_retenido   = reg.impuesto_retenido /100

      UPDATE int_det_voluntaria
      SET    int_det_voluntaria.monto_neto          = reg1.monto_neto,
             int_det_voluntaria.monto_sin_iva       = reg1.monto_sin_iva,
             int_det_voluntaria.impuesto_retenido   = reg1.impuesto_retenido,
             int_det_voluntaria.medio               = reg.tipo_aportacion,
             int_det_voluntaria.estado              = estado,
             int_det_voluntaria.usuario             = USER
      WHERE  rowid = reg.numero

      IF estado = 7 THEN
         CONTINUE FOREACH
      END IF

      SELECT "X"
      FROM afi_mae_afiliado
      WHERE  n_seguro = reg.nss

      IF SQLCA.SQLCODE <> 0 THEN
         IF reg.nss = "11111111111" THEN
             SELECT UNIQUE a.n_seguro
             INTO   vnss
             FROM   afi_solicitud a
             WHERE  a.n_unico  = reg.curp
             AND    a.tipo_solicitud = 8

             IF SQLCA.SQLCODE <> 0 THEN
                 LET vnss = "           "

                 UPDATE int_det_voluntaria
                 SET    resul_operacion = "02",
                        estado          = 3 #-- CUENTA NO ADMINISTRADA POR LA AFORE --#
                 WHERE  rowid = reg.numero
             ELSE
                 UPDATE int_det_voluntaria
                 SET    int_det_voluntaria.nss = vnss
                 WHERE  rowid = reg.numero
             END IF
         ELSE
            UPDATE int_det_voluntaria
            SET    resul_operacion = "02",
                   estado          = 3  #-- CUENTA NO ADMINISTRADA POR LA AFORE --#
            WHERE  nss = reg.nss
            AND    rowid = reg.numero
         END IF
      ELSE
         SELECT "X"
         FROM   cta_act_marca 
         WHERE  nss = reg.nss
         AND    marca_cod IN(150,221,222)
         GROUP BY 1

         IF SQLCA.SQLCODE = 0 THEN
            UPDATE int_det_voluntaria
            SET    resul_operacion = "02",
                   estado          = 4 #-- CUENTA EN PROCESO OPERATIVO --#
            WHERE  nss = reg.nss
            AND    rowid = reg.numero
         END IF
      END IF

      IF  reg.tipo_movimiento = "A" 
      AND reg.tipo_aportacion = "R" THEN  #-- REINTEGRO --#

         SELECT "c.X"
         FROM   ret_mto_devol c
         WHERE  c.nss = reg.nss
         AND    c.mto_reintegro = reg1.monto_neto
         AND    c.estado_solicitud = 50   #-- RECIBIDO --#
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            UPDATE int_det_voluntaria
            SET    int_det_voluntaria.resul_operacion = "02",
                   int_det_voluntaria.estado = 8   #-- NO EXISTE REINTEGRO --#
            WHERE  int_det_voluntaria.nss   = reg.nss
            AND    int_det_voluntaria.rowid = reg.numero
         END IF
      END IF

   END FOREACH

   SELECT count(*)
   INTO   tot_v
   FROM   int_det_voluntaria
   WHERE  folio = v_folio_cod
   AND    tipo_movimiento = "A"
   AND    tipo_aportacion = "V"
   AND    resul_operacion = "01"
   AND    estado = 1

   SELECT count(*)
   INTO   tot_c
   FROM   int_det_voluntaria
   WHERE  folio = v_folio_cod
   AND    tipo_movimiento = "A"
   AND    tipo_aportacion = "C"
   AND    resul_operacion = "01"
   AND    estado = 1

   SELECT count(*)
   INTO   tot_l
   FROM   int_det_voluntaria
   WHERE  folio = v_folio_cod
   AND    tipo_movimiento = "A"
   AND    tipo_aportacion = "L"
   AND    resul_operacion = "01"
   AND    estado = 1
   
   SELECT count(*)
   INTO   tot_i
   FROM   int_det_voluntaria
   WHERE  folio = v_folio_cod
   AND    tipo_movimiento = "A"
   AND    tipo_aportacion = "I"
   AND    resul_operacion = "01"
   AND    estado = 1   

   SELECT count(*)
   INTO   tot_as
   FROM   int_det_voluntaria
   WHERE  folio = v_folio_cod
   AND    tipo_movimiento = "S"
   AND    tipo_aportacion = "A"
   AND    resul_operacion = "01"
   AND    estado = 1      

   SELECT count(*)
   INTO   tot_reintegro
   FROM   int_det_voluntaria
   WHERE  folio = v_folio_cod
   AND    tipo_movimiento = "A"
   AND    tipo_aportacion = "R"
   AND    resul_operacion = "01"
   AND    estado = 1   

   SELECT count(*)
   INTO   tot_r
   FROM   int_det_voluntaria
   WHERE  folio = v_folio_cod
   AND    tipo_movimiento = "R"
   AND    resul_operacion = "01"
   AND    estado = 1      

   SELECT count(*)
   INTO   tot_z
   FROM   int_det_voluntaria
   WHERE  folio = v_folio_cod
   AND    resul_operacion = "02"      

   DISPLAY "TOTAL VOLUNTARIAS..........: ", tot_v AT 9,20
   DISPLAY "TOTAL COMPLEMENTARIAS......: ", tot_c AT 10,20
   DISPLAY "TOTAL LARGO PLAZO..........: ", tot_l AT 11,20
   DISPLAY "TOTAL INDEPENDIENTES.......: ", tot_i AT 12,20
   DISPLAY "TOTAL AHORRO SOLIDARIO.....: ", tot_as AT 13,20
   DISPLAY "TOTAL REINTEGRO............: ", tot_reintegro AT 14,20
   DISPLAY "TOTAL RETIRO...............: ", tot_r AT 15,20
   DISPLAY "TOTAL RECHAZADOS...........: ", tot_z AT 16,20

END FUNCTION


REPORT salida(vcontenido)
#------------------------

   DEFINE
      vcontenido   CHAR(1)

   OUTPUT
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT
      ON EVERY ROW
         PRINT vcontenido,
               "\\"

END REPORT


REPORT salida1(v_folio_cod,
               vtipo_reporte,
               vtipo_registro,
               vresult_operacion,
               vfecha_recepcion,
               vhora_recepcion )
#--------------------------------

   DEFINE
      vtipo_reporte       CHAR(1),
      vtipo_registro      CHAR(02),
      vresult_operacion   CHAR(02),
      vhora_recepcion     CHAR(8)

   DEFINE
      vfecha_recepcion    DATE

   DEFINE
      v_folio_cod         INTEGER


   OUTPUT
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT
      ON EVERY ROW
         IF vtipo_reporte = "C" THEN
            PRINT v_folio_cod      USING '----------',
                  vfecha_recepcion USING "MM/DD/YYYY",
                  vhora_recepcion
         END IF

         IF vtipo_reporte = "D" THEN
            PRINT v_folio_cod    USING '----------'
         END IF

         IF vtipo_reporte = "S" THEN
            PRINT v_folio_cod    USING '----------'
         END IF

END REPORT


FUNCTION  reverso()
#------------------

   DEFINE
      HOY       DATE

   DEFINE
      v_folio   INTEGER

   DEFINE
      ban       SMALLINT 

   LET HOY = TODAY
   LET ban = 0

   OPEN WINDOW v3 AT 5,3 WITH FORM "INTB03032" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " INTB0303                    REVERSO DE CARGA                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY  USING"DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

   INPUT BY NAME v_folio WITHOUT DEFAULTS
      AFTER FIELD v_folio
         IF v_folio IS NULL
         OR v_folio = 0 THEN
            ERROR " FOLIO INCORRECTO "
            NEXT FIELD v_folio
         END  IF

         SELECT "a.x"
         FROM   int_archivo_vol a
         WHERE  a.folio = v_folio

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE INFORMACION DEL FOLIO "
            NEXT FIELD v_folio
         END IF


      ON KEY (ESC)
         IF v_folio IS NULL
         OR v_folio = 0 THEN
            ERROR " FOLIO INCORRECTO "
            NEXT FIELD v_folio
         END IF

         SELECT "a.x"
         FROM   int_archivo_vol a
         WHERE  a.folio = v_folio

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE INFORMACION DEL FOLIO "
            NEXT FIELD v_folio
         END IF

         SELECT "a.X"
         FROM   int_det_voluntaria a
         WHERE  a.folio = v_folio
         AND    a.estado = 2

         IF SQLCA.SQLCODE = 0 THEN
            ERROR " FOLIO YA LIQUIDADO "
            NEXT FIELD v_folio
         END IF

         EXIT INPUT

      ON KEY ( INTERRUPT, CONTROL-C )
         LET ban = 1
         EXIT INPUT
   END INPUT

   IF ban = 1 THEN
      PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
      FOR enter
      CLOSE WINDOW v3
      RETURN
   END IF

   PROMPT " DESEA REVERSAR LA CARGA S/N ? "
   FOR enter

   IF enter MATCHES "[Ss]" THEN

      DISPLAY " REVERSANDO DATOS " AT 17,1 ATTRIBUTE(REVERSE)
      SLEEP 3

      DELETE
      FROM   int_cza_voluntaria
      WHERE  folio = v_folio
{svera
      SELECT COUNT(*)
      INTO ban
      FROM int_det_voluntaria
      WHERE  folio = v_folio
}
      DELETE
      FROM   int_det_voluntaria
      WHERE  folio = v_folio

      DELETE
      FROM   int_sum_voluntaria
      WHERE  folio = v_folio

      DELETE
      FROM   int_archivo_vol
      WHERE  folio = v_folio
   ELSE
      PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
      FOR enter
      LET ban = 1
   END IF

   IF ban = 1 THEN
     CLOSE WINDOW v3
     RETURN
   END IF

--sveraDISPLAY "Detalle Reverso : ", ban USING "#####&" AT 18,1

   PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR enter
   CLOSE WINDOW v3
END FUNCTION


FUNCTION  consulta_a()
#---------------------

   DEFINE vol RECORD
      nss                CHAR(11),
      nombre             CHAR(40),
      paterno            CHAR(40),
      materno            CHAR(40),
      tipo_movimiento    CHAR(1),
      tipo_aportacion    CHAR(01),
      monto_neto         DECIMAL(15,2),
      estado             SMALLINT
   END RECORD

   DEFINE arreglo ARRAY[500] OF RECORD
      apunta             CHAR(1),
      nss                CHAR(11),
      monto_neto         DECIMAL(15,2),
      nombre             CHAR(40),
      tipo_movimiento    CHAR(01),
      tipo_aportacion    CHAR(01),
      liquidado          CHAR(02)
   END RECORD

   DEFINE desc_subcta ARRAY[500] OF CHAR(40)

   DEFINE
      liqui              CHAR(02),
      aporta             CHAR(03),
      nombre             CHAR(30)
                         
   DEFINE                
      HOY                DATE
                         
   DEFINE                
      v_folio            INTEGER
                         
   DEFINE                
      ban                ,
      i                  ,
      pos                SMALLINT


   LET HOY = TODAY
   LET ban = 0

   OPEN WINDOW v4 AT 5,3 WITH FORM "INTB03033" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " INTB0303                   REGISTROS ACEPTADOS                             " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY  USING"DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

   INPUT BY NAME v_folio WITHOUT DEFAULTS
      AFTER FIELD v_folio
         IF v_folio IS NULL
         OR v_folio = 0 THEN
            ERROR " FOLIO INCORRECTO "
            NEXT FIELD v_folio
         END  IF

      ON KEY (ESC)
         IF v_folio IS NULL
         OR v_folio = 0 THEN
            ERROR " FOLIO INCORRECTO "
            NEXT FIELD v_folio
         END IF

         SELECT "a.x"
         FROM   int_det_voluntaria a
         WHERE  a.folio = v_folio
         AND    a.estado IN(1,2)
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE INFORMACION DEL FOLIO "
            NEXT FIELD v_folio
         END IF

         EXIT INPUT

      ON KEY (INTERRUPT,CONTROL-C)
         PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
         FOR enter
         LET ban = 1
         EXIT INPUT
   END INPUT

   IF ban = 1 THEN
      CLOSE WINDOW v4
      RETURN
   END IF

   DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)
   SLEEP 3

   LET i  = 1

   DECLARE cur_apt CURSOR FOR
   SQL
      SELECT a.nss,
             a.nombre,
             a.paterno,
             a.materno,
             a.tipo_movimiento,
             a.tipo_aportacion,
             a.monto_neto,
             a.estado,
             NVL( subct_desc, 'SUBCTA. NO EXISTE EN tab_subcuenta: ' || subcuenta )
      FROM   int_det_voluntaria a, OUTER tab_subcuenta b
      WHERE  a.folio = $v_folio
      AND    a.estado IN ( 1, 2 )
      AND    a.resul_operacion = "01"
      AND    a.subcuenta = b.subct_cod
   END SQL

   FOREACH cur_apt INTO vol.*, desc_subcta[i]
      CASE vol.estado
         WHEN 1
            LET liqui = "NO"
         WHEN 2
            LET liqui = "SI"
      END CASE

      LET nombre = vol.nombre CLIPPED, " ",
                    vol.paterno CLIPPED, " ",
                    vol.materno CLIPPED
      LET arreglo[i].apunta          = ' ';
      LET arreglo[i].nss             = vol.nss
      LET arreglo[i].monto_neto      = vol.monto_neto
      LET arreglo[i].nombre          = nombre
      LET arreglo[i].tipo_movimiento = vol.tipo_movimiento
      LET arreglo[i].tipo_aportacion = vol.tipo_aportacion
      LET arreglo[i].liquidado       = liqui

      LET i = i + 1
   END FOREACH

   IF( ( i - 1 ) = 0 ) THEN
      ERROR " NO EXISTE INFORMACION DEL FOLIO "
      SLEEP 3
      CLOSE WINDOW v4
      RETURN
   END IF

   ERROR ""
   CALL SET_COUNT( i - 1 )

   INPUT ARRAY arreglo WITHOUT DEFAULTS FROM scr_liquida.*
      BEFORE FIELD apunta

         LET pos = ARR_CURR()

         DISPLAY desc_subcta[pos] TO desc_subcta ATTRIBUTE (REVERSE)

      AFTER FIELD apunta
         DISPLAY '' TO desc_subcta

      ON KEY( INTERRUPT, CONTROL-C )
         EXIT INPUT
   END INPUT

   CLOSE WINDOW v4
END FUNCTION


FUNCTION  consulta_b()
#---------------------

   DEFINE vol RECORD
      nss         CHAR(11),
      nombre      CHAR(40),
      paterno     CHAR(40),
      materno     CHAR(40),
      tipo_movi   CHAR(01),
      tipo_apor   CHAR(01),
      estado      SMALLINT
   END RECORD

   DEFINE arr_glo ARRAY[500] OF RECORD
      apunta      CHAR(1),
      nss         CHAR(11),
      nombre      CHAR(40),
      tipo_movi   CHAR(01),
      tipo_apor   CHAR(01),
      motivo      CHAR(30)
   END RECORD

   DEFINE desc_subcta ARRAY[500] OF CHAR(40)

   DEFINE
      motivo_1    CHAR(30),
      nombre      CHAR(30)

   DEFINE
      HOY         DATE

   DEFINE
      v_folio     INTEGER

   DEFINE
      ban         ,
      i           ,
      pos         SMALLINT


   LET HOY = TODAY
   LET ban = 0

   OPEN WINDOW v5 AT 5,3 WITH FORM "INTB03034" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " INTB0303                   REGISTROS RECHAZADOS                             " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY  USING"DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

   INPUT BY NAME v_folio WITHOUT DEFAULTS
      AFTER FIELD v_folio
         IF v_folio IS NULL
         OR v_folio = 0 THEN
            ERROR " FOLIO INCORRECTO "
            NEXT FIELD v_folio
         END  IF

      ON KEY (ESC)
         IF v_folio IS NULL
         OR v_folio = 0 THEN
            ERROR " FOLIO INCORRECTO "
            NEXT FIELD v_folio
         END IF

         SELECT "a.x"
         FROM   int_det_voluntaria a
         WHERE  a.folio = v_folio
         AND    a.estado NOT IN(1,2)
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE INFORMACION DEL FOLIO "
            NEXT FIELD v_folio
         END IF

         EXIT INPUT

      ON KEY (INTERRUPT,CONTROL-C)
         PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
         FOR enter
         LET ban = 1
         EXIT INPUT
   END INPUT

   IF ban = 1 THEN
      CLOSE WINDOW v4
      RETURN
   END IF

   DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)
   SLEEP 3

   LET i  = 1

   DECLARE cur_apt_r CURSOR FOR
   SQL
      SELECT a.nss,
             a.nombre,
             a.paterno,
             a.materno,
             a.tipo_movimiento,
             a.tipo_aportacion,
             a.estado,
             NVL( subct_desc, 'SUBCTA. NO EXISTE EN tab_subcuenta: ' || subcuenta )
      FROM   int_det_voluntaria a, OUTER tab_subcuenta b 
      WHERE  a.folio = $v_folio
      AND    a.estado NOT IN( 1, 2 )
      AND    a.resul_operacion = "02" 
      AND    a.subcuenta = b.subct_cod
   END SQL

   FOREACH cur_apt_r INTO vol.*, desc_subcta[i]
      CASE vol.estado
         WHEN 3
            LET motivo_1 = "NO REGISTRADO EN LA AFORE"
         WHEN 4
            LET motivo_1 = "CUENTA EN PROCESO OPERATIVO"
         WHEN 6
            LET motivo_1 = "MONTO EN CEROS O NULO"
         WHEN 7
            LET motivo_1 = "TIPO APORTACION INCORRECTA"
         WHEN 8
            LET motivo_1 = "NO EXISTE REINTEGRO"
      END CASE

      LET nombre = vol.nombre CLIPPED, " ",
                   vol.paterno CLIPPED, " ",
                   vol.materno CLIPPED
      LET arr_glo[i].apunta          = ' ';
      LET arr_glo[i].nss             = vol.nss
      LET arr_glo[i].nombre          = nombre
      LET arr_glo[i].tipo_movi       = vol.tipo_movi
      LET arr_glo[i].tipo_apor       = vol.tipo_apor
      LET arr_glo[i].motivo          = motivo_1

      LET i = i + 1
   END FOREACH

   IF( ( i - 1 ) = 0 ) THEN
      ERROR " NO EXISTE INFORMACION DEL FOLIO "
      SLEEP 3
      CLOSE WINDOW v5
      RETURN
   END IF

   ERROR ""
   CALL SET_COUNT( i - 1 )

   INPUT ARRAY arr_glo WITHOUT DEFAULTS FROM scr_rechazo.*
      BEFORE FIELD apunta

         LET pos = ARR_CURR()

         DISPLAY desc_subcta[pos] TO desc_subcta ATTRIBUTE ( REVERSE )

      AFTER FIELD apunta
         DISPLAY '' TO desc_subcta

      ON KEY( INTERRUPT, CONTROL-C )
         EXIT INPUT
   END INPUT

   CLOSE WINDOW v5
END FUNCTION
