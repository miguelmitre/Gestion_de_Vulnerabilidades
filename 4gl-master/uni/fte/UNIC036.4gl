################################################################################
#Owner             => E.F.P.
#Programa RETC028  => PROCESO DE CARGA DE RESPUESTA DE OP. 73
#Por               => MIGUEL ANGEL HERNANDEZ MARINEZ.
#fecha actualiza   => 25 de septiembre de 2003
#Sistema           => UNI
################################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_bat  RECORD LIKE dis_ctrl_proceso.*

   DEFINE vhora_max          CHAR(08),
          vconsecutivo         INTEGER,
          vhora_final        CHAR(08),
          vresultado         CHAR(50),
          vetapa_cod         SMALLINT,
          vproc              CHAR(06),
          vrech              CHAR(06),
          vpend              CHAR(06),
          hoy                DATE,
          generar            CHAR(15),
          gusuario           CHAR(08),
          vnom_archivo           CHAR(21),
          vreporte               CHAR(200),
          vcont_rech             INTEGER,
          vcont_acep             INTEGER,
          vcont_pend             INTEGER,
          vfecha_lote            CHAR(08),
          vtipo_reporte          CHAR(01),
          hora_inicial           CHAR(08),
          hora_final             CHAR(08),
          cla_sel                CHAR(500)

   DEFINE reg RECORD
          folio                  integer,
          tipo_registro          char(2),
          consecutivo_reg        integer,
          nss_cta1               char(11),
          nss_uni                char(11),
          cod_resul_operacion    char(2),
          diagnostico_nss_cta1   char(2),
          diagnostico_nss_uni    char(2),
          proceso_nss_cta1_afi   char(2),
          proceso_nss_cta1_tra   char(2),
          proceso_nss_cta1_ret   char(2),
          proceso_nss_cta1_exc   char(2),
          proceso_nss_uni_afi    char(2),
          proceso_nss_uni_tra    char(2),
          proceso_nss_uni_ret    char(2),
          proceso_nss_uni_exc    char(2),
          numero_nss_familia     char(2),
          nss_uni_armado         char(11),
          estado                 smallint,
          result_operacion       char(2),
          diag_operacion         char(2)
   END RECORD

   DEFINE vfolio                 INTEGER

   DEFINE gparam_uni  RECORD LIKE seg_modulo.*

   DEFINE opc                    CHAR(1)

   DEFINE log1                   CHAR(40)

END GLOBALS
#*********************************************************************
MAIN

   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   SELECT *,
          USER
   INTO   gparam_uni.*,
          gusuario
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   LET log1 = "/tmp/",gusuario CLIPPED,".UNIC036.log"

   #CALL STARTLOG(log1)
   CALL STARTLOG("UNIC036.log")

   LET hoy = TODAY

   CALL Proceso()

END MAIN
#*********************************************************************
FUNCTION Inicializa()

   LET hoy = TODAY

   LET vnom_archivo = generar[1,8] CLIPPED,".EXCESO" CLIPPED

   LET vcont_acep = 0
   LET vcont_rech = 0

END FUNCTION
#*********************************************************************
FUNCTION Proceso()

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod = 'UNIC028' ",
                 " AND    etapa_cod = 1 " CLIPPED

   PREPARE claexe FROM cla_sel

   DECLARE cur_proceso CURSOR FOR claexe

   OPEN cur_proceso
      FETCH cur_proceso INTO vconsecutivo
   CLOSE cur_proceso

   LET cla_sel = " SELECT * ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod = 'UNIC028' ",
                 " AND    etapa_cod = 1 ",        -- ETAPA 1
                 " AND    consecutivo = ",vconsecutivo CLIPPED
   PREPARE claexe2 FROM cla_sel

   DECLARE cur_proceso2 CURSOR FOR claexe2

   OPEN cur_proceso2
      FETCH cur_proceso2 INTO g_bat.*
   CLOSE cur_proceso2

   LET generar = g_bat.parametro1 CLIPPED

   IF generar IS NOT NULL THEN
      CALL Proceso_principal()
           RETURNING vfolio

      LET vhora_final = TIME
      LET vproc       = vcont_acep
      LET vrech       = vcont_rech

      IF vfolio = 0 THEN
         LET vresultado = "ESTE ARCHIVO YA HA SIDO PROCESADO"
      ELSE 
         LET vresultado = "ACEPTADOS: " CLIPPED,vproc CLIPPED,"  RECHAZADOS: " CLIPPED,vrech CLIPPED
      END IF

      CALL Actualiza_etapa(vfolio,1,vresultado)
   END IF

   ERROR "PROCESO TERMINADO "

END FUNCTION
#*********************************************************************
FUNCTION Proceso_principal()

   DEFINE ejecuta    CHAR(200),
          vfolio     INTEGER

   CALL Inicializa()

   ERROR "PROCESANDO INFORMACION..."

   CALL Ingresa_etapa(vfolio,2,"Inicia separa archivo de respuesta unificacion") 
      CALL Separa_archivo() -- ETAPA 2
   CALL Actualiza_etapa(vfolio,2,"Termina separa archivo de respuesta unificacion")

   CALL Ingresa_etapa(vfolio,3,"Inicia carga historicos de respuesta unificacion")
      CALL Sube_datos()
           RETURNING vfolio  -- ETAPA 3

         CALL Actualiza_etapa_1(vfolio,1)
         CALL Actualiza_etapa_1(vfolio,2)
   CALL Actualiza_etapa(vfolio,3,"Termina carga historicos de respuesta unificacion")

   CALL Ingresa_etapa(vfolio,4,"Inicia validacion de respuesta unificacion")
      CALL validacion(vfolio) -- ETAPA 4
   CALL Actualiza_etapa(vfolio,4,"Termina validacion de respuesta unificacion")

   RETURN vfolio

END FUNCTION
#*********************************************************************
FUNCTION Ingresa_etapa(vfolio,vetapa_cod,vresultado)

   DEFINE vfolio         INTEGER,
          vetapa_cod     DECIMAL(2,0),
          vresultado     CHAR(50)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES 
      (TODAY,             -- fecha_proceso
       "UNIC028",         -- proceso_cod
       vetapa_cod,        -- etapa_cod   
       hora_inicial,      -- hora_inicial
       hora_final,        -- hora_final
       g_bat.parametro1,  -- parametro1
       NULL,              -- parametro2
       NULL,              -- parametro3
       NULL,              -- parametro4
       NULL,              -- parametro5
       vfolio,            -- folio 
       vresultado,        -- resultado
       gusuario,          -- usuario
       0                  -- consecutivo
      )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso etapa ",
            vetapa_cod," ",STATUS
      EXIT PROGRAM
   END IF

END FUNCTION
#*********************************************************************
FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vresultado)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          vresultado   CHAR(50),
          hoy          DATE

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod = 'UNIC028' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe3 FROM cla_sel

   DECLARE cur_proceso3 CURSOR FOR claexe3

   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vconsecutivo
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    hora_final = ","'",vhora_final,"',",
                 "        folio      = ",vfolio,",",
                 "        resultado  = ","'",vresultado,"'",
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod   = 'UNIC028'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED
   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

END FUNCTION
#*********************************************************************
FUNCTION Actualiza_etapa_1(vfolio,vetapa_cod)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          hoy          DATE

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod = 'UNIC028' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe5 FROM cla_sel

   DECLARE cur_proceso5 CURSOR FOR claexe5

   OPEN cur_proceso5
      FETCH cur_proceso5 INTO vconsecutivo
   CLOSE cur_proceso5

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    folio      = ",vfolio,
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod   = 'UNIC028'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo  = ",vconsecutivo CLIPPED

   PREPARE claexe6 FROM cla_sel

   EXECUTE claexe6

END FUNCTION
#*********************************************************************
FUNCTION Separa_archivo()  -- ETAPA 2 

   DEFINE vfolio         INTEGER,
          ejecuta        CHAR(200)

   ERROR "SEPARANDO ARCHIVO,ETAPA 2"

   LET ejecuta = "sed -e '/^01/!d' ",gparam_uni.ruta_rescate CLIPPED,"/",
                generar CLIPPED," >",gparam_uni.ruta_rescate CLIPPED,"/uni_cza"
   RUN ejecuta

   LET ejecuta = "sed -e '/^02/!d' ",gparam_uni.ruta_rescate CLIPPED,"/",
                generar CLIPPED," >",gparam_uni.ruta_rescate CLIPPED,"/uni_det"
   RUN ejecuta

   LET ejecuta = "sed -e '/^09/!d' ",gparam_uni.ruta_rescate CLIPPED,"/",
                generar CLIPPED," >",gparam_uni.ruta_rescate CLIPPED,"/uni_sum"
   RUN ejecuta

   ERROR "SEPARACION DE ARCHIVO TERMINADO,ETAPA 2"

END FUNCTION
#*********************************************************************
FUNCTION Sube_datos()  -- ETAPA 3

   DEFINE ejecuta          CHAR(200),
          vlote            CHAR(03),
          vfolio           INTEGER,
          vfecha_recepcion DATE,
          vhora_recepcion  CHAR(10),
          vestado          SMALLINT,
          vfecha_estado    DATE,
          vhora_estado     CHAR(10),
          i                INTEGER,
          cfecha_envio     CHAR(10),
          vfecha_envio     CHAR(08),
          dvfecha_lote     CHAR(10),
          vfecha_aux       DATE,
          vcontenido       CHAR(1)

   DEFINE vid_pago         CHAR(16),
          posicion1        CHAR(1),
          posicion2        CHAR(1),
          error_id         CHAR(50)

   ERROR "SUBE DATOS A TABLAS HISTORICAS,ETAPA 3"

   LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                 "; cut -c 15-22 uni_cza > uni_fecha_lote"
   RUN ejecuta

   LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                 "; cut -c 23-30 uni_cza > uni_lote"
   RUN ejecuta

   WHENEVER ERROR CONTINUE
      DROP TABLE fecha_lote
      DROP TABLE lote
   WHENEVER ERROR STOP

   CREATE TEMP TABLE fecha_lote
      (fecha_lote CHAR(08))

   CREATE TEMP TABLE lote
      (lote INTEGER)

   LET ejecuta = gparam_uni.ruta_rescate CLIPPED,
                 "/uni_fecha_lote" CLIPPED

   LOAD FROM ejecuta INSERT INTO fecha_lote

   LET ejecuta = gparam_uni.ruta_rescate CLIPPED,
                 "/uni_lote" CLIPPED

   LOAD FROM ejecuta INSERT INTO lote

   SELECT fecha_lote
   INTO   vfecha_lote
   FROM   fecha_lote

   SELECT lote
   INTO   vlote
   FROM   lote

   LET dvfecha_lote = vfecha_lote [5,6],"/",
                      vfecha_lote [7,8],"/",
                      vfecha_lote [1,4]

   LET vfecha_aux = dvfecha_lote

   SELECT "X"
   FROM   uni_cza_solicitud
   WHERE  fecha_transferencia = vfecha_aux
   AND    consecutivo_dia  = vlote

   IF SQLCA.SQLCODE <> 0 THEN

      INSERT INTO uni_folio_trabajo
      VALUES(0)

      SELECT MAX(folio)
      INTO   vfolio
      FROM   uni_folio_trabajo

      LET vfecha_recepcion = TODAY
      LET vhora_recepcion  = TIME

      --------------------   GENERA uni_cza_solicitud   --------------------

      LET vreporte = gparam_uni.ruta_rescate CLIPPED,"/vfolio_cza"

      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"   ---- Cabeza

         OUTPUT TO REPORT salida(vfolio,
                                 vfecha_recepcion,
                                 vhora_recepcion
                                 )
      FINISH REPORT salida

      LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                    "; paste vfolio_cza uni_cza > cza1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' cza1 > uni_cza "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_uni.ruta_exp CLIPPED,
                    "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_uni_cza -l /tmp/",
                    gusuario CLIPPED,".dbload_uni_cza.log -e 1 -k;"
      RUN ejecuta

      --------------------   GENERA adi   --------------------

      LET vreporte = gparam_uni.ruta_rescate CLIPPED,"/adi"

      START REPORT salida2 TO vreporte
         LET vcontenido = "a"

         OUTPUT TO REPORT salida2(vcontenido)
      FINISH REPORT salida2

      --------------------   GENERA uni_det_solicitud   --------------------

      LET vreporte = gparam_uni.ruta_rescate CLIPPED,"/vfolio_cza1"

      START REPORT salida1 TO vreporte
         LET vtipo_reporte = "D"   ---- Detalle

         OUTPUT TO REPORT salida1(vfolio)

      FINISH REPORT salida1

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_det

      LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza1 > vfolio_det" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det2 con el numero de registros igual al
      -- num. registros que tiene el detalle

      LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                    "; sed -f vfolio_det uni_det > vfolio_det2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det borrando lineas que no sirven

      LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_det2 > vfolio_det" CLIPPED 
      RUN ejecuta

      -- Se crea det1 pegando datos genrales con el detalle 

      LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                    "; paste vfolio_det uni_det > det1" CLIPPED
      RUN ejecuta

      -- Se crea det eliminando espacio en blanco que se genera cuando
      -- se pegan los 2 archivos

      LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' det1 > uni_det "
      RUN ejecuta

      -- Se suben los datos del detalle

      LET ejecuta = "cd ",gparam_uni.ruta_exp CLIPPED,
                    "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_uni_det -l /tmp/",
                    gusuario CLIPPED,".dbload_uni_det.log -e 1 -k;"
      RUN ejecuta

      --------------------   GENERA uni_sum_solicitud   --------------------

      LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                    "; paste vfolio_cza1 uni_sum > sum1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' sum1 > uni_sum "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_uni.ruta_exp CLIPPED,
                    "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_uni_sum -l /tmp/",
                    gusuario CLIPPED,".dbload_uni_sum.log -e 1 -k;"
      RUN ejecuta

      -- Se borran todo los archivo auxiliares

      LET ejecuta = "cd ",gparam_uni.ruta_rescate CLIPPED,
                    "; rm archivos uni_cza uni_det uni_sum uni_fecha_lote uni_lote vfolio_cza vfolio_cza1 vfolio_det vfolio_det2 cza1 det1 dep1 sum1 adi"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_uni.ruta_exp CLIPPED,
                    "/;DBDATE=mdy4;export DBDATE;"

      RUN ejecuta

      RETURN vfolio
   ELSE
      ERROR "ESTE ARCHIVO YA HA SIDO PROCESADO"
      RETURN 0
   END IF

   ERROR "SUBE DATOS A TABLAS HISTORICAS TERMINADO,ETAPA 3"

END FUNCTION
#*********************************************************************
FUNCTION validacion(vfolio)

   DEFINE vfolio     INTEGER

   DECLARE cursor_uni CURSOR FOR
   SELECT *
   FROM   uni_det_solicitud
   WHERE  folio = vfolio

   FOREACH cursor_uni INTO reg.*

      SELECT "X"
      FROM   uni_solicitud
      WHERE  nss_cta1 = reg.nss_cta1
      AND    nss_uni  = reg.nss_uni

      IF SQLCA.SQLCODE = 0 THEN
         UPDATE uni_solicitud
         SET    folio = vfolio,
                consecutivo_reg = reg.consecutivo_reg,
                estado = 30
         WHERE  nss_cta1 = reg.nss_cta1
         AND    nss_uni  = reg.nss_uni
         AND    estado = 20
      ELSE
         UPDATE uni_det_solicitud
         SET    diag_operacion = "20"
         WHERE  folio = vfolio
         AND    nss_cta1 = reg.nss_cta1
         AND    nss_uni  = reg.nss_uni
      END IF

      IF reg.cod_resul_operacion = "00" THEN

         CASE reg.diagnostico_nss_cta1
            WHEN "00"
               LET reg.result_operacion = "01"
            WHEN "02"
               LET reg.result_operacion = "02"
            WHEN "03"
               LET reg.result_operacion = "02"
            #WHEN "04"
            #   LET reg.result_operacion = "02"
            WHEN "05"
               LET reg.result_operacion = "02"
            WHEN "07"
               LET reg.result_operacion = "02"
            WHEN "08"
               LET reg.result_operacion = "02"
            WHEN "09"
               LET reg.result_operacion = "02"
            WHEN "10"
               LET reg.result_operacion = "02"
            WHEN "11"
               LET reg.result_operacion = "02"
            WHEN "12"
               LET reg.result_operacion = "02"
            WHEN "13"
               LET reg.result_operacion = "02"
            WHEN "14"
               LET reg.result_operacion = "02"

            OTHERWISE
               LET reg.result_operacion = "03"
         END CASE

         IF reg.result_operacion <> "03" THEN
            CASE reg.diagnostico_nss_uni
               WHEN "00"
                  LET reg.result_operacion = "01"
               WHEN "02"
                  LET reg.result_operacion = "02"
               WHEN "03"
                  LET reg.result_operacion = "02"
               #WHEN "04"
               #   LET reg.result_operacion = "02"
               WHEN "05"
                  LET reg.result_operacion = "02"
               WHEN "07"
                  LET reg.result_operacion = "02"
               WHEN "08"
                  LET reg.result_operacion = "02"
               WHEN "09"
                  LET reg.result_operacion = "02"
               WHEN "10"
                  LET reg.result_operacion = "02"
               WHEN "11"
                  LET reg.result_operacion = "02"
               WHEN "12"
                  LET reg.result_operacion = "02"
               WHEN "13"
                  LET reg.result_operacion = "02"
               WHEN "14"
                  LET reg.result_operacion = "02"

               OTHERWISE
                  LET reg.result_operacion = "03"
            END CASE
         END IF
      ELSE
         CASE reg.diagnostico_nss_cta1
            WHEN "06"
               LET reg.result_operacion = "02"
            OTHERWISE
               LET reg.result_operacion = "03"
         END CASE

         IF reg.result_operacion <> "03" THEN
            CASE reg.diagnostico_nss_uni
               WHEN "06"
                  LET reg.result_operacion = "02"

               OTHERWISE
                  LET reg.result_operacion = "03"
            END CASE
         END IF
      END IF

      IF reg.diagnostico_nss_cta1 = "04" THEN
         CASE
            WHEN reg.proceso_nss_cta1_afi = "01"
               LET reg.result_operacion = "02"
            WHEN reg.proceso_nss_cta1_tra = "01"
               LET reg.result_operacion = "02"
            WHEN reg.proceso_nss_cta1_ret = "01"
               LET reg.result_operacion = "02"
            WHEN reg.proceso_nss_cta1_exc = "01"
               LET reg.result_operacion = "02"
            OTHERWISE
               LET reg.result_operacion = "03"
         END CASE
      END IF

      IF reg.result_operacion <> "03" THEN
         IF reg.diagnostico_nss_uni = "04" THEN
            CASE
               WHEN reg.proceso_nss_uni_afi = "01"
                  LET reg.result_operacion = "02"
               WHEN reg.proceso_nss_uni_tra = "01"
                  LET reg.result_operacion = "02"
               WHEN reg.proceso_nss_uni_ret = "01"
                  LET reg.result_operacion = "02"
               WHEN reg.proceso_nss_uni_exc = "01"
                  LET reg.result_operacion = "02"
               OTHERWISE
                  LET reg.result_operacion = "03"
            END CASE
         END IF
      END IF

      UPDATE uni_det_solicitud
      SET    result_operacion = reg.result_operacion
      WHERE  folio = vfolio
      AND    nss_cta1 = reg.nss_cta1
      AND    nss_uni  = reg.nss_uni

   END FOREACH
END FUNCTION
#*********************************************************************
REPORT salida(vfolio,
              vfecha_recepcion,
              vhora_recepcion
              )

   DEFINE vfolio           INTEGER,
          vfecha_recepcion DATE,
          vhora_recepcion  CHAR(08),
          vestado          CHAR(01),
          vfecha_estado    DATE,
          vhora_estado     CHAR(08)

   OUTPUT
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT
      ON EVERY ROW
         IF vtipo_reporte = "C" THEN
            PRINT vfolio           USING '----------',
                  vfecha_recepcion USING "YYYY/MM/DD",
                  vhora_recepcion

         ELSE
            PRINT vfolio           USING '----------'
         END IF
END REPORT
#*********************************************************************
REPORT salida1(vfolio)

   DEFINE vfolio           INTEGER,
          vestado          CHAR(01),
          vfecha_estado    DATE,
          vhora_estado     CHAR(08)

   OUTPUT
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT
      ON EVERY ROW
         IF vtipo_reporte = "D" THEN
            PRINT vfolio        USING '----------'

         ELSE
            PRINT vfolio        USING '----------'
         END IF
END REPORT
#*********************************************************************
REPORT salida2(vcontenido)

   DEFINE vcontenido    CHAR(1)

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
#*********************************************************************

