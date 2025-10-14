###############################################################################
#Proyecto          => SAFRE                                                   #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => DIS                                                     #
#Programa          => DISB2001P                                               #
#Descripcion       => GENERA HISTORICOS (dis_det_bono, dis_dep_bono,          #
#                  => dis_cza_bono, dis_sum_bono) de BONO ISSSTE              #
#                  => GENERA ARCHIVO CONFAF                                   #
#Fecha Inicio      => 22 octubre 2008                                         #
#Elaboro           => DMR                                                     #
#Modificaciones    => (Se agrego filtro para verificar nss que                #
#                      no son issste pero que reciben estas aportaciones)     #
#Fecha Mod         =>                                                         #
#                  =>                                                         #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      vrecauda                CHAR(001),
      vident_viv_garantia     CHAR(01)

   DEFINE
      g_bat                   RECORD LIKE dis_ctrl_proceso.*,
      gparam_dis              RECORD LIKE seg_modulo.*,
      vcod_afore              LIKE tab_afore_local.codigo_afore,
      vhora_final             CHAR(08),
      vresultado              CHAR(50),
      vetapa_cod              SMALLINT,
      vproc                   CHAR(08),
      vrech                   CHAR(08),
      hoy                     DATE,
      gusuario                CHAR(08),
      vnom_archivo            CHAR(22),
      vreporte                CHAR(200),
      vcont_rech              INTEGER,
      vcont_acep              INTEGER,
      vfecha_lote_isss        CHAR(08),
      xfecha_lote_isss        DATE,
      vtipo_reporte           CHAR(01),
      hora_inicial            CHAR(08),
      hora_final              CHAR(08),
      cla_sel                 CHAR(1000),
      cla_apo                 CHAR(500),
      cla_mon                 CHAR(1000),
      cla_com                 CHAR(500),
      cla_upd                 CHAR(1500),

      tot_acep RECORD
         importe_bono         DECIMAL(22,4),
         importe_dbmx         DECIMAL(18,2)
      END RECORD,

      tot_rech RECORD
         importe_bono         DECIMAL(22,4),
         importe_dbmx         DECIMAL(18,2)
      END RECORD

   DEFINE reg_ident RECORD
          pid                 ,
          proceso_cod         ,
          opera_cod           INTEGER ,
          nom_archivo         CHAR(20),
          tipo                SMALLINT,
          fecha_recepcion     DATE,
          val_cs              DECIMAL(16,6)      --v2
   END RECORD

   DEFINE gfecha_recepcion    DATE,
          vrow                INTEGER,
	  vperiodo            CHAR(06)

   DEFINE vnum_reg            INTEGER
   DEFINE tipo_archivo        CHAR(01)

   DEFINE vid_sie             CHAR(08)

   DEFINE vnum_ctas_xpagar    SMALLINT
   DEFINE longi               INTEGER
   DEFINE new_cadena          CHAR(20)

   DEFINE aux_cuo_soc         DECIMAL(16,6)

   DEFINE vfolio              INTEGER
END GLOBALS


MAIN
   CALL STARTLOG("DISB2001P.log")

   DISPLAY " "
   DISPLAY ".1"

   LET reg_ident.pid             = ARG_VAL(1)
   LET reg_ident.proceso_cod     = ARG_VAL(2)
   LET reg_ident.opera_cod       = ARG_VAL(3)
   LET reg_ident.nom_archivo     = ARG_VAL(4)
   LET reg_ident.tipo            = ARG_VAL(5)
   LET reg_ident.fecha_recepcion = ARG_VAL(6)
   LET reg_ident.val_cs          = ARG_VAL(7)   --valor de la cuota social

   IF reg_ident.pid <> 0 THEN
      CALL Inserta_proceso()
   END IF

   DISPLAY "PID             : ",reg_ident.pid
   DISPLAY "Proceso_cod     : ",reg_ident.proceso_cod
   DISPLAY "Opera_cod       : ",reg_ident.opera_cod
   DISPLAY "Archivo         : ",reg_ident.nom_archivo
   DISPLAY "Tipo dispersion : ",reg_ident.tipo
   DISPLAY "Fecha recepcion : ",reg_ident.fecha_recepcion

   LET hoy = TODAY

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM  dis_ctrl_proceso ",
                 "WHERE proceso_cod = 'DISB2001P' ",
                 "AND   etapa_cod = 1 "

   LET new_cadena =''
   LET longi = length(reg_ident.nom_archivo)                    --c22-11
   CALL Obtiene_nombre_arch(longi,reg_ident.nom_archivo)        --c22-11
   RETURNING new_cadena                                         --c22-11

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   SELECT *
   INTO  g_bat.*
   FROM  dis_ctrl_proceso
   WHERE proceso_cod = "DISB2001P"
   AND   etapa_cod   = 1
   AND   consecutivo = vrow

   IF reg_ident.nom_archivo IS NOT NULL THEN
      CALL Proceso_principal() RETURNING vfolio

      LET vproc = vcont_acep

      LET vrech = vnum_reg

      IF vfolio = 0 THEN
         LET vresultado = "ESTE ARCHIVO YA HA SIDO PROCESADO"
      ELSE
         LET vresultado = "ACEPTADOS: ",vproc,"  RECHAZADOS: ",vrech

         IF reg_ident.pid <> 0 THEN
            CALL actualiza_bat_f(vfolio)
         END IF
      END IF

      LET vhora_final = TIME

      LET cla_sel = "SELECT MAX(consecutivo) ",
                    "FROM dis_ctrl_proceso ",
                    "WHERE proceso_cod = 'DISB2001P' ",
                    " AND etapa_cod = 1 " CLIPPED

      PREPARE claexe99 FROM cla_sel
      DECLARE cur_proceso99 CURSOR FOR claexe99
      OPEN cur_proceso99
         FETCH cur_proceso99 INTO vrow
      CLOSE cur_proceso99

      LET cla_upd = "UPDATE dis_ctrl_proceso ",
            " SET   dis_ctrl_proceso.hora_final = ","'",vhora_final,"'",",",
            "       dis_ctrl_proceso.folio      = ",vfolio,",",
            "       dis_ctrl_proceso.resultado  = ","'",vresultado,"'",
            " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001P' ",
            " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
            " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

      PREPARE claexe11 FROM cla_upd
      EXECUTE claexe11

      LET cla_sel = "SELECT MAX(consecutivo) ",
                    "FROM dis_ctrl_proceso ",
                    "WHERE proceso_cod = 'DISB2001P' ",
                    "AND etapa_cod = 3 " CLIPPED

      PREPARE claexe91 FROM cla_sel
      DECLARE cur_proceso91 CURSOR FOR claexe91
      OPEN cur_proceso91
         FETCH cur_proceso91 INTO vrow
      CLOSE cur_proceso91

      LET cla_upd = "UPDATE dis_ctrl_proceso ",
            " SET    dis_ctrl_proceso.folio       = ",vfolio,
            " WHERE  dis_ctrl_proceso.proceso_cod = 'DISB2001P' ",
            " AND    dis_ctrl_proceso.etapa_cod   = 3 ",
            " AND    dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

      PREPARE claexe12 FROM cla_upd
      EXECUTE claexe12
   END IF
END MAIN


FUNCTION Proceso_principal()
   DEFINE
      ejecuta CHAR(200),
      vfolio  INTEGER

   CALL Inicializa()

   CALL Lectura()

   ERROR "PROCESANDO INFORMACION...",reg_ident.nom_archivo
 
   CALL Ingresa_etapa(vfolio,3,"Separa archivo")
   CALL Separa_archivo()                                  -- ETAPA 3
   CALL Actualiza_etapa(vfolio,3,"Separa archivo")

   CALL Ingresa_etapa(vfolio,4,"Carga historicos")
   CALL Sube_datos() RETURNING vfolio                     -- ETAPA 4
   CALL Actualiza_etapa(vfolio,4,"Carga historicos")

   UPDATE dis_dep_bono
   SET tipo_siefore = 0
   WHERE folio = vfolio

   IF vfolio <> 0 THEN

      --- ACTIVA OPTCOMPIND PARA QUE TODOS LOS QUERYES ENTREN POR INDICE -----

      LET ejecuta = "cd ",gparam_dis.ruta_exp CLIPPED,
                       "/;OPTCOMPIND=0;export OPTCOMPIND;"
      RUN ejecuta

      LET cla_apo = "SELECT codigo_siefore,",   --c22-6
                           "porcentaje ",       --c22-6
                    "FROM   cta_regimen ",      --c22-6
                    "WHERE  nss = ? ",          --c22-6
                    "AND    subcuenta = ? "     --c22-6


      LET cla_mon = "SELECT h.n_seguro,",
                    " h.n_unico,",
                    " h.fecha_reden,",
                    " h.fecha_reden,",
                    " h.importe_bono,",
                    " h.importe_dbmx,",
                    " h.result_operacion,",
                    " h.consec_reg_lote ",
                    " FROM   dis_det_bono h ",
                    " WHERE  h.folio    = ? ",
                    " AND    h.n_unico  = ? "

      --Obtenemos el numero de dias cotizados

      CALL Ingresa_etapa(vfolio,5,"Dispersa aportes")
      CALL Genera_aportes_nor(vfolio)                     -- ETAPA 5
      CALL Actualiza_etapa(vfolio,5,"Dispersa aportes")

      CALL Ingresa_etapa(vfolio,7,"Genera confaf")
      CALL Genera_confaf(vfolio)                          -- ETAPA 7
      CALL Actualiza_etapa(vfolio,7,"Genera confaf")

      ------ ACTIVA OPTCOMPIND DE DEFAULT ---------------
      LET ejecuta = "cd ",gparam_dis.ruta_exp CLIPPED,
                       "/;OPTCOMPIND=2;export OPTCOMPIND;"
      RUN ejecuta

      RETURN vfolio
   ELSE
      RETURN 0
   END IF
END FUNCTION


FUNCTION Inicializa()
   LET hoy = TODAY

   SELECT *,USER
   INTO   gparam_dis.*,gusuario
   FROM   seg_modulo
   WHERE  modulo_cod = "dis"

   SELECT codigo_afore
   INTO vcod_afore
   FROM tab_afore_local

   LET tot_acep.importe_bono = 0
   LET tot_acep.importe_dbmx = 0

   LET tot_rech.importe_bono = 0
   LET tot_rech.importe_dbmx = 0

   LET vcont_acep = 0
   LET vcont_rech = 0

   LET vnum_reg         = 0
   LET vnum_ctas_xpagar = 0
END FUNCTION


FUNCTION Inserta_proceso()
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB2001P",             -- proceso_cod
       1,                       -- etapa_cod     -- LECTURA
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       reg_ident.nom_archivo,   -- parametro1
       vrecauda,                -- parametro2
       reg_ident.val_cs,        -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       gusuario,                -- usuario
       0)

   IF STATUS < 0 THEN
      DISPLAY "Program stopped"
      ERROR "ERROR AL INSERTAR EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 3
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Obtiene_nombre_arch(long, cadena)
   DEFINE
      long ,i       SMALLINT,
      cadena        CHAR(800),
      cadena_limpia CHAR(800)

   LET cadena_limpia=' '

   FOR i = 1 to long
      IF NOT (cadena[i,i]='.') THEN
         LET cadena_limpia = cadena_limpia CLIPPED || cadena[i,i]
      END IF
   END FOR
   RETURN cadena_limpia
END FUNCTION


FUNCTION Ingresa_etapa(vfolio,vetapa_cod,vresultado)
   DEFINE
      vfolio       INTEGER,
      vetapa_cod   DECIMAL(2,0),
      vresultado   CHAR(50)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB2001P",             -- proceso_cod
       vetapa_cod,              -- etapa_cod
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_bat.parametro1,        -- parametro1
       NULL,                    -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       vfolio,                  -- folio
       vresultado,              -- resultado
       gusuario,                -- usuario
       0)

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso etapa ",vetapa_cod," ",STATUS
      SLEEP 3
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vresultado)
   DEFINE
      vfolio       INTEGER,
      vetapa_cod   DECIMAL(2,0),
      vresultado   CHAR(50)

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM dis_ctrl_proceso ",
                 " WHERE proceso_cod = 'DISB2001P' ",
                 " AND etapa_cod = ",vetapa_cod CLIPPED

   PREPARE claexe3 FROM cla_sel
   DECLARE cur_proceso3 CURSOR FOR claexe3
   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vrow
   CLOSE cur_proceso3

   LET vhora_final = TIME

   LET cla_upd = " UPDATE dis_ctrl_proceso ",
                 " SET  dis_ctrl_proceso.hora_final = ","'",vhora_final,"'",",",
                 "      dis_ctrl_proceso.folio      = ",vfolio,",",
	         "      dis_ctrl_proceso.resultado  = ","'",vresultado,"'",
                 " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001P' ",
                 " AND   dis_ctrl_proceso.etapa_cod   = ",vetapa_cod,
                 " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

   PREPARE claexe35 FROM cla_upd
   EXECUTE claexe35
END FUNCTION


FUNCTION Separa_archivo()  -- ETAPA 3
   DEFINE
      ejecuta CHAR(200)

  LET ejecuta = "head -n 1 ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/cza_bono"
  RUN ejecuta

  LET ejecuta = "grep '^02' ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/det_bono"
  RUN ejecuta

  LET ejecuta = "grep '^08' ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/dep_bono"
  RUN ejecuta

  LET ejecuta = "tail -1 ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/sum_bono"
  RUN ejecuta
END FUNCTION


FUNCTION Sube_datos()     -- ETAPA 4
   DEFINE
      ejecuta             CHAR(200),
      vlote               CHAR(03),
      cla_ins             CHAR(200),
      vfolio              INTEGER,
      vfecha_recepcion    DATE,
      vhora_recepcion     CHAR(10),
      vestado             SMALLINT,
      vfecha_estado       DATE,
      vhora_estado        CHAR(10),
      vparam              CHAR(10),
      i                   INTEGER,
      xfecha_archivo      CHAR(10),
      cfecha_envio        CHAR(10),
      vfecha_envio        CHAR(08)

   LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                 "; cut -c 17-24 cza_bono > fecha_lote"
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                 "; cut -c 25-27 cza_bono > lote"
   RUN ejecuta

   WHENEVER ERROR CONTINUE
      DROP TABLE fecha_lote
      DROP TABLE tmp_lote
   WHENEVER ERROR STOP

   CREATE TEMP TABLE fecha_lote
      (fecha_lote CHAR(08))

   CREATE TEMP TABLE tmp_lote
      (lote CHAR(03))

   LET ejecuta = gparam_dis.ruta_rescate CLIPPED,"/fecha_lote" CLIPPED
   LOAD FROM ejecuta INSERT INTO fecha_lote

   LET ejecuta = gparam_dis.ruta_rescate CLIPPED,"/lote" CLIPPED
   LOAD FROM ejecuta INSERT INTO tmp_lote  --c22-11

   SELECT fecha_lote
   INTO   vfecha_lote_isss
   FROM   fecha_lote

   LET xfecha_archivo = vfecha_lote_isss[5,6],"/",
                        vfecha_lote_isss[7,8],"/",
                        vfecha_lote_isss[1,4]
   LET xfecha_lote_isss = xfecha_archivo
 
   SELECT lote
   INTO   vlote
   FROM   tmp_lote

   SELECT "X"
   FROM  dis_cza_bono
   WHERE fech_creac_lote = vfecha_lote_isss
   AND   lote_del_dia    = vlote

   IF STATUS = NOTFOUND THEN
      INSERT INTO glo_folio VALUES(0)
      SELECT MAX(folio)
      INTO   vfolio
      FROM   glo_folio

      LET vfecha_recepcion = TODAY
      LET vhora_recepcion  = TIME
      LET vestado          = 2
      LET vfecha_estado    = TODAY
      LET vhora_estado     = TIME
      LET gfecha_recepcion = vfecha_recepcion


      --------------------   GENERA dis_cza_bono   --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_cza_bono"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"   ---- Cabeza
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza_bono cza_bono > cza1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' cza1 > cza_bono "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_af -c sube_cza_bono -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  cza_bono cza1"
      RUN ejecuta


      --------------------   GENERA dis_det_bono   --------------------

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_det
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza_bono > vfolio_det_bono" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_detb2 con el numero de registros igual al
      -- num. registros que tiene el detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f vfolio_det_bono det_bono > vfolio_detb2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det borrando lineas que no sirven
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_detb2 > vfolio_det_bono" CLIPPED 
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm vfolio_detb2"
      RUN ejecuta

      -- Se crea detb2 cortando pos 1-234 y detb3 cortando
      -- hasta del pos 236-450 y se pegan en el archivo det (Result Operacion)
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-234 det_bono > detb2"   --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 236-450 det_bono > detb3" --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste -d '1' detb2 detb3 > det_bono"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm detb2 detb3"
      RUN ejecuta

      -- Se crea det1 pegando datos generales con el detalle 
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_det_bono det_bono > detb1" CLIPPED
      RUN ejecuta

      -- Se crea det eliminando espacio en blanco que se genera cuando
      -- se pegan los 2 archivos
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' detb1 > det_bono "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm detb1 vfolio_det_bono"
      RUN ejecuta

      -- Se suben los datos del detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/; dbload -d safre_af -c sube_det_bono -l dbload.log;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det_bono"
      RUN ejecuta


      --------------------   GENERA dis_dep_bono  --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_dep_bono"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "D"   ---- deposito
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      LET cfecha_envio = TODAY
      LET vfecha_envio = cfecha_envio[7,10],
                         cfecha_envio[1,02],
                         cfecha_envio[4,05]

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_depb2"
      START REPORT salida2 TO vreporte
         OUTPUT TO REPORT salida2(vfecha_lote_isss,vfecha_envio)
      FINISH REPORT salida2

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_dep_bono > depb01_1 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_depb2 > depb02_1 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f depb01_1 dep_bono > depb01_2 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f depb02_1 dep_bono > depb02_2 "  CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' depb01_2 > depb01_3" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^08/d' depb02_2 > depb02_3"
      RUN ejecuta

      --  Aqui se coloca la posicion final del campo tipo_siefore del Layout 08
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-141 dep_bono > depb2; mv depb2 dep_bono " --c22
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste depb01_3 dep_bono depb02_3 > depb1;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' depb1 > dep_bono "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
"/;DBDATE=Y4MD0;export DBDATE;dbload -d safre_af -c sube_dep_bono -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  vfolio_dep_bono vfolio_depb2 depb01_1 depb02_1 depb01_2 depb02_2 depb01_3 depb02_3 dep_bono depb1"

      RUN ejecuta


      --------------------   GENERA dis_sum_bono   --------------------

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza_bono sum_bono > sumb1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' sumb1 > sum_bono "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_af -c sube_sum_bono -l dbload.log"

      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; rm  sumb1 sum_bono"
      RUN ejecuta


      ---- ACTIVA OPTCOMPIND PARA QUE TODOS LOS QUERYES ENTREN POR INDICE ----

      LET ejecuta = "cd ",gparam_dis.ruta_exp CLIPPED,
                    "/;OPTCOMPIND=0;export OPTCOMPIND;"
      RUN ejecuta

      ------ ACTIVA OPTCOMPIND DE DEFAULT ---------------

      LET ejecuta = "cd ",gparam_dis.ruta_exp CLIPPED,
                    "/;OPTCOMPIND=2;export OPTCOMPIND;"
      RUN ejecuta

      RETURN vfolio
   ELSE
      ERROR "ESTE @ ARCHIVO YA HA SIDO PROCESADO"
      SLEEP 3
      ERROR ""
      RETURN 0
   END IF
END FUNCTION


REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
              vestado,vfecha_estado,vhora_estado)
   DEFINE
      vfolio           INTEGER,
      vfecha_recepcion CHAR(10),
      vhora_recepcion  CHAR(08),
      vestado          CHAR(01),
      vfecha_estado    CHAR(10),
      vhora_estado     CHAR(08)

   OUTPUT
      TOP MARGIN    0
      LEFT MARGIN   0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         IF vtipo_reporte = "C" THEN
            PRINT vfolio           USING '----------',
                  vfecha_recepcion,
                  vhora_recepcion,
                  vestado,
                  vfecha_estado,
                  vhora_estado
         ELSE
            PRINT vfolio    USING '----------'
         END IF
END REPORT


REPORT salida2(vfecha_archivo,vfecha_envio)
   DEFINE
      vfecha_archivo CHAR(08),
      vfecha_envio   CHAR(08)

   OUTPUT
      TOP MARGIN    0
      LEFT MARGIN   0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         PRINT vfecha_archivo,vfecha_envio,"2"
END REPORT


FUNCTION Genera_aportes_nor(vfolio)    -- ETAPA 5
   DEFINE
      vfolio                   INTEGER,
      vfentcons                DATE,
      vcoduni                  CHAR(10),
      index                    CHAR(100),

      reg RECORD
         nss                   CHAR(11),
         curp                  CHAR(18),
         fecha_pago            CHAR(8),
         fecha_rviv            CHAR(8),
         importe_bono          DECIMAL(22,4),
         importe_dbmx          DECIMAL(18,2),
         result_operacion      CHAR(02),
         consec_reg_lote       INTEGER
     END RECORD,

     vimporte_bono             DECIMAL(22,4),
     vimporte_dbmx             DECIMAL(18,2),
     vtipo_subcuenta           CHAR(02),
     vsub_isss                 SMALLINT,
     vind_rechazo              SMALLINT,
     ejecuta                   CHAR(200),
     dcurp                     CHAR(18),
     dnss                      CHAR(11),
     cont_regi                 SMALLINT,
     ruta_archiv               CHAR(100)

   WHENEVER ERROR CONTINUE
   DATABASE safre_tmp

   DROP TABLE curp_validados
   CREATE TABLE curp_validados
   (
     curp CHAR(18)
   )

   WHENEVER ERROR STOP
   DATABASE safre_af

   LET ruta_archiv = gparam_dis.ruta_envio CLIPPED,"/curp_valid.txt"

   UNLOAD TO ruta_archiv
   SELECT a.n_unico
   FROM dis_det_bono a
   WHERE a.folio = vfolio

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,
                 "; sort -u curp_valid.txt > curp_valuni.txt"
   RUN ejecuta

   LET ruta_archiv = gparam_dis.ruta_envio CLIPPED,"/curp_valuni.txt"

   LOAD FROM ruta_archiv
   INSERT INTO safre_tmp:curp_validados

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,
                 "; rm curp_valid.txt curp_valuni.txt"
   RUN ejecuta

   DATABASE safre_tmp
   CREATE INDEX ind_val2 ON curp_validados(curp)
   UPDATE STATISTICS FOR TABLE curp_validados
   DATABASE safre_af

   DECLARE cur_val CURSOR FOR
   SELECT * FROM safre_tmp:curp_validados

   FOREACH cur_val INTO dcurp

      CALL buscar_cual_nss(dcurp)
      RETURNING dcurp,dnss,vtipo_subcuenta,vind_rechazo

      ------- validacion que nss exista en cta_regimen -------  --c22-6.3
      IF vind_rechazo = 0 THEN
         SELECT count(*)                                    --c22-6.3
         INTO   cont_regi                                    
         FROM   cta_regimen                                 --c22-6.3
         WHERE  nss = dnss                                  --c22-6.3

         IF cont_regi = 0 THEN                              --c22-6.3
            ERROR "NSS no existe en cta_regimen ",dnss      --c22-6.3
            LET vind_rechazo = 1                            --c22-6.3
         END IF                                             --c22-6.3
      END IF                                                --c22-6.3
      --------------------------------------------------------  --c22-6.3

      PREPARE cl_dispersa FROM cla_mon
      DECLARE cur_hist CURSOR FOR cl_dispersa
      OPEN    cur_hist USING vfolio,dcurp
      FOREACH cur_hist INTO  reg.*

         LET reg.nss               = dnss

         LET vimporte_bono         = reg.importe_bono        / 10000
         LET vimporte_dbmx         = reg.importe_dbmx        / 100

         --Motivo por el cual rechazar
         IF vind_rechazo = 1 THEN
            CALL Contador_rechazo()
            CALL Sumatoria_rechazo(vimporte_bono,
                                   vimporte_dbmx)
 
            UPDATE dis_det_bono  
            SET    result_operacion = "02"
            WHERE  folio           = vfolio
            AND    n_unico         = reg.curp
            AND    consec_reg_lote = reg.consec_reg_lote
         ELSE
            CALL Contador_aceptado()
            CALL Sumatoria_aceptado(vimporte_bono,
                                    vimporte_dbmx)


            ------------- DISPERSA APORTES EN dis_provision ------------------

            IF vimporte_bono > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimporte_bono,36,17,vfentcons,reg.consec_reg_lote,0,0,"BONO PENSION")
            END IF

            IF vimporte_dbmx > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimporte_dbmx,30,17,vfentcons,reg.consec_reg_lote,0,0,"DBMX PENSION")
            END IF

         END IF

         LET vimporte_bono         = 0
         LET vimporte_dbmx         = 0

      END FOREACH
      CLOSE cur_hist
   END FOREACH
END FUNCTION


FUNCTION Contador_rechazo()
   LET vcont_rech = vcont_rech + 1
END FUNCTION


FUNCTION Contador_aceptado()
   LET vcont_acep = vcont_acep + 1
END FUNCTION


FUNCTION Dispersa_aporte(vfolio,
                         vnss,
                         vcurp,
                         vcoduni,
                         cfecha_pago,
                         cfecha_rviv,
                         vmonto,
                         vsubcuenta,
                         vmovimiento,
                         vfentcons,
                         vconsec_reg_lote,
                         vmonto_part_viv,
                         vmonto_valor_part,
                         vn_rfc_entidad)

   DEFINE
      vfolio             INTEGER,
      vnss               CHAR(11),
      vcurp              CHAR(18),
      vcoduni            CHAR(10),
      cfecha_pago        CHAR(8),
      cfecha_rviv        CHAR(8),
      vfecha_pago        DATE,
      vfecha_rcv         DATE,
      vfecha_viv         DATE,
      vfecha_valor       DATE,
      vfecha_archivo     DATE,
      xfecha_pago        CHAR(10),
      xfecha_rcv         CHAR(10),
      xfecha_viv         CHAR(10),
      vmonto             DECIMAL(18,6),
      vsubcuenta         SMALLINT,
      vmovimiento        SMALLINT,
      vfentcons          DATE,
      vconsec_reg_lote   INTEGER,
      vmonto_part_viv    SMALLINT,
      vmonto_valor_part  SMALLINT,
      vcomision          DECIMAL(16,6),
      vcomision2         DECIMAL(16,6),
      vtipo              SMALLINT,
      vval_porcentaje    DECIMAL(16,6),
      vsiefore           SMALLINT,
      vporcentaje_sie    DECIMAL(5,2),
      vantiguedad_desde  SMALLINT,
      vantiguedad_hasta  SMALLINT,
      vdias_antiguedad   INTEGER,
      vn_rfc_entidad     CHAR(12),
      vsua               CHAR(06),
      abo RECORD LIKE    dis_provision.*

   LET vfecha_pago  = MDY (cfecha_pago[5,6],cfecha_pago[7,8],cfecha_pago[1,4])
   LET vfecha_valor = MDY (cfecha_rviv[5,6],cfecha_rviv[5,6],cfecha_pago[1,4])

   PREPARE claexe4 FROM cla_apo
   DECLARE cur_prov CURSOR FOR claexe4
   OPEN    cur_prov USING vnss, vsubcuenta
   FOREACH cur_prov INTO  vsiefore, vporcentaje_sie

      LET vmonto = vmonto * (vporcentaje_sie / 100)

      LET abo.tipo_movimiento   = vmovimiento        -- SI
      LET abo.subcuenta         = vsubcuenta         -- SI
      LET abo.folio             = vfolio             -- SI
      LET abo.consecutivo_lote  = vconsec_reg_lote
      LET abo.siefore           = vsiefore           -- SI
      LET abo.nss               = vnss               -- SI
      LET abo.curp              = vcurp              -- NO
      LET abo.folio_sua         = ""                 -- SI
      LET abo.fecha_pago        = vfecha_pago        -- NO
      LET abo.fecha_valor       = vfecha_valor       -- SI
      LET abo.fecha_conversion  = NULL               -- SI

      IF vsubcuenta = 36 THEN
         LET abo.monto_en_pesos    = vmonto_part_viv -- SI
         LET abo.monto_en_acciones = vmonto          -- SI
      ELSE
         LET abo.monto_en_pesos    = vmonto          -- SI
         LET abo.monto_en_acciones = vmonto_part_viv -- SI
      END IF

      LET abo.precio_accion     = vmonto_valor_part  -- SI
      LET abo.dias_cotizados    = 0                  -- NO
      LET abo.sucursal          = vcoduni            -- SI
      LET abo.id_aportante      = vn_rfc_entidad     -- SI
      LET abo.estado            = 5                  -- SI
      LET abo.fecha_proceso     = TODAY              -- SI
      LET abo.usuario           = gusuario           -- SI
      LET abo.fecha_archivo     = xfecha_lote_isss   -- NO
      LET abo.etiqueta          = 0                  -- SI

      INSERT INTO dis_provision VALUES(abo.*)

      IF STATUS < 0 THEN
         ERROR "ERROR AL INSERTAR REGISTRO EN dis_provision ",STATUS USING '-----'
         EXIT PROGRAM
      END IF

   END FOREACH
   CLOSE cur_prov
END FUNCTION


FUNCTION Sumatoria_rechazo(vimporte_bono,
                           vimporte_dbmx)

   DEFINE
      vimporte_bono         DECIMAL(22,4),
      vimporte_dbmx         DECIMAL(18,2)

   LET tot_rech.importe_bono = tot_rech.importe_bono + vimporte_bono
   LET tot_rech.importe_dbmx = tot_rech.importe_dbmx + vimporte_dbmx
 
END FUNCTION


FUNCTION Sumatoria_aceptado(vimporte_bono,
                            vimporte_dbmx)

   DEFINE
      vimporte_bono         DECIMAL(22,4),
      vimporte_dbmx         DECIMAL(18,2)

   LET tot_acep.importe_bono = tot_acep.importe_bono + vimporte_bono
   LET tot_acep.importe_dbmx = tot_acep.importe_dbmx + vimporte_dbmx

END FUNCTION


FUNCTION Genera_confaf(vfolio)          -- ETAPA 7
   DEFINE
      vfolio                INTEGER,
      ejecuta               CHAR(200),
      tipo_ar               SMALLINT,
      rident_pago           CHAR(16)

   DEFINE
      total_bono            DECIMAL(22,4),
      total_dbmx            DECIMAL(18,2),

      acept_bono            DECIMAL(22,4),
      acept_dbmx            DECIMAL(18,2),

      recha_bono            DECIMAL(22,4),
      recha_dbmx            DECIMAL(18,2)

   DEFINE
      vsub                  CHAR(08),         --c22-6
      vsie                  SMALLINT,         --c22-6
      vpor                  DECIMAL(16,8),    --c22-6
      vident_sie            CHAR(08),         --c22-6
      vpesos                DECIMAL(18,4),    --c22-6
      vcuantos              SMALLINT,         --c22-6.3
      vcontador_sie         SMALLINT,         --c22-6.3
      vsuma                 DECIMAL(18,4),    --c22-6
      vimporte              DECIMAL(18,4),    --c22-6
      vimporte2             DECIMAL(18,4),    --c22-6.9
      vrechazo              DECIMAL(18,4)     --c22-6

   DEFINE g_dep RECORD LIKE dis_dep_bono.*

   ------------------------- TOTAL ACEPTADO APORTES ------------------------
   LET acept_bono            = tot_acep.importe_bono
   LET acept_dbmx            = tot_acep.importe_dbmx

   ------------------------- TOTAL RECHAZOS APORTES ------------------------
   LET recha_bono            = tot_rech.importe_bono
   LET recha_dbmx            = tot_rech.importe_dbmx

   -------- Prepara aportes y comisiones de tmp_res_provision  -------

   CREATE TEMP TABLE tmp_res_provision                             --c22-6
   (sub   CHAR(08),                                                --c22-6
    sie   SMALLINT,                                                --c22-6
    por   DECIMAL(12,2),
    idsie CHAR(08),
    pesos DECIMAL(18,4))                                           --c22-6


   ---- APORTE DBMX ----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "41",                                                    --c22-6
          A.siefore,                                               --c22-6
          100,         #100 cambiar en porc. % (<>) en una subcta  --c22-6
          B.razon_social,                                          --c22-6
          sum(A.monto_en_pesos)                                    --c22-6
   FROM   dis_provision A, tab_siefore_local B                     --c22-6
   WHERE  A.folio     =   vfolio                                   --c22-6
   AND    A.subcuenta IN (30)                                      --c22-6
   AND    A.tipo_movimiento IN (17)                                --c22-6
   AND    A.estado    =   5                                        --c22-6
   AND    A.siefore   =   B.codigo_siefore                         --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6
 

   ---- APORTE BONO ----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "49",                                                    --c22-6
          A.siefore,                                               --c22-6
          100,         #100 cambiar en porc. % (<>) en una subcta  --c22-6
          B.razon_social,                                          --c22-6
          sum(A.monto_en_acciones)                                 --c22-6
   FROM   dis_provision A, tab_siefore_local B                     --c22-6
   WHERE  A.folio     = vfolio                                     --c22-6
   AND    A.subcuenta IN (36)                                      --c22-6
   AND    A.tipo_movimiento IN (17)                                --c22-6
   AND    A.estado    = 5                                          --c22-6
   AND    A.siefore   = B.codigo_siefore                           --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6

--   UPDATE dis_dep_bono
--   SET ident_pago[14,15] = 41
--   WHERE folio = vfolio

   -------- Obtiene aportes de tmp_res_provision --------
   DECLARE cur_resumen CURSOR FOR                                  --c22-6
   SELECT sub,                                                     --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          SUM(pesos),                                              --c22-6
          count(*)                                                 --c22-6.3
   FROM   tmp_res_provision                                        --c22-6
   WHERE  pesos <> 0  --alex
   GROUP  BY 1,2,3,4                                               --c22-6
   ORDER  BY 1,2,3,4                                               --c22-6

   LET vcontador_sie = 0                                           --c22-6.3

   FOREACH cur_resumen INTO vsub,vsie,vpor,vident_sie,vpesos       --c22-6.4
 
      SELECT COUNT(*)                       --c22-6.4
      INTO   vcuantos                       --c22-6.4
      FROM   tmp_res_provision              --c22-6.4
      WHERE  sub = vsub                     --c22-6.4

      SELECT SUM(pesos)                     --c22-6
      INTO   vsuma                          --c22-6
      FROM   tmp_res_provision              --c22-6
      WHERE  sub = vsub                     --c22-6

--    LET vpor = vpesos / vsuma             --c22-113

      CASE
         WHEN vsub = "41"
            LET vrechazo = recha_dbmx
         WHEN vsub = "49"
            LET vrechazo = recha_bono
      END CASE

--    IF vsie = 1 THEN                                         --c22-6.3

      SELECT *                                                 --c22-6
      INTO   g_dep.*
      FROM   dis_dep_bono                                      --c22-6
      WHERE  folio             = vfolio                        --c22-6
      AND    ident_pago[14,15] = vsub                          --c22-6
      AND    tipo_siefore      = 0                             --c22-6

      IF vcuantos = 1 THEN                                     --c22-6.3

         ### SOLO " OPCIONES 41 DBMX y 49 BONO
         IF vsub = 41 THEN
            UPDATE dis_dep_bono                                --c22-6
            SET    importe_bono     = importe_bono / 10000,    --c22-6.5
                   importe_dbmx     = importe_dbmx / 100,      --c22-6.5
                   impt_dbmx_acept  = vpesos,                  --c22-6
                   impt_dbmx_dev    = 0,  --vrechazo,          --c22-6.9
                   ident_siefore    = vident_sie,              --c22-6
                   tipo_siefore     = vsie                     --c22-6
            WHERE  folio            = vfolio                   --c22-6
            AND    ident_pago[14,15]= vsub                     --c22-6
            AND    tipo_siefore     = 0                        --c22-6

            IF vrechazo > 0 THEN                               --c22-6.9
               LET vimporte2 = g_dep.importe_dbmx / 100        --c22-6.9
               LET g_dep.importe_bono=g_dep.importe_bono/10000 --c22-6.9
               INSERT INTO dis_dep_bono VALUES                 --c22-6.9
                 (                                             --c22-6.9
                 g_dep.folio,                                  --c22-6.9
                 g_dep.tipo_registro,                          --c22-6.9
                 g_dep.ident_servicio,                         --c22-6.9
                 g_dep.ident_pago,                             --c22-6.9
                 g_dep.importe_bono,                           --c22-6.9
                 vimporte2,                                    --c22-6.9
                 g_dep.fech_liquidacion,                       --c22-6.9
                 0,                                            --c22-6.9
                 0,                                            --c22-6.9
                 0,                                            --c22-6.9
                 vrechazo,                                     --c22-6.9
                 "",                                           --c22-6.9
                 0,                                            --c22-6.9
                 g_dep.fecha_archivo,                          --c22-6.9
                 g_dep.fecha_envio,                            --c22-6.9
                 g_dep.estado
                 )                                             --c22-6.9
            END IF                                             --c22-6.9
         ELSE
            UPDATE dis_dep_bono                                --c22-6
            SET    importe_bono     = importe_bono / 10000,    --c22-6.5 
                   importe_dbmx     = importe_dbmx / 100,      --c22-6.5 
                   impt_bono_acept  = vpesos,                  --c22-6
                   impt_bono_dev    = vrechazo,                --c22-6.9
                   ident_siefore    = "",         --vident_sie --c22-6
                   tipo_siefore     = "000"       --vsie       --c22-6
            WHERE  folio            = vfolio                   --c22-6
            AND    ident_pago[14,15]= vsub                     --c22-6
            AND    tipo_siefore     = 0                        --c22-6

{DMR        IF vrechazo > 0 THEN                               --c22-6.9
 BONO UDIS     LET vimporte2 = g_dep.importe_bono / 10000      --c22-6.9
 1 SOLO REG    LET g_dep.importe_dbmx = g_dep.importe_dbmx/100 --c22-6.9
               INSERT INTO dis_dep_bono VALUES                 --c22-6.9
                 (                                             --c22-6.9
                 g_dep.folio,                                  --c22-6.9
                 g_dep.tipo_registro,                          --c22-6.9
                 g_dep.ident_servicio,                         --c22-6.9
                 g_dep.ident_pago,                             --c22-6.9
                 vimporte2,                                    --c22-6.9
                 g_dep.importe_dbmx,                           --c22-6.9
                 g_dep.fech_liquidacion,                       --c22-6.9
                 0,                                            --c22-6.9
                 0,                                            --c22-6.9
                 vrechazo,                                     --c22-6.9
                 0,                                            --c22-6.9
                 "",                                           --c22-6.9
                 0,                                            --c22-6.9
                 g_dep.fecha_archivo,                          --c22-6.9
                 g_dep.fecha_envio,                            --c22-6.9
                 g_dep.estado
                 )                                             --c22-6.9
            END IF                                             --c22-6.9  DMR}
         END IF
      ELSE                                                     --c22-6.3
         LET vcontador_sie = vcontador_sie + 1                 --c22-6.3

         IF vsub = 41 THEN
            IF vcontador_sie = 1 THEN                          --c22-6.3

               IF vsie = 2 THEN                                --c22-6.5
                  LET vrechazo = 0                             --c22-6.5
               END IF                                          --c22-6.5

               UPDATE dis_dep_bono                             --c22-6.3
               SET    importe_bono     = importe_bono / 10000, --c22-6.5
                      importe_dbmx     = importe_dbmx / 100,   --c22-6.5
                      impt_dbmx_acept  = vpesos,               --c22-6.3
                      impt_dbmx_dev    = 0,  --vrechazo,       --c22-6.10
                      ident_siefore    = vident_sie,           --c22-6.3
                      tipo_siefore     = vsie                  --c22-6.3
               WHERE  folio            = vfolio                --c22-6.3
               AND    ident_pago[14,15]= vsub                  --c22-6.3
               AND    tipo_siefore     = 0                     --c22-6.3

               IF vrechazo > 0 THEN                            --c22-6.9
                  LET vimporte2 = g_dep.importe_dbmx / 100     --c22-6.9
                  LET g_dep.importe_bono=g_dep.importe_bono/10000  --c22
                  INSERT INTO dis_dep_bono VALUES              --c22-6.9
                    (                                          --c22-6.9
                    g_dep.folio,                               --c22-6.9
                    g_dep.tipo_registro,                       --c22-6.9
                    g_dep.ident_servicio,                      --c22-6.9
                    g_dep.ident_pago,                          --c22-6.9
                    g_dep.importe_bono,                        --c22-6.9
                    vimporte2,                                 --c22-6.9
                    g_dep.fech_liquidacion,                    --c22-6.9
                    0,                                         --c22-6.9
                    0,                                         --c22-6.9
                    0,                                         --c22-6.9
                    vrechazo,                                  --c22-6.9
                    "",                                        --c22-6.9
                    0,                                         --c22-6.9
                    g_dep.fecha_archivo,                       --c22-6.9
                    g_dep.fecha_envio,                         --c22-6.9
                    g_dep.estado                               --c22-6.9
                    )
               END IF
            ELSE

           --  IF vsie = 2 THEN    -- OR vsie= 3 THEN          --c22-119
           --     LET vrechazo = 0                             --c22-119
           --  END IF                                          --c22-119

               IF vrechazo > 0 THEN                            --c22-119
                  LET vimporte   = g_dep.importe_dbmx --/ 100  --c22-119
               ELSE                                            --c22-119
                  LET vimporte   = g_dep.importe_dbmx / 100    --c22-119
               END IF                                          --c22-119

               IF vsie >= 2 THEN                               --c22-119
                  LET vrechazo = 0                             --c22-119
               END IF                                          --c22-119

               IF vcontador_sie = 2 THEN
                  LET g_dep.importe_bono=g_dep.importe_bono/10000
               ELSE
                  LET g_dep.importe_bono=g_dep.importe_bono
               END IF

               INSERT INTO dis_dep_bono VALUES                 --c22-6
                 (                                             --c22-6
                 g_dep.folio,                                  --c22-6
                 g_dep.tipo_registro,                          --c22-6
                 g_dep.ident_servicio,                         --c22-6
                 g_dep.ident_pago,                             --c22-6
                 g_dep.importe_bono,                           --c22-6
                 vimporte,                                     --c22-6
                 g_dep.fech_liquidacion,                       --c22-6
                 0,                                            --c22-6
                 vpesos,                                       --c22-6
                 0,                                            --c22-6
                 vrechazo,                                     --c22-6
                 vident_sie,                                   --c22-6
                 vsie,
                 g_dep.fecha_archivo,                          --c22-6.9
                 g_dep.fecha_envio,                            --c22-6.9
                 g_dep.estado                                  --c22-6
                 )                                             --c22-6
            END IF                                             --c22-6

            IF vcontador_sie = vcuantos THEN                   --c22-6.3
               LET vcontador_sie = 0                           --c22-6.3
            END IF                                             --c22-6.3
         ELSE
            IF vcontador_sie = 1 THEN                          --c22-6.3

               IF vsie = 2 THEN                                --c22-6.5
                  LET vrechazo = 0                             --c22-6.5
               END IF                                          --c22-6.5

               UPDATE dis_dep_bono                             --c22-6.3
               SET    importe_bono     = importe_bono / 10000, --c22-6.5
                      importe_dbmx     = importe_dbmx / 100,   --c22-6.5
                      impt_bono_acept  = vpesos,               --c22-6.3
                      impt_bono_dev    = 0,  --vrechazo,       --c22-6.10
                      ident_siefore    = vident_sie,           --c22-6.3
                      tipo_siefore     = vsie                  --c22-6.3
               WHERE  folio            = vfolio                --c22-6.3
               AND    ident_pago[14,15]= vsub                  --c22-6.3
               AND    tipo_siefore     = 0                     --c22-6.3

               IF vrechazo > 0 THEN                            --c22-6.9
                  LET vimporte2 = g_dep.importe_bono / 10000   --c22-6.9
                  LET g_dep.importe_dbmx=g_dep.importe_dbmx/100  --c22-6
                  INSERT INTO dis_dep_bono VALUES              --c22-6.9
                    (                                          --c22-6.9
                    g_dep.folio,                               --c22-6.9
                    g_dep.tipo_registro,                       --c22-6.9
                    g_dep.ident_servicio,                      --c22-6.9
                    g_dep.ident_pago,                          --c22-6.9
                    vimporte2,                                 --c22-6.9
                    g_dep.importe_dbmx,                        --c22-6.9
                    g_dep.fech_liquidacion,                    --c22-6.9
                    0,                                         --c22-6.9
                    0,                                         --c22-6.9
                    vrechazo,                                  --c22-6.9
                    0,                                         --c22-6.9
                    "",                                        --c22-6.9
                    0,                                         --c22-6.9
                    g_dep.fecha_archivo,                       --c22-6.9
                    g_dep.fecha_envio,                         --c22-6.9
                    g_dep.estado                               --c22-6.9
                    )
               END IF
            ELSE

           --  IF vsie = 2 THEN    -- OR vsie= 3 THEN          --c22-119
           --     LET vrechazo = 0                             --c22-119
           --  END IF                                          --c22-119

               IF vrechazo > 0 THEN                            --c22-119
                  LET vimporte   = g_dep.importe_bono --/ 100  --c22-119
               ELSE                                            --c22-119
                  LET vimporte   = g_dep.importe_bono / 10000  --c22-119
               END IF                                          --c22-119

               IF vsie >= 2 THEN                               --c22-119
                  LET vrechazo = 0                             --c22-119
               END IF                                          --c22-119

               LET g_dep.importe_dbmx = g_dep.importe_dbmx/100 --c22-6
               INSERT INTO dis_dep_bono VALUES                 --c22-6
                 (                                             --c22-6
                 g_dep.folio,                                  --c22-6
                 g_dep.tipo_registro,                          --c22-6
                 g_dep.ident_servicio,                         --c22-6
                 g_dep.ident_pago,                             --c22-6
                 vimporte,                                     --c22-6
                 g_dep.importe_dbmx,                           --c22-6
                 g_dep.fech_liquidacion,                       --c22-6
                 vpesos,                                       --c22-6
                 0,                                            --c22-6
                 vrechazo,                                     --c22-6
                 0,                                            --c22-6
                 vident_sie,                                   --c22-6
                 vsie,
                 g_dep.fecha_archivo,                          --c22-6.9
                 g_dep.fecha_envio,                            --c22-6.9
                 g_dep.estado                                  --c22-6
                 )                                             --c22-6
            END IF                                             --c22-6

            IF vcontador_sie = vcuantos THEN                   --c22-6.3
               LET vcontador_sie = 0                           --c22-6.3
            END IF                                             --c22-6.3
         END IF                                                --c22-6.3
      END IF                                                   --c22-6

      DECLARE curctar CURSOR FOR
      SELECT ident_pago
      FROM dis_dep_bono
      WHERE folio = vfolio
      AND   impt_bono_acept = 0
      AND   impt_bono_dev   = 0
      AND   impt_dbmx_acept = 0
      AND   impt_dbmx_dev   = 0
      AND   ident_pago      = g_dep.ident_pago

      FOREACH curctar INTO rident_pago
         IF rident_pago[14,15] = "41" THEN
            UPDATE dis_dep_bono
            SET impt_dbmx_dev = vrechazo,
                importe_dbmx  = importe_dbmx / 100
            WHERE folio      = vfolio
            AND   ident_pago = rident_pago
         ELSE
            UPDATE dis_dep_bono
            SET impt_bono_dev = vrechazo,
                importe_bono  = importe_bono / 10000
            WHERE folio      = vfolio
            AND   ident_pago = rident_pago
         END IF
      END FOREACH
   END FOREACH

   CALL Genera_salidas(vfolio,
                       acept_bono,
                       acept_dbmx,
                       recha_bono,
                       recha_dbmx
                       )

   # Para tipo 1 y como valor default
   LET vnom_archivo= new_cadena CLIPPED,".BONCONF" CLIPPED

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; cat cza rech dep sum > ",vnom_archivo CLIPPED
   RUN ejecuta
END FUNCTION

 
FUNCTION Genera_salidas(vfolio,
                        acept_bono,
                        acept_dbmx,
                        recha_bono,
                        recha_dbmx
                        )

   DEFINE
      vfolio                 INTEGER,
      acept_bono             DECIMAL(22,4),
      acept_dbmx             DECIMAL(18,2),
      recha_bono             DECIMAL(22,4),
      recha_dbmx             DECIMAL(18,2)
 
   DEFINE reg_cza RECORD
      tipo_registro          CHAR(02),
      ident_servicio         CHAR(02),
      ident_operacion        CHAR(02),
      tipo_ent_origen        CHAR(02),
      clave_ent_origen       CHAR(03),
      tipo_ent_destino       CHAR(02),
      clave_ent_destino      CHAR(03),
      fech_creac_lote        CHAR(08),
      lote_del_dia           CHAR(03),
      mod_recp_envio         CHAR(02),
      fech_limite_resp       CHAR(08),
      result_operacion       CHAR(02),
      motivo_rechazo1        CHAR(03),
      motivo_rechazo2        CHAR(03),
      motivo_rechazo3        CHAR(03)
   END RECORD

   DEFINE reg_rech RECORD
      tipo_registro          CHAR(2),
      ident_servicio         CHAR(2),
      consec_reg_lote        INTEGER,
      n_unico                CHAR(18),
      n_seguro               CHAR(11),
      n_seguro_issste        CHAR(11),
      n_rfc                  CHAR(13),
      id_procesar            CHAR(8),
      paterno                CHAR(40),
      materno                CHAR(40),
      nombres                CHAR(40),
      sel_trab               CHAR(1),
      ind_tipo_bono_isss     CHAR(1),
      fecha_reden            CHAR(8),
      importe_bono           DECIMAL(22,4),
      importe_dbmx           DECIMAL(18,2),
      result_operacion       CHAR(2),
      det_mot_rechazo1       CHAR(3),
      det_mot_rechazo2       CHAR(3),
      det_mot_rechazo3       CHAR(3)
   END RECORD

   DEFINE reg_dep RECORD
      tipo_registro          CHAR(02),
      ident_servicio         CHAR(02),
      ident_pago             CHAR(16),
      importe_bono           DECIMAL(22,4),
      importe_dbmx           DECIMAL(18,2),
      fech_liquidacion       DATE,
      impt_bono_acept        DECIMAL(22,4),
      impt_dbmx_acept        DECIMAL(18,2),
      impt_bono_dev          DECIMAL(22,4),
      impt_dbmx_dev          DECIMAL(18,2),
      ident_siefore          CHAR(08),
      tipo_siefore           SMALLINT
   END RECORD

   DEFINE reg_sum RECORD
      tipo_registro          CHAR(02),
      ident_servicio         CHAR(02),
      ident_operacion        CHAR(02),
      tipo_ent_origen        CHAR(02),
      clave_ent_origen       CHAR(03),
      tipo_ent_destino       CHAR(02),
      clave_ent_destino      CHAR(03),
      fech_creac_lote        CHAR(08),
      lote_del_dia           CHAR(03),
      impt_tot_bono          DECIMAL(22,4),
      impt_tot_dbmx          DECIMAL(18,2),
      num_de_reg_aport       CHAR(9),
      num_cuentas_xpagar     CHAR(6)
   END RECORD,

   vimpt_tot_bono            DECIMAL(22,4),
   vimpt_tot_dbmx            DECIMAL(18,2)

   DEFINE xident_siefore     CHAR(08),      --c22-6.6
          xtipo_siefore      SMALLINT,      --c22-6.6
          vimpt_acept        DECIMAL(18,2), --c22-6.6
          vtipo_sie          SMALLINT       --c22-6.6

   --------------- GENERA RESPUESTA cza ---------------------------
   SELECT tipo_registro,
          ident_servicio,
          ident_operacion,
          tipo_ent_origen,
          clave_ent_origen,
          tipo_ent_destino,
          clave_ent_destino,
          fech_creac_lote,
          lote_del_dia,
          mod_recp_envio,
          fech_limite_resp,
          result_operacion,
          motivo_rechazo1,
          motivo_rechazo2,
          motivo_rechazo3
   INTO  reg_cza.*
   FROM  dis_cza_bono
   WHERE folio = vfolio

   LET reg_cza.tipo_ent_origen   = "01"
   LET reg_cza.clave_ent_origen  = vcod_afore
   LET reg_cza.tipo_ent_destino  = "03"
   LET reg_cza.clave_ent_destino = "001"
   LET reg_cza.mod_recp_envio    = "02"
   LET reg_cza.result_operacion  = ""
   LET reg_cza.motivo_rechazo1   = ""
   LET reg_cza.motivo_rechazo2   = ""
   LET reg_cza.motivo_rechazo3   = ""

   DISPLAY "GENERANDO CZA..." #---AT 10,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/cza"
   START REPORT rep_cza TO vreporte
      OUTPUT TO REPORT rep_cza(reg_cza.*)
   FINISH REPORT rep_cza

   --------------- GENERA RESPUESTA RECHAZOS ---------------
   DECLARE cur_rech CURSOR FOR
   SELECT
      a.tipo_registro,
      a.ident_servicio,
      a.consec_reg_lote,
      a.n_unico,
      a.n_seguro,
      a.n_seguro_issste,
      a.n_rfc,
      a.id_procesar,
      a.paterno,
      a.materno,
      a.nombres,
      a.sel_trab,
      a.ind_tipo_bono_isss,
      a.fecha_reden,
      a.importe_bono,
      a.importe_dbmx,
      a.result_operacion,
      a.det_mot_rechazo1,
      a.det_mot_rechazo2,
      a.det_mot_rechazo3
   FROM  dis_det_bono a
   WHERE a.folio = vfolio
   AND   a.result_operacion = "02"              ---- Rechazos
   ORDER BY a.consec_reg_lote, a.tipo_registro

   LET reg_rech.result_operacion  = "00"
   LET reg_rech.det_mot_rechazo1  = "000"
   LET reg_rech.det_mot_rechazo2  = "000"
   LET reg_rech.det_mot_rechazo3  = "000"

   DISPLAY "GENERANDO RECH..." #---AT 12,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/rech"
   START REPORT rep_rech TO vreporte
      FOREACH cur_rech INTO reg_rech.*
         OUTPUT TO REPORT rep_rech(reg_rech.*)
      END FOREACH
   FINISH REPORT rep_rech

   --------------- GENERA RESPUESTA DEPOSITOS ---------------------

   DECLARE cur_41 CURSOR FOR                                     --c22-6.6
   SELECT  ident_siefore,                                        --c22-6.6
           tipo_siefore                                          --c22-6.6
   FROM    dis_dep_bono                                          --c22-6.6
   WHERE   folio = vfolio                                        --c22-6.6
   AND     ident_pago[14,15] = "41"                              --c22-6.6

   FOREACH cur_41 INTO xident_siefore,xtipo_siefore              --c22-6.6

      SELECT impt_dbmx_acept,                                    --c22-6.6
             tipo_siefore                                        --c22-6.6
      INTO   vimpt_acept,                                        --c22-6.6
             vtipo_sie                                           --c22-6.6
      FROM   dis_dep_bono                                        --c22-6.6
      WHERE  folio = vfolio                                      --c22-6.6
      AND    ident_pago[14,15] = "11"                            --c22-6.6
      AND    tipo_siefore = 0                                    --c22-6.6

      IF STATUS <> NOTFOUND THEN                                 --c22-6.6
         IF vimpt_acept = 0 AND vtipo_sie = 0 THEN               --c22-6.6
            UPDATE dis_dep_bono                                  --c22-6.6
            SET    tipo_siefore  = xtipo_siefore,                --c22-6.6
                   ident_siefore = xident_siefore                --c22-6.6
            WHERE  folio             = vfolio                    --c22-6.6
            AND    ident_pago[14,15] = "11"                      --c22-6.6
            AND    tipo_siefore      = 0                         --c22-6.6
         END IF                                                  --c22-6.6
      END IF                                                     --c22-6.6
   END FOREACH                                                   --c22-6.6

   DECLARE cur_49 CURSOR FOR                                     --c22-6.6
   SELECT  ident_siefore,                                        --c22-6.6
           tipo_siefore                                          --c22-6.6
   FROM    dis_dep_bono                                          --c22-6.6
   WHERE   folio = vfolio                                        --c22-6.6
   AND     ident_pago[14,15] = "49"                              --c22-6.6

   FOREACH cur_49 INTO xident_siefore,xtipo_siefore              --c22-6.6

      SELECT impt_bono_acept,                                    --c22-6.6
             tipo_siefore                                        --c22-6.6
      INTO   vimpt_acept,                                        --c22-6.6
             vtipo_sie                                           --c22-6.6
      FROM   dis_dep_bono                                        --c22-6.6
      WHERE  folio = vfolio                                      --c22-6.6
      AND    ident_pago[14,15] = "12"                            --c22-6.6
      AND    tipo_siefore = 0                                    --c22-6.6

      IF STATUS <> NOTFOUND THEN                                 --c22-6.6
         IF vimpt_acept = 0 AND vtipo_sie = 0 THEN               --c22-6.6
            UPDATE dis_dep_bono                                  --c22-6.6
            SET    tipo_siefore = xtipo_siefore,                 --c22-6.6
                   ident_siefore = xident_siefore                --c22-6.6
            WHERE  folio             = vfolio                    --c22-6.6
            AND    ident_pago[14,15] = "12"                      --c22-6.6
            AND    tipo_siefore      = 0                         --c22-6.6
         END IF                                                  --c22-6.6
      END IF                                                     --c22-6.6
   END FOREACH                                                   --c22-6.6

   DECLARE cur_dep2 CURSOR FOR
   SELECT
      tipo_registro,
      ident_servicio,
      ident_pago,
      importe_bono,
      importe_dbmx,
      fech_liquidacion,
      impt_bono_acept,
      impt_dbmx_acept,
      impt_bono_dev,
      impt_dbmx_dev,
      ident_siefore,
      tipo_siefore
   FROM dis_dep_bono  
   WHERE folio = vfolio

   DISPLAY "GENERANDO DEP....." #------AT 14,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/dep"
   START REPORT rep_dep TO vreporte
      FOREACH cur_dep2 INTO reg_dep.*
         OUTPUT TO REPORT rep_dep(reg_dep.*)
      END FOREACH
   FINISH REPORT rep_dep

   --------------- GENERA RESPUESTA SUM       ---------------------
   DECLARE cur_sum CURSOR FOR
   SELECT tipo_registro,
          ident_servicio,
          ident_operacion,
          tipo_ent_origen,
          clave_ent_origen,
          tipo_ent_destino,
          clave_ent_destino,
          fech_creac_lote,
          lote_del_dia,
          impt_tot_bono,
          impt_tot_dbmx,
          num_de_reg_aport,
          num_cuentas_xpagar
   FROM   dis_sum_bono  
   WHERE  folio = vfolio
   FOR UPDATE

   DISPLAY "GENERANDO SUM..." #----AT 16,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/sum"
   START REPORT rep_sum TO vreporte
      FOREACH cur_sum INTO reg_sum.*

         LET reg_sum.tipo_ent_origen        = "01"
         LET reg_sum.clave_ent_origen       = vcod_afore
         LET reg_sum.tipo_ent_destino       = "03"
         LET reg_sum.clave_ent_destino      = "001"
         LET reg_sum.fech_creac_lote        = reg_cza.fech_creac_lote
         LET reg_sum.lote_del_dia           = reg_cza.lote_del_dia
         LET reg_sum.num_de_reg_aport       = "000000000"

         UPDATE dis_sum_bono
         SET    impt_tot_bono        = recha_bono     + acept_bono,
                impt_tot_dbmx        = recha_dbmx     + acept_dbmx
         WHERE  CURRENT OF cur_sum

         OUTPUT TO REPORT rep_sum(reg_sum.*,
                                  acept_bono,
                                  acept_dbmx,
                                  recha_bono,
                                  recha_dbmx
                                  )
      END FOREACH
   FINISH REPORT rep_sum
END FUNCTION


REPORT rep_cza(reg_cza)
   DEFINE reg_cza RECORD
      tipo_registro     CHAR(02),
      ident_servicio    CHAR(02),
      ident_operacion   CHAR(02),
      tipo_ent_origen   CHAR(02),
      clave_ent_origen  CHAR(03),
      tipo_ent_destino  CHAR(02),
      clave_ent_destino CHAR(03),
      fech_creac_lote   CHAR(08),
      lote_del_dia      CHAR(03),
      mod_recp_envio    CHAR(02),
      fech_limite_resp  CHAR(08),
      result_operacion  CHAR(02),
      motivo_rechazo1   CHAR(03),
      motivo_rechazo2   CHAR(03),
      motivo_rechazo3   CHAR(03)
   END RECORD

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         PRINT COLUMN 1,reg_cza.tipo_registro,
                        reg_cza.ident_servicio,
                        reg_cza.ident_operacion,
                        reg_cza.tipo_ent_origen,
                        reg_cza.clave_ent_origen,
                        reg_cza.tipo_ent_destino,
                        reg_cza.clave_ent_destino,
                        reg_cza.fech_creac_lote,
                        reg_cza.lote_del_dia,
                        reg_cza.mod_recp_envio,
                        reg_cza.fech_limite_resp,
                        reg_cza.result_operacion,
                        reg_cza.motivo_rechazo1,
                        reg_cza.motivo_rechazo2,
                        reg_cza.motivo_rechazo3,
                        402 SPACES
END REPORT


REPORT rep_rech(reg_rech)
DEFINE reg_rech RECORD
      tipo_registro         CHAR(2),
      ident_servicio        CHAR(2),
      consec_reg_lote       INTEGER,
      n_unico               CHAR(18),
      n_seguro              CHAR(11),
      n_seguro_issste       CHAR(11),
      n_rfc                 CHAR(13),
      id_procesar           CHAR(8),
      paterno               CHAR(40),
      materno               CHAR(40),
      nombres               CHAR(40),
      sel_trab              CHAR(1),
      ind_tipo_bono_isss    CHAR(1),
      fecha_reden           CHAR(8),
      importe_bono          DECIMAL(22,4),
      importe_dbmx          DECIMAL(18,2),
      result_operacion      CHAR(2),
      det_mot_rechazo1      CHAR(3),
      det_mot_rechazo2      CHAR(3),
      det_mot_rechazo3      CHAR(3)
   END RECORD,

   cconsec_reg_lote        CHAR(08),
   cimpt_bono              CHAR(17),
   cimpt_dbmx              CHAR(15),

   xconsec_reg_lote        CHAR(08),
   ximpt_bono              CHAR(16),
   ximpt_dbmx              CHAR(14)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW

   LET vnum_reg = vnum_reg + 1

   LET cimpt_bono = reg_rech.importe_bono / 10000 USING '&&&&&&&&&&&&.&&&&'
   LET cimpt_dbmx = reg_rech.importe_dbmx / 100   USING '&&&&&&&&&&&&.&&'

   LET xconsec_reg_lote = reg_rech.consec_reg_lote  USING '&&&&&&&&'

   LET ximpt_bono = cimpt_bono [1,12], cimpt_bono [14,17]
   LET ximpt_dbmx = cimpt_dbmx [1,12], cimpt_dbmx [14,15]

         PRINT
            COLUMN 1,reg_rech.tipo_registro,
                     reg_rech.ident_servicio,
                     xconsec_reg_lote,
                     reg_rech.n_unico,
                     reg_rech.n_seguro,
                     reg_rech.n_seguro_issste,
                     reg_rech.n_rfc,
                     reg_rech.id_procesar,
                     reg_rech.paterno,
                     reg_rech.materno,
                     reg_rech.nombres,
                     reg_rech.sel_trab,
                     reg_rech.ind_tipo_bono_isss,
                     reg_rech.fecha_reden,
                     ximpt_bono,
                     ximpt_dbmx,
                     "00",                      --- result_operacion,
                     reg_rech.det_mot_rechazo1,
                     reg_rech.det_mot_rechazo2,
                     reg_rech.det_mot_rechazo3,
                     206 space
END REPORT


REPORT rep_dep(reg_dep)
   DEFINE reg_dep RECORD
      tipo_registro     CHAR(02),
      ident_servicio    CHAR(02),
      ident_pago        CHAR(16),
      importe_bono      DECIMAL(22,4),
      importe_dbmx      DECIMAL(18,2),
      fech_liquidacion  DATE,
      impt_bono_acept   DECIMAL(22,4),
      impt_dbmx_acept   DECIMAL(18,2),
      impt_bono_dev     DECIMAL(22,4),
      impt_dbmx_dev     DECIMAL(18,2),
      ident_siefore     CHAR(08),
      tipo_siefore      SMALLINT
   END RECORD,

   c19_importe_bono     CHAR(19),
   c18_importe_bono     CHAR(18),
   c17_importe_dbmx     CHAR(17),
   c16_importe_dbmx     CHAR(16),
   c19_impt_bono_acept  CHAR(19),
   c17_impt_dbmx_acept  CHAR(17),
   c18_impt_bono_acept  CHAR(18),
   c16_impt_dbmx_acept  CHAR(16),
   c19_impt_bono_dev    CHAR(19),
   c17_impt_dbmx_dev    CHAR(17),
   c18_impt_bono_dev    CHAR(18),
   c16_impt_dbmx_dev    CHAR(16),
   xfecha_liquidacion   CHAR(10),
   vfecha_liquidacion   CHAR(08)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         LET vnum_ctas_xpagar = vnum_ctas_xpagar + 1

         IF reg_dep.importe_bono IS NULL THEN
            LET c18_importe_bono="000000000000000000"
         ELSE
            LET c19_importe_bono=reg_dep.importe_bono USING"&&&&&&&&&&&&&&.&&&&"
            LET c18_importe_bono=c19_importe_bono[01,14],c19_importe_bono[16,19]
         END IF

         IF reg_dep.importe_dbmx IS NULL THEN
            LET c16_importe_dbmx="0000000000000000"
         ELSE
            LET c17_importe_dbmx=reg_dep.importe_dbmx USING"&&&&&&&&&&&&&&.&&"
            LET c16_importe_dbmx=c17_importe_dbmx[01,14],c17_importe_dbmx[16,17]
         END IF

         IF reg_dep.impt_bono_acept IS NULL THEN
            LET c18_impt_bono_acept = "000000000000000000"
         ELSE
            LET c19_impt_bono_acept = reg_dep.impt_bono_acept
                                      USING"&&&&&&&&&&&&&&.&&&&"
            LET c18_impt_bono_acept = c19_impt_bono_acept[01,14],
                                      c19_impt_bono_acept[16,19]
         END IF

         IF reg_dep.impt_bono_dev IS NULL THEN
            LET c18_impt_bono_dev = "000000000000000000"
         ELSE
            LET c19_impt_bono_dev = reg_dep.impt_bono_dev
                                    USING"&&&&&&&&&&&&&&.&&&&"
            LET c18_impt_bono_dev = c19_impt_bono_dev[01,14],
                                    c19_impt_bono_dev[16,19]
         END IF

         IF reg_dep.impt_dbmx_acept IS NULL THEN
            LET c16_impt_dbmx_acept = "0000000000000000"
         ELSE
            LET c17_impt_dbmx_acept = reg_dep.impt_dbmx_acept
                                      USING"&&&&&&&&&&&&&&.&&"
            LET c16_impt_dbmx_acept = c17_impt_dbmx_acept[01,14],
                                      c17_impt_dbmx_acept[16,17]
         END IF

         IF reg_dep.impt_dbmx_dev IS NULL THEN
            LET c16_impt_dbmx_dev = "0000000000000000"
         ELSE
            LET c17_impt_dbmx_dev = reg_dep.impt_dbmx_dev
                                    USING"&&&&&&&&&&&&&&.&&"
            LET c16_impt_dbmx_dev = c17_impt_dbmx_dev[01,14],
                                    c17_impt_dbmx_dev[16,17]
         END IF

         LET xfecha_liquidacion = reg_dep.fech_liquidacion
         LET vfecha_liquidacion = xfecha_liquidacion[7,10],
                                  xfecha_liquidacion[1,02],
                                  xfecha_liquidacion[4,05]

         PRINT COLUMN 01,reg_dep.tipo_registro,
                         reg_dep.ident_servicio,
                         reg_dep.ident_pago,
                         c18_importe_bono,
                         c16_importe_dbmx,
                         vfecha_liquidacion,
                         c18_impt_bono_acept,
                         c16_impt_dbmx_acept,
                         c18_impt_bono_dev,
                         c16_impt_dbmx_dev,
                         reg_dep.ident_siefore,
                         reg_dep.tipo_siefore    USING '&&&',
                         309 space
END REPORT


REPORT rep_sum(reg_sum,
               acept_bono,
               acept_dbmx,
               recha_bono,
               recha_dbmx
               )

   DEFINE
      acept_bono             DECIMAL(22,4),
      acept_dbmx             DECIMAL(18,2),
      recha_bono             DECIMAL(22,4),
      recha_dbmx             DECIMAL(18,2)

   DEFINE reg_sum RECORD
      tipo_registro          CHAR(02),
      ident_servicio         CHAR(02),
      ident_operacion        CHAR(02),
      tipo_ent_origen        CHAR(02),
      clave_ent_origen       CHAR(03),
      tipo_ent_destino       CHAR(02),
      clave_ent_destino      CHAR(03),
      fech_creac_lote        CHAR(08),
      lote_del_dia           CHAR(03),
      impt_tot_bono          DECIMAL(22,4),
      impt_tot_dbmx          DECIMAL(18,2),
      num_de_reg_aport       CHAR(9),
      num_cuentas_xpagar     CHAR(6)
   END RECORD,

   c18_impt_tot_bono         ,
   c18_impt_tot_dbmx         CHAR(18),

   c19_impt_tot_bono         ,
   c19_impt_tot_dbmx         CHAR(19),

   d16_impt_tot_bono         DECIMAL(22,4),
   d16_impt_tot_dbmx         DECIMAL(18,2)


   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW

         IF reg_sum.impt_tot_bono IS NULL THEN
            LET reg_sum.impt_tot_bono = 0
         END IF
         IF reg_sum.impt_tot_dbmx IS NULL THEN
            LET reg_sum.impt_tot_dbmx = 0
         END IF

         LET reg_sum.impt_tot_bono = recha_bono * 10000
         LET reg_sum.impt_tot_dbmx = recha_dbmx * 100
 
         PRINT COLUMN 1,
                     reg_sum.tipo_registro,
                     reg_sum.ident_servicio,
                     reg_sum.ident_operacion,
                     reg_sum.tipo_ent_origen,
                     vcod_afore              USING "&&&" ,
                     reg_sum.tipo_ent_destino,
                     reg_sum.clave_ent_destino,
                     reg_sum.fech_creac_lote,
                     reg_sum.lote_del_dia,
                     reg_sum.impt_tot_bono    USING '&&&&&&&&&&&&&&&&&&',
                     reg_sum.impt_tot_dbmx    USING '&&&&&&&&&&&&&&&&',
                     vnum_reg                 USING '&&&&&&&&&',
                     vnum_ctas_xpagar         USING '&&&&&&',
                     374 space
END REPORT


FUNCTION Lectura()
   DEFINE
       ejecuta  CHAR(200)

   CASE reg_ident.tipo
      WHEN 1
         LET vrecauda = "N"
         EXIT CASE
      WHEN 2
         LET vrecauda = "E"
         EXIT CASE
      OTHERWISE
         EXIT CASE
   END CASE
END FUNCTION


FUNCTION actualiza_bat_f(v_folio)
   DEFINE v_cat          CHAR(600),
          vv_fecha_log   CHAR(030),
          vv_prog        CHAR(010),
          paso           CHAR(100)

   DEFINE v_fecha_log DATETIME YEAR TO SECOND

   DEFINE v_folio  integer
   DEFINE reg_ruta RECORD LIKE seg_modulo.*

   SELECT A.*
   INTO   reg_ruta.*
   FROM   seg_modulo A
   WHERE  modulo_cod = "bat"
 
   UPDATE bat_ctr_operacion
   SET    folio      = v_folio,
          estado_cod = 4,
          fecha_fin  = CURRENT
   WHERE pid         = reg_ident.pid
   AND   proceso_cod = reg_ident.proceso_cod
   AND   opera_cod   = reg_ident.opera_cod

   UPDATE bat_tmp_predecesor
   SET    bandera_ejecuta  = 1
   WHERE  pid_prod         = reg_ident.pid
   AND    proceso_cod_prod = reg_ident.proceso_cod
   AND    opera_cod_prod   = reg_ident.opera_cod

   LET v_fecha_log = CURRENT
   LET vv_fecha_log = v_fecha_log
 
   SELECT A.programa_cod
   INTO   vv_prog
   FROM   bat_ctr_operacion A
   WHERE  A.pid         = reg_ident.pid
   AND    A.proceso_cod = reg_ident.proceso_cod
   AND    A.opera_cod   = reg_ident.opera_cod

   LET paso = "nohup:"         ,
       reg_ident.pid         USING"&&&&&",":",
       reg_ident.proceso_cod USING"&&&&&",":",
       reg_ident.opera_cod   USING"&&&&&"
 
   LET v_cat = "echo '"        ,
       vv_fecha_log[1,4]       ,
       vv_fecha_log[6,7]       ,
       vv_fecha_log[9,10]      ,
       vv_fecha_log[12,13]     ,
       vv_fecha_log[15,16]     ,
       vv_fecha_log[18,19]     ,
       "|"                     ,
       vv_prog  CLIPPED        ,
       "|"                     ,
       "FINOK"                 ,
       "|"                     ,
       reg_ruta.ruta_listados CLIPPED,
       "/"                     ,
       paso CLIPPED            ,
       "'"                     ,
       " >> "                  ,
       reg_ruta.ruta_envio CLIPPED,
       "/"                     ,
       "aad_safre.log"

   LET v_cat = v_cat CLIPPED
   RUN v_cat
END FUNCTION


FUNCTION buscar_cual_nss(curpa)
DEFINE
   curpa      CHAR(18),
   conta_afi  SMALLINT,
   nssa       CHAR(11),
   tipo_sol   SMALLINT,
   i          SMALLINT,
   tipo_adm   CHAR(2)

   SELECT count(*)
   INTO conta_afi
   FROM afi_mae_afiliado
   WHERE n_unico = curpa

   IF conta_afi = 0 OR conta_afi IS NULL THEN
      RETURN curpa, "00000000000", "01", 1       ## 1 Rechazo
   END IF

   IF conta_afi = 1 THEN
      SELECT n_seguro, tipo_solicitud
      INTO   nssa , tipo_sol
      FROM afi_mae_afiliado
      WHERE n_unico = curpa
  
      SELECT "OK"
      FROM  cta_act_marca
      WHERE nss = nssa
      AND marca_cod IN (130,150)
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         IF tipo_sol = 8 OR tipo_sol = 12  THEN       #### TRABAJADOR ISSSTE
            SELECT "OK"
            FROM  cta_ctr_reg_ind
            WHERE curp = curpa
            AND   nti  = nssa
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
               ERROR " Inconsistencia en Datos, NSS issste no tiene NTI ",curpa

               INSERT INTO cta_ctr_reg_ind
               VALUES (curpa,nssa," ",2,"01","02",today,user)

               RETURN curpa, nssa, "01", 0              ## 0 Aceptado
            ELSE
               DECLARE cur_ccta CURSOR FOR
               SELECT tipo_administracion
               FROM   cta_ctr_reg_ind
               WHERE  curp = curpa
               AND    nti  = nssa

               FOREACH cur_ccta INTO tipo_adm
                  EXIT FOREACH
               END FOREACH
               RETURN curpa, nssa, tipo_adm, 0          ## 0 Aceptado
            END IF
         ELSE
            RETURN curpa, nssa, "01", 0                 ## 0 Aceptado
         END IF
      ELSE
         RETURN curpa, nssa, "01", 1                    ## 1 Rechazo
      END IF
   END IF 

   IF conta_afi > 1 THEN
      DECLARE cur_mae CURSOR FOR
      SELECT n_seguro, tipo_solicitud
      FROM afi_mae_afiliado
      WHERE n_unico = curpa

      LET i = 0
      FOREACH cur_mae INTO nssa, tipo_sol
         LET i = i + 1

         SELECT "OK"
         FROM  cta_act_marca
         WHERE nss = nssa
         AND marca_cod IN (130,150)
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            IF tipo_sol = 8 OR tipo_sol = 12  THEN      #### TRABAJADOR ISSSTE
               SELECT "OK"
               FROM  cta_ctr_reg_ind
               WHERE curp = curpa
               AND   nti  = nssa
               GROUP BY 1

               IF STATUS = NOTFOUND THEN
                  ERROR " Inconsistencia en Datos, NSS issste no tiene NTI ",curpa

                  INSERT INTO cta_ctr_reg_ind
                  VALUES (curpa,nssa," ",2,"01","02",today,user)

                  RETURN curpa, nssa, "01", 0              ## 0 Aceptado
               ELSE
                  DECLARE cur_ccta2 CURSOR FOR
                  SELECT tipo_administracion
                  FROM   cta_ctr_reg_ind
                  WHERE  curp = curpa
                  AND    nti  = nssa

                  FOREACH cur_ccta INTO tipo_adm
                     EXIT FOREACH
                  END FOREACH
                  RETURN curpa, nssa, tipo_adm, 0          ## 0 Aceptado
               END IF
            ELSE
               RETURN curpa, nssa, "01", 0                 ## 0 Aceptado
            END IF
         ELSE
            IF i = conta_afi THEN
               RETURN curpa, nssa, "01", 1                 ## 1 Rechazado
            ELSE
               CONTINUE FOREACH
            END IF
         END IF
      END FOREACH
   END IF
END FUNCTION

