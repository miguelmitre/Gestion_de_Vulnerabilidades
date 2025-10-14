###############################################################################
#Proyecto          => SAFRE                                                   #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => DIS                                                     #
#Programa          => DISB2001A                                               #
#Descripcion       => GENERA HISTORICOS (dis_det_bono_ant, dis_dep_bono_ant,  #
#                  => dis_cza_bono_ant, dis_sum_bono_ant) de BONO ISSSTE ANTI.#
#                  => GENERA ARCHIVO CONFAF                                   #
#Fecha Inicio      => 03 septiembre 2009                                      #
#Elaboro           => DMR                                                     #
#Modificaciones    => (Se agrego filtro para verificar nss que                #
#                      no son issste pero que reciben estas aportaciones)     #
#Fecha Mod         => DMR INV-2235 18 JULIO 2013                              #
#                  => se agrego validación para no considerar la fecha nula   #
#                  => para el script de obtencion de la fecha de redencion,   #
#                     ademas si se encuentra mas de 1 fecha redencion valida  #
#                     se rechaza el registro porque se afectaria saldo al NSS.#
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      vrecauda                CHAR(01),
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
         imp_red_ant_udis     DECIMAL(18,4),
         imp_red_ant_pesos    DECIMAL(16,2)
      END RECORD,

      tot_rech RECORD
         imp_red_ant_udis     DECIMAL(18,4),
         imp_red_ant_pesos    DECIMAL(16,2)
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
   CALL STARTLOG("DISB2001A.log")

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
                 "WHERE proceso_cod = 'DISB2001A' ",
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
   WHERE proceso_cod = "DISB2001A"
   AND   etapa_cod   = 1
   AND   consecutivo = vrow

   IF reg_ident.nom_archivo IS NOT NULL THEN
      CALL Proceso_principal() RETURNING vfolio

      LET vproc = vcont_acep

      LET vrech = vcont_rech

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
                    "WHERE proceso_cod = 'DISB2001A' ",
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
            " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001A' ",
            " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
            " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

      PREPARE claexe11 FROM cla_upd
      EXECUTE claexe11

      LET cla_sel = "SELECT MAX(consecutivo) ",
                    "FROM dis_ctrl_proceso ",
                    "WHERE proceso_cod = 'DISB2001A' ",
                    "AND etapa_cod = 3 " CLIPPED

      PREPARE claexe91 FROM cla_sel
      DECLARE cur_proceso91 CURSOR FOR claexe91
      OPEN cur_proceso91
         FETCH cur_proceso91 INTO vrow
      CLOSE cur_proceso91

      LET cla_upd = "UPDATE dis_ctrl_proceso ",
            " SET    dis_ctrl_proceso.folio       = ",vfolio,
            " WHERE  dis_ctrl_proceso.proceso_cod = 'DISB2001A' ",
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

   UPDATE dis_dep_bono_ant
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
                    " h.fecha_reden_ant,",
                    " h.imp_red_ant_udis,",
                    " h.imp_red_ant_pesos,",
                    " h.result_operacion,",
                    " h.consec_reg_lote ",
                    " FROM  dis_det_bono_ant h ",
                    " WHERE h.folio    = ? ",
                    " AND   h.n_unico  = ? "

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

   LET tot_acep.imp_red_ant_udis  = 0
   LET tot_acep.imp_red_ant_pesos = 0

   LET tot_rech.imp_red_ant_udis  = 0
   LET tot_rech.imp_red_ant_pesos = 0

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
       "DISB2001A",             -- proceso_cod
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
       "DISB2001A",             -- proceso_cod
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
      vfolio     INTEGER,
      vetapa_cod DECIMAL(2,0),
      vresultado CHAR(50)

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM dis_ctrl_proceso ",
                 " WHERE proceso_cod = 'DISB2001A' ",
                 " AND etapa_cod = ",vetapa_cod CLIPPED

   PREPARE claexe3 FROM cla_sel
   DECLARE cur_proceso3 CURSOR FOR claexe3
   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vrow
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_upd = " UPDATE dis_ctrl_proceso ",
                 " SET  dis_ctrl_proceso.hora_final = ","'",vhora_final,"'",",",
                 "      dis_ctrl_proceso.folio      = ",vfolio,",",
	         "      dis_ctrl_proceso.resultado  = ","'",vresultado,"'",
                 " WHERE  dis_ctrl_proceso.proceso_cod = 'DISB2001A' ",
                 " AND    dis_ctrl_proceso.etapa_cod   = ",vetapa_cod,
                 " AND    dis_ctrl_proceso.consecutivo = ",vrow CLIPPED
   PREPARE claexe35 FROM cla_upd
   EXECUTE claexe35
END FUNCTION


FUNCTION Separa_archivo()  -- ETAPA 3
   DEFINE
      ejecuta CHAR(200)

  LET ejecuta = "head -n 1 ",gparam_dis.ruta_rescate CLIPPED,"/",
             reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,
             "/cza_bono_ant"
  RUN ejecuta

  LET ejecuta = "grep '^02' ",gparam_dis.ruta_rescate CLIPPED,"/",
             reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,
             "/det_bono_ant"
  RUN ejecuta

  LET ejecuta = "grep '^08' ",gparam_dis.ruta_rescate CLIPPED,"/",
             reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,
             "/dep_bono_ant"
  RUN ejecuta

  LET ejecuta = "tail -1 ",gparam_dis.ruta_rescate CLIPPED,"/",
             reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,
             "/sum_bono_ant"
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
                 "; cut -c 17-24 cza_bono_ant > fecha_lote"
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                 "; cut -c 25-27 cza_bono_ant > lote"
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
   FROM  dis_cza_bono_ant
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


      --------------------   GENERA dis_cza_bono_ant   --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_cza_bono"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"   ---- Cabeza
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza_bono cza_bono_ant > cza1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' cza1 > cza_bono_ant "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_af -c sube_cza_bono_ant -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  cza_bono_ant cza1 "
      RUN ejecuta


      --------------------   GENERA dis_det_bono_ant   --------------------

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_det
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza_bono > vfolio_det_bono" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_detb2 con el numero de registros igual al
      -- num. registros que tiene el detalle
      LET ejecuta="cd ",gparam_dis.ruta_rescate CLIPPED,
                  "; sed -f vfolio_det_bono det_bono_ant > vfolio_detb2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det borrando lineas que no sirven
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_detb2 > vfolio_det_bono" CLIPPED 
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm vfolio_detb2"
      RUN ejecuta

      -- Se crea detb2 cortando pos 1-242 y detb3 cortando
      -- hasta del pos 244-450 y se pegan en el archivo det (Result Operacion)
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-242 det_bono_ant > detb2"   --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 244-450 det_bono_ant > detb3" --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste -d '1' detb2 detb3 > det_bono_ant"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm detb2 detb3"
      RUN ejecuta

      -- Se crea det1 pegando datos generales con el detalle 
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_det_bono det_bono_ant > detb1" CLIPPED
      RUN ejecuta

      -- Se crea det eliminando espacio en blanco que se genera cuando
      -- se pegan los 2 archivos
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' detb1 > det_bono_ant "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm detb1 vfolio_det_bono"
      RUN ejecuta

      -- Se suben los datos del detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/; dbload -d safre_af -c sube_det_bono_ant -l dbload.log;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det_bono_ant"
      RUN ejecuta


      --------------------   GENERA dis_dep_bono_ant  --------------------

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
                    "; sed -f depb01_1 dep_bono_ant > depb01_2 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f depb02_1 dep_bono_ant > depb02_2 "  CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' depb01_2 > depb01_3" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^08/d' depb02_2 > depb02_3"
      RUN ejecuta

      --  Aqui se coloca la posicion final del campo tipo_siefore del Layout 08
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-141 dep_bono_ant > depb2; mv depb2 dep_bono_ant"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste depb01_3 dep_bono_ant depb02_3 > depb1;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' depb1 > dep_bono_ant "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
"/;DBDATE=Y4MD0;export DBDATE;dbload -d safre_af -c sube_dep_bono_ant -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  vfolio_dep_bono vfolio_depb2 depb01_1 depb02_1 depb01_2 depb02_2 depb01_3 depb02_3 dep_bono_ant depb1"

      RUN ejecuta


      --------------------   GENERA dis_sum_bono_ant   --------------------

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza_bono sum_bono_ant > sumb1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' sumb1 > sum_bono_ant "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_af -c sube_sum_bono_ant -l dbload.log"

      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; rm  sumb1 sum_bono_ant fecha_lote lote vfolio_cza_bono"
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
      motivo                   CHAR(3),
      vcoduni                  CHAR(10),
      index                    CHAR(100),

      reg RECORD
         nss                   CHAR(11),
         curp                  CHAR(18),
         fecha_reden           CHAR(8),
         fecha_reden_ant       CHAR(8),
         imp_red_ant_udis      DECIMAL(18,4),
         imp_red_ant_pesos     DECIMAL(16,2),
         result_operacion      CHAR(02),
         consec_reg_lote       INTEGER
     END RECORD,

     vimp_red_ant_udis         DECIMAL(18,4),
     vimp_red_ant_pesos        DECIMAL(16,2),
     vtipo_subcuenta           CHAR(02),
     vsub_isss                 SMALLINT,
     vind_rechazo              SMALLINT,
     ejecuta                   CHAR(200),
     dcurp                     CHAR(18),
     dnss                      CHAR(11),
     cont_regi                 SMALLINT,
     ruta_archiv               CHAR(100),
     cad_freden                CHAR(10),
     xfec_reden_ant            DATE,
     xfec_reden                DATE,
     xbono_nominal             DECIMAL(18,4),
     xnominal_pesos            DECIMAL(16,2),
     xbono_rec_udis            DECIMAL(18,4),
     xbono_rec_pesos           DECIMAL(16,2),
     sol_val_bono              CHAR(100),
     hfecha_reden              DATE,
     himporte_bono             DECIMAL(16,4),
     xcad10                    CHAR(10),
     xcad8                     CHAR(8),
     rfecha_red                DATE,
     monto_ajuste              DECIMAL(18,6),
     saldo_udis                DECIMAL(18,6),
     udis_acept_cxc            DECIMAL(18,6),
     conta_fechas              SMALLINT

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
    FROM dis_det_bono_ant a
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

    PREPARE cl_dispersa FROM cla_mon
    DECLARE cur_hist CURSOR FOR cl_dispersa

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

       LET udis_acept_cxc = 0
       FOREACH cur_hist USING vfolio,dcurp
                        INTO  reg.*

          LET motivo  = "000"
          LET reg.nss = dnss
          
          SELECT count(UNIQUE fecha_reden)
          INTO   conta_fechas
          FROM   dis_det_bono
          WHERE  n_unico = reg.curp
          AND    fecha_reden <> "00010101"
          
          IF conta_fechas > 1 THEN
             LET vind_rechazo = 1
             LET motivo = "444"             ## Rechazo, 2 Fechas de Redencion afectan cálculo
          ELSE
          	 SELECT UNIQUE fecha_reden
             INTO  xcad8
             FROM  dis_det_bono             ## Se verifica exista BONO del trab
             WHERE n_unico = reg.curp
             AND    fecha_reden <> "00010101"
             
             IF STATUS = NOTFOUND THEN
                SELECT "OK"
                FROM   taa_viv_recepcion    ## Se verifica Bono de Traspasos
                WHERE  nss = reg.nss
                AND    importe_bono > 0     ## comentado en pension temp. traspasos
                GROUP BY 1
             
                IF STATUS = NOTFOUND THEN
                   LET vind_rechazo = 1
                   LET motivo = "555"
                END IF
             ELSE
--                LET xcad10 = reg.fecha_reden[5,6],"/",
--                             reg.fecha_reden[7,8],"/",
--                             reg.fecha_reden[1,4]
--           
--                LET rfecha_red = xcad10
             
                IF xcad8 <> reg.fecha_reden THEN
                   LET vind_rechazo = 1
                   LET motivo = "666"
                END IF
             END IF
          END IF

          IF vind_rechazo <> 1 THEN
             LET vimp_red_ant_udis = reg.imp_red_ant_udis / 10000

             LET cad_freden = reg.fecha_reden_ant[5,6],"/",
                              reg.fecha_reden_ant[7,8],"/",
                              reg.fecha_reden_ant[1,4]

             LET xfec_reden_ant = cad_freden

             LET sol_val_bono = "EXECUTE FUNCTION fn_valua_bono_suma (?,?)"
             PREPARE eje_sol_val FROM sol_val_bono

             DECLARE cur_sol_val CURSOR FOR eje_sol_val
             OPEN cur_sol_val USING reg.nss,
                                    xfec_reden_ant
                 FETCH cur_sol_val INTO xfec_reden,
                                        xbono_nominal,
                                        reg.imp_red_ant_udis,
                                        xnominal_pesos,
                                        reg.imp_red_ant_pesos,
                                        xbono_rec_udis,
                                        xbono_rec_pesos
             CLOSE cur_sol_val

             IF reg.imp_red_ant_udis > 0  AND  reg.imp_red_ant_pesos = 0 THEN
                LET vind_rechazo = 1
                LET motivo = "999"
             END IF

             SELECT sum(monto_en_acciones)
             INTO saldo_udis
             FROM dis_cuenta
             WHERE nss = reg.nss
             AND   subcuenta = 36

             IF (saldo_udis IS NULL) OR (saldo_udis < reg.imp_red_ant_udis) OR
                (saldo_udis < vimp_red_ant_udis)
             THEN
                LET vind_rechazo = 1
                LET motivo = "888"
             END IF

             IF xbono_nominal <> vimp_red_ant_udis THEN  # archivo vs calculo
                LET vind_rechazo = 1
                LET motivo = "777"
             END IF
          END IF
 

          LET vimp_red_ant_udis  = reg.imp_red_ant_udis
          LET vimp_red_ant_pesos = reg.imp_red_ant_pesos
          LET monto_ajuste       = xbono_nominal - reg.imp_red_ant_udis
          LET udis_acept_cxc     = udis_acept_cxc + reg.imp_red_ant_udis

          --Motivo por el cual rechazar
          IF vind_rechazo = 1 THEN
             CALL Contador_rechazo()
             CALL Sumatoria_rechazo(vimp_red_ant_udis,
                                    vimp_red_ant_pesos)
 
             UPDATE dis_det_bono_ant
             SET    result_operacion  = "02",
                    det_mot_rechazo1  = motivo,
                    imp_red_ant_udis  = 0,
                    imp_red_ant_pesos = 0
             WHERE  folio           = vfolio
             AND    n_unico         = reg.curp
             AND    consec_reg_lote = reg.consec_reg_lote
          ELSE
             CALL Contador_aceptado()
             CALL Sumatoria_aceptado(vimp_red_ant_udis,
                                     vimp_red_ant_pesos)

             UPDATE dis_det_bono_ant
             SET imp_red_ant_udis  = vimp_red_ant_udis,
                 imp_red_ant_pesos = vimp_red_ant_pesos
             WHERE folio = vfolio
             AND   consec_reg_lote = reg.consec_reg_lote

             ------------- DISPERSA APORTES EN dis_provision ------------------

             IF vimp_red_ant_udis > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_reden,xfec_reden_ant,-vimp_red_ant_pesos,-vimp_red_ant_udis,36,27,xfec_reden_ant,reg.consec_reg_lote,0,0,"BONO PENS-ANTI")

CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_reden,xfec_reden_ant,vimp_red_ant_pesos,0,30,33,xfec_reden_ant,reg.consec_reg_lote,0,0,"BONO PENS-ANTI")

CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_reden,xfec_reden_ant,0,-monto_ajuste,36,34,xfec_reden_ant,reg.consec_reg_lote,0,0,"RA-BONO PENS-ANTI")
             END IF

          END IF

          LET vimp_red_ant_udis  = 0
          LET vimp_red_ant_pesos = 0

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
                         cfecha_reden,
                         vfecha_reden_ant,
                         vmonto_pes,
                         vmonto_udis,
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
      cfecha_reden       CHAR(8),
      vfecha_reden       DATE,
      vfecha_reden_ant   DATE,
      vfecha_conv        DATE,
      vfecha_valor       DATE,
      vfecha_archivo     DATE,
      xfecha_reden       CHAR(10),
      xfecha_reden_ant   CHAR(10),
      xfecha_viv         CHAR(10),

      vmonto_pes         DECIMAL(16,2),
      vmonto_udis        DECIMAL(18,4),

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

   LET vfecha_reden     = MDY(cfecha_reden[5,6],cfecha_reden[7,8],
                              cfecha_reden[1,4])

   PREPARE claexe4 FROM cla_apo
   DECLARE cur_prov CURSOR FOR claexe4
   OPEN    cur_prov USING vnss, vsubcuenta
   FOREACH cur_prov INTO  vsiefore, vporcentaje_sie

      LET vmonto_pes = vmonto_pes * (vporcentaje_sie / 100)

      LET abo.tipo_movimiento   = vmovimiento        -- SI
      LET abo.subcuenta         = vsubcuenta         -- SI
      LET abo.folio             = vfolio             -- SI
      LET abo.consecutivo_lote  = vconsec_reg_lote
      LET abo.siefore           = vsiefore           -- SI
      LET abo.nss               = vnss               -- SI
      LET abo.curp              = vcurp              -- NO
      LET abo.folio_sua         = ""                 -- SI
      LET abo.fecha_pago        = vfecha_reden       -- NO
      LET abo.fecha_valor       = vfecha_reden_ant   -- SI
      LET abo.fecha_conversion  = vfecha_reden_ant   -- SI
      LET abo.monto_en_pesos    = vmonto_pes         -- SI
      LET abo.monto_en_acciones = vmonto_udis        -- SI

      SELECT precio_del_dia
      INTO  abo.precio_accion
      FROM  glo_valor_accion
      WHERE fecha_valuacion = vfecha_reden_ant
      AND   codigo_siefore  = vsiefore
 
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


FUNCTION Sumatoria_rechazo(vimp_red_ant_udis,
                           vimp_red_ant_pesos)

   DEFINE
      vimp_red_ant_udis     DECIMAL(18,4),
      vimp_red_ant_pesos    DECIMAL(16,2)

  LET tot_rech.imp_red_ant_udis =tot_rech.imp_red_ant_udis  + vimp_red_ant_udis
  LET tot_rech.imp_red_ant_pesos=tot_rech.imp_red_ant_pesos + vimp_red_ant_pesos
 
END FUNCTION


FUNCTION Sumatoria_aceptado(vimp_red_ant_udis,
                            vimp_red_ant_pesos)

   DEFINE
      vimp_red_ant_udis     DECIMAL(18,4),
      vimp_red_ant_pesos    DECIMAL(16,2)

  LET tot_acep.imp_red_ant_udis =tot_acep.imp_red_ant_udis  + vimp_red_ant_udis
  LET tot_acep.imp_red_ant_pesos=tot_acep.imp_red_ant_pesos + vimp_red_ant_pesos

END FUNCTION


FUNCTION Genera_confaf(vfolio)          -- ETAPA 7
   DEFINE
      vfolio                INTEGER,
      ejecuta               CHAR(200),
      tipo_ar               SMALLINT,
      rident_pago           CHAR(16)

   DEFINE
      total_red_udis        DECIMAL(18,4),
      total_red_pesos       DECIMAL(16,2),

      acept_red_udis        DECIMAL(18,4),
      acept_red_pesos       DECIMAL(16,2),

      recha_red_udis        DECIMAL(18,4),
      recha_red_pesos       DECIMAL(16,2)

   DEFINE
      vsub                  CHAR(08),         --c22-6
      vsie                  SMALLINT,         --c22-6
      vpor                  DECIMAL(16,8),    --c22-6
      vident_sie            CHAR(08),         --c22-6
      vpesos                DECIMAL(18,4),    --c22-6
      vudis                 DECIMAL(18,4),    --c22-6
      vcuantos              SMALLINT,         --c22-6.3
      vcontador_sie         SMALLINT,         --c22-6.3
      vsuma                 DECIMAL(18,4),    --c22-6
      vimporte              DECIMAL(18,4),    --c22-6
      vimporte2             DECIMAL(18,4),    --c22-6.9
      vrechazo              DECIMAL(18,4)     --c22-6

   DEFINE g_dep RECORD LIKE dis_dep_bono_ant.*

   ------------------------- TOTAL ACEPTADO APORTES ------------------------
   LET acept_red_udis        = tot_acep.imp_red_ant_udis
   LET acept_red_pesos       = tot_acep.imp_red_ant_pesos

   ------------------------- TOTAL RECHAZOS APORTES ------------------------
   LET recha_red_udis        = tot_rech.imp_red_ant_udis
   LET recha_red_pesos       = tot_rech.imp_red_ant_pesos

   -------- Prepara aportes y comisiones de tmp_res_provision  -------

   CREATE TEMP TABLE tmp_res_provision                             --c22-6
      (sub   CHAR(08),                                             --c22-6
       sie   SMALLINT,                                             --c22-6
       por   DECIMAL(12,2),
       idsie CHAR(08),
       pesos DECIMAL(18,4))                                        --c22-6


   ---- APORTES RETIRO ISSSTE ----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "41",                                                    --c22-6
          A.siefore,                                               --c22-6
          100,         #100 cambiar en porc. % (<>) en una subcta  --c22-6
          B.razon_social,                                          --c22-6
          sum(A.monto_en_pesos)                                    --c22-6
   FROM   dis_provision A, tab_siefore_local B                     --c22-6
   WHERE  A.folio     =   vfolio                                   --c22-6
   AND    A.subcuenta IN (30)                                      --c22-6
   AND    A.tipo_movimiento IN (33)                                --c22-6
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
   AND    A.tipo_movimiento IN (27)                                --c22-6
   AND    A.estado    = 5                                          --c22-6
   AND    A.siefore   = B.codigo_siefore                           --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6


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

      CASE
         WHEN vsub = "41"
            LET vrechazo = recha_red_udis
         WHEN vsub = "49"
            LET vrechazo = recha_red_udis
      END CASE

      SELECT *                                                 --c22-6
      INTO   g_dep.*
      FROM   dis_dep_bono_ant                                  --c22-6
      WHERE  folio             = vfolio                        --c22-6
      AND    ident_pago[14,15] = vsub                          --c22-6
      AND    tipo_siefore      = 0                             --c22-6

      IF vcuantos = 1 THEN                                     --c22-6.3

         ### SOLO " OPCIONES 41 RET ISSSTE y 49 BONO
         IF vsub = 41 THEN
            UPDATE dis_dep_bono_ant                            --c22-6
            SET    imp_red_ant_udis = acept_red_udis,          --c22-6.5
                   imp_red_ant_pesos= acept_red_pesos,         --c22-6.5
                   ident_siefore    = vident_sie,              --c22-6
                   tipo_siefore     = vsie                     --c22-6
            WHERE  folio            = vfolio                     --c22-6
            AND    ident_pago[14,15]= vsub                       --c22-6

{
            IF vrechazo > 0 THEN                               --c22-6.9
               LET vimporte2 = g_dep.imp_red_ant_pesos         --c22-6.9
               INSERT INTO dis_dep_bono_ant VALUES             --c22-6.9
                 (                                             --c22-6.9
                 g_dep.folio,                                  --c22-6.9
                 g_dep.tipo_registro,                          --c22-6.9
                 g_dep.ident_servicio,                         --c22-6.9
                 g_dep.ident_pago,                             --c22-6.9
                 g_dep.imp_red_ant_udis,                       --c22-6.9
                 vimporte2,                                    --c22-6.9
                 g_dep.fech_liquidacion,                       --c22-6.9
                 0,                                            --c22-6.9
                 0,                                            --c22-6.9
                 0,                                            --c22-6.9
                 0,                                            --c22-6.9
                 "",                                           --c22-6.9
                 0,                                            --c22-6.9
                 g_dep.fecha_archivo,                          --c22-6.9
                 g_dep.fecha_envio,                            --c22-6.9
                 g_dep.estado
                 )                                             --c22-6.9
            END IF                                             --c22-6.9   }
         ELSE
            UPDATE dis_dep_bono_ant                            --c22-6
            SET    imp_red_ant_udis = acept_red_udis,          --c22-6.5 
                   imp_red_ant_pesos= acept_red_pesos,         --c22-6.5 
                   ident_siefore    = vident_sie,              --c22-6
                   tipo_siefore     = vsie                     --c22-6
            WHERE  folio            = vfolio                   --c22-6
            AND    ident_pago[14,15]= vsub                     --c22-6

{DMR        IF vrechazo > 0 THEN                               --c22-6.9
 BONO UDIS     LET vimporte2 = g_dep.imp_red_ant_udis          --c22-6.9
 1 SOLO REG                                                    --c22-6.9
               INSERT INTO dis_dep_bono_ant VALUES             --c22-6.9
                 (                                             --c22-6.9
                 g_dep.folio,                                  --c22-6.9
                 g_dep.tipo_registro,                          --c22-6.9
                 g_dep.ident_servicio,                         --c22-6.9
                 g_dep.ident_pago,                             --c22-6.9
                 vimporte2,                                    --c22-6.9
                 g_dep.imp_red_ant_pesos,                      --c22-6.9
                 g_dep.fech_liquidacion,                       --c22-6.9
                 0,                                            --c22-6.9
                 0,                                            --c22-6.9
                 0,                                            --c22-6.9
                 0,                                            --c22-6.9
                 "",                                           --c22-6.9
                 0,                                            --c22-6.9
                 g_dep.fecha_archivo,                          --c22-6.9
                 g_dep.fecha_envio,                            --c22-6.9
                 g_dep.estado
                 )                                             --c22-6.9
            END IF                                             --c22-6.9  DMR }
         END IF
      ELSE                                                     --c22-6.3
         LET vcontador_sie = vcontador_sie + 1                 --c22-6.3

         IF vsub = 41 THEN
            IF vcontador_sie = 1 THEN                          --c22-6.3

               IF vsie = 2 THEN                                --c22-6.5
                  LET vrechazo = 0                             --c22-6.5
               END IF                                          --c22-6.5

               SELECT sum(monto_en_acciones) INTO vudis FROM dis_provision b
               WHERE b.folio = vfolio
               AND b.subcuenta = 36
               AND b.tipo_movimiento = 27
               AND b.nss IN (SELECT a.nss FROM cta_regimen a
                             WHERE a.nss = b.nss
                             AND a.subcuenta = 30
                             AND a.codigo_siefore = vsie)

               LET vudis = vudis * (-1)

               UPDATE dis_dep_bono_ant                         --c22-6.3
               SET    imp_red_ant_udis = vudis,  --0           --c22-6.5
                      imp_red_ant_pesos= vpesos,               --c22-6.5
                      ident_siefore    = vident_sie,           --c22-6.3
                      tipo_siefore     = vsie                  --c22-6.3
               WHERE  folio            = vfolio                --c22-6.3
               AND    ident_pago[14,15]= vsub                  --c22-6.3
               AND    tipo_siefore     = 0                     --c22-6.3

            {  IF vrechazo > 0 THEN                            --c22-6.9
                  LET vimporte2 = g_dep.imp_red_ant_pesos      --c22-6.9
                  INSERT INTO dis_dep_bono_ant VALUES          --c22-6.9
                    (                                          --c22-6.9
                    g_dep.folio,                               --c22-6.9
                    g_dep.tipo_registro,                       --c22-6.9
                    g_dep.ident_servicio,                      --c22-6.9
                    g_dep.ident_pago,                          --c22-6.9
                    g_dep.imp_red_ant_udis,                    --c22-6.9
                    vimporte2,                                 --c22-6.9
                    g_dep.fech_liquidacion,                    --c22-6.9
                    0,                                         --c22-6.9
                    0,                                         --c22-6.9
                    0,                                         --c22-6.9
                    0,                                         --c22-6.9
                    "",                                        --c22-6.9
                    0,                                         --c22-6.9
                    g_dep.fecha_archivo,                       --c22-6.9
                    g_dep.fecha_envio,                         --c22-6.9
                    g_dep.estado                               --c22-6.9
                    )
               END IF      }
            ELSE

           --  IF vsie = 2 THEN    -- OR vsie= 3 THEN          --c22-119
           --     LET vrechazo = 0                             --c22-119
           --  END IF                                          --c22-119

               IF vrechazo > 0 THEN                            --c22-119
                  LET vimporte   = g_dep.imp_red_ant_pesos     --c22-119
               ELSE                                            --c22-119
                  LET vimporte   = g_dep.imp_red_ant_pesos     --c22-119
               END IF                                          --c22-119

               IF vsie >= 2 THEN                               --c22-119
                  LET vrechazo = 0                             --c22-119
               END IF                                          --c22-119

               SELECT sum(monto_en_acciones) INTO vudis FROM dis_provision b
               WHERE b.folio = vfolio
               AND b.subcuenta = 36
               AND b.tipo_movimiento = 27
               AND b.nss IN (SELECT a.nss FROM cta_regimen a
                             WHERE a.nss = b.nss
                             AND a.subcuenta = 30
                             AND a.codigo_siefore = vsie)

               LET vudis = vudis * (-1)

               INSERT INTO dis_dep_bono_ant VALUES             --c22-6
                 (                                             --c22-6
                 g_dep.folio,                                  --c22-6
                 g_dep.tipo_registro,                          --c22-6
                 g_dep.ident_servicio,                         --c22-6
                 g_dep.ident_pago,                             --c22-6
                 vudis,                       --0,             --c22-6
                 vpesos,                                       --c22-6
                 g_dep.fech_liquidacion,                       --c22-6
                 0,                                            --c22-6
                 0,                                            --c22-6
                 0,                                            --c22-6
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
         ELSE
            IF vcontador_sie = 1 THEN                          --c22-6.3

               IF vsie = 2 THEN                                --c22-6.5
                  LET vrechazo = 0                             --c22-6.5
               END IF                                          --c22-6.5

               UPDATE dis_dep_bono_ant                         --c22-6.3
               SET    imp_red_ant_udis = acept_red_udis,       --c22-6.5
                      imp_red_ant_pesos= acept_red_pesos,      --c22-6.5
                      ident_siefore    = vident_sie,           --c22-6.3
                      tipo_siefore     = vsie                  --c22-6.3 
               WHERE  folio            = vfolio                --c22-6.3
               AND    ident_pago[14,15]= vsub                  --c22-6.3
               AND    tipo_siefore     = 0                     --c22-6.3

          {    IF vrechazo > 0 THEN                            --c22-6.9
                  LET vimporte2 = g_dep.imp_red_ant_udis       --c22-6.9
                  INSERT INTO dis_dep_bono_ant VALUES          --c22-6.9
                    (                                          --c22-6.9
                    g_dep.folio,                               --c22-6.9
                    g_dep.tipo_registro,                       --c22-6.9
                    g_dep.ident_servicio,                      --c22-6.9
                    g_dep.ident_pago,                          --c22-6.9
                    vimporte2,                                 --c22-6.9
                    g_dep.imp_red_ant_pesos,                   --c22-6.9
                    g_dep.fech_liquidacion,                    --c22-6.9
                    0,                                         --c22-6.9
                    0,                                         --c22-6.9
                    0,                                         --c22-6.9
                    0,                                         --c22-6.9
                    "",                                        --c22-6.9
                    0,                                         --c22-6.9
                    g_dep.fecha_archivo,                       --c22-6.9
                    g_dep.fecha_envio,                         --c22-6.9
                    g_dep.estado                               --c22-6.9
                    )
               END IF    }
            ELSE

           --  IF vsie = 2 THEN    -- OR vsie= 3 THEN          --c22-119
           --     LET vrechazo = 0                             --c22-119
           --  END IF                                          --c22-119

               IF vrechazo > 0 THEN                            --c22-119
                  LET vimporte   = g_dep.imp_red_ant_udis      --c22-119
               ELSE                                            --c22-119
                  LET vimporte   = g_dep.imp_red_ant_udis      --c22-119
               END IF                                          --c22-119

               IF vsie >= 2 THEN                               --c22-119
                  LET vrechazo = 0                             --c22-119
               END IF                                          --c22-119

               INSERT INTO dis_dep_bono_ant VALUES             --c22-6
                 (                                             --c22-6
                 g_dep.folio,                                  --c22-6
                 g_dep.tipo_registro,                          --c22-6
                 g_dep.ident_servicio,                         --c22-6
                 g_dep.ident_pago,                             --c22-6
                 vimporte,                                     --c22-6
                 vpesos,                                       --c22-6
                 g_dep.fech_liquidacion,                       --c22-6
                 0,                                            --c22-6
                 0,                                            --c22-6
                 0,                                            --c22-6
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
      FROM  dis_dep_bono_ant
      WHERE folio = vfolio
      AND   imp_red_ant_udis  = 0
      AND   imp_red_ant_pesos = 0
      AND   ident_pago        = g_dep.ident_pago

      FOREACH curctar INTO rident_pago
         IF rident_pago[14,15] = "41" THEN
            UPDATE dis_dep_bono_ant
            SET imp_red_ant_udis  = 0,
                imp_red_ant_pesos = 0,
                ident_siefore     = vident_sie
            WHERE folio      = vfolio
            AND   ident_pago = rident_pago
         ELSE
            UPDATE dis_dep_bono_ant
            SET imp_red_ant_udis  = 0,
                imp_red_ant_pesos = 0,
                ident_siefore     = vident_sie
            WHERE folio      = vfolio
            AND   ident_pago = rident_pago
         END IF
      END FOREACH
   END FOREACH

   CALL Genera_salidas(vfolio,
                       acept_red_udis,
                       acept_red_pesos,
                       recha_red_udis,
                       recha_red_pesos
                       )

   # Para tipo 1 y como valor default
   LET vnom_archivo= new_cadena CLIPPED,".BONACONF" CLIPPED

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; cat cza rech dep sum > ",vnom_archivo CLIPPED
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; chmod 777 ",vnom_archivo CLIPPED
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; rm cza rech dep sum "
   RUN ejecuta
END FUNCTION

 
FUNCTION Genera_salidas(vfolio,
                        acept_red_udis,
                        acept_red_pesos,
                        recha_red_udis,
                        recha_red_pesos
                        )

   DEFINE
      vfolio                 INTEGER,
      acept_red_udis         DECIMAL(18,4),
      acept_red_pesos        DECIMAL(16,2),
      recha_red_udis         DECIMAL(18,4),
      recha_red_pesos        DECIMAL(16,2)
 
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
      tipo_pension           CHAR(2),
      fecha_reden            CHAR(8),
      fecha_reden_ant        CHAR(8),
      imp_red_ant_udis       DECIMAL(18,4),
      imp_red_ant_pesos      DECIMAL(16,2),
      result_operacion       CHAR(2),
      det_mot_rechazo1       CHAR(3),
      det_mot_rechazo2       CHAR(3),
      det_mot_rechazo3       CHAR(3)
   END RECORD

   DEFINE reg_dep RECORD
      tipo_registro          CHAR(02),
      ident_servicio         CHAR(02),
      ident_pago             CHAR(16),
      imp_red_ant_udis       DECIMAL(18,4),
      imp_red_ant_pesos      DECIMAL(16,2),
      fech_liquidacion       DATE,
      filler1                CHAR(18),
      filler2                CHAR(16),
      filler3                CHAR(18),
      filler4                CHAR(16),
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
      impt_tot_red_udis      DECIMAL(18,4),
      impt_tot_red_pesos     DECIMAL(16,2),
      num_de_reg_aport       CHAR(9),
      num_cuentas_xpagar     CHAR(6)
   END RECORD,

   vimpt_tot_red_udis        DECIMAL(18,4),
   vimpt_tot_red_pesos       DECIMAL(16,2)

   DEFINE xident_siefore     CHAR(08),      --c22-6.6
          xtipo_siefore      SMALLINT,      --c22-6.6
          vimpt_acept        DECIMAL(16,2), --c22-6.6
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
   FROM  dis_cza_bono_ant
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
      a.tipo_pension,
      a.fecha_reden,
      a.fecha_reden_ant,
      a.imp_red_ant_udis,
      a.imp_red_ant_pesos,
      a.result_operacion,
      a.det_mot_rechazo1,
      a.det_mot_rechazo2,
      a.det_mot_rechazo3
   FROM  dis_det_bono_ant a
   WHERE a.folio = vfolio
--   AND   a.result_operacion = "02"              ---- Rechazos
   ORDER BY a.consec_reg_lote, a.tipo_registro

   DISPLAY "GENERANDO RECH..." #---AT 12,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/rech"
   START REPORT rep_rech TO vreporte
      FOREACH cur_rech INTO reg_rech.*
         LET reg_rech.det_mot_rechazo1  = "000"
         LET reg_rech.det_mot_rechazo2  = "000"
         LET reg_rech.det_mot_rechazo3  = "000"

         OUTPUT TO REPORT rep_rech(reg_rech.*)
      END FOREACH
   FINISH REPORT rep_rech

   --------------- GENERA RESPUESTA DEPOSITOS ---------------------

   DECLARE cur_41 CURSOR FOR                                     --c22-6.6
   SELECT  ident_siefore,                                        --c22-6.6
           tipo_siefore                                          --c22-6.6
   FROM    dis_dep_bono_ant                                      --c22-6.6
   WHERE   folio = vfolio                                        --c22-6.6
   AND     ident_pago[14,15] = "41"                              --c22-6.6

   FOREACH cur_41 INTO xident_siefore,xtipo_siefore              --c22-6.6

      SELECT ident_siefore,tipo_siefore                          --c22-6.6
      INTO   xident_siefore,vtipo_sie                            --c22-6.6
      FROM   dis_dep_bono_ant                                    --c22-6.6
      WHERE  folio = vfolio                                      --c22-6.6
      AND    ident_pago[14,15] = "11"                            --c22-6.6
      AND    tipo_siefore = 0                                    --c22-6.6

      IF STATUS <> NOTFOUND THEN                                 --c22-6.6
         IF vtipo_sie = 0 THEN                                   --c22-6.6
            UPDATE dis_dep_bono_ant                              --c22-6.6
            SET    ident_siefore = xident_siefore,               --c22-6.6
                   tipo_siefore  = xtipo_siefore
            WHERE  folio             = vfolio                    --c22-6.6
            AND    ident_pago[14,15] = "11"                      --c22-6.6
            AND    tipo_siefore      = 0                         --c22-6.6
         END IF                                                  --c22-6.6
      END IF                                                     --c22-6.6
   END FOREACH                                                   --c22-6.6

   DECLARE cur_49 CURSOR FOR                                     --c22-6.6
   SELECT  ident_siefore,                                        --c22-6.6
           tipo_siefore                                          --c22-6.6
   FROM    dis_dep_bono_ant                                      --c22-6.6
   WHERE   folio = vfolio                                        --c22-6.6
   AND     ident_pago[14,15] = "49"                              --c22-6.6

   FOREACH cur_49 INTO xident_siefore,xtipo_siefore              --c22-6.6

      SELECT ident_siefore,tipo_siefore                          --c22-6.6
      INTO   xident_siefore,vtipo_sie                            --c22-6.6
      FROM   dis_dep_bono_ant                                    --c22-6.6
      WHERE  folio = vfolio                                      --c22-6.6
      AND    ident_pago[14,15] = "12"                            --c22-6.6
      AND    tipo_siefore = 0                                    --c22-6.6

      IF STATUS <> NOTFOUND THEN                                 --c22-6.6
         IF vtipo_sie = 0 THEN                                   --c22-6.6
            UPDATE dis_dep_bono_ant                              --c22-6.6
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
      imp_red_ant_udis,
      imp_red_ant_pesos,
      fech_liquidacion,
      filler1,
      filler2,
      filler3,
      filler4,
      ident_siefore,
      tipo_siefore
   FROM dis_dep_bono_ant
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
          impt_tot_red_udis,
          impt_tot_red_udis,
          num_de_reg_aport,
          num_cuentas_xpagar
   FROM   dis_sum_bono_ant
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

         UPDATE dis_sum_bono_ant
         SET    impt_tot_red_udis    = acept_red_udis,
                impt_tot_red_pesos   = acept_red_pesos
         WHERE  CURRENT OF cur_sum

         OUTPUT TO REPORT rep_sum(reg_sum.*,
                                  acept_red_udis,
                                  acept_red_pesos,
                                  recha_red_udis,
                                  recha_red_pesos
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
      tipo_pension          CHAR(2),
      fecha_reden           CHAR(8),
      fecha_reden_ant       CHAR(8),
      imp_red_ant_udis      DECIMAL(18,4),
      imp_red_ant_pesos     DECIMAL(16,2),
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

   IF reg_rech.result_operacion = "02" THEN
      LET reg_rech.imp_red_ant_udis  = 0
      LET reg_rech.imp_red_ant_pesos = 0
   END IF

   LET cimpt_bono = reg_rech.imp_red_ant_udis USING '&&&&&&&&&&&&.&&&&'
   LET cimpt_dbmx = reg_rech.imp_red_ant_pesos USING '&&&&&&&&&&&&.&&'

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
                     reg_rech.tipo_pension,
                     reg_rech.fecha_reden,
                     reg_rech.fecha_reden_ant,
                     ximpt_bono,
                     ximpt_dbmx,
                     reg_rech.result_operacion,  --- result_operacion,
                     reg_rech.det_mot_rechazo1,
                     reg_rech.det_mot_rechazo2,
                     reg_rech.det_mot_rechazo3,
                     198 space
END REPORT


REPORT rep_dep(reg_dep)
   DEFINE reg_dep RECORD
      tipo_registro     CHAR(02),
      ident_servicio    CHAR(02),
      ident_pago        CHAR(16),
      imp_red_ant_udis  DECIMAL(18,4),
      imp_red_ant_pesos DECIMAL(16,2),
      fech_liquidacion  DATE,
      filler1           CHAR(18),
      filler2           CHAR(16),
      filler3           CHAR(18),
      filler4           CHAR(16),
      ident_siefore     CHAR(08),
      tipo_siefore      SMALLINT
   END RECORD

   DEFINE precio_udi     LIKE glo_valor_accion.precio_del_dia,

   c19_imp_red_ant_udis  CHAR(19),
   c18_imp_red_ant_udis  CHAR(18),
   c17_imp_red_ant_pesos CHAR(17),
   c16_imp_red_ant_pesos CHAR(16),
   xfecha_liquidacion    CHAR(10),
   vfecha_liquidacion    CHAR(08)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         IF reg_dep.ident_pago[14,15] = "49" THEN
            LET reg_dep.ident_siefore = "        "
            LET reg_dep.tipo_siefore      = 0
            LET reg_dep.imp_red_ant_pesos = 0
         END IF

         LET vnum_ctas_xpagar = vnum_ctas_xpagar + 1

         SELECT precio_del_dia
         INTO precio_udi
         FROM glo_valor_accion
         WHERE fecha_valuacion = reg_dep.fech_liquidacion
         AND   codigo_siefore  = 13

--       LET reg_dep.imp_red_ant_udis = reg_dep.imp_red_ant_pesos / precio_udi

         IF reg_dep.imp_red_ant_udis IS NULL THEN
            LET c18_imp_red_ant_udis = "000000000000000000"
         ELSE
            LET c19_imp_red_ant_udis=reg_dep.imp_red_ant_udis USING "&&&&&&&&&&&&&&.&&&&"
            LET c18_imp_red_ant_udis=c19_imp_red_ant_udis[01,14],c19_imp_red_ant_udis[16,19]
         END IF

         IF reg_dep.imp_red_ant_pesos IS NULL THEN
            LET c16_imp_red_ant_pesos = "0000000000000000"
         ELSE
            LET c17_imp_red_ant_pesos=reg_dep.imp_red_ant_pesos USING "&&&&&&&&&&&&&&.&&"
            LET c16_imp_red_ant_pesos=c17_imp_red_ant_pesos[01,14],c17_imp_red_ant_pesos[16,17]
         END IF

         IF reg_dep.filler1 IS NULL OR reg_dep.filler1 = 0 THEN
            LET reg_dep.filler1 = "                  "
         END IF
         IF reg_dep.filler2 IS NULL OR reg_dep.filler2 = 0 THEN
            LET reg_dep.filler2 = "                "
         END IF
         IF reg_dep.filler3 IS NULL OR reg_dep.filler3 = 0 THEN
            LET reg_dep.filler3 = "                  "
         END IF
         IF reg_dep.filler4 IS NULL OR reg_dep.filler4 = 0 THEN
            LET reg_dep.filler4 = "                "
         END IF

         LET xfecha_liquidacion = reg_dep.fech_liquidacion
         LET vfecha_liquidacion = xfecha_liquidacion[7,10],
                                  xfecha_liquidacion[1,02],
                                  xfecha_liquidacion[4,05]

         PRINT COLUMN 01,reg_dep.tipo_registro,
                         reg_dep.ident_servicio,
                         reg_dep.ident_pago,
                         c18_imp_red_ant_udis,
                         c16_imp_red_ant_pesos,
                         vfecha_liquidacion,
                         reg_dep.filler1,
                         reg_dep.filler2,
                         reg_dep.filler3,
                         reg_dep.filler4,
                         reg_dep.ident_siefore,
                         reg_dep.tipo_siefore USING '&&&',
                         309 space
END REPORT


REPORT rep_sum(reg_sum,
               acept_red_udis,
               acept_red_pesos,
               recha_red_udis,
               recha_red_pesos
               )

   DEFINE
      acept_red_udis         CHAR(18),
      acept_red_pesos        CHAR(16),
      recha_red_udis         CHAR(18),
      recha_red_pesos        CHAR(16)

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
      impt_tot_red_udis      DECIMAL(18,4),
      impt_tot_red_pesos     DECIMAL(16,2),
      num_de_reg_aport       CHAR(9),
      num_cuentas_xpagar     CHAR(6)
   END RECORD,

   c18_impt_tot_red_udis     ,
   c18_impt_tot_red_pesos    CHAR(18),

   c19_impt_tot_red_udis     ,
   c19_impt_tot_red_pesos    CHAR(19),

   d16_impt_tot_red_udis     DECIMAL(18,4),
   d16_impt_tot_red_pesos    DECIMAL(16,2)


   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW

         IF reg_sum.impt_tot_red_udis IS NULL THEN
            LET reg_sum.impt_tot_red_udis = 0
         END IF
         IF reg_sum.impt_tot_red_pesos IS NULL THEN
            LET reg_sum.impt_tot_red_pesos = 0
         END IF

         LET reg_sum.impt_tot_red_udis  = acept_red_udis * 10000
         LET reg_sum.impt_tot_red_pesos = acept_red_pesos * 100

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
                     reg_sum.impt_tot_red_udis  USING '&&&&&&&&&&&&&&&&&&',
                     reg_sum.impt_tot_red_pesos USING '&&&&&&&&&&&&&&&&',
                     vnum_reg                   USING '&&&&&&&&&',
                     vnum_ctas_xpagar           USING '&&&&&&',
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

   DEFINE v_fecha_log    DATETIME YEAR TO SECOND
   DEFINE v_folio        INTEGER
   DEFINE reg_ruta       RECORD LIKE seg_modulo.*

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

               IF vcod_afore = 564 THEN      ####MET 1 campo mas cta_ctr_reg_ind
                  INSERT INTO cta_ctr_reg_ind
                  VALUES (curpa,nssa," ",2,"","01","02",today,user)
               ELSE
                  INSERT INTO cta_ctr_reg_ind
                  VALUES (curpa,nssa," ",2,"01","02",today,user)
               END IF

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
                  ERROR "Inconsistencia en Datos,NSS issste no tiene NTI ",curpa

                  IF vcod_afore = 564 THEN    ###MET 1 campo mas cta_ctr_reg_ind
                     INSERT INTO cta_ctr_reg_ind
                     VALUES (curpa,nssa," ",2,"","01","02",today,user)
                  ELSE
                     INSERT INTO cta_ctr_reg_ind
                     VALUES (curpa,nssa," ",2,"01","02",today,user)
                  END IF

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

