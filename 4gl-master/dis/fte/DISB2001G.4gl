###############################################################################
#Proyecto          => SAFRE                                                   #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => DIS                                                     #
#Programa          => DISB2001G                                               #
#Descripcion       => GENERA HISTORICOS (dis_det_devorec,dis_dep_devorec,     #
#                  => dis_cza_devorec,dis_sum_devorec)                        #
#                  => GENERA ARCHIVO RESPUESTA                                #
#Fecha Inicio      => 21 Julio 2011                                           #
#Elaboro           => DMR                                                     #
#Modificaciones    =>                                                         #
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
      vfecha_lote_devo        CHAR(08),
      xfecha_lote_devo        DATE,
      vtipo_reporte           CHAR(01),
      hora_inicial            CHAR(08),
      hora_final              CHAR(08),
      cla_sel                 CHAR(1000),
      cla_apo                 CHAR(500),
      cla_mon                 CHAR(1000),
      cla_com                 CHAR(500),
      cla_upd                 CHAR(1500),

      tot_acep RECORD
         impt_cv_isss_tra     DECIMAL(17,2),
         impt_cv_isss_pat     DECIMAL(17,2),
         impt_cs_isss         DECIMAL(17,2),
         impt_ahr_sol_dep     DECIMAL(17,2),
         impt_bono            DECIMAL(17,2),
         valor_nominal_bono   DECIMAL(17,2)
      END RECORD,

      tot_rech RECORD
         impt_cv_isss_tra     DECIMAL(17,2),
         impt_cv_isss_pat     DECIMAL(17,2),
         impt_cs_isss         DECIMAL(17,2),
         impt_ahr_sol_dep     DECIMAL(17,2),
         impt_bono            DECIMAL(17,2),
         valor_nominal_bono   DECIMAL(17,2)
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

   DEFINE g_com RECORD
      subcuenta               SMALLINT,
      val_porcentaje          DECIMAL(16,6)
   END RECORD

   DEFINE gfecha_recepcion    DATE

   DEFINE vconsec             INTEGER,
          vrow                INTEGER

   DEFINE vfecha_aux          DATE,
	  vbim_cert           INTEGER,
	  vbim_pago           INTEGER

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
   CALL STARTLOG("DISB2001G.log")

   DISPLAY " "
   DISPLAY ".1"

   LET vbim_cert = 0
   LET vbim_pago = 0

   LET reg_ident.pid             = ARG_VAL(1)
   LET reg_ident.proceso_cod     = ARG_VAL(2)
   LET reg_ident.opera_cod       = ARG_VAL(3)
   LET reg_ident.nom_archivo     = ARG_VAL(4)
   LET reg_ident.tipo            = ARG_VAL(5)
   LET reg_ident.fecha_recepcion = ARG_VAL(6)
   LET reg_ident.val_cs          = ARG_VAL(7)

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
                 "WHERE proceso_cod = 'DISB2001G' ",
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
   WHERE proceso_cod = "DISB2001G"
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
                    "WHERE proceso_cod = 'DISB2001G' ",
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
            " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001G' ",
            " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
            " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

      PREPARE claexe11 FROM cla_upd
      EXECUTE claexe11

      LET cla_sel = "SELECT MAX(consecutivo) ",
                    "FROM dis_ctrl_proceso ",
                    "WHERE proceso_cod = 'DISB2001G' ",
                    "AND etapa_cod = 3 " CLIPPED

      PREPARE claexe91 FROM cla_sel
      DECLARE cur_proceso91 CURSOR FOR claexe91

      OPEN cur_proceso91
         FETCH cur_proceso91 INTO vrow
      CLOSE cur_proceso91

      LET cla_upd = "UPDATE dis_ctrl_proceso ",
            " SET    dis_ctrl_proceso.folio       = ",vfolio,
            " WHERE  dis_ctrl_proceso.proceso_cod = 'DISB2001G' ",
            " AND    dis_ctrl_proceso.etapa_cod   = 3 ",        
            " AND    dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

      PREPARE claexe12 FROM cla_upd
      EXECUTE claexe12
   END IF
END MAIN


FUNCTION Proceso_principal()
   DEFINE 
      ejecuta   CHAR(200),
      vfolio    INTEGER

   CALL Inicializa()

   CALL Lectura()

   ERROR "PROCESANDO INFORMACION...",reg_ident.nom_archivo
 
   CALL Ingresa_etapa(vfolio,3,"Separa archivo")
   CALL Separa_archivo()                                  -- ETAPA 3
   CALL Actualiza_etapa(vfolio,3,"Separa archivo")

   CALL Ingresa_etapa(vfolio,4,"Carga historicos")
   CALL Sube_datos() RETURNING vfolio                     -- ETAPA 4
   CALL Actualiza_etapa(vfolio,4,"Carga historicos")

   UPDATE dis_dep_devorec
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


      LET cla_com = "SELECT a.tipo, ",                          --c22-6
                           "b.val_porcentaje, ",                --c22-6
                           "c.codigo_siefore, ",                --c22-6
                           "c.porcentaje, ",                    --c22-6
                           "b.tipo_comision ",                  --c22-6
                    "FROM   tab_movimiento a, ",                --c22-6
                           "dis_val_comision b, ",              --c22-6
                           "cta_regimen c ",                    --c22-6
                    "WHERE  a.codigo     = b.tipo_comision ",   --c22-6
                    " AND   b.subcuenta     = ? ",              --c22-6
                    " AND   b.tipo_comision <> 110 ",           --c22-6
                    " AND   b.antiguedad_hasta >= ? ",          --c22-6
                    " AND   b.antiguedad_desde <= ? ",          --c22-6
                    " AND   b.subcuenta = c.subcuenta ",        --c22-6
                    " AND   c.nss       = ? "                   --c22-6

      LET cla_mon = "SELECT h.n_seguro,",
                    " h.n_unico,",
                    " h.sel_trab,",
                    " h.ind_trab_bono,",
                    " h.impt_cv_isss_tra,",
                    " h.impt_cv_isss_pat,",
                    " h.impt_cs_isss,",
                    " h.impt_ahr_sol_dep,",
                    " h.impt_bono,",
                    " h.valor_nominal_bono / 100,",
                    " h.result_operacion,",
                    " h.consec_reg_lote,",
                    " h.n_rfc",
                    " FROM   dis_det_devorec h ",
                    " WHERE  h.folio = ? ",
                    " AND    h.n_unico = ? "

      --- crea tabla tmporal para contestar confaf (dis_dep_devorec) ---c22-6

      CREATE TEMP TABLE tmp_dis_provision    --c22-6
         (                                   --c22-6
           sie   SMALLINT,                   --c22-6
           sub   SMALLINT,                   --c22-6
           mov   SMALLINT,                   --c22-6
           por   DECIMAL(12,2),              --c22-6
           idsie CHAR(08),                   --c22-6
           pesos DECIMAL(16,6)               --c22-6
         );

      CREATE TEMP TABLE tmp_siefore_local    --c22-6
         (                                   --c22-6
          codigo_siefore  SMALLINT,          --c22-6
          razon_social    CHAR(08)           --c22-6
         );                                  --c22-6

      INSERT INTO tmp_siefore_local          --c22-6
      SELECT codigo_siefore,                 --c22-6
             razon_social                    --c22-6
      FROM   tab_siefore_local               --c22-6

      --Generamos aportes y archivo de respuesta

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
   INTO  gparam_dis.*,gusuario
   FROM  seg_modulo
   WHERE modulo_cod = "dis"

   SELECT codigo_afore
   INTO vcod_afore
   FROM tab_afore_local

   LET tot_acep.impt_cv_isss_tra     = 0
   LET tot_acep.impt_cv_isss_pat     = 0
   LET tot_acep.impt_cs_isss         = 0
   LET tot_acep.impt_ahr_sol_dep     = 0
   LET tot_acep.impt_bono            = 0
   LET tot_acep.valor_nominal_bono   = 0

   LET tot_rech.impt_cv_isss_tra     = 0
   LET tot_rech.impt_cv_isss_pat     = 0
   LET tot_rech.impt_cs_isss         = 0
   LET tot_rech.impt_ahr_sol_dep     = 0
   LET tot_rech.impt_bono            = 0
   LET tot_rech.valor_nominal_bono   = 0

   LET vcont_acep = 0
   LET vcont_rech = 0

   LET vnum_reg = 0
   LET vnum_ctas_xpagar = 0
END FUNCTION


FUNCTION Inserta_proceso()
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB2001G",             -- proceso_cod
       1,                       -- etapa_cod     -- LECTURA
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       reg_ident.nom_archivo,   -- parametro1
       vrecauda,                -- parametro2
       reg_ident.fecha_recepcion,--parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       gusuario,                -- usuario
       0)

   IF STATUS < 0 THEN
      DISPLAY "Program stopped"
      ERROR "ERROR AL INSERTAR EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Obtiene_nombre_arch(long, cadena)
   DEFINE
      long ,i        SMALLINT,
      cadena         CHAR(800),
      cadena_limpia  CHAR(800)

   LET cadena_limpia = ' '

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
       "DISB2001G",             -- proceso_cod
       vetapa_cod,              -- etapa_cod   
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_bat.parametro1,        -- parametro1
       NULL,                    -- parametro2
       reg_ident.fecha_recepcion,--parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       vfolio,                  -- folio 
       vresultado,              -- resultado
       gusuario,                -- usuario
       0)

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso etapa ",vetapa_cod," ",STATUS
      SLEEP 4
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
                 " WHERE proceso_cod = 'DISB2001G' ",
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
                 " WHERE  dis_ctrl_proceso.proceso_cod  = 'DISB2001G' ",
                 " AND    dis_ctrl_proceso.etapa_cod    = ",vetapa_cod,
                 " AND    dis_ctrl_proceso.consecutivo  = ",vrow CLIPPED

   PREPARE claexe35 FROM cla_upd
   EXECUTE claexe35
END FUNCTION


FUNCTION Separa_archivo()  -- ETAPA 3
  DEFINE
     ejecuta CHAR(200)

  LET ejecuta = "head -n 1 ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/cza_devo"
  RUN ejecuta

  LET ejecuta = "grep '^02' ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/det_devo"
  RUN ejecuta

  LET ejecuta = "grep '^08' ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/dep_devo"
  RUN ejecuta

  LET ejecuta = "tail -1 ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/sum_devo"
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
      cfecha_envio        CHAR(10),
      vfecha_envio        CHAR(08)

   LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                 "; cut -c 17-24 cza_devo > fecha_lote_cr"
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                 "; cut -c 25-27 cza_devo > lote_cr"
   RUN ejecuta

   WHENEVER ERROR CONTINUE
      DROP TABLE fecha_lote
      DROP TABLE tmp_lote         --c22-11
   WHENEVER ERROR STOP

   CREATE TEMP TABLE fecha_lote
      (fecha_lote CHAR(08))
      
   CREATE TEMP TABLE tmp_lote     --c22-11
      (lote CHAR(03))

   LET ejecuta = gparam_dis.ruta_rescate CLIPPED,"/fecha_lote_cr" CLIPPED
   LOAD FROM ejecuta INSERT INTO fecha_lote

   LET ejecuta = gparam_dis.ruta_rescate CLIPPED,"/lote_cr" CLIPPED
   LOAD FROM ejecuta INSERT INTO tmp_lote  --c22-11

   SELECT fecha_lote
   INTO   vfecha_lote_devo
   FROM   fecha_lote
 
   SELECT lote
   INTO   vlote
   FROM   tmp_lote

   SELECT "X"
   FROM  dis_cza_devorec
   WHERE fech_creac_lote = vfecha_lote_devo
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


      --------------------   GENERA dis_cza_devorec  --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_cza_devo"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"   ---- Cabeza
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza_devo cza_devo > cza1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' cza1 > cza_devo "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_af -c sube_cza_devorec -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  cza_devo cza1"
      RUN ejecuta


      --------------------   GENERA dis_det_devorec  --------------------

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_det
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza_devo > vfolio_det_devo" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det2 con el numero de registros igual al
      -- num. registros que tiene el detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f vfolio_det_devo det_devo > vfolio_det2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det borrando lineas que no sirven
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_det2 > vfolio_det_devo" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm vfolio_det2"
      RUN ejecuta

      -- Se crea det2 cortando pos 1-280 y det3 cortando
      -- hasta del pos 282-450 y se pegan en el archivo det (Result Operacion)
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-280 det_devo > det2"   --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 282-450 det_devo > det3" --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste -d '1' det2 det3 > det_devo"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det2 det3"
      RUN ejecuta

      -- Se crea det1 pegando datos generales con el detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_det_devo det_devo > det1" CLIPPED
      RUN ejecuta

      -- Se crea det eliminando espacio en blanco que se genera cuando
      -- se pegan los 2 archivos
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' det1 > det_devo "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det1 vfolio_det_devo"
      RUN ejecuta  

      -- Se suben los datos del detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;dbload -d safre_af -c sube_det_devorec -l dbload.log;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det_devo"
      RUN ejecuta


      --------------------   GENERA dis_dep_devorec --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_dep_devo"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "D"   ---- deposito
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      LET cfecha_envio = TODAY
      LET vfecha_envio = cfecha_envio[7,10],
                         cfecha_envio[1,02],
                         cfecha_envio[4,05]

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_dep2"
      START REPORT salida2 TO vreporte
         OUTPUT TO REPORT salida2(vfecha_lote_devo,vfecha_envio)
      FINISH REPORT salida2

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_dep_devo > dep01_1 " CLIPPED
      RUN ejecuta     

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_dep2 > dep02_1 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f dep01_1 dep_devo > dep01_2 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f dep02_1 dep_devo > dep02_2 "  CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' dep01_2 > dep01_3" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^08/d' dep02_2 > dep02_3"
      RUN ejecuta

      --  Aqui se coloca la posicion final del campo tipo_siefore del Layout 08
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-87 dep_devo > dep2; mv dep2 dep_devo " --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste dep01_3 dep_devo dep02_3 > dep1;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' dep1 > dep_devo "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
"/;DBDATE=Y4MD0;export DBDATE;dbload -d safre_af -c sube_dep_devorec -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  vfolio_dep_devo vfolio_dep2 dep01_1 dep02_1 dep01_2 dep02_2 dep01_3 dep02_3 dep_devo dep1"
      RUN ejecuta


      --------------------   GENERA dis_sum_devorec  --------------------

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza_devo sum_devo > sum1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' sum1 > sum_devo "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_af -c sube_sum_devorec -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; rm  sum1 sum_devo fecha_lote_cr lote_cr vfolio_cza_devo"
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
         sel_trab              CHAR(01),
         ind_trab_bono         CHAR(01),
         impt_cv_isss_tra      DECIMAL(17,2),
         impt_cv_isss_pat      DECIMAL(17,2),
         impt_cs_isss          DECIMAL(17,2),
         impt_ahr_sol_dep      DECIMAL(17,2),
         impt_bono             DECIMAL(17,2),
         valor_nominal_bono    DECIMAL(17,2),
         result_operacion      CHAR(02),
         consec_reg_lote       INTEGER,
         n_rfc                 CHAR(13)
      END RECORD,

      reg2 RECORD
         acc_cv_isss_tra       DECIMAL(19,6),
         acc_cv_isss_pat       DECIMAL(19,6),
         acc_cs_isss           DECIMAL(19,6),
         acc_ahr_sol_dep       DECIMAL(19,6),
         acc_impt_bono         DECIMAL(19,6),
         acc_nominal_bono      DECIMAL(19,6)
      END RECORD,

      val_nom_bono             DECIMAL(17,2),
      vimpt_cv_isss_tra        DECIMAL(17,2),
      vimpt_cv_isss_pat        DECIMAL(17,2),
      vimpt_cs_isss            DECIMAL(17,2),
      vimpt_ahr_sol_dep        DECIMAL(17,2),
      vimpt_bono               DECIMAL(17,2),
      vvalor_nominal_bono      DECIMAL(17,2),
      vtipo_rechazo            CHAR(03),
      vtipo_subcuenta          CHAR(02),
      vsub_isss                SMALLINT,
      vind_rechazo             SMALLINT,
      ejecuta                  CHAR(200),
      dcurp                    CHAR(18),
      dnss                     CHAR(11),
      cont_regi, i             SMALLINT,
      siefore34                SMALLINT,
      ruta_archiv              CHAR(100),
      precio_acc               LIKE glo_valor_accion.precio_del_dia,
      precio_sie1_vol          LIKE glo_valor_accion.precio_del_dia,
      acc_reclas               LIKE dis_cuenta.monto_en_acciones,
      pes_reclas               LIKE dis_cuenta.monto_en_acciones,
      acc_ahoret               LIKE dis_cuenta.monto_en_acciones,
      pes_ahoret               LIKE dis_cuenta.monto_en_acciones,
      acc_ahosol_tra           LIKE dis_cuenta.monto_en_acciones,
      pes_ahosol_tra           LIKE dis_cuenta.monto_en_acciones,
      acc_apo_ret08            LIKE dis_cuenta.monto_en_acciones,
      pes_apo_ret08            LIKE dis_cuenta.monto_en_acciones,
      acc_rema_37              LIKE dis_cuenta.monto_en_acciones,
      pes_rema_37              LIKE dis_cuenta.monto_en_acciones,
      acc_rema_38              LIKE dis_cuenta.monto_en_acciones,
      pes_rema_38              LIKE dis_cuenta.monto_en_acciones

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
   FROM dis_det_devorec a
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
   SELECT UNIQUE curp FROM safre_tmp:curp_validados

   FOREACH cur_val INTO dcurp
 
      CALL buscar_cual_nss(dcurp,vfolio)
      RETURNING dcurp,dnss,vtipo_subcuenta,vind_rechazo

      ------- validacion que nss exista en cta_regimen -------  --c22-6.3
      IF vind_rechazo = 0 THEN
         FOR i = 1 TO 36
            IF i=31 OR i=32 OR i=33 OR i=36 OR i=30 OR i=37 OR i=38 THEN
               SELECT count(*)                                    --c22-6.3
               INTO   cont_regi
               FROM   cta_regimen                                 --c22-6.3
               WHERE  nss = dnss
               AND    subcuenta = i                               --c22-6.3

               IF cont_regi = 0 THEN
                  DISPLAY "NSS no existe en cta_regimen o faltan subcuentas ",dnss
                  LET vind_rechazo  = 1
                  LET vtipo_rechazo = "09"                        --c22-6.3
                  EXIT FOR
               END IF                                             --c22-6.3
            END IF                                                --c22-6.3
         END FOR
      END IF                                                --c22-6.3
      --------------------------------------------------------  --c22-6.3

      OPEN    cur_hist USING vfolio, dcurp
      FOREACH cur_hist INTO  reg.*
         LET acc_reclas     = 0
         LET pes_reclas     = 0
         LET acc_ahoret     = 0
         LET pes_ahoret     = 0
         LET acc_ahosol_tra = 0
         LET pes_ahosol_tra = 0
         LET acc_apo_ret08  = 0
         LET pes_apo_ret08  = 0

         LET reg.nss       = dnss

         CALL saldo(dnss, 31,"T")
         RETURNING reg2.acc_cv_isss_tra, reg.impt_cv_isss_tra

         CALL saldo(dnss, 31,"P")
         RETURNING reg2.acc_cv_isss_pat, reg.impt_cv_isss_pat

         CALL saldo(dnss, 32,"T")
         RETURNING reg2.acc_cs_isss, reg.impt_cs_isss

         CALL saldo(dnss, 33,"T")
         RETURNING reg2.acc_ahr_sol_dep, reg.impt_ahr_sol_dep

         IF reg.ind_trab_bono = 1 THEN                   ###BONO SIN REDIMIR
            CALL saldo(dnss, 36,"T")
            RETURNING reg2.acc_nominal_bono, val_nom_bono

            CALL saldo(dnss, 30,"T")
            RETURNING acc_apo_ret08, pes_apo_ret08

            IF reg2.acc_nominal_bono <> reg.valor_nominal_bono THEN
               LET vind_rechazo = 1                     ### MONTO BONO DIFERENTE
            END IF

            UPDATE dis_det_devorec
            SET    impt_cv_isss_tra  = reg.impt_cv_isss_tra,
                   impt_cv_isss_pat  = reg.impt_cv_isss_pat,
                   impt_cs_isss      = reg.impt_cs_isss,
                   impt_ahr_sol_dep  = reg.impt_ahr_sol_dep,
                   impt_bono         = 0,
                   valor_nominal_bono= reg.valor_nominal_bono
            WHERE  folio = vfolio
            AND    consec_reg_lote = reg.consec_reg_lote
            AND    n_unico = reg.curp
         ELSE
            CALL saldo(dnss, 30,"T")
            RETURNING reg2.acc_impt_bono, vimpt_bono

            UPDATE dis_det_devorec
            SET    impt_cv_isss_tra  = reg.impt_cv_isss_tra,
                   impt_cv_isss_pat  = reg.impt_cv_isss_pat,
                   impt_cs_isss      = reg.impt_cs_isss,
                   impt_ahr_sol_dep  = reg.impt_ahr_sol_dep,
                   impt_bono         = vimpt_bono,
                   valor_nominal_bono= 0
            WHERE  folio = vfolio
            AND    consec_reg_lote = reg.consec_reg_lote
            AND    n_unico = reg.curp
         END IF

--         CALL saldo(dnss, 13,"T")
--         RETURNING acc_ahoret, pes_ahoret

         CALL saldo(dnss, 34,"T")
         RETURNING acc_ahosol_tra, pes_ahosol_tra

         CALL saldo(dnss, 37,"T")
         RETURNING acc_rema_37, pes_rema_37

         CALL saldo(dnss, 38,"T")
         RETURNING acc_rema_38, pes_rema_38 

         LET vimpt_cv_isss_tra     = reg.impt_cv_isss_tra
         LET vimpt_cv_isss_pat     = reg.impt_cv_isss_pat
         LET vimpt_cs_isss         = reg.impt_cs_isss
         LET vimpt_ahr_sol_dep     = reg.impt_ahr_sol_dep
         LET vimpt_bono            = vimpt_bono
         LET vvalor_nominal_bono   = reg.valor_nominal_bono

         --Motivo por el cual rechazar
         IF vind_rechazo = 1 THEN
            CALL Contador_rechazo()
            CALL Sumatoria_rechazo(vimpt_cv_isss_tra,
                                   vimpt_cv_isss_pat,
                                   vimpt_cs_isss,
                                   vimpt_ahr_sol_dep,
                                   vimpt_bono,
                                   vvalor_nominal_bono) 

            UPDATE dis_det_devorec
            SET    result_operacion = "02"
            WHERE  folio = vfolio
            AND    consec_reg_lote = reg.consec_reg_lote
            AND    n_unico = reg.curp
         ELSE
            CALL Contador_aceptado()
            CALL Sumatoria_aceptado(vimpt_cv_isss_tra,
                                    vimpt_cv_isss_pat,
                                    vimpt_cs_isss,
                                    vimpt_ahr_sol_dep,
                                    vimpt_bono,
                                    vvalor_nominal_bono)

            ------------- DISPERSA CARGOS EN dis_provision ------------------

            IF vimpt_cv_isss_tra > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,vimpt_cv_isss_tra,reg2.acc_cv_isss_tra,31,89,today,reg.consec_reg_lote,0,0,"APOR-TRAB")
            END IF

            IF vimpt_cv_isss_pat > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,vimpt_cv_isss_pat,reg2.acc_cv_isss_pat,31,89,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF

            IF vimpt_cs_isss > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,vimpt_cs_isss,reg2.acc_cs_isss,32,89,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF

            IF vimpt_ahr_sol_dep > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,vimpt_ahr_sol_dep,reg2.acc_ahr_sol_dep,33,89,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF

            IF pes_rema_37 > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,pes_rema_37,acc_rema_37,37,89,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF

            IF pes_rema_38 > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,pes_rema_38,acc_rema_38,38,89,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF

            IF vimpt_bono > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,vimpt_bono,reg2.acc_impt_bono,30,89,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF

            IF acc_reclas > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,pes_reclas,acc_reclas,30,87,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")

               LET pes_reclas = pes_reclas * (-1)
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,pes_reclas,0,19,86,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF

            IF vvalor_nominal_bono > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,val_nom_bono,vvalor_nominal_bono,36,89,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF

            IF acc_ahoret > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,pes_ahoret,acc_ahoret,13,87,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")

               LET pes_ahoret = pes_ahoret * (-1)
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,pes_ahoret,0,19,86,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF

            IF acc_ahosol_tra > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,pes_ahosol_tra,acc_ahosol_tra,34,87,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")

               SELECT codigo_siefore
               INTO siefore34
               FROM cta_regimen
               WHERE nss = dnss
               AND   subcuenta = 34

               IF siefore34 <> 1 THEN
                  SELECT precio_del_dia
                  INTO   precio_sie1_vol
                  FROM   glo_valor_accion
                  WHERE  fecha_valuacion = reg_ident.fecha_recepcion
                  AND    codigo_siefore  = 1

                  LET acc_ahosol_tra = pes_ahosol_tra / precio_sie1_vol
               END IF
 
               LET acc_ahosol_tra = acc_ahosol_tra * (-1)
               LET pes_ahosol_tra = pes_ahosol_tra * (-1)

CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,pes_ahosol_tra,acc_ahosol_tra,10,86,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF

            IF acc_apo_ret08 > 0 THEN
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,pes_apo_ret08,acc_apo_ret08,30,87,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")

               LET pes_apo_ret08 = pes_apo_ret08 * (-1)
               LET acc_apo_ret08 = acc_apo_ret08 * (-1)
CALL Dispersa_cargo(vfolio,reg.nss,reg.curp,vcoduni,today,today,pes_apo_ret08,acc_apo_ret08,13,86,today,reg.consec_reg_lote,0,0,"CAMBIO_REG")
            END IF
         END IF

         LET vimpt_cv_isss_tra     = 0
         LET vimpt_cv_isss_pat     = 0
         LET vimpt_cs_isss         = 0
         LET vimpt_ahr_sol_dep     = 0
         LET vimpt_bono            = 0
         LET vvalor_nominal_bono   = 0
      END FOREACH
      CLOSE cur_hist
   END FOREACH
END FUNCTION


FUNCTION saldo(nssc, subc, tipoc)
DEFINE
   nssc    CHAR(11),
   subc    SMALLINT,
   tipoc   CHAR(1),
   accc    DECIMAL(19,6),
   pesc    DECIMAL(16,2),
   siec    SMALLINT,
   precioc LIKE glo_valor_accion.precio_del_dia

   IF subc = 31 THEN
      IF subc = 31 AND tipoc = "T" THEN
         SELECT NVL(sum(monto_en_acciones),0)
         INTO accc
         FROM  dis_cuenta
         WHERE nss = nssc
         AND subcuenta = subc
         AND id_aportante IN ("APOR-TRAB","INT.EXT-TRA")
      ELSE
         SELECT NVL(sum(monto_en_acciones),0)
         INTO accc
         FROM  dis_cuenta
         WHERE nss = nssc
         AND subcuenta = subc
         AND id_aportante NOT IN ("APOR-TRAB","INT.EXT-TRA")
      END IF
   ELSE
      SELECT NVL(sum(monto_en_acciones),0)
      INTO accc
      FROM  dis_cuenta
      WHERE nss = nssc
      AND subcuenta = subc
   END IF

   SELECT codigo_siefore
   INTO   siec
   FROM   cta_regimen
   WHERE  nss       = nssc
   AND    subcuenta = subc

   SELECT NVL(precio_del_dia,0)
   INTO   precioc
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = reg_ident.fecha_recepcion
   AND    codigo_siefore  = siec

   LET pesc = accc * precioc

   RETURN accc, pesc
END FUNCTION


FUNCTION Contador_rechazo()
   LET vcont_rech = vcont_rech + 1
END FUNCTION


FUNCTION Contador_aceptado()
   LET vcont_acep = vcont_acep + 1
END FUNCTION


FUNCTION Dispersa_cargo(vfolio,
                        vnss,
                        vcurp,
                        vcoduni,
                        cfecha_pago,
                        cfecha_rviv,
                        monto_pes,
                        monto_acc,
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
      vsua               CHAR(06),
      vcoduni            CHAR(10),
      cfecha_pago        CHAR(08),
      cfecha_rviv        CHAR(08),
      cfecha_viv         CHAR(08),
      vfecha_pago        DATE,
      vfecha_rcv         DATE,
      vfecha_viv         DATE,
      vfecha_valor       DATE,
      vfecha_archivo     DATE,
      xfecha_pago        CHAR(10),
      xfecha_rcv         CHAR(10),
      xfecha_viv         CHAR(10),
      xfecha_archivo     CHAR(10),
      vsubcuenta         SMALLINT,
      vmovimiento        SMALLINT,
      vfentcons          DATE,
      vconsec_reg_lote   INTEGER,
      vmonto_part_viv    DECIMAL(18,6),
      vmonto_valor_part  DECIMAL(21,14),
      monto_acc          DECIMAL(19,6),
      monto_pes          DECIMAL(16,2),
      vtipo              SMALLINT,
      vval_porcentaje    DECIMAL(16,6),
      vsiefore           SMALLINT,
      vporcentaje_sie    DECIMAL(5,2),
      vantiguedad_desde  SMALLINT,
      vantiguedad_hasta  SMALLINT,
      vdias_antiguedad   INTEGER,
      vn_rfc_entidad     CHAR(12),
      precio_acc LIKE    glo_valor_accion.precio_del_dia,
      abo RECORD LIKE    dis_provision.*

   LET xfecha_pago = cfecha_pago[5,6],"/",
                     cfecha_pago[7,8],"/",
	             cfecha_pago[1,4]
   LET vfecha_pago = xfecha_pago

   IF vsubcuenta = 13 or vsubcuenta = 19  THEN
      LET xfecha_rcv = cfecha_rviv[5,6],"/",
                       cfecha_rviv[7,8],"/",
	               cfecha_rviv[1,4]
      LET vfecha_valor = xfecha_rcv
   ELSE
      IF vsubcuenta = 14 OR vsubcuenta = 35 THEN
         LET xfecha_viv = cfecha_rviv[5,6],
                          cfecha_rviv[7,8],
                          cfecha_rviv[1,4]
         LET vfecha_valor = xfecha_viv
      ELSE
         LET vfecha_valor = TODAY
      END IF
   END IF

   LET xfecha_archivo = vfecha_lote_devo[5,6],"/",
                        vfecha_lote_devo[7,8],"/",
                        vfecha_lote_devo[1,4]
   LET vfecha_archivo = xfecha_archivo

   SELECT codigo_siefore
   INTO vsiefore
   FROM cta_regimen
   WHERE nss = vnss
   AND subcuenta = vsubcuenta

   LET abo.tipo_movimiento   = vmovimiento       -- SI
   LET abo.subcuenta         = vsubcuenta        -- SI
   LET abo.folio             = vfolio            -- SI 
   LET abo.consecutivo_lote  = vconsec_reg_lote
   LET abo.siefore           = vsiefore          -- SI
   LET abo.nss               = vnss              -- SI
   LET abo.curp              = vcurp             -- NO
   LET abo.folio_sua         = ""                -- SI
   LET abo.fecha_pago        = TODAY             -- NO
   LET abo.fecha_valor       = vfecha_valor      -- SI

   SELECT precio_del_dia
   INTO  precio_acc
   FROM  glo_valor_accion
   WHERE fecha_valuacion = reg_ident.fecha_recepcion
   AND   codigo_siefore  = vsiefore

   LET abo.fecha_conversion  = reg_ident.fecha_recepcion
   LET abo.monto_en_pesos    = monto_pes         -- SI
   LET abo.monto_en_acciones = monto_acc         -- SI
   LET abo.precio_accion     = precio_acc        -- SI
   LET abo.dias_cotizados    = 0                 -- NO
   LET abo.sucursal          = vcoduni           -- SI
   LET abo.id_aportante      = vn_rfc_entidad    -- SI 
   LET abo.estado            = 5                 -- SI 
   LET abo.fecha_proceso     = TODAY             -- SI
   LET abo.usuario           = gusuario          -- SI
   LET abo.fecha_archivo     = vfecha_archivo    -- NO
   LET abo.etiqueta          = 0                 -- SI

   SELECT razon_social                   --c22-6
   INTO   vid_sie                        --c22-6
   FROM   tmp_siefore_local              --c22-6
   WHERE  codigo_siefore = abo.siefore   --c22-6

   IF abo.subcuenta = 36 THEN
      INSERT INTO tmp_dis_provision VALUES 
      (abo.siefore,          --c22-6
       abo.subcuenta,        --c22-6
       abo.tipo_movimiento,  --c22-6
       vporcentaje_sie,      --c22-6
       vid_sie,              --c22-6
       abo.monto_en_acciones)
   ELSE
      INSERT INTO tmp_dis_provision VALUES 
      (abo.siefore,          --c22-6
       abo.subcuenta,        --c22-6
       abo.tipo_movimiento,  --c22-6
       vporcentaje_sie,      --c22-6
       vid_sie,              --c22-6
       abo.monto_en_pesos)
   END IF

   LET abo.monto_en_pesos    = abo.monto_en_pesos * (-1)
   LET abo.monto_en_acciones = abo.monto_en_acciones * (-1)

   INSERT INTO dis_provision VALUES(abo.*)

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTAR REGISTRO EN dis_provision ",STATUS USING '-----'
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Sumatoria_rechazo(vimpt_cv_isss_tra,
                           vimpt_cv_isss_pat,
                           vimpt_cs_isss,
                           vimpt_ahr_sol_dep,
                           vimpt_bono,
                           valor_nominal_bono)

   DEFINE
      vimpt_cv_isss_tra    DECIMAL(17,2),
      vimpt_cv_isss_pat    DECIMAL(17,2),
      vimpt_cs_isss        DECIMAL(17,2),
      vimpt_ahr_sol_dep    DECIMAL(17,2),
      vimpt_bono           DECIMAL(17,2),
      valor_nominal_bono   DECIMAL(17,2)

LET tot_rech.impt_cv_isss_tra  = tot_rech.impt_cv_isss_tra  + vimpt_cv_isss_tra
LET tot_rech.impt_cv_isss_pat  = tot_rech.impt_cv_isss_pat  + vimpt_cv_isss_pat
LET tot_rech.impt_cs_isss      = tot_rech.impt_cs_isss      + vimpt_cs_isss
LET tot_rech.impt_ahr_sol_dep  = tot_rech.impt_ahr_sol_dep  + vimpt_ahr_sol_dep
LET tot_rech.impt_bono         = tot_rech.impt_bono         + vimpt_bono
LET tot_rech.valor_nominal_bono= tot_rech.valor_nominal_bono+ valor_nominal_bono

END FUNCTION


FUNCTION Sumatoria_aceptado(vimpt_cv_isss_tra,
                            vimpt_cv_isss_pat,
                            vimpt_cs_isss,
                            vimpt_ahr_sol_dep,
                            vimpt_bono,
                            valor_nominal_bono)

   DEFINE
      vimpt_cv_isss_tra     DECIMAL(17,2),
      vimpt_cv_isss_pat     DECIMAL(17,2),
      vimpt_cs_isss         DECIMAL(17,2),
      vimpt_ahr_sol_dep     DECIMAL(17,2),
      vimpt_bono            DECIMAL(17,2),
      valor_nominal_bono    DECIMAL(17,2)

LET tot_acep.impt_cv_isss_tra  = tot_acep.impt_cv_isss_tra  + vimpt_cv_isss_tra
LET tot_acep.impt_cv_isss_pat  = tot_acep.impt_cv_isss_pat  + vimpt_cv_isss_pat
LET tot_acep.impt_cs_isss      = tot_acep.impt_cs_isss      + vimpt_cs_isss
LET tot_acep.impt_ahr_sol_dep  = tot_acep.impt_ahr_sol_dep  + vimpt_ahr_sol_dep
LET tot_acep.impt_bono         = tot_acep.impt_bono         + vimpt_bono      
LET tot_acep.valor_nominal_bono= tot_acep.valor_nominal_bono+ valor_nominal_bono

END FUNCTION


FUNCTION Genera_confaf(vfolio)          -- ETAPA 7 
   DEFINE   
      vfolio                   INTEGER,
      ejecuta                  CHAR(200),
      tipo_ar                  SMALLINT,
      rident_pago              CHAR(16)

   DEFINE
      total_rcv                DECIMAL(16,2),
      total_vol                DECIMAL(16,2),
      total_compl              DECIMAL(16,2),
      total_viv_92             DECIMAL(16,2),
      total_viv_2008           DECIMAL(16,2),
      total_gub                DECIMAL(16,2),
      total_viv_gar_92         DECIMAL(16,2),
      total_viv_gar_2008       DECIMAL(16,2),

      acept_cv_isss_tra        DECIMAL(16,2),
      acept_cv_isss_pat        DECIMAL(16,2),
      acept_cs_isss            DECIMAL(16,2),
      acept_ahr_sol_dep        DECIMAL(16,2),
      acept_bono               DECIMAL(16,2),
      acept_valor_nominal_bono DECIMAL(16,2),

      recha_cv_isss_tra        DECIMAL(16,2),
      recha_cv_isss_pat        DECIMAL(16,2),
      recha_cs_isss            DECIMAL(16,2),
      recha_ahr_sol_dep        DECIMAL(16,2),
      recha_bono               DECIMAL(16,2),
      recha_valor_nominal_bono DECIMAL(16,2)

   DEFINE
      vidp                     CHAR(02),         --c22-6
      vsub                     CHAR(08),         --c22-6
      vsie                     SMALLINT,         --c22-6
      vpor                     DECIMAL(16,8),    --c22-6
      vident_sie               CHAR(08),         --c22-6
      vpesos                   DECIMAL(16,6),    --c22-6
      vcuantos                 SMALLINT,         --c22-6.3
      vcontador_sie            SMALLINT,         --c22-6.3
      vsuma                    DECIMAL(16,6),    --c22-6
      vimporte                 DECIMAL(16,2),    --c22-6
      vimporte2                DECIMAL(16,2),    --c22-6.9
      vrechazo                 DECIMAL(16,2)     --c22-6

   DEFINE
      g_dep   RECORD LIKE      dis_dep_devorec.*

   ------------------------- TOTAL ACEPTADO APORTES ------------------------
   LET acept_cv_isss_tra       = tot_acep.impt_cv_isss_tra
   LET acept_cv_isss_pat       = tot_acep.impt_cv_isss_pat
   LET acept_cs_isss           = tot_acep.impt_cs_isss
   LET acept_ahr_sol_dep       = tot_acep.impt_ahr_sol_dep
   LET acept_bono              = tot_acep.impt_bono
   LET acept_valor_nominal_bono= tot_acep.valor_nominal_bono

   ------------------------- TOTAL RECHAZOS APORTES ------------------------
   LET recha_cv_isss_tra       = tot_rech.impt_cv_isss_tra
   LET recha_cv_isss_pat       = tot_rech.impt_cv_isss_pat
   LET recha_cs_isss           = tot_rech.impt_cs_isss
   LET recha_ahr_sol_dep       = tot_rech.impt_ahr_sol_dep
   LET recha_bono              = tot_rech.impt_bono
   LET recha_valor_nominal_bono= tot_rech.valor_nominal_bono

   -------------------------------------------------------------------------
   CREATE INDEX tmp_dis_provision_1 ON tmp_dis_provision(sub,mov); --c22-6
   CREATE INDEX tmp_dis_provision_2 ON tmp_dis_provision(sie);     --c22-6
   UPDATE STATISTICS FOR TABLE tmp_dis_provision;                  --c22-6

   -------- Prepara aportes y comisiones de tmp_dis_provision  -------

   CREATE TEMP TABLE tmp_res_provision                             --c22-6
      (sub   CHAR(08),                                             --c22-6
       sie   SMALLINT,                                             --c22-6
       por   DECIMAL(12,2),
       idsie CHAR(08),
       pesos DECIMAL(16,6))                                        --c22-6

   ---- APORTE RCV ----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "71",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub IN (31)                                              --c22-6
   AND    mov IN (1,2,3,89)
   GROUP  BY 1,2,3,4                                               --c22-6
   
   ---- APORTE CUO SOC ISSSTE ----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "72",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub IN (32)                                              --c22-6
   AND    mov IN (1,2,3,89)                                        --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6

   ---- APORTE AHORRO SOLIDARIO ----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "73",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub IN (33)                                              --c22-6
   AND    mov IN (1,2,3,89)                                           --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6
 
   ---- APORTE BONO REDIMIDO----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "74",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub = 30                                                 --c22-6
   AND    mov IN (1,2,89)                                          --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6

   ---- APORTE NOMINAL BONO ---- 
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "75",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub = 36                                                 --c22-6
   AND    mov IN (1,2,89)                                          --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6

   -------- Obtiene aportes de tmp_res_provision --------
   SELECT count(*) INTO vcuantos FROM tmp_res_provision
   IF vcuantos = 0 THEN                        # Archivo Completo Rechazado
      DECLARE cur_noacep CURSOR FOR
      SELECT ident_pago[14,15] FROM dis_dep_devorec
      WHERE folio = vfolio
   
      FOREACH cur_noacep INTO vidp
         UPDATE dis_dep_devorec
         SET    importe          = importe / 100, 
                impt_aport_acept = 0,        
                impt_aport_dev   = importe / 100,
                ident_siefore    = vident_sie,    
                tipo_siefore     = 0
         WHERE  folio            = vfolio
         AND    ident_pago[14,15]= vidp
         AND    tipo_siefore     = 0
      END FOREACH
   END IF

   DECLARE cur_resumen CURSOR FOR                                  --c22-6
   SELECT sub,                                                     --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          SUM(pesos)                                               --c22-6
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

    --LET vpor = vpesos / vsuma             --c22-113

      CASE
         WHEN vsub = "71"
            LET vrechazo = recha_cv_isss_tra  + recha_cv_isss_pat
         WHEN vsub = "72"
            LET vrechazo = recha_cs_isss
         WHEN vsub = "73"
            LET vrechazo = recha_ahr_sol_dep
         WHEN vsub = "74"
            LET vrechazo = recha_bono
         WHEN vsub = "75"
            LET vrechazo = recha_valor_nominal_bono
      END CASE

--    IF vsie = 1 THEN                                         --c22-6.3

      SELECT *                                                 --c22-6
      INTO   g_dep.*
      FROM   dis_dep_devorec                                   --c22-6
      WHERE  folio             = vfolio                        --c22-6
      AND    ident_pago[14,15] = vsub                          --c22-6
      AND    tipo_siefore      = 0                             --c22-6
      AND    ident_siefore     = ""

      IF vcuantos = 1 THEN                                     --c22-6.3

         UPDATE dis_dep_devorec                                --c22-6
         SET    importe          = importe / 100,              --c22-6.5
                impt_aport_acept = vpesos,                     --c22-6
                impt_aport_dev   = 0,  --vrechazo,             --c22-6.9
                ident_siefore    = vident_sie,                 --c22-6
                tipo_siefore     = vsie                        --c22-6
         WHERE  folio            = vfolio                      --c22-6
         AND    ident_pago[14,15]= vsub                        --c22-6
         AND    tipo_siefore     = 0                           --c22-6
 
         IF vrechazo > 0 THEN                                  --c22-6.9
            LET vimporte2 = g_dep.importe / 100                --c22-6.9
            INSERT INTO dis_dep_devorec VALUES                 --c22-6.9
              (                                                --c22-6.9
              g_dep.folio,                                     --c22-6.9
              g_dep.tipo_registro,                             --c22-6.9
              g_dep.ident_servicio,                            --c22-6.9
              g_dep.ident_pago,                                --c22-6.9
              vimporte2,                                       --c22-6.9
              0,                                               --c22-6.9
              vrechazo,                                        --c22-6.9
              g_dep.fech_liquidacion,                          --c22-6.9
              "",                                              --c22-6.9
              0,                                               --c22-6.9
              g_dep.fecha_archivo,                             --c22-6.9
              g_dep.fecha_envio,                               --c22-6.9
              g_dep.estado
              )                                                --c22-6.9
         END IF                                                --c22-6.9
      ELSE                                                     --c22-6.3
         LET vcontador_sie = vcontador_sie + 1                 --c22-6.3

         IF vcontador_sie = 1 THEN                             --c22-6.3

         --   IF vsie = 2 THEN                                 --c22-6.5
         --      LET vrechazo = 0                              --c22-6.5
         --   END IF                                           --c22-6.5

            UPDATE dis_dep_devorec                             --c22-6.3
            SET    importe          = importe / 100,           --c22-6.5
                   impt_aport_acept = vpesos,                  --c22-6.3
                   impt_aport_dev   = 0,  --vrechazo,          --c22-6.10
                   ident_siefore    = vident_sie,              --c22-6.3
                   tipo_siefore     = vsie                     --c22-6.3
            WHERE  folio            = vfolio                   --c22-6.3
            AND    ident_pago[14,15]= vsub                     --c22-6.3
            AND    tipo_siefore     = 0                        --c22-6.3

            IF vrechazo > 0 THEN                               --c22-6.9
               LET vimporte2 = g_dep.importe / 100             --c22-6.9
               INSERT INTO dis_dep_devorec VALUES              --c22-6.9
                 (                                             --c22-6.9
                 g_dep.folio,                                  --c22-6.9
                 g_dep.tipo_registro,                          --c22-6.9
                 g_dep.ident_servicio,                         --c22-6.9
                 g_dep.ident_pago,                             --c22-6.9
                 vimporte2,                                    --c22-6.9
                 0,                                            --c22-6.9
                 vrechazo,                                     --c22-6.9
                 g_dep.fech_liquidacion,                       --c22-6.9
                 "",                                           --c22-6.9
                 0,                                            --c22-6.9
                 g_dep.fecha_archivo,                          --c22-6.9
                 g_dep.fecha_envio,                            --c22-6.9
                 g_dep.estado                                  --c22-6.9
                 )
            END IF
         ELSE

        --  IF vsie = 2 THEN    -- OR vsie= 3 THEN             --c22-119
        --     LET vrechazo = 0                                --c22-119
        --  END IF                                             --c22-119

            IF vrechazo > 0 THEN                               --c22-119
               LET vimporte   = g_dep.importe --/ 100          --c22-119
               LET vrechazo = 0                                --c22-119
            ELSE                                               --c22-119
               LET vimporte   = g_dep.importe / 100            --c22-119
            END IF                                             --c22-119

            INSERT INTO dis_dep_devorec VALUES                 --c22-6
              (                                                --c22-6
              g_dep.folio,                                     --c22-6
              g_dep.tipo_registro,                             --c22-6
              g_dep.ident_servicio,                            --c22-6
              g_dep.ident_pago,                                --c22-6
              vimporte,                                        --c22-6
              vpesos,                                          --c22-6
              vrechazo,                                        --c22-6
              g_dep.fech_liquidacion,                          --c22-6
              vident_sie,                                      --c22-6
              vsie,
              g_dep.fecha_archivo,                             --c22-6.9
              g_dep.fecha_envio,                               --c22-6.9
              g_dep.estado                                     --c22-6
              )                                                --c22-6

         END IF                                                --c22-6

         IF vcontador_sie = vcuantos THEN                      --c22-6.3
            LET vcontador_sie = 0                              --c22-6.3
         END IF                                                --c22-6.3

      END IF                                                   --c22-6
   END FOREACH                                                 --c22-6

   DECLARE curctar CURSOR FOR
   SELECT ident_pago
   FROM dis_dep_devorec
   WHERE folio = vfolio
   AND   impt_aport_acept = 0
   AND   impt_aport_dev   = 0
-- AND   ident_pago       = g_dep.ident_pago
   AND   ident_pago[14,15] IN ("71","72","73","74","75")

   FOREACH curctar INTO rident_pago
      UPDATE dis_dep_devorec
      SET impt_aport_dev = importe / 100,
          importe        = importe / 100
      WHERE folio      = vfolio
      AND   ident_pago = rident_pago
   END FOREACH

   CALL Genera_salidas(vfolio,
                       acept_cv_isss_tra,
                       acept_cv_isss_pat,
                       acept_cs_isss,
                       acept_ahr_sol_dep,
                       acept_bono,
                       acept_valor_nominal_bono,

                       recha_cv_isss_tra,
                       recha_cv_isss_pat,
                       recha_cs_isss,
                       recha_ahr_sol_dep,
                       recha_bono,
                       recha_valor_nominal_bono
                       )

   LET vnom_archivo = new_cadena CLIPPED,".CONF_CR" CLIPPED

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; cat cza_cr rech_cr dep_cr sum_cr > ",vnom_archivo CLIPPED
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; chmod 777 ",vnom_archivo
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; rm cza_cr rech_cr dep_cr sum_cr "
   RUN ejecuta
END FUNCTION

 
FUNCTION Genera_salidas(vfolio,
                        acept_cv_isss_tra,
                        acept_cv_isss_pat,
                        acept_cs_isss,
                        acept_ahr_sol_dep,
                        acept_bono,
                        acept_valor_nominal_bono,

                        recha_cv_isss_tra,
                        recha_cv_isss_pat,
                        recha_cs_isss,
                        recha_ahr_sol_dep,
                        recha_bono,
                        recha_valor_nominal_bono
                        )

   DEFINE 
      vfolio                        INTEGER,
      acept_cv_isss_tra             DECIMAL(17,2),
      acept_cv_isss_pat             DECIMAL(17,2),
      acept_cs_isss                 DECIMAL(17,2),
      acept_ahr_sol_dep             DECIMAL(17,2),
      acept_bono                    DECIMAL(17,2),
      acept_valor_nominal_bono      DECIMAL(17,2),

      recha_cv_isss_tra             DECIMAL(17,2),
      recha_cv_isss_pat             DECIMAL(17,2),
      recha_cs_isss                 DECIMAL(17,2),
      recha_ahr_sol_dep             DECIMAL(17,2),
      recha_bono                    DECIMAL(17,2),
      recha_valor_nominal_bono      DECIMAL(17,2)

   DEFINE
      reg_cza  RECORD
                  tipo_registro      CHAR(02),
                  ident_servicio     CHAR(02),
                  ident_operacion    CHAR(02),
                  tipo_ent_origen    CHAR(02),
                  clave_ent_origen   CHAR(03),
                  tipo_ent_destino   CHAR(02),
                  clave_ent_destino  CHAR(03),
                  fech_creac_lote    CHAR(08),
                  lote_del_dia       CHAR(03),
                  mod_recp_envio     CHAR(02),
                  fech_limite_resp   CHAR(08),
                  result_operacion   CHAR(02),
                  motivo_rechazo1    CHAR(03),
                  motivo_rechazo2    CHAR(03),
                  motivo_rechazo3    CHAR(03)
               END RECORD

   DEFINE
      reg_rech RECORD
                  tipo_registro      CHAR(2),
                  ident_servicio     CHAR(2),
                  consec_reg_lote    INTEGER,
                  n_unico            CHAR(18),
                  n_seguro           CHAR(11),
                  n_seguro_issste    CHAR(11),
                  n_rfc              CHAR(13),
                  id_procesar        CHAR(08),
                  paterno            CHAR(40),
                  materno            CHAR(40),
                  nombres            CHAR(40),
                  sel_trab           CHAR(1),
                  ind_trab_bono      CHAR(1),
                  impt_cv_isss_tra   DECIMAL(17,2),
                  impt_cv_isss_pat   DECIMAL(17,2),
                  impt_cs_isss       DECIMAL(17,2),
                  impt_ahr_sol_dep   DECIMAL(17,2),
                  impt_bono          DECIMAL(17,2),
                  valor_nominal_bono DECIMAL(17,2),
                  result_operacion   CHAR(2),
                  det_mot_rechazo1   CHAR(3),
                  det_mot_rechazo2   CHAR(3),
                  det_mot_rechazo3   CHAR(3)
               END RECORD

   DEFINE
      reg_dep  RECORD
                  tipo_registro      CHAR(02),
                  ident_servicio     CHAR(02), 
                  ident_pago         CHAR(16),
                  importe            DECIMAL(17,2),
                  impt_aport_acept   DECIMAL(17,2),
                  impt_aport_dev     DECIMAL(17,2),
                  fech_liquidacion   DATE,
                  ident_siefore      CHAR(08),
                  tipo_siefore       SMALLINT
               END RECORD

   DEFINE
      reg_sum  RECORD
                  tipo_registro      CHAR(02),
                  ident_servicio     CHAR(02),
                  ident_operacion    CHAR(02),
                  tipo_ent_origen    CHAR(02),
                  clave_ent_origen   CHAR(03),
                  tipo_ent_destino   CHAR(02),
                  clave_ent_destino  CHAR(03),
                  fech_creac_lote    CHAR(08),
                  lote_del_dia       CHAR(03),
                  importe_total      DECIMAL(17,2),
                  total_val_nominal  DECIMAL(17,2),
                  num_de_reg_aport   CHAR(9),
                  num_cuentas_xpagar CHAR(6)
               END RECORD

   DEFINE 
      vimpt_tot_cv_isss_tra          DECIMAL(17,2),
      vimpt_tot_cv_isss_pat          DECIMAL(17,2),
      vimpt_tot_cs_isss              DECIMAL(17,2),
      vimpt_tot_ahr_sol_dep          DECIMAL(17,2),
      vimpt_tot_bono                 DECIMAL(17,2),
      vimpt_tot_valor_nominal_bono   DECIMAL(17,2),
      c15_impt_total_xpagar          ,
      c15_impt_aport_acept           CHAR(15),
      c16_impt_total_xpagar          ,
      c16_impt_aport_acept           CHAR(16),
      vtotal_aport_rech              ,
      vtotal_int_rech                DECIMAL(15,2)

   DEFINE
      xident_siefore                 CHAR(08),
      xtipo_siefore                  SMALLINT,
      vimpt_acept                    DECIMAL(15,2),
      vtipo_sie                      SMALLINT

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
   FROM  dis_cza_devorec
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
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/cza_cr"
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
      a.ind_trab_bono,
      a.impt_cv_isss_tra,
      a.impt_cv_isss_pat,
      a.impt_cs_isss,
      a.impt_ahr_sol_dep,
      a.impt_bono,
      a.valor_nominal_bono,
      a.result_operacion,
      a.det_mot_rechazo1,
      a.det_mot_rechazo2,
      a.det_mot_rechazo3
   FROM  dis_det_devorec a
   WHERE a.folio = vfolio
   ORDER BY a.consec_reg_lote, a.tipo_registro

--   LET reg_rech.result_operacion  = "00"
   LET reg_rech.det_mot_rechazo1  = "000"
   LET reg_rech.det_mot_rechazo2  = "000"
   LET reg_rech.det_mot_rechazo3  = "000"
   
   DISPLAY "GENERANDO RECH..." #---AT 12,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/rech_cr"
   START REPORT rep_rech TO vreporte
      FOREACH cur_rech INTO reg_rech.*
         OUTPUT TO REPORT rep_rech(reg_rech.*)
      END FOREACH
   FINISH REPORT rep_rech

   --------------- GENERA RESPUESTA DEPOSITOS ---------------------

   DECLARE cur_71 CURSOR FOR                                     --c22-6.6
   SELECT  ident_siefore,                                        --c22-6.6
           tipo_siefore                                          --c22-6.6
   FROM    dis_dep_devorec                                       --c22-6.6
   WHERE   folio = vfolio                                        --c22-6.6
   AND     ident_pago[14,15] = "71"                              --c22-6.6

   FOREACH cur_71 INTO xident_siefore,xtipo_siefore              --c22-6.6

      SELECT impt_aport_acept,                                   --c22-6.6
             tipo_siefore                                        --c22-6.6
      INTO   vimpt_acept,                                        --c22-6.6
             vtipo_sie                                           --c22-6.6
      FROM   dis_dep_devorec                                     --c22-6.6
      WHERE  folio = vfolio                                      --c22-6.6
      AND    ident_pago[14,15] = "11"                            --c22-6.6
      AND    tipo_siefore = 0                                    --c22-6.6

      IF STATUS <> NOTFOUND THEN                                 --c22-6.6
         IF vimpt_acept = 0 AND vtipo_sie = 0 THEN               --c22-6.6
            UPDATE dis_dep_devorec                               --c22-6.6
            SET    tipo_siefore  = xtipo_siefore,                --c22-6.6
                   ident_siefore = xident_siefore                --c22-6.6
            WHERE  folio             = vfolio                    --c22-6.6
            AND    ident_pago[14,15] = "11"                      --c22-6.6
            AND    tipo_siefore      = 0                         --c22-6.6
         END IF                                                  --c22-6.6
      END IF                                                     --c22-6.6
   END FOREACH                                                   --c22-6.6

   DECLARE cur_72 CURSOR FOR                                     --c22-6.6
   SELECT  ident_siefore,                                        --c22-6.6
           tipo_siefore                                          --c22-6.6
   FROM    dis_dep_devorec                                       --c22-6.6
   WHERE   folio = vfolio                                        --c22-6.6
   AND     ident_pago[14,15] = "72"                              --c22-6.6

   FOREACH cur_72 INTO xident_siefore,xtipo_siefore              --c22-6.6

      SELECT impt_aport_acept,                                   --c22-6.6
             tipo_siefore                                        --c22-6.6
      INTO   vimpt_acept,                                        --c22-6.6
             vtipo_sie                                           --c22-6.6
      FROM   dis_dep_devorec                                     --c22-6.6
      WHERE  folio = vfolio                                      --c22-6.6
      AND    ident_pago[14,15] = "12"                            --c22-6.6
      AND    tipo_siefore = 0                                    --c22-6.6

      IF STATUS <> NOTFOUND THEN                                 --c22-6.6
         IF vimpt_acept = 0 AND vtipo_sie = 0 THEN               --c22-6.6
            UPDATE dis_dep_devorec                               --c22-6.6
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
      importe,
      impt_aport_acept,
      impt_aport_dev,
      fech_liquidacion,
      ident_siefore,
      tipo_siefore
   FROM dis_dep_devorec
   WHERE folio = vfolio

   DISPLAY "GENERANDO DEP....." #------AT 14,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/dep_cr"
   START REPORT rep_dep TO vreporte
      FOREACH cur_dep2 INTO reg_dep.*
         OUTPUT TO REPORT rep_dep(reg_dep.*)
      END FOREACH
   FINISH REPORT rep_dep

   IF recha_cv_isss_tra IS NULL THEN
      LET recha_cv_isss_tra = 0
   END IF
   IF recha_cv_isss_pat IS NULL THEN
      LET recha_cv_isss_pat = 0
   END IF
   IF recha_cs_isss IS NULL THEN
      LET recha_cs_isss = 0
   END IF
   IF recha_ahr_sol_dep IS NULL THEN
      LET recha_ahr_sol_dep = 0
   END IF
   IF recha_bono IS NULL THEN
      LET recha_bono = 0
   END IF
   IF recha_valor_nominal_bono IS NULL THEN
      LET recha_valor_nominal_bono = 0 
   END IF

   IF acept_cv_isss_tra IS NULL THEN
      LET acept_cv_isss_tra = 0
   END IF
   IF acept_cv_isss_pat IS NULL THEN
      LET acept_cv_isss_pat = 0
   END IF
   IF acept_cs_isss IS NULL THEN
      LET acept_cs_isss = 0
   END IF
   IF acept_ahr_sol_dep IS NULL THEN
      LET acept_ahr_sol_dep = 0
   END IF
   IF acept_bono IS NULL THEN
      LET acept_bono = 0
   END IF
   IF acept_valor_nominal_bono IS NULL THEN
      LET acept_valor_nominal_bono = 0
   END IF

   UPDATE dis_sum_devorec
   SET   importe_total     = recha_cv_isss_tra  + acept_cv_isss_tra
                           + recha_cv_isss_pat  + acept_cv_isss_pat
                           + recha_cs_isss      + acept_cs_isss
                           + recha_ahr_sol_dep  + acept_ahr_sol_dep
                           + recha_bono         + acept_bono,
         total_val_nominal = recha_valor_nominal_bono + acept_valor_nominal_bono
   WHERE folio = vfolio

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
          importe_total,
          total_val_nominal,
          num_de_reg_aport,
          num_cuentas_xpagar
   FROM   dis_sum_devorec
   WHERE  folio = vfolio
   FOR UPDATE

   DISPLAY "GENERANDO SUM..." #----AT 16,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/sum_cr"
   START REPORT rep_sum TO vreporte
      FOREACH cur_sum INTO reg_sum.*

         LET reg_sum.tipo_ent_origen   = "01"
         LET reg_sum.clave_ent_origen  = vcod_afore
         LET reg_sum.tipo_ent_destino  = "03"
         LET reg_sum.clave_ent_destino = "001"
         LET reg_sum.fech_creac_lote   = reg_cza.fech_creac_lote
         LET reg_sum.lote_del_dia      = reg_cza.lote_del_dia
         LET reg_sum.num_de_reg_aport  = "000000000"

         UPDATE dis_sum_devorec
         SET    importe_total        = recha_cv_isss_tra  + acept_cv_isss_tra
                                     + recha_cv_isss_pat  + acept_cv_isss_pat
                                     + recha_cs_isss      + acept_cs_isss
                                     + recha_ahr_sol_dep  + acept_ahr_sol_dep
                                     + recha_bono         + acept_bono,
                total_val_nominal    = recha_valor_nominal_bono
                                     + acept_valor_nominal_bono
 
         WHERE CURRENT OF cur_sum

         OUTPUT TO REPORT rep_sum(reg_sum.*,
                                  acept_cv_isss_tra,
                                  acept_cv_isss_pat,
                                  acept_cs_isss,
                                  acept_ahr_sol_dep,
                                  acept_bono,
                                  acept_valor_nominal_bono,
                                  recha_cv_isss_tra,
                                  recha_cv_isss_pat,
                                  recha_cs_isss,
                                  recha_ahr_sol_dep,
                                  recha_bono,
                                  recha_valor_nominal_bono
                                  )
      END FOREACH
   FINISH REPORT rep_sum
END FUNCTION


REPORT rep_cza(reg_cza)
   DEFINE
      reg_cza RECORD
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
   DEFINE
      reg_rech RECORD
                  tipo_registro       CHAR(2),
                  ident_servicio      CHAR(2),
                  consec_reg_lote     INTEGER,
                  n_unico             CHAR(18),
                  n_seguro            CHAR(11),
                  n_seguro_issste     CHAR(11),
                  n_rfc               CHAR(13),
                  id_procesar         CHAR(8),
                  paterno             CHAR(40),
                  materno             CHAR(40),
                  nombres             CHAR(40),
                  sel_trab            CHAR(1),
                  ind_trab_bono       CHAR(1),
                  impt_cv_isss_tra    DECIMAL(17,2),
                  impt_cv_isss_pat    DECIMAL(17,2),
                  impt_cs_isss        DECIMAL(17,2),
                  impt_ahr_sol_dep    DECIMAL(17,2),
                  impt_bono           DECIMAL(17,2),
                  valor_nominal_bono  DECIMAL(17,2),
                  result_operacion    CHAR(2),
                  det_mot_rechazo1    CHAR(3),
                  det_mot_rechazo2    CHAR(3),
                  det_mot_rechazo3    CHAR(3)
               END RECORD

   DEFINE
      cconsec_reg_lote                CHAR(08),
      cimpt_cv_isss_tra               CHAR(15),
      cimpt_cv_isss_pat               CHAR(15),
      cimpt_cs_isss                   CHAR(15),
      cimpt_ahr_sol_dep               CHAR(15),
      cimpt_bono                      CHAR(15),
      cvalor_nominal_bono             CHAR(15),
   
      xconsec_reg_lote                CHAR(08),
      ximpt_cv_isss_tra               CHAR(14),
      ximpt_cv_isss_pat               CHAR(14),
      ximpt_cs_isss                   CHAR(14),
      ximpt_ahr_sol_dep               CHAR(14),
      ximpt_bono                      CHAR(14),
      xvalor_nominal_bono             CHAR(14)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW

   LET vnum_reg = vnum_reg + 1
 
   LET cimpt_cv_isss_tra  =reg_rech.impt_cv_isss_tra   USING '&&&&&&&&&&&&.&&'
   LET cimpt_cv_isss_pat  =reg_rech.impt_cv_isss_pat   USING '&&&&&&&&&&&&.&&'
   LET cimpt_cs_isss      =reg_rech.impt_cs_isss       USING '&&&&&&&&&&&&.&&'
   LET cimpt_ahr_sol_dep  =reg_rech.impt_ahr_sol_dep   USING '&&&&&&&&&&&&.&&'
   LET cimpt_bono         =reg_rech.impt_bono          USING '&&&&&&&&&&&&.&&'
   LET cvalor_nominal_bono=reg_rech.valor_nominal_bono USING '&&&&&&&&&&&&.&&'

   LET xconsec_reg_lote   = reg_rech.consec_reg_lote  USING '&&&&&&&&'

   LET ximpt_cv_isss_tra  = cimpt_cv_isss_tra  [1,12],cimpt_cv_isss_tra  [14,15]
   LET ximpt_cv_isss_pat  = cimpt_cv_isss_pat  [1,12],cimpt_cv_isss_pat  [14,15]
   LET ximpt_cs_isss      = cimpt_cs_isss      [1,12],cimpt_cs_isss      [14,15]
   LET ximpt_ahr_sol_dep  = cimpt_ahr_sol_dep  [1,12],cimpt_ahr_sol_dep  [14,15]
   LET ximpt_bono         = cimpt_bono         [1,12],cimpt_bono         [14,15]
   LET xvalor_nominal_bono= cvalor_nominal_bono[1,12],cvalor_nominal_bono[14,15]

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
                     reg_rech.ind_trab_bono,
                     ximpt_cv_isss_tra,
                     ximpt_cv_isss_pat,
                     ximpt_cs_isss,
                     ximpt_ahr_sol_dep,
                     ximpt_bono,
                     xvalor_nominal_bono,
                     reg_rech.result_operacion,  --- result_operacion,
                     "000",
                     reg_rech.det_mot_rechazo2,
                     reg_rech.det_mot_rechazo3,
                     160 space
END REPORT


REPORT rep_dep(reg_dep)
   DEFINE
      reg_dep RECORD
                 tipo_registro     CHAR(02),
                 ident_servicio    CHAR(02),
                 ident_pago        CHAR(16),
                 importe           DECIMAL(17,2),
                 impt_aport_acept  DECIMAL(17,2),
                 impt_aport_dev    DECIMAL(17,2),
                 fech_liquidacion  DATE,
                 ident_siefore     CHAR(08),
                 tipo_siefore      SMALLINT
              END RECORD

   DEFINE
      c17_importe                  CHAR(17),
      c16_importe                  CHAR(16),
      c17_impt_aport_acept         CHAR(17),
      c16_impt_aport_acept         CHAR(16),
      c17_impt_aport_dev           CHAR(17),
      c16_impt_aport_dev           CHAR(16),
      xfecha_liquidacion           CHAR(10),
      vfecha_liquidacion           CHAR(08)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT 
      ON EVERY ROW
         LET vnum_ctas_xpagar = vnum_ctas_xpagar + 1

         IF reg_dep.importe IS NULL THEN
            LET c16_importe = "0000000000000000"
         ELSE
            LET c17_importe = reg_dep.importe USING "&&&&&&&&&&&&&&.&&"
            LET c16_importe = c17_importe[01,14],c17_importe[16,17]
         END IF

         IF reg_dep.impt_aport_acept IS NULL THEN
            LET c16_impt_aport_acept = "0000000000000000"
         ELSE
            LET c17_impt_aport_acept = reg_dep.impt_aport_acept
                                       USING "&&&&&&&&&&&&&&.&&"
            LET c16_impt_aport_acept = c17_impt_aport_acept[01,14],
                                       c17_impt_aport_acept[16,17]
         END IF

         IF reg_dep.impt_aport_dev IS NULL THEN
            LET c16_impt_aport_dev = "0000000000000000"
         ELSE
            LET c17_impt_aport_dev = reg_dep.impt_aport_dev
                                     USING "&&&&&&&&&&&&&&.&&"
            LET c16_impt_aport_dev = c17_impt_aport_dev[01,14],
                                     c17_impt_aport_dev[16,17]
         END IF

         LET xfecha_liquidacion = reg_dep.fech_liquidacion
         LET vfecha_liquidacion = xfecha_liquidacion[7,10],
                                  xfecha_liquidacion[1,02],
                                  xfecha_liquidacion[4,05]

         IF reg_dep.tipo_siefore = 13 THEN
            LET reg_dep.ident_siefore = "        "
            LET reg_dep.tipo_siefore  = ""
         END IF

         PRINT COLUMN 01,reg_dep.tipo_registro,
                         reg_dep.ident_servicio,
                         reg_dep.ident_pago,
                         c16_importe,
                         c16_impt_aport_acept,
                         c16_impt_aport_dev,
                         vfecha_liquidacion,
                         reg_dep.ident_siefore,
                         reg_dep.tipo_siefore    USING '&&&',
                         363 space
END REPORT


REPORT rep_sum(reg_sum,
               acept_cv_isss_tra,
               acept_cv_isss_pat,
               acept_cs_isss,
               acept_ahr_sol_dep,
               acept_bono,
               acept_valor_nominal_bono,
               recha_cv_isss_tra,
               recha_cv_isss_pat,
               recha_cs_isss,
               recha_ahr_sol_dep,
               recha_bono,
               recha_valor_nominal_bono
               )

   DEFINE
      acept_cv_isss_tra              DECIMAL(17,2),
      acept_cv_isss_pat              DECIMAL(17,2),
      acept_cs_isss                  DECIMAL(17,2),
      acept_ahr_sol_dep              DECIMAL(17,2),
      acept_bono                     DECIMAL(17,2),
      acept_valor_nominal_bono       DECIMAL(17,2),

      recha_cv_isss_tra              DECIMAL(17,2),
      recha_cv_isss_pat              DECIMAL(17,2),
      recha_cs_isss                  DECIMAL(17,2),
      recha_ahr_sol_dep              DECIMAL(17,2),
      recha_bono                     DECIMAL(17,2),
      recha_valor_nominal_bono       DECIMAL(17,2)

   DEFINE
      reg_sum RECORD          
                 tipo_registro       CHAR(02),
                 ident_servicio      CHAR(02),
                 ident_operacion     CHAR(02),
                 tipo_ent_origen     CHAR(02),
                 clave_ent_origen    CHAR(03),
                 tipo_ent_destino    CHAR(02),
                 clave_ent_destino   CHAR(03),
                 fech_creac_lote     CHAR(08),
                 lote_del_dia        CHAR(03),
                 importe_total       DECIMAL(17,2),
                 total_val_nominal   DECIMAL(17,2),
                 num_de_reg_aport    CHAR(9),
                 num_cuentas_xpagar  CHAR(6)
              END RECORD

   DEFINE
      c16_importe_total              ,
      c16_total_val_nominal          CHAR(16),

      c17_importe_total              ,
      c17_total_val_nominal          CHAR(17),

      d17_importe_total              ,
      d17_total_val_nominal          DECIMAL(17,2)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         SELECT importe_total, total_val_nominal
         INTO   reg_sum.importe_total, reg_sum.total_val_nominal
         FROM   dis_sum_devorec
         WHERE  folio = vfolio

         IF reg_sum.importe_total IS NULL THEN
            LET reg_sum.importe_total = 0
         END IF

         LET c17_importe_total = reg_sum.importe_total
                                 USING "&&&&&&&&&&&&&&.&&"
         LET c16_importe_total = c17_importe_total[ 1,14],
                                 c17_importe_total[16,17]

         IF reg_sum.total_val_nominal IS NULL THEN
            LET reg_sum.total_val_nominal = 0
         END IF

         LET c17_total_val_nominal = reg_sum.total_val_nominal
                                     USING "&&&&&&&&&&&&&&.&&"
         LET c16_total_val_nominal = c17_total_val_nominal[ 1,14],
                                     c17_total_val_nominal[16,17]

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
                     c16_importe_total,
                     c16_total_val_nominal,
                     vnum_reg                     USING '&&&&&&&&&',
                     vnum_ctas_xpagar             USING '&&&&&&',
                     376 space
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
   DEFINE
      v_cat          CHAR(600),
      vv_fecha_log   CHAR(030),
      vv_prog        CHAR(010),
      paso           CHAR(100)
   
   DEFINE
      v_fecha_log    DATETIME YEAR TO SECOND

   DEFINE
      v_folio        INTEGER

   DEFINE
      reg_ruta       RECORD LIKE seg_modulo.*

   SELECT A.*
   INTO   reg_ruta.*
   FROM   seg_modulo A
   WHERE  modulo_cod = "bat"
 
   UPDATE bat_ctr_operacion
   SET    folio      = v_folio ,      
          estado_cod = 4    ,
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

   LET paso = "nohup:"            ,
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


FUNCTION buscar_cual_nss(curpa,vfolio)
   DEFINE
      curpa      CHAR(18),
      conta_afi  SMALLINT,
      nssa       CHAR(11),
      nssb       CHAR(11),
      tipo_sol   SMALLINT,
      i          SMALLINT,
      vfolio     INTEGER,
      tipo_adm   CHAR(2),
      cadena     CHAR(400)

   SELECT count(*)
   INTO conta_afi
   FROM afi_mae_afiliado
   WHERE n_unico = curpa

   IF conta_afi = 0 THEN
      RETURN curpa, "00000000000", "01", 1       ## 1 Rechazo
   END IF

   IF conta_afi = 1 THEN
      SELECT n_seguro, tipo_solicitud
      INTO  nssa, tipo_sol
      FROM  afi_mae_afiliado
      WHERE n_unico = curpa

      SELECT "OK"
      FROM  cta_act_marca
      WHERE nss = nssa
      AND   marca_cod IN (130,150)
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

               IF vcod_afore = 564 THEN        #METLIFE
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
	       FROM  cta_ctr_reg_ind
	       WHERE curp = curpa
	       AND   nti  = nssa 

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
      SELECT UNIQUE n_seguro
      INTO  nssb
      FROM  dis_det_devorec
      WHERE @folio = vfolio
      AND   @n_unico = curpa

      SELECT "OK"
      FROM  afi_mae_afiliado
      WHERE n_seguro = nssb
      AND   n_unico  = curpa 

      IF STATUS <> NOTFOUND THEN
         LET cadena = "SELECT n_seguro, tipo_solicitud ",
                      " FROM  afi_mae_afiliado ",
                      "WHERE  n_unico = '",curpa,"'",
                      " AND   n_seguro= '",nssb,"'"
      ELSE
         LET cadena = "SELECT n_seguro, tipo_solicitud ",
                      " FROM  afi_mae_afiliado ",
                      "WHERE  n_unico = '",curpa,"'"
      END IF

      PREPARE bus_sel FROM cadena
      DECLARE cur_mae CURSOR FOR bus_sel

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

                  IF vcod_afore = 564 THEN        #METLIFE
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

