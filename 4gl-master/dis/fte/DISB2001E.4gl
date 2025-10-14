###############################################################################
#Proyecto          => SAFRE                                                   #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => DIS                                                     #
#Programa          => DISB2001E                                               #
#Descripcion       => GENERA HISTORICOS (dis_det_issste, dis_dep_issste,      #
#                  => dis_cza_issste, dis_sum_issste) de ISSSTE               # 
#                  => COBRA COMISIONES, GENERA ARCHIVO CONFAF                 #
#Fecha Inicio      => 14 Enero 2008                                           #
#Elaboro           => DMR                                                     #
#Modificaciones    => (Se agrego filtro para verificar nss que                #
#                      no son issste pero que reciben estas aportaciones)     #
#                  => (Comisiones, dias antiguedad)                           #
#Fecha Mod         => 29 Enero 2008                                           #
#Fecha Mod         => 25 Junio 2008 ( NUEVO MPT DE RECAUDACION JUNIO 2008 )   #
#                  =>               TRAE RCV ISSSTE, CUO SOC ISSSTE, FOV 08   #
#Fecha Mod         => 15 Julio 2008 ( NUEVO MPT DE RECAUDACION JULIO 2008 )   #
#                  =>               TRAE SAR ISSSTE, FOV 92 Y FOV 08          #
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
         impt_sar_isss        DECIMAL(15,2),
         impt_ret_isss        DECIMAL(15,2),
         impt_cv_isss_pat     DECIMAL(15,2),
         impt_cv_isss_tra     DECIMAL(15,2),
         inte_ext_sar_isss    DECIMAL(15,2),
         inte_ext_ret_isss    DECIMAL(15,2),
         inte_ext_cv_isss_pat DECIMAL(15,2),
         inte_ext_cv_isss_tra DECIMAL(15,2),
         impt_viv_92          DECIMAL(15,2),
         impt_viv_2008        DECIMAL(15,2),
         inte_ext_viv_92      DECIMAL(15,2),
         inte_ext_viv_2008    DECIMAL(15,2),
         impt_viv_gar_92      DECIMAL(15,2),
         impt_viv_gar_2008    DECIMAL(15,2),
         apli_int_viv_92      DECIMAL(18,6),
         apli_int_viv_2008    DECIMAL(18,6),
         apli_int_gar_92      DECIMAL(18,6),
         apli_int_gar_2008    DECIMAL(18,6),
         impt_cs_isss         DECIMAL(15,2),
         impt_aport_vol       DECIMAL(15,2),
         impt_aport_compl     DECIMAL(15,2),
         impt_aport_lplazo    DECIMAL(15,2),
         impt_ahr_sol_tra     DECIMAL(15,2),
         impt_ahr_sol_dep     DECIMAL(15,2)
      END RECORD,

      tot_comi RECORD
         impt_ret_isss        DECIMAL(15,6),
         impt_cv_isss         DECIMAL(15,6)
      END RECORD,

      tot_rech RECORD
         impt_sar_isss        DECIMAL(15,2),
         impt_ret_isss        DECIMAL(15,2),
         impt_cv_isss_pat     DECIMAL(15,2),
         impt_cv_isss_tra     DECIMAL(15,2),
         inte_ext_sar_isss    DECIMAL(15,2),
         inte_ext_ret_isss    DECIMAL(15,2),
         inte_ext_cv_isss_pat DECIMAL(15,2),
         inte_ext_cv_isss_tra DECIMAL(15,2),
         impt_viv_92          DECIMAL(15,2),
         impt_viv_2008        DECIMAL(15,2),
         inte_ext_viv_92      DECIMAL(15,2),
         inte_ext_viv_2008    DECIMAL(15,2),
         impt_viv_gar_92      DECIMAL(15,2),
         impt_viv_gar_2008    DECIMAL(15,2),
         apli_int_viv_92      DECIMAL(18,6),
         apli_int_viv_2008    DECIMAL(18,6),
         apli_int_gar_92      DECIMAL(18,6),
         apli_int_gar_2008    DECIMAL(18,6),
         impt_cs_isss         DECIMAL(15,2),
         impt_aport_vol       DECIMAL(15,2),
         impt_aport_compl     DECIMAL(15,2),
         impt_aport_lplazo    DECIMAL(15,2),
         impt_ahr_sol_tra     DECIMAL(15,2),
         impt_ahr_sol_dep     DECIMAL(15,2)
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
   DEFINE val_dias_cot        INTEGER

   DEFINE vfolio              INTEGER
END GLOBALS


MAIN
   CALL STARTLOG("DISB2001E.log")

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
   LET reg_ident.val_cs          = ARG_VAL(7)   --valor de la cuota social --v2

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
                 "WHERE proceso_cod = 'DISB2001E' ",
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
   WHERE proceso_cod = "DISB2001E"
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
                    "WHERE proceso_cod = 'DISB2001E' ",
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
            " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001E' ",
            " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
            " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

      PREPARE claexe11 FROM cla_upd
      EXECUTE claexe11

      LET cla_sel = "SELECT MAX(consecutivo) ",
                    "FROM dis_ctrl_proceso ",
                    "WHERE proceso_cod = 'DISB2001E' ",
                    "AND etapa_cod = 3 " CLIPPED

      PREPARE claexe91 FROM cla_sel
      DECLARE cur_proceso91 CURSOR FOR claexe91
      OPEN cur_proceso91
         FETCH cur_proceso91 INTO vrow
      CLOSE cur_proceso91

      LET cla_upd = "UPDATE dis_ctrl_proceso ",
            " SET    dis_ctrl_proceso.folio       = ",vfolio,
            " WHERE  dis_ctrl_proceso.proceso_cod = 'DISB2001E' ",
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

   UPDATE dis_dep_issste
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
                    " h.fech_pago,",
                    " h.fech_valor_rviv,",
                    " h.impt_sar_isss,",
                    " h.impt_ret_isss,",
                    " h.impt_cv_isss_pat,",
                    " h.impt_cv_isss_tra,",
                    " h.inte_ext_sar_isss,",
                    " h.inte_ext_ret_isss,",
                    " h.inte_ext_cv_isss_pat,",
                    " h.inte_ext_cv_isss_tra,",
                    " h.impt_viv_92,",
                    " h.impt_viv_2008,",
                    " h.inte_ext_viv_92,",
                    " h.inte_ext_viv_2008,",
                    " h.apli_int_viv_92,",
                    " h.apli_int_viv_2008,",
                    " h.apli_int_ext_viv_92,",
                    " h.apli_int_ext_viv_2008,",
                    " h.impt_cs_isss,",
                    " h.impt_aport_vol,",
                    " h.impt_aport_compl,",
                    " h.impt_aport_lplazo,",
                    " h.impt_ahr_sol_tra,",
                    " h.impt_ahr_sol_dep,",
                    " h.periodo_pago,",
                    " h.result_operacion,",
                    " h.consec_reg_lote,",
                    " h.valor_aivs / 100000000000000,",
                    " h.n_rfc_entidad,",
                    " h.ident_viv_garantia ",
                    " FROM   dis_det_issste h ",
                    " WHERE  h.folio = ? ",
                    " AND    h.n_unico = ? "

      --- crea tabla tmporal para contestar confaf (dis_dep_issste) ---c22-6

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
      
      --Obtenemos el numero de dias cotizados 

      LET  val_dias_cot = 0
      CALL Obtiene_dias_bimes_ant(vfolio)

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

   LET tot_acep.impt_sar_isss        = 0
   LET tot_acep.impt_ret_isss        = 0
   LET tot_acep.impt_cv_isss_pat     = 0
   LET tot_acep.impt_cv_isss_tra     = 0
   LET tot_acep.inte_ext_sar_isss    = 0
   LET tot_acep.inte_ext_ret_isss    = 0
   LET tot_acep.inte_ext_cv_isss_pat = 0
   LET tot_acep.inte_ext_cv_isss_tra = 0
   LET tot_acep.impt_viv_92          = 0
   LET tot_acep.impt_viv_2008        = 0
   LET tot_acep.inte_ext_viv_92      = 0
   LET tot_acep.inte_ext_viv_2008    = 0
   LET tot_acep.impt_viv_gar_92      = 0
   LET tot_acep.impt_viv_gar_2008    = 0
   LET tot_acep.apli_int_viv_92      = 0
   LET tot_acep.apli_int_viv_2008    = 0
   LET tot_acep.apli_int_gar_92      = 0
   LET tot_acep.apli_int_gar_2008    = 0
   LET tot_acep.impt_cs_isss         = 0
   LET tot_acep.impt_aport_vol       = 0
   LET tot_acep.impt_aport_compl     = 0
   LET tot_acep.impt_aport_lplazo    = 0
   LET tot_acep.impt_ahr_sol_tra     = 0
   LET tot_acep.impt_ahr_sol_dep     = 0

   LET tot_comi.impt_ret_isss        = 0
   LET tot_comi.impt_cv_isss         = 0

   LET tot_rech.impt_sar_isss        = 0
   LET tot_rech.impt_ret_isss        = 0
   LET tot_rech.impt_cv_isss_pat     = 0
   LET tot_rech.impt_cv_isss_tra     = 0
   LET tot_rech.inte_ext_sar_isss    = 0
   LET tot_rech.inte_ext_ret_isss    = 0
   LET tot_rech.inte_ext_cv_isss_pat = 0
   LET tot_rech.inte_ext_cv_isss_tra = 0
   LET tot_rech.impt_viv_92          = 0
   LET tot_rech.impt_viv_2008        = 0
   LET tot_rech.inte_ext_viv_92      = 0
   LET tot_rech.inte_ext_viv_2008    = 0
   LET tot_rech.impt_viv_gar_92      = 0
   LET tot_rech.impt_viv_gar_2008    = 0
   LET tot_rech.apli_int_viv_92      = 0
   LET tot_rech.apli_int_viv_2008    = 0
   LET tot_rech.apli_int_gar_92      = 0
   LET tot_rech.apli_int_gar_2008    = 0
   LET tot_rech.impt_cs_isss         = 0
   LET tot_rech.impt_aport_vol       = 0
   LET tot_rech.impt_aport_compl     = 0
   LET tot_rech.impt_aport_lplazo    = 0
   LET tot_rech.impt_ahr_sol_tra     = 0
   LET tot_rech.impt_ahr_sol_dep     = 0

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
       "DISB2001E",             -- proceso_cod
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
      SLEEP 4
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Obtiene_nombre_arch(long, cadena)
DEFINE long ,i       SMALLINT,
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
       "DISB2001E",             -- proceso_cod
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
                 " WHERE proceso_cod = 'DISB2001E' ",
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
                 " WHERE  dis_ctrl_proceso.proceso_cod  = 'DISB2001E' ",
                 " AND    dis_ctrl_proceso.etapa_cod    = ",vetapa_cod,
                 " AND    dis_ctrl_proceso.consecutivo  = ",vrow CLIPPED

   PREPARE claexe35 FROM cla_upd
   EXECUTE claexe35
END FUNCTION


FUNCTION Separa_archivo()  -- ETAPA 3 
   DEFINE
      ejecuta CHAR(200)

  LET ejecuta = "head -n 1 ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/cza_isss"
  RUN ejecuta
                
  LET ejecuta = "grep '^02' ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/det_isss"
  RUN ejecuta
                
  LET ejecuta = "grep '^08' ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/dep_isss"
  RUN ejecuta

  LET ejecuta = "tail -1 ",gparam_dis.ruta_rescate CLIPPED,"/",
  reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/sum_isss"
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
                 "; cut -c 17-24 cza_isss > fecha_lote_isss"
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                 "; cut -c 25-27 cza_isss > lote_isss"
   RUN ejecuta
   
   WHENEVER ERROR CONTINUE
      DROP TABLE fecha_lote
      DROP TABLE tmp_lote         --c22-11
   WHENEVER ERROR STOP

   CREATE TEMP TABLE fecha_lote
      (fecha_lote CHAR(08))
      
   CREATE TEMP TABLE tmp_lote     --c22-11
      (lote CHAR(03))

   LET ejecuta = gparam_dis.ruta_rescate CLIPPED,"/fecha_lote_isss" CLIPPED
   LOAD FROM ejecuta INSERT INTO fecha_lote

   LET ejecuta = gparam_dis.ruta_rescate CLIPPED,"/lote_isss" CLIPPED
   LOAD FROM ejecuta INSERT INTO tmp_lote  --c22-11

   SELECT fecha_lote
   INTO   vfecha_lote_isss
   FROM   fecha_lote
   
   SELECT lote
   INTO   vlote
   FROM   tmp_lote

   SELECT "X"
   FROM  dis_cza_issste
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


      --------------------   GENERA dis_cza_issste   --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_cza_isss"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"   ---- Cabeza
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza_isss cza_isss > cza1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' cza1 > cza_isss "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;dbload -d safre_af -c sube_cza_issste -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  cza_isss cza1 fecha_lote_isss lote_isss "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; chmod 777 dbload.log "
      RUN ejecuta

      --------------------   GENERA dis_det_issste   --------------------

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_det
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza_isss > vfolio_det_isss" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det2 con el numero de registros igual al
      -- num. registros que tiene el detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f vfolio_det_isss det_isss > vfolio_det2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det borrando lineas que no sirven
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_det2 > vfolio_det_isss" CLIPPED 
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm vfolio_det2"
      RUN ejecuta

      -- Se crea det2 cortando pos 1-610 y det3 cortando
      -- hasta del pos 612-620 y se pegan en el archivo det (Result Operacion)
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-610 det_isss > det2"   --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 612-620 det_isss > det3" --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste -d '1' det2 det3 > det_isss"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det2 det3"
      RUN ejecuta

      -- Se crea det2 cortando pos 1-509 y det3 cortando  (VALOR AIVS)
      -- hasta del pos 511-620 y se pegan en el archivo det
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-503 det_isss > det4"     --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 506-509 det_isss > det5"   --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste -d '0' det4 det5 > det2"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 510-620 det_isss > det3" --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste -d '.' det2 det3 > det_isss"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det2 det3 det4 det5"
      RUN ejecuta
    
      -- Se crea det1 pegando datos generales con el detalle 
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_det_isss det_isss > det1" CLIPPED
      RUN ejecuta

      -- Se crea det eliminando espacio en blanco que se genera cuando
      -- se pegan los 2 archivos
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' det1 > det_isss "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det1 vfolio_det_isss"
      RUN ejecuta

      -- Se suben los datos del detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;dbload -d safre_af -c sube_det_issste -l dbload.log;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det_isss"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; chmod 777 dbload.log "
      RUN ejecuta

      --------------------   GENERA dis_dep_issste  --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_dep_isss"
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
         OUTPUT TO REPORT salida2(vfecha_lote_isss,vfecha_envio)
      FINISH REPORT salida2

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_dep_isss > dep01_1 " CLIPPED
      RUN ejecuta     

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_dep2 > dep02_1 " CLIPPED
      RUN ejecuta     

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f dep01_1 dep_isss > dep01_2 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f dep02_1 dep_isss > dep02_2 "  CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' dep01_2 > dep01_3" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^08/d' dep02_2 > dep02_3"
      RUN ejecuta

      --  Aqui se coloca la posicion final del campo tipo_siefore del Layout 08
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-138 dep_isss > dep2; mv dep2 dep_isss " --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste dep01_3 dep_isss dep02_3 > dep1;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' dep1 > dep_isss "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
"/;DBDATE=Y4MD0;export DBDATE;dbload -d safre_af -c sube_dep_issste -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  vfolio_dep_isss vfolio_dep2 dep01_1 dep02_1 dep01_2 dep02_2 dep01_3 dep02_3 dep_isss dep1"

      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; chmod 777 dbload.log "
      RUN ejecuta

      --------------------   GENERA dis_sum_issste   --------------------

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza_isss sum_isss > sum1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' sum1 > sum_isss "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_af -c sube_sum_issste -l dbload.log"

      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; rm  sum1 sum_isss vfolio_cza_isss "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; chmod 777 dbload.log "
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
      vimpt_viv_gar_92         DECIMAL(15,2),
      vimpt_viv_gar_2008       DECIMAL(15,2),

      reg RECORD
         nss                   CHAR(11),
         curp                  CHAR(18),
         fecha_pago            CHAR(08),
         fecha_rviv            CHAR(08),
         impt_sar_isss         DECIMAL(15,2),
         impt_ret_isss         DECIMAL(15,2),
         impt_cv_isss_pat      DECIMAL(15,2),
         impt_cv_isss_tra      DECIMAL(15,2),
         inte_ext_sar_isss     DECIMAL(15,2),
         inte_ext_ret_isss     DECIMAL(15,2),
         inte_ext_cv_isss_pat  DECIMAL(15,2),
         inte_ext_cv_isss_tra  DECIMAL(15,2),
         impt_viv_92           DECIMAL(15,2),
         impt_viv_2008         DECIMAL(15,2),
         inte_ext_viv_92       DECIMAL(15,2),
         inte_ext_viv_2008     DECIMAL(15,2),
         apli_int_viv_92       DECIMAL(18,6),
         apli_int_viv_2008     DECIMAL(18,6),
         apli_int_ext_viv_92   DECIMAL(18,6),
         apli_int_ext_viv_2008 DECIMAL(18,6),
         impt_cs_isss          DECIMAL(15,2),
         impt_aport_vol        DECIMAL(15,2),
         impt_aport_compl      DECIMAL(15,2),
         impt_aport_lplazo     DECIMAL(15,2),
         impt_ahr_sol_tra      DECIMAL(15,2),
         impt_ahr_sol_dep      DECIMAL(15,2),
         periodo_pago          CHAR(06),
         result_operacion      CHAR(02),
         consec_reg_lote       INTEGER,
         valor_aivs            DECIMAL(21,14),
         n_rfc_entidad         CHAR(12)
     END RECORD,

     vimpt_sar_isss            DECIMAL(15,2),
     vimpt_ret_isss            DECIMAL(15,2),
     vimpt_cv_isss_pat         DECIMAL(15,2),
     vimpt_cv_isss_tra         DECIMAL(15,2),
     vinte_ext_sar_isss        DECIMAL(15,2),
     vinte_ext_ret_isss        DECIMAL(15,2),
     vinte_ext_cv_isss_pat     DECIMAL(15,2),
     vinte_ext_cv_isss_tra     DECIMAL(15,2),
     vimpt_viv_92              DECIMAL(15,2),
     vimpt_viv_2008            DECIMAL(15,2),
     vinte_ext_viv_92          DECIMAL(15,2),
     vinte_ext_viv_2008        DECIMAL(15,2),
     vapli_int_viv_92          DECIMAL(18,6),
     vapli_int_viv_2008        DECIMAL(18,6),
     vimpt_cs_isss             DECIMAL(15,2),
     vimpt_aport_vol           DECIMAL(15,2),
     vimpt_aport_compl         DECIMAL(15,2),
     vimpt_aport_lplazo        DECIMAL(15,2),
     vimpt_ahr_sol_tra         DECIMAL(15,2),
     vimpt_ahr_sol_dep         DECIMAL(15,2),
     vapli_int_gar_92          DECIMAL(18,6),
     vapli_int_gar_2008        DECIMAL(18,6),
     vtipo_subcuenta           CHAR(02),
     vsub_isss                 SMALLINT,
     vind_rechazo              SMALLINT,
     ejecuta                   CHAR(200),
     dcurp                     CHAR(18),
     dnss                      CHAR(11),
     cont_regi                 SMALLINT,
     ruta_archiv               CHAR(100),
     conta,conta_cons          SMALLINT,
     consecu                   INTEGER

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
    FROM dis_det_issste a
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

    CREATE TEMP TABLE cons
    (
     consecus  INTEGER
    )

    PREPARE cl_dispersa FROM cla_mon
    DECLARE cur_hist CURSOR FOR cl_dispersa

    DECLARE cur_val CURSOR FOR
    SELECT UNIQUE curp FROM safre_tmp:curp_validados

    FOREACH cur_val INTO dcurp

       CALL buscar_cual_nss(dcurp,vfolio)
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

       OPEN    cur_hist USING vfolio, dcurp
       FOREACH cur_hist INTO  reg.*, vident_viv_garantia

       SELECT count(*) INTO conta_cons
       FROM cons
       WHERE consecus = reg.consec_reg_lote
     
       IF conta_cons = 0 THEN 
          INSERT INTO cons 
          VALUES(reg.consec_reg_lote)

          LET reg.nss       = dnss

          LET vimpt_sar_isss        = reg.impt_sar_isss        / 100
          LET vimpt_ret_isss        = reg.impt_ret_isss        / 100
          LET vimpt_cv_isss_pat     = reg.impt_cv_isss_pat     / 100
          LET vimpt_cv_isss_tra     = reg.impt_cv_isss_tra     / 100
          LET vinte_ext_sar_isss    = reg.inte_ext_sar_isss    / 100
          LET vinte_ext_ret_isss    = reg.inte_ext_ret_isss    / 100
          LET vinte_ext_cv_isss_pat = reg.inte_ext_cv_isss_pat / 100
          LET vinte_ext_cv_isss_tra = reg.inte_ext_cv_isss_tra / 100
          LET vimpt_viv_92          = reg.impt_viv_92          / 100
          LET vimpt_viv_2008        = reg.impt_viv_2008        / 100
          LET vinte_ext_viv_92      = reg.inte_ext_viv_92      / 100
          LET vinte_ext_viv_2008    = reg.inte_ext_viv_2008    / 100
          LET vimpt_cs_isss         = reg.impt_cs_isss         / 100
          LET vimpt_aport_vol       = reg.impt_aport_vol       / 100
          LET vimpt_aport_compl     = reg.impt_aport_compl     / 100
          LET vimpt_aport_lplazo    = reg.impt_aport_lplazo    / 100
          LET vimpt_ahr_sol_tra     = reg.impt_ahr_sol_tra     / 100
          LET vimpt_ahr_sol_dep     = reg.impt_ahr_sol_dep     / 100

          IF vident_viv_garantia = "1" THEN
             LET vimpt_viv_gar_92 = reg.impt_viv_92 / 100
             LET vimpt_viv_92     = 0
             LET vapli_int_viv_92 = 0
             LET vapli_int_gar_92= (reg.apli_int_viv_92+reg.apli_int_ext_viv_92)
                                 / 1000000

             LET vimpt_viv_gar_2008 = reg.impt_viv_2008 / 100
             LET vimpt_viv_2008     = 0
             LET vapli_int_viv_2008 = 0
             LET vapli_int_gar_2008 = (reg.apli_int_viv_2008 +
                                       reg.apli_int_ext_viv_2008) / 1000000
          ELSE
             LET vimpt_viv_gar_92 = 0
             LET vapli_int_viv_92= (reg.apli_int_viv_92+reg.apli_int_ext_viv_92)
                                 / 1000000
             LET vapli_int_gar_92 = 0

             LET vimpt_viv_gar_2008 = 0
             LET vapli_int_viv_2008 = (reg.apli_int_viv_2008 +
                                       reg.apli_int_ext_viv_2008) / 1000000
             LET vapli_int_gar_2008 = 0
          END IF

          --Motivo por el cual rechazar
          IF vind_rechazo = 1 THEN
             CALL Contador_rechazo()
             CALL Sumatoria_rechazo(vimpt_sar_isss,
                                    vimpt_ret_isss,
                                    vimpt_cv_isss_pat,
                                    vimpt_cv_isss_tra,
                                    vinte_ext_sar_isss,
                                    vinte_ext_ret_isss,
                                    vinte_ext_cv_isss_pat,
                                    vinte_ext_cv_isss_tra,
                                    vimpt_viv_92,
                                    vimpt_viv_2008,
                                    vinte_ext_viv_92,
                                    vinte_ext_viv_2008,
                                    vimpt_viv_gar_92,
                                    vimpt_viv_gar_2008,
                                    vapli_int_viv_92,
                                    vapli_int_viv_2008,
                                    vapli_int_gar_92,
                                    vapli_int_gar_2008,
                                    vimpt_cs_isss,
                                    vimpt_aport_vol,
                                    vimpt_aport_compl,
                                    vimpt_aport_lplazo,
                                    vimpt_ahr_sol_tra,
                                    vimpt_ahr_sol_dep) 
  
             IF vident_viv_garantia = "1" THEN
                LET vident_viv_garantia = "2"
             END IF

             UPDATE dis_det_issste
             SET    result_operacion = "02",
                    ident_viv_garantia = vident_viv_garantia
             WHERE  folio = vfolio
             AND    consec_reg_lote = reg.consec_reg_lote
             AND    n_unico = dcurp   
          ELSE
             CALL Contador_aceptado()
             CALL Sumatoria_aceptado(vimpt_sar_isss,
                                     vimpt_ret_isss,
                                     vimpt_cv_isss_pat,
                                     vimpt_cv_isss_tra,
                                     vinte_ext_sar_isss,
                                     vinte_ext_ret_isss,
                                     vinte_ext_cv_isss_pat,
                                     vinte_ext_cv_isss_tra,
                                     vimpt_viv_92,
                                     vimpt_viv_2008,
                                     vinte_ext_viv_92,
                                     vinte_ext_viv_2008,
                                     vimpt_viv_gar_92,
                                     vimpt_viv_gar_2008,
                                     vapli_int_viv_92,
                                     vapli_int_viv_2008,
                                     vapli_int_gar_92,
                                     vapli_int_gar_2008,
                                     vimpt_cs_isss,
                                     vimpt_aport_vol,
                                     vimpt_aport_compl,
                                     vimpt_aport_lplazo,
                                     vimpt_ahr_sol_tra,
                                     vimpt_ahr_sol_dep)


             ------------- DISPERSA APORTES EN dis_provision ------------------

             IF vimpt_sar_isss > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_sar_isss,13,1,vfentcons,reg.consec_reg_lote,0,0,reg.n_rfc_entidad)
             END IF

             IF vimpt_ret_isss > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_ret_isss,30,1,vfentcons,reg.consec_reg_lote,0,0,reg.n_rfc_entidad)
             END IF

             IF vimpt_cv_isss_pat > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_cv_isss_pat,31,1,vfentcons,reg.consec_reg_lote,0,0,reg.n_rfc_entidad)
             END IF

             IF vimpt_cv_isss_tra > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_cv_isss_tra,31,1,vfentcons,reg.consec_reg_lote,0,0,"APOR-TRAB")
             END IF

             IF vinte_ext_sar_isss > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vinte_ext_sar_isss,13,3,vfentcons,reg.consec_reg_lote,0,0,reg.n_rfc_entidad)
             END IF

             IF vinte_ext_ret_isss > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vinte_ext_ret_isss,30,3,vfentcons,reg.consec_reg_lote,0,0,reg.n_rfc_entidad)
             END IF

             IF vinte_ext_cv_isss_pat > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vinte_ext_cv_isss_pat,31,3,vfentcons,reg.consec_reg_lote,0,0,reg.n_rfc_entidad)
             END IF

             IF vinte_ext_cv_isss_tra > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vinte_ext_cv_isss_tra,31,3,vfentcons,reg.consec_reg_lote,0,0,"INT.EXT-TRA")
             END IF

             IF vimpt_cs_isss > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_cs_isss,32,1,vfentcons,reg.consec_reg_lote,0,0,reg.n_rfc_entidad)
             END IF

             IF vimpt_aport_vol > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_aport_vol,3,1,vfentcons,reg.consec_reg_lote,0,0,"VOL-ISSSTE")
             END IF

             IF vimpt_aport_compl > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_aport_compl,11,1,vfentcons,reg.consec_reg_lote,0,0,"COMPL.ISSSTE")
             END IF

             IF vimpt_aport_lplazo > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_aport_lplazo,15,1,vfentcons,reg.consec_reg_lote,0,0,"LPLAZO-ISSSTE")
             END IF

             IF vimpt_ahr_sol_tra > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_ahr_sol_tra,34,1,vfentcons,reg.consec_reg_lote,0,0,reg.n_rfc_entidad)
             END IF

             IF vimpt_ahr_sol_dep > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_ahr_sol_dep,33,1,vfentcons,reg.consec_reg_lote,0,0,reg.n_rfc_entidad)
             END IF

             ----------------------- VIVIENDA -----------------------

             IF vident_viv_garantia <> "1" THEN
                IF vimpt_viv_92 > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_viv_92,14,1,vfentcons,reg.consec_reg_lote,reg.apli_int_viv_92/1000000,reg.valor_aivs,reg.n_rfc_entidad)
                END IF

                IF vimpt_viv_2008 > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_viv_2008,35,1,vfentcons,reg.consec_reg_lote,reg.apli_int_viv_2008/1000000,reg.valor_aivs,reg.n_rfc_entidad)
                END IF

                IF vinte_ext_viv_92 > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vinte_ext_viv_92,14,4,vfentcons,reg.consec_reg_lote,reg.apli_int_ext_viv_92/1000000,reg.valor_aivs,reg.n_rfc_entidad)
                END IF

                IF vinte_ext_viv_2008 > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vinte_ext_viv_2008,35,4,vfentcons,reg.consec_reg_lote,reg.apli_int_ext_viv_2008/1000000,reg.valor_aivs,reg.n_rfc_entidad)
                END IF

             ELSE

                IF vimpt_viv_gar_92 > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_viv_gar_92,14,1,vfentcons,reg.consec_reg_lote,reg.apli_int_viv_92/1000000,reg.valor_aivs,reg.n_rfc_entidad)

CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_viv_gar_92,14,7,vfentcons,reg.consec_reg_lote,reg.apli_int_viv_92/1000000,reg.valor_aivs,reg.n_rfc_entidad)
                END IF

                IF vimpt_viv_gar_2008 > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_viv_gar_2008,35,1,vfentcons,reg.consec_reg_lote,reg.apli_int_viv_2008/1000000,reg.valor_aivs,reg.n_rfc_entidad)

CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vimpt_viv_gar_2008,35,7,vfentcons,reg.consec_reg_lote,reg.apli_int_viv_2008/1000000,reg.valor_aivs,reg.n_rfc_entidad)
                END IF

                IF vinte_ext_viv_92 > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vinte_ext_viv_92,14,4,vfentcons,reg.consec_reg_lote,reg.apli_int_ext_viv_92/1000000,reg.valor_aivs,reg.n_rfc_entidad)

CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vinte_ext_viv_92,14,6,vfentcons,reg.consec_reg_lote,reg.apli_int_ext_viv_92/1000000,reg.valor_aivs,reg.n_rfc_entidad)
                END IF

                IF vinte_ext_viv_2008 > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vinte_ext_viv_2008,35,4,vfentcons,reg.consec_reg_lote,reg.apli_int_ext_viv_2008/1000000,reg.valor_aivs,reg.n_rfc_entidad)

CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,vcoduni,reg.fecha_pago,reg.fecha_rviv,vinte_ext_viv_2008,35,6,vfentcons,reg.consec_reg_lote,reg.apli_int_ext_viv_2008/1000000,reg.valor_aivs,reg.n_rfc_entidad)
                END IF

             END IF

             --- DISPERSA COMISIONES EN dis_provision ---    
             IF vimpt_ret_isss > 0 THEN
CALL Dispersa_comision(vfolio,reg.nss,reg.curp,vcoduni,"COM_ISSS" ,reg.fecha_pago,reg.fecha_rviv,vimpt_ret_isss,30,vfentcons,reg.consec_reg_lote)
             END IF

             IF vimpt_cv_isss_pat > 0 THEN
CALL Dispersa_comision(vfolio,reg.nss,reg.curp,vcoduni,"COM_ISS_CVP" ,reg.fecha_pago,reg.fecha_rviv,vimpt_cv_isss_pat,31,vfentcons,reg.consec_reg_lote)
             END IF

             IF vimpt_cv_isss_tra > 0 THEN
CALL Dispersa_comision(vfolio,reg.nss,reg.curp,vcoduni,"COM_ISS_CVT" ,reg.fecha_pago,reg.fecha_rviv,vimpt_cv_isss_tra,31,vfentcons,reg.consec_reg_lote)
             END IF
          END IF

          LET vimpt_sar_isss        = 0
          LET vimpt_ret_isss        = 0
          LET vimpt_cv_isss_pat     = 0
          LET vimpt_cv_isss_tra     = 0
          LET vinte_ext_sar_isss    = 0
          LET vinte_ext_ret_isss    = 0
          LET vinte_ext_cv_isss_pat = 0
          LET vinte_ext_cv_isss_tra = 0
          LET vimpt_viv_92          = 0
          LET vimpt_viv_2008        = 0
          LET vinte_ext_viv_92      = 0
          LET vinte_ext_viv_2008    = 0
          LET vapli_int_viv_92      = 0
          LET vapli_int_viv_2008    = 0
          LET vapli_int_gar_92      = 0
          LET vapli_int_gar_2008    = 0
          LET vimpt_viv_gar_92      = 0
          LET vimpt_viv_gar_2008    = 0
          LET vimpt_cs_isss         = 0
          LET vimpt_aport_vol       = 0
          LET vimpt_aport_compl     = 0
          LET vimpt_aport_lplazo    = 0
          LET vimpt_ahr_sol_tra     = 0
          LET vimpt_ahr_sol_dep     = 0

          INITIALIZE reg TO NULL
       END IF
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


FUNCTION Cobra_comision(vfentcons,vperiodo_pago,vnss,vtipo_solicitud)  
   DEFINE
      vfentcons          DATE,
      vperiodo_pago      CHAR(06),
      vnss               CHAR(11),
      vtipo_solicitud    SMALLINT,
      vano               SMALLINT,
      vmes               SMALLINT,
      bim_cert           SMALLINT,
      bim_pago           SMALLINT

   DEFINE
      ano                INTEGER,
      anox               CHAR(04),
      mesx               CHAR(02),
      bim                CHAR(06)

   LET vano = YEAR(vfentcons)
   LET vmes = MONTH(vfentcons)
   CALL mes_a_bim(vano,vmes,"C") RETURNING bim_cert

   -- si tipo sol 5 se resta un bim al bim cert
   IF (vtipo_solicitud = 5 OR vtipo_solicitud = 1) THEN
      LET bim = bim_cert - 1

      IF bim[5] = "0" THEN
         LET ano  = bim[1,4]
         LET ano  = ano - 1
         LET anox = ano
         LET mesx = "6"
         LET bim  = anox,mesx
      END IF

      LET bim_cert = bim
   END IF

   LET vano = vperiodo_pago[1,4]
   LET vmes = vperiodo_pago[5,6]
   CALL mes_a_bim(vano,vmes,"P") RETURNING bim_pago

   IF bim_cert <= bim_pago THEN
      LET vbim_cert = bim_cert
      LET vbim_pago = bim_pago
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF
END FUNCTION


FUNCTION mes_a_bim(anyo,mes,tipo)
   DEFINE 
       anyo         INTEGER,
       mes          INTEGER,
       mes_b        INTEGER,
       bimestre     INTEGER,
       tipo         CHAR(01)
   
    LET mes_b = (mes + 1) / 2

----    Comentado el periodo de gracia para AAD
----    IF tipo = "C" THEN 
----       LET mes_b = mes_b - 1 
----       IF mes_b = 0 THEN
----          LET mes_b = 6
----          LET anyo = anyo - 1
----       END IF
----    END IF

    LET bimestre = (anyo * 10) + mes_b

    RETURN bimestre 
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
      vmonto             DECIMAL(16,6),
      vsubcuenta         SMALLINT,
      vmovimiento        SMALLINT,
      vfentcons          DATE,
      vconsec_reg_lote   INTEGER,
      vmonto_part_viv    DECIMAL(18,6),
      vmonto_valor_part  DECIMAL(21,14),
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

   LET xfecha_archivo = vfecha_lote_isss[5,6],"/",
                        vfecha_lote_isss[7,8],"/",
                        vfecha_lote_isss[1,4]
   LET vfecha_archivo = xfecha_archivo


   PREPARE claexe4 FROM cla_apo              --c22-6
   DECLARE cur_prov CURSOR FOR claexe4       --c22-6
   OPEN    cur_prov USING vnss, vsubcuenta
   FOREACH cur_prov INTO  vsiefore, vporcentaje_sie

      LET vcomision = vmonto * (vporcentaje_sie / 100)

      IF vmovimiento = 7 OR vmovimiento = 6  THEN     --para 43 bis
         LET vcomision = vcomision * -1
         LET vmonto_part_viv = vmonto_part_viv * -1   ----jerry
      END IF

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
      LET abo.monto_en_pesos    = vcomision          -- SI
      LET abo.monto_en_acciones = vmonto_part_viv    -- SI
      LET abo.precio_accion     = vmonto_valor_part  -- SI
      LET abo.dias_cotizados    = 0                  -- NO
      LET abo.sucursal          = vcoduni            -- SI
      LET abo.id_aportante      = vn_rfc_entidad     -- SI 
      LET abo.estado            = 5              -- SI 
      LET abo.fecha_proceso     = TODAY          -- SI
      LET abo.usuario           = gusuario       -- SI
      LET abo.fecha_archivo     = vfecha_archivo -- NO
      LET abo.etiqueta          = 0              -- SI

      SELECT razon_social                   --c22-6
      INTO   vid_sie                        --c22-6
      FROM   tmp_siefore_local              --c22-6
      WHERE  codigo_siefore = abo.siefore   --c22-6

      INSERT INTO tmp_dis_provision VALUES 
         (abo.siefore,          --c22-6
          abo.subcuenta,        --c22-6
          abo.tipo_movimiento,  --c22-6
          vporcentaje_sie,      --c22-6
          vid_sie,              --c22-6
          abo.monto_en_pesos)   --c22-6
   
      INSERT INTO dis_provision VALUES(abo.*)

      IF STATUS < 0 THEN
         ERROR "ERROR AL INSERTAR REGISTRO EN dis_provision ",STATUS USING '-----'
         EXIT PROGRAM
      END IF

   END FOREACH  --c22-6
END FUNCTION


FUNCTION Dispersa_comision(vfolio,vnss,vcurp,vcoduni,vreg_pat,cfecha_pago,
         cfecha_rviv,vmonto,vsubcuenta,vfentcons,vconsec_reg_lote)
   DEFINE
      vfolio              INTEGER,
      vnss                CHAR(11),
      vcurp               CHAR(18),
      vsua                CHAR(06),
      vcoduni             CHAR(10),
      vreg_pat            CHAR(11),
      cfecha_pago         CHAR(08),
      cfecha_rviv         CHAR(08),
      vfecha_pago         DATE,
      vfecha_rcv          DATE,
      vfecha_archivo      DATE,
      xfecha_pago         CHAR(10),
      xfecha_rcv          CHAR(10),
      xfecha_archivo      CHAR(10),
      vmonto              DECIMAL(16,6),
      vsubcuenta          SMALLINT,
      vfentcons           DATE,
      vfecha_1a_afil      DATE,
      vconsec_reg_lote    INTEGER,
      vcomision           DECIMAL(16,6),
      vcomision2          DECIMAL(16,6),
      vtipo               SMALLINT,
      vval_porcentaje     DECIMAL(16,6),
      vsiefore            SMALLINT,
      vtipo_comision      SMALLINT,
      vporcentaje_sie     DECIMAL(5,2),
      vdias_antiguedad    INTEGER,
      per_pago_aporte     CHAR(6),
      abo RECORD LIKE     dis_provision.*

   LET xfecha_pago = cfecha_pago[5,6],"/",
	             cfecha_pago[7,8],"/",
	             cfecha_pago[1,4]
   LET vfecha_pago = xfecha_pago

   LET xfecha_rcv = cfecha_rviv[5,6],"/",
                    cfecha_rviv[7,8],"/",
	            cfecha_rviv[1,4]
   LET vfecha_rcv = xfecha_rcv

   LET xfecha_archivo = vfecha_lote_isss[5,6],"/",
                        vfecha_lote_isss[7,8],"/",
                        vfecha_lote_isss[1,4]
   LET vfecha_archivo = xfecha_archivo

   SELECT "OK"
   FROM afi_mae_afiliado
   WHERE n_seguro = vnss

   IF STATUS <> NOTFOUND THEN 
      SELECT fentcons, fecha_1a_afil
      INTO vfentcons, vfecha_1a_afil
      FROM afi_mae_afiliado
      WHERE n_seguro = vnss
   END IF
   
   IF vfecha_1a_afil < vfentcons THEN
      -- LET vdias_antiguedad = gfecha_recepcion - vfecha_1a_afil  --c7.6--v5
      CALL cal_dias(vfecha_1a_afil, gfecha_recepcion)
      RETURNING vdias_antiguedad
   ELSE
      -- LET vdias_antiguedad = gfecha_recepcion - vfentcons       --c7.6--v5
      CALL cal_dias(vfentcons, gfecha_recepcion)
      RETURNING vdias_antiguedad
   END IF

   IF vdias_antiguedad < 0  OR vdias_antiguedad IS NULL THEN
      LET vdias_antiguedad = 0
   END IF

   PREPARE claexe5 FROM cla_com              --c22-6
   DECLARE cur_com2 CURSOR FOR claexe5       --c22-6
   FOREACH cur_com2 USING vsubcuenta,        --c22-6 
                          vdias_antiguedad,  --c22-6 
                          vdias_antiguedad,  --c22-6 
                          vnss               --c22-6 
                    INTO  vtipo,             --c22-6
                          vval_porcentaje,   --c22-6
                          vsiefore,          --c22-6
                          vporcentaje_sie,   --c22-6
                          vtipo_comision     --c22-6
      
--    LET vcomision = (((0.0103/0.065))*(1-(168.28/vmonto)))*vmonto

      LET aux_cuo_soc      = 0
      LET vcomision        = 0

      SELECT periodo_pago
      INTO per_pago_aporte
      FROM dis_det_issste
      WHERE folio = vfolio
      AND   consec_reg_lote = vconsec_reg_lote

      SELECT val_cuo_soc
      INTO   aux_cuo_soc
      FROM   factor_cuo_soc
      WHERE  periodo_pago = per_pago_aporte

      IF vcod_afore= 516 THEN   -- Afore XXI 516
         LET vcomision = ((((vval_porcentaje/100)*(1-(aux_cuo_soc/vmonto))) *
                             vmonto))*(0.02/0.065) * vtipo
      ELSE
         LET vcomision = (((vval_porcentaje/100)*(1-(aux_cuo_soc/vmonto))) *
                            vmonto) * vtipo
      END IF

      IF vcod_afore= 564 OR vcod_afore= 574 THEN  #METLIFE Y SCOTIA NO COMISION
	 LET vcomision = 1
      END IF

      IF vcod_afore = 562 THEN     #INVERCAP SOLO COMISION periodo 200706
         IF per_pago_aporte <> 200706 THEN
            LET vcomision = 1
         END IF
      END IF

      IF vcod_afore = 558 THEN     #ACTINVER SOLO COMISION periodos <= 200705
         IF per_pago_aporte > 200705 THEN
            LET vcomision = 1
         END IF
      END IF

      IF vcomision >= 0 THEN
         LET vcomision = 0
      ELSE
         LET abo.tipo_movimiento   = vtipo_comision -- SI
         LET abo.subcuenta         = vsubcuenta     -- SI
         LET abo.siefore           = vsiefore       -- SI
         LET abo.folio             = vfolio         -- SI 
         LET abo.consecutivo_lote  = vconsec_reg_lote
         LET abo.nss               = vnss           -- SI
         LET abo.curp              = vcurp          -- NO
         LET abo.folio_sua         = vsua           -- SI
         LET abo.fecha_pago        = vfecha_pago    -- NO
         LET abo.fecha_valor       = vfecha_rcv     -- SI
         LET abo.fecha_conversion  = NULL           -- SI
         LET abo.monto_en_pesos    = vcomision      -- SI
         LET abo.monto_en_acciones = 0              -- SI
         LET abo.precio_accion     = 0              -- SI
         LET abo.dias_cotizados    = 0              -- NO
         LET abo.sucursal          = vcoduni        -- SI
         LET abo.id_aportante      = vreg_pat       -- SI 
         LET abo.estado            = 5              -- SI 
         LET abo.fecha_proceso     = TODAY          -- SI
         LET abo.usuario           = gusuario       -- SI
         LET abo.fecha_archivo     = vfecha_archivo -- NO
         LET abo.etiqueta          = 0              -- SI

         SELECT razon_social                   --c22-6
         INTO   vid_sie                        --c22-6
         FROM   tmp_siefore_local              --c22-6
         WHERE  codigo_siefore = abo.siefore   --c22-6

         INSERT INTO tmp_dis_provision VALUES
         (abo.siefore,          --c22-6
          abo.subcuenta,        --c22-6
          abo.tipo_movimiento,  --c22-6
          vporcentaje_sie,      --c22-6
          vid_sie,              --c22-6
          abo.monto_en_pesos)   --c22-6

         INSERT INTO dis_provision VALUES(abo.*)

         IF STATUS < 0 THEN
            ERROR "ERROR AL INSERTAR COMISIONES EN dis_provision ",STATUS USING '-------'
            EXIT PROGRAM
         END IF

         LET vcomision2 = vcomision

         IF vsubcuenta = 13 or vsubcuenta = 19 THEN
            LET tot_comi.impt_ret_isss = tot_comi.impt_ret_isss + vcomision2
         END IF
      END IF
   END FOREACH 

   LET vdias_antiguedad = 0
END FUNCTION


FUNCTION Obtiene_dias_bimes_ant(vfolio)
  DEFINE bimes_ant    INTEGER
  DEFINE vfolio       INTEGER
  DEFINE xfecha       CHAR(08)
  DEFINE mes          INTEGER
  DEFINE anno         INTEGER
  DEFINE anno_x       INTEGER

  SELECT fech_creac_lote
  INTO   xfecha
  FROM   dis_cza_issste
  WHERE  folio = vfolio

  IF xfecha is not null THEN
     LET mes       = xfecha[5,6]
     LET bimes_ant = ((mes + 1) / 2)-1
     LET anno_x    = xfecha[1,4]

     IF bimes_ant = 0 THEN
        LET anno      = anno_x - 1
        LET bimes_ant = 6
     ELSE
        LET anno = anno_x
     END IF 

     CASE bimes_ant
         WHEN 1
            LET val_dias_cot = DAY(MDY(2,1,anno)-1)  +
                               DAY(MDY(3,1,anno)-1)
         WHEN 2
            LET val_dias_cot = DAY(MDY(4,1,anno)-1)  +
                               DAY(MDY(5,1,anno)-1)
         WHEN 3
            LET val_dias_cot = DAY(MDY(6,1,anno)-1)  +
                               DAY(MDY(7,1,anno)-1)
         WHEN 4
            LET val_dias_cot = DAY(MDY(8,1,anno)-1)  +
                               DAY(MDY(9,1,anno)-1)
         WHEN 5
            LET val_dias_cot = DAY(MDY(10,1,anno)-1) +
                               DAY(MDY(11,1,anno)-1)
         WHEN 6
            LET val_dias_cot = DAY(MDY(12,1,anno)-1) +
                               31
     END CASE
  ELSE
     LET  val_dias_cot = 0
  END IF

  display "num de dias del bimestre:",val_dias_cot
END FUNCTION


FUNCTION Sumatoria_rechazo(vimpt_sar_isss,
                           vimpt_ret_isss,
                           vimpt_cv_isss_pat,
                           vimpt_cv_isss_tra,
                           vinte_ext_sar_isss,
                           vinte_ext_ret_isss,
                           vinte_ext_cv_isss_pat,
                           vinte_ext_cv_isss_tra,
                           vimpt_viv_92,
                           vimpt_viv_2008,
                           vinte_ext_viv_92,
                           vinte_ext_viv_2008,
                           vimpt_viv_gar_92,
                           vimpt_viv_gar_2008,
                           vapli_int_viv_92,
                           vapli_int_viv_2008,
                           vapli_int_gar_92,
                           vapli_int_gar_2008,
                           vimpt_cs_isss,
                           vimpt_aport_vol,
                           vimpt_aport_compl,
                           vimpt_aport_lplazo,
                           vimpt_ahr_sol_tra,
                           vimpt_ahr_sol_dep)

   DEFINE      
     vimpt_sar_isss        DECIMAL(15,2),
     vimpt_ret_isss        DECIMAL(15,2),
     vimpt_cv_isss_pat     DECIMAL(15,2),
     vimpt_cv_isss_tra     DECIMAL(15,2),
     vinte_ext_sar_isss    DECIMAL(15,2),
     vinte_ext_ret_isss    DECIMAL(15,2),
     vinte_ext_cv_isss_pat DECIMAL(15,2),
     vinte_ext_cv_isss_tra DECIMAL(15,2),
     vimpt_viv_92          DECIMAL(15,2),
     vimpt_viv_2008        DECIMAL(15,2),
     vinte_ext_viv_92      DECIMAL(15,2),
     vinte_ext_viv_2008    DECIMAL(15,2),
     vimpt_viv_gar_92      DECIMAL(15,2),
     vimpt_viv_gar_2008    DECIMAL(15,2),
     vapli_int_viv_92      DECIMAL(18,6),
     vapli_int_viv_2008    DECIMAL(18,6),
     vapli_int_gar_92      DECIMAL(18,6),
     vapli_int_gar_2008    DECIMAL(18,6),
     vimpt_cs_isss         DECIMAL(15,2),
     vimpt_aport_vol       DECIMAL(15,2),
     vimpt_aport_compl     DECIMAL(15,2),
     vimpt_aport_lplazo    DECIMAL(15,2),
     vimpt_ahr_sol_tra     DECIMAL(15,2),
     vimpt_ahr_sol_dep     DECIMAL(15,2)

LET tot_rech.impt_sar_isss     = tot_rech.impt_sar_isss     + vimpt_sar_isss
LET tot_rech.impt_ret_isss     = tot_rech.impt_ret_isss     + vimpt_ret_isss
LET tot_rech.impt_cv_isss_pat  = tot_rech.impt_cv_isss_pat  + vimpt_cv_isss_pat
LET tot_rech.impt_cv_isss_tra  = tot_rech.impt_cv_isss_tra  + vimpt_cv_isss_tra
LET tot_rech.inte_ext_sar_isss = tot_rech.inte_ext_sar_isss + vinte_ext_sar_isss
LET tot_rech.inte_ext_ret_isss = tot_rech.inte_ext_ret_isss + vinte_ext_ret_isss
LET tot_rech.inte_ext_cv_isss_pat = tot_rech.inte_ext_cv_isss_pat +
                                    vinte_ext_cv_isss_pat
LET tot_rech.inte_ext_cv_isss_tra = tot_rech.inte_ext_cv_isss_tra +
                                    vinte_ext_cv_isss_tra
LET tot_rech.impt_cs_isss      = tot_rech.impt_cs_isss      + vimpt_cs_isss
LET tot_rech.impt_aport_vol    = tot_rech.impt_aport_vol    + vimpt_aport_vol
LET tot_rech.impt_aport_compl  = tot_rech.impt_aport_compl  + vimpt_aport_compl
LET tot_rech.impt_aport_lplazo = tot_rech.impt_aport_lplazo + vimpt_aport_lplazo
LET tot_rech.impt_ahr_sol_tra  = tot_rech.impt_ahr_sol_tra  + vimpt_ahr_sol_tra
LET tot_rech.impt_ahr_sol_dep  = tot_rech.impt_ahr_sol_dep  + vimpt_ahr_sol_dep

----VIVIENDA
LET tot_rech.impt_viv_92       = tot_rech.impt_viv_92       + vimpt_viv_92
LET tot_rech.inte_ext_viv_92   = tot_rech.inte_ext_viv_92   + vinte_ext_viv_92
LET tot_rech.impt_viv_gar_92   = tot_rech.impt_viv_gar_92   + vimpt_viv_gar_92
LET tot_rech.apli_int_viv_92   = tot_rech.apli_int_viv_92   + vapli_int_viv_92
LET tot_rech.apli_int_gar_92   = tot_rech.apli_int_gar_92   + vapli_int_gar_92

LET tot_rech.impt_viv_2008     = tot_rech.impt_viv_2008     + vimpt_viv_2008
LET tot_rech.inte_ext_viv_2008 = tot_rech.inte_ext_viv_2008 + vinte_ext_viv_2008
LET tot_rech.impt_viv_gar_2008 = tot_rech.impt_viv_gar_2008 + vimpt_viv_gar_2008
LET tot_rech.apli_int_viv_2008 = tot_rech.apli_int_viv_2008 + vapli_int_viv_2008
LET tot_rech.apli_int_gar_2008 = tot_rech.apli_int_gar_2008 + vapli_int_gar_2008

IF vident_viv_garantia <> "1" THEN  -- vivienda normal
   LET tot_rech.impt_viv_92   = tot_rech.impt_viv_92   + vinte_ext_viv_92 
   LET tot_rech.impt_viv_2008 = tot_rech.impt_viv_2008 + vinte_ext_viv_2008 
ELSE                                -- vivienda garantia
   LET tot_rech.impt_viv_gar_92  = tot_rech.impt_viv_gar_92 + vinte_ext_viv_92
   LET tot_rech.impt_viv_gar_2008= tot_rech.impt_viv_gar_2008+vinte_ext_viv_2008
END IF
 
END FUNCTION


FUNCTION Sumatoria_aceptado(vimpt_sar_isss,
                            vimpt_ret_isss,
                            vimpt_cv_isss_pat,
                            vimpt_cv_isss_tra,
                            vinte_ext_sar_isss,
                            vinte_ext_ret_isss,
                            vinte_ext_cv_isss_pat,
                            vinte_ext_cv_isss_tra,
                            vimpt_viv_92,
                            vimpt_viv_2008,
                            vinte_ext_viv_92,
                            vinte_ext_viv_2008,
                            vimpt_viv_gar_92,
                            vimpt_viv_gar_2008,
                            vapli_int_viv_92,
                            vapli_int_viv_2008,
                            vapli_int_gar_92,
                            vapli_int_gar_2008,
                            vimpt_cs_isss,
                            vimpt_aport_vol,
                            vimpt_aport_compl,
                            vimpt_aport_lplazo,
                            vimpt_ahr_sol_tra,
                            vimpt_ahr_sol_dep)

   DEFINE
      vimpt_sar_isss        DECIMAL(15,2),
      vimpt_ret_isss        DECIMAL(15,2),
      vimpt_cv_isss_pat     DECIMAL(15,2),
      vimpt_cv_isss_tra     DECIMAL(15,2),
      vinte_ext_sar_isss    DECIMAL(15,2),
      vinte_ext_ret_isss    DECIMAL(15,2),
      vinte_ext_cv_isss_pat DECIMAL(15,2),
      vinte_ext_cv_isss_tra DECIMAL(15,2),
      vimpt_viv_92          DECIMAL(15,2),
      vimpt_viv_2008        DECIMAL(15,2),
      vinte_ext_viv_92      DECIMAL(15,2),
      vinte_ext_viv_2008    DECIMAL(15,2),
      vimpt_viv_gar_92      DECIMAL(15,2),
      vimpt_viv_gar_2008    DECIMAL(15,2),
      vapli_int_viv_92      DECIMAL(18,6),
      vapli_int_viv_2008    DECIMAL(18,6),
      vapli_int_gar_92      DECIMAL(18,6),
      vapli_int_gar_2008    DECIMAL(18,6),
      vimpt_cs_isss         DECIMAL(15,2),
      vimpt_aport_vol       DECIMAL(15,2),
      vimpt_aport_compl     DECIMAL(15,2),
      vimpt_aport_lplazo    DECIMAL(15,2),
      vimpt_ahr_sol_tra     DECIMAL(15,2),
      vimpt_ahr_sol_dep     DECIMAL(15,2)

LET tot_acep.impt_sar_isss     = tot_acep.impt_sar_isss     + vimpt_sar_isss
LET tot_acep.impt_ret_isss     = tot_acep.impt_ret_isss     + vimpt_ret_isss
LET tot_acep.impt_cv_isss_pat  = tot_acep.impt_cv_isss_pat  + vimpt_cv_isss_pat
LET tot_acep.impt_cv_isss_tra  = tot_acep.impt_cv_isss_tra  + vimpt_cv_isss_tra
LET tot_acep.inte_ext_sar_isss = tot_acep.inte_ext_sar_isss + vinte_ext_sar_isss
LET tot_acep.inte_ext_ret_isss = tot_acep.inte_ext_ret_isss + vinte_ext_ret_isss
LET tot_acep.inte_ext_cv_isss_pat = tot_acep.inte_ext_cv_isss_pat +
                                    vinte_ext_cv_isss_pat
LET tot_acep.inte_ext_cv_isss_tra = tot_acep.inte_ext_cv_isss_tra +
                                    vinte_ext_cv_isss_tra
LET tot_acep.impt_cs_isss      = tot_acep.impt_cs_isss      + vimpt_cs_isss
LET tot_acep.impt_aport_vol    = tot_acep.impt_aport_vol    + vimpt_aport_vol
LET tot_acep.impt_aport_compl  = tot_acep.impt_aport_compl  + vimpt_aport_compl
LET tot_acep.impt_aport_lplazo = tot_acep.impt_aport_lplazo + vimpt_aport_lplazo
LET tot_acep.impt_ahr_sol_tra  = tot_acep.impt_ahr_sol_tra  + vimpt_ahr_sol_tra
LET tot_acep.impt_ahr_sol_dep  = tot_acep.impt_ahr_sol_dep  + vimpt_ahr_sol_dep

----VIVIENDA
LET tot_acep.impt_viv_92       = tot_acep.impt_viv_92       + vimpt_viv_92
LET tot_acep.inte_ext_viv_92   = tot_acep.inte_ext_viv_92   + vinte_ext_viv_92
LET tot_acep.impt_viv_gar_92   = tot_acep.impt_viv_gar_92   + vimpt_viv_gar_92
LET tot_acep.apli_int_viv_92   = tot_acep.apli_int_viv_92   + vapli_int_viv_92
LET tot_acep.apli_int_gar_92   = tot_acep.apli_int_gar_92   + vapli_int_gar_92

LET tot_acep.impt_viv_2008     = tot_acep.impt_viv_2008     + vimpt_viv_2008
LET tot_acep.inte_ext_viv_2008 = tot_acep.inte_ext_viv_2008 + vinte_ext_viv_2008
LET tot_acep.impt_viv_gar_2008 = tot_acep.impt_viv_gar_2008 + vimpt_viv_gar_2008
LET tot_acep.apli_int_viv_2008 = tot_acep.apli_int_viv_2008 + vapli_int_viv_2008
LET tot_acep.apli_int_gar_2008 = tot_acep.apli_int_gar_2008 + vapli_int_gar_2008

IF vident_viv_garantia <> "1" THEN  -- vivienda normal
   LET tot_acep.impt_viv_92   = tot_acep.impt_viv_92   + vinte_ext_viv_92
   LET tot_acep.impt_viv_2008 = tot_acep.impt_viv_2008 + vinte_ext_viv_2008
ELSE                                -- vivienda garantia
   LET tot_acep.impt_viv_gar_92  = tot_acep.impt_viv_gar_92 + vinte_ext_viv_92
   LET tot_acep.impt_viv_gar_2008= tot_acep.impt_viv_gar_2008+vinte_ext_viv_2008
END IF

END FUNCTION


FUNCTION Genera_confaf(vfolio)          -- ETAPA 7 
   DEFINE   
      vfolio                INTEGER,
      ejecuta               CHAR(200),
      tipo_ar               SMALLINT,
      rident_pago           CHAR(16)

   DEFINE
      total_rcv             DECIMAL(16,2),
      total_vol             DECIMAL(16,2),
      total_compl           DECIMAL(16,2),
      total_viv_92          DECIMAL(16,2),
      total_viv_2008        DECIMAL(16,2),
      total_gub             DECIMAL(16,2),
      total_viv_gar_92      DECIMAL(16,2),
      total_viv_gar_2008    DECIMAL(16,2),

      acept_sar_isss        DECIMAL(16,2),
      acept_ret_isss        DECIMAL(16,2),
      acept_cv_isss_pat     DECIMAL(16,2),
      acept_cv_isss_tra     DECIMAL(16,2),
      acept_ext_sar_isss    DECIMAL(16,2),
      acept_ext_ret_isss    DECIMAL(16,2),
      acept_ext_cv_isss_pat DECIMAL(16,2),
      acept_ext_cv_isss_tra DECIMAL(16,2),
      acept_viv_92          DECIMAL(16,2),
      acept_viv_2008        DECIMAL(16,2),
      acept_ext_viv_92      DECIMAL(16,2),
      acept_ext_viv_2008    DECIMAL(16,2),
      acept_cs_isss         DECIMAL(16,2),
      acept_vol             DECIMAL(16,2),
      acept_compl           DECIMAL(16,2),
      acept_lplazo          DECIMAL(16,2),
      acept_ahr_sol_tra     DECIMAL(16,2),
      acept_ahr_sol_dep     DECIMAL(16,2),
      acept_part_92         DECIMAL(18,6),
      acept_part_2008       DECIMAL(18,6),
      acept_viv_gar_92      DECIMAL(16,2),
      acept_viv_gar_2008    DECIMAL(16,2),
      acept_part_gar_92     DECIMAL(18,6),
      acept_part_gar_2008   DECIMAL(18,6),

      recha_sar_isss        DECIMAL(16,2),
      recha_ret_isss        DECIMAL(16,2),
      recha_cv_isss_pat     DECIMAL(16,2),
      recha_cv_isss_tra     DECIMAL(16,2),
      recha_ext_sar_isss    DECIMAL(16,2),
      recha_ext_ret_isss    DECIMAL(16,2),
      recha_ext_cv_isss_pat DECIMAL(16,2),
      recha_ext_cv_isss_tra DECIMAL(16,2),
      recha_viv_92          DECIMAL(16,2),
      recha_viv_2008        DECIMAL(16,2),
      recha_ext_viv_92      DECIMAL(16,2),
      recha_ext_viv_2008    DECIMAL(16,2),
      recha_cs_isss         DECIMAL(16,2),
      recha_vol             DECIMAL(16,2),
      recha_compl           DECIMAL(16,2),
      recha_lplazo          DECIMAL(16,2),
      recha_ahr_sol_tra     DECIMAL(16,2),
      recha_ahr_sol_dep     DECIMAL(16,2),
      recha_part_92         DECIMAL(18,6),
      recha_part_2008       DECIMAL(18,6),
      recha_viv_gar_92      DECIMAL(16,2),
      recha_viv_gar_2008    DECIMAL(16,2),
      recha_part_gar_92     DECIMAL(18,6),
      recha_part_gar_2008   DECIMAL(18,6)

   DEFINE
      vidp                  CHAR(02),         --c22-6
      vsub                  CHAR(08),         --c22-6
      vsie                  SMALLINT,         --c22-6
      vpor                  DECIMAL(16,8),    --c22-6
      vident_sie            CHAR(08),         --c22-6
      vpesos                DECIMAL(16,6),    --c22-6
      vcuantos              SMALLINT,         --c22-6.3
      vcontador_sie         SMALLINT,         --c22-6.3
      vsuma                 DECIMAL(16,6),    --c22-6
      vimporte              DECIMAL(16,2),    --c22-6
      vimporte2             DECIMAL(16,2),    --c22-6.9
      vrechazo              DECIMAL(16,2)     --c22-6

   DEFINE g_dep RECORD LIKE dis_dep_issste.*

   ------------------------- TOTAL ACEPTADO APORTES ------------------------
   LET acept_sar_isss        = tot_acep.impt_sar_isss
   LET acept_ret_isss        = tot_acep.impt_ret_isss
   LET acept_cv_isss_pat     = tot_acep.impt_cv_isss_pat
   LET acept_cv_isss_tra     = tot_acep.impt_cv_isss_tra
   LET acept_ext_sar_isss    = tot_acep.inte_ext_sar_isss
   LET acept_ext_ret_isss    = tot_acep.inte_ext_ret_isss
   LET acept_ext_cv_isss_pat = tot_acep.inte_ext_cv_isss_pat
   LET acept_ext_cv_isss_tra = tot_acep.inte_ext_cv_isss_tra
   LET acept_viv_92          = tot_acep.impt_viv_92
   LET acept_viv_2008        = tot_acep.impt_viv_2008
   LET acept_ext_viv_92      = tot_acep.inte_ext_viv_92
   LET acept_ext_viv_2008    = tot_acep.inte_ext_viv_2008
   LET acept_cs_isss         = tot_acep.impt_cs_isss
   LET acept_vol             = tot_acep.impt_aport_vol
   LET acept_compl           = tot_acep.impt_aport_compl
   LET acept_lplazo          = tot_acep.impt_aport_lplazo
   LET acept_ahr_sol_tra     = tot_acep.impt_ahr_sol_tra
   LET acept_ahr_sol_dep     = tot_acep.impt_ahr_sol_dep
   LET acept_viv_gar_92      = tot_acep.impt_viv_gar_92
   LET acept_viv_gar_2008    = tot_acep.impt_viv_gar_2008

   LET acept_part_92         = tot_acep.apli_int_viv_92
   LET acept_part_2008       = tot_acep.apli_int_viv_2008
   LET acept_part_gar_92     = tot_acep.apli_int_gar_92
   LET acept_part_gar_2008   = tot_acep.apli_int_gar_2008

   ------------------------- TOTAL RECHAZOS APORTES ------------------------
   LET recha_sar_isss        = tot_rech.impt_sar_isss
   LET recha_ret_isss        = tot_rech.impt_ret_isss
   LET recha_cv_isss_pat     = tot_rech.impt_cv_isss_pat
   LET recha_cv_isss_tra     = tot_rech.impt_cv_isss_tra
   LET recha_ext_sar_isss    = tot_rech.inte_ext_sar_isss
   LET recha_ext_ret_isss    = tot_rech.inte_ext_ret_isss
   LET recha_ext_cv_isss_pat = tot_rech.inte_ext_cv_isss_pat
   LET recha_ext_cv_isss_tra = tot_rech.inte_ext_cv_isss_tra
   LET recha_viv_92          = tot_rech.impt_viv_92
   LET recha_viv_2008        = tot_rech.impt_viv_2008
   LET recha_ext_viv_92      = tot_rech.inte_ext_viv_92
   LET recha_ext_viv_2008    = tot_rech.inte_ext_viv_2008
   LET recha_cs_isss         = tot_rech.impt_cs_isss
   LET recha_vol             = tot_rech.impt_aport_vol
   LET recha_compl           = tot_rech.impt_aport_compl
   LET recha_lplazo          = tot_rech.impt_aport_lplazo
   LET recha_ahr_sol_tra     = tot_rech.impt_ahr_sol_tra
   LET recha_ahr_sol_dep     = tot_rech.impt_ahr_sol_dep
   LET recha_viv_gar_92      = tot_rech.impt_viv_gar_92
   LET recha_viv_gar_2008    = tot_rech.impt_viv_gar_2008

   LET recha_part_92         = tot_rech.apli_int_viv_92
   LET recha_part_2008       = tot_rech.apli_int_viv_2008
   LET recha_part_gar_92     = tot_rech.apli_int_gar_92
   LET recha_part_gar_2008   = tot_rech.apli_int_gar_2008

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
   SELECT "41",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub IN (30,31)                                           --c22-6
   AND    mov IN (1,2,3,100,101,102,103,104,105,106,107,108,109,111,
                  112,113,114,115,116,117,118,119)                 --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6
   
   ---- COMISION RCV ----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "11",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub IN (30,31)                                           --c22-6
   AND    mov IN (100,101,102,103,104,105,106,107,108,109,111,112,
                  113,114,115,116,117,118,119)                     --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6

   ---- APORTE SAR ISSSTE ----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "43",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub IN (13)                                              --c22-6
   AND    mov IN (1,2,3,100,101,102,103,104,105,106,107,108,109,111,
                  112,113,114,115,116,117,118,119)                 --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6

   ---- APORTE CUO SOC ISSSTE ----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "48",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub IN (32)                                              --c22-6
   AND    mov IN (1,2,3)                                           --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6

   ---- APORTE AHORRO SOLIDARIO ----
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "47",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub IN (33,34)                                           --c22-6
   AND    mov IN (1,2,3)                                           --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6
 
   ---- APORTE  VOL ----  ## el 81 es por Layout 08/feb/2011
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "81",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub = 3                                                  --c22-6
   AND    mov IN (1,2)                                             --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6

   ---- APORTE COMPL ---- ## el 82 es por Layout 08/feb/2011
   INSERT INTO tmp_res_provision                                   --c22-6
   SELECT "82",                                                    --c22-6
          sie,                                                     --c22-6
          por,                                                     --c22-6
          idsie,                                                   --c22-6
          sum(pesos)                                               --c22-6
   FROM   tmp_dis_provision                                        --c22-6
   WHERE  sub = 11                                                 --c22-6
   AND    mov IN (1,2)                                             --c22-6
   GROUP  BY 1,2,3,4                                               --c22-6

   ---- APORTE AHORRO LARGO PLAZO ---- ## el 83 por Layout 08/feb/2011
   INSERT INTO tmp_res_provision                                   --c22-11
   SELECT "83",                                                    --c22-11
          sie,                                                     --c22-11
          por,                                                     --c22-11
          idsie,                                                   --c22-11
          sum(pesos)                                               --c22-11
   FROM   tmp_dis_provision                                        --c22-11
   WHERE  sub = 15                                                 --c22-112
   AND    mov IN (1,2)                                             --c22-11
   GROUP  BY 1,2,3,4                                               --c22-11

   -------- Obtiene aportes de tmp_res_provision --------
   SELECT count(*) INTO vcuantos FROM tmp_res_provision
   IF vcuantos = 0 THEN                        # Archivo Completo Rechazado
      DECLARE cur_noacep CURSOR FOR
      SELECT ident_pago[14,15] FROM dis_dep_issste
      WHERE folio = vfolio
      AND   ident_pago[14,15] NOT IN (42,44,45,46)
   
      FOREACH cur_noacep INTO vidp
         UPDATE dis_dep_issste                    
         SET    importe          = importe / 100, 
                impt_aport_acept = 0,        
                impt_aport_dev   = importe / 100,
                apli_viv_env     = 0,             
                apli_viv_acept   = 0,             
                apli_viv_dev     = 0,             
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

    --LET vpor = vpesos / vsuma             --c22-113

      CASE
         WHEN vsub = "41"
            LET vrechazo = recha_ret_isss     + recha_ext_ret_isss    +
                           recha_cv_isss_pat  + recha_ext_cv_isss_pat +
                           recha_cv_isss_tra  + recha_ext_cv_isss_tra 
         WHEN vsub = "11"
            LET vrechazo = 0
         WHEN vsub = "43"
            LET vrechazo = recha_sar_isss     + recha_ext_sar_isss
         WHEN vsub = "81"
            LET vrechazo = recha_vol
         WHEN vsub = "82"
            LET vrechazo = recha_compl
         WHEN vsub = "83"
            LET vrechazo = recha_lplazo
         WHEN vsub = "47"
            LET vrechazo = recha_ahr_sol_tra + recha_ahr_sol_dep
         WHEN vsub = "48"
            LET vrechazo = recha_cs_isss
      END CASE

--    IF vsie = 1 THEN                                         --c22-6.3

      SELECT *                                                 --c22-6
      INTO   g_dep.*
      FROM   dis_dep_issste                                    --c22-6
      WHERE  folio             = vfolio                        --c22-6
      AND    ident_pago[14,15] = vsub                          --c22-6
      AND    tipo_siefore      = 0                             --c22-6
      AND    ident_siefore     = ""

      IF vcuantos = 1 THEN                                     --c22-6.3

         UPDATE dis_dep_issste                                 --c22-6
         SET    importe          = importe / 100,              --c22-6.5
                impt_aport_acept = vpesos,                     --c22-6
                impt_aport_dev   = 0,  --vrechazo,             --c22-6.9
                apli_viv_env     = 0,                          --c22-6
                apli_viv_acept   = 0,                          --c22-6
                apli_viv_dev     = 0,                          --c22-6
                ident_siefore    = vident_sie,                 --c22-6
                tipo_siefore     = vsie                        --c22-6
         WHERE  folio            = vfolio                      --c22-6
         AND    ident_pago[14,15]= vsub                        --c22-6
         AND    tipo_siefore     = 0                           --c22-6
 
         ---VOL
         IF vsub = 81 THEN                                     --c22-6.6
            UPDATE dis_dep_issste                              --c22-6.6
            SET    ident_siefore    = vident_sie,              --c22-6.6
                   tipo_siefore     = vsie                     --c22-6.6
            WHERE  folio            = vfolio                   --c22-6.6
            AND    ident_pago[14,15]= "13"                     --c22-6.6
            AND    tipo_siefore     = 0                        --c22-6.6
         END IF

         ---COMPLE RET
         IF vsub = "82" THEN                                   --c22-111
            UPDATE dis_dep_issste                              --c22-111
            SET    ident_siefore    = vident_sie,              --c22-111
                   tipo_siefore     = vsie                     --c22-111
            WHERE  folio            = vfolio                   --c22-111
            AND    ident_pago[14,15]= "14"                     --c22-111
            AND    tipo_siefore     = 0                        --c22-111
         END IF

         ---AP. LPLAZO
         IF vsub = "83" THEN                                   --c22-111
            UPDATE dis_dep_issste                              --c22-111
            SET    ident_siefore    = vident_sie,              --c22-111
                   tipo_siefore     = vsie                     --c22-111
            WHERE  folio            = vfolio                   --c22-111
            AND    ident_pago[14,15]= "15"                     --c22-111
            AND    tipo_siefore     = 0                        --c22-111
         END IF

         IF vrechazo > 0 THEN                                  --c22-6.9
            LET vimporte2 = g_dep.importe / 100                --c22-6.9
            INSERT INTO dis_dep_issste VALUES                  --c22-6.9
              (                                                --c22-6.9
              g_dep.folio,                                     --c22-6.9
              g_dep.tipo_registro,                             --c22-6.9
              g_dep.ident_servicio,                            --c22-6.9
              g_dep.ident_pago,                                --c22-6.9
              vimporte2,                                       --c22-6.9
              g_dep.fech_liquidacion,                          --c22-6.9
              0,                                               --c22-6.9
              vrechazo,                                        --c22-6.9
              g_dep.apli_viv_env,                              --c22-6.9
              g_dep.apli_viv_acept,                            --c22-6.9
              g_dep.apli_viv_dev,                              --c22-6.9
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

            UPDATE dis_dep_issste                              --c22-6.3
            SET    importe          = importe / 100,           --c22-6.5
                   impt_aport_acept = vpesos,                  --c22-6.3
                   impt_aport_dev   = 0,  --vrechazo,          --c22-6.10
                   apli_viv_env     = 0,                       --c22-6.3
                   apli_viv_acept   = 0,                       --c22-6.3
                   apli_viv_dev     = 0,                       --c22-6.3
                   ident_siefore    = vident_sie,              --c22-6.3
                   tipo_siefore     = vsie                     --c22-6.3
            WHERE  folio            = vfolio                   --c22-6.3
            AND    ident_pago[14,15]= vsub                     --c22-6.3
            AND    tipo_siefore     = 0                        --c22-6.3

            ---VOL
            IF vsub = 81 THEN                                  --c22-6.6
               UPDATE dis_dep_issste                           --c22-6.6
               SET    ident_siefore    = vident_sie,           --c22-6.6
                      tipo_siefore     = vsie                  --c22-6.6
               WHERE  folio            = vfolio                --c22-6.6
               AND    ident_pago[14,15]= "13"                  --c22-6.6
               AND    tipo_siefore     = 0                     --c22-6.6
            END IF

            ---COMP RET
            IF vsub = "82" THEN                                --c22-111
               UPDATE dis_dep_issste                           --c22-111
               SET    ident_siefore    = vident_sie,           --c22-111
                      tipo_siefore     = vsie                  --c22-111
               WHERE  folio            = vfolio                --c22-111
               AND    ident_pago[14,15]= "14"                  --c22-111
               AND    tipo_siefore     = 0                     --c22-111
            END IF

            ---AP. LPLAZO
            IF vsub = "83" THEN                                --c22-111
               UPDATE dis_dep_issste                           --c22-111
               SET    ident_siefore    = vident_sie,           --c22-111
                      tipo_siefore     = vsie                  --c22-111
               WHERE  folio            = vfolio                --c22-111
               AND    ident_pago[14,15]= "15"                  --c22-111
               AND    tipo_siefore     = 0                     --c22-111
            END IF

            IF vrechazo > 0 THEN                               --c22-6.9
               LET vimporte2 = g_dep.importe / 100             --c22-6.9
               INSERT INTO dis_dep_issste VALUES               --c22-6.9
                 (                                             --c22-6.9
                 g_dep.folio,                                  --c22-6.9
                 g_dep.tipo_registro,                          --c22-6.9
                 g_dep.ident_servicio,                         --c22-6.9
                 g_dep.ident_pago,                             --c22-6.9
                 vimporte2,                                    --c22-6.9
                 g_dep.fech_liquidacion,                       --c22-6.9
                 0,                                            --c22-6.9
                 vrechazo,                                     --c22-6.9
                 g_dep.apli_viv_env,                           --c22-6.9
                 g_dep.apli_viv_acept,                         --c22-6.9
                 g_dep.apli_viv_dev,                           --c22-6.9
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

            LET g_dep.apli_viv_env   = 0                       --c22-6
            LET g_dep.apli_viv_acept = 0                       --c22-6
            LET g_dep.apli_viv_dev   = 0                       --c22-6
   
            INSERT INTO dis_dep_issste VALUES                  --c22-6
              (                                                --c22-6
              g_dep.folio,                                     --c22-6
              g_dep.tipo_registro,                             --c22-6
              g_dep.ident_servicio,                            --c22-6
              g_dep.ident_pago,                                --c22-6
              vimporte,                                        --c22-6
              g_dep.fech_liquidacion,                          --c22-6
              vpesos,                                          --c22-6
              vrechazo,                                        --c22-6
              g_dep.apli_viv_env,                              --c22-6
              g_dep.apli_viv_acept,                            --c22-6
              g_dep.apli_viv_dev,                              --c22-6
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
   FROM dis_dep_issste
   WHERE folio = vfolio
   AND   impt_aport_acept = 0
   AND   impt_aport_dev   = 0
-- AND   ident_pago       = g_dep.ident_pago
   AND   ident_pago[14,15] IN ("41","43","47","48","81","82","83")

   FOREACH curctar INTO rident_pago
      UPDATE dis_dep_issste
      SET impt_aport_dev = importe / 100,
          importe        = importe / 100
      WHERE folio      = vfolio
      AND   ident_pago = rident_pago
   END FOREACH


----------------------  IMPORTES VIV  ---------------------------
   UPDATE dis_dep_issste
      SET importe          = importe / 100,
          impt_aport_acept = acept_viv_92,
          impt_aport_dev   = recha_viv_92,
          apli_viv_env     = apli_viv_env / 1000000,      ----jerry
          apli_viv_acept   = acept_part_92,               ----jerry
          apli_viv_dev     = recha_part_92                ----jerry
    WHERE folio = vfolio
      AND ident_pago[14,15] = "42"  ----  aportaciones viv_92

   UPDATE dis_dep_issste
      SET importe          = importe / 100,
          impt_aport_acept = acept_viv_2008,
          impt_aport_dev   = recha_viv_2008,
          apli_viv_env     = apli_viv_env / 1000000,      ----jerry
          apli_viv_acept   = acept_part_2008,             ----jerry
          apli_viv_dev     = recha_part_2008              ----jerry
    WHERE folio = vfolio
      AND ident_pago[14,15] = "44"  ----  aportaciones viv_2008

   UPDATE dis_dep_issste
      SET importe          = importe / 100,
          impt_aport_acept = acept_viv_gar_92,
          impt_aport_dev   = recha_viv_gar_92,
          apli_viv_env     = apli_viv_env / 1000000,      ----jerry 
          apli_viv_acept   = acept_part_gar_92,           ----jerry
          apli_viv_dev     = recha_part_gar_92            ----jerry
    WHERE folio = vfolio
      AND ident_pago[14,15] = "45"  ----  aportaciones viv_92

   UPDATE dis_dep_issste
      SET importe          = importe / 100,
          impt_aport_acept = acept_viv_gar_2008,
          impt_aport_dev   = recha_viv_gar_2008,
          apli_viv_env     = apli_viv_env / 1000000,      ----jerry 
          apli_viv_acept   = acept_part_gar_2008,         ----jerry
          apli_viv_dev     = recha_part_gar_2008          ----jerry
    WHERE folio = vfolio
      AND ident_pago[14,15] = "46"  ----  aportaciones viv_2008

   CALL Genera_salidas(vfolio,
                       acept_sar_isss,
                       acept_ret_isss,
                       acept_cv_isss_pat,
                       acept_cv_isss_tra,
                       acept_ext_sar_isss,
                       acept_ext_ret_isss,
                       acept_ext_cv_isss_pat,
                       acept_ext_cv_isss_tra,
                       acept_viv_92,
                       acept_viv_2008,
                       acept_ext_viv_92,
                       acept_ext_viv_2008,
                       acept_cs_isss,
                       acept_vol,
                       acept_compl,
                       acept_lplazo,
                       acept_ahr_sol_tra,
                       acept_ahr_sol_dep,
                       acept_part_92,
                       acept_part_2008,
                       acept_viv_gar_92,
                       acept_viv_gar_2008,

                       recha_sar_isss,
                       recha_ret_isss,
                       recha_cv_isss_pat,
                       recha_cv_isss_tra,
                       recha_ext_sar_isss,
                       recha_ext_ret_isss,
                       recha_ext_cv_isss_pat,
                       recha_ext_cv_isss_tra,
                       recha_viv_92,
                       recha_viv_2008,
                       recha_ext_viv_92,
                       recha_ext_viv_2008,
                       recha_cs_isss,
                       recha_vol,
                       recha_compl,
                       recha_lplazo,
                       recha_ahr_sol_tra,
                       recha_ahr_sol_dep,
                       recha_part_92,
                       recha_part_2008,
                       recha_viv_gar_92,
                       recha_viv_gar_2008
                       )

   SELECT ident_arch
   INTO tipo_ar
   FROM dis_cza_issste
   WHERE folio = vfolio
  
   IF tipo_ar = 3 THEN
      LET vnom_archivo= new_cadena CLIPPED,".CONFINS" CLIPPED
   ELSE
      IF tipo_ar = 2 THEN
         LET vnom_archivo= new_cadena CLIPPED,".CONFINV" CLIPPED
      ELSE
         # Para tipo 1 y como valor default 
         LET vnom_archivo= new_cadena CLIPPED,".CONFIND" CLIPPED
      END IF
   END IF

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
            "; cat cza_iste rech_iste dep_iste sum_iste > ",vnom_archivo CLIPPED
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; rm cza_iste rech_iste dep_iste sum_iste "
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; chmod 777 ",vnom_archivo CLIPPED
   RUN ejecuta
END FUNCTION

 
FUNCTION Genera_salidas(vfolio,
                        acept_sar_isss,
                        acept_ret_isss,
                        acept_cv_isss_pat,
                        acept_cv_isss_tra,
                        acept_ext_sar_isss,
                        acept_ext_ret_isss,
                        acept_ext_cv_isss_pat,
                        acept_ext_cv_isss_tra,
                        acept_viv_92,
                        acept_viv_2008,
                        acept_ext_viv_92,
                        acept_ext_viv_2008,
                        acept_cs_isss,
                        acept_vol,
                        acept_compl,
                        acept_lplazo,
                        acept_ahr_sol_tra,
                        acept_ahr_sol_dep,
                        acept_part_92,
                        acept_part_2008,
                        acept_viv_gar_92,
                        acept_viv_gar_2008,

                        recha_sar_isss,
                        recha_ret_isss,
                        recha_cv_isss_pat,
                        recha_cv_isss_tra,
                        recha_ext_sar_isss,
                        recha_ext_ret_isss,
                        recha_ext_cv_isss_pat,
                        recha_ext_cv_isss_tra,
                        recha_viv_92,
                        recha_viv_2008,
                        recha_ext_viv_92,
                        recha_ext_viv_2008,
                        recha_cs_isss,
                        recha_vol,
                        recha_compl,
                        recha_lplazo,
                        recha_ahr_sol_tra,
                        recha_ahr_sol_dep,
                        recha_part_92,
                        recha_part_2008,
                        recha_viv_gar_92,
                        recha_viv_gar_2008
                        )

   DEFINE 
      vfolio                 INTEGER,
      acept_sar_isss         DECIMAL(16,2),
      acept_ret_isss         DECIMAL(16,2),
      acept_cv_isss_pat      DECIMAL(16,2),
      acept_cv_isss_tra      DECIMAL(16,2),
      acept_ext_sar_isss     DECIMAL(16,2),
      acept_ext_ret_isss     DECIMAL(16,2),
      acept_ext_cv_isss_pat  DECIMAL(16,2),
      acept_ext_cv_isss_tra  DECIMAL(16,2),
      acept_viv_92           DECIMAL(16,2),
      acept_viv_2008         DECIMAL(16,2),
      acept_ext_viv_92       DECIMAL(16,2),
      acept_ext_viv_2008     DECIMAL(16,2),
      acept_cs_isss          DECIMAL(16,2),
      acept_vol              DECIMAL(16,2),
      acept_compl            DECIMAL(16,2),
      acept_lplazo           DECIMAL(16,2),
      acept_ahr_sol_tra      DECIMAL(16,2),
      acept_ahr_sol_dep      DECIMAL(16,2),
      acept_part_92          DECIMAL(18,6),
      acept_part_2008        DECIMAL(18,6),
      acept_viv_gar_92       DECIMAL(16,2),
      acept_viv_gar_2008     DECIMAL(16,2),

      recha_sar_isss         DECIMAL(16,2),
      recha_ret_isss         DECIMAL(16,2),
      recha_cv_isss_pat      DECIMAL(16,2),
      recha_cv_isss_tra      DECIMAL(16,2),
      recha_ext_sar_isss     DECIMAL(16,2),
      recha_ext_ret_isss     DECIMAL(16,2),
      recha_ext_cv_isss_pat  DECIMAL(16,2),
      recha_ext_cv_isss_tra  DECIMAL(16,2),
      recha_viv_92           DECIMAL(16,2),
      recha_viv_2008         DECIMAL(16,2),
      recha_ext_viv_92       DECIMAL(16,2),
      recha_ext_viv_2008     DECIMAL(16,2),
      recha_cs_isss          DECIMAL(16,2),
      recha_vol              DECIMAL(16,2),
      recha_compl            DECIMAL(16,2),
      recha_lplazo           DECIMAL(16,2),
      recha_ahr_sol_tra      DECIMAL(16,2),
      recha_ahr_sol_dep      DECIMAL(16,2),
      recha_part_92          DECIMAL(18,6),
      recha_part_2008        DECIMAL(18,6),
      recha_viv_gar_92       DECIMAL(16,2),
      recha_viv_gar_2008     DECIMAL(16,2)
 
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
      motivo_rechazo3        CHAR(03),
      filler                 CHAR(271),
      ident_arch             CHAR(01)
   END RECORD

   DEFINE reg_rech RECORD
      tipo_registro          CHAR(2),
      ident_servicio         CHAR(2),
      consec_reg_lote        INTEGER,
      cve_icefa              CHAR(3),
      n_rfc_entidad          CHAR(12),
      nom_entidad            CHAR(40),
      ident_entidad          CHAR(7),
      cve_ramo               CHAR(5),
      cve_pagaduria          CHAR(5),
      periodo_pago           CHAR(6),
      fech_pago              CHAR(8),
      fech_valor_rviv        CHAR(8),
      n_seguro               CHAR(11),
      n_seguro_issste        CHAR(11),
      n_rfc                  CHAR(13),
      n_unico                CHAR(18),
      paterno                CHAR(40),
      materno                CHAR(40),
      nombres                CHAR(40),
      ind_trab_bono          CHAR(1),
      ident_tipo_aport       CHAR(2),
      sueldo_base_cot_rcv    DECIMAL(12,2),
      dias_cot_bimestre      SMALLINT,
      dias_incap_bimestre    SMALLINT,
      dias_ausen_bimestre    SMALLINT,
      impt_sar_isss          DECIMAL(12,2),
      impt_ret_isss          DECIMAL(12,2),
      impt_cv_isss_pat       DECIMAL(12,2),
      impt_cv_isss_tra       DECIMAL(12,2),
      inte_ext_sar_isss      DECIMAL(12,2),
      inte_ext_ret_isss      DECIMAL(12,2),
      inte_ext_cv_isss_pat   DECIMAL(12,2),
      inte_ext_cv_isss_tra   DECIMAL(12,2),
      impt_viv_92            DECIMAL(12,2),
      impt_viv_2008          DECIMAL(12,2),
      inte_ext_viv_92        DECIMAL(12,2),
      inte_ext_viv_2008      DECIMAL(12,2),
      ident_viv_garantia     CHAR(1),
      apli_int_viv_92        DECIMAL(18,6),
      apli_int_viv_2008      DECIMAL(18,6),
      apli_int_ext_viv_92    DECIMAL(18,6),
      apli_int_ext_viv_2008  DECIMAL(18,6),
      valor_aivs             DECIMAL(21,14),
      impt_cs_isss           DECIMAL(12,2),
      impt_aport_vol         DECIMAL(12,2),
      impt_aport_compl       DECIMAL(12,2),
      impt_aport_lplazo      DECIMAL(12,2),
      impt_ahr_sol_tra       DECIMAL(12,2),
      impt_ahr_sol_dep       DECIMAL(12,2),
      ident_aportacion       CHAR(1),
      filler                 CHAR(38),
      result_operacion       CHAR(2),
      det_mot_rechazo1       CHAR(3),
      det_mot_rechazo2       CHAR(3),
      det_mot_rechazo3       CHAR(3)
   END RECORD

   DEFINE reg_dep RECORD
      tipo_registro          CHAR(02),
      ident_servicio         CHAR(02),
      ident_pago             CHAR(16),
      importe                DECIMAL(15,2),
      fech_liquidacion       DATE,
      impt_aport_acept       DECIMAL(15,2),
      impt_aport_dev         DECIMAL(15,2),
      apli_viv_env           DECIMAL(22,6),  ----jerry
      apli_viv_acept         DECIMAL(22,6),  ----jerry
      apli_viv_dev           DECIMAL(22,6),  ----jerry
      ident_siefore          CHAR(08),       --c22-6
      tipo_siefore           SMALLINT        --c22-6 
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
      impt_tot_sar_isss      DECIMAL(17,2),
      impt_tot_ret_isss      DECIMAL(17,2),
      impt_tot_cv_isss_pat   DECIMAL(17,2),
      impt_tot_cv_isss_tra   DECIMAL(17,2),
      inte_ext_sar_isss      DECIMAL(17,2),
      inte_ext_ret_isss      DECIMAL(17,2),
      inte_ext_cv_isss_pat   DECIMAL(17,2),
      inte_ext_cv_isss_tra   DECIMAL(17,2),
      impt_tot_viv_92        DECIMAL(17,2),
      impt_tot_viv_2008      DECIMAL(17,2),
      inte_ext_viv_92        DECIMAL(17,2),
      inte_ext_viv_2008      DECIMAL(17,2),
      impt_tot_cs_isss       DECIMAL(17,2),
      impt_tot_apo_vol       DECIMAL(17,2),
      impt_tot_apo_comp      DECIMAL(17,2),
      impt_tot_apo_lplazo    DECIMAL(17,2),
      impt_apo_ahr_sol_tra   DECIMAL(17,2),
      impt_apo_ahr_sol_dep   DECIMAL(17,2),
      num_de_reg_aport       CHAR(9),
      num_cuentas_xpagar     CHAR(6),
      impt_total_xpagar      DECIMAL(17,2)
   END RECORD,

   vimpt_tot_sar_isss        DECIMAL(17,2),
   vimpt_tot_ret_isss        DECIMAL(17,2),
   vimpt_tot_cv_isss_pat     DECIMAL(17,2),
   vimpt_tot_cv_isss_tra     DECIMAL(17,2),
   vinte_tot_ext_sar_isss    DECIMAL(17,2),
   vinte_tot_ext_ret_isss    DECIMAL(17,2),
   vinte_tot_ext_cv_isss_pat DECIMAL(17,2),
   vinte_tot_ext_cv_isss_tra DECIMAL(17,2),
   vimpt_tot_viv_92          DECIMAL(17,2),
   vimpt_tot_viv_2008        DECIMAL(17,2),
   vinte_tot_ext_viv_92      DECIMAL(17,2),
   vinte_tot_ext_viv_2008    DECIMAL(17,2),
   vimpt_tot_cs_isss         DECIMAL(17,2),
   vimpt_tot_apo_vol         DECIMAL(17,2),
   vimpt_tot_apo_comp        DECIMAL(17,2),
   vimpt_tot_apo_lplazo      DECIMAL(17,2),
   vimpt_tot_ahr_sol_tra     DECIMAL(17,2),
   vimpt_tot_ahr_sol_dep     DECIMAL(17,2),
   c15_impt_total_xpagar     ,
   c15_impt_aport_acept      CHAR(15),
   c16_impt_total_xpagar     ,
   c16_impt_aport_acept      CHAR(16),
   vtotal_aport_rech         ,
   vtotal_int_rech           DECIMAL(15,2)

   DEFINE xident_siefore     CHAR(08),      --c22-6.6
          xtipo_siefore      SMALLINT,      --c22-6.6
          vimpt_acept        DECIMAL(15,2), --c22-6.6
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
          motivo_rechazo3,
          filler,
          ident_arch
   INTO  reg_cza.*
   FROM  dis_cza_issste
   WHERE folio = vfolio

   LET tipo_archivo              = reg_cza.ident_arch
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
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/cza_iste"
   START REPORT rep_cza TO vreporte
      OUTPUT TO REPORT rep_cza(reg_cza.*)
   FINISH REPORT rep_cza

   --------------- GENERA RESPUESTA RECHAZOS ---------------
   DECLARE cur_rech CURSOR FOR
   SELECT
      a.tipo_registro,
      a.ident_servicio,
      a.consec_reg_lote,
      a.cve_icefa,
      a.n_rfc_entidad,
      a.nom_entidad,
      a.ident_entidad,
      a.cve_ramo,
      a.cve_pagaduria,
      a.periodo_pago,
      a.fech_pago,
      a.fech_valor_rviv,
      a.n_seguro,
      a.n_seguro_issste,
      a.n_rfc,
      a.n_unico,
      a.paterno,
      a.materno,
      a.nombres,
      a.ind_trab_bono,
      a.ident_tipo_aport,
      a.sueldo_base_cot_rcv,
      a.dias_cot_bimestre,
      a.dias_incap_bimestre,
      a.dias_ausen_bimestre,
      a.impt_sar_isss,
      a.impt_ret_isss,
      a.impt_cv_isss_pat,
      a.impt_cv_isss_tra,
      a.inte_ext_sar_isss,
      a.inte_ext_ret_isss,
      a.inte_ext_cv_isss_pat,
      a.inte_ext_cv_isss_tra,
      a.impt_viv_92,
      a.impt_viv_2008,
      a.inte_ext_viv_92,
      a.inte_ext_viv_2008,
      a.ident_viv_garantia,
      a.apli_int_viv_92,
      a.apli_int_viv_2008,
      a.apli_int_ext_viv_92,
      a.apli_int_ext_viv_2008,
      a.valor_aivs,
      a.impt_cs_isss,
      a.impt_aport_vol,
      a.impt_aport_compl,
      a.impt_aport_lplazo,
      a.impt_ahr_sol_tra,
      a.impt_ahr_sol_dep,
      a.ident_aportacion,
      a.filler,
      a.result_operacion,
      a.det_mot_rechazo1,
      a.det_mot_rechazo2,
      a.det_mot_rechazo3
   FROM  dis_det_issste a
   WHERE a.folio = vfolio
   AND   a.result_operacion = "02"              ---- Rechazos
   ORDER BY a.consec_reg_lote, a.tipo_registro

   LET reg_rech.result_operacion  = "00"
   LET reg_rech.det_mot_rechazo1  = "000"
   LET reg_rech.det_mot_rechazo2  = "000"
   LET reg_rech.det_mot_rechazo3  = "000"
   
   DISPLAY "GENERANDO RECH..." #---AT 12,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/rech_iste"
   START REPORT rep_rech TO vreporte
      FOREACH cur_rech INTO reg_rech.*
         OUTPUT TO REPORT rep_rech(reg_rech.*)
      END FOREACH
   FINISH REPORT rep_rech

   --------------- GENERA RESPUESTA DEPOSITOS ---------------------

   DECLARE cur_41 CURSOR FOR                                     --c22-6.6
   SELECT  ident_siefore,                                        --c22-6.6
           tipo_siefore                                          --c22-6.6
   FROM    dis_dep_issste                                        --c22-6.6
   WHERE   folio = vfolio                                        --c22-6.6
   AND     ident_pago[14,15] = "41"                              --c22-6.6

   FOREACH cur_41 INTO xident_siefore,xtipo_siefore              --c22-6.6

      SELECT impt_aport_acept,                                   --c22-6.6
             tipo_siefore                                        --c22-6.6
      INTO   vimpt_acept,                                        --c22-6.6
             vtipo_sie                                           --c22-6.6
      FROM   dis_dep_issste                                      --c22-6.6
      WHERE  folio = vfolio                                      --c22-6.6
      AND    ident_pago[14,15] = "11"                            --c22-6.6
      AND    tipo_siefore = 0                                    --c22-6.6

      IF STATUS <> NOTFOUND THEN                                 --c22-6.6
         IF vimpt_acept = 0 AND vtipo_sie = 0 THEN               --c22-6.6
            UPDATE dis_dep_issste                                --c22-6.6
            SET    tipo_siefore = xtipo_siefore,                 --c22-6.6
                   ident_siefore = xident_siefore                --c22-6.6
            WHERE  folio             = vfolio                    --c22-6.6
            AND    ident_pago[14,15] = "11"                      --c22-6.6
            AND    tipo_siefore      = 0                         --c22-6.6
         END IF                                                  --c22-6.6
      END IF                                                     --c22-6.6
   END FOREACH                                                   --c22-6.6

   DECLARE cur_42 CURSOR FOR                                     --c22-6.6
   SELECT  ident_siefore,                                        --c22-6.6
           tipo_siefore                                          --c22-6.6
   FROM    dis_dep_issste                                        --c22-6.6
   WHERE   folio = vfolio                                        --c22-6.6
   AND     ident_pago[14,15] = "42"                              --c22-6.6

   FOREACH cur_42 INTO xident_siefore,xtipo_siefore              --c22-6.6

      SELECT impt_aport_acept,                                   --c22-6.6
             tipo_siefore                                        --c22-6.6
      INTO   vimpt_acept,                                        --c22-6.6
             vtipo_sie                                           --c22-6.6
      FROM   dis_dep_issste                                      --c22-6.6
      WHERE  folio = vfolio                                      --c22-6.6
      AND    ident_pago[14,15] = "12"                            --c22-6.6
      AND    tipo_siefore = 0                                    --c22-6.6

      IF STATUS <> NOTFOUND THEN                                 --c22-6.6
         IF vimpt_acept = 0 AND vtipo_sie = 0 THEN               --c22-6.6
            UPDATE dis_dep_issste                                --c22-6.6
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
      fech_liquidacion,
      impt_aport_acept,
      impt_aport_dev,
      apli_viv_env,
      apli_viv_acept,
      apli_viv_dev,
      ident_siefore,
      tipo_siefore
   FROM dis_dep_issste
   WHERE folio = vfolio

   DISPLAY "GENERANDO DEP....." #------AT 14,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/dep_iste"
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
          impt_tot_sar_isss,
          impt_tot_ret_isss,
          impt_tot_cv_isss_pat,
          impt_tot_cv_isss_tra,
          inte_ext_sar_isss,
          inte_ext_ret_isss,
          inte_ext_cv_isss_pat,
          inte_ext_cv_isss_tra,
          impt_tot_viv_92,
          impt_tot_viv_2008,
          inte_ext_viv_92,
          inte_ext_viv_2008,
          impt_tot_cs_isss,
          impt_tot_apo_vol,
          impt_tot_apo_comp,
          impt_tot_apo_lplazo,
          impt_apo_ahr_sol_tra,
          impt_apo_ahr_sol_dep,
          num_de_reg_aport,
          num_cuentas_xpagar,
          impt_total_xpagar
   FROM   dis_sum_issste
   WHERE  folio = vfolio
   FOR UPDATE

   DISPLAY "GENERANDO SUM..." #----AT 16,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/sum_iste"
   START REPORT rep_sum TO vreporte
      FOREACH cur_sum INTO reg_sum.*

         LET reg_sum.tipo_ent_origen        = "01"
         LET reg_sum.clave_ent_origen       = vcod_afore
         LET reg_sum.tipo_ent_destino       = "03"
         LET reg_sum.clave_ent_destino      = "001"
         LET reg_sum.fech_creac_lote        = reg_cza.fech_creac_lote
         LET reg_sum.lote_del_dia           = reg_cza.lote_del_dia
         LET reg_sum.num_de_reg_aport       = "000000000"

         SELECT sum(impt_viv_92)/100
         INTO  recha_viv_92
         FROM  dis_det_issste
         WHERE folio = vfolio
         AND   result_operacion = 02

         IF recha_viv_92 IS NULL OR recha_viv_92 = "" THEN
            LET recha_viv_92 = 0
         END IF

         SELECT sum(impt_viv_2008)/100
         INTO  recha_viv_2008
         FROM  dis_det_issste
         WHERE folio = vfolio
         AND   result_operacion = 02

         IF recha_viv_2008 IS NULL OR recha_viv_2008 = "" THEN
            LET recha_viv_2008 = 0
         END IF

         UPDATE dis_sum_issste
         SET    impt_tot_sar_isss    = recha_sar_isss     + acept_sar_isss,
                impt_tot_ret_isss    = recha_ret_isss     + acept_ret_isss,
                impt_tot_cv_isss_pat = recha_cv_isss_pat  + acept_cv_isss_pat,
                impt_tot_cv_isss_tra = recha_cv_isss_tra  + acept_cv_isss_tra,
                inte_ext_ret_isss    = recha_ext_ret_isss + acept_ext_ret_isss,
                inte_ext_sar_isss    = recha_ext_sar_isss + acept_ext_sar_isss,
                inte_ext_cv_isss_pat = recha_ext_cv_isss_pat + acept_ext_cv_isss_pat,
                inte_ext_cv_isss_tra = recha_ext_cv_isss_tra + acept_ext_cv_isss_tra,
                impt_tot_viv_92      = recha_viv_92       + acept_viv_92,
                impt_tot_viv_2008    = recha_viv_2008     + acept_viv_2008,
                inte_ext_viv_92      = recha_ext_viv_92   + acept_ext_viv_92,
                inte_ext_viv_2008    = recha_ext_viv_2008 + acept_ext_viv_2008,
                impt_tot_cs_isss     = recha_cs_isss      + acept_cs_isss,
                impt_tot_apo_vol     = recha_vol          + acept_vol,
                impt_tot_apo_comp    = recha_compl        + acept_compl,
                impt_tot_apo_lplazo  = recha_lplazo       + acept_lplazo,
                impt_apo_ahr_sol_tra = recha_ahr_sol_tra  + acept_ahr_sol_tra,
                impt_apo_ahr_sol_dep = recha_ahr_sol_dep  + acept_ahr_sol_dep
         WHERE  CURRENT OF cur_sum

         OUTPUT TO REPORT rep_sum(reg_sum.*,
                                  acept_sar_isss,
                                  acept_ret_isss,
                                  acept_cv_isss_pat,
                                  acept_cv_isss_tra,
                                  acept_ext_sar_isss,
                                  acept_ext_ret_isss,
                                  acept_ext_cv_isss_pat,
                                  acept_ext_cv_isss_tra,
                                  acept_viv_92,
                                  acept_viv_2008,
                                  acept_ext_viv_92,
                                  acept_ext_viv_2008,
                                  acept_cs_isss,
                                  acept_vol,
                                  acept_compl,
                                  acept_lplazo,
                                  acept_ahr_sol_tra,
                                  acept_ahr_sol_dep,
                                  acept_viv_gar_92,
                                  acept_viv_gar_2008,
                                  recha_sar_isss,
                                  recha_ret_isss,
                                  recha_cv_isss_pat,
                                  recha_cv_isss_tra,
                                  recha_ext_sar_isss,
                                  recha_ext_ret_isss,
                                  recha_ext_cv_isss_pat,
                                  recha_ext_cv_isss_tra,
                                  recha_viv_92,
                                  recha_viv_2008,
                                  recha_ext_viv_92,
                                  recha_ext_viv_2008,
                                  recha_cs_isss,
                                  recha_vol,
                                  recha_compl,
                                  recha_lplazo,
                                  recha_ahr_sol_tra,
                                  recha_ahr_sol_dep,
                                  recha_viv_gar_92,
                                  recha_viv_gar_2008
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
      motivo_rechazo3   CHAR(03),
      filler            CHAR(271),
      ident_arch        CHAR(01)
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
                        reg_cza.filler,
                        reg_cza.ident_arch,
                        300 SPACES
END REPORT


REPORT rep_rech(reg_rech)
DEFINE reg_rech RECORD
      tipo_registro         CHAR(2),
      ident_servicio        CHAR(2),
      consec_reg_lote       INTEGER,
      cve_icefa             CHAR(3),
      n_rfc_entidad         CHAR(12),
      nom_entidad           CHAR(40),
      ident_entidad         CHAR(7),
      cve_ramo              CHAR(5),
      cve_pagaduria         CHAR(5),
      periodo_pago          CHAR(6),
      fech_pago             CHAR(8),
      fech_valor_rviv       CHAR(8),
      n_seguro              CHAR(11),
      n_seguro_issste       CHAR(11),
      n_rfc                 CHAR(13),
      n_unico               CHAR(18),
      paterno               CHAR(40),
      materno               CHAR(40),
      nombres               CHAR(40),
      ind_trab_bono         CHAR(1),
      ident_tipo_aport      CHAR(2),
      sueldo_base_cot_rcv   DECIMAL(12,2),
      dias_cot_bimestre     SMALLINT,
      dias_incap_bimestre   SMALLINT,
      dias_ausen_bimestre   SMALLINT,
      impt_sar_isss         DECIMAL(12,2),
      impt_ret_isss         DECIMAL(12,2),
      impt_cv_isss_pat      DECIMAL(12,2),
      impt_cv_isss_tra      DECIMAL(12,2),
      inte_ext_sar_isss     DECIMAL(12,2),
      inte_ext_ret_isss     DECIMAL(12,2),
      inte_ext_cv_isss_pat  DECIMAL(12,2),
      inte_ext_cv_isss_tra  DECIMAL(12,2),
      impt_viv_92           DECIMAL(12,2),
      impt_viv_2008         DECIMAL(12,2),
      inte_ext_viv_92       DECIMAL(12,2),
      inte_ext_viv_2008     DECIMAL(12,2),
      ident_viv_garantia    CHAR(1),
      apli_int_viv_92       DECIMAL(18,6),
      apli_int_viv_2008     DECIMAL(18,6),
      apli_int_ext_viv_92   DECIMAL(18,6),
      apli_int_ext_viv_2008 DECIMAL(18,6),
      valor_aivs            DECIMAL(21,14),
      impt_cs_isss          DECIMAL(12,2),
      impt_aport_vol        DECIMAL(12,2),
      impt_aport_compl      DECIMAL(12,2),
      impt_aport_lplazo     DECIMAL(12,2),
      impt_ahr_sol_tra      DECIMAL(12,2),
      impt_ahr_sol_dep      DECIMAL(12,2),
      ident_aportacion      CHAR(1),
      filler                CHAR(38),
      result_operacion      CHAR(2),
      det_mot_rechazo1      CHAR(3),
      det_mot_rechazo2      CHAR(3),
      det_mot_rechazo3      CHAR(3)
   END RECORD,

   cconsec_reg_lote        CHAR(08),
   csdo_base_cot_rcv       CHAR(08),
   cimpt_sar_isss          CHAR(13),
   cimpt_ret_isss          CHAR(13),
   cimpt_cv_isss_pat       CHAR(13),
   cimpt_cv_isss_tra       CHAR(13),
   cinte_ext_sar_isss      CHAR(13),
   cinte_ext_ret_isss      CHAR(13),
   cinte_ext_cv_isss_pat   CHAR(13),
   cinte_ext_cv_isss_tra   CHAR(13),
   cimpt_viv_92            CHAR(13),
   cimpt_viv_2008          CHAR(13),
   cinte_ext_viv_92        CHAR(13),
   cinte_ext_viv_2008      CHAR(13),
   cimpt_aport_vol         CHAR(08),
   cimpt_aport_compl       CHAR(08),
   cimpt_aport_lplazo      CHAR(08),
   cimpt_cs_isss           CHAR(13),
   cimpt_ahr_sol_tra       CHAR(08),
   cimpt_ahr_sol_dep       CHAR(08),
   cvalor_aivs             CHAR(21),
   
   xconsec_reg_lote        CHAR(08),
   xsdo_base_cot_rcv       CHAR(07),
   xdias_cotz_bimest       CHAR(02),
   xdias_incap_bimest      CHAR(02),
   xdias_ausent_bimest     CHAR(02),

   ximpt_sar_isss          CHAR(12),
   ximpt_ret_isss          CHAR(12),
   ximpt_cv_isss_pat       CHAR(12),
   ximpt_cv_isss_tra       CHAR(12),
   xinte_ext_sar_isss      CHAR(12),
   xinte_ext_ret_isss      CHAR(12),
   xinte_ext_cv_isss_pat   CHAR(12),
   xinte_ext_cv_isss_tra   CHAR(12),
   ximpt_viv_92            CHAR(12),
   ximpt_viv_2008          CHAR(12),
   xinte_ext_viv_92        CHAR(12),
   xinte_ext_viv_2008      CHAR(12),
   ximpt_aport_vol         CHAR(07),
   ximpt_aport_compl       CHAR(07),
   ximpt_aport_lplazo      CHAR(07),
   ximpt_cs_isss           CHAR(12),
   ximpt_ahr_sol_tra       CHAR(07),
   ximpt_ahr_sol_dep       CHAR(07),
   xvalor_aivs             CHAR(20),

   xdias_cot_bimestre      CHAR(03),
   xdias_incap_bimestre    CHAR(03),
   xdias_ausen_bimestre    CHAR(03) 

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW

   LET vnum_reg = vnum_reg + 1
 
LET csdo_base_cot_rcv  = reg_rech.sueldo_base_cot_rcv / 100 USING '&&&&&.&&'

LET cimpt_sar_isss     = reg_rech.impt_sar_isss     / 100 USING '&&&&&&&&&&.&&'
LET cimpt_ret_isss     = reg_rech.impt_ret_isss     / 100 USING '&&&&&&&&&&.&&'
LET cimpt_cv_isss_pat  = reg_rech.impt_cv_isss_pat  / 100 USING '&&&&&&&&&&.&&'
LET cimpt_cv_isss_tra  = reg_rech.impt_cv_isss_tra  / 100 USING '&&&&&&&&&&.&&'
LET cinte_ext_sar_isss = reg_rech.inte_ext_sar_isss / 100 USING '&&&&&&&&&&.&&'
LET cinte_ext_ret_isss = reg_rech.inte_ext_ret_isss / 100 USING '&&&&&&&&&&.&&'
LET cinte_ext_cv_isss_pat = reg_rech.inte_ext_cv_isss_pat / 100 USING '&&&&&&&&&&.&&'
LET cinte_ext_cv_isss_tra = reg_rech.inte_ext_cv_isss_tra / 100 USING '&&&&&&&&&&.&&'
LET cimpt_viv_92       = reg_rech.impt_viv_92       / 100 USING '&&&&&&&&&&.&&'
LET cimpt_viv_2008     = reg_rech.impt_viv_2008     / 100 USING '&&&&&&&&&&.&&'
LET cinte_ext_viv_92   = reg_rech.inte_ext_viv_92   / 100 USING '&&&&&&&&&&.&&'
LET cinte_ext_viv_2008 = reg_rech.inte_ext_viv_2008 / 100 USING '&&&&&&&&&&.&&'
LET cimpt_aport_vol    = reg_rech.impt_aport_vol    / 100 USING '&&&&&.&&'
LET cimpt_aport_compl  = reg_rech.impt_aport_compl  / 100 USING '&&&&&.&&'
LET cimpt_aport_lplazo = reg_rech.impt_aport_lplazo / 100 USING '&&&&&.&&'
LET cimpt_cs_isss      = reg_rech.impt_cs_isss      / 100 USING '&&&&&&&&&&.&&'
LET cimpt_ahr_sol_tra  = reg_rech.impt_ahr_sol_tra  / 100 USING '&&&&&.&&'
LET cimpt_ahr_sol_dep  = reg_rech.impt_ahr_sol_dep  / 100 USING '&&&&&.&&'

LET cvalor_aivs        = reg_rech.valor_aivs USING "&&&&&&.&&&&&&&&&&&&&&"
LET xvalor_aivs        = cvalor_aivs[1,06], cvalor_aivs[8,21]

LET xconsec_reg_lote   = reg_rech.consec_reg_lote  USING '&&&&&&&&'
LET xsdo_base_cot_rcv  = csdo_base_cot_rcv[1,05] , csdo_base_cot_rcv[7,08]

LET ximpt_sar_isss     = cimpt_sar_isss     [1,10],cimpt_sar_isss     [12,13]
LET ximpt_ret_isss     = cimpt_ret_isss     [1,10],cimpt_ret_isss     [12,13]
LET ximpt_cv_isss_pat  = cimpt_cv_isss_pat  [1,10],cimpt_cv_isss_pat  [12,13]
LET ximpt_cv_isss_tra  = cimpt_cv_isss_tra  [1,10],cimpt_cv_isss_tra  [12,13]
LET xinte_ext_sar_isss = cinte_ext_sar_isss [1,10],cinte_ext_sar_isss [12,13]
LET xinte_ext_ret_isss = cinte_ext_ret_isss [1,10],cinte_ext_ret_isss [12,13]
LET xinte_ext_cv_isss_pat = cinte_ext_cv_isss_pat [1,10],cinte_ext_cv_isss_pat [12,13]
LET xinte_ext_cv_isss_tra = cinte_ext_cv_isss_tra [1,10],cinte_ext_cv_isss_tra [12,13]

LET ximpt_viv_92       = cimpt_viv_92       [1,10],cimpt_viv_92       [12,13]
LET ximpt_viv_2008     = cimpt_viv_2008     [1,10],cimpt_viv_2008     [12,13]
LET xinte_ext_viv_92   = cinte_ext_viv_92   [1,10],cinte_ext_viv_92   [12,13]
LET xinte_ext_viv_2008 = cinte_ext_viv_2008 [1,10],cinte_ext_viv_2008 [12,13]

LET ximpt_aport_vol    = cimpt_aport_vol    [1,05],cimpt_aport_vol    [07,08]
LET ximpt_aport_compl  = cimpt_aport_compl  [1,05],cimpt_aport_compl  [07,08]
LET ximpt_aport_lplazo = cimpt_aport_lplazo [1,05],cimpt_aport_lplazo [07,08]

LET ximpt_cs_isss      = cimpt_cs_isss      [1,10],cimpt_cs_isss      [12,13]

LET ximpt_ahr_sol_tra  = cimpt_ahr_sol_tra  [1,05],cimpt_ahr_sol_tra  [07,08]
LET ximpt_ahr_sol_dep  = cimpt_ahr_sol_dep  [1,05],cimpt_ahr_sol_dep  [07,08]

LET xdias_cot_bimestre   = reg_rech.dias_cot_bimestre USING '&&&'
LET xdias_incap_bimestre = reg_rech.dias_incap_bimestre USING '&&&'
LET xdias_ausen_bimestre = reg_rech.dias_ausen_bimestre USING '&&&'

         PRINT
            COLUMN 1,reg_rech.tipo_registro,
                     reg_rech.ident_servicio,
                     xconsec_reg_lote,
                     reg_rech.cve_icefa,
                     reg_rech.n_rfc_entidad,
                     reg_rech.nom_entidad,
                     reg_rech.ident_entidad,
                     reg_rech.cve_ramo,
                     reg_rech.cve_pagaduria,
                     reg_rech.periodo_pago,
                     reg_rech.fech_pago,
                     reg_rech.fech_valor_rviv,
                     reg_rech.n_seguro,
                     reg_rech.n_seguro_issste,
                     reg_rech.n_rfc,
                     reg_rech.n_unico,
                     reg_rech.paterno,
                     reg_rech.materno,
                     reg_rech.nombres,
                     reg_rech.ind_trab_bono,
                     reg_rech.ident_tipo_aport,
                     xsdo_base_cot_rcv,
                     xdias_cot_bimestre,
                     xdias_incap_bimestre,
                     xdias_ausen_bimestre,
                     ximpt_sar_isss,
                     ximpt_ret_isss,
                     ximpt_cv_isss_pat,
                     ximpt_cv_isss_tra,
                     xinte_ext_sar_isss,
                     xinte_ext_ret_isss,
                     xinte_ext_cv_isss_pat,
                     xinte_ext_cv_isss_tra,
                     ximpt_viv_92,
                     ximpt_viv_2008,
                     xinte_ext_viv_92,
                     xinte_ext_viv_2008,
                     reg_rech.ident_viv_garantia,
                     reg_rech.apli_int_viv_92   USING '&&&&&&&&&&&&&&&',
                     reg_rech.apli_int_viv_2008 USING '&&&&&&&&&&&&&&&',
                     reg_rech.apli_int_ext_viv_92   USING '&&&&&&&&&&&&&&&',
                     reg_rech.apli_int_ext_viv_2008 USING '&&&&&&&&&&&&&&&',
                     xvalor_aivs,
                     ximpt_cs_isss,
                     ximpt_aport_vol,
                     ximpt_aport_compl,
                     ximpt_aport_lplazo,
                     ximpt_ahr_sol_tra,
                     ximpt_ahr_sol_dep,
                     reg_rech.ident_aportacion,
                     reg_rech.filler,
                     "00",                      --- result_operacion,
                     reg_rech.det_mot_rechazo1,
                     reg_rech.det_mot_rechazo2,
                     reg_rech.det_mot_rechazo3
END REPORT


REPORT rep_dep(reg_dep)
   DEFINE reg_dep RECORD
      tipo_registro     CHAR(02),
      ident_servicio    CHAR(02),
      ident_pago        CHAR(16),
      importe           DECIMAL(15,2),
      fech_liquidacion  DATE,
      impt_aport_acept  DECIMAL(15,2),
      impt_aport_dev    DECIMAL(15,2),
      apli_viv_env      DECIMAL(22,6),
      apli_viv_acept    DECIMAL(22,6),
      apli_viv_dev      DECIMAL(22,6),
      ident_siefore     CHAR(08),
      tipo_siefore      SMALLINT
   END RECORD,

   c16_importe          CHAR(16),
   c15_importe          CHAR(15),
   c16_impt_aport_acept CHAR(16),
   c15_impt_aport_acept CHAR(15),
   c16_impt_aport_dev   CHAR(16),
   c15_impt_aport_dev   CHAR(15),
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

         IF reg_dep.importe IS NULL THEN
            LET c15_importe = "000000000000000"
         ELSE
            LET c16_importe = reg_dep.importe USING"&&&&&&&&&&&&&.&&"
            LET c15_importe = c16_importe[01,13],c16_importe[15,16]
         END IF

         IF reg_dep.impt_aport_acept IS NULL THEN
            LET c15_impt_aport_acept = "000000000000000"
         ELSE
            LET c16_impt_aport_acept = reg_dep.impt_aport_acept
                                       USING"&&&&&&&&&&&&&.&&"
            LET c15_impt_aport_acept = c16_impt_aport_acept[01,13],
                                       c16_impt_aport_acept[15,16]
         END IF

         IF reg_dep.impt_aport_dev IS NULL THEN
            LET c15_impt_aport_dev = "000000000000000"
         ELSE
            LET c16_impt_aport_dev = reg_dep.impt_aport_dev
                                     USING"&&&&&&&&&&&&&.&&"
            LET c15_impt_aport_dev = c16_impt_aport_dev[01,13],
                                     c16_impt_aport_dev[15,16]
         END IF

         LET xfecha_liquidacion = reg_dep.fech_liquidacion
         LET vfecha_liquidacion = xfecha_liquidacion[7,10],
                                  xfecha_liquidacion[1,02],
                                  xfecha_liquidacion[4,05]

         LET reg_dep.apli_viv_env   = reg_dep.apli_viv_env   * 1000000
         LET reg_dep.apli_viv_acept = reg_dep.apli_viv_acept * 1000000
         LET reg_dep.apli_viv_dev   = reg_dep.apli_viv_dev   * 1000000

         PRINT COLUMN 01,reg_dep.tipo_registro,
                         reg_dep.ident_servicio,
                         reg_dep.ident_pago,
                         c15_importe,
                         vfecha_liquidacion,
                         c15_impt_aport_acept,
                         c15_impt_aport_dev,
                         reg_dep.apli_viv_env   USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.apli_viv_acept USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.apli_viv_dev   USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.ident_siefore,
                         reg_dep.tipo_siefore    USING '&&&',
                         482 space
END REPORT


REPORT rep_sum(reg_sum,
               acept_sar_isss,
               acept_ret_isss,
               acept_cv_isss_pat,
               acept_cv_isss_tra,
               acept_ext_sar_isss,
               acept_ext_ret_isss,
               acept_ext_cv_isss_pat,
               acept_ext_cv_isss_tra,
               acept_viv_92,
               acept_viv_2008,
               acept_ext_viv_92,
               acept_ext_viv_2008,
               acept_cs_isss,
               acept_vol,
               acept_compl,
               acept_lplazo,
               acept_ahr_sol_tra,
               acept_ahr_sol_dep,
               acept_viv_gar_92,
               acept_viv_gar_2008,
               recha_sar_isss,
               recha_ret_isss,
               recha_cv_isss_pat,
               recha_cv_isss_tra,
               recha_ext_sar_isss,
               recha_ext_ret_isss,
               recha_ext_cv_isss_pat,
               recha_ext_cv_isss_tra,
               recha_viv_92,
               recha_viv_2008,
               recha_ext_viv_92,
               recha_ext_viv_2008,
               recha_cs_isss,
               recha_vol,
               recha_compl,
               recha_lplazo,
               recha_ahr_sol_tra,
               recha_ahr_sol_dep,
               recha_viv_gar_92,
               recha_viv_gar_2008
               )

   DEFINE
      acept_sar_isss         DECIMAL(16,2),
      acept_ret_isss         DECIMAL(16,2),
      acept_cv_isss_pat      DECIMAL(16,2),
      acept_cv_isss_tra      DECIMAL(16,2),
      acept_ext_sar_isss     DECIMAL(16,2),
      acept_ext_ret_isss     DECIMAL(16,2),
      acept_ext_cv_isss_pat  DECIMAL(16,2),
      acept_ext_cv_isss_tra  DECIMAL(16,2),
      acept_viv_92           DECIMAL(16,2),
      acept_viv_2008         DECIMAL(16,2),
      acept_ext_viv_92       DECIMAL(16,2),
      acept_ext_viv_2008     DECIMAL(16,2),
      acept_cs_isss          DECIMAL(16,2),
      acept_vol              DECIMAL(16,2),
      acept_compl            DECIMAL(16,2),
      acept_lplazo           DECIMAL(16,2),
      acept_ahr_sol_tra      DECIMAL(16,2),
      acept_ahr_sol_dep      DECIMAL(16,2),
      acept_viv_gar_92       DECIMAL(16,2),
      acept_viv_gar_2008     DECIMAL(16,2),
      recha_sar_isss         DECIMAL(16,2),
      recha_ret_isss         DECIMAL(16,2),
      recha_cv_isss_pat      DECIMAL(16,2),
      recha_cv_isss_tra      DECIMAL(16,2),
      recha_ext_sar_isss     DECIMAL(16,2),
      recha_ext_ret_isss     DECIMAL(16,2),
      recha_ext_cv_isss_pat  DECIMAL(16,2),
      recha_ext_cv_isss_tra  DECIMAL(16,2),
      recha_viv_92           DECIMAL(16,2),
      recha_viv_2008         DECIMAL(16,2),
      recha_ext_viv_92       DECIMAL(16,2),
      recha_ext_viv_2008     DECIMAL(16,2),
      recha_cs_isss          DECIMAL(16,2),
      recha_vol              DECIMAL(16,2),
      recha_compl            DECIMAL(16,2),
      recha_lplazo           DECIMAL(16,2),
      recha_ahr_sol_tra      DECIMAL(16,2),
      recha_ahr_sol_dep      DECIMAL(16,2),
      recha_viv_gar_92       DECIMAL(16,2),
      recha_viv_gar_2008     DECIMAL(16,2)

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
      impt_tot_sar_isss      DECIMAL(17,2),
      impt_tot_ret_isss      DECIMAL(17,2),
      impt_tot_cv_isss_pat   DECIMAL(17,2),
      impt_tot_cv_isss_tra   DECIMAL(17,2),
      inte_ext_sar_isss      DECIMAL(17,2),
      inte_ext_ret_isss      DECIMAL(17,2),
      inte_ext_cv_isss_pat   DECIMAL(17,2),
      inte_ext_cv_isss_tra   DECIMAL(17,2),
      impt_tot_viv_92        DECIMAL(17,2),
      impt_tot_viv_2008      DECIMAL(17,2),
      inte_ext_viv_92        DECIMAL(17,2),
      inte_ext_viv_2008      DECIMAL(17,2),
      impt_tot_cs_isss       DECIMAL(17,2),
      impt_tot_apo_vol       DECIMAL(17,2),
      impt_tot_apo_comp      DECIMAL(17,2),
      impt_tot_apo_lplazo    DECIMAL(17,2),
      impt_apo_ahr_sol_tra   DECIMAL(17,2),
      impt_apo_ahr_sol_dep   DECIMAL(17,2),
      num_de_reg_aport       CHAR(9),
      num_cuentas_xpagar     CHAR(6),
      impt_total_xpagar      DECIMAL(17,2)
   END RECORD,

   c15_impt_tot_sar_isss     ,
   c15_impt_tot_ret_isss     ,
   c15_impt_tot_cv_isss_pat  ,
   c15_impt_tot_cv_isss_tra  ,
   c15_inte_ext_sar_isss     ,
   c15_inte_ext_ret_isss     ,
   c15_inte_ext_cv_isss_pat  ,
   c15_inte_ext_cv_isss_tra  ,
   c15_impt_tot_viv_92       ,
   c15_impt_tot_viv_2008     ,
   c15_inte_ext_viv_92       ,
   c15_inte_ext_viv_2008     ,
   c15_impt_tot_cs_isss      ,
   c15_impt_tot_apo_vol      ,
   c15_impt_tot_apo_comp     ,
   c15_impt_tot_apo_lplazo   ,
   c15_impt_apo_ahr_sol_tra  ,
   c15_impt_apo_ahr_sol_dep  CHAR(15),

   c16_impt_tot_sar_isss     ,
   c16_impt_tot_ret_isss     ,
   c16_impt_tot_cv_isss_pat  ,
   c16_impt_tot_cv_isss_tra  ,
   c16_inte_ext_sar_isss     ,
   c16_inte_ext_ret_isss     ,
   c16_inte_ext_cv_isss_pat  ,
   c16_inte_ext_cv_isss_tra  ,
   c16_impt_tot_viv_92       ,
   c16_impt_tot_viv_2008     ,
   c16_inte_ext_viv_92       ,
   c16_inte_ext_viv_2008     ,
   c16_impt_tot_cs_isss      ,
   c16_impt_tot_apo_vol      ,
   c16_impt_tot_apo_comp     ,
   c16_impt_tot_apo_lplazo   ,
   c16_impt_apo_ahr_sol_tra  ,
   c16_impt_apo_ahr_sol_dep  CHAR(16),

   c15_impt_total_xpagar     ,
   c15_impt_aport_acept      CHAR(15),
   c16_impt_total_xpagar     ,
   c16_impt_aport_acept      CHAR(16),
   vtotal_aport_rech         ,
   vtotal_int_rech           DECIMAL(15,2),

   d15_impt_tot_sar_isss     ,
   d15_impt_tot_ret_isss     ,
   d15_impt_tot_cv_isss_pat  ,
   d15_impt_tot_cv_isss_tra  ,
   d15_inte_ext_sar_isss     ,
   d15_inte_ext_ret_isss     ,
   d15_inte_ext_cv_isss_pat  ,
   d15_inte_ext_cv_isss_tra  ,
   d15_impt_tot_viv_92       ,
   d15_impt_tot_viv_2008     ,
   d15_inte_ext_viv_92       ,
   d15_inte_ext_viv_2008     ,
   d15_impt_tot_cs_isss      ,
   d15_impt_tot_apo_vol      ,
   d15_impt_tot_apo_comp     ,
   d15_impt_tot_apo_lplazo   ,
   d15_impt_apo_ahr_sol_tra  ,
   d15_impt_apo_ahr_sol_dep  DECIMAL(15,2),

   d15_impt_total_xpagar     ,
   d15_impt_aport_acept      ,
   d15_impt_aport_dev        ,
   d13_impt_aport_acept      DECIMAL(15,2)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW

         IF reg_sum.impt_tot_viv_92 IS NULL THEN
            LET reg_sum.impt_tot_viv_92 = 0
         END IF
         IF reg_sum.inte_ext_viv_92 IS NULL THEN
            LET reg_sum.inte_ext_viv_92 = 0
         END IF

         IF reg_sum.impt_tot_viv_2008 IS NULL THEN
            LET reg_sum.impt_tot_viv_2008 = 0
         END IF
         IF reg_sum.inte_ext_viv_2008 IS NULL THEN
            LET reg_sum.inte_ext_viv_2008 = 0
         END IF

         LET reg_sum.impt_tot_sar_isss    = recha_sar_isss        * 100
         LET reg_sum.impt_tot_ret_isss    = recha_ret_isss        * 100
         LET reg_sum.impt_tot_cv_isss_pat = recha_cv_isss_pat     * 100
         LET reg_sum.impt_tot_cv_isss_tra = recha_cv_isss_tra     * 100
         LET reg_sum.inte_ext_sar_isss    = recha_ext_sar_isss    * 100
         LET reg_sum.inte_ext_ret_isss    = recha_ext_ret_isss    * 100
         LET reg_sum.inte_ext_cv_isss_pat = recha_ext_cv_isss_pat * 100
         LET reg_sum.inte_ext_cv_isss_tra = recha_ext_cv_isss_tra * 100
         LET reg_sum.impt_tot_viv_92      = recha_viv_92          * 100
         LET reg_sum.impt_tot_viv_2008    = recha_viv_2008        * 100
         LET reg_sum.inte_ext_viv_92      = recha_ext_viv_92      * 100
         LET reg_sum.inte_ext_viv_2008    = recha_ext_viv_2008    * 100
         LET reg_sum.impt_tot_cs_isss     = recha_cs_isss         * 100
         LET reg_sum.impt_tot_apo_vol     = recha_vol             * 100
         LET reg_sum.impt_tot_apo_comp    = recha_compl           * 100
         LET reg_sum.impt_tot_apo_lplazo  = recha_lplazo          * 100
         LET reg_sum.impt_apo_ahr_sol_tra = recha_ahr_sol_tra     * 100 
         LET reg_sum.impt_apo_ahr_sol_dep = recha_ahr_sol_dep     * 100 
 
         LET d15_impt_total_xpagar = acept_sar_isss     + acept_ret_isss     +
                                     acept_cv_isss_pat  + acept_cv_isss_tra  +
                                     acept_viv_92       + acept_viv_2008     +
                                     acept_vol          + acept_compl        +
                                     acept_lplazo       + acept_cs_isss      +
                                     acept_viv_gar_92   + acept_viv_gar_2008 +
                                     acept_ext_sar_isss + acept_ext_ret_isss +
                                     acept_ext_cv_isss_pat +
                                     acept_ext_cv_isss_tra +
                                     acept_ahr_sol_tra  + acept_ahr_sol_dep

         LET vtotal_aport_rech = 0
         LET vtotal_int_rech   = 0
 
         IF d15_impt_total_xpagar IS NULL THEN
            LET  c15_impt_total_xpagar = "000000000000000"
         ELSE
            LET c16_impt_total_xpagar = d15_impt_total_xpagar USING "&&&&&&&&&&&&&.&&"
            LET c15_impt_total_xpagar = c16_impt_total_xpagar[1,13],
                                        c16_impt_total_xpagar[15,16]
         END IF

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
                     reg_sum.impt_tot_sar_isss    USING '&&&&&&&&&&&&&&&',
                     reg_sum.impt_tot_ret_isss    USING '&&&&&&&&&&&&',
                     reg_sum.impt_tot_cv_isss_pat USING '&&&&&&&&&&&&&&&',
                     reg_sum.impt_tot_cv_isss_tra USING '&&&&&&&&&&&&&&&',
                     reg_sum.inte_ext_sar_isss    USING '&&&&&&&&&&&&',
                     reg_sum.inte_ext_ret_isss    USING '&&&&&&&&&&&&&&&',
                     reg_sum.inte_ext_cv_isss_pat USING '&&&&&&&&&&&&&&&',
                     reg_sum.inte_ext_cv_isss_tra USING '&&&&&&&&&&&&&&&',
                     reg_sum.impt_tot_viv_92      USING '&&&&&&&&&&&&&&&',
                     reg_sum.impt_tot_viv_2008    USING '&&&&&&&&&&&&&&&',
                     reg_sum.inte_ext_viv_92      USING '&&&&&&&&&&&&&&&',
                     reg_sum.inte_ext_viv_2008    USING '&&&&&&&&&&&&&&&',
                     reg_sum.impt_tot_cs_isss     USING '&&&&&&&&&&&&&&&',
                     reg_sum.impt_tot_apo_vol     USING '&&&&&&&&&&&&&&&',
                     reg_sum.impt_tot_apo_comp    USING '&&&&&&&&&&&&&&&',
                     reg_sum.impt_tot_apo_lplazo  USING '&&&&&&&&&&&&&&&',
                     reg_sum.impt_apo_ahr_sol_tra USING '&&&&&&&&&&&&&&&',
                     reg_sum.impt_apo_ahr_sol_dep USING '&&&&&&&&&&&&&&&',
                     vnum_reg                     USING '&&&&&&&&&',
                     vnum_ctas_xpagar             USING '&&&&&&',
                     c15_impt_total_xpagar,
                     299 space
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
      SELECT UNIQUE n_seguro
      INTO  nssb
      FROM  dis_det_issste
      WHERE @folio = vfolio
      AND   @n_unico = curpa

      SELECT  count(UNIQUE n_seguro)
      INTO  conta_afi
      FROM  dis_det_issste
      WHERE @folio = vfolio
      AND   @n_unico = curpa

      SELECT "OK"
      FROM afi_mae_afiliado
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


FUNCTION cal_dias(fec_ini,fec_fin)
DEFINE
   fec_ini, fec_fin, fec DATE,
   num_dias              SMALLINT,
   anof, anoi            SMALLINT,
   diaf, diai            SMALLINT,
   anos                  SMALLINT,
   residuo               SMALLINT

   LET num_dias = 0

   IF fec_ini > fec_fin THEN
      ERROR " ERROR en la fechas de recepcion y afiliacion "
   ELSE
      LET anoi = YEAR(fec_ini)
      LET anof = YEAR(fec_fin)

      LET anos = anof - anoi

      LET fec = MDY(MONTH(fec_ini), DAY(fec_ini), YEAR(fec_fin))

      LET residuo = YEAR(fec_fin) MOD 4

      IF MONTH(fec_ini) = 2 AND DAY(fec_ini) = 29 AND residuo <> 0 THEN
         LET fec = MDY(MONTH(fec_ini), DAY(fec_ini-1), year(fec_fin))
      END IF

      IF fec > fec_fin THEN
         LET anos = anos -1
      END IF

      LET num_dias = anos * 365
   END IF

   RETURN num_dias
END FUNCTION

