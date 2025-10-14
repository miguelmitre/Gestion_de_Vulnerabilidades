###############################################################################
#Proyecto          => SAFRE                                                   #
#Propietario       => E.F.P.                                                  #
#Programa          => DISB085B                                                #
-------------------------------------------------------------------------------
#Fecha generacion  => 21 Abril 2006                                           #
#Autor             => ALEJANDRO RAMIREZ                                       #
#Descripcion       => Agrega al archivo .DISPAF el layout nuevo de comision   #
#                  => de flujo y no afecta tablas productivas.(bimestral)     #
#                  => La manera de ejecutarse es: fglgo DISB085B nom_arc fol  #
#agregar           => tablas: tmp_comisiones (safre_tmp),                     #
#                             dis_det_aporte8(safre_af),                      #
#                             sube_det8      (script)                         #
###############################################################################
DATABASE safre_af

GLOBALS

   DEFINE vrecauda  CHAR(001)
   DEFINE 
      g_bat RECORD LIKE dis_ctrl_proceso.*,
      vhora_max     CHAR(08),
      vhora_final   CHAR(08),
      vresultado    CHAR(50),
      vetapa_cod    SMALLINT,
      vproc         CHAR(06),
      vrech         CHAR(06),
      hoy           DATE,
      vsalida       CHAR(01),
      nom_archivo   CHAR(12),
      gparam_dis    RECORD LIKE seg_modulo.*,
      gusuario      CHAR(08),
      comando       CHAR(200),
      vcod_afore    LIKE tab_afore_local.codigo_afore,
      vnom_archivo  CHAR(21),
      vreporte      CHAR(200),
      opc           CHAR(01),
      vcont_rech    INTEGER,
      vcont_acep    INTEGER,
      vcobra        SMALLINT,
      vfecha_lote   CHAR(08),
      vtipo_reporte CHAR(01),
      vaportes      CHAR(01),
      hora_inicial  CHAR(08),
      hora_final    CHAR(08),
      cla_sel       CHAR(1000),
      cla_apo       CHAR(500), --c22-6
      cla_com       CHAR(500), --c22-6
      cla_upd       CHAR(1500),

      tot_acep RECORD
         impt_ret           decimal(15,2),
         impt_act_rec_ret   decimal(15,2),
         impt_ces_vej       decimal(15,2),
         impt_act_r_ces_vej decimal(15,2),
         impt_aport_vol     decimal(15,2),
         impt_aport_compl   decimal(15,2), --c22-6
         impt_aport_pat     decimal(15,2),
         impt_cuota_soc     decimal(15,2),
         impt_aport_est     decimal(15,2),
         impt_aport_esp     decimal(15,2),
         impt_act_cuo_soc   decimal(15,2),
         impt_act_aport_est decimal(15,2),
         impt_act_cuo_esp   decimal(15,2),
         impt_viv_gar       decimal(15,2),
         part_viv           decimal(18,6),   ----jerry
         part_viv_gar       decimal(18,6)    ----jerry
      END RECORD,

      tot_acepint RECORD
         impt_ret           decimal(15,2),
         impt_act_rec_ret   decimal(15,2),
         impt_ces_vej       decimal(15,2),
         impt_act_r_ces_vej decimal(15,2),
         impt_aport_vol     decimal(15,2),
         impt_aport_compl   decimal(15,2), --c22-6
         impt_aport_pat     decimal(15,2),
         impt_cuota_soc     decimal(15,2),
         impt_aport_est     decimal(15,2),
         impt_aport_esp     decimal(15,2),
         impt_act_cuo_soc   decimal(15,2),
         impt_act_aport_est decimal(15,2),
         impt_act_cuo_esp   decimal(15,2),
         impt_viv_gar       decimal(15,2)
      END RECORD,

      tot_comi RECORD
         impt_ret           decimal(15,6),
         impt_ces_vej       decimal(15,6),
         impt_aport_est     decimal(15,6),
         impt_aport_esp     decimal(15,6) 
      END RECORD,
      
      tot_rech RECORD
         impt_ret           decimal(15,2),
         impt_act_rec_ret   decimal(15,2),
         impt_ces_vej       decimal(15,2),
         impt_act_r_ces_vej decimal(15,2),
         impt_aport_vol     decimal(15,2),
         impt_aport_compl   decimal(15,2), --c22-6
         impt_aport_pat     decimal(15,2),
         impt_cuota_soc     decimal(15,2),
         impt_aport_est     decimal(15,2),
         impt_aport_esp     decimal(15,2),
         impt_act_cuo_soc   decimal(15,2),
         impt_act_aport_est decimal(15,2),
         impt_act_cuo_esp   decimal(15,2),
         impt_viv_gar       decimal(15,2),
         part_viv           decimal(18,6),   ----jerry
         part_viv_gar       decimal(18,6)    ----jerry
      END RECORD,
        
      tot_rechint RECORD
         impt_ret           decimal(15,2),
         impt_act_rec_ret   decimal(15,2),
         impt_ces_vej       decimal(15,2),
         impt_act_r_ces_vej decimal(15,2),
         impt_aport_vol     decimal(15,2),
         impt_aport_compl   decimal(15,2), --c22-6
         impt_aport_pat     decimal(15,2),
         impt_cuota_soc     decimal(15,2),
         impt_aport_est     decimal(15,2),
         impt_aport_esp     decimal(15,2),
         impt_act_cuo_soc   decimal(15,2),
         impt_act_aport_est decimal(15,2),
         impt_act_cuo_esp   decimal(15,2),
         impt_viv_gar       decimal(15,2)
      END RECORD

   DEFINE reg_ident RECORD 
          nom_archivo   CHAR(20),
          xfolio        INTEGER, 
          tipo          SMALLINT
   END RECORD

   DEFINE prioridad CHAR(25)

   DEFINE g_com RECORD
      subcuenta      SMALLINT,
      val_porcentaje DECIMAL(16,6)
   END RECORD

   DEFINE gfecha_recepcion DATE

   DEFINE vconsec INTEGER,
	       vnssx   CHAR(11),
          vrow    INTEGER

   DEFINE graba SMALLINT

   DEFINE vfecha_aux DATE,
	  vperiodo   CHAR(06),
	  vbim_cert  INTEGER,
	  vbim_pago  INTEGER,
	  vmto_ret   DECIMAL(16,6),
	  vmto_ces   DECIMAL(16,6),
	  vcomret    DECIMAL(16,6),
	  vcomces    DECIMAL(16,6)

   DEFINE vnum_reg  INTEGER
   DEFINE tipo_archivo CHAR(01)  --@

   DEFINE vid_sie CHAR(08)   --c22-6

   DEFINE vnum_ctas_xpagar SMALLINT  --c22-6

END GLOBALS

MAIN
   DEFINE 
      vfolio INTEGER,
      nom_archivo CHAR(20)

DISPLAY " "
DISPLAY ".1"

let vbim_cert = 0
let vbim_pago = 0
let vmto_ret  = 0
let vmto_ces  = 0
let vcomret   = 0
let vcomces   = 0

   CALL STARTLOG("DISB085B.log")     
  
   LET reg_ident.nom_archivo             = ARG_VAL(1)
   LET reg_ident.xfolio                  = ARG_VAL(2)
   LET reg_ident.tipo                    = ARG_VAL(3)


   IF reg_ident.nom_archivo IS NULL OR reg_ident.nom_archivo=' ' THEN 
      ERROR "FAVOR DE INGRESAR EL NOMBRE DEL ARCHIVO"
      SLEEP 5
      exit program
   END IF
   IF reg_ident.xfolio IS NULL THEN 
      ERROR "FAVOR DE INGRESAR EL FOLIO DE REFERENCIA"
      SLEEP 5
      exit program
   END IF

   LET reg_ident.tipo        = 1
 
------------------------------------------------------------ OJO

   DISPLAY "Archivo               : ",reg_ident.nom_archivo
   DISPLAY "xfolio                : ",reg_ident.xfolio
   DISPLAY "Tipo dispersion       : ",reg_ident.tipo

   LET hoy = TODAY

let vconsec = 0

      CALL Proceso_principal() RETURNING vfolio

      LET vproc = vcont_acep
     
      LET vhora_final = TIME

END MAIN

FUNCTION Proceso_principal()
   DEFINE 
      ejecuta CHAR(200),
      vfolio  INTEGER

   CALL Inicializa()

   ERROR "PROCESANDO INFORMACION...",reg_ident.nom_archivo 
   
   CALL Separa_archivo()                                  -- ETAPA 3   
   CALL Sube_datos() RETURNING vfolio                     -- ETAPA 4
   LET vfolio= 4567                              
   IF vfolio <> 0 THEN

      --- ACTIVA OPTCOMPIND PARA QUE TODOS LOS QUERYES ENTREN POR INDICE -----

      LET ejecuta = "cd ",gparam_dis.ruta_exp CLIPPED,
                       "/;OPTCOMPIND=0;export OPTCOMPIND;"
      RUN ejecuta

      CALL Genera_salidas(vfolio)

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"/",
                 "; cp cza dep sum ",gparam_dis.ruta_envio CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; cat cza rech dep sum > ",vnom_archivo CLIPPED
      RUN ejecuta


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

--   LET gparam_dis.ruta_rescate='/des/act/safre/dis/fte/part_viv'
--   LET gparam_dis.ruta_envio='/des/act/safre/dis/fte/part_viv'

   SELECT codigo_afore
     INTO vcod_afore
     FROM tab_afore_local

   LET vnom_archivo = reg_ident.nom_archivo[1,6] CLIPPED,".CONFAF10" CLIPPED

   LET tot_acep.impt_ret           = 0
   LET tot_acep.impt_act_rec_ret   = 0
   LET tot_acep.impt_ces_vej       = 0
   LET tot_acep.impt_act_r_ces_vej = 0
   LET tot_acep.impt_aport_vol     = 0
   LET tot_acep.impt_aport_compl   = 0  --c22-6
   LET tot_acep.impt_aport_pat     = 0
   LET tot_acep.impt_cuota_soc     = 0
   LET tot_acep.impt_aport_est     = 0
   LET tot_acep.impt_aport_esp     = 0
   LET tot_acep.impt_act_cuo_soc   = 0
   LET tot_acep.impt_act_aport_est = 0
   LET tot_acep.impt_act_cuo_esp   = 0
   LET tot_acep.impt_viv_gar      = 0
   LET tot_acep.part_viv           = 0   ----jerry
   LET tot_acep.part_viv_gar       = 0   ----jerry

   LET tot_acepint.impt_ret           = 0
   LET tot_acepint.impt_act_rec_ret   = 0
   LET tot_acepint.impt_ces_vej       = 0
   LET tot_acepint.impt_act_r_ces_vej = 0
   LET tot_acepint.impt_aport_vol     = 0
   LET tot_acepint.impt_aport_compl   = 0  --c22-6
   LET tot_acepint.impt_aport_pat     = 0
   LET tot_acepint.impt_cuota_soc     = 0
   LET tot_acepint.impt_aport_est     = 0
   LET tot_acepint.impt_aport_esp     = 0
   LET tot_acepint.impt_act_cuo_soc   = 0
   LET tot_acepint.impt_act_aport_est = 0
   LET tot_acepint.impt_act_cuo_esp   = 0
   LET tot_acepint.impt_viv_gar       = 0

   LET tot_comi.impt_ret           = 0
   LET tot_comi.impt_ces_vej       = 0
   LET tot_comi.impt_aport_est     = 0
   LET tot_comi.impt_aport_esp     = 0
      
   LET tot_rech.impt_ret           = 0
   LET tot_rech.impt_act_rec_ret   = 0
   LET tot_rech.impt_ces_vej       = 0
   LET tot_rech.impt_act_r_ces_vej = 0
   LET tot_rech.impt_aport_vol     = 0
   LET tot_rech.impt_aport_compl   = 0  --c22-6
   LET tot_rech.impt_aport_pat     = 0
   LET tot_rech.impt_cuota_soc     = 0
   LET tot_rech.impt_aport_est     = 0
   LET tot_rech.impt_aport_esp     = 0
   LET tot_rech.impt_act_cuo_soc   = 0
   LET tot_rech.impt_act_aport_est = 0
   LET tot_rech.impt_act_cuo_esp   = 0
   LET tot_rech.impt_viv_gar       = 0
   LET tot_rech.part_viv           = 0   ----jerry
   LET tot_rech.part_viv_gar       = 0   ----jerry

   LET tot_rechint.impt_ret           = 0
   LET tot_rechint.impt_act_rec_ret   = 0
   LET tot_rechint.impt_ces_vej       = 0
   LET tot_rechint.impt_act_r_ces_vej = 0
   LET tot_rechint.impt_aport_vol     = 0
   LET tot_rechint.impt_aport_compl   = 0  --c22-6
   LET tot_rechint.impt_aport_pat     = 0
   LET tot_rechint.impt_cuota_soc     = 0
   LET tot_rechint.impt_aport_est     = 0
   LET tot_rechint.impt_aport_esp     = 0
   LET tot_rechint.impt_act_cuo_soc   = 0
   LET tot_rechint.impt_act_aport_est = 0
   LET tot_rechint.impt_act_cuo_esp   = 0
   LET tot_rechint.impt_viv_gar       = 0

   LET vcont_acep = 0
   LET vcont_rech = 0

   LET vnum_reg = 0
   LET vnum_ctas_xpagar = 0     --c22-6

         
   DATABASE safre_tmp
   INSERT INTO tmp_comisiones
   SELECT nss,folio_sua,subcuenta,
          SUM(monto_en_pesos) * -1 mto
   FROM   safre_af:dis_provision
   WHERE  folio=reg_ident.xfolio
   AND    tipo_movimiento between 100 and 109
   GROUP BY 1,2,3

   CREATE INDEX ix_comi ON tmp_comisiones (nss,folio_sua,subcuenta);
   DATABASE safre_af

END FUNCTION


FUNCTION Separa_archivo()  -- ETAPA 3 
   DEFINE
      ejecuta CHAR(200)

   LET ejecuta = "head -n 1 ",gparam_dis.ruta_rescate CLIPPED,"/",
                reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/cza"
   RUN ejecuta
                
   LET ejecuta = "sed -e '/^02/!d' ",gparam_dis.ruta_rescate CLIPPED,"/",
                reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/det"
   RUN ejecuta
                
   LET ejecuta = "sed -e '/^03/!d' ",gparam_dis.ruta_rescate CLIPPED,"/",
                reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/int"
   RUN ejecuta
                
   LET ejecuta = "sed -e '/^08/!d' ",gparam_dis.ruta_rescate CLIPPED,"/",
                reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/dep"
   RUN ejecuta


   LET ejecuta = "sed -e '/^09/!d' ",gparam_dis.ruta_rescate CLIPPED,"/",
                reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/sum"
   RUN ejecuta
                 
END FUNCTION

FUNCTION Sube_datos()  -- ETAPA 4
   DEFINE
      ejecuta          CHAR(200),
      vlote            CHAR(03),
      cla_ins          CHAR(200),
      vfolio           INTEGER,
      vfecha_recepcion DATE,
      vhora_recepcion  CHAR(10),
      vestado          SMALLINT,
      vfecha_estado    DATE,
      vhora_estado     CHAR(10),
      vparam           CHAR(10),
      i                INTEGER,
      cfecha_envio     CHAR(10),
      vfecha_envio     CHAR(08)

   LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                 "; cut -c 17-24 cza > fecha_lote"
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                 "; cut -c 25-27 cza > lote"
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
   LOAD FROM ejecuta INSERT INTO tmp_lote

   SELECT fecha_lote
     INTO vfecha_lote
     FROM fecha_lote
   
   SELECT lote
     INTO vlote
     FROM tmp_lote

   SELECT "X"
     FROM dis_cza_aporte8 
    WHERE fech_creac_lote = vfecha_lote
      AND lote_del_dia    = vlote

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

      --------------------   GENERA obtengo folio    --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_cza"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"   ---- Cabeza
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      --------------------   GENERA dis_det_aporte   --------------------

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_det
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza > vfolio_det" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det2 con el numero de registros igual al
      -- num. registros que tiene el detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f vfolio_det det > vfolio_det2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det borrando lineas que no sirven
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_det2 > vfolio_det" CLIPPED 
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm vfolio_det2"
      RUN ejecuta

      -- Se crea det2 cortando pos 1-283 y det3 cortando
      -- hasta del pos 285-295 y se pegan en el archivo det
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-350 det > det2"   ----jerry jerry jerry --c22-6
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 352-360 det > det3" ----jerry jerry jerry --c22-6
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste -d '1' det2 det3 > det"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det2 det3"
      RUN ejecuta
    
      -- Se crea det1 pegando datos genrales con el detalle 
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_det det > det1" CLIPPED
      RUN ejecuta

      -- Se crea det eliminando espacio en blanco que se genera cuando
      -- se pegan los 2 archivos
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' det1 > det "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det1 vfolio_det"
      RUN ejecuta

      -- Se suben los datos del detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;dbload -d safre_af -c sube_det8 -l dbload.log;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det"
      RUN ejecuta


------ ACTIVA OPTCOMPIND PARA QUE TODOS LOS QUERYES ENTREN POR INDICE -----

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
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
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
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
   FORMAT
      ON EVERY ROW
         PRINT vfecha_archivo,vfecha_envio,"2"
END REPORT


FUNCTION Genera_salidas(vfolio)

   DEFINE 
      vfolio INTEGER,
      acept_rcv     DECIMAL(16,2),
   acept_vol     DECIMAL(16,2),     --c22-6
   acept_compl   DECIMAL(16,2),     --c22-6
      acept_viv     DECIMAL(16,2),
      acept_gub     DECIMAL(16,2),
      acept_viv_gar DECIMAL(16,2),
      acept_part    DECIMAL(18,6),   ----jerry
      acept_int_rcv DECIMAL(16,2),
   acept_int_vol   DECIMAL(16,2),     --c22-6
   acept_int_compl DECIMAL(16,2),     --c22-6
      acept_int_viv DECIMAL(16,2),
      acept_int_gub DECIMAL(16,2),
      acept_int_vivg DECIMAL(16,2),
      acept_part_gar DECIMAL(18,6),  ----jerry
      recha_rcv     DECIMAL(16,2),
   recha_vol     DECIMAL(16,2),     --c22-6
   recha_compl   DECIMAL(16,2),     --c22-6
      recha_viv     DECIMAL(16,2),
      recha_gub     DECIMAL(16,2),
      recha_viv_gar  DECIMAL(16,2),
      recha_part    DECIMAL(18,6),   ----jerry
      recha_int_rcv DECIMAL(16,2),
   recha_int_vol   DECIMAL(16,2),     --c22-6
   recha_int_compl DECIMAL(16,2),     --c22-6
      recha_int_viv DECIMAL(16,2),
      recha_int_gub DECIMAL(16,2),
      recha_int_vivg DECIMAL(16,2),
      recha_part_gar DECIMAL(18,6)   ----jerry

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
      filler            CHAR(271),   ----jerry
      ident_arch        CHAR(01)      
   END RECORD

   DEFINE reg_rech RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),
      consec_reg_lote    INTEGER,
      n_seguro           CHAR(11),
      n_rfc              CHAR(13),
      n_unico            CHAR(18),
      nom_trabajador     CHAR(50),
      periodo_pago       CHAR(06),
      fech_pago          CHAR(08),
      fech_valor_rcv     CHAR(08),
      fech_valor_viv     CHAR(08),
      ult_salario_diario DECIMAL(9,2),
      folio_pago_sua     CHAR(06),
      reg_patronal_imss  CHAR(11),
      rfc_patron         CHAR(13),
      cve_ent_receptora  CHAR(03),
      dias_cotz_bimest   CHAR(02),
      dias_incap_bimest  CHAR(02),
      dias_ausent_bimest CHAR(02),
      impt_ret           DECIMAL(16,2),
      impt_act_rec_ret   DECIMAL(16,2),
      impt_ces_vej       DECIMAL(16,2),
      impt_act_r_ces_vej DECIMAL(16,2),
      impt_aport_vol     DECIMAL(16,2),
      impt_aport_compl   DECIMAL(16,2), --c22-6
      impt_aport_pat     DECIMAL(16,2),
      impt_cuota_soc     DECIMAL(16,2),
      impt_aport_est     DECIMAL(16,2),
      impt_aport_esp     DECIMAL(16,2),
      impt_act_cuo_soc   DECIMAL(16,2),
      impt_act_aport_est DECIMAL(16,2),
      impt_act_cuo_esp   DECIMAL(16,2),
      fecha_pago_gub     CHAR(08), 
      ident_viv_garantia CHAR(01),
      part_viv           DECIMAL(18,6),  ----jerry
      valor_part         DECIMAL(18,6),  ----jerry
      filler1            CHAR(43),       ----jerry --c22-6
      result_operacion   CHAR(02),
      det_mot_rechazo1   CHAR(03),
      det_mot_rechazo2   CHAR(03),
      det_mot_rechazo3   CHAR(03) 
   END RECORD

   DEFINE reg_rech1 RECORD
      comiflu_ret        DECIMAL(16,2),
      comiflu_cv         DECIMAL(16,2),
      comiflu_est        DECIMAL(16,2),
      comiflu_esp        DECIMAL(16,2),
      comiflu_tot        DECIMAL(16,2),
      fecha_alt_afo      DATE,
      fecha_1a_afil      DATE
   -- tipo_trabaja       CHAR(02)
   END RECORD

   DEFINE reg_dep RECORD
      tipo_registro    CHAR(02),
      ident_servicio   CHAR(02),
      ident_pago       CHAR(16),
      importe          DECIMAL(15,2),
      fech_liquidacion DATE,
      impt_aport_acept DECIMAL(15,2),
      impt_aport_dev   DECIMAL(15,2) ,
      part_viv_env     DECIMAL(22,6),  ----jerry
      part_viv_acept   DECIMAL(22,6),  ----jerry
      part_viv_dev     DECIMAL(22,6),  ----jerry
 ident_siefore    CHAR(08),       --c22-6
 tipo_siefore     SMALLINT        --c22-6 
   END RECORD

   DEFINE reg_sum RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),
      ident_operacion    CHAR(02),
      tipo_ent_origen    CHAR(02),
      clave_ent_origen   CHAR(03),
      tipo_ent_destino   CHAR(02),
      clave_ent_destino  CHAR(03),
      fech_creac_lote    CHAR(08),
      lote_del_dia       CHAR(03),
      impt_total_rcv     DECIMAL(17,2),
      impt_total_int_rcv DECIMAL(17,2),
 impt_tot_apo_vol   DECIMAL(17,2),       --c22-6
 impt_tot_int_vol   DECIMAL(17,2),       --c22-6
 impt_tot_apo_comp  DECIMAL(17,2),       --c22-6
 impt_tot_int_comp  DECIMAL(17,2),       --c22-6
      impt_total_patr    DECIMAL(17,2),
      impt_tatal_int_pat DECIMAL(17,2),
      impt_total_guber   DECIMAL(17,2),
      impt_total_int_gub DECIMAL(17,2),
      num_de_reg_aport   CHAR(08),
      num_cuenta_xpagar  CHAR(06),
      impt_total_xpagar  DECIMAL(17,2),
      impt_tot_viv_gar   DECIMAL(17,2),
      impt_tot_int_vgar  DECIMAL(17,2),
      impt_tot_part_viv  DECIMAL(22,6),   ----jerry
      tot_part_viv_gar   DECIMAL(22,6),   ----jerry
      filler             CHAR(88)         ----jerry --c22-6
   END RECORD,

   vimpt_total_rcv     DECIMAL(17,2),
   vimpt_total_int_rcv DECIMAL(17,2),
 vimpt_tot_apo_vol   DECIMAL(17,2),       --c22-6
 vimpt_tot_int_vol   DECIMAL(17,2),       --c22-6
 vimpt_tot_apo_comp  DECIMAL(17,2),       --c22-6
 vimpt_tot_int_comp  DECIMAL(17,2),       --c22-6
   vimpt_total_patr    DECIMAL(17,2),
   vimpt_total_int_pat DECIMAL(17,2),
   vimpt_total_guber   DECIMAL(17,2),
   vimpt_total_int_gub DECIMAL(17,2),
   vimpt_total_xpagar  DECIMAL(17,2),
   vimpt_tot_viv_gar   DECIMAL(17,2),
   vimpt_tot_int_vgar  DECIMAL(17,2),
   vimpt_tot_part_viv  DECIMAL(22,6),  ----jerry
   vtot_part_viv_gar   DECIMAL(22,6),  ----jerry

   c15_impt_total_rcv,
   c15_impt_total_int_rcv,
   c15_impt_aport_pat,
   c15_impt_total_int_pat,
   c15_impt_total_guber,
   c15_impt_total_int_gub,
   c15_impt_total_xpagar,
   c15_impt_tot_viv_gar,
   c15_impt_tot_int_vgar,
   c15_impt_aport_acept  CHAR(15),
   c16_impt_total_rcv,
   c16_impt_total_int_rcv,
   c16_impt_aport_pat,
   c16_impt_total_int_pat,
   c16_impt_total_guber,
   c16_impt_total_int_gub,
   c16_impt_total_xpagar,
   c16_impt_tot_viv_gar,
   c16_impt_tot_int_vgar,
   c16_impt_aport_acept  CHAR(16),
   renvio                CHAR(60),
   vtotal_aport_rech,
   vtotal_int_rech       DECIMAL(15,2),
   i_num_de_reg_aport    INTEGER

   DEFINE xident_siefore CHAR(08),      --c22-6.6
          xtipo_siefore  SMALLINT,      --c22-6.6
          vimpt_acept    DECIMAL(15,2), --c22-6.6
          vtipo_sie      SMALLINT       --c22-6.6

   DEFINE vmto1          DECIMAL(16,2),
          vmto2          DECIMAL(16,2),
          vmto3          DECIMAL(16,2),
          vmto4          DECIMAL(16,2),
          vmto_tot       DECIMAL(16,2),
          vfent          DATE,
          vtipo          SMALLINT,
          vfecha_1a_afil DATE

   --------------- GENERA DETALLE   -------------------------------
   DECLARE cur_rech CURSOR FOR
   SELECT
      a.tipo_registro,
      a.ident_servicio,
      a.consec_reg_lote,
      a.n_seguro,
      a.n_rfc,
      a.n_unico,
      a.nom_trabajador,
      a.periodo_pago,
      a.fech_pago,
      a.fech_valor_rcv,
      a.fech_valor_viv,
      a.ult_salario_diario,
      a.folio_pago_sua,
      a.reg_patronal_imss,
      a.rfc_patron,
      a.cve_ent_receptora,
      a.dias_cotz_bimestre,
      a.dias_incap_bimest,
      a.dias_ausent_bimest,
      a.impt_ret,                         --/100,
      a.impt_act_rec_ret,                 --/100,
      a.impt_ces_vej,                     --/100,
      a.impt_act_r_ces_vej,               --/100,
      a.impt_aport_vol,                   --/100,
      a.impt_aport_compl,                 --/100,  --c22-6
      a.impt_aport_pat,                   --/100,
      a.impt_cuota_soc,                   --/100,
      a.impt_aport_est,                   --/100,
      a.impt_aport_esp,                   --/100,
      a.impt_act_cuo_soc,                 --/100,
      a.impt_act_aport_est,               --/100,
      a.impt_act_cuo_esp,                 --/100,
      a.fecha_pago_gub,
      a.ident_viv_garantia,
      a.part_viv,                         --/1000000,          ----jerry
      a.valor_part,                       --/1000000,        ----jerry
      a.filler1,           ----jerry
      a.result_operacion,
      a.det_mot_rechazo1,
      a.det_mot_rechazo2,
      a.det_mot_rechazo3 
    FROM dis_det_aporte8 a
   WHERE a.folio = vfolio
     ORDER BY  a.consec_reg_lote,a.tipo_registro


   LET reg_rech.cve_ent_receptora = "001"
   LET reg_rech.result_operacion  = "00"
   LET reg_rech.det_mot_rechazo1  = "000"
   LET reg_rech.det_mot_rechazo2  = "000"
   LET reg_rech.det_mot_rechazo3  = "000"
   

   DISPLAY "GENERANDO RECH..." #---AT 12,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/rech"
   START REPORT rep_rech TO vreporte
      FOREACH cur_rech INTO reg_rech.*

         --AGREGAMOS LAYOUT SOLICITADO
         --retiros
         LET vmto1=0
         LET vmto2=0
         LET vmto3=0
         LET vmto4=0
         LET vmto_tot=0
         LET reg_rech1.comiflu_ret       = 0
         LET reg_rech1.comiflu_cv        = 0
         LET reg_rech1.comiflu_est       = 0
         LET reg_rech1.comiflu_esp       = 0
         LET reg_rech1.comiflu_tot       = 0

         SELECT mto
         INTO   vmto1
         FROM   safre_tmp:tmp_comisiones
         WHERE  nss = reg_rech.n_seguro
         AND    folio_sua = reg_rech.folio_pago_sua
         AND    subcuenta = 1

         IF vmto1 IS NULL THEN LET vmto1 = 0 END IF

         --cesantia y vejes
         SELECT mto
         INTO   vmto2
         FROM   safre_tmp:tmp_comisiones
         WHERE  nss = reg_rech.n_seguro
         AND    folio_sua = reg_rech.folio_pago_sua
         AND    subcuenta = 2

         IF vmto2 IS NULL THEN LET vmto2 = 0 END IF

         --estatutarias    
         SELECT mto
         INTO   vmto3
         FROM   safre_tmp:tmp_comisiones
         WHERE  nss = reg_rech.n_seguro
         AND    folio_sua = reg_rech.folio_pago_sua
         AND    subcuenta = 6

         IF vmto3 IS NULL THEN LET vmto3 = 0 END IF

         --especiales      
         SELECT mto
         INTO   vmto4
         FROM   safre_tmp:tmp_comisiones
         WHERE  nss = reg_rech.n_seguro
         AND    folio_sua = reg_rech.folio_pago_sua 
         AND    subcuenta = 9

         IF vmto4 IS NULL THEN LET vmto4 = 0 END IF

         LET vmto_tot = vmto1 + vmto2 + vmto3 + vmto4

         IF vmto_tot IS NULL THEN LET vmto_tot = 0 END IF

         LET reg_rech1.comiflu_ret       = vmto1
         LET reg_rech1.comiflu_cv        = vmto2
         LET reg_rech1.comiflu_est       = vmto3
         LET reg_rech1.comiflu_esp       = vmto4
         LET reg_rech1.comiflu_tot       = vmto_tot

         LET vfent=null
         LET vtipo=null
         LET vfecha_1a_afil=null

         SELECT fentcons,tipo_solicitud,fecha_1a_afil
         INTO   vfent,vtipo,vfecha_1a_afil
         FROM   afi_mae_afiliado
         WHERE  n_seguro = reg_rech.n_seguro 
         
         IF  vtipo = 5 THEN
             --asignado
             LET reg_rech1.fecha_alt_afo     = vfent
          -- LET reg_rech1.tipo_trabaja      = "TA"
         ELSE 
          -- IF vtipo = 1 THEN
                --registrado
                LET reg_rech1.fecha_alt_afo     = vfent
           --   LET reg_rech1.tipo_trabaja      = "TR"
          -- END IF
         END IF
                
         LET reg_rech1.fecha_1a_afil = vfecha_1a_afil

         OUTPUT TO REPORT rep_rech(reg_rech.*,reg_rech1.*)
      END FOREACH
   FINISH REPORT rep_rech

END FUNCTION

REPORT rep_rech(reg_rech,reg_rech2)
   DEFINE reg_rech RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),
      consec_reg_lote    INTEGER,
      n_seguro           CHAR(11),
      n_rfc              CHAR(13),
      n_unico            CHAR(18),
      nom_trabajador     CHAR(50),
      periodo_pago       CHAR(06),
      fech_pago          CHAR(08),
      fech_valor_rcv     CHAR(08),
      fech_valor_viv     CHAR(08),
      ult_salario_diario DECIMAL(9,2),
      folio_pago_sua     CHAR(06),
      reg_patronal_imss  CHAR(11),
      rfc_patron         CHAR(13),
      cve_ent_receptora  CHAR(03),
      dias_cotz_bimest   CHAR(02),
      dias_incap_bimest  CHAR(02),
      dias_ausent_bimest CHAR(02),
      impt_ret           DECIMAL(16,2),
      impt_act_rec_ret   DECIMAL(16,2),
      impt_ces_vej       DECIMAL(16,2),
      impt_act_r_ces_vej DECIMAL(16,2),
      impt_aport_vol     DECIMAL(16,2),
      impt_aport_compl   DECIMAL(16,2),  --c22-6
      impt_aport_pat     DECIMAL(16,2),
      impt_cuota_soc     DECIMAL(16,2),
      impt_aport_est     DECIMAL(16,2),
      impt_aport_esp     DECIMAL(16,2),
      impt_act_cuo_soc   DECIMAL(16,2),
      impt_act_aport_est DECIMAL(16,2),
      impt_act_cuo_esp   DECIMAL(16,2),
      fecha_pago_gub     CHAR(08), 
      ident_viv_garantia CHAR(01),
      part_viv           DECIMAL(18,6),  ----jerry
      valor_part         DECIMAL(18,6),  ----jerry
      filler1            CHAR(43),       --c22-6
      result_operacion   CHAR(02),
      det_mot_rechazo1   CHAR(03),
      det_mot_rechazo2   CHAR(03),
      det_mot_rechazo3   CHAR(03) 
   END RECORD 

   DEFINE reg_rech2 RECORD
      comiflu_ret        DECIMAL(16,2),
      comiflu_cv         DECIMAL(16,2),
      comiflu_est        DECIMAL(16,2),
      comiflu_esp        DECIMAL(16,2),
      comiflu_tot        DECIMAL(16,2),
      fecha_alt_afo      DATE,
      fecha_1a_afil      DATE 
    --tipo_trabaja       CHAR(02)
   END RECORD,

   cconsec_reg_lote    CHAR(08),
   cult_salario_diario CHAR(08),
   cimpt_ret           CHAR(08),
   cimpt_act_rec_ret   CHAR(08),
   cimpt_ces_vej       CHAR(08),
   cimpt_act_r_ces_vej CHAR(08),
   cimpt_aport_vol     CHAR(08),
   cimpt_aport_compl   CHAR(08),  --c22-6
   cimpt_aport_pat     CHAR(08),
   cimpt_cuota_soc     CHAR(08),
   cimpt_aport_est     CHAR(08),
   cimpt_aport_esp     CHAR(08),
   cimpt_act_cuo_soc   CHAR(08),
   cimpt_act_aport_est CHAR(08),
   cimpt_act_cuo_esp   CHAR(08),
   xconsec_reg_lote    CHAR(08),
   xult_salario_diario CHAR(07),
   xdias_cotz_bimest   CHAR(02),
   xdias_incap_bimest  CHAR(02),
   xdias_ausent_bimest CHAR(02),
   ximpt_ret           CHAR(07),
   ximpt_act_rec_ret   CHAR(07),
   ximpt_ces_vej       CHAR(07),
   ximpt_act_r_ces_vej CHAR(07),
   ximpt_aport_vol     CHAR(07),
   ximpt_aport_compl   CHAR(07),  --c22-6
   ximpt_aport_pat     CHAR(07),
   ximpt_cuota_soc     CHAR(07),
   ximpt_aport_est     CHAR(07),
   ximpt_aport_esp     CHAR(07),
   ximpt_act_cuo_soc   CHAR(07),
   ximpt_act_aport_est CHAR(07),
   ximpt_act_cuo_esp   CHAR(07),

   cconsec_reg_lotei    CHAR(08),
   cult_salario_diarioi CHAR(08),
   cimpt_reti           CHAR(08),
   cimpt_act_rec_reti   CHAR(08),
   cimpt_ces_veji       CHAR(08),
   cimpt_act_r_ces_veji CHAR(08),
   cimpt_aport_voli     CHAR(08),
   cimpt_aport_compli   CHAR(08),  --c22-6
   cimpt_aport_pati     CHAR(08),
   cimpt_cuota_soci     CHAR(08),
   cimpt_aport_esti     CHAR(08),
   cimpt_aport_espi     CHAR(08),
   cimpt_act_cuo_soci   CHAR(08),
   cimpt_act_aport_esti CHAR(08),
   cimpt_act_cuo_espi   CHAR(08),
   xconsec_reg_lotei    CHAR(08),
   xult_salario_diarioi CHAR(07),
   xdias_cotz_bimesti   CHAR(02),
   xdias_incap_bimesti  CHAR(02),
   xdias_ausent_bimesti CHAR(02),
   ximpt_reti           CHAR(07),
   ximpt_act_rec_reti   CHAR(07),
   ximpt_ces_veji       CHAR(07),
   ximpt_act_r_ces_veji CHAR(07),
   ximpt_aport_voli     CHAR(07),
   ximpt_aport_compli   CHAR(07),  --c22-6
   ximpt_aport_pati     CHAR(07),
   ximpt_cuota_soci     CHAR(07),
   ximpt_aport_esti     CHAR(07),
   ximpt_aport_espi     CHAR(07),
   ximpt_act_cuo_soci   CHAR(07),
   ximpt_act_aport_esti CHAR(07),
   ximpt_act_cuo_espi   CHAR(07)

   DEFINE xcomi1        CHAR(08),
          xcomi2        CHAR(08),
          xcomi3        CHAR(08),
          xcomi4        CHAR(08),
          xcomitot      CHAR(08)

   DEFINE ycomi1        CHAR(07),
          ycomi2        CHAR(07),
          ycomi3        CHAR(07),
          ycomi4        CHAR(07),
          ycomitot      CHAR(07)

   OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT
      ON EVERY ROW

      IF reg_rech.tipo_registro = "02" THEN
         lET vnum_reg = vnum_reg + 1
      END IF
    
LET cult_salario_diario = reg_rech.ult_salario_diario / 100 USING '&&&&&.&&'
LET cimpt_ret           = reg_rech.impt_ret           / 100 USING '&&&&&.&&'
LET cimpt_act_rec_ret   = reg_rech.impt_act_rec_ret   / 100 USING '&&&&&.&&'
LET cimpt_ces_vej       = reg_rech.impt_ces_vej       / 100 USING '&&&&&.&&'
LET cimpt_act_r_ces_vej = reg_rech.impt_act_r_ces_vej / 100 USING '&&&&&.&&'
LET cimpt_aport_vol     = reg_rech.impt_aport_vol     / 100 USING '&&&&&.&&'

LET cimpt_aport_compl=reg_rech.impt_aport_compl/100 USING '&&&&&.&&' --c22-6

LET cimpt_aport_pat     = reg_rech.impt_aport_pat     / 100 USING '&&&&&.&&'
LET cimpt_cuota_soc     = reg_rech.impt_cuota_soc     / 100 USING '&&&&&.&&'
LET cimpt_aport_est     = reg_rech.impt_aport_est     / 100 USING '&&&&&.&&'
LET cimpt_aport_esp     = reg_rech.impt_aport_esp     / 100 USING '&&&&&.&&'
LET cimpt_act_cuo_soc   = reg_rech.impt_act_cuo_soc   / 100 USING '&&&&&.&&'
LET cimpt_act_aport_est = reg_rech.impt_act_aport_est / 100 USING '&&&&&.&&'
LET cimpt_act_cuo_esp   = reg_rech.impt_act_cuo_esp   / 100 USING '&&&&&.&&'

LET xcomi1              = reg_rech2.comiflu_ret             USING '&&&&&.&&'
LET xcomi2              = reg_rech2.comiflu_cv              USING '&&&&&.&&'
LET xcomi3              = reg_rech2.comiflu_est             USING '&&&&&.&&'
LET xcomi4              = reg_rech2.comiflu_esp             USING '&&&&&.&&'
LET xcomitot            = reg_rech2.comiflu_tot             USING '&&&&&.&&'


LET xconsec_reg_lote    = reg_rech.consec_reg_lote    USING '&&&&&&&&'
LET xult_salario_diario = cult_salario_diario[1,05],  cult_salario_diario[7,08]
LET xdias_cotz_bimest   = reg_rech.dias_cotz_bimest   USING '&&'
LET xdias_incap_bimest  = reg_rech.dias_incap_bimest  USING '&&'
LET xdias_ausent_bimest = reg_rech.dias_ausent_bimest USING '&&'
LET ximpt_ret           = cimpt_ret          [1,05],cimpt_ret          [07,08]
LET ximpt_act_rec_ret   = cimpt_act_rec_ret  [1,05],cimpt_act_rec_ret  [07,08]
LET ximpt_ces_vej       = cimpt_ces_vej      [1,05],cimpt_ces_vej      [07,08]
LET ximpt_act_r_ces_vej = cimpt_act_r_ces_vej[1,05],cimpt_act_r_ces_vej[07,08]
LET ximpt_aport_vol     = cimpt_aport_vol    [1,05],cimpt_aport_vol    [07,08]

LET ximpt_aport_compl=cimpt_aport_compl[1,05],cimpt_aport_compl[07,08] --c22-6

LET ximpt_aport_pat     = cimpt_aport_pat    [1,05],cimpt_aport_pat    [07,08]
LET ximpt_cuota_soc     = cimpt_cuota_soc    [1,05],cimpt_cuota_soc    [07,08]
LET ximpt_aport_est     = cimpt_aport_est    [1,05],cimpt_aport_est    [07,08]
LET ximpt_aport_esp     = cimpt_aport_esp    [1,05],cimpt_aport_esp    [07,08]
LET ximpt_act_cuo_soc   = cimpt_act_cuo_soc  [1,05],cimpt_act_cuo_soc  [07,08]
LET ximpt_act_aport_est = cimpt_act_aport_est[1,05],cimpt_act_aport_est[07,08]
LET ximpt_act_cuo_esp   = cimpt_act_cuo_esp  [1,05],cimpt_act_cuo_esp  [07,08]

LET ycomi1              = xcomi1             [1,05],xcomi1             [07,08]
LET ycomi2              = xcomi2             [1,05],xcomi2             [07,08]
LET ycomi3              = xcomi3             [1,05],xcomi3             [07,08]
LET ycomi4              = xcomi4             [1,05],xcomi4             [07,08]
LET ycomitot            = xcomitot           [1,05],xcomitot           [07,08]

         PRINT
            COLUMN 1,reg_rech.tipo_registro,
                     reg_rech.ident_servicio,
                     reg_rech.consec_reg_lote    USING '&&&&&&&&',
                     reg_rech.n_seguro,
                     reg_rech.n_rfc,
                     reg_rech.n_unico,
                     reg_rech.nom_trabajador,
                     reg_rech.periodo_pago,
                     reg_rech.fech_pago,
                     reg_rech.fech_valor_rcv,
                     reg_rech.fech_valor_viv,
                     reg_rech.ult_salario_diario USING '&&&&&&&',
                     reg_rech.folio_pago_SUA,
                     reg_rech.reg_patronal_IMSS,
                     reg_rech.rfc_patron,
                     reg_rech.cve_ent_receptora,
                     xdias_cotz_bimest,
                     xdias_incap_bimest,
                     xdias_ausent_bimest,
                     ximpt_ret,
                     ximpt_act_rec_ret,
                     ximpt_ces_vej,
                     ximpt_act_r_ces_vej,
                     ximpt_aport_vol,
                     ximpt_aport_compl, --c22-6
                     ximpt_aport_pat,
                     ximpt_cuota_soc,
                     ximpt_aport_est,
                     ximpt_aport_esp,
                     ximpt_act_cuo_soc,
                     ximpt_act_aport_est,
                     ximpt_act_cuo_esp,
                     reg_rech.fecha_pago_gub,
                     reg_rech.ident_viv_garantia,
                     reg_rech.part_viv   USING '&&&&&&&&&&&&&&&', ----jerry
                     reg_rech.valor_part USING '&&&&&&&&&&&',     ----jerry
                     reg_rech.filler1,
                     "00",                      --- result_operacion,
                     reg_rech.det_mot_rechazo1,
                     reg_rech.det_mot_rechazo2,
                     reg_rech.det_mot_rechazo3,
                     --AGREGANDO LAYOUT
                     ycomi1, 
                     ycomi2, 
                     ycomi3, 
                     ycomi4, 
                     ycomitot,
                     reg_rech2.fecha_alt_afo USING 'YYYYMMDD',
                     reg_rech2.fecha_1a_afil USING 'YYYYMMDD'
                  -- reg_rech2.tipo_trabaja
END REPORT

