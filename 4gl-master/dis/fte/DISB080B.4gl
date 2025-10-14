###############################################################################
#Proyecto          => SAFRE                                                   #
#Propietario       => E.F.P.                                                  #
#Programa          => DISB080B                                                #
-------------------------------------------------------------------------------
#Fecha generacion  => 04 Julio 2005                                           #
#Autor             => ALEJANDRO RAMIREZ                                       #
#Descripcion       => Agrega al archivo .DISPAF el layout nuevo de comision   #
#                  => de flujo.                                               #
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
          pid                    ,
          proceso_cod            ,
          opera_cod     INTEGER  ,
          nom_archivo   CHAR(20) ,
          tipo          SMALLINT,
          fecha_recepcion DATE,
          recurrente    CHAR(01)
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

   CALL STARTLOG("DISB080B.log")     
  
   LET reg_ident.pid             = ARG_VAL(1)
   LET reg_ident.proceso_cod     = ARG_VAL(2)
   LET reg_ident.opera_cod       = ARG_VAL(3)
   LET reg_ident.nom_archivo     = ARG_VAL(4)
   LET reg_ident.tipo            = ARG_VAL(5)
   LET reg_ident.fecha_recepcion = ARG_VAL(6)
   LET reg_ident.recurrente      = ARG_VAL(7)
------------------------------------------------------------ OJO
 
   LET reg_ident.pid         = 1 --- proceso manual o bathc
   LET reg_ident.proceso_cod = 0
   LET reg_ident.opera_cod   = 0
   LET reg_ident.nom_archivo = "F050523.DISPAF"
   LET reg_ident.tipo        = 1
   LET reg_ident.fecha_recepcion = TODAY
   LET reg_ident.recurrente   = 'N'
 
------------------------------------------------------------ OJO

   DISPLAY "PID                   : ",reg_ident.pid
   DISPLAY "Proceso_cod           : ",reg_ident.proceso_cod
   DISPLAY "Opera_cod             : ",reg_ident.opera_cod
   DISPLAY "Archivo               : ",reg_ident.nom_archivo
   DISPLAY "Tipo dispersion       : ",reg_ident.tipo
   DISPLAY "Fecha recepcion       : ",reg_ident.fecha_recepcion
   DISPLAY "Recurrente            : ",reg_ident.recurrente

   LET hoy = TODAY

let vconsec = 0

      CALL Proceso_principal() RETURNING vfolio

      LET vproc = vcont_acep
     
      SELECT COUNT(*)
      INTO   vcont_rech
      FROM   safre_tmp:dis_det_aporte
  	   WHERE  folio = vfolio
      AND    result_operacion = "02"

      LET vrech = vcont_rech
     
      IF vfolio = 0 THEN
         LET vresultado = "ESTE ARCHIVO YA HA SIDO PROCESADO"
      ELSE 

         LET vresultado = "ACEPTADOS: ",vproc,"  RECHAZADOS: ",vrech

      END IF

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

   IF vfolio <> 0 THEN

      --- ACTIVA OPTCOMPIND PARA QUE TODOS LOS QUERYES ENTREN POR INDICE -----

      LET ejecuta = "cd ",gparam_dis.ruta_exp CLIPPED,
                       "/;OPTCOMPIND=0;export OPTCOMPIND;"
      RUN ejecuta

      CALL Genera_salidas(vfolio)

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
     FROM safre_tmp:dis_cza_aporte 
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

      --------------------   GENERA dis_cza_aporte   --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_cza"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"   ---- Cabeza
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza cza > cza1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' cza1 > cza "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_tmp -c sube_cza -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  cza cza1"
      RUN ejecuta

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
                    "/;dbload -d safre_tmp -c sube_det -l dbload.log;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det"
      RUN ejecuta

      --------------------   GENERA dis_det_interes   --------------------

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_int
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza > vfolio_int" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_int2 con el numero de registros igual al
      -- num. registros que tiene el detalle de intereses
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f vfolio_int int > vfolio_int2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_int borrando lineas que no sirven
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_int2 > vfolio_int" CLIPPED 
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm vfolio_int2"
      RUN ejecuta
      
      -- Se crea int2 cortando pos 1-283 y int3 cortando
      -- hasta del pos 285-295 y se pegan en el archivo int
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
         --         "; cut -c 1-283 int > int2"              --c22-6
                    "; cut -c 1-290 int > int2"              --c22-6
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
         --         "; cut -c 285-320 int > int3"   ----jerry  --c22-6
                    "; cut -c 292-360 int > int3"   ----jerry  --c22-6
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste -d '1' int2 int3 > int"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm int2 int3"
      RUN ejecuta

      -- Se crea int1 pegando datos genrales con el detalle 
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_int int > int1" CLIPPED
      RUN ejecuta

      -- Se crea int eliminando espacio en blanco que se genera cuando
      -- se pegan los 2 archivos
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' int1 > int "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm int1 vfolio_int"
      RUN ejecuta

      -- Se suben los datos del detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;dbload -d safre_tmp -c sube_int -l dbload.log;"
      RUN ejecuta

      -- Se borran todo los archivo auxiliares
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  archivos fecha_lote lote int"

      RUN ejecuta

      --------------------   GENERA dis_dep_aporte   --------------------
      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_dep"
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
         OUTPUT TO REPORT salida2(vfecha_lote,vfecha_envio)
      FINISH REPORT salida2

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_dep > dep01_1 " CLIPPED
      RUN ejecuta     

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_dep2 > dep02_1 " CLIPPED
      RUN ejecuta     

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f dep01_1 dep > dep01_2 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f dep02_1 dep > dep02_2 "  CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' dep01_2 > dep01_3" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^08/d' dep02_2 > dep02_3"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            ----    "; cut -c 1-127 dep > dep2; mv dep2 dep " ----jerry --c22-6
                    "; cut -c 1-138 dep > dep2; mv dep2 dep " ----jerry --c22-6
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste dep01_3 dep dep02_3 > dep1;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' dep1 > dep "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
"/;DBDATE=Y4MD0;export DBDATE;dbload -d safre_tmp -c sube_dep -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  vfolio_dep vfolio_dep2 dep01_1 dep02_1 dep01_2 dep02_2 dep01_3 dep02_3 dep dep1"

      RUN ejecuta
      --------------------   GENERA dis_sum_aporte   --------------------

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza sum > sum1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' sum1 > sum "
      RUN ejecuta
###1
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_tmp -c sube_sum -l dbload.log"

      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  sum1 sum"

----      RUN ejecuta



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
      det_mot_rechazo3   CHAR(03),
----  filler             CHAR(02),       ----jerry
      tipo_registroi     CHAR(02),
      ident_servicioi    CHAR(02),
      consec_reg_lotei   INTEGER,
      n_seguroi          CHAR(11),
      n_rfci             CHAR(13),
      n_unicoi           CHAR(18),
      nom_trabajadori    CHAR(50),
      periodo_pagoi      CHAR(06),
      fech_pagoi         CHAR(08),
      fech_valor_rcvi    CHAR(08),
      fech_valor_vivi    CHAR(08),
      ult_salario_diarioi DECIMAL(9,2),
      folio_pago_suai    CHAR(06),
      reg_patronal_imssi CHAR(11),
      rfc_patroni        CHAR(13),
      cve_ent_receptorai CHAR(03),
      dias_cotz_bimesti  CHAR(02),
      dias_incap_bimesti CHAR(02),
      dias_ausent_bimesti CHAR(02),
      impt_reti          DECIMAL(16,2),
      impt_act_rec_reti  DECIMAL(16,2),
      impt_ces_veji      DECIMAL(16,2),
      impt_act_r_ces_veji DECIMAL(16,2),
      impt_aport_voli    DECIMAL(16,2),
      impt_aport_compli  DECIMAL(16,2),  --c22-6
      impt_aport_pati    DECIMAL(16,2),   ----jerry ojo
      impt_cuota_soci    DECIMAL(16,2),
      impt_aport_esti    DECIMAL(16,2),
      impt_aport_espi    DECIMAL(16,2),
      impt_act_cuo_soci  DECIMAL(16,2),
      impt_act_aport_esti DECIMAL(16,2),
      impt_act_cuo_espi  DECIMAL(16,2),
      fecha_pago_gubi    CHAR(08), 
      ident_viv_garantiai CHAR(01),
      filler0i            CHAR(09),
      result_operacioni  CHAR(02),
      det_mot_rechazo1i  CHAR(03),
      det_mot_rechazo2i  CHAR(03),
      det_mot_rechazo3i  CHAR(03),
      filleri            CHAR(60)      ----jerry --c22-6
   END RECORD

   DEFINE reg_rech1 RECORD
      comiflu_ret        DECIMAL(16,2),
      comiflu_cv         DECIMAL(16,2),
      comiflu_est        DECIMAL(16,2),
      comiflu_esp        DECIMAL(16,2),
      comiflu_tot        DECIMAL(16,2),
      fecha_alt_afo      DATE,
      tipo_trabaja       CHAR(02)
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
          vtipo          SMALLINT

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
          ident_arch  --@
     INTO reg_cza.*
     FROM safre_tmp:dis_cza_aporte
    WHERE folio = vfolio

   LET tipo_archivo              = reg_cza.ident_arch  --@
   LET reg_cza.tipo_ent_origen   ="01"
   LET reg_cza.clave_ent_origen  =vcod_afore
   LET reg_cza.tipo_ent_destino  ="03"
   LET reg_cza.clave_ent_destino ="001"
   LET reg_cza.mod_recp_envio    ="02"
   LET reg_cza.result_operacion  =""
   LET reg_cza.motivo_rechazo1   =""
   LET reg_cza.motivo_rechazo2   =""
   LET reg_cza.motivo_rechazo3   =""

   DISPLAY "GENERANDO CZA..." #---AT 10,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/cza"
   START REPORT rep_cza TO vreporte
      OUTPUT TO REPORT rep_cza(reg_cza.*)
   FINISH REPORT rep_cza

   --------------- GENERA RESPUESTA RECHAZOS  ---------------------
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
      a.impt_ret,
      a.impt_act_rec_ret,
      a.impt_ces_vej,
      a.impt_act_r_ces_vej,
      a.impt_aport_vol,
      a.impt_aport_compl,  --c22-6
      a.impt_aport_pat,
      a.impt_cuota_soc,
      a.impt_aport_est,
      a.impt_aport_esp,
      a.impt_act_cuo_soc,
      a.impt_act_aport_est,
      a.impt_act_cuo_esp,
      a.fecha_pago_gub,
      a.ident_viv_garantia,
      a.part_viv,          ----jerry
      a.valor_part,        ----jerry
      a.filler1,           ----jerry
      a.result_operacion,
      a.det_mot_rechazo1,
      a.det_mot_rechazo2,
      a.det_mot_rechazo3,
----      a.filler,        ----jerry
      b.tipo_registro,
      b.ident_servicio,
      b.consec_reg_lote,
      b.n_seguro,
      b.n_rfc,
      b.n_unico,
      b.nom_trabajador,
      b.periodo_pago,
      b.fech_pago,
      b.fech_valor_rcv,
      b.fech_valor_viv,
      b.ult_salario_diario,
      b.folio_pago_sua,
      b.reg_patronal_imss,
      b.rfc_patron,
      b.cve_ent_receptora,
      b.dias_cotz_bimestre,
      b.dias_incap_bimest,
      b.dias_ausent_bimest,
      b.impt_int_ret,   
      b.impt_int_act_rr,
      b.impt_int_ces_vej,
      b.impt_int_act_rcv, 
      b.impt_int_aport_vol,
      b.impt_int_aport_com,     --c22-6
      b.impt_int_aport_pat,     ----jerry ojo
      b.impt_int_cuota_soc,
      b.impt_int_aport_est,
      b.impt_int_aport_esp,
      b.impt_int_act_cuo_s,
      b.impt_int_act_est, 
      b.impt_int_a_cuo_esp,
      b.fecha_pago_gub,
      b.ident_viv_garantia,
      b.filler0,
      b.result_operacion,
      b.det_mot_rechazo1,
      b.det_mot_rechazo2,
      b.det_mot_rechazo3,
      b.filler
    FROM safre_tmp:dis_det_aporte a,OUTER safre_tmp:dis_det_interes b
   WHERE a.folio = vfolio
---AND (a.result_operacion = "02" OR a.ident_viv_garantia = "1")
---  AND a.result_operacion = "02"  --- quitar cuando 43 bis
     AND a.folio = b.folio
     AND a.n_seguro = b.n_seguro 
     AND a.consec_reg_lote = b.consec_reg_lote 
     ORDER BY  a.consec_reg_lote,a.tipo_registro

   LET reg_rech.cve_ent_receptora = "001"
   LET reg_rech.result_operacion  = "00"
   LET reg_rech.det_mot_rechazo1  = "000"
   LET reg_rech.det_mot_rechazo2  = "000"
   LET reg_rech.det_mot_rechazo3  = "000"
----   LET reg_rech.filler            = "00"   ----jerry
   LET reg_rech.cve_ent_receptorai= "001"
   LET reg_rech.result_operacioni = "00"
   LET reg_rech.det_mot_rechazo1i = "000"
   LET reg_rech.det_mot_rechazo2i = "000"
   LET reg_rech.det_mot_rechazo3i = "000"
---   LET reg_rech.filleri           = "00"  --c22-6
   

   DISPLAY "GENERANDO RECH..." #---AT 12,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/rech"
   START REPORT rep_rech TO vreporte
      FOREACH cur_rech INTO reg_rech.*

         --AGREGAMOS LAYOUT SOLICITADO
         --retiros
         SELECT sum(monto_en_pesos) * -1
         INTO   vmto1
         FROM   dis_cuenta
         WHERE  folio = 89
         AND    subcuenta = 1
         AND    tipo_movimiento between 100 and 109
         AND    nss = reg_rech.n_seguro
         AND    folio_sua = reg_rech.folio_pago_sua    --v1 

         IF vmto1 IS NULL THEN LET vmto1 = 0 END IF

         --cesantia y vejes
         SELECT sum(monto_en_pesos) * -1
         INTO   vmto2
         FROM   dis_cuenta
         WHERE  folio = 89
         AND    subcuenta = 2
         AND    tipo_movimiento between 100 and 109
         AND    nss = reg_rech.n_seguro
         AND    folio_sua = reg_rech.folio_pago_sua    --v1

         IF vmto2 IS NULL THEN LET vmto2 = 0 END IF

         --estatutarias    
         SELECT sum(monto_en_pesos) * -1
         INTO   vmto3
         FROM   dis_cuenta
         WHERE  folio = 89
         AND    subcuenta = 6
         AND    tipo_movimiento between 100 and 109
         AND    nss = reg_rech.n_seguro
         AND    folio_sua = reg_rech.folio_pago_sua    --v1

         IF vmto3 IS NULL THEN LET vmto3 = 0 END IF

         --especiales      
         SELECT sum(monto_en_pesos) * -1
         INTO   vmto4
         FROM   dis_cuenta
         WHERE  folio = 89
         AND    subcuenta = 9
         AND    tipo_movimiento between 100 and 109
         AND    nss = reg_rech.n_seguro
         AND    folio_sua = reg_rech.folio_pago_sua    --v1

         IF vmto4 IS NULL THEN LET vmto4 = 0 END IF

         LET vmto_tot = vmto1 + vmto2 + vmto3 + vmto4

         IF vmto_tot IS NULL THEN LET vmto_tot = 0 END IF

         LET reg_rech1.comiflu_ret       = vmto1
         LET reg_rech1.comiflu_cv        = vmto2
         LET reg_rech1.comiflu_est       = vmto3
         LET reg_rech1.comiflu_esp       = vmto4
         LET reg_rech1.comiflu_tot       = vmto_tot

         SELECT fentcons,tipo_solicitud
         INTO   vfent,vtipo
         FROM   afi_mae_afiliado
         WHERE  n_seguro = reg_rech.n_seguro 
         
         IF  vtipo = 5 THEN
             --asignado
             LET reg_rech1.fecha_alt_afo     = vfent
             LET reg_rech1.tipo_trabaja      = "TA"
         ELSE 
          -- IF vtipo = 1 THEN
                --registrado
                LET reg_rech1.fecha_alt_afo     = vfent
                LET reg_rech1.tipo_trabaja      = "TR"
          -- END IF
         END IF
                

         OUTPUT TO REPORT rep_rech(reg_rech.*,reg_rech1.*)
      END FOREACH
   FINISH REPORT rep_rech

   --------------- GENERA RESPUESTA DEPOSITOS ---------------------

   DECLARE cur_dep CURSOR FOR
   SELECT
      tipo_registro,
      ident_servicio,
      ident_pago,
      importe,
      fech_liquidacion,
      impt_aport_acept,
      impt_aport_dev,
      part_viv_env,      ----jerry
      part_viv_acept,    ----jerry
      part_viv_dev,      ----jerry
      ident_siefore,     --c22-6
      tipo_siefore       --c22-6
    FROM safre_tmp:dis_dep_aporte
   WHERE folio = vfolio

   DISPLAY "GENERANDO DEP....." #------AT 14,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/dep"
   START REPORT rep_dep TO vreporte
      FOREACH cur_dep INTO reg_dep.*
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
          impt_total_rcv,
          impt_total_int_rcv,
 impt_tot_apo_vol,       --c22-6
 impt_tot_int_vol,       --c22-6
 impt_tot_apo_comp,      --c22-6
 impt_tot_int_comp,      --c22-6
          impt_total_patr,
          impt_tatal_int_pat,
          impt_total_guber,
          impt_total_int_gub,
          num_de_reg_aport,
          num_cuenta_xpagar,
          impt_total_xpagar,
          impt_tot_viv_gar,
          impt_tot_int_vgar,
          impt_tot_part_viv,   ----jerry
          tot_part_viv_gar,    ----jerry
          filler
   FROM   safre_tmp:dis_sum_aporte
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
         LET reg_sum.num_de_reg_aport       = "00000000"

         LET vimpt_total_rcv     = reg_sum.impt_total_rcv     / 100
         LET vimpt_total_int_rcv = reg_sum.impt_total_int_rcv / 100

 LET vimpt_tot_apo_vol  = reg_sum.impt_tot_apo_vol  / 100     --c22-6
 LET vimpt_tot_int_vol  = reg_sum.impt_tot_int_vol  / 100     --c22-6
 LET vimpt_tot_apo_comp = reg_sum.impt_tot_apo_comp / 100     --c22-6
 LET vimpt_tot_int_comp = reg_sum.impt_tot_int_comp / 100     --c22-6

         LET vimpt_total_patr    = reg_sum.impt_total_patr    / 100
         LET vimpt_total_int_pat = reg_sum.impt_tatal_int_pat / 100
         LET vimpt_total_guber   = reg_sum.impt_total_guber   / 100
         LET vimpt_total_int_gub = reg_sum.impt_total_int_gub / 100
         LET vimpt_total_xpagar  = reg_sum.impt_total_xpagar  / 100
         LET vimpt_tot_viv_gar   = reg_sum.impt_tot_viv_gar   / 100
         LET vimpt_tot_int_vgar  = reg_sum.impt_tot_int_vgar  / 100

    LET vimpt_tot_part_viv     = reg_sum.impt_tot_part_viv / 1000000 ----jerry
    LET vtot_part_viv_gar        = reg_sum.tot_part_viv_gar / 1000000 ----jerry

         LET reg_sum.impt_total_rcv     = vimpt_total_rcv
         LET reg_sum.impt_total_int_rcv = vimpt_total_int_rcv

 LET reg_sum.impt_tot_apo_vol  = vimpt_tot_apo_vol        --c22-6
 LET reg_sum.impt_tot_int_vol  = vimpt_tot_int_vol        --c22-6
 LET reg_sum.impt_tot_apo_comp = vimpt_tot_apo_comp       --c22-6
 LET reg_sum.impt_tot_int_comp = vimpt_tot_int_comp       --c22-6

         LET reg_sum.impt_total_patr    = vimpt_total_patr
         LET reg_sum.impt_tatal_int_pat = vimpt_total_int_pat
         LET reg_sum.impt_total_guber   = vimpt_total_guber
         LET reg_sum.impt_total_int_gub = vimpt_total_int_gub
         LET reg_sum.impt_total_xpagar  = vimpt_total_xpagar
         LET reg_sum.impt_tot_viv_gar   = vimpt_tot_viv_gar
         LET reg_sum.impt_tot_int_vgar  = vimpt_tot_int_vgar

         OUTPUT TO REPORT rep_sum(reg_sum.*,
                                  acept_rcv,
                               acept_vol,          --c22-6
                               acept_compl,        --c22-6
                                  acept_viv,
                                  acept_gub,
                                  acept_viv_gar,
                                  acept_part,       ----jerry
                                  acept_int_rcv,
                               acept_int_vol,      --c22-6
                               acept_int_compl,    --c22-6
                                  acept_int_viv,
                                  acept_int_gub,
                                  acept_int_vivg,
                                  acept_part_gar,   ----jerry
                                  recha_rcv,
                               recha_vol,          --c22-6
                               recha_compl,        --c22-6
                                  recha_int_rcv,
                                  recha_viv,
                                  recha_viv_gar,
                                  recha_part,       ----jerry
                                  recha_int_viv,
                               recha_int_vol,       --c22-6
                               recha_int_compl,     --c22-6
                                  recha_gub,
                                  recha_int_gub,
                                  recha_int_vivg,
                                  recha_part_gar)   ----jerry
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
      filler            CHAR(271), ----jerry
      ident_arch        CHAR(01)
   END RECORD

   OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
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
                        40 SPACES                 --c22-6

END REPORT

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
      det_mot_rechazo3   CHAR(03),
      tipo_registroi     CHAR(02),
      ident_servicioi    CHAR(02),
      consec_reg_lotei   INTEGER,
      n_seguroi          CHAR(11),
      n_rfci             CHAR(13),
      n_unicoi           CHAR(18),
      nom_trabajadori    CHAR(50),
      periodo_pagoi      CHAR(06),
      fech_pagoi         CHAR(08),
      fech_valor_rcvi    CHAR(08),
      fech_valor_vivi    CHAR(08),
      ult_salario_diarioi DECIMAL(9,2),
      folio_pago_suai    CHAR(06),
      reg_patronal_imssi CHAR(11),
      rfc_patroni        CHAR(13),
      cve_ent_receptorai CHAR(03),
      dias_cotz_bimesti  CHAR(02),
      dias_incap_bimesti CHAR(02),
      dias_ausent_bimesti CHAR(02),
      impt_reti          DECIMAL(16,2),
      impt_act_rec_reti  DECIMAL(16,2),
      impt_ces_veji      DECIMAL(16,2),
      impt_act_r_ces_veji DECIMAL(16,2),
      impt_aport_voli    DECIMAL(16,2),
      impt_aport_compli  DECIMAL(16,2),  --c22-6
      impt_aport_pati    DECIMAL(16,2),   ----jerry ojo
      impt_cuota_soci    DECIMAL(16,2),
      impt_aport_esti    DECIMAL(16,2),
      impt_aport_espi    DECIMAL(16,2),
      impt_act_cuo_soci  DECIMAL(16,2),
      impt_act_aport_esti DECIMAL(16,2),
      impt_act_cuo_espi  DECIMAL(16,2),
      fecha_pago_gubi    CHAR(08), 
      ident_viv_garantiai CHAR(01),
      filler0i           CHAR(09),  -- quitar cuando 43 bis
      result_operacioni  CHAR(02),
      det_mot_rechazo1i  CHAR(03),
      det_mot_rechazo2i  CHAR(03),
      det_mot_rechazo3i  CHAR(03),
      filleri            CHAR(60)    ----jerry  --c22-6
   END RECORD 

   DEFINE reg_rech2 RECORD
      comiflu_ret        DECIMAL(16,2),
      comiflu_cv         DECIMAL(16,2),
      comiflu_est        DECIMAL(16,2),
      comiflu_esp        DECIMAL(16,2),
      comiflu_tot        DECIMAL(16,2),
      fecha_alt_afo      DATE,
      tipo_trabaja       CHAR(02)
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

LET cult_salario_diarioi= reg_rech.ult_salario_diarioi / 100 USING '&&&&&.&&'
LET cimpt_reti          = reg_rech.impt_reti           / 100 USING '&&&&&.&&'
LET cimpt_act_rec_reti  = reg_rech.impt_act_rec_reti   / 100 USING '&&&&&.&&'
LET cimpt_ces_veji      = reg_rech.impt_ces_veji       / 100 USING '&&&&&.&&'
LET cimpt_act_r_ces_veji= reg_rech.impt_act_r_ces_veji / 100 USING '&&&&&.&&'
LET cimpt_aport_voli    = reg_rech.impt_aport_voli     / 100 USING '&&&&&.&&'

LET cimpt_aport_compli=reg_rech.impt_aport_compli/100 USING '&&&&&.&&' --c22-6

LET cimpt_aport_pati    = reg_rech.impt_aport_pati     / 100 USING '&&&&&.&&'
LET cimpt_cuota_soci    = reg_rech.impt_cuota_soci     / 100 USING '&&&&&.&&'
LET cimpt_aport_esti    = reg_rech.impt_aport_esti     / 100 USING '&&&&&.&&'
LET cimpt_aport_espi    = reg_rech.impt_aport_espi     / 100 USING '&&&&&.&&'
LET cimpt_act_cuo_soci  = reg_rech.impt_act_cuo_soci   / 100 USING '&&&&&.&&'
LET cimpt_act_aport_esti= reg_rech.impt_act_aport_esti / 100 USING '&&&&&.&&'
LET cimpt_act_cuo_espi  = reg_rech.impt_act_cuo_espi   / 100 USING '&&&&&.&&'

LET xconsec_reg_lotei   = reg_rech.consec_reg_lotei    USING '&&&&&&&&'
LET xult_salario_diarioi= cult_salario_diarioi[1,05], cult_salario_diarioi[7,08]
LET xdias_cotz_bimesti  = reg_rech.dias_cotz_bimesti   USING '&&'
LET xdias_incap_bimesti = reg_rech.dias_incap_bimesti  USING '&&'
LET xdias_ausent_bimesti= reg_rech.dias_ausent_bimesti USING '&&'
LET ximpt_reti          = cimpt_reti          [1,05],cimpt_reti          [07,08]
LET ximpt_act_rec_reti  = cimpt_act_rec_reti  [1,05],cimpt_act_rec_reti  [07,08]
LET ximpt_ces_veji      = cimpt_ces_veji      [1,05],cimpt_ces_veji      [07,08]
LET ximpt_act_r_ces_veji= cimpt_act_r_ces_veji[1,05],cimpt_act_r_ces_veji[07,08]
LET ximpt_aport_voli    = cimpt_aport_voli    [1,05],cimpt_aport_voli    [07,08]

LET ximpt_aport_compli=cimpt_aport_compli[1,05],cimpt_aport_compli[07,08]--c22-6

LET ximpt_aport_pati    = cimpt_aport_pati    [1,05],cimpt_aport_pati    [07,08]
LET ximpt_cuota_soci    = cimpt_cuota_soci    [1,05],cimpt_cuota_soci    [07,08]
LET ximpt_aport_esti    = cimpt_aport_esti    [1,05],cimpt_aport_esti    [07,08]
LET ximpt_aport_espi    = cimpt_aport_espi    [1,05],cimpt_aport_espi    [07,08]
LET ximpt_act_cuo_soci  = cimpt_act_cuo_soci  [1,05],cimpt_act_cuo_soci  [07,08]
LET ximpt_act_aport_esti= cimpt_act_aport_esti[1,05],cimpt_act_aport_esti[07,08]
LET ximpt_act_cuo_espi  = cimpt_act_cuo_espi  [1,05],cimpt_act_cuo_espi  [07,08]

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
                     reg_rech2.tipo_trabaja
END REPORT

REPORT rep_dep(reg_dep)
   DEFINE reg_dep RECORD
      tipo_registro    CHAR(02),
      ident_servicio   CHAR(02),
      ident_pago       CHAR(16),
      importe          DECIMAL(15,2),
      fech_liquidacion DATE,
      impt_aport_acept DECIMAL(15,2),
      impt_aport_dev   DECIMAL(15,2),
      part_viv_env     DECIMAL(22,6),  ----jerry
      part_viv_acept   DECIMAL(22,6),  ----jerry
      part_viv_dev     DECIMAL(22,6),  ----jerry
 ident_siefore    CHAR(08),       --c22-6
 tipo_siefore     SMALLINT        --c22-6
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
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT 
      ON EVERY ROW
         LET vnum_ctas_xpagar = vnum_ctas_xpagar + 1  --c22-6

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
            LET c15_impt_aport_dev = c16_impt_aport_dev[01,13] ,
                                     c16_impt_aport_dev[15,16]
         END IF

         LET xfecha_liquidacion = reg_dep.fech_liquidacion
         LET vfecha_liquidacion = xfecha_liquidacion[7,10],
                                  xfecha_liquidacion[1,02],
                                  xfecha_liquidacion[4,05]

 LET reg_dep.part_viv_env   = reg_dep.part_viv_env   * 1000000 ----jerry jerry
 LET reg_dep.part_viv_acept = reg_dep.part_viv_acept * 1000000 ----jerry jerry
 LET reg_dep.part_viv_dev   = reg_dep.part_viv_dev   * 1000000 ----jerry jerry

         PRINT COLUMN 01,reg_dep.tipo_registro,
                         reg_dep.ident_servicio,
                         reg_dep.ident_pago,
                         c15_importe,
                         vfecha_liquidacion,
                         c15_impt_aport_acept,
                         c15_impt_aport_dev,
                   reg_dep.part_viv_env   USING '&&&&&&&&&&&&&&&&&&', ----jerry
                   reg_dep.part_viv_acept USING '&&&&&&&&&&&&&&&&&&', ----jerry
                   reg_dep.part_viv_dev   USING '&&&&&&&&&&&&&&&&&&', ----jerry
              reg_dep.ident_siefore,                  --c22-6
              reg_dep.tipo_siefore    USING '&&&',    --c22-6
                         222 space                    --c22-6

END REPORT 

REPORT rep_sum(reg_sum,
               acept_rcv,
           acept_vol,          --c22-6
           acept_compl,        --c22-6
               acept_viv,
               acept_gub,
               acept_viv_gar,
               acept_part,       ----jerry
               acept_int_rcv,
           acept_int_vol,      --c22-6
           acept_int_compl,    --c22-6
               acept_int_viv,
               acept_int_gub,
               acept_int_vivg,
               acept_part_gar,   ----jerry
               recha_rcv,
           recha_vol,          --c22-6
           recha_compl,        --c22-6
               recha_int_rcv,
               recha_viv,
               recha_viv_gar,
               recha_part,       ----jerry
               recha_int_viv,
           recha_int_vol,       --c22-6
           recha_int_compl,     --c22-6
               recha_gub,
               recha_int_gub,
               recha_int_vivg,
               recha_part_gar)   ----jerry
   DEFINE
      acept_rcv     DECIMAL(16,2),
   acept_vol     DECIMAL(16,2),    --c22-6
   acept_compl   DECIMAL(16,2),    --c22-6
      acept_viv     DECIMAL(16,2),
      acept_gub     DECIMAL(16,2),
      acept_viv_gar DECIMAL(16,2),
      acept_part    DECIMAL(18,6),   ----jerry
      acept_int_rcv DECIMAL(16,2),
   acept_int_vol   DECIMAL(16,2),    --c22-6
   acept_int_compl DECIMAL(16,2),    --c22-6
      acept_int_viv DECIMAL(16,2),
      acept_int_gub DECIMAL(16,2),
      acept_int_vivg DECIMAL(16,2),
      acept_part_gar DECIMAL(18,6),  ----jerry
      recha_rcv     DECIMAL(16,2),
   recha_vol     DECIMAL(16,2),    --c22-6
   recha_compl   DECIMAL(16,2),    --c22-6
      recha_viv     DECIMAL(16,2),
      recha_viv_gar DECIMAL(16,2),
      recha_part    DECIMAL(18,6),   ----jerry
      recha_gub     DECIMAL(16,2),
      recha_int_rcv DECIMAL(16,2),
   recha_int_vol   DECIMAL(16,2),    --c22-6
   recha_int_compl DECIMAL(16,2),    --c22-6
      recha_int_viv DECIMAL(16,2),
      recha_int_gub DECIMAL(16,2),
      recha_int_vivg DECIMAL(16,2),
      recha_part_gar DECIMAL(18,6)   ----jerry


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
      filler             CHAR(88)         ----jerry  --c22-6
   END RECORD,

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

   d15_impt_total_rcv,
   d15_impt_total_int_rcv,
   d15_impt_aport_pat,
   d15_impt_total_int_pat,
   d15_impt_total_guber,
   d15_impt_total_int_gub,
   d15_impt_total_xpagar,
d15_impt_tot_viv_gar,
d15_impt_tot_int_vgar,
   d15_impt_total_xpagari,
   d15_impt_aport_acept,
   d15_impt_aport_dev,
   d13_impt_aport_acept DECIMAL(15,2)

   OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT
      ON EVERY ROW
         LET reg_sum.impt_tot_part_viv = recha_part      * 1000000 ----jerry
         LET reg_sum.tot_part_viv_gar  = recha_part_gar  * 1000000 ----jerry
         LET reg_sum.impt_tot_apo_vol  = recha_vol       * 100     --c22-6
         LET reg_sum.impt_tot_int_vol  = recha_int_vol   * 100     --c22-6
         LET reg_sum.impt_tot_apo_comp = recha_compl     * 100     --c22-6
         LET reg_sum.impt_tot_int_comp = recha_int_compl * 100     --c22-6

         LET d15_impt_total_rcv     = recha_rcv
         LET d15_impt_total_int_rcv = recha_int_rcv
         LET d15_impt_aport_pat     = recha_viv
         LET d15_impt_total_int_pat = recha_int_viv
         LET d15_impt_total_guber   = recha_gub
         LET d15_impt_total_int_gub = recha_int_gub
         LET d15_impt_total_xpagar  = acept_rcv     + acept_viv      + --c22-6
                                      acept_gub     + acept_int_rcv  + --c22-6
                                      acept_int_viv + acept_int_gub  + --c22-6
                                      acept_viv_gar + acept_int_vivg + --c22-6
                                      acept_vol     + acept_compl    + --c22-6
                                      acept_int_vol + acept_int_compl  --c22-6

      LET d15_impt_tot_viv_gar  = recha_viv_gar
      LET d15_impt_tot_int_vgar = recha_int_vivg

    LET vtotal_aport_rech = 0
    LET vtotal_int_rech   = 0

    IF d15_impt_total_rcv IS NULL  THEN
       LET  c15_impt_total_rcv = "000000000000000"
    ELSE
       LET c16_impt_total_rcv = d15_impt_total_rcv USING"&&&&&&&&&&&&&.&&"
       LET c15_impt_total_rcv = c16_impt_total_rcv[01,13],
                                c16_impt_total_rcv[15,16]
    END IF

    IF d15_impt_total_int_rcv IS NULL  THEN
       LET  c15_impt_total_int_rcv = "000000000000000"
    ELSE
    LET c16_impt_total_int_rcv = d15_impt_total_int_rcv USING"&&&&&&&&&&&&&.&&"
    LET c15_impt_total_int_rcv = c16_impt_total_int_rcv[01,13],
                                 c16_impt_total_int_rcv[15,16]
    END IF

    IF d15_impt_aport_pat IS NULL  THEN
       LET  c15_impt_aport_pat = "000000000000000"
    ELSE
     LET c16_impt_aport_pat     = d15_impt_aport_pat USING"&&&&&&&&&&&&&.&&"
     LET c15_impt_aport_pat     = c16_impt_aport_pat[1,13],
                                  c16_impt_aport_pat[15,16]
    END IF

    IF d15_impt_total_int_pat IS NULL THEN
        LET c15_impt_total_int_pat = "000000000000000"
    ELSE
     LET c16_impt_total_int_pat = d15_impt_total_int_pat USING"&&&&&&&&&&&&&.&&"
     LET c15_impt_total_int_pat = c16_impt_total_int_pat[1,13],
                                  c16_impt_total_int_pat[15,16]
    END IF

    IF d15_impt_tot_viv_gar IS NULL  THEN
       LET  c15_impt_tot_viv_gar = "000000000000000"
    ELSE
     LET c16_impt_tot_viv_gar     = d15_impt_tot_viv_gar USING"&&&&&&&&&&&&&.&&"
     LET c15_impt_tot_viv_gar     = c16_impt_tot_viv_gar[1,13],
                                    c16_impt_tot_viv_gar[15,16]
    END IF

    IF d15_impt_tot_int_vgar IS NULL THEN
        LET c15_impt_tot_int_vgar = "000000000000000"
    ELSE
     LET c16_impt_tot_int_vgar = d15_impt_tot_int_vgar USING"&&&&&&&&&&&&&.&&"
     LET c15_impt_tot_int_vgar = c16_impt_tot_int_vgar[1,13],
                                 c16_impt_tot_int_vgar[15,16]
    END IF

    IF d15_impt_total_guber IS NULL  THEN
       LET  c15_impt_total_guber = "000000000000000"
    ELSE
    LET c16_impt_total_guber = d15_impt_total_guber USING"&&&&&&&&&&&&&.&&"
    LET c15_impt_total_guber = c16_impt_total_guber[1,13],
                               c16_impt_total_guber[15,16]
    END IF

   IF d15_impt_total_int_gub IS NULL THEN
    LET c15_impt_total_int_gub = "000000000000000"
   ELSE
    LET c16_impt_total_int_gub = d15_impt_total_int_gub USING"&&&&&&&&&&&&&.&&"
    LET c15_impt_total_int_gub = c16_impt_total_int_gub[1,13],
                                 c16_impt_total_int_gub[15,16]
   END IF
    IF d15_impt_total_xpagari IS NULL  THEN
       LET  d15_impt_total_xpagari = 0
    END IF

    IF d15_impt_total_xpagar IS NULL  THEN
       LET  c15_impt_total_xpagar = "000000000000000"
    ELSE
 LET c16_impt_total_xpagar = d15_impt_total_xpagar USING"&&&&&&&&&&&&&.&&"
 LET c15_impt_total_xpagar = c16_impt_total_xpagar[1,13],
                             c16_impt_total_xpagar[15,16]
    END IF

    IF d15_impt_aport_acept IS NULL  THEN
       LET  c15_impt_aport_acept = "000000000000000"
    ELSE
 LET c16_impt_aport_acept    = d15_impt_aport_acept USING"&&&&&&&&&&&&&.&&"
 LET c15_impt_aport_acept    = c16_impt_aport_acept[1,13],
                               c16_impt_aport_acept[15,16]
    END IF


    DISPLAY 'TIPO ARCHIVO :', tipo_archivo   --@
    IF tipo_archivo = '2' THEN    ---- ACLARACIONES ESPECIALES
    ELSE
       LET c15_impt_total_int_rcv = '000000000000000' --penticion marisol correo
       LET c15_impt_total_int_pat = '000000000000000' --miercoles 30-03-2005
       LET c15_impt_total_int_gub = '000000000000000'
       LET c15_impt_tot_int_vgar  = '000000000000000'
    END IF
 

         PRINT COLUMN 1,
                     reg_sum.tipo_registro,
                     reg_sum.ident_servicio,
                     reg_sum.ident_operacion,
                     reg_sum.tipo_ent_origen,
                     vcod_afore              USING"&&&" ,
                     reg_sum.tipo_ent_destino,
                     reg_sum.clave_ent_destino,
                     reg_sum.fech_creac_lote,
                     reg_sum.lote_del_dia,
                     c15_impt_total_rcv,
                     c15_impt_total_int_rcv,
          reg_sum.impt_tot_apo_vol   USING '&&&&&&&&&&&&&&&',  --c22-6
          reg_sum.impt_tot_int_vol   USING '&&&&&&&&&&&&&&&',  --c22-6
          reg_sum.impt_tot_apo_comp  USING '&&&&&&&&&&&&&&&',  --c22-6
          reg_sum.impt_tot_int_comp  USING '&&&&&&&&&&&&&&&',  --c22-6
                     c15_impt_aport_pat,
                     c15_impt_total_int_pat,
                     c15_impt_total_guber,
                     c15_impt_total_int_gub,
                     vnum_reg                 USING "&&&&&&&&",
------                     reg_sum.num_de_reg_aport,
                     vnum_ctas_xpagar    USING '&&&&&&',   --c22-6.5
-----                     reg_sum.num_cuenta_xpagar,                  --c22-6.5
                     c15_impt_total_xpagar,
                     c15_impt_tot_viv_gar,
                     c15_impt_tot_int_vgar,
          reg_sum.impt_tot_part_viv USING '&&&&&&&&&&&&&&&&&&', ----jerry
          reg_sum.tot_part_viv_gar  USING '&&&&&&&&&&&&&&&&&&', ----jerry
                     reg_sum.filler
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
