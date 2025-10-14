###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#                  => E.F.P.                                                  #
#Programa          => DISBHIS                                                 #
#Descripcion       => GENERA HISTORICOS (histpagos_cza,dis_det_aporte,        #
#                  => dis_det_interes y histpagos_sum),                       # 
#Sistema           => DIS.                                                    #
#Fecha Inicio      => 31 mayo 2000.                                           #
#Fecha Termino     => 18 junio 2000.                                          #
#By                => GERARDO ALFONSO VEGA PAREDES                            #
#Fecha ult. modif. =>                                                         #
#By                =>                                                         #
#                  => Busqueda por consecutivo en archivos de aclaraciones esp#
###############################################################################

DATABASE safre_af

GLOBALS

   DEFINE g_bat RECORD LIKE dis_ctrl_proceso.*

   DEFINE vrecauda  CHAR(001)

   DEFINE vhora_max     CHAR(08),
          vhora_final   CHAR(08),
          vresultado    CHAR(50),
          vetapa_cod    SMALLINT,
          vproc         CHAR(06),
          vrech         CHAR(06),
          hoy           DATE,
          vsalida       CHAR(01),
          nom_archivo   CHAR(12),
          gparam_dis    RECORD LIKE dis_parametro.*,
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
          cla_sel       CHAR(450)

   DEFINE tot_acep RECORD
      impt_ret           decimal(15,2),
      impt_act_rec_ret   decimal(15,2),
      impt_ces_vej       decimal(15,2),
      impt_act_r_ces_vej decimal(15,2),
      impt_aport_vol     decimal(15,2),
      impt_aport_pat     decimal(15,2),
      impt_cuota_soc     decimal(15,2),
      impt_aport_est     decimal(15,2),
      impt_aport_esp     decimal(15,2),
      impt_act_cuo_soc   decimal(15,2),
      impt_act_aport_est decimal(15,2),
      impt_act_cuo_esp   decimal(15,2)
   END RECORD

   DEFINE tot_acepint RECORD
      impt_ret           decimal(15,2),
      impt_act_rec_ret   decimal(15,2),
      impt_ces_vej       decimal(15,2),
      impt_act_r_ces_vej decimal(15,2),
      impt_aport_vol     decimal(15,2),
      impt_aport_pat     decimal(15,2),
      impt_cuota_soc     decimal(15,2),
      impt_aport_est     decimal(15,2),
      impt_aport_esp     decimal(15,2),
      impt_act_cuo_soc   decimal(15,2),
      impt_act_aport_est decimal(15,2),
      impt_act_cuo_esp   decimal(15,2)
   END RECORD

   DEFINE tot_comi RECORD
      impt_ret           decimal(15,6),
      impt_ces_vej       decimal(15,6),
      impt_aport_est     decimal(15,6),
      impt_aport_esp     decimal(15,6) 
   END RECORD
      
   DEFINE tot_rech RECORD
      impt_ret           decimal(15,2),
      impt_act_rec_ret   decimal(15,2),
      impt_ces_vej       decimal(15,2),
      impt_act_r_ces_vej decimal(15,2),
      impt_aport_vol     decimal(15,2),
      impt_aport_pat     decimal(15,2),
      impt_cuota_soc     decimal(15,2),
      impt_aport_est     decimal(15,2),
      impt_aport_esp     decimal(15,2),
      impt_act_cuo_soc   decimal(15,2),
      impt_act_aport_est decimal(15,2),
      impt_act_cuo_esp   decimal(15,2)
   END RECORD
        
   DEFINE tot_rechint RECORD
      impt_ret           decimal(15,2),
      impt_act_rec_ret   decimal(15,2),
      impt_ces_vej       decimal(15,2),
      impt_act_r_ces_vej decimal(15,2),
      impt_aport_vol     decimal(15,2),
      impt_aport_pat     decimal(15,2),
      impt_cuota_soc     decimal(15,2),
      impt_aport_est     decimal(15,2),
      impt_aport_esp     decimal(15,2),
      impt_act_cuo_soc   decimal(15,2),
      impt_act_aport_est decimal(15,2),
      impt_act_cuo_esp   decimal(15,2)
   END RECORD

   DEFINE reg_ident RECORD 
      pid         INTEGER,
      proceso_cod INTEGER,
      opera_cod   INTEGER,
      nom_archivo CHAR(20),
      folio       SMALLINT
   END RECORD

   DEFINE prioridad CHAR(25),
          vfolio    INTEGER

   DEFINE vfecha_aux DATE,
          mes        SMALLINT,
          dia        SMALLINT,
          ano        SMALLINT
   
END GLOBALS

MAIN
   DEFINE nom_archivo CHAR(20)

   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   CALL STARTLOG("DISBHIS2.log")     

   PROMPT "reg_ident.nom_archivo " for reg_ident.nom_archivo 
   PROMPT "vfolio                " for vfolio       

   LET hoy = TODAY

   CALL Proceso_principal() 

END MAIN

FUNCTION Proceso_principal()
   DEFINE ejecuta CHAR(200)

   CALL Inicializa()

   ERROR "PROCESANDO INFORMACION...",reg_ident.nom_archivo 
   
   CALL Separa_archivo() 
   
   CALL Sube_datos() 

END FUNCTION

FUNCTION Inicializa()
   LET hoy = TODAY

   SELECT *,
          USER
   INTO   gparam_dis.*,
          gusuario
   FROM   dis_parametro

   SELECT codigo_afore
   INTO   vcod_afore
   FROM   tab_afore_local

   LET vnom_archivo = reg_ident.nom_archivo[1,7] CLIPPED,".CONFAF" CLIPPED

   LET tot_acep.impt_ret           = 0
   LET tot_acep.impt_act_rec_ret   = 0
   LET tot_acep.impt_ces_vej       = 0
   LET tot_acep.impt_act_r_ces_vej = 0
   LET tot_acep.impt_aport_vol     = 0
   LET tot_acep.impt_aport_pat     = 0
   LET tot_acep.impt_cuota_soc     = 0
   LET tot_acep.impt_aport_est     = 0
   LET tot_acep.impt_aport_esp     = 0
   LET tot_acep.impt_act_cuo_soc   = 0
   LET tot_acep.impt_act_aport_est = 0
   LET tot_acep.impt_act_cuo_esp   = 0

   LET tot_acepint.impt_ret           = 0
   LET tot_acepint.impt_act_rec_ret   = 0
   LET tot_acepint.impt_ces_vej       = 0
   LET tot_acepint.impt_act_r_ces_vej = 0
   LET tot_acepint.impt_aport_vol     = 0
   LET tot_acepint.impt_aport_pat     = 0
   LET tot_acepint.impt_cuota_soc     = 0
   LET tot_acepint.impt_aport_est     = 0
   LET tot_acepint.impt_aport_esp     = 0
   LET tot_acepint.impt_act_cuo_soc   = 0
   LET tot_acepint.impt_act_aport_est = 0
   LET tot_acepint.impt_act_cuo_esp   = 0

   LET tot_comi.impt_ret           = 0
   LET tot_comi.impt_ces_vej       = 0
   LET tot_comi.impt_aport_est     = 0
   LET tot_comi.impt_aport_esp     = 0
      
   LET tot_rech.impt_ret           = 0
   LET tot_rech.impt_act_rec_ret   = 0
   LET tot_rech.impt_ces_vej       = 0
   LET tot_rech.impt_act_r_ces_vej = 0
   LET tot_rech.impt_aport_vol     = 0
   LET tot_rech.impt_aport_pat     = 0
   LET tot_rech.impt_cuota_soc     = 0
   LET tot_rech.impt_aport_est     = 0
   LET tot_rech.impt_aport_esp     = 0
   LET tot_rech.impt_act_cuo_soc   = 0
   LET tot_rech.impt_act_aport_est = 0
   LET tot_rech.impt_act_cuo_esp   = 0

   LET tot_rechint.impt_ret           = 0
   LET tot_rechint.impt_act_rec_ret   = 0
   LET tot_rechint.impt_ces_vej       = 0
   LET tot_rechint.impt_act_r_ces_vej = 0
   LET tot_rechint.impt_aport_vol     = 0
   LET tot_rechint.impt_aport_pat     = 0
   LET tot_rechint.impt_cuota_soc     = 0
   LET tot_rechint.impt_aport_est     = 0
   LET tot_rechint.impt_aport_esp     = 0
   LET tot_rechint.impt_act_cuo_soc   = 0
   LET tot_rechint.impt_act_aport_est = 0
   LET tot_rechint.impt_act_cuo_esp   = 0

   LET vcont_acep = 0
   LET vcont_rech = 0
END FUNCTION


FUNCTION Separa_archivo()  -- ETAPA 3 
   DEFINE
      ejecuta CHAR(200)

   LET ejecuta = "sed -e '/^09/!d' ",gparam_dis.ruta_rescate CLIPPED,"/",
       reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/sum"
   RUN ejecuta
                 
END FUNCTION

FUNCTION Sube_datos()  -- ETAPA 4 
   DEFINE
      ejecuta          CHAR(200),
      vlote            CHAR(03),
      cla_ins          CHAR(200),
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
                 "; cut -c 17-24 sum > fecha_lote"
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                 "; cut -c 25-27 sum > lote"
   RUN ejecuta
   
   WHENEVER ERROR CONTINUE
      DROP TABLE fecha_lote
      DROP TABLE lote
   WHENEVER ERROR STOP

   CREATE TEMP TABLE fecha_lote
      (fecha_lote CHAR(08))
      
   CREATE TEMP TABLE lote
      (lote CHAR(03))

   LET ejecuta = gparam_dis.ruta_rescate CLIPPED,"/fecha_lote" CLIPPED
   LOAD FROM ejecuta INSERT INTO fecha_lote

   LET ejecuta = gparam_dis.ruta_rescate CLIPPED,"/lote" CLIPPED
   LOAD FROM ejecuta INSERT INTO lote

   SELECT fecha_lote
     INTO vfecha_lote
     FROM fecha_lote
   
   SELECT lote
     INTO vlote
     FROM lote

   SELECT "X"
     FROM dis_cza_aporte 
    WHERE fech_creac_lote = vfecha_lote
      AND lote_del_dia    = vlote

   LET STATUS = 0


      LET mes = vfecha_lote[5,6]
      LET dia = vfecha_lote[7,8]
      LET ano = vfecha_lote[1,4]

      LET vfecha_aux = mdy(mes,dia,ano)

      LET vfecha_recepcion = vfecha_aux
      LET vhora_recepcion  = TIME
      LET vestado          = 2
      LET vfecha_estado    = vfecha_aux
      LET vhora_estado     = TIME

      --------------------   GENERA dis_cza_aporte   --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_cza"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"   ---- Cabeza
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      --------------------   GENERA dis_sum_aporte   --------------------

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza sum > sum1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' sum1 > sum "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_af -c sube_sum -l dbload.log"

      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  sum1 sum"

      RUN ejecuta





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

