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

   CALL STARTLOG("DISBHIS3.log")     


---   PROMPT "reg_ident.nom_archivo " for reg_ident.nom_archivo 
---   PROMPT "vfolio                " for vfolio       

   LET vfolio = 97

   LET hoy = TODAY

   CALL Proceso_principal() 

END MAIN

FUNCTION Proceso_principal()
   DEFINE ejecuta CHAR(200)
 
   DELETE 
   FROM   dis_det_aporte
   WHERE  folio = 97

   DELETE 
   FROM   dis_det_interes
   WHERE  folio = 97

   CALL Inicializa()

   ERROR "PROCESANDO INFORMACION...",reg_ident.nom_archivo 
   
----   CALL Separa_archivo() 
   
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

   LET STATUS = 0

      LET vfecha_lote = "19980611"  -- fecha enczabezado


      LET mes = vfecha_lote[5,6]
      LET dia = vfecha_lote[7,8]
      LET ano = vfecha_lote[1,4]

      LET vfecha_aux = mdy(mes,dia,ano)

      LET vfecha_recepcion = vfecha_aux
      LET vhora_recepcion  = "18:23:13"
      LET vestado          = 2
      LET vfecha_estado    = vfecha_aux
      LET vhora_estado     = "18:23:13"

      --------------------   GENERA dis_cza_aporte   --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_cza"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"         ---- Cabeza
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
                    "; cut -c 1-283 det > det2" 
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 285-295 det > det3"
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
                    "/;dbload -d safre_af -c sube_det -l dbload.log;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det"
----      RUN ejecuta

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
                    "; cut -c 1-283 int > int2" 
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 285-295 int > int3"
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
                    "/;dbload -d safre_af -c sube_int -l dbload.log;"
      RUN ejecuta

      -- Se borran todo los archivo auxiliares
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  archivos fecha_lote lote "

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

