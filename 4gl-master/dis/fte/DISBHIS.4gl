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

   CALL STARTLOG("DISBHIS.log")     

   LET reg_ident.pid         = ARG_VAL(1)
   LET reg_ident.proceso_cod = ARG_VAL(2)
   LET reg_ident.opera_cod   = ARG_VAL(3)
   LET reg_ident.nom_archivo = ARG_VAL(4)
   LET reg_ident.folio       = ARG_VAL(5)

   LET vfolio = reg_ident.folio

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(hora_inicial) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  proceso_cod = 'DISBHIS' ",
                 " AND    etapa_cod = 1",
                 " AND    folio = ",reg_ident.folio CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vhora_max
   CLOSE cur_proceso9

   SELECT *
   INTO   g_bat.*
   FROM   dis_ctrl_proceso
   WHERE  proceso_cod  = "DISBHIS"
   AND    etapa_cod    = 1
   AND    hora_inicial = vhora_max
   AND    folio        = reg_ident.folio

   IF reg_ident.nom_archivo IS NOT NULL THEN

      CALL Proceso_principal() 

      LET vhora_final = TIME

      UPDATE dis_ctrl_proceso
      SET    hora_final    = vhora_final,
             resultado     = "HISTORICO TERMINADO"
      WHERE  proceso_cod   = "DISBHIS"
      AND    etapa_cod     = 1                        -- ETAPA 1
      AND    hora_inicial  = vhora_max
      AND    folio         = reg_ident.folio
   END IF

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

   IF STATUS = NOTFOUND THEN

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

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza cza > cza1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' cza1 > cza "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            "/;dbload -d safre_af -c sube_cza -l dbload.log"
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
----------
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
                    "; cut -c 1-73 dep > dep2; mv dep2 dep "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste dep01_3 dep dep02_3 > dep1;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' dep1 > dep "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
"/;DBDATE=Y4MD0;export DBDATE;dbload -d safre_af -c sube_dep -l dbload.log"
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
            "/;dbload -d safre_af -c sube_sum -l dbload.log"

      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      "; rm  sum1 sum"

      RUN ejecuta

------ ACTIVA OPTCOMPIND PARA QUE TODOS LOS QUERYES ENTREN POR INDICE -----

      LET ejecuta = "cd ",gparam_dis.ruta_ejecuta CLIPPED,
                    "/;OPTCOMPIND=0;export OPTCOMPIND;"
      RUN ejecuta

------ ACTIVA OPTCOMPIND DE DEFAULT ---------------
      LET ejecuta = "cd ",gparam_dis.ruta_ejecuta CLIPPED,
                    "/;OPTCOMPIND=2;export OPTCOMPIND;"
      RUN ejecuta

   ELSE
      ERROR "ESTE ARCHIVO YA HA SIDO PROCESADO"
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

