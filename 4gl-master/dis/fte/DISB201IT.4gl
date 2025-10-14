###############################################################################
#Proyecto          => SAFRE                                                   #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => DIS                                                     #
#Programa          => DISB201IT                                               #
#Descripcion       => GENERA HISTORICOS (dis_cza_iti, dis_det_iti, dis_dep_iti#
#                  => dis_sum_iti)                                            #
#Fecha Inicio      => 31 mayo 2000.                                           #
#Fecha Termino     => 18 junio 2000.                                          #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
-------------------------------------------------------------------------------
#Fecha modif       => 8 junio 2001.                                           #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
#Descripcion       => Busqueda por consecutivo en archivos de aclaraciones esp#
-------------------------------------------------------------------------------
#Fecha modif       => 02 septiembre 2003                                      #
#Autor             => ALEJANDRO RAMIREZ                                       #
#Descripcion       => Inhibir la generacion de registros 03 de aclaraciones   #
#                  => ordinarias y agregado de campo en layout                #
-------------------------------------------------------------------------------
#Fecha ini modif   => 19 noviembre 2004                                       #
#Fecha fni modif   => 28 noviembre 2004                                       #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
#Descripcion       => Adecuacion layout (031001) (031002) (031003) (031008)   #
#                     (031009) de acuerdo a circular 22-6 multi siefore,      #
#                     utilizacion de preparados para utilizar multiseifore    #
#                     para dispersion y comision                              #
-------------------------------------------------------------------------------
#Fecha modif       => 09 Febrero  2005                                        #
#Autor             => ALEJANDRO RAMIREZ                                       #
#Descripcion       => Agregar la opcion de recurrencia                        #
-------------------------------------------------------------------------------
#Fecha modif       => 10 Agosto   2005                                        #
#Autor             => ALEJANDRO RAMIREZ                                       #
#Descripcion       => (c7.6) Comisiones sobre la 1ra afiliacion               #
-------------------------------------------------------------------------------
#Fecha modif       => 23 Diciembre 2005                                       #
#Autor             => ALEJANDRO RAMIREZ                                       #
#Descripcion       => (c22-11) Cambios de nueva circular.                     #
-------------------------------------------------------------------------------
#Fecha modif       => 25 Agosto 2006                                          #
#Autor             => ALEJANDRO RAMIREZ                                       #
#Descripcion       => (c22-117) Correccion para vivi rema/ext y desfase en    #
#                  => al ejecutar dispersion especial                         #
-------------------------------------------------------------------------------
#Fecha modif       => Marzo 2007                                              #
#Autor             => DMR                                                     #
#Descripcion       => Se agrego la funcion cal_dias, para calcular el numero  #
#                     de dias para el cobro de comision, ya que habia una dif.#
#                     cuando el dia y mes del proceso era igual a la fentcons #
#                     o a la fecha de 1ra afiliacion                          #
#Fecha modif       => Febrero 2012                                            #
#Autor             => DMR                                                     #
#Descripcion       => Se creo tabla dis_prov_iti, para agilizar la generacion #
#                     del archivo de respuesta de los bimestrales,se quito la #
#                     programacion de comision respaldo tamaño 281928         #
#Modi.             => DMR 15 Febrero 2013                                     #
#Descr.            => Estandarizacion de version afores, SQLCA.SQLCODE y Liq  #
#                  => sin prioridad en prog liq, se aplica en las rutinas de  #
#                  => liq. liq_apor_est_esp y liq_apor_rcv_esp                #
#Modi.             => DMR 21 Mayo  2013                                       #
#Descr.            => Se modifico modulo completo para considerar cuenta por  #
#                  => cobrar con valor 05 tipo_deposito 031008 NO NOTIFICADO  #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      vrecauda              CHAR(001)

   DEFINE
      g_bat RECORD          LIKE dis_ctrl_proceso.*,
      vhora_max             CHAR(08),
      vhora_final           CHAR(08),
      vresultado            CHAR(50),
      vetapa_cod            SMALLINT,
      vproc                 CHAR(06),
      vrech                 CHAR(06),
      hoy                   DATE,
      vsalida               CHAR(01),
      nom_archivo           CHAR(12),
      gparam_dis RECORD     LIKE seg_modulo.*,
      gusuario              CHAR(08),
      comando               CHAR(200),
      vcod_afore            LIKE tab_afore_local.codigo_afore,
      vnom_archivo          CHAR(21),
      vreporte              CHAR(200),
      opc                   CHAR(01),
      vcont_rech            INTEGER,
      vcont_acep            INTEGER,
      vcobra                SMALLINT,
      vfecha_lote           CHAR(08),
      vtipo_reporte         CHAR(01),
      vaportes              CHAR(01),
      hora_inicial          CHAR(08),
      hora_final            CHAR(08),
      cla_sel               CHAR(1000),
      cla_apo               CHAR(500), --c22-6
      cla_com               CHAR(500), --c22-6
      cla_upd               CHAR(1500),

      tot_acep RECORD
         impt_ret           DECIMAL(15,2),
         impt_act_rec_ret   DECIMAL(15,2),
         impt_ces_vej       DECIMAL(15,2),
         impt_act_r_ces_vej DECIMAL(15,2),
         impt_aport_vol     DECIMAL(15,2),
         impt_aport_compl   DECIMAL(15,2),  --c22-6
         impt_aport_pat     DECIMAL(15,2),
         impt_cuota_soc     DECIMAL(15,2),
         impt_aport_est     DECIMAL(15,2),
         impt_aport_esp     DECIMAL(15,2),
         impt_act_cuo_soc   DECIMAL(15,2),
         impt_act_aport_est DECIMAL(15,2),
         impt_act_cuo_esp   DECIMAL(15,2),
         impt_viv_gar       DECIMAL(15,2),
         part_viv           DECIMAL(18,6),  ----jerry
         impt_rem_viv       DECIMAL(15,2),  --c22-11
         impt_rem_gar       DECIMAL(15,2),  --c22-11
         apli_rem           DECIMAL(18,6),  --c22-11
         apli_rem_gar       DECIMAL(18,6),  --c22-11
         inte_ext_viv       DECIMAL(15,2),  --c22-11
         inte_ext_gar       DECIMAL(15,2),  --c22-11
         apli_ext           DECIMAL(18,6),  --c22-11
         apli_ext_gar       DECIMAL(18,6),  --c22-11
         impt_aport_lplazo  DECIMAL(15,2),  --c22-11
         impt_aport_subadi  DECIMAL(15,2),  --c22-11
         part_viv_gar       DECIMAL(18,6)   ----jerry
      END RECORD,

      tot_acepint RECORD
         impt_ret           DECIMAL(15,2),
         impt_act_rec_ret   DECIMAL(15,2),
         impt_ces_vej       DECIMAL(15,2),
         impt_act_r_ces_vej DECIMAL(15,2),
         impt_aport_vol     DECIMAL(15,2),
         impt_aport_compl   DECIMAL(15,2),  --c22-6
         impt_aport_pat     DECIMAL(15,2),
         impt_cuota_soc     DECIMAL(15,2),
         impt_aport_est     DECIMAL(15,2),
         impt_aport_esp     DECIMAL(15,2),
         impt_act_cuo_soc   DECIMAL(15,2),
         impt_act_aport_est DECIMAL(15,2),
         impt_act_cuo_esp   DECIMAL(15,2),
         impt_viv_gar       DECIMAL(15,2),
         inte_ext_viv       DECIMAL(15,2),  --c22-11
         inte_ext_gar       DECIMAL(15,2),  --c22-11
         impt_aport_lplazo  DECIMAL(15,2),  --c22-11
         impt_aport_subadi  DECIMAL(15,2)   --c22-11
      END RECORD,  

      tot_comi RECORD
         impt_ret           DECIMAL(15,6),
         impt_ces_vej       DECIMAL(15,6),
         impt_aport_est     DECIMAL(15,6),
         impt_aport_esp     DECIMAL(15,6) 
      END RECORD,
      
      tot_rech RECORD
         impt_ret           DECIMAL(15,2),
         impt_act_rec_ret   DECIMAL(15,2),
         impt_ces_vej       DECIMAL(15,2),
         impt_act_r_ces_vej DECIMAL(15,2),
         impt_aport_vol     DECIMAL(15,2),
         impt_aport_compl   DECIMAL(15,2), --c22-6
         impt_aport_pat     DECIMAL(15,2),
         impt_cuota_soc     DECIMAL(15,2),
         impt_aport_est     DECIMAL(15,2),
         impt_aport_esp     DECIMAL(15,2),
         impt_act_cuo_soc   DECIMAL(15,2),
         impt_act_aport_est DECIMAL(15,2),
         impt_act_cuo_esp   DECIMAL(15,2),
         impt_viv_gar       DECIMAL(15,2),
         part_viv           DECIMAL(18,6),  ----jerry
         impt_rem_viv       DECIMAL(15,2),  --c22-11
         impt_rem_gar       DECIMAL(15,2),  --c22-11 
         apli_rem           DECIMAL(18,6),  --c22-11
         apli_rem_gar       DECIMAL(18,6),  --c22-11
         inte_ext_viv       DECIMAL(15,2),  --c22-11
         inte_ext_gar       DECIMAL(15,2),  --c22-11
         apli_ext           DECIMAL(18,6),  --c22-11
         apli_ext_gar       DECIMAL(18,6),  --c22-11
         impt_aport_lplazo  DECIMAL(15,2),  --c22-11
         impt_aport_subadi  DECIMAL(15,2),  --c22-11
         part_viv_gar       DECIMAL(18,6)   ----jerry
      END RECORD,
        
      tot_rechint RECORD
         impt_ret           DECIMAL(15,2),
         impt_act_rec_ret   DECIMAL(15,2),
         impt_ces_vej       DECIMAL(15,2),
         impt_act_r_ces_vej DECIMAL(15,2),
         impt_aport_vol     DECIMAL(15,2),
         impt_aport_compl   DECIMAL(15,2), --c22-6
         impt_aport_pat     DECIMAL(15,2),
         impt_cuota_soc     DECIMAL(15,2),
         impt_aport_est     DECIMAL(15,2),
         impt_aport_esp     DECIMAL(15,2),
         impt_act_cuo_soc   DECIMAL(15,2),
         impt_act_aport_est DECIMAL(15,2),
         impt_act_cuo_esp   DECIMAL(15,2),
         impt_viv_gar       DECIMAL(15,2),
         inte_ext_viv       DECIMAL(15,2), --c22-11
         inte_ext_gar       DECIMAL(15,2), --c22-11
         impt_aport_lplazo  DECIMAL(15,2), --c22-11
         impt_aport_subadi  DECIMAL(15,2)  --c22-11
      END RECORD

   DEFINE reg_ident RECORD 
          pid               ,
          proceso_cod       ,
          opera_cod         INTEGER,
          nom_archivo       CHAR(20),
          tipo              SMALLINT,
          fecha_recepcion   DATE,
          folio             INTEGER
   END RECORD

   DEFINE prioridad         CHAR(25)

   DEFINE g_com RECORD
      subcuenta             SMALLINT,
      val_porcentaje        DECIMAL(16,6)
   END RECORD

   DEFINE gfecha_recepcion  DATE

   DEFINE vconsec           INTEGER,
          vnssx             CHAR(11),
          vrow              INTEGER

   DEFINE graba             SMALLINT

   DEFINE vfecha_aux        DATE,
	  vperiodo          CHAR(06),
	  vbim_cert         INTEGER,
	  vbim_pago         INTEGER,
	  vmto_ret          DECIMAL(16,6),
	  vmto_ces          DECIMAL(16,6),
	  vcomret           DECIMAL(16,6),
	  vcomces           DECIMAL(16,6)

   DEFINE vnum_reg          INTEGER,
          vfolio            INTEGER

   DEFINE tipo_archivo      CHAR(01)

   DEFINE vid_sie           CHAR(08)  --c22-6

   DEFINE vnum_ctas_xpagar  SMALLINT  --c22-6
   DEFINE longi             INTEGER   --c22-11
   DEFINE new_cadena        CHAR(10)  --c22-11
END GLOBALS


MAIN
   DEFINE 
      nom_archivo  CHAR(20)

   DISPLAY " "
   DISPLAY ".1"

   LET vbim_cert = 0
   LET vbim_pago = 0
   LET vmto_ret  = 0
   LET vmto_ces  = 0
   LET vcomret   = 0
   LET vcomces   = 0

   CALL STARTLOG("DISB201IT.log")     
  
   LET reg_ident.pid             = ARG_VAL(1)
   LET reg_ident.proceso_cod     = ARG_VAL(2)
   LET reg_ident.opera_cod       = ARG_VAL(3)
   LET reg_ident.nom_archivo     = ARG_VAL(4)
   LET reg_ident.tipo            = ARG_VAL(5)
   LET reg_ident.fecha_recepcion = ARG_VAL(6)
   LET reg_ident.folio           = ARG_VAL(7)

   IF reg_ident.pid <> 0 THEN
      CALL Inserta_proceso()
   END IF

   DISPLAY "PID                   : ",reg_ident.pid
   DISPLAY "Proceso_cod           : ",reg_ident.proceso_cod
   DISPLAY "Opera_cod             : ",reg_ident.opera_cod
   DISPLAY "Archivo               : ",reg_ident.nom_archivo
   DISPLAY "Tipo dispersion       : ",reg_ident.tipo
   DISPLAY "Fecha recepcion       : ",reg_ident.fecha_recepcion
   DISPLAY "Folio                 : ",reg_ident.folio

   LET hoy = TODAY
   LET vconsec = 0

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM  dis_ctrl_proceso ",
                 "WHERE proceso_cod = 'DISB201IT' ",
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
   WHERE proceso_cod = "DISB201IT"
   AND   etapa_cod   = 1
   AND   consecutivo = vrow

   IF reg_ident.nom_archivo IS NOT NULL THEN

      LET vfolio = reg_ident.folio

      CALL Proceso_principal()

      LET vproc = vcont_acep
     
      SELECT COUNT(*)
      INTO   vcont_rech
      FROM   dis_det_iti
      WHERE  folio = vfolio
      AND    result_operacion = "02"

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
                    "WHERE proceso_cod = 'DISB201IT' ",
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
            " WHERE dis_ctrl_proceso.proceso_cod = 'DISB201IT' ",
            " AND   dis_ctrl_proceso.etapa_cod   = 1 ",        
            " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

      PREPARE claexe11 FROM cla_upd
      EXECUTE claexe11

      LET cla_sel = "SELECT MAX(consecutivo) ",
                    "FROM dis_ctrl_proceso ",
                    "WHERE proceso_cod = 'DISB201IT' ",
                    "AND etapa_cod = 3 " CLIPPED

      PREPARE claexe91 FROM cla_sel
      DECLARE cur_proceso91 CURSOR FOR claexe91
      OPEN cur_proceso91
         FETCH cur_proceso91 INTO vrow
      CLOSE cur_proceso91

      LET cla_upd = "UPDATE dis_ctrl_proceso ",
            " SET   dis_ctrl_proceso.folio       = ",vfolio,
            " WHERE dis_ctrl_proceso.proceso_cod = 'DISB201IT' ",
            " AND   dis_ctrl_proceso.etapa_cod   = 3 ",        
            " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

      PREPARE claexe12 FROM cla_upd
      EXECUTE claexe12
   END IF
END MAIN


FUNCTION Proceso_principal()
   DEFINE
      ejecuta CHAR(200),
      index   CHAR(100)

   CALL Inicializa()

   CALL Lectura()

   ERROR "PROCESANDO INFORMACION...",reg_ident.nom_archivo

   CALL Ingresa_etapa(vfolio,3,"Separa archivo") 
   CALL Separa_archivo()                                  -- ETAPA 3   
   CALL Actualiza_etapa(vfolio,3,"Separa archivo")

   CALL Ingresa_etapa(vfolio,4,"Carga historicos")
   CALL Sube_datos()                                      -- ETAPA 4
   CALL Actualiza_etapa(vfolio,4,"Carga historicos")

   IF vfolio <> 0 THEN

      --- ACTIVA OPTCOMPIND PARA QUE TODOS LOS QUERYES ENTREN POR INDICE -----

      LET ejecuta = "cd ",gparam_dis.ruta_exp CLIPPED,
                       "/;OPTCOMPIND=0;export OPTCOMPIND;"
      RUN ejecuta

      IF vrecauda = "E" THEN
         ERROR "Buscando nss validados ..."
         CALL Prepara_especial(vfolio) 
         ERROR ""
      END IF

--      DATABASE safre_tmp
--         LET index = " SET INDEXES FOR dis_prov_iti DISABLED"
--         PREPARE cr_idx_dp1 FROM index
--         EXECUTE cr_idx_dp1
--      DATABASE safre_af

      LET cla_apo = "SELECT codigo_siefore,",       --c22-6
                           "porcentaje ",           --c22-6
                    "FROM  safre_af:cta_regimen ",  --c22-6
                    "WHERE nss = ? ",               --c22-6
                    "AND   subcuenta = ? "          --c22-6

      PREPARE claexe4 FROM cla_apo                  --c22-6
      DECLARE cur_prov CURSOR FOR claexe4           --c22-6

      LET cla_com = "SELECT a.tipo, ",                             --c22-6
                           "b.val_porcentaje, ",                   --c22-6
                           "c.codigo_siefore, ",                   --c22-6
                           "c.porcentaje, ",                       --c22-6
                           "b.tipo_comision ",                     --c22-6
                    " FROM  tab_movimiento a, ",                   --c22-6
                          " dis_val_comision b, ",                 --c22-6
                          " cta_regimen c ",                       --c22-6
                    " WHERE a.codigo        = b.tipo_comision ",   --c22-6
                    " AND   b.subcuenta     = ? ",                 --c22-6
                    " AND   b.tipo_comision <> 110 ",              --c22-6
                    " AND   b.antiguedad_hasta >= ? ",             --c22-6
                    " AND   b.antiguedad_desde <= ? ",             --c22-6
                    " AND   b.subcuenta = c.subcuenta ",           --c22-6
                    " AND   c.nss       = ? "                      --c22-6

      PREPARE claexe5 FROM cla_com           --c22-6
      DECLARE cur_com2 CURSOR FOR claexe5    --c22-6

      --- crea tabla tmporal para contestar confaf (dis_dep_iti) --- --c22-6
      CREATE TEMP TABLE tmp_dis_prov_iti   --c22-6
         (                                 --c22-6
           sie   SMALLINT,                 --c22-6
           sub   SMALLINT,                 --c22-6
           mov   SMALLINT,                 --c22-6
           por   DECIMAL(12,2),            --c22-6
           idsie CHAR(08),                 --c22-6
           pesos DECIMAL(16,6)             --c22-6
         );

      CREATE TEMP TABLE tmp_siefore_local  --c22-6
         (                                 --c22-6
          codigo_siefore  SMALLINT,        --c22-6
          razon_social    CHAR(08)         --c22-6
         );                                --c22-6

      INSERT INTO tmp_siefore_local        --c22-6
      SELECT codigo_siefore,               --c22-6
             razon_social                  --c22-6
      FROM   tab_siefore_local             --c22-6
      
      CALL Ingresa_etapa(vfolio,5,"Dispersa Intereses") 
      IF vrecauda = "N" THEN
         CALL Genera_aportes_nor(vfolio)                  -- ETAPA 5
      END IF
      CALL Actualiza_etapa(vfolio,5,"Dispersa Intereses")

      DATABASE safre_tmp
         CREATE INDEX d_iti_1 ON dis_prov_iti  (folio, subcuenta,
                                                tipo_movimiento, estado);
         CREATE INDEX d_iti_2 ON dis_prov_iti  (nss);
         CREATE INDEX d_iti_3 on dis_prov_iti  (folio, subcuenta);
         UPDATE STATISTICS FOR TABLE dis_prov_iti;
      DATABASE safre_af

      CALL Ingresa_etapa(vfolio,7,"Genera confaf")    
      CALL Genera_confaf(vfolio)                          -- ETAPA 7 
      CALL Actualiza_etapa(vfolio,7,"Genera confaf")    

      CALL Ingresa_etapa(vfolio,8,"Provisiona Aportes")
      CALL Provisiones(vfolio)                            -- ETAPA 8
      CALL Actualiza_etapa(vfolio,8,"Provisiona Aportes")

      ------ ACTIVA OPTCOMPIND DE DEFAULT ---------------
      LET ejecuta = "cd ",gparam_dis.ruta_exp CLIPPED,
                       "/;OPTCOMPIND=2;export OPTCOMPIND;"
      RUN ejecuta
   ELSE
      DISPLAY "ERROR FOLIO CERO !!! "
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

   LET vnom_archivo = new_cadena CLIPPED,".TRAN" CLIPPED                --c22-11
-- LET vnom_archivo=reg_ident.nom_archivo[1,6] CLIPPED,".CONFAF" CLIPPED--c22-11

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
   LET tot_acep.impt_viv_gar       = 0
   LET tot_acep.part_viv           = 0   ----jerry
   LET tot_acep.impt_rem_viv       = 0   --c22-11
   LET tot_acep.apli_rem           = 0   --c22-116
   LET tot_acep.apli_rem_gar       = 0   --c22-116
   LET tot_acep.inte_ext_viv       = 0   --c22-11
   LET tot_acep.impt_aport_lplazo  = 0   --c22-11
   LET tot_acep.impt_aport_subadi  = 0   --c22-11
   LET tot_acep.impt_rem_gar       = 0   --c22-11
   LET tot_acep.inte_ext_gar       = 0   --c22-11
   LET tot_acep.apli_ext           = 0   --c22-116
   LET tot_acep.apli_ext_gar       = 0   --c22-116
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
   LET tot_acepint.inte_ext_viv       = 0  --c22-11
   LET tot_acepint.inte_ext_gar       = 0  --c22-11
   LET tot_acepint.impt_aport_lplazo  = 0  --c22-11
   LET tot_acepint.impt_aport_subadi  = 0  --c22-11

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
   LET tot_rech.impt_rem_viv       = 0   --c22-11
   LET tot_rech.impt_rem_gar       = 0   --c22-11
   LET tot_rech.apli_rem           = 0   --c22-116
   LET tot_rech.apli_rem_gar       = 0   --c22-116
   LET tot_rech.inte_ext_viv       = 0   --c22-11
   LET tot_rech.inte_ext_gar       = 0   --c22-11
   LET tot_rech.apli_ext           = 0   --c22-116
   LET tot_rech.apli_ext_gar       = 0   --c22-116
   LET tot_rech.impt_aport_lplazo  = 0   --c22-11
   LET tot_rech.impt_aport_subadi  = 0   --c22-11
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
   LET tot_rechint.inte_ext_viv       = 0   --c22-11
   LET tot_rechint.inte_ext_gar       = 0   --c22-11
   LET tot_rechint.impt_aport_lplazo  = 0   --c22-11
   LET tot_rechint.impt_aport_subadi  = 0   --c22-11

   LET vcont_acep = 0
   LET vcont_rech = 0

   LET vnum_reg = 0
   LET vnum_ctas_xpagar = 0

   DATABASE safre_tmp
   WHENEVER ERROR CONTINUE
      DROP TABLE dis_prov_iti
   WHENEVER ERROR STOP

   CREATE TABLE dis_prov_iti
   (
    tipo_movimiento smallint not null ,
    subcuenta smallint not null ,
    siefore smallint,
    folio decimal(10,0) not null ,
    consecutivo_lote integer,
    nss char(11) not null ,
    curp char(18),
    folio_sua char(6),
    fecha_pago date,
    fecha_valor date,
    fecha_conversion date,
    monto_en_pesos decimal(16,6),
    monto_en_acciones decimal(16,6),
    precio_accion decimal(16,6),
    dias_cotizados integer,
    sucursal char(10),
    id_aportante char(11),
    estado smallint,
    fecha_proceso date,
    usuario char(8),
    fecha_archivo date,
    etiqueta integer
   );
   DATABASE safre_af
END FUNCTION


FUNCTION Inserta_proceso()
   LET hora_inicial = TIME
   LET hora_final   = null

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB201IT",             -- proceso_cod
       1,                       -- etapa_cod   -- LECTURA
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       reg_ident.nom_archivo,   -- parametro1
       vrecauda,                -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       reg_ident.folio,         -- folio
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
      ELSE
         IF cadena[i,i] = '.' THEN
            EXIT FOR
         ELSE
            LET cadena_limpia = cadena_limpia CLIPPED
         END IF
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
       "DISB201IT",             -- proceso_cod
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
      vfolio         INTEGER,
      vetapa_cod     DECIMAL(2,0),
      vresultado     CHAR(50)

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM dis_ctrl_proceso ",
                 " WHERE proceso_cod = 'DISB201IT' ",
                 " AND etapa_cod = ",vetapa_cod CLIPPED
                                                      
   PREPARE claexe3 FROM cla_sel                        
   DECLARE cur_proceso3 CURSOR FOR claexe3
   OPEN cur_proceso3                                   
      FETCH cur_proceso3 INTO vrow
   CLOSE cur_proceso3                                  

   LET vhora_final   = TIME

   LET cla_upd= " UPDATE dis_ctrl_proceso ",
                " SET   dis_ctrl_proceso.hora_final = ","'",vhora_final,"'",",",
                "       dis_ctrl_proceso.folio      = ",vfolio,",",
                "       dis_ctrl_proceso.resultado  = ","'",vresultado,"'",
                " WHERE  dis_ctrl_proceso.proceso_cod  = 'DISB201IT' ",
                " AND    dis_ctrl_proceso.etapa_cod    = ",vetapa_cod,
                " AND    dis_ctrl_proceso.consecutivo  = ",vrow CLIPPED
   PREPARE claexe35 FROM cla_upd
   EXECUTE claexe35
END FUNCTION


FUNCTION Prepara_especial(vfolio)
   DEFINE vfolio INTEGER

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE maeafi_esp
      DROP TABLE nss_esp
   WHENEVER ERROR STOP

   CREATE TABLE maeafi_esp
      (consecutivo INTEGER,
       fentcons    DATE,
       finicta     DATE,
       tipo_solicitud SMALLINT,
       fecha_1a_afil DATE);   --c7.6

   CREATE INDEX maeafi_esp_1 ON maeafi_esp (consecutivo);

   CREATE table nss_esp
    (consec_lote INTEGER);
   CREATE index nss_esp_1 on nss_esp (consec_lote);

   DATABASE safre_af
   WHENEVER ERROR STOP

-- c22-11
-- LET comando = gparam_dis.ruta_rescate CLIPPED,"/",reg_ident.nom_archivo[1,6],
   LET comando = gparam_dis.ruta_rescate CLIPPED,"/",new_cadena CLIPPED,
                                          ".con"

   LOAD FROM comando INSERT INTO safre_tmp:nss_esp
                      
   IF SQLCA.SQLCODE = 100 THEN
      DISPLAY  "Program Stopped .NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
      EXIT PROGRAM
   END IF
   WHENEVER ERROR STOP

   INSERT INTO safre_tmp:maeafi_esp
   SELECT consec_lote,
          fentcons,
          finicta,
          tipo_solicitud,
          fecha_1a_afil          --c7.6
   FROM   safre_tmp:nss_esp,
          dis_det_iti h,
          afi_mae_afiliado a
   WHERE  h.folio           = vfolio
   AND    h.consec_reg_lote = consec_lote
   AND    h.n_seguro        = a.n_seguro

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTAR EN maeafi_esp"
      SLEEP 4
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Separa_archivo()  -- ETAPA 3
   DEFINE
      ejecuta CHAR(200)

   LET ejecuta = "head -n 1 ",gparam_dis.ruta_rescate CLIPPED,"/",
   reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/cza_iti"
   RUN ejecuta

   LET ejecuta = "grep '^03' ",gparam_dis.ruta_rescate CLIPPED,"/",
   reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/det_iti"
   RUN ejecuta
 
   LET ejecuta = "grep '^08' ",gparam_dis.ruta_rescate CLIPPED,"/",
   reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/dep_iti"
   RUN ejecuta

   LET ejecuta = "tail -1 ",gparam_dis.ruta_rescate CLIPPED,"/",
   reg_ident.nom_archivo CLIPPED," >",gparam_dis.ruta_rescate CLIPPED,"/sum_iti"
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
                 "; cut -c 17-24 cza_iti > fecha_lote_iti"
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                 "; cut -c 25-27 cza_iti > lote_iti"
   RUN ejecuta
   
   WHENEVER ERROR CONTINUE
      DROP TABLE itfecha_lote
      DROP TABLE ittmp_lote     --c22-11
   WHENEVER ERROR STOP

   CREATE TEMP TABLE itfecha_lote
      (fecha_lote CHAR(08))
      
   CREATE TEMP TABLE ittmp_lote --c22-11
      (lote CHAR(03))

   LET ejecuta = gparam_dis.ruta_rescate CLIPPED,"/fecha_lote_iti" CLIPPED
   LOAD FROM ejecuta INSERT INTO itfecha_lote

   LET ejecuta = gparam_dis.ruta_rescate CLIPPED,"/lote_iti" CLIPPED
   LOAD FROM ejecuta INSERT INTO ittmp_lote  --c22-11

   SELECT fecha_lote
   INTO   vfecha_lote
   FROM   itfecha_lote
   
   SELECT lote
   INTO   vlote
   FROM   ittmp_lote

   SELECT "X"
   FROM  dis_cza_iti
   WHERE fech_creac_lote = vfecha_lote
   AND   lote_del_dia    = vlote

   IF SQLCA.SQLCODE = 100 THEN

      LET vfecha_recepcion = TODAY
      LET vhora_recepcion  = TIME
      LET vestado          = 2
      LET vfecha_estado    = TODAY
      LET vhora_estado     = TIME
      LET gfecha_recepcion = vfecha_recepcion


      --------------------   GENERA dis_cza_iti      --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_cza_iti"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"   ---- Cabeza
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza_iti cza_iti > icza1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' icza1 > cza_iti "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;dbload -d safre_af -c sube_cza_iti -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; rm  cza_iti icza1 fecha_lote_iti lote_iti "
      RUN ejecuta


      --------------------   GENERA dis_det_iti      --------------------

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_det
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza_iti > vfolio_det_iti" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det2 con el numero de registros igual al
      -- num. registros que tiene el detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f vfolio_det_iti det_iti > ifolio_det2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det borrando lineas que no sirven
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' ifolio_det2 > vfolio_det_iti" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm ifolio_det2"
      RUN ejecuta

      -- Se crea det2 cortando pos 1-283 y det3 cortando
      -- hasta del pos 285-295 y se pegan en el archivo det
      --            "; cut -c 1-350 det_ims > det2"   --c22-11
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cut -c 1-440 det_iti > idet2"   --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
      --            "; cut -c 352-360 det_iti > idet3" --c22-11
                    "; cut -c 442-450 det_iti > idet3" --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste -d '1' idet2 idet3 > det_iti"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm idet2 idet3"
      RUN ejecuta
    
      -- Se crea det1 pegando datos genrales con el detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_det_iti det_iti > idet1" CLIPPED
      RUN ejecuta

      -- Se crea det eliminando espacio en blanco que se genera cuando
      -- se pegan los 2 archivos
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' idet1 > det_iti "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; rm idet1 vfolio_det_iti"
      RUN ejecuta

      -- Se suben los datos del detalle
      LET ejecuta= "cd ",gparam_dis.ruta_rescate CLIPPED,
                   "/;dbload -d safre_af -c sube_det_iti -l dbload2.log -n 5000 -k;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm det_iti"
      RUN ejecuta


{DMR  --------------------   GENERA dis_int_iti       --------------------

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_int
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza_iti > vfolio_int_iti" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_int2 con el numero de registros igual al
      -- num. registros que tiene el detalle de intereses
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f vfolio_int_iti int_iti > vfolio_iti2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_int borrando lineas que no sirven
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_iti2 > vfolio_int_iti" CLIPPED 
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm vfolio_iti2"
      RUN ejecuta
      
      -- Se crea int2 cortando pos 1-283 y int3 cortando
      -- hasta del pos 285-295 y se pegan en el archivo int
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
         --         "; cut -c 1-290 int_iti > iti2"              --c22-6
                    "; cut -c 1-325 int_iti > iti2"              --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
         --         "; cut -c 292-360 int_iti > iti3"   ----jerry  --c22-6
                    "; cut -c 327-450 int_iti > iti3"   ----jerry  --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste -d '1' iti2 iti3 > int_iti"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,"; rm iti2 iti3"
      RUN ejecuta

      -- Se crea int1 pegando datos genrales con el detalle 
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_int_iti int_iti > iti1" CLIPPED
      RUN ejecuta

      -- Se crea int eliminando espacio en blanco que se genera cuando
      -- se pegan los 2 archivos
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' iti1 > int_iti "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; rm iti1 vfolio_int_iti"
      RUN ejecuta

      -- Se suben los datos del detalle
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;dbload -d safre_af -c sube_int_iti -l dbload.log;"
      RUN ejecuta

      -- Se borran todo los archivo auxiliares
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; rm int_iti"
      RUN ejecuta
DMR}


      --------------------   GENERA dis_dep_iti      --------------------

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_dep_iti"
      START REPORT salida TO vreporte
         LET vtipo_reporte = "D"   ---- deposito
         OUTPUT TO REPORT salida(vfolio,vfecha_recepcion,vhora_recepcion,
                                 vestado,vfecha_estado,vhora_estado)
      FINISH REPORT salida

      LET cfecha_envio = TODAY
      LET vfecha_envio = cfecha_envio[7,10],
                         cfecha_envio[1,02],
                         cfecha_envio[4,05]

      LET vreporte = gparam_dis.ruta_rescate CLIPPED,"/vfolio_dep_iti2"
      START REPORT salida2 TO vreporte
         OUTPUT TO REPORT salida2(vfecha_lote,vfecha_envio)
      FINISH REPORT salida2

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_dep_iti > depiti01_1 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; cat adi vfolio_dep_iti2 > depiti02_1 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f depiti01_1 dep_iti > depiti01_2 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -f depiti02_1 dep_iti > depiti02_2 " CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' depiti01_2 > depiti01_3" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; sed -e '/^08/d' depiti02_2 > depiti02_3"
      RUN ejecuta

      --  Aqui de coloca la posicion final del campo tipo_siefore del Layout 08
      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
            ---- "; cut -c 1-127 dep_iti > depiti2; mv depiti2 dep_iti " --c22-6
            --   "; cut -c 1-138 dep_iti > depiti2; mv depiti2 dep_iti " --c22-6
                 "; cut -c 1-246 dep_iti > depiti2; mv depiti2 dep_iti" --c22-11
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste depiti01_3 dep_iti depiti02_3 > depiti1;"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' depiti1 > dep_iti "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
 "/;DBDATE=Y4MD0;export DBDATE;dbload -d safre_af -c sube_dep_iti -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                  "; rm  vfolio_dep_iti vfolio_dep_iti2 depiti01_1 depiti02_1",
                  " depiti01_2 depiti02_2 depiti01_3 depiti02_3 dep_iti depiti1"
      RUN ejecuta


      --------------------   GENERA dis_sum_iti      --------------------

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; paste vfolio_cza_iti sum_iti > sumit1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' sumit1 > sum_iti "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "/;dbload -d safre_af -c sube_sum_iti -l dbload.log"
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dis.ruta_rescate CLIPPED,
                    "; rm  sumit1 sum_iti vfolio_cza_iti"
      RUN ejecuta


------ ACTIVA OPTCOMPIND PARA QUE TODOS LOS QUERYES ENTREN POR INDICE -----

      LET ejecuta = "cd ",gparam_dis.ruta_exp CLIPPED,
                    "/;OPTCOMPIND=0;export OPTCOMPIND;"
      RUN ejecuta

------ ACTIVA OPTCOMPIND DE DEFAULT ---------------

      LET ejecuta = "cd ",gparam_dis.ruta_exp CLIPPED,
                    "/;OPTCOMPIND=2;export OPTCOMPIND;"
      RUN ejecuta

   ELSE
      ERROR "ESTE @ ARCHIVO YA HA SIDO PROCESADO"
      SLEEP 3
      ERROR ""
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
            PRINT vfolio USING '----------',
                  vfecha_recepcion,
                  vhora_recepcion,
                  vestado,
                  vfecha_estado,
                  vhora_estado 
          ELSE
             PRINT vfolio USING '----------'
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
      vfolio                INTEGER,
      vresult_operacion     CHAR(02),
      vfentcons             DATE,
      vfecha_1a_afil        DATE,
      vfinicta              DATE,
      vcoduni               CHAR(10),
      index                 CHAR(100),
      vlongitud             INTEGER,
      vident_viv_garantia   CHAR(01),
      vtipo_solicitud       SMALLINT,

      reg RECORD
         nss                CHAR(11),
         curp               CHAR(13),
         sua                CHAR(06),
         reg_pat            CHAR(11),
         fecha_pago         CHAR(08),
         fecha_rcv          CHAR(08),
         fecha_viv          CHAR(08),
         impt_ret           DECIMAL(15,2),
         impt_act_rec_ret   DECIMAL(15,2),
         impt_ces_vej       DECIMAL(15,2),
         impt_act_r_ces_vej DECIMAL(15,2),
         impt_aport_vol     DECIMAL(15,2),
         impt_aport_compl   DECIMAL(15,2),  --c22-6
         impt_aport_pat     DECIMAL(15,2),
         impt_cuota_soc     DECIMAL(15,2),
         impt_aport_est     DECIMAL(15,2),
         impt_aport_esp     DECIMAL(15,2),
         impt_act_cuo_soc   DECIMAL(15,2),
         impt_act_aport_est DECIMAL(15,2),
         impt_act_cuo_esp   DECIMAL(15,2),
         inte_ext_viv       DECIMAL(15,2),  --c22-11
         impt_aport_lplazo  DECIMAL(15,2),  --c22-11
         impt_aport_subadi  DECIMAL(15,2),  --c22-11
         periodo_pago       CHAR(06),
         result_operacion   CHAR(02),
         consec_reg_lote    INTEGER
      END RECORD,

      vimpt_ret             DECIMAL(15,2),
      vimpt_act_rec_ret     DECIMAL(15,2),
      vimpt_ces_vej         DECIMAL(15,2),
      vimpt_act_r_ces_vej   DECIMAL(15,2),
      vimpt_aport_vol       DECIMAL(15,2),
      vimpt_aport_compl     DECIMAL(15,2), --c22-6
      vimpt_aport_pat       DECIMAL(15,2),
      vimpt_viv_gar         DECIMAL(15,2),
      vimpt_cuota_soc       DECIMAL(15,2),
      vimpt_aport_est       DECIMAL(15,2),
      vimpt_aport_esp       DECIMAL(15,2),
      vimpt_act_cuo_soc     DECIMAL(15,2),
      vimpt_act_aport_est   DECIMAL(15,2),
      vimpt_act_cuo_esp     DECIMAL(15,2),
      vpart_viv             DECIMAL(18,6), ----jerry
      vvalor_part           DECIMAL(18,6), ----jerry

      vimpt_rem_viv         DECIMAL(15,2), --c22-11
      vimpt_rem_gar         DECIMAL(15,2), --c22-11
      vapli_rem             DECIMAL(18,6), --c22-11
      vapli_rem_gar         DECIMAL(18,6), --c22-11

      vinte_ext_viv         DECIMAL(15,2), --c22-11
      vinte_ext_gar         DECIMAL(15,2), --c22-11
      vapli_ext             DECIMAL(18,6), --c22-11
      vapli_ext_gar         DECIMAL(18,6), --c22-22

      vimpt_aport_lplazo    DECIMAL(15,2), --c22-11
      vimpt_aport_subadi    DECIMAL(15,2), --c22-11
      vpart_viv_gar         DECIMAL(18,6), ----jerry
      ind_reg               SMALLINT

--   DATABASE safre_tmp
--      LET index = " SET INDEXES FOR dis_prov_iti DISABLED"
--      PREPARE cr_idx_dp1 FROM index
--      EXECUTE cr_idx_dp1
--   DATABASE safre_af

   DECLARE cur_hist CURSOR FOR 
   SELECT h.n_seguro,
          h.n_unico,
          h.folio_pago_sua,
          h.reg_patronal_imss,
          h.fech_pago,
          h.fech_valor_rcv,
          h.fech_valor_viv,
          h.impt_ret,
          h.impt_act_rec_ret,
          h.impt_ces_vej,
          h.impt_act_r_ces_vej,
          h.impt_aport_vol,
          h.impt_aport_compl,   --c22-6
          h.impt_aport_pat,
          h.impt_cuota_soc,
          h.impt_aport_est,
          h.impt_aport_esp,
          h.impt_act_cuo_soc,
          h.impt_act_aport_est,
          h.impt_act_cuo_esp,
          h.inte_ext_viv,       --c22-11
          h.impt_aport_lplazo,  --c22-11
          h.impt_aport_subadi,  --c22-11
          h.periodo_pago,
          h.result_operacion,
          h.consec_reg_lote,
          m.fentcons ,    
          m.finicta,
          h.ident_viv_garantia,
          m.tipo_solicitud,
          m.fecha_1a_afil       --c7.6
   FROM   dis_det_iti h, OUTER afi_mae_afiliado m 
   WHERE  h.folio = vfolio
   AND    m.n_seguro = h.n_seguro 

   FOREACH cur_hist INTO reg.*,
                         vfentcons,
                         vfinicta,
                         vident_viv_garantia,
                         vtipo_solicitud,
                         vfecha_1a_afil       --c7.6

      LET vimpt_ret           = reg.impt_ret           / 100
      LET vimpt_act_rec_ret   = reg.impt_act_rec_ret   / 100
      LET vimpt_ces_vej       = reg.impt_ces_vej       / 100
      LET vimpt_act_r_ces_vej = reg.impt_act_r_ces_vej / 100
      LET vimpt_aport_vol     = reg.impt_aport_vol     / 100
      LET vimpt_aport_compl   = reg.impt_aport_compl   / 100    --c22-6

      IF vident_viv_garantia  = "1" THEN
         ##### APORTES GAR ####
         LET vimpt_viv_gar    = reg.impt_aport_pat     / 100
         LET vimpt_aport_pat  = 0

         LET vpart_viv_gar    = 0              / 1000000        ----jerry
         LET vpart_viv        = 0                               ----jerry

         ##### INTERESE EXTEMP #### 
         LET vinte_ext_gar    = reg.inte_ext_viv       / 100    --c22-11
         LET vinte_ext_viv    = 0                               --c22-11
      ELSE
         ##### APORTES ####
         LET vimpt_viv_gar    = 0
         LET vimpt_aport_pat  = reg.impt_aport_pat     / 100

         LET vpart_viv_gar    = 0                               ----jerry
         LET vpart_viv        = 0              / 1000000        ----jerry

         ##### INTERESE EXTEMP #### 
         LET vinte_ext_gar    = 0                               --c22-11
         LET vinte_ext_viv    = reg.inte_ext_viv       / 100    --c22-11
      END IF

      LET vimpt_cuota_soc     = reg.impt_cuota_soc     / 100
      LET vimpt_aport_est     = reg.impt_aport_est     / 100
      LET vimpt_aport_esp     = reg.impt_aport_esp     / 100
      LET vimpt_act_cuo_soc   = reg.impt_act_cuo_soc   / 100
      LET vimpt_act_aport_est = reg.impt_act_aport_est / 100
      LET vimpt_act_cuo_esp   = reg.impt_act_cuo_esp   / 100

      LET vimpt_aport_lplazo  = reg.impt_aport_lplazo / 100     --c22-11
      LET vimpt_aport_subadi  = reg.impt_aport_subadi / 100     --c22-11

      ------- validacion que nss exista en cta_regimen -------  --c22-6.3
      LET ind_reg = 0

      SELECT UNIQUE nss                                --c22-6.3
      FROM   cta_regimen                               --c22-6.3
      WHERE  nss = reg.nss                             --c22-6.3

      IF SQLCA.SQLCODE = NOTFOUND THEN                 --c22-6.3
         LET ind_reg = 1
         ERROR "NSS no existe en cta_regimen ",reg.nss --c22-6.3
         SLEEP 1                                       --c22-6.3
      END IF                                           --c22-6.3
      --------------------------------------------------------  --c22-6.3

      IF vfentcons IS NULL OR ind_reg = 1 THEN

         CALL Contador_rechazo()

         LET vaportes="S"

         CALL Sumatoria_rechazo(vimpt_ret,
                                vimpt_act_rec_ret,
                                vimpt_ces_vej,
                                vimpt_act_r_ces_vej,
                                vimpt_aport_vol,
                                vimpt_aport_compl,  --c22-6
                                vimpt_aport_pat,
                                vimpt_cuota_soc,
                                vimpt_aport_est,
                                vimpt_aport_esp,
                                vimpt_act_cuo_soc,
                                vimpt_act_aport_est,
                                vimpt_act_cuo_esp,
                                vimpt_viv_gar,
                                vpart_viv,             ----jerry
                                vpart_viv_gar,         ----jerry
                                vimpt_rem_viv,         --c22-11
                                vimpt_rem_gar,         --c22-11
                                vapli_rem,             --c22-11
                                vapli_rem_gar,         --c22-11
                                vinte_ext_viv,         --c22-11
                                vinte_ext_gar,         --c22-11
                                vapli_ext,             --c22-11
                                vapli_ext_gar,         --c22-11
                                vimpt_aport_lplazo,    --c22-11
                                vimpt_aport_subadi)    --c22-11

         IF vident_viv_garantia = "1" THEN
            LET vident_viv_garantia = "2"
         END IF

         UPDATE dis_det_iti
         SET    result_operacion = "02",
                ident_viv_garantia = vident_viv_garantia
         WHERE  folio = vfolio
         AND    n_seguro = reg.nss
         AND    consec_reg_lote = reg.consec_reg_lote
      ELSE
         LET vfecha_aux = vfinicta
         LET vperiodo   = reg.periodo_pago

      --  IF vfinitmte IS NOT NULL THEN
      --     LET vfentcons = vfinitmte
      --  END IF

---let vconsec = vconsec + 1
---let vnssx = reg.nss
---call llena_nss()

         CALL Contador_aceptado()

         LET vaportes="S"

         CALL Sumatoria_aceptado(vimpt_ret,
                                 vimpt_act_rec_ret,
                                 vimpt_ces_vej,
                                 vimpt_act_r_ces_vej,
                                 vimpt_aport_vol,
                                 vimpt_aport_compl,
                                 vimpt_aport_pat,
                                 vimpt_cuota_soc,
                                 vimpt_aport_est,
                                 vimpt_aport_esp,
                                 vimpt_act_cuo_soc,
                                 vimpt_act_aport_est,
                                 vimpt_act_cuo_esp,
                                 vimpt_viv_gar,
                                 vpart_viv,             ----jerry
                                 vpart_viv_gar,         ----jerry
                                 vimpt_rem_viv,         --c22-11
                                 vimpt_rem_gar,         --c22-11
                                 vapli_rem,             --c22-11
                                 vapli_rem_gar,         --c22-11
                                 vinte_ext_viv,         --c22-11
                                 vinte_ext_gar,         --c22-11
                                 vapli_ext,             --c22-11
                                 vapli_ext_gar,         --c22-11
                                 vimpt_aport_lplazo,    --c22-11
                                 vimpt_aport_subadi)    --c22-11

-------------------- DISPERSA APORTES EN dis_prov_iti --------------------------

         IF vimpt_ret > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_ret,1,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_act_rec_ret > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_act_rec_ret,1,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_ces_vej > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_ces_vej,2,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_act_r_ces_vej > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_act_r_ces_vej,2,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_aport_vol > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_aport_vol,3,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_aport_compl > 0 THEN  --c22-6
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_aport_compl,11,3,vfentcons,reg.consec_reg_lote,0,0)   --c22-6
         END IF --c22-6

{ #####  DMR VIVIENDA NO MANEJA INTERESES EN TRANSITO

         IF vimpt_aport_pat > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_aport_pat,4,1,vfentcons,reg.consec_reg_lote,vpart_viv,vvalor_part)  ----jerry
         END IF   --c22-117

         IF vimpt_rem_viv > 0 OR vapli_rem > 0 THEN    --c22-111
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,"REMANENTE",reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_rem_viv,4,3,vfentcons,reg.consec_reg_lote,vapli_rem,vvalor_part)  --c22-11
         END IF                       --c22-111

         IF vinte_ext_viv > 0 OR vapli_ext > 0 THEN    --c22-111
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,"EXTEMPORA",reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vinte_ext_viv,4,4,vfentcons,reg.consec_reg_lote,vapli_ext,vvalor_part)  --c22-11
         END IF                       --c22-111 

         IF vimpt_viv_gar <> 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_viv_gar,4,1,vfentcons,reg.consec_reg_lote,vpart_viv_gar,vvalor_part)  ----jerry jerry jerry jerry

CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_viv_gar,4,7,vfentcons,reg.consec_reg_lote,vpart_viv_gar,vvalor_part)  ----jerry jerry jerry jerry
         END IF

         IF vimpt_rem_gar <> 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,"REMANENTE",reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_rem_gar,4,3,vfentcons,reg.consec_reg_lote,vapli_rem_gar,vvalor_part)  --c22-11

CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,"REMANENTE",reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_rem_gar,4,8,vfentcons,reg.consec_reg_lote,vapli_rem_gar,vvalor_part)  --c22-11
	 END IF

         IF vinte_ext_gar <> 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,"EXTEMPORA",reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vinte_ext_gar,4,4,vfentcons,reg.consec_reg_lote,vapli_ext_gar,vvalor_part)   --c22-11

CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,"EXTEMPORA",reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vinte_ext_gar,4,38,vfentcons,reg.consec_reg_lote,vapli_ext_gar,vvalor_part)  --c22-11
	 END IF

#####  DMR  }

         IF vimpt_cuota_soc > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_cuota_soc,5,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_act_cuo_soc > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_act_cuo_soc,5,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_aport_est > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_aport_est,6,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_act_aport_est > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_act_aport_est,6,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_aport_esp > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_aport_esp,9,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_act_cuo_esp > 0 THEN
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_act_cuo_esp,9,3,vfentcons,reg.consec_reg_lote,0,0)   ----jerry
         END IF

         IF vimpt_aport_lplazo > 0 THEN --c22-11
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_aport_lplazo,15,3,vfentcons,reg.consec_reg_lote,0,0)                          --c22-112
         END IF

         IF vimpt_aport_subadi > 0 THEN --c22-11
CALL Dispersa_aporte(vfolio,reg.nss,reg.curp,reg.sua,vcoduni,reg.reg_pat,reg.fecha_pago,reg.fecha_rcv,reg.fecha_viv,vimpt_aport_subadi,17,3,vfentcons,reg.consec_reg_lote,0,0)                          --c22-112
         END IF

--------------------SE CALCULABA COMISIONES POR FLUJO--------------------------

      END IF

      LET vimpt_ret           = 0
      LET vimpt_act_rec_ret   = 0
      LET vimpt_ces_vej       = 0
      LET vimpt_act_r_ces_vej = 0
      LET vimpt_aport_vol     = 0
      LET vimpt_aport_compl   = 0
      LET vimpt_aport_pat     = 0
      LET vimpt_viv_gar       = 0
      LET vimpt_cuota_soc     = 0
      LET vimpt_aport_est     = 0
      LET vimpt_aport_esp     = 0
      LET vimpt_act_cuo_soc   = 0
      LET vimpt_act_aport_est = 0
      LET vimpt_act_cuo_esp   = 0
      LET vpart_viv           = 0    ----jerry
      LET vvalor_part         = 0    ----jerry
      LET vimpt_rem_viv       = 0    --c22-11
      LET vimpt_rem_gar       = 0    --c22-11
      LET vapli_rem           = 0    --c22-11
      LET vapli_rem_gar       = 0    --c22-11
      LET vinte_ext_viv       = 0    --c22-11
      LET vinte_ext_gar       = 0    --c22-11
      LET vapli_ext           = 0    --c22-11
      LET vapli_ext_gar       = 0    --c22-11
      LET vimpt_aport_lplazo  = 0    --c22-11
      LET vimpt_aport_subadi  = 0    --c22-11
      LET vpart_viv_gar       = 0    ----jerry

   END FOREACH

--   DATABASE safre_tmp
--      LET index = " SET INDEXES FOR dis_prov_iti ENABLED"
--      PREPARE cr_idx_dp FROM index
--      PREPARE prioridad FROM " SET PDQPRIORITY HIGH "
--      EXECUTE prioridad
--      EXECUTE cr_idx_dp
--   DATABASE safre_af
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
                         vsua,
                         vcoduni,
                         vreg_pat,
                         cfecha_pago,
                         cfecha_rcv,
                         cfecha_viv,
                         vmonto,
                         vsubcuenta,
                         vmovimiento,
                         vfentcons,
                         vconsec_reg_lote,
                         vmonto_part_viv,
                         vmonto_valor_part)   ----jerry  --c22-6

   DEFINE
      vfolio             INTEGER,
      vnss               CHAR(11),
      vcurp              CHAR(13),
      vsua               CHAR(06),
      vcoduni            CHAR(10),
      vreg_pat           CHAR(11),
      cfecha_pago        CHAR(08),
      cfecha_rcv         CHAR(08),
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
      vmonto_part_viv    DECIMAL(18,6),    ----jerry
      vmonto_valor_part  DECIMAL(18,6),    ----jerry
      vcomision          DECIMAL(16,6),
      vcomision2         DECIMAL(16,6),
      vtipo              SMALLINT,
      vval_porcentaje    DECIMAL(16,6),
      vsiefore           SMALLINT,
      vporcentaje_sie    DECIMAL(5,2),
      vantiguedad_desde  SMALLINT,
      vantiguedad_hasta  SMALLINT,
      vdias_antiguedad   INTEGER,
      abo RECORD LIKE    safre_af:dis_provision.*

   LET xfecha_pago = cfecha_pago[5,6],"/",
                     cfecha_pago[7,8],"/",
                     cfecha_pago[1,4]
   LET vfecha_pago = xfecha_pago

   IF vsubcuenta = 1 OR vsubcuenta = 2  OR
      vsubcuenta = 3 OR vsubcuenta = 5  OR
      vsubcuenta = 6 OR vsubcuenta = 9  OR 
      vsubcuenta= 11 OR vsubcuenta = 15 OR
      vsubcuenta= 17 THEN                --c22-113

      LET xfecha_rcv = cfecha_rcv[5,6],"/",
                       cfecha_rcv[7,8],"/",
	               cfecha_rcv[1,4]
      LET vfecha_valor = xfecha_rcv
   ELSE
      IF vsubcuenta = 4 THEN
         LET xfecha_viv = cfecha_viv[5,6],
                          cfecha_viv[7,8],
                          cfecha_viv[1,4]
         LET vfecha_valor = xfecha_viv
      ELSE
         LET vfecha_valor = TODAY
      END IF
   END IF

   LET xfecha_archivo = vfecha_lote[5,6],"/",
                        vfecha_lote[7,8],"/",
                        vfecha_lote[1,4]
   LET vfecha_archivo = xfecha_archivo

   FOREACH cur_prov USING vnss,            --c22-6
                          vsubcuenta       --c22-6
                    INTO  vsiefore,        --c22-6
                          vporcentaje_sie  --c22-6

      LET vcomision = vmonto * (vporcentaje_sie / 100)

      IF vmovimiento = 7 OR vmovimiento = 8 OR vmovimiento = 38 THEN
         LET vcomision = vcomision * -1
         LET vmonto_part_viv = vmonto_part_viv * -1  ----jerry
      END IF
   
      LET abo.tipo_movimiento   = vmovimiento    -- SI
      LET abo.subcuenta         = vsubcuenta     -- SI
      LET abo.folio             = vfolio         -- SI 
      LET abo.consecutivo_lote  = vconsec_reg_lote
      LET abo.siefore           = vsiefore       -- SI  --c22-6
      LET abo.nss               = vnss           -- SI
      LET abo.curp              = vcurp          -- NO
      LET abo.folio_sua         = vsua           -- SI
      LET abo.fecha_pago        = vfecha_pago    -- NO
      LET abo.fecha_valor       = vfecha_valor   -- SI
      LET abo.fecha_conversion  = NULL           -- SI
      LET abo.monto_en_pesos    = vcomision      -- SI
      LET abo.monto_en_acciones = vmonto_part_viv    -- SI  ----jerry 
      LET abo.precio_accion     = vmonto_valor_part  -- SI  ----jerry
      LET abo.dias_cotizados    = 0              -- NO
      LET abo.sucursal          = vcoduni        -- SI
      LET abo.id_aportante      = "BANXICO"      -- SI 
      LET abo.estado            = 5              -- SI 
      LET abo.fecha_proceso     = TODAY          -- SI
      LET abo.usuario           = gusuario       -- SI
      LET abo.fecha_archivo     = vfecha_archivo -- NO
      LET abo.etiqueta          = 0              -- SI

      SELECT razon_social                   --c22-6
      INTO   vid_sie                        --c22-6
      FROM   tmp_siefore_local              --c22-6
      WHERE  codigo_siefore = abo.siefore   --c22-6

      INSERT INTO tmp_dis_prov_iti VALUES 
         (abo.siefore,          --c22-6
          abo.subcuenta,        --c22-6
          abo.tipo_movimiento,  --c22-6
          vporcentaje_sie,      --c22-6
          vid_sie,              --c22-6
          abo.monto_en_pesos)   --c22-6

      IF abo.tipo_movimiento = 7 OR abo.tipo_movimiento = 8 OR
	 abo.tipo_movimiento = 38 
      THEN
         INSERT INTO safre_tmp:dis_prov_iti VALUES(abo.*)
      ELSE
         IF abo.monto_en_pesos > 0 THEN
            INSERT INTO safre_tmp:dis_prov_iti VALUES(abo.*)
         END IF
      END IF

      IF STATUS < 0 THEN
         ERROR "ERROR AL INSERTAR REGISTRO EN dis_prov_iti ",STATUS USING '-----'
         EXIT PROGRAM
      END IF

   END FOREACH  --c22-6
END FUNCTION


FUNCTION Sumatoria_rechazo(vimpt_ret,
	                   vimpt_act_rec_ret,
                           vimpt_ces_vej,
	                   vimpt_act_r_ces_vej,
	                   vimpt_aport_vol,
	                   vimpt_aport_compl,  --c22-6
	                   vimpt_aport_pat,
	                   vimpt_cuota_soc,
	                   vimpt_aport_est,
	                   vimpt_aport_esp,
	                   vimpt_act_cuo_soc,
	                   vimpt_act_aport_est,
	                   vimpt_act_cuo_esp,
                           vimpt_viv_gar,
                           vpart_viv,           ----jerry
                           vpart_viv_gar,       ----jerry
                           vimpt_rem_viv,       --c22-11
                           vimpt_rem_gar,       --c22-11
                           vapli_rem,           --c22-11
                           vapli_rem_gar,       --c22-11
                           vinte_ext_viv,       --c22-11
                           vinte_ext_gar,       --c22-11
                           vapli_ext,           --c22-11
                           vapli_ext_gar,       --c22-11
                           vimpt_aport_lplazo,  --c22-11
                           vimpt_aport_subadi)  --c22-11

   DEFINE      
      vimpt_ret            DECIMAL(15,2),
      vimpt_act_rec_ret    DECIMAL(15,2),
      vimpt_ces_vej        DECIMAL(15,2),
      vimpt_act_r_ces_vej  DECIMAL(15,2),
      vimpt_aport_vol      DECIMAL(15,2),
      vimpt_aport_compl    DECIMAL(15,2),  --c22-6
      vimpt_aport_pat      DECIMAL(15,2),
      vimpt_viv_gar        DECIMAL(15,2),
      vimpt_cuota_soc      DECIMAL(15,2),
      vimpt_aport_est      DECIMAL(15,2),
      vimpt_aport_esp      DECIMAL(15,2),
      vimpt_act_cuo_soc    DECIMAL(15,2),
      vimpt_act_aport_est  DECIMAL(15,2),
      vimpt_act_cuo_esp    DECIMAL(15,2),
      vpart_viv            DECIMAL(18,6),  ----jerry
      vimpt_rem_viv        DECIMAL(15,2),  --c22-11
      vimpt_rem_gar        DECIMAL(15,2),  --c22-11
      vapli_rem            DECIMAL(18,6),  --c22-11
      vapli_rem_gar        DECIMAL(18,6),  --c22-11
      vinte_ext_viv        DECIMAL(15,2),  --c22-11
      vinte_ext_gar        DECIMAL(15,2),  --c22-11
      vapli_ext            DECIMAL(18,6),  --c22-11
      vapli_ext_gar        DECIMAL(18,6),  --c22-11
      vimpt_aport_lplazo   DECIMAL(15,2),  --c22-11
      vimpt_aport_subadi   DECIMAL(15,2),  --c22-11
      vpart_viv_gar        DECIMAL(18,6)   ----jerry

   IF vaportes="S" THEN
LET tot_rech.impt_ret          =tot_rech.impt_ret           +vimpt_ret
LET tot_rech.impt_act_rec_ret  =tot_rech.impt_act_rec_ret   +vimpt_act_rec_ret
LET tot_rech.impt_ces_vej      =tot_rech.impt_ces_vej       +vimpt_ces_vej
LET tot_rech.impt_act_r_ces_vej=tot_rech.impt_act_r_ces_vej +vimpt_act_r_ces_vej
LET tot_rech.impt_aport_vol    =tot_rech.impt_aport_vol     +vimpt_aport_vol
LET tot_rech.impt_aport_compl=tot_rech.impt_aport_compl+vimpt_aport_compl--c22-6
LET tot_rech.impt_cuota_soc    =tot_rech.impt_cuota_soc     +vimpt_cuota_soc
LET tot_rech.impt_aport_est    =tot_rech.impt_aport_est     +vimpt_aport_est
LET tot_rech.impt_aport_esp    =tot_rech.impt_aport_esp     +vimpt_aport_esp
LET tot_rech.impt_act_cuo_soc  =tot_rech.impt_act_cuo_soc   +vimpt_act_cuo_soc
LET tot_rech.impt_act_aport_est=tot_rech.impt_act_aport_est +vimpt_act_aport_est
LET tot_rech.impt_act_cuo_esp  =tot_rech.impt_act_cuo_esp   +vimpt_act_cuo_esp

----VIVIENDA
LET tot_rech.impt_aport_pat = tot_rech.impt_aport_pat + vimpt_aport_pat
LET tot_rech.impt_viv_gar   = tot_rech.impt_viv_gar   + vimpt_viv_gar
LET tot_rech.part_viv       = tot_rech.part_viv       + vpart_viv      ----jerry
LET tot_rech.part_viv_gar   = tot_rech.part_viv_gar   + vpart_viv_gar  ----jerry

--c22-11
----REMANENTE
LET tot_rech.impt_rem_viv = tot_rech.impt_rem_viv + vimpt_rem_viv
LET tot_rech.impt_rem_gar = tot_rech.impt_rem_gar + vimpt_rem_gar
LET tot_rech.apli_rem     = tot_rech.apli_rem     + vapli_rem
LET tot_rech.apli_rem_gar = tot_rech.apli_rem_gar + vapli_rem_gar

----EXTEMPO
LET tot_rech.inte_ext_viv = tot_rech.inte_ext_viv + vinte_ext_viv
LET tot_rech.inte_ext_gar = tot_rech.inte_ext_gar + vinte_ext_gar
LET tot_rech.apli_ext     = tot_rech.apli_ext     + vapli_ext    
LET tot_rech.apli_ext_gar = tot_rech.apli_ext_gar + vapli_ext_gar    

--c22-11
LET tot_rech.impt_aport_lplazo =tot_rech.impt_aport_lplazo+vimpt_aport_lplazo
LET tot_rech.impt_aport_subadi =tot_rech.impt_aport_subadi+vimpt_aport_subadi

   ELSE
LET tot_rechint.impt_ret          =tot_rechint.impt_ret +
                                   vimpt_ret

LET tot_rechint.impt_act_rec_ret  =tot_rechint.impt_act_rec_ret +
                                   vimpt_act_rec_ret

LET tot_rechint.impt_ces_vej      =tot_rechint.impt_ces_vej +
                                   vimpt_ces_vej

LET tot_rechint.impt_act_r_ces_vej=tot_rechint.impt_act_r_ces_vej +
                                   vimpt_act_r_ces_vej

LET tot_rechint.impt_aport_vol    =tot_rechint.impt_aport_vol +
                                   vimpt_aport_vol

LET tot_rechint.impt_aport_compl  =tot_rechint.impt_aport_compl +    --c22-6
                                   vimpt_aport_compl                 --c22-6


LET tot_rechint.impt_aport_pat    =tot_rechint.impt_aport_pat +
                                   vimpt_aport_pat

LET tot_rechint.impt_cuota_soc    =tot_rechint.impt_cuota_soc +
                                   vimpt_cuota_soc

LET tot_rechint.impt_aport_est    =tot_rechint.impt_aport_est +
                                   vimpt_aport_est

LET tot_rechint.impt_aport_esp    =tot_rechint.impt_aport_esp +
                                   vimpt_aport_esp

LET tot_rechint.impt_act_cuo_soc  =tot_rechint.impt_act_cuo_soc +
                                   vimpt_act_cuo_soc

LET tot_rechint.impt_act_aport_est=tot_rechint.impt_act_aport_est+
                                   vimpt_act_aport_est

LET tot_rechint.impt_act_cuo_esp  =tot_rechint.impt_act_cuo_esp +
                                   vimpt_act_cuo_esp

--c22-11
LET tot_rechint.inte_ext_viv    =tot_rechint.inte_ext_viv     +vinte_ext_viv
LET tot_rechint.inte_ext_gar    =tot_rechint.inte_ext_gar     +vinte_ext_gar
LET tot_rechint.impt_aport_lplazo=tot_rechint.impt_aport_lplazo+
                                  vimpt_aport_lplazo
LET tot_rechint.impt_aport_subadi=tot_rechint.impt_aport_subadi+
                                  vimpt_aport_subadi
--c22-11

LET tot_rechint.impt_viv_gar  =tot_rechint.impt_viv_gar + vimpt_viv_gar
   END IF
END FUNCTION


FUNCTION Sumatoria_aceptado(vimpt_ret,
                            vimpt_act_rec_ret,
                            vimpt_ces_vej,
                            vimpt_act_r_ces_vej,
                            vimpt_aport_vol,
                            vimpt_aport_compl,   --c22-6
                            vimpt_aport_pat,
                            vimpt_cuota_soc,
                            vimpt_aport_est,
                            vimpt_aport_esp,
                            vimpt_act_cuo_soc,
                            vimpt_act_aport_est,
                            vimpt_act_cuo_esp,
                            vimpt_viv_gar,
                            vpart_viv,           ----jerry
                            vpart_viv_gar,       ----jerry
                            vimpt_rem_viv,       --c22-11
                            vimpt_rem_gar,       --c22-11
                            vapli_rem,           --c22-11
                            vapli_rem_gar,       --c22-11
                            vinte_ext_viv,       --c22-11
                            vinte_ext_gar,       --c22-11
                            vapli_ext,           --c22-11
                            vapli_ext_gar,       --c22-11
                            vimpt_aport_lplazo,  --c22-11
                            vimpt_aport_subadi)  --c22-11

   DEFINE      
      vimpt_ret             DECIMAL(15,2),
      vimpt_act_rec_ret     DECIMAL(15,2),
      vimpt_ces_vej         DECIMAL(15,2),
      vimpt_act_r_ces_vej   DECIMAL(15,2),
      vimpt_aport_vol       DECIMAL(15,2),
      vimpt_aport_compl     DECIMAL(15,2),  --c22-6
      vimpt_aport_pat       DECIMAL(15,2),
      vimpt_viv_gar         DECIMAL(15,2),
      vimpt_cuota_soc       DECIMAL(15,2),
      vimpt_aport_est       DECIMAL(15,2),
      vimpt_aport_esp       DECIMAL(15,2),
      vimpt_act_cuo_soc     DECIMAL(15,2),
      vimpt_act_aport_est   DECIMAL(15,2),
      vimpt_act_cuo_esp     DECIMAL(15,2),
      vpart_viv             DECIMAL(18,6),  ----jerry
      vimpt_rem_viv         DECIMAL(15,2),  --c22-11
      vimpt_rem_gar         DECIMAL(15,2),  --c22-11
      vapli_rem             DECIMAL(18,6),  --C22-11
      vapli_rem_gar         DECIMAL(18,6),  --C22-11
      vinte_ext_viv         DECIMAL(15,2),  --c22-11
      vinte_ext_gar         DECIMAL(15,2),  --c22-11
      vapli_ext             DECIMAL(18,6),  --c22-11
      vapli_ext_gar         DECIMAL(18,6),  --c22-11
      vimpt_aport_lplazo    DECIMAL(15,2),  --c22-11
      vimpt_aport_subadi    DECIMAL(15,2),  --c22-11
      vpart_viv_gar         DECIMAL(18,6)   ----jerry

   IF vaportes="S" THEN
LET tot_acep.impt_ret          =tot_acep.impt_ret           +vimpt_ret
LET tot_acep.impt_act_rec_ret  =tot_acep.impt_act_rec_ret   +vimpt_act_rec_ret
LET tot_acep.impt_ces_vej      =tot_acep.impt_ces_vej       +vimpt_ces_vej
LET tot_acep.impt_act_r_ces_vej=tot_acep.impt_act_r_ces_vej +vimpt_act_r_ces_vej
LET tot_acep.impt_aport_vol    =tot_acep.impt_aport_vol     +vimpt_aport_vol

LET tot_acep.impt_aport_compl=tot_acep.impt_aport_compl+vimpt_aport_compl--c22-6

LET tot_acep.impt_cuota_soc    =tot_acep.impt_cuota_soc     +vimpt_cuota_soc
LET tot_acep.impt_aport_est    =tot_acep.impt_aport_est     +vimpt_aport_est
LET tot_acep.impt_aport_esp    =tot_acep.impt_aport_esp     +vimpt_aport_esp
LET tot_acep.impt_act_cuo_soc  =tot_acep.impt_act_cuo_soc   +vimpt_act_cuo_soc
LET tot_acep.impt_act_aport_est=tot_acep.impt_act_aport_est +vimpt_act_aport_est
LET tot_acep.impt_act_cuo_esp  =tot_acep.impt_act_cuo_esp   +vimpt_act_cuo_esp

---VIVIENDA
LET tot_acep.impt_aport_pat = tot_acep.impt_aport_pat + vimpt_aport_pat
LET tot_acep.impt_viv_gar   = tot_acep.impt_viv_gar   + vimpt_viv_gar
LET tot_acep.part_viv       = tot_acep.part_viv       + vpart_viv      --jerry

--c22-11
---REMANENTE
LET tot_acep.impt_rem_viv = tot_acep.impt_rem_viv + vimpt_rem_viv
LET tot_acep.impt_rem_gar = tot_acep.impt_rem_gar + vimpt_rem_gar
LET tot_acep.apli_rem     = tot_acep.apli_rem     + vapli_rem 
LET tot_acep.apli_rem_gar = tot_acep.apli_rem_gar + vapli_rem_gar

--c22-11
---EXTEMPORA
LET tot_acep.inte_ext_viv = tot_acep.inte_ext_viv + vinte_ext_viv
LET tot_acep.inte_ext_gar = tot_acep.inte_ext_gar + vinte_ext_gar
LET tot_acep.apli_ext     = tot_acep.apli_ext     + vapli_ext 
LET tot_acep.apli_ext_gar = tot_acep.apli_ext_gar + vapli_ext_gar

LET tot_acep.impt_aport_lplazo =tot_acep.impt_aport_lplazo+vimpt_aport_lplazo
LET tot_acep.impt_aport_subadi =tot_acep.impt_aport_subadi+vimpt_aport_subadi
--c22-11

LET tot_acep.part_viv_gar = tot_acep.part_viv_gar + vpart_viv_gar  ----jerry

   ELSE

LET tot_acepint.impt_ret          =tot_acepint.impt_ret           +
                                   vimpt_ret

LET tot_acepint.impt_act_rec_ret  =tot_acepint.impt_act_rec_ret   +
                                   vimpt_act_rec_ret

LET tot_acepint.impt_ces_vej      =tot_acepint.impt_ces_vej       +
                                   vimpt_ces_vej

LET tot_acepint.impt_act_r_ces_vej=tot_acepint.impt_act_r_ces_vej +
                                   vimpt_act_r_ces_vej

LET tot_acepint.impt_aport_vol    =tot_acepint.impt_aport_vol     +
                                   vimpt_aport_vol

LET tot_acepint.impt_aport_compl  =tot_acepint.impt_aport_compl   +  --c22-6
                                   vimpt_aport_compl                 --c22-6

LET tot_acepint.impt_aport_pat    =tot_acepint.impt_aport_pat     +
                                   vimpt_aport_pat

LET tot_acepint.impt_cuota_soc    =tot_acepint.impt_cuota_soc     +
                                   vimpt_cuota_soc

LET tot_acepint.impt_aport_est    =tot_acepint.impt_aport_est     +
                                   vimpt_aport_est

LET tot_acepint.impt_aport_esp    =tot_acepint.impt_aport_esp     +
                                   vimpt_aport_esp

LET tot_acepint.impt_act_cuo_soc  =tot_acepint.impt_act_cuo_soc   +
                                   vimpt_act_cuo_soc

LET tot_acepint.impt_act_aport_est=tot_acepint.impt_act_aport_est +
                                   vimpt_act_aport_est

LET tot_acepint.impt_act_cuo_esp  =tot_acepint.impt_act_cuo_esp   +
                                   vimpt_act_cuo_esp
--c22-11
LET tot_acepint.inte_ext_viv    =tot_acepint.inte_ext_viv     +vinte_ext_viv
LET tot_acepint.inte_ext_gar    =tot_acepint.inte_ext_gar     +vinte_ext_gar

LET tot_acepint.impt_aport_lplazo = tot_acepint.impt_aport_lplazo +  --c22-11
                                   vimpt_aport_lplazo
LET tot_acepint.impt_aport_subadi = tot_acepint.impt_aport_subadi +  --c22-11
                                   vimpt_aport_subadi
LET tot_acepint.impt_viv_gar  =tot_acepint.impt_viv_gar   + vimpt_viv_gar
   END IF

END FUNCTION


FUNCTION Genera_confaf(vfolio)            -- ETAPA 7 
   DEFINE
      vfolio              INTEGER,
      ejecuta             CHAR(200),
      rident_pago         CHAR(16)

   DEFINE
      total_rcv           DECIMAL(16,2),
      total_vol           DECIMAL(16,2),  --c22-6
      total_compl         DECIMAL(16,2),  --c22-6
      total_viv           DECIMAL(16,2),
      total_gub           DECIMAL(16,2),
      total_viv_gar       DECIMAL(16,2),
      acept_rcv           DECIMAL(16,2),
      acept_vol           DECIMAL(16,2),  --c22-6
      acept_compl         DECIMAL(16,2),  --c22-6
      acept_viv           DECIMAL(16,2),
      acept_gub           DECIMAL(16,2),
      acept_viv_gar       DECIMAL(16,2),
      acept_part          DECIMAL(18,6),  ----jerry

      acept_rem_viv       DECIMAL(16,2),  --c22-11
      acept_rem_gar       DECIMAL(16,2),  --c22-11
      acept_apli_rem_viv  DECIMAL(18,6),  --c22-11
      acept_apli_rem_gar  DECIMAL(18,6),  --c22-11

      acept_ext_viv       DECIMAL(16,2),  --c22-11
      acept_ext_gar       DECIMAL(16,2),  --c22-11
      acept_apli_ext_viv  DECIMAL(18,6),  --c22-11
      acept_apli_ext_gar  DECIMAL(18,6),  --c22-11

      acept_aport_lplazo  DECIMAL(16,2),  --c22-11
      acept_aport_subadi  DECIMAL(16,2),  --c22-11

      recha_rcv           DECIMAL(16,2),
      recha_viv           DECIMAL(16,2),
      recha_vol           DECIMAL(16,2),  --c22-6
      recha_compl         DECIMAL(16,2),  --c22-6
      recha_gub           DECIMAL(16,2),
      recha_viv_gar       DECIMAL(16,2),
      recha_part          DECIMAL(18,6),  ----jerry

      recha_rem_viv       DECIMAL(16,2),  --c22-11
      recha_rem_gar       DECIMAL(16,2),  --c22-11
      recha_apli_rem_viv  DECIMAL(18,6),  --c22-11
      recha_apli_rem_gar  DECIMAL(18,6),  --c22-11

      recha_ext_viv       DECIMAL(16,2),  --c22-11
      recha_ext_gar       DECIMAL(16,2),  --c22-11
      recha_apli_ext_viv  DECIMAL(18,6),  --c22-11
      recha_apli_ext_gar  DECIMAL(18,6),  --c22-11

      recha_aport_lplazo  DECIMAL(16,2),  --c22-11
      recha_aport_subadi  DECIMAL(16,2),  --c22-11

      total_int_rcv       DECIMAL(16,2),
      total_int_vol       DECIMAL(16,2),  --c22-6
      total_int_compl     DECIMAL(16,2),  --c22-6
      total_int_viv       DECIMAL(16,2),
      total_int_gub       DECIMAL(16,2),
      total_int_vivg      DECIMAL(16,2),
      acept_int_rcv       DECIMAL(16,2),
      acept_int_vol       DECIMAL(16,2),  --c22-6
      acept_int_compl     DECIMAL(16,2),  --c22-6
      acept_int_viv       DECIMAL(16,2),
      acept_int_gub       DECIMAL(16,2),
      acept_int_vivg      DECIMAL(16,2),
      acept_int_ext_viv   DECIMAL(16,2),  --c22-11 
      acept_int_ext_gar   DECIMAL(16,2),  --c22-11
      acept_int_lplazo    DECIMAL(16,2),  --c22-11
      acept_int_subadi    DECIMAL(16,2),  --c22-11
      acept_part_gar      DECIMAL(18,6),  ----jerry

      recha_int_rcv       DECIMAL(16,2),
      recha_int_vol       DECIMAL(16,2),  --c22-6
      recha_int_compl     DECIMAL(16,2),  --c22-6
      recha_int_viv       DECIMAL(16,2),
      recha_int_gub       DECIMAL(16,2),
      recha_int_vivg      DECIMAL(16,2),
      recha_int_ext_viv   DECIMAL(16,2),  --c22-11 
      recha_int_ext_gar   DECIMAL(16,2),  --c22-11
      recha_int_lplazo    DECIMAL(16,2),  --c22-11
      recha_int_subadi    DECIMAL(16,2),  --c22-11
      recha_part_gar      DECIMAL(18,6)   ----jerry

   DEFINE
      vsub                CHAR(08),       --c22-6
      vsie                SMALLINT,       --c22-6
      vpor                DECIMAL(16,8),  --c22-6--c22-6
      vident_sie          CHAR(08),       --c22-6
      vpesos              DECIMAL(16,6),  --c22-6
      vcuantos            SMALLINT,       --c22-6.3
      vcontador_sie       SMALLINT,       --c22-6.3
      vsuma               DECIMAL(16,6),  --c22-6--c22-6
      vimporte            DECIMAL(16,2),  --c22-6
      vimporte2           DECIMAL(16,2),  --c22-6.9
      vrechazo            DECIMAL(16,2)   --c22-6

   DEFINE
      g_dep RECORD LIKE   dis_dep_iti.*

   DEFINE
      vdescrip            CHAR(20)        --c22-117

   ------------------------- TOTAL ACEPTADO APORTES ------------------------
   LET acept_rcv = tot_acep.impt_ret          +
                   tot_acep.impt_act_rec_ret  +
                   tot_acep.impt_ces_vej      +
                   tot_acep.impt_act_r_ces_vej

   LET acept_vol   = tot_acep.impt_aport_vol    --c22-6

   LET acept_compl = tot_acep.impt_aport_compl  --c22-6   

   ---VIVIENDA
   LET acept_viv      = tot_acep.impt_aport_pat
   LET acept_viv_gar  = tot_acep.impt_viv_gar
   LET acept_part     = tot_acep.part_viv        ----jerry
   LET acept_part_gar = tot_acep.part_viv_gar    ----jerry

   LET acept_gub = tot_acep.impt_cuota_soc    +
                   tot_acep.impt_aport_est    +
                   tot_acep.impt_aport_esp    +
                   tot_acep.impt_act_cuo_soc  +
                   tot_acep.impt_act_aport_est+
                   tot_acep.impt_act_cuo_esp  
   ---REMANENTE
   LET acept_rem_viv      = tot_acep.impt_rem_viv      --c22-11
   LET acept_rem_gar      = tot_acep.impt_rem_gar      --c22-11
   LET acept_apli_rem_viv = tot_acep.apli_rem          --c22-11
   LET acept_apli_rem_gar = tot_acep.apli_rem_gar      --c22-11

   ---EXTEMPO
   LET acept_ext_viv      = tot_acep.inte_ext_viv      --c22-11
   LET acept_ext_gar      = tot_acep.inte_ext_gar      --c22-11
   LET acept_apli_ext_viv = tot_acep.apli_ext          --c22-11
   LET acept_apli_ext_gar = tot_acep.apli_ext_gar      --c22-11

   LET acept_aport_lplazo = tot_acep.impt_aport_lplazo --c22-11
   LET acept_aport_subadi = tot_acep.impt_aport_subadi --c22-11

   ------------------------- TOTAL RECHASOS APORTES ------------------------
   --SIN VIVIENDA

   SELECT SUM(impt_ret           +         
              impt_act_rec_ret   +         
              impt_ces_vej       +         
              impt_act_r_ces_vej) / 100,  --c22-6
          SUM(impt_aport_vol)     / 100,  --c22-6
          SUM(impt_aport_compl)   / 100,  --c22-6
          SUM(impt_cuota_soc     +         
              impt_aport_est     +         
              impt_aport_esp     +         
              impt_act_cuo_soc   +         
              impt_act_aport_est +         
              impt_act_cuo_esp)  / 100,
          SUM(impt_aport_lplazo) / 100,  --c22-11
          SUM(impt_aport_subadi) / 100   --c22-11
   INTO   recha_rcv,
          recha_vol,                     --c22-6
          recha_compl,                   --c22-6
          recha_gub,
          recha_aport_lplazo,            --c22-11
          recha_aport_subadi             --c22-11
   FROM   dis_det_iti
   WHERE  folio = vfolio
   AND    result_operacion = "02"

   ------------------------- TOTAL RECHASOS VIVIENDA------------------------

   SELECT SUM(impt_aport_pat) / 100,
          0,
          0,                              --c22-11 
          0,                              --c22-11
          SUM(inte_ext_viv)   / 100,      --c22-11
          0                               --c22-11
   INTO   recha_viv,
          recha_part,                     ----jerry jerry jerry
          recha_rem_viv,                  --c22-11 
          recha_apli_rem_viv,             --c22-11
          recha_ext_viv,                  --c22-11
          recha_apli_ext_viv              --c22-11
   FROM   dis_det_iti
   WHERE  folio = vfolio
   AND    result_operacion = "02"
   AND    ident_viv_garantia = "0"

   --VIVIENDA GARANTIA
   LET recha_viv_gar = tot_rech.impt_viv_gar
   LET recha_part_gar = tot_rech.part_viv_gar      ----jerry

   --VIVIENDA REMANENTE
   LET recha_rem_gar = tot_rech.impt_rem_gar       --c22-11
   LET recha_apli_rem_gar = tot_rech.apli_rem_gar  --c22-11

   --VIVIENDA EXTEMPORA
   LET recha_ext_gar = tot_rech.inte_ext_gar       --c22-11
   LET recha_apli_ext_gar = tot_rech.apli_ext_gar  --c22-11

   IF recha_rcv IS NULL THEN
      LET recha_rcv = 0
   END IF
   IF recha_vol IS NULL THEN
      LET recha_vol = 0
   END IF
   IF recha_compl IS NULL THEN
      LET recha_compl = 0
   END IF
   IF recha_gub IS NULL THEN
      LET recha_gub = 0
   END IF
   IF recha_viv IS NULL THEN
      LET recha_viv = 0
   END IF
   IF recha_part IS NULL THEN
      LET recha_part = 0
   END IF

   IF recha_rem_viv IS NULL THEN
      LET recha_rem_viv = 0
   END IF
   IF recha_apli_rem_viv IS NULL THEN
      LET recha_apli_rem_viv = 0
   END IF
   IF recha_ext_viv IS NULL THEN
      LET recha_ext_viv = 0
   END IF
   IF recha_apli_ext_viv IS NULL THEN
      LET recha_apli_ext_viv = 0
   END IF
   IF recha_aport_lplazo IS NULL THEN
      LET recha_aport_lplazo = 0
   END IF
   IF recha_aport_subadi IS NULL THEN
      LET recha_aport_subadi = 0
   END IF

   ------------------------- TOTAL ACEPTADO INTERESES  ---------------------

   LET acept_int_rcv = tot_acepint.impt_ret           +
                       tot_acepint.impt_act_rec_ret   +
                       tot_acepint.impt_ces_vej       +
                       tot_acepint.impt_act_r_ces_vej 
   --                  tot_acepint.impt_aport_vol +

   LET acept_int_vol   = tot_acepint.impt_aport_vol    --c22-6
   LET acept_int_compl = tot_acepint.impt_aport_compl  --c22-6

   LET acept_int_gub = tot_acepint.impt_cuota_soc     +
                       tot_acepint.impt_aport_est     +
                       tot_acepint.impt_aport_esp     +
                       tot_acepint.impt_act_cuo_soc   +
                       tot_acepint.impt_act_aport_est +
                       tot_acepint.impt_act_cuo_esp

   --INTER VIVIENDA
   LET acept_int_viv  = tot_acepint.impt_aport_pat
   LET acept_int_vivg = tot_acepint.impt_viv_gar

   --INTER EXTEMPO  
   LET acept_int_ext_viv = tot_acepint.inte_ext_viv      --c22-11
   LET acept_int_ext_gar = tot_acepint.inte_ext_gar      --c22-11

   LET acept_int_lplazo = tot_acepint.impt_aport_lplazo  --c22-11
   LET acept_int_subadi = tot_acepint.impt_aport_subadi  --c22-11

   ------------------------- TOTAL RECHASOS INTERESES  ---------------------

   LET recha_int_rcv = tot_rechint.impt_ret           +
                       tot_rechint.impt_act_rec_ret   +
                       tot_rechint.impt_ces_vej       +
                       tot_rechint.impt_act_r_ces_vej 
   --                  tot_rechint.impt_aport_vol + --c22-6

   LET recha_int_vol   = tot_rechint.impt_aport_vol   --c22-6
   LET recha_int_compl = tot_rechint.impt_aport_compl --c22-6

   LET recha_int_viv  = tot_rechint.impt_aport_pat
   LET recha_int_vivg = tot_rechint.impt_viv_gar

   LET recha_int_gub  = tot_rechint.impt_cuota_soc     +
                        tot_rechint.impt_aport_est     +
                        tot_rechint.impt_aport_esp     +
                        tot_rechint.impt_act_cuo_soc   +
                        tot_rechint.impt_act_aport_est +
                        tot_rechint.impt_act_cuo_esp 
   
   LET recha_int_ext_viv = tot_rechint.inte_ext_viv
   LET recha_int_ext_gar = tot_rechint.inte_ext_gar

   LET recha_int_lplazo = tot_rechint.impt_aport_lplazo 
   LET recha_int_subadi = tot_rechint.impt_aport_subadi 

--------------------------------------------------------------------------------

   CREATE INDEX tmp_dis_prov_iti_1 ON tmp_dis_prov_iti(sub,mov);  --c22-6
   CREATE INDEX tmp_dis_prov_iti_2 ON tmp_dis_prov_iti(sie);      --c22-6
   UPDATE STATISTICS FOR TABLE tmp_dis_prov_iti;                  --c22-6

   --------    Prepara aportes y comisiones de tmp_dis_prov_iti     -------

   CREATE TEMP TABLE tmp_res_provision                              --c22-6
      (sub   CHAR(08),                                              --c22-6
       sie   SMALLINT,                                              --c22-6
       por   DECIMAL(12,2),
       idsie CHAR(08),
       pesos DECIMAL(16,6))                                         --c22-6

   ---- APORTE RCV ----
   INSERT INTO tmp_res_provision                                    --c22-6
   SELECT "41",                                                     --c22-6
          sie,                                                      --c22-6
          por,                                                      --c22-6
          idsie,                                                    --c22-6
          sum(pesos)                                                --c22-6
   FROM   tmp_dis_prov_iti                                          --c22-6
   WHERE  sub in (1,2)                                              --c22-6
   AND    mov in (1,2)
   GROUP  BY 1,2,3,4                                                --c22-6
   
   ---- COMISION RCV ----
   INSERT INTO tmp_res_provision                                    --c22-6
   SELECT "11",                                                     --c22-6
          sie,                                                      --c22-6
          por,                                                      --c22-6
          idsie,                                                    --c22-6
          sum(pesos)                                                --c22-6
   FROM   tmp_dis_prov_iti                                          --c22-6
   WHERE  sub in (1,2)                                              --c22-6
   AND    mov in (100)
   GROUP  BY 1,2,3,4                                                --c22-6
  
   ---- APORTE  VOL  ----
   INSERT INTO tmp_res_provision                                    --c22-6
   SELECT "45",                                                     --c22-6
          sie,                                                      --c22-6
          por,                                                      --c22-6
          idsie,                                                    --c22-6
          sum(pesos)                                                --c22-6
   FROM   tmp_dis_prov_iti                                          --c22-6
   WHERE  sub = 3                                                   --c22-6
   AND    mov in (1,2)
   GROUP  BY 1,2,3,4                                                --c22-6

   ---- APORTE COMPL ----
   INSERT INTO tmp_res_provision                                    --c22-6
   SELECT "46",                                                     --c22-6
          sie,                                                      --c22-6
          por,                                                      --c22-6
          idsie,                                                    --c22-6
          sum(pesos)                                                --c22-6
   FROM   tmp_dis_prov_iti                                          --c22-6
   WHERE  sub = 11                                                  --c22-6
   AND    mov in (1,2)
   GROUP  BY 1,2,3,4                                                --c22-6

   ---- APORTE EST   ----
   INSERT INTO tmp_res_provision                                    --c22-6
   SELECT "42",                                                     --c22-6
          sie,                                                      --c22-6
          por,                                                      --c22-6
          idsie,                                                    --c22-6
          sum(pesos)                                                --c22-6
   FROM   tmp_dis_prov_iti                                          --c22-6
   WHERE  sub in (5,6,9)                                            --c22-6
   AND    mov in (1,2)
   GROUP  BY 1,2,3,4                                                --c22-6
   
   ---- COMISION EST ----
   INSERT INTO tmp_res_provision                                    --c22-6
   SELECT "12",                                                     --c22-6
          sie,                                                      --c22-6
          por,                                                      --c22-6
          idsie,                                                    --c22-6
          sum(pesos)                                                --c22-6
   FROM   tmp_dis_prov_iti                                          --c22-6
   WHERE  sub in (5,6,9)                                            --c22-6
   AND    mov in (100)
   GROUP  BY 1,2,3,4                                                --c22-6

   ---- APORTE AHORRO LARGO PLAZO----
   INSERT INTO tmp_res_provision                                    --c22-11
   SELECT "49",                                                     --c22-11
          sie,                                                      --c22-11
          por,                                                      --c22-11
          idsie,                                                    --c22-11
          sum(pesos)                                                --c22-11
   FROM   tmp_dis_prov_iti                                          --c22-11
   WHERE  sub = 15                                                  --c22-112
   AND    mov in (1,2)
   GROUP  BY 1,2,3,4                                                --c22-11

   ---- APORTE SUBC. ADICIONAL   ----
   INSERT INTO tmp_res_provision                                    --c22-11
   SELECT "40",                                                     --c22-13
          sie,                                                      --c22-11
          por,                                                      --c22-11
          idsie,                                                    --c22-11
          sum(pesos)                                                --c22-11
   FROM   tmp_dis_prov_iti                                          --c22-11
   WHERE  sub = 17                                                  --c22-112
   AND    mov in (1,2)
   GROUP  BY 1,2,3,4                                                --c22-11

   ---- INTERES TRAN RCV ----      --c22-6.8
   INSERT INTO tmp_res_provision   --c22-6.8
   SELECT "51",                    --c22-6.8
          sie,                     --c22-6.8
          por,                     --c22-6.8
          idsie,                   --c22-6.8
          sum(pesos)               --c22-6.8
   FROM   tmp_dis_prov_iti         --c22-6.8
   WHERE  sub in (1,2)             --c22-6.8
   AND    mov = 3                  --c22-6.8
   GROUP  BY 1,2,3,4               --c22-6.8

   ---- INTERES TRAN EST ----      --c22-6.8
   INSERT INTO tmp_res_provision   --c22-6.8
   SELECT "52",                    --c22-6.8
          sie,                     --c22-6.8
          por,                     --c22-6.8
          idsie,                   --c22-6.8
          sum(pesos)               --c22-6.8
   FROM   tmp_dis_prov_iti         --c22-6.8
   WHERE  sub in (5,6,9)           --c22-6.8
   AND    mov = 3                  --c22-6.8
   GROUP  BY 1,2,3,4               --c22-6.8

   ---- INTERES TRAN VOL ----      --c22-6.8
   INSERT INTO tmp_res_provision   --c22-6.8
   SELECT "55",                    --c22-11
          sie,                     --c22-6.8
          por,                     --c22-6.8
          idsie,                   --c22-6.8
          sum(pesos)               --c22-6.8
   FROM   tmp_dis_prov_iti         --c22-6.8
   WHERE  sub = 3                  --c22-6.8
   AND    mov = 3                  --c22-6.8
   GROUP  BY 1,2,3,4               --c22-6.8

   ---- INTERES TRAN COMPL RET     --c22-112
   INSERT INTO tmp_res_provision   --c22-112
   SELECT "56",                    --c22-112
          sie,                     --c22-112
          por,                     --c22-112
          idsie,                   --c22-112
          sum(pesos)               --c22-112
   FROM   tmp_dis_prov_iti         --c22-112
   WHERE  sub = 11                 --c22-112
   AND    mov = 3                  --c22-112
   GROUP  BY 1,2,3,4               --c22-112

   ---- INTERES TRAN AHORRO LPLAZO --c22-11
   INSERT INTO tmp_res_provision   --c22-11
   SELECT "59",                    --c22-11
          sie,                     --c22-11
          por,                     --c22-11
          idsie,                   --c22-11
          sum(pesos)               --c22-11
   FROM   tmp_dis_prov_iti         --c22-11
   WHERE  sub = 15                 --c22-112
   AND    mov = 3                  --c22-11
   GROUP  BY 1,2,3,4               --c22-11

   ---- INTERES TRAN SUBC. ADICION --c22-11
   INSERT INTO tmp_res_provision   --c22-11
   SELECT "50",                    --c22-11
          sie,                     --c22-11
          por,                     --c22-11
          idsie,                   --c22-11
          sum(pesos)               --c22-11
   FROM   tmp_dis_prov_iti         --c22-11
   WHERE  sub = 17                 --c22-112
   AND    mov = 3                  --c22-11
   GROUP  BY 1,2,3,4               --c22-11

{
   ---SI EN LOS APORTES DE RCV O DE EST SOLO TENGO REGISTRO
   ---PARA ALGUNA DE LAS SIEFORES, INGRESARE EN 0 EL REGIS. DE LA OTRA.

   LET vdescrip=''                                             --c22-117
   LET vsuma=0                                                 --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO   vsuma                                                --c22-117
   FROM   tmp_res_provision                                    --c22-117
   WHERE  sub=41                                               --c22-117
   AND    sie=1                                                --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO   vdescrip                                          --c22-117
      FROM   tab_siefore_local                                 --c22-117
      WHERE codigo_siefore=1                                   --c22-117

      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(41,'1',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   LET vdescrip=''                                             --c22-117
   LET vsuma=0                                                 --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO   vsuma                                                --c22-117
   FROM   tmp_res_provision                                    --c22-117
   WHERE  sub=41                                               --c22-117
   AND    sie=2                                                --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO   vdescrip                                          --c22-117
      FROM   tab_siefore_local                                 --c22-117
      WHERE codigo_siefore=2                                   --c22-117

      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(41,'2',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   ----------------------------- PARA EST
   LET vdescrip=''                                             --c22-117
   LET vsuma=0                                                 --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO   vsuma                                                --c22-117
   FROM   tmp_res_provision                                    --c22-117
   WHERE  sub=42                                               --c22-117
   AND    sie=1                                                --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO   vdescrip                                          --c22-117
      FROM   tab_siefore_local                                 --c22-117
      WHERE codigo_siefore=1                                   --c22-117

      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(42,'1',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   LET vdescrip=''                                             --c22-117
   LET vsuma=0                                                 --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO  vsuma                                                 --c22-117
   FROM  tmp_res_provision                                     --c22-117
   WHERE sub=42                                                --c22-117
   AND   sie=2                                                 --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO   vdescrip                                          --c22-117
      FROM   tab_siefore_local                                 --c22-117
      WHERE codigo_siefore=2                                   --c22-117

      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(42,'2',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   ---Si EN LAS COMISIO DE RCV O DE EST Y COMP RET SOLO TENGO REGISTRO
   ---PARA ALGUNA DE LAS SIEFORES, INGRESARE EN 0 EL REGIS. DE LA OTRA.

   LET vdescrip = ''                                           --c22-117
   LET vsuma    = 0                                            --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO  vsuma                                                 --c22-117
   FROM  tmp_res_provision                                     --c22-117
   WHERE sub = 11                                              --c22-117
   AND   sie = 1                                               --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO  vdescrip                                           --c22-117
      FROM  tab_siefore_local                                  --c22-117
      WHERE codigo_siefore = 1                                 --c22-117

      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(11,'1',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   LET vdescrip = ''                                           --c22-117
   LET vsuma    = 0                                            --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO   vsuma                                                --c22-117
   FROM   tmp_res_provision                                    --c22-117
   WHERE  sub = 11                                             --c22-117
   AND    sie = 2                                              --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO  vdescrip                                           --c22-117
      FROM  tab_siefore_local                                  --c22-117
      WHERE codigo_siefore = 2                                 --c22-117
 
      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(11,'2',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   ----------------------------- PARA EST

   LET vdescrip = ''                                           --c22-117
   LET vsuma    = 0                                            --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO   vsuma                                                --c22-117
   FROM   tmp_res_provision                                    --c22-117
   WHERE  sub = 12                                             --c22-117
   AND    sie = 1                                              --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO  vdescrip                                           --c22-117
      FROM  tab_siefore_local                                  --c22-117
      WHERE codigo_siefore = 1                                 --c22-117

      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(12,'1',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   LET vdescrip = ''                                           --c22-117
   LET vsuma    = 0                                            --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO   vsuma                                                --c22-117
   FROM   tmp_res_provision                                    --c22-117
   WHERE  sub = 12                                             --c22-117
   AND    sie = 2                                              --c22-117
 
   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO  vdescrip                                           --c22-117
      FROM  tab_siefore_local                                  --c22-117
      WHERE codigo_siefore = 2                                 --c22-117
 
      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(12,'2',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   ----------------------------- PARA COMPLE RET

   LET vdescrip = ''                                           --c22-117
   LET vsuma    = 0                                            --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO   vsuma                                                --c22-117
   FROM   tmp_res_provision                                    --c22-117
   WHERE  sub = 16                                             --c22-117
   AND    sie = 1                                              --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO  vdescrip                                           --c22-117
      FROM  tab_siefore_local                                  --c22-117
      WHERE codigo_siefore=1                                   --c22-117

      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(16,'1',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   LET vdescrip = ''                                           --c22-117
   LET vsuma    = 0                                            --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO   vsuma                                                --c22-117
   FROM   tmp_res_provision                                    --c22-117
   WHERE  sub = 16                                             --c22-117
   AND    sie = 2                                              --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO  vdescrip                                           --c22-117
      FROM  tab_siefore_local                                  --c22-117
      WHERE codigo_siefore = 2                                 --c22-117

      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(16,'2',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   ----------------------------- INTE COMPLE RET

   LET vdescrip = ''                                           --c22-117
   LET vsuma    = 0                                            --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO   vsuma                                                --c22-117
   FROM   tmp_res_provision                                    --c22-117
   WHERE  sub = 36                                             --c22-117
   AND    sie = 1                                              --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO  vdescrip                                           --c22-117
      FROM  tab_siefore_local                                  --c22-117
      WHERE codigo_siefore = 1                                 --c22-117

      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(36,'1',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117

   LET vdescrip = ''                                           --c22-117
   LET vsuma    = 0                                            --c22-117

   SELECT sum(pesos)                                           --c22-117
   INTO   vsuma                                                --c22-117
   FROM   tmp_res_provision                                    --c22-117
   WHERE  sub = 36                                             --c22-117
   AND    sie = 2                                              --c22-117

   IF vsuma IS NULL OR vsuma = 0 THEN                          --c22-117
      SELECT razon_social                                      --c22-117
      INTO  vdescrip                                           --c22-117
      FROM  tab_siefore_local                                  --c22-117
      WHERE codigo_siefore = 2                                 --c22-117

      INSERT INTO tmp_res_provision                            --c22-117
      VALUES(36,'2',100,vdescrip,0)                            --c22-117
   END IF                                                      --c22-117
}

   -------- Obtiene aportes de tmp_res_provision --------

   DECLARE cur_resumen CURSOR FOR                              --c22-6
   SELECT sub,                                                 --c22-6
          sie,                                                 --c22-6
          por,                                                 --c22-6
          idsie,                                               --c22-6
          SUM(pesos),                                          --c22-6
          count(*)                                             --c22-6.3
   FROM   tmp_res_provision                                    --c22-6
   GROUP  BY 1,2,3,4                                           --c22-6
   ORDER  BY 1,2,3,4                                           --c22-6

   LET vcontador_sie = 0

   FOREACH cur_resumen INTO vsub,vsie,vpor,vident_sie,vpesos

      SELECT COUNT(*)                                          --c22-6.4
      INTO   vcuantos                                          --c22-6.4
      FROM   tmp_res_provision                                 --c22-6.4
      WHERE  sub = vsub                                        --c22-6.4

      SELECT SUM(pesos)                                        --c22-6--c22-6
      INTO   vsuma                                             --c22-6--c22-6
      FROM   tmp_res_provision                                 --c22-6--c22-6
      WHERE  sub = vsub                                        --c22-6--c22-6

      -- LET vpor = vpesos / vsuma                             --c22-113

      CASE                                                     --c22-6
         WHEN vsub = "11"                                      --c22-6
            LET vrechazo = 0                                   --c22-6
         WHEN vsub = "51"                                      --c22-6
            LET vrechazo = recha_rcv                           --c22-6
         WHEN vsub = "55"                                      --c22-6
            LET vrechazo = recha_vol                           --c22-6
         WHEN vsub = "56"                                      --c22-6
            LET vrechazo = recha_compl                         --c22-6
         WHEN vsub = "12"                                      --c22-6
            LET vrechazo = 0                                   --c22-6
         WHEN vsub = "52"                                      --c22-6
            LET vrechazo = recha_gub                           --c22-6
         WHEN vsub = "59"                                      --c22-11
            LET vrechazo = recha_aport_lplazo                  --c22-11
         WHEN vsub = "50"                                      --c22-11
            LET vrechazo = recha_aport_subadi                  --c22-11
      END CASE                                                 --c22-6

--    IF vsie = 1 THEN                                         --c22-6.3

      SELECT *                                                 --c22-6
      INTO   g_dep.*                                           --c22-6
      FROM   dis_dep_iti                                       --c22-6
      WHERE  folio             = vfolio                        --c22-6
      AND    ident_pago[14,15] = vsub                          --c22-6
      AND    tipo_siefore      = 0                             --c22-6
    
      IF vcuantos = 1 THEN                                     --c22-6.3

         UPDATE dis_dep_iti                                    --c22-6
         SET    importe          = importe / 100,              --c22-6.5
                impt_aport_acept = vpesos,                     --c22-6
                impt_aport_dev   = 0,  --vrechazo,             --c22-6.9
                part_viv_env     = 0,                          --c22-6
                part_viv_acept   = 0,                          --c22-6
                part_viv_dev     = 0,                          --c22-6
                apli_rem_viv_env = 0,                          --c22-11
                apli_rem_viv_ace = 0,                          --c22-11
                apli_rem_viv_dev = 0,                          --c22-11
                apli_ext_viv_env = 0,                          --c22-11
                apli_ext_viv_ace = 0,                          --c22-11
                apli_ext_viv_dev = 0,                          --c22-11
                ident_siefore    = vident_sie,                 --c22-6
                tipo_siefore     = vsie                        --c22-6
         WHERE  folio             = vfolio                     --c22-6
         AND    ident_pago[14,15] = vsub                       --c22-6
         AND    tipo_siefore      = 0                          --c22-6
   
         IF vsub = 45 THEN                                     --c22-6.6
            UPDATE dis_dep_iti                                 --c22-6.6
            SET    ident_siefore = vident_sie,                 --c22-6.6
                   tipo_siefore  = vsie                        --c22-6.6
            WHERE  folio             = vfolio                  --c22-6.6
            AND    ident_pago[14,15] = "15"                    --c22-6.6
            AND    tipo_siefore      = 0                       --c22-6.6
         END IF

         IF vsub = 46 THEN                                     --c22-111
            UPDATE dis_dep_iti                                 --c22-111
            SET    ident_siefore = vident_sie,                 --c22-111
                   tipo_siefore  = vsie                        --c22-111
            WHERE  folio             = vfolio                  --c22-111
            AND    ident_pago[14,15] = "16"                    --c22-111
            AND    tipo_siefore      = 0                       --c22-111
         END IF

         IF vsub = 51 THEN                                     --c22-6.8
            UPDATE dis_dep_iti                                 --c22-6.8
            SET    ident_siefore = vident_sie,                 --c22-6.8
                   tipo_siefore  = vsie                        --c22-6.8
            WHERE  folio             = vfolio                  --c22-6.8
            AND    ident_pago[14,15] = "31"                    --c22-6.8
            AND    tipo_siefore      = 0                       --c22-6.8
         END IF                                                --c22-6.8

         IF vsub = 52 THEN                                     --c22-6.8
            UPDATE dis_dep_iti                                 --c22-6.8
            SET    ident_siefore = vident_sie,                 --c22-6.8
                   tipo_siefore  = vsie                        --c22-6.8
            WHERE  folio             = vfolio                  --c22-6.8
            AND    ident_pago[14,15] = "32"                    --c22-6.8
            AND    tipo_siefore      = 0                       --c22-6.8
         END IF                                                --c22-6.8

         IF vsub = 55 THEN                                     --c22-6.8
            UPDATE dis_dep_iti                                 --c22-6.8
            SET    ident_siefore = vident_sie,                 --c22-6.8
                   tipo_siefore  = vsie                        --c22-6.8
            WHERE  folio             = vfolio                  --c22-6.8
            AND    ident_pago[14,15] = "35"                    --c22-6.8
            AND    tipo_siefore      = 0                       --c22-6.8
         END IF                                                --c22-6.8

         IF vsub = 56 THEN                                     --c22-112
            UPDATE dis_dep_iti                                 --c22-112
            SET    ident_siefore = vident_sie,                 --c22-112
                   tipo_siefore  = vsie                        --c22-112
            WHERE  folio             = vfolio                  --c22-112
            AND    ident_pago[14,15] = "36"                    --c22-112
            AND    tipo_siefore      = 0                       --c22-112
         END IF                                                --c22-112

         IF vrechazo > 0 THEN                                  --c22-6.9
            LET vimporte2 = g_dep.importe / 100                --c22-6.9
            INSERT INTO dis_dep_iti    VALUES                  --c22-6.9
              (                                                --c22-6.9
              g_dep.folio,                                     --c22-6.9
              g_dep.tipo_registro,                             --c22-6.9
              g_dep.ident_servicio,                            --c22-6.9
              g_dep.ident_pago,                                --c22-6.9
              vimporte2,                                       --c22-6.9
              g_dep.fech_liquidacion,                          --c22-6.9
              0,                                               --c22-6.9
              vrechazo,                                        --c22-6.9
              g_dep.part_viv_env,                              --c22-6.9
              g_dep.part_viv_acept,                            --c22-6.9
              g_dep.part_viv_dev,                              --c22-6.9
              g_dep.apli_rem_viv_env,                          --c22-11
              g_dep.apli_rem_viv_ace,                          --c22-11
              g_dep.apli_rem_viv_dev,                          --c22-11
              g_dep.apli_ext_viv_ace,                          --c22-11
              g_dep.apli_ext_viv_ace,                          --c22-11
              g_dep.apli_ext_viv_dev,                          --c22-11
              "",                                              --c22-6.9
              0,                                               --c22-6.9
              g_dep.fecha_archivo,                             --c22-6.9
              g_dep.fecha_envio,                               --c22-6.9
              g_dep.estado                                     --c22-6.9
              )                                                --c22-6.9
         END IF                                                --c22-6.9
      ELSE                                                     --c22-6.3
         LET vcontador_sie = vcontador_sie + 1                 --c22-6.3

         IF vcontador_sie = 1 THEN                             --c22-6.3

--            IF vsie = 2 THEN    --revisar gvp 23-abr-08 si se quita por multi
--               LET vrechazo = 0 
--            END IF             

            UPDATE dis_dep_iti                                 --c22-6.3
            SET    importe          = importe / 100,           --c22-6.5
                   impt_aport_acept = vpesos,                  --c22-6.3
                   impt_aport_dev   = 0,  --vrechazo,          --c22-6.10
                   part_viv_env     = 0,                       --c22-6.3
                   part_viv_acept   = 0,                       --c22-6.3
                   part_viv_dev     = 0,                       --c22-6.3
                   apli_rem_viv_env = 0,                       --c22-11
                   apli_rem_viv_ace = 0,                       --c22-11
                   apli_rem_viv_dev = 0,                       --c22-11
                   apli_ext_viv_env = 0,                       --c22-11
                   apli_ext_viv_ace = 0,                       --c22-11
                   apli_ext_viv_dev = 0,                       --c22-11
                   ident_siefore    = vident_sie,              --c22-6.3
                   tipo_siefore     = vsie                     --c22-6.3
            WHERE  folio             = vfolio                  --c22-6.3
            AND    ident_pago[14,15] = vsub                    --c22-6.3
            AND    tipo_siefore      = 0                       --c22-6.3
 
            IF vsub = 45 THEN                                  --c22-6.6
               UPDATE dis_dep_iti                              --c22-6.6
               SET    ident_siefore = vident_sie,              --c22-6.6
                      tipo_siefore  = vsie                     --c22-6.6
               WHERE  folio             = vfolio               --c22-6.6
               AND    ident_pago[14,15] = "15"                 --c22-6.6
               AND    tipo_siefore      = 0                    --c22-6.6
            END IF

            IF vsub = 46 THEN                                  --c22-111
               UPDATE dis_dep_iti                              --c22-111
               SET    ident_siefore = vident_sie,              --c22-111
                      tipo_siefore  = vsie                     --c22-111
               WHERE  folio             = vfolio               --c22-111
               AND    ident_pago[14,15] = "16"                 --c22-111
               AND    tipo_siefore      = 0                    --c22-111
            END IF

            IF vsub = 51 THEN                                  --c22-6.8
               UPDATE dis_dep_iti                              --c22-6.8
               SET    ident_siefore = vident_sie,              --c22-6.8
                      tipo_siefore  = vsie                     --c22-6.8
               WHERE  folio             = vfolio               --c22-6.8
               AND    ident_pago[14,15] = "31"                 --c22-6.8
               AND    tipo_siefore      = 0                    --c22-6.8
            END IF                                             --c22-6.8

            IF vsub = 52 THEN                                  --c22-6.8
               UPDATE dis_dep_iti                              --c22-6.8
               SET    ident_siefore = vident_sie,              --c22-6.8
                      tipo_siefore  = vsie                     --c22-6.8
               WHERE  folio             = vfolio               --c22-6.8
               AND    ident_pago[14,15] = "32"                 --c22-6.8
               AND    tipo_siefore      = 0                    --c22-6.8
            END IF                                             --c22-6.8

            IF vsub = 55 THEN                                  --c22-6.8
               UPDATE dis_dep_iti                              --c22-6.8
               SET    ident_siefore = vident_sie,              --c22-6.8
                      tipo_siefore  = vsie                     --c22-6.8
               WHERE  folio             = vfolio               --c22-6.8
               AND    ident_pago[14,15] = "35"                 --c22-6.8
               AND    tipo_siefore      = 0                    --c22-6.8
            END IF                                             --c22-6.8

            IF vsub = 56 THEN                                  --c22-112
               UPDATE dis_dep_iti                              --c22-112
               SET    ident_siefore = vident_sie,              --c22-112
                      tipo_siefore  = vsie                     --c22-112
               WHERE  folio             = vfolio               --c22-112
               AND    ident_pago[14,15] = "36"                 --c22-112
               AND    tipo_siefore      = 0                    --c22-112
            END IF                                             --c22-112

            IF vrechazo > 0 THEN                               --c22-6.9
               LET vimporte2 = g_dep.importe / 100             --c22-6.9
               INSERT INTO dis_dep_iti VALUES                  --c22-6.9
                 (                                             --c22-6.9
                 g_dep.folio,                                  --c22-6.9
                 g_dep.tipo_registro,                          --c22-6.9
                 g_dep.ident_servicio,                         --c22-6.9
                 g_dep.ident_pago,                             --c22-6.9
                 vimporte2,                                    --c22-6.9
                 g_dep.fech_liquidacion,                       --c22-6.9
                 0,                                            --c22-6.9
                 vrechazo,                                     --c22-6.9
                 g_dep.part_viv_env,                           --c22-6.9
                 g_dep.part_viv_acept,                         --c22-6.9
                 g_dep.part_viv_dev,                           --c22-6.9
                 g_dep.apli_rem_viv_env,                       --c22-11
                 g_dep.apli_rem_viv_ace,                       --c22-11
                 g_dep.apli_rem_viv_dev,                       --c22-11
                 g_dep.apli_ext_viv_env,                       --c22-11
                 g_dep.apli_ext_viv_ace,                       --c22-11
                 g_dep.apli_ext_viv_dev,                       --c22-11
                 "",                                           --c22-6.9
                 0,                                            --c22-6.9
                 g_dep.fecha_archivo,                          --c22-6.9
                 g_dep.fecha_envio,                            --c22-6.9
                 g_dep.estado                                  --c22-6.9
                 )                                             --c22-6.9
            END IF                                             --c22-6.9
         ELSE
            {
              IF vsie = 2 THEN   --OR vsie= 3                  --c22-119
                 LET vrechazo = 0                              --c22-119
              END IF                                           --c22-119
            }

            IF vrechazo > 0 THEN                               --c22-119
               LET vimporte = g_dep.importe --/ 100            --c22-119
            ELSE                                               --c22-119
               LET vimporte = g_dep.importe / 100              --c22-119
            END IF                                             --c22-119

            IF vsie >= 2 THEN                                  --c22-119
               LET vrechazo = 0                                --c22-119
            END IF                                             --c22-119

            LET g_dep.part_viv_env     = 0                     --c22-6
            LET g_dep.part_viv_acept   = 0                     --c22-6
            LET g_dep.part_viv_dev     = 0                     --c22-6
            LET g_dep.apli_rem_viv_env = 0                     --c22-11
            LET g_dep.apli_rem_viv_ace = 0                     --c22-11
            LET g_dep.apli_rem_viv_dev = 0                     --c22-11
            LET g_dep.apli_ext_viv_env = 0                     --c22-11
            LET g_dep.apli_ext_viv_ace = 0                     --c22-11
            LET g_dep.apli_ext_viv_dev = 0                     --c22-11
      
            INSERT INTO dis_dep_iti VALUES                     --c22-6
              (                                                --c22-6
               g_dep.folio,                                    --c22-6
               g_dep.tipo_registro,                            --c22-6
               g_dep.ident_servicio,                           --c22-6
               g_dep.ident_pago,                               --c22-6
               vimporte,                                       --c22-6
               g_dep.fech_liquidacion,                         --c22-6
               vpesos,                                         --c22-6
               vrechazo,                                       --c22-6
               g_dep.part_viv_env,                             --c22-6
               g_dep.part_viv_acept,                           --c22-6
               g_dep.part_viv_dev,                             --c22-6
               g_dep.apli_rem_viv_env,                         --c22-11
               g_dep.apli_rem_viv_ace,                         --c22-11
               g_dep.apli_rem_viv_dev,                         --c22-11
               g_dep.apli_ext_viv_env,                         --c22-11
               g_dep.apli_ext_viv_ace,                         --c22-11
               g_dep.apli_ext_viv_dev,                         --c22-11
               vident_sie,                                     --c22-6
               vsie,                                           --c22-6
               g_dep.fecha_archivo,                            --c22-6
               g_dep.fecha_envio,                              --c22-6
               g_dep.estado                                    --c22-6
              )                                                --c22-6
         END IF                                                --c22-6

         IF vcontador_sie = vcuantos THEN                      --c22-6.3
            LET vcontador_sie = 0                              --c22-6.3
         END IF                                                --c22-6.3
      END IF                                                   --c22-6

      DECLARE curctar CURSOR FOR
      SELECT ident_pago
      FROM  dis_dep_iti    
      WHERE folio = vfolio
      AND   impt_aport_acept = 0
      AND   impt_aport_dev   = 0
      AND   ident_pago       = g_dep.ident_pago
   -- AND   ident_pago[14,15] IN ("41","43","47","48","49","4A","4B")

      FOREACH curctar INTO rident_pago
         UPDATE dis_dep_iti
         SET impt_aport_dev = vrechazo,
             importe        = importe / 100
         WHERE folio      = vfolio
         AND   ident_pago = rident_pago
      END FOREACH

   END FOREACH                                                 --c22-6

   ----------------------  IMPORTES VIV  ---------------------------

   UPDATE dis_dep_iti
   SET    importe          = importe / 100,
          impt_aport_acept = acept_viv,
          impt_aport_dev   = recha_viv,
          part_viv_env     = part_viv_env / 1000000,  ----jerry jerry
          part_viv_acept   = acept_part,              ----jerry
          part_viv_dev     = recha_part,              ----jerry
          apli_rem_viv_env = 0,                       --c22-11
          apli_rem_viv_ace = 0,                       --c22-11
          apli_rem_viv_dev = 0,                       --c22-11
          apli_ext_viv_env = 0,                       --c22-11
          apli_ext_viv_ace = 0,                       --c22-11
          apli_ext_viv_dev = 0                        --c22-11
   WHERE  folio = vfolio
   AND ident_pago[14,15] = "43"  ----  aportaciones viv
   AND estado = 2

   UPDATE dis_dep_iti
   SET    importe          = importe / 100,
          impt_aport_acept = acept_int_viv,
          impt_aport_dev   = recha_int_viv,
          part_viv_env     = 0,                       ----jerry
          part_viv_acept   = 0,                       ----jerry
          part_viv_dev     = 0,                       ----jerry
          apli_rem_viv_env = 0,                       --c22-11
          apli_rem_viv_ace = 0,                       --c22-11
          apli_rem_viv_dev = 0,                       --c22-11
          apli_ext_viv_env = 0,                       --c22-11
          apli_ext_viv_ace = 0,                       --c22-11
          apli_ext_viv_dev = 0                        --c22-11
   WHERE folio  = vfolio
   AND   ident_pago[14,15] = "53"  ----  intereses viv
   AND   estado = 2

   ----------------------  IMPORTES VIV GAR  -----------------------

   UPDATE dis_dep_iti
   SET    importe          = importe / 100,                      
          impt_aport_acept = acept_viv_gar,                      
          impt_aport_dev   = recha_viv_gar,
          part_viv_env     = part_viv_env / 1000000,  ----jerry jerry
          part_viv_acept   = acept_part_gar,          ----jerry
          part_viv_dev     = recha_part_gar,          ----jerry
          apli_rem_viv_env = 0,                       --c22-11
          apli_rem_viv_ace = 0,                       --c22-11
          apli_rem_viv_dev = 0,                       --c22-11
          apli_ext_viv_env = 0,                       --c22-11
          apli_ext_viv_ace = 0,                       --c22-11
          apli_ext_viv_dev = 0                        --c22-11
   WHERE folio  = vfolio                                         
   AND   ident_pago[14,15] = "44"  ----  aportaciones viv 43 bis
   AND   estado = 2                                             

   UPDATE dis_dep_iti
   SET    importe          = importe / 100,                      
          impt_aport_acept = acept_int_vivg,                     
          impt_aport_dev   = recha_int_vivg,
          part_viv_env     = part_viv_env / 1000000,  ----jerry
          part_viv_acept   = 0,                       ----jerry
          part_viv_dev     = 0,                       ----jerry 
          apli_rem_viv_env = 0,                       --c22-11
          apli_rem_viv_ace = 0,                       --c22-11
          apli_rem_viv_dev = 0,                       --c22-11
          apli_ext_viv_env = 0,                       --c22-11
          apli_ext_viv_ace = 0,                       --c22-11
          apli_ext_viv_dev = 0                        --c22-11
   WHERE folio  = vfolio                                         
   AND   ident_pago[14,15] = "54"  ----  intereses viv 43 bis
   AND   estado = 2

   ----------------------  IMPORTES REMANENT -----------------------   --c22-11

   UPDATE dis_dep_iti
   SET    importe          = importe / 100,
          impt_aport_acept = acept_rem_viv + acept_rem_gar,            --c22-115
          impt_aport_dev   = recha_rem_viv + recha_rem_gar,            --c22-115
          part_viv_env     = part_viv_env   / 1000000,                 --c22-115
          part_viv_acept   = part_viv_acept / 1000000,                 --c22-115
          part_viv_dev     = part_viv_dev   / 1000000,                 --c22-115
          apli_rem_viv_env = apli_rem_viv_env / 1000000,
          apli_rem_viv_ace = acept_apli_rem_viv + acept_apli_rem_gar,  --acept_rem_viv,  --c22-116
          apli_rem_viv_dev = recha_apli_rem_viv + recha_apli_rem_gar,  --recha_rem_viv,  --c22-115
          apli_ext_viv_env = 0,                                        --c22-11
          apli_ext_viv_ace = 0,                                        --c22-11
          apli_ext_viv_dev = 0                                         --c22-11
   WHERE folio  = vfolio                                         
   AND   ident_pago[14,15] = "47"  ----  aportaciones remanente
   AND   estado = 2                                             

   ----------------------  IMPORTES EXTEMPO  -----------------------   --c22-11

   UPDATE dis_dep_iti
   SET    importe          = importe / 100,
          impt_aport_acept = acept_ext_viv + acept_ext_gar,            --c22-115
          impt_aport_dev   = recha_ext_viv + recha_ext_gar,            --c22-115
          part_viv_env     = part_viv_env / 1000000,                   --c22-115
          part_viv_acept   = part_viv_acept / 1000000,                 --c22-115
          part_viv_dev     = part_viv_dev / 1000000,                   --c22-115
          apli_rem_viv_env = 0,                                        --c22-11
          apli_rem_viv_ace = 0,                                        --c22-11
          apli_rem_viv_dev = 0,                                        --c22-11
          apli_ext_viv_env = apli_ext_viv_env / 1000000,               --c22-11
          apli_ext_viv_ace = acept_apli_ext_viv + acept_apli_ext_gar,  --c22-116
          apli_ext_viv_dev = recha_apli_ext_viv + recha_apli_ext_gar   --c22-115
   WHERE folio  = vfolio
   AND   ident_pago[14,15] = "48"  ----  aportaciones extempo  
   AND   estado = 2

   UPDATE dis_dep_iti
   SET    importe          = importe / 100,
          impt_aport_acept = 0,
          impt_aport_dev   = 0,
          part_viv_env     = part_viv_env / 1000000,
          part_viv_acept   = 0,
          part_viv_dev     = 0,
          apli_rem_viv_env = 0,
          apli_rem_viv_ace = 0,
          apli_rem_viv_dev = 0,
          apli_ext_viv_env = apli_ext_viv_env / 1000000,               --c22-11
          apli_ext_viv_ace = acept_int_ext_viv,                        --c22-11
          apli_ext_viv_dev = recha_int_ext_viv                         --c22-11
   WHERE folio  = vfolio
   AND   ident_pago[14,15] = "58"  ----  intereses extempo  
   AND   estado = 2

   CALL Genera_salidas(vfolio,
                       acept_rcv,
                       acept_vol,           --c22-6
                       acept_compl,         --c22-6
                       acept_viv,
                       acept_gub,
                       acept_viv_gar,
                       acept_part,          ----jerry
                       acept_rem_viv,       --c22-11
                       acept_apli_rem_viv,  --c22-11
                       acept_rem_gar,       --c22-11
                       acept_ext_viv,       --c22-11
                       acept_apli_ext_viv,  --c22-11
                       acept_ext_gar,       --c22-11
                       acept_aport_lplazo,  --c22-11
                       acept_aport_subadi,  --c22-11
                       acept_part_gar,      ----jerry
                       acept_int_rcv,
                       acept_int_vol,       --c22-6
                       acept_int_compl,     --c22-6
                       acept_int_viv,
                       acept_int_gub,
                       acept_int_vivg,
                       recha_rcv,
                       recha_vol,           --c22-6
                       recha_compl,         --c22-6
                       recha_int_rcv,
                       recha_viv,
                       recha_viv_gar,
                       recha_part,          ----jerry
                       recha_rem_viv,       --c22-11
                       recha_apli_rem_viv,  --c22-11
                       recha_ext_viv,       --c22-11
                       recha_apli_ext_viv,  --c22-11
                       recha_ext_gar,       --c22-11
                       recha_aport_lplazo,  --c22-11
                       recha_aport_subadi,  --c22-11
                       recha_part_gar,      ----jerry
                       recha_int_vol,       --c22-6
                       recha_int_compl,     --c22-6
                       recha_int_lplazo,    --c22-11
                       recha_int_subadi,    --c22-11
                       recha_int_viv,
                       recha_gub,
                       recha_int_gub,
                       recha_int_vivg)

   LET ejecuta= "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                "; cat cza_iti rech_iti dep_iti sum_iti > ",vnom_archivo CLIPPED
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; rm cza_iti rech_iti dep_iti sum_iti "
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 ";chmod 777 ",vnom_archivo CLIPPED
   RUN ejecuta
END FUNCTION


FUNCTION Genera_salidas(vfolio,
                        acept_rcv,
                        acept_vol,           --c22-6
                        acept_compl,         --c22-6
                        acept_viv,
                        acept_gub,
                        acept_viv_gar,
                        acept_part,          ----jerry
                        acept_rem_viv,       --c22-11
                        acept_apli_rem_viv,  --c22-11
                        acept_rem_gar,       --c22-11
                        acept_ext_viv,       --c22-11
                        acept_apli_ext_viv,  --c22-11
                        acept_ext_gar,       --c22-11
                        acept_aport_lplazo,  --c22-11
                        acept_aport_subadi,  --c22-11
                        acept_part_gar,      ----jerry
                        acept_int_rcv,
                        acept_int_vol,       --c22-6
                        acept_int_compl,     --c22-6
                        acept_int_viv,
                        acept_int_gub,
                        acept_int_vivg,
                        recha_rcv,
                        recha_vol,           --c22-6
                        recha_compl,         --c22-6
                        recha_int_rcv,
                        recha_viv,
                        recha_viv_gar,
                        recha_part,          ----jerry
                        recha_rem_viv,       --c22-11
                        recha_apli_rem_viv,  --c22-11
                        recha_ext_viv,       --c22-11
                        recha_apli_ext_viv,  --c22-11
                        recha_ext_gar,       --c22-11
                        recha_aport_lplazo,  --c22-11
                        recha_aport_subadi,  --c22-11
                        recha_part_gar,      ----jerry
                        recha_int_vol,       --c22-6
                        recha_int_compl,     --c22-6
                        recha_int_lplazo,    --c22-11
                        recha_int_subadi,    --c22-11
                        recha_int_viv,
                        recha_gub,
                        recha_int_gub,
                        recha_int_vivg)

   DEFINE 
      vfolio               INTEGER,
      acept_rcv            DECIMAL(16,2),
      acept_vol            DECIMAL(16,2),    --c22-6
      acept_compl          DECIMAL(16,2),    --c22-6
      acept_viv            DECIMAL(16,2),
      acept_gub            DECIMAL(16,2),
      acept_viv_gar        DECIMAL(16,2),
      acept_part           DECIMAL(18,6),    ----jerry
      acept_rem_viv        DECIMAL(16,2),    --c22-11
      acept_rem_gar        DECIMAL(16,2),    --c22-11
      acept_apli_rem_viv   DECIMAL(18,6),    --c22-11
      acept_ext_viv        DECIMAL(16,2),    --c22-11
      acept_apli_ext_viv   DECIMAL(18,6),    --c22-11
      acept_ext_gar        DECIMAL(16,2),    --c22-11
      acept_aport_lplazo   DECIMAL(16,2),    --c22-11
      acept_aport_subadi   DECIMAL(16,2),    --c22-11
      acept_int_rcv        DECIMAL(16,2),
      acept_int_vol        DECIMAL(16,2),    --c22-6
      acept_int_compl      DECIMAL(16,2),    --c22-6
      acept_int_viv        DECIMAL(16,2),
      acept_int_gub        DECIMAL(16,2),
      acept_int_vivg       DECIMAL(16,2),
      acept_part_gar       DECIMAL(18,6),    ----jerry
      recha_rcv            DECIMAL(16,2),
      recha_vol            DECIMAL(16,2),    --c22-6
      recha_compl          DECIMAL(16,2),    --c22-6
      recha_viv            DECIMAL(16,2),
      recha_gub            DECIMAL(16,2),
      recha_viv_gar        DECIMAL(16,2),
      recha_part           DECIMAL(18,6),    ----jerry
      recha_rem_viv        DECIMAL(16,2),    --c22-11
      recha_apli_rem_viv   DECIMAL(18,6),    --c22-11
      recha_ext_viv        DECIMAL(16,2),    --c22-11
      recha_apli_ext_viv   DECIMAL(18,6),    --c22-11
      recha_ext_gar        DECIMAL(16,2),    --c22-11
      recha_aport_lplazo   DECIMAL(16,2),    --c22-11
      recha_aport_subadi   DECIMAL(16,2),    --c22-11
      recha_int_rcv        DECIMAL(16,2),
      recha_int_vol        DECIMAL(16,2),    --c22-6
      recha_int_compl      DECIMAL(16,2),    --c22-6
      recha_int_lplazo     DECIMAL(16,2),    --c22-11 
      recha_int_subadi     DECIMAL(16,2),    --c22-11 
      recha_int_viv        DECIMAL(16,2),
      recha_int_gub        DECIMAL(16,2),
      recha_int_vivg       DECIMAL(16,2),
      recha_part_gar       DECIMAL(18,6)     ----jerry

   DEFINE reg_cza RECORD
      tipo_registro        CHAR(02),
      ident_servicio       CHAR(02),
      ident_operacion      CHAR(02),
      tipo_ent_origen      CHAR(02),
      clave_ent_origen     CHAR(03),
      tipo_ent_destino     CHAR(02),
      clave_ent_destino    CHAR(03),
      fech_creac_lote      CHAR(08),
      lote_del_dia         CHAR(03),
      mod_recp_envio       CHAR(02),
      fech_limite_resp     CHAR(08),
      result_operacion     CHAR(02),
      motivo_rechazo1      CHAR(03),
      motivo_rechazo2      CHAR(03),
      motivo_rechazo3      CHAR(03),
      filler               CHAR(271),
      ident_arch           CHAR(01)
   END RECORD

   DEFINE reg_rech RECORD
      tipo_registro        CHAR(02),
      ident_servicio       CHAR(02),
      consec_reg_lote      INTEGER,
      n_seguro             CHAR(11),
      n_rfc                CHAR(13),
      n_unico              CHAR(18),
      nom_trabajador       CHAR(50),
      periodo_pago         CHAR(06),
      fech_pago            CHAR(08),
      fech_valor_rcv       CHAR(08),
      fech_valor_viv       CHAR(08),
      ult_salario_diario   DECIMAL(9,2),
      folio_pago_sua       CHAR(06),
      reg_patronal_imss    CHAR(11),
      rfc_patron           CHAR(13),
      cve_ent_receptora    CHAR(03),
      dias_cotz_bimest     CHAR(02),
      dias_incap_bimest    CHAR(02),
      dias_ausent_bimest   CHAR(02),
      impt_ret             DECIMAL(16,2),
      impt_act_rec_ret     DECIMAL(16,2),
      impt_ces_vej         DECIMAL(16,2),
      impt_act_r_ces_vej   DECIMAL(16,2),
      impt_aport_vol       DECIMAL(16,2),
      impt_aport_compl     DECIMAL(16,2),
      impt_aport_pat       DECIMAL(16,2),
      impt_cuota_soc       DECIMAL(16,2),
      impt_aport_est       DECIMAL(16,2),
      impt_aport_esp       DECIMAL(16,2),
      impt_act_cuo_soc     DECIMAL(16,2),
      impt_act_aport_est   DECIMAL(16,2),
      impt_act_cuo_esp     DECIMAL(16,2),
      fecha_pago_gub       CHAR(08),
      ident_viv_garantia   CHAR(01),
      inte_ext_viv         DECIMAL(16,2),  --c22-11
      impt_aport_lplazo    DECIMAL(16,2),  --c22-11
      impt_aport_subadi    DECIMAL(16,2),  --c22-11
      filler1              CHAR(23),       --c22-114 ant 75
      result_operacion     CHAR(02),
      det_mot_rechazo1     CHAR(03),
      det_mot_rechazo2     CHAR(03),
      det_mot_rechazo3     CHAR(03)
   END RECORD

   DEFINE reg_dep RECORD
      tipo_registro        CHAR(02),
      ident_servicio       CHAR(02),
      ident_pago           CHAR(16),
      importe              DECIMAL(15,2),
      fech_liquidacion     DATE,
      impt_aport_acept     DECIMAL(15,2),
      impt_aport_dev       DECIMAL(15,2),
      part_viv_env         DECIMAL(22,6),  ----jerry
      part_viv_acept       DECIMAL(22,6),  ----jerry
      part_viv_dev         DECIMAL(22,6),  ----jerry
      apli_rem_viv_env     DECIMAL(22,6),  --c22-116
      apli_rem_viv_ace     DECIMAL(22,6),  --c22-116
      apli_rem_viv_dev     DECIMAL(22,6),  --c22-116
      apli_ext_viv_env     DECIMAL(22,6),  --c22-116
      apli_ext_viv_ace     DECIMAL(22,6),  --c22-116
      apli_ext_viv_dev     DECIMAL(22,6),  --c22-116
      ident_siefore        CHAR(08),       --c22-6
      tipo_siefore         SMALLINT        --c22-6 
   END RECORD

   DEFINE reg_sum RECORD
      tipo_registro        CHAR(02),
      ident_servicio       CHAR(02),
      ident_operacion      CHAR(02),
      tipo_ent_origen      CHAR(02),
      clave_ent_origen     CHAR(03),
      tipo_ent_destino     CHAR(02),
      clave_ent_destino    CHAR(03),
      fech_creac_lote      CHAR(08),
      lote_del_dia         CHAR(03),
      impt_total_rcv       DECIMAL(17,2),
      impt_total_int_rcv   DECIMAL(17,2),
      impt_tot_apo_vol     DECIMAL(17,2),  --c22-6
      impt_tot_int_vol     DECIMAL(17,2),  --c22-6
      impt_tot_apo_comp    DECIMAL(17,2),  --c22-6
      impt_tot_int_comp    DECIMAL(17,2),  --c22-6
      impt_tot_apo_lplazo  DECIMAL(17,2),  --c22-11
      impt_tot_int_lplazo  DECIMAL(17,2),  --c22-11
      impt_tot_apo_subadi  DECIMAL(17,2),  --c22-11
      impt_tot_int_subadi  DECIMAL(17,2),  --c22-11
      impt_total_patr      DECIMAL(17,2),
      impt_tatal_int_pat   DECIMAL(17,2),
      impt_total_guber     DECIMAL(17,2),
      impt_total_int_gub   DECIMAL(17,2),
      num_de_reg_aport     CHAR(08),
      num_cuenta_xpagar    CHAR(06),
      impt_total_xpagar    DECIMAL(17,2),
      impt_tot_viv_gar     DECIMAL(17,2),
      impt_tot_int_vgar    DECIMAL(17,2),
      impt_tot_part_viv    DECIMAL(22,6),  ----jerry
      tot_part_viv_gar     DECIMAL(22,6),  ----jerry
      impt_tot_rem_viv     DECIMAL(17,2),  --c22-11
      apli_tot_int_rem     DECIMAL(22,6),  --c22-11
      impt_tot_int_ext_viv DECIMAL(17,2),  --c22-11
      apli_tot_int_ext     DECIMAL(22,6),  --c22-11
      impt_tot_acl_ext     DECIMAL(17,2)   --c22-11
   -- filler               CHAR(88)        --c22-11
   END RECORD,

   vimpt_total_rcv         DECIMAL(17,2),
   vimpt_total_int_rcv     DECIMAL(17,2),
   vimpt_tot_apo_vol       DECIMAL(17,2),  --c22-6
   vimpt_tot_int_vol       DECIMAL(17,2),  --c22-6
   vimpt_tot_apo_comp      DECIMAL(17,2),  --c22-6
   vimpt_tot_int_comp      DECIMAL(17,2),  --c22-6
   vimpt_tot_apo_lplazo    DECIMAL(17,2),  --c22-11
   vimpt_tot_int_lplazo    DECIMAL(17,2),  --c22-11
   vimpt_tot_apo_subadi    DECIMAL(17,2),  --c22-11
   vimpt_tot_int_subadi    DECIMAL(17,2),  --c22-11
   vimpt_total_patr        DECIMAL(17,2),
   vimpt_total_int_pat     DECIMAL(17,2),
   vimpt_total_guber       DECIMAL(17,2),
   vimpt_total_int_gub     DECIMAL(17,2),
   vimpt_total_xpagar      DECIMAL(17,2),
   vimpt_tot_viv_gar       DECIMAL(17,2),
   vimpt_tot_int_vgar      DECIMAL(17,2),
   vimpt_tot_part_viv      DECIMAL(22,6),  ----jerry
   vtot_part_viv_gar       DECIMAL(22,6),  ----jerry
   vimpt_tot_rem_viv       DECIMAL(17,2),  --c22-11
   vapli_tot_int_rem       DECIMAL(22,6),  --c22-11
   vimpt_tot_int_ext_viv   DECIMAL(17,2),  --c22-11
   vapli_tot_int_ext       DECIMAL(17,2),  --c22-11
   vimpt_tot_acl_ext       DECIMAL(17,2),  --c22-11  

   c15_impt_total_rcv,
   c15_impt_total_int_rcv,
   c15_impt_aport_pat,
   c15_impt_total_int_pat,
   c15_impt_total_guber,
   c15_impt_total_int_gub,
   c15_impt_total_xpagar,
   c15_impt_tot_viv_gar,
   c15_impt_tot_int_vgar,

   c15_impt_aport_acept    CHAR(15),
   c16_impt_total_rcv,
   c16_impt_total_int_rcv,
   c16_impt_aport_pat,
   c16_impt_total_int_pat,
   c16_impt_total_guber,
   c16_impt_total_int_gub,
   c16_impt_total_xpagar,
   c16_impt_tot_viv_gar,
   c16_impt_tot_int_vgar,
   c16_impt_aport_acept    CHAR(16),

   renvio                  CHAR(60),
   vtotal_aport_rech       DECIMAL(15,2),
   vtotal_int_rech         DECIMAL(15,2),
   i_num_de_reg_aport      INTEGER

   DEFINE
      xident_siefore       CHAR(08),       --c22-6.6
      xtipo_siefore        SMALLINT,       --c22-6.6
      vimpt_acept          DECIMAL(15,2),  --c22-6.6
      vtipo_sie            SMALLINT        --c22-6.6

   DEFINE
      ident_pago_a         CHAR(16),
      importe_a            DECIMAL(16,6),
      apo_acept_a          DECIMAL(16,6),
      apo_recha_a          DECIMAL(16,6)

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
   FROM  dis_cza_iti
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
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/cza_iti"
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
      a.impt_aport_compl,    --c22-6
      a.impt_aport_pat,
      a.impt_cuota_soc,
      a.impt_aport_est,
      a.impt_aport_esp,
      a.impt_act_cuo_soc,
      a.impt_act_aport_est,
      a.impt_act_cuo_esp,
      a.fecha_pago_gub,
      a.ident_viv_garantia,
      a.inte_ext_viv,        --c22-11
      a.impt_aport_lplazo,   --c22-11
      a.impt_aport_subadi,   --c22-11
      a.filler1,             ----jerry
      a.result_operacion,
      a.det_mot_rechazo1,
      a.det_mot_rechazo2,
      a.det_mot_rechazo3
   FROM dis_det_iti a
   WHERE a.folio = vfolio
   AND a.result_operacion = "02"  --- quitar cuando 43 bis
   ORDER BY  a.consec_reg_lote,a.tipo_registro

   LET reg_rech.cve_ent_receptora = "001"
   LET reg_rech.result_operacion  = "00"
   LET reg_rech.det_mot_rechazo1  = "000"
   LET reg_rech.det_mot_rechazo2  = "000"
   LET reg_rech.det_mot_rechazo3  = "000"

   DISPLAY "GENERANDO RECH..." #---AT 12,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/rech_iti"
   START REPORT rep_rech TO vreporte
      FOREACH cur_rech INTO reg_rech.*
         OUTPUT TO REPORT rep_rech(reg_rech.*)
      END FOREACH
   FINISH REPORT rep_rech

   --------------- GENERA RESPUESTA DEPOSITOS ---------------------

   DECLARE cur_41 CURSOR FOR                                     --c22-6.6
   SELECT  ident_siefore,                                        --c22-6.6
           tipo_siefore                                          --c22-6.6
   FROM    dis_dep_iti                                           --c22-6.6
   WHERE   folio = vfolio                                        --c22-6.6
   AND     ident_pago[14,15] = "41"                              --c22-6.6

   FOREACH cur_41 INTO xident_siefore,xtipo_siefore              --c22-6.6
      SELECT impt_aport_acept,                                   --c22-6.6
             tipo_siefore                                        --c22-6.6
      INTO   vimpt_acept,                                        --c22-6.6
             vtipo_sie                                           --c22-6.6
      FROM   dis_dep_iti                                         --c22-6.6
      WHERE  folio = vfolio                                      --c22-6.6
      AND    ident_pago[14,15] = "11"                            --c22-6.6
      AND    tipo_siefore = 0                                    --c22-6.6

      IF SQLCA.SQLCODE = 0 THEN                                  --c22-6.6
         IF vimpt_acept = 0 AND vtipo_sie = 0 THEN               --c22-6.6
            UPDATE dis_dep_iti                                   --c22-6.6
            SET    tipo_siefore = xtipo_siefore,                 --c22-6.6
                   ident_siefore = xident_siefore                --c22-6.6
            WHERE  folio             = vfolio                    --c22-6.6
            AND    ident_pago[14,15] = "11"                      --c22-6.6
            AND    tipo_siefore      = 0                         --c22-6.6
         END IF                                                  --c22-6.6
      END IF                                                     --c22-6.6
   END FOREACH                                                   --c22-6.6

   DECLARE cur_42 CURSOR FOR                                     --c22-6.6
   SELECT ident_siefore,                                         --c22-6.6
          tipo_siefore                                           --c22-6.6
   FROM   dis_dep_iti                                            --c22-6.6
   WHERE  folio = vfolio                                         --c22-6.6
   AND    ident_pago[14,15] = "42"                               --c22-6.6

   FOREACH cur_42 INTO xident_siefore,xtipo_siefore              --c22-6.6
      SELECT impt_aport_acept,                                   --c22-6.6
             tipo_siefore                                        --c22-6.6
      INTO   vimpt_acept,                                        --c22-6.6
             vtipo_sie                                           --c22-6.6
      FROM   dis_dep_iti                                         --c22-6.6
      WHERE  folio = vfolio                                      --c22-6.6
      AND    ident_pago[14,15] = "12"                            --c22-6.6
      AND    tipo_siefore = 0                                    --c22-6.6

      IF SQLCA.SQLCODE = 0 THEN                                  --c22-6.6
         IF vimpt_acept = 0 AND vtipo_sie = 0 THEN               --c22-6.6
            UPDATE dis_dep_iti                                   --c22-6.6
            SET    tipo_siefore  = xtipo_siefore,                --c22-6.6
                   ident_siefore = xident_siefore                --c22-6.6
            WHERE  folio             = vfolio                    --c22-6.6
            AND    ident_pago[14,15] = "12"                      --c22-6.6
            AND    tipo_siefore      = 0                         --c22-6.6
         END IF                                                  --c22-6.6
      END IF                                                     --c22-6.6
   END FOREACH                                                   --c22-6.6

   DECLARE curare CURSOR FOR
   SELECT unique ident_pago, importe
   FROM  dis_dep_iti
   WHERE folio = vfolio

   FOREACH curare INTO ident_pago_a, importe_a
      SELECT sum(impt_aport_acept), sum(impt_aport_dev)
      INTO  apo_acept_a , apo_recha_a
      FROM  dis_dep_iti
      WHERE folio = vfolio
      AND   ident_pago = ident_pago_a

      SELECT siefore_cod,siefore_desc
      INTO  xtipo_siefore, xident_siefore
      FROM  tab_siefore
      WHERE afore_cod   = vcod_afore
      AND   siefore_cod = 1

      IF importe_a = 0 AND apo_acept_a = 0 AND apo_recha_a = 0 THEN
         UPDATE dis_dep_iti
         SET   tipo_siefore  = xtipo_siefore,
               ident_siefore = xident_siefore
         WHERE folio            = vfolio
         AND   impt_aport_dev   = 0
         AND   impt_aport_acept = 0
         AND   importe          = 0
         AND   ident_siefore    = ""
      END IF

      IF importe_a <> (apo_acept_a + apo_recha_a) THEN
         IF (apo_acept_a = 0 AND apo_recha_a = 0) AND (importe_a > 0 ) THEN
            UPDATE dis_dep_iti
            SET   importe        = importe/100,
                  impt_aport_dev = importe_a/100
            WHERE ident_pago = ident_pago_a
            AND   importe    = importe_a
         END IF
      END IF
   END FOREACH

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
      apli_rem_viv_env,  --c22-11
      apli_rem_viv_ace,  --c22-11
      apli_rem_viv_dev,  --c22-11
      apli_ext_viv_env,  --c22-11
      apli_ext_viv_ace,  --c22-11
      apli_ext_viv_dev,  --c22-11
      ident_siefore,     --c22-6
      tipo_siefore       --c22-6
   FROM  dis_dep_iti
   WHERE folio = vfolio

   DISPLAY "GENERANDO DEP....." #------AT 14,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/dep_iti"
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
          impt_tot_apo_vol,      --c22-6
          impt_tot_int_vol,      --c22-6
          impt_tot_apo_comp,     --c22-6
          impt_tot_int_comp,     --c22-6
          impt_tot_apo_lplazo,   --c22-11
          impt_tot_int_lplazo,   --c22-11
          impt_tot_apo_subadi,   --c22-11
          impt_tot_int_subadi,   --c22-11
          impt_total_patr,
          impt_tatal_int_pat,
          impt_total_guber,
          impt_total_int_gub,
          num_de_reg_aport,
          num_cuenta_xpagar,
          impt_total_xpagar,
          impt_tot_viv_gar,
          impt_tot_int_vgar,
          impt_tot_part_viv,     ----jerry
          tot_part_viv_gar,      ----jerry
          impt_tot_rem_viv,      --c22-11
          apli_tot_int_rem,      --c22-11
          impt_tot_int_ext_viv,  --c22-11
          apli_tot_int_ext,      --c22-11
          impt_tot_acl_ext       --c22-11
     --   filler                 --c22-11
   FROM   dis_sum_iti
   WHERE  folio = vfolio
   FOR UPDATE

   DISPLAY "GENERANDO SUM..." #----AT 16,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/sum_iti"
   START REPORT rep_sum TO vreporte
      FOREACH cur_sum INTO reg_sum.*

         LET reg_sum.tipo_ent_origen   = "01"
         LET reg_sum.clave_ent_origen  = vcod_afore
         LET reg_sum.tipo_ent_destino  = "03"
         LET reg_sum.clave_ent_destino = "001"
         LET reg_sum.fech_creac_lote   = reg_cza.fech_creac_lote
         LET reg_sum.lote_del_dia      = reg_cza.lote_del_dia
         LET reg_sum.num_de_reg_aport  = "00000000"

         LET vimpt_total_rcv     = reg_sum.impt_total_rcv     / 100
         LET vimpt_total_int_rcv = reg_sum.impt_total_int_rcv / 100

         LET vimpt_tot_apo_vol  = reg_sum.impt_tot_apo_vol  / 100       --c22-6
         LET vimpt_tot_int_vol  = reg_sum.impt_tot_int_vol  / 100       --c22-6
         LET vimpt_tot_apo_comp = reg_sum.impt_tot_apo_comp / 100       --c22-6
         LET vimpt_tot_int_comp = reg_sum.impt_tot_int_comp / 100       --c22-6

         LET vimpt_tot_apo_lplazo = reg_sum.impt_tot_apo_lplazo / 100   --c22-11
         LET vimpt_tot_int_lplazo = reg_sum.impt_tot_int_lplazo / 100   --c22-11
         LET vimpt_tot_apo_subadi = reg_sum.impt_tot_apo_subadi / 100   --c22-11
         LET vimpt_tot_int_subadi = reg_sum.impt_tot_int_subadi / 100   --c22-11

         LET vimpt_total_patr    = reg_sum.impt_total_patr    / 100
         LET vimpt_total_int_pat = reg_sum.impt_tatal_int_pat / 100
         LET vimpt_total_guber   = reg_sum.impt_total_guber   / 100
         LET vimpt_total_int_gub = reg_sum.impt_total_int_gub / 100
         LET vimpt_total_xpagar  = reg_sum.impt_total_xpagar  / 100
         LET vimpt_tot_viv_gar   = reg_sum.impt_tot_viv_gar   / 100
         LET vimpt_tot_int_vgar  = reg_sum.impt_tot_int_vgar  / 100

         LET vimpt_tot_part_viv  = reg_sum.impt_tot_part_viv / 1000000  ---jerry
         LET vtot_part_viv_gar   = reg_sum.tot_part_viv_gar / 1000000   ---jerry

         LET vimpt_tot_rem_viv   = reg_sum.impt_tot_rem_viv / 100       --c22-11
         LET vapli_tot_int_rem   = reg_sum.apli_tot_int_rem / 1000000   --c22-11
         LET vimpt_tot_int_ext_viv = reg_sum.impt_tot_int_ext_viv / 100 --c22-11
         LET vapli_tot_int_ext   = reg_sum.apli_tot_int_ext / 1000000   --c22-11
         LET vimpt_tot_acl_ext   = reg_sum.impt_tot_acl_ext / 100       --c22-11

         LET reg_sum.impt_total_rcv     = vimpt_total_rcv
         LET reg_sum.impt_total_int_rcv = vimpt_total_int_rcv

         LET reg_sum.impt_tot_apo_vol  = vimpt_tot_apo_vol              --c22-6
         LET reg_sum.impt_tot_int_vol  = vimpt_tot_int_vol              --c22-6
         LET reg_sum.impt_tot_apo_comp = vimpt_tot_apo_comp             --c22-6
         LET reg_sum.impt_tot_int_comp = vimpt_tot_int_comp             --c22-6

         LET reg_sum.impt_tot_apo_lplazo = vimpt_tot_apo_lplazo         --c22-11
         LET reg_sum.impt_tot_int_lplazo = vimpt_tot_int_lplazo         --c22-11
         LET reg_sum.impt_tot_apo_subadi = vimpt_tot_apo_subadi         --c22-11
         LET reg_sum.impt_tot_int_subadi = vimpt_tot_int_subadi         --c22-11

         LET reg_sum.impt_total_patr    = vimpt_total_patr
         LET reg_sum.impt_tatal_int_pat = vimpt_total_int_pat
         LET reg_sum.impt_total_guber   = vimpt_total_guber
         LET reg_sum.impt_total_int_gub = vimpt_total_int_gub
         LET reg_sum.impt_total_xpagar  = vimpt_total_xpagar
         LET reg_sum.impt_tot_viv_gar   = vimpt_tot_viv_gar
         LET reg_sum.impt_tot_int_vgar  = vimpt_tot_int_vgar
         LET reg_sum.impt_tot_rem_viv   = vimpt_tot_rem_viv             --c22-11
         LET reg_sum.apli_tot_int_rem   = vapli_tot_int_rem             --c22-11
         LET reg_sum.impt_tot_int_ext_viv = vimpt_tot_int_ext_viv       --c22-11
         LET reg_sum.apli_tot_int_ext   = vapli_tot_int_ext             --c22-11
         LET reg_sum.impt_tot_acl_ext   = vimpt_tot_acl_ext             --c22-11

         UPDATE dis_sum_iti
         SET    impt_total_rcv     = vimpt_total_rcv,
                impt_total_int_rcv = vimpt_total_int_rcv,
                impt_tot_apo_vol   = vimpt_tot_apo_vol,                 --c22-6
                impt_tot_int_vol   = vimpt_tot_int_vol,                 --c22-6
                impt_tot_apo_comp  = vimpt_tot_apo_comp,                --c22-6
                impt_tot_int_comp  = vimpt_tot_int_comp,                --c22-6

                impt_tot_apo_lplazo = vimpt_tot_apo_lplazo,             --c22-11
                impt_tot_int_lplazo = vimpt_tot_int_lplazo,             --c22-11
                impt_tot_apo_subadi = vimpt_tot_apo_subadi,             --c22-11
                impt_tot_int_subadi = vimpt_tot_int_subadi,             --c22-11

                impt_total_patr    = vimpt_total_patr,
                impt_tatal_int_pat = vimpt_total_int_pat,
  
                impt_total_guber   = vimpt_total_guber,
                impt_total_int_gub = vimpt_total_int_gub,
                impt_total_xpagar  = vimpt_total_xpagar,
                impt_tot_viv_gar   = vimpt_tot_viv_gar,
                impt_tot_int_vgar  = vimpt_tot_int_vgar,
                impt_tot_part_viv  = vimpt_tot_part_viv,               ----jerry
                tot_part_viv_gar   = vtot_part_viv_gar,                ----jerry
            
                impt_tot_rem_viv     = vimpt_tot_rem_viv,              --c22-11
                apli_tot_int_rem     = vapli_tot_int_rem,              --c22-11
                impt_tot_int_ext_viv = vimpt_tot_int_ext_viv,          --c22-11
                apli_tot_int_ext     = vapli_tot_int_ext,              --c22-11
                impt_tot_acl_ext     = vimpt_tot_acl_ext               --c22-11
         WHERE  CURRENT OF cur_sum

         OUTPUT TO REPORT rep_sum(reg_sum.*,
				  vfolio,
                                  acept_rcv,
                                  acept_vol,           --c22-6
                                  acept_compl,         --c22-6
                                  acept_viv,
                                  acept_gub,
                                  acept_viv_gar,
                                  acept_part,          ----jerry
                                  acept_rem_viv,       --c22-11
                                  acept_apli_rem_viv,  --c22-11
                                  acept_rem_gar,       --c22-11
                                  acept_ext_viv,       --c22-11
                                  acept_apli_ext_viv,  --c22-11
                                  acept_ext_gar,       --c22-11
                                  acept_aport_lplazo,  --c22-11
                                  acept_aport_subadi,  --c22-11
                                  acept_int_rcv,
                                  acept_int_vol,       --c22-6
                                  acept_int_compl,     --c22-6
                                  acept_int_viv,
                                  acept_int_gub,
                                  acept_int_vivg,
                                  acept_part_gar,      ----jerry
                                  recha_rcv,
                                  recha_vol,           --c22-6
                                  recha_compl,         --c22-6
                                  recha_int_rcv,
                                  recha_viv,
                                  recha_viv_gar,
                                  recha_part,          ----jerry
                                  recha_rem_viv,       --c22-11
                                  recha_apli_rem_viv,  --c22-11
                                  recha_ext_viv,       --c22-11
                                  recha_apli_ext_viv,  --c22-11
                                  recha_ext_gar,       --c22-11
                                  recha_aport_lplazo,  --c22-11
                                  recha_aport_subadi,  --c22-11
                                  recha_int_viv,
                                  recha_int_vol,       --c22-6
                                  recha_int_compl,     --c22-6
                                  recha_int_lplazo,    --c22-11
                                  recha_int_subadi,    --c22-11
                                  recha_gub,
                                  recha_int_gub,
                                  recha_int_vivg,
                                  recha_part_gar)      ----jerry
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
                        130 SPACES                 --c22-11
END REPORT


REPORT rep_rech(reg_rech)
   DEFINE
      reg_rech RECORD
         tipo_registro       CHAR(02),
         ident_servicio      CHAR(02),
         consec_reg_lote     INTEGER,
         n_seguro            CHAR(11),
         n_rfc               CHAR(13),
         n_unico             CHAR(18),
         nom_trabajador      CHAR(50),
         periodo_pago        CHAR(06),
         fech_pago           CHAR(08),
         fech_valor_rcv      CHAR(08),
         fech_valor_viv      CHAR(08),
         ult_salario_diario  DECIMAL(9,2),
         folio_pago_sua      CHAR(06),
         reg_patronal_imss   CHAR(11),
         rfc_patron          CHAR(13),
         cve_ent_receptora   CHAR(03),
         dias_cotz_bimest    CHAR(02),
         dias_incap_bimest   CHAR(02),
         dias_ausent_bimest  CHAR(02),
         impt_ret            DECIMAL(16,2),
         impt_act_rec_ret    DECIMAL(16,2),
         impt_ces_vej        DECIMAL(16,2),
         impt_act_r_ces_vej  DECIMAL(16,2),
         impt_aport_vol      DECIMAL(16,2),
         impt_aport_compl    DECIMAL(16,2),  --c22-6
         impt_aport_pat      DECIMAL(16,2),
         impt_cuota_soc      DECIMAL(16,2),
         impt_aport_est      DECIMAL(16,2),
         impt_aport_esp      DECIMAL(16,2),
         impt_act_cuo_soc    DECIMAL(16,2),
         impt_act_aport_est  DECIMAL(16,2),
         impt_act_cuo_esp    DECIMAL(16,2),
         fecha_pago_gub      CHAR(08), 
         ident_viv_garantia  CHAR(01),
         inte_ext_viv        DECIMAL(12,2),  --c22-11
         impt_aport_lplazo   DECIMAL(12,2),  --c22-11
         impt_aport_subadi   DECIMAL(12,2),  --c22-11
         filler1             CHAR(23),       --c22-114 ant 75
         result_operacion    CHAR(02),
         det_mot_rechazo1    CHAR(03),
         det_mot_rechazo2    CHAR(03),
         det_mot_rechazo3    CHAR(03)
      END RECORD,

   cconsec_reg_lote       CHAR(08),
   cult_salario_diario    CHAR(08),
   cimpt_ret              CHAR(08),
   cimpt_act_rec_ret      CHAR(08),
   cimpt_ces_vej          CHAR(08),
   cimpt_act_r_ces_vej    CHAR(08),
   cimpt_aport_vol        CHAR(08),
   cimpt_aport_compl      CHAR(08),
   cimpt_aport_pat        CHAR(08),
   cimpt_cuota_soc        CHAR(08),
   cimpt_aport_est        CHAR(08),
   cimpt_aport_esp        CHAR(08),
   cimpt_act_cuo_soc      CHAR(08),
   cimpt_act_aport_est    CHAR(08),
   cimpt_act_cuo_esp      CHAR(08),
   xconsec_reg_lote       CHAR(08),
   xult_salario_diario    CHAR(07),
   xdias_cotz_bimest      CHAR(02),
   xdias_incap_bimest     CHAR(02),
   xdias_ausent_bimest    CHAR(02),
   ximpt_ret              CHAR(07),
   ximpt_act_rec_ret      CHAR(07),
   ximpt_ces_vej          CHAR(07),
   ximpt_act_r_ces_vej    CHAR(07),
   ximpt_aport_vol        CHAR(07),
   ximpt_aport_compl      CHAR(07),
   ximpt_aport_pat        CHAR(07),
   ximpt_cuota_soc        CHAR(07),
   ximpt_aport_est        CHAR(07),
   ximpt_aport_esp        CHAR(07),
   ximpt_act_cuo_soc      CHAR(07),
   ximpt_act_aport_est    CHAR(07),
   ximpt_act_cuo_esp      CHAR(07),

   cconsec_reg_lotei      CHAR(08),
   cult_salario_diarioi   CHAR(08),
   cimpt_reti             CHAR(08),
   cimpt_act_rec_reti     CHAR(08),
   cimpt_ces_veji         CHAR(08),
   cimpt_act_r_ces_veji   CHAR(08),
   cimpt_aport_voli       CHAR(08),
   cimpt_aport_compli     CHAR(08),
   cimpt_aport_pati       CHAR(08),
   cimpt_cuota_soci       CHAR(08),
   cimpt_aport_esti       CHAR(08),
   cimpt_aport_espi       CHAR(08),
   cimpt_act_cuo_soci     CHAR(08),
   cimpt_act_aport_esti   CHAR(08),
   cimpt_act_cuo_espi     CHAR(08),
   cinte_ext_viv_acli     CHAR(08),
   cimpt_int_lplazoi      CHAR(08),
   cimpt_int_subadii      CHAR(08),

   xconsec_reg_lotei      CHAR(08),
   xult_salario_diarioi   CHAR(07),
   xdias_cotz_bimesti     CHAR(02),
   xdias_incap_bimesti    CHAR(02),
   xdias_ausent_bimesti   CHAR(02),
   ximpt_reti             CHAR(07),
   ximpt_act_rec_reti     CHAR(07),
   ximpt_ces_veji         CHAR(07),
   ximpt_act_r_ces_veji   CHAR(07),
   ximpt_aport_voli       CHAR(07),
   ximpt_aport_compli     CHAR(07),  --c22-6
   ximpt_aport_pati       CHAR(07),
   ximpt_cuota_soci       CHAR(07),
   ximpt_aport_esti       CHAR(07),
   ximpt_aport_espi       CHAR(07),
   ximpt_act_cuo_soci     CHAR(07),
   ximpt_act_aport_esti   CHAR(07),
   ximpt_act_cuo_espi     CHAR(07),
   xinte_ext_viv_acli     CHAR(07),  --c22-11
   ximpt_int_lplazoi      CHAR(07),  --c22-11
   ximpt_int_subadii      CHAR(07)   --c22-11

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

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

LET ximpt_aport_compl = cimpt_aport_compl[1,05],cimpt_aport_compl[07,08]

LET ximpt_aport_pat     = cimpt_aport_pat    [1,05],cimpt_aport_pat    [07,08]
LET ximpt_cuota_soc     = cimpt_cuota_soc    [1,05],cimpt_cuota_soc    [07,08]
LET ximpt_aport_est     = cimpt_aport_est    [1,05],cimpt_aport_est    [07,08]
LET ximpt_aport_esp     = cimpt_aport_esp    [1,05],cimpt_aport_esp    [07,08]
LET ximpt_act_cuo_soc   = cimpt_act_cuo_soc  [1,05],cimpt_act_cuo_soc  [07,08]
LET ximpt_act_aport_est = cimpt_act_aport_est[1,05],cimpt_act_aport_est[07,08]
LET ximpt_act_cuo_esp   = cimpt_act_cuo_esp  [1,05],cimpt_act_cuo_esp  [07,08]

--c22-11

LET xult_salario_diarioi= cult_salario_diarioi[1,05], cult_salario_diarioi[7,08]
LET ximpt_reti          = cimpt_reti          [1,05],cimpt_reti          [07,08]
LET ximpt_act_rec_reti  = cimpt_act_rec_reti  [1,05],cimpt_act_rec_reti  [07,08]
LET ximpt_ces_veji      = cimpt_ces_veji      [1,05],cimpt_ces_veji      [07,08]
LET ximpt_act_r_ces_veji= cimpt_act_r_ces_veji[1,05],cimpt_act_r_ces_veji[07,08]
LET ximpt_aport_voli    = cimpt_aport_voli    [1,05],cimpt_aport_voli    [07,08]

LET ximpt_aport_compli = cimpt_aport_compli[1,05],cimpt_aport_compli[07,08]

LET ximpt_aport_pati    = cimpt_aport_pati    [1,05],cimpt_aport_pati    [07,08]
LET ximpt_cuota_soci    = cimpt_cuota_soci    [1,05],cimpt_cuota_soci    [07,08]
LET ximpt_aport_esti    = cimpt_aport_esti    [1,05],cimpt_aport_esti    [07,08]
LET ximpt_aport_espi    = cimpt_aport_espi    [1,05],cimpt_aport_espi    [07,08]
LET ximpt_act_cuo_soci  = cimpt_act_cuo_soci  [1,05],cimpt_act_cuo_soci  [07,08]
LET ximpt_act_aport_esti= cimpt_act_aport_esti[1,05],cimpt_act_aport_esti[07,08]
LET ximpt_act_cuo_espi  = cimpt_act_cuo_espi  [1,05],cimpt_act_cuo_espi  [07,08]

--c22-11
LET xinte_ext_viv_acli   =cinte_ext_viv_acli   [01,05],
                          cinte_ext_viv_acli   [07,08]              
LET ximpt_int_lplazoi    =cimpt_int_lplazoi    [01,05],
                          cimpt_int_lplazoi    [07,08]
LET ximpt_int_subadii    =cimpt_int_subadii    [01,05],
                          cimpt_int_subadii    [07,08]
--c22-11
         PRINT
            COLUMN 1,reg_rech.tipo_registro,
                     reg_rech.ident_servicio,
                     xconsec_reg_lote,
                     reg_rech.n_seguro,
                     reg_rech.n_rfc,
                     reg_rech.n_unico,
                     reg_rech.nom_trabajador,
                     reg_rech.periodo_pago,
                     reg_rech.fech_pago,
                     reg_rech.fech_valor_rcv,
                     reg_rech.fech_valor_viv,
                     xult_salario_diario,
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
                     ximpt_aport_compl,
                     ximpt_aport_pat,
                     ximpt_cuota_soc,
                     ximpt_aport_est,
                     ximpt_aport_esp,
                     ximpt_act_cuo_soc,
                     ximpt_act_aport_est,
                     ximpt_act_cuo_esp,
                     reg_rech.fecha_pago_gub,
                     reg_rech.ident_viv_garantia,
                     
                     reg_rech.inte_ext_viv USING '&&&&&&&',             --c22-11
                     reg_rech.impt_aport_lplazo USING '&&&&&&&',        --c22-11
                     reg_rech.impt_aport_subadi USING '&&&&&&&',        --c22-11
                     reg_rech.filler1,
                     "00",                      --- result_operacion,
                     reg_rech.det_mot_rechazo1,
                     reg_rech.det_mot_rechazo2,
                     reg_rech.det_mot_rechazo3,
                     115 SPACES
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
      part_viv_env      DECIMAL(22,6),  ----jerry
      part_viv_acept    DECIMAL(22,6),  ----jerry
      part_viv_dev      DECIMAL(22,6),  ----jerry
      apli_rem_viv_env  DECIMAL(22,6),  --c22-11
      apli_rem_viv_ace  DECIMAL(22,6),  --c22-11
      apli_rem_viv_dev  DECIMAL(22,6),  --c22-11
      apli_ext_viv_env  DECIMAL(22,6),  --c22-116
      apli_ext_viv_ace  DECIMAL(22,6),  --c22-116
      apli_ext_viv_dev  DECIMAL(22,6),  --c22-116
      ident_siefore     CHAR(08),       --c22-6
      tipo_siefore      SMALLINT        --c22-6
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
            LET c15_impt_aport_dev = c16_impt_aport_dev[01,13],
                                     c16_impt_aport_dev[15,16]
         END IF

         LET xfecha_liquidacion = reg_dep.fech_liquidacion
         LET vfecha_liquidacion = xfecha_liquidacion[7,10],
                                  xfecha_liquidacion[1,02],
                                  xfecha_liquidacion[4,05]

         LET reg_dep.part_viv_env   = reg_dep.part_viv_env   * 1000000 ----jerry
         LET reg_dep.part_viv_acept = reg_dep.part_viv_acept * 1000000 ----jerry
         LET reg_dep.part_viv_dev   = reg_dep.part_viv_dev   * 1000000 ----jerry

         LET reg_dep.apli_rem_viv_env = reg_dep.apli_rem_viv_env * 1000000 --c22
         LET reg_dep.apli_rem_viv_ace = reg_dep.apli_rem_viv_ace * 1000000 --c22
         LET reg_dep.apli_rem_viv_dev = reg_dep.apli_rem_viv_dev * 1000000 --c22

         LET reg_dep.apli_ext_viv_env = reg_dep.apli_ext_viv_env * 1000000 --c22
         LET reg_dep.apli_ext_viv_ace = reg_dep.apli_ext_viv_ace * 1000000 --c22
         LET reg_dep.apli_ext_viv_dev = reg_dep.apli_ext_viv_dev * 1000000 --c22

         PRINT COLUMN 01,reg_dep.tipo_registro,
                         reg_dep.ident_servicio,
                         reg_dep.ident_pago,
                         c15_importe,
                         vfecha_liquidacion,
                         c15_impt_aport_acept,
                         c15_impt_aport_dev,
                         reg_dep.part_viv_env     USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.part_viv_acept   USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.part_viv_dev     USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.apli_rem_viv_env USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.apli_rem_viv_ace USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.apli_rem_viv_dev USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.apli_ext_viv_env USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.apli_ext_viv_ace USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.apli_ext_viv_dev USING '&&&&&&&&&&&&&&&&&&',
                         reg_dep.ident_siefore,                   --c22-6
                         reg_dep.tipo_siefore     USING '&&&',    --c22-6
                         204 space                                --c22-11
END REPORT 


REPORT rep_sum(reg_sum,
	       vfolio,
               acept_rcv,
               acept_vol,           --c22-6
               acept_compl,         --c22-6
               acept_viv,
               acept_gub,
               acept_viv_gar,
               acept_part,          ----jerry
               acept_rem_viv,       --c22-11
               acept_apli_rem_viv,  --c22-11
               acept_rem_gar,       --c22-11
               acept_ext_viv,       --c22-11
               acept_apli_ext_viv,  --c22-11
               acept_ext_gar,       --c22-11
               acept_aport_lplazo,  --c22-11
               acept_aport_subadi,  --c22-11
               acept_int_rcv,
               acept_int_vol,       --c22-6
               acept_int_compl,     --c22-6
               acept_int_viv,
               acept_int_gub,
               acept_int_vivg,
               acept_part_gar,      ----jerry
               recha_rcv,
               recha_vol,           --c22-6
               recha_compl,         --c22-6
               recha_int_rcv,
               recha_viv,
               recha_viv_gar,
               recha_part,          ----jerry
               recha_rem_viv,       --c22-11
               recha_apli_rem_viv,  --c22-11
               recha_ext_viv,       --c22-11
               recha_apli_ext_viv,  --c22-11
               recha_ext_gar,       --c22-11
               recha_aport_lplazo,  --c22-11
               recha_aport_subadi,  --c22-11
               recha_int_viv,
               recha_int_vol,       --c22-6
               recha_int_compl,     --c22-6
               recha_int_lplazo,    --c22-11
               recha_int_subadi,    --c22-11
               recha_gub,
               recha_int_gub,
               recha_int_vivg,
               recha_part_gar)      ----jerry

   DEFINE
      vfolio               INTEGER,
      acept_rcv            DECIMAL(16,2),
      acept_vol            DECIMAL(16,2),  --c22-6
      acept_compl          DECIMAL(16,2),  --c22-6
      acept_viv            DECIMAL(16,2),
      acept_gub            DECIMAL(16,2),
      acept_viv_gar        DECIMAL(16,2),
      acept_part           DECIMAL(18,6),  ----jerry
      acept_rem_viv        DECIMAL(16,2),  --c22-11
      acept_apli_rem_viv   DECIMAL(18,6),  --c22-11
      acept_rem_gar        DECIMAL(16,2),  --c22-11
      acept_ext_viv        DECIMAL(16,2),  --c22-11
      acept_apli_ext_viv   DECIMAL(18,6),  --c22-11
      acept_ext_gar        DECIMAL(16,2),  --c22-11
      acept_aport_lplazo   DECIMAL(16,2),  --c22-11
      acept_aport_subadi   DECIMAL(16,2),  --c22-11
                   
      acept_int_rcv        DECIMAL(16,2),
      acept_int_vol        DECIMAL(16,2),  --c22-6
      acept_int_compl      DECIMAL(16,2),  --c22-6
      acept_int_viv        DECIMAL(16,2),
      acept_int_gub        DECIMAL(16,2),
      acept_int_vivg       DECIMAL(16,2),
      acept_part_gar       DECIMAL(18,6),  ----jerry
      recha_rcv            DECIMAL(16,2),
      recha_vol            DECIMAL(16,2),  --c22-6
      recha_compl          DECIMAL(16,2),  --c22-6
      recha_viv            DECIMAL(16,2),
      recha_viv_gar        DECIMAL(16,2),
      recha_part           DECIMAL(18,6),  ----jerry
      recha_rem_viv        DECIMAL(16,2),  --c22-11
      recha_apli_rem_viv   DECIMAL(18,6),  --c22-11
      recha_ext_viv        DECIMAL(16,2),  --c22-11
      recha_apli_ext_viv   DECIMAL(18,6),  --c22-11
      recha_ext_gar        DECIMAL(16,2),  --c22-11
      recha_aport_lplazo   DECIMAL(16,2),  --c22-11
      recha_aport_subadi   DECIMAL(16,2),  --c22-11
      recha_gub            DECIMAL(16,2),
      recha_int_rcv        DECIMAL(16,2),
      recha_int_vol        DECIMAL(16,2),  --c22-6
      recha_int_compl      DECIMAL(16,2),  --c22-6
      recha_int_lplazo     DECIMAL(16,2),  --c22-11
      recha_int_subadi     DECIMAL(16,2),  --c22-11
      recha_int_viv        DECIMAL(16,2),
      recha_int_gub        DECIMAL(16,2),
      recha_int_vivg       DECIMAL(16,2),
      recha_part_gar       DECIMAL(18,6)   ----jerry

   DEFINE reg_sum RECORD          
      tipo_registro        CHAR(02),
      ident_servicio       CHAR(02),
      ident_operacion      CHAR(02),
      tipo_ent_origen      CHAR(02),
      clave_ent_origen     CHAR(03),
      tipo_ent_destino     CHAR(02),
      clave_ent_destino    CHAR(03),
      fech_creac_lote      CHAR(08),
      lote_del_dia         CHAR(03),
      impt_total_rcv       DECIMAL(17,2),
      impt_total_int_rcv   DECIMAL(17,2),
      impt_tot_apo_vol     DECIMAL(17,2),  --c22-6
      impt_tot_int_vol     DECIMAL(17,2),  --c22-6
      impt_tot_apo_comp    DECIMAL(17,2),  --c22-6
      impt_tot_int_comp    DECIMAL(17,2),  --c22-6
      impt_tot_apo_lplazo  DECIMAL(17,2),  --c22-11
      impt_tot_int_lplazo  DECIMAL(17,2),  --c22-11
      impt_tot_apo_subadi  DECIMAL(17,2),  --c22-11
      impt_tot_int_subadi  DECIMAL(17,2),  --c22-11
      impt_total_patr      DECIMAL(17,2),
      impt_tatal_int_pat   DECIMAL(17,2),
      impt_total_guber     DECIMAL(17,2),
      impt_total_int_gub   DECIMAL(17,2),
      num_de_reg_aport     CHAR(08),
      num_cuenta_xpagar    CHAR(06),
      impt_total_xpagar    DECIMAL(17,2),
      impt_tot_viv_gar     DECIMAL(17,2),
      impt_tot_int_vgar    DECIMAL(17,2),
      impt_tot_part_viv    DECIMAL(22,6),  ----jerry
      tot_part_viv_gar     DECIMAL(22,6),  ----jerry
      impt_tot_rem_viv     DECIMAL(17,2),  --c22-11
      apli_tot_int_rem     DECIMAL(18,6),  --c22-11
      impt_tot_int_ext_viv DECIMAL(17,2),  --c22-11
      apli_tot_int_ext     DECIMAL(17,2),  --c22-11
      impt_tot_acl_ext     DECIMAL(17,2)   --c22-11
  --  filler               CHAR(88)        --c22-11
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
   c15_impt_aport_acept    CHAR(15),

   c16_impt_total_rcv,
   c16_impt_total_int_rcv,
   c16_impt_aport_pat,
   c16_impt_total_int_pat,
   c16_impt_total_guber,
   c16_impt_total_int_gub,
   c16_impt_total_xpagar,
   c16_impt_tot_viv_gar,
   c16_impt_tot_int_vgar,
   c16_impt_aport_acept    CHAR(16),

   renvio                  CHAR(60),
   vtotal_aport_rech       DECIMAL(15,2),
   vtotal_int_rech         DECIMAL(15,2),

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
   d13_impt_aport_acept    DECIMAL(15,2)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         LET reg_sum.impt_tot_part_viv = recha_part      * 1000000     ----jerry
         LET reg_sum.tot_part_viv_gar  = recha_part_gar  * 1000000     ----jerry

	 SELECT impt_aport_dev,apli_rem_viv_dev
	 INTO   recha_rem_viv,recha_apli_rem_viv
	 FROM   dis_dep_iti
	 WHERE  folio = vfolio
         AND    ident_pago[14,15] = "47"

         LET reg_sum.impt_tot_rem_viv  = recha_rem_viv      * 100       --c22-11
         LET reg_sum.apli_tot_int_rem  = recha_apli_rem_viv * 1000000   --c22-11

	 SELECT impt_aport_dev,apli_ext_viv_dev
	 INTO   recha_ext_viv,recha_apli_ext_viv
	 FROM   dis_dep_iti
	 WHERE  folio = vfolio
         AND    ident_pago[14,15] = "48"

         LET reg_sum.impt_tot_int_ext_viv = recha_ext_viv   * 100       --c22-11
         LET reg_sum.apli_tot_int_ext  = recha_apli_ext_viv * 1000000   --c22-11

         LET reg_sum.impt_tot_acl_ext  = 0                         --c22-11 duda
           
         LET reg_sum.impt_tot_apo_vol  = recha_vol       * 100           --c22-6
         LET reg_sum.impt_tot_int_vol  = recha_int_vol   * 100           --c22-6
         LET reg_sum.impt_tot_apo_comp = recha_compl     * 100           --c22-6
         LET reg_sum.impt_tot_int_comp = recha_int_compl * 100           --c22-6

         LET reg_sum.impt_tot_apo_lplazo = recha_aport_lplazo * 100     --c22-11
         LET reg_sum.impt_tot_int_lplazo = recha_int_lplazo   * 100     --c22-11
         LET reg_sum.impt_tot_apo_subadi = recha_aport_subadi * 100     --c22-11
         LET reg_sum.impt_tot_int_subadi = recha_int_subadi   * 100     --c22-11

         LET d15_impt_total_rcv     = recha_rcv
         LET d15_impt_total_int_rcv = recha_int_rcv
         LET d15_impt_aport_pat     = recha_viv
         LET d15_impt_total_int_pat = recha_int_viv
         LET d15_impt_total_guber   = recha_gub
         LET d15_impt_total_int_gub = recha_int_gub

{DMR     LET d15_impt_total_xpagar  = acept_rcv     + acept_viv      + --c22-6
                                      acept_gub     + acept_int_rcv  + --c22-6
                                      acept_int_viv + acept_int_gub  + --c22-6
                                      acept_viv_gar + acept_int_vivg + --c22-6
                                      acept_vol     + acept_compl    + --c22-6
                                      acept_int_vol + acept_int_compl+ --c22-6
                                      acept_rem_viv + acept_rem_gar  + --c22-112
                                      acept_ext_viv + acept_ext_gar  + --c22-112
                                      acept_aport_lplazo             + --c22-112
                                      acept_aport_subadi               --c22-112
DMR}

         LET d15_impt_total_xpagar  = acept_rcv     +                  --c22-6
                                      acept_gub     + acept_int_rcv  + --c22-6
                                      acept_int_gub +                  --c22-6
                                      acept_vol     + acept_compl    + --c22-6
                                      acept_int_vol + acept_int_compl+ --c22-6
                                      acept_aport_lplazo             + --c22-112
                                      acept_aport_subadi               --c22-112

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
       LET c16_impt_total_int_rcv=d15_impt_total_int_rcv USING"&&&&&&&&&&&&&.&&"
       LET c15_impt_total_int_rcv=c16_impt_total_int_rcv[01,13],
                                  c16_impt_total_int_rcv[15,16]
    END IF

    IF d15_impt_aport_pat IS NULL  THEN
       LET c15_impt_aport_pat = "000000000000000"
    ELSE
       LET c16_impt_aport_pat = d15_impt_aport_pat USING"&&&&&&&&&&&&&.&&"
       LET c15_impt_aport_pat = c16_impt_aport_pat[1,13],
                                c16_impt_aport_pat[15,16]
    END IF

    IF d15_impt_total_int_pat IS NULL THEN
        LET c15_impt_total_int_pat = "000000000000000"
    ELSE
       LET c16_impt_total_int_pat=d15_impt_total_int_pat USING"&&&&&&&&&&&&&.&&"
       LET c15_impt_total_int_pat=c16_impt_total_int_pat[1,13],
                                  c16_impt_total_int_pat[15,16]
    END IF

    IF d15_impt_tot_viv_gar IS NULL  THEN
       LET c15_impt_tot_viv_gar = "000000000000000"
    ELSE
       LET c16_impt_tot_viv_gar = d15_impt_tot_viv_gar USING"&&&&&&&&&&&&&.&&"
       LET c15_impt_tot_viv_gar = c16_impt_tot_viv_gar[1,13],
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
       LET c16_impt_total_int_gub=d15_impt_total_int_gub USING"&&&&&&&&&&&&&.&&"
       LET c15_impt_total_int_gub=c16_impt_total_int_gub[1,13],
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
       LET c15_impt_aport_acept = "000000000000000"
    ELSE
       LET c16_impt_aport_acept = d15_impt_aport_acept USING"&&&&&&&&&&&&&.&&"
       LET c15_impt_aport_acept = c16_impt_aport_acept[1,13],
                                  c16_impt_aport_acept[15,16]
    END IF

    DISPLAY 'TIPO ARCHIVO :', tipo_archivo   --@
    IF tipo_archivo = '2' THEN    ---- ACLARACIONES ESPECIALES
    ELSE
       LET c15_impt_total_int_rcv = '000000000000000'                    --c22-6
       LET c15_impt_total_int_pat = '000000000000000'                    --c22-6
       LET c15_impt_total_int_gub = '000000000000000'                    --c22-6
       LET c15_impt_tot_int_vgar  = '000000000000000'                    --c22-6
    END IF

    PRINT COLUMN 1, reg_sum.tipo_registro,
                    reg_sum.ident_servicio,
                    reg_sum.ident_operacion,
                    reg_sum.tipo_ent_origen,
                    vcod_afore                  USING "&&&",
                    reg_sum.tipo_ent_destino,
                    reg_sum.clave_ent_destino,
                    reg_sum.fech_creac_lote,
                    reg_sum.lote_del_dia,
                    c15_impt_total_rcv,
                    c15_impt_total_int_rcv,
                    reg_sum.impt_tot_apo_vol    USING '&&&&&&&&&&&&&&&', --c22-6
                    reg_sum.impt_tot_int_vol    USING '&&&&&&&&&&&&&&&', --c22-6
                    reg_sum.impt_tot_apo_comp   USING '&&&&&&&&&&&&&&&', --c22-6
                    reg_sum.impt_tot_int_comp   USING '&&&&&&&&&&&&&&&', --c22-6
                    reg_sum.impt_tot_apo_lplazo USING '&&&&&&&&&&&&&&&', --c211
                    reg_sum.impt_tot_int_lplazo USING '&&&&&&&&&&&&&&&', --c211
                    reg_sum.impt_tot_apo_subadi USING '&&&&&&&&&&&&&&&', --c211
                    reg_sum.impt_tot_int_subadi USING '&&&&&&&&&&&&&&&', --c211
                    c15_impt_aport_pat,
                    c15_impt_total_int_pat,
                    c15_impt_total_guber,
                    c15_impt_total_int_gub,
                    vnum_reg                    USING "&&&&&&&&",
-----                    reg_sum.num_de_reg_aport,
                    vnum_ctas_xpagar            USING '&&&&&&',        --c22-6.5
-----                    reg_sum.num_cuenta_xpagar,                    --c22-6.5
                    c15_impt_total_xpagar,
                    c15_impt_tot_viv_gar,
                    c15_impt_tot_int_vgar,
                    reg_sum.impt_tot_part_viv USING '&&&&&&&&&&&&&&&&&&',  --jer
                    reg_sum.tot_part_viv_gar  USING '&&&&&&&&&&&&&&&&&&',  --jer
                    reg_sum.impt_tot_rem_viv  USING '&&&&&&&&&&&&&&&',     --c21
                    reg_sum.apli_tot_int_rem  USING '&&&&&&&&&&&&&&&&&&',  --c21
                    reg_sum.impt_tot_int_ext_viv USING '&&&&&&&&&&&&&&&',  --c21
                    reg_sum.apli_tot_int_ext  USING '&&&&&&&&&&&&&&&&&&',  --c21
                    reg_sum.impt_tot_acl_ext  USING '&&&&&&&&&&&&&&&',     --c21
                    37 space                                               --c21
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
   SET   folio      = v_folio,
         estado_cod = 4,
         fecha_fin  = CURRENT
   WHERE pid         = reg_ident.pid
   AND   proceso_cod = reg_ident.proceso_cod
   AND   opera_cod   = reg_ident.opera_cod

   UPDATE bat_tmp_predecesor
   SET   bandera_ejecuta = 1
   WHERE pid_prod         = reg_ident.pid
   AND   proceso_cod_prod = reg_ident.proceso_cod
   AND   opera_cod_prod   = reg_ident.opera_cod

   LET v_fecha_log  = CURRENT
   LET vv_fecha_log = v_fecha_log
   
   SELECT A.programa_cod 
   INTO   vv_prog 
   FROM   bat_ctr_operacion A
   WHERE  A.pid         = reg_ident.pid
   AND    A.proceso_cod = reg_ident.proceso_cod
   AND    A.opera_cod   = reg_ident.opera_cod

   LET paso = "nohup:",
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


FUNCTION Provisiones(folio_provs)
   DEFINE folio_provs  INTEGER

   INSERT INTO dis_provision
   SELECT * FROM safre_tmp:dis_prov_iti
   WHERE folio = folio_provs

   DELETE FROM safre_tmp:dis_prov_iti
   WHERE folio = folio_provs
END FUNCTION

