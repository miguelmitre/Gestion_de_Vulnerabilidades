#*********************************************************************#
#Proyecto          => Sistema de safre_af.( MEXICO )                  #
#Propietario       => E.F.P.                                          #
#Programa          => EXCB008                                         #
#Descripcion       => DEVOLUCION DE PAGOS EN EXCESO ISSSTE PROVISION  #
#Fecha Inicio      => 18 ENERO 2009                                   #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                 #
#                     STEFANIE DANIELA VERA PIÑA                      #
#Sistema           => EXC.                                            #
#*********************************************************************#
DATABASE safre_af

GLOBALS

   DEFINE g_bat  RECORD LIKE dis_ctrl_proceso.*

   DEFINE gparam_dev  RECORD LIKE seg_modulo.*

   DEFINE g_det_imss  RECORD LIKE exc_det_exceso.*

   DEFINE g_det_isss  RECORD LIKE exc_det_exceso_issste.*

   DEFINE g_dep RECORD
      ident_pago             CHAR(2),
      monto_soli_instituto   DECIMAL(16,6),
      monto_par_vivienda     DECIMAL(18,6),
      codigo_siefore         SMALLINT
   END RECORD

   DEFINE g_sum  RECORD
      monto_tot_rcv          DECIMAL(16,6),
      monto_tot_plu_rcv      DECIMAL(16,6),
      monto_tot_min_rcv      DECIMAL(16,6),
      monto_tot_com_rcv      DECIMAL(16,6),
      monto_tot_pat          DECIMAL(16,6),
      monto_tot_gub          DECIMAL(16,6),
      monto_tot_plu_gub      DECIMAL(16,6),
      monto_tot_min_gub      DECIMAL(16,6),
      monto_tot_com_gub      DECIMAL(16,6),
      monto_tot_plu_pat      DECIMAL(16,6),
      monto_tot_par_viv      DECIMAL(18,6)
   END RECORD                    

   DEFINE reg_contig RECORD 
      folio                INTEGER,
      estado               SMALLINT,                               
      fecha_estado         DATE,                                   
      hora_estado          CHAR(8),                                
      tipo_registro        CHAR(2),                                
      ident_servicio       CHAR(2),                                
      consec_reg_lote      INTEGER,                                
      nss                  CHAR(11),                               
      clave_ent_orig       CHAR(3),                                
      rfc                  CHAR(13),                               
      curp                 CHAR(18),                               
      nom_trabajador       CHAR(50),                               
      periodo_pago         CHAR(6),                                
      fecha_pago           DATE,              
      fecha_valor_rcv      DATE,                                   
      fecha_valor_viv      DATE,                                   
      folio_pago_sua       CHAR(6),                                
      clave_ent_recep      CHAR(3),                                
      reg_patronal_imss    CHAR(11),                               
      monto_ret            DECIMAL(16,6),                          
      monto_act_ret        DECIMAL(16,6),                          
      monto_ces_vej_pat    DECIMAL(16,6),                          
      monto_act_cv_pat     DECIMAL(16,6),                          
      monto_ces_vej_tra    DECIMAL(16,6),                          
      monto_act_cv_tra     DECIMAL(16,6),                          
      monto_act_ces_vej    DECIMAL(16,6),                          
      monto_cuo_soc        DECIMAL(16,6),                          
      monto_aport_est      DECIMAL(16,6),                          
      monto_aport_esp      DECIMAL(16,6),                          
      monto_aport_pat      DECIMAL(16,6),                          
      dias_cotz_bimestre   SMALLINT,                               
      monto_par_viv        DECIMAL(18,6),                          
      mto_act_ret_rend     DECIMAL(16,6),                          
      mto_act_cv_rend_pat  DECIMAL(16,6),                          
      mto_act_cv_rend_tra  DECIMAL(16,6),                          
      fecha_envio_orig     DATE,                                   
      result_operacion     CHAR(2),                                
      tipo_diagnostico     CHAR(3)                  
   END RECORD 
   
   DEFINE
      diag_dev               CHAR(2),
      ejecuta_procedure      CHAR(500),
      ejecuta_marca          CHAR(200),
      ejecuta_desmarca       CHAR(200),
      ejecuta_rev_marca      CHAR(200),
      vhora_final            CHAR(08),
      vresultado             CHAR(50),
      vproc                  CHAR(06),
      vpar                   CHAR(06),
      vrech                  CHAR(06),
      vpend                  CHAR(06),
      generar                CHAR(25),
      gusuario               CHAR(08),
      vnom_archivo           CHAR(21),
      vreporte               CHAR(300),
      vfecha_lote            CHAR(08),
      vtipo_reporte          CHAR(01),
      hora_inicial           CHAR(08),
      hora_final             CHAR(08),
      cla_sel                CHAR(500),
      log1                   CHAR(40)

   DEFINE
      hoy                    DATE

   DEFINE
      precio_dia             DECIMAL(16,6)

   DEFINE
      vconsecutivo           ,
      vcont_rech             ,
      vcont_acep             ,
      vcont_par              ,
      vcont_pend             ,
      vfolio                 INTEGER

   DEFINE
      vetapa_cod             ,
      vtipo_arch             ,
      x_afore_local          SMALLINT

END GLOBALS


MAIN

   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   LET vtipo_arch = ARG_VAL(1)

   SELECT codigo_afore
   INTO   x_afore_local
   FROM   tab_afore_local
   GROUP BY 1

   SELECT *,
          USER
   INTO   gparam_dev.*,
          gusuario
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   LET log1 = "/tmp/",gusuario CLIPPED,".EXCB008.log"

   CALL STARTLOG(log1)

   LET hoy = TODAY

   LET ejecuta_procedure = "EXECUTE PROCEDURE fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE clausula_sql3 FROM ejecuta_procedure

   LET ejecuta_marca = "EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
   PREPARE clausula_sql FROM ejecuta_marca

   LET ejecuta_desmarca = "EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "
   PREPARE clausula_sql2 FROM ejecuta_desmarca

   LET ejecuta_rev_marca = "EXECUTE PROCEDURE reversa_marca (?,?,?)"
   PREPARE cla_rev_marca FROM ejecuta_rev_marca

   CALL Proceso()

END MAIN


FUNCTION Inicializa()
#--------------------

   LET hoy = TODAY

   LET vnom_archivo = generar[1,8] CLIPPED,".EXCESO" CLIPPED

   LET vcont_acep              = 0
   LET vcont_par               = 0
   LET vcont_rech              = 0
   LET vcont_pend              = 0

   LET vproc = 0
   LET vpar  = 0
   LET vrech = 0
   LET vpend = 0

   LET precio_dia = 0

END FUNCTION


FUNCTION Proceso()
#-----------------

   DEFINE 
      vresult_operacion    CHAR(2)

   DEFINE 
      vcontador            SMALLINT

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod = 'EXC' ",
                 " AND    etapa_cod = 1 " CLIPPED

   PREPARE claexe FROM cla_sel

   DECLARE cur_proceso CURSOR FOR claexe

   OPEN cur_proceso
      FETCH cur_proceso INTO vconsecutivo
   CLOSE cur_proceso

   LET cla_sel = " SELECT * ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod = 'EXC' ",
                 " AND    etapa_cod = 1 ",        -- ETAPA 1
                 " AND    consecutivo = ",vconsecutivo CLIPPED

   PREPARE claexe2 FROM cla_sel

   DECLARE cur_proceso2 CURSOR FOR claexe2

   OPEN cur_proceso2
      FETCH cur_proceso2 INTO g_bat.*
   CLOSE cur_proceso2

   LET generar = g_bat.parametro1 CLIPPED

display "nombre archivo:",generar

   IF generar IS NOT NULL THEN
      LET vfolio = Proceso_principal()

      LET vhora_final = TIME
      LET vproc       = vcont_acep
      LET vpar        = vcont_par
      LET vrech       = vcont_rech
      LET vpend       = vcont_pend

      IF vfolio = 0 THEN
         LET vresultado = "ESTE ARCHIVO YA HA SIDO PROCESADO"
      ELSE
         LET vproc = 0
         LET vpar  = 0
         LET vrech = 0
         LET vpend = 0

         IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
            DECLARE cur_3 CURSOR FOR
            SELECT result_operacion,
                   COUNT(*)
            FROM   exc_det_exceso
            WHERE  folio = vfolio
            GROUP BY 1

            FOREACH cur_3 INTO vresult_operacion,
                                vcontador
            
                CASE vresult_operacion 
                   WHEN "01" 
                      LET vproc = vcontador
                   WHEN "04" 
                      LET vpar = vcontador
                   WHEN "02" 
                      LET vrech = vcontador
                   WHEN "03" 
                      LET vpend = vcontador
                END CASE
            END FOREACH

         ELSE
            DECLARE cur_4 CURSOR FOR
            SELECT result_operacion,
                   COUNT(*)
            FROM   exc_det_exceso_issste
            WHERE  folio = vfolio
            GROUP BY 1

            FOREACH cur_4 INTO vresult_operacion,
                                vcontador

                CASE vresult_operacion 
                   WHEN "01" 
                      LET vproc = vcontador
                   WHEN "04" 
                      LET vpar = vcontador
                   WHEN "02" 
                      LET vrech = vcontador
                   WHEN "03" 
                      LET vpend = vcontador
                END CASE
            END FOREACH
         END IF

         LET vresultado = "ACEPT :" CLIPPED,vproc CLIPPED,
                          " PARC :" CLIPPED,vpar CLIPPED,
                          " RECH :" CLIPPED,vrech CLIPPED,
                          " PEND :" CLIPPED,vpend CLIPPED
      END IF

      CALL Actualiza_etapa(vfolio,1,vresultado)
   END IF

   DISPLAY "PROCESO TERMINADO DE PAGOS EN EXCESO"

END FUNCTION


FUNCTION Proceso_principal()
#---------------------------

   DEFINE 
      ejecuta    CHAR(300)

   DEFINE
      vfolio     INTEGER

   CALL Inicializa()

   DISPLAY "PROCESANDO INFORMACION..."

   CALL Ingresa_etapa(vfolio,2,"Inicia separa archivo de pagos en exceso")
   CALL Separa_archivo()                                  -- ETAPA 2
   CALL Actualiza_etapa(vfolio,2,"Termina separa archivo de pagos en exceso")

   CALL Ingresa_etapa(vfolio,3,"Inicia carga historicos de pagos en exceso")
   LET  vfolio = Sube_datos()         -- ETAPA 3

   CALL Actualiza_etapa_1(vfolio,1)
   CALL Actualiza_etapa_1(vfolio,2)
   CALL actualiza_monto(vfolio)

   CALL Actualiza_etapa(vfolio,3,"Termina carga historicos de pagos en exceso")

   CALL Ingresa_etapa(vfolio,4,"Inicia validacion y provision de registros ")
   CALL validacion(vfolio)                                -- ETAPA 4
   CALL Actualiza_etapa(vfolio,4,"Termina validacion y provision de registros")

{
   IF x_afore_local = 578 THEN

      DISPLAY "SE EJECUTO EL REVERSO DE PARCIALES ",x_afore_local

      CALL reversa_parciales(vfolio)
   END IF
}
   RETURN vfolio

END FUNCTION


FUNCTION Ingresa_etapa(vfolio,vetapa_cod,vresultado)
#---------------------------------------------------

   DEFINE
      vresultado     CHAR(50)

   DEFINE
      vetapa_cod     DECIMAL(2,0)

   DEFINE
      vfolio         INTEGER

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES
      (hoy,               -- fecha_proceso
       "EXC",             -- proceso_cod
       vetapa_cod,        -- etapa_cod
       hora_inicial,      -- hora_inicial
       hora_final,        -- hora_final
       g_bat.parametro1,  -- parametro1
       NULL,              -- parametro2
       NULL,              -- parametro3
       NULL,              -- parametro4
       NULL,              -- parametro5
       vfolio,            -- folio
       vresultado,        -- resultado
       gusuario,          -- usuario
       0                  -- correlativo
      )

   IF SQLCA.SQLCODE <> 0 THEN
      DISPLAY "DISPLAY AL INSERTA EN TABLA dis_ctrl_proceso etapa ",
            vetapa_cod," ",STATUS
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Separa_archivo()  -- ETAPA 2
#------------------------

   DEFINE
      ejecuta        CHAR(300)

   DEFINE
      vfolio         INTEGER


   DISPLAY "SEPARANDO ARCHIVO,ETAPA 2"

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#

      LET ejecuta = "sed -e '/^01/!d' ",gparam_dev.ruta_rescate CLIPPED,"/",
                    generar CLIPPED," >",gparam_dev.ruta_rescate CLIPPED,"/exc_cza"
      RUN ejecuta

      LET ejecuta = "sed -e '/^02/!d' ",gparam_dev.ruta_rescate CLIPPED,"/",
                   generar CLIPPED," >",gparam_dev.ruta_rescate CLIPPED,"/exc_det"
      RUN ejecuta
      
      LET ejecuta = "sed -e '/^08/!d' ",gparam_dev.ruta_rescate CLIPPED,"/",
                   generar CLIPPED," >",gparam_dev.ruta_rescate CLIPPED,"/exc_dep"
      RUN ejecuta
      
      LET ejecuta = "sed -e '/^09/!d' ",gparam_dev.ruta_rescate CLIPPED,"/",
                   generar CLIPPED," >",gparam_dev.ruta_rescate CLIPPED,"/exc_sum"
      RUN ejecuta
   ELSE

      LET ejecuta = "sed -e '/^01/!d' ",gparam_dev.ruta_rescate CLIPPED,"/",
                    generar CLIPPED," >",gparam_dev.ruta_rescate CLIPPED,"/exc_cza_issste"
      RUN ejecuta

      LET ejecuta = "sed -e '/^02/!d' ",gparam_dev.ruta_rescate CLIPPED,"/",
                   generar CLIPPED," >",gparam_dev.ruta_rescate CLIPPED,"/exc_det_issste"
      RUN ejecuta
      
      LET ejecuta = "sed -e '/^08/!d' ",gparam_dev.ruta_rescate CLIPPED,"/",
                   generar CLIPPED," >",gparam_dev.ruta_rescate CLIPPED,"/exc_dep_issste"
      RUN ejecuta
      
      LET ejecuta = "sed -e '/^09/!d' ",gparam_dev.ruta_rescate CLIPPED,"/",
                   generar CLIPPED," >",gparam_dev.ruta_rescate CLIPPED,"/exc_sum_issste"
      RUN ejecuta
   END IF

   DISPLAY "SEPARACION DE ARCHIVO TERMINADO,ETAPA 2"
END FUNCTION


FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vresultado)
#-----------------------------------------------------

   DEFINE
      vresultado   CHAR(50)

   DEFINE
      hoy          DATE

   DEFINE
      vetapa_cod   DECIMAL(2,0)

   DEFINE 
      vfolio       INTEGER

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod = 'EXC' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe3 FROM cla_sel

   DECLARE cur_proceso3 CURSOR FOR claexe3

   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vconsecutivo
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    hora_final = ","'",vhora_final,"',",
                 "        folio      = ",vfolio,",",
                 "        resultado  = ","'",vresultado,"'",
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod   = 'EXC'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED
   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

END FUNCTION


FUNCTION Actualiza_etapa_1(vfolio,vetapa_cod)
#--------------------------------------------

   DEFINE
      hoy          DATE

   DEFINE
      vetapa_cod   DECIMAL(2,0)

   DEFINE 
      vfolio       INTEGER

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod = 'EXC' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe5 FROM cla_sel

   DECLARE cur_proceso5 CURSOR FOR claexe5

   OPEN cur_proceso5
      FETCH cur_proceso5 INTO vconsecutivo
   CLOSE cur_proceso5

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    folio      = ",vfolio,
                 " WHERE  fecha_proceso = ","'",hoy,"'",
                 " AND    proceso_cod   = 'EXC'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo  = ",vconsecutivo CLIPPED

   PREPARE claexe6 FROM cla_sel

   EXECUTE claexe6

END FUNCTION


FUNCTION Sube_datos()  -- ETAPA 3
#--------------------

   DEFINE
      ejecuta          CHAR(200),
      vlote            CHAR(03),
      vhora_recepcion  CHAR(10),
      vhora_estado     CHAR(10),
      cfecha_envio     CHAR(10),
      vfecha_envio     CHAR(08),
      dvfecha_lote     CHAR(10),
      vcontenido       CHAR(1),
      vid_pago         CHAR(16),
      posicion1        CHAR(1),
      posicion2        CHAR(1),
      display_id       CHAR(50),
      vcomando         CHAR(100),
      carga_reg        CHAR(300),
      c8fecha_paso     CHAR(8),
      c10fecha_paso    CHAR(10)

   DEFINE
      vfecha_recepcion ,
      vfecha_aux       ,
      vfecha_estado    DATE

   DEFINE
      i                ,
      vfolio           INTEGER

   DEFINE
      vestado          SMALLINT

   DISPLAY "SUBE DATOS A TABLAS HISTORICAS,ETAPA 3"

   LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                 "; cut -c 17-24 cza > exc_fecha_lote"
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                 "; cut -c 25-27 cza > exc_lote"
   RUN ejecuta

   LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                 "; cut -c 5-20 dep > exc_id_pago"
   RUN ejecuta

   WHENEVER ERROR CONTINUE
      DROP TABLE fecha_lote
      DROP TABLE lote
      DROP TABLE id_pago
   WHENEVER ERROR STOP

   CREATE TEMP TABLE fecha_lote
      (fecha_lote CHAR(08))

   CREATE TEMP TABLE lote
      (lote CHAR(03))

   CREATE TEMP TABLE id_pago
      (id_pago CHAR(16))

   LET ejecuta = gparam_dev.ruta_rescate CLIPPED,
                 "/exc_fecha_lote" CLIPPED

   LOAD FROM ejecuta INSERT INTO fecha_lote

   LET ejecuta = gparam_dev.ruta_rescate CLIPPED,
                 "/exc_lote" CLIPPED

   LOAD FROM ejecuta INSERT INTO lote

   LET ejecuta = gparam_dev.ruta_rescate CLIPPED,
                 "/exc_id_pago" CLIPPED

   LOAD FROM ejecuta INSERT INTO id_pago

   DECLARE cur_idpago CURSOR FOR
   SELECT id_pago
   FROM   id_pago

   FOREACH cur_idpago INTO vid_pago
      LET posicion1 = vid_pago[13]

      IF  posicion1 <> "3" THEN
          LET display_id = "Proceso cancelado <ENTIDAD A DEVOLVER> invalida: ",
                         posicion1

          CALL Actualiza_etapa(vfolio,3,display_id)

          EXIT PROGRAM
      END IF

      LET posicion2 = vid_pago[14]
      IF  posicion2 = "7" OR posicion2 = "8" THEN
          DISPLAY "VERIFICA IDENTIFICADOR DE PAGO"
      ELSE
          LET display_id = "Proceso cancelado <TIPO DE DEPOSITO> invalida: ",
                          posicion2

          CALL Actualiza_etapa(vfolio,3,display_id)

          EXIT PROGRAM
      END IF
   END FOREACH

   SELECT fecha_lote
   INTO   vfecha_lote
   FROM   fecha_lote

   SELECT lote
   INTO   vlote
   FROM   lote

   LET dvfecha_lote = vfecha_lote [5,6],"/",
                      vfecha_lote [7,8],"/",
                      vfecha_lote [1,4]

   LET vfecha_aux = dvfecha_lote

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
      SELECT "X"
      FROM   exc_cza_exceso
      WHERE  fecha_creac_lote = vfecha_aux
      AND    consecutivo_dia  = vlote
   ELSE
      SELECT "X"
      FROM   exc_cza_exceso_issste
      WHERE  fecha_creac_lote = vfecha_aux
      AND    consecutivo_dia  = vlote
   END IF

   IF SQLCA.SQLCODE <> 0 THEN

      INSERT INTO glo_folio
      VALUES(0)

      SELECT MAX(folio)
      INTO   vfolio
      FROM   glo_folio

      LET vfecha_recepcion = TODAY
      LET vhora_recepcion  = TIME
      LET vestado          = 2
      LET vfecha_estado    = TODAY
      LET vhora_estado     = TIME


      --------------------   GENERA exc_cza_exceso_issste   --------------------

      LET vreporte = gparam_dev.ruta_rescate CLIPPED,"/vfolio_cza"

      START REPORT salida TO vreporte
         LET vtipo_reporte = "C"   ---- Cabeza

         OUTPUT TO REPORT salida(vfolio,
                                 vfecha_recepcion,
                                 vhora_recepcion,
                                 vestado,
                                 vfecha_estado,
                                 vhora_estado)
      FINISH REPORT salida

      IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; paste vfolio_cza exc_cza > cza1" CLIPPED
         RUN ejecuta
         
         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "/;sed 's/	//g' cza1 > exc_cza "
         RUN ejecuta
         
         LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                       "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_exceso_cza -l /tmp/",gusuario CLIPPED,".dbload_exc_cza.log -e 1 -k;"
         RUN ejecuta
      ELSE
         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; paste vfolio_cza exc_cza_issste > cza1" CLIPPED
         RUN ejecuta
         
         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "/;sed 's/   //g' cza1 > exc_cza_issste "
         RUN ejecuta
         
         LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                       "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_exceso_cza_issste -l /tmp/",gusuario CLIPPED,".dbload_exc_cza_issste.log -e 1 -k;"
         RUN ejecuta
      END IF


      --------------------   GENERA adi   --------------------

      LET vreporte = gparam_dev.ruta_rescate CLIPPED,"/adi"

      START REPORT salida2 TO vreporte
         LET vcontenido = "a"

         OUTPUT TO REPORT salida2(vcontenido)
      FINISH REPORT salida2


      --------------------   GENERA exc_det_exceso_issste   --------------------

      LET vreporte = gparam_dev.ruta_rescate CLIPPED,"/vfolio_cza1"

      START REPORT salida1 TO vreporte
         LET vtipo_reporte = "D"   ---- Detalle

         OUTPUT TO REPORT salida1(vfolio,
                                 vestado,
                                 vfecha_estado,
                                 vhora_estado)
      FINISH REPORT salida1


      -- Se crea archivo adi que contiene a\  y se pega en vfolio_det

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza1 > vfolio_det" CLIPPED
      RUN ejecuta


      IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#

         -- Se crea archivo vfolio_det2 con el numero de registros igual al
         -- num. registros que tiene el detalle

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; sed -f vfolio_det exc_det > vfolio_det2" CLIPPED
         RUN ejecuta

      ELSE
         -- Se crea archivo vfolio_det2 con el numero de registros igual al
         -- num. registros que tiene el detalle
         
         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; sed -f vfolio_det exc_det_issste > vfolio_det2" CLIPPED
         RUN ejecuta
      END IF


      -- Se crea archivo vfolio_det borrando lineas que no sirven

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_det2 > vfolio_det" CLIPPED
      RUN ejecuta


      IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
         -- Se crea det1 pegando datos genrales con el detalle

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; paste vfolio_det exc_det > det1" CLIPPED
         RUN ejecuta

         -- Se crea det eliminando espacio en blanco que se genera cuando
         -- se pegan los 2 archivos

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "/;sed 's/	//g' det1 > exc_det "
         RUN ejecuta

         -- Se suben los datos del detalle

--svera         LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
--svera                       "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_exceso_det -l /tmp/",gusuario CLIPPED,".dbload_exc_det.log -e 1 -k;"
--svera         RUN ejecuta

      ELSE
         -- Se crea det1 pegando datos genrales con el detalle
         
         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; paste vfolio_det exc_det_issste > det1" CLIPPED
         RUN ejecuta

         -- Se crea det eliminando espacio en blanco que se genera cuando
         -- se pegan los 2 archivos
         
         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "/;sed 's/	//g' det1 > exc_det_issste "
         RUN ejecuta

         -- Se suben los datos del detalle

           LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                         "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_exceso_det_issste -l /tmp/",gusuario CLIPPED,".dbload_exc_det_issste.log -e 1 -k;"
           RUN ejecuta
      END IF
      
      ------ PLAN CONTINGENTE --------



      CREATE TEMP TABLE archivo_sideco (lista  CHAR(300))
      
      LET ejecuta = gparam_dev.ruta_rescate CLIPPED,"/",generar CLIPPED

      LOAD FROM ejecuta INSERT INTO archivo_sideco
      
      DECLARE cur_contig CURSOR FOR
      SELECT  *
      FROM    archivo_sideco

      FOREACH cur_contig INTO carga_reg
     
         IF carga_reg[001,002] = "02" THEN
         
            LET reg_contig.tipo_registro       = carga_reg[001,002]
            LET reg_contig.ident_servicio      = carga_reg[003,004]
            LET reg_contig.consec_reg_lote     = carga_reg[005,012]
            LET reg_contig.nss                 = carga_reg[013,023]
            LET reg_contig.clave_ent_orig      = carga_reg[024,026]
            LET reg_contig.rfc                 = carga_reg[027,039]
            LET reg_contig.curp                = carga_reg[040,057]
            LET reg_contig.nom_trabajador      = carga_reg[058,107]
            LET reg_contig.periodo_pago        = carga_reg[108,113]
            LET c8fecha_paso                   = carga_reg[114,121]
            
            LET c10fecha_paso = c8fecha_paso[5,6],"/",
                                c8fecha_paso[7,8],"/",
                                c8fecha_paso[1,4]

            LET reg_contig.fecha_pago = c10fecha_paso



            LET c8fecha_paso = carga_reg[122,129]
            
            LET c10fecha_paso = c8fecha_paso[5,6],"/",
                                c8fecha_paso[7,8],"/",
                                c8fecha_paso[1,4]
            
            LET reg_contig.fecha_valor_rcv = c10fecha_paso
            


            LET c8fecha_paso = carga_reg[130,137]
            
            LET c10fecha_paso = c8fecha_paso[5,6],"/",
                                c8fecha_paso[7,8],"/",
                                c8fecha_paso[1,4]
            
            LET reg_contig.fecha_valor_viv = c10fecha_paso
            
            
            LET reg_contig.folio_pago_sua      = carga_reg[138,143]
            LET reg_contig.clave_ent_recep     = carga_reg[144,146]
            LET reg_contig.reg_patronal_imss   = carga_reg[147,157]
            LET reg_contig.monto_ret           = carga_reg[158,166]
            LET reg_contig.monto_ces_vej_pat   = carga_reg[167,175]
            LET reg_contig.monto_ces_vej_tra   = carga_reg[176,184]
            LET reg_contig.monto_cuo_soc       = carga_reg[185,193]
            LET reg_contig.monto_act_ret       = carga_reg[194,202]
            LET reg_contig.monto_act_cv_pat    = carga_reg[203,211]
            LET reg_contig.monto_act_cv_tra    = carga_reg[212,220]
            LET reg_contig.monto_aport_pat     = carga_reg[221,229]
            LET reg_contig.dias_cotz_bimestre  = carga_reg[230,231]
            LET reg_contig.monto_par_viv       = carga_reg[232,246]
            LET reg_contig.mto_act_ret_rend    = carga_reg[247,255]
            LET reg_contig.mto_act_cv_rend_pat = carga_reg[256,264]
            LET reg_contig.mto_act_cv_rend_tra = carga_reg[265,273]
            
            LET c8fecha_paso = carga_reg[274,281]
            
            LET c10fecha_paso = c8fecha_paso[5,6],"/",
                                c8fecha_paso[7,8],"/",
                                c8fecha_paso[1,4]
                                
            LET reg_contig.fecha_envio_orig    = c10fecha_paso
            LET reg_contig.tipo_diagnostico    = carga_reg[282,283]
            
            LET reg_contig.folio             = vfolio
            LET reg_contig.estado            = vestado
            LET reg_contig.fecha_estado      = vfecha_estado
            LET reg_contig.hora_estado       =  vhora_estado
            LET reg_contig.monto_act_ces_vej = 0
            LET reg_contig.monto_aport_est   = 0
            LET reg_contig.monto_aport_esp   = 0
            LET reg_contig.result_operacion  = "01"
            LET reg_contig.tipo_diagnostico  = " "
            
            INSERT INTO exc_det_exceso VALUES(reg_contig.*)
            
         END IF
      END FOREACH


      --------------------   GENERA exc_dep_exceso_issste   --------------------

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_dep

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza1 > vfolio_dep" CLIPPED
      RUN ejecuta


      IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
         -- Se crea archivo vfolio_dep2 con el numero de registros igual al
         -- num. registros que tiene el detalle

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; sed -f vfolio_dep exc_dep > vfolio_dep2" CLIPPED
         RUN ejecuta

      ELSE
         -- Se crea archivo vfolio_dep2 con el numero de registros igual al
         -- num. registros que tiene el detalle
         
         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; sed -f vfolio_dep exc_dep_issste > vfolio_dep2" CLIPPED
         RUN ejecuta
      END IF


      -- Se crea archivo vfolio_det borrando lineas que no sirven

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_dep2 > vfolio_dep" CLIPPED
      RUN ejecuta


      IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
         -- Se crea dep1 pegando datos genrales con el detalle

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; paste vfolio_dep exc_dep > dep1" CLIPPED
         RUN ejecuta

         -- Se crea det eliminando espacio en blanco que se genera cuando
         -- se pegan los 2 archivos

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "/;sed 's/	//g' dep1 > exc_dep "
         RUN ejecuta

         -- Se suben los datos del detalle

         LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                       "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_exceso_dep -l /tmp/",gusuario CLIPPED,".dbload_exc_dep.log -e 1 -k;"
         RUN ejecuta

      ELSE
         -- Se crea dep1 pegando datos genrales con el detalle

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; paste vfolio_dep exc_dep_issste > dep1" CLIPPED
         RUN ejecuta

         -- Se crea det eliminando espacio en blanco que se genera cuando
         -- se pegan los 2 archivos

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "/;sed 's/	//g' dep1 > exc_dep_issste "
         RUN ejecuta

         -- Se suben los datos del detalle

         LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                       "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_exceso_dep_issste -l /tmp/",gusuario CLIPPED,".dbload_exc_dep_issste.log -e 1 -k;"
         RUN ejecuta
      END IF


      --------------------   GENERA exc_sum_exceso   --------------------

      IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; paste vfolio_cza1 exc_sum > sum1" CLIPPED
         RUN ejecuta

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "/;sed 's/	//g' sum1 > exc_sum "
         RUN ejecuta

         LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                       "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_exceso_sum -l /tmp/",gusuario CLIPPED,".dbload_exc_sum.log -e 1 -k;"
         RUN ejecuta

         -- Se borran todo los archivo auxiliares

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; rm archivos exc_cza exc_det exc_dep exc_sum exc_fecha_lote exc_id_pago exc_lote vfolio_cza vfolio_cza1 vfolio_dep vfolio_dep2 vfolio_det vfolio_det2 cza1 det1 dep1 sum1 adi"
         RUN ejecuta

         LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                       "/;DBDATE=mdy4;export DBDATE;"

         RUN ejecuta

         RETURN vfolio
      ELSE
         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; paste vfolio_cza1 exc_sum_issste > sum1" CLIPPED
         RUN ejecuta

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "/;sed 's/	//g' sum1 > exc_sum_issste "
         RUN ejecuta

         LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                       "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_exceso_sum_issste -l /tmp/",gusuario CLIPPED,".dbload_exc_sum_issste.log -e 1 -k;"
         RUN ejecuta

         -- Se borran todo los archivo auxiliares

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; rm archivos exc_cza exc_det_issste exc_dep_issste exc_sum_issste exc_fecha_lote exc_id_pago exc_lote vfolio_cza vfolio_cza1 vfolio_dep vfolio_dep2 vfolio_det vfolio_det2 cza1 det1 dep1 sum1 adi"
         RUN ejecuta

         LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                       "/;DBDATE=mdy4;export DBDATE;"

         RUN ejecuta

         RETURN vfolio
      END IF
   ELSE
      DISPLAY "ESTE ARCHIVO YA HA SIDO PROCESADO"
      RETURN 0
   END IF

   DISPLAY "SUBE DATOS A TABLAS HISTORICAS TERMINADO,ETAPA 3"

END FUNCTION


FUNCTION actualiza_monto(vfolio)
#--------------------------------

   DEFINE vfolio     INTEGER

   DISPLAY "ACTUALIZA MONTOS"

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
   DECLARE cursor_2 CURSOR FOR
   SELECT NVL(monto_ret,0),
          NVL(monto_act_ret,0),
          NVL(monto_ces_vej_pat,0),
          NVL(monto_act_cv_pat,0),
          NVL(monto_ces_vej_tra,0),
          NVL(monto_act_cv_tra,0),
          NVL(monto_cuo_soc,0),
          NVL(monto_aport_pat,0),
          NVL(monto_par_viv,0),
          NVL(mto_act_ret_rend,0),
          NVL(mto_act_cv_rend_pat,0),
          NVL(mto_act_cv_rend_tra,0)
   FROM   exc_det_exceso
   WHERE  folio = vfolio
   FOR UPDATE

      FOREACH cursor_2 INTO g_det_imss.monto_ret,
                            g_det_imss.monto_act_ret,
                            g_det_imss.monto_ces_vej_pat,
                            g_det_imss.monto_act_cv_pat,
                            g_det_imss.monto_ces_vej_tra,
                            g_det_imss.monto_act_cv_tra,
                            g_det_imss.monto_cuo_soc,
                            g_det_imss.monto_aport_pat,
                            g_det_imss.monto_par_viv,
                            g_det_imss.mto_act_ret_rend,
                            g_det_imss.mto_act_cv_rend_pat,
                            g_det_imss.mto_act_cv_rend_tra

         UPDATE exc_det_exceso
         SET    monto_ret           = g_det_imss.monto_ret/100,
                monto_act_ret       = g_det_imss.monto_act_ret/100,
                monto_ces_vej_pat   = g_det_imss.monto_ces_vej_pat/100,
                monto_act_cv_pat    = g_det_imss.monto_act_cv_pat/100,
                monto_ces_vej_tra   = g_det_imss.monto_ces_vej_tra/100,
                monto_act_cv_tra    = g_det_imss.monto_act_cv_tra/100,
                monto_cuo_soc       = g_det_imss.monto_cuo_soc/100,
                monto_aport_pat     = g_det_imss.monto_aport_pat/100,
                monto_par_viv       = g_det_imss.monto_par_viv/1000000,
                mto_act_ret_rend    = g_det_imss.mto_act_ret_rend/100,
                mto_act_cv_rend_pat = g_det_imss.mto_act_cv_rend_pat/100,
                mto_act_cv_rend_tra = g_det_imss.mto_act_cv_rend_tra/100
         WHERE  CURRENT OF cursor_2

      END FOREACH

   ELSE
      DECLARE cursor_5 CURSOR FOR
      SELECT NVL(mto_global_pagado,0),
             NVL(impt_sar_issste,0),
             NVL(impt_ret_issste,0),
             NVL(impt_cv_patron,0),
             NVL(impt_fondo_viv92,0),
             NVL(aplic_int_fondo_viv92,0),
             NVL(impt_fondo_viv08,0),
             NVL(aplic_int_fondo_viv08,0),
             NVL(impt_ahorro_solid,0)
      FROM   exc_det_exceso_issste
      WHERE  folio = vfolio
      FOR UPDATE

      FOREACH cursor_5 INTO g_det_isss.mto_global_pagado        ,
                            g_det_isss.impt_sar_issste          ,
                            g_det_isss.impt_ret_issste          ,
                            g_det_isss.impt_cv_patron           ,
                            g_det_isss.impt_fondo_viv92         ,
                            g_det_isss.aplic_int_fondo_viv92    ,
                            g_det_isss.impt_fondo_viv08         ,
                            g_det_isss.aplic_int_fondo_viv08    ,
                            g_det_isss.impt_ahorro_solid        

         UPDATE exc_det_exceso_issste
         SET    mto_global_pagado     = g_det_isss.mto_global_pagado/100,
                impt_sar_issste       = g_det_isss.impt_sar_issste/100,
                impt_ret_issste       = g_det_isss.impt_ret_issste /100,
                impt_cv_patron        = g_det_isss.impt_cv_patron/100,
                impt_fondo_viv92      = g_det_isss.impt_fondo_viv92/100,
                impt_fondo_viv08      = g_det_isss.impt_fondo_viv08/100,
                aplic_int_fondo_viv92 = g_det_isss.aplic_int_fondo_viv92/1000000,
                aplic_int_fondo_viv08 = g_det_isss.aplic_int_fondo_viv08/1000000,
                impt_ahorro_solid     = g_det_isss.impt_ahorro_solid/100
         WHERE  CURRENT OF cursor_2

      END FOREACH
   END IF

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
      DECLARE cursor_3 CURSOR FOR
      SELECT ident_pago[14,15],
             NVL(monto_soli_institu,0),
             NVL(monto_par_vivienda,0),
             codigo_siefore
      FROM   exc_dep_exceso
      WHERE  folio = vfolio
      FOR UPDATE

      FOREACH cursor_3 INTO g_dep.ident_pago,
                            g_dep.monto_soli_instituto,
                            g_dep.monto_par_vivienda,
                            g_dep.codigo_siefore

         IF g_dep.ident_pago = "71" THEN
            LET g_dep.codigo_siefore = 0
         ELSE
            LET g_dep.codigo_siefore = 0
         END IF

         UPDATE exc_dep_exceso
         SET    monto_soli_institu   = g_dep.monto_soli_instituto/100,
                monto_par_vivienda   = g_dep.monto_par_vivienda/1000000,
                codigo_siefore       = g_dep.codigo_siefore
         WHERE  CURRENT OF cursor_3

      END FOREACH
   ELSE
      DECLARE cursor_6 CURSOR FOR
      SELECT ident_pago[14,15],
             NVL(monto_soli_instituto,0),
             codigo_siefore       
      FROM   exc_dep_exceso_issste
      WHERE  folio = vfolio
      FOR UPDATE

      FOREACH cursor_6 INTO g_dep.ident_pago,
                            g_dep.monto_soli_instituto,
                            g_dep.codigo_siefore

         IF g_dep.ident_pago = "71" THEN
            LET g_dep.codigo_siefore = 0
         ELSE
            LET g_dep.codigo_siefore = 0
         END IF

         UPDATE exc_dep_exceso_issste
         SET    monto_soli_instituto = g_dep.monto_soli_instituto/100,
                codigo_siefore       = g_dep.codigo_siefore
         WHERE  CURRENT OF cursor_3

      END FOREACH
   END IF

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
      DECLARE cursor_4 CURSOR FOR
      SELECT NVL(monto_tot_rcv,0),
             NVL(monto_tot_par_viv,0)
      FROM   exc_sum_exceso
      WHERE  folio = vfolio
      FOR UPDATE

      FOREACH cursor_4 INTO g_sum.monto_tot_rcv,
      	                    g_sum.monto_tot_par_viv
      
         UPDATE exc_sum_exceso
         SET    monto_tot_rcv     = g_sum.monto_tot_rcv/100,
                monto_tot_par_viv = g_sum.monto_tot_par_viv/1000000
         WHERE  CURRENT OF cursor_4

      END FOREACH
   END IF

   DISPLAY "ACTUALIZACION TERMINADA"

END FUNCTION


FUNCTION validacion(vfolio)      ---ETAPA 4
#--------------------------

   DEFINE
      vfecha_pri_general      ,
      mar_fecha_act_marca     DATE

   DEFINE
      vcont_curp              ,
      vfolio                  INTEGER

   DEFINE
      cuantos_tmp_dis_cuenta  ,
      mar_activo_marca        ,
      mar_marca_cod           ,
      mig_marca_entra         ,
      uni_sw                  SMALLINT

   DISPLAY "VALIDACION Y PROVISION,ETAPA 4"

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
      DECLARE cursor_exc CURSOR FOR
      SELECT *
      FROM   exc_det_exceso
      WHERE  folio = vfolio
      ORDER BY 8,7

      LET cuantos_tmp_dis_cuenta = 0

      FOREACH cursor_exc INTO g_det_imss.*

         IF g_det_imss.clave_ent_orig = "001" THEN
            LET mig_marca_entra = 540
         ELSE
            LET mig_marca_entra = 542
         END IF

         DISPLAY "g_det_imss.nss :",g_det_imss.nss

         SELECT "OK"
         FROM   afi_mae_afiliado
         WHERE  n_seguro = g_det_imss.nss
         GROUP BY 1

         IF SQLCA.SQLCODE =  100 THEN
            CALL actualiza_reg(g_det_imss.folio,
                               g_det_imss.curp,
                               g_det_imss.nss,
                               "02",   #-- RECHAZADO --#
                               "588",
                               g_det_imss.consec_reg_lote,
                               "",
                               mig_marca_entra,
                               gusuario)
         ELSE
            CALL valida_datos(g_det_imss.folio,
                              g_det_imss.curp,
                              g_det_imss.nss,
                              g_det_imss.consec_reg_lote,
                              g_det_imss.clave_ent_orig,
                              g_det_imss.monto_aport_pat,
                              0,
                              0,
                              g_det_imss.fecha_pago)
         END IF
      END FOREACH
   ELSE
      DECLARE cursor_exc1 CURSOR FOR
      SELECT *
      FROM   exc_det_exceso_issste
      WHERE  folio = vfolio
      ORDER BY 8,7

      LET cuantos_tmp_dis_cuenta = 0

      FOREACH cursor_exc1 INTO g_det_isss.*

         IF g_det_isss.clave_ent_orig = "001" THEN
            LET mig_marca_entra = 543
         ELSE
            LET mig_marca_entra = 544
         END IF

         display "g_det_isss.curp: ",g_det_isss.curp

         SELECT COUNT(*)
         INTO   vcont_curp
         FROM   afi_mae_afiliado
         WHERE  n_unico = g_det_isss.curp

         display "vcont_curp: ",vcont_curp

         IF vcont_curp > 1 THEN   #-- CURP DUPLICADA --#
            CALL actualiza_reg(g_det_isss.folio,
                               g_det_isss.curp,
                               g_det_isss.nss,
                               "02",   #-- RECHAZADO --#
                               "588",
                               g_det_isss.consec_reg_lote,
                               "",
                               0,
                               gusuario)
            CONTINUE FOREACH
         ELSE
            SELECT n_seguro
            INTO   g_det_isss.nss
            FROM   afi_mae_afiliado
            WHERE  n_unico = g_det_isss.curp

            IF SQLCA.SQLCODE =  100 THEN
               CALL actualiza_reg(g_det_isss.folio,
                                  g_det_isss.curp,
                                  g_det_isss.nss,
                                  "02",   #-- RECHAZADO --#
                                  "588",
                                  g_det_isss.consec_reg_lote,
                                  "",
                                  mig_marca_entra,
                                  gusuario)
            ELSE
               CALL valida_datos(g_det_isss.folio,
                                 g_det_isss.curp,
                                 g_det_isss.nss,
                                 g_det_isss.consec_reg_lote,
                                 g_det_isss.clave_ent_orig,
                                 0,
                                 g_det_isss.impt_fondo_viv92,
                                 g_det_isss.impt_fondo_viv08,
                                 g_det_isss.fecha_pago)
            END IF
         END IF
      END FOREACH
   END IF

END FUNCTION


FUNCTION valida_datos(reg_val)
#-----------------------------

   DEFINE reg_val RECORD
      folio                    INTEGER,
      curp                     CHAR(18),
      nss                      CHAR(11),
      consec_reg_lote          INTEGER,
      clave_ent_orig           CHAR(3),
      monto_aport_pat          DECIMAL(16,6),
      impt_fondo_viv92         DECIMAL(18,6),
      impt_fondo_viv08         DECIMAL(18,6),
      fecha_pago               DATE
   END RECORD

   DEFINE
      auxfecha_pago            CHAR(10),
      auxfecha_pago1           CHAR(8),
      tipo_rech                CHAR(3)

   DEFINE
      x_saldo_acc              DECIMAL(18,6),
      x_saldo_pes              DECIMAL(18,6)

   DEFINE
      tmp_dis_cuenta           INTEGER

   DEFINE
      cuantos_tmp_dis_cuenta   ,
      mig_marca_entra          ,
      mig_codigo_rechazo       ,
      vestado_proceso          SMALLINT


   WHENEVER ERROR CONTINUE

      SELECT COUNT(*)
      INTO   cuantos_tmp_dis_cuenta
      FROM   tmp_dis_cuenta
      WHERE  nss = reg_val.nss
   WHENEVER ERROR STOP

   IF cuantos_tmp_dis_cuenta = 0 THEN
      CALL genera_tmp_cuenta (reg_val.nss)
   END IF

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
      #-- CONTINUA --#
   ELSE
      IF reg_val.clave_ent_orig = "001" THEN
         CALL saldo_al_dia(reg_val.nss,
                           0,   #--SUBCUENTA
                           0)   #--GRUPO
              RETURNING x_saldo_acc,
                        x_saldo_pes
      ELSE
         IF reg_val.impt_fondo_viv92 > 0 THEN   #-- FOVISSSTE --#
            CALL saldo_al_dia(reg_val.nss,
                              14,   #--SUBCUENTA
                              0)    #--GRUPO
                 RETURNING x_saldo_acc,
                           x_saldo_pes
         END IF

         IF reg_val.impt_fondo_viv08 > 0 THEN
            CALL saldo_al_dia(reg_val.nss,
                              35,   #--SUBCUENTA
                              0)    #--GRUPO
                 RETURNING x_saldo_acc,
                           x_saldo_pes
         END IF
      END IF
   END IF

   LET x_saldo_acc = 0
   LET x_saldo_pes = 0

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
      IF reg_val.clave_ent_orig = "001" THEN
         LET mig_marca_entra = 540
      ELSE
         LET mig_marca_entra = 542
      END IF
   ELSE
      IF reg_val.clave_ent_orig = "001" THEN
         LET mig_marca_entra = 543
      ELSE
         LET mig_marca_entra = 544
      END IF
   END IF

   LET auxfecha_pago = reg_val.fecha_pago

   LET auxfecha_pago1 = auxfecha_pago[7,10],
                        auxfecha_pago[1,2],
                        auxfecha_pago[4,5]


   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
      LET tipo_rech = 100
   ELSE
      LET tipo_rech = valida_importes()
   END IF

   IF tipo_rech > 100 THEN
      CALL actualiza_reg(reg_val.folio,
                         reg_val.curp,
                         reg_val.nss,
                         "02",   #-- RECHAZADO --#
                         tipo_rech,
                         reg_val.consec_reg_lote,
                         "",
                         mig_marca_entra,
                         gusuario)
   END IF

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
      CALL valida_sufici_imss()
         RETURNING tipo_rech,
                   diag_dev
   ELSE
      CALL valida_sufici_isss()
         RETURNING tipo_rech,
                   diag_dev
   END IF

   IF tipo_rech > 100 THEN
      CALL actualiza_reg(reg_val.folio,
                         reg_val.curp,
                         reg_val.nss,
                         diag_dev,
                         tipo_rech,
                         reg_val.consec_reg_lote,
                         "",
                         mig_marca_entra,
                         gusuario)
   END IF  
   
   IF tipo_rech = "100" THEN
      CALL marca(reg_val.nss,              -- nss
                 mig_marca_entra,          -- marca_entra
                 reg_val.consec_reg_lote,  -- correlativo
                 0,                        -- marca_estado
                 0,                        -- codigo_rechazo
                 0,                        -- marca_causa
                 " ",                      -- fecha_causa
                 gusuario                  -- usuario
                )

      RETURNING vestado_proceso,
                mig_codigo_rechazo

      IF mig_codigo_rechazo = 0 THEN
         IF diag_dev = "01" THEN   #-- ACEPTADO --#
            CALL Contador_aceptado()
         ELSE
           CALL Contador_parcial()
         END IF

         IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
            CALL provisiona_cuenta_imss()
         ELSE
            CALL provisiona_cuenta_issste()
         END IF

      ELSE
         IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
            IF mig_codigo_rechazo = 321 OR     #-- CUENTA CANCELADA --#
               mig_codigo_rechazo = 947 THEN   #-- CUENTA INHABILITADA  --#
               CALL actualiza_reg(reg_val.folio,
                                  reg_val.curp,
                                  reg_val.nss,
                                  "02",   #--RECHAZADO --#
                                  mig_codigo_rechazo,
                                  reg_val.consec_reg_lote,
                                  "",
                                  mig_marca_entra,
                                  gusuario)
            END IF

            IF mig_codigo_rechazo = 975 THEN     #-- TRANSFERENCIA ENTRE SIEFORES --#
               CALL actualiza_reg(reg_val.folio,
                                  reg_val.curp,
                                  reg_val.nss,
                                  "03",   #--PENDIENTE --#
                                  mig_codigo_rechazo,
                                  reg_val.consec_reg_lote,
                                  "",
                                  mig_marca_entra,
                                  gusuario)
            END IF
         END IF

         IF vtipo_arch = 2 THEN   #-- ARCHIVO ISSSTE --#
            IF mig_codigo_rechazo = 528 OR
               mig_codigo_rechazo = 530 OR
               mig_codigo_rechazo = 575 OR
               mig_codigo_rechazo = 586 OR
               mig_codigo_rechazo = 587 THEN

               CALL actualiza_reg(reg_val.folio,
                                  reg_val.curp,
                                  reg_val.nss,
                                  "03",   #--PENDIENTE --#
                                  mig_codigo_rechazo,
                                  reg_val.consec_reg_lote,
                                  "",
                                  mig_marca_entra,
                                  gusuario)
            ELSE
               CALL actualiza_reg(reg_val.folio,
                                  reg_val.curp,
                                  reg_val.nss,
                                  "02",   #-- RECHAZADO --#
                                  mig_codigo_rechazo,
                                  g_det_isss.consec_reg_lote,
                                  "",
                                  mig_marca_entra,
                                  gusuario)
            END IF
         END IF
      END IF
   END IF

   DISPLAY "VALIDACION Y PROVISION TERMINADA,ETAPA 4"

END FUNCTION


FUNCTION actualiza_reg(rfolio,
                       vcurp,
                       vnss,
                       res_op,
                       tipo_rech,
                       x_consecutivo,
                       x_folio_sua,
                       x_mig_marca_entra,
                       x_gusuario)
#----------------------------------------

   DEFINE rfolio               INTEGER,
          vcurp                CHAR(18),
          vnss                 CHAR(11),
          res_op               CHAR(02),
          tipo_rech            CHAR(03),
          x_consecutivo        INTEGER,
          x_folio_sua          CHAR(6),
          x_mig_marca_entra    SMALLINT,
          x_gusuario           CHAR(8),
          x_vestado_proceso    SMALLINT,
          x_mig_codigo_rechazo SMALLINT

   display "---- FUNCION ACTUALIZA REGISTRO ----"
   display "rfolio,          :",rfolio          
   display "vcurp            :",vcurp
   display "vnss,            :",vnss           
   display "res_op,          :",res_op          
   display "tipo_rech,       :",tipo_rech       
   display "x_consecutivo,   :",x_consecutivo   
   display "x_folio_sua,     :",x_folio_sua     
   display "x_mig_marca_entra:",x_mig_marca_entra
   display "x_gusuario      :",x_gusuario     

   IF tipo_rech = 588 OR
      tipo_rech = 528 OR
      tipo_rech = 529 OR
      tipo_rech = 530 OR
      tipo_rech = 575 OR
      tipo_rech = 586 OR
      tipo_rech = 587 THEN
   ELSE
      display "--- marca rechazo historico ---"
      display "vcurp           :",vcurp
      display "vnss            :",vnss          
      display "x_mig_marca_entra:",x_mig_marca_entra
      display "x_consecutivo,   :",x_consecutivo  
      display "tipo_rech,       :",tipo_rech      
      display "x_gusuario       :",x_gusuario       

      CALL marca(vnss,              -- nss
                 x_mig_marca_entra, -- marca_entra
                 x_consecutivo,     -- correlativo
                 30,                -- marca_estado
                 tipo_rech,         -- codigo_rechazo
                 0,                 -- marca_causa
                 " ",               -- fecha_causa
                 x_gusuario         -- usuario
                )

      RETURNING x_vestado_proceso,
                x_mig_codigo_rechazo

      display "x_vestado_proceso: ",x_vestado_proceso
      display "x_mig_codigo_rechazo: ",x_mig_codigo_rechazo

   END IF

   IF res_op = "02" THEN   #-- RECHAZADO --#
      CALL contador_rechazo()
   ELSE
      CALL contador_pendiente()
   END IF

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
      UPDATE exc_det_exceso
      SET    result_operacion = res_op,
             tipo_diagnostico = tipo_rech,
             monto_par_viv    = 0
      WHERE  nss = vnss
      AND    folio = rfolio
      AND    consec_reg_lote = x_consecutivo
   ELSE
      UPDATE exc_det_exceso_issste
      SET    result_operacion = res_op,
             tipo_diagnostico = tipo_rech,
             aplic_int_fondo_viv92 = 0,
             aplic_int_fondo_viv08 = 0
      WHERE  curp = vcurp
      AND    folio = rfolio
      AND    consec_reg_lote = x_consecutivo
   END IF

END FUNCTION


FUNCTION Contador_aceptado()
#----------------------------
   LET vcont_acep = vcont_acep + 1
END FUNCTION


FUNCTION Contador_rechazo()
#--------------------------
   LET vcont_rech = vcont_rech + 1
END FUNCTION


FUNCTION Contador_pendiente()
#----------------------------
   LET vcont_pend = vcont_pend + 1
END FUNCTION


FUNCTION Contador_parcial()
#----------------------------
   LET vcont_par = vcont_par + 1
END FUNCTION


REPORT salida(vfolio,
              vfecha_recepcion,
              vhora_recepcion,
              vestado,
              vfecha_estado,
              vhora_estado)
#------------------------------

   DEFINE vfolio           INTEGER,
          vfecha_recepcion DATE,
          vhora_recepcion  CHAR(08),
          vestado          CHAR(01),
          vfecha_estado    DATE,
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
                  vfecha_recepcion USING "YYYY/MM/DD",
                  vhora_recepcion,
                  vestado,
                  vfecha_estado    USING "YYYY/MM/DD",
                  vhora_estado
         ELSE
            PRINT vfolio           USING '----------'
         END IF
END REPORT


REPORT salida1(vfolio,
               vestado,
               vfecha_estado,
               vhora_estado)
#-----------------------------

   DEFINE vfolio           INTEGER,
          vestado          CHAR(01),
          vfecha_estado    DATE,
          vhora_estado     CHAR(08)

   OUTPUT
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT
      ON EVERY ROW
         IF vtipo_reporte = "D" THEN
            PRINT vfolio        USING '----------',
                  vestado,
                  vfecha_estado USING "YYYY/MM/DD",
                  vhora_estado
         ELSE
            PRINT vfolio        USING '----------'
         END IF
END REPORT


REPORT salida2(vcontenido)
#-------------------------

   DEFINE vcontenido    CHAR(1)

   OUTPUT
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT
      ON EVERY ROW
         PRINT vcontenido,
               "\\"
END REPORT


FUNCTION marca(x_nss,
               x_marca_entra,
               x_consec_reg_lote,
               x_marca_estado,
               x_codigo_rechazo,
               x_marca_causa,
               x_fecha_causa,
               x_usuario)
#----------------------------------

   DEFINE x_nss             CHAR(11),
          x_operacion       CHAR(1),
          x_marca_entra     SMALLINT,
          x_consec_reg_lote SMALLINT,
          x_marca_estado    SMALLINT,
          x_codigo_rechazo  SMALLINT,
          x_marca_causa     SMALLINT,
          x_fecha_causa     DATE,
          x_usuario         CHAR(8),
          xx_codigo_marca   SMALLINT,
          xx_codigo_rechazo SMALLINT,
          ejecuta_procedure CHAR(200)

   DECLARE cursor_marca CURSOR FOR clausula_sql

   OPEN cursor_marca USING x_nss,
                           x_marca_entra,
                           x_consec_reg_lote,
                           x_marca_estado,
                           x_codigo_rechazo,
                           x_marca_causa,
                           x_fecha_causa,
                           x_usuario

      FETCH cursor_marca INTO xx_codigo_marca,
                              xx_codigo_rechazo

   CLOSE cursor_marca

   RETURN xx_codigo_marca,
          xx_codigo_rechazo

END FUNCTION


FUNCTION saldo_al_dia(v_nss,
                      v_subcuenta,
                      v_grupo)
#-----------------------------------

   DEFINE 
      v_saldo_dia        CHAR(100),
      v_nss              CHAR(11)

   DEFINE
      hoy                ,
      vfecha_saldo       DATE

   DEFINE
      f_monto_acc        DECIMAL(16,6),
      f_monto_pes        DECIMAL(16,6),
      v_saldo_acc        DECIMAL(16,6),
      v_saldo_pes        DECIMAL(16,6)

   DEFINE
      v_subcuenta        ,
      v_grupo            ,
      f_subcuenta        ,
      f_siefore          SMALLINT

   LET hoy = TODAY

   WHENEVER ERROR CONTINUE
      DROP TABLE temp_saldo_act
   WHENEVER ERROR STOP

   CREATE TEMP TABLE temp_saldo_act
   (nss            CHAR(11),
    subcuenta      SMALLINT,
    siefore        SMALLINT,
    acciones       DECIMAL(16,6),
    pesos          DECIMAL(16,6)
   )

   IF v_subcuenta = 0 THEN

      LET vfecha_saldo = hoy

      LET v_saldo_dia = "EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? ) "

      PREPARE eje_saldo_dia FROM v_saldo_dia
      
      LET v_saldo_acc  = 0
      LET v_saldo_pes  = 0
      
      DECLARE c_saldo CURSOR FOR  eje_saldo_dia
      FOREACH c_saldo  USING v_nss,
                             v_subcuenta,
                             v_grupo,
                             vfecha_saldo
                        INTO f_subcuenta,
                             f_siefore,
                             f_monto_acc,
                             f_monto_pes

           INSERT INTO temp_saldo_act
           VALUES (v_nss,
                   f_subcuenta,
                   f_siefore,
                   f_monto_acc,
                   f_monto_pes
                  )

           LET v_saldo_acc  = v_saldo_acc + f_monto_acc
           LET v_saldo_pes  = v_saldo_pes + f_monto_pes
      END FOREACH
   ELSE
      LET vfecha_saldo = MDY(MONTH(hoy),1,YEAR(hoy))

      LET vfecha_saldo = vfecha_saldo + 1 UNITS MONTH

      CALL fn_saldo_dia_viv(v_nss,v_subcuenta,v_grupo,vfecha_saldo)
      RETURNING v_saldo_acc,
                v_saldo_pes

      INSERT INTO temp_saldo_act
      VALUES (v_nss,
              v_subcuenta,
              12,
              v_saldo_acc,
              v_saldo_pes
             )
   END IF

   RETURN v_saldo_acc,
          v_saldo_pes
END FUNCTION


FUNCTION fn_saldo_dia_viv( pnss,
                           psubcuenta,
                           pgrupo,
                           pfecha_saldo)
#---------------------------------------
   DEFINE
      pnss            CHAR(11)

   DEFINE
      pfecha_saldo     ,
      v_fecha_viv      DATE

   DEFINE              
      v_saldo_acc      DECIMAL(16,6),
      v_saldo_pes      DECIMAL(16,6),
      v_precio_viv     DECIMAL(19,14),
      v_precio_fov     DECIMAL(19,14)

   DEFINE
      psubcuenta       ,
      pgrupo           ,
      v_tipo           ,
      v_subcuenta      ,
      v_siefore        ,
      num_siefore_g    ,
      num_siefore_t    ,
      num_siefore_v    SMALLINT

   SELECT precio_del_dia
   INTO   v_precio_fov
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = pfecha_saldo
   AND    codigo_siefore = 12

   IF SQLCA.SQLCODE <> 0 THEN
     DISPLAY "NO EXISTE EL PRECIO DE VIVIENDA DEL :",pfecha_saldo
   END IF

   WHENEVER ERROR CONTINUE
      DROP TABLE exc_tmp_saldo_viv
   WHENEVER ERROR STOP

   SELECT subcuenta    ,
          siefore      ,
          NVL(SUM(monto_en_acciones),0) monto_en_acciones,
          NVL(SUM(monto_en_pesos),0) monto_en_pesos
   FROM   dis_cuenta
   WHERE  nss = pnss
   AND    subcuenta = psubcuenta
   AND    fecha_conversion <= pfecha_saldo
   GROUP BY 1,2
   ORDER BY 1,2
   INTO TEMP exc_tmp_saldo_viv

   SELECT subcuenta    ,
          siefore      ,
          monto_en_acciones,
          monto_en_acciones * v_precio_fov
   INTO   v_subcuenta,
          v_siefore,
          v_saldo_acc,
          v_saldo_pes
   FROM   exc_tmp_saldo_viv
   WHERE  siefore = 12
   AND    monto_en_acciones <> 0
   ORDER BY 1,2 DESC

   RETURN v_saldo_acc,
          v_saldo_pes

END FUNCTION


FUNCTION genera_tmp_cuenta (p_nss)
#---------------------------------

   DEFINE p_nss            CHAR(11),
          v_nombre_tabla   CHAR(20),
          sel_his          CHAR(5000)

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta;
   WHENEVER ERROR STOP

   DECLARE cur_his CURSOR FOR
   SELECT  tabname
   FROM    systables
   WHERE   tabname MATCHES "dis_cuenta??"

   FOREACH cur_his INTO v_nombre_tabla

      IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
         LET sel_his = sel_his CLIPPED,
                       " SELECT * ",
                       " FROM ",v_nombre_tabla,
                       " WHERE nss = ","'",p_nss,"'"  ,
                       " AND    subcuenta IN (1,2,4) ",
                       " AND   tipo_movimiento NOT IN (888,999) ",
                       " UNION ALL "
      ELSE
         LET sel_his = sel_his CLIPPED,
                       " SELECT * ",
                       " FROM ",v_nombre_tabla,
                       " WHERE nss = ","'",p_nss,"'"  ,
                       " AND    subcuenta IN (13,14,19,30,31,33,35) ",
                       " AND   tipo_movimiento NOT IN (888,999) ",
                       " UNION ALL "
      END IF
   END FOREACH
   CLOSE cur_his

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#
      LET sel_his = sel_his CLIPPED,
                    " SELECT * ",
                    " FROM   dis_cuenta ",
                    " WHERE  nss = ","'",p_nss,"'"  ,
                    " AND    subcuenta IN (1,2,4) ",
                    " AND    tipo_movimiento NOT IN (888,999) ",
                    " INTO TEMP tmp_dis_cuenta "
   ELSE
      LET sel_his = sel_his CLIPPED,
                    " SELECT * ",
                    " FROM   dis_cuenta ",
                    " WHERE  nss = ","'",p_nss,"'"  ,
                    " AND    subcuenta IN (13,14,19,30,31,33,35) ",
                    " AND    tipo_movimiento NOT IN (888,999) ",
                    " INTO TEMP tmp_dis_cuenta "
   END IF

   LET sel_his = sel_his CLIPPED

   PREPARE eje_sel_his FROM sel_his

   EXECUTE eje_sel_his

   CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta(folio,
                                                  consecutivo_lote,
                                                  subcuenta,
                                                  siefore
                                                 )
   CREATE INDEX tmp_dis_cuenta2 ON tmp_dis_cuenta(subcuenta)

   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION


FUNCTION saldo_actualizado(v_nss,
                           v_subcuenta,
                           v_siefore,
                           v_monto_acc,
                           v_monto_pes)
#---------------------------------------

   DEFINE
      v_saldo_dia        CHAR(100),
      v_nss              CHAR(11)

   DEFINE
      hoy                ,
      vfecha_saldo       DATE

   DEFINE
      f_monto_acc        DECIMAL(16,6),
      f_monto_pes        DECIMAL(16,6),
      v_monto_acc        DECIMAL(16,6),
      v_monto_pes        DECIMAL(16,6),
      v_saldo_acc        DECIMAL(16,6),
      v_saldo_pes        DECIMAL(16,6)

   DEFINE
      li_flag            ,
      aux_bandera        ,
      v_subcuenta        ,
      v_grupo            ,
      v_siefore          ,
      f_subcuenta        ,
      f_siefore          SMALLINT


   LET hoy     = TODAY
   LET li_flag = 0
   LET v_grupo = 0

-- VERIFICA TABLA TEMPORAL
   WHENEVER ERROR CONTINUE
     SELECT COUNT(*)
     FROM temp_saldo_act
   WHENEVER ERROR STOP

   IF SQLCA.SQLCODE != 0 THEN
      CREATE TEMP TABLE temp_saldo_act
      (nss            CHAR(11),
       subcuenta      SMALLINT,
       siefore        SMALLINT,
       acciones       DECIMAL(16,6),
       pesos          DECIMAL(16,6)
      )
   END IF

-- VERIFICA SI EXISTE NSS
   SELECT COUNT(*)
   INTO li_flag
   FROM temp_saldo_act
   WHERE nss = v_nss

   IF li_flag = 0 THEN
-- INSERTA SALDO DE NSS

      LET li_flag = 0

      IF v_subcuenta = 14 OR
         v_subcuenta = 35 THEN

         LET vfecha_saldo = MDY(MONTH(hoy),1,YEAR(hoy))

         LET vfecha_saldo = vfecha_saldo + 1 UNITS MONTH

         CALL fn_saldo_dia_viv(v_nss,v_subcuenta,v_grupo,vfecha_saldo)
         RETURNING v_saldo_acc,
                   v_saldo_pes

         INSERT INTO temp_saldo_act
         VALUES (v_nss,
                 v_subcuenta,
                 12,
                 v_saldo_acc,
                 v_saldo_pes
                )

         UPDATE STATISTICS FOR TABLE temp_saldo_act
      ELSE

         LET vfecha_saldo = hoy

         LET v_saldo_dia = "EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? ) "

         PREPARE eje_saldo_dia_act FROM v_saldo_dia
         DECLARE c_saldo_act CURSOR FOR  eje_saldo_dia_act
         
         FOREACH c_saldo_act USING v_nss,
                                   li_flag,  --Subcuenta 0
                                   v_grupo,  --Grupo     0
                                   vfecha_saldo
                             INTO f_subcuenta,
                                  f_siefore,
                                  f_monto_acc,
                                  f_monto_pes

            INSERT INTO temp_saldo_act
            VALUES (v_nss,
                    f_subcuenta,
                    f_siefore,
                    f_monto_acc,
                    f_monto_pes
                   )
         END FOREACH

         UPDATE STATISTICS FOR TABLE temp_saldo_act
      END IF
   END IF

   LET f_monto_acc = 0
   LET f_monto_pes = 0

-- RECUPERA SALDO ACTUAL
   SELECT acciones,
          pesos
   INTO   f_monto_acc,
          f_monto_pes
   FROM   temp_saldo_act
   WHERE  nss       = v_nss
   AND    subcuenta = v_subcuenta
   AND    siefore   = v_siefore


   IF v_subcuenta = 14 OR
      v_subcuenta = 35 THEN
      IF v_monto_acc <= f_monto_acc THEN
         LET aux_bandera = 100   #-- SUFICIENTE --#
      ELSE
         LET aux_bandera = 593   #-- INSUFICIENTE --#
      END IF
   ELSE
      IF v_monto_acc <= f_monto_acc OR
         v_monto_pes <= f_monto_pes THEN
         LET aux_bandera = 100   #-- SUFICIENTE --#
      ELSE
        LET aux_bandera = 592
      END IF
   END IF

display "v_subcuenta:",v_subcuenta 
display "v_siefore  :",v_siefore   
display "v_monto_acc:",v_monto_acc 
display "v_monto_pes:",v_monto_pes 
display "f_monto_acc:",f_monto_acc 
display "f_monto_pes:",f_monto_pes 

   RETURN aux_bandera
END FUNCTION


FUNCTION actualiza_saldos_tmp(v_nss,
                              v_subcuenta,
                              v_siefore,
                              v_monto_acc,
                              v_monto_pes)
#------------------------------------------

   DEFINE v_nss              CHAR(11),
          v_subcuenta        SMALLINT,
          v_grupo            SMALLINT,
          v_siefore          SMALLINT,
          v_monto_acc        DECIMAL(16,6),
          v_monto_pes        DECIMAL(16,6)

   DEFINE f_monto_acc        DECIMAL(16,6),
          f_monto_pes        DECIMAL(16,6)

   SELECT acciones,
          pesos
   INTO   f_monto_acc,
          f_monto_pes
   FROM   temp_saldo_act
   WHERE  nss       = v_nss
   AND    subcuenta = v_subcuenta
   AND    siefore   = v_siefore

   LET f_monto_acc = f_monto_acc - v_monto_acc
   LET f_monto_pes = f_monto_pes - v_monto_pes

   UPDATE temp_saldo_act
   SET    acciones = f_monto_acc,
          pesos    = f_monto_pes
   WHERE  nss       = v_nss
   AND    subcuenta = v_subcuenta
   AND    siefore   = v_siefore
END FUNCTION


FUNCTION valida_importes()
#-------------------------

   DEFINE
      tipo_rech   CHAR(3)

   IF g_det_isss.clave_ent_orig  = "001" THEN
      IF g_det_isss.impt_fondo_viv92 > 0 THEN
         LET tipo_rech = "327"  #-- RECHAZADO --#
      END IF

      IF g_det_isss.impt_fondo_viv08 > 0 THEN
         LET tipo_rech = "327"
      END IF

      IF generar[1,4] = "BANX" THEN
         IF g_det_isss.impt_ret_issste > 0 THEN
            LET tipo_rech = "327"
         END IF

         IF g_det_isss.impt_cv_patron > 0 THEN
            LET tipo_rech = "327"
         END IF

         IF g_det_isss.impt_ahorro_solid > 0 THEN
            LET tipo_rech = "327"
         END IF
      END IF

      RETURN tipo_rech
   END IF

   IF  g_det_isss.clave_ent_orig  = "002" THEN
      IF g_det_isss.impt_sar_issste > 0 THEN
         LET tipo_rech = "327"
      END IF

      IF g_det_isss.impt_ret_issste > 0 THEN
         LET tipo_rech = "327"
      END IF

      IF g_det_isss.impt_cv_patron > 0 THEN
         LET tipo_rech = "327"
      END IF

      IF g_det_isss.impt_ahorro_solid > 0 THEN
         LET tipo_rech = "327"
      END IF

      RETURN tipo_rech
   END IF

   LET tipo_rech = "100"  #-- ACEPTADO --#

   RETURN tipo_rech

END FUNCTION


FUNCTION valida_sufici_isss()
#----------------------------

   DEFINE
      diag_dev       CHAR(2),
      tipo_rech      CHAR(3)

   DEFINE
      vpesos         DECIMAL(16,2)
      
   DEFINE
      vban           ,
      vban1          ,
      vban2          SMALLINT

   LET vpesos = 0
   LET vban   = 0
   LET vban1  = 0
   LET vban2  = 0

   IF g_det_isss.clave_ent_orig  = "001" THEN
      IF g_det_isss.impt_sar_issste > 0 THEN

         display "g_det_isss.impt_sar_issste ",g_det_isss.impt_sar_issste

         IF generar[1,4] = "BANX" THEN
            SELECT NVL(SUM(pesos),0)
            INTO   vpesos
            FROM   temp_saldo_act
            WHERE  nss = g_det_isss.nss
            AND    subcuenta = 19
         ELSE
            SELECT NVL(SUM(pesos),0)
            INTO   vpesos
            FROM   temp_saldo_act
            WHERE  nss = g_det_isss.nss
            AND    subcuenta = 13
         END IF

         DISPLAY "vpesos ",vpesos

         IF vpesos >= g_det_isss.impt_sar_issste THEN
            LET vban = 1
         ELSE
            IF  vpesos  < g_det_isss.impt_sar_issste
            AND vpesos  > 0  THEN
               LET vban1 = 1
            ELSE
               LET vban2 = 1
            END IF
         END IF
      END IF

      IF g_det_isss.impt_ret_issste > 0 THEN

         display "g_det_isss.impt_ret_issste ",g_det_isss.impt_ret_issste

         SELECT NVL(SUM(pesos),0)
         INTO   vpesos
         FROM   temp_saldo_act
         WHERE  nss = g_det_isss.nss
         AND    subcuenta = 30

         display "vpesos ",vpesos

         IF vpesos >= g_det_isss.impt_ret_issste THEN
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_isss.impt_ret_issste
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
              LET vban2 = 1
            END IF
         END IF
      END IF

      IF g_det_isss.impt_cv_patron > 0 THEN

         display "g_det_isss.impt_cv_patron ",g_det_isss.impt_cv_patron

         SELECT NVL(SUM(pesos),0)
         INTO   vpesos
         FROM   temp_saldo_act
         WHERE  nss = g_det_isss.nss
         AND    subcuenta = 31

         display "vpesos ",vpesos

         IF vpesos >= g_det_isss.impt_cv_patron THEN
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_isss.impt_cv_patron
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
               LET vban2 = 1
            END IF
         END IF
      END IF

      IF g_det_isss.impt_ahorro_solid > 0 THEN

         display "g_det_isss.impt_ahorro_solid ",g_det_isss.impt_ahorro_solid

         SELECT NVL(SUM(pesos),0)
         INTO   vpesos
         FROM   temp_saldo_act
         WHERE  nss = g_det_isss.nss
         AND    subcuenta = 33

         display "vpesos ",vpesos

         IF vpesos >= g_det_isss.impt_ahorro_solid THEN    
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_isss.impt_ahorro_solid
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
              LET vban2 = 1
            END IF
         END IF
      END IF
   END IF

   IF g_det_isss.clave_ent_orig  = "002" THEN
      IF g_det_isss.impt_fondo_viv92 > 0 THEN

         display "g_det_isss.impt_fondo_viv92 ",g_det_isss.impt_fondo_viv92

         SELECT NVL(SUM(pesos),0)
         INTO   vpesos
         FROM   temp_saldo_act
         WHERE  nss = g_det_isss.nss
         AND    subcuenta = 14

         display "vpesos ",vpesos

         IF vpesos >= g_det_isss.impt_fondo_viv92 THEN
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_isss.impt_fondo_viv92
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
              LET vban2 = 1
            END IF
         END IF

         display "vban ",vban
         display "vban1 ",vban1
      END IF

      IF g_det_isss.impt_fondo_viv08 > 0 THEN

         display "g_det_isss.impt_fondo_viv08 ",g_det_isss.impt_fondo_viv08

         SELECT NVL(SUM(pesos),0)
         INTO   vpesos
         FROM   temp_saldo_act
         WHERE  nss = g_det_isss.nss
         AND    subcuenta = 35

         display "vpesos ",vpesos

         IF vpesos >= g_det_isss.impt_fondo_viv08 THEN
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_isss.impt_fondo_viv08
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
              LET vban2 = 1
            END IF
         END IF
      END IF
   END IF

   IF vban1 = 1 THEN
      LET tipo_rech = "100"
      LET diag_dev = "04"   #-- PARCIAL --#

      UPDATE exc_det_exceso_issste
      SET    result_operacion = diag_dev
      WHERE  curp = g_det_isss.curp
      AND    folio =g_det_isss.folio
      AND    consec_reg_lote = g_det_isss.consec_reg_lote

   ELSE
      IF vban = 1 THEN
         LET tipo_rech = "100"
         LET diag_dev = "01"   #-- ACEPTADO --#
      END IF
   END IF

   display "vban2 ",vban2

   IF g_det_isss.clave_ent_orig  = "001" AND
      vban2 = 1 THEN
      LET tipo_rech = "592"  #-- RECHAZADO --#
      LET diag_dev = "02" #--RECHAZADO --#
   END IF

   IF g_det_isss.clave_ent_orig  = "002" AND
      vban2 = 1 THEN    
      LET tipo_rech = "593"  #-- RECHAZADO --#
      LET diag_dev = "02" #--RECHAZADO --#    	
   END IF

   RETURN tipo_rech,diag_dev

END FUNCTION


FUNCTION valida_sufici_imss()
#----------------------------

   DEFINE
      diag_dev       CHAR(2),
      tipo_rech      CHAR(3)

   DEFINE
      vfecha_valor   DATE

   DEFINE
     vpesos          DECIMAL(16,2)

   DEFINE
      vban           ,
      vban1          ,
      vban2          SMALLINT

   LET vpesos = 0
   LET vban   = 0
   LET vban1  = 0
   LET vban2  = 0

   IF g_det_imss.clave_ent_orig  = "001" THEN
      IF g_det_imss.monto_ret > 0 THEN

         display "g_det_imss.monto_ret ",g_det_imss.monto_ret

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
         INTO   vpesos
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 1
         AND    A.tipo_movimiento = 1
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy 

         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

         display  "vpesos ",vpesos
                  
         IF vpesos >= g_det_imss.monto_ret THEN
            LET vban = 1
         ELSE
            IF  vpesos  < g_det_imss.monto_ret
            AND vpesos  > 0  THEN
               LET vban1 = 1
            ELSE
               LET vban2 = 1
            END IF
         END IF
      END IF

      IF g_det_imss.monto_act_ret > 0 THEN

         display "g_det_imss.monto_act_ret ",g_det_imss.monto_act_ret

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
         INTO   vpesos
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 1
         AND    A.tipo_movimiento = 2
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy

         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

         display "vpesos ",vpesos
                  
         IF vpesos >= g_det_imss.monto_act_ret THEN
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_imss.monto_act_ret
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
              LET vban2 = 1
            END IF
         END IF
      END IF

      IF g_det_imss.mto_act_ret_rend > 0 THEN

         display "g_det_imss.mto_act_ret_rend ",g_det_imss.mto_act_ret_rend

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
         INTO   vpesos
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 1
         AND    A.tipo_movimiento = 3
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy

         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

        display "vpesos ",vpesos
                 
         IF vpesos >= g_det_imss.mto_act_ret_rend THEN
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_imss.mto_act_ret_rend
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
               LET vban2 = 1
            END IF
         END IF
      END IF

      IF g_det_imss.monto_ces_vej_pat > 0 THEN

         display "g_det_imss.monto_ces_vej_pat ",g_det_imss.monto_ces_vej_pat

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
         INTO   vpesos
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 2
         AND    A.tipo_movimiento = 1
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy

         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF
         
         display "vpesos ",vpesos
                  
         IF vpesos >= g_det_imss.monto_ces_vej_pat THEN
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_imss.monto_ces_vej_pat
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
              LET vban2 = 1
            END IF
         END IF
      END IF

      IF g_det_imss.monto_act_cv_pat > 0 THEN

         display "g_det_imss.monto_act_cv_pat ",g_det_imss.monto_act_cv_pat

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
         INTO   vpesos
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 2
         AND    A.tipo_movimiento = 2
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy

         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

         display "vpesos ",vpesos
                  
         IF vpesos >= g_det_imss.monto_act_cv_pat THEN
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_imss.monto_act_cv_pat
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
              LET vban2 = 1
            END IF
         END IF
      END IF

      IF g_det_imss.mto_act_cv_rend_pat > 0 THEN

         display "g_det_imss.mto_act_cv_rend_pat ",g_det_imss.mto_act_cv_rend_pat

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
         INTO   vpesos
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 2
         AND    A.tipo_movimiento = 3
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy


         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

         display "vpesos ",vpesos
         
         IF vpesos >= g_det_imss.mto_act_cv_rend_pat THEN
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_imss.mto_act_cv_rend_pat
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
              LET vban2 = 1
            END IF
         END IF
      END IF
   END IF

   IF g_det_imss.clave_ent_orig  = "002" THEN
      IF g_det_imss.monto_aport_pat  > 0 THEN

         display "g_det_imss.monto_aport_pat  ",g_det_imss.monto_aport_pat

         LET vfecha_valor = MDY(MONTH(hoy),1,YEAR(hoy))

         LET vfecha_valor = vfecha_valor + 1 UNITS MONTH

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
         INTO   vpesos
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 4
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = vfecha_valor

         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF
         
         display "vpesos ",vpesos
                  
         IF vpesos >= g_det_imss.monto_aport_pat THEN
            LET vban = 1
            LET vban2 = 0
         ELSE
            IF  vpesos  < g_det_imss.monto_aport_pat 
            AND vpesos  > 0  THEN
               LET vban1 = 1
               LET vban2 = 0
            ELSE
              LET vban2 = 1
            END IF
         END IF

         display "vban ",vban
         display "vban1 ",vban1
      END IF
   END IF

   IF vban1 = 1 THEN
      LET tipo_rech = "100"
      LET diag_dev = "04"   #-- PARCIAL --#

      UPDATE exc_det_exceso
      SET    result_operacion = diag_dev
      WHERE  nss = g_det_imss.nss
      AND    folio =g_det_imss.folio
      AND    consec_reg_lote = g_det_imss.consec_reg_lote

   ELSE
      IF vban = 1 THEN
         LET tipo_rech = "100"
         LET diag_dev = "01"   #-- ACEPTADO --#
      END IF
   END IF

   display "vban2 ",vban2

   IF g_det_imss.clave_ent_orig  = "001" AND
      vban2 = 1 THEN
      LET tipo_rech = "592"  #-- RECHAZADO --#
      LET diag_dev = "02" #--RECHAZADO --#
   END IF

   IF g_det_imss.clave_ent_orig  = "002" AND
      vban2 = 1 THEN    
      LET tipo_rech = "593"  #-- RECHAZADO --#
      LET diag_dev = "02" #--RECHAZADO --#    	
   END IF
   
   RETURN tipo_rech,diag_dev

END FUNCTION


FUNCTION provisiona_cuenta_imss()
#--------------------------------

   DEFINE
      vfecha_valor       DATE

   DEFINE
      vacciones          DECIMAL(16,6),
      vpesos             DECIMAL(16,2),
      vprecio_del_dia    DECIMAL(16,6)

   DEFINE
      vcodigo_siefore    ,
      vtipo_movimiento   SMALLINT


   IF g_det_imss.clave_ent_orig  = "001" THEN

      IF g_det_imss.monto_ret > 0 THEN

         LET vtipo_movimiento = 540

         display "g_det_imss.monto_ret ",g_det_imss.monto_ret


         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                NVL(SUM(A.monto_en_acciones),0)
         INTO   vpesos,
                vacciones
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 1
         AND    A.tipo_movimiento = 1
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy 


         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF
 
         display "vpesos ",vpesos
         display "vaciones ",vacciones

        
         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_imss.nss
         AND    a.subcuenta = 1
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = hoy

         IF vpesos >= g_det_imss.monto_ret THEN

            LET vacciones = g_det_imss.monto_ret / vprecio_del_dia

            CALL provisiona_cuenta(g_det_imss.monto_ret,        --- pesos
                                   vacciones,                   --- acciones
                                   g_det_imss.nss,              --- solicitud
                                   g_det_imss.consec_reg_lote,
                                   g_det_imss.folio,
                                   g_det_imss.curp,
                                   1,                           --- subcuenta
                                   vtipo_movimiento,            --- tipo_movimiento
                                   vprecio_del_dia,             --- precio accion
                                   hoy,                         --- fecha valuacion
                                   vcodigo_siefore)             --- siefore

         ELSE
            IF  vpesos  < g_det_imss.monto_ret
            AND vpesos  > 0  THEN
                CALL provisiona_cuenta(vpesos,                      --- pesos
                                       vacciones,                   --- acciones
                                       g_det_imss.nss,              --- solicitud
                                       g_det_imss.consec_reg_lote,
                                       g_det_imss.folio,
                                       g_det_imss.curp,
                                       1,                           --- subcuenta
                                       vtipo_movimiento,            --- tipo_movimiento
                                       vprecio_del_dia,             --- precio accion
                                       hoy,                         --- fecha valuacion
                                       vcodigo_siefore)             --- siefore
            END IF
         END IF
      END IF


      IF g_det_imss.monto_act_ret > 0 THEN

         LET vtipo_movimiento = 545

         display "g_det_imss.monto_act_ret ",g_det_imss.monto_act_ret


         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                NVL(SUM(A.monto_en_acciones),0)
         INTO   vpesos,
                vacciones
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 1
         AND    A.tipo_movimiento = 2
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy


         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

         display "vpesos ",vpesos
         display "vaciones ",vacciones
                  
         SELECT a.codigo_siefore ,
                b.precio_del_dia
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_imss.nss
         AND    a.subcuenta = 1
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = hoy

         IF vpesos >= g_det_imss.monto_act_ret THEN

            LET vacciones = g_det_imss.monto_act_ret / vprecio_del_dia
            
            CALL provisiona_cuenta(g_det_imss.monto_act_ret,    --- pesos
                                   vacciones,                   --- acciones
                                   g_det_imss.nss,              --- solicitud
                                   g_det_imss.consec_reg_lote,
                                   g_det_imss.folio,
                                   g_det_imss.curp,
                                   1,                           --- subcuenta
                                   vtipo_movimiento,            --- tipo_movimiento
                                   vprecio_del_dia,             --- precio accion
                                   hoy,                         --- fecha valuacion
                                   vcodigo_siefore)             --- siefore
         ELSE
            IF  vpesos  < g_det_imss.monto_act_ret
            AND vpesos  > 0  THEN

               CALL provisiona_cuenta(vpesos,                       --- pesos
                                      vacciones,                    --- acciones
                                       g_det_imss.nss,              --- solicitud
                                       g_det_imss.consec_reg_lote,
                                       g_det_imss.folio,
                                       g_det_imss.curp,
                                      1,                            --- subcuenta
                                      vtipo_movimiento,             --- tipo_movimiento
                                      vprecio_del_dia,              --- precio accion
                                      hoy,                          --- fecha valuacion
                                      vcodigo_siefore)              --- siefore
            END IF
         END IF
      END IF


      IF g_det_imss.mto_act_ret_rend > 0 THEN

         LET vtipo_movimiento = 550

         display "g_det_imss.mto_act_ret_rend ",g_det_imss.mto_act_ret_rend

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                NVL(SUM(A.monto_en_acciones),0)
         INTO   vpesos,
                vacciones
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 1
         AND    A.tipo_movimiento = 3
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy

         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

         display "vpesos ",vpesos
         display "vaciones ",vacciones

         
         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_imss.nss
         AND    a.subcuenta = 1
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = hoy

         IF vpesos >= g_det_imss.mto_act_ret_rend THEN

            LET vacciones = g_det_imss.mto_act_ret_rend / vprecio_del_dia

            CALL provisiona_cuenta(g_det_imss.mto_act_ret_rend, --- pesos
                                   vacciones,                   --- acciones
                                   g_det_imss.nss,              --- solicitud
                                   g_det_imss.consec_reg_lote,
                                   g_det_imss.folio,
                                   g_det_imss.curp,
                                   1,                           --- subcuenta
                                   vtipo_movimiento,            --- tipo_movimiento
                                   vprecio_del_dia,             --- precio accion
                                   hoy,                         --- fecha valuacion
                                   vcodigo_siefore)             --- siefore
         ELSE
            IF  vpesos  < g_det_imss.mto_act_ret_rend
            AND vpesos  > 0  THEN

               CALL provisiona_cuenta(vpesos,                      --- pesos
                                      vacciones,                   --- acciones
                                      g_det_imss.nss,              --- solicitud
                                      g_det_imss.consec_reg_lote,
                                      g_det_imss.folio,
                                      g_det_imss.curp,
                                      1,                           --- subcuenta
                                      vtipo_movimiento,            --- tipo_movimiento
                                      vprecio_del_dia,             --- precio accion
                                      hoy,                         --- fecha valuacion
                                      vcodigo_siefore)             --- siefore
            END IF
         END IF
      END IF


      IF g_det_imss.monto_ces_vej_pat   > 0 THEN

         LET vtipo_movimiento = 540

         display "g_det_imss.monto_ces_vej_pat ",g_det_imss.monto_ces_vej_pat

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                NVL(SUM(A.monto_en_acciones),0)
         INTO   vpesos,
                vacciones
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 2
         AND    A.tipo_movimiento = 1
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy


         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

         display "vpesos    ",vpesos
         display "vacciones ",vacciones
                  
         
         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_imss.nss
         AND    a.subcuenta = 2
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = hoy

         IF vpesos >= g_det_imss.monto_ces_vej_pat THEN
            LET vacciones = g_det_imss.monto_ces_vej_pat / vprecio_del_dia

            CALL provisiona_cuenta(g_det_imss.monto_ces_vej_pat,   --- pesos
                                   vacciones,                      --- acciones
                                   g_det_imss.nss,                 --- solicitud
                                   g_det_imss.consec_reg_lote,
                                   g_det_imss.folio,
                                   g_det_imss.curp,
                                   2,                              --- subcuenta
                                   vtipo_movimiento,               --- tipo_movimiento
                                   vprecio_del_dia,                --- precio accion
                                   hoy,                            --- fecha valuacion
                                   vcodigo_siefore)                --- siefore
         ELSE
            IF  vpesos  < g_det_imss.monto_ces_vej_pat
            AND vpesos  > 0  THEN
                CALL provisiona_cuenta(vpesos,                      --- pesos
                                       vacciones,                   --- acciones
                                       g_det_imss.nss,              --- solicitud
                                       g_det_imss.consec_reg_lote,
                                       g_det_imss.folio,
                                       g_det_imss.curp,
                                       2,                           --- subcuenta
                                       vtipo_movimiento,            --- tipo_movimiento
                                       vprecio_del_dia,             --- precio accion
                                       hoy,                         --- fecha valuacion
                                       vcodigo_siefore)             --- siefore
            END IF
         END IF
      END IF


      IF g_det_imss.monto_act_cv_pat   > 0 THEN

         LET vtipo_movimiento = 545

         display "g_det_imss.monto_act_cv_pat ",g_det_imss.monto_act_cv_pat

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                NVL(SUM(A.monto_en_acciones),0)
         INTO   vpesos,
                vacciones
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 2
         AND    A.tipo_movimiento = 2
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy


         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

         display "vpesos    ",vpesos
         display "vacciones ",vacciones
         
                  
         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_imss.nss
         AND    a.subcuenta = 2
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = hoy

         IF vpesos >= g_det_imss.monto_act_cv_pat THEN
            LET vacciones = g_det_imss.monto_act_cv_pat / vprecio_del_dia

            CALL provisiona_cuenta(g_det_imss.monto_act_cv_pat,    --- pesos
                                   vacciones,                      --- acciones
                                   g_det_imss.nss,                 --- solicitud
                                   g_det_imss.consec_reg_lote,
                                   g_det_imss.folio,
                                   g_det_imss.curp,
                                   2,                              --- subcuenta
                                   vtipo_movimiento,               --- tipo_movimiento
                                   vprecio_del_dia,                --- precio accion
                                   hoy,                            --- fecha valuacion
                                   vcodigo_siefore)                --- siefore
         ELSE
            IF  vpesos  < g_det_imss.monto_act_cv_pat
            AND vpesos  > 0  THEN
                CALL provisiona_cuenta(vpesos,                         --- pesos
                                       vacciones,                      --- acciones
                                       g_det_imss.nss,                 --- solicitud
                                       g_det_imss.consec_reg_lote,
                                       g_det_imss.folio,
                                       g_det_imss.curp,
                                       2,                              --- subcuenta
                                       vtipo_movimiento,               --- tipo_movimiento
                                       vprecio_del_dia,                --- precio accion
                                       hoy,                            --- fecha valuacion
                                       vcodigo_siefore)                --- siefore
            END IF
         END IF
      END IF


      IF g_det_imss.mto_act_cv_rend_pat   > 0 THEN

         LET vtipo_movimiento = 550

         display "g_det_imss.mto_act_cv_rend_pat ",g_det_imss.mto_act_cv_rend_pat


         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                NVL(SUM(A.monto_en_acciones),0)
         INTO   vpesos,
                vacciones
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 2
         AND    A.tipo_movimiento = 3
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = hoy


         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

         display "vpesos    ",vpesos
         display "vacciones ",vacciones
                  
         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_imss.nss
         AND    a.subcuenta = 2
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = hoy

         IF vpesos >= g_det_imss.mto_act_cv_rend_pat THEN
            LET vacciones = g_det_imss.mto_act_cv_rend_pat / vprecio_del_dia

            CALL provisiona_cuenta(g_det_imss.mto_act_cv_rend_pat, --- pesos
                                   vacciones,                      --- acciones
                                   g_det_imss.nss,                 --- solicitud
                                   g_det_imss.consec_reg_lote,
                                   g_det_imss.folio,
                                   g_det_imss.curp,
                                   2,                              --- subcuenta
                                   vtipo_movimiento,               --- tipo_movimiento
                                   vprecio_del_dia,                --- precio accion
                                   hoy,                            --- fecha valuacion
                                   vcodigo_siefore)                --- siefore
         ELSE
            IF  vpesos  < g_det_imss.mto_act_cv_rend_pat
            AND vpesos  > 0  THEN
                CALL provisiona_cuenta(vpesos,                         --- pesos
                                       vacciones,                      --- acciones
                                       g_det_imss.nss,                 --- solicitud
                                       g_det_imss.consec_reg_lote,
                                       g_det_imss.folio,
                                       g_det_imss.curp,
                                       2,                              --- subcuenta
                                       vtipo_movimiento,               --- tipo_movimiento
                                       vprecio_del_dia,                --- precio accion
                                       hoy,                            --- fecha valuacion
                                       vcodigo_siefore)                --- siefore
            END IF
         END IF
      END IF
   END IF


   IF g_det_imss.clave_ent_orig  = "002" THEN
      IF g_det_imss.monto_aport_pat  > 0 THEN

         LET vtipo_movimiento = 540

         display "g_det_imss.monto_aport_pat ",g_det_imss.monto_aport_pat

         LET vfecha_valor = MDY(MONTH(hoy),1,YEAR(hoy))

         LET vfecha_valor = vfecha_valor + 1 UNITS MONTH

         SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                NVL(SUM(A.monto_en_acciones),0)
         INTO   vpesos,
                vacciones
         FROM   dis_cuenta A,
                glo_valor_accion B
         WHERE  A.nss = g_det_imss.nss
         AND    A.subcuenta = 4
         AND    A.siefore = B.codigo_siefore
         AND    B.fecha_valuacion = vfecha_valor


         IF vpesos IS  NULL
         OR vpesos = "" THEN
            LET vpesos = 0
         END IF

         display "vpesos    ",vpesos
         DISPLAY "vacciones ",vacciones
                  
         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_imss.nss
         AND    a.subcuenta = 4
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = vfecha_valor

         IF vpesos >= g_det_imss.monto_aport_pat THEN

            LET vacciones = g_det_imss.monto_aport_pat / vprecio_del_dia

            CALL provisiona_cuenta(g_det_imss.monto_aport_pat,  --- pesos
                                   vacciones,                   --- acciones
                                   g_det_imss.nss,              --- solicitud
                                   g_det_imss.consec_reg_lote,
                                   g_det_imss.folio,
                                   g_det_imss.curp,
                                   4,                           --- subcuenta
                                   vtipo_movimiento,            --- tipo_movimiento
                                   vprecio_del_dia,             --- precio accion
                                   vfecha_valor,                --- fecha valuacion
                                   vcodigo_siefore)             --- siefore
         ELSE
            IF  vpesos  < g_det_imss.monto_aport_pat
            AND vpesos  > 0  THEN
                CALL provisiona_cuenta(vpesos,                         --- pesos
                                       vacciones,                      --- acciones
                                       g_det_imss.nss,                 --- solicitud
                                       g_det_imss.consec_reg_lote,
                                       g_det_imss.folio,
                                       g_det_imss.curp,
                                       4,                              --- subcuenta
                                       vtipo_movimiento,               --- tipo_movimiento
                                       vprecio_del_dia,                --- precio accion
                                       vfecha_valor,                   --- fecha valuacion
                                       vcodigo_siefore)                --- siefore
            END IF
         END IF
      END IF
   END IF

END FUNCTION


FUNCTION provisiona_cuenta_issste()
#----------------------------------

   DEFINE
      vfecha_valor       DATE

   DEFINE
      vacciones          DECIMAL(16,6),
      vpesos             DECIMAL(16,6),
      vprecio_del_dia    DECIMAL(16,6)

   DEFINE
      vcodigo_siefore    ,
      vtipo_movimiento   SMALLINT

   IF g_det_isss.clave_ent_orig  = "001" AND
      diag_dev = "01" THEN   #-- ACEPTADO --#
      LET vtipo_movimiento = 543 --- LIQUIDA RCV TOTAL
   END IF

   IF g_det_isss.clave_ent_orig  = "001" AND
       diag_dev = "04" THEN
      LET vtipo_movimiento = 544 --- LIQUIDA RCV PARCIAL
   END IF

   IF g_det_isss.clave_ent_orig  = "002" AND
      diag_dev = "01" THEN   #-- ACEPTADO --#
      LET vtipo_movimiento = 553 --- LIQUIDA FOND VIV TOTAL
   END IF

   IF g_det_isss.clave_ent_orig  = "002" AND
       diag_dev = "04" THEN
      LET vtipo_movimiento = 554 --- LIQUIDA FOND VIV PARCIAL
   END IF

   IF g_det_isss.clave_ent_orig  = "001" THEN
      IF g_det_isss.impt_sar_issste > 0 THEN

         display g_det_isss.impt_sar_issste

         IF generar[1,4] = "BANX" THEN
            SELECT NVL(SUM(pesos),0),
                   NVL(SUM(acciones),0)
            INTO   vpesos,
                   vacciones
            FROM   temp_saldo_act
            WHERE  nss = g_det_isss.nss
            AND    subcuenta = 19

            display "vpesos ",vpesos
            display "vaciones ",vacciones

            IF vpesos >= g_det_isss.impt_sar_issste THEN

               CALL provisiona_cuenta(g_det_isss.impt_sar_issste,  --- pesos
                                      0,                           --- acciones
                                      g_det_isss.nss,              --- solicitud
                                      g_det_isss.consec_reg_lote,
                                      g_det_isss.folio,
                                      g_det_isss.curp,
                                      19,                          --- subcuenta
                                      vtipo_movimiento,            --- tipo_movimiento
                                      0,                           --- precio accion
                                      hoy,                         --- fecha valuacion
                                      0)                           --- siefore
            ELSE
               IF  vpesos  < g_det_isss.impt_sar_issste
               AND vpesos  > 0  THEN
                   CALL provisiona_cuenta(vpesos,                      --- pesos
                                          0,                           --- acciones
                                          g_det_isss.nss,              --- solicitud
                                          g_det_isss.consec_reg_lote,
                                          g_det_isss.folio,
                                          g_det_isss.curp,
                                          19,                          --- subcuenta
                                          vtipo_movimiento,            --- tipo_movimiento
                                          0,                           --- precio accion
                                          hoy,                         --- fecha valuacion
                                          0)                           --- siefore
               END IF
            END IF
         ELSE
            SELECT NVL(SUM(pesos),0),
                   NVL(SUM(acciones),0)
            INTO   vpesos,
                   vacciones
            FROM   temp_saldo_act
            WHERE  nss = g_det_isss.nss
            AND    subcuenta = 13

            display "vpesos ",vpesos
            display "vaciones ",vacciones

            SELECT a.codigo_siefore ,
                   b.precio_del_dia 
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_det_isss.nss
            AND    a.subcuenta = 13
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy

            IF vpesos >= g_det_isss.impt_sar_issste THEN

               LET vacciones = g_det_isss.impt_sar_issste / vprecio_del_dia

               CALL provisiona_cuenta(g_det_isss.impt_sar_issste,  --- pesos
                                      vacciones,                   --- acciones
                                      g_det_isss.nss,              --- solicitud
                                      g_det_isss.consec_reg_lote,
                                      g_det_isss.folio,
                                      g_det_isss.curp,
                                      13,                          --- subcuenta
                                      vtipo_movimiento,            --- tipo_movimiento
                                      vprecio_del_dia,             --- precio accion
                                      hoy,                         --- fecha valuacion
                                      vcodigo_siefore)             --- siefore
            ELSE
               IF  vpesos  < g_det_isss.impt_sar_issste
               AND vpesos  > 0  THEN
                   CALL provisiona_cuenta(vpesos,                      --- pesos
                                          vacciones,                   --- acciones
                                          g_det_isss.nss,              --- solicitud
                                          g_det_isss.consec_reg_lote,
                                          g_det_isss.folio,
                                          g_det_isss.curp,
                                          13,                          --- subcuenta
                                          vtipo_movimiento,            --- tipo_movimiento
                                          vprecio_del_dia,             --- precio accion
                                          hoy,                         --- fecha valuacion
                                          vcodigo_siefore)             --- siefore
               END IF
            END IF
         END IF
      END IF

      IF g_det_isss.impt_ret_issste > 0 THEN

         display g_det_isss.impt_ret_issste

         SELECT NVL(SUM(pesos),0),
                NVL(SUM(acciones),0)
         INTO   vpesos,
                vacciones
         FROM   temp_saldo_act
         WHERE  nss = g_det_isss.nss
         AND    subcuenta = 30

         display "vpesos ",vpesos
         display "vaciones ",vacciones

         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_isss.nss
         AND    a.subcuenta = 30
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = hoy

         IF vpesos >= g_det_isss.impt_ret_issste THEN

            LET vacciones = g_det_isss.impt_ret_issste / vprecio_del_dia
            
            CALL provisiona_cuenta(g_det_isss.impt_ret_issste,  --- pesos
                                   vacciones,                   --- acciones
                                   g_det_isss.nss,              --- solicitud
                                   g_det_isss.consec_reg_lote,
                                   g_det_isss.folio,
                                   g_det_isss.curp,
                                   30,                          --- subcuenta
                                   vtipo_movimiento,            --- tipo_movimiento
                                   vprecio_del_dia,             --- precio accion
                                   hoy,                         --- fecha valuacion
                                   vcodigo_siefore)             --- siefore
         ELSE
            IF  vpesos  < g_det_isss.impt_ret_issste
            AND vpesos  > 0  THEN

               CALL provisiona_cuenta(vpesos,                      --- pesos
                                      vacciones,                   --- acciones
                                      g_det_isss.nss,              --- solicitud
                                      g_det_isss.consec_reg_lote,
                                      g_det_isss.folio,
                                      g_det_isss.curp,
                                      30,                          --- subcuenta
                                      vtipo_movimiento,            --- tipo_movimiento
                                      vprecio_del_dia,             --- precio accion
                                      hoy,                         --- fecha valuacion
                                      vcodigo_siefore)             --- siefore
            END IF
         END IF
      END IF

      IF g_det_isss.impt_cv_patron > 0 THEN

         display g_det_isss.impt_cv_patron

         SELECT NVL(SUM(pesos),0),
                NVL(SUM(acciones),0)
         INTO   vpesos,
                vacciones
         FROM   temp_saldo_act
         WHERE  nss = g_det_isss.nss
         AND    subcuenta = 31

         display "vpesos ",vpesos
         display "vaciones ",vacciones

         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_isss.nss
         AND    a.subcuenta = 31
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = hoy

         IF vpesos >= g_det_isss.impt_cv_patron THEN

            LET vacciones = g_det_isss.impt_cv_patron / vprecio_del_dia

            CALL provisiona_cuenta(g_det_isss.impt_cv_patron,   --- pesos
                                   vacciones,                   --- acciones
                                   g_det_isss.nss,              --- solicitud
                                   g_det_isss.consec_reg_lote,
                                   g_det_isss.folio,
                                   g_det_isss.curp,
                                   31,                          --- subcuenta
                                   vtipo_movimiento,            --- tipo_movimiento
                                   vprecio_del_dia,             --- precio accion
                                   hoy,                         --- fecha valuacion
                                   vcodigo_siefore)             --- siefore
         ELSE
            IF  vpesos  < g_det_isss.impt_cv_patron
            AND vpesos  > 0  THEN

               CALL provisiona_cuenta(vpesos,                      --- pesos
                                      vacciones,                   --- acciones
                                      g_det_isss.nss,              --- solicitud
                                      g_det_isss.consec_reg_lote,
                                      g_det_isss.folio,
                                      g_det_isss.curp,
                                      31,                          --- subcuenta
                                      vtipo_movimiento,            --- tipo_movimiento
                                      vprecio_del_dia,             --- precio accion
                                      hoy,                         --- fecha valuacion
                                      vcodigo_siefore)             --- siefore
            END IF
         END IF
      END IF

      IF g_det_isss.impt_ahorro_solid > 0 THEN

         display "g_det_isss.impt_ahorro_solid ",g_det_isss.impt_ahorro_solid      	

         SELECT NVL(SUM(pesos),0),
                NVL(SUM(acciones),0)
         INTO   vpesos,
                vacciones
         FROM   temp_saldo_act
         WHERE  nss = g_det_isss.nss
         AND    subcuenta = 33

         display "vpesos    ",vpesos
         display "vacciones ",vacciones

         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_isss.nss
         AND    a.subcuenta = 33
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = hoy

         IF vpesos >= g_det_isss.impt_ahorro_solid THEN
            LET vacciones = g_det_isss.impt_ahorro_solid / vprecio_del_dia

            CALL provisiona_cuenta(g_det_isss.impt_ahorro_solid,   --- pesos
                                   vacciones,                      --- acciones
                                   g_det_isss.nss,                 --- solicitud
                                   g_det_isss.consec_reg_lote,
                                   g_det_isss.folio,
                                   g_det_isss.curp,
                                   33,                             --- subcuenta
                                   vtipo_movimiento,               --- tipo_movimiento
                                   vprecio_del_dia,                --- precio accion
                                   hoy,                            --- fecha valuacion
                                   vcodigo_siefore)                --- siefore
         ELSE
            IF  vpesos  < g_det_isss.impt_ahorro_solid
            AND vpesos  > 0  THEN
                CALL provisiona_cuenta(vpesos,                      --- pesos
                                       vacciones,                   --- acciones
                                       g_det_isss.nss,              --- solicitud
                                       g_det_isss.consec_reg_lote,
                                       g_det_isss.folio,
                                       g_det_isss.curp,
                                       33,                          --- subcuenta
                                       vtipo_movimiento,            --- tipo_movimiento
                                       vprecio_del_dia,             --- precio accion
                                       hoy,                         --- fecha valuacion
                                       vcodigo_siefore)             --- siefore
            END IF
         END IF
      END IF
   END IF

   IF g_det_isss.clave_ent_orig  = "002" THEN
      IF g_det_isss.impt_fondo_viv92 > 0 THEN

         display "g_det_isss.impt_fondo_viv92 ",g_det_isss.impt_fondo_viv92

         SELECT NVL(SUM(pesos),0),
                NVL(SUM(acciones),0)
         INTO   vpesos,
                vacciones
         FROM   temp_saldo_act
         WHERE  nss = g_det_isss.nss
         AND    subcuenta = 14

         display "vpesos    ",vpesos
         DISPLAY "vacciones ",vacciones

         LET vfecha_valor = MDY(MONTH(hoy),1,YEAR(hoy))

         LET vfecha_valor = vfecha_valor + 1 UNITS MONTH

         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_isss.nss
         AND    a.subcuenta = 14
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = vfecha_valor

         IF vpesos >= g_det_isss.impt_fondo_viv92 THEN

            LET vacciones = g_det_isss.impt_fondo_viv92 / vprecio_del_dia

            CALL provisiona_cuenta(g_det_isss.impt_fondo_viv92, --- pesos
                                   vacciones,                   --- acciones
                                   g_det_isss.nss,              --- solicitud
                                   g_det_isss.consec_reg_lote,
                                   g_det_isss.folio,
                                   g_det_isss.curp,
                                   14,                          --- subcuenta
                                   vtipo_movimiento,            --- tipo_movimiento
                                   vprecio_del_dia,             --- precio accion
                                   vfecha_valor,                --- fecha valuacion
                                   vcodigo_siefore)             --- siefore
         ELSE
            IF  vpesos  < g_det_isss.impt_fondo_viv92
            AND vpesos  > 0  THEN
                CALL provisiona_cuenta(vpesos,                         --- pesos
                                       vacciones,                      --- acciones
                                       g_det_isss.nss,                 --- solicitud
                                       g_det_isss.consec_reg_lote,
                                       g_det_isss.folio,
                                       g_det_isss.curp,
                                       14,                             --- subcuenta
                                       vtipo_movimiento,               --- tipo_movimiento
                                       vprecio_del_dia,                --- precio accion
                                       vfecha_valor,                   --- fecha valuacion
                                       vcodigo_siefore)                --- siefore
            END IF
         END IF
      END IF

      IF g_det_isss.impt_fondo_viv08 > 0 THEN

         DISPLAY "g_det_isss.impt_fondo_viv08 ",g_det_isss.impt_fondo_viv08

         SELECT NVL(SUM(pesos),0),
                NVL(SUM(acciones),0)
         INTO   vpesos,
                vacciones
         FROM   temp_saldo_act
         WHERE  nss = g_det_isss.nss
         AND    subcuenta = 35

         display "vpesos    ",vpesos
         DISPLAY "vacciones ",vacciones

         LET vfecha_valor = MDY(MONTH(hoy),1,YEAR(hoy))

         LET vfecha_valor = vfecha_valor + 1 UNITS MONTH

         SELECT a.codigo_siefore ,
                b.precio_del_dia 
         INTO   vcodigo_siefore,
                vprecio_del_dia
         FROM   cta_regimen a, glo_valor_accion b
         WHERE  a.nss = g_det_isss.nss
         AND    a.subcuenta = 35
         AND    a.codigo_siefore = b.codigo_siefore
         AND    b.fecha_valuacion = vfecha_valor

         IF vpesos >= g_det_isss.impt_fondo_viv08 THEN

            LET vacciones = g_det_isss.impt_fondo_viv08 / vprecio_del_dia

            CALL provisiona_cuenta(g_det_isss.impt_fondo_viv08, --- pesos
                                   vacciones,                   --- acciones
                                   g_det_isss.nss,              --- solicitud
                                   g_det_isss.consec_reg_lote,
                                   g_det_isss.folio,
                                   g_det_isss.curp,
                                   35,                          --- subcuenta
                                   vtipo_movimiento,            --- tipo_movimiento
                                   vprecio_del_dia,             --- precio accion
                                   vfecha_valor,                --- fecha valuacion
                                   vcodigo_siefore)             --- siefore
         ELSE
            IF  vpesos  < g_det_isss.impt_fondo_viv08
            AND vpesos  > 0  THEN
                CALL provisiona_cuenta(vpesos,                      --- pesos
                                       vacciones,                   --- acciones
                                       g_det_isss.nss,              --- solicitud
                                       g_det_isss.consec_reg_lote,
                                       g_det_isss.folio,
                                       g_det_isss.curp,
                                       35,                          --- subcuenta
                                       vtipo_movimiento,            --- tipo_movimiento
                                       vprecio_del_dia,             --- precio accion
                                       vfecha_valor,                --- fecha valuacion
                                       vcodigo_siefore)             --- siefore
            END IF
         END IF
      END IF
   END IF
END FUNCTION


FUNCTION provisiona_cuenta(monto_reg,        --- pesos            
                           monto_acc_reg,    --- acciones         
                           reg_2,            --- solicitud        
                           subcuenta,        --- subcuenta        
                           movimiento,       --- tipo_movimiento  
                           precio_dia,       --- precio accion    
                           x_fecha_proceso,  --- fecha valuacion  
                           siefore)          --- siefore          
#----------------------------------------------------------


   DEFINE
      monto_reg           DECIMAL(16,6),
      monto_acc_reg       DECIMAL(16,6),
      subcuenta           INTEGER,
      movimiento          INTEGER,
      precio_dia          DECIMAL(16,6),
      monto_en_pesos      DECIMAL(16,6),
      monto_en_acciones   DECIMAL(16,6),
      vfecha_archivo      DATE,
      aux_id_aportante    CHAR(20),
      x_fecha_proceso     DATE,
      siefore             CHAR(6),
      x_status            SMALLINT,
      vfolio_sua          CHAR(6),
      vfecha_proceso      DATE

   DEFINE reg_2 RECORD
      nss                 CHAR(11),
      consec_reg_lote     INTEGER,
      folio               INTEGER,
      curp                CHAR(18)
   END RECORD

   DEFINE f_saldo_act  SMALLINT

   LET f_saldo_act = 0
 
   display  "reg_2.consec_reg_lote:",reg_2.consec_reg_lote

   IF vtipo_arch = 1 THEN   #-- ARCHIVO IMSS --#

      LET aux_id_aportante = "PAG-EXC-PAT"

      LET monto_en_pesos    = monto_reg * -1

      LET monto_en_acciones = monto_acc_reg * -1

      IF subcuenta = 4 THEN
         LET vfecha_proceso = hoy
      ELSE
         LET vfecha_proceso = x_fecha_proceso
      END IF

      INSERT INTO dis_provision
      VALUES (movimiento            ,
              subcuenta             ,
              siefore               ,
              reg_2.folio           ,
              reg_2.consec_reg_lote ,
              reg_2.nss             ,
              ''                    , --curp
              ''                    ,
              vfecha_proceso        , --fecha_pago
              x_fecha_proceso       , --fecha_valor
              vfecha_proceso        , --fecha_conversion
              monto_en_pesos        ,
              monto_en_acciones     ,
              precio_dia            ,
              '0'                   ,--dias cotizados
              ''                    ,--sucursal
              aux_id_aportante      ,
              '6'                   ,--estado
              hoy                   ,--fecha_proceso
              USER                  ,
              hoy                   , --fecha_archivo
              '1'                   --etiqueta
              );

   ELSE
      CALL saldo_actualizado(reg_2.nss,
                             subcuenta,
                             siefore,
                             monto_acc_reg,      --acciones
                             monto_reg)          --pesos
         RETURNING f_saldo_act

      IF f_saldo_act = 100 THEN

         LET monto_en_pesos    = monto_reg * -1

         LET monto_en_acciones = monto_acc_reg * -1

         LET aux_id_aportante = "EXC-ISSSTE"

         LET vfolio_sua = ""
         DECLARE cursor_prov_cargo CURSOR FOR clausula_sql3

         OPEN cursor_prov_cargo USING reg_2.folio,
                                      vfolio_sua,
                                      reg_2.nss,
                                      subcuenta,
                                      movimiento,
                                      reg_2.consec_reg_lote,
                                      siefore,
                                      monto_en_acciones,
                                      monto_en_pesos,
                                      aux_id_aportante,
                                      x_fecha_proceso

         FETCH cursor_prov_cargo INTO x_status

         CLOSE cursor_prov_cargo

         IF subcuenta = 14 THEN
            UPDATE exc_det_exceso_issste
            SET    aplic_int_fondo_viv92 = monto_acc_reg
            WHERE  folio = reg_2.folio
            AND    curp  = reg_2.curp
            AND    consec_reg_lote = reg_2.consec_reg_lote
            AND    result_operacion IN ("01","04")
         END IF

         IF subcuenta = 35 THEN
            UPDATE exc_det_exceso_issste
            SET    aplic_int_fondo_viv08 = monto_acc_reg
            WHERE  folio = reg_2.folio
            AND    curp  = reg_2.curp
            AND    consec_reg_lote = reg_2.consec_reg_lote
            AND    result_operacion IN ("01","04")
         END IF

         CALL actualiza_saldos_tmp(reg_2.nss,
                                   subcuenta,
                                   siefore,
                                   monto_acc_reg,  --acciones
                                   monto_reg)      --pesos
      END IF
   END IF
   ---RETURN f_saldo_act
END FUNCTION


FUNCTION reversa_parciales(vfolio)
#---------------------------------

   DEFINE
      vcurp             CHAR(18),
      vnss              CHAR(11),
      vclave_ent_orig   CHAR(3),
      vtipo_rech        CHAR(3)
      
   DEFINE
      vconsec_reg_lote  ,
      vfolio            INTEGER
      
   DEFINE
      vmarca_entra      ,
      vmarca_cod        SMALLINT

   DECLARE cur_1 CURSOR FOR
   SELECT curp
   FROM   exc_det_exceso_issste
   WHERE  folio = vfolio
   AND    result_operacion = "04"
   GROUP BY 1
   
   FOREACH cur_1 INTO vcurp

DISPLAY "REVERSO DE PARCIALES"
DISPLAY "vcurp ",vcurp
         
   	  DECLARE cur_2 CURSOR FOR
   	  SELECT consec_reg_lote,
   	         clave_ent_orig
      FROM   exc_det_exceso_issste
      WHERE  folio = vfolio
      AND    curp = vcurp
      AND    result_operacion IN ("01","04")
      
      FOREACH cur_2 INTO vconsec_reg_lote,
      	                 vclave_ent_orig
   	
   	     SELECT n_seguro
   	     INTO   vnss
   	     FROM   afi_mae_afiliado
   	     WHERE  n_unico = vcurp
   	     AND    tipo_solicitud = 8
 
         IF vclave_ent_orig = "001" THEN 	     
   	        SELECT marca_cod 
   	        INTO   vmarca_cod
   	        FROM   cta_act_marca
   	        WHERE  nss = vnss
   	        AND    correlativo = vconsec_reg_lote
            AND    marca_cod = 543
   	     ELSE
   	        SELECT marca_cod 
   	        INTO   vmarca_cod
   	        FROM   cta_act_marca
   	        WHERE  nss = vnss
   	        AND    correlativo = vconsec_reg_lote
   	        AND    marca_cod = 544
   	     END IF
   
         EXECUTE cla_rev_marca USING vnss,
                               vmarca_cod,
                               vconsec_reg_lote

         DELETE FROM dis_provision 
         WHERE  folio = vfolio
         AND    nss = vnss
         AND    consecutivo_lote = vconsec_reg_lote

         IF vclave_ent_orig  = "001" THEN
            LET vtipo_rech = "592"  #-- RECHAZADO --#
            LET vmarca_entra = 543
         END IF
         
         IF vclave_ent_orig  = "002" THEN
            LET vtipo_rech = "593"  #-- RECHAZADO --#
            LET vmarca_entra = 544
         END IF              

         CALL actualiza_reg(vfolio,
                            vcurp,
                            vnss,
                            "02",   #-- RECHAZADO --#
                            vtipo_rech,
                            vconsec_reg_lote,
                            "",
                            vmarca_entra,
                            gusuario)
      END FOREACH
   END FOREACH 
END FUNCTION
