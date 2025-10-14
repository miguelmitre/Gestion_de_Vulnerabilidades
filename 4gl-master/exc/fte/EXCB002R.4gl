#*********************************************************************#
#Proyecto          => Sistema de safre_af.( MEXICO )                  #
#Propietario       => E.F.P.                                          #
#Programa          => EXCB002                                         #
#Descripcion       => DEVOLUCION DE PAGOS EN EXCESO PROVISION         #
#Fecha Inicio      => 06 de diciembre de 2000.                        #
#Fecha Termino     => 28 de marzo de 2001    .                        #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                 #
#Actualizado       => 16 de mayo de 2002.                             #
#Actualizado       => 6 de junio de 2002.                             #
#Actualizado       => 22 de mayo de 2004.                             #
#Actualizado       => 22 de julio de 2004.                            #
#Actualizado       => 19 de octubre de 2004.                          #
#Actualizado       => 08 de diciembre de 2004.                        #
#Actualizado       => 07 de enero de 2005.                            #
#Sistema           => EXC.                                            #
#*********************************************************************#
DATABASE safre_af

GLOBALS

   DEFINE g_bat  RECORD LIKE dis_ctrl_proceso.*

   DEFINE vhora_max          CHAR(08),
          vconsecutivo         INTEGER,
          vhora_final        CHAR(08),
          vresultado         CHAR(50),
          vetapa_cod         SMALLINT,
          vproc              CHAR(06),
          vrech              CHAR(06),
          vpend              CHAR(06),
          hoy                DATE,
          generar            CHAR(25),
          gusuario           CHAR(08),
          vnom_archivo       CHAR(21),
          vreporte           CHAR(300),
          vcont_rech         INTEGER,
          vcont_acep         INTEGER,
          vcont_pend         INTEGER,
          vfecha_lote        CHAR(08),
          vtipo_reporte      CHAR(01),
          hora_inicial       CHAR(08),
          hora_final         CHAR(08),
          cla_sel            CHAR(500)

   DEFINE vfolio             INTEGER

   DEFINE gparam_dev  RECORD LIKE seg_modulo.*

   DEFINE g_reg  RECORD LIKE exc_det_exceso.*

   DEFINE det  RECORD
          monto_ret          DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_ces_vej_pat  DECIMAL(16,6),
          monto_ces_vej_tra  DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_cuo_soc      DECIMAL(16,6),
          monto_aport_est    DECIMAL(16,6),
          monto_aport_esp    DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6),
          monto_par_viv      DECIMAL(18,6)
   END RECORD

   DEFINE g_dep  RECORD
          ident_pago         CHAR(2),
          monto_soli_institu DECIMAL(16,6),
          monto_aceptado     DECIMAL(16,6),
          monto_parcial      DECIMAL(16,6),
          monto_pendiente    DECIMAL(16,6),
          monto_rechazado    DECIMAL(16,6),
          monto_par_solicitado   DECIMAL(18,6),
          monto_par_aceptado     DECIMAL(18,6),
          monto_par_parcial      DECIMAL(18,6),
          monto_par_pendiente    DECIMAL(18,6),
          monto_par_rechazado    DECIMAL(18,6),
          codigo_siefore         SMALLINT
   END RECORD

   DEFINE dep  RECORD
          monto_soli_institu DECIMAL(16,6),
          monto_aceptado     DECIMAL(16,6),
          monto_parcial      DECIMAL(16,6),
          monto_pendiente    DECIMAL(16,6),
          monto_rechazado    DECIMAL(16,6),
          monto_par_solicitado   DECIMAL(18,6),
          monto_par_aceptado     DECIMAL(18,6),
          monto_par_parcial      DECIMAL(18,6),
          monto_par_pendiente    DECIMAL(18,6),
          monto_par_rechazado    DECIMAL(18,6)
   END RECORD

   DEFINE g_sum  RECORD
          monto_tot_rcv      DECIMAL(16,6),
          monto_tot_plu_rcv  DECIMAL(16,6),
          monto_tot_min_rcv  DECIMAL(16,6),
          monto_tot_com_rcv  DECIMAL(16,6),
          monto_tot_pat      DECIMAL(16,6),
          monto_tot_gub      DECIMAL(16,6),
          monto_tot_plu_gub  DECIMAL(16,6),
          monto_tot_min_gub  DECIMAL(16,6),
          monto_tot_com_gub  DECIMAL(16,6),
          monto_tot_plu_pat  DECIMAL(16,6),
          monto_tot_par_viv  DECIMAL(18,6)
   END RECORD

   DEFINE d_sum  RECORD
          monto_tot_rcv      DECIMAL(16,6),
          monto_tot_plu_rcv  DECIMAL(16,6),
          monto_tot_min_rcv  DECIMAL(16,6),
          monto_tot_com_rcv  DECIMAL(16,6),
          monto_tot_pat      DECIMAL(16,6),
          monto_tot_gub      DECIMAL(16,6),
          monto_tot_plu_gub  DECIMAL(16,6),
          monto_tot_min_gub  DECIMAL(16,6),
          monto_tot_com_gub  DECIMAL(16,6),
          monto_tot_plu_pat  DECIMAL(16,6),
          monto_tot_par_viv  DECIMAL(18,6)
   END RECORD

   DEFINE neto  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE comi  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE comi2  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE tot  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_cuota_soc    DECIMAL(16,6),
          monto_aport_est    DECIMAL(16,6),
          monto_aport_esp    DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6)
   END RECORD

   DEFINE tot_1  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE tot_2  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6)
   END RECORD

   DEFINE acc  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE total  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE total_acc RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE vces_vej           DECIMAL(16,2),
          rval_porcentaje    DECIMAL(16,6),
          precio_acc         DECIMAL(16,6),
          xxfecha_liqui      DATE,
          precio_dia         DECIMAL(16,6),
          vtipo_registro     CHAR(2)

   DEFINE vfecha_aplica      DATE,
          vtasa_aplica_viv   DECIMAL(16,6)

   DEFINE vaport_pat         CHAR(01)

   DEFINE opc                CHAR(1)

   DEFINE monto_ret_acc      DECIMAL(16,6),
          impt_ret_acc       DECIMAL(16,6),
          comi_impt_ret_acc  DECIMAL(16,6),
          comi2_impt_ret_acc DECIMAL(16,6),
          tot_monto_ret_acc  DECIMAL(16,6)

   DEFINE vces_vej_acc           DECIMAL(16,6),
          impt_ces_vej_acc       DECIMAL(16,6),
          comi_impt_ces_vej_acc  DECIMAL(16,6),
          comi2_impt_ces_vej_acc DECIMAL(16,6),
          tot_monto_ces_vej_acc  DECIMAL(16,6)

   DEFINE dep_ident_pago        CHAR(2)

   DEFINE total_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE total_acc_acla RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE log1                  CHAR(40)

   DEFINE ejecuta_procedure     CHAR(500),
          ejecuta_marca         CHAR(200),
          ejecuta_desmarca      CHAR(200),
          ejecuta_fn_mayor      CHAR(200),
          ejecuta_fn_paga       CHAR(200),
          ejecuta_fn_menor      CHAR(200) -- sie_dos

END GLOBALS
#*********************************************************************
MAIN

   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   SELECT *,
          USER
   INTO   gparam_dev.*,
          gusuario
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   LET log1 = "/tmp/",gusuario CLIPPED,".EXCB002.log"

   CALL STARTLOG(log1)

   LET hoy = "02/21/2005"

   LET ejecuta_procedure = "EXECUTE PROCEDURE fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE clausula_sql3 FROM ejecuta_procedure

   LET ejecuta_marca = "EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
   PREPARE clausula_sql FROM ejecuta_marca

   LET ejecuta_desmarca = "EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "
   PREPARE clausula_sql2 FROM ejecuta_desmarca

   LET ejecuta_fn_mayor = "EXECUTE PROCEDURE fn_ap_exc_mayores_1 (?,?,?,?)" --- sie_dos
   PREPARE cla_fn_mayor FROM ejecuta_fn_mayor  -- sie_dos

   LET ejecuta_fn_menor = "EXECUTE PROCEDURE fn_ap_exc_menores_1 (?,?,?,?)" --- sie_dos
   PREPARE cla_fn_menor FROM ejecuta_fn_menor  -- sie_dos

   LET ejecuta_fn_paga = "EXECUTE PROCEDURE fn_paga_exc (?,?)" --- sie_dos
   PREPARE cla_fn_paga FROM ejecuta_fn_paga  -- sie_dos

   CALL Proceso()

END MAIN
#*********************************************************************
FUNCTION Inicializa()

   LET hoy = "02/21/2005"

   LET vnom_archivo = generar[1,8] CLIPPED,".EXCESO" CLIPPED

   LET vcont_acep              = 0
   LET vcont_rech              = 0

   LET det.monto_ret           = 0
   LET det.monto_act_ret       = 0
   LET det.monto_ces_vej_pat   = 0
   LET det.monto_ces_vej_tra   = 0
   LET det.monto_act_ces_vej   = 0
   LET det.monto_cuo_soc       = 0
   LET det.monto_aport_est     = 0
   LET det.monto_aport_esp     = 0
   LET det.monto_aport_pat     = 0

   LET dep.monto_soli_institu  = 0
   LET dep.monto_aceptado      = 0
   LET dep.monto_parcial       = 0
   LET dep.monto_pendiente     = 0
   LET dep.monto_rechazado     = 0

   LET d_sum.monto_tot_rcv     = 0
   LET d_sum.monto_tot_plu_rcv = 0
   LET d_sum.monto_tot_min_rcv = 0
   LET d_sum.monto_tot_com_rcv = 0
   LET d_sum.monto_tot_pat     = 0
   LET d_sum.monto_tot_gub     = 0
   LET d_sum.monto_tot_plu_gub = 0
   LET d_sum.monto_tot_min_gub = 0
   LET d_sum.monto_tot_com_gub  = 0
   LET d_sum.monto_tot_plu_pat  = 0

   LET neto.impt_ret           = 0
   LET neto.impt_ces_vej       = 0

   LET comi.impt_ret           = 0
   LET comi.impt_ces_vej       = 0

   LET comi2.impt_ret          = 0
   LET comi2.impt_ces_vej      = 0

   LET tot.monto_ret           = 0
   LET tot.monto_ces_vej       = 0
   LET tot.monto_cuota_soc     = 0
   LET tot.monto_aport_est     = 0
   LET tot.monto_aport_esp     = 0
   LET tot.monto_aport_pat     = 0

   LET tot_1.monto_ret         = 0
   LET tot_1.monto_ces_vej     = 0
   LET tot_1.monto_act_ret     = 0
   LET tot_1.monto_act_ces_vej = 0

   LET tot_2.monto_ret         = 0
   LET tot_2.monto_ces_vej     = 0
   LET tot_2.monto_act_ret     = 0
   LET tot_2.monto_act_ces_vej = 0
   LET tot_2.monto_aport_pat   = 0

   LET acc.monto_ret           = 0
   LET acc.monto_ces_vej       = 0
   LET acc.monto_act_ret       = 0
   LET acc.monto_act_ces_vej   = 0

   LET total.monto_ret         = 0
   LET total.monto_ces_vej     = 0

   LET total_acc.monto_ret     = 0
   LET total_acc.monto_ces_vej = 0

   LET rval_porcentaje         = 0
   LET precio_acc              = 0
   LET xxfecha_liqui           = NULL
   LET precio_dia              = 0
   LET vtasa_aplica_viv        = 0

   LET monto_ret_acc           = 0
   LET impt_ret_acc            = 0
   LET comi_impt_ret_acc       = 0
   LET comi2_impt_ret_acc      = 0
   LET tot_monto_ret_acc       = 0

   LET vces_vej_acc            = 0
   LET impt_ces_vej_acc        = 0
   LET comi_impt_ces_vej_acc   = 0
   LET comi2_impt_ces_vej_acc  = 0
   LET tot_monto_ces_vej_acc   = 0

   LET total_acla.monto_ret         = 0
   LET total_acla.monto_ces_vej     = 0

   LET total_acc_acla.monto_ret     = 0
   LET total_acc_acla.monto_ces_vej = 0

END FUNCTION
#*********************************************************************
FUNCTION Proceso()

   LET hoy = "02/21/2005"

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

   IF generar IS NOT NULL THEN
      LET vfolio = Proceso_principal()

      LET vhora_final = TIME
      LET vproc       = vcont_acep
      LET vrech       = vcont_rech
      LET vpend       = vcont_pend

      IF vfolio = 0 THEN
         LET vresultado = "ESTE ARCHIVO YA HA SIDO PROCESADO"
      ELSE 
         LET vresultado = "ACEPTADOS: " CLIPPED,vproc CLIPPED,
                          " RECHAZADOS: " CLIPPED,vrech CLIPPED,
                          " PENDIENTE: " CLIPPED,vpend CLIPPED
      END IF

      CALL Actualiza_etapa(vfolio,1,vresultado)
   END IF

   DISPLAY "PROCESO TERMINADO DE PAGOS EN EXCESO"

END FUNCTION
#*********************************************************************
FUNCTION Proceso_principal()

   DEFINE ejecuta    CHAR(300),
          vfolio     INTEGER

   CALL Inicializa()

   DISPLAY "PROCESANDO INFORMACION..."

   CALL Ingresa_etapa(vfolio,2,"Inicia separa archivo de pagos en exceso") 
      CALL Separa_archivo()                                  -- ETAPA 2
   CALL Actualiza_etapa(vfolio,2,"Termina separa archivo de pagos en exceso")

   CALL Ingresa_etapa(vfolio,3,"Inicia carga historicos de pagos en exceso")
      LET vfolio = Sube_datos()         -- ETAPA 3
         CALL Actualiza_etapa_1(vfolio,1)
         CALL Actualiza_etapa_1(vfolio,2)
      CALL actualiza_monto(vfolio)
   CALL Actualiza_etapa(vfolio,3,"Termina carga historicos de pagos en exceso")

   CALL Ingresa_etapa(vfolio,4,"Inicia validacion y provision de registros ")
      CALL validacion(vfolio)                                -- ETAPA 4
   CALL Actualiza_etapa(vfolio,4,"Termina validacion y provision de registros")

   RETURN vfolio

END FUNCTION
#*********************************************************************
FUNCTION Ingresa_etapa(vfolio,vetapa_cod,vresultado)

   DEFINE vfolio         INTEGER,
          vetapa_cod     DECIMAL(2,0),
          vresultado     CHAR(50)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES 
      ("02/21/2005",             -- fecha_proceso
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
#*********************************************************************
FUNCTION Separa_archivo()  -- ETAPA 2 

   DEFINE vfolio         INTEGER,
          ejecuta        CHAR(300)

   DISPLAY "SEPARANDO ARCHIVO,ETAPA 2"

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

   DISPLAY "SEPARACION DE ARCHIVO TERMINADO,ETAPA 2"
END FUNCTION
#*********************************************************************
FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vresultado)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          vresultado   CHAR(50),
          hoy          DATE

   LET hoy = "02/21/2005"

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
#*********************************************************************
FUNCTION Actualiza_etapa_1(vfolio,vetapa_cod)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          hoy          DATE

   LET hoy = "02/21/2005"

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
#*********************************************************************
FUNCTION Sube_datos()  -- ETAPA 3

   DEFINE ejecuta          CHAR(200),
          vlote            CHAR(03),
          vfolio           INTEGER,
          vfecha_recepcion DATE,
          vhora_recepcion  CHAR(10),
          vestado          SMALLINT,
          vfecha_estado    DATE,
          vhora_estado     CHAR(10),
          i                INTEGER,
          cfecha_envio     CHAR(10),
          vfecha_envio     CHAR(08),
          dvfecha_lote     CHAR(10),
          vfecha_aux       DATE,
          vcontenido       CHAR(1)

   DEFINE vid_pago         CHAR(16),
          posicion1        CHAR(1),
          posicion2        CHAR(1),
          display_id         CHAR(50)

   DEFINE vcomando         CHAR(100)

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

   SELECT "X"
   FROM   exc_cza_exceso
   WHERE  fecha_creac_lote = vfecha_aux
   AND    consecutivo_dia  = vlote

   IF SQLCA.SQLCODE <> 0 THEN
{
      INSERT INTO glo_folio
      VALUES(0)

      SELECT MAX(folio)
      INTO   vfolio
      FROM   glo_folio
} 
let vfolio = 19999
      LET vfecha_recepcion = "02/21/2005"
      LET vhora_recepcion  = TIME
      LET vestado          = 2
      LET vfecha_estado    = "02/21/2005"
      LET vhora_estado     = TIME

      --------------------   GENERA exc_cza_exceso   --------------------

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

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; paste vfolio_cza exc_cza > cza1" CLIPPED
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "/;sed 's/	//g' cza1 > exc_cza "
      RUN ejecuta

      LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                    "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_exceso_cza -l /tmp/",gusuario CLIPPED,".dbload_exc_cza.log -e 1 -k;"
      RUN ejecuta

      --------------------   GENERA adi   --------------------

      LET vreporte = gparam_dev.ruta_rescate CLIPPED,"/adi"

      START REPORT salida2 TO vreporte
         LET vcontenido = "a"

         OUTPUT TO REPORT salida2(vcontenido)
      FINISH REPORT salida2

      --------------------   GENERA exc_det_exceso   --------------------

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

      -- Se crea archivo vfolio_det2 con el numero de registros igual al
      -- num. registros que tiene el detalle

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; sed -f vfolio_det exc_det > vfolio_det2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det borrando lineas que no sirven

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_det2 > vfolio_det" CLIPPED 
      RUN ejecuta

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

      LET ejecuta = "cd ",gparam_dev.ruta_exp CLIPPED,
                    "/;DBDATE=y4md;export DBDATE;dbload -d safre_af -c sube_exceso_det -l /tmp/",gusuario CLIPPED,".dbload_exc_det.log -e 1 -k;"
      RUN ejecuta

      --------------------   GENERA exc_dep_exceso   --------------------

      -- Se crea archivo adi que contiene a\  y se pega en vfolio_dep

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; cat adi vfolio_cza1 > vfolio_dep" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_dep2 con el numero de registros igual al
      -- num. registros que tiene el detalle

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; sed -f vfolio_dep exc_dep > vfolio_dep2" CLIPPED
      RUN ejecuta

      -- Se crea archivo vfolio_det borrando lineas que no sirven

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; sed -e '/^ /!d' vfolio_dep2 > vfolio_dep" CLIPPED 
      RUN ejecuta

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

      --------------------   GENERA exc_sum_exceso   --------------------

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
      DISPLAY "ESTE ARCHIVO YA HA SIDO PROCESADO"
      RETURN 0
   END IF

   DISPLAY "SUBE DATOS A TABLAS HISTORICAS TERMINADO,ETAPA 3"

END FUNCTION
#*********************************************************************
FUNCTION actualiza_monto(vfolio)

   DEFINE vfolio     INTEGER

   DISPLAY "ACTUALIZA MONTOS"

   DECLARE cursor_2 CURSOR FOR
   SELECT NVL(monto_ret,0),
          NVL(monto_act_ret,0),
          NVL(monto_ces_vej_pat,0),
          NVL(monto_ces_vej_tra,0),
          NVL(monto_act_ces_vej,0),
          NVL(monto_cuo_soc,0),
          NVL(monto_aport_est,0),
          NVL(monto_aport_esp,0),
          NVL(monto_aport_pat,0),
          NVL(monto_par_viv,0)
   FROM   exc_det_exceso
   WHERE  folio = vfolio
   FOR UPDATE

   FOREACH cursor_2 INTO g_reg.monto_ret,
                         g_reg.monto_act_ret,
                         g_reg.monto_ces_vej_pat,
                         g_reg.monto_ces_vej_tra,
                         g_reg.monto_act_ces_vej,
                         g_reg.monto_cuo_soc,
                         g_reg.monto_aport_est,
                         g_reg.monto_aport_esp,
                         g_reg.monto_aport_pat,
                         g_reg.monto_par_viv

      UPDATE exc_det_exceso
      SET    monto_ret         = g_reg.monto_ret/100,
             monto_act_ret     = g_reg.monto_act_ret/100,
             monto_ces_vej_pat = g_reg.monto_ces_vej_pat/100,
             monto_ces_vej_tra = g_reg.monto_ces_vej_tra/100,
             monto_act_ces_vej = g_reg.monto_act_ces_vej/100,
             monto_cuo_soc     = g_reg.monto_cuo_soc/100,
             monto_aport_est   = g_reg.monto_aport_est/100,
             monto_aport_esp   = g_reg.monto_aport_esp/100,
             monto_aport_pat   = g_reg.monto_aport_pat/100,
             monto_par_viv     = g_reg.monto_par_viv/1000000
      WHERE  CURRENT OF cursor_2

   END FOREACH

   DECLARE cursor_3 CURSOR FOR
   SELECT ident_pago[14,15],
          NVL(monto_soli_institu,0),
          NVL(monto_aceptado,0),
          NVL(monto_parcial,0),
          NVL(monto_pendiente,0),
          NVL(monto_rechazado,0),
          NVL(monto_par_solicitado,0),
          NVL(monto_par_aceptado,0),
          NVL(monto_par_parcial,0),
          NVL(monto_par_pendiente,0),
          NVL(monto_par_rechazado,0),
          codigo_siefore
   FROM   exc_dep_exceso
   WHERE  folio = vfolio
   FOR UPDATE 

   FOREACH cursor_3 INTO g_dep.ident_pago,
                         g_dep.monto_soli_institu,
                         g_dep.monto_aceptado,
                         g_dep.monto_parcial,
                         g_dep.monto_pendiente,
                         g_dep.monto_rechazado,
                         g_dep.monto_par_solicitado,
                         g_dep.monto_par_aceptado,
                         g_dep.monto_par_parcial,
                         g_dep.monto_par_pendiente,
                         g_dep.monto_par_rechazado,
                         g_dep.codigo_siefore

      IF g_dep.ident_pago = "71" THEN
         LET g_dep.codigo_siefore = 2
      ELSE
         LET g_dep.codigo_siefore = 0
      END IF

      UPDATE exc_dep_exceso
      SET    monto_soli_institu   = g_dep.monto_soli_institu/100,
             monto_aceptado       = g_dep.monto_aceptado/100,
             monto_parcial        = g_dep.monto_parcial/100,
             monto_pendiente      = g_dep.monto_pendiente/100,
             monto_rechazado      = g_dep.monto_rechazado/100,
             monto_par_solicitado = g_dep.monto_par_solicitado/1000000,
             monto_par_aceptado   = g_dep.monto_par_aceptado/1000000,
             monto_par_parcial    = g_dep.monto_par_parcial/1000000,
             monto_par_pendiente  = g_dep.monto_par_pendiente/1000000,
             monto_par_rechazado  = g_dep.monto_par_rechazado/1000000,
             codigo_siefore       = g_dep.codigo_siefore
      WHERE  CURRENT OF cursor_3

   END FOREACH

   DECLARE cursor_4 CURSOR FOR
   SELECT NVL(monto_tot_rcv,0), 
          NVL(monto_tot_plu_rcv,0),
          NVL(monto_tot_min_rcv,0),
          NVL(monto_tot_com_rcv,0),
          NVL(monto_tot_pat,0),
          NVL(monto_tot_gub,0),
          NVL(monto_tot_plu_gub,0),
          NVL(monto_tot_min_gub,0),
          NVL(monto_tot_com_gub,0),
          NVL(monto_tot_plu_pat,0),
          NVL(monto_tot_par_viv,0)
   FROM   exc_sum_exceso
   WHERE  folio = vfolio
   FOR UPDATE

   FOREACH cursor_4 INTO g_sum.monto_tot_rcv,
                         g_sum.monto_tot_plu_rcv,
                         g_sum.monto_tot_min_rcv,
                         g_sum.monto_tot_com_rcv,
                         g_sum.monto_tot_pat,
                         g_sum.monto_tot_gub,
                         g_sum.monto_tot_plu_gub,
                         g_sum.monto_tot_min_gub,
                         g_sum.monto_tot_com_gub,
                         g_sum.monto_tot_plu_pat,
                         g_sum.monto_tot_par_viv

      UPDATE exc_sum_exceso
      SET    monto_tot_rcv     = g_sum.monto_tot_rcv/100,
             monto_tot_plu_rcv = g_sum.monto_tot_plu_rcv/100,
             monto_tot_min_rcv = g_sum.monto_tot_min_rcv/100,
             monto_tot_com_rcv = g_sum.monto_tot_com_rcv/100,
             monto_tot_pat     = g_sum.monto_tot_pat/100,
             monto_tot_gub     = g_sum.monto_tot_gub/100,
             monto_tot_plu_gub = g_sum.monto_tot_plu_gub/100,
             monto_tot_min_gub = g_sum.monto_tot_min_gub/100,
             monto_tot_com_gub = g_sum.monto_tot_com_gub/100,
             monto_tot_plu_pat = g_sum.monto_tot_plu_pat/100,
             monto_tot_par_viv = g_sum.monto_tot_par_viv/1000000
      WHERE  CURRENT OF cursor_4

   END FOREACH

   DISPLAY "ACTUALIZACION TERMINADA"

END FUNCTION
#*********************************************************************
FUNCTION validacion(vfolio)      ---ETAPA 4

   DEFINE vfolio                INTEGER,
          vestado_proceso       SMALLINT,
          tipo_rech             CHAR(03),
          auxfecha_pago         CHAR(10),
          auxfecha_pago1        CHAR(8),
          vfentcons             DATE,
          vfinitmte             DATE,
          vfecha_pri_general    DATE,
          mig_marca_entra       SMALLINT,
          mig_codigo_rechazo    SMALLINT

   DEFINE v  RECORD
          folio                 INTEGER,
          consec_reg_lote       INTEGER,
          fecha_recepcion       DATE,
          impt_ret              DECIMAL(16,6),
          impt_ces_vej          DECIMAL(16,6),
          impt_act_rec_ret      DECIMAL(16,6),
          impt_act_r_ces_vej    DECIMAL(16,6),
          impt_cuota_soc        DECIMAL(16,6),
          impt_aport_est        DECIMAL(16,6),
          impt_aport_esp        DECIMAL(16,6),
          impt_aport_pat        DECIMAL(16,6)
   END RECORD

   DEFINE vtipo_solicitud       SMALLINT

   DEFINE vces_vej              DECIMAL(16,6)

   DEFINE aux_monto_ces_vej_pat DECIMAL(16,6),
          aux_monto_ces_vej_tra DECIMAL(16,6),
          aux_monto_aport_pat   DECIMAL(16,6)

   DEFINE opc                   CHAR(1)
   DEFINE dep_ident_pago        CHAR(2)

   DEFINE mar_activo_marca      SMALLINT,
          mar_fecha_act_marca   DATE,
          mar_marca_cod         SMALLINT

   DEFINE x_afore_local         SMALLINT,
          uni_sw                SMALLINT

   DEFINE x_ident_viv_garantia  CHAR(1)
   DEFINE x_credito_garantia    CHAR(1)
   DEFINE vvfentcons            DATE

   DEFINE x_saldo_acc        DECIMAL(18,6),
          x_saldo_pes        DECIMAL(18,6)

   DISPLAY "VALIDACION Y PROVISION,ETAPA 4"

   SELECT codigo_afore
   INTO   x_afore_local
   FROM   tab_afore_local
   GROUP BY 1

   DECLARE cursor_exc CURSOR FOR
   SELECT *
   FROM   exc_det_exceso
   WHERE  folio = vfolio

   FOREACH cursor_exc INTO g_reg.*

      CALL genera_tmp_cuenta (g_reg.nss)

      CALL saldo_al_dia(g_reg.nss,
                        0,
                        0)
           RETURNING x_saldo_acc,
                     x_saldo_pes

      LET x_saldo_acc = 0
      LET x_saldo_pes = 0

      IF g_reg.clave_ent_orig = "001" THEN
         LET mig_marca_entra = 540
      ELSE
         LET mig_marca_entra = 542
      END IF

      LET auxfecha_pago = g_reg.fecha_pago

      LET auxfecha_pago1 = auxfecha_pago[7,10],
                           auxfecha_pago[1,2],
                           auxfecha_pago[4,5]

      SELECT tipo_solicitud,
             fentcons,
             finitmte
      INTO   vtipo_solicitud,
             vfentcons,
             vfinitmte
      FROM   afi_mae_afiliado
      WHERE  n_seguro = g_reg.nss

      IF SQLCA.SQLCODE <> 0 THEN
         CALL actualiza_reg(g_reg.folio,
                            g_reg.nss,
                            "02",
                            "588",
                            g_reg.consec_reg_lote,
                            g_reg.folio_pago_sua,
                            mig_marca_entra,
                            gusuario)
      ELSE

###### diferencia de version, solo XXI ######
         SELECT fentcons
         INTO   vvfentcons
         FROM   afi_his_afiliado
         WHERE  n_seguro = g_reg.nss
         AND    tipo_solicitud = 5

         IF SQLCA.SQLCODE = 0 THEN
            LET vfentcons = vvfentcons
         END IF

#############################################

         LET tipo_rech = valida_traspaso (g_reg.nss,
                                          g_reg.periodo_pago,
                                          auxfecha_pago1,
                                          g_reg.folio_pago_sua,
                                          g_reg.reg_patronal_imss,
                                          g_reg.clave_ent_recep,
                                          vtipo_solicitud,
                                          g_reg.clave_ent_orig) --- traspasos

         IF tipo_rech <> "100" THEN
            CALL actualiza_reg (g_reg.folio,
                                g_reg.nss,
                                "02",
                                tipo_rech,
                                g_reg.consec_reg_lote,
                                g_reg.folio_pago_sua,
                                mig_marca_entra,
                                gusuario)
         ELSE
            LET tipo_rech = valida_retiro (g_reg.clave_ent_orig,
                                           g_reg.nss) --- retiros

            IF tipo_rech <> "100" THEN
               CALL actualiza_reg (g_reg.folio,
                                   g_reg.nss,
                                   "02",
                                   tipo_rech,
                                   g_reg.consec_reg_lote,
                                   g_reg.folio_pago_sua,
                                   mig_marca_entra,
                                   gusuario)
            ELSE
               IF g_reg.monto_ces_vej_tra > 0 AND
                  g_reg.monto_ces_vej_pat > 0 THEN

                  CALL actualiza_reg(g_reg.folio,
                                     g_reg.nss,
                                     "02",
                                     "327",
                                     g_reg.consec_reg_lote,
                                     g_reg.folio_pago_sua,
                                     mig_marca_entra,
                                     gusuario)
               ELSE
                  IF g_reg.monto_ces_vej_pat = 0 AND
                     g_reg.monto_ces_vej_tra > 0 THEN

                     LET vces_vej = g_reg.monto_ces_vej_tra
                     LET vaport_pat ="N"
                  END IF

                  IF g_reg.monto_ces_vej_tra = 0 AND
                     g_reg.monto_ces_vej_pat > 0 THEN

                     LET vces_vej = g_reg.monto_ces_vej_pat
                     LET vaport_pat = "S"
                  END IF

                  IF g_reg.monto_ces_vej_tra = 0 AND
                     g_reg.monto_ces_vej_pat = 0 THEN

                     LET vces_vej = 0
                  END IF

                  IF vaport_pat = "S" THEN
{
                     SELECT a.monto_ces_vej_pat,
                            a.monto_aport_pat
                     INTO   aux_monto_ces_vej_pat,
                            aux_monto_aport_pat
                     FROM   exc_det_exceso a
                     WHERE  a.folio            <> g_reg.folio
                     AND    a.nss               = g_reg.nss
                     AND    a.periodo_pago      = g_reg.periodo_pago
                     AND    a.fecha_pago        = g_reg.fecha_pago
                     AND    a.folio_pago_sua    = g_reg.folio_pago_sua
                     AND    a.reg_patronal_imss = g_reg.reg_patronal_imss
                     AND    a.clave_ent_recep   = g_reg.clave_ent_recep
                     AND    a.result_operacion  = "01"
		     GROUP BY 1,2
}
                     IF SQLCA.SQLCODE = 0 THEN
                        IF aux_monto_ces_vej_pat > 0 AND
                           g_reg.monto_ces_vej_pat > 0 THEN
                           CALL actualiza_reg(g_reg.folio,
                                              g_reg.nss,
                                              "02",
                                              "612",
                                              g_reg.consec_reg_lote,
                                              g_reg.folio_pago_sua,
                                              mig_marca_entra,
                                              gusuario)

                           LET aux_monto_ces_vej_pat = 0

                           CONTINUE FOREACH
                        ELSE
                           IF aux_monto_aport_pat > 0 AND
                              g_reg.monto_aport_pat > 0 THEN
                              CALL actualiza_reg(g_reg.folio,
                                                 g_reg.nss,
                                                 "02",
                                                 "612",
                                                 g_reg.consec_reg_lote,
                                                 g_reg.folio_pago_sua,
                                                 mig_marca_entra,
                                                 gusuario)

                              LET aux_monto_aport_pat = 0

                              CONTINUE FOREACH
                           END IF
                        END IF
                     END IF
                  END IF

                  IF vaport_pat = "N" THEN
{
                     SELECT a.monto_ces_vej_tra
                     INTO   aux_monto_ces_vej_tra
                     FROM   exc_det_exceso a
                     WHERE  a.folio            <> g_reg.folio
                     AND    a.nss               = g_reg.nss
                     AND    a.periodo_pago      = g_reg.periodo_pago
                     AND    a.fecha_pago        = g_reg.fecha_pago
                     AND    a.folio_pago_sua    = g_reg.folio_pago_sua
                     AND    a.reg_patronal_imss = g_reg.reg_patronal_imss
                     AND    a.clave_ent_recep   = g_reg.clave_ent_recep
                     AND    a.result_operacion  = "01"
}
                     IF SQLCA.SQLCODE = 0 THEN
                        IF aux_monto_ces_vej_tra > 0 AND
                           g_reg.monto_ces_vej_tra > 0 THEN
                           CALL actualiza_reg(g_reg.folio,
                                              g_reg.nss,
                                              "02",
                                              "612",
                                              g_reg.consec_reg_lote,
                                              g_reg.folio_pago_sua,
                                              mig_marca_entra,
                                              gusuario)

                           LET aux_monto_ces_vej_tra = 0

                           CONTINUE FOREACH
                        END IF
                     END IF
                  END IF

                  SELECT folio,
                         consec_reg_lote,
                         fecha_recepcion,
                         impt_ret/100,
                         impt_ces_vej/100,
                         impt_act_rec_ret/100,
                         impt_act_r_ces_vej/100,
                         impt_cuota_soc/100,
                         impt_aport_est/100,
                         impt_aport_esp/100,
                         impt_aport_pat/100,
                         ident_viv_garantia
                  INTO   v.folio,
                         v.consec_reg_lote,
                         v.fecha_recepcion,
                         v.impt_ret,
                         v.impt_ces_vej,
                         v.impt_act_rec_ret,
                         v.impt_act_r_ces_vej,
                         v.impt_cuota_soc,
                         v.impt_aport_est,
                         v.impt_aport_esp,
                         v.impt_aport_pat,
                         x_ident_viv_garantia
                  FROM   dis_det_aporte a
                  WHERE  a.n_seguro          = g_reg.nss
                  AND    a.periodo_pago      = g_reg.periodo_pago
                  AND    a.fech_pago         = auxfecha_pago1
                  AND    a.folio_pago_sua    = g_reg.folio_pago_sua
                  AND    a.reg_patronal_imss = g_reg.reg_patronal_imss
                  AND    a.cve_ent_receptora = g_reg.clave_ent_recep
                  AND    a.result_operacion[2] = "1"
                  GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12

                  IF SQLCA.SQLCODE <> 0 THEN
                     CALL actualiza_reg(g_reg.folio,
                                        g_reg.nss,
                                        "02",
                                        "327",
                                        g_reg.consec_reg_lote,
                                        g_reg.folio_pago_sua,
                                        mig_marca_entra,
                                        gusuario)
                  ELSE
                     IF g_reg.clave_ent_orig = "002" THEN 
                        CALL saldo_al_dia(g_reg.nss,
                                          4,
                                          0)
                             RETURNING x_saldo_acc,
                                       x_saldo_pes

                        IF g_reg.monto_par_viv <= x_saldo_acc THEN
                           LET tipo_rech = "100"
                        ELSE
                           CALL actualiza_reg(g_reg.folio,
                                              g_reg.nss,
                                              "02",
                                              "593",
                                              g_reg.consec_reg_lote,
                                              g_reg.folio_pago_sua,
                                              mig_marca_entra,
                                              gusuario)
                           CONTINUE FOREACH 
                        END IF
                     ELSE
                        LET tipo_rech = valida_monto (v.*,
                                                      g_reg.*,
                                                      vces_vej) --- montos mayor
                     END IF

                     IF tipo_rech = "700" THEN
                        CALL actualiza_reg (g_reg.folio,
                                            g_reg.nss,
                                            "02",
                                            tipo_rech,
                                            g_reg.consec_reg_lote,
                                            g_reg.folio_pago_sua,
                                            mig_marca_entra,
                                            gusuario)
                     ELSE
                        IF x_ident_viv_garantia = "1" THEN   ---- CREDITO EN GARANTIA 
                           CALL actualiza_reg (g_reg.folio,
                                               g_reg.nss,
                                               "02",
                                               "964",
                                               g_reg.consec_reg_lote,
                                               g_reg.folio_pago_sua,
                                               mig_marca_entra,
                                               gusuario)

                           CONTINUE FOREACH
                        END IF

                        LET uni_sw = 0

                        SELECT "X"
                        FROM   tmp_dis_cuenta
                        WHERE  tipo_movimiento BETWEEN 241 AND 244
                        GROUP BY 1

                        IF SQLCA.SQLCODE = 0 THEN
                           LET uni_sw = 1
                        END IF

                        IF uni_sw = 1 THEN
                           CALL actualiza_reg(g_reg.folio,
                                              g_reg.nss,
                                              "02",
                                              "034",
                                              g_reg.consec_reg_lote,
                                              g_reg.folio_pago_sua,
                                              mig_marca_entra,
                                              gusuario)
                        ELSE
                           SELECT fecha_pri_general
                           INTO   vfecha_pri_general
                           FROM   cta_ctr_cuenta
                           WHERE  nss = g_reg.nss

                           CALL marca(g_reg.nss,              -- nss
                                      mig_marca_entra,        -- marca_entra
                                      g_reg.consec_reg_lote,  -- correlativo
                                      0,                      -- marca_estado
                                      0,                      -- codigo_rechazo
                                      0,                      -- marca_causa
                                      " ",                    -- fecha_causa
                                      gusuario                -- usuario
                                     )

                                RETURNING vestado_proceso,
                                          mig_codigo_rechazo

                           IF mig_codigo_rechazo = 0 THEN
                              CALL actualiza_control(g_reg.*,
                                                     vces_vej,
                                                     v.*,
                                                     vfentcons,
                                                     vfecha_pri_general)
                           ELSE
                              IF mig_codigo_rechazo = 528 OR
                                 mig_codigo_rechazo = 529 OR
                                 mig_codigo_rechazo = 530 OR
                                 mig_codigo_rechazo = 575 OR
                                 mig_codigo_rechazo = 586 OR
                                 mig_codigo_rechazo = 587 THEN

                                 CALL actualiza_reg(g_reg.folio,
                                                    g_reg.nss,
                                                    "03",
                                                    mig_codigo_rechazo,
                                                    g_reg.consec_reg_lote,
                                                    g_reg.folio_pago_sua,
                                                    mig_marca_entra,
                                                    gusuario)
                              ELSE
                                 CALL actualiza_reg(g_reg.folio,
                                                    g_reg.nss,
                                                    "02",
                                                    mig_codigo_rechazo,
                                                    g_reg.consec_reg_lote,
                                                    g_reg.folio_pago_sua,
                                                    mig_marca_entra,
                                                    gusuario)
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
     END IF
   END FOREACH

   DISPLAY "VALIDACION Y PROISION TERMINADA,ETAPA 4"

END FUNCTION
#*********************************************************************
FUNCTION actualiza_reg(rfolio,
                       vnss,
                       res_op,
                       tipo_rech,
                       x_consecutivo,
                       x_folio_sua,
                       x_mig_marca_entra,
                       x_gusuario)

   DEFINE rfolio               INTEGER,
          vnss                 CHAR(11),
          res_op               CHAR(02),
          tipo_rech            CHAR(03),
          x_consecutivo        INTEGER,
          x_folio_sua          CHAR(6),
          x_mig_marca_entra    SMALLINT,
          x_gusuario           CHAR(8),
          x_vestado_proceso    SMALLINT,
          x_mig_codigo_rechazo SMALLINT

   IF tipo_rech = 588 OR
      tipo_rech = 528 OR
      tipo_rech = 529 OR
      tipo_rech = 530 OR
      tipo_rech = 575 OR
      tipo_rech = 586 OR
      tipo_rech = 587 THEN
   ELSE
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
   END IF

   IF res_op = "02" THEN
      CALL contador_rechazo()
   ELSE
      CALL contador_pendiente()
   END IF

   UPDATE exc_det_exceso
   SET    result_operacion = res_op,
          tipo_diagnostico = tipo_rech
   WHERE nss = vnss
   AND   folio = rfolio
   AND   consec_reg_lote = x_consecutivo
   AND   folio_pago_sua =  x_folio_sua

END FUNCTION
#*********************************************************************
FUNCTION actualiza_control(reg_1,vces_vej,r,vfentcons,vfecha_pri_general)

   DEFINE reg_1  RECORD LIKE exc_det_exceso.* 

   DEFINE vfentcons          DATE,
          vfinitmte          DATE,
          vfecha_pri_general DATE

   DEFINE r  RECORD
          folio              INTEGER,
          consec_reg_lote    INTEGER,
          fecha_recepcion    DATE,
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6),
          impt_act_rec_ret   DECIMAL(16,6),
          impt_act_r_ces_vej DECIMAL(16,6),
          impt_cuota_soc     DECIMAL(16,6),
          impt_aport_est     DECIMAL(16,6),
          impt_aport_esp     DECIMAL(16,6),
          impt_aport_pat     DECIMAL(16,6)
   END RECORD 

   DEFINE neto  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE comi  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE comi2  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE tot  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_cuota_soc    DECIMAL(16,6),
          monto_aport_est    DECIMAL(16,6),
          monto_aport_esp    DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6),
          monto_par_viv      DECIMAL(18,6)
   END RECORD

   DEFINE tot_1  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE tot_2  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6),
          monto_par_viv      DECIMAL(18,6)
   END RECORD

   DEFINE acc  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE total  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE total_acc  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE vces_vej           DECIMAL(16,6),
          rval_porcentaje    DECIMAL(16,6),
          precio_acc         DECIMAL(16,6),
          xxfecha_liqui      DATE,
          precio_dia         DECIMAL(16,6),
          vtipo_registro     CHAR(2),
          hoy                DATE,
          opc                CHAR(1)

   DEFINE vfecha_aplica      DATE,
          vtasa_aplica_viv   DECIMAL(16,6)

   DEFINE tot_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE tot_1_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE tot_2_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6)
   END RECORD

   DEFINE acc_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE porce  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6)
   END RECORD

   DEFINE aclafecha_pago     CHAR(10),
          aclafecha_pago1    CHAR(8)

   DEFINE acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE monto_ret_acc      DECIMAL(16,6),
          impt_ret_acc       DECIMAL(16,6),
          comi_impt_ret_acc  DECIMAL(16,6),
          comi2_impt_ret_acc DECIMAL(16,6),
          tot_monto_ret_acc  DECIMAL(16,6)

   DEFINE vces_vej_acc           DECIMAL(16,6),
          impt_ces_vej_acc       DECIMAL(16,6),
          comi_impt_ces_vej_acc  DECIMAL(16,6),
          comi2_impt_ces_vej_acc DECIMAL(16,6),
          tot_monto_ces_vej_acc  DECIMAL(16,6)

   DEFINE total_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE total_acc_acla RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE x_saldo_acc        DECIMAL(18,6),
          x_saldo_pes        DECIMAL(18,6)

   DEFINE x_ind_transferencia SMALLINT,
          v_subcuenta         SMALLINT,
          v_siefore           SMALLINT,
          v_tipo_movimiento   SMALLINT,
          v_fecha_conversion  DATE,
          v_precio_accion     DECIMAL(16,6),
          v_monto_acc         DECIMAL(16,6),
          v_monto_pesos       DECIMAL(16,6),
          v_acciones          DECIMAL(16,6),
          v_pesos             DECIMAL(16,6),
          x_movimiento        SMALLINT,
          r_subcuenta         SMALLINT,
          r_siefore           SMALLINT,
          r_acciones          DECIMAL(16,6),
          r_pesos             DECIMAL(16,6)

   DEFINE ban_ret             SMALLINT,
	  ban_act_ret         SMALLINT,
  	  ban_ces_vej         SMALLINT,
	  ban_act_ces_vej     SMALLINT

   ---Montos de retiro a devolver-----------------

   LET aclafecha_pago = reg_1.fecha_pago

   LET aclafecha_pago1 = aclafecha_pago[7,10],
                         aclafecha_pago[1,2],
                         aclafecha_pago[4,5]

---- SIE_DOS INI
   LET hoy = "02/21/2005"

IF reg_1.clave_ent_orig = "001" THEN
   SELECT ind_transferencia
   INTO   x_ind_transferencia
   FROM   cta_ctr_cuenta
   WHERE  nss = reg_1.nss

   IF x_ind_transferencia = 1 THEN
      LET ban_ret         = 0
      LET ban_act_ret     = 0
      LET ban_ces_vej = 0
      LET ban_act_ces_vej = 0

      DECLARE cur_fn_mayor CURSOR FOR  cla_fn_mayor 
      FOREACH cur_fn_mayor USING reg_1.nss,
                                 r.folio,
                                 r.consec_reg_lote,
                                 hoy
                            INTO v_subcuenta,
                                 v_siefore,
                                 v_tipo_movimiento,
                                 v_fecha_conversion,
                                 v_precio_accion,
                                 v_monto_acc,
                                 v_monto_pesos,
                                 v_acciones,
                                 v_pesos

         IF v_subcuenta = 1 THEN
            CASE 
              WHEN v_tipo_movimiento = 1
                 IF reg_1.monto_ret > 0 THEN
		    LET porce.monto_ret = reg_1.monto_ret * 100/r.impt_ret
                    LET tot_2.monto_ret = (v_pesos - v_monto_pesos) * porce.monto_ret/100
                    LET acc.monto_ret   = v_acciones * porce.monto_ret/100
		    LET v_pesos = v_pesos  * porce.monto_ret/100
		    LET v_acciones = v_acciones  * porce.monto_ret/100
                    LET ban_ret = 1
                 END IF
              WHEN v_tipo_movimiento = 2
                 IF reg_1.monto_act_ret > 0 THEN
		    LET porce.monto_act_ret = reg_1.monto_act_ret * 100/r.impt_act_rec_ret
                    LET tot_2.monto_act_ret = (v_pesos - v_monto_pesos) * porce.monto_act_ret/100
                    LET acc.monto_act_ret   = v_acciones * porce.monto_act_ret/100
		    LET v_pesos = v_pesos  * porce.monto_act_ret/100
		    LET v_acciones = v_acciones  * porce.monto_act_ret/100
                    LET ban_act_ret = 1
                 END IF

              WHEN v_tipo_movimiento = 3
                 IF ban_ret = 1 THEN
                    LET tot_1_acla.monto_ret = v_pesos * porce.monto_ret/100
                    LET acc_acla.monto_ret   = v_acciones  * porce.monto_ret/100
		    LET v_pesos = v_pesos  * porce.monto_ret/100
		    LET v_acciones = v_acciones  * porce.monto_ret/100
                 END IF
              WHEN v_tipo_movimiento = 4
                 IF ban_act_ret = 1 THEN
                    LET tot_1_acla.monto_act_ret = v_pesos  * porce.monto_act_ret/100
                    LET acc_acla.monto_act_ret   = v_acciones * porce.monto_act_ret/100
		    LET v_pesos = v_pesos  * porce.monto_act_ret/100
		    LET v_acciones = v_acciones  * porce.monto_act_ret/100
                 END IF
              WHEN v_tipo_movimiento >= 100
                 IF ban_ret = 1 THEN
                    LET comi2.impt_ret = (v_monto_pesos * -1) * porce.monto_ret/100
                 END IF
            END CASE
         ELSE
            CASE
               WHEN v_tipo_movimiento = 1
                 IF reg_1.monto_ces_vej_pat > 0 THEN
		    LET porce.monto_ces_vej = reg_1.monto_ces_vej_pat * 100/r.impt_ces_vej
                    LET tot_2.monto_ces_vej = (v_pesos - v_monto_pesos) * porce.monto_ces_vej/100
                    LET acc.monto_ces_vej   = v_acciones * porce.monto_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_ces_vej/100
                    LET ban_ces_vej = 1
                 END IF
              WHEN v_tipo_movimiento = 2
                 IF reg_1.monto_act_ces_vej > 0 THEN
		    LET porce.monto_act_ces_vej = reg_1.monto_act_ces_vej * 100/r.impt_act_r_ces_vej
                    LET tot_2.monto_act_ces_vej = (v_pesos - v_monto_pesos) * porce.monto_act_ces_vej/100
                    LET acc.monto_act_ces_vej   = v_acciones * porce.monto_act_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_act_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_act_ces_vej/100
                    LET ban_act_ces_vej = 1
                 END IF

              WHEN v_tipo_movimiento = 3 
                 IF ban_ces_vej = 1 THEN
                    LET tot_1_acla.monto_ces_vej = v_pesos * porce.monto_ces_vej/100
                    LET acc_acla.monto_ces_vej   = v_acciones * porce.monto_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_ces_vej/100
                 END IF
              WHEN v_tipo_movimiento = 4
                 IF ban_act_ces_vej = 1 THEN
                    LET tot_1_acla.monto_act_ces_vej = v_pesos * porce.monto_act_ces_vej/100
                    LET acc_acla.monto_act_ces_vej   = v_acciones * porce.monto_act_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_act_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_act_ces_vej/100
                 END IF
              WHEN v_tipo_movimiento >= 100
                 IF ban_ces_vej = 1 THEN
                    LET comi2.impt_ces_vej = (v_monto_pesos * -1) * porce.monto_ces_vej/100
                 END IF
            END CASE
         END IF

         IF v_tipo_movimiento < 100 THEN
            CASE v_tipo_movimiento
               WHEN 1 LET x_movimiento = 540
               WHEN 2 LET x_movimiento = 545
               WHEN 3 LET x_movimiento = 550
               WHEN 4 LET x_movimiento = 555
            END CASE

            DECLARE cur_fn_paga CURSOR FOR  cla_fn_paga 
            FOREACH cur_fn_paga USING v_subcuenta,
                                      v_acciones
                                 INTO r_subcuenta,
                                      r_siefore,
                                      r_acciones,
                                      r_pesos

               LET precio_dia =  precio_acc_dia (r_siefore,hoy)

               CALL provisiona_cuenta(r_pesos,
                                      r_acciones,
                                      reg_1.*,
                                      r_subcuenta,
                                      x_movimiento,
                                      precio_dia,
                                      hoy,
                                      r_siefore)
            END FOREACH
         END IF
      END FOREACH
   ELSE
      LET ban_ret         = 0
      LET ban_act_ret     = 0
      LET ban_ces_vej = 0
      LET ban_act_ces_vej = 0

      DECLARE cur_fn_menor CURSOR FOR  cla_fn_menor 
      FOREACH cur_fn_menor USING reg_1.nss,
                                 r.folio,
                                 r.consec_reg_lote,
                                 hoy
                            INTO v_subcuenta,
                                 v_siefore,
                                 v_tipo_movimiento,
                                 v_fecha_conversion,
                                 v_precio_accion,
                                 v_monto_acc,
                                 v_monto_pesos,
                                 v_acciones,
                                 v_pesos

{
display reg_1.nss
display v_subcuenta,
        v_siefore,
        v_tipo_movimiento,
        v_fecha_conversion,
        v_monto_acc,
        v_monto_pesos,
        v_acciones,
        v_pesos
}
         IF v_subcuenta = 1 THEN
            CASE 
              WHEN v_tipo_movimiento = 1
                 IF reg_1.monto_ret > 0 THEN
		    LET porce.monto_ret = reg_1.monto_ret * 100/r.impt_ret
                    LET tot_2.monto_ret = (v_pesos - v_monto_pesos) * porce.monto_ret/100
                    LET acc.monto_ret   = v_acciones * porce.monto_ret/100
		    LET v_pesos = v_pesos  * porce.monto_ret/100
		    LET v_acciones = v_acciones  * porce.monto_ret/100
                    LET ban_ret = 1
                 END IF
              WHEN v_tipo_movimiento = 2
                 IF reg_1.monto_act_ret > 0 THEN
		    LET porce.monto_act_ret = reg_1.monto_act_ret * 100/r.impt_act_rec_ret
                    LET tot_2.monto_act_ret = (v_pesos - v_monto_pesos) * porce.monto_act_ret/100
                    LET acc.monto_act_ret   = v_acciones * porce.monto_act_ret/100
		    LET v_pesos = v_pesos  * porce.monto_act_ret/100
		    LET v_acciones = v_acciones  * porce.monto_act_ret/100
                    LET ban_act_ret = 1
                 END IF
              WHEN v_tipo_movimiento = 3
                 IF ban_ret = 1 THEN
                    LET tot_1_acla.monto_ret = v_pesos * porce.monto_ret/100
                    LET acc_acla.monto_ret   = v_acciones * porce.monto_ret/100
		    LET v_pesos = v_pesos  * porce.monto_ret/100
		    LET v_acciones = v_acciones  * porce.monto_ret/100
                 END IF
              WHEN v_tipo_movimiento = 4
                 IF ban_act_ret = 1 THEN
                    LET tot_1_acla.monto_act_ret = v_pesos * porce.monto_act_ret/100
                    LET acc_acla.monto_act_ret   = v_acciones * porce.monto_act_ret/100
		    LET v_pesos = v_pesos  * porce.monto_act_ret/100
		    LET v_acciones = v_acciones  * porce.monto_act_ret/100
                 END IF
              WHEN v_tipo_movimiento >= 100
                 IF ban_ret = 1 THEN
                    LET comi2.impt_ret = (v_monto_pesos * -1)* porce.monto_ret/100
                 END IF
            END CASE
         ELSE
            CASE
               WHEN v_tipo_movimiento = 1
                 IF reg_1.monto_ces_vej_pat > 0 THEN
		    LET porce.monto_ces_vej = reg_1.monto_ces_vej_pat * 100/r.impt_ces_vej
                    LET tot_2.monto_ces_vej = (v_pesos - v_monto_pesos) * porce.monto_ces_vej/100
                    LET acc.monto_ces_vej   = v_acciones * porce.monto_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_ces_vej/100
                    LET ban_ces_vej = 1
                 END IF

              WHEN v_tipo_movimiento = 2
                 IF reg_1.monto_act_ces_vej > 0 THEN
		    LET porce.monto_act_ces_vej = reg_1.monto_act_ces_vej * 100/r.impt_act_r_ces_vej
                    LET tot_2.monto_act_ces_vej = (v_pesos - v_monto_pesos) * porce.monto_act_ces_vej/100
                    LET acc.monto_act_ces_vej   = v_acciones * porce.monto_act_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_act_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_act_ces_vej/100
                    LET ban_act_ces_vej = 1
                 END IF

              WHEN v_tipo_movimiento = 3 
                 IF ban_ces_vej = 1 THEN
                    LET tot_1_acla.monto_ces_vej = v_pesos  * porce.monto_ces_vej/100
                    LET acc_acla.monto_ces_vej   = v_acciones  * porce.monto_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_ces_vej/100
                 END IF

              WHEN v_tipo_movimiento = 4
                 IF ban_act_ces_vej = 1 THEN
                    LET tot_1_acla.monto_act_ces_vej = v_pesos * porce.monto_act_ces_vej/100
                    LET acc_acla.monto_act_ces_vej   = v_acciones * porce.monto_act_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_act_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_act_ces_vej/100
                 END IF
              WHEN v_tipo_movimiento >= 100
                 IF ban_ces_vej = 1 THEN
                    LET comi2.impt_ces_vej = (v_monto_pesos * -1)* porce.monto_ces_vej/100
                 END IF
            END CASE
         END IF

         IF v_tipo_movimiento < 100 THEN
            CASE v_tipo_movimiento
               WHEN 1 LET x_movimiento = 540
               WHEN 2 LET x_movimiento = 545
               WHEN 3 LET x_movimiento = 550
               WHEN 4 LET x_movimiento = 555
            END CASE

            LET precio_dia = precio_acc_dia (v_siefore,hoy)
{
display "menores"
display reg_1.nss
display v_pesos
display v_acciones
display v_subcuenta
display x_movimiento
display precio_dia
display hoy
display v_siefore
}
            CALL provisiona_cuenta(v_pesos,
                                   v_acciones,
                                   reg_1.*,
                                   v_subcuenta,
                                   x_movimiento,
                                   precio_dia,
                                   hoy,
                                   v_siefore)
         END IF
      END FOREACH
   END IF
ELSE
   IF reg_1.monto_par_viv > 0 THEN

      LET hoy = "02/21/2005"
      LET precio_acc = precio_acc_dia (11,hoy)

      LET  tot.monto_aport_pat = reg_1.monto_par_viv * precio_acc

      LET tot_2.monto_aport_pat = tot.monto_aport_pat

      LET tot_2.monto_par_viv = reg_1.monto_par_viv

      CALL provisiona_cuenta(tot.monto_aport_pat,
                             tot_2.monto_par_viv,
                             reg_1.*,
                             4,
                             540,
                             0,
                             hoy,
                             11)
   END IF
END IF
---- SIE_DOS FIN

   IF vaport_pat = "N" THEN
      IF tot_1.monto_ces_vej IS NULL THEN
         LET tot_1.monto_ces_vej = 0
      END IF

      IF tot_1.monto_act_ces_vej IS NULL THEN
         LET tot_1.monto_act_ces_vej = 0 
      END IF

      IF tot_1_acla.monto_ces_vej IS NULL THEN
         LET tot_1_acla.monto_ces_vej = 0
      END IF

      IF tot_1_acla.monto_act_ces_vej IS NULL THEN
         LET tot_1_acla.monto_act_ces_vej = 0
      END IF

      IF acc.monto_ces_vej IS NULL THEN
         LET acc.monto_ces_vej = 0
      END IF

      IF acc.monto_act_ces_vej IS NULL THEN
         LET acc.monto_act_ces_vej = 0 
      END IF

      IF acc_acla.monto_ces_vej IS NULL THEN
         LET tot_1_acla.monto_ces_vej = 0
      END IF

      IF acc_acla.monto_act_ces_vej IS NULL THEN
         LET tot_1_acla.monto_act_ces_vej = 0
      END IF

      CALL provisiona_cuenta(tot_1.monto_ces_vej + tot_1.monto_act_ces_vej + tot_1_acla.monto_ces_vej + tot_1_acla.monto_act_ces_vej,
                          acc.monto_ces_vej + acc.monto_act_ces_vej + acc_acla.monto_ces_vej + acc_acla.monto_act_ces_vej,
                          reg_1.*,
                          3,
                          1,
                          precio_dia,
                          hoy,
                          2)
   END IF

   -----Montos de aportes patronales (vivienda)--------------------------


   --- llenar tabla historica de plusvalia y minusvalia-----------

   IF tot_2.monto_ret IS NULL THEN
      LET tot_2.monto_ret = 0
   END IF

   IF tot_2.monto_act_ret IS NULL THEN
      LET tot_2.monto_act_ret = 0
   END IF

   IF tot_1_acla.monto_ret IS NULL THEN
      LET tot_1_acla.monto_ret = 0
   END IF

   IF tot_1_acla.monto_act_ret IS NULL THEN
      LET tot_1_acla.monto_act_ret = 0
   END IF
   IF tot_2.monto_ces_vej IS NULL THEN
      LET tot_2.monto_ces_vej = 0
   END IF

   IF tot_2.monto_act_ces_vej IS NULL THEN
      LET tot_2.monto_act_ces_vej = 0
   END IF

   IF tot_1_acla.monto_ces_vej IS NULL THEN
      LET tot_1_acla.monto_ces_vej = 0
   END IF

   IF tot_1_acla.monto_act_ces_vej IS NULL THEN
      LET tot_1_acla.monto_act_ces_vej = 0
   END IF

   IF acc.monto_ret IS NULL THEN
      LET acc.monto_ret = 0
   END IF
   IF acc_acla.monto_ret IS NULL THEN
      LET acc_acla.monto_ret = 0
   END IF

   IF acc.monto_act_ret IS NULL THEN
      LET acc.monto_act_ret = 0
   END IF

   IF acc_acla.monto_act_ret IS NULL THEN
      LET acc_acla.monto_act_ret = 0
   END IF

   IF acc.monto_ces_vej IS NULL THEN
      LET acc.monto_ces_vej = 0
   END IF

   IF acc_acla.monto_ces_vej IS NULL THEN
      LET acc_acla.monto_ces_vej = 0
   END IF
   IF acc.monto_act_ces_vej IS NULL THEN
      LET acc.monto_act_ces_vej = 0
   END IF

   IF acc_acla.monto_act_ces_vej IS NULL THEN
      LET acc_acla.monto_act_ces_vej = 0
   END IF

   IF tot_2.monto_ret > 0 THEN
      LET total.monto_ret     = tot_2.monto_ret +
                                tot_2.monto_act_ret +
                                tot_1_acla.monto_ret +
                                tot_1_acla.monto_act_ret

      LET total_acc.monto_ret = acc.monto_ret +
                                acc.monto_act_ret +
                                acc_acla.monto_ret +
                                acc_acla.monto_act_ret

   ELSE
      LET total.monto_ret     = tot_2.monto_ret +
                                tot_2.monto_act_ret

      LET total_acla.monto_ret = tot_1_acla.monto_ret +
                                 tot_1_acla.monto_act_ret

      LET total_acc.monto_ret = acc.monto_ret +
                                acc.monto_act_ret

      LET total_acc_acla.monto_ret = acc_acla.monto_ret +
                                     acc_acla.monto_act_ret
   END IF

   IF tot_2.monto_ces_vej > 0 THEN
      LET total.monto_ces_vej = tot_2.monto_ces_vej +
                                tot_2.monto_act_ces_vej +
                                tot_1_acla.monto_ces_vej +
                                tot_1_acla.monto_act_ces_vej

      LET total_acc.monto_ces_vej = acc.monto_ces_vej +
                                    acc.monto_act_ces_vej +
                                    acc_acla.monto_ces_vej +
                                    acc_acla.monto_act_ces_vej

   ELSE
      LET total.monto_ces_vej = tot_2.monto_ces_vej +
                                tot_2.monto_act_ces_vej

      LET total_acla.monto_ces_vej = tot_1_acla.monto_ces_vej +
                                     tot_1_acla.monto_act_ces_vej

      LET total_acc.monto_ces_vej = acc.monto_ces_vej +
                                    acc.monto_act_ces_vej

      LET total_acc_acla.monto_ces_vej = acc_acla.monto_ces_vej +
                                         acc_acla.monto_act_ces_vej

   END IF

   LET precio_acc = precio_acc_dia (v_siefore,v_fecha_conversion)

   IF total.monto_ret> 0 OR
      total.monto_ces_vej > 0 OR
      tot_2.monto_aport_pat > 0 THEN

      LET vtipo_registro = "04"

      IF tot_2.monto_aport_pat IS NULL THEN
         LET tot_2.monto_aport_pat = 0
      END IF

      IF tot_2.monto_par_viv IS NULL THEN
         LET tot_2.monto_par_viv = 0
      END IF

      IF precio_acc IS NULL THEN
         LET precio_acc = 0
      END IF

      INSERT INTO exc_exceso_plu_min
      VALUES (reg_1.folio,              -- folio
              vtipo_registro,           -- tipo registro
              reg_1.ident_servicio,     -- identificador de servicio
              reg_1.consec_reg_lote,    -- ret_consecutivo lote
              reg_1.reg_patronal_imss,  -- registro patronal imss
              reg_1.nss,                -- nss
              total.monto_ret,          -- monto minusvalia retiro
              total.monto_ces_vej,      -- monto minusvalia ces y vej
              0,                        -- monto minusvalia cuo soc
              0,                        -- monto minusvalia estatal
              0,                        -- monto minusvalia especial
              0,                        -- monto plusvalia patronal (viv)
              total_acc.monto_ret,      -- acciones minusvalia retiro
              total_acc.monto_ces_vej,  -- acciones minusvalia ces y vej
              0,                        -- acciones minusvalia cuo soc
              0,                        -- acciones minusvalia estatal
              0,                        -- acciones minusvalia especial
              tot_2.monto_par_viv,       -- participaciones de vivienda
              v_fecha_conversion,        -- fecha liquidacion inicio
              v_precio_accion                -- precio accion de inicio
             )
   ELSE
      IF total.monto_ret < 0 OR
         total.monto_ces_vej < 0 OR
         tot_2.monto_aport_pat < 0 THEN

         LET vtipo_registro = "05"

         IF tot_2.monto_aport_pat IS NULL THEN
            LET tot_2.monto_aport_pat = 0
         END IF

         IF tot_2.monto_par_viv IS NULL THEN
            LET tot_2.monto_par_viv = 0
         END IF

         IF precio_acc IS NULL THEN
            LET precio_acc = 0
         END IF

         INSERT INTO exc_exceso_plu_min
         VALUES (reg_1.folio,              -- folio
                 vtipo_registro,           -- tipo registro
                 reg_1.ident_servicio,     -- identificador de servicio
                 reg_1.consec_reg_lote,    -- ret_consecutivo lote
                 reg_1.reg_patronal_imss,  -- registro patronal imss
                 reg_1.nss,                -- nss
                 total.monto_ret,          -- monto plusvalia retiro
                 total.monto_ces_vej,      -- monto plusvalia ces y vej
                 0,                        -- monto plusvalia cuo soc
                 0,                        -- monto plusvalia estatal
                 0,                        -- monto plusvalia especial
                 0,                        -- monto plusvalia patronal (viv)
                 total_acc.monto_ret,      -- acciones minusvalia retiro
                 total_acc.monto_ces_vej,  -- acciones minusvalia ces y vej
                 0,                        -- acciones minusvalia cuo soc
                 0,                        -- acciones minusvalia estatal
                 0,                        -- acciones minusvalia especial
                 tot_2.monto_par_viv,       -- participaciones de vivienda
                 v_fecha_conversion,            -- fecha liquidacion inicio
                 v_precio_accion                -- precio accion de inicio
                )

         IF total_acla.monto_ret > 0 OR
            total_acla.monto_ces_vej > 0 THEN
            LET vtipo_registro = "04"

            IF tot_2.monto_aport_pat IS NULL THEN
               LET tot_2.monto_aport_pat = 0
            END IF

            IF tot_2.monto_par_viv IS NULL THEN
               LET tot_2.monto_par_viv = 0
            END IF

            IF precio_acc IS NULL THEN
               LET precio_acc = 0
            END IF

            INSERT INTO exc_exceso_plu_min
            VALUES (reg_1.folio,                   -- folio
                    vtipo_registro,                -- tipo registro
                    reg_1.ident_servicio,          -- identificador de servicio
                    reg_1.consec_reg_lote,         -- consecutivo lote
                    reg_1.reg_patronal_imss,       -- registro patronal imss
                    reg_1.nss,                     -- nss
                    total_acla.monto_ret,          -- monto minusvalia retiro
                    total_acla.monto_ces_vej,      -- monto minusvalia ces y vej
                    0,                             -- monto minusvalia cuo soc
                    0,                             -- monto minusvalia estatal
                    0,                             -- monto minusvalia especial
                    0,                                   -- monto plusvalia patronal (viv)
                    total_acc_acla.monto_ret,      -- acciones minusvalia retiro
                    total_acc_acla.monto_ces_vej,  -- acciones minusvalia ces y vej
                    0,                             -- acciones minusvalia cuo soc
                    0,                             -- acciones minusvalia estatal
                    0,                             -- acciones minusvalia especial
                    tot_2.monto_par_viv,       -- participaciones de vivienda
                    v_fecha_conversion,            -- fecha liquidacion inicio
                    0                              -- precio accion de inicio
                   )
         END IF
      END IF
   END IF

   IF comi2.impt_ret IS NULL THEN
      LET comi2.impt_ret = 0
   END IF

   IF comi2.impt_ces_vej IS NULL THEN
      LET comi2.impt_ces_vej = 0
   END IF

--   IF comi2.impt_ret > 0 OR 
--      comi2.impt_ces_vej > 0 THEN
      INSERT INTO exc_exceso_comis
      VALUES (reg_1.folio,              -- folio
              reg_1.consec_reg_lote,    -- ret_consecutivo lote
              reg_1.reg_patronal_imss,  -- registro patronal imss
              reg_1.nss,                -- nss
              comi2.impt_ret,           -- monto omision retiro
              comi2.impt_ces_vej        -- monto comision ces y vej
             )
--   END IF

   CALL Contador_aceptado()
END FUNCTION
#*********************************************************************
FUNCTION provisiona_cuenta(monto_reg,
                        monto_acc_reg,
                        reg_2,
                        subcuenta,
                        movimiento,
                        precio_dia,
                        x_fecha_proceso,
                        siefore)

   DEFINE  monto_reg          DECIMAL(16,6),
           monto_acc_reg      DECIMAL(16,6),
           subcuenta          INTEGER,
           movimiento         INTEGER,
           precio_dia         DECIMAL(16,6),
           monto_en_pesos     DECIMAL(16,6),
           monto_en_acciones  DECIMAL(16,6),
           vfecha_archivo     DATE,
           aux_id_aportante   CHAR(20),
           x_fecha_proceso    DATE,
           siefore            CHAR(6),
           x_status           SMALLINT

   DEFINE reg_2  RECORD LIKE exc_det_exceso.*

   IF subcuenta <> 3 THEN
      LET monto_en_pesos    = monto_reg * -1

      LET monto_en_acciones = monto_acc_reg * -1
      LET aux_id_aportante = "PAG-EXC-PAT"
   ELSE
      LET aux_id_aportante = "PAG-EXC-VOL"
   END IF

   DECLARE cursor_prov_cargo CURSOR FOR clausula_sql3

   OPEN cursor_prov_cargo USING reg_2.folio,
                                reg_2.folio_pago_sua,
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

END FUNCTION
#*********************************************************************
FUNCTION valida_pendiente(vestado_proceso)

   DEFINE tipo_rech               CHAR(3),
          vestado_proceso         SMALLINT

   LET tipo_rech = "100"

   IF vestado_proceso >= 400 AND   -- PENDIENTE POR ESTAR 
      vestado_proceso <= 499 THEN  -- EN PROCESO DE RETIRO
      LET tipo_rech  = "587"
   END IF

   IF vestado_proceso >= 800 AND   -- PENDIENTE POR ESTAR 
      vestado_proceso <= 850 THEN
      LET tipo_rech  = "586"
   END IF

   IF vestado_proceso = 880 THEN
      LET tipo_rech  = "587"
   END IF

   IF vestado_proceso >= 540 AND   -- PENDIENTE POR ESTAR EN 
      vestado_proceso <= 542 THEN  -- PROCESO DE DEVOLUCION 
      LET tipo_rech  = "575"       -- PAGOS EN EXCESO
   END IF

   IF vestado_proceso = 220 THEN   -- PENDIENTE POR ESTAR EN 
       LET tipo_rech = "528"       -- PROCESO DE TRASPASOS AF-AF
   END IF

   IF vestado_proceso = 230 THEN   -- PENDIENTE POR ESTAR EN 
      LET tipo_rech = "530"        -- PROCESO DE TRANSFERENCIA ACREDITADOS
   END IF

   RETURN tipo_rech
END FUNCTION
#*********************************************************************
FUNCTION valida_monto (v,g_reg,vces_vej)

   DEFINE tipo_rech            CHAR(3)

   DEFINE g_reg  RECORD LIKE exc_det_exceso.*

   DEFINE v  RECORD
          folio                INTEGER,
          fecha_recepcion      DATE,
          consec_reg_lote      INTEGER,
          impt_ret             DECIMAL(16,6),
          impt_ces_vej         DECIMAL(16,6),
          impt_act_rec_ret     DECIMAL(16,6),
          impt_act_r_ces_vej   DECIMAL(16,6),
          impt_cuota_soc       DECIMAL(16,6),
          impt_aport_est       DECIMAL(16,6),
          impt_aport_esp       DECIMAL(16,6),
          impt_aport_pat       DECIMAL(16,6)
   END RECORD 

   DEFINE vces_vej             DECIMAL(16,6),
          rval_porcentaje      DECIMAL(16,6)

   LET tipo_rech = "100"

   IF v.impt_ret < g_reg.monto_ret THEN
      LET tipo_rech =  "700"
   END IF

   IF v.impt_act_rec_ret < g_reg.monto_act_ret THEN
      LET tipo_rech =  "700"
   END IF

   IF v.impt_ces_vej < vces_vej THEN
      LET tipo_rech =  "700"
   END IF

   IF v.impt_act_r_ces_vej < g_reg.monto_act_ces_vej THEN
      LET tipo_rech =  "700"
   END IF

   IF g_reg.monto_cuo_soc > 0 THEN
      LET tipo_rech =  "700" 
   END IF

   IF g_reg.monto_aport_est > 0 THEN
      LET tipo_rech =  "700"
   END IF

   IF g_reg.monto_aport_esp > 0 THEN
      LET tipo_rech =  "700"
   END IF

   IF v.impt_aport_pat < g_reg.monto_aport_pat THEN
      LET tipo_rech =  "700"
   END IF

   RETURN tipo_rech
END FUNCTION
#*********************************************************************
FUNCTION valida_traspaso(regnss,
                         regperiodo_pago,
                         regfecha_pago,
                         regfolio_pago_sua,
                         regreg_patronal_imss,
                         regclave_ent_recep,
                         vtipo_solicitud,
                         regclave)

   DEFINE regnss                 CHAR(11),
          regperiodo_pago        CHAR(6),
          regfecha_pago          CHAR(8),
          regfolio_pago_sua      CHAR(6),
          regreg_patronal_imss   CHAR(11),
          regclave_ent_recep     CHAR(3),
          vtipo_solicitud        SMALLINT,
          regclave               CHAR(3),
          monto_vivienda         DECIMAL(16,6),
          diag_rechazo           CHAR(3),
          x_fecha_mov_banxico    DATE

   SELECT "X"
   FROM   taa_cd_det_cedido
   WHERE  n_seguro = regnss
   AND    fecha_trasp <= "02/21/2005"
   AND    estado IN (12,103)
   GROUP BY 1

   IF SQLCA.SQLCODE = 0 THEN      -- TRASPASO AF-AF COMO CEDENTE (T.historica)
      LET diag_rechazo = "948"  
      RETURN diag_rechazo
   END IF

   SELECT fecha_mov_banxico
   INTO   x_fecha_mov_banxico
   FROM   taa_det_recep_ps
   WHERE  nss_afo_recep = regnss
   AND    ident_operacion = "09"
   GROUP BY 1

   IF SQLCA.SQLCODE = 0 THEN      -- TRASPASO PS-AF
      SELECT "X"
      FROM   dis_det_aporte
      WHERE  n_seguro          = regnss
      AND    fecha_recepcion    >= x_fecha_mov_banxico
      AND    periodo_pago      = regperiodo_pago
      AND    fech_pago         = regfecha_pago
      AND    folio_pago_sua    = regfolio_pago_sua
      AND    reg_patronal_imss = regreg_patronal_imss
      AND    cve_ent_receptora = regclave_ent_recep
      AND    result_operacion[2] = "1"

      IF SQLCA.SQLCODE = 0 THEN      -- HISPAGOS DET SI ESTA EL APORTE
         LET diag_rechazo = "100"
         RETURN diag_rechazo
      END IF

      LET diag_rechazo = "948"
      RETURN diag_rechazo
   END IF

   SELECT "X"
   FROM   safre_tmp:det_tras
   WHERE  nss_afo_recep = regnss
   GROUP BY 1

   IF SQLCA.SQLCODE = 0 THEN      -- TRASPASO PS-AFORE
      LET diag_rechazo = "948"
      RETURN diag_rechazo
   END IF

   SELECT fecha_mov_banxico
   INTO   x_fecha_mov_banxico
   FROM   taa_rcv_recepcion
   WHERE  nss = regnss
   AND    ident_operacion = "09"
   GROUP BY 1

   IF SQLCA.SQLCODE = 0 THEN      -- TRASPASO AFORE-RECEP

      SELECT "X"
      FROM   dis_det_aporte
      WHERE  n_seguro          = regnss
      AND    fecha_recepcion    >= x_fecha_mov_banxico
      AND    periodo_pago      = regperiodo_pago
      AND    fech_pago         = regfecha_pago
      AND    folio_pago_sua    = regfolio_pago_sua
      AND    reg_patronal_imss = regreg_patronal_imss
      AND    cve_ent_receptora = regclave_ent_recep
      AND    result_operacion[2] = "1"

      IF SQLCA.SQLCODE = 0 THEN      -- HISPAGOS DET SI ESTA EL APORTE
         LET diag_rechazo = "100"
         RETURN diag_rechazo
      END IF

      LET diag_rechazo = "948"
      RETURN diag_rechazo
   END IF

   SELECT "X"
   FROM   safre_tmp:det_tra_rcv
   WHERE  n_seguro = regnss
   GROUP BY 1

   IF SQLCA.SQLCODE = 0 THEN      -- TRASPASO AFORE-RECEP(T.temporal)
      LET diag_rechazo = "948"
      RETURN diag_rechazo
   END IF

   IF regclave = "002" THEN
      SELECT "X"
      FROM    acr_det_cedido
      WHERE   nss_afore  = regnss
      AND     estado     = 0
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN      -- TRANSFERENCIA DE ACREDITADOS (T.historica)
         LET diag_rechazo = "595"
         RETURN diag_rechazo
      END IF

      SELECT "X"
      FROM    safre_tmp:det_tra_acr
      WHERE   nss_afore  = regnss
      AND     estado     = 0
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         LET diag_rechazo = "595"
         RETURN diag_rechazo
      END IF
   END IF

   LET diag_rechazo = "100"
   RETURN diag_rechazo

END FUNCTION
#*********************************************************************
FUNCTION valida_retiro(regclave,regnss)

   DEFINE regnss          CHAR(11),
          diag_rechazo    CHAR(3),
          regclave        CHAR(3)

   IF regclave = "001" THEN
      SELECT "X"
      FROM   tmp_dis_cuenta
      WHERE  subcuenta IN (1,2)
      AND   (tipo_movimiento BETWEEN 401 AND 412 OR
             tipo_movimiento BETWEEN 457 AND 485 OR
             tipo_movimiento = 488  OR
             tipo_movimiento BETWEEN 491 AND 492 OR
             tipo_movimiento BETWEEN 800 AND 870)
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         LET diag_rechazo = "699"
         RETURN diag_rechazo
      END IF
   END IF

   IF regclave = "002" THEN
      SELECT "X"
      FROM   tmp_dis_cuenta
      WHERE  subcuenta  = 4
      AND   (tipo_movimiento BETWEEN 401 AND 412 OR
             tipo_movimiento BETWEEN 457 AND 485 OR
             tipo_movimiento = 488  OR
             tipo_movimiento BETWEEN 491 AND 492 OR
             tipo_movimiento BETWEEN 800 AND 870)
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         LET diag_rechazo = "699"
         RETURN diag_rechazo
      END IF
   END IF

   IF regclave = "001" THEN
      SELECT "X"
      FROM   tmp_dis_cuenta
      WHERE  subcuenta IN (1,2)
      AND    tipo_movimiento BETWEEN 486 AND 487
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         LET diag_rechazo = "945"
         RETURN diag_rechazo
      END IF
   END IF

   LET diag_rechazo = "100"
   RETURN diag_rechazo
END FUNCTION
#*********************************************************************
FUNCTION mes_a_bim (anyo,mes)

   DEFINE anyo,
          mes,
          anyo_b,
          mes_b,
          bimestre   SMALLINT

   LET anyo_b = anyo

   CASE mes
      WHEN  1 LET mes_b = 1
      WHEN  2 LET mes_b = 1
      WHEN  3 LET mes_b = 2
      WHEN  4 LET mes_b = 2
      WHEN  5 LET mes_b = 3
      WHEN  6 LET mes_b = 3
      WHEN  7 LET mes_b = 4
      WHEN  8 LET mes_b = 4
      WHEN  9 LET mes_b = 5
      WHEN 10 LET mes_b = 5
      WHEN 11 LET mes_b = 6
      WHEN 12 LET mes_b = 6
   END CASE

   LET bimestre = (anyo_b * 10) + mes_b

   RETURN bimestre

END FUNCTION
#*********************************************************************
FUNCTION cuenta_acc (reg_1,rfolio,x_subcuenta,x_movimiento)

   DEFINE reg_1  RECORD LIKE exc_det_exceso.* 

   DEFINE rfolio               INTEGER,
          x_subcuenta            INTEGER,
          x_movimiento           INTEGER,
          fecha_recepcion      DATE,
          precio_acc           DECIMAL(16,6),
          opc                  CHAR(1),
          vsua2                CHAR(3),
          x_fecha_liquidacion  DATE,
          x_ident_pago         CHAR(2)

   SELECT fecha_conversion,
          precio_accion
   INTO   x_fecha_liquidacion,
          precio_acc
   FROM   tmp_dis_cuenta
   WHERE  folio = folio
   AND    subcuenta  = x_subcuenta
   AND    tipo_movimiento = x_movimiento 
   GROUP BY 1,2

   IF SQLCA.SQLCODE <> 0 THEN
      DISPLAY "NO HAY PRECIO ACCION",STATUS
      LET precio_acc = 0
      LET x_fecha_liquidacion = NULL
   END IF

   RETURN precio_acc,
          x_fecha_liquidacion

END FUNCTION
#*********************************************************************
FUNCTION precio_acc_dia (x_siefore,fecha_recepcion)

   DEFINE fecha_recepcion  DATE,
          precio_dia       DECIMAL(16,6),
          x_siefore         SMALLINT

   LET cla_sel = " SELECT precio_del_dia ",
                 " FROM   glo_valor_accion ",
                 " WHERE  fecha_valuacion = ","'",fecha_recepcion,"'",
                 " AND    codigo_siefore = ",x_siefore

   PREPARE claexe_pre FROM cla_sel

   DECLARE cursor_pre CURSOR FOR claexe_pre

   OPEN cursor_pre
      FETCH cursor_pre INTO precio_dia

      IF SQLCA.SQLCODE = 0 THEN
         CLOSE cursor_pre
         RETURN precio_dia
      END IF

      DISPLAY "NO HAY PRECIO DE ACCION DEL DIA CON SIEFORE:",x_siefore CLIPPED," ",STATUS,fecha_recepcion

      LET precio_dia = 0
      RETURN precio_dia

END FUNCTION
#*********************************************************************
FUNCTION Contador_aceptado()
   LET vcont_acep = vcont_acep + 1
END FUNCTION
#*********************************************************************
FUNCTION Contador_rechazo()
   LET vcont_rech = vcont_rech + 1
END FUNCTION
#*********************************************************************
FUNCTION Contador_pendiente()
   LET vcont_pend = vcont_pend + 1
END FUNCTION
#*********************************************************************
REPORT salida(vfolio,
              vfecha_recepcion,
              vhora_recepcion,
              vestado,
              vfecha_estado,
              vhora_estado)

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
#*********************************************************************
REPORT salida1(vfolio,
               vestado,
               vfecha_estado,
               vhora_estado)

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
#*********************************************************************
REPORT salida2(vcontenido)

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
#*********************************************************************
FUNCTION marca(x_nss,
               x_marca_entra,
               x_consec_reg_lote,
               x_marca_estado,
               x_codigo_rechazo,
               x_marca_causa,
               x_fecha_causa,
               x_usuario)

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
######################################################################
FUNCTION des_marca(x_nss,
                   x_marca_entra,
                   x_consecutivo_lote,
                   x_marca_estado,
                   x_marca_causa,
                   x_usuario,
                   x_folio,
                   x_folio_sua )

   DEFINE x_nss             CHAR(11),
          x_operacion       CHAR(1),
          x_marca_entra     SMALLINT,
          x_consecutivo_lote     SMALLINT,
          x_marca_estado    SMALLINT,
          x_marca_causa     SMALLINT,
          x_codigo_rechazo  SMALLINT,
          x_usuario         CHAR(8),
          xx_codigo_marca   SMALLINT,
          xx_codigo_rechazo SMALLINT,
          ejecuta_procedure CHAR(200),
          x_folio           INTEGER,
          x_folio_sua       CHAR(6)

   EXECUTE clausula_sql2 USING x_nss,
                               x_marca_entra,
                               x_consecutivo_lote,
                               x_marca_estado,
                               x_marca_causa,
                               x_usuario

   UPDATE exc_det_exceso
   SET    result_operacion = "02",
          tipo_diagnostico = "593"
   WHERE nss = x_nss
   AND   folio = x_folio
   AND   consec_reg_lote = x_consecutivo_lote
   AND   folio_pago_sua =  x_folio_sua

   CALL contador_rechazo()
END FUNCTION
######################################################################
FUNCTION saldo_al_dia(v_nss,
                      v_subcuenta,
                      v_grupo)

   DEFINE v_saldo_dia        CHAR(100),
          v_nss              CHAR(11),
          v_subcuenta        SMALLINT,
          v_grupo            SMALLINT

   DEFINE f_subcuenta        SMALLINT,
          f_siefore          SMALLINT,
          f_monto_acc        DECIMAL(16,6),
          f_monto_pes        DECIMAL(16,6)

   DEFINE v_saldo_acc        DECIMAL(16,6),
          v_saldo_pes        DECIMAL(16,6),
          hoy          DATE

   LET hoy = "02/21/2005"

   WHENEVER ERROR CONTINUE
      DROP TABLE temp_saldo
   WHENEVER ERROR STOP

   CREATE TEMP TABLE temp_saldo
   (subcuenta      SMALLINT,
    siefore        SMALLINT,
    acciones       DECIMAL(16,6),
    pesos          DECIMAL(16,6)
   )

   LET v_saldo_dia = "EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? ) "
 
   PREPARE eje_saldo_dia FROM v_saldo_dia

   LET v_saldo_acc  = 0
   LET v_saldo_pes  = 0

   DECLARE c_saldo CURSOR FOR  eje_saldo_dia 
   FOREACH c_saldo  USING v_nss,
                          v_subcuenta,
                          v_grupo,
                          hoy
                     INTO f_subcuenta,
                          f_siefore,
                          f_monto_acc,
                          f_monto_pes

        INSERT INTO temp_saldo
        VALUES (f_subcuenta,
                f_siefore,
                f_monto_acc,
                f_monto_pes
               )

        LET v_saldo_acc  = v_saldo_acc + f_monto_acc 
        LET v_saldo_pes  = v_saldo_pes + f_monto_pes
   END FOREACH

   RETURN v_saldo_acc,
          v_saldo_pes
END FUNCTION
######################################################################
FUNCTION genera_tmp_cuenta (p_nss)

   DEFINE p_nss            CHAR(11),
          v_nombre_tabla   CHAR(20),
          sel_his          CHAR(2000)

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta;
   WHENEVER ERROR STOP

   DECLARE cur_his CURSOR FOR
   SELECT  tabname
   FROM    systables
   WHERE   tabname MATCHES "dis_cuenta??"

   FOREACH cur_his INTO v_nombre_tabla

      LET sel_his = sel_his CLIPPED,
               " SELECT * ",
               " FROM ",v_nombre_tabla,
               " WHERE nss = ","'",p_nss,"'"  ,
	       " AND    subcuenta IN (1,2,4) ",
               " AND   tipo_movimiento NOT IN (888,999) ",
               " UNION ALL "
   END FOREACH
   CLOSE cur_his

   LET sel_his = sel_his CLIPPED,
               " SELECT * ",
               " FROM   dis_cuenta ",
               " WHERE  nss = ","'",p_nss,"'"  ,
	       " AND    subcuenta IN (1,2,4) ",
               " AND    tipo_movimiento NOT IN (888,999) ",
               " INTO TEMP tmp_dis_cuenta "

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
######################################################################
{

   IF reg_1.monto_ret > 0 THEN
      CALL comision (1,
                     100,
                     r.fecha_recepcion,
                     r.folio,
                     vfentcons,
                     vfecha_pri_general,
                     reg_1.periodo_pago)

           RETURNING rval_porcentaje

      CALL cuenta_acc (reg_1.*,r.folio,1,1)
           RETURNING precio_acc,
                     xxfecha_liqui

      IF precio_acc = 0 THEN
         UPDATE cta_ctr_cuenta
         SET    marca_cod    = 0,
                fecha_act_marca = "02/21/2005"
         WHERE  nss = reg_1.nss
         AND    marca_cod = 540

         CALL actualiza_reg(reg_1.folio,
                            reg_1.nss,
                            "02",
                            "327",
                            reg_1.consec_reg_lote,
                            reg_1.folio_pago_sua,
                            "540","")
              RETURN
      END IF

      LET monto_ret_acc = reg_1.monto_ret / precio_acc
      LET impt_ret_acc  = r.impt_ret / precio_acc

      LET porce.monto_ret = monto_ret_acc / impt_ret_acc

      LET neto.impt_ret = impt_ret_acc - (impt_ret_acc * rval_porcentaje/100)

      LET comi_impt_ret_acc = impt_ret_acc - neto.impt_ret   --COMISION
 
      LET comi2_impt_ret_acc = ((monto_ret_acc * comi_impt_ret_acc)/impt_ret_acc)*-1

      LET tot_monto_ret_acc = monto_ret_acc + comi2_impt_ret_acc

      LET acc.monto_ret = tot_monto_ret_acc

      LET comi.impt_ret  = comi_impt_ret_acc * precio_acc
      LET comi2.impt_ret = comi2_impt_ret_acc * precio_acc
      LET tot.monto_ret  = tot_monto_ret_acc * precio_acc

      LET hoy = "02/21/2005"

      LET precio_dia = precio_acc_dia (1,hoy)

      LET tot_1.monto_ret = acc.monto_ret * precio_dia

      LET tot_2.monto_ret = tot_1.monto_ret - tot.monto_ret
--display tot_1.monto_ret
--display tot_2.monto_ret

      CALL provisiona_cuenta(tot_1.monto_ret,
                          acc.monto_ret,
                          reg_1.*,
                          1,
                          540,
                          precio_dia,
                          hoy,
                          1)

      LET rval_porcentaje = 0

      SELECT impt_int_ret
      INTO   acla.monto_ret
      FROM   dis_det_interes
      WHERE  n_seguro          = reg_1.nss
      AND    folio             = r.folio
      AND    periodo_pago      = reg_1.periodo_pago
      AND    fech_pago         = aclafecha_pago1
      AND    folio_pago_sua    = reg_1.folio_pago_sua
      AND    reg_patronal_imss = reg_1.reg_patronal_imss
      AND    cve_ent_receptora = reg_1.clave_ent_recep

      IF SQLCA.SQLCODE = 0 THEN

         LET acla.monto_ret = acla.monto_ret / 100

         CALL cuenta_acc (reg_1.*,r.folio,1,3)
              RETURNING precio_acc,
                        xxfecha_liqui

         LET tot_acla.monto_ret = acla.monto_ret * porce.monto_ret

         LET acc_acla.monto_ret = tot_acla.monto_ret / precio_acc

         LET hoy = "02/21/2005"

         CALL precio_acc_dia (1,hoy)
              RETURNING precio_dia

         LET tot_1_acla.monto_ret = acc_acla.monto_ret * precio_dia

         CALL provisiona_cuenta(tot_1_acla.monto_ret,
                             acc_acla.monto_ret,
                             reg_1.*,
                             1,
                             550,
                             precio_dia,
                             hoy,
                             1)
      END IF
   END IF

   IF reg_1.monto_act_ret > 0 THEN

      CALL cuenta_acc (reg_1.*,r.folio,1,2)
           RETURNING precio_acc,
                     xxfecha_liqui

      LET porce.monto_act_ret = reg_1.monto_act_ret / r.impt_act_rec_ret

      LET acc.monto_act_ret = reg_1.monto_act_ret / precio_acc

      LET hoy = "02/21/2005"

      CALL precio_acc_dia (1,hoy)
           RETURNING precio_dia

      LET tot_1.monto_act_ret = acc.monto_act_ret * precio_dia

      LET tot_2.monto_act_ret = tot_1.monto_act_ret - reg_1.monto_act_ret

      CALL provisiona_cuenta(tot_1.monto_act_ret,
                          acc.monto_act_ret,
                          reg_1.*,
                          1,
                          545,
                          precio_dia,
                          hoy,
                          1)
 
      SELECT impt_int_act_rr
      INTO   acla.monto_act_ret
      FROM   dis_det_interes
      WHERE  n_seguro          = reg_1.nss
      AND    folio             = r.folio
      AND    periodo_pago      = reg_1.periodo_pago
      AND    fech_pago         = aclafecha_pago1
      AND    folio_pago_sua    = reg_1.folio_pago_sua
      AND    reg_patronal_imss = reg_1.reg_patronal_imss
      AND    cve_ent_receptora = reg_1.clave_ent_recep

      IF SQLCA.SQLCODE = 0 THEN

         LET acla.monto_act_ret = acla.monto_act_ret / 100

         CALL cuenta_acc (reg_1.*,r.folio,1,4) ---- Trae precion accion de dis_cuenta
              RETURNING precio_acc,
                        xxfecha_liqui

         LET tot_acla.monto_act_ret = acla.monto_act_ret * porce.monto_act_ret

         LET acc_acla.monto_act_ret = tot_acla.monto_act_ret / precio_acc

         LET hoy = "02/21/2005"

         CALL precio_acc_dia (1,hoy)
              RETURNING precio_dia

         LET tot_1_acla.monto_act_ret = acc_acla.monto_act_ret * precio_dia

         CALL provisiona_cuenta(tot_1_acla.monto_act_ret,
                             acc_acla.monto_act_ret,
                             reg_1.*,
                             1,
                             555,
                             precio_dia,
                             hoy,
                             1)
      END IF
   END IF

   ----Montos de cesentia y vejez------------------------------

   IF vces_vej > 0 THEN

      CALL comision (2,
                     100,
                     r.fecha_recepcion,
                     r.folio,
                     vfentcons,
                     vfecha_pri_general,
                     reg_1.periodo_pago)

           RETURNING rval_porcentaje

      CALL cuenta_acc (reg_1.*,r.folio,2,1)
           RETURNING precio_acc,
                     xxfecha_liqui

      LET vces_vej_acc     = vces_vej / precio_acc
      LET impt_ces_vej_acc = r.impt_ces_vej / precio_acc

      LET porce.monto_ces_vej = vces_vej_acc / impt_ces_vej_acc

      LET neto.impt_ces_vej = impt_ces_vej_acc - (impt_ces_vej_acc * rval_porcentaje/100)

      LET comi_impt_ces_vej_acc = impt_ces_vej_acc - neto.impt_ces_vej   --COMISION

      LET comi2_impt_ces_vej_acc = ((vces_vej_acc * comi_impt_ces_vej_acc)/impt_ces_vej_acc)*-1

      LET tot_monto_ces_vej_acc = vces_vej_acc + comi2_impt_ces_vej_acc

      LET acc.monto_ces_vej = tot_monto_ces_vej_acc

      LET comi.impt_ces_vej  = comi_impt_ces_vej_acc * precio_acc
      LET comi2.impt_ces_vej = comi2_impt_ces_vej_acc * precio_acc
      LET tot.monto_ces_vej  = tot_monto_ces_vej_acc * precio_acc

      LET hoy = "02/21/2005"

      CALL precio_acc_dia (1,hoy)
           RETURNING precio_dia

      LET tot_1.monto_ces_vej = acc.monto_ces_vej * precio_dia

      LET tot_2.monto_ces_vej = tot_1.monto_ces_vej - tot.monto_ces_vej 

      CALL provisiona_cuenta(tot_1.monto_ces_vej,
                          acc.monto_ces_vej,
                          reg_1.*,
                          2,
                          540,
                          precio_dia,
                          hoy,
                          1)

      LET rval_porcentaje = 0

      SELECT impt_int_ces_vej
      INTO   acla.monto_ces_vej
      FROM   dis_det_interes
      WHERE  n_seguro          = reg_1.nss
      AND    folio             = r.folio
      AND    periodo_pago      = reg_1.periodo_pago
      AND    fech_pago         = aclafecha_pago1
      AND    folio_pago_sua    = reg_1.folio_pago_sua
      AND    reg_patronal_imss = reg_1.reg_patronal_imss
      AND    cve_ent_receptora = reg_1.clave_ent_recep

      IF SQLCA.SQLCODE = 0 THEN
         LET acla.monto_ces_vej = acla.monto_ces_vej / 100

         CALL cuenta_acc (reg_1.*,r.folio,2,3) ---- Trae precion accion de dis_cuenta
              RETURNING precio_acc,
                        xxfecha_liqui

         LET tot_acla.monto_ces_vej = acla.monto_ces_vej * porce.monto_ces_vej

         LET acc_acla.monto_ces_vej = tot_acla.monto_ces_vej / precio_acc

         LET hoy = "02/21/2005"

         CALL precio_acc_dia (1,hoy)
              RETURNING precio_dia

         LET tot_1_acla.monto_ces_vej = acc_acla.monto_ces_vej * precio_dia

         CALL provisiona_cuenta(tot_1_acla.monto_ces_vej,
                             acc_acla.monto_ces_vej,
                             reg_1.*,
                             2,
                             550,
                             precio_dia,
                             hoy,
                             1)
      END IF
   END IF

   IF reg_1.monto_act_ces_vej > 0 THEN

      CALL cuenta_acc (reg_1.*,r.folio,2,2) ---- Trae precion accion de dis_cuenta
           RETURNING precio_acc,
                     xxfecha_liqui

      LET porce.monto_act_ces_vej = reg_1.monto_act_ces_vej / r.impt_act_r_ces_vej

      LET acc.monto_act_ces_vej = reg_1.monto_act_ces_vej / precio_acc

      LET hoy = "02/21/2005"

      CALL precio_acc_dia (1,hoy)
           RETURNING precio_dia

      LET tot_1.monto_act_ces_vej = acc.monto_act_ces_vej * precio_dia

      LET tot_2.monto_act_ces_vej = tot_1.monto_act_ces_vej - reg_1.monto_act_ces_vej 

      CALL provisiona_cuenta(tot_1.monto_act_ces_vej,
                          acc.monto_act_ces_vej,
                          reg_1.*,
                          2,
                          545,
                          precio_dia,
                          hoy,
                          1)

      SELECT impt_int_act_rcv
      INTO   acla.monto_act_ces_vej
      FROM   dis_det_interes
      WHERE  n_seguro          = reg_1.nss
      AND    folio             = r.folio
      AND    periodo_pago      = reg_1.periodo_pago
      AND    fech_pago         = aclafecha_pago1
      AND    folio_pago_sua    = reg_1.folio_pago_sua
      AND    reg_patronal_imss = reg_1.reg_patronal_imss
      AND    cve_ent_receptora = reg_1.clave_ent_recep

      IF SQLCA.SQLCODE = 0 THEN
         LET acla.monto_act_ces_vej = acla.monto_act_ces_vej / 100

         CALL cuenta_acc (reg_1.*,r.folio,2,4) ---- Trae precion accion de dis_cuenta
              RETURNING precio_acc,
                        xxfecha_liqui

         LET tot_acla.monto_act_ces_vej = acla.monto_act_ces_vej * porce.monto_act_ces_vej

         LET acc_acla.monto_act_ces_vej = tot_acla.monto_act_ces_vej / precio_acc

         LET hoy = "02/21/2005"

         CALL precio_acc_dia (1,hoy)
              RETURNING precio_dia

         LET tot_1_acla.monto_act_ces_vej = acc_acla.monto_act_ces_vej * precio_dia

         CALL provisiona_cuenta(tot_1_acla.monto_act_ces_vej,
                             acc_acla.monto_act_ces_vej,
                             reg_1.*,
                             2,
                             555,
                             precio_dia,
                             hoy,
                             1)
      END IF
   END IF

#*********************************************************************
FUNCTION comision (sub,
                   mov,
                   fecha_recepcion,
                   x_folio,
                   vfentcons,
                   vfecha_pri_general,
                   vperiodo_pago)

   DEFINE sub                INTEGER,
          mov                INTEGER,
          fecha_recepcion    DATE,
          rval_porcentaje    DECIMAL(16,6),
          x_ano              SMALLINT,
          x_mes              SMALLINT,
          bim_cert           SMALLINT,
          bim_pago           SMALLINT,
          vfentcons          DATE,
          vfinitmte          DATE,
          vfecha_pri_general DATE,
          vperiodo_pago      CHAR(6),
          dias_antiguedad    INTEGER,
          x_folio            INTEGER,
          opc                char(1)

   LET x_ano = YEAR(vfentcons)
   LET x_mes = MONTH(vfentcons)

   LET bim_cert = mes_a_bim(x_ano,x_mes) 

   LET x_ano = vperiodo_pago[1,4]
   LET x_mes = vperiodo_pago[5,6]

   LET bim_pago = mes_a_bim(x_ano,x_mes)
        RETURNING bim_pago

   --LET dias_antiguedad = fecha_recepcion - vfecha_pri_general
   LET dias_antiguedad = fecha_recepcion - vfentcons

   IF dias_antiguedad < 0 THEN
       LET dias_antiguedad = 0
   END IF

   IF bim_cert <= bim_pago THEN
      SELECT "X"
      FROM   safre_af:dis_val_comision
      WHERE  subcuenta        = sub
     # AND    tipo_comision    = mov
      AND    fecha_desde     <= fecha_recepcion
      AND    tipo_comision NOT IN (110,130)
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         SELECT b.val_porcentaje
         INTO   rval_porcentaje
         FROM   tab_movimiento a,dis_val_comision b
         WHERE  a.codigo        = b.tipo_comision
         AND    b.subcuenta     = sub
         AND    b.antiguedad_hasta >= dias_antiguedad
         AND    b.antiguedad_desde <= dias_antiguedad

         IF SQLCA.SQLCODE <> 0 THEN
            LET rval_porcentaje = 0
         END IF
      ELSE
         IF dias_antiguedad <= 2191 THEN
            SELECT val_porcentaje
            INTO   rval_porcentaje
            FROM   safre_af:dis_his_val_comis
            WHERE  subcuenta    = sub
            AND    tipo_comision = mov
            AND    fecha_hasta >= fecha_recepcion
            AND    fecha_desde <= fecha_recepcion
 
            IF SQLCA.SQLCODE <> 0 THEN
               LET rval_porcentaje = 0
            END IF
         ELSE
            SELECT val_porcentaje
            INTO   rval_porcentaje
            FROM   safre_af:dis_his_val_comis
            WHERE  subcuenta    = sub
            AND    antiguedad_hasta >= dias_antiguedad
            AND    antiguedad_desde <= dias_antiguedad
            AND    fecha_hasta >= fecha_recepcion
            AND    fecha_desde <= fecha_recepcion

            IF SQLCA.SQLCODE <> 0 THEN
               LET rval_porcentaje = 0
            END IF
         END IF
      END IF
   ELSE
      LET rval_porcentaje = 0
   END IF

   RETURN rval_porcentaje
END FUNCTION
}
