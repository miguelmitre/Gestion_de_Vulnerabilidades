###############################################################################
#Proyecto          => Safre                                                   #
#Propietario       => E.F.P.                                                  #
#Modulo            => DIS.                                                    #
#Programa          => DISB070B                                                #
#Descripcion       => Genera nss c/saldo cero, sin cuenta, icefas devuleas,   #
#                  => pagos en exceso y voluntarias.                          #
#Fecha Inicio      => 04-julio-2004.                                          #
#Fecha Termino     =>                                                         #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
-------------------------------------------------------------------------------
#Modificado        => Alejandro Ramirez 29 Ago 2005                           #
#Descripcion       => (v2) Se agrego la actualizacion a ctar_ctr_cuenta       #
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE hoy         DATE
   DEFINE vfecha_proceso DATE
   DEFINE fecha_corte DATE
   DEFINE opc         CHAR(01)
   DEFINE cla_ins     CHAR(500)
   DEFINE acelera_on  CHAR(30)
   DEFINE cla_sel     CHAR(1000),
          cla_upd     CHAR(1000),
          vhora_final CHAR(08),
          vresultado  CHAR(50),
          vrow        INTEGER

END GLOBALS

MAIN

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
   DEFER INTERRUPT

   CALL STARTLOG("DISB070B.log")

   LET vfecha_proceso = ARG_VAL(1)
   LET fecha_corte   = ARG_VAL(2)

   CALL Proceso_principal()
   CALL Actualiza_ult_rcv()   --v2

   LET vhora_final = TIME
   LET vresultado = "PROCESO SALDO CERO TERMINADO"
   
   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM dis_ctrl_proceso ",
                 "WHERE proceso_cod = 'DISB070' ",
                  " AND etapa_cod = 9 " CLIPPED
   
   PREPARE claexe99 FROM cla_sel
   DECLARE cur_proceso99 CURSOR FOR claexe99
   OPEN cur_proceso99
      FETCH cur_proceso99 INTO vrow
   CLOSE cur_proceso99
   
   LET cla_upd = "UPDATE dis_ctrl_proceso ",
            "SET    dis_ctrl_proceso.hora_final = ","'",vhora_final,"'",",",
            "       dis_ctrl_proceso.folio      = 0 ",",",
            "       dis_ctrl_proceso.resultado  = ","'",vresultado,"'",
            " WHERE  dis_ctrl_proceso.proceso_cod  = 'DISB070' ",
            " AND    dis_ctrl_proceso.etapa_cod    = 9 ",
            " AND    dis_ctrl_proceso.consecutivo = ",vrow CLIPPED
   PREPARE claexe11 FROM cla_upd
   EXECUTE claexe11

END MAIN

FUNCTION Proceso_principal()

   LET acelera_on = "set pdqpriority high"  
   LET acelera_on = acelera_on CLIPPED      
   PREPARE exe_acelera_on  FROM acelera_on  
   EXECUTE exe_acelera_on
  
   CALL Crea_tablas()
   CALL Nss_sin_cuenta()
   CALL Base_nss_saldo()
   CALL Saldo_cero_pgo_exceso()
   CALL Saldo_cero_dev_icefa()
   CALL Saldo_cero_voluntaria()
   CALL Integra_nss()
END FUNCTION

FUNCTION Crea_tablas()
   DATABASE safre_tmp
   WHENEVER ERROR CONTINUE
  
   DROP TABLE nss_cuenta;
   CREATE TABLE nss_cuenta (nss CHAR(11));

   DROP TABLE nss_sin_cuenta;
   CREATE TABLE nss_sin_cuenta (nss CHAR(11));

   DROP TABLE cta_saldo;
   CREATE TABLE cta_saldo
      (nss      CHAR(11),
       sub      SMALLINT,
       pesos    DECIMAL(16,6),
       acciones DECIMAL(16,6)
       );

   DROP TABLE nss_exceso
   CREATE TABLE nss_exceso
      (nss      CHAR(11),
       sub      SMALLINT,
       pesos    DECIMAL(16,6),
       acciones DECIMAL(16,6)
       );

   DROP TABLE nss_exceso_cero_rcv;
   CREATE TABLE nss_exceso_cero_rcv
      (nss      CHAR(11),
       acciones DECIMAL(16,6)
       );

   DROP TABLE nss_exceso_cero_viv;
   CREATE TABLE nss_exceso_cero_viv
      (nss      CHAR(11),
       acciones DECIMAL(16,6)
       );

   DROP TABLE nss_icefa;
   CREATE TABLE nss_icefa
      (nss      CHAR(11),
       sub      SMALLINT,
       pesos    DECIMAL(16,6),
       acciones DECIMAL(16,6)
       );

   DROP TABLE nss_icefa_cero;
   CREATE TABLE nss_icefa_cero
      (nss      CHAR(11),
       acciones DECIMAL(16,6)
       );


   DROP TABLE nss_voluntaria;
   CREATE TABLE nss_voluntaria
      (nss      CHAR(11),
       sub      SMALLINT,
       pesos    DECIMAL(16,6),
       acciones DECIMAL(16,6)
       );

   DROP TABLE nss_vol_cero;
   CREATE TABLE nss_vol_cero
      (nss      CHAR(11),
       acciones DECIMAL(16,6)
       );

   DATABASE safre_af

   WHENEVER ERROR STOP

END FUNCTION

-----------------------------------------------------------------------------
FUNCTION Nss_sin_cuenta()
--   SET PDQPRIORITY HIGH;
   INSERT INTO safre_tmp:nss_cuenta
   SELECT UNIQUE nss
   FROM   safre_af:dis_cuenta

   INSERT INTO safre_tmp:nss_sin_cuenta
   SELECT n_seguro
   FROM   safre_tmp:cuota
   WHERE  n_seguro NOT IN (SELECT nss
                           FROM   safre_tmp:nss_cuenta)
   AND    fentcons <= fecha_corte
   AND    tipo_solicitud <> 5

END FUNCTION

-----------------------------------------------------------------------------
FUNCTION Base_nss_saldo()
   INSERT INTO safre_tmp:cta_saldo
   SELECT nss,
          subcuenta,
          SUM(monto_en_pesos),
          SUM(monto_en_acciones) 
   FROM   safre_af:dis_cuenta, 
          safre_tmp:cuota
   WHERE  fecha_conversion <= fecha_corte
   AND    n_seguro = nss
   AND    n_seguro not in (SELECT nss
                           FROM   safre_af:cta_act_marca
                           WHERE  marca_cod = 140)
   GROUP BY 1,2
{10}

END FUNCTION

-----------------------------------------------------------------------------
FUNCTION Saldo_cero_pgo_exceso()
   INSERT INTO safre_tmp:nss_exceso
   SELECT sdo.nss,
          sdo.sub,
          SUM(sdo.pesos),
          SUM(sdo.acciones)
   FROM   safre_tmp:cta_saldo sdo,
          safre_af:exc_det_exceso exc
   WHERE  sdo.nss = exc.nss
   AND    exc.result_operacion = "01"
   GROUP  BY 1,2

   INSERT INTO safre_tmp:nss_exceso_cero_rcv   ---- insertar en tabla general
   SELECT nss,
          SUM(acciones)
   FROM   safre_tmp:nss_exceso
   WHERE  sub not in (4,8)
   GROUP  BY 1 HAVING SUM(acciones) = 0
                        
   INSERT INTO safre_tmp:nss_exceso_cero_viv   ---- insertar en tabla general
   SELECT nss,
          SUM(pesos)
   FROM   safre_tmp:nss_exceso
   WHERE  sub in (4,8)
   GROUP  BY 1 HAVING SUM(pesos) = 0

END FUNCTION

-----------------------------------------------------------------------------
FUNCTION Saldo_cero_dev_icefa()
   INSERT INTO safre_tmp:nss_icefa
   SELECT sdo.nss,
          sdo.sub,
          SUM(sdo.pesos),
          SUM(sdo.acciones)
   FROM   safre_tmp:cta_saldo sdo,
          safre_af:dev_det_normal ice
   WHERE  sdo.nss = ice.n_seguro
   AND    ice.status = 1
   GROUP  BY 1,2

   INSERT INTO safre_tmp:nss_icefa_cero    ---- insertar en tabla general
   SELECT nss,
          SUM(acciones)
   FROM   safre_tmp:nss_icefa
   WHERE  sub = 7
   GROUP  BY 1 HAVING SUM(acciones) = 0

END FUNCTION

-----------------------------------------------------------------------------
FUNCTION Saldo_cero_voluntaria()
   INSERT INTO safre_tmp:nss_voluntaria
   SELECT sdo.nss,
          sdo.sub,
          SUM(sdo.pesos),
          SUM(sdo.acciones)
   FROM   safre_tmp:cta_saldo sdo,
          safre_af:int_det_voluntaria vol
   WHERE  sdo.nss = vol.nss
   AND    resul_operacion = "01"
   AND    estado = 2
   GROUP  BY 1,2

   INSERT INTO safre_tmp:nss_vol_cero
   SELECT nss,
          SUM(acciones)
   FROM   safre_tmp:nss_voluntaria
   WHERE  sub = 10
   GROUP  BY 1 HAVING SUM(acciones) = 0
   
END FUNCTION

-----------------------------------------------------------------------------
FUNCTION Integra_nss()
   LET cla_ins = "INSERT INTO cta_nss_sdo_cero ",
                 "SELECT nss,","'",fecha_corte,"'",
                 ",'",vfecha_proceso,"'",
                 ",1 ",
                 "FROM   safre_tmp:nss_sin_cuenta "
   LET cla_ins = cla_ins CLIPPED
   PREPARE cla_exe_ins1 FROM cla_ins
   EXECUTE cla_exe_ins1

   LET cla_ins = "INSERT INTO cta_nss_sdo_cero ",
                 "SELECT nss,","'",fecha_corte,"'",
                 ",'",vfecha_proceso,"'",
                 ",2 ",
                 "FROM   safre_tmp:nss_exceso_cero_rcv "
   LET cla_ins = cla_ins CLIPPED
   PREPARE cla_exe_ins2 FROM cla_ins
   EXECUTE cla_exe_ins2

   LET cla_ins = "INSERT INTO cta_nss_sdo_cero ",
                 "SELECT nss,","'",fecha_corte,"'",
                 ",'",vfecha_proceso,"'",
                 ",2 ",
                 "FROM   safre_tmp:nss_exceso_cero_viv "
   LET cla_ins = cla_ins CLIPPED
   PREPARE cla_exe_ins3 FROM cla_ins
   EXECUTE cla_exe_ins3

   LET cla_ins = "INSERT INTO cta_nss_sdo_cero ",
                 "SELECT nss,","'",fecha_corte,"'",
                 ",'",vfecha_proceso,"'",
                 ",3 ",
                 "FROM   safre_tmp:nss_icefa_cero "
   LET cla_ins = cla_ins CLIPPED
   PREPARE cla_exe_ins4 FROM cla_ins
   EXECUTE cla_exe_ins4

   LET cla_ins = "INSERT INTO cta_nss_sdo_cero ",
                 "SELECT nss,","'",fecha_corte,"'",
                 ",'",vfecha_proceso,"'",
                 ",4 ",
                 "FROM   safre_tmp:nss_vol_cero "
   LET cla_ins = cla_ins CLIPPED
   PREPARE cla_exe_ins5 FROM cla_ins
   EXECUTE cla_exe_ins5

END FUNCTION

FUNCTION Actualiza_ult_rcv()   --v2
  
  DEFINE xnss CHAR(11)

   DECLARE cur_ult_rcv CURSOR FOR
   SELECT unique nss
   FROM   cta_nss_sdo_cero
   WHERE  fecha_proceso =  vfecha_proceso

   FOREACH cur_ult_rcv INTO xnss

      UPDATE cta_ctr_cuenta
      SET    fecha_saldo_cero = vfecha_proceso,
             ind_saldo_cero   = 1
      WHERE  nss = xnss

   END FOREACH

END FUNCTION
