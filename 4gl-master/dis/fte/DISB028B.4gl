###############################################################################
#Proyecto          => AFORES                                                  #
#Propietario       => E.F.P                                                   #
#Modulo            => DIS                                                     #
#Programa          => DISB028B                                                #
#Descripcion       => ACTUALIZA CTAS INHABILITADAS A ACTIVAS DE INTER. TRAN.  #
#Autor             => ALEJANDRO RAMIREZ LARA.                                 #
#Fecha             => 08 noviembre 2005.                                      #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      vfolio                          INTEGER

   DEFINE
      reg_dis_cuenta RECORD
                        nss           CHAR(11),
                        subcuenta     SMALLINT,
                        fecha_conver  DATE,
                        marca_cod     SMALLINT,
                        correlativo   INTEGER,
                        monto         DECIMAL(16,6),
                        usuario       CHAR(8)
                     END RECORD

   DEFINE
      inter          RECORD
                        int_ret       DECIMAL(16,6),
                        int_cv        DECIMAL(16,6),
                        int_vol       DECIMAL(16,6),
                        int_viv       DECIMAL(16,6),
                        int_soc       DECIMAL(16,6)
                     END RECORD
END GLOBALS


MAIN
   CALL STARTLOG("DISB028B.log")

   LET vfolio = ARG_VAL(1)

   DISPLAY "Arranca DISB028B....",vfolio

   CALL crea_tabla()
   CALL Actualiza_activos()

   DISPLAY "Finaliza DISB028B....",vfolio
END MAIN


FUNCTION Actualiza_activos()
   DEFINE
      g_reg  RECORD 
                nss               CHAR(11),
                monto_retiro      DECIMAL(16,6),
                monto_cesantia    DECIMAL(16,6),
                monto_voluntaria  DECIMAL(16,6),
                monto_vivienda97  DECIMAL(16,6),
                monto_cuota_soc   DECIMAL(16,6),
                monto_sar         DECIMAL(16,6),
                monto_vivienda92  DECIMAL(16,6),
                fecha_rehabilita  DATE,
                marca_cod         SMALLINT,
                fecha_actualiza   DATE,
                estado            SMALLINT,
                usuario           CHAR(8),
                correlativo       INTEGER
             END RECORD

   LET inter.int_ret = 0
   LET inter.int_cv  = 0
   LET inter.int_vol = 0
   LET inter.int_viv = 0
   LET inter.int_soc = 0

   DECLARE cur_3 CURSOR FOR
   SELECT a.nss,
          a.subcuenta,
          a.fecha_conversion,
          act.marca_cod,
          act.correlativo,
          sum(a.monto_en_pesos),
          user
   FROM   dis_cuenta a,
          tab_marca tab,
          cta_act_marca act 
   WHERE  a.folio            = vfolio
   AND    a.nss              = act.nss
   AND    a.subcuenta       IN (1,2,3,5,6,9,11,17,15)
   AND    a.tipo_movimiento  = 3
   AND    act.marca_cod      = tab.marca_cod
   AND    tab.ind_habilita   = 2
   AND    a.fecha_conversion > act.fecha_ini
   AND    a.nss IN (SELECT nss FROM cta_act_marca
                    WHERE  marca_cod = 140)
   GROUP BY 1,2,3,4,5
   ORDER BY 1

   FOREACH cur_3 INTO reg_dis_cuenta.*
      CASE reg_dis_cuenta.subcuenta
         WHEN 1
            LET inter.int_ret     = reg_dis_cuenta.monto
            EXIT CASE
         WHEN 2
            LET inter.int_cv      = reg_dis_cuenta.monto
            EXIT CASE
         WHEN 3
            LET inter.int_vol     = reg_dis_cuenta.monto
            EXIT CASE
         WHEN 5
            LET inter.int_soc     = reg_dis_cuenta.monto
            EXIT CASE
         OTHERWISE
            EXIT CASE
      END CASE

      SELECT "X"
      FROM   safre_tmp:tmp_acumula_nss
      WHERE  nss = reg_dis_cuenta.nss

      IF STATUS = NOTFOUND THEN
         INSERT INTO safre_tmp:tmp_acumula_nss VALUES
         (
          reg_dis_cuenta.nss,
          inter.int_ret,
          inter.int_cv,
          inter.int_vol,
          inter.int_viv,
          inter.int_soc,
          0, 
          0,
          reg_dis_cuenta.fecha_conver,
          reg_dis_cuenta.marca_cod,
          TODAY,
          0,
          reg_dis_cuenta.usuario,
          reg_dis_cuenta.correlativo
         ) 

         LET inter.int_ret = 0
         LET inter.int_cv  = 0
         LET inter.int_vol = 0
         LET inter.int_soc = 0
      ELSE
         CASE reg_dis_cuenta.subcuenta
            WHEN 1
               UPDATE safre_tmp:tmp_acumula_nss
               SET   monto_retiro = inter.int_ret
               WHERE nss = reg_dis_cuenta.nss

               EXIT CASE
            WHEN 2
               UPDATE safre_tmp:tmp_acumula_nss
               SET   monto_cesantia = inter.int_cv
               WHERE nss = reg_dis_cuenta.nss 

               EXIT CASE
            WHEN 3
               UPDATE safre_tmp:tmp_acumula_nss
               SET   monto_voluntaria = inter.int_vol
               WHERE nss = reg_dis_cuenta.nss 
 
               EXIT CASE
            WHEN 5
               UPDATE safre_tmp:tmp_acumula_nss
               SET   monto_cuota_social = inter.int_soc
               WHERE nss = reg_dis_cuenta.nss 

               EXIT CASE
            OTHERWISE

               EXIT CASE
         END CASE
      END IF 
   END FOREACH

   --------------------------------------------------------------------------
   DISPLAY "Insertando en cta_rehabilitada..."

   ---INSERTANDO LA INFORMACION 
   DECLARE c_3 CURSOR FOR
   SELECT * from safre_tmp:tmp_acumula_nss

   FOREACH c_3 INTO g_reg.* 
      INSERT INTO cta_rehabilitada VALUES
      (
       vfolio,
       g_reg.nss,
       g_reg.monto_retiro,
       g_reg.monto_cesantia,
       g_reg.monto_voluntaria,
       g_reg.monto_vivienda97,
       g_reg.monto_cuota_soc,
       g_reg.monto_sar,
       g_reg.monto_vivienda92,
       g_reg.fecha_rehabilita,
       g_reg.marca_cod,
       g_reg.fecha_actualiza,
       g_reg.estado,
       g_reg.usuario
      )
 
      INSERT INTO cta_his_inhabilitada
      SELECT act.*
      FROM   cta_act_marca act,
             tab_marca     tab
      WHERE  act.nss          = g_reg.nss
      AND    act.marca_cod    = tab.marca_cod
      AND    act.correlativo  = g_reg.correlativo
      AND    tab.ind_habilita = 2

      PREPARE spl_exe FROM
      "EXECUTE PROCEDURE desmarca_cuenta(?,?,?,?,?,?) "
      EXECUTE spl_exe USING g_reg.nss,
                            g_reg.marca_cod,
                            g_reg.correlativo,
                            g_reg.estado,
                            '101',      --g_reg.marca_causa
                            g_reg.usuario
  
   END FOREACH
END FUNCTION


FUNCTION crea_tabla()
   DATABASE safre_tmp

   SELECT "x" FROM systables
   WHERE tabname = 'tmp_acumula_nss'

   IF STATUS = NOTFOUND THEN
      CREATE TABLE tmp_acumula_nss
      (
       nss               CHAR(11),
       monto_retiro      DECIMAL(16,6),
       monto_cesantia    DECIMAL(16,6),
       monto_voluntaria  DECIMAL(16,6),
       monto_vivienda97  DECIMAL(16,6),
       monto_cuota_soc   DECIMAL(16,6),
       monto_sar         DECIMAL(16,6),
       monto_vivienda92  DECIMAL(16,6),
       fecha_rehabilita  DATE,
       marca_cod         SMALLINT,
       fecha_actualiza   DATE,
       estado            SMALLINT,
       usuario           CHAR(8),
       correlativo       INTEGER
      )

      CREATE INDEX idx_tmp_acu ON tmp_acumula_nss(nss)
   ELSE
      DELETE FROM tmp_acumula_nss
   END IF

   DATABASE safre_af 
END FUNCTION

