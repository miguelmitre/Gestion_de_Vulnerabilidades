#*********************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                    #
#Propietario       => E.F.P.                                          #
#Programa          => INTB01011 y INTB0149                            #
#Descripcion       => GLOBALS y FUNCION DETALLE_NOTIFICA, CZA_NOTIFICA#
#Sistema           => INT .                                           #
#Fecha             => 30 de Noviembre del 2005                        #
#Por               => Laura Eugenia Cortes Guzman                     #
#Fecha             => 30 de Noviembre del 2005                        #
#Por               => Laura Eugenia Cortes Guzman                     #
#*********************************************************************#
DATABASE safre_af
GLOBALS
   
    DEFINE v_det                       , 
           v_cza                       ,
           v_det2               CHAR(70)

    DEFINE hoy                      DATE      ,
           folio                    INTEGER   ,
           fecha_proceso            DATE      ,
           fecha_creacion           DATE      ,
           hora                     CHAR(0008), 
           usuario                  CHAR(0008),
           g_fun_des                CHAR(1000),
           g_fun_sdo_ant_sep        CHAR(1000),
           g_fun_sdo_sep            CHAR(1000),
           enter                    CHAR(0001),
           leyenda_cza              CHAR(0120),
           ref                      CHAR(0002),
           fecha                    CHAR(0010),
           selec_tab1               CHAR(2000),
           ban                      SMALLINT  ,
           total_registros                    ,
           total_registros_02                 ,
           total_registros_03_04_05     INTEGER 

DEFINE     reg_1      RECORD LIKE   sep_det_reg_sol_reclamante.*

END GLOBALS

#########################################################################
# ---------------------------------------------------------------------
# Funcion del detalle
# ---------------------------------------------------------------------
#

FUNCTION detalle_notif_442(lref,prog,det_cza)
#dn4----------------------------------------

   DEFINE  prog                     SMALLINT  ,
           det_cza                  SMALLINT  ,
           car_gen                  SMALLINT  ,
           lref                     CHAR(02)  ,
           l_num_reg                INTEGER

   LET ref                      = lref
   LET l_num_reg                = 0
   LET total_registros          = 1
   LET total_registros_02       = 0
   LET total_registros_03_04_05 = 0

   START REPORT r_report TO v_det2

   LET selec_tab1 = " SELECT b.* ",
                    " FROM  sep_det_reg_sol_reclamante b ",
                    " WHERE b.fecha_proceso = ?          ",
                    " AND   b.estado        = 8          "

   PREPARE apt_42 FROM selec_tab1  
   DECLARE cur_42 CURSOR FOR apt_42
   FOREACH cur_42 USING fecha_proceso
                  INTO reg_1.*

     CALL genera_det02(reg_1.n_seguro       , #separado
                       reg_1.n_seguro       , #separador
                       reg_1.folio          , #folio separacion 
                       reg_1.fecha_proceso  ) #fecha separacion

     CALL genera_det02(reg_1.nss            , #separador
                       reg_1.n_seguro       , #separado
                       reg_1.folio          , #folio separacion  
                       reg_1.fecha_proceso  ) #fecha separacion

   DISPLAY "Total registros archivo: ",total_registros USING "###,###&" 
   AT 17,1
   END FOREACH
   FINISH REPORT r_report
END FUNCTION


##########################################################################
#---------------------------------------------------------
# Generando la cabeza
#---------------------------------------------------------
#
FUNCTION cabeza_notif_442(lref,leyenda_cza,prog,det_cza)

   DEFINE  cve_afore   SMALLINT     ,
           lref        CHAR(02)     ,
           leyenda_cza CHAR(0120)   ,
           v_rep       CHAR(2200)   ,
           no_carta    CHAR(0005)   ,
           prog        SMALLINT     ,
           det_cza     SMALLINT     ,
           consec_lote SMALLINT     

   LET ref = lref

   INITIALIZE fecha_creacion TO NULL

   LET consec_lote  = 0
   LET cve_afore    = 0

   START REPORT r_report TO v_cza

   SELECT a.codigo_afore 
   INTO cve_afore 
   FROM tab_afore_local a

   SELECT MAX(a.folio) 
   INTO consec_lote 
   FROM int_notifica_folio a

   IF (consec_lote IS NULL OR consec_lote = 0) THEN
      LET consec_lote = 1
   ELSE
      LET consec_lote = consec_lote + 1
   END IF

   LET no_carta = "302",ref

   LET v_rep = "01"                         ,"|", #1
               no_carta                     ,"|", #2
               leyenda_cza                  ,"|", #3
               cve_afore                    ,"|", #4
               TODAY USING"dd/mm/yyyy"      ,"|", #5
               consec_lote                  ,"|", #6
               total_registros              ,"|", #7
               total_registros_02           ,"|", #8
               total_registros_03_04_05     ,"|"  #9

   LET prog = 442

   OUTPUT TO REPORT r_report(v_rep,prog,1)

   LET fecha_creacion = TODAY

   INSERT INTO int_notifica_folio 
   VALUES (consec_lote,fecha_creacion,ref,total_registros)

   FINISH REPORT r_report
END FUNCTION


#########################################################################
FUNCTION genera_det02(nss_detalle               ,
                      nss_anterior              ,
                      folio_separacion          ,
                      fecha_proceso_separacion  )

DEFINE     reg_03     RECORD LIKE dis_cuenta.*

DEFINE     reg_04     ARRAY[8] OF RECORD 
           cargo      DEC(16,2),
           abono      DEC(16,2)
END RECORD

DEFINE     cargo      DEC(12,2),
           abono      DEC(12,2)

DEFINE     nss_detalle                   CHAR(11)      ,
           nss_anterior                  CHAR(11)      ,
           folio_separacion              INTEGER       ,
           fecha_proceso_separacion      DATE          ,

           no_carta                 CHAR(05)     ,
           fecha_creacion           CHAR(10)     ,
           concepto                 CHAR(50)     ,

           long, i                               ,
           consec_lote                    INTEGER,

           p_mae_afi  RECORD
                  nss               CHAR(11)     ,
                  rfc               CHAR(13)     ,
                  curp              CHAR(18)     ,
                  paterno           CHAR(40)     ,
                  materno           CHAR(40)     ,
                  nombres           CHAR(40)     ,
                  n_folio           DECIMAL(10,0),
                  tipo_solicitud    SMALLINT
           END RECORD,

           p_telefono               CHAR(20)   ,

           p_dom      RECORD
                  calle             CHAR(40),
                  numero            CHAR(10),
                  depto             CHAR(10),
                  colonia           CHAR(60),
                  codigo_postal     CHAR(5),
                  municip_deleg     INTEGER,
                  estado            SMALLINT,
                  poblacion         SMALLINT
           END RECORD,

           l_cero                   SMALLINT,
           car_gen                  SMALLINT,
           prog                     SMALLINT,
           f_subcuenta              SMALLINT,
           f_siefore                SMALLINT,
           f_monto_acc              DECIMAL(16,6),
           f_monto_pesos            DECIMAL(16,6),

           s_rcv_ant                DECIMAL(11,2),
           s_rcv_des                DECIMAL(11,2),
           s_rcv_mov                DECIMAL(11,2),

           s_ret92_ant              DECIMAL(11,2),
           s_ret92_des              DECIMAL(11,2),
           s_ret92_mov              DECIMAL(11,2),

           s_ahorr_ant              DECIMAL(11,2),
           s_ahorr_des              DECIMAL(11,2),
           s_ahorr_mov              DECIMAL(11,2),

           s_apcom_ant              DECIMAL(11,2),
           s_apcom_des              DECIMAL(11,2),
           s_apcom_mov              DECIMAL(11,2),

           s_apvol_ant              DECIMAL(11,2),
           s_apvol_des              DECIMAL(11,2),
           s_apvol_mov              DECIMAL(11,2),

           s_viv97_ant              DECIMAL(11,2),
           s_viv97_des              DECIMAL(11,2),
           s_viv97_mov              DECIMAL(11,2),

           s_viv92_ant              DECIMAL(11,2),
           s_viv92_des              DECIMAL(11,2),
           s_viv92_mov              DECIMAL(11,2),
           s_fovis_ant              DECIMAL(11,2),
           s_fovis_des              DECIMAL(11,2),
           s_fovis_mov              DECIMAL(11,2),
           s_tot_ant1               DECIMAL(11,2),
           s_tot_des1               DECIMAL(11,2),
           s_tot_mov1               DECIMAL(11,2),
           s_tot_viv_ant1           DECIMAL(11,2),
           s_tot_viv_des1           DECIMAL(11,2),
           s_tot_viv_mov1           DECIMAL(11,2),
           domi                     DECIMAL(10,0),
           sie_rcv                  CHAR(08),
           sie_ret92                CHAR(08),
           sie_ahorro               CHAR(08),
           sie_apcom                CHAR(08),
           sie_apvol                CHAR(08),
           calle                    CHAR(40),
           colonia                  CHAR(60),
           depto                    CHAR(10),
           desc_delega              CHAR(40),
           estado                   CHAR(40),
           poblacion                CHAR(40),
           centro_reparto           CHAR(05),
           entidad                  CHAR(04),
           numero                   CHAR(10),
           sexo                     CHAR(01),
           desc_entidad             CHAR(30),
           fecha_barra              CHAR(08),
           telefono                 CHAR(15),
           n_folio                  CHAR(10),
           paterno                  CHAR(40),
           materno                  CHAR(40),
           nombres                  CHAR(40),
           curp                     CHAR(18),
           rfc                      CHAR(13),
           v_rep                    CHAR(2200),
           consec_3                 INTEGER,
           indicador                INTEGER ,
           cargo_rcv                DEC(16,2),
           abono_rcv                DEC(16,2),
           cargo_viv                DEC(16,2),
           abono_viv                DEC(16,2)

   INITIALIZE p_dom.*, 
              p_telefono TO NULL

   INITIALIZE fecha_creacion    ,
              calle             ,
              depto             ,
              colonia           ,
              telefono          TO NULL

   INITIALIZE desc_delega     ,
              entidad         ,
              poblacion       ,
              centro_reparto  ,
              folio           TO NULL

   INITIALIZE p_mae_afi.*     ,
              paterno         ,
              materno         ,
              nombres         TO NULL

   LET l_cero     = 0
   LET i          = 0   
   LET domi       = 0   
   LET long       = 0

            SELECT a.n_seguro       ,
                   a.n_rfc          ,
                   a.n_unico        ,
                   a.paterno        ,
                   a.materno        ,
                   a.nombres        ,
                   a.n_folio        ,
                   a.tipo_solicitud
            INTO   p_mae_afi.*
            FROM  afi_mae_afiliado a
            WHERE a.n_seguro       = nss_detalle


         IF p_mae_afi.curp IS NULL OR
            p_mae_afi.curp = " "  THEN
            LET curp       = "                  "
         ELSE
             LET long = 0
             LET i    = 0
             LET curp = p_mae_afi.curp CLIPPED
             LET long = LENGTH(curp)
             IF long < 18 THEN
                LET long = long + 1
                FOR i = long TO 18
                    LET curp[i,i] = " "
                END FOR
             END IF
         END IF

         IF p_mae_afi.rfc IS NULL OR
            p_mae_afi.rfc = " "  THEN
            LET rfc = "                  "
         ELSE
             LET long = 0
             LET i    = 0
             LET rfc = p_mae_afi.rfc CLIPPED
             LET long = LENGTH(rfc)
             IF long < 13 THEN
                LET long = long + 1
                FOR i = long TO 13
                    LET rfc[i,i] = " "
                END FOR
             END IF
         END IF

         LET paterno  = p_mae_afi.paterno
         LET materno  = p_mae_afi.materno
         LET nombres  = p_mae_afi.nombres

         LET long = 0    LET i   = 0
         LET long = LENGTH(paterno)
         IF long < 40 THEN
            LET long = long + 1
            FOR i = long TO 40
                LET paterno[i,i] = " "
            END FOR
         END IF

         LET long = 0    LET i   = 0
         LET long = LENGTH(materno)
         IF long < 40 THEN
            LET long = long + 1
            FOR i = long TO 40
                LET materno[i,i] = " "
            END FOR
         END IF

         LET long = 0    LET i   = 0
         LET long = LENGTH(nombres)
         IF long < 40 THEN
            LET long = long + 1
            FOR i = long TO 30
                LET nombres[i,i] = " "
            END FOR
         END IF

         IF p_mae_afi.n_folio = 0 THEN
               LET n_folio = "0000000000"
         ELSE
               LET n_folio = p_mae_afi.n_folio USING "&&&&&&&&&&"
         END IF

##telefono

         DECLARE apt_tel CURSOR FOR
             SELECT n.telefono 
               FROM afi_telefono n
              WHERE n.nss            = p_mae_afi.nss
                AND n.n_folio        = p_mae_afi.n_folio
                AND n.tipo_solicitud = p_mae_afi.tipo_solicitud
                AND n.telefono IS NOT NULL
                AND n.tel_cod  < 7

         FOREACH apt_tel INTO p_telefono
                 LET telefono = p_telefono CLIPPED
                 EXIT FOREACH
         END FOREACH

         LET long = 0
         LET i    = 0
         LET long = LENGTH(telefono)
         IF long < 15 THEN
            LET long = long + 1
            FOR i = long TO 15
                LET telefono[i,i] = " "
            END FOR
         END IF 

##domicilio

         SELECT MIN(s.ROWID)
                INTO  domi
                FROM  afi_domicilio s
                WHERE s.nss            = p_mae_afi.nss
                AND   s.n_folio        = p_mae_afi.n_folio
                AND   s.tipo_solicitud = p_mae_afi.tipo_solicitud

         IF SQLCA.SQLCODE = 0 THEN
            SELECT o.calle, o.numero, o.depto, o.colonia, o.codpos,
                   o.delega, o.estado, o.ciudad
                   INTO  p_dom.*
                   FROM  afi_domicilio o
                   WHERE o.rowid = domi

            IF p_dom.calle IS NULL OR 
               p_dom.calle =  "  " THEN
               LET calle = "                                        "
            ELSE
                LET long = 0
                LET i    = 0
                LET calle    = p_dom.calle
                LET long = LENGTH(calle)
                IF long < 40 THEN
                   LET long = long + 1
                   FOR i = long TO 40
                       LET calle[i,i] = " "
                   END FOR 
                END IF
            END IF

            IF p_dom.numero IS NULL OR 
               p_dom.numero =  "  " THEN
               LET numero = "          "
            ELSE
                LET long = 0
                LET i    = 0
                LET numero    = p_dom.numero
                LET long = LENGTH(numero)
                IF long < 10 THEN
                   LET long = long + 1
                   FOR i = long TO 10
                       LET numero[i,i] = " "
                   END FOR 
                END IF
            END IF

            IF p_dom.depto IS NULL OR 
               p_dom.depto =  "  " THEN
               LET p_dom.depto = "          "
            ELSE
                LET long = 0
                LET i    = 0
                LET depto    = p_dom.depto
                LET long = LENGTH(depto)
                IF long < 10 THEN
                   LET long = long + 1
                   FOR i = long TO 10
                       LET depto[i,i] = " "
                   END FOR 
                END IF
            END IF

            IF p_dom.colonia IS NULL OR
               p_dom.colonia = " "   THEN
               LET colonia = "                                        ",
                             "                    "
            ELSE
                LET long = 0
                LET i    = 0
                LET colonia = p_dom.colonia
                LET long = LENGTH(colonia)
                IF long < 60 THEN
                   LET long = long + 1
                   FOR i = long TO 60
                       LET colonia[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.codigo_postal IS NULL OR
               p_dom.codigo_postal = "  "  THEN
               LET p_dom.codigo_postal = "     "
               LET centro_reparto = "     "
            ELSE
                 SELECT am.centro_reparto INTO centro_reparto 
                        FROM  tab_reparto am
                        WHERE am.codigo_postal = p_dom.codigo_postal
                 IF centro_reparto IS NULL OR
                    centro_reparto = " "   THEN
                    LET centro_reparto = "     "
                 ELSE
                      LET long = 0   LET i = 0
                      LET long = LENGTH(centro_reparto)
                      IF long < 5 THEN
                         LET long = long + 1
                         FOR i = long TO 5
                             LET centro_reparto[i,i] = " "
                         END FOR
                      END IF
                 END IF
            END IF

            IF p_dom.estado IS NULL OR
               p_dom.estado =  " "  THEN
               LET estado = "                                        "
            ELSE
                SELECT p.estad_desc INTO estado
                       FROM tab_estado p
                       WHERE p.estad_cod = p_dom.estado
                 IF SQLCA.SQLCODE != 0 THEN
                    LET estado = "                                        "
                 ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(estado)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET estado[i,i] = " "
                         END FOR
                      END IF
                 END IF
            END IF

            IF p_dom.municip_deleg IS NULL OR
               p_dom.municip_deleg =  " "  THEN
               LET desc_delega = "                                        "
            ELSE
                SELECT q.deleg_desc INTO desc_delega
                       FROM tab_delegacion q
                       WHERE q.deleg_cod = p_dom.municip_deleg
                 IF SQLCA.SQLCODE != 0 THEN
                    LET desc_delega = "                                        "
                 ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(desc_delega)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET desc_delega[i,i] = " "
                         END FOR
                      END IF
                 END IF
            END IF

            IF p_dom.poblacion IS NULL OR
               p_dom.poblacion =  " "  THEN
               LET poblacion = "                                        "
            ELSE
                SELECT q.ciudad_desc INTO poblacion
                       FROM  tab_ciudad q
                       WHERE q.ciudad_cod = p_dom.poblacion
                IF SQLCA.SQLCODE != 0 THEN
                    LET poblacion = "                                        "
                ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(poblacion)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET poblacion[i,i] = " "
                         END FOR
                      END IF
                END IF
            END IF
         END IF


##fecha_carta

##entidad
         LET entidad = "    "
##fecha generacion de la carta
         LET fecha_creacion = TODAY USING "dd/mm/yyyy"
         LET fecha_barra    = TODAY USING "yyyymmdd"
##no_carta
         LET no_carta = "302",ref
##saldo despues

   DELETE FROM int_sal_des

   LET f_subcuenta   = 0
   LET f_siefore     = 0
   LET f_monto_acc   = 0
   LET f_monto_pesos = 0

   LET g_fun_des  = 'EXECUTE PROCEDURE fn_saldo_dia(?,?,?,?) '
   PREPARE  spl_sdo_dia   FROM  g_fun_des
   DECLARE  c_sdo_dia   CURSOR  FOR  spl_sdo_dia
   FOREACH  c_sdo_dia USING p_mae_afi.nss,
                            l_cero,
                            l_cero,
                            fecha_proceso
            INTO f_subcuenta, f_siefore, f_monto_acc, f_monto_pesos

                INSERT INTO int_sal_des
                       VALUES (p_mae_afi.nss,
                               f_subcuenta,
                               f_siefore,
                               f_monto_acc,
                               f_monto_pesos)

             LET f_subcuenta   = 0
             LET f_siefore     = 0
             LET f_monto_acc   = 0
             LET f_monto_pesos = 0

   END FOREACH
   LET s_rcv_des   = 0
   LET s_ret92_des = 0
   LET s_ahorr_des = 0
   LET s_apcom_des = 0
   LET s_apvol_des = 0
   LET s_viv97_des = 0
   LET s_viv92_des = 0
   LET s_fovis_des = 0

   CALL saldo("int_sal_des",p_mae_afi.nss) # nombre de tabla y nss detalle
   RETURNING s_rcv_des   ,
             s_ret92_des   ,
             s_ahorr_des   ,
             s_apcom_des   ,
             s_apvol_des   ,
             s_viv97_des   ,
             s_viv92_des   ,
             s_fovis_des

   LET s_tot_des1     = 0
   LET s_tot_des1     = s_rcv_des   +
                        s_ret92_des +  # saldo total rcv 
                        s_ahorr_des +  # valuado al dia
                        s_apcom_des +
                        s_apvol_des

   LET s_tot_viv_des1 = 0   
   LET s_tot_viv_des1 = s_viv97_des +  # saldo total viv
                        s_viv92_des +  # valuado al dia
                        s_fovis_des

##SIEFORE despues separa_cta

   CALL trae_siefore_subcuenta(p_mae_afi.nss,1) # nss y rcv
   RETURNING sie_rcv

   CALL trae_siefore_subcuenta(p_mae_afi.nss,7)
   RETURNING sie_ret92

   CALL trae_siefore_subcuenta(p_mae_afi.nss,13)
   RETURNING sie_ahorro

   CALL trae_siefore_subcuenta(p_mae_afi.nss,11)
   RETURNING sie_apcom

   CALL trae_siefore_subcuenta(p_mae_afi.nss,3)
   RETURNING sie_apvol

##saldo antes

   DELETE FROM int_sal_ant

   LET f_subcuenta   = 0
   LET f_siefore     = 0
   LET f_monto_acc   = 0
   LET f_monto_pesos = 0

   LET g_fun_sdo_ant_sep = 'EXECUTE PROCEDURE fn_saldo_dia_ant_sep(?,?,?,?,?) '
   PREPARE  spl_sdo_ant_sep   FROM  g_fun_sdo_ant_sep
   DECLARE  c_sdo_ant_sep  CURSOR  FOR  spl_sdo_ant_sep
   FOREACH  c_sdo_ant_sep USING p_mae_afi.nss   , #nss detalle
                                l_cero          , #grupo 0
                                l_cero          , #scta  0
                                fecha_proceso   , #fecha saldo
                                folio_separacion
                           
                      INTO f_subcuenta      ,
                           f_siefore        ,
                           f_monto_acc      ,
                           f_monto_pesos

                INSERT INTO int_sal_ant
                       VALUES (p_mae_afi.nss ,
                               f_subcuenta   ,
                               f_siefore     ,
                               f_monto_acc   ,
                               f_monto_pesos)

             LET f_subcuenta   = 0
             LET f_siefore     = 0
             LET f_monto_acc   = 0
             LET f_monto_pesos = 0
   END FOREACH

   LET s_rcv_ant   = 0
   LET s_ret92_ant = 0
   LET s_ahorr_ant = 0
   LET s_apcom_ant = 0
   LET s_apvol_ant = 0
   LET s_viv97_ant = 0
   LET s_viv92_ant = 0
   LET s_fovis_ant = 0

   CALL saldo("int_sal_ant",p_mae_afi.nss) # nombre de tabla y nss detalle
   RETURNING s_rcv_ant   ,
             s_ret92_ant ,
             s_ahorr_ant ,
             s_apcom_ant ,
             s_apvol_ant ,
             s_viv97_ant ,
             s_viv92_ant ,
             s_fovis_ant

   LET s_tot_ant1     = 0
   LET s_tot_ant1     = s_rcv_ant   +
                        s_ret92_ant +  # saldo total rcv 
                        s_ahorr_ant +  # valuado al dia anterior
                        s_apcom_ant +
                        s_apvol_ant

   LET s_tot_viv_ant1 = 0
   LET s_tot_viv_ant1 = s_viv97_ant +  # saldo total viv
                        s_viv92_ant +  # valuado al dia anterior
                        s_fovis_ant


#saldo movimiento separacion

   DELETE FROM int_sal_mov

   LET f_subcuenta   = 0
   LET f_siefore     = 0
   LET f_monto_acc   = 0
   LET f_monto_pesos = 0

   LET g_fun_sdo_sep = 'EXECUTE PROCEDURE fn_saldo_dia_sep(?,?,?,?,?) '
   PREPARE  spl_sdo_sep   FROM  g_fun_sdo_sep
   DECLARE  c_sdo_sep  CURSOR  FOR  spl_sdo_sep
   FOREACH  c_sdo_sep USING p_mae_afi.nss  , #nss detalle
                            l_cero         , #grupo 0
                            l_cero         , #scta  0
                            fecha_proceso  , #fecha saldo
                            folio_separacion
                       INTO f_subcuenta    ,
                            f_siefore      ,
                            f_monto_acc    ,
                            f_monto_pesos

                INSERT INTO int_sal_mov
                       VALUES (p_mae_afi.nss ,
                               f_subcuenta   ,
                               f_siefore     ,
                               f_monto_acc   ,
                               f_monto_pesos)

             LET f_subcuenta   = 0
             LET f_siefore     = 0
             LET f_monto_acc   = 0
             LET f_monto_pesos = 0
   END FOREACH

   LET s_rcv_mov   = 0
   LET s_ret92_mov = 0
   LET s_ahorr_mov = 0
   LET s_apcom_mov = 0
   LET s_apvol_mov = 0
   LET s_viv97_mov = 0
   LET s_viv92_mov = 0
   LET s_fovis_mov = 0

   CALL saldo("int_sal_mov",p_mae_afi.nss) # nombre de tabla y nss detalle
   RETURNING s_rcv_mov   ,
             s_ret92_mov ,
             s_ahorr_mov ,
             s_apcom_mov ,
             s_apvol_mov ,
             s_viv97_mov ,
             s_viv92_mov ,
             s_fovis_mov

   LET s_tot_mov1     = 0
   LET s_tot_mov1     = s_rcv_mov   +
                        s_ret92_mov +  # saldo total rcv 
                        s_ahorr_mov +  # valuado al dia anterior
                        s_apcom_mov +
                        s_apvol_mov

   LET s_tot_viv_mov1 = 0
   LET s_tot_viv_mov1 = s_viv97_mov +  # saldo total viv
                        s_viv92_mov +  # valuado al dia anterior
                        s_fovis_mov

## 
   LET car_gen = 1
         LET v_rep  =  "02"                                , "|",  #01
                       p_mae_afi.nss                       , "|",  #02
                       no_carta                            , "|",  #03
                       nombres                             , "|",  #04
                       paterno                             , "|",  #05
                       materno                             , "|",  #06
                       calle                               , "|",  #07
                       p_dom.numero                        , "|",  #08
                       depto                               , "|",  #09
                       colonia                             , "|",  #10
                       desc_delega                         , "|",  #11
                       poblacion                           , "|",  #12
                       estado                              , "|",  #13
                       p_dom.codigo_postal                 , "|",  #14
                       centro_reparto                      , "|",  #15
                       telefono                            , "|",  #16
                       fecha_creacion                      , "|",  #17
                       n_folio                             , "|",  #18
                       curp                                , "|",  #19
                       rfc                                 , "|",  #20
                       nss_anterior                        , "|",  #21
                       fecha_proceso USING "dd/mm/yyyy"    , "|",  #22
                       s_rcv_ant                           , "|",  #23
                       s_rcv_mov                           , "|",  #24
                       s_rcv_des                           , "|",  #25
	           	        sie_rcv                             , "|",  #26
		                 s_ret92_ant                         , "|",  #27
		                 s_ret92_mov                         , "|",  #28
		                 s_ret92_des                         , "|",  #29
		                 sie_ret92                           , "|",  #30
		                 s_ahorr_ant                         , "|",  #31
		                 s_ahorr_mov                         , "|",  #32
		                 s_ahorr_des                         , "|",  #33
		                 sie_ahorro                          , "|",  #34
		                 s_apcom_ant                         , "|",  #35
		                 s_apcom_mov                         , "|",  #36
		                 s_apcom_des                         , "|",  #37
		                 sie_apcom                           , "|",  #38
		                 s_apvol_ant                         , "|",  #39
		                 s_apvol_mov                         , "|",  #40
		                 s_apvol_des                         , "|",  #41
		                 sie_apvol                           , "|",  #42
		                 s_tot_ant1                          , "|",  #43
		                 s_tot_mov1                          , "|",  #44
		                 s_tot_des1                          , "|",  #45
		                 s_viv97_ant                         , "|",  #46
		                 s_viv97_mov                         , "|",  #47
		                 s_viv97_des                         , "|",  #48
		                 s_viv92_ant                         , "|",  #49
		                 s_viv92_mov                         , "|",  #50
		                 s_viv92_des                         , "|",  #51
		                 s_fovis_ant                         , "|",  #52
		                 s_fovis_mov                         , "|",  #53
		                 s_fovis_des                         , "|",  #54
		                 s_tot_viv_ant1                      , "|",  #55
		                 s_tot_viv_mov1                      , "|",  #56
		                 s_tot_viv_des1                      , "|",  #57
                       no_carta                            , "|",  #58
                       p_mae_afi.nss                       , "|",  #59
                       fecha_barra                         , "|",  #60
                       n_folio                             , "|",  #61
                       car_gen USING "&"                   , "|",  #62
                       curp                                , "|",  #63
                       "000"                               , "|"   #64

     LET prog = 442

     LET total_registros    = total_registros    + 1
     LET total_registros_02 = total_registros_02 + 1
     OUTPUT TO REPORT r_report(v_rep,prog,2)


#detalle 3

     FOR i = 1 TO 8 
       LET reg_04[i].cargo = 0
       LET reg_04[i].abono = 0
     END FOR

     LET consec_3 = 0

     DECLARE cur_03 CURSOR FOR

        SELECT b.* 
        FROM   dis_cuenta b
        WHERE  b.nss              = p_mae_afi.nss
        AND    b.folio            = folio_separacion
        AND    b.fecha_conversion = fecha_proceso_separacion
        AND    b.id_aportante not in ('AJUSTE','SALDO','CORTE')
        ORDER BY b.subcuenta,b.fecha_valor,b.monto_en_pesos
        
     FOREACH cur_03 INTO reg_03.*
         
         INITIALIZE concepto TO NULL

         SELECT a.descripcion 
         INTO   concepto 
         FROM   tab_movimiento a
         WHERE  a.codigo = reg_03.tipo_movimiento

         IF STATUS = NOTFOUND THEN
            LET concepto = "                                                  "
         END IF

         LET consec_3 = consec_3 + 1

         CALL clasifica_subcuenta(reg_03.subcuenta)
         RETURNING indicador

         IF reg_03.monto_en_pesos > 0 THEN
            LET abono = 0
            LET abono = reg_03.monto_en_pesos
            LET cargo = 0
            LET reg_04[indicador].abono = reg_04[indicador].abono +
                                          abono
         ELSE
            LET cargo = 0
            LET cargo = reg_03.monto_en_pesos
            LET abono = 0
            LET reg_04[indicador].cargo = reg_04[indicador].cargo +
                                  cargo
         END IF
            
         LET v_rep = "03"                                  , "|",  #01
                     p_mae_afi.nss                         , "|",  #02
                     indicador                             , "|",  #03
                     reg_03.fecha_valor USING"dd/mm/yyyy"  , "|",  #04
                     concepto                              , "|",  #05
                     reg_03.id_aportante                   , "|",  #06
                     cargo                                 , "|",  #07
                     abono                                 , "|",  #08
                     consec_3                              , "|",  #09
                     " "                                   , "|"   #10

         LET total_registros          = total_registros          + 1
         LET total_registros_03_04_05 = total_registros_03_04_05 + 1
         OUTPUT TO REPORT r_report(v_rep,prog,3)
     END FOREACH


#detalle 4

     FOR i = 1 TO 8 
   
         LET consec_3 = consec_3 + 1

         LET v_rep = "04"                                  , "|",  #01
                     p_mae_afi.nss                         , "|",  #02
                     i                                     , "|",  #03
                     reg_04[i].cargo                       , "|",  #04
                     reg_04[i].abono                       , "|",  #05
                     consec_3                              , "|",  #06
                     " "                                   , "|"   #07

         LET total_registros          = total_registros          + 1
         LET total_registros_03_04_05 = total_registros_03_04_05 + 1
         OUTPUT TO REPORT r_report(v_rep,prog,4)

     END FOR


#detalle 5

     LET cargo_rcv = 0
     LET cargo_viv = 0
     LET abono_rcv = 0
     LET abono_viv = 0

     LET consec_3 = consec_3 + 1

     FOR i = 1 TO 8
       IF i < 6 THEN 
         LET cargo_rcv = cargo_rcv + reg_04[i].cargo 
         LET abono_rcv = abono_rcv + reg_04[i].abono
       ELSE 
         LET cargo_viv = cargo_viv + reg_04[i].cargo
         LET abono_viv = abono_viv + reg_04[i].abono
       END IF
     END FOR
     
         LET v_rep = "05"                                  , "|",  #01
                     p_mae_afi.nss                         , "|",  #02
                     "0"                                   , "|",  #03
                     cargo_rcv                             , "|",  #04
                     abono_rcv                             , "|",  #05
                     cargo_viv                             , "|",  #05
                     abono_viv                             , "|",  #05
                     consec_3                              , "|",  #06
                     " "                                   , "|"   #07

         LET total_registros          = total_registros          + 1
         LET total_registros_03_04_05 = total_registros_03_04_05 + 1
         OUTPUT TO REPORT r_report(v_rep,prog,5)

         INSERT INTO int_ctr_carta
         VALUES ( p_mae_afi.nss              ,
                  p_mae_afi.n_folio          ,
                  p_mae_afi.tipo_solicitud   ,
                  " "                        ,
                  30223                      ,
                  " "                        ,
                  20                         ,
                  TODAY                      ,
                  hora                       ,
                  consec_lote                ,
                  0                          ,
                  0)

     DISPLAY "Total Nss              : ",total_registros_02 
     USING "###,###&" AT 16,1

     INITIALIZE p_dom.*          ,
                p_telefono       TO NULL

     INITIALIZE fecha_creacion   ,
                calle            ,
                depto            ,
                colonia          ,
                telefono         TO NULL

     INITIALIZE desc_delega      ,
                entidad          ,
                poblacion        ,
                centro_reparto   ,
                folio            TO NULL

     INITIALIZE paterno          ,
                materno          ,
                nombres          TO NULL

     LET i        = 0   
     LET domi     = 0  
     LET long     = 0

END FUNCTION

FUNCTION trae_siefore_subcuenta(lt_nss,lt_scta)
#tss-------------------------------------------

DEFINE lt_nss      CHAR(011)
DEFINE lt_scta     SMALLINT
DEFINE lt_siefore  SMALLINT
DEFINE lt_tot_sie  SMALLINT

   SELECT COUNT(*) 
   INTO   lt_tot_sie 
   FROM   int_sal_des a 
   WHERE  a.nss_act      = lt_nss
   AND    a.subcuenta    = lt_scta

   IF lt_tot_sie = 1 THEN 
        SELECT a.siefore 
        INTO   lt_siefore
        FROM   int_sal_des a
        WHERE  a.nss_act      = lt_nss
        AND    a.subcuenta    = lt_scta
        GROUP BY 1
   END IF

   IF lt_tot_sie > 1 THEN 
       SELECT "OK" 
       FROM   cta_ctr_cuenta a 
       WHERE  a.nss = lt_nss
       AND    a.ind_transferencia = 8
       GROUP BY 1  
  
        IF STATUS <> NOTFOUND THEN 
           LET lt_siefore = 2 
        ELSE 
           SELECT a.codigo_siefore 
           INTO   lt_siefore 
           FROM   cta_regimen a
           WHERE  a.nss       = lt_nss
           AND    a.subcuenta = lt_scta
           GROUP BY 1
        END IF
   END IF

RETURN lt_siefore

END FUNCTION


FUNCTION saldo(tabla,ls_nss)
#s---------------------------
DEFINE ls_nss              CHAR(0011)
DEFINE tabla               CHAR(0050)
DEFINE txt_qry             CHAR(1000)

DEFINE ls_scta             SMALLINT,
       ls_sdo_scta         DECIMAL(16,6)

DEFINE l_rcv_6d            ,
       l_ret92_6d          ,
       l_ahorr_6d          ,
       l_apcom_6d          ,
       l_apvol_6d          ,
       l_viv97_6d          ,
       l_viv92_6d          ,
       l_fovis_6d          DECIMAL(16,6),

       l_rcv_2d           ,
       l_ret92_2d         ,
       l_ahorr_2d         ,
       l_apcom_2d         ,
       l_apvol_2d         ,
       l_viv97_2d         ,
       l_viv92_2d         ,
       l_fovis_2d         DECIMAL(16,2)


   LET l_rcv_6d   = 0
   LET l_ret92_6d = 0
   LET l_ahorr_6d = 0
   LET l_apcom_6d = 0
   LET l_apvol_6d = 0
   LET l_viv97_6d = 0
   LET l_viv92_6d = 0
   LET l_fovis_6d = 0

   LET txt_qry = 
   " SELECT a.subcuenta,SUM(a.mto_pesos) ",
   " FROM   ",tabla CLIPPED," a      ",
   " WHERE  a.nss_act    = ?        ",
   " GROUP BY 1 "

   LET txt_qry = txt_qry CLIPPED

   PREPARE qry_saldo FROM txt_qry
   DECLARE cur_saldo CURSOR FOR qry_saldo 

   FOREACH cur_saldo USING ls_nss 
                     INTO  ls_scta,ls_sdo_scta
   CASE ls_scta 
   WHEN 1
        LET l_rcv_6d = l_rcv_6d + ls_sdo_scta
        EXIT CASE
   WHEN 2
        LET l_rcv_6d = l_rcv_6d + ls_sdo_scta
        EXIT CASE
   WHEN 3
        LET l_apvol_6d = l_apvol_6d + ls_sdo_scta
        EXIT CASE
   WHEN 4
        LET l_viv97_6d = l_viv97_6d + ls_sdo_scta
        EXIT CASE
   WHEN 5
        LET l_rcv_6d = l_rcv_6d + ls_sdo_scta
        EXIT CASE
   WHEN 6
        LET l_rcv_6d = l_rcv_6d + ls_sdo_scta
        EXIT CASE
   WHEN 7
        LET l_ret92_6d = l_ret92_6d + ls_sdo_scta
        EXIT CASE
   WHEN 8
        LET l_viv92_6d = l_viv92_6d + ls_sdo_scta
        EXIT CASE
   WHEN 9
        LET l_rcv_6d = l_rcv_6d + ls_sdo_scta
        EXIT CASE
   WHEN 10
        LET l_apvol_6d = l_apvol_6d + ls_sdo_scta
        EXIT CASE
   WHEN 11
        LET l_apcom_6d = l_apcom_6d + ls_sdo_scta
        EXIT CASE
   WHEN 12
        LET l_apcom_6d = l_apcom_6d + ls_sdo_scta
        EXIT CASE
   WHEN 13
        LET l_ahorr_6d = l_ahorr_6d + ls_sdo_scta
        EXIT CASE
   WHEN 14
        LET l_fovis_6d = l_fovis_6d + ls_sdo_scta
        EXIT CASE
   END CASE
 END FOREACH               

   LET l_rcv_2d   = l_rcv_6d
   LET l_apvol_2d = l_apvol_6d   # convierte a 2 decimales
   LET l_viv97_2d = l_viv97_6d
   LET l_ret92_2d = l_ret92_6d
   LET l_viv92_2d = l_viv92_6d
   LET l_apcom_2d = l_apcom_6d
   LET l_ahorr_2d = l_ahorr_6d
   LET l_fovis_2d = l_fovis_6d

   RETURN l_rcv_2d      , 
          l_ret92_2d    ,
          l_ahorr_2d    ,
          l_apcom_2d    ,
          l_apvol_2d    ,
          l_viv97_2d    ,
          l_viv92_2d    ,
          l_fovis_2d    

END FUNCTION

FUNCTION clasifica_subcuenta(l_subct)
#cs--------------------------------------

DEFINE l_subct     ,
       l_indicador SMALLINT
       

CASE l_subct
WHEN 1 
   LET l_indicador = 1
   EXIT CASE
WHEN 2 
   LET l_indicador = 1
   EXIT CASE
WHEN 3 
   LET l_indicador = 5
   EXIT CASE
WHEN 4 
   LET l_indicador = 6
   EXIT CASE
WHEN 5 
   LET l_indicador = 1
   EXIT CASE
WHEN 6 
   LET l_indicador = 1
   EXIT CASE
WHEN 7 
   LET l_indicador = 2
   EXIT CASE
WHEN 8 
   LET l_indicador = 7
   EXIT CASE
WHEN 9 
   LET l_indicador = 1
   EXIT CASE
WHEN 10 
   LET l_indicador = 5
   EXIT CASE
WHEN 11 
   LET l_indicador = 4
   EXIT CASE
WHEN 12 
   LET l_indicador = 4
   EXIT CASE
WHEN 13 
   LET l_indicador = 3
   EXIT CASE
WHEN 14 
   LET l_indicador = 8
   EXIT CASE
END CASE

RETURN l_indicador

END FUNCTION
