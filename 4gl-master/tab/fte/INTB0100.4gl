#*********************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                    #
#Propietario       => E.F.P.                                          #
#Programa          => INTB0100                                        #
#Descripcion       => GLOBALS y FUNCION DETALLE_NOTIFICA, CZA_NOTIFICA#
#Sistema           => INT .                                           #
#Fecha Elaboracion => 26 de julio de 2001    .                        #
#Elaborado por     => Laura Eugenia Cortes Guzman                     #
#Fecha Ult. Modifi.=> 12 de Febrero del 2006 .                        #
#Modificado por    => Laura Eugenia Cortes Guzman                     #
#*********************************************************************#
DATABASE safre_af
GLOBALS
    DEFINE p_tabafore               RECORD LIKE tab_afore_local.*,
           hoy_hoy                  DATE,
           fentcons                 DATE,
           finicta                  DATE,
           fe_carta                 DATE,
           fecha_rec                DATE,
           fecha_elaboracion        DATE,
           fecha_no_local           DATE,
           fecha_gene_car           DATE,
           finitmte                 DATE,
           opera                    CHAR(2),
           pais_cod                 CHAR(3),
           pais_desc                CHAR(40),
           --fena                     DATE,
           fena                     CHAR(10),
           estadon                  SMALLINT,
           estadon_desc             CHAR(60),

           fecha_init               CHAR(10),
           hoy                      CHAR(06),
           n_seguro                 CHAR(11),
           enter, opc               CHAR(01),
           estatus                  CHAR(01),
           tipo                     CHAR(02),
           leyenda                  CHAR(200),
           leyenda_cza              CHAR(120),
           no_carta                 CHAR(05),
           desc_entidad             CHAR(30),
           fecha_creacion           CHAR(10),
           fecha_barra              CHAR(08),
           ref                      CHAR(02),
           tipo_proceso             CHAR(01),
           telefono                 CHAR(15),
           barras                   CHAR(28),
           folio                    CHAR(10),
           f_certifica              CHAR(10),
           f_asigna                 CHAR(10),
           paterno                  CHAR(40),
           materno                  CHAR(40),
           nombres                  CHAR(40),
           curp                     CHAR(18),
           rfc                      CHAR(13),
           motivo                   CHAR(03),
           motivo1                  CHAR(03),
           motivo2                  CHAR(03),
           moti_vo                  CHAR(03),
           rechazo                  CHAR(120),
           rechazo1                 CHAR(120),
           rechazo2                 CHAR(120),
           inter_noti               CHAR(300),
           selec_tab                CHAR(2000),
           selec_tab_1              CHAR(2000),

           long, i                  INTEGER,
           ban                      INTEGER,

           no_motivo                SMALLINT,
           cod_mot                  SMALLINT,


           clave_afore              SMALLINT,
           codigo_cert              SMALLINT,

           consec_lote              INTEGER,

           numero_reg               DECIMAL(10,0),
           --precio                   DECIMAL(11,6),
           precio                   DECIMAL(19,14),
           consecutivo_envio        DECIMAL(8,0),

           calle                    CHAR(40),
           colonia                  CHAR(60),
           depto                    CHAR(10),
           numero                   CHAR(10),
           desc_delega              CHAR(40),
           estado                   CHAR(40),
           poblacion                CHAR(40),
           centro_reparto           CHAR(05),
           entidad                  CHAR(04),
           sexo                     CHAR(01),
           domi                     DECIMAL(10,0),


           p_mae_afi  RECORD
                  nss               CHAR(11),
                  rfc               CHAR(13),
                  curp              CHAR(18),
                  paterno           CHAR(40),
                  materno           CHAR(40),
                  nombres           CHAR(40),
                  sexo              SMALLINT,
                  folio             DECIMAL(10,0),
                  fecha_certifica   DATE,
                  tipo_solicitud    SMALLINT,
                  fecha_elabora     DATE,
                  fena              DATE,
                  nacionalidad      CHAR(03),
                  finitmte          DATE,
                  finicta           DATE,
                  fbdnsar           DATE
           END RECORD,

           p_telefono      RECORD
                  telefono          CHAR(20)
           END RECORD,

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

{           asig   RECORD
                  retiro            DECIMAL(11,2),
                  cesantia          DECIMAL(11,2),
                  cuota             DECIMAL(11,2),
                  viv               DECIMAL(11,2),
                  apvol             DECIMAL(11,2),
                  rcv               DECIMAL(11,2)
           END RECORD,
}
           asig       RECORD
                rcv               DECIMAL(11,2),
                ret92             DECIMAL(11,2),
                ahorro            DECIMAL(11,2),
                apcom             DECIMAL(11,2),
                apvol             DECIMAL(11,2),
                s_inver           DECIMAL(13,2),
                viv97             DECIMAL(11,2),
                viv92             DECIMAL(11,2),
                s_viv             DECIMAL(13,2),
                fondo             DECIMAL(11,2),
                largop            DECIMAL(11,2)
           END RECORD,

           asig_c   RECORD
                  retiro97          DECIMAL(11,2),
                  cesantia          DECIMAL(11,2),
                  cuota             DECIMAL(11,2),
                  retiro92          DECIMAL(11,2),
                  apvolpat          DECIMAL(11,2),
                  apvolven          DECIMAL(11,2),
                  viv97             DECIMAL(11,2),
                  viv92             DECIMAL(11,2),
                  tot_subcta        DECIMAL(11,2)
           END RECORD,

           taa_recep   RECORD
                  folio             INTEGER,
                  f_recepcion       DATE,
                  cve_ced_cuenta    CHAR(03)
           END RECORD,

           afi_mod   RECORD
                  fecha_modifica    INTEGER,
                  status_interno    CHAR(03)
           END RECORD,

           p_rechazo   RECORD
                  f_rechazo         DATE,
                  motivo            CHAR(03),
                  rechazo           CHAR(120)
           END RECORD,

           m_o_t_i_v_o              smallint,

           reg_carta     RECORD LIKE int_ctr_carta.*,

           ho_ra                    CHAR(8),
           usuario                  CHAR(8),
           fecha_425                DATE,
           diferencia               INTEGER,
           fecha                    CHAR(10),
           v_rep                    CHAR(2200),
           des_transferente         CHAR(30),
           nacion                   CHAR(25),
           f_elabora                CHAR(10)

    DEFINE vdiag_proceso            CHAR(3),
           vmotivo_rechazo          CHAR(2),
           vdiag_imagen             CHAR(2),
           vdiag_folio              CHAR(3),
           vtip_rech                SMALLINT,
           vmotivo                  CHAR(3),
           vrechazo                 CHAR(120)

END GLOBALS
#
# ---------------------------------------------------------------------
# Funcion del detalle
# ---------------------------------------------------------------------
#

FUNCTION detalle_notifica(ref,prog,det_cza)
   DEFINE prog     SMALLINT,
          det_cza  SMALLINT,
          ref      CHAR(02)

   CALL limpia2()
   PREPARE apt_1 FROM selec_tab
   DECLARE cur_1 CURSOR FOR apt_1
   FOREACH cur_1 INTO p_mae_afi.*

{display p_mae_afi.*
sleep 30}
         IF p_mae_afi.rfc IS NULL OR
            p_mae_afi.rfc = " "  THEN
            LET rfc = "             "
         ELSE
             LET long = 0
             LET i    = 0
             LET rfc = p_mae_afi.rfc CLIPPED
             LET long = LENGTH(rfc)
             IF long < 18 THEN
                LET long = long + 1
                FOR i = long TO 13
                    LET rfc[i,i] = " "
                END FOR
             END IF
         END IF


         IF p_mae_afi.curp IS NULL OR
            p_mae_afi.curp = " "  THEN
            LET curp = "                  "
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

         LET sexo = p_mae_afi.sexo USING "&"

         IF p_mae_afi.folio = 0 THEN
               LET folio = "0000000000"
         ELSE
                 LET folio = p_mae_afi.folio USING "&&&&&&&&&&"
         END IF

         IF p_mae_afi.fecha_certifica = " "   OR
            p_mae_afi.fecha_certifica IS NULL THEN
             LET f_certifica = "           "
         ELSE
               LET f_certifica = 
                            p_mae_afi.fecha_certifica USING "DD/MM/YYYY"
         END IF

         IF p_mae_afi.fecha_elabora = " "   OR
            p_mae_afi.fecha_elabora IS NULL THEN
             LET f_elabora = "           "
         ELSE
               LET f_elabora = p_mae_afi.fecha_elabora USING "DD/MM/YYYY"
         END IF

         LET finicta = p_mae_afi.finicta
##telefono

         DECLARE apt_tel CURSOR FOR
             SELECT n.telefono FROM afi_telefono n
                    WHERE n.nss      = p_mae_afi.nss
                      AND n.telefono IS NOT NULL
         FOREACH apt_tel INTO p_telefono.*
                 LET telefono = p_telefono.telefono CLIPPED
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
		  AND s.n_folio        = p_mae_afi.folio
		  AND s.tipo_solicitud = p_mae_afi.tipo_solicitud
                  AND s.marca_envio    = "X"

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
                LET calle = p_dom.calle
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
                LET numero  = p_dom.numero
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
               LET depto = "          "
            ELSE
                LET long = 0
                LET i    = 0
                LET depto  = p_dom.depto
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
                IF (STATUS = NOTFOUND) THEN
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

            INITIALIZE pais_cod, pais_desc TO NULL
            SELECT o.pais_cod INTO pais_cod 
                   FROM  afi_domicilio o
                   WHERE o.rowid = domi
            IF pais_cod != "   " OR
               pais_cod IS NOT NULL THEN
               SELECT m.pais_desc INTO pais_desc FROM tab_pais m
               WHERE  m.pais_cod MATCHES pais_cod
            ELSE
               LET pais_desc = "                                        "
            END IF
         END IF

         INITIALIZE nacion TO NULL

         SELECT v.nacionalidad INTO nacion FROM tab_nacionalidad v
         WHERE  v.codigo_pais = p_mae_afi.nacionalidad
         IF SQLCA.SQLCODE <> 0 THEN
            LET nacion = "                         "
         ELSE
             LET long = 0    LET i   = 0
             LET long = LENGTH(nacion)
             IF long < 25 THEN
                LET long = long + 1
                FOR i = long TO 25
                    LET nacion[i,i] = " "
                END FOR
             END IF
         END IF

         INITIALIZE fena, estadon_desc TO NULL
         LET estadon = 0

         SELECT m.fena, m.estadon INTO fena,estadon FROM afi_mae_afiliado m
         WHERE  m.n_seguro = p_mae_afi.nss

         IF estadon <> 0 THEN
            SELECT b.estad_desc
            INTO   estadon_desc
            FROM   tab_estado b
            WHERE  b.estad_cod = estadon

            IF STATUS = NOTFOUND THEN
               LET estadon_desc = "                              ",
                                  "                              "
            END IF
         END IF
##fecha_carta
         LET fe_carta = TODAY USING "mm/dd/yyyy"
##entidad
         LET entidad = "    "
##fecha generacion de la carta
         LET fecha_creacion = TODAY USING "dd/mm/yyyy"
         LET fecha_barra    = TODAY USING "yyyymmdd"
##no_carta
         LET no_carta = "302",ref
##codigo_barras

         LET barras = "C",no_carta,"000",p_mae_afi.nss,
                      fecha_barra

         SELECT a.codigo_afore, a.razon_social[1,30]
         INTO   clave_afore, des_transferente
         FROM   tab_afore_local a

         LET numero_reg = numero_reg + 1
         LET consecutivo_envio = numero_reg        ## USING "&&&&&&&&"

         IF prog = 407 THEN
            SELECT b.finitmte INTO finitmte FROM afi_mae_afiliado b
                   WHERE b.n_seguro = p_mae_afi.nss

            IF finitmte = " "   OR
                  finitmte IS NULL THEN
                   LET f_asigna = "           "
            ELSE
                     LET f_asigna = finitmte USING "DD/MM/YYYY"
            END IF

            LET tipo_proceso = "R"
{
            SELECT a.folio, a.fecha_mov_banxico, a.cve_ced_cuenta
                   INTO taa_recep.* FROM taa_det_recep_ps a
                  WHERE a.nss_afo_recep = p_mae_afi.nss
                    AND a.ident_operacion = '09'
                    AND a.tipo_registro   = '02'
}
            SELECT a.codigo_afore 
              INTO taa_recep.cve_ced_cuenta 
              FROM tab_afore_local a

            IF STATUS = NOTFOUND THEN
               LET asig_c.retiro97   = 0
               LET asig_c.cesantia   = 0
               LET asig_c.cuota      = 0
               LET asig_c.retiro92   = 0
               LET asig_c.apvolpat   = 0
               LET asig_c.apvolven   = 0
               LET asig_c.viv97      = 0
               LET asig_c.viv92      = 0
               LET asig_c.tot_subcta = 0
            ELSE
               SELECT afore_desc INTO desc_entidad FROM tab_afore
               WHERE afore_cod = taa_recep.cve_ced_cuenta

               LET asig_c.retiro97   = 0
               LET asig_c.cesantia   = 0
               LET asig_c.cuota      = 0
               LET asig_c.retiro92   = 0
               LET asig_c.apvolpat   = 0
               LET asig_c.apvolven   = 0
               LET asig_c.viv97      = 0
               LET asig_c.viv92      = 0
               LET asig_c.tot_subcta = 0

               SELECT a.precio_del_dia INTO precio FROM glo_valor_accion a
##               WHERE a.fecha_valuacion = TODAY
               WHERE a.fecha_valuacion = p_mae_afi.fecha_certifica
               AND   a.codigo_siefore  = 1

               SELECT ROUND(SUM(monto_en_acciones)* precio,2) 
               INTO   asig_c.retiro97
               FROM   dis_cuenta
               where  nss               = p_mae_afi.nss
               and    subcuenta         = 1
               and    tipo_movimiento   > 0
               and    fecha_conversion  <= p_mae_afi.fecha_certifica

               SELECT ROUND(SUM(monto_en_acciones)* precio,2) 
               INTO   asig_c.cesantia
               FROM   dis_cuenta
               where  nss              = p_mae_afi.nss
               and    subcuenta        IN(2,6,9)
               and    tipo_movimiento  > 0
               and    fecha_conversion <= p_mae_afi.fecha_certifica

               SELECT ROUND(SUM(monto_en_acciones)* precio,2) 
               INTO   asig_c.apvolpat
               FROM   dis_cuenta
               where  nss = p_mae_afi.nss
               and    subcuenta = 3
               and    tipo_movimiento > 0
               and    fecha_conversion <= p_mae_afi.fecha_certifica

               SELECT ROUND(SUM(monto_en_pesos),2) 
               INTO   asig_c.viv97
               FROM   dis_cuenta
               where  nss = p_mae_afi.nss
               and    subcuenta = 4
               and    tipo_movimiento > 0
               and    fecha_conversion <= p_mae_afi.fecha_certifica

               SELECT ROUND(SUM(monto_en_acciones)* precio,2) 
               INTO   asig_c.cuota
               FROM   dis_cuenta
               where  nss = p_mae_afi.nss
               and    subcuenta = 5
               and    tipo_movimiento > 0
               and    fecha_conversion <= p_mae_afi.fecha_certifica

               SELECT ROUND(SUM(monto_en_acciones)* precio,2) 
               INTO   asig_c.retiro92
               FROM   dis_cuenta
               where  nss = p_mae_afi.nss
               and    subcuenta = 7
               and    tipo_movimiento > 0
               and    fecha_conversion <= p_mae_afi.fecha_certifica
   
               SELECT ROUND(SUM(monto_en_pesos),2) 
               INTO   asig_c.viv92
               FROM   dis_cuenta
               where  nss = p_mae_afi.nss
               and    subcuenta = 8
               and    tipo_movimiento > 0
               and    fecha_conversion <= p_mae_afi.fecha_certifica

               SELECT ROUND(SUM(monto_en_acciones)* precio,2)
               INTO   asig_c.apvolven
               FROM   dis_cuenta
               where  nss = p_mae_afi.nss
               and    subcuenta = 10
               and    tipo_movimiento > 0
               and    fecha_conversion <= p_mae_afi.fecha_certifica

            END IF

            IF asig_c.retiro97 IS NULL THEN
               LET asig_c.retiro97 = 0
            END IF
   
            IF asig_c.retiro92 IS NULL THEN
               LET asig_c.retiro92 = 0
            END IF

            IF asig_c.cesantia IS NULL THEN
               LET asig_c.cesantia = 0
            END IF
               
            IF asig_c.cuota IS NULL THEN
               LET asig_c.cuota = 0
            END IF

            IF asig_c.viv97 IS NULL THEN
               LET asig_c.viv97 = 0
            END IF

            IF asig_c.viv92 IS NULL THEN
               LET asig_c.viv92 = 0
            END IF

            IF asig_c.apvolven IS NULL THEN
               LET asig_c.apvolven = 0
            END IF
               
            IF asig_c.apvolpat IS NULL THEN
               LET asig_c.apvolpat = 0
            END IF
               
            LET asig_c.tot_subcta = asig_c.retiro97 + 
                                    asig_c.retiro92 + 
                                    asig_c.cesantia + 
                                    asig_c.cuota    + 
                                    asig_c.apvolpat + 
                                    asig_c.apvolven + 
                                    asig_c.viv97    + 
                                    asig_c.viv92   


            SELECT a.finicta INTO fecha_rec FROM afi_mae_afiliado a
            WHERE a.n_seguro = p_mae_afi.nss 

            LET fecha_init = fecha_rec USING "dd/mm/yyyy"

            LET v_rep  =  "02"                                  , "|",  #01
                          no_carta                              , "|",  #02
                          nombres                               , "|",  #03
                          paterno                               , "|",  #04
                          materno                               , "|",  #05
                          calle                                 , "|",  #06
                          numero                                , "|",  #07
                          depto                                 , "|",  #08
                          colonia                               , "|",  #09
                          desc_delega                           , "|",  #10
                          poblacion                             , "|",  #11
                          estado                                , "|",  #12
                          p_dom.codigo_postal                   , "|",  #13
                          centro_reparto                        , "|",  #14
                          telefono                              , "|",  #15
                          fecha_creacion                        , "|",  #16
                          p_mae_afi.nss                         , "|",  #17
                          {folio                                 , "|",  #18
                          fecha_init                            , "|",  #19
                          tipo_proceso                          , "|",  #20
                          f_certifica                           , "|",  #21
                          f_asigna                              , "|",  #22
                          curp                                  , "|",  #23
                          taa_recep.cve_ced_cuenta              , "|",  #24
                          desc_entidad                          , "|",  #25}
                          sexo                                  , "|",  #18
                          curp                                  , "|",  #19
                          rfc                                   , "|",  #20
                          p_mae_afi.fbdnsar USING "dd/mm/yyyy"  , "|",  #21
                          f_elabora                             , "|",  #22
                          folio                                 , "|",  #23
                          tipo_proceso                          , "|",  #24
                          clave_afore USING "&&&"               , "|",  #25
                          des_transferente                      , "|",  #26
                          nacion                                , "|",  #27
                          fena                                  , "|",  #28
                          finicta USING "dd/mm/yyyy"            , "|",  #29
                          f_asigna                              , "|",  #30
                          asig_c.retiro97   USING "&&&&&&&&.&&" , "|",  #41
                          asig_c.cesantia   USING "&&&&&&&&.&&" , "|",  #42
                          asig_c.cuota      USING "&&&&&&&&.&&" , "|",  #43
                          asig_c.retiro92   USING "&&&&&&&&.&&" , "|",  #44
                          asig_c.apvolpat   USING "&&&&&&&&.&&" , "|",  #45
                          asig_c.apvolven   USING "&&&&&&&&.&&" , "|",  #46
                          asig_c.viv97      USING "&&&&&&&&.&&" , "|",  #47
                          asig_c.viv92      USING "&&&&&&&&.&&" , "|",  #48
                          asig_c.tot_subcta USING "&&&&&&&&.&&" , "|",  #49
                          barras                                , "|"   #50
         END IF

         IF prog = 412 THEN
            LET v_rep  =  "02"                                , "|",  #01
                          no_carta                            , "|",  #02
                          nombres                             , "|",  #03
                          paterno                             , "|",  #04
                          materno                             , "|",  #05
                          calle                               , "|",  #06
                          numero                              , "|",  #07
                          depto                               , "|",  #08
                          colonia                             , "|",  #09
                          desc_delega                         , "|",  #10
                          poblacion                           , "|",  #11
                          estado                              , "|",  #12
                          p_dom.codigo_postal                 , "|",  #13
                          centro_reparto                      , "|",  #14
                          telefono                            , "|",  #15
                          fecha_creacion                      , "|",  #16
                          p_mae_afi.nss                       , "|",  #17
                          folio                               , "|",  #18
                          curp                                , "|",  #19
                          barras                              , "|"   #20
         END IF

         IF prog = 413 OR prog = 445 THEN
            SELECT MAX(a.fecha_modifica), a.status_interno 
                      INTO  afi_mod.*
                      FROM  afi_mae_modifica a
                      WHERE a.n_seguro = p_mae_afi.nss
                      GROUP BY 2

            LET afi_mod.fecha_modifica = afi_mod.fecha_modifica 
                                         USING "dd/mm/yyyy"
            LET estatus = "O"
            LET tipo_proceso = "R"

            LET v_rep  =  "02"                                , "|",  #01
                          no_carta                            , "|",  #02
                          nombres                             , "|",  #03
                          paterno                             , "|",  #04
                          materno                             , "|",  #05
                          calle                               , "|",  #06
                          numero                              , "|",  #07
                          depto                               , "|",  #08
                          colonia                             , "|",  #09
                          desc_delega                         , "|",  #10
                          poblacion                           , "|",  #11
                          estado                              , "|",  #12
                          p_dom.codigo_postal                 , "|",  #13
                          centro_reparto                      , "|",  #14
                          telefono                            , "|",  #15
                          fecha_creacion                      , "|",  #16
                          p_mae_afi.nss                       , "|",  #17
                          folio                               , "|",  #18
                          afi_mod.fecha_modifica              , "|",  #19
                          p_mae_afi.rfc                       , "|",  #20
                          estatus                             , "|",  #21
                          tipo_proceso                        , "|",  #22
                          barras                              , "|"   #23
         END IF

         IF prog = 418  OR  prog = 459 THEN
            DECLARE app CURSOR FOR
            SELECT UNIQUE MAX(w.f_rechazo), w.rdeta_cod, w.observacion
                   FROM   afi_rechaza_cert w
                   WHERE  w.n_seguro = p_mae_afi.nss
                     AND  w.n_folio  = p_mae_afi.folio
                     AND  w.tipo_solicitud = p_mae_afi.tipo_solicitud
                     AND  w.f_rechazo = p_mae_afi.fecha_certifica
                   GROUP BY 2,3
            FOREACH app INTO p_rechazo.*
	       EXIT FOREACH
            END FOREACH

            CASE p_mae_afi.tipo_solicitud
            WHEN 1
               SELECT f.rdeta_desc_l INTO p_rechazo.rechazo FROM tab_rdeta f
               WHERE  f.rdeta_cod   =  p_rechazo.motivo
                 AND  f.modulo_cod = "afi"
            WHEN 2
               SELECT f.rdeta_desc_l INTO p_rechazo.rechazo FROM tab_rdeta f
               WHERE  f.rdeta_cod   =  p_rechazo.motivo
                 AND  f.modulo_cod = "taa"
            WHEN 8
               SELECT f.rdeta_desc_l INTO p_rechazo.rechazo FROM tab_rdeta f
               WHERE  f.rdeta_cod   =  p_rechazo.motivo
                 AND  f.modulo_cod = "afi"
            END CASE

            LET moti_vo = p_rechazo.motivo
            LET long = 0
            LET i    = 0
            LET long = LENGTH(moti_vo)
            IF long = 1 THEN
                LET p_rechazo.motivo = "00",moti_vo CLIPPED
            ELSE
               IF long = 2 THEN
                   LET p_rechazo.motivo = "0",moti_vo CLIPPED
               END IF
            END IF

            LET rechazo = p_rechazo.rechazo
            LET long = 0
            LET i    = 0
            LET long = LENGTH(rechazo)
            IF long < 120 THEN
               LET long = long + 1
               FOR i = long TO 120
                   LET rechazo[i,i] = " "
               END FOR
            END IF
            LET p_rechazo.rechazo = rechazo
            LET motivo = "   "
            LET rechazo = "                                                  ",
                          "                                                  ",
                          "                    " 
            LET v_rep  =  "02"                                , "|",  #01
                          no_carta                            , "|",  #02
                          nombres                             , "|",  #03
                          paterno                             , "|",  #04
                          materno                             , "|",  #05
                          calle                               , "|",  #06
                          numero                              , "|",  #07
                          depto                               , "|",  #08
                          colonia                             , "|",  #09
                          desc_delega                         , "|",  #10
                          poblacion                           , "|",  #11
                          estado                              , "|",  #12
                          p_dom.codigo_postal                 , "|",  #13
                          centro_reparto                      , "|",  #14
                          telefono                            , "|",  #15
                          fecha_creacion                      , "|",  #16
                          p_mae_afi.nss                       , "|",  #17
                          sexo                                , "|",  #18
                          p_mae_afi.curp                      , "|",  #19
                          f_certifica                         , "|",  #20
                          folio                               , "|",  #21
                          entidad                             , "|",  #22
                          p_rechazo.motivo                    , "|",  #23
                          motivo                              , "|",  #24
                          motivo                              , "|",  #25
                          motivo                              , "|",  #26
                          motivo                              , "|",  #27
                          p_rechazo.rechazo                   , "|",  #28
                          rechazo                             , "|",  #29
                          rechazo                             , "|",  #30
                          rechazo                             , "|",  #31
                          rechazo                             , "|",  #32
                          barras                              , "|"   #33
         END IF

         IF prog = 425 THEN
            IF p_mae_afi.curp IS NULL OR                  
               p_mae_afi.curp = "                  "  THEN
               CONTINUE FOREACH                           
            END IF                                        

            LET long = 0                                   
            LET i    = 0                                   
            LET curp = p_mae_afi.curp CLIPPED              
            LET long = LENGTH(curp)                        
            IF long = 18 AND                               
                p_mae_afi.curp <> "                  " THEN 
                SELECT MAX(g.fecha_actualiza) INTO fecha_425
                FROM  afi_dispersa_curp g            
                WHERE g.n_seguro = p_mae_afi.nss     
            
                SELECT "f.X" FROM afi_dispersa_curp f       
                WHERE f.n_seguro = p_mae_afi.nss     
                  AND f.status_renapo IN(71,72,73)   
                  AND f.fecha_actualiza = fecha_425  
                IF STATUS = NOTFOUND THEN                   
                    CONTINUE FOREACH                         
                END IF                                      
            END IF                                         

            LET v_rep  =  "02"                                , "|",  #01
                          no_carta                            , "|",  #02
                          nombres                             , "|",  #03
                          paterno                             , "|",  #04
                          materno                             , "|",  #05
                          calle                               , "|",  #06
                          numero                              , "|",  #07
                          depto                               , "|",  #08
                          colonia                             , "|",  #09
                          desc_delega                         , "|",  #10
                          poblacion                           , "|",  #11
                          estado                              , "|",  #12
                          p_dom.codigo_postal                 , "|",  #13
                          centro_reparto                      , "|",  #14
                          telefono                            , "|",  #15
                          fecha_creacion                      , "|",  #16
                          p_mae_afi.nss                       , "|",  #17
                          sexo                                , "|",  #18
                          curp                                , "|",  #19
                          f_certifica                         , "|",  #20
                          folio                               , "|",  #21
                          entidad                             , "|",  #22
                          barras                              , "|"   #23

             INSERT INTO int_ctr_carta
                    VALUES(p_mae_afi.nss,
                           p_mae_afi.folio,
                           p_mae_afi.tipo_solicitud,
                           p_mae_afi.fecha_certifica,
                           30216, 
                           null,
                           20,
                           fe_carta,
                           ho_ra,
                           0,
                           consecutivo_envio,
                           0)
         END IF

         IF prog = 428 THEN
            SELECT b.opera_cod INTO opera FROM int_ctr_carta b
            WHERE  b.nss = p_mae_afi.nss
              AND  b.n_folio = p_mae_afi.folio
              AND  b.tipo_solicitud = p_mae_afi.tipo_solicitud
              AND  b.fecha_registro  = p_mae_afi.fecha_certifica
              AND  b.docto_cod  = 30219

            INITIALIZE p_rechazo.* TO NULL

            CASE opera
                WHEN "P"
                     SELECT MAX(f.f_rechazo), f.rdeta_cod, f.observacion
                            INTO   p_rechazo.*
                            FROM   afi_rechaza_cert f
                            WHERE  f.n_seguro = p_mae_afi.nss
                              AND  f.f_rechazo = p_mae_afi.fecha_certifica
                              AND  f.n_folio   = p_mae_afi.folio
                            GROUP BY 2,3

                     IF STATUS = NOTFOUND THEN
                         CALL limpia2()
                         CONTINUE FOREACH
                     END IF

                     IF p_rechazo.motivo = 962 THEN    ##peticion 27/06/03
                         LET numero_reg = numero_reg - 1
                         LET consecutivo_envio = numero_reg
                         CALL limpia2()
                         CONTINUE FOREACH
                     END IF

                     SELECT f.rdeta_desc_l
                     INTO   p_rechazo.rechazo
                     FROM   tab_rdeta f
                     WHERE  f.rdeta_cod   =  p_rechazo.motivo
                     AND    f.modulo_cod  = "taa"
                OTHERWISE
                     INITIALIZE moti_vo TO NULL
                     LET no_motivo = 0

                     {SELECT a.motivo_rechazo
                     INTO   p_rechazo.motivo
                     FROM   taa_det_devol a
                     WHERE  a.n_seguro           = p_mae_afi.nss
                     AND    a.fecha_presentacion = p_mae_afi.fecha_certifica

                     LET no_motivo = p_rechazo.motivo

                     SELECT a.desc_rech
                     INTO   p_rechazo.rechazo
                     FROM   tab_dev_taa a
                     WHERE  a.cod_rech = no_motivo
                     LET p_rechazo.motivo = no_motivo USING "&&&"}


                     SELECT a.diag_proceso[1,3], 
                            a.motivo_rechazo, 
                            a.diag_imagen[1,2],
                            a.diag_folio
                     INTO   vdiag_proceso,
                            vmotivo_rechazo,
                            vdiag_imagen,
                            vdiag_folio
                     FROM   taa_det_devol a
                     WHERE  a.n_seguro           = p_mae_afi.nss
                     AND    a.fecha_presentacion = p_mae_afi.fecha_certifica

                     #IF (vdiag_proceso IS NOT NULL)            OR 
                     IF (vdiag_proceso <> '   ')   THEN
                        CALL obtiene_desc(vdiag_proceso, 1, 0) 
                        RETURNING p_rechazo.motivo, vrechazo, vtip_rech 

                        LET p_rechazo.motivo  = p_rechazo.motivo USING "&&&"
                        LET p_rechazo.rechazo = vrechazo
                        LET vtip_rech = 1
                     ELSE
                        LET vtip_rech = 0
                     END IF

                     #IF (vmotivo_rechazo IS NOT NULL) OR 
                     IF (vmotivo_rechazo <> '  ')   THEN
                        CALL obtiene_desc(vmotivo_rechazo, 2, vtip_rech)
                        RETURNING vmotivo, vrechazo, vtip_rech
                        IF vtip_rech = 0 THEN
                           LET p_rechazo.motivo  = vmotivo
                           LET p_rechazo.motivo  = p_rechazo.motivo USING "&&"
                           LET p_rechazo.rechazo = vrechazo

                           LET vtip_rech = 1
                        ELSE
                          CALL asigna_var(vmotivo, vrechazo, 2, vtip_rech)
                          LET vtip_rech = 2
                        END IF
                     END IF

                     #IF (vdiag_imagen IS NOT NULL) OR 
                     IF (vdiag_imagen <> '  ')   THEN
                        CALL obtiene_desc(vdiag_imagen, 2, vtip_rech)
                        RETURNING vmotivo, vrechazo, vtip_rech

                        IF vtip_rech = 0 THEN
                           LET p_rechazo.motivo  = vmotivo
                           LET p_rechazo.motivo  = p_rechazo.motivo USING "&&"
                           LET p_rechazo.rechazo = vrechazo
                        ELSE

                          CALL asigna_var(vmotivo, vrechazo, 2, vtip_rech)
                        END IF
                     END IF
            END CASE

            IF p_rechazo.motivo != 14 OR p_rechazo.motivo != 719 THEN
               LET leyenda = "NO OBSTANTE LO ANTERIOR CUENTE CON LA ",
                             "SEGURIDAD DE QUE AFORE COPPEL, ",
                             "EN TODO MOMENTO SE MANTENDRA AL PENDIENTE ",
                             "DE SU TRASPASO, Y LE INFORMARA AL RESPECTO"
            END IF

            LET rechazo = p_rechazo.rechazo
            LET long = 0
            LET i    = 0
            LET long = LENGTH(rechazo)
            IF long < 120 THEN
               LET long = long + 1
               FOR i = long TO 120
                   LET rechazo[i,i] = " "
               END FOR
            END IF
            LET p_rechazo.rechazo = rechazo
            LET motivo  = "   "
            LET rechazo = "                                                  ",
                          "                                                  ",
                          "                    " 
            LET m_o_t_i_v_o = 0
            LET m_o_t_i_v_o = p_rechazo.motivo CLIPPED

            LET barras = "C",no_carta, m_o_t_i_v_o USING "&&&",
                          p_mae_afi.nss, fecha_barra

            LET v_rep  =  "02"                                , "|",  #01
                          no_carta                            , "|",  #02
                          nombres                             , "|",  #03
                          paterno                             , "|",  #04
                          materno                             , "|",  #05
                          calle                               , "|",  #06
                          numero                              , "|",  #07
                          depto                               , "|",  #08
                          colonia                             , "|",  #09
                          desc_delega                         , "|",  #10
                          poblacion                           , "|",  #11
                          estado                              , "|",  #12
                          p_dom.codigo_postal                 , "|",  #13
                          centro_reparto                      , "|",  #14
                          telefono                            , "|",  #15
                          fecha_creacion                      , "|",  #16
                          p_mae_afi.nss                       , "|",  #17
                          folio                               , "|",  #18
                          p_rechazo.rechazo                   , "|",  #19
                          rechazo1                            , "|",  #20
                          rechazo2                            , "|",  #21
                          rechazo                             , "|",  #22
                          rechazo                             , "|",  #23
                          p_rechazo.motivo                    , "|",  #24
                          motivo1                             , "|",  #25
                          motivo2                             , "|",  #26
                          motivo                              , "|",  #27
                          motivo                              , "|",  #28
                          rechazo                             , "|",  #29
                          "T"                                 , "|",  #30
                          leyenda                             , "|",  #31
                          curp                                , "|",  #32
                          rfc                                 , "|",  #33
                          barras                              , "|"   #34

         END IF

         IF prog = 435 THEN
            LET tipo_proceso = "R"
            LET v_rep  =  "02"                                , "|",  #01
                          no_carta                            , "|",  #02
                          nombres                             , "|",  #03
                          paterno                             , "|",  #04
                          materno                             , "|",  #05
                          calle                               , "|",  #06
                          numero                              , "|",  #07
                          depto                               , "|",  #08
                          colonia                             , "|",  #09
                          desc_delega                         , "|",  #10
                          poblacion                           , "|",  #11
                          estado                              , "|",  #12
                          p_dom.codigo_postal                 , "|",  #13
                          centro_reparto                      , "|",  #14
                          telefono                            , "|",  #15
                          fecha_creacion                      , "|",  #16
                          p_mae_afi.nss                       , "|",  #17
                          p_mae_afi.folio                     , "|",  #18
                          f_certifica                         , "|",  #19
                          tipo_proceso                        , "|",  #20
                          barras                              , "|"   #21
         END IF

         IF prog = 436 THEN
            IF p_mae_afi.tipo_solicitud = 1 THEN
               LET tipo_proceso = "R"
            ELSE
               LET tipo_proceso = "T"
            END IF
            LET v_rep  =  "02"                                , "|",  #01
                          no_carta                            , "|",  #02
                          nombres                             , "|",  #03
                          paterno                             , "|",  #04
                          materno                             , "|",  #05
                          calle                               , "|",  #06
                          numero                              , "|",  #07
                          depto                               , "|",  #08
                          colonia                             , "|",  #09
                          desc_delega                         , "|",  #10
                          poblacion                           , "|",  #11
                          estado                              , "|",  #12
                          p_dom.codigo_postal                 , "|",  #13
                          centro_reparto                      , "|",  #14
                          telefono                            , "|",  #15
                          fecha_creacion                      , "|",  #16
                          p_mae_afi.nss                       , "|",  #17
                          folio                               , "|",  #18
                          f_certifica                         , "|",  #19
                          tipo_proceso                        , "|",  #20
                          barras                              , "|"   #21
         END IF

         IF prog = 449 THEN
            IF p_mae_afi.tipo_solicitud = 8 THEN
               LET tipo_proceso = "R"
            ELSE
               LET tipo_proceso = "T"
            END IF
            LET v_rep  =  "02"                                , "|",  #01
                          no_carta                            , "|",  #02
                          nombres                             , "|",  #03
                          paterno                             , "|",  #04
                          materno                             , "|",  #05
                          calle                               , "|",  #06
                          numero                              , "|",  #07
                          depto                               , "|",  #08
                          colonia                             , "|",  #09
                          desc_delega                         , "|",  #10
                          poblacion                           , "|",  #11
                          estado                              , "|",  #12
                          p_dom.codigo_postal                 , "|",  #13
                          centro_reparto                      , "|",  #14
                          telefono                            , "|",  #15
                          fecha_creacion                      , "|",  #16
                          p_mae_afi.nss                       , "|",  #17
                          folio                               , "|",  #18
                          f_certifica                         , "|",  #19
                          tipo_proceso                        , "|",  #20
                          curp                                , "|",  #21
                          barras                              , "|"   #22
         END IF


##genera arch.

     OUTPUT TO REPORT r_report(v_rep,prog,det_cza)

     SET LOCK MODE TO WAIT

     INSERT INTO int_his_emision_docto VALUES ( prog, p_mae_afi.nss,
                                                fe_carta,
                                                no_carta,
                                                v_rep,
                                                usuario)

     LET ho_ra = TIME
     CASE prog

     WHEN 407
        UPDATE int_ctr_carta
	   SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
               int_ctr_carta.fecha_genera = fe_carta,
               int_ctr_carta.hora_genera  = ho_ra ,
               int_ctr_carta.consecutivo  = consecutivo_envio
         WHERE int_ctr_carta.nss = p_mae_afi.nss
           AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
           AND int_ctr_carta.docto_cod    = 30202
           AND int_ctr_carta.edo_genera   = 10

     WHEN 412
            UPDATE int_ctr_carta 
	       SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                   int_ctr_carta.fecha_genera = fe_carta,
                   int_ctr_carta.hora_genera  = ho_ra ,
                   int_ctr_carta.consecutivo  = consecutivo_envio
             WHERE int_ctr_carta.nss = p_mae_afi.nss
               AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
               AND int_ctr_carta.docto_cod    = 30224
               AND int_ctr_carta.edo_genera   = 10

     WHEN 445
              UPDATE int_ctr_carta
                 SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                     int_ctr_carta.fecha_genera = fe_carta,
                     int_ctr_carta.hora_genera  = ho_ra ,
                     int_ctr_carta.consecutivo  = consecutivo_envio
               WHERE int_ctr_carta.nss = p_mae_afi.nss
                 AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
                 AND int_ctr_carta.docto_cod    = 30504
                 AND int_ctr_carta.edo_genera   = 10

     WHEN 413
            UPDATE int_ctr_carta    
	       SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                   int_ctr_carta.fecha_genera = fe_carta,
                   int_ctr_carta.hora_genera  = ho_ra ,
                   int_ctr_carta.consecutivo  = consecutivo_envio
             WHERE int_ctr_carta.nss = p_mae_afi.nss
               AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
               AND int_ctr_carta.docto_cod    = 30204
               AND int_ctr_carta.edo_genera   = 10

     WHEN 418
            UPDATE int_ctr_carta    
	       SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                   int_ctr_carta.fecha_genera = fe_carta,
                   int_ctr_carta.hora_genera  = ho_ra ,
                   int_ctr_carta.consecutivo  = consecutivo_envio
             WHERE int_ctr_carta.nss = p_mae_afi.nss
               AND int_ctr_carta.n_folio = p_mae_afi.folio
               AND int_ctr_carta.tipo_solicitud = p_mae_afi.tipo_solicitud
               AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
               AND int_ctr_carta.docto_cod    = 30209
               AND int_ctr_carta.edo_genera   = 10

     WHEN 428
            UPDATE int_ctr_carta    
	       SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                   int_ctr_carta.fecha_genera = fe_carta,
                   int_ctr_carta.hora_genera  = ho_ra ,
                   int_ctr_carta.consecutivo  = consecutivo_envio
             WHERE int_ctr_carta.nss = p_mae_afi.nss
               AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
               AND int_ctr_carta.docto_cod    = 30219
               AND int_ctr_carta.edo_genera   = 10

     WHEN 435
            UPDATE int_ctr_carta    
	       SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                   int_ctr_carta.fecha_genera = fe_carta,
                   int_ctr_carta.hora_genera  = ho_ra ,
                   int_ctr_carta.consecutivo  = consecutivo_envio
            WHERE int_ctr_carta.nss = p_mae_afi.nss
              AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
              AND int_ctr_carta.docto_cod    = 30228
              AND int_ctr_carta.edo_genera   = 10

     WHEN 436
            UPDATE int_ctr_carta
	       SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                   int_ctr_carta.fecha_genera = fe_carta,
                   int_ctr_carta.hora_genera  = ho_ra ,
                   int_ctr_carta.consecutivo  = consecutivo_envio
             WHERE int_ctr_carta.nss = p_mae_afi.nss
               AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
               AND int_ctr_carta.docto_cod    = 30229
               AND int_ctr_carta.edo_genera   = 10

     WHEN 449
             UPDATE int_ctr_carta
                SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                    int_ctr_carta.fecha_genera = fe_carta,
                    int_ctr_carta.hora_genera  = ho_ra ,
                    int_ctr_carta.consecutivo  = consecutivo_envio
              WHERE int_ctr_carta.nss = p_mae_afi.nss
                AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
                AND int_ctr_carta.docto_cod    = 30529
                AND int_ctr_carta.edo_genera   = 10

     WHEN 459
            UPDATE int_ctr_carta    
	       SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                   int_ctr_carta.fecha_genera = fe_carta,
                   int_ctr_carta.hora_genera  = ho_ra ,
                   int_ctr_carta.consecutivo  = consecutivo_envio
             WHERE int_ctr_carta.nss = p_mae_afi.nss
               AND int_ctr_carta.n_folio = p_mae_afi.folio
               AND int_ctr_carta.tipo_solicitud = p_mae_afi.tipo_solicitud
               AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
               AND int_ctr_carta.docto_cod    = 30509
               AND int_ctr_carta.edo_genera   = 10
     END CASE

     INSERT INTO int_constancia VALUES ( p_mae_afi.folio,
                                         p_mae_afi.nss,
                                         p_mae_afi.tipo_solicitud,
                                         TODAY,
                                         p_mae_afi.fecha_certifica,
                                         no_carta )


     SET LOCK MODE TO NOT WAIT


     CALL limpia2()
     DISPLAY "REGISTROS : ",numero_reg USING "####&" AT 17,1

   END FOREACH
   DISPLAY "REGISTROS : ",numero_reg USING "####&" AT 17,1
   FINISH REPORT r_report
END FUNCTION
#
#---------------------------------------------------------
# Generando la cabeza
#---------------------------------------------------------
#
FUNCTION cabeza_notifica(ref,leyenda_cza,prog,det_cza)

   DEFINE  cve_afore   SMALLINT,
           afore_local CHAR(03),
           ref         CHAR(02),
           leyenda_cza CHAR(120),
           prog        SMALLINT,
           det_cza     SMALLINT

   INITIALIZE tipo, afore_local, fecha_creacion TO NULL
   LET clave_afore  = 0
   LET consec_lote  = 0
   LET cve_afore    = 0

   SELECT ee.codigo_afore INTO cve_afore FROM tab_afore_local ee
   LET afore_local = cve_afore USING "&&&"

   SELECT MAX(gh.folio) INTO consec_lote FROM int_notifica_folio gh

   IF consec_lote IS NULL OR
      consec_lote  =  0   THEN
      LET consec_lote = 1
   ELSE
       LET consec_lote = consec_lote + 1
   END IF

   LET long = LENGTH(leyenda_cza)
   IF long < 120 THEN
       LET long = long + 1
       FOR i = long TO 120
           LET leyenda_cza[i,i] = " "
       END FOR
   END IF

   IF prog = "445" OR prog = "449" OR prog = "459" THEN
      LET no_carta = "305",ref
   ELSE
      LET no_carta = "302",ref
   END IF

   LET fecha_creacion = TODAY USING "MM/DD/YYYY"


   LET v_rep = "01",                         "|",
               no_carta,                     "|",
               leyenda_cza,                  "|",
               afore_local,                  "|",
               fecha_creacion,               "|",
               consec_lote USING "&&&&&&&&", "|",
               numero_reg  USING "&&&&&&&&", "|"


   OUTPUT TO REPORT r_report(v_rep,prog,det_cza)

   INSERT INTO int_notifica_folio 
               VALUES (consec_lote, fecha_creacion, ref, numero_reg)

   FINISH REPORT r_report
END FUNCTION

FUNCTION limpia2()
    INITIALIZE p_tabafore.*, p_dom.*,  p_telefono.* TO NULL
    INITIALIZE fecha_creacion, calle, depto, colonia, telefono TO NULL
    INITIALIZE desc_delega, entidad, poblacion, centro_reparto,folio TO NULL
    INITIALIZE tipo_proceso, paterno, materno, nombres, asig.*  TO NULL
    INITIALIZE p_rechazo.*, motivo, rechazo, numero, sexo TO NULL

    LET i           = 0
    LET domi        = 0
    LET long        = 0
    LET cod_mot     = 0
    LET diferencia  = 0
    LET clave_afore = 0

    LET vdiag_proceso    = NULL
    LET vmotivo_rechazo  = NULL
    LET vdiag_imagen     = NULL
    LET vdiag_folio      = NULL

    LET motivo1  = "   "
    LET motivo2  = "   "

    LET rechazo1 = "                                                  ",
                   "                                                  ",
                   "                    " 
    LET rechazo2 = "                                                  ",
                   "                                                  ",
                   "                    " 

END FUNCTION

FUNCTION obtiene_desc(fdiagnostico, ftip_diag, ftip_rech)

DEFINE
  fdiagnostico SMALLINT,
  ftip_diag    SMALLINT,
  ftip_rech    SMALLINT,
  fdes_rech    CHAR(120)
  
  
  SELECT a.desc_rech
  INTO   fdes_rech        
  FROM   tab_dev_taa a
  WHERE  a.cod_rech  = fdiagnostico
  AND    a.tipo_diag = ftip_diag

  CASE ftip_rech
    WHEN 0 LET ftip_rech = 0
    WHEN 1 LET ftip_rech = 1
    WHEN 2 LET ftip_rech = 2
  END CASE
  
  RETURN fdiagnostico, fdes_rech, ftip_rech

END FUNCTION

FUNCTION asigna_var(gmotivo, grechazo, gtip_diag, gtip_rech)
  DEFINE
    gmotivo   CHAR(3),
    grechazo  CHAR(120),
    gtip_diag SMALLINT,
    gtip_rech SMALLINT
  
  CASE gtip_rech
    WHEN 1
       LET motivo1 = gmotivo USING "#&&"
       LET rechazo = grechazo
       LET long = 0
       LET i    = 0
       LET long = LENGTH(rechazo)
       
       IF  long < 120 THEN
           LET long = long + 1
           FOR i = long TO 120
               LET rechazo[i,i] = " "
           END FOR
       END IF
       
       LET grechazo = rechazo
       LET rechazo1 = grechazo
       
    WHEN 2
       LET motivo2 = gmotivo USING "#&&"
       LET rechazo = grechazo        
       LET long = 0
       LET i    = 0
       LET long = LENGTH(rechazo)
              
       IF  long < 120 THEN
           LET long = long + 1
           FOR i = long TO 120
               LET rechazo[i,i] = " "
           END FOR
       END IF
              
       LET grechazo = rechazo
       LET rechazo2 = grechazo
  END CASE

END FUNCTION
