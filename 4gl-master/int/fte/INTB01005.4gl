#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB01005 Y INTB0110                          #
#Descripcion       => CZA. Y DET. DE NOTIF. DE UNIFICACION CTAS.    #
#Sistema           => INT .                                         #
#Fecha             => 16 de Mayo del 2002    .                      #
#Por               => Laura Eugenia Cortes Guzman                   #
#Modificado por    => Miguel Angel Hernandez Martinez               #
#Fecha modi        => 23 de abril del 2007   .                      #
#*******************************************************************#

DATABASE safre_af
GLOBALS
    DEFINE hoy_hoy                  DATE,
           fentcons                 DATE,
           fecha_no_local           DATE,
           fecha_gene_car           DATE,

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
           rechazo                  CHAR(50),
           inter_noti               CHAR(300),
           selec_tab_1              CHAR(2000),

           long, i                  INTEGER,
           ban                      INTEGER,

           cod_mot                  SMALLINT,


           clave_afore              SMALLINT,
           codigo_cert              SMALLINT,

           consec_lote              INTEGER,

           numero_reg               DECIMAL(10,0),
           consecutivo_envio        DECIMAL(8,0),

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
                  tipo_solicitud    SMALLINT
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

           reg_carta      RECORD LIKE  int_ctr_carta.*,

           fe_carta                 DATE,
           ho_ra                    CHAR(08),

           p_unifica  RECORD
                  nss_uni           CHAR(11),
                  nss_cta1          CHAR(11),
                  cve_ent_cta1      CHAR(10),
                  fec_unifica       DATE,
                  folio_liquida     INTEGER
           END RECORD,

           usuario                  char(08),
           desc_ent_cta1            char(20),
           diferencia               INTEGER,
           fecha                    CHAR(10),
           v_rep                    CHAR(2200)

END GLOBALS
#
# ---------------------------------------------------------------------
# Funcion del detalle
# ---------------------------------------------------------------------
#
FUNCTION detalle_notifica_uni(ref,prog,det_cza)
   DEFINE prog            SMALLINT,
          det_cza         SMALLINT,
          jj              INTEGER,
          ref             CHAR(02),
          cuantas_emi     SMALLINT,
          emi_carta       CHAR(01),

           f_v                      DATE,
           s_rcv                    DECIMAL(11,2),
           s_ret92                  DECIMAL(11,2),
           s_ahorro                 DECIMAL(11,2),
           s_apcom                  DECIMAL(11,2),
           s_apvol                  DECIMAL(11,2),
           s_suma_inversion         DECIMAL(13,2),
           s_viv97                  DECIMAL(11,2),
           s_viv92                  DECIMAL(11,2),
           s_fondo                  DECIMAL(11,2),
           s_suma_viv               DECIMAL(13,2),

           nombre_sie               CHAR(08),
           csie_rcv     SMALLINT,
           csie_ret92   SMALLINT,
           csie_ahorro  SMALLINT,
           csie_apcom   SMALLINT,
           csie_apvol   SMALLINT,

           sie_rcv      CHAR(08),
           sie_ret92    CHAR(08),
           sie_ahorro   CHAR(08),
           sie_apcom    CHAR(08),
           sie_apvol    CHAR(08),

           sie   RECORD
              subcuenta     SMALLINT,
              codigo_siefore SMALLINT
           END RECORD

   DEFINE xx_afore_cod  CHAR(3),
          xx_afore_desc CHAR(50)

   CALL limpia()

   PREPARE apt_uni FROM selec_tab_1
   DECLARE cur_uni CURSOR FOR apt_uni
   FOREACH cur_uni INTO p_unifica.*

         SELECT a.cve_ent_nss,
                b.afore_desc
         INTO   xx_afore_cod,
                xx_afore_desc
         FROM   uni_unificador a,tab_afore b
         WHERE  a.nss_uni       = p_unifica.nss_uni
         AND    a.fliquida      = p_unifica.fec_unifica  
         AND    a.folio_liquida = p_unifica.folio_liquida
         AND    a.cve_ent_nss   = b.afore_cod

         SELECT a.n_seguro,
                a.n_rfc,
                a.n_unico,
                a.paterno,
                a.materno,
                a.nombres,
                a.sexo,
                a.n_folio,
                a.fentcons,
                a.tipo_solicitud
         INTO   p_mae_afi.*
         FROM   afi_mae_afiliado a
         WHERE  a.n_seguro = p_unifica.nss_uni

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

         IF p_mae_afi.rfc IS NULL OR
            p_mae_afi.rfc = " "  THEN
            LET rfc = "             "
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
##telefono
         DECLARE apt_tel CURSOR FOR
         SELECT n.telefono
         FROM   afi_telefono n
         WHERE  n.nss      = p_mae_afi.nss
         AND    n.telefono IS NOT NULL
         AND    n.tel_cod  < 7

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
         AND   s.n_folio        = p_mae_afi.folio
         AND   s.tipo_solicitud = p_mae_afi.tipo_solicitud

         IF SQLCA.SQLCODE = 0 THEN
            SELECT o.calle,
                   o.numero,
                   o.depto,
                   o.colonia,
                   o.codpos,
                   o.delega,
                   o.estado,
                   o.ciudad
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
                LET numero = p_dom.numero
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
                LET depto = p_dom.depto
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
               SELECT am.centro_reparto
               INTO   centro_reparto 
               FROM   tab_reparto am
               WHERE  am.codigo_postal = p_dom.codigo_postal
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
                SELECT p.estad_desc
                INTO   estado
                FROM   tab_estado p
                WHERE  p.estad_cod = p_dom.estado
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
               SELECT q.deleg_desc
               INTO   desc_delega
               FROM   tab_delegacion q
               WHERE  q.deleg_cod = p_dom.municip_deleg
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
               SELECT q.ciudad_desc
               INTO   poblacion
               FROM   tab_ciudad q
               WHERE  q.ciudad_cod = p_dom.poblacion
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
         END IF

##descripcion
         SELECT afore_desc
	 INTO   desc_ent_cta1
	 FROM   tab_afore
	 WHERE  afore_cod = p_unifica.cve_ent_cta1

         IF STATUS = NOTFOUND THEN
            LET desc_ent_cta1 = "                    "
         END IF

## siefores

       INITIALIZE sie.*,sie_rcv,sie_ret92,sie_ahorro,sie_apcom,sie_apvol TO NULL
       LET sie_rcv = 0
       LET sie_ret92 = 0
       LET sie_ahorro = 0
       LET sie_apcom = 0
       LET sie_apvol = 0

       DECLARE ap_sie CURSOR FOR
       SELECT m.subcuenta,
              m.codigo_siefore
       FROM   cta_regimen m
       WHERE  m.nss       = p_mae_afi.nss
       AND    m.subcuenta IN(1,7,10,11,13)
         FOREACH ap_sie INTO  sie.*

            INITIALIZE nombre_sie TO NULL
            SELECT razon_social[1,8]
            INTO   nombre_sie
            FROM   tab_siefore_local
            WHERE  codigo_siefore = sie.codigo_siefore

             CASE sie.subcuenta
                  WHEN 1   LET sie_rcv     = nombre_sie 
			   LET csie_rcv    = sie.codigo_siefore
                  WHEN 7   LET sie_ret92   = nombre_sie
			   LET csie_ret92  = sie.codigo_siefore
                  WHEN 10  LET sie_apvol   = nombre_sie
			   LET csie_apvol  = sie.codigo_siefore
                  WHEN 11  LET sie_apcom   = nombre_sie
			   LET csie_apcom  = sie.codigo_siefore
                  WHEN 13  LET sie_ahorro  = nombre_sie
			   LET csie_ahorro = sie.codigo_siefore
             END CASE
         END FOREACH
##montos
         LET s_rcv = 0
         SELECT ROUND(SUM(bb.monto_en_pesos),2)
         INTO   s_rcv
         FROM   dis_cuenta bb
         WHERE  bb.nss       = p_mae_afi.nss
         AND    bb.subcuenta IN(1,2,5,6,9)
         AND    bb.folio = p_unifica.folio_liquida
         AND    bb.fecha_conversion = p_unifica.fec_unifica

         IF s_rcv IS NULL THEN
            LET s_rcv = 0
         END IF
               
         LET s_ret92 = 0
         SELECT ROUND(SUM(bb.monto_en_pesos),2)
         INTO   s_ret92
         FROM   dis_cuenta bb
         WHERE  bb.nss       = p_mae_afi.nss
         AND    bb.subcuenta = 7
         AND    bb.folio = p_unifica.folio_liquida
         AND    bb.fecha_conversion = p_unifica.fec_unifica

         IF s_ret92 IS NULL THEN
            LET s_ret92 = 0
         END IF
               
         LET s_ahorro = 0
         SELECT ROUND(SUM(bb.monto_en_pesos),2)
         INTO   s_ahorro
         FROM   dis_cuenta bb
         WHERE  bb.nss       = p_mae_afi.nss
         AND    bb.subcuenta = 13
         AND    bb.folio = p_unifica.folio_liquida
         AND    bb.fecha_conversion = p_unifica.fec_unifica

         IF s_ahorro IS NULL THEN
            LET s_ahorro = 0
         END IF
               
         LET s_apcom = 0
         SELECT ROUND(SUM(bb.monto_en_pesos),2)
         INTO   s_apcom
         FROM   dis_cuenta bb
         WHERE  bb.nss       = p_mae_afi.nss
         AND    bb.subcuenta IN(11,12)
         AND    bb.folio = p_unifica.folio_liquida
         AND    bb.fecha_conversion = p_unifica.fec_unifica

         IF s_apcom IS NULL THEN
            LET s_apcom = 0
         END IF
               
         LET s_apvol = 0
         SELECT ROUND(SUM(h.monto_en_pesos),2)
         INTO   s_apvol
         FROM   dis_cuenta h
         WHERE  h.nss       = p_mae_afi.nss
         AND    h.subcuenta IN(3,10)
         AND    h.folio = p_unifica.folio_liquida
         AND    h.fecha_conversion = p_unifica.fec_unifica

         IF s_apvol IS NULL THEN
            LET s_apvol = 0
         END IF
               
         LET s_suma_inversion = 0
         LET s_suma_inversion = s_rcv  +  s_ret92  +  s_ahorro  +
		                s_apcom + s_apvol

	 LET f_v = MDY(MONTH(p_unifica.fec_unifica),1,
                   YEAR(p_unifica.fec_unifica))

         LET s_viv97 = 0
         SELECT ROUND(SUM(p.monto_en_pesos),2)
         INTO   s_viv97
         FROM   dis_cuenta p
         WHERE  p.nss       = p_mae_afi.nss
         AND    p.subcuenta IN(4)
         AND    p.folio = p_unifica.folio_liquida
         AND    p.fecha_valor = f_v

         IF s_viv97 IS NULL THEN
            LET s_viv97 = 0
         END IF
            
         LET s_viv92 = 0
         SELECT ROUND(SUM(p.monto_en_pesos),2)
         INTO   s_viv92
         FROM   dis_cuenta p
         WHERE  p.nss       = p_mae_afi.nss
         AND    p.subcuenta IN(8)
         AND    p.folio = p_unifica.folio_liquida
         AND    p.fecha_valor = f_v

         IF s_viv92 IS NULL THEN
            LET s_viv92 = 0
         END IF
            
         LET s_fondo = 0
         SELECT ROUND(SUM(t.monto_en_pesos),2)
         INTO   s_fondo
         FROM   dis_cuenta t
         WHERE  t.nss       = p_mae_afi.nss
         AND    t.subcuenta = 14
         AND    t.folio = p_unifica.folio_liquida
         AND    t.fecha_conversion = p_unifica.fec_unifica

         IF s_fondo IS NULL THEN
            LET s_fondo = 0
         END IF
            
         LET s_suma_viv = 0
         LET s_suma_viv = s_viv97  +  s_viv92  +  s_fondo
##fecha_carta
         LET fe_carta = TODAY USING "mm/dd/yyyy"
         LET ho_ra = TIME
##entidad
         LET entidad = "    "
##fecha generacion de la carta
         LET fecha_creacion = TODAY USING "dd/mm/yyyy"
         LET fecha_barra    = TODAY USING "yyyymmdd"
##no_carta
         LET no_carta = "302",ref
##codigo_barras

         LET cuantas_emi = 0

         SELECT COUNT(*)
         INTO   cuantas_emi
         FROM   int_ctr_carta
         WHERE  nss = p_mae_afi.nss
         AND    docto_cod = no_carta

         IF cuantas_emi IS NULL OR cuantas_emi = 0 THEN
            LET emi_carta = "1"
         ELSE
            LET emi_carta = "2"
         END IF

         LET numero_reg = numero_reg + 1
         LET consecutivo_envio = numero_reg        ## USING "&&&&&&&&"
         LET p_unifica.fec_unifica = p_unifica.fec_unifica USING "dd/mm/yyyy"

         LET xx_afore_desc = xx_afore_desc CLIPPED

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
                       p_unifica.nss_uni                   , "|",  #17
                       xx_afore_cod                        , "|",  #18
                       xx_afore_desc                       , "|",  #19
                       p_unifica.nss_cta1                  , "|",  #20
                       p_unifica.cve_ent_cta1              , "|",  #21
                       desc_ent_cta1                       , "|",  #22
                       p_unifica.fec_unifica               , "|",  #23
                       folio                               , "|",  #24
                       curp                                , "|",  #25
                       rfc                                 , "|",  #26
                       s_rcv                               , "|",  #27
                       s_ret92                             , "|",  #28
                       s_ahorro                            , "|",  #29
                       s_apcom                             , "|",  #30
                       s_apvol                             , "|",  #31
                       s_suma_inversion                    , "|",  #32
                       sie_rcv                             , "|",  #33
                       sie_ret92                           , "|",  #34
                       sie_ahorro                          , "|",  #35
                       sie_apcom                           , "|",  #36
                       sie_apvol                           , "|",  #37
                       s_viv97                             , "|",  #38
                       s_viv92                             , "|",  #39
                       s_fondo                             , "|",  #40
                       s_suma_viv                          , "|",  #41
                       no_carta                            , "|",  #42
                       p_unifica.nss_uni                   , "|",  #43
                       fecha_barra                         , "|",  #44
                       folio                               , "|",  #45
                       1                                   , "|",  #46
--                       emi_carta                           , "|",  #24
                       curp                                , "|",  #47
                       "000"                               , "|"   #48

        OUTPUT TO REPORT r_report(v_rep,prog,det_cza)

        SET LOCK MODE TO WAIT
        INSERT INTO int_his_emision_docto VALUES ( prog, p_mae_afi.nss,
                                                   fe_carta,
                                                   no_carta,
                                                   v_rep,
                                                   usuario)

        INSERT INTO int_ctr_carta VALUES ( p_mae_afi.nss,
                                           p_mae_afi.folio,
                                           p_mae_afi.tipo_solicitud,
                                           p_mae_afi.fecha_certifica,
                                           no_carta,
                                           "",
                                           20,
                                           fe_carta,
                                           ho_ra,
                                           0,
                                           numero_reg,
                                           0)

        INSERT INTO int_constancia VALUES ( p_mae_afi.folio,
                                            p_mae_afi.nss,
                                            p_mae_afi.tipo_solicitud,
                                            TODAY,
                                            p_mae_afi.fecha_certifica,
                                            no_carta )
        SET LOCK MODE TO NOT WAIT

        DISPLAY "REGISTROS : ",numero_reg USING "####&" AT 17,1

        CALL limpia()
   END FOREACH
   DISPLAY "REGISTROS : ",numero_reg USING "####&" AT 17,1
   FINISH REPORT r_report
END FUNCTION
#
#---------------------------------------------------------
# Generando la cabeza
#---------------------------------------------------------
#
FUNCTION cabeza_notifica_uni(ref,leyenda_cza,prog,det_cza)

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

   SELECT ee.codigo_afore
   INTO   cve_afore
   FROM   tab_afore_local ee
   LET afore_local = cve_afore USING "&&&"

   SELECT MAX(gh.folio)
   INTO   consec_lote
   FROM   int_notifica_folio gh

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

   LET no_carta = "302",ref

   LET fecha_creacion = TODAY USING "DD/MM/YYYY"


   LET v_rep = "01",                         "|",
               no_carta,                     "|",
               leyenda_cza,                  "|",
               afore_local,                  "|",
               fecha_creacion,               "|",
               consec_lote USING "&&&&&&&&", "|",
               numero_reg  USING "&&&&&&&&", "|"


   OUTPUT TO REPORT r_report(v_rep,prog,det_cza)

   INSERT INTO int_notifica_folio 
               VALUES (consec_lote, TODAY, ref, numero_reg)

   FINISH REPORT r_report
END FUNCTION

FUNCTION limpia()
    INITIALIZE p_dom.*, p_mae_afi.*, p_telefono.* TO NULL
    INITIALIZE fecha_creacion, calle, depto, colonia, telefono TO NULL
    INITIALIZE desc_delega, entidad, poblacion, centro_reparto,folio TO NULL
    INITIALIZE tipo_proceso, paterno, materno, nombres,numero,sexo TO NULL

    LET i           = 0
    LET domi        = 0
    LET long        = 0
    LET cod_mot     = 0
    LET diferencia  = 0
    LET clave_afore = 0

END FUNCTION

