#*********************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                    #
#Propietario       => E.F.P.                                          #
#Programa          => INTB01002  INTB0108                             #
#Descripcion       => GLOBALS y FUNCION DETALLE_NOTIFICA, CZA_NOTIFICA#
#Sistema           => INT .                                           #
#Fecha             => 18 de Abril del 2002   .                        #
#Por               => Laura Eugenia Cortes Guzman                     #
# Req-1182			 	 => Francisca Schell Rosales 												#
# Fecha Modif			 => 22/02/2013																			#
# Req-1261				 => Francisca Schell Rosales												#
#REQ CPL-1321			 => FSR Actualización subcuentas en calculo					#
#REQ CPL-1802      => FSR Se agrega subcuenta 19 y 36                 #
#*********************************************************************#
DATABASE safre_af
GLOBALS
    DEFINE p_tabafore               RECORD LIKE tab_afore_local.*,
           hoy_hoy                  DATE,
           fe_carta                 DATE,
           fentcons                 DATE,
           vfecha_mov_banxico        DATE,
           fecha_gene_car           DATE,

           hoy                      CHAR(06),
           n_seguro                 CHAR(11),
           enter, opc               CHAR(01),
           tipo                     CHAR(02),
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
           f_elabora                CHAR(10),
           fena                     CHAR(10),
           f_asigna                 CHAR(10),
           f_red_bono               CHAR(10),
           fecha_liquida            CHAR(10),
           paterno                  CHAR(40),
           materno                  CHAR(40),
           nombres                  CHAR(40),
           curp                     CHAR(18),
           rfc                      CHAR(18),
           desc_motivo              CHAR(70),
           status_proceso           CHAR(01),
           inter_noti               CHAR(300),
           selec_tab1               CHAR(2000),

           long, i                  INTEGER,
           ban                      INTEGER,
           consec_lote              INTEGER,

           cod_mot                  SMALLINT,
           clave_afore              SMALLINT,
           codigo_cert              SMALLINT,

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
                  vfecha_mov_banxico DATE,
                  cve_transferente  CHAR(03),
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
                  fbdnsar           DATE,
                  importe_bono      DECIMAL(13,2),
                  fecha_red_bono    DATE,
                  taa_folio					DECIMAL(10,0) #1261
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

           des_transferente         CHAR(30),
           des_afore_registra       CHAR(30),

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
                largop            DECIMAL(11,2),
                rcv_issste        DECIMAL(11,2),
                bono_pen          DECIMAL(11,2) #CPL-1802
           END RECORD,

           reg_carta      RECORD LIKE  int_ctr_carta.*,

           afore_xx               CHAR(20),
           comis_xx               CHAR(07),
           nacion                 CHAR(25),
           prom_simple 			  DECIMAL(4,2),

           gen_compara   RECORD
               afore_a,  afore_b,
               afore_c,  afore_d,
               afore_e,  afore_f,
               afore_g,  afore_h,
               afore_i,  afore_j,
               afore_k,  afore_l,
               afore_m,  afore_n,
               afore_o,  afore_p,
               afore_q,  afore_r,
               afore_s,  afore_t,
               afore_u,  afore_v,
               afore_w,  afore_x,
               afore_y,  afore_z      CHAR(20),


               rend_a,  rend_b,
               rend_c,  rend_d,
               rend_e,  rend_f,
               rend_g,  rend_h,
               rend_i,  rend_j,
               rend_k,  rend_l,
               rend_m,  rend_n,
               rend_o,  rend_p,
               rend_q,  rend_r,
               rend_s,  rend_t,
               rend_u,  rend_v,
               rend_w,  rend_x,
               rend_y,  rend_z        CHAR(06),

               comis_a,  comis_b,
               comis_c,  comis_d,
               comis_e,  comis_f,
               comis_g,  comis_h,
               comis_i,  comis_j,
               comis_k,  comis_l,
               comis_m,  comis_n,
               comis_o,  comis_p,
               comis_q,  comis_r,
               comis_s,  comis_t,
               comis_u,  comis_v,
               comis_w,  comis_x,
               comis_y,  comis_z      CHAR(07),

               rendn_a,  rendn_b,
               rendn_c,  rendn_d,
               rendn_e,  rendn_f,
               rendn_g,  rendn_h,
               rendn_i,  rendn_j,
               rendn_k,  rendn_l,
               rendn_m,  rendn_n,
               rendn_o,  rendn_p,
               rendn_q,  rendn_r,
               rendn_s,  rendn_t,
               rendn_u,  rendn_v,
               rendn_w,  rendn_x,
               rendn_y,  rendn_z        CHAR(06)
           END RECORD,

           ho_ra, usuario           CHAR(8),
           diferencia               INTEGER,
           fecha                    CHAR(10),
           v_rep                    CHAR(3000),
           f_val, f_v               DATE,
           lc_query                 CHAR(3000)

END GLOBALS

#
# ---------------------------------------------------------------------
# Funcion del detalle
# ---------------------------------------------------------------------
#

FUNCTION detalle_notifica_09(ref,prog,det_cza)

   DEFINE prog         SMALLINT,
          det_cza      SMALLINT,
          ref          CHAR(02),
          cuan_tas     SMALLINT,
          car_gen      SMALLINT,
          nombre_sie   CHAR(08),

          csie_rcv     SMALLINT,
          csie_ret92   SMALLINT,
          csie_ahorro  SMALLINT,
          csie_apcom   SMALLINT,
          csie_apvol   SMALLINT,
          csie_largop  SMALLINT,

          sie_rcv      CHAR(08),
          sie_ret92    CHAR(08),
          sie_ahorro   CHAR(08),
          sie_apcom    CHAR(08),
          sie_apvol    CHAR(08),
          sie_largop   CHAR(08),

          sie   RECORD
             subcuenta     SMALLINT,
             codigo_siefore SMALLINT
          END RECORD,

          ahorro_retiro,
          ahorro_retiro_issste,
          ahorro_vol,
          ahorro_viv,
          ahorro_viv_issste
          #CPL-1321 ahorro_lplazo     
          DECIMAL(13,2),

          tot_ahorro        DECIMAL(15,2), 
          vsiefore          SMALLINT,
          vprom_simple	    CHAR(07),

          v_anio_mov_banxico,
          v_anio_actual,
          v_anio_valor         CHAR(04),
          v_tabla,
          v_tablav             CHAR(13),
          v_anio               CHAR(2),
          v_existe_dis_ctaxx,
          v_existe_dis_ctaxxv  INTEGER,
          v_subcuenta          CHAR(35),
          v_subcuentav         CHAR(2),
          v_dis                INTEGER,
          v_con_fv             INTEGER


   CALL limpia()
   
   PREPARE apt_08_1 FROM selec_tab1
   DECLARE cur_09 CURSOR FOR apt_08_1
   FOREACH cur_09 INTO p_mae_afi.*

   INITIALIZE v_anio_mov_banxico, v_anio_actual, v_tabla, v_anio TO NULL 

    CALL llena_comision_registro(p_mae_afi.vfecha_mov_banxico, p_mae_afi.nss) RETURNING gen_compara.*,ban

       # Se obtiene siefore regimen CPL-1182
      SELECT c.codigo_siefore
      INTO vsiefore
      FROM cta_nss_regimen c
     WHERE c.nss = p_mae_afi.nss
      AND grupo_regimen = 1
      
      
   ## Se calcula el promedio simple   CPL-1182
      SELECT ROUND(AVG(a.rendimiento_neto),2)as rend_neto
      INTO prom_simple
      FROM tab_rendimiento_neto a
      WHERE p_mae_afi.vfecha_mov_banxico BETWEEN a.fecha_ini AND a.fecha_fin
      AND a.siefore_cod = vsiefore
      
	   LET vprom_simple    = prom_simple USING "##&.&&","%" #CPL-1182
    
         IF p_mae_afi.curp IS NULL OR
            p_mae_afi.curp[1,1] = " "  THEN
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
            p_mae_afi.rfc[1,1] = " "  THEN
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
               LET f_elabora = 
                            p_mae_afi.fecha_elabora USING "DD/MM/YYYY"
         END IF

         IF p_mae_afi.fena = " "   OR
            p_mae_afi.fena IS NULL THEN
             LET fena = "           "
         ELSE
               LET fena = 
                            p_mae_afi.fena USING "DD/MM/YYYY"
         END IF

         IF p_mae_afi.finitmte = " "   OR
            p_mae_afi.finitmte IS NULL THEN
             LET f_asigna = "           "
         ELSE
               LET f_asigna = 
                            p_mae_afi.finitmte USING "DD/MM/YYYY"
         END IF

         IF p_mae_afi.fecha_red_bono = " "   OR
            p_mae_afi.fecha_red_bono IS NULL OR 
            p_mae_afi.fecha_red_bono = "01/01/0001" THEN
             LET f_red_bono = "           "
         ELSE
               LET f_red_bono = 
                            p_mae_afi.fecha_red_bono USING "DD/MM/YYYY"
         END IF

         IF p_mae_afi.importe_bono IS NULL THEN
             LET p_mae_afi.importe_bono = 0
         END IF

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

##telefono
         DECLARE apt_tel CURSOR FOR
             SELECT n.telefono FROM afi_telefono n
                    WHERE n.nss      = p_mae_afi.nss
                      AND n.n_folio        = p_mae_afi.folio
                      AND n.tipo_solicitud = p_mae_afi.tipo_solicitud
                      AND n.telefono IS NOT NULL
                      AND n.tel_cod  < 7
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

         LET tipo_proceso = p_mae_afi.tipo_solicitud USING "&"

         IF p_mae_afi.vfecha_mov_banxico IS NULL OR
            p_mae_afi.vfecha_mov_banxico =  " "  THEN
            LET fecha_liquida = "          "
         ELSE
            LET fecha_liquida = p_mae_afi.vfecha_mov_banxico USING "dd/mm/yyyy"
            LET f_val = MDY(MONTH(p_mae_afi.vfecha_mov_banxico),1,
                            YEAR(p_mae_afi.vfecha_mov_banxico))
         END IF

       INITIALIZE sie.*,sie_rcv,sie_ret92,sie_ahorro,sie_apcom,sie_apvol TO NULL
       LET sie_rcv = 0
       LET sie_ret92 = 0
       LET sie_ahorro = 0
       LET sie_apcom = 0
       LET sie_apvol = 0

         DECLARE ap_sie CURSOR FOR
             SELECT m.subcuenta,m.codigo_siefore FROM cta_regimen m
             WHERE  m.nss       = p_mae_afi.nss
             AND    m.subcuenta IN(1,7,10,11,13,15)
         FOREACH ap_sie INTO  sie.*

            INITIALIZE nombre_sie TO NULL
            SELECT razon_social[1,8] INTO nombre_sie from tab_siefore_local
            WHERE codigo_siefore = sie.codigo_siefore

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
                  WHEN 15  LET sie_largop  = nombre_sie
                           LET csie_largop = sie.codigo_siefore
             END CASE
         END FOREACH

##fecha_carta
         LET ho_ra = TIME
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


##SALDOS
         LET cuan_tas = 0
         SELECT COUNT(*) INTO cuan_tas FROM int_ctr_carta
         WHERE  docto_cod = no_carta
         AND    nss       = p_mae_afi.nss
         AND    n_folio   = p_mae_afi.folio
         AND    tipo_solicitud = p_mae_afi.tipo_solicitud
         IF cuan_tas = 0 OR cuan_tas IS NULL THEN
            LET car_gen = 1
         ELSE
            LET car_gen = 2
         END IF

         LET asig.rcv        = 0
         LET asig.rcv_issste = 0
         LET asig.ret92      = 0
         LET asig.ahorro     = 0
         LET asig.apcom      = 0
         LET asig.apvol      = 0
         LET asig.s_inver    = 0
         LET asig.viv97      = 0
         LET asig.viv92      = 0
         LET asig.s_viv      = 0
         LET asig.fondo      = 0
         LET asig.largop     = 0
         LET asig.bono_pen   = 0 #CPL-1802

         SELECT a.afore_desc INTO des_transferente FROM tab_afore a
                WHERE a.afore_cod = p_mae_afi.cve_transferente   

         SELECT a.razon_social[1,30]
         INTO   des_afore_registra
         FROM   tab_afore_local a

         LET v_anio_mov_banxico = YEAR(p_mae_afi.vfecha_mov_banxico)
         LET v_anio_actual =  YEAR(TODAY)
            
         FOR v_dis = 1 TO 8
            CASE v_dis
               WHEN 1  -- asig.rcv
                  LET v_subcuenta = "(1,2,5,6,9)"
               WHEN 2  -- asig.rcv_issste
                  LET v_subcuenta = "(30,31,32,39,19)"
               WHEN 3  -- asig.ret92
                  LET v_subcuenta = "(7)"
               WHEN 4  -- asig.ahorro
                  LET v_subcuenta = "(13)"
               WHEN 5  -- asig.apcom
                  LET v_subcuenta = "(11,12,24,25)"
               WHEN 6  -- asig.apvol
                  LET v_subcuenta = "(3,10,22,23,33,34)"
               WHEN 7  -- asig.fondo
                  LET v_subcuenta = "(14,35)"
               WHEN 8  -- asig.largop
                  LET v_subcuenta = "(15,16,26,27,20,21,28,29,17,18)"
            END CASE

            INITIALIZE lc_query TO NULL
            
            IF v_anio_mov_banxico = v_anio_actual THEN

               LET lc_query = "",
                              "\n SELECT ROUND(SUM(monto_en_pesos),2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND fecha_conversion = '", p_mae_afi.vfecha_mov_banxico, "'",
                              "\n   AND folio 		     = ", p_mae_afi.taa_folio,
                              "\n   AND subcuenta IN", v_subcuenta
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_mov_banxico[3,4]

                  --Se valida que exista la tabla
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                                   "\n SELECT ROUND(SUM(monto_en_pesos),2) ",
                                   "\n   FROM dis_cuenta", v_anio_mov_banxico[3,4],
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND fecha_conversion = '", p_mae_afi.vfecha_mov_banxico, "'",
                                   "\n   AND folio 		     = ", p_mae_afi.taa_folio,
                                   "\n   AND subcuenta IN ", v_subcuenta, ""
                 ELSE
                    LET lc_query = "",
                                    "\n SELECT ROUND(SUM(monto_en_pesos),2) ",
                                    "\n   FROM dis_cuenta",
                                    "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                    "\n   AND fecha_conversion = '", p_mae_afi.vfecha_mov_banxico, "'",
                                    "\n   AND folio 		     = ", p_mae_afi.taa_folio,
                                    "\n   AND subcuenta IN", v_subcuenta
                  END IF

               END IF
               
                 {SELECT ROUND(SUM(bb.monto_en_pesos),2)
                 INTO asig.rcv FROM dis_cuenta bb
                 WHERE bb.nss              = p_mae_afi.nss
                 AND bb.fecha_conversion 	 = p_mae_afi.vfecha_mov_banxico
                 AND bb.folio 						 = p_mae_afi.taa_folio #1261
                 AND bb.subcuenta        IN(1,2,5,6,9)}
               IF v_dis = 1 THEN 
                  PREPARE prp_rcv FROM lc_query
                  EXECUTE prp_rcv INTO asig.rcv

                  IF asig.rcv IS NULL THEN
                     LET asig.rcv = 0
                  END IF
               END IF
               
                 {SELECT ROUND(SUM(bb.monto_en_pesos),2)
                   INTO asig.rcv_issste 
                   FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                   AND bb.fecha_conversion 	 = p_mae_afi.vfecha_mov_banxico 
                   AND bb.folio 						 = p_mae_afi.taa_folio #1261
                   AND bb.subcuenta        IN (30,31,32,39,19)} #CPL-1321 (30,31,32,33,34) #CPL-1802 Se agrega 19
               IF v_dis = 2 THEN                  
                  PREPARE prp_rcv_issste FROM lc_query
                  EXECUTE prp_rcv_issste INTO asig.rcv_issste

                  IF asig.rcv_issste IS NULL THEN
	                 LET asig.rcv_issste = 0
                  END IF
               END IF
                 {SELECT ROUND(SUM(bb.monto_en_pesos),2) 
                 INTO asig.ret92 
                 FROM dis_cuenta bb
                 WHERE bb.nss              = p_mae_afi.nss
                 AND bb.fecha_conversion = p_mae_afi.vfecha_mov_banxico
                 AND bb.folio 						= p_mae_afi.taa_folio #1261
                 AND bb.subcuenta        = 7}
               IF v_dis = 3 THEN
                  PREPARE prp_ret92 FROM lc_query
                  EXECUTE prp_ret92 INTO asig.ret92

                  IF asig.ret92 IS NULL THEN
                     LET asig.ret92 = 0
                  END IF
               END IF
                 {SELECT ROUND(SUM(h.monto_en_pesos),2) 
                 INTO asig.ahorro 
                 FROM dis_cuenta h
                 WHERE h.nss              = p_mae_afi.nss
                 AND h.fecha_conversion = p_mae_afi.vfecha_mov_banxico
                 AND h.folio 					 = p_mae_afi.taa_folio #1261
                 AND h.subcuenta        = 13}
               IF v_dis = 4 THEN
                  PREPARE prp_ahorro FROM lc_query
                  EXECUTE prp_ahorro INTO asig.ahorro

                  IF asig.ahorro IS NULL THEN
                     LET asig.ahorro = 0
                  END IF
               END IF
                 {SELECT ROUND(SUM(n.monto_en_pesos),2) 
                 INTO asig.apcom 
                 FROM dis_cuenta n
                 WHERE n.nss              = p_mae_afi.nss
                 AND n.fecha_conversion = p_mae_afi.vfecha_mov_banxico
                 AND n.folio 					 = p_mae_afi.taa_folio #1261
                 AND n.subcuenta        IN(11,12,24,25)}
               IF v_dis = 5 THEN
                  PREPARE prp_apcom FROM lc_query
                  EXECUTE prp_apcom INTO asig.apcom

                  IF asig.apcom IS NULL THEN
                     LET asig.apcom = 0
                  END IF
               END IF
                 {SELECT ROUND(SUM(n.monto_en_pesos),2) 
                 INTO asig.apvol 
                 FROM dis_cuenta n
                 WHERE n.nss              = p_mae_afi.nss
                 AND n.fecha_conversion = p_mae_afi.vfecha_mov_banxico
                 AND n.folio 					 = p_mae_afi.taa_folio #1261
                 AND n.subcuenta        IN(3,10,22,23,33,34)}
               IF v_dis = 6 THEN
                  PREPARE prp_apvol FROM lc_query
                  EXECUTE prp_apvol INTO asig.apvol

                  IF asig.apvol IS NULL THEN
                     LET asig.apvol = 0
                  END IF
               END IF
                 {SELECT ROUND(SUM(n.monto_en_pesos),2) 
                 INTO asig.fondo 
                 FROM dis_cuenta n
                 WHERE n.nss              = p_mae_afi.nss
                 AND n.fecha_conversion = p_mae_afi.vfecha_mov_banxico
                 AND n.folio 					 = p_mae_afi.taa_folio #1261
                 AND n.subcuenta       IN (14,35)}
               IF v_dis = 7 THEN
                  PREPARE prp_fondo FROM lc_query
                  EXECUTE prp_fondo INTO asig.fondo

                  IF asig.fondo IS NULL THEN
                     LET asig.fondo = 0
                  END IF
               END IF
                 {SELECT ROUND(SUM(n.monto_en_pesos),2) 
                 INTO asig.largop 
                 FROM dis_cuenta n
                 WHERE n.nss              = p_mae_afi.nss
                 AND n.fecha_conversion = p_mae_afi.vfecha_mov_banxico
                 AND n.folio 					 = p_mae_afi.taa_folio #1261
                 AND n.subcuenta        IN (15,16,26,27,20,21,28,29,17,18)}
               IF v_dis = 8 THEN
                  PREPARE prp_largop FROM lc_query
                  EXECUTE prp_largop INTO asig.largop

                  IF asig.largop IS NULL THEN
                     LET asig.largop = 0
                  END IF
               END IF              
         END FOR

         LET f_v = MDY(MONTH(p_mae_afi.vfecha_mov_banxico),1,
                   YEAR(p_mae_afi.vfecha_mov_banxico))
                         
        FOR v_con_fv = 1 TO 3
            CASE v_con_fv
               WHEN 1  -- asig.viv97
                  LET v_subcuentav = 4
               WHEN 2  -- asig.viv92
                  LET v_subcuentav = 8
               WHEN 3  -- asig.bono_pen
                  LET v_subcuentav = 36
            END CASE
            
            LET v_anio_valor = YEAR(f_v)
            LET v_anio_actual =  YEAR(TODAY)

            IF v_anio_valor = v_anio_actual THEN

               LET lc_query = "",
                              "\n SELECT ROUND(SUM(monto_en_pesos),2) ",                                        
                              "\n   FROM dis_cuenta",
                              "\n  WHERE nss = '", p_mae_afi.nss, "'", 
                              "\n    AND fecha_valor = '", f_v, "'",
                              "\n    AND folio = ", p_mae_afi.taa_folio,
                              " \n   AND subcuenta = ", v_subcuentav, "" 
               ELSE
                  LET v_tablav = 'dis_cuenta'||v_anio_valor[3,4]
                  
                  --Se valida que exista la tabla
                  SELECT count(*)
                    INTO v_existe_dis_ctaxxv
                    FROM systables
                   WHERE tabname MATCHES v_tablav
                       
                  IF v_existe_dis_ctaxxv > 0 THEN
                  
                     LET lc_query = "",
                                    "\n SELECT ROUND(SUM(monto_en_pesos),2) ",                                        
                                    "\n   FROM dis_cuenta",v_anio_valor[3,4],
                                    "\n  WHERE nss = '", p_mae_afi.nss, "'", 
                                    "\n    AND fecha_valor = '", f_v, "'",
                                    "\n    AND folio = ", p_mae_afi.taa_folio,
                                    " \n   AND subcuenta = ", v_subcuentav, "" 
                  ELSE
                     LET lc_query = "",
                                    "\n SELECT ROUND(SUM(monto_en_pesos),2) ",                                        
                                    "\n   FROM dis_cuenta",
                                    "\n  WHERE nss = '", p_mae_afi.nss, "'", 
                                    "\n    AND fecha_valor = '", f_v, "'",
                                    "\n    AND folio = ", p_mae_afi.taa_folio,
                                    " \n   AND subcuenta = ", v_subcuentav, "" 
                  END IF
               END IF
         
             {SELECT ROUND(SUM(p.monto_en_pesos),2) 
             INTO asig.viv97   
             FROM dis_cuenta p
             WHERE p.nss              = p_mae_afi.nss
             AND p.fecha_valor      = f_v
             AND p.folio 					 = p_mae_afi.taa_folio #1261
             AND p.subcuenta        = 4}
            IF v_con_fv = 1 THEN
               PREPARE prp_viv97 FROM lc_query
               EXECUTE prp_viv97 INTO asig.viv97
               
               IF asig.viv97 IS NULL THEN
                  LET asig.viv97 = 0
               END IF
            END IF
             {SELECT ROUND(SUM(t.monto_en_pesos),2) 
              INTO asig.viv92  
              FROM dis_cuenta t
              WHERE t.nss              = p_mae_afi.nss
              AND t.fecha_valor = f_v 
              AND t.folio 					= p_mae_afi.taa_folio #1261
              AND t.subcuenta        = 8}
            IF v_con_fv = 2 THEN
               PREPARE prp_viv92 FROM lc_query
               EXECUTE prp_viv92 INTO asig.viv92
            
               IF asig.viv92 IS NULL THEN
                  LET asig.viv92 = 0
               END IF
            END IF
            {SELECT ROUND(SUM(t.monto_en_pesos),2) #CPL-1802 
            INTO asig.bono_pen 
            FROM dis_cuenta t
            WHERE t.nss              = p_mae_afi.nss
            AND t.fecha_valor = f_v 
            AND t.folio 					= p_mae_afi.taa_folio 
            AND t.subcuenta        = 36}
            IF v_con_fv = 3 THEN
               PREPARE prp_bono_pen FROM lc_query
               EXECUTE prp_bono_pen INTO asig.bono_pen
            
               IF asig.bono_pen IS NULL THEN
                  LET asig.bono_pen = 0
               END IF
            END IF

         END FOR
         
         LET asig.s_inver = asig.rcv + asig.ret92 + asig.ahorro +
                            asig.apcom + asig.apvol + asig.largop

         LET asig.s_viv   = asig.viv97 + asig.viv92

         LET ahorro_retiro        = 0
         LET ahorro_retiro_issste = 0
         LET ahorro_vol           = 0
         LET ahorro_viv           = 0
         LET ahorro_viv_issste    = 0
        #CPL-1321 LET ahorro_lplazo        = 0
         LET tot_ahorro           = 0

         LET ahorro_retiro        = asig.rcv + asig.ret92
         LET ahorro_retiro_issste = asig.rcv_issste + asig.ahorro
         LET ahorro_vol           = asig.apcom + asig.apvol + asig.largop #CPL-1321
         LET ahorro_viv           = asig.viv97 + asig.viv92
       #CPL-1321  LET ahorro_lplazo        = asig.largop
         LET ahorro_viv_issste    = asig.fondo
         LET tot_ahorro           = ahorro_retiro + ahorro_vol + ahorro_viv +
                                    ahorro_retiro_issste + ahorro_viv_issste

         LET afore_xx = "                    "
         LET comis_xx = "       "

         LET numero_reg = numero_reg + 1
         LET consecutivo_envio = numero_reg        ## USING "&&&&&&&&"

         LET v_rep  =  "02"                                , "|",  #01
                       no_carta                            , "|",  #02
                       nombres                             , "|",  #03
                       paterno                             , "|",  #04
                       materno                             , "|",  #05
                       calle                               , "|",  #06
                       p_dom.numero                        , "|",  #07
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
                       rfc                                 , "|",  #20
                       p_mae_afi.fbdnsar USING "dd/mm/yyyy", "|",  #21
                       f_elabora                           , "|",  #22
                       folio                               , "|",  #23
                       tipo_proceso                        , "|",  #24
                       p_mae_afi.cve_transferente          , "|",  #25
                       des_transferente                    , "|",  #26
                       nacion                              , "|",  #27
                       fena                                , "|",  #28
                       fecha_liquida                       , "|",  #29
                       f_asigna                            , "|",  #30 
                       ahorro_retiro                       , "|",  #31
                       sie_rcv                             , "|",  #32
                       ahorro_vol                          , "|",  #33
                       sie_apvol                           , "|",  #34
                       ahorro_viv                          , "|",  #35
                       tot_ahorro                          , "|",  #36
                       asig.largop                         , "|",  #37
                       sie_largop                          , "|",  #38
                       des_afore_registra                  , "|",  #39
                       gen_compara.afore_a                 , "|",  #40
                       gen_compara.rend_a                  , "|",  #41
                       gen_compara.comis_a                 , "|",  #42
                       gen_compara.rendn_a                 , "|",  #43
                       gen_compara.afore_b                 , "|",  #44
                       gen_compara.rend_b                  , "|",  #45
                       gen_compara.comis_b                 , "|",  #46
                       gen_compara.rendn_b                 , "|",  #47
                       gen_compara.afore_c                 , "|",  #48
                       gen_compara.rend_c                  , "|",  #49
                       gen_compara.comis_c                 , "|",  #50
                       gen_compara.rendn_c                 , "|",  #51
                       gen_compara.afore_d                 , "|",  #52
                       gen_compara.rend_d                  , "|",  #53
                       gen_compara.comis_d                 , "|",  #54
                       gen_compara.rendn_d                 , "|",  #55
                       gen_compara.afore_e                 , "|",  #56
                       gen_compara.rend_e                  , "|",  #57
                       gen_compara.comis_e                 , "|",  #58
                       gen_compara.rendn_e                 , "|",  #59
                       gen_compara.afore_f                 , "|",  #60
                       gen_compara.rend_f                  , "|",  #61
                       gen_compara.comis_f                 , "|",  #62
                       gen_compara.rendn_f                 , "|",  #63
                       gen_compara.afore_g                 , "|",  #64
                       gen_compara.rend_g                  , "|",  #65
                       gen_compara.comis_g                 , "|",  #66
                       gen_compara.rendn_g                 , "|",  #67
                       gen_compara.afore_h                 , "|",  #68
                       gen_compara.rend_h                  , "|",  #69
                       gen_compara.comis_h                 , "|",  #70
                       gen_compara.rendn_h                 , "|",  #71
                       gen_compara.afore_i                 , "|",  #72
                       gen_compara.rend_i                  , "|",  #73
                       gen_compara.comis_i                 , "|",  #74
                       gen_compara.rendn_i                 , "|",  #75
                       gen_compara.afore_j                 , "|",  #76
                       gen_compara.rend_j                  , "|",  #77
                       gen_compara.comis_j                 , "|",  #78
                       gen_compara.rendn_j                 , "|",  #79
                       gen_compara.afore_k                 , "|",  #80
                       gen_compara.rend_k                  , "|",  #81
                       gen_compara.comis_k                 , "|",  #82
                       gen_compara.rendn_k                 , "|",  #83
                       gen_compara.afore_l                 , "|",  #84
                       gen_compara.rend_l                  , "|",  #85
                       gen_compara.comis_l                 , "|",  #86
                       gen_compara.rendn_l                 , "|",  #87
                       gen_compara.afore_m                 , "|",  #88
                       gen_compara.rend_m                  , "|",  #89
                       gen_compara.comis_m                 , "|",  #90
                       gen_compara.rendn_m                 , "|",  #91
                       gen_compara.afore_n                 , "|",  #92
                       gen_compara.rend_n                  , "|",  #93
                       gen_compara.comis_n                 , "|",  #94
                       gen_compara.rendn_n                 , "|",  #95
                       gen_compara.afore_o                 , "|",  #96
                       gen_compara.rend_o                  , "|",  #97
                       gen_compara.comis_o                 , "|",  #98
                       gen_compara.rendn_o                 , "|",  #99
                       gen_compara.afore_p                 , "|",  #100
                       gen_compara.rend_p                  , "|",  #101
                       gen_compara.comis_p                 , "|",  #102
                       gen_compara.rendn_p                 , "|",  #103
                       gen_compara.afore_q                 , "|",  #104
                       gen_compara.rend_q                  , "|",  #105
                       gen_compara.comis_q                 , "|",  #106
                       gen_compara.rendn_q                 , "|",  #107
                       gen_compara.afore_r                 , "|",  #108
                       gen_compara.rend_r                  , "|",  #109
                       gen_compara.comis_r                 , "|",  #110
                       gen_compara.rendn_r                 , "|",  #111
                       gen_compara.afore_s                 , "|",  #112
                       gen_compara.rend_s                  , "|",  #113
                       gen_compara.comis_s                 , "|",  #114
                       gen_compara.rendn_s                 , "|",  #115
                       gen_compara.afore_t                 , "|",  #116
                       gen_compara.rend_t                  , "|",  #117
                       gen_compara.comis_t                 , "|",  #118
                       gen_compara.rendn_t                 , "|",  #119
                       gen_compara.afore_u                 , "|",  #120
                       gen_compara.rend_u                  , "|",  #121
                       gen_compara.comis_u                 , "|",  #122
                       gen_compara.rendn_u                 , "|",  #123
                       gen_compara.afore_v                 , "|",  #124
                       gen_compara.rend_v                  , "|",  #125
                       gen_compara.comis_v                 , "|",  #126
                       gen_compara.rendn_v                 , "|",  #127
                       gen_compara.afore_w                 , "|",  #128
                       gen_compara.rend_w                  , "|",  #129
                       gen_compara.comis_w                 , "|",  #130
                       gen_compara.rendn_w                 , "|",  #131
                       gen_compara.afore_x                 , "|",  #132
                       gen_compara.rend_x                  , "|",  #133
                       gen_compara.comis_x                 , "|",  #134
                       gen_compara.rendn_x                 , "|",  #135
                       gen_compara.afore_y                 , "|",  #136
                       gen_compara.rend_y                  , "|",  #137
                       gen_compara.comis_y                 , "|",  #138
                       gen_compara.rendn_y                 , "|",  #139
                       gen_compara.afore_z                 , "|",  #140
                       gen_compara.rend_z                  , "|",  #141
                       gen_compara.comis_z                 , "|",  #142
                       gen_compara.rendn_z                 , "|",  #143
                       p_mae_afi.importe_bono              , "|",  #144
                       f_red_bono                          , "|",  #145
                       ahorro_retiro_issste                , "|",  #146
                       ahorro_viv_issste                   , "|",  #147
                       asig.bono_pen                       , "|",  #148 #CPL-1802
                       barras                              , "|",  #149
                       vprom_simple						 , "|"   #149 #CPL-1182
    
##genera arch.
# v_rep = cadena de valores
# prog = 408
# det_cza = 2 (detalle) 1(cabeza) 

     OUTPUT TO REPORT r_report(v_rep,prog,det_cza) 


     SET LOCK MODE TO WAIT
     INSERT INTO int_his_emision_docto VALUES ( prog, p_mae_afi.nss,
                                                fe_carta,
                                                no_carta,
                                                v_rep,
                                                usuario)

        UPDATE int_ctr_carta  
           SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
               int_ctr_carta.fecha_genera = fe_carta,
               int_ctr_carta.hora_genera  = ho_ra ,
               int_ctr_carta.consecutivo  = numero_reg
         WHERE int_ctr_carta.nss = p_mae_afi.nss
           AND int_ctr_carta.fecha_registro = p_mae_afi.vfecha_mov_banxico
           AND int_ctr_carta.docto_cod = 30203
           AND int_ctr_carta.edo_genera   = 10

     INSERT INTO int_constancia VALUES ( folio,
                                         p_mae_afi.nss,
                                         p_mae_afi.tipo_solicitud,
                                         TODAY,
                                         p_mae_afi.vfecha_mov_banxico,
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
FUNCTION cabeza_notifica_09(ref,leyenda_cza,prog,det_cza)

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
    INITIALIZE p_tabafore.*, asig.*, p_dom.*, p_telefono.* TO NULL
    INITIALIZE fecha_creacion, calle, depto, colonia, telefono TO NULL
    INITIALIZE desc_delega, entidad, poblacion, centro_reparto,folio TO NULL
    INITIALIZE paterno, materno, nombres  TO NULL
    INITIALIZE tipo_proceso,des_transferente  TO NULL

    LET i           = 0
    LET domi        = 0
    LET long        = 0
    LET cod_mot     = 0
    LET diferencia  = 0
    LET clave_afore = 0


END FUNCTION
