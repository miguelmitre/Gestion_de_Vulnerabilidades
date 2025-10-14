#*********************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                    #
#Propietario       => E.F.P.                                          #
#Programa          => INTB01010 y INTB0158                            #
#Descripcion       => GLOBALS y FUNCION DETALLE_NOTIFICA, CZA_NOTIFICA#
#Sistema           => INT .                                           #
#Por               => Laura Eugenia Cortes Guzman                     #
#Fecha             => 18 de Agosto del 2005  .                        #
#Modificado por    => Laura Eugenia Cortes Guzman                     #
#Fecha Ult.Modif.  => 26 de Septiembre del 2006                       #
# Req-1182			=> Francisca Schell Rosales 							            #
# Fecha Modif		=> 22/02/2013											                  	#
# Ultima Modif  => 31/01/2017 CPL-2505 Se Añade filtro estad_cod      #
#                  A la Busqueda de la descripcion de la Delegacion   #
#                  Ya que solo la Búsqueda era por municipio delega   #
#                  y se estaba Duplicando.                          	#
#*********************************************************************#
DATABASE safre_af
GLOBALS
    DEFINE p_tabafore               RECORD LIKE tab_afore_local.*,
           hoy_hoy                  DATE,
           fentcons                 DATE,
           fe_carta                 DATE,
           fecha_mov_banxico        DATE,
           fecha_gene_car           DATE,
           f_v                      DATE,
           no_carta                 CHAR(05),
           fecha_creacion           CHAR(10),
           ho_ra, usuario           CHAR(08),

           hora                     CHAR(08),
           hoy                      CHAR(06),
           n_seguro                 CHAR(11),
           enter, opc               CHAR(01),
           leyenda_cza              CHAR(120),
           ref                      CHAR(02),
           fecha                    CHAR(10),
           inter_noti               CHAR(300),
           selec_tab1               CHAR(3000),
           prom_simple 			  DECIMAL(4,2),

           gen_compara    RECORD
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
               afore_y,  afore_z        CHAR(20),

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
               comis_y,  comis_z        CHAR(07),

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

           numero_reg               DECIMAL(10,0),
           consecutivo_envio        DECIMAL(8,0),

           long, i                  INTEGER,
           ban                      INTEGER,
           diferencia               INTEGER,
           consec_lote              INTEGER,

           cod_mot                  SMALLINT,
           clave_afore              SMALLINT,
           codigo_cert              SMALLINT,

           p_mae_afi  RECORD
                  fecha_mov_banxico DATE,           --a.fecha_mov_banxico
                  cve_transferente  CHAR(03),       --a.cve_ced_cuenta
                  ent_transferente  CHAR(03),       --a.cve_recep_cuenta
                  nss               CHAR(11),       --b.n_seguro
                  rfc               CHAR(13),       --b.n_rfc
                  curp              CHAR(18),       --b.n_unico
                  paterno           CHAR(40),       --b.paterno
                  materno           CHAR(40),       --b.materno
                  nombres           CHAR(40),       --b.nombres
                  sexo              SMALLINT,       --b.sexo
                  folio             DECIMAL(10,0),  --b.n_folio
                  fecha_certifica   DATE,           --b.fentcons
                  tipo_solicitud    SMALLINT,       --b.tipo_solicitud
                  fecha_elabora     DATE,           --b.fecha_elaboracion
                  fena              DATE,           --b.fena
                  nacional          CHAR(03),       --b.nacionalidad
                  tip_prob          CHAR(1),        --b.tip_prob
                  fbdnsar           DATE,           --b.fecha_1a_afil
                  importe_bono      DECIMAL(13,2),  --a.importe_bono
                  fecha_red_bono    DATE         ,  --a.fecha_red_bono
                  folio_liq         INTEGER         --a.folio #CPL-20151
           END RECORD,

           reg_carta      RECORD LIKE  int_ctr_carta.*
END GLOBALS
#
# ---------------------------------------------------------------------
# Funcion del detalle

# ---------------------------------------------------------------------
#

FUNCTION detalle_notifica_58(ref,prog,det_cza)
   DEFINE  prog                     SMALLINT,
           det_cza                  SMALLINT,
           cuan_tas                 SMALLINT,
           car_gen                  SMALLINT,
           ref                      CHAR(02),
           indica_formato           CHAR(02),

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
           des_receptora            CHAR(30),

           s_rcv                    DECIMAL(11,2),
           s_rcv_issste             DECIMAL(11,2),
           s_ret92                  DECIMAL(11,2),
           s_ahorro                 DECIMAL(11,2),
           s_apcom                  DECIMAL(11,2),
           s_apvol                  DECIMAL(11,2),
           s_largop                 DECIMAL(11,2),
           s_suma_inversion         DECIMAL(13,2),
           s_viv97                  DECIMAL(11,2),
           s_viv92                  DECIMAL(11,2),
           s_fondo                  DECIMAL(11,2),
           s_suma_viv               DECIMAL(13,2),
           domi                     DECIMAL(10,0),

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
           tipo_proceso             CHAR(01),
           telefono                 CHAR(15),
           barras                   CHAR(28),
           folio                    CHAR(10),
           folio_procesar           DECIMAL(10,0),
           fol_procesar             CHAR(10),
           f_certifica              CHAR(10),
           f_elabora                CHAR(10),
           fena                     CHAR(10),
           f_red_bono               CHAR(10),
           fecha_liquida            CHAR(10),
           paterno                  CHAR(40),
           materno                  CHAR(40),
           nombres                  CHAR(40),
           curp                     CHAR(18),
           rfc                      CHAR(13),
           doc_probatorio           CHAR(40),
           nacion                   CHAR(20),
           espa20                   CHAR(20),
           espa07                   CHAR(07),
           leye01                   CHAR(110),
           leye02                   CHAR(110),
           leye03                   CHAR(110),
           leye04                   CHAR(110),
           leye05                   CHAR(110),
           v_rep                    CHAR(2500),
           f_val                    DATE,
           max_fecha                DATE,
           sol_fentcons             DATE,
           fecha_trasp              CHAR(10),
           tpo_dato_dif             CHAR(40),
           ind_nombre               SMALLINT,

           cat       RECORD
               afore_x                  CHAR(03),
               comis_afo                DECIMAL(5,2)
           END RECORD,

           sum_ahorro_retiro        DECIMAL(13,2),
           sum_ahorro_ret_issste    DECIMAL(13,2),
           sum_ahorro_vol           DECIMAL(13,2),
           sum_ahorro_vivienda      DECIMAL(13,2),
           sum_ahorro_viv_issste    DECIMAL(13,2),
           sum_total                DECIMAL(13,2),
           cve_afore                CHAR(03),

           correo    RECORD LIKE        afi_correo_elect.*,
           c_electronico                CHAR(200),
            vsiefore        SMALLINT,
            vprom_simple	CHAR(07)

            DEFINE 
             v_anio_mov_banxico,
             v_anio_actual        CHAR(4),
             v_tabla              CHAR(13),             
             v_existe_dis_ctaxx   INTEGER,
             v_subcuenta          CHAR(35),
             v_dis, v_dis2        INTEGER,
             v_con_fv             INTEGER,
             lc_query             CHAR(3000)


   INITIALIZE p_tabafore.*, p_dom.*, p_telefono.*  TO NULL
   INITIALIZE fecha_creacion, calle, depto, colonia, telefono TO NULL
   INITIALIZE desc_delega, entidad, poblacion, centro_reparto,folio TO NULL
   INITIALIZE paterno, materno, nombres,fena, nacion  TO NULL
   INITIALIZE tipo_proceso,des_receptora,des_transferente,rfc  TO NULL
   INITIALIZE v_anio_mov_banxico, v_anio_actual, v_tabla, v_subcuenta TO NULL

   LET i        = 0   LET domi       = 0   LET long        = 0
   LET cod_mot  = 0   LET diferencia = 0   LET clave_afore = 0

   PREPARE apt_58 FROM selec_tab1
   DECLARE cur_58 CURSOR FOR apt_58
   FOREACH cur_58 INTO p_mae_afi.*

   CALL llena_comision_tras(p_mae_afi.fecha_mov_banxico, p_mae_afi.nss) RETURNING gen_compara.*,ban

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
      WHERE p_mae_afi.fecha_mov_banxico BETWEEN a.fecha_ini AND a.fecha_fin
      AND a.siefore_cod = vsiefore

	   LET vprom_simple    = prom_simple USING "##&.&&","%" #CPL-1182

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
            p_mae_afi.rfc[1] = " "  THEN
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
                WHERE s.nss = p_mae_afi.nss
                AND   s.n_folio = p_mae_afi.folio
                AND   s.tipo_solicitud = p_mae_afi.tipo_solicitud
                AND   s.marca_envio    = "X"

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
                         AND q.estad_cod = p_dom.estado #CPL-2505
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

         IF p_mae_afi.fecha_mov_banxico IS NULL OR

            p_mae_afi.fecha_mov_banxico =  " "  THEN
            LET fecha_liquida = "          "
         ELSE
            LET fecha_liquida = p_mae_afi.fecha_mov_banxico USING "dd/mm/yyyy"
            LET f_val = MDY(MONTH(p_mae_afi.fecha_mov_banxico),1,
                            YEAR(p_mae_afi.fecha_mov_banxico))
         END IF
##fecha_carta
         LET fe_carta = TODAY USING "mm/dd/yyyy"
         LET ho_ra    = TIME
##entidad
         LET entidad = "    "
##fecha generacion de la carta
         LET fecha_creacion = TODAY USING "dd/mm/yyyy"
         LET fecha_barra    = TODAY USING "yyyymmdd"
##no_carta
         LET no_carta = "302",ref
##codigo_barras

         LET barras = "C",no_carta,"000",p_mae_afi.nss,
##                      fecha_barra
                      p_mae_afi.fecha_mov_banxico USING "YYYYMMDD"

##siefore
       INITIALIZE sie.*,sie_rcv,sie_ret92,sie_ahorro,sie_apcom,sie_apvol TO NULL
       LET sie_rcv = 0
       LET sie_ret92 = 0
       LET sie_ahorro = 0
       LET sie_apcom = 0
       LET sie_apvol = 0
       LET sie_largop = 0

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
##montos
         LET s_rcv = 0

         ------------------------------------------------------------------------
        LET v_anio_mov_banxico = YEAR(p_mae_afi.fecha_mov_banxico)
         LET v_anio_actual =  YEAR(TODAY)
            
         FOR v_dis = 1 TO 8
            CASE v_dis
               WHEN 1  -- s_rcv
                  LET v_subcuenta = "(1,2,5,6,9)"
               WHEN 2  -- s_rcv_issste
                  LET v_subcuenta = "(30,31,32,33,34)"
               WHEN 3  -- s_ret92
                  LET v_subcuenta = "(7)"
               WHEN 4  -- s_ahorro
                  LET v_subcuenta = "(13)"
               WHEN 5  -- s_apcom
                  LET v_subcuenta = "(11,12,24,25)"
               WHEN 6  -- s_apvol
                  LET v_subcuenta = "(3,10,22,23)"
               WHEN 7  -- s_largpp
                  LET v_subcuenta = "(15,16,26,27,20,21,28,29)"
            END CASE

            INITIALIZE lc_query TO NULL
            
            IF v_anio_mov_banxico = v_anio_actual THEN

               LET lc_query = "",
                                    "\n SELECT ROUND(SUM(monto_en_pesos),2) ",
                                    "\n   FROM dis_cuenta",
                                    "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                    "\n   AND folio 		     = ", p_mae_afi.folio_liq,
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
                                   "\n   AND folio 		     = ", p_mae_afi.folio_liq,
                                   "\n   AND subcuenta IN", v_subcuenta
                 ELSE
                    LET lc_query = "",
                                    "\n SELECT ROUND(SUM(monto_en_pesos),2) ",
                                    "\n   FROM dis_cuenta",
                                    "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                    "\n   AND folio 		     = ", p_mae_afi.folio_liq,
                                    "\n   AND subcuenta IN", v_subcuenta
                  END IF

               END IF
         ------------------------------------------------------------------------
         {SELECT ROUND(SUM(bb.monto_en_pesos),2) INTO s_rcv FROM dis_cuenta bb
                WHERE bb.nss       = p_mae_afi.nss
                  AND bb.folio = p_mae_afi.folio_liq #CPL-2051
                  AND bb.subcuenta IN(1,2,5,6,9)}
         IF v_dis = 1 THEN 
            PREPARE prp_rcv FROM lc_query
            EXECUTE prp_rcv INTO s_rcv

            IF s_rcv IS NULL THEN
               LET s_rcv = 0
            END IF
         END IF

         LET s_rcv_issste = 0
         {SELECT ROUND(SUM(bb.monto_en_pesos),2) INTO s_rcv_issste FROM dis_cuenta bb
                WHERE bb.nss       = p_mae_afi.nss
                  AND bb.folio = p_mae_afi.folio_liq #CPL-2051
                  AND bb.subcuenta IN(30,31,32,33,34)}
         IF v_dis = 2 THEN                  
            PREPARE prp_rcv_issste FROM lc_query
            EXECUTE prp_rcv_issste INTO s_rcv_issste

            IF s_rcv_issste IS NULL THEN
               LET s_rcv_issste = 0
            END IF
         END IF

         LET s_ret92 = 0
         {SELECT ROUND(SUM(h.monto_en_pesos),2) INTO s_ret92 FROM dis_cuenta h
                WHERE h.nss       = p_mae_afi.nss
                  AND h.folio = p_mae_afi.folio_liq #CPL-2051
                  AND h.subcuenta = 7}
         IF v_dis = 3 THEN
            PREPARE prp_ret92 FROM lc_query
            EXECUTE prp_ret92 INTO s_ret92

            IF s_ret92 IS NULL THEN
               LET s_ret92 = 0
            END IF
         END IF

         LET s_ahorro = 0
         {SELECT ROUND(SUM(n.monto_en_pesos),2) INTO s_ahorro FROM dis_cuenta n
                WHERE n.nss       = p_mae_afi.nss
                  AND n.folio = p_mae_afi.folio_liq #CPL-2051
                  AND n.subcuenta = 13}
         IF v_dis = 4 THEN
            PREPARE prp_ahorro FROM lc_query
            EXECUTE prp_ahorro INTO s_ahorro

            IF s_ahorro IS NULL THEN
               LET s_ahorro = 0
            END IF
         END IF

         LET s_apcom = 0
         {SELECT ROUND(SUM(n.monto_en_pesos),2) INTO s_apcom FROM dis_cuenta n
                WHERE n.nss       = p_mae_afi.nss
                  AND n.folio = p_mae_afi.folio_liq #CPL-2051
                  AND n.subcuenta IN(11,12,24,25)}
         IF v_dis = 5 THEN
            PREPARE prp_apcom FROM lc_query
            EXECUTE prp_apcom INTO s_apcom

            IF s_apcom IS NULL THEN
               LET s_apcom = 0
            END IF
         END IF

         LET s_apvol = 0
         {SELECT ROUND(SUM(n.monto_en_pesos),2) INTO s_apvol FROM dis_cuenta n
                WHERE n.nss       = p_mae_afi.nss
                  AND n.folio = p_mae_afi.folio_liq #CPL-2051
                  AND n.subcuenta in(3,10,22,23)}
         IF v_dis = 6 THEN
            PREPARE prp_apvol FROM lc_query
            EXECUTE prp_apvol INTO s_apvol

            IF s_apvol IS NULL THEN
               LET s_apvol = 0
            END IF
         END IF

         LET s_largop = 0
         {SELECT ROUND(SUM(n.monto_en_pesos),2) INTO s_largop FROM dis_cuenta n
                WHERE n.nss       = p_mae_afi.nss
                  AND n.folio = p_mae_afi.folio_liq #CPL-2051
                  AND n.subcuenta in(15,16,26,27,20,21,28,29)}
         IF v_dis = 7 THEN
            PREPARE prp_largop FROM lc_query
            EXECUTE prp_largop INTO s_largop

            IF s_largop IS NULL THEN
               LET s_largop = 0
            END IF
         END IF

         END FOR 
         
         LET s_suma_inversion = 0
         LET s_suma_inversion = s_rcv  +  s_ret92  +  s_ahorro  +
                                s_apcom + s_apvol

         LET f_v = MDY(MONTH(p_mae_afi.fecha_mov_banxico), 1,
                   YEAR(p_mae_afi.fecha_mov_banxico))

         FOR v_dis2 = 1 TO 3
            CASE v_dis2
               WHEN 1  -- s_viv97
                  LET v_subcuenta = "(4)"
               WHEN 2  -- s_viv92
                  LET v_subcuenta = "(8)"
               WHEN 3  -- s_fondo
                  LET v_subcuenta = "(14,35)"
            END CASE

            INITIALIZE lc_query TO NULL
            
            IF v_anio_mov_banxico = v_anio_actual THEN

               LET lc_query = "",
                                    "\n SELECT ROUND(SUM(monto_en_pesos),2) ",
                                    "\n   FROM dis_cuenta",
                                    "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                    "\n   AND folio 		     = ", p_mae_afi.folio_liq,
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
                                   "\n   AND folio 		     = ", p_mae_afi.folio_liq,
                                   "\n   AND subcuenta IN", v_subcuenta
                 ELSE
                    LET lc_query = "",
                                    "\n SELECT ROUND(SUM(monto_en_pesos),2) ",
                                    "\n   FROM dis_cuenta",
                                    "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                    "\n   AND folio 		     = ", p_mae_afi.folio_liq,
                                    "\n   AND subcuenta IN", v_subcuenta
                  END IF

               END IF
               
         LET s_viv97 = 0
         {SELECT ROUND(SUM(p.monto_en_pesos),2) INTO s_viv97 FROM dis_cuenta p
                WHERE p.nss       = p_mae_afi.nss
                  --AND p.fecha_valor = p_mae_afi.fecha_mov_banxico  ---f_v
                  AND p.folio = p_mae_afi.folio_liq #CPL-2051
                  AND p.subcuenta IN(4)}

         IF v_dis2 = 1 THEN
            PREPARE prp_viv97 FROM lc_query
            EXECUTE prp_viv97 INTO s_viv97

            IF s_viv97 IS NULL THEN
               LET s_viv97 = 0
            END IF
         END IF

         LET s_viv92 = 0
         {SELECT ROUND(SUM(p.monto_en_pesos),2) INTO s_viv92 FROM dis_cuenta p
                WHERE p.nss       = p_mae_afi.nss
                  --AND p.fecha_valor = f_v
                  AND p.folio = p_mae_afi.folio_liq #CPL-2051
                  AND p.subcuenta IN(8)}

         IF v_dis2 = 2 THEN
            PREPARE prp_viv92 FROM lc_query
            EXECUTE prp_viv92 INTO s_viv92

            IF s_viv92 IS NULL THEN
               LET s_viv92 = 0
            END IF
         END IF

         LET s_fondo = 0
         {SELECT ROUND(SUM(t.monto_en_pesos),2) INTO s_fondo  FROM dis_cuenta t
                   WHERE t.nss       = p_mae_afi.nss
                     AND t.folio = p_mae_afi.folio_liq #CPL-2051
                     AND t.subcuenta in(14,35)}

         IF v_dis2 = 3 THEN
            PREPARE prp_fondo FROM lc_query
            EXECUTE prp_fondo INTO s_fondo

            IF s_fondo IS NULL THEN
               LET s_fondo = 0
            END IF
         END IF

         END FOR
         
         LET s_suma_viv = 0
         LET s_suma_viv = s_viv97  +  s_viv92

         LET sum_ahorro_retiro        = 0
         LET sum_ahorro_ret_issste    = 0
         LET sum_ahorro_vol           = 0
         LET sum_ahorro_vivienda      = 0
         LET sum_ahorro_viv_issste    = 0
         LET sum_total                = 0

         LET sum_ahorro_retiro        = s_rcv + s_ret92
         LET sum_ahorro_ret_issste    = s_rcv_issste + s_ahorro
         LET sum_ahorro_vol           = s_apvol + s_apcom + s_largop
         LET sum_ahorro_vivienda      = s_viv97 + s_viv92
         LET sum_ahorro_viv_issste    = s_fondo
         LET sum_total                = sum_ahorro_retiro     +
                                        sum_ahorro_ret_issste +
                                        sum_ahorro_vol        +
                                        sum_ahorro_vivienda   +
                                        sum_ahorro_viv_issste

## comienza cir-28-8

         SELECT NVL(m.documento_2,0)
         INTO   ind_nombre
         FROM   afi_mae_afiliado m
         WHERE  m.n_seguro = p_mae_afi.nss

         IF STATUS = NOTFOUND THEN
            LET indica_formato = "FA"
            LET tpo_dato_dif = '                                        '
         ELSE
            IF ind_nombre = 0 OR
               ind_nombre IS NULL OR
               ind_nombre = " "   OR
               ind_nombre = 1     THEN
               LET indica_formato = "FA"
               LET tpo_dato_dif = '                                        '
            ELSE
                 LET indica_formato = "FB"
                 LET tpo_dato_dif = 'DEL NOMBRE                              '
            END IF
         END IF

         SELECT docprob_desc
         INTO   doc_probatorio
         FROM   tab_doc_prob
         WHERE  docprob_cod = p_mae_afi.tip_prob

         IF doc_probatorio  IS NULL OR
            doc_probatorio  =  " "  THEN
            LET doc_probatorio = "                                        "
         ELSE
              LET long = 0
              LET i    = 0
              LET long = LENGTH(doc_probatorio)

              IF long < 40 THEN
                 LET long = long + 1
                 FOR i = long TO 40
                     LET doc_probatorio[i,i] = " "
                 END FOR
              END IF
         END IF

         SELECT a.nacionalidad
         INTO   nacion
         FROM   tab_nacionalidad a
         WHERE  a.codigo_pais = p_mae_afi.nacional

         LET long = 0
         LET i    = 0
         LET long = LENGTH(nacion)
         IF long < 20 THEN
            LET long = long + 1
            FOR i = long TO 20
                LET nacion[i,i] = " "
            END FOR
         END IF

         LET espa20 = "                    "
         LET espa07 = "       "

         LET fena = p_mae_afi.fena USING "dd/mm/yyyy"


         IF indica_formato = "FB" THEN

              INITIALIZE leye01, leye02, leye03, leye04, leye05 TO NULL

              LET leye01="Se detecto que el nombre con el que Usted esta reg",
                         "istrado en la Base de Datos Nacional SAR es difere",
                         "nte al del"

              LET leye02=doc_probatorio CLIPPED

              LET leye03="que   Usted presento al momento de solicitar el tr",
                         "aspaso de la cuenta, por lo que le sugerimos acudi",
                         "ir  a  las"
              LET leye04="oficinas  de  esta Administradora para modificar s",
                         "us datos, a efecto de evitarle problemas al moment",
                         "o. de  su "
              LET leye05="retiro. "
         ELSE
            LET leye01="                                                  ",
                       "                                                  ",
                       "          "
            LET leye02=leye01
            LET leye03=leye01
            LET leye04=leye01
            LET leye05=leye01
         END IF

## termina circ-28-8 y comienza la c-28-11
         INITIALIZE c_electronico TO NULL
         DECLARE apt_c_elec CURSOR FOR
             SELECT n.* FROM afi_correo_elect n
                    WHERE n.nss            = p_mae_afi.nss
                      AND n.n_folio        = p_mae_afi.folio
                      AND n.tipo_solicitud = p_mae_afi.tipo_solicitud
                      AND n.cod_correo_e   IN("2","1")
                      AND n.marca_envio = "X"
         FOREACH apt_c_elec INTO correo.*
                 LET c_electronico = c_electronico CLIPPED,
                                     correo.correo_elect CLIPPED,
                                     ";"
         END FOREACH

         LET long = 0
         LET i    = 0
         LET long = LENGTH(c_electronico)
         IF long < 200 THEN
            LET long = long + 1
            FOR i = long TO 200
                LET c_electronico[i,i] = " "
            END FOR
         END IF

         INITIALIZE folio_procesar, fol_procesar TO NULL
         SELECT n.folio_sol INTO folio_procesar FROM afi_det_internet n
                WHERE n.nss     = p_mae_afi.nss
                AND   n.n_folio   = p_mae_afi.folio

         IF folio_procesar = 0 THEN
               LET fol_procesar = "0000000000"
         ELSE
                 LET fol_procesar = folio_procesar USING "&&&&&&&&&&"
         END IF


## Entidad Transferente
         SELECT a.afore_desc INTO des_transferente FROM tab_afore a
                WHERE a.afore_cod = p_mae_afi.cve_transferente

         LET long = 0
         LET i    = 0
         LET long = LENGTH(des_transferente)
         IF long < 30 THEN
            LET long = long + 1
            FOR i = long TO 30
                LET des_transferente[i,i] = " "
            END FOR
         END IF

         SELECT ee.codigo_afore INTO cve_afore FROM tab_afore_local ee

         SELECT a.afore_desc INTO des_receptora    FROM tab_afore a
                WHERE a.afore_cod = cve_afore

         LET long = 0
         LET i    = 0
         LET long = LENGTH(des_receptora)
         IF long < 30 THEN
            LET long = long + 1
            FOR i = long TO 30
                LET des_receptora[i,i] = " "
            END FOR
         END IF

###GENERA archivo

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
                       p_mae_afi.fbdnsar USING "dd/mm/yyyy", "|",  #20
                       f_elabora                           , "|",  #21
                       fol_procesar                        , "|",  #22
                       tipo_proceso                        , "|",  #23
                       p_mae_afi.cve_transferente          , "|",  #24
                       des_transferente                    , "|",  #25
                       s_rcv                               , "|",  #26
                       s_ret92                             , "|",  #27
                       s_ahorro                            , "|",  #28
                       s_apcom                             , "|",  #29
                       s_apvol                             , "|",  #30
                       s_suma_inversion                    , "|",  #31
                       sie_rcv                             , "|",  #32
                       sie_ret92                           , "|",  #33
                       sie_ahorro                          , "|",  #34
                       sie_apcom                           , "|",  #35
                       sie_apvol                           , "|",  #36
                       s_viv97                             , "|",  #37
                       s_viv92                             , "|",  #38
                       s_fondo                             , "|",  #39
                       s_suma_viv                          , "|",  #40
                       rfc                                 , "|",  #41
                       f_elabora                           , "|",  #42
                       p_mae_afi.fecha_mov_banxico USING "dd/mm/yyyy" , "|", #43
                       fena                                , "|",  #44
                       nacion                              , "|",  #45
                       doc_probatorio                      , "|",  #46
                       gen_compara.afore_a                 , "|",  #47
                       gen_compara.rend_a                  , "|",  #48
                       gen_compara.comis_a                 , "|",  #49
                       gen_compara.rendn_a                 , "|",  #50
                       gen_compara.afore_b                 , "|",  #51
                       gen_compara.rend_b                  , "|",  #52
                       gen_compara.comis_b                 , "|",  #53
                       gen_compara.rendn_b                 , "|",  #54
                       gen_compara.afore_c                 , "|",  #55
                       gen_compara.rend_c                  , "|",  #56
                       gen_compara.comis_c                 , "|",  #57
                       gen_compara.rendn_c                 , "|",  #58
                       gen_compara.afore_d                 , "|",  #59
                       gen_compara.rend_d                  , "|",  #60
                       gen_compara.comis_d                 , "|",  #61
                       gen_compara.rendn_d                 , "|",  #62
                       gen_compara.afore_e                 , "|",  #63
                       gen_compara.rend_e                  , "|",  #64
                       gen_compara.comis_e                 , "|",  #65
                       gen_compara.rendn_e                 , "|",  #66
                       gen_compara.afore_f                 , "|",  #67
                       gen_compara.rend_f                  , "|",  #68
                       gen_compara.comis_f                 , "|",  #69
                       gen_compara.rendn_f                 , "|",  #70
                       gen_compara.afore_g                 , "|",  #71
                       gen_compara.rend_g                  , "|",  #72
                       gen_compara.comis_g                 , "|",  #73
                       gen_compara.rendn_g                 , "|",  #74
                       gen_compara.afore_h                 , "|",  #75
                       gen_compara.rend_h                  , "|",  #76
                       gen_compara.comis_h                 , "|",  #77
                       gen_compara.rendn_h                 , "|",  #78
                       gen_compara.afore_i                 , "|",  #79
                       gen_compara.rend_i                  , "|",  #80
                       gen_compara.comis_i                 , "|",  #81
                       gen_compara.rendn_i                 , "|",  #82
                       gen_compara.afore_j                 , "|",  #83
                       gen_compara.rend_j                  , "|",  #84
                       gen_compara.comis_j                 , "|",  #85
                       gen_compara.rendn_j                 , "|",  #86
                       gen_compara.afore_k                 , "|",  #87
                       gen_compara.rend_k                  , "|",  #88
                       gen_compara.comis_k                 , "|",  #89
                       gen_compara.rendn_k                 , "|",  #90
                       gen_compara.afore_l                 , "|",  #91
                       gen_compara.rend_l                  , "|",  #92
                       gen_compara.comis_l                 , "|",  #93
                       gen_compara.rendn_l                 , "|",  #94
                       gen_compara.afore_m                 , "|",  #95
                       gen_compara.rend_m                  , "|",  #96
                       gen_compara.comis_m                 , "|",  #97
                       gen_compara.rendn_m                 , "|",  #98
                       gen_compara.afore_n                 , "|",  #99
                       gen_compara.rend_n                  , "|",  #100
                       gen_compara.comis_n                 , "|",  #101
                       gen_compara.rendn_n                 , "|",  #102
                       gen_compara.afore_o                 , "|",  #103
                       gen_compara.rend_o                  , "|",  #104
                       gen_compara.comis_o                 , "|",  #105
                       gen_compara.rendn_o                 , "|",  #106
                       gen_compara.afore_p                 , "|",  #107
                       gen_compara.rend_p                  , "|",  #108
                       gen_compara.comis_p                 , "|",  #109
                       gen_compara.rendn_p                 , "|",  #110
                       gen_compara.afore_q                 , "|",  #111
                       gen_compara.rend_q                  , "|",  #112
                       gen_compara.comis_q                 , "|",  #113
                       gen_compara.rendn_q                 , "|",  #114
                       gen_compara.afore_r                 , "|",  #115
                       gen_compara.rend_r                  , "|",  #116
                       gen_compara.comis_r                 , "|",  #117
                       gen_compara.rendn_r                 , "|",  #118
                       gen_compara.afore_s                 , "|",  #119
                       gen_compara.rend_s                  , "|",  #120
                       gen_compara.comis_s                 , "|",  #121
                       gen_compara.rendn_s                 , "|",  #122
                       gen_compara.afore_t                 , "|",  #123
                       gen_compara.rend_t                  , "|",  #124
                       gen_compara.comis_t                 , "|",  #125
                       gen_compara.rendn_t                 , "|",  #126
                       gen_compara.afore_u                 , "|",  #127
                       gen_compara.rend_u                  , "|",  #128
                       gen_compara.comis_u                 , "|",  #129
                       gen_compara.rendn_u                 , "|",  #130
                       gen_compara.afore_v                 , "|",  #131
                       gen_compara.rend_v                  , "|",  #132
                       gen_compara.comis_v                 , "|",  #133
                       gen_compara.rendn_v                 , "|",  #134
                       gen_compara.afore_w                 , "|",  #135
                       gen_compara.rend_w                  , "|",  #136
                       gen_compara.comis_w                 , "|",  #137
                       gen_compara.rendn_w                 , "|",  #138
                       gen_compara.afore_x                 , "|",  #139
                       gen_compara.rend_x                  , "|",  #140
                       gen_compara.comis_x                 , "|",  #141
                       gen_compara.rendn_x                 , "|",  #142
                       gen_compara.afore_y                 , "|",  #143
                       gen_compara.rend_y                  , "|",  #144
                       gen_compara.comis_y                 , "|",  #145
                       gen_compara.rendn_y                 , "|",  #146
                       gen_compara.afore_z                 , "|",  #147
                       gen_compara.rend_z                  , "|",  #148
                       gen_compara.comis_z                 , "|",  #149
                       gen_compara.rendn_z                 , "|",  #150
                       indica_formato                      , "|",  #151
                       sum_ahorro_retiro                   , "|",  #152
                       sum_ahorro_vol                      , "|",  #153
                       sum_ahorro_vivienda                 , "|",  #154
                       sum_total                           , "|",  #155
                       cve_afore                           , "|",  #156
                       des_receptora                       , "|",  #157
                       c_electronico                       , "|",  #158
                       tpo_dato_dif                        , "|",  #159
                       p_mae_afi.importe_bono              , "|",  #160
                       f_red_bono                          , "|",  #161
                       sum_ahorro_ret_issste               , "|",  #162
                       sum_ahorro_viv_issste               , "|",  #163
                       barras                              , "|",  #164
                       vprom_simple						 , "|"   #165 #CPL-1182

##genera arch.
     LET hora = TIME

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
               int_ctr_carta.hora_genera  = hora ,
               int_ctr_carta.consecutivo  = numero_reg
        WHERE int_ctr_carta.nss = p_mae_afi.nss
          AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_mov_banxico
          AND int_ctr_carta.docto_cod    = 30280
          AND int_ctr_carta.edo_genera   = 10

     INSERT INTO int_constancia VALUES ( folio,
                                         p_mae_afi.nss,
                                         p_mae_afi.tipo_solicitud,
                                         TODAY,
                                         p_mae_afi.fecha_certifica,
                                         no_carta )

     SET LOCK MODE TO NOT WAIT

     DISPLAY "REGISTROS : ",numero_reg USING "####&" AT 17,1

     INITIALIZE p_tabafore.*, p_dom.*, p_telefono.* TO NULL
     INITIALIZE fecha_creacion, calle, depto, colonia, telefono TO NULL
     INITIALIZE desc_delega, entidad, poblacion, centro_reparto,folio TO NULL
     INITIALIZE paterno, materno, nombres,fena, nacion  TO NULL
     INITIALIZE tipo_proceso,des_transferente, sol_fentcons, rfc  TO NULL

     LET i        = 0   LET domi       = 0   LET long        = 0
     LET cod_mot  = 0   LET diferencia = 0   LET clave_afore = 0

   END FOREACH
   DISPLAY "REGISTROS : ",numero_reg USING "####&" AT 17,1
   FINISH REPORT r_report
END FUNCTION
#
#---------------------------------------------------------
# Generando la cabeza
#---------------------------------------------------------
#
FUNCTION cabeza_notifica_58(ref,leyenda_cza,prog,det_cza)

   DEFINE  cve_afore   SMALLINT,
           afore_local CHAR(03),
           ref         CHAR(02),
           leyenda_cza CHAR(120),
           v_rep       CHAR(2200),
           prog        SMALLINT,
           det_cza     SMALLINT

   INITIALIZE afore_local, fecha_creacion TO NULL
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

   LET fecha_creacion = TODAY USING "MM/DD/YYYY"

   INSERT INTO int_notifica_folio
               VALUES (consec_lote, fecha_creacion, ref, numero_reg)

   FINISH REPORT r_report
END FUNCTION

