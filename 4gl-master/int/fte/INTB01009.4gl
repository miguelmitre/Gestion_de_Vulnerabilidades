#*********************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                    #
#Propietario       => E.F.P.                                          #
#Programa          => INTB01009  INTB0107 INTB0106                    #
#Descripcion       => GLOBALS y FUNCION DETALLE_NOTIFICA, CZA_NOTIFICA#
#Sistema           => INT .                                           #
#Fecha Elaboracion => 20 de Diciembre del 2004                        #
#Elaborado por     => Laura Eugenia Cortes Guzman                     #
#Fecha Ult.Modif.  => 20 de Diciembre del 2004                        #
#Modifado por      => Laura Eugenia Cortes Guzman                     #
# Req-1182			=> Francisca Schell Rosales 							            #
# Fecha Modif		=> 22/02/2013												                  #
#CPL-3045          => FRANCISCA SCHELL **SE AGREGAN NUEVAS SIEFORES   #
#*********************************************************************#
DATABASE safre_af
GLOBALS
    DEFINE p_tabafore               RECORD LIKE tab_afore_local.*,
           hoy_hoy                  DATE,
           fe_carta                 DATE,
           fentcons                 DATE,
           finicta                  DATE,
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
           fecha_liquida            CHAR(10),
           paterno                  CHAR(40),
           materno                  CHAR(40),
           nombres                  CHAR(40),
           curp                     CHAR(18),
           rfc                      CHAR(13),
           desc_motivo              CHAR(70),
           status_proceso           CHAR(01),
           inter_noti               CHAR(300),
           selec_tab                CHAR(2000),

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

           des_transferente         CHAR(30),
           precio            DECIMAL(19,14),
           precio_1          DECIMAL(19,14),
           precio_2          DECIMAL(19,14),
           precio_3          DECIMAL(19,14),             ---mas siefores
           precio_4          DECIMAL(19,14),             ---mas siefores
           precio_5          DECIMAL(19,14),             ---mas siefores
           precio_14         DECIMAL(19,14), #INV-5412
           precio_15         DECIMAL(19,14), #INV-5412
           precio_16         DECIMAL(19,14), #INV-5412
           precio_17         DECIMAL(19,14), #INV-5412
           precio_18         DECIMAL(19,14), #INV-5412
           precio_19         DECIMAL(19,14), #INV-5412
           precio_20         DECIMAL(19,14), #INV-5412
           precio_21         DECIMAL(19,14), #INV-5412
           precio_22         DECIMAL(19,14), #INV-5412           

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

           reg_carta      RECORD LIKE  int_ctr_carta.*,

           afore_xx               CHAR(20),
           comis_xx               CHAR(07),
           nacion                 CHAR(25),
		   prom_simple 			  DECIMAL(4,2),
		   vsiefore        SMALLINT,
          vprom_simple	CHAR(08),

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
               afore_y,  afore_z       CHAR(20),

               rend_a,   rend_b,
               rend_c,   rend_d,
               rend_e,   rend_f,
               rend_g,   rend_h,
               rend_i,   rend_j,
               rend_k,   rend_l,
               rend_m,   rend_n,
               rend_o,   rend_p,
               rend_q,   rend_r,
               rend_s,   rend_t,
               rend_u,   rend_v,
               rend_w,   rend_x,
               rend_y,   rend_z       CHAR(06),

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

               rendneto_a, rendneto_b,
               rendneto_c, rendneto_d,
               rendneto_e, rendneto_f,
               rendneto_g, rendneto_h,
               rendneto_i, rendneto_j,
               rendneto_k, rendneto_l,
               rendneto_m, rendneto_n,
               rendneto_o, rendneto_p,
               rendneto_q, rendneto_r,
               rendneto_s, rendneto_t,
               rendneto_u, rendneto_v,
               rendneto_w, rendneto_x,
               rendneto_y, rendneto_z   CHAR(06)
           END RECORD,

           ho_ra, usuario           CHAR(8),
           diferencia               INTEGER,
           fecha                    CHAR(10),
           v_rep                    CHAR(2200),
           f_val, f_v               DATE

END GLOBALS
#
# ---------------------------------------------------------------------
# Funcion del detalle
# ---------------------------------------------------------------------
#

FUNCTION detalle_notifica9(ref,prog,det_cza)
   DEFINE prog     SMALLINT,
          det_cza  SMALLINT,
          ref      CHAR(02),
          csie_rcv     SMALLINT,
          csie_ret92   SMALLINT,
          csie_ahorro  SMALLINT,
          csie_apcom   SMALLINT,
          csie_apvol   SMALLINT,
          csie_largop  SMALLINT,

          nombre_sie   CHAR(08),
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

          dia_hoy          DATE,

          rcv_1             DECIMAL(11,2),
          ret92_1           DECIMAL(11,2),
          ahorro_1          DECIMAL(11,2),
          apcom_1           DECIMAL(11,2),
          apvol_1           DECIMAL(11,2),
          viv97_1           DECIMAL(11,2),
          viv92_1           DECIMAL(11,2),
          fondo_1           DECIMAL(11,2),
          largop_1          DECIMAL(11,2),

          rcv_2             DECIMAL(11,2),
          ret92_2           DECIMAL(11,2),
          ahorro_2          DECIMAL(11,2),
          apcom_2           DECIMAL(11,2),
          apvol_2           DECIMAL(11,2),
          viv97_2           DECIMAL(11,2),
          viv92_2           DECIMAL(11,2),
          fondo_2           DECIMAL(11,2),
          largop_2          DECIMAL(11,2),

--->mas siefores
          rcv_3             DECIMAL(11,2),
          ret92_3           DECIMAL(11,2),
          ahorro_3          DECIMAL(11,2),
          apcom_3           DECIMAL(11,2),
          apvol_3           DECIMAL(11,2),
          viv97_3           DECIMAL(11,2),
          viv92_3           DECIMAL(11,2),
          fondo_3           DECIMAL(11,2),
          largop_3          DECIMAL(11,2),

          rcv_4             DECIMAL(11,2),
          ret92_4           DECIMAL(11,2),
          ahorro_4          DECIMAL(11,2),
          apcom_4           DECIMAL(11,2),
          apvol_4           DECIMAL(11,2),
          viv97_4           DECIMAL(11,2),
          viv92_4           DECIMAL(11,2),
          fondo_4           DECIMAL(11,2),
          largop_4          DECIMAL(11,2),

          rcv_5             DECIMAL(11,2),
          ret92_5           DECIMAL(11,2),
          ahorro_5          DECIMAL(11,2),
          apcom_5           DECIMAL(11,2),
          apvol_5           DECIMAL(11,2),
          viv97_5           DECIMAL(11,2),
          viv92_5           DECIMAL(11,2),
          fondo_5           DECIMAL(11,2),
          largop_5          DECIMAL(11,2),
---<
--#INV-5412  siefore 14
          rcv_14             DECIMAL(11,2),
          ret92_14           DECIMAL(11,2),
          ahorro_14          DECIMAL(11,2),
          apcom_14           DECIMAL(11,2),
          apvol_14           DECIMAL(11,2),
          viv97_14           DECIMAL(11,2),
          viv92_14           DECIMAL(11,2),
          fondo_14           DECIMAL(11,2),
          largop_14          DECIMAL(11,2),   

--#INV-5412  siefore 15
          rcv_15             DECIMAL(11,2),
          ret92_15           DECIMAL(11,2),
          ahorro_15          DECIMAL(11,2),
          apcom_15           DECIMAL(11,2),
          apvol_15           DECIMAL(11,2),
          viv97_15           DECIMAL(11,2),
          viv92_15           DECIMAL(11,2),
          fondo_15           DECIMAL(11,2),
          largop_15          DECIMAL(11,2), 

--#INV-5412  siefore 16
          rcv_16             DECIMAL(11,2),
          ret92_16           DECIMAL(11,2),
          ahorro_16          DECIMAL(11,2),
          apcom_16           DECIMAL(11,2),
          apvol_16           DECIMAL(11,2),
          viv97_16           DECIMAL(11,2),
          viv92_16           DECIMAL(11,2),
          fondo_16           DECIMAL(11,2),
          largop_16          DECIMAL(11,2), 

--#INV-5412  siefore 17
          rcv_17             DECIMAL(11,2),
          ret92_17           DECIMAL(11,2),
          ahorro_17          DECIMAL(11,2),
          apcom_17           DECIMAL(11,2),
          apvol_17           DECIMAL(11,2),
          viv97_17           DECIMAL(11,2),
          viv92_17           DECIMAL(11,2),
          fondo_17           DECIMAL(11,2),
          largop_17          DECIMAL(11,2), 

--#INV-5412  siefore 18
          rcv_18             DECIMAL(11,2),
          ret92_18           DECIMAL(11,2),
          ahorro_18          DECIMAL(11,2),
          apcom_18           DECIMAL(11,2),
          apvol_18           DECIMAL(11,2),
          viv97_18           DECIMAL(11,2),
          viv92_18           DECIMAL(11,2),
          fondo_18           DECIMAL(11,2),
          largop_18          DECIMAL(11,2), 

--#INV-5412  siefore 19
          rcv_19             DECIMAL(11,2),
          ret92_19           DECIMAL(11,2),
          ahorro_19          DECIMAL(11,2),
          apcom_19           DECIMAL(11,2),
          apvol_19           DECIMAL(11,2),
          viv97_19           DECIMAL(11,2),
          viv92_19           DECIMAL(11,2),
          fondo_19           DECIMAL(11,2),
          largop_19          DECIMAL(11,2), 

--#INV-5412  siefore 20
          rcv_20             DECIMAL(11,2),
          ret92_20           DECIMAL(11,2),
          ahorro_20          DECIMAL(11,2),
          apcom_20           DECIMAL(11,2),
          apvol_20           DECIMAL(11,2),
          viv97_20           DECIMAL(11,2),
          viv92_20           DECIMAL(11,2),
          fondo_20           DECIMAL(11,2),
          largop_20          DECIMAL(11,2), 

--#INV-5412  siefore 21
          rcv_21             DECIMAL(11,2),
          ret92_21           DECIMAL(11,2),
          ahorro_21          DECIMAL(11,2),
          apcom_21           DECIMAL(11,2),
          apvol_21           DECIMAL(11,2),
          viv97_21           DECIMAL(11,2),
          viv92_21           DECIMAL(11,2),
          fondo_21           DECIMAL(11,2),
          largop_21          DECIMAL(11,2), 

--#INV-5412  siefore 22
          rcv_22             DECIMAL(11,2),
          ret92_22           DECIMAL(11,2),
          ahorro_22          DECIMAL(11,2),
          apcom_22           DECIMAL(11,2),
          apvol_22           DECIMAL(11,2),
          viv97_22           DECIMAL(11,2),
          viv92_22           DECIMAL(11,2),
          fondo_22           DECIMAL(11,2),
          largop_22          DECIMAL(11,2),                                                                                       
---< 

          ahorro_retiro,
          ahorro_vol,
          ahorro_viv,
          ahorro_lplazo     DECIMAL(13,2),

          tot_ahorro        DECIMAL(15,2),

          v_anio_finicta,   
          v_anio_actual      CHAR(4),
          lc_query           CHAR(3000),
          v_precio           DECIMAL(19,14),
          v_siefore          SMALLINT,
          v_tabla            VARCHAR(13),
          v_subcuenta        CHAR(9),
          v_sub_1,
          v_sub_2,
          v_sub_3,
          v_sub_4,
          v_sub_6,
          v_sub_7,
          v_existe_dis_ctaxx INTEGER


  LET dia_hoy  = TODAY


   CALL limpia()
   INITIALIZE p_mae_afi.* TO NULL
   INITIALIZE v_siefore, v_tabla, v_sub_1, v_sub_2, v_sub_3, v_sub_4, v_sub_6, v_sub_7 TO NULL
   
   PREPARE apt_09 FROM selec_tab
   DECLARE cur_09 CURSOR FOR apt_09
   FOREACH cur_09 INTO p_mae_afi.*

{       ## TIPO TRABAJADOR - TIPO ADMINISTRACION  
       SELECT a.tipo_trab_ind, a.tipo_administracion
         INTO vtipo_trab_ind, vtipo_administracion
         FROM cta_ctr_reg_ind a
        WHERE a.curp = p_mae_afi.curp
          AND a.nti  = p_mae_afi.nss        
    
       ##Independientes
       IF vtipo_trab_ind       = '1'  AND
          vtipo_administracion = '01' THEN
          LET tipo_com         = 3       
       END IF
    
       ##Issste - Banxico Icefa
       IF vtipo_trab_ind       = '2'  AND
          vtipo_administracion = '07' THEN
          LET tipo_com         = 5       
       END IF
    
       ##Issste - Afore Siefore
       IF vtipo_trab_ind       = '2'  AND
          vtipo_administracion = '01' THEN
          LET tipo_com         = 7       
       END IF
}
       CALL llena_comision_no_afiliados(p_mae_afi.finicta, p_mae_afi.nss) RETURNING gen_compara.*,ban
       
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
      WHERE p_mae_afi.finicta BETWEEN a.fecha_ini AND a.fecha_fin
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
            FOR i = long TO 40
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

         IF p_mae_afi.fena = " "   OR
            p_mae_afi.fena IS NULL THEN
             LET fena = "           "
         ELSE
               LET fena = p_mae_afi.fena USING "DD/MM/YYYY"
         END IF

         IF p_mae_afi.finitmte = " "   OR
            p_mae_afi.finitmte IS NULL THEN
                LET f_asigna = "           "
         ELSE
                  LET f_asigna = p_mae_afi.finitmte USING "DD/MM/YYYY"
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

       INITIALIZE sie.*,sie_rcv,sie_ret92,sie_apvol TO NULL
       INITIALIZE sie_ahorro,sie_apcom,sie_largop TO NULL

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
            SELECT UNIQUE razon_social[1,8] INTO nombre_sie from tab_siefore_local
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

         LET finicta = p_mae_afi.finicta

##fecha_carta
         LET ho_ra = TIME
         LET fe_carta = TODAY USING "mm/dd/yyyy"
##entidad
         LET entidad = "    "
##fecha generacion de la carta
         LET fecha_creacion = TODAY USING "dd/mm/yyyy"
         LET fecha_barra    = TODAY USING "yyyymmdd"
##no_carta
##codigo_barras

         LET barras = "C",no_carta,"000",p_mae_afi.nss,
                      fecha_barra

         LET afore_xx = "                    "
         LET comis_xx = "       "

         IF prog = "407" OR prog = "4071" THEN

            LET tipo_proceso = "R"

            LET asig.rcv     = 0
            LET asig.ret92   = 0
            LET asig.ahorro  = 0
            LET asig.apcom   = 0
            LET asig.apvol   = 0
            LET asig.s_inver = 0
            LET asig.viv97   = 0
            LET asig.viv92   = 0
            LET asig.s_viv   = 0
            LET asig.fondo   = 0
            LET asig.largop  = 0

            LET rcv_1 = 0   
            LET rcv_2 = 0
            LET rcv_3 = 0
            LET rcv_4 = 0
            LET rcv_5 = 0            
            LET rcv_14 = 0   #INV-5412
            LET rcv_15 = 0   #INV-5412
            LET rcv_16 = 0   #INV-5412
            LET rcv_17 = 0   #INV-5412
            LET rcv_18 = 0   #INV-5412
            LET rcv_19 = 0   #INV-5412
            LET rcv_20 = 0   #INV-5412
            LET rcv_21 = 0   #INV-5412
            LET rcv_22 = 0   #INV-5412
             
            LET precio_1 = 0  
            LET precio_2 = 0  
            LET precio_3 = 0  
            LET precio_4 = 0  
            LET precio_5 = 0  
            LET precio_14 = 0 #INV-5412
            LET precio_15 = 0 #INV-5412
            LET precio_16 = 0 #INV-5412
            LET precio_17 = 0 #INV-5412
            LET precio_18 = 0 #INV-5412
            LET precio_19 = 0 #INV-5412
            LET precio_20 = 0 #INV-5412
            LET precio_21 = 0 #INV-5412
            LET precio_22 = 0 #INV-5412

            SELECT a.precio_del_dia INTO precio_1 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 1

            SELECT a.precio_del_dia INTO precio_2 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 2

--->mas siefores
            SELECT a.precio_del_dia INTO precio_3 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 3

            SELECT a.precio_del_dia INTO precio_4 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 4

            SELECT a.precio_del_dia INTO precio_5 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 5
---<
---<#INV-5412
            SELECT a.precio_del_dia INTO precio_14 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 14            

            SELECT a.precio_del_dia INTO precio_15 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 15    

            SELECT a.precio_del_dia INTO precio_16 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 16

            SELECT a.precio_del_dia INTO precio_17 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 17             

            SELECT a.precio_del_dia INTO precio_18 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 18

            SELECT a.precio_del_dia INTO precio_19 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 19

            SELECT a.precio_del_dia INTO precio_20 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 20  

            SELECT a.precio_del_dia INTO precio_21 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 21

            SELECT a.precio_del_dia INTO precio_22 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 22
            
--->#INV-5412                                                       
---------> 

-------------------------------------------------------------------------------------------> UNO

         LET v_anio_finicta = YEAR(p_mae_afi.finicta)
         LET v_anio_actual =  YEAR(TODAY)
            
         FOR v_sub_1 = 1 TO 14
            CASE v_sub_1
               WHEN 1  -- rcv_1
                  LET v_precio = precio_1
                  LET v_siefore = 1 
               WHEN 2  -- rcv_2
                  LET v_precio = precio_2
                  LET v_siefore = 2
               WHEN 3  -- rcv_3
                  LET v_precio = precio_3
                  LET v_siefore = 3
               WHEN 4  -- rcv_4
                  LET v_precio = precio_4
                  LET v_siefore = 4
               WHEN 5  -- rcv_5
                  LET v_precio = precio_5
                  LET v_siefore = 5
               WHEN 6  -- rcv_14
                  LET v_precio = precio_14
                  LET v_siefore = 14
               WHEN 7  -- rcv_15
                  LET v_precio = precio_15
                  LET v_siefore = 15
               WHEN 8  -- rcv_16
                  LET v_precio = precio_16
                  LET v_siefore = 16
               WHEN 9  -- rcv_17
                  LET v_precio = precio_17
                  LET v_siefore = 17
               WHEN 10  -- rcv_18
                  LET v_precio = precio_18
                  LET v_siefore = 18
               WHEN 11  -- rcv_19
                  LET v_precio = precio_19
                  LET v_siefore = 19
               WHEN 12  -- rcv_20
                  LET v_precio = precio_20
                  LET v_siefore = 20
               WHEN 13  -- rcv_21
                  LET v_precio = precio_21
                  LET v_siefore = 21
               WHEN 14  -- rcv_22
                  LET v_precio = precio_22
                  LET v_siefore = 22
            END CASE

            INITIALIZE lc_query, v_tabla TO NULL
            
            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta IN (1,2,5,6,9)",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta IN (1,2,5,6,9)",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                 ELSE
                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta",
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta IN (1,2,5,6,9)",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                  END IF

               END IF
               
            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_1,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_1 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
--                     AND bb.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
--                     AND bb.siefore          = csie_rcv
                     AND bb.siefore          = 1}
            IF v_sub_1 = 1 THEN 
               PREPARE prp_rcv_1a FROM lc_query
               EXECUTE prp_rcv_1a INTO rcv_1
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_2,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_2 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 2}
            IF v_sub_1 = 2 THEN 
               PREPARE prp_rcv_2a FROM lc_query
               EXECUTE prp_rcv_2a INTO rcv_2
            END IF

--->mas siefores
            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_3,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_3 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 3}
            IF v_sub_1 = 3 THEN 
               PREPARE prp_rcv_3a FROM lc_query
               EXECUTE prp_rcv_3a INTO rcv_3
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_4,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_4 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 4}
            IF v_sub_1 = 4 THEN 
               PREPARE prp_rcv_4a FROM lc_query
               EXECUTE prp_rcv_4a INTO rcv_4
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_5,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_5 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 5}
            IF v_sub_1 = 5 THEN
               PREPARE prp_rcv_5a FROM lc_query
               EXECUTE prp_rcv_5a INTO rcv_5
            END IF
---<
--CPL-3045
            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_14,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_14 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 14}
            IF v_sub_1 = 6 THEN
               PREPARE prp_rcv_14a FROM lc_query
               EXECUTE prp_rcv_14a INTO rcv_14
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_15,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_15 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 15}
            IF v_sub_1 = 7 THEN
               PREPARE prp_rcv_15a FROM lc_query
               EXECUTE prp_rcv_15a INTO rcv_15
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_16,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_16 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 16}
            IF v_sub_1 = 8 THEN
               PREPARE prp_rcv_16a FROM lc_query
               EXECUTE prp_rcv_16a INTO rcv_16
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_17,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_17 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 17}  
            IF v_sub_1 = 9 THEN
               PREPARE prp_rcv_17a FROM lc_query
               EXECUTE prp_rcv_17a INTO rcv_17
            END IF 

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_18,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_18 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 18}
            IF v_sub_1 = 10 THEN
               PREPARE prp_rcv_18a FROM lc_query
               EXECUTE prp_rcv_18a INTO rcv_18
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_19,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_19 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 19}
            IF v_sub_1 = 11 THEN
               PREPARE prp_rcv_19a FROM lc_query
               EXECUTE prp_rcv_19a INTO rcv_19
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_20,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_20 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 20}
            IF v_sub_1 = 12 THEN
               PREPARE prp_rcv_20a FROM lc_query
               EXECUTE prp_rcv_20a INTO rcv_20
            END IF   

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_21,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_21 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 21}
            IF v_sub_1 = 13 THEN
               PREPARE prp_rcv_21a FROM lc_query
               EXECUTE prp_rcv_21a INTO rcv_21
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_22,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_22 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 22}  
            IF v_sub_1 = 14 THEN
               PREPARE prp_rcv_22a FROM lc_query
               EXECUTE prp_rcv_22a INTO rcv_22
            END IF
         END FOR   

-------------------------------------------------------------------------------------------< UNO
--> CPL-3045                                                                                                                                                                             

            IF rcv_1 IS NULL THEN
                LET rcv_1 = 0
            END IF

            IF rcv_2 IS NULL THEN
                LET rcv_2 = 0
            END IF

--->mas siefores
            IF rcv_3 IS NULL THEN
                LET rcv_3 = 0
            END IF

            IF rcv_4 IS NULL THEN
                LET rcv_4 = 0
            END IF

            IF rcv_5 IS NULL THEN
                LET rcv_5 = 0
            END IF

            IF rcv_14 IS NULL THEN
                LET rcv_14 = 0
            END IF   

            IF rcv_15 IS NULL THEN
                LET rcv_15 = 0
            END IF            

            IF rcv_16 IS NULL THEN
                LET rcv_16 = 0
            END IF   

            IF rcv_17 IS NULL THEN
                LET rcv_17 = 0
            END IF   

            IF rcv_18 IS NULL THEN
                LET rcv_18 = 0
            END IF   

            IF rcv_19 IS NULL THEN
                LET rcv_19 = 0
            END IF    

            IF rcv_20 IS NULL THEN
                LET rcv_20 = 0
            END IF  

            IF rcv_21 IS NULL THEN
                LET rcv_21 = 0
            END IF      

            IF rcv_22 IS NULL THEN
                LET rcv_22 = 0
            END IF                                                                        
---<

            LET asig.rcv = rcv_1 + rcv_2 +
                           rcv_3 + rcv_4 + rcv_5 +  rcv_14 +
                           rcv_15 + rcv_16 + rcv_17 + rcv_18 + rcv_19 + rcv_20 + 
                           rcv_21 + rcv_22         ---mas siefores

--display "asig.rcv1 :",asig.rcv
            IF asig.rcv IS NULL THEN
               LET asig.rcv = 0
            END IF

            LET ret92_1 = 0  
            LET ret92_2 = 0  
            LET ret92_3 = 0  
            LET ret92_4 = 0  
            LET ret92_5 = 0    ---mas siefores
--CPL-3045
            LET ret92_14 = 0
            LET ret92_15 = 0  
            LET ret92_16 = 0  
            LET ret92_17 = 0 
            LET ret92_18 = 0
            LET ret92_19 = 0
            LET ret92_20 = 0   
            LET ret92_21 = 0
            LET ret92_22 = 0      

-------------------------------------------------------------------------------------------> DOS
         FOR v_sub_2 = 1 TO 14
            CASE v_sub_2
               WHEN 1  -- ret92_1
                  LET v_precio = precio_1
                  LET v_siefore = 1

               WHEN 2  -- ret92_2
                  LET v_precio = precio_2
                  LET v_siefore = 2

               WHEN 3  -- ret92_3 
                  LET v_precio = precio_3
                  LET v_siefore = 3

               WHEN 4  -- ret92_4
                  LET v_precio = precio_4
                  LET v_siefore = 4

               WHEN 5  -- ret92_5
                  LET v_precio = precio_5
                  LET v_siefore = 5

               WHEN 6  -- ret92_14
                  LET v_precio = precio_14
                  LET v_siefore = 14

               WHEN 7  -- 
                  LET v_precio = precio_15
                  LET v_siefore = 15

               WHEN 8  -- ret92_16
                  LET v_precio = precio_16
                  LET v_siefore = 16

               WHEN 9  -- 
                  LET v_precio = precio_17
                  LET v_siefore = 17

               WHEN 10  -- ret92_18
                  LET v_precio = precio_18
                  LET v_siefore = 18

               WHEN 11  -- ret92_19
                  LET v_precio = precio_19
                  LET v_siefore = 19

               WHEN 12  -- ret92_20
                  LET v_precio = precio_20
                  LET v_siefore = 20

               WHEN 13  -- ret92_21
                  LET v_precio = precio_21
                  LET v_siefore = 21

               WHEN 14  -- ret92_22
                  LET v_precio = precio_22
                  LET v_siefore = 22
            END CASE 

            INITIALIZE lc_query, v_tabla TO NULL

            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = 7",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = 7",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                 ELSE
                    LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = 7",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                  END IF

               END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_1,2)
                 INTO ret92_1 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
--                     AND h.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
--                     AND h.siefore          = csie_ret92
                     AND h.siefore          = 1}
            IF v_sub_2 = 1 THEN
               PREPARE prp_ret92_1a FROM lc_query
               EXECUTE prp_ret92_1a INTO ret92_1
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_2,2)
                 INTO ret92_2 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
--                     AND h.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 2}
            IF v_sub_2 = 2 THEN
               PREPARE prp_ret92_2a FROM lc_query
               EXECUTE prp_ret92_2a INTO ret92_2
            END IF

--->mas siefores
            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_3,2)
                 INTO ret92_3 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 3}
            IF v_sub_2 = 3 THEN
               PREPARE prp_ret92_3a FROM lc_query
               EXECUTE prp_ret92_3a INTO ret92_3
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_4,2)
                 INTO ret92_4 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 4}
            IF v_sub_2 = 4 THEN
               PREPARE prp_ret92_4a FROM lc_query
               EXECUTE prp_ret92_4a INTO ret92_4
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_5,2)
                 INTO ret92_5 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 5}
            IF v_sub_2 = 5 THEN
               PREPARE prp_ret92_5a FROM lc_query
               EXECUTE prp_ret92_5a INTO ret92_5
            END IF
---<
--CPL-3045

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_14,2)
                 INTO ret92_14 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 14}
            IF v_sub_2 = 6 THEN
               PREPARE prp_ret92_14a FROM lc_query
               EXECUTE prp_ret92_14a INTO ret92_14
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_15,2)
                 INTO ret92_15 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 15}
            IF v_sub_2 = 7 THEN
               PREPARE prp_ret92_15a FROM lc_query
               EXECUTE prp_ret92_15a INTO ret92_15
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_16,2)
                 INTO ret92_16 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 16}
            IF v_sub_2 = 8 THEN
               PREPARE prp_ret92_16a FROM lc_query
               EXECUTE prp_ret92_16a INTO ret92_16
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_17,2)
                 INTO ret92_17 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 17}
            IF v_sub_2 = 9 THEN
               PREPARE prp_ret92_17a FROM lc_query
               EXECUTE prp_ret92_17a INTO ret92_17
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_18,2)
                 INTO ret92_18 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 18}
            IF v_sub_2 = 10 THEN
               PREPARE prp_ret92_18a FROM lc_query
               EXECUTE prp_ret92_18a INTO ret92_18
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_19,2)
                 INTO ret92_19 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 19}
            IF v_sub_2 = 11 THEN
               PREPARE prp_ret92_19a FROM lc_query
               EXECUTE prp_ret92_19a INTO ret92_19
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_20,2)
                 INTO ret92_20 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 20}
            IF v_sub_2 = 12 THEN
               PREPARE prp_ret92_20a FROM lc_query
               EXECUTE prp_ret92_20a INTO ret92_20
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_21,2)
                 INTO ret92_21 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 21}
            IF v_sub_2 = 13 THEN
               PREPARE prp_ret92_21a FROM lc_query
               EXECUTE prp_ret92_21a INTO ret92_21
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_22,2)
                 INTO ret92_22 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 22} 
             IF v_sub_2 = 14 THEN
               PREPARE prp_ret92_22a FROM lc_query
               EXECUTE prp_ret92_22a INTO ret92_22
            END IF

         END FOR 
         
            IF ret92_1 IS NULL THEN
                LET ret92_1 = 0
            END IF

            IF ret92_2 IS NULL THEN
                LET ret92_2 = 0
            END IF

--->mas siefores
            IF ret92_3 IS NULL THEN
                LET ret92_3 = 0
            END IF

            IF ret92_4 IS NULL THEN
                LET ret92_4 = 0
            END IF

            IF ret92_5 IS NULL THEN
                LET ret92_5 = 0
            END IF

            IF ret92_14 IS NULL THEN
                LET ret92_14 = 0
            END IF            

            IF ret92_15 IS NULL THEN
                LET ret92_15 = 0
            END IF            

            IF ret92_16 IS NULL THEN
                LET ret92_16 = 0
            END IF            

            IF ret92_17 IS NULL THEN
                LET ret92_17 = 0
            END IF      

            IF ret92_18 IS NULL THEN
                LET ret92_18 = 0
            END IF     

            IF ret92_19 IS NULL THEN
                LET ret92_19 = 0
            END IF 

            IF ret92_20 IS NULL THEN
                LET ret92_20 = 0
            END IF   

            IF ret92_21 IS NULL THEN
                LET ret92_21 = 0
            END IF       

            IF ret92_22 IS NULL THEN
                LET ret92_22 = 0
            END IF                                                 
---<

            LET asig.ret92 = ret92_1 + ret92_2 +
                             ret92_3 + ret92_4 + ret92_5     ---mas siefores
                             + ret92_14 + ret92_15 + ret92_16 + ret92_17 + ret92_18 + ret92_19 + ret92_20 + 
                             ret92_21 + ret92_22

            IF asig.ret92 IS NULL THEN
               LET asig.ret92 = 0
            END IF
-------------------------------------------------------------------------------------------< DOS

            LET ahorro_1 = 0   LET ahorro_2 = 0  --LET precio = 0
            LET ahorro_3 = 0   LET ahorro_4 = 0    LET ahorro_5 = 0     ---mas siefores
            LET ahorro_14 = 0   LET ahorro_15 = 0  LET ahorro_16 = 0
            LET ahorro_17 = 0   LET ahorro_18 = 0  LET ahorro_19 = 0
            LET ahorro_20 = 0   LET ahorro_21 = 0  LET ahorro_22 = 0      

-------------------------------------------------------------------------------------------> TRES
         FOR v_sub_3 = 1 TO 13
            CASE v_sub_3
               WHEN 1  -- ahorro_1
                  LET v_precio = precio_1
                  LET v_siefore = 1

               WHEN 2  -- ahorro_2
                  LET v_precio = precio_2
                  LET v_siefore = 2

               WHEN 3  -- ahorro_3
                  LET v_precio = precio_3
                  LET v_siefore = 3

               WHEN 4  -- ahorro_4
                  LET v_precio = precio_4
                  LET v_siefore = 4

               WHEN 5  -- ahorro_5
                  LET v_precio = precio_5
                  LET v_siefore = 5

               WHEN 6  -- ahorro_14
                  LET v_precio = precio_14
                  LET v_siefore = 14

               WHEN 7  -- ahorro_15
                  LET v_precio = precio_15
                  LET v_siefore = 15

               WHEN 8  -- ahorro_16
                  LET v_precio = precio_16
                  LET v_siefore = 16

               WHEN 9  -- ahorro_17
                  LET v_precio = precio_17
                  LET v_siefore = 17

               WHEN 10  -- ahorro_18
                  LET v_precio = precio_18
                  LET v_siefore = 18

               WHEN 11  -- ahorro_19
                  LET v_precio = precio_19
                  LET v_siefore = 19

               WHEN 12  -- ahorro_20
                  LET v_precio = precio_20
                  LET v_siefore = 20

               WHEN 13  -- ahorro_21
                  LET v_precio = precio_21
                  LET v_siefore = 21
            END CASE

            INITIALIZE lc_query, v_tabla TO NULL
            
            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = 13",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta = 13",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                 ELSE
                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta",
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta = 13",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                  END IF

               END IF
               
            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_1,2)
                 INTO ahorro_1 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
##                     AND n.siefore          = csie_ahorro
                     AND n.siefore          = 1}
            IF v_sub_3 = 1 THEN
               PREPARE prp_ahorro_1a FROM lc_query
               EXECUTE prp_ahorro_1a INTO ahorro_1
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_2,2)
                 INTO ahorro_2 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
##                     AND n.siefore          = csie_ahorro
                     AND n.siefore          = 2}
            IF v_sub_3 = 2 THEN
               PREPARE prp_ahorro_2a FROM lc_query
               EXECUTE prp_ahorro_2a INTO ahorro_2
            END IF

--->
            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_3,2)
                 INTO ahorro_3 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 3}
            IF v_sub_3 = 3 THEN
               PREPARE prp_ahorro_3a FROM lc_query
               EXECUTE prp_ahorro_3a INTO ahorro_3
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_4,2)
                 INTO ahorro_4 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 4}
            IF v_sub_3 = 4 THEN
               PREPARE prp_ahorro_4a FROM lc_query
               EXECUTE prp_ahorro_4a INTO ahorro_4
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_5,2)
                 INTO ahorro_5 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 5}
            IF v_sub_3 = 5 THEN
               PREPARE prp_ahorro_5a FROM lc_query
               EXECUTE prp_ahorro_5a INTO ahorro_5
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_14,2)
                 INTO ahorro_14 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 14}
            IF v_sub_3 = 6 THEN
               PREPARE prp_ahorro_14a FROM lc_query
               EXECUTE prp_ahorro_14a INTO ahorro_14
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_15,2)
                 INTO ahorro_15 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 15}
            IF v_sub_3 = 7 THEN
               PREPARE prp_ahorro_15a FROM lc_query
               EXECUTE prp_ahorro_15a INTO ahorro_15
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_16,2)
                 INTO ahorro_16 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 16}
            IF v_sub_3 = 8 THEN
               PREPARE prp_ahorro_16a FROM lc_query
               EXECUTE prp_ahorro_16a INTO ahorro_16
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_17,2)
                 INTO ahorro_17 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 17}
            IF v_sub_3 = 9 THEN
               PREPARE prp_ahorro_17a FROM lc_query
               EXECUTE prp_ahorro_17a INTO ahorro_17
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_18,2)
                 INTO ahorro_18 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 18}
            IF v_sub_3 = 10 THEN
               PREPARE prp_ahorro_18a FROM lc_query
               EXECUTE prp_ahorro_18a INTO ahorro_18
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_19,2)
                 INTO ahorro_19 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 19}
            IF v_sub_3 = 11 THEN
               PREPARE prp_ahorro_19a FROM lc_query
               EXECUTE prp_ahorro_19a INTO ahorro_19
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_20,2)
                 INTO ahorro_20 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 20}
            IF v_sub_3 = 12 THEN
               PREPARE prp_ahorro_20a FROM lc_query
               EXECUTE prp_ahorro_20a INTO ahorro_20
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_21,2)
                 INTO ahorro_21 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 21} 
            IF v_sub_3 = 13 THEN
               PREPARE prp_ahorro_21a FROM lc_query
               EXECUTE prp_ahorro_21a INTO ahorro_21
            END IF
         END FOR
-------------------------------------------------------------------------------------------< TRES
---<

            IF ahorro_1 IS NULL THEN
                LET ahorro_1 = 0
            END IF

            IF ahorro_2 IS NULL THEN
                LET ahorro_2 = 0
            END IF
--->mas siefores
            IF ahorro_3 IS NULL THEN
                LET ahorro_3 = 0
            END IF

            IF ahorro_4 IS NULL THEN
                LET ahorro_4 = 0
            END IF

            IF ahorro_5 IS NULL THEN
                LET ahorro_5 = 0
            END IF

            IF ahorro_14 IS NULL THEN
                LET ahorro_14 = 0
            END IF 

            IF ahorro_15 IS NULL THEN
                LET ahorro_15 = 0
            END IF      

            IF ahorro_16 IS NULL THEN
                LET ahorro_16 = 0
            END IF              

            IF ahorro_17 IS NULL THEN
                LET ahorro_17 = 0
            END IF        

            IF ahorro_18 IS NULL THEN
                LET ahorro_18 = 0
            END IF       

            IF ahorro_19 IS NULL THEN
                LET ahorro_19 = 0
            END IF       

            IF ahorro_20 IS NULL THEN
                LET ahorro_20 = 0
            END IF       

            IF ahorro_21 IS NULL THEN
                LET ahorro_21 = 0
            END IF      

            IF ahorro_22 IS NULL THEN
                LET ahorro_22 = 0
            END IF                                                    
---<
            LET asig.ahorro = ahorro_1 + ahorro_2 +
                              ahorro_3 + ahorro_4 + ahorro_5 +   ---mas siefores
                              ahorro_14 + ahorro_15 + ahorro_16 +
                              ahorro_17 + ahorro_18 + ahorro_19 + 
                              ahorro_20 + ahorro_21 + ahorro_22

            IF asig.ahorro IS NULL THEN
               LET asig.ahorro = 0
            END IF

            LET apcom_1 = 0   LET apcom_2 = 0  --LET precio = 0
            LET apcom_3 = 0   LET apcom_4 = 0   LET apcom_5 = 0
            LET apcom_14 = 0   LET apcom_15 = 0  LET apcom_16 = 0
						LET apcom_17 = 0   LET apcom_18 = 0  LET apcom_19 = 0
						LET apcom_20 = 0   LET apcom_21 = 0  LET apcom_22 = 0

-------------------------------------------------------------------------------------------> CUATRO
         FOR v_sub_4 = 1 TO 14
            CASE v_sub_4
               WHEN 1  -- apcom_1
                  LET v_precio = precio_1
                  LET v_siefore = 1

               WHEN 2  -- apcom_2
                  LET v_precio = precio_2
                  LET v_siefore = 2

               WHEN 3  -- apcom_3
                  LET v_precio = precio_3
                  LET v_siefore = 3

               WHEN 4  -- apcom_4
                  LET v_precio = precio_4
                  LET v_siefore = 4

               WHEN 5  -- apcom_5
                  LET v_precio = precio_5
                  LET v_siefore = 5

               WHEN 6  -- apcom_14
                  LET v_precio = precio_14
                  LET v_siefore = 14

               WHEN 7  -- apcom_15
                  LET v_precio = precio_15
                  LET v_siefore = 15

               WHEN 8  -- apcom_16
                  LET v_precio = precio_16
                  LET v_siefore = 16

               WHEN 9  -- apcom_17
                  LET v_precio = precio_17
                  LET v_siefore = 17

               WHEN 10  -- apcom_18
                  LET v_precio = precio_18
                  LET v_siefore = 18

               WHEN 11  -- apcom_19
                  LET v_precio = precio_19
                  LET v_siefore = 19

               WHEN 12  -- apcom_20
                  LET v_precio = precio_20
                  LET v_siefore = 20

               WHEN 13  -- apcom_21
                  LET v_precio = precio_21
                  LET v_siefore = 21

               WHEN 14  -- apcom_22
                  LET v_precio = precio_22
                  LET v_siefore = 22
            END CASE

            INITIALIZE lc_query, v_tabla TO NULL
            
            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta IN (11,12)",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta IN (11,12)",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                 ELSE
                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta",
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta IN (11,12)",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                  END IF

               END IF
               

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_1,2)
                 INTO apcom_1 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
##                     AND n.siefore          = csie_apcom
                     AND n.siefore          = 1}
            IF v_sub_4 = 1 THEN
               PREPARE prp_apcom_1a FROM lc_query
               EXECUTE prp_apcom_1a INTO apcom_1
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_2,2)
                 INTO apcom_2 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
##                     AND n.siefore          = csie_apcom
                     AND n.siefore          = 2}
            IF v_sub_4 = 2 THEN
               PREPARE prp_apcom_2a FROM lc_query
               EXECUTE prp_apcom_2a INTO apcom_2
            END IF
            
--->mas siefores
            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_3,2)
                 INTO apcom_3 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 3}
            IF v_sub_4 = 3 THEN
               PREPARE prp_apcom_3a FROM lc_query
               EXECUTE prp_apcom_3a INTO apcom_3
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_4,2)
                 INTO apcom_4 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 4}
            IF v_sub_4 = 4 THEN
               PREPARE prp_apcom_4a FROM lc_query
               EXECUTE prp_apcom_4a INTO apcom_4
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_5,2)
                 INTO apcom_5 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 5}
            IF v_sub_4 = 5 THEN
               PREPARE prp_apcom_5a FROM lc_query
               EXECUTE prp_apcom_5a INTO apcom_5
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_14,2)
                 INTO apcom_14 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 14}
            IF v_sub_4 = 6 THEN
               PREPARE prp_apcom_14a FROM lc_query
               EXECUTE prp_apcom_14a INTO apcom_14
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_15,2)
                 INTO apcom_15 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 15 }
            IF v_sub_4 = 7 THEN
               PREPARE prp_apcom_15a FROM lc_query
               EXECUTE prp_apcom_15a INTO apcom_15
            END IF       
                     
            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_16,2)
                 INTO apcom_16 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 16}
            IF v_sub_4 = 8 THEN
               PREPARE prp_apcom_16a FROM lc_query
               EXECUTE prp_apcom_16a INTO apcom_16
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_17,2)
                 INTO apcom_17 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 17}
            IF v_sub_4 = 9 THEN
               PREPARE prp_apcom_17a FROM lc_query
               EXECUTE prp_apcom_17a INTO apcom_17
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_18,2)
                 INTO apcom_18 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 18}
            IF v_sub_4 = 10 THEN
               PREPARE prp_apcom_18a FROM lc_query
               EXECUTE prp_apcom_18a INTO apcom_18
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_19,2)
                 INTO apcom_19 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 19}
            IF v_sub_4 = 11 THEN
               PREPARE prp_apcom_19a FROM lc_query
               EXECUTE prp_apcom_19a INTO apcom_19
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_20,2)
                 INTO apcom_20 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 20}
            IF v_sub_4 = 12 THEN
               PREPARE prp_apcom_20a FROM lc_query
               EXECUTE prp_apcom_20a INTO apcom_20
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_21,2)
                 INTO apcom_21 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 21}
            IF v_sub_4 = 13 THEN
               PREPARE prp_apcom_21a FROM lc_query
               EXECUTE prp_apcom_21a INTO apcom_21
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_22,2)
                 INTO apcom_22 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 22}
             IF v_sub_4 = 14 THEN
               PREPARE prp_apcom_22a FROM lc_query
               EXECUTE prp_apcom_22a INTO apcom_22
            END IF  
         END FOR      
-------------------------------------------------------------------------------------------< CUATRO                     
---<

            IF apcom_1 IS NULL THEN
                LET apcom_1 = 0
            END IF

            IF apcom_2 IS NULL THEN
                LET apcom_2 = 0
            END IF

--->mas siefores
            IF apcom_3 IS NULL THEN
                LET apcom_3 = 0
            END IF

            IF apcom_4 IS NULL THEN
                LET apcom_4 = 0
            END IF

            IF apcom_5 IS NULL THEN
                LET apcom_5 = 0
            END IF

            IF apcom_14 IS NULL THEN
                LET apcom_14 = 0
            END IF     

            IF apcom_15 IS NULL THEN
                LET apcom_15 = 0
            END IF      

            IF apcom_16 IS NULL THEN
                LET apcom_16 = 0
            END IF      

            IF apcom_17 IS NULL THEN
                LET apcom_17 = 0
            END IF       

            IF apcom_18 IS NULL THEN
                LET apcom_18 = 0
            END IF     

            IF apcom_19 IS NULL THEN
                LET apcom_19 = 0
            END IF      

            IF apcom_20 IS NULL THEN
                LET apcom_20 = 0
            END IF       

            IF apcom_21 IS NULL THEN
                LET apcom_21 = 0
            END IF     

            IF apcom_22 IS NULL THEN
                LET apcom_22 = 0
            END IF                                                             
---<

            LET asig.apcom = apcom_1 + apcom_2 +
                             apcom_3 + apcom_4 +apcom_5  +       ---mas siefores
                             apcom_14 + apcom_15 + apcom_16 +
                             apcom_17 + apcom_18 + apcom_19 + 
                             apcom_20 + apcom_21 + apcom_22                             

            IF asig.apcom IS NULL THEN
               LET asig.apcom = 0
            END IF

            LET precio = 0
            SELECT a.precio_del_dia INTO precio FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = csie_apvol

-------------------------------------------------------------------------------------------> CINCO
            INITIALIZE lc_query, v_tabla TO NULL
            
            IF v_anio_finicta = v_anio_actual THEN
                  
               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta IN(3,10)",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", csie_apvol, ""

            ELSE
               LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

               --Se valida que exista la tabla
               LET v_existe_dis_ctaxx = 0
               SELECT count(*)
                 INTO v_existe_dis_ctaxx
                 FROM systables
                WHERE tabname MATCHES v_tabla

               --Si no existe la tabla, se consultara en dis_cuenta
               IF v_existe_dis_ctaxx > 0 THEN

                  LET lc_query = "",
                                 "\n ROUND(SUM(monto_en_acciones)* ", precio,",2) ",
                                 "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                                 "\n WHERE nss = '", p_mae_afi.nss,"'",
                                 "\n   AND subcuenta IN(3,10)",
                                 "\n   AND tipo_movimiento  > 0 ",
                                 "\n   AND siefore = ", csie_apvol, ""
               ELSE
                  LET lc_query = "",
                                 "\n ROUND(SUM(monto_en_acciones)* ", precio, ",2) ",
                                 "\n   FROM dis_cuenta",
                                 "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                 "\n   AND subcuenta IN(3,10)",
                                 "\n   AND tipo_movimiento  > 0 ",
                                 "\n   AND siefore = ", csie_apvol, ""
               END IF

            END IF

----------
            {SELECT ROUND(SUM(n.monto_en_acciones)* precio,2)
                 INTO asig.apvol FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        IN(3,10)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = csie_apvol}
            PREPARE prp_csie_apvola FROM lc_query
            EXECUTE prp_csie_apvola INTO asig.apvol

            IF asig.apvol IS NULL THEN
               LET asig.apvol = 0
            END IF
-------------------------------------------------------------------------------------------< CINCO


            LET precio = 0

            SELECT a.precio_del_dia INTO precio FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 11

-------------------------------------------------------------------------------------------> SEIS
         FOR v_sub_6 = 1 TO 2
            CASE v_sub_6
               WHEN 1  -- asig.viv97
                  LET v_subcuenta = 4
               WHEN 2  -- asig.viv92
                  LET v_subcuenta = 8
            END CASE

            INITIALIZE lc_query, v_tabla TO NULL

            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = ",v_subcuenta,
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = 11"

                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  LET v_existe_dis_ctaxx = 0
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", precio, ",2) ",
                                   "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta = ", v_subcuenta,
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = 11"
                 ELSE
                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)*", precio, ",2) ",
                                   "\n   FROM dis_cuenta",
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta = ", v_subcuenta,
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = 11"
                  END IF

               END IF
               
            {SELECT ROUND(SUM(n.monto_en_acciones)* precio,2)
                 INTO asig.viv97   FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        = 4
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 11}
            IF v_sub_6 = 1 THEN
               PREPARE prp_viv97a FROM lc_query
               EXECUTE prp_viv97a INTO asig.viv97
            END IF
            
            IF asig.viv97 IS NULL THEN
               LET asig.viv97 = 0
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio,2)
                 INTO asig.viv92   FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        = 8
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 11}
            IF v_sub_6 = 2 THEN
               PREPARE prp_viv92a FROM lc_query
               EXECUTE prp_viv92a INTO asig.viv92
            END IF
            
            IF asig.viv92 IS NULL THEN
               LET asig.viv92 = 0
            END IF
            
         END FOR
-------------------------------------------------------------------------------------------< SEIS

-------------------------------------------------------------------------------------------> SIETE
         FOR v_sub_7 = 1 TO 2
            CASE v_sub_7
               WHEN 1  -- asig.fondo
                  LET v_subcuenta = 14
               WHEN 2  -- asig.largop
                  LET v_subcuenta = 15
            END CASE

            INITIALIZE lc_query, v_tabla TO NULL

            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_pesos),2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = ", v_subcuenta,
                              "\n   AND tipo_movimiento  > 0 "

               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  LET v_existe_dis_ctaxx = 0
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                              "\n ROUND(SUM(monto_en_pesos),2) ",
                              "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = ", v_subcuenta,
                              "\n   AND tipo_movimiento  > 0 "
                 ELSE
                    LET lc_query = "",
                              "\n ROUND(SUM(monto_en_pesos),2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = ", v_subcuenta,
                              "\n   AND tipo_movimiento  > 0 "
                  END IF

               END IF
               
            {SELECT ROUND(SUM(t.monto_en_pesos),2)
                    INTO asig.fondo  FROM dis_cuenta t
                      WHERE t.nss              = p_mae_afi.nss
--                        AND t.fecha_conversion <= p_mae_afi.fecha_certifica
                        AND t.subcuenta        IN(14)
                        AND t.tipo_movimiento  > 0}
            IF v_sub_7 = 1 THEN
               PREPARE prp_fondoa FROM lc_query
               EXECUTE prp_fondoa INTO asig.fondo
            END IF
            
            IF asig.fondo IS NULL THEN
               LET asig.fondo = 0
            END IF

            {SELECT ROUND(SUM(t.monto_en_pesos),2)
                    INTO asig.largop  FROM dis_cuenta t
                      WHERE t.nss              = p_mae_afi.nss
--                        AND t.fecha_conversion <= p_mae_afi.fecha_certifica
                        AND t.subcuenta        = 15
                        AND t.tipo_movimiento  > 0}
            IF v_sub_7 = 2 THEN
               PREPARE prp_largopa FROM lc_query
               EXECUTE prp_largopa INTO asig.largop
            END IF
            
            IF asig.largop IS NULL THEN
               LET asig.largop = 0
            END IF
         END FOR
-------------------------------------------------------------------------------------------< SIETE
            LET asig.s_inver = asig.rcv + asig.ret92 + asig.ahorro +
                               asig.apcom + asig.apvol + asig.largop

            LET asig.s_viv   = asig.viv97 + asig.viv92 + asig.fondo

            LET ahorro_retiro = 0
            LET ahorro_vol    = 0
            LET ahorro_viv    = 0
            LET ahorro_lplazo = 0
            LET tot_ahorro    = 0

            LET ahorro_retiro = asig.rcv + asig.ret92 + asig.ahorro
            LET ahorro_vol    = asig.apcom + asig.apvol + asig.largop
            LET ahorro_viv    = asig.viv97 + asig.viv92
            LET ahorro_lplazo = asig.largop
            LET tot_ahorro    = ahorro_retiro + ahorro_vol + ahorro_viv 
            
--display "asig_rcv2 :",asig.rcv
--display "asig_ret9 :",asig.ret92
----display "asig_ahor :",asig.ahorro
--display "ahor_ret  :",ahorro_retiro
--prompt "enter" for enter
         END IF

         SELECT a.codigo_afore, a.razon_social[1,30]
         INTO   clave_afore, des_transferente
         FROM   tab_afore_local a
         GROUP BY 1,2

         LET numero_reg = numero_reg + 1
         LET consecutivo_envio = numero_reg        ## USING "&&&&&&&&"

         CASE prog 
         WHEN "407"
            LET no_carta = "302",ref

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
                          clave_afore USING "&&&"             , "|",  #25
                          des_transferente                    , "|",  #26
                          nacion                              , "|",  #27
                          fena                                , "|",  #28
                          finicta USING "dd/mm/yyyy"          , "|",  #29
                          f_asigna                            , "|",  #30
                          ahorro_retiro                       , "|",  #31
                          sie_rcv                             , "|",  #32
                          ahorro_vol                          , "|",  #33
                          sie_apvol                           , "|",  #34
                          ahorro_viv                          , "|",  #35
                          tot_ahorro                          , "|",  #36
                          asig.largop                         , "|",  #37
                          sie_largop                          , "|",  #38
                          des_transferente                    , "|",  #39
                          gen_compara.afore_a                 , "|",  #40
                          gen_compara.rend_a                  , "|",  #41
                          gen_compara.comis_a                 , "|",  #42
                          gen_compara.rendneto_a              , "|",  #43
                          gen_compara.afore_b                 , "|",  #44
                          gen_compara.rend_b                  , "|",  #45
                          gen_compara.comis_b                 , "|",  #46
                          gen_compara.rendneto_b              , "|",  #47
                          gen_compara.afore_c                 , "|",  #48
                          gen_compara.rend_c                  , "|",  #49
                          gen_compara.comis_c                 , "|",  #50
                          gen_compara.rendneto_c              , "|",  #51
                          gen_compara.afore_d                 , "|",  #52
                          gen_compara.rend_d                  , "|",  #53
                          gen_compara.comis_d                 , "|",  #54
                          gen_compara.rendneto_d              , "|",  #55
                          gen_compara.afore_e                 , "|",  #56
                          gen_compara.rend_e                  , "|",  #57
                          gen_compara.comis_e                 , "|",  #58
                          gen_compara.rendneto_e              , "|",  #59
                          gen_compara.afore_f                 , "|",  #60
                          gen_compara.rend_f                  , "|",  #61
                          gen_compara.comis_f                 , "|",  #62
                          gen_compara.rendneto_f              , "|",  #63
                          gen_compara.afore_g                 , "|",  #64
                          gen_compara.rend_g                  , "|",  #65
                          gen_compara.comis_g                 , "|",  #66
                          gen_compara.rendneto_g              , "|",  #67
                          gen_compara.afore_h                 , "|",  #68
                          gen_compara.rend_h                  , "|",  #69
                          gen_compara.comis_h                 , "|",  #70
                          gen_compara.rendneto_h              , "|",  #71
                          gen_compara.afore_i                 , "|",  #72
                          gen_compara.rend_i                  , "|",  #73
                          gen_compara.comis_i                 , "|",  #74
                          gen_compara.rendneto_i              , "|",  #75
                          gen_compara.afore_j                 , "|",  #76
                          gen_compara.rend_j                  , "|",  #77
                          gen_compara.comis_j                 , "|",  #78
                          gen_compara.rendneto_j              , "|",  #79
                          gen_compara.afore_k                 , "|",  #80
                          gen_compara.rend_k                  , "|",  #81
                          gen_compara.comis_k                 , "|",  #82
                          gen_compara.rendneto_k              , "|",  #83
                          gen_compara.afore_l                 , "|",  #84
                          gen_compara.rend_l                  , "|",  #85
                          gen_compara.comis_l                 , "|",  #86
                          gen_compara.rendneto_l              , "|",  #87
                          gen_compara.afore_m                 , "|",  #88
                          gen_compara.rend_m                  , "|",  #89
                          gen_compara.comis_m                 , "|",  #90
                          gen_compara.rendneto_m              , "|",  #91
                          gen_compara.afore_n                 , "|",  #92
                          gen_compara.rend_n                  , "|",  #93
                          gen_compara.comis_n                 , "|",  #94
                          gen_compara.rendneto_n              , "|",  #95
                          gen_compara.afore_o                 , "|",  #96
                          gen_compara.rend_o                  , "|",  #97
                          gen_compara.comis_o                 , "|",  #98
                          gen_compara.rendneto_o              , "|",  #99
                          gen_compara.afore_p                 , "|",  #100
                          gen_compara.rend_p                  , "|",  #101
                          gen_compara.comis_p                 , "|",  #102
                          gen_compara.rendneto_p              , "|",  #103
                          gen_compara.afore_q                 , "|",  #104
                          gen_compara.rend_q                  , "|",  #105
                          gen_compara.comis_q                 , "|",  #106
                          gen_compara.rendneto_q              , "|",  #107
                          gen_compara.afore_r                 , "|",  #108
                          gen_compara.rend_r                  , "|",  #109
                          gen_compara.comis_r                 , "|",  #110
                          gen_compara.rendneto_r              , "|",  #111
                          gen_compara.afore_s                 , "|",  #112
                          gen_compara.rend_s                  , "|",  #113
                          gen_compara.comis_s                 , "|",  #114
                          gen_compara.rendneto_s              , "|",  #115
                          gen_compara.afore_t                 , "|",  #116
                          gen_compara.rend_t                  , "|",  #117
                          gen_compara.comis_t                 , "|",  #118
                          gen_compara.rendneto_t              , "|",  #119
                          gen_compara.afore_u                 , "|",  #120
                          gen_compara.rend_u                  , "|",  #121
                          gen_compara.comis_u                 , "|",  #122
                          gen_compara.rendneto_u              , "|",  #123
                          gen_compara.afore_v                 , "|",  #124
                          gen_compara.rend_v                  , "|",  #125
                          gen_compara.comis_v                 , "|",  #126
                          gen_compara.rendneto_v              , "|",  #127
                          gen_compara.afore_w                 , "|",  #128
                          gen_compara.rend_w                  , "|",  #129
                          gen_compara.comis_w                 , "|",  #130
                          gen_compara.rendneto_w              , "|",  #131
                          gen_compara.afore_x                 , "|",  #132
                          gen_compara.rend_x                  , "|",  #133
                          gen_compara.comis_x                 , "|",  #134
                          gen_compara.rendneto_x              , "|",  #135
                          gen_compara.afore_y                 , "|",  #136
                          gen_compara.rend_y                  , "|",  #137
                          gen_compara.comis_y                 , "|",  #138
                          gen_compara.rendneto_y              , "|",  #139
                          gen_compara.afore_z                 , "|",  #140
                          gen_compara.rend_z                  , "|",  #141
                          gen_compara.comis_z                 , "|",  #142
                          gen_compara.rendneto_z              , "|",  #143
                          barras                              , "|",   #144
                          vprom_simple						 , "|"   #145 #CPL-1182
         
         WHEN "4071" -- Menores de edad
            LET no_carta = "302",ref

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
                          clave_afore USING "&&&"             , "|",  #25
                          des_transferente                    , "|",  #26
                          nacion                              , "|",  #27
                          fena                                , "|",  #28
                          finicta USING "dd/mm/yyyy"          , "|",  #29
                          f_asigna                            , "|",  #30
                          ahorro_retiro                       , "|",  #31
                          sie_rcv                             , "|",  #32
                          ahorro_vol                          , "|",  #33
                          sie_apvol                           , "|",  #34
                          ahorro_viv                          , "|",  #35
                          tot_ahorro                          , "|",  #36
                          asig.largop                         , "|",  #37
                          sie_largop                          , "|",  #38
                          des_transferente                    , "|",  #39
                          gen_compara.afore_a                 , "|",  #40
                          gen_compara.rend_a                  , "|",  #41
                          gen_compara.comis_a                 , "|",  #42
                          gen_compara.rendneto_a              , "|",  #43
                          gen_compara.afore_b                 , "|",  #44
                          gen_compara.rend_b                  , "|",  #45
                          gen_compara.comis_b                 , "|",  #46
                          gen_compara.rendneto_b              , "|",  #47
                          gen_compara.afore_c                 , "|",  #48
                          gen_compara.rend_c                  , "|",  #49
                          gen_compara.comis_c                 , "|",  #50
                          gen_compara.rendneto_c              , "|",  #51
                          gen_compara.afore_d                 , "|",  #52
                          gen_compara.rend_d                  , "|",  #53
                          gen_compara.comis_d                 , "|",  #54
                          gen_compara.rendneto_d              , "|",  #55
                          gen_compara.afore_e                 , "|",  #56
                          gen_compara.rend_e                  , "|",  #57
                          gen_compara.comis_e                 , "|",  #58
                          gen_compara.rendneto_e              , "|",  #59
                          gen_compara.afore_f                 , "|",  #60
                          gen_compara.rend_f                  , "|",  #61
                          gen_compara.comis_f                 , "|",  #62
                          gen_compara.rendneto_f              , "|",  #63
                          gen_compara.afore_g                 , "|",  #64
                          gen_compara.rend_g                  , "|",  #65
                          gen_compara.comis_g                 , "|",  #66
                          gen_compara.rendneto_g              , "|",  #67
                          gen_compara.afore_h                 , "|",  #68
                          gen_compara.rend_h                  , "|",  #69
                          gen_compara.comis_h                 , "|",  #70
                          gen_compara.rendneto_h              , "|",  #71
                          gen_compara.afore_i                 , "|",  #72
                          gen_compara.rend_i                  , "|",  #73
                          gen_compara.comis_i                 , "|",  #74
                          gen_compara.rendneto_i              , "|",  #75
                          gen_compara.afore_j                 , "|",  #76
                          gen_compara.rend_j                  , "|",  #77
                          gen_compara.comis_j                 , "|",  #78
                          gen_compara.rendneto_j              , "|",  #79
                          gen_compara.afore_k                 , "|",  #80
                          gen_compara.rend_k                  , "|",  #81
                          gen_compara.comis_k                 , "|",  #82
                          gen_compara.rendneto_k              , "|",  #83
                          gen_compara.afore_l                 , "|",  #84
                          gen_compara.rend_l                  , "|",  #85
                          gen_compara.comis_l                 , "|",  #86
                          gen_compara.rendneto_l              , "|",  #87
                          gen_compara.afore_m                 , "|",  #88
                          gen_compara.rend_m                  , "|",  #89
                          gen_compara.comis_m                 , "|",  #90
                          gen_compara.rendneto_m              , "|",  #91
                          gen_compara.afore_n                 , "|",  #92
                          gen_compara.rend_n                  , "|",  #93
                          gen_compara.comis_n                 , "|",  #94
                          gen_compara.rendneto_n              , "|",  #95
                          gen_compara.afore_o                 , "|",  #96
                          gen_compara.rend_o                  , "|",  #97
                          gen_compara.comis_o                 , "|",  #98
                          gen_compara.rendneto_o              , "|",  #99
                          gen_compara.afore_p                 , "|",  #100
                          gen_compara.rend_p                  , "|",  #101
                          gen_compara.comis_p                 , "|",  #102
                          gen_compara.rendneto_p              , "|",  #103
                          gen_compara.afore_q                 , "|",  #104
                          gen_compara.rend_q                  , "|",  #105
                          gen_compara.comis_q                 , "|",  #106
                          gen_compara.rendneto_q              , "|",  #107
                          gen_compara.afore_r                 , "|",  #108
                          gen_compara.rend_r                  , "|",  #109
                          gen_compara.comis_r                 , "|",  #110
                          gen_compara.rendneto_r              , "|",  #111
                          gen_compara.afore_s                 , "|",  #112
                          gen_compara.rend_s                  , "|",  #113
                          gen_compara.comis_s                 , "|",  #114
                          gen_compara.rendneto_s              , "|",  #115
                          gen_compara.afore_t                 , "|",  #116
                          gen_compara.rend_t                  , "|",  #117
                          gen_compara.comis_t                 , "|",  #118
                          gen_compara.rendneto_t              , "|",  #119
                          gen_compara.afore_u                 , "|",  #120
                          gen_compara.rend_u                  , "|",  #121
                          gen_compara.comis_u                 , "|",  #122
                          gen_compara.rendneto_u              , "|",  #123
                          gen_compara.afore_v                 , "|",  #124
                          gen_compara.rend_v                  , "|",  #125
                          gen_compara.comis_v                 , "|",  #126
                          gen_compara.rendneto_v              , "|",  #127
                          gen_compara.afore_w                 , "|",  #128
                          gen_compara.rend_w                  , "|",  #129
                          gen_compara.comis_w                 , "|",  #130
                          gen_compara.rendneto_w              , "|",  #131
                          gen_compara.afore_x                 , "|",  #132
                          gen_compara.rend_x                  , "|",  #133
                          gen_compara.comis_x                 , "|",  #134
                          gen_compara.rendneto_x              , "|",  #135
                          gen_compara.afore_y                 , "|",  #136
                          gen_compara.rend_y                  , "|",  #137
                          gen_compara.comis_y                 , "|",  #138
                          gen_compara.rendneto_y              , "|",  #139
                          gen_compara.afore_z                 , "|",  #140
                          gen_compara.rend_z                  , "|",  #141
                          gen_compara.comis_z                 , "|",  #142
                          gen_compara.rendneto_z              , "|",  #143
                          barras                              , "|",   #144
                          vprom_simple						 , "|"   #145 #CPL-1182
                                           
         WHEN "406"
            LET no_carta = "302",ref

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
                          clave_afore USING "&&&"             , "|",  #25
                          des_transferente                    , "|",  #26
                          nacion                              , "|",  #27
                          fena                                , "|",  #28
                          finicta USING "dd/mm/yyyy"          , "|",  #29
                          f_asigna                            , "|",  #30
                          ahorro_retiro                       , "|",  #31
                          sie_rcv                             , "|",  #32
                          ahorro_vol                          , "|",  #33
                          sie_apvol                           , "|",  #34
                          ahorro_viv                          , "|",  #35
                          tot_ahorro                          , "|",  #36
                          asig.largop                         , "|",  #37
                          sie_largop                          , "|",  #38
                          des_transferente                    , "|",  #39
                          gen_compara.afore_a                 , "|",  #40
                          gen_compara.rend_a                  , "|",  #41
                          gen_compara.comis_a                 , "|",  #42
                          gen_compara.rendneto_a              , "|",  #43
                          gen_compara.afore_b                 , "|",  #44
                          gen_compara.rend_b                  , "|",  #45
                          gen_compara.comis_b                 , "|",  #46
                          gen_compara.rendneto_b              , "|",  #47
                          gen_compara.afore_c                 , "|",  #48
                          gen_compara.rend_c                  , "|",  #49
                          gen_compara.comis_c                 , "|",  #50
                          gen_compara.rendneto_c              , "|",  #51
                          gen_compara.afore_d                 , "|",  #52
                          gen_compara.rend_d                  , "|",  #53
                          gen_compara.comis_d                 , "|",  #54
                          gen_compara.rendneto_d              , "|",  #55
                          gen_compara.afore_e                 , "|",  #56
                          gen_compara.rend_e                  , "|",  #57
                          gen_compara.comis_e                 , "|",  #58
                          gen_compara.rendneto_e              , "|",  #59
                          gen_compara.afore_f                 , "|",  #60
                          gen_compara.rend_f                  , "|",  #61
                          gen_compara.comis_f                 , "|",  #62
                          gen_compara.rendneto_f              , "|",  #63
                          gen_compara.afore_g                 , "|",  #64
                          gen_compara.rend_g                  , "|",  #65
                          gen_compara.comis_g                 , "|",  #66
                          gen_compara.rendneto_g              , "|",  #67
                          gen_compara.afore_h                 , "|",  #68
                          gen_compara.rend_h                  , "|",  #69
                          gen_compara.comis_h                 , "|",  #70
                          gen_compara.rendneto_h              , "|",  #71
                          gen_compara.afore_i                 , "|",  #72
                          gen_compara.rend_i                  , "|",  #73
                          gen_compara.comis_i                 , "|",  #74
                          gen_compara.rendneto_i              , "|",  #75
                          gen_compara.afore_j                 , "|",  #76
                          gen_compara.rend_j                  , "|",  #77
                          gen_compara.comis_j                 , "|",  #78
                          gen_compara.rendneto_j              , "|",  #79
                          gen_compara.afore_k                 , "|",  #80
                          gen_compara.rend_k                  , "|",  #81
                          gen_compara.comis_k                 , "|",  #82
                          gen_compara.rendneto_k              , "|",  #83
                          gen_compara.afore_l                 , "|",  #84
                          gen_compara.rend_l                  , "|",  #85
                          gen_compara.comis_l                 , "|",  #86
                          gen_compara.rendneto_l              , "|",  #87
                          gen_compara.afore_m                 , "|",  #88
                          gen_compara.rend_m                  , "|",  #89
                          gen_compara.comis_m                 , "|",  #90
                          gen_compara.rendneto_m              , "|",  #91
                          gen_compara.afore_n                 , "|",  #92
                          gen_compara.rend_n                  , "|",  #93
                          gen_compara.comis_n                 , "|",  #94
                          gen_compara.rendneto_n              , "|",  #95
                          gen_compara.afore_o                 , "|",  #96
                          gen_compara.rend_o                  , "|",  #97
                          gen_compara.comis_o                 , "|",  #98
                          gen_compara.rendneto_o              , "|",  #99
                          gen_compara.afore_p                 , "|",  #100
                          gen_compara.rend_p                  , "|",  #101
                          gen_compara.comis_p                 , "|",  #102
                          gen_compara.rendneto_p              , "|",  #103
                          gen_compara.afore_q                 , "|",  #104
                          gen_compara.rend_q                  , "|",  #105
                          gen_compara.comis_q                 , "|",  #106
                          gen_compara.rendneto_q              , "|",  #107
                          gen_compara.afore_r                 , "|",  #108
                          gen_compara.rend_r                  , "|",  #109
                          gen_compara.comis_r                 , "|",  #110
                          gen_compara.rendneto_r              , "|",  #111
                          gen_compara.afore_s                 , "|",  #112
                          gen_compara.rend_s                  , "|",  #113
                          gen_compara.comis_s                 , "|",  #114
                          gen_compara.rendneto_s              , "|",  #115
                          gen_compara.afore_t                 , "|",  #116
                          gen_compara.rend_t                  , "|",  #117
                          gen_compara.comis_t                 , "|",  #118
                          gen_compara.rendneto_t              , "|",  #119
                          gen_compara.afore_u                 , "|",  #120
                          gen_compara.rend_u                  , "|",  #121
                          gen_compara.comis_u                 , "|",  #122
                          gen_compara.rendneto_u              , "|",  #123
                          gen_compara.afore_v                 , "|",  #124
                          gen_compara.rend_v                  , "|",  #125
                          gen_compara.comis_v                 , "|",  #126
                          gen_compara.rendneto_v              , "|",  #127
                          gen_compara.afore_w                 , "|",  #128
                          gen_compara.rend_w                  , "|",  #129
                          gen_compara.comis_w                 , "|",  #130
                          gen_compara.rendneto_w              , "|",  #131
                          gen_compara.afore_x                 , "|",  #132
                          gen_compara.rend_x                  , "|",  #133
                          gen_compara.comis_x                 , "|",  #134
                          gen_compara.rendneto_x              , "|",  #135
                          gen_compara.afore_y                 , "|",  #136
                          gen_compara.rend_y                  , "|",  #137
                          gen_compara.comis_y                 , "|",  #138
                          gen_compara.rendneto_y              , "|",  #139
                          gen_compara.afore_z                 , "|",  #140
                          gen_compara.rend_z                  , "|",  #141
                          gen_compara.comis_z                 , "|",  #142
                          gen_compara.rendneto_z              , "|",  #143
                          barras                              , "|",   #144
                          vprom_simple						 , "|"   #145 #CPL-1182
         
         WHEN "4061" -- Menores de edad
            LET no_carta = "302",ref

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
                          clave_afore USING "&&&"             , "|",  #25
                          des_transferente                    , "|",  #26
                          nacion                              , "|",  #27
                          fena                                , "|",  #28
                          finicta USING "dd/mm/yyyy"          , "|",  #29
                          f_asigna                            , "|",  #30
                          ahorro_retiro                       , "|",  #31
                          sie_rcv                             , "|",  #32
                          ahorro_vol                          , "|",  #33
                          sie_apvol                           , "|",  #34
                          ahorro_viv                          , "|",  #35
                          tot_ahorro                          , "|",  #36
                          asig.largop                         , "|",  #37
                          sie_largop                          , "|",  #38
                          des_transferente                    , "|",  #39
                          gen_compara.afore_a                 , "|",  #40
                          gen_compara.rend_a                  , "|",  #41
                          gen_compara.comis_a                 , "|",  #42
                          gen_compara.rendneto_a              , "|",  #43
                          gen_compara.afore_b                 , "|",  #44
                          gen_compara.rend_b                  , "|",  #45
                          gen_compara.comis_b                 , "|",  #46
                          gen_compara.rendneto_b              , "|",  #47
                          gen_compara.afore_c                 , "|",  #48
                          gen_compara.rend_c                  , "|",  #49
                          gen_compara.comis_c                 , "|",  #50
                          gen_compara.rendneto_c              , "|",  #51
                          gen_compara.afore_d                 , "|",  #52
                          gen_compara.rend_d                  , "|",  #53
                          gen_compara.comis_d                 , "|",  #54
                          gen_compara.rendneto_d              , "|",  #55
                          gen_compara.afore_e                 , "|",  #56
                          gen_compara.rend_e                  , "|",  #57
                          gen_compara.comis_e                 , "|",  #58
                          gen_compara.rendneto_e              , "|",  #59
                          gen_compara.afore_f                 , "|",  #60
                          gen_compara.rend_f                  , "|",  #61
                          gen_compara.comis_f                 , "|",  #62
                          gen_compara.rendneto_f              , "|",  #63
                          gen_compara.afore_g                 , "|",  #64
                          gen_compara.rend_g                  , "|",  #65
                          gen_compara.comis_g                 , "|",  #66
                          gen_compara.rendneto_g              , "|",  #67
                          gen_compara.afore_h                 , "|",  #68
                          gen_compara.rend_h                  , "|",  #69
                          gen_compara.comis_h                 , "|",  #70
                          gen_compara.rendneto_h              , "|",  #71
                          gen_compara.afore_i                 , "|",  #72
                          gen_compara.rend_i                  , "|",  #73
                          gen_compara.comis_i                 , "|",  #74
                          gen_compara.rendneto_i              , "|",  #75
                          gen_compara.afore_j                 , "|",  #76
                          gen_compara.rend_j                  , "|",  #77
                          gen_compara.comis_j                 , "|",  #78
                          gen_compara.rendneto_j              , "|",  #79
                          gen_compara.afore_k                 , "|",  #80
                          gen_compara.rend_k                  , "|",  #81
                          gen_compara.comis_k                 , "|",  #82
                          gen_compara.rendneto_k              , "|",  #83
                          gen_compara.afore_l                 , "|",  #84
                          gen_compara.rend_l                  , "|",  #85
                          gen_compara.comis_l                 , "|",  #86
                          gen_compara.rendneto_l              , "|",  #87
                          gen_compara.afore_m                 , "|",  #88
                          gen_compara.rend_m                  , "|",  #89
                          gen_compara.comis_m                 , "|",  #90
                          gen_compara.rendneto_m              , "|",  #91
                          gen_compara.afore_n                 , "|",  #92
                          gen_compara.rend_n                  , "|",  #93
                          gen_compara.comis_n                 , "|",  #94
                          gen_compara.rendneto_n              , "|",  #95
                          gen_compara.afore_o                 , "|",  #96
                          gen_compara.rend_o                  , "|",  #97
                          gen_compara.comis_o                 , "|",  #98
                          gen_compara.rendneto_o              , "|",  #99
                          gen_compara.afore_p                 , "|",  #100
                          gen_compara.rend_p                  , "|",  #101
                          gen_compara.comis_p                 , "|",  #102
                          gen_compara.rendneto_p              , "|",  #103
                          gen_compara.afore_q                 , "|",  #104
                          gen_compara.rend_q                  , "|",  #105
                          gen_compara.comis_q                 , "|",  #106
                          gen_compara.rendneto_q              , "|",  #107
                          gen_compara.afore_r                 , "|",  #108
                          gen_compara.rend_r                  , "|",  #109
                          gen_compara.comis_r                 , "|",  #110
                          gen_compara.rendneto_r              , "|",  #111
                          gen_compara.afore_s                 , "|",  #112
                          gen_compara.rend_s                  , "|",  #113
                          gen_compara.comis_s                 , "|",  #114
                          gen_compara.rendneto_s              , "|",  #115
                          gen_compara.afore_t                 , "|",  #116
                          gen_compara.rend_t                  , "|",  #117
                          gen_compara.comis_t                 , "|",  #118
                          gen_compara.rendneto_t              , "|",  #119
                          gen_compara.afore_u                 , "|",  #120
                          gen_compara.rend_u                  , "|",  #121
                          gen_compara.comis_u                 , "|",  #122
                          gen_compara.rendneto_u              , "|",  #123
                          gen_compara.afore_v                 , "|",  #124
                          gen_compara.rend_v                  , "|",  #125
                          gen_compara.comis_v                 , "|",  #126
                          gen_compara.rendneto_v              , "|",  #127
                          gen_compara.afore_w                 , "|",  #128
                          gen_compara.rend_w                  , "|",  #129
                          gen_compara.comis_w                 , "|",  #130
                          gen_compara.rendneto_w              , "|",  #131
                          gen_compara.afore_x                 , "|",  #132
                          gen_compara.rend_x                  , "|",  #133
                          gen_compara.comis_x                 , "|",  #134
                          gen_compara.rendneto_x              , "|",  #135
                          gen_compara.afore_y                 , "|",  #136
                          gen_compara.rend_y                  , "|",  #137
                          gen_compara.comis_y                 , "|",  #138
                          gen_compara.rendneto_y              , "|",  #139
                          gen_compara.afore_z                 , "|",  #140
                          gen_compara.rend_z                  , "|",  #141
                          gen_compara.comis_z                 , "|",  #142
                          gen_compara.rendneto_z              , "|",  #143
                          barras                              , "|",   #144
                          vprom_simple						 , "|"   #145 #CPL-1182
                                          
         WHEN "458"
            LET no_carta = "305",ref

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
                          clave_afore USING "&&&"             , "|",  #25
                          des_transferente                    , "|",  #26
                          nacion                              , "|",  #27
                          fena                                , "|",  #28
                          finicta USING "dd/mm/yyyy"          , "|",  #29
                          f_asigna                            , "|",  #30
                          ahorro_retiro                       , "|",  #31
                          sie_rcv                             , "|",  #32
                          ahorro_vol                          , "|",  #33
                          sie_apvol                           , "|",  #34
                          ahorro_viv                          , "|",  #35
                          tot_ahorro                          , "|",  #36
                          asig.largop                         , "|",  #37
                          sie_largop                          , "|",  #38
                          des_transferente                    , "|",  #39
                          gen_compara.afore_a                 , "|",  #40
                          gen_compara.rend_a                  , "|",  #41
                          gen_compara.comis_a                 , "|",  #42
                          gen_compara.rendneto_a              , "|",  #43
                          gen_compara.afore_b                 , "|",  #44
                          gen_compara.rend_b                  , "|",  #45
                          gen_compara.comis_b                 , "|",  #46
                          gen_compara.rendneto_b              , "|",  #47
                          gen_compara.afore_c                 , "|",  #48
                          gen_compara.rend_c                  , "|",  #49
                          gen_compara.comis_c                 , "|",  #50
                          gen_compara.rendneto_c              , "|",  #51
                          gen_compara.afore_d                 , "|",  #52
                          gen_compara.rend_d                  , "|",  #53
                          gen_compara.comis_d                 , "|",  #54
                          gen_compara.rendneto_d              , "|",  #55
                          gen_compara.afore_e                 , "|",  #56
                          gen_compara.rend_e                  , "|",  #57
                          gen_compara.comis_e                 , "|",  #58
                          gen_compara.rendneto_e              , "|",  #59
                          gen_compara.afore_f                 , "|",  #60
                          gen_compara.rend_f                  , "|",  #61
                          gen_compara.comis_f                 , "|",  #62
                          gen_compara.rendneto_f              , "|",  #63
                          gen_compara.afore_g                 , "|",  #64
                          gen_compara.rend_g                  , "|",  #65
                          gen_compara.comis_g                 , "|",  #66
                          gen_compara.rendneto_g              , "|",  #67
                          gen_compara.afore_h                 , "|",  #68
                          gen_compara.rend_h                  , "|",  #69
                          gen_compara.comis_h                 , "|",  #70
                          gen_compara.rendneto_h              , "|",  #71
                          gen_compara.afore_i                 , "|",  #72
                          gen_compara.rend_i                  , "|",  #73
                          gen_compara.comis_i                 , "|",  #74
                          gen_compara.rendneto_i              , "|",  #75
                          gen_compara.afore_j                 , "|",  #76
                          gen_compara.rend_j                  , "|",  #77
                          gen_compara.comis_j                 , "|",  #78
                          gen_compara.rendneto_j              , "|",  #79
                          gen_compara.afore_k                 , "|",  #80
                          gen_compara.rend_k                  , "|",  #81
                          gen_compara.comis_k                 , "|",  #82
                          gen_compara.rendneto_k              , "|",  #83
                          gen_compara.afore_l                 , "|",  #84
                          gen_compara.rend_l                  , "|",  #85
                          gen_compara.comis_l                 , "|",  #86
                          gen_compara.rendneto_l              , "|",  #87
                          gen_compara.afore_m                 , "|",  #88
                          gen_compara.rend_m                  , "|",  #89
                          gen_compara.comis_m                 , "|",  #90
                          gen_compara.rendneto_m              , "|",  #91
                          gen_compara.afore_n                 , "|",  #92
                          gen_compara.rend_n                  , "|",  #93
                          gen_compara.comis_n                 , "|",  #94
                          gen_compara.rendneto_n              , "|",  #95
                          gen_compara.afore_o                 , "|",  #96
                          gen_compara.rend_o                  , "|",  #97
                          gen_compara.comis_o                 , "|",  #98
                          gen_compara.rendneto_o              , "|",  #99
                          gen_compara.afore_p                 , "|",  #100
                          gen_compara.rend_p                  , "|",  #101
                          gen_compara.comis_p                 , "|",  #102
                          gen_compara.rendneto_p              , "|",  #103
                          gen_compara.afore_q                 , "|",  #104
                          gen_compara.rend_q                  , "|",  #105
                          gen_compara.comis_q                 , "|",  #106
                          gen_compara.rendneto_q              , "|",  #107
                          gen_compara.afore_r                 , "|",  #108
                          gen_compara.rend_r                  , "|",  #109
                          gen_compara.comis_r                 , "|",  #110
                          gen_compara.rendneto_r              , "|",  #111
                          gen_compara.afore_s                 , "|",  #112
                          gen_compara.rend_s                  , "|",  #113
                          gen_compara.comis_s                 , "|",  #114
                          gen_compara.rendneto_s              , "|",  #115
                          gen_compara.afore_t                 , "|",  #116
                          gen_compara.rend_t                  , "|",  #117
                          gen_compara.comis_t                 , "|",  #118
                          gen_compara.rendneto_t              , "|",  #119
                          gen_compara.afore_u                 , "|",  #120
                          gen_compara.rend_u                  , "|",  #121
                          gen_compara.comis_u                 , "|",  #122
                          gen_compara.rendneto_u              , "|",  #123
                          gen_compara.afore_v                 , "|",  #124
                          gen_compara.rend_v                  , "|",  #125
                          gen_compara.comis_v                 , "|",  #126
                          gen_compara.rendneto_v              , "|",  #127
                          gen_compara.afore_w                 , "|",  #128
                          gen_compara.rend_w                  , "|",  #129
                          gen_compara.comis_w                 , "|",  #130
                          gen_compara.rendneto_w              , "|",  #131
                          gen_compara.afore_x                 , "|",  #132
                          gen_compara.rend_x                  , "|",  #133
                          gen_compara.comis_x                 , "|",  #134
                          gen_compara.rendneto_x              , "|",  #135
                          gen_compara.afore_y                 , "|",  #136
                          gen_compara.rend_y                  , "|",  #137
                          gen_compara.comis_y                 , "|",  #138
                          gen_compara.rendneto_y              , "|",  #139
                          gen_compara.afore_z                 , "|",  #140
                          gen_compara.rend_z                  , "|",  #141
                          gen_compara.comis_z                 , "|",  #142
                          gen_compara.rendneto_z              , "|",  #143
                          barras                              , "|",   #144
                          vprom_simple						 , "|"   #145 #CPL-1182
         END CASE
##genera arch.

     OUTPUT TO REPORT r_report(v_rep,prog,det_cza)

     SET LOCK MODE TO WAIT
     INSERT INTO int_his_emision_docto VALUES ( prog, p_mae_afi.nss,
                                                fe_carta,
                                                no_carta,
                                                v_rep,
                                                usuario)
       CASE prog 
       WHEN "407"
          UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                 int_ctr_carta.fecha_genera = fe_carta,
                 int_ctr_carta.hora_genera  = ho_ra ,
                 int_ctr_carta.consecutivo  = consecutivo_envio
           WHERE int_ctr_carta.nss = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
             AND int_ctr_carta.docto_cod = 30202
             AND int_ctr_carta.edo_genera   = 10
       
        WHEN "4071" -- Menores de edad
          UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                 int_ctr_carta.fecha_genera = fe_carta,
                 int_ctr_carta.hora_genera  = ho_ra ,
                 int_ctr_carta.consecutivo  = consecutivo_envio
           WHERE int_ctr_carta.nss            = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
             AND int_ctr_carta.docto_cod      = 30202
             AND int_ctr_carta.edo_genera     = 10      
             
       WHEN "406"
          UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                 int_ctr_carta.fecha_genera = fe_carta,
                 int_ctr_carta.hora_genera  = ho_ra ,
                 int_ctr_carta.consecutivo  = consecutivo_envio
           WHERE int_ctr_carta.nss = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
             AND int_ctr_carta.docto_cod = 30201
             AND int_ctr_carta.edo_genera   = 10
             
       WHEN "4061" -- Menores de edad
          UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                 int_ctr_carta.fecha_genera = fe_carta,
                 int_ctr_carta.hora_genera  = ho_ra ,
                 int_ctr_carta.consecutivo  = consecutivo_envio
           WHERE int_ctr_carta.nss = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
             AND int_ctr_carta.docto_cod = 30201
             AND int_ctr_carta.edo_genera   = 10
             
       WHEN "458"
          UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                 int_ctr_carta.fecha_genera = fe_carta,
                 int_ctr_carta.hora_genera  = ho_ra ,
                 int_ctr_carta.consecutivo  = consecutivo_envio
           WHERE int_ctr_carta.nss = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
             AND int_ctr_carta.docto_cod = 30501
             AND int_ctr_carta.edo_genera   = 10
       END CASE

     INSERT INTO int_constancia VALUES ( folio,
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
FUNCTION cabeza_notifica9(ref,leyenda_cza,prog,det_cza)

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

   CASE prog
        WHEN "406"  LET no_carta = "302",ref
        WHEN "4061" LET no_carta = "302",ref  -- Menores de edad      	
        WHEN "407"  LET no_carta = "302",ref
        WHEN "4071" LET no_carta = "302",ref  -- Menores de edad        	
        WHEN "458"  LET no_carta = "305",ref
   END CASE

   LET fecha_creacion = TODAY USING "MM/DD/YYYY"

   LET v_rep = "01",                         "|",
               no_carta,                     "|",
               leyenda_cza,                  "|",
               afore_local,                  "|",
               fecha_creacion,               "|",
               consec_lote USING "&&&&&&&&", "|",
               numero_reg  USING "&&&&&&&&", "|"

   OUTPUT TO REPORT r_report(v_rep,prog,det_cza)
{
   INSERT INTO int_notifica_folio 
               VALUES (consec_lote, fecha_creacion, ref, numero_reg)
}
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

FUNCTION detalle_notifica91(ref,prog,det_cza)
  
   DEFINE prog     SMALLINT,
          det_cza  SMALLINT,
          ref      CHAR(02),
          csie_rcv     SMALLINT,
          csie_ret92   SMALLINT,
          csie_ahorro  SMALLINT,
          csie_apcom   SMALLINT,
          csie_apvol   SMALLINT,
          csie_largop  SMALLINT,

          nombre_sie   CHAR(08),
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

          dia_hoy          DATE,

          rcv_1             DECIMAL(11,2),
          ret92_1           DECIMAL(11,2),
          ahorro_1          DECIMAL(11,2),
          apcom_1           DECIMAL(11,2),
          apvol_1           DECIMAL(11,2),
          viv97_1           DECIMAL(11,2),
          viv92_1           DECIMAL(11,2),
          fondo_1           DECIMAL(11,2),
          largop_1          DECIMAL(11,2),

          rcv_2             DECIMAL(11,2),
          ret92_2           DECIMAL(11,2),
          ahorro_2          DECIMAL(11,2),
          apcom_2           DECIMAL(11,2),
          apvol_2           DECIMAL(11,2),
          viv97_2           DECIMAL(11,2),
          viv92_2           DECIMAL(11,2),
          fondo_2           DECIMAL(11,2),
          largop_2          DECIMAL(11,2),

--->mas siefores
          rcv_3             DECIMAL(11,2),
          ret92_3           DECIMAL(11,2),
          ahorro_3          DECIMAL(11,2),
          apcom_3           DECIMAL(11,2),
          apvol_3           DECIMAL(11,2),
          viv97_3           DECIMAL(11,2),
          viv92_3           DECIMAL(11,2),
          fondo_3           DECIMAL(11,2),
          largop_3          DECIMAL(11,2),

          rcv_4             DECIMAL(11,2),
          ret92_4           DECIMAL(11,2),
          ahorro_4          DECIMAL(11,2),
          apcom_4           DECIMAL(11,2),
          apvol_4           DECIMAL(11,2),
          viv97_4           DECIMAL(11,2),
          viv92_4           DECIMAL(11,2),
          fondo_4           DECIMAL(11,2),
          largop_4          DECIMAL(11,2),

          rcv_5             DECIMAL(11,2),
          ret92_5           DECIMAL(11,2),
          ahorro_5          DECIMAL(11,2),
          apcom_5           DECIMAL(11,2),
          apvol_5           DECIMAL(11,2),
          viv97_5           DECIMAL(11,2),
          viv92_5           DECIMAL(11,2),
          fondo_5           DECIMAL(11,2),
          largop_5          DECIMAL(11,2),

--#INV-5412  siefore 14
          rcv_14             DECIMAL(11,2),
          ret92_14           DECIMAL(11,2),
          ahorro_14          DECIMAL(11,2),
          apcom_14           DECIMAL(11,2),
          apvol_14           DECIMAL(11,2),
          viv97_14           DECIMAL(11,2),
          viv92_14           DECIMAL(11,2),
          fondo_14           DECIMAL(11,2),
          largop_14          DECIMAL(11,2),   

--#INV-5412  siefore 15
          rcv_15             DECIMAL(11,2),
          ret92_15           DECIMAL(11,2),
          ahorro_15          DECIMAL(11,2),
          apcom_15           DECIMAL(11,2),
          apvol_15           DECIMAL(11,2),
          viv97_15           DECIMAL(11,2),
          viv92_15           DECIMAL(11,2),
          fondo_15           DECIMAL(11,2),
          largop_15          DECIMAL(11,2), 

--#INV-5412  siefore 16
          rcv_16             DECIMAL(11,2),
          ret92_16           DECIMAL(11,2),
          ahorro_16          DECIMAL(11,2),
          apcom_16           DECIMAL(11,2),
          apvol_16           DECIMAL(11,2),
          viv97_16           DECIMAL(11,2),
          viv92_16           DECIMAL(11,2),
          fondo_16           DECIMAL(11,2),
          largop_16          DECIMAL(11,2), 

--#INV-5412  siefore 17
          rcv_17             DECIMAL(11,2),
          ret92_17           DECIMAL(11,2),
          ahorro_17          DECIMAL(11,2),
          apcom_17           DECIMAL(11,2),
          apvol_17           DECIMAL(11,2),
          viv97_17           DECIMAL(11,2),
          viv92_17           DECIMAL(11,2),
          fondo_17           DECIMAL(11,2),
          largop_17          DECIMAL(11,2), 

--#INV-5412  siefore 18
          rcv_18             DECIMAL(11,2),
          ret92_18           DECIMAL(11,2),
          ahorro_18          DECIMAL(11,2),
          apcom_18           DECIMAL(11,2),
          apvol_18           DECIMAL(11,2),
          viv97_18           DECIMAL(11,2),
          viv92_18           DECIMAL(11,2),
          fondo_18           DECIMAL(11,2),
          largop_18          DECIMAL(11,2), 

--#INV-5412  siefore 19
          rcv_19             DECIMAL(11,2),
          ret92_19           DECIMAL(11,2),
          ahorro_19          DECIMAL(11,2),
          apcom_19           DECIMAL(11,2),
          apvol_19           DECIMAL(11,2),
          viv97_19           DECIMAL(11,2),
          viv92_19           DECIMAL(11,2),
          fondo_19           DECIMAL(11,2),
          largop_19          DECIMAL(11,2), 

--#INV-5412  siefore 20
          rcv_20             DECIMAL(11,2),
          ret92_20           DECIMAL(11,2),
          ahorro_20          DECIMAL(11,2),
          apcom_20           DECIMAL(11,2),
          apvol_20           DECIMAL(11,2),
          viv97_20           DECIMAL(11,2),
          viv92_20           DECIMAL(11,2),
          fondo_20           DECIMAL(11,2),
          largop_20          DECIMAL(11,2), 

--#INV-5412  siefore 21
          rcv_21             DECIMAL(11,2),
          ret92_21           DECIMAL(11,2),
          ahorro_21          DECIMAL(11,2),
          apcom_21           DECIMAL(11,2),
          apvol_21           DECIMAL(11,2),
          viv97_21           DECIMAL(11,2),
          viv92_21           DECIMAL(11,2),
          fondo_21           DECIMAL(11,2),
          largop_21          DECIMAL(11,2), 

--#INV-5412  siefore 22
          rcv_22             DECIMAL(11,2),
          ret92_22           DECIMAL(11,2),
          ahorro_22          DECIMAL(11,2),
          apcom_22           DECIMAL(11,2),
          apvol_22           DECIMAL(11,2),
          viv97_22           DECIMAL(11,2),
          viv92_22           DECIMAL(11,2),
          fondo_22           DECIMAL(11,2),
          largop_22          DECIMAL(11,2),          
---<

          ahorro_retiro,
          ahorro_vol,
          ahorro_viv,
          ahorro_lplazo     DECIMAL(13,2),

          tot_ahorro        DECIMAL(15,2),

          v_anio_finicta,   
          v_anio_actual      CHAR(4),
          lc_query           CHAR(3000),
          v_precio           DECIMAL(19,14),
          v_siefore          SMALLINT,
          v_tabla            VARCHAR(13),
          v_subcuenta        CHAR(9),
          v_sub_1,
          v_sub_2,
          v_sub_3,
          v_sub_4,
          v_sub_6,
          v_sub_7,
          v_existe_dis_ctaxx INTEGER

  LET dia_hoy  = TODAY
     
   CALL limpia()
   INITIALIZE p_mae_afi.* TO NULL
   INITIALIZE v_siefore, v_tabla, v_sub_1, v_sub_2, v_sub_3, v_sub_4, v_sub_6, v_sub_7 TO NULL
      
   PREPARE apt_091 FROM selec_tab
   DECLARE cur_091 CURSOR FOR apt_091
   FOREACH cur_091 INTO p_mae_afi.*

{       ## TIPO TRABAJADOR - TIPO ADMINISTRACION  
       SELECT a.tipo_trab_ind, a.tipo_administracion
         INTO vtipo_trab_ind, vtipo_administracion
         FROM cta_ctr_reg_ind a
        WHERE a.curp = p_mae_afi.curp
          AND a.nti  = p_mae_afi.nss        
    
       ##Independientes
       IF vtipo_trab_ind       = '1'  AND
          vtipo_administracion = '01' THEN
          LET tipo_com         = 3       
       END IF
    
       ##Issste - Banxico Icefa
       IF vtipo_trab_ind       = '2'  AND
          vtipo_administracion = '07' THEN
          LET tipo_com         = 5       
       END IF
    
       ##Issste - Afore Siefore
       IF vtipo_trab_ind       = '2'  AND
          vtipo_administracion = '01' THEN
          LET tipo_com         = 7       
       END IF}

       CALL llena_comision_registro(p_mae_afi.finicta, p_mae_afi.nss) RETURNING gen_compara.*,ban
       
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
      WHERE p_mae_afi.finicta BETWEEN a.fecha_ini AND a.fecha_fin
      AND a.siefore_cod = vsiefore
      
	   LET vprom_simple    = prom_simple USING "##&&.&&","%" #CPL-1182
	   Display "Promedio simple", vprom_simple
	   sleep 3	
		
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
            FOR i = long TO 40
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

         IF p_mae_afi.fena = " "   OR
            p_mae_afi.fena IS NULL THEN
             LET fena = "           "
         ELSE
               LET fena = p_mae_afi.fena USING "DD/MM/YYYY"
         END IF

         IF p_mae_afi.finitmte = " "   OR
            p_mae_afi.finitmte IS NULL THEN
                LET f_asigna = "           "
         ELSE
                  LET f_asigna = p_mae_afi.finitmte USING "DD/MM/YYYY"
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

##telefono
         DECLARE apt_tel_91 CURSOR FOR
             SELECT n.telefono FROM afi_telefono n
                    WHERE n.nss      = p_mae_afi.nss
                      AND n.n_folio        = p_mae_afi.folio
                      AND n.tipo_solicitud = p_mae_afi.tipo_solicitud
                      AND n.telefono IS NOT NULL
                      AND n.tel_cod  < 7
         FOREACH apt_tel_91 INTO p_telefono.*
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

       INITIALIZE sie.*,sie_rcv,sie_ret92,sie_apvol TO NULL
       INITIALIZE sie_ahorro,sie_apcom,sie_largop TO NULL

       LET sie_rcv = 0
       LET sie_ret92 = 0
       LET sie_ahorro = 0
       LET sie_apcom = 0
       LET sie_apvol = 0
       LET sie_largop = 0

         DECLARE ap_sie_91 CURSOR FOR
             SELECT m.subcuenta,m.codigo_siefore FROM cta_regimen m
             WHERE  m.nss       = p_mae_afi.nss
             AND    m.subcuenta IN(1,7,10,11,13,15)
         FOREACH ap_sie_91 INTO  sie.*

            INITIALIZE nombre_sie TO NULL
            SELECT UNIQUE razon_social[1,8] INTO nombre_sie from tab_siefore_local
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

         LET finicta = p_mae_afi.finicta

##fecha_carta
         LET ho_ra = TIME
         LET fe_carta = TODAY USING "mm/dd/yyyy"
##entidad
         LET entidad = "    "
##fecha generacion de la carta
         LET fecha_creacion = TODAY USING "dd/mm/yyyy"
         LET fecha_barra    = TODAY USING "yyyymmdd"
##no_carta
##codigo_barras

         LET barras = "C",no_carta,"000",p_mae_afi.nss,
                      fecha_barra

         LET afore_xx = "                    "
         LET comis_xx = "       "

         IF prog = "407" or prog = "4071" THEN

            LET tipo_proceso = "R"

            LET asig.rcv     = 0
            LET asig.ret92   = 0
            LET asig.ahorro  = 0
            LET asig.apcom   = 0
            LET asig.apvol   = 0
            LET asig.s_inver = 0
            LET asig.viv97   = 0
            LET asig.viv92   = 0
            LET asig.s_viv   = 0
            LET asig.fondo   = 0
            LET asig.largop  = 0

            LET rcv_1 = 0   
            LET rcv_2 = 0
            LET rcv_3 = 0
            LET rcv_4 = 0
            LET rcv_5 = 0            
            LET rcv_14 = 0   #INV-5412
            LET rcv_15 = 0   #INV-5412
            LET rcv_16 = 0   #INV-5412
            LET rcv_17 = 0   #INV-5412
            LET rcv_18 = 0   #INV-5412
            LET rcv_19 = 0   #INV-5412
            LET rcv_20 = 0   #INV-5412
            LET rcv_21 = 0   #INV-5412
            LET rcv_22 = 0   #INV-5412
             
            LET precio_1 = 0  
            LET precio_2 = 0  
            LET precio_3 = 0  
            LET precio_4 = 0  
            LET precio_5 = 0  
            LET precio_14 = 0 #INV-5412
            LET precio_15 = 0 #INV-5412
            LET precio_16 = 0 #INV-5412
            LET precio_17 = 0 #INV-5412
            LET precio_18 = 0 #INV-5412
            LET precio_19 = 0 #INV-5412
            LET precio_20 = 0 #INV-5412
            LET precio_21 = 0 #INV-5412
            LET precio_22 = 0 #INV-5412

            SELECT a.precio_del_dia INTO precio_1 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 1

            SELECT a.precio_del_dia INTO precio_2 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 2

--->mas siefores
            SELECT a.precio_del_dia INTO precio_3 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 3

            SELECT a.precio_del_dia INTO precio_4 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 4

            SELECT a.precio_del_dia INTO precio_5 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 5

            SELECT a.precio_del_dia INTO precio_14 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 14 #INV-5412

            SELECT a.precio_del_dia INTO precio_15 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 15 #INV-5412    

            SELECT a.precio_del_dia INTO precio_16 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 16 #INV-5412 

            SELECT a.precio_del_dia INTO precio_17 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 17 #INV-5412

            SELECT a.precio_del_dia INTO precio_18 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 18 #INV-5412   

            SELECT a.precio_del_dia INTO precio_19 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 19 #INV-5412   

            SELECT a.precio_del_dia INTO precio_20 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 20 #INV-5412

            SELECT a.precio_del_dia INTO precio_21 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 21 #INV-5412

            SELECT a.precio_del_dia INTO precio_22 FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 22 #INV-5412              
---<
------------------------------------------------------------------------------------------------------ PRIMERAS
         LET v_anio_finicta = YEAR(p_mae_afi.finicta)
         LET v_anio_actual =  YEAR(TODAY)
            
         FOR v_sub_1 = 1 TO 14
            CASE v_sub_1
               WHEN 1  -- rcv_1
                  LET v_precio = precio_1
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 1
               WHEN 2  -- rcv_2
                  LET v_precio = precio_2
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 2
               WHEN 3  -- rcv_3
                  LET v_precio = precio_3
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 3
               WHEN 4  -- rcv_4
                  LET v_precio = precio_4
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 4
               WHEN 5  -- rcv_5
                  LET v_precio = precio_5
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 5
               WHEN 6  -- rcv_14
                  LET v_precio = precio_14
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 14
               WHEN 7  -- rcv_15
                  LET v_precio = precio_15
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 15
               WHEN 8  -- rcv_16
                  LET v_precio = precio_16
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 16
               WHEN 9  -- rcv_17
                  LET v_precio = precio_17
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 17
               WHEN 10  -- rcv_18
                  LET v_precio = precio_18
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 18
               WHEN 11  -- rcv_19
                  LET v_precio = precio_19
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 19
               WHEN 12  -- rcv_20
                  LET v_precio = precio_20
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 20
               WHEN 13  -- rcv_21
                  LET v_precio = precio_21
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 21
               WHEN 14  -- rcv_22
                  LET v_precio = precio_22
                  LET v_subcuenta = "(1,2,5,6,9)"
                  LET v_siefore = 22
            END CASE

            INITIALIZE lc_query, v_tabla TO NULL
            
            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta IN (1,2,5,6,9)",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta IN (1,2,5,6,9)",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                 ELSE
                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta",
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta IN (1,2,5,6,9)",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                  END IF

               END IF

                {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_1,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_1 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
--                     AND bb.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
--                     AND bb.siefore          = csie_rcv
                     AND bb.siefore          = 1}
            IF v_sub_1 = 1 THEN 
               PREPARE prp_rcv_1 FROM lc_query
               EXECUTE prp_rcv_1 INTO rcv_1
            END IF
            
            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_2,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_2 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 2}
            IF v_sub_1 = 2 THEN 
               PREPARE prp_rcv_2 FROM lc_query
               EXECUTE prp_rcv_2 INTO rcv_2
            END IF

--->mas siefores
            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_3,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_3 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 3}
            IF v_sub_1 = 3 THEN 
               PREPARE prp_rcv_3 FROM lc_query
               EXECUTE prp_rcv_3 INTO rcv_3
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_4,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_4 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 4}
            IF v_sub_1 = 4 THEN 
               PREPARE prp_rcv_4 FROM lc_query
               EXECUTE prp_rcv_4 INTO rcv_4
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_5,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_5 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 5}
            IF v_sub_1 = 5 THEN
               PREPARE prp_rcv_5 FROM lc_query
               EXECUTE prp_rcv_5 INTO rcv_5
            END IF

--CPL-3045
            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_14,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_14 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 14}
            IF v_sub_1 = 6 THEN
               PREPARE prp_rcv_14 FROM lc_query
               EXECUTE prp_rcv_14 INTO rcv_14
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_15,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_15 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 15}
            IF v_sub_1 = 7 THEN
               PREPARE prp_rcv_15 FROM lc_query
               EXECUTE prp_rcv_15 INTO rcv_15
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_16,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_16 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 16}
            IF v_sub_1 = 8 THEN
               PREPARE prp_rcv_16 FROM lc_query
               EXECUTE prp_rcv_16 INTO rcv_16
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_17,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_17 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 17 }
            IF v_sub_1 = 9 THEN
               PREPARE prp_rcv_17 FROM lc_query
               EXECUTE prp_rcv_17 INTO rcv_17
            END IF    

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_18,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_18 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 18}
            IF v_sub_1 = 10 THEN
               PREPARE prp_rcv_18 FROM lc_query
               EXECUTE prp_rcv_18 INTO rcv_18
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_19,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_19 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 19}
            IF v_sub_1 = 11 THEN
               PREPARE prp_rcv_19 FROM lc_query
               EXECUTE prp_rcv_19 INTO rcv_19
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_20,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_20 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 20}
            IF v_sub_1 = 12 THEN
               PREPARE prp_rcv_20 FROM lc_query
               EXECUTE prp_rcv_20 INTO rcv_20
            END IF

            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_21,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_21 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 21}
            IF v_sub_1 = 13 THEN
               PREPARE prp_rcv_21 FROM lc_query
               EXECUTE prp_rcv_21 INTO rcv_21
            END IF
            
            {SELECT ROUND(SUM(bb.monto_en_acciones)* precio_22,2)
                 --INTO asig.rcv FROM dis_cuenta bb
                 INTO rcv_22 FROM dis_cuenta bb
                   WHERE bb.nss              = p_mae_afi.nss
                     AND bb.subcuenta        IN(1,2,5,6,9)
                     AND bb.tipo_movimiento  > 0
                     AND bb.siefore          = 22}
            IF v_sub_1 = 14 THEN
               PREPARE prp_rcv_22 FROM lc_query
               EXECUTE prp_rcv_22 INTO rcv_22
            END IF
         END FOR ---------------------------------------------------------------------------------- 1
---<

            IF rcv_1 IS NULL THEN
                LET rcv_1 = 0
            END IF

            IF rcv_2 IS NULL THEN
                LET rcv_2 = 0
            END IF

--->mas siefores
            IF rcv_3 IS NULL THEN
                LET rcv_3 = 0
            END IF

            IF rcv_4 IS NULL THEN
                LET rcv_4 = 0
            END IF

            IF rcv_5 IS NULL THEN
                LET rcv_5 = 0
            END IF

            IF rcv_14 IS NULL THEN
                LET rcv_14 = 0
            END IF    

            IF rcv_15 IS NULL THEN
                LET rcv_15 = 0
            END IF 

            IF rcv_16 IS NULL THEN
                LET rcv_16 = 0
            END IF      

            IF rcv_17 IS NULL THEN
                LET rcv_17 = 0
            END IF   

            IF rcv_18 IS NULL THEN
                LET rcv_18 = 0
            END IF   

            IF rcv_19 IS NULL THEN
                LET rcv_19 = 0
            END IF     

            IF rcv_20 IS NULL THEN
                LET rcv_20 = 0
            END IF    

            IF rcv_21 IS NULL THEN
                LET rcv_21 = 0
            END IF                                                                                   
---<

            LET asig.rcv = rcv_1 + rcv_2
                         + rcv_3 + rcv_4 + rcv_5 +        ---mas siefores
                           rcv_14 + rcv_15 + rcv_16 +
                           rcv_17 + rcv_18 + rcv_19 + 
                           rcv_20 + rcv_21 + rcv_22                         

--display "asig.rcv1 :",asig.rcv
            IF asig.rcv IS NULL THEN
               LET asig.rcv = 0
            END IF

            LET ret92_1 = 0  LET ret92_2 = 0  --LET precio = 0
            LET ret92_3 = 0  LET ret92_4 = 0  LET ret92_5 = 0    ---mas siefores
            LET ret92_14 = 0   LET ret92_15 = 0  LET ret92_16 = 0
            LET ret92_17 = 0   LET ret92_18 = 0  LET ret92_19 = 0
            LET ret92_20 = 0   LET ret92_21 = 0  LET ret92_22 = 0            

---------------------------------------------------------------------------------------------------------------------- SEGUNDAS
         FOR v_sub_2 = 1 TO 14
            CASE v_sub_2
               WHEN 1  -- ret92_1
                  LET v_precio = precio_1
                  LET v_siefore = 1

               WHEN 2  -- ret92_2
                  LET v_precio = precio_2
                  LET v_siefore = 2

               WHEN 3  -- ret92_3 
                  LET v_precio = precio_3
                  LET v_siefore = 3

               WHEN 4  -- ret92_4
                  LET v_precio = precio_4
                  LET v_siefore = 4

               WHEN 5  -- ret92_5
                  LET v_precio = precio_5
                  LET v_siefore = 5

               WHEN 6  -- ret92_14
                  LET v_precio = precio_14
                  LET v_siefore = 14

               WHEN 7  -- 
                  LET v_precio = precio_15
                  LET v_siefore = 15

               WHEN 8  -- ret92_16
                  LET v_precio = precio_16
                  LET v_siefore = 16

               WHEN 9  -- 
                  LET v_precio = precio_17
                  LET v_siefore = 17

               WHEN 10  -- ret92_18
                  LET v_precio = precio_18
                  LET v_siefore = 18

               WHEN 11  -- ret92_19
                  LET v_precio = precio_19
                  LET v_siefore = 19

               WHEN 12  -- ret92_20
                  LET v_precio = precio_20
                  LET v_siefore = 20

               WHEN 13  -- ret92_21
                  LET v_precio = precio_21
                  LET v_siefore = 21

               WHEN 14  -- ret92_22
                  LET v_precio = precio_22
                  LET v_siefore = 22
            END CASE 

            INITIALIZE lc_query, v_tabla TO NULL

            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = 7",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = 7",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                 ELSE
                    LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = 7",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                  END IF

               END IF
            ---
            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_1,2)
                 INTO ret92_1 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
--                     AND h.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
--                     AND h.siefore          = csie_ret92
                     AND h.siefore          = 1}
            IF v_sub_2 = 1 THEN
               PREPARE prp_ret92_1 FROM lc_query
               EXECUTE prp_ret92_1 INTO ret92_1
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_2,2)
                 INTO ret92_2 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
--                     AND h.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 2}
            IF v_sub_2 = 2 THEN
               PREPARE prp_ret92_2 FROM lc_query
               EXECUTE prp_ret92_2 INTO ret92_2
            END IF

--->mas siefores
            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_3,2)
                 INTO ret92_3 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 3}
            IF v_sub_2 = 3 THEN
               PREPARE prp_ret92_3 FROM lc_query
               EXECUTE prp_ret92_3 INTO ret92_3
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_4,2)
                 INTO ret92_4 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 4}
            IF v_sub_2 = 4 THEN
               PREPARE prp_ret92_4 FROM lc_query
               EXECUTE prp_ret92_4 INTO ret92_4
            END IF
            
            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_5,2)
                 INTO ret92_5 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 5}
            IF v_sub_2 = 5 THEN
               PREPARE prp_ret92_5 FROM lc_query
               EXECUTE prp_ret92_5 INTO ret92_5
            END IF
--CPL-3045
            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_14,2)
                 INTO ret92_14 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 14}
            IF v_sub_2 = 6 THEN
               PREPARE prp_ret92_14 FROM lc_query
               EXECUTE prp_ret92_14 INTO ret92_14
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_15,2)
                 INTO ret92_15 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 15}
            IF v_sub_2 = 7 THEN
               PREPARE prp_ret92_15 FROM lc_query
               EXECUTE prp_ret92_15 INTO ret92_15
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_16,2)
                 INTO ret92_16 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 16}
            IF v_sub_2 = 8 THEN
               PREPARE prp_ret92_16 FROM lc_query
               EXECUTE prp_ret92_16 INTO ret92_16
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_17,2)
                 INTO ret92_17 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 17}
            IF v_sub_2 = 9 THEN
               PREPARE prp_ret92_17 FROM lc_query
               EXECUTE prp_ret92_17 INTO ret92_17
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_18,2)
                 INTO ret92_18 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 18}
            IF v_sub_2 = 10 THEN
               PREPARE prp_ret92_18 FROM lc_query
               EXECUTE prp_ret92_18 INTO ret92_18
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_19,2)
                 INTO ret92_19 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 19}
            IF v_sub_2 = 11 THEN
               PREPARE prp_ret92_19 FROM lc_query
               EXECUTE prp_ret92_19 INTO ret92_19
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_20,2)
                 INTO ret92_20 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 20}
            IF v_sub_2 = 12 THEN
               PREPARE prp_ret92_20 FROM lc_query
               EXECUTE prp_ret92_20 INTO ret92_20
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_21,2)
                 INTO ret92_21 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 21}
            IF v_sub_2 = 13 THEN
               PREPARE prp_ret92_21 FROM lc_query
               EXECUTE prp_ret92_21 INTO ret92_21
            END IF

            {SELECT ROUND(SUM(h.monto_en_acciones)* precio_22,2)
                 INTO ret92_22 FROM dis_cuenta h
                   WHERE h.nss              = p_mae_afi.nss
                     AND h.subcuenta        = 7
                     AND h.tipo_movimiento  > 0
                     AND h.siefore          = 22}
            IF v_sub_2 = 14 THEN
               PREPARE prp_ret92_22 FROM lc_query
               EXECUTE prp_ret92_22 INTO ret92_22
            END IF
         END FOR -------------------------------------------------------------------- 2
---<

            IF ret92_1 IS NULL THEN
                LET ret92_1 = 0
            END IF

            IF ret92_2 IS NULL THEN
                LET ret92_2 = 0
            END IF

--->mas siefores
            IF ret92_3 IS NULL THEN
                LET ret92_3 = 0
            END IF

            IF ret92_4 IS NULL THEN
                LET ret92_4 = 0
            END IF

            IF ret92_5 IS NULL THEN
                LET ret92_5 = 0
            END IF

            IF ret92_14 IS NULL THEN
                LET ret92_14 = 0
            END IF            

            IF ret92_15 IS NULL THEN
                LET ret92_15 = 0
            END IF            

            IF ret92_16 IS NULL THEN
                LET ret92_16 = 0
            END IF            

            IF ret92_17 IS NULL THEN
                LET ret92_17 = 0
            END IF      

            IF ret92_18 IS NULL THEN
                LET ret92_18 = 0
            END IF     

            IF ret92_19 IS NULL THEN
                LET ret92_19 = 0
            END IF 

            IF ret92_20 IS NULL THEN
                LET ret92_20 = 0
            END IF   

            IF ret92_21 IS NULL THEN
                LET ret92_21 = 0
            END IF       

            IF ret92_22 IS NULL THEN
                LET ret92_22 = 0
            END IF              
---<

            LET asig.ret92 = ret92_1 + ret92_2 +
                             ret92_3 + ret92_4 + ret92_5     ---mas siefores
                             + ret92_14 + ret92_15 + ret92_16 + ret92_17 + ret92_18 + ret92_19 + ret92_20 + 
                             ret92_21 + ret92_22                             

            IF asig.ret92 IS NULL THEN
               LET asig.ret92 = 0
            END IF

            LET ahorro_1 = 0   LET ahorro_2 = 0  --LET precio = 0
            LET ahorro_3 = 0   LET ahorro_4 = 0  LET ahorro_5 = 0     ---mas siefores
            LET ahorro_14 = 0   LET ahorro_15 = 0  LET ahorro_16 = 0
            LET ahorro_17 = 0   LET ahorro_18 = 0  LET ahorro_19 = 0
            LET ahorro_20 = 0   LET ahorro_21 = 0  LET ahorro_22 = 0            

---------------------------------------------------------------------------------------------------------------------- TERCERAS
         FOR v_sub_3 = 1 TO 13
            CASE v_sub_3
               WHEN 1  -- ahorro_1
                  LET v_precio = precio_1
                  LET v_siefore = 1

               WHEN 2  -- ahorro_2
                  LET v_precio = precio_2
                  LET v_siefore = 2

               WHEN 3  -- ahorro_3
                  LET v_precio = precio_3
                  LET v_siefore = 3

               WHEN 4  -- ahorro_4
                  LET v_precio = precio_4
                  LET v_siefore = 4

               WHEN 5  -- ahorro_5
                  LET v_precio = precio_5
                  LET v_siefore = 5

               WHEN 6  -- ahorro_14
                  LET v_precio = precio_14
                  LET v_siefore = 14

               WHEN 7  -- ahorro_15
                  LET v_precio = precio_15
                  LET v_siefore = 15

               WHEN 8  -- ahorro_16
                  LET v_precio = precio_16
                  LET v_siefore = 16

               WHEN 9  -- ahorro_17
                  LET v_precio = precio_17
                  LET v_siefore = 17

               WHEN 10  -- ahorro_18
                  LET v_precio = precio_18
                  LET v_siefore = 18

               WHEN 11  -- ahorro_19
                  LET v_precio = precio_19
                  LET v_siefore = 19

               WHEN 12  -- ahorro_20
                  LET v_precio = precio_20
                  LET v_siefore = 20

               WHEN 13  -- ahorro_21
                  LET v_precio = precio_21
                  LET v_siefore = 21
            END CASE

            INITIALIZE lc_query, v_tabla TO NULL
            
            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = 13",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta = 13",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                 ELSE
                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta",
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta = 13",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                  END IF

               END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_1,2)
                 INTO ahorro_1 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
##                     AND n.siefore          = csie_ahorro
                     AND n.siefore          = 1}
            IF v_sub_3 = 1 THEN
               PREPARE prp_ahorro_1 FROM lc_query
               EXECUTE prp_ahorro_1 INTO ahorro_1
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_2,2)
                 INTO ahorro_2 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
##                     AND n.siefore          = csie_ahorro
                     AND n.siefore          = 2}
            IF v_sub_3 = 2 THEN
               PREPARE prp_ahorro_2 FROM lc_query
               EXECUTE prp_ahorro_2 INTO ahorro_2
            END IF

--->mas siefores
            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_3,2)
                 INTO ahorro_3 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 3}
            IF v_sub_3 = 3 THEN
               PREPARE prp_ahorro_3 FROM lc_query
               EXECUTE prp_ahorro_3 INTO ahorro_3
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_4,2)
                 INTO ahorro_4 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 4}
            IF v_sub_3 = 4 THEN
               PREPARE prp_ahorro_4 FROM lc_query
               EXECUTE prp_ahorro_4 INTO ahorro_4
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_5,2)
                 INTO ahorro_5 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 5}
            IF v_sub_3 = 5 THEN
               PREPARE prp_ahorro_5 FROM lc_query
               EXECUTE prp_ahorro_5 INTO ahorro_5
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_14,2)
                 INTO ahorro_14 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 14}
            IF v_sub_3 = 6 THEN
               PREPARE prp_ahorro_14 FROM lc_query
               EXECUTE prp_ahorro_14 INTO ahorro_14
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_15,2)
                 INTO ahorro_15 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 15}
            IF v_sub_3 = 7 THEN
               PREPARE prp_ahorro_15 FROM lc_query
               EXECUTE prp_ahorro_15 INTO ahorro_15
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_16,2)
                 INTO ahorro_16 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 16}
            IF v_sub_3 = 8 THEN
               PREPARE prp_ahorro_16 FROM lc_query
               EXECUTE prp_ahorro_16 INTO ahorro_16
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_17,2)
                 INTO ahorro_17 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 17}
            IF v_sub_3 = 9 THEN
               PREPARE prp_ahorro_17 FROM lc_query
               EXECUTE prp_ahorro_17 INTO ahorro_17
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_18,2)
                 INTO ahorro_18 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 18}
            IF v_sub_3 = 10 THEN
               PREPARE prp_ahorro_18 FROM lc_query
               EXECUTE prp_ahorro_18 INTO ahorro_18
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_19,2)
                 INTO ahorro_19 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 19}
            IF v_sub_3 = 11 THEN
               PREPARE prp_ahorro_19 FROM lc_query
               EXECUTE prp_ahorro_19 INTO ahorro_19
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_20,2)
                 INTO ahorro_20 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 20}
            IF v_sub_3 = 12 THEN
               PREPARE prp_ahorro_20 FROM lc_query
               EXECUTE prp_ahorro_20 INTO ahorro_20
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_21,2)
                 INTO ahorro_21 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        = 13
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 21} 
            IF v_sub_3 = 13 THEN
               PREPARE prp_ahorro_21 FROM lc_query
               EXECUTE prp_ahorro_21 INTO ahorro_21
            END IF

         END FOR ------------------------------------------------- 3
---<

            IF ahorro_1 IS NULL THEN
                LET ahorro_1 = 0
            END IF

            IF ahorro_2 IS NULL THEN
                LET ahorro_2 = 0
            END IF

--->mas siefores
            IF ahorro_3 IS NULL THEN
                LET ahorro_3 = 0
            END IF

            IF ahorro_4 IS NULL THEN
                LET ahorro_4 = 0
            END IF

            IF ahorro_5 IS NULL THEN
                LET ahorro_5 = 0
            END IF

            IF ahorro_14 IS NULL THEN
                LET ahorro_14 = 0
            END IF 

            IF ahorro_15 IS NULL THEN
                LET ahorro_15 = 0
            END IF      

            IF ahorro_16 IS NULL THEN
                LET ahorro_16 = 0
            END IF              

            IF ahorro_17 IS NULL THEN
                LET ahorro_17 = 0
            END IF        

            IF ahorro_18 IS NULL THEN
                LET ahorro_18 = 0
            END IF       

            IF ahorro_19 IS NULL THEN
                LET ahorro_19 = 0
            END IF       

            IF ahorro_20 IS NULL THEN
                LET ahorro_20 = 0
            END IF       

            IF ahorro_21 IS NULL THEN
                LET ahorro_21 = 0
            END IF      

            IF ahorro_22 IS NULL THEN
                LET ahorro_22 = 0
            END IF              
---<

            LET asig.ahorro = ahorro_1 + ahorro_2 +
                              ahorro_3 + ahorro_4 + ahorro_5  +  ---mas siefores
                              ahorro_14 + ahorro_15 + ahorro_16 +
                              ahorro_17 + ahorro_18 + ahorro_19 + 
                              ahorro_20 + ahorro_21 + ahorro_22                              


            IF asig.ahorro IS NULL THEN
               LET asig.ahorro = 0
            END IF

            LET apcom_1 = 0   LET apcom_2 = 0  --LET precio = 0
            LET apcom_3 = 0   LET apcom_4 = 0   LET apcom_5 = 0     ---mas siefores
            LET apcom_14 = 0   LET apcom_15 = 0  LET apcom_16 = 0
						LET apcom_17 = 0   LET apcom_18 = 0  LET apcom_19 = 0
						LET apcom_20 = 0   LET apcom_21 = 0  LET apcom_22 = 0            

---------------------------------------------------------------------------------------------------------------------- CUARTAS

         FOR v_sub_4 = 1 TO 14
            CASE v_sub_4
               WHEN 1  -- apcom_1
                  LET v_precio = precio_1
                  LET v_siefore = 1

               WHEN 2  -- apcom_2
                  LET v_precio = precio_2
                  LET v_siefore = 2

               WHEN 3  -- apcom_3
                  LET v_precio = precio_3
                  LET v_siefore = 3

               WHEN 4  -- apcom_4
                  LET v_precio = precio_4
                  LET v_siefore = 4

               WHEN 5  -- apcom_5
                  LET v_precio = precio_5
                  LET v_siefore = 5

               WHEN 6  -- apcom_14
                  LET v_precio = precio_14
                  LET v_siefore = 14

               WHEN 7  -- apcom_15
                  LET v_precio = precio_15
                  LET v_siefore = 15

               WHEN 8  -- apcom_16
                  LET v_precio = precio_16
                  LET v_siefore = 16

               WHEN 9  -- apcom_17
                  LET v_precio = precio_17
                  LET v_siefore = 17

               WHEN 10  -- apcom_18
                  LET v_precio = precio_18
                  LET v_siefore = 18

               WHEN 11  -- apcom_19
                  LET v_precio = precio_19
                  LET v_siefore = 19

               WHEN 12  -- apcom_20
                  LET v_precio = precio_20
                  LET v_siefore = 20

               WHEN 13  -- apcom_21
                  LET v_precio = precio_21
                  LET v_siefore = 21

               WHEN 14  -- apcom_22
                  LET v_precio = precio_22
                  LET v_siefore = 22
            END CASE

            INITIALIZE lc_query, v_tabla TO NULL
            
            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta IN (11,12)",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", v_siefore, ""
                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta IN (11,12)",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                 ELSE
                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", v_precio, ",2) ",
                                   "\n   FROM dis_cuenta",
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta IN (11,12)",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", v_siefore, ""
                  END IF

               END IF
-----
            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_1,2)
                 INTO apcom_1 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
##                     AND n.siefore          = csie_apcom
                     AND n.siefore          = 1}
            IF v_sub_4 = 1 THEN
               PREPARE prp_apcom_1 FROM lc_query
               EXECUTE prp_apcom_1 INTO apcom_1
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_2,2)
                 INTO apcom_2 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
##                     AND n.siefore          = csie_apcom
                     AND n.siefore          = 2}
            IF v_sub_4 = 2 THEN
               PREPARE prp_apcom_2 FROM lc_query
               EXECUTE prp_apcom_2 INTO apcom_2
            END IF

--->mas siefores
            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_3,2)
                 INTO apcom_3 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 3}
            IF v_sub_4 = 3 THEN
               PREPARE prp_apcom_3 FROM lc_query
               EXECUTE prp_apcom_3 INTO apcom_3
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_4,2)
                 INTO apcom_4 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 4}
            IF v_sub_4 = 4 THEN
               PREPARE prp_apcom_4 FROM lc_query
               EXECUTE prp_apcom_4 INTO apcom_4
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_5,2)
                 INTO apcom_5 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 5}
            IF v_sub_4 = 5 THEN
               PREPARE prp_apcom_5 FROM lc_query
               EXECUTE prp_apcom_5 INTO apcom_5
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_14,2)
                 INTO apcom_14 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 14}
            IF v_sub_4 = 6 THEN
               PREPARE prp_apcom_14 FROM lc_query
               EXECUTE prp_apcom_14 INTO apcom_14
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_15,2)
                 INTO apcom_15 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 15}
            IF v_sub_4 = 7 THEN
               PREPARE prp_apcom_15 FROM lc_query
               EXECUTE prp_apcom_15 INTO apcom_15
            END IF
                     
            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_16,2)
                 INTO apcom_16 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 16}
            IF v_sub_4 = 8 THEN
               PREPARE prp_apcom_16 FROM lc_query
               EXECUTE prp_apcom_16 INTO apcom_16
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_17,2)
                 INTO apcom_17 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 17}
            IF v_sub_4 = 9 THEN
               PREPARE prp_apcom_17 FROM lc_query
               EXECUTE prp_apcom_17 INTO apcom_17
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_18,2)
                 INTO apcom_18 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 18}
            IF v_sub_4 = 10 THEN
               PREPARE prp_apcom_18 FROM lc_query
               EXECUTE prp_apcom_18 INTO apcom_18
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_19,2)
                 INTO apcom_19 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 19}
            IF v_sub_4 = 11 THEN
               PREPARE prp_apcom_19 FROM lc_query
               EXECUTE prp_apcom_19 INTO apcom_19
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_20,2)
                 INTO apcom_20 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 20}
            IF v_sub_4 = 12 THEN
               PREPARE prp_apcom_20 FROM lc_query
               EXECUTE prp_apcom_20 INTO apcom_20
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_21,2)
                 INTO apcom_21 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 21}
            IF v_sub_4 = 13 THEN
               PREPARE prp_apcom_21 FROM lc_query
               EXECUTE prp_apcom_21 INTO apcom_21
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio_22,2)
                 INTO apcom_22 FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
                     AND n.subcuenta        IN(11,12)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 22}
             IF v_sub_4 = 14 THEN
               PREPARE prp_apcom_22 FROM lc_query
               EXECUTE prp_apcom_22 INTO apcom_22
            END IF  
         END FOR --------------------------------------------------------- 4            
---<

            IF apcom_1 IS NULL THEN
                LET apcom_1 = 0
            END IF

            IF apcom_2 IS NULL THEN
                LET apcom_2 = 0
            END IF

--->mas siefores
            IF apcom_3 IS NULL THEN
                LET apcom_3 = 0
            END IF

            IF apcom_4 IS NULL THEN
                LET apcom_4 = 0
            END IF

            IF apcom_5 IS NULL THEN
                LET apcom_5 = 0
            END IF

            IF apcom_14 IS NULL THEN
                LET apcom_14 = 0
            END IF     

            IF apcom_15 IS NULL THEN
                LET apcom_15 = 0
            END IF      

            IF apcom_16 IS NULL THEN
                LET apcom_16 = 0
            END IF      

            IF apcom_17 IS NULL THEN
                LET apcom_17 = 0
            END IF       

            IF apcom_18 IS NULL THEN
                LET apcom_18 = 0
            END IF     

            IF apcom_19 IS NULL THEN
                LET apcom_19 = 0
            END IF      

            IF apcom_20 IS NULL THEN
                LET apcom_20 = 0
            END IF       

            IF apcom_21 IS NULL THEN
                LET apcom_21 = 0
            END IF     

            IF apcom_22 IS NULL THEN
                LET apcom_22 = 0
            END IF              
---<

            LET asig.apcom = apcom_1 + apcom_2 +
                             apcom_3 + apcom_4 +apcom_5 +        ---mas siefores
                             apcom_14 + apcom_15 + apcom_16 +
                             apcom_17 + apcom_18 + apcom_19 + 
                             apcom_20 + apcom_21 + apcom_22    


            IF asig.apcom IS NULL THEN
               LET asig.apcom = 0
            END IF

---------------------------------------------------------------------------------------------------------------------- QUINTA
            LET precio = 0
            SELECT a.precio_del_dia INTO precio FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = csie_apvol

            INITIALIZE lc_query, v_tabla TO NULL
            
            IF v_anio_finicta = v_anio_actual THEN
                  
               LET lc_query = "",
                              "\n ROUND(SUM(nonto_en_acciones)* ", precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta IN(3,10)",
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = ", csie_apvol, ""

                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  LET v_existe_dis_ctaxx = 0
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", precio, ",2) ",
                                   "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                                   "\n WHERE nss = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta IN(3,10)",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", csie_apvol, ""
                 ELSE
                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", precio, ",2) ",
                                   "\n   FROM dis_cuenta",
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta IN(3,10)",
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = ", csie_apvol, ""
                  END IF

               END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio,2)
                 INTO asig.apvol FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        IN(3,10)
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = csie_apvol}
            PREPARE prp_csie_apvol FROM lc_query
            EXECUTE prp_csie_apvol INTO asig.apvol

            IF asig.apvol IS NULL THEN
               LET asig.apvol = 0
            END IF
      --------------------------------------------------- 5
      
            LET precio = 0

            SELECT a.precio_del_dia INTO precio FROM glo_valor_accion a
            WHERE a.fecha_valuacion = dia_hoy
            AND   a.codigo_siefore  = 11

---------------------------------------------------------------------------------------------------------------------- SEXTAS
         FOR v_sub_6 = 1 TO 2
            CASE v_sub_6
               WHEN 1  -- asig.viv97
                  LET v_subcuenta = 4
               WHEN 2  -- asig.viv92
                  LET v_subcuenta = 8
            END CASE

            INITIALIZE lc_query, v_tabla TO NULL

            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_acciones)*", precio, ",2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss              = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = ",v_subcuenta,
                              "\n   AND tipo_movimiento  > 0 ",
                              "\n   AND siefore = 11"

                              
               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  LET v_existe_dis_ctaxx = 0
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", precio, ",2) ",
                                   "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta = ", v_subcuenta,
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = 11"
                 ELSE
                    LET lc_query = "",
                                   "\n ROUND(SUM(monto_en_acciones)* ", precio, ",2) ",
                                   "\n   FROM dis_cuenta",
                                   "\n WHERE nss              = '", p_mae_afi.nss,"'",
                                   "\n   AND subcuenta = ", v_subcuenta,
                                   "\n   AND tipo_movimiento  > 0 ",
                                   "\n   AND siefore = 11"
                  END IF

               END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio,2)
                 INTO asig.viv97   FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        = 4
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 11}
            IF v_sub_6 = 1 THEN
               PREPARE prp_viv97 FROM lc_query
               EXECUTE prp_viv97 INTO asig.viv97
            END IF
            
            IF asig.viv97 IS NULL THEN
               LET asig.viv97 = 0
            END IF

            {SELECT ROUND(SUM(n.monto_en_acciones)* precio,2)
                 INTO asig.viv92   FROM dis_cuenta n
                   WHERE n.nss              = p_mae_afi.nss
--                     AND n.fecha_conversion <= p_mae_afi.fecha_certifica
                     AND n.subcuenta        = 8
                     AND n.tipo_movimiento  > 0
                     AND n.siefore          = 11}
            IF v_sub_6 = 2 THEN
               PREPARE prp_viv92 FROM lc_query
               EXECUTE prp_viv92 INTO asig.viv92
            END IF
            
            IF asig.viv92 IS NULL THEN
               LET asig.viv92 = 0
            END IF

         END FOR
         
---------------------------------------------------------------------------------------------------------------------- SEPTIMAS

         FOR v_sub_7 = 1 TO 2
            CASE v_sub_7
               WHEN 1  -- asig.fondo
                  LET v_subcuenta = 14
               WHEN 2  -- asig.largop
                  LET v_subcuenta = 15
            END CASE

            INITIALIZE lc_query, v_tabla TO NULL

            IF v_anio_finicta = v_anio_actual THEN

               LET lc_query = "",
                              "\n ROUND(SUM(monto_en_pesos),2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = ", v_subcuenta,
                              "\n   AND tipo_movimiento  > 0 "

               ELSE
                  LET v_tabla = 'dis_cuenta'||v_anio_finicta[3,4]

                  --Se valida que exista la tabla
                  LET v_existe_dis_ctaxx = 0
                  SELECT count(*)
                    INTO v_existe_dis_ctaxx
                    FROM systables
                   WHERE tabname MATCHES v_tabla

                 --Si no existe la tabla, se consultara en dis_cuenta
                 IF v_existe_dis_ctaxx > 0 THEN

                    LET lc_query = "",
                              "\n ROUND(SUM(monto_en_pesos),2) ",
                              "\n   FROM dis_cuenta", v_anio_finicta[3,4],
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = ", v_subcuenta,
                              "\n   AND tipo_movimiento  > 0 "
                 ELSE
                    LET lc_query = "",
                              "\n ROUND(SUM(monto_en_pesos),2) ",
                              "\n   FROM dis_cuenta",
                              "\n WHERE nss = '", p_mae_afi.nss,"'",
                              "\n   AND subcuenta = ", v_subcuenta,
                              "\n   AND tipo_movimiento  > 0 "
                  END IF

               END IF
               
            {SELECT ROUND(SUM(t.monto_en_pesos),2)
                    INTO asig.fondo  FROM dis_cuenta t
                      WHERE t.nss              = p_mae_afi.nss
--                        AND t.fecha_conversion <= p_mae_afi.fecha_certifica
                        AND t.subcuenta        IN(14)
                        AND t.tipo_movimiento  > 0}
            IF v_sub_7 = 1 THEN
               PREPARE prp_fondo FROM lc_query
               EXECUTE prp_fondo INTO asig.fondo
            END IF
            
            IF asig.fondo IS NULL THEN
               LET asig.fondo = 0
            END IF
            
            {SELECT ROUND(SUM(t.monto_en_pesos),2)
                    INTO asig.largop  FROM dis_cuenta t
                      WHERE t.nss              = p_mae_afi.nss
--                        AND t.fecha_conversion <= p_mae_afi.fecha_certifica
                        AND t.subcuenta        = 15
                        AND t.tipo_movimiento  > 0}
            IF v_sub_7 = 2 THEN
               PREPARE prp_largop FROM lc_query
               EXECUTE prp_largop INTO asig.largop
            END IF
            
            IF asig.largop IS NULL THEN
               LET asig.largop = 0
            END IF
         END FOR

            LET asig.s_inver = asig.rcv + asig.ret92 + asig.ahorro +
                               asig.apcom + asig.apvol + asig.largop

            LET asig.s_viv   = asig.viv97 + asig.viv92 + asig.fondo

            LET ahorro_retiro = 0
            LET ahorro_vol    = 0
            LET ahorro_viv    = 0
            LET ahorro_lplazo = 0
            LET tot_ahorro    = 0

            LET ahorro_retiro = asig.rcv + asig.ret92 + asig.ahorro
            LET ahorro_vol    = asig.apcom + asig.apvol + asig.largop
            LET ahorro_viv    = asig.viv97 + asig.viv92
            LET ahorro_lplazo = asig.largop
            LET tot_ahorro    = ahorro_retiro + ahorro_vol + ahorro_viv 
            
--display "asig_rcv2 :",asig.rcv
--display "asig_ret9 :",asig.ret92
----display "asig_ahor :",asig.ahorro
--display "ahor_ret  :",ahorro_retiro
--prompt "enter" for enter
         END IF -- end if 407

         SELECT a.codigo_afore, a.razon_social[1,30]
         INTO   clave_afore, des_transferente
         FROM   tab_afore_local a

         LET numero_reg = numero_reg + 1
         LET consecutivo_envio = numero_reg        ## USING "&&&&&&&&"

         CASE prog 
         WHEN "407"
            LET no_carta = "302",ref

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
                          clave_afore USING "&&&"             , "|",  #25
                          des_transferente                    , "|",  #26
                          nacion                              , "|",  #27
                          fena                                , "|",  #28
                          finicta USING "dd/mm/yyyy"          , "|",  #29
                          f_asigna                            , "|",  #30
                          ahorro_retiro                       , "|",  #31
                          sie_rcv                             , "|",  #32
                          ahorro_vol                          , "|",  #33
                          sie_apvol                           , "|",  #34
                          ahorro_viv                          , "|",  #35
                          tot_ahorro                          , "|",  #36
                          asig.largop                         , "|",  #37
                          sie_largop                          , "|",  #38
                          des_transferente                    , "|",  #39
                          gen_compara.afore_a                 , "|",  #40
                          gen_compara.rend_a                  , "|",  #41
                          gen_compara.comis_a                 , "|",  #42
                          gen_compara.rendneto_a              , "|",  #43
                          gen_compara.afore_b                 , "|",  #44
                          gen_compara.rend_b                  , "|",  #45
                          gen_compara.comis_b                 , "|",  #46
                          gen_compara.rendneto_b              , "|",  #47
                          gen_compara.afore_c                 , "|",  #48
                          gen_compara.rend_c                  , "|",  #49
                          gen_compara.comis_c                 , "|",  #50
                          gen_compara.rendneto_c              , "|",  #51
                          gen_compara.afore_d                 , "|",  #52
                          gen_compara.rend_d                  , "|",  #53
                          gen_compara.comis_d                 , "|",  #54
                          gen_compara.rendneto_d              , "|",  #55
                          gen_compara.afore_e                 , "|",  #56
                          gen_compara.rend_e                  , "|",  #57
                          gen_compara.comis_e                 , "|",  #58
                          gen_compara.rendneto_e              , "|",  #59
                          gen_compara.afore_f                 , "|",  #60
                          gen_compara.rend_f                  , "|",  #61
                          gen_compara.comis_f                 , "|",  #62
                          gen_compara.rendneto_f              , "|",  #63
                          gen_compara.afore_g                 , "|",  #64
                          gen_compara.rend_g                  , "|",  #65
                          gen_compara.comis_g                 , "|",  #66
                          gen_compara.rendneto_g              , "|",  #67
                          gen_compara.afore_h                 , "|",  #68
                          gen_compara.rend_h                  , "|",  #69
                          gen_compara.comis_h                 , "|",  #70
                          gen_compara.rendneto_h              , "|",  #71
                          gen_compara.afore_i                 , "|",  #72
                          gen_compara.rend_i                  , "|",  #73
                          gen_compara.comis_i                 , "|",  #74
                          gen_compara.rendneto_i              , "|",  #75
                          gen_compara.afore_j                 , "|",  #76
                          gen_compara.rend_j                  , "|",  #77
                          gen_compara.comis_j                 , "|",  #78
                          gen_compara.rendneto_j              , "|",  #79
                          gen_compara.afore_k                 , "|",  #80
                          gen_compara.rend_k                  , "|",  #81
                          gen_compara.comis_k                 , "|",  #82
                          gen_compara.rendneto_k              , "|",  #83
                          gen_compara.afore_l                 , "|",  #84
                          gen_compara.rend_l                  , "|",  #85
                          gen_compara.comis_l                 , "|",  #86
                          gen_compara.rendneto_l              , "|",  #87
                          gen_compara.afore_m                 , "|",  #88
                          gen_compara.rend_m                  , "|",  #89
                          gen_compara.comis_m                 , "|",  #90
                          gen_compara.rendneto_m              , "|",  #91
                          gen_compara.afore_n                 , "|",  #92
                          gen_compara.rend_n                  , "|",  #93
                          gen_compara.comis_n                 , "|",  #94
                          gen_compara.rendneto_n              , "|",  #95
                          gen_compara.afore_o                 , "|",  #96
                          gen_compara.rend_o                  , "|",  #97
                          gen_compara.comis_o                 , "|",  #98
                          gen_compara.rendneto_o              , "|",  #99
                          gen_compara.afore_p                 , "|",  #100
                          gen_compara.rend_p                  , "|",  #101
                          gen_compara.comis_p                 , "|",  #102
                          gen_compara.rendneto_p              , "|",  #103
                          gen_compara.afore_q                 , "|",  #104
                          gen_compara.rend_q                  , "|",  #105
                          gen_compara.comis_q                 , "|",  #106
                          gen_compara.rendneto_q              , "|",  #107
                          gen_compara.afore_r                 , "|",  #108
                          gen_compara.rend_r                  , "|",  #109
                          gen_compara.comis_r                 , "|",  #110
                          gen_compara.rendneto_r              , "|",  #111
                          gen_compara.afore_s                 , "|",  #112
                          gen_compara.rend_s                  , "|",  #113
                          gen_compara.comis_s                 , "|",  #114
                          gen_compara.rendneto_s              , "|",  #115
                          gen_compara.afore_t                 , "|",  #116
                          gen_compara.rend_t                  , "|",  #117
                          gen_compara.comis_t                 , "|",  #118
                          gen_compara.rendneto_t              , "|",  #119
                          gen_compara.afore_u                 , "|",  #120
                          gen_compara.rend_u                  , "|",  #121
                          gen_compara.comis_u                 , "|",  #122
                          gen_compara.rendneto_u              , "|",  #123
                          gen_compara.afore_v                 , "|",  #124
                          gen_compara.rend_v                  , "|",  #125
                          gen_compara.comis_v                 , "|",  #126
                          gen_compara.rendneto_v              , "|",  #127
                          gen_compara.afore_w                 , "|",  #128
                          gen_compara.rend_w                  , "|",  #129
                          gen_compara.comis_w                 , "|",  #130
                          gen_compara.rendneto_w              , "|",  #131
                          gen_compara.afore_x                 , "|",  #132
                          gen_compara.rend_x                  , "|",  #133
                          gen_compara.comis_x                 , "|",  #134
                          gen_compara.rendneto_x              , "|",  #135
                          gen_compara.afore_y                 , "|",  #136
                          gen_compara.rend_y                  , "|",  #137
                          gen_compara.comis_y                 , "|",  #138
                          gen_compara.rendneto_y              , "|",  #139
                          gen_compara.afore_z                 , "|",  #140
                          gen_compara.rend_z                  , "|",  #141
                          gen_compara.comis_z                 , "|",  #142
                          gen_compara.rendneto_z              , "|",  #143
                          barras                              , "|",   #144
                          vprom_simple						 , "|"   #145 #CPL-1182
         
         WHEN "4071"
            LET no_carta = "302",ref

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
                          clave_afore USING "&&&"             , "|",  #25
                          des_transferente                    , "|",  #26
                          nacion                              , "|",  #27
                          fena                                , "|",  #28
                          finicta USING "dd/mm/yyyy"          , "|",  #29
                          f_asigna                            , "|",  #30
                          ahorro_retiro                       , "|",  #31
                          sie_rcv                             , "|",  #32
                          ahorro_vol                          , "|",  #33
                          sie_apvol                           , "|",  #34
                          ahorro_viv                          , "|",  #35
                          tot_ahorro                          , "|",  #36
                          asig.largop                         , "|",  #37
                          sie_largop                          , "|",  #38
                          des_transferente                    , "|",  #39
                          gen_compara.afore_a                 , "|",  #40
                          gen_compara.rend_a                  , "|",  #41
                          gen_compara.comis_a                 , "|",  #42
                          gen_compara.rendneto_a              , "|",  #43
                          gen_compara.afore_b                 , "|",  #44
                          gen_compara.rend_b                  , "|",  #45
                          gen_compara.comis_b                 , "|",  #46
                          gen_compara.rendneto_b              , "|",  #47
                          gen_compara.afore_c                 , "|",  #48
                          gen_compara.rend_c                  , "|",  #49
                          gen_compara.comis_c                 , "|",  #50
                          gen_compara.rendneto_c              , "|",  #51
                          gen_compara.afore_d                 , "|",  #52
                          gen_compara.rend_d                  , "|",  #53
                          gen_compara.comis_d                 , "|",  #54
                          gen_compara.rendneto_d              , "|",  #55
                          gen_compara.afore_e                 , "|",  #56
                          gen_compara.rend_e                  , "|",  #57
                          gen_compara.comis_e                 , "|",  #58
                          gen_compara.rendneto_e              , "|",  #59
                          gen_compara.afore_f                 , "|",  #60
                          gen_compara.rend_f                  , "|",  #61
                          gen_compara.comis_f                 , "|",  #62
                          gen_compara.rendneto_f              , "|",  #63
                          gen_compara.afore_g                 , "|",  #64
                          gen_compara.rend_g                  , "|",  #65
                          gen_compara.comis_g                 , "|",  #66
                          gen_compara.rendneto_g              , "|",  #67
                          gen_compara.afore_h                 , "|",  #68
                          gen_compara.rend_h                  , "|",  #69
                          gen_compara.comis_h                 , "|",  #70
                          gen_compara.rendneto_h              , "|",  #71
                          gen_compara.afore_i                 , "|",  #72
                          gen_compara.rend_i                  , "|",  #73
                          gen_compara.comis_i                 , "|",  #74
                          gen_compara.rendneto_i              , "|",  #75
                          gen_compara.afore_j                 , "|",  #76
                          gen_compara.rend_j                  , "|",  #77
                          gen_compara.comis_j                 , "|",  #78
                          gen_compara.rendneto_j              , "|",  #79
                          gen_compara.afore_k                 , "|",  #80
                          gen_compara.rend_k                  , "|",  #81
                          gen_compara.comis_k                 , "|",  #82
                          gen_compara.rendneto_k              , "|",  #83
                          gen_compara.afore_l                 , "|",  #84
                          gen_compara.rend_l                  , "|",  #85
                          gen_compara.comis_l                 , "|",  #86
                          gen_compara.rendneto_l              , "|",  #87
                          gen_compara.afore_m                 , "|",  #88
                          gen_compara.rend_m                  , "|",  #89
                          gen_compara.comis_m                 , "|",  #90
                          gen_compara.rendneto_m              , "|",  #91
                          gen_compara.afore_n                 , "|",  #92
                          gen_compara.rend_n                  , "|",  #93
                          gen_compara.comis_n                 , "|",  #94
                          gen_compara.rendneto_n              , "|",  #95
                          gen_compara.afore_o                 , "|",  #96
                          gen_compara.rend_o                  , "|",  #97
                          gen_compara.comis_o                 , "|",  #98
                          gen_compara.rendneto_o              , "|",  #99
                          gen_compara.afore_p                 , "|",  #100
                          gen_compara.rend_p                  , "|",  #101
                          gen_compara.comis_p                 , "|",  #102
                          gen_compara.rendneto_p              , "|",  #103
                          gen_compara.afore_q                 , "|",  #104
                          gen_compara.rend_q                  , "|",  #105
                          gen_compara.comis_q                 , "|",  #106
                          gen_compara.rendneto_q              , "|",  #107
                          gen_compara.afore_r                 , "|",  #108
                          gen_compara.rend_r                  , "|",  #109
                          gen_compara.comis_r                 , "|",  #110
                          gen_compara.rendneto_r              , "|",  #111
                          gen_compara.afore_s                 , "|",  #112
                          gen_compara.rend_s                  , "|",  #113
                          gen_compara.comis_s                 , "|",  #114
                          gen_compara.rendneto_s              , "|",  #115
                          gen_compara.afore_t                 , "|",  #116
                          gen_compara.rend_t                  , "|",  #117
                          gen_compara.comis_t                 , "|",  #118
                          gen_compara.rendneto_t              , "|",  #119
                          gen_compara.afore_u                 , "|",  #120
                          gen_compara.rend_u                  , "|",  #121
                          gen_compara.comis_u                 , "|",  #122
                          gen_compara.rendneto_u              , "|",  #123
                          gen_compara.afore_v                 , "|",  #124
                          gen_compara.rend_v                  , "|",  #125
                          gen_compara.comis_v                 , "|",  #126
                          gen_compara.rendneto_v              , "|",  #127
                          gen_compara.afore_w                 , "|",  #128
                          gen_compara.rend_w                  , "|",  #129
                          gen_compara.comis_w                 , "|",  #130
                          gen_compara.rendneto_w              , "|",  #131
                          gen_compara.afore_x                 , "|",  #132
                          gen_compara.rend_x                  , "|",  #133
                          gen_compara.comis_x                 , "|",  #134
                          gen_compara.rendneto_x              , "|",  #135
                          gen_compara.afore_y                 , "|",  #136
                          gen_compara.rend_y                  , "|",  #137
                          gen_compara.comis_y                 , "|",  #138
                          gen_compara.rendneto_y              , "|",  #139
                          gen_compara.afore_z                 , "|",  #140
                          gen_compara.rend_z                  , "|",  #141
                          gen_compara.comis_z                 , "|",  #142
                          gen_compara.rendneto_z              , "|",  #143
                          barras                              , "|",   #144
                          vprom_simple						 , "|"   #145 #CPL-1182                 
         WHEN "406"
            LET no_carta = "302",ref

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
                          clave_afore USING "&&&"             , "|",  #25
                          des_transferente                    , "|",  #26
                          nacion                              , "|",  #27
                          fena                                , "|",  #28
                          finicta USING "dd/mm/yyyy"          , "|",  #29
                          f_asigna                            , "|",  #30
                          ahorro_retiro                       , "|",  #31
                          sie_rcv                             , "|",  #32
                          ahorro_vol                          , "|",  #33
                          sie_apvol                           , "|",  #34
                          ahorro_viv                          , "|",  #35
                          tot_ahorro                          , "|",  #36
                          asig.largop                         , "|",  #37
                          sie_largop                          , "|",  #38
                          des_transferente                    , "|",  #39
                          gen_compara.afore_a                 , "|",  #40
                          gen_compara.rend_a                  , "|",  #41
                          gen_compara.comis_a                 , "|",  #42
                          gen_compara.rendneto_a              , "|",  #43
                          gen_compara.afore_b                 , "|",  #44
                          gen_compara.rend_b                  , "|",  #45
                          gen_compara.comis_b                 , "|",  #46
                          gen_compara.rendneto_b              , "|",  #47
                          gen_compara.afore_c                 , "|",  #48
                          gen_compara.rend_c                  , "|",  #49
                          gen_compara.comis_c                 , "|",  #50
                          gen_compara.rendneto_c              , "|",  #51
                          gen_compara.afore_d                 , "|",  #52
                          gen_compara.rend_d                  , "|",  #53
                          gen_compara.comis_d                 , "|",  #54
                          gen_compara.rendneto_d              , "|",  #55
                          gen_compara.afore_e                 , "|",  #56
                          gen_compara.rend_e                  , "|",  #57
                          gen_compara.comis_e                 , "|",  #58
                          gen_compara.rendneto_e              , "|",  #59
                          gen_compara.afore_f                 , "|",  #60
                          gen_compara.rend_f                  , "|",  #61
                          gen_compara.comis_f                 , "|",  #62
                          gen_compara.rendneto_f              , "|",  #63
                          gen_compara.afore_g                 , "|",  #64
                          gen_compara.rend_g                  , "|",  #65
                          gen_compara.comis_g                 , "|",  #66
                          gen_compara.rendneto_g              , "|",  #67
                          gen_compara.afore_h                 , "|",  #68
                          gen_compara.rend_h                  , "|",  #69
                          gen_compara.comis_h                 , "|",  #70
                          gen_compara.rendneto_h              , "|",  #71
                          gen_compara.afore_i                 , "|",  #72
                          gen_compara.rend_i                  , "|",  #73
                          gen_compara.comis_i                 , "|",  #74
                          gen_compara.rendneto_i              , "|",  #75
                          gen_compara.afore_j                 , "|",  #76
                          gen_compara.rend_j                  , "|",  #77
                          gen_compara.comis_j                 , "|",  #78
                          gen_compara.rendneto_j              , "|",  #79
                          gen_compara.afore_k                 , "|",  #80
                          gen_compara.rend_k                  , "|",  #81
                          gen_compara.comis_k                 , "|",  #82
                          gen_compara.rendneto_k              , "|",  #83
                          gen_compara.afore_l                 , "|",  #84
                          gen_compara.rend_l                  , "|",  #85
                          gen_compara.comis_l                 , "|",  #86
                          gen_compara.rendneto_l              , "|",  #87
                          gen_compara.afore_m                 , "|",  #88
                          gen_compara.rend_m                  , "|",  #89
                          gen_compara.comis_m                 , "|",  #90
                          gen_compara.rendneto_m              , "|",  #91
                          gen_compara.afore_n                 , "|",  #92
                          gen_compara.rend_n                  , "|",  #93
                          gen_compara.comis_n                 , "|",  #94
                          gen_compara.rendneto_n              , "|",  #95
                          gen_compara.afore_o                 , "|",  #96
                          gen_compara.rend_o                  , "|",  #97
                          gen_compara.comis_o                 , "|",  #98
                          gen_compara.rendneto_o              , "|",  #99
                          gen_compara.afore_p                 , "|",  #100
                          gen_compara.rend_p                  , "|",  #101
                          gen_compara.comis_p                 , "|",  #102
                          gen_compara.rendneto_p              , "|",  #103
                          gen_compara.afore_q                 , "|",  #104
                          gen_compara.rend_q                  , "|",  #105
                          gen_compara.comis_q                 , "|",  #106
                          gen_compara.rendneto_q              , "|",  #107
                          gen_compara.afore_r                 , "|",  #108
                          gen_compara.rend_r                  , "|",  #109
                          gen_compara.comis_r                 , "|",  #110
                          gen_compara.rendneto_r              , "|",  #111
                          gen_compara.afore_s                 , "|",  #112
                          gen_compara.rend_s                  , "|",  #113
                          gen_compara.comis_s                 , "|",  #114
                          gen_compara.rendneto_s              , "|",  #115
                          gen_compara.afore_t                 , "|",  #116
                          gen_compara.rend_t                  , "|",  #117
                          gen_compara.comis_t                 , "|",  #118
                          gen_compara.rendneto_t              , "|",  #119
                          gen_compara.afore_u                 , "|",  #120
                          gen_compara.rend_u                  , "|",  #121
                          gen_compara.comis_u                 , "|",  #122
                          gen_compara.rendneto_u              , "|",  #123
                          gen_compara.afore_v                 , "|",  #124
                          gen_compara.rend_v                  , "|",  #125
                          gen_compara.comis_v                 , "|",  #126
                          gen_compara.rendneto_v              , "|",  #127
                          gen_compara.afore_w                 , "|",  #128
                          gen_compara.rend_w                  , "|",  #129
                          gen_compara.comis_w                 , "|",  #130
                          gen_compara.rendneto_w              , "|",  #131
                          gen_compara.afore_x                 , "|",  #132
                          gen_compara.rend_x                  , "|",  #133
                          gen_compara.comis_x                 , "|",  #134
                          gen_compara.rendneto_x              , "|",  #135
                          gen_compara.afore_y                 , "|",  #136
                          gen_compara.rend_y                  , "|",  #137
                          gen_compara.comis_y                 , "|",  #138
                          gen_compara.rendneto_y              , "|",  #139
                          gen_compara.afore_z                 , "|",  #140
                          gen_compara.rend_z                  , "|",  #141
                          gen_compara.comis_z                 , "|",  #142
                          gen_compara.rendneto_z              , "|",  #143
                          barras                              , "|",   #144
						  vprom_simple						 , "|"   #145 #CPL-1182
         
         WHEN "4061" -- Menores de edad
            LET no_carta = "302",ref

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
                          clave_afore USING "&&&"             , "|",  #25
                          des_transferente                    , "|",  #26
                          nacion                              , "|",  #27
                          fena                                , "|",  #28
                          finicta USING "dd/mm/yyyy"          , "|",  #29
                          f_asigna                            , "|",  #30
                          ahorro_retiro                       , "|",  #31
                          sie_rcv                             , "|",  #32
                          ahorro_vol                          , "|",  #33
                          sie_apvol                           , "|",  #34
                          ahorro_viv                          , "|",  #35
                          tot_ahorro                          , "|",  #36
                          asig.largop                         , "|",  #37
                          sie_largop                          , "|",  #38
                          des_transferente                    , "|",  #39
                          gen_compara.afore_a                 , "|",  #40
                          gen_compara.rend_a                  , "|",  #41
                          gen_compara.comis_a                 , "|",  #42
                          gen_compara.rendneto_a              , "|",  #43
                          gen_compara.afore_b                 , "|",  #44
                          gen_compara.rend_b                  , "|",  #45
                          gen_compara.comis_b                 , "|",  #46
                          gen_compara.rendneto_b              , "|",  #47
                          gen_compara.afore_c                 , "|",  #48
                          gen_compara.rend_c                  , "|",  #49
                          gen_compara.comis_c                 , "|",  #50
                          gen_compara.rendneto_c              , "|",  #51
                          gen_compara.afore_d                 , "|",  #52
                          gen_compara.rend_d                  , "|",  #53
                          gen_compara.comis_d                 , "|",  #54
                          gen_compara.rendneto_d              , "|",  #55
                          gen_compara.afore_e                 , "|",  #56
                          gen_compara.rend_e                  , "|",  #57
                          gen_compara.comis_e                 , "|",  #58
                          gen_compara.rendneto_e              , "|",  #59
                          gen_compara.afore_f                 , "|",  #60
                          gen_compara.rend_f                  , "|",  #61
                          gen_compara.comis_f                 , "|",  #62
                          gen_compara.rendneto_f              , "|",  #63
                          gen_compara.afore_g                 , "|",  #64
                          gen_compara.rend_g                  , "|",  #65
                          gen_compara.comis_g                 , "|",  #66
                          gen_compara.rendneto_g              , "|",  #67
                          gen_compara.afore_h                 , "|",  #68
                          gen_compara.rend_h                  , "|",  #69
                          gen_compara.comis_h                 , "|",  #70
                          gen_compara.rendneto_h              , "|",  #71
                          gen_compara.afore_i                 , "|",  #72
                          gen_compara.rend_i                  , "|",  #73
                          gen_compara.comis_i                 , "|",  #74
                          gen_compara.rendneto_i              , "|",  #75
                          gen_compara.afore_j                 , "|",  #76
                          gen_compara.rend_j                  , "|",  #77
                          gen_compara.comis_j                 , "|",  #78
                          gen_compara.rendneto_j              , "|",  #79
                          gen_compara.afore_k                 , "|",  #80
                          gen_compara.rend_k                  , "|",  #81
                          gen_compara.comis_k                 , "|",  #82
                          gen_compara.rendneto_k              , "|",  #83
                          gen_compara.afore_l                 , "|",  #84
                          gen_compara.rend_l                  , "|",  #85
                          gen_compara.comis_l                 , "|",  #86
                          gen_compara.rendneto_l              , "|",  #87
                          gen_compara.afore_m                 , "|",  #88
                          gen_compara.rend_m                  , "|",  #89
                          gen_compara.comis_m                 , "|",  #90
                          gen_compara.rendneto_m              , "|",  #91
                          gen_compara.afore_n                 , "|",  #92
                          gen_compara.rend_n                  , "|",  #93
                          gen_compara.comis_n                 , "|",  #94
                          gen_compara.rendneto_n              , "|",  #95
                          gen_compara.afore_o                 , "|",  #96
                          gen_compara.rend_o                  , "|",  #97
                          gen_compara.comis_o                 , "|",  #98
                          gen_compara.rendneto_o              , "|",  #99
                          gen_compara.afore_p                 , "|",  #100
                          gen_compara.rend_p                  , "|",  #101
                          gen_compara.comis_p                 , "|",  #102
                          gen_compara.rendneto_p              , "|",  #103
                          gen_compara.afore_q                 , "|",  #104
                          gen_compara.rend_q                  , "|",  #105
                          gen_compara.comis_q                 , "|",  #106
                          gen_compara.rendneto_q              , "|",  #107
                          gen_compara.afore_r                 , "|",  #108
                          gen_compara.rend_r                  , "|",  #109
                          gen_compara.comis_r                 , "|",  #110
                          gen_compara.rendneto_r              , "|",  #111
                          gen_compara.afore_s                 , "|",  #112
                          gen_compara.rend_s                  , "|",  #113
                          gen_compara.comis_s                 , "|",  #114
                          gen_compara.rendneto_s              , "|",  #115
                          gen_compara.afore_t                 , "|",  #116
                          gen_compara.rend_t                  , "|",  #117
                          gen_compara.comis_t                 , "|",  #118
                          gen_compara.rendneto_t              , "|",  #119
                          gen_compara.afore_u                 , "|",  #120
                          gen_compara.rend_u                  , "|",  #121
                          gen_compara.comis_u                 , "|",  #122
                          gen_compara.rendneto_u              , "|",  #123
                          gen_compara.afore_v                 , "|",  #124
                          gen_compara.rend_v                  , "|",  #125
                          gen_compara.comis_v                 , "|",  #126
                          gen_compara.rendneto_v              , "|",  #127
                          gen_compara.afore_w                 , "|",  #128
                          gen_compara.rend_w                  , "|",  #129
                          gen_compara.comis_w                 , "|",  #130
                          gen_compara.rendneto_w              , "|",  #131
                          gen_compara.afore_x                 , "|",  #132
                          gen_compara.rend_x                  , "|",  #133
                          gen_compara.comis_x                 , "|",  #134
                          gen_compara.rendneto_x              , "|",  #135
                          gen_compara.afore_y                 , "|",  #136
                          gen_compara.rend_y                  , "|",  #137
                          gen_compara.comis_y                 , "|",  #138
                          gen_compara.rendneto_y              , "|",  #139
                          gen_compara.afore_z                 , "|",  #140
                          gen_compara.rend_z                  , "|",  #141
                          gen_compara.comis_z                 , "|",  #142
                          gen_compara.rendneto_z              , "|",  #143
                          barras                              , "|",   #144
						  vprom_simple						 , "|"   #145 #CPL-1182
						                  
         WHEN "458"
            LET no_carta = "305",ref

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
                          clave_afore USING "&&&"             , "|",  #25
                          des_transferente                    , "|",  #26
                          nacion                              , "|",  #27
                          fena                                , "|",  #28
                          finicta USING "dd/mm/yyyy"          , "|",  #29
                          f_asigna                            , "|",  #30
                          ahorro_retiro                       , "|",  #31
                          sie_rcv                             , "|",  #32
                          ahorro_vol                          , "|",  #33
                          sie_apvol                           , "|",  #34
                          ahorro_viv                          , "|",  #35
                          tot_ahorro                          , "|",  #36
                          asig.largop                         , "|",  #37
                          sie_largop                          , "|",  #38
                          des_transferente                    , "|",  #39
                          gen_compara.afore_a                 , "|",  #40
                          gen_compara.rend_a                  , "|",  #41
                          gen_compara.comis_a                 , "|",  #42
                          gen_compara.rendneto_a              , "|",  #43
                          gen_compara.afore_b                 , "|",  #44
                          gen_compara.rend_b                  , "|",  #45
                          gen_compara.comis_b                 , "|",  #46
                          gen_compara.rendneto_b              , "|",  #47
                          gen_compara.afore_c                 , "|",  #48
                          gen_compara.rend_c                  , "|",  #49
                          gen_compara.comis_c                 , "|",  #50
                          gen_compara.rendneto_c              , "|",  #51
                          gen_compara.afore_d                 , "|",  #52
                          gen_compara.rend_d                  , "|",  #53
                          gen_compara.comis_d                 , "|",  #54
                          gen_compara.rendneto_d              , "|",  #55
                          gen_compara.afore_e                 , "|",  #56
                          gen_compara.rend_e                  , "|",  #57
                          gen_compara.comis_e                 , "|",  #58
                          gen_compara.rendneto_e              , "|",  #59
                          gen_compara.afore_f                 , "|",  #60
                          gen_compara.rend_f                  , "|",  #61
                          gen_compara.comis_f                 , "|",  #62
                          gen_compara.rendneto_f              , "|",  #63
                          gen_compara.afore_g                 , "|",  #64
                          gen_compara.rend_g                  , "|",  #65
                          gen_compara.comis_g                 , "|",  #66
                          gen_compara.rendneto_g              , "|",  #67
                          gen_compara.afore_h                 , "|",  #68
                          gen_compara.rend_h                  , "|",  #69
                          gen_compara.comis_h                 , "|",  #70
                          gen_compara.rendneto_h              , "|",  #71
                          gen_compara.afore_i                 , "|",  #72
                          gen_compara.rend_i                  , "|",  #73
                          gen_compara.comis_i                 , "|",  #74
                          gen_compara.rendneto_i              , "|",  #75
                          gen_compara.afore_j                 , "|",  #76
                          gen_compara.rend_j                  , "|",  #77
                          gen_compara.comis_j                 , "|",  #78
                          gen_compara.rendneto_j              , "|",  #79
                          gen_compara.afore_k                 , "|",  #80
                          gen_compara.rend_k                  , "|",  #81
                          gen_compara.comis_k                 , "|",  #82
                          gen_compara.rendneto_k              , "|",  #83
                          gen_compara.afore_l                 , "|",  #84
                          gen_compara.rend_l                  , "|",  #85
                          gen_compara.comis_l                 , "|",  #86
                          gen_compara.rendneto_l              , "|",  #87
                          gen_compara.afore_m                 , "|",  #88
                          gen_compara.rend_m                  , "|",  #89
                          gen_compara.comis_m                 , "|",  #90
                          gen_compara.rendneto_m              , "|",  #91
                          gen_compara.afore_n                 , "|",  #92
                          gen_compara.rend_n                  , "|",  #93
                          gen_compara.comis_n                 , "|",  #94
                          gen_compara.rendneto_n              , "|",  #95
                          gen_compara.afore_o                 , "|",  #96
                          gen_compara.rend_o                  , "|",  #97
                          gen_compara.comis_o                 , "|",  #98
                          gen_compara.rendneto_o              , "|",  #99
                          gen_compara.afore_p                 , "|",  #100
                          gen_compara.rend_p                  , "|",  #101
                          gen_compara.comis_p                 , "|",  #102
                          gen_compara.rendneto_p              , "|",  #103
                          gen_compara.afore_q                 , "|",  #104
                          gen_compara.rend_q                  , "|",  #105
                          gen_compara.comis_q                 , "|",  #106
                          gen_compara.rendneto_q              , "|",  #107
                          gen_compara.afore_r                 , "|",  #108
                          gen_compara.rend_r                  , "|",  #109
                          gen_compara.comis_r                 , "|",  #110
                          gen_compara.rendneto_r              , "|",  #111
                          gen_compara.afore_s                 , "|",  #112
                          gen_compara.rend_s                  , "|",  #113
                          gen_compara.comis_s                 , "|",  #114
                          gen_compara.rendneto_s              , "|",  #115
                          gen_compara.afore_t                 , "|",  #116
                          gen_compara.rend_t                  , "|",  #117
                          gen_compara.comis_t                 , "|",  #118
                          gen_compara.rendneto_t              , "|",  #119
                          gen_compara.afore_u                 , "|",  #120
                          gen_compara.rend_u                  , "|",  #121
                          gen_compara.comis_u                 , "|",  #122
                          gen_compara.rendneto_u              , "|",  #123
                          gen_compara.afore_v                 , "|",  #124
                          gen_compara.rend_v                  , "|",  #125
                          gen_compara.comis_v                 , "|",  #126
                          gen_compara.rendneto_v              , "|",  #127
                          gen_compara.afore_w                 , "|",  #128
                          gen_compara.rend_w                  , "|",  #129
                          gen_compara.comis_w                 , "|",  #130
                          gen_compara.rendneto_w              , "|",  #131
                          gen_compara.afore_x                 , "|",  #132
                          gen_compara.rend_x                  , "|",  #133
                          gen_compara.comis_x                 , "|",  #134
                          gen_compara.rendneto_x              , "|",  #135
                          gen_compara.afore_y                 , "|",  #136
                          gen_compara.rend_y                  , "|",  #137
                          gen_compara.comis_y                 , "|",  #138
                          gen_compara.rendneto_y              , "|",  #139
                          gen_compara.afore_z                 , "|",  #140
                          gen_compara.rend_z                  , "|",  #141
                          gen_compara.comis_z                 , "|",  #142
                          gen_compara.rendneto_z              , "|",  #143
                          barras                              , "|",   #144
                          vprom_simple						 , "|"   #145 #CPL-1182
         END CASE
##genera arch.

     OUTPUT TO REPORT r_report(v_rep,prog,det_cza)

     SET LOCK MODE TO WAIT
     INSERT INTO int_his_emision_docto VALUES ( prog, p_mae_afi.nss,
                                                fe_carta,
                                                no_carta,
                                                v_rep,
                                                usuario)
       CASE prog 
       WHEN "407"
          UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                 int_ctr_carta.fecha_genera = fe_carta,
                 int_ctr_carta.hora_genera  = ho_ra ,
                 int_ctr_carta.consecutivo  = consecutivo_envio
           WHERE int_ctr_carta.nss = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
             AND int_ctr_carta.docto_cod = 30202
             AND int_ctr_carta.edo_genera   = 10
       
       WHEN "4071" -- Menores de edad
          UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                 int_ctr_carta.fecha_genera = fe_carta,
                 int_ctr_carta.hora_genera  = ho_ra ,
                 int_ctr_carta.consecutivo  = consecutivo_envio
           WHERE int_ctr_carta.nss            = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
             AND int_ctr_carta.docto_cod      = 30202
             AND int_ctr_carta.edo_genera     = 10
             
       WHEN "406"
          UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                 int_ctr_carta.fecha_genera = fe_carta,
                 int_ctr_carta.hora_genera  = ho_ra ,
                 int_ctr_carta.consecutivo  = consecutivo_envio
           WHERE int_ctr_carta.nss = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
             AND int_ctr_carta.docto_cod = 30201
             AND int_ctr_carta.edo_genera   = 10
       
       WHEN "4061" -- Menores de edad
          UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                 int_ctr_carta.fecha_genera = fe_carta,
                 int_ctr_carta.hora_genera  = ho_ra ,
                 int_ctr_carta.consecutivo  = consecutivo_envio
           WHERE int_ctr_carta.nss = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
             AND int_ctr_carta.docto_cod = 30201
             AND int_ctr_carta.edo_genera   = 10
             
       WHEN "458"
          UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                 int_ctr_carta.fecha_genera = fe_carta,
                 int_ctr_carta.hora_genera  = ho_ra ,
                 int_ctr_carta.consecutivo  = consecutivo_envio
           WHERE int_ctr_carta.nss = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
             AND int_ctr_carta.docto_cod = 30501
             AND int_ctr_carta.edo_genera   = 10
       END CASE

     INSERT INTO int_constancia VALUES ( folio,
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