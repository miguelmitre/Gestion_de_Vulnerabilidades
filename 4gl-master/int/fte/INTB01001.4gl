#*********************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                    #
#Propietario       => E.F.P.                                          #
#Programa          => INTB01001 y INTB0134                            #
#Descripcion       => GLOBALS y FUNCION DETALLE_NOTIFICA, CZA_NOTIFICA#
#Sistema           => INT .                                           #
#Fecha             => 18 de Abril del 2002   .                        #
#Por               => Laura Eugenia Cortes Guzman                     #
#*********************************************************************#
DATABASE safre_af
GLOBALS
    DEFINE p_tabafore               RECORD LIKE tab_afore_local.*,
           hoy_hoy                  DATE,
           fe_carta                 DATE,
           fentcons                 DATE,
           fecha_presentacion       DATE,
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
           f_asigna                 CHAR(10),
           paterno                  CHAR(40),
           materno                  CHAR(40),
           nombres                  CHAR(40),
           curp                     CHAR(18),
           rfc                      CHAR(13),
           desc_motivo              CHAR(120),
           status_proceso           CHAR(01),
           inter_noti               CHAR(300),
           selec_tab1               CHAR(2000),

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
                  tipo_solicitud    SMALLINT,
                  fecha_presenta    DATE,
                  motivo_rechazo    CHAR(03),
                  proceso           CHAR(01)
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

           ho_ra                    CHAR(08),
           usuario                  CHAR(08),
           diferencia               INTEGER,
           fecha                    CHAR(10),
           motivo_rechazo           CHAR(03),
           --moti_vo                  CHAR(02),
           moti_vo                  CHAR(03),
           mo_ti_vo                 SMALLINT,
           m_otivo                  SMALLINT,

           v_rep                    CHAR(2200)

END GLOBALS
#
# ---------------------------------------------------------------------
# Funcion del detalle
# ---------------------------------------------------------------------
#

FUNCTION detalle_notifica_34(ref,prog,det_cza)
   DEFINE prog             SMALLINT,
          det_cza          SMALLINT,
          cuantas_emi      SMALLINT,
          emi_carta        CHAR(01),
          ref              CHAR(02)

   CALL limpia()
   PREPARE apt_34 FROM selec_tab1
   DECLARE cur_34 CURSOR FOR apt_34
   FOREACH cur_34 INTO p_mae_afi.*

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
##telefono
         DECLARE apt_tel CURSOR FOR
             SELECT n.telefono FROM afi_telefono n
                    WHERE n.n_folio        = p_mae_afi.folio
                    AND n.tipo_solicitud = p_mae_afi.tipo_solicitud
                            AND n.nss      = p_mae_afi.nss
                      AND n.telefono IS NOT NULL
                      AND n.tel_cod  <> 7
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
         SELECT MAX(s.ROWID)
                INTO  domi
                FROM  afi_domicilio s
                WHERE s.n_folio = p_mae_afi.folio
                  AND s.tipo_solicitud = p_mae_afi.tipo_solicitud
                        AND s.nss = p_mae_afi.nss
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
                LET calle= p_dom.calle
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
                LET numero= p_dom.numero
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

         LET cuantas_emi = 0
         SELECT COUNT(*) INTO cuantas_emi FROM int_ctr_carta
         WHERE nss        = p_mae_afi.nss
         AND   docto_cod  = no_carta
         IF cuantas_emi = 0 OR cuantas_emi IS NULL THEN
            LET emi_carta = "1"
         ELSE
            LET emi_carta = "2"
         END IF

         INITIALIZE moti_vo TO NULL
         LET mo_ti_vo = 0


         IF p_mae_afi.proceso = "D" THEN
             LET m_otivo = p_mae_afi.motivo_rechazo
             SELECT a.desc_rech
             INTO   desc_motivo
             FROM   tab_dev_taa a
             WHERE  a.cod_rech = m_otivo
             LET p_mae_afi.motivo_rechazo = m_otivo USING "&&&"
         ELSE
            CASE p_mae_afi.motivo_rechazo
               WHEN "000"
                   LET desc_motivo = "SOLICITUD NO ATENDIDA POR LA ",
                                     "AFORE TRANSFERENTE"
               WHEN "00"
                   LET p_mae_afi.motivo_rechazo = "000"
                   LET desc_motivo = "SOLICITUD NO ATENDIDA POR LA ",
                                     "AFORE TRANSFERENTE"
               WHEN "0"
                   LET p_mae_afi.motivo_rechazo = "000"
                   LET desc_motivo = "SOLICITUD NO ATENDIDA POR LA ",
                                     "AFORE TRANSFERENTE"
               WHEN "4"
                   LET desc_motivo = "CUENTA EN PROCESO DE RETIRO "
                   LET p_mae_afi.motivo_rechazo = "004"
               WHEN "04"
                   LET desc_motivo = "CUENTA EN PROCESO DE RETIRO "
                   LET p_mae_afi.motivo_rechazo = "004"
               WHEN "004"
                   LET p_mae_afi.motivo_rechazo = "004"
                   LET desc_motivo = "CUENTA EN PROCESO DE RETIRO "
               WHEN "5"
                   LET p_mae_afi.motivo_rechazo = "005"
                   LET desc_motivo = "CUENTA EN PROCESO DE TRASPASO "
               WHEN "05"
                   LET p_mae_afi.motivo_rechazo = "005"
                   LET desc_motivo = "CUENTA EN PROCESO DE TRASPASO "
               WHEN "005"
                   LET p_mae_afi.motivo_rechazo = "005"
                   LET desc_motivo = "CUENTA EN PROCESO DE TRASPASO "
               WHEN "6"
                   LET p_mae_afi.motivo_rechazo = "006"
                   LET desc_motivo = "CUENTA EN PROCESO DE DEVOLUCION DE PAGOS ",
                                     "EN EXCESO "
               WHEN "06"
                   LET p_mae_afi.motivo_rechazo = "006"
                   LET desc_motivo = "CUENTA EN PROCESO DE DEVOLUCION DE PAGOS ",
                                     "EN EXCESO "
               WHEN "006"
                   LET p_mae_afi.motivo_rechazo = "006"
                   LET desc_motivo = "CUENTA EN PROCESO DE DEVOLUCION DE PAGOS ",
                                     "EN EXCESO "
               WHEN "7"
                   LET p_mae_afi.motivo_rechazo = "007"
                   LET desc_motivo = "CUENTA EN PROCESO DE DEVOLUCION DE PAGOS ",
                                     "SIN JUSTIFICACION LEGAL "
               WHEN "07"
                   LET p_mae_afi.motivo_rechazo = "007"
                   LET desc_motivo = "CUENTA EN PROCESO DE DEVOLUCION DE PAGOS ",
                                     "SIN JUSTIFICACION LEGAL "
               WHEN "007"
                   LET p_mae_afi.motivo_rechazo = "007"
                   LET desc_motivo = "CUENTA EN PROCESO DE DEVOLUCION DE PAGOS ",
                                     "SIN JUSTIFICACION LEGAL "
               WHEN "008"
                   LET p_mae_afi.motivo_rechazo = "008"
                   LET desc_motivo = "CUENTA CON DIFERENCIAS EN NOMBRE "
               WHEN "08"
                   LET p_mae_afi.motivo_rechazo = "008"
                   LET desc_motivo = "CUENTA CON DIFERENCIAS EN NOMBRE "
               WHEN "8"
                   LET p_mae_afi.motivo_rechazo = "008"
                   LET desc_motivo = "CUENTA CON DIFERENCIAS EN NOMBRE "
               WHEN "9"
                   LET p_mae_afi.motivo_rechazo = "009"
                   LET desc_motivo = "CUENTA EN PROCESO DE INTERESES 43 BIS."
               WHEN "09"
                   LET p_mae_afi.motivo_rechazo = "009"
                   LET desc_motivo = "CUENTA EN PROCESO DE INTERESES 43 BIS."
               WHEN "009"
                   LET p_mae_afi.motivo_rechazo = "009"
                   LET desc_motivo = "CUENTA EN PROCESO DE INTERESES 43 BIS."
               WHEN "011"
                   LET p_mae_afi.motivo_rechazo = "011"
                   LET desc_motivo = "CUENTA EN PROCESO DE TRASPASO "
               WHEN "11"
                   LET p_mae_afi.motivo_rechazo = "011"
                   LET desc_motivo = "CUENTA EN PROCESO DE TRASPASO "
               WHEN "015"
                   LET p_mae_afi.motivo_rechazo = "015"
                   LET desc_motivo = "CUENTA EN PROCESO DE UNIFICACION "
               WHEN "15"
                   LET p_mae_afi.motivo_rechazo = "015"
                   LET desc_motivo = "CUENTA EN PROCESO DE UNIFICACION "
               WHEN "018"
                   LET p_mae_afi.motivo_rechazo = "018"
                   LET desc_motivo = "CUENTA EN PROCESO DE REVERSO "
               WHEN "18"
                   LET p_mae_afi.motivo_rechazo = "018"
                   LET desc_motivo = "CUENTA EN PROCESO DE REVERSO "
               WHEN "028"
                   LET p_mae_afi.motivo_rechazo = "028"
                   LET desc_motivo = "FIRMA DEL TRABAJADOR NO COINCIDE"
               WHEN "28"
                LET p_mae_afi.motivo_rechazo = "028"
                   LET desc_motivo = "FIRMA DEL TRABAJADOR NO COINCIDE"
               WHEN "029"
                   LET p_mae_afi.motivo_rechazo = "029"
                   LET desc_motivo = "EL TRABAJADOR NO DESEA EL TRASPASO"
               WHEN "29"
                   LET p_mae_afi.motivo_rechazo = "029"
                   LET desc_motivo = "EL TRABAJADOR NO DESEA EL TRASPASO"
               WHEN "030"
                   LET p_mae_afi.motivo_rechazo = "030"
                   LET desc_motivo = "LA CUENTA SE ENCUENTRA EN PROCESO ",
                                     "JUDICIAL"
               WHEN "30"
                   LET p_mae_afi.motivo_rechazo = "030"
                   LET desc_motivo = "LA CUENTA SE ENCUENTRA EN PROCESO ",
                                     "JUDICIAL"
               WHEN "031"
                   LET p_mae_afi.motivo_rechazo = "031"
                   LET desc_motivo = "LOS DOCUMENTOS DIGITALIZADOS NO ",
                                     "COINCIDEN, ESTAN INCOMPLETOS O ILEGIBLES"
               WHEN "31"
                   LET p_mae_afi.motivo_rechazo = "031"
                   LET desc_motivo = "LOS DOCUMENTOS DIGITALIZADOS NO ",
                                     "COINCIDEN, ESTAN INCOMPLETOS O ILEGIBLES"
               WHEN "032"
                   LET p_mae_afi.motivo_rechazo = "032"
                   LET desc_motivo = "CUENTA EN PROCESO DE MODIFICACION ",
                                     "DE DATOS"
               WHEN "32"
                   LET p_mae_afi.motivo_rechazo = "032"
                   LET desc_motivo = "CUENTA EN PROCESO DE MODIFICACION ",
                                     "DE DATOS"
               WHEN "33"
                   LET p_mae_afi.motivo_rechazo = "033"
                   LET desc_motivo = "IMAGENES ILEGIBLES "
               WHEN "033"
                   LET p_mae_afi.motivo_rechazo = "033"
                   LET desc_motivo = "IMAGENES ILEGIBLES "
               WHEN "34"
                   LET p_mae_afi.motivo_rechazo = "034"
                   LET desc_motivo = "IMAGENES INCOMPLETAS POR FALTA DE ",
                                     "ALGUN(OS) DOCUMENTO(S)"
               WHEN "034"
                   LET p_mae_afi.motivo_rechazo = "034"
                   LET desc_motivo = "IMAGENES INCOMPLETAS POR FALTA DE ",
                                     "ALGUN(OS) DOCUMENTO(S)"
            END CASE
         END IF

         LET mo_ti_vo = p_mae_afi.motivo_rechazo
         --LET moti_vo  = mo_ti_vo USING "&&"
         LET moti_vo  = mo_ti_vo USING "&&&"

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
                       f_certifica                         , "|",  #20
                       folio                               , "|",  #21
                       entidad                             , "|",  #22
                       moti_vo                             , "|",  #23
                       p_mae_afi.proceso                   , "|",  #24
                       desc_motivo                         , "|",  #25
                       rfc                                 , "|",  #26
                       no_carta                            , "|",  #27
                       p_mae_afi.nss                       , "|",  #28
                       fecha_barra                         , "|",  #29
                       folio                               , "|",  #30
                       emi_carta                           , "|",  #31
                       curp                                , "|",  #32
                       "000"                               , "|"   #33

##genera arch.

     OUTPUT TO REPORT r_report(v_rep,prog,det_cza)

     SET LOCK MODE TO WAIT

     INSERT INTO int_his_emision_docto VALUES ( prog, p_mae_afi.nss,
                                                fe_carta,
                                                no_carta,
                                                v_rep,
                                                usuario)

    SELECT "f.X" FROM int_ctr_carta f
           WHERE f.nss = p_mae_afi.nss
             AND f.fecha_registro = p_mae_afi.fecha_presenta
             AND f.docto_cod    = 30227
             AND f.edo_genera   = 10
    IF STATUS = NOTFOUND  THEN
       INSERT INTO int_ctr_carta VALUES(p_mae_afi.nss,
                                        p_mae_afi.folio,
                                        p_mae_afi.tipo_solicitud,
                                        p_mae_afi.fecha_presenta,
                                        30227,
                                        p_mae_afi.proceso,
                                        20,
                                        TODAY,
                                        ho_ra,
                                        0,
                                        numero_reg,
                                        0
                                       )
    ELSE

           UPDATE int_ctr_carta    
              SET int_ctr_carta.edo_genera = 20,     #1-Carta Emitida
                  int_ctr_carta.fecha_genera = fe_carta,
                  int_ctr_carta.hora_genera  = ho_ra ,
                  int_ctr_carta.consecutivo  = numero_reg
           WHERE int_ctr_carta.nss = p_mae_afi.nss
             AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_presenta
             AND int_ctr_carta.docto_cod    = 30227
             AND int_ctr_carta.edo_genera   = 10

    END IF

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
FUNCTION cabeza_notifica_34(ref,leyenda_cza,prog,det_cza)

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

   LET fecha_creacion = TODAY USING "MM/DD/YYYY"
 
   INSERT INTO int_notifica_folio 
               VALUES (consec_lote, fecha_creacion, ref, numero_reg)

   FINISH REPORT r_report
END FUNCTION

FUNCTION limpia()
    INITIALIZE p_tabafore.*, p_dom.*, p_mae_afi.*, p_telefono.*      TO NULL
    INITIALIZE fecha_creacion, calle, depto, colonia, telefono       TO NULL
    INITIALIZE desc_delega, entidad, poblacion, centro_reparto,folio TO NULL
    INITIALIZE tipo_proceso, paterno, materno, nombres               TO NULL

    LET i        = 0 LET domi        = 0 LET long        = 0
    LET cod_mot  = 0 LET diferencia  = 0 LET clave_afore = 0
END FUNCTION
