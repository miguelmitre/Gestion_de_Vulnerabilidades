#is*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                    #
#Propietario       => E.F.P.                                          #
#Programa          => INTB01003 INTB0133 INTB0153                     #
#Descripcion       => GLOBALS y FUNCION DETALLE_NOTIFICA, CZA_NOTIFICA#
#Sistema           => INT .                                           #
#Fecha Elaboracion => 26 de Abril del 2002   .                        #
#Elaborado por     => Laura Eugenia Cortes Guzman                     #
#Fecha ult.Modif.  => 26 de Abril del 2002   .                        #
#Modificado por    => Laura Eugenia Cortes Guzman                     #
#*********************************************************************#
# para cambiar a 120 posiciones buscar /#cam enter
#
DATABASE safre_af
GLOBALS
    DEFINE p_tabafore               RECORD LIKE tab_afore_local.*,
           hoy_hoy                  DATE,
           fentcons                 DATE,
           f_rechazo                DATE,
           rdeta_cod                CHAR(03),
           fecha_no_local           DATE,
           fe_cha                   DATE,
           fecha_gene_car           DATE,
           procanase                CHAR(50),

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
           rfc                      CHAR(13),
           curp                     CHAR(18),
           motivo                   CHAR(03),
           rechazo                  CHAR(120),
#hasta aqui
           inter_noti               CHAR(300),
           selec_tab                CHAR(10000),

           long, i                  INTEGER,
           ban,  jj                 INTEGER,

           clave_afore              SMALLINT,
           codigo_cert              SMALLINT,

           consec_lote              INTEGER,

           numero_reg               DECIMAL(10,0),
           numero_reg_noafil        DECIMAL(10,0),

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
                  rechazo           CHAR(120)
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
                  poblacion         SMALLINT   ,
                  pais_cod          CHAR(3)
           END RECORD,

           pais_desc                CHAR(40),

           reg1   ARRAY[10000] OF RECORD
                  f_rechazo         DATE,
                  rdeta_cod         CHAR(03),
                  n_seguro          CHAR(11)
                  --folio             DECIMAL(10,0)
           END RECORD,

           reg    RECORD
                  f_rechazo         DATE,
                  rdeta_cod         CHAR(03),
                  n_seguro          CHAR(11),
                  folio             DECIMAL(10,0)
           END RECORD,

           reg_carta      RECORD LIKE  int_ctr_carta.*,

           re_chazo                 CHAR(120),
#hasta aqui

           fe_carta                 DATE,
           ho_ra, usuario           CHAR(08),
           diferencia               INTEGER,
           fecha                    CHAR(10),
           v_rep                    CHAR(2200)

   DEFINE tam_cod_rdeta SMALLINT
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

   LET numero_reg = 0         LET numero_reg_noafil = 0
   CALL limpia()

   LET jj = 1
   PREPARE apt_1 FROM selec_tab
   DECLARE cur_1 CURSOR FOR apt_1
   FOREACH cur_1 INTO reg1[jj].*

       LET fe_cha = reg1[jj].f_rechazo USING "MM/DD/YYYY"

       CASE prog
       WHEN 433
            SELECT UNIQUE a.n_seguro,       a.n_rfc,     a.n_unico,    
                   a.paterno,        a.materno,   a.nombres,          
                   a.sexo,           a.n_folio,   a.fentcons,              
                   a.tipo_solicitud, b.observacion
            INTO  p_mae_afi.*
            FROM  afi_solicitud a,  afi_rechaza_cert b  
            WHERE (a.status_interno in(40,42))
              AND  a.tipo_solicitud = 1           
              AND  a.n_seguro       = reg1[jj].n_seguro  
              AND  b.rdeta_cod      = reg1[jj].rdeta_cod 
              AND  b.f_rechazo      = reg1[jj].f_rechazo 
              AND  a.fentcons       = reg1[jj].f_rechazo 
              AND  a.n_seguro       = b.n_seguro  
              AND  a.fentcons       = b.f_rechazo 
              AND  (b.rdeta_cod      <> '20' OR
                    b.rdeta_cod      <> '860')
              --AND  a.n_folio        = reg1[jj].folio
              AND  a.n_folio        = b.n_folio
              AND  b.tipo_solicitud = a.tipo_solicitud         
              GROUP BY 1,2,3,4,5,6,7,8,9,10,11

       WHEN 447
            SELECT UNIQUE a.n_seguro,       a.n_rfc,     a.n_unico,    
                   a.paterno,        a.materno,   a.nombres,          
                   a.sexo,           a.n_folio,   a.fentcons,              
                   a.tipo_solicitud, b.observacion
            INTO  p_mae_afi.*
            FROM  afi_solicitud a,  afi_rechaza_cert b  
            WHERE (a.status_interno in(40,42))
              AND  a.tipo_solicitud = 8           
              AND  a.n_seguro       = reg1[jj].n_seguro  
              AND  b.rdeta_cod      = reg1[jj].rdeta_cod 
              AND  b.f_rechazo      = reg1[jj].f_rechazo 
              AND  a.fentcons       = reg1[jj].f_rechazo 
              AND  a.n_seguro       = b.n_seguro  
              AND  a.fentcons       = b.f_rechazo 
              AND  b.rdeta_cod      <> '20'         
              --AND  a.n_folio        = reg1[jj].folio
              AND  a.n_folio        = b.n_folio
              AND  b.tipo_solicitud = a.tipo_solicitud         
              GROUP BY 1,2,3,4,5,6,7,8,9,10,11
       END CASE

       LET tam_cod_rdeta = LENGTH (reg1[jj].rdeta_cod)
       IF tam_cod_rdeta = 2 THEN
          LET reg1[jj].rdeta_cod = '0',reg1[jj].rdeta_cod CLIPPED
       END IF 

       IF prog = 447 THEN 
           SELECT a.rdeta_desc_c INTO re_chazo FROM tab_rdeta a
           WHERE  a.rdeta_cod = reg1[jj].rdeta_cod 
             AND  a.modulo_cod= "ind"
       ELSE
           SELECT a.rdeta_desc_l INTO re_chazo FROM tab_rdeta a
           WHERE  a.rdeta_cod = reg1[jj].rdeta_cod 
             AND  a.modulo_cod= "afi"
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
                    WHERE n.nss      = p_mae_afi.nss
                      AND n.telefono IS NOT NULL
                      AND n.tel_cod < 7
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
                  AND s.n_folio = p_mae_afi.folio
                  AND s.tipo_solicitud = p_mae_afi.tipo_solicitud
                  AND s.marca_envio    = "X"

         IF SQLCA.SQLCODE = 0 THEN
            SELECT o.calle, o.numero, o.depto, o.colonia, o.codpos,
                   o.delega, o.estado, o.ciudad, o.pais_cod
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
               LET p_dom.numero = "          "
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

            IF p_dom.pais_cod IS NOT NULL THEN
               SELECT m.pais_desc INTO pais_desc FROM tab_pais m
               WHERE  m.pais_cod = p_dom.pais_cod
               IF STATUS = NOTFOUND THEN
                  LET pais_desc = "                                        "
               END IF
            ELSE
               LET pais_desc = "                                        "
            END IF

         END IF

## procanase
	   LET procanase = "                    ",
	                   "                    ",
	                   "          "


            LET rechazo = p_mae_afi.rechazo
            LET long = 0
            LET i    = 0
            LET long = LENGTH(rechazo)

            IF long < 120 THEN
               LET long = long + 1
               FOR i = long TO 120
                   LET rechazo[i,i] = " "
               END FOR
            END IF
            LET p_mae_afi.rechazo = rechazo

            LET motivo = "   "

            LET rechazo = "                                        ",
                          "                                        ",
                          "                                        "


            LET tipo_proceso = p_mae_afi.tipo_solicitud USING "&"

##fecha_carta
         LET fe_carta = TODAY USING "mm/dd/yyyy"
         LET ho_ra = TIME
##entidad
         LET entidad = "    "
##fecha generacion de la carta
         LET fecha_creacion = TODAY USING "dd/mm/yyyy"
         LET fecha_barra    = TODAY USING "yyyymmdd"
##no_carta
         CASE prog
         WHEN 433  LET no_carta = "302",ref
         WHEN 447  LET no_carta = "305",ref
         END CASE
##codigo_barras

         LET barras = "C",no_carta,"000",p_mae_afi.nss,
                      fecha_barra

        LET numero_reg = numero_reg + 1

        CASE prog
        WHEN 433
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
                          p_dom.codigo_postal                 , "|",  #14
                          centro_reparto                      , "|",  #15
                          telefono                            , "|",  #16
                          fecha_creacion                      , "|",  #18
                          p_mae_afi.nss                       , "|",  #19
                          folio                               , "|",  #10
                          tipo_proceso                        , "|",  #11
                          "          "                        , "|",  #22
                          reg1[jj].rdeta_cod                  , "|",  #23
                          motivo                              , "|",  #24
                          motivo                              , "|",  #25
                          motivo                              , "|",  #26
                          motivo                              , "|",  #27
                          re_chazo                            , "|",  #28
                          rechazo                             , "|",  #29
                          rechazo                             , "|",  #30
                          rechazo                             , "|",  #31
                          rechazo                             , "|",  #32
                          procanase                           , "|",  #33
                          barras                              , "|"   #34

        WHEN 447
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
                          p_dom.codigo_postal                 , "|",  #14
                          centro_reparto                      , "|",  #15
                          telefono                            , "|",  #16
                          fecha_creacion                      , "|",  #18
                          p_mae_afi.nss                       , "|",  #19
                          folio                               , "|",  #10
                          tipo_proceso                        , "|",  #11
                          "          "                        , "|",  #22
                          reg1[jj].rdeta_cod                  , "|",  #23
                          motivo                              , "|",  #24
                          motivo                              , "|",  #25
                          motivo                              , "|",  #26
                          motivo                              , "|",  #27
                          re_chazo                            , "|",  #28
                          rechazo                             , "|",  #29
                          rechazo                             , "|",  #30
                          rechazo                             , "|",  #31
                          rechazo                             , "|",  #32
                          procanase                           , "|",  #33
                          curp                                , "|",  #33
                          rfc                                 , "|",  #33
                          barras                              , "|"   #34

        END CASE
     OUTPUT TO REPORT r_report(v_rep,prog,det_cza)

     SET LOCK MODE TO WAIT

     INSERT INTO int_his_emision_docto VALUES ( prog, p_mae_afi.nss,
                                                fe_carta,
                                                no_carta,
                                                v_rep,
                                                usuario)

        UPDATE int_ctr_carta    
	   SET int_ctr_carta.edo_genera   = 20,     #1-Carta Emitida
               int_ctr_carta.fecha_genera = fe_carta,
               int_ctr_carta.hora_genera  = ho_ra ,
               int_ctr_carta.consecutivo  = numero_reg
         WHERE int_ctr_carta.nss          = p_mae_afi.nss
           AND int_ctr_carta.fecha_registro = p_mae_afi.fecha_certifica
           AND int_ctr_carta.docto_cod    = no_carta
           AND int_ctr_carta.edo_genera   = 10


     INSERT INTO int_constancia VALUES ( p_mae_afi.folio,
                                         p_mae_afi.nss,
                                         p_mae_afi.tipo_solicitud,
                                         TODAY,
                                         p_mae_afi.fecha_certifica,
                                         no_carta )
     SET LOCK MODE TO NOT WAIT


     DISPLAY "REGISTROS : ",numero_reg USING "####&" AT 17,1

     CALL limpia()
     LET jj = jj + 1

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
           fe_cha      CHAR(10),
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
   WHEN 433   LET no_carta = "302",ref
   WHEN 447   LET no_carta = "305",ref
   END CASE

   LET fe_cha = TODAY USING "DD/MM/YYYY"


   LET v_rep = "01",                         "|",
               no_carta,                     "|",
               leyenda_cza,                  "|",
               afore_local,                  "|",
               fe_cha        ,               "|",
               consec_lote USING "&&&&&&&&", "|",
               numero_reg  USING "&&&&&&&&", "|"


   OUTPUT TO REPORT r_report(v_rep,prog,det_cza)

   INSERT INTO int_notifica_folio 
               VALUES (consec_lote, fecha_creacion, ref, numero_reg)

   FINISH REPORT r_report
END FUNCTION

FUNCTION limpia()
    INITIALIZE p_tabafore.*, p_dom.*, p_mae_afi.*, p_telefono.* TO NULL
    INITIALIZE fecha_creacion, calle, depto, colonia, telefono TO NULL
    INITIALIZE desc_delega, entidad, poblacion, centro_reparto,folio TO NULL
    INITIALIZE tipo_proceso, paterno, materno, nombres TO NULL
    INITIALIZE motivo, rechazo,numero, sexo TO NULL
    INITIALIZE re_chazo TO NULL

    LET i           = 0
    LET domi        = 0
    LET long        = 0
    LET diferencia  = 0
    LET clave_afore = 0
    LET tam_cod_rdeta = 0

END FUNCTION
