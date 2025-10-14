##############################################################################
#Project              => SAFRE (Mexico)
#Owner                => E.F.P.
#Programa UNI_DOM     => DESPLIEGA DATOS SOLICITANTE POR NSS
#Creado por           => OMAR SANDOVAL BADILLO              
#Fecha elaboracion    => 18 abril 2005                      
#Sistema              => UNI
##############################################################################
DATABASE safre_af 

FUNCTION busca_datos(x_nss,x_folio,x_tipo_solicitud)
   DEFINE enc              RECORD
          nss              CHAR(11),
          folio            INTEGER,
          tipo_solicitud   SMALLINT,
          d_tiposolicitud  CHAR(3),
          curp             CHAR(18),
          paterno          CHAR(50),
          materno          CHAR(50),
          nombres          CHAR(50),
          f_ingreso        DATE,
          desc_afore       CHAR(37),
          f_registro       DATE,
          status           CHAR(50),
          rfc              CHAR(18)
   END RECORD

   DEFINE  x_nss	   CHAR(11)
   DEFINE  x_folio	   INTEGER
   DEFINE  x_curp	   CHAR(18)
   DEFINE  x_origen	   CHAR(20)
   DEFINE  x_cod_promotor   LIKE afi_mae_afiliado.cod_promotor
   DEFINE  x_tipo_solicitud LIKE afi_mae_afiliado.tipo_solicitud

   DEFINE  x_status        SMALLINT,
           n_veces         SMALLINT,
           b_rechazo       SMALLINT, 
           x_origen_cod    SMALLINT,
           i               SMALLINT,
           estados         SMALLINT

   DEFINE opc              CHAR(1)
   DEFINE estado_desc      CHAR(60)

   LET b_rechazo      = 0
   LET x_origen_cod   = 0
   LET x_origen       = "INEXISTENTE"
   LET x_status       = 0
   LET x_cod_promotor = NULL
   #LET enc.desc_afore = ""

   OPEN WINDOW uni_dom1 AT 2,2 WITH FORM "UNI_DOM1" ATTRIBUTE(BORDER)
   DISPLAY "                          CONSULTA DE PREAPERTURA                              " AT 2,1 ATTRIBUTE(REVERSE)   

   ERROR "Buscando Datos ..."
        
   SELECT n_seguro,		#Maeafili
          n_folio,
          tipo_solicitud,
          n_unico,
          paterno,
          materno,
          nombres,
          frecafor,
          fentcons,
          status_interno,
	  cod_promotor,
          estadon,
          n_rfc
   INTO   enc.nss,
          enc.folio,
          enc.tipo_solicitud,
          enc.curp,
          enc.paterno,
          enc.materno,
          enc.nombres,
          enc.f_ingreso,
          enc.f_registro,
          enc.status,
          x_cod_promotor,
          estados,
          enc.rfc
   FROM   afi_mae_afiliado
   WHERE  n_seguro = x_nss

   IF STATUS <> NOTFOUND THEN
      LET x_status     = enc.status
      LET x_origen     = "AFILIADO"
      LET x_origen_cod = 1
      ERROR ""
   ELSE
      SELECT n_seguro,
             n_folio,
             tipo_solicitud,
             n_unico,
             paterno,
             materno,
             nombres,
             frecafor,
             fentcons,
             status_interno,
	     cod_promotor,
             estadon,
             n_rfc
      INTO   enc.nss,
             enc.folio,
             enc.tipo_solicitud,
             enc.curp,
             enc.paterno,
             enc.materno,
             enc.nombres,
             enc.f_ingreso,
             enc.f_registro,
             enc.status,
             x_cod_promotor,
             estados,
             enc.rfc
      FROM   afi_solicitud
      WHERE  n_seguro = x_nss
      AND    n_folio  = x_folio
      AND    tipo_solicitud = x_tipo_solicitud

      IF STATUS <> NOTFOUND THEN
	 LET x_status     = enc.status
	 LET x_origen     = "SOLICITUD"
	 LET x_origen_cod = 2
         ERROR ""
      END IF
   END IF

   CASE
      WHEN enc.tipo_solicitud = 0
         LET enc.d_tiposolicitud = "EXT"
      WHEN enc.tipo_solicitud = 1
         LET enc.d_tiposolicitud = "REG"
      WHEN enc.tipo_solicitud = 2
         LET enc.d_tiposolicitud = "TRA"
      WHEN enc.tipo_solicitud = 3
         LET enc.d_tiposolicitud = "UNI"
      WHEN enc.tipo_solicitud = 4
         LET enc.d_tiposolicitud = "VIR"
      WHEN enc.tipo_solicitud = 5
         LET enc.d_tiposolicitud = "ASI"
   END CASE

   CALL muestra_status(x_origen_cod,x_origen,x_status)
      RETURNING estado_desc

   LET enc.status = estado_desc
   #CALL estado_marca(enc.nss)
   
   DISPLAY BY NAME enc.nss THRU enc.rfc
   DISPLAY BY NAME enc.desc_afore
   DISPLAY "                         Datos Generales del Solicitante                       " AT 9,1 ATTRIBUTE(REVERSE)

   CALL Despliega_datos(enc.nss,
                        enc.folio,
                        x_origen_cod,
                        enc.tipo_solicitud)

END FUNCTION
###########################################################################
FUNCTION Despliega_datos(x_nss,x_folio,origen_cod,x_tipo_solicitud) 

   DEFINE x_nss            CHAR(11)
   DEFINE x_folio          LIKE afi_mae_afiliado.n_folio
   DEFINE x_curp           CHAR(18)
   DEFINE x_tipo_solicitud SMALLINT
   DEFINE pat              CHAR(20),
          mat              CHAR(20),
          nom              CHAR(20)

   DEFINE opc              CHAR(1)
   DEFINE tel              CHAR(40)

   DEFINE motivorec        SMALLINT, 
          b_verificacion   SMALLINT,
          origen_cod       SMALLINT,
          i                SMALLINT,
          n_veces          SMALLINT

   DEFINE enc RECORD
          nss              CHAR(11),
          folio            INTEGER,
          curp             CHAR(18),
          paterno          CHAR(50),
          materno          CHAR(50),
          nombres          CHAR(50),
          f_ingreso        DATE,
          desc_afore       CHAR(37),
          f_registro       DATE,
          status           CHAR(50),
          rfc              CHAR(18)
   END RECORD

   DEFINE datos RECORD
          f_solicitud        LIKE afi_mae_afiliado.fecha_elaboracion,
          f_envio            LIKE afi_mae_afiliado.fecha_envio,
          f_certifica        LIKE afi_mae_afiliado.fentcons,
          cod_promotor       CHAR(11),
          nombre_pro         CHAR(30),
          rechazo_cod        LIKE afi_rechaza_cert.rdeta_cod,
          rechazo_desc       CHAR(30), 
          telefonop          CHAR(20),
          sexo               LIKE afi_mae_afiliado.sexo,
          desc_sexo          CHAR(60),
          fena               LIKE afi_mae_afiliado.fena,
          estadon            LIKE afi_mae_afiliado.estadon,
          desc_estadon       CHAR(60),
          nacionalidad       LIKE afi_mae_afiliado.nacionalidad,
          tip_prob          LIKE afi_mae_afiliado.tip_prob,
          fol_prob          LIKE afi_mae_afiliado.fol_prob,
          doc_prob          LIKE afi_mae_afiliado.doc_prob,
          docprob_desc       LIKE tab_doc_prob.docprob_desc,
          ind_infonavit      LIKE afi_mae_afiliado.ind_infonavit
   END RECORD

   DEFINE domicilio ARRAY[100] OF RECORD
          calle              LIKE  afi_domicilio.calle,
          num_ext            LIKE  afi_domicilio.numero,
          num_int            LIKE  afi_domicilio.depto,
          colon_desc         CHAR(50),
          codpos             LIKE  afi_domicilio.codpos,
          deleg_desc         CHAR(50),
          deleg_cod          LIKE afi_domicilio.delega,
          ciudad_desc        CHAR(50),
          ciudad_cod         LIKE afi_domicilio.ciudad,
          estad_desc         CHAR(50),
          estad_cod          LIKE afi_domicilio.estado,
          telefono           CHAR(40),
          extension          CHAR(05),
          tipo_dom           LIKE afi_domicilio.dom_cod,
          marca_envio        LIKE afi_domicilio.marca_envio,
          marca_desc         CHAR(16),
	  dom_desc           CHAR(20)
          #tel_desc           CHAR(15)
   END RECORD

   DEFINE total_dom          SMALLINT
   DEFINE mintelefono        INTEGER,
          cuantos_tel        SMALLINT

   DEFINE x_cve_lada         CHAR(3),
          xx_dom_desc        CHAR(20)

   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP

   INITIALIZE datos.* TO NULL
   INITIALIZE domicilio TO NULL

   LET b_verificacion = 0

   CASE origen_cod
      WHEN 1
         SELECT count(*) 
         INTO   n_veces 
         FROM   afi_mae_afiliado
         WHERE  n_seguro = x_nss
         AND    n_folio  = x_folio
         AND    tipo_solicitud = x_tipo_solicitud

         IF n_veces > 1 THEN
            ##RETURN n_veces
         ELSE
            SELECT MIN(rowid)
            INTO   mintelefono
            FROM   afi_telefono
            WHERE  nss = x_nss
            AND    n_folio = x_folio
            AND    tipo_solicitud = x_tipo_solicitud

            DECLARE cur_dom CURSOR FOR
            SELECT dom.calle,
                   dom.numero,
                   dom.depto,
                   dom.colonia,
                   dom.codpos,
                   dom.delega,
                   dom.ciudad,
                   dom.estado,
                   dom.dom_cod,
                   dom.marca_envio
            FROM   afi_domicilio dom
            WHERE  dom.nss            = x_nss 
            AND    dom.n_folio        = x_folio
            AND    dom.tipo_solicitud = x_tipo_solicitud
            ORDER BY 10 DESC

            LET i = 1

            FOREACH cur_dom INTO domicilio[i].calle,
                                 domicilio[i].num_ext,
                                 domicilio[i].num_int,
                                 domicilio[i].colon_desc,
                                 domicilio[i].codpos,
                                 domicilio[i].deleg_cod,
                                 domicilio[i].ciudad_cod,
                                 domicilio[i].estad_cod,
                                 domicilio[i].tipo_dom,
                                 domicilio[i].marca_envio

               SELECT dom_desc
               INTO   domicilio[i].dom_desc
               FROM   tab_domicilio
               WHERE  dom_cod = domicilio[i].tipo_dom

               IF domicilio[i].marca_envio = "X" THEN
                  LET domicilio[i].marca_desc = "CORRESPONDENCIA"
               ELSE
                  LET domicilio[i].marca_desc = "_______________"
               END IF

               SELECT telefono,
                      cve_lada,
                      extension
               INTO   domicilio[i].telefono,
                      x_cve_lada,
                      domicilio[i].extension
               FROM   afi_telefono
               WHERE  nss = x_nss
               AND    n_folio = x_folio
               AND    tipo_solicitud = x_tipo_solicitud
               AND    rowid = mintelefono

               IF x_cve_lada IS NULL OR x_cve_lada = " " THEN
                  LET domicilio[i].telefono = domicilio[i].telefono
               ELSE
                  LET domicilio[i].telefono = "(",x_cve_lada CLIPPED,") ",
                                              domicilio[i].telefono
               END IF

               SELECT estad_desc
               INTO   domicilio[i].estad_desc
               FROM   tab_estado 
               WHERE  estad_cod = domicilio[i].estad_cod

               IF STATUS = NOTFOUND THEN
                  LET domicilio[i].estad_desc = "NO ESPECIFICADO"
               END IF

               SELECT deleg_desc
               INTO   domicilio[i].deleg_desc
               FROM   tab_delegacion
               WHERE  deleg_cod = domicilio[i].deleg_cod

               IF STATUS = NOTFOUND THEN
                  LET domicilio[i].deleg_desc = "NO ESPECIFICADO"
               END IF

               SELECT ciudad_desc 
               INTO   domicilio[i].ciudad_desc
               FROM   tab_ciudad
               WHERE  ciudad_cod = domicilio[i].ciudad_cod

               IF STATUS = NOTFOUND THEN
                  LET domicilio[i].ciudad_desc = "NO ESPECIFICADO"
               END IF

               LET i = i + 1
            END FOREACH

            SELECT afi.fecha_elaboracion,
                   afi.fecha_envio,
                   afi.fentcons,
                   afi.cod_promotor,
                   afi.sexo,
                   afi.fena,
                   afi.estadon,
                   afi.tip_prob,
                   afi.fol_prob,
                   afi.doc_prob,
                   afi.ind_infonavit,
                   afi.nacionalidad
            INTO   datos.f_solicitud,
                   datos.f_envio,
                   datos.f_certifica,
                   datos.cod_promotor,
                   datos.sexo,
                   datos.fena,
                   datos.estadon,
                   datos.tip_prob,
                   datos.fol_prob,
                   datos.doc_prob,
                   datos.ind_infonavit,
                   datos.nacionalidad
            FROM   afi_mae_afiliado afi
            WHERE  afi.n_seguro = x_nss 
            AND    afi.n_folio  = x_folio
            AND    afi.tipo_solicitud = x_tipo_solicitud

            SELECT paterno,
                   materno,
                   nombres,
                   fono
            INTO   pat,
                   mat,
                   nom,
                   tel
            FROM   pro_mae_promotor
            WHERE  cod_promotor = datos.cod_promotor
         END IF 
      WHEN 2
         SELECT COUNT(*) 
         INTO   n_veces 
         FROM   afi_solicitud
         WHERE  n_seguro = x_nss
         AND    n_folio = x_folio
         AND    tipo_solicitud = x_tipo_solicitud

         IF n_veces > 1 THEN
            ##RETURN n_veces 
         ELSE
            SELECT MIN(rowid)
            INTO   mintelefono
            FROM   afi_telefono
            WHERE  nss = x_nss
            AND    n_folio = x_folio
            AND    tipo_solicitud = x_tipo_solicitud

            DECLARE cur_dom1 CURSOR FOR
            SELECT dom.calle,
                   dom.numero,
                   dom.depto,
                   dom.colonia,
                   dom.codpos,
                   dom.delega,
                   dom.ciudad,
                   dom.estado,
                   dom.dom_cod,
                   dom.marca_envio
            FROM   afi_domicilio dom
            WHERE  dom.nss            = x_nss 
            AND    dom.n_folio        = x_folio
            AND    dom.tipo_solicitud = x_tipo_solicitud
            ORDER BY 10 DESC

            LET i = 1
            FOREACH cur_dom1 INTO domicilio[i].calle,
                                  domicilio[i].num_ext,
                                  domicilio[i].num_int,
                                  domicilio[i].colon_desc,
                                  domicilio[i].codpos,
                                  domicilio[i].deleg_cod,
                                  domicilio[i].ciudad_cod,
                                  domicilio[i].estad_cod,
                                  domicilio[i].tipo_dom,
                                  domicilio[i].marca_envio

               SELECT dom_desc
               INTO   domicilio[i].dom_desc
               FROM   tab_domicilio
               WHERE  dom_cod = domicilio[i].tipo_dom

               SELECT telefono,
                      cve_lada,
                      extension
               INTO   domicilio[i].telefono,
                      x_cve_lada,
                      domicilio[i].extension
               FROM   afi_telefono
               WHERE  nss = x_nss
               AND    n_folio = x_folio
               AND    tipo_solicitud = x_tipo_solicitud
               AND    rowid = mintelefono

               IF x_cve_lada IS NULL OR x_cve_lada = " " THEN
                  LET domicilio[i].telefono = domicilio[i].telefono
               ELSE
                  LET domicilio[i].telefono = "(",x_cve_lada CLIPPED,") ",
                                              domicilio[i].telefono
               END IF

              IF domicilio[i].marca_envio = "X" THEN
                 LET domicilio[i].marca_desc = "CORRESPONDENCIA"
              ELSE
                 LET domicilio[i].marca_desc = "_______________"
              END IF

              SELECT estad_desc
              INTO   domicilio[i].estad_desc
              FROM   tab_estado 
              WHERE  estad_cod = domicilio[i].estad_cod

              IF STATUS = NOTFOUND THEN
                 LET domicilio[i].estad_desc = "NO ESPECIFICADO"
              END IF

              SELECT deleg_desc
              INTO   domicilio[i].deleg_desc
              FROM   tab_delegacion
              WHERE  deleg_cod = domicilio[i].deleg_cod

              IF STATUS = NOTFOUND THEN
                 LET domicilio[i].deleg_desc = "NO ESPECIFICADO"
              END IF

              SELECT ciudad_desc 
              INTO   domicilio[i].ciudad_desc
              FROM   tab_ciudad
              WHERE  ciudad_cod = domicilio[i].ciudad_cod

              IF STATUS = NOTFOUND THEN
                 LET domicilio[i].ciudad_desc = "NO ESPECIFICADO"
              END IF
              LET i = i + 1
            END FOREACH

            SELECT afi.fecha_elaboracion,
                   afi.fecha_envio,
                   afi.fentcons,
                   afi.cod_promotor,
                   afi.sexo,
                   afi.fena,
                   afi.estadon,
                   afi.tip_prob,
                   afi.fol_prob,
                   afi.doc_prob,
                   afi.ind_infonavit,
                   afi.nacionalidad
            INTO   datos.f_solicitud,
                   datos.f_envio,
                   datos.f_certifica,
                   datos.cod_promotor,
                   datos.sexo,
                   datos.fena,
                   datos.estadon,
                   datos.tip_prob,
                   datos.fol_prob,
                   datos.doc_prob,
                   datos.ind_infonavit,
                   datos.nacionalidad
            FROM   afi_solicitud afi
            WHERE  afi.n_seguro = x_nss 
            AND    afi.n_folio  = x_folio
            AND    afi.tipo_solicitud = x_tipo_solicitud

            SELECT paterno,
                   materno,
                   nombres,
                   fono
            INTO   pat,
                   mat,
                   nom,
                   tel
            FROM   pro_mae_promotor
            WHERE  cod_promotor = datos.cod_promotor
         END IF 

      WHEN 3
      WHEN 4
         SELECT count(*) 
         INTO   n_veces 
         FROM   rec_mae_externo
         WHERE  n_seguro = x_nss

         IF n_veces > 1 THEN
            ## RETURN n_veces 
         ELSE 
            LET i = 1

            SELECT factualiza,
                   "",
                   "",
                   "",
                   fonop,
                   callep,
                   numep,
                   deptop,
                   codposp,
                   coloniap,
                   delegap,
                   ciudadp,
                   estadop,
                   sexo,
                   fena,
                   estadop,
                   "",
                   "",
                   "",
                   "",
                   ""
            INTO   datos.f_solicitud,
                   datos.f_envio,
                   datos.f_certifica,
                   datos.cod_promotor,
                   domicilio[i].telefono,
                   domicilio[i].calle,
                   domicilio[i].num_ext,
                   domicilio[i].num_int,
                   domicilio[i].codpos,
                   domicilio[i].colon_desc,
                   domicilio[i].deleg_cod,
                   domicilio[i].ciudad_cod,
                   domicilio[i].estad_cod,
                   datos.sexo,
                   datos.fena,
                   datos.estadon,
                   datos.tip_prob,
                   datos.fol_prob,
                   datos.doc_prob,
                   datos.ind_infonavit,
                   datos.nacionalidad
            FROM   rec_mae_externo
            WHERE  n_seguro = x_nss

            LET datos.cod_promotor = ""
            LET domicilio[i].marca_desc = "_______________"

            SELECT estad_desc
            INTO   domicilio[i].estad_desc
            FROM   tab_estado 
            WHERE  estad_cod = domicilio[i].estad_cod

            IF STATUS = NOTFOUND THEN
               LET domicilio[i].estad_desc = "NO EXISTE"
            END IF

            SELECT deleg_desc
            INTO   domicilio[i].deleg_desc
            FROM   tab_delegacion
            WHERE  deleg_cod = domicilio[i].deleg_cod

            IF STATUS = NOTFOUND THEN
               LET domicilio[i].deleg_desc = "NO EXISTE"
            END IF

            SELECT ciudad_desc
            INTO   domicilio[i].ciudad_desc
            FROM   tab_ciudad
            WHERE  ciudad_cod = domicilio[i].ciudad_cod

            IF STATUS = NOTFOUND THEN
               LET domicilio[i].ciudad_desc = "NO EXISTE"
            END IF
            LET i = i + 1
         END IF  

      WHEN 0
         LET b_verificacion = 1
         EXIT CASE 
   END CASE

   IF b_verificacion = 0 THEN
      LET n_veces = 1

      SELECT COUNT(*)
      INTO   cuantos_tel
      FROM   afi_telefono
      WHERE  nss = x_nss
      AND    n_folio = x_folio
      AND    tipo_solicitud = x_tipo_solicitud

      IF cuantos_tel > 1 THEN
         DISPLAY " Control : <T>Telefonos  <C>Salir " AT 20,1 ATTRIBUTE(REVERSE)
      END IF

      LET datos.telefonop  = tel
      LET datos.nombre_pro = pat CLIPPED, " ",
                             mat CLIPPED, " ",
                             nom CLIPPED

      SELECT estad_desc 
      INTO   datos.desc_estadon
      FROM   tab_estado
      WHERE  estad_cod = datos.estadon

      IF STATUS = NOTFOUND THEN
         LET datos.desc_estadon = "NO EXISTE"
      END IF

      SELECT sexo_desc
      INTO   datos.desc_sexo
      FROM   tab_sexo
      WHERE  sexo_cod = datos.sexo

      SELECT docprob_desc 
      INTO   datos.docprob_desc 
      FROM   tab_doc_prob
      WHERE  docprob_cod = datos.tip_prob

      IF STATUS = NOTFOUND THEN
         LET datos.docprob_desc = "NO EXISTE"
      END IF

      LET enc.nss   = x_nss
      LET enc.folio = ""
      LET datos.rechazo_desc = enc.desc_afore[1,30] 
      ERROR ""

      DISPLAY datos.* TO scr_afi.*

      LET total_dom = i - 1

      DISPLAY " Domicilios : " AT 14,1
      DISPLAY total_dom AT 14,16
      DISPLAY "_" AT 14,62
      DISPLAY "_" AT 14,78 

      IF (i-1) >= 1 THEN
         CALL SET_COUNT(i-1)

         DISPLAY ARRAY domicilio TO scr_dom.*
            ON KEY (INTERRUPT)
               EXIT DISPLAY

               ON KEY (CONTROL-T)
                  IF cuantos_tel > 1 THEN
                     CALL consulta_mas_telefonos(x_nss,
                                                 x_folio,
                                                 x_tipo_solicitud)
                  END IF
         END DISPLAY
         PROMPT "[Enter] para Regresar ... " FOR opc 
      ELSE
         ERROR "NO TIENE DOMICILIO..."
         SLEEP 2
         ERROR ""
      END IF
      CLOSE WINDOW uni_dom1
   ELSE
      ERROR " NO EXISTEN DATOS REGISTRADOS ..."
      SLEEP 2
      ERROR "" 
      LET n_veces = 0
   END IF 
END FUNCTION
#####################################################################
FUNCTION consulta_mas_telefonos(xx_nss,
                                xx_n_folio,
                                xx_tipo_solicitud)

   DEFINE xx_nss             CHAR(11),
          xx_n_folio         INTEGER,
          xx_tipo_solicitud  SMALLINT,
          pos                SMALLINT

   DEFINE reg_tel ARRAY[30] OF RECORD
          trap            CHAR(1),
          telefono        CHAR(40),
          tel_cod         SMALLINT,
          extension       CHAR(20)
   END RECORD

   DEFINE x_cve_lada      CHAR(3)
   DEFINE x_email         CHAR(40)
   DEFINE cur_row         SMALLINT,
	  scr_row         SMALLINT,
	  cont_inp        SMALLINT,
	  item_row_cnt    SMALLINT

   DEFINE sql_stat        INTEGER		 
   DEFINE sw              SMALLINT,
	  i               SMALLINT,
	  opc             CHAR(1)

   OPEN WINDOW uni_dom2 AT 18,49 WITH FORM "UNI_DOM2"
   DISPLAY "<Ctrl-C> Salir" AT 5,1 ATTRIBUTE(REVERSE)

   LET x_email = NULL

   DECLARE cursor_tel CURSOR FOR
   SELECT "",
          telefono,
          tel_cod,
          extension,
          cve_lada
   FROM   afi_telefono
   WHERE  nss = xx_nss
   AND    n_folio = xx_n_folio
   AND    tipo_solicitud = xx_tipo_solicitud

   LET pos = 1

   FOREACH cursor_tel INTO reg_tel[pos].*,
                           x_cve_lada

      IF x_cve_lada IS NULL OR x_cve_lada = " " THEN
         LET reg_tel[pos].telefono = reg_tel[pos].telefono
      ELSE
         LET reg_tel[pos].telefono = "(",x_cve_lada CLIPPED,") ",
                                     reg_tel[pos].telefono
      END IF

      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos > 0 THEN
      CALL SET_COUNT(pos)
      LET cont_inp = TRUE

      WHILE (cont_inp = TRUE)
        INPUT ARRAY reg_tel WITHOUT DEFAULTS FROM scr_tel_1.*
           AFTER ROW
              LET cur_row = ARR_CURR()
              LET scr_row = SCR_LINE()

              DISPLAY reg_tel[cur_row].* TO scr_tel_1[scr_row].*

           BEFORE ROW
              LET cur_row = ARR_CURR()
              LET scr_row = SCR_LINE()

              IF reg_tel[cur_row].tel_cod = 7 THEN
				     ERROR "E-mail: ",reg_tel[cur_row].telefono
				  END IF

              IF (cur_row = pos + 1) THEN
                 LET cont_inp = TRUE
                 EXIT INPUT
              ELSE
                 LET scr_row = SCR_LINE()
                 DISPLAY reg_tel[cur_row].* TO scr_tel_1[scr_row].* 
                 LET cont_inp = FALSE
              END IF

           ON KEY (INTERRUPT)
              LET sw = 1
              EXIT INPUT
        END INPUT
      END WHILE
      CLOSE WINDOW uni_dom2
   ELSE
      ERROR " NO EXISTEN DATOS REGISTRADOS ..."
      SLEEP 2
      ERROR "" 
   END IF
END FUNCTION
#####################################################
FUNCTION muestra_status(x_origen_cod,x_origen,x_estado)
   DEFINE x_origen_cod    SMALLINT
   DEFINE x_estado        SMALLINT
   DEFINE x_estado_desc   CHAR(50),
          x_origen        CHAR(20)

   CASE x_origen_cod
      WHEN 1 SELECT est.estado_desc
             INTO x_estado_desc
             FROM tab_status_afi est
             WHERE est.estado_cod = x_estado

      WHEN 2 SELECT est.estado_desc
             INTO x_estado_desc
             FROM tab_status_afi est
             WHERE est.estado_cod = x_estado

      WHEN 3 SELECT est.estado_desc
             INTO x_estado_desc
             FROM tab_status_afi est
             WHERE est.estado_cod = x_estado
   END CASE

   IF x_estado_desc IS NULL THEN
      LET x_estado_desc = ""
   END IF

   LET x_estado_desc = x_origen CLIPPED,": ",x_estado_desc CLIPPED
   LET x_estado_desc = x_estado_desc CLIPPED

   RETURN x_estado_desc
END FUNCTION


