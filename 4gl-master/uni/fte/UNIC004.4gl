#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.           				            #
#Programa          => GENERA SOLICITUDES DE CERTIFICACION                   #
#Fecha             => 21 de febrero del 2000                                #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                      #
#Fecha Actualiza   => 28 de noviembre 2003                                  #
#Fecha Actualiza   => 31 de marzo 2004                                  #
#############################################################################
DATABASE safre_af
GLOBALS
      DEFINE g_reg2  RECORD 
          nss               CHAR(11),
          curp              CHAR(18),
          rfc               CHAR(13),
          paterno           CHAR(40),
          materno           CHAR(40),
          nombre            CHAR(40),
          nombre_imss       CHAR(50),
          fecha_nac         DATE,
          sexo              CHAR(01),
          --ent_nac           CHAR(02),
          ent_nac           SMALLINT,
          nacionalidad      CHAR(03),
          ind_cred_info     CHAR(1),
          cve_afo_ced       CHAR(03),
	  folio             INTEGER
      END RECORD

      DEFINE g_reg6  RECORD 
          folio             INTEGER,
          nss               CHAR(11),
          curp              CHAR(18),
          rfc               CHAR(13),
          paterno           CHAR(40),
          materno           CHAR(40),
          nombre            CHAR(40),
          nombre_imss       CHAR(50),
          fecha_nac         DATE,
          sexo              CHAR(01),
          ent_nac           CHAR(02),
          ind_cred_info     CHAR(01),
          nacionalidad      CHAR(03),
          cve_afo_ced       CHAR(03)
      END RECORD

      DEFINE g_reg7  RECORD 
          nss               CHAR(11),
          rfc               CHAR(13),
          fecha_nac         DATE,
          sexo              CHAR(01),
          ent_nac           CHAR(02)
      END RECORD

      DEFINE g_cza_uni_trasp RECORD
          folio                INTEGER,
          tipo_registro        CHAR(02),
          ident_servicio       CHAR(02),
          ident_operacion      CHAR(02),
          tipo_ent_origen      CHAR(02),
          cve_ent_origen       CHAR(03),
          tipo_ent_destino     CHAR(02),
          cve_ent_destino      CHAR(03),
          fecha_transfer       DATE,
          ent_federativa       CHAR(03),
          consec_lote          SMALLINT,
          cve_mod_recep        CHAR(02),
          estado               SMALLINT
      END RECORD

      DEFINE g_det_uni_trasp RECORD
          folio                INTEGER,
          tipo_registro        CHAR(02),
          cont_servicio        INTEGER,
          cve_operacion        CHAR(02),
          nss                  CHAR(11),
          curp                 CHAR(18),
          rfc                  CHAR(13),
          paterno              CHAR(40),
          materno              CHAR(40),
          nombre               CHAR(40),
          fecha_nac            DATE,
          sexo                 CHAR(01),
          --ent_nac              CHAR(02),
          ent_nac              SMALLINT,
          ind_cred_info        CHAR(01),
          nacionalidad         CHAR(03),
          cve_doc_proba        CHAR(01),
          cve_afo_ced          CHAR(03),
          estado               SMALLINT
      END RECORD


      DEFINE g_sum_uni_trasp RECORD
          folio                integer ,
          tipo_registro        char(2),
          tipo_ent_origen      char(2),
          cve_ent_origen       char(3),
          fecha_transfer       date,
          consec_dia           integer ,
          id_servicio          char(2),
          id_operacion         char(2),
          total_entrada        integer,
          estado               smallint
      END RECORD

      DEFINE reg_4 RECORD
          folio                INTEGER,
          tipo_registro        CHAR(02),
          ident_servicio       CHAR(02),
          ident_operacion      CHAR(02),
          tipo_ent_origen      CHAR(02),
          cve_ent_origen       CHAR(03),
          tipo_ent_destino     CHAR(02),
          cve_ent_destino      CHAR(03),
          fecha_transfer       DATE,
          ent_federativa       CHAR(03),
          consec_lote          SMALLINT,
          cve_mod_recep        CHAR(02),
          estado               SMALLINT
      END RECORD

      DEFINE reg_5 RECORD
          folio                integer ,
          tipo_registro        char(2),
          tipo_ent_origen      char(2),
          cve_ent_origen       char(3),
          fecha_transfer       date,
          consec_dia           integer ,
          id_servicio          char(2),
          id_operacion         char(2),
          total_entrada        integer,
          estado               smallint
      END RECORD

      DEFINE reg_3 RECORD #glo #reg_3
          folio                INTEGER,
          tipo_registro        CHAR(2),
          cont_servicio        INTEGER,
          cve_operacion        CHAR(2),
          nss                  CHAR(11),
          curp                 CHAR(18),
          rfc                  CHAR(13),
          paterno              CHAR(40),
          materno              CHAR(40),
          nombre               CHAR(40),
          fecha_nac            DATE,
          sexo                 CHAR(1),
          --ent_nac              CHAR(2),
          ent_nac              SMALLINT,
          ind_cred_info        CHAR(1),
          nacionalidad         CHAR(3),
          cve_doc_proba        CHAR(1),
          cve_afo_ced          CHAR(3),
	  fecha_certifica      DATE,
          estado               SMALLINT
      END RECORD

      DEFINE #glo #integer
          cont                 ,
          vlote                ,
          vfolio               ,
          vcodigo_afore        ,
          vsolicitado          ,
          vconfronta           ,
          vaceptado            ,
          vgenera              ,
          tot_registros        INTEGER

      DEFINE #glo #date
          HOY                  DATE

      DEFINE #glo #char
          G_LISTA	       	CHAR(500),
          cat            	CHAR(500),
          borra          	CHAR(200),
          vnombre         	CHAR(50),
          vclave_entidad  	CHAR(3),
          enter           	CHAR(1),
          aux_pausa       	CHAR(1),
          vpregunta             CHAR(40),
          char            	CHAR(1)

      DEFINE g_paramgrales   	RECORD LIKE seg_modulo.*

     DEFINE vfecha   DATE
END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("UNIC004.log")
    CALL inicio()  #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY = TODAY

    SELECT codigo_afore
    INTO   vcodigo_afore
    FROM   tab_afore_local

    LET vclave_entidad = "09 "

    SELECT estado
    INTO   vconfronta
    FROM   uni_status
    WHERE  descripcion = "CONFRONTADO"

    SELECT estado
    INTO   vaceptado
    FROM   uni_status
    WHERE  descripcion = "ACEPTADO CONFRONTA"

    SELECT estado
    INTO   vsolicitado
    FROM   uni_status
    WHERE  descripcion = "SOLICITADO"

    SELECT MAX(lotes_num) + 1
    INTO   vlote
    FROM   tab_lote
    WHERE  lotes_cod   = 13
    AND    lotes_fecha = HOY

    IF vlote IS NULL THEN
        LET vlote = 1
    END IF

    INSERT INTO tab_lote VALUES(HOY,13,"UNIFICACION DE CUENTAS",0,vlote)

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0031" ATTRIBUTE(BORDER)
    DISPLAY "UNIC004         GENERA SOLICITUDES DE CERTIFICACION                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        UNIFICACION DE CUENTAS                                       " AT 4,1 ATTRIBUTE(REVERSE)

    DISPLAY "                                 < Ctrl-C > Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vpregunta

      AFTER FIELD vpregunta
	IF vpregunta = "S"
	OR vpregunta = "s"  THEN

            SELECT "X"
            FROM   uni_unificado b
            WHERE  b.tipo_ent_cta1 = "00"
            AND    b.estado       = vaceptado
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                SELECT "X"
                FROM   uni_unificador
                WHERE  estado           = vaceptado
                AND    ident_movimiento = "02"
                AND    cve_afo_recep    = vcodigo_afore
                AND    tipo_ent_nss     = "00"
                #AND    estado_familia   = 1
                GROUP BY 1
       
                IF STATUS = NOTFOUND THEN
                    ERROR "NO HAY SOLICITUDES PARA CERTIFICACION"
                    SLEEP 3
                    ERROR ""
                    EXIT PROGRAM
                END IF

            END IF

      ERROR " PROCESANDO INFORMACION " 

 CALL miercoles_siguiente(HOY)
      RETURNING vfecha

   LET vfecha = HOY

      CALL solicitud_traspaso() #st
      CALL genera_encabezado() #ge
      CALL genera_sumario() #gs
      CALL segundo_paso() #sp

      IF vgenera = 1 THEN
          ERROR " " 
      DISPLAY "ARCHIVO GENERADO EN : ",g_paramgrales.ruta_envio CLIPPED,
              "/",HOY USING"YYYYMMDD",".30UN" AT 16,1
          PROMPT "PROCESO FINALIZADO ...[ENTER] PARA CONTINUAR "
          FOR enter
          EXIT INPUT
      ELSE
          ERROR " " 
          DISPLAY "NO HAY SOLICITUDES DE LA OPERACION 30 " AT 16,1
          PROMPT "PROCESO FINALIZADO ...[ENTER] PARA CONTINUAR "
          FOR enter
          EXIT INPUT
      END IF

    ELSE
	ERROR" PROCESO CANCELADO "
	SLEEP 2
        ERROR ""
	EXIT PROGRAM
    END IF
      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM

    END INPUT

    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END FUNCTION
################################################################## Traspaso    
FUNCTION solicitud_traspaso()
#st
    DEFINE nss_unifica    CHAR(11)
    DEFINE nss_unifica1   CHAR(11)
    DEFINE xnss           CHAR(11)
    DEFINE probatorio     char(1)
    DEFINE probatorio1    char(1)
    DEFINE sexo           char(1)
    DEFINE nac            char(2)
    DEFINE tipo           char(2)
    DEFINE fecha          date 
    DEFINE cont_num       integer
    DEFINE curp           char(18)
    DEFINE rfc            char(13)
    DEFINE vrfc1          char(13)
    DEFINE pater          char(40)
    DEFINE mater          char(40)
    DEFINE nombre         char(40)
    DEFINE xpaterno,xmaterno,xnombres CHAR(40)

    DECLARE cur_1 CURSOR FOR

       SELECT a.nss_uni,
              a.rfc_uni,
              a.sexo_uni,
              a.ent_nac_uni,
              a.fecha_nac_uni,
              a.tipo_ent_nss
       FROM   uni_unificador a
       WHERE  a.ident_movimiento = "02"
       AND    a.cve_afo_recep    = vcodigo_afore
       AND    a.estado_familia   = 1

    FOREACH cur_1 INTO nss_unifica,rfc,sexo,nac,fecha,tipo

       DECLARE cur_2 CURSOR FOR

          SELECT b.nss_cta1,
                 b.curp_cta1,
                 b.rfc_cta1,
                 b.paterno_cta1,
                 b.materno_cta1,
                 b.nombre_cta1,
                 b.nombre_imss_cta1,
                 b.fecha_nac_cta1,
                 b.sexo_cta1,
                 b.ent_nac_cta1,
		 " ",
		 " ",
                 b.cve_ent_cta1,
		 b.folio
          FROM   uni_unificado b
          WHERE  b.nss_uni       = nss_unifica
          AND    b.tipo_ent_cta1 = "00"
          AND    b.diag_unifica  = "01"
          AND    b.estado        = vaceptado
 
       FOREACH cur_2 INTO g_reg2.*
 
           IF g_reg2.nombre_imss IS NOT NULL THEN
               CALL separa_nombre(g_reg2.nombre_imss)
               RETURNING xpaterno,xmaterno,xnombres
    
               LET g_reg2.paterno = xpaterno
               LET g_reg2.materno = xmaterno
               LET g_reg2.nombre  = xnombres
           END IF

           IF (g_reg2.paterno IS NULL OR
               g_reg2.paterno MATCHES " *") THEN
               LET g_reg2.paterno = "N/A"
           END IF

           IF (g_reg2.materno IS NULL OR
               g_reg2.materno MATCHES " *") THEN
               LET g_reg2.materno = "N/A"
           END IF

           IF (g_reg2.nombre  IS NULL OR
               g_reg2.nombre  MATCHES " *") THEN
               LET g_reg2.nombre  = "N/A"
           END IF

           IF g_reg2.ent_nac <> "35"  THEN
               LET g_reg2.nacionalidad = "MEX"
           END IF

           LET probatorio = ""
           LET vrfc1 = g_reg2.rfc

           IF tipo = "59" OR
	      tipo = "00" THEN
              LET cont_num = 0
              DECLARE cur_num CURSOR FOR
                  SELECT nss_cta1
                  FROM   uni_unificado
                  WHERE  nss_uni = nss_unifica
                  AND    tipo_ent_cta1 = "01"
                  AND    cve_ent_cta1 = vcodigo_afore
                  #AND    estado       = vaceptado
              FOREACH cur_num INTO xnss
                  LET cont_num = 1
                  IF cont_num = 1 THEN
                      EXIT FOREACH
                  END IF
              END FOREACH

              ###  8 Dic 2009 Modificación por muertos  
              IF    xnss IS NULL  
               OR   xnss  = ' ' THEN 
                  SELECT  nss   
                    INTO  nss_unifica 
                    FROM  uni_fallecido
                   WHERE  nss           = nss_unifica
                     AND  cve_afore     = vcodigo_afore
                     AND  tipo_registro = '59'          
                     AND  tipo_nss      =   1
                     AND  folio         = g_reg2.folio
              ELSE          
                  LET nss_unifica = xnss
              END IF
              ###  8 Dic 2009 Modificación por muertos  
           END IF

           SELECT a.n_unico,
                  a.n_rfc,
                  a.fena,
                  a.sexo,
                  a.estadon
           INTO   g_reg2.curp,
                  g_reg2.rfc,
                  g_reg2.fecha_nac,
                  g_reg2.sexo,
                  g_reg2.ent_nac
           FROM   afi_mae_afiliado a
           WHERE  a.n_seguro = nss_unifica

           IF (g_reg2.curp IS NULL OR
               g_reg2.curp MATCHES " *") THEN
               LET probatorio = ""
           ELSE
               LET probatorio = "5"
           END IF

           IF (g_reg2.rfc IS NULL OR
               g_reg2.rfc MATCHES " *") THEN
               LET g_reg2.rfc = vrfc1
           END IF

           LET g_reg2.ind_cred_info = "0"
           LET nac = g_reg2.ent_nac using"&&"

           SELECT "X"
           FROM   afi_mae_afiliado
           WHERE  n_seguro      = nss_unifica
           AND    ind_infonavit = "S"
           GROUP BY 1

           IF STATUS <> NOTFOUND THEN
              LET g_reg2.ind_cred_info = "1"
           END IF

           IF g_reg2.cve_afo_ced = "548" THEN
              LET g_reg2.cve_afo_ced = "552"
           END IF
           IF g_reg2.cve_afo_ced = "514" THEN
              LET g_reg2.cve_afo_ced = "538"
	   END IF

	   SELECT "X"
	   FROM   uni_det_certifica
	   WHERE  nss   = g_reg2.nss
	   GROUP BY 1

           IF STATUS = NOTFOUND  THEN

               INSERT INTO uni_det_certifica VALUES(
                                       g_reg2.folio, #folio
                                       "00", #tipo_registro
                                       "", #cont_servicio
                                       "30", #cve_operacion
                                       g_reg2.nss,  #nss
                                       g_reg2.curp, #curp
                                       g_reg2.rfc,  #rfc
                                       g_reg2.paterno, #paterno
                                       g_reg2.materno, #materno
                                       g_reg2.nombre , #nombre
                                       g_reg2.fecha_nac, #fecha_nac
                                       g_reg2.sexo,
                                       --g_reg2.ent_nac,
                                       nac,
                                       g_reg2.ind_cred_info, #ind_cred_info
                                       g_reg2.nacionalidad, #nacionalidad
                                       probatorio, #cve_doc_proba
                                       g_reg2.cve_afo_ced,
                                       "",
                                       "",
                                       "10" #estado
                                       )

               INSERT INTO uni_his_solicitud VALUES(
                                       g_reg2.folio, #folio
                                       "30", #cve_operacion
                                       g_reg2.nss,  #nss
                                       g_reg2.cve_afo_ced,
                                       HOY)

		END IF

           UPDATE uni_unificado
           SET    estado   = vsolicitado
           WHERE  folio    = g_reg2.folio
           AND    nss_cta1 = g_reg2.nss
           AND    cve_ent_cta1 = g_reg2.cve_afo_ced
           AND    estado   = vaceptado

           CALL actualiza_unificador(nss_unifica,g_reg2.folio)

{
           UPDATE uni_unificador
           SET    estado  = vsolicitado
           WHERE  folio   = g_reg2.folio
           AND    nss_uni = nss_unifica
}

       END FOREACH

    END FOREACH

#Para solicitar nss_unificadores

    DECLARE cur_6 CURSOR FOR

       SELECT nss_uni,folio
       FROM   uni_unificador
       WHERE  estado           = vaceptado
       AND    ident_movimiento = "02"
       AND    cve_afo_recep    = vcodigo_afore
       AND    tipo_ent_nss     = "00"
       AND    estado_familia   = 1

    FOREACH cur_6 INTO nss_unifica1,vfolio

       DECLARE cur_7 CURSOR FOR

          SELECT b.nss_cta1,
                 b.rfc_cta1,
                 b.fecha_nac_cta1,
                 b.sexo_cta1,
                 b.ent_nac_cta1
          FROM   uni_unificado b
          WHERE  b.nss_uni      = nss_unifica1
          AND    b.folio        = vfolio
          AND    b.cve_ent_cta1 = vcodigo_afore
          AND    b.tipo_ent_cta1 IN ("01", "59")    #### Cambio de 01 incluye 59 8 Dic 2009 fallecidos
          AND    b.diag_unifica = "01"
          AND    b.estado       in(25,30,40)
          #### 8 Dic 2009  Cambio por fallecidos
         ORDER BY b.tipo_ent_cta1
      
       FOREACH cur_7 INTO g_reg7.*

           DECLARE cur_8 CURSOR FOR

               SELECT folio,
		      nss_uni,
                      curp_uni,
                      rfc_uni,
                      paterno_uni,
                      materno_uni,
                      nombre_uni,
                      nombre_imss_uni,
                      fecha_nac_uni,
                      sexo_uni,
                      ent_nac_uni,
                      " ",
                      " ",
                      cve_ent_nss
               FROM   uni_unificador
               #WHERE  folio          = vfolio
               WHERE  nss_uni        = nss_unifica1
               AND    estado         = vaceptado
               AND    estado_familia = 1
   
            FOREACH cur_8 INTO g_reg6.*

            #IF g_reg6.folio = vfolio THEN
                IF g_reg6.nombre_imss IS NOT NULL THEN
                    CALL separa_nombre(g_reg6.nombre_imss)
                    RETURNING xpaterno,xmaterno,xnombres
    
                    LET g_reg6.paterno = xpaterno
                    LET g_reg6.materno = xmaterno
                    LET g_reg6.nombre  = xnombres

                END IF

                IF (g_reg6.paterno IS NULL OR
                    g_reg6.paterno MATCHES " *") THEN
                    LET g_reg6.paterno = "N/A"
                END IF

                IF (g_reg6.materno IS NULL OR
                    g_reg6.materno MATCHES " *") THEN
                    LET g_reg6.materno = "N/A"
                END IF

                IF (g_reg6.nombre  IS NULL OR
                    g_reg6.nombre  MATCHES " *") THEN
                    LET g_reg6.nombre  = "N/A"
                END IF

                LET g_reg6.ind_cred_info = "0"

                SELECT n_unico,
                       n_rfc
                INTO   g_reg6.curp,
                       g_reg6.rfc
                FROM   afi_mae_afiliado
                WHERE  n_seguro = g_reg7.nss

                #IF STATUS <> NOTFOUND THEN
                IF (g_reg6.curp IS NULL OR
                    g_reg6.curp MATCHES " *") THEN
                    LET probatorio1 = ""
                ELSE
                    LET probatorio1 = "5"
                END IF

                IF g_reg6.ent_nac <> "35" THEN
                    LET g_reg6.nacionalidad = "MEX"
                END IF

                IF g_reg6.cve_afo_ced = "548" THEN
                   LET g_reg6.cve_afo_ced = "552"
                END IF
                IF g_reg6.cve_afo_ced = "514" THEN
                   LET g_reg6.cve_afo_ced = "538"
	        END IF

	        SELECT "X"
	        FROM   uni_det_certifica
	        WHERE  nss   = g_reg6.nss
	        GROUP BY 1

                IF STATUS = NOTFOUND  THEN
                
                    INSERT INTO uni_det_certifica VALUES(
                                                g_reg6.folio, #folio
                                                "00", #tipo_registro
                                                "", #cont_servicio
                                                "30", #cve_operacion
                                                g_reg6.nss,  #nss
                                                g_reg6.curp, #curp
                                                g_reg7.rfc,  #rfc
                                                g_reg6.paterno, #paterno
                                                g_reg6.materno, #materno
                                                g_reg6.nombre , #nombre
                                                g_reg7.fecha_nac, #fecha_nac
                                                g_reg7.sexo,
                                                g_reg7.ent_nac,
                                                "", #ind_cred_info
                                                g_reg6.nacionalidad,#nacional
                                                probatorio1, #cve_doc_proba
                                                g_reg6.cve_afo_ced,
                                                "",
                                                "",
                                                "10" #estado
                                                )

                    INSERT INTO uni_his_solicitud VALUES(
                                       g_reg6.folio, #folio
                                       "30", #cve_operacion
                                       g_reg6.nss,  #nss
                                       g_reg6.cve_afo_ced,
                                       HOY)

                END IF
{
           UPDATE uni_unificado
           SET    estado  = vsolicitado
           WHERE  folio   = g_reg6.folio
           AND    nss_uni = g_reg6.nss
           AND    cve_ent_cta1 = vcodigo_afore
           AND    diag_unifica = "01"
           AND    estado = vaceptado
}
           CALL actualiza_unificado(g_reg6.nss,g_reg6.folio)

{
           UPDATE uni_unificador
           SET    estado  = vsolicitado
           WHERE  folio   = g_reg6.folio
           AND    nss_uni = g_reg6.nss

           UPDATE uni_unificado
           SET    estado  = vsolicitado
           WHERE  folio   = g_reg6.folio
           AND    nss_uni = g_reg6.nss
}


       #END IF 
    END FOREACH
    EXIT  FOREACH   ### 8 Dic 2009 Fallecidos solo trate uno
    END FOREACH
    END FOREACH
END FUNCTION
#######################################################TRASPASO RECEPTORA
FUNCTION genera_encabezado()
#ge

    SELECT max(folio)
    INTO vfolio
    #FROM   uni_cza_notifica
    FROM   uni_unificado

    LET g_cza_uni_trasp.folio            = vfolio 
    LET g_cza_uni_trasp.tipo_registro    = "01"
    LET g_cza_uni_trasp.ident_servicio   = "01"
    LET g_cza_uni_trasp.ident_operacion  = "10"
    LET g_cza_uni_trasp.tipo_ent_origen  = "01"
    LET g_cza_uni_trasp.cve_ent_origen   = vcodigo_afore
    LET g_cza_uni_trasp.tipo_ent_destino = "03"
    LET g_cza_uni_trasp.cve_ent_destino  = "001"
    LET g_cza_uni_trasp.fecha_transfer   = vfecha #HOY USING "YYYYMMDD"
    LET g_cza_uni_trasp.ent_federativa   = vclave_entidad
    LET g_cza_uni_trasp.consec_lote      = vlote
    LET g_cza_uni_trasp.cve_mod_recep    = "02"
    LET g_cza_uni_trasp.estado           = 10

    SELECT "X"
    FROM   uni_cza_certifica
    WHERE  folio = vfolio

    IF STATUS = NOTFOUND THEN
        INSERT INTO uni_cza_certifica VALUES (g_cza_uni_trasp.*)
    END IF

END FUNCTION
#######################################################TRASPASO CEDENTE
FUNCTION genera_sumario()
#gs
   let g_sum_uni_trasp.folio           = vfolio
   let g_sum_uni_trasp.tipo_registro   = "09" 
   let g_sum_uni_trasp.tipo_ent_origen = "01"
   let g_sum_uni_trasp.cve_ent_origen  = vcodigo_afore      
   let g_sum_uni_trasp.fecha_transfer  = vfecha
   let g_sum_uni_trasp.consec_dia      = vlote  
   let g_sum_uni_trasp.id_servicio     = "01"
   let g_sum_uni_trasp.id_operacion    = "10"
   let g_sum_uni_trasp.total_entrada   = 0
   let g_sum_uni_trasp.estado          = 10

    SELECT "X"
    FROM   uni_sum_certifica
    WHERE  folio = vfolio
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        INSERT INTO uni_sum_certifica VALUES (g_sum_uni_trasp.*)
    END IF


END FUNCTION
 
FUNCTION segundo_paso()
#sp--------------------
    SELECT * 
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = "uni"

    LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,"DETUNI"
                  CLIPPED

    LET vgenera = 0

    SELECT "X"
    FROM   uni_det_certifica
    WHERE  estado = 10
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
        LET vgenera = 1
        DECLARE cur_3 CURSOR FOR
        SELECT *
        FROM   uni_det_certifica
        --WHERE  folio = vfolio
        WHERE  estado = 10

        START REPORT listado_3 TO G_LISTA
        LET CONT = 0
        FOREACH cur_3 INTO reg_3.*
            LET cont = cont + 1
            OUTPUT TO REPORT listado_3(reg_3.*) #l3
        END FOREACH
        FINISH REPORT listado_3

          LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED, "CZAUNI" 
                    CLIPPED

        DECLARE cur_4 CURSOR FOR
        SELECT *
        FROM   uni_cza_certifica
        WHERE  folio = vfolio

        START REPORT listado_4 TO G_LISTA 
        FOREACH cur_4 INTO reg_4.*
            OUTPUT TO REPORT listado_4(reg_4.*) #l4
        END FOREACH
        FINISH REPORT listado_4

          LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED, "SUMUNI" 
                    CLIPPED

        DECLARE cur_5 CURSOR FOR
        SELECT *
        FROM   uni_sum_certifica
        WHERE  folio = vfolio

        START REPORT listado_5 TO G_LISTA
        FOREACH cur_5 INTO reg_5.*
            OUTPUT TO REPORT listado_5(reg_5.*) #l5
        END FOREACH
        FINISH REPORT listado_5

        LET cat = "cat ",g_paramgrales.ruta_envio CLIPPED,"/CZAUNI ",
                     g_paramgrales.ruta_envio CLIPPED,"/DETUNI ",
                     g_paramgrales.ruta_envio CLIPPED,"/SUMUNI > ",
                     g_paramgrales.ruta_envio CLIPPED,"/",
                     HOY USING"YYYYMMDD",".30UN"
        RUN cat

        LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/CZAUNI "
        RUN borra
        LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/DETUNI "
        RUN borra
        LET borra = "rm ",g_paramgrales.ruta_envio CLIPPED,"/SUMUNI "
        RUN borra
    END IF

END FUNCTION

REPORT listado_3(reg_3)
#l3--------------------
    DEFINE reg_3 RECORD #glo #reg_3
        folio          INTEGER,
        tipo_registro  CHAR(2),
        cont_servicio  INTEGER,
        cve_operacion  CHAR(2),
        nss            CHAR(11),
        curp           CHAR(18),
        rfc            CHAR(13),
        paterno        CHAR(40),
        materno        CHAR(40),
        nombre         CHAR(40),
        fecha_nac      DATE,
        sexo           CHAR(1),
        ent_nac        CHAR(2),
        ind_cred_info  CHAR(1),
        nacionalidad   CHAR(3),
        cve_doc_proba  CHAR(1),
        cve_afo_ced    CHAR(3),
	fecha_certifica      DATE,
        estado         SMALLINT
    END RECORD
   
    OUTPUT
        PAGE LENGTH   1
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,"02",#tipo_registro
            COLUMN 003,cont USING"&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 013,"30",#cve_operacion
            COLUMN 015,reg_3.nss,
            COLUMN 026,reg_3.curp,
            COLUMN 044,reg_3.rfc,
            COLUMN 057,reg_3.paterno,
            COLUMN 097,reg_3.materno,
            COLUMN 137,reg_3.nombre,
            COLUMN 177,reg_3.fecha_nac  USING"YYYYMMDD"   ,
            COLUMN 185,"0000000000", #cve_promotor
            #COLUMN 202,"0000000000", #folio_solic
            COLUMN 213,reg_3.sexo,
            COLUMN 214,reg_3.ent_nac using "&&",
            COLUMN 216,"0",#ind_cred_info --verificar con miguel--
            COLUMN 217,reg_3.nacionalidad,#nacionalidad
            COLUMN 220,reg_3.cve_doc_proba,
            --COLUMN 251,reg_3.cve_afo_ced,
            COLUMN 262,309 SPACES
END REPORT

REPORT listado_4(reg_4)
#l4--------------------
        DEFINE reg_4 RECORD
            folio                INTEGER  ,
            tipo_registro        CHAR(02),
            ident_servicio       CHAR(02),
            ident_operacion      CHAR(02),
            tipo_ent_origen      CHAR(02),
            cve_ent_origen       CHAR(03),
            tipo_ent_destino     CHAR(02),
            cve_ent_destino      CHAR(03),
            fecha_transfer       DATE,
            ent_federativa       CHAR(03),
            consec_lote          SMALLINT,
            cve_mod_recep        CHAR(02),
            estado               SMALLINT
        END RECORD

    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,"01",#tipo_registro
            COLUMN 003,"01",#cont_servicio ,
            COLUMN 005,"10",#cve_operacion
            COLUMN 007,"01",
            COLUMN 009,vcodigo_afore USING "&&&",
            COLUMN 012,"03",
            COLUMN 014,"001",
            COLUMN 017,vfecha USING "YYYYMMDD",
            COLUMN 025,reg_4.ent_federativa,
            #COLUMN 028,vlote USING "&&&",
            COLUMN 028,"030",
            COLUMN 031,"02",  
            COLUMN 033, 538 SPACES

END REPORT

REPORT listado_5(reg_5)
#l5--------------------
        DEFINE reg_5 RECORD
            folio                INTEGER ,
            tipo_registro        CHAR(2),
            tipo_ent_origen      CHAR(2),
            cve_ent_origen       CHAR(3),
            fecha_transfer       DATE,
            consec_dia           INTEGER ,
            id_servicio          CHAR(2),
            id_operacion         CHAR(2),
            total_entrada        INTEGER,
            estado               SMALLINT
        END RECORD

    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,"09",#tipo_registro
            COLUMN 003,"01",#cont_servicio ,
            COLUMN 005,vcodigo_afore USING"&&&",
            COLUMN 008,vfecha USING "YYYYMMDD",
            #COLUMN 016,vlote USING "&&&",
            COLUMN 016,"030",
            COLUMN 019,"01",
            COLUMN 021,"10",#cve_operacion
            COLUMN 023,cont USING"&&&&&&&&&" ,#cont_servicio ,
            COLUMN 032,539 SPACES

END REPORT
FUNCTION separa_nombre(nombre_var)
   DEFINE
      xpaterno,
      xmaterno,
      xnombres  CHAR(40),
      nombre_var  CHAR(50),
      longitud  CHAR(150),
      i,algo,pos integer,
      opc CHAR(1),
      nom1 CHAR(40),
      nom2 CHAR(40)
      
     LET algo = 0
     LET pos = 0

     #LET longitud = LENGTH(vnombre)
     LET longitud = LENGTH(nombre_var)
     FOR i=1 TO longitud
        #IF vnombre[i] = "$" then
        IF nombre_var[i] = "$" then
           IF algo=0 then
              #LET xpaterno = vnombre[1,i-1]
              LET xpaterno = nombre_var[1,i-1]
              LET pos = i+1
              LET algo = 1
           ELSE
              #IF vnombre[i] ="$" AND vnombre[i-1]="$" THEN 
              IF nombre_var[i] ="$" AND nombre_var[i-1]="$" THEN 
                 LET xmaterno = ""
                 LET algo = 0
                 LET pos = i+1
              ELSE
                 #LET xmaterno = vnombre[pos,i-1]  
                 LET xmaterno = nombre_var[pos,i-1]  
                 LET algo = 0
                 LET pos = i+1
              END IF
           END IF
        END IF
     END FOR
     #LET nom1 = vnombre[pos]
     LET nom1 = nombre_var[pos]
     #LET nom2 = vnombre[i-1]
     LET nom2 = nombre_var[i-1]
     IF nom1 = " " THEN     
        LET i = i + 1       
     END IF                 

     #LET xnombres = vnombre[pos,i-1]
     LET xnombres = nombre_var[pos,i-1]
     RETURN xpaterno,xmaterno,xnombres
END FUNCTION
FUNCTION actualiza_unificado(g_cta1)
    DEFINE g_cta1 RECORD
        nss_uni   CHAR(11),
        folio     INTEGER
    END RECORD
    DEFINE  vsolicita SMALLINT
    DEFINE  vtipo_ent CHAR(02)

    LET  vsolicita = 0
    LET  vtipo_ent = ""

    SELECT "X"
    FROM   uni_unificador
    WHERE  folio   = g_cta1.folio
    AND    nss_uni = g_cta1.nss_uni
    AND    estado  = vaceptado
    AND    cve_ent_uni   = vcodigo_afore
    AND    cve_afo_recep = vcodigo_afore
    AND    tipo_ent_uni  = "01"
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
        SELECT "X"
        FROM   uni_unificado
        WHERE  nss_uni = g_cta1.nss_uni
        AND    folio   = g_cta1.folio
        AND    cve_ent_uni <> vcodigo_afore
        AND    estado  = vsolicitado
        GROUP BY 1

        IF STATUS = NOTFOUND THEN

            UPDATE uni_unificador
            SET    estado  = vsolicitado
            WHERE  folio   = g_cta1.folio
            AND    nss_uni = g_cta1.nss_uni
            AND    estado   = vaceptado

        END IF
    ELSE
{
        UPDATE uni_unificado
        SET    estado        = vsolicitado
        WHERE  folio         = g_cta1.folio
        AND    nss_uni       = g_cta1.nss_uni
        AND    cve_ent_cta1  = vcodigo_afore
        AND    tipo_ent_cta1 = "01"
        AND    estado        = vaceptado
}
        SELECT "X"
        FROM   uni_unificado
        WHERE  folio   = g_cta1.folio
        AND    nss_uni = g_cta1.nss_uni
        AND    estado  = vaceptado
        AND    cve_ent_cta1 <> vcodigo_afore
        GROUP BY 1

        IF STATUS = NOTFOUND THEN

            SELECT tipo_ent_nss
            INTO   vtipo_ent
            FROM   uni_unificador
            WHERE  folio   = g_cta1.folio
            AND    nss_uni = g_cta1.nss_uni
            AND    estado  = vaceptado
            #AND    cve_ent_nss  <> vcodigo_afore
            AND    cve_afo_recep = vcodigo_afore
            GROUP BY 1

            CASE vtipo_ent
                WHEN  "00"
                    SELECT "X"
                    FROM   uni_det_certifica
                    WHERE  nss = g_cta1.nss_uni
                    AND    folio = g_cta1.folio
                    GROUP BY 1

                    IF STATUS <> NOTFOUND THEN
                       LET vsolicita = 1
                    END IF

                WHEN  "01"
                    SELECT "X"
                    FROM   uni_det_traspaso
                    WHERE  nss = g_cta1.nss_uni
                    AND    folio = g_cta1.folio
                    GROUP BY 1
 
                    IF STATUS <> NOTFOUND THEN
                       LET vsolicita = 1
                    END IF

                WHEN  "59"
                    SELECT "X"
                    FROM   uni_det_asignado
                    WHERE  nss = g_cta1.nss_uni
                    AND    folio = g_cta1.folio
                    GROUP BY 1

                    IF STATUS <> NOTFOUND THEN
                       LET vsolicita = 1
                    END IF

            END CASE
            IF vsolicita = 1 THEN
                UPDATE uni_unificador
                SET    estado  = vsolicitado
                WHERE  folio   = g_cta1.folio
                AND    nss_uni = g_cta1.nss_uni
                AND    estado   = vaceptado

                UPDATE uni_unificado
                SET    estado        = vsolicitado
                WHERE  folio         = g_cta1.folio
                AND    nss_uni       = g_cta1.nss_uni
                AND    cve_ent_cta1  = vcodigo_afore
                AND    tipo_ent_cta1 = "01"
                AND    estado        = vaceptado
            END IF
            SELECT "X"
            FROM   uni_unificador
            WHERE  folio   = g_cta1.folio
            AND    nss_uni = g_cta1.nss_uni
            AND    estado  = vaceptado
            AND    cve_ent_nss  = vcodigo_afore
            AND    tipo_ent_nss  = "01"
            AND    cve_afo_recep = vcodigo_afore
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                UPDATE uni_unificador
                SET    estado  = vsolicitado
                WHERE  folio   = g_cta1.folio
                AND    nss_uni = g_cta1.nss_uni
                AND    cve_ent_nss  = vcodigo_afore
                AND    estado   = vaceptado

                UPDATE uni_unificado
                SET    estado        = vsolicitado
                WHERE  folio         = g_cta1.folio
                AND    nss_uni       = g_cta1.nss_uni
                AND    cve_ent_cta1  = vcodigo_afore
                AND    tipo_ent_cta1 = "01"
                AND    estado        = vaceptado
            END IF
        END IF
    END IF

END FUNCTION
FUNCTION actualiza_unificador(g_uni)
    DEFINE g_uni RECORD
        nss       CHAR(11),
        folio     INTEGER
    END RECORD
    DEFINE  vsolicita SMALLINT
    DEFINE  vtipo_ent CHAR(02)

    LET  vsolicita = 0
    LET  vtipo_ent = ""

    SELECT "X"
    FROM   uni_unificado
    WHERE  folio   = g_uni.folio
    AND    nss_uni = g_uni.nss
    AND    estado  = vaceptado
    AND    cve_ent_cta1 <> vcodigo_afore
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        SELECT tipo_ent_nss
        INTO   vtipo_ent
        FROM   uni_unificador
        WHERE  folio   = g_uni.folio
        AND    nss_uni = g_uni.nss
        AND    estado  = vaceptado
        #AND    cve_ent_nss  <> vcodigo_afore
        AND    cve_afo_recep = vcodigo_afore
        GROUP BY 1

        CASE vtipo_ent
            WHEN  "00"
                SELECT "X"
                FROM   uni_det_certifica
                WHERE  nss = g_uni.nss
                AND    folio = g_uni.folio
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                   LET vsolicita = 1
                END IF

            WHEN  "01"
                SELECT "X"
                FROM   uni_det_traspaso
                WHERE  nss = g_uni.nss
                AND    folio = g_uni.folio
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                   LET vsolicita = 1
                END IF

            WHEN  "59"
                SELECT "X"
                FROM   uni_det_asignado
                WHERE  nss = g_uni.nss
                AND    folio = g_uni.folio
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                   LET vsolicita = 1
                END IF
           END CASE
        IF vsolicita = 1 THEN
            UPDATE uni_unificador
            SET    estado  = vsolicitado
            WHERE  folio   = g_uni.folio
            AND    nss_uni = g_uni.nss
            AND    estado   = vaceptado

            UPDATE uni_unificado
            SET    estado  = vsolicitado
            WHERE  folio   = g_uni.folio
            AND    nss_uni = g_uni.nss
            AND    cve_ent_cta1 = vcodigo_afore
            AND    estado   = vaceptado
        END IF

        SELECT "X"
        FROM   uni_unificador
        WHERE  folio   = g_uni.folio
        AND    nss_uni = g_uni.nss
        AND    estado  = vaceptado
        AND    cve_ent_nss  = vcodigo_afore
        AND    tipo_ent_nss  = "01"
        AND    cve_afo_recep = vcodigo_afore
        GROUP BY 1

        IF STATUS <> NOTFOUND THEN
            UPDATE uni_unificador
            SET    estado  = vsolicitado
            WHERE  folio   = g_uni.folio
            AND    nss_uni = g_uni.nss
            AND    cve_ent_nss = vcodigo_afore
            AND    estado   = vaceptado

            UPDATE uni_unificado
            SET    estado  = vsolicitado
            WHERE  folio   = g_uni.folio
            AND    nss_uni = g_uni.nss
            AND    cve_ent_cta1 = vcodigo_afore
            AND    estado   = vaceptado
        END IF
     END IF
END FUNCTION
#############################################################################
FUNCTION miercoles_siguiente(diaActual)
   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE

   DEFINE diaHabilSig   DATE,
          diaSemana     SMALLINT,
          feriado       SMALLINT,
          finSemana     SMALLINT

   LET diaHabilSig = diaActual

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilSig)

      IF diaSemana = 3  THEN

         SELECT *
         FROM   tab_feriado
         WHERE  feria_fecha = diaHabilSig

         IF STATUS <> NOTFOUND THEN
            LET feriado = 1
         END IF

         IF feriado = 1 THEN
            LET diaHabilSig = diaHabilSig + 1 UNITS DAY
            EXIT WHILE
         ELSE
            EXIT WHILE
         END IF
      END IF

      LET diaHabilSig = diaHabilSig +1 UNITS DAY
   END WHILE

   RETURN diaHabilSig

END FUNCTION
