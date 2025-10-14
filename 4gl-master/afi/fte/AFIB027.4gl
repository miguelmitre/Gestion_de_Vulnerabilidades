#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIB027  => GENERACION ARCHIVO PLANO PARA PROCESAR (MODIFICADOS)  #
#Sistema           => AFI.                                                  #
#Autor             => Eduardo Joaquin esendiz Medina                        #
#Fecha             => 10 DE ABRIL DE 2006                                   #
#Actualizacion     => EDUARDO JOAQUIN RESENDIZ MEDINA                       #
#Fecha             => 24 JUNIO 2009 NO AILIADOS ISSSTE                      #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE
        generar     CHAR(1),
        enter       CHAR(1),
        g_opcion    CHAR(2),
        g_usuario   CHAR(8),
        HORA        CHAR(8),
        G_LISTA     CHAR(200),
        comm        CHAR(500),
        list_salida CHAR(500)

    DEFINE
        HOYDIA        ,
        fecha_hasta   ,
        HOY           DATE

    DEFINE
        bnd_proceso ,
        h_corr      ,
        h_corrd     ,
        num         SMALLINT

    DEFINE
        HAY_REGISTROS ,
        verror        ,
        vfol_prob     ,
        vcont         ,
        verror_or     INTEGER

    DEFINE g_reg RECORD
        n_seguro             CHAR(11),
        n_folio              DECIMAL(10,0),
        tipo_solicitud       SMALLINT,
        consecutivo          SMALLINT,
        folio_archivo        INTEGER,
        tipo_registro        CHAR(2),
        tipo_trabajador      CHAR(1),
        rfc_entidad          CHAR(12),
        nombre_entidad       CHAR(40),
        id_centro_pago       CHAR(7),
        cve_ramo             CHAR(5),
        cve_pagaduria        CHAR(5),
        rfc_ant              CHAR(13),
        curp_ant             CHAR(18),
        nsi_ant              CHAR(11),
        paterno_ant          CHAR(40),
        materno_ant          CHAR(40),
        nombres_ant          CHAR(40),
        rfc_modificado       CHAR(13),
        curp_modificado      CHAR(18),
        nsi_modificado       CHAR(11),
        paterno_modificado   CHAR(40),
        materno_modificado   CHAR(40),
        nombres_modificado   CHAR(40),
        fena_modificado      DATE,
        estadon_modificado   SMALLINT,
        sexo_modificado      SMALLINT,
        edo_civil_modificado SMALLINT,
        domicilio_modificado CHAR(60),
        colonia_modificado   CHAR(30),
        delega_modificado    CHAR(30),
        cpos_cod_modificado  CHAR(5),
        cod_result_op        CHAR(2),
        tipo_movimiento      CHAR(1),                 ---03082011
        motivo_rechazo1      CHAR(3),
        motivo_rechazo2      CHAR(3),
        motivo_rechazo3      CHAR(3),
        status_interno       SMALLINT,
        rechazo_interno      SMALLINT,
        usuario              CHAR(8),
        factualiza           DATE
    END RECORD

    DEFINE w_aux RECORD
        n_unico          LIKE afi_mae_afiliado.n_unico,
        n_rfc            LIKE afi_mae_afiliado.n_rfc,
        paterno          LIKE afi_mae_afiliado.paterno,
        materno          LIKE afi_mae_afiliado.materno,
        nombres          LIKE afi_mae_afiliado.nombres,
        fena             LIKE afi_mae_afiliado.fena,          ---op53-54
        estadon          LIKE afi_mae_afiliado.estadon,       ---op53-54
        sexo             LIKE afi_mae_afiliado.sexo,          ---op53-54
        edocivil         LIKE afi_mae_afiliado.edo_civil      ---op53-54
        {cod_res_op       CHAR(02),
        diag_proceso     CHAR(15),
        femision_cert    DATE,
        rfc_bd           CHAR(13),
        fec_primer_reg   DATE,
        fec_alta_afor_act DATE,
        cve_afore_reg    CHAR(03),
        fec_recep        DATE,
        hora_recep       DATETIME HOUR TO SECOND,
        consec_rec       INTEGER}
    END RECORD

    DEFINE w_ant RECORD
        n_seguro         LIKE afi_mae_afiliado.n_seguro,        ---op53-54
        n_unico          LIKE afi_mae_afiliado.n_unico,
        n_rfc            LIKE afi_mae_afiliado.n_rfc,
        paterno          LIKE afi_mae_afiliado.paterno,
        materno          LIKE afi_mae_afiliado.materno,
        nombres          LIKE afi_mae_afiliado.nombres
        {cod_res_op       CHAR(02),
        diag_proceso     CHAR(15),
        femision_cert    DATE,
        rfc_bd           CHAR(13),
        fec_primer_reg   DATE,
        fec_alta_afor_act DATE,
        cve_afore_reg    CHAR(03),
        fec_recep        DATE,
        hora_recep       DATETIME HOUR TO SECOND,
        consec_rec       INTEGER}
    END RECORD

    DEFINE k_aux RECORD
        tipo_reg         CHAR(02),
        con_dservicio    CHAR(10),
        clave_doperacion CHAR(02),
        fech_recep_afor  CHAR(08),
        folio_solicitud  CHAR(10)
    END RECORD

    DEFINE g_paramgrales RECORD LIKE seg_modulo.*
    DEFINE g_afore       RECORD LIKE tab_afore_local.*
    DEFINE x_lotes       RECORD LIKE tab_lote.*

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE reg_afi_mae RECORD LIKE afi_mae_afiliado.*

    DEFINE a_arr ARRAY[10] OF RECORD
           marca CHAR(1),
           conta SMALLINT
           END RECORD

    DEFINE b_arr ARRAY[10] OF RECORD
           marca CHAR(1),
           conta SMALLINT
           END RECORD

    DEFINE
        bnd_mod   ,
        i         ,
        j         ,
        sumatoria ,
        paso      ,
        paso1     ,
        pasa_dat  ,
        tod_id    SMALLINT

    DEFINE
        cont_reg  INTEGER

    DEFINE
        a_marca   CHAR(1),
        b_marca   CHAR(1),
        consec    CHAR(10),
        consecd   CHAR(10)

    DEFINE vnsi             LIKE cta_ctr_reg_ind.nss_issste  ---op53-54
    DEFINE vnti             LIKE cta_ctr_reg_ind.nti         ---op53-54
    DEFINE ban_54           SMALLINT                         ---op53-54
    DEFINE vid_centro_pago  CHAR(7)                          ---op53-54
    DEFINE st_afore         CHAR(02)                         ---op53-54
    DEFINE vstatus_int_op54 SMALLINT                         ---op53-54
    DEFINE vanio            CHAR(4)                          ---op53-54
    DEFINE vmes             CHAR(2)                          ---op53-54
    DEFINE vdia             CHAR(2)                          ---op53-54
    DEFINE vfena            CHAR(8)                          ---op53-54
    DEFINE vsexo            CHAR(1)                          ---op53-54
    DEFINE vedo_civil       CHAR(1)                          ---op53-54
    DEFINE cont_op54        SMALLINT                         ---op53_54
    DEFINE cont_mod_normal  SMALLINT                         ---op53_54
    DEFINE vfena_tot        CHAR(10)                         ---op53-54
    DEFINE aceptados        SMALLINT
    DEFINE rechazados       SMALLINT

    DEFINE vrfc             CHAR(13)

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('AFIB027.log')
    CALL inicio()     #i
    CALL inicializa() #iz

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I

        CALL proceso_principal()   #pp
    ELSE
        #CALL fecha_max()
        CALL rescata_valores()     #rv
        CALL actualiza_operacion() #ao
    END IF

END MAIN

FUNCTION inicio()
#i---------------

    LET reg_bat.pid          = ARG_VAL(1)
    LET reg_bat.proceso_cod  = ARG_VAL(2)
    LET reg_bat.opera_cod    = ARG_VAL(3)

    INITIALIZE reg_afi_mae.* TO NULL

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF

    LET HOYDIA   = TODAY
    LET HOY      = TODAY
    LET HORA     = TIME
    LET num      = 1
    LET g_opcion = '02'
    LET k_aux.tipo_reg         = "03"  
    LET k_aux.con_dservicio    = "          "
    LET k_aux.clave_doperacion = "53" 
    LET k_aux.fech_recep_afor  = "       "
    LET k_aux.folio_solicitud  = "          "

    SELECT *
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    LET a_arr[1].marca = 'A' LET a_arr[2].marca = 'B' LET a_arr[3].marca = 'C'
    LET a_arr[4].marca = 'D' LET a_arr[5].marca = 'E' LET a_arr[6].marca = 'F'
    LET a_arr[7].marca = 'G' LET a_arr[8].marca = 'H' LET a_arr[9].marca = 'I'
    LET a_arr[10].marca = 'J'

    LET a_arr[1].conta  = 512
    LET a_arr[2].conta  = 256
    LET a_arr[3].conta  = 128
    LET a_arr[4].conta  = 64
    LET a_arr[5].conta  = 32
    LET a_arr[6].conta  = 16
    LET a_arr[7].conta  = 8
    LET a_arr[8].conta  = 4
    LET a_arr[9].conta  = 2
    LET a_arr[10].conta = 1

    FOR i = 1 TO 10
        LET b_arr[i].* = a_arr[i].*
    END FOR

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE modifica_op53
    WHENEVER ERROR STOP

    CREATE TABLE modifica_op53
        (tipo_registro CHAR(02),
         contador      SMALLINT,
         clave_oper    CHAR(02),
         id_cent_pago  CHAR(07),
         rfc           CHAR(13),
         curp_ant      CHAR(18),
         curp_nueva    CHAR(18),
         nsi           CHAR(11),
         paterno       CHAR(40),
         materno       CHAR(40),
         nombres       CHAR(40),
         fena          CHAR(08),
         estadon       SMALLINT,
         sexo          CHAR(01),
         edo_civil     CHAR(01),
         status_afore  CHAR(02),
         tipo_trabajador CHAR(1))                    --03082011

    DATABASE safre_af

END FUNCTION

FUNCTION inicializa()
#iz--------------

    LET i         = 0
    LET j         = 0
    LET paso      = 0
    LET paso1     = 0
    LET tod_id    = 0
    LET bnd_mod   = 0
    LET pasa_dat  = 0
    LET cont_reg  = 0
    LET sumatoria = 0
    LET a_marca   = NULL
    LET b_marca   = NULL
    LET vanio     = NULL
    LET vmes      = NULL
    LET vdia      = NULL
    LET vfena     = NULL
    LET vsexo     = NULL
    LET vedo_civil= NULL

    INITIALIZE w_aux.*       TO NULL
    INITIALIZE w_ant.*       TO NULL
    INITIALIZE reg_afi_mae.* TO NULL
    INITIALIZE g_reg.*       TO NULL
    

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIB0081" ATTRIBUTE(BORDER)
    DISPLAY " AFIB027    GENERACION DE ARCHIVO PROCESAR (MODIF NO AFIL)                     " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                                 < Ctrl-C > Salir                           " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)


    DISPLAY g_paramgrales.ruta_envio AT 10,10

    INPUT BY NAME generar

    AFTER FIELD generar
        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        IF generar  MATCHES "[Ss]" THEN
            #CALL fecha_max()
            CALL rescata_valores()
            CALL genera_listado()

            IF NOT HAY_REGISTROS THEN
                ERROR "NO HAY REGISTROS PARA PROCESAR"
                SLEEP 3
                EXIT PROGRAM
            END IF

            EXIT INPUT
        ELSE
            ERROR "PROCESO CANCELADO" SLEEP 2
            EXIT PROGRAM
        END IF

        ON KEY ( INTERRUPT )
            ERROR "PROCESO CANCELADO" SLEEP 2
            EXIT PROGRAM

    END INPUT

    PROMPT "NOMBRE ARCHIVO PLANO : ", comm CLIPPED,
           " /[Enter] para salir" FOR enter

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    DEFINE
        contador   INTEGER,
        ban_rfc    SMALLINT,
        ban_unico  SMALLINT,
        vfol_sol   DECIMAL(8,0),
        cuenta_reg SMALLINT

    LET contador   = 0 
    LET ban_rfc    = 0
    LET ban_unico  = 0
    LET cuenta_reg = 0
    LET aceptados  = 0
    LET rechazados = 0

    LET HAY_REGISTROS = FALSE

    SELECT COUNT(*)
    INTO cont_op54
    FROM afi_ctr_det_op54
    WHERE status_interno IN (120,121)

    SELECT COUNT(*)
    INTO   cont_mod_normal
    FROM   afi_mae_afiliado a, afi_mae_modifica b
    WHERE  a.status_interno in(120,160)
    --AND    a.n_unico        = b.n_unico
    AND    a.n_seguro       = b.n_seguro
    AND    a.n_folio        = b.n_folio
    AND    a.tipo_solicitud IN (8,12)
    AND    a.tipo_solicitud = b.tipo_solicitud
    AND    b.cod_operacion  = 0
    AND    b.status_interno = 120


    IF cont_op54 IS NULL THEN
      LET cont_op54 = 0
    END IF

    IF cont_mod_normal IS NULL THEN
      LET cont_mod_normal = 0
    END IF

    LET HAY_REGISTROS = cont_op54 + cont_mod_normal
    LET contador = HAY_REGISTROS

    IF NOT bnd_proceso THEN
        DISPLAY "REGISTROS A SER ENVIADOS ", contador AT 16,20
        SLEEP 3
    ELSE
        DISPLAY "REGISTROS A SER ENVIADOS ", contador
    END IF 

    IF HAY_REGISTROS THEN
        SELECT *
        INTO   x_lotes.*
        FROM   tab_lote
        WHERE  lotes_cod   = 53
        AND    lotes_fecha = HOYDIA

        #IF SQLCA.SQLCODE = 0 THEN
        IF STATUS <> NOTFOUND THEN
            UPDATE tab_lote
            SET    lotes_correlativo = lotes_correlativo + 1
            WHERE  lotes_cod         = 53
            AND    lotes_fecha       = HOYDIA
        ELSE
            INSERT INTO tab_lote
            VALUES(HOYDIA     ,
                   53          ,
                   'MODIFICACION NO AFIL',
                   1          ,
                   1)
        END IF

        SELECT @lotes_correlativo
        INTO   h_corr
        FROM   tab_lote
        WHERE  lotes_cod   = 53
        AND    lotes_fecha = HOYDIA

        LET consec = h_corr
{
        UPDATE tab_lote
        SET    lotes_correlativo = lotes_correlativo + 1
        WHERE  lotes_cod   = 53
        AND    lotes_fecha = HOYDIA
}
        SELECT l.lotes_correlativo
        INTO   h_corrd
        FROM   tab_lote l
        WHERE  l.lotes_cod   = 53
        AND    l.lotes_fecha = HOYDIA

        LET consecd = h_corrd

        SELECT *,USER
        INTO   g_afore.*,g_usuario
        FROM tab_afore_local

        DECLARE curs_1 CURSOR FOR
        SELECT *
        FROM   afi_ctr_det_op54
        WHERE  status_interno IN (120,121)


        FOREACH curs_1 INTO g_reg.*            ---op53-54

        LET cuenta_reg = cuenta_reg + 1


            LET HAY_REGISTROS         = TRUE

            INITIALIZE st_afore TO NULL

            IF g_reg.status_interno = 121 THEN
                LET st_afore = '02'
                LET rechazados = rechazados + 1
            ELSE
                IF g_reg.status_interno = 120 THEN
                    LET st_afore = '01'
                    LET aceptados = aceptados + 1
                END IF
            END IF

            #### Modificacion #### 
            --DECLARE c_2 CURSOR FOR
             SELECT *
             INTO   reg_afi_mae.*
               FROM afi_mae_afiliado a
              --WHERE @n_seguro = w_aux.n_seguro   ----CURP
              --WHERE @n_unico = w_aux.n_unico
              --WHERE a.n_unico        = g_reg.curp_ant
              WHERE a.n_seguro       = g_reg.n_seguro
              AND   a.tipo_solicitud = 8
              AND   a.n_folio        = g_reg.n_folio
              --AND   a.n_seguro       = g_reg.n_seguro

            --FOREACH c_2 INTO reg_afi_mae.* 

            --END FOREACH           ---op53-54

                --- Elimina espacios
                LET reg_afi_mae.paterno      = reg_afi_mae.paterno CLIPPED
                LET g_reg.paterno_modificado = g_reg.paterno_modificado CLIPPED   ---op53-54

                LET reg_afi_mae.materno      = reg_afi_mae.materno CLIPPED
                LET g_reg.materno_modificado = g_reg.materno_modificado CLIPPED   ---op53-54

                LET reg_afi_mae.nombres      = reg_afi_mae.nombres CLIPPED
                LET g_reg.nombres_modificado = g_reg.nombres_modificado CLIPPED   ---op53-54

                LET reg_afi_mae.n_rfc        = reg_afi_mae.n_rfc   CLIPPED
                LET g_reg.rfc_modificado     = g_reg.rfc_modificado     CLIPPED   ---op53-54
                LET g_reg.rfc_ant            = g_reg.rfc_ant CLIPPED

                IF g_reg.materno_modificado IS NULL OR
                   g_reg.materno_modificado[1] = " " THEN
                    --LET w_aux.materno = "N/A"
                    LET g_reg.materno_modificado = "N/A"
                END IF

                LET vfena_tot = g_reg.fena_modificado 
                LET vanio     = vfena_tot[7,10]
                LET vmes      = vfena_tot[1,2]
                LET vdia      = vfena_tot[4,5]
                LET vfena     = vanio, vmes, vdia

                IF g_reg.sexo_modificado = 1 THEN
                   LET vsexo = 'M'
                ELSE
                   IF g_reg.sexo_modificado = 2 THEN
                      LET vsexo = 'F'
                   END IF
                END IF

                IF g_reg.edo_civil_modificado = 1 THEN
                  LET vedo_civil = 0
                ELSE
                  IF g_reg.edo_civil_modificado = 2 THEN
                     LET vedo_civil = 1
                  END IF
                END IF

                IF g_reg.nsi_ant IS NULL OR
                   g_reg.nsi_ant MATCHES ' *' THEN
                     LET g_reg.nsi_ant = '00000000000'
                END IF

                INITIALIZE vrfc TO NULL

                IF g_reg.status_interno = 121 THEN
                   LET vrfc = g_reg.rfc_ant
                ELSE
                    IF g_reg.status_interno = 120 THEN
                       LET vrfc = g_reg.rfc_modificado
                    END IF
                END IF

                INSERT INTO safre_tmp:modifica_op53
                VALUES (k_aux.tipo_reg,
                        cuenta_reg,
                        k_aux.clave_doperacion,
                        g_reg.id_centro_pago,
                        vrfc,
                        g_reg.curp_ant,
                        g_reg.curp_modificado,
                        g_reg.nsi_ant,
                        g_reg.paterno_modificado,
                        g_reg.materno_modificado,
                        g_reg.nombres_modificado,
                        vfena,
                        g_reg.estadon_modificado,
                        vsexo,
                        vedo_civil,
                        st_afore,
                        g_reg.tipo_trabajador)              ---03082011

                CALL inicializa()

                LET ban_unico = 0

        END FOREACH 
        #### Modificacion #### 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   
       LET consecd = h_corrd

       DECLARE curs_2 CURSOR FOR
       SELECT 
              b.n_unico,
              b.n_rfc,
              b.paterno,
              b.materno,
              b.nombres,
              b.fena,                                      ---op53-54
              b.estadon,                                   ---op53-54
              b.sexo,                                      ---op53-54
              b.edo_civil,                                 ---op53-54
              a.n_unico,                                   ---op53-54
              a.n_seguro,                                  ---op53-54
              b.status_interno                             ---op53-54
       FROM   afi_mae_afiliado a, afi_mae_modifica b
       WHERE  a.status_interno IN(120,160) # STATUS MODIFICADOS
       AND    a.n_seguro       = b.n_seguro   ----CURP   ---op53-54 (se descomento)
       --AND    a.n_unico        = b.n_unico               ---op53-54
       AND    a.n_folio        = b.n_folio
       AND    a.tipo_solicitud IN (8,12)
       AND    a.tipo_solicitud = b.tipo_solicitud
       AND    b.cod_operacion  = 0
       AND    b.status_interno = 120

       FOREACH curs_2 INTO w_aux.*, 
                           w_ant.n_unico,w_ant.n_seguro,vstatus_int_op54

           LET HAY_REGISTROS         = TRUE

           SELECT c.nss_issste,c.nti
           INTO   vnsi,vnti
           FROM   cta_ctr_reg_ind c
           WHERE  c.curp  = w_ant.n_unico
           --AND    @nti      = w_ant.n_seguro

           IF w_aux.materno IS NULL OR
              w_aux.materno MATCHES ' *' THEN
               LET w_aux.materno = "N/A"
           END IF
   
           #### Modificacion #### 
           DECLARE c_1 CURSOR FOR
            SELECT *
            INTO   reg_afi_mae.*
              FROM afi_mae_afiliado a
             --WHERE @n_seguro = w_aux.n_seguro   ----CURP
             WHERE a.n_unico        = w_ant.n_unico
             AND   a.tipo_solicitud = 8
             AND   a.n_seguro       = w_ant.n_seguro
   
           FOREACH c_1 INTO reg_afi_mae.* 
   
               --- Elimina espacios
               LET reg_afi_mae.paterno      = reg_afi_mae.paterno CLIPPED
               LET w_aux.paterno            = w_aux.paterno       CLIPPED
               --LET w_ant.paterno            = w_ant.paterno       CLIPPED  ---op53-54
   
               LET reg_afi_mae.materno      = reg_afi_mae.materno CLIPPED
               LET w_aux.materno            = w_aux.materno       CLIPPED
               --LET w_ant.materno            = w_ant.materno       CLIPPED  ---op53-54
   
               LET reg_afi_mae.nombres      = reg_afi_mae.nombres CLIPPED
               LET w_aux.nombres            = w_aux.nombres       CLIPPED
               --LET w_ant.nombres            = w_ant.nombres       CLIPPED  ---op53-54
   
               LET reg_afi_mae.n_rfc        = reg_afi_mae.n_rfc   CLIPPED
               LET w_aux.n_rfc              = w_aux.n_rfc         CLIPPED
               --LET w_ant.n_rfc              = w_ant.n_rfc         CLIPPED  --op53-54
   
               --- Valida materno en caso de que sea nulo
               IF w_aux.materno IS NULL OR
                  w_aux.materno[1] = " " THEN
                   LET w_aux.materno = "N/A"
               END IF

                IF vstatus_int_op54 = 120 THEN
                    LET st_afore = '01'
                END IF

                LET g_reg.id_centro_pago = '0000000'

                LET vfena_tot = w_aux.fena 
                LET vanio     = vfena_tot[7,10]
                LET vmes      = vfena_tot[1,2]
                LET vdia      = vfena_tot[4,5]
                LET vfena     = vanio, vmes, vdia

                LET cuenta_reg = cuenta_reg + 1

                IF w_aux.sexo = 1 THEN
                   LET vsexo = 'M'
                ELSE
                   IF w_aux.sexo = 2 THEN
                      LET vsexo = 'F'
                   END IF
                END IF

                IF w_aux.edocivil  = 1 THEN
                  LET vedo_civil = 0
                ELSE
                  IF w_aux.edocivil = 2 THEN
                     LET vedo_civil = 1
                  END IF
                END IF

                INSERT INTO safre_tmp:modifica_op53
                VALUES (k_aux.tipo_reg,
                        cuenta_reg,
                        k_aux.clave_doperacion,
                        g_reg.id_centro_pago,
                        reg_afi_mae.n_rfc,
                        w_ant.n_unico,
                        w_aux.n_unico,
                        vnsi,
                        w_aux.paterno,
                        w_aux.materno,
                        w_aux.nombres,
                        vfena,
                        w_aux.estadon,
                        vsexo,
                        vedo_civil,
                        st_afore,
                        g_reg.tipo_trabajador)              ---03082011

               LET vcont = vcont + 1
               LET aceptados = aceptados + 1

               CALL inicializa()

               LET ban_unico = 0

           END FOREACH

       END FOREACH 
   
#<<<    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    END IF

    IF NOT HAY_REGISTROS THEN
        DISPLAY "Program stopped, NO HAY REGISTROS PARA PROCESAR..."
        EXIT PROGRAM
    END IF

END FUNCTION

FUNCTION genera_listado()

 DEFINE g_reg_mod RECORD
         tipo_registro CHAR(02),
         contador      SMALLINT,
         clave_oper    CHAR(02),
         id_cent_pago  CHAR(07),
         rfc           CHAR(13),
         curp_ant      CHAR(18),
         curp_nueva    CHAR(18),
         nsi           CHAR(11),
         paterno       CHAR(40),
         materno       CHAR(40),
         nombres       CHAR(40),
         fena          CHAR(08),
         estadon       SMALLINT,
         sexo          CHAR(01),
         edo_civil     CHAR(01),
         status_afore  CHAR(02),
         tipo_trabajador CHAR(1)              --03082011
 END RECORD 

 DEFINE contador SMALLINT

        INITIALIZE g_reg_mod.* TO NULL
        LET contador = 0

        LET comm = g_paramgrales.ruta_envio CLIPPED,"/E",
                   HOYDIA USING "yyyymmdd" CLIPPED,
                   consec CLIPPED, ".MNA" CLIPPED

        DISPLAY "Archivo: ", comm CLIPPED

        LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",
                      g_usuario CLIPPED, ".MODIFIND." CLIPPED,
                      HOY USING "dd-mm-yyyy","_", consec CLIPPED,
                      HORA CLIPPED

        START REPORT listado_2 TO G_LISTA
        START REPORT listado TO comm

        DECLARE cur_3 CURSOR FOR
        SELECT *
        FROM safre_tmp:modifica_op53

        FOREACH cur_3 INTO g_reg_mod.*

                OUTPUT TO REPORT listado(g_reg_mod.*)              ---op53-54


                OUTPUT TO REPORT listado_2(k_aux.tipo_reg,
                                           k_aux.con_dservicio,
                                           k_aux.clave_doperacion,
                                           g_reg_mod.curp_ant,
                                           g_reg_mod.rfc,                                                                
                                           g_reg_mod.paterno,
                                           g_reg_mod.materno,
                                           g_reg_mod.nombres)

                LET vcont = vcont + 1

                SELECT 'X'
                FROM afi_mae_afiliado
                WHERE  n_unico        = g_reg_mod.curp_ant
                AND    status_interno = 120
                AND    tipo_solicitud = 8

                IF SQLCA.SQLCODE = 0 THEN
                   UPDATE afi_mae_afiliado
                   SET    status_interno = 130,
                          status_captura = 130,
                          lote           = h_corr,
                          fecha_envio    = HOYDIA
                   --WHERE  n_seguro       = w_aux.n_seguro
                   WHERE  n_unico        = g_reg_mod.curp_ant
                   AND    status_interno = 120
                   AND    tipo_solicitud = 8
                ELSE
                   UPDATE afi_mae_afiliado
                   SET    status_interno = 130,
                          status_captura = 130,
                          lote           = h_corr,
                          fecha_envio    = HOYDIA
                   --WHERE  n_seguro       = w_aux.n_seguro
                   WHERE  n_unico        = g_reg_mod.curp_nueva
                   AND    status_interno = 120
                   AND    tipo_solicitud = 8
                END IF

                UPDATE afi_mae_modifica
                SET    status_interno = 130
                --WHERE  n_seguro       = w_aux.n_seguro
                WHERE  n_unico        = g_reg_mod.curp_nueva
                AND    status_interno = 120
                AND    cod_operacion  = 0
                AND    diag_proceso   = '0'
                AND    tipo_solicitud = 8

                UPDATE afi_ctr_det_op54
                SET    status_interno = 130
                WHERE  curp_modificado= g_reg_mod.curp_nueva
                AND    status_interno = 120

                UPDATE afi_ctr_det_op54
                SET    status_interno = 131
                --WHERE  n_seguro       = w_aux.n_seguro
                WHERE  curp_modificado= g_reg_mod.curp_nueva
                AND    status_interno = 121

                UPDATE afi_ctr_cza_op54
                SET    status_interno = 130
                WHERE  status_interno = 120

                UPDATE afi_ctr_curp
                SET    fecha_envio    = HOYDIA
                WHERE  nss            = g_reg_mod.curp_nueva 
                AND    cve_operacion  = "53"
                AND    fecha_envio   IS NULL

                LET contador = contador + 1

                IF vcont > 9999 THEN
                    EXIT FOREACH
                END IF

                INITIALIZE g_reg_mod.* TO NULL

        END FOREACH

        FINISH REPORT listado
        FINISH REPORT listado_2

        CALL limpia_nulos()        

        DISPLAY "REGISTROS A SER ENVIADOS ", contador AT 16,20

        LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                      g_usuario CLIPPED,".MODIFIND." 
                      CLIPPED,HOY USING "dd-mm-yyyy","_", consec CLIPPED,
                      HORA CLIPPED
        RUN G_LISTA

END FUNCTION 

REPORT listado(g_lst_mod)
#--------------------
-----------DE ACUERDO AL LAY OUT
 DEFINE g_lst_mod RECORD
         tipo_registro CHAR(02),
         contador      SMALLINT,
         clave_oper    CHAR(02),
         id_cent_pago  CHAR(07),
         rfc           CHAR(13),
         curp_ant      CHAR(18),
         curp_nueva    CHAR(18),
         nsi           CHAR(11),
         paterno       CHAR(40),
         materno       CHAR(40),
         nombres       CHAR(40),
         fena          CHAR(08),
         estadon       SMALLINT,
         sexo          CHAR(01),
         edo_civil     CHAR(01),
         status_afore  CHAR(02),
         tipo_trabajador CHAR(1)                --03082011
 END RECORD 

    DEFINE 
        nombre_comp  ,
        nombre_comp2 CHAR(120)

    DEFINE
        d CHAR(02),
        m CHAR(02),
        a CHAR(02)

    DEFINE
        dia    CHAR(02),
        mes    CHAR(02),
        ano    CHAR(04),
        dia1   CHAR(02),
        mes1   CHAR(02),
        ano1   CHAR(04)

    DEFINE 
        hoy_env    CHAR(8),
        num10      CHAR(10),
        tot_char   CHAR(9)

    DEFINE x_afore RECORD LIKE tab_afore_local.*

    DEFINE 
        tot ,
        var SMALLINT

    DEFINE vfena CHAR(8)           ---op53-54
    DEFINE vdatpat CHAR(7)         ---op53-54

    OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 60

    FORMAT
    FIRST PAGE HEADER
        LET hoy_env = YEAR(HOYDIA) USING "&&&&",
                      MONTH(HOYDIA)USING "&&"  ,
                      DAY(HOYDIA)  USING "&&"

        SELECT * 
        INTO   x_afore.*
        FROM   tab_afore_local

        IF g_opcion IS NULL THEN
            LET g_opcion= '02'
        END IF

        PRINT COLUMN 001,"01",
                         "01",
                         "53",
                         "01",
                         x_afore.codigo_afore USING "&&&",
                         "03",
                         "001",
                         hoy_env,
--                         "001 ",
                         h_corr USING "&&&",
--                         272 SPACES
                         423 SPACES      ---op53-54

        LET verror = 00000512

    ON EVERY ROW
{        LET a     = YEAR (w_aux.frecafor)  using "&&&&"
        LET m     = MONTH (w_aux.frecafor) using "&&"
        LET d     = DAY (w_aux.frecafor)   using "&&"
        LET dia   = DAY(w_aux.fena)        USING"&&"
        LET mes   = MONTH(w_aux.fena)      USING"&&"
        LET ano   = YEAR(w_aux.fena)       USING "&&&&"}
        LET dia1  = DAY(TODAY)             USING"&&"
        LET mes1  = MONTH(TODAY)           USING"&&"
        LET ano1  = YEAR(TODAY)            USING "&&&&"
        LET num10 = "          "
        LET num10 = num                    USING "&&&&&&&&&&"
        LET verror_or = 0

        IF g_lst_mod.id_cent_pago IS NULL OR
           g_lst_mod.id_cent_pago MATCHES "[ *]" THEN
            LET g_lst_mod.id_cent_pago = '0000000'
        END IF

        IF g_lst_mod.nsi IS NULL OR
           g_lst_mod.nsi MATCHES "[ *]" THEN
            LET g_lst_mod.nsi = '00000000000'
        END IF

        PRINT
            COLUMN 1,"03",
                     num10,
                     "53",
                     g_lst_mod.id_cent_pago USING "&&&&&&&",
                     g_lst_mod.rfc        ,
                     g_lst_mod.curp_ant,
                     g_lst_mod.curp_nueva,
                     g_lst_mod.nsi,
                     g_lst_mod.paterno,
                     g_lst_mod.materno,
                     g_lst_mod.nombres,
                     g_lst_mod.fena,
                     g_lst_mod.estadon USING "&&",
                     g_lst_mod.sexo,
                     g_lst_mod.edo_civil,
                     g_lst_mod.status_afore,
                     99 SPACES,
                     g_lst_mod.tipo_trabajador,
                     135 SPACES


        LET num = num + 1

    ON LAST ROW
        LET tot = 0
        LET tot = count(*)
        LET tot_char = tot USING "&&&&&&&&&"
        

        PRINT COLUMN 001,"09",
                          aceptados USING "&&&&&&&&&",
                          rechazados USING "&&&&&&&&&",
                          tot_char,
                          9 SPACES,
                          "000000000",
                          403 SPACES

END REPORT

REPORT listado_2(w_aux)
#--------------------

---ADECUARLO

    DEFINE w_aux  RECORD
        tipo_reg         CHAR(02),
        con_dservicio    CHAR(10),
        clave_doperacion CHAR(02),
        n_unico          LIKE afi_mae_afiliado.n_unico,
        n_rfc            LIKE afi_mae_afiliado.n_rfc,
        paterno          CHAR(40),
        materno          CHAR(40),
        nombres          CHAR(40)
    END RECORD

    DEFINE
        nombre_comp,
        nombre_comp2 CHAR(120)

    DEFINE
        d CHAR(02),
        m CHAR(02),
        a CHAR(04)

    DEFINE
        dia      CHAR(02),
        mes      CHAR(02),
        ano      CHAR(04),
        dia1     CHAR(02),
        mes1     CHAR(02),
        ano1     CHAR(04),
        hoy      CHAR(8),
        num10    CHAR(10),
        tot_char CHAR(9)

    DEFINE x_afore RECORD LIKE tab_afore_local.*

    DEFINE
        tot SMALLINT

    OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 66

    FORMAT
    PAGE HEADER
        LET hoy = YEAR(TODAY) USING "&&&&",
                  MONTH(TODAY)USING "&&"  ,
                  DAY(TODAY)  USING "&&"

        SELECT * 
        INTO   x_afore.*
        FROM   tab_afore_local

       PRINT
           COLUMN 01,"=================================================",
           COLUMN 50,"=============================="
       PRINT
           COLUMN 01,x_afore.razon_social  CLIPPED,
           COLUMN 35,"LISTA DE MODIFICADOS NO AFIL.",
           COLUMN 60,"FECHA     :",TODAY     USING "DD-MM-YYYY"
       PRINT
           COLUMN 01,"AFIB027",
           COLUMN 35,"ENVIADOS  A PROCESAR",
           COLUMN 60,"Nro.PAGINA:",PAGENO    USING "##########"
       PRINT
           COLUMN 01,"=================================================",
           COLUMN 50,"=============================="

       PRINT 
           COLUMN 01,"CURP",
           COLUMN 19,"RFC",
           COLUMN 38,"A.PATERNO",
           COLUMN 54,"A.MATERNO"

       PRINT 
           COLUMN 01,"NOMBRES"

       PRINT
           COLUMN 01,"-------------------------------------------------",
           COLUMN 50,"------------------------------"
        
                 
    ON EVERY ROW
{        LET a     = YEAR (w_aux.frecafor)  using "&&&&"
        LET m     = MONTH (w_aux.frecafor) using "&&"
        LET d     = DAY (w_aux.frecafor)   using "&&"
        LET dia   = DAY(w_aux.fena)        USING"&&"
        LET mes   = MONTH(w_aux.fena)      USING"&&"
        LET ano   = YEAR(w_aux.fena)       USING "&&&&"}
        LET dia1  = DAY(TODAY)             USING"&&"
        LET mes1  = MONTH(TODAY)           USING"&&"
        LET ano1  = YEAR(TODAY)            USING "&&&&"
        LET num10 = "          "
        LET num10 = num                    USING "&&&&&&&&&&"

        PRINT
            COLUMN 01,w_aux.n_unico,
            COLUMN 19,w_aux.n_rfc,
            COLUMN 38,w_aux.paterno [1,15],
            COLUMN 54,w_aux.materno [1,15]                      
        PRINT
            COLUMN 01,w_aux.nombres [1,20]

END REPORT

FUNCTION Tipo_de_envio()
#te---------------------

    DEFINE z_reg ARRAY[6] OF RECORD
        cod  CHAR(2),
        desc CHAR(20)
    END RECORD

    DEFINE
        i SMALLINT

    LET z_reg[1].cod = "01" LET z_reg[1].desc = "CINTA"
    LET z_reg[2].cod = "02" LET z_reg[2].desc = "TRA"
    LET z_reg[3].cod = "03" LET z_reg[3].desc = "EN LINEA"
    LET z_reg[4].cod = "04" LET z_reg[4].desc = "LIB.PENDIENTE"
    LET z_reg[5].cod = "05" LET z_reg[5].desc = "CARTUCHO"
    LET z_reg[6].cod = "06" LET z_reg[6].desc = "DAT"

    OPEN WINDOW cent AT 11,25 WITH FORM "AFIB0272" ATTRIBUTE(BORDER)
    DISPLAY "       MEDIOS  DE  ENVIO      " AT 2,1 ATTRIBUTE(REVERSE)

    FOR i = 1 TO 6
        DISPLAY z_reg[i].* TO scr_1[i].*
    END FOR

    INPUT BY NAME g_opcion
    AFTER FIELD g_opcion
        IF g_opcion IS NULL THEN
            ERROR "OPCION NO PUEDE SER NULA"
            NEXT FIELD g_opcion
        END IF

        IF g_opcion <> "01" AND
           g_opcion <> "02" AND
           g_opcion <> "03" AND
           g_opcion <> "04" AND
           g_opcion <> "05" AND
           g_opcion <> "06" THEN
            ERROR "OPcion ERRONEA, reingrese"
            NEXT FIELD g_opcion
        ELSE
            EXIT INPUT
            END IF
        END INPUT

    CLOSE WINDOW cent

END FUNCTION

FUNCTION limpia_nulos()

    LET comm = "cd " ,g_paramgrales.ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ","E", HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".MNA > informix" 

    RUN comm

    LET comm = "cd " ,g_paramgrales.ruta_envio CLIPPED ,
               "/ ; mv informix  ","E", HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".MNA " 

    RUN comm

END FUNCTION

FUNCTION actualiza_operacion()
#ao---------------------------

    UPDATE bat_ctr_operacion
    SET    estado_operacion = 4,
           fecha_fin        = CURRENT,
           nombre_archivo   = nom_afi
    WHERE  pid              = reg_bat.pid
    AND    proceso_cod      = reg_bat.proceso_cod
    AND    opera_cod        = reg_bat.opera_cod

    UPDATE bat_ctr_proceso
    SET    estado_proceso = 4,
           fecha_fin      = CURRENT
    WHERE  pid            = reg_bat.pid
    AND    proceso_cod    = reg_bat.proceso_cod

END FUNCTION

FUNCTION fecha_max()
#fh-------------------

    DEFINE diaSemana SMALLINT

    LET diaSemana = WEEKDAY(HOY)

    LET fecha_hasta = HOY - diaSemana UNITS DAY

    CALL fechas(fecha_hasta) RETURNING HOYDIA

END FUNCTION

FUNCTION fechas(diaActual)
#sdh----------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE,
        numDias   SMALLINT

    LET diaTmp = diaActual

    FOR contador = 1 TO 3
        IF contador = 1 THEN
            CALL habil_siguiente(diaTmp) RETURNING diaTmp
        ELSE
            LET diaTmp = diaTmp + 1 UNITS DAY
            CALL habil_siguiente(diaTmp) RETURNING diaTmp
        END IF
    END FOR

    RETURN diaTmp

END FUNCTION

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE

    DEFINE
        diaHabilSig DATE,
        diaSemana   SMALLINT,
        feriado     SMALLINT,
        finSemana   SMALLINT

    LET diaHabilSig = diaActual

    WHILE TRUE
        LET feriado   = 0
        LET finSemana = 0
        LET diaSemana = WEEKDAY(diaHabilSig)

        IF diaSemana = 0 OR diaSemana = 6 THEN
            LET finSemana = 1
        END IF

        SELECT *
        FROM   tab_feriado
        WHERE  feria_fecha = diaHabilSig

        IF STATUS <> NOTFOUND THEN
            LET feriado = 1
        END IF

        IF feriado = 1 OR finSemana = 1 THEN
            LET diaHabilSig = diaHabilSig + 1 UNITS DAY
        ELSE
            EXIT WHILE
        END IF
    END WHILE

    RETURN diaHabilSig

END FUNCTION

