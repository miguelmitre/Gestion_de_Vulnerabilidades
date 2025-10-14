################################################################################
##Proyecto           => SYSTEMA DE AFORES ( MEXICO )                          ##
##Owner              => E.F.P.                                                ##
##Programa RETM901   => MANTENEDOR DE REGISTRO DE SOLICITUDES DEL ISSSTE      ##
##Sistema            => RETIROS TOTALES                                       ##
##Fecha Creacion     => 28 DE SEPTIEMBRE 2006                                 ##
##By                 => DMR                                                   ##
##Fecha Modifica     => 17 DE ABRIL 2008                                      ##
##                   => ADAPTACION PARA SOPORTE A MULTISIEFORES               ##
##                   => XAVIER TORRES RIOS                                    ##
##Fecha Modifica     => 18 de Agosto de 2008                                  ##
##                   => ADAPTACION PARA NUEVO LAYOUT ISSSTE                   ##
##                   => CESAR DAVID CHAVEZ MARTINEZ                           ##
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE ga_solicitud ARRAY[1000] OF RECORD #glo #ga_solicitud
        nss_imss              LIKE ret_sol_issste_tot.nss_imss           ,
        nss_issste            LIKE ret_sol_issste_tot.nss_issste         ,
        curp                  LIKE ret_sol_issste_tot.curp               ,
        rfc                   LIKE afi_mae_afiliado.n_rfc                ,
        nombre_afore          LIKE afi_mae_afiliado.nombres              ,
        paterno_afore         LIKE afi_mae_afiliado.paterno              ,
        materno_afore         LIKE afi_mae_afiliado.materno              ,
        fecha_nac             LIKE ret_sol_issste_tot.fecha_nac          ,
        tipo_retiro           LIKE ret_sol_issste_tot.tipo_retiro        ,
        desc_retiro           CHAR(40)                                   ,
        tipo_beneficio        LIKE ret_sol_issste_tot.tipo_beneficio     ,
        desc_beneficio        CHAR(40)                                   ,
        n_pensionista         LIKE ret_sol_issste_tot.n_pensionista      ,
        fecha_ini_pen         LIKE ret_sol_issste_tot.fecha_ini_pen      ,
        docprob_cod           LIKE ret_sol_issste_tot.cve_doc_prob       ,
        docprob_desc          CHAR(40)                                   ,
        fec_sol_susp          LIKE ret_sol_issste_tot.fec_sol_susp       ,
        fec_sol_reac          LIKE ret_sol_issste_tot.fec_sol_reac       ,
        fecha_baja            LIKE ret_sol_issste_tot.fecha_baja         ,
        fecha_solicitud       LIKE ret_sol_issste_tot.fecha_solicitud    ,
        entidad_tramite       LIKE ret_sol_issste_tot.entidad_tramite    ,
        tipo_inversion        LIKE ret_sol_issste_tot.tipo_inversion     ,
        fecha_val_ahorro      LIKE ret_sol_issste_tot.fecha_val_ahorro   ,
        porcentaje_ahorro     LIKE ret_sol_issste_tot.porcentaje_ahorro  ,
        n_oficio              LIKE ret_sol_issste_tot.n_oficio           ,
        fecha_oficio          LIKE ret_sol_issste_tot.fecha_oficio       ,
        entidad_oficio        LIKE ret_sol_issste_tot.entidad_oficio     ,
        porcentaje_fon_viv    LIKE ret_sol_issste_tot.porcentaje_fon_viv ,
        rfc_dependencia       LIKE ret_sol_issste_tot.rfc_dependencia    ,
        nom_dependencia       LIKE ret_sol_issste_tot.nom_dependencia    ,
        cve_ramo              LIKE ret_sol_issste_tot.cve_ramo           ,
        cve_pagaduria         LIKE ret_sol_issste_tot.cve_pagaduria      ,
        fecha_val_viv         LIKE ret_sol_issste_tot.fecha_val_viv      ,
        fecha_captura         DATE                                       ,
        estado_solicitud      LIKE ret_sol_issste_tot.estado_solicitud   ,
        descripcion_status    CHAR(25)                                   ,
        consecutivo           LIKE ret_sol_issste_tot.consecutivo        ,
        folio                 LIKE ret_sol_issste_tot.folio
    END RECORD

    DEFINE rg_dato RECORD #glo #rg_input
        nss_imss              LIKE ret_sol_issste_tot.nss_imss           ,
        nss_issste            LIKE ret_sol_issste_tot.nss_issste         ,
        curp                  LIKE ret_sol_issste_tot.curp               ,
        rfc                   LIKE afi_mae_afiliado.n_rfc                ,
        nombre_afore          LIKE afi_mae_afiliado.nombres              ,
        paterno_afore         LIKE afi_mae_afiliado.paterno              ,
        materno_afore         LIKE afi_mae_afiliado.materno              ,
        fecha_nac             LIKE ret_sol_issste_tot.fecha_nac          ,
        tipo_retiro           LIKE ret_sol_issste_tot.tipo_retiro        ,
        desc_retiro           CHAR(40)                                   ,
        tipo_beneficio        LIKE ret_sol_issste_tot.tipo_beneficio     ,
        desc_beneficio        CHAR(40)                                   ,
        n_pensionista         LIKE ret_sol_issste_tot.n_pensionista      ,
        fecha_ini_pen         LIKE ret_sol_issste_tot.fecha_ini_pen      ,
        docprob_cod           LIKE ret_sol_issste_tot.cve_doc_prob       ,
        docprob_desc          CHAR(25)                                   ,
        fec_sol_susp          LIKE ret_sol_issste_tot.fec_sol_susp       ,
        fec_sol_reac          LIKE ret_sol_issste_tot.fec_sol_reac       ,
        fecha_baja            LIKE ret_sol_issste_tot.fecha_baja         ,
        fecha_solicitud       LIKE ret_sol_issste_tot.fecha_solicitud    ,
        entidad_tramite       LIKE ret_sol_issste_tot.entidad_tramite    ,
        tipo_inversion        LIKE ret_sol_issste_tot.tipo_inversion     ,
        fecha_val_ahorro      LIKE ret_sol_issste_tot.fecha_val_ahorro   ,
        porcentaje_ahorro     LIKE ret_sol_issste_tot.porcentaje_ahorro  ,
        n_oficio              LIKE ret_sol_issste_tot.n_oficio           ,
        fecha_oficio          LIKE ret_sol_issste_tot.fecha_oficio       ,
        entidad_oficio        LIKE ret_sol_issste_tot.entidad_oficio     ,
        porcentaje_fon_viv    LIKE ret_sol_issste_tot.porcentaje_fon_viv ,
        rfc_dependencia       LIKE ret_sol_issste_tot.rfc_dependencia    ,
        nom_dependencia       LIKE ret_sol_issste_tot.nom_dependencia    ,
        cve_ramo              LIKE ret_sol_issste_tot.cve_ramo           ,
        cve_pagaduria         LIKE ret_sol_issste_tot.cve_pagaduria      ,
        fecha_val_viv         LIKE ret_sol_issste_tot.fecha_val_viv      ,
        fecha_captura         LIKE ret_sol_issste_tot.fecha_captura      ,
        estado_solicitud      LIKE ret_sol_issste_tot.estado_solicitud   ,
        descripcion_status    CHAR(25)                                   ,
        consecutivo           LIKE ret_sol_issste_tot.consecutivo        ,
        folio                 LIKE ret_sol_issste_tot.folio
    END RECORD

    DEFINE reg_1 RECORD #glo #reg_1
        capturado             LIKE ret_status.status ,
        confirmado            LIKE ret_status.status ,
        liquidado             LIKE ret_status.status
    END RECORD

    DEFINE #glo #date
        HOY                   ,
        vfecha_ini_p          ,
        vfecha_resolucion     ,
	      vfecha_causa          DATE

    DEFINE  #glo #char
        txt_2                 CHAR(3000) ,
        txt_3                 CHAR(3000) ,
        txt_5                 CHAR(3000) ,
        x_busca               CHAR(2200) ,
        s_codigo_afore        CHAR(0004) ,
        option_afore          CHAR(0006) ,
        usuario               CHAR(0012) ,
        v_marca               CHAR(0100) ,
        v_desmarca            CHAR(0100) ,
        ejecuta               CHAR(0500) ,
        enter                 CHAR       ,
        opc                   CHAR(0001) ,
	      x_error               CHAR(0500) ,
        desc_status_rech      CHAR(0020) ,
	      x_usuario             CHAR(0012) ,
	      x_estado_solicitud    CHAR(0040) ,
	      v_ejecuta             CHAR(0200) ,
        vaccion               CHAR(0001) ,
        vnss                  CHAR(0011) ,
        vregimen              CHAR(0002) ,
        vtipo_seguro          CHAR(0002) ,
        vtipo_pension         CHAR(0002) ,
        vtipo_retiro          CHAR(0001) ,
        desc_inversion        CHAR(13)   ,
        c10_fecha             CHAR(10)   ,
        ventana               CHAR(15)

    DEFINE #glo #smallint
        arr_c		          ,
        marca_ent             ,
        i                     ,
        pos                   ,
        v_marca_res           ,
        v_marca_ent           ,
        v_cod_rechazo         ,
        s_tipo_movimiento     ,
        sw_2                  ,
        sw                    ,
        vtipo_prestacion      ,
        vestado_marca         ,
        vcodigo_rechazo       ,
        vmarca_causa          ,
        flag                  SMALLINT

    DEFINE #glo #integer
        ult_consecutivo       ,
        vmax_folio            INTEGER
END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETM901.log")
    CALL init() #i

    OPEN WINDOW retm9011 AT 2,2 WITH FORM "RETM9011" ATTRIBUTE(BORDER)

    DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)

    DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 10,1 ATTRIBUTE(REVERSE)

    CASE vaccion
        WHEN "C"
            CALL inicializa()
            CALL agrega() #a
        WHEN "M"
            CALL inicializa()
            CALL modifica() #m
        OTHERWISE

    MENU "MENU"
        BEFORE MENU
            IF option_afore <> FALSE THEN
                HIDE OPTION ALL
                SHOW OPTION "Agrega","Consulta","Modifica","Elimina","Salida"
            ELSE
                HIDE OPTION ALL
                SHOW OPTION "Agrega","Consulta","Modifica",
                            "Elimina","conFirma","Salida"
            END IF

        COMMAND KEY("A") "Agrega" "Agrega Solicitud"
            CALL inicializa()
            CALL agrega() #a

        COMMAND KEY("C") "Consulta" "Consulta Solicitud"
            CALL inicializa()
            CALL consulta() #c

        COMMAND KEY("M") "Modifica" "Modifica Solicitud"
            CALL inicializa()
            CALL modifica() #m

        COMMAND KEY("E") "Elimina" "Elimina Solicitud"
            CALL inicializa()
            CALL elimina() #e

        COMMAND KEY("F") "conFirma" "Confirma Solicitud"
            CALL inicializa()
            CALL confirma() #c

        COMMAND KEY("S") "Salida" "Regresa al Menu"
            EXIT MENU

    END MENU
    END CASE
    CLOSE WINDOW retm9011
END MAIN


FUNCTION init()
#in-------------

    LET vaccion           = ARG_VAL(1)
    LET vnss              = ARG_VAL(2)
    LET vregimen          = ARG_VAL(3)
    LET vtipo_prestacion  = ARG_VAL(4)
    LET vtipo_seguro      = ARG_VAL(5)
    LET vtipo_pension     = ARG_VAL(6)
    LET vtipo_retiro      = ARG_VAL(7)
    LET vfecha_ini_p      = ARG_VAL(8)
    LET vfecha_resolucion = ARG_VAL(9)

    LET HOY = TODAY

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT A.status
    INTO   reg_1.capturado
    FROM   ret_status A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.status
    INTO   reg_1.confirmado
    FROM   ret_status A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.status
    INTO   reg_1.liquidado
    FROM   ret_status A
    WHERE  A.descripcion = "LIQUIDADO"

    ######## MARCAJE ###################

    LET v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? ) "
    PREPARE eje_marca FROM v_marca

    ####################################
END FUNCTION


FUNCTION inicializa()
#i--------------------
    INITIALIZE rg_dato.* TO NULL
    CLEAR FORM
END FUNCTION


FUNCTION agrega()
#a---------------
    DEFINE #char #loc
        id                    CHAR(02) ,
        descripcion           CHAR(20)

    --Ventana de seleccion del tipo de retiro--

    OPEN WINDOW retm9017 AT 07,10 WITH FORM "RETM9015" ATTRIBUTE(BORDER)
        DISPLAY "                    TIPO DE RETIRO ISSSTE                       " AT 2,1 ATTRIBUTE(REVERSE)

        INPUT BY NAME id
            AFTER FIELD id
                IF id IS NULL OR id = 0 THEN
                    CALL despliega_tipo_retiro()  #dtr
                    LET  id = rg_dato.tipo_retiro CLIPPED
                    DISPLAY id TO id
                    DISPLAY rg_dato.desc_retiro TO descripcion
                ELSE
                    LET rg_dato.tipo_retiro = id
                    SELECT "OK"
                    FROM   tab_retiro_issste
                    WHERE  tipo_retiro = rg_dato.tipo_retiro
                    AND    tipo_retiro NOT IN (4,5)

                    IF STATUS <> NOTFOUND THEN
                       SELECT desc_retiro
                       INTO   rg_dato.desc_retiro
                       FROM   tab_retiro_issste
                       WHERE  tipo_retiro = rg_dato.tipo_retiro

                       DISPLAY rg_dato.desc_retiro TO descripcion
                       EXIT INPUT
                    ELSE
                       ERROR " TIPO DE RETIRO ISSSTE INEXISTENTE ... "
                       NEXT FIELD id
                    END IF
                END IF
        END INPUT
    CLOSE WINDOW retm9017

    CASE rg_dato.tipo_retiro
       WHEN 1       --pantalla del tipo de retiro 1
          CALL retiro_tipo_1 ("a")  #r1#
          CLOSE WINDOW retm9011_1
       WHEN 2       --pantalla del tipo de retiro 2
          CALL retiro_tipo_2 ("a")  #r2#
          CLOSE WINDOW retm9011_2
       WHEN 3
          CALL retiro_tipo_3_6 ("a")#r3#
          CLOSE WINDOW retm9011_3
       WHEN 6  --pantalla del tipo de retiro 3 y 6
          CALL retiro_tipo_3_6 ("a")#r3#
          CLOSE WINDOW retm9011_3
       WHEN 7       --pantalla del tipo de retiro 7
          CALL retiro_tipo_7 ("a")  #r7#
          CLOSE WINDOW retm9011_7
       WHEN 8       --pantalla del tipo de retiro 8
          CALL retiro_tipo_8 ("a")  #r8#
          CLOSE WINDOW retm9011_8
       WHEN 9       --pantalla del tipo de retiro 9
          CALL retiro_tipo_9 ("a")  #r9#
          CLOSE WINDOW retm9011_9
    END CASE
END FUNCTION


#####################################################################
##
##
##
#####################################################################
FUNCTION consulta()
#c-----------------
    OPEN WINDOW retm9013 AT 2,2 WITH FORM "RETM9013" ATTRIBUTE(BORDER)

    DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)

    DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 ATTRIBUTE(REVERSE)

    DISPLAY " ESC : Consulta                                            CONTROL-C : SALIR   " AT 2,1

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON a.nss_imss          ,
                                 a.nss_issste        ,
                                 a.curp              ,
                                 a.rfc               ,
                                 a.nombre_afore      ,
                                 a.paterno_afore     ,
                                 a.materno_afore     ,
                                 a.tipo_retiro       ,
                                 a.tipo_beneficio    ,
                                 a.docprob_cod       ,
                                 a.fecha_captura     ,
                                 a.estado_solicitud  ,
                                 a.folio             ,
                                 a.consecutivo
        ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (ESC)
                LET int_flag = FALSE
                EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE  THEN
       LET int_flag = FALSE
       CLOSE WINDOW retm9013
       RETURN
    END IF

    ERROR "PROCESANDO INFORMACION"
    LET   txt_2 =" SELECT A.nss_imss                 , ",
                        " A.nss_issste               , ", #nss_issste
                        " A.curp                     , ", #curp
                        " B.n_rfc                    , ", #rfc
                        " B.paterno                  , ", #paterno
                        " B.materno                  , ", #materno
                        " B.nombres                  , ", #nombres
                        " A.fecha_nac                , ", #fecha_nac
                        " A.tipo_retiro              , ",
                        " ' '                        , ", #des_tipo_ret
                        " A.tipo_beneficio           , ", #tipo_beneficio
                        " ' '                        , ", #desc_beneficio
                        " A.n_pensionista            , ",
                        " A.fecha_ini_pen            , ",
                        " A.cve_doc_prob             , ",
                        " ' '                        , ", #desc_doc_prob
                        " A.fec_sol_susp             , ",
                        " A.fec_sol_reac             , ",
                        " A.fecha_baja               , ",
                        " A.fecha_solicitud          , ",
                        " A.entidad_tramite          , ",
                        " A.tipo_inversion           , ",
                        " A.fecha_val_ahorro         , ",
                        " A.porcentaje_ahorro        , ",
                        " A.n_oficio                 , ",
                        " A.fecha_oficio             , ",
                        " A.entidad_oficio           , ",
                        " A.porcentaje_fon_viv       , ",
                        " A.rfc_dependencia          , ",
                        " A.nom_dependencia          , ",
                        " A.cve_ramo                 , ",
                        " A.cve_pagaduria            , ",
                        " A.fecha_val_viv            , ",
                        " A.fecha_captura            , ",
                        " A.estado_solicitud         , ",
                        " ' '                        , ",
                        " A.consecutivo              , ",
                        " A.folio                      ",  #folio_int
                 " FROM   ret_sol_issste_tot A, ",
			 "afi_mae_afiliado B ",
                 " WHERE  ",x_busca CLIPPED             ,
                 " AND    A.nss_imss    = B.n_seguro    "

    PREPARE pre_2 FROM txt_2
    DECLARE cur_2 CURSOR FOR pre_2
    LET i = 1
    FOREACH cur_2 INTO ga_solicitud[i].*

        SELECT n_rfc     ,
               nombres   ,
               paterno   ,
               materno
        INTO   ga_solicitud[i].rfc,
               ga_solicitud[i].nombre_afore,
               ga_solicitud[i].paterno_afore,
               ga_solicitud[i].materno_afore
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].nss_imss

  {     SELECT A.fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta A, ret_solicitud_tx B
        WHERE  A.nss      = ga_solicitud[i].n_seguro
        AND    B.nss      = A.nss
        AND    A.consecutivo_lote = ga_solicitud[i].consecutivo
        AND    A.consecutivo_lote = B.consecutivo
        AND    A.estado   = reg_1.liquidado
        GROUP BY 1    }

        SELECT desc_retiro
        INTO   ga_solicitud[i].desc_retiro
        FROM   tab_retiro_issste
        WHERE  tipo_retiro = ga_solicitud[i].tipo_retiro

        SELECT desc_beneficio
        INTO   ga_solicitud[i].desc_beneficio
        FROM   tab_beneficio_issste
        WHERE  tipo_beneficio = ga_solicitud[i].tipo_beneficio

        SELECT docprob_desc
        INTO   ga_solicitud[i].docprob_desc
        FROM   tab_doc_prob
        WHERE  docprob_cod = ga_solicitud[i].docprob_cod

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_status A
        WHERE  A.status  = ga_solicitud[i].estado_solicitud
        display "xx ",ga_solicitud[i].estado_solicitud
        display "xx ",ga_solicitud[i].descripcion_status
        LET i = i + 1

     END FOREACH

     IF i = 1 THEN
         INITIALIZE rg_dato.* TO NULL
         CLEAR FORM
         ERROR "    NO EXISTE REGISTRO "
         ATTRIBUTE(REVERSE)
         SLEEP 2
         ERROR ""
         CLOSE WINDOW retm9013
         RETURN
     END IF

     CALL SET_COUNT(i-1)

     ERROR ""

     DISPLAY ARRAY ga_solicitud TO scr_1.*

        ON KEY (CONTROL-C)
           CALL inicializa()
           EXIT DISPLAY

        ON KEY (INTERRUPT)
           CALL inicializa()
           EXIT DISPLAY

     END DISPLAY

     CLOSE WINDOW retm9013
END FUNCTION


FUNCTION modifica()
#m-----------------
    OPEN WINDOW retm9013 AT 2,2 WITH FORM "RETM9013" ATTRIBUTE(BORDER)

    DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)

    DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Consulta               ENTER : MODIFICAR            CONTROL-C : SALIR   " AT 2,1

    LET int_flag = FALSE
    LET sw_2 = 1 ## modifica

    IF vaccion = "M" THEN

        IF int_flag = TRUE  THEN
            LET INT_FLAG = FALSE
            CLOSE WINDOW retm9013
            RETURN
        END IF

        LET txt_3 = " SELECT A.nss_imss                 ,",
                    "        A.nss_issste               ,", #nss_issste
                    "        A.curp                     ,", #curp
                    "        B.n_rfc                    ,", #n_rfc
                    "        B.paterno                  ,", #paterno
                    "        B.materno                  ,", #materno
                    "        B.nombres                  ,", #nombre
                    "        A.fecha_nac                ,", #fecha_nac
                    "        A.tipo_retiro              ,", #tipo_retiro
                    "        ' '                        ,", #des_tipo_ret
                    "        A.tipo_prestacion          ,", #tipo_beneficio
                    "        ' '                        ,", #desc_beneficio
                    "        A.n_pensionista            ,",
                    "        A.fecha_ini_pen            ,",
                    "        A.cve_doc_prob             ,",
                    "        ' '                        ,", #desc_doc_prob
                    "        A.fec_sol_susp             ,",
                    "        A.fec_sol_reac             ,",
                    "        A.fecha_baja               ,",
                    "        A.fecha_solicitud          ,",
                    "        A.entidad_tramite          ,",
                    "        A.tipo_inversion           ,",
                    "        A.fecha_val_ahorro         ,",
                    "        A.porcentaje_ahorro        ,",
                    "        A.n_oficio                 ,", #n_oficio
                    "        A.fecha_oficio             ,", #fecha_oficio
                    "        A.entidad_oficio           ,", #entidad_oficio
                    "        A.porcentaje_fon_viv       ,", #porcentaje_fon_viv
                    "        A.rfc_dependencia          ,",
                    "        A.nom_dependencia          ,",
                    "        A.cve_ramo                 ,",
                    "        A.cve_pagaduria            ,",
                    "        A.fecha_val_viv            ,",
                    "        A.fecha_captura            ,",
                    "        A.estado_solicitud         ,",
                    "        ' '                        ,",
                    "        A.consecutivo              ,",
                    "        A.folio                     ",
                    " FROM   ret_sol_issste_tot A       ,",
		    "        afi_mae_afiliado B          ",
                    " WHERE  A.nss_imss   = '",vnss,   "'",
                    " AND    A.nss_imss   = B.n_seguro   "

    ELSE
        CONSTRUCT BY NAME x_busca ON a.nss_imss         ,
                                     a.nss_issste       ,
                                     a.curp             ,
                                     a.rfc              ,
                                     a.nombre_afore     ,
                                     a.paterno_afore    ,
                                     a.materno_afore    ,
                                     a.tipo_retiro      ,
                                     a.tipo_beneficio   ,
                                     a.docprob_cod

            ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

            ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT

            ON KEY (ESC)
                LET INT_FLAG = FALSE
                EXIT CONSTRUCT

        END CONSTRUCT

        IF int_flag = TRUE  THEN
            LET INT_FLAG = FALSE
            CLOSE WINDOW retm9013
            RETURN
        END IF

        LET txt_3 = " SELECT A.nss_imss                 ,",
                    "        A.nss_issste               ,", #nss_issste
                    "        A.curp                     ,", #curp
                    "        B.n_rfc                    ,", #n_rfc
                    "        B.paterno                  ,", #paterno
                    "        B.materno                  ,", #materno
                    "        B.nombres                  ,", #nombre
                    "        A.fecha_nac                ,", #fecha_nac
                    "        A.tipo_retiro              ,", #tipo_retiro
                    "        ' '                        ,", #des_tipo_ret
                    "        A.tipo_beneficio           ,", #tipo_beneficio
                    "        ' '                        ,", #desc_beneficio
                    "        A.n_pensionista            ,",
                    "        A.fecha_ini_pen            ,",
                    "        A.cve_doc_prob             ,",
                    "        ' '                        ,", #desc_doc_prob
                    "        A.fec_sol_susp             ,",
                    "        A.fec_sol_reac             ,",
                    "        A.fecha_baja               ,",
                    "        A.fecha_solicitud          ,",
                    "        A.entidad_tramite          ,",
                    "        A.tipo_inversion           ,",
                    "        A.fecha_val_ahorro         ,",
                    "        A.porcentaje_ahorro        ,",
                    "        A.n_oficio                 ,", #n_oficio
                    "        A.fecha_oficio             ,", #fecha_oficio
                    "        A.entidad_oficio           ,", #entidad_oficio
                    "        A.porcentaje_fon_viv       ,", #porcentaje_fon_viv
                    "        A.rfc_dependencia          ,",
                    "        A.nom_dependencia          ,",
                    "        A.cve_ramo                 ,",
                    "        A.cve_pagaduria            ,",
                    "        A.fecha_val_viv            ,",
                    "        A.fecha_captura            ,",
                    "        A.estado_solicitud         ,",
                    "        ' '                        ,",
                    "        A.consecutivo              ,",
                    "        A.folio                     ",
                    " FROM   ret_sol_issste_tot A       ,",
		    "        afi_mae_afiliado B          ",
                    " WHERE  ",x_busca CLIPPED            ,
                    " AND    A.nss_imss = B.n_seguro     "
    END IF

    PREPARE pre_3 FROM txt_3
    DECLARE cur_3 CURSOR FOR pre_3
    LET i = 1
    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_3 INTO ga_solicitud[i].*

        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

        IF ga_solicitud[i].estado_solicitud > 0 THEN

	    PROMPT "SOLICITUD DEL NSS <",ga_solicitud[i].nss_imss,
	     			      "> SE ENCUENTRA EN EL ESTADO : ",
		  		      x_estado_solicitud CLIPPED,
				      " " ATTRIBUTE(REVERSE)
	           FOR opc ATTRIBUTE(REVERSE)
	    CONTINUE FOREACH
        END IF

        SELECT paterno   ,
               materno   ,
               nombres
        INTO   ga_solicitud[i].paterno_afore,
               ga_solicitud[i].materno_afore,
               ga_solicitud[i].nombre_afore
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].nss_imss

        SELECT desc_retiro
        INTO   ga_solicitud[i].desc_retiro
        FROM   tab_retiro_issste
        WHERE  tipo_retiro = ga_solicitud[i].tipo_retiro

        SELECT desc_beneficio
        INTO   ga_solicitud[i].desc_beneficio
        FROM   tab_beneficio_issste
        WHERE  tipo_beneficio = ga_solicitud[i].tipo_beneficio

        SELECT docprob_desc
        INTO   ga_solicitud[i].docprob_desc
        FROM   tab_doc_prob
        WHERE  docprob_cod = ga_solicitud[i].docprob_cod

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_status A
        WHERE  A.status = ga_solicitud[i].estado_solicitud

        LET i = i + 1

    END FOREACH

    ERROR ""
    IF (i-1) >= 1 THEN
        CALL SET_COUNT(i-1)

        DISPLAY ARRAY ga_solicitud TO scr_1.*

            ON KEY ( CONTROL-C )
               LET int_flag=TRUE
               EXIT DISPLAY

            ON KEY ( INTERRUPT )
               LET int_flag=TRUE
               EXIT DISPLAY

            ON KEY ( CONTROL-M )
               LET pos = ARR_CURR()

               EXIT DISPLAY
        END DISPLAY
    ELSE
        ERROR "    NO EXISTE REGISTRO"
        ATTRIBUTE(REVERSE)
        SLEEP 2
        ERROR ""
        CLOSE WINDOW retm9013
        RETURN
    END IF

    IF int_flag = TRUE THEN
        LET int_flag=FALSE
        CLOSE WINDOW retm9013
        RETURN
    END IF

    CALL construccion(ga_solicitud[pos].*,sw_2) #co

    CLOSE WINDOW retm9013
END FUNCTION


FUNCTION elimina()
#e-----------------
    DEFINE #loc #smallint
        arr            ,
        src            SMALLINT

    OPEN WINDOW retm9013 AT 2,2 WITH FORM "RETM9013" ATTRIBUTE(BORDER)

    DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)

    DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 ATTRIBUTE(REVERSE)

    DISPLAY " ESC : Consulta               ENTER : ELIMINAR             CONTROL-C : SALIR   " AT 2,1

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON a.nss_imss          ,
                                 a.nss_issste        ,
                                 a.curp              ,
                                 a.rfc               ,
                                 a.nombre_afore      ,
                                 a.paterno_afore     ,
                                 a.materno_afore     ,
                                 a.tipo_retiro       ,
                                 a.tipo_beneficio    ,
                                 a.docprob_cod       ,
                                 a.fecha_captura     ,
                                 a.estado_solicitud  ,
                                 a.folio             ,
                                 a.consecutivo
        ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (ESC)
                LET int_flag = FALSE
                EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE  THEN
       LET int_flag = FALSE
       CLOSE WINDOW retm9013
       RETURN
    END IF

    LET   txt_2 =" SELECT A.nss_imss                 , ",
                        " A.nss_issste               , ", #nss_issste
                        " A.curp                     , ", #curp
                        " B.n_rfc                    , ", #rfc
                        " B.paterno                  , ", #paterno
                        " B.materno                  , ", #materno
                        " B.nombres                  , ", #nombres
                        " A.fecha_nac                , ", #fecha_nac
                        " A.tipo_retiro              , ",
                        " ' '                        , ", #des_tipo_ret
                        " A.tipo_beneficio           , ", #tipo_beneficio
                        " ' '                        , ", #desc_beneficio
                        " A.n_pensionista            , ",
                        " A.fecha_ini_pen            , ",
                        " A.cve_doc_prob             , ",
                        " ' '                        , ", #desc_doc_prob
                        " A.fec_sol_susp             , ",
                        " A.fec_sol_reac             , ",
                        " A.fecha_baja               , ",
                        " A.fecha_solicitud          , ",
                        " A.entidad_tramite          , ",
                        " A.tipo_inversion           , ",
                        " A.fecha_val_ahorro         , ",
                        " A.porcentaje_ahorro        , ",
                        " A.n_oficio                 , ",
                        " A.fecha_oficio             , ",
                        " A.entidad_oficio           , ",
                        " A.porcentaje_fon_viv       , ",
                        " A.rfc_dependencia          , ",
                        " A.nom_dependencia          , ",
                        " A.cve_ramo                 , ",
                        " A.cve_pagaduria            , ",
                        " A.fecha_val_viv            , ",
                        " A.fecha_captura            , ",
                        " A.estado_solicitud         , ",
                        " ' '                        , ",
                        " A.consecutivo              , ",
                        " A.folio                      ",  #folio_int
                 " FROM   ret_sol_issste_tot A, ",
			 "afi_mae_afiliado B ",
                 " WHERE  ",x_busca CLIPPED             ,
                 " AND    A.nss_imss    = B.n_seguro    "

    PREPARE pre_4 FROM txt_2
    DECLARE cur_4 CURSOR FOR pre_4
    LET i = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_4 INTO ga_solicitud[i].*

        SELECT n_rfc     ,
               nombres   ,
               paterno   ,
               materno
        INTO   ga_solicitud[i].rfc,
               ga_solicitud[i].nombre_afore,
               ga_solicitud[i].paterno_afore,
               ga_solicitud[i].materno_afore
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].nss_imss

  {     SELECT A.fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta A, ret_solicitud_tx B
        WHERE  A.nss      = ga_solicitud[i].n_seguro
        AND    B.nss      = A.nss
        AND    A.consecutivo_lote = ga_solicitud[i].consecutivo
        AND    A.consecutivo_lote = B.consecutivo
        AND    A.estado   = reg_1.liquidado
        GROUP BY 1    }

        SELECT desc_retiro
        INTO   ga_solicitud[i].desc_retiro
        FROM   tab_retiro_issste
        WHERE  tipo_retiro = ga_solicitud[i].tipo_retiro

        SELECT desc_beneficio
        INTO   ga_solicitud[i].desc_beneficio
        FROM   tab_beneficio_issste
        WHERE  tipo_beneficio = ga_solicitud[i].tipo_beneficio

        SELECT docprob_desc
        INTO   ga_solicitud[i].docprob_desc
        FROM   tab_doc_prob
        WHERE  docprob_cod = ga_solicitud[i].docprob_cod

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_status A
        WHERE  A.status  = ga_solicitud[i].estado_solicitud

        IF ga_solicitud[i].estado_solicitud > 0 THEN

           PROMPT "SOLICITUD DEL NSS <",ga_solicitud[i].nss_imss,
                                 "> SE ENCUENTRA EN EL ESTADO : ",
                                 ga_solicitud[i].descripcion_status CLIPPED,
                                 " " ATTRIBUTE(REVERSE)
           FOR opc ATTRIBUTE(REVERSE)
           CONTINUE FOREACH
        END IF

        LET i = i + 1

    END FOREACH

    ERROR ""
    IF (i-1) >= 1 THEN
        CALL SET_COUNT(i-1)

        DISPLAY ARRAY ga_solicitud TO scr_1.*

             ON KEY ( CONTROL-C )
                LET int_flag=1
                EXIT DISPLAY

             ON KEY ( INTERRUPT )
                LET int_flag=1
                EXIT DISPLAY

          {  ON KEY ( CONTROL-B )

             LET      arr_c           =  arr_curr()
             LET pos = ARR_CURR()
             SELECT "OK"
             FROM   ret_beneficiario
             WHERE  nss         =  ga_solicitud[pos].n_seguro
             AND    consecutivo =  ga_solicitud[pos].consecutivo
             GROUP BY 1

             IF STATUS <> NOTFOUND THEN
                LET v_ejecuta = "fglgo RETM810 ",ga_solicitud[arr_c].n_seguro CLIPPED,
                                " ", ga_solicitud[arr_c].consecutivo CLIPPED,
                                " ",'C' CLIPPED
                RUN v_ejecuta
             ELSE
                ERROR " NO SE ENCONTRARON BENEFICIARIOS ..."
                ATTRIBUTE(REVERSE) SLEEP 2
                EXIT DISPLAY
             END IF                                             }

             ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 WHILE TRUE
                     PROMPT "DESEAS ELIMINAR EL REGISTRO S/N ? " FOR opc
                         IF opc NOT MATCHES "[SsNn]" THEN
                             CONTINUE WHILE
                         ELSE
                             EXIT WHILE
                         END IF
                 END WHILE

                 IF opc MATCHES "[Nn]"  THEN
                     ERROR " REGISTRO CANCELADO "
                     ATTRIBUTE(REVERSE)
                     SLEEP 2
                     CALL inicializa() #i
                     LET int_flag = 1
                     EXIT DISPLAY
                 END IF

                 LET arr = ARR_CURR()
                 LET src = SCR_LINE()

                 DELETE FROM ret_beneficiario
                 WHERE  nss = ga_solicitud[arr].nss_imss
                 AND  consecutivo = ga_solicitud[arr].consecutivo

                 IF SQLCA.SQLCODE < 0 THEN
                    LET x_error = "DELETE ret_beneficiario:",
                                  "nss ",ga_solicitud[arr].nss_imss,
                                  "consecutivo ",ga_solicitud[arr].consecutivo,
                                  err_get(SQLCA.SQLCODE)

                    CALL errorlog(x_error CLIPPED)

                    PROMPT " ERROR DE BORRADO ret_beneficiario AVISE A SISTEMAS"
                    FOR enter
                    EXIT PROGRAM
                 END IF

                 DELETE FROM ret_sol_issste_tot
                 WHERE  nss_imss    = ga_solicitud[arr].nss_imss
                 AND    consecutivo = ga_solicitud[arr].consecutivo
                 AND    estado_solicitud = 0

                 IF SQLCA.SQLCODE < 0 THEN
                    LET x_error = "DELETE ret_sol_issste_tot :",
                                  "nss ",ga_solicitud[arr].nss_imss,
                                  "consecutivo ",ga_solicitud[arr].consecutivo,
                                  err_get(SQLCA.SQLCODE)

                    CALL errorlog(x_error CLIPPED)

                    PROMPT " ERROR DE BORRADO ret_sol_issste_tot AVISE A SISTEMAS"
                    FOR enter
                    EXIT PROGRAM
                 ELSE
                    WHENEVER ERROR STOP
                    ##### DESMARCAJE ##################################

                    SELECT movimiento
                    INTO   s_tipo_movimiento
                    FROM   tab_retiro_issste
                    WHERE  tipo_retiro = ga_solicitud[arr].tipo_retiro

                    LET v_marca_ent     = s_tipo_movimiento
                    LET vestado_marca   = 40
                    LET vcodigo_rechazo = 0
                    LET vmarca_causa    = 0
                    LET vfecha_causa    = NULL

                    LET v_desmarca   = " EXECUTE PROCEDURE desmarca_cuenta('",
                                       ga_solicitud[arr].nss_imss,"',",
                                       v_marca_ent,",",
                                       ga_solicitud[arr].consecutivo,",",
                                       vestado_marca,",",
                                       vmarca_causa,",' ",
                                       usuario,"')"
                    PREPARE eje_rever_mar FROM v_desmarca
                    EXECUTE eje_rever_mar

                    ##########################################################

                    ERROR "REGISTRO ELIMINADO ",""
                    ATTRIBUTE(REVERSE)
                    EXIT DISPLAY
                 END IF
         END DISPLAY
     ELSE
         ERROR " NO EXISTE REGISTRO"
         ATTRIBUTE(REVERSE)
         CLOSE WINDOW retm9013
         RETURN
     END IF

     IF int_flag = 1 THEN
         CLOSE WINDOW retm9013
         RETURN
         LET int_flag=0
     END IF

CLOSE WINDOW retm9013
END FUNCTION


FUNCTION confirma()
#cf----------------
    OPEN WINDOW retm9015 AT 2,2 WITH FORM "RETM9013" ATTRIBUTE(BORDER)

    DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)

    DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 ATTRIBUTE(REVERSE)

    DISPLAY " ESC : Consulta               ENTER : CONFIRMAR            CONTROL-C : SALIR   " AT 2,1

    LET int_flag = FALSE
    LET sw_2 = 2  #confirma
    CLEAR FORM

    CONSTRUCT BY NAME x_busca ON a.nss_imss          ,
                                 a.nss_issste        ,
                                 a.curp              ,
                                 a.rfc               ,
                                 a.nombre_afore      ,
                                 a.paterno_afore     ,
                                 a.materno_afore     ,
                                 a.tipo_retiro       ,
                                 a.tipo_beneficio    ,
                                 a.docprob_cod       ,
                                 a.fecha_captura     ,
                                 a.estado_solicitud  ,
                                 a.folio             ,
                                 a.consecutivo

        ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE  THEN
       LET int_flag = FALSE
       CLOSE WINDOW retm9015
       RETURN
    END IF

    LET   txt_5 =" SELECT A.nss_imss                 , ",
                        " A.nss_issste               , ", #nss_issste
                        " A.curp                     , ",
                        " B.n_rfc                    , ", #n_rfc
                        " B.paterno                  , ", #paterno_afore
                        " B.materno                  , ", #materno_afore
                        " B.nombres                  , ", #nombre_afore
                        " A.fecha_nac                , ", #fecha_nac
                        " A.tipo_retiro              , ",
                        " ' '                        , ", #des_tipo_ret
                        " A.tipo_beneficio           , ", #tipo_prestacion
                        " ' '                        , ", #desc_prestacion
                        " A.n_pensionista            , ",
                        " A.fecha_ini_pen            , ",
                        " A.cve_doc_prob             , ", #cve_doc_prob
                        " ' '                        , ", #desc_doc_prob
                        " A.fec_sol_susp             , ",
                        " A.fec_sol_reac             , ",
                        " A.fecha_baja               , ",
                        " A.fecha_solicitud          , ",
                        " A.entidad_tramite          , ",
                        " A.tipo_inversion           , ",
                        " A.fecha_val_ahorro         , ",
                        " A.porcentaje_ahorro        , ",
                        " A.n_oficio                 , ",
                        " A.fecha_oficio             , ",
                        " A.entidad_oficio           , ",
                        " A.porcentaje_fon_viv       , ",
                        " A.rfc_dependencia          , ",
                        " A.nom_dependencia          , ",
                        " A.cve_ramo                 , ",
                        " A.cve_pagaduria            , ",
                        " A.fecha_val_viv            , ",
                        " A.fecha_captura            , ",
                        " A.estado_solicitud         , ",
                        " ' '                        , ", #descripcion_status
                        " A.consecutivo              , ",
                        " A.folio                      ", #folio_int
                        " FROM  ret_sol_issste_tot A , ",
		                "       afi_mae_afiliado B     ",
                        " WHERE ",x_busca CLIPPED       ,
     		            " AND   A.estado_solicitud  IN (0,-1) ",
                        " AND   A.nss_imss          = B.n_seguro "


    PREPARE pre_8 FROM txt_5
    DECLARE cur_8 CURSOR FOR pre_8
    LET i = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_8 INTO ga_solicitud[i].*
        SELECT usuario_captura
        INTO   x_usuario
        FROM   ret_sol_issste_tot
        WHERE  nss_imss    = ga_solicitud[i].nss_imss
        AND    consecutivo = ga_solicitud[i].consecutivo

      IF s_codigo_afore <> "564 " THEN
        IF x_usuario = usuario THEN
            PROMPT "  USUARIO ES EL MISMO DE CAPTURA " ATTRIBUTE (REVERSE)
            FOR opc ATTRIBUTE (REVERSE)
            LET sw = 1
            EXIT FOREACH
        END IF                     ---SOLICITADO POR MLM
      END IF
        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

        IF ga_solicitud[i].estado_solicitud > 0 THEN

  	   PROMPT "SOLICITUD DEL NSS <",ga_solicitud[i].nss_imss,
	         	             "> SE ENCUENTRA EN EL ESTADO : ",
				     x_estado_solicitud CLIPPED,
				     " " ATTRIBUTE(REVERSE)
	   FOR opc ATTRIBUTE(REVERSE)
	   CONTINUE FOREACH
        END IF

        SELECT paterno   ,
               materno   ,
               nombres
        INTO   ga_solicitud[i].paterno_afore,
               ga_solicitud[i].materno_afore,
               ga_solicitud[i].nombre_afore
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].nss_imss

        SELECT desc_retiro
        INTO   ga_solicitud[i].desc_retiro
        FROM   tab_retiro_issste
        WHERE  tipo_retiro = ga_solicitud[i].tipo_retiro

        SELECT desc_beneficio
        INTO   ga_solicitud[i].desc_beneficio
        FROM   tab_beneficio_issste
        WHERE  tipo_beneficio = ga_solicitud[i].tipo_beneficio

        SELECT docprob_desc
        INTO   ga_solicitud[i].docprob_desc
        FROM   tab_doc_prob
        WHERE  docprob_cod = ga_solicitud[i].docprob_cod

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_status A
        WHERE  A.status = ga_solicitud[i].estado_solicitud

        LET i = i + 1
    END FOREACH

    ERROR ""
    IF (i-1) >= 1 THEN
         CALL SET_COUNT(i-1)

         DISPLAY ARRAY ga_solicitud TO scr_1.*

             ON KEY ( CONTROL-C )
                LET int_flag=TRUE
                EXIT DISPLAY

             ON KEY ( INTERRUPT )
                LET int_flag=TRUE
                EXIT DISPLAY

             ON KEY ( CONTROL-M )
                LET pos = ARR_CURR()
                EXIT DISPLAY

         END DISPLAY
    ELSE
         ERROR "    NO EXISTE REGISTRO "
         ATTRIBUTE(REVERSE)
         SLEEP 2
         ERROR ""
         CLOSE WINDOW retm9015
         RETURN
    END IF

    IF int_flag = TRUE THEN
	 LET int_flag = FALSE
         CLOSE WINDOW retm9015
         RETURN
    END IF

    CALL construccion(ga_solicitud[pos].*,sw_2) #co

CLOSE WINDOW retm9015
END FUNCTION


FUNCTION construccion(reg_mod,sw_3)
#co--------------------------------
    DEFINE  reg_mod        RECORD #loc #reg_mod
        nss_imss           LIKE ret_sol_issste_tot.nss_imss       ,
        nss_issste         LIKE ret_sol_issste_tot.nss_issste     ,
        curp               LIKE ret_sol_issste_tot.curp           ,
        rfc                LIKE afi_mae_afiliado.n_rfc            ,
        nombre_afore       LIKE afi_mae_afiliado.nombres          ,
        paterno_afore      LIKE afi_mae_afiliado.paterno          ,
        materno_afore      LIKE afi_mae_afiliado.materno          ,
        fecha_nac          LIKE ret_sol_issste_tot.fecha_nac      ,
        tipo_retiro        LIKE ret_sol_issste_tot.tipo_retiro    ,
        desc_retiro        CHAR(40)                               ,
        tipo_beneficio     LIKE ret_sol_issste_tot.tipo_beneficio ,
        desc_beneficio     CHAR(40)                               ,
        n_pensionista      LIKE ret_sol_issste_tot.n_pensionista  ,
        fecha_ini_pen      LIKE ret_sol_issste_tot.fecha_ini_pen  ,
        docprob_cod        LIKE ret_sol_issste_tot.cve_doc_prob   ,
        docprob_desc       CHAR(40)                               ,
        fec_sol_susp       LIKE ret_sol_issste_tot.fec_sol_susp   ,
        fec_sol_reac       LIKE ret_sol_issste_tot.fec_sol_reac   ,
        fecha_baja         LIKE ret_sol_issste_tot.fecha_baja     ,
        fecha_solicitud    LIKE ret_sol_issste_tot.fecha_solicitud,
        entidad_tramite    LIKE ret_sol_issste_tot.entidad_tramite,
        tipo_inversion     LIKE ret_sol_issste_tot.tipo_inversion ,
        fecha_val_ahorro   LIKE ret_sol_issste_tot.fecha_val_ahorro ,
        porcentaje_ahorro  LIKE ret_sol_issste_tot.porcentaje_ahorro,
        n_oficio           LIKE ret_sol_issste_tot.n_oficio         ,
        fecha_oficio       LIKE ret_sol_issste_tot.fecha_oficio     ,
        entidad_oficio     LIKE ret_sol_issste_tot.entidad_oficio   ,
        porcentaje_fon_viv LIKE ret_sol_issste_tot.porcentaje_fon_viv,
        rfc_dependencia    LIKE ret_sol_issste_tot.rfc_dependencia   ,
        nom_dependencia    LIKE ret_sol_issste_tot.nom_dependencia   ,
        cve_ramo           LIKE ret_sol_issste_tot.cve_ramo          ,
        cve_pagaduria      LIKE ret_sol_issste_tot.cve_pagaduria     ,
        fecha_val_viv      LIKE ret_sol_issste_tot.fecha_val_viv    ,
        fecha_captura      DATE                                      ,
        estado_solicitud   LIKE ret_sol_issste_tot.estado_solicitud  ,
        descripcion_status CHAR(25)                                  ,
        consecutivo        LIKE ret_sol_issste_tot.consecutivo       ,
        folio              LIKE ret_sol_issste_tot.folio
    END RECORD

    DEFINE #loc #smallint
        sw_3               SMALLINT

    LET rg_dato.nss_imss           =   reg_mod.nss_imss
    LET rg_dato.nss_issste         =   reg_mod.nss_issste
    LET rg_dato.curp               =   reg_mod.curp
    LET rg_dato.rfc                =   reg_mod.rfc
    LET rg_dato.nombre_afore       =   reg_mod.nombre_afore
    LET rg_dato.paterno_afore      =   reg_mod.paterno_afore
    LET rg_dato.materno_afore      =   reg_mod.materno_afore
    LET rg_dato.fecha_nac          =   reg_mod.fecha_nac
    LET rg_dato.tipo_retiro        =   reg_mod.tipo_retiro
    LET rg_dato.desc_retiro        =   reg_mod.desc_retiro
    LET rg_dato.tipo_beneficio     =   reg_mod.tipo_beneficio
    LET rg_dato.desc_beneficio     =   reg_mod.desc_beneficio
    LET rg_dato.n_pensionista      =   reg_mod.n_pensionista
    LET rg_dato.fecha_ini_pen      =   reg_mod.fecha_ini_pen
    LET rg_dato.docprob_cod        =   reg_mod.docprob_cod
    LET rg_dato.docprob_desc       =   reg_mod.docprob_desc
    LET rg_dato.fec_sol_susp       =   reg_mod.fec_sol_susp
    LET rg_dato.fec_sol_reac       =   reg_mod.fec_sol_reac
    LET rg_dato.fecha_baja         =   reg_mod.fecha_baja
    LET rg_dato.fecha_solicitud    =   reg_mod.fecha_solicitud
    LET rg_dato.entidad_tramite    =   reg_mod.entidad_tramite
    LET rg_dato.tipo_inversion     =   reg_mod.tipo_inversion
    LET rg_dato.fecha_val_ahorro   =   reg_mod.fecha_val_ahorro
    LET rg_dato.porcentaje_ahorro  =   reg_mod.porcentaje_ahorro
    LET rg_dato.n_oficio           =   reg_mod.n_oficio
    LET rg_dato.fecha_oficio       =   reg_mod.fecha_oficio
    LET rg_dato.entidad_oficio     =   reg_mod.entidad_oficio
    LET rg_dato.porcentaje_fon_viv =   reg_mod.porcentaje_fon_viv
    LET rg_dato.rfc_dependencia    =   reg_mod.rfc_dependencia
    LET rg_dato.nom_dependencia    =   reg_mod.nom_dependencia
    LET rg_dato.cve_ramo           =   reg_mod.cve_ramo
    LET rg_dato.cve_pagaduria      =   reg_mod.cve_pagaduria
    LET rg_dato.fecha_val_viv      =   reg_mod.fecha_val_viv
    LET rg_dato.fecha_captura      =   reg_mod.fecha_captura
    LET rg_dato.estado_solicitud   =   reg_mod.estado_solicitud
    LET rg_dato.descripcion_status =   reg_mod.descripcion_status
    LET rg_dato.consecutivo        =   reg_mod.consecutivo
    LET rg_dato.folio              =   reg_mod.folio

    IF sw_3  = 2  THEN
        DISPLAY " ESC : Consulta               ENTER : CONFIRMAR            CONTROL-C : SALIR   " AT 2,1
    END IF

    WHENEVER ERROR STOP

     CASE reg_mod.tipo_retiro
      WHEN 1       --pantalla del tipo de retiro 1
         CALL retiro_tipo_1 ("m")  #r1#
      WHEN 2       --pantalla del tipo de retiro 2
         CALL retiro_tipo_2 ("m")  #r2#
      WHEN 3
         CALL retiro_tipo_3_6 ("m")#r3#
      WHEN 6  --pantalla del tipo de retiro 3 y 6
         CALL retiro_tipo_3_6 ("m")#r3#
      WHEN 7       --pantalla del tipo de retiro 7
         CALL retiro_tipo_7 ("m")  #r7#
      WHEN 8       --pantalla del tipo de retiro 8
         CALL retiro_tipo_8 ("m")  #r8#
      WHEN 9       --pantalla del tipo de retiro 9
         CALL retiro_tipo_9 ("m")  #r9#
    END CASE

    LET reg_mod.tipo_beneficio     =   rg_dato.tipo_beneficio
    LET reg_mod.desc_beneficio     =   rg_dato.desc_beneficio
    LET reg_mod.n_pensionista      =   rg_dato.n_pensionista
    LET reg_mod.fecha_ini_pen      =   rg_dato.fecha_ini_pen
    LET reg_mod.docprob_cod        =   rg_dato.docprob_cod
    LET reg_mod.docprob_desc       =   rg_dato.docprob_desc
    LET reg_mod.fec_sol_susp       =   rg_dato.fec_sol_susp
    LET reg_mod.fec_sol_reac       =   rg_dato.fec_sol_reac
    LET reg_mod.fecha_baja         =   rg_dato.fecha_baja
    LET reg_mod.fecha_solicitud    =   rg_dato.fecha_solicitud
    LET reg_mod.entidad_tramite    =   rg_dato.entidad_tramite
    LET reg_mod.tipo_inversion     =   rg_dato.tipo_inversion
    LET reg_mod.fecha_val_ahorro   =   rg_dato.fecha_val_ahorro
    LET reg_mod.porcentaje_ahorro  =   rg_dato.porcentaje_ahorro
    LET reg_mod.n_oficio           =   rg_dato.n_oficio
    LET reg_mod.fecha_oficio       =   rg_dato.fecha_oficio
    LET reg_mod.entidad_oficio     =   rg_dato.entidad_oficio
    LET reg_mod.porcentaje_fon_viv =   rg_dato.porcentaje_fon_viv
    LET reg_mod.fecha_val_viv      =   rg_dato.fecha_val_viv

    IF int_flag = TRUE THEN
        ERROR "PROCESO CANCELADO"
        LET int_flag = FALSE
        RETURN
    END IF

    IF sw_3 = 2    THEN
     --CURRENT WINDOW IS retm9015
       WHILE TRUE
           PROMPT " DESEA CONFIRMAR LA CAPTURA S/N ? " FOR opc
           IF opc NOT MATCHES "[SsNn]" THEN
              CONTINUE WHILE
           ELSE
              EXIT WHILE
           END IF
       END WHILE

       IF opc MATCHES "[Ss]"  THEN
           UPDATE ret_sol_issste_tot
           SET    estado_solicitud = reg_1.confirmado         ,
                  fecha_confirma   = TODAY                    ,
                  usuario_confirma = usuario
           WHERE  nss_imss         = reg_mod.nss_imss
           AND    consecutivo      = reg_mod.consecutivo

           IF SQLCA.SQLCODE < 0 THEN
	      LET x_error = " UPDATE ret_sol_issste_tot:",
			         " nss ",reg_mod.nss_imss,
			         " consecutivo ",reg_mod.consecutivo,
			         err_get(SQLCA.SQLCODE)

                  CALL errorlog(x_error CLIPPED)

	          PROMPT " ERROR DE UPDATE ret_sol_issste_tot AVISE A SISTEMAS "
	          FOR enter
	          EXIT PROGRAM
           END IF

           ERROR " REGISTRO CONFIRMADO "  ATTRIBUTE(REVERSE)
           SLEEP 2
           --INITIALIZE reg_mod.* TO NULL
           CALL inicializa()
        ELSE
           ERROR " CONFIRMACION CANCELADA "  ATTRIBUTE(REVERSE)
           SLEEP 2
           --INITIALIZE reg_mod.* TO NULL
        END IF
        CASE reg_mod.tipo_retiro
             WHEN 1       --pantalla del tipo de retiro 1
                  CLOSE WINDOW retm9011_1
             WHEN 2       --pantalla del tipo de retiro 2
                  CLOSE WINDOW retm9011_2
             WHEN 3
                  CLOSE WINDOW retm9011_3
             WHEN 6  --pantalla del tipo de retiro 3 y 6
                  CLOSE WINDOW retm9011_3
             WHEN 7       --pantalla del tipo de retiro 7
                  CLOSE WINDOW retm9011_7
             WHEN 8       --pantalla del tipo de retiro 8
                  CLOSE WINDOW retm9011_8
             WHEN 9       --pantalla del tipo de retiro 9
                  CLOSE WINDOW retm9011_9
        END CASE
        INITIALIZE reg_mod.* TO NULL
        CURRENT WINDOW IS retm9015
    ELSE
        WHILE TRUE
           --CURRENT WINDOW IS retm9013

           PROMPT " DESEAS ACTUALIZARLA   S/N ? " FOR opc
           IF opc NOT MATCHES "[SsNn]" THEN
              CONTINUE WHILE
           ELSE
              EXIT WHILE
           END IF
        END WHILE

        IF opc MATCHES "[Ss]"  THEN
            UPDATE ret_sol_issste_tot
            SET    tipo_retiro      = reg_mod.tipo_retiro      ,
                   tipo_beneficio   = reg_mod.tipo_beneficio   ,
                   n_pensionista    = reg_mod.n_pensionista    ,
                   fecha_ini_pen    = reg_mod.fecha_ini_pen    ,
                   cve_doc_prob     = reg_mod.docprob_cod      ,
                   fec_sol_susp     = reg_mod.fec_sol_susp     ,
                   fec_sol_reac     = reg_mod.fec_sol_reac     ,
                   fecha_baja       = reg_mod.fecha_baja       ,
                   entidad_tramite  = reg_mod.entidad_tramite  ,
                   tipo_inversion   = reg_mod.tipo_inversion   ,
                   fecha_val_ahorro = reg_mod.fecha_val_ahorro ,
                   porcentaje_ahorro= reg_mod.porcentaje_ahorro,
                   n_oficio         = reg_mod.n_oficio         ,
                   fecha_oficio     = reg_mod.fecha_oficio     ,
                   entidad_oficio   = reg_mod.entidad_oficio   ,
                   porcentaje_fon_viv= reg_mod.porcentaje_fon_viv,
                   rfc_dependencia   = reg_mod.rfc_dependencia   ,
                   nom_dependencia   = reg_mod.nom_dependencia   ,
                   cve_ramo          = reg_mod.cve_ramo          ,
                   cve_pagaduria     = reg_mod.cve_pagaduria     ,
                   fecha_val_viv     = reg_mod.fecha_val_viv     ,
                   fecha_modifica    = TODAY                     ,
                   usuario_modifica  = usuario
            WHERE  nss_imss    = reg_mod.nss_imss
            AND    consecutivo = reg_mod.consecutivo

            ERROR " REGISTRO MODIFICADO "
            ATTRIBUTE(REVERSE) SLEEP 2

            --INITIALIZE reg_mod.* TO NULL
            CALL inicializa()
        ELSE
            ERROR " ACTUALIZACION CANCELADA "  ATTRIBUTE(REVERSE)
            SLEEP 2
            --INITIALIZE reg_mod.* TO NULL
        END IF
        CASE reg_mod.tipo_retiro
             WHEN 1       --pantalla del tipo de retiro 1
                  CLOSE WINDOW retm9011_1
             WHEN 2       --pantalla del tipo de retiro 2
                  CLOSE WINDOW retm9011_2
             WHEN 3
                  CLOSE WINDOW retm9011_3
             WHEN 6  --pantalla del tipo de retiro 3 y 6
                  CLOSE WINDOW retm9011_3
             WHEN 7       --pantalla del tipo de retiro 7
                  CLOSE WINDOW retm9011_7
             WHEN 8       --pantalla del tipo de retiro 8
                  CLOSE WINDOW retm9011_8
             WHEN 9       --pantalla del tipo de retiro 9
                  CLOSE WINDOW retm9011_9
        END CASE
        INITIALIZE reg_mod.* TO NULL
    END IF
END FUNCTION


FUNCTION cat_tipo_ret()
#ctr-------------------
    DEFINE c_des_tipo_ret LIKE tab_retiro_issste.desc_retiro

    SELECT desc_retiro
    INTO   c_des_tipo_ret
    FROM   tab_retiro_issste
    WHERE  tipo_retiro = rg_dato.tipo_retiro

    RETURN c_des_tipo_ret
END FUNCTION


FUNCTION despliega_tipo_retiro()
#dtr----------------------------
    DEFINE ra_reg ARRAY[100] OF RECORD
        codigo                CHAR(02) ,
        descripcion           CHAR(50)
    END RECORD

    DEFINE #loc #char
        prepare_1             CHAR(200) ,
        x_buscar              CHAR(030)

    OPEN WINDOW retm9016 AT 05,12 WITH FORM "RETM9014" ATTRIBUTE(BORDER)
    DISPLAY "                    TIPO DE RETIRO ISSSTE                      " AT 2,1  ATTRIBUTE(REVERSE)
    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
                ERROR "    DESCRIPCION A BUSCAR NO PUEDE SER NULA"
                ATTRIBUTE(NORMAL)
                NEXT FIELD x_buscar
            ELSE
                EXIT INPUT
            END IF

    END INPUT

    WHILE TRUE
        LET prepare_1 = " SELECT * FROM tab_retiro_issste ",
                        " WHERE desc_retiro MATCHES ",'"',x_buscar CLIPPED,'"',
                        " AND   tipo_retiro NOT IN (4,5) ",
                        " ORDER BY 1 " CLIPPED

        PREPARE pre_7 FROM prepare_1
        DECLARE cur_7 CURSOR FOR pre_7

        LET pos = 1

        FOREACH cur_7 INTO ra_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                ATTRIBUTE(NORMAL)
                EXIT FOREACH
            END IF
        END FOREACH

        IF (pos-1) < 1 THEN
            ERROR "    ARCHIVO TIPO DE RETIRO VACIO"
            ATTRIBUTE(NORMAL)
        END IF

        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY ra_reg TO scr_2.*
            ON KEY ( CONTROL-C )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( INTERRUPT )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( CONTROL-M )
                LET pos = ARR_CURR()
                LET rg_dato.tipo_retiro = ra_reg[pos].codigo
                LET rg_dato.desc_retiro = ra_reg[pos].descripcion

                EXIT DISPLAY
        END DISPLAY

        IF pos <> 0 THEN
            EXIT WHILE
        END IF
    END WHILE
    CLOSE WINDOW retm9016
END FUNCTION


FUNCTION despliega_doc_probatorio()
#ddp----------------------------
    DEFINE  ra_reg ARRAY[100] OF RECORD
        codigo            CHAR(02),
        descripcion       CHAR(50)
    END RECORD

    DEFINE   prepare_1    CHAR(200),
             x_buscar     CHAR(030)

    OPEN WINDOW retm9016 AT 05,12 WITH FORM "RETM9014" ATTRIBUTE(BORDER)
    DISPLAY "                  TIPO DE DOCUMENTO PROBATORIO               " AT 2,1  ATTRIBUTE(REVERSE)
    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
                ERROR "    DESCRIPCION A BUSCAR NO PUEDE SER NULA"
                ATTRIBUTE(NORMAL)
                NEXT FIELD x_buscar
            ELSE
                EXIT INPUT
            END IF

    END INPUT

    WHILE TRUE
        LET prepare_1 = " SELECT * FROM tab_doc_prob ",
                        " WHERE docprob_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                        " AND   docprob_cod IN (1,2,3,4,7) ",
                        " ORDER BY 1 " CLIPPED

        PREPARE pre_5 FROM prepare_1
        DECLARE cur_5 CURSOR FOR pre_5

        LET pos = 1

        FOREACH cur_5 INTO ra_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                ATTRIBUTE(NORMAL)
                EXIT FOREACH
            END IF
        END FOREACH

        IF (pos-1) < 1 THEN
            ERROR "    ARCHIVO TIPO DE RETIRO VACIO"
            ATTRIBUTE(NORMAL)
        END IF

        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY ra_reg TO scr_2.*
            ON KEY ( CONTROL-C )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( INTERRUPT )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( CONTROL-M )
                LET pos = ARR_CURR()
                LET rg_dato.docprob_cod   = ra_reg[pos].codigo
                LET rg_dato.docprob_desc  = ra_reg[pos].descripcion
                EXIT DISPLAY

        END DISPLAY

        IF pos <> 0 THEN
            EXIT WHILE
        END IF
    END WHILE
    CLOSE WINDOW retm9016
END FUNCTION


FUNCTION despliega_tipo_beneficio()
#dtb----------------------------
    DEFINE  ra_reg ARRAY[100] OF RECORD
        codigo            CHAR(03),
        descripcion       CHAR(50)
    END RECORD

    DEFINE   prepare_1    CHAR(200),
             x_buscar     CHAR(030)

    OPEN WINDOW retm9016 AT 05,12 WITH FORM "RETM9014" ATTRIBUTE(BORDER)
    DISPLAY "                 TIPO DE BENEFICIO ISSSTE               " AT 2,1  ATTRIBUTE(REVERSE)
    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
                ERROR "    DESCRIPCION A BUSCAR NO PUEDE SER NULA"
                ATTRIBUTE(NORMAL)
                NEXT FIELD x_buscar
            ELSE
                EXIT INPUT
            END IF
    END INPUT

    WHILE TRUE
       LET prepare_1= " SELECT * FROM tab_beneficio_issste ",
                      " WHERE desc_beneficio MATCHES ",'"',x_buscar CLIPPED,'"',
                      " ORDER BY 1 " CLIPPED

       PREPARE pre_6 FROM prepare_1
       DECLARE cur_6 CURSOR FOR pre_6

       LET pos = 1

       FOREACH cur_6 INTO ra_reg[pos].*
           LET pos = pos + 1
           IF pos >= 1000 THEN
               ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
               ATTRIBUTE(NORMAL)
               EXIT FOREACH
           END IF
       END FOREACH

       IF (pos-1) < 1 THEN
           ERROR "    ARCHIVO TIPO DE RETIRO VACIO"
           ATTRIBUTE(NORMAL)
       END IF

       CALL SET_COUNT(pos-1)
       DISPLAY ARRAY ra_reg TO scr_2.*
           ON KEY ( CONTROL-C )
               LET pos = 0
               EXIT DISPLAY

           ON KEY ( INTERRUPT )
               LET pos = 0
               EXIT DISPLAY

           ON KEY ( CONTROL-M )
               LET pos = ARR_CURR()
               LET rg_dato.tipo_beneficio   = ra_reg[pos].codigo
               LET rg_dato.desc_beneficio   = ra_reg[pos].descripcion
               EXIT DISPLAY

       END DISPLAY

       IF pos <> 0 THEN
           EXIT WHILE
       END IF
    END WHILE
    CLOSE WINDOW retm9016
END FUNCTION

FUNCTION despliega_issste(sw10)
#dtr----------------------------
    DEFINE ra_reg ARRAY[100] OF RECORD
        codigo                CHAR(02) ,
        descripcion           CHAR(50)
    END RECORD

    DEFINE #loc #char
        prepare_1             CHAR(200) ,
        x_buscar              CHAR(030)

    DEFINE #loc #smallint
        sw10                  SMALLINT

    OPEN WINDOW retm9016 AT 05,12 WITH FORM "RETM9014" ATTRIBUTE(BORDER)
    DISPLAY "                    TIPO DE RETIRO ISSSTE                      " AT 2,1  ATTRIBUTE(REVERSE)
    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
                ERROR "    DESCRIPCION A BUSCAR NO PUEDE SER NULA"
                ATTRIBUTE(NORMAL)
                NEXT FIELD x_buscar
            ELSE
                EXIT INPUT
            END IF

    END INPUT

    WHILE TRUE
        LET prepare_1 = " SELECT cvedelissste, desc_issste FROM tab_issste ",
                        " WHERE desc_issste MATCHES ",'"',x_buscar CLIPPED,'"',
                        " ORDER BY 1 " CLIPPED

        PREPARE pre_9 FROM prepare_1
        DECLARE cur_9 CURSOR FOR pre_9

        LET pos = 1

        FOREACH cur_9 INTO ra_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                ATTRIBUTE(NORMAL)
                EXIT FOREACH
            END IF
        END FOREACH

        IF (pos-1) < 1 THEN
            ERROR "    ARCHIVO DELEG. ISSSTE VACIO"
            ATTRIBUTE(NORMAL)
        END IF

        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY ra_reg TO scr_2.*
            ON KEY ( CONTROL-C )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( INTERRUPT )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( CONTROL-M )
                LET pos = ARR_CURR()
                IF sw10 = 1 THEN
                   LET rg_dato.entidad_oficio = ra_reg[pos].codigo
                   --LET rg_dato.desc_retiro = ra_reg[pos].descripcion
                ELSE
                   LET rg_dato.entidad_tramite = ra_reg[pos].codigo
                END IF

                EXIT DISPLAY
        END DISPLAY

        IF pos <> 0 THEN
            EXIT WHILE
        END IF
    END WHILE
    CLOSE WINDOW retm9016
END FUNCTION

FUNCTION marca_cuenta(vl_nss,vl_marca_ent,vl_consecutivo)
#mc------------------------------------------------------
    DEFINE #loc #smallint
        vl_marca_ent                     ,
        vl_marca_res                     ,
        vl_convive_cod                   ,
        vl_cod_rechazo                   SMALLINT

    DEFINE #loc #char
        vl_nss                           CHAR(011)

    DEFINE #loc #integer
        vl_consecutivo                   INTEGER


    DEFINE #loc #reg_20
        reg_20 RECORD
              estado_marca          SMALLINT,
              codigo_rechazo        SMALLINT,
              marca_causa           SMALLINT,
              fecha_causa           DATE
    END RECORD


    LET reg_20.estado_marca   = 0
    LET reg_20.codigo_rechazo = 0
    LET reg_20.marca_causa    = 0
    INITIALIZE reg_20.fecha_causa TO NULL

    WHENEVER ERROR CONTINUE
    DECLARE cur_sp CURSOR FOR eje_marca
    OPEN cur_sp USING vl_nss                , # nss
                      vl_marca_ent          , # marca entrante
                      vl_consecutivo        , # correlativo
                      reg_20.estado_marca   , # estado_marca
                      reg_20.codigo_rechazo , # codigo de rechazo
                      reg_20.marca_causa    , # marca_causa
                      reg_20.fecha_causa    , # fecha_causa
                      usuario                 # usuario

     FETCH cur_sp INTO vl_marca_res   , # misma marca si convive o
                      vl_cod_rechazo    # marca activa que rechaza
                                        # codigo de rechazo

     CLOSE cur_sp
     RETURN vl_marca_res,
            vl_cod_rechazo
     WHENEVER ERROR STOP
END FUNCTION

FUNCTION retiro_tipo_1(bnd)
#rt1-----------------------
    DEFINE
        tip_solic             LIKE afi_mae_afiliado.tipo_solicitud      ,
        desc_solic            LIKE tab_tipo_solic.desc_solicitud        ,
        tip_trab              LIKE cta_ctr_reg_ind.tipo_trab_ind        ,
        desc_trab             LIKE tab_tipo_trab_ind.desc_tipo_trab_ind ,
        bnd                   CHAR (01)

    LET rg_dato.fecha_solicitud = HOY
    LET ventana                 = "retm9011_1"

    INITIALIZE ult_consecutivo TO NULL

    IF bnd = "a" THEN
        OPEN WINDOW retm9011_1 AT 2,2 WITH FORM "RETM9011_1" ATTRIBUTE (BORDER)
        DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
        DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)

        DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 10,1 ATTRIBUTE(REVERSE)
    ELSE
        OPEN WINDOW retm9011_1 AT 2,2 WITH FORM "RETM9013_1" ATTRIBUTE(BORDER)

        DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
        DISPLAY HOY USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)

        DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 8,1 ATTRIBUTE(REVERSE)
        DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 17,1 ATTRIBUTE(REVERSE)
        DISPLAY " ESC : Consulta               ENTER : MODIFICAR            CONTROL-C : SALIR   " AT 2,1
    END IF

    IF bnd = "a" THEN
        CALL captura_datos_generales() #cdg#
        IF INT_FLAG = 1 THEN
           --CLOSE WINDOW retm9011_1
           RETURN
        END IF
    ELSE
       DISPLAY rg_dato.nss_imss           TO  nss_imss
       DISPLAY rg_dato.nss_issste         TO  nss_issste
       DISPLAY rg_dato.curp               TO  curp
       DISPLAY rg_dato.rfc                TO  rfc
       DISPLAY rg_dato.nombre_afore       TO  nombre_afore
       DISPLAY rg_dato.paterno_afore      TO  paterno_afore
       DISPLAY rg_dato.materno_afore      TO  materno_afore
       DISPLAY rg_dato.fecha_nac          TO  fecha_nac
       DISPLAY rg_dato.fecha_captura      TO  fecha_captura
       DISPLAY rg_dato.estado_solicitud   TO  estado_solicitud
       DISPLAY rg_dato.descripcion_status TO  descripcion_status
       DISPLAY rg_dato.consecutivo        TO  consecutivo
       DISPLAY rg_dato.folio              TO  folio
    END IF

    INPUT BY NAME rg_dato.tipo_retiro       ,rg_dato.desc_retiro        ,rg_dato.tipo_beneficio    ,
                  rg_dato.desc_beneficio    ,rg_dato.docprob_cod        ,rg_dato.docprob_desc      ,
                  rg_dato.fecha_solicitud   ,rg_dato.fecha_val_ahorro  ,
                  rg_dato.n_oficio           ,rg_dato.fecha_oficio      ,
                  rg_dato.entidad_oficio    ,rg_dato.fecha_val_viv
                  WITHOUT DEFAULTS

       AFTER FIELD tipo_beneficio
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
              NEXT FIELD tipo_retiro
          END IF

          IF rg_dato.tipo_beneficio IS NULL THEN
             CALL despliega_tipo_beneficio()  #dtb
             DISPLAY rg_dato.tipo_beneficio TO tipo_beneficio
             DISPLAY rg_dato.desc_beneficio TO desc_beneficio
          ELSE
             SELECT "OK"
             FROM   tab_beneficio_issste
             WHERE  tipo_beneficio = rg_dato.tipo_beneficio

             IF STATUS <> NOTFOUND THEN
                SELECT desc_beneficio
                INTO   rg_dato.desc_beneficio
                FROM   tab_beneficio_issste
                WHERE  tipo_beneficio = rg_dato.tipo_beneficio

                DISPLAY rg_dato.desc_beneficio TO desc_beneficio
             ELSE
                ERROR " TIPO DE BENEFICIO ISSSTE INEXISTENTE ... "
                NEXT FIELD tipo_beneficio
             END IF
          END IF

          IF rg_dato.tipo_retiro = 1 AND rg_dato.tipo_beneficio <> 65 THEN
             ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
             NEXT FIELD tipo_beneficio
          END IF
          IF rg_dato.tipo_beneficio = 65 AND rg_dato.tipo_retiro <> 1 THEN
             ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
             NEXT FIELD tipo_beneficio
          END IF

       BEFORE FIELD docprob_cod
          IF rg_dato.tipo_retiro <> 1 AND rg_dato.tipo_beneficio <> 65 THEN
             NEXT FIELD tipo_beneficio
          END IF

      AFTER FIELD docprob_cod
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
              NEXT FIELD tipo_beneficio
          END IF

          IF rg_dato.docprob_cod IS NULL OR
             rg_dato.docprob_cod = 0     THEN
             CALL despliega_doc_probatorio()  #ddp
             DISPLAY rg_dato.docprob_cod  TO docprob_cod
             DISPLAY rg_dato.docprob_desc TO docprob_desc
          ELSE
             SELECT "OK"
             FROM   tab_doc_prob
             WHERE  docprob_cod = rg_dato.docprob_cod
             AND    docprob_cod IN ( 1,2,3,4,7)

             IF STATUS <> NOTFOUND THEN
                SELECT docprob_desc
                INTO   rg_dato.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = rg_dato.docprob_cod

                DISPLAY rg_dato.docprob_desc TO docprob_desc
             ELSE
                ERROR " TIPO DE DOCUMENTO PROBATORIO INEXISTENTE ... "
                NEXT FIELD docprob_cod
             END IF
          END IF

          IF rg_dato.tipo_beneficio = 65 AND rg_dato.docprob_cod IS NULL THEN
             ERROR " DEBE ELEGIR UN DOCUMENTO PROBATORIO PARA ESTE RETIRO... "
             NEXT FIELD docprob_cod
          END IF

       AFTER FIELD fecha_val_ahorro
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD docprob_cod
          END IF
          IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro = "" THEN
             ERROR "LA FECHA VALOR DE AHORRO NO DEBE SER NULA "
             NEXT FIELD fecha_val_ahorro
          END IF
          { IF rg_dato.fecha_val_ahorro > HOY THEN
              ERROR "LA FECHA VALOR DE AHORRO NO DEBE SER MAYOR A LA FECHA ",
                    "DEL DIA " ATTRIBUTE(REVERSE)
              NEXT FIELD fecha_val_ahorro
          END IF }

        AFTER FIELD fecha_oficio
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD n_oficio
          END IF
          IF rg_dato.fecha_oficio IS NULL OR rg_dato.fecha_oficio = "" THEN
             ERROR "LA FECHA DEL OFICIO NO DEBE SER NULA "
             NEXT FIELD fecha_oficio
          END IF
          IF rg_dato.fecha_oficio > HOY THEN
             ERROR "LA FECHA DEL OFICIO NO DEBE SER MAYOR A LA FECHA ",
                   "DEL DIA " ATTRIBUTE(REVERSE)
             NEXT FIELD fecha_oficio
          ELSE
             IF rg_dato.fecha_oficio IS NULL THEN
                ERROR "DEBE TECLEAR LA FECHA DEL OFICIO ... " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_oficio
             END IF
          END IF

       AFTER FIELD n_oficio
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD docprob_cod
          END IF
          IF rg_dato.n_oficio IS NULL THEN
             ERROR " DEBE CAPTURAR EL NUMERO DE OFICIO ... "
             NEXT FIELD n_oficio
          END IF

       AFTER FIELD entidad_oficio
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
              NEXT FIELD n_oficio
          END IF

          IF rg_dato.entidad_oficio IS NULL OR
             rg_dato.entidad_oficio = 0     THEN
             CALL despliega_issste(1)  #ddp
             DISPLAY rg_dato.entidad_oficio  TO entidad_oficio
             --DISPLAY rg_dato.docprob_desc TO docprob_desc
          ELSE
             SELECT "OK"
             FROM   tab_issste
             WHERE  cvedelissste = rg_dato.entidad_oficio

             IF STATUS <> NOTFOUND THEN
                {SELECT docprob_desc
                INTO   rg_dato.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = rg_dato.docprob_cod

                DISPLAY rg_dato.docprob_desc TO docprob_desc}
             ELSE
                ERROR " ENTIDAD OFICIO NO EXISTENTE                  "
                NEXT FIELD entidad_oficio
             END IF
          END IF
          IF rg_dato.entidad_oficio IS NULL THEN
             ERROR " DEBE CAPTURAR LA ENTIDAD DE OFICIO ... "
             NEXT FIELD entidad_oficio
          END IF

       ON KEY ( ESC )

         IF rg_dato.tipo_beneficio IS NULL OR rg_dato.tipo_beneficio = "" THEN
            ERROR "EL TIPO BENEFICIARIO NO PUEDE ESTAR VACIO"
            NEXT FIELD tipo_beneficio
         END IF
         IF rg_dato.docprob_cod IS NULL OR rg_dato.docprob_cod = "" THEN
            ERROR "EL DOCUMENTO PROBATORIO NO PUEDE ESTAR VACIO"
            NEXT FIELD docprob_cod
         END IF
         IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro = "" THEN
            ERROR "LA FECHA VALOR DE AHORRO NO PUEDE ESTAR VACIA"
            NEXT FIELD fecha_val_ahorro
         END IF
         IF rg_dato.n_oficio IS NULL OR rg_dato.n_oficio = "" THEN
            ERROR "EL NUMERO DE OFICIO NO PUEDE ESTAR VACIO"
            NEXT FIELD n_oficio
         END IF
         IF rg_dato.fecha_oficio IS NULL OR rg_dato.fecha_oficio = "" THEN
            ERROR "LA FECHA DEL OFICIO NO PUEDE ESTAR VACIA"
            NEXT FIELD fecha_oficio
         END IF
         IF rg_dato.entidad_oficio IS NULL OR rg_dato.entidad_oficio = "" THEN
            ERROR "LA ENTIDAD DEL OFICIO NO ESTAR VACIA"
            NEXT FIELD entidad_oficio
         END IF

         IF bnd = "a" THEN
            IF inserta()THEN #inserta registros enla base de datos
               ERROR "REGISTRO INSERTADO"
	       EXIT INPUT
	    ELSE
	       ERROR ""
               EXIT INPUT
            END IF
         ELSE
            EXIT INPUT
         END IF

       ON KEY ( CONTROL-C )
            IF ult_consecutivo IS NOT NULL THEN
               DELETE FROM ret_consecutivo
               WHERE consecutivo = ult_consecutivo
            END IF
            EXIT INPUT

       ON KEY ( INTERRUPT )
            IF ult_consecutivo IS NOT NULL THEN
              DELETE FROM ret_consecutivo
              WHERE consecutivo = ult_consecutivo
            END IF
            EXIT INPUT

       END INPUT
       --CLOSE WINDOW retm9011_1
END FUNCTION
#####################################################################
##
##
##
#####################################################################
FUNCTION retiro_tipo_2(bnd)

   DEFINE tip_solic           LIKE afi_mae_afiliado.tipo_solicitud
   DEFINE desc_solic          LIKE tab_tipo_solic.desc_solicitud
   DEFINE tip_trab            LIKE cta_ctr_reg_ind.tipo_trab_ind
   DEFINE desc_trab           LIKE tab_tipo_trab_ind.desc_tipo_trab_ind
   DEFINE bnd                 CHAR(01)

   LET rg_dato.fecha_solicitud = HOY
   LET ventana = "retm9011_2"
   INITIALIZE ult_consecutivo TO NULL

   IF bnd = "a" THEN
      OPEN WINDOW retm9011_2 AT 2,2 WITH FORM "RETM9011_2" ATTRIBUTE (BORDER)
         DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)

         DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 10,1 ATTRIBUTE(REVERSE)
   ELSE
      OPEN WINDOW retm9011_2 AT 2,2 WITH FORM "RETM9013_2" ATTRIBUTE(BORDER)

         DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY HOY USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)

         DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 8,1 ATTRIBUTE(REVERSE)
         DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 ATTRIBUTE(REVERSE)
         DISPLAY " ESC : Consulta               ENTER : MODIFICAR            CONTROL-C : SALIR   " AT 2,1
   END IF

      IF bnd = "a" THEN
         CALL captura_datos_generales() #cdg#
         IF INT_FLAG = 1 THEN
            --CLOSE WINDOW retm9011_2
            RETURN
         END IF
      ELSE
         DISPLAY rg_dato.nss_imss           TO  nss_imss
         DISPLAY rg_dato.nss_issste         TO  nss_issste
         DISPLAY rg_dato.curp               TO  curp
         DISPLAY rg_dato.rfc                TO  rfc
         DISPLAY rg_dato.nombre_afore       TO  nombre_afore
         DISPLAY rg_dato.paterno_afore      TO  paterno_afore
         DISPLAY rg_dato.materno_afore      TO  materno_afore
         DISPLAY rg_dato.fecha_nac          TO  fecha_nac
         DISPLAY rg_dato.fecha_captura      TO  fecha_captura
         DISPLAY rg_dato.estado_solicitud   TO  estado_solicitud
         DISPLAY rg_dato.descripcion_status TO  descripcion_status
         DISPLAY rg_dato.consecutivo        TO  consecutivo
         DISPLAY rg_dato.folio              TO  folio
      END IF

      INPUT BY NAME rg_dato.tipo_retiro       ,rg_dato.desc_retiro        ,rg_dato.tipo_beneficio    ,
                    rg_dato.desc_beneficio    ,rg_dato.n_pensionista      ,rg_dato.fecha_ini_pen     ,
                    rg_dato.fecha_solicitud   ,rg_dato.entidad_tramite    ,
                    rg_dato.fecha_val_ahorro  ,rg_dato.n_oficio           ,
                    rg_dato.fecha_oficio      ,rg_dato.entidad_oficio     ,
                    rg_dato.fecha_val_viv     WITHOUT DEFAULTS

       AFTER FIELD tipo_beneficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_oficio
            END IF

            IF rg_dato.tipo_beneficio IS NULL THEN
               CALL despliega_tipo_beneficio()  #dtb
               DISPLAY rg_dato.tipo_beneficio TO tipo_beneficio
               DISPLAY rg_dato.desc_beneficio TO desc_beneficio
            ELSE
               SELECT "OK"
               FROM   tab_beneficio_issste
               WHERE  tipo_beneficio = rg_dato.tipo_beneficio

               IF STATUS <> NOTFOUND THEN
                  SELECT desc_beneficio
                  INTO   rg_dato.desc_beneficio
                  FROM   tab_beneficio_issste
                  WHERE  tipo_beneficio = rg_dato.tipo_beneficio

                  DISPLAY rg_dato.desc_beneficio TO desc_beneficio
               ELSE
                  ERROR " TIPO DE BENEFICIO ISSSTE INEXISTENTE ... "
                  NEXT FIELD tipo_beneficio
               END IF
            END IF

            IF rg_dato.tipo_retiro = 2 AND rg_dato.tipo_beneficio <> 101 AND
               rg_dato.tipo_beneficio<>102 AND rg_dato.tipo_beneficio<>103 AND
               rg_dato.tipo_beneficio<>104 AND rg_dato.tipo_beneficio<>105 AND
               rg_dato.tipo_beneficio<>634
            THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF
            IF (rg_dato.tipo_beneficio = 101 OR rg_dato.tipo_beneficio = 102 OR
                rg_dato.tipo_beneficio = 103 OR rg_dato.tipo_beneficio = 104 OR
                rg_dato.tipo_beneficio = 105 ) AND rg_dato.tipo_retiro <> 2 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF

         AFTER FIELD n_pensionista
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_beneficio
            END IF

            IF rg_dato.n_pensionista IS NULL
            THEN
               ERROR " EL NUMERO DE PENSIONISTA NO PUEDE SER NULO PARA ESTE TIPO DE RETIRO "
               NEXT FIELD n_pensionista
            END IF

         AFTER FIELD fecha_ini_pen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_pensionista
            END IF

            IF rg_dato.fecha_ini_pen IS NULL  THEN
               ERROR " LA FECHA DE INICIO DE PENSION NO PUEDE SER NULA PARA ESTE TIPO DE RETIRO "
               NEXT FIELD fecha_ini_pen
            END IF

            IF rg_dato.fecha_ini_pen > HOY THEN
                ERROR "LA FECHA INICIO DE PENSION NO DEBE SER MAYOR A LA FECHA",
                      " DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_ini_pen
            END IF

         AFTER FIELD entidad_tramite
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_ini_pen
            END IF

          IF rg_dato.entidad_tramite IS NULL OR
             rg_dato.entidad_tramite = 0     THEN
             CALL despliega_issste(2)  #ddp
             DISPLAY rg_dato.entidad_tramite  TO entidad_tramite
             --DISPLAY rg_dato.docprob_desc TO docprob_desc
          ELSE
             SELECT "OK"
             FROM   tab_issste
             WHERE  cvedelissste = rg_dato.entidad_tramite

             IF STATUS <> NOTFOUND THEN
                {SELECT docprob_desc
                INTO   rg_dato.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = rg_dato.docprob_cod

                DISPLAY rg_dato.docprob_desc TO docprob_desc}
             ELSE
                ERROR " ENTIDAD TRAMITE NO EXISTENTE      "
                NEXT FIELD entidad_tramite
             END IF
          END IF

            IF rg_dato.entidad_tramite IS NULL
            THEN
               ERROR " LA ENTIDAD TRAMITE ES UN CAMPO OBLIGATORIO PARA ESTE TIPO DE RETIRO "
               NEXT FIELD entidad_tramite
            END IF

         AFTER FIELD fecha_val_ahorro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD entidad_tramite
            END IF
            IF rg_dato.fecha_val_ahorro IS NULL OR  rg_dato.fecha_val_ahorro = "" THEN
               ERROR "LA FECHA VALOR DE AHORRO NO PUEDE ESTAR VACIA"
               NEXT FIELD fecha_val_ahorro
            END IF
            { IF rg_dato.fecha_val_ahorro > HOY THEN
                ERROR "LA FECHA VALOR DE AHORRO NO DEBE SER MAYOR A LA FECHA ",
                      "DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_val_ahorro
            END IF }

         AFTER FIELD n_oficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD fecha_ini_pen
            END IF
            IF rg_dato.n_oficio IS NULL THEN
               ERROR " DEBE CAPTURAR EL NUMERO DE OFICIO ... "
               NEXT FIELD n_oficio
            END IF

         AFTER FIELD fecha_oficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD fecha_val_ahorro
            END IF

            IF rg_dato.fecha_oficio > HOY THEN
               ERROR "LA FECHA DEL OFICIO NO DEBE SER MAYOR A LA FECHA ",
                     "DEL DIA " ATTRIBUTE(REVERSE)
               NEXT FIELD fecha_oficio
            ELSE
               IF rg_dato.fecha_oficio IS NULL THEN
                  ERROR "DEBE TECLEAR LA FECHA DEL OFICIO ... " ATTRIBUTE(REVERSE)
                  NEXT FIELD fecha_oficio
               END IF
            END IF

         AFTER FIELD entidad_oficio
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD n_oficio
          END IF
          IF rg_dato.entidad_oficio IS NULL OR
             rg_dato.entidad_oficio = 0     THEN
             CALL despliega_issste(1)  #ddp
             DISPLAY rg_dato.entidad_oficio  TO entidad_oficio
             --DISPLAY rg_dato.docprob_desc TO docprob_desc
          ELSE
             SELECT "OK"
             FROM   tab_issste
             WHERE  cvedelissste = rg_dato.entidad_oficio

             IF STATUS <> NOTFOUND THEN
                {SELECT docprob_desc
                INTO   rg_dato.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = rg_dato.docprob_cod

                DISPLAY rg_dato.docprob_desc TO docprob_desc}
             ELSE
                ERROR " ENTIDAD OFICIO NO EXISTENTE                  "
                NEXT FIELD entidad_oficio
             END IF
          END IF
            IF rg_dato.entidad_oficio IS NULL THEN
               ERROR " DEBE CAPTURAR LA ENTIDAD DE OFICIO ... "
               NEXT FIELD entidad_oficio
            END IF

        ON KEY ( ESC )
           IF rg_dato.tipo_beneficio IS NULL OR rg_dato.tipo_beneficio = "" THEN
              ERROR "EL TIPO BENEFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD tipo_beneficio
           END IF
           IF rg_dato.n_pensionista IS NULL OR rg_dato.n_pensionista = "" THEN
              ERROR "EL NUMERO DE PENSIONISTA NO PUEDE ESTAR VACIO"
              NEXT FIELD n_pensionista
           END IF
           IF rg_dato.fecha_ini_pen IS NULL OR rg_dato.fecha_ini_pen = "" THEN
              ERROR "LA FECHA DE INICIO DE PENSION NO PUEDE ESTAR VACIA"
              NEXT FIELD fecha_ini_pen
           END IF
           IF rg_dato.entidad_tramite IS NULL OR rg_dato.entidad_tramite = "" THEN
              ERROR "LA ENTIDAD DEL TRAMITE NO PUEDE ESTAR VACIA"
              NEXT FIELD entidad_tramite
           END IF
           IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro = "" THEN
              ERROR "LA FECHA VALOR DE AHORRO NO PUEDE ESTAR VACIA"
              NEXT FIELD fecha_val_ahorro
           END IF
           IF rg_dato.n_oficio IS NULL OR rg_dato.n_oficio = "" THEN
              ERROR "EL NUMERO DE OFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD n_oficio
           END IF
           IF rg_dato.fecha_oficio IS NULL OR rg_dato.fecha_oficio = "" THEN
              ERROR "EL NUMERO DE OFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD fecha_oficio
           END IF
           IF rg_dato.entidad_oficio IS NULL OR rg_dato.entidad_oficio = "" THEN
              ERROR "EL NUMERO DE OFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD entidad_oficio
           END IF

           IF bnd = "a" THEN
              IF inserta()THEN #inserta registros enla base de datos
                 ERROR "REGISTRO INSERTADO"
	         EXIT INPUT
	      ELSE
	         ERROR ""
                 EXIT INPUT
              END IF
           ELSE
              EXIT INPUT
           END IF

        ON KEY ( CONTROL-C )
              IF ult_consecutivo IS NOT NULL THEN
                 DELETE FROM ret_consecutivo
                 WHERE consecutivo = ult_consecutivo
              END IF
              EXIT INPUT

        ON KEY ( INTERRUPT )
              IF ult_consecutivo IS NOT NULL THEN
                DELETE FROM ret_consecutivo
                WHERE consecutivo = ult_consecutivo
              END IF
              EXIT INPUT

      END INPUT
      --CLOSE WINDOW retm9011_2
END FUNCTION
#####################################################################
##
##
##
#####################################################################
FUNCTION retiro_tipo_3_6(bnd)

   DEFINE tip_solic           LIKE afi_mae_afiliado.tipo_solicitud
   DEFINE desc_solic          LIKE tab_tipo_solic.desc_solicitud
   DEFINE tip_trab            LIKE cta_ctr_reg_ind.tipo_trab_ind
   DEFINE desc_trab           LIKE tab_tipo_trab_ind.desc_tipo_trab_ind
   DEFINE bnd                 CHAR(01)

   LET rg_dato.fecha_solicitud = HOY
   LET ventana = "retm9011_3"
   INITIALIZE ult_consecutivo TO NULL

   IF bnd  = "a" THEN
      OPEN WINDOW retm9011_3 AT 2,2 WITH FORM "RETM9011_3" ATTRIBUTE (BORDER)
         DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)

         DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 10,1 ATTRIBUTE(REVERSE)
   ELSE
      OPEN WINDOW retm9011_3 AT 2,2 WITH FORM "RETM9013_3" ATTRIBUTE(BORDER)

         DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY HOY USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)

         DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 8,1 ATTRIBUTE(REVERSE)
         DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
         DISPLAY " ESC : Consulta               ENTER : MODIFICAR            CONTROL-C : SALIR   " AT 2,1
   END IF

      IF bnd = "a" THEN
         CALL captura_datos_generales() #cdg#
         IF INT_FLAG = 1 THEN
            --CLOSE WINDOW retm9011_3
            RETURN
         END IF
      ELSE
         DISPLAY rg_dato.nss_imss           TO  nss_imss
         DISPLAY rg_dato.nss_issste         TO  nss_issste
         DISPLAY rg_dato.curp               TO  curp
         DISPLAY rg_dato.rfc                TO  rfc
         DISPLAY rg_dato.nombre_afore       TO  nombre_afore
         DISPLAY rg_dato.paterno_afore      TO  paterno_afore
         DISPLAY rg_dato.materno_afore      TO  materno_afore
         DISPLAY rg_dato.fecha_nac          TO  fecha_nac
         DISPLAY rg_dato.fecha_captura      TO  fecha_captura
         DISPLAY rg_dato.estado_solicitud   TO  estado_solicitud
         DISPLAY rg_dato.descripcion_status TO  descripcion_status
         DISPLAY rg_dato.consecutivo        TO  consecutivo
         DISPLAY rg_dato.folio              TO  folio
      END IF

      INPUT BY NAME rg_dato.tipo_retiro       ,rg_dato.desc_retiro        ,rg_dato.tipo_beneficio    ,
                    rg_dato.desc_beneficio    ,rg_dato.fecha_solicitud    ,
                    rg_dato.fecha_val_ahorro  ,rg_dato.n_oficio          ,
                    rg_dato.fecha_oficio      ,rg_dato.entidad_oficio     ,
                    rg_dato.fecha_val_viv     WITHOUT DEFAULTS

         AFTER FIELD tipo_beneficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_oficio
            END IF

            IF rg_dato.tipo_beneficio IS NULL THEN
               CALL despliega_tipo_beneficio()  #dtb
               DISPLAY rg_dato.tipo_beneficio TO tipo_beneficio
               DISPLAY rg_dato.desc_beneficio TO desc_beneficio
            ELSE
               SELECT "OK"
               FROM   tab_beneficio_issste
               WHERE  tipo_beneficio = rg_dato.tipo_beneficio

               IF STATUS <> NOTFOUND THEN
                  SELECT desc_beneficio
                  INTO   rg_dato.desc_beneficio
                  FROM   tab_beneficio_issste
                  WHERE  tipo_beneficio = rg_dato.tipo_beneficio

                  DISPLAY rg_dato.desc_beneficio TO desc_beneficio
               ELSE
                  ERROR " TIPO DE BENEFICIO ISSSTE INEXISTENTE ... "
                  NEXT FIELD tipo_beneficio
               END IF
            END IF

            IF rg_dato.tipo_retiro = 3 AND rg_dato.tipo_beneficio <> 555 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF
            IF rg_dato.tipo_beneficio = 555 AND rg_dato.tipo_retiro <> 3 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF
            IF rg_dato.tipo_retiro = 6 AND rg_dato.tipo_beneficio <> 888 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF
            IF rg_dato.tipo_beneficio = 888 AND rg_dato.tipo_retiro <> 6 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF

         AFTER FIELD fecha_val_ahorro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD entidad_oficio
            END IF
            IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro = "" THEN
               ERROR "LA FECHA VALOR DE AHORRO NO DEBE SER NULA "
               NEXT FIELD fecha_val_ahorro
            END IF
            { IF rg_dato.fecha_val_ahorro > HOY THEN
                ERROR "LA FECHA VALOR DE AHORRO NO DEBE SER MAYOR A LA FECHA ",
                      "DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_val_ahorro
            END IF }

         AFTER FIELD n_oficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD fecha_val_ahorro
            END IF
            IF rg_dato.n_oficio IS NULL THEN
               ERROR " DEBE CAPTURAR EL NUMERO DE OFICIO ... "
               NEXT FIELD n_oficio
            END IF

         AFTER FIELD fecha_oficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD fecha_val_viv
            END IF

            IF rg_dato.fecha_oficio > HOY THEN
               ERROR "LA FECHA DEL OFICIO NO DEBE SER MAYOR A LA FECHA ",
                     "DEL DIA " ATTRIBUTE(REVERSE)
               NEXT FIELD fecha_oficio
            ELSE
               IF rg_dato.fecha_oficio IS NULL THEN
                  ERROR "DEBE TECLEAR LA FECHA DEL OFICIO ... " ATTRIBUTE(REVERSE)
                  NEXT FIELD fecha_oficio
               END IF
            END IF

         AFTER FIELD entidad_oficio
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD n_oficio
          END IF
          IF rg_dato.entidad_oficio IS NULL OR
             rg_dato.entidad_oficio = 0     THEN
             CALL despliega_issste(1)  #ddp
             DISPLAY rg_dato.entidad_oficio  TO entidad_oficio
             --DISPLAY rg_dato.docprob_desc TO docprob_desc
          ELSE
             SELECT "OK"
             FROM   tab_issste
             WHERE  cvedelissste = rg_dato.entidad_oficio

             IF STATUS <> NOTFOUND THEN
                {SELECT docprob_desc
                INTO   rg_dato.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = rg_dato.docprob_cod

                DISPLAY rg_dato.docprob_desc TO docprob_desc}
             ELSE
                ERROR " ENTIDAD OFICIO NO EXISTENTE                  "
                NEXT FIELD entidad_oficio
             END IF
          END IF
            IF rg_dato.entidad_oficio IS NULL THEN
               ERROR " DEBE CAPTURAR LA ENTIDAD DE OFICIO ... "
               NEXT FIELD entidad_oficio
            END IF

        ON KEY ( ESC )
           IF rg_dato.tipo_beneficio IS NULL OR rg_dato.tipo_beneficio = "" THEN
              ERROR "EL TIPO DE BENEFICIARIO NO PUEDE ESTAR VACIO"
              NEXT FIELD tipo_beneficio
           END IF
           IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro = "" THEN
              ERROR "LA FECHA VALOR AHORRO NO PUEDE ESTAR VACIA"
              NEXT FIELD fecha_val_ahorro
           END IF
           IF rg_dato.n_oficio IS NULL OR rg_dato.n_oficio = "" THEN
              ERROR "EL NUMERO DE OFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD n_oficio
           END IF
           IF rg_dato.fecha_oficio IS NULL OR rg_dato.fecha_oficio = "" THEN
              ERROR "LA FECHA DEL OFICIO NO PUEDE ESTAR VACIA"
              NEXT FIELD fecha_oficio
           END IF
           IF rg_dato.entidad_oficio IS NULL OR rg_dato.entidad_oficio = "" THEN
              ERROR "LA ENTIDAD OFICIO NO PUEDE ESTAR VACIA"
              NEXT FIELD entidad_oficio
           END IF
           IF rg_dato.fecha_val_viv IS NULL OR rg_dato.fecha_val_viv = "" THEN
              ERROR "LA FECHA VALOR VIVIENDA NO PUEDE ESTAR VACIA"
              NEXT FIELD fecha_val_viv
           END IF

           IF bnd = "a" THEN
              IF inserta()THEN #inserta registros enla base de datos
                 ERROR "REGISTRO INSERTADO"
	         EXIT INPUT
	      ELSE
	         ERROR ""
                 EXIT INPUT
              END IF
           ELSE
              EXIT INPUT
           END IF

        ON KEY ( CONTROL-C )
              IF ult_consecutivo IS NOT NULL THEN
                 DELETE FROM ret_consecutivo
                 WHERE consecutivo = ult_consecutivo
              END IF
              EXIT INPUT

        ON KEY ( INTERRUPT )
              IF ult_consecutivo IS NOT NULL THEN
                DELETE FROM ret_consecutivo
                WHERE consecutivo = ult_consecutivo
              END IF
              EXIT INPUT

      END INPUT
      --CLOSE WINDOW retm9011_3
END FUNCTION
#####################################################################
##
##
##
#####################################################################
FUNCTION retiro_tipo_7(bnd)

   DEFINE tip_solic           LIKE afi_mae_afiliado.tipo_solicitud
   DEFINE desc_solic          LIKE tab_tipo_solic.desc_solicitud
   DEFINE tip_trab            LIKE cta_ctr_reg_ind.tipo_trab_ind
   DEFINE desc_trab           LIKE tab_tipo_trab_ind.desc_tipo_trab_ind
   DEFINE bnd                 CHAR(01)

   LET rg_dato.fecha_solicitud = HOY
   LET ventana = "retm9011_7"
   INITIALIZE ult_consecutivo TO NULL

   IF bnd = "a" THEN
      OPEN WINDOW retm9011_7 AT 2,2 WITH FORM "RETM9011_4" ATTRIBUTE (BORDER)
         DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)

         DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 10,1 ATTRIBUTE(REVERSE)
   ELSE
      OPEN WINDOW retm9011_7 AT 2,2 WITH FORM "RETM9013_4" ATTRIBUTE(BORDER)

         DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY HOY USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)

         DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 8,1 ATTRIBUTE(REVERSE)
         DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 ATTRIBUTE(REVERSE)
         DISPLAY " ESC : Consulta               ENTER : MODIFICAR            CONTROL-C : SALIR   " AT 2,1
   END IF

      IF bnd = "a" THEN
         CALL captura_datos_generales() #cdg#
         IF INT_FLAG = 1 THEN
            --CLOSE WINDOW retm9011_7
            RETURN
         END IF
      ELSE
         DISPLAY rg_dato.nss_imss           TO  nss_imss
         DISPLAY rg_dato.nss_issste         TO  nss_issste
         DISPLAY rg_dato.curp               TO  curp
         DISPLAY rg_dato.rfc                TO  rfc
         DISPLAY rg_dato.nombre_afore       TO  nombre_afore
         DISPLAY rg_dato.paterno_afore      TO  paterno_afore
         DISPLAY rg_dato.materno_afore      TO  materno_afore
         DISPLAY rg_dato.fecha_nac          TO  fecha_nac
         DISPLAY rg_dato.fecha_captura      TO  fecha_captura
         DISPLAY rg_dato.estado_solicitud   TO  estado_solicitud
         DISPLAY rg_dato.descripcion_status TO  descripcion_status
         DISPLAY rg_dato.consecutivo        TO  consecutivo
         DISPLAY rg_dato.folio              TO  folio
      END IF

      INPUT BY NAME rg_dato.tipo_retiro       ,rg_dato.desc_retiro        ,rg_dato.tipo_beneficio    ,
                    rg_dato.desc_beneficio    ,rg_dato.n_pensionista      ,rg_dato.fecha_ini_pen     ,
                    rg_dato.fec_sol_susp      ,
                    rg_dato.fec_sol_reac      ,rg_dato.fecha_baja         ,rg_dato.fecha_solicitud   ,
                    rg_dato.entidad_tramite   ,rg_dato.fecha_val_ahorro  ,
                    rg_dato.n_oficio          ,rg_dato.fecha_oficio      ,
                    rg_dato.entidad_oficio    ,rg_dato.fecha_val_viv
                    WITHOUT DEFAULTS

         AFTER FIELD tipo_beneficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_val_viv
            END IF

            IF rg_dato.tipo_beneficio IS NULL THEN
               CALL despliega_tipo_beneficio()  #dtb
               DISPLAY rg_dato.tipo_beneficio TO tipo_beneficio
               DISPLAY rg_dato.desc_beneficio TO desc_beneficio
            ELSE
               SELECT "OK"
               FROM   tab_beneficio_issste
               WHERE  tipo_beneficio = rg_dato.tipo_beneficio

               IF STATUS <> NOTFOUND THEN
                  SELECT desc_beneficio
                  INTO   rg_dato.desc_beneficio
                  FROM   tab_beneficio_issste
                  WHERE  tipo_beneficio = rg_dato.tipo_beneficio

                  DISPLAY rg_dato.desc_beneficio TO desc_beneficio
               ELSE
                  ERROR " TIPO DE BENEFICIO ISSSTE INEXISTENTE ... "
                  NEXT FIELD tipo_beneficio
               END IF
            END IF

            IF rg_dato.tipo_retiro = 7 AND rg_dato.tipo_beneficio <> 999 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF
            IF rg_dato.tipo_beneficio = 999 AND rg_dato.tipo_retiro <> 7 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF

         AFTER FIELD n_pensionista
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_beneficio
            END IF

            IF rg_dato.n_pensionista IS NULL
            THEN
               ERROR " EL NUMERO DE PENSIONISTA NO PUEDE SER NULO PARA ESTE TIPO DE RETIRO "
               NEXT FIELD n_pensionista
            END IF

         AFTER FIELD fecha_ini_pen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_pensionista
            END IF

            IF rg_dato.fecha_ini_pen IS NULL  THEN
               ERROR " LA FECHA DE INICIO DE PENSION NO PUEDE SER NULA PARA ESTE TIPO DE RETIRO "
               NEXT FIELD fecha_ini_pen
            END IF

            IF rg_dato.fecha_ini_pen > HOY THEN
                ERROR "LA FECHA INICIO DE PENSION NO DEBE SER MAYOR A LA FECHA",
                      " DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_ini_pen
            END IF

         AFTER FIELD fec_sol_susp
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD fecha_ini_pen
            END IF

            IF rg_dato.tipo_retiro = 7 AND rg_dato.fec_sol_susp IS NULL THEN
               ERROR " FECHA DE SOLICITUD DE SUSPENSION NO PUEDE SER NULA PARA ESTE TIPO DE RETIRO "
               NEXT FIELD fec_sol_susp
            END IF

            IF rg_dato.fec_sol_susp >= HOY THEN
                ERROR "LA FECHA DE SUSPENCION DE PENSION NO PUEDE SER MAYOR A",
                      " LA FECHA DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fec_sol_susp
            END IF

         AFTER FIELD fec_sol_reac
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fec_sol_susp
            END IF

            IF rg_dato.tipo_retiro = 7 AND rg_dato.fec_sol_reac IS NULL THEN
               ERROR " FECHA DE SOLICITUD DE REACTIVACION NO PUEDE SER NULA PARA ESTE TIPO DE RETIRO "
               NEXT FIELD fec_sol_reac
            END IF

            IF rg_dato.fec_sol_reac > HOY THEN
                ERROR "LA FECHA DE REACTIVACION DE CUENTA NO PUEDE SER MAYOR A",
                      " LA FECHA DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fec_sol_reac
            END IF

         AFTER FIELD fecha_baja
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_val_viv
            END IF

            IF rg_dato.tipo_retiro = 7 AND rg_dato.fecha_baja IS NULL THEN
               ERROR " LA FECHA DE BAJA NO PUEDE SER NULA PARA ESTE TIPO DE RETIRO "
               NEXT FIELD fecha_baja
            END IF

            IF rg_dato.fecha_baja >= HOY THEN
                ERROR "LA FECHA DE BAJA NO PUEDE SER MAYOR A LA FECHA ",
                      " DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_baja
            END IF

         AFTER FIELD entidad_tramite
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_baja
            END IF

          IF rg_dato.entidad_tramite IS NULL OR
             rg_dato.entidad_tramite = 0     THEN
             CALL despliega_issste(2)  #ddp
             DISPLAY rg_dato.entidad_tramite  TO entidad_tramite
             --DISPLAY rg_dato.docprob_desc TO docprob_desc
          ELSE
             SELECT "OK"
             FROM   tab_issste
             WHERE  cvedelissste = rg_dato.entidad_tramite

             IF STATUS <> NOTFOUND THEN
                {SELECT docprob_desc
                INTO   rg_dato.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = rg_dato.docprob_cod

                DISPLAY rg_dato.docprob_desc TO docprob_desc}
             ELSE
                ERROR " ENTIDAD TRAMITE NO EXISTENTE      "
                NEXT FIELD entidad_tramite
             END IF
          END IF

            IF rg_dato.entidad_tramite IS NULL
            THEN
               ERROR " LA ENTIDAD TRAMITE ES UN CAMPO OBLIGATORIO PARA ESTE TIPO DE RETIRO "
               NEXT FIELD entidad_tramite
            END IF

         AFTER FIELD fecha_val_ahorro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD entidad_tramite
            END IF
            IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro ="" THEN
               ERROR "LA FECHA VALOR AHORRO NO ESTAR VACIA"
               NEXT FIELD fecha_val_ahorro
            END IF
            { IF rg_dato.fecha_val_ahorro > HOY THEN
                ERROR "LA FECHA VALOR DE AHORRO NO DEBE SER MAYOR A LA FECHA ",
                      "DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_val_ahorro
            END IF  }

         AFTER FIELD n_oficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD fecha_val_ahorro
            END IF
            IF rg_dato.n_oficio IS NULL THEN
               ERROR " DEBE CAPTURAR EL NUMERO DE OFICIO ... "
               NEXT FIELD n_oficio
            END IF

         AFTER FIELD fecha_oficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD n_oficio
            END IF

            IF rg_dato.fecha_oficio > HOY THEN
               ERROR "LA FECHA DEL OFICIO NO DEBE SER MAYOR A LA FECHA ",
                     "DEL DIA " ATTRIBUTE(REVERSE)
               NEXT FIELD fecha_oficio
            ELSE
               IF rg_dato.fecha_oficio IS NULL THEN
                  ERROR "DEBE TECLEAR LA FECHA DEL OFICIO ... " ATTRIBUTE(REVERSE)
                  NEXT FIELD fecha_oficio
               END IF
            END IF

         AFTER FIELD entidad_oficio
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD fecha_oficio
          END IF
          IF rg_dato.entidad_oficio IS NULL OR
             rg_dato.entidad_oficio = 0     THEN
             CALL despliega_issste(1)  #ddp
             DISPLAY rg_dato.entidad_oficio  TO entidad_oficio
             --DISPLAY rg_dato.docprob_desc TO docprob_desc
          ELSE
             SELECT "OK"
             FROM   tab_issste
             WHERE  cvedelissste = rg_dato.entidad_oficio

             IF STATUS <> NOTFOUND THEN
                {SELECT docprob_desc
                INTO   rg_dato.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = rg_dato.docprob_cod

                DISPLAY rg_dato.docprob_desc TO docprob_desc}
             ELSE
                ERROR " ENTIDAD OFICIO NO EXISTENTE                  "
                NEXT FIELD entidad_oficio
             END IF
          END IF
            IF rg_dato.entidad_oficio IS NULL THEN
               ERROR " DEBE CAPTURAR LA ENTIDAD DE OFICIO ... "
               NEXT FIELD entidad_oficio
            END IF

        ON KEY ( ESC )
           IF rg_dato.tipo_beneficio IS NULL OR rg_dato.tipo_beneficio = "" THEN
              ERROR "EL TIPO DE BENEFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD tipo_beneficio
           END IF
           IF rg_dato.n_pensionista IS NULL OR rg_dato.n_pensionista = "" THEN
              ERROR "EL NUMERO DE PENSIONISTA NO PUEDE ESTAR VACIO"
              NEXT FIELD n_pensionista
           END IF
           IF rg_dato.fecha_ini_pen IS NULL OR rg_dato.fecha_ini_pen = "" THEN
              ERROR "LA FECHA DE INICIO DE PENSION NO PUEDE ESTAR VACIO"
              NEXT FIELD fecha_ini_pen
           END IF
           IF rg_dato.fec_sol_susp IS NULL OR rg_dato.fec_sol_susp = "" THEN
              ERROR "LA FECHA DE LA SOLICITUD DE SUSPENSION NO PUEDE ESTAR VACIO"
              NEXT FIELD fec_sol_susp
           END IF
           IF rg_dato.fec_sol_reac IS NULL OR rg_dato.fec_sol_reac = "" THEN
              ERROR "LA FECHA DE LA SOLICITUD DE REACTIVACION NO PUEDE ESTAR VACIO"
              NEXT FIELD fec_sol_reac
           END IF
           IF rg_dato.fecha_baja IS NULL OR rg_dato.fecha_baja = "" THEN
              ERROR "LA FECHA DE BAJA NO PUEDE ESTAR VACIO"
              NEXT FIELD fecha_baja
           END IF
           IF rg_dato.entidad_tramite IS NULL OR rg_dato.entidad_tramite = "" THEN
              ERROR "LA ENTIDAD DEL TRAMITE NO PUEDE ESTAR VACIO"
              NEXT FIELD entidad_tramite
           END IF
           IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro = "" THEN
              ERROR "LA FECHA VALOR DE AHORRO NO PUEDE ESTAR VACIA"
              NEXT FIELD fecha_val_ahorro
           END IF
           IF rg_dato.n_oficio IS NULL OR rg_dato.n_oficio = "" THEN
              ERROR "EL NUMERO DE OFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD n_oficio
           END IF
           IF rg_dato.fecha_oficio IS NULL OR rg_dato.fecha_oficio = "" THEN
              ERROR "LA FECHA DEL OFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD fecha_oficio
           END IF
            IF rg_dato.entidad_oficio IS NULL OR rg_dato.entidad_oficio = "" THEN
              ERROR "LA ENTIDAD DEL OFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD entidad_oficio
           END IF

           IF bnd = "a" THEN
              IF inserta()THEN #inserta registros enla base de datos
                 ERROR "REGISTRO INSERTADO"
	         EXIT INPUT
	      ELSE
	         ERROR ""
                 EXIT INPUT
              END IF
           ELSE
              EXIT INPUT
           END IF

        ON KEY ( CONTROL-C )
              IF ult_consecutivo IS NOT NULL THEN
                 DELETE FROM ret_consecutivo
                 WHERE consecutivo = ult_consecutivo
              END IF
              EXIT INPUT

        ON KEY ( INTERRUPT )
              IF ult_consecutivo IS NOT NULL THEN
                DELETE FROM ret_consecutivo
                WHERE consecutivo = ult_consecutivo
              END IF
              EXIT INPUT

      END INPUT
      --CLOSE WINDOW retm9011_7
END FUNCTION
#####################################################################
##
##
##
#####################################################################
FUNCTION retiro_tipo_8(bnd)

   DEFINE tip_solic           LIKE afi_mae_afiliado.tipo_solicitud
   DEFINE desc_solic          LIKE tab_tipo_solic.desc_solicitud
   DEFINE tip_trab            LIKE cta_ctr_reg_ind.tipo_trab_ind
   DEFINE desc_trab           LIKE tab_tipo_trab_ind.desc_tipo_trab_ind
   DEFINE bnd                 CHAR(01)

   LET rg_dato.fecha_solicitud = HOY
   LET ventana = "retm9011_8"
   INITIALIZE ult_consecutivo TO NULL

   IF bnd = "a" THEN
   OPEN WINDOW retm9011_8 AT 2,2 WITH FORM "RETM9011_5" ATTRIBUTE (BORDER)
      DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)

      DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 10,1 ATTRIBUTE(REVERSE)
   ELSE
      OPEN WINDOW retm9011_8 AT 2,2 WITH FORM "RETM9013_5" ATTRIBUTE(BORDER)

         DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY HOY USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)

         DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 8,1 ATTRIBUTE(REVERSE)
         DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 17,1 ATTRIBUTE(REVERSE)
         DISPLAY " ESC : Consulta               ENTER : MODIFICAR            CONTROL-C : SALIR   " AT 2,1
   END IF

      IF bnd = "a" THEN
         CALL captura_datos_generales() #cdg#
         IF INT_FLAG = 1 THEN
            --CLOSE WINDOW retm9011_8
            RETURN
         END IF
      ELSE
         DISPLAY rg_dato.nss_imss           TO  nss_imss
         DISPLAY rg_dato.nss_issste         TO  nss_issste
         DISPLAY rg_dato.curp               TO  curp
         DISPLAY rg_dato.rfc                TO  rfc
         DISPLAY rg_dato.nombre_afore       TO  nombre_afore
         DISPLAY rg_dato.paterno_afore      TO  paterno_afore
         DISPLAY rg_dato.materno_afore      TO  materno_afore
         DISPLAY rg_dato.fecha_nac          TO  fecha_nac
         DISPLAY rg_dato.fecha_captura      TO  fecha_captura
         DISPLAY rg_dato.estado_solicitud   TO  estado_solicitud
         DISPLAY rg_dato.descripcion_status TO  descripcion_status
         DISPLAY rg_dato.consecutivo        TO  consecutivo
         DISPLAY rg_dato.folio              TO  folio
      END IF

      INPUT BY NAME rg_dato.tipo_retiro       ,rg_dato.desc_retiro        ,rg_dato.tipo_beneficio    ,
                    rg_dato.desc_beneficio    ,rg_dato.fecha_solicitud    ,
                    rg_dato.fecha_val_ahorro  ,rg_dato.porcentaje_ahorro  ,rg_dato.n_oficio          ,
                    rg_dato.fecha_oficio      ,rg_dato.entidad_oficio     ,rg_dato.porcentaje_fon_viv,
                    rg_dato.fecha_val_viv
                    WITHOUT DEFAULTS

         AFTER FIELD tipo_beneficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD porcentaje_fon_viv
            END IF

            IF rg_dato.tipo_beneficio IS NULL THEN
               CALL despliega_tipo_beneficio()  #dtb
               DISPLAY rg_dato.tipo_beneficio TO tipo_beneficio
               DISPLAY rg_dato.desc_beneficio TO desc_beneficio
            ELSE
               SELECT "OK"
               FROM   tab_beneficio_issste
               WHERE  tipo_beneficio = rg_dato.tipo_beneficio

               IF STATUS <> NOTFOUND THEN
                  SELECT desc_beneficio
                  INTO   rg_dato.desc_beneficio
                  FROM   tab_beneficio_issste
                  WHERE  tipo_beneficio = rg_dato.tipo_beneficio

                  DISPLAY rg_dato.desc_beneficio TO desc_beneficio
               ELSE
                  ERROR " TIPO DE BENEFICIO ISSSTE INEXISTENTE ... "
                  NEXT FIELD tipo_beneficio
               END IF
            END IF

            IF rg_dato.tipo_retiro = 8 AND rg_dato.tipo_beneficio <> 123 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF
            IF rg_dato.tipo_beneficio = 123 AND rg_dato.tipo_retiro <> 8 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF

         AFTER FIELD fecha_val_ahorro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD porcentaje_fon_viv
            END IF
            IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro  = "" THEN
               ERROR "LA FECHA VALOR AHORRO NO PUEDE ESTAR VACIA"
               NEXT FIELD fecha_val_ahorro
            END IF
            { IF rg_dato.fecha_val_ahorro > HOY THEN
                ERROR "LA FECHA VALOR DE AHORRO NO DEBE SER MAYOR A LA FECHA ",
                      "DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_val_ahorro
            END IF }

         AFTER FIELD porcentaje_ahorro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_val_viv
            END IF
            IF rg_dato.porcentaje_ahorro IS NULL AND rg_dato.tipo_retiro= 8 THEN
               ERROR " EL PORCENTAJE DE AHORRO NO PUEDE SER NULO PARA ESTE TIPO DE RETIRO ... "
               NEXT FIELD porcentaje_ahorro
            END IF
            IF rg_dato.porcentaje_ahorro > 100 THEN
               ERROR " EL PORCENTAJE NO PUEDE SER MAYOR AL 100 %"
               NEXT FIELD porcentaje_ahorro
            END IF

         AFTER FIELD n_oficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD porcentaje_ahorro
            END IF
            IF rg_dato.n_oficio IS NULL THEN
               ERROR " DEBE CAPTURAR EL NUMERO DE OFICIO ... "
               NEXT FIELD n_oficio
            END IF

         AFTER FIELD fecha_oficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD porcentaje_ahorro
            END IF

            IF rg_dato.fecha_oficio > HOY THEN
               ERROR "LA FECHA DEL OFICIO NO DEBE SER MAYOR A LA FECHA ",
                     "DEL DIA " ATTRIBUTE(REVERSE)
               NEXT FIELD fecha_oficio
            ELSE
               IF rg_dato.fecha_oficio IS NULL THEN
                  ERROR "DEBE TECLEAR LA FECHA DEL OFICIO ... " ATTRIBUTE(REVERSE)
                  NEXT FIELD fecha_oficio
               END IF
            END IF

         AFTER FIELD entidad_oficio
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD fecha_oficio
          END IF
          IF rg_dato.entidad_oficio IS NULL OR
             rg_dato.entidad_oficio = 0     THEN
             CALL despliega_issste(1)  #ddp
             DISPLAY rg_dato.entidad_oficio  TO entidad_oficio
             --DISPLAY rg_dato.docprob_desc TO docprob_desc
          ELSE
             SELECT "OK"
             FROM   tab_issste
             WHERE  cvedelissste = rg_dato.entidad_oficio

             IF STATUS <> NOTFOUND THEN
                {SELECT docprob_desc
                INTO   rg_dato.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = rg_dato.docprob_cod

                DISPLAY rg_dato.docprob_desc TO docprob_desc}
             ELSE
                ERROR " ENTIDAD OFICIO NO EXISTENTE                  "
                NEXT FIELD entidad_oficio
             END IF
          END IF
            IF rg_dato.entidad_oficio IS NULL THEN
               ERROR " DEBE CAPTURAR LA ENTIDAD DE OFICIO ... "
               NEXT FIELD entidad_oficio
            END IF

        AFTER FIELD porcentaje_fon_viv
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_oficio
            END IF
            IF rg_dato.porcentaje_fon_viv IS NULL AND rg_dato.tipo_retiro = 8
            THEN
               ERROR " EL PORCENTAJE DE VIVIENDA NO PUEDE SER NULO PARA ESTE TIPO DE RETIRO ... "
               NEXT FIELD porcentaje_fon_viv
            END IF
            IF rg_dato.porcentaje_fon_viv > 100 THEN
               ERROR " EL PORCENTAJE NO PUEDE SER MAYOR AL 100 %"
               NEXT FIELD porcentaje_fon_viv
            END IF

        ON KEY ( ESC )
           IF rg_dato.tipo_beneficio IS NULL OR rg_dato.tipo_beneficio = "" THEN
              ERROR "EL TIPO DE BENEFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD tipo_beneficio
           END IF
           IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro = "" THEN
              ERROR "EL TIPO DE BENEFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD fecha_val_ahorro
           END IF
           IF rg_dato.porcentaje_ahorro IS NULL OR rg_dato.porcentaje_ahorro = "" THEN
              ERROR "EL PROCENTAJE DE AHORRO NO PUEDE ESTAR VACIO"
              NEXT FIELD porcentaje_ahorro
           END IF
           IF rg_dato.n_oficio IS NULL OR rg_dato.n_oficio = "" THEN
              ERROR "EL NUMERO DE OFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD n_oficio
           END IF
           IF rg_dato.fecha_oficio IS NULL OR rg_dato.fecha_oficio = "" THEN
              ERROR "LA FECHA DE OFICIO NO PUEDE ESTAR VACIA"
              NEXT FIELD fecha_oficio
           END IF
           IF rg_dato.entidad_oficio IS NULL OR rg_dato.entidad_oficio = "" THEN
              ERROR "LA ENTIDAD DE OFICIO NO PUEDE ESTAR VACIA"
              NEXT FIELD entidad_oficio
           END IF
           IF rg_dato.porcentaje_fon_viv IS NULL OR rg_dato.porcentaje_fon_viv = "" THEN
              ERROR "EL PORCENTAJE DE FONDO DE LA VIVIENDA NO PUEDE ESTAR VACIA"
              NEXT FIELD porcentaje_fon_viv
           END IF

           IF bnd = "a" THEN
              IF inserta()THEN #inserta registros enla base de datos
                 ERROR "REGISTRO INSERTADO"
	         EXIT INPUT
	      ELSE
	         ERROR ""
                 EXIT INPUT
              END IF
           ELSE
              EXIT INPUT
           END IF

        ON KEY ( CONTROL-C )
              IF ult_consecutivo IS NOT NULL THEN
                 DELETE FROM ret_consecutivo
                 WHERE consecutivo = ult_consecutivo
              END IF
              EXIT INPUT

        ON KEY ( INTERRUPT )
              IF ult_consecutivo IS NOT NULL THEN
                DELETE FROM ret_consecutivo
                WHERE consecutivo = ult_consecutivo
              END IF
              EXIT INPUT

      END INPUT
      --CLOSE WINDOW retm9011_8
END FUNCTION
#####################################################################
##
##
##
#####################################################################
FUNCTION retiro_tipo_9(bnd)

   DEFINE tip_solic           LIKE afi_mae_afiliado.tipo_solicitud
   DEFINE desc_solic          LIKE tab_tipo_solic.desc_solicitud
   DEFINE tip_trab            LIKE cta_ctr_reg_ind.tipo_trab_ind
   DEFINE desc_trab           LIKE tab_tipo_trab_ind.desc_tipo_trab_ind
   DEFINE bnd                 CHAR(01)

   LET rg_dato.fecha_solicitud = HOY
   LET ventana = "retm9011_9"
   INITIALIZE ult_consecutivo TO NULL

   IF bnd = "a" THEN
      OPEN WINDOW retm9011_9 AT 2,2 WITH FORM "RETM9011_6" ATTRIBUTE (BORDER)
         DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)

         DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 10,1 ATTRIBUTE(REVERSE)
   ELSE
      OPEN WINDOW retm9011_9 AT 2,2 WITH FORM "RETM9013_6" ATTRIBUTE(BORDER)

         DISPLAY " RETM901                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY HOY USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)

         DISPLAY "              DATOS DE LA SOLICITUD RETIROS ISSSTE TOTALES                     " AT 8,1 ATTRIBUTE(REVERSE)
         DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
         DISPLAY " ESC : Consulta               ENTER : MODIFICAR            CONTROL-C : SALIR   " AT 2,1
   END IF

      IF bnd = "a" THEN
         CALL captura_datos_generales() #cdg#
         IF INT_FLAG = 1 THEN
            --CLOSE WINDOW retm9011_9
            RETURN
         END IF
      ELSE
         DISPLAY rg_dato.nss_imss           TO  nss_imss
         DISPLAY rg_dato.nss_issste         TO  nss_issste
         DISPLAY rg_dato.curp               TO  curp
         DISPLAY rg_dato.rfc                TO  rfc
         DISPLAY rg_dato.nombre_afore       TO  nombre_afore
         DISPLAY rg_dato.paterno_afore      TO  paterno_afore
         DISPLAY rg_dato.materno_afore      TO  materno_afore
         DISPLAY rg_dato.fecha_nac          TO  fecha_nac
         DISPLAY rg_dato.fecha_captura      TO  fecha_captura
         DISPLAY rg_dato.estado_solicitud   TO  estado_solicitud
         DISPLAY rg_dato.descripcion_status TO  descripcion_status
         DISPLAY rg_dato.consecutivo        TO  consecutivo
         DISPLAY rg_dato.folio              TO  folio
      END IF

      INPUT BY NAME rg_dato.tipo_retiro       ,rg_dato.desc_retiro        ,rg_dato.tipo_beneficio    ,
                    rg_dato.desc_beneficio    ,rg_dato.fecha_solicitud    ,
                    rg_dato.fecha_val_ahorro  ,rg_dato.fecha_val_viv
                    WITHOUT DEFAULTS

         AFTER FIELD tipo_beneficio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fech_val_ahorro
            END IF

            IF rg_dato.tipo_beneficio IS NULL THEN
               CALL despliega_tipo_beneficio()  #dtb
               DISPLAY rg_dato.tipo_beneficio TO tipo_beneficio
               DISPLAY rg_dato.desc_beneficio TO desc_beneficio
            ELSE
               SELECT "OK"
               FROM   tab_beneficio_issste
               WHERE  tipo_beneficio = rg_dato.tipo_beneficio

               IF STATUS <> NOTFOUND THEN
                  SELECT desc_beneficio
                  INTO   rg_dato.desc_beneficio
                  FROM   tab_beneficio_issste
                  WHERE  tipo_beneficio = rg_dato.tipo_beneficio

                  DISPLAY rg_dato.desc_beneficio TO desc_beneficio
               ELSE
                  ERROR " TIPO DE BENEFICIO ISSSTE INEXISTENTE ... "
                  NEXT FIELD tipo_beneficio
               END IF
            END IF

            IF rg_dato.tipo_retiro = 9 AND rg_dato.tipo_beneficio <> 0 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF
            IF rg_dato.tipo_beneficio = 0 AND rg_dato.tipo_retiro <> 9 THEN
               ERROR " TIPO DE BENEFICIO INCORRECTO PARA EL TIPO DE RETIRO ..."
               NEXT FIELD tipo_beneficio
            END IF

         AFTER FIELD fecha_val_ahorro
            IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro = "" THEN
               ERROR "LA FECHA DE VALOR DE AHORRO NO PUEDE ESTAR VACIA"
               NEXT FIELD fecha_val_ahorro
            END IF
            { IF rg_dato.fecha_val_ahorro > HOY THEN
                ERROR "LA FECHA VALOR DE AHORRO NO DEBE SER MAYOR A LA FECHA ",
                      "DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_val_ahorro
            END IF }

        ON KEY ( ESC )
           IF rg_dato.tipo_beneficio IS NULL OR rg_dato.tipo_beneficio = "" THEN
              ERROR "EL TIPO DE BENEFICIO NO PUEDE ESTAR VACIO"
              NEXT FIELD tipo_beneficio
           END IF
           IF rg_dato.fecha_val_ahorro IS NULL OR rg_dato.fecha_val_ahorro = "" THEN
              ERROR "LA FECHA VALOR DEL AHORRO NO PUEDE ESTAR VACIA"
              NEXT FIELD fecha_val_ahorro
           END IF

           IF bnd = "a" THEN
              IF inserta()THEN #inserta registros enla base de datos
                 ERROR "REGISTRO INSERTADO"
	         EXIT INPUT
	      ELSE
	         ERROR ""
                 EXIT INPUT
              END IF
           ELSE
              EXIT INPUT
           END IF

        ON KEY ( CONTROL-C )
              IF ult_consecutivo IS NOT NULL THEN
                 DELETE FROM ret_consecutivo
                 WHERE consecutivo = ult_consecutivo
              END IF
              EXIT INPUT

        ON KEY ( INTERRUPT )
              IF ult_consecutivo IS NOT NULL THEN
                DELETE FROM ret_consecutivo
                WHERE consecutivo = ult_consecutivo
              END IF
              EXIT INPUT

      END INPUT
      --CLOSE WINDOW retm9011_9
END FUNCTION

FUNCTION captura_datos_generales()
#cdg------------------------------
    DEFINE #loc #decimal
        mon_sie1              DEC(16,6),
        mon_sie2              DEC(16,6),
        mon_sie3              DEC(16,6),
        mon_sie4              DEC(16,6),
        mon_sie5              DEC(16,6),
        mon_pes               DEC(12,2),
        sw9                   smallint

    DEFINE
        tip_solic             LIKE afi_mae_afiliado.tipo_solicitud      ,
        desc_solic            LIKE tab_tipo_solic.desc_solicitud        ,
        tip_trab              LIKE cta_ctr_reg_ind.tipo_trab_ind        ,
        desc_trab             LIKE tab_tipo_trab_ind.desc_tipo_trab_ind ,
        curp                  LIKE ret_sol_issste_tot.curp

    LET mon_sie1 = 0
    LET mon_sie2 = 0
    LET mon_sie3 = 0
    LET mon_sie4 = 0
    LET mon_sie5 = 0
    LET mon_pes  = 0
    LET curp     = ""
    LET rg_dato.fecha_solicitud = HOY

    IF MONTH(HOY) = 12 THEN
        LET c10_fecha             = "01/01/",YEAR(HOY) + 1
        LET rg_dato.fecha_val_viv = c10_fecha
    ELSE
        LET c10_fecha             = MONTH(HOY)+ 1 USING"&&", "/01/", YEAR(HOY)
        LET rg_dato.fecha_val_viv = c10_fecha
    END IF

    INPUT BY NAME       rg_dato.nss_imss          ,rg_dato.nss_issste       ,rg_dato.curp             ,
                        rg_dato.rfc               ,rg_dato.nombre_afore     ,rg_dato.paterno_afore    ,
                        rg_dato.materno_afore     ,rg_dato.fecha_nac        ,rg_dato.tipo_inversion
                 WITHOUT DEFAULTS

          BEFORE INPUT
             DISPLAY rg_dato.tipo_retiro TO tipo_retiro
             DISPLAY rg_dato.desc_retiro TO desc_retiro
             DISPLAY rg_dato.fecha_solicitud TO fecha_solicitud
             DISPLAY rg_dato.fecha_val_viv TO fecha_val_viv
             IF rg_dato.tipo_retiro <> 2 THEN
                CASE rg_dato.tipo_retiro
                   WHEN 1
                       LET rg_dato.tipo_beneficio = 65
                   WHEN 3
                       LET rg_dato.tipo_beneficio = 555
                   WHEN 6
                       LET rg_dato.tipo_beneficio = 888
                   WHEN 7
                       LET rg_dato.tipo_beneficio = 999
                   WHEN 8
                       LET rg_dato.tipo_beneficio = 123
                   WHEN 9
                       LET rg_dato.tipo_beneficio = 0
                END CASE

                SELECT desc_beneficio
                INTO   rg_dato.desc_beneficio
                FROM   tab_beneficio_issste
                WHERE  tipo_beneficio = rg_dato.tipo_beneficio

                DISPLAY rg_dato.tipo_beneficio TO tipo_beneficio
                DISPLAY rg_dato.desc_beneficio TO desc_beneficio

             END IF

          AFTER FIELD nss_imss
             IF rg_dato.nss_imss IS NULL OR rg_dato.nss_imss = "" THEN
                 ERROR " NSS NO PUEDE SER NULO.. "
             END IF

             SELECT n_rfc   ,
                    n_unico ,
                    paterno ,
                    materno ,
                    nombres ,
                    tipo_solicitud,
                    fena
             INTO   rg_dato.rfc           ,
                    rg_dato.curp          ,
                    rg_dato.paterno_afore ,
                    rg_dato.materno_afore ,
                    rg_dato.nombre_afore  ,
                    tip_solic             ,
                    rg_dato.fecha_nac
             FROM   afi_mae_afiliado
             WHERE  n_seguro = rg_dato.nss_imss

             LET curp = rg_dato.curp

             IF  SQLCA.SQLCODE = NOTFOUND THEN
                 ERROR " NO EXISTE NUMERO DE SEGURO SOCIAL COMO AFILIADO "
                 INITIALIZE  rg_dato.nss_imss TO NULL
                 DISPLAY rg_dato.nss_imss TO nss_imss
                 --NEXT FIELD nss_imss
             ELSE
                 DISPLAY rg_dato.rfc  TO rfc
                 DISPLAY rg_dato.curp TO curp
                 DISPLAY rg_dato.nombre_afore  TO nombre_afore
                 DISPLAY rg_dato.paterno_afore TO paterno_afore
                 DISPLAY rg_dato.materno_afore TO materno_afore
                 DISPLAY rg_dato.fecha_nac     TO fecha_nac
             END IF

             IF rg_dato.nss_imss IS NOT NULL THEN
                SELECT sum(monto_en_acciones)
                INTO mon_sie1
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 1

                IF (mon_sie1 IS NULL) OR (mon_sie1 < 0) THEN
                   LET mon_sie1 = 0
                END IF

                SELECT sum(monto_en_acciones)
                INTO mon_sie2
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 2

                IF (mon_sie2 IS NULL) OR (mon_sie2 < 0) THEN
                   LET mon_sie2 = 0
                END IF

                SELECT sum(monto_en_acciones)
                INTO mon_sie3
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 3

                IF (mon_sie3 IS NULL) OR (mon_sie3 < 0) THEN
                   LET mon_sie3 = 0
                END IF

                SELECT sum(monto_en_acciones)
                INTO mon_sie4
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 4

                IF (mon_sie4 IS NULL) OR (mon_sie4 < 0) THEN
                   LET mon_sie4 = 0
                END IF

                SELECT sum(monto_en_acciones)
                INTO mon_sie5
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 5

                IF (mon_sie5 IS NULL) OR (mon_sie5 < 0) THEN
                   LET mon_sie5 = 0
                END IF

                SELECT sum(monto_en_pesos)
                INTO mon_pes
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (14)

                IF (mon_pes IS NULL) OR (mon_pes < 0) THEN
                   LET mon_pes = 0
                END IF

                IF (mon_sie1 = 0) AND (mon_sie2 = 0) AND (mon_pes = 0) AND
                   (mon_sie3 = 0) AND (mon_sie4 = 0) AND (mon_sie5 = 0) THEN
                   ERROR " NSS CON SALDO CERO EN LAS SUBCUENTAS DE RETIRO ISSSTE"
                   NEXT FIELD nss_imss
                END IF
             END IF

             SELECT "OK"
             FROM   ret_sol_issste_tot
             WHERE  nss_imss = rg_dato.nss_imss
             AND    estado_solicitud IN (0,3,2,22)
             GROUP BY 1

             IF SQLCA.SQLCODE <> NOTFOUND THEN
                 ERROR " YA EXISTE UNA SOLICITUD EN PROCESO PARA ESTE NSS "
                 ATTRIBUTE(REVERSE) SLEEP 2
                 ERROR ""
                 NEXT FIELD nss_imss
             END IF

             SELECT nss_issste into rg_dato.nss_issste
             FROM cta_ctr_reg_ind
             WHERE NTI = rg_dato.nss_imss

             IF SQLCA.SQLCODE <> NOTFOUND THEN
                DISPLAY rg_dato.nss_issste to nss_issste
                SELECT tipo_administracion, tipo_trab_ind
                INTO rg_dato.tipo_inversion, tip_trab
                FROM cta_ctr_reg_ind
                WHERE NTI = rg_dato.nss_imss

                IF rg_dato.tipo_inversion = "01" THEN
                   LET desc_inversion =  "AFORE SIEFORE"
                ELSE
                   LET desc_inversion =  "AFORE BANXICO"
                END IF

                SELECT desc_tipo_trab_ind
                INTO  desc_trab
                FROM  tab_tipo_trab_ind
                WHERE tipo_trab_ind = tip_trab
             ELSE
                LET rg_dato.tipo_inversion = "01"
                LET desc_inversion = "AFORE SIEFORE"

                LET tip_trab  = 0
                LET desc_trab = "AFILIADO DE LA AFORE"
             END IF

             SELECT "OK"
             FROM  tab_tipo_solic
             WHERE tipo_solicitud = tip_solic

             IF SQLCA.SQLCODE <> NOTFOUND THEN
                SELECT desc_solicitud
                INTO   desc_solic
                FROM   tab_tipo_solic
                WHERE  tipo_solicitud = tip_solic
             ELSE
                LET rg_dato.tipo_inversion = "01"
                LET desc_inversion = "AFORE SIEFORE"
             END IF

             DISPLAY rg_dato.tipo_inversion TO tipo_inversion
             DISPLAY desc_inversion         TO desc_inversion

             DISPLAY tip_solic  TO tipo_solicitud
             DISPLAY desc_solic TO desc_solicitud

             DISPLAY tip_trab   TO tipo_trabajador
             DISPLAY desc_trab  TO desc_trabajador

          AFTER FIELD nss_issste
             IF rg_dato.nss_issste IS NOT NULL AND
                rg_dato.nss_imss IS NULL THEN

                SELECT nti into rg_dato.nss_imss
                FROM cta_ctr_reg_ind
                WHERE nss_issste = rg_dato.nss_issste

                IF STATUS = NOTFOUND THEN
                   ERROR " NSS ISSSTE NO REGISTRADO EN LA AFORE ... "
                ELSE
                   SELECT n_rfc   ,
                          n_unico ,
                          paterno ,
                          materno ,
                          nombres ,
                          tipo_solicitud,
                          fena
                   INTO   rg_dato.rfc           ,
                          rg_dato.curp          ,
                          rg_dato.paterno_afore ,
                          rg_dato.materno_afore ,
                          rg_dato.nombre_afore  ,
                          tip_solic             ,
                          rg_dato.fecha_nac
                   FROM   afi_mae_afiliado
                   WHERE  n_seguro = rg_dato.nss_imss

                   DISPLAY rg_dato.rfc  TO rfc
                   DISPLAY rg_dato.curp TO curp
                   DISPLAY rg_dato.nss_imss      TO nss_imss
                   DISPLAY rg_dato.nombre_afore  TO nombre_afore
                   DISPLAY rg_dato.paterno_afore TO paterno_afore
                   DISPLAY rg_dato.materno_afore TO materno_afore
                   DISPLAY rg_dato.fecha_nac     TO fecha_nac
                END IF
             END IF

         AFTER FIELD curp
             IF rg_dato.curp IS NULL AND rg_dato.nss_imss IS NULL THEN
                 ERROR " EL USUARIO DEBE CAPTURAR OBLIGATORIAMENTE EL CURP o EL NSS-IMSS "
                 NEXT FIELD nss_imss
             ELSE
                 SELECT "OK"
                 FROM afi_mae_afiliado
                 WHERE n_unico = rg_dato.curp

                 IF STATUS = NOTFOUND AND rg_dato.nss_imss IS NULL THEN
                    ERROR  " EL CURP NO EXISTE EN LA BASE DE DATOS "
                    NEXT FIELD nss_imss
                 ELSE
                    SELECT n_rfc   ,
                           n_seguro,
                           paterno ,
                           materno ,
                           nombres ,
                           tipo_solicitud ,
                           fena
                    INTO   rg_dato.rfc           ,
                           rg_dato.nss_imss      ,
                           rg_dato.paterno_afore ,
                           rg_dato.materno_afore ,
                           rg_dato.nombre_afore  ,
                           tip_solic             ,
                           rg_dato.fecha_nac
                    FROM   afi_mae_afiliado
                    WHERE  n_unico = rg_dato.curp

                    DISPLAY rg_dato.rfc      TO rfc
                    DISPLAY rg_dato.nss_imss TO nss_imss
                    DISPLAY rg_dato.nombre_afore  TO nombre_afore
                    DISPLAY rg_dato.paterno_afore TO paterno_afore
                    DISPLAY rg_dato.materno_afore TO materno_afore
                    DISPLAY rg_dato.fecha_nac     TO fecha_nac
                    DISPLAY rg_dato.curp          TO curp
                    --
                    LET curp = rg_dato.curp
                    --
                 END IF
             END IF

             SELECT sum(monto_en_acciones)
             INTO mon_sie1
             FROM dis_cuenta
             WHERE nss =   rg_dato.nss_imss
             AND subcuenta IN (13,19)
             AND siefore = 1

             IF (mon_sie1 IS NULL) OR (mon_sie1 < 0) THEN
                LET mon_sie1 = 0
             END IF

             SELECT sum(monto_en_acciones)
             INTO mon_sie2
             FROM dis_cuenta
             WHERE nss     = rg_dato.nss_imss
             AND subcuenta IN (13,19)
             AND siefore = 2

             IF (mon_sie2 IS NULL) OR (mon_sie2 < 0) THEN
                LET mon_sie2 = 0
             END IF

                SELECT sum(monto_en_acciones)
                INTO mon_sie3
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 3

                IF (mon_sie3 IS NULL) OR (mon_sie3 < 0) THEN
                   LET mon_sie3 = 0
                END IF

                SELECT sum(monto_en_acciones)
                INTO mon_sie4
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 4

                IF (mon_sie4 IS NULL) OR (mon_sie4 < 0) THEN
                   LET mon_sie4 = 0
                END IF

                SELECT sum(monto_en_acciones)
                INTO mon_sie5
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 5

                IF (mon_sie5 IS NULL) OR (mon_sie5 < 0) THEN
                   LET mon_sie5 = 0
                END IF

             SELECT sum(monto_en_pesos)
             INTO mon_pes
             FROM dis_cuenta
             WHERE nss     = rg_dato.nss_imss
             AND subcuenta IN (14)

             IF (mon_pes IS NULL) OR (mon_pes < 0) THEN
                LET mon_pes = 0
             END IF

             IF (mon_sie1 = 0) AND (mon_sie2 = 0) AND (mon_pes = 0) AND
                (mon_sie3 = 0) AND (mon_sie4 = 0) AND (mon_sie5 = 0) THEN
                ERROR " NSS CON SALDO CERO EN LAS SUBCUENTAS DE RETIRO ISSSTE "
                NEXT FIELD curp
             END IF


             { SELECT "OK"
             FROM   ret_sol_issste_tot
             WHERE  curp            = rg_dato.curp
             AND    estado_solicitud IN (0,3,2,22)
             GROUP BY 1 }

             LET sw9 = 0
             SELECT count(*) into sw9
             FROM   ret_sol_issste_tot a
             WHERE  a.curp            = rg_dato.curp
             AND    a.estado_solicitud IN (0,3,2,22)
             IF sw9 <> 0 THEN
                 ERROR " YA EXISTE AL DIA DE HOY UNA SOLICITUD PARA ESTE CURP "
                 ATTRIBUTE(REVERSE) SLEEP 2
                 ERROR ""
                 NEXT FIELD curp
             END IF

             SELECT "OK"
             FROM cta_ctr_reg_ind
             WHERE NTI = rg_dato.nss_imss

             IF SQLCA.SQLCODE <> NOTFOUND THEN
                SELECT tipo_administracion, tipo_trab_ind, nss_issste
                INTO rg_dato.tipo_inversion, tip_trab, rg_dato.nss_issste
                FROM cta_ctr_reg_ind
                WHERE NTI = rg_dato.nss_imss
                DISPLAY rg_dato.nss_issste TO nss_issste

                IF rg_dato.tipo_inversion = "01" THEN
                   LET desc_inversion =  "AFORE SIEFORE"
                ELSE
                   LET desc_inversion =  "AFORE BANXICO"
                END IF

                SELECT desc_tipo_trab_ind
                INTO  desc_trab
                FROM  tab_tipo_trab_ind
                WHERE tipo_trab_ind = tip_trab
             ELSE
                LET rg_dato.tipo_inversion = "01"
                LET desc_inversion = "AFORE SIEFORE"

                LET tip_trab  = 0
                LET desc_trab = "AFILIADO DE LA AFORE"
             END IF

             SELECT "OK"
             FROM  tab_tipo_solic
             WHERE tipo_solicitud = tip_solic

             IF SQLCA.SQLCODE <> NOTFOUND THEN
                SELECT desc_solicitud
                INTO   desc_solic
                FROM   tab_tipo_solic
                WHERE  tipo_solicitud = tip_solic
             ELSE
                LET rg_dato.tipo_inversion = "01"
                LET desc_inversion = "AFORE SIEFORE"
             END IF

             DISPLAY rg_dato.tipo_inversion TO tipo_inversion
             DISPLAY desc_inversion         TO desc_inversion

             DISPLAY tip_solic  TO tipo_solicitud
             DISPLAY desc_solic TO desc_solicitud

             DISPLAY tip_trab   TO tipo_trabajador
             DISPLAY desc_trab  TO desc_trabajador

         AFTER INPUT
            IF rg_dato.curp <> curp THEN
               LET rg_dato.curp = curp
            END IF
            EXIT INPUT

         ON KEY ( ESC )
            SELECT sum(monto_en_acciones)
            INTO mon_sie1
            FROM dis_cuenta
            WHERE nss =   rg_dato.nss_imss
            AND subcuenta IN (13,19)
            AND siefore = 1

            IF (mon_sie1 IS NULL) OR (mon_sie1 < 0) THEN
               LET mon_sie1 = 0
            END IF

            SELECT sum(monto_en_acciones)
            INTO mon_sie2
            FROM dis_cuenta
            WHERE nss     = rg_dato.nss_imss
            AND subcuenta IN (13,19)
            AND siefore = 2

            IF (mon_sie2 IS NULL) OR (mon_sie2 < 0) THEN
               LET mon_sie2 = 0
            END IF

                SELECT sum(monto_en_acciones)
                INTO mon_sie3
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 3

                IF (mon_sie3 IS NULL) OR (mon_sie3 < 0) THEN
                   LET mon_sie3 = 0
                END IF

                SELECT sum(monto_en_acciones)
                INTO mon_sie4
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 4

                IF (mon_sie4 IS NULL) OR (mon_sie4 < 0) THEN
                   LET mon_sie4 = 0
                END IF

                SELECT sum(monto_en_acciones)
                INTO mon_sie5
                FROM dis_cuenta
                WHERE nss     = rg_dato.nss_imss
                AND subcuenta IN (13,19)
                AND siefore = 5

                IF (mon_sie5 IS NULL) OR (mon_sie5 < 0) THEN
                   LET mon_sie5 = 0
                END IF


            SELECT sum(monto_en_pesos)
            INTO mon_pes
            FROM dis_cuenta
            WHERE nss     = rg_dato.nss_imss
            AND subcuenta IN (14)

            IF (mon_pes IS NULL) OR (mon_pes < 0) THEN
               LET mon_pes = 0
            END IF

            IF (mon_sie1 = 0) AND (mon_sie2 = 0) AND (mon_pes = 0) AND
               (mon_sie3 = 0) AND (mon_sie4 = 0) AND (mon_sie5 = 0) THEN
               ERROR " NSS CON SALDO CERO EN LAS SUBCUENTAS DE RETIRO ISSSTE "
               NEXT FIELD curp
            END IF

            IF rg_dato.curp <> curp THEN
               LET rg_dato.curp = curp
               DISPLAY rg_dato.curp TO curp
            END IF
            IF rg_dato.nss_imss IS NULL OR rg_dato.nss_imss = "" THEN
               LET rg_dato.curp = curp
               DISPLAY rg_dato.curp TO curp
            END IF
            IF rg_dato.nss_imss IS NULL OR rg_dato.nss_imss = "" THEN
               ERROR "EL NSS NO PUEDE ESTAR VACIO "
               NEXT FIELD nss_imss
            END IF
            LET INT_FLAG = 0
            EXIT INPUT

         ON KEY ( CONTROL-C )
            LET INT_FLAG = 1
            EXIT INPUT

       END INPUT
END FUNCTION


######################################################################
##
##
######################################################################
#in--------------------------------------------------------------------
FUNCTION inserta()

   DEFINE sw_1                SMALLINT

   DEFINE #loc #decimal
          mon_sie1            DEC(16,6),
          mon_sie2            DEC(16,6),
          mon_pes             DEC(12,2)

   DEFINE #loc #decimal
          vconsecutivo        LIKE ret_solicitud_tx.consecutivo

   LET sw_1 = 0

   IF sw_1 = 0  THEN
      SET LOCK MODE TO WAIT
      LOCK TABLE ret_consecutivo IN EXCLUSIVE MODE

      SELECT MAX(consecutivo)+1
      INTO   ult_consecutivo
      FROM   ret_consecutivo

      INSERT INTO ret_consecutivo VALUES (ult_consecutivo)

      UNLOCK TABLE ret_consecutivo
      SET LOCK MODE TO NOT WAIT

      LET sw_1 = 1
   END IF

   WHENEVER ERROR STOP
   INSERT INTO ret_sol_issste_tot
   VALUES(  ""                        , --folio
            ""                        , --folio_sol
            "S"                       , --tipo_id
            ult_consecutivo           , --consecutivo
            rg_dato.nss_imss          , --nss_imss
            rg_dato.nss_issste        , --nss_issste
            rg_dato.curp              , --curp
            rg_dato.fecha_nac         , --fecha_nac
            rg_dato.tipo_retiro       , --tipo_retiro
            rg_dato.tipo_beneficio    , --tipo_beneficio
            rg_dato.n_pensionista     , --n_pensionista
            rg_dato.entidad_tramite   , --entidad_tramite
            rg_dato.fecha_ini_pen     , --fecha_ini_pen
            rg_dato.docprob_cod       , --cve_doc_prob
            rg_dato.fec_sol_susp      , --fec_sol_susp
            rg_dato.fec_sol_reac      , --fec_sol_reac
            rg_dato.fecha_baja        , --fecha_baja
            rg_dato.fecha_solicitud   , --fecha_solicitud
            rg_dato.tipo_inversion    , --tipo_inversion
            rg_dato.fecha_val_ahorro  , --fecha_val_ahorro
            rg_dato.porcentaje_ahorro , --porcentaje_ahorro
            0                         , --acciones_siefore1   # Acciones Siefore 1
            0                         , --acciones_siefore2   # Acciones Siefore 2
            0                         , --impt_ahorro_ret     # Impt_ahorro_ret
            rg_dato.n_oficio          , --n_oficio
            rg_dato.fecha_oficio      , --fecha_oficio
            rg_dato.entidad_oficio    , --entidad_oficio
            rg_dato.fecha_val_viv     , --fecha_val_viv
            rg_dato.porcentaje_fon_viv, --porcentaje_fon_viv
            0                         , --impt_fon_viv_liq    # impt_fon_viv_liq
            ""                        , --diag_procesar       # Diag_procesar
            ""                        , --diag_issste         # Diag_issste
            ""                        , --status_fondo_viv    # Status Fondo Viv
            ""                        , --status_sub_ret      # Status Sub. Ret.
            rg_dato.rfc_dependencia   , --rfc_dependencia
            rg_dato.nom_dependencia   , --nom_dependencia
            rg_dato.cve_ramo          , --cve_ramo
            rg_dato.cve_pagaduria     , --cve_pagaduria

            ""                        , --id_procesar
            ""                        , --nom_completo_icefa
            ""                        , --nom_beneficiario
            ""                        , --rfc_pension
            ""                        , --paterno_pension
            ""                        , --materno_pension
            ""                        , --nombres_pension

            reg_1.capturado           , --estado_solicitud
            HOY                       , --fecha_captura
            ""                        , --fecha_confirma
            ""                        , --fecha_modifica
            ""                        , --fecha_liquida
            ""                        , --fecha_envio
            usuario                   , --usuario_captura
            ""                        , --usuario_confirma
            ""                        , --usuario_modifica
            0                         , --carta                   # carta
            0                         , --grupo                   # grupo
            " "                         --cve_destino
         )

   IF SQLCA.SQLCODE < 0 THEN
      LET x_error = err_get(SQLCA.SQLCODE)
      LET x_error = "INSERT ret_sol_issste_tot:",
   	    "nss ",rg_dato.nss_imss,
   	    "consecutivo ",ult_consecutivo,
                     x_error CLIPPED

      CALL errorlog(x_error CLIPPED)

      PROMPT "  ERROR AL INSERTAR REGISTROS AVISAR A SISTEMAS "
      FOR enter
      EXIT PROGRAM
   ELSE
      ---------INICIO PROCESO DE MARCAJE DE CUENTAS-----------

      SELECT movimiento
      INTO   s_tipo_movimiento
      FROM   tab_retiro_issste
      WHERE  tipo_retiro = rg_dato.tipo_retiro

      CALL marca_cuenta (rg_dato.nss_imss         ,
                         s_tipo_movimiento        ,
                         ult_consecutivo
                        )#mc
      RETURNING v_marca_res   ,
                v_cod_rechazo

      IF v_cod_rechazo > 0 THEN
          SELECT A.rechazo_desc
          INTO   desc_status_rech
          FROM   tab_rch_marca A
          WHERE  A.rechazo_cod = v_cod_rechazo
          CURRENT WINDOW IS retm9011

          PROMPT " SOLICITUD RECHAZADA(",v_cod_rechazo," ",
                desc_status_rech,") <ENTER> CANCELAR" FOR CHAR enter

          DELETE from ret_sol_issste_tot
          WHERE nss_imss    = rg_dato.nss_imss
          AND   consecutivo = ult_consecutivo

          CLEAR FORM
          RETURN FALSE
      END IF
   END IF

   SELECT "OK"
   FROM   ret_beneficiario
   WHERE  nss         =  rg_dato.nss_imss
   AND    consecutivo =  ult_consecutivo
   GROUP BY 1

   IF STATUS = NOTFOUND THEN
       LET ejecuta = "fglgo RETM810 ",rg_dato.nss_imss," ",
                                      ult_consecutivo," ",
                                     "A"
       RUN ejecuta
   ELSE
       RETURN
   END IF

   SELECT "OK"
   FROM   ret_beneficiario
   WHERE  nss         =  rg_dato.nss_imss
   AND    consecutivo =  ult_consecutivo
   GROUP BY 1

   IF STATUS = NOTFOUND THEN
      ERROR " NO SE PUEDE CAPTURAR LA SOLICITUD SIN BENEFICIARIOS"

      ATTRIBUTE(REVERSE)

      DELETE
      FROM  ret_sol_issste_tot
      WHERE nss_imss    =  rg_dato.nss_imss
      AND   consecutivo =  ult_consecutivo

      WHENEVER ERROR STOP
      ##### DESMARCAJE ##################################

      SELECT movimiento
      INTO   s_tipo_movimiento
      FROM   tab_retiro_issste
      WHERE  tipo_retiro = rg_dato.tipo_retiro

      LET v_marca_ent     = s_tipo_movimiento
      LET vestado_marca   = 40
      LET vcodigo_rechazo = 0
      LET vmarca_causa    = 0
      LET vfecha_causa    = NULL

      LET v_desmarca   = " EXECUTE PROCEDURE desmarca_cuenta('",
                         rg_dato.nss_imss,"',",
                         v_marca_ent,",",
                         ult_consecutivo,",",
                         vestado_marca,",",
                         vmarca_causa,",' ",
                         usuario,"')"

      PREPARE eje_reversa_mar FROM v_desmarca
      EXECUTE eje_reversa_mar

      ####################################################

      RETURN FALSE
   END IF

   ERROR "REGISTRO INGRESADO"
   SLEEP 3

   CLEAR FORM
   INITIALIZE vaccion  TO NULL
   RETURN TRUE
END FUNCTION

