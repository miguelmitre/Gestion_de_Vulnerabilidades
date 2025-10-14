################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa Carga01  => Carga de archivo tra_issste_unifica                      #
#Fecha creacion    => 18 Feb 2010                                              #
#By                => MAGR                                                     #
################################################################################
DATABASE safre_af

GLOBALS   "TRACIU02g.4gl"


MAIN
   DEFINE   ld_hoy                    DATE
   DEFINE   ls_carga                  INTEGER
   DEFINE   ls_valida                 INTEGER
   DEFINE   li_folio                  INTEGER
   DEFINE   res                       CHAR(1)

   CALL STARTLOG("TRACIU02.log")

   DEFER INTERRUPT
   OPTIONS INPUT WRAP         ,
           PROMPT LINE LAST   ,
           ACCEPT KEY CONTROL-I

   CALL f_prepara()
   CALL f_inicio()
   LET  ld_hoy       = TODAY

   OPEN WINDOW f_01 AT 2,2 WITH FORM "TRACU0012"                                                          ATTRIBUTE(BORDER)
   DISPLAY "TRACIU02          Carga BDNSAR - Cuentas Inactivas                                 " AT 2,3   ATTRIBUTE(REVERSE)
   DISPLAY ld_hoy                                                             USING "DD/MM/YYYY" AT 2,67  ATTRIBUTE(REVERSE)

   MENU "MENU"
      COMMAND "Carga Archivo" "Layaout 098301 B.D BDNSAR - Cuentas Inactivas"
              LET  ls_carga   = 0
              LET  ls_valida  = 0
              CALL f_carga()                            RETURNING ls_carga
      COMMAND "Ingreso" "Ingreso de solicitudes al maestro de UNI-SAR"
              CALL f_consulta_folio()            RETURNING li_folio
              IF   li_folio IS NULL 
               OR  li_folio  = 0 THEN 
              ELSE 
                   LET  res = "N"
                   PROMPT " Desea realizar el proceso s/n "   FOR CHAR res
                   IF  res = "S" 
                    OR res = "s"  THEN 
                        CALL f_pide_folio(li_folio)
                   END IF
              END IF
      COMMAND "Salida" "Salida del programa"
         EXIT MENU

   END MENU

   CLOSE WINDOW f_01
END MAIN


FUNCTION f_prepara()
   DEFINE    lc_query                       CHAR(1000)

    --- INSERTAR REGISTRO EN unifica
    LET     lc_query    = "  INSERT INTO tra_issste_unifica  ",
                          "  VALUES ( ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,? )"
    PREPARE p_ins_unif         FROM  lc_query

END FUNCTION


FUNCTION f_inicio()
    DEFINE   lc_query      CHAR(500)

    -- Obtener el FOLIO -----
    SELECT  MAX(folio) + 1
      INTO  gi_folio
      FROM  safre_af:glo_folio

    INSERT  INTO  safre_af:glo_folio   VALUES (gi_folio)

    -- Obtener el USUARIO 
    SELECT  USER
      INTO  gc_usuario
      FROM  safre_af:tab_afore_local

    -- Obtener la Ruta de Archivo entrada
    SELECT  ruta_rescate
      INTO gc_ruta 
      FROM safre_af:seg_modulo 
     WHERE modulo_cod = 'tra'
   
END FUNCTION


FUNCTION f_carga()
    DEFINE   nom_archivo                CHAR(100)
    DEFINE   ls_carga                   SMALLINT
    DEFINE   ld_hoy                     DATE
    DEFINE   archivo_retiro             CHAR(200)
    DEFINE   li_cuantos                 INTEGER
    DEFINE   lc_query                   CHAR(500)
    LET    ld_hoy   = TODAY
    LET    ls_carga = 0
    LET    nom_archivo  = NULL

    OPEN WINDOW f_02 AT 2,2                                                     WITH FORM "TRACU0013"            ATTRIBUTE(BORDER)
    CLEAR FORM
    DISPLAY " < Ctrl-C > Salir                                                                        " AT 1,1   ATTRIBUTE(REVERSE)
    DISPLAY "TRACIU02          Carga BDNSAR - Cuentas Inactivas                                 " AT 3,1   ATTRIBUTE(REVERSE)
    DISPLAY ld_hoy                                      USING "DD/MM/YYYY" AT 3, 67  ATTRIBUTE(REVERSE)

    INPUT BY NAME nom_archivo
                  WITHOUT DEFAULTS

        BEFORE FIELD nom_archivo
            LET nom_archivo = NULL
            CLEAR FORM

        AFTER FIELD nom_archivo
            IF nom_archivo IS NULL THEN
               ERROR " NOMBRE DE ARCHIVO, NO PUEDE SER NULO" ATTRIBUTE(REVERSE)
               NEXT FIELD nom_archivo
            END IF

            CALL f_genera_tmp()
            --- PARA TRATAR A TODOS LOS CARGADOS EN TEMPORAL
            LET     lc_query    = "  SELECT  *                ",
                                  "    FROM  tmp_carga_afo    "
            PREPARE p_cur1             FROM  lc_query
            DECLARE d_cur1      CURSOR FOR   p_cur1

            LET     lc_query    = "  DELETE                   ",
                                  "    FROM  tmp_carga_afo    "
            PREPARE p_del_tem          FROM  lc_query
            EXECUTE p_del_tem
            WHENEVER ERROR CONTINUE
            LET archivo_retiro = gc_ruta CLIPPED, '/', nom_archivo CLIPPED
            LOAD FROM archivo_retiro DELIMITER "+"    INSERT INTO tmp_carga_afo

            SELECT   count(*)
              INTO   li_cuantos
              FROM   tmp_carga_afo
            WHENEVER ERROR STOP

            IF  li_cuantos = 0 THEN
                ERROR "ERROR, NO EXISTE EL ARCHIVO O ESTA VACIO"
                SLEEP 2
                NEXT FIELD nom_archivo
            END IF

            LET    ls_carga = 1
            EXIT INPUT

        ON KEY (INTERRUPT)
            LET    ls_carga = 0
            EXIT INPUT

    END INPUT

    IF    ls_carga  = 1   THEN
          DISPLAY " PROCESANDO INFORMACION, CARGA DE ARCHIVO "                      AT 08,1 ATTRIBUTE(REVERSE)
          CALL f_sigue_carga(nom_archivo)                                           RETURNING ls_carga
          IF  ls_carga    = 1   THEN
              PROMPT " CARGA TERMINADA, <ENTER> PARA CONTINUAR                    " FOR CHAR enter
          ELSE
              PROMPT " ERROR EN LA ESTRUCTURA DEL ARCHIVO, NO SE REALIZA LA CARGA " FOR CHAR enter
          END IF
          LET nom_archivo = NULL
    END IF

    CLOSE WINDOW f_02
    RETURN  ls_carga
END FUNCTION


FUNCTION f_genera_tmp()

       WHENEVER ERROR CONTINUE
       CREATE TEMP TABLE tmp_carga_afo
           (n_reg                    CHAR(579))

       WHENEVER ERROR STOP
END FUNCTION


FUNCTION f_sigue_carga(lc_nom_archivo)
    DEFINE  lc_nom_archivo        CHAR(100)
    DEFINE  li_lei_tot            INTEGER
    DEFINE  li_lei_cbz            INTEGER
    DEFINE  li_lei_det            INTEGER
    DEFINE  li_lei_sum            INTEGER
    DEFINE  n_reg                 CHAR(580)
    DEFINE  ls_carga              SMALLINT
    DEFINE  ld_fecha              CHAR(10)
    DEFINE  li_ahorro             DECIMAL(12,2)
    DEFINE  li_vivienda           DECIMAL(12,2)
    DEFINE  lc_query              CHAR(500)

    LET li_lei_tot     = 0
    LET li_lei_det     = 0
    LET ls_carga       = 0
    LET li_ahorro      = 0
    LET li_vivienda    = 0

    FOREACH d_cur1            INTO   n_reg
        INITIALIZE gr_deta.* TO NULL
        LET li_lei_tot = li_lei_tot + 1
            LET li_lei_det = li_lei_det + 1

            LET   gr_deta.nss                     = n_reg[001,011]
            LET   gr_deta.curp                    = n_reg[012,029]
            LET   gr_deta.rfc                     = n_reg[030,042]
            LET   gr_deta.paterno                 = n_reg[043,082]
            LET   gr_deta.materno                 = n_reg[083,122]
            LET   gr_deta.nombre                  = n_reg[123,162]
            LET   ld_fecha                        = n_reg[168,169],
                                                    "/",
                                                    n_reg[171,172],
                                                    "/",
                                                    n_reg[163,166]
            LET   gr_deta.f_nacimiento            = ld_fecha                       
            LET   gr_deta.sexo                    = n_reg[173,173]
            LET   gr_deta.ent_nacimiento          = n_reg[174,175]
            LET   gr_deta.afore                   = n_reg[176,178]
            LET   gr_deta.id_procesar             = n_reg[179,186]
            LET   gr_deta.curp_icefa              = n_reg[187,204]
            LET   gr_deta.nss_icefa               = n_reg[205,215]
            LET   gr_deta.rfc_icefa               = n_reg[216,228]
            LET   gr_deta.nti                     = n_reg[229,258]
            LET   gr_deta.paterno_icefa           = n_reg[259,298]
            LET   gr_deta.materno_icefa           = n_reg[299,338]
            LET   gr_deta.nombre_icefa            = n_reg[339,378]
            LET   ld_fecha                        = n_reg[384,385],
                                                    "/",
                                                    n_reg[387,388],
                                                    "/",
                                                    n_reg[379,382]
            LET   gr_deta.f_nacimiento_icefa      = ld_fecha        
            LET   gr_deta.nombre_completo         = n_reg[457,576]
            LET   gr_deta.icefa                   = n_reg[577,579]
            LET   gr_deta.f_carga                 = TODAY
            LET   gr_deta.f_actualiza             = ""
            LET   gr_deta.usuario                 = gc_usuario
            LET   gr_deta.correlativo             = 0
            LET   gr_deta.folio                   = gi_folio

            EXECUTE  p_ins_unif                    USING gr_deta.*

        DISPLAY " FOLIO GENERADO             : ",gi_folio       AT 11,8
        DISPLAY " TOTAL REGISTROS LEIDOS     : ",li_lei_tot     AT 12,8
        DISPLAY " TOTAL REGISTROS GRABADOS   : ",li_lei_det     AT 13,8
    END FOREACH


    IF     li_lei_det     >   0                                --- Trae al menos un detalle
       THEN
           LET   ls_carga                         = 1
    ELSE
           ERROR " TIENE DETALLES   ..... ", li_lei_det
           SLEEP 3
           LET   ls_carga                         = 0
    END IF
    RETURN  ls_carga
END FUNCTION
