##############################################################################
#Programa PROM025  => MANTENIMIENTO AL MAESTRO DE PROMOTORES                 #
#Sistema           => PRO.                                                   #
#By                => ADOLFO DE LA MAZA.                                     #
#Fecha             => 27 Noviembre 1996.                                     #
#Modificado por    => LAURA EUGENIA CORTES GUZMAN                            #
#Fecha actualiz.   => 29 de MARZO DEL 2004                                   #
#Modificado Por    => Isabel Fonseca Frias                                   #
#Fecha             => 27 de Febrero del 2008                                 #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT version   #
#                  => 3.0   (v1)                                             #
#Modificado Por    => Isabel Fonseca Frias                                   #
#Fecha             => 28 de septiembre del 2009                              #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT           #
#                  => con fecha 29/07/2009                                   #
#                  => (v10)                                                  #
##############################################################################
DATABASE safre_af
GLOBALS
   DEFINE arr_1 ARRAY[2000] OF RECORD
      codven                LIKE pro_mae_promotor.codven          ,
      cod_promotor          LIKE pro_mae_promotor.cod_promotor    ,
      agenc_cod             LIKE pro_mae_promotor.agenc_cod       ,
      desagenc              CHAR(60)                              ,
      nivel                 LIKE pro_mae_promotor.nivel           ,
      desnivel              CHAR(20)                              ,
      son                   INTEGER                               ,
      seguro                LIKE pro_mae_promotor.seguro          ,
      rfc                   CHAR(10)                              ,
      homonimia             CHAR(3)                               ,
      unico                 LIKE pro_mae_promotor.unico           ,
##      tipo_recibo           LIKE pro_mae_promotor.tipo_recibo     ,
      escolar               LIKE pro_mae_promotor.escolar         ,
      paterno               LIKE pro_mae_promotor.paterno         ,
      materno               LIKE pro_mae_promotor.materno         ,
      nombres               LIKE pro_mae_promotor.nombres         ,
      fnaci                 LIKE pro_mae_promotor.fnaci           ,
      fenvio                LIKE pro_mae_promotor.fenvio          ,
      fecha_registro        LIKE pro_mae_promotor.fecha_registro  ,
      fecha_baja            LIKE pro_mae_promotor.fecha_baja      ,
      fecha_suspende        LIKE pro_mae_promotor.fecha_suspende  ,
      resuelva              LIKE pro_mae_promotor.resuelva        ,
      sexo                  CHAR(1)                               , --(v10)
      edo_naci              CHAR(2)                               , --(v10)
      calle                 LIKE pro_mae_promotor.calle           ,
      numero                LIKE pro_mae_promotor.numero          ,
      dpto                  LIKE pro_mae_promotor.dpto            ,
      codpos                LIKE pro_mae_promotor.codpos          ,
      colonia               LIKE pro_mae_promotor.colonia         ,
      deleg                 LIKE pro_mae_promotor.deleg           ,
      desdeleg              CHAR(20)                              ,
      ciudad                LIKE pro_mae_promotor.ciudad          ,
      desciudad             CHAR(20)                              ,
      estado                LIKE pro_mae_promotor.estado          ,
      desestad              CHAR(20)                              ,
      fono                  LIKE pro_mae_promotor.fono            ,
      ftermino              DATE 
   END RECORD

   DEFINE reg RECORD
      codven                LIKE pro_mae_promotor.codven          ,
      cod_promotor          LIKE pro_mae_promotor.cod_promotor    ,
      seguro                LIKE pro_mae_promotor.seguro          ,
      fecha_baja            LIKE pro_mae_promotor.fecha_baja      ,
      agenc_cod             LIKE pro_mae_promotor.agenc_cod       ,
      desagenc              CHAR(60)                              ,
      unico                 LIKE pro_mae_promotor.unico           ,
      escolar               LIKE pro_mae_promotor.escolar         ,
##      tipo_recibo           LIKE pro_mae_promotor.tipo_recibo     ,
      rfc                   CHAR(10)                              ,
      homonimia             CHAR(3)                               ,
      paterno               LIKE pro_mae_promotor.paterno         ,
      materno               LIKE pro_mae_promotor.materno         ,
      nombres               LIKE pro_mae_promotor.nombres         ,
      fnaci                 LIKE pro_mae_promotor.fnaci           ,
      fenvio                LIKE pro_mae_promotor.fenvio          ,
      calle                 LIKE pro_mae_promotor.calle           ,
      numero                LIKE pro_mae_promotor.numero          ,
      dpto                  LIKE pro_mae_promotor.dpto            ,
      colonia               LIKE pro_mae_promotor.colonia         ,
      deleg                 LIKE pro_mae_promotor.deleg           ,
      ciudad                LIKE pro_mae_promotor.ciudad          ,
      estado                LIKE pro_mae_promotor.estado          ,
      codpos                LIKE pro_mae_promotor.codpos          ,
      fono                  LIKE pro_mae_promotor.fono            ,
      nivel                 LIKE pro_mae_promotor.nivel           ,
      resuelva              LIKE pro_mae_promotor.resuelva        ,
      sexo                  CHAR(1)                               , --(v10)
      edo_naci              CHAR(2)                               , --(v10)
      fecha_suspende        LIKE pro_mae_promotor.fecha_suspende  ,
      fecha_registro        LIKE pro_mae_promotor.fecha_registro  ,
      status                LIKE pro_mae_promotor.status          ,
      status_interno        LIKE pro_mae_promotor.status_interno  ,
      motivo_suspende       LIKE pro_mae_promotor.motivo_suspende ,
      ftermino              DATE     ##Se agrega el 251103
   END RECORD

   DEFINE reg_mod RECORD
      codven                LIKE pro_mae_promotor.codven          ,
      seguro                LIKE pro_mae_promotor.seguro          ,
      nip                   LIKE pro_mae_promotor.nip             ,
      agenc_cod             LIKE pro_mae_promotor.agenc_cod       ,
      unico                 LIKE pro_mae_promotor.unico           ,
##      tipo_recibo           LIKE pro_mae_promotor.tipo_recibo     ,
      escolar               LIKE pro_mae_promotor.escolar         ,
      rfc                   CHAR(10)                              ,
      paterno               LIKE pro_mae_promotor.paterno         ,
      materno               LIKE pro_mae_promotor.materno         ,
      nombres               LIKE pro_mae_promotor.nombres         ,
      fingre                LIKE pro_mae_promotor.fingre          ,
      fenvio                LIKE pro_mae_promotor.fenvio          ,
      fecha_registro        LIKE pro_mae_promotor.fecha_registro  ,
      fecha_baja            LIKE pro_mae_promotor.fecha_baja      ,
      calle                 LIKE pro_mae_promotor.calle           ,
      numero                LIKE pro_mae_promotor.numero          ,
      dpto                  LIKE pro_mae_promotor.dpto            ,
      colonia               LIKE pro_mae_promotor.colonia         ,
      deleg                 LIKE pro_mae_promotor.deleg           ,      
      ciudad                LIKE pro_mae_promotor.ciudad          ,
      estado                LIKE pro_mae_promotor.estado          ,
      codpos                LIKE pro_mae_promotor.codpos          ,
      fono                  LIKE pro_mae_promotor.fono            ,
      sup                   LIKE pro_mae_promotor.sup             ,
      nivel                 LIKE pro_mae_promotor.nivel           ,
      resuelva              LIKE pro_mae_promotor.resuelva        ,
      son                   INTEGER                               ,
      fnaci                 LIKE pro_mae_promotor.fnaci           ,
      diag_proceso          LIKE pro_mae_promotor.diag_proceso    ,
      fautoriz              LIKE pro_mae_promotor.fautoriz        ,
      status                LIKE pro_mae_promotor.status          ,
      nro_solicitud         LIKE pro_mae_promotor.nro_solicitud   ,
      status_interno        LIKE pro_mae_promotor.status_interno  ,
      fecha_certifi         LIKE pro_mae_promotor.fecha_certifi   ,
      motivo_suspende       LIKE pro_mae_promotor.motivo_suspende ,
      fecha_suspende        LIKE pro_mae_promotor.fecha_suspende  ,
      fech_credencial       LIKE pro_mae_promotor.fech_credencial ,
      cod_promotor          LIKE pro_mae_promotor.cod_promotor    ,
      fecha_ult_mod         DATETIME YEAR TO SECOND,
      ftermino              DATE      ##Se agrego 251103
   END RECORD

   DEFINE gr_cuenta RECORD 
      cta_bancaria          LIKE pro_cta_promotor.cta_bancaria ,
      bco_cod               LIKE pro_cta_promotor.bco_cod      ,
      tipo_cuenta           LIKE pro_cta_promotor.tipo_cuenta  ,
      plaza                 LIKE pro_cta_promotor.plaza
   END RECORD

   DEFINE
      vstatus              LIKE pro_mae_promotor.status,
      vstatus_interno      LIKE pro_mae_promotor.status_interno,
      vdiagn_cod           LIKE tab_diagnos_pro.diagn_cod

   DEFINE
      ayo_x                 CHAR(10),
      aaa                   ,
      mm                    ,
      dd                    CHAR(02),
      z_fecha               CHAR(10),
      enter                 CHAR(01),
      desestad              CHAR(30),
      c13_rfc               CHAR(13),
      rfc_arma              CHAR(10),
      rfc_2                 CHAR(10),
      s_rfc                 CHAR(10),
      vvcodven              CHAR(11),
      vdesc_status_corta    CHAR(25),
      ventro                CHAR(1),
      v_sexo                CHAR(1),
      v_edo_naci            CHAR(2),
      vrfc                  CHAR(13)

   DEFINE
      j_fecha               ,
      HOY                   ,
      v_ftermino1           ,
      v_ftermino2           DATE

   DEFINE
      total_reg             INTEGER

   DEFINE
       ayo_1                 ,
       ayo_2                 ,
       ayo_s                 ,
       digito                ,
       sw_1                  SMALLINT

   DEFINE
      v_mod_sin_304         ,
      vrowid                INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    DISPLAY "                         " AT 5,60
    OPTIONS PROMPT LINE LAST ,
            INPUT WRAP       ,
            ACCEPT KEY CONTROL-I

    CALL STARTLOG("PROM025.log")

    CALL init() 
    OPEN WINDOW pantalla AT 2,2 WITH FORM "PROM0251" ATTRIBUTE(BORDER)
    DISPLAY " PROM025             MANTENEDOR MAESTRO DE ",
            "PROMOTORES                          " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY  HOY  USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)
    DISPLAY "                       DOMICILIO DE CORRESPONDEN",
            "CIA                            " AT 15,1 ATTRIBUTE(REVERSE) 
    DISPLAY "                       DATOS GENERALES DEL PROMO",
            "TOR                            " AT 7,1 ATTRIBUTE(REVERSE)

    MENU "MANTENIMIENTO"
        COMMAND KEY("C") "Consulta" "Consulta Promotor"
            CALL consulta() 
            CLEAR FORM

        COMMAND KEY("M") "Modifica" "Modifica Promotor"
            LET v_mod_sin_304 = 0
            CALL modifica(v_mod_sin_304)
            CLEAR FORM

        COMMAND KEY("T") "Mod.Est.Comer" "Actualiza Estructura Comercial"
            LET v_mod_sin_304 = 1
            CALL modifica(v_mod_sin_304)
            CLEAR FORM

        COMMAND KEY("P") "actualiza susPendido" "Actualiza Promotor"
            LET v_mod_sin_304 = 2
            CALL modifica_303(v_mod_sin_304)
            CLEAR FORM

        COMMAND KEY("R") "Restaura" "Restaura Promotor"
            CALL restaura() 
            CLEAR FORM                             

        COMMAND KEY("U") "sUspende" "Suspende Promotor"
            CALL suspende() 
            CLEAR FORM

        COMMAND KEY("A") "Activa" "Activar Promotor" 
            CALL activar()
            CLEAR FORM

        COMMAND KEY("O") "repOrte comercial" "Genera Reporte para Comercial" 
            CALL genera_rep_comer()
            CLEAR FORM

        COMMAND KEY("E") "rEporte maestro" "Genera Reporte del Maestro " 
            CALL genera_rep_maestro()
            CLEAR FORM

        COMMAND KEY ("S") "Salir" "Salir"
            EXIT PROGRAM
    END MENU
    CLOSE WINDOW pantalla
END MAIN

FUNCTION init()
#-------------
    LET HOY = TODAY
    LET v_mod_sin_304 = 0
END FUNCTION


FUNCTION  pidedato(reg_5, v_mod_sin_304)
#---------------------------------------

   DEFINE reg_5 RECORD
      codven                LIKE pro_mae_promotor.codven         ,
      cod_promotor          LIKE pro_mae_promotor.cod_promotor   ,
      agenc_cod             LIKE pro_mae_promotor.agenc_cod      ,
      desagenc              CHAR(60)                             ,
      nivel                 LIKE pro_mae_promotor.nivel          ,
      desnivel              CHAR(20)                             ,
      son                   INTEGER                              ,
      seguro                LIKE pro_mae_promotor.seguro         ,
      rfc                   CHAR(13)                             ,
      homonimia             CHAR(3)                               ,
      unico                 LIKE pro_mae_promotor.unico          ,
##      tipo_recibo           LIKE pro_mae_promotor.tipo_recibo    ,
      escolar               LIKE pro_mae_promotor.escolar        ,
      paterno               LIKE pro_mae_promotor.paterno        ,
      materno               LIKE pro_mae_promotor.materno        ,
      nombres               LIKE pro_mae_promotor.nombres        ,
      fnaci                 LIKE pro_mae_promotor.fnaci          ,
      fenvio                LIKE pro_mae_promotor.fenvio         ,
      fecha_registro        LIKE pro_mae_promotor.fecha_registro ,
      fecha_baja            LIKE pro_mae_promotor.fecha_baja     ,
      fecha_suspende        LIKE pro_mae_promotor.fecha_suspende ,
      resuelva              LIKE pro_mae_promotor.resuelva       ,
      sexo                  CHAR(1)                              , --(v10)
      edo_naci              CHAR(2)                              , --(v10)
      calle                 LIKE pro_mae_promotor.calle          ,
      numero                LIKE pro_mae_promotor.numero         ,
      dpto                  LIKE pro_mae_promotor.dpto           ,
      codpos                LIKE pro_mae_promotor.codpos         ,
      colonia               LIKE pro_mae_promotor.colonia        ,
      deleg                 LIKE pro_mae_promotor.deleg          ,
      desdeleg              CHAR(20)                             ,
      ciudad                LIKE pro_mae_promotor.ciudad         ,
      desciudad             CHAR(20)                             ,
      estado                LIKE pro_mae_promotor.estado         ,
      desestad              CHAR(20)                             ,
      fono                  LIKE pro_mae_promotor.fono           ,
      ftermino              DATE   ##Se agrega el 251103
   END RECORD

   DEFINE reg_4 RECORD
      codven                LIKE pro_mae_promotor.codven         ,
      cod_promotor          LIKE pro_mae_promotor.cod_promotor   ,
      agenc_cod             LIKE pro_mae_promotor.agenc_cod      ,
      desagenc              CHAR(60)                             ,
      nivel                 LIKE pro_mae_promotor.nivel          ,
      desnivel              CHAR(20)                             ,
      son                   INTEGER                              ,
      seguro                LIKE pro_mae_promotor.seguro         ,
      rfc                   CHAR(13)                             ,
      homonimia             CHAR(3)                              ,
      unico                 LIKE pro_mae_promotor.unico          ,
##      tipo_recibo           LIKE pro_mae_promotor.tipo_recibo    ,
      escolar               LIKE pro_mae_promotor.escolar        ,
      paterno               LIKE pro_mae_promotor.paterno        ,
      materno               LIKE pro_mae_promotor.materno        ,
      nombres               LIKE pro_mae_promotor.nombres        ,
      fnaci                 LIKE pro_mae_promotor.fnaci          ,
      fenvio                LIKE pro_mae_promotor.fenvio         ,
      fecha_registro        LIKE pro_mae_promotor.fecha_registro ,
      fecha_baja            LIKE pro_mae_promotor.fecha_baja     ,
      fecha_suspende        LIKE pro_mae_promotor.fecha_suspende ,
      resuelva              LIKE pro_mae_promotor.resuelva       ,
      sexo                  CHAR(1)                              , --(v10)
      edo_naci              CHAR(2)                              , --(v10)
      calle                 LIKE pro_mae_promotor.calle          ,
      numero                LIKE pro_mae_promotor.numero         ,
      dpto                  LIKE pro_mae_promotor.dpto           ,
      codpos                LIKE pro_mae_promotor.codpos         ,
      colonia               LIKE pro_mae_promotor.colonia        ,
      deleg                 LIKE pro_mae_promotor.deleg          ,
      desdeleg              CHAR(20)                             ,
      ciudad                LIKE pro_mae_promotor.ciudad         ,
      desciudad             CHAR(20)                             ,
      estado                LIKE pro_mae_promotor.estado         ,
      desestad              CHAR(20)                             ,
      fono                  LIKE pro_mae_promotor.fono           ,
      ftermino              DATE   ##Se agrega el 251103
   END RECORD

   DEFINE
      desciudad             ,
      desdeleg              ,
      desestad              ,
      desagenc              ,
      desnivel              CHAR(60) ,
      x_cod_resp_uni        CHAR(10) ,
      v_mod_sin_304         INTEGER  ,
      pasa                  SMALLINT,
      pasa_curp             SMALLINT,
      v_1                   SMALLINT,
      val_1                 CHAR(80),
      sexo_cur              CHAR(01),
      dig_curp              SMALLINT,
      desc_err              CHAR(60)


   LET reg_4.* = reg_5.*

   OPEN WINDOW prom0256 AT 2,2 WITH FORM "PROM0256" ATTRIBUTE(BORDER)
   DISPLAY " PROM025             MANTENEDOR MAESTRO DE PROMOTORES                          " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY  HOY  USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)
   DISPLAY "                       DOMICILIO DE CORRESPONDENCIA                            " AT 15,1 ATTRIBUTE(REVERSE)
   DISPLAY "                       DATOS GENERALES DEL PROMOTOR                            " AT 7,1 ATTRIBUTE(REVERSE)
   DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE)
   DISPLAY " Ctrl-C = Salir  Ctrl-B =  Cuenta Bancaria " AT 2,1

   DISPLAY " 1"  TO FORMONLY.son
   DISPLAY total_reg  TO FORMONLY.total_reg
   DISPLAY BY NAME reg_5.*

   CASE v_mod_sin_304
      WHEN 1
         INPUT BY NAME  reg_5.codven       ,
                        reg_5.agenc_cod    ,
                        reg_5.nivel        ,
                        reg_5.escolar      WITHOUT DEFAULTS

            AFTER FIELD codven
            -------------------

               IF reg_5.codven IS NULL THEN
                  ERROR "El numero de empleado no puede ser NULO"
                  NEXT FIELD  codven
               END IF

            AFTER FIELD agenc_cod
            ----------------------

               IF reg_5.agenc_cod IS NULL THEN
                  CALL ayuda_grupo_de_venta() #agdv
                  RETURNING reg_5.agenc_cod,reg_5.desagenc

                  IF reg_5.agenc_cod IS NULL THEN
                     DISPLAY reg_5.agenc_cod TO agenc_cod
                     DISPLAY reg_5.desagenc  TO desagenc
                     NEXT FIELD agenc_cod
                  END IF
               ELSE
                  SELECT A.nombre_uni_n1
                  INTO   reg_5.desagenc
                  FROM   com_nivel1 A
                  WHERE  A.coduni_n1 = reg_5.agenc_cod

                  IF STATUS = NOTFOUND THEN
                     ERROR "NO EXISTE EL GRUPO"
                     NEXT FIELD agenc_cod
                  END IF
               END IF

               DISPLAY reg_5.agenc_cod TO agenc_cod
               DISPLAY reg_5.desagenc  TO desagenc

            AFTER FIELD nivel
            -----------------

               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD agenc_cod
               END IF

               IF reg_5.nivel IS NULL THEN
                  CALL ayuda_tipo_promotor() #atp
                  RETURNING reg_5.nivel,reg_5.desnivel

                  IF reg_5.nivel IS NULL THEN
                      DISPLAY reg_5.nivel    TO nivel
                      DISPLAY reg_5.desnivel TO desnivel
                      NEXT FIELD nivel
                  END IF
               ELSE
                  SELECT A.desc_tipo
                  INTO   reg_5.desnivel
                  FROM   com_tipo_promotor A
                  WHERE  A.cod_tipo_prom = reg_5.nivel

                  IF STATUS = NOTFOUND THEN
                     ERROR "NO EXISTE EL NIVEL"
                     NEXT FIELD agenc_cod
                  END IF
               END IF

               DISPLAY reg_5.nivel    TO nivel
               DISPLAY reg_5.desnivel TO desnivel

            AFTER FIELD escolar
            -------------------

               IF reg_5.escolar IS NULL THEN
                  CALL grado_escolar()
                  RETURNING reg_5.escolar

                  IF reg_5.escolar IS NULL THEN
                     DISPLAY reg.escolar TO escolar
                     NEXT FIELD escolar
                  ELSE
                     DISPLAY reg_5.escolar TO escolar
                  END IF
               ELSE
                  IF reg_5.escolar NOT MATCHES "[ABCDEFGHIJKL]" THEN
                     ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                     NEXT FIELD escolar
                  END IF
               END IF

            ON KEY ( ESC )
               IF reg_5.codven IS NULL THEN
                  ERROR "El numero de empleado no puede ser NULO"
                  NEXT FIELD  codven
               END IF

               IF reg_5.agenc_cod IS NULL THEN
                  CALL ayuda_grupo_de_venta() #agdv
                  RETURNING reg_5.agenc_cod,reg_5.desagenc

                  IF reg_5.agenc_cod IS NULL THEN
                     DISPLAY reg_5.agenc_cod TO agenc_cod
                     DISPLAY reg_5.desagenc  TO desagenc
                     NEXT FIELD agenc_cod
                  END IF
               ELSE
                  SELECT A.nombre_uni_n1
                  INTO   reg_5.desagenc
                  FROM   com_nivel1 A
                  WHERE  A.coduni_n1 = reg_5.agenc_cod

                  IF STATUS = NOTFOUND THEN
                     ERROR "NO EXISTE EL GRUPO"
                     NEXT FIELD agenc_cod
                  END IF
               END IF

               DISPLAY reg_5.agenc_cod TO agenc_cod
               DISPLAY reg_5.desagenc  TO desagenc

               IF reg_5.nivel IS NULL THEN
                  CALL ayuda_tipo_promotor() #atp
                  RETURNING reg_5.nivel,reg_5.desnivel

                  IF reg_5.nivel IS NULL THEN
                     DISPLAY reg_5.nivel    TO nivel
                     DISPLAY reg_5.desnivel TO desnivel
                     NEXT FIELD nivel
                  END IF
               ELSE
                  SELECT A.desc_tipo
                  INTO   reg_5.desnivel
                  FROM   com_tipo_promotor A
                  WHERE  A.cod_tipo_prom = reg_5.nivel

                  IF STATUS = NOTFOUND THEN
                     ERROR "NO EXISTE EL NIVEL"
                     NEXT FIELD agenc_cod
                  END IF
               END IF

               DISPLAY reg_5.nivel    TO nivel
               DISPLAY reg_5.desnivel TO desnivel

               IF reg_5.escolar IS NULL THEN
                  CALL grado_escolar()
                  RETURNING reg_5.escolar

                  IF reg_5.escolar IS NULL THEN
                     DISPLAY reg.escolar TO escolar
                     NEXT FIELD escolar
                  ELSE
                      DISPLAY reg_5.escolar TO escolar
                  END IF
               ELSE
                  IF reg_5.escolar NOT MATCHES "[ABCDEFGHIJKL]" THEN
                     ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                     NEXT FIELD escolar
                  END IF
               END IF

               WHILE TRUE
                  PROMPT "DESEA MODIFICAR  S/N" FOR CHAR enter

                  IF enter MATCHES "[SsNn]" THEN
                     IF enter MATCHES "[Ss]" THEN
                        UPDATE pro_mae_promotor 
                        SET    pro_mae_promotor.codven       = reg_5.codven    ,
                               pro_mae_promotor.agenc_cod    = reg_5.agenc_cod ,
                               pro_mae_promotor.nivel        = reg_5.nivel     ,
                               pro_mae_promotor.escolar      = reg_5.escolar     
                        WHERE  pro_mae_promotor.cod_promotor = reg_5.cod_promotor
                     END IF

                     DISPLAY "REGISTRO MODIFICADO"
                     AT 21,2 ATTRIBUTE(REVERSE)
                     SLEEP 2
                     CLEAR FORM
                     EXIT WHILE
                  ELSE
                     EXIT WHILE
                  END IF
               END WHILE
 
               EXIT INPUT


            ON KEY(CONTROL-C,INTERRUPT)
               EXIT INPUT

         END INPUT

      WHEN 2 ################## MODIFICA SUSPENSION CURP _303 ###############


         INPUT BY NAME  reg_5.rfc,reg_5.unico  WITHOUT DEFAULTS

            BEFORE FIELD rfc
               NEXT FIELD unico

            AFTER FIELD unico
            ------------------

               IF reg_5.unico[1] <> " " OR
                  reg_5.unico IS NOT NULL THEN

                  IF reg_5.unico[11] = "H" THEN
                     LET sexo_cur = "1"
                  ELSE
                     LET sexo_cur = "2"
                  END IF

                  CALL valida_est_curp(reg_5.unico)
                  RETURNING pasa_curp, desc_err

                  IF pasa_curp = 1 THEN
                     ERROR "", desc_err
                     LET pasa_curp = 0
                     NEXT FIELD unico
                  END IF

                  CALL var_dig_curp(reg_5.unico) RETURNING pasa, dig_curp

                  IF pasa = 0 THEN
                     ERROR "Digito Verificador Invalido curp, el digito es : ",
                     dig_curp
                     LET pasa = 0
                     NEXT FIELD unico
                  END IF
               ELSE
                  LET sexo_cur = " "
               END IF
   
               IF reg_5.unico[1] = " " OR reg_5.unico IS NULL THEN
                   ERROR "Debe ingresar CURP correcta"
                   NEXT FIELD unico
               END IF

               IF reg_5.rfc[5,6] <> reg_5.unico[5,6] THEN
                  ERROR "El AÑO DEL RFC Y CURP SON DIFERENTES "
                  ATTRIBUTE(NORMAL)
                  SLEEP 3
                  ERROR ""
               END IF

            ON KEY ( ESC )
               IF reg_5.unico[1] <> " " OR
                  reg_5.unico IS NOT NULL THEN

                  IF reg_5.unico[11] = "H" THEN
                    LET sexo_cur = "1"
                  ELSE
                     LET sexo_cur = "2"
                  END IF

                  CALL valida_est_curp(reg_5.unico)
                  RETURNING pasa_curp, desc_err

                  IF pasa_curp = 1 THEN
                     ERROR "", desc_err
                     LET pasa_curp = 0
                     NEXT FIELD unico
                  END IF

                  CALL var_dig_curp(reg_5.unico) RETURNING pasa, dig_curp

                  IF pasa = 0 THEN
                     ERROR "Digito Verificador Invalido curp, el digito es : ",
                     dig_curp
                     LET pasa = 0
                     NEXT FIELD unico
                  END IF
               ELSE
                  LET sexo_cur = " "
               END IF

               IF reg_5.unico[1] = " " OR reg_5.unico IS NULL THEN
                   ERROR "Debe ingresar CURP correcta"
                   NEXT FIELD unico
               END IF

               IF reg_5.rfc[5,6] <> reg_5.unico[5,6] THEN
                  ERROR "El AÑO DEL RFC Y CURP SON DIFERENTES "
                  ATTRIBUTE(NORMAL)
                  SLEEP 3
                 ERROR ""
               END IF

               WHILE TRUE
                  PROMPT "DESEA MODIFICAR  S/N" FOR CHAR enter

                  IF enter MATCHES "[SsNn]" THEN
                     IF enter MATCHES "[Ss]" THEN
                        UPDATE pro_mae_promotor 
                        SET    pro_mae_promotor.unico = reg_5.unico
                        WHERE  pro_mae_promotor.cod_promotor = reg_5.cod_promotor

                        DISPLAY "REGISTRO MODIFICADO" AT 21,2 ATTRIBUTE(REVERSE)
                        SLEEP 2
                        CLEAR FORM
                        EXIT WHILE
                     ELSE
                        DISPLAY "REGISTRO NO MODIFICADO" AT 21,2 ATTRIBUTE(REVERSE)
                        SLEEP 2
                        CLEAR FORM
                        EXIT WHILE
                     END IF
                  ELSE
                     DISPLAY "REGISTRO NO MODIFICADO"
                     AT 21,2 ATTRIBUTE(REVERSE)
                     SLEEP 2
                     CLEAR FORM
                     EXIT WHILE
                  END IF
               END WHILE

               EXIT INPUT

            ON KEY(INTERRUPT, CONTROL-C)
               EXIT INPUT

         END INPUT


      WHEN 0 ################## MODIFICA REG ACTIVO  ###############
         INPUT BY NAME  reg_5.codven       ,
                        reg_5.cod_promotor ,
                        reg_5.agenc_cod    ,
                        reg_5.nivel        ,
                        reg_5.seguro       ,
                        reg_5.rfc          ,
                        reg_5.unico        ,
                        reg_5.escolar      ,
                        reg_5.paterno      ,
                        reg_5.materno      ,
                        reg_5.nombres      ,
                        reg_5.fnaci        ,
                        reg_5.sexo         ,
                        reg_5.edo_naci     ,
                        reg_5.calle        ,
                        reg_5.numero       ,
                        reg_5.dpto         ,
                        reg_5.codpos       ,
                        reg_5.colonia      ,
                        reg_5.deleg        ,
                        reg_5.ciudad       ,
                        reg_5.estado       ,
                        reg_5.fono  WITHOUT DEFAULTS 

            BEFORE FIELD codven
               NEXT FIELD agenc_cod

            AFTER FIELD agenc_cod
            ----------------------

               IF reg_5.agenc_cod IS NULL THEN
                  CALL ayuda_grupo_de_venta() #agdv
                  RETURNING reg_5.agenc_cod,reg_5.desagenc

                  IF reg_5.agenc_cod IS NULL THEN
                     DISPLAY reg_5.agenc_cod TO agenc_cod
                     DISPLAY reg_5.desagenc  TO desagenc
                     NEXT FIELD agenc_cod
                  END IF
               ELSE
                  SELECT A.nombre_uni_n1
                  INTO   reg_5.desagenc
                  FROM   com_nivel1 A
                  WHERE  A.coduni_n1 = reg_5.agenc_cod

                  IF STATUS = NOTFOUND THEN
                     ERROR "NO EXISTE EL GRUPO"
                     NEXT FIELD agenc_cod
                  END IF
               END IF

               DISPLAY reg_5.agenc_cod TO agenc_cod
               DISPLAY reg_5.desagenc  TO desagenc

            AFTER FIELD nivel
            ------------------

               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD agenc_cod
               END IF

               IF reg_5.nivel IS NULL THEN

                  WHILE TRUE
                     PROMPT " DESEA DESPLEGAR EL CATALOGO S/N ?"
                     FOR CHAR enter

                     IF enter MATCHES "[SsNn]" THEN
                        EXIT WHILE
                     END IF
                  END WHILE

                  IF enter MATCHES "[Ss]" THEN
                     CALL ayuda_tipo_promotor() #atp
                     RETURNING reg_5.nivel,reg_5.desnivel

                     IF reg_5.nivel IS NULL THEN
                        DISPLAY reg_5.nivel    TO nivel
                        DISPLAY reg_5.desnivel TO desnivel
                        NEXT FIELD nivel
                     END IF
                  ELSE
                     LET reg_5.desnivel = " "

                     DISPLAY reg_5.desnivel TO desnivel

                     NEXT FIELD seguro
                  END IF
               ELSE
                  SELECT UNIQUE A.desc_tipo
                  INTO   reg_5.desnivel
                  FROM   com_tipo_promotor A
                  WHERE  A.cod_tipo_prom = reg_5.nivel

                  IF STATUS = NOTFOUND THEN
                     ERROR "NO EXISTE EL NIVEL"
                     NEXT FIELD agenc_cod
                  END IF
               END IF

               DISPLAY reg_5.nivel    TO nivel
               DISPLAY reg_5.desnivel TO desnivel


            AFTER FIELD seguro
            -------------------

               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                   NEXT FIELD PREVIOUS
               END IF

               LET sw_1 = 0

               IF reg_5.seguro IS NULL OR reg_5.seguro = " " THEN
                  ERROR " DEBE INGRESAR RFC CON HOMOCLAVE " ATTRIBUTE(NORMAL)
                  NEXT FIELD rfc
               END IF

               LET sw_1 = 1

               IF reg_5.seguro IS NOT NULL AND reg_5.seguro[1,1] <> " " THEN
                  IF LENGTH(reg_5.seguro) <> 11 THEN
                     ERROR " DEBE INGRESAR NSS COMPLETO" ATTRIBUTE(NORMAL)
                     NEXT FIELD seguro
                  END IF

                  CALL  digito_verif(reg_5.seguro[1,10],10)
                  RETURNING digito

                  IF digito = 32000 THEN
                     ERROR " NSS FORMADO CON DATOS INCORRECTOS" ATTRIBUTE(NORMAL)
                     NEXT FIELD seguro
                  END IF

                  IF LENGTH(reg_5.seguro) = 11 AND
                     digito <> reg_5.seguro[11] THEN

                     ERROR " DIGITO VERIFICADOR INCORRECTO "
                     ATTRIBUTE(NORMAL)
                     SLEEP 2

                     WHILE TRUE
                        PROMPT " DESEA CONTINUAR   S/N  ?"
                        FOR CHAR enter

                        IF enter MATCHES "[SsNn]" THEN
                           EXIT WHILE
                        END IF
                     END WHILE

                     IF enter MATCHES "[Ss]" THEN
                        NEXT FIELD rfc
                     ELSE
                        NEXT FIELD seguro
                     END IF

                     NEXT FIELD seguro
                  END IF

                  IF reg_5.seguro[11] <> "1" AND
                     reg_5.seguro[11] <> "2" AND
                     reg_5.seguro[11] <> "3" AND
                     reg_5.seguro[11] <> "4" AND
                     reg_5.seguro[11] <> "5" AND
                     reg_5.seguro[11] <> "6" AND
                     reg_5.seguro[11] <> "7" AND
                     reg_5.seguro[11] <> "8" AND
                     reg_5.seguro[11] <> "9" AND
                     reg_5.seguro[11] <> "0" THEN

                     ERROR " NSS CON DATOS INCORRECTOS "
                     NEXT FIELD seguro
                  END IF
               ELSE
                   ERROR " NO PUEDE SER NULO O COMENZAR CON BLANCOS "
                   NEXT FIELD seguro
               END IF

{
               SELECT "a.X"
               FROM   pro_solicitud a
               WHERE  a.seguro = reg_5.seguro

               IF SQLCA.SQLCODE = 0 THEN
                  ERROR " YA SE ENCUENTRA REGISTRADO ESTE NSS "
                  NEXT FIELD seguro
               ELSE
}
                  SELECT "a.X"
                  FROM pro_mae_promotor a
                  WHERE a.seguro = reg_5.seguro

                  IF SQLCA.SQLCODE = 0 THEN
                     WHILE TRUE
                         PROMPT " NSS YA REGISTRADO PARA OTRO PROMOTOR...DESEA CONTINUAR  S/N ? "
                         FOR CHAR enter

                         IF enter MATCHES "[SsNn]" THEN
                            EXIT WHILE
                         END IF
                     END WHILE

                     IF enter MATCHES "[Ss]" THEN
                        NEXT FIELD rfc
                     ELSE
                        NEXT FIELD seguro
                     END IF
                  END IF
--               END IF

               NEXT FIELD rfc


            AFTER FIELD rfc
            ----------------

               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD PREVIOUS
               END IF

               IF reg_5.rfc IS NULL OR
                  reg_5.rfc = " " THEN

                  ERROR " RFC NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                  NEXT FIELD rfc
               END IF

               IF LENGTH(reg_5.rfc CLIPPED) <> 10 AND
                  LENGTH(reg_5.rfc CLIPPED) <> 13 THEN
                  ERROR " DEBE INGRESAR RFC COMPLETO " ATTRIBUTE(NORMAL)
                  NEXT FIELD rfc
               END IF

               IF NOT valida_fecha_rfc(reg_5.rfc[5,10]) THEN
                  ERROR " FORMATO DE RFC INCORRECTO ( ----AAMMDD*** ) "
                  NEXT FIELD rfc
               ELSE
                  WHENEVER ERROR CONTINUE
                     LET aaa     = reg_5.rfc[5,6]
                     LET mm      = reg_5.rfc[7,8]
                     LET dd      = reg_5.rfc[9,10]
                     LET z_fecha = mm,"/",dd,"/19",aaa
                     LET j_fecha = z_fecha

                     IF j_fecha IS NULL THEN
                         ERROR " FECHA INVALIDA EN RFC " ATTRIBUTE(NORMAL)
                         NEXT FIELD rfc
                     END IF
                  WHENEVER ERROR STOP

                  IF reg_5.fnaci IS NULL OR reg_5.fnaci = " " THEN
                      INITIALIZE ayo_x TO NULL
                      LET ayo_s = 0
                      LET ayo_1 = 0
                      LET ayo_2 = 0

                      LET ayo_x = HOY 
                      LET ayo_1 = ayo_x[7,10]
                      LET ayo_x = reg.fnaci
                      LET ayo_2 = ayo_x[7,10]
                      LET ayo_s = ayo_1 - ayo_2

                      IF ayo_s < 14 THEN
                         ERROR "  FECHA INVALIDA EN RFC TIENE MENOS DE 14 AÑOS "
                         ATTRIBUTE(NORMAL)
                         NEXT FIELD rfc
                      END IF

                      LET reg_5.fnaci = z_fecha
                      DISPLAY BY NAME reg_5.fnaci
                  END IF
               END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
                  FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN

                  LET v_1 = 0

                  INITIALIZE val_1 TO NULL

                  CALL verifica_rfc(reg_5.rfc[1,4])
                  RETURNING v_1,val_1 #ve--

                  IF v_1 = 1 THEN
                     ERROR " RFC ",val_1 CLIPPED
                     NEXT FIELD rfc
                  END IF

                  IF reg_5.seguro IS NOT NULL THEN
                     IF reg_5.seguro[5,6] <> reg_5.rfc[5,6] THEN
                        ERROR " EL AÑO DEL NSS Y EL RFC SON DIFERENTES "
                        ATTRIBUTE(NORMAL)
                        SLEEP 3
                        ERROR ""
                     END IF
                  END IF
                  NEXT FIELD unico
               END IF

               IF sw_1 = 0 THEN
                  IF LENGTH(reg_5.rfc) < 13 THEN
                     ERROR " "
                     ERROR " DEBIO HABER INGRESADO EL RFC CON HOMOCLAVE "
                     ATTRIBUTE(NORMAL)
                  END IF
               END IF

               IF reg_5.rfc IS NOT NULL OR reg_5.rfc[1,2] <> "  " THEN

                  LET v_1 = 0

                  INITIALIZE val_1 TO NULL

                  CALL verifica_rfc(reg.rfc[1,4])
                  RETURNING v_1,val_1 #ve--

                  IF v_1 = 1 THEN
                     ERROR " RFC ",val_1 CLIPPED
                     NEXT FIELD rfc
                  END IF

                  IF reg_5.seguro IS NOT NULL THEN
                     IF reg_5.seguro[5,6] <> reg_5.rfc[5,6] THEN
                        ERROR " EL AÑO DEL NSS Y EL RFC SON DIFERENTES "
                        ATTRIBUTE(NORMAL)
                        SLEEP 3
                        ERROR ""
                     END IF
                  END IF

                  NEXT FIELD unico

                  IF NOT valida_fecha_rfc(reg_5.rfc[5,10]) THEN
                     ERROR " FORMATO DE RFC INCORRECTO ( ----AAMMDD*** )"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD rfc
                  ELSE
                     WHENEVER ERROR CONTINUE
                       LET aaa     = reg.rfc[5,6]
                       LET mm      = reg.rfc[7,8]
                       LET dd      = reg.rfc[9,10]
                       LET z_fecha = mm,"/",dd,"/19",aaa
                       LET j_fecha = z_fecha

                       IF j_fecha IS NULL THEN
                           ERROR " FECHA INVALIDA EN RFC" ATTRIBUTE(NORMAL)
                           NEXT FIELD rfc
                       END IF
                     WHENEVER ERROR STOP

                     IF reg_5.fnaci IS NULL OR reg_5.fnaci = " " THEN
                        INITIALIZE ayo_x TO NULL
                        LET ayo_s = 0
                        LET ayo_1 = 0
                        LET ayo_2 = 0

                        LET ayo_x = HOY
                        LET ayo_1 = ayo_x[7,10]
                        LET ayo_x = reg.fnaci
                        LET ayo_2 = ayo_x[7,10]
                        LET ayo_s = ayo_1 - ayo_2

                        IF ayo_s < 14 THEN
                           ERROR " FECHA INVALIDA EN RFC TIENE MENOS DE 14 AÑOS "
                           ATTRIBUTE(NORMAL)
                           NEXT FIELD rfc
                        END IF

                        LET reg_5.fnaci = z_fecha

                        DISPLAY BY NAME reg_5.fnaci
                     END IF
                  END IF
               END IF

               IF reg_5.rfc IS NOT NULL THEN
                  LET vrfc = reg_5.rfc[1,10]
                  CALL rfc_promotor(vrfc)
               END IF


            AFTER FIELD unico
            -----------------

               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  IF reg_5.seguro IS NOT NULL AND reg_5.unico IS NOT NULL THEN
                     IF reg_5.seguro[5,6] <> reg_5.rfc[5,6] AND 
                        reg_5.rfc[5,6] <> reg_5.unico[5,6]  THEN
                        ERROR "EL AÑO DEL NSS, RFC Y CURP SON DIFERENTES "
                        ATTRIBUTE(NORMAL)
                        SLEEP 3
                        ERROR ""
                     END IF
                  END IF

                  IF reg_5.seguro IS NULL THEN
                     IF reg_5.rfc[5,6] <> reg_5.unico[5,6] THEN
                        ERROR "EL AÑO DEL RFC Y CURP SON DIFERENTES "
                        ATTRIBUTE(NORMAL)
                        SLEEP 3
                        ERROR ""
                     END IF
                  END IF

                  NEXT FIELD rfc
               END IF

               IF LENGTH(reg_5.unico) < 18 AND
                  LENGTH(reg_5.unico) > 0  THEN
                   ERROR " DEBE INGRESAR CURP COMPLETA "
                   ATTRIBUTE(NORMAL)
                   NEXT FIELD unico
               ELSE
                   IF reg_5.unico[1] <> " " OR
                      reg_5.unico IS NOT NULL THEN
                       IF reg_5.unico[11] = "H" THEN
                           LET sexo_cur = "1"
                       ELSE
                           LET sexo_cur = "2"
                       END IF

                       CALL valida_est_curp(reg_5.unico)
                       RETURNING pasa_curp, desc_err

                       IF pasa_curp = 1 THEN
                          ERROR "", desc_err
                          LET pasa_curp = 0
                          NEXT FIELD unico
                       END IF

                       CALL var_dig_curp(reg_5.unico)
                       RETURNING pasa, dig_curp

                       IF pasa = 0 THEN
                          ERROR " DIGITO VERIFICADOR INVALIDO CURP, EL DIGITO ES : ",
                          dig_curp
                          LET pasa = 0
                          NEXT FIELD unico
                       END IF
                   ELSE
                      LET sexo_cur = " "
                   END IF
               END IF

               IF reg_5.unico[1] = " " OR reg_5.unico IS NULL THEN
                   ERROR " DEBE INGRESAR CURP CORRECTA "
                   NEXT FIELD unico
               END IF

               IF reg_5.seguro IS NOT NULL AND reg_5.unico IS NOT NULL THEN
                  IF reg_5.seguro[5,6] <> reg_5.rfc[5,6] AND
                     reg_5.rfc[5,6] <> reg_5.unico       THEN
                     ERROR "EL AÑO DEL NSS, RFC Y CURP SON DIFERENTES "
                     ATTRIBUTE(NORMAL)
                     SLEEP 3
                     ERROR ""
                  END IF
               END IF

               IF reg_5.seguro IS NULL THEN
                  IF reg_5.rfc[5,6] <> reg_5.unico[5,6] THEN
                     ERROR "EL AÑO DEL RFC Y CURP SON DIFERENTES "
                     ATTRIBUTE(NORMAL)
                     SLEEP 3
                     ERROR ""
                  END IF

                  IF reg_5.rfc[1,4] <> reg.unico[1,4] THEN
                     ERROR "  LOS DATOS DEL RFC Y CURP SON DIFERENTES "
                     ATTRIBUTE(NORMAL)
                     SLEEP 3
                     ERROR ""
                  END IF
               END IF

               --(V10) inicia
               CALL curp(reg_5.unico)
               RETURNING reg_5.sexo,
               reg_5.edo_naci

               DISPLAY BY NAME   reg_5.sexo
               DISPLAY BY NAME   reg_5.edo_naci

               --(V10) finaliza

               NEXT FIELD escolar


            AFTER FIELD escolar
            -------------------

               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD nivel
               END IF

               IF reg_5.escolar IS NULL THEN
                  CALL grado_escolar()
                  RETURNING reg_5.escolar

                  IF reg_5.escolar IS NULL THEN
                     DISPLAY reg.escolar TO escolar
                     NEXT FIELD escolar
                  ELSE
                     DISPLAY reg_5.escolar TO escolar
                     NEXT FIELD paterno
                  END IF
               ELSE
                  IF reg_5.escolar MATCHES "[ABCDEFGHIJKL]" THEN
                     NEXT FIELD paterno
                  ELSE
                     ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                     NEXT FIELD escolar
                  END IF
               END IF


            AFTER FIELD paterno
            --------------------

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               IF reg_5.seguro IS NOT NULL AND reg_5.unico IS NOT NULL THEN
                  IF reg_5.seguro[5,6] <> reg_5.rfc[5,6] AND
                     reg_5.rfc[5,6] <> reg_5.unico       THEN
                     ERROR "EL AÑO DEL NSS, RFC Y CURP SON DIFERENTES "
                     ATTRIBUTE(NORMAL)
                     SLEEP 3
                     ERROR ""
                  END IF

                  IF reg_5.rfc[1,4] <> reg_5.unico[1,4] THEN
                     ERROR "  LOS DATOS DEL RFC Y CURP SON DIFERENTES "
                     ATTRIBUTE(NORMAL)
                     SLEEP 3
                     ERROR ""
                  END IF
               END IF

               IF reg_5.seguro IS NULL THEN
                  IF reg_5.rfc[5,6] <> reg_5.unico[5,6] THEN
                     ERROR "EL AÑO DEL RFC Y CURP SON DIFERENTES "
                     ATTRIBUTE(NORMAL)
                     SLEEP 3
                     ERROR ""
                  END IF
               END IF

               NEXT FIELD unico
            END IF

            IF reg_5.paterno IS NULL OR reg_5.paterno[1] = " " THEN
               ERROR "  EL APELLIDO PATERNO DEBE SER INGRESADO"
               ATTRIBUTE(NORMAL)
               NEXT FIELD paterno
            END IF


           AFTER FIELD materno
           --------------------

           IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
              NEXT FIELD paterno
           END IF

           IF reg_5.materno[1] = " "  OR reg_5.materno[1,2] = " ." OR
              reg_5.materno[1] = "."  OR reg_5.materno[1,2] = ".." OR 
              reg_5.materno[1] = "X " OR reg_5.materno[1,2] = " X" OR 
              reg_5.materno[1,2] = ".X"  THEN
              ERROR "  EL APELLIDO MATERNO NO PUEDE SER UN PUNTO o X "
                    ATTRIBUTE(NORMAL) 
              NEXT FIELD materno  
           END IF


           AFTER FIELD nombres
           --------------------

           IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
              NEXT FIELD materno
           END IF

           IF reg_5.nombres  IS NULL OR reg_5.nombres[1] = " " THEN
              ERROR "  EL NOMBRE ES REQUERIDO" ATTRIBUTE(NORMAL)
              NEXT FIELD nombres
           END IF

           INITIALIZE rfc_arma TO NULL

           CALL arma_clave_rfc(reg_5.paterno,
                               reg_5.materno,
                               reg_5.nombres,
                               reg_5.fnaci) RETURNING rfc_arma #rac

           IF reg_5.rfc <> rfc_arma THEN
              ERROR "  NO COINCIDE EL NOMBRE CON EL RFC " ATTRIBUTE(NORMAL)
              SLEEP 2
              ERROR ""
           END IF


           AFTER FIELD fnaci
           ------------------

           IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
              NEXT FIELD nombres
           END IF

           IF reg_5.fnaci  IS NULL THEN
              ERROR "  LA FECHA DE NACIMIENTOE ES REQUERIDA " ATTRIBUTE(NORMAL)
              NEXT FIELD fnaci
           END IF

           IF reg_5.fnaci IS NOT NULL THEN
              INITIALIZE ayo_x TO NULL
              LET ayo_s = 0
              LET ayo_1 = 0
              LET ayo_2 = 0

              LET ayo_x = HOY
              LET ayo_1 = ayo_x[7,10]
              LET ayo_x = reg.fnaci
              LET ayo_2 = ayo_x[7,10]
              LET ayo_s = ayo_1 - ayo_2

              IF ayo_s < 14 THEN
                 ERROR "  EL PROMOTOR TIENE MENOS DE 14 AÑOS "
                 NEXT FIELD fnaci
              END IF
           END IF

           ---valida rfc
           CALL valida_rfc(reg_5.paterno,
                           reg_5.materno,
                           reg_5.nombres,
                           reg_5.fnaci)
           RETURNING  s_rfc

           LET rfc_2 = reg_5.rfc[1,10]

           IF s_rfc <> rfc_2 THEN
              WHILE TRUE 
                  PROMPT " RFC INCORRECTO DEBE DE SER :",
                         s_rfc,
                         "... DESEA CONTINUAR  S/N ?"
                  FOR CHAR enter

                  IF enter MATCHES "[SsNn]" THEN
                     EXIT WHILE
                  END IF
              END WHILE

              IF enter MATCHES "[Ss]" THEN
                 INITIALIZE ayo_x TO NULL
                 LET ayo_s = 0
                 LET ayo_1 = 0
                 LET ayo_2 = 0

                 LET ayo_x = HOY
                 LET ayo_1 = ayo_x[7,10]
                 LET ayo_x = reg.fnaci
                 LET ayo_2 = ayo_x[7,10]
                 LET ayo_s = ayo_1 - ayo_2

                 IF ayo_s < 14 THEN
                    ERROR "  EL PROMOTOR TIENE MENOS DE 14 AÑOS "
                    NEXT FIELD fnaci
                 END IF
                 NEXT FIELD calle
              ELSE
                 NEXT FIELD rfc
              END IF

           END IF

            AFTER FIELD calle
            -----------------

               IF reg_5.calle IS NULL THEN
                  ERROR "CAMPO NO PUEDE SER NULO"
                  NEXT FIELD calle
               END IF

            AFTER FIELD numero
            -------------------

               IF reg_5.numero IS NULL THEN
                  ERROR "CAMPO NO PUEDE SER NULO"
                  NEXT FIELD numero
               END IF


            AFTER FIELD codpos
            ------------------

               IF reg_5.codpos IS NULL THEN
                  CALL despliega_codigo_postal() #dcp
                  RETURNING reg_5.codpos  ,
                            reg_5.colonia ,
                            reg_5.deleg   ,
                            desdeleg      ,
                            reg_5.ciudad  ,
                            desciudad     ,
                            reg_5.estado  ,
                            desestad
               ELSE
                  SELECT UNIQUE "X"
                  FROM   tab_codpos
                  WHERE  cpos_cod = reg_5.codpos

                  IF STATUS = NOTFOUND THEN
                     ERROR "COD.POST.NO EXISTE EN CATALOGO..",
                           "NULO DESPLIEGA AYUDA"
                     NEXT FIELD codpos
                  END IF

                  CALL despliega_colonias(reg_5.codpos)
                  RETURNING reg_5.colonia ,
                            reg_5.deleg   ,
                            desdeleg      ,
                            reg_5.ciudad  ,
                            desciudad     ,
                            reg_5.estado  ,
                            desestad
               END IF

               DISPLAY BY NAME reg_5.colonia ,
                                   reg_5.deleg   ,
                                   reg_5.ciudad  ,
                                   reg_5.estado

               DISPLAY desdeleg, desciudad, desestad
               TO desdeleg, desciudad, desestad


            AFTER FIELD fono
            -----------------
               IF reg_5.fono IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO ..."
                  NEXT FIELD fono  
               END IF

            ON KEY( CONTROL-B )
               CALL modifica_cuenta_promotor(reg.codven)

            ON KEY( ESC )
               SELECT UNIQUE "X"
               FROM   tab_codpos
               WHERE  cpos_cod = reg_5.codpos

               IF STATUS = NOTFOUND THEN
                  ERROR "COD.POST.NO EXISTE EN CATALOGO...NULO DESPLIEGA AYUDA"
                  NEXT FIELD codpos
               END IF

               IF reg_5.colonia IS NULL THEN
                  ERROR "CAMPO NO PUEDE SER NULO"
                  NEXT FIELD  codpos
               END IF

               IF reg_5.rfc IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD rfc  
               END IF

               IF reg_5.paterno IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD paterno  
               END IF

               IF reg_5.materno IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD materno  
               END IF

               IF reg_5.nombres IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD nombres  
               END IF

               IF reg_5.escolar IS NULL THEN

                  CALL grado_escolar()
                  RETURNING reg_5.escolar

                  IF reg_5.escolar IS NULL THEN
                     DISPLAY reg.escolar TO escolar
                     NEXT FIELD escolar
                  ELSE
                     DISPLAY reg_5.escolar TO escolar
                     NEXT FIELD calle
                  END IF
               ELSE
                  IF reg_5.escolar NOT MATCHES "[ABCDEFGHIJKL]" THEN
                     ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                     NEXT FIELD escolar
                  END IF
               END IF

               IF reg_5.calle IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD calle
               END IF

               IF reg_5.numero IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD numero
               END IF

               IF reg_5.colonia IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD colonia  
               END IF

               IF reg_5.deleg IS NULL THEN 
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD deleg  
               END IF

               IF reg_5.ciudad IS NULL THEN 
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD ciudad  
               END IF

               IF reg_5.estado IS NULL THEN 
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD estado  
               END IF

               IF reg_5.codpos IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD codpos    
               END IF

               IF reg_5.agenc_cod IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD agenc_cod    
               END IF

               IF reg_5.nivel IS NULL THEN
                  ERROR " CAMPO NO PUEDE SER NULO"
                  NEXT FIELD nivel    
               END IF

               WHILE TRUE
                 PROMPT "DESEA MODIFICAR S/N ?" FOR CHAR enter

                 IF enter MATCHES "[SsNn]" THEN
                    IF enter MATCHES "[Ss]" THEN
                       LET c13_rfc[1,10]  = reg_5.rfc
                       --svera LET c13_rfc[11,13] = reg_5.homonimia[1,3]

                       IF reg_4.seguro   <> reg_5.seguro   OR
                          reg_4.rfc      <> reg_5.rfc      OR
                          reg_4.unico    <> reg_5.unico    OR
                          reg_4.escolar  <> reg_5.escolar  OR
                          reg_4.paterno  <> reg_5.paterno  OR
                          reg_4.materno  <> reg_5.materno  OR
                          reg_4.nombres  <> reg_5.nombres  OR
                          reg_4.fnaci    <> reg_5.fnaci    OR
                          reg_4.sexo     <> reg_5.sexo     OR
                          reg_4.edo_naci <> reg_5.edo_naci OR
                          reg_4.calle    <> reg_5.calle    OR
                          reg_4.numero   <> reg_5.numero   OR
                          reg_4.dpto     <> reg_5.dpto     OR
                          reg_4.colonia  <> reg_5.colonia  OR
                          reg_4.deleg    <> reg_5.deleg    OR
                          reg_4.ciudad   <> reg_5.ciudad   OR
                          reg_4.estado   <> reg_5.estado   OR
                          reg_4.codpos   <> reg_5.codpos   OR
                          reg_4.fono     <> reg_5.fono THEN

                          INSERT INTO pro_his_mod
                          VALUES (reg_mod.codven         ,
                                  reg_mod.seguro         ,
                                  reg_mod.nip            ,
                                  reg_mod.agenc_cod      ,
                                  reg_mod.unico          ,
                                  reg_mod.rfc            ,
                                  reg_mod.paterno        ,
                                  reg_mod.materno        ,
                                  reg_mod.nombres        ,
                                  reg_mod.fingre         ,
                                  reg_mod.fenvio         ,
                                  reg_mod.fecha_registro ,
                                  reg_mod.fecha_baja     ,
                                  reg_mod.calle          ,
                                  reg_mod.numero         ,
                                  reg_mod.dpto           ,
                                  reg_mod.colonia        ,
                                  reg_mod.deleg          ,
                                  reg_mod.ciudad         ,
                                  reg_mod.estado         ,
                                  reg_mod.codpos         ,
                                  reg_mod.fono           ,
                                  reg_mod.sup            ,
                                  reg_mod.nivel          ,
                                  reg_mod.resuelva       ,
                                  reg_mod.fnaci          ,
                                  reg_mod.diag_proceso   ,
                                  reg_mod.fautoriz       ,
                                  reg_mod.status         ,
                                  reg_mod.nro_solicitud  ,
                                  reg_mod.status_interno ,
                                  reg_mod.fecha_certifi  ,
                                  reg_mod.motivo_suspende,
                                  reg_mod.fecha_suspende ,
                                  reg_mod.fech_credencial,
                                  reg_mod.cod_promotor   ,
                                  reg_mod.escolar        ,
                                  reg_4.sexo             ,
                                  reg_4.edo_naci         ,
                                  reg_mod.fecha_ult_mod)

                          UPDATE pro_mae_promotor 
                          SET    pro_mae_promotor.seguro     = reg_5.seguro    ,
                                 pro_mae_promotor.rfc        = reg_5.rfc       ,
                                 pro_mae_promotor.unico      = reg_5.unico     ,
                                 pro_mae_promotor.escolar    = reg_5.escolar   ,
                                 pro_mae_promotor.paterno    = reg_5.paterno   ,
                                 pro_mae_promotor.materno    = reg_5.materno   ,
                                 pro_mae_promotor.nombres    = reg_5.nombres   ,
                                 pro_mae_promotor.fnaci      = reg_5.fnaci     ,
--                                 pro_mae_promotor.sexo       = reg_5.sexo      ,
--                                 pro_mae_promotor.edo_naci   = reg_5.edo_naci  ,
                                 pro_mae_promotor.resuelva   = reg_5.resuelva  ,
                                 pro_mae_promotor.calle      = reg_5.calle     ,
                                 pro_mae_promotor.numero     = reg_5.numero    ,
                                 pro_mae_promotor.dpto       = reg_5.dpto      ,
                                 pro_mae_promotor.colonia    = reg_5.colonia   ,
                                 pro_mae_promotor.deleg      = reg_5.deleg     ,
                                 pro_mae_promotor.ciudad     = reg_5.ciudad    ,
                                 pro_mae_promotor.estado     = reg_5.estado    ,
                                 pro_mae_promotor.codpos     = reg_5.codpos    ,
                                 pro_mae_promotor.fono       = reg_5.fono      ,
                                 pro_mae_promotor.nivel      = reg_5.nivel     ,
                                 pro_mae_promotor.agenc_cod  = reg_5.agenc_cod,
                                 pro_mae_promotor.status_interno = 6
                          WHERE  pro_mae_promotor.cod_promotor = reg_5.cod_promotor
                       ELSE
                          UPDATE pro_mae_promotor 
                          SET    pro_mae_promotor.seguro   = reg_5.seguro    ,
                                 pro_mae_promotor.unico    = reg_5.unico     ,
                                 pro_mae_promotor.escolar  = reg_5.escolar    ,
                                 pro_mae_promotor.resuelva = reg_5.resuelva  ,
                                 pro_mae_promotor.rfc      = c13_rfc         ,
                                 pro_mae_promotor.calle    = reg_5.calle     ,
                                 pro_mae_promotor.numero   = reg_5.numero    ,
                                 pro_mae_promotor.dpto     = reg_5.dpto      ,
                                 pro_mae_promotor.colonia  = reg_5.colonia   ,
                                 pro_mae_promotor.deleg    = reg_5.deleg     ,
                                 pro_mae_promotor.ciudad   = reg_5.ciudad    ,
                                 pro_mae_promotor.estado   = reg_5.estado    ,
                                 pro_mae_promotor.codpos   = reg_5.codpos    ,
                                 pro_mae_promotor.fono     = reg_5.fono      ,
                                 pro_mae_promotor.nivel    = reg_5.nivel     ,
                                 pro_mae_promotor.fnaci    = reg_5.fnaci     ,
                                 pro_mae_promotor.agenc_cod  = reg_5.agenc_cod
                          WHERE  pro_mae_promotor.cod_promotor = reg_5. cod_promotor
                       END IF

                       DISPLAY "REGISTRO MODIFICADO"
                       AT 21,2 ATTRIBUTE(REVERSE) 
                       SLEEP 2
                       CLEAR FORM
                       EXIT WHILE
                    ELSE
                       EXIT WHILE
                    END IF
                 END IF
               END WHILE

               EXIT INPUT

            ON KEY(INTERRUPT, CONTROL-C)
               EXIT INPUT
         END INPUT
   END CASE

   CLOSE WINDOW prom0256

END FUNCTION

FUNCTION modifica_303(v_mod_sin_304)
#m-----------------
    DEFINE #loc #char
        v_mod_sin_304         INTEGER   ,
        x_busca               CHAR(500) ,
        txt_1                 CHAR(1500) 

    DEFINE #loc #smallint
        arr_c                 ,
        arc                   ,
        pos                   INTEGER
           
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA " at 1,65 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C = Salir  Enter = Seleccion  Ctrl-B =  Cuenta Bancaria " AT 2,1

    LET sw_1     = 0
    LET INT_FLAG = FALSE

    INITIALIZE x_busca, txt_1 TO NULL

    CONSTRUCT BY NAME x_busca ON A.codven,
                                 A.cod_promotor,
                                 A.paterno,
                                 A.materno,
                                 A.nombres
        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT,CONTROL-C)
            LET sw_1 = 1
            EXIT CONSTRUCT
    END CONSTRUCT

    IF sw_1 = 1 THEN
        RETURN
    END IF

    LET txt_1 = " SELECT A.codven          ,",
                "        A.cod_promotor    ,",
                "        A.agenc_cod       ,",
                "        ' '               ,", #desagenc
                "        A.nivel           ,",
                "        ' '               ,", #desnivel
                "        0                 ,", #num. ocurrencia
                "        A.seguro          ,",
                "        A.rfc[1,10]       ,",
                "        A.rfc[11,13]      ,",
                "        A.unico           ,",
##                "        A.tipo_recibo     ,",
                "        A.escolar         ,",
                "        A.paterno         ,",
                "        A.materno         ,",
                "        A.nombres         ,",
                "        A.fnaci           ,",
                "        A.fenvio          ,",
                "        A.fecha_registro  ,",
                "        A.fecha_baja      ,",
                "        A.fecha_suspende  ,",
                "        A.resuelva        ,",

                "        ' '               ,", --(v10)
                "        ' '               ,", --(v10)

                "        A.calle           ,",
                "        A.numero          ,",
                "        A.dpto            ,",
                "        A.codpos          ,",
                "        A.colonia         ,",
                "        A.deleg           ,",
                "        ' '               ,", #desdeleg
                "        A.ciudad          ,",
                "        ' '               ,", #desciudad
                "        A.estado          ,",
                "        ' '               ,", #desestad
                "        A.fono            ,",
                "        A.fecha_registro   ", #F.Consar Termino
##         "        A.fecha_registro + 18 UNITS MONTH  ", #F.Consar Termino
                " FROM   pro_mae_promotor A ",
                " WHERE  A.status IN(2,1)   ",
                " AND    A.status_interno in(5) ",
                " AND    A.motivo_suspende in('2A','2B','2C','2D','2D','2G',",
                "'2L','3D') ",
                " AND    ",x_busca CLIPPED

    PREPARE pre_x FROM txt_1
    DECLARE cur_x CURSOR FOR pre_x

    LET pos = 1
    FOREACH cur_x INTO arr_1[pos].*
        IF pos = 2000 THEN
            ERROR "ARREGLO LLENO"
            EXIT FOREACH
        ELSE
            SELECT UNIQUE A.nombre_uni_n1   
            INTO   arr_1[pos].desagenc
            FROM   com_nivel1 A
            WHERE  A.coduni_n1 = arr_1[pos].agenc_cod

            SELECT UNIQUE A.desc_tipo   
            INTO   arr_1[pos].desnivel
            FROM   com_tipo_promotor A
            WHERE  A.cod_tipo_prom = arr_1[pos].nivel  

            SELECT UNIQUE A.deleg_desc
            INTO   arr_1[pos].desdeleg
            FROM   tab_delegacion A
            WHERE  A.deleg_cod = arr_1[pos].deleg  

            SELECT UNIQUE A.ciudad_desc   
            INTO   arr_1[pos].desciudad
            FROM   tab_ciudad A
            WHERE  A.ciudad_cod = arr_1[pos].ciudad  

            SELECT UNIQUE A.estad_desc  
            INTO   arr_1[pos].desestad
            FROM   tab_estado A
            WHERE  A.estad_cod = arr_1[pos].estado  

--(v10) inicia

            CALL curp(arr_1[pos].unico) RETURNING arr_1[pos].sexo,
                                                  arr_1[pos].edo_naci
--(v10) finaliza

            INITIALIZE v_ftermino1, v_ftermino2 TO NULL
            LET v_ftermino1 = arr_1[pos].ftermino

            LET v_ftermino2 = v_ftermino1 + 18 UNITS MONTH

            IF v_ftermino2 IS NULL THEN
               LET v_ftermino2 = MDY(MONTH(v_ftermino1),
                                     DAY(v_ftermino1 - 1 UNITS DAY),
                                     YEAR(v_ftermino1))
            END IF

            LET arr_1[pos].ftermino = v_ftermino2 

            LET pos = pos + 1
        END IF
    END FOREACH

    IF pos = 1 THEN
        ERROR" NO HAY REGISTROS"
        RETURN
    END IF

    LET total_reg = pos -1
    DISPLAY total_reg TO FORMONLY.total_reg

    CALL SET_COUNT(pos-1)

    INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.*
        BEFORE FIELD codven
            LET arr_c = ARR_CURR()
            IF arr_c > pos-1 THEN
                DISPLAY "","" AT 1,1
                ERROR "NO HAY MAS REGISTROS HACIA ABAJO"
            ELSE
                CALL despliega_estado_del_promotor(arr_1[arr_c].cod_promotor)
            END IF

        AFTER FIELD codven

        ON KEY ( CONTROL-M )
           LET sw_1 = 0
           LET reg_mod.fecha_ult_mod = CURRENT    
           SELECT A.nip,
                  A.fingre,
                  A.sup,
                  A.diag_proceso,
                  A.nro_solicitud,
                  A.fecha_certifi,
                  A.motivo_suspende,
                  A.fech_credencial,
                  A.status,
                  A.status_interno
           INTO reg_mod.nip,
                reg_mod.fingre,
                reg_mod.sup,
                reg_mod.diag_proceso,
                reg_mod.nro_solicitud,
                reg_mod.fecha_certifi,
                reg_mod.motivo_suspende,
                reg_mod.fech_credencial,
                reg_mod.status,
                reg_mod.status_interno
           FROM pro_mae_promotor  A                                            
           WHERE A.status IN(1,4)
           AND   A.status_interno in(5,4,0)
           AND   A.cod_promotor  = arr_1[arr_c].cod_promotor
           AND   A.codven        = arr_1[arr_c].codven
           AND   A.paterno       = arr_1[arr_c].paterno
           AND   A.materno       = arr_1[arr_c].materno
           AND   A.nombres       = arr_1[arr_c].nombres

           LET reg_mod.codven        = arr_1[arr_c].codven
           LET reg_mod.seguro        = arr_1[arr_c].seguro
           LET reg_mod.agenc_cod     = arr_1[arr_c].agenc_cod
           LET reg_mod.unico         = arr_1[arr_c].unico     
##           LET reg_mod.tipo_recibo   = arr_1[arr_c].tipo_recibo
           LET reg_mod.escolar       = arr_1[arr_c].escolar    
           LET reg_mod.rfc           = arr_1[arr_c].rfc       
           LET reg_mod.calle         = arr_1[arr_c].calle     
           LET reg_mod.numero        = arr_1[arr_c].numero    
           LET reg_mod.dpto          = arr_1[arr_c].dpto      
           LET reg_mod.colonia       = arr_1[arr_c].colonia   
           LET reg_mod.deleg         = arr_1[arr_c].deleg     
           LET reg_mod.ciudad        = arr_1[arr_c].ciudad    
           LET reg_mod.estado        = arr_1[arr_c].estado    
           LET reg_mod.codpos        = arr_1[arr_c].codpos    
           LET reg_mod.fono          = arr_1[arr_c].fono      
           LET reg_mod.nivel         = arr_1[arr_c].nivel     
           LET reg_mod.resuelva      = arr_1[arr_c].resuelva  
           LET reg_mod.son           = arr_1[arr_c].son 
           LET reg_mod.cod_promotor  = arr_1[arr_c].cod_promotor
           LET reg_mod.paterno       = arr_1[arr_c].paterno
           LET reg_mod.materno       = arr_1[arr_c].materno
           LET reg_mod.nombres       = arr_1[arr_c].nombres
           LET reg_mod.fenvio        = arr_1[arr_c].fenvio
           LET reg_mod.fecha_registro= arr_1[arr_c].fecha_registro
           LET reg_mod.fecha_baja    = arr_1[arr_c].fecha_baja
           LET reg_mod.fnaci         = arr_1[arr_c].fnaci
           LET reg_mod.fecha_suspende= arr_1[arr_c].fecha_suspende
              
           CALL pidedato(arr_1[arr_c].*, v_mod_sin_304) #p
           EXIT INPUT

        ON KEY ( CONTROL-B )
           CALL despliega_cuenta_promotor(arr_1[arr_c].codven)
    END INPUT
END FUNCTION


FUNCTION modifica(v_mod_sin_304)
#m------------------------------

   DEFINE #loc #char
      v_mod_sin_304   INTEGER   ,
      x_busca         CHAR(500) ,
      txt_1           CHAR(1500)

   DEFINE #loc #smallint
      arr_c           ,
      arc             ,
      pos             INTEGER

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " MODIFICA " at 1,65 ATTRIBUTE(REVERSE)
   DISPLAY " Ctrl-C = Salir  Enter = Seleccion  Ctrl-B =  Cuenta Bancaria " AT 2,1

   LET sw_1     = 0
   LET INT_FLAG = FALSE

   INITIALIZE x_busca, txt_1 TO NULL

   CONSTRUCT BY NAME x_busca ON A.codven,
                                A.cod_promotor,
                                A.paterno,
                                A.materno,
                                A.nombres
      ON KEY (ESC)
         LET INT_FLAG = FALSE
         EXIT CONSTRUCT

      ON KEY (INTERRUPT,CONTROL-C)
         LET sw_1 = 1
         EXIT CONSTRUCT
   END CONSTRUCT

   IF sw_1 = 1 THEN
      RETURN
   END IF

   IF v_mod_sin_304 = 0 THEN   #-- MODIFICA PROMOTOR ACTIVO O INACTIVO --#

      LET txt_1 = " SELECT A.codven          ,",
                  "        A.cod_promotor    ,",
                  "        A.agenc_cod       ,",
                  "        ' '               ,", #desagenc
                  "        A.nivel           ,",
                  "        ' '               ,", #desnivel
                  "        0                 ,", #num. ocurrencia
                  "        A.seguro          ,",
                  "        A.rfc             ,",
                  "        ' '               ,",
                  "        A.unico           ,",
                  "        A.escolar         ,",
                  "        A.paterno         ,",
                  "        A.materno         ,",
                  "        A.nombres         ,",
                  "        A.fnaci           ,",
                  "        A.fenvio          ,",
                  "        A.fecha_registro  ,",
                  "        A.fecha_baja      ,",
                  "        A.fecha_suspende  ,",
                  "        A.resuelva        ,",
                  "        ' '               ,", --(v10)
                  "        ' '               ,", --(v10)
                  "        A.calle           ,",
                  "        A.numero          ,",
                  "        A.dpto            ,",
                  "        A.codpos          ,",
                  "        A.colonia         ,",
                  "        A.deleg           ,",
                  "        ' '               ,", #desdeleg
                  "        A.ciudad          ,",
                  "        ' '               ,", #desciudad
                  "        A.estado          ,",
                  "        ' '               ,", #desestad
                  "        A.fono            ,",
                  "        A.fecha_registro   ", #F.Consar Termino
                  " FROM   pro_mae_promotor A ",
                  " WHERE  ",x_busca CLIPPED
   ELSE
      LET txt_1 = " SELECT A.codven          ,",   #-- MODIFICA ESTRUCTURA COMERCIAL --#
                  "        A.cod_promotor    ,",
                  "        A.agenc_cod       ,",
                  "        ' '               ,", #desagenc
                  "        A.nivel           ,",
                  "        ' '               ,", #desnivel
                  "        0                 ,", #num. ocurrencia
                  "        A.seguro          ,",
                  "        A.rfc             ,",
                  "        ' '               ,",
                  "        A.unico           ,",
   ##               "        A.tipo_recibo     ,",
                  "        A.escolar         ,",
                  "        A.paterno         ,",
                  "        A.materno         ,",
                  "        A.nombres         ,",
                  "        A.fnaci           ,",
                  "        A.fenvio          ,",
                  "        A.fecha_registro  ,",
                  "        A.fecha_baja      ,",
                  "        A.fecha_suspende  ,",
                  "        A.resuelva        ,",
                  "        ' '               ,", --(v10)
                  "        ' '               ,", --(v10)
                  "        A.calle           ,",
                  "        A.numero          ,",
                  "        A.dpto            ,",
                  "        A.codpos          ,",
                  "        A.colonia         ,",
                  "        A.deleg           ,",
                  "        ' '               ,", #desdeleg
                  "        A.ciudad          ,",
                  "        ' '               ,", #desciudad
                  "        A.estado          ,",
                  "        ' '               ,", #desestad
                  "        A.fono            ,",
                  "        A.fecha_registro   ", #F.Consar Termino
   ##           "        A.fecha_registro + 18 UNITS MONTH  ", #F.Consar Termino
                  " FROM   pro_mae_promotor A ",
                  " WHERE  A.status IN(1,4)   ",
                  " AND    A.status_interno in(5,4,0) ",
                  " AND    ",x_busca CLIPPED
   END IF

   PREPARE pre_1 FROM txt_1
   DECLARE cur_2 CURSOR FOR pre_1

   LET pos = 1

   FOREACH cur_2 INTO arr_1[pos].*
      IF pos = 2000 THEN
         ERROR "ARREGLO LLENO"
         EXIT FOREACH
      ELSE
         SELECT UNIQUE A.nombre_uni_n1
         INTO   arr_1[pos].desagenc
         FROM   com_nivel1 A
         WHERE  A.coduni_n1 = arr_1[pos].agenc_cod

         SELECT UNIQUE A.desc_tipo
         INTO   arr_1[pos].desnivel
         FROM   com_tipo_promotor A
         WHERE  A.cod_tipo_prom = arr_1[pos].nivel

         SELECT UNIQUE A.deleg_desc
         INTO   arr_1[pos].desdeleg
         FROM   tab_delegacion A
         WHERE  A.deleg_cod = arr_1[pos].deleg

         SELECT UNIQUE A.ciudad_desc
         INTO   arr_1[pos].desciudad
         FROM   tab_ciudad A
         WHERE  A.ciudad_cod = arr_1[pos].ciudad

         SELECT UNIQUE A.estad_desc
         INTO   arr_1[pos].desestad
         FROM   tab_estado A
         WHERE  A.estad_cod = arr_1[pos].estado

--(v10) inicia

         CALL curp(arr_1[pos].unico) RETURNING arr_1[pos].sexo,
                                               arr_1[pos].edo_naci
--(v10) finaliza

         INITIALIZE v_ftermino1, v_ftermino2 TO NULL

         LET v_ftermino1 = arr_1[pos].ftermino

         LET v_ftermino2 = v_ftermino1 + 18 UNITS MONTH

         IF v_ftermino2 IS NULL THEN

            LET v_ftermino2 = MDY(MONTH(v_ftermino1),
                              DAY(v_ftermino1 - 1 UNITS DAY),
                              YEAR(v_ftermino1))
         END IF

         LET arr_1[pos].ftermino = v_ftermino2 
         LET arr_1[pos].son = pos
         LET pos = pos + 1

      END IF
   END FOREACH

   IF pos = 1 THEN
      ERROR" NO HAY REGISTROS"
      RETURN
   END IF

   LET total_reg = pos -1

   DISPLAY total_reg TO FORMONLY.total_reg

   CALL SET_COUNT(pos-1)

   INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.*
      BEFORE FIELD codven
         LET arr_c = ARR_CURR()

         IF arr_c > pos-1 THEN
            DISPLAY "","" AT 1,1
            ERROR "NO HAY MAS REGISTROS HACIA ABAJO"
         ELSE
            CALL despliega_estado_del_promotor(arr_1[arr_c].cod_promotor)
         END IF

      AFTER FIELD codven

      ON KEY ( CONTROL-M )
         LET sw_1 = 0

         LET reg_mod.fecha_ult_mod = CURRENT

         SELECT A.nip,
                A.fingre,
                A.sup,
                A.diag_proceso,
                A.nro_solicitud,
                A.fecha_certifi,
                A.motivo_suspende,
                A.fech_credencial,
                A.status,
                A.status_interno
         INTO   reg_mod.nip,
                reg_mod.fingre,
                reg_mod.sup,
                reg_mod.diag_proceso,
                reg_mod.nro_solicitud,
                reg_mod.fecha_certifi,
                reg_mod.motivo_suspende,
                reg_mod.fech_credencial,
                reg_mod.status,
                reg_mod.status_interno
         FROM   pro_mae_promotor  A
         WHERE  A.status IN(1,4)
         AND    A.status_interno in(5,4,0)
         AND    A.cod_promotor  = arr_1[arr_c].cod_promotor
         AND    A.codven        = arr_1[arr_c].codven
         AND    A.paterno       = arr_1[arr_c].paterno
         AND    A.materno       = arr_1[arr_c].materno
         AND    A.nombres       = arr_1[arr_c].nombres

         LET reg_mod.codven        = arr_1[arr_c].codven
         LET reg_mod.seguro        = arr_1[arr_c].seguro
         LET reg_mod.agenc_cod     = arr_1[arr_c].agenc_cod
         LET reg_mod.unico         = arr_1[arr_c].unico
##          LET reg_mod.tipo_recibo   = arr_1[arr_c].tipo_recibo
         LET reg_mod.escolar       = arr_1[arr_c].escolar
         LET reg_mod.rfc           = arr_1[arr_c].rfc
         LET reg_mod.calle         = arr_1[arr_c].calle
         LET reg_mod.numero        = arr_1[arr_c].numero
         LET reg_mod.dpto          = arr_1[arr_c].dpto
         LET reg_mod.colonia       = arr_1[arr_c].colonia
         LET reg_mod.deleg         = arr_1[arr_c].deleg
         LET reg_mod.ciudad        = arr_1[arr_c].ciudad
         LET reg_mod.estado        = arr_1[arr_c].estado
         LET reg_mod.codpos        = arr_1[arr_c].codpos
         LET reg_mod.fono          = arr_1[arr_c].fono
         LET reg_mod.nivel         = arr_1[arr_c].nivel
         LET reg_mod.resuelva      = arr_1[arr_c].resuelva
         LET reg_mod.son           = arr_1[arr_c].son
         LET reg_mod.cod_promotor  = arr_1[arr_c].cod_promotor
         LET reg_mod.paterno       = arr_1[arr_c].paterno
         LET reg_mod.materno       = arr_1[arr_c].materno
         LET reg_mod.nombres       = arr_1[arr_c].nombres
         LET reg_mod.fenvio        = arr_1[arr_c].fenvio
         LET reg_mod.fecha_registro= arr_1[arr_c].fecha_registro
         LET reg_mod.fecha_baja    = arr_1[arr_c].fecha_baja
         LET reg_mod.fnaci         = arr_1[arr_c].fnaci
         LET reg_mod.fecha_suspende= arr_1[arr_c].fecha_suspende

         CALL pidedato(arr_1[arr_c].*, v_mod_sin_304) #p

         EXIT INPUT

      ON KEY ( CONTROL-B )
         CALL despliega_cuenta_promotor(arr_1[arr_c].codven)
   END INPUT

END FUNCTION

FUNCTION suspende()
#s-----------------
    DEFINE 
        c2_mot_suspende              CHAR(002) ,
        x_busca               CHAR(500) ,
        txt_1                 CHAR(1500)

    DEFINE 
        s_status              ,
        s_status_interno      ,
        i                     ,
        s_mot_suspende        ,
        arr_c                 ,
        dias                  SMALLINT,
        pos                   SMALLINT
  
     DEFINE vfecha_baja       DATE 

    INITIALIZE x_busca, txt_1 TO NULL

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " SUSPENDE " AT 1,65 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C = Salir  Ctrl-B = Para dar de baja   Esc = Graba" AT 2,1

    LET sw_1     = 0
    LET INT_FLAG = FALSE

    CONSTRUCT BY NAME x_busca ON codven,
                                  cod_promotor,
                                  paterno,
                                  materno,
                                  nombres,
                                  fecha_baja
        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT,CONTROL-C)
            LET sw_1 = 1
            EXIT CONSTRUCT
    END CONSTRUCT

    IF sw_1 = 1 THEN
        RETURN
    END IF

    LET txt_1 = " SELECT A.codven          ,",
                "        A.cod_promotor    ,",
                "        A.agenc_cod       ,",
                "        ' '               ,", #desagenc
                "        A.nivel           ,",
                "        ' '               ,", #desnivel
                "        0                 ,", #num.ocurrencias
                "        A.seguro          ,",
                "        A.rfc[1,10]       ,",
                "        A.rfc[11,13]      ,",
                "        A.unico           ,",
##                "        A.tipo_recibo     ,",
                "        A.escolar         ,",
                "        A.paterno         ,",
                "        A.materno         ,",
                "        A.nombres         ,",
                "        A.fnaci           ,",
                "        A.fenvio          ,",
                "        A.fecha_registro  ,",
                "        A.fecha_baja      ,",
                "        A.fecha_suspende  ,",
                "        A.resuelva        ,",

                "        ' '               ,", --(v10)
                "        ' '               ,", --(v10)

                "        A.calle           ,",
                "        A.numero          ,",
                "        A.dpto            ,",
                "        A.codpos          ,",
                "        A.colonia         ,",
                "        A.deleg           ,",
                "        ' '               ,", #desdeleg
                "        A.ciudad          ,",
                "        ' '               ,", #desciudad
                "        A.estado          ,",
                "        ' '               ,", #desestad
                "        A.fono             ",
                " FROM   pro_mae_promotor A ",
                " WHERE  (A.status = 1       ",
                " OR    (A.status in(2,3) and A.status_interno = 0))",
                " AND    ",x_busca CLIPPED

    PREPARE pre_4 FROM txt_1
    DECLARE cur_4 CURSOR FOR pre_4

    LET pos = 1
    FOREACH cur_4 INTO arr_1[pos].*
        IF pos = 2000 THEN
            ERROR "ARREGLO LLENO"
            EXIT FOREACH
        ELSE
            SELECT UNIQUE A.nombre_uni_n1   
            INTO   arr_1[pos].desagenc
            FROM   com_nivel1 A
            WHERE  A.coduni_n1 = arr_1[pos].agenc_cod

            SELECT UNIQUE A.desc_tipo   
            INTO   arr_1[pos].desnivel
            FROM   com_tipo_promotor A
            WHERE  A.cod_tipo_prom = arr_1[pos].nivel  

            SELECT UNIQUE A.deleg_desc
            INTO   arr_1[pos].desdeleg
            FROM   tab_delegacion A
            WHERE  A.deleg_cod = arr_1[pos].deleg  

            SELECT UNIQUE A.ciudad_desc   
            INTO   arr_1[pos].desciudad
            FROM   tab_ciudad A
            WHERE  A.ciudad_cod = arr_1[pos].ciudad  

            SELECT UNIQUE A.estad_desc  
            INTO   arr_1[pos].desestad
            FROM   tab_estado A
            WHERE  A.estad_cod = arr_1[pos].estado  

            LET arr_1[pos].fecha_baja = ""
            LET arr_1[pos].son = pos
--(v10) inicia

            CALL curp(arr_1[pos].unico) RETURNING arr_1[pos].sexo,
                                                  arr_1[pos].edo_naci
--(v10) finaliza

            LET pos = pos + 1
        END IF
    END FOREACH

    IF pos = 1 THEN
        ERROR" NO HAY REGISTROS"
        RETURN
    END IF

    DISPLAY total_reg  TO FORMONLY.total_reg
    CALL SET_COUNT(pos-1)

    INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.*
        BEFORE FIELD codven
            LET arr_c = ARR_CURR()
            IF arr_c > pos-1 THEN
                DISPLAY "","" AT 1,1
                ERROR "NO HAY MAS REGISTROS HACIA ABAJO"
            ELSE #dedp
                CALL despliega_estado_del_promotor(arr_1[arr_c].cod_promotor)
            END IF

        AFTER FIELD codven

        AFTER FIELD fecha_baja
            IF arr_1[arr_c].fecha_baja IS NOT NULL THEN
               LET dias = 0

               LET vfecha_baja = arr_1[arr_c].fecha_baja
               IF vfecha_baja > HOY THEN
                  ERROR " FECHA DE SOLICITUD NO PUEDE SER MAYOR AL DIA DE HOY "
                  NEXT FIELD fecha_baja
               END IF

               LET vfecha_baja = arr_1[arr_c].fecha_baja  + 29 UNITS DAY

               LET dias = HOY - vfecha_baja
{svera 
               IF dias > 30 THEN
                  ERROR " FECHA DE SOLICITUD YA SUPERO LOS 30 DIAS DE VIGENCIA "
                  NEXT FIELD fecha_baja
               END IF
}
            ELSE      
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD fecha_baja
            END IF


        ON KEY ( CONTROL-B )
           CALL rutina_para_suspender() #rps
                RETURNING c2_mot_suspende

                IF c2_mot_suspende = 00 THEN
                   ERROR " DEBE DE SELECCIONAR UN MOTIVO DE ",
                         "SUSPENCION (Ctrl-B)" ATTRIBUTE(NORMAL) 
                   NEXT FIELD fecha_baja
                ELSE 
                   NEXT FIELD fecha_baja
                END IF 

        ON KEY (ESC)
            IF arr_1[arr_c].fecha_baja IS NOT NULL THEN
               LET dias = 0

               LET vfecha_baja = arr_1[arr_c].fecha_baja
               IF vfecha_baja > HOY THEN
                  ERROR " FECHA DE SOLICITUD NO PUEDE SER MAYOR AL DIA DE HOY "
                  NEXT FIELD fecha_baja
               END IF

               LET vfecha_baja = arr_1[arr_c].fecha_baja  + 29 UNITS DAY

               LET dias = HOY - vfecha_baja
{svera
               IF dias > 30 THEN
                  ERROR " FECHA DE SOLICITUD YA SUPERO LOS 30 DIAS DE VIGENCIA "
                  NEXT FIELD fecha_baja
               END IF
}
            ELSE      
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD fecha_baja
            END IF

            IF c2_mot_suspende = 00 OR c2_mot_suspende IS NULL THEN
               ERROR " DEBE DE SELECCIONAR UN MOTIVO DE SUSPENCION (Ctrl-B)" 
               ATTRIBUTE(NORMAL)
               NEXT FIELD fecha_baja
            END IF                                        

--            LET s_mot_suspende = c2_mot_suspende[1,1]           --(v10)
            LET s_mot_suspende = 3            --BAJA              --(v10)

            SELECT A.status        ,
                   A.status_interno
            INTO   s_status        ,
                   s_status_interno
            FROM   pro_mae_promotor A
            WHERE  A.cod_promotor = arr_1[arr_c].cod_promotor

            INSERT INTO pro_his_alta
                VALUES(arr_1[arr_c].codven          ,
                       arr_1[arr_c].cod_promotor    ,
                       arr_1[arr_c].fecha_baja      ,
                       arr_1[arr_c].fecha_registro  ,
                       s_status                     ,
                       s_status_interno             ,
                       c2_mot_suspende              ,#motivo_suspende
                       HOY                           #fecha_insercion
                      ) 

            UPDATE pro_mae_promotor 
            SET    pro_mae_promotor.motivo_suspende = c2_mot_suspende         ,
                   pro_mae_promotor.status          = s_mot_suspende          ,
                   pro_mae_promotor.status_interno  = 0                       ,
                   pro_mae_promotor.fecha_baja      = arr_1[arr_c].fecha_baja ,
                   pro_mae_promotor.fecha_suspende  = NULL
            WHERE  pro_mae_promotor.cod_promotor = arr_1[arr_c].cod_promotor

            ERROR "REGISTRO SUSPENDIDO "
            CLEAR FORM
            EXIT INPUT

        ON KEY(INTERRUPT, CONTROL-C)
            DISPLAY "             " AT 5,63
            EXIT INPUT
    END INPUT
END FUNCTION

FUNCTION activar()
#a----------------
    DEFINE 
        c2_mot_suspende       CHAR(002) ,
        x_busca               CHAR(500) ,
        txt_1                 CHAR(1500)

    DEFINE 
        i                     ,
        s_mot_suspende        ,
        arr_c                 ,
        pos                   SMALLINT

    INITIALIZE x_busca, txt_1 TO NULL

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ACTIVA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C = Salir  Ctrl-V = Para activar " AT 2,1

    LET sw_1     = 0
    LET INT_FLAG = FALSE

    CONSTRUCT BY NAME x_busca 
              ON codven,
                 cod_promotor,
                 paterno,
                 materno,
                 nombres,
                 fecha_baja

        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY(INTERRUPT, CONTROL-C)
            LET sw_1 = 1
            EXIT CONSTRUCT
    END CONSTRUCT

    IF sw_1 = 1 THEN
        RETURN
    END IF

    LET txt_1 = " SELECT A.codven          ,",
                "        A.cod_promotor    ,",
                "        A.agenc_cod       ,",
                "        ' '               ,", #desagenc
                "        A.nivel           ,",
                "        ' '               ,", #desnivel
                "        0                 ,", #num. ocurrencias
                "        A.seguro          ,",
                "        A.rfc[1,10]       ,",
                "        A.rfc[11,13]      ,",
                "        A.unico           ,",
##                "        A.tipo_recibo     ,",
                "        A.escolar         ,",
                "        A.paterno         ,",
                "        A.materno         ,",
                "        A.nombres         ,",
                "        A.fnaci           ,",
                "        A.fenvio          ,",
                "        A.fecha_registro  ,",
                "        A.fecha_baja      ,",
                "        A.fecha_suspende  ,",
                "        A.resuelva        ,",

                "        ' '               ,", --(v10)
                "        ' '               ,", --(v10)

                "        A.calle           ,",
                "        A.numero          ,",
                "        A.dpto            ,",
                "        A.codpos          ,",
                "        A.colonia         ,",
                "        A.deleg           ,",
                "        ' '               ,", #desdeleg
                "        A.ciudad          ,",
                "        ' '               ,", #desciudad
                "        A.estado          ,",
                "        ' '               ,", #desestad
                "        A.fono             ",
                " FROM   pro_mae_promotor A ",
                " WHERE  status IN(2,3)     ",
                " AND    status_interno = 0 ",
                " AND    ",x_busca CLIPPED

    PREPARE pre_5 FROM txt_1
    DECLARE cur_5 CURSOR FOR pre_5

    LET pos = 1
    FOREACH cur_5 INTO arr_1[pos].*
        IF pos = 2000 THEN
            ERROR "ARREGLO LLENO"
            EXIT FOREACH
        ELSE
            SELECT A.deleg_desc
            INTO   arr_1[pos].desdeleg
            FROM   tab_delegacion A
            WHERE  A.deleg_cod = arr_1[pos].deleg  

            SELECT A.ciudad_desc   
            INTO   arr_1[pos].desciudad
            FROM   tab_ciudad A
            WHERE  A.ciudad_cod = arr_1[pos].ciudad  

            SELECT A.estad_desc  
            INTO   arr_1[pos].desestad
            FROM   tab_estado A
            WHERE  A.estad_cod = arr_1[pos].estado  
--(v10) inicia

            CALL curp(arr_1[pos].unico) RETURNING arr_1[pos].sexo,
                                                  arr_1[pos].edo_naci
--(v10) finaliza

            LET  arr_1[pos].son = pos
            LET pos = pos + 1
        END IF
    END FOREACH

    IF pos = 1 THEN
        ERROR" NO HAY REGISTROS"
        RETURN
    END IF

    DISPLAY total_reg  TO FORMONLY.son      
    CALL SET_COUNT(pos-1)

    INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.*
        BEFORE FIELD codven
            LET arr_c = ARR_CURR()
            IF arr_c > pos-1 THEN
                DISPLAY "","" AT 1,1
                ERROR "NO HAY MAS REGISTROS HACIA ABAJO"
            ELSE #dedp
                CALL despliega_estado_del_promotor(arr_1[arr_c].cod_promotor) 
            END IF

        AFTER FIELD codven

        ON KEY ( CONTROL-V )
            WHILE TRUE
                PROMPT "DESEA ACTIVAR PROMOTOR S/N " FOR CHAR enter
                IF enter MATCHES "[SsNn]" THEN
                    IF enter MATCHES "[Ss]" THEN
                        UPDATE pro_mae_promotor 
                        SET    pro_mae_promotor.diag_proceso    = "1A" ,
                               pro_mae_promotor.fecha_baja      = NULL ,
                               pro_mae_promotor.motivo_suspende = NULL ,
                               pro_mae_promotor.fecha_suspende  = NULL ,
                               pro_mae_promotor.status          = 1    ,
                               pro_mae_promotor.status_interno  = 5
                        WHERE  pro_mae_promotor.cod_promotor    = 
                                                      arr_1[arr_c].cod_promotor
    
                        ERROR "PROMOTOR ACTIVADO "
                    ELSE
                        ERROR "ACTIVACION CANCELADA"
                    END IF

                    LET sw_1 = 1
                    CLEAR FORM
                    EXIT WHILE
                END IF
            END WHILE

            IF sw_1 = 1 THEN
                EXIT INPUT
            END IF

        ON KEY (INTERRUPT,CONTROL-C)
            EXIT INPUT
    END INPUT
END FUNCTION

FUNCTION consulta()
#c-----------------
    DEFINE 
        x_busca               CHAR(500) ,
        txt_1                 CHAR(1500)

    DEFINE 
        arr_c                 ,
        arc                   ,
        pos                   SMALLINT

    INITIALIZE x_busca, txt_1 TO NULL
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " at 1,65 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C = Salir " AT 1,1
    DISPLAY " Ctrl-B = Cuenta Bancaria ",
            " Ctrl-V = Horas capacitacion Ctrl-E = Historico" AT 2,1    

    LET INT_FLAG = FALSE
    LET sw_1     = 0
    LET total_reg= 0

    CONSTRUCT BY NAME x_busca ON codven,
                                 cod_promotor,
                                 paterno,
                                 materno,
                                 nombres,
                                 fecha_baja,
                                 fecha_registro
        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY(INTERRUPT, CONTROL-C)
            LET sw_1 = 1
            EXIT CONSTRUCT
    END CONSTRUCT

    IF sw_1 = 1 THEN
        RETURN
    END IF

    LET txt_1 = " SELECT A.codven          ,",
                "        A.cod_promotor    ,",
                "        A.agenc_cod       ,",
                "        ' '               ,", #desagenc
                "        A.nivel           ,",
                "        ' '               ,", #desnivel
                "        0                 ,", #num. ocurrencia
                "        A.seguro          ,",
                "        A.rfc[1,10]       ,",
                "        A.rfc[11,13]      ,",
                "        A.unico           ,",
##                "        A.tipo_recibo     ,",
                "        A.escolar         ,",
                "        A.paterno         ,",
                "        A.materno         ,",
                "        A.nombres         ,",
                "        A.fnaci           ,",
                "        A.fenvio          ,",
                "        A.fecha_registro  ,",
                "        A.fecha_baja      ,",
                "        A.fecha_suspende  ,",
                "        A.resuelva        ,",

                "        ' '               ,", --(v10)
                "        ' '               ,", --(v10)

                "        A.calle           ,",
                "        A.numero          ,",
                "        A.dpto            ,",
                "        A.codpos          ,",
                "        A.colonia         ,",
                "        A.deleg           ,",
                "        ' '               ,", #desdeleg
                "        A.ciudad          ,",
                "        ' '               ,", #desciudad
                "        A.estado          ,",
                "        ' '               ,", #desestad
                "        A.fono            ,",
                "        A.fecha_registro  ",  #LC 251103
##                "        A.fecha_registro + 18 UNITS MONTH ",  #LC 251103
                " FROM   pro_mae_promotor A ",
                " WHERE  ",x_busca CLIPPED

    PREPARE pre_3 FROM txt_1
    DECLARE cur_3 CURSOR FOR pre_3

    LET pos = 1
    FOREACH cur_3 INTO arr_1[pos].*
        IF  pos = 2000 THEN
            ERROR "ARREGLO LLENO"
            EXIT FOREACH
        ELSE
            LET arr_1[pos].desagenc = "       "
            LET arr_1[pos].desnivel = "       "
{
            SELECT A.nombre_uni_n1   
            INTO   arr_1[pos].desagenc
            FROM   com_nivel1 A
            WHERE  A.coduni_n1 = arr_1[pos].agenc_cod

            SELECT A.desc_tipo   
            INTO   arr_1[pos].desnivel
            FROM   com_tipo_promotor A
            WHERE  A.cod_tipo_prom = arr_1[pos].nivel  
}
            
            SELECT A.deleg_desc
            INTO   arr_1[pos].desdeleg
            FROM   tab_delegacion A
            WHERE  A.deleg_cod = arr_1[pos].deleg  

            SELECT A.ciudad_desc   
            INTO   arr_1[pos].desciudad
            FROM   tab_ciudad A
            WHERE  A.ciudad_cod = arr_1[pos].ciudad  

            SELECT A.estad_desc  
            INTO   arr_1[pos].desestad
            FROM   tab_estado A
            WHERE  A.estad_cod = arr_1[pos].estado  

            SELECT A.status_interno
            INTO   vstatus_interno 
            FROM   pro_mae_promotor A
            WHERE  A.cod_promotor = arr_1[pos].cod_promotor

--(v10) inicia

            CALL curp(arr_1[pos].unico) RETURNING arr_1[pos].sexo,
                                                  arr_1[pos].edo_naci
--(v10) finaliza


            INITIALIZE v_ftermino1, v_ftermino2 TO NULL
            LET v_ftermino1 = arr_1[pos].ftermino

            LET v_ftermino2 = v_ftermino1 + 18 UNITS MONTH

            IF v_ftermino2 IS NULL THEN
               LET v_ftermino2 = MDY(MONTH(v_ftermino1),
                                     DAY(v_ftermino1 - 1 UNITS DAY),
                                     YEAR(v_ftermino1))
            END IF

            LET arr_1[pos].ftermino = v_ftermino2 
            LET arr_1[pos].son = pos
            LET pos = pos + 1
        END IF
    END FOREACH

    IF pos = 1 THEN
        ERROR" NO HAY REGISTROS"
        RETURN
    END IF

    LET total_reg = pos - 1
    DISPLAY total_reg TO FORMONLY.total_reg

    CALL SET_COUNT(pos-1)

    INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.*
        BEFORE FIELD codven
            LET arr_c = ARR_CURR()
             IF arr_c > pos-1 THEN
                LET  arr_c = pos -1
                DISPLAY "","" AT 1,1
                ERROR "NO HAY MAS REGISTROS HACIA ABAJO"
             ELSE    #dedp
                CALL despliega_estado_del_promotor(arr_1[arr_c].cod_promotor)
--                DISPLAY "" ,vdesc_status_corta  AT 1,57
            END IF

        AFTER FIELD codven
            IF  FGL_LASTKEY() = FGL_KEYVAL("LEFT")  
            AND FGL_LASTKEY() = FGL_KEYVAL("UP") THEN #dedp
               CALL despliega_estado_del_promotor(arr_1[pos-1].cod_promotor)
--               DISPLAY "" ,vdesc_status_corta  AT 1,57
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN #dedp
               CALL despliega_estado_del_promotor(arr_1[pos-1].cod_promotor)
--               DISPLAY "" ,vdesc_status_corta  AT 1,57
            END IF 

        ON KEY ( CONTROL-B )
           CALL despliega_cuenta_promotor(arr_1[arr_c].codven)
            
        ON KEY ( CONTROL-V )
           CALL despliega_capacitacion(arr_1[arr_c].codven) #dc

        ON KEY ( CONTROL-E )
           CALL despliega_historico(arr_1[arr_c].paterno,
                                    arr_1[arr_c].materno,
                                    arr_1[arr_c].nombres,
                                    arr_1[arr_c].cod_promotor,
                                    arr_1[arr_c].fecha_registro,
                                    arr_1[arr_c].agenc_cod,
                                    arr_1[arr_c].nivel,
                                    arr_1[arr_c].fenvio,
                                    arr_1[arr_c].codven) #dh
    END INPUT
END FUNCTION

FUNCTION curp(x_curp)                     --(v10)
#cr----------------
    DEFINE
           x_curp                CHAR(18)


        LET v_sexo     = " "
        LET v_edo_naci = "  "

        -- Obtiene la clave de sexo
        LET v_sexo     = x_curp[11,11]


        -- Obtiene la clave de sexo
        LET v_sexo     = x_curp[11,11]
        CASE v_sexo
             WHEN "M"
                LET v_sexo = 2 
             WHEN "H"
                LET v_sexo = 1 
        END CASE


        -- Obtiene la clave de la entidad de nacimiento
        LET v_edo_naci = x_curp[12,13]


        SELECT cve_ent
        INTO   v_edo_naci
        FROM   pro_ent_naci
        WHERE  cod_ent = v_edo_naci


   RETURN v_sexo,
          v_edo_naci

END FUNCTION

FUNCTION rutina_para_suspender()
#rps----------------------------
    DEFINE reg ARRAY [2000] OF RECORD
        codigo                 CHAR(02),
        descripcion            CHAR(80),
        marcado                CHAR(01)                  
    END RECORD

    DEFINE 
        arr                    ,
        src                    ,
        sale                   ,
        i                      ,
        pos                    ,
        OK                       SMALLINT

    OPEN WINDOW prom0253 AT 06,22 WITH FORM "PROM0253" ATTRIBUTE(BORDER)
    DISPLAY "            DIAGNOSTICO DEL PROMO",
            "TOR                                           " 
            AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [ Esc ] Procesar     [Ctrl-C] Cancelar Suspension " AT 1,1 

        DECLARE cur_1 CURSOR FOR 
            SELECT * 
            FROM   tab_diagnos_pro 
            WHERE  diagn_cod IN ("2E","2G","2L","2P")      --(v10)
            ORDER BY 1

            LET pos = 1
        FOREACH cur_1 INTO reg[pos].codigo     ,
                           reg[pos].descripcion
            LET pos = pos + 1
        END FOREACH

        WHILE TRUE
            CALL SET_COUNT(pos-1)
            INPUT array reg WITHOUT DEFAULTS FROM scr_1.*  
                BEFORE FIELD codigo
                    LET arr = ARR_CURR()
                    LET src = SCR_LINE()
                    NEXT FIELD marcado
    
                BEFORE FIELD marcado
                    LET arr = ARR_CURR()
                    LET src = SCR_LINE()

                AFTER FIELD marcado
                    IF reg[arr].marcado IS NOT NULL 
                    AND reg[arr].marcado <> "X"  THEN
                        ERROR "SOLO 'X' ( EQUIS )"
                        NEXT FIELD marcado
                    END IF

                    IF reg[arr].marcado = "X" THEN
                        LET sale = TRUE
                        LET arr = ARR_CURR()
                        EXIT INPUT
                    END IF

                    IF arr >= pos-1 THEN
                        ERROR "LLEGO AL FINAL DEL ARREGLO"
                        EXIT INPUT
                    END IF

                ON KEY ( INTERRUPT, CONTROL-C )
                    LET reg[arr].codigo='00'

                    LET sale = TRUE
                    EXIT INPUT
            END INPUT

            IF sale THEN
               EXIT WHILE
            END IF
    END WHILE

    CLOSE WINDOW prom0253
    RETURN reg[arr].codigo
END FUNCTION


FUNCTION despliega_estado_del_promotor(c10_cod_promotor)
#dedp---------------------------------------------
    DEFINE reg_1 RECORD #loc #reg_1
        diag_proceso     LIKE pro_mae_promotor.diag_proceso   ,
        status           LIKE pro_mae_promotor.status         ,
        status_interno   LIKE pro_mae_promotor.status_interno ,
        motivo_suspende  LIKE pro_mae_promotor.motivo_suspende
    END RECORD

    DEFINE reg_2 RECORD #loc #reg_2
        diagn_cod        LIKE tab_diagnos_pro.diagn_cod ,
        diagn_desc       LIKE tab_diagnos_pro.diagn_desc
    END RECORD

    DEFINE
        desc_estado           CHAR(60),
        vdesc_status_corta    CHAR(25),
        c10_cod_promotor      CHAR(10)

    SELECT A.diag_proceso    ,
           A.status          ,
           A.status_interno  ,
           A.motivo_suspende
    INTO   reg_1.*
    FROM   pro_mae_promotor A
    WHERE  A.cod_promotor = c10_cod_promotor

    CASE reg_1.status
        WHEN 1
            SELECT *
            INTO   reg_2.*
            FROM   tab_diagnos_pro
            WHERE  diagn_cod = reg_1.diag_proceso

            SELECT UNIQUE "OK"
            FROM   pro_aviso_examen  A, pro_mae_promotor B
            WHERE  A.cod_promotor = c10_cod_promotor
            AND    A.estado       = 1
            AND    A.cod_promotor = B.cod_promotor
            AND B.diag_proceso NOT IN ("2F","2K","5A","5C","5D","5I","6A","6B","7E","7X","7L")         --(v10)

            IF STATUS <> NOTFOUND THEN
               SELECT diagn_desc,diagn_cod
               INTO   reg_2.diagn_desc,
                      reg_2.diagn_cod
               FROM   tab_diagnos_pro
               WHERE  diagn_cod = "5B" #CITA PARA PRESENTAR EL EXAMEN S
            END IF

        WHEN 2

            IF reg_1.diag_proceso = "7E"
            OR reg_1.diag_proceso = "7L"
            OR reg_1.diag_proceso = "7T"
            OR reg_1.diag_proceso = "7M"
            OR reg_1.diag_proceso = "7X"       --(v10)      
            OR reg_1.diag_proceso = "8E" THEN  --(v1)
                SELECT *
                INTO   reg_2.*
                FROM   tab_diagnos_pro
                WHERE  diagn_cod = reg_1.diag_proceso

            ELSE
                SELECT *
                INTO   reg_2.*
                FROM   tab_diagnos_pro
                WHERE  diagn_cod = reg_1.motivo_suspende
            END IF

        WHEN 3
            IF reg_1.diag_proceso = "7E"
            OR reg_1.diag_proceso = "7L"
            OR reg_1.diag_proceso = "7T"
            OR reg_1.diag_proceso = "7M"
            OR reg_1.diag_proceso = "7X"       --(v10)
            OR reg_1.diag_proceso = "8E" THEN  --(v1)
               SELECT *
               INTO   reg_2.*
               FROM   tab_diagnos_pro
               WHERE  diagn_cod = reg_1.diag_proceso
            ELSE
               SELECT *
               INTO   reg_2.*
               FROM   tab_diagnos_pro
               WHERE  diagn_cod = reg_1.motivo_suspende
            END IF

        WHEN 4
            SELECT *
            INTO   reg_2.*
            FROM   tab_diagnos_pro
            WHERE  diagn_cod = reg_1.diag_proceso

            DISPLAY " ",reg_2.diagn_desc CLIPPED,"(",reg_2.diagn_cod,
                    ")...EN PROCESO DE MODIFICACION " AT 1,1 ATTRIBUTE(REVERSE)
            RETURN

            SELECT UNIQUE "OK"
            FROM   pro_aviso_examen  A, pro_mae_promotor B
            WHERE  A.cod_promotor = c10_cod_promotor
            AND    A.estado       = 1
            AND    A.cod_promotor = B.cod_promotor
            AND B.diag_proceso NOT IN ("2F","2K","2L","5A","5C","5D","5I","6A","6B","7E","7X","7L")      --(v10)

            IF STATUS <> NOTFOUND THEN
               SELECT diagn_desc,diagn_cod
               INTO   reg_2.diagn_desc,
                      reg_2.diagn_cod
               FROM   tab_diagnos_pro
               WHERE  diagn_cod = "5B" #CITA PARA PRESENTAR EL EXAMEN S
            END IF

        WHEN 6
            SELECT *
            INTO   reg_2.*
            FROM   tab_diagnos_pro
            WHERE  diagn_cod = reg_1.motivo_suspende

    END CASE

    IF reg_2.diagn_cod = "7M" THEN
       SELECT desc_edo_prom
       INTO   vdesc_status_corta
       FROM   pro_edo_promotor
       WHERE  diagn_cod = reg_1.motivo_suspende
    ELSE
       SELECT desc_edo_prom
       INTO   vdesc_status_corta
       FROM   pro_edo_promotor
       WHERE  diagn_cod = reg_2.diagn_cod
    END IF

    DISPLAY "                                                                               " AT 1,1 ATTRIBUTE(NORMAL)
    DISPLAY " (",reg_2.diagn_cod,")",reg_2.diagn_desc AT 1,1 ATTRIBUTE(NORMAL)
    DISPLAY "" ,vdesc_status_corta  AT 1,65 ATTRIBUTE(REVERSE)

END FUNCTION


FUNCTION despliega_cuenta_promotor(vcodven)

      DEFINE vcodven  CHAR(11)
      DEFINE vresp    CHAR(1)

      DEFINE gr_cuenta RECORD
             cta_bancaria   LIKE      pro_cta_promotor.cta_bancaria,
             bco_cod        LIKE      pro_cta_promotor.bco_cod,
             tipo_cuenta    LIKE      pro_cta_promotor.tipo_cuenta,
             plaza          LIKE      pro_cta_promotor.plaza
      END RECORD

      OPEN WINDOW v1 AT  7,10 WITH FORM "PROM0254" ATTRIBUTE(BORDER)
      DISPLAY "                 dis_cuenta BANCARIAS PROMOTO",
            "RES                 " 
              AT 1,1 ATTRIBUTE(REVERSE)

           SELECT cta_bancaria, bco_cod, 
                  tipo_cuenta, plaza INTO gr_cuenta.* 
           FROM   pro_cta_promotor
           WHERE  pro_cta_promotor.codven = vcodven

           IF STATUS = NOTFOUND THEN 
              ERROR "PROMOTOR NO TIENE CUENTA BANCARIA ASIGNADA"
              ATTRIBUTE (REVERSE)
              SLEEP 4
              ERROR " "
              CLOSE WINDOW v1
              RETURN
           END IF

      DISPLAY BY NAME gr_cuenta.*
       
      PROMPT "               Presione <Enter> Para Continuar                "
      ATTRIBUTE (REVERSE)
      FOR vresp
      ATTRIBUTE (REVERSE)

      CLOSE WINDOW v1
END FUNCTION
#############################################################################
FUNCTION modifica_cuenta_promotor(vcodven)

      DEFINE vresp    CHAR(1)
      DEFINE vcodven  CHAR(11)

      OPEN WINDOW v1 AT  7,10 WITH FORM "PROM0254" ATTRIBUTE(BORDER)
      DISPLAY "                 dis_cuenta BANCARIAS PROMOTO",
            "RES                 " 
              AT 1,1 ATTRIBUTE(REVERSE)
      
             INITIALIZE gr_cuenta.* TO NULL

             LET vvcodven = vcodven
             SELECT rowid INTO vrowid FROM pro_cta_promotor
                    WHERE pro_cta_promotor.codven = vcodven

             SELECT cta_bancaria, bco_cod, 
                    tipo_cuenta, plaza INTO gr_cuenta.* 
             FROM   pro_cta_promotor
             WHERE  pro_cta_promotor.codven = vcodven

             IF STATUS = NOTFOUND THEN
                MENU "Agregar"
                     COMMAND "Agregar" "Agregar Cuenta Bancaria"
                              CALL Adiciona_banco()
                     COMMAND "Salir" "Regresar al Menu Anterior"
                              EXIT MENU
                         CLOSE WINDOW v1
                END MENU
              ELSE
                 MENU "Modificar"
                     COMMAND "Modificar" "Modificar Cuenta Bancaria"
                              CALL Modifica_banco()
                     COMMAND "Salir" "Regresar al Menu Anterior"
                              EXIT MENU
                         CLOSE WINDOW v1
                 END MENU
              END IF
      CLOSE WINDOW v1
END FUNCTION 
##############################################################################
  FUNCTION Adiciona_banco()
           DEFINE resp            CHAR(1)
           DEFINE vdesc_bco       CHAR(30)

           INPUT BY NAME gr_cuenta.*
          
           BEFORE FIELD bco_cod 
                 CALL despliega_icefas() RETURNING gr_cuenta.bco_cod, vdesc_bco
 
           AFTER FIELD plaza
                 PROMPT "Desea Dar de Alta este registro S/N "
                 ATTRIBUTE (REVERSE)
                 FOR RESP
                 ATTRIBUTE (REVERSE)

                 IF resp  MATCHES"[sS]" THEN
                    INSERT INTO pro_cta_promotor 
                           ( codven, cta_bancaria, bco_cod,
                             tipo_cuenta, plaza)
                           VALUES ( vvcodven, gr_cuenta.cta_bancaria,
                                    gr_cuenta.bco_cod, gr_cuenta.tipo_cuenta,
                                    gr_cuenta.plaza)
                    ERROR "Registro Ingresado Satisfactoriamente"
                    ATTRIBUTE (REVERSE)
                    CLEAR FORM
                    EXIT INPUT
                    SLEEP 2
                    RETURN
                 ELSE
                    CLEAR FORM
                    EXIT INPUT
                    RETURN   
                 END IF
      END INPUT
  END FUNCTION

##############################################################################
  FUNCTION Modifica_banco()
          DEFINE resp CHAR(1)

           DISPLAY BY NAME gr_cuenta.*
           INPUT BY NAME gr_cuenta.* WITHOUT DEFAULTS

           AFTER FIELD plaza
                 PROMPT "Desea Modificar este registro S/N "
                        ATTRIBUTE (REVERSE)
                 FOR RESP
                 ATTRIBUTE (REVERSE)

                 IF resp  MATCHES"[sS]" THEN
                    UPDATE pro_cta_promotor 
                       SET (cta_bancaria, bco_cod, tipo_cuenta, plaza)
                           = ( gr_cuenta.*)
                    WHERE  rowid = vrowid
                    ERROR "ACTUALIZACION EFECTUADA SATISFACTORIMENTE" 
                          ATTRIBUTE (REVERSE)
                    SLEEP 2
                    ERROR " "
                    CLEAR FORM
                    EXIT INPUT
                    RETURN
                 ELSE
                    CLEAR FORM
                    EXIT INPUT
                    RETURN   
                 END IF
      END INPUT
END FUNCTION

FUNCTION inicializa()
#--------------------
    DISPLAY "                                         " AT 5,30
    CLEAR FORM
END FUNCTION

FUNCTION despliega_capacitacion(c10_codven)
#dc----------------------------------------
    DEFINE 
        c10_codven            CHAR(10)

    OPEN WINDOW prom0255 AT 6,12 WITH FORM "PROM0255" ATTRIBUTE(BORDER)
    DISPLAY "             MANTENEDOR DE CALIFICACIO",
            "NES                                      " 
            AT 3,1 ATTRIBUTE(REVERSE)

    MENU "MENU"
        COMMAND KEY(A) "Agrega" "Agrega calificacion y horas de capacion"
            CALL agrega_calificacion(c10_codven) #ac
            CLEAR FORM

        COMMAND KEY(C) "Consulta" "Consulta calificacion y horas de capacion"
            CALL consulta_calificacion(c10_codven) #cc
            CLEAR FORM

        COMMAND KEY(M) "Modifica" "Modifica calificacion y horas de capacion"
            CALL modifica_calificacion(c10_codven) #mc
            CLEAR FORM

        COMMAND KEY(E) "Elimina" "Elimina calificacion y horas de capacion"
           #CALL elimina()    #e
            CLEAR FORM

        COMMAND KEY(S) "Salir" "Salir del programa"
            EXIT MENU
    END MENU
    CLOSE WINDOW prom0255
END FUNCTION

FUNCTION agrega_calificacion(c10_codven)
#ac-------------------------------------
    DEFINE arr_2 ARRAY[50] OF RECORD 
        fecha_capacita        LIKE pro_capacita.fecha_capacita ,
        calificacion          LIKE pro_capacita.calificacion   ,
        horas                 LIKE pro_capacita.horas
    END RECORD

    DEFINE 
        c10_codven            CHAR(10)

    DEFINE 
        i                     ,
        arr_c                 ,
        scr_l                 SMALLINT

    FOR i = 1 TO 50
        LET arr_2[i].fecha_capacita = NULL
        LET arr_2[i].calificacion   = NULL
        LET arr_2[i].horas          = NULL
    END FOR 

    INPUT ARRAY arr_2 WITHOUT DEFAULTS FROM scr_1.*
        BEFORE FIELD fecha_capacita
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD fecha_capacita 
            IF arr_2[arr_c].fecha_capacita IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD fecha_capacita
            END IF

        AFTER FIELD calificacion 
            IF arr_2[arr_c].calificacion IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD calificacion
            END IF

        AFTER FIELD horas 
            IF arr_2[arr_c].horas IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD horas
            END IF

        ON KEY (ESC)
            FOR i = 1 TO 50
                IF arr_2[i].fecha_capacita IS NOT NULL THEN
                    INSERT INTO pro_capacita
                        VALUES(c10_codven              ,
                               arr_2[i].fecha_capacita ,
                               arr_2[i].calificacion   ,
                               arr_2[i].horas          ,
                               HOY                     ,
                               1                       ,#status
                               ""                       #usuario
                              )
                ELSE
                    EXIT FOR
                END IF
            END FOR
            ERROR "REGISTRO INGRESADO" 
            CLEAR FORM
            EXIT INPUT
       
        ON KEY (INTERRUPT,CONTROL-C)
            EXIT INPUT
    END INPUT
END FUNCTION

FUNCTION consulta_calificacion(c10_codven)
#cc---------------------------------------
    DEFINE arr_3 ARRAY[50] OF RECORD 
        fecha_capacita        LIKE pro_capacita.fecha_capacita ,
        calificacion          LIKE pro_capacita.calificacion   ,
        horas                 LIKE pro_capacita.horas
    END RECORD

    DEFINE 
        c10_codven            CHAR(10)

    DEFINE 
        pos                   SMALLINT
       
    DECLARE cur_6 CURSOR FOR
       SELECT fecha_capacita ,
              calificacion   ,
              horas
       FROM   pro_capacita
       WHERE  codven = c10_codven

       LET pos = 1
    FOREACH cur_6 INTO arr_3[pos].*
        LET pos = pos + 1
    END FOREACH
   
    CALL SET_COUNT(pos-1)

    DISPLAY ARRAY arr_3 TO scr_1.*
        ON KEY (INTERRUPT,CONTROL-C)
            EXIT DISPLAY
    END DISPLAY
    CLEAR FORM
END FUNCTION

FUNCTION modifica_calificacion(c10_codven)
#mc---------------------------------------
    DEFINE arr_2 ARRAY[50] OF RECORD 
        fecha_capacita        LIKE pro_capacita.fecha_capacita ,
        calificacion          LIKE pro_capacita.calificacion   ,
        horas                 LIKE pro_capacita.horas
    END RECORD

    DEFINE 
        c10_codven            CHAR(10)

    DEFINE 
        i                     ,
        arr_c                 ,
        pos                   SMALLINT

    DECLARE cur_7 CURSOR FOR
        SELECT fecha_capacita ,
               calificacion   ,
               horas
        FROM   pro_capacita
        WHERE  codven = c10_codven

        LET pos = 1
    FOREACH cur_7 INTO arr_2[pos].*
        LET pos = pos + 1
    END FOREACH
   
    CALL SET_COUNT(pos-1)

    INPUT ARRAY arr_2 WITHOUT DEFAULTS FROM scr_1.*
        BEFORE FIELD fecha_capacita
            LET arr_c = ARR_CURR()

        AFTER FIELD fecha_capacita 
            IF arr_2[arr_c].fecha_capacita IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD fecha_capacita
            END IF

        AFTER FIELD calificacion 
            IF arr_2[arr_c].calificacion IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD calificacion
            END IF

        AFTER FIELD horas 
            IF arr_2[arr_c].horas IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD horas
            END IF

        ON KEY (ESC)
            DELETE
            FROM  pro_capacita
            WHERE codven = c10_codven

            FOR i = 1 TO pos - 1
                IF arr_2[i].fecha_capacita IS NOT NULL THEN
                    INSERT INTO pro_capacita
                        VALUES(c10_codven              ,
                               arr_2[i].fecha_capacita ,
                               arr_2[i].calificacion   ,
                               arr_2[i].horas          ,
                               HOY                     ,
                               1                       ,#status
                               ""                       #usuario
                              )
                ELSE
                    EXIT FOR
                END IF
            END FOR
            EXIT INPUT
       
        ON KEY (INTERRUPT, CONTROL-C)
            EXIT INPUT
    END INPUT
    CLEAR FORM
END FUNCTION
 
FUNCTION ayuda_grupo_de_venta()
#agdv--------------------------
    DEFINE reg_6 RECORD 
        coduni_n1             LIKE com_nivel1.coduni_n1   ,
        nombre_uni_n1         LIKE com_nivel1.nombre_uni_n1
    END RECORD

    DEFINE arr_4 ARRAY[2000] OF RECORD 
        coduni_n1             LIKE com_nivel1.coduni_n1   ,
        nombre_uni_n1         LIKE com_nivel1.nombre_uni_n1
    END RECORD

    DEFINE 
        txt_1                 CHAR(1500) ,
        x_busca               CHAR(2000)

    DEFINE 
        pos                      SMALLINT

    INITIALIZE x_busca, txt_1 TO NULL
    LET sw_1 = 0
    OPEN WINDOW prom0258 AT 06,13 WITH FORM "PROM0258" ATTRIBUTE(BORDER)
    DISPLAY "<ESC> Busqueda    <ENTER> Aceptar    <CTRL-C> Salir    ",
            "                        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                  GRUPO DE VENTA                     ",
            "                          " AT 2,1 ATTRIBUTE(REVERSE)
        CONSTRUCT BY NAME x_busca ON coduni_n1, nombre_uni_n1
            ON KEY(ESC)
                LET INT_FLAG = FALSE
                EXIT CONSTRUCT

            ON KEY(INTERRUPT, CONTROL-C)
                LET sw_1 = 1
                EXIT CONSTRUCT
        END CONSTRUCT

        IF sw_1 = 1 THEN
            CLOSE WINDOW prom0258
            RETURN "","                      "
        END IF

        LET txt_1 = " SELECT coduni_n1     ,",
                    "        nombre_uni_n1  ",
                    " FROM   com_nivel1     ",
                    " WHERE  ",x_busca CLIPPED

        PREPARE pre_8 FROM txt_1 
        DECLARE cur_8 CURSOR FOR pre_8

        LET pos = 1
        FOREACH cur_8 INTO arr_4[pos].*
            IF pos = 2000 THEN
                ERROR "FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO"
                EXIT FOREACH
            END IF
            LET pos = pos + 1
        END FOREACH

        IF (pos-1) < 1 THEN
            LET sw_1 = 1
            ERROR "SELECCION GRUPO DE VENTA..... VACIO"
        END IF

        CALL SET_COUNT(pos-1)

        DISPLAY ARRAY arr_4 TO scr_1.*
            ON KEY (INTERRUPT,CONTROL-C)
                LET sw_1 = 1
                EXIT DISPLAY

            ON KEY (CONTROL-M)
                LET pos = ARR_CURR()
                EXIT DISPLAY
        END DISPLAY

    CLOSE WINDOW prom0258

    IF sw_1 = 1 THEN
        RETURN "","                      "
    END IF

    RETURN arr_4[pos].coduni_n1,arr_4[pos].nombre_uni_n1
END FUNCTION

FUNCTION ayuda_tipo_promotor()
#atp--------------------------
    DEFINE reg_7 RECORD 
        cod_tipo_prom         LIKE com_tipo_promotor.cod_tipo_prom,
        desc_tipo             LIKE com_tipo_promotor.desc_tipo    
    END RECORD

    DEFINE arr_5 ARRAY[2000] OF RECORD 
        cod_tipo_prom         LIKE com_tipo_promotor.cod_tipo_prom,
        desc_tipo             LIKE com_tipo_promotor.desc_tipo    
    END RECORD

    DEFINE 
        txt_1                 CHAR(1500) ,
        x_busca               CHAR(2000)

    DEFINE 
        pos                      SMALLINT

    INITIALIZE x_busca, txt_1 TO NULL
    LET sw_1 = 0
    OPEN WINDOW prom0259 AT 06,13 WITH FORM "PROM0259" ATTRIBUTE(BORDER)
    DISPLAY "<ESC> Busqueda    <ENTER> Aceptar    <CTRL-C> Salir    ",
            "                        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                 TIPO DE PROMOTOR               ",
            "                               " AT 2,2 ATTRIBUTE(REVERSE)
        CONSTRUCT BY NAME x_busca ON cod_tipo_prom, desc_tipo
            ON KEY(ESC)
                LET INT_FLAG = FALSE
                EXIT CONSTRUCT

            ON KEY(INTERRUPT, CONTROL-C)
                LET sw_1 = 1
                EXIT CONSTRUCT
        END CONSTRUCT

        IF sw_1 = 1 THEN
            CLOSE WINDOW prom0259
            RETURN "","                      "
        END IF

        LET txt_1 = " SELECT cod_tipo_prom    ,",
                    "        desc_tipo         ",
                    " FROM   com_tipo_promotor ",
                    " WHERE  ",x_busca CLIPPED

        PREPARE pre_9 FROM txt_1 
        DECLARE cur_9 CURSOR FOR pre_9

        LET pos = 1
        FOREACH cur_9 INTO arr_5[pos].*
            IF pos = 2000 THEN
                ERROR "FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO"
                EXIT FOREACH
            END IF
            LET pos = pos + 1
        END FOREACH

        IF (pos-1) < 1 THEN
            LET sw_1 = 1
            ERROR "SELECCION GRUPO DE VENTA..... VACIO"
        END IF

        CALL SET_COUNT(pos-1)

        DISPLAY ARRAY arr_5 TO scr_1.*
            ON KEY (INTERRUPT,CONTROL-C)
                LET sw_1 = 1
                EXIT DISPLAY

            ON KEY (CONTROL-M)
                LET pos = ARR_CURR()
                EXIT DISPLAY
        END DISPLAY

    CLOSE WINDOW prom0259

    IF sw_1 = 1 THEN
        RETURN "","                      "
    END IF

    RETURN arr_5[pos].cod_tipo_prom,arr_5[pos].desc_tipo
END FUNCTION

FUNCTION restaura()
    DEFINE 
        x_busca               CHAR(2000) ,
        txt_1                 CHAR(1500)

    DEFINE 
        arr_c                 ,
        pos                   SMALLINT

    DEFINE vfecha_ult_mod     DATETIME YEAR TO SECOND

    DEFINE reg_rest RECORD 
        codven                LIKE pro_mae_promotor.codven          ,
        seguro                LIKE pro_mae_promotor.seguro          ,
        nip                   LIKE pro_mae_promotor.nip             ,
        agenc_cod             LIKE pro_mae_promotor.agenc_cod       ,
        unico                 LIKE pro_mae_promotor.unico           ,
##        tipo_recibo           LIKE pro_mae_promotor.tipo_recibo     ,
        escolar               LIKE pro_mae_promotor.escolar         ,
        rfc                   CHAR(10)                              ,
        paterno               LIKE pro_mae_promotor.paterno         ,
        materno               LIKE pro_mae_promotor.materno         ,
        nombres               LIKE pro_mae_promotor.nombres         ,
        fingre                LIKE pro_mae_promotor.fingre          ,
        fenvio                LIKE pro_mae_promotor.fenvio          ,
        fecha_registro        LIKE pro_mae_promotor.fecha_registro  ,
        fecha_baja            LIKE pro_mae_promotor.fecha_baja      ,
        calle                 LIKE pro_mae_promotor.calle           ,
        numero                LIKE pro_mae_promotor.numero          ,
        dpto                  LIKE pro_mae_promotor.dpto            ,
        colonia               LIKE pro_mae_promotor.colonia         ,
        deleg                 LIKE pro_mae_promotor.deleg           ,
        ciudad                LIKE pro_mae_promotor.ciudad          ,
        estado                LIKE pro_mae_promotor.estado          ,          
        codpos                LIKE pro_mae_promotor.codpos          ,
        fono                  LIKE pro_mae_promotor.fono            ,
        sup                   LIKE pro_mae_promotor.sup             ,
        nivel                 LIKE pro_mae_promotor.nivel           ,
        resuelva              LIKE pro_mae_promotor.resuelva        ,

        sexo                  CHAR (1)                              , --(v10)
        edo_naci              CHAR (2)                              , --(v10)

        fnaci                 LIKE pro_mae_promotor.fnaci           ,
        diag_proceso          LIKE pro_mae_promotor.diag_proceso    ,
        fautoriz              LIKE pro_mae_promotor.fautoriz        ,
        status                LIKE pro_mae_promotor.status          ,
        nro_solicitud         LIKE pro_mae_promotor.nro_solicitud   ,
        status_interno        LIKE pro_mae_promotor.status_interno  ,
        fecha_certifi         LIKE pro_mae_promotor.fecha_certifi   ,
        motivo_suspende       LIKE pro_mae_promotor.motivo_suspende ,
        fecha_suspende        LIKE pro_mae_promotor.fecha_suspende  ,
        fech_credencial       LIKE pro_mae_promotor.fech_credencial ,
        cod_promotor          LIKE pro_mae_promotor.cod_promotor
    END RECORD
                                                                               
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " RESTAURA " at 1,65 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C = Salir                               Esc = Seleccion  " AT 2,1

    LET sw_1     = 0
    LET INT_FLAG = FALSE
    CONSTRUCT BY NAME x_busca ON codven,cod_promotor,paterno,materno,nombres
        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT, CONTROL-C)
            LET sw_1 = 1
            EXIT CONSTRUCT
    END CONSTRUCT

    IF sw_1 = 1 THEN
        RETURN
    END IF

    LET txt_1 = " SELECT A.codven          ,",
                "        A.cod_promotor    ,",
                "        A.agenc_cod       ,",
                "        ' '               ,", #desagenc
                "        A.nivel           ,",
                "        ' '               ,", #desnivel
                "        0                 ,", #num.ocurrencia
                "        A.seguro          ,",                            
                "        A.rfc[1,10]       ,",
                "        A.rfc[11,13]      ,",
                "        A.unico           ,",
##                "        A.tipo_recibo     ,",
                "        A.escolar         ,",
                "        A.paterno         ,",
                "        A.materno         ,",
                "        A.nombres         ,",
                "        A.fnaci           ,",
                "        A.fenvio          ,",
                "        A.fecha_registro  ,",
                "        A.fecha_baja      ,",
                "        A.fecha_suspende  ,",
                "        A.resuelva        ,",

                "        ' '               ,", --(v10)
                "        ' '               ,", --(v10)

                "        A.calle           ,",
                "        A.numero          ,",
                "        A.dpto            ,",
                "        A.codpos          ,",
                "        A.colonia         ,",
                "        A.deleg           ,",
                "        ' '               ,", #desdeleg                       
                "        A.ciudad          ,",
                "        ' '               ,", #desciudad
                "        A.estado          ,",
                "        ' '               ,", #desestad
                "        A.fono             ",
                " FROM   pro_mae_promotor A ",
                " WHERE  A.status_interno = 6 ",
                " AND    ",x_busca CLIPPED

    PREPARE pre_10 FROM txt_1
    DECLARE cur_10 CURSOR FOR pre_10

    LET pos = 1
    FOREACH cur_10 INTO arr_1[pos].*
        IF pos = 2000 THEN
            ERROR "ARREGLO LLENO"
            EXIT FOREACH                                                     
        ELSE
            SELECT UNIQUE A.nombre_uni_n1
            INTO   arr_1[pos].desagenc
            FROM   com_nivel1 A
            WHERE  A.coduni_n1 = arr_1[pos].agenc_cod

            SELECT UNIQUE A.desc_tipo
            INTO   arr_1[pos].desnivel
            FROM   com_tipo_promotor A
            WHERE  A.cod_tipo_prom = arr_1[pos].nivel

            SELECT UNIQUE A.deleg_desc
            INTO   arr_1[pos].desdeleg
            FROM   tab_delegacion A                                            
            WHERE  A.deleg_cod = arr_1[pos].deleg

            SELECT UNIQUE A.ciudad_desc
            INTO   arr_1[pos].desciudad
            FROM   tab_ciudad A
            WHERE  A.ciudad_cod = arr_1[pos].ciudad

            SELECT UNIQUE A.estad_desc
            INTO   arr_1[pos].desestad
            FROM   tab_estado A
            WHERE  A.estad_cod = arr_1[pos].estado
--(v10) inicia

            CALL curp(arr_1[pos].unico) RETURNING arr_1[pos].sexo,
                                                  arr_1[pos].edo_naci
--(v10) finaliza

            LET pos = pos + 1
        END IF
    END FOREACH

    IF pos = 1 THEN
        ERROR" NO HAY REGISTROS"
        RETURN
    END IF

    DISPLAY " 1"  TO FORMONLY.son      
    DISPLAY total_reg  TO FORMONLY.total_reg

    CALL SET_COUNT(pos-1)
                                                                         
    INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.*
        BEFORE FIELD codven
            LET arr_c = ARR_CURR()
            IF arr_c > pos-1 THEN
                DISPLAY "","" AT 1,1
                ERROR "NO HAY MAS REGISTROS HACIA ABAJO"
            ELSE
                CALL despliega_estado_del_promotor(arr_1[arr_c].cod_promotor) #dedp
            END IF

        AFTER FIELD codven

        ON KEY (ESC)
             LET sw_1 = 0

             PROMPT "DESEA RESTAURAR  S/N" FOR CHAR enter

             IF enter MATCHES "[SsNn]" THEN
                IF enter MATCHES "[Ss]" THEN                               

                   SELECT MAX(fecha_ult_mod)
                   INTO   vfecha_ult_mod
                   FROM   pro_his_mod
                   WHERE  cod_promotor = arr_1[arr_c].cod_promotor

                   SELECT UNIQUE A.codven,
                          A.seguro,
                          A.nip,
                          A.agenc_cod,
                          A.unico,
                          A.rfc,
                          A.paterno,
                          A.materno,
                          A.nombres,
                          A.fingre,
                          A.fenvio,
                          A.fecha_registro,
                          A.fecha_baja,
                          A.calle,
                          A.numero,
                          A.dpto,
                          A.colonia,
                          A.deleg,
                          A.ciudad,
                          A.estado,
                          A.codpos,
                          A.fono,
                          A.sup,
                          A.nivel,
                          A.resuelva,
                          A.fnaci,
                          A.diag_proceso,
                          A.fautoriz,
                          A.status,
                          A.nro_solicitud,
                          A.status_interno,
                          A.fecha_certifi,
                          A.motivo_suspende,
                          A.fecha_suspende,
                          A.fech_credencial,
                          A.cod_promotor                                 
                    INTO  reg_rest.codven,
                          reg_rest.seguro,
                          reg_rest.nip,
                          reg_rest.agenc_cod,
                          reg_rest.unico,
                          reg_rest.rfc,
                          reg_rest.paterno,
                          reg_rest.materno,
                          reg_rest.nombres,
                          reg_rest.fingre,
                          reg_rest.fenvio,
                          reg_rest.fecha_registro,
                          reg_rest.fecha_baja,
                          reg_rest.calle,
                          reg_rest.numero,
                          reg_rest.dpto,
                          reg_rest.colonia,
                          reg_rest.deleg,
                          reg_rest.ciudad,                 
                          reg_rest.estado,
                          reg_rest.codpos,
                          reg_rest.fono,
                          reg_rest.sup,
                          reg_rest.nivel,
                          reg_rest.resuelva,
                          reg_rest.fnaci,
                          reg_rest.diag_proceso,
                          reg_rest.fautoriz,
                          reg_rest.status,
                          reg_rest.nro_solicitud,
                          reg_rest.status_interno,
                          reg_rest.fecha_certifi,
                          reg_rest.motivo_suspende,
                          reg_rest.fecha_suspende,
                          reg_rest.fech_credencial,
                          reg_rest.cod_promotor           
                    FROM  pro_his_mod A
                    WHERE A.fecha_ult_mod   = vfecha_ult_mod
                    AND   A.cod_promotor    = arr_1[arr_c].cod_promotor
                   
                    UPDATE pro_mae_promotor
                    SET pro_mae_promotor.codven          = reg_rest.codven,
                        pro_mae_promotor.seguro          = reg_rest.seguro,     
                        pro_mae_promotor.nip             = reg_rest.nip   ,  
                        pro_mae_promotor.agenc_cod       = reg_rest.agenc_cod,
                        pro_mae_promotor.unico           = reg_rest.unico,
                        pro_mae_promotor.rfc             = reg_rest.rfc,
                        pro_mae_promotor.paterno         = reg_rest.paterno,
                        pro_mae_promotor.materno         = reg_rest.materno,
                        pro_mae_promotor.nombres         = reg_rest.nombres,
                        pro_mae_promotor.fingre          = reg_rest.fingre,      
                        pro_mae_promotor.fenvio          = reg_rest.fenvio,     
                        pro_mae_promotor.fecha_registro  = reg_rest.fecha_registro,    
                        pro_mae_promotor.fecha_baja      = reg_rest.fecha_baja,   
                        pro_mae_promotor.calle           = reg_rest.calle,  
                        pro_mae_promotor.numero          = reg_rest.numero,
                        pro_mae_promotor.dpto            = reg_rest.dpto,
                        pro_mae_promotor.colonia         = reg_rest.colonia,  
                        pro_mae_promotor.deleg           = reg_rest.deleg,     
                        pro_mae_promotor.ciudad          = reg_rest.ciudad,
                        pro_mae_promotor.estado          = reg_rest.estado,
                        pro_mae_promotor.codpos          = reg_rest.codpos,
                        pro_mae_promotor.fono            = reg_rest.fono,
                        pro_mae_promotor.sup             = reg_rest.sup,
                        pro_mae_promotor.nivel           = reg_rest.nivel,
                        pro_mae_promotor.resuelva        = reg_rest.resuelva,   
                        pro_mae_promotor.fnaci           = reg_rest.fnaci,  
                        pro_mae_promotor.diag_proceso    = reg_rest.diag_proceso, 
                        pro_mae_promotor.fautoriz        = reg_rest.fautoriz,
                        pro_mae_promotor.status          = reg_rest.status,         
                        pro_mae_promotor.nro_solicitud   = reg_rest.nro_solicitud,     
                        pro_mae_promotor.status_interno  = reg_rest.status_interno,    
                        pro_mae_promotor.fecha_certifi   = reg_rest.fecha_certifi,   
                        pro_mae_promotor.motivo_suspende = reg_rest.motivo_suspende,  
                        pro_mae_promotor.fecha_suspende  = reg_rest.fecha_suspende,
                        pro_mae_promotor.fech_credencial = reg_rest.fech_credencial, 
                        pro_mae_promotor.cod_promotor    = reg_rest.cod_promotor
                  WHERE pro_mae_promotor.cod_promotor    = arr_1[arr_c].cod_promotor
                  AND   pro_mae_promotor.codven          = arr_1[arr_c].codven

                  DELETE 
                  FROM pro_his_mod
                  WHERE fecha_ult_mod   = vfecha_ult_mod

                  DISPLAY "REGISTRO RESTAURADO"
                  AT 21,2 ATTRIBUTE(REVERSE)
                  SLEEP 2
                  CLEAR FORM
               ELSE
                  ERROR "PROCESO DE RESTAURAR,CANCELADO"             
                  SLEEP 2
                  ERROR " "
                  INITIALIZE reg_rest.* TO NULL
                  CLEAR FORM                                       
               END IF
            END IF
            DISPLAY "" AT 21,1
    EXIT INPUT
    END INPUT
END FUNCTION                                                                   

FUNCTION despliega_historico(reg_his)
   DEFINE reg_his RECORD      
          paterno               LIKE pro_mae_promotor.paterno         ,
          materno               LIKE pro_mae_promotor.materno         , 
          nombres               LIKE pro_mae_promotor.nombres         ,
          cod_promotor          LIKE pro_mae_promotor.cod_promotor    ,
          fecha_registro        LIKE pro_mae_promotor.fecha_registro  ,
          agenc_cod             LIKE pro_mae_promotor.agenc_cod       ,
          nivel                 LIKE pro_mae_promotor.nivel           ,  
          fenvio                LIKE pro_mae_promotor.fenvio          ,   
          codven                LIKE pro_mae_promotor.codven          
   END RECORD 

   DEFINE reg_his2 ARRAY[2000] OF RECORD 
          cod_proceso           SMALLINT                              , 
          desc_corta            CHAR(1)                               ,  
          fenvio                LIKE pro_mae_promotor.fenvio          ,
          diag_proceso          LIKE pro_mae_promotor.diag_proceso    ,   
          diag_desc             CHAR(80)   
   END RECORD 

   DEFINE reg_fecha    RECORD 
         fenvio        DATE,
         folio         INTEGER ,
         diag_proceso  CHAR(2)
   END RECORD

   DEFINE     
           vnombre                CHAR(40)                              ,
           vnro_solicitud         LIKE pro_mae_promotor.nro_solicitud   ,
           vdesc_agenc            CHAR(40)                              ,
                  vdesc_nivel            CHAR(50)                              ,
           vfingre                DATE                                  ,
           vdiag_proceso          CHAR(2)                               ,
           vfolio                 INTEGER

   DEFINE  pos     SMALLINT,
            i       SMALLINT,                    
           var     DATE
  

   WHENEVER ERROR CONTINUE
      CREATE TEMP TABLE temp_pro_25 
      (cod_proceso              SMALLINT,
       desc_corta               CHAR(1),  
       fenvio                   DATE,
       diag_proceso             CHAR(2),
       diag_desc                CHAR(80)   
      )
   WHENEVER ERROR STOP


   OPEN WINDOW prom0257 AT 6,2 WITH FORM "PROM0257" ATTRIBUTE (BORDER)
   DISPLAY " (Ctrl-C) Salir " AT 1,1
   DISPLAY "                   HISTORICO MAESTRO DE PROMOTO",
            "RES                             " AT 2,1 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   DECLARE cur_his CURSOR FOR

   SELECT  A.diag_proceso
   FROM    pro_mae_promotor A
   WHERE   A.cod_promotor = reg_his.cod_promotor

   LET pos = 1
   FOREACH cur_his INTO reg_his2[pos].diag_proceso


      LET vdiag_proceso = reg_his2[pos].diag_proceso

      SELECT UNIQUE nro_solicitud,fingre
      INTO   vnro_solicitud,vfingre
      FROM   pro_solicitud
      WHERE  cod_promotor  = reg_his.cod_promotor

      SELECT UNIQUE nombre_uni_n1
      INTO   vdesc_agenc
      FROM   com_nivel1
      WHERE  coduni_n1 = reg_his.agenc_cod       

      SELECT UNIQUE desc_tipo
      INTO   vdesc_nivel
      FROM   com_tipo_promotor
      WHERE  cod_tipo_prom = reg_his.nivel

      LET vnombre = reg_his.paterno CLIPPED," ",    
                    reg_his.materno CLIPPED," ",
                    reg_his.nombres


#####altas envio 
      IF vnro_solicitud <> 0 THEN

      DECLARE cur_fenvio CURSOR FOR
         SELECT   UNIQUE fenvio 
         FROM     pro_envio_alta
         WHERE    nro_solicitud = vnro_solicitud
         ORDER BY fenvio

      LET pos =1 
      FOREACH cur_fenvio INTO reg_his2[pos].fenvio

         IF reg_his2[pos].fenvio IS NOT NULL THEN
            LET reg_his2[pos].diag_proceso = NULL
            LET reg_his2[pos].diag_desc    = NULL
         END IF

         LET reg_his2[pos].cod_proceso = 301
         LET reg_his2[pos].desc_corta = "E"

         INSERT INTO temp_pro_25  
                VALUES ( reg_his2[pos].cod_proceso,
                         reg_his2[pos].desc_corta,
                         reg_his2[pos].fenvio,
                         reg_his2[pos].diag_proceso,
                         reg_his2[pos].diag_desc
                       )

          LET pos = pos +1
      END FOREACH

      ELSE
         LET  reg_his2[pos].fenvio  = reg_his.fenvio

         IF reg_his2[pos].fenvio IS NOT NULL THEN
            LET reg_his2[pos].diag_proceso = NULL
            LET reg_his2[pos].diag_desc    = NULL
         END IF

         LET reg_his2[pos].cod_proceso = 301
         LET reg_his2[pos].desc_corta = "E"

         INSERT INTO temp_pro_25  
                VALUES ( reg_his2[pos].cod_proceso,
                         reg_his2[pos].desc_corta,
                         reg_his2[pos].fenvio,
                         reg_his2[pos].diag_proceso,
                         reg_his2[pos].diag_desc
                       )
      END IF
--recepcion altas

      SELECT "X"
      FROM   pro_det_agte
      WHERE  cod_promotor = reg_his.cod_promotor
        --AND  cve_afore_consar = 301                        --(v2)
        AND  cve_afore_consar <> 308                        --(v3)
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         LET pos = pos + 1

         DECLARE cur_his2 CURSOR FOR
            SELECT fecha_registro,
                   diag_proceso
            FROM   pro_det_agte
            WHERE  cod_promotor = reg_his.cod_promotor
--              AND  cve_afore_consar = 301                  --(v2)
              AND  cve_afore_consar <> 308                        --(v3)

            ORDER BY fecha_registro                     

         FOREACH cur_his2 INTO reg_his2[pos].fenvio,
                                   reg_his2[pos].diag_proceso

             SELECT UNIQUE diagn_desc
             INTO   reg_his2[pos].diag_desc
             FROM   tab_diagnos_pro
             WHERE  diagn_cod = reg_his2[pos].diag_proceso 

             LET reg_his2[pos].cod_proceso = 301
             LET reg_his2[pos].desc_corta  = "R"

             INSERT INTO temp_pro_25  
                    VALUES ( reg_his2[pos].cod_proceso,
                             reg_his2[pos].desc_corta,
                             reg_his2[pos].fenvio,
                             reg_his2[pos].diag_proceso,
                             reg_his2[pos].diag_desc
                           )

             LET pos = pos + 1
         END FOREACH
      ELSE
         LET pos = pos + 1
      END IF
###fin altas                                        

###envio bajas

      SELECT "OK"
      FROM   pro_envio_scb
      WHERE  cod_promotor = reg_his.cod_promotor
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN       

         DECLARE cur_fenvio2 CURSOR FOR
            SELECT UNIQUE fecha_genera
            FROM   pro_envio_scb
            WHERE  cod_promotor = reg_his.cod_promotor          

         LET pos =1 
         FOREACH cur_fenvio2 INTO reg_his2[pos].fenvio

            IF reg_his2[pos].fenvio IS NOT NULL THEN
               LET reg_his2[pos].diag_proceso = NULL
               LET reg_his2[pos].diag_desc    = NULL
            END IF

            LET reg_his2[pos].cod_proceso = 303
            LET reg_his2[pos].desc_corta  = "E"

            INSERT INTO temp_pro_25  
                   VALUES ( reg_his2[pos].cod_proceso,
                            reg_his2[pos].desc_corta,
                            reg_his2[pos].fenvio,
                            reg_his2[pos].diag_proceso,
                            reg_his2[pos].diag_desc
                          )
              LET pos = pos +1
         END FOREACH
      END IF
--recepcion bajas

      SELECT "OK"
      FROM   pro_recep_scb
      WHERE  cod_promotor = reg_his.cod_promotor
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         LET pos = pos + 1

         DECLARE cur_his4 CURSOR FOR
            SELECT UNIQUE fecha_scb, 
                   cve_scb                
            FROM   pro_recep_scb 
            WHERE  cod_promotor = reg_his.cod_promotor   
            ORDER BY fecha_scb                          

         FOREACH cur_his4 INTO reg_his2[pos].fenvio,
                                     reg_his2[pos].diag_proceso 

            SELECT UNIQUE diagn_desc
            INTO   reg_his2[pos].diag_desc
            FROM   tab_diagnos_pro
            WHERE  diagn_cod = reg_his2[pos].diag_proceso

            LET reg_his2[pos].cod_proceso   = 303
            LET reg_his2[pos].desc_corta    = "R"

            INSERT INTO temp_pro_25  
                   VALUES ( reg_his2[pos].cod_proceso,
                            reg_his2[pos].desc_corta,
                            reg_his2[pos].fenvio,
                            reg_his2[pos].diag_proceso,
                            reg_his2[pos].diag_desc
                          )

            LET pos = pos + 1
         END FOREACH
      ELSE
         LET pos = pos + 1
      END IF
###fin bajas

#### envio revalidacion 

      SELECT UNIQUE "OK"       
      FROM   pro_det_revalida  
      WHERE  cod_promotor = reg_his.cod_promotor

      IF STATUS <> NOTFOUND THEN       
            
         DECLARE cur_11 CURSOR FOR
         SELECT UNIQUE fecha_proceso
         FROM   pro_det_revalida
         WHERE  cod_promotor = reg_his.cod_promotor

         FOREACH cur_11 INTO reg_his2[pos].fenvio
             IF reg_his2[pos].fenvio IS NOT NULL THEN
                 LET reg_his2[pos].diag_proceso = NULL
                 LET reg_his2[pos].diag_desc    = NULL
             END IF

             LET reg_his2[pos].cod_proceso = 302
             LET reg_his2[pos].desc_corta ="E"

             INSERT INTO temp_pro_25  
             VALUES ( reg_his2[pos].cod_proceso,
                      reg_his2[pos].desc_corta,
                      reg_his2[pos].fenvio,
                      reg_his2[pos].diag_proceso,
                      reg_his2[pos].diag_desc
                    )
         END FOREACH
      END IF
--recepcion revalidacion

      SELECT UNIQUE "OK"
      FROM   pro_det_revalida
      WHERE  cod_promotor = reg_his.cod_promotor
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         LET pos = pos + 1

         DECLARE cur_his6 CURSOR FOR
            SELECT fecha_reval
            FROM  pro_det_revalida
            WHERE  cod_promotor = reg_his.cod_promotor
            ORDER BY fecha_reval      

         FOREACH cur_his6 INTO reg_his2[pos].fenvio
 
            DECLARE cur_12 CURSOR FOR
            SELECT diag_reval
            FROM   pro_det_revalida
            WHERE  cod_promotor = reg_his.cod_promotor
            AND    fecha_reval  = reg_his2[pos].fenvio
            GROUP BY 1

            FOREACH cur_12 INTO reg_his2[pos].diag_proceso
                SELECT UNIQUE diagn_desc
                INTO   reg_his2[pos].diag_desc
                FROM   tab_diagnos_pro
                WHERE  diagn_cod = reg_his2[pos].diag_proceso

                LET reg_his2[pos].cod_proceso   = 302
                LET reg_his2[pos].desc_corta    = "R"
      
                INSERT INTO temp_pro_25  
                 VALUES ( reg_his2[pos].cod_proceso,
                          reg_his2[pos].desc_corta,
                          reg_his2[pos].fenvio,
                          reg_his2[pos].diag_proceso,
                          reg_his2[pos].diag_desc
                         ) 

            LET pos = pos + 1
            END FOREACH 
         END FOREACH
      ELSE
         LET pos = pos + 1
      END IF
#### fin revalidacion

#### envio actualizacion  

      SELECT UNIQUE "OK"       
      FROM   pro_envio_mod     
      WHERE  cod_promotor = reg_his.cod_promotor
     
      IF STATUS <> NOTFOUND THEN       

         SELECT UNIQUE fenvio
         INTO   reg_his2[pos].fenvio
         FROM   pro_envio_mod
         WHERE  cod_promotor = reg_his.cod_promotor
  

         IF reg_his2[pos].fenvio IS NOT NULL THEN
            LET reg_his2[pos].diag_proceso = NULL
            LET reg_his2[pos].diag_desc    = NULL
         END IF

         LET reg_his2[pos].cod_proceso = 304
         LET reg_his2[pos].desc_corta  = "E"

         INSERT INTO temp_pro_25  
                VALUES ( reg_his2[pos].cod_proceso,
                         reg_his2[pos].desc_corta,
                         reg_his2[pos].fenvio,
                         reg_his2[pos].diag_proceso,
                         reg_his2[pos].diag_desc
                       )
      END IF
--recepcion actualizacion

      SELECT UNIQUE "OK"  
      FROM   pro_resul_mod  
      WHERE  cod_promotor = reg_his.cod_promotor
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         LET pos = pos + 1

         DECLARE cur_his8 CURSOR FOR
            SELECT fecha_proceso,
                   diag_proceso
            FROM   pro_resul_mod  
            WHERE  cod_promotor = reg_his.cod_promotor
            GROUP BY 1,2

         FOREACH cur_his8 INTO reg_his2[pos].fenvio,
                               reg_his2[pos].diag_proceso
 
            SELECT UNIQUE diagn_desc
            INTO   reg_his2[pos].diag_desc
            FROM   tab_diagnos_pro
            WHERE  diagn_cod = reg_his2[pos].diag_proceso

            LET reg_his2[pos].cod_proceso  = 304
            LET reg_his2[pos].desc_corta   = "R"
      
            INSERT INTO temp_pro_25  
                   VALUES ( reg_his2[pos].cod_proceso,
                            reg_his2[pos].desc_corta,
                            reg_his2[pos].fenvio,
                            reg_his2[pos].diag_proceso,
                            reg_his2[pos].diag_desc
                          )

            LET pos = pos + 1
         END FOREACH
      ELSE
         LET pos = pos + 1
      END IF
#### fin actualizacion

#### aviso examen
         DECLARE cur_his9 CURSOR FOR
            SELECT fecha_proceso
            FROM   pro_aviso_examen
            WHERE  cod_promotor = reg_his.cod_promotor
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN       
               FOREACH cur_his9 INTO reg_his2[pos].fenvio
 
            LET reg_his2[pos].cod_proceso  = 306
            LET reg_his2[pos].desc_corta   = "R"
      
            INSERT INTO temp_pro_25  
                   VALUES ( reg_his2[pos].cod_proceso,
                            reg_his2[pos].desc_corta,
                            reg_his2[pos].fenvio,
                            reg_his2[pos].diag_proceso,
                            reg_his2[pos].diag_desc
                          )

             LET pos = pos + 1
          END FOREACH
      END IF
#### fin aviso examen

### resultado examen
      SELECT UNIQUE "OK"       
      FROM   pro_resul_examen  
      WHERE  cod_promotor = reg_his.cod_promotor
     
      IF STATUS <> NOTFOUND THEN       

         DECLARE cur_his10 CURSOR FOR
            SELECT diag_proceso,
                   fecha_proceso
            FROM   pro_resul_examen
            WHERE  cod_promotor = reg_his.cod_promotor
            GROUP BY 1,2

         FOREACH cur_his10 INTO reg_his2[pos].diag_proceso,
                                   reg_his2[pos].fenvio

            SELECT UNIQUE diagn_desc
            INTO   reg_his2[pos].diag_desc
            FROM   tab_diagnos_pro
            WHERE  diagn_cod = reg_his2[pos].diag_proceso

            LET reg_his2[pos].cod_proceso  = 307
            LET reg_his2[pos].desc_corta   = "R"
      
            INSERT INTO temp_pro_25  
                   VALUES ( reg_his2[pos].cod_proceso,
                            reg_his2[pos].desc_corta,
                            reg_his2[pos].fenvio,
                            reg_his2[pos].diag_proceso,
                            reg_his2[pos].diag_desc
                          )

             LET pos = pos + 1
          END FOREACH
       END IF
### fin resultado examen

### ENVIO REACTIVACION ###                                             --(v1)

    IF vnro_solicitud <> 0 THEN                                        --(v1)

        DECLARE cur_his11 CURSOR FOR                                   --(v1)
        SELECT  A.fenvio                                               --(v1)
        FROM    pro_envio_reac A                                   --(v1)
        WHERE   A.nro_solicitud  = vnro_solicitud                      --(v1) 
        ORDER BY fenvio                                                --(v1)

        LET pos =1
        FOREACH cur_his11 INTO reg_his2[pos].fenvio                    --(v1)

            IF reg_his2[pos].fenvio IS NOT NULL THEN                   --(v1)
                LET reg_his2[pos].diag_proceso = NULL                  --(v1)
                LET reg_his2[pos].diag_desc    = NULL                  --(v1)
            END IF

            LET reg_his2[pos].cod_proceso = 308                        --(v1)
            LET reg_his2[pos].desc_corta = "E"                         --(v1)

            INSERT INTO temp_pro_25 VALUES( reg_his2[pos].cod_proceso,   --(v1)
                                            reg_his2[pos].desc_corta,    --(v1)
                                            reg_his2[pos].fenvio,        --(v1)
                                            reg_his2[pos].diag_proceso,  --(v1)
                                            reg_his2[pos].diag_desc )    --(v1)

            LET pos = pos +1                                           --(v1)
        END FOREACH                                                    --(v1)
    ELSE                                                               --(v1)
        LET  reg_his2[pos].fenvio  = reg_his.fenvio                    --(v1)

        IF reg_his2[pos].fenvio IS NOT NULL THEN                       --(v1)
            LET reg_his2[pos].diag_proceso = NULL                      --(v1)
            LET reg_his2[pos].diag_desc    = NULL                      --(v1)
        END IF

        LET reg_his2[pos].cod_proceso = 308                            --(v1)
        LET reg_his2[pos].desc_corta = "E"                             --(v1)
        INSERT INTO temp_pro_25 VALUES( reg_his2[pos].cod_proceso,       --(v1)
                                        reg_his2[pos].desc_corta,        --(v1)
                                        reg_his2[pos].fenvio,            --(v1)
                                        reg_his2[pos].diag_proceso,      --(v1)
                                        reg_his2[pos].diag_desc )        --(v1)
    END IF

### RECEPCION REACTIVACION ###                                         --(v1)

    SELECT "X"                                                         --(v1)
    FROM   pro_det_agte A                                              --(v1)
    WHERE  A.cod_promotor = reg_his.cod_promotor                       --(v1)
    AND    A.cve_afore_consar = 308                                    --(v2)
    GROUP BY 1                                                         --(v1)

    IF SQLCA.SQLCODE = 0 THEN                                          --(v1)
        LET pos = pos + 1                                              --(v1)

        DECLARE cur_his12 CURSOR FOR                                   --(v1)
        SELECT A.fecha_registro,                                       --(v1)
               A.diag_proceso                                          --(v1) 
        FROM   pro_det_agte A                                          --(v1)
        WHERE  A.cod_promotor = reg_his.cod_promotor                   --(v1)
        AND    A.cve_afore_consar = 308                                --(v2)
        ORDER BY fecha_registro                                        --(v1)

        FOREACH cur_his12 INTO reg_his2[pos].fenvio,                   --(v1) 
                              reg_his2[pos].diag_proceso               --(v1)

            SELECT diagn_desc                                          --(v1)
            INTO   reg_his2[pos].diag_desc                             --(v1)
            FROM   tab_diagnos_pro                                     --(v1)
            WHERE  diagn_cod = reg_his2[pos].diag_proceso              --(v1) 

            LET reg_his2[pos].cod_proceso = 308                        --(v1)
            LET reg_his2[pos].desc_corta  = "R"                        --(v1)


            INSERT INTO temp_pro_25 VALUES( reg_his2[pos].cod_proceso, --(v1)
                                          reg_his2[pos].desc_corta,    --(v1) 
                                          reg_his2[pos].fenvio,        --(v1)
                                          reg_his2[pos].diag_proceso,  --(v1) 
                                          reg_his2[pos].diag_desc )    --(v1)

            LET pos = pos + 1
        END FOREACH                                                    --(v1)
    ELSE                                                               --(v1)
        LET pos = pos + 1                                              --(v1) 
    END IF                                                             --(v1)

   END FOREACH

   DECLARE cur_13 CURSOR FOR
      SELECT *                    
      FROM   temp_pro_25
      ORDER BY 3

   LET pos=1
   FOREACH cur_13 INTO  reg_his2[pos].cod_proceso,
                          reg_his2[pos].desc_corta,
                          reg_his2[pos].fenvio,                      
                          reg_his2[pos].diag_proceso,
                          reg_his2[pos].diag_desc
      LET pos = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      DISPLAY BY NAME vnombre,
                      reg_his.cod_promotor,
                      vnro_solicitud,
                      vfingre,
                      reg_his.fecha_registro,
                      reg_his.agenc_cod,
                      vdesc_agenc,
                      reg_his.nivel,
                      vdesc_nivel

      DISPLAY ARRAY reg_his2 TO scr_1.*
         ON KEY (INTERRUPT,CONTROL-C)
            DROP  TABLE temp_pro_25 
            EXIT DISPLAY
      END DISPLAY
      CLOSE WINDOW prom0257                          
      RETURN
   ELSE
      ERROR " REGISTRO INEXISTENTE" ATTRIBUTE(NORMAL)
      SLEEP 2
      ERROR ""
      CLOSE WINDOW prom0257
      RETURN
   END IF
   CLEAR SCREEN
END FUNCTION                                      

FUNCTION genera_rep_comer()
    DEFINE  rep_1  RECORD
             codven           CHAR(10),
             cod_promotor     CHAR(10),
             paterno          CHAR(40),
             materno          CHAR(40),
             nombres          CHAR(40),
             agenc_cod        CHAR(10),
             nivel            SMALLINT,
             fingre           DATE,
             fecha_registro   DATE,
             fecha_baja       DATE,
             diag_proceso     CHAR(02),
             motivo_suspende  CHAR(02)
            END RECORD,

            G_LISTA           CHAR(70),
            seg_mod   RECORD LIKE seg_modulo.*



   INITIALIZE rep_1.*, G_LISTA, seg_mod.* TO NULL
   SELECT * INTO seg_mod.* FROM seg_modulo
   WHERE  modulo_cod = "pro"

   OPEN WINDOW prom02512 AT 6,2 WITH FORM "PROM02512" ATTRIBUTE (BORDER)
   DISPLAY " (Ctrl-C) Salir " AT 1,1
   DISPLAY "                     GENERA REPORTE DE COMERCIAL ",
            "                              " AT 2,1 ATTRIBUTE(REVERSE)

        WHILE TRUE
            PROMPT " DESEA GENERAR EL REPORTE   S / N  ?" FOR enter
            IF enter MATCHES "[SsNn]" THEN
               EXIT WHILE
            END IF
        END WHILE

        IF enter MATCHES "[Nn]" THEN
           PROMPT "PROCESO CANCELADO...<ENTER> PARA CONTINUAR..." FOR enter
           RETURN
        END IF 

        ERROR "PROCESANDO INFORMACION..."
        SLEEP 2

        LET G_LISTA = seg_mod.ruta_envio CLIPPED,"/archivo_comercial.txt"

        START REPORT listado_2 TO G_LISTA 
        DECLARE apt_comer CURSOR FOR
             SELECT codven,cod_promotor, paterno, materno, 
                    nombres, agenc_cod, nivel,fingre, 
                    fecha_registro, fecha_baja, diag_proceso,
                    motivo_suspende
             from pro_mae_promotor
             order by 6
        FOREACH apt_comer INTO rep_1.*
             OUTPUT TO REPORT listado_2(rep_1.*)
        END FOREACH
        FINISH REPORT listado_2

        DISPLAY "ARCHIVO GENERADO EN :",G_LISTA CLIPPED  AT 16,1
        PROMPT "PROCESO FINALIZADO...<ENTER> PARA CONTINUAR..." FOR enter 
   CLOSE WINDOW prom02512
    
END FUNCTION                                      

REPORT listado_2(rep_1)
    DEFINE  rep_1  RECORD
             codven           CHAR(10),
             cod_promotor     CHAR(10),
             paterno          CHAR(40),
             materno          CHAR(40),
             nombres          CHAR(40),
             agenc_cod        CHAR(10),
             nivel            SMALLINT,
             fingre           DATE,
             fecha_registro   DATE,
             fecha_baja       DATE,
             diag_proceso     CHAR(02),
             motivo_suspende  CHAR(02)
            END RECORD

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
       PAGE HEADER
            PRINT COLUMN 01,"NOMINA",
                            "|",
                            "COD.PROMOTOR",
                            "|",
                            "PATERNO",
                            "|",
                            "MATERNO",
                            "|",
                            "NOMBRES",
                            "|",
                            "GPO.VENTA",
                            "|",
                            "TIPO PROM.",
                            "|",
                            "F.INGRESO.",
                            "|",
                            "F.REGISTRO",
                            "|",
                            "F.BAJA",
                            "|",
                            "DIAG.",
                            "|",
                            "MOTIVO SUSPENDE"
       
       ON EVERY ROW
          PRINT COLUMN 01, rep_1.codven           ,
                            "|",
                           rep_1.cod_promotor     ,
                            "|",
                           rep_1.paterno          ,
                            "|",
                           rep_1.materno          ,
                            "|",
                           rep_1.nombres          ,
                            "|",
                           rep_1.agenc_cod        ,
                            "|",
                           rep_1.nivel            ,
                            "|",
                           rep_1.fingre           ,
                            "|",
                           rep_1.fecha_registro   ,
                            "|",
                           rep_1.fecha_baja       ,
                            "|",
                           rep_1.diag_proceso     ,
                            "|",
                           rep_1.motivo_suspende  
END REPORT

FUNCTION genera_rep_maestro()
    DEFINE  rep_1  RECORD
               codven               char(10) ,
               seguro               char(11) ,
               nip                  integer  ,
               agenc_cod            char(10) ,
               unico                char(18) ,
               rfc                  char(13) ,
               paterno              char(40) ,
               materno              char(40) ,
               nombres              char(40) ,
               fingre               date     ,
               fenvio               date     ,
               fecha_registro       date     ,
               fecha_baja           date     ,
               calle                char(70) ,
               numero               char(10),
               dpto                 char(10),
               colonia              char(30),
               deleg                smallint,
               ciudad               smallint,
               estado               smallint,
               codpos               char(5) ,
               fono                 char(100),
               sup                  char(10),
               nivel                smallint,
               resuelva             smallint,
               fnaci                date    ,
               diag_proceso         char(2) ,
               nro_solicitud        integer ,
               fecha_certifi        date    ,
               motivo_suspende      char(2) ,
               fecha_suspende       date    ,
               cod_promotor         char(10)
            END RECORD,

            G_LISTA           CHAR(70),
            seg_mod   RECORD LIKE seg_modulo.*



   INITIALIZE rep_1.*, G_LISTA, seg_mod.* TO NULL
   SELECT * INTO seg_mod.* FROM seg_modulo
   WHERE  modulo_cod = "pro"

   OPEN WINDOW prom02512 AT 6,2 WITH FORM "PROM02512" ATTRIBUTE (BORDER)
   DISPLAY " (Ctrl-C) Salir " AT 1,1
   DISPLAY "                     GENERA REPORTE DEL MAESTRO  ",
            "                              " AT 2,1 ATTRIBUTE(REVERSE)

        WHILE TRUE
            PROMPT " DESEA GENERAR EL REPORTE   S / N  ?" FOR enter
            IF enter MATCHES "[SsNn]" THEN
               EXIT WHILE
            END IF
        END WHILE

        IF enter MATCHES "[Nn]" THEN
           PROMPT "PROCESO CANCELADO...<ENTER> PARA CONTINUAR..." FOR enter
           RETURN
        END IF 

        ERROR "PROCESANDO INFORMACION..."
        SLEEP 2

        LET G_LISTA = seg_mod.ruta_envio CLIPPED,
                      "/maestro_" CLIPPED,
                      HOY USING "DDMMYY" CLIPPED,
                      ".txt"

        START REPORT listado_maestro TO G_LISTA 
        DECLARE apt_maestro CURSOR FOR
            SELECT  codven, seguro, nip, agenc_cod , unico ,
                    rfc , paterno, materno, nombres, fingre,
                    fenvio, fecha_registro, fecha_baja, calle ,
                    numero, dpto, colonia, deleg, ciudad, estado,
                    codpos, fono, sup, nivel, resuelva, fnaci,
                    diag_proceso, nro_solicitud, fecha_certifi,
                    motivo_suspende, fecha_suspende, cod_promotor 
             from pro_mae_promotor
             order by 1
        FOREACH apt_maestro INTO rep_1.*
             OUTPUT TO REPORT listado_maestro(rep_1.*)
        END FOREACH
        FINISH REPORT listado_maestro

        DISPLAY "ARCHIVO GENERADO EN :",G_LISTA CLIPPED  AT 16,1
        PROMPT "PROCESO FINALIZADO...<ENTER> PARA CONTINUAR..." FOR enter 
   CLOSE WINDOW prom02512
    
END FUNCTION                                      

REPORT listado_maestro(rep_1)
    DEFINE  rep_1  RECORD
               codven               char(10) ,
               seguro               char(11) ,
               nip                  integer  ,
               agenc_cod            char(10) ,
               unico                char(18) ,
               rfc                  char(13) ,
               paterno              char(40) ,
               materno              char(40) ,
               nombres              char(40) ,
               fingre               date     ,
               fenvio               date     ,
               fecha_registro       date     ,
               fecha_baja           date     ,
               calle                char(70) ,
               numero               char(10),
               dpto                 char(10),
               colonia              char(30),
               deleg                smallint,
               ciudad               smallint,
               estado               smallint,
               codpos               char(5) ,
               fono                 char(100),
               sup                  char(10),
               nivel                smallint,
               resuelva             smallint,
               fnaci                date    ,
               diag_proceso         char(2) ,
               nro_solicitud        integer ,
               fecha_certifi        date    ,
               motivo_suspende      char(2) ,
               fecha_suspende       date    ,
               cod_promotor         char(10)
            END RECORD

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
       PAGE HEADER
            PRINT COLUMN 01,

               'NOMINA'              ,'|',
               'NSS'                 ,'|',
               'TIPO PROMOTOR'       ,'|',
               'GRUPO DE VENTA'      ,'|',
               'CURP'                ,'|',
               'RFC'                 ,'|',
               'PATERNO'             ,'|',
               'MATERNO'             ,'|',
               'NOMBRES'             ,'|',
               'FECHA INGRESO'       ,'|',
               'FECHA ENVIO'         ,'|',
               'FECHA REGISTRO'      ,'|',
               'FECHA BAJA'          ,'|',
               'CALLE'               ,'|',
               'NUMERO EXT.'         ,'|',
               'NUMERO INT.'         ,'|',
               'COLONIA '            ,'|',
               'DELEGACION'          ,'|',
               'CIUDAD'              ,'|',
               'ESTADO'              ,'|',
               'CODPOS'              ,'|',
               'TELEFONO'            ,'|',
               'SUPERVISOR'          ,'|',
               'NIVEL '              ,'|',
               'CALIFICACION'        ,'|',
               'FECHA NACIMIENTO'    ,'|',
               'DIAGNOSTICO'         ,'|',
               'NRO SOLICITUD'       ,'|',
               'FECHA CERTIFICA'     ,'|',
               'MOTIVO SUSPENDE'     ,'|',
               'FECHA SUSPENDE'      ,'|',
               'CODIGO PROMOTOR'

       ON EVERY ROW
          PRINT COLUMN 01, 
               rep_1.codven               ,'|',
               rep_1.seguro               ,'|',
               rep_1.nip                  ,'|',
               rep_1.agenc_cod            ,'|',
               rep_1.unico                ,'|',
               rep_1.rfc                  ,'|',
               rep_1.paterno              ,'|',
               rep_1.materno              ,'|',
               rep_1.nombres              ,'|',
               rep_1.fingre               ,'|',
               rep_1.fenvio               ,'|',
               rep_1.fecha_registro       ,'|',
               rep_1.fecha_baja           ,'|',
               rep_1.calle                ,'|',
               rep_1.numero               ,'|',
               rep_1.dpto                 ,'|',
               rep_1.colonia              ,'|',
               rep_1.deleg                ,'|',
               rep_1.ciudad               ,'|',
               rep_1.estado               ,'|',
               rep_1.codpos               ,'|',
               rep_1.fono                 ,'|',
               rep_1.sup                  ,'|',
               rep_1.nivel                ,'|',
               rep_1.resuelva             ,'|',
               rep_1.fnaci                ,'|',
               rep_1.diag_proceso         ,'|',
               rep_1.nro_solicitud        ,'|',
               rep_1.fecha_certifi        ,'|',
               rep_1.motivo_suspende      ,'|',
               rep_1.fecha_suspende       ,'|',
               rep_1.cod_promotor         
END REPORT


FUNCTION rfc_promotor(f_vrfc)
#rp-------------------------

   DEFINE arr_rfc ARRAY[100] OF RECORD #glo #arr_rfc
      cod_promotor LIKE pro_solicitud.cod_promotor
   END RECORD

   DEFINE
      f_vrfc   CHAR(10)

   DEFINE
      pos      SMALLINT

   DECLARE cur_rfc CURSOR FOR
   SELECT  unique  cod_promotor
   FROM    pro_solicitud
   WHERE   rfc[1,10] = f_vrfc

   LET pos = 1

   FOREACH cur_rfc INTO arr_rfc[pos].*
      LET pos = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      LET ventro = 1

      OPEN WINDOW vent_rfc AT 9,19 WITH FORM "PROM0033" ATTRIBUTE( BORDER)
      DISPLAY "             <Ctrl-c> Salir                     "
              AT 1,1 ATTRIBUTE(REVERSE)

      DISPLAY ARRAY arr_rfc TO scr_1.*
         ON KEY (INTERRUPT)
            EXIT DISPLAY
         ON KEY (control-c)
            EXIT DISPLAY
      END DISPLAY
      CLOSE WINDOW vent_rfc

   END IF

END FUNCTION
