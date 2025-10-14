#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIM011  => MANTENIMIENTO DE AFILIADOS (SOLO DATOS DE PROCESAR)   #
#Fecha actualiz    => 19 DE MAYO DE 2000.                                   #
#Por               => MAURO MUNIZ CABALLERO, modificados(historico)         #
#Fecha modifica    => 04 de diciembre de 2001.                              #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ.                      #
#Sistema           => AFI.                                                  #
#Fecha modifica    => 28 de junio de 2005.                                  #
#Modificado por    => FERNANDO HERRERA HERNANDEZ (CURP)                     #
#Sistema           => AFI.                                                  #
#Modifico          => FERNANDO HERRERA HERNANDEZ.                           #
#Fecha             => 04 DE AGOSTRO DEL 2005 (TRABAJADORES INDEPENDIENTES)  #
#                  => 05 ENE 2009 MODIFICACION CURP                         #
#############################################################################

DATABASE safre_af

GLOBALS

   DEFINE
     sw_carta            ,
     sw_cod              ,
     sw_fol              ,
     sw_1                ,
     digito              SMALLINT

   DEFINE
     c_pat               CHAR(1),
     c_mat               CHAR(1),
     c_nom               CHAR(1),
     c_fen               CHAR(1),
     c_doc               CHAR(1),
     enter               CHAR(1),
     aaa                 ,
     mm                  ,
     dd                  CHAR(2),
     z_fecha             CHAR(10),
     xx_fecha            CHAR(10)

   DEFINE
     j_fecha             DATE,
     HOY                 DATE

   DEFINE
     aux_pausa           CHAR(1),
     ACCION              CHAR(1),
     pat                 CHAR(40),
     mat                 CHAR(40),
     nom                 CHAR(40),
     x_fecha             CHAR(10),
     comando             CHAR(250),
     param               CHAR(193)

   DEFINE g_master       RECORD
     codven              LIKE afi_mae_afiliado.codven,
     nip                 LIKE pro_mae_promotor.nip,
     desc_codven         CHAR(60),
     agenc_cod           CHAR(10),
     agenc_desc          CHAR(60),
     tipo_solicitud      LIKE afi_mae_afiliado.tipo_solicitud,
     fecha_elaboracion   LIKE afi_mae_afiliado.fecha_elaboracion,
     folio_edo_cta       CHAR(8),
     cod_afore_ced       SMALLINT,
     desc_afore          CHAR(50),
     fecha_emision       DATE,
     n_folio             LIKE afi_mae_afiliado.n_folio,
     cod_operacion       SMALLINT,
     desc_cod_oper       CHAR(15),
     frecafor            LIKE afi_mae_afiliado.frecafor,
     paterno             CHAR(40),
     materno             CHAR(40),
     nombres             CHAR(40),
     n_seguro            LIKE afi_mae_afiliado.n_seguro,
     n_rfc               LIKE afi_mae_afiliado.n_rfc,
     n_unico             LIKE afi_mae_afiliado.n_unico,
     sexo                LIKE afi_mae_afiliado.sexo,
     desc_sexo           CHAR(60),
     edo_civil           SMALLINT,
     desc_edo_civil      CHAR(60),
     fena                LIKE afi_mae_afiliado.fena,
     salario_base_comis  LIKE afi_mae_afiliado.salario_base_comis,
     cod_esq_comision    LIKE afi_mae_afiliado.cod_esq_comision,
     desc_esq_comision   CHAR(50),   
     estadon             LIKE afi_mae_afiliado.estadon,
     desc_estadon        CHAR(60),
     nacionalidad        LIKE afi_mae_afiliado.nacionalidad,
     desc_nacionalidad   CHAR(60),
     tip_prob	         LIKE afi_mae_afiliado.tip_prob,
     fol_prob            LIKE afi_mae_afiliado.fol_prob,
     doc_prob            LIKE afi_mae_afiliado.doc_prob,
     docprob_desc        LIKE tab_doc_prob.docprob_desc,
     ind_infonavit       LIKE afi_mae_afiliado.ind_infonavit,
     desc_ind_info       CHAR(25),
     cod_error_origen    LIKE afi_mae_afiliado.cod_error_origen,
     const_curp          SMALLINT,
     tipo_trab_ind       CHAR(02),
     desc_tipo_trab_ind  CHAR(60),
     fecha_activ         DATE            -- CPL-3165 
   END RECORD

   DEFINE g_master1      RECORD
     codven              LIKE afi_mae_afiliado.codven,
     nip                 LIKE pro_mae_promotor.nip,
     desc_codven         CHAR(60),
     agenc_cod           CHAR(10),
     agenc_desc          CHAR(60),
     tipo_solicitud      LIKE afi_mae_afiliado.tipo_solicitud,
     fecha_elaboracion   LIKE afi_mae_afiliado.fecha_elaboracion,
     folio_edo_cta       CHAR(8),
     cod_afore_ced       SMALLINT,
     desc_afore          CHAR(50),
     fecha_emision       DATE,
     n_folio             LIKE afi_mae_afiliado.n_folio,
     cod_operacion       SMALLINT,
     desc_cod_oper       CHAR(15),
     frecafor            LIKE afi_mae_afiliado.frecafor,
     paterno             CHAR(40),
     materno             CHAR(40),
     nombres             CHAR(40),
     n_seguro            LIKE afi_mae_afiliado.n_seguro,
     n_rfc               LIKE afi_mae_afiliado.n_rfc,
     n_unico             LIKE afi_mae_afiliado.n_unico,
     sexo                LIKE afi_mae_afiliado.sexo,
     desc_sexo           CHAR(60),
     edo_civil           SMALLINT,
     desc_edo_civil      CHAR(60),
     fena                LIKE afi_mae_afiliado.fena,
     salario_base_comis  LIKE afi_mae_afiliado.salario_base_comis,
     cod_esq_comision    LIKE afi_mae_afiliado.cod_esq_comision,
     desc_esq_comision   CHAR(50),   
     estadon             LIKE afi_mae_afiliado.estadon,
     desc_estadon        CHAR(60),
     nacionalidad        LIKE afi_mae_afiliado.nacionalidad,
     desc_nacionalidad   CHAR(60),
     tip_prob	         LIKE afi_mae_afiliado.tip_prob,
     fol_prob            LIKE afi_mae_afiliado.fol_prob,
     doc_prob            LIKE afi_mae_afiliado.doc_prob,
     docprob_desc        LIKE tab_doc_prob.docprob_desc,
     ind_infonavit       LIKE afi_mae_afiliado.ind_infonavit,
     desc_ind_info       CHAR(25),
     cod_error_origen    LIKE afi_mae_afiliado.cod_error_origen,
     const_curp          SMALLINT,
     tipo_trab_ind       CHAR(02),
     desc_tipo_trab_ind  CHAR(60),
     fecha_activ         DATE            -- CPL-3165 
   END RECORD

   DEFINE g_master2      ARRAY [100] OF RECORD 
     codven              LIKE afi_mae_afiliado.codven,
     nip                 LIKE pro_mae_promotor.nip,
     desc_codven         CHAR(60),
     agenc_cod           CHAR(10),
     agenc_desc          CHAR(60),
     tipo_solicitud      LIKE afi_mae_afiliado.tipo_solicitud,
     fecha_modifica      DATE,
     fecha_elaboracion   LIKE afi_mae_afiliado.fecha_elaboracion,
     folio_edo_cta       CHAR(8),
     cod_afore_ced       SMALLINT,
     desc_afore          CHAR(50),
     fecha_emision       DATE,
     n_folio             LIKE afi_mae_afiliado.n_folio,
     cod_operacion	 SMALLINT,
     desc_cod_oper	 CHAR(15),
     frecafor            LIKE afi_mae_afiliado.frecafor,
     paterno             CHAR(40),
     materno             CHAR(40),
     nombres             CHAR(40),
     n_seguro            LIKE afi_mae_afiliado.n_seguro,
     n_rfc               LIKE afi_mae_afiliado.n_rfc,
     n_unico             LIKE afi_mae_afiliado.n_unico,
     sexo                LIKE afi_mae_afiliado.sexo,
     desc_sexo           CHAR(60),
     edo_civil           SMALLINT,
     desc_edo_civil      CHAR(60),
     fena                LIKE afi_mae_afiliado.fena,
     salario_base_comis  LIKE afi_mae_afiliado.salario_base_comis,
     cod_esq_comision    LIKE afi_mae_afiliado.cod_esq_comision,
     desc_esq_comision   CHAR(50),   
     estadon             LIKE afi_mae_afiliado.estadon,
     desc_estadon        CHAR(60),
     nacionalidad        LIKE afi_mae_afiliado.nacionalidad,
     desc_nacionalidad   CHAR(60),
     tip_prob            LIKE afi_mae_afiliado.tip_prob,
     docprob_desc        LIKE tab_doc_prob.docprob_desc,
     fol_prob            LIKE afi_mae_afiliado.fol_prob,
     doc_prob            LIKE afi_mae_afiliado.doc_prob,
     ind_infonavit       LIKE afi_mae_afiliado.ind_infonavit,
     desc_ind_info       CHAR(25),
     cod_error_origen    LIKE afi_mae_afiliado.cod_error_origen,
     const_curp          SMALLINT,
     folio_nvo           DECIMAL(8,0)
   END RECORD

   DEFINE g_afore RECORD LIKE tab_afore_local.*
   DEFINE r_cod   RECORD LIKE tab_supervisor.*

   DEFINE
     cod_error           CHAR(4),
     g_usuario           CHAR(8),
     g_hora              CHAR(8),
     COMMA               CHAR(700)

   DEFINE
     g_status_captura    ,
     HAY_DATOS1          ,
     HAY_DATOS2          ,
     tot_error           ,
     paterno_2           ,
     materno_3           ,
     nombre_4            ,
     sexo_5              ,
     macim_6             ,
     entidad_7           ,
     nacion_8            ,
     cve_prob_9          ,
     doc_prob_10         ,
     fol_prob_11         ,
     cve_asig_12         ,
     su_estatus          ,
     reclamo             ,
     pos                 ,
     vstatmod            ,
     vstatusint          SMALLINT,
     vcont               INTEGER,
     vfolio_nvo          DECIMAL(8,0)

   DEFINE
     opc                 CHAR(1)  ,
     vnss                CHAR(11) ,
     vmarca_entra        SMALLINT ,
     vmarca_estado       SMALLINT ,
     vcodigo_rechazo     SMALLINT ,
     ejecuta             CHAR(300),
     xcodigo_marca       SMALLINT ,
     xcodigo_rechazo     SMALLINT ,
     edo_proc            SMALLINT ,
     su_st_int           SMALLINT ,
     operacion           CHAR(40)

   DEFINE
     dcto_2	         CHAR(1),
     dcto_3	         CHAR(9),
     dcto_4	         CHAR(7),
     dcto_7	         CHAR(7)

   DEFINE
     r_nac 	         SMALLINT,
     a_nac 	         SMALLINT,
     b_nac 	         SMALLINT

   DEFINE
     xn_fena             CHAR(10)

   DEFINE 
     f_reclama           INTEGER,
     id_reclama          CHAR(1),
     vlongitud           SMALLINT,
     llama_val           SMALLINT,
     vtipo_sol           SMALLINT,
     comm_arg            CHAR(500),
     KEY                 INTEGER,
     pasa_curp           SMALLINT,
     pasa                SMALLINT,
     dig_curp            SMALLINT,
     desc_err            CHAR(60),
     cve_arma            CHAR(10),
     vdesc_solic         CHAR(12)

--->05 ene 2009 MOD CURP
   DEFINE cadena     CHAR(17)
   DEFINE cadena2    CHAR(17)
   DEFINE cadena3    CHAR(17)
   DEFINE ban_cadena SMALLINT
   DEFINE r          SMALLINT

   DEFINE g_compara RECORD
          paterno LIKE afi_mae_afiliado.paterno,
          materno LIKE afi_mae_afiliado.materno,
          nombres LIKE afi_mae_afiliado.nombres,
          sexo    LIKE afi_mae_afiliado.sexo,
          fena    LIKE afi_mae_afiliado.fena,
          estadon LIKE afi_mae_afiliado.estadon,
          nacionalidad LIKE afi_mae_afiliado.nacionalidad,
          tip_prob LIKE afi_mae_afiliado.tip_prob,
          doc_prob LIKE afi_mae_afiliado.doc_prob,
          fol_prob LIKE afi_mae_afiliado.fol_prob
   END RECORD

END GLOBALS

MAIN

   LET g_master.n_seguro = ARG_VAL(1)
   LET g_master.n_folio  = ARG_VAL(2)
   LET vtipo_sol         = ARG_VAL(3)
   LET ACCION            = ARG_VAL(4)
   LET reclamo           = ARG_VAL(5)
   LET llama_val         = ARG_VAL(6)
   LET f_reclama         = ARG_VAL(7)
   LET id_reclama        = ARG_VAL(8)

   IF g_master.n_seguro = 0 THEN
      LET g_master.n_seguro = NULL
   END IF

   IF g_master.n_folio = 0 THEN
      LET g_master.n_folio  = NULL
   END IF

   IF reclamo = 0 THEN
      LET reclamo = NULL
   END IF

   IF f_reclama = 0 THEN
      LET f_reclama = NULL
   END IF

   IF id_reclama = 0 THEN
      LET id_reclama = NULL
   END IF

   IF llama_val IS NULL OR
      llama_val <> 52 THEN
      LET llama_val = 0
   END IF

   DEFER INTERRUPT
   OPTIONS PROMPT LINE LAST,
     INPUT WRAP,
     ACCEPT KEY CONTROL-I,
     COMMENT LINE LAST

   CALL STARTLOG("AFIM011.log")
   CALL inicio()
   CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

   SELECT *, @USER 
   INTO   g_afore.*,g_usuario 
   FROM   tab_afore_local

   LET HOY = TODAY

   LET edo_proc        = 600
   LET vmarca_estado   = 0
   LET vcodigo_rechazo = 0

END FUNCTION

FUNCTION valor_argumento()
#va-----------------------

  LET comm_arg = g_master.n_seguro, " ",
                 g_master.n_folio, " ",
                 g_master.tipo_solicitud, " ",
                 ACCION, " ",
                 reclamo, " ",
                 llama_val, " ",
                 f_reclama, " ",
                 id_reclama CLIPPED

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------
  DEFINE bnd_mod CHAR(1)

  OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIM0111" ATTRIBUTE(BORDER)
  DISPLAY " AFIM011                 MANTENIMIENTO  AFILIADOS                              " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY "                                                                               " AT 6,1 ATTRIBUTE(REVERSE)
  DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
  DISPLAY "               Nombre del Trabajador segun Documento Probatorio                " AT 10,1 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

  IF NOT llama_val THEN
     MENU "AFILIADOS"
       COMMAND "Consulta" "Consulta de Afiliados"
          LET ACCION = "C"
          CALL Consulta()
          CALL Inicializa()
       COMMAND "Modifica" "Modificacion Datos"
          LET ACCION = "M"
          WHILE TRUE
             PROMPT "(A) Cert, (B)Cert No Afil (C)No Cert, (D) Salir : "
             ATTRIBUTES (REVERSE) FOR bnd_mod
             IF bnd_mod MATCHES'[AaBbCcDd]' THEN
                IF bnd_mod MATCHES '[Aa]' THEN
                   CALL Modifica()
                   CALL Inicializa()
                END IF
                IF bnd_mod MATCHES '[Bb]' THEN
                   CALL Modifica_No_Afil()
                   CALL Inicializa()
                END IF
                IF bnd_mod MATCHES '[Cc]' THEN
                   CALL Modifica_no_cert()
                   CALL Inicializa()
                END IF
                IF bnd_mod MATCHES '[Dd]' THEN
                   EXIT WHILE
                   RETURN
                END IF
             ELSE
                ERROR "Solo debe presionar (A)Cert, (B)Cert No Afil ",
                      "(C)No cert, (D)Salir "
                SLEEP 1
                ERROR ""
             END IF
          END WHILE
       COMMAND "Otros datos" "Otros datos de Afiliados"
          LET ACCION = "M"
          CALL Consulta()
          CALL Inicializa()
--->05 ene 2009 MOD CURP
{       COMMAND "Folio" "Cambio de folio de solicitud"
          LET ACCION = "F"
          CALL Consulta()
          CALL Inicializa()}
---<
       COMMAND "Despliega" "Despliegue de Afiliados"
          LET ACCION = "D"
          CALL Inicializa()
          CALL Despliega() 
       COMMAND "Salir" "Salir de Programa"
          EXIT MENU
     END MENU
  END IF

  IF llama_val = 52 THEN
     MENU "AFILIADOS"
       COMMAND "Consulta" "Consulta de Afiliados"
          LET ACCION = "C"
          CALL Consulta()
          CALL Inicializa()
       COMMAND "Despliega" "Despliegue de Afiliados"
          LET ACCION = "D"
          CALL Inicializa()
          CALL Despliega()
       COMMAND "Salir" "Salir de Programa"
          EXIT MENU
     END MENU
  END IF

END FUNCTION

FUNCTION Inicializa()

  LET vstatmod = 0
  INITIALIZE g_master.* TO NULL
  DISPLAY "                                                                               " AT 6,1 ATTRIBUTE(REVERSE)

  CLEAR FORM

END FUNCTION

FUNCTION Consulta()
#C-----------------

  DEFINE tot_afil  SMALLINT
  DEFINE vn_folio  DECIMAL(10,0)

  IF reclamo IS NOT NULL THEN
     LET g_master.n_seguro = ARG_VAL(1)
     LET g_master.n_folio  = ARG_VAL(2)
  END IF

  DISPLAY "" AT 1,1
  DISPLAY "" AT 2,1

  CASE ACCION
    WHEN 'C' DISPLAY " CONSULTA " AT 1,69 ATTRIBUTE(REVERSE)
    WHEN 'M' DISPLAY " CONSULTA " AT 1,69 ATTRIBUTE(REVERSE)
    WHEN 'F' DISPLAY " MOD. FOL " AT 1,69 ATTRIBUTE(REVERSE)
  END CASE

  DISPLAY "CTRL:[E]Dom [T]Tel [B]Ben [V]Pat [N]Sie [P]Mod [Y]Tip Admo           " AT 1,1 ATTRIBUTE(CYAN)
  DISPLAY "CTRL:[O]Rech PROCESAR [U]Disp RENAPO [F]Not IMSS [G]Despl [W]Marca [C]Salir    " AT 2,1 ATTRIBUTE(CYAN)

  INITIALIZE g_master.tipo_solicitud TO NULL         -- CPL-3165
  IF g_master.n_seguro IS NULL THEN
     CALL Inicializa()
     -- INITIALIZE g_master.tipo_solicitud TO NULL 
  END IF

  INPUT BY NAME g_master.tipo_solicitud, 
                g_master.n_folio,
                g_master.n_seguro,
                g_master.n_unico  WITHOUT DEFAULTS

    AFTER FIELD tipo_solicitud
    
      -----------
      -- CPL-3165
      IF (g_master.tipo_solicitud IS NOT NULL) AND 
         (g_master.tipo_solicitud =  1 OR g_master.tipo_solicitud =  2 OR g_master.tipo_solicitud =  3 OR  
          g_master.tipo_solicitud =  4 OR g_master.tipo_solicitud =  5 OR g_master.tipo_solicitud =  6 OR  
          g_master.tipo_solicitud =  7 OR g_master.tipo_solicitud =  8 OR g_master.tipo_solicitud =  9 OR  
          g_master.tipo_solicitud = 32 OR g_master.tipo_solicitud = 33 OR g_master.tipo_solicitud = 34) THEN   
          -- ACEPTAR EL TIPO DE SOLICITUD
      ELSE    
         NEXT FIELD n_seguro
      END IF

      --IF (g_master.tipo_solicitud IS NULL) OR            
      --    (g_master.tipo_solicitud <  1 OR g_master.tipo_solicitud > 29 ) THEN 
      --   NEXT FIELD n_seguro
      --END IF
      -- CPL-3165
      -----------

      CALL despliega_desc_solic(g_master.tipo_solicitud)

      IF ACCION = 'M'                AND
         g_master.tipo_solicitud = 8 THEN
         ERROR "No se pueden modificar datos para este tipo de solicitud."
         NEXT FIELD tipo_solicitud
      END IF

    AFTER FIELD n_folio
      IF g_master.n_folio IS NULL THEN
         NEXT FIELD n_seguro
      ELSE
         -- DISPLAY "AQUI ",g_master.n_folio," - ",g_master.tipo_solicitud 
         IF NOT Rescata_datos("N",
                              g_master.n_folio,
                              g_master.tipo_solicitud,
                              "") THEN
            ERROR "Afiliado NO existe"
            NEXT FIELD n_folio
         END IF

         DISPLAY BY NAME g_master.*

         IF ACCION = 'F' THEN
            CALL Modifica_folio()
         END IF
      END IF

    AFTER FIELD n_seguro
      IF g_master.n_seguro IS NULL THEN
         NEXT FIELD n_unico
      ELSE
         IF NOT Rescata_datos("C",g_master.n_seguro,0,"") THEN
            ERROR "Afiliado NO existe"
            NEXT FIELD n_unico
         END IF

         DISPLAY BY NAME g_master.*
      END IF

      IF ACCION = 'M' AND 
         g_master.tipo_solicitud = 8 THEN
         ERROR "No se pueden modificar datos para este tipo de solicitud."
         NEXT FIELD tipo_solicitud
      END IF

    AFTER FIELD n_unico
      IF g_master.n_unico IS NULL THEN
         NEXT FIELD n_folio
      ELSE
         SELECT COUNT(*)
         INTO   tot_afil
         FROM   afi_mae_afiliado a
         WHERE  a.n_unico = g_master.n_unico

         IF tot_afil > 1 THEN
            CALL despliega_curp()

            IF NOT Rescata_datos("N",
                                 g_master.n_folio,
                                 g_master.tipo_solicitud,
                                 "") THEN
               ERROR "Afiliado NO existe"
               NEXT FIELD n_folio
            END IF

            DISPLAY BY NAME g_master.*
         ELSE
            IF NOT Rescata_datos("U",g_master.n_seguro,0,g_master.n_unico) THEN
               ERROR "Afiliado NO existe"
               NEXT FIELD n_unico
            END IF

            DISPLAY BY NAME g_master.*
         END IF
      END IF

      IF ACCION = 'M'                AND
         g_master.tipo_solicitud = 8 THEN
         ERROR "No se pueden modificar datos para este tipo de solicitud."
         NEXT FIELD tipo_solicitud
      END IF

    ON KEY ( CONTROL-O )
       CALL motivo_rechazo()

    ON KEY ( CONTROL-F )
       CALL motivo_notifica()

    ON KEY ( CONTROL-U )
       CALL motivo_renapo()

    ON KEY ( CONTROL-P )
       SELECT "X"
       FROM   afi_mae_modifica md
       WHERE  md.n_seguro = g_master.n_seguro
       AND    md.n_folio = g_master.n_folio
       AND    md.tipo_solicitud = g_master.tipo_solicitud
       GROUP BY 1

       IF SQLCA.SQLCODE = 0 THEN
          CALL verifica_datos_modificados()
       ELSE
          PROMPT "Registro sin modificaciones, [Enter] p/continuar " 
          ATTRIBUTES (REVERSE) FOR enter
       END IF

    ON KEY ( CONTROL-G )
       CALL Despliega()
       DISPLAY BY NAME g_master.n_folio

    ON KEY ( CONTROL-Y )
       IF g_master.tipo_solicitud = 8 THEN
          CALL fctipo_admon(g_master.n_unico)
       END IF

    ON KEY ( INTERRUPT )
       CALL Inicializa()
       DISPLAY "                                                                               " AT 6,1 ATTRIBUTE(REVERSE)
       DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
       DISPLAY "                  " AT 1,69 
       EXIT INPUT

    ON KEY ( CONTROL-E, CONTROL-T, CONTROL-B, CONTROL-V, CONTROL-N )
       IF g_master.n_seguro IS NULL THEN
          ERROR "Campo NO puede ser NULO"
          NEXT FIELD n_seguro
       END IF

       CALL valor_argumento()

       INITIALIZE COMMA TO NULL

       LET KEY = FGL_LASTKEY()

       CASE KEY
         WHEN 2
           LET COMMA = "fglgo AFIM014 "
         WHEN 5
           LET COMMA = "fglgo AFIM012 "
         WHEN 14 
           LET COMMA = "cd /safre/cta/exp/; fglgo CTAC102 ", g_master.n_seguro
         WHEN 20
           LET COMMA = "fglgo AFIM013 "
         WHEN 22
           LET COMMA = "fglgo AFIM015 "
       END CASE

       LET COMMA = COMMA CLIPPED," ", comm_arg CLIPPED
       RUN COMMA 

    ON KEY ( CONTROL-W )
       IF g_master.n_seguro IS NULL OR
          g_master.n_seguro MATCHES ' *' THEN
          ERROR "Campo NO puede ser NULO"
          NEXT FIELD n_seguro
       END IF

       INITIALIZE COMMA TO NULL
       LET COMMA = "cd /safre/tab/exp;fglgo TABM0601.4gi ", g_master.n_seguro
       RUN COMMA 

  END INPUT

END FUNCTION

FUNCTION Rescata_datos(l_aux_val,x_valor,x_tipo,x_unico)
#rd---------------------------------------------
  DEFINE l_aux_val CHAR(1)
  DEFINE x_valor   CHAR(11)
  DEFINE x_tipo    SMALLINT
  DEFINE xx_num    DECIMAL(11,0)
  DEFINE x_unico   CHAR(18)
  DEFINE lc_comando CHAR(2000)   -- CPL-3165 

  CASE l_aux_val
    WHEN "C"
      SELECT cod_promotor,
             agenc_cod,
             n_folio,
             frecafor,
             paterno,
             materno,
             nombres,
             n_seguro,
             n_unico,
             n_rfc,
             sexo,
             edo_civil,
             fena,
             salario_base_comis,
             cod_esq_comision,
             estadon,
             status_interno,
             tip_prob,
             fol_prob,
             doc_prob,
             ind_infonavit,
             nacionalidad,
             tipo_solicitud,
             fecha_elaboracion,
             ' ', --cod_error_origen,
             folio_edo_cta,
             cod_afore_ced,
             const_curp,
             femision
      INTO   g_master.codven,
             g_master.agenc_cod,
             g_master.n_folio,
             g_master.frecafor,
             g_master.paterno,
             g_master.materno,
             g_master.nombres,
             g_master.n_seguro,
             g_master.n_unico,
             g_master.n_rfc,
             g_master.sexo,
             g_master.edo_civil,
             g_master.fena,
             g_master.salario_base_comis,
             g_master.cod_esq_comision,
             g_master.estadon,
             su_estatus,
             g_master.tip_prob,
             g_master.fol_prob,
             g_master.doc_prob,
             g_master.ind_infonavit,
             g_master.nacionalidad,
             g_master.tipo_solicitud,
             g_master.fecha_elaboracion,
             g_master.cod_error_origen,
             g_master.folio_edo_cta,
             g_master.cod_afore_ced,
             g_master.const_curp,
             g_master.fecha_emision
      FROM   afi_mae_afiliado
      WHERE  n_seguro =  x_valor
      IF SQLCA.SQLCODE <> 0 THEN
         RETURN FALSE
      END IF

    WHEN "N"
      LET xx_num = x_valor
      SELECT cod_promotor,
             agenc_cod,
             n_folio,
             frecafor,
             paterno,
             materno,
             nombres,
             n_seguro,
             n_unico,
             n_rfc,
             sexo,
             edo_civil,
             fena,
             salario_base_comis,
             cod_esq_comision,
             estadon,
             status_interno,
             tip_prob,
             fol_prob,
             doc_prob,
             ind_infonavit,
             nacionalidad,
             tipo_solicitud,
             fecha_elaboracion,
             ' ',  --cod_error_origen,
             folio_edo_cta,
             cod_afore_ced,
             const_curp,
             femision
      INTO   g_master.codven,
             g_master.agenc_cod,
             g_master.n_folio,
             g_master.frecafor,
             g_master.paterno,
             g_master.materno,
             g_master.nombres,
             g_master.n_seguro,
             g_master.n_unico,
             g_master.n_rfc,
             g_master.sexo,
             g_master.edo_civil,
             g_master.fena,
             g_master.salario_base_comis,
             g_master.cod_esq_comision,
             g_master.estadon,
             su_estatus,
             g_master.tip_prob,
             g_master.fol_prob,
             g_master.doc_prob,
             g_master.ind_infonavit,
             g_master.nacionalidad,
             g_master.tipo_solicitud,
             g_master.fecha_elaboracion,
             g_master.cod_error_origen,
             g_master.folio_edo_cta,
             g_master.cod_afore_ced,
             g_master.const_curp,
             g_master.fecha_emision
      FROM   afi_mae_afiliado
      WHERE  n_folio        = xx_num
      AND    tipo_solicitud = g_master.tipo_solicitud
      IF SQLCA.SQLCODE <> 0 THEN
         RETURN FALSE
      END IF

    WHEN "U"
      SELECT cod_promotor,
             agenc_cod,
             n_folio,
             frecafor,
             paterno,
             materno,
             nombres,
             n_seguro,
             n_unico,
             n_rfc,
             sexo,
             edo_civil,
             fena,
             salario_base_comis,
             cod_esq_comision,
             estadon,
             status_interno,
             tip_prob,
             fol_prob,
             doc_prob,
             ind_infonavit,
             nacionalidad,
             tipo_solicitud,
             fecha_elaboracion,
             ' ',  --cod_error_origen,
             folio_edo_cta,
             cod_afore_ced,
             const_curp,
             femision
      INTO   g_master.codven,
             g_master.agenc_cod,
             g_master.n_folio,
             g_master.frecafor,
             g_master.paterno,
             g_master.materno,
             g_master.nombres,
             g_master.n_seguro,
             g_master.n_unico,
             g_master.n_rfc,
             g_master.sexo,
             g_master.edo_civil,
             g_master.fena,
             g_master.salario_base_comis,
             g_master.cod_esq_comision,
             g_master.estadon,
             su_estatus,
             g_master.tip_prob,
             g_master.fol_prob,
             g_master.doc_prob,
             g_master.ind_infonavit,
             g_master.nacionalidad,
             g_master.tipo_solicitud,
             g_master.fecha_elaboracion,
             g_master.cod_error_origen,
             g_master.folio_edo_cta,
             g_master.cod_afore_ced,
             g_master.const_curp,
             g_master.fecha_emision
      FROM   afi_mae_afiliado
      WHERE  n_unico = g_master.n_unico
      IF SQLCA.SQLCODE <> 0 THEN
         RETURN FALSE
      END IF
  END CASE

  LET g_master1.* = g_master.*

  IF ACCION = 'M' THEN
     SELECT 'X'
     FROM   afi_mae_modifica
     WHERE  n_seguro       = g_master.n_seguro
     AND    cod_operacion  = 0
     AND    status_interno = 120
     IF SQLCA.SQLCODE = 0 THEN
        SELECT n_folio,
               frecafor,
               paterno,
               materno,
               nombres,
               n_seguro,
               n_unico,
               n_rfc,
               sexo,
               edo_civil,
               fena,
               salario_base_comis,
               nicho,
               estadon,
               status_interno,
               tip_prob,
               fol_prob,
               doc_prob,
               ind_infonavit,
               nacionalidad,
               tipo_solicitud,
               fecha_elaboracion,
               cod_error_origen,
               folio_edo_cta,
               cod_afore_ced,
               const_curp,
               folio_nvo
        INTO   g_master.n_folio,
               g_master.frecafor,
               g_master.paterno,
               g_master.materno,
               g_master.nombres,
               g_master.n_seguro,
               g_master.n_unico,
               g_master.n_rfc,
               g_master.sexo,
               g_master.edo_civil,
               g_master.fena,
               g_master.salario_base_comis,
               g_master.cod_esq_comision,
               g_master.estadon,
               su_estatus,
               g_master.tip_prob,
               g_master.fol_prob,
               g_master.doc_prob,
               g_master.ind_infonavit,
               g_master.nacionalidad,
               g_master.tipo_solicitud,
               g_master.fecha_elaboracion,
               g_master.cod_error_origen,
               g_master.folio_edo_cta,
               g_master.cod_afore_ced,
               g_master.const_curp,
               vfolio_nvo
        FROM   afi_mae_modifica
        WHERE  n_seguro =  g_master.n_seguro
        AND    cod_operacion  = 0
        AND    status_interno = 120
     ELSE
        LET vfolio_nvo = g_master.n_folio
     END IF
  END IF

  LET vstatusint = su_estatus

  IF g_master.const_curp IS NOT NULL AND
     g_master.const_curp <> " "      AND
     g_master.const_curp <> 1        AND
     g_master.const_curp <> 2        THEN
     LET g_master.const_curp = 1
  END IF

  SELECT a.tipo_trab_ind, b.desc_tipo_trab_ind
    INTO g_master.tipo_trab_ind, g_master.desc_tipo_trab_ind
    FROM cta_ctr_reg_ind a, tab_tipo_trab_ind b
   WHERE a.nti           = g_master.n_seguro
     AND a.curp          = g_master.n_unico
     AND a.tipo_trab_ind = b.tipo_trab_ind

  SELECT @descripcion
    INTO g_master.desc_ind_info
    FROM safre_af:tab_ind_cred
   WHERE @codigo = g_master.ind_infonavit

  SELECT paterno,
         materno,
         nombres,
         nip
  INTO   pat,
         mat,
         nom,
         g_master.nip
  FROM   pro_mae_promotor
  WHERE  cod_promotor = g_master.codven
  IF SQLCA.SQLCODE <> 0 THEN
     LET g_master.desc_codven = "NO EXISTE"
  ELSE
     LET g_master.desc_codven = pat CLIPPED," ",
                                mat CLIPPED," ",
                                nom CLIPPED
  END IF

  SELECT nombre_uni_n1 
  INTO   g_master.agenc_desc 
  FROM   com_nivel1
  WHERE  coduni_n1 = g_master.agenc_cod
  IF SQLCA.SQLCODE <> 0 THEN
     LET g_master.agenc_desc = "NO EXISTE"
  END IF

  SELECT ecivi_desc
  INTO   g_master.desc_edo_civil
  FROM   tab_edo_civil
  WHERE  ecivi_cod = g_master.edo_civil
  IF SQLCA.SQLCODE <> 0 THEN
     LET g_master.desc_edo_civil = "NO EXISTE"
  END IF

  SELECT sexo_desc
  INTO   g_master.desc_sexo
  FROM   tab_sexo
  WHERE  sexo_cod = g_master.sexo
  IF SQLCA.SQLCODE <> 0 THEN
     LET g_master.desc_sexo = "NO EXISTE"
  END IF

  SELECT estad_desc
  INTO   g_master.desc_estadon
  FROM   tab_estado
  WHERE  estad_cod = g_master.estadon
  IF STATUS = NOTFOUND THEN
     LET g_master.desc_estadon = "NO EXISTE"
  END IF

  SELECT pais_desc 
  INTO   g_master.desc_nacionalidad
  FROM   tab_pais
  WHERE  pais_cod = g_master.nacionalidad
  IF STATUS = NOTFOUND THEN
     LET g_master.desc_nacionalidad = "NO EXISTE"
  END IF

  SELECT docprob_desc 
  INTO   g_master.docprob_desc 
  FROM   tab_doc_prob
  WHERE  docprob_cod = g_master.tip_prob
  IF SQLCA.SQLCODE <> 0 THEN
     LET g_master.docprob_desc = "NO EXISTE"
  END IF

  SELECT afore_desc
  INTO   g_master.desc_afore
  FROM   tab_afore
  WHERE  afore_cod = g_master.cod_afore_ced
  IF STATUS = NOTFOUND THEN
     LET g_master.desc_afore = NULL
  END IF

  IF g_master.ind_infonavit = 'N' THEN
     LET g_master.ind_infonavit = '0'
  END IF

  IF g_master.ind_infonavit = 'S' THEN
     LET g_master.ind_infonavit = '1'
  END IF

  SELECT @descripcion
    INTO g_master.desc_ind_info
    FROM tab_ind_cred
   WHERE @codigo = g_master.ind_infonavit

  SELECT ni.nicho_des
  INTO   g_master.desc_esq_comision
  FROM   tab_nicho ni
  WHERE  ni.nicho = g_master.cod_esq_comision
  IF STATUS = NOTFOUND THEN
     LET g_master.desc_esq_comision = NULL
  END IF

  -- CPL-3165
  LET lc_comando = '',
                   '\n SELECT FIRST 1 fecha_activacion                    ', 
                   '\n   FROM afi_act_menor_edad                          ',
                   '\n  WHERE n_folio        = ',g_master.n_folio,'       ',
                   '\n    AND tipo_solicitud = ',g_master.tipo_solicitud,'',
                   '\n  ORDER BY fecha_carga DESC                       '
  -- DISPLAY lc_comando CLIPPED                  
  PREPARE con_act FROM lc_comando                  
  EXECUTE con_act INTO g_master.fecha_activ
   
  IF SQLCA.SQLCODE = NOTFOUND THEN 
  	 LET g_master.fecha_activ = ""
  END IF 	        
  -- CPL-3165 

  CALL despliega_desc_solic(g_master.tipo_solicitud)

  CALL rescata_status(su_estatus)

  RETURN TRUE

END FUNCTION

FUNCTION Modifica()
#M-----------------
  DEFINE xx_status     SMALLINT
  DEFINE rescato       SMALLINT
  DEFINE cod_origen    SMALLINT
  DEFINE vfecha_modif  DATE
  DEFINE val_1         CHAR(80)
  DEFINE doc_prob_arma CHAR(16)

  DEFINE v_1           SMALLINT
  DEFINE a_yo_act      SMALLINT
  DEFINE a_yo_fena     SMALLINT
  DEFINE a_yo          SMALLINT
  DEFINE bla           SMALLINT
  DEFINE ban           SMALLINT
  DEFINE sino          SMALLINT
  DEFINE fol_prob      DECIMAL(10,0)
  DEFINE vrfc_orig     CHAR(4)
  DEFINE vrfc_si       CHAR(4)

  LET vfecha_modif = TODAY
  LET sw_carta     = 0
  LET fol_prob     = 0

  IF reclamo IS NOT NULL THEN
     LET g_master.n_seguro = ARG_VAL(1)
     LET g_master.n_folio  = ARG_VAL(2)
  END IF

  DISPLAY "" AT 1,1
  DISPLAY "" AT 2,1
  DISPLAY " MODIFICA CERT " AT 1,62 ATTRIBUTE(REVERSE)
  DISPLAY " [ Esc ] Modifica [ Ctrl-C ] Salir sin Modificar " 
  AT 1,1 ATTRIBUTE(CYAN)

  DISPLAY " CTRL : [p] Datos modificados                         ",
          "[g] Despliega [c] Salir  " AT 2,1 ATTRIBUTE(CYAN)

  IF g_master.n_seguro IS NULL THEN
     CALL Inicializa()
  END IF

  LET sw_1 = 0 

  LET g_master.cod_error_origen = NULL
  LET g_master.tipo_solicitud   = NULL
  LET g_master.cod_afore_ced    = NULL
  LET g_master.frecafor         = NULL
  LET g_master.fecha_elaboracion= NULL
  LET g_master.fena             = NULL
  LET g_master.sexo             = NULL
  LET g_master.fecha_emision    = NULL
  LET g_master.edo_civil        = NULL
  LET g_master.const_curp       = NULL
  LET g_master.estadon          = NULL
  LET g_master.nip              = NULL

  INPUT BY NAME g_master.* WITHOUT DEFAULTS
    BEFORE FIELD codven
      NEXT FIELD tipo_solicitud

    AFTER FIELD tipo_solicitud
      IF g_master.tipo_solicitud IS NULL THEN
         NEXT FIELD n_seguro
      END IF

{      IF g_master.tipo_solicitud < 1 OR
         g_master.tipo_solicitud > 3 THEN}
      IF g_master.tipo_solicitud = 5 OR
         g_master.tipo_solicitud = 8 THEN
         NEXT FIELD tipo_solicitud
      ELSE
         NEXT FIELD n_folio
      END IF

      CALL despliega_desc_solic(g_master.tipo_solicitud)

    AFTER FIELD n_folio
      IF g_master.n_folio IS NULL THEN
         NEXT FIELD n_folio
      ELSE
         IF NOT Rescata_datos("N",g_master.n_folio,
                             g_master.tipo_solicitud,"") THEN
            ERROR "Afiliado NO existe"
            NEXT FIELD n_folio
         ELSE
            IF vstatmod > 1 THEN
               DISPLAY BY NAME g_master.*

               PROMPT "REGISTRO NO SE PUEDE MODIFICAR, PRESIONE ENTER"
               ATTRIBUTE(reverse) FOR enter
               CALL Inicializa()
               NEXT FIELD tipo_solicitud
            END IF
            IF vstatmod = 1 THEN
               DISPLAY BY NAME g_master.*
               WHILE TRUE
                  PROMPT "REGISTRO SOLO PUEDE MODIFICAR RFC, ",
                         "¿DESEA MODIFICAR? (S/N) : "
                  ATTRIBUTE(reverse) FOR enter
   
                  IF enter MATCHES'[NnSs]' THEN
                     IF enter MATCHES '[Nn]' THEN
                        CALL Inicializa()
                        RETURN 
                     END IF
                     IF enter MATCHES '[Ss]' THEN
                        NEXT FIELD n_rfc
                        CALL Inicializa()
                     END IF
                  ELSE
                     ERROR "Solo debe presionar (S) Si modificar RFC, ",
                           "(N) No modificar RFC"
                     SLEEP 3
                     ERROR ""
                  END IF
               END WHILE
            END IF
         END IF
      END IF

      DISPLAY BY NAME g_master.*

      IF LENGTH(g_master.n_unico) <> 18 OR
         g_master.n_unico[1] = ' ' THEN
         LET sw_cod = 0
      ELSE
         LET sw_cod = 1
      END IF

      NEXT FIELD paterno

    AFTER FIELD n_seguro
      IF g_master.n_seguro IS NULL THEN
         NEXT FIELD tipo_solicitud
      ELSE
         IF NOT Rescata_datos("C",g_master.n_seguro,0,"") THEN
            ERROR "Afiliado NO existe"
            NEXT FIELD n_seguro
         ELSE
            LET g_master1.* = g_master.*

--            IF g_master.tipo_solicitud > 3 THEN
            IF g_master.tipo_solicitud = 5 OR
               g_master.tipo_solicitud = 8 THEN
               PROMPT "TIPO SOLICITUD NO SE PUEDE MODIFICAR, PRESIONE ENTER"
               ATTRIBUTE(reverse) FOR enter
               CALL Inicializa()
               NEXT FIELD tipo_solicitud
            END IF

            IF vstatmod > 1 THEN
               DISPLAY BY NAME g_master.*
               PROMPT "REGISTRO NO SE PUEDE MODIFICAR, PRESIONE ENTER"
               ATTRIBUTE(reverse) FOR enter
               CALL Inicializa()
               NEXT FIELD tipo_solicitud
            END IF
            IF vstatmod = 1 THEN
               DISPLAY BY NAME g_master.*
               WHILE TRUE
                  PROMPT "REGISTRO SOLO PUEDE MODIFICAR RFC, ",
                         "¿DESEA MODIFICAR? (S/N) : "
                  ATTRIBUTE(reverse) FOR enter

                  IF enter MATCHES'[NnSs]' THEN
                     IF enter MATCHES '[Nn]' THEN
                        CALL Inicializa()
                        RETURN 
                     END IF
                     IF enter MATCHES '[Ss]' THEN
                        NEXT FIELD n_rfc
                        CALL Inicializa()
                     END IF
                  ELSE
                     ERROR "Solo debe presionar (S) Si modificar RFC, ",
                           "(N) No modificar RFC"
                     SLEEP 2
                     ERROR ""
                  END IF
               END WHILE
            END IF
         END IF

         SELECT 'X'
           FROM cta_act_marca
          WHERE @nss        = g_master.n_seguro
            AND @marca_cod IN (120,130)
          GROUP BY 1
         IF STATUS <> NOTFOUND THEN
            ERROR "Registro inhabilitado"
            NEXT FIELD tipo_solicitud
         END IF

         SELECT 'X'
           FROM taa_cd_det_cedido
          WHERE @n_seguro = g_master.n_seguro
            AND @estado  IN (12,103)
          GROUP BY 1
         IF STATUS <> NOTFOUND THEN
            ERROR "Registro traspasado"
            NEXT FIELD tipo_solicitud
         END IF
      END IF

      DISPLAY BY NAME g_master.*

      IF LENGTH(g_master.n_unico) <> 18 OR
         g_master.n_unico[1] = ' ' THEN
         LET sw_cod = 0
      ELSE
         LET sw_cod = 1
      END IF

      NEXT FIELD paterno

    AFTER FIELD paterno
      IF g_master.paterno IS NULL OR
         g_master.paterno =  " "  THEN
         ERROR "APELLIDO PATERNO NO PUEDE SER NULO " ATTRIBUTE (REVERSE)
         SLEEP 2
         ERROR " " 
         NEXT FIELD paterno
      ELSE
         LET v_1 = 0
         INITIALIZE val_1 TO NULL
         CALL verifica_nombre(g_master.paterno) RETURNING v_1,val_1 #ve--
         IF v_1 = 1 THEN
            ERROR "A.Paterno ",val_1 CLIPPED
            NEXT FIELD paterno
         END IF
      END IF

      NEXT FIELD materno
 
    AFTER FIELD materno
      IF g_master.materno[1] = " " THEN
         ERROR "Ingrese Ap. Materno correcto o deje el campo nulo"
         LET g_master.materno = NULL
         DISPLAY BY NAME g_master.materno
         NEXT FIELD materno
      END IF

      IF g_master.materno != " " OR
         g_master.materno IS NOT NULL THEN
         LET v_1 = 0
         INITIALIZE val_1 TO NULL
         CALL verifica_nombre(g_master.materno) RETURNING v_1,val_1 #ve--
         IF v_1 = 1 THEN
            ERROR "A.Materno ",val_1 CLIPPED
            NEXT FIELD materno
         END IF
      END IF

      NEXT FIELD nombres
 
    AFTER FIELD nombres
      IF g_master.nombres IS NULL OR
         g_master.nombres[1] =  " "  THEN
         ERROR "EL NOMBRE NO PUEDE SER NULO " ATTRIBUTE (REVERSE)
         SLEEP 2
         ERROR " " 
         NEXT FIELD nombres
      ELSE
         LET v_1 = 0
         INITIALIZE val_1 TO NULL
         CALL verifica_nombre(g_master.nombres) RETURNING v_1,val_1 #ve--
         IF v_1 = 1 THEN
            ERROR "El Nombre ",val_1 CLIPPED
            NEXT FIELD nombres
         END IF
      END IF

      NEXT FIELD n_rfc
 
    AFTER FIELD n_rfc 
      IF g_master.n_rfc IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD n_rfc
      ELSE
         LET v_1 = 0
         INITIALIZE val_1 TO NULL
         CALL verifica_rfc(g_master.n_rfc[1,4])
         RETURNING v_1,val_1 ##ve--
         IF v_1 = 1 THEN
            ERROR "R.F.C ",val_1 CLIPPED
            NEXT FIELD n_rfc
         END IF

         CALL valida_est_rfc(g_master.n_rfc)
         RETURNING  pasa_curp, desc_err

         IF pasa_curp = 1 THEN
            ERROR "", desc_err
            LET pasa_curp = 0
            NEXT FIELD n_rfc
         END IF
      END IF

      CALL arma_clave_rfc(g_master.paterno,
                          g_master.materno,
                          g_master.nombres,
                          g_master.fena) RETURNING cve_arma #rac

      IF cve_arma[1,4] != g_master.n_rfc[1,4] THEN
         LET cve_arma = cve_arma[1,4]
         ERROR "El inicio del rfc debe ser: ", cve_arma
         WHILE TRUE
            PROMPT "¿Dejar inicio de rfc encontrado [S/N]? " FOR enter
            IF enter MATCHES "[Ss/Nn]" THEN
               IF enter MATCHES "[Ss]" THEN
                  LET g_master.n_rfc = cve_arma CLIPPED, g_master.n_rfc[5,13]
                  DISPLAY BY NAME g_master.n_rfc
                  EXIT WHILE
               ELSE
                  LET vrfc_orig = g_master.n_rfc[1,4]

                  SELECT @palabra_si
                    INTO vrfc_si
                    FROM afi_no_conviene
                   WHERE @palabra_no = vrfc_orig

                  IF SQLCA.SQLCODE = 0 THEN
                     LET g_master.n_rfc = vrfc_si, g_master.n_rfc[5,13]
                     DISPLAY BY NAME g_master.n_rfc
                  END IF
                  EXIT WHILE
               END IF
            END IF
         END WHILE
      END IF

      IF LENGTH(g_master.n_rfc) <> 10 AND
         LENGTH(g_master.n_rfc) <> 13 THEN
         ERROR "Debe ingresar R.F.C. completo"
         NEXT FIELD n_rfc
      END IF

      IF NOT valida_fecha_rfc(g_master.n_rfc[5,10]) THEN
         ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
         NEXT FIELD n_rfc
      ELSE
         WHENEVER ERROR CONTINUE
            LET aaa = g_master.n_rfc[5,6]
            LET mm = g_master.n_rfc[7,8]
            LET dd = g_master.n_rfc[9,10]
            LET z_fecha = mm,"/",dd,"/19",aaa
            LET j_fecha = z_fecha
         WHENEVER ERROR STOP

         IF j_fecha IS NULL THEN
            ERROR "fecha Invalida en RFC"
            NEXT FIELD n_rfc
         END IF
      END IF

      IF vstatmod = 1 THEN
         NEXT FIELD const_curp
      ELSE
         NEXT FIELD sexo
      END IF

    AFTER FIELD sexo
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         IF g_master.sexo IS NULL OR g_master.sexo = " " OR
            g_master.sexo = 0  THEN
            ERROR "Digite correctamente el sexo antes de pasar a otro campo"
            NEXT FIELD sexo
         END IF
         NEXT FIELD n_rfc
      END IF

      IF g_master.sexo IS NULL OR g_master.sexo = " " OR
         g_master.sexo = 0  THEN
         CALL Despliega_sexos() RETURNING g_master.sexo,
                                          g_master.desc_sexo
      ELSE
         SELECT sexo_desc
           INTO g_master.desc_sexo
           FROM tab_sexo
          WHERE sexo_cod = g_master.sexo

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "Sexo Inexistente"
            NEXT FIELD sexo
         END IF
      END IF

      DISPLAY BY NAME g_master.sexo,g_master.desc_sexo

      NEXT FIELD edo_civil

    AFTER FIELD edo_civil
      IF g_master.edo_civil IS NULL THEN
         CALL Despliega_estados_civiles() 
         RETURNING g_master.edo_civil, g_master.desc_edo_civil
      ELSE
         SELECT ecivi_desc
         INTO g_master.desc_edo_civil
         FROM tab_edo_civil
         WHERE ecivi_cod = g_master.edo_civil

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "Estado Civil Inexistente"
            NEXT FIELD edo_civil
         END IF
      END IF

      DISPLAY BY NAME g_master.edo_civil,g_master.desc_edo_civil

      NEXT FIELD fena
 
    BEFORE FIELD fena
      LET x_fecha = g_master.n_rfc[7,8],"/",
                    g_master.n_rfc[9,10],"/",
                    "19",g_master.n_rfc[5,6]

      LET xx_fecha = x_fecha

    AFTER FIELD fena
      IF g_master.fena IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD fena
      END IF

      IF g_master.fena >= TODAY THEN
         ERROR "La fecha de nacimiento -NO- debe ser",
               "igual o mayor al dia de hoy"
         NEXT FIELD fena
      END IF

      LET a_yo_act  = 0 LET a_yo_fena = 0   LET a_yo = 0

      LET a_yo_act  = YEAR(TODAY) USING "&&&&"
      LET a_yo_fena = YEAR(g_master.fena) USING "&&&&"
      LET a_yo      = a_yo_act - a_yo_fena

      IF a_yo > 120 THEN
         ERROR "Esta persona pasa del rango de 120 ayos, Verifique nuevamente"
         NEXT FIELD fena
      END IF

      IF xx_fecha <> g_master.fena THEN
         WHILE TRUE
            PROMPT "Existen inconsistencias entre RFC/Fecha nacimiento, ",
                   "es correcto ¨[S/N]? " FOR enter
            IF enter MATCHES "[Ss/Nn]" THEN
               IF enter MATCHES "[Ss]" THEN
                  NEXT FIELD salario_base_comis
                  EXIT WHILE
               ELSE
                  NEXT FIELD n_rfc
               END IF
            ELSE
               ERROR "Solo debe presionar (S)i o (N)o"
               SLEEP 3
               ERROR ""
            END IF
         END WHILE
      END IF 

    BEFORE FIELD salario_base_comis
      IF g_master.salario_base_comis IS NULL THEN
         LET g_master.salario_base_comis = 0
         DISPLAY BY NAME g_master.salario_base_comis
      END IF

    AFTER FIELD salario_base_comis
      IF g_master.salario_base_comis IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD salario_base_comis
      END IF
      NEXT FIELD cod_esq_comision

    BEFORE FIELD cod_esq_comision                           
      IF g_master.cod_esq_comision IS NULL THEN           
         LET g_master.cod_esq_comision = 0               
         DISPLAY BY NAME g_master.cod_esq_comision
      END IF                                              
                                                                   
    AFTER FIELD cod_esq_comision                            
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN            
         NEXT FIELD salario_base_comis                   
      END IF                                              

      {IF g_master.cod_esq_comision IS NULL THEN           
         ERROR "Campo nicho de mercado NO puede ser NULO"
         NEXT FIELD cod_esq_comision                     
      END IF

      SELECT ni.nicho_des                        
      INTO   g_master.desc_esq_comision          
      FROM   tab_nicho ni                        
      WHERE  ni.nicho = g_master.cod_esq_comision
                                           
      IF STATUS = NOTFOUND THEN                  
         ERROR "Nicho Inexistente"              
         NEXT FIELD cod_esq_comision            
      END IF}

      LET g_master.desc_esq_comision = NULL 
                                           
      DISPLAY BY name g_master.desc_esq_comision 

    AFTER FIELD estadon
      IF g_master.estadon IS NULL OR
         g_master.estadon =  0    THEN
         CALL Despliega_estados() RETURNING g_master.estadon,
                                            g_master.desc_estadon

         IF g_master.estadon = 0 THEN
            NEXT FIELD estadon 
         END IF
      ELSE
         SELECT estad_desc 
         INTO   g_master.desc_estadon 
         FROM   tab_estado
         WHERE  estad_cod = g_master.estadon

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "Entidad de Nacimiento Inexistente"
            NEXT FIELD estadon
         END IF
      END IF

      DISPLAY BY NAME g_master.estadon,g_master.desc_estadon

      NEXT FIELD nacionalidad

    BEFORE FIELD nacionalidad
      IF g_master.nacionalidad IS NULL THEN
         LET g_master.nacionalidad = "MEX"

         DISPLAY BY NAME g_master.nacionalidad
      END IF

    AFTER FIELD nacionalidad
      IF g_master.nacionalidad IS NULL OR
         g_master.nacionalidad = " "   THEN
         CALL Despliega_pais() RETURNING g_master.nacionalidad,
                                         g_master.desc_nacionalidad
      ELSE
         SELECT pais_desc
         INTO   g_master.desc_nacionalidad
         FROM   tab_pais
         WHERE  pais_cod = g_master.nacionalidad

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "Pais Inexistente"
            NEXT FIELD nacionalidad
         END IF
      END IF

      DISPLAY BY NAME g_master.nacionalidad, g_master.desc_nacionalidad

      NEXT FIELD tip_prob
 
    AFTER FIELD tip_prob
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD nacionalidad
      END IF

      IF g_master.tip_prob IS NULL OR g_master.tip_prob = " " OR
         g_master.tip_prob = 0 THEN
         CALL Despliega_documento_probatorio()
         RETURNING g_master.tip_prob,g_master.docprob_desc

         IF g_master.tip_prob = 0 THEN
            NEXT FIELD tip_prob 
         END IF
      ELSE
         SELECT docprob_desc
         INTO   g_master.docprob_desc
         FROM   tab_doc_prob
         WHERE  docprob_cod = g_master.tip_prob

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "Tipo de Documento Probatorio Inexistente"
            NEXT FIELD tip_prob
         END IF
      END IF
{
      IF g_master.tip_prob = 5 THEN
         IF g_master1.tip_prob = 6 THEN
            ERROR "Cambio de Tipo de Documento Probatorio no valido"
            LET g_master.tip_prob = g_master1.tip_prob
            DISPLAY BY NAME g_master.tip_prob
            NEXT FIELD tip_prob
         ELSE
            NEXT FIELD n_unico
         END IF
      END IF
}
      DISPLAY BY NAME g_master.tip_prob,g_master.docprob_desc

--->05 ene 2009
{
      IF g_master.tip_prob <> 6 AND
         g_master.tip_prob <> 0 THEN
         NEXT FIELD n_unico
      ELSE
         NEXT FIELD fol_prob
      END IF
}
      IF g_master.tip_prob = "5" THEN
         LET g_master.doc_prob = g_master.n_unico[1,16]

             SELECT 'X'
             FROM   cta_act_marca a
             WHERE  a.nss = g_master.n_seguro
             AND    a.marca_cod IN(SELECT b.marca_resulta
                                   FROM   tab_marca b
                                   WHERE  b.ind_habilita = 1)

             IF SQLCA.SQLCODE <> 0 THEN
                 WHILE TRUE
                 PROMPT "Cuenta con base a la CURP aun sigue activa, ",
                        "DESEA SEGUIR LA CAPTURA ¿S/N? "
                 ATTRIBUTES(reverse)
                 FOR aux_pausa

                 IF aux_pausa MATCHES "[SsNn]" THEN
                     IF aux_pausa MATCHES "[Nn]" THEN
                         RETURN
                     ELSE
                         NEXT FIELD n_unico
                         EXIT WHILE
                     END IF
                 ELSE
                     ERROR "Solo debe presionar (S) Si o (N) No"
                     SLEEP 2
                     ERROR ""
                 END IF
                 END WHILE
             ELSE
                 NEXT FIELD n_unico
             END IF
      ELSE
         --NEXT FIELD fol_prob
         NEXT FIELD n_unico   ---nuevo 21012009
      END IF

    AFTER FIELD n_unico
      IF g_master.n_unico = "                  " THEN
         INITIALIZE g_master.n_unico TO NULL
      END IF

       IF g_master.tip_prob = 5 THEN               ---16 Dic 2008
         IF LENGTH(g_master.n_unico) <> 18 OR
            g_master.n_unico[1] = " " OR
            g_master.n_unico IS NULL THEN
            ERROR "Debe ingresar CURP completa"

            LET g_master.n_unico = g_master1.n_unico
         
            NEXT FIELD n_unico
         END IF
      ELSE
         IF g_master.n_unico IS NOT NULL THEN               ---16 Dic 2008
            IF LENGTH(g_master.n_unico) <> 18 OR
               g_master.n_unico[1] = " " THEN
               --g_master.n_unico IS NULL THEN
               ERROR "Debe ingresar CURP completa"

               LET g_master.n_unico = g_master1.n_unico

               NEXT FIELD n_unico
            END IF
         END IF
      END IF

      NEXT FIELD fol_prob

    AFTER FIELD fol_prob
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD tip_prob
      END IF

      IF g_master.fol_prob IS NULL THEN
         ERROR "Numero de Folio NO puede ser NULO"
         NEXT FIELD fol_prob
      END IF

      DISPLAY BY NAME g_master.fol_prob

    BEFORE FIELD doc_prob 
      IF g_master.tip_prob = "6" THEN
         LET g_master.doc_prob = NULL

         DISPLAY BY NAME g_master.doc_prob

         NEXT FIELD ind_infonavit
      END IF

      IF g_master.tip_prob = "5" THEN
         LET g_master.doc_prob = g_master.n_unico[1,16]
         CALL arma_clave(g_master.paterno,
                         g_master.materno,
                         g_master.nombres,
                         g_master.fena,
                         g_master.estadon,
                         g_master.sexo) RETURNING doc_prob_arma #ac
          ---05 ene 2009 MOD CURP
          IF g_master.doc_prob <> doc_prob_arma THEN
              ERROR "INICIO DE CURP PROPUESTA ",doc_prob_arma
          END IF
         ---05 ene 2009 MOD CURP
      END IF

    AFTER FIELD doc_prob 
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD fol_prob
      END IF

      CASE g_master.tip_prob
        WHEN 1
          IF LENGTH(g_master.doc_prob) <> 16 THEN
             ERROR "Documento probatorio erroneo"
             SLEEP 2
             LET g_master.doc_prob = ''
             DISPLAY BY NAME g_master.doc_prob
             NEXT FIELD doc_prob
          END IF
   
          CALL valida_anyo_nac() RETURNING sino
   
          IF sino THEN
             ERROR "Año de registro erroneo"
             SLEEP 2
             LET g_master.doc_prob = ''
             DISPLAY BY NAME g_master.doc_prob
             NEXT FIELD doc_prob
          END IF
        WHEN 2
	  LET g_master.doc_prob = g_master.doc_prob[2,16]

          IF LENGTH(g_master.doc_prob) <> 15 THEN
             ERROR "Documento probatorio erroneo"
             SLEEP 2
             LET g_master.doc_prob = ''
             DISPLAY BY NAME g_master.doc_prob
             NEXT FIELD doc_prob
          END IF
          LET g_master.doc_prob = dcto_2, g_master.doc_prob
   
          CALL valida_anyo_nac() RETURNING sino
   
          IF sino THEN
             ERROR "Año de registro erroneo"
             SLEEP 2
             LET g_master.doc_prob = ''
             DISPLAY BY NAME g_master.doc_prob
             NEXT FIELD doc_prob
          END IF
        WHEN 3
          LET g_master.doc_prob = g_master.doc_prob[10,16]

          IF LENGTH(g_master.doc_prob) <> 7 THEN
             ERROR "Documento probatorio erroneo"
             SLEEP 2
             LET g_master.doc_prob = ''
             DISPLAY BY NAME g_master.doc_prob
             NEXT FIELD doc_prob
          END IF
          LET g_master.doc_prob = dcto_3, g_master.doc_prob
        WHEN 4
          LET g_master.doc_prob = g_master.doc_prob[8,16]

          IF LENGTH(g_master.doc_prob) <> 9 THEN
             ERROR "Documento probatorio erroneo"
             SLEEP 2
             LET g_master.doc_prob = ''
             DISPLAY BY NAME g_master.doc_prob
             NEXT FIELD doc_prob
          END IF
          LET g_master.doc_prob = dcto_4, g_master.doc_prob
        WHEN 5
          IF g_master.doc_prob NOT MATCHES doc_prob_arma THEN
             WHILE TRUE
                PROMPT "Existen inconsistencias entre doc ",
                       "prob/raiz curp, es correcto ¿[S/N]? " FOR enter
                IF enter MATCHES "[Ss/Nn]" THEN
                   IF enter MATCHES "[Ss]" THEN
                      WHILE TRUE
                         PROMPT "Documento probatorio ",
                                "tiene error de origen [S/N]? "
                         ATTRIBUTES (REVERSE) FOR c_doc
                         IF c_doc  MATCHES "[SsNn]" THEN
                            IF c_doc MATCHES "[Nn]" THEN
                               NEXT FIELD cod_error_origen
                            ELSE
                               LET cod_origen = cod_origen + 256
                               NEXT FIELD cod_error_origen
                            END IF
                            EXIT WHILE
                         ELSE
                            ERROR "Solo debe presionar (S)i o (N)o" SLEEP 2
                            ERROR ""
                         END IF
                      END WHILE
                      NEXT FIELD cod_error_origen
                      EXIT WHILE
                   ELSE
                      NEXT FIELD n_unico
                   END IF
                ELSE
                   ERROR "Solo debe presionar (S)i o (N)o"
                   SLEEP 3
                   ERROR ""
                END IF
             END WHILE 
          END IF
   
          IF LENGTH(g_master.doc_prob) <> 16 THEN
             ERROR "Documento probatorio erroneo"
             SLEEP 2
             LET g_master.doc_prob = ''
             DISPLAY BY NAME g_master.doc_prob
             NEXT FIELD doc_prob
          END IF
        WHEN 7
	  LET g_master.doc_prob = g_master.doc_prob[8,16]

          IF LENGTH(g_master.doc_prob) <> 9 THEN
             ERROR "Documento probatorio erroneo"
             SLEEP 2
             LET g_master.doc_prob = ''
             DISPLAY BY NAME g_master.doc_prob
             NEXT FIELD doc_prob
          END IF
          LET g_master.doc_prob = dcto_7, g_master.doc_prob
      END CASE

      IF NOT Verifica_documento_probatorio(g_master.tip_prob,
                                           g_master.doc_prob) THEN
         NEXT FIELD doc_prob
      END IF

      DISPLAY BY NAME g_master.doc_prob

    AFTER FIELD ind_infonavit
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD doc_prob
      END IF

      IF g_master.ind_infonavit IS NULL OR
         g_master.ind_infonavit = ' '   THEN
         CALL Despliega_ind_info() RETURNING g_master.ind_infonavit,
                                             g_master.desc_ind_info
         IF g_master.ind_infonavit IS NULL OR
            g_master.ind_infonavit = ' '   THEN
            ERROR "Se requiere la clave del Indicador de Credito"
            NEXT FIELD ind_infonavit
         END IF
      END IF

      IF g_master.ind_infonavit NOT MATCHES "[012]" THEN
         CALL Despliega_ind_info() RETURNING g_master.ind_infonavit,
                                             g_master.desc_ind_info
         IF g_master.ind_infonavit NOT MATCHES "[012]" THEN
            ERROR "(0) Sin credito  (1) Credito Infonavit (2) Credito 43 bis"
            NEXT FIELD ind_infonavit
         END IF
      ELSE
         SELECT @descripcion
           INTO g_master.desc_ind_info
           FROM safre_af:tab_ind_cred
          WHERE @codigo = g_master.ind_infonavit
         DISPLAY BY NAME g_master.desc_ind_info
      END IF

--->05 ene 2009 MOD CURP
{
    BEFORE FIELD cod_error_origen
      DISPLAY BY NAME g_master.cod_error_origen

      IF sw_cod THEN
         CALL verifica_cod_error_orig()
      ELSE
         IF cod_origen > 0 THEN
            LET g_master.cod_error_origen = cod_origen
         END IF
         DISPLAY BY NAME g_master.cod_error_origen
      END IF

    AFTER FIELD cod_error_origen
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD ind_infonavit
      END IF
}
---<

      NEXT FIELD const_curp

    BEFORE FIELD const_curp
      DISPLAY BY NAME g_master.const_curp

    AFTER FIELD const_curp
      IF (g_master.const_curp LIKE "%   " OR
          g_master.const_curp IS NULL)    THEN
          PROMPT "Desea dejar como NULO la constancia curp [S/N]: " FOR enter
          IF enter MATCHES "[Ss]" THEN
             LET g_master.const_curp = NULL
          ELSE
             NEXT FIELD g_master.const_curp
          END IF
      END IF

    ON KEY ( CONTROL-P )
       SELECT "X"
       FROM   afi_mae_modifica md
       WHERE  md.n_seguro = g_master.n_seguro
       AND    md.n_folio = g_master.n_folio
       AND    md.tipo_solicitud = g_master.tipo_solicitud
       GROUP BY 1

       IF SQLCA.SQLCODE = 0 THEN
          CALL verifica_datos_modificados()
       ELSE
          PROMPT "Registro sin modificaciones, [Enter] p/continuar " 
          ATTRIBUTES (REVERSE) FOR enter
       END IF

    ON KEY ( CONTROL-G )
       CALL Despliega()
       DISPLAY BY NAME g_master.n_folio

    ON KEY ( INTERRUPT )
       CALL Inicializa()
       DISPLAY "                                                                               " AT 6,2  ATTRIBUTE(REVERSE)
       DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
       DISPLAY "                  " AT 1,62 
       EXIT INPUT

    ON KEY ( ESC )
{       IF g_master.tip_prob = 6 THEN
          PROMPT "Tipo de documento probatorio incorrecto, ",
                 " presione [ENTER]." ATTRIBUTE(REVERSE) FOR enter
          RETURN
       END IF
}
       WHILE TRUE
--->05 ene 2009 MOD CURP
          SELECT @paterno,@materno,@nombres,@sexo,@fena,@estadon,@nacionalidad,@tip_prob,
                 @doc_prob,@fol_prob
          INTO g_compara.*
          FROM afi_mae_afiliado
          WHERE @n_seguro       = g_master.n_seguro
          AND   @n_folio        = g_master.n_folio
          AND   @tipo_solicitud = g_master.tipo_solicitud

          IF g_master.materno IS NULL THEN
             LET g_master.materno = ' '
          END IF
          IF g_compara.materno IS NULL THEN
             LET g_compara.materno = ' '
          END IF

          IF g_master.doc_prob IS NULL THEN
             LET g_master.doc_prob = ' '
          END IF
          IF g_compara.doc_prob IS NULL THEN
             LET g_compara.doc_prob = ' '
          END IF

          IF g_master.fol_prob IS NULL THEN
             LET g_master.fol_prob = ' '
          END IF
          IF g_compara.fol_prob IS NULL THEN
             LET g_compara.fol_prob = ' '
          END IF

#1 paterno
          --IF g_master1.paterno      <> g_master.paterno      THEN
          IF g_master.paterno      <> g_compara.paterno      THEN
             LET cadena = '1' CLIPPED
          ELSE 
             LET cadena = '0' CLIPPED
          END IF
#2 materno
          --IF g_master1.materno      <> g_master.materno      THEN
          IF g_master.materno      <> g_compara.materno      THEN
             LET cadena = cadena CLIPPED,'1'
          ELSE
             LET cadena = cadena CLIPPED,'0'
          END IF
#3 nombres
          --IF g_master1.nombres      <> g_master.nombres      THEN
          IF g_master.nombres      <> g_compara.nombres      THEN
             LET cadena = cadena CLIPPED,'1'
          ELSE
             LET cadena = cadena CLIPPED,'0'
          END IF
#4 sexo
          --IF g_master1.sexo         <> g_master.sexo         THEN
          IF g_master.sexo      <> g_compara.sexo        THEN
             LET cadena = cadena CLIPPED,'1'
          ELSE
             LET cadena = cadena CLIPPED,'0'
          END IF
#5 fena
          --IF g_master1.fena         <> g_master.fena         THEN
          IF g_master.fena      <> g_compara.fena      THEN
             LET cadena = cadena CLIPPED,'1'
          ELSE
             LET cadena = cadena CLIPPED,'0'
          END IF
#6 estadon
          --IF g_master1.estadon      <> g_master.estadon      THEN
          IF g_master.estadon      <> g_compara.estadon      THEN
             LET cadena = cadena CLIPPED,'1'
          ELSE
             LET cadena = cadena CLIPPED,'0'
          END IF
#7 nacionalidad
          --IF g_master1.nacionalidad <> g_master.nacionalidad THEN
          IF g_master.nacionalidad <> g_compara.nacionalidad      THEN
             LET cadena = cadena CLIPPED,'1'
          ELSE
             LET cadena = cadena CLIPPED,'0'
          END IF
#8 tip prob
          --IF g_master1.tip_prob     <> g_master.tip_prob     THEN
          IF g_master.tip_prob      <> g_compara.tip_prob     THEN
             LET cadena = cadena CLIPPED,'1'
          ELSE
             LET cadena = cadena CLIPPED,'0'
          END IF
#9 año reg
#acta (1)
          IF g_master.tip_prob = '1' THEN
             LET r = 0
             LET ban_cadena = 0
{             FOR r = 6 TO 7
                 --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                 IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                    LET ban_cadena = 1
                    EXIT FOR
                 ELSE
                    LET ban_cadena = 0
                    --EXIT FOR
                 END IF
             END FOR}

             IF g_master.doc_prob[1,1] = " " THEN
                FOR r = 7 TO 8
                    --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                    IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                       LET ban_cadena = 1
                       EXIT FOR
                    ELSE
                       LET ban_cadena = 0
                       --EXIT FOR
                    END IF
                END FOR
             ELSE
                FOR r = 6 TO 7
                    --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                    IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                       LET ban_cadena = 1
                       EXIT FOR
                    ELSE
                       LET ban_cadena = 0
                       --EXIT FOR
                    END IF
                END FOR
             END IF


             IF ban_cadena = 1 THEN
                 LET cadena = cadena CLIPPED,"1"
              ELSE
                 LET cadena = cadena CLIPPED,"0"
              END IF
          END IF

#naturalizacion (4)
          IF g_master.tip_prob = '4' THEN
             LET r = 0
             LET ban_cadena = 0
             FOR r = 1 TO 16
                 IF g_master.doc_prob[r] = " " THEN
                 ELSE 
                     FOR r = r TO r + 4
                         --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                         IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                            LET ban_cadena = 1
                            EXIT FOR
                         ELSE
                            LET ban_cadena = 0
                            --EXIT FOR
                         END IF
                     END FOR
                  EXIT FOR
                  END IF
              END FOR

             IF ban_cadena = 1 THEN
                 LET cadena = cadena CLIPPED,"1"
              ELSE
                 LET cadena = cadena CLIPPED,"0"
              END IF
          END IF

#certificado nacionalidad (7)
          IF g_master.tip_prob = '7' THEN
             LET r = 0
             LET ban_cadena = 0
             FOR r = 1 TO 16
                 IF g_master.doc_prob[r] = " " THEN
                 ELSE 
                     FOR r = r TO r + 4
                         --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                         IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                            LET ban_cadena = 1
                            EXIT FOR
                         ELSE
                            LET ban_cadena = 0
                            --EXIT FOR
                         END IF
                     END FOR
                  EXIT FOR
                  END IF
              END FOR

             IF ban_cadena = 1 THEN
                 LET cadena = cadena CLIPPED,"1"
              ELSE
                 LET cadena = cadena CLIPPED,"0"
              END IF
          END IF

#docto migratorio (3)
          IF g_master.tip_prob = '3' THEN
             LET r = 0
             LET ban_cadena = 0
             FOR r = 1 TO 16
                 IF g_master.doc_prob[r] = " " THEN
                 ELSE 
                     FOR r = r TO r + 7
                         IF r > 16 THEN 
                             EXIT FOR
                         END IF
                         --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                         IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                            LET ban_cadena = 1
                            EXIT FOR
                         ELSE
                            LET ban_cadena = 0
                            --EXIT FOR
                         END IF
                     END FOR
                  EXIT FOR
                  END IF
              END FOR

             IF ban_cadena = 1 THEN
                 LET cadena = cadena CLIPPED,"1"
              ELSE
                 LET cadena = cadena CLIPPED,"0"
              END IF
          END IF

          IF ((g_master.tip_prob <> '1') AND
              (g_master.tip_prob <> '4') AND
              (g_master.tip_prob <> '7') AND
              (g_master.tip_prob <> '3')) THEN
               LET cadena = cadena CLIPPED,"0"
          END IF

#10 folio
          --IF g_master1.fol_prob     <> g_master.fol_prob     THEN
          IF g_master.fol_prob     <> g_compara.fol_prob   THEN
             LET cadena = cadena CLIPPED,'1'
          ELSE
             LET cadena = cadena CLIPPED,'0'
          END IF

#11 y 12 tomo y libro
#actas (1 y 2)
          IF g_master.tip_prob = '1' OR
             g_master.tip_prob = '2' THEN
             LET r = 0
             LET ban_cadena = 0
{             FOR r = 8 TO 11
                 --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                 IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                    LET ban_cadena = 1
                    EXIT FOR
                 ELSE
                    LET ban_cadena = 0
                    --EXIT FOR
                 END IF
             END FOR
}
             IF g_master.doc_prob[1,1] = " " THEN
                FOR r = 9 TO 12
                    --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                    IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                       LET ban_cadena = 1
                       EXIT FOR
                    ELSE
                       LET ban_cadena = 0
                       --EXIT FOR
                    END IF
                END FOR
             ELSE
                FOR r = 8 TO 11
                    --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                    IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                       LET ban_cadena = 1
                       EXIT FOR
                    ELSE
                       LET ban_cadena = 0
                       --EXIT FOR
                    END IF
                END FOR
             END IF

             IF ban_cadena = 1 THEN
                 LET cadena = cadena CLIPPED,"11"
              ELSE
                 LET cadena = cadena CLIPPED,"00"
              END IF
          ELSE
             LET cadena = cadena CLIPPED, "00"
          END IF

#13 y 14 num acta y foja
#actas (1 y 2)
          IF g_master.tip_prob = '1' OR
             g_master.tip_prob = '2' THEN
             LET r = 0
             LET ban_cadena = 0
{             FOR r = 12 TO 16
                 --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                 IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                    LET ban_cadena = 1
                    EXIT FOR
                 ELSE
                    LET ban_cadena = 0
                    --EXIT FOR
                 END IF
             END FOR}

             IF g_master.doc_prob[1,1] = " " THEN
                FOR r = 13 TO 16
                    --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                    IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                       LET ban_cadena = 1
                       EXIT FOR
                    ELSE
                       LET ban_cadena = 0
                       --EXIT FOR
                    END IF
                END FOR
             ELSE
                FOR r = 12 TO 16
                    --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                    IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                       LET ban_cadena = 1
                       EXIT FOR
                    ELSE
                       LET ban_cadena = 0
                       --EXIT FOR
                    END IF
                END FOR
             END IF


             IF ban_cadena = 1 THEN
                 LET cadena = cadena CLIPPED,"11"
              ELSE
                 LET cadena = cadena CLIPPED,"00"
              END IF
          ELSE
             LET cadena = cadena CLIPPED, "00"
          END IF

#15 ent federativa doc prob
#actas (1 y 2)
          IF g_master.tip_prob = '1' OR
             g_master.tip_prob = '2' THEN
             LET r = 0
             LET ban_cadena = 0
             IF g_master.doc_prob[1,1] = " " THEN
                FOR r = 2 TO 3
                    --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                    IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                       LET ban_cadena = 1
                       EXIT FOR
                    ELSE
                       LET ban_cadena = 0
                       --EXIT FOR
                    END IF
                END FOR
             ELSE
                FOR r = 1 TO 2
                    --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                    IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                       LET ban_cadena = 1
                       EXIT FOR
                    ELSE
                       LET ban_cadena = 0
                       --EXIT FOR
                    END IF
                END FOR
             END IF

             IF ban_cadena = 1 THEN
                 LET cadena = cadena CLIPPED,"1"
              ELSE
                 LET cadena = cadena CLIPPED,"0"
              END IF
          ELSE
             LET cadena = cadena CLIPPED, "0"
          END IF

#16 municipio doc prob
#actas (1 y 2)
          IF g_master.tip_prob = '1' OR
             g_master.tip_prob = '2' THEN
             LET r = 0
             LET ban_cadena = 0
{             FOR r = 3 TO 5
                 --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                 IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                    LET ban_cadena = 1
                    EXIT FOR
                 ELSE
                    LET ban_cadena = 0
                    --EXIT FOR
                 END IF
             END FOR}

             IF g_master.doc_prob[1,1] = " " THEN
                FOR r = 4 TO 6
                    --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                    IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                       LET ban_cadena = 1
                       EXIT FOR
                    ELSE
                       LET ban_cadena = 0
                       --EXIT FOR
                    END IF
                END FOR
             ELSE
                FOR r = 3 TO 5
                    --IF g_master1.doc_prob[r] <> g_master.doc_prob[r] THEN
                    IF g_master.doc_prob[r] <> g_compara.doc_prob[r] THEN
                       LET ban_cadena = 1
                       EXIT FOR
                    ELSE
                       LET ban_cadena = 0
                       --EXIT FOR
                    END IF
                END FOR
             END IF

             IF ban_cadena = 1 THEN
                 LET cadena = cadena CLIPPED,"1"
              ELSE
                 LET cadena = cadena CLIPPED,"0"
              END IF
          ELSE
             LET cadena = cadena CLIPPED, "0"
          END IF

#17 
          LET cadena = cadena CLIPPED , '0'
{
          INITIALIZE cadena2 TO NULL
          INITIALIZE cadena3 TO NULL
          SELECT cadena_dif
          INTO cadena2
          FROM afi_ctr_curp
          WHERE nss            = g_master.n_seguro
          AND   tipo_solicitud = g_master.tipo_solicitud
          AND   n_folio        = g_master.n_folio

          IF cadena2 IS NOT NULL THEN
             IF cadena <> cadena2 THEN
                FOR r = 1 TO 17
                   IF cadena2[r] <> '0' THEN
                      LET cadena3 = cadena3 CLIPPED,cadena2[r] CLIPPED
                   ELSE
                      LET cadena3 = cadena3 CLIPPED,cadena[r] CLIPPED
                   END IF
                END FOR
             END IF
             LET cadena = cadena3 CLIPPED
          END IF
}
          DISPLAY cadena AT 21,8
---<

         IF g_master.n_folio = g_master1.n_folio THEN
            CALL Desea_modificar()
         ELSE
            ERROR "Folio no se puede modificar"
            SLEEP 3
            LET aux_pausa = 'N'
         END IF

         IF aux_pausa MATCHES "[SsNn]" THEN
            EXIT WHILE
         END IF

       END WHILE

       IF aux_pausa MATCHES "[Nn]" THEN
          RETURN
       ELSE

          LET g_master1.paterno      = g_master1.paterno      CLIPPED
          LET g_master.paterno       = g_master.paterno       CLIPPED
          LET g_master1.materno      = g_master1.materno      CLIPPED
          LET g_master.materno       = g_master.materno       CLIPPED
          LET g_master1.nombres      = g_master1.nombres      CLIPPED
          LET g_master.nombres       = g_master.nombres       CLIPPED
          LET g_master1.sexo         = g_master1.sexo         CLIPPED
          LET g_master.sexo          = g_master.sexo          CLIPPED
          LET g_master1.fena         = g_master1.fena         CLIPPED
          LET g_master.fena          = g_master.fena          CLIPPED
          LET g_master1.nacionalidad = g_master1.nacionalidad CLIPPED
          LET g_master.nacionalidad  = g_master.nacionalidad  CLIPPED
          LET g_master1.tip_prob     = g_master1.tip_prob     CLIPPED
          LET g_master.tip_prob      = g_master.tip_prob      CLIPPED
          LET g_master1.fol_prob     = g_master1.fol_prob     CLIPPED
          LET g_master.fol_prob      = g_master.fol_prob      CLIPPED
          LET g_master1.doc_prob     = g_master1.doc_prob     CLIPPED
          LET g_master.doc_prob      = g_master.doc_prob      CLIPPED
          LET g_master1.n_rfc        = g_master1.n_rfc        CLIPPED
          LET g_master.n_rfc         = g_master.n_rfc         CLIPPED

          IF g_master1.paterno            = g_master.paterno            AND
             g_master1.materno            = g_master.materno            AND
             g_master1.nombres            = g_master.nombres            AND
             g_master1.sexo               = g_master.sexo               AND
             g_master1.fena               = g_master.fena               AND
             g_master1.salario_base_comis = g_master.salario_base_comis AND
             g_master1.cod_esq_comision   = g_master.cod_esq_comision   AND  
             g_master1.estadon            = g_master.estadon            AND
             g_master1.nacionalidad       = g_master.nacionalidad       AND
             g_master1.tip_prob           = g_master.tip_prob           AND
             g_master1.fol_prob           = g_master.fol_prob           AND
             g_master1.doc_prob           = g_master.doc_prob           AND
             g_master1.n_rfc              = g_master.n_rfc              THEN
---05 ene 2009 MOD CURP
{             IF g_master.cod_error_origen IS NOT NULL THEN
                CALL inserta_modificacion()    #im
             END IF}

---<05 ene 2009 MOD CURP

             IF g_master1.salario_base_comis <> g_master.salario_base_comis OR
                g_master1.cod_esq_comision   <> g_master.cod_esq_comision   OR  
                g_master1.ind_infonavit      <> g_master.ind_infonavit      OR
                g_master1.edo_civil          <> g_master.edo_civil          THEN

                UPDATE afi_mae_afiliado
                SET    salario_base_comis = g_master.salario_base_comis,
                       ind_infonavit      = g_master.ind_infonavit,
                       edo_civil          = g_master.edo_civil,
                       usuario            = g_usuario,
                       const_curp         = g_master.const_curp
                WHERE  n_seguro           = g_master.n_seguro
                AND    n_folio            = g_master.n_folio
                AND    tipo_solicitud     = g_master.tipo_solicitud

                IF SQLCA.SQLCODE = 0 THEN
                   SELECT @USER
                     INTO g_usuario
                     FROM systables
                    GROUP BY 1

                     INSERT INTO afi_mae_modifica
                     VALUES (g_master1.tipo_solicitud     ,
                             g_master1.n_folio            ,
                             g_master1.fecha_elaboracion  ,
                             g_master1.folio_edo_cta      ,
                             g_master1.cod_afore_ced      ,
                             g_master1.fecha_emision      ,
                             g_master1.frecafor           ,
                             g_master1.paterno            ,
                             g_master1.materno            ,
                             g_master1.nombres            ,
                             g_master1.n_seguro           ,
                             g_master1.n_rfc              ,
                             g_master1.n_unico            ,
                             g_master1.sexo               ,
                             g_master1.edo_civil          ,
                             g_master1.fena               ,
                             g_master1.salario_base_comis ,
                             g_master1.cod_esq_comision   ,
                             g_master1.estadon            ,
                             g_master1.nacionalidad       ,
                             g_master1.tip_prob           ,
                             g_master1.fol_prob           ,
                             g_master1.doc_prob           ,
                             g_master1.ind_infonavit      ,
                             --g_master1.cod_error_origen   ,    --05 ene 2009
                             ''                           ,      --05 ene 2009
                             g_master1.const_curp         ,
                             HOY                          ,
                             g_usuario                    ,
                             1                            ,
                             590                          ,
                             vstatusint                   ,
                             ''                           ,
                             ''                           ,
                             ''                           ,
                             vfolio_nvo                   ,
                             ''                           ,
                             f_reclama                    ,
                             id_reclama )

--->05 ene 2009 MOD CURP
                   INSERT INTO afi_ctr_curp
                   VALUES (g_master.n_seguro           ,
                           g_master.n_unico            ,
                           g_master.n_folio            ,
                           g_master.tipo_solicitud     ,
                           cadena                      ,
                           0                           ,--cve_afo_nss_asoc
                           ''                          ,--ident_tip_trab
                           ''                          ,--cod_operacion  
                           ''                          ,--diag_proceso   
                           ''                          ,--curp_oficial   
                           ''                          ,--fecha_envio    
                           ''                          ,--fecha_respuesta
                           '13'                        ,--cve_operacion  
                           g_usuario                   ,
                           HOY                         )--fecha_registro 
---<05 ene 2009 MOD CURP

                END IF

                LET operacion = 'MODIF. AFIL. DATOS NO CERTIFICABLES'
                LET su_st_int = su_estatus

                CALL inserta_logico(g_master.n_folio, g_master.tipo_solicitud,
                                    g_master.n_seguro, su_st_int, g_usuario,
                                    operacion)
             ELSE
               ---05 ene 2009 MOD CURP 
                {IF g_master.cod_error_origen IS NULL THEN
                   ERROR "NO HUBO MODIFICACIONES, EL REGISTRO ",
                         "NO FUE ACTUALIZADO"
                   SLEEP 2
                   ERROR " "
                END IF}
             END IF
          ELSE
             CALL inserta_modificacion()   #im
          END IF
       END IF

       CALL Inicializa()
 
       NEXT FIELD tipo_solicitud

    ON KEY ( CONTROL-E, CONTROL-T, CONTROL-B, CONTROL-V, CONTROL-N )
       IF g_master.n_seguro IS NULL THEN
          ERROR "Campo NO puede ser NULO"
           NEXT FIELD n_seguro
       END IF

       CALL valor_argumento()

       INITIALIZE COMMA TO NULL

       LET KEY = FGL_LASTKEY()

       CASE KEY
         WHEN 2
           LET COMMA = "fglgo AFIM014 "
         WHEN 5
           LET COMMA = "fglgo AFIM012 "
         WHEN 14
           LET COMMA = "fglgo AFIM016 "
         WHEN 20
           LET COMMA = "fglgo AFIM013 "
         WHEN 22
           LET COMMA = "fglgo AFIM015 "
       END CASE

       LET COMMA = COMMA CLIPPED," ", comm_arg CLIPPED
       RUN COMMA 

  END INPUT

END FUNCTION

FUNCTION inserta_modificacion()
#im----------------------------
  DEFINE 
    marca_ant      ,
    vcve_operacion SMALLINT

  SELECT @USER
    INTO g_usuario
    FROM systables
   GROUP BY 1

  LET su_st_int = 120
  LET operacion = 'MODIF. AFILIADO DATOS CERTIFICABLES'

  SELECT "X"
  FROM   afi_mae_modifica a
  WHERE  a.n_seguro       = g_master.n_seguro
  AND    a.n_folio        = g_master.n_folio
  AND    a.tipo_solicitud = g_master.tipo_solicitud
  AND    a.cod_operacion  = 0
  GROUP BY 1
  IF SQLCA.SQLCODE = 0 THEN
     DELETE FROM afi_mae_modifica
     WHERE  n_seguro = g_master.n_seguro
     AND    n_folio  = g_master.n_folio
     AND    tipo_solicitud = g_master.tipo_solicitud
     AND    cod_operacion  = 0
  END IF

  UPDATE afi_mae_afiliado 
  SET    status_interno = 120
  WHERE  n_seguro       = g_master.n_seguro
  AND    n_folio        = g_master.n_folio
  AND    tipo_solicitud = g_master.tipo_solicitud
  AND    status_interno <> 160

  SELECT @USER
  INTO   g_usuario
  FROM   tab_afore_local
  GROUP BY 1

    IF g_master.tipo_solicitud = 8 THEN
       LET vcve_operacion = 53 
    ELSE
       LET vcve_operacion = 13
    END IF

--->05 ene 2009 MOD CURP
    SELECT 'X'
    FROM   afi_ctr_curp a
    WHERE  a.nss            = g_master.n_seguro
    AND    a.n_folio        = g_master.n_folio
    AND    a.tipo_solicitud = g_master.tipo_solicitud
    AND    a.fecha_envio    IS NULL
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
       UPDATE afi_ctr_curp
       SET    cadena_dif     = cadena
       WHERE  nss            = g_master.n_seguro
       AND    n_folio        = g_master.n_folio
       AND    tipo_solicitud = g_master.tipo_solicitud
       AND    fecha_envio    IS NULL
    ELSE
       INSERT INTO afi_ctr_curp
       VALUES (g_master.n_seguro           ,
               g_master.n_unico            ,
               g_master.n_folio            ,
               g_master.tipo_solicitud     ,
               cadena                      ,
               0                           ,--cve_afo_nss_asoc
               ''                          ,--ident_tip_trab
               ''                          ,--cod_operacion  
               ''                          ,--diag_proceso   
               ''                          ,--curp_oficial   
               ''                          ,--fecha_envio    
               ''                          ,--fecha_respuesta
               --'13'                        ,--cve_operacion  
               vcve_operacion              ,--cve_operacion  
               g_usuario                   ,
               HOY                         )--fecha_registro 
    END IF
---<05 ene 2009 MOD CURP

  INSERT INTO afi_mae_modifica
  VALUES(g_master.tipo_solicitud     ,
         g_master.n_folio            ,
         g_master.fecha_elaboracion  ,
         g_master.folio_edo_cta      ,
         g_master.cod_afore_ced      ,
         g_master.fecha_emision      ,
         g_master.frecafor           ,
         g_master.paterno            ,
         g_master.materno            ,
         g_master.nombres            ,
         g_master.n_seguro           ,
         g_master.n_rfc              ,
         g_master.n_unico            ,
         g_master.sexo               ,
         g_master.edo_civil          ,
         g_master.fena               ,
         g_master.salario_base_comis ,
         g_master.cod_esq_comision   ,
         g_master.estadon            ,
         g_master.nacionalidad       ,
         g_master.tip_prob           ,
         g_master.fol_prob           ,
         g_master.doc_prob           ,
         g_master.ind_infonavit      ,
         --g_master.cod_error_origen   ,      --05 ene 2009
         ''                          ,        --05 ene 2009
         g_master.const_curp         ,
         HOY                         ,
         g_usuario                   ,
         0                           ,
         0                           ,
         120                         ,
         ''                          ,
         ''                          ,
         ''                          ,
         vfolio_nvo                  ,
         ''                          ,
         f_reclama                   ,
         id_reclama )
  IF SQLCA.SQLCODE = 0 THEN
     SELECT marca_cod
     INTO   marca_ant
     FROM   cta_act_marca
     WHERE  nss = g_master.n_seguro
     AND    marca_cod = 600

     IF marca_ant <> 600 THEN
        CALL marca_cuenta()
     END IF

     CALL inserta_logico(g_master.n_folio, g_master.tipo_solicitud,
                         g_master.n_seguro, su_st_int, g_usuario,
                         operacion)
     ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
  ELSE
     ERROR "REGISTRO CON DATOS ERRONEOS, NO SE HIZO MODIFICACION"
     SLEEP 2
     ERROR ""
  END IF

END FUNCTION

FUNCTION verifica_cod_error_orig()
#vceo-----------------------------

  IF g_master1.paterno <> g_master.paterno THEN
     LET paterno_2 = 1
  ELSE
     LET paterno_2 = 0
  END IF

  IF g_master1.materno <> g_master.materno THEN
     LET materno_3 = 2
  ELSE
     LET materno_3 = 0
  END IF

  IF g_master1.nombres <> g_master.nombres THEN
     LET nombre_4 = 4
  ELSE
     LET nombre_4 = 0
  END IF

  IF g_master1.sexo <> g_master.sexo THEN
     LET sexo_5 = 8
  ELSE
     LET sexo_5 = 0
  END IF

  IF g_master1.fena <> g_master.fena THEN
     LET macim_6 = 16
  ELSE
     LET macim_6 = 0
  END IF

  IF g_master1.estadon <> g_master.estadon THEN
     LET entidad_7 = 32
  ELSE
     LET entidad_7 = 0
  END IF

  IF g_master1.nacionalidad <> g_master.nacionalidad THEN
     LET nacion_8 = 64
  ELSE
     LET nacion_8 = 0
  END IF

  IF g_master1.tip_prob <> g_master.tip_prob THEN
     LET cve_prob_9 = 128
  ELSE
     LET cve_prob_9 = 0
  END IF

  IF g_master1.doc_prob <> g_master.doc_prob THEN
     LET doc_prob_10 = 256
  ELSE
     LET doc_prob_10 = 0
  END IF

  IF g_master1.fol_prob <> g_master.fol_prob THEN
     LET fol_prob_11 = 512
  ELSE
     LET fol_prob_11 = 0
  END IF

  LET tot_error = paterno_2   +
                  materno_3   +
                  nombre_4    +
                  sexo_5      +
                  macim_6     +
                  entidad_7   +
                  nacion_8    +
                  cve_prob_9  +
                  doc_prob_10 +
                  fol_prob_11 
  
  LET cod_error = tot_error USING "&&&&"

  IF cod_error IS NULL THEN
     LET cod_error = "0000"
  END IF

  IF cod_error <> "0000" THEN
     LET g_master.cod_error_origen = cod_error
     --DISPLAY BY NAME g_master.cod_error_origen     --05 ene 2009
  END IF

END FUNCTION

FUNCTION Desea_modificar()

  PROMPT "¿Desea Modificar la Informacion [S/N]? " FOR aux_pausa

END FUNCTION 

FUNCTION Despliega()
  DEFINE aux_pausa         CHAR(1)
  DEFINE txt               CHAR(300)
  DEFINE txt1              CHAR(300)
  DEFINE paterno           CHAR(50)
  DEFINE materno           CHAR(50)
  DEFINE nombres           CHAR(50)
  DEFINE cla_sel           CHAR(250)
  DEFINE n_busqueda        CHAR(100)
  DEFINE l_reg ARRAY[5000] OF RECORD
         n_seguro          CHAR(11),
         n_unico           CHAR(18),
         tipo_solicitud    SMALLINT,
         n_folio         DECIMAL(10,0),
         nombre            CHAR(50)
  END RECORD
  DEFINE i                 SMALLINT
  DEFINE HACER             SMALLINT
  DEFINE pat,mat,nom       CHAR(50)

  OPEN WINDOW v1 AT  4,4 WITH FORM "AFIM0012" ATTRIBUTE(BORDER)
  DISPLAY "          USTED PUEDE UTILIZAR EL * (ASTERISCO) COMO COMODIN                " AT 1,1 ATTRIBUTE(REVERSE)

  LET HACER    = TRUE
  LET INT_FLAG = TRUE

  CONSTRUCT BY NAME cla_sel ON paterno,materno,nombres

    ON KEY ( INTERRUPT )
       LET HACER = FALSE
       EXIT CONSTRUCT

    ON KEY ( ESC )
       LET INT_FLAG=FALSE
       EXIT CONSTRUCT

  END CONSTRUCT

  LET txt = "SELECT n_seguro,n_unico,tipo_solicitud,n_folio,paterno,materno,",
            "nombres FROM afi_mae_afiliado WHERE ",cla_sel CLIPPED,
            " ORDER BY 5,6" CLIPPED

  IF HACER THEN
     ERROR "Buscando Informacion"

     PREPARE cur1 FROM txt
     DECLARE cursor_1 cursor FOR cur1
     LET i = 1

     FOREACH cursor_1 INTO l_reg[i].n_seguro,
                           l_reg[i].n_unico,
                           l_reg[i].tipo_solicitud,
                           l_reg[i].n_folio,
                           pat,
                           mat,
                           nom

       LET l_reg[i].nombre = pat CLIPPED," ",
                              mat CLIPPED," ",
                              nom CLIPPED
       LET i = i + 1

       IF i >= 32000  THEN
          ERROR "Sobrepaso Capacidad Maxima del Arreglo"
          EXIT FOREACH
       END IF

     END FOREACH

     FREE cursor_1

     IF (i-1) < 1 THEN
         ERROR "ARCHIVO AFILIADOS..... VACIO"
     END IF

     CALL SET_COUNT(i-1)
     DISPLAY ARRAY l_reg TO scr_1.*

       ON KEY ( CONTROL-M )
          LET i = ARR_CURR()
          LET g_master.n_seguro       = l_reg[i].n_seguro
          LET g_master.n_unico        = l_reg[i].n_unico
          LET g_master.n_folio        = l_reg[i].n_folio
          LET g_master.tipo_solicitud = l_reg[i].tipo_solicitud
          EXIT DISPLAY

       ON KEY ( INTERRUPT )
          LET g_master.n_seguro       = NULL
          LET g_master.n_unico        = NULL
          LET g_master.n_folio        = NULL
          LET g_master.tipo_solicitud = NULL
          EXIT DISPLAY

     END DISPLAY
  END IF

  CLOSE WINDOW v1

END FUNCTION

FUNCTION rescata_status(valor)
#rs---------------------------
  DEFINE edo_desc   CHAR(50)
  DEFINE edo_act    SMALLINT
  DEFINE edo_cta    SMALLINT
  DEFINE edo_proc1  SMALLINT
  DEFINE x_unico    LIKE afi_mae_afiliado.n_unico
  DEFINE valor      SMALLINT
  DEFINE valor2     SMALLINT
  DEFINE l_estado   ,
         l_desc     CHAR(25),
         l_act      CHAR(25)
  DEFINE x_fecha    DATE
  DEFINE x_inicio   DATE
  DEFINE x_asigna   DATE
  DEFINE finit      DATE
  DEFINE x_1a_afi   DATE
  DEFINE f_marca    DATE

  LET edo_desc  = NULL
  LET l_estado  = NULL
  LET l_desc    = NULL
  LET finit     = NULL
  LET x_inicio  = NULL
  LET x_fecha   = NULL
  LET x_1a_afi  = NULL
  LET edo_cta   = 0
  LET edo_proc1 = 0
  LET vstatmod  = 0
  LET valor2    = valor

  SELECT @status_interno
    INTO valor
    FROM afi_mae_modifica
   WHERE @n_seguro      = g_master.n_seguro
     AND @cod_operacion = 0
  IF valor IS NULL THEN
     LET valor = valor2
  END IF
       
  SELECT tsa.estado_desc
  INTO   l_estado
  FROM   tab_status_afi tsa
  WHERE  tsa.estado_cod = valor

  DECLARE cur_marca CURSOR FOR
  SELECT c.marca_activa, c.rechazo_cod, a.fecha_ini, a.hora_ini
  FROM   cta_convivencia c, cta_act_marca a
  WHERE  c.marca_entra  = 600
  AND    c.rechazo_cod  > 0
  AND    a.nss          = g_master.n_seguro
  AND    c.marca_activa = a.marca_cod
  ORDER BY 3,4

  FOREACH cur_marca INTO edo_proc1, vstatmod, f_marca
    IF vstatmod > 0 THEN
       EXIT FOREACH
    END IF
  END FOREACH

  IF edo_proc1 IS NULL THEN
     LET edo_proc1 = 0
     LET l_desc    = NULL
  ELSE
     SELECT tm.marca_desc
     INTO   l_desc
     FROM   tab_marca tm
     WHERE  tm.marca_cod = edo_proc1
     AND    tm.marca_cod > 0
  END IF

  DECLARE cur_act CURSOR FOR
  SELECT a.marca_cod, a.fecha_ini, a.marca_causa
  FROM   cta_act_marca a
  WHERE  a.nss = g_master.n_seguro
  ORDER BY 2 

  FOREACH cur_act INTO edo_cta, f_marca, edo_act
    SELECT ta.marca_desc
    INTO   l_act
    FROM   tab_marca ta
    WHERE  ta.marca_cod = edo_act
    AND    ta.marca_cod > 0

    SELECT ma.marca_desc
    INTO   edo_desc
    FROM   tab_marca ma
    WHERE  ma.marca_cod = edo_cta
    AND    ma.marca_cod > 0

    IF edo_act > 0 THEN
       EXIT FOREACH
    END IF
  END FOREACH

  IF vstatmod IS NULL THEN
     LET vstatmod = 0
  END IF

  IF edo_cta = 5 THEN
     LET vstatmod = 2
  END IF

  IF vstatusint = 130 THEN
     LET vstatmod = 2
  END IF

  IF vstatusint = 160 THEN
     LET vstatmod = 1
  END IF

  IF edo_proc1 = edo_cta THEN
     LET edo_desc = ' '
  END IF

  DISPLAY l_estado CLIPPED," /",
          l_desc CLIPPED," / ", 
          edo_desc CLIPPED," ",
          l_act AT 6,2 ATTRIBUTE(REVERSE)

  SELECT fentcons, finicta, finitmte, fecha_1a_afil
  INTO   x_fecha, x_inicio , finit, x_1a_afi
  FROM   afi_mae_afiliado
  WHERE  n_seguro = g_master.n_seguro

  IF finit IS NOT NULL THEN
     LET edo_desc = edo_desc CLIPPED, " ASIG MISMA AFORE"
  END IF

  IF g_master.tipo_solicitud <> 5 THEN
     DISPLAY "F.Cert ",x_fecha USING "dd-mm-yyyy" AT 14,41 ATTRIBUTE(REVERSE)
  ELSE
     DISPLAY "F.Asig ",x_fecha USING "dd-mm-yyyy" AT 14,41 ATTRIBUTE(REVERSE)
  END IF

  DISPLAY "F.Ap.Cta ",x_inicio USING "dd-mm-yyyy" AT 14,20 ATTRIBUTE(REVERSE)

  IF finit IS NOT NULL THEN
     DISPLAY "F.Asig ",finit USING "dd-mm-yyyy" AT 14,1 ATTRIBUTE(REVERSE)
  END IF

  DISPLAY "F.1a.Af ", x_1a_afi USING "dd-mm-yyyy" AT 14,60 ATTRIBUTE(REVERSE)

END FUNCTION 

FUNCTION motivo_rechazo()
#mr----------------------
  DEFINE a ARRAY[100] OF RECORD
    codigo            SMALLINT,
    desc_cod          CHAR(10),
    diagnostico       CHAR(3),
    descripcion       CHAR(80),
    nombre            CHAR(50),
    fecha_rechazo     DATE
  END RECORD

  DEFINE sel_txt     CHAR(400)
  DEFINE mostrar     CHAR(400)

  DEFINE i           SMALLINT

  OPEN WINDOW v34  at 6,3 WITH FORM "AFIM0112" attribute(border)
  DISPLAY " [ Ctrl_c ] Salir " AT 1,1
  DISPLAY "                    MOTIVOS DE RECHAZO PROCESAR                                " AT 3,1 ATTRIBUTE(REVERSE)

  LET sel_txt = 'SELECT m.cod_operacion, ',
                ' "RECHAZO", ',
	        ' m.diag_proceso,  ',
                ' b.rdeta_desc_l, ',
                ' m.nombre_pcanase, ',
                ' m.fecha_modifica ',
                ' FROM   afi_mae_modifica m, tab_rdeta b ',
                ' WHERE  m.n_seguro = ', g_master.n_seguro ,
		' AND    m.tipo_solicitud = ', g_master.tipo_solicitud ,
                ' AND    m.cod_operacion = 2 ',
                ' AND    m.diag_proceso[1,3] = b.rdeta_cod ',
		' AND    b.modulo_cod = ','"afi"', 
                ' ORDER BY 6 desc ' CLIPPED

  PREPARE eje_txt FROM sel_txt
  DECLARE cursor_o CURSOR FOR eje_txt

  LET i = 1

  FOREACH cursor_o INTO a[i].*
    LET i = i + 1
  END FOREACH

  CALL SET_COUNT(i-1)
  DISPLAY ARRAY a TO scr_1.*

    ON KEY ( INTERRUPT )
       EXIT DISPLAY

  END DISPLAY

  CLOSE WINDOW v34

END FUNCTION

FUNCTION motivo_notifica()
#mr----------------------
  DEFINE a ARRAY[100] OF RECORD
    codigo            SMALLINT,
    desc_cod          CHAR(30),
    paterno           CHAR(40),
    materno           CHAR(40),
    nombres           CHAR(40),
    fecha_rechazo     DATE
  END RECORD

  DEFINE i            SMALLINT

  OPEN WINDOW v36  at 6,3 WITH FORM "AFIM0115" attribute(border)
  DISPLAY " [ Ctrl_c ] Salir " AT 1,1
  DISPLAY "                 NOTIFICACION NOMBRE MODIFICADO IMSS                           " AT 3,1 ATTRIBUTE(REVERSE)

  DECLARE cursor_n CURSOR FOR
  SELECT 610,
         'NOMBRE MODIFICADO IMSS',
	 m.paterno_proc, 
         m.materno_proc,
         m.nombres_proc,
         m.f_transf_lote
  FROM   afi_det_notifica m
  WHERE  m.n_seguro = g_master.n_seguro
  ORDER BY 6 desc

  LET i = 1

  FOREACH cursor_n INTO a[i].*
    LET i = i + 1
  END FOREACH

  CALL SET_COUNT(i-1)
  DISPLAY ARRAY a TO scr_1.*

    ON KEY ( INTERRUPT )
       EXIT DISPLAY

  END DISPLAY

  CLOSE WINDOW v36

END FUNCTION

FUNCTION Ingresa_autoriza()
#ia------------------------
  DEFINE cod               DECIMAL(10,0)
  DEFINE desc              CHAR(60)
  DEFINE l_reg ARRAY[1000] OF RECORD
    codigo                 CHAR(10),
    descripcion            CHAR(50)
  END RECORD 
  DEFINE pos               SMALLINT
  DEFINE x_buscar          CHAR(60)
  DEFINE x_texto           CHAR(200)

  LET aux_pausa = "N"

  OPEN WINDOW ventanilla_super AT 8,4 WITH FORM "AFIM0015" ATTRIBUTE(BORDER)
  DISPLAY "                      OPCION RESERVADA PARA SUPERVISORES                       " AT 3,1 ATTRIBUTE ( REVERSE)

  INPUT BY NAME r_cod.super_cod,r_cod.super_desc,r_cod.nip 

    AFTER FIELD super_cod
      IF r_cod.super_cod IS NULL THEN
         CALL Despliega_supervisores()
         RETURNING r_cod.super_cod, r_cod.super_desc

         SELECT super_cod,super_desc 
         INTO   r_cod.super_cod,r_cod.super_desc
         FROM   tab_supervisor
         WHERE  super_cod = r_cod.super_cod
         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "No existe codigo ... "
            NEXT FIELD super_cod
         END IF
      ELSE
         SELECT area_cod,super_desc 
         INTO   r_cod.area_cod,r_cod.super_desc
         FROM   tab_supervisor
         WHERE  super_cod = r_cod.super_cod
         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "CLAVE DE SUPERVISOR INVALIDA... " SLEEP 2
            NEXT FIELD super_cod
         END IF

         DISPLAY BY NAME r_cod.super_desc

      END IF

    AFTER FIELD nip
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD super_cod 
      END IF

      IF r_cod.nip IS NULL THEN
         LET aux_pausa = "S"
         ERROR "El NIP NO ES CORRECTO"
         NEXT FIELD nip
      END IF

      SELECT "X" 
      FROM   tab_supervisor
      WHERE  nip = r_cod.nip
      AND    super_cod = r_cod.super_cod 
      IF SQLCA.SQLCODE <> 0 THEN
         ERROR "Permiso denegado ... "
         LET aux_pausa = "S"
         NEXT FIELD nip
      ELSE
         LET aux_pausa = "N"
         EXIT INPUT
      END IF

    ON KEY ( INTERRUPT )
       LET aux_pausa = "S"
       EXIT INPUT

  END INPUT

  CLOSE WINDOW ventanilla_super

END FUNCTION

FUNCTION verifica_datos_modificados()
#vdm---------------------------------

  OPEN WINDOW ventana_dm AT 2,2 WITH FORM "AFIM0113" ATTRIBUTE(BORDER)

  DISPLAY " AFIM011                 MANTENIMIENTO  AFILIADOS                               " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY "                                                                                " AT 6,1 ATTRIBUTE(REVERSE)
  DISPLAY "                                                                                " AT 14,1 ATTRIBUTE(REVERSE)
  DISPLAY "               Nombre del Trabajador segun Documento Probatorio                 " AT 10,1 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

  CALL Rescata_datos_modificados() #rdm

  IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY g_master2 TO scr_1.*
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY
  END IF

  CLOSE WINDOW ventana_dm

END FUNCTION

FUNCTION Rescata_datos_modificados()
#rdm--------------------------------
  DECLARE cursor_2 CURSOR FOR
  SELECT a.codven,
         a.agenc_cod,
         b.n_folio,
         b.cod_operacion,
         b.frecafor,
         b.paterno,
         b.materno,
         b.nombres,
         b.n_seguro,
         b.n_unico,
         b.n_rfc,
         b.sexo,
         b.edo_civil,
         b.fena,
         b.salario_base_comis,
         b.estadon,
         b.tip_prob,
         c.docprob_desc,
         b.fol_prob,
         b.doc_prob,
         b.ind_infonavit,
         b.nacionalidad,
         b.tipo_solicitud,
         b.fecha_modifica,
         b.fecha_elaboracion,
         b.cod_error_origen,
         a.folio_edo_cta,
         a.cod_afore_ced, 
         b.const_curp,
         a.femision,
         b.folio_nvo
  FROM   afi_mae_afiliado a, afi_mae_modifica b, tab_doc_prob c
  WHERE  a.n_seguro = g_master.n_seguro
  AND    a.n_folio = g_master.n_folio
  AND    a.tipo_solicitud = g_master.tipo_solicitud
  AND    b.n_seguro = g_master.n_seguro
  AND    b.n_folio = g_master.n_folio
  AND    b.tipo_solicitud = g_master.tipo_solicitud
  AND    b.tip_prob = c.docprob_cod
  ORDER BY b.fecha_modifica DESC

  LET pos = 1

  FOREACH cursor_2 INTO g_master2[pos].codven,
                        g_master2[pos].agenc_cod,
                        g_master2[pos].n_folio,
 			g_master2[pos].cod_operacion,
                        g_master2[pos].frecafor,
                        g_master2[pos].paterno,
                        g_master2[pos].materno,
                        g_master2[pos].nombres,
                        g_master2[pos].n_seguro,
                        g_master2[pos].n_unico,
                        g_master2[pos].n_rfc,
                        g_master2[pos].sexo,
                        g_master2[pos].edo_civil,
                        g_master2[pos].fena,
                        g_master2[pos].salario_base_comis,
                        g_master2[pos].estadon,
                        g_master2[pos].tip_prob,
                        g_master2[pos].docprob_desc,
                        g_master2[pos].fol_prob,
                        g_master2[pos].doc_prob,
                        g_master2[pos].ind_infonavit,
                        g_master2[pos].nacionalidad,
                        g_master2[pos].tipo_solicitud,
                        g_master2[pos].fecha_modifica,
                        g_master2[pos].fecha_elaboracion,
                        g_master2[pos].cod_error_origen,
                        g_master2[pos].folio_edo_cta,
                        g_master2[pos].cod_afore_ced,
                        g_master2[pos].const_curp,
                        g_master2[pos].fecha_emision,
                        g_master2[pos].folio_nvo

    CASE g_master2[pos].cod_operacion
      WHEN 0  LET g_master2[pos].desc_cod_oper = 'EN PROCESO'
      WHEN 1  LET g_master2[pos].desc_cod_oper = 'ACEPTADO'
      WHEN 2  LET g_master2[pos].desc_cod_oper = 'RECHAZADO'
      WHEN 3  LET g_master2[pos].desc_cod_oper = 'ACEPTADO'
      WHEN 12 LET g_master2[pos].desc_cod_oper = 'MOD DOMIC'
    END CASE

    IF g_master2[pos].n_folio = g_master2[pos].folio_nvo THEN
       LET g_master2[pos].folio_nvo = ''
    END IF

    SELECT paterno,
           materno,
           nombres,
           nip 
    INTO   pat,
           mat,
           nom,
           g_master2[pos].nip
    FROM   pro_mae_promotor
    WHERE  cod_promotor = g_master2[pos].codven

    IF SQLCA.SQLCODE <> 0 THEN
       LET g_master2[pos].desc_codven = "NO EXISTE"
    ELSE
       LET g_master2[pos].desc_codven = pat CLIPPED," ",
                                        mat CLIPPED," ",
                                        nom CLIPPED
    END IF

    SELECT nombre_uni_n1 
    INTO   g_master2[pos].agenc_desc 
    FROM   com_nivel1
    WHERE  coduni_n1 = g_master2[pos].agenc_cod
    IF SQLCA.SQLCODE <> 0 THEN
       LET g_master2[pos].agenc_desc = "NO EXISTE"
    END IF

    SELECT ecivi_desc 
    INTO   g_master2[pos].desc_edo_civil 
    FROM   tab_edo_civil
    WHERE  ecivi_cod = g_master2[pos].edo_civil
    IF SQLCA.SQLCODE <> 0 THEN
       LET g_master2[pos].desc_edo_civil = "NO EXISTE"
    END IF

    SELECT sexo_desc 
    INTO   g_master2[pos].desc_sexo 
    FROM   tab_sexo
    WHERE  sexo_cod = g_master2[pos].sexo
    IF SQLCA.SQLCODE <> 0 THEN
       LET g_master2[pos].desc_sexo = "NO EXISTE"
    END IF

    SELECT ecivi_desc 
    INTO   g_master2[pos].desc_edo_civil 
    FROM   tab_edo_civil
    WHERE  ecivi_cod = g_master2[pos].edo_civil
    IF SQLCA.SQLCODE <> 0 THEN
       LET g_master2[pos].desc_edo_civil = "NO EXISTE"
    END IF

    SELECT estad_desc 
    INTO   g_master2[pos].desc_estadon
    FROM   tab_estado
    WHERE  estad_cod = g_master2[pos].estadon
    IF SQLCA.SQLCODE <> 0 THEN
       LET g_master2[pos].desc_estadon = "NO EXISTE"
    END IF

    SELECT pais_desc 
    INTO   g_master2[pos].desc_nacionalidad
    FROM   tab_pais
    WHERE  pais_cod = g_master2[pos].nacionalidad
    IF SQLCA.SQLCODE <> 0 THEN
       LET g_master2[pos].desc_nacionalidad = "NO EXISTE"
    END IF

    CASE g_master2[pos].ind_infonavit
      WHEN "N" LET g_master2[pos].ind_infonavit = 0    
      WHEN "S" LET g_master2[pos].ind_infonavit = 1
    END CASE

    SELECT @descripcion
     INTO g_master2[pos].desc_ind_info 
     FROM safre_af:tab_ind_cred
    WHERE @codigo = g_master2[pos].ind_infonavit
    IF STATUS = NOTFOUND THEN
       LET g_master2[pos].desc_ind_info = "NO EXISTE"
    END IF 

    LET pos = pos +1
  END FOREACH

END FUNCTION

FUNCTION Modifica_no_cert()
#--------------------------
  DEFINE xx_status    SMALLINT
  DEFINE rescato      SMALLINT
  DEFINE vfecha_modif DATE

  LET vfecha_modif = TODAY

  DISPLAY "" AT 1,1
  DISPLAY "" AT 2,1
  DISPLAY " MODIFICA NO CERT " AT 1,59 ATTRIBUTE(REVERSE)
  DISPLAY " [ Esc ] Modifica [ Ctrl-C ] Salir sin Modificar " 
  AT 1,1 ATTRIBUTE(CYAN)
  DISPLAY " CONTROL : [v] Otros Datos  [t] Datos modificados  [b] Despliega               " AT 2,1 ATTRIBUTE(CYAN)

  IF g_master.n_seguro IS NULL THEN
     CALL Inicializa()
  END IF

  LET sw_1 = 0 

  LET g_master.cod_error_origen = NULL

  INPUT BY NAME g_master.*
    BEFORE FIELD codven
      NEXT FIELD tipo_solicitud

    AFTER FIELD tipo_solicitud
      IF g_master.tipo_solicitud IS NULL THEN
         NEXT FIELD n_seguro
      END IF

--      IF g_master.tipo_solicitud < 1 OR
--         g_master.tipo_solicitud > 3 THEN
      IF g_master.tipo_solicitud < 1 OR
         g_master.tipo_solicitud = 5 THEN
         NEXT FIELD tipo_solicitud
      ELSE
         NEXT FIELD n_folio
      END IF

      CALL despliega_desc_solic(g_master.tipo_solicitud)

    AFTER FIELD n_folio
      IF g_master.n_folio IS NULL THEN
         NEXT FIELD n_folio
      ELSE
         IF NOT Rescata_datos("N",g_master.n_folio,
                              g_master.tipo_solicitud,"") THEN
            ERROR "Afiliado NO existe"
            NEXT FIELD n_folio
         ELSE
            LET g_master1.* = g_master.*
         END IF
      END IF

      DISPLAY BY NAME g_master.*

      NEXT FIELD edo_civil

    AFTER FIELD n_seguro
      IF g_master.n_seguro IS NULL THEN
         NEXT FIELD tipo_solicitud
      ELSE
         IF NOT Rescata_datos("C",g_master.n_seguro,0,"") THEN
            ERROR "Afiliado NO existe"
            NEXT FIELD n_seguro
         ELSE
            LET g_master1.* = g_master.*
         END IF
      END IF

      DISPLAY BY NAME g_master.*

      NEXT FIELD edo_civil

    AFTER FIELD edo_civil
      IF g_master.edo_civil IS NULL THEN
         CALL Despliega_estados_civiles()
         RETURNING g_master.edo_civil, g_master.desc_edo_civil
      ELSE
         SELECT ecivi_desc
         INTO   g_master.desc_edo_civil
         FROM   tab_edo_civil
         WHERE  ecivi_cod = g_master.edo_civil
         IF STATUS = NOTFOUND THEN
            ERROR "Estado Civil Inexistente"
            NEXT FIELD edo_civil
         END IF
      END IF

      DISPLAY BY NAME g_master.edo_civil,g_master.desc_edo_civil

      NEXT FIELD salario_base_comis

    AFTER FIELD salario_base_comis
      IF g_master.salario_base_comis IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD salario_base_comis
      END IF

      NEXT FIELD ind_infonavit

    AFTER FIELD ind_infonavit
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD doc_prob
      END IF

      IF g_master.ind_infonavit IS NULL OR
         g_master.ind_infonavit = ' '   THEN
         CALL Despliega_ind_info() RETURNING g_master.ind_infonavit,
                                             g_master.desc_ind_info
         IF g_master.ind_infonavit IS NULL OR
            g_master.ind_infonavit =  ' '  THEN
            ERROR "Se requiere la clave de prestamo del INFONAVIT"
            NEXT FIELD ind_infonavit
         END IF
      END IF

      IF g_master.ind_infonavit NOT MATCHES "[012]" THEN
         CALL Despliega_ind_info() RETURNING g_master.ind_infonavit,
                                             g_master.desc_ind_info 
         IF g_master.ind_infonavit NOT MATCHES "[012]" THEN
            ERROR "(0) Sin credito  (1) Credito infonavit  ", 
                  "(2) Credito 43 bis"
            NEXT FIELD ind_infonavit
         END IF
      ELSE
         SELECT @descripcion
           INTO g_master.desc_ind_info
           FROM safre_af:tab_ind_cred
          WHERE @codigo = g_master.ind_infonavit
         DISPLAY BY NAME g_master.desc_ind_info 
      END IF

    ON KEY ( CONTROL-P )
       SELECT "X"
       FROM   afi_mae_modifica md
       WHERE  md.n_seguro = g_master.n_seguro
       AND    md.n_folio = g_master.n_folio
       AND    md.tipo_solicitud = g_master.tipo_solicitud
       GROUP BY 1
       IF SQLCA.SQLCODE = 0 THEN
          CALL verifica_datos_modificados()
       ELSE
          PROMPT "Registro sin modificaciones, [Enter] p/continuar " 
          ATTRIBUTES (REVERSE) FOR enter
       END IF

    ON KEY ( CONTROL-G )
       CALL Despliega()

       DISPLAY BY NAME g_master.n_folio

    ON KEY ( INTERRUPT )
       CALL Inicializa()
       DISPLAY "                                                                          " AT 6,2  ATTRIBUTE(REVERSE)
       DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
       DISPLAY "                  " AT 1,59 
       EXIT INPUT

    ON KEY ( ESC )
       WHILE TRUE
          CALL Desea_modificar()

          IF aux_pausa MATCHES "[SsNn]" THEN
             EXIT WHILE
          END IF
       END WHILE

       IF aux_pausa MATCHES "[Nn]" THEN
          RETURN
       ELSE
          SELECT @USER
          INTO   g_usuario
          FROM   tab_afore_local
          GROUP BY 1

          INSERT INTO afi_mae_modifica
          VALUES (g_master1.tipo_solicitud     ,
                  g_master1.n_folio            ,
                  g_master1.fecha_elaboracion  ,
                  g_master1.folio_edo_cta      ,
                  g_master1.cod_afore_ced      ,
                  g_master1.fecha_emision      ,
                  g_master1.frecafor           ,
                  g_master1.paterno            ,
                  g_master1.materno            ,
                  g_master1.nombres            ,
                  g_master1.n_seguro           ,
                  g_master1.n_rfc              ,
                  g_master1.n_unico            ,
                  g_master1.sexo               ,
                  g_master1.edo_civil          ,
                  g_master1.fena               ,
                  g_master1.salario_base_comis ,
                  g_master1.cod_esq_comision   ,
                  g_master1.estadon            ,
                  g_master1.nacionalidad       ,
                  g_master1.tip_prob           ,
                  g_master1.fol_prob           ,
                  g_master1.doc_prob           ,
                  g_master1.ind_infonavit      ,
                  --g_master1.cod_error_origen   ,          --05 ene 2009
                  ''                           ,            --05 ene 2009
                  g_master1.const_curp         ,
                  HOY                          ,
                  g_usuario                    ,
                  1                            ,
                  590                          ,
                  vstatusint                   ,
                  ''                           ,
                  ''                           ,
                  ''                           ,
                  folio_nvo                    ,
                  ''                           ,
                  f_reclama                    ,
                  id_reclama)

          UPDATE afi_mae_afiliado
          SET    salario_base_comis = g_master.salario_base_comis,
                 ind_infonavit      = g_master.ind_infonavit,
                 edo_civil          = g_master.edo_civil,
                 usuario            = g_usuario
          WHERE  n_seguro           = g_master.n_seguro
          AND    n_folio            = g_master.n_folio
          AND    tipo_solicitud     = g_master.tipo_solicitud
       END IF

       CALL Inicializa()
       NEXT FIELD tipo_solicitud

    ON KEY ( CONTROL-E, CONTROL-T, CONTROL-B, CONTROL-V, CONTROL-N )
       IF g_master.n_seguro IS NULL THEN
          ERROR "Campo NO puede ser NULO"
          NEXT FIELD n_seguro
       END IF

       CALL valor_argumento()

       INITIALIZE COMMA TO NULL

       LET KEY = FGL_LASTKEY()

       CASE KEY
         WHEN 2  LET COMMA = "fglgo AFIM014 "
         WHEN 5  LET COMMA = "fglgo AFIM012 "
         WHEN 14 LET COMMA = "fglgo AFIM016 "
         WHEN 20 LET COMMA = "fglgo AFIM013 "
         WHEN 22 LET COMMA = "fglgo AFIM015 "
       END CASE

       LET COMMA = COMMA CLIPPED," ", comm_arg CLIPPED
       RUN COMMA 

  END INPUT

END FUNCTION

FUNCTION motivo_renapo()
#mr---------------------
  DEFINE a ARRAY[100] OF RECORD
    codigo            SMALLINT,
    descripcion       CHAR(80),
    fecha_asignacion  DATE,
    fecha_rechazo     DATE,
    paterno           CHAR(40),
    materno           CHAR(40),
    nombres           CHAR(40)
  END RECORD

  DEFINE i            SMALLINT

  OPEN WINDOW v35 AT 6,3 WITH FORM "AFIM0114" ATTRIBUTE(BORDER)
  DISPLAY " [ Ctrl_c ] Salir " AT 1,1
  DISPLAY "                    MOTIVOS DE RECHAZO RENAPO                                  " AT 3,1 ATTRIBUTE(REVERSE)

  DECLARE cursor_c CURSOR FOR
  SELECT m.status_renapo,
         d.status_desc,
         m.fecha_asignacion,
         n.fecha_rechazo,
         n.paterno,
         n.materno,
         n.nombres
  FROM   afi_dispersa_curp m, 
         tab_status_renapo d,
  OUTER  afi_status_tp5 n
  WHERE  m.n_seguro = g_master.n_seguro
  AND    m.n_seguro = n.n_seguro
  AND    m.status_renapo = n.status_renapo
  AND    m.fecha_actualiza = n.fecha_actualiza
  AND    d.status_cod = m.status_renapo 
  ORDER BY 4

  LET i = 1

  FOREACH cursor_c INTO a[i].*
    LET i = i + 1
  END FOREACH

  IF (i-1) >= 1 THEN
     CALL SET_COUNT(i-1)

     DISPLAY ARRAY a TO scr_1.*
        ON KEY (INTERRUPT)
           EXIT DISPLAY
     END DISPLAY
  ELSE
     PROMPT "Afiliado no tiene dispersiones de Renapo, [Enter] p/continuar"
     FOR enter 
     ATTRIBUTES(reverse) 
  END IF

  CLOSE WINDOW v35

END FUNCTION

FUNCTION marca_cuenta()
#mc--------------------
  DEFINE
    pmarca_causa SMALLINT

  LET pmarca_causa = 0

  LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                "'",g_master.n_seguro,"'",
                ",",edo_proc,
                ",",g_master.n_folio,
                ",",vmarca_estado,
                ",",vcodigo_rechazo,
                ",",pmarca_causa,
                ",","'","'", ",",
                "'",g_usuario,"'",")"

  LET ejecuta = ejecuta CLIPPED

  PREPARE clausula_spl FROM ejecuta
  DECLARE cursor_marca CURSOR FOR clausula_spl
  OPEN cursor_marca
    FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo
  CLOSE cursor_marca

END FUNCTION

{FUNCTION inserta_logico(st_int, desc_oper)
#il---------------------------------------
  DEFINE
    st_int    SMALLINT,
    desc_oper CHAR(40)

  LET g_hora  = TIME

  INSERT INTO safre_af:afi_ctr_logico
  VALUES (g_master.n_folio,
          g_master.tipo_solicitud,
          g_master.n_seguro,
          st_int,
          g_usuario,
          HOY,
          g_hora,
          desc_oper)

END FUNCTION}

FUNCTION Modifica_folio()
#mf----------------------
  DEFINE xx_status    SMALLINT
  DEFINE rescato      SMALLINT
  DEFINE bnd_fol      SMALLINT
  DEFINE folio_nvo    DECIMAL(8,0)
  DEFINE folio_ant    DECIMAL(8,0)
  DEFINE vfecha_modif DATE

  LET vfecha_modif = TODAY

  DISPLAY "" AT 1,1
  DISPLAY "" AT 2,1
  DISPLAY " MODIFICA FOLIO " AT 1,59 ATTRIBUTE(REVERSE)
  DISPLAY " [ Esc ] Modifica [ Ctrl-C ] Salir sin Modificar " 
  AT 1,1 ATTRIBUTE(CYAN)

  LET sw_1    = 0 
  LET bnd_fol = 0 

  INPUT BY NAME g_master.codven, g_master.tipo_solicitud,
                g_master.n_folio WITHOUT DEFAULTS

    BEFORE FIELD codven
      NEXT FIELD n_folio

    BEFORE FIELD n_folio
      LET folio_ant = g_master.n_folio

      ERROR "Ingrese el nuevo numero de folio "

    AFTER FIELD n_folio
      IF g_master.n_folio IS NULL THEN
         NEXT FIELD n_folio
      END IF

      LET folio_nvo        = g_master.n_folio
      LET g_master.n_folio = folio_ant

      IF g_master.tipo_solicitud = 1 THEN
         IF folio_nvo < 2900101 THEN 
            WHILE TRUE
               PROMPT "Folio esta vencido, ¿desea modificarlo [S/N]? "
               FOR enter

               IF enter MATCHES "[Ss]" THEN
                  LET bnd_fol = 1
                  EXIT WHILE
               ELSE
                  IF enter NOT MATCHES "[nN]" THEN
                     ERROR "Solo debe presionar (S)i o (N)o"
                     SLEEP 3
                     ERROR ""
                  ELSE
                     LET bnd_fol = 0
                     EXIT WHILE
                  END IF
               END IF
            END WHILE 
         ELSE
            LET bnd_fol = 1
         END IF                                        
      END IF                                        

      IF g_master.tipo_solicitud = 2 THEN
         IF folio_nvo < 205001 THEN 
            WHILE TRUE
               PROMPT "Folio esta vencido, ¿desea modificarlo [S/N]? "
               FOR enter

               IF enter MATCHES "[Ss]" THEN
                  LET bnd_fol = 1
                  EXIT WHILE
               ELSE
                  IF enter NOT MATCHES "[nN]" THEN
                     ERROR "Solo debe presionar (S)i o (N)o"
                     SLEEP 3
                     ERROR ""
                  ELSE
                     LET bnd_fol = 0
                     EXIT WHILE
                  END IF
               END IF
            END WHILE 
         ELSE
            LET bnd_fol = 1
         END IF                                        
      END IF                                        

      IF bnd_fol THEN
         SELECT "X"
         FROM   afi_mae_afiliado
         WHERE  n_folio = folio_nvo
         AND    tipo_solicitud = g_master.tipo_solicitud
         IF STATUS = NOTFOUND THEN
            SELECT "X"
            FROM   afi_solicitud
            WHERE  n_folio = folio_nvo
            AND    tipo_solicitud = g_master.tipo_solicitud
            IF STATUS = NOTFOUND THEN
               SELECT 'X'
               FROM   afi_mae_modifica
               WHERE  @folio_nvo      = folio_nvo
               AND    @tipo_solicitud = g_master.tipo_solicitud
               AND    @cod_operacion  = 0
               AND    @diag_proceso   = 0
               IF STATUS = NOTFOUND THEN
                  ERROR "Ingresando modificacion "
                    
                  SELECT 'X'
                  FROM   afi_mae_modifica
                  WHERE  n_seguro       = g_master.n_seguro
                  AND    cod_operacion  = 0
                  AND    status_interno = 120
                  IF SQLCA.SQLCODE = 0 THEN
                     SELECT @USER
                       INTO g_usuario
                       FROM systables
                      GROUP BY 1

                     UPDATE afi_mae_modifica
                     SET    afi_mae_modifica.folio_nvo = folio_nvo
                     WHERE  n_seguro       = g_master.n_seguro
                     AND    cod_operacion  = 0
                     AND    status_interno = 120
                  ELSE
                     SELECT  @USER
                       INTO  g_usuario
                       FROM  tab_afore_local
                     GROUP BY 1

                     INSERT INTO afi_mae_modifica
                     VALUES(g_master.tipo_solicitud     ,
                            g_master.n_folio            ,
                            g_master.fecha_elaboracion  ,
                            g_master.folio_edo_cta      ,
                            g_master.cod_afore_ced      ,
                            g_master.fecha_emision      ,
                            g_master.frecafor           ,
                            g_master.paterno            ,
                            g_master.materno            ,
                            g_master.nombres            ,
                            g_master.n_seguro           ,
                            g_master.n_rfc              ,
                            g_master.n_unico            ,
                            g_master.sexo               ,
                            g_master.edo_civil          ,
                            g_master.fena               ,
                            g_master.salario_base_comis ,
                            g_master.cod_esq_comision   ,
                            g_master.estadon            ,
                            g_master.nacionalidad       ,
                            g_master.tip_prob           ,
                            g_master.fol_prob           ,
                            g_master.doc_prob           ,
                            g_master.ind_infonavit      ,
                            --g_master.cod_error_origen   ,        --05 ene 2009
                            ''                          ,          --05 ene 2009
                            g_master.const_curp         ,
                            HOY                         ,
                            g_usuario                   ,
                            0                           ,
                            0                           ,
                            120                         ,
                            ''                          ,
                            ''                          ,
                            ''                          ,
                            folio_nvo                   ,
                            ''                          ,
                            f_reclama                   ,
                            id_reclama   )

                     UPDATE afi_mae_afiliado
                     SET    status_interno = 120
                     WHERE  n_seguro = g_master.n_seguro
                     AND    status_interno <> 160
                  END IF

                  LET operacion = 'MODIFICA FOLIO VIGENTE'

                  CALL inserta_logico(g_master.n_folio,
                                      g_master.tipo_solicitud,
                                      g_master.n_seguro, su_estatus, 
                                      g_usuario, operacion)

                  ERROR ""

                  PROMPT "Nuevo folio a certificar es :  ", folio_nvo,
                         ", Presione [Enter] para finalizar " 
                  ATTRIBUTES (REVERSE) FOR enter

                  CALL Inicializa()

                  NEXT FIELD tipo_solicitud

                  EXIT INPUT
               ELSE
                  ERROR "Folio ya ingresado anteriormente"
                  SLEEP 3
                  EXIT INPUT
               END IF 
            ELSE
               ERROR "Folio ya ingresado anteriormente"
               SLEEP 3
               EXIT INPUT
            END IF
         ELSE
            ERROR "Folio ya ingresado anteriormente "
            SLEEP 3
            EXIT INPUT
         END IF
      ELSE
         CALL Inicializa()
         DISPLAY "                                                                          " AT 6,2  ATTRIBUTE(REVERSE)
         DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
         DISPLAY "                  " AT 1,59 
         EXIT INPUT
      END IF

    ON KEY ( INTERRUPT )
       CALL Inicializa()
       DISPLAY "                                                                          " AT 6,2  ATTRIBUTE(REVERSE)
       DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
       DISPLAY "                  " AT 1,59 
       EXIT INPUT

  END INPUT

END FUNCTION

FUNCTION valida_anyo_nac()
#van----------------------
  DEFINE sino SMALLINT

  CASE g_master.tip_prob
    WHEN 1
      LET r_nac = g_master.doc_prob[6,7]
    WHEN 2
      LET r_nac = g_master.doc_prob[9,10]
  END CASE

  LET a_nac = xn_fena[9,10]

  IF r_nac < a_nac THEN
     WHILE TRUE
        PROMPT "Hay inconsistencias entre anyo reg./anyo nac., ",
               "es correcto ¿[S/N]? " FOR enter
        IF enter MATCHES "[Ss]" THEN
           LET b_nac = 1
           LET sino  = 0
           EXIT WHILE
        ELSE
           IF enter NOT MATCHES "[nN]" THEN
              ERROR "Solo debe presionar (S)i o (N)o"
              SLEEP 3
              ERROR ""
           ELSE
              LET sino = 1
              EXIT WHILE
           END IF
        END IF
     END WHILE 
  END IF

  RETURN sino

END FUNCTION

FUNCTION despliega_curp()
#di----------------------

  DEFINE rec_1 ARRAY[200] OF RECORD
    n_unico        CHAR(18),
    n_seguro       CHAR(11),
    n_folio        CHAR(8),
    tipo_solicitud SMALLINT,
    estado         CHAR(15),
    fentcons       CHAR(10)
  END RECORD

  DEFINE
    idx        SMALLINT,
    seguro_cnt SMALLINT,
    array_sz   SMALLINT,
    over_size  SMALLINT,
    i          SMALLINT,
    xx_num     INTEGER

  LET array_sz = 200

  OPEN WINDOW AFIM00171 AT 7,4 WITH FORM "AFIM00171" ATTRIBUTE(BORDER)
  DISPLAY "                              [CTRL-C] Salir                                   " AT 1,1 ATTRIBUTE(REVERSE)

  LET over_size = FALSE
  LET seguro_cnt = 1

  DECLARE cur_err CURSOR FOR
  SELECT a.n_unico,
         a.n_seguro,
         a.n_folio,
         a.tipo_solicitud,
         b.desc_solicitud,
         a.fentcons
  FROM   afi_mae_afiliado a,
         tab_tipo_solic b
  WHERE  a.n_unico        = g_master.n_unico
  AND    a.tipo_solicitud = b.tipo_solicitud

  FOREACH cur_err INTO rec_1[seguro_cnt].n_unico,
                       rec_1[seguro_cnt].n_seguro,
                       rec_1[seguro_cnt].n_folio,
                       rec_1[seguro_cnt].tipo_solicitud,
                       rec_1[seguro_cnt].estado,
                       rec_1[seguro_cnt].fentcons

    LET seguro_cnt = seguro_cnt + 1

    IF seguro_cnt > array_sz THEN
       LET over_size = TRUE
       EXIT FOREACH
    END IF
  END FOREACH

  IF (seguro_cnt = 1) THEN
     LET idx = 1
     LET rec_1[idx].n_seguro = NULL
  ELSE
     IF over_size THEN
        MESSAGE "Manuf array full: can only display ",array_sz USING "<<<<"
     END IF
  END IF

  CALL SET_COUNT(seguro_cnt-1)

  LET int_flag = FALSE
  DISPLAY ARRAY rec_1 TO rec.*

     ON KEY ( CONTROL-M )
        LET i = ARR_CURR()
        LET g_master.n_folio        = rec_1[i].n_folio
        LET g_master.tipo_solicitud = rec_1[i].tipo_solicitud
        EXIT DISPLAY

  END DISPLAY

  LET idx = ARR_CURR()

  IF int_flag THEN
     LET int_flag = FALSE
     LET rec_1[idx].n_seguro = NULL
  END IF

     
  CLOSE WINDOW AFIM00171

END FUNCTION

FUNCTION Modifica_No_Afil()
#MNA---------------

  DEFINE vfecha_modif  DATE
  DEFINE vcurp_arma    CHAR(18)
  DEFINE v_1           SMALLINT
  DEFINE val_1         CHAR(80)
  DEFINE i             SMALLINT
  DEFINE vhomocve      CHAR(1)
  DEFINE vrfc_orig     CHAR(4)
  DEFINE vrfc_si       CHAR(4)
  DEFINE tot_afil      SMALLINT
  DEFINE vcurp_valida  CHAR(18),
          sexo_cur     CHAR(1),
          a_yo_act     ,
          a_yo_fena    ,
          a_yo         SMALLINT

  LET vfecha_modif = TODAY

  DISPLAY "" AT 1,1
  DISPLAY "" AT 2,1
  DISPLAY " MODIFICA NO AF" AT 1,62 ATTRIBUTE(REVERSE)
  DISPLAY " [ Esc ] Modifica [ Ctrl-C ] Salir sin Modificar " AT 1,1
  ATTRIBUTE(BOLD)

  DISPLAY " CONTROL : [v] Otros Datos  [t] Datos modificados  [b] Despliega               " AT 2,1 ATTRIBUTE(BOLD)

  IF g_master.n_seguro IS NULL THEN
     CALL Inicializa()
  END IF

  LET g_master.cod_error_origen = NULL
  LET g_master.tipo_solicitud   = NULL
  LET g_master.cod_afore_ced    = NULL
  LET g_master.frecafor         = NULL
  LET g_master.fecha_elaboracion= NULL
  LET g_master.fena             = NULL
  LET g_master.sexo             = NULL
  LET g_master.fecha_emision    = NULL
  LET g_master.edo_civil        = NULL
  #LET g_master.n_operac         = NULL
  LET g_master.const_curp       = NULL
  LET g_master.estadon          = NULL
  LET g_master.nip              = NULL
  LET g_master1.tipo_solicitud  = NULL

  LET v_1                       = 0
  LET val_1                     = NULL
  LET vcurp_arma                = NULL

  INPUT BY NAME g_master.* WITHOUT DEFAULTS
    BEFORE FIELD codven
      NEXT FIELD tipo_solicitud

    AFTER FIELD tipo_solicitud
      IF g_master.tipo_solicitud IS NULL THEN
         NEXT FIELD n_seguro
      END IF

      IF g_master.tipo_solicitud = 5 THEN
         NEXT FIELD tipo_solicitud
      ELSE
         NEXT FIELD n_folio
      END IF

      CALL despliega_desc_solic(g_master.tipo_solicitud)

    AFTER FIELD n_folio
      IF g_master.n_folio IS NULL THEN
         NEXT FIELD n_seguro
      ELSE
         IF NOT Rescata_datos("N",g_master.n_folio,
                              g_master.tipo_solicitud,"") THEN
            ERROR "Afiliado NO existe"
            NEXT FIELD n_folio
         ELSE
            IF vstatmod > 1 THEN
               DISPLAY BY NAME g_master.*

               PROMPT "REGISTRO NO SE PUEDE MODIFICAR, PRESIONE ENTER"
               ATTRIBUTE(reverse) FOR enter
               CALL Inicializa()
               NEXT FIELD tipo_solicitud
            END IF
         END IF


      END IF

      DISPLAY BY NAME g_master.*

      NEXT FIELD n_seguro

    AFTER FIELD n_seguro
      IF (g_master.n_seguro IS NULL) AND
         (g_master.n_seguro[1] = ' ') THEN
         NEXT FIELD n_unico
      ELSE
         IF NOT Rescata_datos("C",g_master.n_seguro,0,"") THEN
            ERROR "Afiliado NO existe"
            NEXT FIELD n_unico
         ELSE
            IF g_master.tipo_solicitud = 5 THEN
               PROMPT "TIPO SOLICITUD NO SE PUEDE MODIFICAR, PRESIONE ENTER"
               ATTRIBUTE(reverse) FOR enter
               CALL Inicializa()
               NEXT FIELD tipo_solicitud
            END IF

            IF vstatmod > 1 THEN
               DISPLAY BY NAME g_master.*

               PROMPT "REGISTRO NO SE PUEDE MODIFICAR, PRESIONE ENTER"
               ATTRIBUTE(reverse) FOR enter
               CALL Inicializa()
               NEXT FIELD tipo_solicitud
            END IF

         END IF
      END IF

      DISPLAY BY NAME g_master.*
--->erm 04 Mayo 2006
       #NEXT FIELD nss_issste
       NEXT FIELD n_rfc

        --IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
         --  FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
          -- NEXT FIELD nss_issste
       -- END IF

      {AFTER FIELD nss_issste

       IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
          NEXT FIELD sexo
       END IF

       --IF tipo_solic = 8 THEN
          IF g_master.nss_issste IS NOT NULL OR
             g_master.nss_issste <> " " THEN

          LET nss_issste = g_master.nss_issste

             IF LENGTH(g_master.nss_issste) <> 11 THEN
                ERROR "NSS ISSSTE debe tener 11 digitos"
                SLEEP 2
                NEXT FIELD nss_issste
             ELSE
                LET arr_nssissste[01].nssissste_pos = nss_issste[01]  
                LET arr_nssissste[02].nssissste_pos = nss_issste[02]
                LET arr_nssissste[03].nssissste_pos = nss_issste[03]  
                LET arr_nssissste[04].nssissste_pos = nss_issste[04]
                LET arr_nssissste[05].nssissste_pos = nss_issste[05]  
                LET arr_nssissste[06].nssissste_pos = nss_issste[06]
                LET arr_nssissste[07].nssissste_pos = nss_issste[07]  
                LET arr_nssissste[08].nssissste_pos = nss_issste[08]
                LET arr_nssissste[09].nssissste_pos = nss_issste[09]  
                LET arr_nssissste[10].nssissste_pos = nss_issste[10]
                LET arr_nssissste[11].nssissste_pos = nss_issste[11]

                ### INICIALIZA ARREGLO CON VALORES NUMERICOS
                LET q = 0
                FOR q = 1 TO 9
                  LET arr_nume[q].num = q
                END FOR
                LET arr_nume[10].num = 0

                ### Valida nss
                LET i         = 0
                LET q         = 0
                LET contador1 = 0
                LET desp_err  = 0

                FOR i = 1 TO 11

                   ### Valida numeros (Pos 1 a 11)
                   IF i >= 1 AND i <= 11 THEN
                      FOR q = 1 TO 10
                         IF arr_nssissste[i].nssissste_pos = arr_nume[q].num THEN
                            LET contador1 = contador1 + 1
                         END IF
                      END FOR
                   END IF

                END FOR

                IF desp_err = 0 THEN
                   IF contador1 < 11 THEN
                      ERROR "Caracteres NO validos en las posiciones del nss"
                      NEXT FIELD nss_issste
                   END IF
                END IF
             END IF

          ELSE}
             {ERROR "NSS ISSSTE no puede ser nulo..."
             SLEEP 2
             ERROR ""
             NEXT FIELD nss_issste}
          {END IF}
       --END IF

          IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
             FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
             NEXT FIELD n_rfc
          END IF

---<erm 04 Mayo 2006

{
    AFTER FIELD n_unico
      IF g_master.n_unico IS NULL THEN
         NEXT FIELD tipo_solicitud
      ELSE
         SELECT COUNT(*)
         INTO   tot_afil
         FROM   afi_mae_afiliado a
         WHERE  a.n_unico = g_master.n_unico

         IF tot_afil > 1 THEN
            CALL despliega_curp()

             IF NOT Rescata_datos("N",
                                 g_master.n_folio,
                                 g_master.tipo_solicitud,
                                 "") THEN
               ERROR "Afiliado NO existe"
               NEXT FIELD n_folio
            END IF

            DISPLAY BY NAME g_master.*
         ELSE
            IF NOT Rescata_datos("U",g_master.n_seguro,0,g_master.n_unico) THEN
               ERROR "Afiliado NO existe"
               NEXT FIELD n_unico
            END IF

            DISPLAY BY NAME g_master.*
         END IF
      END IF
}
      NEXT FIELD paterno

    AFTER FIELD paterno
      IF g_master.paterno IS NULL OR
         g_master.paterno =  " "  THEN
         ERROR "APELLIDO PATERNO NO PUEDE SER NULO "
         ATTRIBUTE (REVERSE)
         SLEEP 2
         ERROR " "
         NEXT FIELD paterno
      ELSE
         LET v_1 = 0
         INITIALIZE val_1 TO NULL
         CALL verifica_nombre(g_master.paterno) RETURNING v_1,val_1 #ve--
         IF v_1 = 1 THEN
            ERROR "A.Paterno ",val_1 CLIPPED
            NEXT FIELD paterno
         END IF
      END IF

      NEXT FIELD materno

    AFTER FIELD materno
      IF g_master.materno[1] = " " THEN
         ERROR "Ingrese Ap. Materno correcto o deje el campo nulo"
         LET g_master.materno = NULL
         DISPLAY BY NAME g_master.materno
         NEXT FIELD materno
      END IF

      IF g_master.materno != " " OR
         g_master.materno IS NOT NULL THEN
         LET v_1 = 0
         INITIALIZE val_1 TO NULL
         CALL verifica_nombre(g_master.materno) RETURNING v_1,val_1 #ve--
         IF v_1 = 1 THEN
            ERROR "A.Materno ",val_1 CLIPPED
            NEXT FIELD materno
         END IF
      END IF

      NEXT FIELD nombres

    AFTER FIELD nombres
      IF g_master.nombres IS NULL OR
         g_master.nombres[1] =  " "  THEN
         ERROR "EL NOMBRE NO PUEDE SER NULO " ATTRIBUTE (REVERSE)
         SLEEP 2
         ERROR " "
         NEXT FIELD nombres
      ELSE
         LET v_1 = 0
         INITIALIZE val_1 TO NULL
         CALL verifica_nombre(g_master.nombres) RETURNING v_1,val_1 #ve--
         IF v_1 = 1 THEN
            ERROR "El Nombre ",val_1 CLIPPED
            NEXT FIELD nombres
         END IF
      END IF

      #NEXT FIELD nss_issste            ---erm 05 Mayo 2006

      NEXT FIELD n_rfc

    AFTER FIELD n_rfc
      IF g_master.n_rfc IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD n_rfc
      ELSE
         LET v_1 = 0
         INITIALIZE val_1 TO NULL
         CALL verifica_rfc(g_master.n_rfc[1,4])
         RETURNING v_1,val_1 ##ve--
         IF v_1 = 1 THEN
            ERROR "R.F.C ",val_1 CLIPPED
            NEXT FIELD n_rfc
         END IF

         CALL valida_est_rfc(g_master.n_rfc)
         RETURNING  pasa_curp, desc_err

         IF pasa_curp = 1 THEN
            ERROR "", desc_err
            LET pasa_curp = 0
            NEXT FIELD n_rfc
         END IF
      END IF

      IF LENGTH(g_master.n_rfc) <> 10 AND
         LENGTH(g_master.n_rfc) <> 13 THEN
         ERROR "Debe ingresar R.F.C. completo"
         NEXT FIELD n_rfc
      END IF

      IF LENGTH(g_master.n_rfc) = 13 THEN
         IF g_master.n_rfc[11,13] <> "   " THEN
            LET v_1 = 0
            INITIALIZE val_1 TO NULL

            FOR i = 11 TO 13
                LET vhomocve = g_master.n_rfc[i]

                IF vhomocve = " " THEN
                   ERROR "RFC tiene espacios en blanco"
                   NEXT FIELD n_rfc
                END IF

                IF vhomocve <> "1" AND
                   vhomocve <> "2" AND
                   vhomocve <> "3" AND
                   vhomocve <> "4" AND
                   vhomocve <> "5" AND
                   vhomocve <> "6" AND
                   vhomocve <> "7" AND
                   vhomocve <> "8" AND
                   vhomocve <> "9" AND
                   vhomocve <> "0" THEN
                    CALL verifica_rfc(vhomocve)
                    RETURNING v_1,val_1 ##ve--
                    IF v_1 = 1 THEN
                       ERROR "R.F.C ",val_1 CLIPPED
                       NEXT FIELD n_rfc
                    END IF
                END IF
            END FOR
         END IF
      END IF

      CALL arma_clave_rfc(g_master.paterno,
                          g_master.materno,
                          g_master.nombres,
                          g_master.fena) RETURNING cve_arma #rac

      LET vcurp_arma = cve_arma CLIPPED

      CALL arma_curp(g_master.paterno,
                     g_master.materno,
                     g_master.nombres,
                     g_master.sexo,
                     g_master.estadon,
                     g_master.n_unico,
                     vcurp_arma) RETURNING vcurp_valida #ac

      CALL var_dig_curp(vcurp_valida) RETURNING pasa, dig_curp
      IF pasa = 0 THEN
         LET vcurp_valida = vcurp_valida[1,17] CLIPPED, dig_curp
      END IF

       IF vcurp_valida <> g_master.n_unico THEN
          ERROR "El Cambio altera la CURP, CURP propuesta: ", vcurp_valida
          --NEXT FIELD paterno
            WHILE TRUE
               PROMPT "¿DESEA REEMPLAZAR LA CURP CAPTURADA POR LA CURP PROPUESTA [S/N]? "
               FOR enter

               IF enter MATCHES "[Ss]" THEN
                  LET g_master.n_unico = vcurp_valida CLIPPED
                  DISPLAY BY NAME g_master.n_unico
                  EXIT WHILE
               ELSE
                  IF enter NOT MATCHES "[nN]" THEN
                     ERROR "Solo debe presionar (S)i o (N)o"
                     SLEEP 3
                     ERROR ""
                  ELSE
                     EXIT WHILE
                  END IF
               END IF
            END WHILE 
       END IF



      IF cve_arma[1,4] != g_master.n_rfc[1,4] THEN
         LET cve_arma = cve_arma[1,4]
         ERROR "El inicio del rfc debe ser: ", cve_arma

         WHILE TRUE
            PROMPT "¿Dejar inicio de rfc encontrado [S/N]? " FOR enter
            IF enter MATCHES "[Ss/Nn]" THEN
               IF enter MATCHES "[Ss]" THEN
                  LET g_master.n_rfc = cve_arma CLIPPED, g_master.n_rfc[5,13]
                  DISPLAY BY NAME g_master.n_rfc
                  EXIT WHILE
               ELSE
                  LET vrfc_orig = g_master.n_rfc[1,4]

                  SELECT @palabra_si
                    INTO vrfc_si
                    FROM afi_no_conviene
                   WHERE @palabra_no = vrfc_orig

                  IF SQLCA.SQLCODE = 0 THEN
                     LET g_master.n_rfc = vrfc_si, g_master.n_rfc[5,13]
                     DISPLAY BY NAME g_master.n_rfc
                  END IF
                  EXIT WHILE
               END IF
            END IF
         END WHILE
      END IF

      IF LENGTH(g_master.n_rfc) <> 10 AND
         LENGTH(g_master.n_rfc) <> 13 THEN
         ERROR "Debe ingresar R.F.C. completo"
         NEXT FIELD n_rfc
      END IF

      IF NOT valida_fecha_rfc(g_master.n_rfc[5,10]) THEN
         ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
         NEXT FIELD n_rfc
      ELSE
         WHENEVER ERROR CONTINUE
           LET aaa     = g_master.n_rfc[5,6]
           LET mm      = g_master.n_rfc[7,8]
           LET dd      = g_master.n_rfc[9,10]
           LET z_fecha = mm,"/",dd,"/19",aaa
           LET j_fecha = z_fecha
         WHENEVER ERROR STOP

         IF j_fecha IS NULL THEN
            ERROR "fecha Invalida en RFC"
            NEXT FIELD n_rfc
         END IF
      END IF

-->inicia modificacion req cpl-565 25-05-2011
      --NEXT FIELD paterno

      NEXT FIELD n_unico
    AFTER FIELD n_unico
      IF g_master.n_unico = "                  " OR
         g_master.n_unico IS NULL THEN
         ERROR "Campo CURP es obligatorio "
         NEXT FIELD n_unico
      ELSE
         IF LENGTH(g_master.n_unico) <> 18 OR
            g_master.n_unico[1] = " " OR
            g_master.n_unico IS NULL THEN
            ERROR "Debe ingresar CURP completa"

            LET g_master.n_unico = g_master1.n_unico
         
            NEXT FIELD n_unico
         ELSE

            IF g_master.n_unico[1] <> " " OR
               g_master.n_unico IS NOT NULL THEN
                IF g_master.n_unico[11] = "H" THEN
                    LET sexo_cur = "1"
                ELSE
                    LET sexo_cur = "2"
                END IF

                CALL valida_est_curp(g_master.n_unico)
                RETURNING pasa_curp, desc_err

                IF pasa_curp = 1 THEN
                   ERROR "", desc_err
                   LET pasa_curp = 0
                   NEXT FIELD n_unico
                END IF

                CALL var_dig_curp(g_master.n_unico) RETURNING pasa, dig_curp

                IF pasa = 0 THEN
                  ERROR "Digito Verificador Invalido curp, el digito es : ",
                  dig_curp
                  LET pasa = 0
                  NEXT FIELD n_unico
                END IF
            ELSE
                LET sexo_cur = " "
            END IF
         --END IF

            IF sexo_cur = '1' OR sexo_cur = '2' THEN
              IF sexo_cur <> g_master.sexo THEN
                WHILE TRUE
                    PROMPT "Sexo diferente en CURP y sexo, es correcto ¿[S/N]? "
                    FOR enter
                    IF enter MATCHES "[Ss/Nn]" THEN
                        IF enter MATCHES "[Ss]" THEN
                            NEXT FIELD estadon
                            EXIT WHILE
                        ELSE
                            NEXT FIELD sexo
                        END IF
                    ELSE
                        ERROR "Solo debe presionar (S) Si o (N) No"
                        SLEEP 3
                        ERROR ""
                    END IF
                END WHILE
              END IF
            END IF
         END IF
      END IF


      NEXT FIELD sexo
    AFTER FIELD sexo
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         IF g_master.sexo IS NULL OR g_master.sexo = " " OR
            g_master.sexo = 0  THEN
            ERROR "Digite correctamente el sexo antes de pasar a otro campo"
            NEXT FIELD sexo
         END IF
         NEXT FIELD n_rfc
      END IF

      IF g_master.sexo IS NULL OR g_master.sexo = " " OR
         g_master.sexo = 0  THEN
         CALL Despliega_sexos() RETURNING g_master.sexo,
                                          g_master.desc_sexo
      ELSE
         SELECT sexo_desc
           INTO g_master.desc_sexo
           FROM tab_sexo
          WHERE sexo_cod = g_master.sexo

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "Sexo Inexistente"
            NEXT FIELD sexo
         END IF
      END IF

      DISPLAY BY NAME g_master.sexo,g_master.desc_sexo

      NEXT FIELD edo_civil

    AFTER FIELD edo_civil
      IF g_master.edo_civil IS NULL THEN
         CALL Despliega_estados_civiles() 
         RETURNING g_master.edo_civil, g_master.desc_edo_civil
      ELSE
         SELECT ecivi_desc
         INTO g_master.desc_edo_civil
         FROM tab_edo_civil
         WHERE ecivi_cod = g_master.edo_civil

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "Estado Civil Inexistente"
            NEXT FIELD edo_civil
         END IF
      END IF

      DISPLAY BY NAME g_master.edo_civil,g_master.desc_edo_civil


      NEXT FIELD fena
 
    BEFORE FIELD fena
      LET x_fecha = g_master.n_rfc[7,8],"/",
                    g_master.n_rfc[9,10],"/",
                    "19",g_master.n_rfc[5,6]

      LET xx_fecha = x_fecha

    AFTER FIELD fena
      IF g_master.fena IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD fena
      END IF

      IF g_master.fena >= TODAY THEN
         ERROR "La fecha de nacimiento -NO- debe ser",
               "igual o mayor al dia de hoy"
         NEXT FIELD fena
      END IF

      LET a_yo_act  = 0 LET a_yo_fena = 0   LET a_yo = 0

      LET a_yo_act  = YEAR(TODAY) USING "&&&&"
      LET a_yo_fena = YEAR(g_master.fena) USING "&&&&"
      LET a_yo      = a_yo_act - a_yo_fena

      IF a_yo > 120 THEN
         ERROR "Esta persona pasa del rango de 120 ayos, Verifique nuevamente"
         NEXT FIELD fena
      END IF

      IF xx_fecha <> g_master.fena THEN
         WHILE TRUE
            PROMPT "Existen inconsistencias entre RFC/Fecha nacimiento, ",
                   "es correcto ¨[S/N]? " FOR enter
            IF enter MATCHES "[Ss/Nn]" THEN
               IF enter MATCHES "[Ss]" THEN
                  NEXT FIELD estadon
                  EXIT WHILE
               ELSE
                  NEXT FIELD n_rfc
               END IF
            ELSE
               ERROR "Solo debe presionar (S)i o (N)o"
               SLEEP 3
               ERROR ""
            END IF
         END WHILE
      END IF 

      NEXT FIELD estadon
    AFTER FIELD estadon
      IF g_master.estadon IS NULL OR
         g_master.estadon =  0    THEN
         CALL Despliega_estados() RETURNING g_master.estadon,
                                            g_master.desc_estadon

         IF g_master.estadon = 0 THEN
            NEXT FIELD estadon 
         END IF
      ELSE
         SELECT estad_desc 
         INTO   g_master.desc_estadon 
         FROM   tab_estado
         WHERE  estad_cod = g_master.estadon

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "Entidad de Nacimiento Inexistente"
            NEXT FIELD estadon
         END IF
      END IF

      DISPLAY BY NAME g_master.estadon,g_master.desc_estadon

      NEXT FIELD paterno
--<termina cpl-565

    ON KEY ( CONTROL-T )
       SELECT "X"
       FROM   afi_mae_modifica md
       WHERE  md.n_seguro       = g_master.n_seguro
       AND    md.n_folio        = g_master.n_folio
       AND    md.tipo_solicitud = g_master.tipo_solicitud
       GROUP BY 1

       IF SQLCA.SQLCODE = 0 THEN
          CALL verifica_datos_modificados()
       ELSE
          PROMPT "Registro sin modificaciones, [Enter] p/continuar "
          ATTRIBUTES (REVERSE) FOR enter
       END IF

    ON KEY ( CONTROL-B )
       CALL Despliega()
       DISPLAY BY NAME g_master.n_folio
    ON KEY ( INTERRUPT )
       CALL Inicializa()
       DISPLAY "                                                                               " AT 6,2  ATTRIBUTE(REVERSE)
       DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
       DISPLAY "                  " AT 1,62
       EXIT INPUT

    ON KEY ( ESC )
       IF g_master.tipo_solicitud <> 8  AND 
          g_master.tipo_solicitud <> 12 THEN
          ERROR "Tipo de solicitud no puede modificarse"
          RETURN
       END IF
       CALL arma_clave_rfc(g_master.paterno,
                           g_master.materno,
                           g_master.nombres,
                           g_master.fena) RETURNING cve_arma #rac

       LET vcurp_arma = cve_arma CLIPPED

       CALL arma_curp(g_master.paterno,
                      g_master.materno,
                      g_master.nombres,
                      g_master.sexo,
                      g_master.estadon,
                      g_master.n_unico,
                      vcurp_arma) RETURNING vcurp_valida #ac

       CALL var_dig_curp(vcurp_valida) RETURNING pasa, dig_curp
       IF pasa = 0 THEN
          LET vcurp_valida = vcurp_valida[1,17] CLIPPED, dig_curp
       END IF

       IF vcurp_valida <> g_master.n_unico THEN
          ERROR "El Cambio altera la CURP, CURP propuesta: ", vcurp_valida
          --NEXT FIELD paterno
            WHILE TRUE
               PROMPT "¿DESEA REEMPLAZAR LA CURP CAPTURADA POR LA CURP PROPUESTA [S/N]? "
               FOR enter

               IF enter MATCHES "[Ss]" THEN
                  LET g_master.n_unico = vcurp_valida CLIPPED
                  EXIT WHILE
               ELSE
                  IF enter NOT MATCHES "[nN]" THEN
                     ERROR "Solo debe presionar (S)i o (N)o"
                     SLEEP 3
                     ERROR ""
                  ELSE
                     EXIT WHILE
                  END IF
               END IF
            END WHILE 
       END IF

       CALL inserta_modificacion()    #im
       CALL Inicializa()
       NEXT FIELD tipo_solicitud

    ON KEY ( CONTROL-V )
       LET COMMA = "fglgo AFIM012.4gi ",
                    g_master.n_folio,
                    " ",
                    ACCION,
                    " ",
                    vcont," ",
                    g_master.tipo_solicitud
       RUN COMMA

  END INPUT

END FUNCTION
