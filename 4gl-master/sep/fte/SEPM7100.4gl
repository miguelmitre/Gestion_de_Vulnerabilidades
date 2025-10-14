DATABASE safre_af

GLOBALS
  DEFINE g_respuesta           SMALLINT
  DEFINE g_usuario_cod         CHAR(20)
  DEFINE cve_afore_local       CHAR(3)
  DEFINE c_mensaje             CHAR(1000)
  DEFINE xcodigo               INTEGER
  DEFINE r_seg_modulo          RECORD LIKE seg_modulo.*
  DEFINE t_bitacora            SMALLINT
  DEFINE t_txt_bitacora        CHAR(80)
  DEFINE
         g_enter               CHAR(001) ,
         g_ejecuta             CHAR(100)

  DEFINE band                  ,
         band_c                SMALLINT

  DEFINE hoy                   DATE

  DEFINE cont_1                INTEGER

  DEFINE arr_parejas ARRAY[5000] OF RECORD 
         arc_1             smallint ,
         correlativo       dec(10,0),
         n_seguro          char(11),
         nss               char(11) ,
         diagnostico       char(2) ,
         clasificacion     char(1) ,
         estado            smallint ,
         des_estado        char(30)
   END RECORD

   DEFINE txt_nss              CHAR(1000) ,
         txt_count             CHAR(1000) ,
         cad_construct         CHAR(0050) ,
         x_nss                 CHAR(0011)


   DEFINE arc_1                 ,
          total_pa              ,
          pos                   ,
          tot_folio_ts          INTEGER

  DEFINE g_idsolicitudseparacion DEC(10,0);
  DEFINE g_idbitacorawsbajacurp  DEC(10,0);

END GLOBALS

MAIN

 OPTIONS INPUT WRAP,
 PROMPT LINE LAST  ,
 ACCEPT KEY CONTROL-I
 DEFER INTERRUPT

 CALL STARTLOG("SEPF500.log")
 LET hoy = TODAY

 CALL inicio()

 OPEN WINDOW ventana_parejas AT 2,2 WITH FORM "SEPM71001" ATTRIBUTE(BORDER)

    DISPLAY "                                                                                 " AT 1,1
    DISPLAY "                                                                                 " AT 2,1
    DISPLAY "  ESC Aceptar  Ctrl-c Salir                                                      " AT 1,1

    DISPLAY " SEPM7100      SOLICITUDES DE BAJA CURP SEPARACION DE CUENTAS                  " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    CONSTRUCT  cad_construct ON a.n_seguro ,
                                a.nss      
                           FROM n_seguro,
                                nss

     ON KEY(ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
    ON KEY(INTERRUPT)
       LET band_c = 0
       EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT int_flag THEN

          LET txt_nss =
          " SELECT unique  '',a.correlativo          , ",
                           " a.n_seguro              , ",
                           " a.nss                   , ",
                           " b.diag_confronta        , ",
                           " b.clasifica_separacion  , ",
                           " a.estado                , ",
                           " c.des_estado              ",
          " FROM sep_det_reg_sol_reclamante a , ",
               " sep_det_solicitud          b , ",
               " sep_estado_separacion      c   ",
          " WHERE ",cad_construct CLIPPED ,
          " AND a.correlativo = b.idsolicitudseparacion ",
          " AND a.estado      = c.estado ",
          " ORDER BY a.correlativo, a.estado,a.n_seguro "

         WHILE TRUE

           PREPARE qry_parejas FROM txt_nss
           DECLARE cur_parejas CURSOR FOR qry_parejas

           LET cont_1 = 1

           FOREACH cur_parejas INTO arr_parejas[cont_1].*
                   LET arr_parejas[cont_1].arc_1 = cont_1
                   LET cont_1 = cont_1 + 1
           END FOREACH

           IF cont_1 = 1 THEN
              ERROR ""
              CLEAR FORM
              PROMPT " No Existen Registros...<ENTER> para Salir "
              ATTRIBUTE(REVERSE) FOR CHAR g_enter
              CLOSE WINDOW ventana_parejas
              EXIT WHILE
           ELSE
              LET txt_count =
              " SELECT COUNT(*) ",
              " FROM sep_det_reg_sol_reclamante a , ",
                   " sep_det_solicitud          b , ",
                   " sep_estado_separacion      c   ",
              " WHERE ",cad_construct CLIPPED ,
              " AND a.correlativo = b.idsolicitudseparacion ",
              " AND a.estado      = c.estado "

             PREPARE qry_txt_count FROM txt_count
             EXECUTE qry_txt_count INTO tot_folio_ts
             LET total_pa = cont_1 - 1
             DISPLAY BY NAME total_pa
             DISPLAY BY NAME tot_folio_ts

             CALL SET_COUNT(cont_1-1)

             LET arc_1 = 0

             DISPLAY " Ctrl-i Captura Sol                                            Ctrl-c Salir  " AT 1,1

             DISPLAY ARRAY arr_parejas TO  scr_sep.*

                ON KEY (CONTROL-I)    -- captura solicitud de baja

                   LET  pos = ARR_CURR()
                   CALL fn_captura_baja(arr_parejas[pos].correlativo,
                                        arr_parejas[pos].n_seguro,
                                        arr_parejas[pos].nss )

                ON KEY (INTERRUPT)
                   LET band = 0
                   EXIT DISPLAY
             END DISPLAY

             IF band = 0 THEN
                PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR "
                FOR CHAR g_enter
                CLOSE WINDOW ventana_parejas
                EXIT WHILE
             END IF
           END IF

       END WHILE

      ELSE
              CLOSE WINDOW ventana_parejas
      END IF

END MAIN

#-----------------------------
FUNCTION inicio()
#-----------------------------
   SELECT *
   INTO  r_seg_modulo.*
   FROM seg_modulo
   WHERE modulo_cod = "sep"

   LET g_usuario_cod = FGL_GETENV("USER")

   SELECT codigo_afore 
   INTO cve_afore_local
   FROM tab_afore_local 

END FUNCTION


#-------------------------------------------------
FUNCTION fn_captura_baja(v_corr,v_n_seguro,v_nss)
#-------------------------------------------------
    DEFINE v_txt_qry               CHAR(3000)
    DEFINE v_corr                  DEC(10,0) ,
           v_n_seguro              CHAR(11)  ,
           v_nss                   CHAR(11)  ,
           v_curp_invadido_origen  CHAR(18)  ,
           v_curp_asociado_origen  CHAR(18)  ,
           v_curp_invadido_final   CHAR(18)  ,
           v_curp_asociado_final   CHAR(18)

    DEFINE l_bajacurp SMALLINT

    DEFINE r_bitacora      RECORD 
           origensolicitudREQ        CHAR(2)
          ,cveentidadorigenREQ       CHAR(3)
          ,idnegocioinvadidoREQ      CHAR(11)
          ,curptrabajadorinvadidoREQ CHAR(18)
          ,appaternonssinvadidoREQ   CHAR(40)
          ,apmaternonssinvadidoREQ   CHAR(40)
          ,nombretrabnssinvadidoREQ  CHAR(40)
          ,menoresnssinvadidoREQ     CHAR(1)
          ,idnegocioasociadoREQ      CHAR(11)
          ,curptrabajadorasociadoREQ CHAR(18)
          ,appaternonssasociadoREQ   CHAR(40)
          ,apmaternonssasociadoREQ   CHAR(40)
          ,nombretrabnssasociadoREQ  CHAR(40)
          ,menoresnssasociadoREQ     CHAR(1)
          ,bajacurpREQ               CHAR(1)
    END RECORD
    DEFINE r_bitacora_response  RECORD
           resultadooperacion char(2) ,
           diagnosticoproceso char(3)
    END RECORD
    DEFINE v_idbitacora        DEC(10,0)
    DEFINE v_tpo_baja_descripcion CHAR(30)
    DEFINE v_desc_menor_inv  CHAR(30)
    DEFINE v_desc_menor_asc  CHAR(30)

    DEFINE r_solBajaCurp   RECORD     
           idsolBajaCurp            DEC(10,0)    ,
           idSolicitudSeparacion    DEC(10,0)    ,
           tpo_baja                 SMALLINT     ,  --phantom
           ind_capturaCurp          SMALLINT     ,  --phantom
           desc_ind_capturaCurp     VARCHAR(30)  ,       
           f_captura                DATE         ,
           nss_invadido             CHAR(11)     ,
           curp_invadido_origen     CHAR(18)     ,
           menoresNssInvadido       SMALLINT     ,
           nss_asociado             CHAR(11)     ,
           curp_asociado_origen     CHAR(18)     ,
           menoresNssAsociado       SMALLINT     ,
           curp_invadido_final      CHAR(18)     ,
           curp_asociado_final      CHAR(18)     ,   --phantom
           estado                   SMALLINT     ,
           desc_estado              VARCHAR(35)  ,
           resultado_operacion_sol  CHAR(2)      ,
           desc_resultado_operacion VARCHAR(20),
           diagnostico              CHAR(3)       ,
           desc_diagnostico         VARCHAR(150)
    END RECORD
    DEFINE b_ind_cap                SMALLINT

             SELECT n_unico 
             INTO   v_curp_invadido_origen  
             FROM   afi_mae_afiliado 
             WHERE  n_seguro = v_n_seguro

             SELECT n_unico 
             INTO   v_curp_asociado_origen  
             FROM   afi_mae_afiliado 
             WHERE  n_seguro = v_nss

    OPEN WINDOW ventana_captura AT 2,2 WITH FORM "SEPM71002" ATTRIBUTE(BORDER)

    DISPLAY " [ESC]Guardar  [Ctrl-b]Envia Solicitud [Ctrl-e]Recupera Response             " AT 1,1
    DISPLAY " [Ctrl-c]Salir [Ctrl-f]Bitacora                                              " AT 2,1

    DISPLAY " SEPM7100   CAPTURA DE SOLICITUD DE BAJA X SEPARACION DE CUENTAS                 " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
   
    INPUT r_solBajaCurp.tpo_baja,
          r_solBajaCurp.curp_invadido_origen,
          r_solBajaCurp.curp_asociado_origen  FROM tpo_baja ,
                                                  curp_invadido_origen ,
                                                  curp_asociado_origen
      BEFORE INPUT 

        LET t_bitacora = 0
        LET t_txt_bitacora = " "
        SELECT COUNT(*) 
        INTO  t_bitacora 
        FROM sep_bitacora_wsbajacurp
        WHERE estado = 3
        AND   idsolicitudseparacion = v_corr

        IF t_bitacora is null THEN LET t_bitacora = 0 END IF

        IF t_bitacora > 0 THEN 
           LET t_txt_bitacora =  "CONSULTAR EN BITACORA SOLICITUDES PROCESADAS"
        ELSE 
           LET t_txt_bitacora = "SIN SOLICITUDES PROCESADAS"
        END IF

        DISPLAY BY NAME t_bitacora
        DISPLAY BY NAME t_txt_bitacora

        SELECT "OK" 
        FROM   sep_sol_baja_curp 
        WHERE idSolicitudSeparacion = v_corr
        AND    estado in (10,20,30,40)

        IF STATUS <> NOTFOUND THEN
          LET v_txt_qry = 
          ' SELECT a.idsolbajacurp, a.idSolicitudSeparacion           , ',
                 ' a.tpo_baja                                         , ',
                 ' a.ind_capturaCurp                                  , ',
                 --' b.ind_capturaCurp||" "||b.ind_captura_descripcion  , ',
                 ' b.ind_captura_descripcion  , ',
                 ' a.f_captura                                        , ',
                 ' a.nss_invadido                                     , ',
                 ' a.curp_invadido_origen                             , ',
                 ' a.menoresNssInvadido                               , ',
                 ' a.nss_asociado                                     , ',
                 ' a.curp_asociado_origen                             , ',
                 ' a.menoresNssAsociado                               , ',
                 ' a.curp_invadido_final                              , ',
                 ' a.curp_asociado_final                              , ',
                 ' a.estado                                           , ',
                 --' c.estado||" "||c.desc_estado                       , ',
                 ' c.desc_estado                       , ',
                 ' a.resultado_operacion                              , ',
                 --' d.resultado_operacion||" "||d.desc_resultado_operacion , ',
                 ' d.desc_resultado_operacion , ',
                 ' a.diagnostico                                      , ',
                 ' e.diagnostico||" "||e.desc_diagnostico ',
          ' FROM sep_sol_baja_curp        a               , ',
               ' sep_ind_capturaCurp      b               , ',
               ' sep_estado_sol_baja_curp c               , ',
               ' OUTER sep_cat_resultado_operacion d      , ',
               ' OUTER sep_cat_diag_bajaCurp       e ',
          ' WHERE a.idSolicitudSeparacion = ',v_corr ,
          ' AND   a.ind_capturaCurp       = b.ind_capturaCurp       ',          
          ' AND   a.estado                = c.estado                ',
          ' AND   a.resultado_operacion   = d.resultado_operacion   ',          
          ' AND   a.diagnostico           = e.diagnostico           '

          LET v_txt_qry = v_txt_qry CLIPPED

          PREPARE prp_qry FROM v_txt_qry
          EXECUTE prp_qry INTO r_solBajaCurp.*

          DISPLAY BY NAME r_solBajaCurp.tpo_baja
               SELECT tpo_baja_descripcion 
               INTO   v_tpo_baja_descripcion
               FROM   sep_tpo_baja_curp 
               WHERE  tpo_baja = r_solBajaCurp.tpo_baja
          DISPLAY BY NAME v_tpo_baja_descripcion
          DISPLAY BY NAME r_solBajaCurp.ind_capturaCurp
          DISPLAY BY NAME r_solBajaCurp.desc_ind_capturaCurp
          DISPLAY BY NAME r_solBajaCurp.f_captura
          DISPLAY BY NAME r_solBajaCurp.nss_invadido ATTRIBUTE (REVERSE)
          DISPLAY BY NAME r_solBajaCurp.curp_invadido_origen
          DISPLAY BY NAME r_solBajaCurp.nss_asociado ATTRIBUTE (REVERSE)
          DISPLAY BY NAME r_solBajaCurp.curp_asociado_origen
          DISPLAY BY NAME r_solBajaCurp.curp_invadido_final
          DISPLAY BY NAME r_solBajaCurp.curp_asociado_final
          DISPLAY BY NAME r_solBajaCurp.estado
          DISPLAY BY NAME r_solBajaCurp.desc_estado
          DISPLAY BY NAME r_solBajaCurp.resultado_operacion_sol
          DISPLAY BY NAME r_solBajaCurp.desc_resultado_operacion
          DISPLAY BY NAME r_solBajaCurp.diagnostico
          DISPLAY BY NAME r_solBajaCurp.desc_diagnostico

          DISPLAY BY NAME r_solBajaCurp.menoresNssInvadido
               SELECT tpo_menor_descripcion 
               INTO   v_desc_menor_inv 
               FROM   sep_tpo_menores 
               WHERE  tpo_menor = r_solBajaCurp.menoresNssInvadido
          DISPLAY BY NAME v_desc_menor_inv

          DISPLAY BY NAME r_solBajaCurp.menoresNssAsociado
               SELECT tpo_menor_descripcion 
               INTO   v_desc_menor_asc
               FROM   sep_tpo_menores 
               WHERE  tpo_menor = r_solBajaCurp.menoresNssAsociado
          DISPLAY BY NAME v_desc_menor_asc
     
        ELSE 

          LET r_solBajaCurp.idSolicitudSeparacion = v_corr
          LET r_solBajaCurp.f_captura             = hoy          
          LET r_solBajaCurp.nss_invadido          = v_n_seguro
          LET r_solBajaCurp.curp_invadido_origen  = v_curp_invadido_origen
          LET r_solBajaCurp.nss_asociado          = v_nss
          LET r_solBajaCurp.curp_asociado_origen  = v_curp_asociado_origen
          LET r_solBajaCurp.estado                = 0

          DISPLAY BY NAME r_solBajaCurp.f_captura
          DISPLAY BY NAME r_solBajaCurp.nss_invadido
          DISPLAY BY NAME r_solBajaCurp.curp_invadido_origen
          DISPLAY BY NAME r_solBajaCurp.nss_asociado
          DISPLAY BY NAME r_solBajaCurp.curp_asociado_origen

        END IF 

      AFTER FIELD tpo_baja

               SELECT tpo_baja_descripcion 
               INTO   v_tpo_baja_descripcion
               FROM   sep_tpo_baja_curp 
               WHERE  tpo_baja = r_solBajaCurp.tpo_baja
               DISPLAY BY NAME v_tpo_baja_descripcion

               LET r_solBajaCurp.menoresNssInvadido = ""
               LET r_solBajaCurp.menoresNssAsociado  = ""
               LET v_desc_menor_inv = ""
               LET v_desc_menor_asc = ""

               DISPLAY BY NAME r_solBajaCurp.menoresNssInvadido
               DISPLAY BY NAME r_solBajaCurp.menoresNssAsociado 
               DISPLAY BY NAME v_desc_menor_inv 
               DISPLAY BY NAME v_desc_menor_asc
      
               LET r_solBajaCurp.curp_invadido_final = ""
               LET r_solBajaCurp.curp_asociado_final = ""
               DISPLAY BY NAME r_solBajaCurp.curp_invadido_final 
               DISPLAY BY NAME r_solBajaCurp.curp_asociado_final 



               CASE r_solBajaCurp.tpo_baja 
                   WHEN 1 
                        IF LENGTH(v_curp_invadido_origen) = 18 THEN 
                           LET r_solBajaCurp.ind_capturaCurp      = 1
                           LET r_solBajaCurp.desc_ind_capturaCurp = "CURP Invadido Existe"

                           DISPLAY BY NAME r_solBajaCurp.ind_capturaCurp
                           DISPLAY BY NAME r_solBajaCurp.desc_ind_capturacurp

                           INITIALIZE r_solBajaCurp.curp_invadido_final TO NULL
                           DISPLAY BY NAME r_solBajaCurp.curp_invadido_final 

                           LET r_solBajaCurp.curp_asociado_final = r_solBajaCurp.curp_asociado_origen
                           DISPLAY BY NAME r_solBajaCurp.curp_asociado_final

                           LET b_ind_cap = 0 --sin capturar curp asociado final

                        ELSE                           

                           LET r_solBajaCurp.ind_capturaCurp      = 2 
                           LET r_solBajaCurp.desc_ind_capturaCurp = "CURP Invadido No Existe"
                           DISPLAY BY NAME r_solBajaCurp.ind_capturacurp
                           DISPLAY BY NAME r_solBajaCurp.desc_ind_capturacurp

                           ERROR "CURP Invadido No Existe, Capturar Curp Invadido a dar de Baja"
                           SLEEP 2
                           ERROR ""
                           INITIALIZE r_solBajaCurp.curp_invadido_final TO NULL
                           DISPLAY BY NAME r_solBajaCurp.curp_invadido_final 

                           LET r_solBajaCurp.curp_asociado_final = r_solBajaCurp.curp_asociado_origen
                           DISPLAY BY NAME r_solBajaCurp.curp_asociado_final

                           LET b_ind_cap = 1 --capturar curp asociado final  
                           NEXT FIELD curp_invadido_origen

                        END IF                           

                        CALL pregunta("CURP INVADIDO RELACIONADA A MENOR DE EDAD S/N: ") RETURNING g_respuesta
                        IF g_respuesta THEN
                           CALL pregunta(" MENOR PERTENECE AL PROPIETARIO DE LA CURP? S/N: ") RETURNING g_respuesta
                           IF g_respuesta THEN
                              LET r_solBajaCurp.menoresNssInvadido = 1 
                           ELSE
                              LET r_solBajaCurp.menoresNssInvadido = 2
                           END IF
                        ELSE
                              LET r_solBajaCurp.menoresNssInvadido = "" 
                        END IF 

                        LET r_solBajaCurp.menoresNssAsociado = ""

                        DISPLAY BY NAME r_solBajaCurp.menoresNssInvadido
                           SELECT tpo_menor_descripcion 
                           INTO   v_desc_menor_inv 
                           FROM   sep_tpo_menores 
                           WHERE  tpo_menor = r_solBajaCurp.menoresNssInvadido
                           DISPLAY BY NAME v_desc_menor_inv

                        DISPLAY BY NAME r_solBajaCurp.menoresNssAsociado
                           SELECT tpo_menor_descripcion 
                           INTO   v_desc_menor_asc
                           FROM   sep_tpo_menores 
                           WHERE  tpo_menor = r_solBajaCurp.menoresNssAsociado
                           DISPLAY BY NAME v_desc_menor_asc

                  WHEN 2
                        IF LENGTH(v_curp_asociado_origen) = 18 THEN
                           LET r_solBajaCurp.ind_capturaCurp  = 3 
                           LET r_solBajaCurp.desc_ind_capturaCurp = "CURP Asociado Existe"

                           #--CPL-3875 LET r_solBajaCurp.curp_invadido_final  = v_curp_asociado_origen

                           DISPLAY BY NAME r_solBajaCurp.ind_capturacurp
                           DISPLAY BY NAME r_solBajaCurp.desc_ind_capturacurp

                           LET b_ind_cap = 2 --sin capturar curp invadido final

                           LET r_solBajaCurp.curp_invadido_final = r_solBajaCurp.curp_invadido_origen
                           DISPLAY BY NAME r_solBajaCurp.curp_invadido_final

                           INITIALIZE r_solBajaCurp.curp_asociado_final TO NULL
                           DISPLAY BY NAME r_solBajaCurp.curp_asociado_final
                        ELSE                           
                           LET r_solBajaCurp.ind_capturaCurp  = 4 
                           LET r_solBajaCurp.desc_ind_capturaCurp = "CURP Asociado No Existe"

                           ERROR "CURP Asociado no Existe, Capturar Curp Asociado a dar de Baja"
                           SLEEP 2
                           ERROR ""

                           LET b_ind_cap = 3 --sin capturar curp asociado final

                           LET r_solBajaCurp.curp_invadido_final = r_solBajaCurp.curp_invadido_origen
                           DISPLAY BY NAME r_solBajaCurp.curp_invadido_final

                           INITIALIZE r_solBajaCurp.curp_asociado_final TO NULL
                           DISPLAY BY NAME r_solBajaCurp.curp_asociado_final

                           NEXT FIELD curp_asociado_origen

                        END IF                           

                        CALL pregunta("CURP ASOCIADA RELACIONADA A MENOR DE EDAD S/N: ") RETURNING g_respuesta
                        IF g_respuesta THEN
                           CALL pregunta(" MENOR PERTENECE AL PROPIETARIO DE LA CURP? S/N: ") RETURNING g_respuesta
                           IF g_respuesta THEN
                              LET r_solBajaCurp.menoresNssAsociado = 1 
                           ELSE
                              LET r_solBajaCurp.menoresNssAsociado = 2
                           END IF
                        ELSE
                              LET r_solBajaCurp.menoresNssAsociado = "" 
                        END IF 
                        LET r_solBajaCurp.menoresNssInvadido = ""
                        DISPLAY BY NAME r_solBajaCurp.menoresNssInvadido
                           SELECT tpo_menor_descripcion 
                           INTO   v_desc_menor_inv
                           FROM   sep_tpo_menores 
                           WHERE  tpo_menor = r_solBajaCurp.menoresNssInvadido
                           DISPLAY BY NAME v_desc_menor_inv

                        DISPLAY BY NAME r_solBajaCurp.menoresNssAsociado
                           SELECT tpo_menor_descripcion 
                           INTO   v_desc_menor_asc
                           FROM   sep_tpo_menores 
                           WHERE  tpo_menor = r_solBajaCurp.menoresNssAsociado
                           DISPLAY BY NAME v_desc_menor_asc

                     WHEN 3
                        IF LENGTH(v_curp_asociado_origen) = 18 AND LENGTH(v_curp_invadido_origen) = 18 THEN

                           LET r_solBajaCurp.ind_capturaCurp      = 5 
                           LET r_solBajaCurp.desc_ind_capturaCurp = "CURPS Invadido y Asociado Existen"
                           LET r_solBajaCurp.curp_invadido_final  = v_curp_asociado_origen
                           LET r_solBajaCurp.curp_asociado_final  = v_curp_invadido_origen
                           DISPLAY BY NAME r_solBajaCurp.ind_capturacurp
                           DISPLAY BY NAME r_solBajaCurp.desc_ind_capturacurp
                           DISPLAY BY NAME r_solBajaCurp.curp_invadido_final
                           DISPLAY BY NAME r_solBajaCurp.curp_asociado_final

                           NEXT FIELD tpo_baja

                        ELSE 
                           LET r_solBajaCurp.ind_capturaCurp      = 6 
                           LET r_solBajaCurp.desc_ind_capturaCurp = "Ambas CURPS deben Existir..."

                           LET b_ind_cap = 5 --no existen ambas curps

                           ERROR "Ambas Curps deben existir para tipo de Baja 3..."
                           SLEEP 2
                           ERROR ""
                           NEXT FIELD curp_invadido_final
                        END IF

                  END CASE


              BEFORE FIELD curp_asociado_origen

                     IF b_ind_cap = 2 THEN NEXT FIELD tpo_baja END IF
                     IF b_ind_cap = 1 THEN NEXT FIELD tpo_baja END IF
                     IF b_ind_cap = 0 THEN NEXT FIELD tpo_baja END IF

              BEFORE FIELD curp_invadido_origen

                     IF b_ind_cap = 0 THEN NEXT FIELD tpo_baja END IF
                     IF b_ind_cap = 3 THEN NEXT FIELD tpo_baja END IF
                     IF b_ind_cap = 2 THEN NEXT FIELD tpo_baja END IF

              AFTER FIELD curp_asociado_origen

                        #--CALL pregunta("CURP INVADIDO RELACIONADA A MENOR DE EDAD S/N: ") RETURNING g_respuesta
                        CALL pregunta("CURP ASOCIADO RELACIONADA A MENOR DE EDAD S/N: ") RETURNING g_respuesta
                        IF g_respuesta THEN
                           CALL pregunta(" MENOR PERTENECE AL PROPIETARIO DE LA CURP? S/N: ") RETURNING g_respuesta
                           IF g_respuesta THEN
                              #--LET r_solBajaCurp.menoresNssInvadido = 1 
                              LET r_solBajaCurp.menoresNssAsociado = 1 
                           ELSE
                              #--LET r_solBajaCurp.menoresNssInvadido = 2
                              LET r_solBajaCurp.menoresNssAsociado = 2
                           END IF
                        ELSE
                              #--LET r_solBajaCurp.menoresNssInvadido = "" 
                              LET r_solBajaCurp.menoresNssAsociado = "" 
                        END IF 
                        --LET r_solBajaCurp.menoresNssAsociado = ""
                        LET r_solBajaCurp.menoresNssInvadido= ""
                        DISPLAY BY NAME r_solBajaCurp.menoresNssInvadido
                           SELECT tpo_menor_descripcion 
                           INTO   v_desc_menor_inv
                           FROM   sep_tpo_menores 
                           WHERE  tpo_menor = r_solBajaCurp.menoresNssInvadido
                           DISPLAY BY NAME v_desc_menor_inv

                        DISPLAY BY NAME r_solBajaCurp.menoresNssAsociado
                           SELECT tpo_menor_descripcion 
                           INTO   v_desc_menor_asc
                           FROM   sep_tpo_menores 
                           WHERE  tpo_menor = r_solBajaCurp.menoresNssAsociado
                           DISPLAY BY NAME v_desc_menor_asc

              AFTER FIELD curp_invadido_origen

                        #--CALL pregunta("CURP ASOCIADA RELACIONADA A MENOR DE EDAD S/N: ") RETURNING g_respuesta
                        CALL pregunta("CURP INVADIDO RELACIONADA A MENOR DE EDAD S/N: ") RETURNING g_respuesta
                        IF g_respuesta THEN
                           CALL pregunta(" MENOR PERTENECE AL PROPIETARIO DE LA CURP? S/N: ") RETURNING g_respuesta
                           IF g_respuesta THEN
                              #--LET r_solBajaCurp.menoresNssAsociado = 1 
                              LET r_solBajaCurp.menoresNssInvadido = 1 
                           ELSE
                              #--LET r_solBajaCurp.menoresNssAsociado = 2
                              LET r_solBajaCurp.menoresNssInvadido = 2
                           END IF
                        ELSE
                              #--LET r_solBajaCurp.menoresNssAsociado = "" 
                              LET r_solBajaCurp.menoresNssInvadido = "" 
                        END IF 
                        #--LET r_solBajaCurp.menoresNssInvadido = ""
                        LET r_solBajaCurp.menoresNssAsociado = ""

                        DISPLAY BY NAME r_solBajaCurp.menoresNssInvadido
                           SELECT tpo_menor_descripcion 
                           INTO   v_desc_menor_inv
                           FROM   sep_tpo_menores 
                           WHERE  tpo_menor = r_solBajaCurp.menoresNssInvadido
                           DISPLAY BY NAME v_desc_menor_inv

                        DISPLAY BY NAME r_solBajaCurp.menoresNssAsociado
                           SELECT tpo_menor_descripcion 
                           INTO   v_desc_menor_asc
                           FROM   sep_tpo_menores 
                           WHERE  tpo_menor = r_solBajaCurp.menoresNssAsociado
                           DISPLAY BY NAME v_desc_menor_asc

      ON KEY (CONTROL-f)  --bitacora

               SELECT "OK" 
               FROM   sep_bitacora_wsbajacurp 
               WHERE  idsolicitudseparacion = v_corr
               GROUP BY 1 
               
               IF STATUS = NOTFOUND THEN
                   PROMPT " NO SE ENCONTRARON REGISTROS...<Enter>..." FOR char g_enter
                   NEXT FIELD tpo_baja
               END IF 
               CALL fn_muestra_bitacora(v_corr,r_solBajaCurp.*)
               NEXT FIELD tpo_baja

      ON KEY (CONTROL-b)  ---inserta en bitacora

               IF r_solBajaCurp.estado <> 10 THEN 
                  PROMPT " ESTADO DE SOLICITUD NO MODIFICABLE...<Enter>..." FOR char g_enter
                  NEXT FIELD tpo_baja           
               END IF

                   LET r_solBajaCurp.estado = 40 
                   LET r_solBajaCurp.desc_estado = "40 SOLICITUD ENVIADA"
                   
                   UPDATE sep_sol_baja_curp 
                   SET    estado = r_solBajaCurp.estado   
                   WHERE  idSolicitudSeparacion = r_solBajaCurp.idSolicitudSeparacion                      

                   DISPLAY BY NAME r_solBajaCurp.estado 
                   DISPLAY BY NAME r_solBajaCurp.desc_estado 

                   SELECT a.paterno   ,
                          a.materno   ,
                          a.nombres
                   INTO r_bitacora.apPaternoNssInvadidoREQ  ,
                        r_bitacora.apMaternoNssInvadidoREQ  ,
                        r_bitacora.nombreTrabNssInvadidoREQ
                   FROM   afi_mae_afiliado a
                   WHERE  a.n_seguro = r_solBajaCurp.nss_invadido

                   SELECT a.paterno   ,
                          a.materno   ,
                          a.nombres
                   INTO r_bitacora.apPaternoNssAsociadoREQ  ,
                        r_bitacora.apMaternoNssAsociadoREQ  ,
                        r_bitacora.nombreTrabNssAsociadoREQ
                   FROM   afi_mae_afiliado a
                   WHERE  a.n_seguro = r_solBajaCurp.nss_asociado

                   LET r_bitacora.origenSolicitudREQ = "01"
                   LET r_bitacora.cveEntidadOrigenREQ = cve_afore_local
                   LET r_bitacora.idNegocioInvadidoREQ = r_solBajaCurp.nss_invadido CLIPPED
                   LET r_bitacora.idNegocioAsociadoREQ = r_solBajaCurp.nss_asociado CLIPPED
                   LET r_bitacora.bajaCurpREQ          = r_solBajaCurp.tpo_baja CLIPPED
                 
                   CASE r_solBajaCurp.tpo_baja
                        WHEN 1
                            LET r_bitacora.curpTrabajadorInvadidoREQ = r_solBajaCurp.curp_invadido_origen CLIPPED
                            LET r_bitacora.curpTrabajadorAsociadoREQ = ""
                            LET r_bitacora.menoresNssInvadidoREQ = r_solBajaCurp.menoresNssInvadido

                        WHEN 2

                            LET r_bitacora.curpTrabajadorInvadidoREQ = ""
                            LET r_bitacora.curpTrabajadorAsociadoREQ = r_solBajaCurp.curp_asociado_origen CLIPPED
                            LET r_bitacora.menoresNssAsociadoREQ = r_solBajaCurp.menoresNssAsociado

                        WHEN 3
                            LET r_bitacora.curpTrabajadorInvadidoREQ = r_solBajaCurp.curp_invadido_origen CLIPPED
                            LET r_bitacora.curpTrabajadorAsociadoREQ = r_solBajaCurp.curp_asociado_origen CLIPPED
                   END CASE

                   INSERT INTO sep_bitacora_wsbajacurp (
                                                         origensolicitudREQ
                                                        ,cveentidadorigenREQ
                                                        ,idnegocioinvadidoREQ
                                                        ,curptrabajadorinvadidoREQ
                                                        ,appaternonssinvadidoREQ
                                                        ,apmaternonssinvadidoREQ
                                                        ,nombretrabnssinvadidoREQ
                                                        ,menoresnssinvadidoREQ
                                                        ,idnegocioasociadoREQ
                                                        ,curptrabajadorasociadoREQ
                                                        ,appaternonssasociadoREQ
                                                        ,apmaternonssasociadoREQ
                                                        ,nombretrabnssasociadoREQ
                                                        ,menoresnssasociadoREQ
                                                        ,bajacurpREQ
                                                        ,idbitacorawsbajacurp
                                                        ,idsolicitudseparacion 
                                                        ,estado             
                                                        ,usrsolicitud ) 
                                               VALUES (
                                                         r_bitacora.origensolicitudREQ
                                                        ,r_bitacora.cveentidadorigenREQ
                                                        ,r_bitacora.idnegocioinvadidoREQ
                                                        ,r_bitacora.curptrabajadorinvadidoREQ
                                                        ,r_bitacora.appaternonssinvadidoREQ
                                                        ,r_bitacora.apmaternonssinvadidoREQ
                                                        ,r_bitacora.nombretrabnssinvadidoREQ
                                                        ,r_bitacora.menoresnssinvadidoREQ
                                                        ,r_bitacora.idnegocioasociadoREQ
                                                        ,r_bitacora.curptrabajadorasociadoREQ
                                                        ,r_bitacora.appaternonssasociadoREQ
                                                        ,r_bitacora.apmaternonssasociadoREQ
                                                        ,r_bitacora.nombretrabnssasociadoREQ
                                                        ,r_bitacora.menoresnssasociadoREQ
                                                        ,r_bitacora.bajacurpREQ
                                                        ,seq_sep_bitacora_wsbajacurp.nextval 
                                                        ,v_corr
                                                        ,1 -- estado se pone 1 para que se tome por el ws 
                                                        ,g_usuario_cod
                                                       )

                       -- actualiza nulos a vacios

                       update sep_bitacora_wsbajacurp set curptrabajadorinvadidoREQ = "" 
                       where idsolicitudseparacion = v_corr and curptrabajadorinvadidoREQ IS NULL OR curptrabajadorinvadidoREQ = ''

                       update sep_bitacora_wsbajacurp set appaternonssinvadidoREQ   = "" 
                       where idsolicitudseparacion = v_corr and appaternonssinvadidoREQ   IS NULL OR appaternonssinvadidoREQ = ''

                       update sep_bitacora_wsbajacurp set apmaternonssinvadidoREQ   = "" 
                       where idsolicitudseparacion = v_corr and apmaternonssinvadidoREQ   IS NULL OR apmaternonssinvadidoREQ = ''

                       update sep_bitacora_wsbajacurp set nombretrabnssinvadidoREQ  = "" 
                       where idsolicitudseparacion = v_corr and nombretrabnssinvadidoREQ  IS NULL OR nombretrabnssinvadidoREQ = ''

                       update sep_bitacora_wsbajacurp set menoresnssinvadidoREQ     = "" 
                       where idsolicitudseparacion = v_corr and menoresnssinvadidoREQ     IS NULL OR menoresnssinvadidoREQ = ''

                       update sep_bitacora_wsbajacurp set idnegocioasociadoREQ      = "" 
                       where idsolicitudseparacion = v_corr and idnegocioasociadoREQ      IS NULL OR idnegocioasociadoREQ = ''

                       update sep_bitacora_wsbajacurp set curptrabajadorasociadoREQ = "" 
                       where idsolicitudseparacion = v_corr and curptrabajadorasociadoREQ IS NULL OR curptrabajadorasociadoREQ = ''

                       update sep_bitacora_wsbajacurp set appaternonssasociadoREQ   = "" 
                       where idsolicitudseparacion = v_corr and appaternonssasociadoREQ   IS NULL OR appaternonssasociadoREQ = ''

                       update sep_bitacora_wsbajacurp set apmaternonssasociadoREQ   = "" 
                       where idsolicitudseparacion = v_corr and apmaternonssasociadoREQ   IS NULL OR apmaternonssasociadoREQ = ''

                       update sep_bitacora_wsbajacurp set nombretrabnssasociadoREQ  = "" 
                       where idsolicitudseparacion = v_corr and nombretrabnssasociadoREQ  IS NULL OR nombretrabnssasociadoREQ = ''

                       update sep_bitacora_wsbajacurp set menoresnssasociadoREQ     = "" 
                       where idsolicitudseparacion = v_corr and menoresnssasociadoREQ     IS NULL OR menoresnssasociadoREQ = ''
                                   
                       -- fin actualiza nulos a vacios

                     LET c_mensaje = "Solicitud de Baja de Curp Enviada a ProceSar...<Enter>..."
                     PROMPT c_mensaje clipped  FOR CHAR g_enter
                     EXIT INPUT

      ON KEY (CONTROL-e) --Recupera el response una vez actualizada la bitacora 


               IF r_solBajaCurp.estado <> 40 THEN 
                  PROMPT " ESTADO DE SOLICITUD NO PROCESABLE...<Enter>..." FOR char g_enter
                  NEXT FIELD tpo_baja           
               END IF

               SELECT idbitacorawsbajacurp
                     ,resultadooperacion
                     ,diagnosticoproceso
               INTO  v_idbitacora        
                    ,r_bitacora_response.*
               FROM sep_bitacora_wsbajacurp
               WHERE idsolicitudseparacion = v_corr
               AND   estado               = 2 

               IF STATUS = NOTFOUND THEN
                  PROMPT " RESPONSE AUN NO RECIBIOD EN ENTIDAD PUENTE...<Enter>..." FOR char g_enter
                  NEXT FIELD tpo_baja
               END IF

               IF r_bitacora_response.resultadooperacion = "01"  THEN
                  LET r_solBajaCurp.estado = 50 
                  LET r_solBajaCurp.desc_estado = "50 REQUEST ACEPTADO"

               ELSE 
                  LET r_solBajaCurp.estado = 55
                  LET r_solBajaCurp.desc_estado = "55 REQUEST RECHAZADO"
               END IF

               LET r_solBajaCurp.resultado_operacion_sol = r_bitacora_response.resultadooperacion

               SELECT desc_resultado_operacion 
               INTO r_solBajaCurp.desc_resultado_operacion 
               FROM sep_cat_resultado_operacion 
               WHERE resultado_operacion = r_bitacora_response.resultadooperacion 

               LET r_solBajaCurp.diagnostico         = r_bitacora_response.diagnosticoproceso

               SELECT desc_diagnostico 
               INTO   r_solBajaCurp.desc_diagnostico
               FROM  sep_cat_diag_bajacurp
               WHERE diagnostico = r_solBajaCurp.diagnostico

               UPDATE sep_sol_baja_curp 
               SET    estado = r_solBajaCurp.estado                              ,
                      resultado_operacion   = r_solBajaCurp.resultado_operacion_sol  , 
                      diagnostico           = r_solBajaCurp.diagnostico
               WHERE  idSolicitudSeparacion = r_solBajaCurp.idSolicitudSeparacion                      

               DISPLAY BY NAME r_solBajaCurp.estado 
               DISPLAY BY NAME r_solBajaCurp.desc_estado 
               DISPLAY BY NAME r_solBajaCurp.resultado_operacion_sol
               DISPLAY BY NAME r_solBajaCurp.desc_resultado_operacion
               DISPLAY BY NAME r_solBajaCurp.diagnostico
               DISPLAY BY NAME r_solBajaCurp.desc_diagnostico
           
               IF r_solBajaCurp.resultado_operacion_sol = "01" THEN
                    CALL fn_actualiza_negocio(v_idbitacora,v_corr,r_solBajaCurp.*)
               END IF

               PROMPT " RESPONSE RECIBIDO Y PROCESADO...<Enter>..." FOR char g_enter

               UPDATE sep_bitacora_wsbajacurp 
               SET    estado = 3 -- response procesado
               WHERE  idbitacorawsbajacurp  = v_idbitacora
               AND    idsolicitudseparacion = v_corr

      ON KEY (ESC)

               IF r_solBajaCurp.estado <> 0 AND r_solBajaCurp.estado <> 10 AND r_solBajaCurp.estado <> 55 THEN 
                  PROMPT " ESTADO DE SOLICITUD NO MODIFICABLE...<Enter>..." FOR char g_enter
                  NEXT FIELD tpo_baja           
               END IF
                   LET r_solBajaCurp.estado = 10 
                   LET r_solBajaCurp.desc_estado = "10 CAPTURADA"
                   DISPLAY BY NAME r_solBajaCurp.estado
                   DISPLAY BY NAME r_solBajaCurp.desc_estado

                   SELECT "OK" 
                   FROM sep_sol_baja_curp 
                   WHERE idSolicitudSeparacion = r_solBajaCurp.idSolicitudSeparacion
                   GROUP BY 1                    

                   IF STATUS = NOTFOUND THEN
                      INSERT INTO sep_sol_baja_curp VALUES (seq_sep_sol_baja_curp.nextval       ,
                                                            r_solBajaCurp.idSolicitudSeparacion ,
                                                            r_solBajaCurp.f_captura             ,
                                                            r_solBajaCurp.nss_invadido          ,
                                                            r_solBajaCurp.curp_invadido_origen  ,
                                                            r_solBajaCurp.menoresNssInvadido    ,
                                                            r_solBajaCurp.nss_asociado          ,
                                                            r_solBajaCurp.curp_asociado_origen  ,
                                                            r_solBajaCurp.menoresNssAsociado    ,
                                                            r_solBajaCurp.curp_invadido_final   ,
                                                            r_solBajaCurp.curp_asociado_final   ,
                                                            r_solBajaCurp.estado                ,
                                                            r_solBajaCurp.tpo_baja              ,
                                                            r_solBajaCurp.ind_capturaCurp       ,
                                                            ""                                     , --resultado operacion 
                                                            ""                                       --diagnostico 
                                                             )
                                                            
                      LET c_mensaje = "Solicitud de Baja Curp CAPTURADA...<Enter>...   "
                      LET c_mensaje = c_mensaje CLIPPED
                   ELSE 
                      UPDATE sep_sol_baja_curp 
                      SET    tpo_baja              = r_solBajaCurp.tpo_baja             ,
                             ind_capturaCurp       = r_solBajaCurp.ind_capturaCurp      ,
                             menoresNssInvadido    = r_solBajaCurp.menoresNssInvadido   ,
                             menoresNssAsociado    = r_solBajaCurp.menoresNssAsociado   ,
                             curp_invadido_final   = r_solBajaCurp.curp_invadido_final  ,
                             curp_asociado_final   = r_solBajaCurp.curp_asociado_final  ,
                             estado                = 10 --capturado
                      WHERE  idSolicitudSeparacion = r_solBajaCurp.idSolicitudSeparacion                      
                      
                      LET c_mensaje = "Captura de Solicitud de Baja Curp ACTUALIZADA...<Enter>...    "
                      LET c_mensaje = c_mensaje CLIPPED
                   END IF

                   PROMPT c_mensaje clipped ATTRIBUTE(REVERSE) FOR char g_enter  
                   --NEXT FIELD tpo_baja
                   EXIT INPUT 
                  
      ON KEY (INTERRUPT) 
         LET INT_FLAG = FALSE
         EXIT INPUT 
   END INPUT

    CLOSE WINDOW ventana_captura

END FUNCTION

#-----------------------------------------------------------------
FUNCTION fn_actualiza_negocio(p_idbitacora,p_corr,r_sep_sol_baja_curp)
#-----------------------------------------------------------------

   DEFINE p_idbitacora      ,
          p_corr            DEC(10,0)  

   DEFINE r_sep_sol_baja_curp  RECORD 
           idsolBajaCurp            DEC(10,0)
          ,idSolicitudSeparacion    DEC(10,0)    
          ,tpo_baja                 SMALLINT     
          ,ind_capturaCurp          SMALLINT     
          ,desc_ind_capturaCurp     VARCHAR(30)  
          ,f_captura                DATE         
          ,nss_invadido             CHAR(11)     
          ,curp_invadido_origen     CHAR(18)     
          ,menoresNssInvadido       SMALLINT
          ,nss_asociado             CHAR(11)     
          ,curp_asociado_origen     CHAR(18)    
          ,menoresNssAsociado       SMALLINT
          ,curp_invadido_final      CHAR(18)     
          ,curp_asociado_final      CHAR(18)    
          ,estado                   SMALLINT     
          ,desc_estado              VARCHAR(35)  
          ,resultado_operacion_sol  CHAR(2)      
          ,desc_resultado_operacion VARCHAR(20)  
          ,diagnostico              CHAR(3)      
          ,desc_diagnostico         VARCHAR(150)
  END RECORD


  DEFINE r_tdp_invadido RECORD 
         tdp_invadido_antes   CHAR(01) ,
         fdp_invadido_antes   CHAR(10) ,
         dp_invadido_antes     CHAR(16) ,
         tdp_invadido_despues CHAR(01) ,
         fdp_invadido_despues CHAR(10) ,
         dp_invadido_despues  CHAR(16) 
  END RECORD
  
  DEFINE r_rp_invadido RECORD 
         rp_invadido_antes    CHAR(01),
         rp_invadido_despues  CHAR(01)
  END RECORD

  DEFINE r_tdp_asociado RECORD 
         tdp_asociado_antes   CHAR(01) ,
         fdp_asociado_antes   CHAR(10) ,
         dp_asociado_antes    CHAR(16) ,
         tdp_asociado_despues CHAR(01) ,
         fdp_asociado_despues CHAR(10) ,
         dp_asociado_despues  CHAR(16) 
  END RECORD

  DEFINE r_rp_asociado RECORD 
         rp_asociado_antes    CHAR(01),
         rp_asociado_despues  CHAR(01)
  END RECORD

   CASE r_sep_sol_baja_curp.tpo_baja 
       WHEN 1
          IF r_sep_sol_baja_curp.ind_capturaCurp = 1 THEN       
            -- se guarda registro histórico de baja de curp en invadido y cambio en asociado
            -- actualización documento probatorio 


            SELECT a.tip_prob     ,
                   a.fol_prob     ,
                   a.doc_prob  
            INTO   r_tdp_invadido.tdp_invadido_antes  ,
                   r_tdp_invadido.fdp_invadido_antes  ,
                   r_tdp_invadido.dp_invadido_antes
            FROM   afi_mae_afiliado a 
            WHERE  a.n_seguro = r_sep_sol_baja_curp.nss_invadido 

            LET r_tdp_invadido.tdp_invadido_despues = "6"
            LET r_tdp_invadido.fdp_invadido_despues = ""
            LET r_tdp_invadido.dp_invadido_despues  = ""

            SELECT a.tip_prob     ,
                   a.fol_prob     ,
                   a.doc_prob
            INTO   r_tdp_asociado.tdp_asociado_antes  ,
                   r_tdp_asociado.fdp_asociado_antes  ,
                   r_tdp_asociado.dp_asociado_antes
            FROM   afi_mae_afiliado a
            WHERE  a.n_seguro = r_sep_sol_baja_curp.nss_asociado

            #--CPL-3875 LET r_tdp_asociado.tdp_asociado_despues = r_tdp_invadido.tdp_invadido_antes
            #--CPL-3875 LET r_tdp_asociado.fdp_asociado_despues = r_tdp_invadido.fdp_invadido_antes
            #--CPL-3875 LET r_tdp_asociado.dp_asociado_despues  = r_tdp_invadido.dp_invadido_antes

            LET r_tdp_asociado.tdp_asociado_despues = r_tdp_asociado.tdp_asociado_antes
            LET r_tdp_asociado.fdp_asociado_despues = r_tdp_asociado.fdp_asociado_antes
            LET r_tdp_asociado.dp_asociado_despues  = r_tdp_asociado.dp_asociado_antes

            -- actualizacion regimen
{
            SELECT reg_pension 
            INTO   r_rp_invadido.rp_invadido_antes
            FROM  afi_cta_peissste 
            WHERE nti = r_sep_sol_baja_curp.nss_invadido            

            --LET r_rp_invadido.rp_invadido_despues = ""
            LET r_rp_invadido.rp_invadido_despues = r_rp_invadido.rp_invadido_antes
            
            SELECT reg_pension 
            INTO   r_rp_asociado.rp_asociado_antes
            FROM  afi_cta_peissste 
            WHERE nti = r_sep_sol_baja_curp.nss_asociado

            LET r_rp_asociado.rp_asociado_despues = r_rp_invadido.rp_invadido_antes
}
            INSERT INTO sep_his_actualiza_invadido VALUES (r_sep_sol_baja_curp.idSolBajaCurp         ,
                                                           r_sep_sol_baja_curp.idSolicitudSeparacion ,
                                                           r_sep_sol_baja_curp.tpo_baja              ,
                                                           r_sep_sol_baja_curp.nss_invadido          , 
                                                           r_sep_sol_baja_curp.curp_invadido_origen  , 
                                                           r_sep_sol_baja_curp.curp_invadido_final   ,
                                                           r_tdp_invadido.tdp_invadido_antes         ,
                                                           r_tdp_invadido.fdp_invadido_antes         ,
                                                           r_tdp_invadido.dp_invadido_antes          ,
                                                           r_tdp_invadido.tdp_invadido_despues       ,
                                                           r_tdp_invadido.fdp_invadido_despues       ,
                                                           r_tdp_invadido.dp_invadido_despues        ,
                                                           r_rp_invadido.rp_invadido_antes           ,
                                                           r_rp_invadido.rp_invadido_despues          )
                                                           
            INSERT INTO sep_his_actualiza_asociado VALUES (r_sep_sol_baja_curp.idSolBajaCurp         ,
                                                           r_sep_sol_baja_curp.idSolicitudSeparacion ,
                                                           r_sep_sol_baja_curp.tpo_baja              ,
                                                           r_sep_sol_baja_curp.nss_asociado          , 
                                                           r_sep_sol_baja_curp.curp_asociado_origen  , 
                                                           r_sep_sol_baja_curp.curp_asociado_final   ,
                                                           r_tdp_asociado.tdp_asociado_antes         ,
                                                           r_tdp_asociado.fdp_asociado_antes         ,
                                                           r_tdp_asociado.dp_asociado_antes          ,
                                                           r_tdp_asociado.tdp_asociado_despues       ,
                                                           r_tdp_asociado.fdp_asociado_despues       ,
                                                           r_tdp_asociado.dp_asociado_despues        ,
                                                           r_rp_asociado.rp_asociado_antes           ,
                                                           r_rp_asociado.rp_asociado_despues          )

            UPDATE afi_mae_afiliado 
            SET    n_unico  = "",
                   tip_prob = 6,
                   fol_prob = r_tdp_invadido.fdp_invadido_despues     ,
                   doc_prob = r_tdp_invadido.dp_invadido_despues      
            WHERE  n_seguro = r_sep_sol_baja_curp.nss_invadido

            --UPDATE afi_cta_peissste
            --SET    reg_pension     = r_rp_asociado.rp_asociado_despues
            --WHERE  nti             = r_sep_sol_baja_curp.nss_asociado

          END IF
            
       EXIT CASE       

       WHEN 2 
            -- se guarda registro histórico de baja de curp en asociado y cambio en invadido
            -- actualización documento probatorio 
          IF r_sep_sol_baja_curp.ind_capturaCurp = 3 THEN

            SELECT a.tip_prob ,
                   a.fol_prob ,
                   a.doc_prob 
            INTO   r_tdp_asociado.tdp_asociado_antes  ,
                   r_tdp_asociado.fdp_asociado_antes  ,
                   r_tdp_asociado.dp_asociado_antes
            FROM   afi_mae_afiliado a
            WHERE  a.n_seguro = r_sep_sol_baja_curp.nss_asociado

            LET r_tdp_asociado.tdp_asociado_despues = "6"
            LET r_tdp_asociado.fdp_asociado_despues = ""
            LET r_tdp_asociado.dp_asociado_despues  = ""

            SELECT a.tip_prob,
                   a.fol_prob,
                   a.doc_prob
            INTO   r_tdp_invadido.tdp_invadido_antes  ,
                   r_tdp_invadido.fdp_invadido_antes  ,
                   r_tdp_invadido.dp_invadido_antes
            FROM   afi_mae_afiliado a
            WHERE  a.n_seguro = r_sep_sol_baja_curp.nss_invadido 

            #--CPL-3875 LET r_tdp_invadido.tdp_invadido_despues = r_tdp_asociado.tdp_asociado_antes
            #--CPL-3875 LET r_tdp_invadido.fdp_invadido_despues = r_tdp_asociado.fdp_asociado_antes
            #--CPL-3875 LET r_tdp_invadido.dp_invadido_despues  = r_tdp_asociado.dp_asociado_antes

            LET r_tdp_invadido.tdp_invadido_despues = r_tdp_invadido.tdp_invadido_antes
            LET r_tdp_invadido.fdp_invadido_despues = r_tdp_invadido.fdp_invadido_antes
            LET r_tdp_invadido.dp_invadido_despues  = r_tdp_invadido.dp_invadido_antes

            -- actualizacion regimen
{
            SELECT reg_pension 
            INTO   r_rp_asociado.rp_asociado_antes
            FROM  afi_cta_peissste 
            WHERE nti = r_sep_sol_baja_curp.nss_asociado

            --LET r_rp_asociado.rp_asociado_despues = ""
            LET r_rp_asociado.rp_asociado_despues = r_rp_asociado.rp_asociado_antes

            SELECT reg_pension 
            INTO   r_rp_invadido.rp_invadido_antes
            FROM  afi_cta_peissste 
            WHERE nti = r_sep_sol_baja_curp.nss_invadido            

            LET r_rp_invadido.rp_invadido_despues = r_rp_asociado.rp_asociado_antes
}
            INSERT INTO sep_his_actualiza_invadido VALUES (r_sep_sol_baja_curp.idSolBajaCurp         ,
                                                           r_sep_sol_baja_curp.idSolicitudSeparacion ,
                                                           r_sep_sol_baja_curp.tpo_baja              ,
                                                           r_sep_sol_baja_curp.nss_invadido          , 
                                                           r_sep_sol_baja_curp.curp_invadido_origen  , 
                                                           r_sep_sol_baja_curp.curp_invadido_final   ,
                                                           r_tdp_invadido.tdp_invadido_antes         ,
                                                           r_tdp_invadido.fdp_invadido_antes         ,
                                                           r_tdp_invadido.dp_invadido_antes          ,
                                                           r_tdp_invadido.tdp_invadido_despues       ,
                                                           r_tdp_invadido.fdp_invadido_despues       ,
                                                           r_tdp_invadido.dp_invadido_despues        ,
                                                           r_rp_invadido.rp_invadido_antes           ,
                                                           r_rp_invadido.rp_invadido_despues          )

            INSERT INTO sep_his_actualiza_asociado VALUES (r_sep_sol_baja_curp.idSolBajaCurp         ,
                                                           r_sep_sol_baja_curp.idSolicitudSeparacion ,
                                                           r_sep_sol_baja_curp.tpo_baja              ,
                                                           r_sep_sol_baja_curp.nss_asociado          , 
                                                           r_sep_sol_baja_curp.curp_asociado_origen  , 
                                                           r_sep_sol_baja_curp.curp_asociado_final   ,
                                                           r_tdp_asociado.tdp_asociado_antes         ,
                                                           r_tdp_asociado.fdp_asociado_antes         ,
                                                           r_tdp_asociado.dp_asociado_antes          ,
                                                           r_tdp_asociado.tdp_asociado_despues       ,
                                                           r_tdp_asociado.fdp_asociado_despues       ,
                                                           r_tdp_asociado.dp_asociado_despues        ,
                                                           r_rp_asociado.rp_asociado_antes           ,
                                                           r_rp_asociado.rp_asociado_despues          )

            UPDATE afi_mae_afiliado
            SET    n_unico  = "",
                   tip_prob = 6 ,
                   fol_prob = r_tdp_asociado.fdp_asociado_despues     ,
                   doc_prob = r_tdp_asociado.dp_asociado_despues      
            WHERE  n_seguro = r_sep_sol_baja_curp.nss_asociado

            --UPDATE afi_cta_peissste
            --SET    reg_pension     = r_rp_asociado.rp_asociado_despues
            --WHERE  nti                 = r_sep_sol_baja_curp.nss_asociado
            
         END IF            

       EXIT CASE       

       WHEN 3
           IF r_sep_sol_baja_curp.ind_capturaCurp = 5 THEN
            -- se guarda registro histórico de intercambio de curps y regimen
            -- actualización documento probatorio 

            SELECT a.tip_prob  ,
                   a.fol_prob  ,
                   a.doc_prob
            INTO   r_tdp_invadido.tdp_invadido_antes  ,
                   r_tdp_invadido.fdp_invadido_antes  ,
                   r_tdp_invadido.dp_invadido_antes
            FROM   afi_mae_afiliado a
            WHERE  a.n_seguro = r_sep_sol_baja_curp.nss_invadido 

            SELECT a.tip_prob  ,
                   a.fol_prob  ,
                   a.doc_prob
            INTO   r_tdp_asociado.tdp_asociado_antes  ,
                   r_tdp_asociado.fdp_asociado_antes  ,
                   r_tdp_asociado.dp_asociado_antes
            FROM   afi_mae_afiliado a
            WHERE  a.n_seguro = r_sep_sol_baja_curp.nss_asociado
            
            LET r_tdp_invadido.tdp_invadido_despues = r_tdp_asociado.tdp_asociado_antes
            LET r_tdp_invadido.fdp_invadido_despues = r_tdp_asociado.fdp_asociado_antes
            LET r_tdp_invadido.dp_invadido_despues  = r_tdp_asociado.dp_asociado_antes

            LET r_tdp_asociado.tdp_asociado_despues = r_tdp_invadido.tdp_invadido_antes
            LET r_tdp_asociado.fdp_asociado_despues = r_tdp_invadido.fdp_invadido_antes
            LET r_tdp_asociado.dp_asociado_despues  = r_tdp_invadido.dp_invadido_antes

            -- actualizacion regimen
{
            SELECT reg_pension 
            INTO   r_rp_invadido.rp_invadido_antes
            FROM  afi_cta_peissste 
            WHERE nti = r_sep_sol_baja_curp.nss_invadido            
            
            SELECT reg_pension 
            INTO   r_rp_asociado.rp_asociado_antes
            FROM  afi_cta_peissste 
            WHERE nti = r_sep_sol_baja_curp.nss_asociado

            LET r_rp_invadido.rp_invadido_despues = r_rp_asociado.rp_asociado_antes
            LET r_rp_asociado.rp_asociado_despues = r_rp_invadido.rp_invadido_antes
}
            INSERT INTO sep_his_actualiza_invadido VALUES (r_sep_sol_baja_curp.idSolBajaCurp         ,
                                                           r_sep_sol_baja_curp.idSolicitudSeparacion ,
                                                           r_sep_sol_baja_curp.tpo_baja              ,
                                                           r_sep_sol_baja_curp.nss_invadido          , 
                                                           r_sep_sol_baja_curp.curp_invadido_origen  , 
                                                           r_sep_sol_baja_curp.curp_invadido_final   ,
                                                           r_tdp_invadido.tdp_invadido_antes         ,
                                                           r_tdp_invadido.fdp_invadido_antes         ,
                                                           r_tdp_invadido.dp_invadido_antes          ,
                                                           r_tdp_invadido.tdp_invadido_despues       ,
                                                           r_tdp_invadido.fdp_invadido_despues       ,
                                                           r_tdp_invadido.dp_invadido_despues        ,
                                                           r_rp_invadido.rp_invadido_antes           ,
                                                           r_rp_invadido.rp_invadido_despues          )

            INSERT INTO sep_his_actualiza_asociado VALUES (r_sep_sol_baja_curp.idSolBajaCurp         ,
                                                           r_sep_sol_baja_curp.idSolicitudSeparacion ,
                                                           r_sep_sol_baja_curp.tpo_baja              ,
                                                           r_sep_sol_baja_curp.nss_asociado          , 
                                                           r_sep_sol_baja_curp.curp_asociado_origen  , 
                                                           r_sep_sol_baja_curp.curp_asociado_final   ,
                                                           r_tdp_asociado.tdp_asociado_antes         ,
                                                           r_tdp_asociado.fdp_asociado_antes         ,
                                                           r_tdp_asociado.dp_asociado_antes          ,
                                                           r_tdp_asociado.tdp_asociado_despues       ,
                                                           r_tdp_asociado.fdp_asociado_despues       ,
                                                           r_tdp_asociado.dp_asociado_despues        ,
                                                           r_rp_asociado.rp_asociado_antes           ,
                                                           r_rp_asociado.rp_asociado_despues          )
            UPDATE afi_mae_afiliado
            SET    n_unico  = r_sep_sol_baja_curp.curp_invadido_final ,
                   tip_prob = r_tdp_invadido.tdp_invadido_despues     ,
                   fol_prob = r_tdp_invadido.fdp_invadido_despues     ,
                   doc_prob = r_tdp_invadido.dp_invadido_despues      
            WHERE  n_seguro = r_sep_sol_baja_curp.nss_invadido                 
            
            UPDATE afi_mae_afiliado
            SET    n_unico  = r_sep_sol_baja_curp.curp_asociado_final ,
                   tip_prob = r_tdp_asociado.tdp_asociado_despues     ,
                   fol_prob = r_tdp_asociado.fdp_asociado_despues     ,
                   doc_prob = r_tdp_asociado.dp_asociado_despues      
            WHERE  n_seguro = r_sep_sol_baja_curp.nss_asociado

            --UPDATE afi_cta_peissste
            --SET    reg_pension     = r_rp_asociado.rp_asociado_despues
            --WHERE  nti                 = r_sep_sol_baja_curp.nss_asociado
         END IF
       EXIT CASE       
   END CASE
END FUNCTION

#--------------------------------------------------
FUNCTION fn_muestra_bitacora(vl_corr, v_solBajaCurp)
#--------------------------------------------------

    DEFINE vl_corr      DEC(10,0) ,
           pos          SMALLINT  ,
           v_idbitacora DEC(10,0)

    DEFINE l_txt        CHAR(2000)

    DEFINE v_solBajaCurp   RECORD     
           idsolBajaCurp            DEC(10,0)    ,
           idSolicitudSeparacion    DEC(10,0)    ,
           tpo_baja                 SMALLINT     ,  --phantom
           ind_capturaCurp          SMALLINT     ,  --phantom
           desc_ind_capturaCurp     VARCHAR(30)  ,       
           f_captura                DATE         ,
           nss_invadido             CHAR(11)     ,
           curp_invadido_origen     CHAR(18)     ,
           menoresNssInvadido       SMALLINT     ,
           nss_asociado             CHAR(11)     ,
           curp_asociado_origen     CHAR(18)     ,
           menoresNssAsociado       SMALLINT     ,
           curp_invadido_final      CHAR(18)     ,
           curp_asociado_final      CHAR(18)     ,   --phantom
           estado                   SMALLINT     ,
           desc_estado              VARCHAR(35)  ,
           resultado_operacion_sol  CHAR(2)      ,
           desc_resultado_operacion VARCHAR(20),
           diagnostico              CHAR(3)       ,
           desc_diagnostico         VARCHAR(150)
    END RECORD

    DEFINE lcurp_invadido_origen     ,
           lcurp_asociado_origen     ,
           lcurp_invadido_final      ,
           lcurp_asociado_final      CHAR(18)

    DEFINE lr_bitacora RECORD 
           idbitacorawsbajacurp   dec(10,0)
          ,idSolicitudSeparacion  dec(10,0)
          ,fechaalta              date     
          ,idnegocioinvadido char(11)
          ,idnegocioasociado char(11)
          ,bajacurp char(1)
          ,tpo_baja_descripcion char(20)
          ,diagnosticoproceso char(3)
    END RECORD 

    DEFINE larr_lr_bitacora ARRAY[2000] OF  RECORD 
           idbitacorawsbajacurp   dec(10,0)
          ,idSolicitudSeparacion  dec(10,0)
          ,fechaalta              date     
          ,idnegocioinvadido char(11)
          ,idnegocioasociado char(11)
          ,bajacurp char(1)
          ,tpo_baja_descripcion char(20)
          ,diagnosticoproceso char(3)
    END RECORD 

    DEFINE cuenta      SMALLINT

    LET cuenta = 1

    LET l_txt = " SELECT a.idbitacorawsbajacurp  , ",
                       " a.idsolicitudseparacion , ",
                       " a.fechaalta             , ",
                       " a.idnegocioinvadidoreq  , ",
                       " a.idnegocioasociadoreq  , ",
                       " a.bajaCurpReq           , ", 
                       " b.tpo_baja_descripcion  , ",
                       " a.diagnosticoproceso     ",
                " FROM sep_bitacora_wsbajacurp a , ",
                     " sep_tpo_baja_curp b " ,
                " WHERE a.bajaCurpReq = b.tpo_baja ",
                " AND  a.idsolicitudseparacion = ? ",
                " ORDER BY a.idbitacorawsbajacurp " 

    PREPARE prp_bitacora FROM l_txt

    DECLARE cur_bitacora CURSOR FOR prp_bitacora 

    FOREACH cur_bitacora USING vl_corr 
                         INTO lr_bitacora.*

            LET larr_lr_bitacora[cuenta].* = lr_bitacora.*
            LET cuenta = cuenta + 1

    END FOREACH


    OPEN WINDOW ventana_bitacora AT 2,2 WITH FORM "SEPM71003" ATTRIBUTE(BORDER)

    DISPLAY "                                                                                  " AT 1,1
    DISPLAY "  Ctrl-w Detalle WS                                                               " AT 2,1
    DISPLAY "  Ctrl-c Salir                                                                    " AT 1,1
    DISPLAY " SEPM7100      SOLICITUDES DE BAJA CURP SEPARACION DE CUENTAS                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    CALL SET_COUNT(cuenta-1)

    LET tot_folio_ts = cuenta-1
    LET total_pa = 1

    DISPLAY BY NAME  tot_folio_ts

    DISPLAY ARRAY larr_lr_bitacora TO scr_his.*

     ON KEY (CONTROL-W)   -- consulta detalle request y response de tabla puente

        LET pos = ARR_CURR()
        LET v_idbitacora           = larr_lr_bitacora[pos].idbitacorawsbajacurp
        CALL despliega_detalle_ws("sep_bitacora_wsbajacurp",v_idbitacora)

     ON KEY(CONTROL-C) EXIT DISPLAY 

    END DISPLAY 
    CLOSE WINDOW ventana_bitacora

END FUNCTION

#-------------------------------------------------------------
FUNCTION despliega_detalle_ws(p_tabla_consulta,v_corr)
#-------------------------------------------------------------

DEFINE v_nss char(11)
DEFINE v_id_cliente dec(10,0)
define p smallint
DEFINE p_tabla       VARCHAR(50),
       p_tabla_consulta VARCHAR(50),
       p_id_registro DECIMAL(9,0),
       p_id_registro_consulta DECIMAL(9,0),
       p_id_maquinaria SMALLINT,
       p_tmp_detalle_solicitud RECORD
          v_ind        SMALLINT,
          v_diag       CHAR(3),
          v_sql_error  INTEGER,
          v_isam_error INTEGER,
          v_msg_error  VARCHAR(100),
          v_etiqueta   VARCHAR(50),
          v_valor      VARCHAR(80)
       END RECORD,
       v_detalle ARRAY[100] OF RECORD
          v_etiqueta VARCHAR(50),
          v_valor    VARCHAR(80)
       END RECORD,
       reg  SMALLINT ,
       vnss CHAR(11) ,
       vnombre CHAR(80)

   DEFINE v_corr     DEC(10,0)
   DEFINE v_consulta CHAR(200)

  OPEN WINDOW sepf7001 AT 7,2 WITH FORM "SEPF7601" ATTRIBUTE(BORDER)
  DISPLAY " Ctrl-c Salir                                                                  " AT 1,1
  DISPLAY "                               DETALLE SERVICIO                                " AT 2,1 ATTRIBUTE(REVERSE)


  LET v_consulta = " EXECUTE FUNCTION fn_glo_recupera_etiquetas(?,?) "

  PREPARE prp_rec_detalle_solicitud FROM v_consulta

   DECLARE cur_rec_detalle_solicitud CURSOR FOR prp_rec_detalle_solicitud

   LET reg = 1

   FOREACH cur_rec_detalle_solicitud USING p_tabla_consulta ,
                                           v_corr
                                    INTO p_tmp_detalle_solicitud.*

      LET v_detalle[reg].v_etiqueta = p_tmp_detalle_solicitud.v_etiqueta
      LET v_detalle[reg].v_valor    = p_tmp_detalle_solicitud.v_valor
      LET reg = reg + 1

   END FOREACH

  LET v_detalle[1].v_etiqueta = "Fecha Solicitud:"

  SELECT idnegocioInvadidoReq
   INTO  vnss 
  FROM sep_bitacora_wsbajacurp
  WHERE idbitacorawsbajacurp = v_corr
 
  DISPLAY BY NAME vnss
  CALL SET_COUNT(reg-1)

  DISPLAY ARRAY v_detalle TO scr_2.* ATTRIBUTE(REVERSE)
    ON KEY (INTERRUPT)
       EXIT DISPLAY
  END DISPLAY

  CLOSE WINDOW sepf7001

END FUNCTION

FUNCTION pregunta(largumento)
#ciclo while para preguntas tipo s/n
-------------------------------------

define largumento          char(60) ,
       lrespuesta          smallint ,
       lenter              char(001)

let lrespuesta = 0

  while true
    prompt largumento clipped for char lenter
      if lenter matches "[SsNn]" then
        if lenter matches "[Ss]" then
           let lrespuesta = 1
           exit while
        else
           let lrespuesta = 0
           exit while
        end if
      end if
  end while
  return lrespuesta
END FUNCTION
