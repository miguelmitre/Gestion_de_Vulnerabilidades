#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIP001  => TRASPASA AFILIADOS APROBADOS POR APP AL MAESTRO       #
#Sistema           => AFI.                                                  #
#Fecha             => MAr 2019                                              #
#Descripcion       => JGHM APERTURA LAS CUENTAS REGISTRO MOVIL APP          #
#############################################################################
#CPL2958           => JGHM Jul 2019 validaciones de APP                     #
#############################################################################

DATABASE safre_af

GLOBALS
 DEFINE 
    enter                 CHAR(1),
    generar               CHAR(1),
    aux_pausa             CHAR(1),
    opc                   CHAR(1),
    gc_usuario            CHAR(8),
    HORA                  CHAR(8),
    vnss                  CHAR(11),
    operacion             CHAR(40),
    v_sql_1               CHAR(50),
    v_sql_2               CHAR(50),
    HOY                   DATE,
    f_ini_tmte            DATE,
    v_query               CHAR(300) 

 DEFINE w_aux             RECORD
    n_seguro              LIKE afi_solicitud.n_seguro,
    n_unico               LIKE afi_solicitud.n_unico ,
    n_rfc                 LIKE afi_solicitud.n_rfc   ,
    paterno               LIKE afi_solicitud.paterno ,
    materno               LIKE afi_solicitud.materno ,
    nombres               LIKE afi_solicitud.nombres ,
    fena                  LIKE afi_solicitud.fena    ,
    sexo                  LIKE afi_solicitud.sexo    ,
    frecafor              LIKE afi_solicitud.frecafor,
    n_folio               LIKE afi_solicitud.n_folio ,
    tipo_sol              LIKE afi_solicitud.tipo_solicitud,
    sta_int               SMALLINT,
    rechaza               SMALLINT,
    lc_tipo_afi           SMALLINT                    -- CPL2958
 END RECORD

 DEFINE g_afore           RECORD LIKE tab_afore_local.*
 DEFINE g_paramgrales     RECORD LIKE seg_modulo.*
 DEFINE g_aficefa         RECORD LIKE afi_icefa.*
 DEFINE gr_ctanssreg      RECORD LIKE cta_nss_regimen.*

 DEFINE reg_bat           RECORD
    pid                   INTEGER,
    proceso_cod           INTEGER,
    opera_cod             INTEGER,
    nombre_archivo        CHAR(25)
 END RECORD

 DEFINE
    bnd_proceso           SMALLINT ,
    pestado_marca         SMALLINT ,
    pcodigo_rechazo       SMALLINT ,
    ejecuta               CHAR(300),
    xcodigo_marca         SMALLINT ,
    xcodigo_rechazo       SMALLINT ,
    pmarca_entra          SMALLINT,
    con_curp              SMALLINT,
    sin_curp              SMALLINT,
    pmarca_causa          SMALLINT,
    pfecha_causa          SMALLINT,
    xmarca_esado          SMALLINT,
    edo_proc              SMALLINT

 DEFINE consulta_carta    CHAR(120)

 DEFINE reg_carta         RECORD LIKE int_ctr_carta.*
 DEFINE afi               RECORD LIKE afi_solicitud.*

 DEFINE v_desmarca        CHAR(300),
        vmarca            SMALLINT,
        vcorrelativo      INTEGER

 DEFINE ga_con            ARRAY[7] OF RECORD 
        ley               CHAR(23),
        ape               SMALLINT,
        noa               SMALLINT
 END RECORD 
 DEFINE gi_x              SMALLINT
 DEFINE gc_linea          CHAR(350) 

END GLOBALS


MAIN

 DISPLAY " "
 DISPLAY ".1"

 CALL STARTLOG("AFIP001.log")

 CALL inicio()  

 DEFER INTERRUPT
 OPTIONS INPUT WRAP,
 PROMPT LINE LAST,
 ACCEPT KEY CONTROL-I

 CALL proceso_principal()
 CALL despliega_resultados()
 
 ERROR "Proceso finalizado"
 PROMPT "Proceso finalizado, [Enter] para salir" FOR enter

END MAIN


FUNCTION inicio()

 -- LET reg_bat.pid            = ARG_VAL(1)
 -- LET reg_bat.proceso_cod    = ARG_VAL(2)
 -- LET reg_bat.opera_cod      = ARG_VAL(3)

 LET bnd_proceso = 0

 -- IF reg_bat.pid THEN
 --    DISPLAY "INICIANDO PROCESO ..."
 --    LET bnd_proceso = 1
 -- END IF

 WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
    DROP TABLE safre_tmp:afi_2
    DROP TABLE safre_tmp:afi_rech
 WHENEVER ERROR STOP

 CREATE TABLE safre_tmp:afi_2
   (n_seguro           CHAR(11) ,
    n_unico            CHAR(18) ,
    n_rfc              CHAR(13) ,
    paterno            CHAR(40) ,
    materno            CHAR(40) ,
    nombres            CHAR(40) ,
    fena               DATE     ,
    sexo               SMALLINT ,
    frecafor           DATE     ,
    n_folio            DECIMAL(10,0),
    tipo_sol           SMALLINT ,
    sta_int            SMALLINT ,
    rechaza            SMALLINT ,
    lc_tipo_afi        SMALLINT );

 CREATE TABLE safre_tmp:afi_rech
   (n_seguro           CHAR(11),
    n_unico            CHAR(18),
    n_rfc              CHAR(13),
    paterno            CHAR(40),
    materno            CHAR(40),
    nombres            CHAR(40),
    fena               DATE,
    sexo               SMALLINT,
    frecafor           DATE,
    n_folio            DECIMAL(10,0),
    tipo_sol           SMALLINT,
    sta_int            SMALLINT,
    motivo             VARCHAR(40));
 DATABASE safre_af

 SELECT *
   INTO g_paramgrales.*
   FROM seg_modulo 
  WHERE modulo_cod = 'afi'

 SELECT *, USER
   INTO g_afore.*, gc_usuario
   FROM tab_afore_local

 LET HOY      = TODAY
 LET HORA     = TIME

 LET pmarca_entra    = 605
 LET pestado_marca   = 0
 LET pcodigo_rechazo = 0
 LET pmarca_causa    = 0
 LET pfecha_causa    = ""

 LET operacion = 'ALTA EN MAESTRO DE AFILIADOS'

 INITIALIZE reg_carta.* TO NULL  
 INITIALIZE gr_ctanssreg.* TO NULL

 LET v_sql_1 = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
 LET v_sql_2 = "EXECUTE PROCEDURE fn_regimen_inv(?,?,?,?,?,?)"
 LET v_desmarca = "EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "

 PREPARE stmt1 FROM v_sql_1
 PREPARE stmt2 FROM v_sql_2

 LET ga_con[1].ley = '60-APP MISMA AFORE    '
 LET ga_con[2].ley = '61-APP DISTINTA AFORE '
 LET ga_con[3].ley = '24-NO AFILIADO APP    '
 LET ga_con[4].ley = '26-MENOR DE EDAD APP  '
 LET ga_con[5].ley = '30-IMSS PURO          '
 LET ga_con[6].ley = '  -NO IDENTIFICADO    '
 LET ga_con[7].ley = 'TOTAL                 '

 LET gi_x = 0
 FOR gi_x = 1 TO 7
    LET ga_con[gi_x].ape = 0
    LET ga_con[gi_x].noa = 0
 END FOR
 
END FUNCTION


FUNCTION proceso_principal()

 OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIP0011" ATTRIBUTE(BORDER)
 DISPLAY "                                < Ctrl-C > Salir                              " AT 1,2 ATTRIBUTE(REVERSE)
 DISPLAY " AFIP001  TRASPASA AFILIADOS APROBADOS REG. MOVIL AL MAESTRO                  " AT 3,1 ATTRIBUTE(REVERSE)
 DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

 INPUT BY NAME generar
    AFTER FIELD generar
       IF generar NOT MATCHES "[SsNn]" THEN
          ERROR "Opcion solo puede ser [S/N]"
       ELSE
          IF generar MATCHES "[Nn]" THEN
             ERROR "PROCESO CANCELADO"
             SLEEP 2
             EXIT PROGRAM
          ELSE
             ERROR "Procesando Informacion... Espere un momento"
             CALL traspasa_datos() 
          END IF
          EXIT INPUT
       END IF

       ON KEY ( INTERRUPT )
          EXIT PROGRAM
 END INPUT
END FUNCTION


FUNCTION traspasa_datos()
 DEFINE 
    pat RECORD LIKE afi_patron.* ,
    ben RECORD LIKE afi_beneficiario.*,
    mae RECORD LIKE afi_mae_afiliado.*,
    cta RECORD LIKE cta_ctr_cuenta.*

 DEFINE 
    mensaje   CHAR(050),
    G_LISTA   CHAR(300)

 DEFINE 
    i             ,
    cont          ,
    HAY           ,
    v_porc        SMALLINT,
    v_existe      ,
    v_edad        ,
    v_criterio    ,
    v_ind_edad    SMALLINT,        
    v_crea_fecha  DATE,        
    v_tipo_proc   ,
    v_tipo_trasp  ,
    v_medio       ,
    v_cve_siefore ,
    v_cve_sief_i  ,
    v_cve_sief_f  ,
    ---v_edad        ,
    v_rechazo     SMALLINT,
    v_folioatencion INTEGER
        
 DEFINE
    v_curp        CHAR(18),
    v_rfc         CHAR(13),
    v_fena        DATE

 DEFINE regrowid RECORD 
    v_rowid   DECIMAL(10,0)
 END RECORD  
    	
 DEFINE v_afisolreg RECORD
    nss   LIKE afi_solicitud_regimen.nss,
    fol   LIKE afi_solicitud_regimen.n_folio,
    ts    LIKE afi_solicitud_regimen.tipo_solicitud,
    edo   LIKE afi_solicitud_regimen.estado
 END RECORD

 DEFINE v_ctareg     RECORD LIKE cta_regimen.*

 DEFINE vn_folio_tes   DECIMAL(10,0)
 DEFINE vn_folio_tes2  DECIMAL(10,0)
 DEFINE vcuenta_tes    SMALLINT
 DEFINE vrowid         INTEGER
 DEFINE conta_tes      SMALLINT

 DEFINE vgrupo_regm    ,
        vgrupo_regm2   ,
        vmarca_cod     SMALLINT
 
 DEFINE ls_sie         SMALLINT
 DEFINE lc_tipo_afi    SMALLINT      -- CPL2958 Afore movil
 
 -- CPL-3087      
 DEFINE v_siefore      SMALLINT 
 -- CPL-3087
 
   DEFINE v_tipo_beneficiario INTEGER
   DEFINE v_tramite_ben       INTEGER
   DEFINE v_porcentaje_tot    DECIMAL(6,2)
   DEFINE v_ind_designacion   INTEGER

   DEFINE v_cod_respuesta     CHAR(2)
   DEFINE v_cod_diagnostico   SMALLINT
	DEFINE v_descripcion       VARCHAR(100)


 LET mensaje = "NO SE HICIERON TRASPASOS AL MAESTRO DE AFILIADOS"
    
 --LET v_tipo_trasp = 5
 LET v_tipo_trasp = 16
 LET v_tipo_proc  = 1
 LET v_medio      = 10

 DECLARE cursor_1 CURSOR FOR 
    SELECT rowid,A.* 
      FROM afi_solicitud A
     WHERE A.status_interno = 60
       AND A.tipo_solicitud IN( 23,24,26,30,38,39,40,41) -- CPL2958 Afore movil --CPL-6460 Se agrega Afore Web
  -- ORDER BY n_seguro

 FOREACH cursor_1 INTO  regrowid.v_rowid,afi.*

    LET HAY = FALSE
    SELECT COUNT(*)
      INTO HAY
      FROM afi_mae_afiliado m
     WHERE m.n_seguro = afi.n_seguro

    IF HAY THEN
       SELECT *
         INTO mae.*
         FROM afi_mae_afiliado ma
        WHERE ma.n_seguro = afi.n_seguro

       IF SQLCA.SQLCODE = 0 THEN
          --  CPL2958  inhibido, se deben tratar los tipo 5 
          --  IF mae.tipo_solicitud = 5 THEN  -- ASIGNADO MISMA AFORE
       	  --    CONTINUE FOREACH             -- sólo para tipos de solicitud 23, únicamente para las solicitudes 
          --                                 -- de registro por App asignadas en otras Afores
          -- ELSE

          INSERT INTO afi_his_afiliado VALUES (mae.*)

          IF SQLCA.SQLCODE = 0 THEN
             DELETE
               FROM afi_mae_afiliado
              WHERE n_seguro = afi.n_seguro
          END IF

          SELECT b.*
            INTO cta.*
            FROM cta_ctr_cuenta b
           WHERE b.nss = afi.n_seguro

          IF cta.nss THEN
             INSERT INTO cta_his_cuenta VALUES (cta.*)
          END IF

          IF mae.tipo_solicitud = 5 THEN
             LET afi.finitmte = mae.fentcons
             LET afi.fentcons = afi.fentcons
             UPDATE afi_det_asignado
                SET fecha_afiliacion = afi.fentcons,
                    estado_asignado  = 100
              WHERE n_seguro = mae.n_seguro
                AND n_folio  = mae.n_folio
                AND tipo_solicitud = mae.tipo_solicitud
          END IF

          LET HAY = FALSE
          -- END IF
       END IF
        
       UPDATE safre_af:taa_cd_det_cedido
          SET estado   = 99
        WHERE n_seguro = afi.n_seguro
          AND estado  IN (12, 103)
    END IF

    LET afi.status_interno = 100
    LET afi.status_captura = 100

    IF NOT HAY THEN
       IF afi.n_unico IS NOT NULL 
        AND afi.n_unico <> "                  " 
        AND LENGTH(afi.n_unico) = 18 THEN
          LET afi.status_interno = 200
          LET afi.status_captura = 0
       ELSE
          LET afi.status_interno = 100
          LET afi.status_captura = 0
       END IF

       LET afi.status = NULL 
     
       # -- Modificacion 9 Ago 2019 solicitan fecrafor, finicta, fentcons = today
 
       LET afi.frecafor = TODAY 
       LET afi.finicta = TODAY 
       LET afi.fentcons = TODAY
       LET afi.fecha_envio = TODAY  #CPL-4109 Se actualiza la fecha de envío a TODAY

       INSERT INTO afi_mae_afiliado VALUES(afi.*)

       IF SQLCA.SQLCODE <> 0 THEN
          INSERT INTO safre_tmp:nss_dup VALUES (afi.n_seguro)
       END IF

       DELETE FROM afi_mae_patron
       WHERE n_folio = afi.n_folio
       AND tipo_solicitud = afi.tipo_solicitud

       INSERT INTO afi_mae_patron      -------- Patrones
          SELECT *
            FROM afi_patron
           WHERE n_folio = afi.n_folio
             AND tipo_solicitud = afi.tipo_solicitud

       DELETE FROM afi_mae_benefici
       WHERE n_folio = afi.n_folio
       AND tipo_solicitud = afi.tipo_solicitud
             
       INSERT INTO afi_mae_benefici    -------- Beneficiarios
          SELECT *
            FROM afi_beneficiario
           WHERE n_folio = afi.n_folio
             AND tipo_solicitud = afi.tipo_solicitud

      --Se registra control de designacion para beneficiarios
      LET v_query = "EXECUTE PROCEDURE fn_control_beneficiarios(?,?,?,?,?,?,?)"
      PREPARE exe_control_beneficiarios FROM v_query

      SELECT SUM(porcentaje)
      INTO v_porcentaje_tot
      FROM afi_mae_benefici
      WHERE n_folio = afi.n_folio
      AND tipo_solicitud = afi.tipo_solicitud

      IF v_porcentaje_tot IS NOT NULL AND v_porcentaje_tot > 0 THEN
         LET v_tipo_beneficiario = 2      --Beneficiarios Designados

         LET v_ind_designacion = 1
      ELSE
         LET v_tipo_beneficiario = 5      --SIN Beneficiarios Designados

         LET v_ind_designacion = 0
      END IF

      LET v_tramite_ben = 2      --Registro Movil

      DECLARE cur_control_beneficiarios CURSOR FOR exe_control_beneficiarios
      OPEN  cur_control_beneficiarios USING  afi.n_folio,
                                             afi.tipo_solicitud,
                                             v_tipo_beneficiario,
                                             v_tramite_ben,
                                             v_porcentaje_tot,
                                             v_ind_designacion,
                                             gc_usuario

      FETCH cur_control_beneficiarios INTO v_cod_respuesta,
                                           v_cod_diagnostico,
                                           v_descripcion
      CLOSE cur_control_beneficiarios
          
       SELECT "X"
         FROM cta_ctr_cuenta
        WHERE cta_ctr_cuenta.nss = afi.n_seguro

       IF SQLCA.SQLCODE = 0 THEN
          UPDATE cta_ctr_cuenta
             SET fecha_pri_rcv      = NULL,
                 fecha_ult_rcv      = NULL,
                 fecha_pri_general  = NULL,
                 fecha_ult_general  = NULL,
                 fecha_vol_pat      = NULL,
                 fecha_vol_ven      = NULL,
                 ind_actividad      = 1,
                 fecha_actividad    = HOY,
                 ind_edad           = 0,
                 fecha_edad         = HOY,
                 criterio_edad      = 0,
                 ind_transferencia  = 0,
                 fecha_ind_transf   = HOY,
                 ind_saldo_cero     = 0,
                 fecha_saldo_cero   = NULL,
                 estado_impresion   = 0,
                 periodo_ult_aporte = NULL,
                 dias_cotizados     = 0,
                 ult_sal_integrado  = 0,
                 tipo_informe       = 0,
                 fecha_informe      = NULL,
                 fecha_registro     = HOY,
                 usuario            = gc_usuario
           WHERE nss                = afi.n_seguro
       ELSE
          INSERT INTO cta_ctr_cuenta     #------ Control cuenta
             VALUES (afi.n_seguro,       #nss
                     "",                 #fecha_pri_rcv
                     "",                 #fecha_ult_rcv
                     "01/01/0001",       #fecha_pri_general
                     "",                 #fecha_ult_general
                     "",                 #fecha_vol_pat
                     "",                 #fecha_vol_ven
                     0,                  #ind_saldo_cero
                     "",                 #fecha_saldo_cero
                     1,                  #ind_actividad
                     HOY,                #fecha_actividad
                     0,                  #ind_edad
                     HOY,                #fecha_edad
                     0,                  #criterio_edad
                     0,                  #ind_transferencia
                     HOY,                #fecha_ind_transf
                     0,                  #estado_impresion,
                     "",                 #periodo_ult_aporte
                     0,                  #dias_cotizados
                     0,                  #ult_sal_integrado
                     0,                  #tipo_informe
                     "",                 #fecha_informe
                     HOY,                #fecha_registro
                     gc_usuario  )       #usuario   
       END IF

      # CPL-3173
      LET vmarca       = 0
      LET vcorrelativo = 0
      
      DECLARE cur_desmarca CURSOR FOR
      SELECT mc.marca_cod, mc.correlativo
        FROM cta_act_marca mc
       WHERE mc.nss = afi.n_seguro
       
      FOREACH cur_desmarca INTO vmarca, vcorrelativo
         IF vmarca = 120 OR
            vmarca = 130 THEN 

            CALL desmarca_cuenta (afi.n_seguro, vmarca, gc_usuario, vcorrelativo)
         END IF
      END FOREACH

      FREE cur_desmarca
      # CPL-3173

       -- CPL2958 Ago 2019 actualizando afi_solicitud 
       -- Modificacion solicitan fecrafor, finicta, fentcons = today
       LET afi.frecafor = TODAY 
       LET afi.finicta = TODAY 
       LET afi.fentcons = TODAY

       #CPL-4109 Se actualiza también la fecha de envío a TODAY
       UPDATE afi_solicitud 
          SET frecafor       = TODAY,
              finicta        = TODAY,
              fentcons       = TODAY,
              fecha_envio    = TODAY
        WHERE n_seguro       = afi.n_seguro
          AND n_folio        = afi.n_folio
          AND tipo_solicitud = afi.tipo_solicitud
   
 
       LET v_crea_fecha = HOY

       DECLARE curs1 CURSOR FOR stmt1
       OPEN  curs1 USING afi.n_seguro, v_crea_fecha
       FETCH curs1 INTO v_existe, v_edad, v_criterio, v_ind_edad,
                        v_curp, v_rfc, v_fena
       CLOSE curs1

       LET v_rechazo = 0

       -- CPL-3087
       --
       LET v_siefore = 0
           
       SELECT codigo_siefore
         INTO v_siefore
         FROM cat_rango_nacimiento
        WHERE id_rango_nacimiento = v_ind_edad
            
       IF SQLCA.SQLCODE = NOTFOUND THEN 
       	  LET v_siefore = v_ind_edad 
       END IF 
       --           
       -- CPL-3087

       DECLARE curs2 CURSOR FOR stmt2
       OPEN curs2 USING afi.n_seguro,
                        v_ind_edad,
                        v_siefore,       -- CPL-3087
                        v_tipo_proc,
                        v_tipo_trasp,
                        v_medio
       FETCH curs2 INTO v_existe, v_ind_edad, v_rechazo, v_folioatencion
       CLOSE curs2

       LET vcuenta_tes   = 0
       LET vn_folio_tes  = 0
       LET vn_folio_tes2 = 0

       SELECT COUNT(*)
         INTO vcuenta_tes
         FROM tes_solicitud
        WHERE nss = afi.n_seguro
          AND tipo_traspaso = 16
          AND fecha_solicitud = TODAY

       IF vcuenta_tes > 1 THEN
          LET conta_tes = 0
          DECLARE curs_3 CURSOR FOR 
             SELECT folio_solicitud,grupo_regimen,ROWID
               FROM tes_solicitud
              WHERE nss             = afi.n_seguro
                AND tipo_traspaso   = 16
                AND fecha_solicitud = TODAY

          FOREACH curs_3 INTO vn_folio_tes,vgrupo_regm,vrowid

             LET conta_tes = conta_tes + 1

             IF conta_tes > 1 THEN
                LET vn_folio_tes2      = 0
                LET vn_folio_tes2      = vn_folio_tes + 1
                UPDATE tes_solicitud 
                   SET folio_solicitud = vn_folio_tes2
                 WHERE nss              = afi.n_seguro
                   AND tipo_traspaso      = 16
                   AND fecha_solicitud    = TODAY
                   AND ROWID              = vrowid
   
                -->actualiza  correlativo de marcas
                SELECT c.grupo_regimen  ,
                       t.marca_cod
                  INTO vgrupo_regm2,
                       vmarca_cod
                  FROM cta_nss_regimen c ,
                       tab_grupo_regimen t
                 WHERE c.nss = afi.n_seguro
                   AND t.grupo_regimen = c.grupo_regimen
                   AND t.ind_actualiza = 1
                   AND t.grupo_regimen = vgrupo_regm

                SELECT 'X'
                  FROM cta_act_marca
                 WHERE nss       = afi.n_seguro
                   AND fecha_ini = TODAY
                   AND marca_cod = vmarca_cod

                IF SQLCA.SQLCODE = 0 THEN
                   UPDATE cta_act_marca 
                      SET correlativo = vn_folio_tes2
                    WHERE nss         = afi.n_seguro
                      AND fecha_ini   = TODAY
                      AND marca_cod   = vmarca_cod
   
                   UPDATE cta_his_marca 
                      SET correlativo = vn_folio_tes2
                    WHERE nss         = afi.n_seguro
                      AND fecha_ini   = TODAY
                      AND marca_cod   = vmarca_cod
                END IF
                --<
             END IF

             LET vcuenta_tes   = 0
             LET vn_folio_tes  = 0
             LET vn_folio_tes2 = 0
             LET vrowid        = 0
          END FOREACH
       END IF
 
       -- -- contabilizando nuevos tipos de solicitud 
       -- CASE afi.tipo_solicitud
       --    WHEN  23 LET gi_x = 1
       --    WHEN  24 LET gi_x = 2
       --    WHEN  26 LET gi_x = 3
       --    WHEN  30 LET gi_x = 4
       -- END CASE

       -- contabilizando por tipo_afiliacion 
       LET lc_tipo_afi = 0
       LET gi_x = 0
       --DISPLAY "fn_busca_movil(): ",afi.n_seguro
       CALL fn_busca_movil(afi.n_seguro) RETURNING lc_tipo_afi
       --DISPLAY "lc_tipo_afi: ", lc_tipo_afi
       CASE lc_tipo_afi
          WHEN 60 LET gi_x = 1
          WHEN 61 LET gi_x = 2
          WHEN 2  LET gi_x = 3
          WHEN 3  LET gi_x = 4
          WHEN 4  LET gi_x = 5
          OTHERWISE LET gi_x = 6
             CALL f_guarda_rechazo(afi.*)
       END CASE

       IF v_rechazo <> 0 THEN
          INSERT INTO safre_tmp:rch_apertura
           VALUES (afi.n_seguro,v_rechazo)
       END IF
    END IF
    
    -- Por instrucciones de COPPEL CPL2958 no se realiza el marcaje de la cuenta    
    -- IF afi.status_interno = 100 THEN
    --    LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
    --                  "'",afi.n_seguro,"'",
    --                  ",",pmarca_entra,
    --                  ",",0,                    
    --                  ",",pestado_marca,
    --                  ",",pcodigo_rechazo,
    --                  ",",pmarca_causa,
    --                  ",","'","'", ",",
    --                  "'",gc_usuario,"'",")" 

    --    LET ejecuta = ejecuta CLIPPED

    --    PREPARE clausula_spl FROM ejecuta
    --    DECLARE cursor_marca CURSOR FOR clausula_spl
    --    OPEN cursor_marca
    --    FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo
    --    CLOSE cursor_marca
    -- END IF

    LET HORA = TIME

    INSERT INTO afi_ctr_logico
       VALUES (afi.n_folio,
               afi.tipo_solicitud,
               afi.n_seguro,
               afi.status_interno,
               gc_usuario,
               HOY,
               HORA,
               operacion  )

    INSERT INTO safre_tmp:afi_2
       VALUES (afi.n_seguro       ,
               afi.n_unico        ,
               afi.n_rfc          ,
               afi.paterno        ,
               afi.materno        ,
               afi.nombres        ,
               afi.fena           ,
               afi.sexo           ,
               afi.frecafor       ,
               afi.n_folio        ,
               afi.tipo_solicitud ,
               afi.status_interno ,
               v_rechazo          ,
               lc_tipo_afi )                 -- CPL2958 Afore movil 

    UPDATE afi_solicitud 
       SET afi_solicitud.status         = 100 ,
           afi_solicitud.status_interno = 100 ,
           afi_solicitud.status_captura = 100
     WHERE afi_solicitud.n_seguro       = afi.n_seguro
       AND afi_solicitud.n_folio        = afi.n_folio
       AND afi_solicitud.tipo_solicitud = afi.tipo_solicitud

    SELECT "X"
      FROM safre_af:rec_solicitud mr
     WHERE mr.n_seguro   = afi.n_seguro
       AND mr.origen_rec <> 1
     GROUP BY 1

    IF SQLCA.SQLCODE = 0 THEN
       UPDATE safre_af:rec_solicitud
          SET safre_af:rec_solicitud.origen_rec = 1
        WHERE safre_af:rec_solicitud.n_seguro = afi.n_seguro
          AND safre_af:rec_solicitud.origen_rec <> 1
    END IF      

    IF afi.n_unico IS NULL THEN
       LET sin_curp = sin_curp + 1
    ELSE
       LET con_curp = con_curp + 1
    END IF

    --DISPLAY "gi_x: ", gi_x
    IF  v_rechazo = 0                -- todo correcto
     OR v_rechazo = 3                -- se quiere cambiar al mismo regimen donde esta registrado
     OR v_rechazo = 91 THEN          -- se aceptaron todas las solicitudes de cambio regimen 
       LET ga_con[gi_x].ape = ga_con[gi_x].ape + 1
       LET ga_con[7].ape = ga_con[7].ape + 1
    ELSE
       LET ga_con[gi_x].noa = ga_con[gi_x].noa + 1
       LET ga_con[7].noa = ga_con[7].noa + 1
    END IF

 END FOREACH

 SELECT "X"
   FROM safre_tmp:afi_2
  GROUP BY 1

 LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",gc_usuario CLIPPED,
               ".REPORTE_APCTAS_APP." CLIPPED,
               HOY USING "dd-mm-yy","_",HORA CLIPPED

 IF STATUS = NOTFOUND THEN 
   START REPORT listado_1 TO G_LISTA
   OUTPUT TO REPORT listado_1(mensaje)
   FINISH REPORT listado_1
 ELSE
 
   START REPORT fn_listado TO G_LISTA
   LET gc_linea = '  CUENTAS APERTURADAS ', '|',
                  'FECHA: ', '|', 
                  HOY USING 'DD-MM-YYYY', '|'        
   OUTPUT TO REPORT fn_listado(gc_linea)
        
   LET gc_linea = 'FECHA APERTURA| USUARIO| FOLIO SOLICITUD | CURP | NSS | TIPO SOLICITUD |',
                  ' TIPO AFILIACION | ESTADO | SIEFORE APERTURA | COMENTARIOS |' 
   OUTPUT TO REPORT fn_listado(gc_linea)
   
   DECLARE cur_1 CURSOR FOR
      SELECT *
        FROM safre_tmp:afi_2
   FOREACH cur_1 INTO w_aux.*
      LET gc_linea = fn_arma_linea(w_aux.*) 
      OUTPUT TO REPORT fn_listado(gc_linea)
   END FOREACH
   
   FINISH REPORT fn_listado
 END IF 

 DISPLAY "Archivo generado ", G_LISTA CLIPPED AT 16,03

 LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,
	       "/",gc_usuario CLIPPED,".REPORTE_APCTAS_APP." CLIPPED,
               HOY USING "dd-mm-yy","_",HORA CLIPPED
 RUN G_LISTA

 # se invoca función que genera reporte de rechazos
 CALL f_gen_rep_rechazos()
END FUNCTION


FUNCTION despliega_resultados()
 DEFINE total_resp SMALLINT

 LET total_resp = con_curp + sin_curp

 DISPLAY " CUENTAS                  APERTURADAS   NO APERTURADAS   " AT 06,03
 DISPLAY ga_con[1].ley, '  ', 
         ga_con[1].ape USING '#######,##&', '  ', 
         ga_con[1].noa USING '####,##&'   AT 07,03
 DISPLAY ga_con[2].ley, '  ', 
         ga_con[2].ape USING '#######,##&', '  ', 
         ga_con[2].noa USING '####,##&'   AT 08,03
 DISPLAY ga_con[3].ley, '  ', 
         ga_con[3].ape USING '#######,##&', '  ', 
         ga_con[3].noa USING '####,##&'   AT 09,03
 DISPLAY ga_con[4].ley, '  ', 
         ga_con[4].ape USING '#######,##&', '  ', 
         ga_con[4].noa USING '####,##&'   AT 10,03
 DISPLAY ga_con[5].ley, '  ', 
         ga_con[5].ape USING '#######,##&', '  ', 
         ga_con[5].noa USING '####,##&'   AT 11,03
 DISPLAY ga_con[6].ley, '  ', 
         ga_con[6].ape USING '#######,##&', '  ', 
         ga_con[6].noa USING '####,##&'   AT 12,03
 DISPLAY ga_con[7].ley, '  ', 
         ga_con[7].ape USING '#######,##&', '  ', 
         ga_con[7].noa USING '####,##&'   AT 13,03

END FUNCTION


REPORT listado_1(mensaje)
 DEFINE mensaje             CHAR(50)

 OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

 FORMAT
    PAGE HEADER
    PRINT
       COLUMN 03,"========================================",
       COLUMN 40,"==================================="
    PRINT
       COLUMN 20," TRASPASO  AL  MAESTRO  DE  AFILIADOS APP"
    PRINT
       COLUMN 03,"----------------------------------------",
       COLUMN 40,"-----------------------------------"
    PRINT 
    PRINT 
    PRINT 
       COLUMN 15,mensaje

END REPORT

--## genera linea
FUNCTION  fn_arma_linea(w_aux)
 DEFINE  lc_linea                   CHAR(350)
 DEFINE  lc_desc                    CHAR(50)
 DEFINE  ls_sie                     SMALLINT  

 DEFINE w_aux             RECORD
    n_seguro              LIKE afi_solicitud.n_seguro,
    n_unico               LIKE afi_solicitud.n_unico ,
    n_rfc                 LIKE afi_solicitud.n_rfc   ,
    paterno               LIKE afi_solicitud.paterno ,
    materno               LIKE afi_solicitud.materno ,
    nombres               LIKE afi_solicitud.nombres ,
    fena                  LIKE afi_solicitud.fena    ,
    sexo                  LIKE afi_solicitud.sexo    ,
    frecafor              LIKE afi_solicitud.frecafor,
    n_folio               LIKE afi_solicitud.n_folio ,
    tipo_sol              LIKE afi_solicitud.tipo_solicitud,
    sta_int               SMALLINT,
    rechaza               SMALLINT,
    lc_tipo_afi           SMALLINT
 END RECORD
 DEFINE lr_sol            RECORD LIKE afi_solicitud.*
 DEFINE lr_afi            RECORD LIKE afi_mae_afiliado.*
 
 DEFINE lr_sal            RECORD
    fec_ape               DATE,
    usu                   CHAR(10),
    n_folio               DECIMAL(10,0),
    curp                  CHAR(18),
    nss                   CHAR(11),
    tipo_sol              CHAR(50),
    sta_int               CHAR(50),
    siefore               CHAR(50),
    des_tipo              CHAR(40),
    des_come              CHAR(70)
 END RECORD    
     
 #CPL-3585 Se comentan las condiciones n_folio y tipo_solicitud
 SELECT *
   INTO lr_afi.*
   FROM afi_mae_afiliado
  WHERE n_seguro = w_aux.n_seguro 
    --AND n_folio  = w_aux.n_folio
    --AND tipo_solicitud = w_aux.tipo_sol
  
 IF STATUS = NOTFOUND THEN 
    SELECT *
      INTO lr_sol.* 
      FROM afi_solicitud A
     WHERE A.n_seguro = w_aux.n_seguro
       --AND A.tipo_solicitud IN( 23,24,26,30)
       AND A.tipo_solicitud IN( 23,24,26,30,38,39,40,41) -- CPL2958 Afore movil --CPL-3430 Se agrega Afore Web
       AND A.n_folio = w_aux.n_folio --CPL-3585 Se agrega condición n_folio solucionando el error reportado "A subquery has returned not exactly one row"
     ORDER BY n_seguro

    LET lr_sal.fec_ape  = ' '
    LET lr_sal.usu      = ' '
    LET lr_sal.n_folio  = lr_sol.n_folio
    LET lr_sal.curp     = lr_sol.n_unico
    LET lr_sal.nss      = w_aux.n_seguro
    LET lr_sal.sta_int  = ' '
 ELSE 
    LET lr_sal.fec_ape  = lr_afi.frecafor
    LET lr_sal.usu      = gc_usuario
    LET lr_sal.n_folio  = lr_afi.n_folio
    LET lr_sal.curp     = lr_afi.n_unico
    LET lr_sal.nss      = w_aux.n_seguro

    LET lc_desc = ' '
    SELECT estado_desc
      INTO lc_desc 
      FROM tab_status_afi
     WHERE estado_cod = lr_afi.status_interno
    LET lr_sal.sta_int  = lr_afi.status_interno USING '&&&', '-', lc_desc CLIPPED
 END IF

 LET lr_sal.tipo_sol = ' ' 
 LET lc_desc = ' '
 SELECT desc_solicitud 
   INTO lc_desc
   FROM tab_tipo_solic
  WHERE tipo_solicitud = w_aux.tipo_sol
 LET lr_sal.tipo_sol = w_aux.tipo_sol USING '&&&', '-', lc_desc CLIPPED
 
 SELECT codigo_siefore
   INTO ls_sie 
   FROM cta_regimen
  WHERE subcuenta = 1
    AND nss = w_aux.n_seguro

 IF STATUS = NOTFOUND THEN 
    LET lr_sal.siefore = ' '
 ELSE
     LET lc_desc = ' '
     SELECT siefore_desc
       INTO lc_desc
       FROM tab_siefore
      WHERE afore_cod  IN (SELECT UNIQUE (codigo_afore)
                             FROM tab_siefore_local)
        AND siefore_cod = ls_sie
     LET lr_sal.siefore  = ls_sie USING '&&', '-', lc_desc CLIPPED 
 END IF

 -- CPL2958 descripcion de tipo de solicitud por registro 
 CASE w_aux.lc_tipo_afi
    WHEN 60 LET lr_sal.des_tipo = '60-Asignado misma Afore         '
    WHEN 61 LET lr_sal.des_tipo = '61-Asignado distinta Afore      '
    WHEN 2  LET lr_sal.des_tipo = '02-No Afiliado                  '
    WHEN 3  LET lr_sal.des_tipo = '03-Registrado                   '
    WHEN 4  LET lr_sal.des_tipo = '04-IMSS puro                    '
 END CASE 
 
 IF  w_aux.rechaza = 0                -- todo correcto
  OR w_aux.rechaza = 3                -- se quiere cambiar al mismo regimen donde esta registrado
  OR w_aux.rechaza = 91 THEN          -- se aceptaron todas las solicitudes de cambio regimen 
    LET lr_sal.des_come = 'CUENTA APERTURADA '                     -- CPL2958
 ELSE
    LET lr_sal.fec_ape = ' '
    LET lr_sal.siefore = ' ' 
    -- CPL2958 descripcion de error SP en comentarios 
    CASE w_aux.rechaza 
       WHEN 1 LET lr_sal.des_come =  '1-El régimen de inversión inválido para tipo trabajador        '
       WHEN 2 LET lr_sal.des_come =  '2-Error por convivencia de marcas                              '
       WHEN 6 LET lr_sal.des_come =  '6-Error por convivencia de marcas                              '
       WHEN 92 LET lr_sal.des_come = '92-Error en generación solicitudes transferencia entre siefores'
       WHEN 90 LET lr_sal.des_come = '90-Error en generación solicitudes transferencia entre siefores'
       WHEN -1 LET lr_sal.des_come = '-1-No existe información en maestro de afiliados               ' 
    END CASE 
 END IF

 LET lc_linea = lr_sal.fec_ape,          '|',
                lr_sal.usu,              '|',
                lr_sal.n_folio,          '|',
                lr_sal.curp,             '|',
                lr_sal.nss,              '|',
                lr_sal.tipo_sol CLIPPED, '|',
                lr_sal.des_tipo CLIPPED, '|',
                lr_sal.sta_int  CLIPPED, '|',
                lr_sal.siefore  CLIPPED, '|',
                lr_sal.des_come CLIPPED, '|'
 LET lc_linea = lc_linea CLIPPED

 RETURN lc_linea
END FUNCTION


--### Imprime 
REPORT  fn_listado(lc_linea)
   DEFINE num_reg                    INTEGER
   DEFINE lc_linea                   CHAR(350)

   OUTPUT
      TOP MARGIN     0
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH    1

   FORMAT

   ON EVERY ROW
      PRINT COLUMN  01, lc_Linea 

END REPORT


FUNCTION fn_busca_movil(lc_nss)
 DEFINE li_id            DECIMAL(12,0)
 DEFINE lc_nss           CHAR(11)
 DEFINE lc_tipo_afi      SMALLINT

 DECLARE dc_movil CURSOR FOR 
    SELECT a.id_folio_movil, a.tipo_afiliacion 
      FROM afi_solicitud_movil a
     WHERE a.status = '01'
       AND a.nss = lc_nss 
     ORDER BY 1

 FOREACH dc_movil INTO  li_id, lc_tipo_afi
 --   Display 'entr ' , li_id, ' ', lc_tipo_afi 
 END FOREACH 
 --DISPLAY ' lc_nss ', lc_nss, ' ', lc_tipo_afi 
 RETURN lc_tipo_afi
END FUNCTION

# CPL-3173
FUNCTION desmarca_cuenta (vnss, vmarca, vusuario, vcorrelativo)
 DEFINE vnss           CHAR(11),
        vmarca         SMALLINT,
        vusuario       CHAR(08),
        vcorrelativo   INTEGER,
        vestado_marca  SMALLINT,
        vmarca_causa   SMALLINT

   LET vestado_marca = 0
   LET vmarca_causa  = 0

   PREPARE eje_desmarca FROM v_desmarca
   EXECUTE eje_desmarca USING vnss,
                              vmarca,
                              vcorrelativo,
                              vestado_marca,
                              vmarca_causa,
                              vusuario
END FUNCTION 
# CPL-3173

#Objetivo: Funcion que guarda rechazo en tabla de rechazos
FUNCTION f_guarda_rechazo(v_r_sol_afi)
   DEFINE v_r_sol_afi    RECORD LIKE afi_solicitud.*
   DEFINE v_rec_rechazos RECORD
     n_seguro            CHAR(11),
     n_unico             CHAR(18),
     n_rfc               CHAR(13),
     paterno             CHAR(40),
     materno             CHAR(40),
     nombres             CHAR(40),
     fena                DATE,
     sexo                SMALLINT,
     frecafor            DATE,
     n_folio             DECIMAL(10,0),
     tipo_sol            SMALLINT,
     sta_int             SMALLINT,
     motivo_rech         VARCHAR(40)
   END RECORD

   LET v_rec_rechazos.n_seguro     = v_r_sol_afi.n_seguro
   LET v_rec_rechazos.n_unico      = v_r_sol_afi.n_unico
   LET v_rec_rechazos.n_rfc        = v_r_sol_afi.n_rfc
   LET v_rec_rechazos.paterno      = v_r_sol_afi.paterno
   LET v_rec_rechazos.materno      = v_r_sol_afi.materno
   LET v_rec_rechazos.nombres      = v_r_sol_afi.nombres
   LET v_rec_rechazos.fena         = v_r_sol_afi.fena
   LET v_rec_rechazos.sexo         = v_r_sol_afi.sexo
   LET v_rec_rechazos.frecafor     = v_r_sol_afi.frecafor
   LET v_rec_rechazos.n_folio      = v_r_sol_afi.n_folio
   LET v_rec_rechazos.tipo_sol     = v_r_sol_afi.tipo_solicitud
   LET v_rec_rechazos.sta_int      = v_r_sol_afi.status_interno
   LET v_rec_rechazos.motivo_rech  = 'TIPO AFILIACIÓN NO IDENTIFICADO'

   INSERT INTO safre_tmp:afi_rech VALUES (v_rec_rechazos.*)
END FUNCTION

#Objetivo: Funcion que genera reporte de rechazos en caso de existir rechazos duranto el proceso
FUNCTION f_gen_rep_rechazos()
   DEFINE v_num_reg      INTEGER
   DEFINE v_rec_rechazos RECORD
     n_seguro            CHAR(11),
     n_unico             CHAR(18),
     n_rfc               CHAR(13),
     paterno             CHAR(40),
     materno             CHAR(40),
     nombres             CHAR(40),
     fena                DATE,
     sexo                SMALLINT,
     frecafor            DATE,
     n_folio             DECIMAL(10,0),
     tipo_sol            SMALLINT,
     sta_int             SMALLINT,
     motivo_rechazo      VARCHAR(40)
   END RECORD
   DEFINE v_cadena       CHAR(300)
   DEFINE lc_linea       CHAR(350)

   # se verifica si existen rechazos
   SELECT COUNT(*)
     INTO v_num_reg
     FROM safre_tmp:afi_rech

   IF v_num_reg = 0 THEN
      RETURN
   END IF

   LET v_cadena = g_paramgrales.ruta_listados CLIPPED,"/",gc_usuario CLIPPED,
                  ".REPORTE_RECHAZOS." CLIPPED,
                  HOY USING "dd-mm-yy","_",HORA CLIPPED

   START REPORT fn_rechazos TO v_cadena

   LET lc_linea = 'CUENTAS NO IDENTIFICADAS   ','   ','FECHA: ', '|', (HOY USING 'DD-MM-YYYY'), '|'
   OUTPUT TO REPORT fn_rechazos(lc_linea)

   LET lc_linea = 'FOLIO SOLICITUD | CURP | NSS | TIPO SOLICITUD | ESTATUS | MOTIVO RECHAZO |' 
   OUTPUT TO REPORT fn_rechazos(lc_linea)

   DECLARE cur_rechazos CURSOR FOR
   SELECT *
     FROM safre_tmp:afi_rech

   FOREACH cur_rechazos INTO v_rec_rechazos.*
      LET lc_linea = v_rec_rechazos.n_folio,'|',
                     v_rec_rechazos.n_unico,'|',
                     v_rec_rechazos.n_seguro,'|',
                     v_rec_rechazos.tipo_sol,'|',
                     v_rec_rechazos.sta_int CLIPPED, '|',
                     v_rec_rechazos.motivo_rechazo CLIPPED, '|'

      LET lc_linea = lc_linea CLIPPED

      OUTPUT TO REPORT fn_rechazos(lc_linea)
   END FOREACH

   FINISH REPORT fn_rechazos

   DISPLAY "Rep inconsistenc ", v_cadena CLIPPED AT 17,03

   LET v_cadena = "chmod 777 ",v_cadena
   RUN v_cadena
END FUNCTION

--### Imprime 
REPORT  fn_rechazos(lc_linea)
   DEFINE num_reg                    INTEGER
   DEFINE lc_linea                   CHAR(350)

   OUTPUT
      TOP MARGIN     0
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH    1

   FORMAT

   ON EVERY ROW
      PRINT COLUMN  01, lc_Linea 

END REPORT
