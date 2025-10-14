################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa RETM900  => ADMINISTRACION DE SOLICITUDES DE RETIROS-ISSSTE          #
#                     PARA PARCIALES                                           #
#Fecha creacion    => 02 DE OCTUBRE DEL 2006                                   #
#By                => STEFANIE DANIELA VERA PIÑA                               #
#Sistema           => RET                                                      #
#Modifico          => XAVIER TORRES RIOS                                       #
#                  => 22 de Noviembre 2007                                     #
#                     Se permite capturar el nss_issste o si se captura el     #
#                     nss_imss o la curp trae el valor que tenga en la tabla   #
#                     cta_ctr_reg_ind                                          #
################################################################################
DATABASE safre_af  
GLOBALS
    DEFINE reg RECORD #glo #reg
        nss_imss           LIKE ret_sol_issste_par.nss_imss             ,
        rfc                LIKE ret_sol_issste_par.rfc                  ,
        curp               LIKE ret_sol_issste_par.curp                 ,
        paterno            LIKE ret_sol_issste_par.paterno              ,
        materno            LIKE ret_sol_issste_par.materno              ,
        nombres            LIKE ret_sol_issste_par.nombres              ,
        tipo_inversion     LIKE ret_sol_issste_par.tipo_inversion       ,
        desc_tipo_inv      LIKE tab_tipo_administracion.desc_tipo_admon ,
        tipo_solicitud     LIKE afi_mae_afiliado.tipo_solicitud         ,
        desc_tipo_sol      LIKE tab_tipo_solic.desc_solicitud           ,
        tipo_trabajador    LIKE tab_tipo_trab_ind.tipo_trab_ind         ,
        desc_tipo_trab     LIKE tab_tipo_trab_ind.desc_tipo_trab_ind    ,
        nss_issste         LIKE ret_sol_issste_par.nss_issste           ,
        tipo_retiro        LIKE ret_sol_issste_par.tipo_retiro          ,
        des_tipo_retiro    LIKE tab_retiro_issste.desc_retiro           ,
        tipo_beneficio     LIKE ret_sol_issste_par.tipo_beneficio       ,
        desc_tipo_benef    LIKE tab_beneficio_issste.desc_beneficio     ,
        fecha_baja         LIKE ret_sol_issste_par.fecha_baja           ,
        folio_dictamen     LIKE ret_sol_issste_par.folio_dictamen       ,
        fecha_valoriza     LIKE ret_sol_issste_par.fecha_valoriza       ,
        folio_solicitud    LIKE ret_sol_issste_par.folio_solicitud      ,
        fecha_solicitud    LIKE ret_sol_issste_par.fecha_solicitud      ,
        fecha_entrega_rec  LIKE ret_sol_issste_par.fecha_entrega_rec    ,
        rechazo_cod        LIKE ret_sol_issste_par.rechazo_cod          ,
        rechazo_desc       LIKE tab_rch_marca.rechazo_desc              ,
        fecha_captura      LIKE ret_sol_issste_par.fecha_captura        ,
        fecha_liquida      DATE                                         ,
        usuario_captura    LIKE ret_sol_issste_par.usuario_captura      ,
        folio              LIKE ret_sol_issste_par.folio                ,
        estado_solicitud   LIKE ret_sol_issste_par.estado_solicitud     ,
        desc_estado        LIKE ret_estado.descripcion                  ,
        consecutivo        LIKE ret_sol_issste_par.consecutivo
    END RECORD

    DEFINE arr_1 ARRAY[1000] OF RECORD #glo #arr_1
        nss_imss           LIKE ret_sol_issste_par.nss_imss             ,
        rfc                LIKE ret_sol_issste_par.rfc                  ,
        curp               LIKE ret_sol_issste_par.curp                 ,
        paterno            LIKE ret_sol_issste_par.paterno              ,
        materno            LIKE ret_sol_issste_par.materno              ,
        nombres            LIKE ret_sol_issste_par.nombres              ,
        tipo_inversion     LIKE ret_sol_issste_par.tipo_inversion       ,
        desc_tipo_inv      LIKE tab_tipo_administracion.desc_tipo_admon ,
        tipo_solicitud     LIKE afi_mae_afiliado.tipo_solicitud         ,
        desc_tipo_sol      LIKE tab_tipo_solic.desc_solicitud           ,
        tipo_trabajador    LIKE tab_tipo_trab_ind.tipo_trab_ind         ,
        desc_tipo_trab     LIKE tab_tipo_trab_ind.desc_tipo_trab_ind    ,
        nss_issste         LIKE ret_sol_issste_par.nss_issste           ,
        tipo_retiro        LIKE ret_sol_issste_par.tipo_retiro          ,
        des_tipo_retiro    LIKE tab_retiro_issste.desc_retiro           ,
        tipo_beneficio     LIKE ret_sol_issste_par.tipo_beneficio       ,
        desc_tipo_benef    LIKE tab_beneficio_issste.desc_beneficio     ,
        fecha_baja         LIKE ret_sol_issste_par.fecha_baja           ,
        folio_dictamen     LIKE ret_sol_issste_par.folio_dictamen       ,
        fecha_valoriza     LIKE ret_sol_issste_par.fecha_valoriza       ,
        folio_solicitud    LIKE ret_sol_issste_par.folio_solicitud      ,
        fecha_solicitud    LIKE ret_sol_issste_par.fecha_solicitud      ,
        fecha_entrega_rec  LIKE ret_sol_issste_par.fecha_entrega_rec    ,
        rechazo_cod        LIKE ret_sol_issste_par.rechazo_cod          ,
        rechazo_desc       LIKE tab_rch_marca.rechazo_desc              ,
        fecha_captura      LIKE ret_sol_issste_par.fecha_captura        ,
        fecha_liquida      DATE                                         ,
        usuario_captura    LIKE ret_sol_issste_par.usuario_captura      ,
        folio              LIKE ret_sol_issste_par.folio                ,
        estado_solicitud   LIKE ret_sol_issste_par.estado_solicitud     ,
        desc_estado        LIKE ret_estado.descripcion                  ,
        consecutivo        LIKE ret_sol_issste_par.consecutivo
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2
        capturado          LIKE ret_estado.estado_solicitud ,
        confirmado         LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE reg_20 RECORD
        estado_marca       SMALLINT ,
        codigo_rechazo     SMALLINT ,
        marca_causa        SMALLINT , 
        fecha_causa        DATE
    END RECORD

    DEFINE reg_rev RECORD
        nss                CHAR(11) ,
        marca_cod          SMALLINT ,
        correlativo        INTEGER
    END RECORD

    DEFINE #glo #date
        d_primero_mes      ,
        HOY                ,
        vfecha_causa       ,
        vfecha_modifica    ,
        vfecha_confirma    DATE
        
    DEFINE #glo #char
        desc_rechazo_cod   CHAR(40)   ,
        enter              CHAR(1)    ,
        txt_1              CHAR(2800) ,
        opc                CHAR(1)    ,
        usuario            CHAR(8)    ,
        v_marca            CHAR(100)  ,
        v_desmarca         CHAR(100)  ,
        v_reversa          CHAR(100)  ,
        v_ejecuta          CHAR(500)  ,
        vfecha_val         CHAR(150)  ,
        x_busca            CHAR(1200) ,
        x_error            CHAR(500)  ,  
        x_estado_solicitud CHAR(40)   ,
        x_usuario          CHAR(12)  

    DEFINE #glo #smallint
        arr_c		   ,
        cont_reg           ,
        pos		   ,
        s_tipo_movimiento  ,
        sw                 ,
        vcodigo_rechazo    ,
        vestado_marca      ,
        v_rechazo_cod      ,
        vmarca_causa       ,
        v_marca_res        ,
        v_codafore         ,
	v_marca_ent        SMALLINT

    DEFINE
        vtipo_solicitud    LIKE afi_mae_afiliado.tipo_solicitud      ,
        vdesc_tipo_sol     LIKE tab_tipo_solic.desc_solicitud        ,
        vtipo_trabajador   LIKE cta_ctr_reg_ind.tipo_trab_ind        ,
        vdesc_tipo_trab    LIKE tab_tipo_trab_ind.desc_tipo_trab_ind ,
        vusuario_modif     LIKE ret_sol_issste_par.usuario_modifica  ,
        vusuario_confirma  LIKE ret_sol_issste_par.usuario_confirma

END GLOBALS

MAIN   
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I 

    CALL init()
    CALL STARTLOG("RETM900.log")   

    OPEN WINDOW retm9001 AT 2,3 WITH FORM "RETM9001" ATTRIBUTE (BORDER)
    DISPLAY " RETM900       SOLICITUD DE RETIROS-ISSSTE PARA PARCIALES                      " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    MENU "MENU" 
        COMMAND KEY(A) "(A)grega" "Agrega solicitud"
            CALL inicializa()
            CALL agrega() #a
       
        COMMAND KEY(C) "(C)onsulta" "Consulta solicitud"
            CALL inicializa()
            CALL consulta() #c
   
        COMMAND KEY(M) "(M)odifica" "Modifica solicitud"
            CALL inicializa()
            CALL modifica() #m

        COMMAND KEY(E) "(E)limina" "Elimina solicitud"
            CALL inicializa()
            CALL elimina() #e

        COMMAND KEY("F") "con(F)irma" "Confirma Solicitud" 
            CALL inicializa()
            CALL confirma() #cc   

        COMMAND KEY(S) "(S)alida" "Vuelve al menu"
            EXIT PROGRAM
    END MENU
CLOSE WINDOW retm9001
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT USER,codigo_afore
    INTO   usuario,v_codafore
    FROM   tab_afore_local

    SELECT A.estado_solicitud
    INTO   reg_2.capturado
    FROM   ret_estado A
    WHERE  A.descripcion      = "CAPTURADO"
    AND    A.estado_solicitud <> 0

    SELECT A.estado_solicitud
    INTO   reg_2.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    ----- MARCAJE ------
    LET v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"

    LET reg_20.estado_marca   = 0
    LET reg_20.codigo_rechazo = 0
    LET reg_20.marca_causa    = 0
    INITIALIZE reg_20.fecha_causa TO NULL

    ----- DESMARCAJE ------
    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

    PREPARE eje_desmarca FROM v_desmarca

    LET v_marca_ent = 0

{PENDIENTE
    LET vfecha_val = " EXECUTE FUNCTION fn_obten_fecha_val ( ? ) "
    PREPARE eje_fecha_val FROM vfecha_val

    DECLARE cur_obten_fecha_val CURSOR FOR eje_fecha_val
    OPEN cur_obten_fecha_val USING HOY
        FETCH cur_obten_fecha_val INTO d_primero_mes
    CLOSE cur_obten_fecha_val
}
END FUNCTION

FUNCTION inicializa()
#i-------------------
    INITIALIZE reg.* TO NULL
    CLEAR FORM
END FUNCTION

FUNCTION agrega()
#a---------------
    DEFINE #loc #smallint
        sw_1                       SMALLINT

    DEFINE #loc #integer
        ult_consecutivo            INTEGER

    OPEN WINDOW retm9002 AT 2,3 WITH FORM "RETM9002" ATTRIBUTE (BORDER)
    DISPLAY " RETM900                  DATOS DEL TRABAJADOR                                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "AGREGA" AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY " <Esc> : Agrega    Ctrl-B:Dom Trab/Benef    Ctrl-C : Salir " AT 2,1
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 10,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 
    ATTRIBUTE(REVERSE)

    LET sw_1              = 0
    LET reg.fecha_captura = HOY
    LET reg.rechazo_cod   = 0

    DISPLAY BY NAME reg.fecha_captura

    INPUT BY NAME reg.nss_imss           ,
		  reg.curp               ,
                  reg.nss_issste         ,
                  reg.tipo_retiro        ,
                  reg.tipo_beneficio     ,
                  reg.fecha_baja         ,
                  reg.folio_dictamen     ,
                  reg.fecha_valoriza     ,
                  reg.folio_solicitud    ,
                  reg.fecha_solicitud    ,
                  reg.fecha_entrega_rec  WITHOUT DEFAULTS

        BEFORE FIELD nss_imss
            IF sw_1 = 0 THEN

                SELECT MAX(consecutivo)+1
                INTO   ult_consecutivo
                FROM   ret_consecutivo

                INSERT INTO ret_consecutivo VALUES (ult_consecutivo)

                LET reg.consecutivo    = ult_consecutivo
                LET reg.fecha_solicitud   = HOY
                LET reg.fecha_captura     = HOY 
                LET reg.usuario_captura   = usuario

                DISPLAY BY NAME reg.fecha_solicitud   ,
                                reg.fecha_captura     ,
                                reg.consecutivo       ,
                                reg.usuario_captura   ,
                                reg.fecha_entrega_rec
                LET sw_1 = 1
            END IF

        AFTER FIELD nss_imss
            LET reg.fecha_captura = HOY

            IF reg.nss_imss IS NOT NULL THEN

                SELECT n_unico ,
                       n_rfc   ,
                       paterno ,
                       materno ,
                       nombres 
                INTO   reg.curp ,
                       reg.rfc  ,
                       reg.paterno ,
                       reg.materno ,
                       reg.nombres
                FROM   afi_mae_afiliado
                WHERE  n_seguro = reg.nss_imss
                GROUP BY 1,2,3,4,5

                IF STATUS = NOTFOUND THEN
                    PROMPT "  TRABAJADOR NO AFILIADO A LA AFORE " FOR CHAR enter

                    INITIALIZE reg.nss_imss TO NULL
                    CLEAR nss_imss  

                    NEXT FIELD curp
                ELSE
                    SELECT tipo_solicitud
                    INTO   vtipo_solicitud
                    FROM   afi_mae_afiliado 
                    WHERE  n_seguro = reg.nss_imss

                    SELECT desc_solicitud
                    INTO   vdesc_tipo_sol
                    FROM   tab_tipo_solic
                    WHERE  tipo_solicitud = vtipo_solicitud         

             { SELECT nss_issste into rg_dato.nss_issste
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
             END IF }
 
                    SELECT tipo_trab_ind,tipo_administracion, nss_issste
                    INTO   vtipo_trabajador,reg.tipo_inversion ,reg.nss_issste
                    FROM   cta_ctr_reg_ind
                    WHERE  nti = reg.nss_imss

                    IF STATUS = NOTFOUND THEN
                        LET reg.tipo_inversion = 1
                        
                        LET vdesc_tipo_trab = "IMSS"
                    ELSE
                        SELECT desc_tipo_trab_ind
                        INTO   vdesc_tipo_trab
                        FROM   tab_tipo_trab_ind
                        WHERE  tipo_trab_ind = vtipo_trabajador
                    END IF

                    SELECT desc_tipo_admon
                    INTO   reg.desc_tipo_inv
                    FROM   tab_tipo_administracion
                    WHERE  tipo_administracion = reg.tipo_inversion

                    CASE reg.tipo_inversion
                        WHEN 1
                            LET reg.fecha_valoriza = HOY
                    END CASE

                    DISPLAY BY NAME reg.curp
                    DISPLAY BY NAME reg.rfc
                    DISPLAY BY NAME reg.nss_issste
                    DISPLAY BY NAME reg.paterno
                    DISPLAY BY NAME reg.materno
                    DISPLAY BY NAME reg.nombres
                    DISPLAY BY NAME reg.fecha_valoriza
                    DISPLAY BY NAME reg.tipo_inversion
                    DISPLAY reg.desc_tipo_inv TO desc_tipo_inversion
                    DISPLAY vtipo_solicitud   TO tipo_solicitud 
                    DISPLAY vdesc_tipo_sol    TO desc_tipo_sol
                    DISPLAY vtipo_trabajador  TO tipo_trabajador
                    DISPLAY vdesc_tipo_trab   TO desc_tipo_trab
                    DISPLAY reg.nss_issste    TO nss_issste      

                    NEXT FIELD nss_issste
                END IF
            END IF

        AFTER FIELD curp
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF

            IF reg.curp IS NULL AND reg.nss_imss IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD curp             
            ELSE
                SELECT n_seguro
                INTO   reg.nss_imss
                FROM   afi_mae_afiliado 
                WHERE  n_unico = reg.curp

                SELECT n_unico ,
                       n_rfc   ,
                       paterno ,
                       materno ,
                       nombres ,
                       n_seguro
                INTO   reg.curp ,
                       reg.rfc  ,
                       reg.paterno ,
                       reg.materno ,
                       reg.nombres ,
                       reg.nss_imss
                FROM   afi_mae_afiliado
                WHERE  n_seguro = reg.nss_imss
                GROUP BY 1,2,3,4,5,6

                IF STATUS = NOTFOUND THEN
                    PROMPT "  CURP INEXISTENTE " FOR CHAR enter

                    INITIALIZE reg.curp TO NULL
                    CLEAR curp

                    NEXT FIELD curp
                ELSE
                    SELECT tipo_solicitud
                    INTO   vtipo_solicitud
                    FROM   afi_mae_afiliado
                    WHERE  n_seguro = reg.nss_imss

                    SELECT desc_solicitud
                    INTO   vdesc_tipo_sol
                    FROM   tab_tipo_solic
                    WHERE  tipo_solicitud = vtipo_solicitud

             {  IF rg_dato.nss_issste IS NOT NULL AND 
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
                END IF }                                  

                    SELECT tipo_trab_ind,tipo_administracion, nss_issste
                    INTO   vtipo_trabajador,reg.tipo_inversion, reg.nss_issste
                    FROM   cta_ctr_reg_ind
                    WHERE  nti = reg.nss_imss

                    IF STATUS = NOTFOUND THEN
                        LET reg.tipo_inversion = 1

                        LET vdesc_tipo_trab = "IMSS"
                    ELSE
                        SELECT desc_tipo_trab_ind
                        INTO   vdesc_tipo_trab
                        FROM   tab_tipo_trab_ind
                        WHERE  tipo_trab_ind = vtipo_trabajador
                    END IF

                    SELECT desc_tipo_admon
                    INTO   reg.desc_tipo_inv
                    FROM   tab_tipo_administracion
                    WHERE  tipo_administracion = reg.tipo_inversion

                    CASE reg.tipo_inversion
                        WHEN 1
                            LET reg.fecha_valoriza = HOY
                    END CASE

                    DISPLAY BY NAME reg.nss_imss
                    DISPLAY BY NAME reg.nss_issste
                    DISPLAY BY NAME reg.curp
                    DISPLAY BY NAME reg.rfc
                    DISPLAY BY NAME reg.paterno
                    DISPLAY BY NAME reg.materno
                    DISPLAY BY NAME reg.nombres
                    DISPLAY BY NAME reg.fecha_valoriza
                    DISPLAY BY NAME reg.tipo_inversion
                    DISPLAY reg.desc_tipo_inv TO desc_tipo_inversion
                    DISPLAY reg.nss_issste    TO nss_issste          
                    DISPLAY vtipo_solicitud   TO tipo_solicitud
                    DISPLAY vdesc_tipo_sol    TO desc_tipo_sol
                    DISPLAY vtipo_trabajador  TO tipo_trabajador
                    DISPLAY vdesc_tipo_trab   TO desc_tipo_trab
                END IF
            END IF
        
        AFTER FIELD nss_issste 
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF

        AFTER FIELD tipo_retiro 
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF
 
            IF reg.tipo_retiro IS NULL OR 
               reg.tipo_retiro = 0     THEN
                CALL despliega_tipo_retiro()  #dtr
                DISPLAY reg.tipo_retiro TO tipo_retiro
                DISPLAY reg.des_tipo_retiro TO des_tipo_retiro
            ELSE
                IF  reg.tipo_retiro <> 4
                AND reg.tipo_retiro <> 5 THEN
                    ERROR ""
                    ERROR "   SOLO PUEDE ELEGIR OPCION 4 ó 5  "
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD tipo_retiro
                END IF

                SELECT "OK"
                FROM   tab_retiro_issste
                WHERE  tipo_retiro = reg.tipo_retiro
            
                IF STATUS <> NOTFOUND THEN
                   SELECT desc_retiro 
                   INTO   reg.des_tipo_retiro
                   FROM   tab_retiro_issste
                   WHERE  tipo_retiro = reg.tipo_retiro
 
                   DISPLAY reg.des_tipo_retiro TO des_tipo_retiro
                ELSE
                   ERROR " TIPO DE RETIRO ISSSTE INEXISTENTE ... "
                   NEXT FIELD tipo_retiro 
                END IF  
            END IF  

        AFTER FIELD tipo_beneficio 
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF

            IF reg.tipo_beneficio IS NULL OR 
               reg.tipo_beneficio = 0     THEN
                CALL despliega_tipo_beneficio()  #dtb
                DISPLAY reg.tipo_beneficio TO tipo_beneficio
                DISPLAY reg.desc_tipo_benef TO desc_tipo_benef
            ELSE
                SELECT "OK"
                FROM   tab_beneficio_issste
                WHERE  tipo_beneficio = reg.tipo_beneficio
            
                IF STATUS <> NOTFOUND THEN
                    SELECT desc_beneficio 
                    INTO   reg.desc_tipo_benef
                    FROM   tab_beneficio_issste
                    WHERE  tipo_beneficio = reg.tipo_beneficio
                 
                    DISPLAY reg.desc_tipo_benef TO desc_tipo_benef
                ELSE
                    ERROR " TIPO DE BENEFICIO ISSSTE INEXISTENTE ... "
                    NEXT FIELD tipo_beneficio 
                END IF  
            END IF  

        AFTER FIELD fecha_baja
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF

            IF reg.fecha_baja IS NULL THEN
                IF  reg.tipo_retiro = 5 
                AND reg.tipo_beneficio = 777 THEN
                    ERROR "    CAMPO NO PUEDE SER NULO"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_baja
                END IF
            END IF
                   
        AFTER FIELD folio_dictamen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF

            IF reg.folio_dictamen IS NULL THEN
                IF  reg.tipo_retiro = 4 
                AND reg.tipo_beneficio = 666 THEN
                    ERROR "    CAMPO NO PUEDE SER NULO"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_dictamen
                END IF
            END IF

        AFTER FIELD fecha_valoriza
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF

        AFTER FIELD folio_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

        AFTER FIELD fecha_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF reg.fecha_solicitud IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_solicitud
            ELSE
                IF reg.fecha_solicitud > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_solicitud
                END IF
            END IF

        AFTER FIELD fecha_entrega_rec
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF reg.tipo_retiro = 4 AND reg.fecha_entrega_rec IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_entrega_rec
            END IF
             
      ON KEY (ESC)
          IF reg.nss_imss IS NOT NULL THEN

              SELECT n_unico ,
                     n_rfc   ,
                     paterno ,
                     materno ,
                     nombres
              INTO   reg.curp ,
                     reg.rfc  ,
                     reg.paterno ,
                     reg.materno ,
                     reg.nombres
              FROM   afi_mae_afiliado
              WHERE  n_seguro = reg.nss_imss
              GROUP BY 1,2,3,4,5

              IF STATUS = NOTFOUND THEN
                  PROMPT "  TRABAJADOR NO AFILIADO A LA AFORE " FOR CHAR enter

                  INITIALIZE reg.nss_imss TO NULL
                  CLEAR nss_imss

                  NEXT FIELD curp
              ELSE
                  SELECT tipo_solicitud
                  INTO   vtipo_solicitud
                  FROM   afi_mae_afiliado
                  WHERE  n_seguro = reg.nss_imss

                  SELECT desc_solicitud
                  INTO   vdesc_tipo_sol
                  FROM   tab_tipo_solic
                  WHERE  tipo_solicitud = vtipo_solicitud

                  SELECT tipo_trab_ind,tipo_administracion
                  INTO   vtipo_trabajador,reg.tipo_inversion
                  FROM   cta_ctr_reg_ind
                  WHERE  nti = reg.nss_imss

                  IF STATUS = NOTFOUND THEN
                      LET reg.tipo_inversion = 1

                      LET vdesc_tipo_trab = "IMSS"
                  ELSE
                      SELECT desc_tipo_trab_ind
                      INTO   vdesc_tipo_trab
                      FROM   tab_tipo_trab_ind
                      WHERE  tipo_trab_ind = vtipo_trabajador
                  END IF

                  SELECT desc_tipo_admon
                  INTO   reg.desc_tipo_inv
                  FROM   tab_tipo_administracion
                  WHERE  tipo_administracion = reg.tipo_inversion

                  CASE reg.tipo_inversion
                      WHEN 1
                          LET reg.fecha_valoriza = HOY
                  END CASE

                  DISPLAY BY NAME reg.curp
                  DISPLAY BY NAME reg.rfc
                  DISPLAY BY NAME reg.paterno
                  DISPLAY BY NAME reg.materno
                  DISPLAY BY NAME reg.nombres
                  DISPLAY BY NAME reg.fecha_valoriza
                  DISPLAY BY NAME reg.tipo_inversion
                  DISPLAY reg.desc_tipo_inv TO desc_tipo_inversion
                  DISPLAY vtipo_solicitud   TO tipo_solicitud
                  DISPLAY vdesc_tipo_sol    TO desc_tipo_sol
                  DISPLAY vtipo_trabajador  TO tipo_trabajador
                  DISPLAY vdesc_tipo_trab   TO desc_tipo_trab
              END IF
          END IF

          IF reg.curp IS NULL AND reg.nss_imss IS NULL THEN
              ERROR "    CAMPO NO PUEDE SER NULO"
              ATTRIBUTE(NORMAL)
              NEXT FIELD curp
          ELSE
              SELECT n_seguro
              INTO   reg.nss_imss
              FROM   afi_mae_afiliado
              WHERE  n_unico = reg.curp

              SELECT n_unico ,
                     n_rfc   ,
                     paterno ,
                     materno ,
                     nombres ,
                     n_seguro
              INTO   reg.curp ,
                     reg.rfc  ,
                     reg.paterno ,
                     reg.materno ,
                     reg.nombres ,
                     reg.nss_imss
              FROM   afi_mae_afiliado
              WHERE  n_seguro = reg.nss_imss
              GROUP BY 1,2,3,4,5,6


              IF STATUS = NOTFOUND THEN
                  PROMPT "  CURP INEXISTENTE " FOR CHAR enter

                  INITIALIZE reg.curp TO NULL
                  CLEAR curp

                  NEXT FIELD curp
              ELSE
                  SELECT tipo_solicitud
                  INTO   vtipo_solicitud
                  FROM   afi_mae_afiliado
                  WHERE  n_seguro = reg.nss_imss

                  SELECT desc_solicitud
                  INTO   vdesc_tipo_sol
                  FROM   tab_tipo_solic
                  WHERE  tipo_solicitud = vtipo_solicitud

                  SELECT tipo_trab_ind,tipo_administracion
                  INTO   vtipo_trabajador,reg.tipo_inversion
                  FROM   cta_ctr_reg_ind
                  WHERE  nti = reg.nss_imss

                  IF STATUS = NOTFOUND THEN
                      LET reg.tipo_inversion = 1

                      LET vdesc_tipo_trab = "IMSS"
                  ELSE
                      SELECT desc_tipo_trab_ind
                      INTO   vdesc_tipo_trab
                      FROM   tab_tipo_trab_ind
                      WHERE  tipo_trab_ind = vtipo_trabajador
                  END IF

                  SELECT desc_tipo_admon
                  INTO   reg.desc_tipo_inv
                  FROM   tab_tipo_administracion
                  WHERE  tipo_administracion = reg.tipo_inversion

                  CASE reg.tipo_inversion
                      WHEN 1
                          LET reg.fecha_valoriza = HOY
                  END CASE

                  DISPLAY BY NAME reg.curp
                  DISPLAY BY NAME reg.rfc
                  DISPLAY BY NAME reg.paterno
                  DISPLAY BY NAME reg.materno
                  DISPLAY BY NAME reg.nombres
                  DISPLAY BY NAME reg.fecha_valoriza
                  DISPLAY BY NAME reg.tipo_inversion
                  DISPLAY reg.desc_tipo_inv TO desc_tipo_inversion
                  DISPLAY vtipo_solicitud   TO tipo_solicitud
                  DISPLAY vdesc_tipo_sol    TO desc_tipo_sol
                  DISPLAY vtipo_trabajador  TO tipo_trabajador
                  DISPLAY vdesc_tipo_trab   TO desc_tipo_trab
              END IF
          END IF

          IF reg.tipo_retiro IS NULL OR
             reg.tipo_retiro = 0     THEN
              CALL despliega_tipo_retiro()  #dtr
              DISPLAY reg.tipo_retiro TO tipo_retiro
              DISPLAY reg.des_tipo_retiro TO des_tipo_retiro
          ELSE
              IF  reg.tipo_retiro <> 4
              AND reg.tipo_retiro <> 5 THEN
                  ERROR ""
                  ERROR "   SOLO PUEDE ELEGIR OPCION 4 ó 5  "
                  ATTRIBUTE(NORMAL)
                  NEXT FIELD tipo_retiro
              END IF

              SELECT "OK"
              FROM   tab_retiro_issste
              WHERE  tipo_retiro = reg.tipo_retiro

              IF STATUS <> NOTFOUND THEN
                  SELECT desc_retiro
                  INTO   reg.des_tipo_retiro
                  FROM   tab_retiro_issste
                  WHERE  tipo_retiro = reg.tipo_retiro

                  DISPLAY reg.des_tipo_retiro TO des_tipo_retiro
              ELSE
                  ERROR " TIPO DE RETIRO ISSSTE INEXISTENTE ... "
                  NEXT FIELD tipo_retiro
              END IF
          END IF

          IF reg.tipo_beneficio IS NULL OR
             reg.tipo_beneficio = 0     THEN
              CALL despliega_tipo_beneficio()  #dtb
              DISPLAY reg.tipo_beneficio TO tipo_beneficio
              DISPLAY reg.desc_tipo_benef TO desc_tipo_benef
          ELSE
              SELECT "OK"
              FROM   tab_beneficio_issste
              WHERE  tipo_beneficio = reg.tipo_beneficio

              IF STATUS <> NOTFOUND THEN
                  SELECT desc_beneficio
                  INTO   reg.desc_tipo_benef
                  FROM   tab_beneficio_issste
                  WHERE  tipo_beneficio = reg.tipo_beneficio

                  DISPLAY reg.desc_tipo_benef TO desc_tipo_benef
              ELSE
                  ERROR " TIPO DE BENEFICIO ISSSTE INEXISTENTE ... "
                  NEXT FIELD tipo_beneficio
              END IF
          END IF

          IF reg.fecha_baja IS NULL THEN
              IF  reg.tipo_retiro = 5
              AND reg.tipo_beneficio = 777 THEN
                  ERROR "    CAMPO NO PUEDE SER NULO"
                  ATTRIBUTE(NORMAL)
                  NEXT FIELD fecha_baja
              END IF
          END IF

          IF reg.folio_dictamen IS NULL THEN
              IF  reg.tipo_retiro = 4
              AND reg.tipo_beneficio = 666 THEN
                  ERROR "    CAMPO NO PUEDE SER NULO"
                  ATTRIBUTE(NORMAL)
                  NEXT FIELD folio_dictamen
              END IF
          END IF

          IF reg.fecha_solicitud IS NULL THEN
              ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
              NEXT FIELD fecha_solicitud
          ELSE
              IF reg.fecha_solicitud > HOY THEN
                  ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                  ATTRIBUTE(NORMAL)
                  NEXT FIELD fecha_solicitud
              END IF
          END IF

          IF reg.tipo_retiro = 4 AND reg.fecha_entrega_rec IS NULL THEN
              ERROR "    CAMPO NO PUEDE SER NULO"
              ATTRIBUTE(NORMAL)
              NEXT FIELD fecha_entrega_rec
          END IF

          ----- MARCAJE -----

	  SELECT movimiento
	  INTO   s_tipo_movimiento
   	  FROM   tab_retiro_issste
	  WHERE  tipo_retiro = reg.tipo_retiro

          CALL marca_cuenta (reg.nss_imss      ,
                             s_tipo_movimiento ,
                             reg.consecutivo
                            )#mc
          RETURNING v_marca_res ,
                    v_rechazo_cod

          LET reg.rechazo_cod = v_rechazo_cod

          IF v_rechazo_cod > 0 THEN

              SELECT A.rechazo_desc
              INTO   desc_rechazo_cod
              FROM   tab_rch_marca A
              WHERE  A.rechazo_cod = v_rechazo_cod

              PROMPT " SOLICITUD RECHAZADA(",v_rechazo_cod,"  ",
	 	      desc_rechazo_cod CLIPPED,
	  	     ")<ENTER> CONTINUAR" FOR CHAR enter
          END IF

          WHENEVER ERROR CONTINUE
          INSERT INTO ret_sol_issste_par
          VALUES(reg.nss_imss         ,
                 reg.nss_issste       ,
                 reg.curp             ,
                 reg.consecutivo      ,
                 0                    ,#folio
                 reg.folio_solicitud  ,    
                 ""                   ,#tipo_id
                 reg.paterno          ,
                 reg.materno          ,
                 reg.nombres          ,
                 reg.rfc              ,
                 reg.tipo_retiro      ,  
                 reg.tipo_beneficio   , 
                 reg.fecha_baja       , 
                 reg.folio_dictamen   ,
                 reg.fecha_solicitud  ,  
                 reg.fecha_entrega_rec,
                 reg.tipo_inversion   ,
                 reg.fecha_valoriza   ,
                 0                    ,#acciones_ret_sief1
                 0                    ,#acciones_ret_sief2
                 0                    ,#importe_retiro
                 ""                   ,#diag_procesar
                 ""                   ,#diag_issste
                 ""                   ,#estado_sub_ret
                 reg.fecha_entrega_rec,#fecha_entrega_rec
                 reg_2.capturado      ,
                 reg.rechazo_cod      ,
                 reg.fecha_captura    ,
                 ""                   ,#fecha_confirma
                 ""                   ,#fecha_modifica
                 ""                   ,#fecha_envio
                 reg.usuario_captura  ,
                 ""                   ,#usuario_confirma 
                 ""                   ,#usuario_modifica
                 0                    ,#carta
                 ""                   ,#grupo
                 ""                   )#cve_destino

          IF SQLCA.SQLCODE < 0 THEN
	      LET x_error = "INSERT ret_sol_issste_par:",
                            "nss_imss ",reg.nss_imss,
 	                    "consecutivo ",reg.consecutivo,
			     err_get(SQLCA.SQLCODE)

              CALL errorlog(x_error CLIPPED)
              
	      PROMPT "   ERROR AL INGRESAR REGISTROS AVISE A SISTEMAS "
	      FOR enter
	      EXIT PROGRAM
          END IF

	  WHENEVER ERROR STOP

	  SELECT "OK"
	  FROM   ret_beneficiario
	  WHERE  nss = reg.nss_imss
          AND    consecutivo = reg.consecutivo
	  GROUP BY 1

	  IF STATUS = NOTFOUND THEN
              LET v_ejecuta = "fglgo RETM810 ",
			       reg.nss_imss CLIPPED," ",
			       reg.consecutivo CLIPPED," ",'A'

              RUN v_ejecuta
          ELSE
	      ERROR "    YA SE HAN CAPTURADO BENEFICIARIOS    "
    	      ATTRIBUTE(REVERSE)
	  END IF

          SELECT "OK"
          FROM   ret_beneficiario
          WHERE  nss = reg.nss_imss
          AND    consecutivo =  reg.consecutivo
          GROUP BY 1

          IF STATUS = NOTFOUND THEN
              ERROR "    NO SE PUEDE CAPTURAR LA SOLICITUD SIN BENEFICIARIOS"
              ATTRIBUTE(NORMAL)

              DELETE
              FROM  ret_sol_issste_par
              WHERE nss_imss = reg.nss_imss
              AND   consecutivo = reg.consecutivo

              NEXT FIELD nss_imss
          END IF

          DISPLAY "  REGISTRO INGRESADO  ","" AT 22,1
          ATTRIBUTE(REVERSE) SLEEP 3

          CALL inicializa() #i
          DISPLAY "                   ","" AT 20,1
          LET sw_1 = 0
          NEXT FIELD nss_imss

      ON KEY(CONTROL-C)
          DELETE
          FROM   ret_consecutivo
          WHERE  consecutivo = ult_consecutivo

          EXIT INPUT

      ON KEY(INTERRUPT)
         DELETE
         FROM   ret_consecutivo
         WHERE  consecutivo = ult_consecutivo

         EXIT INPUT
    END INPUT
    CLOSE WINDOW retm9002
END FUNCTION


FUNCTION consulta()
#c-----------------
    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER

    OPEN WINDOW retm9003 AT 2,3 WITH FORM "RETM9003" ATTRIBUTE (BORDER)
    DISPLAY " RETM900                  DATOS DEL TRABAJADOR                                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "CONSULTA" AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY " <Esc> : Consulta    Ctrl-B:Dom Trab/Benef    Ctrl-C : Salir " AT 2,1
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 10,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 ATTRIBUTE(REVERSE)

    INITIALIZE reg.* TO NULL
    CLEAR FORM

    LET int_flag              = FALSE

    CONSTRUCT BY NAME x_busca ON  A.nss_imss         ,
                                  A.rfc              ,
                                  A.curp             ,
                                  A.paterno          ,
                                  A.materno          ,
                                  A.nombres          ,
                                  A.nss_issste       ,
                                  A.fecha_captura    ,
                                  A.estado_solicitud ,
                                  A.folio            ,
                                  A.consecutivo

        ON KEY (CONTROL-C)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (Esc)

            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT


    IF int_flag = TRUE THEN
            LET int_flag = FALSE
        CLOSE WINDOW retm9003
        RETURN
    END IF

    LET txt_1 =" SELECT A.nss_imss         ,",
               "        A.rfc              ,",
               "        A.curp             ,",
               "        A.paterno          ,",
               "        A.materno          ,",
               "        A.nombres          ,",
               "        A.tipo_inversion   ,",
               "        ' '                ,",#desc_tipo_inversion
               "        ' '                ,",#tipo_solicitud
               "        ' '                ,",#desc_tipo_sol
               "        ' '                ,",#tipo_trabajador
               "        ' '                ,",#desc_tipo_trab
               "        A.nss_issste       ,",
               "        A.tipo_retiro      ,",
               "        ' '                ,",#desc_tipo_retiro
               "        A.tipo_beneficio   ,",
               "        ' '                ,",#desc_tipo_benef
               "        A.fecha_baja       ,",
               "        A.folio_dictamen   ,",
               "        A.fecha_valoriza   ,",
               "        A.folio_solicitud  ,",
               "        A.fecha_solicitud  ,",
               "        A.fecha_entrega_rec,",
               "        A.rechazo_cod      ,",
               "        ' '                ,",#rechazo_desc
               "        A.fecha_captura    ,",
               "        ' '                ,",#fecha_liquida
               "        A.usuario_captura  ,",
               "        A.folio            ,",
               "        A.estado_solicitud ,",
               "        ' '                ,",#desc_estado
               "        A.consecutivo       ",
               " FROM  ret_sol_issste_par A ",
               " WHERE ",x_busca CLIPPED

    PREPARE pre_1 FROM txt_1
    DECLARE cur_1 CURSOR FOR pre_1

    LET i = 1
    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_1 INTO arr_1[i].*
        SELECT tipo_solicitud
        INTO   arr_1[i].tipo_solicitud
        FROM   afi_mae_afiliado
        WHERE  n_seguro = arr_1[i].nss_imss

        SELECT desc_solicitud
        INTO   arr_1[i].desc_tipo_sol
        FROM   tab_tipo_solic
        WHERE  tipo_solicitud = arr_1[i].tipo_solicitud

        SELECT tipo_trab_ind,tipo_administracion
        INTO   arr_1[i].tipo_trabajador,arr_1[i].tipo_inversion
        FROM   cta_ctr_reg_ind
        WHERE  nti = arr_1[i].nss_imss

        IF STATUS = NOTFOUND THEN
            LET arr_1[i].tipo_inversion = 1

            LET arr_1[i].desc_tipo_trab = "IMSS"
        ELSE
            SELECT desc_tipo_trab_ind
            INTO   arr_1[i].desc_tipo_trab
            FROM   tab_tipo_trab_ind
            WHERE  tipo_trab_ind = arr_1[i].tipo_trabajador
        END IF

        SELECT desc_tipo_admon
        INTO   arr_1[i].desc_tipo_inv
        FROM   tab_tipo_administracion
        WHERE  tipo_administracion = arr_1[i].tipo_inversion

        SELECT desc_retiro
        INTO   arr_1[i].des_tipo_retiro
        FROM   tab_retiro_issste
        WHERE  tipo_retiro = arr_1[i].tipo_retiro

        SELECT desc_beneficio
        INTO   arr_1[i].desc_tipo_benef
        FROM   tab_beneficio_issste
        WHERE  tipo_beneficio = arr_1[i].tipo_beneficio

        SELECT descripcion
        INTO   arr_1[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   arr_1[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = arr_1[i].rechazo_cod
        END IF

        SELECT fecha_conversion
        INTO   arr_1[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = arr_1[i].nss_imss
        AND    consecutivo_lote = arr_1[i].consecutivo
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    ERROR ""
    LET cont_reg = i-1

    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT 22,51
    ATTRIBUTE(REVERSE)

    IF i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        DISPLAY "  NO EXISTE REGISTRO  ","" AT 22,1
        ATTRIBUTE(NORMAL) SLEEP 3
        CLOSE WINDOW retm8055

        RETURN
    END IF

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_1 TO scr_1.*
        ON KEY (CONTROL-B)
            LET arr_c = ARR_CURR()
            LET v_ejecuta = "fglgo RETM810 ",arr_1[arr_c].nss_imss CLIPPED," ",arr_1[arr_c].consecutivo CLIPPED," ",'A'
            RUN v_ejecuta
        ON KEY ( CONTROL-C )
            CALL inicializa()
            EXIT DISPLAY
        ON KEY ( INTERRUPT )
            CALL inicializa()
            EXIT DISPLAY
    END DISPLAY
CLOSE WINDOW retm9003
END FUNCTION


FUNCTION confirma()
#cc----------------
    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER,
        vmodif                INTEGER

    OPEN WINDOW retm9003 AT 2,3 WITH FORM "RETM9003" ATTRIBUTE (BORDER)
    DISPLAY " RETM900                  DATOS DEL TRABAJADOR                                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "CONFIRMA" AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY " <Esc> : Confirma    Ctrl-B:Dom Trab/Benef    Ctrl-C : Salir " AT 2,1
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 10,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 ATTRIBUTE(REVERSE)

    INITIALIZE reg.* TO NULL
    CLEAR FORM

    LET int_flag = FALSE
    LET vmodif = 2

    CONSTRUCT BY NAME x_busca ON  A.nss_imss         ,
                                  A.rfc              ,
                                  A.curp             ,
                                  A.paterno          ,
                                  A.materno          ,
                                  A.nombres          ,
                                  A.nss_issste       ,
                                  A.fecha_captura    ,
                                  A.estado_solicitud ,
                                  A.folio            ,
                                  A.consecutivo

        ON KEY (CONTROL-C)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (Esc)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE THEN
            LET int_flag = FALSE
        CLOSE WINDOW retm9003
        RETURN
    END IF

    LET txt_1 =" SELECT A.nss_imss         ,",
               "        A.rfc              ,",
               "        A.curp             ,",
               "        A.paterno          ,",
               "        A.materno          ,",
               "        A.nombres          ,",
               "        A.tipo_inversion   ,",
               "        ' '                ,",#desc_tipo_inversion
               "        ' '                ,",#tipo_solicitud
               "        ' '                ,",#desc_tipo_sol
               "        ' '                ,",#tipo_trabajador
               "        ' '                ,",#desc_tipo_trab
               "        A.nss_issste       ,",
               "        A.tipo_retiro      ,",
               "        ' '                ,",#desc_tipo_retiro
               "        A.tipo_beneficio   ,",
               "        ' '                ,",#desc_tipo_benef
               "        A.fecha_baja       ,",
               "        A.folio_dictamen   ,",
               "        A.fecha_valoriza   ,",
               "        A.folio_solicitud  ,",
               "        A.fecha_solicitud  ,",
               "        A.fecha_entrega_rec,",
               "        A.rechazo_cod      ,",
               "        ' '                ,",#rechazo_desc
               "        A.fecha_captura    ,",
               "        ' '                ,",#fecha_liquida
               "        A.usuario_captura  ,",
               "        A.folio            ,",
               "        A.estado_solicitud ,",
               "        ' '                ,",#desc_estado
               "        A.consecutivo       ",
               " FROM  ret_sol_issste_par A ",
               " WHERE ",x_busca CLIPPED,
               " AND   A.estado_solicitud = 0 ",
               " AND   A.rechazo_cod = 0 "

    PREPARE pre_2 FROM txt_1
    DECLARE cur_2 CURSOR FOR pre_2

    LET i = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_2 INTO arr_1[i].*

        SELECT usuario_captura
        INTO   x_usuario 
        FROM   ret_sol_issste_par
        WHERE  nss_imss = arr_1[i].nss_imss
        AND    consecutivo = arr_1[i].consecutivo

      {IF v_codafore <> 564 THEN
        IF x_usuario = usuario THEN
            PROMPT "USUARIO ES EL MISMO DE CAPTURA" ATTRIBUTE (REVERSE)
            FOR opc ATTRIBUTE (REVERSE)

            LET sw = 1      
            EXIT FOREACH
        END IF                    -- SOLICITADO POR MLM
      END IF}
        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].estado_solicitud > 0 THEN

	    PROMPT "SOLICITUD DEL NSS <",arr_1[i].nss_imss,
	     			      "> SE ENCUENTRA EN EL ESTADO : ",
				      x_estado_solicitud CLIPPED,
				      " " ATTRIBUTE(REVERSE) 
	       FOR opc ATTRIBUTE(REVERSE)
  	    CONTINUE FOREACH
        END IF

        SELECT tipo_solicitud
        INTO   arr_1[i].tipo_solicitud
        FROM   afi_mae_afiliado
        WHERE  n_seguro = arr_1[i].nss_imss

        SELECT desc_solicitud
        INTO   arr_1[i].desc_tipo_sol
        FROM   tab_tipo_solic
        WHERE  tipo_solicitud = arr_1[i].tipo_solicitud

        SELECT tipo_trab_ind,tipo_administracion
        INTO   arr_1[i].tipo_trabajador,arr_1[i].tipo_inversion
        FROM   cta_ctr_reg_ind
        WHERE  nti = arr_1[i].nss_imss

        IF STATUS = NOTFOUND THEN
            LET arr_1[i].tipo_inversion = 1

            LET arr_1[i].desc_tipo_trab = "IMSS"
        ELSE
            SELECT desc_tipo_trab_ind
            INTO   arr_1[i].desc_tipo_trab
            FROM   tab_tipo_trab_ind
            WHERE  tipo_trab_ind = arr_1[i].tipo_trabajador
        END IF

        SELECT desc_tipo_admon
        INTO   arr_1[i].desc_tipo_inv
        FROM   tab_tipo_administracion
        WHERE  tipo_administracion = arr_1[i].tipo_inversion

        SELECT desc_retiro
        INTO   arr_1[i].des_tipo_retiro
        FROM   tab_retiro_issste
        WHERE  tipo_retiro = arr_1[i].tipo_retiro

        SELECT desc_beneficio
        INTO   arr_1[i].desc_tipo_benef
        FROM   tab_beneficio_issste
        WHERE  tipo_beneficio = arr_1[i].tipo_beneficio

        SELECT descripcion
        INTO   arr_1[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   arr_1[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = arr_1[i].rechazo_cod
        END IF

        SELECT fecha_conversion
        INTO   arr_1[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = arr_1[i].nss_imss
        AND    consecutivo_lote = arr_1[i].consecutivo
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    LET cont_reg = i-1

    ERROR ""
    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT 22,51
    ATTRIBUTE(REVERSE)

    IF i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm9003
        RETURN
    END IF

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_1 TO scr_1.*
        ON KEY ( CONTROL-C )
            CALL inicializa()
            LET int_flag = TRUE
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            CALL inicializa()
            LET int_flag = TRUE
            EXIT DISPLAY

        ON KEY (CONTROL-B)
            LET arr_c = arr_curr()
            LET v_ejecuta = "fglgo RETM810 ",arr_1[arr_c].nss_imss CLIPPED," ",
                            arr_1[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
            RUN v_ejecuta

        ON KEY (CONTROL-M)
            LET pos = ARR_CURR()

            { IF arr_1[pos].usuario_captura = usuario THEN
               ERROR "    NO SE PUEDE CONFIRMAR CON EL MISMO USUARIO DE CAPTURA"
               ATTRIBUTE(NORMAL)
               CALL inicializa()
               LET int_flag = TRUE
            END IF }

            EXIT DISPLAY
    END DISPLAY

    IF int_flag = TRUE THEN
	LET int_flag = FALSE
        CLOSE WINDOW retm9003
        RETURN
    ELSE
        CALL construccion(arr_1[pos].*,vmodif)
        CLOSE WINDOW retm9003
    END IF
END FUNCTION


FUNCTION construccion(reg_mod,vmodif2)
#c------------------------------------
    DEFINE reg_mod RECORD
        nss_imss           LIKE ret_sol_issste_par.nss_imss             ,
        rfc                LIKE ret_sol_issste_par.rfc                  ,
        curp               LIKE ret_sol_issste_par.curp                 ,
        paterno            LIKE ret_sol_issste_par.paterno              ,
        materno            LIKE ret_sol_issste_par.materno              ,
        nombres            LIKE ret_sol_issste_par.nombres              ,
        tipo_inversion     LIKE ret_sol_issste_par.tipo_inversion       ,
        desc_tipo_inv      LIKE tab_tipo_administracion.desc_tipo_admon ,
        tipo_solicitud     LIKE afi_mae_afiliado.tipo_solicitud         ,
        desc_tipo_sol      LIKE tab_tipo_solic.desc_solicitud           ,
        tipo_trabajador    LIKE tab_tipo_trab_ind.tipo_trab_ind         ,
        desc_tipo_trab     LIKE tab_tipo_trab_ind.desc_tipo_trab_ind    ,
        nss_issste         LIKE ret_sol_issste_par.nss_issste           ,
        tipo_retiro        LIKE ret_sol_issste_par.tipo_retiro          ,
        des_tipo_retiro    LIKE tab_retiro_issste.desc_retiro           ,
        tipo_beneficio     LIKE ret_sol_issste_par.tipo_beneficio       ,
        desc_tipo_benef    LIKE tab_beneficio_issste.desc_beneficio     ,
        fecha_baja         LIKE ret_sol_issste_par.fecha_baja           ,
        folio_dictamen     LIKE ret_sol_issste_par.folio_dictamen       ,
        fecha_valoriza     LIKE ret_sol_issste_par.fecha_valoriza       ,
        folio_solicitud    LIKE ret_sol_issste_par.folio_solicitud      ,
        fecha_solicitud    LIKE ret_sol_issste_par.fecha_solicitud      ,
        fecha_entrega_rec  LIKE ret_sol_issste_par.fecha_entrega_rec    ,
        rechazo_cod        LIKE ret_sol_issste_par.rechazo_cod          ,
        rechazo_desc       LIKE tab_rch_marca.rechazo_desc              ,
        fecha_captura      LIKE ret_sol_issste_par.fecha_captura        ,
        fecha_liquida      DATE                                         ,
        usuario_captura    LIKE ret_sol_issste_par.usuario_captura      ,
        folio              LIKE ret_sol_issste_par.folio                ,
        estado_solicitud   LIKE ret_sol_issste_par.estado_solicitud     ,
        desc_estado        LIKE ret_estado.descripcion                  ,
        consecutivo        LIKE ret_sol_issste_par.consecutivo
    END RECORD

    DEFINE #loc #smallint
        i                     SMALLINT

    DEFINE #loc #char
        opc                   CHAR(01)

    DEFINE #loc #integer
        vmodif2               INTEGER

    LET reg.nss_imss          = reg_mod.nss_imss          
    LET reg.rfc               = reg_mod.rfc
    LET reg.curp              = reg_mod.curp
    LET reg.paterno           = reg_mod.paterno
    LET reg.materno           = reg_mod.materno
    LET reg.nombres           = reg_mod.nombres
    LET reg.tipo_inversion    = reg_mod.tipo_inversion
    LET reg.desc_tipo_inv     = reg_mod.desc_tipo_inv
    LET reg.tipo_solicitud    = reg_mod.tipo_solicitud
    LET reg.desc_tipo_sol     = reg_mod.desc_tipo_sol
    LET reg.tipo_trabajador   = reg_mod.tipo_trabajador
    LET reg.desc_tipo_trab    = reg_mod.desc_tipo_trab
    LET reg.nss_issste        = reg_mod.nss_issste
    LET reg.tipo_retiro       = reg_mod.tipo_retiro
    LET reg.des_tipo_retiro   = reg_mod.des_tipo_retiro
    LET reg.tipo_beneficio    = reg_mod.tipo_beneficio
    LET reg.desc_tipo_benef   = reg_mod.desc_tipo_benef
    LET reg.fecha_baja        = reg_mod.fecha_baja
    LET reg.folio_dictamen    = reg_mod.folio_dictamen
    LET reg.fecha_valoriza    = reg_mod.fecha_valoriza
    LET reg.folio_solicitud   = reg_mod.folio_solicitud
    LET reg.fecha_solicitud   = reg_mod.fecha_solicitud
    LET reg.fecha_entrega_rec = reg_mod.fecha_entrega_rec
    LET reg.rechazo_cod       = reg_mod.rechazo_cod
    LET reg.rechazo_desc      = reg_mod.rechazo_desc
    LET reg.fecha_captura     = reg_mod.fecha_captura
    LET reg.fecha_liquida     = reg_mod.fecha_liquida
    LET reg.usuario_captura   = reg_mod.usuario_captura
    LET reg.folio             = reg_mod.folio
    LET reg.estado_solicitud  = reg_mod.estado_solicitud
    LET reg.desc_estado       = reg_mod.desc_estado
    LET reg.consecutivo       = reg_mod.consecutivo

    INPUT BY NAME reg.nss_issste         ,
                  reg.tipo_beneficio     ,
                  reg.fecha_baja         ,
                  reg.folio_dictamen     ,
                  reg.folio_solicitud    ,
                  reg.fecha_solicitud    ,
                  reg.fecha_entrega_rec  WITHOUT DEFAULTS

        BEFORE FIELD nss_issste
            IF vmodif2 = 1 THEN
                LET vfecha_modifica = HOY
                LET vusuario_modif  = usuario
            END IF

            IF vmodif2 = 2 THEN
                LET vfecha_confirma  = HOY
                LET vusuario_confirma = usuario
            END IF

        
        AFTER FIELD nss_issste 
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF

        AFTER FIELD tipo_beneficio 
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF

            IF reg.tipo_beneficio IS NULL OR 
               reg.tipo_beneficio = 0     THEN
                CALL despliega_tipo_beneficio()  #dtb
                DISPLAY reg.tipo_beneficio TO tipo_beneficio
                DISPLAY reg.desc_tipo_benef TO desc_tipo_benef
            ELSE
                SELECT "OK"
                FROM   tab_beneficio_issste
                WHERE  tipo_beneficio = reg.tipo_beneficio
            
                IF STATUS <> NOTFOUND THEN
                    SELECT desc_beneficio 
                    INTO   reg.desc_tipo_benef
                    FROM   tab_beneficio_issste
                    WHERE  tipo_beneficio = reg.tipo_beneficio
                 
                    DISPLAY reg.desc_tipo_benef TO desc_tipo_benef
                ELSE
                    ERROR " TIPO DE BENEFICIO ISSSTE INEXISTENTE ... "
                    NEXT FIELD tipo_beneficio 
                END IF  
            END IF  

        AFTER FIELD fecha_baja
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF

            IF reg.fecha_baja IS NULL THEN
                IF  reg.tipo_retiro = 5 
                AND reg.tipo_beneficio = 777 THEN
                    ERROR "    CAMPO NO PUEDE SER NULO"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_baja
                END IF
            END IF
                   
        AFTER FIELD folio_dictamen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  PREVIOUS
            END IF

            IF reg.folio_dictamen IS NULL THEN
                IF  reg.tipo_retiro = 4 
                AND reg.tipo_beneficio = 666 THEN
                    ERROR "    CAMPO NO PUEDE SER NULO"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_dictamen
                END IF
            END IF

        AFTER FIELD folio_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

        AFTER FIELD fecha_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF reg.fecha_solicitud IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_solicitud
            ELSE
                IF reg.fecha_solicitud > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_solicitud
                END IF
            END IF

        AFTER FIELD fecha_entrega_rec
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF reg.tipo_retiro = 4 AND reg.fecha_entrega_rec IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_entrega_rec
            END IF

      ON KEY (CONTROL-B)
          LET v_ejecuta = "fglgo RETM810 ",reg.nss_imss CLIPPED," ",
                          reg.consecutivo CLIPPED," ",'C' CLIPPED
          RUN v_ejecuta

      ON KEY ( CONTROL-C )
          EXIT INPUT

      ON KEY ( INTERRUPT )
          EXIT INPUT

      ON KEY (ESC)
          IF reg.nss_imss IS NOT NULL THEN

              SELECT n_unico ,
                     n_rfc   ,
                     paterno ,
                     materno ,
                     nombres
              INTO   reg.curp ,
                     reg.rfc  ,
                     reg.paterno ,
                     reg.materno ,
                     reg.nombres
              FROM   afi_mae_afiliado
              WHERE  n_seguro = reg.nss_imss
              GROUP BY 1,2,3,4,5

              IF STATUS = NOTFOUND THEN
                  PROMPT "  TRABAJADOR NO AFILIADO A LA AFORE " FOR CHAR enter

                  INITIALIZE reg.nss_imss TO NULL
                  CLEAR nss_imss

                  NEXT FIELD curp
              ELSE
                  SELECT tipo_solicitud
                  INTO   vtipo_solicitud
                  FROM   afi_mae_afiliado
                  WHERE  n_seguro = reg.nss_imss

                  SELECT desc_solicitud
                  INTO   vdesc_tipo_sol
                  FROM   tab_tipo_solic
                  WHERE  tipo_solicitud = vtipo_solicitud

                  SELECT tipo_trab_ind,tipo_administracion,nss_issste
                  INTO   vtipo_trabajador,reg.tipo_inversion,reg.nss_issste
                  FROM   cta_ctr_reg_ind
                  WHERE  nti = reg.nss_imss

                  IF STATUS = NOTFOUND THEN
                      LET reg.tipo_inversion = 1

                      LET vdesc_tipo_trab = "IMSS"
                  ELSE
                      SELECT desc_tipo_trab_ind
                      INTO   vdesc_tipo_trab
                      FROM   tab_tipo_trab_ind
                      WHERE  tipo_trab_ind = vtipo_trabajador
                  END IF

                  SELECT desc_tipo_admon
                  INTO   reg.desc_tipo_inv
                  FROM   tab_tipo_administracion
                  WHERE  tipo_administracion = reg.tipo_inversion

                  DISPLAY BY NAME reg.curp
                  DISPLAY BY NAME reg.nss_issste
                  DISPLAY BY NAME reg.rfc
                  DISPLAY BY NAME reg.paterno
                  DISPLAY BY NAME reg.materno
                  DISPLAY BY NAME reg.nombres
                  DISPLAY BY NAME reg.tipo_inversion
                  DISPLAY reg.desc_tipo_inv TO desc_tipo_inversion
                  DISPLAY reg.nss_issste    TO nss_issste           
                  DISPLAY vtipo_solicitud   TO tipo_solicitud
                  DISPLAY vdesc_tipo_sol    TO desc_tipo_sol
                  DISPLAY vtipo_trabajador  TO tipo_trabajador
                  DISPLAY vdesc_tipo_trab   TO desc_tipo_trab
              END IF
          END IF

          IF reg.curp IS NULL AND reg.nss_imss IS NULL THEN
              ERROR "    CAMPO NO PUEDE SER NULO"
              ATTRIBUTE(NORMAL)
              NEXT FIELD curp
          ELSE
              SELECT n_seguro
              INTO   reg.nss_imss
              FROM   afi_mae_afiliado
              WHERE  n_unico = reg.curp

              SELECT n_unico ,
                     n_rfc   ,
                     paterno ,
                     materno ,
                     nombres ,
                     n_seguro
              INTO   reg.curp ,
                     reg.rfc  ,
                     reg.paterno ,
                     reg.materno ,
                     reg.nombres ,
                     reg.nss_imss
              FROM   afi_mae_afiliado
              WHERE  n_seguro = reg.nss_imss
              GROUP BY 1,2,3,4,5,6


              IF STATUS = NOTFOUND THEN
                  PROMPT "  CURP INEXISTENTE " FOR CHAR enter

                  INITIALIZE reg.curp TO NULL
                  CLEAR curp

                  NEXT FIELD curp
              ELSE
                  SELECT tipo_solicitud
                  INTO   vtipo_solicitud
                  FROM   afi_mae_afiliado
                  WHERE  n_seguro = reg.nss_imss

                  SELECT desc_solicitud
                  INTO   vdesc_tipo_sol
                  FROM   tab_tipo_solic
                  WHERE  tipo_solicitud = vtipo_solicitud

                  SELECT tipo_trab_ind,tipo_administracion,nss_issste
                  INTO   vtipo_trabajador,reg.tipo_inversion,reg.nss_issste
                  FROM   cta_ctr_reg_ind
                  WHERE  nti = reg.nss_imss

                  IF STATUS = NOTFOUND THEN
                      LET reg.tipo_inversion = 1

                      LET vdesc_tipo_trab = "IMSS"
                  ELSE
                      SELECT desc_tipo_trab_ind
                      INTO   vdesc_tipo_trab
                      FROM   tab_tipo_trab_ind
                      WHERE  tipo_trab_ind = vtipo_trabajador
                  END IF

                  SELECT desc_tipo_admon
                  INTO   reg.desc_tipo_inv
                  FROM   tab_tipo_administracion
                  WHERE  tipo_administracion = reg.tipo_inversion

                  DISPLAY BY NAME reg.curp
                  DISPLAY BY NAME reg.nss_issste
                  DISPLAY BY NAME reg.rfc
                  DISPLAY BY NAME reg.paterno
                  DISPLAY BY NAME reg.materno
                  DISPLAY BY NAME reg.nombres
                  DISPLAY BY NAME reg.tipo_inversion
                  DISPLAY reg.desc_tipo_inv TO desc_tipo_inversion
                  DISPLAY reg.nss_issste    TO nss_issste           
                  DISPLAY vtipo_solicitud   TO tipo_solicitud
                  DISPLAY vdesc_tipo_sol    TO desc_tipo_sol
                  DISPLAY vtipo_trabajador  TO tipo_trabajador
                  DISPLAY vdesc_tipo_trab   TO desc_tipo_trab
              END IF
          END IF

          IF reg.tipo_beneficio IS NULL OR
             reg.tipo_beneficio = 0     THEN
              CALL despliega_tipo_beneficio()  #dtb
              DISPLAY reg.tipo_beneficio TO tipo_beneficio
              DISPLAY reg.desc_tipo_benef TO desc_tipo_benef
          ELSE
              SELECT "OK"
              FROM   tab_beneficio_issste
              WHERE  tipo_beneficio = reg.tipo_beneficio

              IF STATUS <> NOTFOUND THEN
                  SELECT desc_beneficio
                  INTO   reg.desc_tipo_benef
                  FROM   tab_beneficio_issste
                  WHERE  tipo_beneficio = reg.tipo_beneficio

                  DISPLAY reg.desc_tipo_benef TO desc_tipo_benef
              ELSE
                  ERROR " TIPO DE BENEFICIO ISSSTE INEXISTENTE ... "
                  NEXT FIELD tipo_beneficio
              END IF
          END IF

          IF reg.fecha_baja IS NULL THEN
              IF  reg.tipo_retiro = 5
              AND reg.tipo_beneficio = 777 THEN
                  ERROR "    CAMPO NO PUEDE SER NULO"
                  ATTRIBUTE(NORMAL)
                  NEXT FIELD fecha_baja
              END IF
          END IF

          IF reg.folio_dictamen IS NULL THEN
              IF  reg.tipo_retiro = 4
              AND reg.tipo_beneficio = 666 THEN
                  ERROR "    CAMPO NO PUEDE SER NULO"
                  ATTRIBUTE(NORMAL)
                  NEXT FIELD folio_dictamen
              END IF
          END IF

          IF reg.fecha_solicitud IS NULL THEN
              ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
              NEXT FIELD fecha_solicitud
          ELSE
              IF reg.fecha_solicitud > HOY THEN
                  ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                  ATTRIBUTE(NORMAL)
                  NEXT FIELD fecha_solicitud
              END IF
          END IF

          IF reg.tipo_retiro = 4 AND reg.fecha_entrega_rec IS NULL THEN
              ERROR "    CAMPO NO PUEDE SER NULO"
              ATTRIBUTE(NORMAL)
              NEXT FIELD fecha_entrega_rec
          END IF

          EXIT INPUT
    END INPUT

    IF vmodif2 = 1  THEN
        CALL pre_gunta() RETURNING opc
        IF opc MATCHES "[Nn]" THEN
            DISPLAY "  MODIFICACION CANCELADA  "
                        AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
            CALL inicializa() #i
	    RETURN
        END IF
    ELSE
        IF vmodif2 = 2 THEN
            CALL pre_gunta2() RETURNING opc
            IF opc MATCHES "[Nn]" THEN
                DISPLAY "  CONFIRMACION CANCELADA  "
                        AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
                CALL inicializa() #i
	        RETURN
            END IF
        END IF
    END IF

    IF vmodif2 = 1 THEN
        WHENEVER ERROR CONTINUE
        UPDATE ret_sol_issste_par
        SET    ret_sol_issste_par.nss_issste        = reg.nss_issste       ,
               ret_sol_issste_par.tipo_beneficio    = reg.tipo_beneficio   ,
               ret_sol_issste_par.fecha_baja        = reg.fecha_baja       ,
               ret_sol_issste_par.folio_dictamen    = reg.folio_dictamen   ,
               ret_sol_issste_par.folio_solicitud   = reg.folio_solicitud  ,
               ret_sol_issste_par.fecha_solicitud   = reg.fecha_solicitud  ,
               ret_sol_issste_par.fecha_entrega_rec = reg.fecha_entrega_rec,
               ret_sol_issste_par.usuario_modifica  = vusuario_modif       ,
               ret_sol_issste_par.fecha_modifica    = vfecha_modifica
        WHERE  ret_sol_issste_par.nss_imss = reg.nss_imss
        AND    ret_sol_issste_par.consecutivo = reg.consecutivo

        IF SQLCA.SQLCODE < 0 THEN
	    LET x_error = "UPDATE ret_sol_issste_par:",
	  		  "nss_imss ",reg.nss_imss,
			  "consecutivo ",reg.consecutivo,
		  	  err_get(SQLCA.SQLCODE)

                   CALL errorlog(x_error CLIPPED)

		   PROMPT " ERROR DE UPDATE ret_sol_issste_par AVISE A SISTEMAS "
		   FOR enter
		   EXIT PROGRAM
        END IF

        WHENEVER ERROR STOP

        DISPLAY "  REGISTRO MODIFICADO  ","" AT 22,1
        ATTRIBUTE(REVERSE)

        SLEEP 3
        CALL inicializa() #i
        INITIALIZE reg_mod.* TO NULL
        RETURN
    ELSE
        IF vmodif2 = 2 THEN
	    WHENEVER ERROR CONTINUE
            UPDATE ret_sol_issste_par
            SET    ret_sol_issste_par.nss_issste        = reg.nss_issste       ,
                   ret_sol_issste_par.tipo_beneficio    = reg.tipo_beneficio   ,
                   ret_sol_issste_par.fecha_baja        = reg.fecha_baja       ,
                   ret_sol_issste_par.folio_dictamen    = reg.folio_dictamen   ,
                   ret_sol_issste_par.folio_solicitud   = reg.folio_solicitud  ,
                   ret_sol_issste_par.fecha_solicitud   = reg.fecha_solicitud  ,
                   ret_sol_issste_par.fecha_entrega_rec = reg.fecha_entrega_rec,
                   ret_sol_issste_par.usuario_confirma  = vusuario_confirma    ,
                   ret_sol_issste_par.fecha_confirma    = vfecha_confirma      ,
                   ret_sol_issste_par.estado_solicitud  = reg_2.confirmado
            WHERE  ret_sol_issste_par.nss_imss = reg.nss_imss
            AND    ret_sol_issste_par.consecutivo = reg.consecutivo

	    IF SQLCA.SQLCODE < 0 THEN
	        LET x_error = "UPDATE ret_sol_issste_par:",
			      "nss_imss ",reg.nss_imss,
			      "consecutivo ",reg.consecutivo,
			      err_get(SQLCA.SQLCODE)

                CALL errorlog(x_error CLIPPED)

		PROMPT " ERROR DE UPDATE ret_sol_issste_par AVISE A SISTEMAS "
		FOR enter
		EXIT PROGRAM
            END IF

            WHENEVER ERROR STOP
             DISPLAY "  REGISTRO CONFIRMADO  ","" AT 22,1
                        ATTRIBUTE(REVERSE)
             SLEEP 3
             CALL inicializa() #i
             INITIALIZE reg_mod.* TO NULL
             RETURN
          END IF
       END IF

END FUNCTION


FUNCTION pre_gunta()
#------------------
     DEFINE opc CHAR(01)

     WHILE TRUE
         PROMPT "  DESEAS ACTUALIZARLA   S/N ? " FOR opc
         IF opc NOT MATCHES "[SsNn]" THEN
            CONTINUE WHILE
         ELSE
             EXIT WHILE
         END IF
     END WHILE

     RETURN opc
END FUNCTION

FUNCTION pre_gunta2()
#-------------------
     DEFINE opc CHAR(01)

     WHILE TRUE
         PROMPT "  DESEAS CONFIRMAR LA CAPTURA  S/N ? " FOR opc
         IF opc NOT MATCHES "[SsNn]" THEN
            CONTINUE WHILE
         ELSE
            EXIT WHILE
         END IF
     END WHILE
     RETURN opc
END FUNCTION


FUNCTION modifica()
#m-----------------
    DEFINE #loc #smallint
        i                     ,
        sw_2                  ,
        vmodif             INTEGER

    OPEN WINDOW retm9003 AT 2,3 WITH FORM "RETM9003" ATTRIBUTE (BORDER)
    DISPLAY " RETM900                  DATOS DEL TRABAJADOR                                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "MODIFICA" AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY " <Esc> : Modifica    Ctrl-B:Dom Trab/Benef    Ctrl-C : Salir " AT 2,1
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 10,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 ATTRIBUTE(REVERSE)

    LET int_flag   = FALSE
    LET sw_2   = 1
    LET vmodif = 1

    CONSTRUCT BY NAME x_busca ON  A.nss_imss         ,
                                  A.rfc              ,
                                  A.curp             ,
                                  A.paterno          ,
                                  A.materno          ,
                                  A.nombres          ,
                                  A.nss_issste       ,
                                  A.fecha_captura    ,
                                  A.estado_solicitud ,
                                  A.folio            ,
                                  A.consecutivo

        ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT)
           LET int_flag = TRUE
           EXIT CONSTRUCT

        ON KEY (Esc)
           LET int_flag = FALSE
           EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        CLOSE WINDOW retm9003
        RETURN
    END IF

    LET txt_1 =" SELECT A.nss_imss         ,",
               "        A.rfc              ,",
               "        A.curp             ,",
               "        A.paterno          ,",
               "        A.materno          ,",
               "        A.nombres          ,",
               "        A.tipo_inversion   ,",
               "        ' '                ,",#desc_tipo_inversion
               "        ' '                ,",#tipo_solicitud
               "        ' '                ,",#desc_tipo_sol
               "        ' '                ,",#tipo_trabajador
               "        ' '                ,",#desc_tipo_trab
               "        A.nss_issste       ,",
               "        A.tipo_retiro      ,",
               "        ' '                ,",#desc_tipo_retiro
               "        A.tipo_beneficio   ,",
               "        ' '                ,",#desc_tipo_benef
               "        A.fecha_baja       ,",
               "        A.folio_dictamen   ,",
               "        A.fecha_valoriza   ,",
               "        A.folio_solicitud  ,",
               "        A.fecha_solicitud  ,",
               "        A.fecha_entrega_rec,",
               "        A.rechazo_cod      ,",
               "        ' '                ,",#rechazo_desc
               "        A.fecha_captura    ,",
               "        ' '                ,",#fecha_liquida
               "        A.usuario_captura  ,",
               "        A.folio            ,",
               "        A.estado_solicitud ,",
               "        ' '                ,",#desc_estado
               "        A.consecutivo       ",
               " FROM  ret_sol_issste_par A ",
               " WHERE ",x_busca CLIPPED

    PREPARE pre_3 FROM txt_1
    DECLARE cur_3 CURSOR FOR pre_3

    LET i = 1
    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_3 INTO arr_1[i].*

        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].estado_solicitud > 0 THEN

	    PROMPT "SOLICITUD DEL NSS <",arr_1[i].nss_imss,
	   			      "> SE ENCUENTRA EN EL ESTADO : ",
				      x_estado_solicitud CLIPPED,
				      " " ATTRIBUTE(REVERSE) 
	       FOR opc ATTRIBUTE(REVERSE)
	    CONTINUE FOREACH
        END IF

        SELECT tipo_solicitud
        INTO   arr_1[i].tipo_solicitud
        FROM   afi_mae_afiliado
        WHERE  n_seguro = arr_1[i].nss_imss

        SELECT desc_solicitud
        INTO   arr_1[i].desc_tipo_sol
        FROM   tab_tipo_solic
        WHERE  tipo_solicitud = arr_1[i].tipo_solicitud

        SELECT tipo_trab_ind,tipo_administracion
        INTO   arr_1[i].tipo_trabajador,arr_1[i].tipo_inversion
        FROM   cta_ctr_reg_ind
        WHERE  nti = arr_1[i].nss_imss

        IF STATUS = NOTFOUND THEN
            LET arr_1[i].tipo_inversion = 1

            LET arr_1[i].desc_tipo_trab = "IMSS"
        ELSE
            SELECT desc_tipo_trab_ind
            INTO   arr_1[i].desc_tipo_trab
            FROM   tab_tipo_trab_ind
            WHERE  tipo_trab_ind = arr_1[i].tipo_trabajador
        END IF

        SELECT desc_tipo_admon
        INTO   arr_1[i].desc_tipo_inv
        FROM   tab_tipo_administracion
        WHERE  tipo_administracion = arr_1[i].tipo_inversion

        SELECT desc_retiro
        INTO   arr_1[i].des_tipo_retiro
        FROM   tab_retiro_issste
        WHERE  tipo_retiro = arr_1[i].tipo_retiro

        SELECT desc_beneficio
        INTO   arr_1[i].desc_tipo_benef
        FROM   tab_beneficio_issste
        WHERE  tipo_beneficio = arr_1[i].tipo_beneficio

        SELECT descripcion
        INTO   arr_1[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   arr_1[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = arr_1[i].rechazo_cod
        END IF

        SELECT fecha_conversion
        INTO   arr_1[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = arr_1[i].nss_imss
        AND    consecutivo_lote = arr_1[i].consecutivo
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    ERROR ""
    LET cont_reg = i-1

    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT 22,51
    ATTRIBUTE(REVERSE)

    IF (i-1) >= 1 THEN
        CALL SET_COUNT(i-1)

        DISPLAY ARRAY arr_1 TO scr_1.*
           ON KEY ( CONTROL-C )
               LET int_flag=	TRUE
               EXIT DISPLAY
 
           ON KEY ( INTERRUPT )
               LET int_flag=	TRUE
               EXIT DISPLAY
 
           ON KEY (CONTROL-B)
               LET arr_c =  arr_curr()
               LET v_ejecuta = "fglgo RETM810 ",arr_1[arr_c].nss_imss CLIPPED," ",
                               arr_1[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
               RUN v_ejecuta

           ON KEY ( CONTROL-M )
               LET pos = ARR_CURR()
               EXIT DISPLAY
        END DISPLAY
    ELSE
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm9003
       RETURN
    END IF

    IF int_flag = TRUE THEN
        LET int_flag=FALSE
        CLOSE WINDOW retm9003
        RETURN
    END IF

    CALL construccion(arr_1[pos].*,vmodif)
CLOSE WINDOW retm9003
END FUNCTION


FUNCTION elimina()
#e----------------
    DEFINE #loc #smallint
        arr                   ,
        src                   ,
        i                     ,
        v_status              SMALLINT

    DEFINE #loc #integer
        arr_c                 INTEGER

    OPEN WINDOW retm9003 AT 2,3 WITH FORM "RETM9003" ATTRIBUTE (BORDER)
    DISPLAY " RETM900                  DATOS DEL TRABAJADOR                                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "ELIMINA" AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY " <ENTER> : Elimina    Ctrl-C : Salir " AT 2,1
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 10,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 18,1 ATTRIBUTE(REVERSE)

    INITIALIZE reg.* TO NULL
    CLEAR FORM

    LET int_flag = FALSE

        CONSTRUCT BY NAME x_busca ON  A.nss_imss         ,
                                      A.rfc              ,
                                      A.curp             ,
                                      A.paterno          ,
                                      A.materno          ,
                                      A.nombres          ,
                                      A.nss_issste       ,
                                      A.fecha_captura    ,
                                      A.estado_solicitud ,
                                      A.folio            ,
                                      A.consecutivo

        ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (Esc)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE THEN
            LET int_flag = FALSE
        CLOSE WINDOW retm9003
        RETURN
    END IF

    LET txt_1 =" SELECT A.nss_imss         ,",
               "        A.rfc              ,",
               "        A.curp             ,",
               "        A.paterno          ,",
               "        A.materno          ,",
               "        A.nombres          ,",
               "        A.tipo_inversion   ,",
               "        ' '                ,",#desc_tipo_inversion
               "        ' '                ,",#tipo_solicitud
               "        ' '                ,",#desc_tipo_sol
               "        ' '                ,",#tipo_trabajador
               "        ' '                ,",#desc_tipo_trab
               "        A.nss_issste       ,",
               "        A.tipo_retiro      ,",
               "        ' '                ,",#desc_tipo_retiro
               "        A.tipo_beneficio   ,",
               "        ' '                ,",#desc_tipo_benef
               "        A.fecha_baja       ,",
               "        A.folio_dictamen   ,",
               "        A.fecha_valoriza   ,",
               "        A.folio_solicitud  ,",
               "        A.fecha_solicitud  ,",
               "        A.fecha_entrega_rec,",
               "        A.rechazo_cod      ,",
               "        ' '                ,",#rechazo_desc
               "        A.fecha_captura    ,",
               "        ' '                ,",#fecha_liquida
               "        A.usuario_captura  ,",
               "        A.folio            ,",
               "        A.estado_solicitud ,",
               "        ' '                ,",#desc_estado
               "        A.consecutivo       ",
               " FROM  ret_sol_issste_par A ",
               " WHERE ",x_busca CLIPPED

    PREPARE pre_4 FROM txt_1
    DECLARE cur_4 CURSOR FOR pre_4

    ERROR "PROCESANDO INFORMACION"

    LET i = 1
    FOREACH cur_4 INTO arr_1[i].*

        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].estado_solicitud > 0 THEN

	    PROMPT "SOLICITUD DEL NSS <",arr_1[i].nss_imss,
	  			      "> SE ENCUENTRA EN EL ESTADO : ",
				      x_estado_solicitud CLIPPED,
				      " " ATTRIBUTE(REVERSE) 
	       FOR opc ATTRIBUTE(REVERSE)
	    CONTINUE FOREACH
        END IF

        SELECT tipo_solicitud
        INTO   arr_1[i].tipo_solicitud
        FROM   afi_mae_afiliado
        WHERE  n_seguro = arr_1[i].nss_imss

        SELECT desc_solicitud
        INTO   arr_1[i].desc_tipo_sol
        FROM   tab_tipo_solic
        WHERE  tipo_solicitud = arr_1[i].tipo_solicitud

        SELECT tipo_trab_ind,tipo_administracion
        INTO   arr_1[i].tipo_trabajador,arr_1[i].tipo_inversion
        FROM   cta_ctr_reg_ind
        WHERE  nti = arr_1[i].nss_imss

        IF STATUS = NOTFOUND THEN
            LET arr_1[i].tipo_inversion = 1

            LET arr_1[i].desc_tipo_trab = "IMSS"
        ELSE
            SELECT desc_tipo_trab_ind
            INTO   arr_1[i].desc_tipo_trab
            FROM   tab_tipo_trab_ind
            WHERE  tipo_trab_ind = arr_1[i].tipo_trabajador
        END IF

        SELECT desc_tipo_admon
        INTO   arr_1[i].desc_tipo_inv
        FROM   tab_tipo_administracion
        WHERE  tipo_administracion = arr_1[i].tipo_inversion

        SELECT desc_retiro
        INTO   arr_1[i].des_tipo_retiro
        FROM   tab_retiro_issste
        WHERE  tipo_retiro = arr_1[i].tipo_retiro

        SELECT desc_beneficio
        INTO   arr_1[i].desc_tipo_benef
        FROM   tab_beneficio_issste
        WHERE  tipo_beneficio = arr_1[i].tipo_beneficio

        SELECT descripcion
        INTO   arr_1[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   arr_1[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = arr_1[i].rechazo_cod
        END IF

        SELECT A.estado_solicitud
        INTO   v_status
        FROM   ret_solicitud_tx A
        WHERE  A.nss         = arr_1[i].nss_imss
        AND    A.consecutivo = arr_1[i].consecutivo

        SELECT fecha_conversion
        INTO   arr_1[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = arr_1[i].nss_imss
        AND    consecutivo_lote = arr_1[i].consecutivo
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    IF i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm9003
        RETURN
    END IF

    LET cont_reg = i-1

    ERROR ""
    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT 22,51 
    ATTRIBUTE(REVERSE)

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY arr_1 TO scr_1.*

        ON KEY (CONTROL-B)
            LET      arr_c           =  arr_curr()
            LET v_ejecuta = "fglgo RETM810 ",arr_1[arr_c].nss_imss CLIPPED," ",
                            arr_1[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
            RUN v_ejecuta
        ON KEY ( CONTROL-M )

            PROMPT "  DESEA ELIMAN EL REGISTRO S/N " FOR CHAR enter
            IF enter MATCHES "[Ss]" THEN

               IF  v_status <> 0 THEN
                  PROMPT "  NO PUEDE SER ELIMINADO ESTE REGISTRO" FOR CHAR enter
                  EXIT DISPLAY
               ELSE
                  LET arr = ARR_CURR()
                  LET src = SCR_LINE()

                  DELETE
                  FROM  ret_beneficiario 
                  WHERE ret_beneficiario.consecutivo = arr_1[arr].consecutivo

                  DELETE
                  FROM  ret_sol_issste_par
                  WHERE ret_sol_issste_par.consecutivo = arr_1[arr].consecutivo

                  DELETE
                  FROM  ret_consecutivo
                  WHERE consecutivo = arr_1[arr].consecutivo

                  ----- REVERSAR MARCAJE -----

                  SELECT movimiento
		  INTO   s_tipo_movimiento
		  FROM   tab_retiro_issste
	  	  WHERE  tipo_retiro = arr_1[arr].tipo_retiro

                  LET reg_rev.marca_cod   = s_tipo_movimiento
                  LET reg_rev.nss         = arr_1[arr].nss_imss
                  LET reg_rev.correlativo = arr_1[arr].consecutivo

		  LET vestado_marca   = 40
		  LET vcodigo_rechazo = 0
		  LET vmarca_causa    = 0
		  LET vfecha_causa    = NULL

                  LET v_reversa = " EXECUTE PROCEDURE desmarca_cuenta('",
                                    reg_rev.nss,"',",
				    reg_rev.marca_cod,",",
                                    reg_rev.correlativo,",",
				    vestado_marca,",",
				    vmarca_causa,",' ",
				    usuario,"')"
                  PREPARE eje_rever03 FROM v_reversa
                  EXECUTE eje_rever03
                  ----------------------------

                  DISPLAY "  REGISTRO ELIMINADO  ",""
                          AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
                  EXIT DISPLAY
               END IF
            END IF
            EXIT DISPLAY

        ON KEY ( CONTROL-C )
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            EXIT DISPLAY
    END DISPLAY
CLOSE WINDOW retm9003
END FUNCTION

FUNCTION despliega_tipo_retiro()
#dtr----------------------------
    DEFINE  ra_reg ARRAY[100] OF RECORD
        codigo            CHAR(02),
        descripcion       CHAR(50)
    END RECORD

    DEFINE   prepare_1    CHAR(200),
             x_buscar     CHAR(030)

    OPEN WINDOW retm9014 AT 05,12 WITH FORM "RETM9014" ATTRIBUTE(BORDER)
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
                        " AND   tipo_retiro IN (4,5) ",
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
                LET reg.tipo_retiro = ra_reg[pos].codigo           
                LET reg.des_tipo_retiro = ra_reg[pos].descripcion      
                EXIT DISPLAY
  
        END DISPLAY                                                      

        IF pos <> 0 THEN        
            EXIT WHILE          
        END IF                  
    END WHILE                   
    CLOSE WINDOW retm9014       
END FUNCTION                    


FUNCTION despliega_tipo_beneficio()
#dtb----------------------------
    DEFINE  ra_reg ARRAY[100] OF RECORD
        codigo            CHAR(03),
        descripcion       CHAR(50)
    END RECORD

    DEFINE   prepare_1    CHAR(200),
             x_buscar     CHAR(030)

    OPEN WINDOW retm9014 AT 05,12 WITH FORM "RETM9014" ATTRIBUTE(BORDER)
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
           ERROR "    ARCHIVO TIPO DE BENEFICIO VACIO"                    
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
               LET reg.tipo_beneficio   = ra_reg[pos].codigo           
               LET reg.desc_tipo_benef  = ra_reg[pos].descripcion      
               EXIT DISPLAY
  
       END DISPLAY                                                      

       IF pos <> 0 THEN        
           EXIT WHILE          
       END IF                  
    END WHILE                   
    CLOSE WINDOW retm9014       
END FUNCTION                     


FUNCTION marca_cuenta(vl_nss,vl_marca_ent,vl_consecutivo)
#mc------------------------------------------------------
    DEFINE #loc #smallint
         vl_marca_ent        ,
         vl_marca_res        ,
         vl_rechazo_cod      SMALLINT

    DEFINE #loc #char
         vl_nss              CHAR(011)

    DEFINE #loc #integer
         vl_consecutivo      INTEGER

    PREPARE eje_marca FROM v_marca
    DECLARE cur_sp CURSOR FOR eje_marca
    OPEN cur_sp USING vl_nss                , # nss
                      vl_marca_ent          , # marca entrant
                      vl_consecutivo        , # correlativo
                      reg_20.estado_marca   , # estado_marca
                      reg_20.codigo_rechazo , # codigo rechazo
                      reg_20.marca_causa    , # marca_causa
                      reg_20.fecha_causa    , # fecha_causa
                      usuario

    FETCH cur_sp INTO vl_marca_res   , # misma marca si convive o
                      vl_rechazo_cod   # marca activa que rechaza

    CLOSE cur_sp

    RETURN vl_marca_res,
           vl_rechazo_cod

END FUNCTION
